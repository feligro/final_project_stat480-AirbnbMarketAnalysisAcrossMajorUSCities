library(data.table)
library(lubridate)   # para month(), wday()
library(Matrix)      # para sparse.model.matrix() y sparseMatrix()

###############################
### 0. LECTURA DE DATOS
###############################

# New York
lis  <- fread("NYlistings.csv.gz")
cal  <- fread("NYcalendar.csv.gz")

# Los Angeles
lis2 <- fread("LAlistings.csv.gz")
cal2 <- fread("LAcalendar.csv.gz")

# Dallas
lis3 <- fread("DAlisting.csv.gz")
cal3 <- fread("DAcalendar.csv.gz")

# Chicago
lis4 <- fread("CHlisting.csv.gz")
cal4 <- fread("CHcalendar.csv.gz")

# --- Big Data check: todos los .csv.gz de la carpeta (incluye las 4 ciudades) ---

files <- list.files(pattern = "\\.csv\\.gz$", full.names = TRUE, recursive = TRUE)
sizes_bytes <- file.info(files)$size
sizes_mb <- round(sizes_bytes / (1024^2), 2)

dataset_sizes <- data.frame(
  File = basename(files),
  Path = files,
  Size_MB = sizes_mb
)

print(dataset_sizes)

total_mb <- sum(sizes_mb, na.rm = TRUE)
total_gb <- round(total_mb / 1024, 2)

cat("\n---------------------------------------\n")
cat("Total size of all .csv.gz files:", total_mb, "MB (≈", total_gb, "GB)\n")

if (total_mb >= 200) {
  cat("✅ Requirement met: total ≥ 200 MB — Big Data confirmed!\n")
} else {
  cat("⚠️ Total < 200 MB — consider adding another city dataset.\n")
}
cat("---------------------------------------\n")

###############################
### 1. FUNCIONES AUXILIARES
###############################

clean_price <- function(x){
  x <- gsub("\\$", "", x)         # remove $
  x <- gsub(",", "", x)           # remove commas
  x[x == "" | x == " "] <- NA     # blank → NA
  as.numeric(x)
}

clean_date <- function(x){
  as.Date(x)
}

clean_id <- function(x){
  as.character(x)
}

###############################
### 2. CLEAN CALENDAR DATASETS
###############################

clean_calendar <- function(df){
  df <- as.data.table(df)
  
  # IDs
  if ("listing_id" %in% names(df)) {
    df$listing_id <- clean_id(df$listing_id)
  }
  
  # Dates
  if ("date" %in% names(df)) {
    df$date <- clean_date(df$date)
    
    # Features temporales:
    df[, month   := month(date)]
    df[, weekday := wday(date)]          # 1 = domingo por defecto en lubridate
    df[, is_weekend := weekday %in% c(6,7)]   # sábado = 7, viernes = 6 (dependiendo localización)
  }
  
  # Prices
  if ("price" %in% names(df)) {
    df$price <- clean_price(df$price)
  }
  if ("adjusted_price" %in% names(df)) {
    df$adjusted_price <- clean_price(df$adjusted_price)
  }
  
  # Drop columnas de calendar que no queremos
  drop_cols_cal <- c(
    "calendar_updated",
    "calendar_last_scraped",
    "license"
  )
  drop_cols_cal <- intersect(drop_cols_cal, names(df))
  if (length(drop_cols_cal) > 0) {
    df[, (drop_cols_cal) := NULL]
  }
  
  return(df)
}

cal  <- clean_calendar(cal)
cal2 <- clean_calendar(cal2)
cal3 <- clean_calendar(cal3)
cal4 <- clean_calendar(cal4)

###############################
### 3. CLEAN LISTINGS DATASETS
###############################

clean_listings <- function(df){
  df <- as.data.table(df)
  
  # --- Convert IDs ---
  if ("id" %in% names(df)) {
    df$id <- clean_id(df$id)
  }
  if ("host_id" %in% names(df)) {
    df$host_id <- clean_id(df$host_id)
  }
  
  # --- Clean price ---
  if ("price" %in% names(df)) {
    df$price <- clean_price(df$price)
  }
  
  # --- Noches numéricas ---
  if ("minimum_nights" %in% names(df)) {
    df$minimum_nights <- as.numeric(df$minimum_nights)
  }
  if ("maximum_nights" %in% names(df)) {
    df$maximum_nights <- as.numeric(df$maximum_nights)
  }
  
  # --- room_type como factor ---
  if ("room_type" %in% names(df)) {
    df[, room_type := factor(room_type)]
  }
  
  # --- amenity_count a partir de amenities ---
  if ("amenities" %in% names(df)) {
    df[, amenity_count := sapply(strsplit(amenities, ","), length)]
  }
  
  # --- DROP columnas que NO queremos usar en listings ---
  drop_cols_listings <- c(
    "listing_url",
    "scrape_id",
    "last_scraped",
    "source",
    "name",
    "description",
    "neighborhood_overview",
    "picture_url",
    "host_url",
    "host_name",
    "host_location",
    "host_about",
    "host_thumbnail_url",
    "host_picture_url",
    "host_neighbourhood",
    "neighbourhood",
    "bathrooms_text",
    "calendar_updated",
    "calendar_last_scraped",
    "number_of_reviews_ltm",
    "number_of_reviews_l_30d",
    "first_review",
    "last_review",
    "license",
    "calculated_host_listings_count_entire_homes",
    "calculated_host_listings_count_private_rooms",
    "calculated_host_listings_count_shared_rooms"
    # OJO: NO incluimos "amenities" aquí porque la estamos usando.
  )
  
  drop_cols_listings <- intersect(drop_cols_listings, names(df))
  if (length(drop_cols_listings) > 0) {
    df[, (drop_cols_listings) := NULL]
  }
  
  return(df)
}

lis  <- clean_listings(lis)
lis2 <- clean_listings(lis2)
lis3 <- clean_listings(lis3)
lis4 <- clean_listings(lis4)

###############################
### 4. AGREGAR CALENDAR → FEATURES A LISTINGS
###############################

add_calendar_features <- function(lis_dt, cal_dt){
  # Cálculo de métricas por listing_id
  # occupancy_rate = sum(available == "f") / .N
  # avg_available_price = mean(price[available == "t"], na.rm=TRUE)
  
  cal_agg <- cal_dt[, .(
    occupancy_rate = sum(available == "f", na.rm = TRUE) / .N,
    avg_available_price = mean(price[available == "t"], na.rm = TRUE)
  ), by = listing_id]
  
  # Renombrar listing_id a id para merge
  setnames(cal_agg, "listing_id", "id")
  
  # Asegurarnos de que id es character
  lis_dt[, id := clean_id(id)]
  cal_agg[, id := clean_id(id)]
  
  # Merge left
  lis_merged <- merge(lis_dt, cal_agg, by = "id", all.x = TRUE)
  
  return(lis_merged)
}

lis  <- add_calendar_features(lis,  cal)
lis2 <- add_calendar_features(lis2, cal2)
lis3 <- add_calendar_features(lis3, cal3)
lis4 <- add_calendar_features(lis4, cal4)

###############################
### 5. MATRICES DISPERSAS DE AMENITIES POR CIUDAD
###############################

amenity_matrix_NY <- if ("amenities" %in% names(lis))  sparse.model.matrix(~ amenities - 1, data = lis)  else NULL
amenity_matrix_LA <- if ("amenities" %in% names(lis2)) sparse.model.matrix(~ amenities - 1, data = lis2) else NULL
amenity_matrix_DA <- if ("amenities" %in% names(lis3)) sparse.model.matrix(~ amenities - 1, data = lis3) else NULL
amenity_matrix_CH <- if ("amenities" %in% names(lis4)) sparse.model.matrix(~ amenities - 1, data = lis4) else NULL

###############################
### 6. OUTPUT DIMENSIONS
###############################

cat("Cleaned datasets created:\n")
cat("NY  - cal:  ", nrow(cal),  "rows\n")
cat("LA  - cal2: ", nrow(cal2), "rows\n")
cat("DA  - cal3: ", nrow(cal3), "rows\n")
cat("CH  - cal4: ", nrow(cal4), "rows\n\n")

cat("NY  - lis:  ", nrow(lis),  "rows ×", ncol(lis),  "cols\n")
cat("LA  - lis2: ", nrow(lis2), "rows ×", ncol(lis2), "cols\n")
cat("DA  - lis3: ", nrow(lis3), "rows ×", ncol(lis3), "cols\n")
cat("CH  - lis4: ", nrow(lis4), "rows ×", ncol(lis4), "cols\n")

###############################
### 7. CHECK RÁPIDO DE NUEVAS VARIABLES
###############################

summary(lis$amenity_count)
summary(lis$occupancy_rate)
summary(lis$avg_available_price)

summary(cal[, .(month, weekday, is_weekend)])

###############################
### 8. UNIR TODOS LOS LISTINGS
###############################

# Añadimos una variable city para saber de dónde viene cada listing
lis[,  city := "NY"]
lis2[, city := "LA"]
lis3[, city := "DA"]
lis4[, city := "CH"]

# Unimos todos los listings en un solo data.table
all_listings <- rbindlist(list(lis, lis2, lis3, lis4), use.names = TRUE, fill = TRUE)

cat("Final merged listings dataset:\n")
cat("Rows:", nrow(all_listings), "  Cols:", ncol(all_listings), "\n")

###############################
### 9. NUEVA VARIABLE log(price)
###############################

# Creamos log_price (log natural); evitamos problemas con NA o precios <= 0
all_listings[, log_price := NA_real_]
all_listings[!is.na(price) & price > 0, log_price := log(price)]

summary(all_listings$log_price)

###############################
### 9bis. "t"/"f" → 0/1
###############################

tf_to_binary <- function(x) {
  x <- as.character(x)
  x[x == "t"] <- "1"
  x[x == "f"] <- "0"
  x[!(x %in% c("0","1"))] <- NA   # cualquier otra cosa → NA
  as.integer(x)
}

tf_cols <- c(
  "host_identity_verified",
  "host_is_superhost",
  "host_has_profile_pic",
  "has_availability",
  "instant_bookable"
)

tf_cols <- intersect(tf_cols, names(all_listings))

for (col in tf_cols) {
  all_listings[[col]] <- tf_to_binary(all_listings[[col]])
}

str(all_listings[, ..tf_cols])

###############################
### 10. NUEVA VARIABLE high_occupancy
###############################
# Usamos un umbral razonable, por ejemplo 0.8 (80% de ocupación).
# high_occupancy = TRUE si occupancy_rate >= 0.8

threshold_occupancy <- 0.8
all_listings[, high_occupancy := NA]
all_listings[!is.na(occupancy_rate),
             high_occupancy := occupancy_rate >= threshold_occupancy]

table(all_listings$high_occupancy, useNA = "ifany")

########################################
### A. Info básica de missingness
########################################

n <- nrow(all_listings)
na_prop <- colSums(is.na(all_listings)) / n
sort(round(na_prop, 3))

########################################
### B. 1) Filas sin price → fuera
########################################

cat("Filas totales antes de filtrar por price:", nrow(all_listings), "\n")
all_listings <- all_listings[!is.na(price)]
cat("Filas después de eliminar NA en price:", nrow(all_listings), "\n")

# Recalcular log_price con los que quedan
all_listings[, log_price := log(price)]

########################################
### C. 2) Tratamiento de reviews
########################################

# Indicator: tiene al menos 1 review
all_listings[, has_reviews := number_of_reviews > 0]

# reviews_per_month: NA -> 0 (no reviews)
all_listings[is.na(reviews_per_month), reviews_per_month := 0]

# columnas de review_scores
review_cols <- c(
  "review_scores_rating",
  "review_scores_accuracy",
  "review_scores_cleanliness",
  "review_scores_checkin",
  "review_scores_communication",
  "review_scores_location",
  "review_scores_value"
)

# Para listings sin reviews: NA -> 0 (no aplica)
for (col in review_cols) {
  all_listings[number_of_reviews == 0 & is.na(get(col)), (col) := 0]
}

# Por si quedara algún NA residual en los que SÍ tienen reviews → mediana
for (col in review_cols) {
  med <- median(all_listings[[col]], na.rm = TRUE)
  all_listings[is.na(get(col)), (col) := med]
}

########################################
### D. 3) Variables numéricas con pocos NA
########################################

few_na_num <- c(
  "host_listings_count",
  "host_total_listings_count",
  "minimum_minimum_nights",
  "maximum_minimum_nights",
  "minimum_maximum_nights",
  "maximum_maximum_nights",
  "minimum_nights_avg_ntm",
  "maximum_nights_avg_ntm"
)

few_na_num <- intersect(few_na_num, names(all_listings))

for (col in few_na_num) {
  med <- median(all_listings[[col]], na.rm = TRUE)
  all_listings[is.na(get(col)), (col) := med]
}

# occupancy_rate / high_occupancy: si está NA, asumimos 0 ocupación
all_listings[is.na(occupancy_rate), occupancy_rate := 0]
all_listings[is.na(high_occupancy), high_occupancy := FALSE]

########################################
### E. 4) Categórica con NA
########################################

if ("neighbourhood_group_cleansed" %in% names(all_listings)) {
  all_listings[is.na(neighbourhood_group_cleansed),
               neighbourhood_group_cleansed := "Unknown"]
  all_listings[, neighbourhood_group_cleansed :=
                 factor(neighbourhood_group_cleansed)]
}

########################################
### F. 5) bathrooms, bedrooms, beds
###     Imputación por mediana dentro de (city, room_type)
########################################

cols_bb <- c("bathrooms", "bedrooms", "beds")
cols_bb <- intersect(cols_bb, names(all_listings))

for (col in cols_bb) {
  all_listings[,
               (col) := {
                 v <- get(col)
                 # mediana por grupo; si todo NA en el grupo, usa mediana global
                 if (all(is.na(v))) {
                   v_glob <- all_listings[[col]]
                   v[is.na(v)] <- median(v_glob, na.rm = TRUE)
                 } else {
                   m <- median(v, na.rm = TRUE)
                   v[is.na(v)] <- m
                 }
                 v
               },
               by = .(city, room_type)
  ]
}

########################################
### G. 6) avg_available_price
########################################

# Primero, si falta pero tenemos price, usamos price
all_listings[is.na(avg_available_price) & !is.na(price),
             avg_available_price := price]

# Lo que quede aún NA → mediana global
med_aap <- median(all_listings$avg_available_price, na.rm = TRUE)
all_listings[is.na(avg_available_price), avg_available_price := med_aap]

########################################
### H. 7) host_since (si lo vas a usar)
########################################

if ("host_since" %in% names(all_listings)) {
  # convertir a Date si no lo está
  if (!inherits(all_listings$host_since, "Date")) {
    all_listings[, host_since := as.Date(host_since)]
  }
  # imputación con fecha mediana (aprox)
  med_date_num <- median(as.numeric(all_listings$host_since), na.rm = TRUE)
  all_listings[is.na(host_since), host_since := as.Date(med_date_num, origin = "1970-01-01")]
}

########################################
### I. 8) Comprobación final de NA
########################################

na_final <- colSums(is.na(all_listings))
cat("NA restantes por columna (solo las que aún tienen):\n")
print(na_final[na_final > 0])

###############################
### 11. SPARSE MATRIX DE AMENITIES (BIG P)
###     Usa las top K amenities normalizadas como columnas
###############################

library(Matrix)

if ("amenities" %in% names(all_listings)) {
  
  amenities_raw <- all_listings$amenities
  amenities_raw[is.na(amenities_raw)] <- ""
  
  # 11.1 Extraer strings entre ""Amenity""
  extract_amenities <- function(x) {
    if (is.na(x) || x == "") return(character(0))
    m <- gregexpr('""([^""]+)""', x, perl = TRUE)
    matches <- regmatches(x, m)[[1]]
    if (length(matches) == 0) return(character(0))
    clean <- gsub('^""|""$', "", matches)
    trimws(clean)
  }
  
  amenities_list <- lapply(amenities_raw, extract_amenities)
  cat("Ejemplo de amenities extraídas (fila 1):\n")
  print(amenities_list[[1]])
  
  # 11.2 Normalización de texto
  normalize_amenity <- function(x) {
    x <- tolower(x)
    x <- iconv(x, "UTF-8", "ASCII//TRANSLIT")
    
    # quitar tamaños tipo "24 inch", "19 inch"
    x <- gsub("[0-9]+\\s*inch", "", x)
    # quitar Mbps
    x <- gsub("[0-9]+\\s*mbps", "", x)
    # quitar otros números
    x <- gsub("[0-9]+", "", x)
    
    # textos de disponibilidad que no aportan
    x <- gsub("available all year|available seasonally|open 24 hours|open specific hours",
              "", x)
    
    # unificar plataformas de streaming
    x <- gsub("amazon prime video|prime video|amazon prime", " streaming", x)
    x <- gsub("hbo max", " streaming", x)
    x <- gsub("disney\\+", " streaming", x)
    x <- gsub("netflix", " streaming", x)
    x <- gsub("apple tv", " streaming", x)
    x <- gsub("hulu", " streaming", x)
    x <- gsub("roku", " streaming", x)
    x <- gsub("fire tv", " streaming", x)
    x <- gsub("chromecast", " streaming", x)
    
    # unificar tv
    x <- gsub("hdtv", " tv", x)
    x <- gsub("smart tv", " tv", x)
    
    # unificar neveras
    x <- gsub("refrigerator|fridge", " fridge", x)
    # mantener freezer como tal
    # aire acondicionado
    x <- gsub("air conditioning|a/c|ac", " air conditioning", x)
    
    # wifi
    x <- gsub("fast wifi|wi[- ]?fi|wifi", " wifi", x)
    
    x <- gsub("\\s+", " ", x)
    trimws(x)
  }
  
  amenities_norm_list <- lapply(amenities_list, function(v) {
    v_norm <- normalize_amenity(v)
    unique(v_norm[v_norm != ""])
  })
  
  # 11.3 Frecuencia por amenity normalizada (en cuántos listings aparece)
  all_norm <- unlist(amenities_norm_list)
  amenity_counts <- table(all_norm)
  amenity_counts <- sort(amenity_counts, decreasing = TRUE)
  
  cat("Número total de amenities normalizadas únicas:",
      length(amenity_counts), "\n")
  cat("Top 20 amenities más frecuentes:\n")
  print(head(amenity_counts, 20))
  
  # 11.4 Elegimos K amenities más frecuentes (BIG P)
  top_k <- 400   # <- AJUSTA AQUÍ: 200–500 según quieras
  top_k <- min(top_k, length(amenity_counts))
  selected_amenities <- names(amenity_counts)[1:top_k]
  
  cat("Usando", length(selected_amenities),
      "amenities como columnas (big P).\n")
  
  # 11.5 Construimos sparse matrix SOLO con esas top K amenities
  n_listings <- length(amenities_norm_list)
  
  # Para cada listing, nos quedamos con amenities que están en selected_amenities
  filtered_per_listing <- lapply(amenities_norm_list, function(v) {
    v[v %in% selected_amenities]
  })
  
  lens <- sapply(filtered_per_listing, length)
  
  if (sum(lens) > 0) {
    i_vec <- rep(seq_len(n_listings), times = lens)
    amen_to_col <- setNames(seq_along(selected_amenities), selected_amenities)
    j_vec <- amen_to_col[unlist(filtered_per_listing)]
    x_vec <- rep(1L, length(j_vec))
    
    amenities_sparse <- sparseMatrix(
      i = i_vec,
      j = j_vec,
      x = x_vec,
      dims = c(n_listings, length(selected_amenities)),
      dimnames = list(NULL, selected_amenities)
    )
    
    cat("Dimensiones de amenities_sparse (big P):\n")
    print(dim(amenities_sparse))
  } else {
    amenities_sparse <- NULL
    warning("Tras filtrar por top K no quedan amenities; revisa top_k.")
  }
  
  # 11.6 Eliminamos la columna de texto original
  all_listings[, amenities := NULL]
  
} else {
  amenities_sparse <- NULL
  warning("Column 'amenities' not found in all_listings.")
}


###############################
### 12. DATASET FINAL
###############################

cat("Final dataset ready for modeling.\n")
cat("Listings rows:", nrow(all_listings), "  cols:", ncol(all_listings), "\n")

amenities_sparse <- amenities_sparse[complete.cases(all_listings), ]
all_listings2 <- all_listings[complete.cases(all_listings), ]
# Si quieres guardar:
saveRDS(all_listings2, "all_listings.rds")
saveRDS(amenities_sparse, "amenities_sparse.rds")
