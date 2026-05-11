# 1. Import data
data <- read.csv(
  "D:/kopi.csv",
  sep = ";",
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = FALSE,
  check.names = TRUE
)

# 2. Ambil kolom utama
data <- data[, c(
  "id",
  "jenis_kopi",
  "kopi_ml",
  "susu_ml",
  "gula_gram",
  "kafein_mg",
  "durasi_tidur_jam",
  "tingkat_fokus"
)]

# 3. Ubah kolom numerik
num_cols <- c(
  "kopi_ml",
  "susu_ml",
  "gula_gram",
  "kafein_mg",
  "durasi_tidur_jam",
  "tingkat_fokus"
)

data[num_cols] <- lapply(data[num_cols], function(x) {
  as.numeric(gsub(",", ".", x))
})

data$jenis_kopi <- as.factor(data$jenis_kopi)

# 4. Cek struktur data
head(data)
str(data)
summary(data)

# 5. Cek missing value
colSums(is.na(data))

# 6. Bersihkan missing value jika ada
data_clean <- na.omit(data)

# 7. Cek jumlah data per jenis kopi
table(data_clean$jenis_kopi)

# 8. Statistik deskriptif
aggregate(tingkat_fokus ~ jenis_kopi, data = data_clean, mean)
aggregate(tingkat_fokus ~ jenis_kopi, data = data_clean, sd)
aggregate(tingkat_fokus ~ jenis_kopi, data = data_clean, min)
aggregate(tingkat_fokus ~ jenis_kopi, data = data_clean, max)

# 9. Boxplot
boxplot(
  tingkat_fokus ~ jenis_kopi,
  data = data_clean,
  main = "Tingkat Fokus Berdasarkan Jenis Kopi",
  xlab = "Jenis Kopi",
  ylab = "Tingkat Fokus"
)

# 10. Deteksi outlier dengan IQR
Q1 <- quantile(data_clean$tingkat_fokus, 0.25)
Q3 <- quantile(data_clean$tingkat_fokus, 0.75)
IQR_value <- IQR(data_clean$tingkat_fokus)

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

outlier_data <- data_clean[
  data_clean$tingkat_fokus < lower_bound |
  data_clean$tingkat_fokus > upper_bound,
]

outlier_data

# 11. One-Way ANOVA
anova_model <- aov(tingkat_fokus ~ jenis_kopi, data = data_clean)

summary(anova_model)

# 12. Cek normalitas residual
residual_anova <- residuals(anova_model)

shapiro.test(residual_anova)

# 13. Cek homogenitas varians
bartlett.test(tingkat_fokus ~ jenis_kopi, data = data_clean)

# 14. Tukey HSD
TukeyHSD(anova_model)

# 15. Alternatif non-parametrik
kruskal.test(tingkat_fokus ~ jenis_kopi, data = data_clean)
