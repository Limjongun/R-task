# 1. import data dari file csv dengan pengaturan separator dan encoding yang sesuai
data <- read.csv(
  "D:/kopi.csv",
  sep = ";",
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = FALSE,
  check.names = TRUE
)

# 2. ambil hanya kolom utama yang akan digunakan dalam analisis
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

# 3. ubah kolom numerik agar bisa diproses dalam perhitungan statistik
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

# 4. cek struktur dan ringkasan data untuk memastikan format sudah benar
head(data)
str(data)
summary(data)

# 5. cek jumlah missing value pada setiap kolom data
colSums(is.na(data))

# 6. hapus data yang memiliki missing value agar analisis lebih valid
data_clean <- na.omit(data)

# 7. cek jumlah data pada masing-masing jenis kopi yang tersedia
table(data_clean$jenis_kopi)

# 8. hitung statistik deskriptif tingkat fokus berdasarkan jenis kopi
aggregate(tingkat_fokus ~ jenis_kopi, data = data_clean, mean)
aggregate(tingkat_fokus ~ jenis_kopi, data = data_clean, sd)
aggregate(tingkat_fokus ~ jenis_kopi, data = data_clean, min)
aggregate(tingkat_fokus ~ jenis_kopi, data = data_clean, max)

# 9. buat boxplot untuk melihat distribusi tingkat fokus tiap jenis kopi
boxplot(
  tingkat_fokus ~ jenis_kopi,
  data = data_clean,
  main = "Tingkat Fokus Berdasarkan Jenis Kopi",
  xlab = "Jenis Kopi",
  ylab = "Tingkat Fokus"
)

# 10. deteksi outlier menggunakan metode interquartile range atau iqr
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

# 11. lakukan uji one-way anova untuk membandingkan rata-rata fokus
anova_model <- aov(tingkat_fokus ~ jenis_kopi, data = data_clean)

summary(anova_model)

# 12. cek normalitas residual sebagai syarat dalam uji anova
residual_anova <- residuals(anova_model)

shapiro.test(residual_anova)

# 13. cek homogenitas varians antar kelompok jenis kopi
bartlett.test(tingkat_fokus ~ jenis_kopi, data = data_clean)

# 14. lakukan uji lanjut tukey hsd untuk melihat perbedaan kelompok
TukeyHSD(anova_model)

# 15. gunakan uji kruskal wallis sebagai alternatif non-parametrik
kruskal.test(tingkat_fokus ~ jenis_kopi, data = data_clean)
