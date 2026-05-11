library(ggplot2)
library(corrplot)
library(dplyr)

# Boxplot tingkat fokus
ggplot(data_clean, aes(x = jenis_kopi, y = tingkat_fokus, fill = jenis_kopi)) +
  geom_boxplot() +
  labs(
    title = "Boxplot Tingkat Fokus Berdasarkan Jenis Kopi",
    x = "Jenis Kopi",
    y = "Tingkat Fokus"
  ) +
  theme_minimal()

# Barplot rata-rata fokus
mean_focus <- data_clean %>%
  group_by(jenis_kopi) %>%
  summarise(
    rata_rata_fokus = mean(tingkat_fokus, na.rm = TRUE)
  )

ggplot(mean_focus, aes(x = jenis_kopi, y = rata_rata_fokus, fill = jenis_kopi)) +
  geom_col() +
  labs(
    title = "Rata-rata Tingkat Fokus Berdasarkan Jenis Kopi",
    x = "Jenis Kopi",
    y = "Rata-rata Tingkat Fokus"
  ) +
  theme_minimal()

# Scatter kafein vs fokus
ggplot(data_clean, aes(x = kafein_mg, y = tingkat_fokus, color = jenis_kopi)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Hubungan Kafein dengan Tingkat Fokus",
    x = "Kafein (mg)",
    y = "Tingkat Fokus",
    color = "Jenis Kopi"
  ) +
  theme_minimal()

# Scatter gula vs fokus
ggplot(data_clean, aes(x = gula_gram, y = tingkat_fokus, color = jenis_kopi)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Hubungan Gula dengan Tingkat Fokus",
    x = "Gula (gram)",
    y = "Tingkat Fokus",
    color = "Jenis Kopi"
  ) +
  theme_minimal()

# Histogram tingkat fokus
ggplot(data_clean, aes(x = tingkat_fokus)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  labs(
    title = "Distribusi Tingkat Fokus",
    x = "Tingkat Fokus",
    y = "Frekuensi"
  ) +
  theme_minimal()

# Histogram per jenis kopi
ggplot(data_clean, aes(x = tingkat_fokus, fill = jenis_kopi)) +
  geom_histogram(bins = 12, color = "black", alpha = 0.7) +
  facet_wrap(~ jenis_kopi) +
  labs(
    title = "Distribusi Tingkat Fokus per Jenis Kopi",
    x = "Tingkat Fokus",
    y = "Frekuensi"
  ) +
  theme_minimal()

# Heatmap korelasi
numeric_data <- data_clean[, c(
  "kopi_ml",
  "susu_ml",
  "gula_gram",
  "kafein_mg",
  "durasi_tidur_jam",
  "tingkat_fokus"
)]

cor_matrix <- cor(numeric_data, use = "complete.obs")

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  number.cex = 0.8
)

# Heatmap alternatif ggplot
cor_data <- as.data.frame(as.table(cor_matrix))

ggplot(cor_data, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = round(Freq, 2)), color = "black", size = 4) +
  labs(
    title = "Heatmap Korelasi Variabel Numerik",
    x = "",
    y = "",
    fill = "Korelasi"
  ) +
  theme_minimal()
