
data <- read.csv(
  "D:/kopi.csv",
  sep = ";",
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = FALSE,
  check.names = TRUE
)


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


head(data)
str(data)
summary(data)

colSums(is.na(data))


data_clean <- na.omit(data)


table(data_clean$jenis_kopi)


aggregate(tingkat_fokus ~ jenis_kopi, data = data_clean, mean)
aggregate(tingkat_fokus ~ jenis_kopi, data = data_clean, sd)
aggregate(tingkat_fokus ~ jenis_kopi, data = data_clean, min)
aggregate(tingkat_fokus ~ jenis_kopi, data = data_clean, max)


boxplot(
  tingkat_fokus ~ jenis_kopi,
  data = data_clean,
  main = "Tingkat Fokus Berdasarkan Jenis Kopi",
  xlab = "Jenis Kopi",
  ylab = "Tingkat Fokus"
)


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



library(car)


leveneTest(tingkat_fokus ~ jenis_kopi, data = data_clean)


leveneTest(tingkat_fokus ~ jenis_kopi, data = data_clean, center = median)


anova_model <- aov(tingkat_fokus ~ jenis_kopi, data = data_clean)

summary(anova_model)


residual_anova <- residuals(anova_model)

shapiro.test(residual_anova)


bartlett.test(tingkat_fokus ~ jenis_kopi, data = data_clean)


TukeyHSD(anova_model)

kruskal.test(tingkat_fokus ~ jenis_kopi, data = data_clean)
