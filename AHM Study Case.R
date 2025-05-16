penjualan <- c(
  Honda = 19.4,
  Hero_Motor = 5.9,
  Yamaha = 4.6,
  Yadea = 4.3,
  TVS_Motor = 3.7,
  Bajaj_Auto = 3.1,
  Suzuki = 2.0,
  Italika = 1.3
)

total_penjualan <- sum(penjualan)

# Menghitung proporsi tiap merek dalam 100 motor
proporsi_100 <- round(penjualan / total_penjualan * 100, 1)

hasil <- data.frame(
  Merek = names(penjualan),
  Penjualan_Juta_Unit = penjualan,
  Proporsi_dari_100_Motor = proporsi_100
)

print(hasil)


library(ggplot2)

# Data
data_motor <- data.frame(
  Merek = c("Honda", "Hero Motor", "Yamaha", "Yadea", "TVS Motor", "Bajaj Auto", "Suzuki", "Italika"),
  Penjualan = c(19.4, 5.9, 4.6, 4.3, 3.7, 3.1, 2.0, 1.3)
)

# Tambahkan kolom warna
data_motor$Warna <- ifelse(data_motor$Merek == "Honda", "Honda", "Lainnya")

# Bar Chart
ggplot(data_motor, aes(x = reorder(Merek, -Penjualan), y = Penjualan, fill = Warna)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Penjualan), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Honda" = "red", "Lainnya" = "grey")) +
  labs(title = "8 Merek Sepeda Motor Terlaris di Dunia (2024)",
       x = "Merek",
       y = "Penjualan (Juta Unit)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

library(readxl)
library(ggplot2)


file_path <- "C:/Users/X1 PRO/AppData/Local/Programs/Microsoft VS Code/motorkux_playstore_reviews.xlsx"
data <- read_excel(file_path)


review_subset <- data$ringkasan[1:328]
review_df <- data.frame(ringkasan = review_subset) %>% 
  filter(ringkasan != "Puas" & !is.na(ringkasan))  # Juga filter NA jika ada


review_freq <- as.data.frame(table(review_df$ringkasan))
colnames(review_freq) <- c("kategori", "freq")


review_freq <- review_freq[order(-review_freq$freq), ]
review_freq$cum_freq <- cumsum(review_freq$freq)
review_freq$cum_percent <- 100 * review_freq$cum_freq / sum(review_freq$freq)

# Buat Diagram Pareto
ggplot(review_freq, aes(x = reorder(kategori, -freq), y = freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_line(aes(y = cum_percent * max(freq) / 100, group = 1), color = "red", size = 1) +
  geom_point(aes(y = cum_percent * max(freq) / 100), color = "red", size = 2) +
  scale_y_continuous(
    name = "Frekuensi",
    sec.axis = sec_axis(~ . * 100 / max(review_freq$freq), name = "Kumulatif (%)")
  ) +
  xlab("Kategori Review") +
  ggtitle("Diagram Pareto Review (PlayStore)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotasi label x jika perlu



file_path <- "C:/Users/X1 PRO/Downloads/motorkux_reviews_multiple_countries.xlsx"
data <- read_excel(file_path)
data

review_subset <- data$ringkasan[1:234]
review_df <- data.frame(ringkasan = review_subset) %>% 
  filter(ringkasan != "Puas" & !is.na(ringkasan))  # Juga filter NA jika ada


review_freq <- as.data.frame(table(review_df$ringkasan))
colnames(review_freq) <- c("kategori", "freq")


review_freq <- review_freq[order(-review_freq$freq), ]
review_freq$cum_freq <- cumsum(review_freq$freq)
review_freq$cum_percent <- 100 * review_freq$cum_freq / sum(review_freq$freq)

# Buat Diagram Pareto
ggplot(review_freq, aes(x = reorder(kategori, -freq), y = freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_line(aes(y = cum_percent * max(freq) / 100, group = 1), color = "red", size = 1) +
  geom_point(aes(y = cum_percent * max(freq) / 100), color = "red", size = 2) +
  scale_y_continuous(
    name = "Frekuensi",
    sec.axis = sec_axis(~ . * 100 / max(review_freq$freq), name = "Kumulatif (%)")
  ) +
  xlab("Kategori Review") +
  ggtitle("Diagram Pareto Review (AppStore)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotasi label x jika perlu


library(qcc)
library(SixSigma)

# Define the causes for each category
cMan <- c("Kurangnya panduan penggunaan", "Kesalahan input pengguna","Staff cabang kurang paham sistem digital")
cMethod <- c("!! Sudah booking tetap harus antre","Alur booking yang rumit", "Proses pendataan motor tidak intuitif")
cMachine <- c("!! Server lambat/overload", "Bug pada fitur tertentu", "Perangkat tidak Kompatibel")
cMaterial <- c("!! Data motor tidak terupdate", "Data tidak terintegrasi di kantor cabang", "Iklan terlalu banyak")
cEnvironment <- c("Koneksi internet tidak stabil", "Beberapa wilayah belum didukung")
cManagement <- c("Kurangnya pengujian sebelum update", "Prioritas fitur tidak sesuai kebutuhan pengguna")

cGroups <- c("Man (Pengguna/Operator)", "Method (Proses/Alur)", "Machine (Teknologi)", 
             "Material (Data/Konten)", "Environment (Lingkungan)", "Management (Kebijakan)")

cEffect <- "Keluhan pengguna"

# Version 1: Using qcc package
cause.and.effect(
  cause = list(
    "Man (Pengguna/Operator)" = cMan,
    "Method (Proses/Alur)" = cMethod,
    "Machine (Teknologi)" = cMachine,
    "Material (Data/Konten)" = cMaterial,
    "Environment (Lingkungan)" = cEnvironment,
    "Management (Kebijakan)" = cManagement
  ),
  effect = cEffect,
  title = "Diagram Ishikawa: Analisis Keluhan Pengguna Aplikasi",
  cex = c(1.0, 0.8, 1.0), 
  font = c(2, 1, 2)        
)

# Version 2: Using SixSigma package (alternative)
ss.ceDiag(
  effect = cEffect,
  causes.gr = cGroups,
  causes = list(
    cMan, 
    cMethod, 
    cMachine, 
    cMaterial, 
    cEnvironment,
    cManagement
  ),
  main = "Diagram Ishikawa: Analisis Keluhan Pengguna Aplikasi",
  sub = "Berdasarkan Data Review PlayStore/AppStore"
)

