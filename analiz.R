# -----------------------------------------------------------------------------
# Proje: Denetim Verisi Analizi (Etkileşimli "Red Flag" Grafiği ile)
# Açıklama: Bu betik, bir Excel dosyasındaki denetim verilerini okur,
#           şüpheli durumları ("RED FLAG") tespit eder ve sonuçları
#           interaktif bir dağılım grafiği ile raporlar.
# -----------------------------------------------------------------------------


### 1. ORTAM AYARLARI VE KÜTÜPHANELER ###

# --- EN ÖNEMLİ DÜZELTME: SİSTEM YERELİNİ AYARLAMA ---
# Bu komut, R oturumunu UTF-8 karakter setini kullanmaya zorlayarak
# Windows'taki Türkçe karakter kodlama sorunlarını kökünden çözer.
# Hata alırsanız "tr_TR.UTF-8" yerine "en_US.UTF-8" de deneyebilirsiniz.
Sys.setlocale("LC_ALL", "Turkish")

# Gerekli paketler yüklü değilse, aşağıdaki satırların başındaki '#' işaretini
# kaldırıp bir kereliğine mahsus çalıştırın.
# install.packages(c("tidyverse", "readxl", "janitor", "writexl", "plotly", "htmlwidgets"))

library(tidyverse)
library(readxl)
library(janitor)
library(writexl)
library(plotly)
library(htmlwidgets)


### 2. VERİ YÜKLEME VE TEMİZLEME ###

# R'ın çalışma dizininin "denetim_dataseti.xlsx" dosyasının bulunduğu
# klasör olarak ayarlandığından emin olun.
denetim_verisi <- read_excel("denetim_dataseti.xlsx", col_names = TRUE) %>%
  clean_names()

glimpse(denetim_verisi)


### 3. VERİ ANALİZİ VE ETKİLEŞİM İÇİN HAZIRLIK ###

analiz_verisi <- denetim_verisi %>%
  # Önceki düzeltmemiz hala geçerli ve iyi bir güvenlik önlemi
  mutate(across(where(is.character), ~ iconv(., to = "UTF-8", sub = ""))) %>%
  
  mutate(
    # Tarih sütununu R'ın anlayacağı Date formatına çevirelim
    islem_tarihi = as.Date(islem_tarihi),
    
    gerceklesen_oran = basvuru_bedeli / islem_tutari,
    durum = case_when(
      is.na(gerceklesen_oran) | is.infinite(gerceklesen_oran) ~ "Veri Hatalı",
      gerceklesen_oran > 0.15 ~ "RED FLAG",
      TRUE ~ "Normal"
    ),
    
    # Fare ile noktanın üzerine gelindiğinde gösterilecek metni oluşturuyoruz.
    hover_text = paste0(
      "<b>Görevli:</b> ", gorevlinin_adi, "\n",
      "<b>İşlem Tarihi:</b> ", format(islem_tarihi, "%d-%m-%Y"), "\n",
      "<b>İşlem Tutarı:</b> ", scales::comma(islem_tutari, big.mark = ".", decimal.mark = ","), " TL\n",
      "<b>Başvuru Bedeli:</b> ", scales::comma(basvuru_bedeli, big.mark = ".", decimal.mark = ","), " TL\n",
      "<b>Gerçekleşen Oran:</b> %", round(gerceklesen_oran * 100, 2)
    )
  )

# Rapor için şüpheli işlemleri hazırlama
red_flag_islemler <- analiz_verisi %>%
  filter(durum == "RED FLAG") %>%
  select(gorevlinin_adi, islem_tarihi, islem_tutari, basvuru_bedeli, gerceklesen_oran) %>%
  arrange(desc(gerceklesen_oran))


### 4. GÖRSELLEŞTİRME ###

options(scipen = 999)

# 4.1. ETKİLEŞİMLİ Dağılım Grafiği
dagilim_grafigi_static <- ggplot(analiz_verisi, aes(x = islem_tutari, y = basvuru_bedeli, text = hover_text)) +
  geom_point(aes(color = durum), alpha = 0.7, size = 2) +
  geom_abline(intercept = 0, slope = 0.15, color = "blue", linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = c("RED FLAG" = "#e74c3c", "Normal" = "gray50", "Veri Hatalı" = "orange")) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "İşlem Tutarı ve Başvuru Bedeli Dağılımı",
    subtitle = "Mavi çizgi %15'lik sınırı, kırmızı noktalar ise şüpheli işlemleri göstermektedir.",
    x = "İşlem Tutarı (TL)",
    y = "Başvuru Bedeli (TL)",
    color = "İşlem Durumu"
  ) +
  theme_minimal(base_size = 12)

etkilesimli_grafik <- ggplotly(dagilim_grafigi_static, tooltip = "text")
print(etkilesimli_grafik)


# 4.2. Görevli Bazında Özet Grafik
gorevli_grafigi <- analiz_verisi %>%
  filter(durum %in% c("RED FLAG", "Normal")) %>%
  count(gorevlinin_adi, durum) %>%
  ggplot(aes(x = n, y = reorder(gorevlinin_adi, n), fill = durum)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("RED FLAG" = "#e74c3c", "Normal" = "gray50")) +
  labs(title = "Görevli Bazında Şüpheli İşlem Sayıları", x = "Toplam İşlem Sayısı", y = "Görevlinin Adı", fill = "İşlem Durumu") +
  theme_minimal(base_size = 12)

print(gorevli_grafigi)


### 5. SONUÇLARI KAYDETME ###

write_xlsx(red_flag_islemler, "Süpheli_İslemler_Raporu.xlsx")
saveWidget(etkilesimli_grafik, "Etkilesimli_Denetim_Grafigi.html", selfcontained = TRUE)
ggsave("Gorevli_Bazinda_Grafik.png", plot = gorevli_grafigi, width = 10, height = 8, dpi = 300)

print("Analiz tamamlandı. 'Etkilesimli_Denetim_Grafigi.html' dosyası oluşturuldu. Çift tıklayarak açabilirsiniz!")

