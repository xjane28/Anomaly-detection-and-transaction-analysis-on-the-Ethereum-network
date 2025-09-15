setwd("/Users/jana/Documents/statprakt_projekt")

data <- read.csv("transaction_dataset.csv", stringsAsFactors = FALSE)

attach(data)

summary(data)

str(data)

#---------------------------

# Postavljanje praga: 90. percentil najvećih transakcija
value_threshold <- quantile(max.val.sent, 0.9)
# Smatramo da su "velike transakcije" one koje su veće od 90% svih zabilježenih maksimalnih transakcija

# Stvaranje binarne varijable Large_Transaction
Large_Transaction <- max.val.sent > value_threshold
# Račun je označen kao TRUE ako njegova maksimalna transakcija prelazi izračunati prag

# Frekvencijska tablica: Broj legitimnih i prevarnih računa za svaki tip transakcije
tablica <- table(Large_Transaction, FLAG)
print(tablica)
# Tablica prikazuje broj računa koji jesu ili nisu imali veliku transakciju, raspoređeno po tipu računa:
# FLAG = 0 → legitimni, FLAG = 1 → prevarni

# Prikaz proporcija unutar grupe (redova)
print(prop.table(tablica, 1))
# Prikazuje koliki udio računa s/bez velike transakcije čine legitimni i prevarni računi
# To znači da među računima koji nisu imali veliku transakciju, otprilike 24% su ipak bili prevarantski
# Među računima koji jesu imali veliku transakciju, samo 5% su prijevare
# Zakljucak: Računi s velikim transakcijama (TRUE) su uglavnom legitimni (95%)
#            Računi bez velikih transakcija (FALSE) imaju veći udio prijevara (24%)
#            Suprotno očekivanju, velika transakcija nije indikator prijevare - zapravo, prijevare su češće kod manjih transakcija

# Bar dijagram
dev.off()  

bp <- barplot(tablica,
              beside = TRUE,
              col = c("skyblue", "tomato"),
              legend.text = c("LT = FALSE", "LT = TRUE"),
              names.arg = c("Legitimno", "Prevara"),
              main = "Stupčasti dijagram broja velikih transakcija",
              xlab = "FLAG (0 = legitimno, 1 = prevara)",
              ylab = "Broj računa",
              ylim = c(0, max(tablica) * 1.2))  # dodano 20% prostora

# Dodavanje brojeva iznad stupaca
text(x = bp, 
     y = tablica, 
     labels = tablica, 
     pos = 3, cex = 0.8, col = "black")

# Koliko računa je imalo veliku transakciju?
print(sum(Large_Transaction)) #984

#---------------------------

# Analiza Sent_tnx (ukupan broj poslanih transakcija)

# Postavljanje praga na 20. percentil za nisku aktivnost u broju poslanih transakcija
sent_tnx_threshold <- quantile(Sent.tnx, 0.2, na.rm = TRUE)

# Kreiranje binarne varijable za "niski broj poslanih transakcija"
Low_Sent_Tnx <- Sent.tnx <= sent_tnx_threshold

# Frekvencijska tablica
tab_sent_tnx <- table(Low_Sent_Tnx, FLAG)
print(tab_sent_tnx)

# Prikaz proporcija unutar reda (proporcije po grupama low/high)
print(prop.table(tab_sent_tnx, 1))
# Gledamo: Među računima s niskim brojem poslanih transakcija, koliko ih je prevarantskih?
# Ili: Među računima s visokim brojem transakcija, koliko ih je legitimnih?

# Barplot za Sent_tnx
# Kreiranje barplota i spremanje koordinata
bp <- barplot(tab_sent_tnx,
              beside = TRUE,
              col = c("skyblue", "tomato"),
              legend.text = c("NISKO", "VISOKO"),
              names.arg = c("Legitimno", "Prevara"),
              main = "Stupčasti dijagrami prema broju poslanih transakcija",
              xlab = "FLAG (0=legitimno, 1=prevara)",
              ylab = "Broj računa",
              ylim = c(0, max(tab_sent_tnx) * 1.2))  # prostor za oznake

# Dodavanje brojeva iznad stupaca
text(x = bp,
     y = tab_sent_tnx,
     labels = tab_sent_tnx,
     pos = 3, cex = 0.8, col = "black")




#---------------------------------------



# Analiza Avg_min_between_sent_tnx (prosječno vrijeme između poslanih transakcija)

# Postavljanje praga na 80. percentil za dugo vrijeme između transakcija
time_threshold <- quantile(Avg.min.between.sent.tnx, 0.8, na.rm = TRUE)

# Kreiranje binarne varijable za "dugo vrijeme između transakcija"
Long_Avg_Time_Between <- Avg.min.between.sent.tnx >= time_threshold

# Frekvencijska tablica
tab_avg_time <- table(Long_Avg_Time_Between, FLAG)
print(tab_avg_time)

# Prikaz proporcija unutar reda (proporcije po grupama)
print(prop.table(tab_avg_time, 1))

# Barplot za Avg_min_between_sent_tnx
bp <- barplot(tab_avg_time,
              beside = TRUE,
              col = c("skyblue", "tomato"),
              legend.text = c("KRATKO", "DUGO"),
              names.arg = c("Legitimno", "Prevara"),
              main = "Stupčasti dijagrami računa prema prosječnom vremenu između poslanih transakcija",
              xlab = "FLAG (0=legitimno, 1=prevara)",
              ylab = "Broj računa",
              ylim = c(0, max(tab_avg_time) * 1.2))  # prostor za oznake

# Dodavanje brojeva iznad stupaca
text(x = bp,
     y = tab_avg_time,
     labels = tab_avg_time,
     pos = 3, cex = 0.8, col = "black")


barplot(t(tab_avg_time),
        beside = TRUE,
        col = c("skyblue", "tomato"),
        legend.text = c("Legitimno", "Prevara"),
        names.arg = c("KRATKO", "DUGO"),
        main = "Stupčasti dijagrami računa prema prosječnom vremenu između poslanih transakcija",
        xlab = "Prosječno vrijeme između transakcija",
        ylab = "Broj računa")



tab_flag_time <- table(Long_Avg_Time_Between, FLAG)

# Hi-kvadrat test
chi_test_flag_time <- chisq.test(tab_flag_time)

print(chi_test_flag_time)





#-----------------------------------------

# Postavljanje praga na 20. percentil za nisku vrijednost prosječnih transakcija
avg_val_sent_threshold <- quantile(avg.val.sent, 0.2, na.rm = TRUE)

# Kreiranje binarne varijable: TRUE = niska vrijednost transakcija
Low_Avg_Val_Sent <- avg.val.sent <= avg_val_sent_threshold

# Tablica frekvencija: povezanost niske vrijednosti i prevara
tab_val_sent <- table(Low_Avg_Val_Sent, FLAG)
print(tab_val_sent)

# Proporcije po grupama
print(prop.table(tab_val_sent, 1))

# Chi-square test
chi_test_val_sent <- chisq.test(tab_val_sent)
print(chi_test_val_sent)

# Barplot s brojevima iznad stupaca
bp <- barplot(t(tab_val_sent),
              beside = TRUE,
              col = c("lightgreen", "salmon"),
              legend.text = c("Legitimni", "Prevarantski"),
              names.arg = c("VISOKA", "NISKA"),
              main = "Povezanost prosječne vrijednosti transakcija i statusa računa",
              xlab = "Prosječna vrijednost transakcija",
              ylab = "Broj računa",
              ylim = c(0, max(t(tab_val_sent)) * 1.2))  # dodano 20% prostora iznad stupaca

# Dodavanje brojeva iznad stupaca
text(x = bp,
     y = t(tab_val_sent),
     labels = t(tab_val_sent),
     pos = 3, cex = 0.8, col = "black")


#----------------------------------------------------------------


avg_time_sent <- Avg.min.between.sent.tnx
flag <- as.factor(FLAG)

# Definiranje zajedničkog raspona x-osi
x_min <- floor(min(avg_time_sent))
x_max <- ceiling(max(avg_time_sent))
xlim_range <- c(x_min, x_max)

# Određeni uži raspon za x-os
short_xlim <- c(0, 20000)  

# Postavi layout
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)
options(scipen = 5)  # isključi znanstvenu notaciju

# Histogram za legitimne račune
hist(avg_time_sent[flag == 0],
     main = "Legitimni računi",
     xlab = "Prosječno vrijeme slanja (sekunde)",
     col = "lightblue",
     xlim = short_xlim,
     breaks = 50,
     xaxt = "n")

axis(1, at = pretty(short_xlim), labels = pretty(short_xlim))

# Histogram za prevarantske račune
hist(avg_time_sent[flag == 1],
     main = "Prevarantski računi",
     xlab = "Prosječno vrijeme slanja (sekunde)",
     col = "salmon",
     xlim = short_xlim,
     breaks = 50,
     xaxt = "n")

axis(1, at = pretty(short_xlim), labels = pretty(short_xlim))

# Provjera normalnosti QQ-dijagramom (slika 6) – prikaz skraćen do x = 2
par(mfrow = c(1, 2))

qqnorm(avg_time_sent[flag == 0],
       main = "QQ-plot (legitimni)",
       xlim = c(0, 2))
qqline(avg_time_sent[flag == 0], col = "blue")

qqnorm(avg_time_sent[flag == 1],
       main = "QQ-plot (prevarantski)",
       xlim = c(0, 2))
qqline(avg_time_sent[flag == 1], col = "red")

# Provjera normalnosti shapiro-testom:
# Nulta hipoteza (H0): Podaci dolaze iz normalne distribucije.
# Alternativna hipoteza (H1): Podaci ne dolaze iz normalne distribucije.
set.seed(42)  
sample_legit <- sample(avg_time_sent[flag == 0], 5000) #Shapiro-Wilk test radi samo za uzorke do 5000, pa uzimam slučajan uzorak 5000 elemenata
shapiro.test(sample_legit)
shapiro.test(avg_time_sent[flag == 1])
# Pošto su p-vrijednosti puno manje od uobičajene razine značajnosti 0.05, odbacujemo nultu hipotezu.
# Zaključak: podaci za obje grupe nisu normalno distribuirani - ne koristim t-test

dev.off()

# napravi ECDF objekte
ecdf0 <- ecdf(group0)
ecdf1 <- ecdf(group1)

# iscrtaj prvi ECDF
plot(ecdf0,
     col = "blue",
     main = "Empirijske distribucije",
     xlab = "Prosječno vrijeme",
     ylab = "ECDF",
     lty = 1,
     lwd = 2,
     do.points = FALSE)   # bez točkica, samo linija

# dodaj drugi ECDF
lines(ecdf1,
      col = "red",
      lty = 1,
      lwd = 2,
      do.points = FALSE)

legend("bottomright",
       legend = c("Legitimni", "Prevarantski"),
       col = c("blue", "red"),
       lty = 1,
       lwd = 2)

par(mfrow = c(1, 2))

# Boxplot za legitimne račune (slika 8)
boxplot(group0,
        main = "Legitimni računi",
        col = "lightblue",
        ylab = "Prosječno vrijeme (u minutama)")

# Boxplot za prevarantske račune
boxplot(group1,
        main = "Prevarantski računi",
        col = "salmon",
        ylab = "Prosječno vrijeme (u minutama)")
# Uočavamo velik broj ekstremnih vrijednosti.
# Većina podataka ima slične, relativno male vrijednosti, no postoji značajan broj jako velikih vrijednosti koje odskaču.
# Ovo upućuje na jako asimetričnu distribuciju, s dugim desnim repom 
# Također, medijan je jako blizu dnu grafa. 
# Znači da više od 50 % vrijednosti je nisko (blizu nule), a samo mali dio podataka ima visoke vrijednosti koje "podižu" ukupni raspon.
# To dodatno upućuje na asimetričnu distribuciju.



# H0: Distribucije prosječnog vremena između transakcija za legitimne i prevarantske račune su iste (nema razlike).
# H1: Distribucije se razlikuju (postoji statistički značajna razlika).

wilcox_test_result <- wilcox.test(group0, group1)
print(wilcox_test_result)
# p-value < 2.2e-16
# Na temelju ove vrlo niske p-vrijednosti odbacujemo nultu hipotezu (H0) da su distribucije prosječnog vremena između poslanih transakcija jednake za legitimne i prevarantske račune.
# To znači da postoji značajna razlika u prosječnom vremenu između transakcija za ove dvije grupe.



#-------------------------------------------

unique_sent <- Unique.Sent.To.Addresses
flag <- as.factor(FLAG)

# Osnovne statistike po grupama
summary_legit <- summary(unique_sent[flag == 0])
summary_fraud <- summary(unique_sent[flag == 1])
print("Statistika za legitimne račune:")
print(summary_legit)
print("Statistika za prevarantske račune:")
print(summary_fraud)

# Boxplot
par(mfrow = c(1,1))
boxplot(unique_sent[flag == 0], unique_sent[flag == 1],
        col = c("lightblue", "salmon"),
        names = c("Legitimni", "Prevarantski"),
        ylab = "Broj jedinstvenih adresa poslanih transakcija",
        main = "Kutijasti dijagrami broja jedinstvenih primatelja")


# Provjerimo je li distribucija normalna:
# Pretpostavljam da su varijable već definirane
# unique_sent i flag

par(mfrow = c(1, 2))

# Histogrami
hist(unique_sent[flag == 0], main = "Legitimni računi - histogram", xlab = "Broj jedinstvenih adresa", col = "lightblue")
hist(unique_sent[flag == 1], main = "Prevarantski računi - histogram", xlab = "Broj jedinstvenih adresa", col = "salmon")

# QQ dijagrami
par(mfrow = c(1, 2))
qqnorm(unique_sent[flag == 0], main = "Legitimni računi - QQ dijagram")
qqline(unique_sent[flag == 0], col = "blue")

qqnorm(unique_sent[flag == 1], main = "Prevarantski računi - QQ dijagram")
qqline(unique_sent[flag == 1], col = "red")

# Shapiro-Wilk test (za uzorke do 5000)
set.seed(42)
sample_legit <- sample(unique_sent[flag == 0], min(5000, length(unique_sent[flag == 0])))
sample_fraud <- sample(unique_sent[flag == 1], min(5000, length(unique_sent[flag == 1])))

shapiro_legit <- shapiro.test(sample_legit)
shapiro_fraud <- shapiro.test(sample_fraud)

print("Shapiro-Wilk test za legitimne račune:")
print(shapiro_legit) # p-value < 2.2e-16

print("Shapiro-Wilk test za prevarantske račune:")
print(shapiro_fraud) # p-value < 2.2e-16

# Odbacujemo hipotezu o normalnoj distribuciji podataka za obje grupe.


# Najprije uspoređujem ecdf funkcije, ako su istog oblika - primjenjujem wilcox.test
par(mfrow = c(1,1))  
plot(ecdf(unique_sent[flag == 0]), col = "blue", main = "Empirijske distribucije", 
     xlab = "Broj jedinstvenih adresa", ylab = "ECDF", lwd = 2)
lines(ecdf(unique_sent[flag == 1]), col = "red", lwd = 2)
legend("bottomright", legend = c("Legitimni", "Prevarantski"), 
       col = c("blue", "red"), lwd = 2)



# Wilcoxon test
# H0: Distribucije broja jedinstvenih adresa kojima se šalju transakcije su jednake za legitimne i prevarantske račune.
# H1: Distribucije broja jedinstvenih adresa razlikuju se između legitimnih i prevarantskih računa.
wilcox_res <- wilcox.test(unique_sent[flag == 0], unique_sent[flag == 1])
print(wilcox_res)
#p-value < 2.2e-16




#------------------------------------------------------------


# Matrica
mat <- cbind(Sent.tnx,
             Received.Tnx,
             Number.of.Created.Contracts,
             total.Ether.sent,
             total.ether.received,
             total.ether.balance)

# Korelacijska matrica
cor_matrix <- cor(mat, use = "complete.obs")

# Heatmap
heatmap(cor_matrix,
        main = "Toplinska karta glavnih varijabli",
        col = heat.colors(10),
        symm = TRUE)

dev.off()

# 1. total.ether.received vs total.Ether.sent

par(mfrow = c(1, 2))

hist(total.ether.received, 
     main = "Histogram: total.ether.received", 
     col = "lightblue", 
     xlab = "total.ether.received")

shapiro.test(sample(total.ether.received, 5000)) # p-value < 2.2e-16

boxplot(total.ether.received, main = "Boxplot: total.ether.received")
# Većina podataka je niska, blizu nule (kutija je praktički „spljoštena“ pri dnu).
# Nekoliko vrlo visokih vrijednosti su outlieri ili ekstremne vrijednosti daleko iznad ostatka skupa.


# Primjecujemo da je odstupanje od normalnosti veliko, imamo jako asimetricnu distribuciju
# Zato umjesto Pearsonovog testa koristimo Spearmanov (imamo veliki uzorak n >> 10)


cor.test(total.ether.received, total.Ether.sent, method = "spearman")
# p-value < 2.2e-16
# rezultat nije egzaktno izračunat, već aproksimiran

# 2. total.ether.received vs total.ether.balance

# Za varijablu total.ether.received smo se već uvjerili da nije normalno distribuirana, pa koristimo Spearmanov test

cor.test(total.ether.received, total.ether.balance, method = "spearman")
# p-value < 2.2e-16
# rezultat nije egzaktno izračunat, već aproksimiran



# 3. total.Ether.sent vs total.ether.balance
par(mfrow = c(1, 2))

qqnorm(total.Ether.sent, main = "QQ-plot: total.Ether.sent")
qqline(total.Ether.sent, col = "red")
shapiro.test(sample(total.Ether.sent, 5000)) # p-value < 2.2e-16


qqnorm(total.ether.balance, main = "QQ-plot: total.ether.balance")
qqline(total.ether.balance, col = "red")
shapiro.test(sample(total.ether.balance, 5000)) # p-value < 2.2e-16

# U oba slučaja temeljem Q-Q dijagrama i Shapiro teste odbacujemo pretpostavku o normalnosti distribucije -> koristimo Spearmanov test

cor.test(total.ether.balance, total.Ether.sent, method = "spearman")



#--------------------------------------------------


# Izdvajamo indekse za lažne i legitimne račune
fraud_idx <- which(FLAG == "1")
legit_idx <- which(FLAG == "0")

par(mfrow = c(1, 2))

# FRAUD graf (log10 skala)
plot(log10(total.Ether.sent[fraud_idx] + 1), log10(total.ether.received[fraud_idx] + 1),
     main = "Prevarantski računi (log)",
     xlab = "log(Total Ether Sent + 1)",
     ylab = "log(Total Ether Received + 1)",
     col = "red", pch = 16)

# LEGIT graf (log10 skala)
plot(log10(total.Ether.sent[legit_idx] + 1), log10(total.ether.received[legit_idx] + 1),
     main = "Legitimni računi (log)",
     xlab = "log(Total Ether Sent + 1)",
     ylab = "log(Total Ether Received + 1)",
     col = "blue", pch = 16)


# H0: Ne postoji razlika u srednjoj vrijednosti ukupno primljenog Ethera između prevarantskih i legitimnih računa.
# H1: Prevarantski računi u prosjeku primaju više Ethera od legitimnih računa.
wilcox.test(total.ether.received[fraud_idx], total.ether.received[legit_idx], alternative = "greater")

# H0: Ne postoji razlika u srednjoj vrijednosti ukupno poslanog Ethera između prevarantskih i legitimnih računa.
# H1: Prevarantski računi u prosjeku šalju manje Ethera od legitimnih računa.
wilcox.test(total.Ether.sent[fraud_idx], total.Ether.sent[legit_idx], alternative = "less")





log_sent <- log10(total.Ether.sent + 1)
log_received <- log10(total.ether.received + 1)

prevarantski_sent <- log_sent[FLAG == 1 & !is.na(log_sent)]
legitimni_sent <- log_sent[FLAG == 0 & !is.na(log_sent)]

prevarantski_received <- log_received[FLAG == 1 & !is.na(log_received)]
legitimni_received <- log_received[FLAG == 0 & !is.na(log_received)]

# Gustoće
d1 <- density(prevarantski_sent)
d2 <- density(legitimni_sent)

d3 <- density(prevarantski_received)
d4 <- density(legitimni_received)


par(mfrow = c(1, 2))  

# Gustoća za SENT
plot(d1, col = "red", lwd = 2, xlab = "log10(Total Ether Sent + 1)", 
     main = "Gustoća: Ether Sent")
lines(d2, col = "blue", lwd = 2)
legend("topright", legend = c("Prevarantski", "Legitimni"), col = c("red", "blue"), lwd = 2)

# Gustoća za RECEIVED
plot(d3, col = "red", lwd = 2, xlab = "log10(Total Ether Received + 1)", 
     main = "Gustoća: Ether Received")
lines(d4, col = "blue", lwd = 2)
legend("topright", legend = c("Prevarantski", "Legitimni"), col = c("red", "blue"), lwd = 2)


par(mfrow = c(1, 1))


# Kolmogorov-Smirnov test
# H0: Distribucija ukupno poslanog Ethera (nakon log-transformacije) jednaka je za prevarantske i legitimne račune.
# H1: Distribucije se razlikuju.
ks.test(prevarantski_sent, legitimni_sent)
# p-value < 2.2e-16


#------------------------------------------------


# Fitaj logistički model
model <- glm(FLAG ~ total.Ether.sent +
               total.ether.received +
               Unique.Sent.To.Addresses +
               Unique.Received.From.Addresses +
               Number.of.Created.Contracts +
               avg.val.sent +
               avg.val.received +
               ERC20.total.ether.sent +
               ERC20.total.Ether.received +
               ERC20.uniq.sent.addr
             , data = data, family = binomial)

# Sažetak modela
summary(model)

# Predikcija vjerojatnosti klase 1
pred_probs <- predict(model, newdata = data, type = "response")

# Pretvaranje vjerojatnosti u klase 0 i 1
pred_classes <- as.numeric(pred_probs > 0.5)

# Ukloni redove s NA u FLAG ili pred_classes
valid_idx <- which(!is.na(pred_classes) & !is.na(data$FLAG))

# Izračunaj točnost samo za valjane indekse
accuracy <- mean(pred_classes[valid_idx] == data$FLAG[valid_idx])


# Ispis točnosti u %
print(paste("Točnost modela:", round(accuracy * 100, 2), "%"))



library(ggplot2)

# Matrica konfuzije (base R)
conf_matrix <- table(
  Predicted = pred_classes[valid_idx],
  Actual = data$FLAG[valid_idx]
)
print(conf_matrix)

# Pretvori u data.frame za ggplot
cm_df <- as.data.frame(conf_matrix)

# Nacrtaj heatmap matricu konfuzije
ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 0.5, size = 5) +
  scale_fill_gradient(low = "lightyellow", high = "yellow") +
  labs(
    title = "Matrica konfuzije",
    x = "Stvarna klasa",
    y = "Predviđena klasa"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",       # bold
      hjust = 0.5,         # centriranje
      size = 14            # veći font
    )
  )



# Definiraj boje na temelju vrijednosti FLAG 
colors <- c("blue", "red")  
point_colors <- colors[FLAG + 1]  # jer je FLAG 0 ili 1, indeksiramo +1

# Plot
plot(pred_probs,
     col = point_colors,
     pch = 16,
     xlab = "Indeks opažanja",
     ylab = "Predviđena vjerojatnost FLAG=1",
     main = "Predviđene vjerojatnosti prema stvarnim klasama",
     log = "y")  # logaritamska skala y-osi

legend("topright", legend = c("FLAG = 0", "FLAG = 1"),
       col = colors, pch = 16)

table(FLAG)
prop.table(table(FLAG))


par(mfrow = c(2, 5), mar = c(4, 4, 2, 1)) 

options(scipen = 8)  # veća vrijednost = manje eksponencijalnog formata

# Nacrtaj pojedinačne boxplotove po klasama FLAG
boxplot(total.Ether.sent ~ FLAG, data = data, 
        main = "total.Ether.sent", col = c("lightblue", "salmon"))

boxplot(total.ether.received ~ FLAG, data = data, 
        main = "total.ether.received", col = c("lightblue", "salmon"))

boxplot(Unique.Sent.To.Addresses ~ FLAG, data = data, 
        main = "Unique.Sent.To.Addresses", col = c("lightblue", "salmon"))

boxplot(Unique.Received.From.Addresses ~ FLAG, data = data, 
        main = "Unique.Received.From.Addresses", col = c("lightblue", "salmon"))

boxplot(Number.of.Created.Contracts ~ FLAG, data = data, 
        main = "Number.of.Created.Contracts", col = c("lightblue", "salmon"))

boxplot(avg.val.sent ~ FLAG, data = data, 
        main = "avg.val.sent", col = c("lightblue", "salmon"))

boxplot(avg.val.received ~ FLAG, data = data, 
        main = "avg.val.received", col = c("lightblue", "salmon"))

boxplot(ERC20.total.ether.sent ~ FLAG, data = data, 
        main = "ERC20.total.ether.sent", col = c("lightblue", "salmon"))

boxplot(ERC20.total.Ether.received ~ FLAG, data = data, 
        main = "ERC20.total.Ether.received", col = c("lightblue", "salmon"))

boxplot(ERC20.uniq.sent.addr ~ FLAG, data = data, 
        main = "ERC20.uniq.sent.addr", col = c("lightblue", "salmon"))
















































#----------------------------------------------------------------------
# VARIJABLA total.Ether.sent


# Funkcija za izvlačenje vodeće znamenke
get_first_digit <- function(x) {
  x <- x[x > 0]
  as.numeric(substr(gsub("^0+|\\.", "", x), 1, 1))
}


# Razdvajanje po grupama
vals_legit <- total.Ether.sent[FLAG == 0]
vals_fraud <- total.Ether.sent[FLAG == 1]

# Dohvaćamo vodeće znamenke
fd_legit <- get_first_digit(vals_legit)
fd_fraud <- get_first_digit(vals_fraud)

# Empirijske frekvencije
freq_legit <- table(factor(fd_legit, levels = 1:9))
freq_fraud <- table(factor(fd_fraud, levels = 1:9))

# Relativne frekvencije
(prop_legit <- prop.table(freq_legit))
(prop_fraud <- prop.table(freq_fraud))

# Benfordove očekivane vrijednosti
(benford_probs <- log10(1 + 1 / (1:9)))

# Chi-square testovi
# H0: Distribucija vodećih znamenki prati Benfordov zakon.
# H1: Distribucija vodećih znamenki odstupa od Benfordovog zakona.
chisq.test(freq_legit, p = benford_probs)
chisq.test(freq_fraud, p = benford_probs)
# Zaključak: Distribucija vodećih znamenki kod legitimnih računa značajno odstupa od Benfordove distribucije.


# Grafički prikaz
df <- data.frame(
  Znamenka = rep(1:9, 3),
  Udio = c(prop_legit, prop_fraud, benford_probs),
  Grupa = rep(c("Legitimni", "Prevarantski", "Benford"), each = 9)
)

barplot(
  height = t(matrix(df$Udio, nrow = 9)),
  beside = TRUE,
  col = c("skyblue", "salmon", "gray80"),
  names.arg = 1:9,
  legend.text = c("Legitimni", "Prevarantski", "Benford"),
  args.legend = list(x = "topright", bty = "n"),
  xlab = "Vodeća znamenka",
  ylab = "Relativna frekvencija",
  main = "Usporedba vodećih znamenki s Benfordovim zakonom"
)



#--------------------------------------------------
#VARIJABLA total.ether.balance



# Razdvajanje po grupama
vals_legit <- total.ether.balance[FLAG == 0]
vals_fraud <- total.ether.balance[FLAG == 1]

# Dohvaćamo vodeće znamenke
fd_legit <- get_first_digit(vals_legit)
fd_fraud <- get_first_digit(vals_fraud)

# Empirijske frekvencije
freq_legit <- table(factor(fd_legit, levels = 1:9))
freq_fraud <- table(factor(fd_fraud, levels = 1:9))

# Relativne frekvencije
(prop_legit <- prop.table(freq_legit))
(prop_fraud <- prop.table(freq_fraud))

# Benfordove očekivane vrijednosti
(benford_probs <- log10(1 + 1 / (1:9)))

# Chi-square testovi
chisq.test(freq_legit, p = benford_probs)
chisq.test(freq_fraud, p = benford_probs)

# Grafički prikaz
df <- data.frame(
  Znamenka = rep(1:9, 3),
  Udio = c(prop_legit, prop_fraud, benford_probs),
  Grupa = rep(c("Legitimni", "Prevarantski", "Benford"), each = 9)
)

barplot(
  height = t(matrix(df$Udio, nrow = 9)),
  beside = TRUE,
  col = c("skyblue", "salmon", "gray80"),
  names.arg = 1:9,
  legend.text = c("Legitimni", "Prevarantski", "Benford"),
  args.legend = list(x = "topright", bty = "n"),
  xlab = "Vodeća znamenka",
  ylab = "Relativna frekvencija",
  main = "Usporedba vodećih znamenki za total.ether.balance s Benfordovim zakonom"
)


#------------------------------------------------------------------
# VARIJABLA avg.val.received


# Razdvajanje po grupama
vals_legit <- avg.val.received[FLAG == 0]
vals_fraud <- avg.val.received[FLAG == 1]

# Dohvaćamo vodeće znamenke
fd_legit <- get_first_digit(vals_legit)
fd_fraud <- get_first_digit(vals_fraud)

# Empirijske frekvencije
freq_legit <- table(factor(fd_legit, levels = 1:9))
freq_fraud <- table(factor(fd_fraud, levels = 1:9))

# Relativne frekvencije
(prop_legit <- prop.table(freq_legit))
(prop_fraud <- prop.table(freq_fraud))

# Benfordove očekivane vrijednosti
(benford_probs <- log10(1 + 1 / (1:9)))

# Chi-square testovi
chisq.test(freq_legit, p = benford_probs)
chisq.test(freq_fraud, p = benford_probs)

# Grafički prikaz
df <- data.frame(
  Znamenka = rep(1:9, 3),
  Udio = c(prop_legit, prop_fraud, benford_probs),
  Grupa = rep(c("Legitimni", "Prevarantski", "Benford"), each = 9)
)

barplot(
  height = t(matrix(df$Udio, nrow = 9)),
  beside = TRUE,
  col = c("skyblue", "salmon", "gray80"),
  names.arg = 1:9,
  legend.text = c("Legitimni", "Prevarantski", "Benford"),
  args.legend = list(x = "topright", bty = "n"),
  xlab = "Vodeća znamenka",
  ylab = "Relativna frekvencija",
  main = "Usporedba vodećih znamenki za avg.val.received s Benfordovim zakonom"
)



#-----------------------------------------------------------------

install.packages("httr")
install.packages("jsonlite")
install.packages("lubridate") # za rad s datumima
install.packages("dplyr")

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(benford.analysis)

data1 <- read_csv("second_order_df.csv")

# --- 2. Filtriraj samo uspješne transakcije (isError == 0) ---
data1 <- data1 %>%
  filter(isError == 0)

# --- 3. Pretvori 'Value' iz wei u ETH ---
data1 <- data1 %>%
  mutate(value_eth = as.numeric(Value) / 1e18)

# --- 4. Pretvori 'TimeStamp' u Date ---
data1 <- data1 %>%
  mutate(date = as_date(as_datetime(as.numeric(TimeStamp))))

# --- 5. Agregiraj dnevno: suma vrijednosti po danu ---
daily <- data1 %>%
  group_by(date) %>%
  summarise(daily_total_eth = sum(value_eth, na.rm = TRUE), .groups = "drop")

# --- 6. Primijeni Benfordovu analizu (1. znamenka) ---
# Ukloni nule jer nemaju vodeću znamenku
daily_nonzero <- daily %>%
  filter(daily_total_eth > 0)

# Pokreni analizu pomoću benford.analysis paketa
bf_result <- benford(daily_nonzero$daily_total_eth, number.of.digits = 1)

# --- 7. Rezultati i testovi ---
summary(bf_result)        # statistički sažetak
plot(bf_result)           # graf distribucije

bf_result$MAD
bf_result$MAD.conformity


#--------------------------------------------------





# Vitalik Buterin (suosnivač Ethereuma): 0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045 (normal transactions) (podaci: 2015. - 2025.)


# --- 1. Učitaj CSV datoteku ---
data <- read_csv("VitalikButerin.csv")

# --- 2. Zbroj 'Value_IN(ETH)' i 'Value_OUT(ETH)' ---
data <- data %>%
  mutate(value_eth = as.numeric(`Value_IN(ETH)`) + as.numeric(`Value_OUT(ETH)`))

# --- 3. Pretvori vremensku oznaku u datum ---
data <- data %>%
  mutate(date = as_date(as_datetime(as.numeric(UnixTimestamp)))) %>%
  filter(value_eth > 0)

# --- 4. Agregiraj po danu ---
daily <- data %>%
  group_by(date) %>%
  summarise(daily_total_eth = sum(value_eth, na.rm = TRUE), .groups = "drop") %>%
  filter(daily_total_eth > 0)

# --- 5. Benfordova analiza na dnevnim sumama ---
bf_result <- benford(daily$daily_total_eth, number.of.digits = 1)

# --- 6. Pretvori u data.frame za ggplot ---
benford_table <- as.data.frame(bf_result$bfd)

# Nazivi stupaca u paketu benford.analysis
actual_col   <- "data.dist.freq"
expected_col <- "benford.dist.freq"

benford_table$actual_pct   <- benford_table[[actual_col]]   * 100
benford_table$expected_pct <- benford_table[[expected_col]] * 100

# --- 7. vizualizacija: stvarno vs. Benford ---
ggplot(benford_table, aes(x = factor(digits))) +
  geom_col(aes(y = actual_pct, fill = "Stvarno"), alpha = 0.7) +
  geom_line(aes(y = expected_pct, group = 1, colour = "Benford"), linewidth = 1.2) +
  scale_y_continuous(
    name = "Učestalost (%)",
    limits = c(0, max(benford_table$actual_pct) * 1.1)
  ) +
  scale_fill_manual(values = c("Stvarno" = "#1f78b4"), name = "") +
  scale_colour_manual(values = c("Benford" = "red"), name = "") +
  labs(
    title = "Vitalik Buterin – dnevne sume ETH transakcija vs. Benford",
    x = "Prva znamenka"
  ) +
  theme_minimal()

# --- 8. Tablični prikaz stvarno vs. očekivano (+ odstupanje) ---
benford_table %>%
  transmute(
    znamenka     = digits,
    stvarno_pct  = round(actual_pct, 2),
    benford_pct  = round(expected_pct, 2),
    odstupanje   = round(actual_pct - expected_pct, 2)
  ) %>%
  arrange(znamenka) %>%
  print()

# --- 9. Ključni pokazatelji ---
cat("MAD:", bf_result$MAD, "\n")
cat("Benford konformnost:", bf_result$MAD.conformity, "\n")


# MAD: 0.108805
# Benford konformnost: Nonconformity
# Zakljucak: Visoko kontrolirani računi s netržišnim ponašanjem imaju izraženo odstupanje od Benfordovog zakona.






# Kraken Hot Wallet (vrlo aktivna burzovna adresa): 0x267be1c1d684f78cb4f6a176c4911b741e4ffdc0 (normal transactions) (podaci: Srpanj 2025.)



# --- 1. Učitaj CSV Kraken hot wallet ---
data <- read_csv("KrakenHotWallet.csv")

# --- 2. Ukupna vrijednost po transakciji ---
data <- data %>%
  mutate(value_eth = as.numeric(`Value_IN(ETH)`) + as.numeric(`Value_OUT(ETH)`),
         datetime = as_datetime(as.numeric(UnixTimestamp))) %>%
  filter(value_eth > 0)

# --- 3. Benford analiza po transakcijama ---
bf_tx <- benford(data1$value_eth, number.of.digits = 1)
print(names(bf_tx$bfd))  # ispiši sve nazive stupaca u bfd tablici

# --- 4. Ispis osnovnih rezultata ---
cat("----- BENFORD PO TRANSAKCIJAMA -----\n")
summary(bf_tx)
cat("MAD:", bf_tx$MAD, "\n")
cat("Benford konformnost:", bf_tx$MAD.conformity, "\n")

# --- 5. Vizualizacija: osnovni plot iz paketa ---
plot(bf_tx)

# --- 6. Napredna vizualizacija: distribucija prve znamenke ----
benford_table <- as.data.frame(bf_tx$bfd)

# Postavi nazive stupaca ručno za tvoju verziju paketa
actual_col   <- "data.dist.freq"
expected_col <- "benford.dist.freq"

# Pretvori u postotke
benford_table$actual_pct   <- benford_table[[actual_col]]   * 100
benford_table$expected_pct <- benford_table[[expected_col]] * 100

# Nacrtaj
ggplot(benford_table, aes(x = factor(digits))) +
  geom_col(aes(y = actual_pct, fill = "Stvarno"), alpha = 0.7) +
  geom_line(aes(y = expected_pct, group = 1, colour = "Benford"), linewidth = 1.2) +
  scale_y_continuous(
    name = "Učestalost (%)",
    limits = c(0, max(benford_table$actual_pct) * 1.1)
  ) +
  scale_fill_manual(values = c("Stvarno" = "#1f78b4"), name = "") +
  scale_colour_manual(values = c("Benford" = "red"), name = "") +
  labs(
    title = "Kraken Hot Wallet – prva znamenka vs. Benford",
    x = "Prva znamenka"
  ) +
  theme_minimal()


# --- 7. Tablični prikaz stvarno vs. očekivano (+ odstupanje) -----------
benford_table %>%
  transmute(
    znamenka     = digits,
    stvarno_pct  = round(actual_pct, 2),
    benford_pct  = round(expected_pct, 2),
    odstupanje   = round(actual_pct - expected_pct, 2)
  ) %>%
  arrange(znamenka) %>%
  print()

# --- 8. T-test i chi-square (opcionalno) ---
bf_tx$stats

# Zakljucak: Kraken hot wallet transakcije generalno slijede Benfordov zakon








# MEV Bot (Maximal Extractable Value): 0x5f65f7b609678448494de4c87521cdf6cef1e932 (token transfers - ERC-20) (podaci: 24.4.2020. - 24.5.2020.)

# dohvat ERC-20 transakcija za MEVBot adresu
library(httr)
library(jsonlite)
library(purrr)

# --- 1. Adresa MEVBot i API ključ ---
address <- "0x5f65f7b609678448494de4c87521cdf6cef1e932"
api_key <- Sys.getenv("API_KEY")

# --- 2. Funkcija za dohvat jedne stranice podataka ---
get_erc20_transfers <- function(address, api_key, page = 1, offset = 1000) {
  base_url <- "https://api.etherscan.io/api"
  
  res <- GET(base_url, query = list(
    module = "account",
    action = "tokentx",
    address = address,
    startblock = 0,
    endblock = 99999999,
    page = page,
    offset = offset,
    sort = "asc",
    apikey = api_key
  ))
  
  data <- content(res, "text", encoding = "UTF-8")
  json <- fromJSON(data)
  
  if(json$status == "1") {
    return(json$result)
  } else {
    return(NULL)
  }
}

# --- 3. Iterativno dohvaćanje svih stranica ---
all_data <- list()
page <- 1
offset <- 1000
repeat {
  cat("Dohvaćam stranicu", page, "\n")
  page_data <- get_erc20_transfers(address, api_key, page, offset)
  
  if(is.null(page_data) || length(page_data) == 0) break
  
  all_data <- append(all_data, list(page_data))
  
  if(length(page_data) < offset) break
  
  page <- page + 1
  Sys.sleep(0.2)
}

# --- 4. Spajanje i priprema podataka ---
df_all <- bind_rows(all_data) %>%
  mutate(
    value_eth = as.numeric(value) / (10 ^ as.numeric(tokenDecimal)),
    datetime = as_datetime(as.numeric(timeStamp))
  ) %>%
  filter(value_eth > 0)

# =======================
# --- 5. BENFORD PO DANU ---
# =======================
daily <- df_all %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(daily_total_eth = sum(value_eth), .groups = "drop") %>%
  filter(daily_total_eth > 0)

bf_day <- benford(daily$daily_total_eth, number.of.digits = 1)

cat("----- BENFORD PO DANU -----\n")
summary(bf_day)
plot(bf_day)
cat("MAD:", bf_day$MAD, "\n")
cat("Benford konformnost:", bf_day$MAD.conformity, "\n\n")

# =======================
# --- 6. BENFORD PO SATU ---
# =======================
hourly <- df_all %>%
  mutate(hour = floor_date(datetime, unit = "hour")) %>%
  group_by(hour) %>%
  summarise(hourly_total_eth = sum(value_eth), .groups = "drop") %>%
  filter(hourly_total_eth > 0)

bf_hour <- benford(hourly$hourly_total_eth, number.of.digits = 1)

cat("----- BENFORD PO SATU -----\n")
summary(bf_hour)
plot(bf_hour)
cat("MAD:", bf_hour$MAD, "\n")
cat("Benford konformnost:", bf_hour$MAD.conformity, "\n\n")

# =====================================
# --- 7. BENFORD PO TRANSAKCIJAMA ---
# =====================================
tx_values <- df_all %>%
  filter(value_eth > 0)

bf_tx <- benford(tx_values$value_eth, number.of.digits = 1)

cat("----- BENFORD PO TRANSAKCIJAMA -----\n")
summary(bf_tx)
plot(bf_tx)
cat("MAD:", bf_tx$MAD, "\n")
cat("Benford konformnost:", bf_tx$MAD.conformity, "\n")

start_date <- min(df_all$datetime)
end_date <- max(df_all$datetime)

cat("Podaci pokrivaju razdoblje od", start_date, "do", end_date, "\n")

as_datetime(1587688713)
as_datetime(1588965648)

# Zakljucak: Nema ljudskog ponašanja – sve je automatizirano pa je ocekivano da nece slijediti Benfordov zakon




# Binance Hot Wallet (jedna od glavnih hot wallet adresa Binance burze): 0x3f5CE5FBFe3E9af3971dD833D26BA9b5C936f0bE (normal transactions) (podaci: 5.8.2017. - 14.8.2017.)


# --- Parametri ---
address <- "0x3f5CE5FBFe3E9af3971dD833D26BA9b5C936f0bE"

# --- Funkcija za dohvat s retry mehanizmom ---
get_transactions <- function(address, api_key, page = 1, offset = 1000, max_attempts = 3) {
  base_url <- "https://api.etherscan.io/api"
  attempt <- 1
  
  while (attempt <= max_attempts) {
    res <- tryCatch({
      GET(base_url, query = list(
        module = "account",
        action = "txlist",
        address = address,
        startblock = 0,
        endblock = 99999999,
        page = page,
        offset = offset,
        sort = "asc",
        apikey = api_key
      ))
    }, error = function(e) NULL)
    
    if (!is.null(res)) {
      data <- content(res, "text", encoding = "UTF-8")
      json <- fromJSON(data)
      
      if (json$status == "1" && json$message == "OK") {
        return(json$result)
      } else if (json$message == "No transactions found") {
        return(NULL)
      }
    }
    
    cat("Greška kod pokušaja", attempt, "- čekam 5 sekundi...\n")
    Sys.sleep(5)
    attempt <- attempt + 1
  }
  
  cat("Neuspješan dohvat nakon", max_attempts, "pokušaja.\n")
  return(NULL)
}

# --- Dohvati maksimalno 5000 transakcija u 5 stranica po 1000 ---
all_data <- list()
for (page in 1:5) {
  cat("Dohvaćam stranicu", page, "\n")
  page_data <- get_transactions(address, api_key, page = page, offset = 1000)
  if (is.null(page_data) || length(page_data) == 0) break
  all_data <- append(all_data, list(page_data))
  Sys.sleep(0.5)
}

# --- Spajanje i osnovna obrada ---
df_all <- bind_rows(all_data) %>%
  mutate(
    value_eth = as.numeric(value) / 1e18,
    datetime = as_datetime(as.numeric(timeStamp))
  ) %>%
  filter(value_eth > 0)

# --- Info ---
cat("\nUkupno dohvaćeno transakcija:", nrow(df_all), "\n")
cat("Podaci od", min(df_all$datetime), "do", max(df_all$datetime), "\n")


# --- Izvuci prvu znamenku ---
df_all <- df_all %>%
  mutate(
    first_digit = as.numeric(substr(as.character(value_eth), 1, 1))
  ) %>%
  filter(!is.na(first_digit) & first_digit != 0)  # isključi 0 ili NA

# --- Opažene frekvencije ---
obs_freq <- df_all %>%
  count(first_digit) %>%
  mutate(freq = n / sum(n))

# --- Benfordove očekivane frekvencije ---
benford_freq <- data.frame(
  first_digit = 1:9,
  expected = log10(1 + 1/(1:9))
)

# --- Spoji za usporedbu ---
compare <- merge(obs_freq, benford_freq, by = "first_digit", all.y = TRUE)
compare$n[is.na(compare$n)] <- 0
compare$freq[is.na(compare$freq)] <- 0

# --- Prikaži u tablici ---
print(compare)

# --- Barplot usporedba ---
ggplot(compare, aes(x = factor(first_digit))) +
  geom_bar(aes(y = freq), stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_point(aes(y = expected), color = "red", size = 3) +
  geom_line(aes(y = expected, group = 1), color = "red", linetype = "dashed") +
  labs(title = "Benfordova analiza prve znamenke vrijednosti transakcija",
       x = "Prva znamenka",
       y = "Frekvencija",
       caption = "Plava: opažene frekvencije, crvena: Benford očekivanja") +
  theme_minimal()

# --- Chi-squared test za statističku provjeru ---
chisq_test <- chisq.test(x = compare$n, p = compare$expected)
print(chisq_test)
# Izračun MAD
mad_value <- mean(abs(compare$freq - compare$expected))
cat("MAD vrijednost:", mad_value, "\n")

# Conformity based on MAD
if (mad_value <= 0.006) {
  conformity <- "Perfect conformity with Benford's Law."
} else if (mad_value <= 0.012) {
  conformity <- "Good conformity with Benford's Law."
} else if (mad_value <= 0.015) {
  conformity <- "Moderate conformity with Benford's Law."
} else {
  conformity <- "Poor conformity or deviation from Benford's Law."
}

cat("Conformity: ", conformity, "\n")

# Print p-value from Chi-squared test for additional statistical insight
cat("Chi-squared test p-value:", chisq_test$p.value, "\n")

cat("Podaci od", min(df_all$datetime), "do", max(df_all$datetime), "\n")

dev.off()



# preskocila
# Coinbase Wallet (decentralizirani, samostalni novčanik): 0x3f5CE5FBFe3E9af3971dD833D26BA9b5C936f0bE (vrijednosti Eth koje su u svakoj transakciji poslane/primljene na tu adresu) (podaci: 5.8.2017. - 14.8.2017.)


# --- Parametri ---
address <- "0x503828976D22510aad0201ac7EC88293211D23Da"

# --- Funkcija za dohvat s retry mehanizmom ---
get_transactions <- function(address, api_key, page = 1, offset = 1000, max_attempts = 3) {
  base_url <- "https://api.etherscan.io/api"
  attempt <- 1
  
  while (attempt <= max_attempts) {
    res <- tryCatch({
      GET(base_url, query = list(
        module = "account",
        action = "txlist",
        address = address,
        startblock = 0,
        endblock = 99999999,
        page = page,
        offset = offset,
        sort = "asc",
        apikey = api_key
      ))
    }, error = function(e) NULL)
    
    if (!is.null(res)) {
      data <- content(res, "text", encoding = "UTF-8")
      json <- fromJSON(data)
      
      if (json$status == "1" && json$message == "OK") {
        return(json$result)
      } else if (json$message == "No transactions found") {
        return(NULL)
      }
    }
    
    cat("Greška kod pokušaja", attempt, "- čekam 5 sekundi...\n")
    Sys.sleep(5)
    attempt <- attempt + 1
  }
  
  cat("Neuspješan dohvat nakon", max_attempts, "pokušaja.\n")
  return(NULL)
}

# --- Dohvati maksimalno 5000 transakcija u 5 stranica po 1000 ---
all_data <- list()
for (page in 1:5) {
  cat("⛏️ Dohvaćam stranicu", page, "\n")
  page_data <- get_transactions(address, api_key, page = page, offset = 1000)
  if (is.null(page_data) || length(page_data) == 0) break
  all_data <- append(all_data, list(page_data))
  Sys.sleep(0.5)
}

# --- Spajanje i osnovna obrada ---
df_all <- bind_rows(all_data) %>%
  mutate(
    value_eth = as.numeric(value) / 1e18,
    datetime = as_datetime(as.numeric(timeStamp))
  ) %>%
  filter(value_eth > 0)

# --- Info ---
cat("\nUkupno dohvaćeno transakcija:", nrow(df_all), "\n")
cat("Podaci od", min(df_all$datetime), "do", max(df_all$datetime), "\n")

library(ggplot2)

# --- Izvuci prvu znamenku ---
df_all <- df_all %>%
  mutate(
    first_digit = as.numeric(substr(as.character(value_eth), 1, 1))
  ) %>%
  filter(!is.na(first_digit) & first_digit != 0)  # isključi 0 ili NA

# --- Opažene frekvencije ---
obs_freq <- df_all %>%
  count(first_digit) %>%
  mutate(freq = n / sum(n))

# --- Benfordove očekivane frekvencije ---
benford_freq <- data.frame(
  first_digit = 1:9,
  expected = log10(1 + 1/(1:9))
)

# --- Spoji za usporedbu ---
compare <- merge(obs_freq, benford_freq, by = "first_digit", all.y = TRUE)
compare$n[is.na(compare$n)] <- 0
compare$freq[is.na(compare$freq)] <- 0

# --- Prikaži u tablici ---
print(compare)

# --- Barplot usporedba ---
ggplot(compare, aes(x = factor(first_digit))) +
  geom_bar(aes(y = freq), stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_point(aes(y = expected), color = "red", size = 3) +
  geom_line(aes(y = expected, group = 1), color = "red", linetype = "dashed") +
  labs(title = "Benfordova analiza prve znamenke vrijednosti transakcija",
       x = "Prva znamenka",
       y = "Frekvencija",
       caption = "Plava: opažene frekvencije, crvena: Benford očekivanja") +
  theme_minimal()

# --- Chi-squared test za statističku provjeru ---
chisq_test <- chisq.test(x = compare$n, p = compare$expected)
print(chisq_test)
# Izračun MAD
mad_value <- mean(abs(compare$freq - compare$expected))
cat("MAD vrijednost:", mad_value, "\n")

# Conformity based on MAD
if (mad_value <= 0.006) {
  conformity <- "Perfect conformity with Benford's Law."
} else if (mad_value <= 0.012) {
  conformity <- "Good conformity with Benford's Law."
} else if (mad_value <= 0.015) {
  conformity <- "Moderate conformity with Benford's Law."
} else {
  conformity <- "Poor conformity or deviation from Benford's Law."
}

cat("Conformity: ", conformity, "\n")

# Print p-value from Chi-squared test for additional statistical insight
cat("Chi-squared test p-value:", chisq_test$p.value, "\n")

cat("Podaci od", min(df_all$datetime), "do", max(df_all$datetime), "\n")

#promatran premalen interval - samo 4 transakcije pa preskačem u analizi


















# ANALIZA POZNATIH PREVARANTSKIH ADRESA









# Fake MyEtherWallet scam (phishing adresa): 0xB3764761E297D6f121e79C32A65829Cd1dDb4D32 (vrijednosti ETH transakcija) (podaci: 18.7.2017. - 20.3.2025.)


# --- Parametri ---
address <- "0xB3764761E297D6f121e79C32A65829Cd1dDb4D32"

# --- Funkcija za dohvat s retry mehanizmom ---
get_transactions <- function(address, api_key, page = 1, offset = 1000, max_attempts = 3) {
  base_url <- "https://api.etherscan.io/api"
  attempt <- 1
  
  while (attempt <= max_attempts) {
    res <- tryCatch({
      GET(base_url, query = list(
        module = "account",
        action = "txlist",
        address = address,
        startblock = 0,
        endblock = 99999999,
        page = page,
        offset = offset,
        sort = "asc",
        apikey = api_key
      ))
    }, error = function(e) NULL)
    
    if (!is.null(res)) {
      data <- content(res, "text", encoding = "UTF-8")
      json <- fromJSON(data)
      
      if (json$status == "1" && json$message == "OK") {
        return(json$result)
      } else if (json$message == "No transactions found") {
        return(NULL)
      }
    }
    
    cat("Greška kod pokušaja", attempt, "- čekam 5 sekundi...\n")
    Sys.sleep(5)
    attempt <- attempt + 1
  }
  
  cat("Neuspješan dohvat nakon", max_attempts, "pokušaja.\n")
  return(NULL)
}

# --- Dohvati maksimalno 5000 transakcija u 5 stranica po 1000 ---
all_data <- list()
for (page in 1:5) {
  cat("Dohvaćam stranicu", page, "\n")
  page_data <- get_transactions(address, api_key, page = page, offset = 1000)
  if (is.null(page_data) || length(page_data) == 0) break
  all_data <- append(all_data, list(page_data))
  Sys.sleep(0.5)
}

# --- Spajanje i osnovna obrada ---
df_all <- bind_rows(all_data) %>%
  mutate(
    value_eth = as.numeric(value) / 1e18,
    datetime = as_datetime(as.numeric(timeStamp))
  ) %>%
  filter(value_eth > 0)

# --- Info ---
cat("\nUkupno dohvaćeno transakcija:", nrow(df_all), "\n")
cat("Podaci od", min(df_all$datetime), "do", max(df_all$datetime), "\n")

library(ggplot2)

# --- Izvuci prvu znamenku ---
df_all <- df_all %>%
  mutate(
    first_digit = as.numeric(substr(as.character(value_eth), 1, 1))
  ) %>%
  filter(!is.na(first_digit) & first_digit != 0)  # isključi 0 ili NA

# --- Opažene frekvencije ---
obs_freq <- df_all %>%
  count(first_digit) %>%
  mutate(freq = n / sum(n))

# --- Benfordove očekivane frekvencije ---
benford_freq <- data.frame(
  first_digit = 1:9,
  expected = log10(1 + 1/(1:9))
)

# --- Spoji za usporedbu ---
compare <- merge(obs_freq, benford_freq, by = "first_digit", all.y = TRUE)
compare$n[is.na(compare$n)] <- 0
compare$freq[is.na(compare$freq)] <- 0

# --- Prikaži u tablici ---
print(compare)

# --- Barplot usporedba ---
ggplot(compare, aes(x = factor(first_digit))) +
  geom_bar(aes(y = freq), stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_point(aes(y = expected), color = "red", size = 3) +
  geom_line(aes(y = expected, group = 1), color = "red", linetype = "dashed") +
  labs(title = "Benfordova analiza prve znamenke vrijednosti transakcija",
       x = "Prva znamenka",
       y = "Frekvencija",
       caption = "Plava: opažene frekvencije, crvena: Benford očekivanja") +
  theme_minimal()

# --- Chi-squared test za statističku provjeru ---
chisq_test <- chisq.test(x = compare$n, p = compare$expected)
print(chisq_test)
# Izračun MAD
mad_value <- mean(abs(compare$freq - compare$expected))
cat("MAD vrijednost:", mad_value, "\n")

# Conformity based on MAD
if (mad_value <= 0.006) {
  conformity <- "Perfect conformity with Benford's Law."
} else if (mad_value <= 0.012) {
  conformity <- "Good conformity with Benford's Law."
} else if (mad_value <= 0.015) {
  conformity <- "Moderate conformity with Benford's Law."
} else {
  conformity <- "Poor conformity or deviation from Benford's Law."
}

cat("Conformity: ", conformity, "\n")

# Print p-value from Chi-squared test for additional statistical insight
cat("Chi-squared test p-value:", chisq_test$p.value, "\n")

cat("Podaci od", min(df_all$datetime), "do", max(df_all$datetime), "\n")






#Giveaway Scam (Elon Musk Twitter scams): 0x281055afc982d96fab65b3a49cac8b878184cb16 (vrijednosti transakcija) (podaci: 20.8.2016. - 1.12.2018.)


# --- Parametri ---
address <- "0x281055afc982d96fab65b3a49cac8b878184cb16"

# --- Funkcija za dohvat s retry mehanizmom ---
get_transactions <- function(address, api_key, page = 1, offset = 1000, max_attempts = 3) {
  base_url <- "https://api.etherscan.io/api"
  attempt <- 1
  
  while (attempt <= max_attempts) {
    res <- tryCatch({
      GET(base_url, query = list(
        module = "account",
        action = "txlist",
        address = address,
        startblock = 0,
        endblock = 99999999,
        page = page,
        offset = offset,
        sort = "asc",
        apikey = api_key
      ))
    }, error = function(e) NULL)
    
    if (!is.null(res)) {
      data <- content(res, "text", encoding = "UTF-8")
      json <- fromJSON(data)
      
      if (json$status == "1" && json$message == "OK") {
        return(json$result)
      } else if (json$message == "No transactions found") {
        return(NULL)
      }
    }
    
    cat("Greška kod pokušaja", attempt, "- čekam 5 sekundi...\n")
    Sys.sleep(5)
    attempt <- attempt + 1
  }
  
  cat("Neuspješan dohvat nakon", max_attempts, "pokušaja.\n")
  return(NULL)
}

# --- Dohvati maksimalno 5000 transakcija u 5 stranica po 1000 ---
all_data <- list()
for (page in 1:5) {
  cat("⛏️ Dohvaćam stranicu", page, "\n")
  page_data <- get_transactions(address, api_key, page = page, offset = 1000)
  if (is.null(page_data) || length(page_data) == 0) break
  all_data <- append(all_data, list(page_data))
  Sys.sleep(0.5)
}

# --- Spajanje i osnovna obrada ---
df_all <- bind_rows(all_data) %>%
  mutate(
    value_eth = as.numeric(value) / 1e18,
    datetime = as_datetime(as.numeric(timeStamp))
  ) %>%
  filter(value_eth > 0)

# --- Info ---
cat("\nUkupno dohvaćeno transakcija:", nrow(df_all), "\n")
cat("Podaci od", min(df_all$datetime), "do", max(df_all$datetime), "\n")

library(ggplot2)

# --- Izvuci prvu znamenku ---
df_all <- df_all %>%
  mutate(
    first_digit = as.numeric(substr(as.character(value_eth), 1, 1))
  ) %>%
  filter(!is.na(first_digit) & first_digit != 0)  # isključi 0 ili NA

# --- Opažene frekvencije ---
obs_freq <- df_all %>%
  count(first_digit) %>%
  mutate(freq = n / sum(n))

# --- Benfordove očekivane frekvencije ---
benford_freq <- data.frame(
  first_digit = 1:9,
  expected = log10(1 + 1/(1:9))
)

# --- Spoji za usporedbu ---
compare <- merge(obs_freq, benford_freq, by = "first_digit", all.y = TRUE)
compare$n[is.na(compare$n)] <- 0
compare$freq[is.na(compare$freq)] <- 0

# --- Prikaži u tablici ---
print(compare)

# --- Barplot usporedba ---
ggplot(compare, aes(x = factor(first_digit))) +
  geom_bar(aes(y = freq), stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_point(aes(y = expected), color = "red", size = 3) +
  geom_line(aes(y = expected, group = 1), color = "red", linetype = "dashed") +
  labs(title = "Benfordova analiza prve znamenke vrijednosti transakcija",
       x = "Prva znamenka",
       y = "Frekvencija",
       caption = "Plava: opažene frekvencije, crvena: Benford očekivanja") +
  theme_minimal()

# --- Chi-squared test za statističku provjeru ---
chisq_test <- chisq.test(x = compare$n, p = compare$expected)
print(chisq_test)
# Izračun MAD
mad_value <- mean(abs(compare$freq - compare$expected))
cat("MAD vrijednost:", mad_value, "\n")

# Conformity based on MAD
if (mad_value <= 0.006) {
  conformity <- "Perfect conformity with Benford's Law."
} else if (mad_value <= 0.012) {
  conformity <- "Good conformity with Benford's Law."
} else if (mad_value <= 0.015) {
  conformity <- "Moderate conformity with Benford's Law."
} else {
  conformity <- "Poor conformity or deviation from Benford's Law."
}

cat("Conformity: ", conformity, "\n")

# Print p-value from Chi-squared test for additional statistical insight
cat("Chi-squared test p-value:", chisq_test$p.value, "\n")

cat("Podaci od", min(df_all$datetime), "do", max(df_all$datetime), "\n")




# dohvat ERC-20 transakcija za Giveaway Scam (Elon Musk Twitter scams) adresu (podaci: 21.07.2017. - 11.07.2025.)


address <- "0x281055afc982d96fab65b3a49cac8b878184cb16"

# --- 2. Funkcija za dohvat jedne stranice podataka ---
get_erc20_transfers <- function(address, api_key, page = 1, offset = 1000) {
  base_url <- "https://api.etherscan.io/api"
  
  res <- GET(base_url, query = list(
    module = "account",
    action = "tokentx",
    address = address,
    startblock = 0,
    endblock = 99999999,
    page = page,
    offset = offset,
    sort = "asc",
    apikey = api_key
  ))
  
  data <- content(res, "text", encoding = "UTF-8")
  json <- fromJSON(data)
  
  if(json$status == "1") {
    return(json$result)
  } else {
    return(NULL)
  }
}

# --- 3. Iterativno dohvaćanje svih stranica ---
all_data <- list()
page <- 1
offset <- 1000
repeat {
  cat("Dohvaćam stranicu", page, "\n")
  page_data <- get_erc20_transfers(address, api_key, page, offset)
  
  if(is.null(page_data) || length(page_data) == 0) break
  
  all_data <- append(all_data, list(page_data))
  
  if(length(page_data) < offset) break
  
  page <- page + 1
  Sys.sleep(0.2)
}

# --- 4. Spajanje i priprema podataka ---
df_all <- bind_rows(all_data) %>%
  mutate(
    value_eth = as.numeric(value) / (10 ^ as.numeric(tokenDecimal)),
    datetime = as_datetime(as.numeric(timeStamp))
  ) %>%
  filter(value_eth > 0)

# =======================
# --- 5. BENFORD PO DANU ---
# =======================
daily <- df_all %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(daily_total_eth = sum(value_eth), .groups = "drop") %>%
  filter(daily_total_eth > 0)

bf_day <- benford(daily$daily_total_eth, number.of.digits = 1)

cat("----- BENFORD PO DANU -----\n")
summary(bf_day)
plot(bf_day)
cat("MAD:", bf_day$MAD, "\n")
cat("Benford konformnost:", bf_day$MAD.conformity, "\n\n")

# =======================
# --- 6. BENFORD PO SATU ---
# =======================
hourly <- df_all %>%
  mutate(hour = floor_date(datetime, unit = "hour")) %>%
  group_by(hour) %>%
  summarise(hourly_total_eth = sum(value_eth), .groups = "drop") %>%
  filter(hourly_total_eth > 0)

bf_hour <- benford(hourly$hourly_total_eth, number.of.digits = 1)

cat("----- BENFORD PO SATU -----\n")
summary(bf_hour)
plot(bf_hour)
cat("MAD:", bf_hour$MAD, "\n")
cat("Benford konformnost:", bf_hour$MAD.conformity, "\n\n")

# =====================================
# --- 7. BENFORD PO TRANSAKCIJAMA ---
# =====================================
tx_values <- df_all %>%
  filter(value_eth > 0)

bf_tx <- benford(tx_values$value_eth, number.of.digits = 1)

cat("----- BENFORD PO TRANSAKCIJAMA -----\n")
summary(bf_tx)
plot(bf_tx)
cat("MAD:", bf_tx$MAD, "\n")
cat("Benford konformnost:", bf_tx$MAD.conformity, "\n")

start_date <- min(df_all$datetime)
end_date <- max(df_all$datetime)

cat("Podaci pokrivaju razdoblje od", start_date, "do", end_date, "\n")





# Fake ICO scam (Lažna ICO kampanja): 0x71C7656EC7ab88b098defB751B7401B5f6d8976F (vrijednosti transakcija) (podaci: 20.8.2016. - 10.5.2024.)


address <- "0x281055afc982d96fab65b3a49cac8b878184cb16"

# --- Funkcija za dohvat normalnih (ETH) transakcija ---
get_eth_transactions <- function(address, api_key, page = 1, offset = 10000) {
  base_url <- "https://api.etherscan.io/api"
  
  res <- GET(base_url, query = list(
    module = "account",
    action = "txlist",    # za normalne ETH transakcije
    address = address,
    startblock = 0,
    endblock = 99999999,
    page = page,
    offset = offset,
    sort = "asc",
    apikey = api_key
  ))
  
  data <- content(res, "text", encoding = "UTF-8")
  json <- fromJSON(data)
  
  if (json$status == "1") {
    return(json$result)
  } else {
    return(NULL)
  }
}

# --- Iterativno dohvaćanje svih stranica ---
all_data <- list()
page <- 1
offset <- 10000
repeat {
  cat("Dohvaćam stranicu", page, "\n")
  page_data <- get_eth_transactions(address, api_key, page, offset)
  
  if (is.null(page_data) || length(page_data) == 0) break
  
  all_data <- append(all_data, list(page_data))
  
  if (length(page_data) < offset) break
  
  page <- page + 1
  Sys.sleep(0.3)  # malo duže čekanje zbog rate limita
}

# --- Spajanje i priprema podataka ---
df_all <- bind_rows(all_data) %>%
  mutate(
    value_eth = as.numeric(value) / 1e18,   # pretvaranje wei u ETH
    datetime = as_datetime(as.numeric(timeStamp))
  ) %>%
  filter(value_eth > 0)

# =======================
# --- BENFORD PO DANU ---
# =======================
daily <- df_all %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(daily_total_eth = sum(value_eth), .groups = "drop") %>%
  filter(daily_total_eth > 0)

bf_day <- benford(daily$daily_total_eth, number.of.digits = 1)

cat("----- BENFORD PO DANU -----\n")
summary(bf_day)
plot(bf_day)
cat("MAD:", bf_day$MAD, "\n")
cat("Benford konformnost:", bf_day$MAD.conformity, "\n\n")

# =======================
# --- BENFORD PO SATU ---
# =======================
hourly <- df_all %>%
  mutate(hour = floor_date(datetime, unit = "hour")) %>%
  group_by(hour) %>%
  summarise(hourly_total_eth = sum(value_eth), .groups = "drop") %>%
  filter(hourly_total_eth > 0)

bf_hour <- benford(hourly$hourly_total_eth, number.of.digits = 1)

cat("----- BENFORD PO SATU -----\n")
summary(bf_hour)
plot(bf_hour)
cat("MAD:", bf_hour$MAD, "\n")
cat("Benford konformnost:", bf_hour$MAD.conformity, "\n\n")

# ============================
# --- BENFORD PO TRANSAKCIJAMA ---
# ============================
tx_values <- df_all %>%
  filter(value_eth > 0)

bf_tx <- benford(tx_values$value_eth, number.of.digits = 1)

cat("----- BENFORD PO TRANSAKCIJAMA -----\n")
summary(bf_tx)
plot(bf_tx)
cat("MAD:", bf_tx$MAD, "\n")
cat("Benford konformnost:", bf_tx$MAD.conformity, "\n")

start_date <- min(df_all$datetime)
end_date <- max(df_all$datetime)

cat("Podaci pokrivaju razdoblje od", as_datetime(start_date), "do", as_datetime(end_date), "\n")







# Fake Uniswap Airdrop (veći scam u povijesti kripto): 0xd551234ae421e3bcba99a0da6d736074f22192ff (vrijednosti transakcija (podaci: 7.1.2017. - 9.1.2018.)


address <- "0xd551234ae421e3bcba99a0da6d736074f22192ff"

# --- Funkcija za dohvat normalnih (ETH) transakcija ---
get_eth_transactions <- function(address, api_key, page = 1, offset = 10000) {
  base_url <- "https://api.etherscan.io/api"
  
  res <- GET(base_url, query = list(
    module = "account",
    action = "txlist",    # za normalne ETH transakcije
    address = address,
    startblock = 0,
    endblock = 99999999,
    page = page,
    offset = offset,
    sort = "asc",
    apikey = api_key
  ))
  
  data <- content(res, "text", encoding = "UTF-8")
  json <- fromJSON(data)
  
  if (json$status == "1") {
    return(json$result)
  } else {
    return(NULL)
  }
}

# --- Iterativno dohvaćanje svih stranica ---
all_data <- list()
page <- 1
offset <- 10000
repeat {
  cat("Dohvaćam stranicu", page, "\n")
  page_data <- get_eth_transactions(address, api_key, page, offset)
  
  if (is.null(page_data) || length(page_data) == 0) break
  
  all_data <- append(all_data, list(page_data))
  
  if (length(page_data) < offset) break
  
  page <- page + 1
  Sys.sleep(0.3)  # malo duže čekanje zbog rate limita
}

# --- Spajanje i priprema podataka ---
df_all <- bind_rows(all_data) %>%
  mutate(
    value_eth = as.numeric(value) / 1e18,   # pretvaranje wei u ETH
    datetime = as_datetime(as.numeric(timeStamp))
  ) %>%
  filter(value_eth > 0)

# =======================
# --- BENFORD PO DANU ---
# =======================
daily <- df_all %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(daily_total_eth = sum(value_eth), .groups = "drop") %>%
  filter(daily_total_eth > 0)

bf_day <- benford(daily$daily_total_eth, number.of.digits = 1)

cat("----- BENFORD PO DANU -----\n")
summary(bf_day)
plot(bf_day)
cat("MAD:", bf_day$MAD, "\n")
cat("Benford konformnost:", bf_day$MAD.conformity, "\n\n")

nrow(df_all)

# =======================
# --- BENFORD PO SATU ---
# =======================
hourly <- df_all %>%
  mutate(hour = floor_date(datetime, unit = "hour")) %>%
  group_by(hour) %>%
  summarise(hourly_total_eth = sum(value_eth), .groups = "drop") %>%
  filter(hourly_total_eth > 0)

bf_hour <- benford(hourly$hourly_total_eth, number.of.digits = 1)

cat("----- BENFORD PO SATU -----\n")
summary(bf_hour)
plot(bf_hour)
cat("MAD:", bf_hour$MAD, "\n")
cat("Benford konformnost:", bf_hour$MAD.conformity, "\n\n")

# ============================
# --- BENFORD PO TRANSAKCIJAMA ---
# ============================
tx_values <- df_all %>%
  filter(value_eth > 0)

bf_tx <- benford(tx_values$value_eth, number.of.digits = 1)

cat("----- BENFORD PO TRANSAKCIJAMA -----\n")
summary(bf_tx)
plot(bf_tx)
cat("MAD:", bf_tx$MAD, "\n")
cat("Benford konformnost:", bf_tx$MAD.conformity, "\n")

start_date <- min(df_all$datetime)
end_date <- max(df_all$datetime)

cat("Podaci pokrivaju razdoblje od", as_datetime(start_date), "do", as_datetime(end_date), "\n")







# PlusToken (jedna od najvećih Ponzi shema): 0x3f3a8443c3b3f2031d65b7e901b2c9c5c9be614b (vrijednosti transakcija) (podaci: 21.8.2016. - 5.10.2024.)


# --- Parametri ---
address <- "0x281055afc982d96fab65b3a49cac8b878184cb16"

# --- Funkcija za dohvat s retry mehanizmom ---
get_transactions <- function(address, api_key, page = 1, offset = 1000, max_attempts = 3) {
  base_url <- "https://api.etherscan.io/api"
  attempt <- 1
  
  while (attempt <= max_attempts) {
    res <- tryCatch({
      GET(base_url, query = list(
        module = "account",
        action = "txlist",
        address = address,
        startblock = 0,
        endblock = 99999999,
        page = page,
        offset = offset,
        sort = "asc",
        apikey = api_key
      ))
    }, error = function(e) NULL)
    
    if (!is.null(res)) {
      data <- content(res, "text", encoding = "UTF-8")
      json <- fromJSON(data)
      
      if (json$status == "1" && json$message == "OK") {
        return(json$result)
      } else if (json$message == "No transactions found") {
        return(NULL)
      }
    }
    
    cat("Greška kod pokušaja", attempt, "- čekam 5 sekundi...\n")
    Sys.sleep(5)
    attempt <- attempt + 1
  }
  
  cat("Neuspješan dohvat nakon", max_attempts, "pokušaja.\n")
  return(NULL)
}

# --- Dohvati maksimalno 5000 transakcija u 5 stranica po 1000 ---
all_data <- list()
for (page in 1:5) {
  cat("Dohvaćam stranicu", page, "\n")
  page_data <- get_transactions(address, api_key, page = page, offset = 1000)
  if (is.null(page_data) || length(page_data) == 0) break
  all_data <- append(all_data, list(page_data))
  Sys.sleep(0.5)
}

# --- Spajanje i osnovna obrada ---
df_all <- bind_rows(all_data) %>%
  mutate(
    value_eth = as.numeric(value) / 1e18,
    datetime = as_datetime(as.numeric(timeStamp))
  ) %>%
  filter(value_eth > 0)

# --- Info ---
cat("\nUkupno dohvaćeno transakcija:", nrow(df_all), "\n")
cat("Podaci od", min(df_all$datetime), "do", max(df_all$datetime), "\n")


# --- Izvuci prvu znamenku ---
df_all <- df_all %>%
  mutate(
    first_digit = as.numeric(substr(as.character(value_eth), 1, 1))
  ) %>%
  filter(!is.na(first_digit) & first_digit != 0)  # isključi 0 ili NA

# --- Opažene frekvencije ---
obs_freq <- df_all %>%
  count(first_digit) %>%
  mutate(freq = n / sum(n))

# --- Benfordove očekivane frekvencije ---
benford_freq <- data.frame(
  first_digit = 1:9,
  expected = log10(1 + 1/(1:9))
)

# --- Spoji za usporedbu ---
compare <- merge(obs_freq, benford_freq, by = "first_digit", all.y = TRUE)
compare$n[is.na(compare$n)] <- 0
compare$freq[is.na(compare$freq)] <- 0

# --- Prikaži u tablici ---
print(compare)

# --- Barplot usporedba ---
ggplot(compare, aes(x = factor(first_digit))) +
  geom_bar(aes(y = freq), stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_point(aes(y = expected), color = "red", size = 3) +
  geom_line(aes(y = expected, group = 1), color = "red", linetype = "dashed") +
  labs(title = "Benfordova analiza prve znamenke vrijednosti transakcija",
       x = "Prva znamenka",
       y = "Frekvencija",
       caption = "Plava: opažene frekvencije, crvena: Benford očekivanja") +
  theme_minimal()

# --- Chi-squared test za statističku provjeru ---
chisq_test <- chisq.test(x = compare$n, p = compare$expected)
print(chisq_test)
# Izračun MAD
mad_value <- mean(abs(compare$freq - compare$expected))
cat("MAD vrijednost:", mad_value, "\n")

# Conformity based on MAD
if (mad_value <= 0.006) {
  conformity <- "Perfect conformity with Benford's Law."
} else if (mad_value <= 0.012) {
  conformity <- "Good conformity with Benford's Law."
} else if (mad_value <= 0.015) {
  conformity <- "Moderate conformity with Benford's Law."
} else {
  conformity <- "Poor conformity or deviation from Benford's Law."
}

cat("Conformity: ", conformity, "\n")

# Print p-value from Chi-squared test for additional statistical insight
cat("Chi-squared test p-value:", chisq_test$p.value, "\n")

cat("Podaci od", min(df_all$datetime), "do", max(df_all$datetime), "\n")




# zelim analizirati i ostale znacajke poput druge znamenke pomocu benford.analysis


address <- "0x281055afc982d96fab65b3a49cac8b878184cb16"
api_key <- Sys.getenv("API_KEY")

# --- Funkcija za dohvat normalnih (ETH) transakcija ---
get_eth_transactions <- function(address, api_key, page = 1, offset = 10000) {
  base_url <- "https://api.etherscan.io/api"
  
  res <- GET(base_url, query = list(
    module = "account",
    action = "txlist",    # za normalne ETH transakcije
    address = address,
    startblock = 0,
    endblock = 99999999,
    page = page,
    offset = offset,
    sort = "asc",
    apikey = api_key
  ))
  
  data <- content(res, "text", encoding = "UTF-8")
  json <- fromJSON(data)
  
  if (json$status == "1") {
    return(json$result)
  } else {
    return(NULL)
  }
}

# --- Iterativno dohvaćanje svih stranica ---
all_data <- list()
page <- 1
offset <- 10000
repeat {
  cat("Dohvaćam stranicu", page, "\n")
  page_data <- get_eth_transactions(address, api_key, page, offset)
  
  if (is.null(page_data) || length(page_data) == 0) break
  
  all_data <- append(all_data, list(page_data))
  
  if (length(page_data) < offset) break
  
  page <- page + 1
  Sys.sleep(0.3)  # malo duže čekanje zbog rate limita
}

# --- Spajanje i priprema podataka ---
df_all <- bind_rows(all_data) %>%
  mutate(
    value_eth = as.numeric(value) / 1e18,   # pretvaranje wei u ETH
    datetime = as_datetime(as.numeric(timeStamp))
  ) %>%
  filter(value_eth > 0)

# =======================
# --- BENFORD PO DANU ---
# =======================
daily <- df_all %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(daily_total_eth = sum(value_eth), .groups = "drop") %>%
  filter(daily_total_eth > 0)

bf_day <- benford(daily$daily_total_eth, number.of.digits = 1)

cat("----- BENFORD PO DANU -----\n")
summary(bf_day)
plot(bf_day)
cat("MAD:", bf_day$MAD, "\n")
cat("Benford konformnost:", bf_day$MAD.conformity, "\n\n")

# =======================
# --- BENFORD PO SATU ---
# =======================
hourly <- df_all %>%
  mutate(hour = floor_date(datetime, unit = "hour")) %>%
  group_by(hour) %>%
  summarise(hourly_total_eth = sum(value_eth), .groups = "drop") %>%
  filter(hourly_total_eth > 0)

bf_hour <- benford(hourly$hourly_total_eth, number.of.digits = 1)

cat("----- BENFORD PO SATU -----\n")
summary(bf_hour)
plot(bf_hour)
cat("MAD:", bf_hour$MAD, "\n")
cat("Benford konformnost:", bf_hour$MAD.conformity, "\n\n")

# ============================
# --- BENFORD PO TRANSAKCIJAMA ---
# ============================
tx_values <- df_all %>%
  filter(value_eth > 0)

bf_tx <- benford(tx_values$value_eth, number.of.digits = 1)

cat("----- BENFORD PO TRANSAKCIJAMA -----\n")
summary(bf_tx)
plot(bf_tx)
cat("MAD:", bf_tx$MAD, "\n")
cat("Benford konformnost:", bf_tx$MAD.conformity, "\n")

start_date <- min(df_all$datetime)
end_date <- max(df_all$datetime)

cat("Podaci pokrivaju razdoblje od", as_datetime(start_date), "do", as_datetime(end_date), "\n")





  
  #----------------------------------- automatizacija procesa - value varijabla
  
# ANALIZA value VARIJABLE (U ETH) - vrijednost transakcija

library(readr)

# --- Funkcija za dohvat transakcija po adresi ---
get_transactions <- function(address, api_key, startblock = 0, endblock = 99999999, offset = 10000) {
  base_url <- "https://api.etherscan.io/api"
  all_data <- list()
  page <- 1
  
  repeat {
    cat(" Dohvaćam:", address, "- stranica:", page, "\n")
    
    attempt <- 1
    max_attempts <- 3
    res <- NULL
    
    repeat {
      res <- tryCatch({
        GET(base_url,
            query = list(
              module = "account",
              action = "txlist",
              address = address,
              startblock = startblock,
              endblock = endblock,
              page = page,
              offset = offset,
              sort = "asc",
              apikey = api_key
            ),
            timeout(60),
            user_agent("Mozilla/5.0 (BenfordAnalyzer/1.0; +https://yourwebsite.com)")
        )
      }, error = function(e) {
        cat(" Pokušaj", attempt, "nije uspio:", e$message, "\n")
        return(NULL)
      })
      
      if (!is.null(res)) break
      
      if (attempt >= max_attempts) {
        cat(" Neuspješno dohvaćanje podataka za adresu:", address, "nakon", max_attempts, "pokušaja.\n")
        return(NULL)
      }
      
      attempt <- attempt + 1
      Sys.sleep(2)
    }
    
    data <- content(res, "text", encoding = "UTF-8")
    json <- fromJSON(data)
    
    if (json$status != "1" || length(json$result) == 0) break
    
    all_data <- append(all_data, list(json$result))
    if (length(json$result) < offset) break
    
    page <- page + 1
    Sys.sleep(0.3)
  }
  
  if (length(all_data) == 0) return(NULL)
  
  bind_rows(all_data)
}

# --- Glavna funkcija za obradu svih adresa ---
analyze_addresses <- function(csv_path, api_key) {
  addresses_df <- read_csv(csv_path)
  results <- list()
  
  for (i in 1:nrow(addresses_df)) {
    address <- addresses_df$Address[i]
    startblock <- addresses_df$StartBlock[i]
    endblock <- addresses_df$EndBlock[i]
    
    df <- get_transactions(address, api_key, startblock, endblock)
    
    if (is.null(df)) {
      cat(" Nema podataka za adresu:", address, "\n")
      next
    }
    
    df_clean <- df %>%
      mutate(
        value_eth = as.numeric(value) / 1e18,
        datetime = as_datetime(as.numeric(timeStamp))
      ) %>%
      filter(value_eth > 0)
    
    if (nrow(df_clean) == 0) {
      cat("️ Nema transakcija s vrijednošću > 0 za", address, "\n")
      next
    }
    
    benford_test <- benford(df_clean$value_eth, number.of.digits = 1)
    MAD_value <- benford_test$MAD
    
    custom_conformity <- case_when(
      MAD_value <= 0.06 ~ "Strong conformity",
      MAD_value <= 0.10 ~ "Moderate conformity",
      MAD_value <= 0.14 ~ "Weak conformity",
      TRUE              ~ "Nonconformity"
    )
    
    predicted_flag <- ifelse(custom_conformity == "Nonconformity", 1, 0)
    
    result <- tibble(
      Address = address,
      TotalTransactions = nrow(df_clean),
      TotalETH = sum(df_clean$value_eth),
      MedianValue = median(df_clean$value_eth),
      MaxValue = max(df_clean$value_eth),
      MAD = MAD_value,
      Conformity = benford_test$MAD.conformity,
      CustomConformity = custom_conformity,
      PredictedFlag = predicted_flag
    )
    
    results[[length(results) + 1]] <- result
  }
  
  final_df <- bind_rows(results)
  return(final_df)
}

# --- Poziv i spremanje rezultata ---
api_key <- Sys.getenv("API_KEY")
result_df <- analyze_addresses("transaction_dataset.csv", api_key)

# Spremi rezultate u CSV
write_csv(result_df, "benford_analysis_results.csv")
print(result_df)

# --- Sažetak klasifikacija ---
cat("\n Statistika klasifikacija:\n")
print(summary(result_df))
print(table(result_df$CustomConformity))
cat("\n Postotak označenih (PredictedFlag = 1):\n")
print(round(prop.table(table(result_df$PredictedFlag)) * 100, 2))





# graf 1  - Distribucija MAD vrijednosti


ggplot(result_df, aes(x = MAD)) +
  geom_histogram(binwidth = 0.01, fill = "steelblue", color = "black") +
  labs(
    title = "Distribucija MAD vrijednosti po adresama",
    x = "MAD (Mean Absolute Deviation)",
    y = "Broj adresa"
  ) +
  theme_minimal()



# graf 2 - Ukupan broj transakcija po klasi konformnosti

ggplot(result_df, aes(x = CustomConformity, y = TotalTransactions, fill = CustomConformity)) +
  geom_boxplot() +
  labs(
    title = "Broj transakcija po klasi konformnosti",
    x = "Klasa konformnosti",
    y = "Ukupan broj transakcija"
  ) +
  theme_minimal()



# graf 3 - Udio sumnjivih vs. nesumnjivih računa (pie chart)

library(scales)

pie_df <- result_df %>%
  mutate(Label = ifelse(PredictedFlag == 1, "Sumnjivi", "Nesumnjivi")) %>%
  count(Label) %>%
  mutate(Percentage = percent(n / sum(n)))

ggplot(pie_df, aes(x = "", y = n, fill = Label)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = Percentage), position = position_stack(vjust = 0.5)) +
  labs(title = "Udio sumnjivih i nesumnjivih računa") +
  theme_void()




# graf 4 - Barplot: Broj adresa po klasi konformnosti

ggplot(result_df, aes(x = CustomConformity, fill = CustomConformity)) +
  geom_bar() +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    vjust = -0.3   # lagano iznad stupca
  ) +
  labs(
    title = "Broj adresa po klasi konformnosti",
    x = "Klasa konformnosti",
    y = "Broj adresa"
  ) +
  theme_minimal()


dev.off()

# graf 5 - Heatmap (korelacija između MAD i broja transakcija)

ggplot(result_df, aes(x = MAD, y = TotalTransactions)) +
  geom_bin2d(bins = 30) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Toplinska karta: MAD vs. Broj transakcija",
    x = "MAD vrijednost",
    y = "Ukupan broj transakcija"
  ) +
  theme_minimal()

# Gusto zasićeno tamno plavo područje znači:
#  - Postoji mnogo adresa s niskim MAD i manjim brojem transakcija
#  - To su najvjerojatnije legitimne adrese (visoka sukladnost)

# Područja s višim MAD vrijednostima:
#  - Pokazuju adrese koje odstupaju od Benfordove distribucije
#  - Ako su istovremeno i visoko aktivne (više transakcija), to može biti znak za uzbunu.

# Rijetka područja (svijetloplava):
#  - Odnosi se na manje tipične kombinacije, npr. adrese s jako velikim brojem transakcija i visokim MAD-om (desno-gore)
#  - Ovakve adrese mogu biti sumnjive i vrijedne dodatne istrage









# funkcija benford() iz paketa benford.analysis koristi zadane kriterije za kategorizaciju odstupanja od Benfordovog zakona. 
# Te klasifikacije su temeljene na Hillovim pragovima, gdje: *tablica* MAD vrijednost klasifikacija
# ti pragovi su dizajnirani za financijske izvještaje i knjigovodstvo — a ne za transakcije kriptovaluta, 
# stoga sam prilagodila kriterij:


# Predviđeni rezultati (prema prilagođenim pragovima):  Strong conformity: 818
#                                                       Moderate conformity: 1015
#                                                       Weak conformity: 1278
#                                                       Nonconformity: 1052


prop.table(table(FLAG)) # stvarni rezultati

# Stvarni rezultati:     Legitimni: 77.86%
#                        Prevarantski: 22.14%


# Analiziran je podskup od pocetnog skupa podataka (4163 adresa)
# Ako uzmemo da su 1052 (Nonconformity) racuni oznaceni kao sumnjivi, tada je pretpostavka od ukupno 25.27% racuna sumnjivo,
# sto nije daleko od stvarnih rezultata - 22.14%














# preskocila sam
#----------------------------------- automatizacija procesa - value_token


# --- Funkcija za dohvat transakcija po adresi ---
get_transactions <- function(address, api_key, startblock = 0, endblock = 99999999, offset = 10000) {
  base_url <- "https://api.etherscan.io/api"
  all_data <- list()
  page <- 1
  
  repeat {
    cat("Dohvaćam:", address, "- stranica:", page, "\n")
    
    res <- GET(base_url, query = list(
      module = "account",
      action = "tokentx",  # token transakcije umjesto txlist!
      address = address,
      startblock = startblock,
      endblock = endblock,
      page = page,
      offset = offset,
      sort = "asc",
      apikey = api_key
    ))
    
    data <- content(res, "text", encoding = "UTF-8")
    json <- fromJSON(data)
    
    if (json$status != "1" || length(json$result) == 0) break
    
    all_data <- append(all_data, list(json$result))
    if (length(json$result) < offset) break
    
    page <- page + 1
    Sys.sleep(0.3)
  }
  
  if (length(all_data) == 0) return(NULL)
  
  bind_rows(all_data)
}

# --- Glavna automatizacija ---
analyze_addresses <- function(csv_path, api_key) {
  addresses_df <- read_csv(csv_path)
  results <- list()
  
  for (i in 1:nrow(addresses_df)) {
    address <- addresses_df$Address[i]
    startblock <- addresses_df$StartBlock[i]
    endblock <- addresses_df$EndBlock[i]
    
    df <- get_transactions(address, api_key, startblock, endblock)
    
    if (is.null(df)) {
      cat(" Nema podataka za adresu:", address, "\n")
      next
    }
    
    # Pretvaranje value_token u decimalne vrijednosti
    df_clean <- df %>%
      mutate(
        token_value = as.numeric(value) / 10^as.numeric(tokenDecimal),
        datetime = as_datetime(as.numeric(timeStamp))
      ) %>%
      filter(token_value > 0)
    
    if (nrow(df_clean) == 0) {
      cat(" Nema transakcija s token_value > 0 za", address, "\n")
      next
    }
    
    benford_test <- benford(df_clean$token_value, number.of.digits = 1)
    
    MAD_value <- benford_test$MAD
    
    # Prilagođena klasifikacija prema distribuciji
    custom_conformity <- case_when(
      MAD_value <= 0.06 ~ "Strong conformity",
      MAD_value <= 0.10 ~ "Moderate conformity",
      MAD_value <= 0.14 ~ "Weak conformity",
      TRUE              ~ "Nonconformity"
    )
    
    predicted_flag <- ifelse(custom_conformity == "Nonconformity", 1, 0)
    
    result <- tibble(
      Address = address,
      TotalTransactions = nrow(df_clean),
      MAD = MAD_value,
      Conformity = benford_test$MAD.conformity,
      CustomConformity = custom_conformity,
      PredictedFlag = predicted_flag
    )
    
    results[[length(results) + 1]] <- result
  }
  
  final_df <- bind_rows(results)
  return(final_df)
}

# --- Poziv i spremanje ---
api_key <- Sys.getenv("API_KEY")
result_df <- analyze_addresses("transaction_dataset.csv", api_key)


# Spremi rezultate
write_csv(result_gas_df, "benford_token_analysis_results.csv")
print(result_df)

# --- Analiza rezultata ---
summary(result_df)
table(result_df$CustomConformity)
round(prop.table(table(result_df$PredictedFlag)) * 100, 2)

# --- Vizualizacija posljednje analize
plot(benford_test) #aut2




# Ukupan broj analiziranih adresa: 1687


# Distribucija konformnosti po Benfordovom zakonu (CustomConformity):
# - Strong conformity: 299 adresa (17.7%)
# - Moderate conformity: 258 adresa (15.3%)
# - Weak conformity: 290 adresa (17.2%)
# - Nonconformity: 840 adresa (49.8%) ← gotovo polovica adresa!
#
# Gotovo 50% adresa pokazuje značajno odstupanje od Benfordove distribucije,
# što može upućivati na sumnjive obrasce ponašanja kod token transakcija.

# Predikcija sumnjivosti (PredictedFlag, temeljen na MAD):
# - PredictedFlag = 1 (sumnjive adrese): 49.79%
# - PredictedFlag = 0 (benigne adrese): 50.21%
#
# Vrlo uravnotežen omjer između klasificiranih sumnjivih i nesumnjivih adresa.
# Može upućivati na mješovitu populaciju – dio legitimnih adresa i dio sumnjivih.

# MAD vrijednosti (Mean Absolute Deviation):
# - Min: 0.0064 (jaka konformnost)
# - Median: 0.1377








# preskocila sam
#----------------------------------- automatizacija procesa - totalGasCost (u ETH)


# --- Funkcija za dohvat transakcija po adresi ---
get_transactions <- function(address, api_key, startblock = 0, endblock = 99999999, offset = 10000) {
  base_url <- "https://api.etherscan.io/api"
  all_data <- list()
  page <- 1
  
  repeat {
    cat("Dohvaćam:", address, "- stranica:", page, "\n")
    
    res <- GET(base_url, query = list(
      module = "account",
      action = "txlist",  # obične transakcije
      address = address,
      startblock = startblock,
      endblock = endblock,
      page = page,
      offset = offset,
      sort = "asc",
      apikey = api_key
    ))
    
    data <- content(res, "text", encoding = "UTF-8")
    json <- fromJSON(data)
    
    if (json$status != "1" || length(json$result) == 0) break
    
    all_data <- append(all_data, list(json$result))
    if (length(json$result) < offset) break
    
    page <- page + 1
    Sys.sleep(0.3)
  }
  
  if (length(all_data) == 0) return(NULL)
  
  bind_rows(all_data)
}

# --- Glavna funkcija ---
analyze_addresses_gas <- function(csv_path, api_key) {
  addresses_df <- read_csv(csv_path)
  results <- list()
  
  for (i in 1:nrow(addresses_df)) {
    address <- addresses_df$Address[i]
    startblock <- addresses_df$StartBlock[i]
    endblock <- addresses_df$EndBlock[i]
    
    df <- get_transactions(address, api_key, startblock, endblock)
    
    if (is.null(df)) {
      cat(" Nema podataka za adresu:", address, "\n")
      next
    }
    
    # Pretvori i izračunaj totalGasCost u ETH
    df_clean <- df %>%
      mutate(
        gasUsed = as.numeric(gasUsed),
        gasPrice = as.numeric(gasPrice),
        totalGasCost = (gasUsed * gasPrice) / 1e18,  # ETH
        datetime = as_datetime(as.numeric(timeStamp))
      ) %>%
      filter(!is.na(totalGasCost), totalGasCost > 0)
    
    if (nrow(df_clean) == 0) {
      cat(" Nema transakcija s totalGasCost > 0 za", address, "\n")
      next
    }
    
    benford_test <- benford(df_clean$totalGasCost, number.of.digits = 1)
    
    MAD_value <- benford_test$MAD
    
    # Prilagođena klasifikacija
    custom_conformity <- case_when(
      MAD_value <= 0.06 ~ "Strong conformity",
      MAD_value <= 0.10 ~ "Moderate conformity",
      MAD_value <= 0.14 ~ "Weak conformity",
      TRUE              ~ "Nonconformity"
    )
    
    predicted_flag <- ifelse(custom_conformity == "Nonconformity", 1, 0)
    
    result <- tibble(
      Address = address,
      TotalTransactions = nrow(df_clean),
      MAD = MAD_value,
      Conformity = benford_test$MAD.conformity,
      CustomConformity = custom_conformity,
      PredictedFlag = predicted_flag
    )
    
    results[[length(results) + 1]] <- result
  }
  
  final_df <- bind_rows(results)
  return(final_df)
}

# --- Poziv i spremanje ---
api_key <- Sys.getenv("API_KEY")
result_gas_df <- analyze_addresses_gas("transaction_dataset.csv", api_key)

# Spremi rezultate
write_csv(result_gas_df, "benford_gas_analysis_results.csv")
print(result_gas_df)

# --- Analiza rezultata ---
summary(result_gas_df)
table(result_gas_df$CustomConformity)
round(prop.table(table(result_gas_df$PredictedFlag)) * 100, 2)

# --- Vizualizacija posljednje analize
plot(benford_test) #aut3


# ukupan broj analiziranih adresa: 3622

# Distribucija klasifikacije po Benfordovoj konformnosti (CustomConformity):
# - Strong conformity: 255 adresa (7.0%)
# - Moderate conformity: 503 adresa (13.9%)
# - Weak conformity: 890 adresa (24.6%)
# - Nonconformity: 1974 adresa (54.5%) ← više od polovice adresa ne prati Benfordov zakon!
#
# Ovo ukazuje da je više od 50% adresa sumnjivo s obzirom na odstupanje od očekivane distribucije.
# Moglo bi sugerirati neregularnosti u potrošnji gasa (gas cost) kod tih adresa.

# Predikcija sumnjivosti (PredictedFlag):
# - PredictedFlag = 1 (sumnjive): 54.5% adresa
# - PredictedFlag = 0 (benigne): 45.5%
#
# Ova binarna zastavica temelji se na MAD vrijednosti i omogućuje brzo filtriranje potencijalno sumnjivih adresa.

# MAD vrijednosti:
# - Min: 0.0078 (vrlo nisko, jaka konformnost)
# - Median: 0.1446
# - Max: 0.2121 (jako odstupanje)
#
#  Prema Benfordovoj teoriji:
# - MAD < 0.06 → Strong conformity 
# - 0.06 – 0.10 → Moderate conformity 
# - 0.10 – 0.14 → Weak conformity 
# - > 0.14 → Nonconformity 
#
#  Budući da je median = 0.1446, većina adresa se nalazi na granici ili iznad praga za Nonconformity (0.14).
# To podržava hipotezu o postojanju znatnog broja adresa koje se ne ponašaju prema očekivanoj distribuciji.

#  Vizualizacija zadnje adrese:
# - Iako je MAD = 0.0582 → pripada kategoriji Strong conformity
# - Međutim, ukupna slika na razini dataset-a ukazuje na problematičan uzorak.

# Zaključak:
# → Varijabla `totalGasCost` u ETH pokazuje značajnu razinu odstupanja od Benfordovog zakona.
# → Više od 54% adresa ima Nonconformity → moguće ukazuje na manipulacije, specifične obrasce ponašanja ili automatizirane transakcije (botovi).
# → Ovu varijablu ima smisla dalje istraživati kao signal za otkrivanje sumnjivih aktivnosti na blockchainu.




























if (!require(keras)) install.packages("keras")
if (!require(tidyverse)) install.packages("tidyverse")

library(keras)
library(tidyverse)

# --- 1. Pripremi podatke ---

# date (datum), value_eth (npr. dnevni zbroj vrijednosti transakcija)


set.seed(123)
dates <- seq.Date(from=as.Date("2025-07-01"), to=as.Date("2025-07-31"), by="day")
values <- rnorm(length(dates), mean=100, sd=10)
data <- tibble(date=dates, value_eth=values)

# --- 2. Normaliziraj vrijednosti ---
scaler <- function(x) (x - min(x)) / (max(x) - min(x))
data <- data %>% mutate(value_scaled = scaler(value_eth))

# --- 3. Pripremi ulazne sekvence za LSTM ---

# Funkcija za stvaranje vremenskih prozora
create_sequences <- function(data, seq_length) {
  sequences <- list()
  for (i in 1:(nrow(data) - seq_length)) {
    seq <- data$value_scaled[i:(i + seq_length - 1)]
    sequences[[i]] <- seq
  }
  sequences <- do.call(rbind, sequences)
  return(sequences)
}

seq_length <- 5
sequences <- create_sequences(data, seq_length)

# Podijeli na ulaz (X) i izlaz (Y)
X <- sequences[, 1:(seq_length-1)]
Y <- sequences[, seq_length]

# Promijeni oblik za LSTM: [uzorci, koraci, značajke]
X <- array(as.numeric(unlist(X)), dim = c(nrow(X), seq_length-1, 1))
Y <- array(as.numeric(Y), dim = c(length(Y), 1))

# --- 4. Definiraj i istreniraj LSTM model ---

model <- keras_model_sequential() %>%
  layer_lstm(units = 50, input_shape = c(seq_length - 1, 1), return_sequences = FALSE) %>%
  layer_dense(units = 1)

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam()
)

history <- model %>% fit(
  x = X, y = Y,
  epochs = 50,
  batch_size = 4,
  validation_split = 0.2,
  verbose = 1
)

# --- 5. Koristi model za predviđanje i detekciju anomalija ---

predictions <- model %>% predict(X)
mse <- (Y - predictions)^2

# Postavi prag za anomalije (npr. srednja MSE + 2*std)
threshold <- mean(mse) + 2 * sd(mse)

anomalies <- which(mse > threshold)

cat("Pronađene anomalije na pozicijama:\n")
print(anomalies)

# --- 6. Vizualizacija rezultata ---

plot(data$date[(seq_length):nrow(data)], Y, type="l", col="blue", ylim=range(c(Y, predictions)),
     ylab="Normalized value", xlab="Date", main="LSTM Predikcije i Anomalije")
lines(data$date[(seq_length):nrow(data)], predictions, col="red")
points(data$date[(seq_length):nrow(data)][anomalies], Y[anomalies], col="darkorange", pch=19)
legend("topright", legend=c("Stvarne vrijednosti", "Predviđene vrijednosti", "Anomalije"),
       col=c("blue", "red", "darkorange"), lty=c(1,1,NA), pch=c(NA, NA, 19))






