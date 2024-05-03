a <- 2 * exp(5)
b <- 2 * a
max(a, b)

help(sum)

a <- 15:25
mean(a)

apropos("sum", mode = "function")

setwd("/Users/mrbre/Desktop/Studia/APU/Lab1")
a <- "smartfony Samsung"
save(a, file = "smartfony")
remove(a)
a
load("smartfony")
a

install.packages("gridExtra")
library(gridExtra)
help(package = "gridExtra")
grid.table(mtcars[1:10, ])

c <- seq(20, 100, 4)
rev(c)

a <- rev(5:9)
b <- 11:16
d <- c(b, a)
d

nazwa <- c("Galaxy A55", "Galaxy A35", "Galaxy S24 Ultra", "Galaxy A54", "Galaxy A34", "Galaxy A25", "Galaxy S23", "Galaxy S23+", "Galaxy S21 FE", "Galaxy M34")
wyswietlacz <- c(6.6, 6.6, 6.8, 6.4, 6.6, 6.5, 6.1, 6.6, 6.4, 6.5)
pamiec_RAM <- c(8, 6, 12, 8, 6, 6, 8, 8, 6, 6)
pamiec_wbudowana <- c(128, 128, 512, 128, 128, 128, 256, 256, 128, 128)
aparat_foto <- c(50, 50, 50, 50, 48, 50, 50, 50, 32, 50)
cena <- c(2099, 1649, 6599, 2399, 1899, 1099, 3499, 4499, 2999, 1199)
liczba_opinii <- c(2, 0, 43, 192, 63, 5, 164, 8, 55, 23)
smartfony <- data.frame(nazwa, wyswietlacz, pamiec_RAM, pamiec_wbudowana, aparat_foto, cena, liczba_opinii)
mean(smartfony$cena)

smartfonybackup <- smartfony
nowysmartfon <- data.frame(nazwa ="Galaxy S22", wyswietlacz = 6.1, pamiec_RAM = 8, pamiec_wbudowana = 128, aparat_foto = 50, cena = 3999, liczba_opinii = 141)
smartfony <- rbind(smartfony, nowysmartfon)
mean(smartfony$cena)

ocena <- c(4, 4, 5, 5, 5, 4.5, 5, 5, 4.5, 5, 4.5)
smartfonybackup <- smartfony
smartfony <- cbind(smartfony, ocena)

tapply(smartfony$cena, smartfony$ocena, mean)

smartfonybackup <- smartfony
smartfon1 <- data.frame(nazwa ="Galaxy S5", wyswietlacz = 6.5, pamiec_RAM = 8, pamiec_wbudowana = 64, aparat_foto = 32, cena = 1599, liczba_opinii = 33, ocena = 5)
smartfon2 <- data.frame(nazwa ="Galaxy S6", wyswietlacz = 6.4, pamiec_RAM = 8, pamiec_wbudowana = 64, aparat_foto = 36, cena = 1999, liczba_opinii = 66, ocena = 4.5)
smartfon3 <- data.frame(nazwa ="Galaxy S7", wyswietlacz = 6.1, pamiec_RAM = 16, pamiec_wbudowana = 128, aparat_foto = 48, cena = 2599, liczba_opinii = 99, ocena = 5)
smartfon4 <- data.frame(nazwa ="Galaxy S9", wyswietlacz = 6.6, pamiec_RAM = 16, pamiec_wbudowana = 128, aparat_foto = 50, cena = 2999, liczba_opinii = 124, ocena = 5)
smartfony <- rbind(smartfony, smartfon1)
smartfony <- rbind(smartfony, smartfon2)
smartfony <- rbind(smartfony, smartfon3)
smartfony <- rbind(smartfony, smartfon4)
liczebnosc <- table(smartfony$ocena)
barplot(liczebnosc)
procenty <- liczebnosc / sum(liczebnosc)
pie(procenty)

install.packages("plotrix")
library(plotrix)
fan.plot(liczebnosc, labels = names(liczebnosc))

smartfonybackup <- smartfony
grid.table(smartfony)
smartfony[, "status_opinii"] <- ifelse(smartfony$liczba_opinii == 0, "nie ma", "mniej niż 50")
grid.table(smartfony)
smartfony[, "status_opinii"] <- ifelse(smartfony$liczba_opinii <= 50, smartfony$liczba_opinii, "50-100")
smartfony[, "status_opinii"] <- ifelse(smartfony$liczba_opinii > 100, "więcej niż 100", smartfony$status_opinii)
grid.table(smartfony)
smartfony[, "status_opinii"] <- ifelse(smartfony$liczba_opinii == 0, "nie ma", "mniej niż 50")
grid.table(smartfony)
smartfony[, "status_opinii"] <- ifelse(smartfony$liczba_opinii <= 50, smartfony$status_opinii, "50-100")
smartfony[, "status_opinii"] <- ifelse(smartfony$liczba_opinii > 100, "więcej niż 100", smartfony$status_opinii)
grid.table(smartfony)
smartfony$status_opinii <- factor(smartfony$status_opinii)
pie(table(smartfony$status_opinii))

paste(smartfony$nazwa, "ma ocenę klientów", smartfony$ocena, "bo ma liczbę opinii", smartfony$liczba_opinii)

write.csv(smartfony, "smartfony_data.csv")

