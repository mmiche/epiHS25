# Zwei Datenreihen von je 30 Messungen aus Pacheco et al. 2024 (https://doi.org/10.1017/dsj.2024.9)

# Konstrukt 1: Total Effort (Investierte Mühe in ein Projekt zur Herstellung eines Produkts)
totalEffort <- c(110, 203, 335, 220, 173, 264, 369, 207, 224, 198, 136, 326, 318, 371, 76, 216, 208, 110, 327, 238, 364, 168, 112, 96, 222, 75, 281, 203, 294, 279)

# Konstrukt 2: Product Quality
productQuality <- c(4.24, 6.02, 7.68, 5.84, 6.6, 6.56, 5.8, 6.32, 5.56, 5.32, 4.96, 6.44, 7.28, 7.52, 6.9, 5.8, 5.72, 5.04, 5, 7.4, 6.6, 5.6, 6.16, 5.92, 7.64, 5.56, 6.52, 7.08, 6, 6.52)

# In Pacheco et al. 2024 (Text und Tabelle 4) steht, dass Korrelationsanalysen durchgeführt wurden und dass je Hypothese eine PRAKTISCH RELEVANTE Nullhypothese einer Korrelation des Betrags von 0.3 getestet werden sollte. Einfach ausgedrückt: Die Autor/innen wollten (laut ihrer eigenen publizierten Absicht) wissen, ob die beobachtete Korrelation statistisch signifikant grösser ausfiel als der Betrag von 0.3, ANSTATT der Default-Nullhypothese einer Korrelation von exakt 0.

# Leider haben die Autor/innen ihre Absicht nicht in die Tat umgesetzt, denn die publizierten Ergebnisse und auch deren Interpretation zeigen, dass sie trotzdem die Default-Nullhypothese einer Korrelation von exakt 0 verwendet haben.

# Wir wollen hier also einmal (nur für die erste der insgesamt 5 Hypothesen) schauen, wie der p-Wert unter der spezifischen Nullhypothesen eigentlich gewesen wäre:

# Zuerst reproduzieren wir das publizierte (falsche) Ergebnis:
cor.test(totalEffort, productQuality)

# Dann das richtige Ergebnis:
library(car)
fit <- lm(scale(totalEffort) ~ scale(productQuality))
car::linearHypothesis(model=fit, "scale(productQuality)=0.3")

# Antwort: Der richtige p-Wert wäre 0.49 gewesen.