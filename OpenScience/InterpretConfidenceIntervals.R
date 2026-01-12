# Simulation zur Frage "Wie verhält sich der frequentistische p-Wert, wenn die Punktschätzung fixiert wird, und die Breite des 95% Konfidenzintervalls variiert?" >> Neu <<
# Grund: Alte Simulation war unnötig aufwändig und zudem nicht präzise.

# Wir gehen von einer linearen Regression aus. Für obige Frage ist es egal ob simple (ein Prädiktor) oder multiple (mehrere Prädiktoren) Regression.
# Wir müssen drei Dinge festlegen:
# Regressionsgewicht, z.B. .5
# Standardfehler, z.B. .2
# Freiheitsgrade, z.B. 40.
# Damit können wir den Wert der 2-seiten t-Statistik berechnen, wodurch wir wiederum den p-Wert berechnen können.
# Notiz: Die Ergebnisse der linearen Regression werden letztlich als t-Statistik ausgegeben (siehe folgender R Output unten: summary(mod)).

# Vorab prüfen wir, ob wir alles Nötige verstanden haben:
library(datasets) # Beinhaltet mehrere Datensätze, u.a. airquality.
# help(package="datasets")
# Multiple lineare Regression:
# Temperature = outcome. Ozone und Wind = Prädiktoren.
mod <- lm(Temp ~ Ozone + Wind, data=airquality)
(smry <- summary(mod))
# Von 153 Beobachtungen im airquality Datensatz, sind über die drei Variablen Temp, Ozone und Wind 37 fehlende Werte vorhanden, d.h. es bleiben 116 vollständige Beobachtungen übrig. Es werden drei Parameter geschätzt (Intercept, Ozone, Wind), deshalb Anzahl Freiheitsgrade = 116 - 3 = 113.
# estmts = estimates.
(estmts <- coefficients(smry))
# Umbenennung von Spalte 2 (Std. Error zu StdError); Sinn: Damit es weiter unten im Skript einfacher wird (also: weniger R code).
colnames(estmts)[2] <- "StdError"

# 95% CI der Modelparameter (intercept und Regressionsgewichte):
confint(mod)

# Manuelle Prüfung vom dritten Prädiktor (Wind):
(tValue <- estmts[3,"Estimate"]/estmts[3,"StdError"])
# p-Wert 2-seitige t-Verteilung mit 113 Freiheitsgraden
(pValue <- pt(q=tValue, df=113)*2)

# Custom Funktion, um 2-seitigen p-Wert korrekt zu berechnen.
# Notiz: Abhängig davon, ob Regressionsgewicht negativ oder positiv ist, muss man in der Funktion qt() das Argument lower.tail auf TRUE belassen oder es zu FALSE ändern.
pValFun <- function(tValue=NULL, df=NULL) {
    if(tValue < 0) {
        return(pt(q=tValue, df=df)*2)
    } else {
        return(pt(q=tValue, df=df, lower.tail = FALSE)*2)
    }
}

# Custom Funktion, die die t-Statistik, den p-Wert und das CI berechnet.
manualComputation <- function(data=NULL, percent=.95, df=NULL) {
    # Berechne t-Statistik
    tVals <- data[,"Estimate"]/data[,"StdError"]
    # Berechne 2-seitigen p-Wert
    pVals <- c()
    for(i in 1:length(tVals)) {
        pVals <- c(pVals, pValFun(tValue=tVals[i], df=df))
    }
    # Ermittle kritischen 2-seitigen Wert unter t-Verteilung
    crit <- qt((1-percent)/2, df=df, lower.tail = FALSE)
    # left = untere Grenze des CI
    left <- data[,"Estimate"] - crit * data[,"StdError"]
    # right = obere Grenze des CI
    right <- data[,"Estimate"] + crit * data[,"StdError"]
    # out = Alle Ergebnisse als data.frame ausgeben.
    out <- data.frame(
        Estimate=data[,"Estimate"],
        StdError=data[,"StdError"],
        tValue=tVals,
        pValue=pVals,
        lowerCI=left,
        upperCI=right
    )
    return(out)
}

# Prüfe (d.h. vergleiche mit Ergebnissen oben):
manualComputation(data=estmts[,c("Estimate", "StdError")], percent=.95, df=113)
# ------------------------------------------------
#
# Jetzt zur Simulation (3 Szenarien), um zu beobachten, wie sich der p-Wert verhält:

# ------------
# Szenario 1: Fixiere Regressionsgewicht und Freiheitsgrade, variiere Standardfehler, einmal vergrössern (also wird das 95% CI breiter), einmal verkleinern (95% CI wird kleiner).

sz1Larger <- data.frame(Estimate=.5, StdError=c(.2, .22, .24))
sz1L <- manualComputation(data=sz1Larger, percent = .95, df=40)
# Neue Spalte anhängen: dist = Distanz zwischen unterer und oberer CI-Grenze.
sz1L$dist <- apply(sz1L[,5:6], 1, dist)
# Zunehmender StdError: p-Wert steigt, CI wird breiter.
sz1L

sz1Smaller <- data.frame(Estimate=.5, StdError=c(.2, .18, .16))
sz1S <- manualComputation(data=sz1Smaller, percent = .95, df=40)
sz1S$dist <- apply(sz1S[,5:6], 1, dist)
# Sinkender StdError: p-Wert sinkt, CI wird enger.
sz1S

# ------------
# Szenario 2: Fixiere Standardfehler und Freiheitsgrade, variiere Regressionsgewicht, einmal vergrössern, einmal verkleinern.

sz2Larger <- data.frame(Estimate=c(.5, .55, .6), StdError=.2)
sz2L <- manualComputation(data=sz2Larger, percent = .95, df=40)
sz2L$dist <- apply(sz2L[,5:6], 1, dist)
# Zunehmendes Estimate: p-Wert sinkt, CI bleibt konstant.
sz2L

sz2Smaller <- data.frame(Estimate=c(.5, .45, .4), StdError=.2)
sz2S <- manualComputation(data=sz2Smaller, percent = .95, df=40)
sz2S$dist <- apply(sz2S[,5:6], 1, dist)
# Abnehmendes Estimate: p-Wert steigt, CI bleibt konstant
sz2S

# ------------
# Szenario 3: Fixiere Regressionsgewicht und Standardfehler, variiere die Freiheitsgrade, einmal vergrössern, einmal verkleinern.

sz3 <- data.frame(Estimate=.5, StdError=.2)

sz3L.40 <- manualComputation(data=sz3, percent = .95, df=40)
sz3L.45 <- manualComputation(data=sz3, percent = .95, df=45)
sz3L.50 <- manualComputation(data=sz3, percent = .95, df=50)
# Füge obige drei Outputs in gemeinsame Matrix.
sz3L <- rbind(sz3L.40, sz3L.45, sz3L.50)
# Berechne Distanz für Spalten 5 und 6, und zwar je Zeile in sz3L
sz3L$dist <- apply(sz3L[,5:6], MARGIN = 1, FUN = dist)
# Freiheitsgrade steigen = Erhöhung der Stichprobengrösse: p-Wert sinkt, CI wird enger.
sz3L

sz3S.40 <- manualComputation(data=sz3, percent = .95, df=40)
sz3S.35 <- manualComputation(data=sz3, percent = .95, df=35)
sz3S.30 <- manualComputation(data=sz3, percent = .95, df=30)
sz3S <- rbind(sz3S.40, sz3S.35, sz3S.30)
sz3S$dist <- apply(sz3S[,5:6], MARGIN = 1, FUN = dist)
# Freiheitsgrade sinken = Verrinerung der Stichprobengrösse: p-Wert steigt, CI wird breiter.
sz3S
# ------------------------------------------------

# Selbe Simulation mit mehr als 3 Werten, jedoch nur, um die Ergebnisse dann zu visualisieren. Heisst: Keine neuen Erkenntnisse, lediglich Bestätigung oben gewonnener Erkenntnisse und in visualisierter Form.

library(ggplot2)

# ------------
# Szenario 1: Fixiere Regressionsgewicht und Freiheitsgrade, variiere Standardfehler.
sz1Df <- data.frame(Estimate=.5, StdError=seq(.15, .3, by=.01))
sz1 <- manualComputation(data=sz1Df, percent = .95, df=40)
sz1$dist <- apply(sz1[,5:6], 1, dist)
sz1

# Beispiel-Visualisierung
ggplot(data=sz1, aes(x=StdError, y=pValue)) +
    geom_point()

# ------------
# Szenario 2: Fixiere Standardfehler und Freiheitsgrade, variiere Regressionsgewicht
sz2Df <- data.frame(Estimate=seq(.3, .7, by=.025), StdError=.2)
sz2 <- manualComputation(data=sz2Df, percent = .95, df=40)
sz2$dist <- apply(sz2[,5:6], 1, dist)
sz2

# Beispiel-Visualisierungen
ggplot(data=sz2, aes(x=Estimate, y=pValue)) +
    geom_point()

ggplot(data=sz2, aes(x=tValue, y=pValue)) +
    geom_point()

ggplot(data=sz2, aes(x=dist, y=pValue)) +
    geom_point()

# ------------
# Szenario 3: Fixiere Regressionsgewicht und Standardfehler, variiere die Freiheitsgrade.

sz3Df <- data.frame(Estimate=.5, StdError=.2)
dfSeq <- seq(22, 60, by=2)
sz3 <- manualComputation(data=sz3Df, percent = .95, df=20)
# sz3Ls <- list()
for(i in dfSeq) {
    sz3 <- rbind(sz3, manualComputation(data=sz3Df, percent = .95, df=i))
}
sz3$dist <- apply(sz3[,5:6], 1, dist)
sz3$df <- seq(20, 60, by=2)
sz3

# Beispiel-Visualisierungen
# (Steigung von df (degrees of freedom) von 20 bis 60)
ggplot(data=sz3, aes(x=pValue, y=dist)) +
    geom_point()

ggplot(data=sz3, aes(x=df, y=pValue)) +
    geom_point()

ggplot(data=sz3, aes(x=df, y=dist)) +
    geom_point()

# ------------
# Hänge Spalte an, die angibt, von welchem Szenario die Ergebnisse stammen.
sz1$szenario <- "Szenario1"
sz2$szenario <- "Szenario2"
sz3$szenario <- "Szenario3"

# Füge alle Ergebnisse in einer Übersicht zusammen.
# Entferne Spalte "df" von sz3 vor dem Zusammenfügen.
removeCol <- which(colnames(sz3) == "df")
# sz = Szenarien
sz <- rbind(sz1, sz2, sz3[,-removeCol])

# Wie verhält sich der p-Wert?
ggplot(data=sz, aes(x=tValue, y=pValue)) +
    geom_point() +
    facet_wrap(~szenario, scales="free")

# Wie verhält sich die Breite des CI (dist)?
ggplot(data=sz, aes(x=tValue, y=dist)) +
    geom_point() +
    facet_wrap(~szenario, scales="free")
# ------------------------------------------------