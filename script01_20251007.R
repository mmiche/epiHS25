# Overall relevant literature (I say a few words, regarding 'why relevant'):

# The very least you might want to do, is download the papers and read their abstract.

# Ernst and Albers (2017) https://doi.org/10.7717/peerj.3323
# Flatt and Jacobs (2019) https://doi.org/10.1177/1523422319869915
# Shatz (2024) https://doi.org/10.3758/s13428-023-02072-x

# Testing assumptions of linear regression, use:
# Jones et al. (2025) https://doi.org/10.1371/journal.pone.0299617

# Savieri et al. (2025) https://doi.org/10.5334/jors.553

# Babyak (2004) Psychosomatic Medicine 66(3):p 411-421, May 2004.
# Freedland (2009) Psychosomatic Medicine 71:205–216 (2009)
# Delicandro et al. (2021) https://doi.org/10.20982/tqmp.17.1.p001

# Testing assumptions of logistic regression, use:
# Harris (2021) https://doi.org/10.1136/fmch-2021-001290


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# I found yet another (possibly useful) online source.
# https://rpsystats.com/
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Load libraries:
library(tidyverse)
library(gvlma) # Statistical tests of linear regression assumptions.
library(performance)
# Adapt full path to the csv file on your computer:
maric <- readr::read_csv("baza_cov2soul_recode_final_07.3.2022_no_code.csv")
# Preprocess (Change numeric code of gender to 0 and 1 instead of 1 and 2)
maric$gender01 <- maric$gender - 1
# c19prot must be reversed.
maric$c19prot <- abs(maric$c19prot-1)
names(maric)

# Predictor names
preds <- c("gender01", "age_gr", "educ", "mar_stat", "employ_bin",
           "settlem", "DG_self", "som_ill", "LTE_close", "LTE_job",
           "LTE_pers", "LTE_sp", "c19inf", "c19isol", "c19prot",
           "C19fam_risk")
# Outcome names
outcomes <- c("DG_cur", "MOOD_cur", "ANX_cur", "SUDcur_bin",
              "phq9", "gad7")

# maricz: z-transformed predictors and outcomes phq9, gad7 (multiple linear regression, beta weights)
maricz <- as.data.frame(scale(maric[,c(preds, outcomes[5:6])]))
# :
maricz$DG_cur <- maric$DG_cur; maricz$MOOD_cur <- maric$MOOD_cur
maricz$ANX_cur <- maric$ANX_cur; maricz$SUDcur_bin <- maric$SUDcur_bin
# age_gr = Age group with 4 levels, oldest group = reference group.
maric$age_gr <- maricz$age_gr <- factor(maric$age_gr, levels = c(4,1,2,3))
# mar_stat = marital status with 4 levels. Married = first level = reference level.
maric$mar_stat <- maricz$mar_stat <- as.factor(maric$mar_stat)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Multivariable  L I N E A R  regression - Outcome PHQ9
# --------------------------------------
fmla <- formula(paste0("phq9 ~ ", paste0(preds, collapse = "+")))
phq9lm <- lm(formula = fmla, data=maric)
summary(phq9lm)

# ?performance::check_model
performance::check_model(phq9lm, size_dot=1.2)
# # In order to be able to save the output as a png file, to include it in a document, e.g., in the master thesis, irrespective which software is used, such as Microsoft Word, Open Office, ...
# savePlots <- plot(performance::check_model(phq9lm, size_dot=1.2))
# ggsave(filename="test.png", plot = savePlots, device = "png", width=8, height=12, units="in", dpi=300)

# Statistically (only for linear regression models)
gvlma::gvlma(phq9lm)

# Main source for linear regression model test assumptions: Jones et al. (2025)
# Including this URL:
# https://github.com/Lee-V-Jones/Reporting_Linear_Regression_Assumptions
#

# ------------------
# See Fig.2 in Jones_2025
# Plot shows the scattered (scaled; = standardized) residuals against their remaining counterpart, namely the so-called predicted (or fitted) outcome values ('counterpart' from the viewpoint of the regression model, which splits the observed data into two parts: part 1 = the systematic part [= data, as fitted according to the applied LINEAR model; fitted.values = outcome values as fitted by the model], part 2 = the unsystematic part [= the differences between the observed and the fitted outcome values; the 'errors' as far as the LINEAR model is concerned]).

# Clear recommendation: R-Buch (5. Auflage; https://doi.org/10.1007/978-3-662-61736-6), Abschnitt 6.5 Regressionsdiagnostik

# As an orientation, read the caption of Fig. 2 in Jones_2025: ... 'there should be as many points above as below the line' (and the line should be horizontal at y-axis = 0 across the entire x-axis).

# plot with 'which' set to 1:
plot(phq9lm, which=1)

# What does the command plot do, when applied to an lm object?
?plot.lm

# Challenge: Standardization usually means z-transformation (using the R command 'scale'). However, standardizing the residuals is a little different. How and why residuals may be standardized in this slightly different way, can be read here:
# https://r-resources.massey.ac.nz/161251/Lectures/Lecture5.html
# See short description of raw residuals and standardized residuals:
# 1. technical problem with raw residuals as substitutes for the error terms,
# 2. unlike error terms, raw residuals do not have equal variance,
# 3. therefore preferable to work with the standardized residuals.

# prdctd = predicted (or fitted) values
prdctd <- phq9lm$fitted.values
# resid = residuals
resid <- phq9lm$residuals

plot(prdctd, resid)
graphics::panel.smooth(x=phq9lm$fitted.values, y=resid)

?graphics::panel.smooth
# If needed, inspect the raw code of a function.
graphics::panel.smooth
# This function uses the function lowess from the stats package.
# It uses (= accepts) the default parameter values of the lowess function.
?stats::lowess
# Run the command, just to see the results in the console:
stats::lowess(x=prdctd, y=resid)

# Use ggplot2
plotWhich1 <- 
    ggplot(data=data.frame(prdctd, resid), aes(x=prdctd, y=resid)) +
    geom_point() +
    geom_line(
        data=as.data.frame(stats::lowess(x=prdctd, y=resid)),
        aes(x=x, y=y), color="red", linewidth=.8, linetype="dashed")
# ------------------

# Fig.3 in Jones_2025

# Understanding QQ Plots
# https://library.virginia.edu/data/articles/understanding-q-q-plots

# plot with 'which' set to 2:
plot(phq9lm, which=2)
?stats::rstandard # See how sd is computed
stdResid <- stats::rstandard(phq9lm)

# Theoretical quantiles
stdResidDesc <- sort(stdResid) # stdResidDesc = standardized residuals in descending order
# ----------------------------------------------
# Code in script lines 147-159 (except for 150), copied and adapted from here:
# See: https://www.datacamp.com/tutorial/qq-plot
rank <- 1:length(stdResidDesc)
prob <- (rank - .5)/length(stdResidDesc)
theorQuant <- qnorm(prob)
plot(theorQuant, stdResidDesc)

# Calculate slope and intercept for the Q-Q line
q1_obs <- quantile(stdResidDesc, probs = 0.25)
q3_obs <- quantile(stdResidDesc, probs = 0.75)
q1_theo <- qnorm(0.25)
q3_theo <- qnorm(0.75)
slope <- (q3_obs - q1_obs) / (q3_theo - q1_theo)
intercept <- q1_obs - slope * q1_theo
abline(a=intercept, b=slope)
# ----------------------------------------------

fig3Df <- data.frame(theorQuant, stdResidDesc)

# Use ggplot2
plotWhich2 <- 
    ggplot(data=fig3Df,
           aes(x=theorQuant, y=stdResidDesc)) +
    geom_point() +
    geom_abline(slope=slope, intercept=intercept)


ggplot(data=fig3Df, aes(x=stdResid)) +
    geom_histogram(color="black", fill="grey") +
    xlab(label="Standardized Residuals") +
    ylab(label="Frequency")


# ------------------

# plot with 'which' set to 3:
plot(phq9lm, which=3)
sqrtAbsStdResid <- sqrt(abs(stdResid))
plot(prdctd, sqrtAbsStdResid)

plotWhich3Df <- data.frame(fittedValues=prdctd, sqrtAbsStdResid)

# Use ggplot2
plotWhich3 <- 
    ggplot(data=plotWhich3Df,
           aes(x=fittedValues, y=sqrtAbsStdResid)) +
    geom_point() +
    geom_line(
        data=as.data.frame(stats::lowess(x=prdctd, y=sqrtAbsStdResid)),
        aes(x=x, y=y), color="red", linewidth=.8, linetype="dashed")

# ------------------

# At least roughly understand Cook's distance bar plot
# https://cran.r-project.org/web/packages/olsrr/vignettes/influence_measures.html
# 'detect observations that strongly influence fitted values of the model' ... in other words: detect possible outliers in the data which might better be treated appropriately or even removed, before fitting the model to the data.

# plot with 'which' set to 4:
plot(phq9lm, which=4)


# Cook's distance
# https://rpubs.com/DragonflyStats/Cooks-Distance

cookd <- stats::cooks.distance(phq9lm)

library(broom)
phq9aug <- broom::augment(phq9lm)

data.frame(cookd, phq9aug$.cooksd)[1:5,]
all(cookd == phq9aug$.cooksd)

# Another check (see stdResid above)
# See R-Buch, Abschnitt 6.5.1
P <- 20
E <- phq9lm$residuals
N <- nrow(maricz)
MSE <- sum(E^2) / (N - (P+1))
h <- phq9aug$.hat
Estd <- E / sqrt(MSE * (1-h))
head(data.frame(stdResid, Estd))
all.equal(stdResid, Estd)

# Use ggplot2
ggplot(data=phq9aug, aes(x=1:nrow(phq9aug), y=.cooksd)) +
    geom_bar(stat="identity", position="identity")

# Use package olsrr
library(olsrr)

olsrr::ols_plot_cooksd_chart(model=phq9lm)

# ------------------

# Leverage
# https://rpubs.com/DragonflyStats/Leverage

# plot with 'which' set to 5:
plot(phq9lm, which=5)

# Use base R plot
plot(phq9aug$.hat, stdResid, xlim=c(0,.0525))
graphics::panel.smooth(x=phq9aug$.hat, y=stdResid)

lvrg <- stats::hatvalues(phq9lm)

head(data.frame(lvrg, phq9aug$.hat))
all(lvrg == phq9aug$.hat)

# Influential observations
inflmeas <- stats::influence.measures(phq9lm)
summary(inflmeas)

# Use ggplot2
# WARNING: geom_smooth loess is NOT the same as lowess (from the stats package)
# --------
ggplot(data=data.frame(Leverage=phq9aug$.hat, stdResid), aes(x=Leverage, y=stdResid)) +
    geom_point() +
    geom_smooth(method="loess", se=FALSE, color="red", linewidth=.7, linetype="dashed")

# Use lowess instead of loess function
ggplot(data=data.frame(Leverage=phq9aug$.hat, stdResid), aes(x=Leverage, y=stdResid)) +
    geom_point() +
    geom_line(
        data=as.data.frame(stats::lowess(x=phq9aug$.hat, y=stdResid)),
        aes(x=x, y=y), color="red", linewidth=.8, linetype="dashed")


# What is the difference between lowess and loess?
# ?stats::lowess
# ... using locally-weighted polynomial regression
# ?stats::loess
# ... locally polynomial surface ..., using local fitting
#
# I do not care right now about details of their differences.
# ------------------

# plot with 'which' set to 6:
plot(phq9lm, which=6)

# Use ggplot2 ... check the internet, if you want to produce that plot with ggplot2.

# One example, if I decided that I wanted to produce that plot:
# https://graysonwhite.com/gglm/reference/stat_cooks_leverage.html
# install.packages("gglm")
library(gglm)
ggplot(data=phq9lm) + gglm::stat_cooks_leverage()

# Another possible online source for regression diagnostics (and a million and a half other things):
# https://thomaselove.github.io/431notes-2017/regression-diagnostics.html

# ------------------

# Checking whether multicollinearity is a problem:

# Addressing Multicollinearity (see Jones et al., 2025, page 11)
# https://library.virginia.edu/data/articles/addressing-multicollinearity

# vif = variance inflation factor
car::vif(phq9lm)

# Use ggplot2 ... one moment ... why, what for?! Just check whether any of the values exceeds the value 5.

# [,-2] means remove the second column (degrees of freedom)
any(car::vif(phq9lm)[,-2] >= 5) # No multicollinearity problem here.

# If you want to get the range of vif values
range(car::vif(phq9lm)[,"GVIF"])
summary(car::vif(phq9lm))
# ------------------

# ------------------
# And again, another online source:
# https://www.bookdown.org/rwnahhas/RMPH/

# Use package car and function crPlots

# Test (diagnosis) of non-linearity

# Which of the predictors are on a continuous scale
psych::describe(maric[,preds])
# indeed, only education.

car::crPlots(model=phq9lm, terms = ~ educ,
             pch=20, col="grey",
             smooth=list(smoother=car::gamLine))
# ------------------

# How would you go on, if you decided to run a so-called 'robust linear regression model' instead of the default (or conventional) linear regression model?

library(MASS)
library(boot)
library(car)

set.seed(1)
robustphq9lm <- MASS::rlm(fmla, data=maricz, maxit=200)
set.seed(1)
# ?car::Boot
bootphq9lm <- car::Boot(robustphq9lm, R=1000)
phq9Df <- as.data.frame(car::Confint(object=bootphq9lm, level=.95, type="norm"))
colnames(phq9Df) <- c("Estimate", "l95", "u95")
phq9Df

# Conclusion: We have conducted yet another regression model with no specifically expressed goal. This is what Carlin and Moreno-Betancur refer to as "The logic here seems entirely backwards". In other words: Bad, or at least not good, research practice.

# What is good research practice?
# First, think. Then, think again (if needed); try to communicate your thoughts to others and see whether your thoughts make sense to others or not. And so on. Then, at the very end, connect your thoughts and your clearly expressed expectations of what the data should contain and how the data should confirm your expectations (hypotheses), in the form of a certain part of the results of an adequate statistical analysis.

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



