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

# Multivariable  L O G I S T I C  regression - Outcome DG_cur
# ------------------------------------------
#
# DG_cur only model with almost adequate EPV (183/20 = 9.15)
# EPV = event per variable ratio of 1:10, that is, 10 outcome cases for each predictor in the model.
# EPV is not a strong rule, rather a heuristic.

# ---------------------------------
# Carlin and Moreno-Betancur (2025) https://doi.org/10.1002/sim.10244
# --------------------------
# Section 5.1.2
# Easiest descriptive task: Compute outcome prevalence for a binary outcome, e.g., DG_cur
mean(maric$DG_cur)
# Use null model of lm:
lm(DG_cur ~ 1, data=maric)
# Use null model of glm, link-function "log":
coefficients(glm(DG_cur ~ 1, family=binomial(link="log"), data=maric))
# Exponentiate intercept
exp(-1.883088)

# Check prevalence difference between female and male, using glm
coefficients(glm(DG_cur ~ gender01, family=binomial(link="log"), data=maric))
# Exponentiate intercept (prevalence of DG_cur in gender01 = 0)
exp(-1.7581993)
# Check: prevalence of DG_cur in gender = 0 equals 22.35%?
mean(maric$DG_cur[maric$gender01==0])
# ---------------------------------

# Based on LASSO output, use only 7 selected predictors (see below).

# LASSO regression is one option among penalized regression.
# L = Least
# A = Absolute
# S = Shrinkage and
# S = Selection
# O = Operator.
# ----------------
library(glmnet)
library(glmnetUtils)

X <- model.matrix(object=~., data=maric[,preds])[,-1]

mod_cv <- cv.glmnet(x=X,
                    y=maric[["DG_cur"]],
                    family='binomial',
                    intercept = FALSE, alpha=1)

# Result:
coef(mod_cv, c(mod_cv$lambda.min, mod_cv$lambda.1se))
# mod_cv$glmnet.fit
plot(mod_cv)

# Take sparsest model, s2 (1se), keeps 7 predictors:
fmla <- formula(paste0("DG_cur ~ ", paste0(preds[c(1:3,6,7,10,12)], collapse = "+")))
dgcurMod <- glm(formula=fmla, family = binomial(link="logit"), data=maric)
summary(dgcurMod)

library(rms)

rms::lrm(fmla, data=maric)

# Harris (2021)
# https://github.com/jenineharris/logistic-regression-tutorial

# 1. Download the R-script '20211210-logistic-regression-tutorial-code.R' from GitHub

# 2. Make a complete backup copy of that script ...

# 3. Open one of the scripts in R-Studio and start checking what is needed for checking the test assumptions of logistic regression ...
# "Start checking" = Find the code to check assumptions, copy it and paste it here, then modify the code, so that it fits the model above.

# I copy-pasted from J. Harris' script, i.e., script lines (see Harris' script) 76-106 .
# The script lines below, i.e., 363-365 and 396, are from me. 

# Which of the predictors are on a (at least apparently) continuous scale?
summary(maric[,preds[c(1:3,6,7,10,12)]])
# The only predictor available is education (educ).

# STEP 2 & 3: ESTIMATE THE MODELS & ASSUMPTION CHECKING

# need to estimate the models in order to check the assumptions

# estimate the full model
dgcurMod

# check VIF for no perfect multicollinearity assumption
car::vif(dgcurMod)

# check linearity for the yearsSmoke variable - instead yearsSmoke, use educ
# make a variable of the logit of the predicted values
logit.use <- log(dgcurMod$fitted.values/(1-dgcurMod$fitted.values))

# make a small data frame with the logit variable and the (yearsSmoke) educ predictor
linearity.data <- data.frame(logit.use, education = dgcurMod$model$educ)

# create a plot with linear and actual relationships shown
linearPlot <- linearity.data %>%
    ggplot(aes(x = education, y = logit.use))+
    geom_point(aes(size = "Observation"), color = "gray50", alpha = .6) +
    geom_smooth(se = FALSE, aes(color = "Loess curve")) + 
    geom_smooth(method = lm, se = FALSE, aes(color = "linear")) + 
    theme_minimal(base_size = 14, base_family = "serif") +
    labs(x = "Education", y = "Log-odds of DG_cur predicted probability") +
    scale_color_manual(name="Type of fit line", values=c("gray50", "black")) +
    scale_size_manual(values = 1.5, name = "")
linearPlot
