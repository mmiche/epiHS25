maric <- readr::read_csv("/Users/mmiche/Desktop/TeachingClass/FS2025/TheorieseminarFS25/ProjectMaricAddOn/Maric2022_20250422/baza_cov2soul_recode_final_07.3.2022_no_code.csv")
# names(maric)

# Example of preprocessing:
maric$gender01 <- maric$gender - 1

# 
# Run a two-sided t test
(t.res <- t.test(phq9 ~ gender01, data=maric, alternative="two.sided", var.equal=TRUE))
t.res$statistic
t.res$statistic**2 # see smryaov[[1]]$`F value` (same, no coincidence!)
t.res$p.value # see smryaov[[1]]$`Pr(>F)` (same, because two.sided t.test)

# Run an anova
(smryaov <- summary(aov(phq9 ~ gender01, data=maric)))
smryaov[[1]]$`F value` # see t.res$statistic**2 (no coincidence)
smryaov[[1]]$`Pr(>F)` # t.res$p.value (no coincidence)

# Run a linear regression (outcome is continuous, predictor 'group' is dichotomous)
summary(lm(phq9 ~ gender01, data=maric))
# (Most relevant) output in R console
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   2.5648     0.1540  16.653  < 2e-16
# gender01      1.2715     0.2151   5.912  4.4e-09
# 
# Residual standard error: 3.728 on 1201 degrees of freedom
# Multiple R-squared:  0.02828,	Adjusted R-squared:  0.02747 
# F-statistic: 34.95 on 1 and 1201 DF,  p-value: 4.395e-09

# No coincidences:
# t value: -5.91215 and Estimate/Std. Error = 1.2715/.2151 = 5.912
# t value squared = 34.95352 = smryaov[[1]]$`Pr(>F)` = F-statistic: 34.95
# t.res$p.value = smryaov[[1]]$`Pr(>F)` = Pr(>|t|) of gender01 = 4.4e-09 = p-value of entire model = 4.395e-09 (only if linear model contains one binary predictor, i.e., no adjusting variables).