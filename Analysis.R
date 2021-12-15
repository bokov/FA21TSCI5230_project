#' ## Model  Summary

#' Call for any packages and data needed to run script
packageList <- c("knitr","Publish","ggplot2","rio","GGally","dplyr","pander","readr","synthpop", "rmarkdown", "readxl", "tibble", "broom")
for(package in packageList){
  if(!require(package,character.only = TRUE)){
    install.packages(package);require(package,character.only = TRUE);}
};

if(!file.exists('data/dtrain.Rds')){source('data.R',local=TRUE,verbose=FALSE)};
 dtrain <- readRDS('data/dtrain.Rds');
 dtest <- readRDS('data/dtest.Rds');

#' Fitting data to appropriate statistic linear model with our predictors
fit <- lm(NOREADMISSION ~ ACS_PER_CAPITA_INCOME + ACS_PCT_POSTHS_ED + ACS_MEDIAN_HOME_VALUE + ACS_PCT_MEDICAID_ANY + ACS_PCT_DISABLE + SDI_SCORE, data = dtrain);
# summary(fit);
#' Provide a clean version of the summary - both produce the same results
# summary(fit) %>% tidy;
# fit %>% tidy;
#' Show a quick look at all of the summary statistics
# glance(fit);

#' Create a 'null' model that does not specify predictors
fit0 <- lm(NOREADMISSION ~ 1, data = dtrain);
# summary(fit0);
# summary(fit0) %>% tidy;
# fit0 %>% tidy;
# glance(fit0);

# When using lm repeatedly you can use '.~' to keep the original outcome and '~.' to keep the original predictors
#'
#' Create an all encompassing model for your predictors
# Include some interaction terms with interaction model: 'OUTCOME ~ PRED1*PRED2*PRED3 ...'
# For all 2 way interactions : '~ (A+B+C)^2', for all 3 way interactions: '~ (A+B+C)^3'
# Additive model for note: 'OUTCOME ~ PRED1 + PRED2 + PRED3 + ...'
# Formula with all numeric predictiors:
frm_huge <- select(dtrain,where(is.numeric)) %>% names() %>%
  setdiff('NOREADMISSION') %>% paste(collapse=' + ') %>%
  paste('NOREADMISSION ~ (',.,')^2') %>% as.formula
#fit1 <- lm(NOREADMISSION ~ (ACS_PER_CAPITA_INCOME + ACS_PCT_POSTHS_ED + ACS_MEDIAN_HOME_VALUE + ACS_PCT_MEDICAID_ANY + ACS_PCT_DISABLE + SDI_SCORE)^2, data = dtrain);
#fit1 <- update(fit,frm_allnum);
#fit2 <- update(fit1,.~(.)^2);

# If you forget the arguments for a function you can check it with args()
# i.e. args(step)
#' Create a BIC stepwise regression
# BIC is when k is the log of the number of rows of the data file
# AIC is always set at 2
set.seed(12345);
fitBIC <- step(fit, scope = list(lower = fit0, upper = frm_huge), direction = "both", k = log(nrow(dtrain)));
# summary(fitBIC) %>% tidy;

#' Compare the models
# anova(fit0, fit1);
# plot(fit1);
# plot(data$________, predict(fit1,dat1test)-dat1test$________);
# plot(dat1train$________, predict(fit1)-dat1train$________)

#Save enviroment of the script for use in R markdown file
save.image(file='data/Analysis.Rdata')
#load('data/Analysis.Rdata')
saveRDS(fit, file="data/analysis_fit.RDS")
saveRDS(fitBIC, file="data/analysis_fitBIC.RDS")
