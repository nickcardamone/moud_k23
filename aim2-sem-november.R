### Aim 2 SEM

library(tidyverse)
library(dplyr)
library(lavaan)
library(knitr)
library(mvnormalTest)
library(tidySEM)


data <- readxl::read_xlsx("C://Users//Nick//Desktop//Aim2_10-22-2022.xlsx")


bupe <- data[,1:11]
meth <- data[,12:22]
ntx <- data[,23:33]

bupe <- bupe %>% na.omit()
meth <- meth %>% na.omit()
ntx <- ntx %>% na.omit()


mvnout <- mardia(data)

## Shapiro-Wilk Univariate normality test
mvnout$uv.shapiro

## Mardia Multivariate normaility test
mvnout$mv.test

#Results from both the univariate and multivariate tests indicate that the measures do not come from normally distributed univariate or multivariate distributions 
#(the 'No' results in the table). We address these issues in the following model specification stage.

bupe_model <- '
  attitudes =~ bupe_stressful + bupe_inconvenient + bupe_uncomf + bupe_foolish + bupe_useless
  norms =~ bupe_willingness + bupe_supervisor
  control =~ bupe_autonomy + bupe_confidence + bupe_availability
  bupe_likelihood ~ attitudes + norms + control'

meth_model <- '
  attitudes =~ meth_stressful + meth_inconvenient + meth_uncomf + meth_foolish + meth_useless
  norms =~ meth_willingness + meth_supervisor
  control =~ meth_autonomy + meth_confidence + meth_availability
  meth_likelihood ~ attitudes + norms + control'

ntx_model <- '
  attitudes =~ ntx_stressful + ntx_inconvenient + ntx_uncomf + ntx_foolish + ntx_useless
  norms =~ ntx_willingness + ntx_supervisor
  control =~ ntx_autonomy + ntx_confidence + ntx_availability
  ntx_likelihood ~ attitudes + norms + control'

fit.mod <- sem(meth_model, data = meth, std.lv = TRUE, estimator = "MLM")

fitMeasures(fit.mod, c("chisq.scaled", "df.scaled", "pvalue.scaled"))

fitMeasures(fit.mod, c("rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled"))

fitMeasures(fit.mod, c("cfi.scaled", "srmr"))

## MEASUREMENT MODEL
standardizedsolution(fit.mod, type = "std.all", se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE)%>% 
  filter(op == "=~") %>% 
  select(LV=lhs, Item=rhs, Coefficient=est.std, ci.lower, ci.upper, SE=se, Z=z, 'p-value'=pvalue)

parameterEstimates(fit.mod, standardized=TRUE, rsquare = TRUE) %>% 
  filter(op == "r2") %>% 
  select(Item=rhs, R2 = est)

## STRUCTURAL MODEL
standardizedsolution(fit.mod, type = "std.all", se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE)%>% 
  filter(op == "~") %>% 
  select(LV=lhs, Item=rhs, Coefficient=est.std, ci.lower, ci.upper, SE=se, Z=z, 'p-value'=pvalue)


## GRAPH BUPE
lay = get_layout("", "", "", "", "", "bupe_likelihood", "", "", "", "",
                 "", "", "attitudes","","norms","","control","", "", "",
                 "bupe_foolish", "bupe_inconvenient","bupe_stressful", "bupe_uncomf", "bupe_useless", "bupe_willingness", "bupe_supervisor", "bupe_autonomy", "bupe_availability", "bupe_confidence", rows = 3)
graph_sem(fit.mod, layout = lay)


## GRAPH BUPE
lay = get_layout("", "", "", "", "", "meth_likelihood", "", "", "", "",
                 "", "", "attitudes","","norms","","control","", "", "",
                 "meth_foolish", "meth_inconvenient","meth_stressful", "meth_uncomf", "meth_useless", "meth_willingness", "meth_supervisor", "meth_autonomy", "meth_availability", "meth_confidence", rows = 3)
graph_sem(fit.mod, layout = lay)

## GRAPH NTX
lay = get_layout("", "", "", "", "", "ntx_likelihood", "", "", "", "",
                 "", "", "attitudes","","norms","","control","", "", "",
                 "ntx_foolish", "ntx_inconvenient","ntx_stressful", "ntx_uncomf", "ntx_useless", "ntx_willingness", "ntx_supervisor", "ntx_autonomy", "ntx_availability", "ntx_confidence", rows = 3)
graph_sem(fit.mod, layout = lay)

