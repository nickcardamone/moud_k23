data2 <- readxl::read_xlsx("C://Users//Nick//Desktop//Aim2_10-22-2022.xlsx")


ggplot(data, aes(y = ntx_likelihood, x = ntx_se)) +
  geom_point() +
  theme_classic(base_size = 15) +
  stat_smooth(method = "lm", formula = 'y ~ x', se=F,fullrange = T) +
  facet_wrap(~ORG)


library(MASS)
# Fit the full model 
full.model <- lm(Fertility ~., data = swiss)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)


mod1 <- lm(ntx_likelihood~ ntx_se +ntx_norms+ntx_attitudes, data = data)

summary(mod1)

library(lmerTest)
library(ordinal)
data$ntx_likelihood <- as.ordered(data$ntx_likelihood)
data$bupe_likelihood <- as.ordered(data$bupe_likelihood)
data$meth_likelihood <- as.ordered(data$meth_likelihood)

data$ORG <- as.factor(data$ORG)

preds <- data %>% select(ntx_se, ntx_norms, ntx_attitudes, bupe_se, bupe_norms, bupe_attitudes, meth_se, meth_norms, meth_attitudes)
preds <- scale(preds)
data <- data.frame(data$ORG, data$bupe_likelihood, data$meth_likelihood, data$ntx_likelihood, preds)


## NTX

ntx_mod_null <- ordinal::clmm(data.ntx_likelihood ~ (ntx_se | data.ORG) + (ntx_norms | data.ORG) + (ntx_attitudes | data.ORG), data=data)
ntx_mod <- ordinal::clmm(data.ntx_likelihood ~ ntx_se + ntx_norms + ntx_attitudes + (ntx_se | data.ORG) + (ntx_norms | data.ORG) + (ntx_attitudes | data.ORG), data=data)

summary(ntx_mod_null)
summary(ntx_mod)

ntx_mod_drop <- drop1(ntx_mod)
ntx_mod_drop[ntx_mod_drop$AIC == min(ntx_mod_drop$AIC), ]

ntx_mod_final <- clmm2(data.ntx_likelihood ~ ntx_se + ntx_norms + ntx_attitudes, random = data.ORG, Hess = T, data=data)

summary(ntx_mod_final)

#######

## BUPE

bupe_mod_null <- ordinal::clmm(data.bupe_likelihood ~ (bupe_se | data.ORG) + (bupe_norms | data.ORG) + (bupe_attitudes | data.ORG), data=data)
bupe_mod <- ordinal::clmm(data.bupe_likelihood ~ bupe_se + bupe_norms + bupe_attitudes + (bupe_se | data.ORG) + (bupe_norms | data.ORG) + (bupe_attitudes | data.ORG), data=data)

summary(bupe_mod_null)
summary(bupe_mod)

bupe_mod_drop <- drop1(bupe_mod)
bupe_mod_drop[bupe_mod_drop$AIC == min(bupe_mod_drop$AIC), ]

bupe_mod_final <- clmm2(data.bupe_likelihood ~ bupe_se + bupe_norms + bupe_attitudes, random = data.ORG, Hess = T, data=data)

summary(bupe_mod_final)

#######

## METH

meth_mod_null <- ordinal::clmm(data.meth_likelihood ~ (meth_se | data.ORG) + (meth_norms | data.ORG) + (meth_attitudes | data.ORG) + (1 | data.ORG), data=data)
meth_mod <- ordinal::clmm(data.meth_likelihood ~ meth_se + meth_norms + meth_attitudes + (meth_se | data.ORG) + (meth_norms | data.ORG) + (meth_attitudes | data.ORG) + (1 | data.ORG), data=data)

summary(meth_mod_null)
summary(meth_mod)

meth_mod_drop <- drop1(meth_mod)
meth_mod_drop[meth_mod_drop$AIC == min(meth_mod_drop$AIC), ]

meth_mod_final <- clmm2(data.meth_likelihood ~ meth_se + meth_norms + meth_attitudes, random = data.ORG, Hess = T, data=data)

summary(meth_mod_final)


meth_data <- data %>% select(data.Org, data.meth_likelihood, meth_se, meth_norms, meth_attitudes)

meth_data <- na.omit(meth_data)

m_norms <- clmm2(data.meth_likelihood ~ meth_norms, Hess = T, data=data)
m_attitudes <- clmm2(data.meth_likelihood ~ meth_norms+meth_attitudes, data=data)

m0 <- clmm2(data.meth_likelihood ~ meth_se + meth_norms + meth_attitudes, data=data)
m1 <- clmm2(data.meth_likelihood ~ meth_se + meth_norms + meth_attitudes, random = data.ORG, Hess = T, data=data)

anova(m_norms, m_attitudes)





