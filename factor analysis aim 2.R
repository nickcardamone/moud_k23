library(FactoMineR)
Survey1.df <- Survey1 %>% as.data.frame()
Survey1.pca.mine <- PCA(na.exclude(Survey1.df), graph=TRUE)
Survey1.pca.mine$eig
Survey1.pca.mine
