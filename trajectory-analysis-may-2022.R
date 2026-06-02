vars <- readxl::read_xlsx("C://Users//Nick//Desktop//coef-barriers.xlsx")

library(dotwhisker)
library(broom)
library(dplyr)

vars2 <- vars %>%
  dplyr::mutate(
    .data = .,
    term = Variable,
    estimate = Coefficient,
    conf.low = Coefficient - 1.96 * error,
    conf.high = Coefficient + 1.96 * error
  ) %>% select(term, estimate, conf.low, conf.high, p)
p = dotwhisker::dwplot(vars2)


three_brackets <- list(
  c("Organizational", "% Methadone", "Average Caseload"),
  c("Demographic", "% Black", "Average age"),
  c("Barriers", "X-waiver", "Alternatives")
)



{

p + geom_vline(
    xintercept = 0,
    colour = "grey60",
    linetype = 2) + xlab("Coefficient Estimate") + ylab("") + 
    geom_segment(aes(x=conf.low,y=term,xend=conf.high,
                     yend=term,col=p<0.05))+
    geom_point(aes(x=estimate,y=term,col=p<0.05)) + 
  ggtitle("Single Logistic Regression") +
  theme(
    plot.title = element_text(face = "bold") 
  )
} %>% dotwhisker::add_brackets(three_brackets, fontSize = 0.5)

vars <- read.csv("C://Users//Nick//Desktop//Regression Data.csv")
vars2 <- vars %>% select(-agency_name)
vars2 <- as.data.frame(vars2)
vars2$pct_black <- as.numeric(vars2$pct_black)
vars2$pct_male <- as.numeric(vars2$pct_male)
vars2$avg_age <- as.numeric(vars2$avg_age)

dat2 <- vars2 %>% mutate_at(2:29, ~(scale(.) %>% as.vector))

dat2 <- dat2 %>% pivot_longer(cols = 2:29, names_to = "vars", values_to = "vals")
library(ggridges)
dat2$vars <- factor(dat2$vars, ordered = TRUE)

bar <- dat2 %>% filter(vars == "avg_age" | vars == "caseload" | vars == "pct_male" | vars == "pct_black" | vars == "methadone" | vars == "linkout_oralbup")

bar %>%  ggplot(aes(x = vals, y=vars, fill = as.factor(group_r))) + geom_density_ridges(quantile_lines=TRUE,
                                                                                       quantile_fun=function(x,...)mean(x), alpha = 0.7) + theme_ridges(center = TRUE) + ggtitle("Organizational Characteristics") #+ facet_grid(rows = vars(type), scales = "free_y")




data <- dat2 %>% select(-GROUP)

model <- lm(data = data, group_r ~ .)
summary(model)
library(car)
library(grid)
library(gridBase)
vif_values <- vif(model)
library(tibble)
vif <- tibble::rownames_to_column(vif, "name")
ggplot(data = vif, mapping = aes(y = vif_values, x = name))  + coord_flip() + geom_col() + geom_hline(yintercept = 5, color = "red")


#create horizontal bar chart to display each VIF value
b <- barplot(vif_values, main = "VIF Values", horiz = T, col = "steelblue", names.arg="", xlim=c(0,50))
vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)

grid.text(names(vif_values),
          y = unit(b, "native"), x=unit(-1, "lines"))
popViewport(3)
#add vertical line at 5
abline(h = 5, lwd = 3, lty = 2)


#https://jslefche.github.io/sem_book/composite-variables.html

install.packages("corrplot")
library(corrplot)
corr <- cor(data, use="pairwise.complete.obs", method="pearson") 
testRes = cor.mtest(data, conf.level = 0.95)

corrplot(corr, p.mat = testRes$p, method = 'color', diag = FALSE, type = 'upper',
         sig.level = c(0.0001, 0.001, 0.01, 0.05), pch.cex = 0.9,
         insig = 'label_sig', pch.col = 'grey20', order = 'FPC')



