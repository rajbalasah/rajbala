
############## 1 sample z test ##########
Faltoons=read.csv(file.choose())
View(Faltoons)
attach(Faltoons)
table(Weekdays,Weekend)
chisq.test(table(Weekdays,Weekend))


################ One-way Anova ########
Anova_results <- aov(Weekdays~Weekend,data = Faltoons)
summary(Anova_results)

############### Kruskal Wallis #################
kruskal.test(Weekdays,Weekend)

