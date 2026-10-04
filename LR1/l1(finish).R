library(nortest)
library(goftest)
library(rcompanion)
library(ppcor)
library(corrplot)
library(GGally)
library(ggplot2)


df <- read.csv2("var_15.csv")
df <- df[, -1]

#3
names(df)[names(df) == "Average.income"] <- "Average income"
names(df)[names(df) == "Work.experience"] <- "Work experience"
names(df)[names(df) == "Professional.specialization"] <- "Professional specialization"
names(df)[names(df) == "Average.number.of.pages"] <- "Average number of pages"
names(df)[names(df) == "Activity.level..score."] <- "Activity level (score)"
names(df)[names(df) == "Activity.level..level."] <- "Activity level (level)"

#4
df$"Group" <- as.factor(df$"Group")
df$"Gender" <- as.factor(df$"Gender")
df$"Age" <- as.integer(df$"Age")
df$"Work experience" <- as.numeric(df$"Work experience")
df$"Average income" <- as.numeric(df$"Average income")
df$"Professional specialization" <- as.factor(df$"Professional specialization")
df$"Average number of pages" <- as.integer(df$"Average number of pages")
df$"Activity level (score)" <- as.integer(df$"Activity level (score)")
df$"Activity level (level)" <- as.factor(df$"Activity level (level)")
print(str(df))

#5
print(summary(df))


#6
#6.1
dev.new()
plot(df$"Work experience", df$"Average income",main = "Диаграмма рассеяния", xlab = "Стаж работы", ylab = "Средний доход", pch = 11)

#6.2
dev.new()
x <- c(summary(df$"Professional specialization"))
piepercent <- round(100*x/sum(x), 1)
pie(x,piepercent,main="Профессиональная специализация", col=c("red", "blue","green","yellow"),clockwise=TRUE)
legend("topright", c("Не использую","Крайне редко","Ежедневно,","Постоянно"), fill =c("red", "blue","green","yellow"))

#6.3
dev.new()
par(mfrow = c(2, 2))
x <- table(df$"Activity level (level)"[(df$"Gender"=="1")&(df$"Group"=="1")])
piepercent <- round(100*x/sum(x), 1)
pie(x,piepercent,main="Мужчины первой группы", col=c("red", "blue","green"),clockwise=TRUE)
legend("topright", c("High","Low","Medium"), fill =c("red", "blue","green"))
x <- table(df$"Activity level (level)"[(df$"Gender"=="1")&(df$"Group"=="2")])
piepercent <- round(100*x/sum(x), 1)
pie(x,piepercent,main="Мужчины второй группы", col=c("red", "blue","green"),clockwise=TRUE)
legend("topright", c("High","Low","Medium"), fill =c("red", "blue","green"))
x <- table(df$"Activity level (level)"[(df$"Gender"=="2")&(df$"Group"=="1")])
piepercent <- round(100*x/sum(x), 1)
pie(x,piepercent,main="Женщины первой группы", col=c("red", "blue","green"),clockwise=TRUE)
legend("topright", c("High","Low","Medium"), fill =c("red", "blue","green"))
x <- table(df$"Activity level (level)"[(df$"Gender"=="2")&(df$"Group"=="1")])
piepercent <- round(100*x/sum(x), 1)
pie(x,piepercent,main="Женщины второй группы", col=c("red", "blue","green"),clockwise=TRUE)
legend("topright", c("High","Low","Medium"), fill =c("red", "blue","green"))

#6.4
dev.new()
par(mfrow = c(2, 2))
barplot(table(df$"Activity level (score)"[(df$"Gender"=="1")&(df$"Group"=="1")]),col=c("red"),main="Мужчины первой группы",xlab="Степень активности", ylab="Количество наблюдений")
barplot(table(df$"Activity level (score)"[(df$"Gender"=="1")&(df$"Group"=="2")]),col=c("blue"),main="Мужчины второй группы",xlab="Степень активности", ylab="Количество наблюдений")
barplot(table(df$"Activity level (score)"[(df$"Gender"=="2")&(df$"Group"=="1")]),col=c("green"),main="Женщины первой группы",xlab="Степень активности", ylab="Количество наблюдений")
barplot(table(df$"Activity level (score)"[(df$"Gender"=="2")&(df$"Group"=="2")]),col=c("yellow"),main="Женщины второй группы",xlab="Степень активности", ylab="Количество наблюдений")

#6.5
dev.new()
boxplot(df$"Average income" ~ df$"Gender", xlab = "Пол", ylab = "Средний доход", data = df)

#6.6
dev.new()
par(mfrow = c(2, 3))
hist(df$"Age", freq=FALSE, breaks=12,xlabel="Возраст",main="Возраст")
hist(df$"Work experience", freq=FALSE, breaks=12,xlabel="Стаж работы",main="Стаж работы")
hist(df$"Average income", freq=FALSE, breaks=12,xlabel="Средний доход",main="Средний доход")
hist(df$"Average number of pages", freq=FALSE, breaks=12,xlabel="Среднее количество просматриваемых страниц в месяц",main="Среднее количество просматриваемых страниц в месяц")
hist(df$"Activity level (score)", freq=FALSE, breaks=12,xlabel="Степень активности",main="Степень активности")

#6.7
dev.new()
pairs(~df$"Age"+df$"Work experience"+df$"Average income"+df$"Average number of pages"+df$"Activity level (score)",data=df,main="Матричный график", col="red")

#7
#7.1
print(pearson.test(df$"Average income"))
print(ad.test(df$"Average income"))
print(shapiro.test(df$"Average income"))
plotNormalHistogram(df$"Average income", xlab="Средний доход", ylab="Количество наблюдений", length = 1000, breaks = seq(min(df$"Average income"), max(df$"Average income"),length.out = 7))

#7.2
M <- df[,unlist(lapply(df, is.numeric))]
N1<-cor(M, use="pairwise.complete.obs",method="pearson")
N2<-cor(M, use="pairwise.complete.obs",method="spearman")
N3<-cor(M, use="pairwise.complete.obs",method="kendall")
View(N1)
View(N2)
View(N3)

#7.3
print(cor.test(df$"Work experience",df$"Average number of pages", method="pearson"))

#7.4
C1<-pcor(M)
View(C1$estimate)
View(C1$p.value)
C2<-pcor(M, method="spearman")
C3<-pcor(M, method="kendall")

#7.5
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD",
"#4477AA"))
corrplot(N1, method="color", col=NULL,type="upper", order="hclust",
addCoef.col = "black", tl.col="black", tl.srt=45,
sig.level = 0.01, insig = "blank",
diag=FALSE
)

#8
aov_model<-aov(df$"Work experience"~df$"Group",data=df)
summary(aov_model)
kruskal.test(df$"Work experience"~df$"Group",data=df)

#9
table_P9<-table(df$"Activity level (level)", df$"Professional specialization")
View(table_P9)
prop.table(table_P9)
addmargins(table_P9)
chisq.test(table(df$"Activity level (level)",df$"Gender"))
fisher.test(table(df$"Activity level (level)",df$"Gender"))

factor_names<-names(df[,unlist(lapply(df, is.factor))])
chi_results<-matrix(NA,nrow = length(factor_names),ncol=length(factor_names))
colnames(chi_results)<-c(factor_names )
rownames(chi_results)<-c(factor_names )
for (col_name in factor_names){
  for (col_name2 in factor_names){
    chi_results[col_name,col_name2] <-
    chisq.test(table(df[,col_name],df[,col_name2]))$p.value
  }
}
chi_results

fisher_results<-matrix(NA,nrow = length(factor_names),ncol=length(factor_names))
colnames(fisher_results)<-c(factor_names )
rownames(fisher_results)<-c(factor_names )
for (col_name in factor_names){
  for (col_name2 in factor_names){
    fisher_results[col_name,col_name2] <-
      fisher.test(table(df[,col_name],df[,col_name2]), workspace = 2e7)$p.value
  }
}
fisher_results

#10
ggpairs(df,columns = 2:5, aes(color = Group,alpha = 0.5))
ggpairs(df,columns = c(2,6,9), aes(color = Group,alpha = 0.5))
ggpairs(df,columns = c(3:5,7:8), aes(color = Group,alpha = 0.5))
ggpairs(df,columns = c(2,8:9), aes(color = Group,alpha = 0.5))
