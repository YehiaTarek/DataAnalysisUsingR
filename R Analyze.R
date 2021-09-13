library(readxl)
pok <- read_excel("C:/Users/yehia/Desktop/R BW/pok.xlsx")
View(pok)
duration= pok$HP
range(duration)
min(duration)
breaks = seq(1,255, by = 10)
breaks
duration.cut = cut(duration, breaks, right=FALSE) 
duration.freq = table(duration.cut) 
duration.freq
summary(pok)
hist(duration,right=FALSE)    
duration.relfreq = duration.freq / nrow(pok)
duration.relfreq
duration.cumfreq = cumsum(duration.freq)
duration.cumfreq
duration.cumrelfreq = duration.cumfreq / nrow(pok)
cumrelfreq0 = c(0, duration.cumrelfreq) 
plot(breaks,cumrelfreq0,main="Weights",xlab="weights",ylab="Cumulative frequency") 
lines(breaks, cumrelfreq0) 
stem(pok$HP)
boxplot(duration, horizontal=TRUE)
barplot(duration.freq)
dotchart(pok$HP)
mytable <- table(pok$Generation)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls,
    main="Pie Chart of generations") 

t.test(pok$Attack,pok$Defense,paired = TRUE)
t.test(pok$HP,mu=3)  
 chisq.test(pok$Attack, pok$Defense)
 fisher.test(pok$Attack, pok$Defense,simulate.p.value=TRUE)
 source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
 crosstab(pok, row.vars = "Generation", col.vars = "Type", type = "c")
 crosstab(pok, row.vars = c("Generation", "Type"), col.vars = "Legendary", type = "t")

 cor(pok$Attack,pok$Defense,method="pearson")
 cor.test(pok$Attack,pok$Defense,method="pearson")
 cov(pok$Attack,pok$Defense,method="pearson")
mod<-lm(pok$Defense~pok$Attack)
summary(mod)


boxplot(pok$Legendary)
t.test(pok$Legendary, mu=0,alternative = "two.sided",conf.level = 0.95, var.eq=FALSE,paired=FALSE)

error <- qt(0.975,df=length(pok$HP)-1)*sd(pok$HP)/sqrt(length(pok$HP))
error
left <- mean(pok$HP)-error
right <- mean(pok$HP)+error
left
right
res <- wilcox.test(pok$Attack, pok$Defense)
res