treatment <- c(
  'one', 'one', 'one', 
  'two', 'two', 'two', 
  'there', 'there', 'there', 
  'four', 'four', 'four', 
  'five', 'five', 'five', 
  'six', 'six', 'six', 
  'seven', 'seven', 'seven' 
)

res <- c(22.14, 23.26, 22.55, 
         20.22, 21.31, 19.87, 
         19.75, 18.78, 21.78,
         25.78, 24.98, 27.00, 
         30.12, 31.33, 32.22, 
         27.76, 27.64,28.22,
         17.77, 19.88, 18.86)

z <- data.frame(treatment, res)
z

aov.mean <- aggregate(res, by=list(treatment), FUN=mean)
aov.mean

aov.sd <- aggregate(res, by=list(treatment), FUN=sd)
aov.sd

aov <- data.frame(aov.mean, sd=aov.sd$x)
aov

library(ggplot2)
ggplot(data=aov, aes(x=Group.1, y=x)) +
  geom_bar(stat='identity', position='dodge') +
  geom_errorbar(aes(ymax=x+sd, ymin=x-sd), 
                position=position_dodge(0.9), 
                width=0.15) + 
  scale_fill_brewer(palette='Set1')

# 方差分析
fit <- aov(res~treatment)
summary(fit)
library(agricolae)

# Dancan's test
out <- LSD.test(fit, 'treatment', p.adj = 'none')
out

groups <- out$groups$groups
groups
m <- cbind(aov, groups)
m

ggplot(data=m,aes(x=Group.1, y=x))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymax=sd+x,ymin=x-sd),position=position_dodge(0.9),width=0.15)+
  scale_fill_brewer(palette="Set1") + 
  geom_text(aes(y=x+sd+1,label=groups),
                   position=position_dodge(0.9),size=4,fontface="bold")
