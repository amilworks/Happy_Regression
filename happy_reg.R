# Project 
happy <- read.table("/Users/Macbook/Documents/Regression_analysis/R_files/projdata.txt", header = TRUE)
head(happy)

ggpairs(happy, title = "Scatterplot Matrix of Patient Satisfaction", columnLabels = c("Happy", "Gender", "Workhours", "Relationship"))+amil

gender <- as.factor(happy$gender)

ggplot(data = happy, aes(x = happy, y = relationship))+amil+
  geom_point(data = happy, shape = sprintf('\u2665'), col = c("#040D40","#FF404C")[gender], size = happy$workhrs/1.1, alpha = .3)+
  ggtitle("| Happiness and Relationship", subtitle = "    How a Healthy Relationship Relates to Happiness")+
  labs(x= "Happiness Level", y="Relationship Level")

ggsave("haprel.pdf", device=cairo_pdf, family="Arial Unicode MS", width = 15, height = 9, scale = 1.3)
#col = "#FF404C",
summary(happy)
interaction.plot(happy$relationship, happy$gender, response = happy$happy)

lm(data= happy, happy~.) %>% drop1(test="F")
lm(data= happy, happy~.^2) %>% drop1(test="F")

fit_hap <- lm(data= happy, happy~.)
summary(fit_hap)

happy %>% 
  ggplot() +
  aes(x = relationship, color=gender, group = factor(gender), y = happy) +
  stat_summary(fun.y = mean, geom = "point", size=7) +
  stat_summary(fun.y = mean, geom = "line")+amil+ggtitle("Interaction between Relationship and Gender")





ggplot(data = happy, aes(x = happy, y = relationship))+amil+
  geom_point(data = happy, shape = sprintf('\u2665'), col = c("#040D40","#FF404C")[gender], size = happy$workhrs/1.1, alpha = .3)+
  geom_line(aes(y=fitted(fit_hap)))


 autoplot(fit_hap, which = 1:2, label.n = 0, colour = "#01526D", size = 2, color="gender")+amil

grid.arrange(resid1,diag1, nrow=2)

resid1 <- ggplot(data = happy, aes(fit_hap$residuals))+
  geom_histogram( bins = 20, color="white", fill="#01526D")+amil+
  ggtitle('Histogram of Residuals', subtitle = "Fitted Multiple Linear Model")+
  labs(x="Residuals", y="Count")




t1 <- lm(happy~., data=happy)
drop1(t1, ~.^2, test="F", trace = TRUE)
drop1(t1, test="F", trace = TRUE)
drop1(update(t1,.~.-gender*workhrs), test="F", trace = TRUE)

t2 <- lm(happy ~ relationship + gender:relationship + workhrs:relationship, data=happy)
summary(t1)


t3 <- lm(happy ~ gender + workhrs + relationship + gender*relationship, data=happy)
summary(t3)

drop1(t3, test="F", trace = TRUE)

t5 <- lm(happy~.^2, data=happy)


anova(t1, t3, t5)

extractAIC(t1)

null=lm(happy~1,data=happy)
full=lm(happy~.^2,data=happy) 

step(null,scope=list(lower=null,upper=full),direction='forward')

step(full,direction='backward')

step(null,scope=list(upper=full),direction='both', steps = 5000, k=log(n)) %>% anova()



#final model:
 # lm(formula = happy ~ relationship + gender + workhrs + relationship:gender, 
  #   data = projdata)

 ggplot(data = happy, aes(t3$residuals))+
  geom_histogram( bins = 20, color="white", fill="#01526D")+amil+
  ggtitle('Histogram of Residuals', subtitle = "Fitted Multiple Linear Model")+
  labs(x="Residuals", y="Count")

 
 autoplot(t3, which = 1:2, label.n = 0, colour = "#01526D", size = 2, color="gender")+amil
 
 
 
 hap_f <- filter(happy, gender == 1)
 hap_m <- filter(happy, gender == 0)
 
 
 ggplot(happy) +
   aes(x = relationship, y = happy) +
   geom_point(color = "#01526D", size=3, alpha=.5) +
   geom_smooth(method = "lm", col="#FF404C", size=2, se=FALSE, data = hap_f) +
   geom_smooth(method = "lm", col="#0D85E7", size=2, se=FALSE, data = hap_m)+amil+
   ggtitle("Interaction Plot: Relationship vs. Happy", "Visualizing Interactions for Happiness")+labs(x="Relationship", y="Happy")
 
