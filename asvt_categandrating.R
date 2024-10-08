##REQUIRED PACKAGES
library(ggplot2)
library(tidyverse)
library(tidymodels)
library(lme4)
library(lmerTest)
library(car)
library(ordinal)
library(emmeans)
library(broom)
library(sjPlot)
library(hrbrthemes)
library(RColorBrewer)
library(generalhoslem)
library(emuR)
library(ggsignif)
library(interactions)
library(effects)
library(RVAideMemoire)
library(dplyr)
library(likert)
asvtL1tidy <- read_csv("asvtL1tidy.csv")
asvtL1tidy$id <-as.factor(asvtL1tidy$id)
asvtL1tidy$token <-as.factor(asvtL1tidy$token)
asvtL1tidy$prof<-as.factor(asvtL1tidy$prof)
asvtL1tidy$item <-as.factor(asvtL1tidy$item)
asvtL1tidy$categ <-as.factor(asvtL1tidy$categ)
asvtL1tidy$rating <-factor(asvtL1tidy$rating, ordered=TRUE)
asvtL1tidy$stimulus <-ifelse(asvtL1tidy$token=="asvt1", "1(=[\u0251])",
                             ifelse(asvtL1tidy$token=="asvt2", "2",
                                    ifelse(asvtL1tidy$token=="asvt3", "3",
                                           ifelse(asvtL1tidy$token=="asvt4","4", "5(=[\u028c])"))))
asvtL1tidy$stimulus <-as.factor(asvtL1tidy$stimulus)

asvtL1tidy$perc_category <-ifelse(asvtL1tidy$categ=="B1","/a/", "/o/")
asvtL1tidy$perc_category <-as.factor(asvtL1tidy$perc_category)
#add f1 and f2
stim <- c("asvt1", "asvt2", "asvt3", "asvt4", "asvt5")
#subset by category
asvtL1tidya <-subset(asvtL1tidy, categ=="B1")
asvtL1tidyo <-subset(asvtL1tidy, categ=="B2")

# also subset by group
asvtL1tidyAdv<-subset(asvtL1tidy, prof=="A")
asvtL1tidyBeg<-subset(asvtL1tidy, prof=="B")

#ordinal regression
myols0 = clmm(rating~ token*categ +(1|id), data = asvtL1tidy)
myols1 = clmm(rating~ stimulus + perc_category + (1|id), data= asvtL1tidy)
myols2 = clmm(rating~ token + (1|id), data= asvtL1tidy)
myols3 = clmm(rating~ categ + (1|id), data= asvtL1tidy)
summary(myols0)
Anova.clmm(myols0, type="III")
myols001 = clmm(rating~(prof*categ) + (prof*token) + (categ*token) + (1|id), data = asvtL1tidy)
drop1(myols0, test="Chisq")
myols002 = clmm(rating ~ token*categ + (1|id), data=vtefL1tidy) #this one
Anova.clmm(myols002, type="III")
#myols003 = clmm(rating ~ token*prof + (1|id), data=notoken5)
#myols004 = clmm(rating ~ categ*prof + (1|id), data=notoken5)
sjPlot::plot_model(myols002, type = "pred", terms= c("categ", "token"))
ph1<-emmeans(myols0, ~ token)
ph2<-emmeans(myols0, ~ categ)

#plot raw data
my_labels <- c("1 (very bad exemplar)",
               "2", 
               "3", 
               "4",
               "5 (very good exemplar)") 
percentages<-c(89,83,73, 75, 11, 46, 2, 35, 32)
mylabs<- c('asvt1'="1 (=[\u0251])", 'asvt2'="2", 'asvt3'="3", 'asvt4'="4", 'asvt5'="5 (=[\u028C])")

asvtL1tidy %>%
  ggplot(aes(prof, fill=categ)) +
  geom_bar(position ="stack", stat="count") +
  scale_y_continuous(limit=c(0, 80)) +
  scale_x_discrete(labels=c("A", "B")) +
  labs(y= "Categorizations (counts)", x="Group", fill = "Categorized as") +
  facet_grid(cols= vars(token), labeller=as_labeller(mylabs)) +
  theme_classic() + scale_fill_grey(start=0.7, end=0.3, labels= c("/a/", "/o/"))

#log regression for categs
##Approach: RESP == a
asvtL1tidy$RESP<-ifelse(asvtL1tidy$categ=="B1", 0, 1)
myregnew01<-glmer(RESP ~ token + (1|id), family=binomial, glmerControl(optimizer="bobyqa"), data=asvtL1tidy)
Anova(myregnew01, type="III")
posthoc<-emmeans(myregnew01, pairwise~ token)

##Likert plots
#for categs as /a/
asvtL1tidyaAdv<- subset(asvtL1tidya, prof=="A")
asvtL1tidyaBeg<- subset(asvtL1tidya, prof=="B")
mytabaA<- table(asvtL1tidyaAdv$rating, asvtL1tidyaAdv$token)
mytabaB<- table(asvtL1tidyaBeg$rating, asvtL1tidyaBeg$token)

#counts
Item <-rep(c("1 (= [\u0251])", "2", "3", "4", "5 (= [\u028c])"), 2)
r1<-c(mytabaA[1,], mytabaB[1,])
r2<-c(mytabaA[2,], mytabaB[2,])
r3<-c(mytabaA[3,], mytabaB[3,])
r4<-c(mytabaA[4,], mytabaB[4,])
r5<-c(mytabaA[5,], mytabaB[5,])
group <-rep(c("Group A","Group B"), each=5)
categ<-rep("perceived as /a/", 10)
sumdfa<- data.frame(Item, r1, r2, r3, r4, r5, group, categ)

sumdfa <- sumdfa %>%
  dplyr::rename("1 (Very bad)" = r1,
                "2" = r2,
                "3" = r3,
                "4" = r4,
                "5 (Very good)" = r5)

##for categs as /o/
asvtL1tidyoAdv<- subset(asvtL1tidyo, prof=="A")
asvtL1tidyoBeg<- subset(asvtL1tidyo, prof=="B")
mytaboA<- table(asvtL1tidyoAdv$rating, asvtL1tidyoAdv$token)
mytaboB<- table(asvtL1tidyoBeg$rating, asvtL1tidyoBeg$token)

#counts
Item <-rep(c("1 (= [\u0251])", "2", "3", "4", "5 (= [\u028c])"), 2)
r1<-c(mytaboA[1,], mytaboB[1,])
r2<-c(mytaboA[2,], mytaboB[2,])
r3<-c(mytaboA[3,], mytaboB[3,])
r4<-c(mytaboA[4,], mytaboB[4,])
r5<-c(mytaboA[5,], mytaboB[5,])
group <-rep(c("Group A","Group B"), each=5)
categ<- rep("perceived as /o/", 10)

sumdfo<- data.frame(Item, r1, r2, r3, r4, r5, group, categ)

sumdfo <- sumdfo %>%
  dplyr::rename("1 (Very bad)" = r1,
                "2" = r2,
                "3" = r3,
                "4" = r4,
                "5 (Very good)" = r5)
#merge both dataframes
combined2<-data.frame(rbind(sumdfa, sumdfo))
colnames(combined2)<- c("Item", "1 (very bad)", "2", "3", "4", "5 (very good)", "group", "categ")
combined$group=as.factor(combined$group)
combined$categ=as.factor(combined$categ)

HH::likert(Item~.|categ + group, combined2, positive.order=FALSE, as.percent = FALSE, strip = TRUE, strip.left=FALSE,
           main="", col=c("#7b3294", "#c2a5cf", "#f7f7f7", "#a6dba0", "#008837"),
           xlab="Counts",ylab="Stimulus", rightAxis=FALSE, sub= list("Rating",x=unit(.55, "npc")))
