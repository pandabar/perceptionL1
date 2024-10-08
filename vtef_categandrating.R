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
vtefL1tidy <- read_csv("vtefL1tidy.csv")
vtefL1tidy$id <-as.factor(vtefL1tidy$id)
vtefL1tidy$token <-as.factor(vtefL1tidy$token)
vtefL1tidy$prof<-as.factor(vtefL1tidy$prof)
vtefL1tidy$item <-as.factor(vtefL1tidy$item)
vtefL1tidy$categ <-as.factor(vtefL1tidy$categ)
vtefL1tidy$rating <-factor(vtefL1tidy$rating, ordered=TRUE)
vtefL1tidy$stimulus <-ifelse(vtefL1tidy$token=="vtef1", "1(=[\u028c])",
                             ifelse(vtefL1tidy$token=="vtef2", "2",
                                    ifelse(vtefL1tidy$token=="vtef3", "3",
                                           ifelse(vtefL1tidy$token=="vtef4","4", "5(=[\u025b])"))))
vtefL1tidy$stimulus <-as.factor(vtefL1tidy$stimulus)
vtefL1tidy$perc_category <-ifelse(vtefL1tidy$categ=="B1", "/a/", "/e/")
vtefL1tidy$perc_category <-as.factor(vtefL1tidy$perc_category)
#add f1 and f2
vowels  <- c("vtef1", "vtef2", "vtef3", "vtef4", "vtef5")
F1 <- c(833, 832.5, 832, 831.5, 831)
F2 <- c(1453, 1589, 1726, 1863, 2000)

#add Hz and F2Bark
vtefL1tidy$F1<- F1[match(vtefL1tidy$token, vowels)] 
vtefL1tidy$F2<- F2[match(vtefL1tidy$token, vowels)] 
vtefL1tidy$Bark <- bark(vtefL1tidy$F2)

#remove data obtained from stimulus (token) 5, cuz there's no point in comparing ratings by group if A= 0 
notoken5<-subset(vtefL1tidy, token !="vtef5")

#subset by category
vtefL1tidya <-subset(vtefL1tidy, categ=="B1")
vtefL1tidye <-subset(vtefL1tidy, categ=="B3")

# also subset by group
vtefL1tidyAdv<-subset(vtefL1tidy, prof=="A")
vtefL1tidyBeg<-subset(vtefL1tidy, prof=="B")

#ordinal regression. I used novtef5 to test for group, but
#since there was no effect of group I ran another clmm with
#all the data

myols002 = clmm(rating ~ stimulus*perc_category + (1|id), data=vtefL1tidy) #this one
Anova.clmm(myols002, type="III")

# Post-hoc emmeans
ph<-emmeans(myols002, pairwise~ perc_category|stimulus)

#plot raw data (counts)
mylabs<- c('vtef1'="1 (=[\u028C])", 'vtef2'="2", 'vtef3'="3", 'vtef4'="4", 'vtef5'="5 (=[\u025B])")

vtefL1tidy %>%
  ggplot(aes(prof, fill=categ)) +
  geom_bar(position ="stack", stat="count") +
  scale_y_continuous(limit=c(0, 80)) +
  scale_x_discrete(labels=c("A", "B")) +
  labs(y= "Categorizations (counts)", x="Group", fill = "Categorized as") +
  facet_grid(cols= vars(token), labeller=as_labeller(mylabs)) +
  theme_classic() + scale_fill_grey(start=0.7, end=0.3, labels= c("/a/", "/e/"))

#log regression for categs
##Approach: RESP == a
vtefL1tidy$RESP<-ifelse(vtefL1tidy$categ=="B1", 0, 1)
notoken5$RESP<-ifelse(notoken5$categ=="B1", 0, 1)
myregnew01<-glmer(RESP ~ prof * token + (1|id), family=binomial, glmerControl(optimizer="bobyqa"), data=notoken5)
Anova(myregnew01, type="III")
emmeans(myregnew01, pairwise~prof|token)

#this plots the interaction effect
cat_plot(myregnew01, pred= prof, modx=token, geom="line", colors="Greys", legend.main = "Stimulus", x.label= "Group", y.label="Probability of perceiving /e/", modx.labels= c("1 (=[\u028C])", "2", "3", "4"), line.thickness= 0.8, vary.lty=TRUE)+
  theme_classic() + annotate(geom="text", x=1.5, y=c(0.8, 0.95), label= "paste(italic(p) , \" < .05\")", parse = TRUE, size=3)

#plot ratings - EXTREMELY CONVOLUTED but does the job
#for categs as /a/
vtefL1tidyaAdv<- subset(vtefL1tidya, prof=="A")
vtefL1tidyaBeg<- subset(vtefL1tidya, prof=="B")
mytabaA<- table(vtefL1tidyaAdv$rating, vtefL1tidyaAdv$token)
mytabaB<- table(vtefL1tidyaBeg$rating, vtefL1tidyaBeg$token)

#counts
Item <-rep(c("1 (= [\u028c])", "2", "3", "4", "5 (= [\u025B])"), 2)
r1<-c(mytabaA[1,], mytabaB[1,])
r2<-c(mytabaA[2,], mytabaB[2,])
r3<-c(mytabaA[3,], mytabaB[3,])
r4<-c(mytabaA[4,], mytabaB[4,])
r5<-c(mytabaA[5,], mytabaB[5,])
group <-rep(c("A","B"), each=5)

sumdfa<- data.frame(Item, r1, r2, r3, r4, r5, group)

sumdfa <- sumdfa %>%
  dplyr::rename("1 (Very bad)" = r1,
                "2" = r2,
                "3" = r3,
                "4" = r4,
                "5 (Very good)" = r5)

##for categs as /e/
vtefL1tidyeAdv<- subset(vtefL1tidye, prof=="A")
vtefL1tidyeBeg<- subset(vtefL1tidye, prof=="B")
mytabeA<- table(vtefL1tidyeAdv$rating, vtefL1tidyeAdv$token)
mytabeB<- table(vtefL1tidyeBeg$rating, vtefL1tidyeBeg$token)

#for counts
Item <-rep(c("1 (= [\u028c])", "2", "3", "4", "5 (= [\u025B])"), 2)
r1<-c(mytabeA[1,], mytabeB[1,])
r2<-c(mytabeA[2,], mytabeB[2,])
r3<-c(mytabeA[3,], mytabeB[3,])
r4<-c(mytabeA[4,], mytabeB[4,])
r5<-c(mytabeA[5,], mytabeB[5,])
group <-c(rep(c("A","B"), each=5))

sumdfe<- data.frame(Item, r1, r2, r3, r4, r5, group)

sumdfe <- sumdfe %>%
  dplyr::rename("1 (Very bad)" = r1,
                "2" = r2,
                "3" = r3,
                "4" = r4,
                "5 (Very good)" = r5)
combined<-data.frame(rbind(sumdfa, sumdfe))
colnames(combined)<- c("Item", "1 (very bad)", "2", "3", "4", "5 (very good)", "group")
combined$categ <-rep(c("perceived as /a/", "perceived as /e/"), each=10)
combined$group <-c(rep(c("Group A","Group B"), each=5), rep(c("Group A","Group B"), each=5))
combined$group=as.factor(combined$group)
combined$categ=as.factor(combined$categ)
#install.packages("gridExtra")
require(gridExtra)
require(lattice)
#####THIS IS WHAT I DEFINITELY USED####
HH::likert(Item~.|categ + group, combined, positive.order=FALSE, as.percent = FALSE, strip = TRUE, strip.left=FALSE,
           main="", col=c("#7b3294", "#c2a5cf", "#f7f7f7", "#a6dba0", "#008837"),
           xlab="Counts",ylab="Stimulus", rightAxis=FALSE, sub= list("Rating",x=unit(.55, "npc")))
#######################################
