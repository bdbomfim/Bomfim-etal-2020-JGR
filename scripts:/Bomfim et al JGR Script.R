#Figures and Stats Analyses

#Bomfim et al JGR

#updated July 17, 2019

library(corrplot)
library(pastecs)
library(ggplot2)
library(dplyr)
library(psych)
library(sjstats)
library(ggpubr)
library(magrittr)
library(vegan)
library(MASS)
library(lsmeans)
library(lmerTest)
library(multcompView)
library(mgcv)
library(nlme)
library(lsmeans)
library(afex)
library(tidyverse)
library(patchwork)
library(reshape2)
devtools::install_github("thomasp85/patchwork")
library(RColorBrewer)#color-blind-friendly palette
#palette using grey
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

anfig<-read.csv(file.choose())#Bomfim et al Data Figure 2
attach(anfig)
str(anfig)
anfig$Fire.frequency=as.factor(anfig$Fire.frequency)
str(anfig)
levels(anfig$Depth)
levels(anfig$Depth)=c("0-10","10-30")
levels(anfig$Fire.presence)

#Figure 2#

fig2 <- ggboxplot(anfig, x = "Depth", y = "mean.ANF..ngN.fixed.g.dw.h.",shape="Fire.presence",
                  palette =(c("#000000","#E69F00")),color="Fire.presence",alpha=0.5,fill = "Fire.presence",size=0.8,
                   add = "jitter", width=0.7,ylab="ANF",xlab="Soil depth (cm)",short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),font.main=c(16,"plain"),font.x=c(16,"bold"))
fig2
ggpar(fig2,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Soil depth (cm)",ylab="ANF (ng N gdw-1 h-1)",font.legend = c(20,"bold"))

names(anfig)
levels(anfig$Depth)
anfig$Depth = factor(anfig$Depth,levels(anfig$Depth)[c(2,1)])
colnames(anfig)[colnames(anfig)=="Fire presence"] <- "Fire_presence"

fig2 <- ggboxplot(anfig, x = "Depth", y = "mean.ANF..ngN.fixed.g.dw.h.",shape="Fire_presence",
                  palette =(c("red","white")),alpha=0.9,fill = "Fire_presence",size=0.8,
                  add = "jitter", width=0.7,ylab="ANF",xlab="Soil depth (cm)",short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),font.main=c(16,"plain"),font.x=c(16))
fig2
ggpar(fig2, font.x=c(20),font.y=c(20),legend="top",legend.title ="Fire presence",font.xtickslab = 18,font.ytickslab = 18,orientation = "horizontal", xlab="Soil depth (cm)",ylab="ANF (ng N gdw-1 h-1)",font.legend = c(20))

###### end of figure 2#######

soilfig<-read.csv(file.choose())#Data Figure 3
attach(soilfig)
names(soilfig)
str(soilfig)
soilfig$Fire.frequency=as.factor(soilfig$Fire.frequency)
levels(soilfig$Fire.presence)

#Figure 3

soilfig$logANF=log10(soilfig$mean.ANF..ng.N.g.dw.h.)
soilfig$logavP=log10(soilfig$M3.P..microg.g.)

gam_fig3<-gam(logANF~Fire.presence+s(CN..mass.,logavP,by=Fire.presence),data=soilfig)
summary(gam_fig3)#58% deviance explained
concurvity(gam_fig3,full=TRUE)
concurvity(gam_fig3,full=FALSE)
gam.check(gam_fig3)
hist(residuals(gam_fig3))
plot.gam(gam1D,scheme=2,pages=1,too.far=10,shade=TRUE)

#Figure S1#

figS1 <- ggboxplot(anfig, x = "Stand", y = "mean.ANF..ngN.fixed.g.dw.h.",shape="Fire.presence",
                  palette =(c("#CC79A7","#E69F00","#F0E442","#0072B2", "#D55E00")),color="Fire.frequency",alpha=0.5,fill = "Fire.frequency",size=0.8,
                  add = "jitter", width=0.7,ylim=c(0,2.5),ylab="ANF",xlab="Soil depth (cm)",short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),font.main=c(16,"plain"),font.x=c(16,"bold"))
figS1
figS1a<-facet(figS1 + theme_light(),facet.by="Fire.presence",
              short.panel.labs = FALSE,linetype="dashed",size=3,
              panel.labs.background = list(fill = "white", color = "white"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Fire.presence=c("B","UB")))

ggpar(figS1a,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Stand",ylab="ANF (ng g dw-1 h-1)",font.legend = c(20,"bold"))

#Figure S2#

figS2 <- ggboxplot(soilfig, x = "Stand", y = "CN",shape="Fire.frequency",
                   palette =(c("#CC79A7","#E69F00","#F0E442","#0072B2", "#D55E00")),color="Fire.frequency",alpha=0.5,fill = "Fire.frequency",size=0.8,
                   add = "jitter", width=0.7,ylab="ANF",xlab="Soil depth (cm)",short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),font.main=c(16,"plain"),font.x=c(16,"bold"))
figS2
figS2a<-facet(figS2 + theme_light(),facet.by="Fire.Presence",
              short.panel.labs = FALSE,linetype="dashed",size=3,
              panel.labs.background = list(fill = "white", color = "white"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Fire.Presence=c("B","UB")))

ggpar(figS2a,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Stand",ylab="Total C:N",font.legend = c(20,"bold"))

figS2b <- ggboxplot(soilfig, x = "Stand", y = "M3P.microg.g",shape="Fire.frequency",
                   palette =(c("#CC79A7","#E69F00","#F0E442","#0072B2", "#D55E00")),color="Fire.frequency",alpha=0.5,fill = "Fire.frequency",size=0.8,
                   add = "jitter", width=0.7,ylab="ANF",xlab="Soil depth (cm)",short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),font.main=c(16,"plain"),font.x=c(16,"bold"))
figS2b
figS2bb<-facet(figS2b + theme_light(),facet.by="Fire.Presence",
              short.panel.labs = FALSE,linetype="dashed",size=3,
              panel.labs.background = list(fill = "white", color = "white"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Fire.Presence=c("B","UB")))

ggpar(figS2bb,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Stand",ylab="Available P (micro g g)",font.legend = c(20,"bold"))

#Figure S3#

figS3 <- ggboxplot(soilfig, x = "Stand", y = "TN.gkg",shape="Fire.frequency",
                   palette =(c("#CC79A7","#E69F00","#F0E442","#0072B2", "#D55E00")),color="Fire.frequency",alpha=0.5,fill = "Fire.frequency",size=0.8,
                   add = "jitter", width=0.7,short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),font.main=c(16,"plain"),font.x=c(16,"bold"))
figS3
figS3a<-facet(figS3 + theme_light(),facet.by="Fire.Presence",
              short.panel.labs = FALSE,linetype="dashed",size=3,
              panel.labs.background = list(fill = "white", color = "white"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Fire.Presence=c("B","UB")))

ggpar(figS3a,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Stand",ylab="Total N (g kg-1)",font.legend = c(20,"bold"))


###Table 3##

#calculate standard deviations to add to Table 3
#use data frame soilfig

aggregate(soilfig$CN..mass.,by=list(Firepresence=soilfig$Fire.presence,D=soilfig$Depth), sd,
          na.rm=TRUE)

aggregate(soilfig$CN..mass.,by=list(Firepresence=soilfig$Fire.presence,D=soilfig$Depth), mean,
          na.rm=TRUE)

aggregate(soilfig$mean.ANF..ng.N.g.dw.h.,by=list(Firepresence=soilfig$Fire.presence,D=soilfig$Depth), sd,
          na.rm=TRUE)

aggregate(soilfig$M3.P..microg.g.,by=list(Firepresence=soilfig$Fire.presence,D=soilfig$Depth), sd,
          na.rm=TRUE)

aggregate(soilfig$M3.P..microg.g.,by=list(Firepresence=soilfig$Fire.presence,D=soilfig$Depth), mean,
          na.rm=TRUE)

aggregate(soilfig$TC..g.kg.,by=list(P=soilfig$Fire.presence, S=soilfig$Stand), mean,
          na.rm=TRUE)

aggregate(soilfull$TotalC,by=list(S=soilfull$Stand, P=soilfull$Fire.Presence), mean,
          na.rm=TRUE)
aggregate(soilfull$TotalC,by=list(S=soilfull$Stand, P=soilfull$Fire.Presence), sd,
          na.rm=TRUE)
aggregate(soilfull$`TotalC:N`,by=list(S=soilfull$Stand, P=soilfull$Fire.Presence), mean,
          na.rm=TRUE)
aggregate(soilfull$`TotalC:N`,by=list(S=soilfull$Stand, P=soilfull$Fire.Presence), sd,
          na.rm=TRUE)

aggregate(soilfig$TC..g.kg.,by=list(P=soilfig$Fire.presence, S=soilfig$Stand), sd,
          na.rm=TRUE)

aggregate(soilfig$TN..g.kg.,by=list(Firepresence=soilfig$Fire.presence,D=soilfig$Depth), sd,
          na.rm=TRUE)
aggregate(soilfull$TotalN,by=list(S=soilfull$Stand,Fi=soilfull$Fire.Presence), mean,
          na.rm=TRUE)
aggregate(soilfull$TotalN,by=list(S=soilfull$Stand,Fi=soilfull$Fire.Presence), sd,
          na.rm=TRUE)
hist(soilfig$TC..g.kg.)

aggregate(soilfull$PMN,by=list(FP=soilfull$Fire.Presence, D=soilfull$Depth), mean,
          na.rm=TRUE)
aggregate(soilfull$PMN,by=list(FP=soilfull$Fire.Presence, D=soilfull$Depth), sd,
          na.rm=TRUE)
aggregate(soilfull$TotalP,by=list(FP=soilfull$Fire.Presence, D=soilfull$Depth), mean,
          na.rm=TRUE)
aggregate(soilfull$TotalP,by=list(FP=soilfull$Fire.Presence, D=soilfull$Depth), sd,
          na.rm=TRUE)
aggregate(soilfull$NP,by=list(FP=soilfull$Fire.Presence, D=soilfull$Depth), mean,
          na.rm=TRUE)
aggregate(soilfull$NP,by=list(FP=soilfull$Fire.Presence, D=soilfull$Depth), sd,
          na.rm=TRUE)
aggregate(soilfull$TotalFe,by=list(FP=soilfull$Fire.Presence, D=soilfull$Depth), mean,
          na.rm=TRUE)
aggregate(soilfull$TotalFe,by=list(FP=soilfull$Fire.Presence, D=soilfull$Depth), sd,
          na.rm=TRUE)
aggregate(soilfull$`TotalFe:P`,by=list(FP=soilfull$Fire.Presence, D=soilfull$Depth), mean,
          na.rm=TRUE)
aggregate(soilfull$`TotalFe:P`,by=list(FP=soilfull$Fire.Presence, D=soilfull$Depth), sd,
          na.rm=TRUE)
aggregate(soilfull$`M3-Fe`,by=list(FP=soilfull$Fire.Presence, D=soilfull$Depth), mean,
          na.rm=TRUE)
aggregate(soilfull$`M3-Fe`,by=list(FP=soilfull$Fire.Presence, D=soilfull$Depth), sd,
          na.rm=TRUE)
aggregate(soilfull$`AvailableFe:P`,by=list(FP=soilfull$Fire.Presence, D=soilfull$Depth), mean,
          na.rm=TRUE)
aggregate(soilfull$`AvailableFe:P`,by=list(FP=soilfull$Fire.Presence, D=soilfull$Depth), sd,
          na.rm=TRUE)
aggregate(soilfull$`pH(H2O)`,by=list(FP=soilfull$Fire.Presence, D=soilfull$Depth), mean,
          na.rm=TRUE)
aggregate(soilfull$`pH(H2O)`,by=list(FP=soilfull$Fire.Presence, D=soilfull$Depth), sd,
          na.rm=TRUE)
aggregate(soilfull$`pH(KCl)`,by=list(FP=soilfull$Fire.Presence, D=soilfull$Depth), mean,
          na.rm=TRUE)

aggregate(soilfull$`pH(KCl)`,by=list(FP=soilfull$Fire.Presence, D=soilfull$Depth), sd,
          na.rm=TRUE)
aggregate(soilfull$`TotalC:P`,by=list(FP=soilfull$Fire.Presence, D=soilfull$Depth), mean,
          na.rm=TRUE)

aggregate(soilfull$`TotalC:P`,by=list(FP=soilfull$Fire.Presence, D=soilfull$Depth), sd,
          na.rm=TRUE)

#Table S5

str(soilfull)
aggregate(soilfull$PMN,by=list(Stand=soilfull$Stand,Firepresence=soilfig$Fire.presence,D=soilfig$Depth), mean,
          na.rm=TRUE)
aggregate(soilfull$labmoist.,by=list(Stand=soilfull$Stand,Firepresence=soilfull$Fire.Presence,D=soilfull$Depth), mean,
          na.rm=TRUE)

aggregate(soilfull$labmoist.,by=list(Stand=soilfull$Stand,Firepresence=soilfull$Fire.Presence,D=soilfull$Depth), sd,
          na.rm=TRUE)

####Stats####
names(anfig)

gam1<-gam(mean.ANF..ngN.fixed.g.dw.h.~Stand*Fire.presence*Depth,data=anfig)
summary(gam1)

#least squares means soil ANF per fire frequency
library(car)
str(anfig)
anfig$loganf=log10(anfig$mean.ANF..ngN.fixed.g.dw.h.)
meansANF<-aov(loganf~Stand*Fire.presence,data=anfig)
summary(meansANF)
Anova(meansANF,type="II")
shapiro.test(residuals(meansANF)) #normal distribution assumption ok

aov1 %>%
  lsmeans(specs = "b") %>%
  pairs() %>%
  update(by=NULL, adjust = "tukey")

library(emmeans)
ANF=lsmeans(meansANF,specs="Stand") 

Tukey.HSD_ANF<-cld(leastsquareANF,
                alpha=0.05,
                Letters=letters,      ### Use lower-case letters for .group
                adjust="tukey") 
Tukey.HSD_ANF
kruskal.test(mean.ANF..ngN.fixed.g.dw.h.~Fire.presence,data=anfig)

#least squares means soil CN

meansCN<-lm(CN~Site*Fire.Presence,data=soilfig)
summary(meansCN)
anova(meansCN)
shapiro.test(residuals(meansCN)) #normal distribution assumption ok

leastsquareN =lsmeans(meansCN,
                      pairwise ~ Site*Fire.Presence, 
                      adjust="tukey") 

Tukey.HSDN<-cld(leastsquareN,
                alpha=0.05,
                Letters=letters,      ### Use lower-case letters for .group
                adjust="tukey") 
Tukey.HSDN

#least squares means soil available P

names(soilfig)
meansavP<-lm(M3P.microg.g~Site*Fire.Presence,data=soilfig)
summary(meansavP)
anova(meansavP)
shapiro.test(residuals(meansavP)) #normal distribution assumption ok

leastsquareP =lsmeans(meansavP,
                      pairwise ~ Site*Fire.Presence, 
                      adjust="tukey") 

Tukey.HSDP<-cld(leastsquareP,
                alpha=0.05,
                Letters=letters,      ### Use lower-case letters for .group
                adjust="tukey") 
Tukey.HSDP

#least squares means total soil N

names(soilfig)
meansN<-lm(TN.gkg~Site*Fire.Presence,data=soilfig)
summary(meansN)
anova(meansN)
shapiro.test(residuals(meansN)) #normal distribution assumption ok

leastsquare_N =lsmeans(meansN,
                      pairwise ~ Site*Fire.Presence, 
                      adjust="tukey") 

Tukey.HSD_N<-cld(leastsquare_N,
                alpha=0.05,
                Letters=letters,      ### Use lower-case letters for .group
                adjust="tukey") 
Tukey.HSD_N


pN <- ggboxplot(soilfig, x = "Site", y = "TN.gkg",
                color = "Fire.frequency",palette ="npg",
                add = "jitter", shape="Fire.Presence",width=1.0,ylab="Total soil N (g/kg)",short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),panel.labs=list(Fire.Presence=c("B","NB")),font.main=c(16,"plain"),font.x=c(16,"bold"))
pN

pN1<-facet(pN + theme_bw(), facet.by = "Fire.Presence",
           short.panel.labs = FALSE,   # Allow long labels in panels
           panel.labs.background = list(fill = "lightgray", color = "white"),panel.labs.font = list(color = "black", size = 22))

ggpar(pN1,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Forest site",font.legend = c(20,"bold"))

#least squares means for all soil properties in Table S5

soilfull<-read.csv(file.choose())#Bomfim et al. JGR Full Soil Data
attach(soilfull)
names(soilfull)
str(soilfull)
soilfull$Fire.frequency=as.factor(soilfull$Fire.frequency)
levels(soilfull$Fire.Presence)

names(soilfull)
soilfull$logFeP=log10(soilfull$`TotalFe:P`)
soilfull$logCP=log10(soilfull$`TotalC:P`)
soilfull$logCFe=log10(soilfull$Cfe)
soilfull$logPMN_P=log10(soilfull$PMN.M3P)
soilfull$logNP=log10(soilfull$NP)
soilfull$logM3FeP=log10(soilfull$`AvailableFe:P`)
soilfull$logM3Fe=log10(soilfull$`M3-Fe`)

ts5_1<-lm(`TotalC:N` ~Stand, na.action=na.exclude,data=soilfull)
summary(ts5_1)
anova(ts5_1)
qqnorm(residuals(ts5_1))
qqline(residuals(ts5_1))
shapiro.test(residuals(ts5_1))

#normally distributed, continue
LSts5_1<-emmeans(ts5_1, specs = pairwise ~Stand, type = "response", adjust = "Tukey")
LSts5_1$contrasts %>%
  summary(infer = TRUE)
max.print=999999

LSts5_1b<-emmeans(ts5_1, specs = pairwise ~Fire.Presence*Depth, type = "response", adjust = "Tukey")
LSts5_1b$contrasts %>%
  summary(infer = TRUE)
max.print=999999

emm2 = emmeans(ts5_1, specs = pairwise ~ Depth:Fire.Presence|Stand, type = "response")
emm2
emm2$contrasts %>%
  summary(infer = TRUE)

emm2$contrasts %>%
  rbind()
emm2$contrasts%>%confint()

#Table S10
names(soilfull)
levels(soilfull$Fire.Presence)
levels(soilfull$Depth)=c("0-10","10-30")
soilfull$logANF=log10(soilfull$ANF_nggh)
soilfull$logM3Fe=log10(soilfull$`M3-Fe`)
soilfull$logPMN=log10(soilfull$PMN)
soilfull$logFeP=log10(soilfull$`TotalFe:P`)
soilfull$logAvFeP=log10(soilfull$`AvailableFe:P`)
soilfull$logCP=log10(soilfull$`TotalC:P`)

names(soilfull)
mainfig<-(soilfull[c(1,3,9:10,6:7)])
str(mainfig)

?melt
df.m<-melt(mainfig,Fire.Presence="Label",id=c("Stand","Fire.Presence"))
str(df.m)
ggplot(data = df.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=Fire.Presence, color=Stand))

  
p <- ggplot(data = df.m, aes(y=variable, x=value)) + 
  geom_boxplot(aes(fill=Fire.Presence, color=Stand))
p + facet_wrap( ~ variable, scales="free")

p<- ggboxplot(df.m, x = "Stand", y = "variable",
              palette =(c("#CC79A7","#E69F00","#F0E442","#0072B2", "#D55E00")),color="Fire.Presence",alpha=0.5,fill = "Fire.Presence",size=0.6,
                add = "jitter", width=0.8,ylab="RR Fe Winter",xlab="Vegetation",short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),font.main=c(16,"plain"),font.x=c(16,"bold"))
p
p1<-facet(pFe + theme_light(),facet.by="Site",
            short.panel.labs = FALSE,linetype="dashed",size=3,
            panel.labs.background = list(fill = "white", color = "white"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Site=c("North","Central","South")))

ggpar(p,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Vegetation",font.legend = c(20,"bold"))




#Breaking down in Burned vs Unburned data

burned<-soilfull[soilfull$Fire.Presence == "B",]
burned

unburned<-soilfull[soilfull$Fire.Presence == "UB",]
unburned

names(soilfull)
head(soilfull)

ts5_2<-lm(`logPMN`~Stand*Fire.Presence, na.action=na.exclude,data=soilfull)
summary(ts5_2)
anova(ts5_2)
qqnorm(residuals(ts5_2))
qqline(residuals(ts5_2))
shapiro.test(residuals(ts5_2))

ts5_2a<-lm(TotalC~Stand, na.action=na.exclude,data=burned)
summary(ts5_2a)

#normally distributed, continue

emmPMN = emmeans(ts5_2, specs = pairwise ~ Fire.Presence|Stand, type = "response")
emmPMN
emmPMN$contrasts %>%
  summary(infer = TRUE)

max.print=999999

emm2 = emmeans(ts5_2, specs = pairwise ~ Stand, type = "response", adjust="Tukey")
emm2
emm2$contrasts %>%
  summary(infer = TRUE)

head(soilfull)
p1<-plot(LSts5_1, comparisons = TRUE,labcex=3,xlab="Estimated marginal means log-transformed Total C:P",ylab="Stand")+theme_bw()
p1
#The blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. 
#If an arrow from one mean overlaps an arrow from another group, the difference is not “significant,” based on the adjust setting (which defaults to "tukey") and the value of alpha (which defaults to 0.05).

ts5_3<-lm(TotalC~Stand, na.action=na.exclude,data=soilfull)
summary(ts5_3)
anova(ts5_3)
qqnorm(residuals(ts5_3))
qqline(residuals(ts5_3))
shapiro.test(residuals(ts5_3))
emm3 = emmeans(ts5_3, specs = pairwise ~ Fire.Presence*Depth|Stand, type = "response", adjust="Tukey")
emm3
p2<-plot(emm3, comparisons = TRUE,xlab="Least-squares mean (Total P)",ylab="Fire presence")+theme_bw()
p2

names(soilfull)
ts5_4<-lm(logNP~Stand*Fire.Presence*Depth, na.action=na.exclude,data=soilfull)

emm2 = emmeans(ts5_4, specs = pairwise ~ Depth:Fire.Presence|Stand, type = "response")
emm2
emm2$contrasts %>%
  summary(infer = TRUE)

emm4 = emmeans(ts5_4, specs = pairwise ~ Fire.Presence|Depth, type = "response", adjust="Tukey")
emm4

p3<-plot(emm4, comparisons = TRUE,xlab="Least-squares mean (log total N:P)",ylab="Fire presence")+theme_bw()
p3

ts5_5<-lm(M3Fe_M3P~Fire.Presence*Depth, na.action=na.exclude,data=soilfull)

emm5 = emmeans(ts5_5, specs = pairwise ~ Fire.Presence|Depth, type = "response", adjust="Tukey")
emm5

p5<-plot(emm5, comparisons = TRUE,xlab="Least-squares mean (available Fe:P)",ylab="Fire presence")+theme_bw()
p5

p1 + p2 + p3 + p5+ plot_layout(ncol = 1)

ts5_6<-lm(logCP~Fire.Presence*Depth, na.action=na.exclude,data=soilfull)

emm6 = emmeans(ts5_6, specs = pairwise ~ Fire.Presence|Depth, type = "response", adjust="Tukey")
emm6

p6<-plot(emm6, comparisons = TRUE,xlab="Least-squares mean (log Total C:P)",ylab="Fire presence")+theme_bw()
p6

ts5_7<-lm(lab.moisture_.~Stand*Fire.Presence*Depth, na.action=na.exclude,data=soilfull)

emm7 = emmeans(ts5_7, specs = pairwise ~ Depth:Fire.Presence|Stand, type = "response", adjust="Tukey")
emm7

p7<-plot(emm7, comparisons = TRUE,xlab="Least-squares mean (pH in H2O)",ylab="Fire presence")+theme_bw()
p7

ts5_8<-lm(pH_KCl~Fire.Presence*Depth, na.action=na.exclude,data=soilfull)

emm8 = emmeans(ts5_8, specs = pairwise ~ Fire.Presence|Depth, type = "response", adjust="Tukey")
emm8

p8<-plot(emm8, comparisons = TRUE,xlab="Least-squares mean (pH in KCl)",ylab="Fire presence")+theme_bw()
p8

ts5_9<-lm(PMN~Fire.Presence*Depth, na.action=na.exclude,data=soilfull)

emm9 = emmeans(ts5_9, specs = pairwise ~ Fire.Presence|Depth, type = "response", adjust="Tukey")
emm9

p9<-plot(emm9, comparisons = TRUE,xlab="Least-squares mean (PMN - NH4-N)",ylab="Fire presence")+theme_bw()
p9

p6 + p7 + p8 + plot_layout(ncol = 1)

ts5_10<-lm(Tfe_g_kg~Fire.Presence*Depth, na.action=na.exclude,data=soilfull)

emm10 = emmeans(ts5_10, specs = pairwise ~ Fire.Presence|Depth, type = "response", adjust="Tukey")
emm10

p10<-plot(emm10, comparisons = TRUE,xlab="Least-squares mean (Total Fe)",ylab="Fire presence")+theme_bw()
p10

ts5_11<-lm(logFeP~Fire.Presence*Depth, na.action=na.exclude,data=soilfull)

emm11 = emmeans(ts5_11, specs = pairwise ~ Fire.Presence|Depth, type = "response", adjust="Tukey")
emm11

p11<-plot(emm11, comparisons = TRUE,xlab="Least-squares mean (log Total Fe:P)",ylab="Fire presence")+theme_bw()
p11

p9 + p10 + p11 + plot_layout(ncol = 1)

emm2$contrasts %>%
  rbind()
emm2$contrasts%>%confint()

#regressions PMN and ANF

p_2<-gam(ANF_nggh~Fire.Presence+Depth+s(PMN,by=Fire.Presence),data=soilfull)
summary(p_2)
plot(p_2)

#Transforming Table S8 in Figure S4 (correlations among all studied variables)

#Correlation plot to supporting information
names(soilfull)

#renaming columns for final figure
colnames(soilfull)[colnames(soilfull)=="CN_mass"] <- "TotalC:N"
colnames(soilfull)[colnames(soilfull)=="M3P_microgg"] <- "M3-P"
colnames(soilfull)[colnames(soilfull)=="TC_gkg"] <- "TotalC"
colnames(soilfull)[colnames(soilfull)=="TN_gkg"] <- "TotalN"
colnames(soilfull)[colnames(soilfull)=="TP.g_kg"] <- "TotalP"
colnames(soilfull)[colnames(soilfull)=="Fe.P"] <- "TotalFe:P"
colnames(soilfull)[colnames(soilfull)=="M3Fe_M3P"] <- "AvailableFe:P"
colnames(soilfull)[colnames(soilfull)=="CP"] <- "TotalC:P"
colnames(soilfull)[colnames(soilfull)=="Tfe_gkg"] <- "TotalFe"
colnames(soilfull)[colnames(soilfull)=="M3Fe_microgg"] <- "M3-Fe"
colnames(soilfull)[colnames(soilfull)=="field.g_g.dw"] <- "Moisture"
colnames(soilfull)[colnames(soilfull)=="pH_H2O"] <- "pH(H2O)"
colnames(soilfull)[colnames(soilfull)=="pH_KCl"] <- "pH(KCl)"

#preparing data
names(soilfull)
s8plot<-cor(soilfull[c(9,10,7,8,14,13,17,23,15,24,16,11,18)], method="s")
s8plot
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(soilfull[c(9,10,7,8,14,13,17,23,15,24,16,11,18)])

s8plot2<-corrplot(s8plot, col=col,type="full",order="hclust"
                  ,tl.col="black", p.mat = p.mat, sig.level = 0.05,
                  number.cex = 2,tl.cex = 1.3,addshade = "all",tl.pos="lt",
                  number.font = 2,hclust.method = "ward.D2",addrect = 3,cl.lim=c(-1,1),
                  cl.length = 5,cl.cex=1.5,"pie")

s8plotb<-cor(soilfull[c(6,9,10,7,8,14,13,17,23,15,24,16,11,18)], method="s")
s8plotb
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(soilfull[c(6,9,10,7,8,14,13,17,23,15,24,16,11,18)])

s8plot2b<-corrplot(s8plotb, col=col,type="full",order="hclust"
                  ,tl.col="black", p.mat = p.mat, sig.level = 0.05,
                  number.cex = 2,tl.cex = 1.3,addshade = "all",tl.pos="lt",
                  number.font = 2,hclust.method = "ward.D2",addrect = 3,cl.lim=c(-1,1),
                  cl.length = 5,cl.cex=1.5,"pie")

#species abundance new Figure S1
peaveg2<-read.csv(file.choose())#PEA sp matrix dbh 10 updated
attach(peaveg2)
dim(peaveg2)
str(peaveg2)
names(peaveg2)

vegfb<-data.matrix(peaveg2[,2:44])#use this one for updated dbh30
str(vegfb)
head(vegfb)
vegfb[1:43]=as.numeric(vegfb[1:43])
str(vegfb)
rownames(vegfb) <- peaveg2$Site.DBH.10

H<-diversity(vegfb, index = "shannon")
H
S <- specnumber(vegfb) ## rowSums(BCI > 0) does the same...
S
J <- H/log(S)
J
disms <- vegdist(decostand(vegfb,"norm"), "bray")
disms

heatmap(scale(vegfb), Rowv = NA, Colv = NA,
        scale = "none",cexRow = 1.5,cexCol=1)
levels(peaveg2$Site.DBH.10)
rownames(vegfb) <- peaveg2$Site.DBH.10
rownames(vegfb)

scaleyellowred <- colorRampPalette(c("lightyellow", "red"), space = "rgb")(100)
col <- colorRampPalette(brewer.pal(11, "RdYlBu"))(256)
maxab <- apply(vegfb, 2, max)
head(maxab)

disms <- vegdist(decostand(vegf,"norm"), "bray")
disms
data.distb <- vegdist(decostand(vegfb, "norm"),method = "bray")
row.clusb <- hclust(data.distb, "aver")
heatmap(as.matrix(vegfb), Rowv = as.dendrogram(row.clusb), cexRow = 1.5,cexCol=1.2,Colv = NA, col = scaleyellowred, margins = c(10, 3))

row.clus2 <- hclust(data.distb, "ward.D2")
heatmap(as.matrix(vegfb), Rowv = as.dendrogram(row.clus2), cexRow = 1.6,cexCol=0.9,Colv = NA, col = scaleyellowred, margins = c(10, 3))


data.dist.gb <- vegdist(t(vegfb), method = "bray")
col.clusb <- hclust(data.dist.gb, "aver")
heatmap(as.matrix(vegfb), Rowv = as.dendrogram(row.clus2), Colv = as.dendrogram(col.clusb), col = scaleyellowred, margins = c(10, 3))

cols <- brewer.pal(4, "BuGn") # sets how many colours of the palette are fixed??
# can not be because it allows later even pla(2) so a number smaller than four
pal <- colorRampPalette(cols)
# pal is now a function
colourmap <- pal(20)
heatmap(vegfb, col = colourmap)
