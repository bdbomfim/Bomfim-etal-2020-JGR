#CHAPTER 3 PEA 16

soilanf<-read.csv(file.choose())#SOIL ANF PEA 16_updated
attach(soilanf)
str(soilanf)
library(ggplot2)
library(ggpubr)
library(magrittr)
library(mgcv)
library(nlme)
library(dplyr)
library(psych)
library(sjstats)

#to make figures
pea2<-read.csv(file.choose())#ALL SOIL PEA 16_updated_noI7
attach(pea2)
names(pea2)
str(pea2)#n=182

For <-soilanf[soilanf$Vegetation == "forest",]
levels(For$Site)
str(For)
levels(For$Fire.presence)
levels(For$Depth)=c("0-10","10-30")

levels(soilanf$Fire.presence)=c("B","UB")
levels(soilanf$Depth)=c("0-10","10-30")
For$Fire.frequency=as.factor(For$Fire.frequency)

levels(soilanf$Depth)=c("0-10","10-30")
levels(soilanf$Depth)
PEAanf$Site=as.factor(PEAanf$Site)
levels(soilanf$Site)

str(soilanf)
soilanf$Fire.frequency=as.factor(soilanf$Fire.frequency)
PEAanf$Rep=as.factor(PEAanf$Rep)
PEAanf$Depth=as.factor(PEAanf$Depth)

Grasses <-soilanf[soilanf$Vegetation == "grassland",]
Grasses

FORESTS <-soilanf[soilanf$Vegetation == "forest",]
FORESTS

str(FORESTS)

aggregate(FORESTS$mean.ANF..ngN.fixed.g.dw.h.,by=list(FP=FORESTS$Fire.presence,D=FORESTS$Depth), mean,
          na.rm=TRUE)
aggregate(gamfinal$ANF.ng.N_g.DW_h,by=list(lu=gamfinal$S), mean,
          na.rm=TRUE)
aggregate(gamfinal$ANF.ng.N_g.DW_h,by=list(lu=gamfinal$D), mean,
          na.rm=TRUE)


Top<-FORESTS[FORESTS$Depth == "0-10",]
Top

Bottom<-FORESTS[FORESTS$Depth == "10-30",]
Bottom

names(FORESTS)
levels(FORESTS$Fire.presence)
names(soilanf)

p <- ggboxplot(soilanf, x = "Vegetation", y = "mean.ANF..ngN.fixed.g.dw.h.",
                color = "Fire.presence",palette ="npg",fill="lightgray",
                add = "jitter", width=1.0,ylim=c(0,3),ylab="soil ANF (ng N/g dw/h)",xlab="Vegetation type",short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),font.main=c(16,"plain"),font.x=c(16,"bold"))
p
p0<-facet(p + theme_bw(), facet.by = "Depth",
          short.panel.labs = FALSE,   # Allow long labels in panels
          panel.labs.background = list(fill = "gray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Depth=c("0-10","10-30")))

ggpar(p,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Site",font.legend = c(20,"bold"))

levels(FORESTS$Fire.presence)
levels(FORESTS$Site)=c("G1","G2","G3","G5","G6","G7","F2","F2b","F1","F4","F5")

p1 <- ggboxplot(For, x = "Depth", y = "mean.ANF..ngN.fixed.g.dw.h.",
                color = "Fire.presence",palette ="npg",
                add = "jitter", width=0.8,ylab="ANF (ng g dw-1 h-1)",xlab="Soil depth (cm)",short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),panel.labs=list(Depth=c("0-10","10-30")),font.main=c(16,"plain"),font.x=c(16,"bold"))
p1
p2<-facet(p1 + theme_bw(), facet.by = "Fire.presence",
          short.panel.labs = FALSE,   # Allow long labels in panels
          panel.labs.background = list(fill = "gray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Fire.presence=c("B","UB")))

ggpar(p1,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Soil depth (cm)",font.legend = c(20,"bold"))

p_1 <- ggboxplot(FORESTS, x = "Depth", y = "mean.ANF..ngN.fixed.g.dw.h.",
                color = "Fire.presence",palette ="lancet",
                add = "jitter", width=1.0,ylim=c(0,3),ylab="ANF (ng g dw-1 h-1)",xlab="Soil depth (cm)",short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),panel.labs=list(Fire.presence=c("B","NB")),font.main=c(16,"plain"),font.x=c(16,"bold"))
p_1
p_2<-facet(p_1 + theme_bw(), facet.by = "Fire.presence",
          short.panel.labs = FALSE,   # Allow long labels in panels
          panel.labs.background = list(fill = "gray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Fire.presence=c("B","UB")))

ggpar(p_1,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Site",font.legend = c(20,"bold"))

anf_mean<-lm(mean.ANF..ngN.fixed.g.dw.h.~Fire.presence*Depth,data=FORESTS)
summary(anf_mean)
shapiro.test(FORESTS$mean.ANF..ngN.fixed.g.dw.h.)#W=0.91
shapiro.test(Top$mean.ANF..ngN.fixed.g.dw.h.)#W=0.94
leastsquare_anf =lsmeans(anf_mean,
                       pairwise ~ Fire.presence*Depth, 
                       adjust="tukey") 

Tukey.HSD_anf<-cld(leastsquare_anf,
                 alpha=0.05,
                 Letters=letters,      ### Use lower-case letters for .group
                 adjust="tukey") 
Tukey.HSD_anf

options(max.print=999999)

anf2_mean<-lm(mean.ANF..ngN.fixed.g.dw.h.~Site*Fire.presence,data=FORESTS)
summary(anf2_mean)
shapiro.test(residuals(anf2_mean))#W=0.91
shapiro.test(FORESTS$mean.ANF..ngN.fixed.g.dw.h.)#W=0.91
shapiro.test(Top$mean.ANF..ngN.fixed.g.dw.h.)#W=0.94
leastsquare_anf2 =lsmeans(anf2_mean,
                         pairwise ~ Site*Fire.presence, 
                         adjust="tukey") 

Tukey.HSD_anf2<-cld(leastsquare_anf2,
                   alpha=0.05,
                   Letters=letters,      ### Use lower-case letters for .group
                   adjust="tukey") 
Tukey.HSD_anf2

kruskal.test(mean.ANF..ngN.fixed.g.dw.h.~Fire.frequency,data=Top)
str(FORESTS)

F5 <-FORESTS[FORESTS$Site == "I6",]
F5

kruskal.test(mean.ANF..ngN.fixed.g.dw.h.~Fire.presence,data=F5)#p=0.14

F1 <-FORESTS[FORESTS$Site == "I3",]
F1

kruskal.test(mean.ANF..ngN.fixed.g.dw.h.~Fire.presence,data=F1)

F2 <-FORESTS[FORESTS$Site == "I1",]
F2

kruskal.test(mean.ANF..ngN.fixed.g.dw.h.~Fire.presence,data=F2)

F2b <-FORESTS[FORESTS$Site == "I2",]
F2b

kruskal.test(mean.ANF..ngN.fixed.g.dw.h.~Fire.presence,data=F2b)

F4 <-FORESTS[FORESTS$Site == "I5",]
F4

kruskal.test(mean.ANF..ngN.fixed.g.dw.h.~Fire.presence,data=F4)


p3 <- ggboxplot(FORESTS, x = "Site", y = "mean.ANF..ngN.fixed.g.dw.h.",
                color = "Fire.frequency", shape="Fire.presence",palette ="npg",
                add = "jitter", width=1.0,ylab="soil ANF (ng N/g dw/h)",xlab="Seasonally flooded forest",short.panel.labs = FALSE,order = c("G3","I3","G1","I1","G2","I2","G5","I5","G6","I6"),font.label = list(size = 14, face = "bold"),panel.labs=list(Depth=c("0-10","10-30")),font.main=c(16,"plain"),font.x=c(16,"bold"))
p3
p4<-facet(p3 + theme_bw(), facet.by = "Fire.presence",
          short.panel.labs = FALSE,   # Allow long labels in panels
          panel.labs.background = list(fill = "gray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Depth=c("0-10","10-30")))

ggpar(p4,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Site",font.legend = c(20,"bold"))

p3 <- ggboxplot(forests, x = "Site", y = "mean.ANF..ngN.fixed.g.dw.h.",
                color = "Fire.class",palette ="npg",
                add = "jitter", width=1.0,ylab="soil ANF (ng N/g dw/h)",xlab="Seasonally flooded forest",short.panel.labs = FALSE,order = c("G3","I3","G1","I1","G2","I2","G5","I5","G6","I6"),font.label = list(size = 14, face = "bold"),panel.labs=list(Fire.presence=c("B","NB")),font.main=c(16,"plain"),font.x=c(16,"bold"))
p3
p4<-facet(p3 + theme_bw(), facet.by = "Fire.presence",
          short.panel.labs = FALSE,   # Allow long labels in panels
          panel.labs.background = list(fill = "gray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Fire.presence=c("B","NB")))

ggpar(p4,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Site",font.legend = c(20,"bold"))

names(FORESTS)
levels(FORESTS$Fire.presence)

p1.1 <- ggboxplot(FORESTS, x = "Site", y = "mean.ANF..ngN.fixed.g.dw.h.",
                color = "Fire.frequency",palette ="npg",
                add = "jitter", width=1.0,ylab="Soil ANF (ng N/g dw/ h)",ylim=c(0,2.5),short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),order=c("I3","I1","I2","I5","I6"),panel.labs=list(Fire.presence=c("B","NB")),font.main=c(16,"plain"),font.x=c(16,"bold"))
p1.1
p2.1<-facet(p1.1 + theme_bw(), facet.by = "Fire.presence",
          short.panel.labs = FALSE,   # Allow long labels in panels
          panel.labs.background = list(fill = "gray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Fire.presence=c("B","NB")))

ggpar(p2.1,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Forest site",font.legend = c(20,"bold"))

p2 <- ggboxplot(forests, x = "Fire.presence", y = "mean.ANF..ngN.fixed.g.dw.h.",
                  color = "Depth",palette ="npg",
                  add = "jitter", fill="lightgray",width=1.0,ylab="Soil ANF (ng N/g dw/ h)",ylim=c(0,3),short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),panel.labs=list(Depth=c("0-10","10-30")),font.main=c(16,"plain"),font.x=c(16,"bold"))
p2
ggpar(p2,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Forest site",font.legend = c(20,"bold"))

p2a <- ggboxplot(FORESTS, x = "Depth", y = "mean.ANF..ngN.fixed.g.dw.h.",
                palette ="npg", color="Fire.presence",
                add = "jitter", fill="lightgray",width=1.0,ylab="Soil ANF (ng N/g dw/ h)",ylim=c(0,3),short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),panel.labs=list(Depth=c("0-10","10-30")),font.main=c(16,"plain"),font.x=c(16,"bold"))
p2a
ggpar(p2a,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Soil depth (cm)",font.legend = c(20,"bold"))



p3<-facet(p2 + theme_bw(), facet.by = "Depth",
            short.panel.labs = FALSE,   # Allow long labels in panels
            panel.labs.background = list(fill = "lightgray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Fire.scar=c("NB","B")))

ggpar(p3,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Vegetation",font.legend = c(20,"bold"))

str(forests)
p4 <- ggboxplot(impucas, x = "Site", y = "mean.ANF..ngN.fixed.g.dw.h.",
                color = "Fire.scar",palette ="lancet",fill="lightgray",
                add = "jitter", width=1.0,ylab="Soil ANF (ng N/g dw/ h)",ylim=c(0,5),short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),panel.labs=list(Depth=c("0-10","10-30")),font.main=c(16,"plain"),font.x=c(16,"bold"))
p4

p5<-facet(p4 + theme_bw(), facet.by = "Depth",
          short.panel.labs = FALSE,   # Allow long labels in panels
          panel.labs.background = list(fill = "lightgray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Depth=c("0-10","10-30")))

ggpar(p5,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Seasonally flooded forest",font.legend = c(20,"bold"))

p4.1 <- ggboxplot(grass, x = "Site", y = "mean.ANF..ngN.fixed.g.dw.h.",
                color = "Fire.scar",palette ="lancet",fill="lightgray",
                add = "jitter", width=1.0,ylab="Soil ANF (ng N/g dw/ h)",ylim=c(0,3),short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),panel.labs=list(Depth=c("0-10","10-30")),font.main=c(16,"plain"),font.x=c(16,"bold"))
p4.1

p5.1<-facet(p4.1 + theme_bw(), facet.by = "Depth",
          short.panel.labs = FALSE,   # Allow long labels in panels
          panel.labs.background = list(fill = "lightgray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Depth=c("0-10","10-30")))

ggpar(p5.1,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Seasonally flooded grassland",font.legend = c(20,"bold"))

p6 <- ggboxplot(grass, x = "Fire.legacy", y = "mean.ANF..ngN.fixed.g.dw.h.",
                color = "Cluster",palette ="lancet",
                add = "jitter",fill="lightgray",shape="Depth",width=1.0,ylab="Soil ANF (ng N/g dw/ h)",ylim=c(0,3),short.panel.labs = FALSE,font.label = list(size = 14, face = "bold"),panel.labs=list(Cluster=c("NB","B")),font.main=c(16,"plain"),font.x=c(16,"bold"))
p6
p7<-facet(p6 + theme_bw(), facet.by = "Depth",
          short.panel.labs = FALSE,   # Allow long labels in panels
          panel.labs.background = list(fill = "lightgray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Cluster=c("Forest","Grassland")))

ggpar(p7,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Grassland Fire Legacy (yrs burnt)",font.legend = c(20,"bold"))

shapiro.test(soilanf$mean.ANF..ngN.fixed.g.dw.h.)
soilanf$loganf=log10(soilanf$mean.ANF..ngN.fixed.g.dw.h.)
shapiro.test(soilanf$loganf)#NORMAL

shapiro.test(forests$mean.ANF..ngN.fixed.g.dw.h.)
forests$log_anf=log10(forests$mean.ANF..ngN.fixed.g.dw.h.)
shapiro.test(forests$log_anf)#NORMAL

library(car)
names(forests)
soilfire<-lm(loganf~Vegetation*Fire.scar, data=soilanf)
summary(soilfire)
Anova(soilfire,type="II")

soilfire2<-aov(loganf~Vegetation*Fire.scar, data=soilanf)
summary(soilfire2)
Anova(soilfire,type="III")

soilfire2<-aov(loganf~Fire.presence*Site, data=Top)
summary(soilfire2)
Anova(soilfire2,type="III")
TukeyHSD(soilfire2,"Fire.presence",ordered=TRUE)

soilfire3<-aov(loganf~Fire.presence*Site, data=Bottom)
summary(soilfire3)
Anova(soilfire2,type="III")
TukeyHSD(soilfire3,"Fire.presence",ordered=TRUE)


soilfire3<-gam(loganf~Depth*Fire.presence*Fire.class, data=FORESTS)
summary(soilfire3)
Anova(soilfire3,type="III")

soilfire3.1<-lm(loganf~Depth*Fire.presence*Fire.class, data=FORESTS)
summary(soilfire3.1)
anova(soilfire3.1,type="III")

soilfire3<-gam(loganf~Depth*Fire.presence*Site, data=forests)
summary(soilfire3)
anova(soilfire3,type="II")

library(car)
str(FORESTS)
soilfire3a<-aov(loganf~Depth*Fire.presence*Fire.class, data=forests)
summary(soilfire3a)
anf_final<-aov(loganf~Depth*Fire.presence*Fire.class, data=FORESTS)
summary(anf_final)
Anova(soilfire3a,type="III")
TukeyHSD(anf_final,"Depth", ordered=TRUE)
summary(soilfire3a)
anova(soilfire3a,type="III")
TukeyHSD(soilfire3a, "Depth:Fire.scar", ordered=TRUE)

soilfire3b<-aov(log_anf~Site*Depth*Fire.scar, data=FORESTS)
summary(soilfire3b)
anova(soilfire3b,type="III")
TukeyHSD(soilfire3b, "Depth:Fire.scar", ordered=TRUE)

str(soilanf)
shapiro.test(mean.ANF..ngN.fixed.g.dw.h.)
soilanf$loganf=log10(soilanf$mean.ANF..ngN.fixed.g.dw.h.)
shapiro.test(soilanf$loganf)#NORMAL

names(soilanf)
pea16 <- lmer(loganf~ Site + Depth + Fire.presence+(1 | Fire.class), data = forests,
                    REML = FALSE)
summary(pea16)
lmsoil<-lm(loganf~Depth*Fire.presence*Fire.class, data=FORESTS)
summary(lmsoil)
shapiro.test(resid(lmsoil))#NORMAL

plot(lmsoil)
plot(soilanf$loganf,lmsoil$fitted.values,data=soilanf)
abline(lmsoil)
anova(lmsoil)
library(estimability)
library(lsmeans)
lit.rgs1 <- ref.grid(anf_final)
summary(lit.rgs1)
contrast(lit.rgs1, method = "pairwise")
plot(contrast(lit.rgs1, method="pairwise"))
shapiro.test(resid(lmsoil))

leastsquare =lsmeans(lmsoil,
                     pairwise ~ Depth, 
                     adjust="tukey") 

Tukey.HSD<-cld(leastsquare,
               alpha=0.1,
               Letters=letters,      ### Use lower-case letters for .group
               adjust="tukey") 
Tukey.HSD

options(max.print=999999)

levels(soilanf$Vegetation)
str(soilanf)
head(soilanf)
soilanf$Vegetation

grass <-soilanf[soilanf$Vegetation == "grassland",]
grass

impucas <-soilanf[soilanf$Vegetation == "forest",]
str(impucas)

library(mgcv)
library(nlme)
gamsoil2<-gam(loganf~Fire.scar*Depth, data=impucas)
summary(gamsoil2)#p=0.07 10-30 lower
par(mfrow=c(1,2))
plot.gam(gamsoil2, select=1, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of Fire Scar", xlabs="Fire Scar", shade=T, se=TRUE, all.terms=T,adj=0.5)
mtext("(a)", side=3,cex=1.2,adj=0, line=0.5)
mtext("p = 0.03", side=3,cex=1.0,adj=0.8, line=0.3)

plot.gam(gamsoil2, select=2, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of soil depth", xlabs="soil depth (cm)", shade=T, se=TRUE, all.terms=T,adj=0.5)
mtext("(b)", side=3,cex=1.2,adj=0, line=0.5)
mtext("p = 0.03", side=3,cex=1.0,adj=0.8, line=0.3)

gamsoil3<-gam(loganf~Depth*Fire.legacy*Fire.scar, data=impucas)
summary(gamsoil3)
anova(gamsoil3)
mtext("p = 0.51", side=3,cex=1.0,adj=0.65, line=-1.5)
mtext("p = 0.76", side=3,cex=1.0,adj=0.9, line=-2.5)

plot.gam(gamsoil3, select=1, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of Fire Scar", xlabs="Fire Scar", shade=T, se=TRUE, all.terms=T,adj=0.5)
mtext("(a)", side=3,cex=1.2,adj=0, line=0.5)
mtext("p = 0.03", side=3,cex=1.0,adj=0.8, line=0.3)

plot.gam(gamsoil2, select=2, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of soil depth", xlabs="soil depth (cm)", shade=T, se=TRUE, all.terms=T,adj=0.5)
mtext("(b)", side=3,cex=1.2,adj=0, line=0.5)
mtext("p = 0.03", side=3,cex=1.0,adj=0.8, line=0.3)

gamsoil4<-gam(loganf~Depth*Site*Fire.presence, data=forests)
summary(gamsoil4)
anova(gamsoil4)

gamsoil5<-gam(loganf~Depth*Fire.presence, data=forests)
summary(gamsoil5)
anova(gamsoil5)

par(mfrow=c(1,3))
plot.gam(gamsoil5, select=2, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of Fire Scar", xlabs="Fire presence (Burned vs Nonburned)", shade=T, se=TRUE, all.terms=T,adj=0.5)
mtext("(a)", side=3,cex=1.2,adj=0, line=0.5)
mtext("p = 0.03", side=3,cex=1.0,adj=0.8, line=0.3)

plot.gam(gamsoil4, select=2, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of soil depth", xlabs="Forest site", shade=T, se=TRUE, all.terms=T,adj=0.5)
mtext("(b)", side=3,cex=1.2,adj=0, line=0.5)
mtext("p = 0.03", side=3,cex=1.0,adj=0.8, line=0.3)

plot.gam(gamsoil4, select=3, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of soil depth", xlabs="Fire scar", shade=T, se=TRUE, all.terms=T,adj=0.5)
mtext("(c)", side=3,cex=1.2,adj=0, line=0.5)
mtext("p = 0.03", side=3,cex=1.0,adj=0.8, line=0.3)

plot.gam(gamsoil4, select=4, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of soil depth", xlabs="Fire scar", shade=T, se=TRUE, all.terms=T,adj=0.5)
mtext("(c)", side=3,cex=1.2,adj=0, line=0.5)

aovsoil4<-aov(loganf~Depth*Fire.presence, data=forests)
summary(aovsoil4)
anova(gamsoil4)

print(model.tables(aovsoil4,"means"),digits=3)

shapiro.test(resid(lmsoil2))
Anova(lmsoil2,type="III")
plot(TukeyHSD(lmsoil2, "Cluster", ordered=TRUE))#significant
TukeyHSD(aovsoil4, "Depth:Fire.scar", ordered=TRUE)#NO SIGNIFICANT DIFFERENCE
TukeyHSD(lmsoil2, "Depth", ordered=TRUE)#significant
TukeyHSD(lmsoil2, "Cluster:Depth", ordered=TRUE)#significant difference between NB topsoil and B 10-30
TukeyHSD(lmsoil2, "Cluster:Fire.legacy", ordered=TRUE)

lit.rgs1 <- ref.grid(aovsoil4)
summary(lit.rgs1)
contrast(lit.rgs1, method = "pairwise")
plot(contrast(lit.rgs1, method="pairwise"))
shapiro.test(resid(lmsoil))

leastsquare =lsmeans(aovsoil4,
                     pairwise ~ Fire.scar*Depth, 
                     adjust="tukey") 

Tukey.HSD<-cld(leastsquare,
               alpha=0.05,
               Letters=letters,      ### Use lower-case letters for .group
               adjust="tukey") 
Tukey.HSD
library(lsmeans)
leastsquare2 =lsmeans(lmsoil2,
                     pairwise ~ Cluster*Depth, 
                     adjust="tukey") 

Tukey.HSD2<-cld(leastsquare2,
               alpha=0.05,
               Letters=letters,      ### Use lower-case letters for .group
               adjust="tukey") 
Tukey.HSD2

#GRASSLANDS
gamgrass<-gam(loganf~Depth*Site*Fire.scar+Site*Depth, data=grass)
summary(gamgrass)
anova(gamgrass)
plot.gam(gamgrass, select=1, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of Depth", xlabs="Soil depth (cm)", shade=T, se=TRUE, all.terms=T,adj=0.5)
mtext("(a)", side=3,cex=1.2,adj=0, line=0.5)
mtext("p = 0.03", side=3,cex=1.0,adj=0.8, line=0.3)

plot.gam(gamgrass, select=2, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of Site", xlabs="Grasslands", shade=T, se=TRUE, all.terms=T,adj=0.5)
mtext("(b)", side=3,cex=1.2,adj=0, line=0.5)
mtext("p = 0.03", side=3,cex=1.0,adj=0.8, line=0.3)

plot.gam(gamgrass, select=3, cex.lab=1.5, cex.axis=1.5,rug=FALSE,ylabs="Partial effect of Fire scar", xlabs="Fire scar", shade=T, se=TRUE, all.terms=T,adj=0.5)
mtext("(c)", side=3,cex=1.2,adj=0, line=0.5)
mtext("p = 0.03", side=3,cex=1.0,adj=0.8, line=0.3)




gamgrass2<-gam(loganf~Depth+Site+Fire.scar, data=grass)
summary(gamgrass2)
anova(gamgrass2)

aovgrass<-aov(loganf~Depth*Site*Fire.scar, data=grass)
summary(aovgrass)
shapiro.test(resid(lmsoil2.1))
Anova(aovgrass,type="III")
plot(TukeyHSD(lmsoil2, "Cluster", ordered=TRUE))#significant
TukeyHSD(aovgrass, "Depth", ordered=TRUE)#NO SIGNIFICANT DIFFERENCE
TukeyHSD(aovgrass, "Depth:Site", ordered=TRUE)#significant
TukeyHSD(lmsoil2.1, "Cluster:Depth", ordered=TRUE)#significant difference between NB topsoil and B 10-30
TukeyHSD(lmsoil2, "Cluster:Fire.legacy", ordered=TRUE)

leastsquare2.1 =lsmeans(lmsoil2.1,
                      pairwise ~ Cluster, 
                      adjust="tukey") 

Tukey.HSD2.1<-cld(leastsquare2.1,
                alpha=0.05,
                Letters=letters,      ### Use lower-case letters for .group
                adjust="tukey") 
Tukey.HSD2.1

lmsoil3<-aov(loganf~Cluster*Depth*Fire.legacy+Site*Depth, data=grass)
summary(lmsoil3)
shapiro.test(resid(lmsoil3))
Anova(lmsoil3,type="III")
TukeyHSD(lmsoil3, "Cluster", ordered=TRUE)
TukeyHSD(lmsoil3, "Fire.legacy", ordered=TRUE)
TukeyHSD(lmsoil3, "Depth", ordered=TRUE)#only significant factor for grasslands

aovsoil<-aov(loganf~Vegetation*Cluster*Depth*Fire.legacy, data=soilanf)
summary(aovsoil)
TukeyHSD(aovsoil, "Vegetation:Cluster:Fire.legacy", ordered=TRUE)
TukeyHSD(aovsoil, "Vegetation:Cluster", ordered=TRUE)

#TRYING KRUSKAL TEST
library(stats)
kruskal.test(loganf~Fire.presence,data=forests)#Kruskal-Wallis chi-squared = 1.5731, df = 3, p-value = 0.6655
kruskal.test(loganf~Depth,data=forests)#Kruskal-Wallis chi-squared = 1.5731, df = 3, p-value = 0.6655
kruskal.test(loganf~Fire.class,data=forests)#Kruskal-Wallis chi-squared = 1.5731, df = 3, p-value = 0.6655
kruskal.test(loganf~Depth,data=soilanf)#Kruskal-Wallis chi-squared = 1.5731, df = 3, p-value = 0.6655


library(car)
library(MASS)
library(lme4)
library(nlme)
lmerpea <- lmer(loganf ~ Vegetation : Depth + (1 | Cluster), data = soilanf,
                    REML = FALSE) #best fit!
summary(lmerpea)

soilfire2<-gam(loganf~Vegetation*Depth*Cluster, data=soilanf)
summary(soilfire2)
anova(soilfire2)

par(mfrow = c(1,1))
plot.gam(soilfire2,all.terms=TRUE)
plot.gam(soilfire2,select=1,labcex=1.2,main="Vegetation",cex.axis=1.5,cex.lab=1.5, cex.main=1.6,se=TRUE, all.terms=T, xlab="",ylab="Partial effect for Vegetation",shade=TRUE,shade.col='gray90',rug=FALSE)
mtext("(a)", side=3,cex=1.0,adj=0, line=0.5)
mtext("p < 0.0001", side=3,cex=1.0,adj=0.8, line=-10)
plot.gam(gambynew3, scheme=2,select=2,labcex=1.2,main="eucalypt plantation",too.far=10,cex.axis=1.5,cex.lab=1.5,cex.main=1.6,xlab="Total N (log g/kg)",ylab="M3-Fe (log micro g/g)", shade=T, se=TRUE, all.terms=T,shade.col='gray90',rug=TRUE,contour.col="black",adj=0.5)
mtext("(b)", side=3,cex=1.0,adj=0, line=0.5)
mtext("p = 0.32", side=3,cex=1.0,adj=1, line=0.3)
plot.gam(gambynew3, scheme=2,select=3,labcex=1.2,main="pasture",too.far=10,cex.axis=1.6,cex.lab=1.5, cex.main=1.6,xlab="Total N (log g/kg)",ylab="M3-Fe (log micro g/g)", shade=T, se=TRUE, all.terms=T,shade.col='gray90',rug=TRUE,contour.col="black",adj=0.5)
mtext("(c)", side=3,cex=1.0,adj=0, line=0.5)
mtext(" p = 0.12", side=3,cex=1.0,adj=1, line=0.3)


soilfire3<-gam(loganf~Vegetation*Depth*Cluster+Fire.legacy, data=soilanf)
summary(soilfire3)
anova(soilfire3)
par(mfrow = c(1,3))
plot.gam(soilfire3,pages=1,all.terms=TRUE)

library(gamm4)
install.packages('itsadug')
install.packages('plotfunctions')
library(itsadug)
library(plotfunctions)
br <- gamm4(loganf~Vegetation+Fire.legacy+Depth,data=soilanf,random=~(1|Cluster))
plot(br$gam,pages=1)
summary(br$gam) ## summary of gam
summary(br$mer) ## underlying mixed model
anova(br$gam)
anova(br$mer)

soil3 <- gamm(loganf~Vegetation:Fire.legacy:Depth, random=~(1|Cluster), data=soilanf)
plot(soil3$gam,pages=1)

summary(soil3$lme) # details of underlying lme fit
summary(soil3$gam) # gam style summary of fitted model
anova(soil3$gam) 
gam.check(soil3$gam)

#soil figures for ms
levels(For$Site)=c("G1", "G2", "G3", "G5", "G6", "G7", "F2", "F2b", "F1", "F4", "F5")
pC1 <- ggboxplot(For, x = "Site", y = "TC_g_kg",
                color = "Fire.frequency",palette ="lancet",
                add = "jitter", width=1.0,ylab="Total C (g kg-1)",xlab="Site",short.panel.labs = FALSE,order = c("G3","F1","G1","F2","G2","F2b","G5","F4","G6","F5"),font.label = list(size = 14, face = "bold"),panel.labs=list(Depth=c("0-10","10-30")),font.main=c(16,"plain"),font.x=c(16,"bold"))
pC1
pC2<-facet(pC1 + theme_bw(), facet.by = "Fire.Presence",
          short.panel.labs = FALSE,   # Allow long labels in panels
          panel.labs.background = list(fill = "gray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Fire.Presence=c("B","UB")))

ggpar(pC2,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Site",font.legend = c(20,"bold"))

names(For)
pCN1 <- ggboxplot(For, x = "Site", y = "CN",
                 color = "Fire.frequency",palette ="lancet",
                 add = "jitter", width=1.0,ylab="Total C/N",xlab="Site",short.panel.labs = FALSE,order = c("G3","F1","G1","F2","G2","F2b","G5","F4","G6","F5"),font.label = list(size = 14, face = "bold"),panel.labs=list(Depth=c("0-10","10-30")),font.main=c(16,"plain"),font.x=c(16,"bold"))
pCN1
pCN2<-facet(pCN1 + theme_bw(), facet.by = "Fire.Presence",
           short.panel.labs = FALSE,   # Allow long labels in panels
           panel.labs.background = list(fill = "gray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Fire.Presence=c("B","UB")))

ggpar(pCN2,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Site",font.legend = c(20,"bold"))

pN1 <- ggboxplot(For, x = "Site", y = "TN_g_kg",
                 color = "Fire.frequency",palette ="lancet",
                 add = "jitter", width=1.0,ylab="Total N (g kg-1)",xlab="Site",short.panel.labs = FALSE,order = c("G3","F1","G1","F2","G2","F2b","G5","F4","G6","F5"),font.label = list(size = 14, face = "bold"),panel.labs=list(Depth=c("0-10","10-30")),font.main=c(16,"plain"),font.x=c(16,"bold"))
pN1
pN2<-facet(pN1 + theme_bw(), facet.by = "Fire.Presence",
           short.panel.labs = FALSE,   # Allow long labels in panels
           panel.labs.background = list(fill = "gray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Fire.Presence=c("B","UB")))

ggpar(pN2,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Site",font.legend = c(20,"bold"))

P1 <- ggboxplot(For, x = "Site", y = "M3P",
                 color = "Fire.frequency",palette ="lancet",
                 add = "jitter", width=1.0,ylab="Available P (microg g-1)",xlab="Site",short.panel.labs = FALSE,order = c("G3","F1","G1","F2","G2","F2b","G5","F4","G6","F5"),font.label = list(size = 14, face = "bold"),panel.labs=list(Depth=c("0-10","10-30")),font.main=c(16,"plain"),font.x=c(16,"bold"))
P1
P2<-facet(P1 + theme_bw(), facet.by = "Fire.Presence",
           short.panel.labs = FALSE,   # Allow long labels in panels
           panel.labs.background = list(fill = "gray", color = "gray"),panel.labs.font = list(color = "black", size = 22),panel.labs=list(Fire.Presence=c("B","UB")))

ggpar(P2,font.x=c(20,"bold"),font.y=c(20,"bold"),legend="top",font.xtickslab = 18,font.ytickslab = 18,xlab="Site",font.legend = c(20,"bold"))

str(For)
Top <-For[For$Depth == "0-10",]
Bot<-For[For$Depth == "10-30",]

tc<-lm(logtc~Fire.Presence,data=Bot)
summary(tc)
shapiro.test(TC_g_kg)
Bot$logtc=log10(Bot$TC_g_kg)
leastsquare_tc =lsmeans(tc,
                         pairwise ~ Fire.Presence, 
                         adjust="tukey") 

Tukey.HSD_tc<-cld(leastsquare_tc,
                   alpha=0.1,
                   Letters=letters,      ### Use lower-case letters for .group
                   adjust="tukey") 
Tukey.HSD_tc
kruskal.test(TC_g_kg~Fire.Presence,data=Top)

tn<-lm(logtn~Fire.Presence,data=Top)
summary(tn)
shapiro.test(Top$logtn)
Top$logtn=log10(Top$TN_g_kg)
leastsquare_tn =lsmeans(tn,
                        pairwise ~ Fire.Presence, 
                        adjust="tukey") 

Tukey.HSD_tn<-cld(leastsquare_tn,
                  alpha=0.1,
                  Letters=letters,      ### Use lower-case letters for .group
                  adjust="tukey") 
Tukey.HSD_tn
kruskal.test(TC_g_kg~Fire.Presence,data=Top)

Tn<-lm(logTN~Fire.Presence*Depth,data=For)
summary(Tn)
shapiro.test(Top$logtn)
For$logTN=log10(For$TN_g_kg)
leastsquare_Tn =lsmeans(Tn,
                        pairwise ~ Fire.Presence*Depth, 
                        adjust="tukey") 

Tukey.HSD_Tn<-cld(leastsquare_Tn,
                  alpha=0.1,
                  Letters=letters,      ### Use lower-case letters for .group
                  adjust="tukey") 
Tukey.HSD_Tn
kruskal.test(TC_g_kg~Fire.Presence,data=Top)

str(Top)
cn<-lm(CN~Fire.Presence,data=Top)
summary(cn)
shapiro.test(Top$logtn)
Top$logtn=log10(Top$TN_g_kg)
leastsquare_cn =lsmeans(cn,
                        pairwise ~ Fire.Presence, 
                        adjust="tukey") 

Tukey.HSD_cn<-cld(leastsquare_cn,
                  alpha=0.1,
                  Letters=letters,      ### Use lower-case letters for .group
                  adjust="tukey") 
Tukey.HSD_cn
kruskal.test(TC_g_kg~Fire.Presence,data=Top)

cn1<-lm(CN~Fire.Presence,data=Bot)
summary(cn1)
shapiro.test(Top$logtn)
Top$logtn=log10(Top$TN_g_kg)
leastsquare_cn1 =lsmeans(cn1,
                        pairwise ~ Fire.Presence, 
                        adjust="tukey") 

Tukey.HSD_cn1<-cld(leastsquare_cn1,
                  alpha=0.1,
                  Letters=letters,      ### Use lower-case letters for .group
                  adjust="tukey") 
Tukey.HSD_cn1
kruskal.test(TC_g_kg~Fire.Presence,data=Top)

Cn1<-lm(CN~Fire.Presence*Depth,data=For)
summary(Cn1)
shapiro.test(Top$logtn)
Top$logtn=log10(Top$TN_g_kg)
leastsquare_Cn1 =lsmeans(Cn1,
                         pairwise ~ Fire.Presence*Depth, 
                         adjust="tukey") 

Tukey.HSD_Cn1<-cld(leastsquare_Cn1,
                   alpha=0.1,
                   Letters=letters,      ### Use lower-case letters for .group
                   adjust="tukey") 
Tukey.HSD_Cn1
kruskal.test(TC_g_kg~Fire.Presence,data=Top)

P<-lm(logP~Fire.Presence*Depth,data=For)
summary(P)
shapiro.test(Top$logtn)
shapiro.test(residuals(P))
For$logP=log10(For$M3P)
leastsquare_P =lsmeans(P,
                         pairwise ~ Fire.Presence*Depth, 
                         adjust="tukey") 

Tukey.HSD_P<-cld(leastsquare_Cn1,
                   alpha=0.1,
                   Letters=letters,      ### Use lower-case letters for .group
                   adjust="tukey") 
Tukey.HSD_P
kruskal.test(TC_g_kg~Fire.Presence,data=Top)
