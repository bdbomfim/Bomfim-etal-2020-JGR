#AGU 2016 CODE

setwd('/Users/bbomf/Dropbox/R/')

bnf1516<-read.csv("../R/BNF WET DRY SEASONS SOIL LITTER.csv", header=TRUE)
attach(bnf1516)
str(bnf1516)

bnfavg1516<-read.csv("../R/ALL AVERAGE DATA_SOIL_LITTER_BOTH_SEASONS.csv", header=TRUE)
attach(bnfavg1516)
str(bnfavg1516)

bnfsoil<-read.csv("../R/BNF FARM SOIL season.csv")
attach(bnfsoil)
str(bnfsoil)

gamsoil<-read.csv("../R/gam soil full dataset AGU.csv", header=TRUE)
attach(gamsoil)
str(gamsoil)

#gam for wet season
gamsoilwet<-read.csv("../R/gam soil wet season.csv", header=TRUE)
attach(gamsoilwet)
str(gamsoilwet)

litterpred<-read.csv("../R/LITTER SEASON AGU.csv", header=TRUE)
attach(litterpred)
str(litterpred)
shapiro.test(TC..mol.kg.)
shapiro.test(TN..mol.kg.)
#running lm
shapiro.test(Bnf)
lcn<-lm(Ln.Bnf~TC..mol.kg.+TN..mol.kg., data=litterpred)
summary(lcn)
lcn2<-lm(Ln.Bnf~(TC..mol.kg.)^2, data=litterpred)
summary(lcn2)
plot(TC..mol.kg.,Ln.Bnf)
abline(lcn2)
str(litterpred)

my_grob14 = grobTree(textGrob(expression('p = 0.009'), x=0.8,  y=0.90, hjust=0,gp=gpar(col="Black", fontsize=20, fontface="italic")))
my_grob15 = grobTree(textGrob(expression('R' ^2* '= 0.2'), x=0.8,  y=0.95, hjust=0,gp=gpar(col="Black", fontsize=20, fontface="italic")))
ggplot(litterpred, aes(y=Ln.Bnf, x=TC..mol.kg., color = factor(c)))+geom_point(size = 6)+geom_smooth(method="lm", color = "black")+ annotation_custom(my_grob14)+ annotation_custom(my_grob15)+ labs(y = expression('SOIL BNF (µgN fixed gDW-1 h-1)'), x = expression('SOIL TOTAL CARBON (g kg-1)', size = 24))+theme(legend.title = element_text())+scale_color_discrete(name="Site")+guides(colour = guide_legend(override.aes = list(size=15)))+theme(legend.text=element_text(size = 24))+theme(axis.title.x=element_text(size = 24))+theme(axis.title.y=element_text(size = 24))+theme(axis.text.x=element_text(size = 24))+theme(axis.text.y=element_text(size = 24))+scale_color_brewer(palette = "Spectral")
lcn3<-lm(Bnf~(TC..mol.kg.)^2, data=litterpred)
summary(lcn3)
plot(TC..mol.kg.,Bnf)
abline(lcn2)

#normality test
shapiro.test(Ln.Bnf)#W=0.9, the best possible
shapiro.test(bnf)
shapiro.test(log.bnf)
shapiro.test(exp.bnf)
shapiro.test(lnsqr.bnf)
shapiro.test(Pow.Bnf)#best so far

#transforming bnf
library(jtrans)
bnftrans<-jtrans(bnf1516$Bnf, test = "shapiro.test", exclude_original = TRUE, z_lim = c(0.25,
                                                                    1.25), z_length = 101)
bnftrans
shapiro.test(bnftrans$transformed)#normal!!! w=0.97
plot(density(bnf1516$Bnf))
plot(density(bnftrans$transformed))
qqnorm(bnftrans$transformed)
qqline(bnftrans$transformed)


bnf1516$Bnf_trans<-(bnf1516$Bnf)^(-0.25)
bnf1516$Bnf_trans 
bnf1516$Ln.Bnf
bnflitseason$bnf_t<-(log10(bnflitseason$X.gN.fixed.g.DW.h))
shapiro.test(bnflitseason$bnf_t) #normal

# ----- Finding the exponent for a power transformation ----

means <- aggregate(bnf1516$Ln.Bnf, list(bnf1516$c), mean)
vars <- aggregate(bnf1516$Ln.Bnf, list(bnf1516$c), var)
logmeans <- log10(means$x)
logvars <- log10(vars$x)
power.mod<-lm(logvars ~ logmeans)
summary(power.mod) #slope is 1.8024 -> power = 1-(1.8024/2) = 0.0988

#Full Factorial ANOVA - all factorial combinations
library(car)
library(estimability)
library(lsmeans)
library(Matrix)
library(lme4)

str(bnf1516)
shapiro.test(bnf1516$Ln.Bnf)
aovfull<-lm(Ln.Bnf~c*s*y*d, data=bnf1516)
summary(aovfull)#significant, r2=0.6489, p < 0.00001
lsfull <- ref.grid(aovfull)
summary(lsfull)
contrast(lsfull, method = "pairwise")
plot(contrast(lsfull, method="pairwise"))
plot(aovfull$residuals)
aovfull$aic
shapiro.test(resid(aovfull)) #W = 0.90 #I will consider 90% confidence level
aovfull$fitted.values
plot(Ln.Bnf, aovfull$fitted.values)
abline(aovfull)

#for litter all seasons
aovlit<-lm(bnflitseason$bnf_t~Site*Hillslope*Season, data=bnflitseason)
summary(aovlit)
aovlit$fitted.values
plot( aovlit$fitted.values, bnflitseason$bnf_t)
abline(aovlit)
AOVlit<-aov(bnflitseason$bnf_t~Site*Hillslope*Season, data=bnflitseason)
summary(AOVlit)
TukeyHSD(AOVlit, "Site:Hillslope", ordered=TRUE)
TukeyHSD(AOVlit, "Site", ordered=TRUE)
TukeyHSD(AOVlit, "Site:Hillslope", ordered=TRUE)
par(cex=1)
plot(Tukey_avglit)
plot(tuksea)
summary(tuksea)

#for soil all seasons
aovsoil<-lm(Ln.Bnf~c*s*y*d, data=gamsoil)
summary(aovsoil)#significant, r2=0.33, p < 0.00001
shapiro.test(resid(aovfull)) #W = 0.90 #I will consider 90% confidence level
aovfull$fitted.values
plot(Ln.Bnf, aovsoil$fitted.values)
abline(aovfull)
AOVsoil<-aov(Ln.Bnf~c*s*y*d, data=gamsoil)
summary(AOVsoil)
TukeyHSD(AOVsoil, "c:y", ordered=TRUE)
TukeyHSD(AOVlit, "Site", ordered=TRUE)
TukeyHSD(AOVlit, "Site:Hillslope", ordered=TRUE)

library(car)
library(ggplot2)
library(gridExtra)
library(multcompView)

Tukey_avglit <- TukeyHSD(AOVlit, "Site", ordered=TRUE)
plot(Tukey_avglit , las=1 , col="brown" )

generate_label_df <- function(Tukey_avglit, variable){
  
  Tukey.levels <- Tukey_avglit[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  Tukey.labels$Legacy_Rep=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$Legacy_Rep) , ]
  return(Tukey.labels)}

LABELS<-generate_label_df(Tukey_avglit , "Site")

my_colors=c( rgb(143,199,74,maxColorValue = 255),rgb(242,104,34,maxColorValue = 255), rgb(111,145,202,maxColorValue = 255),rgb(254,188,18,maxColorValue = 255) , rgb(74,132,54,maxColorValue = 255),rgb(236,33,39,maxColorValue = 255),rgb(165,103,40,maxColorValue = 255))

a=boxplot(bnflitseason$bnf_t ~ bnflitseason$Site , ylim=c(min(bnflitseason$bnf_t) , 1.1*max(bnflitseason$bnf_t)) , col=my_colors[as.numeric(LABELS[,1])] , ylab="µg N fixed g dw-1 h-1" , xlab="LAND USE", main="BNF")

over=0.1*max( a$stats[nrow(a$stats),] )

text( c(1:nlevels(bnflitseason$Site)) , a$stats[nrow(a$stats),]+over , LABELS[,1]  , col=my_colors[as.numeric(LABELS[,1])] )


aovAGU<-aov(Ln.Bnf~c*s*y*d, data=bnf1516)
summary(aovAGU)
coefficients(aovAGU)
aovAGU$fitted.values
plot(Ln.Bnf, aovAGU$fitted.values)
TukeyHSD(aovAGU, "c", ordered=TRUE)
TukeyHSD(aovAGU, "s", ordered=TRUE)
TukeyHSD(aovAGU, "y", ordered=TRUE)
TukeyHSD(aovAGU, "d", ordered=TRUE)

#for soil only
bnfsoil$lnbnf=log10(bnfsoil$X.gN.fixed.g.DW.h)
shapiro.test(bnfsoil$lnbnf)

aovfull2<-lm(bnftrans$transformed~c*s*y*d, data=bnf1516)
summary(aovfull2)
shapiro.test(resid(aovfull2))
plot(resid(aovfull2))
plot(aovfull2)

aovfull3<-lm(Bnf_trans~c*s*y*d, data=bnf1516)
summary(aovfull3)
shapiro.test(resid(aovfull3))
plot(resid(aovfull3))
plot(aovfull3)
leveneTest(ln.bnf ~ c*s*y*d, data = bnf1516)

#Linear Mixed Effect Model lmer using terms c:s as random
library(Matrix)
library(nlme)
library(lme4)
obj<-lmer(bnf~c*y*d + s*y + d:s + d:s:y, random = ~ c:s + y:c:s + d:c:s + y:d:c:s, data=bnf1516) 
anova(obj)
obj2<-lmer(Ln.Bnf~c*y*d + s*y + d:s + d:s:y +(1|c:s) +(1|y:c:s) + (1|d:c:s) + (1|y:d:c:s), data=bnf1516) 
summary(obj2)
print(obj2, correlation=TRUE)
shapiro.test(resid(obj2))#W=0.90
anova(obj2)
obj2$aic
anova(obj2,aovfull)#similar, but aovfull has higher AIC
anova(obj2, aovfull)
shapiro.test(resid(object2))

obj3<-lmer(Ln.Bnf~c*y*d + s*y + d*s + d*s*y +(1|c/s) +(1|y/c/s) + (1|d/c/s), data=bnf1516) 
summary(obj3)
anova(obj2, aovfull)
shapiro.test(resid(obj3))

#generalized additive model
shapiro.test(Av.P..ægPO4.P..g.dw.)#not normal
shapiro.test(Av..Fe.æg.g.dw)#not normal
shapiro.test(TC..g.kg.)#not normal
shapiro.test(TN..g.kg.)#not normal
shapiro.test(CN..molar.)#NORMAL
shapiro.test(PMN.æg.NH4.N.gdw.)#not normal
shapiro.test(pH..H2O.)#not normal
shapiro.test(pH..KCl.)#not normal
shapiro.test(BD..g.cm3.)#NORMAL
shapiro.test(Ln.Bnf)#not normal
shapiro.test(moisture....)
shapiro.test(TP.æg.g.dw)#not normal
shapiro.test(TP..mol.kg.)#not normal

#TRANSFORMATIONS
gamsoil$bnf_trans<-(1-(gamsoil$Ln.Bnf)^2)/10
gamsoil$bnf_trans
shapiro.test(gamsoil$bnf_trans)#NORMAL

gamsoil$moi_trans<-((gamsoil$moisture)^(2))
gamsoil$moi_trans
shapiro.test(gamsoil$moi_trans)#NORMAL

gamsoil$tp_trans<-(log10(gamsoil$TP.æg.g.dw))
gamsoil$tp_trans
shapiro.test(gamsoil$tp_trans)#NORMAL

gamsoil$tpmol_trans<-((gamsoil$TP..mol.kg.)^2)*50
gamsoil$tpmol_trans
shapiro.test(gamsoil$tpmol_trans)#NOT NORMAL

gamsoil$Av.P_trans<-log10(gamsoil$Av.P...gPO4.P..g.dw.)
gamsoil$Av.P_trans
shapiro.test(gamsoil$Av.P_trans)#NORMAL

gamsoil$Av.Fe_trans<-(gamsoil$Av..Fe..g.g.dw)^(-0.5)
gamsoil$Av.Fe_trans
shapiro.test(gamsoil$Av.Fe_trans)#NORMAL

gamsoil$TC_trans<-log10(gamsoil$TC..g.kg.)
gamsoil$TC_trans
shapiro.test(gamsoil$TC_trans)#NORMAL

gamsoil$TN_trans<-log10(gamsoil$TN..g.kg.)^(0.5)
gamsoil$TN_trans
shapiro.test(gamsoil$TN_trans)#NORMAL

gamsoil$PMN_trans<-log10(gamsoil$PMN)^(0.5)
gamsoil$PMN_trans
shapiro.test(gamsoil$PMN_trans)#NORMAL

gamsoil$PH1_trans<-log10(gamsoil$pH..H2O.)^(-5)
gamsoil$PH1_trans
shapiro.test(gamsoil$PH1_trans)#NORMAL

gamsoil$PH2_trans<-log10(gamsoil$pH..KCl.)^(-5)
gamsoil$PH2_trans
shapiro.test(gamsoil$PH2_trans)#NORMAL


library(splines)
library(foreach)

library(nlme)

GAM_status <- function () {
  if (all(c("gam", "mgcv") %in% .packages())) print("Not OK")
  else print("OK")
}
GAM_status
gamobject<-gam(Ln.Bnf~s(TC_trans)+s(tp_trans)+s(moi_trans)+s(TN_trans)+s(PMN_trans)+s(Av.P_trans)+s(Av.Fe_trans)+s(PH1_trans)+s(PH2_trans), data=gamsoil) 
summary(gamobject)
gamobject$effects
gamobject$rank
gamobject$coefficients
gamobject$weights
gamobject$method #glm.fit
gamobject$family
gamobject$aic #AIC is 297.1
gamobject$smooth.frame
gamobject$fitted.values
gamobject$y
par(mfrow = c(1,1))
plot(gamobject$fitted.values)
plot(gamobject, residuals=F, se=TRUE,pch=19, cex=0.75, scheme=1, shade=T,shade.col='gray90')
plot(Bnf, gamobject$fitted.values)
p<-exp(gamobject$fitted.values)
plot(p)
str(p)
plot(p, Bnf)
plot(gamobject, se=TRUE)
par(mfrow = c(3,3))
plot(gamobject, shade=TRUE, se=TRUE)
predict(gamobject, type="terms", newdata=gamsoil)
gamobject$qr
plot(Ln.Bnf ~ Av.P_trans, data=gamsoil)

str(gamsoil)
gamobject2<-gam(Ln.Bnf~s(TC_trans)+s(CN)+s(tp_trans)+s(moi_trans)+s(TN_trans)+s(PMN_trans)+s(Av.P_trans)+s(Av.Fe_trans)+s(PH1_trans)+s(PH2_trans), data=gamsoil) 
summary(gamobject2)
gamobject2$aic #302.3
par(mfrow = c(3,3))
plot(gamobject2, shade=TRUE, se=TRUE)

gamobjectall<-gam(Ln.Bnf~s(TC_trans)+s(TN_trans)+s(moi_trans)+ s(CN..molar.)+s(PMN_trans)+s(Av.P_trans)+s(Av.Fe_trans)+s(PH1_trans)+s(PH2_trans), data=gamsoil) 
summary(gamobjectall)
gamobject$effects
gamobject$rank
gamobject$coefficients
gamobject$weights
gamobject$method #glm.fit
gamobject$family
gamobjectall$aic #AIC is 314.0239
gamobject$smooth.frame
gamobject$fitted.values
gamobject$y
par(mfrow = c(1,1))
plot(gamobject$fitted.values)
plot(gamobject, residuals=F, se=TRUE,pch=19, cex=0.75, scheme=1, shade=T,shade.col='gray90')
plot(Bnf, gamobject$fitted.values)
p<-exp(gamobject$fitted.values)
plot(p)
str(p)
plot(p, Bnf)
plot(gamobject, se=TRUE)
par(mfrow = c(3,3))
plot(gamobject, shade=TRUE, se=TRUE)
predict(gamobject, type="terms", newdata=gamsoil)
gamobject$qr
plot(Ln.Bnf ~ Av.P_trans, data=gamsoil)

gamobject3<-gam(Ln.Bnf~s(TC_trans)+s(TN_trans)+s(PMN_trans)+s(moisture....)+s(CN..molar.)+s(Av.P_trans)+s(Av.Fe_trans)+s(PH1_trans)+s(PH2_trans), data=gamsoil)
summary(gamobject3) #HIGHEST AIC
gamobject3$aic
gamob2<-gam(Bnf~s(TC..g.kg.)+s(TN..g.kg.)+s(CN..molar.) +s(PMN.æg.NH4.N.gdw.)+s(Av.P..ægPO4.P..g.dw.)+s(Av..Fe.æg.g.dw)+s(pH..H2O.)+s(pH..KCl.), family=binomial, data=gamsoil) 
summary(gamob2)
gamob2$aic #AIC is 74.06
par(mfrow=c(1,1))
plot(gamob2$residuals)
plot(gamob2$fitted.values)
plot(Bnf, gamob$fitted.values)
par(mfrow = c(3,3))
plot(gamobject3, se=TRUE)
plot(gamob2$fitted.values)
gamob2$fitted.values
gamob2$aic
par(mfrow = c(3,3))
plot(gamob, shade=TRUE, se=TRUE)
anova(gamobject,gamob, test='Chi')

gamobject4<-gam(Ln_Bnf ~ s(SOC), data=gamsoilwet)
gamsoilwet
summary(gamobject4) 
gamobject4$aic
trans <- function(TC_trans) + gaussian()$linkinv(x)
gamobject$deviance
plot(gamobject, se=TRUE)
gamobject$nl.chisq

plot(resid(gamobject))
shapiro.test(resid(gamobject))
plot(gamobject, ask=FALSE)
plot(gamobject,pages=1)
gam.check(gamobject)
str(gamsoil)

gamobjectC<-gam(Ln.Bnf~s(TC..g.kg.)+s(TN..g.kg.)+s(TP.æg.g.dw)+s(CN)+s(PMN)+s(Av.P...gPO4.P..g.dw.)+s(moisture)+s(Av..Fe..g.g.dw)+s(pH..H2O.)+s(pH..KCl.), data=gamsoil) 
summary(gamobjectC)
vif(gamobjectC)
gamobjectC$aic #AIC is 323.21
plot(resid(gamobjectC))
shapiro.test(resid(gamobjectC))
par(mfrow = c(3,3))
plot(gamobjectC, shade=TRUE, se=TRUE)
plot(gamobjectC)
gamobjectC$nl.chisq
anova(gamobject,gamobjectC)
gamsoil$pH..H2O.

gamobjectC2lo<-gam(Ln.Bnf~lo(TC..g.kg.)+lo(TP.æg.g.dw)+lo(CN)+lo(TN..g.kg.)+lo(PMN)+lo(Av.P...gPO4.P..g.dw.)+lo(moisture)+lo(Av..Fe..g.g.dw)+lo(pH..H2O.)+lo(pH..KCl.), data=gamsoil) 
summary(gamobjectC2lo)
par(mfrow = c(3,4))
plot(gamobjectC2lo, ask=FALSE)
gamobjectC2lo$aic #344.94
gamobjectC2lo$effects
gamobjectC2lo$var
effect<-as.data.frame(gamobjectC2lo$effects)
effect
gamobjectC2lo$fitted.values
plot(Ln.Bnf,gamobjectC2lo$fitted.values)
par(mfrow=c(1,1))
plot(resid(gamobjectC2lo))
p<-plot(resid(gamobjectC2lo)~fitted.values(gamobjectC2lo), col=gamsoil$c, labels=row.names(coverlabel))
p
     


gamobjectC4lo<-gam(Ln.Bnf~(TC..g.kg.)+lo(TP.æg.g.dw)+lo(CN)+(TN..g.kg.)+lo(PMN)+lo(Av.P...gPO4.P..g.dw.)+lo(moisture)+lo(Av..Fe..g.g.dw)+lo(pH..KCl.), data=gamsoil) 
summary(gamobjectC4lo)
par(mfrow = c(3,4))
plot(gamobjectC4lo, ask=FALSE)

gamobjectC3lo<-gam(Ln.Bnf~lo(TC..g.kg.)+lo(TN..g.kg.)+lo(CN..molar.)+lo(PMN.æg.NH4.N.gdw.)+lo(Av.P..ægPO4.P..g.dw.)+lo(moisture....)+lo(Av..Fe.æg.g.dw)+lo(pH..H2O.)+lo(pH..KCl.), data=gamsoil) 
summary(gamobjectC3lo)
par(mfrow = c(3,3))

plot(gamobjectC3lo)
gamobject$family

gamobjectC2loc<-gam(Ln.Bnf~(TC..g.kg.)+(TN..g.kg.)+lo(PMN.æg.NH4.N.gdw.)+lo(Av.P..ægPO4.P..g.dw.)+lo(moisture....)+lo(Av..Fe.æg.g.dw)+lo(pH..H2O.)+lo(pH..KCl.), data=gamsoil) 
summary(gamobjectC2loc)
par(mfrow = c(3,3))
plot(gamobjectC2loc)
gamobjectC2loc$aic
str(gamsoil)

gamobject2<-gam(Ln.Bnf~lo(TC_trans)+lo(TN_trans)+lo(PMN_trans)+lo(moisture....)+lo(Av.P_trans)+lo(Av.Fe_trans)+lo(PH1_trans)+lo(PH2_trans), data=gamsoil) 
summary(gamobject2)
par(mfrow=c(1,1))
plot(resid(gamobject2))
gamobject2$nl.df
par(mfrow = c(3,3))
plot(gamobject2)

anova(gamobject,gamobject2, gamobject3, gamobjectC, gamobjectall, test="Chi")
boxplot(gamobject$residuals,gamobject2$residuals, gamobject3$residuals,
        names=c("GAM s","GAM lo", "GAM 3"))
points(Ln.Bnf,fitted(gamobject),col=2)
points(Ln.Bnf,fitted(gamobject2),col=3)
gamobject$aic
gamobject2$aic
gamobject3$aic
gamobjectC$aic
gamobjectC2$aic
gamobjectall$aic
summary(gamobjectC2)

#comparing GAM to GLM
library(car)
glm1<-glm(Ln.Bnf~TC_trans+moi_trans+TN_trans, family = gaussian, data=gamsoil)
summary(glm1)
par(mfrow = c(2, 2))
plot(glm1)
shapiro.test(resid(glm1))
Anova(glm1, type=3)
glm1$method
glm1$formula
glm1$deviance
glm1$aic

glm3<-glm(Ln.Bnf~moi_trans, family = gaussian, data=gamsoil)
summary(glm3)
par(mfrow = c(2, 2))
plot(glm3)
shapiro.test(resid(glm3))
Anova(glm3, type=3)
glm3$aic
anova(glm1,glm3)
glm3$effects
sqPearson <- cor(Ln.Bnf, glm3$fitted.values) ^ 2
sqPearson

glm2<-glm(Ln.Bnf~TC_trans+TN_trans+moi_trans+PH1_trans+CN, family = gaussian, data=gamsoil)
summary(glm2)
par(mfrow = c(2, 2))
plot(glm3)
shapiro.test(resid(glm1))
Anova(glm2, type=3)
glm3$aic
anova(glm1,glm3, glm2)

#glm bnf wet season
str(gamsoilwet)
glmwet<-glm(Ln_Bnf~moisture+SOC, data=gamsoilwet)
summary(glmwet)
plot(Ln_Bnf, fitted(glmwet))
abline(glmwet)
plot(SOC, moisture)
plot(SOC,Ln_Bnf)
plot(moisture, Ln_Bnf)
plot(SOC, TP..g.g.dw)
plot(TN, TP..g.g.dw)
