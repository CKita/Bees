################################################################################
#### Ecological Synthesis Lab (SintECO): https://marcomellolab.wordpress.com

#### Authors: Cristina A. Kita, Laura C. Leal & Marco A. R. Mello

#### See README for further info: https://github.com/CKita/Bees#readme
################################################################################


################################################################################
############################ LETHAL EFFECT ##################################### 
################################################################################


#Let's get ready for running the code. 

#Set the working directory to the source of this script file.  
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#Delete all previous objects.
rm(list= ls())

#Load or install the required packages.
if(!require(metafor)){
  install.packages("metafor")
  library(metafor)
}

if(!require(scales)){
  install.packages("scales")
  library(scales)
}

if(!require(ape)){
  install.packages("ape")
  library(ape)
}

if(!require(rotl)){
  install.packages("rotl")
  library(rotl)
}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(devtools)){
  install.packages("devtools")
  library(devtools)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(patchwork)){
  install.packages("patchwork")
  library(patchwork)
}

if(!require(R.rsp)){
  install.packages("R.rsp")
  library(R.rsp)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(emmeans)){
  install.packages("emmeans")
  library(emmeans)
}  

if(!require(orchaRd)){
  devtools::install_github("daniel1noble/orchaRd", force = TRUE)
  library(orchaRd)
}

if(!require(minqa)){
  install.packages("minqa")
  library(minqa)
}

if(!require(dfoptim)){
  install.packages("dfoptim")
  library(dfoptim)
}

############################ EFFECT SIZES  #####################################


#Now, let's import and check the data.
dat <- read.csv("../Data/lethal.csv", h= T, dec =".", sep = ";")
class(dat)
str(dat)
head(dat)
tail(dat)

#To calculate the effect sizes, we are going to use the "escalc" function.
#To calculate the bee survival probability, we are going to use odds ratio
#("OR") as a metric of effect size.
effects_t <- escalc("OR", dat$ai,dat$bi, dat$ci, dat$di, dat$n1i, dat$n2i)
head(effects_t)

#Now, let's bind the effect sizes with the raw data by columns.
lethal_effects<- cbind(dat, effects_t)
head(lethal_effects)

#Export the complete data set.
write.csv(lethal_effects, "../Data/lethal_effects.csv", row.names = F) 


###################### MODEL - MEAN LETHAL EFFECT SIZE #########################


#We are going to build a meta-analytic model to calculate the mean effect size.
#Let's use the complete data set to build our model.  
dados <- read.table("../Data/lethal_effects.csv", h=T, dec=".", sep = ",")
class(dados)
str(dados)
head(dados)
tail(dados)

#Change some columns to the class "factor" 

dados$exposure_type <- as.factor(dados$exposure_type)
dados$bee_stage <- as.factor(dados$bee_stage)


#Considering that bee survival rate is correlated with life history, we are
# going to build a phylogenetic covariance matrix using the Interactive Tree
# of Life online tree generator database. 
#Let's search for our bee species in the Tree of Life.
spp <- tnrs_match_names(unique(dados$bee_species), context_name = "Animals") 
spp

#Create a subtree with our bee species.
my_tree = tol_induced_subtree(ott_ids=spp$ott_id) 
class(my_tree)
str(my_tree)
my_tree

#Now calculate the phylogenetic distances and build the covariance matrix. 
otl_tips=strip_ott_ids(my_tree$tip.label, remove_underscores=TRUE)
taxon_map=structure(spp$search_string, names=spp$unique_name)
my_tree$tip.label=taxon_map[otl_tips]
my_tree.ult = compute.brlen(my_tree, method = "Grafen")
plot(my_tree.ult,no.margin=T) #let's see
cov.matrix = vcv(my_tree.ult,corr=T)
cov.matrix[,0]

#Now we'll build a meta-analytic mixed-effects model.
#We'll use the function rma.mv from the metafor package because of the 
#non-independence of our data. 
lethal <- rma.mv(yi, vi, random = list(   ~1|id_code,
                                          ~1|bee_species,
                                          ~1|pesticide_applied,
                                          ~1|hours_after_exposure,
                                          ~1|exposure_type,
                                          ~1|bee_stage),
                   method="REML",  #"REML" = multi-level 
            R = list(bee_species = cov.matrix),digits = 3, data = dados, 
            control=list(optimizer="BFGS"))                   

summary(lethal)

#Let's plot the result. 
tiff("../Figures/lethal.tiff", units="in", width=5.5,
     height=6, res=1200, compression = 'lzw')

orchaRd::orchard_plot(lethal, data= dados,
                      group = "id_code", xlab = "Odds ratio", branch.size = 1.4, 
                      trunk.size = 8, k.pos = "none") +
        labs(x = "Lethal effect") +
        scale_fill_manual(values="slateblue1") +
        scale_colour_manual(values="slateblue1")+
        theme_classic() + 
        theme(axis.text = element_text(size = 14, colour = "black"),
              axis.title = element_text(size = 16),
              axis.text.y = element_blank(),
              legend.position = "top") 
dev.off()


#We can see that some effect sizes are outliers compared to the mean effect 
#size. So, let's do a sensibility test to analyse the influence of these
#outliers. In our sensibility test, let's evaluate the standardized residuals.   
#Calculate the standardized residuals.
rs = rstandard(lethal) 
plot(rs$resid, ylim = c(-8.0,8), xlim =c(-8,50))

#If our residuals are > -3 or <3, it means that something extremely unusual is
#happening. 
abline(h = -3)  #below this line
abline(h = 3)   ##above this line
#28 points. In other words, 28 outliers

#let's identify the outliers.
text(rs$resid, labels = dados$id_code, cex= 1, pos = 2)

#Build a data frame to further inspect the outliers.
only_id_code <- dados$id_code
only_id_code <- as.data.frame(only_id_code)

rs <-as.data.frame(rs$resid)

residuals <- cbind(only_id_code, rs)
residuals

#Let's run a new model without them.
let_sensi<- read.csv("../Data/let_sensi_out.csv",
                       h= T, dec =".", sep = ";") #data frame without outliers 
head(let_sensi)
str(let_sensi)

#Build a covariance matrix using only the bee species of our data set
#without outliers.
spp <- tnrs_match_names(unique(let_sensi$bee_species),
                        context_name = "Animals") 
my_tree = tol_induced_subtree(ott_ids=spp$ott_id)
otl_tips=strip_ott_ids(my_tree$tip.label, remove_underscores=TRUE)
taxon_map=structure(spp$search_string, names=spp$unique_name)
my_tree$tip.label=taxon_map[otl_tips]
my_tree.ult = compute.brlen(my_tree, method = "Grafen")
plot(my_tree.ult,no.margin=T) 
cov.matrix = vcv(my_tree.ult,corr=T)
cov.matrix[,0]

#Run the new model without the outliers.
model.let.sensi.out <- rma.mv(yi, vi, 
                                random = list(         ~1|id_code,
                                                       ~1|bee_species,
                                                       ~1|pesticide_applied,
                                                       ~1|hours_after_exposure,
                                                       ~1|exposure_type,
                                                       ~1|bee_stage),
                                method="REML",  # "REML" = multi-level 
                                R = list(bee_species = cov.matrix),
                                digits = 3, 
                                control=list(optimizer="mads"),
                                data = let_sensi)

summary(model.let.sensi.out)

tiff("../Figures/lethal_sensibility_test.tiff", units="in", width=5.5,
     height=6, res=1200, compression = 'lzw')

orchaRd::orchard_plot(model.let.sensi.out, data= let_sensi,
                      group = "id_code", xlab = "Odds ratio", branch.size = 1.4, 
                      trunk.size = 8, k.pos = "none") +
        labs(x = "Lethal effect") + 
        scale_fill_manual(values="slateblue1") +
        scale_colour_manual(values="slateblue1")+
        theme_classic() + 
        theme(axis.text = element_text(size = 14, colour = "black"),
              axis.title = element_text(size = 16),
              axis.text.y = element_blank(),
              legend.position = "top") 

dev.off()

#Result: mean effect size is negative and different from zero. In other words,
#pesticide effects are strong enough to be detected even when sample size
#is reduced.  


############################ HETEROGENEITY #####################################

#Now, we'll calculate the heterogeneity of the model and the heterogeneity of
#each random variable, using the I² statistic with a 95% confidence interval (CI).
dados$wi <- 1/sqrt(dados$vi)
#precision = 1 / standard error of effect size (Equation 20; Nakagawa & Santos 2012)
s2m.0 <- sum(dados$wi*(length(dados$wi)-1))/(sum(dados$wi)^2-sum(dados$wi^2)) # Equation 22

#Calculate the total heterogeneity. In other words, the heterogeneity that is
#explained by our model including predictor and response variables.
I2.lethal <- ((lethal$sigma2[1] + lethal$sigma2[2] +
               lethal$sigma2[3] + lethal$sigma2[4] +
                       lethal$sigma2[5] + lethal$sigma2[6])/  
                 (lethal$sigma2[1] + lethal$sigma2[2] +
                          lethal$sigma2[3] + lethal$sigma2[4] +
                          lethal$sigma2[5] + lethal$sigma2[6] + s2m.0)) * 100 

#The values of heterogeneity 25%, 50%, and 75% are considered small, medium,
#and high, respectively, as suggested by Higgins (2003).
I2.lethal #mean 

##95% CI for I2.total:
I2.lethal - qchisq(.95, df=1)/2; I2.lethal + qchisq(.95, df=1)/2


#Calculating I² for each random factor 
#--- study ID ---
I2.study.bee <- ((lethal$sigma2[1])/(
        lethal$sigma2[1]+ lethal$sigma2[2] +  lethal$sigma2[3]  +  
                lethal$sigma2[4] +
                lethal$sigma2[5] + lethal$sigma2[6] +
                s2m.0)) * 100

I2.study.bee

##95% CI for I2.study.bee:
I2.study.bee - qchisq(.95, df=1)/2; I2.study.bee + qchisq(.95, df=1)/2

#--- bee_species ---
I2.bee <- ((lethal$sigma2[2])/(
        lethal$sigma2[1]+ lethal$sigma2[2] +  lethal$sigma2[3] + 
                lethal$sigma2[4] + lethal$sigma2[5] + lethal$sigma2[6] +
                s2m.0)) * 100

I2.bee

##and 95% CI for I2.bee:
I2.bee - qchisq(.95, df=1)/2; I2.bee + qchisq(.95, df=1)/2

#--- pesticide applied ---
I2.agro<- ((lethal$sigma2[3])/(
        lethal$sigma2[1]+lethal$sigma2[2] +  lethal$sigma2[3] + 
                lethal$sigma2[4] + lethal$sigma2[5] + lethal$sigma2[6] +
                s2m.0)) * 100

I2.agro

##95% CI for I2.agro:
I2.agro - qchisq(.95, df=1)/2; I2.agro + qchisq(.95, df=1)/2

#----hours ---
I2.hours<- ((lethal$sigma2[4])/(
        lethal$sigma2[1]+lethal$sigma2[2] + lethal$sigma2[3] 
        + lethal$sigma2[4] + lethal$sigma2[5] + lethal$sigma2[6] +
                s2m.0)) * 100

I2.hours

## 95% CI for I2.hours:
I2.hours - qchisq(.95, df=1)/2; I2.hours + qchisq(.95, df=1)/2

#---exposure type ---
I2.exp<- ((lethal$sigma2[5])/(
        lethal$sigma2[1]+lethal$sigma2[2] +  lethal$sigma2[3]
        + lethal$sigma2[4] + lethal$sigma2[5] + lethal$sigma2[6] +
                s2m.0)) * 100

I2.exp

## 95% CI for I2.exp:
I2.exp - qchisq(.95, df=1)/2; I2.exp + qchisq(.95, df=1)/2

#---stage of bee---

I2.stage<- ((lethal$sigma2[6])/(
  lethal$sigma2[1]+lethal$sigma2[2] +  lethal$sigma2[3]
  + lethal$sigma2[4] + lethal$sigma2[5] + lethal$sigma2[6] +
    s2m.0)) * 100

I2.stage

## 95% CI for I2.exp:
I2.stage - qchisq(.95, df=1)/2; I2.stage + qchisq(.95, df=1)/2


#Confidence intervals for sigma2.
confint(lethal)


############################ PUBLICATION BIAS ##################################


#Publication bias in each model using an adapted version of Egger’s regression
#(Nakagawa & Santos, 2012). A publication bias is pointed out when the
#intercept of the regression significantly deviates from zero
eggsm3 = lm(residuals(lethal)~sqrt(dados$vi)) 
summary(eggsm3) 
confint.lm(eggsm3, level = 0.95)

model.residuals <- residuals(lethal) #model residuals
effect.sizes.vi<- sqrt(dados$vi) #variances of each effect size (vi)

bias <- cbind(model.residuals, effect.sizes.vi)
bias <- as.data.frame(bias)

tiff("../Figures/publication_bias.tiff", units="in", width=5,
     height=6, res=1200, compression = 'lzw')

ggplot(bias, aes(effect.sizes.vi, model.residuals)) + 
        geom_point(size = 3, alpha = 0.3) +
        geom_hline(yintercept= 0.0,linetype=2) +
        labs(x="Standard Error", y="Residual Value") +
        theme_classic() + 
        theme(axis.text = element_text(size = 14, colour = "black"), 
              axis.title = element_text(size = 16)) 

dev.off()

############################ END ###############################################