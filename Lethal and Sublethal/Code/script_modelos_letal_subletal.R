################################################################################
#### Ecological Synthesis Lab (SintECO): https://marcomellolab.wordpress.com

#### Authors: Cristina A. Kita, Laura C. Leal & Marco A. R. Mello

#### See README for further info:
#### https://github.com/CKita/Bees#readme
################################################################################


################################################################################
###################### LETHAL AND SUBLETHAL EFFECTS ############################
################################################################################


#Let's get ready for running the code provided here. 

#Set the working directory that contains the lethal and sublethal data.  
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#Delete all previous objects.
rm(list= ls())

#Load or install the required packages
if(!require(metafor)){
  install.packages("metafor")
  library(metafor)
}

if(!require(orchaRd)){
  devtools::install_github("daniel1noble/orchaRd", force = TRUE)
  library(orchaRd)
}

if(!require(devtools)){
  install.packages("devtools")
  library(devtools)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(patchwork)){
  install.packages("patchwork")
  library(patchwork)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}


############## MODEL -  Overall lethal and sublethal pesticide effects ######### 


# First, let's see our data set
dados_completos <- read.csv("../Data/let_sublet.csv", h= T, dec =".", sep = ",")
class(dados_completos)
str(dados_completos)
head(dados_completos)
tail(dados_completos)

#Change some columns to the class "factor" 
dados_completos$nature_x <- as.factor(dados_completos$nature_x) 
dados_completos$study_type <- as.factor(dados_completos$study_type)
dados_completos$effect_type <- as.factor(dados_completos$effect_type)
dados_completos$pesticide_effect <- as.factor(dados_completos$pesticide_effect)
str(dados_completos)

#Summarize the data 
table(dados_completos$pesticide_effect)
table(dados_completos$study_type)


#Before starting to build the model to calculate the overall mean effect size, 
#let's remember how we calculate the effect sizes.  

#Remember that: 

#1
#To calculate the effect sizes, we used Hedges’ g as the metric of effect size.
#Therefore, our effect sizes result from standardized differences in the
# means of treatments.

#2
#We used the treatment group as a reference (with pesticide application).
#In other words, we subtracted from it the mean value of the control group
#(without pesticide application or organic farms). Therefore, negative values
#of Hedges’ g occur when the mean from the control group is higher than that
#of the treatment group. It indicates a negative effect of pesticide
#application on the bee community and its pollination service. 

#OK. Let's get started.  

#We are going to build a meta-analytic mixed-effects model.
#We are going to use the function rma.mv from the metafor package because of
# the non-independence of our data.
model.geral <- rma.mv(yi, vi, random = list( ~1|id_code, ~1|study_type,
                                             ~1|sampling_method), 
                      method="REML",  # "REML" = multi-level 
                      digits = 3, data = dados_completos)

summary(model.geral)

##Let's see the result in a graphic 
tiff("../Figures/overall.tiff", units="in", width=5, height=6,
     res=1200, compression = 'lzw')

orchard_plot(model.geral, xlab = "Hedges' g",
             data = dados_completos, group = "id_code") +
        labs(x = "Overall effect") + 
        scale_color_manual(values = "pink1") + 
        scale_fill_manual(values = "pink1") + 
        theme_classic() + 
        theme(axis.text = element_text(size = 14, colour = "black"),  
              axis.title = element_text(size = 16), 
              axis.text.y = element_blank(), 
              legend.position = "top")  

dev.off()


###### HETEROGENEITY - model: overall lethal and sublethal pesticide effects ###


#Now, we are going to calculate the heterogeneity the model and the
#heterogeneity of each random variable, using the I² statistic with a
#95% confidence interval (CI).
dados_completos$wi <- 1/sqrt(dados_completos$vi)
# precision = 1 / standard error of effect size (Equation 20; Nakagawa & Santos 2012)

s2m.0 <- sum(dados_completos$wi*(length(dados_completos$wi)-1))/
  (sum(dados_completos$wi)^2-sum(dados_completos$wi^2)) # Equation 22

#Let's calculate the total heterogeneity. In other words, the heterogeneity
#that is explained by our model including predictor and response variables.
I2.model.g <- ((model.geral$sigma2[1] +
                  model.geral$sigma2[2] + model.geral$sigma2[3] ))/
        (model.geral$sigma2[1]+  model.geral$sigma2[2] +
           model.geral$sigma2[3] +s2m.0) * 100

I2.model.g #mean 

## and 95% CI for I2.total:
I2.model.g - qchisq(.95, df=1)/2; I2.model.g + qchisq(.95, df=1)/2

#The values of heterogeneity 25%, 50%, and 75% 
#are considered as small, medium, and high, respectively, as suggested by
#Higgins (2003).
#Calculate I² for each random factor .

#--- ID code ---
I2.id <- ((model.geral$sigma2[1])/
                  (model.geral$sigma2[1] + 
                     model.geral$sigma2[2] + model.geral$sigma2[3] + 
                           s2m.0)) * 100

I2.id

## and 95% CI for I2.id:
I2.id - qchisq(.95, df=1)/2; I2.id + qchisq(.95, df=1)/2


#--- study type ---
I2.study <- ((model.geral$sigma2[2])/
                     (model.geral$sigma2[1] +
                        model.geral$sigma2[2] + model.geral$sigma2[3] + 
                              s2m.0)) * 100

I2.study

## and 95% CI for I2.study:
I2.study - qchisq(.95, df=1)/2; I2.study + qchisq(.95, df=1)/2


#--- sampling method ---
I2.sampling <- ((model.geral$sigma2[3])/
                        (model.geral$sigma2[1] +
                           model.geral$sigma2[2] + model.geral$sigma2[3] + 
                                 s2m.0)) * 100

I2.sampling

## and 95% CI for I2.sampling:
I2.sampling - qchisq(.95, df=1)/2; I2.sampling + qchisq(.95, df=1)/2

#confidence intervals for sigma2
confint(model.geral)


############################### PUBLICATION BIAS ###############################


## MODEL: overall lethal and sublethal pesticide effects
#Publication bias in each model using an adapted version of Egger’s regression (Nakagawa & Santos, 2012)
#A publication bias is pointed out when the intercept of the regression significantly deviates from zero
egger = lm(residuals(model.geral)~sqrt(dados_completos$vi)) 
summary(egger)  
confint.lm(egger, level = 0.95)

## MODEL: Pesticide effect type as a moderator
#We used pesticide effect type (i.e., lethal or sublethal) as a moderator to estimate 
#the magnitude of lethal and sublethal effects separately
model.1 <- rma.mv(yi, vi, mods= ~pesticide_effect, 
                  random = list( ~1|id_code, ~1|study_type, ~1|sampling_method),
                  method="REML",  # "REML" = multi-level 
                  digits = 3, data = dados_completos, 
                  control = list(optimizer="optim", optmethod="Nelder-Mead"))


summary(model.1) 

#Let's see the result in a graphic 
#We need a model without moderator (i.e., mods = ~moderator -1)
model.1f <- rma.mv(yi, vi, mods = ~pesticide_effect -1,
                   random = list( ~1|id_code, ~1|study_type, ~1|sampling_method),
                   method="REML",  # "REML" = multi-level 
                   digits = 3, data = dados_completos)

summary(model.1f)

#Let's generate a figure  
tiff("../Figures/let_sublet.tiff", units="in", width=5, height=6, res=1200, compression = 'lzw')

orchard_plot(model.1f, mod = "pesticide_effect",
             xlab = "Hedges' g", data = dados_completos , group = "id_code" ) +
        scale_fill_manual(values = c( "#00A087B2", "sienna1")) +
        theme_classic() + 
        theme(axis.text = element_text(size = 14, colour = "black"),
              axis.title = element_text(size = 16),
              legend.position = "top") 

dev.off()

#In the sublethal group, we can see that some effect sizes are extreme
#comparing to the mean effect size. So, let's do a sensibility test to
#analyze the influence of these effect sizes.

#### sensibility test: sub_lethal ####

#Let's see only the sublethal effect sizes
sub <- read.csv("../Data/sub.csv", h= T, dec =".", sep = ",")
class(sub)
str(sub)
head(sub)
tail(sub)

model.sub <- rma.mv(yi, vi, random = list( ~1|id_code, ~1|study_type,
                                           ~1|sampling_method), 
                    method="REML",  # "REML" = multi-level 
                    digits = 3, data = sub)

summary(model.sub)

#Plot the result
orchard_plot(model.sub, xlab = "Hedges' g", data = sub , group = "id_code") +
        labs(x= "Sublethal") +
        scale_color_manual(values = "sienna1") + 
        scale_fill_manual(values = "sienna1") + 
        theme_classic() + 
        theme(axis.text = element_text(size = 14, colour = "black"), 
              axis.title = element_text(size = 16), 
              axis.text.y = element_blank(), 
              legend.position = "top")

#In our sensibility test, let's evaluate the standardized residuals.   
#Calculate the standardized residuals.
rs= rstandard(model.sub) 
plot( rs$resid, ylim = c(-10,8), xlim =c(-5,50))

#If our residuals are > or <3, then it means that something extremely unusual is happening.
abline(h = -3) #below this line
abline(h = 3) #above this line

#let's identify them
text(rs$resid, labels = sub$id_code, cex= 1, pos = 4)

# There are four outliers (C0091)
#Let's build a model without outliers 
sub.sensi.out <- read.csv("../Data/sub_sensi_out.csv", h= T, dec =".", sep = ",")
class(sub.sensi.out)
str(sub.sensi.out)
head(sub.sensi.out)
tail(sub.sensi.out)

model.sub.sensi.out <- rma.mv(yi, vi, random = 
                                list( ~1|id_code, ~1|study_type,
                                      ~1|sampling_method),
                              method="REML",  # "REML" = multi-level 
                              digits = 3, data = sub.sensi.out)

summary(model.sub.sensi.out)

#Plot the result 
tiff("../Figures/sensibility_test_sublet.tiff", units="in",
     width=5, height=6, res=1200, compression = 'lzw')

orchard_plot(model.sub.sensi.out, xlab = "Hedges' g",
             data = sub.sensi.out, group = "id_code") +
        labs(x= "Sublethal") +
        scale_color_manual(values = "sienna1") + 
        scale_fill_manual(values = "sienna1") + 
        theme_classic() + 
        theme(axis.text = element_text(size = 14, colour = "black"), 
              axis.title = element_text(size = 16), 
              axis.text.y = element_blank(), 
              legend.position = "top") 

dev.off() 

#Result: mean effect size is negative and different from zero. In other words,
# sublethal effects are strong enough to be detected even when we remove
# the outliers. 

#### sensibility test: lethal ####

#Let's see only the lethal effect sizes
let <- read.csv("../Data/let.csv", h= T, dec =".", sep = ",")
class(let)
str(let)
head(let)
tail(let)

model.let <- rma.mv(yi, vi, random = list( ~1|id_code,  ~1|sampling_method),
                    method="REML",  # "REML" = multi-level 
                    digits = 3, data = let)

summary(model.let)

#Plot the result
orchard_plot(model.let, xlab = "Hedges' g", data = let, group = "id_code" ) +
        labs(x = "Lethal") + 
        scale_color_manual(values = "#00A087B2") + 
        scale_fill_manual(values = "#00A087B2") + 
        theme_classic() + 
        theme(axis.text = element_text(size = 14, colour = "black"), 
              axis.title = element_text(size = 16), 
              axis.text.y = element_blank(), 
              legend.position = "top")  


#In our sensibility test, let's evaluate the standardized residuals.
#Calculate the standardized residuals.
rs= rstandard(model.let) 
plot( rs$resid, ylim = c(-10,8), xlim =c(-5,50))

#If our residuals are > or <3, then it means that something extremely
#unusual is happening.
abline(h = -3)
abline(h = 3)

#Let's identify them.
text(rs$resid, labels = let$id_code, cex= 1, pos = 2)

#There are no outliers.


######################### HETEROGENEITY ########################################


## MODEL: pesticide effect type as a moderator


#Now, we are going to calculate the heterogeneity the model and the
#heterogeneity of each random variable, using the I² statistic with a 
#95% confidence interval (CI).
dados_completos$wi <- 1/sqrt(dados_completos$vi)
# precision = 1 / standard error of effect size (Equation 20; Nakagawa & Santos 2012)

s2m.0 <- sum(dados_completos$wi*(length(dados_completos$wi)-1))/
  (sum(dados_completos$wi)^2-sum(dados_completos$wi^2)) # Equation 22

I2.model.1 <- ((model.1$sigma2[1] + model.1$sigma2[2] + model.1$sigma2[3] ))/
        (model.1$sigma2[1]+ + model.1$sigma2[2] + model.1$sigma2[3] +s2m.0) * 100

I2.model.1

## and 95% CI for I2.total:
I2.model.1 - qchisq(.95, df=1)/2; I2.model.1 + qchisq(.95, df=1)/2

#Calculating I² for each random factor 

#--- ID code ---

I2.id.1 <- ((model.1$sigma2[1])/
                    (model.1$sigma2[1] + model.1$sigma2[2] + model.1$sigma2[3] + 
                             s2m.0)) * 100

I2.id.1

## and 95% CI for I2.id.1:
I2.id.1 - qchisq(.95, df=1)/2; I2.id.1 + qchisq(.95, df=1)/2

#--- study type ---
I2.study.1 <- ((model.1$sigma2[2])/
                       (model.1$sigma2[1] + model.1$sigma2[2] + model.1$sigma2[3] + 
                                s2m.0)) * 100

I2.study.1

## and 95% CI for I2.study.1:
I2.study.1 - qchisq(.95, df=1)/2; I2.study.1 + qchisq(.95, df=1)/2

#--- sampling method ---
I2.sampling.1 <- ((model.1$sigma2[3])/
                          (model.1$sigma2[1] + model.1$sigma2[2] + model.1$sigma2[3] + 
                                   s2m.0)) * 100

I2.sampling.1

## and 95% CI for I2.sampling.1:
I2.sampling.1 - qchisq(.95, df=1)/2; I2.sampling.1 + qchisq(.95, df=1)/2

#confidence intervals for sigma2
confint(model.1)


######################### PUBLICATION BIAS #####################################


## MODEL: pesticide effect type as a moderator

#Publication bias in each model using an adapted version of Egger’s 
#regression (Nakagawa & Santos, 2012). A publication bias is pointed out
#when the intercept of the regression significantly deviates from zero.
egger2 = lm(residuals(model.1)~sqrt(dados_completos$vi)) 

summary(egger2)  

confint.lm(egger2, level = 0.95)


############################ END ###############################################