################################################################################
#### Ecological Synthesis Lab (SintECO): https://marcomellolab.wordpress.com

#### Authors: Cristina A. Kita, Laura C. Leal & Marco A. R. Mello

#### See README for further info:
#### https://github.com/CKita/Bees#readme
################################################################################


################################################################################
############### CONSEQUENCES OF LETHAL AND SUBLETHAL EFFECTS ###################
################################################################################


#Let's get ready for running the code provided here. 

#Set the working directory to the source of this script file.   
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


########## MODEL - CONSEQUENCES OF LETHAL AND SUBLETHAL EFFECT SIZES ###########


# First, let's see our data set

conseq <- read.csv("../Data/consequences.csv", h= T, dec =".", sep = ";")
class(conseq)
str(conseq)
head(conseq)
tail(conseq)


#Change some columns to the class "factor" 

conseq$study_type <- as.factor(conseq$study_type)
conseq$nature_x <- as.factor(conseq$nature_x)
conseq$effect_type <- as.factor(conseq$effect_type) 
conseq$farm_type <- as.factor(conseq$farm_type) 


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


model.conseq <- rma.mv(yi, vi, 
                       random = list( ~1|id_code, ~1|sampling_method, 
                                      ~1|farm_type, ~1|plant_species),
                       method="REML",  # "REML" = multi-level 
                       digits = 3, data = conseq)

summary(model.conseq)

#Let's see the result in a graphic 

#Plot the result

tiff("../Figures/consequences_let_sub.tiff", units="in",
     width=6, height=6, res=1200, compression = 'lzw')

orchaRd::orchard_plot(model.conseq, xlab = "Hedges' g", data = conseq,
                      group = "id_code", branch.size = 1.4, 
                      trunk.size = 8, k.pos = "none" ) +
        labs(x = "Consequences for bee communities" ) + 
        scale_fill_manual(values = "#00A087B2") +
        theme_classic() + 
        theme(axis.text = element_text(size = 14, colour = "black"), 
              axis.title = element_text(size = 16), 
              axis.text.y = element_blank(), 
              legend.position = "top")  

dev.off()

#In our sensibility test, let's evaluate the standardized residuals.
#Calculate the standardized residuals.
rs= rstandard(model.conseq) 
plot( rs$resid, ylim = c(-10,8), xlim =c(-5,50))

#If our residuals are > or <3, then it means that something extremely
#unusual is happening.
abline(h = -3)
abline(h = 3)

#Let's identify them.
text(rs$resid, labels = conseq$id_code, cex= 1, pos = 2)

#There are no outliers.

########################### HETEROGENEITY ######################################

#Now, we are going to calculate the heterogeneity of the model and the
#heterogeneity of each random variable, using the I² statistic with a
#95% confidence interval (CI).

conseq$wi <- 1/sqrt(conseq$vi)
# precision = 1 / standard error of effect size (Equation 20; Nakagawa & Santos 2012)

s2m.0 <- sum(conseq$wi*(length(conseq$wi)-1))/
        (sum(conseq$wi)^2-sum(conseq$wi^2)) # Equation 22

#Let's calculate the total heterogeneity. In other words, the heterogeneity
#that is explained by our model including predictor and response variables.
I2.conseq <- ((model.conseq$sigma2[1] + model.conseq$sigma2[2] +
                       model.conseq$sigma2[3] + model.conseq$sigma2[4] ))/
        (model.conseq$sigma2[1]+  model.conseq$sigma2[2] +
                 model.conseq$sigma2[3] + model.conseq$sigma2[4] +s2m.0) * 100

I2.conseq #mean 

## and 95% CI for I2.total:
I2.conseq - qchisq(.95, df=1)/2; I2.conseq + qchisq(.95, df=1)/2

#The values of heterogeneity 25%, 50%, and 75% 
#are considered as small, medium, and high, respectively, as suggested by
#Higgins (2003).
#Calculate I² for each random factor .

#--- ID code ---
I2.id <- ((model.conseq$sigma2[1])/
                  (model.conseq$sigma2[1] + 
                           model.conseq$sigma2[2] + model.conseq$sigma2[3] + 
                           model.conseq$sigma2[4] +
                                                     s2m.0)) * 100

I2.id

## and 95% CI for I2.id:
I2.id - qchisq(.95, df=1)/2; I2.id + qchisq(.95, df=1)/2


#--- sampling method ---
I2.sampling <- ((model.conseq$sigma2[2])/
                        (model.conseq$sigma2[1] + 
                                 model.conseq$sigma2[2] + model.conseq$sigma2[3] + 
                                 model.conseq$sigma2[4] +
                                                                  s2m.0)) * 100
I2.sampling

## and 95% CI for I2.study:
I2.sampling - qchisq(.95, df=1)/2; I2.sampling + qchisq(.95, df=1)/2


#--- farm type ---
I2.farm <-  ((model.conseq$sigma2[3])/
                     (model.conseq$sigma2[1] + 
                              model.conseq$sigma2[2] + model.conseq$sigma2[3] +
                              model.conseq$sigma2[4] +
                                                            s2m.0)) * 100
I2.farm

## and 95% CI for I2.sampling:
I2.farm - qchisq(.95, df=1)/2; I2.farm + qchisq(.95, df=1)/2


#--- plant species ---

I2.plant <-  ((model.conseq$sigma2[4])/
                     (model.conseq$sigma2[1] + 
                              model.conseq$sigma2[2] + model.conseq$sigma2[3] +
                              model.conseq$sigma2[4] +
                              s2m.0)) * 100
I2.plant

## and 95% CI for I2.sampling:
I2.plant - qchisq(.95, df=1)/2; I2.plant + qchisq(.95, df=1)/2

#confidence intervals for sigma2
confint(model.conseq)


############################### PUBLICATION BIAS ###############################

#Publication bias in each model using an adapted version of Egger’s regression 
#(Nakagawa & Santos, 2012).A publication bias is pointed out when the 
#intercept of the regression significantly deviates from zero

egger = lm(residuals(model.conseq)~sqrt(conseq$vi)) 
summary(egger)  
confint.lm(egger, level = 0.95)


model.residuals <- residuals(model.conseq) #model residuals 
effect.sizes.vi <- sqrt(conseq$vi) #variances of each effect size (vi)

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