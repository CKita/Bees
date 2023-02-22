################################################################################
#### Ecological Synthesis Lab (SintECO): https://marcomellolab.wordpress.com

#### Authors: Cristina A. Kita, Laura C. Leal & Marco A. R. Mello

#### See README for further info:
#### https://github.com/CKita/Bees#readme
################################################################################


################################################################################
######################### SUBLETHAL EFFECTS ####################################
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


################### MODEL - MEAN SUBLETHAL EFFECT SIZE #########################

# First, let's see our data set
sub <- read.csv("../Data/sub.csv", h= T, dec =".", sep = ";")
class(sub)
str(sub)
head(sub)
tail(sub)

#Change some columns to the class "factor" 

sub$study_type <- as.factor(sub$study_type)
sub$nature_x <- as.factor(sub$nature_x)

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
#the non-independence of our data.

model.sub <- rma.mv(yi, vi, random = list( ~1|id_code, ~1|study_type,
                                           ~1|sampling_method), 
                    method="REML",  # "REML" = multi-level 
                    digits = 3, data = sub)

summary(model.sub)

#Plot the result

tiff("../Figures/sublethal.tiff", units="in", width=7,
     height=6, res=1200, compression = 'lzw')

orchaRd::orchard_plot(model.sub, xlab = "Hedges' g", data = sub , 
                      group = "id_code", branch.size = 1.4, 
                      trunk.size = 8) +
        labs(x= "Sublethal effect") +
        scale_color_manual(values = "sienna1") + 
        scale_fill_manual(values = "sienna1") + 
        theme_classic() + 
        theme(axis.text = element_text(size = 14, colour = "black"), 
              axis.title = element_text(size = 16), 
              axis.text.y = element_blank(), 
              legend.position = "top")

dev.off()

#In our sensibility test, let's evaluate the standardized residuals.   
#Calculate the standardized residuals.

rs= rstandard(model.sub) 
plot( rs$resid, ylim = c(-10,8), xlim =c(-5,50))

#If our residuals are > or <3, 
#then it means that something extremely unusual is happening.
abline(h = -3) #below this line
abline(h = 3) #above this line

#let's identify them
text(rs$resid, labels = sub$id_code, cex= 1, pos = 4)

# There are no outliers

############################# HETEROGENEITY ####################################

#Now, we are going to calculate the heterogeneity of the model and the
#heterogeneity of each random variable, using the I² statistic with a
#95% confidence interval (CI).

sub$wi <- 1/sqrt(sub$vi)
# precision = 1 / standard error of effect size (Equation 20; Nakagawa & Santos 2012)

s2m.0 <- sum(sub$wi*(length(sub$wi)-1))/
        (sum(sub$wi)^2-sum(sub$wi^2)) # Equation 22

#Let's calculate the total heterogeneity. In other words, the heterogeneity
#that is explained by our model including predictor and response variables.
I2.sub <- ((model.sub$sigma2[1] +
                        model.sub$sigma2[2] + model.sub$sigma2[3] ))/
        (model.sub$sigma2[1]+  model.sub$sigma2[2] +
                 model.sub$sigma2[3] +s2m.0) * 100

I2.sub #mean 

## and 95% CI for I2.total:
I2.sub - qchisq(.95, df=1)/2; I2.sub + qchisq(.95, df=1)/2

#The values of heterogeneity 25%, 50%, and 75% 
#are considered as small, medium, and high, respectively, as suggested by
#Higgins (2003).
#Calculate I² for each random factor .

#--- ID code ---
I2.id <- ((model.sub$sigma2[1])/
                  (model.sub$sigma2[1] + 
                           model.sub$sigma2[2] + model.sub$sigma2[3] + 
                           s2m.0)) * 100

I2.id

## and 95% CI for I2.id:
I2.id - qchisq(.95, df=1)/2; I2.id + qchisq(.95, df=1)/2


#--- study type ---
I2.study <- ((model.sub$sigma2[2])/
                     (model.sub$sigma2[1] +
                              model.sub$sigma2[2] + model.sub$sigma2[3] + 
                              s2m.0)) * 100

I2.study

## and 95% CI for I2.study:
I2.study - qchisq(.95, df=1)/2; I2.study + qchisq(.95, df=1)/2


#--- sampling method ---
I2.sampling <- ((model.sub$sigma2[3])/
                        (model.sub$sigma2[1] +
                                 model.sub$sigma2[2] + model.sub$sigma2[3] + 
                                 s2m.0)) * 100

I2.sampling

## and 95% CI for I2.sampling:
I2.sampling - qchisq(.95, df=1)/2; I2.sampling + qchisq(.95, df=1)/2

#confidence intervals for sigma2
confint(model.sub)


#########################  PUBLICATION BIAS ####################################


#Publication bias in each model using an adapted version of Egger’s regression 
#(Nakagawa & Santos, 2012).A publication bias is pointed out when the 
#intercept of the regression significantly deviates from zero.

egger = lm(residuals(model.sub)~sqrt(sub$vi)) 
summary(egger)  
confint.lm(egger, level = 0.95)

#let´s plot
model.residuals <- residuals(model.sub) #model residuals
effect.sizes.vi<- sqrt(sub$vi) #variances of each effect size (vi)

bias <- cbind(model.residuals, effect.sizes.vi)
bias <- as.data.frame(bias)

tiff("../Figures/publication_bias.tiff", units="in", width=10,
     height=6, res=1200, compression = 'lzw')

ggplot(bias, aes(effect.sizes.vi, model.residuals)) + 
        geom_point(size = 3, alpha = 0.3) +
        geom_hline(yintercept= 0.0,linetype=2) +
        labs(x="Standard Error", y="Residual Value") +
        theme_classic() + 
        theme(axis.text = element_text(size = 14, colour = "black"), 
              axis.title = element_text(size = 16)) 
            
dev.off()

################################ END ###########################################

