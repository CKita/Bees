################################################################################
#### Ecological Synthesis Lab (SintECO): https://marcomellolab.wordpress.com

#### Authors: Cristina A. Kita, Laura C. Leal & Marco A. R. Mello

#### See README for further info:
#### https://github.com/CKita/Bees#readme
################################################################################


################################################################################
################## EFFECT SIZES: CATEGORICAL DATA ############################## 
################################################################################


#Let's get ready for running the code.

#Set the working directory to the source of this script file.  
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#Delete all previous objects.
rm(list= ls())

#Now,load or install the required packages.
if(!require(metafor)){
  install.packages("metafor")
  library(metafor)
}

#First, let's see our data.
AG_cat <- read.csv("../Data/cat.csv", h=T, dec = ".", sep = ";")
class(AG_cat)
str(AG_cat)
head(AG_cat)
tail(AG_cat)
summary(AG_cat)

#Change some columns to the class "numeric".
for (i in 4:10){
        AG_cat[ ,i] <- as.numeric(AG_cat[,i] )
}

str(AG_cat)
summary(AG_cat)

#Before starting to calculate the effect sizes, let's remember some points:

#1
#To calculate the effect sizes, we are going to use the "escalc" function.

#2
#Our metric of effect size is Hedges’ g. 
#Therefore, we are going to use the measure argument "standardized mean
#difference" (SMD).
#OBS: For measure="SMD", the positive bias in the standardized mean
# difference (i.e., in a Cohen's d value) is automatically corrected for
#within the function, yielding Hedges' g (Hedges, 1981). 

#3
#We are going to use the treatment group as a reference (with pesticide application), subtracting
#from it the mean value of the control group (without pesticide application or organic farms).

#4
#We need to input the following values in the escalc function:

#m1i = mean_treatment
#m2i = mean_control
#sd1i = sd_treatment (standard deviation)
#sd2i = sd_control (standard deviation)
#n1i = sample_size_treatment
#n2i = sample_size_control

#Let's calculate the effect sizes.
effect_sizes <- escalc("SMD", m1i = mean_treatment, m2i = mean_control, 
                       sd1i = sd_treatment, sd2i= sd_control, 
                       n1i=sample_size_treatment, n2i= sample_size_control, 
                       data = AG_cat)
class(effect_sizes)
str(effect_sizes)
head(effect_sizes) #effect size (yi) and variance (vi)
tail(effect_sizes)

#Export the results. 
write.csv(effect_sizes, "../Results/effect_sizes_cat.csv", row.names = F)


############################ END ###############################################