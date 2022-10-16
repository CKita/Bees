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

if(!require(compute.es)){
  install.packages("compute.es")
  library(compute.es)
}

#First, let's see our data.
AG_cat <- read.csv("../Data/cat.csv", h=T, dec = ",")
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

#Now, let's use the compute.es function to calculate the Hedge's g value from
#the statistic F reported in the paper C0142. 
#First, let's pick the data from C0142. 
d_brutos <- read.csv("../Data/planilha_bruta.csv", h =T, dec= ",")
C0142_BRUTO <- d_brutos[d_brutos$id_code == "C0142", ]

class(C0142_BRUTO)
str(C0142_BRUTO)
summary(C0142_BRUTO)

C0142_BRUTO$value[1]<- 0.62
C0142_BRUTO$value <- as.numeric(C0142_BRUTO$value)
C0142_BRUTO$value

#Now, Let's convert the data.
effect_c0142 <- fes(C0142_BRUTO$value, 
                    C0142_BRUTO$sample_size_control, 
                    C0142_BRUTO$sample_size_treatment)

class(effect_c0142)
str(effect_c0142)
head(effect_c0142)
tail(effect_c0142)

es_f_d_var <- cbind(effect_c0142$g, effect_c0142$var.g) #pick g and var.g 
colnames(es_f_d_var) <- c("yi", "vi") #name the columns 
es_f_d_var

C0142_stat <- cbind (C0142_BRUTO$id_code, C0142_BRUTO$study_type,
                     C0142_BRUTO$nature_x, C0142_BRUTO$total_sample_size,
                     C0142_BRUTO$sample_size_control, 
                     C0142_BRUTO$sample_size_treatment, 
                     C0142_BRUTO$statistic, 
                     C0142_BRUTO$value)

colnames(C0142_stat) <- c("id_code", "study_type", "nature_x",
                          "total_sample_size", "sample_size_control",
                          "sample_size_treatment", "statistic", "value")

es_F_prontos <- cbind(C0142_stat, es_f_d_var) #bind the data

es_F_prontos

#Add some information 
effect_type <- as.data.frame(rep("community", 2))
plant_specie <- as.data.frame(rep("Cucurbita ssp.", 2))
pesticide_effect <- as.data.frame(rep("consequences", 2))

colnames(effect_type) <- "effect_type"
colnames(plant_specie) <- "plant_specie"
colnames(pesticide_effect) <- "pesticide_effect"

effect_type
plant_specie
pesticide_effect

ef_plant <- cbind(effect_type, plant_specie, pesticide_effect)
pronto <- cbind(es_F_prontos, ef_plant)

class(pronto)
str(pronto)
head(pronto)
tail(pronto)

#Change the class of some columns.
for (i in 4:6){
        pronto [ ,i] <- as.numeric(pronto[,i] )
}

str(pronto)

for (i in 8:10){
        pronto [ ,i] <- as.numeric(pronto[,i] )
}

str(pronto)

#Combine the results from the "escalc" and "fes" functions.
str(effect_sizes)
str(pronto)

effects_prontos <- dplyr::bind_rows( effect_sizes, pronto)
class(effects_prontos)
str(effects_prontos)
head(effects_prontos)
tail(effects_prontos)

#Export the results. 
write.csv(effects_prontos, "../Results/effect_sizes_cat.csv", row.names = F)


############################ END ###############################################