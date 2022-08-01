#######################################

#Effect sizes - categorical data 

#######################################


#Let's get ready for running the code provided here. 

#Set the working directory that contains the categorical data.  

#Delete all previous objects

rm(list= ls())

#Now,load the required packages:

library("metafor")
library("compute.es")



###############################################################
#### EFFECT SIZES - CATEGORICAL DATA 
###############################################################


#First, let's see our data set

AG_cat <- read.csv("cat.csv", h=T, dec = ",")

View(AG_cat)

summary(AG_cat)

#Changing some columns to the class "numeric"

for (i in 4:10){
        AG_cat[ ,i] <- as.numeric(AG_cat[,i] )
}

summary(AG_cat)


##########################################################################################################################
#Before starting to calculate the effect sizes,
#Let's remember some points:

#1
#To calculate the effect sizes, we are going to use the "escalc" function.

#2
#Our metric of effect size is Hedges’ g. 
#Therefore, we are going to use the measure "standardized mean difference" (SMD).

#3
#We are going to use the treatment group as a reference (with pesticide application), subtracting
#from it the mean value of the control group (without pesticide application or organic farms).

#4
#We need to input the following information in the escalc function:

#m1i = mean_treatment
#m2i = mean_control
#sd1i = sd_treatment (standard deviation)
#sd2i = sd_control (standard deviation)
#n1i = sample_size_treatment
#n2i = sample_size_control

#############################################################################################################################

#Let's calculate the effect sizes

effect_sizes <- escalc("SMD", m1i = mean_treatment, m2i = mean_control, 
                       sd1i = sd_treatment, sd2i= sd_control, 
                       n1i=sample_size_treatment, n2i= sample_size_control, 
                       data = AG_cat)

View(effect_sizes) #effect size (yi) and variance (vi) 


#Now, let's use the compute.es function to calculate the hedge's g value from the statistic F reported in the paper C0142.

#First, let's pick the data from C0142. 

d_brutos <- read.csv("planilha_bruta.csv", h =T, dec= ",")

C0142_BRUTO <- d_brutos[d_brutos$id_code == "C0142", ]

summary(C0142_BRUTO)

View(C0142_BRUTO)
str(C0142_BRUTO)

C0142_BRUTO$value[1]<- 0.62
C0142_BRUTO$value <- as.numeric(C0142_BRUTO$value)

C0142_BRUTO$value


#Now, Let's use the function fes 

effect_c0142 <- fes(C0142_BRUTO$value, C0142_BRUTO$sample_size_control, C0142_BRUTO$sample_size_treatment)

View(effect_c0142)

es_f_d_var <- cbind(effect_c0142$d, effect_c0142$var.d) #picking the data

colnames(es_f_d_var) <- c("yi", "vi") #naming the columns 

es_f_d_var

View(es_f_d_var)


C0142_stat <- cbind (C0142_BRUTO$id_code, C0142_BRUTO$study_type, C0142_BRUTO$nature_x, 
                     C0142_BRUTO$total_sample_size , C0142_BRUTO$sample_size_control, 
                     C0142_BRUTO$sample_size_treatment, C0142_BRUTO$statistic, C0142_BRUTO$value)

colnames(C0142_stat) <- c("id_code", "study_type", "nature_x", "total_sample_size", "sample_size_control", "sample_size_treatment", "statistic", "value")

es_F_prontos <- cbind(C0142_stat, es_f_d_var) #binding the data

es_F_prontos

#adding some information 

effect_type <- as.data.frame(rep("community", 2))

plant_specie <- as.data.frame(rep("", 2))

colnames(effect_type) <- "effect_type"
colnames(plant_specie) <- "plant_specie"

plant_specie

ef_plant <- cbind(effect_type, plant_specie)

pronto <- cbind(es_F_prontos, ef_plant)

View(pronto)

str(pronto)

#Changing the class of some columns

for (i in 4:6){
        pronto [ ,i] <- as.numeric(pronto[,i] )
}

str(pronto)

for (i in 8:10){
        pronto [ ,i] <- as.numeric(pronto[,i] )
}

str(pronto)


#combining the data from escalc and fes functions 

str(effect_sizes)

str(pronto)

effects_prontos <- dplyr::bind_rows( effect_sizes, pronto)

View(effects_prontos)
str(effects_prontos)


#saving 

#.txt
write.table(effects_prontos, "effect_sizes_cat.txt", dec = ".", sep = ",", row.names = F )

#.csv 

write.csv(effects_prontos, "effect_sizes_cat.csv", row.names = F)

############################################################################################################################
