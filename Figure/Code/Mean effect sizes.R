################################################################################
#### Ecological Synthesis Lab (SintECO): https://marcomellolab.wordpress.com

#### Authors: Cristina A. Kita, Laura C. Leal & Marco A. R. Mello

#### See README for further info: https://github.com/CKita/Bees#readme
################################################################################


################################################################################
############################ MEAN EFFECT SIZES ################################# 
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
############################ EFFECT SIZES ######################################

#------------------------MEAN LETHAL EFFECT SIZE-------------------------------- 


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

p1 <- orchaRd::orchard_plot(lethal, data= dados,
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

#check the plot
p1

#-----------------------MEAN SUBLETHAL EFFECT SIZE------------------------------ 

# First, let's see our data set
sub <- read.csv("../Data/sub.csv", h= T, dec =".", sep = ";")
class(sub)
str(sub)
head(sub)
tail(sub)

#Change some columns to the class "factor" 

sub$study_type <- as.factor(sub$study_type)
sub$nature_x <- as.factor(sub$nature_x)

#We are going to build a meta-analytic mixed-effects model.
#We are going to use the function rma.mv from the metafor package because of
#the non-independence of our data.

model.sub <- rma.mv(yi, vi, random = list( ~1|id_code, ~1|study_type,
                                           ~1|sampling_method), 
                    method="REML",  # "REML" = multi-level 
                    digits = 3, data = sub)

summary(model.sub)

#Plot the result

p2 <- orchaRd::orchard_plot(model.sub, xlab = "Hedges' g", data = sub , 
                      group = "id_code", branch.size = 1.4, 
                      trunk.size = 8, k.pos = "none") +
  labs(x= "Sublethal effect") +
  scale_color_manual(values = "sienna1") + 
  scale_fill_manual(values = "sienna1") + 
  theme_classic() + 
  theme(axis.text = element_text(size = 14, colour = "black"), 
        axis.title = element_text(size = 16), 
        axis.text.y = element_blank(), 
        legend.position = "top")

#check the plot
p2

#---------------CONSEQUENCES OF LETHAL AND SUBLETHAL EFFECT SIZES--------------- 

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

p3 <- orchaRd::orchard_plot(model.conseq, xlab = "Hedges' g", data = conseq,
                      group = "id_code", branch.size = 1.4, 
                      trunk.size = 8, k.pos = "none" ) +
  labs(x = "Consequences for bee communities" ) + 
  scale_fill_manual(values = "#00A087B2") +
  theme_classic() + 
  theme(axis.text = element_text(size = 14, colour = "black"), 
        axis.title = element_text(size = 16), 
        axis.text.y = element_blank(), 
        legend.position = "top")  

#check the plot 
p3

#-------------------

#Export all plots together as a single PNG image.

png("../Figure/mean_effect_sizes.png", res = 300,
    width = 3000, height = 4200, unit = "px")

cowplot::plot_grid(p1, p2, p3, rel_heights = 11.0,ncol =1,
                   rel_widths = 0.5, align = "v",
                   labels = c("A", "B", "C"))

dev.off()

################################ END ###########################################
