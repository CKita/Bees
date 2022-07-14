##############################################

# Bee survival  

##############################################


#Let's get ready for running the code provided here. 

#Set the working directory that contains the bee survival data.  

#Delete all previous objects

rm(list= ls())


##############################################
##### EFFECT SIZES 
##############################################

#Now, let's see our raw data set

dat <- read.csv("dados_sobrevivencia.csv", h= T, dec =".", sep = ",")

View(dat)

#To calculate the effect sizes, Load the required package: 

library(metafor)

#To calculate the effect sizes, we are going to use the "escalc" function.
#To calculate the bee survival probability, we are going to use odds ratio ("OR") as a metric of effect size.

effects_t <- escalc("OR", dat$ai,dat$bi, dat$ci, dat$di, dat$n1i, dat$n2i)

View(effects_t)

#Now, let's bind the effect sizes with the raw data by columns 

dat_comp <- cbind(dat, effects_t)

View(dat_comp)

#OK. Let's save the complete data set

write.csv2(dat_comp, "data_comp.csv", row.names = F) 



##############################################
#### MODEL - MEAN EFFECT SIZE 
##############################################

##We are going to build a meta-analytic model to calculate the mean effect size.
#Let's use the complete data set to build our model.  

dados <- read.table("data_comp.csv", h=T, dec=",", sep = ";")

View(dados)

#Considering that bee survival rate is correlated with life history, we built a phylogenetic covariance matrix
#using the Interactive Tree of Life online tree generator database. 
#So, let's do this

#install the packages: 

install.packages("scales")
install.packages("ape")
install.packages("rotl")

library(scales)
library(ape)
library(rotl)

#Let's search our bee species in the Tree of Life 

spp <- tnrs_match_names(unique(dados$bee_specie), context_name = "Animals") 

my_tree = tol_induced_subtree(ott_ids=spp$ott_id) #here, we are creating a sub tree with our bee species. 

#Now we are calculating the phylogenetic distances and creating the covariance matrix 

otl_tips=strip_ott_ids(my_tree$tip.label, remove_underscores=TRUE)

taxon_map=structure(spp$search_string, names=spp$unique_name)

my_tree$tip.label=taxon_map[otl_tips]

my_tree.ult = compute.brlen(my_tree, method = "Grafen")

plot(my_tree.ult,no.margin=T) #let's see

cov.matrix = vcv(my_tree.ult,corr=T)
cov.matrix[,0]

#OK. Now, let's build our model

#We are going to build a meta-analytic mixed-effects model.
#We are going to use the function rma.mv from the metafor package because of the non-independence of our data. 

survival <- rma.mv(yi, vi, random = list( ~1|id_code, ~1|bee_specie, ~1|agrochemical, ~1|hours_after_exposure, ~1|exposure_type), method="REML",  #"REML" = multi-level 
            R = list(bee_specie = cov.matrix),digits = 3, data = dados)                   

summary(survival)


#Let's see the result in a graphic 

#install the required packages:

install.packages("dplyr")
install.packages("devtools")
install.packages("tidyverse")
install.packages("patchwork")
install.packages("R.rsp")
install.packages("ggplot2")
install.packages("Rtools42")

devtools::install_github("itchyshin/orchard_plot", subdir = "orchaRd", force = TRUE, build_vignettes = TRUE)

library(devtools)
library(orchaRd)
library(patchwork)
library(tidyverse)
library(metafor)


#Let's generate a figure .tiff  

tiff('survival.tiff', units="in", width=5, height=6, res=1200, compression = 'lzw')

orchard_plot(survival, xlab = "Odds ratio") +
        labs(y = "Survival") +
        scale_color_manual(values = "slateblue1") +
        scale_fill_manual(values = "yellow") +
        theme_classic() + 
        theme(axis.text = element_text(size = 14, colour = "black"),
              axis.title = element_text(size = 16),
              axis.text.x = element_blank(),
              legend.position = "top") +
        coord_flip()

dev.off()

#We can see that some effect sizes are extreme comparing to the mean effect size.
#So, let's do a sensibility test to analyse the influence of these effect sizes.

#here, the sensibility test that we are going to do is evaluating the standardized residuals   

#Calculating the standardized residuals
rs = rstandard(survival) 
plot(rs$resid, ylim = c(-8.0,8), xlim =c(-8,50))

#If our residuals are +/-3, then it means that something extremely unusual is happening. 
abline(h = -3)
abline(h = 3)

text(rs$resid, labels = dados$id_code, cex= 1, pos = 2)

#So, the effect sizes C0073, C0084, C0189 e C0191  are outliers (28 effect sizes). 

#Let's build a models without outliers 

surv_sensi2<- read.csv("survival_sensi_out2.csv", h= T, dec =".", sep = ",")

View(surv_sensi2)

#Building a covariance matrix using only the bee species of our data set without outliers 

spp <- tnrs_match_names(unique(surv_sensi2$bee_specie), context_name = "Animals") 

my_tree = tol_induced_subtree(ott_ids=spp$ott_id)

otl_tips=strip_ott_ids(my_tree$tip.label, remove_underscores=TRUE)

taxon_map=structure(spp$search_string, names=spp$unique_name)

my_tree$tip.label=taxon_map[otl_tips]

my_tree.ult = compute.brlen(my_tree, method = "Grafen")

plot(my_tree.ult,no.margin=T) 

cov.matrix = vcv(my_tree.ult,corr=T)
cov.matrix[,0]

#our model without outliers 

model.surv.sensi.out2 <- rma.mv(yi, vi, random = list( ~1|id_code, ~1|bee_specie, ~1|agrochemical, ~1|hours_after_exposure, ~1|exposure_type), method="REML",  # "REML" = multi-level 
                                R = list(bee_specie = cov.matrix),digits = 3, control = list(optimizer="optim", optmethod="Nelder-Mead"), data = surv_sensi2)

summary(model.surv.sensi.out2)

tiff('survival_sensibility_test.tiff', units="in", width=5, height=6, res=1200, compression = 'lzw')

orchard_plot(model.surv.sensi.out2, xlab = "Odds ratio") +
        labs(y = "Survival") + # troca o nome do eixo y
        scale_color_manual(values = "pink1") + #troca a cor dos pontos do fundo
        scale_fill_manual(values = "seagreen3") + #troca a cor do ponto central 
        theme_classic() + #tira a caixa em volta do grafico
        theme(axis.text = element_text(size = 14, colour = "black"), #muda o tamanho do texto dos eixos e na cor do eixo 
              axis.title = element_text(size = 16), #muda o tamanho do titulo do eixo
              axis.text.x = element_blank(), #Isso remove o texto automatico do eixo y
              legend.position = "top") + #troca a posicao da legenda 
        coord_flip() 

dev.off()

#Result: mean effect size is negative and different from zero. In other words, pesticide effects are strong enough to be detected
#even when our sampling size is reduced.  


###############################################
#### HETEROGENEITY
###############################################

#Now, we are going to calculate the heterogeneity the model and the heterogeneity of each random variable,
#using the I² statistic with a 95% confidence interval (CI).


dados$wi <- 1/sqrt(dados$vi) # precision = 1 / standard error of effect size (Equation 20; Nakagawa & Santos 2012)

s2m.0 <- sum(dados$wi*(length(dados$wi)-1))/(sum(dados$wi)^2-sum(dados$wi^2)) # Equation 22

#Let's calculate the total heterogeneity. In other words, the heterogeneity that is explained by our model 
#including predictor and response variables

I2.survival <- ((survival$sigma2[1] + survival$sigma2[2] +  survival$sigma2[3] + survival$sigma2[4] + survival$sigma2[5])/  
                 (survival$sigma2[1] + survival$sigma2[2] +  survival$sigma2[3] + survival$sigma2[4] + survival$sigma2[5] + s2m.0)) * 100 

#The values of heterogeneity 25%, 50%, and 75% 
#were considered as small, medium, and high, respectively, as suggested by Higgins (2003).

I2.survival #mean 


## and 95% CI for I2.total:

I2.survival - qchisq(.95, df=1)/2; I2.survival + qchisq(.95, df=1)/2


#Calculating I² for each random factor 

#--- study ID ---

I2.study.bee <- ((survival$sigma2[1])/(
        survival$sigma2[1]+ survival$sigma2[2] +  survival$sigma2[3]  +  survival$sigma2[4] + survival$sigma2[5] +
                s2m.0)) * 100

I2.study.bee

## and 95% CI for I2.study.bee:

I2.study.bee - qchisq(.95, df=1)/2; I2.study.bee + qchisq(.95, df=1)/2

#--- bee_specie ---

I2.bee <- ((survival$sigma2[2])/(
        survival$sigma2[1]+ survival$sigma2[2] +  survival$sigma2[3] + survival$sigma2[4] + survival$sigma2[5] +
                s2m.0)) * 100

I2.bee

## and 95% CI for I2.bee:
I2.bee - qchisq(.95, df=1)/2; I2.bee + qchisq(.95, df=1)/2

#--- agrochemical ---

I2.agro<- ((survival$sigma2[3])/(
        survival$sigma2[1]+survival$sigma2[2] +  survival$sigma2[3] + survival$sigma2[4] + survival$sigma2[5] +
                s2m.0)) * 100

I2.agro

## and 95% CI for I2.agro:
I2.agro - qchisq(.95, df=1)/2; I2.agro + qchisq(.95, df=1)/2

#----hours ---

I2.hours<- ((survival$sigma2[4])/(
        survival$sigma2[1]+survival$sigma2[2] +  survival$sigma2[3] + survival$sigma2[4] + survival$sigma2[5] +
                s2m.0)) * 100

I2.hours

## and 95% CI for I2.hours:
I2.hours - qchisq(.95, df=1)/2; I2.hours + qchisq(.95, df=1)/2

#---exposure type

I2.exp<- ((survival$sigma2[5])/(
        survival$sigma2[1]+survival$sigma2[2] +  survival$sigma2[3] + survival$sigma2[4] + survival$sigma2[5] +
                s2m.0)) * 100

I2.exp

## and 95% CI for I2.exp:
I2.exp - qchisq(.95, df=1)/2; I2.exp + qchisq(.95, df=1)/2

#confidence intervals for sigma2

confint(survival)


#############################################
#### PUBLICATION BIAS
#############################################


#Publication bias in each model using an adapted version of Egger’s regression (Nakagawa & Santos, 2012)

#A publication bias is pointed out when the intercept of the regression significantly deviates from zero

eggsm3 = lm(residuals(survival)~sqrt(dados$vi)) 

summary(eggsm3) 

confint.lm(eggsm3, level = 0.95)

##############################################################################

