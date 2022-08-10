################################################################################
#### Ecological Synthesis Lab (SintECO): https://marcomellolab.wordpress.com

#### Authors: Cristina A. Kita, Laura C. Leal & Marco A. R. Mello

#### See README for further info:
#### https://github.com/CKita/Bees#readme
################################################################################


################################################################################
################## EFFECT SIZES: CONTINUOUS DATA ############################### 
################################################################################


#Let's get ready for running the code provided here. 

#Set the working directory that contains the continuous data.  

#Delete all previous objects

rm(list= ls())


#Now,load the required package:

library("compute.es")


#First, let's see our data set


AG_cont <- read.csv("continuous.csv", h=T, dec = ",")

View(AG_cont)

summary(AG_cont)


#Changing some columns to the class "numeric"

AG_cont$value <- as.numeric(AG_cont$value)


#let's use the compute.es function to calculate the hedge's g value from the statistic reported in the papers.


#converting statistic F to Hedges'g: 

#########################################
# remember that:                        #
#                                       #
# fes(f, n.1, n.2)                      #
#                                       #
# f =  statistic value                  #
# n.1 =  sample size of treatment group #
# n.2 =  sample size of control group   #
#                                       #
#########################################

#Let's pick the data 

AG_cont

C0135_ <- AG_cont[AG_cont$id_code == "C0135", ]

C0135_ <- C0135_[c(1:9),]

C0135_

C0179_ <- AG_cont[AG_cont$id_code ==  "C0179", ]

C0179_

#binding 

C0135_C0179 <- rbind(C0135_, C0179_)

C0135_C0179

summary(C0135_C0179)

#Changing some columns to the class "numeric"

for (i in 4:6){
        C0135_C0179 [ ,i] <- as.numeric(C0135_C0179[,i] )
}

C0135_C0179$value <- as.numeric(C0135_C0179$value)

summary(C0135_C0179)

#converting  

es_f <- fes(C0135_C0179$value, C0135_C0179$sample_size_control, C0135_C0179$sample_size_treatment)

View(es_f)

es_f_g_var <- cbind(es_f$g, es_f$var.g) #picking the data 

colnames(es_f_g_var) <- c("yi", "vi") #naming the columns 

View(es_f_g_var)

es_F_prontos <- cbind(C0135_C0179, es_f_g_var) ##binding the data

View(es_F_prontos)


#converting statistic t to Hedges'g: 

###############################################
# remember that:                              #
#                                             #
# tes(t, n.1, n.2)                            #
#                                             #
# t = t-test value reported in primary study. #
# n.1 = Sample size of treatment group.       #
# n.2 = Sample size of control group.         #
#                                             #
###############################################

#Let's pick the data 

AG_cont

dado_t <- AG_cont[AG_cont$id_code == "C0143", ]
dado_t

class(dado_t)
str(dado_t)

dado_t$sample_size_control <- as.numeric(dado_t$sample_size_control)
dado_t$sample_size_treatment <- as.numeric(dado_t$sample_size_treatment)

#converting 

es_t <- tes(dado_t$value, dado_t$sample_size_control, dado_t$sample_size_treatment)  

View(es_t)

es_t_d_var <- cbind(es_t$g, es_t$var.g) #picking the data

es_t_d_var

colnames(es_t_d_var) <- c("yi", "vi") #naming the columns 

es_t_prontos <- cbind(dado_t, es_t_d_var) #binding 

View(es_t_prontos)


#converting statistic r to Hedges'g:

########################################################################
# Remember that:                                                       #
#                                                                      #
# res(r, var.r = NULL, n)                                              #
#                                                                      #
# r = Correlation coefficient.                                         #
#                                                                      #
# var.r = Variance of r. If value is not reported then leave it blank  #
# and variances will be computed based on sample size.                 #
#                                                                      #
# n = Total sample size.                                               #            
########################################################################

#Let's pick the data 

AG_cont

dado_C0120 <- AG_cont[AG_cont$id_code == "C0120", ]
dado_C0120


dado_C0135 <- AG_cont[AG_cont$id_code == "C0135", ]
dado_C0135

dado_C0135 <- dado_C0135[c(10:12),]

dado_r <- rbind(dado_C0120, dado_C0135)
dado_r

str(dado_r)

dado_r$total_sample_size <- as.numeric(dado_r$total_sample_size)
dado_r$sample_size_control <- as.numeric(dado_r$sample_size_control)
dado_r$sample_size_treatment <- as.numeric(dado_r$sample_size_treatment)

#converting  

es_r <- res(dado_r$value, var.r = NULL, dado_r$total_sample_size)

View(es_r)

es_r_d_var <- cbind(es_r$d, es_r$var.d) #picking the data

colnames(es_r_d_var) <- c("yi", "vi") #naming the columns 

as.data.frame(es_r_d_var) #we need to convert to Hedges'g 


#Let's make a function to convert hedges' d to hedges' g because the function res only provides d and var.d values.

#picking the information needed
d <- es_r$d
d

vd <- es_r$var.d
vd

#df= degrees of freedom, which is n1 +n2 - 2 
df <- dado_r$total_sample_size - 2


#to calulate the "j" from hedge's g 

j <- function(x){
        
        1-3/(4*x-1)
}

Js <- j(df)
Js

#to calculate the g

g <- function(y){
        
        y*d
}

G <- g(Js)

#to calculate the variance of g (i.e., var.g)

var.g <- function(z){
        
        Js^2*vd
}


Var.g <- var.g(Js)

#now, let's bind 

es_r_g_var.g <- cbind(G, Var.g)

colnames(es_r_g_var.g) <- c("yi", "vi")

es_r_g_var.g

es_r_prontos <- cbind(dado_r, es_r_g_var.g)  

View(es_r_prontos)


#binding all the statistics converted 

es_cont <- rbind(es_r_prontos, es_F_prontos, es_t_prontos )

View(es_cont)

es_cont <- es_cont[-18,] #not related to bee pollination

#OBS:
#There are values of effect size need to be multiplied by -1, because when we convert the statistics,the effect size is always positive.
#In cases in which authors reported a negative effect of pesticide application, positive values of effect sizes do not make sense.
#Therefore, in this cases, we need to manually multiply the effect size by -1. 

#let's multiply by -1 the effect sizes of papers that reported a negative effect of pesticide application. 

es_cont$yi[c(1,2,3,4,5,8,9,12,14,15,18)] <- es_cont$yi[c(1,2,3,4,5,8,9,12,14,15,18)] * -1

View(es_cont)

#saving 

write.csv(es_cont, file = "effect_sizes_cont.csv", row.names = F)

write.table(es_cont, file = "effect_sizes_cont.txt", row.names = F, col.names = T, dec = ".", sep = ",")
