################################################################################
#### Ecological Synthesis Lab (SintECO): https://marcomellolab.wordpress.com

#### Authors: Cristina A. Kita, Laura C. Leal & Marco A. R. Mello

#### See README for further info:
#### https://github.com/CKita/Bees#readme
################################################################################


################################################################################
################## EFFECT SIZES: CONTINUOUS DATA ############################### 
################################################################################


#Let's get ready for running the code. 

#Set the working directory to the source of this script file.  
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#Delete all previous objects.
rm(list= ls())

#Now,load or install the required packages.
if(!require(compute.es)){
  install.packages("compute.es")
  library(compute.es)
}

#First, let's see our data.
AG_cont <- read.csv("../Data/continuous.csv", h=T, dec = ".", sep = ";")
class(AG_cont)
str(AG_cont)
head(AG_cont)
tail(AG_cont)
summary(AG_cont)

#Change some columns to "numeric".
AG_cont$value <- as.numeric(AG_cont$value)
str(AG_cont)

#Let's use the compute.es function to calculate the hedge's g value from the
#statistic reported in the papers.

#Convert statistic F to Hedges'g: 

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

#Let's pick the data.
AG_cont
C0135_ <- AG_cont[AG_cont$id_code == "C0135", ]
C0135_ <- C0135_[c(1:9),]
C0135_

str(C0135_)

#Change some columns to the class "numeric".
for (i in 4:6){
        C0135_ [ ,i] <- as.numeric(C0135_[,i] )
}


summary(C0135_)

#Convert the data. 
es_f <- fes(C0135_$value, 
            C0135_$sample_size_control, 
            C0135_$sample_size_treatment)
class(es_f)
str(es_f)

es_f_g_var <- cbind(es_f$g, es_f$var.g) #pick the data 
colnames(es_f_g_var) <- c("yi", "vi") #name the columns 
class(es_f_g_var)
str(es_f_g_var)

C0135_ef <- cbind(C0135_, es_f_g_var) ##bind the data


#Let's pick the data.

AG_cont
C0142_ <- AG_cont[AG_cont$id_code == "C0142", ]
C0142_

#fixing plant species name.

C0142_[c(1,2), 10] <- "Curcubita ssp."

str(C0142_)

#Change some columns to the class "numeric".

for (i in 4:6){
  C0142_ [ ,i] <- as.numeric(C0142_[,i] )
}

summary(C0142_)

#Convert the data. 

effect_c0142 <- fes(C0142_$value, 
                    C0142_$sample_size_control, 
                    C0142_$sample_size_treatment)

class(effect_c0142)
str(effect_c0142)
head(effect_c0142)
tail(effect_c0142)

es_f_d_var2 <- cbind(effect_c0142$g, effect_c0142$var.g) #pick g and var.g 
colnames(es_f_d_var2) <- c("yi", "vi") #name the columns 
es_f_d_var2

C0142_ef <- cbind(C0142_, es_f_d_var2) #bind the data

str(C0142_ef)

#binding

es_F_prontos <- rbind(C0135_ef, C0142_ef) 

class(es_F_prontos)
str(es_F_prontos)


#Convert the t statistic to Hedges' g: 

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

#Let's pick the data.
AG_cont
dado_t <- AG_cont[AG_cont$id_code == "C0143", ]
dado_t
class(dado_t)
str(dado_t)
dado_t$sample_size_control <- as.numeric(dado_t$sample_size_control)
dado_t$sample_size_treatment <- as.numeric(dado_t$sample_size_treatment)

#Convert the data. 
es_t <- tes(dado_t$value, dado_t$sample_size_control,
            dado_t$sample_size_treatment)  
class(es_t)
str(es_t)
es_t_d_var <- cbind(es_t$g, es_t$var.g) #picking the data
es_t_d_var
colnames(es_t_d_var) <- c("yi", "vi") #naming the columns 
es_t_prontos <- cbind(dado_t, es_t_d_var) #binding 
class(es_t_prontos)
str(es_t_prontos)

#Convert the r statistic to Hedges' g:

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

#Let's pick the data.
AG_cont
dado_C0120 <- AG_cont[AG_cont$id_code == "C0120", ]
dado_C0120
dado_C0135 <- AG_cont[AG_cont$id_code == "C0135", ]
dado_C0135
dado_C0135 <- dado_C0135[c(10:12),]
dado_r <- rbind(dado_C0120, dado_C0135)
dado_r

dado_r$total_sample_size <- as.numeric(dado_r$total_sample_size)
dado_r$sample_size_control <- as.numeric(dado_r$sample_size_control)
dado_r$sample_size_treatment <- as.numeric(dado_r$sample_size_treatment)

class(dado_r)
str(dado_r)

#Convert the data.  
es_r <- res(dado_r$value, var.r = NULL, dado_r$total_sample_size)
es_r
str(es_r)
es_r_d_var <- cbind(es_r$d, es_r$var.d) #picking the data
colnames(es_r_d_var) <- c("yi", "vi") #naming the columns 
as.data.frame(es_r_d_var) #we need to convert to Hedges'g 

#Let's make a function to convert hedges' d to hedges' g because the function
#res only provides d and var.d values.

#Pick the data.
d <- es_r$d
d
vd <- es_r$var.d
vd

#df = degrees of freedom, calculated as n1 +n2 - 2.
df <- dado_r$total_sample_size - 2

#Calulate the j from hedge's g 
j <- function(x){
        
        1-3/(4*x-1)
}

Js <- j(df)
Js

#Calculate g.
g <- function(y){
        
        y*d
}

G <- g(Js)
G

#Calculate the variance of g (i.e., var.g).
var.g <- function(z){
        
        Js^2*vd
}

Var.g <- var.g(Js)

#Now, let's bind the data.
es_r_g_var.g <- cbind(G, Var.g)
colnames(es_r_g_var.g) <- c("yi", "vi")
es_r_g_var.g
es_r_prontos <- cbind(dado_r, es_r_g_var.g)  
str(es_r_prontos)

#Bind all the converted values. 
es_cont <- rbind(es_r_prontos, es_F_prontos, es_t_prontos )
str(es_cont)


#OBS:
#There are values of effect size need to be multiplied by -1, because when
# we convert the statistics,the effect size is always positive. In cases in
#which authors reported a negative effect of pesticide application, positive
#values of effect sizes do not make sense. Therefore, in this cases, we need
#to manually multiply the effect size by -1. 

#Let's multiply by -1 the effect sizes of papers that reported a negative 
#effect of pesticide application. 
es_cont$yi[c(1,2,3,4,5,8,9,12,16)] <- 
  es_cont$yi[c(1,2,3,4,5,8,9,12,16)] * -1

class(es_cont)
str(es_cont)
head(es_cont)

#Export the results.
write.csv(es_cont, file = "../Results/effect_sizes_cont.csv", row.names = F)

############################ END ###############################################
