#script tamanhos de efeito e modelo - sobrevivencia de abelhas 


dat <- read.csv("dados_sobrevivencia.csv", h= T, dec =".", sep = ",")

View(dat)

#Calculando os tamanhos de efeito####

#abrindo o pacote necessario
library(metafor)

#como temos os dados na forma de tabela de contigencia, vamos usar odds ratio para calcular a probabilidade de sobrevivencia 
#das abelhas 
#para isso, a gente usa funcao escalc e indica que queremos o resultado em odds ratio ("OR")

effects_t <- escalc("OR", dat$ai,dat$bi, dat$ci, dat$di, dat$n1i, dat$n2i)

View(effects_t)

dat_comp <- cbind(dat, effects_t)

View(dat_comp)

#salvando
write.csv2(dat_comp, "data_comp.csv", row.names = F) #estou salvando assim para fazer a matriz de covariancia filogenetica

write.csv(dat_comp, "data_comp_2.csv", row.names = F)

#agora abrindo a planilha certa

dados <- read.table("data_comp.csv", h=T, dec=",", sep = ";")

View(dados)

#criando matrix de covariancia filogenetica para inserir como variavel aleatoria

install.packages("scales")
install.packages("ape")
install.packages("rotl")

library(scales)
library(ape)
library(rotl)


spp <- tnrs_match_names(unique(dados$bee_specie), context_name = "Animals") #aqui para procurar os nomes das minhas spp de abelhas

#na categoria "Animals" #do Tree of Life

my_tree = tol_induced_subtree(ott_ids=spp$ott_id) #aqui estou criando uma sub arvore com as minhas especies de abelhas 

#daqui para baixo estou vendos as distancias filogeneticas e criando a matriz de covariancia 
otl_tips=strip_ott_ids(my_tree$tip.label, remove_underscores=TRUE)

taxon_map=structure(spp$search_string, names=spp$unique_name)

my_tree$tip.label=taxon_map[otl_tips]

my_tree.ult = compute.brlen(my_tree, method = "Grafen")

windows()
plot(my_tree.ult,no.margin=T) #vamos ver como ficou

cov.matrix = vcv(my_tree.ult,corr=T)
cov.matrix[,0]

#Montando o modelo####

library(metafor)

survival <- rma.mv(yi, vi, random = list( ~1|id_code, ~1|bee_specie, ~1|agrochemical, ~1|hours_after_exposure, ~1|exposure_type), method="REML",  # "REML" = multi-level 
            R = list(bee_specie = cov.matrix),digits = 3, data = dados)   #aqui que entra a matriz de covariancia filogenetica                      


summary(survival)


###Grafico####

#instalando pacotes:

install.packages("dplyr")

install.packages("devtools")
install.packages("tidyverse")
install.packages("metafor")
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

#vamos fazer um teste de sensibilidade porque temos pontos extremos 

#teste de sensibilidade ####

rs= rstandard(survival) #para ver se tem outliers 
hat= hatvalues(survival)/mean(hatvalues(survival))#para ver se tem pontos com alta alavancagem 
plot(hat, rs$resid, ylim = c(-8.0,8), xlim =c(-5,5))
text(hat, rs$resid, labels = sub$id_code, cex= 1, pos = 2)
abline(h = -3)
abline(h = 3)
abline( v = 2)


# Os pontos C0084 e C0191 são outliers (20 pontos). 
# Podemos observar melhor esses pontos vendo o gráfico.  

#modelo sem outliers

surv_sensi<- read.csv("survival_sensi_out.csv", h= T, dec =".", sep = ",")

View(surv_sensi)

model.surv.sensi.out <- rma.mv(yi, vi, random = list( ~1|id_code, ~1|bee_specie, ~1|agrochemical, ~1|hours_after_exposure, ~1|exposure_type), method="REML",  # "REML" = multi-level 
                                           R = list(bee_specie = cov.matrix),digits = 3, control = list(optimizer="optim", optmethod="Nelder-Mead"), data = surv_sensi)

summary(model.surv.sensi.out)

orchard_plot(model.surv.sensi.out, xlab = "Odds ratio") +
        labs(y = "Survival") + # troca o nome do eixo y
        scale_color_manual(values = "pink1") + #troca a cor dos pontos do fundo
        scale_fill_manual(values = "seagreen3") + #troca a cor do ponto central 
        theme_classic() + #tira a caixa em volta do grafico
        theme(axis.text = element_text(size = 14, colour = "black"), #muda o tamanho do texto dos eixos e na cor do eixo 
              axis.title = element_text(size = 16), #muda o tamanho do titulo do eixo
              axis.text.x = element_blank(), #Isso remove o texto automatico do eixo y
              legend.position = "top") + #troca a posicao da legenda 
        coord_flip() 

#continuou negativo e significativo (-1.878; p = 0.022) 
# isso indica que o efeito do agrotóxico é forte o bastante para não deixar de ser detectado mesmo quando o tamanho amostral é menor.


###Heterogeneidades####

dados$wi <- 1/sqrt(dados$vi) # precision = 1 / standard error of effect size (Equation 20; Nakagawa & Santos 2012)

s2m.0 <- sum(dados$wi*(length(dados$wi)-1))/(sum(dados$wi)^2-sum(dados$wi^2)) # Equation 22


#vamos calcular a heterogeneidade TOTAL, ou seja, a heterogeneidade explicada pelo modelo
#incluindo preditores e variaveis respostas 

I2.survival <- ((survival$sigma2[1] + survival$sigma2[2] +  survival$sigma2[3] + survival$sigma2[4] + survival$sigma2[5])/ #[1] porque eu tenho o fator aleatorio id_code e [2] pq tem o fator aleatoria bee_specie. 
                 (survival$sigma2[1] + survival$sigma2[2] +  survival$sigma2[3] + survival$sigma2[4] + survival$sigma2[5] + s2m.0)) * 100 

#o sigma foi dado no summary do modelo. Ele indica a heterogenidade, mas de 
#forma nao intuitiva. Entao a gente calcula o I2 porque a heterogenidade pode
#pode ser interpretada como uma porcentagem 

I2.survival #heterogeneidade media

#meu modelo tem 90% de heterogeneidade. 
#em ecologia, geralemnte a heterogeneidade > 70%


## and 95% CI for I2.total:

I2.survival - qchisq(.95, df=1)/2; I2.survival + qchisq(.95, df=1)/2


#Calculando I² para cada fator aleatorio

#--- study ID ---

I2.study.bee <- ((survival$sigma2[1])/(
        survival$sigma2[1]+ survival$sigma2[2] +  survival$sigma2[3]  +  survival$sigma2[4] + survival$sigma2[5] +
                s2m.0)) * 100

I2.study.bee

## and 95% CI for I2.paper:

I2.study.bee - qchisq(.95, df=1)/2; I2.study.bee + qchisq(.95, df=1)/2

#--- bee_specie ---

I2.bee <- ((survival$sigma2[2])/(
        survival$sigma2[1]+ survival$sigma2[2] +  survival$sigma2[3] + survival$sigma2[4] + survival$sigma2[5] +
                s2m.0)) * 100

I2.bee

## and 95% CI for I2.paper:
I2.bee - qchisq(.95, df=1)/2; I2.bee + qchisq(.95, df=1)/2

#--- agrochemical ---
I2.agro<- ((survival$sigma2[3])/(
        survival$sigma2[1]+survival$sigma2[2] +  survival$sigma2[3] + survival$sigma2[4] + survival$sigma2[5] +
                s2m.0)) * 100

I2.agro

## and 95% CI for I2.paper:
I2.agro - qchisq(.95, df=1)/2; I2.agro + qchisq(.95, df=1)/2

#----hours

I2.hours<- ((survival$sigma2[4])/(
        survival$sigma2[1]+survival$sigma2[2] +  survival$sigma2[3] + survival$sigma2[4] + survival$sigma2[5] +
                s2m.0)) * 100

I2.hours

## and 95% CI for I2.paper:
I2.hours - qchisq(.95, df=1)/2; I2.hours + qchisq(.95, df=1)/2

#---exposure type

I2.exp<- ((survival$sigma2[5])/(
        survival$sigma2[1]+survival$sigma2[2] +  survival$sigma2[3] + survival$sigma2[4] + survival$sigma2[5] +
                s2m.0)) * 100

I2.exp

## and 95% CI for I2.paper:
I2.exp - qchisq(.95, df=1)/2; I2.exp + qchisq(.95, df=1)/2

#intervalos de confiaca sigma2

confint(survival)

#Vies de publicacao####

#Publication bias (egger)
#Intercept significativo indica que ha vies de publicacao


eggsm3 = lm(residuals(survival)~sqrt(dados$vi)) 

summary(eggsm3) #tem vies de publicacao pq o intercepto deu diferente de zero e deu significativo 

confint.lm(eggsm3, level = 0.95)

##############################################################################

