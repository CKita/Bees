#script dos modelos 


#COMUNIDADE E POLINIZACAO#


#carregando os dados

dados_completos <- read.csv("let_sublet.csv", h= T, dec =".", sep = ",")

View(dados_completos)

str(dados_completos)

dados_completos$nature_x <- as.factor(dados_completos$nature_x) 
dados_completos$study_type <- as.factor(dados_completos$study_type)
dados_completos$effect_type <- as.factor(dados_completos$effect_type)
dados_completos$pesticide_effect <- as.factor(dados_completos$pesticide_effect)

table(dados_completos$pesticide_effect)
table(dados_completos$study_type)

#estimando o tamanho de efeito medio (mean difference: treatment - control; HEDGES'G)

#control = without pesticide
#treatment = with pesticide

#Abrindo o pacote metafor
library(metafor)


#vamos para os modelos:


#### MODELO GERAL ####

#nesse modelo vejo o efeito geral dos agrotoxicos sobre as abelhas e sobre o servico de polinizacao prestado por elas

#variaveis aleatorias:

#id_code = id
#study_type = field or lab
#sampling_method = pan_trap, net, video_record, observation, directly_from_the_colony
#nao coloquei tipo de fazenda como variavel aleatoria porque nao pode conter NA


model.geral <- rma.mv(yi, vi, random = list( ~1|id_code, ~1|study_type, ~1|sampling_method), method="REML",  # "REML" = multi-level 
                      digits = 3, data = dados_completos)

summary(model.geral)

#interpretando o summary do modelo geral:

##o tamanho de efeito medio estimado deu um valor negativo e significativo 
# d = -0.638; 95% CI: -1.232 - -0.044; p = 0.035 
#isso quer dizer que a aplicacao dos agrotoxicos e prejudicial para as abelhas e para a polinizacao das lavouras
#pois, em media, a comunidade de abelhas e suas atividades sao maiores no grupo controle (sem agrotoxicos). 
#tamanho de efeito medio deu negativo e moderado (d = 0.2, small effect size ; d = 0.5, medium effect size; d = 0.8; large effect size)

#agora vamos ver esse resultado graficamente 


#### Grafico do modelo geral ####

#usando os pacotes "orchaRd" e "ggplot2"

#instalando pacotes:
install.packages("devtools")
install.packages("tidyverse")
install.packages("metafor")
install.packages("patchwork")
install.packages("R.rsp")
install.packages("ggplot2")

devtools::install_github("itchyshin/orchard_plot", subdir = "orchaRd", force = TRUE, build_vignettes = TRUE)

library(orchaRd)
library(patchwork)
library(tidyverse)
library(metafor)
library(ggplot2)

#fazendo o grafico

tiff('overall.tiff', units="in", width=5, height=6, res=1200, compression = 'lzw')

orchard_plot(model.geral, xlab = "Hedges' g") +
        labs(y = "Overall effect") + # troca o nome do eixo y
        scale_color_manual(values = "pink1") + #troca a cor dos pontos do fundo
        scale_fill_manual(values = "seagreen3") + #troca a cor do ponto central 
        theme_classic() + #tira a caixa em volta do grafico
        theme(axis.text = element_text(size = 14, colour = "black"), #muda o tamanho do texto dos eixos e na cor do eixo 
              axis.title = element_text(size = 16), #muda o tamanho do titulo do eixo
              axis.text.x = element_blank(), #Isso remove o texto automatico do eixo y
              legend.position = "top") + #troca a posicao da legenda 
        coord_flip() #para colocar a variavel resposta no y 

dev.off()
###HETEROGENEIDADE - moldelo geral

#Heterogeneidade (I?) do modelo geral ####

dados_completos$wi <- 1/sqrt(dados_completos$vi) # precision = 1 / standard error of effect size (Equation 20; Nakagawa & Santos 2012)

s2m.0 <- sum(dados_completos$wi*(length(dados_completos$wi)-1))/(sum(dados_completos$wi)^2-sum(dados_completos$wi^2)) # Equation 22


I2.model.g <- ((model.geral$sigma2[1] + model.geral$sigma2[2] + model.geral$sigma2[3] ))/
        (model.geral$sigma2[1]+  model.geral$sigma2[2] + model.geral$sigma2[3] +s2m.0) * 100

I2.model.g

## and 95% CI for I2.total:
I2.model.g - qchisq(.95, df=1)/2; I2.model.g + qchisq(.95, df=1)/2

#Calculando I? para cada fator aleatorio

#--- ID code ---

I2.id <- ((model.geral$sigma2[1])/
                  (model.geral$sigma2[1] + model.geral$sigma2[2] + model.geral$sigma2[3] + 
                           s2m.0)) * 100

I2.id

## and 95% CI for I2.id:

I2.id - qchisq(.95, df=1)/2; I2.id + qchisq(.95, df=1)/2


#--- study type ---

I2.study <- ((model.geral$sigma2[2])/
                     (model.geral$sigma2[1] + model.geral$sigma2[2] + model.geral$sigma2[3] + 
                              s2m.0)) * 100

I2.study

## and 95% CI for I2.study_type:

I2.study - qchisq(.95, df=1)/2; I2.study + qchisq(.95, df=1)/2


#--- sampling method ---

I2.sampling <- ((model.geral$sigma2[3])/
                        (model.geral$sigma2[1] + model.geral$sigma2[2] + model.geral$sigma2[3] + 
                                 s2m.0)) * 100

I2.sampling

## and 95% CI for I2.sampling:

I2.sampling - qchisq(.95, df=1)/2; I2.sampling + qchisq(.95, df=1)/2

#calculando os intervalos de confianca dos sigmas2

confint(model.geral)

#agora vamos ver o vies de publicacao


#VIES - modelo geral

#Vies de publicacao: Egger modelo geral####

#calculando vies de publicacao

egger = lm(residuals(model.geral)~sqrt(dados_completos$vi)) 

summary(egger) #tem vies de publicacao pq o intercepto deu diferente de zero e deu significativo 

confint.lm(egger, level = 0.95)


#################################################################

### MODELO 01 - Tipo de efeito dos agrotoxicos como moderador####

#colocando o pesticide_effect como moderador. Assim, consigo ver o efeito letais e sub-letais dos agrotoxicos na comunidade e na polinizacao separadamente
#pesticide_effect = lethal or sub-lethal  

table(dados_completos$study_type)

model.1 <- rma.mv(yi, vi, mods= ~pesticide_effect, random = list( ~1|id_code, ~1|study_type, ~1|sampling_method), method="REML",  # "REML" = multi-level 
                  digits = 3, data = dados_completos)


summary(model.1) 


#interpretando o summary do modelo 1:

#o tamanho de efeito medio estimado para efeitos letais deu um valor negativo, mas nao significativo 
# g = -0.292; 95% CI: -0.799 to 0.215; p = 0.259


#o tamanho de efeito medio para efeitos sub-letais deu um valor negativo, mas nao significativo
#g = -0.590: 95% CI: -1.256 - 0.077: p = 0.083


#agora vamos fazer o grafico

#grafico do modelo 1####

# Para construir um grafico com moderador, precisamos de um modelo sem o intercepto. 
#Para isso, basta criar o modelo e mods = moderador-1


model.1f <- rma.mv(yi, vi, mods = ~pesticide_effect -1, random = list( ~1|id_code, ~1|study_type, ~1|sampling_method), method="REML",  # "REML" = multi-level 
                   digits = 3, data = dados_completos)

summary(model.1f)

#d?vida: por que no modelo sem moderador, o efeito sub-letail deu n?o significativo e nesse modelo com o moderador ele deu significativo?

#fazendo o grafico

tiff('let_sublet.tiff', units="in", width=5, height=6, res=1200, compression = 'lzw')


orchard_plot(model.1f, mod = "pesticide_effect", xlab = "Hedges' g") +
        labs(y = " ") +
        scale_fill_manual(values = c( "slateblue1", "sienna1")) +
        theme_classic() + 
        theme(axis.text = element_text(size = 14, colour = "black"),
              axis.title = element_text(size = 16),
              legend.position = "top") +
        coord_flip() +
        scale_y_discrete(labels = c("Lethal", "Sublethal"))

dev.off()

#Heterogeneidade modelo 1####

#calculando a heterogeneidade (I?) do modelo, ou seja, heterogeneidade total do modelo 1 

dados_completos$wi <- 1/sqrt(dados_completos$vi) # precision = 1 / standard error of effect size (Equation 20; Nakagawa & Santos 2012)

s2m.0 <- sum(dados_completos$wi*(length(dados_completos$wi)-1))/(sum(dados_completos$wi)^2-sum(dados_completos$wi^2)) # Equation 22


I2.model.1 <- ((model.1$sigma2[1] + model.1$sigma2[2] + model.1$sigma2[3] ))/
        (model.1$sigma2[1]+ + model.1$sigma2[2] + model.1$sigma2[3] +s2m.0) * 100

I2.model.1

## and 95% CI for I2.total:
I2.model.1 - qchisq(.95, df=1)/2; I2.model.1 + qchisq(.95, df=1)/2


#Calculando I? para cada fator aleatorio

#--- ID code ---

I2.id.1 <- ((model.1$sigma2[1])/
                    (model.1$sigma2[1] + model.1$sigma2[2] + model.1$sigma2[3] + 
                             s2m.0)) * 100

I2.id.1

## and 95% CI for I2.id:

I2.id.1 - qchisq(.95, df=1)/2; I2.id.1 + qchisq(.95, df=1)/2


#--- study type ---

I2.study.1 <- ((model.1$sigma2[2])/
                       (model.1$sigma2[1] + model.1$sigma2[2] + model.1$sigma2[3] + 
                                s2m.0)) * 100

I2.study.1

## and 95% CI for I2.study_type:

I2.study.1 - qchisq(.95, df=1)/2; I2.study.1 + qchisq(.95, df=1)/2



#--- sampling method ---

I2.sampling.1 <- ((model.1$sigma2[3])/
                          (model.1$sigma2[1] + model.1$sigma2[2] + model.1$sigma2[3] + 
                                   s2m.0)) * 100

I2.sampling.1

## and 95% CI for I2.sampling:

I2.sampling.1 - qchisq(.95, df=1)/2; I2.sampling.1 + qchisq(.95, df=1)/2

#calculando os intervalos de confianca dos sigmas2

confint(model.1)


#Vies de publicacao modelo 1####


#calculando vies de publicacao

egger2 = lm(residuals(model.1)~sqrt(dados_completos$vi)) 

summary(egger2) #tem vies de publicacao pq o intercepto deu diferente de zero e deu significativo 

confint.lm(egger2, level = 0.95)


######

#### sensibility test: sub_lethal ####

#vamos fazer um teste de sensibilidade para os efeitos subletais porque ele apresenta pontos extremos. 
#O que n?o ocorre nos efeitos letais 

sub <- read.csv("sub.csv", h= T, dec =".", sep = ",")

View(sub)

model.sub <- rma.mv(yi, vi, random = list( ~1|id_code, ~1|study_type, ~1|sampling_method), method="REML",  # "REML" = multi-level 
                    digits = 3, data = sub)

summary(model.sub)

#grafico

orchard_plot(model.sub, xlab = "Hedges' g") +
        labs(y = "Overall effect") + # troca o nome do eixo y
        scale_color_manual(values = "pink1") + #troca a cor dos pontos do fundo
        scale_fill_manual(values = "seagreen3") + #troca a cor do ponto central 
        theme_classic() + #tira a caixa em volta do grafico
        theme(axis.text = element_text(size = 14, colour = "black"), #muda o tamanho do texto dos eixos e na cor do eixo 
              axis.title = element_text(size = 16), #muda o tamanho do titulo do eixo
              axis.text.x = element_blank(), #Isso remove o texto automatico do eixo y
              legend.position = "top") + #troca a posicao da legenda 
        coord_flip() 


#teste de sensibilidade

rs= rstandard(model.sub) #para ver se tem outliers 
hat= hatvalues(model.sub)/mean(hatvalues(model.sub))#para ver se tem pontos com alta alavancagem 
plot(hat, rs$resid, ylim = c(-8.0,8), xlim =c(-5,5))
text(hat, rs$resid, labels = sub$id_code, cex= 1, pos = 2)
abline(h = -3)
abline(h = 3)
abline( v = 2)


#os pontos: C0180, C0143 e C0144 possuem alta alavancagem (4 pontos); os pontos C0091 s?o outliers (4 pontos)

#modelo sem os pontos com alta alavancagem 

sub.sensi.alav <- read.csv("sub_sensi_alav.csv", h= T, dec =".", sep = ",")

View(sub.sensi.alav)

model.sub.sensi.alav <- rma.mv(yi, vi, random = list( ~1|id_code, ~1|study_type, ~1|sampling_method), method="REML",  # "REML" = multi-level 
                              digits = 3, data = sub.sensi.alav)

summary(model.sub.sensi.alav)

orchard_plot(model.sub.sensi.alav, xlab = "Hedges' g") +
        labs(y = "Overall effect") + # troca o nome do eixo y
        scale_color_manual(values = "pink1") + #troca a cor dos pontos do fundo
        scale_fill_manual(values = "seagreen3") + #troca a cor do ponto central 
        theme_classic() + #tira a caixa em volta do grafico
        theme(axis.text = element_text(size = 14, colour = "black"), #muda o tamanho do texto dos eixos e na cor do eixo 
              axis.title = element_text(size = 16), #muda o tamanho do titulo do eixo
              axis.text.x = element_blank(), #Isso remove o texto automatico do eixo y
              legend.position = "top") + #troca a posicao da legenda 
        coord_flip() 

#continuou negativo e significativo (-0.941; p = 0.015) - overall

#sem outliers 

sub.sensi.out <- read.csv("sub_sensi_out.csv", h= T, dec =".", sep = ",")

View(sub.sensi.out)

model.sub.sensi.out <- rma.mv(yi, vi, random = list( ~1|id_code, ~1|study_type, ~1|sampling_method), method="REML",  # "REML" = multi-level 
                    digits = 3, data = sub.sensi.out)

summary(model.sub.sensi.out)

orchard_plot(model.sub.sensi.out, xlab = "Hedges' g") +
        labs(y = "Overall effect") + # troca o nome do eixo y
        scale_color_manual(values = "pink1") + #troca a cor dos pontos do fundo
        scale_fill_manual(values = "seagreen3") + #troca a cor do ponto central 
        theme_classic() + #tira a caixa em volta do grafico
        theme(axis.text = element_text(size = 14, colour = "black"), #muda o tamanho do texto dos eixos e na cor do eixo 
              axis.title = element_text(size = 16), #muda o tamanho do titulo do eixo
              axis.text.x = element_blank(), #Isso remove o texto automatico do eixo y
              legend.position = "top") + #troca a posicao da legenda 
        coord_flip() 

#continuou negativo e diferente de zero. Ficou mais negativo (-0.780; p <.001)
#isso indica que o efeito deve mesmo ser resultado de um processo biol?gico e n?o um artefato causado por esses outliers

