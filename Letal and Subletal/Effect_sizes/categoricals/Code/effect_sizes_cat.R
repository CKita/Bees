#TAMANHOS DE EFEITO - DADOS CATEGORICOS 

#instalando os pacotes metafor e o compute.es

install.packages("metafor")
install.packages("compute.es")

#abrindo os pacotes
library("metafor")
library("compute.es")

#explorando o metafor
?metafor

#vamos usar a funcao escalc para calcular os tamanhos de efeito

?escalc

#como vamos calcular os tamanhos de efeito por Hedges'g, vamos usar "Measures for Quantitative Variables",
# e o argumento para "standardized mean difference" = "SMD"

#carregadndo os dados
AG_cat <- read.csv("cat.csv", h=T, dec = ",")

View(AG_cat)

summary(AG_cat)

#arrumando a classe das colunas

for (i in 4:10){
        AG_cat[ ,i] <- as.numeric(AG_cat[,i] )
}

summary(AG_cat)


####agora vamos calcular os tamanhos de efeito####

#lembrando que estamos fazendo tratamento(com agrotoxico) - controle (sem agrotoxico),
#entao valores de hedges (yi) negativos indicam efeito negativo do agrotoxico 

#nesse caso, para a funcao escalc dar certo, temos que entrar com as seguintes informacoes:

#m1i = mean_treatment
#m2i = mean_control
#sd1i = sd_treatment
#sd2i = sd_control
#n1i = sample_size_treatment
#n2i = sample_size_control

#entao vamos fazer isso

effect_sizes <- escalc("SMD", m1i = mean_treatment, m2i = mean_control, 
                       sd1i = sd_treatment, sd2i= sd_control, 
                       n1i=sample_size_treatment, n2i= sample_size_control, 
                       data = AG_cat)

View(effect_sizes) #tamanho de efeito (yi) e variancia (vi) 

#vamos salvar essa planilha com os tamanhos de efeito

write.table(effect_sizes, "just_effect_sizes_cat.txt", dec = ".", sep = ",", row.names = F )

write.csv(effect_sizes, "just_effect_sizes_cat.csv", row.names = F)


#o dado do id C0142 da para ser utilizado, pois embora nao tenha sido dado informacoes
#para calcular o tamanho de efeito pelas medias e sds, foi dado a estatistica F que pode ser 
#convertida para hedges. Entao vamos aproveitar o dado

d_brutos <- read.csv("planilha_bruta.csv", h =T, dec= ",")

C0142_BRUTO <- d_brutos[d_brutos$id_code == "C0142", ]

summary(C0142_BRUTO)

View(C0142_BRUTO)
str(C0142_BRUTO)

C0142_BRUTO$value[1]<- 0.62
C0142_BRUTO$value <- as.numeric(C0142_BRUTO$value)

C0142_BRUTO$value

#calculando o tamanho de efeito hedges a partir da estatistica F 
#para isso vamos utilizar o pacote compute.es, especificamente a funcao fes

effect_c0142 <- fes(C0142_BRUTO$value, C0142_BRUTO$sample_size_control, C0142_BRUTO$sample_size_treatment)

View(effect_c0142)

es_f_d_var <- cbind(effect_c0142$d, effect_c0142$var.d) #pegando as colunas que me interessam 

colnames(es_f_d_var) <- c("yi", "vi") #nomeando 

es_f_d_var

View(es_f_d_var)


C0142_stat <- cbind (C0142_BRUTO$id_code, C0142_BRUTO$study_type, C0142_BRUTO$nature_x, 
                     C0142_BRUTO$total_sample_size , C0142_BRUTO$sample_size_control, 
                     C0142_BRUTO$sample_size_treatment, C0142_BRUTO$statistic, C0142_BRUTO$value)

colnames(C0142_stat) <- c("id_code", "study_type", "nature_x", "total_sample_size", "sample_size_control", "sample_size_treatment", "statistic", "value")

es_F_prontos <- cbind(C0142_stat, es_f_d_var) #juntando os tamanhos de efeito com os outros dados 

es_F_prontos

effect_type <- as.data.frame(rep("community", 2))

plant_specie <- as.data.frame(rep("", 2))

colnames(effect_type) <- "effect_type"
colnames(plant_specie) <- "plant_specie"

plant_specie

ef_plant <- cbind(effect_type, plant_specie)

pronto <- cbind(es_F_prontos, ef_plant)

View(pronto)

str(pronto)

#arrumando as classes das colunas

for (i in 4:6){
        pronto [ ,i] <- as.numeric(pronto[,i] )
}

str(pronto)

for (i in 8:10){
        pronto [ ,i] <- as.numeric(pronto[,i] )
}

str(pronto)

#vamos juntar entao esse dado C0142 com o restante


str(effect_sizes)

str(pronto)

effects_prontos <- dplyr::bind_rows( effect_sizes, pronto)

View(effects_prontos)
str(effects_prontos)

#salvando 

#em txt
write.table(effects_prontos, "effect_sizes_cat.txt", dec = ".", sep = ",", row.names = F )

#em csv 

write.csv(effects_prontos, "effect_sizes_cat.csv", row.names = F)
