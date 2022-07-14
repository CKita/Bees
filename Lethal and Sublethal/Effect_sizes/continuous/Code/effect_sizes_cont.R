## CONVERTENDO AS ESTATISTICAS PARA HEDGES 


#baixando o pacaote para fazer as conversoes

install.packages("compute.es")

library(compute.es )

#vamos abrir a planilha dos dados continuos

AG_cont <- read.csv("continuous.csv", h=T, dec = ",")

View(AG_cont)

summary(AG_cont)

#arrumando a classe da coluna "value" para numeric 

AG_cont$value <- as.numeric(AG_cont$value)


#agora vamos comecar as conversoes das estatisticas 

#convertendo F

#fes(f, n.1, n.2)

#f =  valor da estatastica
#n.1 =  tamanho amostral do tratamento
#n.2 = o tamanho amostral do controle 


#entao vou pegar os dados que tem a estatistica F
AG_cont

C0135_ <- AG_cont[AG_cont$id_code == "C0135", ]

C0135_ <- C0135_[c(1:9),]

C0135_

C0179_ <- AG_cont[AG_cont$id_code ==  "C0179", ]

C0179_

#vamos juntar esses dados 
C0135_C0179 <- rbind(C0135_, C0179_)

C0135_C0179

summary(C0135_C0179)

#tranformando para a classe numeric

for (i in 4:6){
        C0135_C0179 [ ,i] <- as.numeric(C0135_C0179[,i] )
}

C0135_C0179$value <- as.numeric(C0135_C0179$value)

summary(C0135_C0179)

#convertendo 

es_f <- fes(C0135_C0179$value, C0135_C0179$sample_size_control, C0135_C0179$sample_size_treatment)

View(es_f)

es_f_g_var <- cbind(es_f$g, es_f$var.g) #pegando as colunas que me interessam 

colnames(es_f_g_var) <- c("yi", "vi") #nomeando 

View(es_f_g_var)

es_F_prontos <- cbind(C0135_C0179, es_f_g_var) #juntando os tamanhos de efeito com os outros dados 

View(es_F_prontos)


#convertendo a estatistica t

#tes(t, n.1, n.2)
#t = t-test value reported in primary study.
#n.1 = Sample size of treatment group.
#n.2 = Sample size of comparison group

AG_cont

dado_t <- AG_cont[AG_cont$id_code == "C0143", ]
dado_t

class(dado_t)
str(dado_t)

dado_t$sample_size_control <- as.numeric(dado_t$sample_size_control)
dado_t$sample_size_treatment <- as.numeric(dado_t$sample_size_treatment)

#convertendo 

es_t <- tes(dado_t$value, dado_t$sample_size_control, dado_t$sample_size_treatment)  

View(es_t)

es_t_d_var <- cbind(es_t$g, es_t$var.g) #pegando as colunas que me interessam

es_t_d_var

colnames(es_t_d_var) <- c("yi", "vi") #nomeando as colunas 

es_t_prontos <- cbind(dado_t, es_t_d_var) #juntando os tamanhos de efeito com os outros dados 

View(es_t_prontos)


#agora convertendo a estatistica r

#res(r, var.r = NULL, n)
#r = Correlation coefficient.
#var.r = Variance of r. If value is not reported then leave it blank and variances will be computed based on sample size. 
#n = Total sample size.                                                            

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

#convertendo 

es_r <- res(dado_r$value, var.r = NULL, dado_r$total_sample_size)

View(es_r)

es_r_d_var <- cbind(es_r$d, es_r$var.d) #pegando as colunas que me interessam

colnames(es_r_d_var) <- c("yi", "vi") #nomeando as colunas 

as.data.frame(es_r_d_var) #tem que passar para hedges g


#vou fazer uma funcao para conversao de hedges'd para hedges'g porque o R nao esta dando o valor de g e var.g

#pegando as informacoes necessarias para fazer a conversao
d <- es_r$d
d

vd <- es_r$var.d
vd

#df= degrees of freedom, which is n1 +n2 - 2 
df <- dado_r$total_sample_size - 2


#para calcular o j do hedge's g 

j <- function(x){
        
        1-3/(4*x-1)
}

Js <- j(df)
Js

#para calcular o g

g <- function(y){
        
        y*d
}

G <- g(Js)

#para calcular a variancia de g

var.g <- function(z){
        
        Js^2*vd
}


Var.g <- var.g(Js)

#agora juntando 

es_r_g_var.g <- cbind(G, Var.g)

colnames(es_r_g_var.g) <- c("yi", "vi")

es_r_g_var.g

es_r_prontos <- cbind(dado_r, es_r_g_var.g) #juntando os tamanhos de efeito com os outros dados 

View(es_r_prontos)


#juntando todos os dados convertidos

es_cont <- rbind(es_r_prontos, es_F_prontos, es_t_prontos )

View(es_cont)

es_cont <- es_cont[-18,] #vi que esse dado nao estava atrelado a polinzacao por abelhas

#alguns dos valores tiveream que ser multiplicados por -1 ja que quando eu faco a conversao das estatisticas para
#valores de tamanho de efeito, todos os valores sao positivos, o que nao refletem o efeito negativo dos pesticidas, 
#mas valores positivos nao condizem com o resultado dos artigos.
#entao nesses casos, a gente tem que colocar o sinal manualmente.
#para ver quais valores eu tinha que multiplicar por -1, eu voltei nos artigos originais e vi o que os autores falaram sobre o
#efeito do pesticida. Se ele foi negativo, eu multipliquei por -1 

es_cont$yi[c(1,2,3,4,5,8,9,12,14,15,18)] <- es_cont$yi[c(1,2,3,4,5,8,9,12,14,15,18)] * -1

View(es_cont)

#salvando a planilha pronta

write.csv(es_cont, file = "effect_sizes_cont.csv", row.names = F)

write.table(es_cont, file = "effect_sizes_cont.txt", row.names = F, col.names = T, dec = ".", sep = ",")
