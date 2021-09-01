#pacotes para usar
library(tidyverse)
library(gt)
library(readxl)

#carregando dados do RN, mortes por causa, mortes tot, pop residente----
todos.dados<-read_xlsx("trabalho final.xlsx", sheet = "MASCULINO.R")

#criando o intervalo
n<-c(1,4,rep(5,10))

#idade
idade=c(0,1,seq(5,20,5),seq(30,80,10))

#total de mortes
mortes.t<-todos.dados$total

#pop resi
expos<-todos.dados$pop.resi

#tabua de vida masculina ----
dados<-data.frame(idade, n, mortes.t,expos)

##Estimando as taxas espec?ficas de mortalidade, nMx, para todas as mortes
dados$nMx<-round(dados$mortes.t/dados$expos, digits=5)


head(dados);tail(dados)

### Estimando os nax###
dados$nax<-1

## Estimando os nax a partir da idade de 5 anos at? o penultimo grupo de idade
for(i in 3:11){dados$nax[i]<-round(2.5,digits=2)}

##Para o ?ltimo grupo de idade da t?bua. No nosso caso 85+
dados$nax[12]<-round(1/dados$nMx[12],digits=2)

## ax para os primeiros grupos de idade - homens                               
dados$nax[1]<-round(ifelse(dados$nMx[1]<0.107,0.045+2.684*dados$nMx[1],0.330), digits=2)  
dados$nax[2]<-round(ifelse(dados$nMx[1]<0.107,1.651-2.816*dados$nMx[1],1.352), digits=2)  


## Estimando as probabilidades de morte nqx
for (i in 1:11){dados$nqx[i]<-(dados$n[i]*dados$nMx[i])/
  ((1)+((dados$n[i]-dados$nax[i])*dados$nMx[i]))}


##A probabilidade de morte para ?ltima idade da t?bua
dados$nqx[12]<-1

##EStimando as probabilidades de sobreviv?ncia npx
dados$npx<-(1-dados$nqx)

##Criando uma coorte hipotetica e aplicando as probabilidades de sobrevivencia para gerar o lx
dados$lx<-100000 #estabelecendo o raiz da tabua

##para os demais grupos de idade
for (i in 2:12){dados$lx[i]<-(dados$npx[i-1]*dados$lx[i-1])}

##Estimando o n?mero de mortes de tabela ndx
for (i in 1:11){dados$ndx[i]<-dados$lx[i]-dados$lx[i+1]}

##Para o ?ltimo grupo de idade
dados$ndx[12]<-dados$lx[12]

## Estimando o pessoas-ano vivido da tabela de vida Lx
##Todos que viveram ate o fim do intervalo + aqueles que viveram certo tempo e morreram antes de completar a ultima idade do intervalo

for (i in 1:11){dados$nLx[i]<-((dados$n[i]*dados$lx[i+1])+(dados$ndx[i]*dados$nax[i]))}

##Para o ultimo grupo de idade mortes/taxa
dados$nLx[12]<-dados$ndx[12]/dados$nMx[12]

##Estimando o Tx - pessoas-ano acima da idade x
dados$Tx<-(rev(cumsum(rev(dados$nLx))))


##Estimando a esperan?a de vida ex = Tx/lx
dados$ex<-round(dados$Tx/dados$lx,digits=2)

tabua.masculina=dados
tabua.masculina

## Vamos estimar a esperanca de vida caso infecaco fosse eliminada nos homems
#infecao----

## Proporcao de mortes, sem infeccao
p.no.infec=(dados$mortes.t-todos.dados$Infecção)/dados$mortes.t
p.no.infec

tabua.s.infec=data.frame(cbind(idade,n,p.no.infec))

## Extraindo da probabilidade de sobrevivencia essa proporcao = px^proporcao
tabua.s.infec$npx.n=tabua.masculina$npx^tabua.s.infec$p.no.infec

## Novo nqx = 1-npx
tabua.s.infec$nqx.n=1-tabua.s.infec$npx.n

##Criando uma coorte hipotetica e aplicando as probabilidades de sobrevivencia para gerar o lx
tabua.s.infec$lx<-100000 #estabelecendo o raiz da tabua

##para os demais grupos de idade
for (i in 2:12){tabua.s.infec$lx[i]<-(tabua.s.infec$npx.n[i-1]*tabua.s.infec$lx[i-1])}

##Estimando o n?mero de mortes de tabela ndx
for (i in 1:11){tabua.s.infec$ndx[i]<-tabua.s.infec$lx[i]-tabua.s.infec$lx[i+1]}

##Para o ultimo grupo de idade
tabua.s.infec$ndx[12]<-tabua.s.infec$lx[12]




#nax novo




## Para os grupos de idade 0 a 9 e penultimo da tabua
## = n + p.sem.causa*(nqx/nqx.n)*(nax-n)
tabua.s.infec$nax.n=tabua.s.infec$n+tabua.s.infec$p.no.infec*(tabua.masculina$nqx/tabua.s.infec$nqx.n)*(tabua.masculina$nax-tabua.s.infec$n)

## Para os demais grupos com intervalo de 5 anos, assume-se que as mortes seguem uma funcao quadratica sobre os intervalos x-5 a x+10, assim:

## No nosso caso, 
for (i in 4:10){tabua.s.infec$nax.n[i]=((tabua.s.infec$ndx[i-1]*-5/24)+2.5*tabua.s.infec$ndx[i]+(tabua.s.infec$ndx[i+1]*5/24))/tabua.s.infec$ndx[i]}

# para o ultimo grupo
tabua.s.infec$nax.n[12]=tabua.masculina$ex[12]/tabua.s.infec$p.no.infec[12]

## Estimando o pessoas-ano vivido da tabela de vida Lx
##Todos que viveram ate o fim do intervalo + aqueles que viveram certo tempo e morreram antes de completar a ultima idade do intervalo
for (i in 1:11){tabua.s.infec$nLx[i]<-((tabua.s.infec$n[i]*tabua.s.infec$lx[i+1])+(tabua.s.infec$ndx[i]*tabua.s.infec$nax.n[i]))}

##Para o ultimo grupo de idade mortes/taxa
tabua.s.infec$nLx[12]<-tabua.s.infec$ndx[12]*tabua.s.infec$nax.n[12]


##Estimando o Tx - pessoas-ano acima da idade x
tabua.s.infec$Tx<-(rev(cumsum(rev(tabua.s.infec$nLx))))


##Estimando a esperan?a de vida ex = Tx/lx
tabua.s.infec$ex.n<-round(tabua.s.infec$Tx/tabua.s.infec$lx,digits=2)


#neoplasma----
## Proporcao de mortes, sem neoplasma
p.no.neo=(dados$mortes.t-todos.dados$neoplasmas)/dados$mortes.t
p.no.neo

tabua.s.neo=data.frame(cbind(idade,n,p.no.neo))

## Extraindo da probabilidade de sobrevivencia essa proporcao = px^proporcao
tabua.s.neo$npx.n=tabua.masculina$npx^tabua.s.neo$p.no.neo

## Novo nqx = 1-npx
tabua.s.neo$nqx.n=1-tabua.s.neo$npx.n

##Criando uma coorte hipotetica e aplicando as probabilidades de sobrevivencia para gerar o lx
tabua.s.neo$lx<-100000 #estabelecendo o raiz da tabua

##para os demais grupos de idade
for (i in 2:12){tabua.s.neo$lx[i]<-(tabua.s.neo$npx.n[i-1]*tabua.s.neo$lx[i-1])}

##Estimando o n?mero de mortes de tabela ndx
for (i in 1:11){tabua.s.neo$ndx[i]<-tabua.s.neo$lx[i]-tabua.s.neo$lx[i+1]}

##Para o ultimo grupo de idade
tabua.s.neo$ndx[12]<-tabua.s.neo$lx[12]




#nax novo




## Para os grupos de idade 0 a 9 e penultimo da tabua
## = n + p.sem.causa*(nqx/nqx.n)*(nax-n)
tabua.s.neo$nax.n=tabua.s.neo$n+tabua.s.neo$p.no.neo*(tabua.masculina$nqx/tabua.s.neo$nqx.n)*(tabua.masculina$nax-tabua.s.neo$n)

## Para os demais grupos com intervalo de 5 anos, assume-se que as mortes seguem uma funcao quadratica sobre os intervalos x-5 a x+10, assim:

## No nosso caso, 
for (i in 4:10){tabua.s.neo$nax.n[i]=((tabua.s.neo$ndx[i-1]*-5/24)+2.5*tabua.s.neo$ndx[i]+(tabua.s.neo$ndx[i+1]*5/24))/tabua.s.neo$ndx[i]}

# para o ultimo grupo
tabua.s.neo$nax.n[12]=tabua.masculina$ex[12]/tabua.s.neo$p.no.neo[12]

## Estimando o pessoas-ano vivido da tabela de vida Lx
##Todos que viveram ate o fim do intervalo + aqueles que viveram certo tempo e morreram antes de completar a ultima idade do intervalo
for (i in 1:11){tabua.s.neo$nLx[i]<-((tabua.s.neo$n[i]*tabua.s.neo$lx[i+1])+(tabua.s.neo$ndx[i]*tabua.s.neo$nax.n[i]))}

##Para o ultimo grupo de idade mortes/taxa
tabua.s.neo$nLx[12]<-tabua.s.neo$ndx[12]*tabua.s.neo$nax.n[12]


##Estimando o Tx - pessoas-ano acima da idade x
tabua.s.neo$Tx<-(rev(cumsum(rev(tabua.s.neo$nLx))))


##Estimando a esperan?a de vida ex = Tx/lx
tabua.s.neo$ex.n<-round(tabua.s.neo$Tx/tabua.s.neo$lx,digits=2)

#circulatório----
## Proporcao de mortes, sem circulatorio
p.no.circ=(dados$mortes.t-todos.dados$aparelho.circu)/dados$mortes.t
p.no.circ

tabua.s.circ=data.frame(cbind(idade,n,p.no.circ))

## Extraindo da probabilidade de sobrevivencia essa proporcao = px^proporcao
tabua.s.circ$npx.n=tabua.masculina$npx^tabua.s.circ$p.no.circ

## Novo nqx = 1-npx
tabua.s.circ$nqx.n=1-tabua.s.circ$npx.n

##Criando uma coorte hipotetica e aplicando as probabilidades de sobrevivencia para gerar o lx
tabua.s.circ$lx<-100000 #estabelecendo o raiz da tabua

##para os demais grupos de idade
for (i in 2:12){tabua.s.circ$lx[i]<-(tabua.s.circ$npx.n[i-1]*tabua.s.circ$lx[i-1])}

##Estimando o n?mero de mortes de tabela ndx
for (i in 1:11){tabua.s.circ$ndx[i]<-tabua.s.circ$lx[i]-tabua.s.circ$lx[i+1]}

##Para o ultimo grupo de idade
tabua.s.circ$ndx[12]<-tabua.s.circ$lx[12]




#nax novo




## Para os grupos de idade 0 a 9 e penultimo da tabua
## = n + p.sem.causa*(nqx/nqx.n)*(nax-n)
tabua.s.circ$nax.n=tabua.s.circ$n+tabua.s.circ$p.no.circ*(tabua.masculina$nqx/tabua.s.circ$nqx.n)*(tabua.masculina$nax-tabua.s.circ$n)

## Para os demais grupos com intervalo de 5 anos, assume-se que as mortes seguem uma funcao quadratica sobre os intervalos x-5 a x+10, assim:

## No nosso caso, 
for (i in 4:10){tabua.s.circ$nax.n[i]=((tabua.s.circ$ndx[i-1]*-5/24)+2.5*tabua.s.circ$ndx[i]+(tabua.s.circ$ndx[i+1]*5/24))/tabua.s.circ$ndx[i]}

# para o ultimo grupo
tabua.s.circ$nax.n[12]=tabua.masculina$ex[12]/tabua.s.circ$p.no.circ[12]

## Estimando o pessoas-ano vivido da tabela de vida Lx
##Todos que viveram ate o fim do intervalo + aqueles que viveram certo tempo e morreram antes de completar a ultima idade do intervalo
for (i in 1:11){tabua.s.circ$nLx[i]<-((tabua.s.circ$n[i]*tabua.s.circ$lx[i+1])+(tabua.s.circ$ndx[i]*tabua.s.circ$nax.n[i]))}

##Para o ultimo grupo de idade mortes/taxa
tabua.s.circ$nLx[12]<-tabua.s.circ$ndx[12]*tabua.s.circ$nax.n[12]


##Estimando o Tx - pessoas-ano acima da idade x
tabua.s.circ$Tx<-(rev(cumsum(rev(tabua.s.circ$nLx))))


##Estimando a esperan?a de vida ex = Tx/lx
tabua.s.circ$ex.n<-round(tabua.s.circ$Tx/tabua.s.circ$lx,digits=2)


#causas externas----
## Proporcao de mortes, sem circulatorio
p.no.exte=(dados$mortes.t-todos.dados$causas.externas)/dados$mortes.t
p.no.exte

tabua.s.exte=data.frame(cbind(idade,n,p.no.exte))

## Extraindo da probabilidade de sobrevivencia essa proporcao = px^proporcao
tabua.s.exte$npx.n=tabua.masculina$npx^tabua.s.exte$p.no.exte

## Novo nqx = 1-npx
tabua.s.exte$nqx.n=1-tabua.s.exte$npx.n

##Criando uma coorte hipotetica e aplicando as probabilidades de sobrevivencia para gerar o lx
tabua.s.exte$lx<-100000 #estabelecendo o raiz da tabua

##para os demais grupos de idade
for (i in 2:12){tabua.s.exte$lx[i]<-(tabua.s.exte$npx.n[i-1]*tabua.s.exte$lx[i-1])}

##Estimando o n?mero de mortes de tabela ndx
for (i in 1:11){tabua.s.exte$ndx[i]<-tabua.s.exte$lx[i]-tabua.s.exte$lx[i+1]}

##Para o ultimo grupo de idade
tabua.s.exte$ndx[12]<-tabua.s.exte$lx[12]




#nax novo




## Para os grupos de idade 0 a 9 e penultimo da tabua
## = n + p.sem.causa*(nqx/nqx.n)*(nax-n)
tabua.s.exte$nax.n=tabua.s.exte$n+tabua.s.exte$p.no.exte*(tabua.masculina$nqx/tabua.s.exte$nqx.n)*(tabua.masculina$nax-tabua.s.exte$n)

## Para os demais grupos com intervalo de 5 anos, assume-se que as mortes seguem uma funcao quadratica sobre os intervalos x-5 a x+10, assim:

## No nosso caso, 
for (i in 4:10){tabua.s.exte$nax.n[i]=((tabua.s.exte$ndx[i-1]*-5/24)+2.5*tabua.s.exte$ndx[i]+(tabua.s.exte$ndx[i+1]*5/24))/tabua.s.exte$ndx[i]}

# para o ultimo grupo
tabua.s.exte$nax.n[12]=tabua.masculina$ex[12]/tabua.s.exte$p.no.exte[12]

## Estimando o pessoas-ano vivido da tabela de vida Lx
##Todos que viveram ate o fim do intervalo + aqueles que viveram certo tempo e morreram antes de completar a ultima idade do intervalo
for (i in 1:11){tabua.s.exte$nLx[i]<-((tabua.s.exte$n[i]*tabua.s.exte$lx[i+1])+(tabua.s.exte$ndx[i]*tabua.s.exte$nax.n[i]))}

##Para o ultimo grupo de idade mortes/taxa
tabua.s.exte$nLx[12]<-tabua.s.exte$ndx[12]*tabua.s.exte$nax.n[12]


##Estimando o Tx - pessoas-ano acima da idade x
tabua.s.exte$Tx<-(rev(cumsum(rev(tabua.s.exte$nLx))))


##Estimando a esperan?a de vida ex = Tx/lx
tabua.s.exte$ex.n<-round(tabua.s.exte$Tx/tabua.s.exte$lx,digits=2)


#outras----
## Proporcao de mortes, sem circulatorio
p.no.outr=(dados$mortes.t-todos.dados$Outras.causas)/dados$mortes.t
p.no.outr

tabua.s.outr=data.frame(cbind(idade,n,p.no.outr))

## Extraindo da probabilidade de sobrevivencia essa proporcao = px^proporcao
tabua.s.outr$npx.n=tabua.masculina$npx^tabua.s.outr$p.no.outr

## Novo nqx = 1-npx
tabua.s.outr$nqx.n=1-tabua.s.outr$npx.n

##Criando uma coorte hipotetica e aplicando as probabilidades de sobrevivencia para gerar o lx
tabua.s.outr$lx<-100000 #estabelecendo o raiz da tabua

##para os demais grupos de idade
for (i in 2:12){tabua.s.outr$lx[i]<-(tabua.s.outr$npx.n[i-1]*tabua.s.outr$lx[i-1])}

##Estimando o n?mero de mortes de tabela ndx
for (i in 1:11){tabua.s.outr$ndx[i]<-tabua.s.outr$lx[i]-tabua.s.outr$lx[i+1]}

##Para o ultimo grupo de idade
tabua.s.outr$ndx[12]<-tabua.s.outr$lx[12]




#nax novo




## Para os grupos de idade 0 a 9 e penultimo da tabua
## = n + p.sem.causa*(nqx/nqx.n)*(nax-n)
tabua.s.outr$nax.n=tabua.s.outr$n+tabua.s.outr$p.no.outr*(tabua.masculina$nqx/tabua.s.outr$nqx.n)*(tabua.masculina$nax-tabua.s.outr$n)

## Para os demais grupos com intervalo de 5 anos, assume-se que as mortes seguem uma funcao quadratica sobre os intervalos x-5 a x+10, assim:

## No nosso caso, 
for (i in 4:10){tabua.s.outr$nax.n[i]=((tabua.s.outr$ndx[i-1]*-5/24)+2.5*tabua.s.outr$ndx[i]+(tabua.s.outr$ndx[i+1]*5/24))/tabua.s.outr$ndx[i]}

# para o ultimo grupo
tabua.s.outr$nax.n[12]=tabua.masculina$ex[12]/tabua.s.outr$p.no.outr[12]

## Estimando o pessoas-ano vivido da tabela de vida Lx
##Todos que viveram ate o fim do intervalo + aqueles que viveram certo tempo e morreram antes de completar a ultima idade do intervalo
for (i in 1:11){tabua.s.outr$nLx[i]<-((tabua.s.outr$n[i]*tabua.s.outr$lx[i+1])+(tabua.s.outr$ndx[i]*tabua.s.outr$nax.n[i]))}

##Para o ultimo grupo de idade mortes/taxa
tabua.s.outr$nLx[12]<-tabua.s.outr$ndx[12]*tabua.s.outr$nax.n[12]


##Estimando o Tx - pessoas-ano acima da idade x
tabua.s.outr$Tx<-(rev(cumsum(rev(tabua.s.outr$nLx))))


##Estimando a esperan?a de vida ex = Tx/lx
tabua.s.outr$ex.n<-round(tabua.s.outr$Tx/tabua.s.outr$lx,digits=2)

#tabelas
tabua.s.circ.masc<-tabua.s.circ
tabua.s.exte.masc<-tabua.s.exte
tabua.s.infec.masc<-tabua.s.infec
tabua.s.neo.masc<-tabua.s.neo
tabua.s.outr.masc<-tabua.s.outr

#tabua masculina
tabua.masculina %>%
  mutate(nMx=nMx*100) %>% 
  gt() %>% 
  fmt_number(columns = c(nMx,nax,nqx,npx,ex), decimals = 2) %>% 
  fmt_number(columns = c(lx,ndx,nLx,Tx),decimals =0 ,use_seps = FALSE) %>% 
  cols_label(idade=md("**Idade**"),mortes.t=md("**Total de mortes**"),
             expos=md("**Pop. Residente**"),n=md("**n**"),
             nMx=md("**nMx**"),nax=md('**nax**'),
             nqx=md("**nqx**"),npx=md('**npx**'),lx=md("**lx**"),ndx=md("**ndx**"),
             nLx=md("**nLx**"),Tx=md("**TX**"),ex=md("**Ex**")) %>% 
  tab_header(title = md("**Tabua de vida Masculina**"), subtitle = "Rio Grande do Norte, 2012") %>% 
  cols_align(align = "center") %>% 
  tab_source_note(source_note = "Fonte: Censo Demográfico 2012") %>% 
  gtsave("tabua.masculina.png")#para salvar




#esperanças----
experancas<-data.frame(todos.dados$`Faixa Etária`,tabua.masculina$ex,tabua.feminina$ex,
                       tabua.s.infec.masc$ex.n,tabua.s.infec.fem$ex.n,
                       tabua.s.circ.masc$ex.n,tabua.s.circ.fem$ex.n,
                       tabua.s.neo.masc$ex.n,tabua.s.neo.fem$ex.n,
                       tabua.s.exte.masc$ex.n,tabua.s.exte.fem$ex.n,
                       tabua.s.outr.masc$ex.n,tabua.s.outr.fem$ex.n)



experancas %>% 
  gt(rowname_col = "todos.dados..Faixa.Etária.") %>% 
  tab_spanner(label = md("**Todas as Causas**"),
              columns =c(tabua.masculina.ex,tabua.feminina.ex) ) %>% 
  tab_spanner(label = md("**Sem Infecção**"),
              columns = c(tabua.s.infec.masc.ex.n,tabua.s.infec.fem.ex.n)) %>% 
  tab_spanner(label = md("**Sem Apar. Circulatório**"),
              columns = c(tabua.s.circ.masc.ex.n,tabua.s.circ.fem.ex.n)) %>% 
  tab_spanner(label = md("**Sem Causas Externas**"),
              columns = c(tabua.s.exte.masc.ex.n,tabua.s.exte.fem.ex.n)) %>% 
  tab_spanner(label = md("**Sem Neoplasia**"),
            columns = c(tabua.s.neo.masc.ex.n,tabua.s.neo.fem.ex.n)) %>% 
  tab_spanner(label = md("**Sem Outras Causas**"),
              columns = c(tabua.s.outr.masc.ex.n,tabua.s.outr.fem.ex.n)) %>% 
  cols_label(tabua.masculina.ex="Masc",tabua.feminina.ex="Fem",
             tabua.s.infec.masc.ex.n="Masc",tabua.s.infec.fem.ex.n="Fem",
             tabua.s.circ.masc.ex.n="Masc",tabua.s.circ.fem.ex.n="Fem",
             tabua.s.neo.masc.ex.n="Masc",tabua.s.neo.fem.ex.n="Fem",
             tabua.s.exte.masc.ex.n="Masc",tabua.s.exte.fem.ex.n="Fem",
             tabua.s.outr.masc.ex.n="Masc",tabua.s.outr.fem.ex.n="Fem") %>%  
  tab_header(title = md("**Esperanças de vida comparadas**"), subtitle = "Rio Grande do Norte, 2012") %>% 
  cols_align(align = "center") %>% 
  tab_source_note(source_note = "Fonte: Censo Demográfico 2012") %>%
  gtsave("esperanças.png")

#tem masc e fem----
tem.fem.mas<-data.frame(tabua.feminina$nMx,tabua.masculina$nMx)
tem.fem.mas %>% 
  gather(key = 'sexo', value = 'nMx') %>% 
  mutate(idade =c(0,1,seq(5,20,5),seq(30,80,10),0,1,seq(5,20,5),seq(30,80,10))) %>% 
  ggplot(aes(x=idade,y =log(nMx), group =sexo )) +
  geom_line(aes(color=sexo))+
  geom_point(aes(color=sexo))+
  theme_minimal()+
  theme(legend.position="bottom")+
  scale_color_manual(labels = c("Feminino","Masculino"), values = c("#177e94","#7fdbdb"))+
  labs(title = "Taxa Específica de Mortalidade",subtitle = "Rio Grande do Norte, 2012"
       ,x="Idade",color="Sexo", y="nMX",caption = "FONTE: Censo Demográfico 2012")


#grafico lx----
lx.masc<-data.frame(tabua.masculina$lx,tabua.s.infec.masc$lx,tabua.s.neo.masc$lx,
                   tabua.s.circ.masc$lx,tabua.s.exte.masc$lx,tabua.s.outr.masc$lx
)
lx.mas<-lx.masc %>% 
  gather(key='LX',value = 'lx') %>% 
  mutate(Idade=c(rep((c(0,1,seq(5,20,5),seq(30,80,10))),6))) 
lx.mas %>% 
  ggplot(aes(x=Idade, y=lx, group = LX,color=LX,size=LX)) +
  geom_line(aes(linetype = LX))+
  scale_color_manual(labels = c("lx (todas as causas)","lx (sem Circulatório)",
                                "lx (sem Causa Externa)","lx (sem Infecção)",
                                "lx (sem Neoplasia)","lx (sem Outras Causas)"), 
                     values = c("firebrick3","gray",
                                "peru","paleturquoise4","royalblue2",
                                "blue4"))+
  scale_size_manual(values = c(2.5,rep(0.5,5)))+
  scale_linetype_manual(values=c("dotted", rep("solid",5)))+
  labs(title = "Homens Sobrevivêntes à idade exata x",
       subtitle = "Rio Grande do Norte 2012",
       x="Idade",
       y="lx",
       caption = "Fonte: Censo Demográfico 2012", color = "")+
  theme_minimal()+theme(legend.position="bottom")


#grafixo nqx ----

nqx.masc<-data.frame(todos=tabua.masculina$nqx,
                     infec=tabua.s.infec.masc$nqx.n,
                    neo=tabua.s.neo.masc$nqx.n,
                    circ=tabua.s.circ.masc$nqx.n,
                    exte=tabua.s.exte.masc$nqx.n,
                    outra=tabua.s.outr.masc$nqx.n)

nqx.masc<-nqx.masc %>% 
  gather(key='nqx',value = 'value') %>% 
  mutate(Idade=c(rep((c(0,1,seq(5,20,5),seq(30,80,10))),6))) 


nqx.masc %>% 
  ggplot(aes(x=Idade, y=log(value), group=nqx))+
  geom_line(aes(color=nqx, size = nqx,linetype=nqx))+
  scale_linetype_manual(values=c(rep("solid",5),"dotted"))+
  scale_size_manual(values = c(rep(0.5,5),2))+
  scale_color_manual(labels = c("nqx (sem Circulatório)","nqx (sem Causa Externa)",
                                "nqx (todas as causas)","nqx (sem Infecção)",
                                "nqx (sem Neoplasia)","nqx (sem Outras Causas)"), 
                     values = c("gray",
                                "peru","paleturquoise4","royalblue2",
                                "blue4","firebrick3"))+
  labs(title = "Probabilidade de morte entre homens",
       subtitle = "Rio Grande do Norte 2012",
       x="Idade",
       y="nqx",
       caption = "Fonte: Censo Demográfico 2012", color = "")+
  theme_minimal()+theme(legend.position="bottom")



#ganhos na expec-----
ganho.exp.masc<-data.frame(
  infeccao = tabua.s.infec.masc$ex.n-tabua.masculina$ex,
  neoplasia=tabua.s.neo.masc$ex.n-tabua.masculina$ex,
  circulatorio=tabua.s.circ.masc$ex.n-tabua.masculina$ex,
  causas_externas=tabua.s.exte.masc$ex.n-tabua.masculina$ex,
  outras=tabua.s.outr.masc$ex.n-tabua.masculina$ex)



#os dois juntos----
ganho.exp.fem.mas<-data.frame(
  idade=todos.dados$`Faixa Etária`,
  infeccao.f = tabua.s.infec.fem$ex.n-tabua.feminina$ex,
  neoplasia.f=tabua.s.neo.fem$ex.n-tabua.feminina$ex,
  circulatorio.f=tabua.s.circ.fem$ex.n-tabua.feminina$ex,
  causas_externas.f=tabua.s.exte.fem$ex.n-tabua.feminina$ex,
  outras.f=tabua.s.outr.fem$ex.n-tabua.feminina$ex,
  infeccao.m = tabua.s.infec.masc$ex.n-tabua.masculina$ex,
  neoplasia.m=tabua.s.neo.masc$ex.n-tabua.masculina$ex,
  circulatorio.m=tabua.s.circ.masc$ex.n-tabua.masculina$ex,
  causas_externas.m=tabua.s.exte.masc$ex.n-tabua.masculina$ex,
  outras.m=tabua.s.outr.masc$ex.n-tabua.masculina$ex)

ganho.exp.fem.mas %>% 
  gt(rowname_col = "idade") %>% 
  tab_spanner(label = md("**Infecção**"), columns = c(infeccao.f,infeccao.m))%>% 
  tab_spanner(label = md("**Neoplasia**"), columns = c(neoplasia.f,neoplasia.m))%>% 
  tab_spanner(label = md("**Circulatório**"), columns = c(circulatorio.f,circulatorio.m))%>% 
  tab_spanner(label = md("**Causas Externas**"), columns = c(causas_externas.f,causas_externas.m))%>% 
  tab_spanner(label = md("**Outras**"), columns = c(outras.f,outras.m)) %>% 
  cols_label(infeccao.f=md("**Feminino**"),infeccao.m=md("**Masculino**"),
             neoplasia.f=md("**Feminino**"),neoplasia.m=md("**Masculino**"),
             circulatorio.f=md("**Feminino**"),circulatorio.m=md("**Masculino**"),
             causas_externas.f=md("**Feminino**"),causas_externas.m=md("**Masculino**"),
             outras.f=md("**Feminino**"),outras.m=md("**Masculino**")) %>% 
  tab_header(title = "Ganhos(em anos) de Vida por Causa Eliminada",
             subtitle = "Rio Grande do Norte 2012") %>% 
  tab_source_note(source_note = "Fonte: Censo Demográfico 2012") %>% 
  gtsave("ganhos-masc-fem.png")
