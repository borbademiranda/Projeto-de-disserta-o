# pacotes
library(tidyverse)
library(broom)
library(lme4)

# setting working directory
setwd("C:/Users/test/Desktop/Projeto de dissertação/scripts")

##### carregando base de dados original - ESS França e Alemanha #####
ess <- read.csv("ESS1-8e01.csv")

ess$cname <- NULL
ess$cproddat <- NULL
ess$name <- NULL
ess$edition <- NULL

##### recodificando variáveis #####

# inserindo NAs nas variáveis relacionadas à ocupação do respondente
ess$iscoco[ess$iscoco > 9330] <- NA
ess$isco08[ess$isco08 > 9629] <- NA

# criando variável blue collar
ess$blue_c <- ifelse(ess$iscoco > 7000 & ifelse(ess$isco08 > 7000 & ess$isco08 <= 9330, 1, 0), 1, 0)
ess$blue_c[is.na(ess$blue_c)] <- 1

# acompanhamento de notícias sobre política
ess$tvpol[ess$tvpol > 7] <- NA
ess$rdpol[ess$rdpol > 7] <- NA
ess$nwsppol[ess$nwsppol > 7] <- NA

# interesse e sentimento de participação política
ess$polintr[ess$polintr > 4] <- NA
ess$psppsgv[ess$psppsgv > 10] <- NA
ess$psppsgva[ess$psppsgva > 5] <- NA
ess$actrolg[ess$actrolg > 10] <- NA
ess$actrolga[ess$actrolga > 5] <- NA
ess$psppipl[ess$psppipl > 10] <- NA
ess$psppipla[ess$psppipla > 5] <- NA
ess$cptppol[ess$cptppol > 10] <- NA
ess$cptppola[ess$cptppola > 5] <- NA
ess$polcmpl[ess$polcmpl > 5] <- NA
ess$poldcs[ess$poldcs > 5] <- NA

# confiança nas instituições políticas
ess$trstprl[ess$trstprl > 10] <- NA
ess$trstplt[ess$trstplt > 10] <- NA
ess$trstprt[ess$trstprt > 10] <- NA
ess$trstep[ess$trstep > 10] <- NA

# o respondente votou ou não nas últimas eleições
ess$vote[ess$vote > 2] <- NA
ess$vote <- ifelse(ess$vote == 1, 1, 0)

# contato com políticos
ess$contplt[ess$contplt > 2] <- NA
ess$contplt[ess$contplt == 2] <- 0

# trabalhou em partidos ou organizações políticas nos últimos 12 meses
ess$wrkprty[ess$wrkprty > 2] <- NA
ess$wrkprty[ess$wrkprty == 2] <- 0

# usou adereços de campanha
ess$badge[ess$badge > 2] <- NA
ess$badge[ess$badge == 2] <- 0

# se sente próximo de algum partido
ess$clsprty[ess$clsprty > 2] <- NA
ess$clsprty[ess$clsprty == 2] <- 0

# o quão próximo de um partido
ess$prtdgcl[ess$prtdgcl > 4] <- NA

# membro de um partido político
ess$mmbprty[ess$mmbprty > 2] <- NA
ess$mmbprty[ess$mmbprty == 2] <- 0

# escala esquerda-direita
ess$lrscale[ess$lrscale > 10] <- NA

# satisfação com a vida
ess$stflife[ess$stflife > 10] <- NA

# satisfação com a economia
ess$stfeco[ess$stfeco > 10] <- NA

# satisfação com o governo
ess$stfgov[ess$stfgov > 10] <- NA

# satisfação com a democracia
ess$stfdem[ess$stfdem > 10] <- NA

# partidos políticos que desafiam a democracia devem ser banidos
ess$prtyban[ess$prtyban > 5] <- NA
ess$prtyban <- max(ess$prtyban, na.rm = T) - ess$prtyban

# o processo de integração europeia já foi longe de mais
ess$euftf[ess$euftf > 10] <- NA

# restrição no número de imigrantes
ess$imdfetn[ess$imdfetn > 4] <- NA

# restrição de imigrantes provenientes de países pobres fora da Europa
ess$impcntr[ess$impcntr > 4] <- NA

# imigração é bom para a economia do país
ess$imbgeco[ess$imbgeco > 10] <- NA

# a cultura do país é enriquecida pelos imigrantes
ess$imueclt[ess$imueclt > 10] <- NA

# imigrantes fazem do país um lugar melhor para se viver
ess$imwbcnt[ess$imwbcnt > 10] <- NA

# imigrantes fazem pioram ou melhoram os índices de crime
ess$imwbcrm[ess$imwbcrm > 10] <- NA

# em cada 100 pessoas, quantas são imigrantes?
ess$noimbro[ess$noimbro >= 777] <- NA

# aceitação dos costumes do país de destino como critério para se aceitar mais imigrantes
ess$qfimcmt[ess$qfimcmt > 10] <- NA

# qualificação para imigração: ser branco
ess$qfimwht[ess$qfimwht > 10] <- NA

# é importante seguir tradições e costumes
ess$imptrad[ess$imptrad > 6] <- NA
ess$imptrad <- max(ess$imptrad, na.rm = T) - ess$imptrad

# é importante o governo ser forte e prover segurança
ess$ipstrgv[ess$ipstrgv > 6] <- NA
ess$ipstrgv <- max(ess$ipstrgv, na.rm = T) - ess$ipstrgv

# importante viver em vizinhanças seguras
ess$impsafe[ess$impsafe > 6] <- NA
ess$impsafe <- max(ess$impsafe, na.rm = T) - ess$impsafe

# o emprego atual é seguro
ess$jbscr[ess$jbscr > 4] <- NA
ess$jbscr <- max(ess$jbscr, na.rm = T) - ess$jbscr

# sempre otimista em relação ao futuro
ess$optftr[ess$optftr > 5] <- NA
ess$optftr <- max(ess$optftr, na.rm = T)

# se sente ansioso nos últimos tempos
ess$fltanx[ess$fltanx > 4] <- NA

# difícil se sentir esperançoso com o futuro
ess$nhpftr[ess$nhpftr > 5] <- NA
ess$nhpftr <- max(ess$nhpftr) - ess$nhpftr

# a vida está ficando pior para os concidadãos
ess$lfwrs[ess$lfwrs > 5] <- NA
ess$lfwrs <- max(ess$lfwrs) - ess$lfwrs

# percepção do número de desempregados
ess$uemplwk[ess$uemplwk > 11] <- NA %>% as.factor()

# pessoas de minorias raciais ou étnicas convivem na mesma área que o respondente
ess$acetalv[ess$acetalv > 3] <- NA

# anos de educação
ess$eduyrs[ess$eduyrs >= 77] <- NA

# gênero
ess$gndr[ess$gndr > 2] <- NA
ess$gndr[ess$gndr == 2] <- 0

# idade
ess$agea[ess$agea == 999] <- NA

# ficou desempregado em alguma ocasião nos últimos cinco anos
ess$uemp5yr[ess$uemp5yr > 2] <- NA
ess$uemp5yr[ess$uemp5yr == 2] <- 0

##### criando variável para voto em populistas #####
# subsetting datasets
de <- ess[ess$cntry == "DE",]
fr <- ess[ess$cntry == "FR",]

# subsetting data for 2012 and 2013
de13 <- de[de$essround %in% c(7, 8),]
fr12 <- fr[fr$essround %in% c(7, 8),]

# criando variável para voto no rrpp selecionado na Alemanha
de$vote_rrp <- ifelse(de$prtvade1 == 7 | de$prtvbde1 == 7 | de$prtvcde1 == 7 |
                        de$prtvdde1 == 7 | de$prtvede1 == 8, 1, 0)
de$vote_rrp[is.na(de$vote_rrp)] <- 0

# criando variável para o voto no partido selecionado na França
fr$vote_rrp <- ifelse(fr$prtvtfr == 3 & fr$prtvtafr == 3 & fr$prtvtbfr == 2 & 
                       fr$prtvtcfr == 2, 1, 0)
fr$vote_rrp[is.na(fr$vote_rrp)] <- 1

# reunindo novamente os datasets
ess <- rbind(de, fr)

# criando variável
de13$vote_rrp <- ifelse(de13$prtvede1 == 6 & de13$prtvede2 == 6, 1, 0)
table(de13$vote_rrp)

fr12$vote_rrp <- ifelse(fr12$prtvtcfr == 2, 1, 0)
table(fr12$vote_rrp)

# reunindo novamente os datasets
ess12 <- rbind(de13, fr12)

##### testando alguns modelos #####
library(lme4)
l1 <- glmer(data = fr, vote_rrp ~ clsprty + mmbprty + polintr + trstplt + trstprt +
              (1 | regionfr), 
            family = "binomial", control = glmerControl(optimizer = "bobyqa"))
summary(l1)

l2 <- glm(data = fr, vote_rrp ~ clsprty + mmbprty + polintr + trstplt + trstprt +
            badge + lrscale + stfdem + stfgov + blue_c + fltanx + lfwrs + nhpftr + uemplwk, family = "binomial")
summary(l2)

##### criando objeto para retirar do dataset somente as variáveis de interesse #####
vars <- c("cntry", "regionde", "intewde", "regionfr", "cedition", "cseqno", "essround", 
          "idno", "dweight", "pspwght", "pweight",  "tvpol", "rdpol", "nwsppol",  
          "polintr", "psppsgv", "psppsgva", "psppipl", "psppipla", "polcmpl", 
          "contplt", "wrkprty", "badge", "clsprty", "prtdgcl", "mmbprty", 
          "lrscale", "trstprl", "trstplt", "trstprt", "trstep", "stflife", 
          "stfeco", "stfgov", "stfdem", "prtyban", "euftf", "imdfetn", "impcntr", 
          "imbgeco", "imueclt",  "imwbcnt", "imwbcrm", "noimbro", "qfimcmt", 
          "imptrad", "ipstrgv", "impsafe", "jbscr", "optftr", "fltanx", "nhpftr", 
          "lfwrs", "uemplwk", "acetalv", "eduyrs", "blue_c", "uemp5yr", "gndr", 
          "agea", "vote", "vote_rrp", "prtvade1", "prtvbde1", "prtvcde1", "prtvdde1", 
          "prtvede1", "prtvde2",  "prtvade2", "prtvbde2", "prtvcde2", "prtvdde2", 
          "prtvede2", "prtvtfr",  "prtvtafr", "prtvtbfr", "prtvtcfr", "prtclde", 
          "prtclade", "prtclbde", "prtclcde", "prtcldde", "prtclede", "prtclafr", 
          "prtclbfr", "prtclcfr", "prtcldfr", "prtclefr", "prtmbde", "prtmbade", 
          "prtmbbde", "prtmbcde", "prtmbafr", "prtmbbfr", "prtmbcfr")

# criando datasets com as variáveis de interesse
ess2 <- ess[vars]
fr2 <- fr[vars]
ess122 <- ess12[vars]

##### salvando novos datasets #####
write.csv(ess2, "ess_de-fr.csv")
write.csv(fr2, "ess_fr.csv")
write.csv(ess122, "ess_de_fr_12.csv")

##### carregando dados do CHES #####
setwd("C:/Users/test/Desktop/Lucas/Ciência Política UFPE - Mestrado/2018.2 - Partidos políticos e sistemas partidários/scripts")
ches <- read.csv("1999-2014_CHES_dataset_means.csv")
setwd("C:/Users/test/Desktop/Projeto de dissertação/scripts")

# subsetting data for west-eu countries
west_eu <- ches[ches$eastwest == "west",]

# subsetting radical right parties
rrp <- west_eu[west_eu$family == "rad right" | west_eu$party == "LN",]

# parties dataset
stargazer(rrp[c("country", "party")], summary = F, type = "html", out = "parties.htm")

##### carregando CLEA dataset #####
setwd("./scripts")
load("clea_lc_20181119.rdata")

##### retirando países do dataset para análise #####

# subsetting observations of France
clea_fr <- clea_lc_20181119[clea_lc_20181119$ctr_n == "France",]

# subsetting France and Germany
clea_defr <- clea_lc_20181119[clea_lc_20181119$ctr_n %in% c("Germany", "France"),]

# subsetting West eu data
clea_westeu <- clea_lc_20181119[clea_lc_20181119$ctr_n %in% c("France", "Germany",
                                                              "Italy", "Austria",
                                                              "Belgium", "Denmark",
                                                              "Finland", "Greece",
                                                              "Netherlands", "Sweden", 
                                                              "UK"),]

##### salvando datasets #####
write.csv(clea_fr, file = "clea_fr.csv")
write.csv(clea_defr, file = "clea_de-fr.csv")
write.csv(clea_westeu, file = "clea_westeu.csv")

##### testando dados - algumas estatísticas descritivas #####
library(ggplot2)
cleafr <- read.csv("clea_fr.csv")
cleafr$pty_n[cleafr$pty_n == "National Front"] <- "front national"

cleafn <- cleafr[cleafr$pty_n =="front national",]

# gráfico de linha 1 - desempenho médio do FN por ano
line1 <- ggplot(cleafn, aes(x = yr, y = pvs1)) +
  theme_classic() +
  scale_x_continuous(limits = c(1978, 2017), breaks = c(1978, 1981, 1986, 1988,
                                                        1993, 1997, 2002, 2007, 
                                                        2012, 2017)) +
  theme(plot.title = element_text(face = "bold", hjust = .5, size = 12),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "Ano", y = "Média de votos (%)") +
  ggtitle("Média de votos do FN por ano de eleição") +
  stat_summary(fun.y = mean, geom = "line")

# desempenho do partido por distrito ao longo dos anos
line2 <- ggplot(cleafn, aes(x = yr, y = pvs1)) +
  geom_line(aes(group = cst), alpha = .1) +
  geom_smooth(method = "lm", colour = "red", se = F) +
  theme_classic() +
  ggtitle("Porcentagem de votos do FN nas constituências por ano de eleição") +
  labs(x = "Ano", y = "Porcentagem de votos") +
  theme(plot.title = element_text(size = 12, hjust = 2, face = "bold")) +
  scale_x_continuous(limits = c(1978, 2017), breaks = c(1978, 1981, 1986, 1988,
                                                        1993, 1997, 2002, 2007, 
                                                        2012, 2017))

############################# VERSÃO 12/04 ####################################
setwd("C:/Users/test/Desktop/Projeto de dissertação")

library(ffbase)
ess <- read.csv2.ffdf(file = "ESS1-8e01.csv")

ess2 <- read.csv("ESS1-8e01.csv")

vars <- c("cntry", "cregion", "cedition", "cseqno", "essround", 
          "idno", "dweight", "pspwght", "pweight",  "tvpol", "rdpol", "nwsppol",  
          "polintr", "psppsgv", "psppsgva", "psppipl", "psppipla", "polcmpl", 
          "contplt", "wrkprty", "badge", "clsprty", "prtdgcl", "mmbprty", 
          "lrscale", "trstprl", "trstplt", "trstprt", "trstep", "stflife", 
          "stfeco", "stfgov", "stfdem", "prtyban", "euftf", "imdfetn", "impcntr", 
          "imbgeco", "imueclt",  "imwbcnt", "imwbcrm", "noimbro", "qfimcmt", 
          "imptrad", "ipstrgv", "impsafe", "jbscr", "optftr", "fltanx", "nhpftr", 
          "lfwrs", "uemplwk", "acetalv", "eduyrs", "blue_c", "uemp5yr", "gndr", 
          "agea", "vote", "vote_rrp", "prtvade1", "prtvbde1", "prtvcde1", "prtvdde1", 
          "prtvede1", "prtvde2",  "prtvade2", "prtvbde2", "prtvcde2", "prtvdde2", 
          "prtvede2", "prtvtfr",  "prtvtafr", "prtvtbfr", "prtvtcfr", "prtclde", 
          "prtclade", "prtclbde", "prtclcde", "prtcldde", "prtclede", "prtclafr", 
          "prtclbfr", "prtclcfr", "prtcldfr", "prtclefr", "prtmbde", "prtmbade", 
          "prtmbbde", "prtmbcde", "prtmbafr", "prtmbbfr", "prtmbcfr", "prtvtat", 
          "prtvtaat", "prtvrbat", "prtvtbe", "prtvtabe", "prtvtbbe", "prtvtcbe",
          "prtvtdk", "prtvtadk", "prtvtbdk", "prtvtcdk", "prtvtfi", "prtvtafi",
          "prtvtbfi", "prtvtcfi", "prtvtdfi", "prtvtgb", "prtvtagb", "prtvtbgb",
          "prtvtgr", "prtvtagr", "prtvtbgr", "prtvtcgr", "prtvtit", "prtvtait", 
          "prtvtbit", "prtvtnl", "prtvtanl", "prtvtbnl", "prtvtcnl", "prtvtdnl",
          "prtvtenl", "prtvtfnl", "prtvtse", "prtvtase", "prtvtbse")

ess_vars <- ess2[vars]

# subsetting data by country
at <- ess2[ess2$cntry == "AT",]
be <- ess2[ess2$cntry == "BE",]
de <- ess2[ess2$cntry == "DE",]
dk <- ess2[ess2$cntry == "DK",]
fi <- ess2[ess2$cntry == "FI",]
fr <- ess2[ess2$cntry == "FR",]
gb <- ess2[ess2$cntry == "GB",]
gr <- ess2[ess2$cntry == "GR",]
it <- ess2[ess2$cntry == "IT",]
nl <- ess2[ess2$cntry == "NL",]
sw <- ess2[ess2$cntry == "SE",]

##### criando variável para voto em populistas por país #####
# austria
at$vote_rrp <- ifelse(at$prtvtat == 3 | at$prtvtaat %in% c(3, 4) | at$prtvtbat %in% c(3, 4), 1, 0)
table(at$vote_rrp)

# bélgica
be$vote_rrp <- ifelse(be$p)
