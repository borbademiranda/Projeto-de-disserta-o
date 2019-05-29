######################### QUALIFICAÇÃO DE MESTRADO #############################
########################## LUCAS BORBA DE MIRANDA ##############################
############################### 31/05/2019 #####################################
# AJUSTE DE DADOS, GRÁFICOS E TABELAS PARA O PROJETO DE QUALIFICAÇÃO DE 
# DISSERTAÇÃO DE MESTRADO NO PROGRAMA DE PÓS-GRADUAÇÃO EM CIÊNCIA POLÍTICA DA
# UNIVERSIDADE FEDERAL DE PERNAMBUCO

# carregando pacotes utilizados
library(tidyverse)
library(broom)
library(ggplot2)
library(stargazer)
library(GGally)
library(ggrepel)

# informando diretório de trabalho 
setwd("C:/Users/test/Desktop/Projeto de dissertação")

##### carregando bases de dados #####
# ESS
essdefr <- read.csv("./scripts/ess_westeu.csv")

# eleições Alemanha 2017
de17 <- read.csv("./scripts/2017.csv")
bwl17 <- read.csv("./scripts/btw17_wbz_zweitstimmen.csv", sep = ";")

# CLEA - somente RRPPs
clea_rrp <- read.csv("clea_westeu_rrp.csv")

##### ajustando dados #####
# invertendo valores das variáveis de percepção sobre imigração (valores altos
# indicam opinião contrária aos imigrantes)
essdefr$imwbcnt <- max(essdefr$imwbcnt, na.rm = T) - essdefr$imwbcnt
essdefr$imueclt <- max(essdefr$imueclt, na.rm = T) - essdefr$imueclt

# criando banco de dados somente com observações de eleitores de RRPPs
essrrp <- essdefr[essdefr$vote_rrp == 1,]

# criando banco de dados para o eleitorado geral (exceto eleitores de RRPPs)
essgeneral <- essdefr[essdefr$vote_rrp == 0,]

##### criando funções #####
# gráfico de densidade
dplot <- function(VB, VW, Numero, Titulo) {
  ggplot() +
    geom_density(data = essgeneral, aes(x = VB, fill = "black"), alpha = .3,
                 adjust = 3.5) +
    geom_density(data = essrrp, aes(x = VW, fill = "grey"), alpha = .3, adjust = 1.5) +
    scale_x_continuous(name = "", breaks = 0 : 10, limits = c(0,10), expand = c(.01,.01)) +
    scale_y_continuous(name = "", breaks =  NULL, labels = NULL, limits = c(0, .5)) +
    scale_fill_identity(name = "", labels = c("Parties", "Voters")) +
    ggtitle(Titulo) +
    theme_classic(base_size = 12) +
    theme(legend.position = c(.3, .7), legend.background = element_blank(), 
          legend.text = element_text(size = 7), 
          plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.text.x = element_text(size = 7))
}

# gráfico de densidade com eixo y menor
dplotb <- function(VB, VW, Numero, Titulo) {
  ggplot() +
    geom_density(data = essgeneral, aes(x = VB, fill = "black"), alpha = .3,
                 adjust = 7) +
    geom_density(data = essrrp, aes(x = VW, fill = "grey"), alpha = .3, adjust = 5) +
    scale_x_continuous(name = "", breaks = 0 : 4, limits = c(0,4), expand = c(.01,.01)) +
    scale_y_continuous(name = "", breaks =  NULL, labels = NULL, limits = c(0, .7)) +
    scale_fill_identity(name = "", labels = c("Parties", "Voters")) +
    ggtitle(Titulo) +
    theme_classic(base_size = 12) +
    theme(legend.position = c(.3, .7), legend.background = element_blank(), 
          legend.text = element_text(size = 7), 
          plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
          axis.text.x = element_text(size = 7))
}

# multiplot 
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


##### histogramas e box plots #####
# excluindo informações omissas
clea_rrp$pvs1[clea_rrp$pvs1 %in% c(-990, -992, -994)] <- NA

# histograma da média de votos por partido
hist1 <- ggplot(data = clea_rrp, aes(x = reorder(pty_n, +pvs1), y = pvs1)) +
  stat_summary(fun.y = mean, geom = "bar", aes(group = 1)) +
  coord_flip() +
  theme_classic() +
  labs(x = "Partido", y = "Média de votos (%)", caption = "Figura 1
Elaboração do autor com dados do CLEA") +
  ggtitle("Média da porcentagem de votos por partido") +
  theme(plot.title = element_text(size = 12, hjust = .5, face = "bold"), 
        plot.caption = element_text(hjust = .5))

# boxplot da distribuição dos votos da direita radical por país
box1 <- ggplot(data = clea_rrp, aes(x = reorder(ctr_n, +pvs1), y = pvs1)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = "point", colour = "red", size = 1.5) +
  coord_flip() +
  theme_classic() +
  labs(x = "País", y = "Porcentagem de votos", caption = "Figura 2
Elaboração do autor com base nos dados do CLEA") +
  ggtitle("Distribuição dos votos da direita radical por distrito e país") +
  theme(plot.title = element_text(size = 12, hjust = .5, face = "bold"),
        plot.caption = element_text(hjust = .5)) +
  scale_y_continuous(breaks = seq(.0, .7, .05))

# boxplot da distribuição dos votos na AfD em 2017 por região
bwl17$east <- as.factor(ifelse(bwl17$Land %in% c(13, 12, 15, 14, 16), 1, 0))

box2 <- ggplot(bwl17, aes(x = factor(Land), y = AfD, fill = east)) +
  geom_boxplot() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = .5),
        plot.caption = element_text(hjust = .5)) +
  ggtitle("Distribuição dos votos na AfD por Land na Alemanha") +
  labs(x = "Região", y = "Número de votos AfD", caption = "Figura 5
Elaboração do autor com base nos dados do Bundeswahlleiter")

# salvando gráficos
ggsave("hist_partymean.png", plot = hist1, device = "png", width = 5, height = 5)
ggsave("box_rrpp_by_ctr.png", plot = box1, device = "png", width = 6, height = 4)
ggsave("boxplot_AfD_2017.png", plot = box2, device = "png", width = 6, height = 4)

##### gráficos de linha #####
# série temporal dos RRPPs nos cinco países onde a família é mais votada
line1 <- ggplot() +
  stat_summary(data = clea_rrp[clea_rrp$ctr_n == "Finland",], aes(x = yr, y = pvs1),
               fun.y = mean, geom = "line", color = "blue") +
  stat_summary(data = clea_rrp[clea_rrp$ctr_n == "Italy" & clea_rrp$pty_n %in% c("LN", "MSI"),], aes(x = yr, y = pvs1),
               fun.y = mean, geom = "line", color = "green") +
  stat_summary(data = clea_rrp[clea_rrp$ctr_n == "Austria" & clea_rrp$pty_n == "FPÖ",],
               aes(x = yr, y = pvs1), fun.y = mean, geom = "line", color = "red") +
  stat_summary(data = clea_rrp[clea_rrp$ctr_n == "France" & clea_rrp$pty_n == "FN",],
               aes(x = yr, y = pvs1), fun.y = mean, geom = "line", color = "lightblue") +
  stat_summary(data = clea_rrp[clea_rrp$ctr_n == "Netherlands",],
               aes(x = yr, y = pvs1), fun.y = mean, geom = "line", color = "orange") +
  theme_classic() +
  labs(x = "Ano da eleição", y = "Média de votos (%)", caption = "Figura 3
       Elaboração do autor com dados do CLEA") +
  ggtitle("Média de votos na direita radical (cinco primeiros países)") +
  theme(plot.title = element_text(size = 12, hjust = .5, face = "bold"),
        plot.caption = element_text(hjust = .5)) +
  scale_x_continuous(limits = c(1950, 2017), breaks = seq(1950, 2017, 5)) +
  geom_vline(xintercept = 2007, linetype = 2, color = "grey 60")

ggsave("timeseries_rrpp.png", plot = line5, device = "png", width = 6, height = 3.5) 

# média de satisfação com o governo
line6 <- ggplot(data = essgeneral, aes(x = essround, y = stfgov)) +
  theme_classic() +
  theme(plot.title = element_text(size = 12, hjust = .5, face = "bold"),
        plot.caption = element_text(hjust = .5)) +
  scale_y_continuous(limits = c(1, 11), breaks = 1 : 11) +
  scale_x_continuous(limits = c(1, 8), breaks = 1 : 8, labels = c("2002", "2004",
                                                                  "2006", "2008",
                                                                  "2010", "2012",
                                                                  "2014", "2016")) +
  labs(x = "Ano do survey", y = "Média de satisfação com o governo") +
  ggtitle("Satisfação com o governo") +
  stat_summary(fun.y = mean, geom = "line", colour = "black", aes(group = 1)) +
  coord_cartesian(ylim = c(0, 11))

# média de confiança nos políticos
line7 <- ggplot(data = essgeneral, aes(x = essround, y = trstplt)) +
  theme_classic() +
  theme(plot.title = element_text(size = 12, hjust = .5, face = "bold"),
        plot.caption = element_text(hjust = .5)) +
  scale_y_continuous(limits = c(1, 11), breaks = 1 : 11) +
  scale_x_continuous(limits = c(1, 8), breaks = 1 : 8, labels = c("2002", "2004",
                                                                  "2006", "2008",
                                                                  "2010", "2012",
                                                                  "2014", "2016")) +
  labs(x = "Ano do survey", y = "Média da confiança nos políticos") +
  ggtitle("Confiança nos políticos") +
  stat_summary(fun.y = mean, geom = "line", colour = "black", aes(group = 1)) +
  coord_cartesian(ylim = c(0, 11))

# satisfação com os políticos - eleitores populistas
line8 <- ggplot(data = essrrp, aes(x = essround, y = stfgov)) +
  theme_classic() +
  theme(plot.title = element_text(size = 12, hjust = .5, face = "bold"),
        plot.caption = element_text(hjust = .5)) +
  scale_y_continuous(limits = c(1, 11), breaks = 1 : 11) +
  scale_x_continuous(limits = c(1, 8), breaks = 1 : 8, labels = c("2002", "2004",
                                                                  "2006", "2008",
                                                                  "2010", "2012",
                                                                  "2014", "2016")) +
  labs(x = "Ano do survey", y = "Média de satisfação com o governo") +
  ggtitle("Satisfação com o governo - eleitor populista") +
  stat_summary(fun.y = mean, geom = "line", colour = "black", aes(group = 1)) +
  coord_cartesian(ylim = c(0, 11))

# média de confiança nos políticos - eleitores populistas
line9 <- ggplot(data = essrrp, aes(x = essround, y = trstplt)) +
  theme_classic() +
  theme(plot.title = element_text(size = 12, hjust = .5, face = "bold"),
        plot.caption = element_text(hjust = .5)) +
  scale_y_continuous(limits = c(1, 11), breaks = 1 : 11) +
  scale_x_continuous(limits = c(1, 8), breaks = 1 : 8, labels = c("2002", "2004",
                                                                  "2006", "2008",
                                                                  "2010", "2012",
                                                                  "2014", "2016")) +
  labs(x = "Ano do survey", y = "Média da confiança nos políticos") +
  ggtitle("Confiança nos políticos - eleitor populista") +
  stat_summary(fun.y = mean, geom = "line", colour = "black", aes(group = 1)) +
  coord_cartesian(ylim = c(0, 11))


##### gráficos de densidade #####
# density plot 1 - posicionamento dos eleitores e partidos num eixo esquerda-direita
dplot1 <- dplot(essgeneral$lrscale, essrrp$lrscale, 1, 
                "Auto posicionamento numa escala esquerda-direita") +
  scale_fill_identity(name = "", labels = c("Eleitorado", "Eleitor RRPP"), 
                      guide = "legend")

# density plot 2 - imigrantes fazem do país um lugar melhor ou pior para se viver
dplot2 <- dplot(essgeneral$imwbcnt, essrrp$imwbcnt, 2, "Imigrantes fazem do país um lugar pior")

# density plot 3 - sentimento do eleitorado sobre o impacto cultural do imigrante
dplot3 <- dplot(essgeneral$imueclt, essrrp$imueclt, 3, "Imigrantes prejudicam a cultura do país")

# density plot 4 - importância de seguir costumes e tradições
dplot4 <- dplotb(essgeneral$imdfetn, essrrp$imdfetn, 4, "Restrição de imigrantes de grupos étnicos diferentes")

# salvando gráficos
ggsave("dplots1_westeu.png", plot = multiplot(dplot1, dplot2, cols = 2), 
       device = "png",  width = 8, height = 3, units = "in")

ggsave("dplots2_westeu.png", plot = multiplot(dplot3, dplot4, cols = 2), device = "png",
       width = 8, height = 3, units = "in")

##### ggpairs #####
ggally_mysmooth <- function(data, mapping, ...){
  ggplot(data = data, mapping=mapping) +
    geom_density(adjust = 2)
}

# correlação e densidade das variáveis de interesse no modelo
pairs1 <- essdefr %>% select(imwbcnt, noimbro, stfgov) %>%  
  ggpairs(lower = "blank", diag =  list(continuous = wrap(ggally_densityDiag, adjust = 3))) +
  theme_void() +
  ggtitle("Correlação e densidade das variáveis de interesse") +
  labs(caption = "Figura 7
Elaboração do autor com base nos dados do ESS") +
theme(plot.title = element_text(hjust = .5, size = 12, face = "bold"),
      plot.caption = element_text(hjust = .5))

ggsave("ggpairs1_westeu.png", plot = pairs1, device = "png", width = 6, height = 3.5)

##### tabelas #####
# teste-t para as variáveis 
tt_lrscale <- t.test(essrrp$lrscale, essgeneral$lrscale) 
tt_imw <- t.test(essgeneral$imwbcnt, essrrp$imwbcnt)
tt_imu <- t.test(essgeneral$imueclt, essrrp$imueclt)
tt_imd <- t.test(essrrp$imdfetn, essgeneral$imdfetn)

# convertendo os objetos para data dataframe
tt_lrscale <- tt_lrscale %>% tidy()
tt_imw <- tt_imw %>% tidy()
tt_imu <- tt_imu %>% tidy()
tt_imd <- tt_imd %>% tidy()

# juntando os testes t
t <- bind_rows(tt_lrscale, tt_imw, tt_imu, tt_imd)

# função para renomear linhas e colunas da tabela
stargazer_htest = function (data, ...) {
  summary = data.frame(`T-statistic` = data$statistic,
                       DF = data$parameter,
                       `P value` = data$p.value,
                       `Alternative hypothesis` = data$alternative,
                       check.names = FALSE)
  stargazer(summary, flip = TRUE, summary = FALSE,
            notes = paste(data$method, data$data.name, sep = ': '), ...)
}

# stargazer t-test
stargazer_htest(t, column.labels = c("lrscale", "imwbcnt", "imueclt", "imdfetn"), 
                out = "t-test.htm", type = "html")
