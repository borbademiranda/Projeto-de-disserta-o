# packages
library(tidyverse)
library(broom)
library(lme4)
library(ggplot2)
library(dotwhisker)
library(stargazer)
library(GGally)
library(zeligverse)
library(ggrepel)

# options
options(scipen = 999)

# working directory
setwd("./scripts")

##### loading datasets #####
cleafr <- read.csv("clea_fr.csv")
essfr <- read.csv("ess_fr.csv")

# loading datasets with region identification
essreg <- read.csv("ESSregions.csv")
essfrreg <- essreg[essreg$cntry == "FR",]

essfrregvars <- essfrreg %>% select(idno, cregion)

essfrregvars$cregion <- essfrregvars$cregion %>% as.character()

essfrregvars$cregion[essfrregvars$cregion == ""] <- NA

# merging datasets
essfr <- essfr %>% left_join(essfrregvars, by = "idno")


# renaming observations
cleafr$pty_n[cleafr$pty_n == "National Front"] <- "front national"

# subsetting FN data
cleafn <- cleafr[cleafr$pty_n == "front national",]
essfn <- essfr[essfr$vote_rrp == 1,]

# ess without FN voters
essgeneral <- essfr[essfr$vote_rrp == 0,]

##### descriptive analysis #####
# line plot 1 - FN average vote share by election year 
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

# line plot 2 - constituency level vote share by year of election
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
# line plot 3 - NPD average vote share by year
line3 <- ggplot(cleadefr[ctr_n == "Germany" & pty_n %in% c("nationaldemokratische partei deutschlands",
                                                           "NPD"),], 
                aes(x = yr, y = pvs1)) +
  theme_classic() +
  stat_summary(fun.y = mean, geom = "line") +
  scale_x_continuous(limits = c(1965, 2013), breaks = c(1965, 1969, 1972, 1976, 1980,
                                                      1983, 1987, 1990, 1994, 1998,
                                                      2002, 2005, 2009, 2013)) +
  theme(plot.title = element_text(face = "bold", size = 12, hjust = .5)) +
  ggtitle("Média de votos do NPD por ano de eleição") +
  labs(x = "Ano", y = "Média de votos (%)")
  
# line plot 4 - NPD constituency level vote share by year of election
line4 <- ggplot(cleadefr[ctr_n == "Germany" & pty_n %in% c("nationaldemokratische partei deutschlands",
                                                           "NPD"),], 
                aes(x = yr, y = pvs1)) +
  geom_line(aes(group = cst), alpha = .1) +
  geom_smooth(method = "lm", colour = "red", se = F, na.rm = T) +
  geom_smooth(colour = "yellow", se = F, method = "loess", na.rm = T) +
  theme_classic() +
  ggtitle("Porcentagem de votos do NPD nas constituências por ano de eleição") +
  labs(x = "Ano", y = "Porcentagem de votos") +
  theme(plot.title = element_text(size = 12, hjust = 1, face = "bold")) +
  scale_x_continuous(limits = c(1965, 2013), breaks = c(1965, 1969, 1972, 1976, 1980,
                                                        1983, 1987, 1990, 1994, 1998,
                                                        2002, 2005, 2009, 2013))

# creating function to make density plots
dplot <- function(VB, VW, Numero, Titulo) {
  ggplot() +
  geom_density(data = essgeneral, aes(x = VB, fill = "black"), alpha = .3,
               adjust = 1.5) +
  geom_density(data = essfn, aes(x = VW, fill = "orange"), alpha = .3) +
  scale_x_continuous(name = "", breaks = 0 : 10, limits = c(0,10), expand = c(.01,.01)) +
  scale_y_continuous(name = "", breaks =  NULL, labels = NULL, limits = c(0, .3)) +
  scale_fill_identity(name = "", labels = c("Parties", "Voters")) +
  ggtitle(Titulo) +
  theme_classic(base_size = 12) +
  theme(legend.position = c(.3, .7), legend.background = element_blank(), 
        legend.text = element_text(size = 7), 
        plot.title = element_text(size = 10, hjust = 0.5, face = "bold"), 
        axis.text.x = element_text(size = 7))
}

# density plot 1 - voters' left-right self positioning divided by general 
# electorate and fn voters
dplot1 <- dplot(essgeneral$lrscale, essfn$lrscale, 1, 
                "Auto posicionamento numa escala esquerda-direita") +
  scale_fill_identity(name = "", labels = c("Eleitorado", "Eleitor FN"), 
                      guide = "legend")

# density plot 2 - voters' left right self positioning by survey year
dplot2 <- ggplot(data = essfr, aes(x = lrscale)) +
  geom_density(aes(group = essround))

##### testing models #####
attach(essfr)
logit1 <- glmer(data = essdefr, vote_rrp ~ imwbcnt + stfgov + prtdgcl + polintr +
                  (1 | regionfr), family = "binomial")
summary(logit1, correlation = T)

car::vif(logit1)

logit2 <- zelig(data = essfr, vote_rrp ~ imwbcnt + stfgov + prtdgcl + polintr, model = "relogit",
                cite = F)
summary(logit2)

# testing correlation between the variables "immigration makes country better or
# worse" and "immigration undermines or enrich country culturally"

# storing two objects with the variables
x <- essfr[43] 
y <- essfr[42]

# correlation
r <- cor(x, y, method = "pearson", use = "complete.obs")

# ggpairs with three variables
pairs1 <- essfr %>% select(imwbcnt, imueclt, stfdem) %>%  
  ggpairs(lower = "blank") +
  theme_void()

ggplot(data = tidy_m1, aes(x = imwbcnt, y = probs)) +
  geom_point() +
  stat_smooth(method = "glm", se=FALSE)

tidy_m1 <- tidy(logit1)

beta1 <- logit1$coefficients
beta1 <- exp(beta1) / (1 + exp(beta1))
int1 <- confint(logit1)
int1 <- exp(int1) / (1 + exp(int1))

tidy_m1$probs <- beta1
tidy_m1$conf <- int1

################################ VERSÃO 27/03 ##################################
# packages
library(tidyverse)
library(broom)
library(lme4)
library(ggplot2)
library(dotwhisker)
library(stargazer)
library(GGally)
library(zeligverse)
library(ggrepel)

setwd("C:/Users/test/Desktop/Projeto de dissertação")

##### loading datasets #####
cleadefr <- read.csv("./scripts/clea_de-fr.csv")
essdefr <- read.csv("./scripts/ess_westeu.csv")
de17 <- read.csv("./scripts/2017.csv")
bwl17 <- read.csv("./scripts/btw17_wbz_zweitstimmen.csv", sep = ";")
cleaeu <- read.csv("scripts/clea_westeu.csv")

# subsetting rrp parties
rrpaus <- cleaeu[cleaeu$pty %in% c("9", "21") & cleaeu$ctr_n == "Austria",]
rrpbe <- cleaeu[cleaeu$pty %in% c("8", "36") & cleaeu$ctr_n == "Belgium",] 
rrpdk <- cleaeu[cleaeu$pty == "6" & cleaeu$ctr_n == "Denmark",]
rrpfi <- cleaeu[cleaeu$pty == "38" & cleaeu$ctr_n == "Finland",]
rrpfr <- cleaeu[cleaeu$pty %in% c("15", "7") & cleaeu$ctr_n == "France",]
rrpge <- cleaeu[cleaeu$pty %in% c("26", "42", "70", "96") & cleaeu$ctr_n == "Germany",]
rrpgr <- cleaeu[cleaeu$pty %in% c("67", "94", "62") & cleaeu$ctr_n == "Greece",]
rrpit <- cleaeu[cleaeu$pty %in% c("1", "15", "21") & cleaeu$ctr_n == "Italy",]
rrpsd <- cleaeu[cleaeu$pty %in% c("16", "26") & cleaeu$ctr_n == "Sweden",]
rrpuk <- cleaeu[cleaeu$pty %in% c("11", "139") & cleaeu$ctr_n == "UK",]
rrpnl <- cleaeu[cleaeu$pty %in% c("71", "56", "5") & cleaeu$ctr_n == "Netherlands",]

# binding datasets
clea_rrp <- bind_rows(rrpaus, rrpbe, rrpdk, rrpfi, rrpfr, rrpge, rrpgr, rrpit, 
                      rrpnl, rrpsd, rrpuk)


# renaming observations
clea_rrp$pty_n <- as.character(clea_rrp$pty_n) 
cleadefr$pty_n[cleadefr$pty_n == "National Front"] <- "front national"
clea_rrp$pty_n[clea_rrp$pty_n %in% c("FPÖ", "freiheitliche partei österreichs")] <- "FPÖ"
clea_rrp$pty_n[clea_rrp$pty_n %in% c("lega nord", "Lega")] <- "LN"
clea_rrp$pty_n[clea_rrp$pty_n %in% c("PS", "perussuomalaiset")] <- "PS"
clea_rrp$pty_n[clea_rrp$pty_n %in% c("Partij voor de Vrijheid", "partij voor de vrijheid")] <- "PVV"
clea_rrp$pty_n[clea_rrp$pty_n == "alleanza nazionale"] <- "AN"
clea_rrp$pty_n[clea_rrp$pty_n == "lijst pim fortuyn"] <- "LPF"
clea_rrp$pty_n[clea_rrp$pty_n == "Flemish Interest (VB)"] <- "VB"
clea_rrp$pty_n[clea_rrp$pty_n %in% c("front national", "National Front") &
                 clea_rrp$ctr_n == "France"] <- "FN"
clea_rrp$pty_n[clea_rrp$pty_n %in% c("front national", "National Front (FN)")] <- "FN (Belgium)"
clea_rrp$pty_n[clea_rrp$pty_n %in% c("Anexartitoi Ellines", 
                                     "Anexartitoi Ellines - Ethniki Patriotiki Symmachia",
                                     "Anexartitoi Ellines - Panos Kammenos")] <- "ANEL"
clea_rrp$pty_n[clea_rrp$pty_n == "Sweden Democrats"] <- "SD"
clea_rrp$pty_n[clea_rrp$pty_n %in% c("UK Independence Party", "united kingdom independence party")] <- "UKIP"
clea_rrp$pty_n[clea_rrp$pty_n %in% c("bündnis zukunft österreich", "BZO")] <- "BZO"
clea_rrp$pty_n[clea_rrp$pty_n %in% c("danks folkeparti", "dansk folkeparti")] <- "DK"
clea_rrp$pty_n[clea_rrp$pty_n == "ny demokrati"] <- "ND"
clea_rrp$pty_n[clea_rrp$pty_n == "Laikos Syndesmos - Chrysi Aygi"] <- "XA"
clea_rrp$pty_n[clea_rrp$pty_n == "movimento sociale italiano"] <- "MSI"
clea_rrp$pty_n[clea_rrp$pty_n %in% c("Popular Orthodox Rally", 
                                     "Laikos Orthodoxos Synagermos (LaOS)", 
                                     "LaOS")] <- "LAOS"
clea_rrp$pty_n[clea_rrp$pty_n %in% c("nationaldemokratische partei deutschlands", 
                                     "NPD")] <- "NPD"
clea_rrp$pty_n[clea_rrp$pty_n %in% c("die republikaner", "REP")] <- "REP"
clea_rrp$pty_n[clea_rrp$pty_n %in% c("Movement for France", "mouvement pour la france")] <- "MPF"
clea_rrp$pty_n[clea_rrp$pty_n == "British National Party"] <- "BNP"
clea_rrp$pty_n[clea_rrp$pty_n == "centrumdemocraten"] <- "CD"
clea_rrp$pty_n[clea_rrp$pty_n == "deutsche volksunioin"] <- "DVU"               

write.csv(clea_rrp, "clea_westeu_rrp.csv")

rm(list = c("cleaeu", "rrpaus", "rrpbe", "rrpdk", "rrpfi", "rrpfr", "rrpge", 
            "rrpgr", "rrpit", "rrpnl", "rrpsd", "rrpuk"))

clea_rrp <- read.csv("clea_westeu_rrp.csv")

##### inverting variables values #####
essdefr$imwbcnt <- max(essdefr$imwbcnt, na.rm = T) - essdefr$imwbcnt
essdefr$imueclt <- max(essdefr$imueclt, na.rm = T) - essdefr$imueclt

# subsetting rrp data
essrrp <- essdefr[essdefr$vote_rrp == 1,]

# ess without rrp voters
essgeneral <- essdefr[essdefr$vote_rrp == 0,]

##### descriptive analysis #####
attach(cleadefr)

# line plot 1 - FN average vote share by election year 
line1 <- ggplot(cleadefr[cleadefr$pty_n == "front national",], aes(x = yr, y = pvs1)) +
  theme_classic() +
  scale_x_continuous(limits = c(1978, 2017), breaks = c(1978, 1981, 1986, 1988,
                                                        1993, 1997, 2002, 2007, 
                                                        2012, 2017)) +
  theme(plot.title = element_text(face = "bold", hjust = .5, size = 12),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "Ano", y = "Média de votos (%)") +
  ggtitle("Média de votos do FN por ano de eleição") +
  stat_summary(fun.y = mean, geom = "line")

ggsave("FN_vote_timeseries.png", plot = line1, device = "png", width = 6, height = 3.5)

# line plot 2 - constituency level vote share by year of election
line2 <- ggplot(cleadefr[cleadefr$pty_n == "front national",], aes(x = yr, y = pvs1)) +
  geom_line(aes(group = cst), alpha = .1) +
  geom_smooth(method = "lm", colour = "red", se = F) +
  theme_classic() +
  ggtitle("Porcentagem de votos do FN nas constituências por ano de eleição") +
  labs(x = "Ano", y = "Porcentagem de votos") +
  theme(plot.title = element_text(size = 12, hjust = .5, face = "bold")) +
  scale_x_continuous(limits = c(1978, 2017), breaks = c(1978, 1981, 1986, 1988,
                                                        1993, 1997, 2002, 2007, 
                                                        2012, 2017))

ggsave("FN_cst_level_timeseries.png", plot = line2, device = "png", width = 6, height = 4)

# line plot 3 - NPD average vote share by year
line3 <- ggplot(cleadefr[cleadefr$ctr_n == "Germany" & cleadefr$pty_n %in% c("nationaldemokratische partei deutschlands",
                                                                             "NPD"),], 
                aes(x = yr, y = pvs1)) +
  theme_classic() +
  stat_summary(fun.y = mean, geom = "line") +
  scale_x_continuous(limits = c(1965, 2013), breaks = c(1965, 1969, 1972, 1976, 1980,
                                                        1983, 1987, 1990, 1994, 1998,
                                                        2002, 2005, 2009, 2013)) +
  theme(plot.title = element_text(face = "bold", size = 12, hjust = .5)) +
  ggtitle("Média de votos do NPD por ano de eleição") +
  labs(x = "Ano", y = "Média de votos (%)")

ggsave("NPD_timeseries.png", plot = line3, device = "png", width = 6, height = 3.5)

# line plot 4 - NPD constituency level vote share by year of election
line4 <- ggplot(cleadefr[cleadefr$ctr_n == "Germany" & cleadefr$pty_n %in% c("nationaldemokratische partei deutschlands",
                                                                             "NPD"),], 
                aes(x = yr, y = pvs1)) +
  geom_line(aes(group = cst), alpha = .1) +
  geom_smooth(method = "lm", colour = "red", se = F, na.rm = T) +
  theme_classic() +
  ggtitle("Porcentagem de votos do NPD nas constituências por ano de eleição") +
  labs(x = "Ano", y = "Porcentagem de votos") +
  theme(plot.title = element_text(size = 12, hjust = .5, face = "bold")) +
  scale_x_continuous(limits = c(1965, 2013), breaks = c(1965, 1969, 1972, 1976, 1980,
                                                        1983, 1987, 1990, 1994, 1998,
                                                        2002, 2005, 2009, 2013))

ggsave("NPD_cst_level_timeseries.png", plot = line4, device = "png", width = 8, height = 5)

# lineplot 5 - média de votos dos cinco maiores partidos por ano
line5 <- ggplot() +
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
  

# boxplot 1 - AfD vote distribution 2013
cleadefr$east <- as.factor(ifelse(cleadefr$sub %in% c("Thüringen", "Sachsen-Anhalt",
                                                      "Brandenburg", "Sachsen", 
                                                      "Mecklenburg-Vorpommern"), 1, 0))

box1 <- ggplot(cleadefr[ctr_n == "Germany" & pty_n %in% c("AfD", "alternative for germany"),], 
               aes(y = pvs1, x = factor(sub), fill = east)) +
  geom_boxplot(width = .5) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12, hjust = .5),
        legend.title = element_text(face = "bold", size = 10, hjust = .5)) +
  ggtitle("Distribuição dos votos na AfD por região nas eleições de 2013") +
  labs(x = "Região", y = "Média de votos (%)") +
  scale_color_continuous(name = "Leste")

ggsave("boxplot_AfD_2013.png", plot = box1, device = "png", width = 6, height = 4)

# boxplot 2 - AfD vote distribution 2017
box2 <- ggplot(de17[de17$Registered.Electors == "Alternative for Germany (AfD)",],
               aes(y = X, x = factor(X2017))) +
  geom_boxplot(width = .3) +
  geom_point(position = position_jitter(.15)) +
  stat_summary(fun.y = mean, geom = "point", colour = "red", size = 1.5) +
  theme_minimal() +
  ggtitle("Boxplot da distribuição dos votos da AfD nas eleições de 2017") +
  theme(plot.title = element_text(size = 12, hjust = .5, face = "bold")) +
  labs(x = "Eleição", y = "Média de votos (%)") +
  geom_label_repel(data = subset(de17[de17$Registered.Electors == "Alternative for Germany (AfD)",], 
                                 X > 22), 
                   aes(factor(X2017), X, label = DE),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50', direction = "x")

ggsave("boxplot_AfD_2017_general.png", plot = box2, device = "png", width = 6, height = 4)  

# boxplot 3 - AfD vote distribution 2017 by land

# creating variable for east/west land
bwl17$east <- as.factor(ifelse(bwl17$Land %in% c(13, 12, 15, 14, 16), 1, 0))

# boxplot - booyah, motherfuckeeeer
box3 <- ggplot(bwl17, aes(x = factor(Land), y = AfD, fill = east)) +
  geom_boxplot() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = .5)) +
  ggtitle("Distribuição dos votos na AfD por Land na Alemanha") +
  labs(x = "Região", y = "Número de votos AfD")

ggsave("boxplot_AfD_2017.png", plot = box3, device = "png", width = 6, height = 4)

# função para nomear outliers
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

bwl17 %>% mutate(outlier = ifelse(is_outlier(AfD), AfD, as.numeric(NA))) %>%
  ggplot(., aes(x = factor(Land), y = AfD)) +
  geom_boxplot() +
  geom_text(aes(label = outlier))

##### carregando banco de dados com observações da europa ocidental #####
clea_rrp <- read.csv("./scripts/clea_rrp.csv")

# boxplot 4 - distribuição dos votos da direita radical por país
clea_rrp$pvs1[clea_rrp$pvs1 %in% c(-990, -992, -994)] <- NA

box4 <- ggplot(data = clea_rrp, aes(x = reorder(ctr_n, +pvs1), y = pvs1)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = "point", colour = "red", size = 1.5) +
  coord_flip() +
  theme_classic() +
  labs(x = "País", y = "Porcentagem de votos", caption = "Figura 2
    Elaboração do autor com base nos dados do CLEA") +
  ggtitle("Distribuição dos votos da direita radical por país") +
  theme(plot.title = element_text(size = 12, hjust = .5, face = "bold"),
        plot.caption = element_text(hjust = .5)) +
  scale_y_continuous(breaks = seq(.0, .7, .05))
ggsave("box_rrpp_by_ctr.png", plot = box4, device = "png", width = 6, height = 4)

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
ggsave("hist_partymean.png", plot = hist1, device = "png", width = 5, height = 5)

box5 <- ggplot(data = clea_rrp, aes(x = reorder(ctr_n, -pvs1), y = pvs1, fill = pty_n)) +
  geom_boxplot(position = "dodge2")

detach(cleadefr)

##### functions #####

# creating function to make density plots
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

# function to density plot with smaller x scale
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

# multiplot function
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


# density plot 1 - voters' left-right self positioning divided by general 
# electorate and rrp voters
dplot1 <- dplot(essgeneral$lrscale, essrrp$lrscale, 1, 
                "Auto posicionamento numa escala esquerda-direita") +
  scale_fill_identity(name = "", labels = c("Eleitorado", "Eleitor RRPP"), 
                      guide = "legend")

# density plot 2 - voters' perception about immigrants
dplot2 <- dplot(essgeneral$imwbcnt, essrrp$imwbcnt, 2, "Imigrantes fazem do país um lugar pior")

# density plot 3 - voters' feelings about the cultural impact of immigrants
dplot3 <- dplot(essgeneral$imueclt, essrrp$imueclt, 3, "Imigrantes prejudicam a cultura do país")

# density plot 4 - it is important to folow customs and traditions
dplot4 <- dplotb(essgeneral$imdfetn, essrrp$imdfetn, 4, "Restrição de imigrantes de grupos étnicos diferentes")

# saving plots
ggsave("dplots1_westeu.png", plot = multiplot(dplot1, dplot2, cols = 2), 
       device = "png",  width = 8, height = 3, units = "in")

ggsave("dplots2_westeu.png", plot = multiplot(dplot3, dplot4, cols = 2), device = "png",
       width = 8, height = 3, units = "in")

##### t-test #####
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

# line plot of the mean satisfaction with government
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

# plot mean trust in politicians
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

# plot mean satisfaction with government of populist voters
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

# plot mean trust in politicians populist voters
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

ggsave("stf.png", plot = multiplot(line6, line8, cols = 2), device = "png", 
       height = 3, width = 8)

ggsave("trust.png", plot = multiplot(line7, line9, cols = 2), device = "png", 
       height = 3, width = 8)

# criando dummy para os muito insatisfeitos e pouco confiantes
essgeneral$bin_stfgov <- ifelse(essgeneral$stfgov > 7, 1, 0)

perc$stfgov <- as.tibble(essgeneral$bin_stfgov == 1 / essgeneral$bin_stfgov)

##### ggpairs das variáveis de interesse utilizadas nos modelos #####
ggally_mysmooth <- function(data, mapping, ...){
  ggplot(data = data, mapping=mapping) +
    geom_density(adjust = 2)
}

pairs1 <- essdefr %>% select(imwbcnt, noimbro, stfgov, trstplt) %>%  
  ggpairs(lower = "blank", diag =  list(continuous = wrap(ggally_densityDiag, adjust = 3))) +
  theme_void() +
  ggtitle("Correlação e densidade das variáveis de interesse") +
  labs(caption = "Figura 7
Elaboração do autor com base nos dados do ESS")
  theme(plot.title = element_text(hjust = .5, size = 12, face = "bold"),
        plot.caption = element_text(hjust = .5))

ggsave("ggpairs1_westeu.png", plot = pairs1, device = "png", width = 6, height = 3.5)

##### testing models #####
library(lme4)
attach(essdefr)
library(sjPlot)
library(sjmisc)

logit1 <- glmer(data = essdefr, vote_rrp ~ imwbcnt + (1 | cregion), family = "binomial")
summary(logit1)

logit2 <- glmer(data = essdefr, vote_rrp ~ noimbro + (1 | cregion), 
                family = "binomial",
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(logit2)

logit3 <- glmer(data = essdefr, vote_rrp ~ imwbcnt*noimbro + (1 | cregion), 
              family = "binomial",
              control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(logit3)

logit4 <- glmer(data = essdefr, vote_rrp ~ imwbcnt + noimbro + imwbcnt*noimbro +
                stfgov + trstplt + gndr + agea + eduyrs + blue_c + (1 | cregion), 
              family = "binomial")
summary(logit4)

logit6 <- glmer(data = essdefr, vote_rrp ~ imwbcnt + noimbro + imwbcnt*noimbro +
                  mean(essdefr$stfgov) + mean(essdefr$trstplt) + mean(essdefr$gndr) + 
                  mean(essdefr$agea) + mean(essdefr$eduyrs) + mean(essdefr$blue_c) + (1 | cregion), 
                family = "binomial")

# convertendo modelos para formato tidy
m1_tidy <- tidy(logit1)
m2_tidy <- tidy(logit2)
m3_tidy <- tidy(logit3)
m4_tidy <- tidy(logit4)

# modifying model names
# model 1
m1_tidy <- m1_tidy %>% 
  mutate(model = "Modelo 1")

# repeat for model 2
m2_tidy <- m2_tidy %>% 
  mutate(model = "Modelo 2")

# model3 
m3_tidy <- m3_tidy %>% mutate(model = "Modelo 3") 

# model 4
m4_tidy <- m4_tidy %>% mutate(model = "Modelo 4")

# binding model data frames together
all_models <- bind_rows(m1_tidy, m2_tidy, m3_tidy, m4_tidy)

# renaming vars
all_models <- all_models %>% 
  relabel_predictors(c("imwbcnt" = "Percepção imigração",
                       "sd_(Intercept).cregion" = "S.D. região",
                       "noimbro" = "Nº de imigrantes",
                       "imwbcnt:noimbro" = "Imwbcnt*Nº de imigrantes",
                       "stfgov" = "Satisfação governo",
                       "trstplt" = "Confiança políticos",
                       "gndr" = "Gênero",
                       "agea" = "Idade (anos)",
                       "eduyrs" = "Educação (anos)",
                       "blue_c" = "Blue-collar"))

# gráfico de coeficientes
coef1 <- dwplot(all_models, show_intercept = F,
                vline = geom_vline(xintercept = 0, colour = "grey 60", linetype = 2)) +
  theme_minimal() +
  ggtitle("Coeficientes dos modelos logísticos multinível") +
  labs(x = "Coeficientes com C.I. 95% (Em log odds)") +
  theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
       legend.title = element_text(size = 10, face = "bold"),
       plot.caption = element_text(hjust = 0.5),
       legend.position = "right",
       legend.title.align = .5) + 
  scale_color_discrete(name = "Modelos")
ggsave("coef1.png", plot = coef1, device = "png", width = 6, height = 4)
  
# gráfico para probabilidades preditas
probs1 <- plot_model(logit4, type = "int", terms = c("imwbcnt", "noimbro [0, 50, 100")) +
  theme_minimal() +
  ggtitle("Probabilidades preditas para o voto na direita radical") +
  labs(y = "Probabilidade predita", x = "Percepção sobre imigração", color = "Nº imigrantes") +
  theme(plot.title = element_text(hjust = .3, face = "bold", size = 14),
        legend.title.align = .5) 
ggsave("probs1.png", plot = probs1, device = "png", width = 7, height = 4)

# modelo com interação de gênero

logit5 <- glmer(data = essdefr, vote_rrp ~ imwbcnt*noimbro*gndr + (1 | cregion), family = "binomial")
summary(logit5)

probs2 <- plot_model(logit5, type = "int") +
  theme_minimal() +
  ggtitle("Probabilidades preditas para o voto na direita radical") +
  labs(y = "Probabilidade predita", x = "Percepção sobre imigração", color = "Nº imigrantes") +
  theme(plot.title = element_text(hjust = .3, face = "bold", size = 14),
        legend.title.align = .5)

ggsave("probs2.png", plot = probs2, device = "png", width = 7, height = 4)
  
