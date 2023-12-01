# OCN7941 UFSC - ANALISE EXPLORATORIA E MODELAGEM PREDITIVA EM OCEANOGRAFIA (2022.2)

setwd("C:/Users/dell/OneDrive/Desktop/TCC Carla Leite")

df <-read.csv2("Tab_TCC1.csv")
df1 <-read.csv2("Tab_Correl.csv")

df$BACIA<-as.factor(df$BACIA)
df$CL_PROF<-as.factor(df$CL_PROF)
df$MASSADAGUA<-as.factor(df$MASSADAGUA)

library(ggplot2)
library(ggpubr)

summary(df)

## RESUMO ESTATISTICO

## HISTOGRAMA DE FREQUENCIA (Globocassidulina spp; G. crassa, G. subglobosa)

ggplot(df, aes(x = GBC)) +
  geom_histogram(aes(fill = ..count..), binwidth = 10, boundary = 5, color="gray", fill="seagreen")+       # ou bins = numero de classes (n?o pode usar bins e binwidth simultaneamente)
  scale_y_continuous(name = "Frequencia Absoluta") +                                                      #boundary = 5   os intervalos comecem com n?meros que terminem em 5
  scale_x_continuous(name = "Abundancia relativa Globocassidulina spp.", 
                     breaks = seq(0, 80, 10)) +
  ggtitle("Globocassidulina spp") +
  theme_classic() +
  theme(axis.title = element_text(size = 12, colour = "black"),                               # modifica o tamanho e fonte do titulo  
        axis.text.x = element_text(size = 10, colour = "black", angle = 0, hjust = 1),       # modifica o texto do eixo X
        axis.text.y = element_text(size = 10, colour = "black"),                              # modifica o texto do eixo Y
        axis.ticks = element_line(colour = "black"),                                          # cor dos marcadores
        axis.ticks.length = unit(.1, "cm"),                                                   # tamanho dos marcadores
        panel.grid =  element_blank(),
        panel.border = element_rect(colour = "black", fill = "NA", size = 0.5))    

## HISTOGRAMA DE FREQUENCIA POR GRUPO (apenas G spp)

ggplot(df, aes(x = GBC)) +
  geom_histogram(aes(fill = BACIA), binwidth = 10, boundary = 5, alpha = 0.6)+       # ou bins = numero de classes (n?o pode usar bins e binwidth simultaneamente)
  scale_y_continuous(name = "Frequencia Absoluta") +                                                      #boundary = 5   os intervalos comecem com n?meros que terminem em 5
  scale_x_continuous(name = "Abundancia relativa Globocassidulina spp.", 
                     breaks = seq(0, 80, 10)) +
  ggtitle("Globocassidulina spp") +
  theme_classic() +
  theme(axis.title = element_text(size = 12, colour = "black"),                               # modifica o tamanho e fonte do titulo  
        axis.text.x = element_text(size = 10, colour = "black", angle = 0, hjust = 1),       # modifica o texto do eixo X
        axis.text.y = element_text(size = 10, colour = "black"),                              # modifica o texto do eixo Y
        axis.ticks = element_line(colour = "black"),                                          # cor dos marcadores
        axis.ticks.length = unit(.1, "cm"),                                                   # tamanho dos marcadores
        panel.grid =  element_blank(),
        panel.border = element_rect(colour = "black", fill = "NA", size = 0.5))    


## TESTES DE NORMALIDADE

## DIAGRAMA DE CAIXA COM TESTES DE COMPARA??ES

library(ggpubr) 

boxplot(df$PROF, col = c("seagreen"), varwidth = TRUE, notch = TRUE)   #tamanho proporcional aos tamanhos dos grupos (varwidth = TRUE)

# criar listas de comparacoes
GR_BACIA <- list( c("PELOTAS", "SANTOS"), c("PELOTAS", "CAMPOS"), c("PELOTAS", "ESP_STO"), c("SANTOS", "CAMPOS"),  c("SANTOS", "ESP_STO"), c("CAMPOS", "ESP_STO") )

GR_PROF <- list( c("<30", "30a80"), c("<30", "80a200"), c("<30", "200a1000"), c("<30", ">1000"),
                 c("30a80", "80a200"), c("30a80", "200a1000"), c("30a80", ">1000"),
                 c("80a200", "200a1000"), c("80a200", ">1000"), 
                 c("200a1000", ">1000"))

GR_AGUA <- list( c("TW", "SACW"), c("TW", "AAIW"), c("TW", "NADW"), 
                 c("SACW", "AAIW"), c("SACW", "NADW"), 
                 c("AAIW", "NADW")) 


# definir ordem de apresenta??o no grafico
library(forcats)

df$BACIA <- fct_relevel(df$BACIA, "PELOTAS", "SANTOS", "CAMPOS", "ESP_STO")
df$CL_PROF <- fct_relevel(df$CL_PROF, "<30", "30a80", "80a200", "200a1000", ">1000")
df$MASSADAGUA <- fct_relevel(df$MASSADAGUA, "TW", "SACW", "AAIW", "NADW")


#plotar diagrama de caixa
ggboxplot(df, x = "BACIA", y = "GBC",
          color = "BACIA")+ 
  scale_y_continuous(name = "Abundancia Globocassidulina (%)") +
  stat_compare_means(comparisons = GR_BACIA, method = "wilcox.test", label = "p.signif") +      # Add pairwise comparisons p-value
  stat_compare_means(label.y = 120,         #altura no eixo Y onde sera apresentado o nivel de significancia
                     hide.ns = FALSE,   #apresenta o simbolo ns (nao significativo)
                     method = "kruskal.test")    #t.test, t.test, t.test, wilcox.test, anova, kruskal.test

ggboxplot(df, x = "CL_PROF", y = "GBC",
          color = "CL_PROF")+ 
  scale_y_continuous(name = "Abund?ncia Globocassidulina spp (%)") +
  stat_compare_means(comparisons = GR_PROF, method = "wilcox.test", label = "p.signif") +      # Add pairwise comparisons p-value
  stat_compare_means(label.y = 150 ,         #altura no eixo Y onde sera apresentado o nivel de significancia
                     hide.ns = FALSE,
                     method = "kruskal.test")   #apresenta o simbolo ns (nao significativo)   #t.test, t.test, t.test, wilcox.test, anova, kruskal.test


ggboxplot(df, x = "MASSADAGUA", y = "GBC",
          color = "MASSADAGUA")+ 
  scale_y_continuous(name = "Abund?ncia Globocassidulina spp (%)") +
  stat_compare_means(comparisons = GR_AGUA, method = "wilcox.test", label = "p.signif") +      # Add pairwise comparisons p-value
  stat_compare_means(label.y = 120,         #altura no eixo Y onde sera apresentado o nivel de significancia
                     hide.ns = FALSE,   #apresenta o simbolo ns (nao significativo)
                     method = "kruskal.test")    #t.test, t.test, t.test, wilcox.test, anova, kruskal.test

## DIAGRAMAS DE DISPERS?O

ggplot(df, aes(x = df$PROF, y = df$GBC)) +
  geom_point(color = "seagreen", fill = "seagreen", shape = 21, size = 3, alpha = .7) +
  xlab("profundidade") +          #opcional: + lims(x = c(16, 40), y = c(3, 7)) +
  ylab("% Globocassidulina spp") + 
  geom_smooth(method=NULL, se=FALSE, color="black")+
  theme_classic()


ggplot(df, aes(x = df$SAL, y = df$GBC)) +
  geom_point(color = "seagreen", fill = "seagreen", shape = 21, size = 3, alpha = .7) +
  xlab("salinidade") +          #opcional: + lims(x = c(16, 40), y = c(3, 7)) +
  ylab("% Globocassidulina spp") + 
  geom_smooth(method=NULL, se=FALSE, color="black")+
  theme_classic()


ggplot(df, aes(x = df$TEMP, y = df$GBC)) +
  geom_point(color = "seagreen", fill = "seagreen", shape = 21, size = 3, alpha = .7) +
  xlab("temperatura") +          #opcional: + lims(x = c(16, 40), y = c(3, 7)) +
  ylab("% Globocassidulina spp") + 
  geom_smooth(method=NULL, se=FALSE, color="black")+
  theme_classic()

# Matriz de Correlacao
library(Hmisc)
tab_r <- rcorr(as.matrix(df1[-1]),type="spearman")

