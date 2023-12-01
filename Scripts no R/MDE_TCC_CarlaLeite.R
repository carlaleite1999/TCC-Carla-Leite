######  MODELO DE DISTRIBUICAO DE ESPECIE (SDM)  ######

## MAPA PREDITIVO DE ADEQUABILIDADE DO HABITAT (map of predicted habitat suitability)

#INSTALAR PACOTES

# install.packages("raster")
# install.packages("rgdal")
# install.packages("maps")
# install.packages("mapdata")
# install.packages("dismo")
# install.packages("rJava")
# install.packages("maptools")
# install.packages("jsonlite")

#CHAMAR PACOTES

library(raster)
library(rgdal)
library(maps)
library(mapdata)
library(dismo) 
library(rJava)
library(maptools)
library(jsonlite)
library(RColorBrewer)
library(ggplot2)

#CARREGAR CONJUNTO DE DADOS
setwd() #selecionar o diretório onde está o conjunto de dados
df <- read.csv2("Tab_MDE_total.CSV")
summary (df)

## CARREGAR DADOS DE PRESENCA (APENAS AS COLUNAS DE LONG E LAT SAO NECESSARIAS) # (os valores de lat e long precisam ser negativos)
df_pres <- read.csv2("MDE_presenca.CSV")  
df_pres <- df_pres[,2:3]

## CARREGAR DADOS DE AUSENCIA OBSERVADAS (APENAS AS COLUNAS DE LONG E LAT SAO NECESSARIAS) # (os valores de lat e long precisam ser negativos)
df_aus <- read.csv2("MDE_ausencia.csv") 
df_aus <- df_aus[,2:3]

## DEFINIR AS ESCALA DE CORES DOS MAPAS
#install.packages("viridis")
library(viridis)
my.colors1 <- viridis(2000)
#OU
#library(grdevices)
library(RColorBrewer)

my.colors2 <- colorRampPalette(rev(brewer.pal(11, 'RdYlBu')))(100)
my.colors3 <- colorRampPalette(rev(brewer.pal(9, 'Blues')))(100)     
my.colors4 = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127"))(100)
my.colors5 = colorRampPalette(c("darkblue","cornflowerblue","#EDF0C0"))(100)
my.colors6 <- colorRampPalette(rev(brewer.pal(9, 'Reds')))(100)

# PLOTAR AS ABUNDANCIAS (Buble plot)
ggplot(data = df, mapping = aes(x = LONG, y = LAT, size = Globocassidulina)) +
  geom_point(alpha = 0.5, shape = 21, color = "black", fill = "blue" , show.legend = FALSE) +
  scale_size(range = c(.1, 6), name = "Abundance (%)") +
  theme_minimal() +
  theme(legend.position = "right") +
  labs(
    subtitle = "", 
    y = "LAT", 
    x = "LONG", 
    title = "Abundância relativa do gênero Globocassidulina spp. ao longo do gradiente latitudinal")


### CARREGAR BASE DE DADOS OCEANOGRAFICOS   
# # https://www.bio-oracle.org/code.php
#https://tomjenkins.netlify.app/files/r-extract-marine-data-pdf.pdf

### install.packages("sdmpredictors")     #'sdmpredictors' provides simple functions for querying and downloading the data layers.
### install.packages("leaflet")  # leaflet allows to load google maps

library(sdmpredictors)

list_layers()
list_layers( datasets="Bio-ORACLE" )

# CARREGAR VARIAVEIS OCEANOGRAFICAS

ocean_data <- load_layers( layercodes = c("BO_bathymean", "BO2_tempmean_bdmean", "BO2_tempmax_bdmean" , "BO2_tempmin_bdmean",
                                          "BO2_tempmean_ss", "BO2_tempmax_ss", "BO2_tempmin_ss",
                                          "BO2_salinitymean_bdmean", "BO2_salinitymax_bdmean", "BO2_salinitymin_bdmean" ,
                                          "BO2_chlomean_ss", "BO2_chlomax_ss", "BO2_chlomin_ss", 
                                          "BO2_nitratemean_bdmean", "BO2_nitratemax_bdmean", "BO2_nitratemin_bdmean" ) , 
                           equalarea=FALSE, rasterstack=TRUE)

#AJUSTAR A ÁREA DE ESTUDO
south.atlantic.ext <- extent(-60, -35, -40, -18)   # ajuste a area de estudo
ocean.crop <- crop(ocean_data, south.atlantic.ext)

# EXTRAIR OS VALORES DAS VARIAVEIS OCEANOGRAFICAS PARA OS PONTOS DE PRESENCA (apenas as que serao usadas no modelo)
df_ocean <- extract(ocean.crop, df_pres)   # 3 e 4 sao as colunas da LONG e LAT
df_ocean <- cbind.data.frame(df_pres, as.data.frame(df_ocean))  # juntar as tab das coordenadas de presenca  e das variaveis ambientais
summary(df_ocean)

### PREPARAR DADOS PARA O MODELO - PARTICAO - Treino e Teste dataset

#df_frag <- cbind.data.frame(df_pres$LONG,df_pres$LAT) #first, just make a data frame of latitudes and longitudes for the model
fold <- dismo::kfold(df_ocean, k=4) # add an index that makes four random groups of observations (75% treino e 25 % teste)
df_test <- df_ocean[fold == 1, ] # 1/4 dos dados para teste
df_train <- df_ocean[fold != 1, ] # 3/4 dos dados para treino


# Criar mascara delimitando a area de aquisicao dos pontos
mask <- raster(ocean.crop)

df_pres_sp <- df_pres
coordinates(df_pres_sp) <- ~LONG+LAT                          # transforma em spatial points
projection(df_pres_sp) <- CRS('+proj=longlat +datum=WGS84')
x <- circles(df_pres_sp, d=50000, lonlat=TRUE)                # criar circulos de 5000 m em torno de cada ponto amostral
pol <- polygons(x)                                            # trabsformar em poligonos
plot(pol, axes=TRUE)                                          # visualizar poligonos

sample_aus <- spsample(pol, 100, type='random', iter=500)     # amostrar pontos desses poligonos (25 pontos)

polig_aus <- cellFromXY(mask, sample_aus)                     # transformar poligonos em uma unica celula
length(polig_aus)
polig_aus <- unique(polig_aus)                                      
length(polig_aus)

p_aus <- xyFromCell(mask, polig_aus)                            
plot(pol, axes=TRUE)
points(p_aus, cex=0.75, pch=20, col='blue')


### GERAR O MODELO USANDO O ALGORITMO MAXENT (Maximum Entropy)

# usando todas as variaveis do ocean_data (ocean.crop) 
mod_multi <- dismo::maxent(ocean.crop, df_train [,1:2])

# Contribuicao de cada variavel para o modelo 
plot(mod_multi)
print(mod_multi)          # vai abrir uma pagina html com varios graficos e tabelas destes resultados (http://localhost:28113/session/raster/maxent/9639697992/maxent.html)

# curva de resposta da especie para cada variavel
response(mod_multi)

# gerar valores previstos de presenca em funcao das variaveis incluidas no modelo
df.pred_multi <- dismo::predict(mod_multi, ocean.crop)

# Histograma dos valores de probabilidade
hist(df.pred_multi)

# PLOTAR O MAPA DE PROBABILIDADE DE OCORRENCIA DA ESPECIE
plot(df.pred_multi,col = my.colors1, axes = FALSE, box = FALSE, main="Probabilidade de Ocorrencia de Globocassidulina spp -  Multiplas Variaveis")
map('worldHires', fill=FALSE, add=TRUE)   #map('worldHires',xlim=c(min(df$LONG)-10,max(df$LONG)+10), ylim=c(min(df$LAT)-10,max(df$LAT)+10), fill=FALSE, add=TRUE)
points(df_pres$LONG, df_pres$LAT, pch="+", col = "black", cex=0.35)


### AVALIACAO DO MODELO

# com dados observados de ausencia
e_multi_aus <- evaluate(mod_multi, p=df_test[,1:2], a=df_aus, x=ocean.crop)  # OU pode ser a = bg sem difernciar treino e teste
plot(e_multi_aus, 'ROC')
print(e_multi_aus)          # resultados da avaliacao do modelo


# MODELO COM VARIAVEIS SELECIONADAS (apenas maiores contribuicoes)

ocean_data2 <- load_layers( layercodes = c("BO2_tempmin_bdmean", "BO2_tempmin_ss", "BO2_chlomin_ss",
                                           "BO2_nitratemean_bdmean", "BO_bathymean", "BO2_salinitymean_bdmean") , 
                            equalarea=FALSE, rasterstack=TRUE)

# recortar para os limites da area de estudo
south.atlantic.ext <- extent(-60, -35, -40, -18)   # ajuste a area de estudo
ocean.crop2 <- crop(ocean_data2, south.atlantic.ext)

# extrair dados apra os pontos de presenca
df_ocean_mod1 <- extract(ocean.crop2, df_pres)   # 3 e 4 s?o as colunas da LONG e LAT
df_ocean_mod1 <- cbind.data.frame(df_pres, as.data.frame(df_ocean_mod1))  # juntar as tab das coordenadas de presenca  e das variaveis ambientais
summary(df_ocean_mod1)

# Modelo MAXENT
mod_multi1 <- dismo::maxent(ocean.crop2, df_train [,1:2])

plot(mod_multi1)
print(mod_multi1)          # vai abrir uma pagina html com varios graficos e tabelas destes resultados (http://localhost:28113/session/raster/maxent/9639697992/maxent.html)

# Avaliacao do modelo com ausencias observadas
e_multi_aus1 <- evaluate(mod_multi1, p=df_test[,1:2], a=df_aus, x=ocean.crop2)  # OU pode ser a = bg sem difernciar treino e teste
plot(e_multi_aus1, 'ROC')
print(e_multi_aus1) 

# MAPA DE PROBABILIDADE DE OCORRENCIA DA ESPECIE (MAPA PREDITIVO DE ADEQUABILIDADE DO HABITAT - map of predicted habitat suitability)
# gerar valores previstos de presenca em funcao das variaveis incluidas no modelo
df.pred_multi1 <- dismo::predict(mod_multi1, ocean.crop2)
plot(df.pred_multi1,col = my.colors1, axes = FALSE, box = FALSE, main="Probabilidade de Ocorrência -  Multiplas Variaveis")
map('worldHires', fill=FALSE, add=TRUE)   #map('worldHires',xlim=c(min(df$LONG)-10,max(df$LONG)+10), ylim=c(min(df$LAT)-10,max(df$LAT)+10), fill=FALSE, add=TRUE)
points(df_pres$LONG, df_pres$LAT, pch="+", col = "black", cex=0.35)


### MAPAS DE DISTRIBUICAO DAS VARIAVEIS USADAS NO MODELO (CROP)

# "BO2_tempmin_bdmean", "BO2_tempmin_ss", "BO2_chlomin_ss", "BO2_nitratemean_bdmean", "BO_bathymean", "BO2_salinitymean_bdmean"

south.atlantic.ext <- extent(-60, -35, -40, -18)

## BATIMETRIA
bat_media <- load_layers("BO_bathymean")
bat_media.crop <- crop(bat_media, south.atlantic.ext)
plot(bat_media.crop, col = my.colors1, axes = FALSE, box = FALSE)
title(cex.sub = 1.25, sub = "MAPA DE DISTRIBUIÇÃO DA VARIÁVEL BATIMETRIA")
map('worldHires', fill = FALSE, add = TRUE)
points(df_pres$LONG, df_pres$LAT, col="black", pch=20, cex=0.35)

## TEMPERATURA DE FUNDO
tempmin_fundo <- load_layers("BO2_tempmin_bdmean")    #nome como consta na base Oracle
tempmin_fundo.crop <- crop(tempmin_fundo, south.atlantic.ext)
plot(tempmin_fundo.crop, col = my.colors1, axes = FALSE, box = FALSE)
title(cex.sub = 1.25, sub = "MAPA DE DISTRIBUIÇÃO - TEMPERATURA MIN DE FUNDO")
map('worldHires', fill = FALSE, add = TRUE)
points(df_pres$LONG, df_pres$LAT, col="black", pch=20, cex=0.35)

## TEMPERATURA DE SUPERFICIE
tempmin_sup <- load_layers("BO2_tempmin_ss")    #nome como consta na base Oracle
tempmin_sup.crop <- crop(tempmin_sup, south.atlantic.ext)
plot(tempmin_sup.crop, col = my.colors1, axes = FALSE, box = FALSE)
title(cex.sub = 1.25, sub = "MAPA DE DISTRIBUIÇÃO - TEMPERATURA MIN DE SUP")
map('worldHires', fill = FALSE, add = TRUE)
points(df_pres$LONG, df_pres$LAT, col="black", pch=20, cex=0.35)

## SALINIDADE DE FUNDO
sal_media <- load_layers("BO2_salinitymean_bdmean")   #nome como consta na base Oracle
south.atlantic.ext <- extent(-60, -35, -40, -18)
sal_media.crop <- crop(sal_media, south.atlantic.ext)
plot(sal_media.crop, col = my.colors3
     , axes = FALSE, box = FALSE)
title(cex.sub = 1.25, sub = "MAPA DE DISTRIBUIÇÃO DA VARIÁVEL SALINIDADE DE FUNDO")
map('worldHires', fill = FALSE, add = TRUE)
points(df_pres$LONG, df_pres$LAT, col="black", pch=20, cex=0.35)

## CLOROFILA 
chl_min <- load_layers("BO2_chlomin_ss")   #nome como consta na base Oracle
chl_min.crop <- crop(chl_min, south.atlantic.ext)
plot(chl_min.crop, col = my.colors1, axes = FALSE, box = FALSE)
title(cex.sub = 1.25, sub = "MAPA DE DISTRIBUIÇÃO DA VARIÁVEL CLOROFILA (minima)")
map('worldHires', fill = FALSE, add = TRUE)
points(df_pres$LONG, df_pres$LAT, col="black", pch=20, cex=0.35)

## NITRATO (MEDIA)
nitrate_min <- load_layers("BO2_nitratemean_bdmean")   #nome como consta na base Oracle
nitrate_min.crop <- crop(nitrate_min, south.atlantic.ext)
plot(nitrate_min.crop, col = my.colors1, axes = FALSE, box = FALSE)
title(cex.sub = 1.25, sub = "MAPA DE DISTRIBUIÇÃO DA VARIÁVEL NITRATO (media)")
map('worldHires', fill = FALSE, add = TRUE)
points(df_pres$LONG, df_pres$LAT, col="black", pch=20, cex=0.35)

# PLOTAR AS AMOSTRAS SOBRE OS MAPAS (colunas sempre na ordem LONG e depois LAT)
points(df$LONG, df$LAT, col="black", pch=20, cex=0.35)            # todos os pontos
points(df_pres$LONG, df_pres$LAT, col="red", pch=20, cex=0.35)  # apenas presenca


