#' ---
#' titulo: live grass gis
#' autor: mauricio vancine
#' data: 22-09-2021
#' ---

# preparar r -------------------------------------------------------------
# pacotes
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(link2GI)) install.packages("link2GI")
if(!require(rgrass7)) install.packages("rgrass7")
if(!require(raster)) install.packages("raster")
if(!require(tmap)) install.packages("tmap")
if(!require(mapview)) install.packages("mapview")
if(!require(mapedit)) install.packages("mapedit")
if(!require(sp)) install.packages("sp")
if(!require(rstatix)) install.packages("rstatix")
if(!require(effectsize)) install.packages("effectsize")

# usar os objetos na classe sp
use_sp()

# mais informacoes para configurar o grass gis e r
# https://grasswiki.osgeo.org/wiki/R_statistics/rgrass7
# https://geocompr.robinlovelace.net/gis.html#rgrass

# conectar ao grass ------------------------------------------------------
# encontrar grass gis no seu computador
dir_grass <- link2GI::findGRASS() 
dir_grass

# iniciar grass
rgrass7::initGRASS(gisBase = dir_grass$instDir,
                   gisDbase = "/home/mude/data/onedrive/live_grass_gis/grassdb", 
                   location = "landsat5", 
                   mapset = "bsb",
                   override = TRUE)

# comandos em r para a location do grass ----------------------------------
# region
rgrass7::execGRASS("g.region", flags = c("a", "p"))

# listar rasters
rgrass7::execGRASS("g.list", type = "raster")
rgrass7::execGRASS("g.list", type = "raster", pattern = "*ndvi")

# informacoes de um raster
rgrass7::execGRASS("r.info", map = "l5c1_prefire_SR_6S_ndvi")

# importar um raster do grass para o r
ndvi <- rgrass7::readRAST("l5c2_prefire_SR_ndvi") %>% 
  raster::raster()
ndvi

# plot
plot(ndvi)

# tmap
tm_shape(ndvi) +
  tm_raster(style = "pretty")

# listar e importar bandas ------------------------------------------------
# listas as bandas com as diferentes correcoes
l5c1 <- rgrass7::execGRASS("g.list", type = "raster", pattern = "l5c1_prefire_reflect_TOA*")
l5c1
attributes(l5c1)$resOut

l5c1_6s <- rgrass7::execGRASS("g.list", type = "raster", pattern = "l5c1_prefire_SR_6S_B*")
l5c1_6s
attributes(l5c1_6s)$resOut

l5c1_dos1 <- rgrass7::execGRASS("g.list", type = "raster", pattern = "l5c1_prefire_SR_dos1.*")
l5c1_dos1
attributes(l5c1_dos1)$resOut

l5c2 <- rgrass7::execGRASS("g.list", type = "raster", pattern = "l5c2_prefire_SR_B*")
l5c2
attributes(l5c2)$resOut

# importar as bandas
l5c1_brick <- attributes(l5c1)$resOut %>%
  rgrass7::readRAST() %>% 
  raster::stack() %>% 
  raster::brick()
l5c1_brick
names(l5c1_brick) <- sub("l5c1_prefire_reflect_TOA_B.", "l5c1_b", names(l5c1_brick))
names(l5c1_brick)

l5c1_dos1_brick <- attributes(l5c1_dos1)$resOut %>%
  rgrass7::readRAST() %>% 
  raster::stack() %>% 
  raster::brick()
l5c1_dos1_brick
names(l5c1_dos1_brick) <- sub("l5c1_prefire_SR_dos1.", "l5c1dos1_b", names(l5c1_dos1_brick))
names(l5c1_dos1_brick)

l5c1_6s_brick <- attributes(l5c1_6s)$resOut %>%
  rgrass7::readRAST() %>% 
  raster::stack() %>% 
  raster::brick()
l5c1_6s_brick
names(l5c1_6s_brick) <- sub("l5c1_prefire_SR_6S_B.", "l5c16s_b", names(l5c1_6s_brick))
names(l5c1_6s_brick)

l5c2_brick <- attributes(l5c2)$resOut %>%
  rgrass7::readRAST() %>% 
  raster::stack() %>% 
  raster::brick()
l5c2_brick
names(l5c2_brick) <- sub("l5c2_prefire_SR_B.", "l5c2_b", names(l5c2_brick))
names(l5c2_brick)

# tmap
comp_natural <- raster::stretch(l5c2_brick, minq = .02, maxq = .98) %>% 
  tm_shape() +
  tm_rgb(r = 3, g = 2, b = 1) +
  tm_grid(lines = FALSE, labels.rot = c(0, 90), labels.size = 1)
comp_natural

comp_falsa <- raster::stretch(l5c2_brick, minq = .02, maxq = .98) %>% 
  tm_shape() +
  tm_rgb(r = 4, g = 3, b = 2) +
  tm_grid(lines = FALSE, labels.rot = c(0, 90), labels.size = 1)
comp_falsa

tmap::tmap_mode(mode = "view")
comp_natural
comp_falsa

tmap::tmap_mode(mode = "plot")
comp_natural
comp_falsa

# outra forma de criar mapas dinamicos
mapview::viewRGB(l5c2_brick, r = 3, g = 2, b = 1)
mapview::viewRGB(l5c2_brick, r = 4, g = 3, b = 2)

# extrair reflectancia para alguns pontos ------------------------------------
# criar um vetor de pontos
pontos <-  mapedit::drawFeatures() # atencao para o Done!
pontos

# adicionar as classes
pontos <- pontos %>% 
  dplyr::mutate(classes = c("Mata de galeria", "Cerrado florestal", "Cerrado aberto"))
pontos

# ver esses pontos
comp_natural_pontos <- raster::stretch(l5c2_brick, minq = .02, maxq = .98) %>% 
  tm_shape() +
  tm_rgb(r = 3, g = 2, b = 1) +
  tm_shape(pontos) +
  tm_dots(size = .1, col = "classes", pal = c("forestgreen", "orange", "yellow")) +
  tm_grid(lines = FALSE, labels.rot = c(0, 90), labels.size = 1)
comp_natural_pontos

tmap::tmap_mode(mode = "view")
comp_natural_pontos

# extrair os valores das bandas para os pontos
pontos_bandas <- pontos %>% 
  sf::st_transform(crs = 32723) %>% 
  dplyr::mutate(raster::extract(x = l5c1_brick, y = ., df = TRUE)) %>% 
  dplyr::mutate(raster::extract(x = l5c1_dos1_brick, y = ., df = TRUE)) %>% 
  dplyr::mutate(raster::extract(x = l5c1_6s_brick, y = ., df = TRUE)) %>% 
  dplyr::mutate(raster::extract(x = l5c2_brick, y = ., df = TRUE))
pontos_bandas

# acessar apenas a tabela de atributos
pontos_bandas_dados <- sf::st_drop_geometry(pontos_bandas) %>% 
  dplyr::select(classes, l5c1_b1:l5c2_b7)
pontos_bandas_dados

# wide para long
pontos_bandas_dados_long <- tidyr::pivot_longer(pontos_bandas_dados, 
                                                cols = l5c1_b1:l5c2_b7, 
                                                names_to = "correcoes_bandas", 
                                                values_to = "reflectancia") %>% 
  tidyr::separate(col = correcoes_bandas, c("correcoes", "bandas"), sep = "_")
pontos_bandas_dados_long

# comprimento das bandas
comp_onda <- tibble::tibble(bandas = paste0("b", c(1:5, 7)),
                            comp_onda = c(.48, .56, .66, .83, 1.65, 2.21))
comp_onda

# join
pontos_bandas_dados_long_onda <- pontos_bandas_dados_long %>% 
  dplyr::left_join(comp_onda) %>% 
  dplyr::filter(bandas != "b6") %>% 
  dplyr::mutate(comp_onda = as.factor(comp_onda))
pontos_bandas_dados_long_onda

# plot mata galeria
pontos_bandas_dados_long_onda %>% 
  dplyr::filter(classes == "Mata de galeria") %>% 
  ggplot(aes(x = comp_onda, y = reflectancia, color = correcoes, group = correcoes)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_color_viridis_d() +
  labs(title = "Mata de galeria", x = "Comprimento de onda (μm)", 
       y = "Reflectância", color = "Correções") +
  theme_bw() +
  theme(title = element_text(size = 20),
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15))

# plot cerrado florestal
pontos_bandas_dados_long_onda %>% 
  dplyr::filter(classes == "Cerrado florestal") %>% 
  ggplot(aes(x = comp_onda, y = reflectancia, color = correcoes, group = correcoes)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_color_viridis_d() +
  labs(title = "Cerrado florestal", x = "Comprimento de onda (μm)", 
       y = "Reflectância", color = "Correções") +
  theme_bw() +
  theme(title = element_text(size = 20),
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15))

# plot cerrado aberto
pontos_bandas_dados_long_onda %>% 
  dplyr::filter(classes == "Cerrado aberto") %>% 
  ggplot(aes(x = comp_onda, y = reflectancia, color = correcoes, group = correcoes)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_color_viridis_d() +
  labs(title = "Cerrado aberto", x = "Comprimento de onda (μm)", 
       y = "Reflectância", color = "Correções") +
  theme_bw() +
  theme(title = element_text(size = 20),
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15))

# comparar bandas ---------------------------------------------------------
# selecionar as bandas
b1 <- raster::brick(l5c1_brick$l5c1_b1, l5c1_6s_brick$l5c16s_b1, 
                    l5c1_dos1_brick$l5c1dos1_b1, l5c2_brick$l5c2_b1)

b1

b2 <- raster::brick(l5c1_brick$l5c1_b2, l5c1_6s_brick$l5c16s_b2, 
                    l5c1_dos1_brick$l5c1dos1_b2, l5c2_brick$l5c2_b2)

b2

b3 <- raster::brick(l5c1_brick$l5c1_b3, l5c1_6s_brick$l5c16s_b3, 
                    l5c1_dos1_brick$l5c1dos1_b3, l5c2_brick$l5c2_b3)

b3

b4 <- raster::brick(l5c1_brick$l5c1_b4, l5c1_6s_brick$l5c16s_b4, 
                    l5c1_dos1_brick$l5c1dos1_b4, l5c2_brick$l5c2_b4)

b4

# tmap
tm_shape(b1) +
  tm_raster(title = "Banda 1", style = "fisher", pal = "Blues", legend.reverse = TRUE) +
  tm_facets()

tm_shape(b2) +
  tm_raster(title = "Banda 2", style = "fisher", pal = "Greens", legend.reverse = TRUE) +
  tm_facets()

tm_shape(b3) +
  tm_raster(title = "Banda 3", style = "fisher", pal = "Reds", legend.reverse = TRUE) +
  tm_facets()

tm_shape(b4) +
  tm_raster(title = "Banda 4", style = "fisher", pal = "Oranges", legend.reverse = TRUE) +
  tm_facets()

# comparacao
parallel::detectCores()

raster::beginCluster(n = 4)
raster::getCluster()
b1_sd <- raster::clusterR(x = b1, calc, args = list(fun = sd))
b1_sd

b2_sd <- raster::clusterR(x = b2, calc, args = list(fun = sd))
b2_sd

b3_sd <- raster::clusterR(x = b3, calc, args = list(fun = sd))
b3_sd

b4_sd <- raster::clusterR(x = b4, calc, args = list(fun = sd))
b4_sd
raster::endCluster()

# tmap
tm_shape(b1_sd) +
  tm_raster(title = "Banda 1 sd", style = "quantile", n = 10, legend.reverse = TRUE)

tm_shape(b2_sd) +
  tm_raster(title = "Banda 2 sd", style = "quantile", n = 10, legend.reverse = TRUE)

tm_shape(b3_sd) +
  tm_raster(title = "Banda 3 sd", style = "quantile", n = 10, legend.reverse = TRUE)

tm_shape(b4_sd) +
  tm_raster(title = "Banda 4 sd", style = "quantile", n = 10, legend.reverse = TRUE)


# histogramas ----
# banda 1
b1_da <- raster::rasterToPoints(b1) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-c(x, y))  %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "correcoes", values_to = "valores") %>% 
  dplyr::mutate(bandas = "banda01", .after = correcoes)
b1_da

ggplot(data = b1_da, aes(x = valores, color = correcoes, fill = correcoes)) +
  geom_density(alpha = .5) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  labs(x = "Reflectância", y = "Densidade", color = "Correções", fill = "Correções") +
  theme_bw() +
  theme(legend.position = c(.8, .8),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15), 
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# banda 2
b2_da <- raster::rasterToPoints(b2) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-c(x, y))  %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "correcoes", values_to = "valores") %>% 
  dplyr::mutate(bandas = "banda02", .after = correcoes)
b2_da

ggplot(data = b2_da, aes(x = valores, color = correcoes, fill = correcoes)) +
  geom_density(alpha = .5) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  labs(x = "Reflectância", y = "Densidade", color = "Correções", fill = "Correções") +
  theme_bw() +
  theme(legend.position = c(.8, .8),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15), 
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# banda 3
b3_da <- raster::rasterToPoints(b3) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-c(x, y))  %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "correcoes", values_to = "valores") %>% 
  dplyr::mutate(bandas = "banda03", .after = correcoes)
b3_da

ggplot(data = b3_da, aes(x = valores, color = correcoes, fill = correcoes)) +
  geom_density(alpha = .5) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  labs(x = "Reflectância", y = "Densidade", color = "Correções", fill = "Correções") +
  theme_bw() +
  theme(legend.position = c(.8, .8),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15), 
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# banda 4
b4_da <- raster::rasterToPoints(b4) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-c(x, y))  %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "correcoes", values_to = "valores") %>% 
  dplyr::mutate(bandas = "banda04", .after = correcoes)
b4_da

ggplot(data = b4_da, aes(x = valores, color = correcoes, fill = correcoes)) +
  geom_density(alpha = .5) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  labs(x = "Reflectância", y = "Densidade", color = "Correções", fill = "Correções") +
  theme_bw() +
  theme(legend.position = c(.8, .8),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15), 
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# boxplot ----
bandas_da <- dplyr::bind_rows(b1_da, b2_da, b3_da, b4_da)
bandas_da

set.seed(42)
bandas_da %>% 
  dplyr::slice_sample(n = 1e5) %>% 
  dplyr::mutate(correcoes = sub("_b1|_b2|_b3|_b4", "", correcoes)) %>% 
  ggplot(aes(x = bandas, y = valores, fill = correcoes)) +
  geom_boxplot(position = position_dodge(0.9)) +
  scale_fill_viridis_d() +
  labs(x = "Bandas", y = "Reflectância", fill = "Correções") +
  theme_bw() +
  theme(legend.position = c(.1, .8), 
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15), 
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# teste estatistico ----
## Fernanda Peres
# https://www.youtube.com/c/Fernandaperes

# normalidade | p < 0.05 rejeitar normalidade
set.seed(42)
b1_da %>% 
  dplyr::slice_sample(n = 5000) %>% 
  dplyr::group_by(correcoes) %>% 
  rstatix::shapiro_test(valores)

set.seed(42)
b2_da %>% 
  dplyr::slice_sample(n = 5000) %>% 
  dplyr::group_by(correcoes) %>% 
  rstatix::shapiro_test(valores)

set.seed(42)
b3_da %>% 
  dplyr::slice_sample(n = 5000) %>% 
  dplyr::group_by(correcoes) %>% 
  rstatix::shapiro_test(valores)

set.seed(42)
b4_da %>% 
  dplyr::slice_sample(n = 5000) %>% 
  dplyr::group_by(correcoes) %>% 
  rstatix::shapiro_test(valores)


# homogeneidade variancia | p < 0.05 rejeitar homogeneiradade variancia
set.seed(42)
b1_da %>% 
  dplyr::slice_sample(n = 5000) %>% 
  rstatix::levene_test(valores ~ correcoes)

set.seed(42)
b2_da %>% 
  dplyr::slice_sample(n = 5000) %>% 
  rstatix::levene_test(valores ~ correcoes)

set.seed(42)
b3_da %>% 
  dplyr::slice_sample(n = 5000) %>% 
  rstatix::levene_test(valores ~ correcoes)

set.seed(42)
b4_da %>% 
  dplyr::slice_sample(n = 5000) %>% 
  rstatix::levene_test(valores ~ correcoes)


# teste t não parametrico - kruskal-wallis test a prioru e a posteriori
set.seed(42)
b1_kruskal_test <- b1_da %>% 
  dplyr::slice_sample(n = 5000) %>% 
  rstatix::kruskal_test(valores ~ correcoes, data = .)
b1_kruskal_test

set.seed(42)
b1_dunn_test <- b1_da %>% 
  dplyr::slice_sample(n = 5000) %>% 
  rstatix::dunn_test(valores ~ correcoes, data = ., p.adjust.method = "bonferroni")
b1_dunn_test

set.seed(42)
b2_kruskal_test <- b2_da %>% 
  dplyr::slice_sample(n = 5000) %>% 
  rstatix::kruskal_test(valores ~ correcoes, data = .)
b2_kruskal_test

set.seed(42)
b2_dunn_test <- b2_da %>% 
  dplyr::slice_sample(n = 5000) %>% 
  rstatix::dunn_test(valores ~ correcoes, data = ., p.adjust.method = "bonferroni")
b2_dunn_test

set.seed(42)
b3_kruskal_test <- b3_da %>% 
  dplyr::slice_sample(n = 5000) %>% 
  rstatix::kruskal_test(valores ~ correcoes, data = .)
b3_kruskal_test

set.seed(42)
b3_dunn_test <- b3_da %>% 
  dplyr::slice_sample(n = 5000) %>% 
  rstatix::dunn_test(valores ~ correcoes, data = ., p.adjust.method = "bonferroni")
b3_dunn_test

set.seed(42)
b4_kruskal_test <- b4_da %>% 
  dplyr::slice_sample(n = 5000) %>% 
  rstatix::kruskal_test(valores ~ correcoes, data = .)
b4_kruskal_test

set.seed(42)
b4_dunn_test <- b4_da %>% 
  dplyr::slice_sample(n = 5000) %>% 
  rstatix::dunn_test(valores ~ correcoes, data = ., p.adjust.method = "bonferroni")
b4_dunn_test




# rascunho ----------------------------------------------------------------

# comparar os ndvis ---
# listar rasters
ndvis <- rgrass7::execGRASS("g.list", type = "raster", pattern = "*ndvi*")
attributes(ndvis)$resOut

attributes(ndvis)$resOut

# importar rasters
ndvi <- rgrass7::readRAST(attributes(ndvis)$resOut) %>% 
  raster::stack() %>% 
  raster::brick()
ndvi

# plot
plot(ndvi)

# tmap
tm_shape(ndvi) +
  tm_raster(title = "NDVI", style = "cont", legend.is.portrait = TRUE, legend.reverse = TRUE) +
  tm_facets() +
  tm_layout(legend.outside.size = .1)

# comparacao
parallel::detectCores()

beginCluster(n = 4)
ndvi6s_ndvidos1 <- clusterR(x = stack(ndvi$l5c1_prefire_SR_6S_ndvi, ndvi$l5c1_prefire_SR_dos1_ndvi), 
                            calc, 
                            args = list(fun = sd))
ndvi6s_ndvidos1

ndvipost_ndvipre <- clusterR(x = stack(ndvi$l5c2_postfire_SR_ndvi, ndvi$l5c2_prefire_SR_ndvi), 
                             overlay, 
                             args = list(fun = function(r1, r2){return(r1-r2)}))
ndvipost_ndvipre
endCluster()

# plot
plot(ndvi6s_ndvidos1)
plot(ndvipost_ndvipre)

# tmap
tm_shape(ndvi6s_ndvidos1) +
  tm_raster(title = "NDVI - 6S_DOS1 - sd", style = "fisher", n = 10,
            legend.is.portrait = TRUE, legend.reverse = TRUE) +
  tm_layout(legend.position = c("right", "bottom")) 

tm_shape(ndvipost_ndvipre) +
  tm_raster(title = "NDVI - Pré e pós incêncdio", style = "fisher", pal = "RdBu",
            legend.is.portrait = TRUE, legend.reverse = TRUE) +
  tm_layout(legend.position = c("right", "bottom")) 

