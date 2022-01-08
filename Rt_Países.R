#Libraries
library(testthat)
library(rlang)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(plotly)
library(data.table)
library(ggpubr)
library(devtools)
install_github("holtzy/epuRate")
library(epuRate)
library(EpiEstim)
library(tidyr)
library(lubridate)
library(googlesheets)
require(RCurl)
library(viridis)
library(flexdashboard)
library(here)
library(rjson)
library(jsonlite)
library(RCurl)
library(highcharter)
library(here)
library(purrr)
library(magrittr)
library(RColorBrewer)
library(rjson)
library(readr)
library(readxl)
library(scales)
library(tibble)


setwd("C:/Users/teres/Desktop/EPIVET/COVID19/Rt_COVID19")
setwd("C:/Users/ines/Documents/Estágio Epidemiologia/COVID19/Rt_COVID19")


#Data OUTROS PAÍSES e transformação para formato de data

# ITÁLIA (https://github.com/pcm-dpc/COVID-19/blob/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv)
italy <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/legacy/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv", stringsAsFactors = FALSE)

## Remover horas e minutos
italy$data <- strftime(italy$data, format = "%Y-%m-%d")

## Alterar para formato de data
italy$data <- as.Date(italy$data, "%Y-%m-%d")

## Tabela: Data e confirmados novos
it_var <- italy %>%
  select(data, nuovi_positivi)
names(it_var) <- c("data", "confirmados_novos")


## Previsão da evolução
covid_it_var <- it_var  %>%
  filter(it_var$data > as.Date("2020-02-24")) %>%  
  dplyr::mutate(t_start = dplyr::row_number())

## Cálculo do Rt Itália- Uncertainty method --> "uncertain_si"
### Serial Interval (c/ base nos valores anteriores)

sens_configs <- 
  make_config(
    list(
      mean_si = 4.7, std_mean_si = 0.7,
      min_mean_si = 3.7, max_mean_si = 6.0,
      std_si = 2.9, std_std_si = 0.5,
      min_std_si = 1.9, max_std_si = 4.9,
      n1 = 1000,
      n2 = 100,
      seed = 123456789
    )
  )

## Aplicar a função Estimate_R
Rt_nonparam_si_it <- estimate_R(as.numeric(covid_it_var$confirmados_novos), 
                                method = "uncertain_si",
                                config = sens_configs
)

sample_windows_it <- seq(length(Rt_nonparam_si_it$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_it <- 
  map(.x = sample_windows_it,
      .f = function(x) {
        
        posterior_sample_obj_it <- 
          sample_posterior_R(
            R = Rt_nonparam_si_it,
            n = 1000, 
            window = x )
        
        posterior_sample_estim_it <- 
          data.frame(
            window_index = x,
            window_t_start = Rt_nonparam_si_it$R$t_start[x],
            window_t_end = Rt_nonparam_si_it$R$t_end[x],
            date_point = covid_it_var[covid_it_var$t_start == Rt_nonparam_si_it$R$t_end[x], "data"],
            R_e_median = median(posterior_sample_obj_it),
            R_e_q0025 = quantile(posterior_sample_obj_it, probs = 0.025),
            R_e_q0975 = quantile(posterior_sample_obj_it, probs = 0.975))
        
        return(posterior_sample_estim_it)}
  ) %>% 
  
  reduce(bind_rows)


## Gráfico Itália ggplot
## Linhas a adicionar no gráfico
d_it = data.frame(date=as.Date(c("2020-03-09", "2020-06-04", "2020-10-08", "2020-10-15")), Evento=c("Confinamento obrigatório", "Levantamento das restrições às movimentações", "Obrigatoriedade de máscara", "Encerramento de escolas e universidades"))

graph_it<- ggplot(posterior_Rt_it, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "darkslateblue",  alpha = 0.5, size = 1, aes(group = 1, text = paste('Data: ', date_point, '<br>Rt médio: ', R_e_median))) +
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "darkslateblue") +
  
  labs( title = "Itália", size = 10,
        subtitle = "Evolução do Número Efetivo Reprodutivo ao longo do tempo",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)",
        caption = "Fonte: Departamento da Proteção Civil de Itália"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_it_var$data), max((posterior_Rt_it$date_point)))
  ) +
  
  scale_y_continuous(
    breaks = 0:10,
    limits = c(0, 10)
  ) +
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) + 
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-09", "2020-06-04", "2020-10-08", "2020-10-15"))), linetype= c("solid", "dotted", "dotdash", "twodash"), colour = "darkred", alpha = 0.5) +
  geom_vline(data=d_it, mapping =  aes(xintercept = date, linetype = Evento), size = 1, colour = 'darkred', alpha = 0.5, show.legend = FALSE) + 
  geom_pointrange(data = last(posterior_Rt_it), mapping = aes(x = date_point, y = R_e_median, ymin = R_e_q0025, ymax = R_e_q0975), stat = "identity", position = "identity", colour = "indianred4", size = 1,5, alpha = 0.8, linetype = "solid") + 
  annotate(geom = "text", x = last(posterior_Rt_it$date_point), y = last(posterior_Rt_it$R_e_median) - 0.5, label = round(last(posterior_Rt_it$R_e_median), digits = 3), size = 3)

### Tornar gráfico interativo
ggplotly(graph_it, tooltip = "text", width = 900, height = 450) %>%
  layout(title = list(text = paste0("Itália", "<br>", "<sup>", "Evolução do Número Efetivo Reprodutivo ao longo do tempo", "</sup>"),font=list(face="bold")), legend = list(x = 100, y = 0.5))





# ALEMANHA (https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0/data)
germany <- fromJSON("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")
germany <- germany$features

## Remover horas e minutos
germany$properties$Meldedatum <- strftime(germany$properties$Meldedatum, format = "%Y-%m-%d")
## Alterar para formato de data
germany$properties$Meldedatum <- as.Date(germany$properties$Meldedatum, format = "%Y-%m-%d")

### Ordenar por data
germany <- as.data.frame(germany[order(germany$properties$Meldedatum), ])

### Nº de registos por dia (agregar os confirmados )
ger_var <- as.data.frame(aggregate(x = germany, list(Data = germany$properties$Meldedatum), FUN = length))
ger_var <- ger_var[,1:2]
names(ger_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_ger_var <- ger_var  %>%
  filter(ger_var$data > as.Date("2020-02-28")) %>% 
  dplyr::mutate(t_start = dplyr::row_number())

## Aplicar a função Estimate_R
Rt_nonparam_si_ger <- estimate_R(as.numeric(covid_ger_var$confirmados_novos), 
                                 method = "uncertain_si",
                                 config = sens_configs
)

sample_windows_ger <- seq(length(Rt_nonparam_si_ger$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_ger <- 
  map(.x = sample_windows_ger,
      .f = function(x) {
        
        posterior_sample_obj_ger <- 
          sample_posterior_R(
            R = Rt_nonparam_si_ger,
            n = 1000, 
            window = x )
        
        posterior_sample_estim_ger <- 
          data.frame(
            window_index = x,
            window_t_start = Rt_nonparam_si_ger$R$t_start[x],
            window_t_end = Rt_nonparam_si_ger$R$t_end[x],
            date_point = covid_ger_var[covid_ger_var$t_start == Rt_nonparam_si_ger$R$t_end[x], "data"],
            R_e_median = median(posterior_sample_obj_ger),
            R_e_q0025 = quantile(posterior_sample_obj_ger, probs = 0.025),
            R_e_q0975 = quantile(posterior_sample_obj_ger, probs = 0.975))
        
        return(posterior_sample_estim_ger)}
  ) %>% 
  
  reduce(bind_rows)


## Gráfico Alemanha ggplot
## Linhas a adicionar no gráfico
d_ger = data.frame(date=as.Date(c("2020-03-13", "2020-03-22", "2020-04-20", "2020-05-04", "2020-11-02")), Evento=c("Encerramento de escolas", "Estado de Emergência", "Reabertura de lojas", "Reabertura de escolas", "Confinamento parcial"))

graph_ger<- ggplot(posterior_Rt_ger, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "goldenrod",  alpha = 0.5, size = 1, aes(group = 1, text = paste('Data: ', date_point,
                                                                                      '<br>Rt médio: ', R_e_median))) +  
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "goldenrod1") +
  
  labs( title = "Alemanha", size= 10,
        subtitle = " Evolução do Número Efetivo Reprodutivo ao longo do tempo",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)",
        caption = "Fonte: NPGEO-DE Corona"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_ger_var$data), max((posterior_Rt_ger$date_point)))
  ) +
  
  scale_y_continuous(
    breaks = 0:10,
    limits = c(0, 10)
  ) +
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) +
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-13", "2020-03-22", "2020-04-20", "2020-05-04", "2020-11-02"))), linetype= c("twodash", "dotted", "dashed", "dotdash", "solid"), colour = "darkred" , alpha = 0.5) +
  geom_vline(data=d_ger, mapping =  aes(xintercept = date, linetype = Evento, ), size = 1, colour = "darkred", alpha = 0.5, show.legend = FALSE) + 
  geom_pointrange(data = last(posterior_Rt_ger), mapping = aes(x = date_point, y = R_e_median, ymin = R_e_q0025, ymax = R_e_q0975), stat = "identity", position = "identity", colour = "indianred4", size = 1,5, alpha = 0.8, linetype = "solid") + 
  annotate(geom = "text", x = last(posterior_Rt_ger$date_point), y = last(posterior_Rt_ger$R_e_median) - 0.5, label = round(last(posterior_Rt_ger$R_e_median), digits = 3), size = 3)

### Tornar gráfico interativo
ggplotly(graph_ger, tooltip = "text", width = 900, height = 450)%>%
  layout(title = list(text = paste0("Alemanha", "<br>", "<sup>", "Evolução do Número Efetivo Reprodutivo ao longo do tempo", "</sup>"),font=list(face="bold")), legend = list(x = 100, y = 0.5))








# ESPANHA (https://cnecovid.isciii.es/covid19/#documentaci%C3%B3n-y-datos)
spain <- read.csv("https://cnecovid.isciii.es/covid19/resources/datos_ccaas.csv")

## Alterar para formato Data
spain$fecha <- as.Date(spain$fecha, "%Y-%m-%d")

## Tabela confirmados novos (somar registos por dia (juntar regiões))
spa_var <- as.data.frame(aggregate(spain$num_casos, by = list(spain$fecha), FUN = sum))
names(spa_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_spa_var <- spa_var  %>%
  filter(spa_var$data > as.Date("2020-01-31")) %>% 
  dplyr::mutate(t_start = dplyr::row_number())

## Aplicar a função Estimate_R
Rt_nonparam_si_spa <- estimate_R(as.numeric(covid_spa_var$confirmados_novos), 
                                 method = "uncertain_si",
                                 config = sens_configs
)

sample_windows_spa <- seq(length(Rt_nonparam_si_spa$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_spa <- 
  map(.x = sample_windows_spa,
      .f = function(x) {
        
        posterior_sample_obj_spa <- 
          sample_posterior_R(
            R = Rt_nonparam_si_spa,
            n = 1000, 
            window = x )
        
        posterior_sample_estim_spa <- 
          data.frame(
            window_index = x,
            window_t_start = Rt_nonparam_si_spa$R$t_start[x],
            window_t_end = Rt_nonparam_si_spa$R$t_end[x],
            date_point = covid_spa_var[covid_spa_var$t_start == Rt_nonparam_si_spa$R$t_end[x], "data"],
            R_e_median = median(posterior_sample_obj_spa),
            R_e_q0025 = quantile(posterior_sample_obj_spa, probs = 0.025),
            R_e_q0975 = quantile(posterior_sample_obj_spa, probs = 0.975))
        
        return(posterior_sample_estim_spa)}
  ) %>% 
  
  reduce(bind_rows)


## Gráfico Espanha ggplot
## Linhas a adicionar no gráfico
d_spa = data.frame(date=as.Date(c("2020-03-15", "2020-05-11", "2020-10-07", "2020-10-25")), Evento=c("Estado de Emergência", "Início do desconfinamento", "Estado de Emergência - Região Autónoma de Madrid", "Estado de Emergência Nacional"))

graph_spa<- ggplot(posterior_Rt_spa, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "thistle3",  alpha = 0.8, size = 1, aes(group = 1, text = paste('Data: ', date_point,
                                                                                     '<br>Rt médio: ', R_e_median))) +  
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.3, fill = "thistle2") +
  
  labs( title = "Espanha", size= 10,
        subtitle = "Evolução do Número Efetivo Reprodutivo ao longo do tempo",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)", 
        caption = "Fonte: Centro Nacional de Epidemiología"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_spa_var$data), max((posterior_Rt_spa$date_point)))
  ) +
  
  scale_y_continuous(
    breaks = 0:10,
    limits = c(0, 10)
  ) +
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) +
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-15", "2020-05-11", "2020-10-07", "2020-10-25"))), linetype = c("solid", "dotdash", "twodash", "dotted"), colour = "darkred" , alpha = 0.5) +
  geom_vline(data=d_spa, mapping =  aes(xintercept = date, linetype = Evento, ), size = 1, colour = "darkred", alpha = 0.5, show.legend = FALSE)+ 
  geom_pointrange(data = last(posterior_Rt_spa), mapping = aes(x = date_point, y = R_e_median, ymin = R_e_q0025, ymax = R_e_q0975), stat = "identity", position = "identity", colour = "indianred4", size = 1,5, alpha = 0.8, linetype = "solid")+ 
  annotate(geom = "text", x = last(posterior_Rt_spa$date_point), y = last(posterior_Rt_spa$R_e_median) - 0.5, label = round(last(posterior_Rt_spa$R_e_median), digits = 3), size = 3)

### Tornar gráfico interativo
ggplotly(graph_spa, tooltip = "text", width = 900, height = 450) %>%
  layout(title = list(text = paste0("Espanha", "<br>", "<sup>", "Evolução do Número Efetivo Reprodutivo ao longo do tempo", "</sup>"),font=list(face="bold")), legend = list(x = 100, y = 0.5))






# BÉLGICA (https://epistat.wiv-isp.be/covid/)
belgium <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv")

## Alterar para formato de data
belgium$DATE <- as.Date(belgium$DATE, "%Y-%m-%d")

## Tabela de confirmados novos - Soma dos registos diários (juntar regiões)
bel_var <- as.data.frame(aggregate(belgium$CASES, by = list(Data = belgium$DATE), FUN = sum))
names(bel_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_bel_var <- bel_var  %>%
  # Neste caso não se filtrou > 28-02-2020 , uma vez que só reportaram a partir de Março.
  dplyr::mutate(t_start = dplyr::row_number())

## Aplicar a função Estimate_R
Rt_nonparam_si_bel <- estimate_R(as.numeric(covid_bel_var$confirmados_novos), 
                                 method = "uncertain_si",
                                 config = sens_configs
)

sample_windows_bel <- seq(length(Rt_nonparam_si_bel$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_bel <- 
  map(.x = sample_windows_bel,
      .f = function(x) {
        
        posterior_sample_obj_bel <- 
          sample_posterior_R(
            R = Rt_nonparam_si_bel,
            n = 1000, 
            window = x )
        
        posterior_sample_estim_bel <- 
          data.frame(
            window_index = x,
            window_t_start = Rt_nonparam_si_bel$R$t_start[x],
            window_t_end = Rt_nonparam_si_bel$R$t_end[x],
            date_point = covid_bel_var[covid_bel_var$t_start == Rt_nonparam_si_bel$R$t_end[x], "data"],
            R_e_median = median(posterior_sample_obj_bel),
            R_e_q0025 = quantile(posterior_sample_obj_bel, probs = 0.025),
            R_e_q0975 = quantile(posterior_sample_obj_bel, probs = 0.975))
        
        return(posterior_sample_estim_bel)}
  ) %>% 
  
  reduce(bind_rows)


## Gráfico Bélgica ggplot
## Linhas a adicionar no gráfico
d_bel = data.frame(date=as.Date(c("2020-03-13", "2020-03-18", "2020-06-08", "2020-09-24")), Evento=c("Confinamento parcial", "Confinamento total", "Reabertura de cafés e restaurantes", "Novas restrições"))

graph_bel<- ggplot(posterior_Rt_bel, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "royalblue2",  alpha = 0.65, size = 1 ,aes(group = 1, text = paste('Data: ', date_point,
                                                                                        '<br>Rt médio: ', R_e_median))) +
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "royalblue1") +
  
  labs( title = "Bélgica", size= 10,
        subtitle = "Evolução do Número Efetivo Reprodutivo ao longo do tempo",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)", 
        caption = "Fonte: Epistat - Sciensano"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_bel_var$data), max((posterior_Rt_bel$date_point)))
  ) +
  
  scale_y_continuous(
    breaks = 0:10,
    limits = c(0, 10)
  ) +
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) + 
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-13", "2020-03-18", "2020-06-08", "2020-09-24"))), linetype = c("solid", "twodash", "dotdash", "dotted"), colour = "darkred" , alpha = 0.5) +
  geom_vline(data=d_bel, mapping =  aes(xintercept = date, linetype = Evento, ), size = 1, colour = "darkred", alpha = 0.5, show.legend = FALSE)+ 
  geom_pointrange(data = last(posterior_Rt_bel), mapping = aes(x = date_point, y = R_e_median, ymin = R_e_q0025, ymax = R_e_q0975), stat = "identity", position = "identity", colour = "indianred4", size = 1,5, alpha = 0.8, linetype = "solid")+ 
  annotate(geom = "text", x = last(posterior_Rt_bel$date_point), y = last(posterior_Rt_bel$R_e_median) - 0.5, label = round(last(posterior_Rt_bel$R_e_median), digits = 3), size = 3)

### Tornar gráfico interativo
ggplotly(graph_bel, tooltip = "text", width = 900, height = 450) %>%
  layout(title = list(text = paste0("Bélgica", "<br>", "<sup>", "Evolução do Número Efetivo Reprodutivo ao longo do tempo", "</sup>"),font=list(face="bold")), legend = list(x = 100, y = 0.5))







# REPÚBLICA CHECA (https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19)
czechr <- read.csv("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakaza.csv")

## Alterar formato para data
czechr$datum <- as.Date(czechr$datum, "%Y-%m-%d")

## Tabela confirmados novos
cz_var <- czechr %>%
  select(ï..datum, prirustkovy_pocet_nakazenych)
names(cz_var) <- c("data", "confirmados_novos")


## Previsão da evolução
covid_cz_var <- cz_var  %>%
  filter(cz_var$data > as.Date("2020-01-27")) %>% 
  dplyr::mutate(t_start = dplyr::row_number())

## Aplicar a função Estimate_R
Rt_nonparam_si_cz <- estimate_R(as.numeric(covid_cz_var$confirmados_novos), 
                                method = "uncertain_si",
                                config = sens_configs
)

sample_windows_cz <- seq(length(Rt_nonparam_si_cz$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_cz <- 
  map(.x = sample_windows_cz,
      .f = function(x) {
        
        posterior_sample_obj_cz <- 
          sample_posterior_R(
            R = Rt_nonparam_si_cz,
            n = 1000, 
            window = x )
        
        posterior_sample_estim_cz <- 
          data.frame(
            window_index = x,
            window_t_start = Rt_nonparam_si_cz$R$t_start[x],
            window_t_end = Rt_nonparam_si_cz$R$t_end[x],
            date_point = covid_cz_var[covid_cz_var$t_start == Rt_nonparam_si_cz$R$t_end[x], "data"],
            R_e_median = median(posterior_sample_obj_cz),
            R_e_q0025 = quantile(posterior_sample_obj_cz, probs = 0.025),
            R_e_q0975 = quantile(posterior_sample_obj_cz, probs = 0.975))
        
        return(posterior_sample_estim_cz)}
  ) %>% 
  
  reduce(bind_rows)


## Gráfico Républica Checa ggplot
## Linhas a adicionar no gráfico
d_cz = data.frame(date=as.Date(c("2020-03-12", "2020-05-11", "2020-10-05", "2020-10-22")), Evento=c("Estado de Emergência", "Reabertura de lojas", "Estado de Emergência", "Proibição de movimentações"))

graph_cz <- ggplot(posterior_Rt_cz, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "steelblue3",  alpha = 0.65, size = 1, aes(group = 1, text = paste('Data: ', date_point,
                                                                                        '<br>Rt médio: ', R_e_median))) +
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "steelblue1") +
  
  labs( title = "Républica Checa", size= 10,
        subtitle = "Evolução do Número Efetivo Reprodutivo ao longo do tempo",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)", 
        caption = "Fonte: Ministério da Saúde da República Checa"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_cz_var$data), max((posterior_Rt_cz$date_point)))
  ) +
  
  scale_y_continuous(
    breaks = 0:10,
    limits = c(0, 10)
  ) +
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) +
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-12", "2020-05-11", "2020-10-05", "2020-10-22"))), linetype = c("solid", "dotted", "solid", "twodash"), colour = "darkred" , alpha = 0.5) +
  geom_vline(data=d_cz, mapping =  aes(xintercept = date, linetype = Evento, ), size = 1, colour = "darkred", alpha = 0.5, show.legend = FALSE)+ 
  geom_pointrange(data = last(posterior_Rt_cz), mapping = aes(x = date_point, y = R_e_median, ymin = R_e_q0025, ymax = R_e_q0975), stat = "identity", position = "identity", colour = "indianred4", size = 1,5, alpha = 0.8, linetype = "solid") + 
  annotate(geom = "text", x = last(posterior_Rt_cz$date_point), y = last(posterior_Rt_cz$R_e_median) - 0.5, label = round(last(posterior_Rt_cz$R_e_median), digits = 3), size = 3)

### Tornar gráfico interativo
ggplotly(graph_cz, tooltip = "text", width = 900, height = 450) %>%
  layout(title = list(text = paste0("República Checa", "<br>", "<sup>", "Evolução do Número Efetivo Reprodutivo ao longo do tempo", "</sup>"),font=list(face="bold")), legend = list(x = 100, y = 0.5))







# SUÍÇA (https://www.bag.admin.ch/bag/en/home/krankheiten/ausbrueche-epidemien-pandemien/aktuelle-ausbrueche-epidemien/novel-cov/situation-schweiz-und-international.html#-640157857)
switzerland <- rio::import(file ="https://www.bag.admin.ch/dam/bag/en/dokumente/mt/k-und-i/aktuelle-ausbrueche-pandemien/2019-nCoV/covid-19-basisdaten-labortests.xlsx.download.xlsx/Dashboard_3_COVID19_labtests_positivity.xlsx")

## Alterar formato para Data
switzerland$Datum <- as.Date(switzerland$Datum, "%Y-%m-%d")

# Selecionar casos positivos diários e criar tabela com a data
swi_var <- switzerland %>%
  filter(Outcome_tests == "Positive") %>%
  select(Datum, Number_of_tests)
names(swi_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_swi_var <- swi_var  %>%
  filter(swi_var$data > as.Date("2020-02-18")) %>% 
  dplyr::mutate(t_start = dplyr::row_number())

## Aplicar a função Estimate_R
Rt_nonparam_si_swi <- estimate_R(as.numeric(covid_swi_var$confirmados_novos), 
                                 method = "uncertain_si",
                                 config = sens_configs
)

sample_windows_swi <- seq(length(Rt_nonparam_si_swi$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_swi <- 
  map(.x = sample_windows_swi,
      .f = function(x) {
        
        posterior_sample_obj_swi <- 
          sample_posterior_R(
            R = Rt_nonparam_si_swi,
            n = 1000, 
            window = x )
        
        posterior_sample_estim_swi <- 
          data.frame(
            window_index = x,
            window_t_start = Rt_nonparam_si_swi$R$t_start[x],
            window_t_end = Rt_nonparam_si_swi$R$t_end[x],
            date_point = covid_swi_var[covid_swi_var$t_start == Rt_nonparam_si_swi$R$t_end[x], "data"],
            R_e_median = median(posterior_sample_obj_swi),
            R_e_q0025 = quantile(posterior_sample_obj_swi, probs = 0.025),
            R_e_q0975 = quantile(posterior_sample_obj_swi, probs = 0.975))
        
        return(posterior_sample_estim_swi)}
  ) %>% 
  
  reduce(bind_rows)


## Gráfico Suíça ggplot
## Linhas a adicionar no gráfico
d_swi = data.frame(date=as.Date(c("2020-03-13", "2020-03-16", "2020-04-27", "2020-10-19")), Evento=c("Encerramento de escolas, probição de eventos e controlo de fronteiras", "Estado de Emergência", "Levantamento gradual das restrições", "Proibição de ajuntamentos"))

graph_swi<- ggplot(posterior_Rt_swi, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "antiquewhite4",  alpha = 0.65, size = 1, aes(group = 1, text = paste('Data: ', date_point,
                                                                                           '<br>Rt médio: ', R_e_median))) +
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.3, fill = "antiquewhite3") +
  
  labs( title = "Suiça", size= 10,
        subtitle = "Evolução do Número Efetivo Reprodutivo longo do tempo",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)", 
        caption = "Fonte: Departamento Federal de Saúde Pública da Suiça"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_swi_var$data), max((posterior_Rt_swi$date_point)))
  ) +
  
  scale_y_continuous(
    breaks = 0:10,
    limits = c(0, 10)
  ) +
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) + 
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-13", "2020-03-16", "2020-04-27", "2020-10-19"))), linetype = c("solid", "twodash", "dotted", "dotdash"), colour = "darkred" , alpha = 0.5) +
  geom_vline(data=d_swi, mapping =  aes(xintercept = date, linetype = Evento, ), size = 1, colour = "darkred", alpha = 0.5, show.legend = FALSE)+ 
  geom_pointrange(data = last(posterior_Rt_swi), mapping = aes(x = date_point, y = R_e_median, ymin = R_e_q0025, ymax = R_e_q0975), stat = "identity", position = "identity", colour = "indianred4", size = 1,5, alpha = 0.8, linetype = "solid") + 
  annotate(geom = "text", x = last(posterior_Rt_swi$date_point), y = last(posterior_Rt_swi$R_e_median) - 0.5, label = round(last(posterior_Rt_swi$R_e_median), digits = 3), size = 3)

### Tornar gráfico interativo
ggplotly(graph_swi, tooltip = "text", width = 900, height = 450) %>%
  layout(title = list(text = paste0("Suiça", "<br>", "<sup>", "Evolução do Número Efetivo Reprodutivo ao longo do tempo", "</sup>"),font=list(face="bold")), legend = list(x = 100, y = 0.5))







#SUÉCIA (https://experience.arcgis.com/experience/09f821667ce64bf7be6f9f87457ed9aa/page/page_0/)
sweden <- "https://fohm.maps.arcgis.com/sharing/rest/content/items/b5e7488e117749c19881cce45db13f7e/data"
sweden <- rio::import(file = sweden)

## Alterar formato de Data
sweden$Statistikdatum <- as.Date(sweden$Statistikdatum, "%Y-%m-%d")

## Tabela de confirmados novos
swe_var <- sweden %>%
  select(Statistikdatum, Totalt_antal_fall)
names(swe_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_swe_var <- swe_var  %>%
  filter(swe_var$data > as.Date("2020-02-04")) %>% 
  dplyr::mutate(t_start = dplyr::row_number())

## Aplicar a função Estimate_R
Rt_nonparam_si_swe <- estimate_R(as.numeric(covid_swe_var$confirmados_novos),
                                 method = "uncertain_si",
                                 config = sens_configs)

sample_windows_swe <- seq(length(Rt_nonparam_si_swe$R$t_start))

# Criar um data frame com valores de R
posterior_Rt_swe <- 
  map(
    .x = sample_windows_swe,
    .f = function(x) {
      
      posterior_sample_obj_swe <- 
        sample_posterior_R(
          R = Rt_nonparam_si_swe,
          n = 1000, 
          window = x
        )
      
      posterior_sample_estim_swe <- 
        data.frame(
          window_index = x,
          window_t_start = Rt_nonparam_si_swe$R$t_start[x],
          window_t_end = Rt_nonparam_si_swe$R$t_end[x],
          date_point = covid_swe_var[covid_swe_var$t_start == Rt_nonparam_si_swe$R$t_end[x], "data"],
          R_e_median = median(posterior_sample_obj_swe),
          R_e_q0025 = quantile(posterior_sample_obj_swe, probs = 0.025),
          R_e_q0975 = quantile(posterior_sample_obj_swe, probs = 0.975)
        )
      
      return(posterior_sample_estim_swe)
      
    }
  ) %>% 
  reduce(bind_rows)

#Grafico Suécia 
## Linhas a adicionar no gráfico
d_swe = data.frame(date=as.Date(c("2020-03-17", "2020-03-19", "2020-03-27", "2020-11-01")), Evento=c("Recomendação de teletrabalho", "Encerramento das escolas", "Proibição de ajuntamentos com mais de 50 pessoas", "Novas medidas de restrição"))

graph_swe <- ggplot(posterior_Rt_swe, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "yellow4",  alpha = 0.5, size = 1, aes(group = 1, text = paste('Data: ', date_point,
                                                                                    '<br>Rt médio: ', R_e_median))) +
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "yellow3") +
  
  labs( title = "Suécia", size= 10,
        subtitle = "Evolução do Número Efetivo Reprodutivo ao longo do tempo",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)", 
        caption = "Fonte: Agência de Saúde Pública da Suécia"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_swe_var$data), max(posterior_Rt_swe$date_point))
  ) +
  
  scale_y_continuous(
    breaks = 0:10,
    limits = c(0, 10)
  ) +
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) + 
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-17", "2020-03-19", "2020-03-27", "2020-11-01"))), linetype = c("dotdash", "solid", "dotted", "twodash"), colour = "darkred" , alpha = 0.5) +
  geom_vline(data=d_swe, mapping =  aes(xintercept = date, linetype = Evento, ), size = 1, colour = "darkred", alpha = 0.5, show.legend = FALSE)+ 
  geom_pointrange(data = last(posterior_Rt_swe), mapping = aes(x = date_point, y = R_e_median, ymin = R_e_q0025, ymax = R_e_q0975), stat = "identity", position = "identity", colour = "indianred4", size = 1,5, alpha = 0.8, linetype = "solid") + 
  annotate(geom = "text", x = last(posterior_Rt_swe$date_point), y = last(posterior_Rt_swe$R_e_median) - 0.5, label = round(last(posterior_Rt_swe$R_e_median), digits = 3), size = 3)

#Tornar o grafico interativo
ggplotly(graph_swe, tooltip = "text", width = 900, height = 450) %>%
  layout(title = list(text = paste0("Suécia", "<br>", "<sup>", "Evolução do Número Efetivo Reprodutivo ao longo do tempo", "</sup>"),font=list(face="bold")), legend = list(x = 100, y = 0.5))








# REINO UNIDO (https://coronavirus.data.gov.uk/cases)
uk <- fromJSON("https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=overview&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesBySpecimenDate%22:%22newCasesBySpecimenDate%22,%22cumCasesBySpecimenDate%22:%22cumCasesBySpecimenDate%22%7D&format=json")
uk <- uk$data

## Alterar para formato Data
uk$date <- as.Date(uk$date, "%Y-%m-%d")
### Ordenar por data
uk <- as.data.frame(uk[order(uk$date), ])

## Tabela confirmados novos
uk_var <- uk %>%
  select(date, newCasesBySpecimenDate)
names(uk_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_uk_var <- uk_var  %>%
  filter(uk_var$data > as.Date("2020-02-28")) %>% 
  dplyr::mutate(t_start = dplyr::row_number())


## Aplicar a função Estimate_R
Rt_nonparam_si_uk <- estimate_R(as.numeric(covid_uk_var$confirmados_novos), 
                                method = "uncertain_si",
                                config = sens_configs
)

sample_windows_uk <- seq(length(Rt_nonparam_si_uk$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_uk <- 
  map(.x = sample_windows_uk,
      .f = function(x) {
        
        posterior_sample_obj_uk <- 
          sample_posterior_R(
            R = Rt_nonparam_si_uk,
            n = 1000, 
            window = x )
        
        posterior_sample_estim_uk <- 
          data.frame(
            window_index = x,
            window_t_start = Rt_nonparam_si_uk$R$t_start[x],
            window_t_end = Rt_nonparam_si_uk$R$t_end[x],
            date_point = covid_uk_var[covid_uk_var$t_start == Rt_nonparam_si_uk$R$t_end[x], "data"],
            R_e_median = median(posterior_sample_obj_uk),
            R_e_q0025 = quantile(posterior_sample_obj_uk, probs = 0.025),
            R_e_q0975 = quantile(posterior_sample_obj_uk, probs = 0.975))
        
        return(posterior_sample_estim_uk)}
  ) %>% 
  
  reduce(bind_rows)


## Gráfico Reino Unido ggplot
## Linhas a adicionar no gráfico
d_uk = data.frame(date=as.Date(c("2020-03-24", "2020-05-22", "2020-09-14", "2020-11-05")), Evento=c("Confinamento obrigatório", "Quarentena obrigatória para quem chega", "Proibição de ajuntamentos", "Confinamento obrigatório"))

graph_uk <- ggplot(posterior_Rt_uk, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "plum4",  alpha = 0.65, size = 1,  aes(group = 1, text = paste('Data: ', date_point,
                                                                                    '<br>Rt médio: ', R_e_median))) +
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.2, fill = "rosybrown2") +
  
  labs( title = "Reino Unido", size= 10,
        subtitle = "Evolução do Número Efetivo Reprodutivo ao longo do tempo",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)", 
        caption = "Fonte: Governo do Reino Unido"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_uk_var$data), max((posterior_Rt_uk$date_point)))
  ) +
  
  scale_y_continuous(
    breaks = 0:10,
    limits = c(0, 10)
  ) +
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) + 
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-24", "2020-05-22", "2020-09-14", "2020-11-05"))), linetype = c("solid", "dotted", "twodash", "solid"), colour = "darkred" , alpha = 0.5) +
  geom_vline(data=d_uk, mapping =  aes(xintercept = date, linetype = Evento, ), size = 1, colour = "darkred", alpha = 0.5, show.legend = FALSE)+ 
  geom_pointrange(data = last(posterior_Rt_uk), mapping = aes(x = date_point, y = R_e_median, ymin = R_e_q0025, ymax = R_e_q0975), stat = "identity", position = "identity", colour = "indianred4", size = 1,5, alpha = 0.8, linetype = "solid") + 
  annotate(geom = "text", x = last(posterior_Rt_uk$date_point), y = last(posterior_Rt_uk$R_e_median) - 0.5, label = round(last(posterior_Rt_uk$R_e_median), digits = 3), size = 3)

### Tornar gráfico interativo
ggplotly(graph_uk, tooltip = "text", width = 900, height = 450) %>%
  layout(title = list(text = paste0("Reino Unido", "<br>", "<sup>", "Evolução do Número Efetivo Reprodutivo ao longo do tempo", "</sup>"),font=list(face="bold")), legend = list(x = 100, y = 0.5))








#AUSTRÁLIA (https://github.com/M3IT/COVID-19_Data/blob/master/Data/COVID_AU_national_daily_change.csv)
australia <- read.csv("https://raw.githubusercontent.com/M3IT/COVID-19_Data/master/Data/COVID_AU_national_daily_change.csv")

## Alterar para formato Data
australia$date <- as.Date(australia$date, "%Y-%m-%d")

## Tabela confirmados novos
aus_var <- australia %>%
  select(date, confirmed)
names(aus_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_aus_var <- aus_var  %>%
  filter(aus_var$data > as.Date("2020-01-25")) %>% 
  dplyr::mutate(t_start = dplyr::row_number())

## Aplicar a função Estimate_R
Rt_nonparam_si_aus <- estimate_R(as.numeric(covid_aus_var$confirmados_novos),
                                 method = "uncertain_si",
                                 config = sens_configs)

sample_windows_aus <- seq(length(Rt_nonparam_si_aus$R$t_start))

# Criar um data frame com valores de R
posterior_Rt_aus <- 
  map(
    .x = sample_windows_aus,
    .f = function(x) {
      
      posterior_sample_obj_aus <- 
        sample_posterior_R(
          R = Rt_nonparam_si_aus,
          n = 1000, 
          window = x
        )
      
      posterior_sample_estim_aus <- 
        data.frame(
          window_index = x,
          window_t_start = Rt_nonparam_si_aus$R$t_start[x],
          window_t_end = Rt_nonparam_si_aus$R$t_end[x],
          date_point = covid_aus_var[covid_aus_var$t_start == Rt_nonparam_si_aus$R$t_end[x], "data"],
          R_e_median = median(posterior_sample_obj_aus),
          R_e_q0025 = quantile(posterior_sample_obj_aus, probs = 0.025),
          R_e_q0975 = quantile(posterior_sample_obj_aus, probs = 0.975)
        )
      
      return(posterior_sample_estim_aus)
      
    }
  ) %>% 
  reduce(bind_rows)

#Grafico Australia
## Linhas a adicionar no gráfico
d_aus = data.frame(date=as.Date(c("2020-03-16", "2020-03-20", "2020-06-30", "2020-08-02", "2020-09-27")), Evento=c("Estado de Emergência - estado Victoria", "Encerramento de fronteiras", "Confinamento obrigatório - estado Victoria", "Estado de Emergência - estado Victoria", "Levantamento gradual das medidas de restrição"))

graph_aus<- ggplot(posterior_Rt_aus, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "cyan4",  alpha = 0.5, size = 1, aes(group = 1, text = paste('Data: ', date_point,
                                                                                  '<br>Rt médio: ', R_e_median))) +
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "cyan3") +
  
  labs( title = " Austrália", size= 10,
        subtitle = "Evolução do Número Efetivo Reprodutivo ao longo do tempo",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)", 
        caption = "Fonte: Matt Bolton"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_aus_var$data), max(posterior_Rt_aus$date_point))
  ) +
  
  scale_y_continuous(
    breaks = 0:10,
    limits = c(0, 10)
  ) +
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) + 
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-16", "2020-03-20", "2020-06-30", "2020-08-02", "2020-09-27"))), linetype = c("dotted", "twodash", "solid", "dotted", "dotdash"), colour = "darkred" , alpha = 0.5) +
  geom_vline(data=d_aus, mapping =  aes(xintercept = date, linetype = Evento, ), size = 1, colour = "darkred", alpha = 0.5, show.legend = FALSE)+ 
  geom_pointrange(data = last(posterior_Rt_aus), mapping = aes(x = date_point, y = R_e_median, ymin = R_e_q0025, ymax = R_e_q0975), stat = "identity", position = "identity", colour = "indianred4", size = 1,5, alpha = 0.8, linetype = "solid") + 
  annotate(geom = "text", x = last(posterior_Rt_aus$date_point), y = last(posterior_Rt_aus$R_e_median) - 0.5, label = round(last(posterior_Rt_aus$R_e_median), digits = 3), size = 3)

#Tornar o grafico interativo
ggplotly(graph_aus, tooltip = "text", width = 900, height = 450) %>%
  layout(title = list(text = paste0("Austrália", "<br>", "<sup>", "Evolução do Número Efetivo Reprodutivo ao longo do tempo", "</sup>"),font=list(face="bold")), legend = list(x = 100, y = 0.5))







# NOVA ZELÂNDIA
## Base de dados WHO (https://covid19.who.int/table)
nzealand <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")

## Alterar formato para data
nzealand$Date_reported <- as.Date(nzealand$Date_reported, "%Y-%m-%d")

## Criar tabela confirmados novos
nzealand_var <- nzealand %>%
  filter(Country == "New Zealand") %>%
  select(Date_reported, New_cases)
names(nzealand_var) <- c("data", "confirmados_novos")

## Alterar para formato Data
nzealand_var$data <- as.Date(nzealand_var$data, "%Y-%m-%d")

## Previsão da evolução
covid_nze_var <- nzealand_var  %>%
  filter(nzealand_var$data > as.Date("2020-01-03")) %>% 
  dplyr::mutate(t_start = dplyr::row_number())


## Aplicar a função Estimate_R
Rt_nonparam_si_nze <- estimate_R(as.numeric(covid_nze_var$confirmados_novos), 
                                 method = "uncertain_si",
                                 config = sens_configs
)

sample_windows_nze <- seq(length(Rt_nonparam_si_nze$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_nze <- 
  map(.x = sample_windows_nze,
      .f = function(x) {
        
        posterior_sample_obj_nze <- 
          sample_posterior_R(
            R = Rt_nonparam_si_nze,
            n = 1000, 
            window = x )
        
        posterior_sample_estim_nze <- 
          data.frame(
            window_index = x,
            window_t_start = Rt_nonparam_si_nze$R$t_start[x],
            window_t_end = Rt_nonparam_si_nze$R$t_end[x],
            date_point = covid_nze_var[covid_nze_var$t_start == Rt_nonparam_si_nze$R$t_end[x], "data"],
            R_e_median = median(posterior_sample_obj_nze),
            R_e_q0025 = quantile(posterior_sample_obj_nze, probs = 0.025),
            R_e_q0975 = quantile(posterior_sample_obj_nze, probs = 0.975))
        
        return(posterior_sample_estim_nze)}
  ) %>% 
  
  reduce(bind_rows)

## Gráfico Nova Zelândia ggplot
## Linhas a adicionar no gráfico
d_nze = data.frame(date=as.Date(c("2020-03-18", "2020-03-25", "2020-06-08")), Evento=c("Encerramento de fronteiras", "Confinamento obrigatório", "Levantamento gradual das medidas de restrição"))

graph_nze <- ggplot(posterior_Rt_nze, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "darkorange",  alpha = 0.5, size = 1, aes(group = 1, text = paste('Data: ', date_point,
                                                                                       '<br>Rt médio: ', R_e_median))) +
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "darkorange") +
  
  labs( title = " Nova Zelândia", size= 10,
        subtitle = "Evolução do Número Efetivo Reprodutivo ao longo do tempo",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)", 
        caption = "Fonte: OMS"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_nze_var$data), max((posterior_Rt_nze$date_point)))
  ) +
  
  scale_y_continuous(
    breaks = 0:10,
    limits = c(0, 10)
  ) +
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) + 
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-18", "2020-03-25", "2020-06-08"))), linetype = c("dotted", "twodash", "solid"), colour = "darkred" , alpha = 0.5) +
  geom_vline(data=d_nze, mapping =  aes(xintercept = date, linetype = Evento, ), size = 1, colour = "darkred", alpha = 0.5, show.legend = FALSE)+ 
  geom_pointrange(data = last(posterior_Rt_nze), mapping = aes(x = date_point, y = R_e_median, ymin = R_e_q0025, ymax = R_e_q0975), stat = "identity", position = "identity", colour = "indianred4", size = 1,5, alpha = 0.8, linetype = "solid") + 
  annotate(geom = "text", x = last(posterior_Rt_nze$date_point), y = last(posterior_Rt_nze$R_e_median) - 0.5, label = round(last(posterior_Rt_nze$R_e_median), digits = 3), size = 3)

### Tornar gráfico interativo
ggplotly(graph_nze, tooltip = "text", width = 900, height = 450) %>%
  layout(title = list(text = paste0("Nova Zelândia", "<br>", "<sup>", "Evolução do Número Efetivo Reprodutivo ao longo do tempo", "</sup>"),font=list(face="bold")), legend = list(x = 100, y = 0.5))







#ÍNDIA (https://api.covid19india.org/documentation/csv/)
india <- read.csv("https://api.covid19india.org/csv/latest/case_time_series.csv")

india$Date_YMD <- as.Date(india$Date_YMD, "%Y-%m-%d")

testing_india <- read.csv("https://www.kaggle.com/sudalairajkumar/covid19-in-india?select=StatewiseTestingDetails.csv")

# Tabela
india_var <- india %>%
  select(Date_YMD, Daily.Confirmed)
names(india_var) <- c("data", "confirmados_novos")

# Previsão da evolução - acrescentar coluna numerada 
covid_india_var <- india_var  %>%
  filter(india_var$data > as.Date("2020-01-31")) %>% 
  dplyr::mutate(t_start = dplyr::row_number())


## Aplicar a função Estimate_R
Rt_nonparam_si_india <- estimate_R(as.numeric(covid_india_var$confirmados_novos),
                                   method = "uncertain_si",
                                   config = sens_configs)

sample_windows_india <- seq(length(Rt_nonparam_si_india$R$t_start))

# Criar um data frame com valores de R
posterior_Rt_india <- 
  map(
    .x = sample_windows_india,
    .f = function(x) {
      
      posterior_sample_obj_india <- 
        sample_posterior_R(
          R = Rt_nonparam_si_india,
          n = 1000, 
          window = x
        )
      
      posterior_sample_estim_india <- 
        data.frame(
          window_index = x,
          window_t_start = Rt_nonparam_si_india$R$t_start[x],
          window_t_end = Rt_nonparam_si_india$R$t_end[x],
          date_point = covid_india_var[covid_india_var$t_start == Rt_nonparam_si_india$R$t_end[x], "data"],
          R_e_median = median(posterior_sample_obj_india),
          R_e_q0025 = quantile(posterior_sample_obj_india, probs = 0.025),
          R_e_q0975 = quantile(posterior_sample_obj_india, probs = 0.975)
        )
      
      return(posterior_sample_estim_india)
      
    }
  ) %>% 
  reduce(bind_rows)

#Grafico Índia
## Linhas a adicionar no gráfico
d_ind = data.frame(date=as.Date(c("2020-03-04", "2020-03-25", "2020-06-08")), Evento=c("Triagem obrigatória a todos os passageiros", "Confinamento obrigatório", "Levantamento gradual das medidas de restrição"))

graph_ind<- ggplot(posterior_Rt_india, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "darkcyan",  alpha = 0.5, size = 1, aes(group = 1, text = paste('Data: ', date_point,
                                                                                     '<br>Rt médio: ', R_e_median))) +
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "darkcyan") +
  
  labs( title = "Índia", size= 10,
        subtitle = "Evolução do Número Efetivo Reprodutivo ao longo do tempo",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)", 
        caption = "Fonte: COVID19 - India API"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 3),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_india_var$data), max(posterior_Rt_india$date_point))
  ) +
  
  scale_y_continuous(
    breaks = seq(from = 0, to = 90, by = 5),
    limits = c(-1, 40)
  ) +
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) + 
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-04", "2020-03-25", "2020-06-08"))), linetype = c("dotted", "solid", "twodash"), colour = "darkred" , alpha = 0.5) +
  geom_vline(data=d_ind, mapping =  aes(xintercept = date, linetype = Evento, ), size = 1, colour = "darkred", alpha = 0.5, show.legend = FALSE)+ 
  geom_pointrange(data = last(posterior_Rt_india), mapping = aes(x = date_point, y = R_e_median, ymin = R_e_q0025, ymax = R_e_q0975), stat = "identity", position = "identity", colour = "indianred4", size = 1,5, alpha = 0.8, linetype = "solid") + 
  annotate(geom = "text", x = last(posterior_Rt_india$date_point), y = last(posterior_Rt_india$R_e_median) - 1.5, label = round(last(posterior_Rt_india$R_e_median), digits = 3), size = 3)

#Tornar o grafico interativo
ggplotly(graph_ind, tooltip = "text", width = 900, height = 450) %>%
  layout(title = list(text = paste0("Índia", "<br>", "<sup>", "Evolução do Número Efetivo Reprodutivo ao longo do tempo", "</sup>"),font=list(face="bold")), legend = list(x = 100, y = 0.5))







# HONG KONG (https://data.gov.hk/en-data/dataset/hk-dh-chpsebcddr-novel-infectious-agent)
hk <- read.csv("http://www.chp.gov.hk/files/misc/enhanced_sur_covid_19_eng.csv")

## Alterar formato de Data
hk$Report.date <- as.Date(hk$Report.date, format = "%d/%m/%Y")

## Selecionar apenas os casos confirmados
hk_var <- hk %>%
  filter(Confirmed.probable == "Confirmed")

## Agrupar o nº de registos por dia
hk_var <- as.data.frame(aggregate(x = hk_var, list(hk_var$Report.date), FUN = length))

## Criar tabela de confirmados novos
hk_var <- hk_var %>%
  select(Group.1, Report.date)
names(hk_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_hk_var <- hk_var  %>%
  filter(hk_var$data > as.Date("2020-02-28")) %>% 
  dplyr::mutate(t_start = dplyr::row_number())

## Aplicar a função Estimate_R
Rt_nonparam_si_hk <- estimate_R(as.numeric(covid_hk_var$confirmados_novos), 
                                method = "uncertain_si",
                                config = sens_configs
)

sample_windows_hk <- seq(length(Rt_nonparam_si_hk$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_hk <- 
  map(.x = sample_windows_hk,
      .f = function(x) {
        
        posterior_sample_obj_hk <- 
          sample_posterior_R(
            R = Rt_nonparam_si_hk,
            n = 1000, 
            window = x )
        
        posterior_sample_estim_hk <- 
          data.frame(
            window_index = x,
            window_t_start = Rt_nonparam_si_hk$R$t_start[x],
            window_t_end = Rt_nonparam_si_hk$R$t_end[x],
            date_point = covid_hk_var[covid_hk_var$t_start == Rt_nonparam_si_hk$R$t_end[x], "data"],
            R_e_median = median(posterior_sample_obj_hk),
            R_e_q0025 = quantile(posterior_sample_obj_hk, probs = 0.025),
            R_e_q0975 = quantile(posterior_sample_obj_hk, probs = 0.975))
        
        return(posterior_sample_estim_hk)}
  ) %>% 
  
  reduce(bind_rows)


## Gráfico Hong Kong ggplot
## Linhas a adicionar no gráfico
d_hk = data.frame(date=as.Date(c("2020-03-25", "2020-07-20", "2020-10-04")), Evento=c("Encerramento de fronteiras", "Novas medidas de restrição", "Proibição de máscara nos protestos"))

graph_hk <- ggplot(posterior_Rt_hk, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "lightseagreen",  alpha = 0.5, size = 1, aes(group = 1, text = paste('Data: ', date_point,
                                                                                          '<br>Rt médio: ', R_e_median))) +
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "lightseagreen") +
  
  labs( title = "Hong Kong", size= 10,
        subtitle = "Evolução do Número Efetivo Reprodutivo ao longo do tempo",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)", 
        caption = "Fonte: Governo de Hong Kong"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_hk_var$data), max((posterior_Rt_hk$date_point)))
  ) +
  
  scale_y_continuous(
    breaks = 0:10,
    limits = c(0, 10)
  ) +
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) + 
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-25", "2020-07-20", "2020-10-04"))), linetype = c("solid", "twodash", "dotted"), colour = "darkred" , alpha = 0.5) +
  geom_vline(data=d_hk, mapping =  aes(xintercept = date, linetype = Evento, ), size = 1, colour = "darkred", alpha = 0.5, show.legend = FALSE)+ 
  geom_pointrange(data = last(posterior_Rt_hk), mapping = aes(x = date_point, y = R_e_median, ymin = R_e_q0025, ymax = R_e_q0975), stat = "identity", position = "identity", colour = "indianred4", size = 1,5, alpha = 0.8, linetype = "solid") + 
  annotate(geom = "text", x = last(posterior_Rt_hk$date_point), y = last(posterior_Rt_hk$R_e_median) - 0.5, label = round(last(posterior_Rt_hk$R_e_median), digits = 3), size = 3)

### Tornar gráfico interativo
ggplotly(graph_hk, tooltip = "text", width = 900, height = 450) %>%
  layout(title = list(text = paste0("Hong Kong", "<br>", "<sup>", "Evolução do Número Efetivo Reprodutivo ao longo do tempo", "</sup>"),font=list(face="bold")), legend = list(x = 100, y = 0.5))






#CHINA (https://covid19.who.int/table)
china <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")

## Alterar formato para data
china$Date_reported <- as.Date(china$Date_reported, "%Y-%m-%d")

## Criar tabela confirmados novos
chi_var <- china %>%
  filter(Country == "China") %>%
  select(Date_reported, New_cases)
names(chi_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_chi_var <- chi_var  %>%
  filter(chi_var$data > as.Date("2020-01-03")) %>% 
  dplyr::mutate(t_start = dplyr::row_number())

## Aplicar a função Estimate_R
Rt_nonparam_si_chi <- estimate_R(as.numeric(covid_chi_var$confirmados_novos), 
                                 method = "uncertain_si",
                                 config = sens_configs
)

sample_windows_chi <- seq(length(Rt_nonparam_si_chi$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_chi <- 
  map(.x = sample_windows_chi,
      .f = function(x) {
        
        posterior_sample_obj_chi <- 
          sample_posterior_R(
            R = Rt_nonparam_si_chi,
            n = 1000, 
            window = x )
        
        posterior_sample_estim_chi <- 
          data.frame(
            window_index = x,
            window_t_start = Rt_nonparam_si_chi$R$t_start[x],
            window_t_end = Rt_nonparam_si_chi$R$t_end[x],
            date_point = covid_chi_var[covid_chi_var$t_start == Rt_nonparam_si_chi$R$t_end[x], "data"],
            R_e_median = median(posterior_sample_obj_chi),
            R_e_q0025 = quantile(posterior_sample_obj_chi, probs = 0.025),
            R_e_q0975 = quantile(posterior_sample_obj_chi, probs = 0.975))
        
        return(posterior_sample_estim_chi)}
  ) %>% 
  
  reduce(bind_rows)

## Gráfico China ggplot
## Linhas a adicionar no gráfico
d_chi = data.frame(date=as.Date(c("2020-01-23", "2020-03-22", "2020-07-15", "2020-08-27", "2020-10-26")), Evento=c("Confinamento total em Hubei", "Desconfinamento gradual em Hubei", "Confinamento total em Xinjiang", "Desconfinamento gradual em Xinjiang", "Confinamento parcial em Xinjiang"))

graph_chi <- ggplot(posterior_Rt_chi, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "aquamarine4",  alpha = 0.5, size = 1, aes(group = 1, text = paste('Data: ', date_point,
                                                                                        '<br>Rt médio: ', R_e_median))) +
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "aquamarine4") +
  
  labs( title = " China", size= 10,
        subtitle = "Evolução do Número Efetivo Reprodutivo ao longo do tempo",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)", 
        caption = "Fonte: OMS"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_chi_var$data), max((posterior_Rt_chi$date_point)))
  ) +
  
  scale_y_continuous(
    breaks = 0:10,
    limits = c(0, 10)
  ) +
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) + 
  geom_vline(xintercept = as.numeric(as.Date(c("2020-01-23", "2020-03-22", "2020-07-15", "2020-08-27", "2020-10-26"))), linetype = c("twodash", "dotdash", "dotted","dashed", "solid"), colour = "darkred" , alpha = 0.5) +
  geom_vline(data=d_chi, mapping =  aes(xintercept = date, linetype = Evento, ), size = 1, colour = "darkred", alpha = 0.5, show.legend = FALSE) + 
  geom_pointrange(data = last(posterior_Rt_chi), mapping = aes(x = date_point, y = R_e_median, ymin = R_e_q0025, ymax = R_e_q0975), stat = "identity", position = "identity", colour = "indianred4", size = 1,5, alpha = 0.8, linetype = "solid") + 
  annotate(geom = "text", x = last(posterior_Rt_chi$date_point), y = last(posterior_Rt_chi$R_e_median) - 0.5, label = round(last(posterior_Rt_chi$R_e_median), digits = 3), size = 3)

### Tornar gráfico interativo
ggplotly(graph_chi, tooltip = "text", width = 900, height = 450) %>%
  layout(title = list(text = paste0("China", "<br>", "<sup>", "Evolução do Número Efetivo Reprodutivo ao longo do tempo", "</sup>"),font=list(face="bold")), legend = list(x = 100, y = 0.5))









# EUA (https://github.com/nytimes/covid-19-data)
usa <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")

## Alterar para formato Data
usa$date <- as.Date(usa$date, "%Y-%m-%d")

## Criar tabela confirmados novos
usa_var <- mutate(usa, new = usa$cases-lag(usa$cases))
usa_var <- usa_var[,c(1,4)]
usa_var <- usa_var[-1,]
names(usa_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_usa_var <- usa_var  %>%
  filter(usa_var$data > as.Date("2020-01-29")) %>% 
  dplyr::mutate(t_start = dplyr::row_number())


## Aplicar a função Estimate_R
Rt_nonparam_si_usa <- estimate_R(as.numeric(covid_usa_var$confirmados_novos), 
                                 method = "uncertain_si",
                                 config = sens_configs
)

sample_windows_usa <- seq(length(Rt_nonparam_si_usa$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_usa <- 
  map(.x = sample_windows_usa,
      .f = function(x) {
        
        posterior_sample_obj_usa <- 
          sample_posterior_R(
            R = Rt_nonparam_si_usa,
            n = 1000, 
            window = x )
        
        posterior_sample_estim_usa <- 
          data.frame(
            window_index = x,
            window_t_start = Rt_nonparam_si_usa$R$t_start[x],
            window_t_end = Rt_nonparam_si_usa$R$t_end[x],
            date_point = covid_usa_var[covid_usa_var$t_start == Rt_nonparam_si_usa$R$t_end[x], "data"],
            R_e_median = median(posterior_sample_obj_usa),
            R_e_q0025 = quantile(posterior_sample_obj_usa, probs = 0.025),
            R_e_q0975 = quantile(posterior_sample_obj_usa, probs = 0.975))
        
        return(posterior_sample_estim_usa)}
  ) %>% 
  
  reduce(bind_rows)


## Gráfico USA ggplot
## Linhas a adicionar no gráfico
d_usa = data.frame(date=as.Date(c("2020-03-13", "2020-03-25", "2020-04-13")), Evento=c("Estado de Emergência", "Confinamento total", "Estado de Calamidade"))

graph_usa <- ggplot(posterior_Rt_usa, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "royalblue4",  alpha = 0.5, size = 1, aes(group = 1, text = paste('Data: ', date_point,
                                                                                       '<br>Rt médio: ', R_e_median))) +
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "royalblue2") +
  
  labs( title = " Estados Unidos da América", size= 10,
        subtitle = "Evolução do Número Efetivo Reprodutivo ao longo do tempo",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)", 
        caption = "Fonte: The New York Times"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_usa_var$data), max((posterior_Rt_usa$date_point)))
  ) +
  
  scale_y_continuous(
    breaks = 0:10,
    limits = c(0, 10)
  ) +
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) + 
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-13", "2020-03-25", "2020-04-13"))), linetype = c("dotted", "solid", "twodash"), colour = "darkred" , alpha = 0.5) +
  geom_vline(data=d_usa, mapping =  aes(xintercept = date, linetype = Evento, ), size = 1, colour = "darkred", alpha = 0.5, show.legend = FALSE) + 
  geom_pointrange(data = last(posterior_Rt_usa), mapping = aes(x = date_point, y = R_e_median, ymin = R_e_q0025, ymax = R_e_q0975), stat = "identity", position = "identity", colour = "indianred4", size = 1,5, alpha = 0.8, linetype = "solid") + 
  annotate(geom = "text", x = last(posterior_Rt_usa$date_point), y = last(posterior_Rt_usa$R_e_median) - 0.5, label = round(last(posterior_Rt_usa$R_e_median), digits = 3), size = 3)

### Tornar gráfico interativo
ggplotly(graph_usa, tooltip = "text", width = 900, height = 450) %>%
  layout(title = list(text = paste0("Estados Unidos da América", "<br>", "<sup>", "Evolução do Número Efetivo Reprodutivo ao longo do tempo", "</sup>"),font=list(face="bold")), legend = list(x = 100, y = 0.5))








#JAPÃO - alterar data do j.son todos os dias (https://github.com/reustle/covid19japan-data/tree/master/docs/summary)
japan <- fromJSON("https://raw.githubusercontent.com/reustle/covid19japan-data/master/docs/summary/2020-11-13.json")
japan <- japan$daily

## Alterar formato para data
japan$date <- as.Date(japan$date, "%Y-%m-%d")

## Criar tabela confirmados novos
jap_var <- japan %>%
  select(confirmed, date)
jap_var <- jap_var[, c(2,1)] #trocar posição das colunas
names(jap_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_jap_var <- jap_var  %>%
  filter(jap_var$data > as.Date("2020-02-10")) %>% 
  dplyr::mutate(t_start = dplyr::row_number())

## Aplicar a função Estimate_R
Rt_nonparam_si_jap <- estimate_R(as.numeric(covid_jap_var$confirmados_novos), 
                                 method = "uncertain_si",
                                 config = sens_configs
)

sample_windows_jap <- seq(length(Rt_nonparam_si_jap$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_jap <- 
  map(.x = sample_windows_jap,
      .f = function(x) {
        
        posterior_sample_obj_jap <- 
          sample_posterior_R(
            R = Rt_nonparam_si_jap,
            n = 1000, 
            window = x )
        
        posterior_sample_estim_jap <- 
          data.frame(
            window_index = x,
            window_t_start = Rt_nonparam_si_jap$R$t_start[x],
            window_t_end = Rt_nonparam_si_jap$R$t_end[x],
            date_point = covid_jap_var[covid_jap_var$t_start == Rt_nonparam_si_jap$R$t_end[x], "data"],
            R_e_median = median(posterior_sample_obj_jap),
            R_e_q0025 = quantile(posterior_sample_obj_jap, probs = 0.025),
            R_e_q0975 = quantile(posterior_sample_obj_jap, probs = 0.975))
        
        return(posterior_sample_estim_jap)}
  ) %>% 
  
  reduce(bind_rows)


## Gráfico Japão ggplot
## Linhas a adicionar no gráfico
d_jap = data.frame(date=as.Date(c("2020-04-07", "2020-05-25")), Evento=c("Estado de Emergência com confinamento voluntário", "Suspensão do Estado de Emergência"))

graph_jap <- ggplot(posterior_Rt_jap, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "royalblue",  alpha = 0.5, size = 1, aes(group = 1, text = paste('Data: ', date_point,
                                                                                      '<br>Rt médio: ', R_e_median))) +
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "royalblue") +
  
  labs( title = " Japão", size= 10,
        subtitle = "Evolução do Número Efetivo Reprodutivo ao longo do tempo",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)", 
        caption = "Fonte: Shane Reustle"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_jap_var$data), max((posterior_Rt_jap$date_point)))
  ) +
  
  scale_y_continuous(
    breaks = 0:10,
    limits = c(0, 10)
  ) +
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) + 
  geom_vline(xintercept = as.numeric(as.Date(c("2020-04-07", "2020-05-25"))), linetype = c("solid", "twodash"), colour = "darkred" , alpha = 0.5) +
  geom_vline(data=d_jap, mapping =  aes(xintercept = date, linetype = Evento, ), size = 1, colour = "darkred", alpha = 0.5, show.legend = FALSE)+ 
  geom_pointrange(data = last(posterior_Rt_jap), mapping = aes(x = date_point, y = R_e_median, ymin = R_e_q0025, ymax = R_e_q0975), stat = "identity", position = "identity", colour = "indianred4", size = 1,5, alpha = 0.8, linetype = "solid") + 
  annotate(geom = "text", x = last(posterior_Rt_jap$date_point), y = last(posterior_Rt_jap$R_e_median) - 0.5, label = round(last(posterior_Rt_jap$R_e_median), digits = 3), size = 3)

### Tornar gráfico interativo
ggplotly(graph_jap, tooltip = "text", width = 900, height = 450) %>%
  layout(title = list(text = paste0("Japão", "<br>", "<sup>", "Evolução do Número Efetivo Reprodutivo ao longo do tempo", "</sup>"),font=list(face="bold")), legend = list(x = 100, y = 0.5))







# MÉXICO (https://covid19.who.int/table)
mexico <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")

## Alterar formato para data
mexico$Date_reported <- as.Date(mexico$Date_reported, "%Y-%m-%d")

## Criar tabela confirmados novos
mex_var <- mexico %>%
  filter(Country == "Mexico") %>%
  select(Date_reported, New_cases)
names(mex_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_mex_var <- mex_var  %>%
  filter(mex_var$data > as.Date("2020-02-28")) %>% 
  dplyr::mutate(t_start = dplyr::row_number())


## Aplicar a função Estimate_R
Rt_nonparam_si_mex <- estimate_R(as.numeric(covid_mex_var$confirmados_novos), 
                                 method = "uncertain_si",
                                 config = sens_configs
)

sample_windows_mex <- seq(length(Rt_nonparam_si_mex$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_mex <- 
  map(.x = sample_windows_mex,
      .f = function(x) {
        
        posterior_sample_obj_mex <- 
          sample_posterior_R(
            R = Rt_nonparam_si_mex,
            n = 1000, 
            window = x )
        
        posterior_sample_estim_mex <- 
          data.frame(
            window_index = x,
            window_t_start = Rt_nonparam_si_mex$R$t_start[x],
            window_t_end = Rt_nonparam_si_mex$R$t_end[x],
            date_point = covid_mex_var[covid_mex_var$t_start == Rt_nonparam_si_mex$R$t_end[x], "data"],
            R_e_median = median(posterior_sample_obj_mex),
            R_e_q0025 = quantile(posterior_sample_obj_mex, probs = 0.025),
            R_e_q0975 = quantile(posterior_sample_obj_mex, probs = 0.975))
        
        return(posterior_sample_estim_mex)}
  ) %>% 
  
  reduce(bind_rows)

## Gráfico México ggplot
## Linhas a adicionar no gráfico
d_mex = data.frame(date=as.Date(c("2020-03-31", "2020-06-01")), Evento=c("Estado de Emergência", "Desconfinamento gradual"))

graph_mex <- ggplot(posterior_Rt_mex, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "chocolate3",  alpha = 0.5, size = 1, aes(group = 1, text = paste('Data: ', date_point,
                                                                                       '<br>Rt médio: ', R_e_median))) +
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "chocolate1") +
  
  labs( title = " México", size= 10,
        subtitle = "Evolução do Número Efetivo Reprodutivo ao longo do tempo",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)", 
        caption = "Fonte: OMS"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_mex_var$data), max((posterior_Rt_mex$date_point)))
  ) +
  
  scale_y_continuous(
    breaks = 0:10,
    limits = c(0, 10)
  ) +
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) + 
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-31", "2020-06-01"))), linetype = c("twodash", "solid"), colour = "darkred" , alpha = 0.5) +
  geom_vline(data=d_mex, mapping =  aes(xintercept = date, linetype = Evento, ), size = 1, colour = "darkred", alpha = 0.5, show.legend = FALSE) + 
  geom_pointrange(data = last(posterior_Rt_mex), mapping = aes(x = date_point, y = R_e_median, ymin = R_e_q0025, ymax = R_e_q0975), stat = "identity", position = "identity", colour = "indianred4", size = 1,5, alpha = 0.8, linetype = "solid") + 
  annotate(geom = "text", x = last(posterior_Rt_mex$date_point), y = last(posterior_Rt_mex$R_e_median) - 0.5, label = round(last(posterior_Rt_mex$R_e_median), digits = 3), size = 3)

### Tornar gráfico interativo
ggplotly(graph_mex, tooltip = "text", width = 900, height = 450) %>%
  layout(title = list(text = paste0("México", "<br>", "<sup>", "Evolução do Número Efetivo Reprodutivo ao longo do tempo", "</sup>"),font=list(face="bold")), legend = list(x = 100, y = 0.5))







# COREIA DO SUL (https://github.com/owid/covid-19-data/blob/master/public/data/ecdc/new_cases.csv)
korea <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/ecdc/new_cases.csv")

## Alterar formato para data
korea$date <- as.Date(korea$date, "%Y-%m-%d")

## Criar tabela confirmados novos
kor_var <- korea %>% 
  select(date, South.Korea)
names(kor_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_kor_var <- kor_var  %>%
  filter(kor_var$data > as.Date("2020-01-20")) %>% 
  dplyr::mutate(t_start = dplyr::row_number())

## Aplicar a função Estimate_R
Rt_nonparam_si_kor <- estimate_R(as.numeric(covid_kor_var$confirmados_novos), 
                                 method = "uncertain_si",
                                 config = sens_configs
)

sample_windows_kor <- seq(length(Rt_nonparam_si_kor$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_kor <- 
  map(.x = sample_windows_kor,
      .f = function(x) {
        
        posterior_sample_obj_kor <- 
          sample_posterior_R(
            R = Rt_nonparam_si_kor,
            n = 1000, 
            window = x )
        
        posterior_sample_estim_kor <- 
          data.frame(
            window_index = x,
            window_t_start = Rt_nonparam_si_kor$R$t_start[x],
            window_t_end = Rt_nonparam_si_kor$R$t_end[x],
            date_point = covid_kor_var[covid_kor_var$t_start == Rt_nonparam_si_kor$R$t_end[x], "data"],
            R_e_median = median(posterior_sample_obj_kor),
            R_e_q0025 = quantile(posterior_sample_obj_kor, probs = 0.025),
            R_e_q0975 = quantile(posterior_sample_obj_kor, probs = 0.975))
        
        return(posterior_sample_estim_kor)}
  ) %>% 
  
  reduce(bind_rows)

## Gráfico Coreia do Sul ggplot
## Linhas a adicionar no gráfico
d_kor = data.frame(date=as.Date(c("2020-02-21", "2020-04-01", "2020-08-16", "2020-10-12")), Evento=c("Estado de Emergência em Daegu e Cheongdo", "Quarentena obrigatória para passageiros dos EUA e Europa", "Confinamento parcial", "Levantamento gradual das restrições"))

graph_kor <- ggplot(posterior_Rt_kor, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "seagreen",  alpha = 0.5, size = 1, aes(group = 1, text = paste('Data: ', date_point,
                                                                                     '<br>Rt médio: ', R_e_median))) +
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "seagreen") +
  
  labs( title = " Coreia do Sul", size= 10,
        subtitle = "Evolução do Número Efetivo Reprodutivo ao longo do tempo",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)", 
        caption = "Fonte: Our World in Data"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_kor_var$data), max((posterior_Rt_kor$date_point)))
  ) +
  
  scale_y_continuous(
    breaks = 0:15,
    limits = c(0, 15)
  ) +
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) + 
  geom_vline(xintercept = as.numeric(as.Date(c("2020-02-21", "2020-04-01", "2020-08-16", "2020-10-12"))), linetype = c("twodash", "dotdash", "solid", "dotted"), colour = "darkred" , alpha = 0.5) +
  geom_vline(data=d_kor, mapping =  aes(xintercept = date, linetype = Evento, ), size = 1, colour = "darkred", alpha = 0.5, show.legend = FALSE)+ 
  geom_pointrange(data = last(posterior_Rt_kor), mapping = aes(x = date_point, y = R_e_median, ymin = R_e_q0025, ymax = R_e_q0975), stat = "identity", position = "identity", colour = "indianred4", size = 1,5, alpha = 0.8, linetype = "solid") + 
  annotate(geom = "text", x = last(posterior_Rt_kor$date_point), y = last(posterior_Rt_kor$R_e_median) - 0.5, label = round(last(posterior_Rt_kor$R_e_median), digits = 3), size = 3)

### Tornar gráfico interativo
ggplotly(graph_kor, tooltip = "text", width = 900, height = 450) %>%
  layout(title = list(text = paste0("Coreia do Sul", "<br>", "<sup>", "Evolução do Número Efetivo Reprodutivo ao longo do tempo", "</sup>"),font=list(face="bold")), legend = list(x = 100, y = 0.5))







# BRASIL (https://covid19.who.int/table)
brasil <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")

## Alterar formato para data
brasil$Date_reported <- as.Date(brasil$Date_reported, "%Y-%m-%d")

## Criar tabela confirmados novos
bra_var <- brasil %>%
  filter(Country == "Brazil") %>%
  select(Date_reported, New_cases)
names(bra_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_bra_var <- bra_var  %>%
  filter(bra_var$data > as.Date("2020-02-28")) %>% 
  dplyr::mutate(t_start = dplyr::row_number())

## Aplicar a função Estimate_R
Rt_nonparam_si_bra <- estimate_R(as.numeric(covid_bra_var$confirmados_novos), 
                                 method = "uncertain_si",
                                 config = sens_configs
)

sample_windows_bra <- seq(length(Rt_nonparam_si_bra$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_bra <- 
  map(.x = sample_windows_bra,
      .f = function(x) {
        
        posterior_sample_obj_bra <- 
          sample_posterior_R(
            R = Rt_nonparam_si_bra,
            n = 1000, 
            window = x )
        
        posterior_sample_estim_bra <- 
          data.frame(
            window_index = x,
            window_t_start = Rt_nonparam_si_bra$R$t_start[x],
            window_t_end = Rt_nonparam_si_bra$R$t_end[x],
            date_point = covid_bra_var[covid_bra_var$t_start == Rt_nonparam_si_bra$R$t_end[x], "data"],
            R_e_median = median(posterior_sample_obj_bra),
            R_e_q0025 = quantile(posterior_sample_obj_bra, probs = 0.025),
            R_e_q0975 = quantile(posterior_sample_obj_bra, probs = 0.975))
        
        return(posterior_sample_estim_bra)}
  ) %>% 
  
  reduce(bind_rows)

## Gráfico Brasil ggplot
d_bra = data.frame(date=as.Date(c("2020-03-17", "2020-05-07", "2020-06-04")), Evento=c("Estado de Emergência - São Paulo e Rio de Janeiro", "Confinamento parcial obrigatório", "Levantamento gradual das medidas de restrição"))

graph_bra <- ggplot(posterior_Rt_bra, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "cadetblue",  alpha = 0.5, size = 1, aes(group = 1, text = paste('Data: ', date_point,
                                                                                      '<br>Rt médio: ', R_e_median))) +
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "cadetblue") +
  
  labs( title = " Brasil", size= 10,
        subtitle = "Evolução do Número Efetivo Reprodutivo ao longo do tempo",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)", 
        caption = "Fonte: OMS"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_bra_var$data), max((posterior_Rt_bra$date_point)))
  ) +
  
  scale_y_continuous(
    breaks = 0:10,
    limits = c(0, 10)
  ) +
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) +
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-17", "2020-05-07", "2020-06-04"))), linetype = c("twodash", "solid", "dotted"), colour = "darkred" , alpha = 0.5) +
  geom_vline(data=d_bra, mapping =  aes(xintercept = date, linetype = Evento, ), size = 1, colour = "darkred", alpha = 0.5, show.legend = FALSE)+ 
  geom_pointrange(data = last(posterior_Rt_bra), mapping = aes(x = date_point, y = R_e_median, ymin = R_e_q0025, ymax = R_e_q0975), stat = "identity", position = "identity", colour = "indianred4", size = 1,5, alpha = 0.8, linetype = "solid") + 
  annotate(geom = "text", x = last(posterior_Rt_bra$date_point), y = last(posterior_Rt_bra$R_e_median) - 0.5, label = round(last(posterior_Rt_bra$R_e_median), digits = 3), size = 3)

### Tornar gráfico interativo
ggplotly(graph_bra, tooltip = "text", width = 900, height = 450) %>%
  layout(title = list(text = paste0("Brasil", "<br>", "<sup>", "Evolução do Número Efetivo Reprodutivo ao longo do tempo", "</sup>"),font=list(face="bold")), legend = list(x = 100, y = 0.5))




# Gráfico último Rt
## Tabela com último Rt para cada país
last_Rt_paises <- last_Rt <- as.data.frame(rbind(last(posterior_R_t), last(posterior_Rt_it), last(posterior_Rt_ger), last(posterior_Rt_spa), last(posterior_Rt_bel), last(posterior_Rt_cz), last(posterior_Rt_swi), last(posterior_Rt_swe), last(posterior_Rt_uk), last(posterior_Rt_aus), last(posterior_Rt_india), last(posterior_Rt_hk), last(posterior_Rt_chi), last(posterior_Rt_usa), last(posterior_Rt_jap), last(posterior_Rt_mex), last(posterior_Rt_kor), last(posterior_Rt_bra)))
last_Rt_paises <- last_Rt_paises[, -c(1:4)]
last_Rt_paises <- cbind(c("Portugal", "Itália", "Alemanha", "Espanha", "Bélgica", "República Checa", "Suiça", "Suécia", "Reino Unido", "Austrália", "Índia", "Hong Kong", "China", "Estados Unidos", "Japão", "México", "Coreia do Sul", "Brasil"), last_Rt_paises)
names(last_Rt_paises)[1] <- "País"

##GGPlot
ggplot(last_Rt_paises, aes(x = País, y = R_e_median, color = País)) + 
  labs(title = "Rt atual em países da UE e Extracomunitários ",
       x = "País",
       y = "Número Reprodutivo Efetivo (Rt)") +
  theme_minimal() +
  theme(plot.title = element_text(size=10, face= "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10)) +
  scale_y_continuous(breaks = seq(0, max(last_Rt$R_e_q0975), by = 0.1)) + 
  geom_pointrange(aes(ymin = R_e_q0025, ymax = R_e_q0975), size = 0.5, alpha = 0.8) +
  geom_text_repel(label = round((last_Rt$R_e_median), digits = 2), color = "black", size = 2.5) +  
  geom_hline(yintercept = 1, colour = "grey65")




