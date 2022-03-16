
data_inat <- read_csv("data_inat.csv", show_col_types = FALSE)

data_inat <- data_inat %>% 
  select(., scientific_name, common_name, time_observed_at) %>%
  # Elimina las filas que no tengan fecha
  filter(., !time_observed_at == "")

data_inat <- data_inat%>% 
  mutate(., month_observed_at = month(time_observed_at))

new_inat_data <- data_inat %>%
  count(scientific_name)

data_inat %>% 
  filter(., .$scientific_name=="Amphisbaena darwinii") %>% 
  select(month_observed_at) %>% 
  summarise(., avg = round(mean(month_observed_at)))

#Creo nueva columna
new_inat_data <- new_inat_data %>% 
  add_column(month_mean = NA)

for (sc_name in unique(data_inat$scientific_name)) {
  month_mean_ <- data_inat %>% 
    filter(., .$scientific_name==sc_name) %>% 
    select(month_observed_at) %>% 
    summarise(., avg = round(mean(month_observed_at)))
    as.integer()
  
  new_inat_data <- new_inat_data %>%
    mutate(month_mean = ifelse( 
      scientific_name == sc_name,
      month_mean_,
      month_mean))
}


matriz <- table(datos)
e <- sort(matriz, decreasing = T)
as.integer(e[1])

month_mean_ <- data_inat %>% 
  filter(., .$scientific_name== "Amphisbaena darwinii") %>% 
  select(month_observed_at)


e <- month_mean_ %>% 
  table()  %>% 
  sort(., decreasing = T)

aa<-c(e)[1]
c(aa)

new_inat_data <- new_inat_data %>%
  mutate(month_mean = ifelse( 
    scientific_name == 'Amphisbaena darwinii',
    month_mean_,
    month_mean))



