
## Librerías / Configuraciones Ini
library(dplyr)
library(data.table)
library(ggplot2)
library(lubridate)
library(factoextra)
library(corrplot)
library(FactoMineR)
setwd("C:/Users/usuario/Documents/maestria/Semestre-II/Metodos Multivariados Aplicados/trabajos/maestria_ana_multv")
# Lectura de datos
df <- fread("owid-covid-data.csv")
df_south <- df %>%  filter(continent == "South America")

head(View(df))


# Explicación Variables: 

## stringency_index : 
#This is a composite measure based on nine response indicators including school closures, workplace closures, and
#travel bans, rescaled to a value from 0 to 100 (100 = strictest). If policies vary at the subnational level, the index is
#shown as the response level of the strictest sub-region.



# Calidad de datos ------------------------------------------------------
# Queremos observar la cantidad de datos faltantes por variable
dim1 <- dim(df_south)[1] 
na_printer <- function(x,dim1,df){
messg <- paste("Variable: " , x)
na_pr <- round(sum(is.na(df[[x]])) / dim1 * 100,2)
na_pr_mss <- paste(na_pr,"%")
mssg_na <- paste("Total de datos faltantes", na_pr_mss,sep = " ")

print(messg)
print(mssg_na)





return(c(x,na_pr))
}

columnas <- names(df)
df_total <- data.frame()
new_cols <- c()
for (col in columnas){
  valores <- na_printer(col,dim1,df_south)
  df_val <- data.frame(Variable = valores[1], Pr_Na = valores[2])
  df_total <- rbind(df_total,df_val)
  if ( as.numeric(valores[2]) < 10){
    
    new_cols <- c(new_cols,col)

  }
 
}

df_south_select <- df_south %>% select(all_of(new_cols))

america <- c("North America","South America")
df_total_select_ame <- df %>% select(all_of(new_cols)) %>%
  filter(continent %in% america)


# Agrupamientos -----------------------------------------------------------
## SELECCION DE VARIABLES PARA LA MEDIA
# ELIMINACIÓN SMOOT Y TEST UNITS
columns_s <- names(df_south_select)
columns_smot <- columns_s[grepl("smoothed",columns_s)]
columns_not_select <- c("test_units",columns_smot)
columns_to_select <- columns_s[! columns_s  %in% columns_not_select ]

# AGRUPAMIENTO
agrupamiento_por_media <- function(df_filter)
  {
df_south_agrp <- df_filter %>% 
  select( all_of(columns_to_select)) %>%
  mutate(Mes = month(date),
         Yr = year(date)) %>% 
  group_by(location,Mes,Yr) %>% 
  summarise_if(is.numeric, ~mean(.x,na.rm = TRUE))

df_total_mean <- df_south_agrp %>% 
  group_by(location)%>% 
  summarise_if(is.numeric, ~mean(.x,na.rm = TRUE))

df_total_mean <- na.omit(df_total_mean) %>% 
  select(-"Mes",-"Yr")

return(df_total_mean)
}

df_total_mean_sur <- agrupamiento_por_media(df_south_select)
df_total_mean_america <- agrupamiento_por_media(df_total_select_ame)
# Análsis Gráfico ---------------------------------------------------------

cor_plots_vars <- function(df_total_mean,limite_inf,limite_sup){
  
  cols_names_num<- names(df_total_mean)[limite_inf:limite_sup]
  
  c2<-cor(df_total_mean[cols_names_num])
  corrplot(c2,method = "number",number.cex = .8,tl.cex = 0.8,cl.cex = 0.8,tl.col = "gray2")
  
}
pca_plots <- function(df_total_mean){
  
  df_total_pca <- df_total_mean
  ### PRCOMP
  mt_pca <- as.matrix(df_total_pca[, -1])
  rownames(mt_pca) <- df_total_pca[,1][[1]]
  res.pca <- prcomp(mt_pca,  scale = TRUE)
  # fviz_pca_var(res.pca, col.var = "contrib",
  #              gradient.cols = c("white", "blue", "red"),
  #              ggtheme = theme_minimal())
  
  print(fviz_pca_ind(res.pca, col.ind = "cos2", 
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE))
  
  print(fviz_pca_biplot(res.pca,gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")))
  
  
  # df_total$Pr_Na <- as.numeric(df_total$Pr_Na)
  # datos = data.frame(x = c(1, 2, NA, 3), y = c(NA, NA, 4, 5))
  # sum(is.na(datos))/prod(dim(datos))
  
  return(res.pca)
}
## Correlacion Paises Sur-America
cor_plots_vars(df_total_mean_sur,5,10)
## Correlacion Paises America
cor_plots_vars(df_total_mean_america,10,18)
## PCA SUR-AMERICA
pca_sur <- pca_plots(df_total_mean_sur)
pca_ame <- pca_plots(df_total_mean_america)


print('hola mundo  cruel')