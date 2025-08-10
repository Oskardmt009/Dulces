# Base de datos con variable cualitativa

library(readxl)
Dulces <- read_excel("Sabor de dulces.xlsx")
View(Dulces)


install.packages("ggplot2")
#Medidas de tendencia central

# Calcular la moda (se instala el paquete primero)

max_cantidad <- Dulces[which.max(Dulces$Cantidad), ]
print(max_cantidad)

#Gráficas

library(ggplot2)
library(dplyr)


#Diagrama de barras

ggplot(Dulces, aes(x = Sabor, y = Cantidad)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Cantidad de dulces por sabor", x = "Sabor", y = "Cantidad")

##Diagrama de Torta

# Agrupar por Sabor y sumar la cantidad de dulces
datos_frecuencia <- Dulces %>%
  group_by(Sabor) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Porcentaje = Cantidad / sum(Cantidad) * 100)

ggplot(datos_frecuencia, aes(x = "", y = Porcentaje, fill = Sabor)) +
  geom_bar(stat = "identity", width = 1) +  # Secciones de la torta
  coord_polar(theta = "y") +  # Convertir a gráfico circular
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
            position = position_stack(vjust = 0.5)) +  # Mostrar porcentajes
  theme_void() +  # Quitar ejes y fondo
  labs(title = "Distribución de Dulces por Sabor (%)") +
  scale_fill_brewer(palette = "Set3")  # Paleta de colores llamativa

#Diagrama de Barras Apiladas

ggplot(datos_frecuencia, aes(x = 1, y = Porcentaje, fill = Sabor)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +  # Gira el gráfico para mejor visualización
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
            position = position_stack(vjust = 0.5)) + 
  theme_minimal() +
  labs(title = "Distribución de Sabores (Barras Apiladas)", x = "", y = "Porcentaje") +
  scale_fill_brewer(palette = "Set3")

#Gráfico de Barras de 100% apiladas

ggplot(datos_frecuencia, aes(x = "", y = Porcentaje, fill = Sabor)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
            position = position_stack(vjust = 0.5)) + 
  theme_minimal() +
  labs(title = "Proporción de Sabores en una Barra", x = "", y = "Porcentaje") +
  scale_fill_brewer(palette = "Paired")

# Gráfico de Donut

ggplot(datos_frecuencia, aes(x = "", y = Porcentaje, fill = Sabor)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
            position = position_stack(vjust = 0.5)) + 
  theme_void() +
  labs(title = "Distribución de Sabores (Donut Chart)") +
  scale_fill_brewer(palette = "Pastel1") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 0, y = 0, label = "Dulces", size = 6)  # Texto en el centro

# Gráfico de Wafflet

install.packages("waffle")  # Si no lo tienes instalado
library(waffle)

waffle_plot <- waffle::waffle(
  round(setNames(datos_frecuencia$Porcentaje, datos_frecuencia$Sabor)),
  rows = 10, size = 0.5,
  colors = RColorBrewer::brewer.pal(n = length(datos_frecuencia$Sabor), "Set3")
)

waffle_plot

#Gráfico de Lollipop

ggplot(datos_frecuencia, aes(x = reorder(Sabor, -Porcentaje), y = Porcentaje)) +
  geom_segment(aes(xend = Sabor, yend = 0), color = "grey") +
  geom_point(size = 6, color = "blue") +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
            vjust = -0.5) +
  theme_minimal() +
  labs(title = "Gráfico de Lollipop - Sabores de Dulces", x = "Sabor", y = "Porcentaje")