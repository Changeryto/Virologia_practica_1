# R version 4.3.1 (2023-06-16) -- "Beagle Scouts"
# Autor: Rubén Téllez Gerardo


library(ggplot2)
setwd("~/Documentos/ENCB/Virología/Practica1_graficas")

viro = read.csv("Datos_virología_P1.csv", stringsAsFactors = TRUE)
viro$log.cel.cm. = log10(viro$cel.cm.)

viro$Método[viro$Método == 1] = "Conteo en microscopio"
viro$Método[viro$Método == 2] = "Conteo en Counter"


# Gráficas por equipo

for (eq in 1:10) {
  datos_eq = subset(viro, viro$Equipo==eq)
  datos_eq$Integrante = droplevels.factor(datos_eq$Integrante)
  personas = levels(datos_eq$Integrante)
  
  
  ggplot(data = datos_eq, aes(x = Hora, y = log.cel.cm., colour = Integrante))+
    geom_line()+
    geom_point(aes(shape = Integrante))+
    scale_x_continuous(breaks = seq(0, 96, 24))+
    scale_y_continuous(breaks = seq(1, 6, 0.2))+
    labs(y = "log cel/cm²",
         x = "Hora",
         caption= sprintf("Gráfica del equipo %i.\nSe muestra la lectura de cada integrante y el Counter.", eq))
  
    ggsave(
      filename = sprintf("Gráfica 1 del equipo %i, .png", eq),
      plot = last_plot(),
      width = 6.46,
      height = 4.97
      )
    
    # Tratando método 2
    
    
    ggplot(data = datos_eq, aes(x = Hora, y = log.cel.cm., colour = Método, shape = Método))+
      stat_summary(geom="line", fun.y = "mean")+
      stat_summary(geom="point", fun.y = "mean", size=2)+
      stat_summary(geom = "errorbar", fun.data = "mean_sdl", fun.args = list(mult = 1), width=5)+
      scale_x_continuous(breaks = seq(0, 96, 24))+
      scale_y_continuous(breaks = seq(1, 6, 0.1))+
      labs(y = "log cel/cm²",
           x = "Hora",
           caption= sprintf("Gráfica del equipo %i.\nSe muestra el promedio de lecturas ± DS\ndel microscopio contra Counter", eq))
      
    
    ggsave(
      filename = sprintf("Gráfica 2 del equipo %i, .png", eq),
      plot = last_plot(),
      width = 6.46,
      height = 4.97
    )
}




# Gráfica grupal

ggplot(data = viro, aes(x = Hora, y = log.cel.cm., colour = Método, shape = Método))+
  stat_summary(geom="line", fun.y = "mean")+
  stat_summary(geom="point", fun.y = "mean", size=2)+
  stat_summary(geom = "errorbar", fun.data = "mean_sdl", fun.args = list(mult = 1), width=5)+
  scale_x_continuous(breaks = seq(0, 96, 24))+
  scale_y_continuous(breaks = seq(1, 6, 0.1))+
  labs(y = "log cel/cm²",
       x = "Hora",
       caption= "Gráfica del grupo 8QV1.\nSe muestra el promedio de lecturas ± DS\ndel microscopio contra Counter")

ggsave(
  filename = "Gráfica del grupo 8QV1.png",
  plot = last_plot(),
  width = 8,
  height = 4.97
)



viro$Hora = as.factor(viro$Hora)
ggplot(data = viro, aes(x = Hora, y = log.cel.cm., colour = Método))+
  #geom_point(aes(shape = Método))+
  geom_boxplot(notch = TRUE)+
  #scale_x_continuous(breaks = seq(0, 96, 24))+
  scale_y_continuous(breaks = seq(1, 6, 0.2))+
  labs(y = "log cel/cm²",
       x = "Hora",
       caption = "Gráfica del grupo 8QV1.\nSe muestran gráficos de cajas de todas las horas medidas,\noutliers e intervalo de confianza: Mediana ± 1.57 RIC / n^(0.5)")

ggsave(
  filename = "Gráfica cajas del grupo 8QV1.png",
  plot = last_plot(),
  width = 8,
  height = 4.97
)


#mean(unlist(subset(datos_eq, datos_eq$Hora == 96 & datos_eq$Método == "Conteo en microscopio")[8])) + 
#  sd(unlist(subset(datos_eq, datos_eq$Hora == 96 & datos_eq$Método == "Conteo en microscopio")[8]))
