library(tidyverse)
library(firatheme)
library(scales)
options(scipen = 999)
library(hrbrthemes)
library(viridis)
library(showtext)
library(forcats)
library("Cairo")
library(ggthemes)
library(scico)
library(cowplot)
library(Redmonder)
library(cowplot)
library(ggtext)



showtext_auto()

font_add_google(family = "patua-one", "Patua One")
font_add_google(family = "montserrat", "Montserrat")
font_add_google(family = "roboto", "Roboto")


estatal <- read.csv("IDEFC_NM_mar22.csv")


estatal %>%
        glimpse()

estatal_2022 <- estatal %>% 
        filter(Año == 2022) %>%
        filter(Tipo.de.delito == "Homicidio")


estatal_2022 <- estatal_2022 %>%
        select(-Año,-Clave_Ent,-Bien.jurídico.afectado,-Subtipo.de.delito,-Modalidad)


estatal_pivot <- estatal_2022 %>%
        select(-Tipo.de.delito) %>%
        pivot_longer(!Entidad, names_to = "mes", values_to = "total_homicidios")


promedio <- 
        estatal_pivot %>%
        summarize(avg = mean(as.numeric(total_homicidios),na.rm = TRUE)) %>%
        pull(avg)

estatal_pivot %>%
        filter(mes == "Marzo") %>%
        group_by(Entidad) %>%
        summarise(total = sum(total_homicidios)) %>%
        arrange(desc(total)) %>%
        ggplot(aes(x = fct_reorder(Entidad,total), y = total))+
        geom_col(fill = "#F9494C")+
        geom_text(aes(label = str_c(prettyNum(round(total),big.mark = ","))),
                  hjust = -0.1,
                  family = "patua-one",
                  size = 4.6)+
        coord_flip()+
        scale_y_continuous(breaks = seq(0,360,40), expand = expansion(c(0, 0.1), 0))+
        theme_fira()+
        theme(plot.caption =  element_text(size = 13,color = "black", face = "bold", family = "montserrat"),
              plot.title = element_markdown(size = 28,color = "black", face = "bold", family = "patua-one",hjust = -0.01),
              axis.text.x = element_blank(),
              axis.text.y = element_text(family = "montserrat", size = 12, face = "bold"),
              plot.background = element_rect(fill = "white"))+
        labs(title = "Total de <span style = 'color:#F9494C'>homicidios</span> por entidad federativa en el mes de marzo 2022",
             caption = "Fuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública | Visualización: Miguel HG (@mike_dvz)",
             x = "",
             y = ""
             )+
        annotate("text", y = 320, x = "Ciudad de México",family = "patua-one", size = 13, color = "#F9494C",
                 label = "3,632")+
        annotate("text", y = 320, x = "Sinaloa",family = "patua-one", size = 15, color = "black",
                 label = "Todo el país")


ggsave(plot = last_plot(), "estatales.png",
       device = "png",
       height = 1080,
       width = 1920,
       units = "px",
       dpi = 190)




marzo <- estatal %>%
        select(-Clave_Ent,-Bien.jurídico.afectado,-Subtipo.de.delito,-Modalidad) %>%
        filter(Tipo.de.delito == "Homicidio")


marzo <- marzo %>%
        select(Entidad,Año,Enero, Febrero, Marzo, Abril, Mayo, Junio, Julio, Agosto, Septiembre, Octubre ,Noviembre ,Diciembre) %>%
        group_by(Entidad,Año) %>%
        pivot_longer(cols = Enero:Diciembre, names_to = "mes", values_to = "homicidios")




marzo %>%
        filter(mes =="Marzo") %>%
        group_by(Año) %>%
        summarise(total_homicidios = sum(homicidios)) %>%
        ggplot(aes(x = factor(Año), y = total_homicidios), )+
        geom_col(fill = "#F9494C")+
        geom_text(aes(label = str_c(prettyNum(round(total_homicidios),big.mark = ","))),
                  hjust = 0.5,
                  vjust = 2.5,
                  family = "patua-one",
                  size = 6,
                  color = "white")+
        theme_fira()+
        theme(plot.caption =  element_text(size = 13,color = "black", face = "bold", family = "montserrat"),
              plot.title = element_markdown(size = 33,color = "black", face = "bold", family = "patua-one",hjust = -0.01),
              axis.text.x = element_text(family = "montserrat",size = 12, face = "bold"),
              axis.text.y = element_text(family = "montserrat", size = 12, face = "bold"),
              plot.background = element_rect(fill = "white"),
              plot.subtitle = element_text(size = 22,color = "black", face = "bold", family = "patua-one",hjust = -0.01))+
        labs(title = "Víctimas de <span style = 'color:#F9494C'>homicidio</span> en México",
             caption = "Fuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública | Visualización: Miguel HG (@mike_dvz)",
             x = "",
             y = "",
             subtitle = "Marzo de cada año")+
        scale_y_continuous(labels = comma,expand = c(0,0))


ggsave(plot = last_plot(), "marzo.png",
       device = "png",
       height = 1080,
       width = 1920,
       units = "px",
       dpi = 190)
        
 
niveles <- c("Diciembre", "Noviembre", "Octubre", "Septiembre", "Agosto", "Julio","Junio", "Mayo", "Abril", "Marzo", "Febrero", "Enero") 

       
 marzo %>%
         group_by(Año,mes)%>%
         summarise(totales = sum(homicidios)) %>%
         ggplot(aes( x = factor(Año), y = factor(mes, levels = niveles), fill = totales))+
         geom_tile(color = "white", size = 1)+
         scale_fill_viridis(labels=function(x) format(x, big.mark = ",", scientific = FALSE), option  = "inferno")+
         theme_minimal_hgrid()+
         theme(legend.position = "bottom")+
         theme(legend.key.height= unit(0.6, 'cm'),
               legend.key.width= unit(1.3, 'cm'),
               legend.title = element_text(size=12))+
         labs(x = "",
              y = "",
              fill = "Homicidios",
              title = "Víctimas de homicidios por mes desde el 2015",
              subtitle = "Todas las modalidades",
              caption = "Fuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública | Visualización: Miguel HG (@mike_dvz)")+
         theme(plot.background = element_rect(fill = "white", color = NA),
               axis.text = element_text(family = "montserrat", size = 12, face = "bold"),
               legend.text = element_text(family = "montserrat",face = "bold", size = 12),
               legend.title = element_text(family = "montserrat", size = 15, face = "bold"),
               plot.title = element_text(family = "patua-one", size = 30, face = "bold"),
               plot.subtitle = element_text(family = "patua-one", size = 17),
               plot.caption = element_text(family = "patua-one", size = 11, face = "bold"))+
         theme(strip.text = element_text(family = "patua-one", face = "bold",size = 13))+
         geom_text(aes(label = totales), color = "white", size = 5)+
         annotate("text", x = as.factor(2020), y = "Marzo", size = 5,
                  lineheight = .9, color = "black", label = "4003")+
         annotate("text", x = as.factor(2021), y = "Mayo", size = 5,
                  lineheight = .9, color = "black", label = "4033")+
         annotate("text", x = as.factor(2018), y = "Mayo", size = 5,
                  lineheight = .9, color = "black", label = "3937")+
         annotate("text", x = as.factor(2019), y = "Mayo", size = 5,
                  lineheight = .9, color = "black", label = "3922")+
         annotate("text", x = as.factor(2018), y = "Diciembre", size = 5,
                  lineheight = .9, color = "black", label = "3943")

 
 
 ggsave(plot = last_plot(), "heatmap.png",
        device = "png",
        height = 1080,
        width = 1920,
        units = "px",
        dpi = 190) 
         
     
      
       
femi <- estatal %>%
  filter(Tipo.de.delito == "Feminicidio") %>%
  select(-Clave_Ent,-Bien.jurídico.afectado,-Subtipo.de.delito,-Modalidad) %>%
  group_by(Entidad,Año) %>%
  pivot_longer(cols = Enero:Diciembre, names_to = "mes", values_to = "homicidios") %>%
  select(-Tipo.de.delito)



femi %>%
  group_by(Año) %>%
  summarise(total_femi = sum(homicidios, na.rm = T)) %>%
  ggplot(aes(x = factor(Año), y = total_femi))+
  geom_col(fill = "#5E4B8B")+
  geom_text(aes(label = str_c(prettyNum(round(total_femi),big.mark = ","))),
            hjust = 0.5,
            vjust = 2.5,
            family = "patua-one",
            size = 6,
            color = "white")+
  theme_fira()+
  theme(plot.caption =  element_text(size = 13,color = "black", face = "bold", family = "montserrat"),
        plot.title = element_markdown(size = 33,color = "black", face = "bold", family = "patua-one",hjust = -0.01),
        axis.text.x = element_text(family = "montserrat",size = 16, face = "bold"),
        axis.text.y = element_text(family = "montserrat", size = 16, face = "bold"),
        plot.background = element_rect(fill = "white"),
        plot.subtitle = element_text(size = 22,color = "black", face = "bold", family = "patua-one",hjust = -0.01))+
  labs(title = "Víctimas de <span style = 'color:#5E4B8B'>feminicidio</span> en México",
       caption = "Fuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública | Visualización: Miguel HG (@mike_dvz)",
       x = "",
       y = "",
       subtitle = "Del 2015 a marzo de 2022")+
  scale_y_continuous(labels = comma,expand = c(0,0), breaks = seq(0,990,300))


ggsave(plot = last_plot(), "femini.png",
       device = "png",
       height = 1080,
       width = 1920,
       units = "px",
       dpi = 190)




