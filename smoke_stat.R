rm(list=ls())

library(tidyverse)
library(dunn.test)
library(ggpubr)
vigor.data<-read_csv2("grass_smoke.csv")
vigor.data$tratamento<-as_factor(vigor.data$tratamento, levels=c("Controle",
                                                                 "5min",
                                                                 "10min",
                                                                 "15min",
                                                               "20min"))
vigor.data$experimento<-factor(vigor.data$experimento)
vigor.data$replica<-as.factor(vigor.data$replica)



#U. decumbens



  # usando cada placa como replica
  
  por_placa<-vigor.data %>%
    mutate(Vigor_massa=(massa_A+massa_R)*G,
           Vigor_comp=(comprimento_A+comprimento_R)*G)%>%
    group_by(Especie,Dia,Tratamento, Replica)%>%
    summarize(media.vigorm=mean(Vigor_massa, na.rm=T),
              media.vigorcm=mean(Vigor_comp, na.rm=T))%>%
    group_by(Especie,Dia)%>%
    summarize(normal.g=shapiro.test(media.vigorm)$p.value, 
              normal.cm=shapiro.test(media.vigorcm)$p.value,
              kw.g=kruskal.test(media.vigorm~Tratamento)$p.value,
              anova.cm=summary(aov(media.vigorcm~Tratamento))[[1]][[1,"Pr(>F)"]])
    
  
  #usando cada planta como replica
    #teste de normalidade
  
  vigor.data %>%
    mutate(Vigor_massa=(massa_A+massa_R)*G,
           Vigor_comp=(comprimento_A+comprimento_R)*G)%>%
    mutate(Vigorg_ln=log(Vigor_massa),
           Vigorc_ln=log(Vigor_comp))%>%
    group_by(Especie,Dia)%>%
    summarize(normal.g=shapiro.test(Vigor_massa)$p.value, 
              normal.lncm=shapiro.test(Vigorg_ln)$p.value,
              normal.lng=shapiro.test(Vigorc_ln)$p.value, 
              normal.cm=shapiro.test(Vigor_comp)$p.value,
              kw.g=kruskal.test(Vigor_massa~Tratamento)$p.value,
              kw.cm=kruskal.test(Vigor_comp~Tratamento)$p.value)
  
  
  #gráficos e testes par a par
  
  #boxplot
  Tres_dias.Kc<-vigor.data %>%
    mutate(Vigor_massa=(massa_A+massa_R)*G)%>%
    filter(Dia=="3d", Especie=="K. coriacea")%>%
    ggplot(aes(x=Tratamento,y=Vigor_massa))+
           geom_boxplot()
  
  #grafico de barras com medias
  Tres_dias.Kc<-vigor.data %>%
    mutate(Vigor_massa=(massa_A+massa_R)*G)%>%
    filter(Especie=="K. coriacea")%>%
    group_by(Tratamento)%>%
    summarize(media=mean(Vigor_massa, na.rm=T))%>%
    ggplot(aes(x=Tratamento,y=media))+
    geom_bar(stat="identity", position="dodge")
  
  
  #
  Tres_dias.Qp<-vigor.data %>%
    mutate(Vigor_massa=(massa_A+massa_R)*G)%>%
    filter(Dia=="3d", Especie=="Q. grandiflora")%>%
    ggplot(aes(x=Tratamento,y=Vigor_massa))+
    geom_boxplot()
  
  Tres_dias.Dm<-vigor.data %>%
    mutate(Vigor_massa=(massa_A+massa_R)*G)%>%
    filter(Dia=="3d", Especie=="D. miscolobium")%>%
    ggplot(aes(x=Tratamento,y=Vigor_massa))+
    geom_boxplot()
  
  Tres_dias.plot
  
  #Gráfico três espécies juntas para poster da Mari
  
  Dm<-vigor.data %>%
    mutate(Vigor_massa=(massa_A+massa_R)*G)%>%
    filter(Dia=="3d", Especie=="D. miscolobium")%>%
    ggplot(aes(x=Tratamento,y=Vigor_massa))+
    geom_boxplot()+
    scale_y_continuous(name=expression(paste("Indice"~"de"~"vigor"~"(g)")),
                       limits = c(0,0.12)) +
    theme_bw()+
    theme(text=element_text(family="Times New Roman",
                            face="bold", size=16), 
          axis.title.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          axis.ticks.length=unit(-0.15, "cm"),
          axis.text.x = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")),
          axis.text.y = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")),
          strip.text=element_text(size=12,face="italic"))+
    guides(fill=FALSE
    )
  
  Kc<-vigor.data %>%
    mutate(Vigor_massa=(massa_A+massa_R)*G)%>%
    filter(Dia=="3d", Especie=="K. coriacea")%>%
    ggplot(aes(x=Tratamento,y=Vigor_massa))+
    geom_boxplot()+
    scale_y_continuous(name=expression(paste("Indice"~"de"~"vigor"~"(g)")),
                       limits = c(0,0.12)) +
    theme_bw()+
    theme(text=element_text(family="Times New Roman",
                            face="bold", size=16), 
          axis.title.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          axis.ticks.length=unit(-0.15, "cm"),
          axis.text.x = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")),
          axis.text.y = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")),
          strip.text=element_text(size=12,face="italic"))+
    guides(fill=FALSE
    )
  
  Qp<-vigor.data %>%
    mutate(Vigor_massa=(massa_A+massa_R)*G)%>%
    filter(Dia=="3d", Especie=="Q. parvifora")%>%
    ggplot(aes(x=Tratamento,y=Vigor_massa))+
    geom_boxplot()+
    scale_y_continuous(name=expression(paste("Indice"~"de"~"vigor"~"(g)")),
                       limits = c(0,0.12)) +
    theme_bw()+
    theme(text=element_text(family="Times New Roman",
                            face="bold", size=16), 
          axis.title.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          axis.ticks.length=unit(-0.15, "cm"),
          axis.text.x = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")),
          axis.text.y = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")),
          strip.text=element_text(size=12,face="italic"))+
    guides(fill=FALSE
    )
  
  
  group.vigor<-ggarrange(Dm, Kc, Qp,
                        labels = c("A", "B", "C"),
                        ncol = 1, nrow = 3)
  
  group.vigor<-annotate_figure(group.vigor,
                              bottom = text_grob("Tratamemnto",
                                                 family="Times New Roman"
                                                 , size=16))
  
  ggsave("Mari_poster.jpeg", plot = last_plot(), device = NULL, path = NULL,
         scale = 1, width = 5, height = 10, units = c("in", "cm", "mm"),
         dpi = 300, limitsize = TRUE)
  
  
  
  #Grafico para mariana, um gráfico só
  
  plot.tudo<-vigor.data %>%
    mutate(Vigor_massa=(massa_A+massa_R)*G)%>%
    filter(Dia=="3d")%>%
    ggplot(aes(x=Tratamento,y=Vigor_massa))+
    geom_boxplot()+
    facet_grid(.~Especie)+
    scale_y_continuous(name=expression(paste("Indice"~"de"~"vigor"~"(g)")),
                       limits = c(0,0.12)) +
    theme_bw()+
    theme(text=element_text(family="Times New Roman",
                            face="bold", size=16), 
          axis.title.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          axis.ticks.length=unit(-0.15, "cm"),
          axis.text.x = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")),
          axis.text.y = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")),
          strip.text=element_text(size=12,face="italic"))+
    guides(fill=FALSE
    )
  
  ggsave("Mari_poster_grid.jpeg", plot = last_plot(), device = NULL, path = NULL,
         scale = 1, width = 12, height = 5, units = c("in", "cm", "mm"),
         dpi = 300, limitsize = TRUE)
  # boxplot das medias, com n=5
  
  Tres_dias.plot2<-vigor.data %>%
    mutate(Vigor_massa=(massa_A+massa_R)*G)%>%
    filter(Dia=="3d", Especie=="K. coriacea")%>%
    group_by(Especie, Tratamento, Dia, Replica)%>%
    summarize(MEDIA=mean(Vigor_massa, na.rm=T))%>%
    ggplot(aes(x=Tratamento,y=MEDIA, fill=Especie))+
    geom_boxplot()
  Tres_dias.plot2
  
  Tres_dias.data<-vigor.data %>%
    mutate(Vigor_massa=(massa_A+massa_R)*G)%>%
    filter(Dia=="3d", Especie=="K. coriacea") 
  
attach(Tres_dias.data)
  capture.output(dunn.test(Vigor_massa, Tratamento) ,file="Kc_3d")
  detach(Tres_dias.data)

  
  Sete_dias.plot<-vigor.data %>%
    mutate(Vigor_massa=(massa_A+massa_R)*G)%>%
    filter(Dia=="7d", Especie=="K. coriacea")%>%
    ggplot(aes(x=Tratamento,y=Vigor_massa))+
    geom_boxplot()
  Sete_dias.plot
  
Sete_dias.data<-vigor.data %>%
    mutate(Vigor_massa=(massa_A+massa_R)*G)%>%
    filter(Dia=="7d", Especie=="K. coriacea") 
  
  attach(Sete_dias.data)
  capture.output(dunn.test(Vigor_massa, Tratamento) ,file="Kc_7d")
  detach(Sete_dias.data)
  
  
  Quinze_dias.plot<-vigor.data %>%
    mutate(Vigor_massa=(massa_A+massa_R)*G)%>%
    filter(Dia=="15d", Especie=="K. coriacea")%>%
    ggplot(aes(x=Tratamento,y=Vigor_massa))+
    geom_boxplot()
Quinze_dias.plot  

Quinze_dias.data<-vigor.data %>%
  mutate(Vigor_massa=(massa_A+massa_R)*G)%>%
  filter(Dia=="15d", Especie=="K. coriacea") 

attach(Quinze_dias.data)
capture.output(dunn.test(Vigor_massa, Tratamento) ,file="Kc_15d")
detach(Quinze_dias.data)

  