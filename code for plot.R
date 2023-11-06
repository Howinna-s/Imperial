library(tidyverse);library(terra);library(purrr);library(ggplot2);library(gridExtra);library(ggthemes)

setwd("D:/Climate data/data")

### 0 LOad the data ####
gdhy = rast(list.files("gdhy",full.names = T))
pc = rast(list.files("pc",full.names = T))

Qair = map(1:35,function(i){
  mean(rast(list.files("Qair",full.names = T)[i]),na.rm=T)
})
Qair = do.call("c",Qair)

PSurf = map(1:35,function(i){
  mean(rast(list.files("PSurf",full.names = T)[i]),na.rm=T)
})
PSurf = do.call("c",PSurf)

PPFD =  map(1:35,function(i){
  mean(rast(list.files("PPFD",full.names = T)[i]),na.rm=T)
})
PPFD = do.call("c",PPFD)

Tair = map(1:35,function(i){
  mean(rast(list.files("Tair",full.names = T)[i]),na.rm=T)
})
Tair = do.call("c",Tair)
Tair = Tair -273.15

CO2 = read.csv("CO2.csv")
CO2 = is.finite(Tair)*CO2$ppm
CO2[CO2==0] = NA

year = is.finite(Tair)*(1982:2016)
year[year==0] = NA

# mask the data with new extent
Qair = crop(Qair,gdhy)
Tair = crop(Tair,gdhy)
CO2 = crop(CO2,gdhy)
year = crop(year,gdhy)
PPFD = crop(PPFD,gdhy)
PSurf = crop(PSurf,gdhy)
## select the date in growning season
sample = (Tair >= 5)
sample[ isFALSE(sample) ] =NA

gdhy = gdhy #* sample
pc   = pc   #* sample
Qair = Qair * sample
Tair = Tair * sample
CO2  = CO2  * sample
year = year * sample


### 1 code for digure 1 ####
##left one
mean_values_gdhy <-global(gdhy, fun=mean,na.rm=T)
mean_values_pc <-global(pc, fun=mean,na.rm=T)

df <- data.frame(
  year = 1982:2016,
  gdhy = mean_values_gdhy,
  pc = mean_values_pc
)

df <- tidyr::gather(df, key = "raster", value = "mean_value", -year)
df$raster[df$raster == "mean"] = "gdhy"
df$raster[df$raster == "mean.1"] = "pc"

p1 = ggplot(df, aes(x = year, y = mean_value, color = raster)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Yield (t/ha)") +
  theme_bw() +
  guides(color = guide_legend(title = "")) +
  ylim(0,6)  +
  scale_y_continuous(breaks = seq(0, 6, by = 0.5))+
  scale_x_continuous(breaks = seq(1982, 2016, by = 1))+
  theme(legend.position=c(0.01,0.99), legend.justification=c(0,1),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(
          family = "serif",
          face = "bold",
          size = 14,
          margin = margin(t = 20, unit = "pt")
        ),
        axis.title.y = element_text(
          family = "serif",
          face = "bold",
          size = 14,
          margin = margin(l = 20, unit = "pt")
        ),
        plot.margin = margin(0.8, 0.5, 0.8, 0, "cm"),
        axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),
                                   size = 11,
                                   family = "serif"),
        axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10),
                                   size = 14,
                                   family = "serif"),
        text = element_text(family = "serif"),
        legend.text = element_text(family = "serif",size = 12)#,
        #panel.grid.major.x = element_blank()
  )



ggsave("D:/Climate data/Finla_figure/average_yield.png", width = 8, height = 6, dpi = 300)
##right one
df_gdhy <- na.omit(as.data.frame(values(gdhy)))
df_pc <- na.omit(as.data.frame(values(pc)))

colnames(df_gdhy) <- 1982:2016
colnames(df_pc) <- 1982:2016

df_gdhy$raster = "gdhy"
df_pc$raster = "pc"

df <- rbind(df_gdhy, df_pc)
df <- pivot_longer(df,cols = -raster ,names_to = "year",values_to = "yield")
df <- df %>% 
  group_by(raster,year) %>% 
  summarise(  y10 = quantile(yield, 0.1),
              y25 = quantile(yield, 0.25),
              y50 = median(yield),
              y75 = quantile(yield, 0.75),
              y90 = quantile(yield, 0.9))
  
p2 = ggplot(df, aes(x=factor(year), fill=factor(raster))) + 
  geom_boxplot(aes(ymin = y10, lower = y25, middle = y50, upper = y75, ymax = y90),stat = "identity",alpha=0.3,na.rm = T) +
  #geom_errorbar(aes(ymin = y10, ymax = y90), width = 0.2) +
  theme_bw() +
  guides(fill = guide_legend(title = "")) +
  theme(legend.position=c(0.01,0.99), legend.justification=c(0,1),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(
          family = "serif",
          face = "bold",
          size = 14,
          margin = margin(t = 20, unit = "pt")
        ),
        axis.title.y = element_text(
          family = "serif",
          face = "bold",
          size = 14,
          margin = margin(l = 20, unit = "pt")
        ),
        plot.margin = margin(0.8, 0.5, 0.8, 0, "cm"),
        axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),
                                   size = 11,
                                   family = "serif"),
        axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10),
                                   size = 14,
                                   family = "serif"),
        text = element_text(family = "serif"),
        legend.text = element_text(family = "serif",size = 12)
  ) +
  scale_fill_brewer(palette="Dark2")  + 
  xlab("Year") +
  ylab("Yield (t/ha)") +
  #labs(fill = "Land Use Type") +
  scale_y_continuous(limits = c(0 , 15),
                     breaks = seq(0, 15, by = 1), 
                     labels = seq(0, 15, by = 1),
                     expand = c(0,0))

p3 = p2+ 
  geom_rect(aes(xmin = 1.5, xmax = 2.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 3.5, xmax = 4.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 5.5, xmax = 6.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 9.5, xmax = 10.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 11.5, xmax = 12.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 13.5, xmax = 14.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 15.5, xmax = 16.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 17.5, xmax = 18.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 19.5, xmax = 20.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 21.5, xmax = 22.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 23.5, xmax = 24.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 25.5, xmax = 26.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 27.5, xmax = 28.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 29.5, xmax = 30.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 31.5, xmax = 32.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 33.5, xmax = 34.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) 
ggsave("D:/Climate data/Finla_figure/average_yield_chart.png", width = 12, height = 6, dpi = 300)
p3
##combine
grid.arrange(p1, p3, ncol = 2)

### 2 code for figure2 ####
install.packages("psych")
library(psych)

gdhy_sr <- sds(gdhy,Qair,Tair,CO2,year,PPFD,PSurf) 
pc_sr <- sds(pc,Qair,Tair,CO2,year,PPFD,PSurf) 

#"gdhy~Qair"
pcor1_result <-lapp(gdhy_sr,\(y,x,z1,z2,z3) {
  out <- rep(NA, nrow(y))
  for (i in 1:nrow(y)) {
    if(is.na(y[i,1]) | is.na(x[i,1])| is.na(z1[i,1])| is.na(z2[i,1])| is.na(z3[i,1]) ){
      out[i] = NA
    }else{
      #cat(i,"\n")
      data <- data.frame(as.vector(unlist(y[i,])), 
                    as.vector(unlist(x[i,])), 
                    as.vector(unlist(z1[i,])), 
                    as.vector(unlist(z2[i,])),
                    as.vector(unlist(z3[i,])))
      out[i] <- tryCatch({
        partial.r(data,c(1,2),c(3.4,5))[2,1]
      }, error = function(err) {
        NA
      })
    }
  }
  out
})

#"gdhy~Tair"
pcor2_result <-lapp(gdhy_sr,\(y,z1,x,z2,z3) {
  out <- rep(NA, nrow(y))
  for (i in 1:nrow(y)) {
    if(is.na(y[i,1]) | is.na(x[i,1])| is.na(z1[i,1])| is.na(z2[i,1])| is.na(z3[i,1]) ){
      out[i] = NA
    }else{
     #cat(i,"\n")
      data <- data.frame(as.vector(unlist(y[i,])), 
                         as.vector(unlist(x[i,])), 
                         as.vector(unlist(z1[i,])), 
                         as.vector(unlist(z2[i,])),
                         as.vector(unlist(z3[i,])))
      out[i] <- tryCatch({
        partial.r(data,c(1,2),c(3.4,5))[2,1]
      }, error = function(err) {
        NA
      })
    }
  }
  out
})

#"gdhy~CO2"
pcor3_result <-lapp(gdhy_sr,\(y,z1,z2,x,z3) {
  out <- rep(NA, nrow(y))
  for (i in 1:nrow(y)) {
    if(is.na(y[i,1]) | is.na(x[i,1])| is.na(z1[i,1])| is.na(z2[i,1])| is.na(z3[i,1]) ){
      out[i] = NA
    }else{
      #cat(i,"\n")
      data <- data.frame(as.vector(unlist(y[i,])), 
                         as.vector(unlist(x[i,])), 
                         as.vector(unlist(z1[i,])), 
                         as.vector(unlist(z2[i,])),
                         as.vector(unlist(z3[i,])))
      out[i] <- tryCatch({
        partial.r(data,c(1,2),c(3.4,5))[2,1]
      }, error = function(err) {
        NA
      })
    }
  }
  out
})

#"gdhy~year"
pcor4_result <-lapp(gdhy_sr,\(y,z1,z2,z3,x) {
  out <- rep(NA, nrow(y))
  for (i in 1:nrow(y)) {
    if(is.na(y[i,1]) | is.na(x[i,1])| is.na(z1[i,1])| is.na(z2[i,1])| is.na(z3[i,1]) ){
      out[i] = NA
    }else{
      #cat(i,"\n")
      data <- data.frame(as.vector(unlist(y[i,])), 
                         as.vector(unlist(x[i,])), 
                         as.vector(unlist(z1[i,])), 
                         as.vector(unlist(z2[i,])),
                         as.vector(unlist(z3[i,])))
      out[i] <- tryCatch({
        partial.r(data,c(1,2),c(3.4,5))[2,1]
      }, error = function(err) {
        NA
      })
    }
  }
  out
})

#"pc~Qair"
pcor5_result <-lapp(pc_sr,\(y,x,z1,z2,z3) {
  out <- rep(NA, nrow(y))
  for (i in 1:nrow(y)) {
    if(is.na(y[i,1]) | is.na(x[i,1])| is.na(z1[i,1])| is.na(z2[i,1])| is.na(z3[i,1]) ){
      out[i] = NA
    }else{
      #cat(i,"\n")
      data <- data.frame(as.vector(unlist(y[i,])), 
                         as.vector(unlist(x[i,])), 
                         as.vector(unlist(z1[i,])), 
                         as.vector(unlist(z2[i,])),
                         as.vector(unlist(z3[i,])))
      out[i] <- tryCatch({
        partial.r(data,c(1,2),c(3.4,5))[2,1]
      }, error = function(err) {
        NA
      })
    }
  }
  out
})

#"pc~Tair"
pcor6_result <-lapp(pc_sr,\(y,z1,x,z2,z3) {
  out <- rep(NA, nrow(y))
  for (i in 1:nrow(y)) {
    if(is.na(y[i,1]) | is.na(x[i,1])| is.na(z1[i,1])| is.na(z2[i,1])| is.na(z3[i,1]) ){
      out[i] = NA
    }else{
      #cat(i,"\n")
      data <- data.frame(as.vector(unlist(y[i,])), 
                         as.vector(unlist(x[i,])), 
                         as.vector(unlist(z1[i,])), 
                         as.vector(unlist(z2[i,])),
                         as.vector(unlist(z3[i,])))
      out[i] <- tryCatch({
        partial.r(data,c(1,2),c(3.4,5))[2,1]
      }, error = function(err) {
        NA
      })
    }
  }
  out
})

#"pc~CO2"
pcor7_result <-lapp(pc_sr,\(y,z1,z2,x,z3) {
  out <- rep(NA, nrow(y))
  for (i in 1:nrow(y)) {
    if(is.na(y[i,1]) | is.na(x[i,1])| is.na(z1[i,1])| is.na(z2[i,1])| is.na(z3[i,1]) ){
      out[i] = NA
    }else{
      #cat(i,"\n")
      data <- data.frame(as.vector(unlist(y[i,])), 
                         as.vector(unlist(x[i,])), 
                         as.vector(unlist(z1[i,])), 
                         as.vector(unlist(z2[i,])),
                         as.vector(unlist(z3[i,])))
      out[i] <- tryCatch({
        partial.r(data,c(1,2),c(3.4,5))[2,1]
      }, error = function(err) {
        NA
      })
    }
  }
  out
})

#"pc~year"
pcor8_result <-lapp(pc_sr,\(y,z1,z2,z3,x) {
  out <- rep(NA, nrow(y))
  for (i in 1:nrow(y)) {
    if(is.na(y[i,1]) | is.na(x[i,1])| is.na(z1[i,1])| is.na(z2[i,1])| is.na(z3[i,1]) ){
      out[i] = NA
    }else{
      #cat(i,"\n")
      data <- data.frame(as.vector(unlist(y[i,])), 
                         as.vector(unlist(x[i,])), 
                         as.vector(unlist(z1[i,])), 
                         as.vector(unlist(z2[i,])),
                         as.vector(unlist(z3[i,])))
      out[i] <- tryCatch({
        partial.r(data,c(1,2),c(3.4,5))[2,1]
      }, error = function(err) {
        NA
      })
    }
  }
  out
})

#"gdhy~PPFD"
pcor9_result <-lapp(gdhy_sr,\(y,z1,z2,z3,z4,x,z5) {
  out <- rep(NA, nrow(y))
  for (i in 1:nrow(y)) {
    if(is.na(y[i,1]) | is.na(x[i,1])| is.na(z1[i,1])| is.na(z2[i,1])| is.na(z3[i,1])| is.na(z4[i,1])| is.na(z5[i,1]) ){
      out[i] = NA
    }else{
      #cat(i,"\n")
      data <- data.frame(as.vector(unlist(y[i,])), 
                         as.vector(unlist(x[i,])), 
                         as.vector(unlist(z1[i,])), 
                         as.vector(unlist(z2[i,])),
                         as.vector(unlist(z3[i,])),
                         as.vector(unlist(z4[i,])),
                         as.vector(unlist(z5[i,]))
                         )
      out[i] <- tryCatch({
        partial.r(data,c(1,2),c(3.4,5))[2,1]
      }, error = function(err) {
        NA
      })
    }
  }
  out
})


#plot the pearson correlation figure
library(tidyterra)
library(cetcolor)

WorldSHP=terra::vect(spData::world)

#layout(matrix(c(1, 2, 3, 4, 5, 6,7,7), nrow = 4, ncol = 2, byrow = TRUE))
layout(matrix(c(1, 2, 3, 4, 5, 6,7,8), nrow = 4, ncol = 2, byrow = TRUE))
plot(pcor1_result, 
     col=cet_pal(20, name = "d1") , 
     fun=function(){plot(WorldSHP, add=TRUE)},
     main = list("gdhy~Qair", line = 2.5, cex = 1.5),
     breaks = seq(-1,1,by=0.1), 
     legend = F,
     type= "continuous",
     loc.main = "bottomright"
)
plot(pcor5_result, 
     col=cet_pal(20, name = "d1") , 
     fun=function(){plot(WorldSHP, add=TRUE)},
     main = list("pc~Qair", line = 2.5, cex = 1.5),
     breaks = seq(-1,1,by=0.1), 
     legend = F,
     type= "continuous",
     loc.main = "bottomright"
)
plot(pcor2_result, 
     col=cet_pal(20, name = "d1") , 
     fun=function(){plot(WorldSHP, add=TRUE)},
     main = list("gdhy~Tair", line = 2.5, cex = 1.5),
     breaks = seq(-1,1,by=0.1), 
     legend = F,
     type= "continuous",
     loc.main = "bottomright"
)
plot(pcor6_result, 
     col=cet_pal(20, name = "d1") , 
     fun=function(){plot(WorldSHP, add=TRUE)},
     main = list("pc~Tair", line = 2.5, cex = 1.5),
     breaks = seq(-1,1,by=0.1), 
     legend = F,
     type= "continuous",
     loc.main = "bottomright"
)
plot(pcor3_result, 
     col=cet_pal(20, name = "d1") , 
     fun=function(){plot(WorldSHP, add=TRUE)},
     main = list("gdhy~CO2", line = 2.5, cex = 1.5),
     breaks = seq(-1,1,by=0.1), 
     legend = F,
     type= "continuous",
     loc.main = "bottomright"
)
plot(pcor7_result, 
     col=cet_pal(20, name = "d1") , 
     fun=function(){plot(WorldSHP, add=TRUE)},
     main = list("pc~CO2", line = 2.5, cex = 1.5),
     breaks = seq(-1,1,by=0.1), 
     legend = F,
     type= "continuous",
     loc.main = "bottomright"
)
plot(pcor4_result, 
      col=cet_pal(20, name = "d1") , 
      fun=function(){plot(WorldSHP, add=TRUE)},
      main = list("gdhy~year", line = 2.5, cex = 1.5),
      breaks = seq(-1,1,by=0.1), 
      legend = F,
      type= "continuous",
      loc.main = "bottomright"
)
plot(pcor8_result, 
     col=cet_pal(20, name = "d1") , 
     fun=function(){plot(WorldSHP, add=TRUE)},
     main = list("pc~year", line = 2.5, cex = 1.5),
     breaks = seq(-1,1,by=0.1), 
     legend = F,
     type= "continuous",
     loc.main = "bottomright"
)

par(mfrow=c(1,1))
plot(pcor5_result, 
     col=cet_pal(20, name = "d1") , 
     fun=function(){plot(WorldSHP, add=TRUE)},
     main = list("pc~Qair", line = 2.5, cex = 1.5),
     breaks = seq(-1,1,by=0.1), 
     legend = "bottom",
     type= "continuous",
     loc.main = "bottomright",
     axes=F, plg=list(x="bottom", cex=1.2,at = seq(-1,1,by=0.1))
)

### 3 code for figure 3 ####
#gdhy_1982 = gdhy[[1:5]] #* sample
#gdhy_2016 = gdhy[[31:35]] #* sample
pc_1982 = rast("D:/Climate data/data/week data/pc_yield_1982_week.tif") #* sample
pc_2016 = rast("D:/Climate data/data/week data/pc_yield_1982_week_TP.tif") #* sample
pc_1982_TPCO2 = rast("D:/Climate data/data/week data/pc_yield_1982_week_TPCO2.tif")

pc_1982_2016 = rast("D:/Climate data/data/week data/pc_yield_1982_week_2016.tif")
pc_1982_P = rast("D:/Climate data/data/week data/pc_yield_1982_week_P.tif")
pc_1982_CO2 = rast("D:/Climate data/data/week data/pc_yield_1982_week_CO2.tif")
pc_1982_T = rast("D:/Climate data/data/week data/pc_yield_1982_week_T.tif")
#cv_gdhy_1982 = app(gdhy_1982,sd,na.rm=T)/app(gdhy_1982,mean,na.rm=T)
#cv_gdhy_2016 = app(gdhy_2016,sd,na.rm=T)/app(gdhy_2016,mean,na.rm=T)
cv_pc_1982 = app(pc_1982,sd,na.rm=T)/app(pc_1982,mean,na.rm=T)
cv_pc_2016 = app(pc_2016,sd,na.rm=T)/app(pc_2016,mean,na.rm=T)
cv_pc_1982_2016 = app(pc_1982_2016,sd,na.rm=T)/app(pc_1982_2016,mean,na.rm=T)
cv_pc_1982_P = app(pc_1982_P,sd,na.rm=T)/app(pc_1982_P,mean,na.rm=T)
cv_pc_1982_T = app(pc_1982_T,sd,na.rm=T)/app(pc_1982_T,mean,na.rm=T)
cv_pc_1982_TPCO2 = app(pc_1982_TPCO2,sd,na.rm=T)/app(pc_1982_TPCO2,mean,na.rm=T)
cv_pc_1982_CO2 = app(pc_1982_CO2,sd,na.rm=T)/app(pc_1982_CO2,mean,na.rm=T)
#speed1 <- (cv_gdhy_2016 - cv_gdhy_1982)/cv_gdhy_1982 *100
speed2016 <- (cv_pc_2016 - cv_pc_1982)/cv_pc_1982 *100
speed1982_TPCO2 <- (cv_pc_1982_TPCO2 - cv_pc_1982)/cv_pc_1982 *100
speed1982_2016 <- (cv_pc_1982_2016 - cv_pc_1982)/cv_pc_1982 *100
speed1982_p <- (cv_pc_1982_P - cv_pc_1982)/cv_pc_1982 *100
speed1982_T <- (cv_pc_1982_T - cv_pc_1982)/cv_pc_1982 *100
speed1982_CO2 <- (cv_pc_1982_CO2 - cv_pc_1982)/cv_pc_1982 *100

m <- c(-Inf, -10, 1,
       -10,  0, 2,
       0,10,3,
       10,Inf,4)   
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc1 <- classify(speed1982_CO2, rclmat, include.lowest=TRUE)
rc2 <- classify(speed1982_TPCO2, rclmat, include.lowest=TRUE)

reclassified1  = as.factor(rc1)
reclassified2  = as.factor(rc2)
reclassified2  = as.factor(speed2)
tar<-levels(reclassified1)[[1]]
tar[["sd"]]<-c("Limited","Marginal","Moderate","Optimal")
levels(reclassified1)<-tar

tar<-levels(reclassified2)[[1]]
tar[["sd"]]<-c("Limited","Marginal","Moderate","Optimal")
levels(reclassified2)<-tar

par(mfrow=c(1,1))
#plot(reclassified1, 
   #  col=cet_pal(4, name = "r2") , 
   #  fun=function(){plot(WorldSHP, add=TRUE)},
   #  main = list("gdhy", line = 2.5, cex = 1.5),
   #  legend = T,
   #  axes=F,
   #  plg=list( loc="bottomleft", cex=2),
   #  loc.main = "bottomright",
#)
plot(reclassified2, 
     col=cet_pal(4, name = "c2") , 
     fun=function(){plot(WorldSHP, add=TRUE)},
     main = list("pc", line = 2.5, cex = 3),
     axes=F,
     plg=list( loc="bottomleft", cex=1.5),
     loc.main = "bottomright",
     egend = "horizontal"
     
)
####
plot(reclassified2, 
     col=cet_pal(4, name = "i1") , 
     fun=function(){plot(WorldSHP, add=TRUE)},
     main = list("'TP'", line = 2.5, cex = 1.5),
     breaks = seq(-20,20,by=0.1), 
     legend = "center",
     type= "continuous",
     xlim = c(-130, 155),
     ylim = c(-55, 65),
     loc.main = "bottomright",
     axes=T,  
     plg = list(x = "top", cex = 1.2),
     labels = seq(-20, 20, by = 10),
     ylab="lat",
     xlab="lon"
)

####
writeRaster(reclassified1,"reclassified1.tif")
writeRaster(reclassified2,"reclassified2.tif")

# plot the box-figure
df_rc1 <- as.data.frame(values(reclassified1))
df_rc2 <- as.data.frame(values(reclassified2))

df_gdhy <- as.data.frame(values(gdhy)) #yearly data
df_pc <- as.data.frame(values(pc)) #yearly data

colnames(df_gdhy) <- 1982:2016
colnames(df_pc) <- 1982:2016

df_gdhy$raster = "CO2"
df_pc$raster = "TPCO2"
df_gdhy$rc = df_rc1$sd
df_pc$rc = df_rc2$sd

df <- na.omit(rbind(df_gdhy, df_pc))
df <- pivot_longer(df,cols = -c(raster,rc) ,names_to = "year",values_to = "yield")
df <- df[,-3] %>% 
  group_by(raster,rc) %>% 
  summarise(  y10 = quantile(yield, 0.1),
              y25 = quantile(yield, 0.25),
              y50 = median(yield),
              y75 = quantile(yield, 0.75),
              y90 = quantile(yield, 0.9))

df$rc = factor(df$rc)
levels(df$rc) = c("Limited","Marginal","Moderate","Optimal")

p2 = ggplot(df, aes(x=factor(rc), fill=factor(raster))) + 
  geom_boxplot(aes(ymin = y10, lower = y25, middle = y50, upper = y75, ymax = y90),stat = "identity",alpha=0.3,na.rm = T,width=0.5) +
  stat_boxplot(geom = "errorbar",aes(ymin = y10, ymax = y90), width = 0.35) +
  theme_bw() +
  guides(fill = guide_legend(title = "")) +
  theme(legend.position=c(0.01,0.99), legend.justification=c(0,1),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(
          family = "serif",
          face = "bold",
          size = 14,
          margin = margin(t = 20, unit = "pt")
        ),
        axis.title.y = element_text(
          family = "serif",
          face = "bold",
          size = 14,
          margin = margin(l = 20, unit = "pt")
        ),
        plot.margin = margin(0.8, 0.5, 0.8, 0, "cm"),
        axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),
                                   size = 14,
                                   family = "serif"),
        axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10),
                                   size = 14,
                                   family = "serif"),
        text = element_text(family = "serif"),
        legend.text = element_text(family = "serif",size = 12)
  ) +
  scale_fill_brewer(palette="Dark2")  + 
  xlab("Type") +
  ylab("Yield (t/ha)") +
  #labs(fill = "Land Use Type") +
  scale_y_continuous(limits = c(0 , 10),
                     breaks = seq(0, 10, by = 1), 
                     labels = seq(0, 10, by = 1),
                     expand = c(0,0))
p3 = p2+ 
  geom_rect(aes(xmin = 1.5, xmax = 2.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) +
  geom_rect(aes(xmin = 3.5, xmax = 4.5,ymin=-Inf,ymax=Inf),
            fill = "gray", alpha = 0.01) 
p2

## plot the figure with Robinson projection area
library(ggplot2)
library(tidyterra)
WorldSHP=terra::vect(spData::world)

mypal2 = cetcolor::cet_pal(20, name = "d1")  

plot_raster = speed2 # adjust the speed with y ！！！！！

RobinsonPlot <- ggplot() +
  geom_spatraster(data = pcor1_result)+                                 
  geom_spatvector(data = WorldSHP, 
                  fill = "transparent") +       
  ggtitle(" Yield CV Change in 1982-2016 with 'CO2'(%)") +              
  scale_fill_gradientn(colors=mypal2,           
                       name = "rate", 
                       na.value = "transparent",
                       lim=c(-1,
                             1),
                        guide = guide_colorbar(
                         barwidth = 20
                       ))+      
  theme_minimal()+                             
  theme(plot.title = element_text(hjust =0.5), 
        text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.position = "bottom")+     
  coord_sf(crs = "ESRI:54030",            
           xlim = c(-130,130)*100000,    
           ylim = c(-60,60)*100000)
print(RobinsonPlot)

# box-plot 2
m <- c(-Inf, 100, 1,
       100,  150, 2,
       200,Inf,3) 

rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc_Tair <- classify(app(Tair,mean), rclmat, include.lowest=TRUE)

df_Tair = as.data.frame(values(rc_Tair))

df_speed3 <- as.data.frame(values(speed1982_TPCO2))
df_speed2 <- as.data.frame(values(speed1982_CO2))
colnames(df_speed3) = "speed"
colnames(df_speed2) = "speed"

df_speed3$raster = "TPCO2"
df_speed2$raster = "CO2"

df_speed3$Tair = df_Tair$mean
df_speed2$Tair = df_Tair$mean

df <- na.omit(rbind(df_speed3, df_speed2))

df <- df %>% 
  group_by(raster,Tair) %>% 
  summarise(  y10 = quantile(speed, 0.1),
              y25 = quantile(speed, 0.25),
              y50 = median(speed),
              y75 = quantile(speed, 0.75),
              y90 = quantile(speed, 0.9))

df$Tair = factor(df$Tair)
levels(df$Tair) = c("<100","100-150",">150")

p2 = ggplot(df, aes(x=factor(Tair), fill=factor(raster))) + 
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  geom_boxplot(aes(ymin = y10, lower = y25, middle = y50, upper = y75, ymax = y90),stat = "identity",alpha=0.3,na.rm = T,width=0.5) +
  stat_boxplot(geom = "errorbar",aes(ymin = y10, ymax = y90), width = 0.35) +
  theme_bw() +
  guides(fill = guide_legend(title = "")) +
  theme(legend.position=c(0.01,0.99), legend.justification=c(0,1),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent", color = NA),
        axis.title.x = element_text(
          family = "serif",
          face = "bold",
          size = 14,
          margin = margin(t = 20, unit = "pt")
        ),
        axis.title.y = element_text(
          family = "serif",
          face = "bold",
          size = 14,
          margin = margin(l = 20, unit = "pt")
        ),
        plot.margin = margin(0.8, 0.5, 0.8, 0, "cm"),
        axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),
                                   size = 14,
                                   family = "serif"),
        axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10),
                                   size = 14,
                                   family = "serif"),
        text = element_text(family = "serif"),
        legend.text = element_text(family = "serif",size = 12),
        panel.grid = element_blank()
  ) +
  scale_fill_brewer(palette="Dark2")  + 
  xlab("Precipitation in baseline (mm)") +
  ylab("Yield CV change (%)") +
  #labs(fill = "Land Use Type") +
  scale_y_continuous(limits = c(-15 , 15),
                     breaks = seq(-15, 15, by = 5), 
                     labels = seq(-15, 15, by = 5),
                     expand = c(0,0)) 
p2

# Put together eight scatter fit plots ####
# ①_1 ####
speed_Tair = ( Tair[[35]] - Tair[[1]])/ Tair[[1]] * 100
#Filtering greater than 10
mean_Tair = mean(Tair,na.rm=T)
sample_mean_Tair = (mean_Tair > 10)
sample_mean_Tair[ isFALSE(sample_mean_Tair) ] =NA
speed_Tair = speed_Tair * sample_mean_Tair

df = na.omit( cbind(as.data.frame(values(speed2)),as.data.frame(values(speed_Tair))) )

#Removal of outliers
percentile_low <- apply(df, 2, quantile, probs = 0.01)
percentile_high <- apply(df, 2, quantile, probs = 0.99)

df_filtered <- df
for (col in names(df)) {
  df_filtered <- subset(df_filtered, df_filtered[, col] >= percentile_low[col] & df_filtered[, col] <= percentile_high[col])
}

colnames(df_filtered) = c("y","x")

lm_model <- lm(y ~ x, data = df_filtered )
r_squared <- summary(lm_model)$r.squared
p_value <- summary(lm_model)$coefficients[2, 4]
a = round(summary(lm_model)$coefficients[2,1],3)
b = round(summary(lm_model)$coefficients[1,1],2)
p_value

p1_1 =ggplot(df_filtered , aes(x = x, y = y)) +
  geom_point(color = "royalblue", alpha = 0.4) +
  labs(x = "Tair average mean change in hotter area(%)", y = "Yield CV change with 'TP'(%)") +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) + 
  geom_smooth(aes(group = 1),method = "lm", se = T, color = "blue")+
  annotate(
    "text", x = 15, y = 50, label = paste0("y = ",a,"x+",b,"\n (p < 0.001) "), 
    hjust = 1, vjust = 1, size = 5
  ) + 
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  geom_vline(xintercept = 0,linetype = "dashed")+
  theme_bw() +
  theme(
    axis.title.x = element_text(
      family = "serif",
      face = "bold",
      size = 14,
      margin = margin(t = 20, unit = "pt")
    ),
    axis.title.y = element_text(
      family = "serif",
      face = "bold",
      size = 14,
      margin = margin(l = 20, unit = "pt")
    ),
    plot.margin = margin(0.8, 0.5, 0.8, 0, "cm"),
    axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),
                               size = 11,
                               family = "serif"),
    axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10),
                               size = 14,
                               family = "serif"),
    text = element_text(family = "serif"),
    legend.text = element_text(family = "serif",size = 12),
    panel.grid = element_blank()
  )
p1_1

# ①_2 ####
speed_Tair = ( Tair[[35]] - Tair[[1]])/ Tair[[1]] * 100
#Filtering greater than 10
mean_Tair = mean(Tair,na.rm=T)
sample_mean_Tair = (mean_Tair > 10)
sample_mean_Tair[ isFALSE(sample_mean_Tair) ] =NA
speed_Tair = speed_Tair * sample_mean_Tair

df = na.omit( cbind(as.data.frame(values(speed4)),as.data.frame(values(speed_Tair))) )

#Removal of outliers
percentile_low <- apply(df, 2, quantile, probs = 0.01)
percentile_high <- apply(df, 2, quantile, probs = 0.99)

df_filtered <- df
for (col in names(df)) {
  df_filtered <- subset(df_filtered, df_filtered[, col] >= percentile_low[col] & df_filtered[, col] <= percentile_high[col])
}

colnames(df_filtered) = c("y","x")

lm_model <- lm(y ~ x, data = df_filtered )
r_squared <- summary(lm_model)$r.squared
p_value <- summary(lm_model)$coefficients[2, 4]
a = round(summary(lm_model)$coefficients[2,1],3)
b = round(summary(lm_model)$coefficients[1,1],2)
p_value

p1_2 =ggplot(df_filtered , aes(x = x, y = y)) +
  geom_point(color = "royalblue", alpha = 0.4) +
  labs(x = "Tair average mean change in hotter area(%)", y = "Yield CV change 1982-2016(%)") +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) + 
  geom_smooth(aes(group = 1),method = "lm", se = T, color = "blue")+
  annotate(
    "text", x = 20, y = 70, label = paste0("y = ",a,"x+",b,"\n (p < 0.001) "), 
    hjust = 1, vjust = 1, size = 5
  ) + 
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  geom_vline(xintercept = 0,linetype = "dashed")+
  theme_bw() +
  theme(
    axis.title.x = element_text(
      family = "serif",
      face = "bold",
      size = 14,
      margin = margin(t = 20, unit = "pt")
    ),
    axis.title.y = element_text(
      family = "serif",
      face = "bold",
      size = 14,
      margin = margin(l = 20, unit = "pt")
    ),
    plot.margin = margin(0.8, 0.5, 0.8, 0, "cm"),
    axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),
                               size = 11,
                               family = "serif"),
    axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10),
                               size = 14,
                               family = "serif"),
    text = element_text(family = "serif"),
    legend.text = element_text(family = "serif",size = 12),
    panel.grid = element_blank()
  )
p1_2

# ②_1 ####
speed_Tair = ( Tair[[35]] - Tair[[1]])/ Tair[[1]] * 100
#Filtering lower than 10
mean_Tair = mean(Tair,na.rm=T)
sample_mean_Tair = (mean_Tair < 10)
sample_mean_Tair[ isFALSE(sample_mean_Tair) ] =NA
speed_Tair = speed_Tair * sample_mean_Tair

df = na.omit( cbind(as.data.frame(values(speed2)),as.data.frame(values(speed_Tair))) )

#Removal of outliers
percentile_low <- apply(df, 2, quantile, probs = 0.01)
percentile_high <- apply(df, 2, quantile, probs = 0.99)

df_filtered <- df
for (col in names(df)) {
  df_filtered <- subset(df_filtered, df_filtered[, col] >= percentile_low[col] & df_filtered[, col] <= percentile_high[col])
}

colnames(df_filtered) = c("y","x")

lm_model <- lm(y ~ x, data = df_filtered )
r_squared <- summary(lm_model)$r.squared
p_value <- summary(lm_model)$coefficients[2, 4]
a = round(summary(lm_model)$coefficients[2,1],3)
b = round(summary(lm_model)$coefficients[1,1],2)
p_value

p2_1 =ggplot(df_filtered , aes(x = x, y = y)) +
  geom_point(color = "royalblue", alpha = 0.4) +
  labs(x = "Tair average mean change in colder area(%)", y = "Yield CV change with 'TP'(%)") +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) + 
  geom_smooth(aes(group = 1),method = "lm", se = T, color = "blue")+
  annotate(
    "text", x = 15, y = 30, label = paste0("y = ",a,"x+",b,"\n (p < 0.001) "), 
    hjust = 1, vjust = 1, size = 5
  ) + 
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  geom_vline(xintercept = 0,linetype = "dashed")+
  theme_bw() +
  theme(
    axis.title.x = element_text(
      family = "serif",
      face = "bold",
      size = 14,
      margin = margin(t = 20, unit = "pt")
    ),
    axis.title.y = element_text(
      family = "serif",
      face = "bold",
      size = 14,
      margin = margin(l = 20, unit = "pt")
    ),
    plot.margin = margin(0.8, 0.5, 0.8, 0, "cm"),
    axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),
                               size = 11,
                               family = "serif"),
    axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10),
                               size = 14,
                               family = "serif"),
    text = element_text(family = "serif"),
    legend.text = element_text(family = "serif",size = 12),
    panel.grid = element_blank()
  )
p2_1

# ②_2 ####
speed_Tair = ( Tair[[35]] - Tair[[1]])/ Tair[[1]] * 100
#Filtering lower than 10
mean_Tair = mean(Tair,na.rm=T)
sample_mean_Tair = (mean_Tair < 10)
sample_mean_Tair[ isFALSE(sample_mean_Tair) ] =NA
speed_Tair = speed_Tair * sample_mean_Tair

df = na.omit( cbind(as.data.frame(values(speed4)),as.data.frame(values(speed_Tair))) )

#Removal of outliers
percentile_low <- apply(df, 2, quantile, probs = 0.01)
percentile_high <- apply(df, 2, quantile, probs = 0.99)

df_filtered <- df
for (col in names(df)) {
  df_filtered <- subset(df_filtered, df_filtered[, col] >= percentile_low[col] & df_filtered[, col] <= percentile_high[col])
}

colnames(df_filtered) = c("y","x")

lm_model <- lm(y ~ x, data = df_filtered )
r_squared <- summary(lm_model)$r.squared
p_value <- summary(lm_model)$coefficients[2, 4]
a = round(summary(lm_model)$coefficients[2,1],3)
b = round(summary(lm_model)$coefficients[1,1],2)
p_value

p2_2 =ggplot(df_filtered , aes(x = x, y = y)) +
  geom_point(color = "royalblue", alpha = 0.4) +
  labs(x = "Tair average mean change in colder area(%)", y = "Yield CV change 1982-2016(%)") +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) + 
  geom_smooth(aes(group = 1),method = "lm", se = T, color = "blue")+
  annotate(
    "text", x = 45, y = 50, label = paste0("y = ",a,"x+",b,"\n (p < 0.001) "), 
    hjust = 1, vjust = 1, size = 5
  ) + 
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  geom_vline(xintercept = 0,linetype = "dashed")+
  theme_bw() +
  theme(
    axis.title.x = element_text(
      family = "serif",
      face = "bold",
      size = 14,
      margin = margin(t = 20, unit = "pt")
    ),
    axis.title.y = element_text(
      family = "serif",
      face = "bold",
      size = 14,
      margin = margin(l = 20, unit = "pt")
    ),
    plot.margin = margin(0.8, 0.5, 0.8, 0, "cm"),
    axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),
                               size = 11,
                               family = "serif"),
    axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10),
                               size = 14,
                               family = "serif"),
    text = element_text(family = "serif"),
    legend.text = element_text(family = "serif",size = 12),
    panel.grid = element_blank()
  )
p2_2

# ③ _1 ####
speed_Tair = ( Tair[[35]] - Tair[[1]])/ Tair[[1]] * 100
df = na.omit( cbind(as.data.frame(values(speed2)),as.data.frame(values(speed_Tair))) )

#Removal of outliers
percentile_low <- apply(df, 2, quantile, probs = 0.01)
percentile_high <- apply(df, 2, quantile, probs = 0.99)

df_filtered <- df
for (col in names(df)) {
  df_filtered <- subset(df_filtered, df_filtered[, col] >= percentile_low[col] & df_filtered[, col] <= percentile_high[col])
}

colnames(df_filtered) = c("y","x")

lm_model <- lm(y ~ x, data = df_filtered )
r_squared <- summary(lm_model)$r.squared
p_value <- summary(lm_model)$coefficients[2, 4]
a = round(summary(lm_model)$coefficients[2,1],3)
b = round(summary(lm_model)$coefficients[1,1],2)
p_value

p3_1 =ggplot(df_filtered , aes(x = x, y = y)) +
  geom_point(color = "royalblue", alpha = 0.4) +
  labs(x = "Tair mean change (%)", y = "Yield CV change with 'TP' (%)") +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) + 
  geom_smooth(aes(group = 1),method = "lm", se = T, color = "blue")+
  annotate(
    "text", x = 15, y = 50, label = paste0("y = ",a,"x+",b,"\n (p < 0.001) "), 
    hjust = 1, vjust = 1, size = 5
  ) + 
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  geom_vline(xintercept = 0,linetype = "dashed")+
  theme_bw() +
  theme(
        axis.title.x = element_text(
          family = "serif",
          face = "bold",
          size = 14,
          margin = margin(t = 20, unit = "pt")
        ),
        axis.title.y = element_text(
          family = "serif",
          face = "bold",
          size = 14,
          margin = margin(l = 20, unit = "pt")
        ),
        plot.margin = margin(0.8, 0.5, 0.8, 0, "cm"),
        axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),
                                   size = 11,
                                   family = "serif"),
        axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10),
                                   size = 14,
                                   family = "serif"),
        text = element_text(family = "serif"),
        legend.text = element_text(family = "serif",size = 12),
        panel.grid = element_blank()
  )
p3_1

# ③ _2####
speed_Tair = ( Tair[[35]] - Tair[[1]])/ Tair[[1]] * 100
df = na.omit( cbind(as.data.frame(values(speed4)),as.data.frame(values(speed_Tair))) )

#Removal of outliers
percentile_low <- apply(df, 2, quantile, probs = 0.01)
percentile_high <- apply(df, 2, quantile, probs = 0.99)

df_filtered <- df
for (col in names(df)) {
  df_filtered <- subset(df_filtered, df_filtered[, col] >= percentile_low[col] & df_filtered[, col] <= percentile_high[col])
}

colnames(df_filtered) = c("y","x")

lm_model <- lm(y ~ x, data = df_filtered )
r_squared <- summary(lm_model)$r.squared
p_value <- summary(lm_model)$coefficients[2, 4]
a = round(summary(lm_model)$coefficients[2,1],3)
b = round(summary(lm_model)$coefficients[1,1],2)
p_value

p3_2 =ggplot(df_filtered , aes(x = x, y = y)) +
  geom_point(color = "royalblue", alpha = 0.4) +
  labs(x = "Tair mean change (%)", y = "Yield CV change 1982-2016 (%)") +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) + 
  geom_smooth(aes(group = 1),method = "lm", se = T, color = "blue")+
  annotate(
    "text", x = 40, y = 50, label = paste0("y = ",a,"x+",b,"\n (p = 0.01) "), 
    hjust = 1, vjust = 1, size = 5
  ) + 
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  geom_vline(xintercept = 0,linetype = "dashed")+
  theme_bw() +
  theme(
    axis.title.x = element_text(
      family = "serif",
      face = "bold",
      size = 14,
      margin = margin(t = 20, unit = "pt")
    ),
    axis.title.y = element_text(
      family = "serif",
      face = "bold",
      size = 14,
      margin = margin(l = 20, unit = "pt")
    ),
    plot.margin = margin(0.8, 0.5, 0.8, 0, "cm"),
    axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),
                               size = 11,
                               family = "serif"),
    axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10),
                               size = 14,
                               family = "serif"),
    text = element_text(family = "serif"),
    legend.text = element_text(family = "serif",size = 12),
    panel.grid = element_blank()
  )
p3_2

# ④_1 ####
speed_Qair = ( Qair[[35]] - Qair[[1]])/ Qair[[1]] * 100
df = na.omit( cbind(as.data.frame(values(speed3)),as.data.frame(values(speed_Qair))) )

#Removal of outliers
percentile_low <- apply(df, 2, quantile, probs = 0.01)
percentile_high <- apply(df, 2, quantile, probs = 0.99)

df_filtered <- df
for (col in names(df)) {
  df_filtered <- subset(df_filtered, df_filtered[, col] >= percentile_low[col] & df_filtered[, col] <= percentile_high[col])
}

colnames(df_filtered) = c("y","x")

lm_model <- lm(y ~ x, data = df_filtered )
r_squared <- summary(lm_model)$r.squared
p_value <- summary(lm_model)$coefficients[2, 4]
a = round(summary(lm_model)$coefficients[2,1],3)
b = round(summary(lm_model)$coefficients[1,1],2)
p_value

p4_1 =ggplot(df_filtered , aes(x = x, y = y)) +
  geom_point(color = "red", alpha = 0.4) +
  labs(x = "Precipitation Mean Change (%)", y = "Yield CV change with'TPCO2'(%)") +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) + 
  geom_smooth(aes(group = 1),method = "lm", se = T, color = "brown")+
  annotate(
    "text", x = 15, y = 50, label = paste0("y = ",a,"x+",b,"\n (p < 0.001) "), 
    hjust = 1, vjust = 1, size = 5
  ) + 
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  geom_vline(xintercept = 0,linetype = "dashed")+
  theme_bw() +
  theme(
    axis.title.x = element_text(
      family = "serif",
      face = "bold",
      size = 14,
      margin = margin(t = 20, unit = "pt")
    ),
    axis.title.y = element_text(
      family = "serif",
      face = "bold",
      size = 14,
      margin = margin(l = 20, unit = "pt")
    ),
    plot.margin = margin(0.8, 0.5, 0.8, 0, "cm"),
    axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),
                               size = 11,
                               family = "serif"),
    axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10),
                               size = 14,
                               family = "serif"),
    text = element_text(family = "serif"),
    legend.text = element_text(family = "serif",size = 12),
    panel.grid = element_blank()
  )
p4_1

# ④_2####
speed_Qair = ( Qair[[35]] - Qair[[1]])/ Qair[[1]] * 100
df = na.omit( cbind(as.data.frame(values(speed2)),as.data.frame(values(speed_Qair))) )

#Removal of outliers
percentile_low <- apply(df, 2, quantile, probs = 0.01)
percentile_high <- apply(df, 2, quantile, probs = 0.99)

df_filtered <- df
for (col in names(df)) {
  df_filtered <- subset(df_filtered, df_filtered[, col] >= percentile_low[col] & df_filtered[, col] <= percentile_high[col])
}

colnames(df_filtered) = c("y","x")

lm_model <- lm(y ~ x, data = df_filtered )
r_squared <- summary(lm_model)$r.squared
p_value <- summary(lm_model)$coefficients[2, 4]
a = round(summary(lm_model)$coefficients[2,1],3)
b = round(summary(lm_model)$coefficients[1,1],2)
p_value

p4_2 =ggplot(df_filtered , aes(x = x, y = y)) +
  geom_point(color = "royalblue", alpha = 0.4) +
  labs(x = "precipitation mean change (%)", y = "Yield CV change of pc (%)") +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) + 
  geom_smooth(aes(group = 1),method = "lm", se = T, color = "blue")+
  annotate(
    "text", x = 15, y = 75, label = paste0("y = ",a,"x+",b,"\n (p < 0.001) "), 
    hjust = 1, vjust = 1, size = 5
  ) + 
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  geom_vline(xintercept = 0,linetype = "dashed")+
  theme_bw() +
  theme(
    axis.title.x = element_text(
      family = "serif",
      face = "bold",
      size = 14,
      margin = margin(t = 20, unit = "pt")
    ),
    axis.title.y = element_text(
      family = "serif",
      face = "bold",
      size = 14,
      margin = margin(l = 20, unit = "pt")
    ),
    plot.margin = margin(0.8, 0.5, 0.8, 0, "cm"),
    axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),
                               size = 11,
                               family = "serif"),
    axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10),
                               size = 14,
                               family = "serif"),
    text = element_text(family = "serif"),
    legend.text = element_text(family = "serif",size = 12),
    panel.grid = element_blank()
  )
p4_2


grid.arrange(p1_1, p1_2,
             p2_1, p2_2,
             p3_1, p3_2,
             p4_1, p4_2,
             ncol = 2)

## calculate the production of wheat ####
## 1 Separately categorize the data of 4 tif files in "PC" and "PCG" files, 1991 and 2016, as the country data. Named PC1991, PC2016, PCG1991, PCG2016.The result output two kinds, one is with the country border on the map, the other is csv. #
PC1991 = rast("new/PC/pc_yield_1991.tif")
PC2016 = rast("new/PC/pc_yield_2016.tif")
PCG1991 = rast("new/PCG/pc_yield_1991_global.tif")
PCG2016 = rast("new/PCG/pc_yield_2016_global.tif")
Value2016 =rast("D:/Climate data/Potential Planting Area with Value.tif")
world_map =vect("new/world_map/World_Countries_Generalized.shp")

library(exactextractr)
world_map$PC1991 = exact_extract(raster::raster(PC1991),as_sf(world_map),"weighted_sum", weights = 'area') / 10000
world_map$PC2016 = exact_extract(raster::raster(PC2016),as_sf(world_map),"weighted_sum", weights = 'area') / 10000
world_map$PCG1991 = exact_extract(raster::raster(PCG1991),as_sf(world_map),"weighted_sum", weights = 'area') / 10000
world_map$PCG2016 = exact_extract(raster::raster(PCG2016),as_sf(world_map),"weighted_sum", weights = 'area') / 10000
world_map$Value2016 = exact_extract(raster::raster(Value2016),as_sf(world_map),"weighted_sum", weights = 'area') / 10000

values(world_map) [values(world_map) == 0] =NA
#output the production by nations
write.csv(values(world_map),"各国产量.csv",row.names = F)
n_colors <- 10
palette <- brewer.pal(n_colors, "Spectral")
RobinsonPlot1 <- ggplot(world_map) +
  geom_spatvector(aes(fill = Value2016/1e9)) +       
  ggtitle("National Wheat Yield in 1991 (billion tons)") +              
  scale_fill_gradientn(colors=viridis::viridis(n_colors),           
                       name = "Yield", 
                       na.value = "transparent",
                       breaks = seq(0, 3 , length.out = n_colors +1),
                       guide = guide_colorbar(
                         barwidth = 20
                       ))+      
  theme_minimal()+                             
  theme(plot.title = element_text(hjust =0.5), 
        text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.position = "bottom")+      
  coord_sf(crs = "ESRI:54030",
           xlim = c(-180,180)*100000,
             ylim = c(-60,90)*100000)
RobinsonPlot1
RobinsonPlot2 <- ggplot(world_map) +
  geom_spatvector(aes(fill = PC2016/1e9)) +       
  ggtitle("National Wheat Yield in 2016 (billion tons)") +              
  scale_fill_gradientn(colors=viridis::viridis(n_colors),           
                       name = "yield", 
                       na.value = "transparent",
                       breaks = seq(0, 3 , length.out = n_colors +1),
                       guide = guide_colorbar(
                         barwidth = 20
                       ))+      
  theme_minimal()+                             
  theme(plot.title = element_text(hjust =0.5), 
        text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.position = "bottom")+      
  coord_sf(crs = "ESRI:54030",
           xlim = c(-180,180)*100000,
           ylim = c(-60,90)*100000)
RobinsonPlot2
RobinsonPlot3 <- ggplot(world_map) +
  geom_spatvector(aes(fill = PCG1991/1e9)) +       
  ggtitle("Potential National production of PCG1991 (billion tons)") +              
  scale_fill_gradientn(colors=viridis::viridis(n_colors),           
                       name = "yield", 
                       na.value = "transparent",
                       #breaks = seq(0, 3 , length.out = n_colors +1),
                       guide = guide_colorbar(
                         barwidth = 20
                       ))+      
  theme_minimal()+                             
  theme(plot.title = element_text(hjust =0.5), 
        text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.position = "bottom")+      
  coord_sf(crs = "ESRI:54030",
           xlim = c(-180,180)*100000,
           ylim = c(-60,90)*100000)
RobinsonPlot3
RobinsonPlot4 <- ggplot(world_map) +
  geom_spatvector(aes(fill = PCG2016/1e9)) +       
  ggtitle("Potential National production of PCG2016 (billion tons)") +              
  scale_fill_gradientn(colors=viridis::viridis(n_colors),           
                       name = "yield", 
                       na.value = "transparent",
                      # breaks = seq(0, 3 , length.out = n_colors +1),
                       guide = guide_colorbar(
                         barwidth = 20
                       ))+      
  theme_minimal()+                             
  theme(plot.title = element_text(hjust =0.5), 
        text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.position = "bottom")+      
  coord_sf(crs = "ESRI:54030",
           xlim = c(-180,180)*100000,
           ylim = c(-60,90)*100000)
RobinsonPlot4
grid.arrange(RobinsonPlot1, RobinsonPlot3,
             RobinsonPlot2, RobinsonPlot4,
             ncol = 2)

## 2、Country-wide GDP YPC1991, YPC2016, YPCG1991, YPCG2016 (GDP = production * unit price for the year)##
world_map$PC1991_price = world_map$PC1991 * 111.980389897233
world_map$PC2016_price = world_map$PC2016 * 161.469904095238
world_map$PCG1991_price = world_map$PCG1991 * 111.980389897233
world_map$PCG2016_price = world_map$PCG2016 * 161.469904095238

PRICE1 <- ggplot(world_map) +
  geom_spatvector(aes(fill = PC1991_price/1e9)) +       
  ggtitle("National production price of PC1991 (billion US dollars)") +              
  scale_fill_gradientn(colors=mypal2,           
                       name = "value", 
                       na.value = "transparent",
                       guide = guide_colorbar(
                         barwidth = 20
                       ))+      
  theme_minimal()+                             
  theme(plot.title = element_text(hjust =0.5), 
        text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.position = "bottom")+      
  coord_sf(crs = "ESRI:54030",
           xlim = c(-180,180)*100000,
           ylim = c(-60,90)*100000)
PRICE1
PRICE2 <- ggplot(world_map) +
  geom_spatvector(aes(fill = PC2016_price/1e9)) +       
  ggtitle("National production price of PC2016 (billion US dollars)") +              
  scale_fill_gradientn(colors=mypal2,           
                       name = "value", 
                       na.value = "transparent",
                       guide = guide_colorbar(
                         barwidth = 20
                       ))+      
  theme_minimal()+                             
  theme(plot.title = element_text(hjust =0.5), 
        text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.position = "bottom")+      
  coord_sf(crs = "ESRI:54030",
           xlim = c(-180,180)*100000,
           ylim = c(-60,90)*100000)

PRICE3 <- ggplot(world_map) +
  geom_spatvector(aes(fill = PCG1991_price/1e9)) +       
  ggtitle("National production price of PCG1991 (billion US dollars)") +              
  scale_fill_gradientn(colors=mypal2,           
                       name = "value", 
                       na.value = "transparent",
                       guide = guide_colorbar(
                         barwidth = 20
                       ))+      
  theme_minimal()+                             
  theme(plot.title = element_text(hjust =0.5), 
        text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.position = "bottom")+      
  coord_sf(crs = "ESRI:54030",
           xlim = c(-180,180)*100000,
           ylim = c(-60,90)*100000)

PRICE4 <- ggplot(world_map) +
  geom_spatvector(aes(fill = PCG2016_price/1e9)) +       
  ggtitle("National production price of PCG2016 (billion US dollars)") +              
  scale_fill_gradientn(colors=mypal2,           
                       name = "value", 
                       na.value = "transparent",
                       guide = guide_colorbar(
                         barwidth = 20
                       ))+      
  theme_minimal()+                             
  theme(plot.title = element_text(hjust =0.5), 
        text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.position = "bottom")+      
  coord_sf(crs = "ESRI:54030",
           xlim = c(-180,180)*100000,
           ylim = c(-60,90)*100000)

grid.arrange(PRICE1, PRICE3,
             PRICE2, PRICE4,
             ncol = 2)

## At the national level, calculate the values of YPC2016-YPC1991 and YPCG2016-YPCG1991####
world_map$YPC_dif = world_map$PC2016_price - world_map$PC1991_price
world_map$YPCG_dif = world_map$PCG2016_price - world_map$PCG1991_price

DIF1 <- ggplot(world_map) +
  geom_spatvector(aes(fill = YPC_dif/1e9)) +       
  ggtitle("production price differences of PC2016-1991 (billion US dollars)") +              
  scale_fill_gradientn(colors=mypal2,           
                       name = "value", 
                       na.value = "transparent",
                       guide = guide_colorbar(
                         barwidth = 20
                       ))+      
  theme_minimal()+                             
  theme(plot.title = element_text(hjust =0.5), 
        text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.position = "bottom")+      
  coord_sf(crs = "ESRI:54030",
           xlim = c(-180,180)*100000,
           ylim = c(-60,90)*100000)

DIF2 <- ggplot(world_map) +
  geom_spatvector(aes(fill = YPCG_dif/1e9)) +       
  ggtitle("production price differences of PCG2016-1991 (billion US dollars)") +              
  scale_fill_gradientn(colors=mypal2,           
                       name = "value", 
                       na.value = "transparent",
                       guide = guide_colorbar(
                         barwidth = 20
                       ))+      
  theme_minimal()+                             
  theme(plot.title = element_text(hjust =0.5), 
        text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.position = "bottom")+      
  coord_sf(crs = "ESRI:54030",
           xlim = c(-180,180)*100000,
           ylim = c(-60,90)*100000)

grid.arrange(DIF1, DIF2,
             nrow = 2)

## 4. calculate the gapPCG1991=YPCG1991-Value of Agricultural Production 1991.csv##
csv2016 <- read.csv("new/Value of Agricultural Production 2016.csv")

world_map$csv1991 = NA
csv1991$Area[which(is.na(match(csv1991$Area,world_map$COUNTRY)))] = c("Bolivia","China","Ethiopia","Iran","Netherlands","Turkiye","United Kingdom","United States")
world_map$csv1991[match(csv1991$Area,world_map$COUNTRY)] = csv1991$Value * 1000

world_map$csv2016 = NA
csv2016$Area[which(is.na(match(csv2016$Area,world_map$COUNTRY)))] = c("Bolivia","China","Czech Republic","Iran","Netherlands","Palestinian Territory","Moldova","Turkiye","United Kingdom","Tanzania","United States")
world_map$csv2016[match(csv2016$Area,world_map$COUNTRY)] = csv2016$Value * 1000

world_map$gapPCG1991 = world_map$PCG1991_price - world_map$csv1991
world_map$gapPCG2016 = world_map$PCG2016_price - world_map$csv2016

world_map$gap = world_map$gapPCG2016 - world_map$gapPCG1991

#plot the gap
ggplot(world_map) +
  geom_spatvector(aes(fill = gap/1e9)) +       
  ggtitle("Wheat Production Value Gap (billion USD)") +              
  scale_fill_gradientn(colors=mypal2,           
                       name = "value", 
                       na.value = "transparent",
                       guide = guide_colorbar(
                         barwidth = 20
                       ))+      
  theme_minimal()+                             
  theme(plot.title = element_text(hjust =0.5), 
        text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.position = "bottom")+      
  coord_sf(crs = "ESRI:54030",
           xlim = c(-180,180)*100000,
           ylim = c(-60,90)*100000)

## 5、Calculate the gross product of the two tifs in the PCG file and make the difference gap=2016-1991。 ####
YPCG2016 = PCG2016 * 161.469904095238 * cellSize(PCG2016)/10000
YPCG1991 = PCG1991 * 111.980389897233 * cellSize(PCG1991)/10000
gap = YPCG2016 - YPCG1991
  
## 6、 Mask the figure with threshold####
gap[gap <= 0] = NA

clip1 = rast("new/1.tif")
gap [ clip1 < 0.2 ] = NA

clip2 = rast("D:/Climate data/data/pc/pc_yield_2016.tif")
gap [ is.finite(clip2) ] = NA

clip3 = rast("D:/Climate data/Yield_variation_TQirCO2.tif")
gap [ clip3 < 0 ] = NA

clip4 = rast("D:/Climate data/clip4.tif")
clip4 = resample(clip4, r_crop)

gap [ clip4 > 500 ] = NA

clip5 = rast("D:/Climate data/data/Cropland2000_5m.tif")
clip5 = resample(clip5, r_crop)

gap = mask(gap,clip5)

gap[gap/1e9 < 0.04 ] = NA
#画
#plot#
ggplot() +
  geom_spatraster(data = gap/1e9)+                                 
  geom_spatvector(data = WorldSHP, 
                  fill = "transparent") +       
  ggtitle("Potential Planting Area with Value(billion USD)") +              
  scale_fill_gradientn(colors=viridis::viridis(n_colors),           
                       name = "value", 
                       na.value = "transparent",
                       breaks = seq(0, 0.4 , length.out = n_colors +1),
                       guide = guide_colorbar(
                         barwidth = 20
                       ))+      
  theme_minimal()+                             
  theme(plot.title = element_text(hjust =0.5), 
        text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.position = "bottom")+      
  coord_sf(crs = "ESRI:54030",            
           xlim = c(-152,152)*100000,    
           ylim = c(-60,70)*100000)
writeRaster(gap,"D:/Climate data/Potential Planting Area with Value.tif",overwrite=TRUE)
world_map$PPAV = exact_extract(raster::raster(gap),as_sf(world_map),"weighted_sum", weights = 'area')

values(world_map) [values(world_map) == 0] =NA

n_colors <- 10
palette <- brewer.pal(n_colors, "Spectral")
RobinsonPlot1 <- ggplot(world_map) +
  geom_spatvector(aes(fill = Value2016/1e15)) +       
  ggtitle("Potential Planting Area with Value (billion USD)") +              
  scale_fill_gradientn(colors=viridis::viridis(n_colors),           
                       name = "Value", 
                       na.value = "transparent",
                       #breaks = seq(0, 3 , length.out = n_colors +1),
                       guide = guide_colorbar(
                         barwidth = 20
                       ))+      
  theme_minimal()+                             
  theme(plot.title = element_text(hjust =0.5), 
        text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.position = "bottom")+      
  coord_sf(crs = "ESRI:54030",
           xlim = c(-180,180)*100000,
           ylim = c(-60,90)*100000)
RobinsonPlot1
