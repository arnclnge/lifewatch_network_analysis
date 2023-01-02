library(vegan)
library(tidyverse)

setwd("~/lifewatch_network_analysis/cpod/")
df_day <- read_csv("csv/df_day.csv") #output from cpod_analysis.R

#-----------------------PCA
df.pca <- df_day %>% filter(species == "NBHF") %>% group_by(station,Season) %>% summarise(sum(dpm),sum(lost_minutes), sum(noise), sum(hrs)) 

colnames(df.pca)[3] <- "DPM"
colnames(df.pca)[4] <- "Lost minutes"
colnames(df.pca)[5] <- "Noise"
colnames(df.pca)[6] <- "Hours of activity"

library(ggbiplot)
pca <- prcomp(df.pca[,c(3:6)], center = TRUE,scale. = TRUE)
summary(pca)

#plot PCA
ggbiplot(pca,labels=df.pca$station, groups=df.pca$Season)+theme_linedraw()
ggsave("plots/pca_nbhf.png", width = 11, height = 6, dpi=300)


#----------------------NMDS
df_nmds = df_day %>%  filter(species == "Dolphins") %>% group_by(station, deployment_fk, Season) %>% summarise(dpm= sum(dpm), lost_minutes=sum(lost_minutes), noise= sum(noise), hrs=sum(hrs))%>% 
                mutate(station_type = case_when(station == "Birkenfels" | station=="Gardencity" | station=="Belwindreefballs"| station=="Westhinder" ~ "offshore",
                                                station == "Cpowerreefballs"|station=="Buitenratel" | station=="Faulbaums"|station=="Grafton"~"midshore",
                                               station == "Nauticaena"~"nearshore"))

mat_nmds = df_nmds[,c("dpm", "lost_minutes","noise", "hrs")] %>%  as.matrix()

set.seed(123)
nmds = metaMDS(mat_nmds, distance = "bray")

plot(nmds)

#extract NMDS scores (x and y coordinates) for sites from newer versions of vegan package
data.scores = as.data.frame(scores(nmds)$sites)

#add columns to data frame 
data.scores$Season = df_nmds$Season
data.scores$station_type = df_nmds$station_type

head(data.scores)

plot.nmds = ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes( shape = station_type, colour = Season))+ 
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.2),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Season", y = "NMDS2", shape = "Station type") 

plot.nmds

#add factors
env <- df_nmds[,c("dpm", "lost_minutes","noise", "hrs")]
en = envfit(nmds, env, permutations = 999, na.rm = TRUE)
en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en)

plot.nmds2 = ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(data = data.scores, aes(colour = Season, shape = station_type), size = 3, alpha = 0.5) + 
  scale_colour_manual(values = c("orange", "steelblue","violet","green"))  + 
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               data = en_coord_cont, linewidth =1, alpha = 0.5, colour = "grey30") +
  geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
            fontface = "bold", label = row.names(en_coord_cont)) + 
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
        legend.title = element_text(size = 10, face = "bold", colour = "gr ey30"), 
        legend.text = element_text(size = 9, colour = "grey30")) + 
  labs(colour = "Season", shape ="Station type")
plot.nmds2

ggsave("plots/nmds_dol.png", width = 11, height = 6, dpi=300)

#------------------ANOSIM
ano = anosim(mat_nmds, df_nmds$station_type, distance = "bray", permutations = 9999)
ano #no statistical diff among the 3 types of stations

ano = anosim(mat_nmds, df_nmds$Season, distance = "bray", permutations = 9999)
ano #no statistical diff among the 4 seasons

#------------------PERMANOVA
df.dist <- vegdist(mat_nmds, method="bray")
adonis2(df.dist ~ station_type*Season, data = df_nmds, permutations = 999, method="bray") #no statistical diff

dune.div <- adonis2(dune ~ Management*A1, data = dune.env, permutations = 999, method="bray")


