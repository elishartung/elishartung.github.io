library(tidyverse)
library(vegan)


emergence1 <- full %>% 
  select(population,plant_number,ends_with("_e")) %>% 
  select(!contains("plant_e")) %>% 
  select(!contains("rivulet")) %>% 
  select(!contains("other")) %>% 
  mutate(plant_id = paste0(population,"_",plant_number)) 

emergence1[is.na(emergence1)] <- 0

emergence_matrix1 <- emergence1 %>% 
  select(ends_with("_e"))

row.names(emergence1) <- emergence1$plant_id


NMDS <- metaMDS(emergence_matrix1,
                distance = "jaccard",
                binary=TRUE,
                autotransform = FALSE)
MDS1 <- NMDS$points[,1]
MDS2 <- NMDS$points[,2]

emergence1$MDS1 <- MDS1
emergence1$MDS2 <- MDS2
names(emergence1)

ggplot(emergence1, aes(x=MDS1,y=MDS2,color=population)) +
  geom_point(size=4,alpha=.2) +
  stat_ellipse()

perm <- adonis(formula = emergence_matrix1 ~ emergence1$population)
perm

crypto_e_mod <- aov(data=emergence1,
                    formula = crypto_e ~ population)
summary(crypto_e_mod)
TukeyHSD(crypto_e_mod) %>% plot()
