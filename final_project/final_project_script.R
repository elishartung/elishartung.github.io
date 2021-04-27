library(tidyverse)
library(janitor)
library(broom)
library(GGally)
library(purrr)
library(readxl)
library(dplyr)
library(vegan)
library(modelr)

### PLOT STUFF
beehive<-read_excel("../../microsite/beehive_clean.xlsx")
wd<- read_excel("../../microsite/wd_clean.xlsx")
rb<- read_excel("../../microsite/rb_clean.xlsx")
wb<- full_join(beehive, wd)
full<- full_join(wb,rb)

beehive_plots <- beehive %>% distinct(tag_number, .keep_all = TRUE)
wd_plots <- wd %>% distinct(tag_number, .keep_all = TRUE)
rb_plots <- rb %>% distinct(tag_number, .keep_all = TRUE)
wb_plots <- full_join(beehive_plots, wd_plots)
full_plots <- full_join(wb_plots, rb_plots)

Disturbance <- ggplot(full_plots, aes(x = disturbance, colour= population)) + 
  geom_bar() +
  theme_minimal() +
  labs(x= "Disturbance", y= "Number of Disturbed Plots") +
  theme(axis.text.x = element_text(angle=90)) +
  theme(axis.text.x = element_text(size= 6)) +
  facet_wrap(~population)
ggsave("Disturbance.jpg")

Top1 <- ggplot(full_plots, aes(x = topography_1, colour= population)) + 
  geom_bar() +
  theme_minimal() +
  labs(x= "Topography 1", y= "Number of Plots") +
  theme(axis.text.x = element_text(angle=90)) +
  theme(axis.text.x = element_text(size= 6)) +
  facet_wrap(~population)
ggsave("Top1.jpg")

Top2 <- ggplot(full_plots, aes(x = topography_2, colour= population)) + 
  geom_bar() +
  theme_minimal() +
  labs(x= "Topography 2", y= "Number of Plots") +
  theme(axis.text.x = element_text(angle=90)) +
  theme(axis.text.x = element_text(size= 5 )) +
  facet_wrap(~population)
ggsave("Top2.jpg")

Rough <- ggplot(full_plots, aes(x = roughness, colour= population)) + 
  geom_bar() +
  theme_minimal() +
  labs(x= "Roughness", y= "Number of Plots") +
  theme(axis.text.x = element_text(angle=90)) +
  theme(axis.text.x = element_text(size= 6)) +
  facet_wrap(~population)
ggsave("Rough.jpg")

full


### PLANT STUFF
emergence1 <-full 
  select(emergence1,population,plant_number,ends_with("_e")) %>%
  select(!contains("plant_e")) %>% 
  select(!contains("rivulet")) %>% 
  select(!contains("other")) %>% 
  mutate(plant_id = paste0(population,"_",plant_number)) 

emergence1[is.na(emergence1)] <- 0

emergence_matrix1 <- emergence1 %>% 
  select(ends_with("_e"))

emergence_matrix1[is.na(emergence_matrix1)] <- 0


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

emergence11 <- emergence1 %>% mutate_all(as.logical)
emergence_matrix2 <-emergence_matrix1 %>% mutate_all(as.logical)
emergence_matrix2$cow_e <- emergence_matrix2$plant_e <- emergence_matrix2$other_e <- emergence_matrix2$rivulet_e <- NULL

perm <- glm(formula= emergence11$plant_number~ crypto_e + lg_gr_e + sm_gr_e + gyp_e + bare_e, 
            family = binomial, data= emergence_matrix2, maxit= 100)
add_predictions(emergence_matrix2, perm, type= "response")


####COVER 
cover1 <- full_plots %>%select(population,plot_number,ends_with("_c"))

cover1[is.na(cover1)] <- 0

cover_matrix1 <- cover1 %>% 
  select(ends_with("_c"))

row.names(cover1) <- cover1$plant_id

NMDS2 <- metaMDS(cover_matrix1,
                distance = "jaccard",
                binary=TRUE,
                autotransform = FALSE)
MDS21 <- NMDS2$points[,1]
MDS22 <- NMDS2$points[,2]

ggplot(cover1, aes(x=MDS21,y=MDS22,color=population)) +
  geom_point(size=4,alpha=.2) +
  stat_ellipse()

perm2 <- adonis(formula = cover_matrix1 ~ cover1$population)
perm2


crypto_c_mod <- aov(data=cover1,
                    formula = crypto_c ~ population)
summary(crypto_c_mod)
TukeyHSD(crypto_c_mod) %>% plot()


add_predictions(full,crypto_c_mod) %>%
  ggplot(aes(x=crypto_c, fill=population)) +
  geom_bar() + geom_bar(aes(x= sm_gr_c, y=pred),stat = "identity", color= "Black") +
  facet_wrap(~population)




sm_gr_c_mod <- aov(data=cover1,
                   formula = sm_gr_c ~ population)
summary(sm_gr_c_mod)
TukeyHSD(sm_gr_c_mod) %>% plot()

lg_gr_c_mod <- aov(data=cover1,
                   formula = lg_gr_c ~ population)
summary(lg_gr_c_mod)
TukeyHSD(lg_gr_c_mod) %>% plot()

gyp_c_mod <- aov(data=cover1,
                 formula = gyp_c ~ population)
summary(gyp_c_mod)
TukeyHSD(gyp_c_mod) %>% plot()

plant_c_mod <- aov(data=cover1,
                 formula = plant_c ~ population)
summary(plant_c_mod)
TukeyHSD(plant_c_mod) %>% plot()

bare_c_mod <- aov(data=cover1,
                 formula = bare_c ~ population)
summary(bare_c_mod)
TukeyHSD(bare_c_mod) %>% plot()

### emergence stuff i might need
crypto_e_mod <- aov(data=emergence1,
                    formula = population ~ crypto_e)
summary(crypto_e_mod)
TukeyHSD(crypto_e_mod) %>% plot()

sm_gr_e_mod <- aov(data=emergence1,
                   formula = sm_gr_e ~ population)
summary(sm_gr_e_mod)
TukeyHSD(sm_gr_e_mod) %>% plot()


lg_gr_e_mod <- aov(data=emergence1,
                   formula = lg_gr_e ~ population)
summary(lg_gr_e_mod)
TukeyHSD(lg_gr_e_mod) %>% plot()


gyp_e_mod <- aov(data=emergence1,
                 formula = gyp_e ~ population)
summary(gyp_e_mod)
TukeyHSD(gyp_e_mod) %>% plot()


bare_e_mod <- aov(data=emergence1,
                  formula = bare_e ~ population)
summary(bare_e_mod)
TukeyHSD(bare_e_mod) %>% plot()



#### removed markdown stuff
Significance of small gravel as a emergence substrate as a function of population.
```{r, echo=FALSE}


sm_gr_e_mod <- aov(data=emergence1,
                   formula = sm_gr_e ~ population)
summary(sm_gr_e_mod)




```

Significance of large gravel as a emergence substrate as a function of population.
```{r, echo=FALSE}
lg_gr_e_mod <- aov(data=emergence1,
                   formula = lg_gr_e ~ population)
summary(lg_gr_e_mod)


```

Significance of exposed gypsum as a emergence substrate as a function of population.
```{r, echo=FALSE}
gyp_e_mod <- aov(data=emergence1,
                 formula = gyp_e ~ population)
summary(gyp_e_mod)



```

Significance of bare soil as a emergence substrate as a function of population.
```{r, echo=FALSE}

bare_e_mod <- aov(data=emergence1,
                  formula = bare_e ~ population)
summary(bare_e_mod)
```

As with cover materials, each emergence substrate above differed significantly between populations.  



