#libraries
library(ggplot2)
library(dplyr)
library(ggrepel)
library(ggpubr)
library(ggtext) #for shaowtext 

###### ENSO ####

### Prep data Set ####

#Data

# Create a data frame with JMF 
ENSO_SP <- read.csv("data/ENSOvsSpr1980_2020.csv") %>% filter(year >= 1980)
xj <- unlist(ENSO_SP[4]) #JMF
yj <- unlist(ENSO_SP[15])
zj <- unlist(ENSO_SP[3])
JFM <- data.frame(x=xj, y=yj, year = zj)
model_JFM <- lm(xj ~ yj, data=JMF)
summary(model_JFM)

# Create a data frame with MAM
ENSO_SU <- read.csv("data/ENSOvsSU_1950_2020.csv") %>% filter(Year >= 1980)
colnames(ENSO_SU)
xm <- unlist(ENSO_SU[4])
ym <- unlist(ENSO_SU[8])
zm <- unlist(ENSO_SU[3])
MAM <- data.frame(x=xm, y=ym, year = zm)
model_MAM <- lm(x ~ y, data=MAM)
summary(model_MAM)

### Create a scatter plot with linear JMF #####
J <- ggplot(JFM, aes(x = JFM$x, y = JFM$y, label=JFM$year)) +
  geom_point(color = "#AAB645", alpha = 1) +
  geom_text_repel(size=3,alpha = 0.3, nudge_y=-(max(y)/100)) +
  xlab(paste("JFM (°C)")) +
  ylab(paste("ANPP mean anommally  (lb/ac)")) +
  
  geom_hline(yintercept=0, linetype="dashed", color="gray") +
  geom_vline(xintercept=0, linetype="dashed", color="gray") +
  
  ylim(-40,45) +
  xlim(-1.5,1.5) +
  
  annotate(
    "text",
    x = -1.10,
    y = 40,
    label = paste(expression("R²"), "=", signif(summary(model_JMF)$r.squared, digits = 3)),
    color = "#595959",
    size = 5
  ) + #Annotate R2
  
  
  annotate(
    "text",
    x = -1.1,
    y = 45,
    label = paste0("y = ", round(coef(model_JMF)[2], digits = 2), "x ", signif(coef(model_JMF)[1], digits = 3)),
    color = "#595959",
    size = 5
  ) + #Annotate linear equation
  
  theme_classic() +
  theme(axis.line = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size=0.8))


### Create a scatter plot with linear MAM #####
M <- ggplot(MAM, aes(x = x, y = y,label=MAM$year)) +
  geom_point(color = "#FEC306", alpha = 1) +
  geom_smooth(method="lm", formula=y~x, se=FALSE, color="#FEC306") +
  geom_text_repel(size=3,alpha = 0.3, nudge_y=-(max(y)/100))+
  xlab(paste("MAM (°C)")) +
  ylab(paste("ANPP mean anommally  (lb/ac)")) +
  
  geom_hline(yintercept=0, linetype="dashed", color="gray") +
  geom_vline(xintercept=0, linetype="dashed", color="gray") +
  
  ylim(-40,45) +
  xlim(-1.5,1.5) +
  
  annotate(
    "text",
    x = -1.10,
    y = 40,
    label = paste(expression("R²"), "=", signif(summary(model_MAM)$r.squared, digits = 3)),
    color = "#595959",
    size = 5
  ) + #Annotate R2
  
  annotate(
    "text",
    x = -1.1,
    y = 45,
    label = paste0("y = ", round(coef(model_MAM)[2], digits = 2), "x ", signif(coef(model_MAM)[1], digits = 3)),
    color = "#595959",
    size = 5
  ) + #Annotate linear equation
  
  theme_classic() +
  theme(axis.line = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.8))

#### Combine multiple ggplot on one page #### 

ggarrange(J, M,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

ggsave("images/NDVI_ANPP_lm.png", 
       dpi = 350,
       height = 18,
       width = 33,
       units = "cm")

ggsave("images/JMF_MAM_ENSO.png", dpi = 350)
