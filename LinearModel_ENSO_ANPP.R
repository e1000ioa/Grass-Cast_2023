#libraries
library(ggplot2)
library(dplyr)
library(ggrepel) #for year labels
library(ggpubr)
library(finalfit) #Pvalue format


  ###### ENSO ####

### Prep data Set ####

#Data

# Create a data frame with JMF 
ENSO_SP <- read.csv("data/ENSOvsSpr1980_2020.csv") %>% filter(year >= 1980)
xj <- unlist(ENSO_SP[4]) #JMF
yj <- unlist(ENSO_SP[15])
zj <- unlist(ENSO_SP[3])
JFM <- data.frame(x=xj, y=yj, year = zj)

model_JFM <- lm(x ~ y, data=JFM)
summary_model_JFM <- summary(model_JFM)
p_value <- summary_model_JFM$coefficients[2, "Pr(>|t|)"]

finalfit::p_tidy(p_value,
                 digits = 3)

### Create a scatter plot with linear JMF #####
J <- ggplot(JFM, aes(x = JFM$x, y = JFM$y, label=JFM$year)) +
  geom_point(color = "#AAB645", alpha = 1) +
  geom_smooth(method="lm", formula=y~x, se=FALSE, color="#AAB645") +
  geom_text_repel(size=3, alpha = 0.3, nudge_y= -(max(JFM$y)/100)) +
  xlab(paste("JFM (°C)")) +
  ylab(paste("ANPP mean anommally  (lb/ac)")) +
  
  geom_hline(yintercept=0, linetype="dashed", color="gray") +
  geom_vline(xintercept=0, linetype="dashed", color="gray") +
  
  ylim(-40,45) +
  xlim(-1.5,1.5) +
  
  #Annotate linear equation
  annotate(
    "text",
    x = -1.4,
    y = 45,
    label = paste0("y = ", round(coef(model_JFM)[2], digits = 2), "x ", signif(coef(model_JFM)[1], digits = 3)),
    color = "#595959",
    size = 5,
    hjust = 0
  ) + 
  
  
  #Annotate R2
  annotate(
    "text",
    x  = -1.4,
    y = 40,
    label = paste(expression("R²"), "=", signif(summary(model_JFM)$r.squared, digits = 3)),
    color = "#595959",
    size = 5,
    hjust = 0
  ) + 
  

  #Annotate p value
  annotate(
    "text",
    x  = -1.4,
    y = 35,
    label = paste("p", finalfit::p_tidy(summary(model_JFM)$coefficients[2,4], digits = 2)),
    color = "#595959",
    size = 5,
    hjust = 0
  ) + 
  
  theme_classic() +
  theme(axis.line = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size=0.8))

# Create a data frame with MAM
ENSO_SU <- read.csv("data/ENSOvsSU_1950_2020.csv") %>% filter(Year >= 1980)
colnames(ENSO_SU)
xm <- unlist(ENSO_SU[4])
ym <- unlist(ENSO_SU[8])
zm <- unlist(ENSO_SU[2])
MAM <- data.frame(x=xm, y=ym, year = zm)

model_MAM <- lm(x ~ y, data=MAM)
summary_model_MAM <- summary(model_MAM)
p_value <- summary_model_MAM$coefficients[2, "Pr(>|t|)"]

### Create a scatter plot with linear MAM #####
M <- ggplot(MAM, aes(x = x, y = y,label=MAM$year)) +
  geom_point(color = "#FEC306", alpha = 1) +
  geom_smooth(method="lm", formula=y~x, se=FALSE, color="#FEC306") +
  geom_text_repel(size=3,alpha = 0.3, nudge_y=-(max(MAM$y)/100))+
  xlab(paste("MAM (°C)")) +
  ylab(paste("ANPP mean anommally  (lb/ac)")) +
  
  geom_hline(yintercept=0, linetype="dashed", color="gray") +
  geom_vline(xintercept=0, linetype="dashed", color="gray") +
  
  ylim(-40,45) +
  xlim(-1.5,1.5) +
  
  annotate(
    "text",
    x = -1.4,
    y = 40,
    label = paste(expression("R²"), "=", signif(summary(model_MAM)$r.squared, digits = 3)),
    color = "#595959",
    size = 5,
    hjust = 0
  ) + #Annotate R2
  
  annotate(
    "text",
    x = -1.4,
    y = 45,
    label = paste0("y = ", round(coef(model_MAM)[2], digits = 2), "x ", signif(coef(model_MAM)[1], digits = 3)),
    color = "#595959",
    size = 5,
    hjust = 0
  ) + #Annotate linear equation
  
  annotate(
    "text",
    x = -1.4,
    y = 35,
    label = paste("p", finalfit::p_tidy(summary(model_MAM)$coefficients[2,4], digits = 2)),
    color = "#595959",
    size = 5,
    hjust = 0
  ) + #Annotate R2
  
  theme_classic() +
  theme(axis.line = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.8))


M
#### Combine multiple ggplot on one page #### 

ggarrange(J, M,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

ggsave("images/NDVI_ANPP_lm.png", 
       dpi = 350,
       height = 18,
       width = 33,
       units = "cm")

