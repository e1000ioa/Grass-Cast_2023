#libraries
library(ggplot2)
library(dplyr)
library(ggrepel) #for year labels
library(ggpubr)
library(finalfit) #Pvalue format
library(scales) #p value fomat


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

###CREATES A DATAFRANE WITH ALL DATA

# Variables to predict
variables_to_predict <- c(
  "low_mean_percent_anom", "low_med_percent_anom", "low_stdev_percent_anom",
  "med_mean_percent_anom", "med_med_percent_anom", "med_stdev_percent_anom",
  "high_mean_percent_anom", "high_med_percent_anom", "high_stdev_percent_anom"
)

# Initialize a list to store results for MAM, JJA, and OND
models_JFM <- list()
models_MAM <- list()
models_JJA <- list()
models_OND <- list()

# Loop through variables and fit linear models for MAM, JJA, and OND
for (var in variables_to_predict) {
  
  formula_JFM <- as.formula(paste(var, "~ JFM"))
  formula_MAM <- as.formula(paste(var, "~ MAM"))
  formula_JJA <- as.formula(paste(var, "~ JJA"))
  formula_OND <- as.formula(paste(var, "~ OND"))
  
  model_JFM <- lm(formula_JFM, data = ENSO_SP)
  model_MAM <- lm(formula_MAM, data = ENSO_SP)
  model_JJA <- lm(formula_JJA, data = ENSO_SP)
  model_OND <- lm(formula_OND, data = ENSO_SP)
  
  models_JFM[[var]] <- model_JFM
  models_MAM[[var]] <- model_MAM
  models_JJA[[var]] <- model_JJA
  models_OND[[var]] <- model_OND
}



#####
results_JFM <- data.frame(
  Variable = variables_to_predict,
  R_squared_JFM = sapply(models_JFM, function(model) round(summary(model)$r.squared, 3)),
  p_value_JFM = sapply(models_JFM, function(model) scales::pvalue(coef(summary(model))["JFM", "Pr(>|t|)"], accuracy = 0.05, # Number to round to
                                                                                                           decimal.mark = ".", # The character to be used to indicate the numeric decimal point
                                                                                                          add_p = TRUE ))) # Add "p=" before the value?


results_MAM <- data.frame(
  Variable = variables_to_predict,
  R_squared_MAM = sapply(models_MAM, function(model) round(summary(model)$r.squared, 3)),
  p_value_MAM = sapply(models_MAM, function(model) scales::pvalue(coef(summary(model))["MAM", "Pr(>|t|)"],  accuracy = 0.05, # Number to round to
                                                                                                            decimal.mark = ".", # The character to be used to indicate the numeric decimal point
                                                                                                            add_p = TRUE ))) # Add "p=" before the value?

results_JJA <- data.frame(
  Variable = variables_to_predict,
  R_squared_JJA = sapply(models_JJA, function(model) round(summary(model)$r.squared, 3)),
  p_value_JJA = sapply(models_JJA, function(model) scales::pvalue(coef(summary(model))["JJA", "Pr(>|t|)"], accuracy = 0.05, # Number to round to
                                                                                                          decimal.mark = ".", # The character to be used to indicate the numeric decimal point
                                                                                                          add_p = TRUE ))) # Add "p=" before the value?


results_OND <- data.frame(
  Variable = variables_to_predict,
  R_squared_OND = sapply(models_OND, function(model) round(summary(model)$r.squared, 3)),
  p_value_OND = sapply(models_OND, function(model) scales::pvalue(coef(summary(model))["OND", "Pr(>|t|)"], accuracy = 0.05, # Number to round to
                                                                                                            decimal.mark = ".", # The character to be used to indicate the numeric decimal point
                                                                                                            add_p = TRUE ))) # Add "p=" before the value?


# Merge the results data frames for MAM, JJA, and OND
results <- merge(results_JFM, results_MAM, by = "Variable")
results <- merge(results, results_JJA, by = "Variable")
results_spring <- merge(results, results_OND, by = "Variable")


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

library(cNORM)
pdata <- prepareData(MAM)

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




# Create a data frame with SUmer
ENSO_SU <- read.csv("data/ENSOvsSU_1950_2020.csv") %>% filter(Year >= 1980)


