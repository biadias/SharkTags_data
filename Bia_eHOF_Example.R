
library(eHOF)
library(ggplot2)
library(ggpubr)
library(pals)
library(tidyverse)

load("C:/Users/Beatriz/Dropbox/A_MBAq/PAPER Bia juvenile WS range shift/Aptos_ws/0_Data/manual/JWS_df.RData")

data<-JWS_df %>% 
  group_by(Temperature, id) %>% 
  summarise(number = n())

table(data$id)


#data = subset(data, id == "JWS_10_19_08A0716_88767")

x = data$Temperature
y = data$number

y = ifelse(data$number > median(y), 1, 0)

#Build eHOF model
m <- HOF(y, x, bootstrap = NULL) 
m

#restrict model selection between 1-5
models = c(1:5)

#choose best model between 1-5
model_type = pick.model(m, model = eHOF.modelnames[models])

#take out best model data
best_model = m$models[[model_type]]

#save central borders parameters from best model
param = Para(m, pick.model(m, model = eHOF.modelnames[models]))$centralBorder

#create a data frame for plotting
df = data.frame(m$x, best_model$fitted)

# rounding and aggregating to speedup plotting
df$m.x = round(df$m.x, 1) 
df = aggregate(best_model.fitted ~ m.x, data = df, FUN = "mean")

#plot best eHOF model
plot(m,
     para = T,
     onlybest = T,
     boxp = F,
     marginal = c('n'),
     gam.se = T,
     # color = 5,
     model = pick.model(m, model = eHOF.modelnames[models]),
     # yl = c(0,0.5),
     lwd = 2)

# save eHOF parameters
par_ALL <- list("model_type" = model_type,
                "df" = df, 
                "Low" = Para(m, pick.model(m, model = eHOF.modelnames[models]))$centralBorder[1], 
                "High" = Para(m, pick.model(m, model = eHOF.modelnames[models]))$centralBorder[2], 
                "T_opt" = Para(m, pick.model(m, model = eHOF.modelnames[models]))$opt[1])

