setwd('C:/Users/je0160je/Work Folders/Documents/BIOS15')
dat <- read.csv2('C:/Users/je0160je/Work Folders/Documents/BIOS15/Eulaema_R.csv')

install.packages("MASS")
install.packages("pscl")
install.packages("patchwork")
#libraries####
library(tidyverse)
library(dplyr)
library(MASS)
library(pscl)
library(patchwork)

#oysten plot####
dat$mcMAP = dat$MAP - mean(dat$MAP, na.rm=T)
m = glm.nb(Eulaema_nigrita ~ mcMAP + forest, data = dat)
summary(m)


plot(dat$forest, dat$Eulaema_nigrita, col="grey", las=1,
     xlab="Forest cover",
     ylab=expression(paste(italic("El. nigrita")," abundance")))
newforest = seq(min(dat$forest), max(dat$forest), length.out=200)
newMAP = rep(mean(dat$mcMAP), length(newforest))
y_hat = predict(m, newdata=list(mcMAP=newMAP,
                                forest=newforest),
                type="response")
lines(newforest, y_hat, lwd=2)
newMAP2 = rep(mean(dat$mcMAP)+sd(dat$mcMAP), length(newforest))
y_hat2 = predict(m, newdata=list(mcMAP=newMAP2,
                                 forest=newforest),
                 type="response")
newMAP3 = rep(mean(dat$mcMAP)-sd(dat$mcMAP), length(newforest))
y_hat3 = predict(m, newdata=list(mcMAP=newMAP3,
                                 forest=newforest),
                 type="response")
lines(newforest, y_hat2, lwd=2, col="blue3")
lines(newforest, y_hat3, lwd=2, col="firebrick")
legend("topleft", lty=1, lwd=2, col=c(1, "blue3", "firebrick"), bty="n",
       legend=c("MAP = Mean",
                "MAP = Mean + SD",
                "MAP = Mean - SD"))
abline(h=exp(coefs[1,1]), lty=2)
abline(h=exp(coefs[1,1] + coefs[3,1]), lty=2)

#my plot####

#predictor variables mean annual temperature, precipitation, temperature and precipitation seasonality, 
#land use (proportion forest cover) and land use heterogeneity
head(dat)

#pair to check for multiblabla
pairs(dat[,5:12], 
      panel=panel.smooth)
#no obvious one to remove

#ordinary to check residuals vs df

m = glm(Eulaema_nigrita~MAT+Pseason, "poisson", data=dat)
summary(m)
#residuals vs diff enormous = do negative binomial

dat$mcMAT = dat$MAT - mean(dat$MAT, na.rm=T)
mod <- glm.nb(data= dat, Eulaema_nigrita ~ Pseason + mcMAP )
summary(mod)

exp(0.0238403)# 1.024127 meaning for every % increase in pseason there is an increase of
              # 2.4 % abundance of E.nigrita
exp(-0.0014454) # 0.9985556 meaning for every 1 mm incease in mcMAP
                # decrease in abundance by 0.14% OR 14% per 100 mm


anova(mod)
pR2(mod)
#now residuals ok
#plot for Pseason ####

newPseason = seq(min(dat$Pseason), max(dat$Pseason), length.out=200) #generating a sequence of x from min to max with 200 steps inbetween
newmcMAP = rep(mean(dat$mcMAP), length(newPseason)) # repeats mcMAP for n=Pseason. To get mcMAp constant for Pseason 
y_hat <- predict(mod,
                 newdata = data.frame(
                   mcMAP = newmcMAP,
                   Pseason = newPseason),
                 type = "response")
pred <- data.frame(newPseason = newPseason,
                   y_hat = y_hat, type="response")
                
plot <- ggplot(data=dat, aes(Pseason,Eulaema_nigrita))+
  geom_point()+
  geom_line(data=pred, 
            linewidth = 1, 
            aes(x= newPseason, 
                y= y_hat,
                color = "Predicted abundance"))+
  labs(x="Precipitation seasonality(%)",
       y="Eulaema Nigrita",
       colour = NULL)+
  theme_minimal()+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    legend.text = element_text(size =12),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8),
    panel.grid.minor = element_blank()
  )
plot

#plot for MAP
newmcMAP2 = seq(min(dat$mcMAP), max(dat$mcMAP), length.out=200)

newPseason2 = rep(mean(dat$Pseason), length(newmcMAP2)) 
y_hat2 <- predict(mod,
                 newdata = data.frame(
                   mcMAP = newmcMAP2,
                   Pseason = newPseason2),
                 type = "response")
pred2 <- data.frame(newPseason2 = newPseason2,
                   y_hat2 = y_hat2, type="response")

plot2 <- ggplot(data=dat, aes(mcMAP,Eulaema_nigrita))+
  geom_point()+
  geom_line(data=pred2,
            linewidth = 1,
            aes(x= newmcMAP2, 
                y= y_hat2, 
                color = "Predicted abundance"),
            )+
  labs(x="Mean centered annual precipitation (mm)",
       y="Eulaema Nigrita",
       colour = NULL)+
  theme_minimal()+
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    legend.text = element_text(size =12),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8),
    panel.grid.minor = element_blank()
  )

plot2

#combining plots for readability
(plot | plot2) +
  plot_annotation(
    tag_levels = "a",
    tag_suffix = ")"
  )+
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.text = element_text(size = 16))
