setwd('C:/Users/je0160je/Work Folders/Documents/BIOS15')
Data.WormMass<- read.csv2('C:/Users/je0160je/Work Folders/Documents/Experiment/Experiment 2 - asfalt 2024/R/new/Mass_R.csv')

#libraries####
library(tidyverse)

#Data####
#calculate mean, SD and SE for the change of mass at the end of the experiment

Data.Summary <- Data.WormMass |> #grouping the treatment factors OC and PAH and calculations
  filter(PAH.treatment != "PAH-M") |>
  group_by(PAH.treatment, OC.treatment)|>
  summarise(
    MeanMass = mean(Mean.Change, na.rm = TRUE),
    SD = sd(Mean.Change, na.rm = TRUE),
    N = n(),
    SE = SD / sqrt(N),
    .groups = "drop"
  )
Data.Summary

#plot####
#plot of average mean net change in earthworm mass (g)
linear.plot <- ggplot(Data.Summary, 
                      aes(OC.treatment, MeanMass, color= PAH.treatment, group= PAH.treatment))+
  geom_line(linewidth= 2, aes(linetype= PAH.treatment))+
  geom_point(size= 4, aes(shape = PAH.treatment))+
  geom_errorbar(aes(ymin= MeanMass - SE,
                    ymax= MeanMass + SE,
                    width = 0.2))+
  theme(panel.background = element_rect(fill="white",
                                        color="black"),
        text = element_text(size=10),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_line(color = "grey90"),
        legend.position = "inside",
        legend.position.inside = c(0.92,0.92),
        legend.background = element_rect(colour = "black",
                                         fill = "white",
                                         linewidth = 0.5),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))+
  labs (y= "Average eartworm mass change(g)",
        x="OC (%)",
        color= "PAH-16 (mg/kg)",
        shape="PAH-16 (mg/kg)",
        linetype ="PAH-16 (mg/kg)")+
  scale_x_discrete(limits = c("OC-L","OC-M", "OC-H"))
linear.plot
ggsave("Figure1.png")

#just a better visualisation of the overlap of data which was pretty large

mass.overlap <- ggplot(Data.WormMass, aes(OC.treatment, Mean.Change, group=PAH.treatment, fill= PAH.treatment))+
  geom_smooth()+
  theme(panel.background = element_rect(fill="white",
                                        color="black"),
        text = element_text(size=10),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_line(color = "grey90"),
        legend.position = "right corner")+
  labs (y= "Average earthworm mass loss (%)",
        x="OC",
        color= "PAH.treatment",
        shape="PAH.treatment",
        linetype ="PAH.treatment")+
  scale_x_discrete(limits = c("OC-L", "OC-M", "OC-H"))
mass.overlap 

#Linear model and anova####

Data.Model <- Data.WormMass |> #filtering out PAH-M from model and grouping 
  group_by(PAH.treatment, OC.treatment)|>
  filter(PAH.treatment != "PAH-M") 

Data.Model$OC.treatment <- factor(Data.Model$OC.treatment) #turning str to factors
Data.Model$PAH.treatment <- factor(Data.Model$PAH.treatment)

#linear model for OC and PAH to look for effects and interactions
linear.model <- lm(Mean.Change ~ OC.treatment * PAH.treatment, 
                   data= Data.Model)

summary(linear.model)#model and summary
anova(linear.model) #anova for model

#Anova indicates that OC is important for mass. However the curve looks very unexpected and I cannot explain why
#next step is to look closer at OC to see which group is different from which

#changing reference level to OC M as it looked visually most different in the figure
Data.Model$OC.treatment <- relevel(
  factor(Data.Model$OC.treatment),
  ref = "OC-M")

#linear model for OC to quantify effect
linear.model.OC <- lm(Mean.Change ~ OC.treatment, 
                   data= Data.Model)
summary(linear.model.OC)
anova(linear.model.OC)

#linear model for PAH to quantify the effect
linear.model.PAH <- lm(Mean.Change~ PAH.treatment, 
                      data= Data.Model)
summary(linear.model.PAH)
anova(linear.model.PAH)

