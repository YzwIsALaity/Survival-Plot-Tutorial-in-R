library(tidyr)
library(dplyr)
library(ggplot2)
library(knitr)
library(survminer)
library(survival)
library(stringi)
# Not in operator
`%notin%` <- Negate(`%in%`)

# Get data 
Dt.Plot <- read.csv('Sample Dataset for Survival Plot.csv')
# Factor variable
Dt.Plot$Treatment <- factor(Dt.Plot$Treatment, levels = c('Group 1', 'Group 2', 'Group 3', 'Group 4'))
Dt.Plot$Stage <- factor(Dt.Plot$Stage, levels = c('Stage I', 'Stage II', 'Stage III', 'Stage IV'))
Dt.Plot$Histology <- factor(Dt.Plot$Histology, levels = c('Squamous Cell Carcinoma', 'Adenocarcinoma', 'Unknown', 'Unspecified Carcinoma', 'Other', 'Sarcoma'))
Dt.Plot$Status <- ifelse(Dt.Plot$Status == 1, 0, 1)
Dt.Plot <- na.omit(Dt.Plot)

# Show dataset
kable(head(Dt.Plot))

# Fit a Kaplan-Meier curve for cancer stage
KM <- survfit(Surv(Time, Status) ~ Stage, data = Dt.Plot)

# Basic: survical curve with ggplot
ggsurvplot(fit = KM, data = Dt.Plot,   # Kaplan-Meier estimator and dataset               
           pval = T,                   # show p-value
           pval.size = 4,              # font size of p-value
           pval.coord = c(175, 1.0),   # coordination of p-value (x, y)
           legend = 'right',           # location of legend
           surv.median.line = 'hv')    # show median survival line

# Version 2
KM2 <- survfit(Surv(Time, Status) ~ Stage + Treatment, data = Dt.Plot)
ggsurvplot(KM2, data = Dt.Plot, 
           facet.by = 'Treatment',                                          # create multipanel used variable "Treatment"
           pval = TRUE, 
           legend = 'right', 
           legend.labs = c('Stage I', 'Stage II', 'Stage III', 'Stage IV')) # create a self-defined legend label

# Customize theme
Customized.Theme <- function(){
  theme_survminer() %+replace%                           # a ggplot2 function 
    theme(panel.border = element_blank(),
          panel.background = element_blank(),                    
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.line.x = element_line(),                               # these two are for the axis line
          axis.line.y = element_line(),
          axis.text.x = element_text(family = 'Times', colour = "black", size = 11),    # there two are for texts in axes
          axis.text.y = element_text(family = 'Times', colour = "black", size = 11),
          axis.ticks.x = element_line(),                              # these two are for ticks in axes
          axis.ticks.y = element_line(),
          axis.title.x = element_text(family = 'Times', colour = "black", size = 11, face = 'bold', vjust = -1),                              
          axis.title.y = element_text(family = 'Times', colour = "black", size = 11, face = 'bold', angle = 90, vjust = 3),
          legend.title = element_text(family = 'Times', colour = "black", size = 11, face = 'bold'),
          legend.text = element_text(family = 'Times', colour = "black", size = 11),
          strip.text.x = element_text(family = 'Times', colour = "black", size = 11))
}

# Color/palette for cancer stage
Legend.Labs <- c('Stage I', 'Stage II', 'Stage III', 'Stage IV')            # create labels for cancer stages
n.color <- length(Legend.Labs)                                              # the lenght of vector for labels
Palette.New <- rainbow(n.color, alpha = 0.5)                                # obtain a vector of colors for labels

# Kaplan-Meier estimator
KM2 <- survfit(Surv(Time, Status) ~ Stage + Treatment, data = Dt.Plot)    

# Survival plot
ggsurvplot(KM2, data = Dt.Plot, 
           facet.by = 'Treatment',                                          # create multipanel used variable "Treatment"
           pval = TRUE, 
           legend = 'right',                                                # legend position
           legend.labs = Legend.Labs,                                       # self-defined legend label
           palette = Palette.New,                                           # self-defined color
           xlab = 'Time (Month)', ylab = 'Survival Probability',            # modify X-axis and Y-axis labels
           ggtheme = Customized.Theme())                                    # use modified non-graphical components






