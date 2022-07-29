library(readxl)
library(tidyverse)
library(cowplot)
library(ggpubr)
library(patchwork)
library(colortools)
library(pracma)
library(kableExtra)
library(knitr)
library(tinytex)
library(gridExtra)


dat <- read_xlsx("results_all.xlsx") 

dat.spm <- read.csv("C:/Artur/vermeer/petrol processes/Oil properties and processes/Sedimentation/spm.csv")

# Plotting function----
plt.fun <- function(dataset, Comp, Scenario){
  
  ggplot(dataset) + 
    geom_line(aes(x = Time, y = Conc, colour = Aromatics)) +
    labs(x = "Time (d)",
         y = ifelse(Comp == "Water column",
                    expression(~(mg%*%m^-3)),
                    expression(~(mg%*%kg^-1))),
         # title = ifelse(Comp == "Water column",
         #                "Concentration in water column",
         #                "Concentration in herring"),
         subtitle = case_when(Scenario == "Scenario 1" ~ "Scenario 1",
                              Scenario == "Scenario 2" ~ "Scenario 2",
                              Scenario == "Scenario 3" ~ "Scenario 3",
                              Scenario == "Scenario 4" ~ "Scenario 4",
                              Scenario == "Scenario 5" ~ "Scenario 5",
                              Scenario == "Scenario 6" ~ "Scenario 6",
                              Scenario == "Scenario 7" ~ "Scenario 7",
                              Scenario == "Scenario 8" ~ "Scenario 8",
                              Scenario == "Scenario 9" ~ "Scenario 9",
                              Scenario == "Scenario 10" ~ "Scenario 10",
                              Scenario == "Scenario 11" ~ "Scenario 11",
                              Scenario == "Scenario 12" ~ "Scenario 12")) +
    theme_cowplot(font_size = 6) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5,
                                       size = 10)) 
  
}

# Nested data sets----
dat.nest <- dat %>% 
  mutate(Comp = recode(Comp,
                       "Small carnivorous bentho-pelagic fish species (yellow perch-type species)" = "herring",
                       "Water_intr" = "Water column")) %>% 
  rename("C10-12" = "Aromatics C10-12",
         "C12-16" = "Aromatics C12-16",
         "C16-21" = "Aromatics C16-21",
         "C21-35" = "Aromatics C21-35",
         SPM = "SPM_g.m-3") %>%
  pivot_longer(cols = 8:11, names_to = "Aromatics", values_to = "Conc") %>%
  arrange(Aromatics) %>%
  mutate(Scenario = case_when(Sea == "Deep" & SPM == 100 & U == 4 & Disp == 1 ~ "Scenario 1",
                              Sea == "Deep" & SPM == 100 & U == 8 & Disp == 1 ~ "Scenario 2",
                              Sea == "Deep" & SPM == 100 & U == 4 & Disp == 1000 ~ "Scenario 3",
                              Sea == "Deep" & SPM == 100 & U == 8 & Disp == 1000 ~ "Scenario 4",
                              Sea == "Deep" & SPM == 1 & U == 4 & Disp == 1 ~ "Scenario 5",
                              Sea == "Deep" & SPM == 1 & U == 8 & Disp == 1 ~ "Scenario 6",
                              Sea == "Deep" & SPM == 1 & U == 4 & Disp == 1000 ~ "Scenario 7",
                              Sea == "Deep" & SPM == 1 & U == 8 & Disp == 1000 ~ "Scenario 8",
                              Sea == "Shallow" & SPM == "var" & U == 4 & Disp == 1 ~ "Scenario 9",
                              Sea == "Shallow" & SPM == "var" & U == 8 & Disp == 1 ~ "Scenario 10",
                              Sea == "Shallow" & SPM == "var" & U == 4 & Disp == 1000 ~ "Scenario 11",
                              Sea == "Shallow" & SPM == "var" & U == 8 & Disp == 1000 ~ "Scenario 12")) %>% 
  group_by(SPM, Sea, U, Disp, Comp, Scenario) %>% 
  nest(plot_data = c("Time", "Conc", "Aromatics")) %>%
  mutate(plots = pmap(list(.x = plot_data,
                           Comp,
                           Scenario),
                      ~plt.fun(dataset = .x,
                               Comp = Comp,
                               Scenario = Scenario))) %>% view()



res_water <- dat.nest %>% 
  filter(Comp == "Water column")

res_water.plt <- wrap_plots(res_water$plots, ncol = 2) +
  plot_annotation(title = "Concentration of dissolved aromatics in water column") & 
  theme(plot.title = element_text(hjust = 0.5))

res_fish <- dat.nest %>% 
  filter(Comp == "herring")

res_fish.plt <- wrap_plots(res_fish$plots, ncol = 2) +
  plot_annotation(title = "Concentration of aromatics in herring") & 
  theme(plot.title = element_text(hjust = 0.5))


ggsave(filename = "res_water.svg",
       plot = res_water.plt,
       dpi = 600,
       units = "cm",
       scale = 1.5,
       width = 10,
       height = 12)

# Table scenarios----

dat.nest %>%
  group_by(Scenario) %>% 
  select(!6) %>% 
  select(1,2,4,5,6) %>% 
  distinct() %>% 
  mutate(Disp = recode(Disp,
                       "1" = "1",
                       "1000" = "1:20"),
         Sea = recode(Sea,
                      "Shallow" = "10",
                      "Deep" = "50")) %>%
  rename("DOR" = "Disp",
         "SPM $(g \\times m^{-3})$" = "SPM",
         "U $(m \\times s^{-1})$" = "U",
         "Sea depth $(m)$" = "Sea") %>%
  relocate(Scenario,.before = "Sea depth $(m)$") %>% 
  kbl(escape = F,
      align = "c") %>% 
  kable_classic(full_width = F,
                "striped") %>%
  column_spec(1, bold = T, border_right = T)

  

# Monthly SPM----
spm <-c(34.4, 84.0, 31.3, 21.9, 19.4, 14.6, 37.1, 43.0, 22.3, 39.4, 28.2, 24.7, 45.8)
date <- c("08-09 Jan 02",
          "06-07 Feb 02",
          "26-27 Mar 02",
          "07-08 May 02",
          "03-04 Jun 02",
          "06-07 Aug 02",
          "07-08 Oct 02",
          "17-18 Feb 03",
          "22-23 Jul 03",
          "27-28 Aug 03",
          "28 Oct 03",
          "08 Jun 04",
          "20 oct 04")

df.spm <- data.frame(spm, date)

ggplot(data = df.spm, aes(x = date, y = spm, group = 1)) +
  geom_line(linetype = "dotdash",
            size = 2,
            colour = '#E69F00') +
  geom_point(size = 4) +
  labs( x = "Date",
        y = expression(atop(" SPM"~(g%*%m^-3)))) +
  theme_cowplot(font_size = 20) +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.8,
                                   hjust = 0.8,
                                   size = 12),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 1))

## Daily SPM----

xinter <- c(0, 6.10, 12.20, 18.30, 24)
tide <- c("HT", "LT", "HT", "LT", "HT")
xtide <- xinter + 1

spm.tide <- ggplot(dat.spm, aes(x = time_d)) +
  geom_point(aes(y = spm_feb), colour = "#B47846", size = 2) +
  geom_line(aes(y = spm_feb), linetype = "dotted", colour = "#B47846", size = 0.7) +
  geom_point(aes(y = spm_jul), colour = "#4682B4", size = 2) +
  geom_line(aes(y = spm_jul), linetype = "dotted", colour = "#4682B4", size = 0.7) +
  geom_vline(xintercept = xinter, linetype = "dashed", size = 0.5) +
  annotate("text", label = tide, x = xtide, y = 70, size = 5) +
  labs(y = expression("SPM "~(g%*%m^-3)),
       x = "Time (hours)") +
  annotate("rect", xmin = 18, xmax = 24,
           ymin = 54, ymax = 64,
           alpha = 1, colour = "black", fill = "white") +
  annotate("point", y = c(57, 62), x = 18.5,
           colour = c("#4682B4", "#B47846"), size = 3) +
  annotate("text", y = c(57, 62), x = 19,
           label = c("July", "February"),
           size = 5, hjust = "left") +
  theme_cowplot()

spm.tide

ggsave("spm_tide.svg",
       plot = spm.tide,
       dpi = 600,
       scale = 1,
       units = "cm",
       width = 15,
       height = 10)



### Legend----

blank <-  ggplot() +
  geom_blank() +
  annotate("text",
           x = 10,
           y = 10,
           size = 8,
           parse = TRUE,
           label = expression(atop("Aromatics"))) +
  annotate("segment",
           x = 1:1,
           xend = 8:8,
           y = c(2, 4, 6, 8),
           yend = c(2, 4, 6, 8),
           colour = c("red", "orange", "green", "blue"),
           linetype = "solid",
           size = 4) +
  annotate("text",
           x = 15,
           y = 2,
           size = 6,
           label = "C10-12 (Naphthalene)") +
  annotate("text",
           x = 17.3,
           y = 4,
           size = 6,
           label = "C12-16 (2-methylnaphthalene)") +
  annotate("text",
           x = 15.3,
           y = 6,
           size = 6,
           label = "C16-21 (Phenanthrene)") +
  annotate("text",
           x = 15.7,
           y = 8,
           size = 6,
           label = "C21-35 (Benzo(a)pyrene)") +
  labs(x = "",
       y = "") +
  expand_limits(x = c(0, 30),
                y = c(0, 15)) +
  theme_cowplot(font_size = 15)



# Statistical moment approach to calculating bioaccumulation----

# Data preparation

dat.new <- dat %>%
  filter(Comp == "Small carnivorous bentho-pelagic fish species (yellow perch-type species)",
         Sea == "Shallow",
         U == 8,
         Disp == 1000,
         `SPM_g.m-3` == "var") %>% 
  mutate(Comp = recode(Comp,
                       "Small carnivorous bentho-pelagic fish species (yellow perch-type species)" = "herring")) %>% 
  rename(Ar10 = "Aromatics C10-12",
         Ar12 = "Aromatics C12-16",
         Ar16 = "Aromatics C16-21",
         Ar21 = "Aromatics C21-35") %>% 
  view()



# uptake and elimination kinetics

# Data modelled Data from de Hoop et al., (2013)

chem <- c("C10-12", "C12-16", "C16-21", "C21-35")
k.el.herring.tot <- c(0.285, 0.546, 0.164, 0.431)
k.up.herring.tot <- c(24.5, 67.0, 133.1, 196.9)
k.m.herring <- c(0.115, 0.039, 0.059, 0.264)
k.el.herring.tot.lit <- c(NA, 0.984, 0.264, NA)
k.up.herring.tot.lit <- c(NA, 2109.6, 3312, NA)
k.m.herring.lit <- c(NA, 2.63, 12.07, NA)

tbl.kin <- data.frame(Aromatics = chem,
                      k.uptake_tot = k.up.herring.tot,
                      k.elimination_tot = k.el.herring.tot,
                      k.metabolic = k.m.herring,
                      k.uptake_tot_lit = k.up.herring.tot.lit,
                      k.elimination_tot_lit = k.el.herring.tot.lit,
                      k.metabolic_lit = k.m.herring.lit)
tbl.kin %>% 
  kbl(escape = TRUE,
      caption = "Modelled and literature (de Hoop et al., 2013) uptake $(kg \\times kg^{-1} \\times d^{-1})$, elimination $(d^{-1})$ and metabolic $(d^{-1})$ rate constants in herring") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>% 
  kable_classic()


# max simulation time

time.max <- 20

# Actually density of concentration values is not required as we already have x and y data points from 
# concentration-time curve

# dens.Ar10 <- density(dat.new$Ar10)

# attributes(dens.Ar10)
# summary(dens.Ar10)
# plot(dens.Ar10)

# output <- matrix(NA, 21, 4)
# output
# 

fun.auc0.Ar21 <- approxfun(x = dat.new$Time,
                           y = dat.new$Ar21, 
                           method = "linear",
                           n = 100)

fun.auc1.Ar21 <- approxfun(x = dat.new$Time,
                           y = dat.new$Ar21*dat.new$Time, 
                           method = "linear",
                           n = 100)

fun.auc2.Ar21 <- approxfun(x = dat.new$Time,
                           y = dat.new$Ar21*dat.new$Time^2, 
                           method = "linear",
                           n = 100)

plot(fun.auc0.Ar21(1:20))
plot(fun.auc1.Ar21(1:20))
plot(fun.auc2.Ar21(1:20))

## Integrating concentration over time intervals using trapz()

## auc0.Ar10 <- trapz(dat.new$Ar10, dat.new$Time)
## 
## auc1.Ar10 <- trapz(dat.new$Ar10 * dat.new$Time, dat.new$Time)
## 
## auc2.Ar10 <- trapz(dat.new$Ar10 * dat.new$Time^2, dat.new$Time)


# Integrating concentration-time function using trapzfun()

auc0.Ar21 <- trapzfun(fun.auc0.Ar21, 1, 19.1, maxit = 1000)

auc1.Ar21 <- trapzfun(fun.auc1.Ar21, 1, 19.1, maxit = 1000)

auc2.Ar21 <- trapzfun(fun.auc2.Ar21, 1, 19.1, maxit = 1000)

auc0.Ar21
auc1.Ar21
auc2.Ar21


# Approximating the area under the right tail of the curve:

auc0.Ar21.corr <- auc0.Ar21$value + ((dat.new$Ar21[1]/tbl.kin$k.elimination_tot[4]) * exp(-(tbl.kin$k.elimination_tot[4] * time.max)))

auc1.Ar21.corr <- auc1.Ar21$value + ((dat.new$Ar21[1]/tbl.kin$k.elimination_tot[4]^2) + (dat.new$Ar21[1]*time.max/tbl.kin$k.elimination_tot[4]) * exp(-(tbl.kin$k.elimination_tot[4] * time.max)))

auc2.Ar21.corr <- auc2.Ar21$value + ((2*(dat.new$Ar21[1]/tbl.kin$k.elimination_tot[4]^3)) + 2*(dat.new$Ar21[1]/tbl.kin$k.elimination_tot[4])) + (dat.new$Ar21[1]*time.max^2/tbl.kin$k.elimination_tot[4])

auc0.Ar21.corr
auc1.Ar21.corr
auc2.Ar21.corr

# Mean Residence Time 

mrt.Ar21 <- auc1.Ar21.corr/auc0.Ar21.corr

# or

1/(Ar12.phe$k.up[1] + Ar12.phe$k.m[1]) + 1/(Ar12.phe$k.el[1] + Ar12.phe[1])

# Variance of residence time

vrt.Ar21 <- auc2.Ar21.corr/auc0.Ar21.corr - (auc1.Ar21.corr/auc0.Ar21.corr)^2

# or


# 1/k.up.herring.Ar10^2 + 1/k.el.herring.Ar10^2


# Summary of statistical moments

tbl.Ar21 <- data.frame("AUC0" = auc0.Ar21.corr,
                       "AUC1" = auc1.Ar21.corr,
                       "AUC2" = auc2.Ar21.corr,
                       "MRT" = mrt.Ar21,
                       "VRT" = vrt.Ar21)
tbl.Ar21

write.csv(tbl.Ar21, "mrt_ar21_sc4_shallow.csv")

mrt <- read_csv("MRT.csv") %>% 
  glimpse() 

mrt.plt <- mrt %>%
  mutate(Chem = recode(Chem,
                       "Ar10" = "C10-12",
                       "Ar12" = "C12-16",
                       "Ar16" = "C16-21",
                       "Ar21" = "C21-35")) %>%
  ggplot(aes(x = Chem, y = MRT)) +
  geom_bar(aes(fill = Chem),
           stat = "identity",
           position = "dodge") +
  facet_wrap(~Scenario,
             ncol = 4,
             scales = "free_x",
             labeller = "label_both") +
  labs(y = "Mean Residence Time in herring (h)",
       x = "") +
  expand_limits(y = c(0, 15)) +
  guides(fill=guide_legend(title="Aromatics")) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


auc.plt <- mrt %>%
  mutate(Chem = recode(Chem,
                       "Ar10" = "C10-12",
                       "Ar12" = "C12-16",
                       "Ar16" = "C16-21",
                       "Ar21" = "C21-35")) %>%
  ggplot(aes(x = Chem, y = AUC0)) +
  geom_bar(aes(fill = Chem),
           stat = "identity",
           position = "dodge") +
  facet_wrap(~Scenario,
             ncol = 4,
             scales = "free_x",
             labeller =  "label_both") +
  labs(y = expression("Area Under the Curve for herring"~(mg%*%h/kg)),
       x = "") +
  guides(fill=guide_legend(title="Aromatics")) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

vrt.plt <- mrt %>%
  mutate(Chem = recode(Chem,
                       "Ar10" = "C10-12",
                       "Ar12" = "C12-16",
                       "Ar16" = "C16-21",
                       "Ar21" = "C21-35")) %>%
  ggplot(aes(x = Chem, y = VRT)) +
  geom_bar(aes(fill = Chem),
           stat = "identity",
           position = "dodge") +
  facet_wrap(~Scenario,
             ncol = 4,
             scales = "free_x",
             labeller =  "label_both") +
  labs(y = expression("Variance of the Residence Time for herring"~(h^2)),
       x = "") +
  guides(fill=guide_legend(title="Aromatics")) +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


# Attempts to automate calculations----

# dat.new$Ar10
# 1:nrow(dat.new)
# dat.new$Time[0:21]
# 
# for(i in dat.new$Time){
#   
#   for (j in dat.new$Ar10) {
#     
#     fun.auc0 <- approxfun(x = dat.new$Time[i],
#                           y = dat.new$Ar10[j],
#                           method = "constant")
#   }
# }

# for (i in 8:ncol(dat.new)) {
# 
#   for (j in 1:nrow(dat.new)) {
# 
#     fun.auc0 <- (approxfun(x = dat.new$Time[j],
#                           y = dat.new[i],
#                           method = "constant"))
# 
#      str(fun.auc0(1))
# 
#   }
# }

# apply(dat.new[1:21, 8:11], approxfun(x = dat.new[1:21 , ],
#                                      y = dat.new[ , 8], 
#                                      method = "constant"))


# subset(dat.new, select = Ar10:Ar21)
# 
# for(i in subset(dat.new, select =  Ar10:Ar21)){
#   
#   approxfun(x = dat.new$Time,
#   y = dat.new[i], 
#   method = "constant")
#   
#   }
