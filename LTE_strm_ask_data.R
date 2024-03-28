library(tidyverse)
library(ggthemes)

theme_set(theme_bw())
theme_update(panel.grid = element_blank())

# LTE data ----

# data with SOC and N content at plot scale from Jensen 2022 & Jensen 2023

soil_plots <- read.table(
  "data_lte/Soil_parameters_Soil_C_N.txt",
  sep = "\t",
  dec = ".",
  header = T
) |>
  pivot_longer(
    cols = c(starts_with("C_"), starts_with("N_")),
    names_sep =  "_",
    names_to = c('variable', 'year')#,
    #values_to =c('C','N')
  ) |>
  pivot_wider(names_from = 'variable',
              values_from = 'value') |>
  mutate(Topsoil_C_obs=25*ifelse(year==1981,1.54,Bulk_Density_2020)*C,
         CN=C/N,
         year=as.numeric(year),
         year2=year*year,
         plotyear=interaction(Sample_ID,year))|>
  filter(Cover_Crop == 'Without')

# we expand the table for the two alternative calculations for
# C inputs and the simulation period

scn_plot <- expand.grid(
  "Allo" = c("Fixed", "PartFixedSt"),
  "plot" = unique(soil_plots$Sample_ID)
) |>
  mutate(id = row_number())

scn_plot_year <- merge(scn_plot, soil_plots, by.x = 'plot',
                       by.y = 'Sample_ID') |>
  mutate(plotyear = interaction(plot, year),
         scn_name =
           paste(Allo, plot, sep = "_"))

plot_est <-
  read.table("data_lte/plot_est.txt", sep = "\t", header = T) |>
  mutate(plotyear = interaction(Sample_ID, year)) |>
  mutate(
    Straw_DM = as.numeric(ifelse(
      between(year, 1951, 1979),
      3.242505,
      paste(Straw_DM)
    )),

    Grain_DM = as.numeric(ifelse(
      between(year, 1951, 1979),
      3.361815,
      paste(Grain_DM)
    )),
    Straw_Rate = as.numeric(ifelse(
      between(year, 1951, 1979),
      4,
      recode(
        Sample_ID,
        "201" = "0","606" = "0", "708" = "0",
        "208" = "4","301" = "4","706" = "4",
        "206" = "8","308" = "8","601" = "8",
        "608" = "12","306" = "12", "701" = "12"
      )
    ))
  )

# C inputs calculations ----

plot_Cinp <-
  merge(x = plot_est,
        y = scn_plot,
        by.x = 'Sample_ID',
        by.y = 'plot') |>
  mutate(
    RE = as.numeric(
      recode(
        Crop,
        "SpringBarley" = '0.8',
        'SpringWheat' = '0.7',
        'WinterWheat' = '0.7'
      )
    ),
    CB = as.numeric(
      recode(
        Crop,
        "SpringBarley" = '0.423',
        'SpringWheat' = '0.424',
        'WinterWheat' = '0.424'
      )
    ),
    SB = 0,

    RB = as.numeric(
      recode(
        Crop,
        "SpringBarley" = '0.17',
        'SpringWheat' = '0.25',
        'WinterWheat' = '0.25'
      )
    )
  ) |>
  mutate(HI = ifelse(
    Allo == "Fixed",
    0.45,
    (Grain_DM * CB) / (Straw_DM * CB + Grain_DM * CB + 0.96) # 0.96 for WW
  )) |>
  mutate(Cresid = 0.96 + Straw_Rate * CB,
         Cbelow = ifelse(Allo == "Fixed", 1.3,
                         (RB / ((
                           1 - RB
                         ) * HI) * (Grain_DM * CB)))) |>
  mutate(
    Ctop = Cresid + (RE * Cbelow),
    Csub = Cbelow * (1 - RE),
    Cman = Slurry_C,
    scn_name = interaction(Allo, Sample_ID),
    idyear = interaction(id, year),
    Crop = "Grain"
  ) |>
  arrange(id, year)
# check
table(plot_Cinp$Straw_Rate, plot_Cinp$Sample_ID)

plot_Cinp |>
  pivot_longer(cols = c(Ctop,Csub), names_to = "C_Source") |>
  group_by(year,C_Source,Sample_ID,Allo) |>
  summarise(mean = mean(value),
            sdDM=sd(value)#/sqrt(n())
  ) |>
  ggplot(aes(y=mean, x=year, col=Allo, shape=C_Source)) +
  scale_x_continuous(breaks = seq(1951,2019,3), minor_breaks = NULL)+
  geom_point() +
  scale_color_calc()+
  labs(y="C Input (Mg/ha)")+
  theme(axis.text.x = element_text(angle = 90), text=element_text(size=16),
        panel.background = NULL)

# Temperature ----

T_ave <- read.table("data_lte/temp_ask.txt")[,1]
T_range <- rep(c(4, 5, 6, 8, 9, 9, 9, 8, 7, 6, 5, 5),69)

# Soil data ----

## Fraction of manure that we consider is already Humidified
fman <-0.192

# Parameters referring to site-specific soil conditions

# Initial C stock at 1m depth
Cinit <- 1.41*20*1.54 + # 0-20 data from 1981
  0.8*1.55*30 + # 20-50 # Bulk density aand C% from landmarkensite report askov
  0.2*1.6*50  # 50-100 # Bulk density aand C% from landmarkensite report askov

# Proportion of the total C allocated in topsoil

Cproptop <- 1.41*25*1.54 /Cinit #0.47

clay_top <- 0.11

clay_sub <- (0.11*15+0.21*25+0.22*40)/80 # landmarkensite report askov

# Diffusion index
phi <- 0.054 #  JB 5 askov

# respiration fraction
fco2 <- 0.628

# romification fraction
fromi <- 0.012

## decomposition rates ----
kFOM <- 0.12

kHUM <- 0.0028

kROM <- 3.858e-05

# transport rate
ftr <- 0.003

# initial pool distribution  bjarne
fHUM_top <- 0.533

fROM_top <- 0.405

fHUM_sub <- 0.387

fROM_sub <- 0.61

# CN relation
CN_top <- 1.41/0.126

CN_sub <- 10 # defect
