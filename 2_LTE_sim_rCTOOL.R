#devtools::install("C:/Users/au710823/OneDrive - Aarhus universitet/rCTOOL", force = TRUE)
#devtools::install_github(repo="francagiannini/rCTOOL")

library(tidyverse)
library(rCTOOL)

# Parametrization ----

## Time period ----
period = rCTOOL::define_timeperiod(yr_start = 1951, yr_end = 2019)

## Managment an C inputs ----
## Monthly allocation
## Fraction of manure that we consider is already Humidified

management = management_config(
  f_man_humification = 0.12,
  manure_monthly_allocation = c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  plant_monthly_allocation = c(0, 0, 0, 8, 12, 16, 64, 0, 0, 0, 0, 0) / 100
) # set to default

# C input calculation has been made previously and is allocated in plot_Cinp

## Soil ----

# Initial C stock at 1m depth
Cinit_ask_sr <- 1.55*25*1.54 + # C % data from Askov average B5 and B3-field;depth, BD from Lermarken site
  0.8*1.55*25 + # 25-50 # Bulk density aand C% from landmarkensite report askov
  0.2*1.6*50  # 50-100 # Bulk density aand C% from landmarkensite report askov

# Proportion of the total C allocated in topsoil
Cproptop <- 1.55 *25*1.54/Cinit_ask_sr #

soil = soil_config(Csoil_init = Cinit_ask_sr, # Initial C stock at 1m depth
                   f_hum_top =  .4803,#0.533,#
                   f_rom_top =  .4881,#0.405,#
                   f_hum_sub =  .3123,#0.387,#
                   f_rom_sub =  .6847,#0.610,#
                   Cproptop = Cproptop, # landmarkensite report askov
                   clay_top = 0.117, # The mean clay content of the 12 specific plots was measured to 11.7% based on a sampling in 2020 (see Jensen et al., 2022).
                   clay_sub = (0.11*15+0.21*25+0.22*40)/80,
                   phi = 0.035,
                   f_co2 = 0.628,
                   f_romi = 0.012,
                   k_fom  = 0.12,
                   k_hum = 0.0028,
                   k_rom = 3.85e-5,
                   ftr = 0.0025
                   )

## Temperature ----

T_ave <- read.table("data_lte/temp_ask.txt")[,1]
T_range <- rep(c(4, 5, 6, 8, 9, 9, 9, 8, 7, 6, 5, 5),69)

temp = bind_cols(month=period$timeperiod$mon,
                 yr=period$timeperiod$yrs,
                 Tavg =T_ave,
                 Range = T_range)

data('scenario_temperature')

soil_pools = initialize_soil_pools(soil_config = soil,
                                   cn = 10 )

scn_list <- list()

# Run scenarios ----

for(id in 1:24) {

  cin_id = define_Cinputs(
    Cin_top = plot_Cinp[which(plot_Cinp$id == id),'Ctop'],
    Cin_sub = plot_Cinp[which(plot_Cinp$id == id),'Csub'],
    Cin_man = plot_Cinp[which(plot_Cinp$id == id),'Cman'],
    time_config = period
  )


  scn_list[[id]] <- run_ctool(time_config = period,
                              cin_config = cin_id,
                              m_config = management,
                              t_config = temp,
                              s_config = soil,
                              soil_pools = soil_pools) |>
    mutate(id_mod = paste(id))
}


plot_results <-
  do.call("rbind", scn_list) |> mutate(idyear=interaction(id_mod,yrs))

plot_results |>
  filter(mon == 12) |>  ggplot(aes(x=yrs,y=as.numeric(C_topsoil)))+
  geom_point()

plot_results_dec <-
  merge(plot_results,plot_Cinp , by='idyear')|>
  filter(mon == 12)

plot_results_oct <-
  merge(plot_results,plot_Cinp , by='idyear')|>
  filter(mon == 10)

plot_results_juan <-
  merge(plot_results,plot_Cinp , by='idyear')|>
  filter(mon == 1)

#oct81 <- plot_results_oct |> filter(year==1981) |> select(HUM_top,HUM_sub, ROM_top,ROM_sub, C_topsoil)
