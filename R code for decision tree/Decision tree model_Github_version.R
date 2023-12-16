########################################################################################################
# Title:          Decision Tree Model - CEA antimicrobials for CABP
# Date:           22 February 2023
# Updated:        16 December 2023
########################################################################################################

##### Description
# Building decision tree model for CEA using R


##### Clear memory
rm(list=ls())



##### Load functions needed for plotting a CE frontier
## Windows:
source("https://raw.githubusercontent.com/mbounthavong/Cost-effectiveness-of-antimicrobials-for-community-aquired-bacterial-pneumonia/main/R%20Functions/CEA_functions2.R")


#### Load libraries
library("tidyverse")
library("psych")


##################################################################################################################
########################################### Building a decision tree #############################################
##################################################################################################################

################################
#### Data inputs (parameters)
################################

strategies <- c("LEF", "LEV", "MOX", "CEF", "CLA")

### Probabilities
p.success_lef = 0.92972      # Probability of initial treatment success - LEF
p.success_lev = 0.93963      # Probability of initial treatment success - LEV
p.success_mox = 0.95104      # Probability of initial treatment success - MOX
p.success_cef = 0.88963      # Probability of initial treatment success - CEF
p.success_cla = 0.94950      # Probability of initial treatment success - CLA

p.hospitalized_lef = 0.303   # Probability of remained hospitalized - LEF
p.hospitalized_lev = 0.303   # Probability of remained hospitalized - LEV
p.hospitalized_mox = 0.303   # Probability of remained hospitalized - MOX
p.hospitalized_cef = 0.303   # Probability of remained hospitalized - CEF
p.hospitalized_cla = 0.303   # Probability of remained hospitalized - CLA

p.discharge_lef = 1 - p.hospitalized_lef   # Probability of discharged - LEF
p.discharge_lev = 1 - p.hospitalized_lev   # Probability of discharged - LEV
p.discharge_mox = 1 - p.hospitalized_mox   # Probability of discharged - MOX
p.discharge_cef = 1 - p.hospitalized_cef   # Probability of discharged - CEF
p.discharge_cla = 1 - p.hospitalized_cla   # Probability of discharged - CLA

p.failure_lef = 1 - p.success_lef   # Probability of initial treatment failure - LEF
p.failure_lev = 1 - p.success_lev   # Probability of initial treatment failure - LEV
p.failure_mox = 1 - p.success_mox   # Probability of initial treatment failure - MOX
p.failure_cef = 1 - p.success_cef   # Probability of initial treatment failure - CEF
p.failure_cla = 1 - p.success_cla   # Probability of initial treatment failure - CLA

p.delayed_lef = 0.93  # Probability of delayed response - LEF
p.delayed_lev = 0.86  # Probability of delayed response - LEV
p.delayed_mox = 0.91  # Probability of delayed response - MOX
p.delayed_cef = 0.91  # Probability of delayed response - CEF
p.delayed_cla = 0.92  # Probability of delayed response - CLA

p.switch_lef = 1 - p.delayed_lef  # Probability of failure and switch to 2nd line tx - LEF
p.switch_lev = 1 - p.delayed_lev  # Probability of failure and switch to 2nd line tx - LEV
p.switch_mox = 1 - p.delayed_mox  # Probability of failure and switch to 2nd line tx - MOX
p.switch_cef = 1 - p.delayed_cef  # Probability of failure and switch to 2nd line tx - CEF
p.switch_cla = 1 - p.delayed_lef  # Probability of failure and switch to 2nd line tx - CLA

p.icu_lef = 0.11    ## Probability of ICU admission - LEF
p.icu_lev = 0.11    ## Probability of ICU admission - LEV
p.icu_mox = 0.086   ## Probability of ICU admission - MOX
p.icu_cef = 0.07    ## Probability of ICU admission - CEF
p.icu_cla = 0.106   ## Probability of ICU admission - CLA

p.initiate_swtich_lef = 1 - p.icu_lef   ## Probability of initiating 2nd line treatment - LEF
p.initiate_swtich_lev = 1 - p.icu_lev   ## Probability of initiating 2nd line treatment - LEV
p.initiate_swtich_mox = 1 - p.icu_mox   ## Probability of initiating 2nd line treatment - MOX
p.initiate_swtich_cef = 1 - p.icu_cef   ## Probability of initiating 2nd line treatment - CEF
p.initiate_swtich_cla = 1 - p.icu_cla   ## Probability of initiating 2nd line treatment - CLA

p.cure_lef = 0.9  ## Probability of cured - LEF
p.cure_lev = 0.9  ## Probability of cured - LEV
p.cure_mox = 0.9  ## Probability of cured - MOX
p.cure_cef = 0.9  ## Probability of cured - CEF
p.cure_cla = 0.9  ## Probability of cured - CLA

p.dead_lef = 1 - p.cure_lef  ## Probability of dead - LEF
p.dead_lev = 1 - p.cure_lev  ## Probability of dead - LEV
p.dead_mox = 1 - p.cure_mox  ## Probability of dead - MOX
p.dead_cef = 1 - p.cure_cef  ## Probability of dead - CEF
p.dead_cla = 1 - p.cure_cla  ## Probability of dead - CLA


### Length of stay
los.succes_dx_lef = 4.9  ## Average general ward LOS, treatment success (days) - LEF
los.succes_dx_lev = 4.9  ## Average general ward LOS, treatment success (days) - LEV
los.succes_dx_mox = 4.9  ## Average general ward LOS, treatment success (days) - MOX
los.succes_dx_cef = 4.9  ## Average general ward LOS, treatment success (days) - CEF
los.succes_dx_cla = 4.9  ## Average general ward LOS, treatment success (days) - CLA

los.succes_lef = 4.2  ## Average general ward LOS discharged, treatment success (days) - LEF
los.succes_lev = 4.2  ## Average general ward LOS discharged, treatment success (days) - LEV
los.succes_mox = 4.2  ## Average general ward LOS discharged, treatment success (days) - MOX
los.succes_cef = 4.2  ## Average general ward LOS discharged, treatment success (days) - CEF
los.succes_cla = 4.2  ## Average general ward LOS discharged, treatment success (days) - CLA

los.initial_failure_lef = 4.2  ## Average general ward LOS discharged, treatment success (days) - LEF
los.initial_failure_lev = 4.2  ## Average general ward LOS discharged, treatment success (days) - LEV
los.initial_failure_mox = 4.2  ## Average general ward LOS discharged, treatment success (days) - MOX
los.initial_failure_cef = 4.2  ## Average general ward LOS discharged, treatment success (days) - CEF
los.initial_failure_cla = 4.2  ## Average general ward LOS discharged, treatment success (days) - CLA

los.failure_lef = 10.1 ## Average general ward LOS, treatment failure (days) - LEF
los.failure_lev = 10.1 ## Average general ward LOS, treatment failure (days) - LEV
los.failure_mox = 10.1 ## Average general ward LOS, treatment failure (days) - MOX
los.failure_cef = 10.1 ## Average general ward LOS, treatment failure (days) - CEF
los.failure_cla = 10.1 ## Average general ward LOS, treatment failure (days) - CLA

los.icu_lef = 3  ## Average ICU LOS (days) - LEF
los.icu_lev = 3  ## Average ICU LOS (days) - LEV
los.icu_mox = 3  ## Average ICU LOS (days) - MOX
los.icu_cef = 3  ## Average ICU LOS (days) - CEF
los.icu_cla = 3  ## Average ICU LOS (days) - CLA

tx.duration_lef = 5  ## Average treatment duration - LEF
tx.duration_lev = 5  ## Average treatment duration - LEV
tx.duration_mox = 5  ## Average treatment duration - MOX
tx.duration_cef = 5  ## Average treatment duration - CEF
tx.duration_cla = 5  ## Average treatment duration - CLA


### Costs
c.lef = 254.27      ## Cost of PO LEF (600 mg Q12H)
c.lev = 0.22        ## Cost of PO LEV (750 mg QD)
c.lev_iv = 79.36    ## Cost of IV LEV (500 mg QD)
c.mox = 2.35        ## Cost of PO MOX (400 mg QD)
c.mox_iv = 40.42    ## Cost of IV MOX (400 mg QD)
c.cef_iv = 2.86     ## Cost of IV CEF (2000 mg QD)
c.cla = 9.63        ## Cost of PO CLA (500 mg BID)
c.second = 20708    ## Cost of second line treatment

c.dr_los = 198.29   ## Cost of physician visit in hospital
c.inp_los = 2823    ## Cost of one-night in the hospital
c.los = (c.dr_los + c.inp_los)  ## Cost of hospital stay - initial high complexity

c.dr_icu = 213.83   ## Cost of physician visit in ICU
c.inp_icu = 3895    ## Cost of one-night in the ICU
c.icu = (c.dr_icu + c.inp_icu)  ## Cost of ICU (0 - 60 min) 

c.dr_x_ray = 34.61  ## Cost of physician exam of X-ray (2 views)
c.proc_xray = 81.59 ## Cost of X-ray machine
c.x_ray = (c.dr_x_ray + c.proc_xray)  ## Cost of chest X-ray

c.cbc = 7.77        ## Cost of CBC labs
c.bmp = 8.46        ## Cost of BMP
c.sputum = 9.47     ## Cost of sputum culture
c.blood = 10.32     ## Cost of blood culture


### Total costs
tc.lef_1 = c.lef*tx.duration_lef + (c.dr_los + c.inp_los)*los.succes_dx_lef + (c.cbc + c.bmp + c.sputum + c.blood)*tx.duration_lef + 1*(c.dr_x_ray + c.proc_xray)
tc.lef_2 = c.lef*tx.duration_lef + (c.dr_los + c.inp_los)*los.succes_lef + (c.cbc + c.bmp + c.sputum + c.blood)*los.succes_lef + 1*(c.dr_x_ray + c.proc_xray)
tc.lef_3 = c.lef*tx.duration_lef + (c.dr_los + c.inp_los)*los.failure_lef + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_lef + 1*(c.dr_x_ray + c.proc_xray)
tc.lef_4 = c.lef*tx.duration_lef + (c.dr_los + c.inp_los)*los.failure_lef + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_lef + 1*(c.dr_x_ray + c.proc_xray)
tc.lef_5 = c.lef*tx.duration_lef + (c.dr_los + c.inp_los)*los.initial_failure_lef + (c.dr_icu + c.inp_icu)*los.icu_lef + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_lef + los.icu_lef) + 1*(c.dr_x_ray + c.proc_xray)
tc.lef_6 = c.lef*tx.duration_lef + (c.dr_los + c.inp_los)*los.initial_failure_lef + (c.dr_icu + c.inp_icu)*los.icu_lef + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_lef + los.icu_lef) + 1*(c.dr_x_ray + c.proc_xray)

tc.lev_1 = c.lev*tx.duration_lev + (c.dr_los + c.inp_los)*los.succes_dx_lev + (c.cbc + c.bmp + c.sputum + c.blood)*tx.duration_lev + 1*(c.dr_x_ray + c.proc_xray)
tc.lev_2 = c.lev*tx.duration_lev + (c.dr_los + c.inp_los)*los.succes_lev + (c.cbc + c.bmp + c.sputum + c.blood)*los.succes_lev + 1*(c.dr_x_ray + c.proc_xray)
tc.lev_3 = c.lev*tx.duration_lev + (c.dr_los + c.inp_los)*los.failure_lev + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_lev + 1*(c.dr_x_ray + c.proc_xray)
tc.lev_4 = c.lev*tx.duration_lev + (c.dr_los + c.inp_los)*los.failure_lev + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_lev + 1*(c.dr_x_ray + c.proc_xray)
tc.lev_5 = c.lev*tx.duration_lev + (c.dr_los + c.inp_los)*los.initial_failure_lev + (c.dr_icu + c.inp_icu)*los.icu_lev + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_lev + los.icu_lev) + 1*(c.dr_x_ray + c.proc_xray)
tc.lev_6 = c.lev*tx.duration_lev + (c.dr_los + c.inp_los)*los.initial_failure_lev + (c.dr_icu + c.inp_icu)*los.icu_lev + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_lev + los.icu_lev) + 1*(c.dr_x_ray + c.proc_xray)

tc.mox_1 = c.mox*tx.duration_mox + (c.dr_los + c.inp_los)*los.succes_dx_mox + (c.cbc + c.bmp + c.sputum + c.blood)*tx.duration_mox + 1*(c.dr_x_ray + c.proc_xray)
tc.mox_2 = c.mox*tx.duration_mox + (c.dr_los + c.inp_los)*los.succes_mox + (c.cbc + c.bmp + c.sputum + c.blood)*los.succes_mox + 1*(c.dr_x_ray + c.proc_xray)
tc.mox_3 = c.mox*tx.duration_mox + (c.dr_los + c.inp_los)*los.failure_mox + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_mox + 1*(c.dr_x_ray + c.proc_xray)
tc.mox_4 = c.mox*tx.duration_mox + (c.dr_los + c.inp_los)*los.failure_mox + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_mox + 1*(c.dr_x_ray + c.proc_xray)
tc.mox_5 = c.mox*tx.duration_mox + (c.dr_los + c.inp_los)*los.initial_failure_mox + (c.dr_icu + c.inp_icu)*los.icu_mox + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_mox + los.icu_mox) + 1*(c.dr_x_ray + c.proc_xray)
tc.mox_6 = c.mox*tx.duration_mox + (c.dr_los + c.inp_los)*los.initial_failure_mox + (c.dr_icu + c.inp_icu)*los.icu_mox + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_mox + los.icu_mox) + 1*(c.dr_x_ray + c.proc_xray)

tc.cef_1 = c.cef_iv*tx.duration_cef + (c.dr_los + c.inp_los)*los.succes_dx_cef + (c.cbc + c.bmp + c.sputum + c.blood)*tx.duration_cef + 1*(c.dr_x_ray + c.proc_xray)
tc.cef_2 = c.cef_iv*tx.duration_cef + (c.dr_los + c.inp_los)*los.succes_cef + (c.cbc + c.bmp + c.sputum + c.blood)*los.succes_cef + 1*(c.dr_x_ray + c.proc_xray)
tc.cef_3 = c.cef_iv*tx.duration_cef + (c.dr_los + c.inp_los)*los.failure_cef + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_cef + 1*(c.dr_x_ray + c.proc_xray)
tc.cef_4 = c.cef_iv*tx.duration_cef + (c.dr_los + c.inp_los)*los.failure_cef + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_cef + 1*(c.dr_x_ray + c.proc_xray)
tc.cef_5 = c.cef_iv*tx.duration_cef + (c.dr_los + c.inp_los)*los.initial_failure_cef + (c.dr_icu + c.inp_icu)*los.icu_cef + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_cef + los.icu_cef) + 1*(c.dr_x_ray + c.proc_xray)
tc.cef_6 = c.cef_iv*tx.duration_cef + (c.dr_los + c.inp_los)*los.initial_failure_cef + (c.dr_icu + c.inp_icu)*los.icu_cef + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_cef + los.icu_cef) + 1*(c.dr_x_ray + c.proc_xray)

tc.cla_1 = c.cla*tx.duration_cla + (c.dr_los + c.inp_los)*los.succes_dx_cla + (c.cbc + c.bmp + c.sputum + c.blood)*tx.duration_cla + 1*(c.dr_x_ray + c.proc_xray)
tc.cla_2 = c.cla*tx.duration_cla + (c.dr_los + c.inp_los)*los.succes_cla + (c.cbc + c.bmp + c.sputum + c.blood)*los.succes_cla + 1*(c.dr_x_ray + c.proc_xray)
tc.cla_3 = c.cla*tx.duration_cla + (c.dr_los + c.inp_los)*los.failure_cla + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_cla + 1*(c.dr_x_ray + c.proc_xray)
tc.cla_4 = c.cla*tx.duration_cla + (c.dr_los + c.inp_los)*los.failure_cla + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_cla + 1*(c.dr_x_ray + c.proc_xray)
tc.cla_5 = c.cla*tx.duration_cla + (c.dr_los + c.inp_los)*los.initial_failure_cla + (c.dr_icu + c.inp_icu)*los.icu_cla + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_cla + los.icu_cla) + 1*(c.dr_x_ray + c.proc_xray)
tc.cla_6 = c.cla*tx.duration_cla + (c.dr_los + c.inp_los)*los.initial_failure_cla + (c.dr_icu + c.inp_icu)*los.icu_cla + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_cla + los.icu_cla) + 1*(c.dr_x_ray + c.proc_xray)


### Utilities
u.hospitalization = 0.85  ## Utility value for CAP hospitalization
u.discharge = 0.92        ## Utility value for discharge after treatment success
u.failure = 0.61          ## Utility value after treatment failure
u.death = 0               ## Utility value for death

wtp    = 100000          # Willingness to pay per QALY gained

### Total utilities
util.lef_1 = u.hospitalization*(tx.duration_lef/365.25)
util.lef_2 = u.discharge*(tx.duration_lef/365.25)
util.lef_3 = u.failure*(los.failure_lef/365.25) + u.hospitalization*(los.failure_lef/365.25)
util.lef_4 = u.failure*(los.failure_lef/365.25) + u.hospitalization*(los.failure_lef/365.25)
util.lef_5 = u.failure*(los.failure_lef/365.25) + u.hospitalization*(los.failure_lef/365.25)
util.lef_6 = u.death

util.lev_1 = u.hospitalization*(tx.duration_lev/365.25)
util.lev_2 = u.discharge*(tx.duration_lev/365.25)
util.lev_3 = u.failure*(los.failure_lev/365.25) + u.hospitalization*(los.failure_lev/365.25)
util.lev_4 = u.failure*(los.failure_lev/365.25) + u.hospitalization*(los.failure_lev/365.25)
util.lev_5 = u.failure*(los.failure_lev/365.25) + u.hospitalization*(los.failure_lev/365.25)
util.lev_6 = u.death

util.mox_1 = u.hospitalization*(tx.duration_mox/365.25)
util.mox_2 = u.discharge*(tx.duration_mox/365.25)
util.mox_3 = u.failure*(los.failure_mox/365.25) + u.hospitalization*(los.failure_mox/365.25)
util.mox_4 = u.failure*(los.failure_mox/365.25) + u.hospitalization*(los.failure_mox/365.25)
util.mox_5 = u.failure*(los.failure_mox/365.25) + u.hospitalization*(los.failure_mox/365.25)
util.mox_6 = u.death

util.cef_1 = u.hospitalization*(tx.duration_cef/365.25)
util.cef_2 = u.discharge*(tx.duration_cef/365.25)
util.cef_3 = u.failure*(los.failure_cef/365.25) + u.hospitalization*(los.failure_cef/365.25)
util.cef_4 = u.failure*(los.failure_cef/365.25) + u.hospitalization*(los.failure_cef/365.25)
util.cef_5 = u.failure*(los.failure_cef/365.25) + u.hospitalization*(los.failure_cef/365.25)
util.cef_6 = u.death

util.cla_1 = u.hospitalization*(tx.duration_cla/365.25)
util.cla_2 = u.discharge*(tx.duration_cla/365.25)
util.cla_3 = u.failure*(los.failure_cla/365.25) + u.hospitalization*(los.failure_cla/365.25)
util.cla_4 = u.failure*(los.failure_cla/365.25) + u.hospitalization*(los.failure_cla/365.25)
util.cla_5 = u.failure*(los.failure_cla/365.25) + u.hospitalization*(los.failure_cla/365.25)
util.cla_6 = u.death



################################
#### Vector of parameter inputs
################################
input <- data.frame(
  p.success_lef = 0.92972,      # Probability of initial treatment success - LEF
  p.success_lev = 0.93963,      # Probability of initial treatment success - LEV
  p.success_mox = 0.95104,      # Probability of initial treatment success - MOX
  p.success_cef = 0.88963,      # Probability of initial treatment success - CEF
  p.success_cla = 0.94950,      # Probability of initial treatment success - CLA
  
  p.hospitalized_lef = 0.3028,   # Probability of remained hospitalized - LEF
  p.hospitalized_lev = 0.3028,   # Probability of remained hospitalized - LEV
  p.hospitalized_mox = 0.3028,   # Probability of remained hospitalized - MOX
  p.hospitalized_cef = 0.3028,   # Probability of remained hospitalized - CEF
  p.hospitalized_cla = 0.3028,   # Probability of remained hospitalized - CLA
  
  p.discharge_lef = 1 - p.hospitalized_lef,   # Probability of discharged - LEF
  p.discharge_lev = 1 - p.hospitalized_lev,   # Probability of discharged - LEV
  p.discharge_mox = 1 - p.hospitalized_mox,   # Probability of discharged - MOX
  p.discharge_cef = 1 - p.hospitalized_cef,   # Probability of discharged - CEF
  p.discharge_cla = 1 - p.hospitalized_cla,   # Probability of discharged - CLA
  
  p.failure_lef = 1 - p.success_lef,   # Probability of initial treatment failure - LEF
  p.failure_lev = 1 - p.success_lev,   # Probability of initial treatment failure - LEV
  p.failure_mox = 1 - p.success_mox,   # Probability of initial treatment failure - MOX
  p.failure_cef = 1 - p.success_cef,   # Probability of initial treatment failure - CEF
  p.failure_cla = 1 - p.success_cla,   # Probability of initial treatment failure - CLA
  
  p.delayed_lef = 0.93,  # Probability of delayed response - LEF
  p.delayed_lev = 0.86,  # Probability of delayed response - LEV
  p.delayed_mox = 0.91,  # Probability of delayed response - MOX
  p.delayed_cef = 0.91,  # Probability of delayed response - CEF
  p.delayed_cla = 0.92,  # Probability of delayed response - CLA
  
  p.switch_lef = 1 - p.delayed_lef,  # Probability of failure and switch to 2nd line tx - LEF
  p.switch_lev = 1 - p.delayed_lev,  # Probability of failure and switch to 2nd line tx - LEV
  p.switch_mox = 1 - p.delayed_mox,  # Probability of failure and switch to 2nd line tx - MOX
  p.switch_cef = 1 - p.delayed_cef,  # Probability of failure and switch to 2nd line tx - CEF
  p.switch_cla = 1 - p.delayed_lef,  # Probability of failure and switch to 2nd line tx - CLA
  
  p.icu_lef = 0.11,    ## Probability of ICU admission - LEF
  p.icu_lev = 0.11,    ## Probability of ICU admission - LEV
  p.icu_mox = 0.086,   ## Probability of ICU admission - MOX
  p.icu_cef = 0.07,    ## Probability of ICU admission - CEF
  p.icu_cla = 0.106,   ## Probability of ICU admission - CLA
  
  p.initiate_swtich_lef = 1 - p.icu_lef,   ## Probability of initiating 2nd line treatment - LEF
  p.initiate_swtich_lev = 1 - p.icu_lev,   ## Probability of initiating 2nd line treatment - LEV
  p.initiate_swtich_mox = 1 - p.icu_mox,   ## Probability of initiating 2nd line treatment - MOX
  p.initiate_swtich_cef = 1 - p.icu_cef,   ## Probability of initiating 2nd line treatment - CEF
  p.initiate_swtich_cla = 1 - p.icu_cla,   ## Probability of initiating 2nd line treatment - CLA
  
  p.cure_lef = 0.9,  ## Probability of cured - LEF
  p.cure_lev = 0.9,  ## Probability of cured - LEV
  p.cure_mox = 0.9,  ## Probability of cured - MOX
  p.cure_cef = 0.9,  ## Probability of cured - CEF
  p.cure_cla = 0.9,  ## Probability of cured - CLA
  
  p.dead_lef = 1 - p.cure_lef,  ## Probability of dead - LEF
  p.dead_lev = 1 - p.cure_lev,  ## Probability of dead - LEV
  p.dead_mox = 1 - p.cure_mox,  ## Probability of dead - MOX
  p.dead_cef = 1 - p.cure_cef,  ## Probability of dead - CEF
  p.dead_cla = 1 - p.cure_cla,  ## Probability of dead - CLA
  
  
  ### Length of stay
  los.succes_dx_lef = 4.9,  ## Average general ward LOS, treatment success (days) - LEF
  los.succes_dx_lev = 4.9,  ## Average general ward LOS, treatment success (days) - LEV
  los.succes_dx_mox = 4.9,  ## Average general ward LOS, treatment success (days) - MOX
  los.succes_dx_cef = 4.9,  ## Average general ward LOS, treatment success (days) - CEF
  los.succes_dx_cla = 4.9,  ## Average general ward LOS, treatment success (days) - CLA
  
  los.succes_lef = 4.2,  ## Average general ward LOS discharged, treatment success (days) - LEF
  los.succes_lev = 4.2,  ## Average general ward LOS discharged, treatment success (days) - LEV
  los.succes_mox = 4.2,  ## Average general ward LOS discharged, treatment success (days) - MOX
  los.succes_cef = 4.2,  ## Average general ward LOS discharged, treatment success (days) - CEF
  los.succes_cla = 4.2,  ## Average general ward LOS discharged, treatment success (days) - CLA
  
  los.initial_failure_lef = 4.2,  ## Average general ward LOS discharged, treatment success (days) - LEF
  los.initial_failure_lev = 4.2,  ## Average general ward LOS discharged, treatment success (days) - LEV
  los.initial_failure_mox = 4.2,  ## Average general ward LOS discharged, treatment success (days) - MOX
  los.initial_failure_cef = 4.2,  ## Average general ward LOS discharged, treatment success (days) - CEF
  los.initial_failure_cla = 4.2,  ## Average general ward LOS discharged, treatment success (days) - CLA
  
  los.icu_lef = 3,  ## Average ICU LOS (days) - LEF
  los.icu_lev = 3,  ## Average ICU LOS (days) - LEV
  los.icu_mox = 3,  ## Average ICU LOS (days) - MOX
  los.icu_cef = 3,  ## Average ICU LOS (days) - CEF
  los.icu_cla = 3,  ## Average ICU LOS (days) - CLA
  
  tx.duration_LEF = 5,  ## Average treatment duration - LEF
  tx.duration_LEV = 5,  ## Average treatment duration - LEV
  tx.duration_MOX = 5,  ## Average treatment duration - MOX
  tx.duration_CEF = 5,  ## Average treatment duration - CEF
  tx.duration_CLA = 5,  ## Average treatment duration - CLA
  
  
  ### Costs
  c.lef = 254.27,      ## Cost of PO LEF (600 mg Q12H)
  c.lev = 0.22,        ## Cost of PO LEV (500 mg QD)
  c.lev_iv = 79.36,    ## Cost of IV LEV (500 mg QD)
  c.mox = 2.35,        ## Cost of PO MOX (400 mg QD)
  c.mox_iv = 40.42,    ## Cost of IV MOX (400 mg QD)
  c.cef_iv = 2.86,     ## Cost of IV CEF (2000 mg QD)
  c.cla = 9.63,        ## Cost of PO CLA (500 mg BID)
  
  c.second = 20708,    ## Cost of second line treatment
  
  c.dr_los = 198.29,   ## Cost of physician visit in hospital
  c.inp_los = 2823,    ## Cost of one-night in the hospital
  c.los = (c.dr_los + c.inp_los),  ## Cost of hospital stay - initial high complexity
  
  c.dr_icu = 213.83,   ## Cost of physician visit in ICU
  c.inp_icu = 3895,    ## Cost of one-night in the ICU
  c.icu = (c.dr_icu + c.inp_icu),  ## Cost of ICU (0 - 60 min) 
  
  c.dr_x_ray = 34.61,  ## Cost of physician exam of X-ray (2 views)
  c.proc_xray = 81.59, ## Cost of X-ray machine
  c.x_ray = (c.dr_x_ray + c.proc_xray),  ## Cost of chest X-ray
  
  c.cbc = 7.77,        ## Cost of CBC labs
  c.bmp = 8.46,        ## Cost of BMP
  c.sputum = 9.47,     ## Cost of sputum culture
  c.blood = 10.32,     ## Cost of blood culture


  
  ### Total costs
  tc.lef_1 = c.lef*tx.duration_lef + (c.dr_los + c.inp_los)*los.succes_dx_lef + (c.cbc + c.bmp + c.sputum + c.blood)*tx.duration_lef + 1*(c.dr_x_ray + c.proc_xray),
  tc.lef_2 = c.lef*tx.duration_lef + (c.dr_los + c.inp_los)*los.succes_lef + (c.cbc + c.bmp + c.sputum + c.blood)*los.succes_lef + 1*(c.dr_x_ray + c.proc_xray),
  tc.lef_3 = c.lef*tx.duration_lef + (c.dr_los + c.inp_los)*los.failure_lef + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_lef + 1*(c.dr_x_ray + c.proc_xray),
  tc.lef_4 = c.lef*tx.duration_lef + (c.dr_los + c.inp_los)*los.failure_lef + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_lef + 1*(c.dr_x_ray + c.proc_xray),
  tc.lef_5 = c.lef*tx.duration_lef + (c.dr_los + c.inp_los)*los.initial_failure_lef + (c.dr_icu + c.inp_icu)*los.icu_lef + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_lef + los.icu_lef) + 1*(c.dr_x_ray + c.proc_xray),
  tc.lef_6 = c.lef*tx.duration_lef + (c.dr_los + c.inp_los)*los.initial_failure_lef + (c.dr_icu + c.inp_icu)*los.icu_lef + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_lef + los.icu_lef) + 1*(c.dr_x_ray + c.proc_xray),
  
  tc.lev_1 = c.lev*tx.duration_lev + (c.dr_los + c.inp_los)*los.succes_dx_lev + (c.cbc + c.bmp + c.sputum + c.blood)*tx.duration_lev + 1*(c.dr_x_ray + c.proc_xray),
  tc.lev_2 = c.lev*tx.duration_lev + (c.dr_los + c.inp_los)*los.succes_lev + (c.cbc + c.bmp + c.sputum + c.blood)*los.succes_lev + 1*(c.dr_x_ray + c.proc_xray),
  tc.lev_3 = c.lev*tx.duration_lev + (c.dr_los + c.inp_los)*los.failure_lev + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_lev + 1*(c.dr_x_ray + c.proc_xray),
  tc.lev_4 = c.lev*tx.duration_lev + (c.dr_los + c.inp_los)*los.failure_lev + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_lev + 1*(c.dr_x_ray + c.proc_xray),
  tc.lev_5 = c.lev*tx.duration_lev + (c.dr_los + c.inp_los)*los.initial_failure_lev + (c.dr_icu + c.inp_icu)*los.icu_lev + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_lev + los.icu_lev) + 1*(c.dr_x_ray + c.proc_xray),
  tc.lev_6 = c.lev*tx.duration_lev + (c.dr_los + c.inp_los)*los.initial_failure_lev + (c.dr_icu + c.inp_icu)*los.icu_lev + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_lev + los.icu_lev) + 1*(c.dr_x_ray + c.proc_xray),
  
  tc.mox_1 = c.mox*tx.duration_mox + (c.dr_los + c.inp_los)*los.succes_dx_mox + (c.cbc + c.bmp + c.sputum + c.blood)*tx.duration_mox + 1*(c.dr_x_ray + c.proc_xray),
  tc.mox_2 = c.mox*tx.duration_mox + (c.dr_los + c.inp_los)*los.succes_mox + (c.cbc + c.bmp + c.sputum + c.blood)*los.succes_mox + 1*(c.dr_x_ray + c.proc_xray),
  tc.mox_3 = c.mox*tx.duration_mox + (c.dr_los + c.inp_los)*los.failure_mox + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_mox + 1*(c.dr_x_ray + c.proc_xray),
  tc.mox_4 = c.mox*tx.duration_mox + (c.dr_los + c.inp_los)*los.failure_mox + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_mox + 1*(c.dr_x_ray + c.proc_xray),
  tc.mox_5 = c.mox*tx.duration_mox + (c.dr_los + c.inp_los)*los.initial_failure_mox + (c.dr_icu + c.inp_icu)*los.icu_mox + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_mox + los.icu_mox) + 1*(c.dr_x_ray + c.proc_xray),
  tc.mox_6 = c.mox*tx.duration_mox + (c.dr_los + c.inp_los)*los.initial_failure_mox + (c.dr_icu + c.inp_icu)*los.icu_mox + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_mox + los.icu_mox) + 1*(c.dr_x_ray + c.proc_xray),
  
  tc.cef_1 = c.cef_iv*tx.duration_cef + (c.dr_los + c.inp_los)*los.succes_dx_cef + (c.cbc + c.bmp + c.sputum + c.blood)*tx.duration_cef + 1*(c.dr_x_ray + c.proc_xray),
  tc.cef_2 = c.cef_iv*tx.duration_cef + (c.dr_los + c.inp_los)*los.succes_cef + (c.cbc + c.bmp + c.sputum + c.blood)*los.succes_cef + 1*(c.dr_x_ray + c.proc_xray),
  tc.cef_3 = c.cef_iv*tx.duration_cef + (c.dr_los + c.inp_los)*los.failure_cef + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_cef + 1*(c.dr_x_ray + c.proc_xray),
  tc.cef_4 = c.cef_iv*tx.duration_cef + (c.dr_los + c.inp_los)*los.failure_cef + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_cef + 1*(c.dr_x_ray + c.proc_xray),
  tc.cef_5 = c.cef_iv*tx.duration_cef + (c.dr_los + c.inp_los)*los.initial_failure_cef + (c.dr_icu + c.inp_icu)*los.icu_cef + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_cef + los.icu_cef) + 1*(c.dr_x_ray + c.proc_xray),
  tc.cef_6 = c.cef_iv*tx.duration_cef + (c.dr_los + c.inp_los)*los.initial_failure_cef + (c.dr_icu + c.inp_icu)*los.icu_cef + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_cef + los.icu_cef) + 1*(c.dr_x_ray + c.proc_xray),
  
  tc.cla_1 = c.cla*tx.duration_cla + (c.dr_los + c.inp_los)*los.succes_dx_cla + (c.cbc + c.bmp + c.sputum + c.blood)*tx.duration_cla + 1*(c.dr_x_ray + c.proc_xray),
  tc.cla_2 = c.cla*tx.duration_cla + (c.dr_los + c.inp_los)*los.succes_cla + (c.cbc + c.bmp + c.sputum + c.blood)*los.succes_cla + 1*(c.dr_x_ray + c.proc_xray),
  tc.cla_3 = c.cla*tx.duration_cla + (c.dr_los + c.inp_los)*los.failure_cla + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_cla + 1*(c.dr_x_ray + c.proc_xray),
  tc.cla_4 = c.cla*tx.duration_cla + (c.dr_los + c.inp_los)*los.failure_cla + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_cla + 1*(c.dr_x_ray + c.proc_xray),
  tc.cla_5 = c.cla*tx.duration_cla + (c.dr_los + c.inp_los)*los.initial_failure_cla + (c.dr_icu + c.inp_icu)*los.icu_cla + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_cla + los.icu_cla) + 1*(c.dr_x_ray + c.proc_xray),
  tc.cla_6 = c.cla*tx.duration_cla + (c.dr_los + c.inp_los)*los.initial_failure_cla + (c.dr_icu + c.inp_icu)*los.icu_cla + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_cla + los.icu_cla) + 1*(c.dr_x_ray + c.proc_xray),
  
  
  
  ### Utilities
  u.hospitalization = 0.85,  ## Utility value for CAP hospitalization
  u.discharge = 0.92,        ## Utility value for discharge after treatment success
  u.failure = 0.61,          ## Utility value after treatment failure
  u.death = 0,               ## Utility value for death
  
  wtp    = 100000,          # Willingness to pay per QALY gained
  
  
  ### Total utilities
  util.lef_1 = u.hospitalization*(tx.duration_lef/365.25),
  util.lef_2 = u.discharge*(tx.duration_lef/365.25),
  util.lef_3 = u.failure*(los.failure_lef/365.25) + u.hospitalization*(los.failure_lef/365.25),
  util.lef_4 = u.failure*(los.failure_lef/365.25) + u.hospitalization*(los.failure_lef/365.25),
  util.lef_5 = u.failure*(los.failure_lef/365.25) + u.hospitalization*(los.failure_lef/365.25),
  util.lef_6 = u.death,
  
  util.lev_1 = u.hospitalization*(tx.duration_lev/365.25),
  util.lev_2 = u.discharge*(tx.duration_lev/365.25),
  util.lev_3 = u.failure*(los.failure_lev/365.25) + u.hospitalization*(los.failure_lev/365.25),
  util.lev_4 = u.failure*(los.failure_lev/365.25) + u.hospitalization*(los.failure_lev/365.25),
  util.lev_5 = u.failure*(los.failure_lev/365.25) + u.hospitalization*(los.failure_lev/365.25),
  util.lev_6 = u.death,
  
  util.mox_1 = u.hospitalization*(tx.duration_mox/365.25),
  util.mox_2 = u.discharge*(tx.duration_mox/365.25),
  util.mox_3 = u.failure*(los.failure_mox/365.25) + u.hospitalization*(los.failure_mox/365.25),
  util.mox_4 = u.failure*(los.failure_mox/365.25) + u.hospitalization*(los.failure_mox/365.25),
  util.mox_5 = u.failure*(los.failure_mox/365.25) + u.hospitalization*(los.failure_mox/365.25),
  util.mox_6 = u.death,
  
  util.cef_1 = u.hospitalization*(tx.duration_cef/365.25),
  util.cef_2 = u.discharge*(tx.duration_cef/365.25),
  util.cef_3 = u.failure*(los.failure_cef/365.25) + u.hospitalization*(los.failure_cef/365.25),
  util.cef_4 = u.failure*(los.failure_cef/365.25) + u.hospitalization*(los.failure_cef/365.25),
  util.cef_5 = u.failure*(los.failure_cef/365.25) + u.hospitalization*(los.failure_cef/365.25),
  util.cef_6 = u.death,
  
  util.cla_1 = u.hospitalization*(tx.duration_cla/365.25),
  util.cla_2 = u.discharge*(tx.duration_cla/365.25),
  util.cla_3 = u.failure*(los.failure_cla/365.25) + u.hospitalization*(los.failure_cla/365.25),
  util.cla_4 = u.failure*(los.failure_cla/365.25) + u.hospitalization*(los.failure_cla/365.25),
  util.cla_5 = u.failure*(los.failure_cla/365.25) + u.hospitalization*(los.failure_cla/365.25),
  util.cla_6 = u.death
  
  
  )




##################################################
#### OPEN TREE Method -- Decision Tree ####
##################################################

### Expected costs
C.LEF <- c(prod(c(p.success_lef*p.hospitalized_lef)),
           prod(c(p.success_lef*p.discharge_lef)),
           prod(c(p.failure_lef*p.delayed_lef)),
           prod(c(p.failure_lef*p.switch_lef*p.initiate_swtich_lef)),
           prod(c(p.failure_lef*p.switch_lef*p.icu_lef*p.cure_lef)),
           prod(c(p.failure_lef*p.switch_lef*p.icu_lef*p.dead_lef))) %*%
         c(tc.lef_1, 
           tc.lef_2, 
           tc.lef_3, 
           tc.lef_4, 
           tc.lef_5, 
           tc.lef_6)

C.LEV <- c(prod(c(p.success_lev*p.hospitalized_lev)),
           prod(c(p.success_lev*p.discharge_lev)),
           prod(c(p.failure_lev*p.delayed_lev)),
           prod(c(p.failure_lev*p.switch_lev*p.initiate_swtich_lev)),
           prod(c(p.failure_lev*p.switch_lev*p.icu_lev*p.cure_lev)),
           prod(c(p.failure_lev*p.switch_lev*p.icu_lev*p.dead_lev))) %*%
         c(tc.lev_1, 
           tc.lev_2, 
           tc.lev_3, 
           tc.lev_4, 
           tc.lev_5, 
           tc.lev_6)

C.MOX <- c(prod(c(p.success_mox*p.hospitalized_mox)),
           prod(c(p.success_mox*p.discharge_mox)),
           prod(c(p.failure_mox*p.delayed_mox)),
           prod(c(p.failure_mox*p.switch_mox*p.initiate_swtich_mox)),
           prod(c(p.failure_mox*p.switch_mox*p.icu_mox*p.cure_mox)),
           prod(c(p.failure_mox*p.switch_mox*p.icu_mox*p.dead_mox))) %*%
         c(tc.mox_1, 
           tc.mox_2, 
           tc.mox_3, 
           tc.mox_4, 
           tc.mox_5, 
           tc.mox_6)

C.CEF <- c(prod(c(p.success_cef*p.hospitalized_cef)),
           prod(c(p.success_cef*p.discharge_cef)),
           prod(c(p.failure_cef*p.delayed_cef)),
           prod(c(p.failure_cef*p.switch_cef*p.initiate_swtich_cef)),
           prod(c(p.failure_cef*p.switch_cef*p.icu_cef*p.cure_cef)),
           prod(c(p.failure_cef*p.switch_cef*p.icu_cef*p.dead_cef))) %*%
         c(tc.cef_1, 
           tc.cef_2, 
           tc.cef_3, 
           tc.cef_4, 
           tc.cef_5, 
           tc.cef_6)

C.CLA <- c(prod(c(p.success_cla*p.hospitalized_cla)),
           prod(c(p.success_cla*p.discharge_cla)),
           prod(c(p.failure_cla*p.delayed_cla)),
           prod(c(p.failure_cla*p.switch_cla*p.initiate_swtich_cla)),
           prod(c(p.failure_cla*p.switch_cla*p.icu_cla*p.cure_cla)),
           prod(c(p.failure_cla*p.switch_cla*p.icu_cla*p.dead_cla))) %*%
         c(tc.cla_1, 
           tc.cla_2, 
           tc.cla_3, 
           tc.cla_4, 
           tc.cla_5, 
           tc.cla_6)

### Expected utility
E.LEF <- c(prod(c(p.success_lef*p.hospitalized_lef)),
           prod(c(p.success_lef*p.discharge_lef)),
           prod(c(p.failure_lef*p.delayed_lef)),
           prod(c(p.failure_lef*p.switch_lef*p.initiate_swtich_lef)),
           prod(c(p.failure_lef*p.switch_lef*p.icu_lef*p.cure_lef)),
           prod(c(p.failure_lef*p.switch_lef*p.icu_lef*p.dead_lef))) %*%
         c(util.lef_1, 
           util.lef_2,
           util.lef_3,
           util.lef_4,
           util.lef_5,
           util.lef_6)

E.LEV <- c(prod(c(p.success_lev*p.hospitalized_lev)),
           prod(c(p.success_lev*p.discharge_lev)),
           prod(c(p.failure_lev*p.delayed_lev)),
           prod(c(p.failure_lev*p.switch_lev*p.initiate_swtich_lev)),
           prod(c(p.failure_lev*p.switch_lev*p.icu_lev*p.cure_lev)),
           prod(c(p.failure_lev*p.switch_lev*p.icu_lev*p.dead_lev))) %*%
         c(util.lev_1, 
           util.lev_2,
           util.lev_3,
           util.lev_4,
           util.lev_5,
           util.lev_6)

E.MOX <- c(prod(c(p.success_mox*p.hospitalized_mox)),
           prod(c(p.success_mox*p.discharge_mox)),
           prod(c(p.failure_mox*p.delayed_mox)),
           prod(c(p.failure_mox*p.switch_mox*p.initiate_swtich_mox)),
           prod(c(p.failure_mox*p.switch_mox*p.icu_mox*p.cure_mox)),
           prod(c(p.failure_mox*p.switch_mox*p.icu_mox*p.dead_mox))) %*%
         c(util.mox_1, 
           util.mox_2,
           util.mox_3,
           util.mox_4,
           util.mox_5,
           util.mox_6)

E.CEF <- c(prod(c(p.success_cef*p.hospitalized_cef)),
           prod(c(p.success_cef*p.discharge_cef)),
           prod(c(p.failure_cef*p.delayed_cef)),
           prod(c(p.failure_cef*p.switch_cef*p.initiate_swtich_cef)),
           prod(c(p.failure_cef*p.switch_cef*p.icu_cef*p.cure_cef)),
           prod(c(p.failure_cef*p.switch_cef*p.icu_cef*p.dead_cef))) %*%
         c(util.cef_1, 
           util.cef_2,
           util.cef_3,
           util.cef_4,
           util.cef_5,
           util.cef_6)

E.CLA <- c(prod(c(p.success_cla*p.hospitalized_cla)),
           prod(c(p.success_cla*p.discharge_cla)),
           prod(c(p.failure_cla*p.delayed_cla)),
           prod(c(p.failure_cla*p.switch_cla*p.initiate_swtich_cla)),
           prod(c(p.failure_cla*p.switch_cla*p.icu_cla*p.cure_cla)),
           prod(c(p.failure_cla*p.switch_cla*p.icu_cla*p.dead_cla))) %*%
         c(util.cla_1, 
           util.cla_2,
           util.cla_3,
           util.cla_4,
           util.cla_5,
           util.cla_6)

### Total costs and effects
TC <- c(C.LEF, C.LEV, C.MOX, C.CEF, C.CLA)
TE <- c(E.LEF, E.LEV, E.MOX, E.CEF, E.CLA)

names(TC) <- names(TE) <- c("LEF", "LEV", "MOX", "CEF", "CLA")
TC
TE


### Incremental costs
DC <- TC - TC[4]
DC

### Incremental effects
DE <- TE - TE[4]
DE


### Incremental Cost-Effectiveness Ratio
ICER <- DC / DE
ICER[4] <- NA
ICER


### Net Monetary Benefit
NMB <- TE * wtp - TC
NMB


### Create Full ICER table
table.icer <- cbind(TC, TE, DC, DE, round(ICER, 2))
table.icer <- as.data.frame(table.icer)
colnames(table.icer) <- c("Costs", "QALYs", "Inc. Costs", "Inc. QALYs", "ICER")
rownames(table.icer) <- strategies
table.icer



############################################
### Plot the Cost-effectiveness frontier
############################################
library("ggplot2")

ce.mat <- cbind(Strategy = 1:5, 
                Cost = TC, 
                Effectiveness = TE)

ce.df <- data.frame(Strategy = strategies,
                    Cost = TC,
                    Effectiveness = TE)
ce.df

ce.front <- getFrontier(ce.mat, plot = F)
plotFrontier(CEmat = ce.df, frontier = ce.front)






##################################################
#### First OWSA - CEF v. MOX ####
##################################################

##################################################
#### Wrap decision tree into a function ####
##################################################
dec_tree <- function(params){
  with(
    as.list(params), 
    {
      
      ### Total costs - CEF
      tc.cef_1 = c.cef_iv*tx.duration_cef + (c.dr_los + c.inp_los)*los.succes_dx_cef + (c.cbc + c.bmp + c.sputum + c.blood)*tx.duration_cef + 1*(c.dr_x_ray + c.proc_xray)
      tc.cef_2 = c.cef_iv*tx.duration_cef + (c.dr_los + c.inp_los)*los.succes_cef + (c.cbc + c.bmp + c.sputum + c.blood)*los.succes_cef + 1*(c.dr_x_ray + c.proc_xray)
      tc.cef_3 = c.cef_iv*tx.duration_cef + (c.dr_los + c.inp_los)*los.failure_cef + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_cef + 1*(c.dr_x_ray + c.proc_xray)
      tc.cef_4 = c.cef_iv*tx.duration_cef + (c.dr_los + c.inp_los)*los.failure_cef + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_cef + 1*(c.dr_x_ray + c.proc_xray)
      tc.cef_5 = c.cef_iv*tx.duration_cef + (c.dr_los + c.inp_los)*los.initial_failure_cef + (c.dr_icu + c.inp_icu)*los.icu_cef + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_cef + los.icu_cef) + 1*(c.dr_x_ray + c.proc_xray)
      tc.cef_6 = c.cef_iv*tx.duration_cef + (c.dr_los + c.inp_los)*los.initial_failure_cef + (c.dr_icu + c.inp_icu)*los.icu_cef + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_cef + los.icu_cef) + 1*(c.dr_x_ray + c.proc_xray)
      
      ### Total costs - MOX
      tc.mox_1 = c.mox*tx.duration_mox + (c.dr_los + c.inp_los)*los.succes_dx_mox + (c.cbc + c.bmp + c.sputum + c.blood)*tx.duration_mox + 1*(c.dr_x_ray + c.proc_xray)
      tc.mox_2 = c.mox*tx.duration_mox + (c.dr_los + c.inp_los)*los.succes_mox + (c.cbc + c.bmp + c.sputum + c.blood)*los.succes_mox + 1*(c.dr_x_ray + c.proc_xray)
      tc.mox_3 = c.mox*tx.duration_mox + (c.dr_los + c.inp_los)*los.failure_mox + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_mox + 1*(c.dr_x_ray + c.proc_xray)
      tc.mox_4 = c.mox*tx.duration_mox + (c.dr_los + c.inp_los)*los.failure_mox + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_mox + 1*(c.dr_x_ray + c.proc_xray)
      tc.mox_5 = c.mox*tx.duration_mox + (c.dr_los + c.inp_los)*los.initial_failure_mox + (c.dr_icu + c.inp_icu)*los.icu_mox + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_mox + los.icu_mox) + 1*(c.dr_x_ray + c.proc_xray)
      tc.mox_6 = c.mox*tx.duration_mox + (c.dr_los + c.inp_los)*los.initial_failure_mox + (c.dr_icu + c.inp_icu)*los.icu_mox + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_mox + los.icu_mox) + 1*(c.dr_x_ray + c.proc_xray)
      
      
      
      ### Total utilities - CEF
      util.cef_1 = u.hospitalization*(tx.duration_cef/365.25)
      util.cef_2 = u.discharge*(tx.duration_cef/365.25)
      util.cef_3 = u.failure*(los.failure_cef/365.25) + u.hospitalization*(los.failure_cef/365.25)
      util.cef_4 = u.failure*(los.failure_cef/365.25) + u.hospitalization*(los.failure_cef/365.25)
      util.cef_5 = u.failure*(los.failure_cef/365.25) + u.hospitalization*(los.failure_cef/365.25)
      util.cef_6 = u.death     
      
      ### Total utilities - MOX
      util.mox_1 = u.hospitalization*(tx.duration_mox/365.25)
      util.mox_2 = u.discharge*(tx.duration_mox/365.25)
      util.mox_3 = u.failure*(los.failure_mox/365.25) + u.hospitalization*(los.failure_mox/365.25)
      util.mox_4 = u.failure*(los.failure_mox/365.25) + u.hospitalization*(los.failure_mox/365.25)
      util.mox_5 = u.failure*(los.failure_mox/365.25) + u.hospitalization*(los.failure_mox/365.25)
      util.mox_6 = u.death
      
      
      ### Expected Benefits for CEF
      E.CEF <- c(prod(c(p.success_cef*p.hospitalized_cef)),
                 prod(c(p.success_cef*p.discharge_cef)),
                 prod(c(p.failure_cef*p.delayed_cef)),
                 prod(c(p.failure_cef*p.switch_cef*p.initiate_swtich_cef)),
                 prod(c(p.failure_cef*p.switch_cef*p.icu_cef*p.cure_cef)),
                 prod(c(p.failure_cef*p.switch_cef*p.icu_cef*p.dead_cef))) %*%
        c(util.cef_1, 
          util.cef_2,
          util.cef_3,
          util.cef_4,
          util.cef_5,
          util.cef_6)
      
      ### Expected Benefits for MOX
      E.MOX <- c(prod(c(p.success_mox*p.hospitalized_mox)),
                 prod(c(p.success_mox*p.discharge_mox)),
                 prod(c(p.failure_mox*p.delayed_mox)),
                 prod(c(p.failure_mox*p.switch_mox*p.initiate_swtich_mox)),
                 prod(c(p.failure_mox*p.switch_mox*p.icu_mox*p.cure_mox)),
                 prod(c(p.failure_mox*p.switch_mox*p.icu_mox*p.dead_mox))) %*%
        c(util.mox_1, 
          util.mox_2,
          util.mox_3,
          util.mox_4,
          util.mox_5,
          util.mox_6)
      
      
      ### Expected Costs for CEF
      C.CEF <- c(prod(c(p.success_cef*p.hospitalized_cef)),
                 prod(c(p.success_cef*p.discharge_cef)),
                 prod(c(p.failure_cef*p.delayed_cef)),
                 prod(c(p.failure_cef*p.switch_cef*p.initiate_swtich_cef)),
                 prod(c(p.failure_cef*p.switch_cef*p.icu_cef*p.cure_cef)),
                 prod(c(p.failure_cef*p.switch_cef*p.icu_cef*p.dead_cef))) %*%
        c(tc.cef_1, 
          tc.cef_2, 
          tc.cef_3, 
          tc.cef_4, 
          tc.cef_5, 
          tc.cef_6)
      
      ### Expected Costs for MOX
      C.MOX <- c(prod(c(p.success_mox*p.hospitalized_mox)),
                 prod(c(p.success_mox*p.discharge_mox)),
                 prod(c(p.failure_mox*p.delayed_mox)),
                 prod(c(p.failure_mox*p.switch_mox*p.initiate_swtich_mox)),
                 prod(c(p.failure_mox*p.switch_mox*p.icu_mox*p.cure_mox)),
                 prod(c(p.failure_mox*p.switch_mox*p.icu_mox*p.dead_mox))) %*%
        c(tc.mox_1, 
          tc.mox_2, 
          tc.mox_3, 
          tc.mox_4, 
          tc.mox_5, 
          tc.mox_6)
      
      
      ### Expected total costs, expected life years, incremental costs, incremental life years , and ICER lists
      C    <- c(C.CEF, C.MOX)
      QALY <- c(E.CEF, E.MOX)
      IC   <- C.CEF - C.MOX
      IE   <- E.CEF - E.MOX
      ICER <- IC / IE
      
      names(C)    <- paste("C", c("CEF", "MOX"), sep = "_")
      names(QALY) <- paste("QALY", c("CEF", "MOX"), sep = "_")
      names(IC)   <- paste("Incr Costs")
      names(IE)   <- paste("Incr QALYs")
      names(ICER) <- paste("ICER")
      
      # Generate the output
      return(c(C, QALY, IC, IE, ICER))
    }
  )
}

#### Now, we can use the function "dec_tree" with the inputs to estimate the ICER and its corresponding values
dec_tree(input)
round(dec_tree(input), 4)


##################################################
#### One-way SA ###
##################################################
p.success_cef <- seq(1, 12, length.out = 50)
## Generate matrix of inputs for decision tree
m.owsa.input <- cbind(p.success_cef = p.success_cef_range, input[-1])


## Run model and capture NMB
full_outcomes <- t(apply(m.owsa.input, 1, dec_tree)) # for the full output

outcomes_NMB <- t(apply(m.owsa.input, 1, dec_tree))[, 5:7] # only include the ICER results

### Using base R commands
plot(p.success_cef_range, outcomes_NMB[, 1], type = "l", xlab = "p.success_cef_range", ylab = "NMB")
lines(p.success_cef_range, outcomes_NMB[, 3], col = "green")
legend("bottomright", strategies, col = 1:3, lty = c(1, 1, 1), bty = "n")

### the ggplot way
source("https://raw.githubusercontent.com/mbounthavong/Cost-effectiveness-of-antimicrobials-for-community-aquired-bacterial-pneumonia/main/R%20Functions/owsa_diagram_code.R") # PC
paramName <- "p.success_cef_range"
outcomeName <- "Net Monetary Benefit"
owsa.plot.det(param = p.success_cef_range, outcomes = outcomes_NMB, paramName = paramName, 
              strategyNames = 1:3, outcomeName = outcomeName)

owsa_df <- cbind(p.success_cef_range, full_outcomes) # Identify the break-even point


##################################################
#### Tornado Plots
##################################################


##################################################
#### Tornado Plot #1 - CEF v. MOX
##################################################
# Define ranges 
p.success_mox_range          <- c(BaseCase = p.success_mox,         low = 0.713  ,  high = 1.000)
p.success_cef_range          <- c(BaseCase = p.success_cef,         low = 0.78  ,  high = 1.000)
p.hospitalized_mox_range     <- c(BaseCase = p.hospitalized_mox,    low = 0.227  ,  high = 0.379)
p.hospitalized_cef_range     <- c(BaseCase = p.hospitalized_cef,    low = 0.280  ,  high = 0.379)
p.delayed_mox_range          <- c(BaseCase = p.delayed_mox,         low = 0.683  ,  high = 1.000)
p.delayed_cef_range          <- c(BaseCase = p.delayed_cef,         low = 0.683  ,  high = 1.000)
p.icu_mox_range              <- c(BaseCase = p.icu_mox,             low = 0.053  ,  high = 0.088)
p.icu_cef_range              <- c(BaseCase = p.icu_cef,             low = 0.083  ,  high = 0.138)
p.cure_mox_range             <- c(BaseCase = p.cure_mox,            low = 0.675  ,  high = 1.000)
p.cure_cef_range             <- c(BaseCase = p.cure_cef,            low = 0.675  ,  high = 1.000)
los.succes_mox_range         <- c(BaseCase = los.succes_mox,        low = 3.750  ,  high = 6.250)
los.succes_cef_range         <- c(BaseCase = los.succes_cef,        low = 3.750  ,  high = 6.250)
los.succes_dx_mox_range      <- c(BaseCase = los.succes_dx_mox,     low = 3.750  ,  high = 6.250)
los.succes_dx_cef_range      <- c(BaseCase = los.succes_dx_cef,     low = 3.750  ,  high = 6.250)
los.initial_failure_mox_range   <- c(BaseCase = los.initial_failure_mox,        low = 3.750  ,  high = 6.250)
los.initial_failure_cef_range   <- c(BaseCase = los.initial_failure_cef,        low = 3.750  ,  high = 6.250)
los.failure_mox_range        <- c(BaseCase = los.failure_mox,       low = 7.500  ,  high = 10.500)
los.failure_cef_range        <- c(BaseCase = los.failure_cef,       low = 9.500  ,  high = 12.500)
los.icu_mox_range            <- c(BaseCase = los.icu_mox,           low = 2.250  ,  high = 3.750)
los.icu_cef_range            <- c(BaseCase = los.icu_cef,           low = 2.250  ,  high = 3.750)
tx.duration_mox_range        <- c(BaseCase = tx.duration_mox,       low = 3.75   ,  high = 5.6)
tx.duration_cef_range        <- c(BaseCase = tx.duration_cef,       low = 4.30   ,  high = 6.25)
c.mox_range                  <- c(BaseCase = c.mox,                 low = 1.763  ,  high = 2.938)
c.cef_range                  <- c(BaseCase = c.cef_iv,              low = 2.145  ,  high = 3.575)
u.hospitalization_range      <- c(BaseCase = u.hospitalization,     low = 0.638  ,  high = 1.000)
u.discharge_range            <- c(BaseCase = u.discharge,           low = 0.690  ,  high = 1.000)
u.failure_range              <- c(BaseCase = u.failure,             low = 0.458  ,  high = 0.763)
c.dr_los_range               <- c(BaseCase = c.dr_los,              low = 148.72 ,  high = 247.87)
c.inp_los_range              <- c(BaseCase = c.inp_los,             low = 2117   ,  high = 3529)
c.dr_icu_range               <- c(BaseCase = c.dr_los,              low = 160.37 ,  high = 267.29)
c.inp_icu_range              <- c(BaseCase = c.inp_icu,             low = 2921   ,  high = 4869)
c.dr_x_ray_range             <- c(BaseCase = c.dr_x_ray,            low = 25.96  ,  high = 43.26)
c.proc_xray_range            <- c(BaseCase = c.proc_xray,           low = 61.19  ,  high = 101.99)
c.second_range               <- c(BaseCase = c.second,              low = 15531,    high = 25885)

## Parameter names
paramNames <-   c( "p.success_mox", 
                   "p.success_cef", 
                   "p.hospitalized_mox", 
                   "p.hospitalized_cef",
                   "p.delayed_mox",
                   "p.delayed_cef",
                   "p.icu_mox",
                   "p.icu_cef",
                   "p.cure_mox",
                   "p.cure_cef",
                   "los.succes_mox",
                   "los.succes_cef",
                   "los.succes_dx_mox",
                   "los.succes_dx_cef",
                   "los.initial_failure_mox",
                   "los.initial_failure_cef",
                   "los.failure_mox",
                   "los.failure_cef",
                   "los.icu_mox",
                   "los.icu_cef",
                   "tx.duration_mox",
                   "tx.duration_cef",
                   "c.mox",
                   "c.cef_iv",
                   "u.hospitalization",
                   "u.discharge",
                   "u.failure",
                   "c.dr_los",
                   "c.inp_los",
                   "c.dr_icu",
                   "c.inp_icu",
                   "c.dr_x_ray",
                   "c.proc_xray",
                   "c.second"
)

## List of inputs
l.tor.in <- vector("list", 34)
names(l.tor.in) <- paramNames
l.tor.in$p.success_mox          <- cbind(p.success_mox          = p.success_mox_range      ,   input[-1])
l.tor.in$p.success_cef          <- cbind(p.success_cef          = p.success_cef_range      ,   input[-2])
l.tor.in$p.hospitalized_mox     <- cbind(p.hospitalized_mox     = p.hospitalized_mox_range ,   input[-3])
l.tor.in$p.hospitalized_cef     <- cbind(p.hospitalized_cef     = p.hospitalized_cef_range ,   input[-4])
l.tor.in$p.delayed_mox          <- cbind(p.delayed_mox          = p.delayed_mox_range      ,   input[-5])
l.tor.in$p.delayed_cef          <- cbind(p.delayed_cef          = p.delayed_cef_range      ,   input[-6])
l.tor.in$p.icu_mox              <- cbind(p.icu_mox              = p.icu_mox_range          ,   input[-7])
l.tor.in$p.icu_cef              <- cbind(p.icu_cef              = p.icu_cef_range          ,   input[-8])
l.tor.in$p.cure_mox             <- cbind(p.cure_mox             = p.cure_mox_range         ,   input[-9])
l.tor.in$p.cure_cef             <- cbind(p.cure_cef             = p.cure_cef_range         ,   input[-10])
l.tor.in$los.succes_mox         <- cbind(los.succes_mox         = los.succes_mox_range     ,   input[-11])
l.tor.in$los.succes_cef         <- cbind(los.succes_cef         = los.succes_cef_range     ,   input[-12])
l.tor.in$los.succes_dx_mox      <- cbind(los.succes_dx_mox      = los.succes_dx_mox_range  ,   input[-13])
l.tor.in$los.succes_dx_cef      <- cbind(los.succes_dx_cef      = los.succes_dx_cef_range  ,   input[-14])
l.tor.in$los.initial_failure_mox      <- cbind(los.initial_failure_mox      = los.initial_failure_mox_range ,   input[-15])
l.tor.in$los.initial_failure_cef      <- cbind(los.initial_failure_cef      = los.initial_failure_cef_range ,   input[-16])
l.tor.in$los.failure_mox        <- cbind(los.failure_mox        = los.failure_mox_range    ,   input[-17])
l.tor.in$los.failure_cef        <- cbind(los.failure_cef        = los.failure_cef_range    ,   input[-18])
l.tor.in$los.icu_mox            <- cbind(los.icu_mox            = los.icu_mox_range        ,   input[-19])
l.tor.in$los.icu_cef            <- cbind(los.icu_cef            = los.icu_cef_range        ,   input[-20])
l.tor.in$tx.duration_mox        <- cbind(tx.duration_mox        = tx.duration_mox_range    ,   input[-21])
l.tor.in$tx.duration_cef        <- cbind(tx.duration_cef        = tx.duration_cef_range    ,   input[-22])
l.tor.in$c.mox                  <- cbind(c.mox                  = c.mox_range              ,   input[-23])
l.tor.in$c.cef_iv               <- cbind(c.cef_iv               = c.cef_range              ,   input[-24])
l.tor.in$u.hospitalization      <- cbind(u.hospitalization      = u.hospitalization_range  ,   input[-25])
l.tor.in$u.discharge            <- cbind(u.discharge            = u.discharge_range        ,   input[-26])
l.tor.in$u.failure              <- cbind(u.failure              = u.failure_range          ,   input[-27])
l.tor.in$c.dr_los               <- cbind(c.dr_los               = c.dr_los_range           ,   input[-28])
l.tor.in$c.inp_los              <- cbind(c.inp_los              = c.inp_los_range          ,   input[-29])
l.tor.in$c.dr_icu               <- cbind(c.dr_icu               = c.dr_icu_range           ,   input[-30])
l.tor.in$c.inp_icu              <- cbind(c.inp_icu              = c.inp_icu_range          ,   input[-31])
l.tor.in$c.dr_x_ray             <- cbind(c.dr_x_ray             = c.dr_x_ray_range         ,   input[-32])
l.tor.in$c.proc_xray            <- cbind(c.proc_xray            = c.proc_xray_range        ,   input[-33])
l.tor.in$c.second               <- cbind(c.second               = c.second_range           ,   input[-34])

## List of outputs
l.tor.out <- vector("list", 34)
names(l.tor.out) <- paramNames

## Run model on different parameters 
# NOTE: we select [ , 7] because that is the location of the ICER output.
for(i in 1:34){
  l.tor.out[[i]] <- t(apply(l.tor.in[[i]], 1, dec_tree))[ , 7] 
}


## Data structure: ymean, ymin, ymax
m.tor <- matrix(unlist(l.tor.out), nrow = 34, ncol = 3, byrow = TRUE, 
                dimnames = list(paramNames, c("basecase", "low", "high")))
m.tor

#############################
## Plot tornado #1
#############################
### Windows:
options(scipen = 5)

source("https://raw.githubusercontent.com/mbounthavong/Cost-effectiveness-of-antimicrobials-for-community-aquired-bacterial-pneumonia/main/R%20Functions/tornado_diagram_code.R")
TornadoPlot(main_title = "Tornado Plot (CEF v. MOX)", Parms = paramNames, Outcomes = m.tor, 
            outcomeName = "Incremental Cost-Effectiveness Ratio (ICER)", 
            xlab = "ICER", 
            ylab = "Parameters", 
            col1="#3182bd", col2="#6baed6") 




##################################################
#### PSA ####
##################################################


set.seed(123455)
nsim <- 10000 # number of iterations

# Initialize TC and LE that will store results from the simulations
TC <- TE <- matrix(NA, 
                   nrow = nsim, 
                   ncol = length(strategies)
                   ) 
                   
                   
for (i in 1:nsim) {

  input <- data.frame(
    p.success_lef <- rbeta(1, 48.66095, 3.678411),      # Probability of initial treatment success - LEF
    p.success_lev <- rbeta(1, 58.28364, 3.744648),      # Probability of initial treatment success - LEV
    p.success_mox <- rbeta(1, 99.46437, 5.120474),       # Probability of initial treatment success - MOX
    p.success_cef <- rbeta(1, 27.98687, 3.472129),      # Probability of initial treatment success - CEF
    p.success_cla <- rbeta(1, 71.89576, 3.82384),       # Probability of initial treatment success - CLA
    
    p.hospitalized_lef <- rbeta(1, 43.96643, 101.2331),   # Probability of remained hospitalized - LEF
    p.hospitalized_lev <- rbeta(1, 43.96643, 101.2331),   # Probability of remained hospitalized - LEV
    p.hospitalized_mox <- rbeta(1, 43.96643, 101.2331),   # Probability of remained hospitalized - MOX
    p.hospitalized_cef <- rbeta(1, 43.96643, 101.2331),   # Probability of remained hospitalized - CEF
    p.hospitalized_cla <- rbeta(1, 43.96643, 101.2331),   # Probability of remained hospitalized - CLA
    
    p.discharge_lef = 1 - p.hospitalized_lef,   # Probability of discharged - LEF
    p.discharge_lev = 1 - p.hospitalized_lev,   # Probability of discharged - LEV
    p.discharge_mox = 1 - p.hospitalized_mox,   # Probability of discharged - MOX
    p.discharge_cef = 1 - p.hospitalized_cef,   # Probability of discharged - CEF
    p.discharge_cla = 1 - p.hospitalized_cla,   # Probability of discharged - CLA
    
    p.failure_lef = 1 - p.success_lef,   # Probability of initial treatment failure - LEF
    p.failure_lev = 1 - p.success_lev,   # Probability of initial treatment failure - LEV
    p.failure_mox = 1 - p.success_mox,   # Probability of initial treatment failure - MOX
    p.failure_cef = 1 - p.success_cef,   # Probability of initial treatment failure - CEF
    p.failure_cla = 1 - p.success_cla,   # Probability of initial treatment failure - CLA
    
    p.delayed_lef <- rbeta(1, 48.49286, 3.65),  # Probability of delayed response - LEF
    p.delayed_lev <- rbeta(1, 20.27143, 3.3),  # Probability of delayed response - LEV
    p.delayed_mox <- rbeta(1, 67.52802, 6.678595),  # Probability of delayed response - MOX
    p.delayed_cef <- rbeta(1, 67.52802, 6.678595),  # Probability of delayed response - CEF
    p.delayed_cla <- rbeta(1, 41.4, 3.6),  # Probability of delayed response - CLA
    
    p.switch_lef = 1 - p.delayed_lef,  # Probability of failure and switch to 2nd line tx - LEF
    p.switch_lev = 1 - p.delayed_lev,  # Probability of failure and switch to 2nd line tx - LEV
    p.switch_mox = 1 - p.delayed_mox,  # Probability of failure and switch to 2nd line tx - MOX
    p.switch_cef = 1 - p.delayed_cef,  # Probability of failure and switch to 2nd line tx - CEF
    p.switch_cla = 1 - p.delayed_lef,  # Probability of failure and switch to 2nd line tx - CLA
    
    p.icu_lef <- rbeta(1, 54.83388, 443.6559),    ## Probability of ICU admission - LEF
    p.icu_lev <- rbeta(1, 54.83388, 443.6559),    ## Probability of ICU admission - LEV
    p.icu_mox <- rbeta(1, 55.78131, 592.8385),   ## Probability of ICU admission - MOX
    p.icu_cef <- rbeta(1, 56.18926, 746.5144),    ## Probability of ICU admission - CEF
    p.icu_cla <- rbeta(1, 51.14392, 431.3459),   ## Probability of ICU admission - CLA
    
    p.initiate_swtich_lef = 1 - p.icu_lef,   ## Probability of initiating 2nd line treatment - LEF
    p.initiate_swtich_lev = 1 - p.icu_lev,   ## Probability of initiating 2nd line treatment - LEV
    p.initiate_swtich_mox = 1 - p.icu_mox,   ## Probability of initiating 2nd line treatment - MOX
    p.initiate_swtich_cef = 1 - p.icu_cef,   ## Probability of initiating 2nd line treatment - CEF
    p.initiate_swtich_cla = 1 - p.icu_cla,   ## Probability of initiating 2nd line treatment - CLA
    
    p.cure_lef <- rbeta(1, 31.5, 3.5),  ## Probability of cured - LEF
    p.cure_lev <- rbeta(1, 31.5, 3.5),  ## Probability of cured - LEV
    p.cure_mox <- rbeta(1, 31.5, 3.5),  ## Probability of cured - MOX
    p.cure_cef <- rbeta(1, 31.5, 3.5),  ## Probability of cured - CEF
    p.cure_cla <- rbeta(1, 31.5, 3.5),  ## Probability of cured - CLA
    
    p.dead_lef = 1 - p.cure_lef,  ## Probability of dead - LEF
    p.dead_lev = 1 - p.cure_lev,  ## Probability of dead - LEV
    p.dead_mox = 1 - p.cure_mox,  ## Probability of dead - MOX
    p.dead_cef = 1 - p.cure_cef,  ## Probability of dead - CEF
    p.dead_cla = 1 - p.cure_cla,  ## Probability of dead - CLA
    
    
    ### Length of stay
    los.succes_dx_lef <- rlnorm(1, 1.582117, 0.1193159),  ## Average general ward LOS, treatment success (days) - LEF
    los.succes_dx_lev <- rlnorm(1, 1.582117, 0.1193159),  ## Average general ward LOS, treatment success (days) - LEV
    los.succes_dx_mox <- rlnorm(1, 1.582117, 0.1193159),  ## Average general ward LOS, treatment success (days) - MOX
    los.succes_dx_cef <- rlnorm(1, 1.582117, 0.1193159),  ## Average general ward LOS, treatment success (days) - CEF
    los.succes_dx_cla <- rlnorm(1, 1.582117, 0.1193159),  ## Average general ward LOS, treatment success (days) - CLA
    
    los.succes_lef <- rlnorm(1, 1.365744, 0.372398),  ## Average general ward LOS discharged, treatment success (days) - LEF
    los.succes_lev <- rlnorm(1, 1.365744, 0.372398),  ## Average general ward LOS discharged, treatment success (days) - LEV
    los.succes_mox <- rlnorm(1, 1.365744, 0.372398),  ## Average general ward LOS discharged, treatment success (days) - MOX
    los.succes_cef <- rlnorm(1, 1.365744, 0.372398),  ## Average general ward LOS discharged, treatment success (days) - CEF
    los.succes_cla <- rlnorm(1, 1.365744, 0.372398),  ## Average general ward LOS discharged, treatment success (days) - CLA
    
    los.initial_failure_lef <- rlnorm(1, 1.365744, 0.372398),  ## Average general ward LOS discharged, treatment success (days) - LEF
    los.initial_failure_lev <- rlnorm(1, 1.365744, 0.372398),  ## Average general ward LOS discharged, treatment success (days) - LEV
    los.initial_failure_mox <- rlnorm(1, 1.365744, 0.372398),  ## Average general ward LOS discharged, treatment success (days) - MOX
    los.initial_failure_cef <- rlnorm(1, 1.365744, 0.372398),  ## Average general ward LOS discharged, treatment success (days) - CEF
    los.initial_failure_cla <- rlnorm(1, 1.365744, 0.372398),  ## Average general ward LOS discharged, treatment success (days) - CLA
    
    los.failure_lef <- rlnorm(1, 2.220489, 0.4290604), ## Average general ward LOS, treatment failure (days) - LEF
    los.failure_lev <- rlnorm(1, 2.220489, 0.4290604), ## Average general ward LOS, treatment failure (days) - LEV
    los.failure_mox <- rlnorm(1, 2.220489, 0.4290604), ## Average general ward LOS, treatment failure (days) - MOX
    los.failure_cef <- rlnorm(1, 2.220489, 0.4290604), ## Average general ward LOS, treatment failure (days) - CEF
    los.failure_cla <- rlnorm(1, 2.220489, 0.4290604), ## Average general ward LOS, treatment failure (days) - CLA
    
    los.icu_lef <- rlnorm(1, 1.090543, 0.1270368),  ## Average ICU LOS (days) - LEF
    los.icu_lev <- rlnorm(1, 1.090543, 0.1270368),  ## Average ICU LOS (days) - LEV
    los.icu_mox <- rlnorm(1, 1.090543, 0.1270368),  ## Average ICU LOS (days) - MOX
    los.icu_cef <- rlnorm(1, 1.090543, 0.1270368),  ## Average ICU LOS (days) - CEF
    los.icu_cla <- rlnorm(1, 1.090543, 0.1270368),  ## Average ICU LOS (days) - CLA
    
    tx.duration_LEF <- rlnorm(1, 1.57154, 0.275312),  ## Average treatment duration - LEF
    tx.duration_LEV <- rlnorm(1, 1.57154, 0.275312),  ## Average treatment duration - LEV
    tx.duration_MOX <- rlnorm(1, 1.57154, 0.275312),  ## Average treatment duration - MOX
    tx.duration_CEF <- rlnorm(1, 1.57154, 0.275312),  ## Average treatment duration - CEF
    tx.duration_CLA <- rlnorm(1, 1.57154, 0.275312),  ## Average treatment duration - CLA
    
    
    ### Costs
    c.lef <- rgamma(1, 63.98493, 0.2511478),      ## Cost of PO LEF (600 mg Q12H)
    c.lev <- rgamma(1, 61.73469, 280.6122),                ## Cost of PO LEV (500 mg QD)
    c.lev_iv <- rgamma(1, 61.73469, 280.6122), ## Cost of IV LEV (500 mg QD)
    c.mox <- rgamma(1, 63.8912, 27.18775),            ## Cost of PO MOX (400 mg QD)
    c.mox_iv <- rgamma(1, 63.8912, 27.18775),    ## Cost of IV MOX (400 mg QD)
    c.cef_iv <- rgamma(1, 63.82135, 22.31516),   ## Cost of IV CEF (2000 mg QD)
    c.cla <- rgamma(1, 63.97342, 6.643139),       ## Cost of PO CLA (500 mg BID)
    
    c.second <- rgamma(1, 4.252417, 0.0002053514),    ## Cost of second line treatment
    
    c.dr_los <- rgamma(1, 63.99613, 0.3227401),   ## Cost of physician visit in hospital
    c.inp_los <- rgamma(1, 64.36209, 0.02279918),    ## Cost of one-night in the hospital
    c.los = (c.dr_los + c.inp_los),  ## Cost of hospital stay - initial high complexity
    
    c.dr_icu <- rgamma(1, 63.9988, 0.2992976),   ## Cost of physician visit in ICU
    c.inp_icu <- rgamma(1, 63.99133, 0.0164291),    ## Cost of one-night in the ICU
    c.icu = (c.dr_icu + c.inp_icu),  ## Cost of ICU (0 - 60 min) 
    
    c.dr_x_ray <- rgamma(1, 64.0074, 1.84939),  ## Cost of physician exam of X-ray (2 views)
    c.proc_xray <- rgamma(1, 63.99686, 0.7843714), ## Cost of X-ray machine
    c.x_ray = (c.dr_x_ray + c.proc_xray),  ## Cost of chest X-ray
    
    c.cbc <- rgamma(1, 64.03296, 8.24105),        ## Cost of CBC labs
    c.bmp <- rgamma(1, 63.93952, 7.557863),        ## Cost of BMP
    c.sputum <- rgamma(1, 63.97298, 6.75533),     ## Cost of sputum culture
    c.blood <- rgamma(1, 64, 6.20155),     ## Cost of blood culture
    
    
    
        ### Total costs
    tc.lef_1 = c.lef*tx.duration_lef + (c.dr_los + c.inp_los)*los.succes_dx_lef + (c.cbc + c.bmp + c.sputum + c.blood)*tx.duration_lef + 1*(c.dr_x_ray + c.proc_xray),
    tc.lef_2 = c.lef*tx.duration_lef + (c.dr_los + c.inp_los)*los.succes_lef + (c.cbc + c.bmp + c.sputum + c.blood)*los.succes_lef + 1*(c.dr_x_ray + c.proc_xray),
    tc.lef_3 = c.lef*tx.duration_lef + (c.dr_los + c.inp_los)*los.failure_lef + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_lef + 1*(c.dr_x_ray + c.proc_xray),
    tc.lef_4 = c.lef*tx.duration_lef + (c.dr_los + c.inp_los)*los.failure_lef + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_lef + 1*(c.dr_x_ray + c.proc_xray),
    tc.lef_5 = c.lef*tx.duration_lef + (c.dr_los + c.inp_los)*los.initial_failure_lef + (c.dr_icu + c.inp_icu)*los.icu_lef + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_lef + los.icu_lef) + 1*(c.dr_x_ray + c.proc_xray),
    tc.lef_6 = c.lef*tx.duration_lef + (c.dr_los + c.inp_los)*los.initial_failure_lef + (c.dr_icu + c.inp_icu)*los.icu_lef + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_lef + los.icu_lef) + 1*(c.dr_x_ray + c.proc_xray),
    
    tc.lev_1 = c.lev*tx.duration_lev + (c.dr_los + c.inp_los)*los.succes_dx_lev + (c.cbc + c.bmp + c.sputum + c.blood)*tx.duration_lev + 1*(c.dr_x_ray + c.proc_xray),
    tc.lev_2 = c.lev*tx.duration_lev + (c.dr_los + c.inp_los)*los.succes_lev + (c.cbc + c.bmp + c.sputum + c.blood)*los.succes_lev + 1*(c.dr_x_ray + c.proc_xray),
    tc.lev_3 = c.lev*tx.duration_lev + (c.dr_los + c.inp_los)*los.failure_lev + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_lev + 1*(c.dr_x_ray + c.proc_xray),
    tc.lev_4 = c.lev*tx.duration_lev + (c.dr_los + c.inp_los)*los.failure_lev + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_lev + 1*(c.dr_x_ray + c.proc_xray),
    tc.lev_5 = c.lev*tx.duration_lev + (c.dr_los + c.inp_los)*los.initial_failure_lev + (c.dr_icu + c.inp_icu)*los.icu_lev + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_lev + los.icu_lev) + 1*(c.dr_x_ray + c.proc_xray),
    tc.lev_6 = c.lev*tx.duration_lev + (c.dr_los + c.inp_los)*los.initial_failure_lev + (c.dr_icu + c.inp_icu)*los.icu_lev + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_lev + los.icu_lev) + 1*(c.dr_x_ray + c.proc_xray),
    
    tc.mox_1 = c.mox*tx.duration_mox + (c.dr_los + c.inp_los)*los.succes_dx_mox + (c.cbc + c.bmp + c.sputum + c.blood)*tx.duration_mox + 1*(c.dr_x_ray + c.proc_xray),
    tc.mox_2 = c.mox*tx.duration_mox + (c.dr_los + c.inp_los)*los.succes_mox + (c.cbc + c.bmp + c.sputum + c.blood)*los.succes_mox + 1*(c.dr_x_ray + c.proc_xray),
    tc.mox_3 = c.mox*tx.duration_mox + (c.dr_los + c.inp_los)*los.failure_mox + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_mox + 1*(c.dr_x_ray + c.proc_xray),
    tc.mox_4 = c.mox*tx.duration_mox + (c.dr_los + c.inp_los)*los.failure_mox + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_mox + 1*(c.dr_x_ray + c.proc_xray),
    tc.mox_5 = c.mox*tx.duration_mox + (c.dr_los + c.inp_los)*los.initial_failure_mox + (c.dr_icu + c.inp_icu)*los.icu_mox + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_mox + los.icu_mox) + 1*(c.dr_x_ray + c.proc_xray),
    tc.mox_6 = c.mox*tx.duration_mox + (c.dr_los + c.inp_los)*los.initial_failure_mox + (c.dr_icu + c.inp_icu)*los.icu_mox + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_mox + los.icu_mox) + 1*(c.dr_x_ray + c.proc_xray),
    
    tc.cef_1 = c.cef_iv*tx.duration_cef + (c.dr_los + c.inp_los)*los.succes_dx_cef + (c.cbc + c.bmp + c.sputum + c.blood)*tx.duration_cef + 1*(c.dr_x_ray + c.proc_xray),
    tc.cef_2 = c.cef_iv*tx.duration_cef + (c.dr_los + c.inp_los)*los.succes_cef + (c.cbc + c.bmp + c.sputum + c.blood)*los.succes_cef + 1*(c.dr_x_ray + c.proc_xray),
    tc.cef_3 = c.cef_iv*tx.duration_cef + (c.dr_los + c.inp_los)*los.failure_cef + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_cef + 1*(c.dr_x_ray + c.proc_xray),
    tc.cef_4 = c.cef_iv*tx.duration_cef + (c.dr_los + c.inp_los)*los.failure_cef + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_cef + 1*(c.dr_x_ray + c.proc_xray),
    tc.cef_5 = c.cef_iv*tx.duration_cef + (c.dr_los + c.inp_los)*los.initial_failure_cef + (c.dr_icu + c.inp_icu)*los.icu_cef + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_cef + los.icu_cef) + 1*(c.dr_x_ray + c.proc_xray),
    tc.cef_6 = c.cef_iv*tx.duration_cef + (c.dr_los + c.inp_los)*los.initial_failure_cef + (c.dr_icu + c.inp_icu)*los.icu_cef + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_cef + los.icu_cef) + 1*(c.dr_x_ray + c.proc_xray),
    
    tc.cla_1 = c.cla*tx.duration_cla + (c.dr_los + c.inp_los)*los.succes_dx_cla + (c.cbc + c.bmp + c.sputum + c.blood)*tx.duration_cla + 1*(c.dr_x_ray + c.proc_xray),
    tc.cla_2 = c.cla*tx.duration_cla + (c.dr_los + c.inp_los)*los.succes_cla + (c.cbc + c.bmp + c.sputum + c.blood)*los.succes_cla + 1*(c.dr_x_ray + c.proc_xray),
    tc.cla_3 = c.cla*tx.duration_cla + (c.dr_los + c.inp_los)*los.failure_cla + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_cla + 1*(c.dr_x_ray + c.proc_xray),
    tc.cla_4 = c.cla*tx.duration_cla + (c.dr_los + c.inp_los)*los.failure_cla + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*los.failure_cla + 1*(c.dr_x_ray + c.proc_xray),
    tc.cla_5 = c.cla*tx.duration_cla + (c.dr_los + c.inp_los)*los.initial_failure_cla + (c.dr_icu + c.inp_icu)*los.icu_cla + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_cla + los.icu_cla) + 1*(c.dr_x_ray + c.proc_xray),
    tc.cla_6 = c.cla*tx.duration_cla + (c.dr_los + c.inp_los)*los.initial_failure_cla + (c.dr_icu + c.inp_icu)*los.icu_cla + c.second + (c.cbc + c.bmp + c.sputum + c.blood)*(los.initial_failure_cla + los.icu_cla) + 1*(c.dr_x_ray + c.proc_xray),
    
    
    ### Utilities
    u.hospitalization <- rbeta(1, 3.966667, 0.7),  ## Utility value for CAP hospitalization
    u.discharge <- rbeta(1, 9.66, 0.84),        ## Utility value for discharge after treatment success
    u.failure <- rbeta(1, 5.839733, 3.7336),          ## Utility value after treatment failure
    u.death = 0,               ## Utility value for death
    
    wtp    = 100000,          # Willingness to pay per QALY gained
    
    
    ### Total utilities
    util.lef_1 = u.hospitalization*(tx.duration_lef/365.25),
    util.lef_2 = u.discharge*(tx.duration_lef/365.25),
    util.lef_3 = u.failure*(los.failure_lef/365.25) + u.hospitalization*(los.failure_lef/365.25),
    util.lef_4 = u.failure*(los.failure_lef/365.25) + u.hospitalization*(los.failure_lef/365.25),
    util.lef_5 = u.failure*(los.failure_lef/365.25) + u.hospitalization*(los.failure_lef/365.25),
    util.lef_6 = u.death,
    
    util.lev_1 = u.hospitalization*(tx.duration_lev/365.25),
    util.lev_2 = u.discharge*(tx.duration_lev/365.25),
    util.lev_3 = u.failure*(los.failure_lev/365.25) + u.hospitalization*(los.failure_lev/365.25),
    util.lev_4 = u.failure*(los.failure_lev/365.25) + u.hospitalization*(los.failure_lev/365.25),
    util.lev_5 = u.failure*(los.failure_lev/365.25) + u.hospitalization*(los.failure_lev/365.25),
    util.lev_6 = u.death,
    
    util.mox_1 = u.hospitalization*(tx.duration_mox/365.25),
    util.mox_2 = u.discharge*(tx.duration_mox/365.25),
    util.mox_3 = u.failure*(los.failure_mox/365.25) + u.hospitalization*(los.failure_mox/365.25),
    util.mox_4 = u.failure*(los.failure_mox/365.25) + u.hospitalization*(los.failure_mox/365.25),
    util.mox_5 = u.failure*(los.failure_mox/365.25) + u.hospitalization*(los.failure_mox/365.25),
    util.mox_6 = u.death,
    
    util.cef_1 = u.hospitalization*(tx.duration_cef/365.25),
    util.cef_2 = u.discharge*(tx.duration_cef/365.25),
    util.cef_3 = u.failure*(los.failure_cef/365.25) + u.hospitalization*(los.failure_cef/365.25),
    util.cef_4 = u.failure*(los.failure_cef/365.25) + u.hospitalization*(los.failure_cef/365.25),
    util.cef_5 = u.failure*(los.failure_cef/365.25) + u.hospitalization*(los.failure_cef/365.25),
    util.cef_6 = u.death,
    
    util.cla_1 = u.hospitalization*(tx.duration_cla/365.25),
    util.cla_2 = u.discharge*(tx.duration_cla/365.25),
    util.cla_3 = u.failure*(los.failure_cla/365.25) + u.hospitalization*(los.failure_cla/365.25),
    util.cla_4 = u.failure*(los.failure_cla/365.25) + u.hospitalization*(los.failure_cla/365.25),
    util.cla_5 = u.failure*(los.failure_cla/365.25) + u.hospitalization*(los.failure_cla/365.25),
    util.cla_6 = u.death
    
    
  )
  
  
  
  
  ##################################################
  #### OPEN TREE Method -- Decision Tree ####
  ##################################################
  
  ### Expected costs
  C.LEF <- c(prod(c(p.success_lef*p.hospitalized_lef)),
             prod(c(p.success_lef*p.discharge_lef)),
             prod(c(p.failure_lef*p.delayed_lef)),
             prod(c(p.failure_lef*p.switch_lef*p.initiate_swtich_lef)),
             prod(c(p.failure_lef*p.switch_lef*p.icu_lef*p.cure_lef)),
             prod(c(p.failure_lef*p.switch_lef*p.icu_lef*p.dead_lef))) %*%
    c(tc.lef_1, 
      tc.lef_2, 
      tc.lef_3, 
      tc.lef_4, 
      tc.lef_5, 
      tc.lef_6)
  
  C.LEV <- c(prod(c(p.success_lev*p.hospitalized_lev)),
             prod(c(p.success_lev*p.discharge_lev)),
             prod(c(p.failure_lev*p.delayed_lev)),
             prod(c(p.failure_lev*p.switch_lev*p.initiate_swtich_lev)),
             prod(c(p.failure_lev*p.switch_lev*p.icu_lev*p.cure_lev)),
             prod(c(p.failure_lev*p.switch_lev*p.icu_lev*p.dead_lev))) %*%
    c(tc.lev_1, 
      tc.lev_2, 
      tc.lev_3, 
      tc.lev_4, 
      tc.lev_5, 
      tc.lev_6)
  
  C.MOX <- c(prod(c(p.success_mox*p.hospitalized_mox)),
             prod(c(p.success_mox*p.discharge_mox)),
             prod(c(p.failure_mox*p.delayed_mox)),
             prod(c(p.failure_mox*p.switch_mox*p.initiate_swtich_mox)),
             prod(c(p.failure_mox*p.switch_mox*p.icu_mox*p.cure_mox)),
             prod(c(p.failure_mox*p.switch_mox*p.icu_mox*p.dead_mox))) %*%
    c(tc.mox_1, 
      tc.mox_2, 
      tc.mox_3, 
      tc.mox_4, 
      tc.mox_5, 
      tc.mox_6)
  
  C.CEF <- c(prod(c(p.success_cef*p.hospitalized_cef)),
             prod(c(p.success_cef*p.discharge_cef)),
             prod(c(p.failure_cef*p.delayed_cef)),
             prod(c(p.failure_cef*p.switch_cef*p.initiate_swtich_cef)),
             prod(c(p.failure_cef*p.switch_cef*p.icu_cef*p.cure_cef)),
             prod(c(p.failure_cef*p.switch_cef*p.icu_cef*p.dead_cef))) %*%
    c(tc.cef_1, 
      tc.cef_2, 
      tc.cef_3, 
      tc.cef_4, 
      tc.cef_5, 
      tc.cef_6)
  
  C.CLA <- c(prod(c(p.success_cla*p.hospitalized_cla)),
             prod(c(p.success_cla*p.discharge_cla)),
             prod(c(p.failure_cla*p.delayed_cla)),
             prod(c(p.failure_cla*p.switch_cla*p.initiate_swtich_cla)),
             prod(c(p.failure_cla*p.switch_cla*p.icu_cla*p.cure_cla)),
             prod(c(p.failure_cla*p.switch_cla*p.icu_cla*p.dead_cla))) %*%
    c(tc.cla_1, 
      tc.cla_2, 
      tc.cla_3, 
      tc.cla_4, 
      tc.cla_5, 
      tc.cla_6)
  
  ### Expected utility
  E.LEF <- c(prod(c(p.success_lef*p.hospitalized_lef)),
             prod(c(p.success_lef*p.discharge_lef)),
             prod(c(p.failure_lef*p.delayed_lef)),
             prod(c(p.failure_lef*p.switch_lef*p.initiate_swtich_lef)),
             prod(c(p.failure_lef*p.switch_lef*p.icu_lef*p.cure_lef)),
             prod(c(p.failure_lef*p.switch_lef*p.icu_lef*p.dead_lef))) %*%
    c(util.lef_1, 
      util.lef_2,
      util.lef_3,
      util.lef_4,
      util.lef_5,
      util.lef_6)
  
  E.LEV <- c(prod(c(p.success_lev*p.hospitalized_lev)),
             prod(c(p.success_lev*p.discharge_lev)),
             prod(c(p.failure_lev*p.delayed_lev)),
             prod(c(p.failure_lev*p.switch_lev*p.initiate_swtich_lev)),
             prod(c(p.failure_lev*p.switch_lev*p.icu_lev*p.cure_lev)),
             prod(c(p.failure_lev*p.switch_lev*p.icu_lev*p.dead_lev))) %*%
    c(util.lev_1, 
      util.lev_2,
      util.lev_3,
      util.lev_4,
      util.lev_5,
      util.lev_6)
  
  E.MOX <- c(prod(c(p.success_mox*p.hospitalized_mox)),
             prod(c(p.success_mox*p.discharge_mox)),
             prod(c(p.failure_mox*p.delayed_mox)),
             prod(c(p.failure_mox*p.switch_mox*p.initiate_swtich_mox)),
             prod(c(p.failure_mox*p.switch_mox*p.icu_mox*p.cure_mox)),
             prod(c(p.failure_mox*p.switch_mox*p.icu_mox*p.dead_mox))) %*%
    c(util.mox_1, 
      util.mox_2,
      util.mox_3,
      util.mox_4,
      util.mox_5,
      util.mox_6)
  
  E.CEF <- c(prod(c(p.success_cef*p.hospitalized_cef)),
             prod(c(p.success_cef*p.discharge_cef)),
             prod(c(p.failure_cef*p.delayed_cef)),
             prod(c(p.failure_cef*p.switch_cef*p.initiate_swtich_cef)),
             prod(c(p.failure_cef*p.switch_cef*p.icu_cef*p.cure_cef)),
             prod(c(p.failure_cef*p.switch_cef*p.icu_cef*p.dead_cef))) %*%
    c(util.cef_1, 
      util.cef_2,
      util.cef_3,
      util.cef_4,
      util.cef_5,
      util.cef_6)
  
  E.CLA <- c(prod(c(p.success_cla*p.hospitalized_cla)),
             prod(c(p.success_cla*p.discharge_cla)),
             prod(c(p.failure_cla*p.delayed_cla)),
             prod(c(p.failure_cla*p.switch_cla*p.initiate_swtich_cla)),
             prod(c(p.failure_cla*p.switch_cla*p.icu_cla*p.cure_cla)),
             prod(c(p.failure_cla*p.switch_cla*p.icu_cla*p.dead_cla))) %*%
    c(util.cla_1, 
      util.cla_2,
      util.cla_3,
      util.cla_4,
      util.cla_5,
      util.cla_6)
  
  ### Total costs and effects
  TC[i,] <- c(C.LEF, C.LEV, C.MOX, C.CEF, C.CLA)
  TE[i,] <- c(E.LEF, E.LEV, E.MOX, E.CEF, E.CLA)
}




names(TC) <- names(TE) <- c("LEF", "LEV", "MOX", "CEF", "CLA")


TC
describeBy(TC)
  
TE
describeBy(TE)

  
### Incremental costs
DC <- TC - TC[3]
DC
describeBy(DC)


### Incremental effects
DE <- TE - TE[3]
DE
describeBy(DE)

ICER <- DC / DE


### Combine matrix to form a dataset for PSA analysis
psa.data <- cbind(TC, TE)
colnames(psa.data) <- c("TC_LEF", "TC_LEV", "TC_MOX", "TC_CEF", "TC_CLA", "TE_LEF", "TE_LEV", "TE_MOX", "TE_CEF", "TE_CLA")
psa.df <- as.data.frame(psa.data)

##### Load functions needed for PSA
## Windows:
source("https://raw.githubusercontent.com/mbounthavong/Cost-effectiveness-of-antimicrobials-for-community-aquired-bacterial-pneumonia/main/R%20Functions/PSA_functions.R")

        
## Create initial parameters
# Label our strategies
Strategies <- c("LEF", "LEV", "MOX", "CEF", "CLA")
Strategies
                   
#Determine the number of strategies
numDepVars <- length(Strategies)
numDepVars                   
                   
## Create matrices for outcomes and parameters
Outcomes <- psa.df[, 2:(numDepVars*2+1)]
head(Outcomes)
summary(Outcomes)




#######################################
#### Displaying results from a PSA ####
#######################################
### Define matrices with costs and effects
m.c <- Outcomes[, c(1:5)]
m.e <- Outcomes[, c(6:10)]

### Define a vector of WTP values
v.wtp <- c(1, seq(5000, 1500000, length.out = 31))



#### CEA Curves (CEAC) & Frontier (CEAF) ####
ceaf(v.wtp = v.wtp, 
     strategies = Strategies, 
     m.e = m.e, 
     m.c = m.c)

#### CEAC for CEF v. MOX
Strategies2 <- c("MOX", "CEF")
m.c2 <- Outcomes[, c(3, 5)]
m.e2 <- Outcomes[, c(8, 10)]

ceaf(v.wtp = v.wtp, 
     strategies = Strategies2, 
     m.e = m.e2, 
     m.c = m.c2)

##########################################
## CE Frontier after PSA
##########################################
library("broom")
library("kableExtra")

psa_cap <- make_psa_obj(
            cost = m.c, 
            effectiveness = m.e,
            currency = "$"
          )

plot(psa_cap)

df_psa_cap <- summary(psa_cap)
head(df_psa_cap)

icer_cap <- calculate_icers(cost = df_psa_cap$meanCost,
                              effect = df_psa_cap$meanEffect,
                              strategies = df_psa_cap$Strategy)
icer_cap %>%
  kable() %>%
  kable_styling()


icer_cap %>%
  filter(Status == "ND")%>%
  kable() %>%
  kable_styling()


plot(icer_cap,
     center = TRUE,
     ellipse = TRUE,
     alpha = 0.5)

plot(icer_cap, label = "all") # can lead to a 'busy' plot
plot(icer_cap, plot_frontier_only = TRUE) # completely removes dominated strategies from plot
plot(icer_cap, currency = "USD", effect_units = "quality-adjusted life-years") # customize axis labels



### CEAC
ceac_obj <- ceac(wtp = v.wtp,
                 psa = psa_cap)

head(ceac_obj)
summary(ceac_obj)


plot(ceac_obj,
     frontier = TRUE,
     points = TRUE)







