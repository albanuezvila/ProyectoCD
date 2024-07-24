#Código de R para importar datos
library(readxl)
Original_ESS11 <- read_excel("Original_ESS11.xlsx")
View(Original_ESS11)

#Código de R para tratar los valores faltantes
library(dplyr)
df <- df %>%
  mutate(
    agea = ifelse(agea == 999, NA, agea),
    ppltrst = ifelse(ppltrst %in% c(77, 88, 99), NA, ppltrst),
    pplfair = ifelse(pplfair %in% c(77, 88, 99), NA, pplfair),
    pplhlp = ifelse(pplhlp %in% c(77, 88, 99), NA, pplhlp),
    lrscale = ifelse(lrscale %in% c(77, 88, 99), NA, lrscale),
    freehms = ifelse(freehms %in% c(7, 8, 9), "NS/NC", as.character(freehms)),
    hmsacld = ifelse(hmsacld %in% c(7, 8, 9), "NS/NC", as.character(hmsacld)),
    imbgeco = ifelse(imbgeco %in% c(77, 88, 99), NA, imbgeco),
    imwbcnt = ifelse(imwbcnt %in% c(77, 88, 99), NA, imwbcnt),
    hinctnta = ifelse(hinctnta %in% c(99), NA, hinctnta),
    hinctnta = ifelse(hinctnta %in% c(77), "12", as.numeric(hinctnta)),
    hinctnta = ifelse(hinctnta %in% c(88), "13", as.numeric(hinctnta)),
  )
df <- na.omit(df)

#Código para recodificar la variable referente al nivel educativo (edulvlb)
df <- df %>%
  mutate(edulvlb_reco = case_when(
    edulvlb == 0 ~ "<ISCED 1",
    edulvlb == 113 ~ "ISCED 1",
    edulvlb %in% c(129, 212, 213) ~ "ISCED 2",
    edulvlb %in% c(221, 222, 223, 229, 311, 312, 313, 321, 322, 323) ~ "ISCED 3",
    edulvlb %in% c(412, 413, 421, 422, 423) ~ "ISCED 4",
    edulvlb %in% c(510, 520, 610, 620, 710, 720, 800) ~ "ISCED 5, 6",
    TRUE ~ "Otros"  # Handle unexpected values
  ))

#Código de R para crear una nueva variable "actitud_positiva" en base a las 
#variables "ppltrust", "pplfair", y "pplhlp"

df$actitud_positiva <- (df$ppltrst + df$pplfair + df$pplhlp) / 3 #Media de las 3 variables
df$actitud_positiva <- round(df$actitud_positiva) #Redondeamos al integer más cercano
df$actitud_positiva[df$actitud_positiva < 0] <- 0 #Aseguramos que los valores están entre 1 y 10
df$actitud_positiva[df$actitud_positiva > 10] <- 10 #Aseguramos que los valores están entre 1 y 10

