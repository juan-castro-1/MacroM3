library(readxl)

# Tasa de depositos a plazo fijo Badlar (Privada Pesos, depositos de mas de $ 1 millon). ###
tasa <- read.csv(url("https://infra.datos.gob.ar/catalog/sspm/dataset/89/distribution/89.1/download/principales-tasas-interes.csv"))
tasa <- ts(tasa$tasas_interes_badlar , start = c(1996, 01), frequency = 12)

tasa <- window(tasa, start=c(2004,01), end = c(2019, 12))
tasa_bruta <- tasa
tasa <- 100*log(1 + tasa_bruta/1200)
remove(tasa_bruta)

di <- diff(tasa)

# Commodity Price Index ####
#pcom.file <- paste(tempfile(), ".ashx", sep = "")
#download.file("https://www.imf.org/~/media/Files/Research/CommodityPrices/Monthly/ExternalData.ashx", pcom.file, mode = "wb")
#
#pcom <- read_excel(pcom.file, skip = 3, sheet = 1)
#pcom <- as.numeric(pcom$Monthly...2)
#
#pcom <- pcom[complete.cases(pcom)] # delete NAs
#pcom <- ts(pcom, start = c(1992, 01), frequency = 12)
#
#remove(pcom.file)


# Nominal Exchange Rate (USD/ARS) ####
#tcn.file <- paste(tempfile(), ".xls", sep = "")
#download.file("http://www.bcra.gov.ar/Pdfs/PublicacionesEstadisticas/com3500.xls", tcn.file, mode = "wb")
#
#er <- read_excel(tcn.file, skip = 1, sheet = 2)
#er <- as.numeric(er$`Tipo de cambio nominal promedio mensual`)
#
#er <- er[complete.cases(er)] # delete NAs
#er <- ts(er, start = c(2002, 03), frequency = 12)
#
#remove(tcn.file)


# EMAE Index ####
yi.file <- paste(tempfile(), ".xls", sep = "")
download.file("https://www.indec.gob.ar/ftp/cuadros/economia/sh_emae_mensual_base2004.xls", yi.file, mode = "wb")

yi <- read_excel(yi.file, skip = 3)
yi <- as.numeric(yi$...4)

yi <- yi[complete.cases(yi)]
yi <- ts(yi, start = c(2004, 1), frequency = 12)
yi <- window(yi, start = c(2004, 1), end = c(2019, 12))
ceros <- ts(0, start = c(2004,01), end=c(2019,12), frequency = 12)

seq_09 <- ceros
seq_09[63:66] <- -1
seq_09[67:68] <- 1

seq_12 <- ceros
seq_12[99:102] <- -1
seq_12[103:104] <- 1

seq_18 <- ceros
seq_18[171:174] <- -1
seq_18[175:176] <- 1



y_seq <- 100*log(yi)
lm_seq <- lm(y_seq ~ seq_09 + seq_12 + seq_18) 
yi <- lm_seq$residuals
remove(seq_09,seq_12,seq_18,ceros,lm_seq,y_seq)
yi <- ts(yi, start = c(2004, 01),frequency = 12)

dy <- diff(yi)

remove(yi.file)

# Local CD Rate ####
#cd.file <- paste(tempfile(), ".xls", sep = "")
#download.file("http://www.bcra.gov.ar/Pdfs/PublicacionesEstadisticas/pashis.xls", cd.file, mode = "wb")
#
#cd <- read_excel(cd.file, skip = 93, sheet = 4)
#cd <- as.matrix(cd)
#cd <- as.numeric(cd[, 6])
#
#cd <- cd[complete.cases(cd)]
#cd <- cd[which(cd != 0, arr.ind = TRUE)]
#cd <- ts(cd, start = c(1995, 1), frequency = 12)
#
#remove(cd.file)


# Consumer Price Index ####

# Historic CPI
ipc <- read.csv(url("https://apis.datos.gob.ar/series/api/series/?ids=178.1_NL_GENERAL_0_0_13&limit=5000&format=csv"))
ipc <- ts(ipc$nivel_general, start = c(1943, 01), frequency = 12)
ipc <- diff(log(ipc))
ipc <- window(ipc, end = c(2006, 12))

# CPI, Province of San Luis 
ipc.sl <- read.csv(url("https://apis.datos.gob.ar/series/api/series/?ids=197.1_NIVEL_GENERAL_2014_0_13&limit=5000&format=csv"))
ipc.sl <- ts(ipc.sl$nivel_general, start = c(2005, 10), frequency = 12)
ipc.sl <- diff(log(ipc.sl))
ipc.sl <- window(ipc.sl, start = c(2007, 01), end = c(2012, 07))

# CPI, City of Buenos Aires
ipc.ba <- read.csv(url("https://apis.datos.gob.ar/series/api/series/?ids=193.1_NIVEL_GENERAL_JULI_0_13&limit=5000&format=csv"))
ipc.ba <- ts(ipc.ba$nivel_general, start = c(2012, 07), frequency = 12)
ipc.ba <- diff(log(ipc.ba))
ipc.ba <- window(ipc.ba, end = c(2016, 04))

# CPI, Greater Buenos Aires (INDEC)
ipc.gba <- read.csv(url("https://apis.datos.gob.ar/series/api/series/?ids=101.1_I2NG_2016_M_22&limit=5000&format=csv"))
ipc.gba <- ts(ipc.gba$ipc_2016_nivel_general, start = c(2016, 04), frequency = 12)
ipc.gba <- diff(log(ipc.gba))
ipc.gba <- window(ipc.gba, end = c(2016, 12))

# CPI, National (INDEC)
ipc.nac <- read.csv(url("https://apis.datos.gob.ar/series/api/series/?ids=145.3_INGNACNAL_DICI_M_15&limit=5000&format=csv"))
ipc.nac <- ts(ipc.nac$ipc_ng_nacional, start = c(2016, 12), frequency = 12)
ipc.nac <- diff(log(ipc.nac))

pc <- c(ipc, ipc.sl, ipc.ba, ipc.gba, ipc.nac)
pc <- c(1, cumprod(exp(pc)))
pc <- ts(pc, start = c(1943, 01), frequency = 12)
pc <- 100 * (pc / mean(tail(pc, 12)))

pc_1 <- window(pc, start=c(2003,12), end=c(2019,11), frequency=12)
pc <- window(pc,start=c(2004,1),end=c(2019,12))
pc <- 100*log(pc[1:192]/pc_1[1:192]);pc
pc <- ts(pc,start=c(2004,1),end=c(2019,12),frequency = 12)
remove(ipc, ipc.sl, ipc.ba, ipc.gba, ipc.nac,pc_1)

dp <- diff(pc)
dp <- dp-mean(dp)
di <- di-mean(di)
dy <- dy-mean(dy)
data <- cbind(dy,dp,di)
data <- data.frame(data)
library(writexl)
write_xlsx(data, "C:\\Users\\User\\Documents\\1. Maestria\\3. Trimestre III\\4. MacroMetrics\\TP\\3\\data.xlsx")