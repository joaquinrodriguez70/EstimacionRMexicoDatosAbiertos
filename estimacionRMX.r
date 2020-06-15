#http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip
#library("RCurl")
library(devtools)
library(EpiEstim)


remove(list = ls())
mydir <-  '/media/joaquin/Nuevo_vol/misdoc/Mios2020/covid19/estimacionr'
myurl <-  'http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip'
myfile <- 'datos_abiertos_covid19.zip'
resultadoConfirmado  = 1


#calculate the average of last x days for a specific column on a data frame and returs a df


#plot r0 for state
calculaREstado <- function(mxCasesDF , estadoTxt, aretirar,saveToFile,pathToSave){
	edoCasesDF <- mxCasesDF[ mxCasesDF$RESULTADO == resultadoConfirmado,c("FECHA_INGRESO","RESULTADO")]

	casos <- aggregate(formula = RESULTADO ~ FECHA_INGRESO,
	           FUN = sum,
	           data = edoCasesDF)
	if (aretirar > 0 ) {
        casos <-head(casos,-aretirar)
    }
	R_estimate <- estimate_R(casos$RESULTADO ,method = "parametric_si",
	                  config = make_config(list(
	                  mean_si = 3.9, std_si = 4.5)))


#	 plot

	if (saveToFile == TRUE) {
	#save
	# 1. Open jpeg file
		setwd(pathToSave)
		png(paste(estadoTxt, ".png",sep=""), width = 800, height = 600)
	# 2. Create the plot
	}
	plot(R_estimate)

	if (saveToFile == TRUE) {
		# 3. Close the file
		dev.off()
		write.csv(R_estimate$R, paste(estadoTxt,".csv",sep=""))
	}

	print(R_estimate$R)

 result <-1
 return(result)
}



#download and load into dataframe
setwd(mydir)
download.file(myurl, myfile )
unzipfile <- unzip (myfile, list = TRUE)
unzip (myfile, unzipfile$Name)
mxCasesDF <- read.csv ( file=unzipfile$Name)

#fill state names

#run graphs increment vs acumulated


#dataframe with country wide cases, Country  name, days to cut off	, savetoFile, path
  calculaREstado (mxCasesDF , "Mexico", 3, TRUE, paste(mydir,"/img",sep=""))
