#Limpiamos area de crabajo
rm(list=ls())
#ls()


#Cargamos la libreria RMySQL
library(RMySQL)


#Funcion para enviar Query

sqlQuery <- function (query,youruser="mysqldb",yourpassword="Exito1321!",
yourdb="mysqldb",
yourhost="nvirginia-mysql-instance1.c6nnllocq12q.us-east-1.rds.amazonaws.com"){
  #Conexion con la DB
  DB <- dbConnect(MySQL(), user=youruser, password=yourpassword,
			dbname=yourdb, host=yourhost)
  #Enviar Query
  rs <- dbSendQuery(DB, query)
  #Convertir resultado en data frame
  #result <- fetch(rs, -1)
  #Cerrar la conexion
  #Obtener el resultado
  dbDisconnect(DB)  
  #print(result)
}

#FUNCION PARA EXTRAER INDICES

indexValue<-function(index){

	#Introducimos el nombre del indice
	url1<-"https://finance.yahoo.com/quote/%5E"
	url2<-"?p=^"
	url<-paste0(url1,index,url2,index)

	#Leemos la pagina web con readLines
	pageText<-readLines(url,warn=F)
	class(pageText); typeof(pageText); mode(pageText)
	length(pageText)

	#Un poco de regular expresions
	z<-as.character("D\\(ib\\)\" data-reactid=\"36\">")
	#cat(z)

	#Ubicamos las posiciones del vector PageText en donde esta el nombre del indice
	pageTextPos<-grep("D\\(ib\\)\" data-reactid=\"36\">",pageText)
	pageTextPos

	#Extraemos las posiciones del vector pageText
	indexText<-pageText[pageTextPos]
	class(pageText); typeof(pageText); mode(pageText)
	length(indexText)

	#Trucazo con regular expresions
	gsub(".*D\\(ib\\)\" data-reactid=\"36\">\\s*|</span>.*", "", indexText)
}


#Funcion para insertar datos

insertSQL<- function(index_id,index_name){
	#Funcion para extrer dato de yahoo finance
	value<-indexValue(index_id)
	#Convertimos dato a numerico
	value<-as.numeric(gsub(",","",value))
	#Formamos el Query
	query<-paste0("INSERT INTO yahoo_index (nombre_indice,valor_indice) VALUES ('",index_name,"',",value,");")
	#Insertamos dato	
	sqlQuery(query)
}


#Function market Time

marketTime<-function() {
	#Días de la semana en los que abre el mercado
	diasHabiles<-c("Monday","Tuesday","Wednesday","Thursday","Friday")

	#Hora en la que abre y en la que cierra
	open<-strptime("14:30","%H:%M")
	close<-strptime("21:01","%H:%M")
	dia<-weekdays(Sys.Date())
	hora<-as.POSIXlt(Sys.time())$hour
	minuto<-as.POSIXlt(Sys.time())$min
	hora_minuto<-strptime(paste0(hora,":",minuto),"%H:%M")
	
	#Mercado abierto True, mercado cerrado False
	all(is.element(dia,diasHabiles),hora_minuto>=open,hora_minuto<close)
}
#huevos

repeat {

	if (marketTime()){
		insertSQL("gspc","S&P 500")
		insertSQL("dji","Dow Jones")
		insertSQL("ixic","Nasdaq")
	}

	Sys.sleep(60*30)
}

