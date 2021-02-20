# Parser XML 
# Ejemplo Boletin COVID 19
# Curso: Lenguajes de Marcado para SIG en WEB
# instalo librerias, si no las tengo previamente instaladas

#install.packages("xml2")
#install.packages("htmltools")
#install.packages("pipeR")

#requiero las librerias previamente instaladas
library(xml2)
library(htmltools)
library(pipeR)


#leo el documento xml
xmldocument <- read_xml(x="boletines.xml", options = "DTDVALID")

name<-xml_name(xmldocument)
boletinesDoc <- xml_children(xmldocument)
boletinDocTags <- xml_name(boletinesDoc)

fechaEmision <- xml_text( xml_find_all(xmldocument, ".//fecha_emision") )
horaEmision <- xml_text( xml_find_all(xmldocument, ".//hora_emision"))
fechaSituacionInicial <- xml_text( xml_find_all(xmldocument, ".//fecha_situacion_inicial"))
fechaSituacionFinal <- xml_text( xml_find_all(xmldocument, ".//fecha_situacion_final"))
elaboro <- xml_text( xml_find_all(xmldocument, ".//elaboro"))
apoyo <- xml_text( xml_find_all(xmldocument, ".//apoyo"))
fuentes <- xml_text( xml_find_all(xmldocument, ".//fuentes"))

#cuento el numero de boletines
numero_boletines <- (length(boletinesDoc))
tabSpace <- "&nbsp;&nbsp;&nbsp;&nbsp;"

#creo una variable de texto vacia para llenarla de forma paulatina con el informe construido en HTML
boletinHTML <- ""
for(i in 1:numero_boletines) 
{
  boletinNumero = xml_attrs(xml_child(xmldocument, i))[["numero"]]
  
  boletinHTML <-  paste(boletinHTML,"<h3>Boletín Diario COVID 19 - CALI</h3>", sep="")
  boletinHTML <-  paste(boletinHTML,"<p>Boletin No. <b>", boletinNumero ,"</b>", sep="")
  boletinHTML <-  paste(boletinHTML, tabSpace,fechaEmision[i],horaEmision[i],"<br>",  sep="")
  boletinHTML <-  paste(boletinHTML,"Elaboró:", elaboro[i],"<br>", sep="")
  boletinHTML <-  paste(boletinHTML,"Apoyó:", apoyo[i],"<br>", sep="")
  boletinHTML <-  paste(boletinHTML,"Fuentes:", fuentes[i],"<br><br><p>", sep="")
  boletinHTML <-  paste(boletinHTML,"Según el instituto nacional de salud, esta es la situación a la fecha:<br><br>","<center><b>Situación Epidemiolígica del COVID-19</b><br>" , fechaSituacionInicial[i]," a ", fechaSituacionFinal[i],"</center>", sep="")
  
  datosChild <- xml_children(xml_find_all(xml_child(xmldocument, i), ".//datos"))
  numero_datos_detalle <- (length(datosChild))
  
  tablaGeneral <- "<table style='width:100%'>
  <tr>
    <th>Descripción</th>
    <th class='naranja' >Infectados</th>
    <th class='naranja' >Activos</th>
    <th class='verde' >Recuperados</th>
    <th class='verde' >% RecuperaciÃ³n</th>
    <th class='rojo' >Fallecidos</th>
    <th class='rojo' >% Letalidad</th>
  </tr>"
  
  municipioData[] <- array()
  
  for(j in 1:numero_datos_detalle) 
  {
    descripcion <- xml_attrs(datosChild[[j]])[["descripcion"]]
    nivel <- xml_attrs(datosChild[[j]])[["nivel"]]
    infectados <- xml_text( xml_find_all(datosChild[j], ".//infectados") )
    activos <- xml_text( xml_find_all(datosChild[j], ".//activos") )
    recuperados <- xml_text( xml_find_all(datosChild[j], ".//recuperados") )
    fallecidos <- xml_text( xml_find_all(datosChild[j], ".//fallecidos") )
    
    #Calculos Letalidad y Recuperados
    p_recuperados = round((as.numeric(recuperados)/as.numeric(infectados))*100,1)
    p_letalidad=round((as.numeric(fallecidos)/as.numeric(infectados))*100,1)
    
    #Si elemento recorrido es un municipio, almaceno su contenido para despues usarlo en tabla resumen (Municipio Cali)
    if(nivel =='municipio')
    {
      municipioData <- c(infectados,activos,recuperados,fallecidos,p_recuperados,p_letalidad)
    }
    
    tablaGeneral <-  paste(tablaGeneral,"<tr class='", nivel, "'>", sep="")
    tablaGeneral <-  paste(tablaGeneral,"<td>",descripcion,"</td>", sep="")
    tablaGeneral <-  paste(tablaGeneral,"<td>",infectados,"</td>", sep="")
    tablaGeneral <-  paste(tablaGeneral,"<td>",activos,"</td>", sep="")
    tablaGeneral <-  paste(tablaGeneral,"<td>",recuperados,"</td>", sep="")
    tablaGeneral <-  paste(tablaGeneral,"<td>",p_recuperados,"</td>", sep="")
    tablaGeneral <-  paste(tablaGeneral,"<td>",fallecidos,"</td>", sep="")
    tablaGeneral <-  paste(tablaGeneral,"<td>",p_letalidad,"</td>", sep="")
    tablaGeneral <-  paste(tablaGeneral,"</tr>", sep="")
  }
  tablaGeneral <-  paste(tablaGeneral,"</table>", sep="")
  
  #creo tabla resumen cali
  tablaResumenCali <- "<table style='width:100%'>"
  tablaResumenCali <-  paste(tablaResumenCali,"<tr><td class='naranja fuenteazul' >Confirmados<br><b>", sep="")
  tablaResumenCali <-  paste(tablaResumenCali,municipioData[1],"</b></td><td class='naranja-amarillo'>Casos Activos<br><b>", sep="")
  tablaResumenCali <-  paste(tablaResumenCali,municipioData[2],"</b></td></tr>", sep="")
  
  tablaResumenCali <-  paste(tablaResumenCali,"<tr><td class='rojo fuenteazul' >Fallecidos<br><b>", sep="")
  tablaResumenCali <-  paste(tablaResumenCali,municipioData[4],"</b></td><td class='verde fuenteazul'>Recuperados<br><b>", sep="")
  tablaResumenCali <-  paste(tablaResumenCali,municipioData[3],"</b></td></tr>", sep="")
  
  tablaResumenCali <-  paste(tablaResumenCali,"<tr><td class='rojo fuenteazul' >Letalidad<br><b>", sep="")
  tablaResumenCali <-  paste(tablaResumenCali,municipioData[6],"%</b></td><td class='verde fuenteazul'>Recuperados<br><b>", sep="")
  tablaResumenCali <-  paste(tablaResumenCali,municipioData[5],"%</b></td></tr>", sep="")
  
  tablaResumenCali <-  paste(tablaResumenCali,"</table>", sep="")
  
  #fin de la tabla resumen cali  
  
  boletinHTML <-  paste(boletinHTML,tablaResumenCali, sep="")
  boletinHTML <-  paste(boletinHTML,"<p><center><b>Comportamiento Casos COVID-19</b><br>" , fechaSituacionInicial[i]," a ", fechaSituacionFinal[i],"</center></p>", sep="")
  boletinHTML <-  paste(boletinHTML,tablaGeneral, sep="")
  boletinHTML <-  paste(boletinHTML,"<hr/><hr/>", sep="")
}

css <- "<style>
table, th, td {
  border: 1px solid black;
  border-collapse: collapse;
}

.pais {
  background-color: #D5FDFF; 
}

.departamento {
  background-color: #D8FFD5; 
}

.municipio {
  background-color: #FFFCD5; 
}

.naranja
{
  background-color: #FFBC00;
}

.naranja-amarillo
{
  background-color: #FDD300;
  border-style: double;
}

.verde
{
  background-color: #05AB2B;
}

.rojo
{
  background-color: #FF0000;
}

.fuenteazul
{
  color: #1E0FF0;
}
</style>"

#consolido estilo css y contenido HTML del boletin 
boletinHTML <-  paste(css, boletinHTML, sep="")

#Imprimo resultado HTML en el visor de RStudio
paste(boletinHTML  , sep ="")%>>%
  HTML %>>%
  html_print


############### FIN ###################


