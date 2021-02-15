################################################################################
################Script 5: Geocodificação por meio da API do google##############
################################################################################

#O que esse script faz?:
#Geocodificacao dos enderecos completos por meio da api do google maps
#Extração dos campos de interesse (status da busca, endereco completo, 
#latitute, longitude, tipo (rooftop, range_interpolated, geometric_center, 
#approximated), matching parcial.

# Para utilizacao da API do google e´ necessario cadastrar o projeto e ativar a API key : 
#https://developers.google.com/maps/documentation/geocoding/start
# A API compara os enderecos informados com a base do Google Maps
# Detalhes sobre os resultados/saida :
#https://developers.google.com/maps/documentation/geocoding/intro#Results 

################################################################################
#Definir area de trabalho: local onde estao os arquivos (no formato .csv ) 
#com os enderecos 
#setwd("/media/BANCO SIM")

#Abrir ou instalar pacotes
if(!require(ggmap))install.packages('ggmap'); library(ggmap)

###################################################
#Abrir os arquivos/bancos com os enderecos no formato .csv já padronizados

bdgeral=read.csv(file = "BANCO_formatado.csv", stringsAsFactors = FALSE)
bdgeral=subset(bdgeral, !is.na(bdgeral$ENDCOMPLETO_F))

#Inserir a chave do projeto 
gkey = "xxxxxxxxxxxxxxxxxxx"
register_google(key = gkey)

###################################################
#Geocode via google API
# Aplicar a funcao geocode (ggmap) em cada endereço, 
#a) Extrair os resultados referentes ao status da busca
#b) Extrair Endereco completo retornado pelo google,
#c) Latitute, 
#d) Longitude  
#e) Tipo de geocoding (rooftop, range_interpolated., geometric_center, approximated)
#f) Matching parcial (partial ou NA)
# e inclui cada item dos resultados no banco de dados

for(i in 1:nrow(bdgeral))
{
  print(i)
  resultgeo <-
    geocode(bdgeral$ENDCOMPLETO_F[i],
            output = "all",
            source = "google")
  if (is.null(resultgeo$status)) {
    bdgeral$STATUS[i] = "Error"
    bdgeral$ENDCOMPLETO_g[i] = NA
    bdgeral$LATITUDE[i] = NA
    bdgeral$LONGITUDE[i] = NA
    bdgeral$PRECISAO[i] = NA
    bdgeral$ENDTIPO_g[i] = NA
    bdgeral$MACTH[i] = NA
    
    next
  }
  bdgeral$STATUS[i] <- as.character(resultgeo[["status"]])
  if (bdgeral$STATUS[i] == "OK") {
    bdgeral$ENDCOMPLETO_g[i] = as.character(resultgeo[["results"]][[1]][["formatted_address"]])
    bdgeral$LATITUDE[i] =  as.numeric(resultgeo[["results"]][[1]][["geometry"]][["location"]][["lat"]])
    bdgeral$LONGITUDE[i] = as.numeric(resultgeo[["results"]][[1]][["geometry"]][["location"]][["lng"]])
    bdgeral$PRECISAO[i] = as.character(resultgeo[["results"]][[1]][["geometry"]][["location_type"]])
    bdgeral$ENDTIPO_g[i] = as.character(resultgeo[["results"]][[1]][["types"]][[1]])
    
    if (!is.null(resultgeo[["results"]][[1]][["partial_match"]])) {
      bdgeral$MACTH[i] = as.character(resultgeo[["results"]][[1]][["partial_match"]])
      
    }
  }
}
##############################################################################
write.csv(bdgeral, file="BANCO_georreferenciado.csv")

#Limpar a area de trabalho, mantendo o que sera utilizado no proximo script
rm(list= ls()[!(ls() %in% c('bdgeral', 'bairros_list', 
                            'bairros_lista', 'dicionario_bairro', 
                            'dicionario_complemento', 'dicionario_numbers'))])



