################################################################################
####################Script 6: Validação dos endereços###########################
################################################################################

#Separação e padronização dos campos de endereço retornados pelo google.
#Comparação entre os campos (logradouro, número, bairro, cidade) 
#do endereco original e os campos do endereço retornado pelo google.
#Comparação entre as informações de logradouro e bairro obtidas 
#por meio da base dos correios


################################################################################
#Instalar e carregar os pacotes do R
if (!require(foreign))install.packages("foreign"); library(foreign)
if (!require(stringr)) install.packages('stringr'); library(stringr)
if (!require(gdata))install.packages('gdata'); library(gdata)
if (!require(stringdist))install.packages('stringdist'); library(stringdist)
if (!require(data.table))install.packages('data.table'); library(data.table)
if (!require(dplyr))install.packages('dplyr'); library(data.table)

################################################################################
#Importar banco formatado e georreferenciado
bdgeral= read.csv("BANCO_georreferenciado.csv", stringsAsFactors = F)

#############################################################################
##importar a tabela com os tipos (rua, vila, avenida erc) dos logradouros no RJ
tipo=read.csv("tipos_rj.csv", stringsAsFactors = F)

#Pares de tipos (p.e, Rua, Vila) com distancia de levenshtein <=3
tipos_problema=read.csv("tipos_problema.csv", stringsAsFactors = F) 
tipos_problema=paste(tipos_problema$TIPO1,tipos_problema$TIPO2, sep = ",")

# Logradouros com nomes repetidos em diferentes bairros 
tipo_rep= read.csv("tipos_duplicata_bairro.csv", stringsAsFactors = F) 

# Importar bairros que fazem fronteira
limites=read.csv("limites_bairros_edit.csv", stringsAsFactors = F)

#Importar e editar a lista de bairros da cidade 
bairros_list = read.csv("bairros.csv")
bairros_list$Bairros = iconv(bairros_list$Bairros, from = "latin1", to = "UTF-8")
bairros_list$Bairros = str_to_upper(bairros_list$Bairros)
bairros_list$Bairros = gsub("[[:punct:]]+", " ", bairros_list$Bairros)
bairros_list$Bairros = gsub("[[:space:]]+", ' ', bairros_list$Bairros)
bairros_lista = unlist(trim(bairros_list$Bairros))

## Importar dicionario de bairros da cidade formato: 'from' bairro grafia errada;  
#'to' bairro grafia correta 
dicionario_bairro = read.csv("dicionarios_trad4.csv")
dicionario_bairro$from = gsub("[[:space:]]+", ' ', dicionario_bairro$from)
dicionario_bairro$to = gsub("[[:space:]]+", ' ', dicionario_bairro$to) 
dicionario_bairro$from = trim(str_to_upper(dicionario_bairro$from))
dicionario_bairro$to = trim(str_to_upper(dicionario_bairro$to))

## Importar dicionario de ruas  (formato: 'from' rua grafia errada ; 
#'to' rua grafia correta
dicionario = read.csv("dicionario_titulos.csv")
dicionario$from = gsub("[[:space:]]+", ' ', dicionario$from)
dicionario$from =trim(dicionario$from)
dicionario$to = trim(dicionario$to)
dicionario$from = paste0('\\b',dicionario$from,'\\b')
dicionario$to = paste(" ",dicionario$to, " ")

## Importar dicionario para o campo complemento
dicionario_complemento = read.csv("dicionario_complemento_3.csv")
dicionario_complemento$from = trim(gsub('[[:space:]]+', ' ', dicionario_complemento$from))
dicionario_complemento$to = trim(gsub('[[:space:]]+', ' ', dicionario_complemento$to))
dicionario_complemento$from= paste0('\\b',dicionario_complemento$from,'\\b')
dicionario_complemento$to = str_to_upper(paste(' ',dicionario_complemento$to,' '))

##Importar dicionario de traducao numeros por extenso
dicionario_numbers=read.csv("dicionario_numbers.csv", sep=',')
dicionario_numbers$from = trim(gsub('[[:space:]]+', ' ', dicionario_numbers$from))
dicionario_numbers$to = trim(gsub('[[:space:]]+', ' ', dicionario_numbers$to))
dicionario_numbers$from = paste0('\\b',dicionario_numbers$from,'\\b')
dicionario_numbers$to = str_to_upper(paste(' ',dicionario_numbers$to,' '))

############################################################################
#Edição dos endereços retornados pelo google:
###Separa em rua, numero e bairro, cidade e CEP o endereco completo retornado pelo google 
# A ordem dos campos varia de acordo com o tipo de end ("street_address", "premisse")
#Exemplos:
#"Rua XXXX, numero Y- BANGU, RIO DE JANEIRO - RJ, cep XXXX, BRAZIL":
#tem  status "premise" e contem 7 campos
#"CINELÂNDIA - CENTRO, RIO DE JANEIRO - RJ, BRAZIL":  5 campos tem  status locl
# Rua XXXX, numero, Ilha da Gigoia - Barra da Tijuca, 
#Rio de Janeiro - RJ, cep xxxx, Brazil: tem  status "bakery" e contem 8 campos

#############################################################################
bdgeral$ENDTIPO_g=as.character(bdgeral$ENDTIPO_g)
bdgeral$ENDTIPO_g[is.na(bdgeral$ENDTIPO_g)]= "ERROR"

bdgeral$ENDCOMPLETO_g=toupper(bdgeral$ENDCOMPLETO_g)

status=NULL
list_end= NULL

for (i in 1:length(bdgeral$ENDCOMPLETO_g)) {
  print(i)
  status[i] = trim(bdgeral$ENDTIPO_g[i])
  bdgeral$ENDNUMERO_g[i] = str_extract(endereco[i], "[,] [[:digit:]]+")
  bdgeral$ENDCOMPL1_g[i] = NA
  list_end[i] = strsplit(endereco[i], ",")
  bdgeral$ENDRUA_g[i] = unlist(list_end[i])[1]
  
  if (!is.na(status[i]) == TRUE) {
    if (status[i] == "subpremise") {
      bdgeral$ENDCOMPL1_g[i] = unlist(list_end[i])[3]
      bdgeral$ENDBAIRRO_g[i] = unlist(list_end[i])[4]
      bdgeral$ENDCEP_g[i] = unlist(list_end[i])[7]
      
    } else if (status[i] == "route") {
      bdgeral$ENDBAIRRO_g[i] = unlist(list_end[i])[2]
      bdgeral$ENDCEP_g[i] = unlist(list_end[i])[5]
      
    } else {
      bdgeral$ENDBAIRRO_g[i] = unlist(list_end[i])[3]
      bdgeral$ENDCEP_g[i] = unlist(list_end[i])[6]
      
    }
  }
  
}   

bdgeral$ENDNUMERO_g=as.numeric(gsub("[[:punct:]]+", "", bdgeral$ENDNUMERO_g))#remover pontuacao
bdgeral$ENDNUMERO_g=as.numeric(gsub("[[:space:]]+", "", bdgeral$ENDNUMERO_g))#remover espacos duplos

bdgeral$ENDCOMPL_g=gsub("[[:punct:]]+", " ", bdgeral$ENDCOMPL1_g)  
bdgeral$ENDCOMPL_g=gsub("[[:space:]]+", " ", bdgeral$ENDCOMPL_g) 
bdgeral$ENDCOMPL_g= trim(bdgeral$ENDCOMPL_g)

bdgeral$ENDRUA_g=gsub("&", " ESQ ", bdgeral$ENDRUA_g)  #indicar esquina
bdgeral$ENDRUA_g=gsub("[[:punct:]]+", " ", bdgeral$ENDRUA_g)  
bdgeral$ENDRUA_g=gsub("[[:space:]]+", " ", bdgeral$ENDRUA_g) 
bdgeral$ENDRUA_g=stringi::stri_trans_general(bdgeral$ENDRUA_g, "Latin-ASCII") #remover acentos
bdgeral$ENDRUA_g=trim(bdgeral$ENDRUA_g)

bdgeral$ENDBAIRRO_g=sub("[[:digit:]]+", "", bdgeral$ENDBAIRRO_g) #remover digitos
bdgeral$ENDBAIRRO_g=gsub("[[:punct:]]+", "", bdgeral$ENDBAIRRO_g)
bdgeral$ENDBAIRRO_g=stringi::stri_trans_general(bdgeral$ENDBAIRRO_g, "Latin-ASCII") 
bdgeral$ENDBAIRRO_g= trim(bdgeral$ENDBAIRRO_g)


######################################################
# Edita o nome da rua retornada pelo google para comparacao com o original
endereco=bdgeral$ENDRUA_g

#Correcao do tipo: 'Função' (for loop endnovo) adaptada de Silveira et al (2017): https://www.scielo.br/j/ress/a/Zd8DBfbVhVwGXZRvVVxPm3M/abstract/?lang=pt
# Separa os endereços por elementos/palavras 
#Compara somente o primeiro elemento com a primeira coluna em "tipolog" 
#substitui pela segunda coluna em "tipolog"
#Combina o primeiro elemento editado com os demais 

endnovo = NULL
for (i in 1:length(endereco)) {
  endsplit = unlist(strsplit(endereco[i], " "), recursive = TRUE)
  
  tipolog = gsub("[[:digit:]]+", "", endsplit[1])
  novotipo = switch(
    tipolog,
    "AV" = "AVENIDA",
    "A" = "AVENIDA",
    "ACENIDA" = "AVENIDA",
    "AVENIA" = "AVENIDA",
    "AVENIA" = "AVENIDA",
    "AQV" = "AVENIDA",
    "AVEN" = "AVENIDA",
    "R" = "RUA",
    "TRUA" = "RUA",
    "TRV" = "TRAVESSA",
    "TRAV" = "TRAVESSA",
    "TV" = "TRAVESSA",
    "T" = "TRAVESSA",
    "TR" = "TRAVESSA",
    "TRA" = "TRAVESSA",
    "PRC" = "PRACA",
    "PC" = "PRACA",
    "PCA" = "PRACA",
    "PR" = "PRACA",
    "BC" = "BECO",
    "EST" = "ESTRADA",
    "ES" = "ESTRADA",
    "ESR" = "ESTRADA",
    "ESTR" = "ESTRADA",
    "ESRT" = "ESTRADA",
    "ETR" = "ESTRADA",
    "ESRADA" = "ESTRADA",
    "STRADA" = "ESTRADA",
    "ESTRDA" = "ESTRADA",
    "ETSR" = "ESTRADA",
    "GST" = "ESTRADA",
    "RAU" = "RUA",
    "RIA" = "RUA",
    "RUIA" = "RUA",
    "RA" = "RUA",
    "UA" = "RUA",
    "TUA" = "RUA",
    "AC" = "AVENIDA",
    "ALAMEDE" = "ALAMEDA",
    "RKUA" = "RUA",
    "AVENIDQ" = "AVENIDA",
    "RYUA" = "RUA",
    "LAD" = "LADEIRA",
    "LD" = "LADEIRA",
    tipolog
  )
  endsplit[1] = novotipo
  endnovo = c(endnovo, paste(endsplit, collapse = " "))
}

bdgeral$ENDRUA_g= endnovo
###Substituir termos de acordo com o dicionario 
for (i in seq_len(nrow(dicionario))) {
  bdgeral$ENDRUA_g=  gsub(dicionario$from[i], paste(" ", dicionario$to[i], " "),
                          bdgeral$ENDRUA_g)
}

bdgeral$ENDRUA_g=gsub("[[:space:]]+", " ", bdgeral$ENDRUA_g) 

#traducao de numeros
endres=as.character(bdgeral[,"ENDRUA_g"])

for (i in seq_len(nrow(dicionario_numbers))) {
  endres=  gsub(dicionario_numbers$from[i], dicionario_numbers$to[i],
                endres)
}


bdgeral$ENDRUA_gnum=endres
bdgeral$ENDRUA_gnum=trim(gsub('[[:space:]]+', ' ',bdgeral$ENDRUA_gnum)) 


bdgeral$ENDBAIRRO_g = trim(gsub("[[:space:]]+", " ", bdgeral$ENDBAIRRO_g))

for (i in seq_len(nrow(dicionario_bairro))) {
  bdgeral$ENDBAIRRO_g =  gsub(dicionario_bairro$from[i],
                              dicionario_bairro$to[i],
                              bdgeral$ENDBAIRRO_g)
}

########################################################

bdgeral$ENDCOMPL_g = ifelse(
  bdgeral$ENDNUMERO_g == bdgeral$ENDCOMPL_g,
  bdgeral$ENDNUMERO_g,
  paste(bdgeral$ENDNUMERO_g, bdgeral$ENDCOMPL_g, sep =
          " ")
)

bdgeral$ENDCOMPL_g = gsub("NA", ' ', bdgeral$ENDCOMPL_g)
bdgeral$ENDCOMPL_g = gsub("[[:punct:]]+", ' ' , bdgeral$ENDCOMPL_g)
bdgeral$ENDCOMPL_g = gsub("[[:space:]]+", ' ' , bdgeral$ENDCOMPL_g)
bdgeral$ENDCOMPL_g = trim(bdgeral$ENDCOMPL_g)

bdgeral$ENDRUA_g = gsub("RIO DE JANEIRO", "", bdgeral$ENDRUA_g)
bdgeral$ENDBAIRRO_g = gsub("RIO DE JANEIRO", "", bdgeral$ENDBAIRRO_g)
bdgeral$ENDBAIRRO_g = gsub("RECREIOANTES", "RECREIO", bdgeral$ENDBAIRRO_g)

bdgeral$ENDBAIRRO_g = gsub("BRAZIL", "", bdgeral$ENDBAIRRO_g)
bdgeral$ENDBAIRRO_g = gsub("STATE OF RIO DE JANEIRO", "", bdgeral$ENDBAIRRO_g)
bdgeral$ENCEP_g2 = gsub("[:alpha:]]+", "", bdgeral$ENDCEP_g)


###################################
#Separação os endereços do google e original
#(incluindo aqueles com "tradução" de números), por tipo (rua, avenida etc) 
#e demais campos

#Solucao rapida: Incluir * no inicio, para facilitar a identificação do tipo
# (apenas no inicio da frase - nao apenas no inicio da string indicada: ^ ou //b)

bdgeral$LOGRATIPO = paste0("*", bdgeral$ENDRUA)
bdgeral$LOGRATIPO_num = paste0("*", bdgeral$ENDRUA_num)

bdgeral$LOGRATIPO_g = paste0("*", bdgeral$ENDRUA_g)
bdgeral$LOGRATIPO_gnum = paste0("*", bdgeral$ENDRUA_gnum)

patterns = paste0("[*]", tipo$TIPOS, "[[:space:]]")
patterns = paste0(patterns, collapse = "|")

bdgeral$TIPO = vapply(strsplit(bdgeral$LOGRATIPO, "[[:space:]]"), "[", "", 1)
bdgeral$TIPO = ifelse(
  str_detect(bdgeral$LOGRATIPO, patterns),
  gsub("[[:punct:]]", "", bdgeral$TIPO), "ST")

bdgeral$TIPO_g = vapply(strsplit(bdgeral$LOGRATIPO_g, "[[:space:]]"), "[", "", 1)
bdgeral$TIPO_g = ifelse(str_detect(bdgeral$LOGRATIPO_g, patterns),
  gsub("[[:punct:]]", "", bdgeral$TIPO_g),"ST")

bdgeral$LOGRATIPO = vapply(strsplit(bdgeral$LOGRATIPO, patterns), "[", "", 2)
bdgeral$LOGRATIPO_num = vapply(strsplit(bdgeral$LOGRATIPO_num, patterns), "[", "", 2)

bdgeral$LOGRATIPO = ifelse(is.na(bdgeral$LOGRATIPO), bdgeral$ENDRUA, bdgeral$LOGRATIPO)
bdgeral$LOGRATIPO_num = ifelse(is.na(bdgeral$LOGRATIPO_num),
                               bdgeral$ENDRUA_num,
                               bdgeral$LOGRATIPO_num)

bdgeral$LOGRATIPO_g = vapply(strsplit(bdgeral$LOGRATIPO_g, patterns), "[", "", 2)
bdgeral$LOGRATIPO_gnum = vapply(strsplit(bdgeral$LOGRATIPO_gnum, patterns), "[", "", 2)

bdgeral$LOGRATIPO_g = ifelse(is.na(bdgeral$LOGRATIPO_g),
                             bdgeral$ENDRUA_g,
                             bdgeral$LOGRATIPO_g)
bdgeral$LOGRATIPO_gnum = ifelse(is.na(bdgeral$LOGRATIPO_gnum),
                                bdgeral$ENDRUA_gnum,
                                bdgeral$LOGRATIPO_gnum)

bdgeral$LOGRATIPO = trim(gsub("[[:space:]]+", ' ', bdgeral$LOGRATIPO))
bdgeral$LOGRATIPO_num = trim(gsub("[[:space:]]+", ' ', bdgeral$LOGRATIPO_num))

bdgeral$LOGRATIPO_g = trim(gsub("[[:space:]]+", ' ', bdgeral$LOGRATIPO_g))
bdgeral$LOGRATIPO_gnum = trim(gsub("[[:space:]]+", ' ', bdgeral$LOGRATIPO_gnum))

#Verificar se os tipos estao na lista de tipos problematicos : com Lv<3
bdgeral$TIPO_status = ifelse(bdgeral$TIPO_par %in% tipos_problema, "TRUE", "FALSE")
#Verificar se os nomes da rua estao na lista nomes problematicos : mesmo nome bairros distintos
bdgeral$LOGRATIPO_status = ifelse(bdgeral$LOGRATIPO %in% tipo_rep, "TRUE", "FALSE")
bdgeral$TIPO_par = paste(bdgeral$TIPO, bdgeral$TIPO_g, sep = ",")
# Se o numero da casa e par o impar
bdgeral$PAR_IMPAR = ifelse(e_par(bdgeral$ENDNUMERO) == TRUE, "LADO PAR", "LADO IMPAR")

###############################################################
# Calcula stringdist/comparacao dos enderecos

bdgeral$LV_RUA = stringdist(bdgeral$ENDRUA_g, bdgeral$ENDRUA, method = 'lv')
bdgeral$LV_RUA_num = stringdist(bdgeral$ENDRUA_gnum, bdgeral$ENDRUA_num, method =
                                  'lv')

bdgeral$IGUAL_NUMERO = bdgeral$ENDNUMERO_g == bdgeral$ENDNUMERO
bdgeral$IGUAL_COMPLEMENTO = bdgeral$ENDCOMPL_g == bdgeral$ENDCOMPL
bdgeral$IGUAL_COMPLEMENTO2 = bdgeral$ENDNUMERO_g == bdgeral$ENDCOMPL
bdgeral$IGUAL_COMPLEMENTO = as.character(bdgeral$IGUAL_COMPLEMENTO)

bdgeral$LV_BAIRRO = stringdist(bdgeral$ENDBAIRRO_g, bdgeral$BAIRRO_F, method =
                                 'lv')

bdgeral$IGUAL_COMPLEMENTO <-
  ifelse(is.na(bdgeral$IGUAL_COMPLEMENTO),
         'FALSE',
         bdgeral$IGUAL_COMPLEMENTO)

bdgeral$IGUAL_COMPLEMENTO2 <-
  ifelse(is.na(bdgeral$IGUAL_COMPLEMENTO2),
         'FALSE',
         bdgeral$IGUAL_COMPLEMENTO2)

bdgeral$IGUAL_NUMERO <- ifelse(is.na(bdgeral$IGUAL_NUMERO),
                               'FALSE', bdgeral$IGUAL_NUMERO)

bdgeral$IGUAL_NUMERO_todos = ifelse(
  bdgeral$IGUAL_NUMERO == TRUE |
    bdgeral$IGUAL_COMPLEMENTO == TRUE |
    bdgeral$IGUAL_COMPLEMENTO2 == TRUE,
  "TRUE",
  "FALSE"
)

###########################################################
bdgeral$DL_0 = ifelse(
  bdgeral$LV_RUA == 0 &
    bdgeral$LV_BAIRRO == 0 & bdgeral$IGUAL_NUMERO_todos == TRUE,
  "TRUE",
  ifelse(
    bdgeral$LV_RUA_num == 0 &
      bdgeral$LV_BAIRRO == 0 &
      bdgeral$IGUAL_NUMERO_todos == TRUE,
    "TRUE",
    "FALSE"
  )
)


bdgeral$DL_0 <- ifelse(is.na(bdgeral$DL_0), 
                       'FALSE', bdgeral$DL_0)

###############################################################################
#Merge com  a base de endereços dos Correios (DNE)
###############################################################################

base_cep_RJ=read.csv(file = "base_cep_RJ_OFICIAL_1809_MunRJ.csv", 
                     stringsAsFactors = F)


base_cep_RJ_merge=subset(base_cep_RJ, 
                         select = c(CEP, BAIRRO_base_F, ENDRUA_base,
                         ENDRUA_base_num, TIPO_base, LOGRATIPO_base_num, 
                         ENDRUA_base_alt, ENDRUA_base_alt2, BAIRRO_base_alt, 
                         BAIRRO_base_alt2))

base_cep_RJ_g=subset(base_cep_RJ, 
                     select = c(CEP, BAIRRO_base_F, ENDRUA_base,
                     ENDRUA_base_num, TIPO_base, LOGRATIPO_base_num, 
                     ENDRUA_base_alt, ENDRUA_base_alt2, BAIRRO_base_alt, 
                     BAIRRO_base_alt2))


names(base_cep_RJ_g)
names(base_cep_RJ_merge)
names(base_cep_RJ_merge)[1]="CEPRES"

names(base_cep_RJ_g)[1]="ENDCEP_g"
names(base_cep_RJ_g)[2]="BAIRRO_base_g"
names(base_cep_RJ_g)[3]="ENDRUA_base_g"
names(base_cep_RJ_g)[4]="ENDRUA_base_num_g"
names(base_cep_RJ_g)[5]="TIPO_base_g"
names(base_cep_RJ_g)[6]="LOGRATIPO_base_num_g"
names(base_cep_RJ_g)[7]="ENDRUA_base_alt_g"
names(base_cep_RJ_g)[8]="ENDRUA_base_alt2_g"
names(base_cep_RJ_g)[9]="BAIRRO_base_alt_g"
names(base_cep_RJ_g)[10]="BAIRRO_base_alt2_g"


bdgeral$CEPRES=ifelse(nchar(bdgeral$CEPRES)<8, NA,bdgeral$CEPRES)
bdgeral$ENDCEP_g=ifelse(nchar(bdgeral$ENDCEP_g)<8, NA,bdgeral$ENDCEP_g)

bdgeral$CEPRES=as.character(bdgeral$CEPRES)
bdgeral$CEPRES=trim(bdgeral$CEPRES)

bdgeral$ENDCEP_g=as.character(bdgeral$ENDCEP_g)
bdgeral$ENDCEP_g=trim(bdgeral$ENDCEP_g)


base_cep_RJ_merge$CEPRES=as.character(base_cep_RJ_merge$CEPRES)
base_cep_RJ_merge$CEPRES=trim(base_cep_RJ_merge$CEPRES)

base_cep_RJ_g$ENDCEP_g=as.character(base_cep_RJ_g$ENDCEP_g)
base_cep_RJ_g$ENDCEP_g=trim(base_cep_RJ_g$ENDCEP_g)


bdgeral=merge(bdgeral, base_cep_RJ_merge, all.x=T, by="CEPRES")

bdgeral=merge(bdgeral, base_cep_RJ_g, all.x=T, by="ENDCEP_g")

rm(base_cep_RJ_merge, base_cep_RJ_g, dicionario_numbers, dicionario)

########################################################
bdgeral$IGUAL_BAIRRO_CEP_base = bdgeral$BAIRRO_base == bdgeral$ENDBAIRRO_g

bdgeral$IGUAL_BAIRRO_CEP_base <-
  ifelse(is.na(bdgeral$IGUAL_BAIRRO_CEP_base),
         "FALSE",
         bdgeral$IGUAL_BAIRRO_CEP_base)

bdgeral$IGUAL_RUA_CEP_base = ifelse(
  bdgeral$ENDRUA_base == bdgeral$ENDRUA_g,
  TRUE,
  ifelse(bdgeral$ENDRUA_base_num ==
           bdgeral$ENDRUA_gnum, TRUE,
         "FALSE")
)
bdgeral$IGUAL_RUA_CEP_base = ifelse(is.na(bdgeral$IGUAL_RUA_CEP_base),
                                    'FALSE',
                                    bdgeral$IGUAL_RUA_CEP_base)

bdgeral$IGUAL_BAIRRO_CEP_base_cepg = ifelse(bdgeral$BAIRRO_base_g == bdgeral$BAIRRO_F, 
                                            "TRUE",  "FALSE")

bdgeral$IGUAL_BAIRRO_CEP_base_cepg = ifelse(
  is.na(bdgeral$IGUAL_BAIRRO_CEP_base_cepg),
  'FALSE',
  bdgeral$IGUAL_BAIRRO_CEP_base_cepg)


bdgeral$IGUAL_RUA_CEP_base_cepg = ifelse(
  bdgeral$ENDRUA_base_g == bdgeral$ENDRUA,
  "TRUE",
  ifelse(bdgeral$ENDRUA_base_g == bdgeral$ENDRUA_num,
    "TRUE",
    ifelse(bdgeral$ENDRUA_base_Wnum_g ==
             bdgeral$ENDRUA, "TRUE", "FALSE")))


bdgeral$IGUAL_RUA_CEP_base_cepg = ifelse(is.na(bdgeral$IGUAL_RUA_CEP_base_cepg),
                                         'FALSE',
                                         bdgeral$IGUAL_RUA_CEP_base_cepg)

bdgeral$IGUAL_RUA_TODOS = ifelse(
  bdgeral$IGUAL_RUA_CEP_base == TRUE |
    bdgeral$IGUAL_RUA_CEP_base_g == TRUE |
    bdgeral$LV_RUA == 0 |
    bdgeral$LV_RUA_num == 0 |
    bdgeral$IGUAL_RUA_CEP_base_cepg == TRUE,
  "TRUE",
  "FALSE")

bdgeral$IGUAL_BAIRRO_TODOS = ifelse(
  bdgeral$IGUAL_BAIRRO_CEP_base == TRUE |
    bdgeral$IGUAL_BAIRRO_CEP_base_g == TRUE |
    bdgeral$LV_BAIRRO == 0 |
    bdgeral$IGUAL_BAIRRO_CEP_base_cepg == TRUE,
  "TRUE",
  "FALSE")


bdgeral$IGUAL_RUA_TODOS = ifelse(is.na(bdgeral$IGUAL_RUA_TODOS),
                                 "FALSE",
                                 bdgeral$IGUAL_RUA_TODOS)

bdgeral$IGUAL_BAIRRO_TODOS = ifelse(is.na(bdgeral$IGUAL_BAIRRO_TODOS),
                                    "FALSE",
                                    bdgeral$IGUAL_BAIRRO_TODOS)



bdgeral$DL_10 = ifelse(bdgeral$LV_RUA == 0 &
    bdgeral$LV_BAIRRO == 0 & bdgeral$IGUAL_NUMERO_todos == TRUE,
  "TRUE",
  ifelse(bdgeral$LV_RUA == 0 &
      bdgeral$IGUAL_BAIRRO_CEP_base == TRUE &
      bdgeral$IGUAL_NUMERO_todos == TRUE,
    "TRUE",
    ifelse(bdgeral$LV_RUA == 0 &
        bdgeral$IGUAL_BAIRRO_CEP_base_cepg == TRUE &
        bdgeral$IGUAL_NUMERO_todos == TRUE,
      "TRUE",
      ifelse(bdgeral$IGUAL_RUA_CEP_base == TRUE &
          bdgeral$LV_BAIRRO == 0 & bdgeral$IGUAL_NUMERO_todos == TRUE,
        "TRUE",
        ifelse(bdgeral$IGUAL_RUA_CEP_base == TRUE &
            bdgeral$IGUAL_BAIRRO_CEP_base == TRUE &
            bdgeral$IGUAL_NUMERO_todos == TRUE,
          "TRUE",
          ifelse(bdgeral$IGUAL_RUA_CEP_base == TRUE &
              bdgeral$IGUAL_BAIRRO_CEP_base_cepg == TRUE &
              bdgeral$IGUAL_NUMERO_todos == TRUE,
            "TRUE",
            
            ifelse(bdgeral$IGUAL_RUA_CEP_base_cepg == TRUE &
                bdgeral$LV_BAIRRO == 0 & bdgeral$IGUAL_NUMERO_todos == TRUE,
              "TRUE",
              ifelse(bdgeral$IGUAL_RUA_CEP_base_cepg == TRUE &
                  bdgeral$IGUAL_BAIRRO_CEP_base == TRUE &
                  bdgeral$IGUAL_NUMERO_todos == TRUE,
                "TRUE",
                ifelse(bdgeral$IGUAL_RUA_CEP_base_cepg == TRUE &
                    bdgeral$IGUAL_BAIRRO_CEP_base_cepg == TRUE &
                    bdgeral$IGUAL_NUMERO_todos == TRUE,
                  "TRUE",
                  "FALSE" )))))))))


bdgeral$DL_base = ifelse(is.na(bdgeral$DL_base), 
                      'FALSE', bdgeral$DL_base)

IGUAL_RUA_CEP_fuzzy=ifelse(bdgeral$ENDRUA_num_fuzzy==bdgeral$ENDRUA_gnum, 
                           "TRUE",
                           ifelse(bdgeral$ENDRUA_num_fuzzy==ENDRUA_base_num_g,
                                  "TRUE", 
                                  "FALSE"))

IGUAL_BAIRRO_CEP_fuzzy=ifelse(bdgeral$BAIRRO_fuzzy==ENDBAIRRO_g, "TRUE",
                              ifelse(bdgeral$BAIRRO_fuzzy==BAIRRO_base_g,
                                     "TRUE",
                                     "FALSE"))
                                
                              

bdgeral$DL_fuzzy= ifelse(IGUAL_RUA_CEP_fuzzy=="TRUE" & 
                           IGUAL_BAIRRO_CEP_fuzzy=="TRUE" &
                           IGUAL_NUMERO_todos=="TRUE", "FALSE")
                           
                          
bdgeral$DL_fuzzy = ifelse(is.na(bdgeral$DL_fuzzy), 
                       'FALSE', bdgeral$DL_fuzzy)


bdgeral$GEOCODED=ifelse(bdgeral$DL_0==TRUE | 
                          bdgeral$DL_base==TRUE | 
                          bdgeral$DL_fuzzy==TRUE, "TRUE", "FALSE")

write.csv(bdgeral, file="banco_geoc_final.csv")

#Limpa a area de trabalho
rm(list= ls()[!(ls() %in% c('bdgeral'))])

##########################################################################
