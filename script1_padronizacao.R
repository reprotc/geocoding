################################################################################
###############Script 1: Padronização básica dos endereços######################
################################################################################

#Recuperação das informações de bairro que estao como NA-missing no campo 
#do bairro, mas que aparecem nos campos complemento ou nome do logradouro

#Padronização das informações referentes ao bairro por meio do dicionário 
# de bairros 

#Padronização do logradouro, correção de erros (mais frequentes) 
#nome do logradouro, conversão de números por extenso e abreviações, 

#Criação do endereço completo (sem DNE)

# Todos os dicionários utilizados sao referentes ao Rio de Janeiro, 
#mas podem ser adaptados para outros locais e base de dados 

# Os dicionários contem expressoes regulares simples: 
#https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf

#############################################################################
#Primeiro: defina a area de trabalho: 
#local onde estao os arquivos com os enderecos e demais bancos
#por exemplo: setwd("/media/BANCOS)

################################################################################
#Instalar e//ou carregar os pacotes do R
if (!require(foreign)) install.packages('foreign'); library(foreign)
if (!require(stringr)) install.packages('stringr'); library(stringr)
if (!require(gdata))  install.packages('gdata'); library(gdata)
if(!require(dplyr)) install.packages('dplyr'); library(dplyr)

#Carregar funcoes
source("geofunctions.R")

################################################################################
#Para os bancos com formatos diferentes dos bancos do SIM, 
#indicar os nomes das colunas 
#do banco de dados referentes ao campo do endereço (logradouro, bairro etc)

#Campo nome da rua 
campoend = "ENDRES"
#Campo nome do bairro 
campobair = "BAIRES"
#Campo numero do endereco
camponum = "NUMRES"
#Campo complemento do endereco
campocompl = "COMPLRES"
#Campo cep
campocep = "CEPRES"
# Codigo OU nome da cidade 
codigo_cidad= "RIO DE JANEIRO"

#############################################################################
#Importar e editar a lista de bairros da cidade  
bairros_list = read.csv("bairros.csv") #lista ate 2018
bairros_list$Bairros = iconv(bairros_list$Bairros, from = "latin1", to = "UTF-8")
bairros_list$Bairros = str_to_upper(bairros_list$Bairros)
bairros_list$Bairros = gsub("[[:punct:]]+", " ", bairros_list$Bairros)
bairros_list$Bairros = gsub("[[:space:]]+", ' ', bairros_list$Bairros)
bairros_lista = unlist(trim(bairros_list$Bairros))

## Importar dicionario de bairros da cidade formato: 
#'from' bairro grafia errada;  'to' bairro grafia correta )
dicionario_bairro = read.csv("dicionarios_trad4.csv")
dicionario_bairro$from = gsub("[[:space:]]+", ' ', dicionario_bairro$from)
dicionario_bairro$to = gsub("[[:space:]]+", ' ', dicionario_bairro$to) 
dicionario_bairro$from = trim(str_to_upper(dicionario_bairro$from))
dicionario_bairro$to = trim(str_to_upper(dicionario_bairro$to))

## Importar dicionario de logradouro  (formato: 'from' rua grafia inconsistente ; 
#'to' rua grafia correta )
dicionario = read.csv("dicionario_titulos.csv")
dicionario$from = gsub("[[:space:]]+", ' ', dicionario$from)
dicionario$from =trim(dicionario$from)
dicionario$to = trim(dicionario$to)
dicionario$from = paste0('\\b',dicionario$from,'\\b') # utilizar 'word boundary'
dicionario$to = str_to_upper(paste(" ",dicionario$to, " "))

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

#Carregar os bancos com os enderecos, no formato csv
bdgeral=read.csv('data_2012_2017.csv',na.string="", stringsAsFactors=FALSE)


################################################################################
# Recuperação do bairro (1) - Erro muito especifico do banco SIM rj 
#Objetivo: Recuperar os bairros que estao no campo logradouro separados por *
# Alguns registros estão com missing no campo BAIRES 
#e com informação de bairro no campo ENDRES no formato: "Rua_______*bairro"

bdgeral$ENDRUA = trim(as.character(bdgeral[, campoend]))
bdgeral$bairro3 = vapply(strsplit(bdgeral$ENDRUA, "[*]"), "[", "", 2)

## Unir o campo bairro de BAIRES e aqueles recuperados no campo ENRES 
bdgeral$BAIRRO = trim(as.character(bdgeral[, campobair])
bdgeral$BAIRRO[bdgeral$BAIRRO == ''] <- NA

bdgeral$BAIRRO = ifelse(is.na(bdgeral$BAIRRO),
                        as.character(bdgeral$bairros3),
                        bdgeral$BAIRRO)
###########
## Padronizar basica do campo bairro 
# Incluindo, caixa alta, retirar espaços duplos, digitos e pontuação  
bdgeral$BAIRRO = gsub('[[:digit:]]+', " ", bdgeral$BAIRRO) 
bdgeral$BAIRRO = gsub("[[:punct:]]+", " ", bdgeral$BAIRRO)
bdgeral$BAIRRO = trim(str_to_upper(bdgeral$BAIRRO)) 
bdgeral$BAIRRO = gsub("[[:space:]]+", ' ', bdgeral$BAIRRO)

##Edição campo BAIRRO
#Substitui os bairros de acordo com o dicionario de bairro
bairro_end = trim(as.character(bdgeral$BAIRRO))

for (i in seq_len(nrow(dicionario_bairro))) {
  bairro_end =  gsub(dicionario_bairro$from[i], dicionario_bairro$to[i],
                     bairro_end)
}

bdgeral$BAIRRO = trim(bairro_end)
bdgeral$BAIRRO = gsub("BAIRRO", "", bdgeral$BAIRRO)
bdgeral$BAIRRO = gsub("[[:space:]]+", ' ', bdgeral$BAIRRO)


##verificar se os bairros recuperados e padronizados estao na 
#lista de bairros do Rio de Janeiro 
#(considerando os bairros oficiais entre 2012 a 2018)
status_bairro = NULL
for (i in 1:length(bdgeral$BAIRRO))
{
  bdgeral$status_bairro[i] = bdgeral$BAIRRO[i] %in% bairros_lista
}

#Recuperacao bairro (2) -
#Alguns bairros estao como NA no campo BAIRES, mas aparecem no campo COMPLRES
#Objetivo: tentar recuperar os bairros que estao no complemento do endereço
# Extrair o bairro, excluindo os demais campos referentes ao bloco, casa, lote, 
#quadra no campo complemento.  
# Outra opção seria extrair as strings que estao na lista de bairros.
#mas pode gerar erro, dado que nomes de ruas podem ser iguais 
# aos nomes de bairros em locais distintos

# A padronização do complemento inclui retirar espaços duplos, mudar o formato 
#para UTF-8 (por conta de alguns acentos nao reconhecidos)
# eliminar pontuação reconhecida
###########################################################

complemento2 = ifelse(is.na(bdgeral$BAIRRO), 
                      trim(as.character(bdgeral[, campocompl])), "")
complemento2= iconv(complemento2, from = "latin1", to = "UTF-8")
complemento2 = str_to_upper(trim(complemento2))
complemento2 = gsub("[[:punct:]]+", " ", complemento2) 
complemento2 = gsub("[[:space:]]+",' ', complemento2) 

#Padronizar os campos nao relacionados ao bairro para facilitar a exclusao 
# Motivo: a extracao direta dos nomes de bairros no 
#complemento pode geral problemas, se o nome de um bairro tambem aparece 
#em nomes de rua em bairros distintos: p.ex, Travessa Humaita, Colegio

for (i in seq_len(nrow(dicionario_complemento))) {
  complemento2 =  gsub(dicionario_complemento$from[i],
                       dicionario_complemento$to[i],
                       complemento2)}

bdgeral$ENDCOMPLE2 = complemento2
bdgeral$ENDCOMPLE2 = gsub("[[:space:]]+",' ', bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = trim(gsub("BAIRRO","", bdgeral$ENDCOMPLE2))
bdgeral$ENDCOMPLE2 = trim(bdgeral$ENDCOMPLE2)

#Padronizar o bairro do campo complemento de acordo com o dicionario de bairros
complemento2=bdgeral$ENDCOMPLE2

for (i in seq_len(nrow(dicionario_bairro))) {
  complemento2 =  gsub(dicionario_bairro$from[i], dicionario_bairro$to[i],
                       complemento2)
}

bdgeral$ENDCOMPLE2 = complemento2
bdgeral$ENDCOMPLE2 = gsub("[[:space:]]+", ' ', bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = trim(gsub("BAIRRO", "", bdgeral$ENDCOMPLE2))


##############################################################
#Excluir campos nao relacionados ao bairro
#(outra opção mais simples: bdgeral$ENDCOMPLE2 = sub("[[:alnum:]]+[[:digit:]]+", "", bdgeral$ENDCOMPLE2)
                      
bdgeral$ENDCOMPLE2 = sub("APT [[:alnum:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("AP [[:alnum:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("APT[[:digit:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("AP[[:digit:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("ET [[:digit:]]+", "", bdgeral$ENDCOMPLE2)

bdgeral$ENDCOMPLE2 = sub("QUADRA [[:alnum:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("QDA [[:alnum:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("QUADRA[[:digit:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("Q[[:digit:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("Q [[:digit:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("VQD [[:digit:]]+", "", bdgeral$ENDCOMPLE2)

bdgeral$ENDCOMPLE2 = sub("B[[:digit:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("B [[:digit:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("BLOCO [[:alnum:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("BLOCO[[:digit:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("BL [[:alnum:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("BL[[:digit:]]+", "", bdgeral$ENDCOMPLE2)

bdgeral$ENDCOMPLE2 = sub("LOTE [[:alnum:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("LOTE[[:alnum:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("LT [[:digit:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("LT[[:digit:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("CL [[:digit:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("VL [[:digit:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("VL[[:digit:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("L[[:digit:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("L [[:digit:]]+", "", bdgeral$ENDCOMPLE2)

bdgeral$ENDCOMPLE2 = sub("CASA [[:digit:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("C[[:digit:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("CA [[:digit:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("CA[[:digit:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("CL [[:digit:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("C [[:digit:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("CS[[:digit:]]+", "", bdgeral$ENDCOMPLE2)

bdgeral$ENDCOMPLE2 = sub("GR [[:alnum:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("GR[[:digit:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("LOJA [[:alnum:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("ENT [[:alnum:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("RUA [[:alnum:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("RUA [[:alnum:]]+ [[:alnum:]]+", "", bdgeral$ENDCOMPLE2)

bdgeral$ENDCOMPLE2 = sub("TERREO ", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("ANDAR ", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("FUNDOS", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("FRENTE", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("KM", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("SOBRADO", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("BAIRRO", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("APT ", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("APTº ", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("CASA", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("BLOCO", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("QUADRA", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("TERREO", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("COBERTURA", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("SN ", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = sub("FD ", " ", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = gsub("[[:digit:]]+", "", bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = gsub("[[:space:]]+", ' ', bdgeral$ENDCOMPLE2)
bdgeral$ENDCOMPLE2 = trim(bdgeral$ENDCOMPLE2)

#verificar se os bairros resgatados do CAMPO COMPLEMENTO estao na lista de 
#bairros do Rio de Janeiro

for (i in 1:length(bdgeral$ENDCOMPLE2)) 
{
  bdgeral$status_bairro_compl[i]= bdgeral$ENDCOMPLE2[i] %in% bairros_lista
}

#Criar a variavel final do bairro, juntando todas as recuperacoes e edicoes
bdgeral$BAIRRO_F= ifelse(bdgeral$status_bairro==FALSE & 
                           bdgeral$status_bairro_compl==TRUE, 
                                    bdgeral$ENDCOMPLE2, bdgeral$BAIRRO)


#####################Padronização nome da rua##################################
endereco = trim(as.character(bdgeral$ENDRUA))
endereco=stringi::stri_trans_general(endereco, "Latin-ASCII") #remover acentos
endereco = str_to_upper(endereco)
endereco = gsub("[[:punct:]]+", " ", endereco) #remover pontuação 
endereco = gsub("[[:space:]]+", " ", endereco) #remover espaços duplos  

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
    "AVENID" = "AVENIDA",
    "AEVNIDA" = "AVENIDA", 
    "AVENIA" = "AVENIDA",
    "AENIDA" = "AVENIDA", 
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
    "ESTRDA" = "ESTRADA",
    "STRADA" = "ESTRADA",
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

bdgeral$ENDRUA=endnovo

#Substitui outros termos de acordo com o dicionario 
endres=as.character(bdgeral[,"ENDRUA"])

for (i in seq_len(nrow(dicionario))) {
  endres =  gsub(dicionario$from[i], dicionario$to[i],
                 endres)
}
bdgeral$ENDRUA=endres
bdgeral$ENDRUA=trim(gsub('[[:space:]]+', ' ',bdgeral$ENDRUA)) 
bdgeral$ENDRUA=gsub("^RUA NA$", NA, bdgeral$ENDRUA)
bdgeral$ENDRUA=gsub("^RU A ", "RUA", bdgeral$ENDRUA)

#Conversao numeros por extenso
endres=as.character(bdgeral[,"ENDRUA"])

for (i in seq_len(nrow(dicionario_numbers))) {
  endres =  gsub(dicionario_numbers$from[i], dicionario_numbers$to[i],
                 endres)
}
bdgeral$ENDRUA_num=endres
bdgeral$ENDRUA_num=trim(gsub('[[:space:]]+', ' ',bdgeral$ENDRUA_num)) 


#Edicao do numero de endereço
# Extrai apenas o numero e exclui outros caracteres (letras, simbolos, 
#quadra/lote) no campo numero
numero = gsub("SN", "", trim(as.character(bdgeral[, camponum])))
numero = gsub("CASA", "", numero)
numero = gsub("^C ", "", numero)
numero = gsub("^CA$", "", numero)
numero = gsub("C A ", "", numero)
numero = gsub("CS", "", numero)
numero = gsub("VL", "", numero)
numero = gsub("VILA", "", numero)
numero = str_extract(numero, "[[:digit:]]+")
numero = gsub('^0+', '0', numero)
numero = gsub("^0$", "", numero)
numero=as.numeric(numero)
bdgeral$ENDNUMERO = numero

#Edicao do campo complemento do endereco para os enderecos incompletos
#a) edita o campo complemento e extrai o digito apos "LOTE ou QUADRA ou 
#CASA" [:alnum:],

#Edita e extrai o numero do lote
complemento = ifelse(!is.na(bdgeral[, campoend]), 
                     trim(as.character(bdgeral[, campocompl])), "")
complemento= iconv(complemento, from = "latin1", to = "UTF-8")
complemento = str_to_upper(trim(complemento))
complemento = gsub("[[:punct:]]+", " ", complemento) 
complemento = trim(gsub("[[:space:]]+",' ', complemento)) 

#Padronizar os campos nao relacionados ao bairro, para facilitar a exclusao 

for (i in seq_len(nrow(dicionario_complemento))) {
  complemento =  gsub(dicionario_complemento$from[i],
                      dicionario_complemento$to[i],
                      complemento)
}

#extrair lote, quadra e casa 
lote = str_extract(complemento, "LOTE [[:alnum:]]+")
bdgeral$ENDLOTE_t = lote
bdgeral$ENDLOTE = str_extract(bdgeral$ENDLOTE_t, "[[:digit:]]+")

quadra = str_extract(complemento, "QUADRA [[:alnum:]]+")
bdgeral$ENDQUADRA = quadra

casa = str_extract(complemento, "CASA [[:alnum:]]+")
bdgeral$ENDCASA_t = casa
bdgeral$ENDCASA = str_extract(bdgeral$ENDCASA_t, "[[:digit:]]+")

# formatar NAs
bdgeral$BAIRRO[bdgeral$BAIRRO == ''] <- NA
bdgeral$BAIRRO_F[bdgeral$BAIRRO_F == ''] <- NA
bdgeral$ENDRUA[bdgeral$ENDRUA == ''] <- NA
bdgeral$ENDRUA_num[bdgeral$ENDRUA_num == ''] <- NA
bdgeral$ENDNUMERO[bdgeral$ENDNUMERO == ''] <- NA
bdgeral$ENDCEP=bdgeral[, campocep]
bdgeral$ENDCEP[bdgeral$ENDCEP == ''] <- NA

#Cria variavel indicando o tipo de endereço 
#Completo: com informacao rua, numero e bairro 
#Incompleto: sem informacao rua ou numero ou bairro

bdgeral$ENDTIPO_FULL = ifelse(!is.na(bdgeral[, "ENDRUA"]) &
                       !is.na(bdgeral[, "ENDNUMERO"]) &
                       !is.na(bdgeral[, "BAIRRO_F"]),
                              "COMPLETO", 
                       ifelse(is.na(bdgeral[, "ENDRUA"]) &
                       !is.na(bdgeral[, "ENDNUMERO"]) &
                       !is.na(bdgeral[, "BAIRRO_F"]),
                               "SEM RUA", 
                       ifelse(!is.na(bdgeral[, "ENDRUA"]) &
                       is.na(bdgeral[, "ENDNUMERO"]) &
                       !is.na(bdgeral[, "BAIRRO_F"]),
                              "SEM NUMERO", 
                       ifelse(!is.na(bdgeral[, "ENDRUA"]) &
                       !is.na(bdgeral[, "ENDNUMERO"]) &
                       is.na(bdgeral[, "BAIRRO_F"]),
                          "SEM BAIRRO", "INCOMPLETO"))))

bdgeral$ENDTIPO_COMPLEMENTO = ifelse(bdgeral$ENDTIPO_FULL=="SEM NUMERO" & 
                                       !is.na(bdgeral[, "ENDLOTE"]) &
                                       !is.na(bdgeral[, "ENDQUADRA"]) &
                                       !is.na(bdgeral[, "ENDCASA"]), "LOTE E QUADRA",
                                     ifelse(bdgeral$ENDTIPO_FULL=="SEM NUMERO" & 
                                              !is.na(bdgeral[, "ENDLOTE"]) &
                                              !is.na(bdgeral[, "ENDQUADRA"]) &
                                              is.na(bdgeral[, "ENDCASA"]), "LOTE E QUADRA", 
                                            ifelse(bdgeral$ENDTIPO_FULL=="SEM NUMERO" & 
                                                     is.na(bdgeral[, "ENDLOTE"]) &
                                                     !is.na(bdgeral[, "ENDQUADRA"]) &
                                                     !is.na(bdgeral[, "ENDCASA"]), "CASA",
                                                   ifelse(bdgeral$ENDTIPO_FULL=="SEM NUMERO" & 
                                                            is.na(bdgeral[, "ENDLOTE"]) &
                                                            is.na(bdgeral[, "ENDQUADRA"]) &
                                                            !is.na(bdgeral[, "ENDCASA"]), "CASA",
                                                          ifelse(bdgeral$ENDTIPO_FULL=="SEM NUMERO" & 
                                                                   !is.na(bdgeral[, "ENDLOTE"]) &
                                                                   is.na(bdgeral[, "ENDQUADRA"]) &
                                                                   !is.na(bdgeral[, "ENDCASA"]), "LOTE", 
                                                                 ifelse(bdgeral$ENDTIPO_FULL=="SEM NUMERO" & 
                                                                          !is.na(bdgeral[, "ENDLOTE"]) &
                                                                          is.na(bdgeral[, "ENDQUADRA"]) &
                                                                          is.na(bdgeral[, "ENDCASA"]), "LOTE", "INCOMPLETO"))))))



#Criar variavel com endereços completos e incompletos - usando complemento
#Rodar apenas se nao for utilizar os dados dos correios 
#no script 3 e feita a criacao do endcompleto apos a padronizacao 
#utilizando do dicionario do DNE_rules
bdgeral$ENDCOMPLETO_F = ifelse(bdgeral$ENDTIPO_FULL=="SEM NUMERO" & 
                                 bdgeral$ENDTIPO_COMPLEMENTO=="LOTE E QUADRA", 
                               paste(
                                 bdgeral$ENDRUA,
                                 ",",
                                 bdgeral$ENDLOTE, 
                                 bdgeral$ENDQUADRA, "-",
                                 bdgeral$BAIRRO_F, ",",
                                 "RIO DE JANEIRO",
                                 sep = " "),
                               ifelse(bdgeral$ENDTIPO_FULL=="SEM NUMERO" 
                                      & bdgeral$ENDTIPO_COMPLEMENTO=="CASA", 
                                      paste(
                                        bdgeral$ENDRUA,
                                        ",",
                                        bdgeral$ENDCASA, "-",
                                        bdgeral$BAIRRO_F,
                                        ",",
                                        "RIO DE JANEIRO",
                                        sep = " "),
                                      ifelse(bdgeral$ENDTIPO_FULL=="SEM NUMERO" 
                                             & bdgeral$ENDTIPO_COMPLEMENTO=="LOTE", 
                                             paste(
                                               bdgeral$ENDRUA,
                                               ",",
                                               bdgeral$ENDLOTE, "-",
                                               bdgeral$BAIRRO_F,
                                               ",",
                                               "RIO DE JANEIRO",
                                               sep = " "), 
                                             paste(
                                               bdgeral$ENDRUA,
                                               ",",
                                               bdgeral$ENDNUMERO, "-",
                                               bdgeral$BAIRRO_F,
                                               ",",
                                               "RIO DE JANEIRO",
                                               sep = " "))))



bdgeral$ENDCOMPLETO_F=ifelse(bdgeral$ENDTIPO_FULL=="SEM RUA" | 
                               bdgeral$ENDTIPO_FULL=="INCOMPLETO", NA,
                             bdgeral$ENDCOMPLETO_F)

bdgeral$ENDCOMPLETO_F=ifelse(bdgeral$ENDTIPO_FULL=="SEM NUMERO" & 
                               bdgeral$ENDTIPO_COMPLEMENTO=="INCOMPLETO", NA,
                             bdgeral$ENDCOMPLETO_F)


bdgeral$ENDCOMPLETO_CEP = ifelse(bdgeral$ENDTIPO_FULL=="SEM NUMERO" & 
                                   bdgeral$ENDTIPO_COMPLEMENTO=="LOTE E QUADRA", 
                                 paste(
                                   bdgeral$ENDRUA,
                                   ",",
                                   bdgeral$ENDLOTE, 
                                   bdgeral$ENDQUADRA, "-",
                                   bdgeral$BAIRRO_F, ",",
                                   bdgeral$CEPRES, ",",
                                   "RIO DE JANEIRO",
                                   sep = " "),
                                 ifelse(bdgeral$ENDTIPO_FULL=="SEM NUMERO" & 
                                          bdgeral$ENDTIPO_COMPLEMENTO=="CASA", 
                                        paste(
                                          bdgeral$ENDRUA,
                                          ",",
                                          bdgeral$ENDCASA, "-",
                                          bdgeral$BAIRRO_F, ",",
                                          bdgeral$CEPRES, ",",
                                          "RIO DE JANEIRO",
                                          sep = " "),
                                        ifelse(bdgeral$ENDTIPO_FULL=="SEM NUMERO" 
                                               & bdgeral$ENDTIPO_COMPLEMENTO=="LOTE", 
                                               paste(
                                                 bdgeral$ENDRUA,
                                                 ",",
                                                 bdgeral$ENDLOTE, "-",
                                                 bdgeral$BAIRRO_F,
                                                 ",",
                                                 bdgeral$CEPRES, ",",
                                                 "RIO DE JANEIRO",
                                                 sep = " "), 
                                               paste(
                                                 bdgeral$ENDRUA,
                                                 ",",
                                                 bdgeral$ENDNUMERO, "-",
                                                 bdgeral$BAIRRO_F,
                                                 ",",
                                                 bdgeral$CEPRES, ",",
                                                 "RIO DE JANEIRO",
                                                 sep = " "))))  



bdgeral$ENDCOMPLETO_F = gsub(" NA ", " ", bdgeral$ENDCOMPLETO_F)
bdgeral$ENDCOMPLETO_F = gsub("[[:space:]]+", " ", bdgeral$ENDCOMPLETO_F)
bdgeral$ENDCOMPLETO_F = gsub("- ,", ",", bdgeral$ENDCOMPLETO_F)
bdgeral$ENDCOMPLETO_F = gsub(", ,", ",", bdgeral$ENDCOMPLETO_F)
bdgeral$ENDCOMPLETO_F = gsub("-,", ",", bdgeral$ENDCOMPLETO_F)
bdgeral$ENDCOMPLETO_F = gsub(", -", ",", bdgeral$ENDCOMPLETO_F)

bdgeral$ENDCOMPLETO_CEP = gsub(" NA ", " ", bdgeral$ENDCOMPLETO_CEP)
bdgeral$ENDCOMPLETO_CEP = gsub("^NA ,", " ", bdgeral$ENDCOMPLETO_CEP)
bdgeral$ENDCOMPLETO_CEP = gsub("[[:space:]]+", " ", bdgeral$ENDCOMPLETO_CEP)
bdgeral$ENDCOMPLETO_CEP = gsub("- ,", ",", bdgeral$ENDCOMPLETO_CEP)
bdgeral$ENDCOMPLETO_CEP = gsub(", ,", ",", bdgeral$ENDCOMPLETO_CEP)
bdgeral$ENDCOMPLETO_CEP = gsub("-,", ",", bdgeral$ENDCOMPLETO_CEP)
bdgeral$ENDCOMPLETO_CEP = gsub(", -", ",", bdgeral$ENDCOMPLETO_CEP)

write.csv(bdgeral, file="BANCO_formatado.csv")


#Limpar a area de trabalho
rm(list= ls()[!(ls() %in% c('bdgeral', 'bairros_list', 'dicionario',
                            'bairros_lista', 'dicionario_bairro', 
                            'dicionario_complemento', 'dicionario_numbers'))])






