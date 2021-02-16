################################################################################
#############Script 3: "Dicionário" automático de logradouros ##################
################################################################################


#Criação do dicionário/corretor de logradouro, 
#utilizando regras de erros de grafia aplicadas à base de enderecos dos correios
#Aplicação do dicionário corretor nos endereços do banco original 
#Criação do endereço completo final

#############################################################################
#Primeiro: definir area de trabalho: 
#local onde estao os arquivos com os enderecos e demais bancos

#por exemplo: setwd("/media/BANCOS)

################################################################################
#Instalar e carregar os pacotes do R
if (!require(gdata))  install.packages('gdata'); library(gdata)
if(!require(dplyr)) install.packages('dplyr'); library(dplyr)
if(!require(stringr)) install.packages('stringr'); library(stringr)


#Carregar funcoes
source("geofunctions.R")

#############################################################
#Importar o banco de dados dos correios ja editado
base_cep_RJ=read.csv(file = "base_cep_RJ_OFICIAL_1809_MunRJ.csv", 
                     stringsAsFactors = F)

base_cep_RJ=subset(base_cep_RJ, 
                   select =c( "ENDRUA_base_num", "LOGRATIPO_base_num", 
                              "BAIRRO_base"))
base_cep_RJ=unique(base_cep_RJ)
names(base_cep_RJ)[2]="to"

#Importar lista de tipos de logradouro no local do estudo
tipo=read.csv(file="tipos_rj.csv")

#Aplicar a funcao dic_erros nos nomes oficiais de logradouros do DNE
#para construir o dicionario corretor:

erros=dic_erros(base_cep_RJ$to)

# Depois, unir o dicionario com a base de dados dos correios para verificar
# e eliminar os nomes repetidos que nao podem ser utilizados na correcao
# por exemplo, uma das regras de identificao de erros exclui as 
#  preposicoes (de, da, do) dos nomes oficiais dos logradouros. 
# Essa regra aplicada a "Rua da uniao" (p.ex, no bairro Mangueira) gera "Rua uniao".
# No entanto, essa substituicao nao pode ser considerada, pois ha outras ruas 
# oficias sem a preposicao "da" como "Rua uniao" (p.ex em Acari)

#Como o bairro do endereco oficial pode estar errado, que dificulta 
# a substituicao por logradouro e bairro, entao a solucao utilizado foi excluir
# as substituicoes (erros criados) e correcoes que aparecem 
# em outros nomes oficiais de logradouros

erros=right_join(erros, base_cep_RJ, by="to")

erros=unique(erros)
erros=subset(erros, !is.na(erros$from))

erros$LOGRATIPO=paste0("*",erros$ENDRUA_base_num)

patterns=paste0("[*]",tipo$TIPOS, "[[:space:]]")
patterns=paste0(patterns, collapse = "|")

erros$TIPO=vapply(strsplit(erros$LOGRATIPO, "[[:space:]]"), "[", "", 1)
erros$TIPO=sub("[*]", "", erros$TIPO)

erros$LOGRATIPO_erro=paste(erros$TIPO, erros$from, sep=" ")
erros$LOGRATIPO_erro=trim(gsub("[[:space:]]+", " ",erros$LOGRATIPO_erro))
  
erros$flag= erros$LOGRATIPO_erro%in%erros$ENDRUA_base_num

erros=subset(erros, erros$flag=="FALSE")
erros=subset(erros, select = c(LOGRATIPO_erro, ENDRUA_base_num))
names(erros)[1]="from"
names(erros)[2]="to" 

#Salvar o dicionario corretor dos nomes de logradouro
write.csv(erros, file="dicionario_log.csv")

###################################################################
#Importar banco formatado 
bdgeral= read.csv("BANCO_formatado.csv", stringsAsFactors = F)

#Importar e formatar dicionario corretor
dicionario_rules=read.csv(file="dicionario_log.csv", stringsAsFactors = F)
dicionario_rules$from = gsub("[[:space:]]+", ' ', dicionario_rules$from)
dicionario_rules$from =trim(dicionario_rules$from)
dicionario_rules$to = trim(dicionario_rules$to)
dicionario_rules$from = paste0('^',dicionario_rules$from,'$')
dicionario_rules$to = paste(" ",dicionario_rules$to, " ")
dicionario_rules$to=str_to_upper(dicionario_rules$to)

#############################################
bdgeral$LOGRATIPO_rules= trim(bdgeral$ENDRUA_num)

###Substituir termos de acordo com o dicionario (DEMORADO!)
for (i in seq_len(nrow(dicionario_rules))) {
  print(i)
  bdgeral$LOGRATIPO_rules = str_replace_all(
    bdgeral$LOGRATIPO_rules,
    regex(dicionario_rules$from[i],
          multiline = TRUE),
    dicionario_rules$to[i])
}

#Criar variavel com endereços completos e incompletos - usando complemento
bdgeral$ENDCOMPLETO_F = ifelse(
  bdgeral$ENDTIPO_FULL == "SEM NUMERO" &
    bdgeral$ENDTIPO_COMPLEMENTO == "LOTE E QUADRA",
  paste(
    bdgeral$ENDRUA_rules,
    ",",
    bdgeral$ENDLOTE,
    bdgeral$ENDQUADRA,
    "-",
    bdgeral$BAIRRO_F,
    ",",
    "RIO DE JANEIRO",
    sep = " "
  ),
  ifelse(
    bdgeral$ENDTIPO_FULL == "SEM NUMERO" &
      bdgeral$ENDTIPO_COMPLEMENTO == "CASA",
    paste(
      bdgeral$ENDRUA_rules,
      ",",
      bdgeral$ENDCASA,
      "-",
      bdgeral$BAIRRO_F,
      ",",
      "RIO DE JANEIRO",
      sep = " "
    ),
    ifelse(
      bdgeral$ENDTIPO_FULL == "SEM NUMERO" &
        bdgeral$ENDTIPO_COMPLEMENTO == "LOTE",# todos os registros foram verificados 
      paste(
        bdgeral$ENDRUA_rules,
        ",",
        bdgeral$ENDLOTE,
        "-",
        bdgeral$BAIRRO_F,
        ",",
        "RIO DE JANEIRO",
        sep = " "
      ),
      paste(
        bdgeral$ENDRUA_rules,
        ",",
        bdgeral$ENDNUMERO,
        "-",
        bdgeral$BAIRRO_F,
        ",",
        "RIO DE JANEIRO",
        sep = " "))))


bdgeral$ENDCOMPLETO_F = ifelse(
  bdgeral$ENDTIPO_FULL == "SEM RUA" |
    bdgeral$ENDTIPO_FULL == "INCOMPLETO", NA,
  bdgeral$ENDCOMPLETO_F)

bdgeral$ENDCOMPLETO_F = ifelse(
  bdgeral$ENDTIPO_FULL == "SEM NUMERO" &
  bdgeral$ENDTIPO_COMPLEMENTO == "INCOMPLETO", NA,
  bdgeral$ENDCOMPLETO_F)


bdgeral$ENDCOMPLETO_CEP = ifelse(
  bdgeral$ENDTIPO_FULL == "SEM NUMERO" &
    bdgeral$ENDTIPO_COMPLEMENTO == "LOTE E QUADRA",
  paste(
    bdgeral$ENDRUA_rules,
    ",",
    bdgeral$ENDLOTE,
    bdgeral$ENDQUADRA,
    "-",
    bdgeral$BAIRRO_F,
    ",",
    bdgeral$CEPRES,
    ",",
    "RIO DE JANEIRO",
    sep = " "),
  ifelse(
    bdgeral$ENDTIPO_FULL == "SEM NUMERO" &
      bdgeral$ENDTIPO_COMPLEMENTO == "CASA",
    paste(
      bdgeral$ENDRUA_rules,
      ",",
      bdgeral$ENDCASA,
      "-",
      bdgeral$BAIRRO_F,
      ",",
      bdgeral$CEPRES,
      ",",
      "RIO DE JANEIRO",
      sep = " "
    ),
    ifelse(
      bdgeral$ENDTIPO_FULL == "SEM NUMERO" &
        bdgeral$ENDTIPO_COMPLEMENTO == "LOTE",
      paste(
        bdgeral$ENDRUA_rules,
        ",",
        bdgeral$ENDLOTE,
        "-",
        bdgeral$BAIRRO_F,
        ",",
        bdgeral$CEPRES,
        ",",
        "RIO DE JANEIRO",
        sep = " "
      ),
      paste(
        bdgeral$ENDRUA_rules,
        ",",
        bdgeral$ENDNUMERO,
        "-",
        bdgeral$BAIRRO_F,
        ",",
        bdgeral$CEPRES,
        ",",
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

write.csv(bdgeral, file="BANCO_formatado_final.csv")


