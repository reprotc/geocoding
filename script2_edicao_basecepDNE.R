################################################################################
############Script 2: Edição da base de dados dos correios (DNE)################
################################################################################

#Criação do campo número/intervalo e lado (utilizando o LOG_COMPLEMENTO) 
#e do campo para os nomes alternativos de logradouro e bairro
#Construção das listas de nomes, tipos e pares de tipos de logradouros
#Padronização básica de todos os campos de endereço do DNE


# Antes de rodar o script e' necessario salvar no formato csv 
# os arquivos da pasta 'delimitado' referentes ao local do estudo:
# Em geral, LOG_LOGRADOURO, LOG_GRANDE USUARIO e LOG BAIRRO) 
#(Salvar o arquivo com separacao por '@')

################################################################################
#Abrir ou instalar pacotes
if (!require(foreign))install.packages("foreign"); library(foreign)
if (!require(stringr)) install.packages('stringr'); library(stringr)
if (!require(gdata))install.packages('gdata'); library(gdata)
if (!require(stringdist))install.packages('stringdist'); library(stringdist)
if (!require(tidyr))install.packages('tidyr'); library(tidyr)
if(!require(dplyr)) install.packages('dplyr'); library(dplyr)
if (!require(data.table))install.packages('data.table'); library(data.table)
if(!require(stringi)) install.packages('stringi'); library(stringi)

#################################################################################
##Importar dicionario de numeros 
dicionario_numbers=read.csv("dicionario_numbers.csv", sep=',')
dicionario_numbers$from = trim(gsub('[[:space:]]+', ' ', dicionario_numbers$from))
dicionario_numbers$to = trim(gsub('[[:space:]]+', ' ', dicionario_numbers$to))
dicionario_numbers$from = paste0('\\b',dicionario_numbers$from,'\\b')
dicionario_numbers$to = str_to_upper(paste(' ',dicionario_numbers$to,' '))

## Importar dicionario de bairros do Rio (from bairro grafia errada - 
#to bairro grafia correta )
dicionario_bairro = read.csv("dicionarios_trad4.csv")
dicionario_bairro$from = gsub("[[:space:]]+", ' ', dicionario_bairro$from)
dicionario_bairro$to = gsub("[[:space:]]+", ' ', dicionario_bairro$to) 
dicionario_bairro$from = trim(str_to_upper(dicionario_bairro$from))
dicionario_bairro$to = trim(str_to_upper(dicionario_bairro$to))


################################################################################
# Abrir os arquivos dos correios
base_cep_log= fread("LOG_LOGRADOURO_RJ.csv", sep = "@", 
                     encoding = "Latin-1", stringsAsFactors=FALSE)
base_cep_GRUlog= fread("LOG_GRANDE_USUARIO.csv", sep = "@", 
                       encoding = "Latin-1", stringsAsFactors=FALSE)
base_cep_bairro= fread("LOG_BAIRRO.csv", sep = "@", 
                        encoding = "Latin-1", stringsAsFactors=FALSE)

base_cep_log_alt= fread("LOG_VAR_LOG.csv", sep = "@", encoding = "Latin-1", 
                         stringsAsFactors=FALSE)
base_cep_bairro_alt= fread("LOG_VAR_BAI.csv", sep = "@", encoding = "Latin-1",
                            stringsAsFactors=FALSE)

base_cep_GRUlog=subset(base_cep_GRUlog, base_cep_GRUlog$UFE_SG=="RJ")
names(base_cep_log)

names(base_cep_GRUlog)
names(base_cep_GRUlog)[1]="LOG_NU"
names(base_cep_GRUlog)[2]="UF"
names(base_cep_GRUlog)[4]="BAI_NU_INI"
names(base_cep_GRUlog)[5]="LOG_NO"
names(base_cep_GRUlog)[6]="LOG_COMPLEMENTO"
names(base_cep_GRUlog)[8]="LOG_NO_ABREV"

base_cep_bairro=base_cep_bairro[,-3] # excluir a variavel que nao sera utilizada 
#nome alternativo para campos com nomes identicos nos 3 bancos
names(base_cep_bairro)[2]="UF_B" 

base_cep_log=merge(base_cep_log, base_cep_GRUlog, all = T)


#Edita os campos com os nomes alternativos/variacao popular
base_cep_log_alt=base_cep_log_alt[,-3]
base_cep_log_alt=base_cep_log_alt[,-2]

base_cep_log_alt=base_cep_log_alt%>%
  group_by(LOG_NU) %>%
  mutate(VLO_TXID = paste0('VLO_TX', row_number())) %>%
  spread(VLO_TXID, VLO_TX)

base_cep_bairro_alt=base_cep_bairro_alt%>%
  group_by(BAI_NU) %>%
  mutate(VDB_TXID = paste0('VDB_TX', row_number())) %>%
  spread(VDB_TXID, VDB_TX)

#A junção de todos os nomes alternativos foi feita apenas para facilitar a indentificacao
# dos nomes que aparecem na lista ou coluna com as variacoes. 

base_cep_log_alt$VLO_TX2= paste(base_cep_log_alt$VLO_TX2, 
                                base_cep_log_alt$VLO_TX3, 
                                base_cep_log_alt$VLO_TX4, 
                                base_cep_log_alt$VLO_TX5,
                                base_cep_log_alt$VLO_TX6, 
                                base_cep_log_alt$VLO_TX7,
                                base_cep_log_alt$VLO_TX8, sep = ";")

base_cep_log_alt$VLO_TX2=gsub(";NA","", base_cep_log_alt$VLO_TX2)
base_cep_log_alt=base_cep_log_alt[,1:3]

base_cep_bairro_alt$VDB_TX2=paste(base_cep_bairro_alt$VDB_TX2,
                                  base_cep_bairro_alt$VDB_TX3,
                                  base_cep_bairro_alt$VDB_TX4,
                                  base_cep_bairro_alt$VDB_TX5,
                                  base_cep_bairro_alt$VDB_TX6,
                                  base_cep_bairro_alt$VDB_TX7,
                                  base_cep_bairro_alt$VDB_TX8,
                                  base_cep_bairro_alt$VDB_TX9,
                                  base_cep_bairro_alt$VDB_TX15,
                                  base_cep_bairro_alt$VDB_TX21,
                                  base_cep_bairro_alt$VDB_TX10,
                                  base_cep_bairro_alt$VDB_TX16,
                                  base_cep_bairro_alt$VDB_TX22,
                                  base_cep_bairro_alt$VDB_TX11,
                                  base_cep_bairro_alt$VDB_TX17,
                                  base_cep_bairro_alt$VDB_TX23,
                                  base_cep_bairro_alt$VDB_TX12,
                                  base_cep_bairro_alt$VDB_TX18,
                                  base_cep_bairro_alt$VDB_TX24,
                                  base_cep_bairro_alt$VDB_TX13,
                                  base_cep_bairro_alt$VDB_TX19,
                                  base_cep_bairro_alt$VDB_TX25,
                                  base_cep_bairro_alt$VDB_TX14,
                                  base_cep_bairro_alt$VDB_TX20,
                                  base_cep_bairro_alt$VDB_TX26,
                                  base_cep_bairro_alt$VDB_TX27,
                                  base_cep_bairro_alt$VDB_TX28,
                                  base_cep_bairro_alt$VDB_TX29,
                                  base_cep_bairro_alt$VDB_TX30,
                                  base_cep_bairro_alt$VDB_TX33,
                                  base_cep_bairro_alt$VDB_TX36,
                                  base_cep_bairro_alt$VDB_TX31,
                                  base_cep_bairro_alt$VDB_TX34,
                                  base_cep_bairro_alt$VDB_TX37,
                                  base_cep_bairro_alt$VDB_TX32,
                                  base_cep_bairro_alt$VDB_TX35,
                                  base_cep_bairro_alt$VDB_TX38,
                                  base_cep_bairro_alt$VDB_TX39,
                                  base_cep_bairro_alt$VDB_TX40,
                                  base_cep_bairro_alt$VDB_TX41,
                                  base_cep_bairro_alt$VDB_TX42,
                                  base_cep_bairro_alt$VDB_TX43,
                                  base_cep_bairro_alt$VDB_TX44,
                                  base_cep_bairro_alt$VDB_TX45,
                                  base_cep_bairro_alt$VDB_TX46,
                                  base_cep_bairro_alt$VDB_TX47,
                                  base_cep_bairro_alt$VDB_TX48, sep = ";")

base_cep_bairro_alt$VDB_TX2=gsub(";NA","", base_cep_bairro_alt$VDB_TX2)
base_cep_bairro_alt=base_cep_bairro_alt[,c(1,2,13)]


##write.csv(base_cep_bairro_alt, file = 'base_cep_bairro_alt.csv')
#write.csv(base_cep_log_alt, file = "base_cep_log_alt")

###############################################################
base_cep_bairro=merge(base_cep_bairro, base_cep_bairro_alt, by="BAI_NU", all.x = T)

#junção dos bancos utilizando as chaves indicadas pelos correios
base_cep_log=merge(base_cep_log, base_cep_log_alt, by = "LOG_NU", all.x = T) 
#junção dos bancos utilizando as chaves indicadas pelos correios
base_cep_RJ=merge(base_cep_log, base_cep_bairro, by.x="BAI_NU_INI", by.y = "BAI_NU") 
rm(base_cep_log, base_cep_log_alt, base_cep_bairro, base_cep_GRUlog, base_cep_bairro_alt)

####################################################################
#LOGRADOURO EDICAO: edição basica do campo logradouro 
base_cep_RJ$TIPO_base=as.character(base_cep_RJ$TLO_TX) #ler como texto
#remover acentos
base_cep_RJ$TIPO_base=stringi::stri_trans_general(base_cep_RJ$TLO_TX, "Latin-ASCII") 
# transformar para upper case/caixa alta
base_cep_RJ$TIPO_base = str_to_upper(base_cep_RJ$TLO_TX)

base_cep_RJ$LOGRATIPO_base=trim(as.character(base_cep_RJ$LOG_NO))
base_cep_RJ$LOGRATIPO_base=stringi::stri_trans_general(base_cep_RJ$LOGRATIPO_base,
                                                       "Latin-ASCII") 
base_cep_RJ$LOGRATIPO_base = str_to_upper(base_cep_RJ$LOGRATIPO_base)

base_cep_RJ$ENDRUA_base=trim(paste(base_cep_RJ$TIPO_base, 
                                   base_cep_RJ$LOGRATIPO_base, sep = " "))

base_cep_RJ$ENDRUA_base_abrev=as.character(base_cep_RJ$LOG_NO_ABREV)
base_cep_RJ$ENDRUA_base_abrev=stringi::stri_trans_general(base_cep_RJ$LOG_NO_ABREV, 
                                                          "Latin-ASCII")
base_cep_RJ$ENDRUA_base_abrev = str_to_upper(base_cep_RJ$LOG_NO_ABREV) # upper case

base_cep_RJ$ENDRUA_base_alt=as.character(base_cep_RJ$VLO_TX1)
base_cep_RJ$ENDRUA_base_alt=stringi::stri_trans_general(base_cep_RJ$ENDRUA_base_alt, 
                                                        "Latin-ASCII") 
base_cep_RJ$ENDRUA_base_alt = str_to_upper(base_cep_RJ$ENDRUA_base_alt) 

base_cep_RJ$ENDRUA_base_alt2=as.character(base_cep_RJ$VLO_TX2)
base_cep_RJ$ENDRUA_base_alt2=stringi::stri_trans_general(base_cep_RJ$ENDRUA_base_alt2, 
                                                         "Latin-ASCII") 
base_cep_RJ$ENDRUA_base_alt2 = str_to_upper(base_cep_RJ$ENDRUA_base_alt2) 


#########################################
#TRADUÇÃO NUMEROS

#ENDRUA:converter digitos para numeros por extenso por meio do dicionário 
#de numeros 1 a 100 - depois criar function
base_cep_RJ$ENDRUA_base_num=base_cep_RJ$ENDRUA_base

for (i in seq_len(nrow(dicionario_numbers))) {
  base_cep_RJ$ENDRUA_base_num=  gsub(dicionario_numbers$from[i], 
                                     dicionario_numbers$to[i],
                                     base_cep_RJ$ENDRUA_base_num, perl = T)
}

base_cep_RJ$ENDRUA_base_num=gsub('[[:space:]]+', ' ',base_cep_RJ$ENDRUA_base_num)

#LOGRATIPO: de digito para numero por extenso
base_cep_RJ$LOGRATIPO_base_num= as.character(base_cep_RJ$LOGRATIPO_base)

for (i in seq_len(nrow(dicionario_numbers))) {
  base_cep_RJ$LOGRATIPO_base_num=  gsub(dicionario_numbers$from[i], 
                                        dicionario_numbers$to[i],
                                        base_cep_RJ$LOGRATIPO_base_num, perl = T)
}

base_cep_RJ$LOGRATIPO_base_num=gsub('[[:space:]]+', ' ',base_cep_RJ$LOGRATIPO_base_num)


#ENDRUA: de numero por extenso para digito
dicionario_numbers$from=gsub('[\\b]|[\\b]', "", dicionario_numbers$from)
dicionario_numbers$from = trim(gsub('[[:space:]]+', ' ', dicionario_numbers$from))
dicionario_numbers$to = trim(gsub('[[:space:]]+', ' ', dicionario_numbers$to))
dicionario_numbers$to = paste0('[[:space:]]',dicionario_numbers$to,'[[:space:]]',"|", 
                               "^", dicionario_numbers$to,'[[:space:]]',"|",
                               '[[:space:]]',dicionario_numbers$to,"$","|",
                               "^",dicionario_numbers$to,"$")
                               
dicionario_numbers$from = str_to_upper(paste(' ',dicionario_numbers$from,' '))

# com Wnum: comecar de 199 a 1, para evitar problemas como VINTE E SEIS = VINTE E 6 
dicionario_numbers=purrr::map_df(dicionario_numbers, rev) 


base_cep_RJ$ENDRUA_base_Wnum=as.character(base_cep_RJ$ENDRUA_base)
for (i in seq_len(nrow(dicionario_numbers))) {
  base_cep_RJ$ENDRUA_base_Wnum=  gsub(dicionario_numbers$to[i], 
                                      dicionario_numbers$from[i],
                                      base_cep_RJ$ENDRUA_base_Wnum)
}


base_cep_RJ$ENDRUA_base_Wnum=gsub('[[:space:]]+', ' ',base_cep_RJ$ENDRUA_base_Wnum)


#LOGRATIPO_base de numero por extenso para digito
base_cep_RJ$LOGRATIPO_base_Wnum=trim(as.character(base_cep_RJ$LOGRATIPO_base))
for (i in seq_len(nrow(dicionario_numbers))) {
  base_cep_RJ$LOGRATIPO_base_Wnum=  gsub(dicionario_numbers$to[i], 
                                         dicionario_numbers$from[i],
                                         base_cep_RJ$LOGRATIPO_base_Wnum)
}
base_cep_RJ$LOGRATIPO_base_Wnum=gsub('[[:space:]]+', ' ',
                                     base_cep_RJ$LOGRATIPO_base_Wnum)


#########################################
#BAIRRO EDICAO BASICA
base_cep_RJ$BAIRRO_base=stringi::stri_trans_general(base_cep_RJ$BAI_NO, 
                                                    "Latin-ASCII") #remover acentos
base_cep_RJ$BAIRRO_base = trim(str_to_upper(base_cep_RJ$BAIRRO_base))
base_cep_RJ$BAIRRO_base =gsub("[[:punct:]]+", "",base_cep_RJ$BAIRRO_base)

########################################################
base_cep_RJ$BAIRRO_base_F=gsub("[[:space:]]+", " ", base_cep_RJ$BAIRRO_base)


for (i in seq_len(nrow(dicionario_bairro))) {
  base_cep_RJ$BAIRRO_base_F=  gsub(dicionario_bairro$from[i], 
                                   dicionario_bairro$to[i],
                                 base_cep_RJ$BAIRRO_base_F)
}

base_cep_RJ$BAIRRO_base_F=gsub("[[:space:]]+", " ", base_cep_RJ$BAIRRO_base_F)
base_cep_RJ$BAIRRO_base_F=trim(base_cep_RJ$BAIRRO_base_F)
########################################################

base_cep_RJ$BAIRRO_base_alt=stringi::stri_trans_general(base_cep_RJ$VDB_TX1, 
                                                        "Latin-ASCII") 
base_cep_RJ$BAIRRO_base_alt = trim(str_to_upper(base_cep_RJ$BAIRRO_base_alt))

base_cep_RJ$BAIRRO_base_alt2=stringi::stri_trans_general(base_cep_RJ$VDB_TX2,
                                                         "Latin-ASCII") 
base_cep_RJ$BAIRRO_base_alt2 = trim(str_to_upper(base_cep_RJ$BAIRRO_base_alt2))


base_cep_RJ$PARES_base=paste(base_cep_RJ$LOGRATIPO_base, 
                             base_cep_RJ$BAIRRO_base_F, sep = ", ")
base_cep_RJ$LOGRADOURO_list=paste(base_cep_RJ$ENDRUA_base,
                                  base_cep_RJ$BAIRRO_base_F, sep = ", ") 


base_cep_RJ$COMPL_base=stringi::stri_trans_general(base_cep_RJ$LOG_COMPLEMENTO,
                                                   "Latin-ASCII") 
base_cep_RJ$COMPL_base = str_to_upper(base_cep_RJ$COMPL_base)

base_cep_RJ$COMPL_base1=str_extract(base_cep_RJ$COMPL_base , "\\((.*)\\)")

##regex [[:alpha:]] [[:punct:]]|[[:alpha:]][[:punct:]][[:space:]] 
#funciona corretamente com: 
# string, 9
# string, 64 2º Andar
# string- de 60 ao fim - lado par
# string - até 61 - lado ímpar
# string - lado ímpar
# string - lado par
# Beira-Mar, 406
#problema com: string - de 172 de 172 ao fim - lado par 
#solução  extrair - lado ímpar e lado par

patterns= c("[[:alpha:]] [[:punct:]]","[[:punct:]][[:space:]]")
patterns=(paste0(patterns,collapse = "|"))

base_cep_RJ$PAR_IMPAR_base=str_extract(base_cep_RJ$COMPL_base, "LADO [[:alpha:]]+")
base_cep_RJ$PAR_IMPAR_base=ifelse(is.na(base_cep_RJ$PAR_IMPAR_base), "SL", 
                                  base_cep_RJ$PAR_IMPAR_base)

base_cep_RJ$ENDNUMERO_base= gsub("- LADO [[:alpha:]]+", " ",
                                 base_cep_RJ$COMPL_base)
base_cep_RJ$ENDNUMERO_base= gsub("\\((.*)\\)", " ", 
                                 base_cep_RJ$ENDNUMERO_base, perl = T)
base_cep_RJ$ENDNUMERO_base=vapply(strsplit(base_cep_RJ$ENDNUMERO_base, patterns),
                                  "[", "", 2)

base_cep_RJ$ENDNUMERO_base=gsub("/[[:digit:]]+ AO FIM", " AO FIM" , base_cep_RJ$ENDNUMERO_base)
base_cep_RJ$ENDNUMERO_base=gsub("A [[:digit:]]+/", " A " , base_cep_RJ$ENDNUMERO_base)
base_cep_RJ$ENDNUMERO_base=gsub("ATE [[:digit:]]+/", " ATE " , base_cep_RJ$ENDNUMERO_base)
base_cep_RJ$ENDNUMERO_base=gsub("/[[:digit:]]+", "" , base_cep_RJ$ENDNUMERO_base)

base_cep_RJ$ENDNUMERO_base=gsub("[/]", "" , base_cep_RJ$ENDNUMERO_base)
base_cep_RJ$ENDNUMERO_base=gsub("º", "" , base_cep_RJ$ENDNUMERO_base)
base_cep_RJ$ENDNUMERO_base=gsub("-", "" , base_cep_RJ$ENDNUMERO_base)
base_cep_RJ$ENDNUMERO_base=gsub("LOJA [[:alnum:]]+", "" , base_cep_RJ$ENDNUMERO_base)
base_cep_RJ$ENDNUMERO_base=gsub("SALA [[:alnum:]]+", "" , base_cep_RJ$ENDNUMERO_base)
base_cep_RJ$ENDNUMERO_base=gsub("LOJAS [[:alnum:]]+", "" , base_cep_RJ$ENDNUMERO_base)
base_cep_RJ$ENDNUMERO_base=gsub("[[:digit:]] PISO", "" , base_cep_RJ$ENDNUMERO_base)
base_cep_RJ$ENDNUMERO_base=trim(base_cep_RJ$ENDNUMERO_base)
base_cep_RJ$ENDNUMERO_base=gsub('^0+', '0', base_cep_RJ$ENDNUMERO_base)
base_cep_RJ$ENDNUMERO_base=numero = gsub("[[:space:]]+0", " ", base_cep_RJ$ENDNUMERO_base)
base_cep_RJ$ENDNUMERO_base=trim(base_cep_RJ$ENDNUMERO_base)

base_cep_RJ$ENDNUMERO_base=ifelse(is.na(base_cep_RJ$ENDNUMERO_base), "SN", 
                                  base_cep_RJ$ENDNUMERO_base)

base_cep_RJ$ENDNUMERO_base=ifelse(str_detect(base_cep_RJ$ENDNUMERO_base, 
                                             "[[:digit:]]+")==FALSE,
                                  "SN", 
                                  ifelse(str_detect(base_cep_RJ$ENDNUMERO_base,
                                                    "CJ")==TRUE,
                                         "SN", base_cep_RJ$ENDNUMERO_base))




base_cep_RJ$NUMERO_controle_base=ifelse(str_detect(base_cep_RJ$ENDNUMERO_base,
                                                   "DE [[:digit:]]+ AO FIM"), 
                                        "MAIOR OU IGUAL", 
                                    ifelse(str_detect( base_cep_RJ$ENDNUMERO_base, 
                                                      "ATE [[:digit:]]+"), 
                                              "MENOR OU IGUAL",
                                    ifelse(str_detect( base_cep_RJ$ENDNUMERO_base, 
                                              "DE [[:digit:]]+ A [[:digit:]]+"), 
                                       "INTERVALO", 
                                    ifelse(str_detect(base_cep_RJ$ENDNUMERO_base,
          "DE[[:space:]]+[[:digit:]]+[[:space:]]+A[[:space:]]+[[:digit:]]+"), 
                                              "INTERVALO", 
                                   ifelse(str_detect(base_cep_RJ$ENDNUMERO_base, 
                                                     "[[:alpha:]]+")==FALSE, 
                                              "NUMERO", 
                                  ifelse(str_detect(base_cep_RJ$ENDNUMERO_base, 
                                "LOT[[:space:]]+|LOTE[[:space:]]+"), "LOTE", 
                                
                                  ifelse(str_detect(base_cep_RJ$ENDNUMERO_base,
                                                    "A QUADRA [[:digit:]]+"), 
                                                               "INTERVALO QUADRA", 
                                  ifelse(str_detect(base_cep_RJ$ENDNUMERO_base,
                                                    "ATE QUADRA [[:digit:]]+"), 
                                                "QUADRA MENOR OU IGUAL", 
                                  ifelse(str_detect(base_cep_RJ$ENDNUMERO_base, 
                                            "DE QUADRA [[:digit:]]+ AO FIM"), 
                                                "QUADRA MAIOR OU IGUAL", 
                                  ifelse(base_cep_RJ$ENDNUMERO_base=="SN", 
                                                  "SN", "ERROR"))))))))))


base_cep_RJ$ENDNUMERO_base_F=ifelse(base_cep_RJ$NUMERO_controle_base=="NUMERO" |
                          base_cep_RJ$NUMERO_controle_base=="LOTE",
                          str_extract(base_cep_RJ$ENDNUMERO_base, "[[:digit:]]+"),
                         
                          ifelse(base_cep_RJ$NUMERO_controle_base=="MAIOR OU IGUAL"| 
                          base_cep_RJ$NUMERO_controle_base=="QUADRA MAIOR OU IGUAL", 
                          str_extract(base_cep_RJ$ENDNUMERO_base, "[[:digit:]]+"),
                          
                          ifelse(base_cep_RJ$NUMERO_controle_base=="MENOR OU IGUAL"|
                          base_cep_RJ$NUMERO_controle_base=="QUADRA MENOR OU IGUAL", 
                          str_extract(base_cep_RJ$ENDNUMERO_base, "[[:digit:]]+"), 
                          
                          ifelse(base_cep_RJ$NUMERO_controle_base=="INTERVALO"|
                          base_cep_RJ$NUMERO_controle_base=="INTERVALO QUADRA", 
                          gsub("[[:space:]]+", ":", trim(gsub("[[:alpha:]]+", "",
                          base_cep_RJ$ENDNUMERO_base))),"SN"))))
                                                  


base_cep_RJ$ENDNUMERO_base_F2=ifelse(base_cep_RJ$NUMERO_controle_base=="INTERVALO"|
                              base_cep_RJ$NUMERO_controle_base=="INTERVALO QUADRA", 
                              gsub("[[:digit:]]+:", "", base_cep_RJ$ENDNUMERO_base_F), NA)

base_cep_RJ$ENDNUMERO_base_F2=gsub("[[punct]]+", "", base_cep_RJ$ENDNUMERO_base_F2)
                                                         
base_cep_RJ$ENDNUMERO_base_F=ifelse(base_cep_RJ$NUMERO_controle_base=="INTERVALO"|
                             base_cep_RJ$NUMERO_controle_base=="INTERVALO QUADRA", 
                           gsub(":[[:digit:]]+", "", base_cep_RJ$ENDNUMERO_base_F),
                           base_cep_RJ$ENDNUMERO_base_F)

base_cep_RJ$ENDNUMERO_base_F=gsub("[[punct]]+", "", base_cep_RJ$ENDNUMERO_base_F)


#somente o municipio Rio de Janeiro faixa do cep	20000001 a 23799999		
base_cep_RJ=subset(base_cep_RJ, base_cep_RJ$CEP>=20000001 & 
                     base_cep_RJ$CEP<=23799999) 

write.csv(base_cep_RJ, file="base_cep_RJ_OFICIAL_1809_MunRJ.csv")

###############################################################################################
#Lista de nomes de Logradouros (sem o tipo) únicos 
#(só ocorrem num mesmo bairro)
#Lista de nomes de Logradouros repetidas (sem o tipo) 
#(ocorrem em mais de um bairro)

LOGRADOURO_list=subset(base_cep_RJ, select = c(ENDRUA_base, BAIRRO_base_F, 
                                               TIPO_base, 
                                               LOGRATIPO_base, ENDRUA_base_num,
                                               ENDRUA_base_Wnum, 
                                               LOGRATIPO_base_num,
                                               LOGRATIPO_base_Wnum))

LOGRADOURO_list=LOGRADOURO_list[!duplicated(LOGRADOURO_list[,c("ENDRUA_base",
                                                               "BAIRRO_base_F")]),]


LOGRADOURO_list = LOGRADOURO_list %>% group_by(ENDRUA_base) %>% 
  mutate(duplicate.flag = n() > 1)
names(LOGRADOURO_list)[9]= "LOG_DUPLICADA"

LOGRADOURO_list = LOGRADOURO_list %>% group_by(LOGRATIPO_base) %>% 
  mutate(duplicate.flag = n() > 1)
names(LOGRADOURO_list)[10]= "TIPO_DUPLICADA"

LOGRADOURO_list = LOGRADOURO_list %>% group_by(LOGRATIPO_base, BAIRRO_base_F) %>% 
  mutate(duplicate.flag = n() > 1)
names(LOGRADOURO_list)[11]= "TIPO_DUPLICADA_BAIRRO"

table(LOGRADOURO_list$LOG_DUPLICADA)
table(LOGRADOURO_list$TIPO_DUPLICADA)
table(LOGRADOURO_list$TIPO_DUPLICADA_BAIRRO)

tipo_rep=subset(LOGRADOURO_list, LOGRADOURO_list$TIPO_DUPLICADA_BAIRRO==TRUE, 
                select=c(LOGRATIPO_base_num))
tipo_rep2=subset(LOGRADOURO_list, LOGRADOURO_list$TIPO_DUPLICADA_BAIRRO==TRUE, 
                 select=c(LOGRATIPO_base_Wnum))

names(tipo_rep)[1]="LOGRATIPO_base"
names(tipo_rep2)[1]="LOGRATIPO_base"

tipo_rep$LOGRATIPO_base=trim(tipo_rep$LOGRATIPO_base)
tipo_rep$LOGRATIPO_base=trim(tipo_rep$LOGRATIPO_base)

tipo_rep=rbind.data.frame(tipo_rep, tipo_rep2)
tipo_rep=tipo_rep[!duplicated(tipo_rep[,c("LOGRATIPO_base")]),]
write.csv(tipo_rep, file="TIPOS_DUPLICATA_BAIRRO.csv")


###################################################################################
#lista de tipos
tipo=base_cep_RJ$TLO_TX
tipo=str_to_upper(tipo)
tipo=stringi::stri_trans_general(tipo, "Latin-ASCII") 
tipo=data.frame(unique(tipo))
names(tipo)[1]="TIPOS"
write.csv(tipo, file("tipos_rj.csv"))
##################################################################3


rm(list= ls()[!(ls() %in% c('base_cep_RJ','bdgeral'))])
