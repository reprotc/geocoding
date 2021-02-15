################################################################################
###########Script 4: Linkage com a base de dados dos correios (DNE) ############
################################################################################


#Recuperação das informações de logradouro e bairro 
#dos endereços com número de cep válido (8 dígitos)
#Linkage com a base DNE (para os endereços sem cep válido), 
#utilizando o logradouro como campo de blocagem.

#############################################################################
#Primeiro: definir area de trabalho: 
#local onde estao os arquivos com os enderecos e demais bancos

#por exemplo: setwd("/media/BANCOS)

################################################################################
#Instalar e carregar os pacotes do R

if (!require(fuzzyjoin)) install.packages('fuzzyjoin'); library(fuzzyjoin)
if (!require(gdata))  install.packages('gdata'); library(gdata)
if(!require(dplyr)) install.packages('dplyr'); library(dplyr)
if(!require(stringr)) install.packages('stringr'); library(stringr)


#Carregar funcoes
source("geofunctions.R")

#carregar base de cep
base_cep_RJ=read.csv("base_cep_RJ_OFICIAL_1809_MunRJ.csv", 
                     stringsAsFactors = F)

#carregar limites de bairros do RJ
limites=read.csv("limites_bairros_edit.csv",
                 stringsAsFactors = F,  na.strings=c("","NA"))

limites[is.na(limites)] = "SLIMITE"
limites$list=paste0("^",limites$Limite1, "$|",
                    "^",limites$Limite2,  "$|",
                    "^",limites$Limite3, "$|",
                    "^",limites$Limite4, "$|",
                    "^",limites$Limite5, "$|",
                    "^",limites$Limite6, "$|",
                    "^",limites$Limite7, "$|",
                    "^",limites$Limite8, "$|",
                    "^",limites$Limite9, "$|",
                    "^",limites$Limite10, "$|",
                    "^",limites$Limite11, "$|",
                    "^",limites$Limite12, "$|",
                    "^",limites$Limite13, "$|",
                    "^",limites$Limite14, "$|",
                    "^",limites$Limite15, "$|",
                    "^",limites$Limite16, "$|",
                    "^",limites$Limite17, "$")


limites_lits = subset(limites, select = c(Bairro, list))
names(limites_lits)[1] = "BAIRRO_F"

bdgeral_fmerge = subset(
  bdgeral,
  bdgeral$DL_10 == FALSE  &
    !is.na(bdgeral$LOGRATIPO),
  select = c(TIPO,LOGRATIPO_num,
    BAIRRO_F, PAR_IMPAR,
    ENDNUMERO, ENDLOTE,
    ENDQUADRA, ENDCASA,
    ENDTIPO_FULL, CEPRES,
    NUMERODO))

base_fmerge = subset(
  base_cep_RJ,
  select = c( TIPO_base,
    LOGRATIPO_base_num,
    BAIRRO_base_F, PAR_IMPAR_base,
    NUMERO_controle_base,
    ENDNUMERO_base_F, NDNUMERO_base_F2,
    LOG_COMPLEMENTO, CEP,
    LOG_NU))

names(base_fmerge)
names(base_fmerge)[2] = "LOGRATIPO_num"

#dividir o banco para facilitar = mudar de acordo com 
#o tamanho do banco final
bdgeral_fmerge1 = bdgeral_fmerge[1:6988, ]
bdgeral_fmerge2 = bdgeral_fmerge[6989:13976, ]
bdgeral_fmerge3 = bdgeral_fmerge[13977:20964, ]

cep_link1 = stringdist_join(
  bdgeral_fmerge1,
  base_fmerge,
  by = "LOGRATIPO_num",
  method = "lv",
  max_dist = 2,
  distance_col = "LV_LOGRATIPO"
)
cep_link2 = stringdist_join(
  bdgeral_fmerge2,
  base_fmerge,
  by = "LOGRATIPO_num",
  method = "lv",
  max_dist = 2,
  distance_col = "LV_LOGRATIPO"
)
cep_link3 = stringdist_join(
  bdgeral_fmerge3,
  base_fmerge,
  by = "LOGRATIPO_num",
  method = "lv",
  max_dist = 2,
  distance_col = "LV_LOGRATIPO"
)

rm(bdgeral_fmerge1, bdgeral_fmerge2, bdgeral_fmerge3, base_fmerge)


cep_link=rbind.data.frame(cep_link1, cep_link2)
cep_link=rbind.data.frame(cep_link, cep_link3)

rm(cep_link1, cep_link2, cep_link3)

#unir base cep com os limites dos bairros
cep_link=merge(cep_link, limites_lits, by="BAIRRO_F", all.x = T)

cep_link$ENDNUMERO_base_F=as.numeric(cep_link$ENDNUMERO_base_F)
cep_link$ENDNUMERO_base_F2=as.numeric(cep_link$ENDNUMERO_base_F2)
cep_link$ENDNUMERO=as.numeric(cep_link$ENDNUMERO)
cep_link$ENDQUADRA=trim(gsub("[[:alpha:]]+", "", cep_link$ENDQUADRA))
cep_link$ENDQUADRA=as.numeric(cep_link$ENDQUADRA)

#verificar se os pares de enderecos (correrios e original) sao compativeis 
#no campo numero

cep_link$COMP_NUMERO = ifelse(
  cep_link$NUMERO_controle_base == "NUMERO" &
    cep_link$ENDNUMERO == cep_link$ENDNUMERO_base_F,"TRUE",
  ifelse(cep_link$NUMERO_controle_base == "MAIOR OU IGUAL" &
      cep_link$ENDNUMERO >= cep_link$ENDNUMERO_base_F, "TRUE",
    ifelse(cep_link$NUMERO_controle_base == "MENOR OU IGUAL" &
        cep_link$ENDNUMERO <= cep_link$ENDNUMERO_base_F, "TRUE",
     ifelse(cep_link$NUMERO_controle_base == "INTERVALO" &
          cep_link$ENDNUMERO >= cep_link$ENDNUMERO_base_F &
          cep_link$ENDNUMERO <= cep_link$ENDNUMERO_base_F2, "TRUE",
       ifelse(cep_link$NUMERO_controle_base == "INTERVALO QUADRA" &
            cep_link$ENDQUADRA >= cep_link$ENDNUMERO_base_F &
            cep_link$ENDQUADRA <= cep_link$ENDNUMERO_base_F2,"TRUE",
         ifelse(cep_link$NUMERO_controle_base == "QUADRA MENOR OU IGUAL" &
              cep_link$ENDQUADRA <= cep_link$ENDNUMERO_base_F,"TRUE",
           ifelse(cep_link$NUMERO_controle_base == "QUADRA MAIOR OU IGUAL" &
                cep_link$ENDQUADRA >= cep_link$ENDNUMERO_base_F,
              "TRUE",
              ifelse(cep_link$NUMERO_controle_base ==
                       "SN", "SN", "FALSE"))))))))
#verificar se os pares de enderecos (correrios e original) sao compativeis 
#no campo lado
cep_link$COMP_NUMERO_LADO= ifelse(cep_link$COMP_NUMERO=="TRUE" & 
                           !cep_link$PAR_IMPAR_base=="SL" & 
                             cep_link$PAR_IMPAR==cep_link$PAR_IMPAR_base, "TRUE",
                          ifelse(cep_link$COMP_NUMERO=="SN" & 
                                  !cep_link$PAR_IMPAR_base=="SL" &
                            cep_link$PAR_IMPAR==cep_link$PAR_IMPAR_base, "TRUE", 
                          ifelse(cep_link$COMP_NUMERO=="SN" 
                                 & cep_link$PAR_IMPAR_base=="SL", "TRUE", 
                          ifelse(cep_link$COMP_NUMERO=="TRUE" 
                                 & cep_link$PAR_IMPAR_base=="SL", "TRUE", 
                                 "FALSE"))))


##verificar se os pares de enderecos (correrios e original) sao compativeis 
#no campo tipo

cep_link$COMP_TIPO=ifelse(cep_link$TIPO==cep_link$TIPO_base, "TRUE", "FALSE")

cep_link=cep_link%>% group_by(NUMERODO) %>% 
  mutate(TIPOFALSE= n_distinct(COMP_TIPO)==1)

cep_link$COMP_TIPO2=ifelse(cep_link$COMP_TIPO=="FALSE" 
                           & cep_link$TIPOFALSE=="TRUE", "TRUE", 
                           cep_link$COMP_TIPO)

#CEPS IGUAIS BANCO ORIGINAL E NOVO CEP
cep_link$CEP_IGUAL=cep_link$CEP==cep_link$CEPRES
cep_link$CEP_IGUAL=ifelse(is.na(cep_link$CEP_IGUAL), "FALSE", cep_link$CEP_IGUAL)

##verificar se os pares de enderecos (correrios e original) sao compativeis 
#no campo bairro

cep_link$COMP_BAIRRO= ifelse(cep_link$BAIRRO_F==cep_link$BAIRRO_base_F, "TRUE", "FALSE")

cep_link$BAIRRO_base_F=trim(cep_link$BAIRRO_base_F)

#cep
for (i in 1:nrow(cep_link)) {
  print(i)
  #cep_link$COMP_BAIRRO_limite[i]=str_detect(cep_link$BAIRRO_base_F[i], cep_link$list[i])
  cep_link$COMP_BAIRRO_limite[i]=grepl(cep_link$list[i], cep_link$BAIRRO_base_F[i])
  
}

#compativel tudo, segundo distancia de lv
cep_link$COMPALL_LV0 = ifelse(
  cep_link$LV_LOGRATIPO == 0 &
    cep_link$COMP_NUMERO_LADO == TRUE &
    cep_link$COMP_TIPO == TRUE & cep_link$COMP_BAIRRO == TRUE,
  "TRUE",
  ifelse(
    cep_link$LV_LOGRATIPO == 0 &
      cep_link$COMP_NUMERO_LADO == TRUE &
      cep_link$COMP_TIPO2 == TRUE &
      cep_link$COMP_BAIRRO == TRUE,
    "TRUE",
    "FALSE"))


cep_link$COMPALL_LV0_lim = ifelse(
  cep_link$LV_LOGRATIPO == 0 &
    cep_link$COMP_NUMERO_LADO == TRUE &
    cep_link$COMP_TIPO == TRUE &
    cep_link$COMP_BAIRRO == FALSE &
    cep_link$COMP_BAIRRO_limite == TRUE,
  "TRUE",
  ifelse(
    cep_link$LV_LOGRATIPO == 0 &
      cep_link$COMP_NUMERO_LADO == TRUE &
      cep_link$COMP_TIPO2 == TRUE &
      cep_link$COMP_BAIRRO == FALSE &
      cep_link$COMP_BAIRRO_limite == TRUE,
    "TRUE",
    "FALSE"))


cep_link$COMPALL_LV2 = ifelse(
  cep_link$LV_LOGRATIPO >= 1 &
    cep_link$LV_LOGRATIPO <= 2 &
    cep_link$COMP_NUMERO_LADO == TRUE &
    cep_link$COMP_TIPO == TRUE & cep_link$COMP_BAIRRO == TRUE,
  "TRUE",
  ifelse(
    cep_link$LV_LOGRATIPO >= 1 &
      cep_link$LV_LOGRATIPO <= 2 &
      cep_link$COMP_NUMERO_LADO == TRUE &
      cep_link$COMP_TIPO2 == TRUE &
      cep_link$COMP_BAIRRO == TRUE,
    "TRUE",
    "FALSE"))

cep_link$COMPALL_LV2_lim = ifelse(
  cep_link$LV_LOGRATIPO >= 1 &
    cep_link$LV_LOGRATIPO <= 2 &
    cep_link$COMP_NUMERO_LADO == TRUE &
    cep_link$COMP_TIPO == TRUE &
    cep_link$COMP_BAIRRO == FALSE &
    cep_link$COMP_BAIRRO_limite == TRUE,
  "TRUE",
  ifelse(
    cep_link$LV_LOGRATIPO >= 1 &
      cep_link$LV_LOGRATIPO <= 2 &
      cep_link$COMP_NUMERO_LADO == TRUE &
      cep_link$COMP_TIPO2 == TRUE &
      cep_link$COMP_BAIRRO == FALSE &
      cep_link$COMP_BAIRRO_limite == TRUE,
    "TRUE",
    "FALSE"))

# contar o numero de pares "compativeis" segundo cada criterio
cep_link=cep_link %>% 
  group_by(NUMERODO, COMPALL_LV0)%>% 
  add_tally()

cep_link=cep_link %>% 
  group_by(NUMERODO, COMPALL_LV0_lim)%>% 
  add_tally()


cep_link=cep_link %>% 
  group_by(NUMERODO, COMPALL_LV2)%>% 
  add_tally()

cep_link=cep_link %>% 
  group_by(NUMERODO, COMPALL_LV2_lim)%>% 
  add_tally()

names(cep_link)
names(cep_link)[36]="N_COMPALL_LV0"
names(cep_link)[37]="N_COMPALL_LV0_lim"
names(cep_link)[38]="N_COMPALL_LV2"
names(cep_link)[39]="N_COMPALL_LV2_lim"

#Verificar separadamente os pares compativeis, segundo criterios
# do mais "rigido" lv=0 ao menos rigido Lv=2

selec_numerodo = subset(
  cep_link,
  cep_link$COMPALL_LV0 == TRUE &
    cep_link$N_COMPALL_LV0 == 1,
  select = c(NUMERODO))

selec_numerodo = unique(selec_numerodo$NUMERODO)
cep_link$NUMERODO_SELEC_LV0 = ifelse(cep_link$NUMERODO %in% selec_numerodo, 
                                     "TRUE", "FALSE")

cep_link$SELEC_LV0_all = ifelse(
  cep_link$COMPALL_LV0 == TRUE & cep_link$N_COMPALL_LV0 == 1,
  "TRUE",
  ifelse(
    cep_link$NUMERODO_SELEC_LV0 == FALSE &
      cep_link$COMPALL_LV0_lim == TRUE &
      cep_link$N_COMPALL_LV0_lim == 1,
    "TRUE",
    "FALSE"))

selec_numerodo = subset(cep_link, cep_link$SELEC_LV0_all == TRUE, 
                        select = c(NUMERODO))

selec_numerodo = unique(selec_numerodo$NUMERODO)
cep_link$NUMERODO_SELEC_LV0_all = ifelse(cep_link$NUMERODO %in% selec_numerodo,
                                         "TRUE", "FALSE")

selec_numerodo2 = subset(
  cep_link,
  cep_link$NUMERODO_SELEC_LV0_all == FALSE &
    cep_link$COMPALL_LV2 == TRUE &
    cep_link$N_COMPALL_LV2 == 1,
  select = c(NUMERODO))


#Verificar se o registro ja foi pareado utilizando o criterio anterior 
#(mais rigido), selecionar apenas os que nao foram pareados anteriormente
selec_numerodo2 = unique(selec_numerodo2$NUMERODO)
cep_link$NUMERODO_SELEC_LV2 = ifelse(cep_link$NUMERODO %in% selec_numerodo2,
                                     "TRUE", "FALSE")

cep_link$SELEC_LV2_all = ifelse(
  cep_link$NUMERODO_SELEC_LV0_all == FALSE &
    cep_link$COMPALL_LV2 == TRUE & cep_link$N_COMPALL_LV2 == 1,
  "TRUE",
  ifelse(
    cep_link$NUMERODO_SELEC_LV0_all == FALSE &
      cep_link$NUMERODO_SELEC_LV2 == FALSE &
      cep_link$COMPALL_LV2_lim == TRUE &
      cep_link$N_COMPALL_LV2_lim == 1,
    "TRUE",
    "FALSE"))

#Selecionar todos os pares
cep_link$SELEC_all = ifelse(cep_link$SELEC_LV0_all == TRUE |
                              cep_link$SELEC_LV2_all == TRUE,
                            "TRUE",
                            "FALSE")

selec_numerodo = subset(cep_link, cep_link$SELEC_all == TRUE, select = c(NUMERODO))
selec_numerodo = unique(selec_numerodo$NUMERODO)
cep_link$NUMERODO_SELEC_all = ifelse(cep_link$NUMERODO %in% selec_numerodo,
                                     "TRUE", "FALSE")

selec_cep = subset(
  cep_link,
  cep_link$SELEC_all == TRUE,
  select = c(
    NUMERODO,
    TIPO,
    TIPO_base,
    LOGRATIPO_num.x,
    LOGRATIPO_num.y,
    BAIRRO_F,
    BAIRRO_base_F,
    CEPRES,
    CEP
  )
)
selec_cep = selec_cep %>% group_by(NUMERODO) %>% mutate(MESMO_record_F = n() > 1)
names(selec_cep)

names(selec_cep)[3] = "TIPO_fuzzy"
names(selec_cep)[5] = "LOGRATIPO_num_fuzzy"
names(selec_cep)[7] = "BAIRRO_fuzzy"
names(selec_cep)[9] = "CEPRES_fuzzy"

selec_cep$ENDRUA_num_fuzzy=paste(selec_cep$TIPO_fuzzy, 
                                 selec_cep$LOGRATIPO_num_fuzzy)
selec_cep$TIPO_status_fuzzy= ifelse(selec_cep$LOGRATIPO_num_fuzzy%in%tipo_rep, 
                                    "TRUE", "FALSE")

n_selec_cep= subset(cep_link, cep_link$SELEC_all==FALSE & 
                      cep_link$NUMERODO_SELEC_all==FALSE)

selec_cep=subset(selec_cep, 
                 select = c(NUMERODO,ENDRUA_num_fuzzy, TIPO_fuzzy,
                            LOGRATIPO_num_fuzzy, BAIRRO_fuzzy, CEPRES_fuzzy,
                                       TIPO_status_fuzzy))


#Unir e padronizar as informacoes obtidas pelo linkage com o banco original
bdgeral=merge(bdgeral, selec_cep, by="NUMERODO", all.x = TRUE)


bdgeral$ENDRUA_num_fuzzy=gsub("[[:punct:]]+", " ", bdgeral$ENDRUA_num_fuzzy)
bdgeral$ENDRUA_num_fuzzy=gsub("[[:space:]]+", " ", bdgeral$ENDRUA_num_fuzzy)
bdgeral$ENDRUA_num_fuzzy=trim(bdgeral$ENDRUA_num_fuzzy)

bdgeral$LOGRATIPO_num_fuzzy=gsub("[[:punct:]]+", " ", bdgeral$LOGRATIPO_num_fuzzy)
bdgeral$LOGRATIPO_num_fuzzy=gsub("[[:space:]]+", " ", bdgeral$LOGRATIPO_num_fuzzy)
bdgeral$LOGRATIPO_num_fuzzy=trim(bdgeral$LOGRATIPO_num_fuzzy)


bdgeral$BAIRRO_fuzzy=gsub("[[:punct:]]+", " ", bdgeral$BAIRRO_fuzzy)
bdgeral$BAIRRO_fuzzy=gsub("[[:space:]]+", " ", bdgeral$BAIRRO_fuzzy)
bdgeral$BAIRRO_fuzzy=trim(bdgeral$BAIRRO_fuzzy)

#Salvar o banco final
write.csv(bdgeral, file="BANCO_formatado_final.csv")

rm(list= ls()[!(ls() %in% c('bdgeral'))])