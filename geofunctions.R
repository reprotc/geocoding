# Funcoes (fora dos pacotes) utilizadas nos scripts

# palavras/nomes mais/menos frequentes
freqfunc_less <- function(x, n) {
  tail(sort(table(unlist(
    strsplit(as.character(x), ", ")
  ))), n)
}


# funcao para construcao do dicionario/corretor de erros no nome do logradouro
dic_erros <- function(var) {
#regras de erros comuns de grafia adaptadas de Giusti et al, 2007:
  #http://nilc.icmc.usp.br/nilc/projects/hpc/cl_2007/cl_2007.pdf
  
rul1 = ifelse(stringr::str_detect(var, "Y") &
                stringr::str_length(var) > 4 ,
              paste(var, sub("Y", "I", var), sep = ";"), NA)
rul2 = ifelse(stringr::str_detect(var, "PH") &
                stringr::str_length(var) > 4,
              paste(var, gsub("PH", "F", var), sep = ";"),NA)
rul3 = ifelse(stringr::str_detect(var, "TH") &
                stringr::str_length(var) > 4,
              paste(var, gsub("TH", "T", var), sep = ";"),NA)
rul4 = ifelse(stringr::str_detect(var, "T") &
                stringr::str_length(var) > 4,
              paste(var, sub("T", "TH", var), sep = ";"),NA)
rul5 = ifelse(stringr::str_detect(var, "MP") &
                stringr::str_length(var) > 4,
              paste(var, sub("MP", "NP", var), sep = ";"),NA)
rul6 = ifelse(stringr::str_detect(var, "MB") &
                stringr::str_length(var) > 4,
              paste(var, sub("MB", "NB", var), sep = ";"),NA)
rul7 = ifelse(stringr::str_detect(var, "CH") &
                stringr::str_length(var) > 4,
              paste(var, gsub("CH", "X", var), sep = ";"),NA)
rul8 = ifelse(stringr::str_detect(var, "Z") &
                stringr::str_length(var) > 4,
              paste(var, gsub("Z", "S", var), sep = ";"),NA)
rul9 = ifelse(stringr::str_detect(var, "S") &
                stringr::str_length(var) > 4,
              paste(var, gsub("S", "Z", var), sep = ";"),NA)
rul10 = ifelse(stringr::str_detect(var, "K") &
                 stringr::str_length(var) > 4,
               paste(var, gsub("K", "C", var), sep = ";"),NA)
rul11 = ifelse(stringr::str_detect(var, "FF") &
                 stringr::str_length(var) > 4,
               paste(var, gsub("FF", "F", var), sep = ";"),NA)
rul12 = ifelse(stringr::str_detect(var, "TT") &
                 stringr::str_length(var) > 4,
               paste(var, gsub("TT", "T", var), sep = ";"),NA)
rul13 = ifelse(stringr::str_detect(var, "SC") &
                 stringr::str_length(var) > 4,
               paste(var, gsub("SC", "C", var), sep = ";"),NA)
rul14 = ifelse(stringr::str_detect(var, "RR") &
                 stringr::str_length(var) > 4,
               paste(var, gsub("RR", "R", var), sep = ";"),NA)
rul15 = ifelse(stringr::str_detect(var, "CHE") &
                 stringr::str_length(var) > 4,
               paste(var, gsub("CHE", "CHA", var), sep = ";"),NA)
rul16 = ifelse(stringr::str_detect(var, "CHA") &
                 stringr::str_length(var) > 4,
               paste(var, gsub("CHA", "CHE", var), sep = ";"),NA)

#Regras/erros pouco frequentes no banco do SIM/tio:
#rul17= ifelse(stringr::str_detect(var, "O\\b") & 
#stringr::str_length(var)>4, 
#paste(var,gsub("O\\b","U", var), sep= ";"), NA)
#rul18= gsub("FRANCISCU","FRANCISCO", rul11)
#rul19= ifelse(stringr::str_detect(var, "E\\b") & 
#stringr::str_length(var)>4, 
#paste(var,gsub("E\\b","I", var), sep= ";"), NA)
#rul20= ifelse(stringr::str_detect(var, "I\\b") & 
#stringr::str_length(var)>4, 
#paste(var,gsub("I\\b","E", var), sep= ";"), NA)
#rul21= ifelse(stringr::str_detect(var, "AL\\b") & 
#stringr::str_length(var)>4,
#paste(var,gsub("AL\\b","AU", var), sep= ";"), NA)
#rul22= ifelse(stringr::str_detect(var, "IZ\\b") & 
#stringr::str_length(var)>4,
#paste(var,gsub("IZ\\b","Z", var), sep= ";"), NA)
#rul23= ifelse(stringr::str_detect(var, "S\\b") & 
#stringr::str_length(var)>4, 
#paste(var,gsub("S\\b","Z", var), sep= ";"), NA)
#rul24= ifelse(stringr::str_detect(var, "Z\\b") & 
#stringr::str_length(var)>4, 
#paste(var,gsub("Z\\b","S", var), sep= ";"), NA)
#rul25= ifelse(stringr::str_detect(var, "O\\b") & 
#stringr::str_length(var)>4,
#paste(var,gsub("O\\b","A", var), sep= ";"), NA)
#rul26= ifelse(stringr::str_detect(var, "A\\b") & 
#stringr::str_length(var)>4,
#paste(var,gsub("A\\b","O", var), sep= ";"), NA)
#rul27= ifelse(stringr::str_detect(var, "TI\\b") & 
#stringr::str_length(var)>4,
#paste(var,gsub("TI\\b","TE", var), sep= ";"), NA)
#rul28= ifelse(stringr::str_detect(var, "TE\\b") & 
#stringr::str_length(var)>4,
#paste(var,gsub("TE\\b","TI", var), sep= ";"), NA)

rul29 = ifelse(stringr::str_detect(var, "\\bE") &
              stringr::str_length(var) > 4,
              paste(var, gsub("\\bE", "HE", var), sep = ";"),NA)
rul30 = ifelse(stringr::str_detect(var, "\\bO") &
              stringr::str_length(var) > 4,
              paste(var, gsub("\\bO", "HO", var), sep = ";"),NA)
rul31 = gdata::trim(ifelse(stringr::str_detect(var, "\\bH") &
              stringr::str_length(var) > 4,
              paste(var, gsub("\\bH", " ", var), sep = ";"),NA))
rul32 = gdata::trim(ifelse(stringr::str_detect(var, "\\bDO\\b") &
              stringr::str_length(var) > 4,
              paste(var, gsub("\\bDO\\b", " ", var), sep = ";"),NA))
rul33 = gdata::trim(ifelse(stringr::str_detect(var, "\\bDE\\b") &
              stringr::str_length(var) > 4,
              paste(var, gsub("\\bDE\\b", " ", var), sep = ";"),NA))
rul34 = gdata::trim(ifelse(stringr::str_detect(var, "\\bDA\\b") &
              stringr::str_length(var) > 4,
              paste(var, gsub("\\bDA\\b", " ", var), sep = ";"),NA))
rul35 = gdata::trim(ifelse(stringr::str_detect(var, "\\bDA\\b") &
              stringr::str_length(var) > 4, 
              paste(var, gsub("\\bDA\\b", " DE ", var), sep = ";"),NA))
rul36 = gdata::trim(ifelse(stringr::str_detect(var, "\\bDO\\b") &
              stringr::str_length(var) > 4,
              paste(var, gsub("\\bDO\\b", " DA ", var), sep = ";"),NA))

rul37 = gdata::trim(ifelse(stringr::str_detect(var, "E\\b") &
              stringr::str_length(var) > 4,
              paste(var, gsub("E\\b", " ", var), sep = ";"),NA))
rul38 = gdata::trim(ifelse(stringr::str_detect(var, "S\\b") &
              stringr::str_length(var) > 4,
              paste(var, sub("S\\b", " ", var), sep = ";"),NA))
rul39 = gdata::trim(ifelse(stringr::str_detect(var, "S\\b") &
              stringr::str_length(var) > 4,
              paste(var, gsub("S\\b", " ", var), sep = ";"),NA))

#Titulos mais frequentes no rio. Outra opção é utilizar todos os titulos descritos no DNE = aumentará o tempo de edição
titulos = unique(
  c(
    "GENERAL",
    "PROFESSOR",
    "PROFESSORA",
    "MAJOR",
    "ALM",
    "ALMIRANTE",
    "PREF",
    "PREFEITO",
    "PRESIDENTE",
    "MINISTRO",
    "SOLDADO",
    "COMANDANTE",
    "BISPO",
    "REGENTE",
    "RADIALISTA",
    "JORNALISTA",
    "PRINCESA",
    "VEREADOR",
    "TENENTE",
    "SENADOR",
    "EMBAIXADOR",
    "DOUTOR",
    "DOUTORA",
    "DOM",
    "ENGENHEIRO",
    "CONDE",
    "CONDESSA",
    "CORONEL",
    "DEPUTADO",
    "BARAO",
    "BARONESA",
    "MARECHAL"
  )
)
titulos = paste0('\\b', titulos, '\\b', collapse = "|")
rul40 = gdata::trim(ifelse(
  stringr::str_detect(var, titulos) &
    stringr::str_length(var) > 4,
  paste(var, gsub(titulos, " ", var), sep = ";"),NA))

data=rbind(rul1, rul2, rul3, rul4, rul5, rul6, rul7, rul8, rul9, rul10,
           rul11, rul12, rul13, rul14, rul15, rul16,rul29,rul30,rul31,rul32,
           rul33,rul34,rul35,rul36,rul37,rul38,rul39, rul40)

#Criar dicionario na forma: From (grafia inconsistente)-to (nome correto do logradouro)
data = data.frame(data[!is.na(data)])
names(data)[1] = "to"
data$to = as.character(data$to)
data$from = vapply(strsplit(data$to, ";"), "[", "", 2)
data$to = vapply(strsplit(data$to, ";"), "[", "", 1)
data$from = gdata::trim(data$from)
data$to = gdata::trim(data$to)
data = data[!duplicated(data[, c("from", "to")]),]
#data=data%>% group_by(from) %>% mutate(duplicate.flag = n() > 1)
#data=subset(data, nchar(gsub(" ", "",data$from))>4 & data$duplicate.flag==FALSE)
data = subset(data, nchar(gsub(" ", "", data$from)) > 4)

rm(titulos, rul1, rul2, rul3, rul4, rul5, rul6, rul7, rul8, rul9, rul10,
   rul11, rul12, rul13, rul14, rul15, rul16, rul29,rul30,rul31,rul32,rul33,
   rul34,rul35,rul36,rul37,rul38,rul39, rul40)

return(data)

}


#par ou impar
e_par <- function(x)
  x %% 2 == 0


