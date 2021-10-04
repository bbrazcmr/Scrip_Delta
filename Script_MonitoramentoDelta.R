#Carrega os pacotes 
library(pdftools)
library(tidyverse)
library(stringr)
library(data.table)
library(base)
library(readxl)
setwd("D:/Cievs")

#Ler o texto do PDF e transformar em base de dados
texto_bruto <- pdf_text("SEI_GDF - 70662811 - Relatório Técnico.pdf") %>%
  strsplit("\n") %>%
  unlist() %>%
  enframe(name = NULL, value = "Linha")

#Limpar os dados do PDF
texto_limpo <- texto_bruto %>%
  tail(-59) %>% 
  head(-82)
#aqui dever? conter o c?digo de limpeza por express?es regurales

#Limpeza manual
texto_final_limpo <- texto_limpo %>% 
  slice(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37,
        39, 41, 43, 45, 47, 49, 51, 53, 57, 59, 61, 63, 65, 67, 69, 71, 73, 75,
        77, 79, 81, 83, 85, 87, 89, 91, 93, 95, 97, 99, 101, 103, 105, 107, 109,
        113, 115, 117, 119, 121, 123, 125, 127, 129, 131, 133, 135, 137, 139,
        141, 143, 145, 147, 149, 151, 153, 155, 157, 159, 161, 163, 165, 169, 
        171, 173, 175, 188, 190, 192, 194, 196, 198, 200, 202, 204, 206, 208,
        210, 212, 214, 216, 218, 220, 222, 228, 230, 232, 234, 236, 238, 240, 
        242, 244, 246, 248, 250, 252, 254, 256, 258, 260, 262, 264, 268, 270, 
        272)

#Separar as vari?veis
base_pdf <- texto_final_limpo %>%
  separate(Linha, c("Vazio", "Id", "Amostra","SES", 
                    "Coleta_dia", "Coleta_mes", "Coleta_ano", 
                    "DN_dia", "DN_mes", "DN_ano", 
                    "Resultado", "Plataforma"))

#Excluir a Coluna "Vazio"
base_pdf$Vazio <- NULL

#Arrumar as vari?veis de Datas
basepdf_teste <- base_pdf %>%
  mutate(ano_novo = as.numeric(DN_ano)) %>%
  mutate(ano_novo = ifelse(ano_novo<22, ano_novo+2000, ano_novo+1900))

base_pdf_final <- basepdf_teste %>%
  mutate(Coleta=paste(Coleta_dia, Coleta_mes, Coleta_ano, sep = "/")) %>%
  mutate(Data_Nascimento=paste(DN_dia,DN_mes,ano_novo, sep = "/"))


#Excluir vari?veis extras de datas
base_pdf_final$Coleta_dia <- NULL
base_pdf_final$Coleta_mes <- NULL
base_pdf_final$Coleta_ano <- NULL
base_pdf_final$DN_dia <- NULL
base_pdf_final$DN_mes <- NULL
base_pdf_final$DN_ano <- NULL
base_pdf_final$ano_novo <- NULL

#Filtrar somente os casos de Delta
base_lacen <- base_pdf_final %>%
  filter(Resultado == "DELTA")

#Remover objetos sem uso
rm(base_pdf, base_pdf_final, texto_bruto, texto_final_limpo, texto_limpo, 
   basepdf_teste)

#Importar a planilha com dados do Track Care
base_track_bruta <- read_xlsx("Delta LACEN.xlsx")

#Preparar a base para o merge
base_track_selec <- select(base_track_bruta, Amostra, Nome, Sexo, Endereço,
                           Resultado)


#Removendo duplicados
base_track <- base_track_selec %>%
  unique() %>%
  rename(sequenciamento = Resultado)

base_track$Resultado <- NULL

#Fazer o merge entre a base_pdf_final e a base_track pelo n? da Amosta
base1 <- merge(base_lacen, base_track, by = "Amostra")

#limpar o espaco de trabalho
rm(base_lacen, base_track, base_track_bruta, base_track_selec)


#Criando a funÃ§Ã£o de remover acentos
rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  pattern <- unique(pattern)
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  accentTypes <- c("´","`","^","~","¨","ç")
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  return(str)
}

#Padronizar variavel de nome na base1
nome_padronizado <- rm_accent(base1$Nome) #remove acento
nome_padronizado = stringr::str_trim(nome_padronizado) #remove espacos em branco 
nome_padronizado = toupper(nome_padronizado) #coloca tudo em CAPS
base_pad <- cbind(base1, nome_padronizado) #junta na base
base_pad$Nome <- NULL

#Limpar
rm(base1, nome_padronizado)
gc()

#Importar base esus
esus <- fread("e_SUS_ORIG.csv")

#Seleciona as variaveis de trabalho
esus_select <- select(esus, nomeCompleto, dataNascimento, dataInicioSintomas,
                      dataTeste, cbo, cpf, nomeMae, sexo, bairro, estado, 
                      municipio, numeroNotificacao, evolucaoCaso)

rm(esus)
gc()

#Prepara a variável nome da base esus para o Join
nome_padronizado <- rm_accent(esus_select$nomeCompleto)
nome_padronizado = stringr::str_trim(nome_padronizado)
nome_padronizado = toupper(nome_padronizado)
esus_select <- cbind(esus_select, nome_padronizado)
esus_select$nomeCompleto <- NULL
rm(nome_padronizado)

#Prepara a variável Data de Nascimento da base_pad
base_trat <- base_pad %>%
  rename(DN=Data_Nascimento) %>%
  mutate(DN=as.Date(DN, "%d/%m/%Y"))
rm(base_pad)

#Prepara a variável Data de Nascimento da base esus_select
esus_trat <- esus_select %>%
  rename(DN=dataNascimento) %>%
  mutate(DN=as.Date(DN, "%d/%m/%Y"))
rm(esus_select)

#Faz o join entre as duas bases
base_join1 <- left_join(base_trat, esus_trat, by=c("nome_padronizado", "DN"))

#Limpa o ambiente de objetos não mais utilizáveis
rm(base_trat, esus_trat)
gc()


#Arrumar CPF
baseJoin_trat <- base_join1 %>%
  separate(cpf, c("parte1", "parte2", "parte3", "parte4"))

baseJoin_trat <- baseJoin_trat %>%
  mutate(CPF = paste(parte1, parte2, parte3, parte4))

baseJoin_trat$parte1 <- NULL
baseJoin_trat$parte2 <- NULL
baseJoin_trat$parte3 <- NULL
baseJoin_trat$parte4 <- NULL

cpf_novo <- gsub("\\s", "", baseJoin_trat$CPF)
join1_df <- cbind(baseJoin_trat, cpf_novo)
join1_df$CPF <- NULL

join1_df = join1_df %>%
  rename(NU_CPF = cpf_novo)

#Importa a planilha com dados do Sivep/SRAG Hospitalizados
base_SRAG_bruta <- read_xlsx("SIVEP_2021.xlsx")

#Selecionar vari?veis de interesse
base_SRAG_select <- select(base_SRAG_bruta, NU_NOTIFIC, DT_SIN_PRI, NU_CPF, 
                           NM_PACIENT, DT_NASC,CS_GESTANT, PAC_COCBO,
                           NM_MAE_PAC, FATOR_RISC, PUERPERA, CARDIOPATI, 
                           HEMATOLOGI, SIND_DOWN, HEPATICA, ASMA, DIABETES, 
                           NEUROLOGIC, PNEUMOPATI, IMUNODEPRE, RENAL,
                           OBESIDADE, MORB_DESC)

base_sivep <- base_SRAG_select %>%
  rename(DN = DT_NASC) %>%
  mutate(DN = as.Date(DN, "%d/%m/%Y"))

#Fazer o join
base_join2 <- left_join(join1_df, base_sivep, by=c("NU_CPF","DN"))

#Limpa o ambiente de trabalho
rm(base_sivep, base_SRAG_bruta, base_SRAG_select, join1_df, cpf_novo, 
   base_join1, baseJoin_trat)


#Abrir banco de Vacina
vacina <- fread("Vacina_novo.csv")

#seleciona as variaveis de trabalho do banco de vacinas
vacina_select <- select(vacina, paciente_nome, paciente_cpf, 
                        paciente_dataNascimento, vacina_dataAplicacao,
                        vacina_nome, vacina_descricao_dose)

#Padroniza a variavel nome para o join
nome_padronizado <- rm_accent(vacina_select$paciente_nome)
nome_padronizado = stringr::str_trim(nome_padronizado)
nome_padronizado = toupper(nome_padronizado)
vacina_select <- cbind(vacina_select, nome_padronizado)
vacina_select$paciente_nome <- NULL
rm(nome_padronizado)
gc()

#Padroniza a variavel Data de Nascimeto do banco de vacinas
vacina_trat <- vacina_select %>%
  rename(DN = paciente_dataNascimento) %>%
  mutate(DN = as.Date(DN, "%d/%m/%Y"))

#Faz o join entre o banco anterior e o banco de vacinas
base_join3 <- left_join(base_join2, vacina_trat, by=c("nome_padronizado", "DN"))

#Limpar o ambiente de trabalho
rm(base_join1, base_join2, baseJoin_trat, vacina, vacina_select, vacina_trat)
gc()

#Transformando as idades
data_hj <- "29/09/2021"
base_teste <- cbind(base_join3, data_hj)
base_teste2 <- base_teste %>%
  mutate(data_hj = as.Date(data_hj, "%d/%m/%Y"))%>%
  mutate(idade_dias = difftime(data_hj, DN)) %>%
  mutate(idade = idade_dias/365) %>%
  mutate(idade_certo = as.integer(idade))

#Agrupar em faixa etaria
banco_df <- base_teste2 %>%
  mutate(faixa=ifelse(idade_certo<2, "Menor de 2 anos", NA),
         faixa=ifelse(idade_certo>=2&idade_certo<=10, "2 a 10", faixa),
         faixa=ifelse(idade_certo>=11&idade_certo<=19, "11 a 19", faixa),
         faixa=ifelse(idade_certo>=20&idade_certo<=29, "20 a 29", faixa),
         faixa=ifelse(idade_certo>=30&idade_certo<=39, "30 a 39", faixa),
         faixa=ifelse(idade_certo>=40&idade_certo<=49, "40 a 49", faixa),
         faixa=ifelse(idade_certo>=50&idade_certo<=59, "50 a 59", faixa),
         faixa=ifelse(idade_certo>=60&idade_certo<=69, "60 a 69", faixa),
         faixa=ifelse(idade_certo>=70&idade_certo<=79, "70 a 79", faixa),
         faixa=ifelse(idade_certo>=80, "80 ou mais", faixa))

#selecionando variaveis do banco final
banco_final <- select(banco_df, Amostra, nome_padronizado, NU_CPF, DN,
                      idade_certo, faixa, Sexo, estado, municipio, bairro,
                      nomeMae, cbo, Resultado, Plataforma, Coleta, 
                      numeroNotificacao, NU_NOTIFIC, dataInicioSintomas,
                      DT_SIN_PRI, FATOR_RISC, CS_GESTANT, PUERPERA, CARDIOPATI,
                      HEMATOLOGI, SIND_DOWN, HEPATICA, ASMA, DIABETES, 
                      NEUROLOGIC, PNEUMOPATI, IMUNODEPRE, RENAL, OBESIDADE,
                      MORB_DESC, evolucaoCaso, vacina_dataAplicacao,
                      vacina_nome, vacina_descricao_dose)

#Limpar o ambiente de trabalho
rm(banco_df, banco_teste3, base_join3, base_teste, base_teste2, data_hj)
gc()

#Faz as última alterações no banco final
delta <- banco_final %>% 
  mutate(sexo = ifelse(Sexo=="F", "Feminino", "Masculino")) %>%
  separate(Coleta, c("Cdia", "Cmes", "Cano"), sep = "/") %>%
  mutate(ano_col = as.numeric(Cano)) %>%
  mutate(ano_col = ifelse(ano_col<=21, ano_col+2000, NA)) %>%
  mutate(DataColeta = paste(Cdia, Cmes, ano_col, sep = "/")) %>%
  mutate(DataColeta = as.Date(DataColeta, "%d/%m/%Y")) %>%
  mutate(InicioSintomas_esus = as.Date(dataInicioSintomas, "%d/%m/%Y"))

#Padroniza as strings
UF <- rm_accent(delta$estado)
UF = stringr::str_trim(UF)
UF = toupper(UF)
delta <- cbind(delta, UF)

Municipio <- rm_accent(delta$municipio)
Municipio = stringr::str_trim(Municipio)
Municipio = toupper(Municipio)
delta <- cbind(delta, Municipio)

RA <- rm_accent(delta$bairro)
RA = stringr::str_trim(RA)
RA = toupper(RA)
delta <- cbind(delta, RA)

#Selciona o banco final final
delta_final <- select(delta, Amostra, nome_padronizado, NU_CPF, DN,
                      idade_certo, faixa, sexo, UF, Municipio, RA,
                      nomeMae, cbo, Resultado, Plataforma, DataColeta, 
                      numeroNotificacao, NU_NOTIFIC, InicioSintomas_esus,
                      DT_SIN_PRI, FATOR_RISC, CS_GESTANT, PUERPERA, CARDIOPATI,
                      HEMATOLOGI, SIND_DOWN, HEPATICA, ASMA, DIABETES, 
                      NEUROLOGIC, PNEUMOPATI, IMUNODEPRE, RENAL, OBESIDADE,
                      MORB_DESC, evolucaoCaso, vacina_dataAplicacao,
                      vacina_nome, vacina_descricao_dose)


#Exporta o arquivo
write.csv(delta_final, "MonitoramentoDelta_290921.csv")

rm(banco_final, delta)
rm(Municipio, RA, UF)





