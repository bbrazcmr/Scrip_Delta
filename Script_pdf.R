#Carregar os pacotes 'pdftools' e 'tidyverse'
library(pdftools)
library(tidyverse)

#Ler o texto do PDF e transformar em base de dados
texto_bruto <- pdf_text("SEI_GDF - 69100691 - Relatório Técnico.pdf") %>%
  strsplit("\n") %>%
  unlist() %>%
  enframe(name = NULL, value = "Linha")

#Limpar os dados do PDF
texto_limpo <- texto_bruto %>%
  tail(-55) %>% 
  head(-100)
#aqui deverá conter o código de limpeza por expressões regurales

#Limpeza manual
texto_final_limpo <- texto_limpo %>% 
  slice(1, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39,
        41, 43, 45, 47, 49, 51, 53, 55, 57, 59, 61, 66, 68, 70, 72, 74, 76,
        78, 80, 82, 84, 86, 88, 90, 92, 94, 96, 98, 100, 102, 104, 106, 108,
        110, 112, 114, 116, 118, 120, 125, 127, 129, 131, 133, 135, 137, 139,
        141, 143, 145, 147, 149, 151, 153, 155, 157, 159, 161, 163, 165, 167,
        169, 171, 173, 175, 177, 179, 184, 186, 188, 190, 192, 194)

#Separar as variáveis
base_pdf <- texto_final_limpo %>%
  separate(Linha, c("Vazio", "Id", "Amostra","SES", 
                    "Coleta_dia", "Coleta_mês", "Coleta_ano", 
                    "DN_dia", "DN_mês", "DN_ano", 
                    "DL_dia", "DL_mês", "DL_ano", 
                    "Resultado", "Plataforma"))

#Excluir a Coluna "Vazio"
base_pdf$Vazio <- NULL

#Arrumar as variáveis de Datas
base_pdf_final <- base_pdf %>%
  mutate(Coleta=paste(Coleta_dia, Coleta_mês, Coleta_ano, sep = "/")) %>%
  mutate(Data_Nascimento=paste(DN_dia,DN_mês,DN_ano, sep = "/")) %>%
  mutate(Data_Liberacao=paste(DL_dia,DL_mês,DL_ano, sep = "/"))

#Excluir variáveis extras de datas
base_pdf_final$Coleta_dia <- NULL
base_pdf_final$Coleta_mês <- NULL
base_pdf_final$Coleta_ano <- NULL
base_pdf_final$DN_dia <- NULL
base_pdf_final$DN_mês <- NULL
base_pdf_final$DN_ano <- NULL
base_pdf_final$DL_dia <- NULL
base_pdf_final$DL_mês <- NULL
base_pdf_final$DL_ano <- NULL

#Importar a planilha com dados do Track Care
install.packages("readxl")
library(readxl)
base_track_bruta <- read_xlsx("LACEN ORIGINAL1.xlsx")

#Preparar a base para o merge
base_track_bruta$PEP <- NULL
base_track_bruta$`Data de Coleta`<- NULL
base_track_bruta$`Data de Nascimento` <- NULL
base_track_bruta$`Local da Solicitação` <- NULL
base_track_bruta$`Data da Liberação` <- NULL
base_track_bruta$`Item de Exame` <- NULL
base_track_bruta$Resultado <- NULL
base_track_bruta$`Recebimento Setor` <- NULL

base_track <- unique(base_track_bruta)

#Fazer o merge entre a base_pdf_final e a base_track pelo nº da Amosta
base1 <- merge(base_pdf_final, base_track, by = 'Amostra')
base1_ord <- base1 %>%
  arrange(Id)

#Importar planilha com dados do Sivep/SRAG Hospitalizados
base_SRAG_bruta <- read_xlsx("SRAGHOSPITALIZADO_2021.xlsx")

#Selecionar variáveis de interesse
base_SRAG_select <- select(base_SRAG_bruta, NU_NOTIFIC, DT_SIN_PRI, SG_UF_NOT,
                    CO_MUN_NOT, NU_CPF, NM_PACIENT, CS_GESTANT, PAC_COCBO,
                    NM_MAE_PAC, FATOR_RISC, PUERPERA, CARDIOPATI, HEMATOLOGI,
                    SIND_DOWN, HEPATICA, ASMA, DIABETES, NEUROLOGIC, PNEUMOPATI,
                    IMUNODEPRE, RENAL, OBESIDADE, MORB_DESC)

base_SRAG <- base_SRAG_select %>%
  rename(Nome = NM_PACIENT)

#Fazer o merge entre a base_SRAG e base1_ord pela variável 'Nome'
base2 <- merge(base1_ord, base_SRAG, by = 'Nome')

  

