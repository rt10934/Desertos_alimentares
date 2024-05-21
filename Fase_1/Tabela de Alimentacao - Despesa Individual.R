#-------------------------------------------------------------------------------
# POF 2017-2018 - PROGRAMA PARA GERAÇÃO DAS ESTIMATIVAS PONTUAIS DA TABELA DE 
# ALIMENTACAO NÍVEL GEOGRÁFICO - BRASIL 
#-------------------------------------------------------------------------------
# É preciso executar antes o arquivo "Leitura dos Microdados - R.R"
# que se encontra no arquivo compactado "Programas_de_Leitura.zip"
# Este passo é necessário para gerar os arquivos com a extensão .rds
# correspondentes aos arquivos com extensão .txt dos microdados da POF

# "....." indica a pasta/diretório de trabalho no HD local separados por "/"
# onde se encontram os arquivos .txt descompactados do arquivo Dados_aaaammdd.zip
# Exemplo: setwd("c:/POF2018/Dados_aaaammdd/")

# Declarar o "diretório" da pasta referente a essa etapa (Etapa_1)
setwd("diretório/Etapa_1") 

# Leitura do REGISTRO - DESPESA INDIVIDUAL (Questionário POF 4)
despesa_individual <- readRDS("DESPESA_INDIVIDUAL.rds") # importando base necessária:

# [1] Transformação do código do item (variável V9001) em 5 números, para ficar 
# no mesmo padrão dos códigos que constam nos arquivos de tradutores das tabelas.
# Esses códigos são simplificados em 5 números, pois os 2 últimos números 
# caracterizam sinônimos ou termos regionais do produto. Todos os resultados da 
# pesquisa são trabalhados com os códigos considerando os 5 primeiros números.

# [2] Seleção dos itens do REGISTRO - DESPESA INDIVIDUAL (POF 4) que entram na 
# tabela de alimentação (todos do quadro 24 e códigos 41001, 48018, 49075, 49089).   

# [3] Anualização e expansão dos valores utilizados para a obtenção dos resultados
# (variável V8000_defla). 
#     a) Para anualizar, utilizamos o quesito "fator_anualizacao". No caso 
#        específico dos quadros 48 e 49, cujas informações se referem a valores 
#        mensais, utilizamos também o quesito V9011 (número de meses). Os valores
#        são anualizados para depois se obter uma média mensal.

#     b) Para expandir, utilizamos o quesito "peso_final".
#     c) Posteriormente, o resultado é dividido por 12 para obter a estimativa 
#        mensal.

# código com os passos:
desp_individual <- 
  subset( transform( despesa_individual,
                     codigo = trunc(V9001/100) # [1]
                     ),
          QUADRO==24|codigo==41001|codigo==48018|codigo==49075|codigo==49089
          )# [2]

desp_individual <-
  transform( desp_individual,
             valor_mensal = ifelse( QUADRO==24|QUADRO==41,
                                    (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12, 
                                    (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12
                                    ) # [3] 
             )[ , c( "UF" , "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM",
                     "NUM_UC", "RENDA_TOTAL" , "V9004" , "V9001" , "valor_mensal", "codigo" ) ]
rm(despesa_individual)

despesa_individual <- aggregate(valor_mensal~UF,data=desp_individual,sum)
names(despesa_individual) <- c("soma")


write.csv(desp_individual, file = "Diretório/Etapa_2/desp_individual.csv") # "$$$$ perguntar giovani"
# colocar o "diretório" da pasta relacionada a armazenamento a etapa 2