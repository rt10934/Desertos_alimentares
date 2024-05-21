/* Utilização da base de dados da POF 2017/2018 para a classificação de 
estabelecimentos de alimentação conforme o Guia Alimentar para a População
Brasileira
OBS: É necessário substituir o "*diretório" pelo diretório onde estão as pastas dos dados


[1] Consolidação da base de alimentação com os dicionários para produção 
	Importação de dicionários de classificação:
	Locais de compra POF para códigos da Classificação Nacional de Atividades
	Econômicas (CNAE)
	Classificação de itens de alimentação conforme o Guia Alimentar*/


* [1] Importação de dicionários de classificação
clear all

/*Importar base de dados de despesa individual e caderneta coletiva
substituindo o <caminho> pelo local dos arquivos*/
import delimited "*diretório\Etapa_2\junta_ali.csv"

* Unificar o código de alimentos (v9001) com o dicionário de classificação do Guia Alimentar
* Substituir <caminho> pelo local desejado para salvar o arquivo
merge m:1 v9001 using "*diretório\Etapa_2\dicionario_alimentos.dta", nogen

* Unificar o código de local de compra POF (v9004) com o dicionário de classificação CNAE
* Substituir <caminho> pelo local desejado para salvar o arquivo
merge m:1 v9004 using "*diretório\Etapa_2\dicionario_locais.dta", nogen

* Salvar a base de alimentação com os dicionários
* Substituir <caminho> pelo local desejado para salvar o arquivo
save "*diretório\Etapa_3\pof_compras.dta", replace

clear all
* Substituir <caminho> pelo local onde foi salvo o arquivo anterior.
use "*diretório\Etapa_3\pof_compras.dta"

*Variável para número de itens de alimentação
gen contagem=1

*Somando contagem e valor por chave unica 
collapse(sum) contagem valor_mensal, by(uf estrato_pof tipo_situacao_reg cod_upa num_dom num_uc renda_total classe_prod cod_local nome_v9001 v9004 nome_local desc_local)

tostring uf estrato_pof tipo_situacao_reg cod_upa num_dom num_uc, replace
gen key_morador=uf+estrato_pof+tipo_situacao_reg+cod_upa+num_dom+num_uc

save "*diretório\Etapa_3\pof_compras_morador", replace

/*					JUNTANDO POF DATA COM MORADOR DATA
*/

clear all
cd "*diretório\Etapa_3\"
use morador_uc.dta
tostring uf estrato_pof tipo_situacao_reg cod_upa num_dom num_uc, replace
gen key_morador=uf+estrato_pof+tipo_situacao_reg+cod_upa+num_dom+num_uc
save morador_uc_key.dta, replace

clear
use "*diretório\pof_compras_morador.dta"

cd "*diretório\Etapa_3\"
merge m:1 key_morador using morador_uc_key.dta
*4.486 sem match, peso equivalente a 5.375.796

drop v1 _merge
save pof_morador.dta, replace
















