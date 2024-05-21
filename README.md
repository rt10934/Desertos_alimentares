# PROJETO DE COOPERAÇÃO TÉCNICA PARA A SEGURANÇA ALIMENTAR E NUTRICIONAL: A DISPONIBILIDADE E O ACESSO À ALIMENTOS SAUDÁVEIS E O COMBATE À POBREZA RURAL - IICA/BRA/17/001

## **Produto 4**: Classificação dos estabelecimentos alimentares nos grupos do guia alimentar para a população brasileira

Para a obtenção dos resultados desta parte do produto, foram realizadas três etapas. Na primeira etapa, foram coletados os dados. Na segunda etapa, houve a junção dos dados coletados com os dicionários de alimentos por grupo do guia alimentar para a população brasileira, assim como com os dicionários de locais de compra pela Classificação Nacional das Atividades Econômicas (CNAE). Na terceira e última etapa, os dados foram processados para obter as tabelas inseridas no texto. Cada uma dessas etapas será descrita de maneira mais detalhada abaixo.

### Etapa 1: abertura dos microdados (R)

São gerados nessa etapa dados referentes a processamentos realizados na etapa 2 e na etapa 3, em que são gerados as tabelas "junta_ali" usada na etapa 2 para realizar o merge com os dicionarios, e também é gerado a tabela "morador_uc" usado na etapa 3.

### Etapa 2: inclusão dos dicionarios dos alimentos por grupo do Guia Alimentar para a População Brasileira, locais de compra pelo CNAE (Stata)

Nesta etapa, é realizado o mrge entre a tabela confeccionada na etapa 1 (junta_ali), com os dicionários dos alimentos por grupo do Guia Alimentar para a População Brasileira "dicionario_alimentos", locais de compra pelo CNAE "dicionario_locais"

### Etapa 3 (tabelas do Produto 4(R))

##### **OBS: alterar nomes conforme publicacao final**

#### Tabelas presentes no "script_1"

No script_1 se encontram as seguintes tabelas do produto 4:

-   **Tabela 3** Quantidade de itens e percentual de aquisição de alimentos pela população por grupo do Guia Alimentar para a População Brasileira. Deserto Alimentar de 2018 e POF 2017/2018**("t3_completa")**.

**OBS:** As tabelas "t3_Rural", "t3_Urbano", "t3_Primeiro_sm", "t3_Segundo_sm", "t3_Terceiro_sm" e "t3_Quarto_sm", são semelhantes a tabela anterior, porém filtradas para os grupos de renda e localização (não incorporadas ao texto)

-   **Tabela 10** Quantidade de itens e percentual de aquisição de alimentos pela população por grupo do Guia Alimentar para a População Brasileira POF 2017/2018 para as localizações**("tabela_10")**.

-   **Tabela 15** Quantidade de itens e percentual de aquisição de alimentos pela população por grupo do Guia Alimentar para a População Brasileira POF 2017/2018 para os níveis de renda.

#### No script_2 estão presentes as tabelas:

-   **Tabela 4.** Milhões de itens de aquisição de alimentos pela população por grupo do Guia Alimentar para a População Brasileira e local de compra da POF 2017/2018 **("t4_completa")**

**OBS:** As tabelas "t4_Rural", "t4_Urbano", "t4_Primeiro_sm", "t4_Segundo_sm", "t4_Terceiro_sm" e "t4_Quarto_sm", são semelhantes a tabela anterior, porém filtradas para os grupos de renda e localização (não incorporadas ao texto)

-   **Tabela 5.** Milhões de itens de aquisição de alimentos pela população por grupo do Guia Alimentar para a População Brasileira e local de compra da POF 2017/2018 e o percentual proveniente de aquisição de cada local de compra por grupo alimentar **("t10_completa")**

**OBS:** As tabelas "t10_Rural", "t10_Urbano", "t10_Primeiro_sm", "t10_Segundo_sm", "t10_Terceiro_sm" e "t10_Quarto_sm", são semelhantes a tabela anterior, porém filtradas para os grupos de renda e localização (não incorporadas ao texto)

#### No script_3 estão presentes as tabelas:

Tabelas de Milhões de itens de aquisição de alimentos pela população por grupo do Guia Alimentar para a População Brasileira e estabelecimento de alimentação (CNAE):

-   **Tabela 6.** Milhões de itens de aquisição de alimentos pela população por grupo do Guia Alimentar para a População Brasileira e estabelecimento de alimentação (CNAE) **("t8_completa")**

-   **Tabela 11.** Milhões de itens de aquisição de alimentos pela população por grupo do Guia Alimentar para a População Brasileira e estabelecimento de alimentação (CNAE) para o meio rural **("t8_Rural")**

-   **Tabela 13.** Milhões de itens de aquisição de alimentos pela população por grupo do Guia Alimentar para a População Brasileira e estabelecimento de alimentação (CNAE) para o meio urbano **("t8_Urbano")**

-   **Tabela 16.** Milhões de itens de aquisição de alimentos pela população por grupo do Guia Alimentar para a População Brasileira e estabelecimento de alimentação (CNAE) para o primeiro quartil **("t8_Primeiro_sm")**

-   **Tabela 18.** Milhões de itens de aquisição de alimentos pela população por grupo do Guia Alimentar para a População Brasileira e estabelecimento de alimentação (CNAE) para o segundo quartil **("t8_Segundo_sm")**

-   **Tabela 20.** Milhões de itens de aquisição de alimentos pela população por grupo do Guia Alimentar para a População Brasileira e estabelecimento de alimentação (CNAE) para o terceiro quartil **("t8_Terceiro_sm")**

-   **Tabela 22.** Milhões de itens de aquisição de alimentos pela população por grupo do Guia Alimentar para a População Brasileira e estabelecimento de alimentação (CNAE) para o quarto quartil **("t8_Quarto_sm")**

Tabelas de Milhões de itens e percentual de aquisição de alimentos pela população por grupo do Guia Alimentar para a População Brasileira e estabelecimento de alimentação (CNAE) e o percentual proveniente de aquisição de cada estabelecimento de compra por grupo alimentar:

-   **Tabela 7.** Milhões de itens e percentual de aquisição de alimentos pela população por grupo do Guia Alimentar para a População Brasileira e estabelecimento de alimentação (CNAE) e o percentual proveniente de aquisição de cada estabelecimento de compra por grupo alimentar **("t9_completa")**

-   **Tabela 12.** Milhões de itens de aquisição de alimentos pela população por grupo do Guia Alimentar para a População Brasileira e estabelecimento de alimentação (CNAE) e o percentual proveniente de aquisição de cada estabelecimento de compra por grupo alimentar para o meio rural **("t9_Rural")**

-   **Tabela 14.** Milhões de itens de aquisição de alimentos pela população por grupo do Guia Alimentar para a População Brasileira e estabelecimento de alimentação (CNAE) o percentual proveniente de aquisição de cada estabelecimento de compra por grupo alimentar para o meio urbano **("t9_Urbano")**

-   **Tabela 17.** Milhões de itens de aquisição de alimentos pela população por grupo do Guia Alimentar para a População Brasileira e estabelecimento de alimentação (CNAE) o percentual proveniente de aquisição de cada estabelecimento de compra por grupo alimentar para o primeiro quartil **("t9_Primeiro_sm")**

-   **Tabela 19.** Milhões de itens de aquisição de alimentos pela população por grupo do Guia Alimentar para a População Brasileira e estabelecimento de alimentação (CNAE) o percentual proveniente de aquisição de cada estabelecimento de compra por grupo alimentar para o segundo quartil **("t9_Segundo_sm")**

-   **Tabela 21.** Milhões de itens de aquisição de alimentos pela população por grupo do Guia Alimentar para a População Brasileira e estabelecimento de alimentação (CNAE) o percentual proveniente de aquisição de cada estabelecimento de compra por grupo alimentar para o terceiro quartil **("t9_Terceiro_sm")**

-   **Tabela 23.** Milhões de itens de aquisição de alimentos pela população por grupo do Guia Alimentar para a População Brasileira e estabelecimento de alimentação (CNAE) o percentual proveniente de aquisição de cada estabelecimento de compra por grupo alimentar para o quarto quartil **("t9_Quarto_sm")**

#### O script_4 apresenta os perfis de estabelecimento por UF:

Foi realizado em duas etapas: Na primeira etapa, realizada no R e alocada no 'script_4', é feita a junção dos dados. Na segunda etapa, feita no Excel ('nome.xls'), são classificados os perfis de estabelecimento por UF.
