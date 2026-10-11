# Relatório de Auditoria — Ciclo 9: datapresentation

**Módulo Auditado:** `datapresentation` (`R/datapresentation.b.R`, `jamovi/datapresentation.a.yaml`, `jamovi/datapresentation.r.yaml`, `jamovi/datapresentation.u.yaml`)  
**Data da Auditoria:** 10/10/2026  
**Auditor Responsável:** Antigravity AI / T3 Code  
**Status da Auditoria:** DIAGNÓSTICO CONCLUÍDO — AGUARDANDO AUTORIZAÇÃO DO USUÁRIO

---

## 1. Contexto e Escopo do Módulo
O módulo `datapresentation` é a central de visualização e tabelamento descritivo do pacote BSS. Ele suporta:
1. **Delineamentos Transversais (Cross-Sectional / Independent Groups):** Análise de múltiplas variáveis dependentes contínuas por fatores de agrupamento, com suporte a ANCOVA via covariáveis ajustadas (`emmeans`).
2. **Delineamentos Longitudinais (Repeated Measures):** Análise de dados pareados/repetidos em formato largo (*wide*), permitindo fatores entre-sujeitos e covariáveis.
3. **Formatação Normativa de Tabelas:** APA (7ª ed.), ABNT (NBR 14724), Vancouver e Chicago.
4. **Gráficos de Alta Qualidade:** Gráficos de barras, boxplots e médias com dispersão (*Mean & Dispersion*), paletas de cores e customizações de eixos e tipografia.

---

## 2. Diagnóstico Científico, Estatístico e Computacional

### 2.1. Coerção Robusta de Tipos em Variáveis Dependentes (`BSS-STAT-033`)
- **Problema:** Quando variáveis contínuas são lidas como caracteres ou fatores (ex. importação de CSV com separadores decimais vírgula/ponto), `mean(data[[dep]])` no R gera o alerta `warning: argument is not numeric or logical: returning NA`, corrompendo toda a tabela descritiva e os gráficos.
- **Solução:** Aplicar a função defensiva `.to_numeric_vec()` sistematicamente a todas as variáveis dependentes (`deps`) e variáveis de medidas repetidas (`rm_vars`).

### 2.2. Resiliência do Pipeline com Covariáveis em Medidas Repetidas (`BSS-STAT-034`)
- **Problema:** No delineamento longitudinal com covariável, quando não há fatores entre-sujeitos (`length(group_long) == 0`), a especificação da fórmula e o agrupamento no `emmeans` requerem manipulação especial para manter a resposta multivariada (`multivariate_response`). Em alguns casos de dados incompletos, o merge com `raw_summary` pode gerar linhas `NA` ou falhas de ordenação.
- **Solução:** Estruturar fallback defensivo com checagem de integridade e ordenação determinística dos fatores de tempo.

### 2.3. Integridade e Limpeza de Colunas Dinâmicas na Tabela Descritiva (`BSS-CODE-013`)
- **Problema:** Ao alternar dinamicamente entre delineamentos (Cross-Sectional vs Longitudinal) ou ao alterar o número de fatores de agrupamento, a sincronização de colunas via `table$deleteColumn()` e `table$addColumn()` pode deixar colunas órfãs se o usuário alternar opções repetidamente.
- **Solução:** Garantir uma rotina unificada e determinística de redefinição de colunas antes da população das linhas.

### 2.4. Resiliência da Paleta Manual de Cores no ggplot2 (`BSS-CODE-014`)
- **Problema:** Em `.resolveColors()`, se o usuário selecionar a paleta `"manual"`, mas não definir cores nos campos `color1` a `color8` (todos mantidos como `"none"`), a lista `mc` fica vazia (`length(mc) == 0`), e a função cai em um caminho sem `return` explícito, dependendo de comportamentos imprevisíveis de retorno de lista em R.
- **Solução:** Adicionar fallback explícito para a paleta padrão (`set1`) quando nenhuma cor manual for especificada.

### 2.5. Cadastro Canônico das Normas Acadêmicas no Repositório Central (`BSS-CODE-015`)
- **Problema:** As normas de estilo suportadas (APA 7th, ABNT NBR 14724, Vancouver, Chicago) não estão formalizadas em `jamovi/00refs.yaml` nem nos metadados YAML.
- **Solução:** Cadastrar `APA2020`, `ABNT2011`, `ICMJE2023` e `Chicago2017` em `jamovi/00refs.yaml` e vincular referências.

---

## 3. Tickets de Correção Propostos

| ID do Ticket | Tipo | Severidade | Descrição Resumida |
| :--- | :--- | :--- | :--- |
| **BSS-STAT-033** | Estatística / Dados | Alta | Coerção defensiva de variáveis contínuas via `.to_numeric_vec()` prevenindo falhas de agregação e estatísticas corrompidas. |
| **BSS-STAT-034** | Estatística / Modelo | Média | Resiliência do pipeline longitudinal com covariáveis ajustadas (`emmeans`) para modelos puramente intra-sujeitos e mistos. |
| **BSS-CODE-013** | Código | Média | Sincronização segura e determinística das colunas dinâmicas de agrupamento na tabela descritiva. |
| **BSS-CODE-014** | Código | Baixa | Fallback defensivo transparente para seleção manual de cores vazia em `.resolveColors()`. |
| **BSS-CODE-015** | Documentação | Baixa | Formalização bibliográfica das diretrizes normativas (`APA2020`, `ABNT2011`, `ICMJE2023`, `Chicago2017`) em `jamovi/00refs.yaml`. |

---

## 4. Solicitação de Autorização
Nenhum arquivo de código foi modificado nesta etapa. Aguardando autorização explícita do usuário para implementar os tickets do Ciclo 9 (`datapresentation`).
