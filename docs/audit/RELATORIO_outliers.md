# Relatório de Auditoria — Ciclo 7: `outliers`

**Data:** 2026-10-10  
**Módulo:** `outliers` (`R/outliers.b.R`, `jamovi/outliers.*.yaml`)  
**Autor do Pacote:** Rafael Lambertucci  
**Auditor:** Antigravity (Auditoria Estatística e Computacional R/jamovi)  
**Status:** Diagnóstico concluído — Aguardando autorização do usuário  

---

## 1. Visão Geral do Módulo

O módulo `outliers` fornece três métodos estatísticos para detecção e tratamento de valores atípicos:
1. **Dixon's Q Test (Dixon 1950, Rorabacher 1991):** Para amostras pequenas ($3 \le N \le 30$).
2. **Modified Z-Score (Iglewicz & Hoaglin 1993, Leys et al. 2013, Wilcox 2017):** Baseado na mediana e MAD, com limiares ajustáveis (2.5, 3.0, 3.5, 4.0).
3. **IQR / Tukey's Fences (Tukey 1977, Hoaglin & Iglewicz 1987):** Baseado no intervalo interquartil (1.5, 2.2, 3.0 $\times$ IQR).
4. **Tratamento de Outliers:** Trimming assimétrico, Trimming simétrico percentual, Winsorização para valores válidos extremos e Winsorização para limites dos fences.
5. **Visualização:** Boxplots anotados com realce e numeração de linha dos outliers via `ggplot2`.

---

## 2. Diagnóstico Detalhado e Fundamentação Científica

### BSS-CODE-009: Erro Fatal de Namespace (`stats::mean`)
- **Problema:** Na linha 408 de `R/outliers.b.R`, ao lidar com $MAD = 0$ (comum quando mais de 50% dos dados coincidem com a mediana), o código chamava `stats::mean(abs(data_var - med), na.rm = TRUE)`. No R, `mean` pertence ao pacote `base` (`base::mean`), e não existe em `stats`. A chamada resultava em:
  `Error: 'mean' is not an exported object from 'namespace:stats'`
  abortando e quebrando a execução do módulo.
- **Correção:** Substituir por `mean(abs(data_var - med), na.rm = TRUE)`.

### BSS-STAT-027: Coerção Indevida de Fatores com `as.numeric()`
- **Problema:** Colunas numéricas importadas como fatores eram convertidas via `as.numeric(data[[var]])`. No R, aplicar `as.numeric` a um fator extrai os códigos inteiros dos níveis ($1, 2, 3, \dots$) e não os valores reais ($10, 20, 30$). Todos os cálculos de outliers eram executados sobre os índices dos níveis, deturpando completamente os diagnósticos e gerando dados tratados corrompidos.
- **Correção:** Integrar e aplicar `.to_numeric_vec()` em todas as etapas de ingestão de dados.

### BSS-STAT-028: Rastreamento Robusto de Linhas Originais
- **Problema:** A indexação de linhas para a tabela de outliers identificados e para os gráficos dependia de `as.numeric(rownames(data_group))`. Em bases com rownames alfanuméricos, isso gerava `NA`, impossibilitando o usuário de localizar a observação atípica no dataset.
- **Correção:** Criar uma coluna explícita de rastreamento `.row_id <- seq_len(nrow(self$data))` antes de qualquer operação de `split()`, garantindo que o número da linha original seja sempre um inteiro exato.

### BSS-STAT-029: Tratamento de Variância Nula e Limpeza Defensiva de Tabelas
- **Problema:** Dados com variância nula nos métodos Modified Z e IQR podiam gerar divisões por zero ou limiares colapsados. A limpeza de tabelas usava `try(deleteRows(), silent = TRUE)` sem verificar `rowCount`.
- **Correção:** Proteção contra variância zero retornando avisos informativos claros e adoção de `if (table$rowCount > 0) table$deleteRows()`.

### BSS-CODE-010: Metadados Bibliográficos e Citações Científicas
- **Problema:** As referências metodológicas fundamentais citadas nos textos da interface não estavam cadastradas em `jamovi/00refs.yaml` nem vinculadas nos YAMLs de análise.
- **Correção:** Cadastrar em `00refs.yaml`:
  - `Dixon1950` (Dixon, W. J., 1950)
  - `Rorabacher1991` (Rorabacher, D. B., 1991)
  - `Iglewicz1993` (Iglewicz, B., & Hoaglin, D. C., 1993)
  - `Leys2013` (Leys, C., et al., 2013)
  - `Wilcox2017` (Wilcox, R. R., 2017)
  - `Tukey1977` (Tukey, J. W., 1977)
  - `Hoaglin1987` (Hoaglin, D. C., & Iglewicz, B., 1987)
  E associar aos elementos de saída em `outliers.a.yaml` e `outliers.r.yaml`.

---

## 3. Matriz de Proposta de Tickets

| Ticket | Escopo | Descrição |
|---|---|---|
| `BSS-CODE-009` | `R/outliers.b.R` | Correção do erro fatal de namespace `stats::mean` para `mean` no cálculo alternativo quando MAD = 0. |
| `BSS-STAT-027` | `R/outliers.b.R` | Coerção numérica estrita de fatores via `.to_numeric_vec()` para evitar corrupção de dados via códigos ordinais. |
| `BSS-STAT-028` | `R/outliers.b.R` | Rastreamento independente de linhas com `.row_id` imune a character rownames. |
| `BSS-STAT-029` | `R/outliers.b.R` | Defesa contra variância zero e limpeza defensiva de tabelas. |
| `BSS-CODE-010` | `jamovi/00refs.yaml`, `jamovi/outliers.*.yaml` | Cadastro formal de Dixon, Rorabacher, Iglewicz, Leys, Wilcox, Tukey e Hoaglin e vinculação às tabelas. |
