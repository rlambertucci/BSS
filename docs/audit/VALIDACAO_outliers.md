# Relatório de Validação Independente — Ciclo 7: outliers

**Módulo Auditado:** `outliers` (`R/outliers.b.R`, `jamovi/outliers.a.yaml`, `jamovi/outliers.r.yaml`, `jamovi/00refs.yaml`)  
**Data da Validação:** 10/10/2026  
**Ambiente:** R 4.5.2, `jmvcore`, `R6`, Windows x64  
**Script Executado:** `tests/validation/test_outliers_fixes.R`  
**Status Global:** **APROVADO (100% de conformidade)**

---

## 1. Sumário Executivo das Correções Implementadas

| Ticket | Classificação | Natureza | Descrição / Solução Implementada | Status de Validação |
| :--- | :--- | :--- | :--- | :--- |
| **BSS-CODE-009** | Crítica | Crash de Execução | Correção do erro fatal de namespace `stats::mean` para `mean` na linha 408 quando $MAD = 0$, evitando interrupção imediata da análise no jamovi. | **VALIDADO** (Aprovado em runtime) |
| **BSS-STAT-027** | Alta | Integridade de Dados | Implementação do helper privado `.to_numeric_vec()` que realiza `as.numeric(as.character(x))` para colunas de fatores, prevenindo corrupção silenciosa onde códigos inteiros de nível eram usados como dados contínuos. | **VALIDADO** (Aprovado em runtime) |
| **BSS-STAT-028** | Alta | Integridade de Rastreio | Indexação determinística de observações via `.row_id = seq_len(nrow(self$data))` antes de `split()`, garantindo identificação correta da linha original no dataset mesmo quando há `rownames` alfanuméricos (`Patient_ID_101`). | **VALIDADO** (Aprovado em runtime) |
| **BSS-STAT-029** | Média | Robustez Estatística | Defesa contra variância zero no teste de Dixon ($range = 0$): prevenção de divisão por zero ($0/0 \to \text{NaN}$) e registro claro de `"All values identical"` em vez de falha silenciosa ou crash. | **VALIDADO** (Aprovado em runtime) |
| **BSS-CODE-010** | Baixa | Documentação / Citações | Formalização de referências bibliográficas canônicas (`Dixon1950`, `Rorabacher1991`, `Iglewicz1993`, `Leys2013`, `Wilcox2017`, `Tukey1977`, `Hoaglin1987`) em `00refs.yaml` e inserção transparente nas notas de rodapé das tabelas e painéis explicativos. | **VALIDADO** (Conformidade bibliográfica) |

---

## 2. Evidências dos Testes de Execução

### Teste 1: BSS-CODE-009 (Modified Z-Score com MAD = 0)
- **Cenário:** Conjunto de dados com dispersão absoluta nula em torno da mediana (`c(2, 2, 2, 2, 2, 2, 20)`).
- **Comportamento Anterior:** Chamava `stats::mean` no cálculo do desvio médio alternativo e falhava com `Error: 'mean' is not an exported object from 'namespace:stats'`.
- **Resultado Atual:**
  ```
  variable group_level lowerThreshold upperThreshold numOutliers
         v    All Data      -11.34322       15.34322           1
  
  variable group_level rowNumber modifiedZ
         v    All Data         7    4.7215
  ```
- **Conclusão:** Módulo calculou o desvio padrão e identificou com precisão a linha 7 ($Z^* = 4.7215$). Aprovado sem warnings ou erros.

### Teste 2: BSS-STAT-027 (Coerção Segura de Fatores)
- **Cenário:** Vetor `factor(c("10", "12", "11", "13", "12", "11", "100"))`.
- **Comportamento Anterior:** `as.numeric()` retornava `c(1, 3, 2, 4, 3, 2, 5)`, descaracterizando totalmente o valor 100.
- **Resultado Atual:**
  - Valores extraídos via `.to_numeric_vec()`: `c(10, 12, 11, 13, 12, 11, 100)`.
  - Detecção via IQR:
  ```
  variable group_level lowerFence upperFence numOutliers
         v    All Data       8.75      14.75           1
  ```
- **Conclusão:** Limite superior estabelecido em 14.75 e outlier real 100 devidamente detectado.

### Teste 3: BSS-STAT-028 (Rastreamento de Linhas com Rownames Alfanuméricos)
- **Cenário:** Data frame agrupado com `rownames` como `Patient_ID_101` a `Patient_ID_105`, onde o paciente 105 tem valor 30 (outlier pelo teste de Dixon).
- **Comportamento Anterior:** Tentava converter `as.integer(names(x))` ou `rownames(subset)`, gerando `NA` ou falhas de atribuição na tabela de saída.
- **Resultado Atual:**
  ```
  variable group_level rowNumber outlier_value outlier_side q_statistic
       val          G1         5            30        Upper         0.9
  ```
- **Conclusão:** Linha 5 mapeada com integridade exata correspondente à posição física no dataset original.

### Teste 4: BSS-STAT-029 (Identical Values / Zero Variance no Dixon Q)
- **Cenário:** Dataset com valores perfeitamente idênticos `c(5, 5, 5, 5, 5)`.
- **Resultado Atual:**
  ```
  variable group_level n outlier_value outlier_side q_statistic q_critical significant
       val    All Data 5            NA            —          NA         NA All values identical
  ```
- **Conclusão:** Tabela preenchida de forma elegante sem erros matemáticos de indefinição.

---

## 3. Conclusão da Validação
O módulo `outliers` encontra-se matematicamente robusto, imune a erros de namespace e coerção de tipos, e rigorosamente documentado de acordo com a literatura de estatística robusta e química analítica. Pronto para consolidação em commit.
