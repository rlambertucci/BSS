# Relatório de Auditoria Independente: Ciclo 4 — `art` (Aligned Rank Transform)

**Data da Auditoria:** 2026-10-10  
**Módulo Avaliado:** `art` (`R/art.b.R`, `jamovi/art.a.yaml`, `jamovi/art.r.yaml`, `jamovi/00refs.yaml`)  
**Status do Ciclo:** DIAGNÓSTICO CONCLUÍDO — AGUARDANDO AUTORIZAÇÃO DO USUÁRIO  
**Branch de Auditoria:** `refactor/audit-v0.3.0`  

---

## 1. Contexto Científico e Metodológico

O **Aligned Rank Transform (ART)** (*Wobbrock et al., 2011*) é um método não-paramétrico fatorial concebido para permitir a análise de variância de dados multivariados e multifatoriais (tanto entre sujeitos quanto com medidas repetidas/misto) sem exigir suposições de normalidade ou esfericidade. O método alinha os dados residualmente para cada efeito principal e de interação antes de aplicar a ordenação por postos (*ranking*), possibilitando testes $F$ legítimos para interações. Para testes post hoc multifatoriais, utiliza o procedimento **ART-C** (*Elkin et al., 2021*) implementado no pacote `ARTool` em combinação com `emmeans`.

O módulo `art` do BSS implementa:
1. ANOVA não-paramétrica para desenhos independentes (*cross-sectional*).
2. Modelos lineares mistos não-paramétricos com efeitos aleatórios de sujeito (*longitudinal*).
3. Suporte a formatos de dados *Wide* (medidas repetidas em colunas de ondas) e *Long*.
4. Testes post hoc pareados via procedimento ART-C (`ARTool::art.con`) com correções de p-valor (*Holm, Bonferroni, Tukey*).
5. Tamanhos de efeito pareados (*Cohen's d* e *Hedges' g*) derivados dos contrastes.

---

## 2. Diagnóstico Detalhado das Não Conformidades

---

### [BSS-STAT-014] Falha de execução no `ARTool::art` quando variáveis possuem espaços ou caracteres não sintáticos
- **Classificação:** Estabilidade Algorítmica e Computacional (**CRÍTICA**).
- **Localização:** `R/art.b.R`, linhas 175–185.
- **Evidência:** 
  A fórmula é gerada concatenando nomes crus de colunas (`paste(predictors, collapse = " * ")`). Se qualquer fator possuir espaços ou caracteres especiais (ex.: `"Grupo Tratamento"`, `"Tempo 1"`, `"ID Paciente"`), a fórmula direta falha em `as.formula()` ou quebra internamente no `ARTool::art`. O `ARTool` utiliza `plyr::ddply` internamente com avaliação quoted, que não tolera backticks ou nomes com espaços, gerando o erro fatal:
  `Error in FUN(X[[i]], ...) : object '`Moisture Level`' not found (ddply -> splitter_d -> eval.quoted)`.
- **Solução Recomendada:**
  Higienizar os nomes das colunas utilizadas no modelo através de `make.names()` antes de montar a fórmula e ajustar o modelo, mapeando os nomes originais para exibição amigável nas tabelas de saída do jamovi.

---

### [BSS-STAT-015] Crash do modelo quando a Variável Dependente é fator ou ordinal
- **Classificação:** Defensividade e Integridade de Tipos (**ALTA**).
- **Localização:** `R/art.b.R`, linhas 160–175.
- **Evidência:**
  Se o usuário importar dados onde a variável dependente for classificada pelo jamovi como fator nominal ou ordinal (comum em escalas Likert tipo 1 a 5), `ARTool::art` interrompe a execução com erro não tratado:
  `Error: Response term must be numeric, ordered factor, or logical (it was factor)`.
  O código atual não aplica `jmvcore::toNumeric(process_data[[dep_var]])`. No formato *wide*, as colunas de onda (`wave_vars`) também não são convertidas previamente para numérico antes da reestruturação.
- **Solução Recomendada:**
  Garantir a conversão defensiva de `dep_var` para numérico via `jmvcore::toNumeric()`, assim como das colunas `wave_vars` no modo *wide*.

---

### [BSS-STAT-016] Falta de limpeza das tabelas gerando erro de linhas duplicadas
- **Classificação:** Interface e Estabilidade do jamovi (**ALTA**).
- **Localização:** `R/art.b.R`, linhas 220, 275 e 335.
- **Evidência:**
  As tabelas `anovaTable`, `posthocTable` e `effectSizeTable` populam os resultados iterando `addRow(rowKey = ...)`. Nenhuma delas executa `table$deleteRows()` antes do preenchimento. Caso o usuário altere opções (como tipo de correção post hoc) sem que o jamovi limpe a tabela, a inserção de chaves repetidas corrompe o objeto interno e dispara erro fatal de dataframe:
  `Error in .rowNamesDF<-: duplicate 'row.names' are not allowed`.
- **Solução Recomendada:**
  Adicionar `if (table$rowCount > 0) table$deleteRows()` no início do preenchimento de cada uma das três tabelas.

---

### [BSS-STAT-017] Refinamento ortográfico e numérico de `Hedges' g`
- **Classificação:** Rigor Científico e Tipografia Estatística (**BAIXA**).
- **Localização:** `R/art.b.R`, linhas 342 e 349.
- **Evidência:**
  1. O rótulo é grafado `"Hedge's g"` (o nome do autor é Larry V. Hedges; a grafia científica correta é `"Hedges' g"`).
  2. A fórmula de correção $J = 1 - \frac{3}{4 \cdot df - 1}$ deve ser protegida para graus de liberdade pequenos ($df > 1$) a fim de evitar anomalias aritméticas.
- **Solução Recomendada:**
  Ajustar a string para `"Hedges' g"` e delimitar a correção para `df > 1`.

---

### [BSS-CODE-006] Omissão das referências de ART nos metadados YAML
- **Classificação:** Conformidade Bibliográfica (**MÉDIA**).
- **Localização:** `jamovi/00refs.yaml` e `jamovi/art.a.yaml`.
- **Evidência:**
  As dependências `ARTool` e `emmeans`, além dos artigos fundamentais metodológicos (*Wobbrock et al., 2011*; *Elkin et al., 2021*), não estão catalogados em `jamovi/00refs.yaml`. O arquivo `jamovi/art.a.yaml` não possui a chave `refs:`.
- **Solução Recomendada:**
  Cadastrar os trabalhos em `00refs.yaml` e vincular formalmente na chave `refs:` de `art.a.yaml`.

---

## 3. Matriz Sintética de Propostas de Correção

| ID do Ticket | Tipo | Severidade | Arquivos Afetados | Descrição Resumida |
|---|---|---|---|---|
| **`BSS-STAT-014`** | Computacional | **CRÍTICA** | `R/art.b.R` | Higienização sintática de colunas com `make.names()` para evitar quebra no `plyr::ddply` interno de `ARTool::art`. |
| **`BSS-STAT-015`** | Tipagem / Defensiva | **ALTA** | `R/art.b.R` | Coerção prévia para numérico (`jmvcore::toNumeric`) da variável dependente e colunas de medidas repetidas. |
| **`BSS-STAT-016`** | Interface / Tabela | **ALTA** | `R/art.b.R` | Limpeza preventiva com `deleteRows()` em `anovaTable`, `posthocTable` e `effectSizeTable` para evitar erro de chaves duplicadas. |
| **`BSS-STAT-017`** | Rigor / Tipografia | **BAIXA** | `R/art.b.R` | Correção ortográfica de `"Hedges' g"` e proteção para $df > 1$. |
| **`BSS-CODE-006`** | Metadados / YAML | **MÉDIA** | `jamovi/00refs.yaml`, `jamovi/art.a.yaml` | Cadastro formal e vinculação das referências de `ARTool`, `emmeans`, `Wobbrock2011` e `Elkin2021`. |

---

*Relatório de auditoria independente gerado e validado em conformidade com o Protocolo BSS. Nenhuma modificação nos arquivos de análise foi realizada; aguardando autorização explícita do usuário.*
