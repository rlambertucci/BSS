# Relatório de Auditoria Independente: Ciclo 3 — `dda` (Descriptive Discriminant Analysis)

**Data da Auditoria:** 2026-10-10  
**Módulo Avaliado:** `dda` (`R/dda.b.R`, `jamovi/dda.a.yaml`, `jamovi/dda.r.yaml`, `DESCRIPTION`, `jamovi/00refs.yaml`)  
**Status do Ciclo:** DIAGNÓSTICO CONCLUÍDO — AGUARDANDO AUTORIZAÇÃO DO USUÁRIO  
**Branch de Auditoria:** `refactor/audit-v0.3.0`  

---

## 1. Contexto Científico e Metodológico

A **Análise Discriminante Descritiva (DDA)** (*Huberty & Olejnik, 2006*) é uma técnica estatística multivariada empregada post hoc após uma MANOVA significativa para examinar e interpretar a separação entre grupos ao longo de combinações lineares ótimas das variáveis dependentes contínuas (funções discriminantes lineares). 

O módulo `dda` do BSS implementa:
1. Extração de funções discriminantes via `MASS::lda` com proporção da variância (*proportion of trace* via decomposição de valores singulares).
2. Coeficientes canônicos de discriminação (*unstandardized coefficients / scaling*).
3. Coeficientes de estrutura (*structure coefficients* / correlações totais entre covariáveis e scores discriminantes, *Courville & Thompson, 2001*).
4. Centróides de grupo nas funções discriminantes.
5. Scores discriminantes individuais por observação.
6. Tamanhos de efeito pareados em LD1 (*Cohen's d* e *Hedges' g*, *Cohen, 1988* via pacote `effsize`).
7. Visualização gráfica 1D e 2D com centróides e elipses normais de 95%.

---

## 2. Diagnóstico Detalhado das Não Conformidades

---

### [BSS-STAT-009] Omissão espúria de linhas por `naOmit` executado no conjunto de dados global
- **Classificação:** Integridade de Dados / Amostragem (CRÍTICA).
- **Localização:** `R/dda.b.R`, linha 51.
- **Evidência:** 
  ```R
  dep <- self$options$dep; covs <- self$options$covs; data <- self$data; data <- jmvcore::naOmit(data)
  ```
  O objeto `self$data` no jamovi contém **todas as colunas da planilha do usuário**. Executar `jmvcore::naOmit(data)` antes de selecionar apenas as variáveis de interesse (`dep` e `covs`) descarta linhas que contenham valores ausentes em quaisquer outras colunas da planilha, mesmo que os dados para a análise DDA estejam 100% completos. Em testes de validação, até 50% ou mais dos casos foram descartados indevidamente.
- **Solução Recomendada:**
  Restringir o escopo do dataframe estritamente às variáveis selecionadas antes de aplicar a remoção de valores ausentes:
  ```R
  data <- self$data[, c(dep, covs), drop = FALSE]
  data <- jmvcore::naOmit(data)
  ```

---

### [BSS-STAT-010] Falta de expurgo de níveis inativos (`droplevels`) na variável de agrupamento
- **Classificação:** Estabilidade Algorítmica e Graus de Liberdade (ALTA).
- **Localização:** `R/dda.b.R`, linhas 53–55.
- **Evidência:**
  ```R
  data[[dep]] <- as.factor(data[[dep]]); group_levels <- levels(data[[dep]]); n_groups <- length(group_levels)
  ```
  Se o fator original possuir níveis não utilizados (ou níveis cujas observações foram filtradas por `naOmit`), `as.factor()` mantém os níveis com contagem zero. Isso induz `MASS::lda` e o gerador de pares `combn` a processar grupos fantasmas vazios, disparando falhas no cálculo de centróides e tamanhos de efeito.
- **Solução Recomendada:**
  Aplicar `droplevels()` explicitamente e validar o número efetivo de observações por nível:
  ```R
  data[[dep]] <- droplevels(as.factor(data[[dep]]))
  group_levels <- levels(data[[dep]])
  n_groups <- length(group_levels)
  ```

---

### [BSS-STAT-011] Tentativa indevida de remoção da coluna identificadora na tabela de coeficientes
- **Classificação:** Apresentação e Integridade de Interface (ALTA).
- **Localização:** `R/dda.b.R`, linha 89.
- **Evidência:**
  ```R
  current_table_cols <- private$.getColumnNames(table)
  cols_to_remove <- setdiff(current_table_cols, dims)
  for(col_name in cols_to_remove) { try(table$deleteColumn(name = col_name), silent=TRUE) }
  ```
  Enquanto todas as outras tabelas dinâmicas do módulo protegem a primeira coluna que contém os nomes das variáveis (`setdiff(..., c("name", dims))` ou `c("varName", dims)`), a tabela `coef` omite `"name"` da lista de proteção. Caso `deleteColumn("name")` seja processado, a coluna de identificação das variáveis é deletada.
- **Solução Recomendada:**
  Uniformizar a proteção:
  ```R
  cols_to_remove <- setdiff(current_table_cols, c("name", dims))
  ```

---

### [BSS-STAT-012] Remoção silenciosa de pontos e centróides no gráfico quando há mais de 8 grupos
- **Classificação:** Integridade Visual e Apresentação Científica (ALTA).
- **Localização:** `R/dda.b.R`, linhas 309 e 321.
- **Evidência:**
  ```R
  ggplot2::scale_color_brewer(palette = "Set2")
  ```
  A paleta `Set2` de ColorBrewer possui um limite rígido de 8 cores discretas. Quando um conjunto de dados possui 9 ou mais grupos, o ggplot2 atribui cor `NA` aos grupos excedentes e **remove silenciosamente os pontos e centróides correspondentes da plotagem** com aviso (`Removed n rows containing missing values (geom_point)`).
- **Solução Recomendada:**
  Aplicar `scale_color_brewer(palette = "Set2")` condicionalmente para até 8 grupos. Para $> 8$ grupos, manter a escala cromática padrão do ggplot2 (`scale_color_hue`) ou expandir paleta para garantir que nenhum grupo seja omitido do gráfico.

---

### [BSS-STAT-013] Ausência de validação defensiva para covariáveis com variância nula (constantes)
- **Classificação:** Validação Defensiva (MÉDIA).
- **Localização:** `R/dda.b.R`, linhas 53–56.
- **Evidência:**
  Se o usuário selecionar acidentalmente uma covariável invariável (com desvio padrão zero), `MASS::lda` falha com mensagens internas confusas sobre covariáveis constantes no grupo, e `cor()` gera alertas de divisão por zero.
- **Solução Recomendada:**
  Verificar se todas as covariáveis possuem variância estritamente positiva antes de invocar `MASS::lda`:
  ```R
  cov_vars <- sapply(data[, covs, drop = FALSE], function(col) stats::var(col, na.rm = TRUE))
  if (any(is.na(cov_vars) | cov_vars == 0)) {
    jmvcore::reject("One or more covariates have zero variance (constant values).", code="zero_variance_covariates")
    return()
  }
  ```

---

### [BSS-CODE-005] Omissão de dependência de `MASS` no `DESCRIPTION` e ausência de referências bibliográficas
- **Classificação:** Transparência Científica e Empacotamento R (MÉDIA).
- **Localização:** `DESCRIPTION`, `jamovi/00refs.yaml`, `jamovi/dda.a.yaml`.
- **Evidência:**
  1. `MASS` é o motor primordial de `DDA` (`MASS::lda`), mas não consta nos `Imports:` do arquivo `DESCRIPTION`.
  2. `MASS` e `effsize` são citados em `jamovi/dda.r.yaml`, mas não estão cadastrados em `jamovi/00refs.yaml`.
  3. O arquivo `jamovi/dda.a.yaml` não possui a chave `refs:`.
  4. As obras clássicas de DDA citadas no infobox (`Huberty & Olejnik, 2006`; `Courville & Thompson, 2001`; `Cohen, 1988`) não possuem cadastro formal no ecossistema bibliográfico do pacote.
- **Solução Recomendada:**
  - Adicionar `MASS` ao `DESCRIPTION`.
  - Cadastrar as entradas bibliográficas em `jamovi/00refs.yaml`.
  - Adicionar a chave `refs:` em `jamovi/dda.a.yaml`.

---

## 3. Matriz Sintética de Propostas de Correção

| ID do Ticket | Tipo | Severidade | Arquivos Afetados | Descrição Resumida |
|---|---|---|---|---|
| **`BSS-STAT-009`** | Integridade de Dados | **CRÍTICA** | `R/dda.b.R` | Restringir `naOmit` apenas às variáveis em análise (`dep`, `covs`), evitando o descarte espúrio de observações válidas decorrente de NAs em outras colunas da planilha. |
| **`BSS-STAT-010`** | Algorítmico | **ALTA** | `R/dda.b.R` | Aplicar `droplevels()` à variável dependente para evitar processamento de níveis fantasmas com 0 observações. |
| **`BSS-STAT-011`** | Interface / Tabela | **ALTA** | `R/dda.b.R` | Proteger a coluna identificadora `"name"` em `cols_to_remove` na tabela de coeficientes (`coef`). |
| **`BSS-STAT-012`** | Visualização | **ALTA** | `R/dda.b.R` | Condicionar paleta `Set2` a $\le 8$ grupos para impedir a eliminação silenciosa de pontos no gráfico com $> 8$ grupos. |
| **`BSS-STAT-013`** | Validação Defensiva | **MÉDIA** | `R/dda.b.R` | Validação defensiva precoce impedindo a execução se alguma covariável tiver variância zero. |
| **`BSS-CODE-005`** | Metadados / Pacote | **MÉDIA** | `DESCRIPTION`, `jamovi/00refs.yaml`, `jamovi/dda.a.yaml` | Adicionar `MASS` em `Imports:`, cadastrar referências bibliográficas completas e vincular em YAML. |

---

*Relatório de auditoria independente gerado em estrita conformidade com o Protocolo BSS. Nenhuma modificação em arquivos de análise foi realizada; aguardando autorização explícita do usuário para implementação.*
