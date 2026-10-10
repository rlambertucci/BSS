# RELATÓRIO DE AUDITORIA — CICLO 2: IMPUTAÇÃO DE DADOS (`dataimputation`)

- **Análise Auditada:** `dataimputation` (MICE Cross-sectional, MICE Longitudinal Multilevel e missRanger Random Forest)
- **Arquivos Inspecionados:**
  - `R/dataimputation.b.R` (859 linhas)
  - `jamovi/dataimputation.a.yaml`
  - `jamovi/dataimputation.r.yaml`
  - `jamovi/dataimputation.u.yaml`
- **Validação Computacional:** `tests/validation/test_dataimputation.R`
- **Data da Auditoria:** 10 de outubro de 2026
- **Status:** DIAGNÓSTICO CONCLUÍDO — AGUARDANDO AUTORIZAÇÃO DO USUÁRIO

---

## 1. Visão Geral e Fundamentação Científica

O módulo `dataimputation` implementa três abordagens para tratamento de dados ausentes sob pressuposto MAR (Missing at Random) e MCAR (Missing Completely at Random):
1. **MICE Cross-sectional:** Imputação multivariada por equações encadeadas completamente condicionais (FCS - Fully Conditional Specification; van Buuren & Groothuis-Oudshoorn, 2011), cobrindo PMM (Predictive Mean Matching), regressão linear bayesiana (norm), regressão logística binária (logreg) e politômica (polyreg).
2. **MICE Longitudinal Multilevel:** Imputação para delineamentos com medidas repetidas e estrutura hierárquica/agrupada utilizando modelos de efeitos aleatórios em dois níveis (`2l.pmm` e `2l.continuous` via integração com `miceadds`; Robitzsch & Grund, 2024), com suporte a conversão automática wide-to-long.
3. **missRanger (Random Forest):** Imputação iterativa não-paramétrica baseada no algoritmo MissForest combinado com PMM rápido (Mayer, 2019; Stekhoven & Bühlmann, 2012), capaz de capturar relações não-lineares complexas e interações de alta ordem sem pressupostos distributivos.
4. **Inspeção Gráfica de Padrões:** Padrões combinatórios de ausência (`VIM::aggr`), correlação de ausência (`GGally::ggcorr`), traços de convergência de cadeias (`chainMean` / `chainVar`) e sobreposição de distribuições observadas vs. imputadas.

---

## 2. Diagnóstico Detalhado dos Problemas Identificados

### [BSS-STAT-005] Coerção destrutiva de Subject ID alfanumérico em MICE Longitudinal
- **Classificação:** Erro Computacional e Inferencial Crítico.
- **Localização:** `R/dataimputation.b.R`, linha 273.
- **Evidência:**
  ```r
  if (!is.null(subject_id_var) && nchar(subject_id_var) > 0 && subject_id_var %in% names(data_to_impute)) {
    data_to_impute[[subject_id_var]] <- as.integer(data_to_impute[[subject_id_var]])
  }
  ```
  Se a variável de ID do sujeito for do tipo texto/caractere (ex.: `"ID01"`, `"S_102"`, comum em pesquisas biomédicas), a conversão direta `as.integer()` do R falha silenciosamente produzindo exclusivamente `NA` (`NAs introduced by coercion`). Como consequência, a variável de agrupamento perde 100% dos dados, fazendo o algoritmo multinível do `miceadds` falhar fatalmente com erro de cluster ausente.
- **Solução Recomendada:** Converter com segurança via fator:
  ```r
  data_to_impute[[subject_id_var]] <- as.integer(as.factor(data_to_impute[[subject_id_var]]))
  ```
- **Impacto da Correção:** Permite que qualquer identificador (numérico, alfanumérico ou texto) seja corretamente reconhecido como cluster longitudinal sem introdução de `NA`.

---

### [BSS-STAT-006] Exibição corrompida de categorias na tabela de dados imputados
- **Classificação:** Erro de Integridade de Apresentação de Resultados.
- **Localização:** `R/dataimputation.b.R`, linha 566.
- **Evidência:**
  ```r
  imputed = as.character(round(suppressWarnings(as.numeric(imputed_col[i])), 3) %||% as.character(imputed_col[i]))
  ```
  Para variáveis categóricas (fatores), a chamada `as.numeric(imputed_col[i])` retorna o índice inteiro do nível (ex.: `1` ou `2`) em vez do rótulo da categoria (ex.: `"Controle"` ou `"Intervenção"`). Se a coluna for string, retorna `"NA"`.
- **Solução Recomendada:** Formatar condicionalmente pelo tipo da variável:
  ```r
  val <- imputed_col[i]
  disp_val <- if (is.numeric(val)) as.character(round(val, 3)) else as.character(val)
  ```
- **Impacto da Correção:** A tabela de valores imputados passará a exibir os rótulos nominais/ordinais reais gerados pela imputação em vez de códigos inteiros opacos.

---

### [BSS-STAT-007] Poluição permanente do ambiente global (`.GlobalEnv`)
- **Classificação:** Violação de Boas Práticas e Padrão CRAN de Engenharia de Software.
- **Localização:** `R/dataimputation.b.R`, linha 293.
- **Evidência:**
  ```r
  if (!is.null(fn)) assign(fn_name, fn, envir = globalenv())
  ```
  O método injeta `mice.impute.2l.pmm` diretamente no `.GlobalEnv` do usuário e não o remove após o término da execução, violando os princípios de encapsulamento de pacotes R.
- **Solução Recomendada:** Registrar limpeza imediata com `on.exit()`:
  ```r
  on.exit({
    if (exists(fn_name, envir = globalenv())) {
      rm(list = fn_name, envir = globalenv())
    }
  }, add = TRUE)
  ```
- **Impacto da Correção:** Garante que o ambiente R do usuário permaneça higienizado após a execução de modelos longitudinais.

---

### [BSS-STAT-008] Ausência de validação defensiva para `NA` no Subject ID em MICE Longitudinal
- **Classificação:** Robustez e Tratamento Defensivo de Dados.
- **Localização:** `R/dataimputation.b.R`, linhas 215 a 240.
- **Evidência:** Se a coluna de Subject ID contiver observações faltantes, o `stats::reshape` ou o `mice` quebram com mensagens de erro técnicas do R em vez de avisar o usuário.
- **Solução Recomendada:** Adicionar verificação precoce:
  ```r
  if (anyNA(data[[subject_id_var]])) {
    jmvcore::reject("Subject ID variable cannot contain missing values.", code = "subject_id_has_na")
    return()
  }
  ```
- **Impacto da Correção:** Previne falhas crípticas em tempo de execução quando a planilha do usuário possui linhas incompletas no identificador.

---

### [BSS-CODE-004] Omissão das referências bibliográficas obrigatórias nos metadados YAML
- **Classificação:** Transparência Científica e Conformidade com o Protocolo (Seção 7).
- **Localização:** `jamovi/dataimputation.a.yaml` e `jamovi/00refs.yaml`.
- **Evidência:** O arquivo `jamovi/dataimputation.a.yaml` não possui a chave `refs:`, e as citações de `mice`, `miceadds`, `missRanger`, `VIM` e `GGally` precisam ser consolidadas formalmente no dicionário bibliográfico `jamovi/00refs.yaml`.
- **Solução Recomendada:** Cadastrar as obras completas em `jamovi/00refs.yaml` e adicionar a chave `refs:` em `dataimputation.a.yaml`.

---

## 3. Matriz Sintética de Propostas de Correção

| ID do Ticket | Tipo | Severidade | Arquivos Afetados | Descrição Resumida |
|---|---|---|---|---|
| **`BSS-STAT-005`** | Computacional | CRÍTICA | `R/dataimputation.b.R` | Correção da coerção de Subject ID alfanumérico para evitar geração espúria de `NA` em MICE longitudinal. |
| **`BSS-STAT-006`** | Apresentação | ALTA | `R/dataimputation.b.R` | Correção da formatação de valores imputados na tabela para exibir rótulos nominais em vez de códigos inteiros. |
| **`BSS-STAT-007`** | Engenharia | MÉDIA | `R/dataimputation.b.R` | Implementação de limpeza com `on.exit()` para descarregar funções injetadas no `.GlobalEnv`. |
| **`BSS-STAT-008`** | Validação | MÉDIA | `R/dataimputation.b.R` | Validação defensiva precoce impedindo `NA` na variável de Subject ID. |
| **`BSS-CODE-004`** | Citações | MÉDIA | `jamovi/00refs.yaml`, `jamovi/dataimputation.a.yaml` | Cadastro formal e vinculação de referências bibliográficas de `mice`, `miceadds`, `missRanger`, `VIM` e `GGally`. |

---

*Relatório de auditoria independente gerado e validado em conformidade com o Protocolo BSS.*
