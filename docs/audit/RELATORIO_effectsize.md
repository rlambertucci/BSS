# Relatório de Auditoria — Ciclo 5: `effectsize`

**Data:** 2026-10-10  
**Módulo:** `effectsize` (`R/effectsize.b.R`, `jamovi/effectsize.*.yaml`)  
**Autor do Pacote:** Rafael Lambertucci  
**Auditor:** Antigravity (Auditoria Estatística e Computacional R/jamovi)  
**Status:** Diagnóstico concluído — Aguardando autorização do usuário  

---

## 1. Visão Geral do Módulo

O módulo `effectsize` calcula tamanhos de efeito pairwise para delineamentos transversais (*independent samples*) e longitudinais (*repeated measures*), suportando:
- Cohen's $d$, Hedges' $g$, Glass's $\Delta$, correlação biserial por postos (*rank biserial*).
- Formatos *wide* (uma coluna por onda temporal) e *long* (variável de tempo + variável de sujeito).
- Ajuste por covariável via ANCOVA (`emmeans`).
- CLES (*Common Language Effect Size* / probabilidade de superioridade).
- Modelos Lineares Generalizados (GzLM) com Efeitos Marginais Médios (AME).
- Intervalos de confiança analíticos e por bootstrap não-paramétrico.

---

## 2. Diagnóstico Detalhado e Fundamentação Científica

### BSS-STAT-018: Falha Crítica no Glass's Delta (Grupo de Referência Ignorado)
- **Problema:** Na estimativa analítica sem covariável, o código invocava `effectsize::glass_delta(ga, gb, ci = ci_level)`. No pacote `effectsize`, a convenção padrão para `glass_delta(x, y)` adota o segundo argumento (`y`) como grupo de controle/referência. O parâmetro de interface `reference_group` era repassado na função, mas ignorado na chamada. Com isso, seja o usuário selecionando o grupo A ou o grupo B como referência, o denominador sempre usava a variância do segundo grupo, violando a definição do estimador de Glass (1976).
- **Correção:** Respeitar o `reference_group` informado pelo usuário, estruturando a chamada com o argumento formal de referência ou ordenando os grupos adequadamente tanto na estimativa pontual, quanto nos CIs e nas replicações bootstrap.

### BSS-STAT-019: Inversão Direcional do CLES (Common Language Effect Size)
- **Problema:** Em `.compute_cles_independent` e `.compute_cles_paired`, o código aplicava:
  ```r
  cles_val <- if (p < 0.5) 1 - p else p
  ```
  Ao forçar o CLES para a faixa $[0.50, 1.00]$, o sinal da diferença era apagado. Em uma comparação explícita $A \text{ vs } B$ onde $A$ é estritamente inferior a $B$ ($P(A > B) = 0.05$), a tabela exibia $0.95$, induzindo o leitor a crer que $A$ é 95% superior a $B$.
- **Correção:** Eliminar a inversão artificial. O CLES deve reportar fielmente a probabilidade de superioridade direcional $P(A > B)$ conforme McGraw & Wong (1992) e Vargha & Delaney (2000).

### BSS-STAT-020: Glass's $\Delta_z$ Longitudinal Tratado como Amostras Independentes
- **Problema:** No modo longitudinal (*within-subjects*), a opção `glass_dz` chamava `effectsize::glass_delta(col1, col2)` sem considerar o emparelhamento dos dados, pois `glass_delta` em `effectsize` gera erro se `paired = TRUE` for passado. O cálculo ignorava a estrutura de medidas repetidas.
- **Correção:** Em medidas repetidas, o estimador canônico padronizado pela variância do controle (ou onda basal) é o Becker's $d$ ($d_b$), implementado nativamente e robustamente por `effectsize::repeated_measures_d(col1, col2, method = "b")`.

### BSS-STAT-021: Cálculo Precário de AME em GzLM e Inversão de Sinal
- **Problema:** Em modelos Gamma e Inverse Gaussian (link log), o efeito marginal era aproximado por `coef_x * mean(dv)`. Além disso, com `relevel(ref = name_a)`, o coeficiente estimava $B - A$, gerando sinal oposto ao da tabela que exibe $A \text{ vs } B$. Se o DV continha valores $\le 0$, o modelo quebrava com erro não tratado.
- **Correção:** Validação prévia de positividade estrita para Gamma/Inverse Gaussian. Obtenção do contraste marginal exato na escala da resposta via `pairs(emmeans::regrid(emmeans::emmeans(fit, "x", type = "response")))`, garantindo direção correta $A - B$ e erro-padrão via método Delta.

### BSS-STAT-022: Estimativa Pontual Alterada pelo Bootstrap e Otimização de Performance (10x)
- **Problema:** Quando `ci_method == "bootstrap"`, o ponto de estimativa do efeito era substituído por `mean(boot_vals)` (introduzindo ruído Monte Carlo à estimativa pontual). Ademais, cada iteração do bootstrap recalculava o intervalo analítico interno (`ci = ci_level`), tornando o procedimento até 10 vezes mais lento.
- **Correção:** O ponto de estimativa deve ser calculado determiniscamente sobre a amostra observada original, reservando o bootstrap estritamente para o cálculo do intervalo percentílico. Nas replicações de bootstrap, passar `ci = NULL` para acelerar o processamento em até 1000%.

### BSS-STAT-023: Coerção de Tipos e Defesa contra Variância Zero
- **Problema:** Colunas numéricas importadas como fatores ou caracteres eram ignoradas silenciosamente com `next`. Casos de variância zero geravam `NaN`.
- **Correção:** Incorporar a função utilitária `.to_numeric_vec()` e tratar variância nula retornando `NA_real_` com mensagem explicativa `"Zero variance"`.

### BSS-CODE-007: Metadados Bibliográficos e Correção Tipográfica
- **Problema:** O pacote `effectsize` é citado em `jamovi/effectsize.r.yaml`, mas não possui entrada cadastrada em `jamovi/00refs.yaml`. No arquivo `jamovi/effectsize.a.yaml`, o título exibe "Hedge's g" (grafia errônea para Hedges' g).
- **Correção:** Adicionar `effectsize` (Ben-Shachar et al., 2020) ao `00refs.yaml`, corrigir grafias em `effectsize.a.yaml` e atualizar referências em `effectsize.a.yaml` e `effectsize.r.yaml`.

---

## 3. Matriz de Proposta de Tickets

| Ticket | Escopo | Descrição |
|---|---|---|
| `BSS-STAT-018` | `R/effectsize.b.R` | Respeitar rigorosamente `reference_group` no Glass's Delta (analítico e bootstrap). |
| `BSS-STAT-019` | `R/effectsize.b.R` | Remover inversão artificial do CLES, preservando a probabilidade direcional $P(A > B)$. |
| `BSS-STAT-020` | `R/effectsize.b.R` | Utilizar `repeated_measures_d(method = "b")` (Becker's $d$) para Glass's $\Delta_z$ longitudinal. |
| `BSS-STAT-021` | `R/effectsize.b.R` | Validação de positividade e cálculo de AME via `emmeans::regrid` na escala de resposta com sinal alinhado. |
| `BSS-STAT-022` | `R/effectsize.b.R` | Ponto de estimativa baseado na amostra original e bootstrap acelerado com `ci = NULL`. |
| `BSS-STAT-023` | `R/effectsize.b.R` | Coerção numérica defensiva via `.to_numeric_vec` e proteção contra variância zero. |
| `BSS-CODE-007` | `jamovi/00refs.yaml`, `jamovi/effectsize.*.yaml` | Cadastro da referência `effectsize` no `00refs.yaml` e correção de "Hedges' g" nos títulos. |
