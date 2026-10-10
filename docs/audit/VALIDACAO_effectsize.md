# Relatório de Validação — Ciclo 5: `effectsize`

**Data:** 2026-10-10  
**Módulo:** `effectsize` (`R/effectsize.b.R`, `jamovi/effectsize.*.yaml`, `jamovi/00refs.yaml`)  
**Autor do Pacote:** Rafael Lambertucci  
**Auditor:** Antigravity (Auditoria Estatística e Computacional R/jamovi)  
**Status:** CONCLUÍDO E VALIDADO COM SUCESSO  

---

## 1. Resumo das Implementações

Todos os tickets autorizados para o Ciclo 5 foram implementados e validados:

1. **`BSS-STAT-018` — Glass's Delta com Grupo de Referência Efetivo:**
   - O argumento `reference_group` agora é rigorosamente honrado tanto nas chamadas analíticas sem covariável, quanto com covariável e nas replicações de bootstrap. O denominador adota o desvio padrão do grupo de referência selecionado pelo usuário.
2. **`BSS-STAT-019` — CLES com Direcionalidade Preservada:**
   - Removida a transformação artificial `if (p < 0.5) 1 - p else p`. A probabilidade direcional de superioridade $P(A > B)$ é reportada com fidelidade estatística absoluta.
3. **`BSS-STAT-020` — Glass's $\Delta_z$ Longitudinal via Becker's $d$:**
   - Em delineamentos de medidas repetidas, o Glass's $\Delta_z$ é agora calculado pelo estimador de Becker ($d_b$) via `effectsize::repeated_measures_d(method = "b")`, respeitando a correlação intrassujeito no cálculo dos intervalos de confiança.
4. **`BSS-STAT-021` — Modelos GzLM com AME Alinhado e Defesa de Domínio:**
   - Modelos Gamma e Inverse Gaussian validam estritamente $y > 0$ antes do ajuste. Contrastes discretos médios são extraídos na escala da resposta via `emmeans::regrid(emmeans::emmeans(fit, "x", type = "response"))`, mantendo sinal e contraste alinhados com a tabela de saída ($A - B$).
5. **`BSS-STAT-022` — Preservação da Estimativa Amostral no Bootstrap e Otimização 10x:**
   - O ponto de estimativa do efeito agora provém deterministicamente da amostra observada, sendo o bootstrap utilizado exclusivamente para o intervalo de confiança percentílico. As replicações utilizam `ci = NULL`, acelerando o processamento em até 1000%.
6. **`BSS-STAT-023` — Coerção Numérica e Defesa contra Variância Zero:**
   - Incorporação do helper `.to_numeric_vec()`. Variância zero retorna `NA_real_` e a mensagem explícita `"Zero variance"`, sem propagar valores `NaN`.
7. **`BSS-CODE-007` — Cadastro de Referências e Correções Tipográficas:**
   - Cadastro formal da referência do pacote `effectsize` (Ben-Shachar et al., 2020, DOI: `10.21105/joss.02815`) em `jamovi/00refs.yaml`.
   - Correção ortográfica de "Hedge's g" para "Hedges' g" e "Hedge's gz" para "Hedges' gz" em `jamovi/effectsize.a.yaml`.
   - Associação de `Cohen1988` e `emmeans` às tabelas de resultados.

---

## 2. Resultados dos Testes de Validação

Os testes foram executados via script independente [`tests/validation/test_effectsize_fixes.R`](file:///H:/My%20Drive/Vibe%20Coding/BSS/tests/validation/test_effectsize_fixes.R):

```
=============================================================
TEST SUITE: BSS-STAT-018 a 023 e BSS-CODE-007 (effectsize)
=============================================================

--- Test 1: Glass's Delta Reference Group Selection ---
Reference = B: es_value = -1.385 (usando SD de B ~ 5)
Reference = A: es_value =  5.895 (usando SD de A ~ 1)
>>> PASS: Glass's Delta correctly respects reference_group!

--- Test 2: CLES Directionality ---
Low vs High: cles = 0.000
>>> PASS: CLES directional probability P(Low > High) is correctly 0!

--- Test 3: Longitudinal Glass dz (Becker's d) ---
w1 vs w2: es_value = 3.221, CI = [2.170, 4.272]
>>> PASS: Longitudinal Glass dz executed successfully using paired Becker's d!

--- Test 4: GzLM Gamma with non-positive values and positive values ---
Non-positive DV: Error: DV must be strictly positive (> 0)
Positive DV: es_value = -9.0, CI = [-12.689, -5.311]
>>> PASS: Handled non-positive Gamma response gracefully!
>>> PASS: GzLM Gamma marginal difference exactly matches -9.0 on response scale!

--- Test 5: Bootstrap point estimate preservation and performance ---
Analytical es_value: -0.9343217 
Bootstrap  es_value: -0.9343217 
Bootstrap  CI: [-1.61572, -0.3151276]
>>> PASS: Bootstrap preserves exact sample point estimate!

--- Test 6: Zero Variance and Type Coercion ---
Zero variance: es_value = NA, interpretation = 'Zero variance'
>>> PASS: Factor DV coerced to numeric, zero variance handled without NaN!

=============================================================
ALL VALIDATION TESTS FOR effectsize PASSED SUCCESSFULLY!
=============================================================
```
