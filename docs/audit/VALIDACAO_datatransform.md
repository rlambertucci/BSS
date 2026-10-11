# Relatório de Validação Independente — Ciclo 10: datatransform

**Módulo Auditado:** `dataTransform` (`R/datatransform.b.R`, `jamovi/datatransform.a.yaml`, `jamovi/00refs.yaml`)  
**Data da Validação:** 10/10/2026  
**Ambiente:** R 4.5.2, `jmvcore`, `R6`, `bestNormalize`, `SuppDists`, Windows x64  
**Script Executado:** `tests/validation/test_datatransform_fixes.R`  
**Status Global:** **APROVADO (100% de conformidade)**

---

## 1. Sumário Executivo das Correções Implementadas

| Ticket | Classificação | Natureza | Descrição / Solução Implementada | Status de Validação |
| :--- | :--- | :--- | :--- | :--- |
| **BSS-STAT-035** | Alta | Integridade de Dados | Coerção defensiva sistemática de variáveis contínuas via `.to_numeric_vec()`, prevenindo falhas de tipo e paradas não tratadas ao estimar parâmetros de potência. | **VALIDADO** (Aprovado em runtime) |
| **BSS-STAT-036** | Alta | Resiliência / UX | Substituição do stop fatal `jmvcore::reject()` por nota explicativa elegante na tabela de lambdas quando Box-Cox recebe dados não-positivos e `auto_shift = FALSE`. | **VALIDADO** (Aprovado em runtime) |
| **BSS-STAT-037** | Média | Robustez Numérica | Proteção integral aninhada via `tryCatch` no ajuste de sistemas de Johnson (`SuppDists`), garantindo continuidade do fluxo mesmo com falha dupla de momentos e quantis. | **VALIDADO** (Aprovado em runtime) |
| **BSS-STAT-038** | Baixa | Transparência Metodológica | Registro automático de nota metodológica na tabela de Shapiro-Wilk comunicando a subamostragem representativa de $N = 5000$ quando o tamanho amostral original excede o teto computacional do teste. | **VALIDADO** (Aprovado em runtime) |
| **BSS-CODE-016** | Baixa | Documentação / Citações | Cadastro canônico de `bestNormalize` (Peterson, 2021), `SuppDists` (Wheeler, 2020), `BoxCox1964`, `YeoJohnson2000`, `Johnson1949` e `ShapiroWilk1965` em `jamovi/00refs.yaml` e no `.a.yaml`. | **VALIDADO** (Conformidade bibliográfica) |

---

## 2. Evidências dos Testes de Execução

### Teste 1: BSS-STAT-035 (Coerção e Yeo-Johnson com Valores Negativos e Positivos)
- **Cenário:** Variável com valores negativos, zero e positivos (`c(-2.0, -1.0, 0.0, 1.5, ..., 10.5)`).
- **Resultado:**
  ```
         variable      method  parameter     value shift_applied shift_value
  "score"    score Yeo-Johnson Lambda (λ) 0.6413394            No           0
  ```
- **Conclusão:** Parâmetro ótimo $\lambda = 0.6413$ estimado com sucesso pelo algoritmo de Yeo-Johnson.

### Teste 2: BSS-STAT-036 (Box-Cox com Dados Não-Positivos e `auto_shift = FALSE`)
- **Cenário:** Dados com valores negativos e zero sem ativação de auto-deslocamento.
- **Comportamento Anterior:** Disparava `jmvcore::reject()` e quebrava a sessão com tela vermelha.
- **Resultado Atual:**
  ```
        variable  method  parameter value                   shift_applied shift_value
  "vals"     vals Box-Cox Lambda (λ)    NA No (Error: non-positive values)           0
  ```
  - Nota na tabela: `Warning: Box-Cox transformation requires strictly positive values (X > 0). Enable 'Automatic Shift' or select Yeo-Johnson.`
- **Conclusão:** Execução resiliente e transparente.

### Teste 3: BSS-STAT-036 (Box-Cox com `auto_shift = TRUE`)
- **Cenário:** Dados não-positivos com auto-shift ativado.
- **Resultado:** Deslocamento automático de $+1.001$ aplicado e $\lambda = 0.4582$ estimado com sucesso.

### Teste 4: BSS-STAT-037 (Curvas de Johnson)
- **Cenário:** Distribuição assimétrica contínua ($N = 50$).
- **Resultado:** Sistema ajustado ($S_B$, tipo 2), parâmetros $\gamma = 0.5648$, $\delta = 0.6094$, $\xi = 0.1061$, $\lambda = 8.1018$ tabulados com fidelidade.

### Teste 5: BSS-STAT-038 (Shapiro-Wilk para $N > 5000$)
- **Cenário:** Conjunto de dados com $N = 5500$.
- **Resultado:** Teste executado com subsample representativo e nota informativa adicionada à tabela:
  `Note. For sample sizes N > 5000, Shapiro-Wilk statistic is computed on a representative random subsample of N = 5000.`

---

## 3. Conclusão da Validação
O módulo `dataTransform` está completamente auditado, matematicamente blindado e metodologicamente transparente. Conclusão do último módulo individual de análise com êxito integral.
