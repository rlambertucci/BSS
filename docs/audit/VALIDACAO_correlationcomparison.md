# Relatório de Validação Independente — Ciclo 8: correlationcomparison

**Módulo Auditado:** `correlationcomparison` (`R/correlationcomparison.b.R`, `jamovi/correlationcomparison.a.yaml`, `jamovi/00refs.yaml`)  
**Data da Validação:** 10/10/2026  
**Ambiente:** R 4.5.2, `jmvcore`, `R6`, Windows x64  
**Script Executado:** `tests/validation/test_correlationcomparison_fixes.R`  
**Status Global:** **APROVADO (100% de conformidade)**

---

## 1. Sumário Executivo das Correções Implementadas

| Ticket | Classificação | Natureza | Descrição / Solução Implementada | Status de Validação |
| :--- | :--- | :--- | :--- | :--- |
| **BSS-CODE-011** | Crítica | Crash de R Runtime | Prevenção de falha fatal `missing value where TRUE/FALSE needed` decorrente de avaliação lógica em campos `NA` ou vazios na interface do jamovi. Implementado helper defensivo `.is_empty()`. | **VALIDADO** (Aprovado em runtime) |
| **BSS-STAT-030** | Alta | Usabilidade / Resiliência | Eliminação de interrupções agressivas com `jmvcore::reject()` em condições ordinárias de preenchimento ($N < 4$, $\|r\| \ge 1$), substituindo por notas explicativas na tabela e preenchimento gracioso com `NA`. | **VALIDADO** (Aprovado em runtime) |
| **BSS-STAT-031** | Alta | Consistência Geométrica | Detecção e reporte amigável de matrizes de correlação dependentes 3×3 com determinante não positivo ($det(R) \le 0$), orientando o usuário sobre a inconsistência geométrica dos dados. | **VALIDADO** (Aprovado em runtime) |
| **BSS-STAT-032** | Média | Rigor Estatístico APA | Cálculo da magnitude do tamanho de efeito da diferença ($\Delta r$) e Intervalo de Confiança assintótico de 95% via metodologia de Zou (2007) para correlações independentes e dependentes sobrepostas. | **VALIDADO** (Aprovado em runtime) |
| **BSS-CODE-012** | Baixa | Documentação / Citações | Inclusão de referências canônicas (`Fisher1921`, `Steiger1980`, `Williams1959`, `Zou2007`) em `jamovi/00refs.yaml` e mapeamento em `jamovi/correlationcomparison.a.yaml`. | **VALIDADO** (Conformidade bibliográfica) |

---

## 2. Evidências dos Testes de Execução

### Teste 1: BSS-CODE-011 (Defensive Input Validation)
- **Cenário:** Campos de entrada ausentes (`NULL`), não preenchidos (`NA`), ou com vetores vazios.
- **Resultado:** Helper `.is_empty()` classificou confiavelmente cada caso, permitindo retornos seguros e evitando qualquer crash lógico em instruções condicionais `if`.

### Teste 2: BSS-STAT-030 (Validação de Limites $N < 4$ e $|r| \ge 1$)
- **Cenários Testados:**
  1. $N_1 = 3, N_2 = 50$: Capturado sem `reject()`, emitindo na tabela:
     `Warning: Sample size (N) must be at least 4 in both groups to compute Fisher's Z-transformation.`
  2. $r_1 = 1.0, r_2 = 0.5$: Capturado sem crash de divisão por zero/logaritmo, emitindo:
     `Warning: Fisher's Z-transformation is undefined for perfect correlations (|r| = 1).`

### Teste 3: BSS-STAT-031 (Matriz Não Positiva-Definida)
- **Cenário:** Tríade geometricamente impossível no espaço euclidiano ($r_{12} = 0.9, r_{13} = -0.9, r_{23} = 0.9 \implies det(R) = -2.888 \le 0$).
- **Resultado:** Capturado de forma limpa, emitindo na tabela:
  `Warning: The correlation matrix implied by r12, r13, and r23 is not positive-definite (det(R) <= 0). Input values are geometrically inconsistent.`

### Teste 4: BSS-STAT-032 (Caso Independente: Fisher $Z$ e IC 95% Zou 2007)
- **Cenário:** $N_1 = 50, N_2 = 50, r_1 = 0.60, r_2 = 0.20$.
- **Resultados Numéricos:**
  - $Z = 2.377$ (exato com Fisher, 1921).
  - $p = 0.0174$.
  - Nota da Tabela: `Difference (r1 - r2) = 0.400, 95% CI: 0.069 to 0.722 (Zou, 2007).`

### Teste 5: BSS-STAT-032 (Caso Dependente: Williams $t$ e IC 95% Zou 2007)
- **Cenário:** $N = 60, r_{12} = 0.50, r_{13} = 0.20, r_{23} = 0.30$.
- **Resultados Numéricos:**
  - $t(57) = 2.182$ (exato com Williams, 1959).
  - $p = 0.0332$.
  - Nota da Tabela: `Difference (r12 - r13) = 0.300, 95% CI: 0.025 to 0.569 (Zou, 2007).`

---

## 3. Conclusão da Validação
O módulo `correlationcomparison` atinge conformidade plena com os mais altos padrões de robustez computacional e diretrizes estatísticas da APA (7ª ed.), oferecendo agora testes bilaterais rigorosos acompanhados da estimativa pontual da diferença e intervalo de confiança assintótico canônico (Zou, 2007).
