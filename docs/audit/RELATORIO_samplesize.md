# Relatório de Auditoria — Ciclo 6: `samplesize`

**Data:** 2026-10-10  
**Módulo:** `samplesize` (`R/samplesize.b.R`, `jamovi/samplesize.*.yaml`)  
**Autor do Pacote:** Rafael Lambertucci  
**Auditor:** Antigravity (Auditoria Estatística e Computacional R/jamovi)  
**Status:** Diagnóstico concluído — Aguardando autorização do usuário  

---

## 1. Visão Geral do Módulo

O módulo `samplesize` fornece cálculos de dimensionamento amostral para dois grandes domínios:
1. **Populações Finitas:**
   - Fórmula de Cochran (1977) com correção para população finita (FPC).
   - Fórmula de Slovin (1960).
2. **Delineamentos Pré-clínicos com Animais (Equação de Recursos):**
   - Delineamento 1: ANOVA One-way ($k$ grupos).
   - Delineamento 2: Medidas Repetidas ($r$ medidas no tempo), com opção de sacrifício experimental.
   - Delineamento 3: ANOVA Mista ($k$ grupos entre sujeitos $\times$ $r$ medidas repetidas), com opção de sacrifício.

---

## 2. Diagnóstico Detalhado e Fundamentação Científica

### BSS-STAT-024: Falhas Algébricas no Método da Equação de Recursos (Mead 1988, Charan & Kantharia 2013, Arifin 2017)
O método da equação de recursos estipula que os graus de liberdade do resíduo (erro) na ANOVA ($E$) devem situar-se entre 10 e 20 ($10 \le E \le 20$):
- **Design 2 com Sacrifício:** No código original, calculava-se `base_min_n <- ceiling(10/(r-1) + 1)` e, quando `sacrifice == TRUE`, multiplicava-se `min_n <- base_min_n * r` e definia-se `min_total <- min_n`. Isso gerava um valor absurdo: o tamanho amostral por grupo tornava-se idêntico ao total! Se animais são sacrificados a cada medida, o delineamento torna-se um One-Way independente entre $r$ coortes: $E = r(n - 1) \implies n = \frac{E}{r} + 1$. O tamanho por coorte é $\lceil 10/r + 1 \rceil$, e o total de animais é $r \times n$.
- **Design 3 (Mixed ANOVA):** O código original usava `denominator <- k * r`. Em uma ANOVA Mista (Split-Plot), os graus de liberdade do erro intra-sujeito (interação Grupo $\times$ Tempo) são dados por $E_{\text{within}} = k(n - 1)(r - 1)$, o que implica denominador $k(r - 1)$, e **não** $k \cdot r$. O denominador $k \cdot r$ subdimensionava gravemente o tamanho amostral (ex.: resultando em $n = 2$ animais por grupo, com erro entre sujeitos de apenas 4 gl, violando o mínimo de 10 gl).

### BSS-STAT-025: Limitação de Fronteira para População Finita e Slovin
- Em amostragem finita sem reposição, o tamanho calculado não pode exceder a população $N$. Deve-se impor $n \le N$.
- A fórmula de Slovin não emprega o nível de confiança (assume tacitamente ~95%). O código preenchia `confidence_level = confidence_level_pct` no relatório de Slovin, induzindo o pesquisador a crer que a fórmula incorporava o valor escolhido. Deve ser exibido como não aplicável ou acompanhado de nota explicativa.

### BSS-STAT-026: Usabilidade na Interface jamovi
- No modo `finite`, o uso de `jmvcore::reject(...)` lançava exceções vermelhas na interface sempre que um campo estava em edição. Deve-se adotar mensagens explicativas no painel de texto, idêntico ao padrão maduro de `animals`.
- O valor padrão de `nMeasures_animals` em `jamovi/samplesize.a.yaml` era `0`, fazendo com que selecionar Designs 2 ou 3 abrisse imediatamente com aviso de parâmetro inválido. O default deve ser `2`.

### BSS-CODE-008: Citações e Metadados Formais
- Cadastrar as referências fundamentais em `jamovi/00refs.yaml`:
  - `Cochran1977` (Cochran, W. G., 1977)
  - `Slovin1960` (Slovin, E., 1960)
  - `Festing2002` (Festing, M. F., & Altman, D. G., 2002)
  - `Charan2013` (Charan, J., & Kantharia, N. D., 2013)
  - `Arifin2017` (Arifin, W. N., & Wan Mohammad, W. M. Z., 2017)
- Vincular citações em `samplesize.a.yaml` e `samplesize.r.yaml`.

---

## 3. Matriz de Proposta de Tickets

| Ticket | Escopo | Descrição |
|---|---|---|
| `BSS-STAT-024` | `R/samplesize.b.R` | Correção algébrica completa dos graus de liberdade de erro e sacrifício experimental em Designs 2 e 3. |
| `BSS-STAT-025` | `R/samplesize.b.R` | Limitação $n \le N$ em populações finitas e transparência metodológica na fórmula de Slovin. |
| `BSS-STAT-026` | `R/samplesize.b.R`, `jamovi/samplesize.a.yaml` | Substituição de `jmvcore::reject` por avisos limpos e ajuste do default `nMeasures_animals` para 2. |
| `BSS-CODE-008` | `jamovi/00refs.yaml`, `jamovi/samplesize.*.yaml` | Cadastro formal de Cochran, Slovin, Festing, Charan e Arifin e vinculação às tabelas. |
