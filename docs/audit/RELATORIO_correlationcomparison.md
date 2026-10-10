# Relatório de Auditoria — Ciclo 8: correlationcomparison

**Módulo Auditado:** `correlationcomparison` (`R/correlationcomparison.b.R`, `jamovi/correlationcomparison.a.yaml`, `jamovi/correlationcomparison.r.yaml`, `jamovi/correlationcomparison.u.yaml`)  
**Data da Auditoria:** 10/10/2026  
**Auditor Responsável:** Antigravity AI / T3 Code  
**Status da Auditoria:** DIAGNÓSTICO CONCLUÍDO — AGUARDANDO AUTORIZAÇÃO DO USUÁRIO

---

## 1. Contexto e Escopo do Módulo
O módulo `correlationcomparison` permite ao pesquisador comparar:
1. **Duas correlações independentes** ($r_1$ vs $r_2$, obtidas de duas amostras independentes $N_1$ e $N_2$), usando a clássica transformação $Z$ de Fisher (1921).
2. **Duas correlações dependentes sobrepostas** ($r_{12}$ vs $r_{13}$, obtidas do mesmo grupo de participantes $N$, compartilhando a variável 1), controlando pela intercorrelação $r_{23}$ entre as variáveis não compartilhadas, utilizando a modificação de Williams (1959) do teste de Hotelling / teste $Z$ de Steiger (1980).

---

## 2. Diagnóstico Científico, Estatístico e Computacional

### 2.1. Bug Crítico de R Runtime: Avaliação de `NA` em Comparações Lógicas (`BSS-CODE-011`)
- **Código Atual (linhas 53-54 e 105-106):**
  ```r
  if (is.null(n1) || is.null(n2) || is.null(r1) || is.null(r2) || n1 == 0 || n2 == 0) { ... }
  ```
- **Mecanismo do Erro:** Quando o usuário edita ou limpa qualquer campo numérico no jamovi, a opção assume o valor `NA`. Em R:
  - `is.null(NA)` é `FALSE`.
  - O interpretador avalia `n1 == 0`, que resulta em `NA`.
  - A expressão `FALSE || ... || NA` resulta em `NA`.
  - O comando `if (NA)` lança: `Error in if (...): missing value where TRUE/FALSE needed`.
- **Impacto:** Crash imediato do módulo com tela vermelha enquanto o usuário digita valores na interface.

### 2.2. Uso Excessivo de `jmvcore::reject()` em Validações Numéricas de UI (`BSS-STAT-030`)
- **Problema:** O código utiliza `jmvcore::reject()` em mais de 10 situações normais de digitação (valores incompletos, $N < 4$, $|r| \ge 1$). Como `jmvcore::reject()` chama `stop()`, o jamovi trava a exibição da tabela com faixas vermelhas de erro.
- **Solução Recomendada:** Substituir `reject()` por saídas graciosas: quando os campos estão vazios ou sendo digitados, retornar silenciosamente; quando os valores inseridos forem inválidos, definir uma nota informativa na tabela (`table$setNote("warning", ...)`) e deixar as células com `NA`, proporcionando uma UX profissional e estável.

### 2.3. Validação de Positividade Definida da Matriz de Correlação ($det(R) \le 0$) (`BSS-STAT-031`)
- **Fundamentação Matemática:** Para três variáveis correlacionadas, o determinante da matriz de correlação é:
  $$|R| = 1 - r_{12}^2 - r_{13}^2 - r_{23}^2 + 2 r_{12} r_{13} r_{23}$$
  Se $|R| \le 0$, a combinação de coeficientes viola a desigualdade triangular no espaço euclidiano (ex.: $r_{12} = 0.9$, $r_{13} = -0.9$, $r_{23} = 0.9$).
- **Problema:** O módulo atualmente dispara `jmvcore::reject()` em vez de apresentar um aviso claro de inconsistência geométrica/matemática dos dados inseridos.

### 2.4. Ausência de Magnitude do Efeito da Diferença ($\Delta r$) e Intervalo de Confiança (`BSS-STAT-032`)
- **Recomendação das Diretrizes APA (7ª ed.) e Zou (2007):** Apenas relatar o p-valor é insuficiente para a prática estatística contemporânea. É fundamental reportar a magnitude da diferença ($\Delta r = r_1 - r_2$ para amostras independentes; $\Delta r = r_{12} - r_{13}$ para dependentes) e o Intervalo de Confiança a 95% para essa diferença.
- **Solução Recomendada:** Sem alterar as colunas estruturais para manter compatibilidade, exibir de forma elegante na nota de rodapé da tabela:
  `Difference: Δr = X.XX, 95% CI [L.LL, U.UU]`.

### 2.5. Formalização Bibliográfica no Banco Central (`BSS-CODE-012`)
- **Problema:** Fisher (1921), Steiger (1980), Williams (1959) e Zou (2007) não estão registrados em `jamovi/00refs.yaml` nem associados no `.a.yaml`.

---

## 3. Tickets de Correção Propostos

| ID do Ticket | Tipo | Severidade | Descrição Resumida |
| :--- | :--- | :--- | :--- |
| **BSS-CODE-011** | Código | Crítica | Tratamento defensivo de entradas vazias e `NA` para evitar erro `missing value where TRUE/FALSE needed`. |
| **BSS-STAT-030** | Estatística / UX | Alta | Substituição de `jmvcore::reject()` por tratamento gracioso com notas informativas na tabela. |
| **BSS-STAT-031** | Matemática | Alta | Tratamento elegante de matrizes de correlação não positivas-definidas ($|R| \le 0$) e limites limítrofes ($r = \pm 1$). |
| **BSS-STAT-032** | Estatística | Média | Cálculo e relato da diferença de correlações ($\Delta r$) e Intervalo de Confiança (IC 95%) em nota explicativa. |
| **BSS-CODE-012** | Documentação | Baixa | Cadastro canônico de `Fisher1921`, `Steiger1980`, `Williams1959` e `Zou2007` em `jamovi/00refs.yaml` e `correlationcomparison.a.yaml`. |

---

## 4. Solicitação de Autorização
Nenhum arquivo de código ou YAML foi modificado até o momento. Aguardando autorização explícita do usuário para prosseguir com a implementação dos tickets.
