# RELATÓRIO DE AUDITORIA — CICLO 1: ANÁLISE DE REDES (`networkanalysis`)

- **Análise Auditada:** `networkanalysis` (Gaussian Graphical Models [GGM] e Mixed Graphical Models [MGM])
- **Arquivos Inspecionados:**
  - `R/networkanalysis.b.R` (1.373 linhas)
  - `jamovi/networkanalysis.a.yaml`
  - `jamovi/networkanalysis.r.yaml`
  - `jamovi/networkanalysis.u.yaml`
- **Validação Computacional:** `tests/validation/test_networkanalysis.R`
- **Data da Auditoria:** 10 de outubro de 2026
- **Status:** DIAGNÓSTICO CONCLUÍDO — AGUARDANDO AUTORIZAÇÃO DO USUÁRIO

---

## 1. Visão Geral e Fundamentação Científica

A análise de redes no BSS disponibiliza duas abordagens psicométricas e biométricas:
1. **Redes Contínuas (GGM - Gaussian Graphical Models):** Estimação de redes de correlação parcial regularizadas via algoritmo EBICglasso (Foygel & Drton, 2010; Epskamp et al., 2018), com seleção de penalidade baseada no Critério de Informação Bayesiano Estendido.
2. **Redes Mistas (MGM - Mixed Graphical Models):** Estimação de modelos gráficos combinando variáveis contínuas (Gaussiana), categóricas (Multinomial) e contagens (Poisson) por meio de regressões nodewise penalizadas (Haslbeck & Waldorp, 2020), com combinação de parâmetros por regra AND ou OR.
3. **Métricas Topológicas e de Centralidade:** Força/Grau (Strength), Proximidade (Closeness), Intermediação (Betweenness) e Influência Esperada (Expected Influence - Robinaugh et al., 2016), além de coeficientes de agrupamento ponderado (Barrat, Onnela, WS, Zhang).

---

## 2. Diagnóstico Detalhado dos Problemas Identificados

### [BSS-STAT-001] Destruição de arestas envolvendo variáveis categóricas (>2 níveis) em Redes Mistas (MGM)
- **Classificação:** Erro Estatístico e Matemático Crítico.
- **Localização:** `R/networkanalysis.b.R`, linhas 737 a 746.
- **Evidência:**
  ```r
  adjMatrix <- mgmFit$pairwise$wadj * mgmFit$pairwise$signs
  ...
  if (any(!is.finite(adjMatrix))) {
    adjMatrix[!is.finite(adjMatrix)] <- 0
  }
  ```
  No pacote `mgm`, interações envolvendo variáveis categóricas com mais de 2 níveis (multinomial) não possuem um sinal unidimensional positivo/negativo definido (`pairwise$signs` retorna `NA` para essas células, pois há múltiplos coeficientes dummy). A multiplicação `wadj * signs` gera `NA` para todas as arestas ligadas a variáveis categóricas. Em seguida, a linha 745 substitui todos os `NA` por `0`, **apagando completamente essas conexões do grafo, da matriz de pesos, das métricas de centralidade e das tabelas do jamovi**.
- **Solução Recomendada:** Utilizar `mgmFit$pairwise$wadj` como matriz ponderada principal de adjacência. O sinal deve modular apenas as arestas contínuas/binárias e orientar as cores das arestas (`edgecolor`), sem zerar arestas multinomial válidas.
- **Impacto da Correção:** Redes mistas passarão a exibir corretamente as conexões entre variáveis categóricas e contínuas que atualmente são omitidas.

---

### [BSS-STAT-002] Conversão inválida de dataframe misto para matriz de texto (`character`)
- **Classificação:** Erro Computacional Crítico de Execução.
- **Localização:** `R/networkanalysis.b.R`, linha 716.
- **Evidência:**
  ```r
  mgm_args <- list(data = as.matrix(currentDataGroup_for_estimation), ...)
  ```
  Quando o usuário inclui variáveis categóricas (fatores) e contínuas (numéricas), a função básica `as.matrix()` do R converte homogeneamente todo o dataframe para uma matriz de texto (`character`). Quando repassada ao `mgm::mgm()`, a execução é abortada com o erro: `ERROR in mgm: Only integer and numeric values permitted`.
- **Solução Recomendada:** Converter as variáveis categóricas (`varsCat_mixed`) para números inteiros indexados a partir de zero (`as.integer(x) - 1L`) antes de formar a matriz numérica para o `mgm`.
- **Impacto da Correção:** Permite que o módulo execute redes mistas com variáveis categóricas sem falha de execução.

---

### [BSS-STAT-003] Divergência na validação do número mínimo de nós para o modelo MGM
- **Classificação:** Inconsistência de Validação de Dados.
- **Localização:** `R/networkanalysis.b.R`, linhas 556-558 e `jamovi/networkanalysis.u.yaml`.
- **Evidência:**
  ```r
  allVarNames <- c(varsContNames, varsCatNames, varsCountNames)
  if (length(allVarNames) < 2) {
    jmvcore::reject("Please select at least 2 variables in total.", code = "not_enough_variables"); return()
  }
  ```
  O código valida um mínimo de 2 variáveis, porém o pacote `mgm::mgm()` exige estritamente no mínimo 3 variáveis (`At least 3 variables required`), provocando crash interno quando o usuário seleciona apenas 2 nós.
- **Solução Recomendada:** Ajustar a validação precoce no `.runMixed` para exigir `length(allVarNames) < 3`, exibindo mensagem amigável no jamovi.
- **Impacto da Correção:** Previne interrupção abrupta da interface.

---

### [BSS-STAT-004] Vulnerabilidade a matrizes não-definidas positivas (non-PD) em deleção pairwise (GGM)
- **Classificação:** Robustez Algorítmica e Inferencial.
- **Localização:** `R/networkanalysis.b.R`, linhas 198 a 313.
- **Evidência:**
  A deleção pairwise em dados com valores ausentes pode gerar matrizes de correlação com autovalores negativos residuais. O `bootnet::estimateNetwork` com `EBICglasso` rejeita a estimação com `matrix is not positive definite`, abortando toda a análise do grupo.
- **Solução Recomendada:** Aplicar suavização para a matriz definida positiva mais próxima (`Matrix::nearPD` ou parâmetro `npd = "nearPD"` de `qgraph::cor_auto`) caso a matriz empírica não seja positiva definida, inserindo nota explicativa no rodapé da tabela de sumário.
- **Impacto da Correção:** Aumenta a taxa de convergência e robustez para datasets biomédicos reais com dados faltantes.

---

### [BSS-CODE-001] Resíduos de mensagens de depuração no console do R
- **Classificação:** Qualidade de Código e Padrão GitHub.
- **Localização:** `R/networkanalysis.b.R`, linhas 11, 92, 102 e 535.
- **Evidência:**
  Presença de `message("networkanalysisClass$.init() called")`, `message("networkanalysisClass$.run() called")`, etc.
- **Solução Recomendada:** Remoção integral dessas chamadas.

---

### [BSS-CODE-002] Omissão das referências bibliográficas obrigatórias nos metadados YAML
- **Classificação:** Transparência Científica e Conformidade com o Protocolo (Seção 7).
- **Localização:** `jamovi/networkanalysis.a.yaml`.
- **Evidência:** Seção `refs:` inexistente no arquivo YAML.
- **Solução Recomendada:** Adicionar formalmente no arquivo `.a.yaml`:
  - `qgraph` (Epskamp et al., 2012)
  - `bootnet` (Epskamp, Borsboom & Fried, 2018)
  - `mgm` (Haslbeck & Waldorp, 2020)
  - `EBICglasso` (Foygel & Drton, 2010)
  - `ExpectedInfluence` (Robinaugh et al., 2016)

---

### [BSS-CODE-003] Falta de orientação sobre a sintaxe da variável de grupos de nós (`nodeGroupVar`)
- **Classificação:** Usabilidade e Engenharia de Interface.
- **Localização:** `R/networkanalysis.b.R`, linhas 162-185 e `jamovi/networkanalysis.u.yaml`.
- **Evidência:** O parser requer que as observações estejam no formato textual `Variavel = Grupo`. Usuários que inserem fatores convencionais têm o agrupamento silenciosamente ignorado.
- **Solução Recomendada:** Inserir nota explicativa na interface e no bloco `.init()` orientando sobre o formato aceito, emitindo aviso informativo caso a variável não contenha o caractere `=`.

---

## 3. Matriz Sintética de Propostas de Correção

| ID do Ticket | Tipo | Severidade | Arquivos Afetados | Descrição Resumida |
|---|---|---|---|---|
| **`BSS-STAT-001`** | Científico/Matemático | CRÍTICA | `R/networkanalysis.b.R` | Correção da matriz de adjacência em redes mistas (impedir anulação de arestas categóricas por multiplicação com `signs` NA). |
| **`BSS-STAT-002`** | Computacional | CRÍTICA | `R/networkanalysis.b.R` | Conversão correta de fatores para inteiros em matriz numérica para o `mgm`. |
| **`BSS-STAT-003`** | Validação | ALTA | `R/networkanalysis.b.R` | Validação de mínimo de 3 nós para redes mistas (`mgm`). |
| **`BSS-STAT-004`** | Estatístico | MÉDIA | `R/networkanalysis.b.R` | Suavização `nearPD` para correlações não-definidas positivas com dados faltantes. |
| **`BSS-CODE-001`** | Código Limpo | BAIXA | `R/networkanalysis.b.R` | Remoção de chamadas `message()` de depuração e ajuste do cabeçalho R6. |
| **`BSS-CODE-002`** | Citações | MÉDIA | `jamovi/networkanalysis.a.yaml` | Inclusão de referências científicas completas na chave `refs:`. |
| **`BSS-CODE-003`** | Interface/Docs | BAIXA | `R/networkanalysis.b.R`, `jamovi/networkanalysis.u.yaml` | Orientação e validação para o campo `nodeGroupVar`. |

---

*Relatório de auditoria independente gerado e validado em conformidade com o Protocolo BSS.*
