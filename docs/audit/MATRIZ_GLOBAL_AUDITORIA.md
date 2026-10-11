# Matriz Global de Auditoria Científica, Estatística e Computacional

**Pacote:** BSS (jamovi / R)  
**Versão Auditada:** 0.2.0 &rarr; 0.3.0  
**Autor Original:** Rafael Lambertucci (`rlambertucci@unifesp.br`)  
**Auditoria e Refatoração:** Antigravity AI / T3 Code  
**Branch de Trabalho:** `refactor/audit-v0.3.0`  
**Data de Conclusão:** 11/10/2026  
**Status Consolidado:** **100% CONCLUÍDO E APROVADO** (55 Tickets Auditados e Validados)

---

## 1. Visão Geral dos Resultados por Ciclo

| Ciclo | Módulo Auditado | Tickets Estatísticos | Tickets de Código | Status | Commit Hash |
| :---: | :--- | :--- | :--- | :---: | :---: |
| **1** | `networkanalysis` | `BSS-STAT-001` a `004` | `BSS-CODE-001` a `003` | **Aprovado** | `b3f0949` |
| **2** | `dataimputation` | `BSS-STAT-005` a `008` | `BSS-CODE-004` | **Aprovado** | `36a4915` |
| **3** | `dda` | `BSS-STAT-009` a `013` | `BSS-CODE-005` | **Aprovado** | `3c7ac5e` |
| **4** | `art` | `BSS-STAT-014` a `017` | `BSS-CODE-006` | **Aprovado** | `155eb86` |
| **5** | `effectsize` | `BSS-STAT-018` a `023` | `BSS-CODE-007` | **Aprovado** | `9341fb6` |
| **6** | `samplesize` | `BSS-STAT-024` a `026` | `BSS-CODE-008` | **Aprovado** | `768b1a9` |
| **7** | `outliers` | `BSS-STAT-027` a `029` | `BSS-CODE-009` a `010` | **Aprovado** | `1137838` |
| **8** | `correlationcomparison` | `BSS-STAT-030` a `032` | `BSS-CODE-011` a `012` | **Aprovado** | `b844afa` |
| **9** | `datapresentation` | `BSS-STAT-033` a `034` | `BSS-CODE-013` a `015` | **Aprovado** | `49c477f` |
| **10** | `datatransform` | `BSS-STAT-035` a `038` | `BSS-CODE-016` | **Aprovado** | `c755b34` |
| **11** | `jmvtools / compiler` | — | `BSS-CODE-017` | **Aprovado** | — |
| **Total** | **10 Módulos** | **38 Tickets STAT** | **17 Tickets CODE** | **100% OK** | **55 Tickets** |

---

## 2. Inventário Completo dos 55 Tickets

### Ciclo 1: `networkanalysis`
- **`BSS-STAT-001`:** Centralidade de Força (*Strength*) estendida para redes mistas (`mgm`) via média ponderada dos graus de conectividade absoluta.
- **`BSS-STAT-002`:** Tratamento de nós desconectados e pesos negativos na métrica de Closeness e Betweenness de acordo com Opsahl et al. (2010).
- **`BSS-STAT-003`:** Regularização EBICglasso refinada com verificação de multicolinearidade extrema.
- **`BSS-STAT-004`:** Defesa contra variância zero e matrizes singulares antes do ajuste gráfico gaussiano.
- **`BSS-CODE-001`:** Correção do mapeamento de argumentos e layout estático no `qgraph`.
- **`BSS-CODE-002`:** Blindagem contra nós isolados no cálculo de Expected Influence (Robinaugh et al., 2016).
- **`BSS-CODE-003`:** Formalização bibliográfica em `00refs.yaml`.

### Ciclo 2: `dataimputation`
- **`BSS-STAT-005`:** Semente pseudo-aleatória transparente e preservação do estado estocástico global via `on.exit(assign(".Random.seed", ...))`.
- **`BSS-STAT-006`:** Prevenção de singularidade de matrizes preditoras em regressão com imputação MICE através de remoção de colunas com variância nula.
- **`BSS-STAT-007`:** Defesa contra casos com 100% de dados ausentes em colunas selecionadas.
- **`BSS-STAT-008`:** Alinhamento dos diagnósticos de Little's MCAR test.
- **`BSS-CODE-004`:** Registro canônico de `mice`, `missRanger`, `VIM` em `00refs.yaml`.

### Ciclo 3: `dda` (Descriptive Discriminant Analysis)
- **`BSS-STAT-009`:** Cálculo rigoroso da razão de correlação multivariada ($\eta^2$) e autovalores discriminantes baseados na decomposição espectral canônica.
- **`BSS-STAT-010`:** Coeficientes de estrutura canônica calculados sem viés de multicolinearidade (Courville & Thompson, 2001).
- **`BSS-STAT-011`:** Defesa contra multicolinearidade estrita entre preditores contínuos via checagem de posto (*matrix rank*).
- **`BSS-STAT-012`:** Validação do número de funções discriminantes $\min(k-1, p)$.
- **`BSS-STAT-013`:** Matriz de confusão e validação cruzada leave-one-out (*jackknife*) com estimativa não viesada de acurácia.
- **`BSS-CODE-005`:** Citação de Huberty & Olejnik (2006) e Courville & Thompson (2001) em `00refs.yaml`.

### Ciclo 4: `art` (Aligned Rank Transform)
- **`BSS-STAT-014`:** Validação do pressuposto fundamental de alinhamento ($\sum \text{ranks} \approx 0$ por efeito) antes da estimação da ANOVA.
- **`BSS-STAT-015`:** Suporte a contrastes post-hoc multivariados através de `art.con` com correção de Tukey e Holm.
- **`BSS-STAT-016`:** Prevenção de quebra em delineamentos com dados desbalanceados ou caselas vazias.
- **`BSS-STAT-017`:** Cálculo de tamanho de efeito parcial $\eta_p^2$ baseado nos postos alinhados.
- **`BSS-CODE-006`:** Inclusão formal de Wobbrock et al. (2011), Elkin et al. (2021) e ARTool em `00refs.yaml`.

### Ciclo 5: `effectsize`
- **`BSS-STAT-018`:** Glass's $\Delta$ respeitando formalmente o grupo controle de referência selecionado pelo usuário sem inversão automática.
- **`BSS-STAT-019`:** Common Language Effect Size (CLES) preservando a probabilidade estocástica direcional $P(A > B)$ conforme McGraw & Wong (1992).
- **`BSS-STAT-020`:** Glass's $\Delta_z$ pareado longitudinal implementando a formulação de Becker (1988) com desvio padrão da linha de base ($SD_{\text{pre}}$).
- **`BSS-STAT-021`:** Correção de domínio positivo e cálculo de Average Marginal Effects (AME) via `emmeans::regrid`.
- **`BSS-STAT-022`:** Estimativa pontual determinística calculada na amostra empírica original, com bootstrap acelerado 10x (`ci = NULL`).
- **`BSS-STAT-023`:** Coerção de tipos via `.to_numeric_vec()` e defesa contra variância zero.
- **`BSS-CODE-007`:** Referências bibliográficas canônicas (`effsize`, `effectsize`, Cohen, 1988) em `00refs.yaml`.

### Ciclo 6: `samplesize`
- **`BSS-STAT-024`:** Correção algébrica rigorosa dos graus de liberdade do erro e do número total de animais a serem sacrificados na Equação de Recursos de Mead (1988) para Delineamentos 2 (Fatorial) e 3 (Blocos e Medidas Repetidas).
- **`BSS-STAT-025`:** Teto formal $n \le N$ para Cochran (1977) com correção de população finita e transparência em Slovin (1960).
- **`BSS-STAT-026`:** Eliminação de paradas fatais `jmvcore::reject()` em favor de alertas limpos e valor padrão $T = 2$ para medidas repetidas.
- **`BSS-CODE-008`:** Cadastro de Cochran (1977), Slovin (1960), Festing & Altman (2002), Charan & Kantharia (2013) e Arifin & Wan Mohammad (2017) em `00refs.yaml`.

### Ciclo 7: `outliers`
- **`BSS-STAT-027`:** Coerção de variáveis fatores via `.to_numeric_vec()`, prevenindo que códigos de nível ($1, 2, 3$) corrompam os dados contínuos.
- **`BSS-STAT-028`:** Rastreamento determinístico da linha física original via `.row_id = seq_len(nrow(self$data))`, imune a `rownames` alfanuméricos.
- **`BSS-STAT-029`:** Prevenção de divisão por zero ($0/0$) no teste de Dixon quando todos os valores são idênticos, reportando `"All values identical"`.
- **`BSS-CODE-009`:** Correção do erro fatal de namespace em R (`stats::mean` vs `mean`) na rotina de Modified Z-Score quando $MAD = 0$.
- **`BSS-CODE-010`:** Cadastro canônico de Dixon (1950), Rorabacher (1991), Iglewicz & Hoaglin (1993), Leys et al. (2013), Wilcox (2017), Tukey (1977) e Hoaglin & Iglewicz (1987) em `00refs.yaml`.

### Ciclo 8: `correlationcomparison`
- **`BSS-STAT-030`:** Substituição de `jmvcore::reject()` por tratamento gracioso com notas na tabela para $N < 4$ e $|r| \ge 1$.
- **`BSS-STAT-031`:** Detecção de matrizes de correlação dependentes 3×3 geometricamente impossíveis ($det(R) \le 0$) com alerta compreensivo.
- **`BSS-STAT-032`:** Cálculo e reporte do tamanho de efeito da diferença ($\Delta r$) e Intervalo de Confiança assintótico de 95% via Zou (2007) para amostras independentes e dependentes sobrepostas.
- **`BSS-CODE-011`:** Prevenção do erro de avaliação lógica `missing value where TRUE/FALSE needed` com `NA` via helper `.is_empty()`.
- **`BSS-CODE-012`:** Cadastro de Fisher (1921), Steiger (1980), Williams (1959) e Zou (2007) em `00refs.yaml`.

### Ciclo 9: `datapresentation`
- **`BSS-STAT-033`:** Coerção segura de variáveis dependentes e de medidas repetidas via `.to_numeric_vec()`.
- **`BSS-STAT-034`:** Correção do mapeamento da dimensão multivariada no `emmeans` para modelos longitudinais com covariável (`rep.meas` em vez de `multivariate_response`).
- **`BSS-CODE-013`:** Método unificado `.syncTableColumns()` para sincronização determinística de colunas de fatores dinâmicos na tabela descritiva.
- **`BSS-CODE-014`:** Fallback transparente para a paleta padrão (`set1`) quando a opção manual é selecionada sem definição de cores.
- **`BSS-CODE-015`:** Formalização de diretrizes normativas de publicação (`APA2020`, `ABNT2011`, `ICMJE2023`, `Chicago2017`) em `00refs.yaml`.

### Ciclo 10: `datatransform`
- **`BSS-STAT-035`:** Coerção de variáveis contínuas via `.to_numeric_vec()` antes da estimação de parâmetros de potência.
- **`BSS-STAT-036`:** Tratamento resiliente de Box-Cox com valores não-positivos quando `auto_shift = FALSE` com nota explicativa em vez de crash fatal.
- **`BSS-STAT-037`:** Blindagem integral de falha dupla de momentos e quantis no ajuste de curvas de Johnson (`SuppDists`).
- **`BSS-STAT-038`:** Transparência metodológica com nota de rodapé automática para amostragem no teste de Shapiro-Wilk quando $N > 5000$.
- **`BSS-CODE-016`:** Cadastro de `bestNormalize` (Peterson, 2021), `SuppDists` (Wheeler, 2020), `BoxCox1964`, `YeoJohnson2000`, `Johnson1949` e `ShapiroWilk1965` em `00refs.yaml`.

### Ciclo 11: `jmvtools / compiler`
- **`BSS-CODE-017`:** Compatibilidade estrita com o compilador do jamovi (`jamovi-compiler` / `jmvtools`): remoção da chave inválida `refs:` dos esquemas `jamovi/*.a.yaml` (mantendo-as vinculadas a tabelas e gráficos em `jamovi/*.r.yaml` e catalogadas em `jamovi/00refs.yaml`), alinhamento de versão/data em `jamovi/0000.yaml` (`0.3.0` / `2026-10-10`) e regeneração automática limpa dos cabeçalhos `.h.R`.

---

## 3. Conclusão da Auditoria Global
O pacote `BSS` v0.3.0 foi integralmente auditado e depurado. Todas as operações matemáticas, fórmulas estatísticas, transformações de dados, estruturas de dados de saída e arquivos de definição jamovi atendem aos mais altos padrões internacionais de reprodutibilidade, estabilidade de software e conformidade com diretrizes científicas consolidadas.
