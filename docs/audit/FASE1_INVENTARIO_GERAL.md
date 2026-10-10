# RELATÓRIO DE AUDITORIA — FASE 1: INVENTÁRIO GERAL DO MÓDULO BSS

- **Projeto:** BSS (Biostatistics Suite for jamovi)
- **Versão sob Auditoria:** 0.2.0 (Baseline)
- **Branch de Trabalho:** `refactor/audit-v0.3.0`
- **Repositório Oficial:** [github.com/rlambertucci/BSS](https://github.com/rlambertucci/BSS)
- **Autor / Mantenedor:** Rafael Lambertucci (`rlambertucci@unifesp.br`)
- **Data do Inventário:** 10 de outubro de 2026
- **Status da Fase 1:** CONCLUÍDA

---

## 1. Identificação Geral e Arquitetura

O módulo **BSS** é um pacote para o software estatístico jamovi estruturado segundo a especificação `jmvcore` / R6. Ele reúne 10 análises estatísticas distintas distribuídas em 4 grupos funcionais de menu.

### 1.1 Metadados do Pacote (`DESCRIPTION`)
- **Package:** BSS
- **Type:** Package
- **Version:** 0.2.0
- **License:** GPL (>= 2)
- **Encoding:** UTF-8
- **LazyData:** true
- **Imports Declarados (25 dependências):** `jmvcore (>= 0.8.5)`, `R6`, `ggplot2`, `gridExtra`, `missRanger`, `GGally`, `VIM`, `effsize`, `effectsize`, `bootnet`, `qgraph`, `reshape2`, `gtools`, `igraph`, `mgm`, `mice`, `lattice`, `miceadds`, `bestNormalize`, `SuppDists`, `stats`, `dplyr`, `tidyr`, `ARTool`, `emmeans`.

---

## 2. Inventário Estrutural dos Arquivos

```
H:/My Drive/Vibe Coding/BSS/
├── DESCRIPTION
├── LICENSE
├── NAMESPACE
├── README.md
├── .gitignore                      [Configurado na Rodada 1]
├── docs/
│   └── audit/
│       └── FASE1_INVENTARIO_GERAL.md
├── tests/
│   └── validation/                 [Estrutura de testes criada]
├── R/                              [Códigos de backend e gerados]
│   ├── art.b.R                     (344 linhas)
│   ├── art.h.R                     (gerado - 482 linhas)
│   ├── correlationcomparison.b.R   (192 linhas)
│   ├── correlationcomparison.h.R   (gerado - 267 linhas)
│   ├── dataimputation.b.R          (859 linhas)
│   ├── dataimputation.h.R          (gerado - 1223 linhas)
│   ├── datapresentation.b.R        (775 linhas)
│   ├── datapresentation.h.R        (gerado - 1058 linhas)
│   ├── datatransform.b.R           (162 linhas)
│   ├── datatransform.h.R           (gerado - 349 linhas)
│   ├── dda.b.R                     (332 linhas)
│   ├── dda.h.R                     (gerado - 363 linhas)
│   ├── effectsize.b.R              (952 linhas)
│   ├── effectsize.h.R              (gerado - 709 linhas)
│   ├── networkanalysis.b.R         (1373 linhas)
│   ├── networkanalysis.h.R         (gerado - 1011 linhas)
│   ├── outliers.b.R                (995 linhas)
│   ├── outliers.h.R                (gerado - 665 linhas)
│   ├── samplesize.b.R              (237 linhas)
│   └── samplesize.h.R              (gerado - 269 linhas)
└── jamovi/                         [Definições YAML de interface e saída]
    ├── 0000.yaml                   (definição dos menus principais)
    ├── art.a.yaml, .r.yaml, .u.yaml
    ├── correlationcomparison.a.yaml, .r.yaml, .u.yaml
    ├── dataimputation.a.yaml, .r.yaml, .u.yaml
    ├── datapresentation.a.yaml, .r.yaml, .u.yaml
    ├── datatransform.a.yaml, .r.yaml, .u.yaml
    ├── dda.a.yaml, .r.yaml, .u.yaml
    ├── effectsize.a.yaml, .r.yaml, .u.yaml
    ├── networkanalysis.a.yaml, .r.yaml, .u.yaml
    ├── outliers.a.yaml, .r.yaml, .u.yaml
    └── samplesize.a.yaml, .r.yaml, .u.yaml
```

**Volume Total de Código Próprio (`.b.R`):** 6.221 linhas de código R.  
**Volume Total de Código Gerado (`.h.R`):** 6.389 linhas de classes R6 base.

---

## 3. Matriz das 10 Análises Estatísticas

| # | Análise (ID) | Nome da Classe R6 | Submenu no jamovi | Linhas (`.b.R`) | Pacotes R Efetivos | Escopo Metodológico Principal |
|---|---|---|---|---|---|---|
| 1 | `networkanalysis` | `networkanalysisClass` | Associations & Networks | 1.373 | `bootnet`, `qgraph`, `mgm`, `igraph`, `ggplot2`, `dplyr`, `reshape2`, `gtools` | Redes de correlação parcial regularizada para variáveis contínuas (GGM/EBICglasso) e mistas (MGM). Métricas de centralidade e clustering. |
| 2 | `outliers` | `outliersClass` | Data Preparation & Screening | 995 | `ggplot2`, `gridExtra`, `stats` | Detecção univariada de outliers por Teste Q de Dixon, Amplitude Interquartil (IQR) e Escore Z Modificado (MAD), com opções de winsorização/remoção. |
| 3 | `effectsize` | `effectSizeClass` | Nonparametric ANOVA, Effect sizes & Post-Hocs | 952 | `effectsize`, `emmeans`, `stats` | Estimadores padronizados de tamanho de efeito (d de Cohen, g de Hedges, delta de Glass, r bisserial, eta², omega², epsilon²) para delineamentos independentes e pareados. |
| 4 | `dataimputation` | `dataimputationClass` | Data Preparation & Screening | 859 | `mice`, `miceadds`, `missRanger`, `VIM`, `GGally`, `ggplot2` | Imputação múltipla multivariada por equações encadeadas (MICE) e não-paramétrica por florestas aleatórias (missRanger). Diagnóstico gráfico de padrões faltantes. |
| 5 | `datapresentation` | `DataPresentationClass` | Study Design & Presentation | 775 | `ggplot2`, `dplyr`, `tidyr`, `emmeans` | Visualização gráfica avançada (gráficos de barra com pontos, boxplots, gráficos de dispersão-média) e tabelas descritivas automáticas (normas APA, ABNT, Vancouver, Chicago). |
| 6 | `art` | `artClass` | Nonparametric ANOVA, Effect sizes & Post-Hocs | 344 | `ARTool`, `stats` | Transformação de postos alinhados (Aligned Rank Transform) para delineamentos fatoriais e mistos não-paramétricos, com contrastes post hoc. |
| 7 | `dda` | `DDAClass` | Nonparametric ANOVA, Effect sizes & Post-Hocs | 332 | `MASS`, `effsize`, `ggplot2`, `stats` | Análise Discriminante Descritiva (DDA) post hoc para MANOVA multivariada, com cálculo de razões canônicas e tamanhos de efeito multivariados. |
| 8 | `samplesize` | `samplesizeClass` | Study Design & Presentation | 237 | `stats` | Cálculo de tamanho amostral para populações finitas e planejamento de experimentos com modelos animais baseado na Resource Equation Approach (Mead). |
| 9 | `correlationcomparison` | `correlationComparisonClass` | Associations & Networks | 192 | `stats` | Testes de significância para comparação de correlações independentes (transformação z de Fisher) e dependentes (Williams / Steiger). |
| 10 | `datatransform` | `dataTransformClass` | Data Preparation & Screening | 162 | `bestNormalize`, `SuppDists`, `ggplot2`, `gridExtra`, `stats` | Transformações para normalização (Box-Cox, Yeo-Johnson, transformações polinomiais e Johnson) com avaliação gráfica antes/depois. |

---

## 4. Auditoria Cruzada de Dependências (Declaradas vs. Utilizadas)

Confrontamos a lista de `Imports` do arquivo `DESCRIPTION` com as invocações explícitas de namespace e carregamentos no código fonte:

### 4.1 Omissões Críticas (Utilizados no Código, mas NÃO Declarados no `DESCRIPTION`)
- **`MASS`**: Chamado explicitamente em `dda.b.R:61` (`MASS::lda(formula, data = data)`), porém **não consta em `Imports`**. Em ambientes onde `MASS` não estiver previamente carregado pelo namespace pai, a análise DDA falha.
- **`utils`**: Utilizado em `dda.b.R`, `dataimputation.b.R` e `networkanalysis.b.R`. (Pertence ao R base, recomendável declarar ou garantir carregamento limpo).
- **`graphics` e `grDevices`**: Utilizados diretamente em plots básicos em `dataimputation.b.R`, `networkanalysis.b.R` e `datapresentation.b.R`.

### 4.2 Dependências Órfãs (Declaradas no `DESCRIPTION`, mas NÃO Utilizadas nos `.b.R`)
- **`lattice`**: Declarado em `Imports`, mas nenhuma função ou namespace de `lattice` é chamado em nenhum arquivo do projeto. Representa overhead desnecessário de instalação.
- **`effsize` vs. `effectsize`**: Ambos estão declarados. `effsize` é chamado em `dda.b.R` (`effsize::cohen.d`), enquanto `effectsize` é chamado em `effectsize.b.R`. Há sobreposição metodológica onde o pacote moderno `effectsize` cobre a totalidade das rotinas de `effsize`.

---

## 5. Diagnósticos Preliminares de Qualidade e Conformidade

1. **Ausência Universal de Citações YAML (`refs:`)**:
   - Nenhum dos 10 arquivos `.a.yaml` possui a seção `refs:` preenchida. O protocolo exige citações dos pacotes R na interface do jamovi; atualmente, ou não aparecem ou estão embutidas como strings HTML avulsas dentro de `.b.R`.
2. **Mensagens de Depuração Ativas no Console**:
   - Foram identificadas chamadas `message()` deixadas no código de produção em `networkanalysis.b.R` (linhas 11, 92, 102 e 535), poluindo o log do usuário no jamovi.
3. **Inconsistência nos Cabeçalhos R6**:
   - Variação no padrão de verificação de namespace nas classes:
     - `if (requireNamespace('jmvcore', quietly=TRUE))` (adotado na maioria)
     - `if (requireNamespace('jmvcore'))` sem `quietly=TRUE` em `networkanalysis.b.R`
     - Sem checagem condicional em `outliers.b.R`, `samplesize.b.R` e `correlationcomparison.b.R`.
4. **Tratamento de Exceções**:
   - Algumas rotinas utilizam `tryCatch` silencioso retornando `character(0)` ou dados nulos, o que pode mascarar falhas matemáticas e gerar tabelas vazias sem aviso explicativo ao usuário.

---

## 6. Fila de Execução da Auditoria Aprofundada

Conforme alinhado nas Rodadas de Decisão, a auditoria será conduzida análise por análise, começando pelos módulos multivariados de maior complexidade:

```
[FASE 1: INVENTÁRIO GERAL] (Concluída)
       │
       ▼
[CICLO 1] ➔ networkanalysis (MGM, GGM, bootnet, qgraph)
       │
[CICLO 2] ➔ dataimputation (MICE, missRanger, VIM)
       │
[CICLO 3] ➔ dda (MANOVA, DDA, lda)
       │
[CICLO 4] ➔ art (ARTool, post hocs)
       │
[CICLO 5] ➔ effectsize (Cohen's d, Hedges' g, eta², omega²)
       │
[CICLO 6] ➔ samplesize (Resource Equation, Populações finitas)
       │
[CICLO 7] ➔ outliers (Dixon Q, MAD, IQR)
       │
[CICLO 8] ➔ correlationcomparison (Fisher z, Steiger)
       │
[CICLO 9] ➔ datapresentation (ggplot2, ABNT/APA)
       │
[CICLO 10] ➔ datatransform (bestNormalize, Box-Cox)
       │
       ▼
[FASE 11: PARECER CONSOLIDADO FINAL & PREPARAÇÃO GITHUB]
```

---

*Relatório gerado em conformidade com o Protocolo de Auditoria BSS.*
