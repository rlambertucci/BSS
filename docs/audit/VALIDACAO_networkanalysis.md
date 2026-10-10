# RELATÓRIO DE REVALIDAÇÃO — CICLO 1: ANÁLISE DE REDES (`networkanalysis`)

- **Análise Revalidada:** `networkanalysis` (Modelos GGM e MGM)
- **Data da Implementação:** 10 de outubro de 2026
- **Branch:** `refactor/audit-v0.3.0`
- **Status:** IMPLEMENTAÇÃO CONCLUÍDA E REVALIDADA COM SUCESSO

---

## 1. Status de Implementação das Correções Autorizadas

| Ticket | Tipo | Status | Resumo da Intervenção |
|---|---|---|---|
| **`BSS-STAT-001`** | Científico/Matemático | **RESOLVIDO** | A matriz de adjacência do MGM agora utiliza `mgmFit$pairwise$wadj`, preservando as conexões de variáveis nominais/multinomial que eram zeradas pela multiplicação por `signs` (`NA`). Sinais são aplicados unicamente a variáveis com sinal unívoco e nas cores das arestas (`edgecolor_cb`). |
| **`BSS-STAT-002`** | Computacional | **RESOLVIDO** | Adicionada conversão explícita de variáveis categóricas (fatores) para inteiros 0-indexados (`as.integer(x) - 1L`) na matriz numérica `mat_for_mgm`, eliminando o erro fatal `Only integer and numeric values permitted`. |
| **`BSS-STAT-003`** | Validação | **RESOLVIDO** | Ajustada a validação de nós no modo misto para exigir no mínimo 3 variáveis (`length(allVarNames) < 3`), em conformidade estrita com o requisito do algoritmo do `mgm`. |
| **`BSS-STAT-004`** | Estatístico/Robustez | **RESOLVIDO** | Implementado fallback defensivo com `nearPD` para projeção da matriz de correlação mais próxima quando a deleção pairwise gera autovalores negativos em GGM, acompanhado de nota técnica no sumário. |
| **`BSS-CODE-001`** | Código Limpo | **RESOLVIDO** | Removidas todas as 4 chamadas `message()` de depuração que poluíam o console do R (`.init()`, `.run()`, `.runContinuous()`, `.runMixed()`) e adicionado `quietly = TRUE` no cabeçalho R6. |
| **`BSS-CODE-002`** | Metadados & Citações | **RESOLVIDO** | Criado o arquivo mestre bibliográfico [`jamovi/00refs.yaml`](../../jamovi/00refs.yaml) e vinculadas as citações canônicas de `qgraph`, `bootnet`, `mgm`, `EBICglasso` e `ExpectedInfluence` em [`jamovi/networkanalysis.a.yaml`](../../jamovi/networkanalysis.a.yaml). |
| **`BSS-CODE-003`** | Usabilidade/Interface | **RESOLVIDO** | Inserida nota informativa defensiva orientando o formato esperado (`VariableName = GroupName`) caso o usuário selecione uma variável para agrupamento de nós sem a sintaxe de mapeamento. |

---

## 2. Testes de Integridade e Ausência de Regressões

1. **Validação Sintática:** O arquivo `R/networkanalysis.b.R` foi testado via `parse()` no R 4.5.2, compilando sem erros sintáticos.
2. **Validação Numérica:** O script `tests/validation/test_networkanalysis.R` executou com sucesso a estimação contínua (EBICglasso) e mista (MGM com variáveis contínuas e categóricas multinomial).
3. **Preservação de Interfaces:** Nenhuma alteração manual foi realizada em classes geradas (`.h.R`), preservando a compatibilidade total com o compilador do jamovi (`jmvtools`).

---

*Ciclo 1 finalizado e certificado em conformidade com o Protocolo BSS.*
