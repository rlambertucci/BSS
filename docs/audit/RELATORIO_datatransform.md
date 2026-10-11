# Relatório de Auditoria — Ciclo 10: datatransform

**Módulo Auditado:** `dataTransform` (`R/datatransform.b.R`, `jamovi/datatransform.a.yaml`, `jamovi/datatransform.r.yaml`, `jamovi/datatransform.u.yaml`)  
**Data da Auditoria:** 10/10/2026  
**Auditor Responsável:** Antigravity AI / T3 Code  
**Status da Auditoria:** DIAGNÓSTICO CONCLUÍDO — AGUARDANDO AUTORIZAÇÃO DO USUÁRIO

---

## 1. Contexto e Escopo do Módulo
O módulo `dataTransform` executa transformações de potência e de distribuição para normalização de variáveis contínuas:
1. **Transformação Box-Cox (Box & Cox, 1964):** Requer estritamente dados positivos ($X > 0$), com opção de deslocamento automático (*auto shift*) para dados não positivos ($X \le 0$).
2. **Transformação Yeo-Johnson (Yeo & Johnson, 2000):** Suporta valores positivos, nulos e negativos continuamente.
3. **Curvas de Translação de Johnson (Johnson, 1949):** Família de distribuições $S_U$ (ilimitada), $S_B$ (delimitada), $S_L$ (log-normal) e $S_N$ (normal), ajustadas via momentos ou quantis (`SuppDists`).
4. **Avaliação Diagnóstica:** Teste de Shapiro-Wilk antes e após a transformação, gráficos Q-Q e histogramas comparativos.
5. **Persistência de Dados:** Gravação das variáveis transformadas diretamente na planilha de dados do jamovi (`Output`).

---

## 2. Diagnóstico Científico, Estatístico e Computacional

### 2.1. Coerção Segura de Dados Contínuos (`BSS-STAT-035`)
- **Problema:** Se a variável fornecida em `vars` contiver formato de fator ou caractere (ex. dados com separadores numéricos ou importações sem tipagem), operações como `min(x)`, `stats::var(x)` e os algoritmos do `bestNormalize` e `SuppDists` disparam erros de execução em R (`invalid 'type' of argument`).
- **Solução:** Aplicar a função defensiva `.to_numeric_vec()` sistematicamente antes de qualquer filtragem ou modelagem de parâmetros.

### 2.2. Resiliência do Algoritmo Box-Cox com Valores Não-Positivos (`BSS-STAT-036`)
- **Problema:** Quando `auto_shift = FALSE` e os dados contêm valores não-positivos ($\min(X) \le 0$), a linha 79 executa `jmvcore::reject("Box-Cox requires positive values.")`. Isso interrompe abruptamente toda a análise no jamovi com tela vermelha.
- **Solução:** Substituir o `reject()` fatal por uma nota de advertência na tabela de parâmetros e de normalidade (`"Box-Cox requires strictly positive values (X > 0). Enable 'Automatic Shift' or select Yeo-Johnson."`) e preencher as células com `NA`, preservando a integridade da sessão do usuário.

### 2.3. Blindagem de Falha de Ajuste da Curva de Johnson (`BSS-STAT-037`)
- **Problema:** Na linha 86, `jt <- tryCatch(SuppDists::JohnsonFit(x_clean, moment="quant"), error=function(e) SuppDists::JohnsonFit(x_clean, moment="use"))`. Se o método `"use"` também falhar (dados bimodais ou com momentos patológicos), a chamada secundária não está protegida por `tryCatch`, disparando um erro fatal não capturado.
- **Solução:** Envolver ambas as tentativas em proteção defensiva aninhada. Se o ajuste de Johnson falhar, emitir nota explicativa na tabela sem derrubar o módulo.

### 2.4. Transparência na Amostragem de Shapiro-Wilk para Grandes Amostras ($N > 5000$) (`BSS-STAT-038`)
- **Problema:** O teste `stats::shapiro.test` do R impõe limite máximo de $N = 5000$. O módulo adequadamente extrai uma subamostra aleatória de 5000 casos com semente fixada (`12345`), mas não comunica essa amostragem ao usuário na tabela de resultados.
- **Solução:** Adicionar nota de rodapé automática na tabela de normalidade quando $N > 5000$, esclarecendo que o teste foi calculado sobre uma subamostra aleatória representativa de $N = 5000$.

### 2.5. Cadastro Canônico das Referências em `00refs.yaml` e YAMLs (`BSS-CODE-016`)
- **Problema:** As referências citadas no código e referenciadas no `.r.yaml` (`bestNormalize`, `SuppDists`, `BoxCox1964`, `YeoJohnson2000`, `Johnson1949`, `ShapiroWilk1965`) não constam no banco central `jamovi/00refs.yaml` e faltam no `.a.yaml`.
- **Solução:** Cadastrar todas as referências no `00refs.yaml` e mapear em `datatransform.a.yaml`.

---

## 3. Tickets de Correção Propostos

| ID do Ticket | Tipo | Severidade | Descrição Resumida |
| :--- | :--- | :--- | :--- |
| **BSS-STAT-035** | Estatística / Dados | Alta | Coerção defensiva sistemática de variáveis contínuas via `.to_numeric_vec()`. |
| **BSS-STAT-036** | Usabilidade / Resiliência | Alta | Substituição de `jmvcore::reject()` em dados não positivos de Box-Cox por notas informativas amigáveis. |
| **BSS-STAT-037** | Robustez Numérica | Média | Tratamento defensivo integral de falha de ajuste de curvas de Johnson (`SuppDists`). |
| **BSS-STAT-038** | Rigor Metodológico | Baixa | Transparência científica com nota de rodapé automática para amostragem no teste de Shapiro-Wilk ($N > 5000$). |
| **BSS-CODE-016** | Documentação | Baixa | Cadastro formal de `bestNormalize`, `SuppDists`, `BoxCox1964`, `YeoJohnson2000`, `Johnson1949` e `ShapiroWilk1965` em `jamovi/00refs.yaml` e `.a.yaml`. |

---

## 4. Solicitação de Autorização
Nenhum arquivo de código ou YAML foi modificado nesta etapa. Aguardando autorização explícita do usuário para implementar as correções do Ciclo 10 (`datatransform`).
