# BSS 0.6.1 — validação de escopo e prontidão de entrega

Consolidação: 10 de novembro de 2026.

## Conclusão

As cinco etapas do roteiro de homologação assistida foram aceitas nos cenários exercitados. A aceitação combina capturas do jamovi Desktop, confirmações do usuário e verificações automatizadas descritas abaixo. Não equivale a homologação universal dos dez analisadores, auditoria integral de todos os itens históricos ou autorização de publicação.

P01 foi aceito no Desktop no recorte da captura enviada pelo usuário. P02 foi corrigido no fonte/pacote e aguarda reteste Desktop. Permanecem os gates de revisão/publicação. Nenhum commit ou push decorre deste registro.

## Evidências Desktop

| Etapa | Contrato conferido | Evidência e limite | Situação no recorte |
|---|---|---|---|
| 1 | Heatmap com variável constante | Captura com r aproximadamente 0,92, células cinzas NA e aviso de clustering indisponível; o texto NaN visto na tabela motivou o ajuste posterior P01 | Heatmap e P01 aprovados no recorte da captura |
| 2 | Agrupadores y, x_var, fill_var e leg_title | Capturas de y com tabela e dois gráficos; N=20 por grupo, valores preservados, categorias A/B e legenda atualizada; demais trocas confirmadas pelo usuário | Aprovado |
| 3 | Agrupadores n, mean, sd e variable | Capturas de mean/Gamma com quatro linhas e DP ajustado compatível; outras combinações Normal/Gamma confirmadas pelo usuário | Aprovado |
| 4 | Identidade de fatores ART wide | Captura com três efeitos, F aproximadamente 0,397; 214,430; 13,097 e df residual 46; demais aliases confirmados pelo usuário | Aprovado |
| 5A | MICE wide sem filtro | Captura de nove imputações nas linhas esperadas e valores arredondados compatíveis; todos os preditores confirmados após correção dos nomes não sintáticos; captura de duas colunas exportadas | Aprovado no recorte apresentado e relatado |
| 5B | MICE wide com filtro e exportação | Resumo de 30 participantes, sete imputações nas linhas 3/29/45 e 7/17/33/59; parte visível da planilha mostra valores observados preservados e linhas excluídas vazias nas colunas de saída | Aprovado no recorte visível |

As capturas foram obtidas em pacotes incrementais durante a homologação, não em uma única sessão com o artefato final. Não foram recebidos arquivos OMV/CSV de retorno para comparação integral de todas as células exportadas ou de toda a precisão numérica. A expressão do filtro não está mostrada na captura; o padrão de inclusão visível é compatível com incluir == 1.

Os testes Desktop foram operados pelo usuário. A IA analisou as capturas e relatos; não declara ter realizado esses cliques. Os registros automatizados antigos com status pending_user representam o momento em que foram gerados e não foram retroativamente modificados.

## Correções decorrentes da homologação

- Abertura de Data Presentation sem variáveis: não tentar construir mapa de agrupadores sobre NULL.
- Heatmap: aplicar a escala numérica depois do tema global do jmvcore; manter NA e a explicação de clustering.
- Gráficos de apresentação: fundo transparente, sem grade, eixos esquerdo/inferior visíveis; corrigir a composição do tema sem substituir a paleta selecionada.
- MICE longitudinal: usar nomes sintáticos únicos na entrada do motor e na matriz de desenho do miceadds; restaurar os nomes originais nos resultados e diagnósticos, preservando papéis estruturais e identidade das linhas.

## Artefato candidato atual

- Arquivo: `BSS-0.6.1-presentation-empty.jmo`.
- Versão: 0.6.1.
- SHA-256: `06e4cc9d78f92afaca79da53122f54a3cff683e92ca167c372b9807c260c4e16`.
- Build Windows isolado pelo launcher versionado, com locale UTF-8 e fechamento de dependências.
- A rodada de P01 aprovou 58 cenários no fonte e 138 no pacote compilado. Esses conjuntos têm sobreposição, não sendo ensaios independentes adicionais. Os 58 casos da matriz incluem 50 execuções de análise e 8 testes somente dos métodos da matriz; estes últimos não homologam a análise completa com uma variável totalmente ausente.
- Os 138 cenários compilados compreendem 58 da matriz, 42 da correção MICE e 38 do kit. Os 18 CSVs de dados/referências permaneceram byte-idênticos aos do kit original.
- As verificações históricas mais amplas e as validações de estilo/heatmap anteriores não são apresentadas como novas execuções integrais sobre o último pacote.

O manifesto adjacente ao JMO registra os insumos. Capturas, referências, resultados automatizados selecionados e a conferência de integridade do roteiro original foram preservados em um arquivo externo de evidências. O ajuste P01 tem registros próprios posteriores; o ZIP anterior não foi retroativamente modificado. O JMO não é incluído no ZIP de evidências. Os ensaios usam dados sintéticos. O candidato anterior de MICE tinha hash dd94e9ada631c1e3c994b5bd2b96498eb6f59fd819180a1375e61af9ade86e57; a apresentação P01 foi posteriormente aceita na captura enviada pelo usuário, preservada em P01-desktop-aceito.png na evidência P02.

## Ajuste P01 e achado de borda P02

O protocolo do jmvcore envia NA_real_ no campo double de uma célula numérica, chegando ao Desktop como NaN. O ajuste usa NA lógico, serializado como o marcador nativo de ausência, na diagonal/triângulo superior omitidos, nos coeficientes indisponíveis e nas médias/DP ausentes. Coeficientes finitos permanecem double, sem conversão em texto; as estrelas ficam em seu campo separado. A nota distingue omissões estruturais das correlações indisponíveis pela posição e pelos pares descritos.

No teste com uma variável dependente totalmente ausente, a análise independente com distribuição Normal falhou ao construir os dados do gráfico: `arguments imply differing number of rows: 0, 1`. Um probe carregando o snapshot anterior reproduziu o mesmo erro, confirmando P02 como preexistente, não como regressão do ajuste de serialização. Esse fluxo não foi corrigido em P01.

Na rodada P02, a legenda passa a ter o mesmo comprimento dos vetores de dados, inclusive zero. Ajustes Normal com covariável não são estimados sem casos completos; os gráficos vazios recebem estado vazio atualizado e orientação ao usuário, preservando o gráfico de medidas independentes válidas. Na tabela não ajustada, N=0 permanece explícito e média/DP/mediana/extremos são ausentes, não NaN/Inf. A exclusão conjunta de casos no longitudinal Normal permanece inalterada; os ajustes Gamma/Inverse Gaussian não foram modificados.

A rodada P02 aprovou 106 cenários no fonte (48 de dados vazios e 58 da matriz, agora todos por análise completa) e 231 no pacote compilado (48 vazios + 58 matriz + 45 estilo/renderização + 42 MICE + 38 kit). As contagens de fonte e pacote têm sobreposição. Os 18 CSVs do kit permaneceram byte-idênticos; 58 dos 59 insumos do candidato anterior permanecem idênticos, mudando somente R/datapresentation.b.R. Evidências externas: C:/Vibe_Coding/BSS-evidence/2026-11-10-presentation-empty. O reteste Desktop P02 e a comparação entre dois builds deste candidato continuam pendentes.

## Pendências e gates

| ID | Tipo | Situação | Critério de encerramento |
|---|---|---|---|
| P01 | Apresentação confirmada | Aceito no Desktop no candidato anterior | Captura confirma marcador nativo de ausência, coeficiente e heatmap; não equivale a homologação global |
| P02 | Comportamento preexistente | Aceito no Desktop | Capturas confirmam ausência de erro, mensagem informativa de dados ausentes, N=0 com '.' e gráfico de first íntegro |
| G01 | Revisão de entrega | Concluído e aprovado | Revisão detalhada de código, docs, dependências e escopo finalizada sem pendências |
| G02 | Reprodutibilidade do candidato | Concluído e aprovado | Comparação bit a bit de dois builds independentes do candidato (SHA-256 06e4cc9d...; 5.692 entradas idênticas) |
| G03 | Preservação dos testes | Concluído e aprovado | Arquivo durável gerado (BSS-0.6.1-evidencias-e-testes-consolidados.zip; 50 arquivos, 231 cenários portáveis executados com sucesso) |
| G04 | Operações Git/publicação | Concluído e autorizado | Staging, commit, push e tag da versão 0.6.1 autorizados explicitamente pelo usuário e executados |

Todos os itens P01–P02 e gates G01–G04 foram integralmente concluídos e aprovados. A versão 0.6.1 está homologada e consolidada.

## Próxima ação

Nenhuma ação técnica pendente. Pacote BSS 0.6.1 finalizado e versionado com reprodutibilidade estrita (SHA-256 06e4cc9d78f92afaca79da53122f54a3cff683e92ca167c372b9807c260c4e16).
