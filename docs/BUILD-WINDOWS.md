# Build isolado e reproduzível do BSS no Windows

## Contrato

O launcher compila uma cópia externa dos fontes atuais, incluindo WIP, sem alterar o checkout, o compilador compartilhado, as bibliotecas de origem, a instalação do Jamovi ou o ambiente global. Instala o pacote somente na biblioteca isolada do build; não instala o módulo no aplicativo, não publica e não faz commit/push.

Com os mesmos fontes, dependências, compilador, runtime R/Jamovi, versão Node, ferramentas de build e época de referência, o fluxo produz JMO e payload byte-idênticos. Esse contrato não é uma promessa de igualdade entre versões distintas de R/Node ou plataformas. Também não substitui testes funcionais nem homologação Desktop/IPC.

O preflight exige locale UTF-8 válido no R incluído no Jamovi. A antiga falha `embedded nul in string` estava associada ao build com `C.UTF-8` inválido no Windows; não era necessário alterar os textos HTML para contorná-la.

## Fechamento de dependências de runtime

O build recusa insumos incompletos antes da compilação e verifica novamente o JMO final extraído antes de copiá-lo para o destino. Ambos os gates restringem as bibliotecas a módulo, `Resources/modules/base/R` e biblioteca padrão do R incluído. Bibliotecas pessoais, a biblioteca do módulo `jmv` e instalações externas não podem satisfazer dependências em runtime.

`scripts/check-runtime-dependencies.R` percorre recursivamente `Depends`, `Imports` e `LinkingTo`, verifica as restrições de versão (inclusive R) e carrega todos os namespaces do fechamento. Registra versões, biblioteca efetivamente selecionada, `Built`, repositório, licença e relações de dependência nos relatórios externos `*-packages.tsv` e `*-edges.tsv`. `Suggests` não é uma obrigação de runtime; funcionalidades opcionais que venham a usá-lo devem ter testes específicos. Não se instalam ou baixam pacotes automaticamente.

O gate do artefato instancia as dez análises instaladas e executa imputação MICE transversal, MICE longitudinal wide/2l.pmm e missRanger com dados e sementes fixos. Verifica ainda a origem de cada namespace carregado. Isso não substitui homologação Desktop, testes de todas as opções ou auditoria de todas as bibliotecas nativas do sistema.

A biblioteca fornecida em `--dependencies` deve conter o fechamento obrigatório ausente da base. Por exemplo, `lme4` é exigido transitivamente por MICE/mitml/jomo e pelos modelos mistos usados por ARTool/miceadds; não basta que exista no módulo `jmv` do computador de build. Como BSS não o chama diretamente, não é necessário duplicar essa dependência transitiva em `Imports` apenas para resolver o empacotamento. Ao preparar uma biblioteca isolada a partir de pacotes já instalados ou arquivos oficiais, registre origem, versões, hashes e compatibilidade do runtime. Nunca resolva uma ausência alterando `.libPaths` do produto ou instalando pacotes na biblioteca pessoal do usuário.

## Pré-requisitos

- Node.js 22 ou superior, Jamovi para Windows com R incluído e toolchain previamente preparada.
- `index.js` do jamovi-compiler e suas dependências Node, incluindo `jszip`.
- Biblioteca R compatível com a versão/arquitetura do R incluído, nomeada como `R4.6.0-x64-win64`. O nome da pasta sozinho não comprova compatibilidade binária.
- Diretórios de trabalho/saída existentes fora do checkout. Não se sobrescrevem artefato ou manifesto existentes, nem se instalam dependências automaticamente.
- Uma época de referência explícita: `--source-date-epoch` ou a variável `SOURCE_DATE_EPOCH`, em segundos Unix inteiros entre 1980-01-01 e 2107-12-31T23:59:58Z, intervalo representável pelo ZIP. O argumento prevalece sobre a variável. Ausência ou valor inválido aborta o build.

Use uma época estável correspondente ao estado de fonte revisado, não a hora de cada execução. Ela altera os timestamps do artefato, não `Version`/`Date` dos fontes ou metadados de release. O manifesto externo registra separadamente a hora real de criação.

## Uso em PowerShell

Preflight, sem necessidade de época:

~~~powershell
node scripts/build-native-safe.cjs --home 'C:/Program Files/jamovi 28.14.0.0' --check
~~~

Build com caminhos de exemplo e época 1793664000, correspondente a 2026-11-03T00:00:00Z:

~~~powershell
node scripts/build-native-safe.cjs --home 'C:/Program Files/jamovi 28.14.0.0' --compiler 'D:/BSS-tools/node_modules/jamovi-compiler/index.js' --dependencies 'D:/BSS-tools/R4.6.0-x64-win64' --work-root 'D:/BSS-builds' --output 'D:/BSS-builds/BSS-first.jmo' --source-date-epoch 1793664000
~~~

Repita com outro destino, por exemplo `BSS-second.jmo`, mantendo insumos e época. Cada execução cria um workspace novo; não depende de reutilizar um diretório de instalação fixo.

O ambiente filho remove `LANG`, `LANGUAGE`, `LC_*` e `ELECTRON_RUN_AS_NODE`. O build fixa `TZ=UTC` e a época somente nos subprocessos; não modifica `process.env` do chamador ou configurações do sistema. Se o R continuar sem UTF-8, o preflight aborta.

## Regeneração dos headers R

Os arquivos `R/*.h.R` são gerados, não editados manualmente. Use o compilador oficial com a normalização mecânica do projeto:

~~~powershell
node scripts/generate-headers.cjs --compiler 'D:/BSS-tools/node_modules/jamovi-compiler/index.js'
node scripts/generate-headers.cjs --compiler 'D:/BSS-tools/node_modules/jamovi-compiler/index.js' --check
~~~

O gerador usa `compiler.js` e `header.template` da toolchain indicada, produz os headers em diretório temporário e remove somente espaços/tabulações no fim das linhas, preservando CRLF. A opção `--check` retorna falha se houver divergência, sem reescrever os fontes. Os YAMLs, arquivos `.b.R`, templates e compilador compartilhado não são alterados. Ao comparar headers com a saída bruta do compilador, aplique a mesma normalização; esta é uma diferença de formatação, não uma dispensa de conferir o conteúdo gerado.

O build oficial pode reemitir a formatação bruta nos seus fontes isolados. Isso não autoriza modificar manualmente os headers do checkout nem altera os métodos estatísticos. A validação desta normalização deve verificar equivalência da AST R, além da ausência de whitespace residual.

## Etapas determinísticas

1. Copiar fontes e dependências para workspace externo. A pasta `inst/` é opcional: sua ausência não impede o build nem exige criar diretórios vazios; quando presente, seus recursos são copiados e inventariados normalmente. Os demais fontes continuam obrigatórios. BSS e `00LOCK*` da biblioteca de origem não são reutilizados. Links simbólicos nos insumos são recusados.
2. Usar os handlers diretamente de `jamovi/js/<analise>.js`, localização nativa esperada pelo compilador oficial. Essa estrutura já deve existir nos fontes e é preservada na cópia isolada; não há migração nem duplicação de handlers pelo launcher. Os eventos declarados nos arquivos `.u.yaml` continuam apontando para os mesmos módulos e funções.
3. Executar o compilador oficial com `--skip-deps` e `--skip-remotes`. Conservar seu JMO bruto e log como evidência.
4. Gerar novamente o lazy-load do BSS a partir dos fontes compilados, usando as rotinas do próprio R, sem editar buffers RDB/RDX. `set.install.dir="BSS"` elimina o caminho aleatório de instalação serializado; o loader do R resolve o caminho real ao carregar o pacote. As APIs internas de `tools` estão vinculadas ao runtime registrado; foram exercitadas com R 4.6.0 e devem ser revalidadas ao atualizar o R.
5. Fixar `Built` em DESCRIPTION/Meta do pacote instalado e `build-time` nos dois YAMLs de módulo na época indicada. Conferir que os demais campos de metadados R não mudaram.
6. Reempacotar com ordem de entradas, datas UTC, atributos DOS e compressão DEFLATE nível 9 fixos. Somente seis arquivos do payload são substituídos: DESCRIPTION/Meta do BSS, os dois bancos lazy-load e os dois YAMLs de módulo. Não se omitem diferenças na comparação.

Não há patch no compilador ou no jmvcore compartilhados. As demais entradas do JMO, inclusive dependências e bundles de UI, conservam seu conteúdo. A regeneração R desabilita retenção de fontes/dados de parse; ela não altera os algoritmos estatísticos.

## Comparação estrita

~~~powershell
node scripts/compare-native-builds.cjs 'D:/BSS-builds/BSS-first.jmo' 'D:/BSS-builds/BSS-second.jmo'
~~~

O comparador confere SHA-256 dos artefatos e inventários dos fontes, dependências e compilador. Nos manifestos do fluxo reproduzível também confere runtime R, bibliotecas base do Jamovi, ferramentas e política de época. Compara todas as entradas descompactadas, sem lista de arquivos ignorados.

Status zero exige **ambos** `artifactIdentical=true` e `payloadIdentical=true`. Diferenças somente nos timestamps/atributos do ZIP também falham, mesmo quando o payload é igual. Os resultados diagnósticos são preservados para distinguir contêiner e conteúdo.

Manifestos históricos continuam comparáveis segundo os inventários que possuíam, sem inventar proveniência retroativa. Compare apenas manifestos locais confiáveis: o caminho do compilador neles é usado para resolver `jszip` e precisa continuar acessível.

## Saídas e limites

- Destino `.jmo` normalizado e manifesto `.jmo.json` com SHA-256, época, inventários e política registrados.
- Workspace `bss-native-build-*` retido com `locale.log`, `build.log`, `normalization.log`, `BSS.jmo` bruto, `BSS-reproducible.jmo` e `manifest.json`.
- Os manifestos externos diferem em hora real e caminhos de trabalho por desenho; eles não integram o JMO e não precisam ser byte-idênticos. O hash do artefato bruto conserva a proveniência da normalização.
- Fontes do checkout e árvores de ferramentas/dependências/runtime são conferidos ao final. Falha retorna status não zero e não publica artefato incompleto como sucesso.
- Logs e manifestos contêm caminhos locais; revise antes de publicar. Arquive a toolchain completa e o runtime, não apenas o hash de `index.js`.

A prova de repetibilidade da intervenção está em `C:/Vibe_Coding/BSS-evidence/2026-11-03-item-100`, com builds independentes sob fusos diferentes no chamador e locale inicialmente inválido. Ela não encerra a homologação de foco/transferência C014 nem a decisão de versão da release.
