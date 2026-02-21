[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/9s8Jkpd5)
# Trabalho prático de BCC328 - Construção de Compiladores I

**Alunos:** Kayo Xavier (21.2.4095) e Pedro Vieira (20.2.4008)

## Instruções

### Iniciando o container Docker

```bash
docker-compose up -d
docker-compose exec sl bash
```

### Compilando o projeto

Dentro do container:

```bash
cabal update
cabal build
```

### Executando um programa SL

```bash
cabal run sl-compiler -- --interpret programa.sl
```

### Modos disponíveis

| Modo | Descrição |
|------|-----------|
| `--lexer` | Imprime tokens com posição (linha e coluna) |
| `--parser` | Imprime a AST como árvore |
| `--pretty` | Imprime código-fonte reconstruído |
| `--typecheck` | Executa análise semântica e verificação de tipos |
| `--interpret` | Analisa, verifica tipos e executa o programa |
| `--run` | Alias para `--interpret` |

### Exemplos

```bash
cabal run sl-compiler -- --interpret test/inputs/example1_factorial.sl
cabal run sl-compiler -- --interpret test/inputs/example2_structs.sl
cabal run sl-compiler -- --interpret test/inputs/example3_arrays.sl
cabal run sl-compiler -- --interpret test/inputs/example4_math.sl
cabal run sl-compiler -- --typecheck test/inputs/example5_identity.sl
cabal run sl-compiler -- --typecheck test/inputs/example6_generics.sl
```

### Executando os testes

```bash
cabal test
```

144 testes automatizados cobrindo: lexer, parser, pretty printer, type checker e interpretador.

## Estrutura do projeto

```
workspace/
├── app/Main.hs              # Ponto de entrada do compilador
├── src/SL/
│   ├── AST.hs               # Definição da AST
│   ├── Lexer.hs             # Analisador léxico (Megaparsec)
│   ├── Parser.hs            # Analisador sintático
│   ├── Pretty.hs            # Pretty printer
│   ├── Env.hs               # Ambiente e tabela de símbolos
│   ├── TypeChecker.hs       # Analisador semântico
│   └── Interpreter.hs       # Interpretador
├── test/
│   ├── Spec.hs              # Testes automatizados (HSpec)
│   └── inputs/              # Programas SL de exemplo
├── workspace.cabal           # Configuração do build (Cabal)
relatorio/
├── relatorio.tex             # Relatório em LaTeX
└── relatorio_tp2_compiladores.pdf  # Relatório em PDF
```
