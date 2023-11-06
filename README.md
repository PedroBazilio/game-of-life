

# Jogo da Vida de Conway

Esta é uma implementação em Haskell adapatada do Jogo da Vida de Conway. Feito para a aula de Linguagens de Programação da Universidade Federal Fluminense

Alunos: Pedro Henrique e Barbara

## Regras

Nesta implementação, usamos as seguintes regras:
- `1` representa uma célula viva.
- `2` representa uma célula morta.
- `3` representa uma célula zumbi.

O jogo evolui por meio de iterações com base nas seguintes regras:
1. Uma célula viva com menos de 2 vizinhos vivos morre (subpopulação).
2. Uma célula viva com 2 ou 3 vizinhos vivos sobrevive.
3. Uma célula viva com mais de 3 vizinhos vivos morre (superpopulação).
4. Uma célula morta com exatamente 3 vizinhos vivos se torna viva (reprodução).

## Como Executar

1. Para executar o programa, certifique-se de ter o Haskell e o Stack ou Cabal instalado. 
2. Descompate o programa em um diretório de sua escolha. 
3. Para compilar e executar utilize os seguintes comandos:
- Obs.: Certifique-se de estar na pasta do projeto.
#### Stack
```sh
stack build
stack exec exec game-of-life-exe  
```

#### Cabal
```sh
cabal run game-of-life-exe 
```

O programa solicitará o número de interações, o número de linhas e o número de colunas na matriz. Você poderá inserir a matriz inicial, onde usará números separados por espaços para representar as células. O programa simulará o Jogo da Vida de acordo com o número especificado de iterações mostrando passo a passo.

## Uso

1. Insira o número de iterações, linhas e colunas.
2. Insira a matriz inicial, onde você representa as células com números:
   - `1` para células vivas.
   - `2` para células mortas.
   - `3` para células zumbis.
3. O programa simulará o Jogo da Vida e mostrará as matrizes após cada iteração.

## Exemplo de Entrada

Aqui está um exemplo de entrada para uma matriz de 3x3:

```
Número de interações: 5
Número de linhas na matriz: 3
Número de colunas na matriz: 3

Observação: Insira cada linha com números separados por espaços, como mostrado no exemplo.
Exemplo: Matriz 3x3
1 2 1
2 3 1
1 1 2

Comece a inserir a matriz:
2 3 1
3 3 3
3 2 1
```

## Saída

O programa mostrará a matriz em cada interação, e a matriz final será exibida no final do número especificado de interações.
