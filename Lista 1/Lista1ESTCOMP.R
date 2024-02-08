#ALUNOS:
#VITOR SANTINI BESSA (11821BCC010)
#GUSTAVO MELO DO CARMO (11721BCC035)

#Exercicio 1
#a
vetor1 <- seq(10,30)
vetor1

#b
vetor2 <- seq(30,10)
vetor2

#c
vetorAux1 <- c(vetor1,vetor2)
vetor3 <- vetorAux1[-22]
vetor3

#Exercicio 2
#a
rep(c(2,4,6,8),times=10)

#b
vetorAux3 <- rep(c(2,4,6,8),times=10)

vetor5 <- c(vetorAux3,2)
vetor5

#c
vetororiginal <- c(3,7,1)
vetororiginal

novovetor <- rep(vetororiginal, times=3)
novovetor

novovetor2 <- rep(vetororiginal, times = c(4, 2, 3))
novovetor2

#Exercicio 3
#a
seq1 <- 20:30
vetor7 <- sum(seq1^2 + (4*seq1))
vetor7

#b
seq2 <- 10:20
vetor8 <- sum(((3^seq2)/seq2) + ((2^seq2)/(seq2^2)))
vetor8

#Exercicio 4
#a
num_bolas <- 100
num_sorteadas <- 40

sorteios <- sample(1:num_bolas, num_sorteadas, replace = TRUE)

bolas_pares <- sorteios[sorteios %% 2 == 0]
num_pares <- length(bolas_pares)

cat("Quantidade de bolas pares sorteadas:", num_pares, "\n")

#b
bolas_maior_70 <- sorteios[sorteios > 70]
num_maior_70 <- length(bolas_maior_70)

cat("Quantidade de bolas maiores do que 70 sorteadas:", num_maior_70, "\n")

#c
retiradas_impares <- which(sorteios %% 2 != 0)

cat("Posições das retiradas das bolas ímpares:", retiradas_impares, "\n")

#Exercicio 5

simular_lancamentos <- function() {
  lancamentos <- 0
  numero_4s <- 0
  
  while (numero_4s < 2) {
    resultado <- sample(1:6, 1)  # Simular um lançamento de dado
    lancamentos <- lancamentos + 1
    
    if (resultado == 4) {
      numero_4s <- numero_4s + 1
    }
  }
  
  return(lancamentos)
}

resultado <- simular_lancamentos()
cat("Número de lançamentos necessários até o 4 ser obtido pela segunda vez:", resultado, "\n")

#Exercicio 6

quantidades <- replicate(10000, simular_lancamentos())

media_lancamentos <- mean(quantidades)

cat("Média do número de lançamentos:", media_lancamentos, "\n")

#A média do número de lançamentos nos dá uma ideia de quantos lançamentos, em média, são necessários até que o número 4 seja obtido pela segunda vez. 
#Quanto maior o valor da média, mais lançamentos são necessários em média para alcançar esse resultado. 
#Essa média nos fornece uma estimativa da expectativa do número de lançamentos necessários com base nas simulações repetidas.

#Exercicio 7

fibonacci <- function(n) {
  if (n < 3) {
    stop("n deve ser maior ou igual a 3")
  }
  
  fib <- c(1, 1)
  
  for (i in 3:n) {
    next_term <- fib[i - 1] + fib[i - 2]
    fib <- c(fib, next_term)
  }
  
  return(fib)
}

# Exemplo de uso da função
n <- 10 
sequencia_fibonacci <- fibonacci(n)
print(sequencia_fibonacci)

#Exercicio 8

funcionarios <- c("Michael Scott", "Dwight Schrute", "Jim Halpert", "Kevin Malone", "Creed Bratton")

sorteio_amigo_oculto <- function() {
  sorteio <- sample(funcionarios, length(funcionarios), replace = FALSE)
  
  # Verificar se algum funcionário sorteou a si mesmo
  if (any(sorteio == funcionarios)) {
    return(0)  # Sorteio deu errado
  } else {
    return(1)  # Sorteio deu certo
  }
}

resultados <- replicate(100000, sorteio_amigo_oculto())

prop_erro <- mean(resultados == 0)

cat("Proporção de vezes que o amigo oculto deu errado:", prop_erro, "\n")

#Exercicio 9

jogo_craps <- function() {
  # Lançar dois dados
  dado1 <- sample(1:6, 1)
  dado2 <- sample(1:6, 1)
  soma <- dado1 + dado2
  
  # Verificar o resultado do primeiro lançamento
  if (soma %in% c(7, 11)) {
    return(1)  # Vitória
  } else if (soma %in% c(2, 3, 12)) {
    return(0)  # Derrota
  } else {
    # Continuar jogando até obter 7 ou a soma inicial
    while (TRUE) {
      novo_dado1 <- sample(1:6, 1)
      novo_dado2 <- sample(1:6, 1)
      nova_soma <- novo_dado1 + novo_dado2
      
      if (nova_soma == soma) {
        return(1)  # Vitória
      } else if (nova_soma == 7) {
        return(0)  # Derrota
      }
    }
  }
}

# Replicar o experimento 100000 vezes
resultados <- replicate(100000, jogo_craps())

# Calcular a proporção de vezes que você ganhou o jogo
prop_vitorias <- mean(resultados == 1)

# Exibir o resultado
cat("Proporção de vezes que você ganhou o jogo de Craps:", prop_vitorias, "\n")

#Exercicio 10

#a
#Função para simular o passeio de Luke e verificar se ele chega em casa
passeio_Luke <- function(L, N = 20) {
  while (L > 0 && L < N) {
    if (runif(1) < 0.5) {
      L <- L - 1  # Passo para a esquerda
    } else {
      L <- L + 1  # Passo para a direita
    }
  }
  if (L == 0) {
    return(1)  # Luke chegou em casa
  } else {
    return(0)  # Luke caiu no precipício
  }
}

#teste com L = 2
passeio_Luke(2,N=20)

#b
#Função para replicar o passeio de Luke e calcular a proporção de vezes que ele chegou em casa
proporcao_casa <- function(L, replicacoes = 10000) {
  resultados <- replicate(replicacoes, passeio_Luke(L))
  proporcao <- mean(resultados)
  return(proporcao)
}

#teste com L=2
proporcao_casa(2,replicacoes = 10000)

#c
#Calcular as proporções para 𝐿 variando de 1 a 19
L_valores <- 1:19
prop_valores <- sapply(L_valores, proporcao_casa)

# Plotar o gráfico de proporções
plot(L_valores, prop_valores, type = "o", xlab = "Posição Inicial (L)", ylab = "Proporção de Chegar em Casa", main = "Proporção de Luke Chegar em Casa vs. Posição Inicial")
