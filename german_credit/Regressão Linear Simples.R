#Bem-vindos ao tutorial de como realizar uma regressão com R
#A primeira coisa a fazer é adicionar a base de dados que queremos
#Para isso, podemos clicar em "Import Dataset" no quadrante superior a direita
#Nesse caso, adicionamos uma base de dados chamada exemplo_1
#Para falarmos com o R que de agora em diante nos referimos a essa base de dados,
#Utilizamos a função "attach()"
attach(exemplo_1)
#Podemos tirar informações simples sobre essa base de dados com a função "summary()"
summary(exemplo_1)

#Para visualizar gráficamente, usamos a função "plot(x,y)" ou "plot(y~x)"
#"y~x" significa "y em função de x"
plot(experiencia,salario)

##ETAPA 1 - CRIAÇÃO DO MODELO
#Para montarmos um modelo de regressão linear com as variáveis dessa base de dados
#Utilizamos a função "lm(y ~ x)", sendo x a variavel explicadora e y a explicada
lm(salario ~ experiencia)

#Para facilitar as próximas funções, é recomendável fazer dessa função um objeto
lm.sal <- lm(salario ~ experiencia)

#Podemos adicionar objetos ao gráfico com a função "abline()"
abline(lm.sal)

#É possível até adicionar uma cor diferente com essa função
abline(lm.sal, col = 'red')

##ETAPA 2 - TESTE DO MODELO
#Para testarmos o modelo, usamos a função "anova()"
anova(lm.sal)

#O valor-p se encontra na última coluna como "Pr(>F)"
#Ao lado, temos estrelas que indicam em qual intervalo se encontra esse valor
#No caso do exemplo, o valor-p é 2.2e-16, um número muito próximo de zero
#O R retornou 3 estrelas pois se encontra entre 0 e 0.001, uma boa posição
#Como o modelo passou no teste, vamos interpretar o que temos

##ETAPA 3 - INTERPRETAÇÃO
#Para interpretarmos o que temos, podemos ver o modelo com a mesma função "summary()"
summary(lm.sal)

#Aqui temos o valor do Multiple R-Squared, indicado para Regressões Simples
#Neste caso temos um resultado bem positivo: 0.9417

#A função de regressão é: "y = 1.806330 + 0.100759x" sendo x = experiencia e y = salario
#Isso significa que com a adição de um ano de experiancia, a estimativa é a adição
#de 0.100759 unidades de salário.