#Bem-vindos ao tutorial de como realizar uma regress�o com R
#A primeira coisa a fazer � adicionar a base de dados que queremos
#Para isso, podemos clicar em "Import Dataset" no quadrante superior a direita
#Nesse caso, adicionamos uma base de dados chamada exemplo_1
#Para falarmos com o R que de agora em diante nos referimos a essa base de dados,
#Utilizamos a fun��o "attach()"
attach(exemplo_1)
#Podemos tirar informa��es simples sobre essa base de dados com a fun��o "summary()"
summary(exemplo_1)

#Para visualizar gr�ficamente, usamos a fun��o "plot(x,y)" ou "plot(y~x)"
#"y~x" significa "y em fun��o de x"
plot(experiencia,salario)

##ETAPA 1 - CRIA��O DO MODELO
#Para montarmos um modelo de regress�o linear com as vari�veis dessa base de dados
#Utilizamos a fun��o "lm(y ~ x)", sendo x a variavel explicadora e y a explicada
lm(salario ~ experiencia)

#Para facilitar as pr�ximas fun��es, � recomend�vel fazer dessa fun��o um objeto
lm.sal <- lm(salario ~ experiencia)

#Podemos adicionar objetos ao gr�fico com a fun��o "abline()"
abline(lm.sal)

#� poss�vel at� adicionar uma cor diferente com essa fun��o
abline(lm.sal, col = 'red')

##ETAPA 2 - TESTE DO MODELO
#Para testarmos o modelo, usamos a fun��o "anova()"
anova(lm.sal)

#O valor-p se encontra na �ltima coluna como "Pr(>F)"
#Ao lado, temos estrelas que indicam em qual intervalo se encontra esse valor
#No caso do exemplo, o valor-p � 2.2e-16, um n�mero muito pr�ximo de zero
#O R retornou 3 estrelas pois se encontra entre 0 e 0.001, uma boa posi��o
#Como o modelo passou no teste, vamos interpretar o que temos

##ETAPA 3 - INTERPRETA��O
#Para interpretarmos o que temos, podemos ver o modelo com a mesma fun��o "summary()"
summary(lm.sal)

#Aqui temos o valor do Multiple R-Squared, indicado para Regress�es Simples
#Neste caso temos um resultado bem positivo: 0.9417

#A fun��o de regress�o �: "y = 1.806330 + 0.100759x" sendo x = experiencia e y = salario
#Isso significa que com a adi��o de um ano de experiancia, a estimativa � a adi��o
#de 0.100759 unidades de sal�rio.