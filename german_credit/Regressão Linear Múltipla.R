#Bem-vindo ao tutorial de como realizar uma regress�o linear m�ltipla
#Primeiro adicionamos nossa base de dados
#Como neste caso essa base se chama "exemplo_2", utilizamos a fun��o "attach()" se referindo
#a esse nome para informarmos ao R que estamos falando sobre ela
attach(exemplo_2)

##ETAPA 1 - CRIACAO DO MODELO
#Para criarmos um modelo de regress�o m�ltipla, usamos a funcao "lm()" com as var�aveis que
#achamos que podem explicar o pre�o
lm(preco ~ area + suites + dormitorios + vagas + salao.de.festas + churrasqueira + piscina + playground + academia + quadra)

#� aconselhavel transformar a fun��o em um objeto para facilitar as an�lises futuras
lm.preco <- lm(preco ~ area + suites + dormitorios + vagas + salao.de.festas + churrasqueira + piscina + playground + academia + quadra)

##ETAPA 2 - TESTANDO O MODELO
#Para testarmos o modelo, usamos a fun��o "anova()". Caso o valor-p de uma vari�vel for
#menor que 0.05, temos que fazer um novo modelo sem essa vari�vel
anova(lm.preco)

#Como neste caso o valor-p das variaveis "churrasqueira","playground","academia" e "quadra"
#N�o alcan�aram 0.05, temos que criar um novo modelo sem essas vari�veis
lm.preco.2 <- lm(preco ~ area + suites + dormitorios + vagas + salao.de.festas + piscina)

#Agora testamos novamente para conferir que todas as vari�veis passam no teste
anova(lm.preco.2)

##ETAPA 3 - INTERPRETA��O
#Uma an�lise geral do modelo pode ser obtida com a fun��o "summary()"
summary(lm.preco.2)

#Aqui podemos observar que o R-quadrado ajustado do modelo �:0.7305
#E tamb�m a fun��o do modelo, ou seja, o que acontece com a adi��o de cada vari�vel
#para o preco

#Por exemplo: a adi��o de uma vaga tende a adicionar R$ 57.899,30 ao pre�o final do
#im�vel. Ou tamb�m que a adi��o de um metro quadrado tende a adiconar R$ 3.029,60 ao preco final
