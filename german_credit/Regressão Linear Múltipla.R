#Bem-vindo ao tutorial de como realizar uma regressão linear múltipla
#Primeiro adicionamos nossa base de dados
#Como neste caso essa base se chama "exemplo_2", utilizamos a função "attach()" se referindo
#a esse nome para informarmos ao R que estamos falando sobre ela
attach(exemplo_2)

##ETAPA 1 - CRIACAO DO MODELO
#Para criarmos um modelo de regressão múltipla, usamos a funcao "lm()" com as varíaveis que
#achamos que podem explicar o preço
lm(preco ~ area + suites + dormitorios + vagas + salao.de.festas + churrasqueira + piscina + playground + academia + quadra)

#É aconselhavel transformar a função em um objeto para facilitar as análises futuras
lm.preco <- lm(preco ~ area + suites + dormitorios + vagas + salao.de.festas + churrasqueira + piscina + playground + academia + quadra)

##ETAPA 2 - TESTANDO O MODELO
#Para testarmos o modelo, usamos a função "anova()". Caso o valor-p de uma variável for
#menor que 0.05, temos que fazer um novo modelo sem essa variável
anova(lm.preco)

#Como neste caso o valor-p das variaveis "churrasqueira","playground","academia" e "quadra"
#Não alcançaram 0.05, temos que criar um novo modelo sem essas variáveis
lm.preco.2 <- lm(preco ~ area + suites + dormitorios + vagas + salao.de.festas + piscina)

#Agora testamos novamente para conferir que todas as variáveis passam no teste
anova(lm.preco.2)

##ETAPA 3 - INTERPRETAÇÃO
#Uma análise geral do modelo pode ser obtida com a função "summary()"
summary(lm.preco.2)

#Aqui podemos observar que o R-quadrado ajustado do modelo é:0.7305
#E também a função do modelo, ou seja, o que acontece com a adição de cada variável
#para o preco

#Por exemplo: a adição de uma vaga tende a adicionar R$ 57.899,30 ao preço final do
#imóvel. Ou também que a adição de um metro quadrado tende a adiconar R$ 3.029,60 ao preco final
