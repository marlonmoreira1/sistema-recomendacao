# carregando os pacotes
library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(lubridate)
library(plyr)
library(dplyr)
library(RColorBrewer)

# carregando os dados
data = read_excel('Online Retail.xlsx')
View(data)

# fazendo um resumo inicial dos dados
summary(data[,c(4,6)])
str(data)

# No resumo as colunas preço unitário e quantidade tem
# como registro mínimo valor negativo
# investigando pra ver se há mais
neg_quant = data %>%
  filter(Quantity < 0)

neg_price = data %>%
  filter(UnitPrice < 0)

View(neg_price)
View(neg_quant)

# Como se trata de promoções e algo do tipo vou retirar esses
# do dataset
data2 = data[!data$Quantity < 0 & !data$UnitPrice < 0,]

summary(data2[,c(4,6)])
# Vi também que há muitos valores nulos
sum(is.na(data2))

for (i in colnames(data2)) {
  print(sum(is.na(data2[i])))
}

# Em sua maioria os valores nulos está no id do cliente

boxplot(data2$Quantity, main = 'Quantidade',ylim=c(0,35))
boxplot(data2$UnitPrice, main = 'Preço',ylim=c(0,20))

# como há alguns outliers vou fazer um filtro
quantile(data2$Quantity, seq(from = 0,to = 1,by = .10))
quantile(data2$UnitPrice, seq(from = 0,to = 1,by = .10))

# pegando somente os dados sem os outliers
quant = data2 %>%
  filter(Quantity < 25)

price = data2 %>%
  filter(UnitPrice < 8)

ggplot(quant,aes(Quantity)) +
  geom_histogram(binwidth = 1,col = 'black', fill= 'red')

ggplot(price,aes(UnitPrice)) +
  geom_histogram(binwidth = 0.2,col = 'black', fill= 'red')

# A quantidade mais comprada (1 e 2) e depois (3,4 e 6) e o valor
# unitário até 2.0

# Agora irei aplicar algumas transformações no dataset.
# Ele necessita está em um formato de dados de transação 
# Todos os itens que foram comprados em uma invoice fiquem 
# na mesma linha. Assim, o algoritmo conseguirá criar as 
# regras de associação.


# Passar algumas variáveis para o tipo de dado correto.

data2$InvoiceNo = as.numeric(data2$InvoiceNo)
data2$Country = as.factor(data2$Country)

# criando essa variável de hora para pegar todas as compras de 
# uma invoice.
data2$date = format(data2$InvoiceDate,"%H:%M:%S")

# Poderia fazer pelo id do cliente, mas há valores faltantes
# então optei pelo invoice.




# esse trecho de código irá usar as duas colunas
# invoice e date para agrupar os dados e a função anônima
# irá separar por vírgula todos os produtos comprados na
# mesma invoice.
transactiondata = ddply(data2,c("InvoiceNo","date"),
                         function(df1)paste(df1$Description,
                                            collapse = ","))


View(transactiondata)

# Não é necessário usar as colunas invoiceno e date
# por isso vou deletá-las
colnames(transactiondata)[3] = 'produtos'
transactiondata$InvoiceNo = NULL
transactiondata$date = NULL

# gravar os dados em um arquivo csv para depois 
# transformá-lo em dados de transação
write.csv(transactiondata,"transactions.csv", quote = FALSE, row.names = FALSE)
# Aqui é que deixarei os dados no formato de transação de fato
transacao = read.transactions('transactions.csv', format = 'basket', sep=',')

summary(transacao)

# Irei plotar os 10 produtos mais comprados
itemFrequencyPlot(tr,topN=10,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
itemFrequencyPlot(tr,topN=10,type="relative",col=brewer.pal(8,'Pastel2'), main="Relative Item Frequency Plot")


associacao = apriori(transacao, parameter = list(supp=0.001, conf=0.8,maxlen=10))
# vamos ver as associações
inspect(associacao[1:10])
# 100% das vezes que um cliente comprou o produto
# 'BILLBOARD FONTS DESIGN' ele também comprou
# WRAP. Já 93% das vezes que um cliente comprou
# o wrap ele também comprou o 'BILLBOARD FONTS DESIGN'


# Associações para clientes que compraram o 
# WHITE HANGING HEART T-LIGHT HOLDER um dos produtos mais comprados.
assoc = apriori(transacao, parameter = list(supp=0.001, conf=0.8), appearance = list(default="lhs",rhs="WHITE HANGING HEART T-LIGHT HOLDER"))
# ele está muito associado com produtos que parecem ser da mesma "família"
# em 80% quem com o pink e o red hanging heart t-light holder 
# também comprou o white.
# 83% quem comprou só o red também comprou 
# o white
inspect(head(assoc))

