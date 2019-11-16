data <- read.csv('data/statystyki.csv')
data <- data %>% left_join(miasta, by = 'city')
library(dplyr)
data <- arrange(data, nazwa)
data$nazwa <- gsub("[\r\n]", "", data$nazwa)
data$poznan <- data$nazwa == 'poznan'
labels(data$poznan) <- c('NIE', 'TAK')
library(ggplot2)

ggplot(data, aes(nazwa, Natura, fill=poznan)) + geom_col() +
  coord_flip() + theme(legend.position = 'none') +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  xlab('Miasto')

ggplot(data, aes(nazwa, SuperHotele, fill=poznan)) + geom_col() +
  coord_flip() + theme(legend.position = 'none') +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  xlab('Miasto')

ggplot(data, aes(nazwa, Hostele, fill=poznan)) + geom_col() +
  coord_flip() + theme(legend.position = 'none') +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  xlab('Miasto')

ggplot(data, aes(nazwa, Historia, fill=poznan)) + geom_col() +
  coord_flip() + theme(legend.position = 'none') +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  xlab('Miasto')

ggplot(data, aes(nazwa, Tlum, fill=poznan)) + geom_col() +
  coord_flip() + theme(legend.position = 'none') +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  xlab('Miasto')

ggplot(data, aes(nazwa, Restauracje, fill=poznan)) + geom_col() +
  coord_flip() + theme(legend.position = 'none') +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  xlab('Miasto')

ggplot(data, aes(nazwa, Pomniki, fill=poznan)) + geom_col() +
  coord_flip() + theme(legend.position = 'none') +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  xlab('Miasto')

ggplot(data, aes(nazwa, Sztuka, fill=poznan)) + geom_col() +
  coord_flip() + theme(legend.position = 'none') +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  xlab('Miasto')

ggplot(data, aes(nazwa, Teatr, fill=poznan)) + geom_col() +
  coord_flip() + theme(legend.position = 'none') +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  xlab('Miasto')

ggplot(data, aes(nazwa, Rowery, fill=poznan)) + geom_col() +
  coord_flip() + theme(legend.position = 'none') +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  xlab('Miasto') + ylab('Sciezki rowerowe')

ggplot(data, aes(nazwa, Imprezy, fill=poznan)) + geom_col() +
  coord_flip() + theme(legend.position = 'none') +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  xlab('Miasto')

ggplot(data, aes(nazwa, to_do, fill=poznan)) + geom_col() +
  coord_flip() + theme(legend.position = 'none') +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  xlab('Miasto') + ylab('Rozrywka')
