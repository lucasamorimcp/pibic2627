#exercicios encontro 1.

###comparecimento eleitoral. base: turnout.csv.

#importe a base e apresente suas dimensoes e um resumo das variaveis.
#quantas observacoes existem? quais anos estao disponiveis?

#cada linha representa uma eleicao.
turnout <- read.csv("turnout.csv")
print(dim(turnout))
print(summary(turnout))
print(turnout$year)

#calcule o comparecimento por ano usando vap e vep.
#no calculo com vap, considere os eleitores aptos no exterior.
#salve as taxas na base. apresente em porcentagem e compare.

#somamos overseas porque a vap nao inclui os eleitores no exterior.
#multiplicamos por 100 para obter porcentagens.
turnout$taxa_vap <- 100 * turnout$total / (turnout$VAP + turnout$overseas)
turnout$taxa_vep <- 100 * turnout$total / turnout$VEP
print(round(turnout[c("year", "taxa_vap", "taxa_vep")], 3))
#a taxa baseada na vep e sempre maior: seu denominador exclui quem nao pode votar.

#calcule anes menos cada uma das taxas anteriores.
#apresente media, minimo e maximo das diferencas. interprete.

#as diferencas entre porcentagens sao medidas em pontos percentuais.
turnout$dif_vap <- turnout$ANES - turnout$taxa_vap
turnout$dif_vep <- turnout$ANES - turnout$taxa_vep
print(round(turnout[c("year", "dif_vap", "dif_vep")], 3))
resumo <- rbind(
  vap = c(media = mean(turnout$dif_vap), min = min(turnout$dif_vap),
          max = max(turnout$dif_vap)),
  vep = c(media = mean(turnout$dif_vep), min = min(turnout$dif_vep),
          max = max(turnout$dif_vep))
)
print(round(resumo, 3))
#vap: media 20,329; minimo 11,061; maximo 26,172 pontos percentuais.
#vep: media 16,836; minimo 8,581; maximo 22,489 pontos percentuais.
#o anes supera as duas taxas. isso sugere sobredeclaracao, mas nao prova sua causa.

#compare anes e a taxa baseada na vep por tipo de eleicao.
#separe presidenciais e meio de mandato. compare as diferencas medias.

#%% calcula o resto da divisao. anos presidenciais sao divisiveis por quatro.
turnout$tipo <- ifelse(turnout$year %% 4 == 0, "presidencial", "meio_mandato")
print(turnout[c("year", "tipo", "ANES", "taxa_vep", "dif_vep")])
#tapply calcula a media separadamente para cada tipo de eleicao.
medias_tipo <- tapply(turnout$dif_vep, turnout$tipo, mean)
print(round(medias_tipo, 3))
#presidenciais: 17,892 pontos percentuais; meio de mandato: 15,429.
#a diferenca media e maior nas presidenciais.


#ordene os anos e divida a serie em duas metades de igual tamanho.
#apresente anes menos a taxa baseada na vep por ano e a media de cada metade.
#informe os periodos. o vies aumentou?

#ordenamos antes de separar as primeiras e as ultimas sete eleicoes.
turnout <- turnout[order(turnout$year), ]
metade <- nrow(turnout) / 2
primeira <- turnout[seq_len(metade), ]
segunda <- turnout[(metade + 1):nrow(turnout), ]
print(primeira[c("year", "dif_vep")])
print(segunda[c("year", "dif_vep")])
resumo_metades <- rbind(
  primeira = c(inicio = min(primeira$year), fim = max(primeira$year),
               media = mean(primeira$dif_vep)),
  segunda = c(inicio = min(segunda$year), fim = max(segunda$year),
              media = mean(segunda$dif_vep))
)
print(round(resumo_metades, 3))
#1980-1992: 15,854 pontos percentuais. 1994-2008: 17,819.
#a diferenca media aumentou 1,965 ponto, mas nao cresce em todos os anos.


#ajuste o comparecimento de 2008 para residentes com direito a voto.
#considere condenados sem direito a voto, nao cidadaos e votos do exterior.
#compare com anes e com as duas medidas de comparecimento. interprete.

eleicao_2008 <- turnout[turnout$year == 2008, ]
#retiramos do denominador os dois grupos sem direito a voto.
vap_ajustada <- eleicao_2008$VAP - eleicao_2008$felons - eleicao_2008$noncit
#retiramos os votos do exterior. a vap ja considera somente residentes.
votos_ajustados <- eleicao_2008$total - eleicao_2008$osvoters
taxa_ajustada <- 100 * votos_ajustados / vap_ajustada
comparacao_2008 <- c(vap = eleicao_2008$taxa_vap, vep = eleicao_2008$taxa_vep,
                     ajustada = taxa_ajustada, anes = eleicao_2008$ANES)
print(round(comparacao_2008, 3))
#vap: 55,674%; vep: 61,554%; ajustada: 62,899%; anes: 78%.
#o ajuste aproxima as taxas, mas o anes permanece 15,101 pontos acima.


###dinamica populacional. importe as bases de quenia, suecia e mundo.
###compare os periodos 1950-1955 e 2005-2010.

quenia <- read.csv("Kenya.csv")
suecia <- read.csv("Sweden.csv")
mundo <- read.csv("World.csv")


#crie uma variavel com o total de pessoas-ano em cada base.
#calcule a taxa bruta de natalidade por regiao e periodo.
#guarde um vetor com os dois periodos por regiao. compare os resultados.

#somamos o tempo de exposicao de homens e mulheres.
quenia$py.total <- quenia$py.men + quenia$py.women
suecia$py.total <- suecia$py.men + suecia$py.women
mundo$py.total <- mundo$py.men + mundo$py.women
#tapply soma os valores de cada periodo. guardamos os denominadores.
py_quenia <- tapply(quenia$py.total, quenia$period, sum)
py_suecia <- tapply(suecia$py.total, suecia$period, sum)
py_mundo <- tapply(mundo$py.total, mundo$period, sum)
#a taxa bruta de natalidade divide nascimentos por pessoas-ano.
cbr_quenia <- tapply(quenia$births, quenia$period, sum) / py_quenia
cbr_suecia <- tapply(suecia$births, suecia$period, sum) / py_suecia
cbr_mundo <- tapply(mundo$births, mundo$period, sum) / py_mundo
#multiplicamos por mil apenas para apresentar as taxas.
print(round(1000 * rbind(quenia = cbr_quenia, suecia = cbr_suecia,
                         mundo = cbr_mundo), 3))
#por mil: quenia 52,095 -> 38,515; suecia 15,396 -> 11,926; mundo 37,329 -> 20,216.
#a natalidade cai nas tres regioes e permanece maior no quenia.


#calcule a fecundidade por faixa etaria, dos 15 aos 49 anos.
#faca isso para cada regiao e periodo. guarde os resultados por regiao.
#compare os padroes do quenia e da suecia.

#selecionamos apenas as sete faixas da vida reprodutiva.
idades <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
fec_quenia <- quenia[quenia$age %in% idades, ]
fec_suecia <- suecia[suecia$age %in% idades, ]
fec_mundo <- mundo[mundo$age %in% idades, ]
#dividimos os nascimentos pelo tempo de exposicao das mulheres da mesma faixa.
fec_quenia$asfr <- fec_quenia$births / fec_quenia$py.women
fec_suecia$asfr <- fec_suecia$births / fec_suecia$py.women
fec_mundo$asfr <- fec_mundo$births / fec_mundo$py.women
print(fec_quenia[c("period", "age", "asfr")])
print(fec_suecia[c("period", "age", "asfr")])
print(fec_mundo[c("period", "age", "asfr")])
#as taxas acima sao nascimentos por mulher-ano, sem multiplicar por mil.
#o quenia tem taxas maiores em todas as faixas, com pico em 20-24 anos.
#na suecia, o pico passa de 20-24 para 30-34 anos: maternidade mais tardia.


#calcule a taxa de fecundidade total por regiao e periodo.
#guarde um vetor com os dois periodos por regiao.
#no mundo, compare tambem a populacao feminina media e os nascimentos. interprete.

#somamos as taxas por idade. cada faixa reprodutiva abrange cinco anos.
tfr_quenia <- 5 * tapply(fec_quenia$asfr, fec_quenia$period, sum)
tfr_suecia <- 5 * tapply(fec_suecia$asfr, fec_suecia$period, sum)
tfr_mundo <- 5 * tapply(fec_mundo$asfr, fec_mundo$period, sum)
print(round(rbind(quenia = tfr_quenia, suecia = tfr_suecia, mundo = tfr_mundo), 3))
#filhos por mulher: quenia 7,591 -> 4,880; suecia 2,227 -> 1,903; mundo 5,007 -> 2,544.
#essas taxas supoem a manutencao do padrao do periodo durante a vida reprodutiva.
#dividimos pessoas-ano por cinco para estimar a populacao media do quinquenio.
mulheres_mundo <- tapply(mundo$py.women, mundo$period, sum) / 5
nascimentos_mundo <- tapply(mundo$births, mundo$period, sum)
#os dados estao em milhares. dividimos por mil para mostrar em milhoes.
print(round(rbind(mulheres_media_milhoes = mulheres_mundo / 1000,
                   nascimentos_milhoes = nascimentos_mundo / 1000), 3))
#mulheres: 1311,137 -> 3310,956 milhoes; nascimentos: 488,892 -> 674,581 milhoes.
#os nascimentos aumentam apesar da menor fecundidade por mulher.
#o tamanho da populacao e sua composicao etaria ajudam a explicar essa diferenca.


#calcule a taxa bruta de mortalidade por regiao e periodo.
#guarde um vetor com os dois periodos por regiao. compare os resultados.

#dividimos os obitos pelo total de pessoas-ano de cada periodo.
cdr_quenia <- tapply(quenia$deaths, quenia$period, sum) / py_quenia
cdr_suecia <- tapply(suecia$deaths, suecia$period, sum) / py_suecia
cdr_mundo <- tapply(mundo$deaths, mundo$period, sum) / py_mundo
print(round(1000 * rbind(quenia = cdr_quenia, suecia = cdr_suecia,
                         mundo = cdr_mundo), 3))
#obitos por mil: quenia 23,963 -> 10,389; suecia 9,845 -> 9,968; mundo 19,319 -> 8,166.
#a taxa cai no quenia e no mundo. na suecia, fica quase estavel.


#calcule a mortalidade por faixa etaria no quenia e na suecia em 2005-2010.
#compare os paises. como isso ajuda a interpretar as taxas brutas?

quenia_2005 <- quenia[quenia$period == "2005-2010", ]
suecia_2005 <- suecia[suecia$period == "2005-2010", ]
#match coloca as mesmas faixas etarias na mesma ordem nos dois bancos.
suecia_2005 <- suecia_2005[match(quenia_2005$age, suecia_2005$age), ]
#dividimos os obitos pelo tempo de exposicao dentro de cada faixa.
quenia_2005$asdr <- quenia_2005$deaths / quenia_2005$py.total
suecia_2005$asdr <- suecia_2005$deaths / suecia_2005$py.total
print(data.frame(idade = quenia_2005$age,
                 quenia_por_mil = round(1000 * quenia_2005$asdr, 3),
                 suecia_por_mil = round(1000 * suecia_2005$asdr, 3)))
#o quenia tem maior mortalidade em todas as faixas etarias.
#as taxas brutas proximas escondem diferencas na composicao etaria.


#use a composicao etaria sueca e as taxas por idade do quenia em 2005-2010.
#calcule a taxa bruta de mortalidade contrafactual do quenia.
#compare com a taxa observada e interprete o papel da composicao etaria.

#os pesos representam a participacao de cada idade nas pessoas-ano da suecia.
suecia_2005$peso <- suecia_2005$py.total / sum(suecia_2005$py.total)
print(sum(suecia_2005$peso))
#mantemos as taxas quenianas e trocamos apenas os pesos etarios.
#as faixas ja foram alinhadas na questao anterior.
cdr_contrafactual <- sum(quenia_2005$asdr * suecia_2005$peso)
print(round(1000 * c(observada = unname(cdr_quenia["2005-2010"]),
                     contrafactual = cdr_contrafactual), 3))
#a taxa passa de 10,389 para 23,216 obitos por mil pessoas-ano.
#com a composicao mais envelhecida da suecia, a taxa queniana seria maior.
#e uma padronizacao com taxas fixas, nao uma previsao.
