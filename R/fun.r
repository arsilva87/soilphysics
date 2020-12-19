#' Solta dez observações
#'
#' A partir de uma base de dados, solta 10 observações aleatórias
#'
#' @export
solta_10 <- function() {
  dplyr::sample_n(dados, 10)
}

#' Prevê o score do filme
#'
#' Com base no orçamento e no ano, solta o rating médio de um filme
#' 
#' @param orcamento Orçamento do filme
#' @param ano Ano do filme
#'
#' @export
funcao_que_preve <- function(orcamento, ano) {
  predict(modelo, newdata = data.frame(budget = orcamento, year = ano))
}
