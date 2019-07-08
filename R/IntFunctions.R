### Internal functions
# Aggregation matrix
aggregation_matrix <- function(imagematrix) {
  linhas_imagem <- nrow(imagematrix)
  colunas_imagem <- ncol(imagematrix)
  matrix_agregacao <- matrix(NA, nrow = linhas_imagem, ncol = colunas_imagem)
  for (c in 2:(colunas_imagem - 1)) {
    for (l in 2:(linhas_imagem - 1)) {
      if (is.na(imagematrix[l, c]) | is.na(imagematrix[l - 1, c]) | is.na(imagematrix[l + 1,
                                                                                      c]) | is.na(imagematrix[l, c - 1]) | is.na(imagematrix[l, c + 1])) {
        aux.indice <- NA
      } else {
        soma.cell.visinha <- sum(imagematrix[l - 1, c], imagematrix[l + 1, c], imagematrix[l,
                                                                                           c - 1], imagematrix[l, c + 1])
        if (imagematrix[l, c] == 1) {
          aux.indice <- soma.cell.visinha/4
        } else {
          aux.indice <- (4 - soma.cell.visinha)/4
        }
        matrix_agregacao[l, c] <- aux.indice
      }
      matrix_agregacao[l, c] <- aux.indice
    }
  }
  # values<-subset(as.vector(matrix_agregacao), !is.na(as.vector(matrix_agregacao))) # Jogando todos
  # os dados para um unico objeto average_aggregation<-mean(values)
  return(matrix_agregacao)
}

# Non-adjusted aggregation index calculator - Calculate non-adjusted aggregation index.
basic_aggregation_index <-
  function(imagematrix) {
    matrix_agregacao <- aggregation_matrix(imagematrix)
    values <- subset(as.vector(matrix_agregacao), !is.na(as.vector(matrix_agregacao)))  # Jogando todos os dados para um unico objeto
    average_aggregation <- mean(values)
    return(average_aggregation)}

# Minimum aggregation index calculator for matrix without transparent pixels - Based on a given matrix, calculate de minimum possible aggregation index for matrix without transparent pixels.
min_aggregation_wo_transparence <-
  function(imagematrix) {
    height <- length(imagematrix[, 1])
    width <- length(imagematrix[1, ])
    total_px <- (height) * (width)
    px_analisados <- (height - 2) * (width - 2)
    total_preto <- sum(subset(as.vector(imagematrix), !is.na(as.vector(imagematrix))))
    total_branco <- total_px - total_preto
    cor_rara <- min(total_preto, total_branco)
    custo1 <- NULL
    custo2 <- NULL
    custo3 <- NULL
    custo4 <- NULL
    if (!height%%2 == 0 & !width%%2 == 0) {
      # Condicao matrix impar x impar
      sobra <- ((total_px - 1)/2) - cor_rara  # calcul de quantos pixeis tem que mudar de cor
      if (sobra == 0) {
        calculo_minimo <- 0
      } else {
        cabe_fora <- (height - 1) + (width - 1)  # Calcula quantos pixeis cabem do lado de fora da imagem
        cabe_1camada <- (height - 3) + (width - 3)  # Calcula quantos pixeis cabem na primeira camada de pixeis dentro da imagem
        if (sobra <= cabe_fora) {
          # o que sobrou cabe na camada mais externa
          calculo_minimo <- sobra * 0.25
        } else {
          if (sobra <= (cabe_fora + cabe_1camada)) {
            # o que sobrou cabe na camada mais externa e primeira camada
            custo1 <- cabe_fora * 0.25
            custo2 <- (sobra - cabe_fora) * 1.75
            calculo_minimo <- custo1 + custo2
          } else {

            custo1 <- cabe_fora * 0.25
            custo2 <- cabe_1camada * 1.75
            custo3 <- (sobra - cabe_fora - cabe_1camada) * 2
            calculo_minimo <- custo1 + custo2 + custo3
          }
        }
      }
    } else {
      # Condicao matrix par x impar ou par x par
      sobra <- (total_px/2) - cor_rara
      if (sobra <= 2) {
        calculo_minimo <- 0
      } else {
        cabe_fora <- (height - 2) + (width - 2)  # Calcula quantos pixeis cabem do lado de fora da matriz de analise - as quinas
        cabe_1camada_interno <- (height - 4) + (width - 4)  # Calcula quantos pixeis cabem do lado na primeira camada
        cabe_1camada_quina <- 2

        if (sobra <= (2 + cabe_fora)) {
          # o que sobrou cabe nos espacos da camada mais externa?
          calculo_minimo <- (sobra - 2) * 0.25
        } else {
          if (sobra <= (2 + cabe_fora + cabe_1camada_quina)) {
            # o que sobrou cabe nos espacos da camada mais externa + quina da primeira camada?
            custo1 <- cabe_fora * 0.25
            custo2 <- (sobra - 2 - cabe_fora) * 1.5
            calculo_minimo <- custo1 + custo2
          } else {
            if (sobra <= (2 + cabe_fora + cabe_1camada_quina + cabe_1camada_interno)) {
              # o que sobrou cabe nos espacos da camada mais externa + quina+ porcao interna da primeira camada?
              custo1 <- cabe_fora * 0.25
              custo2 <- cabe_1camada_quina * 1.5
              custo3 <- (sobra - 2 - cabe_fora - cabe_1camada_quina) * 1.75
              calculo_minimo <- custo1 + custo2 + custo3

            } else {
              # Aqui, foram usados todos os pixeis mais baratos e o restante caiu no mais caro

              custo1 <- cabe_fora * 0.25
              custo2 <- cabe_1camada_quina * 1.5
              custo3 <- cabe_1camada_interno * 1.75
              custo4 <- (sobra - 2 - cabe_fora - cabe_1camada_quina - cabe_1camada_interno) *
                2
              calculo_minimo <- custo1 + custo2 + custo3 + custo4
            }
          }
        }
      }
    }
    pixeis_usados_org <- length(subset(as.vector(imagematrix[-c(1, height), -c(1, width)]), !is.na(as.vector(imagematrix[-c(1,
                                                                                                                            height), -c(1, width)]))))
    pixeis_usados_NAs <- length(subset(as.vector(imagematrix[-c(1, height), -c(1, width)]), is.na(as.vector(imagematrix[-c(1,
                                                                                                                           height), -c(1, width)]))))
    px_analisados <- (pixeis_usados_org - pixeis_usados_NAs)
    resposta <- calculo_minimo/px_analisados
    return(resposta)
  }
# Maximum aggregation index calculator for matrix without transparent pixels - Based on a given matrix, calculate de maximum possible aggregation index for matrix without transparent pixels.
max_aggregation_wo_transparence <-
  function(imagematrix) {
    height <- length(imagematrix[, 1])
    width <- length(imagematrix[1, ])
    total_px <- height * width
    px_analisados <- (height - 2) * (width - 2)
    total_preto <- sum(subset(as.vector(imagematrix), !is.na(as.vector(imagematrix))))
    total_branco <- total_px - total_preto
    cor_rara <- min(total_preto, total_branco)

    if (height > width) {
      menor_lado <- width
    } else {
      menor_lado <- height
    }
    sobra <- cor_rara

    if (sobra <= 4) {
      custo_maximo <- 0
    } else {
      # 4 pixeis podem ir nass quinas e sairem de graca se cor rara cabe no menor lado + quinas do outro
      # lado
      if (cor_rara - 2 <= menor_lado) {
        custo_maximo <- 0.25 * (sobra - 4)
      } else {
        rebarba1 <- cor_rara%%menor_lado
        rebarba2 <- (cor_rara - 1)%%menor_lado
        rebarba3 <- (cor_rara - 2)%%menor_lado

        if (rebarba1 == 0 | rebarba2 == 0 | rebarba3 == 0) {
          custo_maximo <- (menor_lado - 2) * 0.5
        } else {
          if (rebarba1 == 0 | rebarba2 == 0 | rebarba3 == 0 | rebarba1 == menor_lado - 1 | rebarba2 ==
              menor_lado - 1 | rebarba3 == menor_lado - 1) {
            custo_maximo <- ((menor_lado - 2) * 0.5) + 0.25
          } else {
            custo_maximo <- ((menor_lado - 2) * 0.5) + 0.5
          }
        }
      }
    }


    pixeis_usados_org <- length(subset(as.vector(imagematrix[-c(1, height), -c(1, width)]), !is.na(as.vector(imagematrix[-c(1,
                                                                                                                            height), -c(1, width)]))))
    pixeis_usados_NAs <- length(subset(as.vector(imagematrix[-c(1, height), -c(1, width)]), is.na(as.vector(imagematrix[-c(1,
                                                                                                                           height), -c(1, width)]))))
    px_analisados <- (pixeis_usados_org - pixeis_usados_NAs)
    calculo_maximo <- (px_analisados - custo_maximo)/px_analisados


    return(calculo_maximo)
  }
#  Adjusted aggregation index calculator for matrix without transparent pixels - Based on a given matrix, calculate the adjusted aggregation index for matrix WITHOUT transparent pixels. For this, the minimum and maximum possible aggregation index are calculated, then the observed index is placed between this range. Adjusted aggregation index = 0 means that the pixels are set in the minimum possible aggregation. On the other hand, aggregation index = 1 means that the pixels are set in the maximum possible aggregation.
adjusted_aggregation_wo_transparence <-
  function(imagematrix) {
    valor_corrigido <- NULL
    observed <- basic_aggregation_index(imagematrix)
    height1 <- length(imagematrix[, 1])
    width1 <- length(imagematrix[1, ])
    pixels1 <- sum(subset(as.vector(imagematrix), !is.na(as.vector(imagematrix))))

    max.index <- max_aggregation_wo_transparence(imagematrix)
    min.index <- min_aggregation_wo_transparence(imagematrix)
    range <- max.index - min.index
    obs.corrigido <- observed - min.index
    valor_corrigido <- obs.corrigido/range
    return(valor_corrigido)
  }

# Minimum aggregation index calculator for matrix with transparent pixels - Based on a given matrix, calculate de minimum possible aggregation index for matrix with transparent pixels.
min_aggregation_with_transparence <-
  function(imagematrix) {
    largura <- length(imagematrix[1, ])
    altura <- length(imagematrix[, 1])
    pixeis_org <- subset(as.vector(imagematrix), !is.na(as.vector(imagematrix)))
    if (mean(pixeis_org) > 0.5) {
      mudar_de <- 0
      mudar_para <- 1
    } else {
      mudar_de <- 1
      mudar_para <- 0
    }
    px_preto_org <- sum(pixeis_org)
    px_branco_org <- length(pixeis_org) - sum(pixeis_org)
    dif_org <- px_preto_org - px_branco_org

    if (!altura%%2 == 0 & !largura%%2 == 0) {
      matrix_agr_min <- matrix(c(0, 1), ncol = largura + 1, nrow = altura)
      matrix_agr_min <- matrix_agr_min[, -length(matrix_agr_min[1, ])]
    } else {
      if (altura%%2 == 0 & !largura%%2 == 0) {
        matrix_agr_min <- matrix(c(0, 1), ncol = largura + 1, nrow = altura + 1)
        matrix_agr_min <- matrix_agr_min[-length(matrix_agr_min[, 1]), -length(matrix_agr_min[1,
                                                                                              ])]
      } else {
        if (altura%%2 == 0) {
          matrix_agr_min <- matrix(c(0, 1), ncol = largura, nrow = altura + 1)
          matrix_agr_min <- matrix_agr_min[-length(matrix_agr_min[, 1]), ]
        } else {
          matrix_agr_min <- matrix(c(0, 1), ncol = largura, nrow = altura)
        }
      }
    }  # Criar uma matrix de 1,0 em padrao de tabuleiro de xadrez - poderia fazer uma linha de comando mais curta, mas toda vez que temos uma matriz com numero de celular impar ou com numero de linhas par surgem problemas. Quando surge algum problema desses colocamos mais linhas ou colunas de forma que a matriz acabe sendo uma matriz de n de linhas impar e n de colunas par
    # gerar uma matrix auxiliar para calculo da agregacao
    for (c in 1:largura) {
      for (l in 1:altura) {
        if (is.na(imagematrix[l, c])) {
          matrix_agr_min[l, c] <- NA
        }
      }
    }  # Fazemos o espelhamento dos pontos de transparencia da imagem original
    pixeis_min <- subset(as.vector(matrix_agr_min), !is.na(as.vector(matrix_agr_min)))
    px_preto_min <- sum(pixeis_min)
    px_branco_min <- length(pixeis_min) - sum(pixeis_min)
    dif_min <- px_preto_min - px_branco_min
    if (dif_org < 0 & dif_min < 0) {
      if (!altura%%2 == 0 & !largura%%2 == 0) {
        matrix_agr_min <- matrix(c(1, 0), ncol = largura + 1, nrow = altura)
        matrix_agr_min <- matrix_agr_min[, -length(matrix_agr_min[1, ])]
      } else {
        if (altura%%2 == 0 & !largura%%2 == 0) {
          matrix_agr_min <- matrix(c(1, 0), ncol = largura + 1, nrow = altura + 1)
          matrix_agr_min <- matrix_agr_min[-length(matrix_agr_min[, 1]), -length(matrix_agr_min[1,
                                                                                                ])]
        } else {
          if (altura%%2 == 0) {
            matrix_agr_min <- matrix(c(1, 0), ncol = largura, nrow = altura + 1)
            matrix_agr_min <- matrix_agr_min[-length(matrix_agr_min[, 1]), ]
          } else {
            matrix_agr_min <- matrix(c(1, 0), ncol = largura, nrow = altura)
          }
        }
      }
      for (c in 1:largura) {
        for (l in 1:altura) {
          if (is.na(imagematrix[l, c])) {
            matrix_agr_min[l, c] <- NA
          }
        }
      }  # Fazemos o espelhamento dos pontos de
      pixeis_min <- subset(as.vector(matrix_agr_min), !is.na(as.vector(matrix_agr_min)))
      px_preto_min <- sum(pixeis_min)
      px_branco_min <- length(pixeis_min) - sum(pixeis_min)
      dif_min <- px_preto_min - px_branco_min
    }
    n_pixeis_mudar <- abs(dif_org - dif_min)/2  # N de pixeis que vao mudar

    falta_modificar <- n_pixeis_mudar

    # Pintar os pixeis das quinas mais externos
    if (!is.na(matrix_agr_min[1, 1])) {
      if (falta_modificar > 0 & matrix_agr_min[1, 1] == mudar_de) {
        matrix_agr_min[1, 1] <- mudar_para
        falta_modificar <- falta_modificar - 1
      }
    }
    if (!is.na(matrix_agr_min[altura, 1])) {
      if (falta_modificar > 0 & matrix_agr_min[altura, 1] == mudar_de) {
        matrix_agr_min[altura, 1] <- mudar_para
        falta_modificar <- falta_modificar - 1
      }
    }
    if (!is.na(matrix_agr_min[1, largura])) {
      if (falta_modificar > 0 & matrix_agr_min[1, largura] == mudar_de) {
        matrix_agr_min[1, largura] <- mudar_para
        falta_modificar <- falta_modificar - 1
      }
    }
    if (!is.na(matrix_agr_min[altura, largura])) {
      if (falta_modificar > 0 & matrix_agr_min[altura, largura] == mudar_de) {
        matrix_agr_min[altura, largura] <- mudar_para
        falta_modificar <- falta_modificar - 1
      }
    }

    ###################################################### Pintado os pixeis mais externos que nao estao diretamente no calculo da agregacao #
    cabe_l1 <- length(which(matrix_agr_min[1, ] == mudar_de))  # quanto cabe na 1 linha - 18
    if (falta_modificar > 0) {
      if (falta_modificar <= cabe_l1) {
        # cabe tudo na primeira linha?
        matrix_agr_min[1, which(matrix_agr_min[1, ] == mudar_de)[1:falta_modificar]] <- mudar_para  # se sim resolve tudo aqui e termina de montar a figura
      } else {
        # se nao modifica todos da primeira linha e vamos para proxima, a logica vai se repetira nas
        # linhas mais extremas
        falta_modificar <- falta_modificar - cabe_l1
        matrix_agr_min[1, which(matrix_agr_min[1, ] == mudar_de)] <- mudar_para
      }
    }
    cabe_lu <- length(which(matrix_agr_min[altura, ] == mudar_de))  # quanto cabe na ultima linha - 23
    if (falta_modificar > 0) {
      if (falta_modificar <= cabe_lu) {
        # do que falta cabe tudo na ultima linha?
        matrix_agr_min[altura, which(matrix_agr_min[altura, ] == mudar_de)[1:falta_modificar]] <- mudar_para  # se sim resolve tudo aqui e termina de montar a figura
      } else {
        # se nao modifica todos da primeira linha e vamos para proxima, a logica vai se repetira nas
        # linhas mais extremas
        falta_modificar <- falta_modificar - cabe_lu
        matrix_agr_min[altura, which(matrix_agr_min[altura, ] == mudar_de)] <- mudar_para
      }
    }
    cabe_c1 <- length(which(matrix_agr_min[, 1] == mudar_de))  # quanto cabe na 1 linha - 2
    if (falta_modificar > 0) {
      if (falta_modificar <= cabe_c1) {
        # do que falta cabe tudo na primeira coluna?
        matrix_agr_min[which(matrix_agr_min[, 1] == mudar_de)[1:falta_modificar], 1] <- mudar_para  # se sim resolve tudo aqui e termina de montar a figura
      } else {
        # se nao modifica todos da primeira linha e vamos para proxima, a logica vai se repetira nas
        # linhas mais extremas
        falta_modificar <- falta_modificar - cabe_c1
        matrix_agr_min[which(matrix_agr_min[, 1] == mudar_de), 1] <- mudar_para
      }
    }
    cabe_cu <- length(which(matrix_agr_min[, largura] == mudar_de))  # quanto cabe na ultima linha - 3
    if (falta_modificar > 0) {
      if (falta_modificar <= cabe_cu) {
        # do que falta cabe tudo na ultima coluna?
        matrix_agr_min[which(matrix_agr_min[, largura] == mudar_de)[1:falta_modificar], largura] <- mudar_para  # se sim resolve tudo aqui e termina de montar a figura
      } else {
        # se nao modifica todos da primeira linha e vamos para proxima, a logica vai se repetira nas
        # linhas mais extremas
        falta_modificar <- falta_modificar - cabe_cu
        matrix_agr_min[which(matrix_agr_min[, largura] == mudar_de), largura] <- mudar_para
      }
    }

    ###################################################### Pintado as laterais, falta pintar as bordas com NA #
    localizacao_linha <- NULL
    localizacao_coluna <- NULL
    for (c in 2:(largura - 1)) {
      for (l in 2:(altura - 1)) {
        if (falta_modificar > 0 & (!is.na(matrix_agr_min[l, c]) & (matrix_agr_min[l, c] == mudar_de &
                                                                   (is.na(matrix_agr_min[l - 1, c]) | is.na(matrix_agr_min[l + 1, c]) | is.na(matrix_agr_min[l,
                                                                                                                                                             c - 1]) | is.na(matrix_agr_min[l, c + 1]))))) {
          localizacao_linha <- c(localizacao_linha, l)
          localizacao_coluna <- c(localizacao_coluna, c)
          falta_modificar <- falta_modificar - 1
        }
      }
    }
    if (length(localizacao_linha) > 0) {
      for (i in 1:length(localizacao_linha)) {
        matrix_agr_min[localizacao_linha[i], localizacao_coluna[i]] <- mudar_para
      }
    }

    ###################################################### Preencher o centro com pixeis pretos circuladamente # while que vai preenchendo pelo meio
    rodada <- 0
    while (falta_modificar > 0) {
      rodada <- rodada + 1
      cabe_l1 <- length(which(matrix_agr_min[rodada + 1, ] == mudar_de))
      if (falta_modificar > 0) {
        if (falta_modificar <= cabe_l1) {
          matrix_agr_min[rodada + 1, which(matrix_agr_min[rodada + 1, ] == mudar_de)[1:falta_modificar]] <- mudar_para
          falta_modificar <- 0
        } else {
          falta_modificar <- falta_modificar - cabe_l1
          matrix_agr_min[rodada + 1, which(matrix_agr_min[rodada + 1, ] == mudar_de)] <- mudar_para
        }
      }

      cabe_lu <- length(which(matrix_agr_min[altura - rodada, ] == mudar_de))
      if (falta_modificar > 0) {
        if (falta_modificar <= cabe_lu) {
          matrix_agr_min[altura - rodada, which(matrix_agr_min[altura - rodada, ] == mudar_de)[1:falta_modificar]] <- mudar_para
          falta_modificar <- 0
        } else {
          falta_modificar <- falta_modificar - cabe_lu
          matrix_agr_min[altura - rodada, which(matrix_agr_min[altura - rodada, ] == mudar_de)] <- mudar_para
        }
      }

      cabe_c1 <- length(which(matrix_agr_min[, rodada + 1] == mudar_de))
      if (falta_modificar > 0) {
        if (falta_modificar <= cabe_c1) {
          matrix_agr_min[which(matrix_agr_min[, rodada + 1] == mudar_de)[1:falta_modificar],
                         rodada + 1] <- mudar_para
          falta_modificar <- 0
        } else {
          falta_modificar <- falta_modificar - cabe_c1
          matrix_agr_min[which(matrix_agr_min[, rodada + 1] == mudar_de), rodada + 1] <- mudar_para
        }
      }

      cabe_cu <- length(which(matrix_agr_min[, largura - rodada] == mudar_de))
      if (falta_modificar > 0) {
        if (falta_modificar <= cabe_cu) {
          matrix_agr_min[which(matrix_agr_min[, largura - rodada] == mudar_de)[1:falta_modificar],
                         largura - rodada] <- mudar_para
          falta_modificar <- 0
        } else {
          falta_modificar <- falta_modificar - cabe_cu
          matrix_agr_min[which(matrix_agr_min[, largura - rodada] == mudar_de), largura - rodada] <- mudar_para
        }
      }
    }

    min.index <- basic_aggregation_index(matrix_agr_min)
    return(min.index)
  }
# Maximum aggregation index calculator for matrix with transparent pixels - Based on a given matrix, calculate de maximum possible aggregation index for matrix with transparent pixels.
max_aggregation_with_transparence <-
  function(imagematrix) {
    largura <- length(imagematrix[1, ])
    altura <- length(imagematrix[, 1])
    if (altura > largura) {
      imagematrix <- apply(imagematrix, 1, rev)
      largura <- length(imagematrix[1, ])
      altura <- length(imagematrix[, 1])
    }
    pixeis_org <- subset(as.vector(imagematrix), !is.na(as.vector(imagematrix)))
    if (mean(pixeis_org) > 0.5) {
      mudar_de <- 1
      mudar_para <- 0
    } else {
      mudar_de <- 0
      mudar_para <- 1
    }

    px_preto_org <- sum(pixeis_org)
    px_branco_org <- length(pixeis_org) - sum(pixeis_org)
    dif_org <- px_preto_org - px_branco_org
    matrix_agr_max <- matrix(mudar_de, ncol = largura, nrow = altura)
    # gerar uma matrix auxiliar para calculo da agregacao maxima
    for (c in 1:largura) {
      for (l in 1:altura) {
        if (is.na(imagematrix[l, c])) {
          matrix_agr_max[l, c] <- NA
        }
      }
    }  # Fazemos o espelhamento dos pontos de transparencia da imagem original


    falta_modificar <- min(px_preto_org, px_branco_org)
    # pintar as quinas
    if (!is.na(matrix_agr_max[1, 1])) {
      if (falta_modificar > 0 & matrix_agr_max[1, 1] == mudar_de) {
        matrix_agr_max[1, 1] <- mudar_para
        falta_modificar <- falta_modificar - 1
      }
    }
    if (!is.na(matrix_agr_max[altura, 1])) {
      if (falta_modificar > 0 & matrix_agr_max[altura, 1] == mudar_de) {
        matrix_agr_max[altura, 1] <- mudar_para
        falta_modificar <- falta_modificar - 1
      }
    }
    if (!is.na(matrix_agr_max[1, largura])) {
      if (falta_modificar > 0 & matrix_agr_max[1, largura] == mudar_de) {
        matrix_agr_max[1, largura] <- mudar_para
        falta_modificar <- falta_modificar - 1
      }
    }
    if (!is.na(matrix_agr_max[altura, largura])) {
      if (falta_modificar > 0 & matrix_agr_max[altura, largura] == mudar_de) {
        matrix_agr_max[altura, largura] <- mudar_para
        falta_modificar <- falta_modificar - 1
      }
    }

    # se ainda sobrou fazer pintar as colunas internas
    if (falta_modificar > 0) {
      pixeis_para_mudar_por_coluna <- NULL  # objeto que vai contar quantos pixeis tem disponivel para mudar de  cor em cada coluna
      for (i in 1:largura) {
        aux_pixeis <- subset(as.vector(matrix_agr_max[, i]), !is.na(as.vector(matrix_agr_max[,
                                                                                             i])))
        pixeis_para_mudar_por_coluna[i] <- length(aux_pixeis) - sum(aux_pixeis == mudar_para)
      }

      acumulado_pixeis_para_mudar <- NULL
      for (i in 1:largura) {
        acumulado_pixeis_para_mudar[i] <- sum(pixeis_para_mudar_por_coluna[1:i])
      }


      coluna_completa <- which(acumulado_pixeis_para_mudar > falta_modificar)[1] - 1  # colunas que tem que estar com 1 em todo o espaco
      coluna_completa <- as.numeric(coluna_completa)

      if (!coluna_completa == 0) {
        # temos mais de 1 coluna
        total_preenchido <- acumulado_pixeis_para_mudar[coluna_completa]  # total de pixeis que vao ser preenchidos com as colunas completas
        px_ult_coluna <- falta_modificar - total_preenchido  #pixeis da proxima coluna que vao ter que ser preenchidos
        for (c in 1:coluna_completa) {
          for (l in 1:altura) {
            if (!is.na(imagematrix[l, c])) {
              if (matrix_agr_max[l, c] == mudar_de) {
                matrix_agr_max[l, c] <- mudar_para
                falta_modificar <- falta_modificar - 1
              }
            }
          }
        }
      } else {
        # caso especial onde vai ser pintado so a primeira coluna
        px_ult_coluna <- falta_modificar  #pixeis da primeira coluna que vao ter que ser pintados
      }
    }
    if (falta_modificar > 0) {
      pixeis_para_modificar <- which(matrix_agr_max[, coluna_completa + 1] == mudar_de)[1:px_ult_coluna]  # numero dos pixeis que vao mudar
      for (i in 1:px_ult_coluna) {
        matrix_agr_max[pixeis_para_modificar[i], coluna_completa + 1] <- mudar_para
        falta_modificar <- falta_modificar - 1
      }
    }

    max.index <- basic_aggregation_index(matrix_agr_max)
    return(max.index)
  }
# Adjusted aggregation index calculator for matrix with transparent pixels - Based on a given matrix, calculate the adjusted aggregation index for matrix WITH transparent pixels. For this, the minimum and maximum possible aggregation index are calculated, then the observed index is placed between this range. Adjusted aggregation index = 0 means that the pixels are set in the minimum possible aggregation. On the other hand, aggregation index = 1 means that the pixels are set in the maximum possible aggregation.
adjusted_aggregation_with_transparence <-
  function(imagematrix) {
    min.index <- min_aggregation_with_transparence(imagematrix)
    max.index <- max_aggregation_with_transparence(imagematrix)
    observed <- basic_aggregation_index(imagematrix)
    range <- max.index - min.index
    obs.corrigido <- observed - min.index
    valor_corrigido <- obs.corrigido/range
    return(valor_corrigido)
  }
