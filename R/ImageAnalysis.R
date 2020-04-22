#' # Functions to be imported
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom stats sd
#' @importFrom jpeg readJPEG
#' @importFrom png readPNG


#' @title Aggregation index calculator
#' @description The function aggregation_index calculate the aggregation index. It works for matrix with and without transparent pixel. The aggregation index is a standardized estimation of the average proportion of same-color pixels around each image pixel. First, the proportion of same-color neighboring pixels (SCNP) is calculated (marginal lines and columns are excluded). Next, the SCNP for all pixels are averaged; then, given the proportion of black and white pixels, number of pixels in height and width, and location of transparent pixels (when present), the maximum and minimum possible aggregation indexes are calculated. Finally, the observed aggregation is standardized to a scale where the minimum possible value is set at zero and the maximum value is set at one.
#' @param imagematrix The matrix to be analysed.
#' @return
#' \item{adjusted_aggregation}{Standardized aggregation.}
#' \item{non_adjusted_aggregation}{Observed aggregation.}
#' @author Carlos Biagolini-Jr.
#' @seealso threshold_color
#' @examples
#' # First, get a matrix from your image. Here an example of a bush image is used.
#' # Using aggregation_index to estimate vegetation agregation
#' bush<-system.file("extdata/bush.JPG", package ="bwimage")
#' bush_imagematrix<-threshold_color(bush, "jpeg", "proportional", compress_rate = 0.1)
#' aggregation_index(bush_imagematrix)
#'
#' # Using aggregation_index to estimate aggregation of nest wall holes
#' nestwall<-system.file("extdata/bird_nestwall.png", package ="bwimage")
#' nestwall_imagematrix<-threshold_color(nestwall, "png", "width_fixed", target_width=300)
#' aggregation_index(nestwall_imagematrix)
#' @export
aggregation_index <-
  function(imagematrix) {
    if (length(subset(as.vector(imagematrix), is.na(as.vector(imagematrix)))) > 1) {adjusted_aggregation <- adjusted_aggregation_with_transparence(imagematrix)
    non_adjusted_aggregation <- basic_aggregation_index(imagematrix)
    } else {
      adjusted_aggregation <- adjusted_aggregation_wo_transparence(imagematrix)
      non_adjusted_aggregation <- basic_aggregation_index(imagematrix)}

    resposta <- c(adjusted_aggregation, non_adjusted_aggregation)
    names(resposta) <- c("adjusted_aggregation", "non_adjusted_aggregation")
    return(resposta)}

#' @title Summary of image information
#'
#' @description Provide the information of: number of black, white and transparent pixels, total number of pixels, height and width size.
#' @param imagematrix The matrix to be analysed.
#' @return
#' \item{Black}{Number of black pixels}
#' \item{White}{Number of white pixels}
#' \item{Transparent}{Number of transparent pixels}
#' \item{Total}{Total number of pixels}
#' \item{Height}{Size in height}
#' \item{Width}{Size in width}
#' @author Carlos Biagolini-Jr.
#' @seealso threshold_color
#' @examples
#' # First, get a matrix from your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg", "proportional",compress_rate = 0.1)
#' image_information(bush_imagematrix)
#' @export
image_information <-
  function(imagematrix) {
    resultado_em_processo <- NULL
    linhas_imagem <- nrow(imagematrix)
    colunas_imagem <- ncol(imagematrix)
    dados_pixeis <- subset(as.vector(imagematrix), !is.na(as.vector(imagematrix)))
    n_black_pixels <- sum(dados_pixeis)
    n_pixels <- length(dados_pixeis)
    n_white_pixels <- n_pixels - n_black_pixels
    n_transparent <- (linhas_imagem * colunas_imagem) - n_pixels
    width_BW_picture <- nrow(imagematrix)
    height_BW_picture <- ncol(imagematrix)
    resultado_em_processo[1] <- n_black_pixels
    resultado_em_processo[2] <- n_white_pixels
    resultado_em_processo[3] <- n_transparent
    resultado_em_processo[4] <- n_pixels
    resultado_em_processo[5] <- height_BW_picture
    resultado_em_processo[6] <- width_BW_picture

    names(resultado_em_processo) <- c("Black", "White", "Transparent", "Total","Height", "Width")
    return(resultado_em_processo)}
#' @title Denseness for whole image
#'
#' @description Proportion of black pixels in relation to all pixels. It do not take into account transparent pixels (when present).
#' @param imagematrix The matrix to be analysed.
#' @return Proportion of black pixels in relation to all pixels. It do not take into account transparent pixels (when present).
#' @references
#' Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
#' @author Carlos Biagolini-Jr.
#' @seealso threshold_color
#' @examples
#' # Get a matrix from your image. Here  examples provided by bwimage package.
#'
#' # I) Calculate vegetation denseness
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg", "proportional",compress_rate = 0.1)
#' denseness_total(bush_imagematrix)
#'
#' # II) Calculate canopy openness
#' # Convert image into binary matrix
#' canopy<-system.file("extdata/canopy.JPG",package ="bwimage")
#' canopy_matrix<-threshold_color(canopy,"jpeg", compress_method="proportional",compress_rate=0.1)
#' 1-denseness_total(canopy_matrix) # canopy openness
#' @export
denseness_total <-
  function(imagematrix) {
    dados_pixeis <- subset(as.vector(imagematrix), !is.na(as.vector(imagematrix)))
    n_black_pixels <- sum(dados_pixeis)
    n_pixels <- length(dados_pixeis)
    p_black_pixels <- n_black_pixels/n_pixels
    return(p_black_pixels)}
#' @title Denseness in row sections
#'
#' @description Calculate the denseness (proportion of black pixel in relation to the total number of pixels) for a given number of sections (n_sections). n_sections should be set as a number, in this situation denseness_row will break the original matrix in slices, and apply denseness_total function for each section. For instance, in a matrix of 1000x1000 if n_sections = 10, it will break to 10 sections of 100x1000 and analyze it. In other words, the sections will be the following sections of the original matrix [1:100, 1:1000] , [101:200, 1:1000] , [201:300, 1:1000] ,  [301:400, 1:1000] , [401:500, 1:1000] , [501:600, 1:1000] , [601:700, 1:1000] , [701:800, 1:1000] , [801:900, 1:1000] , [901:1000, 1:1000] .The default for parameter n_sections is "all", it will calculate denseness for each row of pixel. In other words, it will break the image in a number of section equal to the image pixel height.
#' @param imagematrix The matrix to be analysed.
#' @param n_sections Break the image in this number of rows.
#' @return
#' \item{Denseness}{Denseness of each row section.}
#' \item{Mean}{Mean of row sections denseness.}
#' \item{SD}{standard deviations of row sections denseness.}
#' @references
#' Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
#' @author Carlos Biagolini-Jr.
#' @seealso denseness_total threshold_color
#' @examples
#' # First, get a matrix from your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg", "proportional",compress_rate = 0.1)
#'
#' # Calculate vegetation denseness in 20 row sections
#' denseness_row(bush_imagematrix, n_sections = 20)
#' @export
denseness_row <-
  function(imagematrix, n_sections = "all") {
    if (n_sections == "all") {
      n_sections <- nrow(imagematrix)
    } else {
      if (nrow(imagematrix) < n_sections) {
        n_sections <- nrow(imagematrix)
      }}
    tamanho_linha <- floor(nrow(imagematrix)/n_sections)
    densidades <- NULL
    intervalos <- seq(from = 1, to = nrow(imagematrix), by = tamanho_linha)
    for (i in 1:n_sections) {
      sub_amostra <- imagematrix[intervalos[i]:(intervalos[i] + tamanho_linha - 1), ]
      densidades[i] <- denseness_total(sub_amostra)
    }
    media <- mean(densidades)
    desvio <- sd(densidades)
    resposta <- list(densidades, media, desvio)
    names(resposta) <-c("Denseness", "Mean", "SD")
    return(resposta)}

#' @title Denseness in column sections
#'
#' @description Calculate the denseness (proportion of black pixel in relation to the total number of pixels) for a given number of sections (n_sections). n_sections should be set as a number, in this situation denseness_column will break the original matrix in slices, and apply denseness_total function for each section. For instance, in a matrix of 1000x1000 if n_sections = 10, it will break to 10 sections of 1000x100 and analyze it. In other words, the sections will be the following sections of the original matrix [1:1000, 1:100] ,[ 1:1000,101:200] ,[ 1:1000,201:300] ,[ 1:1000,301:400] ,[ 1:1000,401:500] ,[ 1:1000,501:600] ,[ 1:1000,601:700] ,[ 1:1000,701:800] ,[ 1:1000,801:900] ,[ 1:1000,901:1000]. The default for parameter n_sections is "all", it will calculate denseness for each column of pixel. In other words, it will break the image in a number of section equal to the image pixel width.
#' @param imagematrix The matrix to be analysed.
#' @param n_sections Break the image in this number of columns.
#' @return
#' \item{Denseness}{Denseness of each column section.}
#' \item{Mean}{Mean of column sections denseness.}
#' \item{SD}{standard deviations of column sections denseness.}
#' @references
#' Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
#' @author Carlos Biagolini-Jr.
#' @seealso denseness_total threshold_color
#' @examples
#' # First, get a matrix from your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)
#'
#' # Calculate vegetation denseness in 20 column sections
#' denseness_column(bush_imagematrix,20)
#' @export
denseness_column <-
  function(imagematrix, n_sections = "all") {
    if (n_sections == "all") {
      n_sections <- ncol(imagematrix)
    } else {
      if (ncol(imagematrix) < n_sections) {
        n_sections <- ncol(imagematrix)}}
    tamanho_coluna <- floor(ncol(imagematrix)/n_sections)
    densidades <- NULL
    intervalos <- seq(from = 1, to = ncol(imagematrix), by = tamanho_coluna)

    for (i in 1:n_sections) {
      sub_amostra <- imagematrix[, intervalos[i]:(intervalos[i] + tamanho_coluna - 1)]
      densidades[i] <- denseness_total(sub_amostra)
    }
    media <- mean(densidades)
    desvio <- sd(densidades)
    resposta <- list(densidades, media, desvio)
    names(resposta) <- c("Denseness", "Mean", "SD")
    return(resposta)
  }
#' @title Hole finder
#'
#' @description Description of when a sequence of  same color pixel start and end.
#' @param section Section to be analysed.
#' @return Description of start and end of each same color sequence
#' @references
#' Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
#' @author Carlos Biagolini-Jr.
#' @seealso hole_section_data threshold_color
#' @examples
#' # First, get a matrix from your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)
#'
#' # Find pixel hole sections in the column 200 of bush image
#' hole_section(bush_imagematrix[,200])
#'
#' # Find pixel hole sections in the row 250 of bush image
#' hole_section(bush_imagematrix[250,])
#' @export
hole_section <-
  function(section) {
    sequencia_de_modificacao <- diff(section)
    nao_nas_original <- which(!is.na(sequencia_de_modificacao))

    comecou <- section[which(!is.na(section))[1]]
    if (sum(sequencia_de_modificacao[nao_nas_original]^2) == 0) {
      matrix_secoes <- matrix(c(min(which(!is.na(section) == T)), max(which(!is.na(section) == T),
                                                                      2), 0, comecou), ncol = 4, nrow = 1)
      colnames(matrix_secoes) <- c("Start", "End", "Size", "Color")
    } else {
      transicoes_para_preto <- which(sequencia_de_modificacao == 1)
      transicoes_para_branco <- which(sequencia_de_modificacao == -1)
      transicoes <- sort(c(transicoes_para_preto, transicoes_para_branco))
      n_seccoes <- length(transicoes) + 1
      matrix_secoes <- matrix(NA, ncol = 4, nrow = n_seccoes)
      colnames(matrix_secoes) <- c("Start", "End", "size", "Color")

      # qual e a cor do primeiro nao NA
      matrix_secoes[1, 4] <- comecou
      inicios <- c(which(!is.na(section))[1], transicoes)
      matrix_secoes[, 1] <- inicios

      for (i in 2:length(matrix_secoes[, 1])) {
        if (comecou == 0) {
          comecou <- 1
        } else {
          comecou <- 0
        }
        matrix_secoes[i, 4] <- comecou
      }

      for (i in 1:(length(matrix_secoes[, 1]) - 1)) {
        valor <- matrix_secoes[i + 1, 1]
        matrix_secoes[i, 2] <- valor
      }

      matrix_secoes[length(matrix_secoes[, 1]), 2] <- max(which(!is.na(section) == T))
    }

    matrix_secoes[, 3] <- matrix_secoes[, 2] - matrix_secoes[, 1] + 1
    return(matrix_secoes)
  }
#' @title Summary of holes information
#'
#' @description Summary information of holes of a given color in a given section. Result unit is the number of cell.
#' @param section Section to be analysed.
#' @param color Color of the hole (0 or 1).
#' @return
#' \item{N}{Number of hole sections}
#' \item{Mean}{Mean size of hole sections}
#' \item{SD}{Standard deviation of hole sections size}
#' \item{Min}{Minimum size of hole sections}
#' \item{Max}{Maximum size of hole sections}
#' @references
#' Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
#' @author Carlos Biagolini-Jr.
#' @seealso hole_section threshold_color
#' @examples
#' # First, get a matrix from your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)
#'
#' # Detail information of white (0) holes sections in the column 200 of bush image
#' hole_section_data(bush_imagematrix[,200], color = 0)
#'
#' # Detail information of black (1) holes sections in the row 250 of bush image
#' hole_section_data(bush_imagematrix[250,], color = 1)
#' @export
hole_section_data <-
  function(section, color = 0) {
    matrix_burcaos <- hole_section(section)
    matrix_burcaos_limpa <- subset(matrix_burcaos, matrix_burcaos[, 4] == color)
    if (is.na(matrix_burcaos_limpa[1])) {
      numero <- 0
      media <- 0
      desvio <- NA
      minimo <- 0
      maximo <- 0
    } else {
      numero <- length(matrix_burcaos_limpa[, 3])
      media <- mean(matrix_burcaos_limpa[, 3])
      desvio <- sd(matrix_burcaos_limpa[, 3])
      minimo <- min(matrix_burcaos_limpa[, 3])
      maximo <- max(matrix_burcaos_limpa[, 3])
    }
    resposta <- c(numero, media, desvio, minimo, maximo)
    names(resposta) <- c("N", "Mean", "SD", "Min","Max")
    return(resposta)
  }
#' @title Holes description in columns sections
#'
#' @description Summary information of holes in a given number of columns (n_sections). n_sections must be set as a number, in this situation hole_columm will sample columns, and apply hole_section_data function for each section. Next, all results will be display on hole_columm output. Example of how column sample works: in a matrix of 250x250 if n_sections =  5 , it will sample columns 1,51,101,151, and 201 and analyze it. In other words, the sections will be following sections of the original matrix [1:250,1] , [1:250,51], [1:250,101], [1:250,151], [1:250,201]. The default for parameter n_sections is "all", it will calculate hole_section_data for each column of pixel. In other words, it will break the image in a number of section equal to the image pixel width.
#' @param imagematrix The matrix to be analysed.
#' @param color Color of the hole (0 or 1).
#' @param n_sections Sample this number of columns.
#' @return
#' \item{N}{Number of sections.}
#' \item{Mean}{Mean sections size.}
#' \item{SD}{standard deviations of sections size.}
#' \item{Min}{Minimum sections size sections size.}
#' \item{Max}{Maximum sections size.}
#' \item{LH}{Stratum with largest hole count.}
#' @references
#' Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
#' @author Carlos Biagolini-Jr.
#' @seealso hole_section_data threshold_color
#' @examples
#' # First, get a matrix from your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)
#'
#' # Information of white (i.e. 0s in matrix) holes in 5 columns uniformly sample among matrix.
#' hole_columm(bush_imagematrix, n_sections=5 )
#'
#' # Information of black (i.e. 1s in matrix) holes in 20 columns uniformly sample among matrix.
#' hole_columm(bush_imagematrix, n_sections=20 )
#' @export
hole_columm <-
  function(imagematrix, color = 0, n_sections = "all") {
    numeros <- NULL
    medias <- NULL
    desvios <- NULL
    minimos <- NULL
    maximos <- NULL
    if (n_sections == "all") {
      sections <- 1:length(imagematrix[1, ])
      n_sections <- length(imagematrix[1, ])
    } else {
      intervalos <- floor(ncol(imagematrix)/n_sections)
      sections <- seq(from = 1, to = length(imagematrix[1, ]), by = intervalos)
    }

    for (i in 1:n_sections) {
      aup <- sections[i]
      aux <- hole_section_data(imagematrix[, aup], color = color)
      numeros[i] <- aux[1]
      medias[i] <- aux[2]
      desvios[i] <- aux[3]
      minimos[i] <- aux[4]
      maximos[i] <- aux[5]
    }
    maiores_estratos <- which(numeros == max(numeros[!is.na(numeros)]))
    resposta <- list(numeros, medias, desvios, minimos, maximos, maiores_estratos)
    names(resposta) <- c("N", "Mean", "SD", "Min","Max", "LH")
    return(resposta)
  }
#' @title Holes description in row sections
#' @description Summary information of holes in a given number of rows (n_sections). n_sections must be set as a number, in this situation hole_row will sample rows, and apply hole_section_data function for each section. Next, all results will be display on hole_columm output. Example of how row sample works: in a matrix of 250x250 if n_sections =  5 , it will sample rows 1,51,101,151, and 201 and analyze it. In other words, the sections will be following sections of the original matrix [1,1:250] , [51,1:250] , [101,1:250] , [151,1:250] , [201,1:250]. The default for parameter n_sections is "all", it will calculate hole_section_data for each row of pixel. In other words, it will break the image in a number of section equal to the image pixel height.
#' @param imagematrix The matrix to be analysed.
#' @param color Color of the hole (0 or 1).
#' @param n_sections Sample this number of rows.
#' @return
#' \item{N}{Number of sections.}
#' \item{Mean}{Mean sections size.}
#' \item{SD}{standard deviations of sections size.}
#' \item{Min}{Minimum sections size sections size.}
#' \item{Max}{Maximum sections size.}
#' \item{LH}{Stratum with largest hole count.}
#' @author Carlos Biagolini-Jr.
#' @seealso hole_section_data threshold_color
#' @examples
#' # First, get a matrix from your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)
#'
#' # Information of white (i.e. 0s in matrix) holes in 10 rows uniformly sample among matrix.
#' hole_row(bush_imagematrix, n_sections=10)
#'
#' # Information of black (i.e. 1s in matrix) holes in 15 rows uniformly sample among matrix.
#' hole_row(bush_imagematrix, n_sections=15)
#' @export
hole_row <-
  function(imagematrix, color = 0, n_sections = "all") {
    numeros <- NULL
    medias <- NULL
    desvios <- NULL
    minimos <- NULL
    maximos <- NULL

    if (n_sections == "all") {
      sections <- 1:length(imagematrix[, 1])
      n_sections <- length(imagematrix[, 1])
    } else {
      intervalos <- floor(nrow(imagematrix)/n_sections)
      sections <- seq(from = 1, to = length(imagematrix[, 1]), by = intervalos)
    }

    for (i in 1:n_sections) {
      aup <- sections[i]
      aux <- hole_section_data(imagematrix[aup, ], color = color)
      numeros[i] <- aux[1]
      medias[i] <- aux[2]
      desvios[i] <- aux[3]
      minimos[i] <- aux[4]
      maximos[i] <- aux[5]
    }
    maiores_estratos <- which(numeros == max(numeros[!is.na(numeros)]))
    resposta <- list(numeros, medias, desvios, minimos, maximos, maiores_estratos)
    names(resposta) <- c("N", "Mean", "SD", "Min","Max", "LH")
    return(resposta)
  }
#' @title Light gap
#'
#' @description Left and right distances from first black pixel to image edge.
#' @param imagematrix The matrix to be analysed
#' @param width_size Real size of image width (in mm, cm, m, etc..).
#' @param scale If FALSE do not ajust the output for real size.
#' @return Distances without black pixel in each side of the picture
#' @references
#' Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
#' @author Carlos Biagolini-Jr.
#' @seealso threshold_color
#' @examples
#' # First, get a matrix from your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)
#'
#' # Calculate vegetation Light gap in the bush image matrix
#' light_gap(bush_imagematrix,width_size=100)
#' # Conclusion: there is no light gap on both sides of bush image.
#' @export
light_gap <-
  function(imagematrix, width_size = NA, scale = TRUE) {
    if (scale == T & (is.na(width_size) | width_size <= 0)) {
      stop("width_size not found")
    } else {
      correcao <- width_size/length(imagematrix[1, ])
    }
    primeira <- 0
    ultima <- 0
    teste <- 0

    # Encontrar primeira coluna com dados
    coluna_rodada <- 1
    while (teste == 0) {
      coluna_dados <- subset(as.vector(imagematrix[, coluna_rodada]), !is.na(as.vector(imagematrix[,                                            coluna_rodada])))
      aux_test <- sum(coluna_dados)
      if (aux_test > 0) {
        teste <- 1
        primeira <- coluna_rodada
      } else {
        coluna_rodada <- coluna_rodada + 1}}

    # Encontrar ultima coluna com dados
    coluna_rodada <- length(imagematrix[1, ])
    teste <- 0
    while (teste == 0) {
      coluna_dados <- subset(as.vector(imagematrix[, coluna_rodada]), !is.na(as.vector(imagematrix[,
                                                                                                   coluna_rodada])))
      aux_test <- sum(coluna_dados)
      if (aux_test > 0) {
        teste <- 1
        ultima <- coluna_rodada
      } else {
        coluna_rodada <- coluna_rodada - 1
      }
    }
    primeira <- primeira - 1
    ultima <- length(imagematrix[1, ]) - ultima

    if (scale == T) {
      primeira <- primeira * correcao
      ultima <- ultima * correcao
    }

    resposta <- c(primeira, ultima)
    names(resposta) <- c("Left gap size", "Right gap size")
    return(resposta)
  }

#' @title Height of the highest black pixel in the image
#'
#' @description Find the higher black pixel in the whole image.
#' @param imagematrix The matrix to be analysed.
#' @param height_size Real size of image width (in mm, cm, m, etc..).
#' @return Height of the highest black pixel. It is scaleted for the real size (in mm, cm, m, etc..) based in the information from argument height_size.
#' @references
#' Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
#' @author Carlos Biagolini-Jr.
#' @seealso threshold_color
#' @examples
#' # First, get a matrix from your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)
#'
#' # Calculate height of the highest black pixel in the bush image matrix
#' heigh_maximum(bush_imagematrix,height_size=100)
#' # Conclusions: The highest vegetation unit ,i.e. highest black pixel, is 84.4 cm above ground.
#' @export
heigh_maximum <-
  function(imagematrix, height_size) {
    if (height_size <= 0) {
      stop("height_size must be greater than zero")
    }
    # Criar funcao para achar a maior coluna
    alt_max <- function(coluna) {
      if (sum(coluna == 1) > 0) {
        resposta <- which(coluna == 1)[1]
      } else {
        resposta <- length(coluna)
      }
      return(resposta)
    }
    alturas <- apply(imagematrix, 2, alt_max)  # encontre o primeiro pixel preto mais alto em cada coluna
    dist_de_baixo <- length(imagematrix[, 1]) - alturas
    dist_de_baixo[!dist_de_baixo == 0] <- dist_de_baixo[!dist_de_baixo == 0] + 1  # somar 1 para corrigir que apenas as que n?o tem nenhum pixel preto vao ter altura de zero
    maior_coluna <- which(dist_de_baixo == max(dist_de_baixo))[1]  #qual coluna tem o pixel mais alto (se for mais de uma como mesmo maximo pega so a primeira)
    altura_maxima <- dist_de_baixo[maior_coluna]
    altura_maxima <- altura_maxima * (height_size/length(imagematrix[, 1]))

    names(altura_maxima) <- c("Height")
    return(altura_maxima)
  }
#' @title Highest black pixel by sections
#'
#' @description Break the original matrix in a number of section ( n_sections), then find the higher black pixel in each image section.
#' @param imagematrix The matrix to be analysed.
#' @param n_sections Break the image in this number of columns.
#' @param height_size Real size of image height (in mm, cm, m, etc..).
#' @return
#' \item{Mean}{Height mean of the highest black pixel in sections.}
#' \item{SD}{Standard deviations of the highest black pixel in sections.}
#' \item{Size}{Height of the highest black pixel in sections.}
#' @references
#' Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
#' @author Carlos Biagolini-Jr.
#' @seealso threshold_color
#' @examples
#' # First, get a matrix from your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)
#'
#' # Profile  of  highest black pixels on sections of the bush image matrix
#' altitudinal_profile(bush_imagematrix,n_sections = 10, height_size=100)
#' # Conclusions:
#' # i)  the mean height of the highest black pixel is 45.28 cm.
#' # ii) standard deviation of highest black height is 21.54.
#' @export
altitudinal_profile <-
  function(imagematrix, n_sections, height_size) {
    if ((length(imagematrix[1, ])/2) < n_sections) {
      stop(paste("Sections width must have at least two cells. Please choose a number of n_sections lower or equal to ",
                 length(imagematrix[1, ])/2))
    }

    tamanho_coluna <- floor(ncol(imagematrix)/n_sections)
    dist_de_baixo <- NULL
    intervalos <- seq(from = 1, to = ncol(imagematrix), by = tamanho_coluna)

    for (i in 1:n_sections) {
      sub_amostra <- imagematrix[, intervalos[i]:(intervalos[i] + tamanho_coluna - 1)]
      aux_maximums <- heigh_maximum(sub_amostra, height_size = height_size)
      dist_de_baixo[i] <- aux_maximums
    }

    media <- mean(dist_de_baixo)
    desvio <- sd(dist_de_baixo)

    resposta <- list(media, desvio, dist_de_baixo)
    names(resposta) <- c("Mean", "SD","Size")
    return(resposta)}

#' @title Cumulative denseness for each line
#'
#' @description Proportion of black pixel below each matrix line.
#' @param imagematrix The matrix to be analysed.
#' @references
#' Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
#' @author Carlos Biagolini-Jr.
#' @seealso threshold_color
#' @examples
#' # First, get a matrix from your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)
#'
#' # Proportion of black pixel below each matrix line.
#' heigh_propotion(bush_imagematrix)
#' @export
heigh_propotion <-
  function(imagematrix) {
    total_row <- NULL
    for (i in 1:length(imagematrix[, 1])) {
      dados_pixeis <- subset(as.vector(imagematrix[i, ]), !is.na(as.vector(imagematrix[i, ])))
      total_row[i] <- sum(dados_pixeis)
    }
    n_pixels <- sum(total_row)
    porporcoes <- total_row/n_pixels
    acumulado <- rep(0, length(porporcoes))
    acumulado[length(porporcoes)] <- porporcoes[length(porporcoes)]

    for (i in seq(from = (length(porporcoes) - 1), to = 1)) {
      acumulado[i] <- porporcoes[i] + acumulado[i + 1]
    }
    return(acumulado)
  }
#' @title Cumulative denseness test
#'
#' @description Find the height which a given proportion of black pixel is found.
#' @param imagematrix The matrix to be analysed.
#' @param proportion Proportion of denseness to test.
#' @param height_size Real size of image height (in mm, cm, m, etc..).
#' @references
#' Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
#' @author Carlos Biagolini-Jr.
#' @seealso threshold_color
#' @examples
#' # First, get a matrix from your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)
#'
#' # See the proportion of black pixels (1) below each bush image matrix row
#' heigh_propotion_test(bush_imagematrix,0.75,100)
#' # Conclusion: in this imagem, 75 percent of the vegetation is hold below 31.2 cm.
#' @export
heigh_propotion_test <-
  function(imagematrix, proportion, height_size) {
    if (is.na(height_size) | height_size <= 0) {
      stop("height_size must be a numeric value grater than zero")
    }
    if (is.na(proportion) | proportion < 0 | proportion > 1) {
      stop("You must set a proportion value ranging from 0-1 to be tested")
    }
    total_acumulado <- heigh_propotion(imagematrix)
    altura <- sum(proportion > round(total_acumulado, 5)) + 1  # Como e um acomulado, quando voce obseva o primeiro 1 isso quer dizer que ali estava a ultima celula com dado, e foi la que foi a partir daquela linha que foi observado esse valor
    altura <- altura * (height_size/length(total_acumulado))  # colocar na escala

    names(altura) <- c(paste("Height below which", proportion, "of the vegetation denseness is located"))
    return(altura)
  }
#' @title Top line
#' @description Line running along the crest of highest black pixel.
#' @param imagematrix The matrix to be analysed.
#' @param height_size Real size of image height (in mm, cm, m, etc..).
#' @param width_size Real size of image width (in mm, cm, m, etc..).
#' @return Top line size that cover black pixels
#' @references
#' Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
#' @author Carlos Biagolini-Jr.
#' @seealso threshold_color
#' @examples
#' # First, get a matrix from your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush, "jpeg", "proportional", compress_rate = 0.1)
#'
#' # See the proportion of black pixels (1) below each bush image matrix row
#' topline(bush_imagematrix,100,100)
#' # Conclusion: topline size is 785.6 cm.
#' @export
topline <-
  function(imagematrix, height_size = NA, width_size = NA) {
    if (is.na(height_size) | height_size <= 0) {
      stop("height_size not found")
    }
    if (is.na(width_size) | width_size <= 0) {
      stop("width_size not found")
    }

    escala_horizontal <- width_size/length(imagematrix[1, ])
    escala_vertical <- height_size/length(imagematrix[, 1])

    # Recortar a parte da imagem com pixeis
    corte <- light_gap(imagematrix, scale = F)
    corte[1] <- corte[1] + 1
    corte[2] <- length(imagematrix[1, ]) - corte[2]
    imagem_trabalho <- imagematrix[, corte[1]:corte[2]]

    # Criar funcao para achar a maior coluna
    alt_max <- function(coluna) {
      if (sum(coluna == 1) > 0) {
        resposta <- which(coluna == 1)[1]
      } else {
        resposta <- length(coluna)
      }
      return(resposta)
    }

    # Perfil de altura
    alturas <- apply(imagem_trabalho, 2, alt_max)  # encontre o primeiro pixel preto mais alto em cada coluna
    dist_de_baixo <- length(imagematrix[, 1]) - alturas + 1

    altura_primeira_coluna <- dist_de_baixo[1]
    altura_ultimaa_coluna <- dist_de_baixo[length(dist_de_baixo)]

    meio <- NULL
    for (i in 1:(length(dist_de_baixo) - 1)) {
      meio[i] <- abs(dist_de_baixo[i] - dist_de_baixo[i + 1])
    }

    total_vertical <- altura_primeira_coluna + altura_ultimaa_coluna + sum(meio)  # quantos pixeis foram caminhados na horizontal
    total_vertical <- total_vertical * escala_vertical  # converter para a escala do estudo

    total_horizontal <- length(dist_de_baixo)
    total_horizontal <- total_horizontal * escala_horizontal  # converter para a escala do estudo

    resposta <- total_horizontal + total_vertical
    names(resposta) <- "topline"
    return(resposta)}

#' @title Denseness in samples
#'
#' @description Calculate the denseness (proportion of black pixel in relation to the total number of pixels) for a given number of samples.
#' @param imagematrix The matrix to be analysed.
#' @param width_size Real size of image width (in mm, cm, m, etc..).
#' @param height_size Real size of image height (in mm, cm, m, etc..).
#' @param sample_width Width of sample area.
#' @param sample_height Height of sample area.
#' @param method Method for sample ("random" or "uniform").
#' @param sample_shape The shape of sample unity ("rectangle" or "ellipse"). See plot_samples function.
#' @param n_samples Defines the number of samples, when sample_shape="random".
#' @param n_sample_horizontal Defines the number of samples column, when sample_shape=" uniform".
#' @param n_sample_vertical Defines the number of samples lines, when sample_shape=" uniform".
#' @param proportion_horizontal Range from 0 to 1. Represent the proportion of horizontal plane to be sample.  If proportion_horizontal=1 (default) all columns beacome potentially sample.
#' @param proportion_vertical Range from 0 to 1. Represent the proportion of vertical plane to be sample.  If proportion_vertical=1 (default) all lines become potentially sample.
#' @param aligin_horizontal Define horizontal align.  Three options are available: "center", "left" or "right".
#' @param aligin_vertical Define vertical align.  Three options are available: "middle","bottom" or "top".
#' @return Proportion of black pixels in samples. It do not take into account transparent pixels (when present).
#' @author Carlos Biagolini-Jr.
#' @seealso plot_samples
#' @examples
#' # Get a matrix from your image. Here  examples provided by bwimage package.
#'
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush<-threshold_color(bush,  "jpeg", "proportional",compress_rate = 0.1)
#' denseness_sample(bush, width_size=100, height_size=100, sample_width=5, sample_height=5)
#' @export
denseness_sample<-
  function(imagematrix, width_size, height_size, sample_width, sample_height, method="random", sample_shape="rectangle", n_samples=10, n_sample_horizontal=10, n_sample_vertical=1, proportion_horizontal=1, proportion_vertical=1,aligin_horizontal="center",aligin_vertical="bottom"){
  if (proportion_horizontal==1&proportion_vertical==1){ # se a pessoa quer trabalhar com a imagem toda
    x0<-1;y0<-1;largura_amostra<-ncol(imagematrix);altura_amostra<-nrow(imagematrix)
  }else{
    # Se pediu para cortar faz a conta:
    # Saber o tamanho da caixa a ser amostrada
    largura_amostra<-floor(proportion_horizontal*length(imagematrix[1,]))
    altura_amostra<-floor(proportion_vertical*length(imagematrix[,1]))
    # Encontrar x0
    if(aligin_horizontal=="center"){x0<-floor((ncol(imagematrix)-largura_amostra)/2)+1}else{
      if(aligin_horizontal=="left"){x0<-1}else{
        if(aligin_horizontal=="right"){x0<-ncol(imagematrix)-largura_amostra+1}else{
          show("invalid parameter - aligin_horizontal")}}}
    # Encontrar y0
    if(aligin_vertical=="top"){y0<-1}else{
      if(aligin_vertical=="middle"){y0<-floor((nrow(imagematrix)-altura_amostra)/2)+1}else{
        if(aligin_vertical=="bottom"){y0<-nrow(imagematrix)-altura_amostra+1}else{
          show("invalid parameter - aligin_vertical")}}}}
  # Agora sabemos de qual pixel ate qual pixel que vamos trabalhar
  # Agora vamos comverter o tamanho da amostra (que foi dada em numero real) para a escala de pixel
  amostra_largura_px<-(sample_width * ncol(imagematrix))/width_size
  amostra_altura_px <-(sample_height * nrow(imagematrix))/height_size
  # Sabendo a area de trabalho, e o tamanho da amostra, vamos calcular o espaco interno que pode ser usado para amostragem
  # Esse espaco interno desconta uma margem que nao da para colocar o centro do objeto amostrado la dentro
  largura_interna<-largura_amostra-amostra_largura_px
  altura_interna<-altura_amostra-amostra_altura_px
  primeiro_px_interno_colunas<-ceiling(amostra_largura_px/2)+(x0-1)
  primeiro_px_interno_linhas<-ceiling(amostra_altura_px/2) +(y0-1)
  ultimo_px_interno_colunas<-floor(largura_amostra- (amostra_largura_px/2))+(x0-1)
  ultimo_px_interno_linhas<-floor(altura_amostra- (amostra_altura_px/2))+(y0-1)

  # Amostrar pontos
  if(method=="random"){
    lista_amostragem<-matrix(NA,ncol=2,nrow=n_samples)# Criar uma lista de pontos para amostrar
    lista_amostragem[,1]<-sample(c(primeiro_px_interno_linhas:ultimo_px_interno_linhas), n_samples) # linhas aleatoriamente amostradas
    lista_amostragem[,2]<-sample( c(primeiro_px_interno_colunas:ultimo_px_interno_colunas), n_samples) # colunas aleatoriamente amostradas
    colnames(lista_amostragem)<-c("linha","coluna")}else{
      if(method=="uniform"){
        # Criar uma lista de pontos para amostrar
        lista_amostragem<-matrix(NA,ncol=2,nrow=(n_sample_horizontal*n_sample_vertical))
        # Pegar a area interna e dividir em um numero de secoes igual o numero de amostrar pedido +1, dessa forma encaixamos os pontos uniformemente no eixo
        tamanho_seccoes_horizontal<-largura_interna/(n_sample_horizontal+1)
        tamanho_seccoes_vertical<-altura_interna/(n_sample_vertical+1)

        # Aqui voce encontra os pontos para amostrar dentro da sub imagem, mas veja que isso dentro de uma escala para essa sub-imagem
        pontos_amostra_colunas<-pontos_amostra_linhas<-NULL
        for(i in 1: n_sample_horizontal){pontos_amostra_colunas[i]<- floor(amostra_largura_px/2+tamanho_seccoes_horizontal*i)}
        for(i in 1: n_sample_vertical){pontos_amostra_linhas[i]<- floor(amostra_altura_px/2+tamanho_seccoes_vertical*i)}

        # Agora vamos voltar para a escala da figura original
        pontos_amostra_colunas<-pontos_amostra_colunas+(x0-1)
        pontos_amostra_linhas<-pontos_amostra_linhas+(y0-1)
        # Preencher a tabela de pontos a serem amostrados
        lista_amostragem[,1]<-rep(pontos_amostra_linhas,each=n_sample_horizontal)
        lista_amostragem[,2]<-rep(pontos_amostra_colunas,n_sample_vertical)
        colnames(lista_amostragem)<-c("linha","coluna")}}

  # converter as dimensoes do objeto que sera amostrado em numero de pixel
  amostra_largura_px<-(sample_width * ncol(imagematrix))/width_size
  amostra_altura_px <-(sample_height * nrow(imagematrix))/height_size
  altura_aux<-floor(amostra_altura_px/2);largura_aux<-floor(amostra_largura_px/2)

  densidades_amostras<-NULL
  if(sample_shape=="rectangle"){
    for(i in 1: length(lista_amostragem[,1])){
      densidades_amostras[i]<-densidade_retangulo(imagematrix,lista_amostragem[i,1],lista_amostragem[i,2],amostra_altura_px,amostra_largura_px )}}else{
        if(sample_shape=="ellipse"){
          for(i in 1: length(lista_amostragem[,1])){
            densidades_amostras[i]<-densidade_elipse(imagematrix,lista_amostragem[i,1],lista_amostragem[i,2],amostra_altura_px,amostra_largura_px)}}}
  resposta<-matrix(NA,ncol=5,nrow=length(densidades_amostras))
  rownames(resposta)<-paste(rep("Sample",length(densidades_amostras)),1:length(densidades_amostras))
  resposta[,1]<-densidades_amostras
  resposta[,2]<-((nrow(imagematrix)-lista_amostragem[,1])*height_size)/nrow(imagematrix)
  resposta[,3]<-(lista_amostragem[,2]*width_size)/ncol(imagematrix)
  resposta[,4]<-lista_amostragem[,1]
  resposta[,5]<-lista_amostragem[,2]

  colnames(resposta)<-c("Sample_denseness","Height","Distance(left)","Matrix(line)","Matrix(column)")
  return(resposta)}



#' @title Plot samples from denseness_sample
#'
#' @description Plot samples from denseness_sample.
#' @param imagematrix The matrix to be analysed.
#' @param central_lines
#' @param central_collumns
#' @param width_size Real size of image width (in mm, cm, m, etc..).
#' @param height_size Real size of image height (in mm, cm, m, etc..).
#' @param sample_width Width of sample area.
#' @param sample_height Height of sample area.
#' @param sample_shape Inform the shape of sample unity used ("rectangle" or "ellipse"). See denseness_sample function.
#' @return Proportion of black pixels in samples. It do not take into account transparent pixels (when present).
#' @author Carlos Biagolini-Jr.
#' @seealso denseness_sample
#' @examples
#' # Get a matrix from your image. Here  examples provided by bwimage package.
#'
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush<-threshold_color(bush,  "jpeg", "proportional",compress_rate = 0.1)
#' a<-denseness_sample(bush, width_size=100, height_size=100, sample_width=5, sample_height=5)
#' plot_samples(bush, a[,4],a[,5], 100,100, 5, 5,"rectangle")
#' @export
plot_samples<-function(imagematrix, central_lines,central_collumns, width_size,height_size, sample_width, sample_height,sample_shape){ #  Amostrar pontos
  lista_amostragem<-cbind(central_lines,central_collumns)
  para_plotar<-imagematrix
  # converter as dimensoes do objeto que sera amostrado em numero de pixel
  amostra_largura_px<-(sample_width * ncol(imagematrix))/width_size
  amostra_altura_px <-(sample_height * nrow(imagematrix))/height_size

  # Definir tamanhos a serem pintados
  largura_pintura<-floor(amostra_largura_px/2)
  altura_pintura<-floor(amostra_altura_px/2)

  # fazer pintura
  if(sample_shape=="rectangle"){
    for(i in 1: length(lista_amostragem[,1])){
      # Funcao que pinta quadrados
      para_plotar[ c((lista_amostragem[i,1]-altura_pintura):(lista_amostragem[i,1]+altura_pintura)),c((lista_amostragem[i,2]-largura_pintura):(lista_amostragem[i,2]+largura_pintura))] <-2 }}else{
        if(sample_shape=="ellipse"){
          for(i in 1: length(lista_amostragem[,1])){
            # 1 Achar o x0 e y0 referente a lista de amostra
            amostra_rangex0<-lista_amostragem[i,1]-altura_pintura
            amostra_rangey0<-lista_amostragem[i,2]-largura_pintura

            # 2 calcular qual range que vai ser pintado
            # Em uma escala do tamanho da amostra
            matrix_corte<-matrix(NA,ncol=2,nrow=(amostra_altura_px))
            # Preencher com as informacoes de quais colunas serao pintadas para cada linha da imagem - nao estao na escala da figura original
            for( z in 1: nrow(matrix_corte)){matrix_corte[z,]<-achar_pontos_elipse(linha=z,xcentral=largura_pintura,ycentral=altura_pintura,alturatotal=amostra_altura_px,larguratotal=amostra_largura_px)
            }
            # Colocar na escala da amostra quais sao os intervalos para pintar
            escala_matrix_corte<-floor(matrix_corte+amostra_rangey0) # quais intervalos das pintadas que serao pintados
            # Pintar dentro da imagem
            for( w in 1: nrow(escala_matrix_corte)){
              para_plotar[ (amostra_rangex0+w-1),  escala_matrix_corte[w,1]:escala_matrix_corte[w,2]]<-2}}}}
  image(t(para_plotar)[,nrow(para_plotar):1], col = c("white","black","red"), xaxt = "n", yaxt = "n") }
