#' @title Aggregation index calculator
#' @description The aggregation index is a standardized estimation of the average proportion of same-color pixels around each image pixel. First, the proportion of same-color neighboring pixels (SCNP) is calculated (marginal lines and columns are excluded). Next, the SCNP for all pixels are averaged; then, given the proportion of black and white pixels, number of pixels in height and width, and location of transparent pixel (when present), the maximum and minimum possible aggregation indexes are calculated. Finally, the observed aggregation is standardized to a scale where the minimum possible value is set at zero and the maximum value is set at one. The function aggregation_index calculate the aggregation index . It works for matrix with and without transparent pixel.
#' @param imagematrix The matrix to be analysed.
#' @return The standardized aggregation index (adjusted_aggregation) and averaged SCNP (non_adjusted_aggregation). For further detail see description.
#' @examples
#' # Na example of a triangle image provided by bwimage package
#' image_file<-system.file("extdata/triangle.png",package ="bwimage" )
#' triangle_imagematrix<-threshold_color(image_file, filetype ="png") # see note below
#' aggregation_index(triangle_imagematrix)
#' # For your images, if the file is in the working directory type:
#' # threshold_color("FILE_NAME.EXTENSION", filetype ="FILE_EXTENSION")
#' # or, if the file is in the other directory:
#' # threshold_color("C:/PATH TO FILE FOLDER/YOUR_FILE_NAME.EXTENSION", filetype ="FILE_EXTENSION")
#' @export
aggregation_index <- function(imagematrix) {
  if (length(subset(as.vector(imagematrix), is.na(as.vector(imagematrix)))) > 1) {adjusted_aggregation <- adjusted_aggregation_with_transparence(imagematrix)
    non_adjusted_aggregation <- basic_aggregation_index(imagematrix)
  } else {
    adjusted_aggregation <- adjusted_aggregation_wo_transparence(imagematrix)
    non_adjusted_aggregation <- basic_aggregation_index(imagematrix)}

  resposta <- c(adjusted_aggregation, non_adjusted_aggregation)
  names(resposta) <- c("adjusted_aggregation", "non_adjusted_aggregation")
  return(resposta)}
#' @title Image to matrix - Single
#'
#' @description Convert a single image into a matrix
#' @param filename Name of the file to be load - ex: "Figure01.JPG".
#' @param filetype Type of the file to be load. Compatible file types: ".JPGE", ".JPG" or ".PNG".
#' @param compress_method For high resolution files, i.e. numbers of pixels in width and height, it is suggested to reduce the resolution to create a smaller matrix, it strongly reduce GPU usage and time necessary to run analyses. On the other hand, by reducing resolution, it will also reduce the accuracy of data description. Methods for image compression: (i) frame_fixed: compress images to a desired target width and height; (ii) proportional:  compress the image by a given ratio provide in the argument "proportion"; (iii) width_fixed: compress images to get a target width. The image height will also be reduced, in the same intensity. For instance, if the original file had 1000 pixel in width, and width_fixed was set to 100, the height reduction will reduce in a rate of 0.1 (100/1000) ; (iv) height_fixed: analogous to width_fixed, but assumes height as reference.
#' @param compress_rate Compress rate to by apply if compress_method=proportional. Note:  it should be ser as number range from 0 to 1 .
#' @param target_width Target width to be used if compress_method=frame_fixed or compress_method= width_fixed.
#' @param target_height Target height to be used if compress_method=frame_fixed or compress_method= height_fixed.
#' @param black_regulation For each pixel, the intensity of red, green and blue is averaged and compared to a black_regulation (threshold). If the average intensity is less than the black_regulation (default is 0.5) the pixel will be set as black, otherwise it will be white.
#' @param transparency_regulation For PNG images, the alpha channel is used to set transparent pixels, i.e. alpha channel values above transparency_regulation (a threshold) will set the pixel as transparent, default is 0.5.  NOTE: In the data matrix the value 1 represents black pixels, 0 represents white pixels and NA represents transparent pixels.
#' @return A matrix of 0, 1 and NA representing white, black and transparent pixels, respectively.
#' @examples
# An example of a triangle draw provided by bwimage package
#' triangle<-system.file("extdata/triangle.PNG",package ="bwimage")
#' triangle_imagematrix<-threshold_color(triangle,  filetype ="png")
#' # An example of a canopy photo provided by bwimage package
#' canopy<-system.file("extdata/canopy.JPG",package ="bwimage")
#' canopy_imagematrix<-threshold_color(canopy, "jpeg", "proportional",compress_rate = 0.25)
#'  # an example of a bird nestwall photo provided by bwimage package
#' nestwall<-system.file("extdata/bird_nestwall.png",package ="bwimage")
#' nestwall_imagematrix<-threshold_color(nestwall, "width_fixed", target_width=500)
#'

#' @export
threshold_color <- function(filename, filetype = "jpeg", compress_method = "none", compress_rate = 1,
                            target_width = 100, target_height = 100, black_regulation = 0.5, transparency_regulation = 0.5) {
  # Validando os dados
  if (filetype == "png"|filetype == "PNG") {
    require(png)
  }
  if (filetype == "jpeg"|filetype =="jpg"|filetype =="JPEG"|filetype =="JPG") {
    require(jpeg)
  }
  if (!(filetype == "png"|filetype == "PNG") & !(filetype == "jpeg"|filetype =="jpg"|filetype =="JPEG"|filetype =="JPG")) {
    stop("Provide a valid file type - i.e. png or jpeg")
  }
  if (compress_rate <= 0 | compress_rate > 1) {
    stop("compress_rate must be a number between 0 and 1 ")
  }
  if (black_regulation >= 1 | black_regulation < 0) {
    stop("black_regulation must be a number between 0 and 1")
  }
  if (!(compress_method == "none" | compress_method == "frame_fixed" | compress_method == "proportional" |
        compress_method == "width_fixed" | compress_method == "height_fixed")) {
    stop("Provide a valid reduction method")
  }

  if (filetype == "jpeg"|filetype == "jpg"|filetype == "JPEG"|filetype == "JPG") {
    imagematrix <- readJPEG(filename)
  }
  if (filetype == "png"|filetype == "PNG") {
    imagematrix <- readPNG(filename)
  }
  # Calcular o fator de correcao para a figura produzida
  if (compress_method == "none") {
    espaco_entre_pixeis_linha <- 1
    espaco_entre_pixeis_coluna <- 1
    linhas_imagem <- nrow(imagematrix)
    colunas_imagem <- ncol(imagematrix)
  } else {
    if (compress_method == "frame_fixed") {
      linhas_imagem <- target_height  # Quantas linhas vao ter na nova figura
      colunas_imagem <- target_width  # Quantas colunas vao ter na nova figura
      espaco_entre_pixeis_linha <- nrow(imagematrix)/linhas_imagem  # espaco entre os pixeis das linhas na figura orginal
      espaco_entre_pixeis_coluna <- ncol(imagematrix)/colunas_imagem  # espaco entre os pixeis das colunas na figura orginal
    } else {
      if (compress_method == "proportional") {
        linhas_imagem <- floor(nrow(imagematrix) * compress_rate)  # Quantas linhas vao ter na nova figura
        colunas_imagem <- floor(ncol(imagematrix) * compress_rate)  # Quantas colunas vao ter na nova figura
        espaco_entre_pixeis_linha <- nrow(imagematrix)/linhas_imagem  # espaco entre os pixeis das linhas na figura orginal
        espaco_entre_pixeis_coluna <- ncol(imagematrix)/colunas_imagem  # espaco entre os pixeis das colunas na figura orginal
      } else {
        if (compress_method == "width_fixed") {
          colunas_imagem <- target_width
          linhas_imagem <- floor((nrow(imagematrix) * target_width)/ncol(imagematrix))
          espaco_entre_pixeis_linha <- nrow(imagematrix)/linhas_imagem  # espaco entre os pixeis das linhas na figura orginal
          espaco_entre_pixeis_coluna <- ncol(imagematrix)/colunas_imagem  # espaco entre os pixeis das colunas na figura orginal

        } else {
          if (compress_method == "height_fixed") {
            linhas_imagem <- target_height  # Quantas linhas vao ter na nova figura
            colunas_imagem <- floor((ncol(imagematrix) * target_height)/nrow(imagematrix))  # Quantas colunas vao ter na nova figura
            espaco_entre_pixeis_linha <- nrow(imagematrix)/linhas_imagem  # espaco entre os pixeis das linhas na figura orginal
            espaco_entre_pixeis_coluna <- ncol(imagematrix)/colunas_imagem  # espaco entre os pixeis das colunas na figura orginal
          }}}}}
  matrix_cores <- matrix(NA, nrow = linhas_imagem, ncol = colunas_imagem)
if (filetype == "jpeg" | filetype == "jpg") {
    for (c in 1:ncol(matrix_cores)) {
      for (l in 1:nrow(matrix_cores)) {
        if (sum(imagematrix[floor(l * espaco_entre_pixeis_linha), floor(c * espaco_entre_pixeis_coluna),
                            1:3]) < ((black_regulation * 1.5) + 1.5)) {
          matrix_cores[l, c] <- 1
        } else {
          matrix_cores[l, c] <- 0}}}}
  if (filetype == "png") {
    for (c in 1:ncol(matrix_cores)) {
      for (l in 1:nrow(matrix_cores)) {
        if (imagematrix[floor(l * espaco_entre_pixeis_linha), floor(c * espaco_entre_pixeis_coluna),
                        4] < transparency_regulation) {
          matrix_cores[l, c] <- NA
        } else {
          # deixando as caixas
          if (sum(imagematrix[floor(l * espaco_entre_pixeis_linha), floor(c * espaco_entre_pixeis_coluna),
                              1:3]) < ((black_regulation * 1.5) + 1.5)) {
            matrix_cores[l, c] <- 1
          } else {
            matrix_cores[l, c] <- 0}}}}}
  return(matrix_cores)}
#' @title Image to matrix - List
#'
#' @description Convert two or more images into a list of matrices
#' @param list_names An object contains the names of the files to be load. Ex: file_names<-c("Figure01.JPG","Figure02.JPG","Figure03.JPG","Figure04.JPG")
#' @param filetype Type of the file to be load. Compatible file types: ".JPGE", ".JPG" or ".PNG".
#' @param compress_method For high resolution files, i.e. numbers of pixels in width and height, it is suggested to reduce the resolution to create a smaller matrix, it strongly reduce GPU usage and time necessary to run analyses. On the other hand, by reducing resolution, it will also reduce the accuracy of data description. Methods for image compression: (i) frame_fixed: compress images to a desired target width and height; (ii) proportional:  compress the image by a given ratio provide in the argument "proportion"; (iii) width_fixed: compress images to get a target width. The image height will also be reduced, in the same intensity. For instance, if the original file had 1000 pixel in width, and width_fixed was set to 100, the height reduction will reduce in a rate of 0.1 (100/1000) ; (iv) height_fixed: analogous to width_fixed, but assumes height as reference.
#' @param compress_rate Compress rate to by apply if compress_method=proportional. Note:  it should be ser as number range from 0 to 1 .
#' @param target_width Target width to be used if compress_method=frame_fixed or compress_method= width_fixed.
#' @param target_height Target height to be used if compress_method=frame_fixed or compress_method= height_fixed.
#' @param black_regulation For each pixel, the intensity of red, green and blue is averaged and compared to a black_regulation (threshold). If the average intensity is less than the black_regulation (default is 0.5) the pixel will be set as black, otherwise it will be white.
#' @param transparency_regulation For PNG images, the alpha channel is used to set transparent pixels, i.e. alpha channel values above transparency_regulation (a threshold) will set the pixel as transparent, default is 0.5.  NOTE: In the data matrix the value 1 represents black pixels, 0 represents white pixels and NA represents transparent pixels.
#' @return A matrix of 0, 1 and NA representing white, black and transparent pixels, respectively.
#' @examples
#' # Image examples provided by bwimage package
#' flag<-system.file("extdata/flag.png",package ="bwimage")
#' star<-system.file("extdata/star.png",package ="bwimage")
#' triangle<-system.file("extdata/triangle.png",package ="bwimage")
#'
#' # Convert images to a list of matrices
#' working_matrices<-threshold_image_list(c(flag,star,triangle), filetype ="png")
#'
#' # For further information, see
#' help(threshold_color)
#' @export
threshold_image_list <- function(list_names, filetype = "jpeg", compress_method = "none", compress_rate = 1,
                                 target_width = 100, target_height = 100, black_regulation = 0.5, transparency_regulation = 0.5) {
  pb <- txtProgressBar(min = 0, max = length(list_names), style = 3)
  lista_de_saida <- list(NA)
  for (i in 1:length(list_names)) {
    matrix_para_analise <- threshold_color(filename = list_names[i], filetype = filetype, compress_method = compress_method,
                                           compress_rate = compress_rate, black_regulation = black_regulation, target_width = target_width,
                                           target_height = target_height, transparency_regulation = transparency_regulation)
    lista_de_saida[[i]] <- matrix_para_analise
    setTxtProgressBar(pb, i)
  }
  close(pb)
  resposta <- lista_de_saida
  return(resposta)}

#' @title Summary of image information
#'
#' @description Provide the information of: number of black, white and transparent pixels, total number of pixels, height and width size.
#' @param imagematrix The matrix to be analysed.
#' @return Basic informations of matrix
#' @details
#'  Description of the bush image example provided by bwimage package: The imagem comprises a vegetation section of 30x100cm. To get this photo, in one of the 100cm side of the panel of 100x100cm covered with a white cloth was placed perpendicular to the ground. A plastic canvas of 50x100cm was used to cover the vegetation along a narrow strip in front of a camera positioned on a tripod at a height of 55 cm. A photograph of the portion of standing vegetation against the white cloth was taken.
#' @examples
#' # First, get a matrix for your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg", "proportional",compress_rate = 0.1)
#' # For further information, see : help(threshold_color)
#'
#' image_information(bush_imagematrix)
#' @export
image_information <- function(imagematrix) {
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

  names(resultado_em_processo) <- c("n_black_pixels", "n_white_pixels", "n_transparent", "n_pixels",
                                    "height_BW_picture", "width_BW_picture")
  return(resultado_em_processo)}


#' @title Denseness total
#'
#' @description Proportion of black pixels in relation to all pixels. It do not take into account transparent pixels (when present).
#' @param imagematrix The matrix to be analysed.
#' @return Proportion of black pixels in relation to all pixels. It do not take into account transparent pixels (when present).
#' @details
#'  Description of the bush image example provided by bwimage package: The imagem comprises a vegetation section of 30x100cm. To get this photo, in one of the 100cm side of the panel of 100x100cm covered with a white cloth was placed perpendicular to the ground. A plastic canvas of 50x100cm was used to cover the vegetation along a narrow strip in front of a camera positioned on a tripod at a height of 55 cm. A photograph of the portion of standing vegetation against the white cloth was taken.
#' @examples
#' # First, get a matrix for your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg", "proportional",compress_rate = 0.1)
#' # For further information, see : help(threshold_color)
#'
#' denseness_total(bush_imagematrix)
#' @export
denseness_total <- function(imagematrix) {
  dados_pixeis <- subset(as.vector(imagematrix), !is.na(as.vector(imagematrix)))
  n_black_pixels <- sum(dados_pixeis)
  n_pixels <- length(dados_pixeis)
  p_black_pixels <- n_black_pixels/n_pixels
  return(p_black_pixels)}

#' @title Denseness by row
#'
#' @description Calculate the denseness (proportion of black pixel in relation to the total number of pixels) for a given number of sections (n_sections). n_sections should be set as a number, in this situation denseness_row will break the original matrix in slices, and apply denseness_total () function for each section. For instance, in a matrix of 1000x1000 if n_sections = 10, it will break to 10 sections of 100x1000 and analyze it. In other words, the sections will be the following sections of the original matrix [1:100, 1:1000] , [101:200, 1:1000] , [201:300, 1:1000] ,  [301:400, 1:1000] , [401:500, 1:1000] , [501:600, 1:1000] , [601:700, 1:1000] , [701:800, 1:1000] , [801:900, 1:1000] , [901:1000, 1:1000] .The default for parameter n_sections is "all", it will calculate denseness for each row of pixel. In other words, it will break the image in a number of section equal to the image pixel height.
#' @param imagematrix The matrix to be analysed.
#' @param n_sections Break the image in this number of rows.
#' @return denseness, mean and standard deviations
#' @details
#'  Description of the bush image example provided by bwimage package: The imagem comprises a vegetation section of 30x100cm. To get this photo, in one of the 100cm side of the panel of 100x100cm covered with a white cloth was placed perpendicular to the ground. A plastic canvas of 50x100cm was used to cover the vegetation along a narrow strip in front of a camera positioned on a tripod at a height of 55 cm. A photograph of the portion of standing vegetation against the white cloth was taken.
#' @examples
#' # First, get a matrix for your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg", "proportional",compress_rate = 0.1)
#' # For further information, see : help(threshold_color)
#'
#' # Calculate vegetation denseness in 20 row sections
#' denseness_row(bush_imagematrix,20)
#'
#' # See also
#' help(denseness_total)
#' @references
#' Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
#' @export
denseness_row <- function(imagematrix, n_sections = "all") {
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
  names(resposta) <- c("denseness by row", "mean denseness row", "SD denseness row")
  return(resposta)}

#' @title Denseness by column
#'
#' @description Calculate the denseness (proportion of black pixel in relation to the total number of pixels) for a given number of sections (n_sections). n_sections should be set as a number, in this situation denseness_column will break the original matrix in slices, and apply denseness_total () function for each section. For instance, in a matrix of 1000x1000 if n_sections = 10, it will break to 10 sections of 1000x100 and analyze it. In other words, the sections will be the following sections of the original matrix [1:1000, 1:100] ,[ 1:1000,101:200] ,[ 1:1000,201:300] ,[ 1:1000,301:400] ,[ 1:1000,401:500] ,[ 1:1000,501:600] ,[ 1:1000,601:700] ,[ 1:1000,701:800] ,[ 1:1000,801:900] ,[ 1:1000,901:1000]. The default for parameter n_sections is "all", it will calculate denseness for each column of pixel. In other words, it will break the image in a number of section equal to the image pixel width.
#' @param imagematrix The matrix to be analysed.
#' @param n_sections Break the image in this number of columns.
#' @return denseness, mean and standard deviations
#' @details
#'  Description of the bush image example provided by bwimage package: The imagem comprises a vegetation section of 30x100cm. To get this photo, in one of the 100cm side of the panel of 100x100cm covered with a white cloth was placed perpendicular to the ground. A plastic canvas of 50x100cm was used to cover the vegetation along a narrow strip in front of a camera positioned on a tripod at a height of 55 cm. A photograph of the portion of standing vegetation against the white cloth was taken.
#' @examples
#' # First, get a matrix for your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)
#' # For further information, see : help(threshold_color)
#'
#' # Calculate vegetation denseness in 20 column sections
#' denseness_column(bush_imagematrix,20)
#'
#' # See also
#' help(denseness_total)
#' @references
#' Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
#' @export
denseness_column <- function(imagematrix, n_sections = "all") {
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
  names(resposta) <- c("denseness by row", "mean denseness row", "SD denseness row")
  return(resposta)
}

#' @title Hole finder
#'
#' @description Description of when a sequence of  same color pixel start and end.
#' @param section Section to be analysed.
#' @return Description of start and end of each same color sequence
#' @details
#'  Description of the bush image example provided by bwimage package: The imagem comprises a vegetation section of 30x100cm. To get this photo, in one of the 100cm side of the panel of 100x100cm covered with a white cloth was placed perpendicular to the ground. A plastic canvas of 50x100cm was used to cover the vegetation along a narrow strip in front of a camera positioned on a tripod at a height of 55 cm. A photograph of the portion of standing vegetation against the white cloth was taken.
#' @examples
#' # First, get a matrix for your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)
#' # For further information, see : help(threshold_color)
#'
#' # Find pixel hole sections in the column 200 of bush image
#' hole_section(bush_imagematrix[,200])
#'
#' # Find pixel hole sections in the row 250 of bush image
#' hole_section(bush_imagematrix[250,])
#' # see also:
#' help(hole_section_data)
#' @references
#' Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
#' @export
hole_section <- function(section) {
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
#' @description Summary information of holes of a given color in a given section.
#' @param section Section to be analysed.
#' @param color Color of the hole (0 or 1).
#' @return Number, mean, standard deviation, minimum and maximum size of hole sections.
#' @details
#'  Description of the bush image example provided by bwimage package: The imagem comprises a vegetation section of 30x100cm. To get this photo, in one of the 100cm side of the panel of 100x100cm covered with a white cloth was placed perpendicular to the ground. A plastic canvas of 50x100cm was used to cover the vegetation along a narrow strip in front of a camera positioned on a tripod at a height of 55 cm. A photograph of the portion of standing vegetation against the white cloth was taken.
#' @examples
#' # First, get a matrix for your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)
#' # For further information, see : help(threshold_color)
#'
#' # Detail information of white (0) holes sections in the column 200 of bush image
#' hole_section_data(bush_imagematrix[,200], color = 0)
#'
#' # Detail information of black (1) holes sections in the row 250 of bush image
#'  hole_section_data(bush_imagematrix[250,], color = 1)
#'
#'  # See also
#'  help(hole_section)
#'
#' @references
#' Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.

#' @export
hole_section_data <- function(section, color = 0) {
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
  names(resposta) <- c("Number of sections", "Mean sections size", "Sections size SD", "Minimum sections size",
                       "Maximum sections size")
  return(resposta)
}

#' @title Holes - columns
#'
#' @description Summary information of holes in a given number of columns (n_sections). n_sections must be set as a number, in this situation hole_columm will sample columns, and apply hole_section_data() function for each section. Next, all results will be display on hole_columm output. Example of how column sample works: in a matrix of 250x250 if n_sections =  5 , it will sample columns 1,51,101,151, and 201 and analyze it. In other words, the sections will be following sections of the original matrix [1:250,1] , [1:250,51], [1:250,101], [1:250,151], [1:250,201]. The default for parameter n_sections is "all", it will calculate hole_section_data() for each column of pixel. In other words, it will break the image in a number of section equal to the image pixel width.
#' @param imagematrix The matrix to be analysed.
#' @param color Color of the hole (0 or 1).
#' @param n_sections Get data of this number of columns.
#' @return Summary of hole in columms
#' @details
#'  Description of the bush image example provided by bwimage package: The imagem comprises a vegetation section of 30x100cm. To get this photo, in one of the 100cm side of the panel of 100x100cm covered with a white cloth was placed perpendicular to the ground. A plastic canvas of 50x100cm was used to cover the vegetation along a narrow strip in front of a camera positioned on a tripod at a height of 55 cm. A photograph of the portion of standing vegetation against the white cloth was taken.
#' @examples
#' # First, get a matrix for your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)
#' # For further information, see : help(threshold_color)
#'
#' # Information of white (i.e. 0s in matrix) holes in 5 columns uniformly sample among matrix.
#' hole_columm(bush_imagematrix, n_sections=5 )
#'
#' # Information of black (i.e. 1s in matrix) holes in 20 columns uniformly sample among matrix.
#' hole_columm(bush_imagematrix, n_sections=20 )
#'
#'  # See also
#'  help(hole_section_data)
#' @references
#' Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
#' @export
hole_columm <- function(imagematrix, color = 0, n_sections = "all") {
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
  names(resposta) <- c("Number of sections", "Mean sections size", "Sections size SD", "Minimum sections size",
                       "Maximum sections size", "Stratum with largest hole count")
  return(resposta)
}

#' @title Holes - rows
#' @description Summary information of holes in a given number of rows (n_sections). n_sections must be set as a number, in this situation hole_row will sample rows, and apply hole_section_data() function for each section. Next, all results will be display on hole_columm output. Example of how row sample works: in a matrix of 250x250 if n_sections =  5 , it will sample rows 1,51,101,151, and 201 and analyze it. In other words, the sections will be following sections of the original matrix [1,1:250] , [51,1:250] , [101,1:250] , [151,1:250] , [201,1:250]. The default for parameter n_sections is "all", it will calculate hole_section_data() for each row of pixel. In other words, it will break the image in a number of section equal to the image pixel height.
#' @param color Color of the hole (0 or 1).
#' @param n_sections Break the image in this number of rows.
#' @return Summary of hole in rows
#' @details
#'  Description of the bush image example provided by bwimage package: The imagem comprises a vegetation section of 30x100cm. To get this photo, in one of the 100cm side of the panel of 100x100cm covered with a white cloth was placed perpendicular to the ground. A plastic canvas of 50x100cm was used to cover the vegetation along a narrow strip in front of a camera positioned on a tripod at a height of 55 cm. A photograph of the portion of standing vegetation against the white cloth was taken.
#' @examples
#' # First, get a matrix for your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)
#' # For further information, see : help(threshold_color)
#'
#' # Information of white (i.e. 0s in matrix) holes in 10 rows uniformly sample among matrix.
#' hole_row(bush_imagematrix, n_sections=10 )
#'
#' # Information of black (i.e. 1s in matrix) holes in 15 rows uniformly sample among matrix.
#' hole_row(bush_imagematrix, n_sections=15 )
#'
#'  # See also
#'  help(hole_section_data)
#' @references
#' Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
#' @export
hole_row <- function(imagematrix, color = 0, n_sections = "all") {
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
  names(resposta) <- c("Number of sections", "Mean sections size", "Sections size SD", "Minimum sections size",
                       "Maximum sections size", "Stratum with largest hole count")
  return(resposta)
}

#' @title Light gap
#'
#' @description Left and right distances from first black pixel to image edge.
#' @param imagematrix The matrix to be analysed.
#' @param width_size Real size of image width (in mm, cm, m, etc..).
#' @param scale If FALSE do not ajust the output for real size.
#' @return Distances without black pixel in each side of the picture
#' @return denseness, mean and standard deviations
#' @details
#'  Description of the bush image example provided by bwimage package: The imagem comprises a vegetation section of 30x100cm. To get this photo, in one of the 100cm side of the panel of 100x100cm covered with a white cloth was placed perpendicular to the ground. A plastic canvas of 50x100cm was used to cover the vegetation along a narrow strip in front of a camera positioned on a tripod at a height of 55 cm. A photograph of the portion of standing vegetation against the white cloth was taken.
#' @examples
#' # First, get a matrix for your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)
#' # For further information, see : help(threshold_color)
#'
#' # Calculate vegetation Light gap in the bush image matrix
#' light_gap(bush_imagematrix,width_size=100)
#' @references
#' Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
#' @export
light_gap <- function(imagematrix, width_size = NA, scale = TRUE) {
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
#' @title Highest black pixel - All image
#'
#' @description Find the higher black pixel in the whole image.
#' @param imagematrix The matrix to be analysed.
#' @param height_size Real size of image width (in mm, cm, m, etc..).
#' @return Height of the highest black pixel
#' @details
#'  Description of the bush image example provided by bwimage package: The imagem comprises a vegetation section of 30x100cm. To get this photo, in one of the 100cm side of the panel of 100x100cm covered with a white cloth was placed perpendicular to the ground. A plastic canvas of 50x100cm was used to cover the vegetation along a narrow strip in front of a camera positioned on a tripod at a height of 55 cm. A photograph of the portion of standing vegetation against the white cloth was taken.
#' @examples
#' # First, get a matrix for your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)
#' # For further information, see : help(threshold_color)
#'
#' # Calculate height of the highest black pixel in the bush image matrix
#' heigh_maximum(bush_imagematrix,height_size=100)
#' # Conclusions: The highest vegetation unit ,i.e. highest black pixel, is 84.4 cm above ground.
#' @references
#' Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
#' @export
heigh_maximum <- function(imagematrix, height_size = NA) {
  if (is.na(height_size) | height_size <= 0) {
    stop("height_size not found")
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

  names(altura_maxima) <- c("Height of the highest black pixel")
  return(altura_maxima)
}

#' @title Highest black pixel by sections
#'
#' @description Break the original matrix in a number of section ( n_sections), then find the higher black pixel in each image sections.
#' @param imagematrix The matrix to be analysed.
#' @param n_sections Break the image in this number of columns.
#' @param height_size Real size of image height (in mm, cm, m, etc..).
#' @return Mean, standard deviations and size of the highest black pixel in each section. Results are present in the scale provide on height_size.
#' @details
#'  Description of the bush image example provided by bwimage package: The imagem comprises a vegetation section of 30x100cm. To get this photo, in one of the 100cm side of the panel of 100x100cm covered with a white cloth was placed perpendicular to the ground. A plastic canvas of 50x100cm was used to cover the vegetation along a narrow strip in front of a camera positioned on a tripod at a height of 55 cm. A photograph of the portion of standing vegetation against the white cloth was taken.
#' @examples
#' # First, get a matrix for your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)
#' # For further information, see : help(threshold_color)
#'
#' # Profile  of  highest black pixels on sections of the bush image matrix
#' altitudinal_profile(bush_imagematrix,n_sections = 10, height_size=100)
#' # Conclusions: i) the mean height of the highest black pixel is 45.28 cm;
#' #  ii) standard deviation of highest black height is 21.54;
#' # iii) information of the height of the highest black
#' @references
#' Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
#' @export
altitudinal_profile <- function(imagematrix, n_sections = NA, height_size = NA) {
  if (is.na(height_size) | height_size <= 0) {
    stop("height_size not found")
  }
  if (is.na(n_sections) | n_sections <= 0) {
    stop("Number of n_sections not found")
  }
  if ((length(imagematrix[1, ])/2) < n_sections) {
    stop(paste("Sections width must have at least two cells. Please choose a number of n_sections lower or equal to ",
               length(imagematrix[1, ])/2))
  }
  if (n_sections == "all") {
    n_sections <- ncol(imagematrix)
  } else {
    if (ncol(imagematrix) < n_sections) {
      n_sections <- ncol(imagematrix)
    }}
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
  names(resposta) <- c("Mean of height of the highest black pixel by sections", "SD Height of the highest black pixel by sections",
                       "Height of the highest black pixel by sections")
  return(resposta)}

#' @title Cumulative denseness
#'
#' @description Proportion of black pixel below each matrix line.
#' @param imagematrix The matrix to be analysed.
#' @details
#'  Description of the bush image example provided by bwimage package: The imagem comprises a vegetation section of 30x100cm. To get this photo, in one of the 100cm side of the panel of 100x100cm covered with a white cloth was placed perpendicular to the ground. A plastic canvas of 50x100cm was used to cover the vegetation along a narrow strip in front of a camera positioned on a tripod at a height of 55 cm. A photograph of the portion of standing vegetation against the white cloth was taken.
#' @examples
#' # First, get a matrix for your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)
#' # For further information, see : help(threshold_color)
#'
#' # See the proportion of black pixels (1) below each bush image matrix row
#' heigh_propotion(bush_imagematrix)
#' @references
#' Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
#' @export
heigh_propotion <- function(imagematrix) {
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
#' @param proportion proportion of denseness to test
#' @param height_size Real size of image height (in mm, cm, m, etc..).
#' @details
#'  Description of the bush image example provided by bwimage package: The imagem comprises a vegetation section of 30x100cm. To get this photo, in one of the 100cm side of the panel of 100x100cm covered with a white cloth was placed perpendicular to the ground. A plastic canvas of 50x100cm was used to cover the vegetation along a narrow strip in front of a camera positioned on a tripod at a height of 55 cm. A photograph of the portion of standing vegetation against the white cloth was taken.
#' @examples
#' # First, get a matrix for your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)
#' # For further information, see : help(threshold_color)
#'
#' # See the proportion of black pixels (1) below each bush image matrix row
#' heigh_propotion_test(bush_imagematrix,0.75,100)
#' # Conclusion: in this imagem, 75 percent of the vegetation is hold below 31.2 cm.
#' @references
#' Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
#' @export
heigh_propotion_test <- function(imagematrix, proportion = NA, height_size = NA) {
  if (is.na(height_size) | height_size <= 0) {
    stop("height_size not found")
  }
  if (is.na(proportion) | proportion < 0 | proportion > 1) {
    stop("You must set a proportion value ranging from 0-1 to be tested")
  }
  total_acumulado <- heigh_propotion(imagematrix)
  altura <- sum(proportion > round(total_acumulado, 5)) + 1  # Como e um acomulado, quando voce obseva o primeiro 1 isso quer dizer que ali estava a ultima celula com dado, e foi la que foi a partir daquela linha que foi observado esse valor
  altura <- altura * (height_size/length(total_acumulado))  # colocar na escala

  names(altura) <- c(paste("Height below whitch", proportion, "of the vegetation denseness is located"))
  return(altura)
}

#' @title Top line
#' @description Line running along the crest of highest black pixel.
#' @param imagematrix The matrix to be analysed.
#' @param height_size Real size of image height (in mm, cm, m, etc..).
#' @param width_size Real size of image width (in mm, cm, m, etc..).
#' @return Line size that cover black pixels
#' @details
#'  Description of the bush image example provided by bwimage package: The imagem comprises a vegetation section of 30x100cm. To get this photo, in one of the 100cm side of the panel of 100x100cm covered with a white cloth was placed perpendicular to the ground. A plastic canvas of 50x100cm was used to cover the vegetation along a narrow strip in front of a camera positioned on a tripod at a height of 55 cm. A photograph of the portion of standing vegetation against the white cloth was taken.
#' @examples
#' # First, get a matrix for your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)
#' # For further information, see : help(threshold_color)
#'
#' # See the proportion of black pixels (1) below each bush image matrix row
#' topline(bush_imagematrix,100,100)
#' # Conclusion: topline size is 785.6 cm.
#' @references
#' Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
#' @export
topline <- function(imagematrix, height_size = NA, width_size = NA) {
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

  # Criar fun??o para achar a maior coluna
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
  names(resposta) <- "topline length"
  return(resposta)
}
###################################################################################################################################################################################################################################################################################################################################################################### FUNCOES INTERNAS ######################################################################################################################################################################################################################################################################################################################################################################
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
basic_aggregation_index <- function(imagematrix) {
  matrix_agregacao <- aggregation_matrix(imagematrix)
  values <- subset(as.vector(matrix_agregacao), !is.na(as.vector(matrix_agregacao)))  # Jogando todos os dados para um unico objeto
  average_aggregation <- mean(values)
  return(average_aggregation)}

# Minimum aggregation index calculator for matrix without transparent pixels - Based on a given matrix, calculate de minimum possible aggregation index for matrix without transparent pixels.
min_aggregation_wo_transparence <- function(imagematrix) {
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
max_aggregation_wo_transparence <- function(imagematrix) {
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
adjusted_aggregation_wo_transparence <- function(imagematrix) {
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
min_aggregation_with_transparence <- function(imagematrix) {
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
max_aggregation_with_transparence <- function(imagematrix) {
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
adjusted_aggregation_with_transparence <- function(imagematrix) {
  min.index <- min_aggregation_with_transparence(imagematrix)
  max.index <- max_aggregation_with_transparence(imagematrix)
  observed <- basic_aggregation_index(imagematrix)
  range <- max.index - min.index
  obs.corrigido <- observed - min.index
  valor_corrigido <- obs.corrigido/range
  return(valor_corrigido)
}

