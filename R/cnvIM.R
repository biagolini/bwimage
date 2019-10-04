# Convert image to matrix

#' @title Image to matrix - Single
#'
#' @description Convert a single image into a matrix
#' @param filename Name of the file to be load - ex: "Figure01.JPG".
#' @param filetype Type of the file to be load. Compatible file types: ".JPGE", ".JPG" or ".PNG".
#' @param compress_method For high resolution files, i.e. numbers of pixels in width and height, it is suggested to reduce the resolution to create a smaller matrix, it strongly reduce GPU usage and time necessary to run analyses. On the other hand, by reducing resolution, it will also reduce the accuracy of data description. Methods for image compression: (i) frame_fixed: compress images to a desired target width and height; (ii) proportional:  compress the image by a given ratio provide in the argument "proportion"; (iii) width_fixed: compress images to get a target width. The image height will also be reduced, in the same intensity. For instance, if the original file had 1000 pixel in width, and width_fixed was set to 100, the height reduction will reduce in a rate of 0.1 (100/1000) ; (iv) height_fixed: analogous to width_fixed, but assumes height as reference.
#' @param compress_rate Compress rate to by apply if compress_method=proportional. Note:  it should be ser as number range from 0 to 1 .
#' @param target_width Target width to be used if compress_method=frame_fixed or compress_method= width_fixed.
#' @param target_height Target height to be used if compress_method=frame_fixed or compress_method= height_fixed.
#' @param black_regulation For each pixel, the intensity of red, green and blue is averaged and compared to a black_regulation (threshold).
#' If the average intensity is less than the black_regulation (default is 0.5) the pixel will be set as black, otherwise it will be white.
#' @param transparency_regulation For PNG images, the alpha channel is used to set transparent pixels, i.e. alpha channel values above transparency_regulation (a threshold) will set the pixel as transparent, default is 0.5.  NOTE: In the data matrix the value 1 represents black pixels, 0 represents white pixels and NA represents transparent pixels.
#' @return A matrix of 0, 1 and NA representing white, black and transparent pixels, respectively.
#' @author Carlos Biagolini-Jr.
#' @examples
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' threshold_color(bush,"jpeg", "frame_fixed",target_width = 15,target_height=15)
#'
#' # For your images, if the file is in the working directory type:
#' # threshold_color("FILE_NAME.EXTENSION", filetype ="FILE_EXTENSION")
#' # or, if the file is in the other directory:
#' # threshold_color("C:/PATH TO FILE FOLDER/YOUR_FILE_NAME.EXTENSION", "FILE_EXTENSION")
#' @export
threshold_color <-
  function(filename, filetype = "jpeg", compress_method = "none", compress_rate = 1,
           target_width = 100, target_height = 100, black_regulation = 0.5, transparency_regulation = 0.5) {
    # Validando os dados
    if (filetype == "png"|filetype == "PNG") {
      requireNamespace("png")
    }
    if (filetype == "jpeg"|filetype =="jpg"|filetype =="JPEG"|filetype =="JPG") {
      requireNamespace("jpeg")
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
#' @param list_names An object contains the names of the files.
#' @param filetype Type of the file to be load. Compatible file types: ".JPGE", ".JPG" or ".PNG".
#' @param compress_method For high resolution files, i.e. numbers of pixels in width and height, it is suggested to reduce the resolution to create a smaller matrix, it strongly reduce GPU usage and time necessary to run analyses. On the other hand, by reducing resolution, it will also reduce the accuracy of data description. Methods for image compression: (i) frame_fixed: compress images to a desired target width and height; (ii) proportional:  compress the image by a given ratio provide in the argument "proportion"; (iii) width_fixed: compress images to get a target width. The image height will also be reduced, in the same intensity. For instance, if the original file had 1000 pixel in width, and width_fixed was set to 100, the height reduction will reduce in a rate of 0.1 (100/1000) ; (iv) height_fixed: analogous to width_fixed, but assumes height as reference.
#' @param compress_rate Compress rate to by apply if compress_method=proportional. Note:  it should be ser as number range from 0 to 1 .
#' @param target_width Target width to be used if compress_method=frame_fixed or compress_method= width_fixed.
#' @param target_height Target height to be used if compress_method=frame_fixed or compress_method= height_fixed.
#' @param black_regulation For each pixel, the intensity of red, green and blue is averaged and compared to a black_regulation (threshold). If the average intensity is less than the black_regulation (default is 0.5) the pixel will be set as black, otherwise it will be white.
#' @param transparency_regulation For PNG images, the alpha channel is used to set transparent pixels, i.e. alpha channel values above transparency_regulation (a threshold) will set the pixel as transparent, default is 0.5.  NOTE: In the data matrix the value 1 represents black pixels, 0 represents white pixels and NA represents transparent pixels.
#' @return A matrix of 0, 1 and NA representing white, black and transparent pixels, respectively.
#' @author Carlos Biagolini-Jr.
#' @seealso threshold_color
#' @examples
#' # Image examples provided by bwimage package
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' canopy<-system.file("extdata/canopy.JPG",package ="bwimage")
#'
#' # Convert images to a list of matrices
#' working_matrices<-threshold_image_list(c(bush,canopy), "jpeg", "proportional", compress_rate = 0.1)
#' @export
threshold_image_list <-
  function(list_names, filetype = "jpeg", compress_method = "none", compress_rate = 1,
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

#' @title stretch circle to square
#'
#' @description Stretch data data from circular image to square in binary matrix
#' @param imagematrix The matrix to be stretched.
#' @param stretch_method Stretch algorithm. Four algorithms (radial, shirley, squircle, and elliptical) are available to stretch the image. The algorithms were adapted from Lambers 2016.
#' @return A matrix of 0, 1 and NA representing white, black and transparent pixels, respectively.
#' @references
#' Lambers 2016 Mappings between Sphere, Disc, and Square. Journal of Computer Graphics Techniques, 5(2): 1-21.
#' @author Carlos Biagolini-Jr.
#' @examples
#' img_location <- system.file("extdata/chesstable.png",package ="bwimage")
#' circular_matrix<- threshold_color(img_location,"png")
#' stretched_matrix<-stretch(circular_matrix,stretch_method="radial")
#' @export
stretch<-function(imagematrix,stretch_method="radial"){
  if (!(stretch_method == "radial" | stretch_method == "shirley" | stretch_method == "squircle" | stretch_method == "elliptical")) {
    stop("Provide a valid stretch method")}
  matrix_resposta<-matrix(NA,ncol=length(imagematrix[1,]),nrow=length(imagematrix[,1]))
  altura<-floor(length(imagematrix[,1])/2) # Altura da imagem dividido por 2
  largura<-floor(length(imagematrix[1,])/2) # Largura da imagem dividido por 2

  for(l in 1: length(imagematrix[,1])){ # linhas = y
    for(c in 1: length(imagematrix[1,])){ # colunas = x
      co1<-correcao_ida(l,c,altura,largura) # Coordenada do pixel a ser pintado em escala -1:1
      if (stretch_method == "radial") {
        # funcao radial
        co2<-radial(co1[1],co1[2]) # Coordenada do pixel que vai ter a cor copida na escala -1:1
      }else{
        if (stretch_method == "shirley"){
          # funcao shirley
          co2<-shirley(co1[1],co1[2]) # Coordenada do pixel que vai ter a cor copida na escala -1:1
        }else{
          if (stretch_method == "squircle") {
            # funcao squircle
            co2<-squircle(co1[1],co1[2],altura,largura) # Coordenada do pixel que vai ter a cor copida na escala -1:1
          }else{
            if (stretch_method == "elliptical") {
              # funcao elliptical
              co2<-elliptical(co1[1],co1[2],altura,largura)  # Coordenada do pixel que vai ter a cor copida na escala -1:1
              show(paste("squircle","l=",l,"c=",c))
            }else{stop("Provide a valid stretch method")}}}}
      co3<-correcao_volta(co2[1],co2[2],altura,largura) # Coordenada do pixel que vai ter a cor copida sem escala
      matrix_resposta[l,c]<- imagematrix[floor(co3[1]),floor(co3[2])]
      co1<-co2<-co3<-NULL}}
  return(matrix_resposta)}
