# Convert image to matrix

#' @title Image to matrix - Single
#'
#' @description Convert a single image into a matrix
#' @param filename Name of the file to be load - ex: "Figure01.JPG".
#' @param filetype Type of the file to be load. Compatible file types: ".JPGE", ".JPG" or ".PNG".
#' @param compress_method For high resolution files, i.e. numbers of pixels in width and height, it is suggested to reduce the resolution to create a smaller matrix,
#' it strongly reduce GPU usage and time necessary to run analyses. On the other hand, by reducing resolution, it will also reduce the accuracy of data description.
#' Methods for image compression: (i) frame_fixed: compress images to a desired target width and height; (ii) proportional:  compress the image by a given ratio provide in the argument "proportion";
#' (iii) width_fixed: compress images to get a target width. The image height will also be reduced, in the same intensity. For instance, if the original file had 1000 pixel in width, and
#' width_fixed was set to 100, the height reduction will reduce in a rate of 0.1 (100/1000) ; (iv) height_fixed: analogous to width_fixed, but assumes height as reference.
#' @param compress_rate Compress rate to by apply if compress_method=proportional. Note:  it should be ser as number range from 0 to 1 .
#' @param target_width Target width to be used if compress_method=frame_fixed or compress_method= width_fixed.
#' @param target_height Target height to be used if compress_method=frame_fixed or compress_method= height_fixed.
#' @param threshold_value For each pixel, the intensity of color channels (red, green and blue) are averaged and compared to a threshold_value (threshold).
#' If the average intensity is less than the threshold_value (default is 0.5) the pixel will be set as black, otherwise it will be white. See channel argument.
#' @param transparency_regulation For PNG images, the alpha channel is used to set transparent pixels, i.e. alpha channel values above transparency_regulation (a threshold)
#'  will set the pixel as transparent, default is 0.5.  NOTE: In the data matrix the value 1 represents black pixels, 0 represents white pixels and NA represents transparent pixels.
#' @param channel RGB channel to be considered in threshold. If channel=RGB (default), the intensity of red, green and blue is averaged and compared to threshold_value.
#' If the average intensity is less than the threshold_value (default is 50%) the pixel will be set as black (matrix cell value =1), otherwise it will be white (matrix cell value =0).
#' If only one channel is defined ("R" for red, "G" for green, and "B" for blue), the average intensity selected channel compared direct to the threshold_value value.
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
           target_width = 100, target_height = 100, threshold_value = 0.5, transparency_regulation = 0.5,channel="rgb") {
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
    if (threshold_value > 1 | threshold_value < 0) {
      stop("threshold_value must be a number between 0 and 1")
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

    #Definir range das cores que serao utilizadas e maximo da soma
    if(channel=="rgb"|channel=="RGB"){range<-1:3;max.soma<-3
    }else{
      if(channel=="r"|channel=="R"){range<-1;max.soma<-1
      }else{
        if(channel=="g"|channel=="G"){range<-2;max.soma<-1
        }else{
          if(channel=="b"|channel=="B"){range<-3;max.soma<-1
          }else{ stop("Provide a valid channel")}}}}
    # Cria uma matriz de responta e pinta as celulas
    matrix_cores <- matrix(NA, nrow = linhas_imagem, ncol = colunas_imagem)
    if (filetype == "jpeg" | filetype == "jpg") {
      for (c in 1:ncol(matrix_cores)) {
        for (l in 1:nrow(matrix_cores)) {
          if ((sum(imagematrix[floor(l * espaco_entre_pixeis_linha), floor(c *espaco_entre_pixeis_coluna),range])/max.soma) < threshold_value ) {
            matrix_cores[l, c] <- 1
          } else {
            matrix_cores[l, c] <- 0}}}}
    if (filetype == "png") {
      for (c in 1:ncol(matrix_cores)) {
        for (l in 1:nrow(matrix_cores)) {
          if (imagematrix[floor(l * espaco_entre_pixeis_linha), floor(c * espaco_entre_pixeis_coluna),4] < transparency_regulation) {matrix_cores[l, c] <- NA
          } else {
            # deixando as caixas
            if ((sum(imagematrix[floor(l * espaco_entre_pixeis_linha), floor(c *espaco_entre_pixeis_coluna),range])/max.soma)<threshold_value) {matrix_cores[l, c] <- 1
            } else {
              matrix_cores[l, c] <- 0}}}}}
    return(matrix_cores)}
#' @title Image to matrix - List
#'
#' @description Convert two or more images into a list of matrices
#' @param list_names An object contains the names of the files.
#' @param filetype Type of the file to be load. Compatible file types: ".JPGE", ".JPG" or ".PNG".
#' @param compress_method For high resolution files, i.e. numbers of pixels in width and height, it is suggested to reduce the resolution to create a smaller matrix,
#' it strongly reduce GPU usage and time necessary to run analyses. On the other hand, by reducing resolution, it will also reduce the accuracy of data description.
#' Methods for image compression: (i) frame_fixed: compress images to a desired target width and height; (ii) proportional:  compress the image by a given ratio provide in the argument "proportion";
#' (iii) width_fixed: compress images to get a target width. The image height will also be reduced, in the same intensity. For instance, if the original file had 1000 pixel in width, and
#' width_fixed was set to 100, the height reduction will reduce in a rate of 0.1 (100/1000) ; (iv) height_fixed: analogous to width_fixed, but assumes height as reference.
#' @param compress_rate Compress rate to by apply if compress_method=proportional. Note:  it should be ser as number range from 0 to 1 .
#' @param target_width Target width to be used if compress_method=frame_fixed or compress_method= width_fixed.
#' @param target_height Target height to be used if compress_method=frame_fixed or compress_method= height_fixed.
#' @param threshold_value For each pixel, the intensity of color channels (red, green and blue) are averaged and compared to a threshold_value (threshold).
#' If the average intensity is less than the threshold_value (default is 0.5) the pixel will be set as black, otherwise it will be white. See channel argument.
#' @param transparency_regulation For PNG images, the alpha channel is used to set transparent pixels, i.e. alpha channel values above transparency_regulation (a threshold)
#'  will set the pixel as transparent, default is 0.5.  NOTE: In the data matrix the value 1 represents black pixels, 0 represents white pixels and NA represents transparent pixels.
#' @param channel RGB channel to be considered in threshold. If channel=RGB (default), the intensity of red, green and blue is averaged and compared to threshold_value.
#' If the average intensity is less than the threshold_value (default is 50%) the pixel will be set as black (matrix cell value =1), otherwise it will be white (matrix cell value =0).
#' If only one channel is defined ("R" for red, "G" for green, and "B" for blue), the average intensity selected channel compared direct to the threshold_value value.
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
           target_width = 100, target_height = 100, threshold_value = 0.5, transparency_regulation = 0.5,channel="rgb") {
    pb <- txtProgressBar(min = 0, max = length(list_names), style = 3)
    lista_de_saida <- list(NA)
    for (i in 1:length(list_names)) {
      matrix_para_analise <- threshold_color(filename = list_names[i], filetype = filetype, compress_method = compress_method,
                                             compress_rate = compress_rate, threshold_value = threshold_value, target_width = target_width,
                                             target_height = target_height, transparency_regulation = transparency_regulation,channel=channel)
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
#' @param method Stretch algorithm. Four algorithms (radial, shirley, squircle, and elliptical) are available to stretch the image. The algorithms were adapted from Lambers 2016.
#' @return A matrix of 0, 1 and NA representing white, black and transparent pixels, respectively.
#' @references
#' Lambers 2016 Mappings between Sphere, Disc, and Square. Journal of Computer Graphics Techniques, 5(2): 1-21.
#' @author Carlos Biagolini-Jr.
#' @examples
#' img_location <- system.file("extdata/chesstable.png",package ="bwimage")
#' image_matrix<- threshold_color(img_location,"png", "frame_fixed",target_width = 50,target_height=50)
#' stretch(image_matrix,method="radial")
#' @export
stretch<-function(imagematrix,method="radial"){
  if (!(method == "radial" | method == "shirley" | method == "squircle" | method == "elliptical")) {
    stop("Provide a valid stretch method")}
  matrix_resposta<-matrix(1,ncol=length(imagematrix[1,]),nrow=length(imagematrix[,1]))
  altura<-floor(length(imagematrix[,1])/2) # Altura da imagem dividido por 2
  largura<-floor(length(imagematrix[1,])/2) # Largura da imagem dividido por 2
  pb <- txtProgressBar(min = 0, max = length(imagematrix[,1]), style = 3)
  for(l in 1: length(imagematrix[,1])){ # linhas = y
    for(c in 1: length(imagematrix[1,])){ # colunas = x
      co1<-correcao_ida(l,c,altura,largura) # Coordenada do pixel a ser pintado em escala -1:1
      if (method == "radial") {
        # funcao radial
        co2<-radial(co1[1],co1[2],altura,largura) # Coordenada do pixel que vai ter a cor copida na escala -1:1
      }else{
        if (method == "shirley"){
          # funcao shirley
          co2<-shirley(co1[1],co1[2],altura,largura) # Coordenada do pixel que vai ter a cor copida na escala -1:1
        }else{
          if (method == "squircle") {
            # funcao squircle
            co2<-squircle(co1[1],co1[2],altura,largura) # Coordenada do pixel que vai ter a cor copida na escala -1:1
          }else{
            if (method == "elliptical") {
              # funcao elliptical
              co2<-elliptical(co1[1],co1[2],altura,largura)  # Coordenada do pixel que vai ter a cor copida na escala -1:1
            }else{stop("Provide a valid stretch method")}}}}
      co3<-correcao_volta(co2[1],co2[2],altura,largura) # Coordenada do pixel que vai ter a cor copida sem escala
      matrix_resposta[l,c]<- imagematrix[floor(co3[1]),floor(co3[2])]
      co1<-co2<-co3<-NULL}
    setTxtProgressBar(pb, l)}
  return(matrix_resposta)}


#' @title Compress square to circle
#'
#' @description Compress data data from square image to circular in binary matrix
#' @param imagematrix The matrix to be compressed.
#' @param method Compress algorithm. Four algorithms (radial, shirley, squircle, and elliptical) are available to stretch the image. The algorithms were adapted from Lambers 2016.
#' @param background Code for background cell value. When compressing a squared matrix, corners of the transformed matrix will no have corresponding pixel from original matrix. Thus, the background value will be the value of transformed matrix corners.
#' @return A matrix of 0, 1 and NA representing white, black and transparent pixels, respectively.
#' @references
#' Lambers 2016 Mappings between Sphere, Disc, and Square. Journal of Computer Graphics Techniques, 5(2): 1-21.
#' @author Carlos Biagolini-Jr.
#' @examples
#' img_location <- system.file("extdata/chesstable.png",package ="bwimage")
#' image_matrix<- threshold_color(img_location,"png", "frame_fixed",target_width = 50,target_height=50)
#' compress(image_matrix,method="radial")
#' @export
compress<-function(imagematrix,method="radial",background=NA){
  if (!(method == "radial" | method == "shirley" | method == "squircle" | method == "elliptical")) {
    stop("Provide a valid compress method")}
  matrix_resposta<-matrix(background,ncol=length(imagematrix[1,]),nrow=length(imagematrix[,1]))
  altura<-floor(length(imagematrix[,1])/2) # Altura da imagem dividido por 2
  largura<-floor(length(imagematrix[1,])/2) # Largura da imagem dividido por 2
  pb <- txtProgressBar(min = 0, max = length(imagematrix[,1]), style = 3)
  for(l in 1: length(imagematrix[,1])){ # linhas = y
    for(c in 1: length(imagematrix[1,])){ # colunas = x
      co1<-correcao_ida(l,c,altura,largura) # Coordenada do pixel a ser pintado em escala -1:1
      if (method == "radial") {
        # funcao radial
        co2<-cradial(co1[1],co1[2],altura,largura) # Coordenada do pixel que vai ter a cor copida na escala -1:1
      }else{
        if (method == "shirley"){
          # funcao shirley
          co2<-cshirley(co1[1],co1[2],altura,largura) # Coordenada do pixel que vai ter a cor copida na escala -1:1
        }else{
          if (method == "squircle") {
            # funcao squircle
            co2<-csquircle(co1[1],co1[2],altura,largura) # Coordenada do pixel que vai ter a cor copida na escala -1:1
          }else{
            if (method == "elliptical") {
              # funcao elliptical
              co2<-celliptical(co1[1],co1[2],altura,largura)  # Coordenada do pixel que vai ter a cor copida na escala -1:1
            }else{
              stop("Provide a valid compress method")
              }}}}
      if(!is.na(co2[1])){
        # primeiro testa de e NA
        if(altura>abs(co2[1])&largura>abs(co2[2])) { # Testar se a coordenda e valida
          co3<-correcao_volta(trunc(co2[1]),trunc(co2[2]) ,altura,largura) # Coordenada do pixel que vai ter a cor copida sem escala
          matrix_resposta[l,c]<- imagematrix[co3[1],co3[2] ]}}
      co1<-co2<-co3<-NULL}
    setTxtProgressBar(pb, l)}
  return(matrix_resposta)}
