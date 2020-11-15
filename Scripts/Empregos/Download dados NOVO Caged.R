library(RCurl)

get_caged <- function(type = NULL, month = NULL, year = NULL, dir = getwd()) {
  types <- c("Estabelecimentos", "Movimenta%E7%F5es") # 1 or 2
  months <- c(
    "Janeiro",
    "Fevereiro",
    "Mar�o",
    "Abril",
    "Maio",
    "Junho",
    "Julho",
    "Agosto",
    "Setembro",
    "Outubro",
    "Novembro",
    "Dezembro"
  )
  ftp_url <- "ftp://ftp.mtps.gov.br/pdet/microdados/NOVO CAGED/"
  response <- function(url) {
    req <- RCurl::getURL(
      utils::URLencode(paste0(ftp_url, url, "/")),
      .encoding = "ISO-8859-1",
      dirlistonly = TRUE
    )
    resul <- unlist(strsplit(req, "\r\n"))
    return(resul)
  }
  get_src <- response(types[type])
  # Estabelecimentos
  if (type == 1) {
    i_months <- c()
    for (i in get_src) {
      i_months <- c(i_months, which(i == months))
    }
    current_dir <- max(i_months)
    sub_current_dir <- paste0(types[type], "/", months[current_dir])
    ftp_query <- response(sub_current_dir)
    file <- ftp_query[
      substr(ftp_query, 1, 16) == paste0("CAGEDESTAB", "2020", "0", month)
    ]
    if (length(file) == 0) {
      stop("Arquivo não encontrado")
    }
    folder_name <- "caged__data"
    print(paste0("O Arquivo será baixado, a pasta ", folder_name, " foi criada"))
    dir.create(folder_name)
    utils::download.file(
      url = utils::URLencode(paste0(ftp_url, sub_current_dir, "/", file)),
      destfile = paste0(dir, "/", folder_name, "/", file),
      mode = "wb"
    )
    print("Arquivo baixado, faça a extração")
    infos <- list(
      dire = paste0(dir, "/", folder_name, "/", file),
      name = file
    )
    return(infos)
  }
  # if (is.null(type)) {
  #   stop("Add type arg")
  # }
  # if (is.null(month)) {
  #   stop("Add month arg")
  # }
  # if (type == 2 || type == types(2)) {
  #   if (is.null(year)) {
  #     stop("Add year arg")
  #   }
  #   if (year < 2020) {
  #     stop("year >= 2020")
  #   }
  #   if (year > timeDate::getRmetricsOptions("currentYear")) {
  #     stop(paste0("year <=", timeDate::getRmetricsOptions("currentYear")))
  #   }
  # }
}

a <- get_caged(type = 1, month = 5)

