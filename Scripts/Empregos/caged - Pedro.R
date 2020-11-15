library(RCurl)

get_caged <- function(type = NULL, month = NULL, year = NULL, dir = getwd()) {
  if (is.null(type)) {
    stop("Selecione um tipo, 1 ou 2")
  }
  if (is.null(month)) {
    stop("Selecione um mês, 1 a 12")
  }
  if (type == 2) {
    if (is.null(year)) {
      stop("Adicione um ano")
    }
    if (year < 2020) {
      stop("Ano deve ser maior ou igual a 2020")
    }
    if (year > timeDate::getRmetricsOptions("currentYear")) {
      stop(paste0("O ano deve ser menor que", timeDate::getRmetricsOptions("currentYear")))
    }
  }
  types <- c("Estabelecimentos", "Movimenta%E7%F5es") # 1 or 2
  months <- c(
    "Janeiro",
    "Fevereiro",
    "Março",
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
    print(file)
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
     dire = paste0(dir, "/", folder_name, "/", substr(file, 1, 16)),
     name = file
    )
    return(infos)
  }
}

read_caged <- function(type, dire) {
  if (type == 1) {
    caged_data <- read.fwf(
      paste0(dire, ".txt"),
      encoding = "ISO-8859-1",
      delim = ";",
      widths = c(6, 2, 2, 6, 1, 7, 5, 5, 1, 5, 1, 1),
      na.strings = c(" "),
      col.names = c(
        "competencia",
        "regiao",
        "uf",
        "municipio",
        "secao",
        "subclasse",
        "admitidos",
        "desligados",
        "font_desl",
        "saldomovimentacao",
        "tipoempregador",
        "tipoestabelecimento"
      ),
      dec = "."
    )
    return(caged_data)
  }
}

caged_maio <- get_caged(type = 1, month = 5)
caged_data_maio <- read_caged(type = 1, dire = caged_maio$dire)
