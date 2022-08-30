#' Base Packages Data
#'
#' The main function to Retrieve from Atoka API data.
#'
#' @param id is the Company's Codice Fiscale, P.IVA, or Atoka ID
#' @param pack is the Atoka API V2 Packages to access among base, economics, atokaIndicators, cervedIndicators and foreignMarket
#' @param key the API TOKEN
#' @param lim the number of companies to retrieve in case of multiple matches
#'
#' @import data.table
#' @import httr
#' @import jsonlite
#'
#' @return a data.table with the data available for the company
#'
#' @export
#'


get_companies = function(id, pack = 'base', token = '5c64eb89575f4531853980280f4bfa03-e', lim = 1L) {

  # 1. Check internet connection
  if (!curl::has_internet()) {
    warning("No internet connection. Please check your network. Could not download data series.", call. = FALSE)
    return(invisible(NULL))
  }

  if(!is.character(id)) {

    id = as.character(id)
    warning("[ID] Not a Character String", call. = FALSE)

  }

  if(!is.character(pack)) {

    pack = as.character(pack)
    warning("[PACK] Not a Character String", call. = FALSE)

  }

  if(!is.character(key)) {

    key = as.character(key)
    warning("[KEY] Not a Character String", call. = FALSE)

  }

  if(!is.integer(lim)) {

    lim = as.integer(lim)
    warning("[LIM] Not an Integer or Natural Number", call. = FALSE)

  }


  ### Call Preparation

  base_url = 'https://api.atoka.io/v2/'
  endpoint = 'companies'

  url = paste0(base_url, endpoint)

    stringa = paste0(

      url, '?',
      'packages=', pack, '&',
      'regNumbers=', id, '&',
      'token=', token, '&',
      'limit=', lim

    )

  res = httr::GET(stringa)

  if (res$status_code != 200) {

    warning(paste('[COMPANY ID]:', id, 'Not found in Atoka DB'), call. = FALSE)

    data = data.table(

      KEY = id,
      SUCCESS = 'NO',
      atokaId = '<Empty>',
      legalName = '<Empty>',
      legalClass = '<Empty>',
      cervedId = '<Empty>',
      vatId = '<Empty>',
      taxId = '<Empty>',

      inGroup = '<Empty>',
      dateFounded = '<Empty>',

      atecoCode = '<Empty>',
      atecoRoot = '<Empty>',

      areaProvince = '<Empty>',
      areaGeo = '<Empty>'


    )

  } else {

  dat = as.data.table(fromJSON(rawToChar(res$content))$items)

  data = data.table(

    KEY = id,
    SUCCESS = 'YES',
    atokaId = dat$id,
    legalName = dat$base.legalName,
    legalClass = dat$base.legalClass,
    cervedId = dat$cervedId,
    vatId = dat$base.vat,
    taxId = dat$base.taxId,

    inGroup = dat$base.inGroup,
    dateFounded = dat$base.founded,

    atecoCode = dat$base.ateco[[1]]$code,
    atecoRoot = dat$base.ateco[[1]]$rootCode,

    areaProvince = dat$base.registeredAddress.provinceCode,
    areaGeo = dat$base.registeredAddress.macroregion


  )

  }

  return(data)

}


PROVA = get_companies(id = 2)
PROVA
