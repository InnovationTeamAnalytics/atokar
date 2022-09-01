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


get_companies_base = function(id, pack = 'base', token = '5c64eb89575f4531853980280f4bfa03-e', lim = 1L) {

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


  ### Response Decode

  res = httr::GET(stringa)
  dat = as.data.table(fromJSON(rawToChar(res$content))$items)


  ### Output Preparation

  if (dim(dat)[[1]] == 0) {

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

    if(!is.null(dat$id)) {d_id = dat$id} else {d_id = '<Empty>'}
    if(!is.null(dat$base.legalName)) {d_legalName = dat$base.legalName} else {d_legalName = '<Empty>'}
    if(!is.null(dat$base.legalClass)) {d_legalClass = dat$base.legalClass} else {d_legalClass = '<Empty>'}
    if(!is.null(dat$cervedId)) {d_cervedId = dat$cervedId} else {d_cervedId = '<Empty>'}
    if(!is.null(dat$base.vat)) {d_vatId = dat$base.vat} else {d_vatId = '<Empty>'}
    if(!is.null(dat$base.taxId)) {d_taxId = dat$base.taxId} else {d_taxId = '<Empty>'}

    if(!is.null(dat$base.inGroup)) {d_inGroup = dat$base.inGroup} else {d_inGroup = '<Empty>'}
    if(!is.null(dat$base.founded)) {d_dateFounded = dat$base.founded} else {d_dateFounded = '<Empty>'}

    if(!is.null(dat$base.ateco[[1]]$code)) {d_atecoCode = dat$base.ateco[[1]]$code} else {d_atecoCode = '<Empty>'}
    if(!is.null(dat$base.ateco[[1]]$rootCode)) {d_atecoRoot = dat$base.ateco[[1]]$rootCode} else {d_atecoRoot = '<Empty>'}

    if(!is.null(dat$base.registeredAddress.provinceCode)) {d_areaProvince = dat$base.registeredAddress.provinceCode} else {d_areaProvince = '<Empty>'}
    if(!is.null(dat$base.registeredAddress.macroregion)) {d_areaGeo = dat$base.registeredAddress.macroregion} else {d_areaGeo = '<Empty>'}

  data = data.table(

    KEY = id,
    SUCCESS = 'YES',
    atokaId = d_id,
    legalName = d_legalName,
    legalClass = d_legalClass,
    cervedId = d_cervedId,
    vatId = d_vatId,
    taxId = d_taxId,

    inGroup = d_inGroup,
    dateFounded = d_dateFounded,

    atecoCode = d_atecoCode,
    atecoRoot = d_atecoRoot,

    areaProvince = d_areaProvince,
    areaGeo = d_areaGeo

  )

  }

  return(data)

}






#' Scores Packages Data
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


get_companies_scores = function(id, pack = 'atokaIndicators', token = '5c64eb89575f4531853980280f4bfa03-e', lim = 1L) {

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


  ### Response Decode

  res = httr::GET(stringa)
  dat = as.data.table(fromJSON(rawToChar(res$content))$items)


  ### Output Preparation

  if (dim(dat)[[1]] == 0) {

    warning(paste('[COMPANY ID]:', id, 'Not found in Atoka DB'), call. = FALSE)

    data = data.table(

      KEY = id,
      SUCCESS = 'NO',
      atokaId = '<Empty>',
      legalName = '<Empty>',
      cervedId = '<Empty>',

      scr_innovation_value = '<Empty>',
      scr_innovation_class = '<Empty>',

      scr_centweb_value = '<Empty>',
      scr_centweb_class = '<Empty>',


    )

  } else {

  dat = as.data.table(fromJSON(rawToChar(res$content))$items)

    if(!is.null(dat$id)) {d_id = dat$id} else {d_id = '<Empty>'}
    if(!is.null(dat$base.legalName)) {d_legalName = dat$base.legalName} else {d_legalName = '<Empty>'}
    if(!is.null(dat$cervedId)) {d_cervedId = dat$cervedId} else {d_cervedId = '<Empty>'}

    if(!is.null(dat$atokaIndicators.innovation.score)) {d_scr_innovation_value = dat$atokaIndicators.innovation.score} else {d_scr_innovation_value = '<Empty>'}
    if(!is.null(dat$atokaIndicators.innovation.scoreLabel)) {d_scr_innovation_class = dat$atokaIndicators.innovation.scoreLabel} else {d_scr_innovation_class = '<Empty>'}

    if(!is.null(dat$atokaIndicators.webCentrality.score)) {d_scr_centweb_value = dat$atokaIndicators.webCentrality.score} else {d_scr_centweb_value = '<Empty>'}
    if(!is.null(dat$atokaIndicators.webCentrality.scoreLabel)) {d_scr_centweb_class = dat$atokaIndicators.webCentrality.scoreLabel} else {d_scr_centweb_class = '<Empty>'}

  data = data.table(

      KEY = id,
      SUCCESS = 'NO',
      atokaId = d_atokaId,
      legalName = d_legalName,
      cervedId = d_cervedId,

      scr_innovation_value = d_scr_innovation_value,
      scr_innovation_class = d_scr_innovation_class,

      scr_centweb_value = d_scr_centweb_value,
      scr_centweb_class = d_scr_centweb_class,

  )

  }

  return(data)

}



