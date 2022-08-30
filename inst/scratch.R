
library(data.table)
library(httr)
library(jsonlite)

dt_companies = fread(file.path('inst', 'company_list.csv'))

key = '5c64eb89575f4531853980280f4bfa03-e'
pack = 'base'
lim = 1

id = dt_companies[1,1][[1]]



base_url = 'https://api.atoka.io/v2/'
endpoint = 'companies'

url = paste0(base_url, endpoint)


stringa = paste0(

  url, '?',
  'packages=', pack, '&',
  'regNumbers=', 2, '&',
  'token=', key, '&',
  'limit=', lim

)


called_wrong = httr::GET(stringa)
called_wrong$status_code


raw2char = rawToChar(called$content)
toJSON = fromJSON(raw2char)
dat = as.data.table(toJSON$items)



trial = get_companies(id = 2, pack = 'base', token = key)

