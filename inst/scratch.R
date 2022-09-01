
library(data.table)
library(httr)
library(jsonlite)

dt_companies = fread(file.path('inst', 'company_list.csv'))

token = '5c64eb89575f4531853980280f4bfa03-e'
pack = 'base'
lim = 1

id = '00470550013'



base_url = 'https://api.atoka.io/v2/'
endpoint = 'companies'

url = paste0(base_url, endpoint)


stringa = paste0(

  url, '?',
  'packages=', 'atokaI', '&',
  'regNumbers=', id, '&',
  'token=', key, '&',
  'limit=', lim

)

called = httr::GET(stringa)
called$status_code

called_wrong = httr::GET(stringa)
called_wrong$status_code


raw2char = rawToChar(called$content)
toJSON = fromJSON(raw2char)
dat = as.data.table(toJSON$items)

dat = as.data.table(fromJSON(rawToChar(httr::content(called, encoding = "UTF-8")))$items)



t_base_w = get_companies_base(id = '00470550013')
t_base_w

t_scores_w = get_companies_scores(id = '00470550013')
t_scores_w

t_scores_w = get_atoka_cvd(id = '01094190525')
