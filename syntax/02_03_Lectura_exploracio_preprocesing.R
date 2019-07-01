#02.1 Lectura de les dades
dd = read.table("data/adult.data", sep = ',')
names(dd) = c('age', 'workclass', 'fnlwgt', 'education', 'education_num', 'marital_status', 'occupation', 'relationship', 'race', 'sex', 'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income')

#02.2 Primera exploració de les dades
head(dd)
str(dd)
summary(dd)

#03.1 Factors a characters i eliminació d'espais en blanc
dd[] = lapply(dd, function(x) {
  if(is.factor(x)) {
    trimws(as.character(x))
  } else if(is.character(x)){
    trimws(x)
  }
  else {
    x
  }
    })

#03.2 Tractament de NA
dd[dd=='?'] <- NA
sum(is.na(dd))
summary(dd)
unlist(lapply(dd, function(x) sum(is.na(x))))
table(dd$workclass)
dd$workclass[is.na(dd$workclass)] = 'Private'
table(dd$occupation)
dd$occupation[is.na(dd$occupation)] = 'Prof-specialty'
sort(table(dd$native_country))
dd$native_country[is.na(dd$native_country)] = 'United-States'

unlist(lapply(dd, function(x) sum(is.na(x))))

save(dd, file = 'data/dd.Rdata')


