library(ggplot2)
library(corrplot)
install.packages('ggplot2')
install.packages("corrplot")
load('data/dd.Rdata')

summary(dd)
#04.1 Univariant
#Age
hist(dd$age)
plot(density(dd$age))

#workclass
barplot(table(dd$workclass))
pie(table(dd$workclass))

#fnlwgt
ggplot(data=dd, aes(dd$fnlwgt)) + geom_histogram()
ggplot(data=dd, aes(dd$fnlwgt)) + geom_density() 

#education
t = as.data.frame(table(dd$education))
ggplot(data=t, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity")

ggplot(t, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  theme_void()

#education_num
ggplot(data=dd, aes(dd$education_num)) + geom_histogram()
ggplot(data=dd, aes(dd$education_num)) + geom_density() 

# Podem veurela com a variable categorica
t = as.data.frame(table(dd$education_num))
ggplot(data=t, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity")

ggplot(t, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  theme_void()

#marital_status
t = as.data.frame(table(dd$marital_status))
ggplot(data=t, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity")

ggplot(t, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  theme_void()

#occupation
t = as.data.frame(table(dd$occupation))
ggplot(data=t, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity")

ggplot(t, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  theme_void()

#relationship
t = as.data.frame(table(dd$relationship))
ggplot(data=t, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity")

ggplot(t, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  theme_void()

#race
t = as.data.frame(table(dd$race))
ggplot(data=t, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity")

ggplot(t, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  theme_void()

#sex
t = as.data.frame(table(dd$sex))
ggplot(data=t, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity")

ggplot(t, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  theme_void()

#capital_gain
ggplot(data=dd, aes(dd$capital_gain)) + geom_histogram()
ggplot(data=dd, aes(dd$capital_gain)) + geom_density() 


#capital_loss
ggplot(data=dd, aes(dd$capital_loss)) + geom_histogram()
ggplot(data=dd, aes(dd$capital_loss)) + geom_density() 

#hours_per_week
ggplot(data=dd, aes(dd$hours_per_week)) + geom_histogram()
ggplot(data=dd, aes(dd$hours_per_week)) + geom_density() 

#native_country
t = as.data.frame(table(dd$native_country))
t = t[order(t$Freq, decreasing = T),]
t = t[1:10,]
# t = t[2:10,]
ggplot(data=t, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity")

ggplot(t, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  theme_void()


#income
t = as.data.frame(table(dd$income))
ggplot(data=t, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity")

ggplot(t, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  theme_void()


#04.2 Multivariant
#Age
ggplot(dd, aes(age, fill = income)) + geom_histogram(alpha = 0.5)
ggplot(dd, aes(age, fill = income)) + geom_density(alpha = 0.2)

#workclass
t = as.data.frame(table(dd$workclass, dd$income))
ggplot(data=t, aes(x=Var1, y=Freq, fill = Var2)) +
  geom_bar(stat="identity")
for(wclass in unique(t$Var1)){
  t[t$Var1 == wclass, "Freq"] = t[t$Var1 == wclass, "Freq"]/sum(t[t$Var1 == wclass, "Freq"])
}
ggplot(data=t, aes(x=Var1, y=Freq, fill = Var2)) +
  geom_bar(stat="identity")

#fnlwgt
ggplot(dd, aes(fnlwgt, fill = income)) + geom_histogram(alpha = 0.5)
ggplot(dd, aes(fnlwgt, fill = income)) + geom_density(alpha = 0.2)

#education
t = as.data.frame(table(dd$education, dd$income))
ggplot(data=t, aes(x=Var1, y=Freq, fill = Var2)) +
  geom_bar(stat="identity")
for(educ in unique(t$Var1)){
  t[t$Var1 == educ, "Freq"] = t[t$Var1 == educ, "Freq"]/sum(t[t$Var1 == educ, "Freq"])
}
ggplot(data=t, aes(x=Var1, y=Freq, fill = Var2)) +
  geom_bar(stat="identity")


#education_num
ggplot(dd, aes(education_num, fill = income)) + geom_histogram(alpha = 0.5)
ggplot(dd, aes(education_num, fill = income)) + geom_density(alpha = 0.2)

t = as.data.frame(table(dd$education_num, dd$income))
ggplot(data=t, aes(x=Var1, y=Freq, fill = Var2)) +
  geom_bar(stat="identity")
for(educ in unique(t$Var1)){
  t[t$Var1 == educ, "Freq"] = t[t$Var1 == educ, "Freq"]/sum(t[t$Var1 == educ, "Freq"])
}
ggplot(data=t, aes(x=Var1, y=Freq, fill = Var2)) +
  geom_bar(stat="identity")


#marital_status
t = as.data.frame(table(dd$marital_status, dd$income))
ggplot(data=t, aes(x=Var1, y=Freq, fill = Var2)) +
  geom_bar(stat="identity")
for(ms in unique(t$Var1)){
  t[t$Var1 == ms, "Freq"] = t[t$Var1 == ms, "Freq"]/sum(t[t$Var1 == ms, "Freq"])
}
ggplot(data=t, aes(x=Var1, y=Freq, fill = Var2)) +
  geom_bar(stat="identity")

#occupation
t = as.data.frame(table(dd$occupation, dd$income))
ggplot(data=t, aes(x=Var1, y=Freq, fill = Var2)) +
  geom_bar(stat="identity")
for(occ in unique(t$Var1)){
  t[t$Var1 == occ, "Freq"] = t[t$Var1 == occ, "Freq"]/sum(t[t$Var1 == occ, "Freq"])
}
ggplot(data=t, aes(x=Var1, y=Freq, fill = Var2)) +
  geom_bar(stat="identity")

#relationship
t = as.data.frame(table(dd$relationship, dd$income))
ggplot(data=t, aes(x=Var1, y=Freq, fill = Var2)) +
  geom_bar(stat="identity")
for(rel in unique(t$Var1)){
  t[t$Var1 == rel, "Freq"] = t[t$Var1 == rel, "Freq"]/sum(t[t$Var1 == rel, "Freq"])
}
ggplot(data=t, aes(x=Var1, y=Freq, fill = Var2)) +
  geom_bar(stat="identity")

#race
t = as.data.frame(table(dd$race, dd$income))
ggplot(data=t, aes(x=Var1, y=Freq, fill = Var2)) +
  geom_bar(stat="identity")
for(rac in unique(t$Var1)){
  t[t$Var1 == rac, "Freq"] = t[t$Var1 == rac, "Freq"]/sum(t[t$Var1 == rac, "Freq"])
}
ggplot(data=t, aes(x=Var1, y=Freq, fill = Var2)) +
  geom_bar(stat="identity")

#sex
t = as.data.frame(table(dd$sex, dd$income))
ggplot(data=t, aes(x=Var1, y=Freq, fill = Var2)) +
  geom_bar(stat="identity")
for(sex in unique(t$Var1)){
  t[t$Var1 == sex, "Freq"] = t[t$Var1 == sex, "Freq"]/sum(t[t$Var1 == sex, "Freq"])
}
ggplot(data=t, aes(x=Var1, y=Freq, fill = Var2)) +
  geom_bar(stat="identity")


#capital_gain
ggplot(dd, aes(capital_gain, fill = income)) + geom_histogram(alpha = 0.5)
ggplot(dd, aes(capital_gain, fill = income)) + geom_density(alpha = 0.2)


#capital_loss
ggplot(dd, aes(capital_loss, fill = income)) + geom_histogram(alpha = 0.5)
ggplot(dd, aes(capital_loss, fill = income)) + geom_density(alpha = 0.2)


#hours_per_week
ggplot(dd, aes(hours_per_week, fill = income)) + geom_histogram(alpha = 0.5)
ggplot(dd, aes(hours_per_week, fill = income)) + geom_density(alpha = 0.2)


#native_country
t = as.data.frame(table(dd$native_country))
t = t[order(t$Freq, decreasing = T),]
t = t[1:10,]
countrynames = t$Var1
# t = t[2:10,]

t = as.data.frame(table(dd$native_country, dd$income))
ggplot(data=t, aes(x=Var1, y=Freq, fill = Var2)) +
  geom_bar(stat="identity")
for(nc in unique(t$Var1)){
  t[t$Var1 == nc, "Freq"] = t[t$Var1 == nc, "Freq"]/sum(t[t$Var1 == nc, "Freq"])
}
ggplot(data=t, aes(x=Var1, y=Freq, fill = Var2)) +
  geom_bar(stat="identity")

t = t[t$Var1 %in% countrynames,]
ggplot(data=t, aes(x=Var1, y=Freq, fill = Var2)) +
  geom_bar(stat="identity")

# Correlacio entre variables numeriques
ddaux = dd
ddaux$income2 = ifelse(ddaux$income == ">50K", 1, 0)
numericas = unlist(lapply(ddaux, is.numeric))

c = cor(ddaux[,numericas])
corrplot(c)
