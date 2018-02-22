                                         ##Case Study Microcredit Unions in Bangladesh##

Microcredit <- read.csv("Microcredit.csv")

# loading the librariest
library(stargazer)

#testing relationship of variables
plot(Microcredit$educhead,Microcredit$ln.exptot)#testing single variable relationships
cor(Microcredit)# testing for multicollinearity

#Setting the basemodel
basemodel <- lm(ln.exptot~wheat+milk+egg+oil+famsize+agehead+educhead+ln.lands++sexhead+vaccess+pcirr, data=Microcredit)
summary(basemodel)
hist(rstandard(basemodel), xlab = "standardized residuals")
plot(fitted(basemodel), resid(basemodel), xlab="Fitted",ylab="Residuals")
abline(h=0, col="blue")

#Setting model with program.male as dummy
maleonly <- lm(ln.exptot~program.male+wheat+milk+egg+oil+famsize+agehead+educhead+ln.lands++sexhead+vaccess+pcirr, data=Microcredit)
summary(maleonly)
hist(rstandard(maleonly), xlab = "standardized residuals")
plot(fitted(maleonly), resid(maleonly), xlab = "Fitted", ylab = "Residuals")
abline(h=0, col = "blue")

#Setting model with program.female as dummy
femaleonly <- lm(ln.exptot~program.female+wheat+milk+egg+oil+famsize+agehead+educhead+ln.lands++sexhead+vaccess+pcirr, data=Microcredit)
summary(femaleonly)
hist(rstandard(femaleonly), xlab = "standardized residuals")
plot(fitted(femaleonly), resid(femaleonly), xlab = "Fitted", ylab = "Residuals")
abline(h=0, col = "blue")

#Setting model with dmmfd as dummy
maleparticipant <- lm(ln.exptot~dmmfd+wheat+milk+egg+oil+famsize+agehead+educhead+ln.lands++sexhead+vaccess+pcirr, data=Microcredit)
summary(maleparticipant)
hist(rstandard(maleparticipant), xlab = "standardized residuals")
plot(fitted(maleparticipant), resid(maleparticipant), xlab = "Fitted", ylab = "Residuals")
abline(h=0, col = "blue")

#Setting model with dfmfd as dummy
femaleparticipant <- lm(ln.exptot~dfmfd+wheat+milk+egg+oil+famsize+agehead+educhead+ln.lands+sexhead+vaccess+pcirr, data=Microcredit)
summary(femaleparticipant)
hist(rstandard(femaleparticipant), xlab = "standardized residuals")
plot(fitted(femaleparticipant), resid(femaleparticipant), xlab = "Fitted", ylab = "Residuals")
abline(h=0, col = "blue")

#Descriptive Statistics
stargazer( # in htm format and with var labels
  Microcredit[c("dmmfd","dfmfd","program.male","program.female","ln.exptot","wheat","milk","egg","oil","famsize","agehead","educhead","ln.lands","sexhead","vaccess","pcirr")],
  type = "html",
  title = "Descriptive statistics",
  digits = 2, #number of digits after the point
  covariate.labels = c("HH Male Paricipant", "HH Female Participant","Village Males Only","Village Females Only","Log of HH per capita total expenditure","Village price of wheat: Tk./kg","Village price of milk: Tk./liter","Village price of egg: Tk./4 counts","Village price of edible oil: Tk./kg","Number of HH members","Age of HH head: years","Education of HH head: years","Log of HH lands: acres","Gender of HH head: 1=Male, 0=Female","Village is accesible by road all year: 1=Yes, 0=No","Proportion of village land irrigated"),
  out= "descriptivestats.htm" # File "descriptivestats.htm" saved to your working directory
)

#Regression model table readout for all models ##Had trouble converting to word, so inputted data into excell tables and went from there
stargazer(basemodel,maleonly,femaleonly,maleparticipant,femaleparticipant,
  type="html",
  title="Microcredit Unions - Bangladesh",
  digits=2, #number of decimals rounded to
  dep.var.labels = c("Log of HH per capita total expenditure","Log of HH per capita total expenditure","Log of HH per capita total expenditure"),
  out= "regressionmodel.htm" # File "regressionmodel.htm" saved to your working directory
  )