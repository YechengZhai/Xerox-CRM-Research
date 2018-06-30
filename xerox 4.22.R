setwd('/Users/Apple/Desktop')
xerox = read.csv('xerox.csv')

Higher_Education <- nrow(xerox[round(xerox$Recommend) <= 6 & xerox$ACCOUNT_SUB_VERTICAL == 'Higher Education', ])/
  nrow(xerox[xerox$ACCOUNT_SUB_VERTICAL == 'Higher Education', ])
Higher_Education # 0.5

# detractor
xeroxd = xerox[round(xerox$Repurchase) <= 6, ]

he = xerox[xerox$ACCOUNT_SUB_VERTICAL == 'Higher Education', ]

xeroxrc = xerox[round(xerox$Recommend) <= 6, ]

install.packages('corrplot')
library(corrplot)
colnames(he)

# 1 sales drilldown --> "Drilldown..Average.of.SALES....Helpful.Responsive"
henew1 = he[,c('X12.Month.Rolling.Revenue', 'X..Establishments', 'X..Employees', 
              "Average.Monthly.Page.Volume", 
              "X..of.Office.Mono.machines", "X..of.Office.Color.machines", 
              "X..of.Office.Printers", "X..of.Production.Mono.machines", 
              "X..of.Production.EPC.machines", "X..of.Production.iGen.machines", 
              "X2016...Call", "X2016...Customer.Care.Call", "X2016...Email", 
              "X2016...Executive.Call", "X2016...Meeting", "X2016...Other", 
              "X2016...Presentation", "X2016...Proposal", 
              "X2016...Quarterly.Business.Review", "X2016...Sales.Call", 
              "X2016...Telemarketing.Call", "X2016.Total",
              "X2017...Call", "X2017...Customer.Care.Call", "X2017...Email", 
              "X2017...Executive.Call", "X2017...Meeting", "X2017...Other", 
              "X2017...Presentation", "X2017...Proposal", "X2017...Quarterly.Business.Review", 
              "X2017...Sales.Call", "X2017...Telemarketing.Call", "X2017.Total", 
              "Total.Contact.Events..2016.2017.", "Drilldown..Average.of.SALES....Helpful.Responsive")]

he_cor1 = cor(he[,c('X12.Month.Rolling.Revenue', 'X..Establishments', 'X..Employees', 
                   "Average.Monthly.Page.Volume", 
                   "X..of.Office.Mono.machines", "X..of.Office.Color.machines", 
                   "X..of.Office.Printers", "X..of.Production.Mono.machines", 
                   "X..of.Production.EPC.machines", "X..of.Production.iGen.machines", 
                   "X2016...Call", "X2016...Customer.Care.Call", "X2016...Email", 
                   "X2016...Executive.Call", "X2016...Meeting", "X2016...Other", 
                   "X2016...Presentation", "X2016...Proposal", 
                   "X2016...Quarterly.Business.Review", "X2016...Sales.Call", 
                   "X2016...Telemarketing.Call", "X2016.Total",
                   "X2017...Call", "X2017...Customer.Care.Call", "X2017...Email", 
                   "X2017...Executive.Call", "X2017...Meeting", "X2017...Other", 
                   "X2017...Presentation", "X2017...Proposal", "X2017...Quarterly.Business.Review", 
                   "X2017...Sales.Call", "X2017.Total", 
                   "Total.Contact.Events..2016.2017.", "Drilldown..Average.of.SALES....Helpful.Responsive")])
corrplot.mixed(cor(he[,c('X12.Month.Rolling.Revenue', 'X..Establishments', 'X..Employees', 
                         "Average.Monthly.Page.Volume", 
                         "X..of.Office.Mono.machines", "X..of.Office.Color.machines", 
                         "X..of.Office.Printers", "X..of.Production.Mono.machines", 
                         "X..of.Production.EPC.machines", "X..of.Production.iGen.machines",
                         "X2016...Call", "X2016...Customer.Care.Call", "X2016...Email", 
                         "X2016...Executive.Call", "X2016...Meeting", "X2016...Other", 
                         "X2016...Presentation", "X2016...Proposal", 
                         "X2016...Quarterly.Business.Review", "X2016...Sales.Call", 
                         "X2016...Telemarketing.Call", "X2016.Total",
                         "X2017...Call", "X2017...Customer.Care.Call", "X2017...Email", 
                         "X2017...Executive.Call", "X2017...Meeting", "X2017...Other", 
                         "X2017...Presentation", "X2017...Proposal", "X2017...Quarterly.Business.Review", 
                         "X2017...Sales.Call","X2017.Total", 
                         "Total.Contact.Events..2016.2017.")]), cl.pos = "n", tl.pos = "n")

model1=lm(Drilldown..Average.of.SALES....Helpful.Responsive~., data = henew1)
gmodel1 = step(model1)
summary(gmodel1)

# 2 sales drilldown --> "Drilldown..Average.of.SALES....Communications"
henew2 = he[,c('X12.Month.Rolling.Revenue', 'X..Establishments', 'X..Employees', 
              "Average.Monthly.Page.Volume", 
              "X..of.Office.Mono.machines", "X..of.Office.Color.machines", 
              "X..of.Office.Printers", "X..of.Production.Mono.machines", 
              "X..of.Production.EPC.machines", "X..of.Production.iGen.machines", 
              "X2016...Call", "X2016...Customer.Care.Call", "X2016...Email", 
              "X2016...Executive.Call", "X2016...Meeting", "X2016...Other", 
              "X2016...Presentation", "X2016...Proposal", 
              "X2016...Quarterly.Business.Review", "X2016...Sales.Call", 
              "X2016...Telemarketing.Call", "X2016.Total",
              "X2017...Call", "X2017...Customer.Care.Call", "X2017...Email", 
              "X2017...Executive.Call", "X2017...Meeting", "X2017...Other", 
              "X2017...Presentation", "X2017...Proposal", "X2017...Quarterly.Business.Review", 
              "X2017...Sales.Call", "X2017...Telemarketing.Call", "X2017.Total", 
              "Total.Contact.Events..2016.2017.", "Drilldown..Average.of.SALES....Communications")]

he_cor2 = cor(he[,c('X12.Month.Rolling.Revenue', 'X..Establishments', 'X..Employees', 
                   "Average.Monthly.Page.Volume", 
                   "X..of.Office.Mono.machines", "X..of.Office.Color.machines", 
                   "X..of.Office.Printers", "X..of.Production.Mono.machines", 
                   "X..of.Production.EPC.machines", "X..of.Production.iGen.machines", 
                   "X2016...Call", "X2016...Customer.Care.Call", "X2016...Email", 
                   "X2016...Executive.Call", "X2016...Meeting", "X2016...Other", 
                   "X2016...Presentation", "X2016...Proposal", 
                   "X2016...Quarterly.Business.Review", "X2016...Sales.Call", 
                   "X2016...Telemarketing.Call", "X2016.Total",
                   "X2017...Call", "X2017...Customer.Care.Call", "X2017...Email", 
                   "X2017...Executive.Call", "X2017...Meeting", "X2017...Other", 
                   "X2017...Presentation", "X2017...Proposal", "X2017...Quarterly.Business.Review", 
                   "X2017...Sales.Call", "X2017.Total", 
                   "Total.Contact.Events..2016.2017.", "Drilldown..Average.of.SALES....Communications")])
corrplot.mixed(cor(he[,c('X12.Month.Rolling.Revenue', 'X..Establishments', 'X..Employees', 
                         "Average.Monthly.Page.Volume", 
                         "X..of.Office.Mono.machines", "X..of.Office.Color.machines", 
                         "X..of.Office.Printers", "X..of.Production.Mono.machines", 
                         "X..of.Production.EPC.machines", "X..of.Production.iGen.machines",
                         "X2016...Call", "X2016...Customer.Care.Call", "X2016...Email", 
                         "X2016...Executive.Call", "X2016...Meeting", "X2016...Other", 
                         "X2016...Presentation", "X2016...Proposal", 
                         "X2016...Quarterly.Business.Review", "X2016...Sales.Call", 
                         "X2016...Telemarketing.Call", "X2016.Total",
                         "X2017...Call", "X2017...Customer.Care.Call", "X2017...Email", 
                         "X2017...Executive.Call", "X2017...Meeting", "X2017...Other", 
                         "X2017...Presentation", "X2017...Proposal", "X2017...Quarterly.Business.Review", 
                         "X2017...Sales.Call","X2017.Total", 
                         "Total.Contact.Events..2016.2017.")]), cl.pos = "n", tl.pos = "n")

model2=lm(Drilldown..Average.of.SALES....Communications~., data = henew2)
gmodel2 = step(model2)
summary(gmodel2)

# 3 Tech Service drilldown --> "Drilldown..Average.of.Tech.Service...Remote.Support"
henew3 = he[,c('X12.Month.Rolling.Revenue', 'X..Establishments', 'X..Employees', 
              "Average.Monthly.Page.Volume", "Total...of.Machines..Serial.Numbers.", 
              "X..of.Office.Mono.machines", "X..of.Office.Color.machines", 
              "X..of.Office.Printers", "X..of.Production.Mono.machines", 
              "X..of.Production.EPC.machines", "X..of.Production.iGen.machines", 
              "X..of.Wide.Format.machines", "Drilldown..Average.of.Tech.Service...Remote.Support")]
he_cor3 = cor(he[,c('X12.Month.Rolling.Revenue', 'X..Establishments', 'X..Employees', 
                    "Average.Monthly.Page.Volume", "Total...of.Machines..Serial.Numbers.", 
                    "X..of.Office.Mono.machines", "X..of.Office.Color.machines", 
                    "X..of.Office.Printers", "X..of.Production.Mono.machines", 
                    "X..of.Production.EPC.machines", "X..of.Production.iGen.machines", 
                    "X..of.Wide.Format.machines", "A3.Serial..s", 
                    "Drilldown..Average.of.Tech.Service...Remote.Support")])
corrplot.mixed(cor(he[,c('X12.Month.Rolling.Revenue', 'X..Establishments', 'X..Employees', 
                         "Average.Monthly.Page.Volume", "Total...of.Machines..Serial.Numbers.", 
                         "X..of.Office.Mono.machines", "X..of.Office.Color.machines", 
                         "X..of.Office.Printers", "X..of.Production.Mono.machines", 
                         "X..of.Production.EPC.machines", "X..of.Production.iGen.machines", 
                         "X..of.Wide.Format.machines")]), cl.pos = "n", tl.pos = "n")

model3=lm(Drilldown..Average.of.Tech.Service...Remote.Support~., data = henew3)
gmodel3 = step(model3)
summary(gmodel3)

# 4 Tech Service drilldown --> Drilldown..Average.of.Tech.Service...Communication
henew4 = he[,c('X12.Month.Rolling.Revenue', 'X..Establishments', 'X..Employees', 
               "Average.Monthly.Page.Volume", "Total...of.Machines..Serial.Numbers.", 
               "X..of.Office.Mono.machines", "X..of.Office.Color.machines", 
               "X..of.Office.Printers", "X..of.Production.Mono.machines", 
               "X..of.Production.EPC.machines", "X..of.Production.iGen.machines", 
               "X..of.Wide.Format.machines", "Drilldown..Average.of.Tech.Service...Communication")]
he_cor4 = cor(he[,c('X12.Month.Rolling.Revenue', 'X..Establishments', 'X..Employees', 
                    "Average.Monthly.Page.Volume", "Total...of.Machines..Serial.Numbers.", 
                    "X..of.Office.Mono.machines", "X..of.Office.Color.machines", 
                    "X..of.Office.Printers", "X..of.Production.Mono.machines", 
                    "X..of.Production.EPC.machines", "X..of.Production.iGen.machines", 
                    "X..of.Wide.Format.machines", "A3.Serial..s", 
                    "Drilldown..Average.of.Tech.Service...Communication")])
corrplot.mixed(cor(he[,c('X12.Month.Rolling.Revenue', 'X..Establishments', 'X..Employees', 
                         "Average.Monthly.Page.Volume", "Total...of.Machines..Serial.Numbers.", 
                         "X..of.Office.Mono.machines", "X..of.Office.Color.machines", 
                         "X..of.Office.Printers", "X..of.Production.Mono.machines", 
                         "X..of.Production.EPC.machines", "X..of.Production.iGen.machines", 
                         "X..of.Wide.Format.machines")]), cl.pos = "n", tl.pos = "n")

model4=lm(Drilldown..Average.of.Tech.Service...Communication~., data = henew4)
gmodel4 = step(model4)
summary(gmodel4)


# 5 Invoicing drilldown -->  "Drilldown2..previous.invoicing.questions.had.7.or.less...Average.of.IINVOICING...Ease.of.Help"
henew5 = he[,c('X12.Month.Rolling.Revenue', 'X..Establishments', 'X..Employees', 
               "Average.Monthly.Page.Volume", "Total...of.Machines..Serial.Numbers.", 
               "X..of.Office.Mono.machines", "X..of.Office.Color.machines", 
               "X..of.Office.Printers", "X..of.Production.Mono.machines", 
               "X..of.Production.EPC.machines", "X..of.Production.iGen.machines", 
               "X..of.Wide.Format.machines", "Drilldown2..previous.invoicing.questions.had.7.or.less...Average.of.IINVOICING...Ease.of.Help")]
he_cor5 = cor(he[,c('X12.Month.Rolling.Revenue', 'X..Establishments', 'X..Employees', 
                    "Average.Monthly.Page.Volume", "Total...of.Machines..Serial.Numbers.", 
                    "X..of.Office.Mono.machines", "X..of.Office.Color.machines", 
                    "X..of.Office.Printers", "X..of.Production.Mono.machines", 
                    "X..of.Production.EPC.machines", "X..of.Production.iGen.machines", 
                    "X..of.Wide.Format.machines", 
                    "Drilldown2..previous.invoicing.questions.had.7.or.less...Average.of.IINVOICING...Ease.of.Help")])
corrplot.mixed(cor(he[,c('X12.Month.Rolling.Revenue', 'X..Establishments', 'X..Employees', 
                         "Average.Monthly.Page.Volume", "Total...of.Machines..Serial.Numbers.", 
                         "X..of.Office.Mono.machines", "X..of.Office.Color.machines", 
                         "X..of.Office.Printers", "X..of.Production.Mono.machines", 
                         "X..of.Production.EPC.machines", "X..of.Production.iGen.machines", 
                         "X..of.Wide.Format.machines")]), cl.pos = "n", tl.pos = "n")

model5=lm(Drilldown2..previous.invoicing.questions.had.7.or.less...Average.of.IINVOICING...Ease.of.Help~., data = henew5)
gmodel5 = step(model5)
summary(gmodel5)

# 6 Invoicing drilldown -->  "Drilldown..Average.of.INVOICING...Accurate"
henew6 = he[,c('X12.Month.Rolling.Revenue', 'X..Establishments', 'X..Employees', 
               "Average.Monthly.Page.Volume", "Total...of.Machines..Serial.Numbers.", 
               "X..of.Office.Mono.machines", "X..of.Office.Color.machines", 
               "X..of.Office.Printers", "X..of.Production.Mono.machines", 
               "X..of.Production.EPC.machines", "X..of.Production.iGen.machines", 
               "X..of.Wide.Format.machines", "Drilldown..Average.of.INVOICING...Accurate")]
he_cor6 = cor(he[,c('X12.Month.Rolling.Revenue', 'X..Establishments', 'X..Employees', 
                    "Average.Monthly.Page.Volume", "Total...of.Machines..Serial.Numbers.", 
                    "X..of.Office.Mono.machines", "X..of.Office.Color.machines", 
                    "X..of.Office.Printers", "X..of.Production.Mono.machines", 
                    "X..of.Production.EPC.machines", "X..of.Production.iGen.machines", 
                    "X..of.Wide.Format.machines", 
                    "Drilldown..Average.of.INVOICING...Accurate")])
corrplot.mixed(cor(he[,c('X12.Month.Rolling.Revenue', 'X..Establishments', 'X..Employees', 
                         "Average.Monthly.Page.Volume", "Total...of.Machines..Serial.Numbers.", 
                         "X..of.Office.Mono.machines", "X..of.Office.Color.machines", 
                         "X..of.Office.Printers", "X..of.Production.Mono.machines", 
                         "X..of.Production.EPC.machines", "X..of.Production.iGen.machines", 
                         "X..of.Wide.Format.machines")]), cl.pos = "n", tl.pos = "n")  

model6=lm(Drilldown..Average.of.INVOICING...Accurate~., data = henew6)
gmodel6 = step(model6)
summary(gmodel6)


