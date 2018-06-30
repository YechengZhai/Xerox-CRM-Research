setwd('/Users/Apple/Desktop')
# healthcare-recommend/repurchase~five overall scores
xerox.healthcare <- xerox[xerox$ACCOUNT_SUB_VERTICAL=='Healthcare',]
model1 <- lm(xerox.healthcare$Recommend ~ xerox.healthcare$`Overall Sales`
            +xerox.healthcare$`Overall Equipment`
            +xerox.healthcare$`Overall Tech Svc`
            +xerox.healthcare$`Overall Supplies`
            +xerox.healthcare$`Overall Invoicing`)
summary(model1)

model2 <- lm(xerox.healthcare$Repurchase ~ xerox.healthcare$`Overall Sales`
             +xerox.healthcare$`Overall Equipment`
             +xerox.healthcare$`Overall Tech Svc`
             +xerox.healthcare$`Overall Supplies`
             +xerox.healthcare$`Overall Invoicing`)
summary(model2)

# k12-recommend/repurchase~five overall scores
xerox.k12 <- xerox[xerox$ACCOUNT_SUB_VERTICAL=='K12',]
model3 <- lm(xerox.k12$Recommend ~ xerox.k12$`Overall Sales`
             +xerox.k12$`Overall Equipment`
             +xerox.k12$`Overall Tech Svc`
             +xerox.k12$`Overall Supplies`
             +xerox.k12$`Overall Invoicing`)
summary(model3)

model4 <- lm(xerox.k12$Repurchase ~ xerox.k12$`Overall Sales`
             +xerox.k12$`Overall Equipment`
             +xerox.k12$`Overall Tech Svc`
             +xerox.k12$`Overall Supplies`
             +xerox.k12$`Overall Invoicing`)
summary(model4)

model.healthcare.overallsales <- lm(xerox.healthcare$`Overall Sales`~xerox.healthcare$`# of Office Mono machines`
   +xerox.healthcare$`# of Office Color machines`
   +xerox.healthcare$`# of Office Printers`
   +xerox.healthcare$`# of Production Mono machines`
   +xerox.healthcare$`# of Production EPC machines`
   +xerox.healthcare$`# of Production iGen machines`
   +xerox.healthcare$`# of Wide Format machines`)
summary(model.healthcare.overallsales)

