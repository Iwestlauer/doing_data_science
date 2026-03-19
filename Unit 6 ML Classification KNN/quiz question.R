Emails = data.frame(Predicted = c("Spam","Ham","Ham", "Ham", "Ham", "Spam", "Ham", "Spam", "Ham", "Spam"), Actual = c("Spam", "Spam", "Ham", "Ham", "Spam", "Ham", "Spam","Ham","Spam","Spam" ))

table(Emails)

confusionMatrix(table(Emails$Predicted,Emails$Actual))
