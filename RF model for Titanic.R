#Titanic kaggle competition 
setwd("~/Desktop/Titanic")
train<-read.csv("train.csv")
train<-train[,c(2,1,3:12)]

test<-read.csv("test.csv")
test.Survived <- data.frame(Survived = rep("None", nrow(test)), test[,])
data.combined <- rbind(train, test.Survived)



data.combined$survived <- as.factor(data.combined$Survived)
data.combined$pclass <- as.factor(data.combined$Pclass)

library(ggplot2)


# Hypothesis - Rich folks survived at a higer rate
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar() +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived") 




extractTitle <- function(Name) {
  Name <- as.character(Name)

  if (length(grep("Miss.", Name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Master.", Name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.", Name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", Name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}


# NOTE - The code below uses a for loop which is not a very R way of
#        doing things
titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"Name"]))
}
data.combined$title <- as.factor(titles)





# Let's try some feature engineering. What about creating a family size feature?
temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch, test$Parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)


library(rpart)
library(rpart.plot)
library(stringr)
# Parse out last name and title
data.combined[1:25, "Name"]

name.splits <- str_split(data.combined$Name, ",")
name.splits[1]
last.names <- sapply(name.splits, "[", 1)
last.names[1:10]

# Add last names to dataframe in case we find it useful later
data.combined$last.name <- last.names

# Now for titles
name.splits <- str_split(sapply(name.splits, "[", 2), " ")
titles <- sapply(name.splits, "[", 2)
unique(titles)

# Re-map titles to be more exact
titles[titles %in% c("Dona.", "the")] <- "Lady."
titles[titles %in% c("Ms.", "Mlle.")] <- "Miss."
titles[titles == "Mme."] <- "Mrs."
titles[titles %in% c("Jonkheer.", "Don.")] <- "Sir."
titles[titles %in% c("Col.", "Capt.", "Major.")] <- "Officer"
table(titles)

# Make title a factor
data.combined$new.title <- as.factor(titles)

# Collapse titles based on visual analysis
indexes <- which(data.combined$new.title == "Lady.")
data.combined$new.title[indexes] <- "Mrs."

indexes <- which(data.combined$new.title == "Dr." |
                   data.combined$new.title == "Rev." |
                   data.combined$new.title == "Sir." |
                   data.combined$new.title == "Officer")
data.combined$new.title[indexes] <- "Mr."


data.combined$new.title <- as.factor(data.combined$new.title)

table(data.combined$new.title)


ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$Ticket)

for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$Ticket == current.ticket)
  current.avg.fare <- data.combined[party.indexes[1], "Fare"] / length(party.indexes)

  for (k in 1:length(party.indexes)) {
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}

data.combined$ticket.party.size <- ticket.party.size
data.combined$avg.fare <- avg.fare
data.combined$avg.fare[rownames(data.combined) %in% c("1044")]<-7.8

write.csv(data.combined, file = "data.combined.csv", row.names = FALSE)

library(randomForest)

# Train a Random Forest with the default parameters using pclass & title
rf.train.1 <- data.combined[1:891, c("Pclass", "new.title","family.size","avg.fare")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)


test.submit.df <- data.combined[892:1309, c("Pclass", "new.title","family.size","avg.fare")]
test.submit.df$avg.fare[rownames(test.submit.df) %in% c("1044")]<-7.8
table(is.na.data.frame(test.submit.df))
# Make predictions
rf.1.preds <- predict(rf.1, test.submit.df)
table(rf.1.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.1.preds)

write.csv(submit.df, file = "RF_SUB_20181111_1.csv", row.names = FALSE)

