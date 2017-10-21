#--------------------------------------------------
#
#  Data analysis for the Kagel Titanic dataset
#
#--------------------------------------------------

test <- read.csv("test.csv", header=TRUE)
train <- read.csv("train.csv", header=TRUE)

test.augmented <- data.frame(Survived = rep("None", nrow(test)), test[,])

data.combined <- rbind(train, test.augmented)

data.combined$Pclass = as.factor(data.combined$Pclass)

data.combined$Survived = as.factor(data.combined$Survived)

table(data.combined$Pclass)

library("ggplot2")

train$Pclass <- as.factor(train$Pclass)

ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

tail(as.character(train$Name))

length(unique(data.combined$Name))

repeated.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

data.combined[which(data.combined$Name %in% repeated.names),]

library(stringr)

married <- data.combined[which(str_detect(data.combined$Name, "Mrs")),]

married[1:5,]

men <- data.combined[which(data.combined$Sex == "male"),]

men[1:5,]


extractTitle <- function(name) {
  name <- as.character(name)
  
  if (length(grep("Miss", name)) > 0) {
    return ("Miss")
  }
  else if (length(grep("Mrs", name)) > 0) {
    return ("Mrs")
  }
  else if (length(grep("Mr", name)) > 0) {
    return ("Mr")
  }
  else if (length(grep("Master", name)) > 0) {
    return ("Master")
  }
  else {
    return ("Other")
  }
}

titles <- NULL

for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i, "Name"]))
}

data.combined$Title <- as.factor(titles)

ggplot(data.combined[1:891,], aes(Title, fill = Survived)) +
  geom_histogram(stat = "count", binwidth = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("PClass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

data.combined$Title <- as.factor(titles)

ggplot(data.combined[1:891,], aes(Sex, fill = Survived)) +
  geom_histogram(stat = "count", binwidth = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("PClass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

summary(data.combined$Age)

summary(data.combined[1:891, "Age"])

master <- data.combined[which(data.combined$Title == "Master"),]

summary(master$Age)

miss <- data.combined[which(data.combined$Title == "Miss"),]

summary(miss$Age)

mrs <- data.combined[which(data.combined$Title == "Mrs"),]

summary(mrs$Age)

mr <- data.combined[which(data.combined$Title == "Mr"),]

summary(mr$Age)

data.combined$SibSp <- as.factor(data.combined$SibSp)

ggplot(data.combined[1:893,], aes(x = SibSp, fill = Survived)) +
  geom_histogram(stat = "count", binwidth = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("PClass, Title") +
  xlab("Sibspa") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

ggplot(data.combined[1:893,], aes(x = Parch, fill = Survived)) +
  geom_histogram(stat = "count", binwidth = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("PClass, Title") +
  xlab("Parch") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

temp.Sibspa <- c(train$SibSp, test$SibSp)
temp.Parch <- c(train$Parch, test$Parch)
data.combined$Familysize <- as.factor(temp.Sibspa + temp.Parch + 1)

ggplot(data.combined[1:893,], aes(x = Familysize, fill = Survived)) +
  geom_histogram(stat = "count", binwidth = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("PClass, Title") +
  xlab("Familysize") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

data.combined$Ticket<-as.character(data.combined$Ticket)

data.combined$FirstChar <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1, 1))

unique(data.combined$FirstChar)

data.combined$TicketClass <- as.factor(data.combined$FirstChar)

summary(data.combined$TicketClass)

ggplot(data.combined[1:893,], aes(x = TicketClass, fill = Survived)) +
  geom_histogram(stat = "count", binwidth = 1) +
  ggtitle("Ticket, Title") +
  xlab("TicketClass") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


ggplot(data.combined[1:893,], aes(x = TicketClass, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Ticket, Title") +
  xlab("Tickeclass") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

data.combined$Cabin <- as.character(data.combined$Cabin)

data.combined[which(is.na(data.combined$Cabin)),"Cabin"] <- "U"
data.combined$Newcbin <- substr(data.combined$Cabin, 1, 1)
#data.combined$Newcbin <- as.factor(ifelse(data.combined$Newcbin == "U", " ", substr(data.combined$Newcbin, 1, 1)))

ggplot(data.combined[1:893,], aes(x = Newcbin, fill = Survived)) +
  geom_bar() +
  ggtitle("Ticket, Title") +
  xlab("Cabin") +
  ylab("Total Count") +
  ylim(0,900) +
  labs(fill = "Survived")

data.combined$MultiCabin <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))

ggplot(data.combined[1:893,], aes(x = MultiCabin, fill = Survived)) +
  geom_bar() +
  ggtitle("Ticket, Title") +
  xlab("Cabin") +
  ylab("Total Count") +
  ylim(0,900) +
  labs(fill = "Survived")


#--------------------------------------------
#
#  Models
#
#--------------------------------------------