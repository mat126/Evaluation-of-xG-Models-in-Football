# Pacchetti ---------------------------------------------------------------

library(dplyr)
library(tools)
library(stringi)
library(gtsummary)
library(pROC)
library(caret)
library(predtools)
library(magrittr)
library(probably)
library(tidymodels)
library(caTools)
library(boot)
library(randomForest)
library(MASS)
library(rpart)
library(ranger)
library(gbm)
library(xgboost)
library(e1071)
library(nnet)
library(ecospat)

# Dataset ------------------------------------------
percorso_file <- file.path("...", "shots_dataset.csv")
shots_dataset <- read.csv(percorso_file)
percorso_file2 <- file.path("...", "FullData.csv")
FullData <- read.csv(percorso_file2)
percorso_file3 <- file.path("...", "players.csv")
players <- read.csv(percorso_file3)
percorso_file4 <- file.path("...", "new.players.csv")
new.players <- read.csv(percorso_file4)
View(shots_dataset)


# Modifiche e pulizia dei dati --------------------------------------------
shots_dataset$player <- stri_trans_general(shots_dataset$player, "Latin-ASCII")
giocatori <- as.data.frame(unique(shots_dataset$player))
View(giocatori)
names(giocatori)[names(giocatori) == "unique(shots_dataset$player)"] <- "Name"
foot_dataset <- FullData[, c("Name", "Preffered_Foot")]
foot_dataset$Name <- stri_trans_general(foot_dataset$Name, "Latin-ASCII")
View(foot_dataset)
giocatori2 <- merge(giocatori, foot_dataset, by = "Name", all.x = TRUE)
View(giocatori2)
colSums((is.na(giocatori2)))
player2 <- players[, c("name", "foot")]
View(player2)
player2 <- na.omit(player2)
player2$name <- stri_trans_general(player2$name, "Latin-ASCII")
names(player2)[names(player2) == "name"] <- "Name"
giocatori3 <- merge(giocatori2, player2, by = "Name", all.x = TRUE)
View(giocatori3)
##

giocatori3$Preffered_Foot <- ifelse(is.na(giocatori3$Preffered_Foot), giocatori3$foot, giocatori3$Preffered_Foot)
colSums(is.na(giocatori3))

new.players <- new.players[, c("name", "foot")]
##
nomi_da_cambiare <- c("Andrej Galabinov", "Anssumane Fati", "Arnaud Kalimuendo Muinga", "Daniel Parejo", "Emile Smith-Rowe", "Kephren Thuram", "Kylian Mbappe-Lottin", "Marco Faraoni", "Nemanja Vidci", "Pablo Daniel Osvaldo", "Luca Toni", "Tanguy NDombele Alvaro", "Thiago Alcantara", "Yeremi Pino", "Papis Demba Cisse", "Kevin Kuranyi", "Dimitar Berbatov")

giocatori3 <- giocatori3 %>%
  mutate(Preffered_Foot = ifelse(Name %in% nomi_da_cambiare, "Right", Preffered_Foot))
giocatori3 <- subset(giocatori3, select = -c(foot))

nomi_da_cambiare2 <- c("Dimitri Kombarov", "Lee Kang-In", "Mohammed Ali-Cho", "Yaroslav Rakitskiy", "Pape Alassane Gueye")

giocatori3 <- giocatori3 %>%
  mutate(Preffered_Foot = ifelse(Name %in% nomi_da_cambiare2, "Left", Preffered_Foot))

nomi_da_cambiare3 <- c("Kostas Mitroglu", "Santiago Cazorla", "Son Heung-Min")

giocatori3 <- giocatori3 %>%
  mutate(Preffered_Foot = ifelse(Name %in% nomi_da_cambiare3, "both", Preffered_Foot))

giocatori3 <- giocatori3 %>%
  mutate(across(everything(), na_if, ""))
giocatori3$Preffered_Foot <- toTitleCase(giocatori3$Preffered_Foot)

giocatori3$Preffered_Foot <- ifelse(giocatori3$Preffered_Foot == "Right", "RightFoot", ifelse(giocatori3$Preffered_Foot == "Left", "LeftFoot", giocatori3$Preffered_Foot))

names(giocatori3)[names(giocatori3) == "Name"] <- "player"

shots_dataset2 <- merge(shots_dataset, giocatori3, by = "player", all.x = TRUE)

View(shots_dataset2)

shots_dataset2 <- shots_dataset2 %>%
  mutate(Preffered_Foot = ifelse(player == "Franck Zambo", "Right", Preffered_Foot))

shots_dataset2 <- shots_dataset2[shots_dataset2$result != "OwnGoal", ]

shots_dataset2$result <- ifelse(shots_dataset2$result == "Goal", 1, 0)

shots_dataset2$result <- as.factor(shots_dataset2$result)

shots_dataset2 <- na.omit(shots_dataset2)

names(shots_dataset2)[names(shots_dataset2) == "xG"] <- "xG Understat"

###

premier_league <- c("Watford", "Norwich", "Sheffield United", "Brighton", "Southampton", "Chelsea", "Liverpool", "Manchester United", "Wolverhampton Wanderers", "Tottenham", "Aston Villa", "Burnley", "Swansea", "West Ham", "Leeds", "Crystal Palace", "Manchester City", "Newcastle United", "Fulham", "Hull", "Arsenal", "Bournemouth", "West Bromwich Albion", "Stoke", "Sunderland", "Huddersfield", "Everton", "Leicester", "Cardiff", "Middlesbrough", "Queens Park Rangers", "Brentford")
seriea <- c("Bologna", "Genoa", "Salernitana", "AC Milan", "Fiorentina", "Cagliari", "Lazio", "Juventus", "Parma Calcio 1913", "Crotone", "SPAL 2013", "Sassuolo", "Lecce", "Udinese", "Roma", "Torino", "Inter", "Brescia", "Verona", "Parma", "Palermo", "Napoli", "Carpi", "Frosinone", "Venezia", "Sampdoria", "Benevento", "Spezia", "Empoli", "Chievo", "Atalanta", "Cesena", "Pescara")
bundesliga <- c("Hamburger SV", "Wolfsburg", "Eintracht Frankfurt", "RasenBallsport Leipzig", "Freiburg", "Bayern Munich", "Bayer Leverkusen", "Schalke 04", "Hertha Berlin", "Werder Bremen", "VfB Stuttgart", "Darmstadt", "Bochum", "Mainz 05", "Hannover 96", "Arminia Bielefeld", "Fortuna Duesseldorf", "Augsburg", "Hoffenheim", "Borussia M.Gladbach", "Paderborn", "FC Cologne", "Borussia Dortmund", "Greuther Fuerth", "Nuernberg", "Ingolstadt", "Union Berlin")
ligue1 <- c("Toulouse", "Marseille", "Nice", "Bordeaux", "Saint-Etienne", "Lille", "Strasbourg", "Guingamp", "Metz", "Brest", "Lyon", "Montpellier", "Reims", "Dijon", "Nantes", "Angers", "Rennes", "SC Bastia", "Paris Saint Germain", "Monaco", "Caen", "GFC Ajaccio", "Nimes", "Lorient", "Amiens", "Lens", "Troyes", "Evian Thonon Gaillard", "Nancy", "Clermont Foot")
liga <- c("Real Betis", "Celta Vigo", "Alaves", "Levante", "Elche", "Granada", "Deportivo La Coruna", "Villarreal", "Cordoba", "Athletic Club", "Rayo Vallecano", "Mallorca", "Real Valladolid", "Real Sociedad", "Sevilla", "Atletico Madrid", "Real Madrid", "Eibar", "Getafe", "Barcelona", "Malaga", "Girona", "Las Palmas", "Valencia", "SD Huesca", "Sporting Gijon", "Osasuna", "Cadiz", "Leganes", "Almeria")
shots_dataset2$lega <- ifelse(shots_dataset2$h_team %in% premier_league | shots_dataset2$a_team %in% premier_league, "Premier League",
                              ifelse(shots_dataset2$h_team %in% seriea | shots_dataset2$a_team %in% seriea, "Serie A", 
                                     ifelse(shots_dataset2$h_team %in% bundesliga | shots_dataset2$a_team %in% bundesliga, "Bundesliga", 
                                            ifelse(shots_dataset2$h_team %in% ligue1 | shots_dataset2$a_team %in% ligue1, "Ligue 1", 
                                                   ifelse(shots_dataset2$h_team %in% liga | shots_dataset2$a_team %in% liga, "Liga", "Russian PL")))))

##

subset_penalty <- subset(shots_dataset2, situation == "Penalty")

subset_openplay <- subset(shots_dataset2, situation == "OpenPlay")

subset_setpiece <- subset(shots_dataset2, situation %in% c("SetPiece", "FromCorner", "DirectFreekick"))

View(subset_openplay)

openplay_foot <- subset(subset_openplay, shotType %in% c("LeftFoot", "RightFoot"))
openplay_head <- subset(subset_openplay, shotType == "Head")

setpiece_foot <- subset(subset_setpiece, shotType %in% c("LeftFoot", "RightFoot"))
setpiece_head <- subset(subset_setpiece, shotType == "Head")

tbl_summary(openplay_foot, include = c(minute, result, X, Y, shotType, lastAction))
tbl_summary(openplay_head, include = c(minute, result, X, Y, shotType, lastAction))
tbl_summary(setpiece_foot, include = c(minute, result, X, Y, shotType, lastAction))
tbl_summary(setpiece_head, include = c(minute, result, X, Y, shotType, lastAction))


#tolgo i lastaction <10

openplay_foot <- subset(openplay_foot, !lastAction %in% c("ChanceMissed", "Error", "KeeperPickup", "KeeperSweeper", "OffsideProvoked", "Punch", "ShieldBallOpp", "Smother"))
openplay_head <- subset(openplay_head, !lastAction %in% c("BallRecovery", "BlockedPass", "Card", "Clearance", "Dispossessed", "Error", "FormationChange", "Interception", "KeeperPickup", "LayOff", "OffsidePass", "Save", "SubstiotutionOff"))
setpiece_foot <- subset(setpiece_foot, !lastAction %in% c("Challenge", "Clearance", "Error", "FormationChange", "KeeperPickup", "KeeperSweeper", "OffsidePass", "OffsideProvoked", "Save", "Start", "SubstiotutionOn"))
setpiece_head <- subset(setpiece_head, !lastAction %in% c("Clearance", "Error", "FormationChange", "KeeperPickup", "LayOff", "OffsideProvoked", "Save", "ShieldBallOpp", "Standard", "Start", "SubstiotutionOn", "Throughball"))

#devo introdurre il is_weakfoot

openplay_foot$is_weakfoot <- ifelse(openplay_foot$Preffered_Foot == openplay_foot$shotType, "No", ifelse(openplay_foot$Preffered_Foot == "both", "No", "Yes"))
setpiece_foot$is_weakfoot <- ifelse(setpiece_foot$Preffered_Foot == setpiece_foot$shotType, "No", ifelse(setpiece_foot$Preffered_Foot == "both", "No", "Yes"))

understat_xG_op_foot <- openplay_foot$xG
understat_xG_op_head <- openplay_head$xG
understat_xG_sp_foot <- setpiece_foot$xG
understat_xG_sp_head <- setpiece_head$xG

View(openplay_foot)


# Legenda -----------------------------------------------------------------

##modello1.1 <- Open Play Foot normale
##modello1.2 <- Open Play Foot interazioni
##modello1.3 <- Open Play Foot lda
##modello1.4 <- Open Play Foot RandomForest
##modello1.5 <- Open Play Foot Bagging
##modello1.6 <- Open Play Foot Boosting (non è venuto)
##modello1.7 <- Open Play Foot SVM (da fare; ci vuole tanto tempo)
##modello1.8 <- open play foot neural net



# Modello OpenPlay foot ---------------------------------------------------


set.seed(123)
index_opf <- sample.split(Y = openplay_foot$result, SplitRatio = 0.75)
train_opf <- openplay_foot[index_opf, ]

indice_test_opf <-  which(index_opf == FALSE)
test_opf <- openplay_foot[indice_test_opf, ] 



# Modello Lineare ---------------------------------------------------------


modello1.1 <- glm(result ~ minute + lastAction + is_weakfoot + X + exp(Y^2) + h_a, 
                                         data = train_opf, family = binomial)

summary1.1 <- summary(modello1.1)

predict1.1 <- predict.glm(modello1.1, newdata = test_opf, type = "response")

roc1.1 <- roc(test_opf$result ~ predict1.1, plot = T)

confusione1.1 <- confusionMatrix(as.factor(ifelse(predict1.1>0.3, 1, 0)), as.factor(test_opf$result)) 

cal_plot1.1 <- rms::val.prob(as.numeric(as.character(predict1.1)), as.numeric(as.character(test_opf$result)))

step(modello1.1, direction = "both", scope = .~(.)^2, trace = 2)

modello1.2 <- glm(formula = result ~ minute + lastAction + is_weakfoot + X + 
                    exp(Y^2) + h_a + lastAction:X + is_weakfoot:exp(Y^2) + X:exp(Y^2) + 
                    is_weakfoot:X + exp(Y^2):h_a + minute:lastAction + minute:h_a + 
                    minute:exp(Y^2) + lastAction:h_a, family = binomial, data = train_opf)

summary1.2 <- summary(modello1.2)

predict1.2 <- predict.glm(modello1.2, newdata = test_opf, type = "response")

confusione1.2 <- confusionMatrix(as.factor(ifelse(predict1.2>0.3, 1, 0)), as.factor(test_opf$result))

cal_plot1.2 <- rms::val.prob(as.numeric(as.character(predict1.2)), as.numeric(as.character(test_opf$result)))

roc1.2 <- roc(test_opf$result ~ predict1.2, plot = T)


# LDA opf -----------------------------------------------------------------


set.seed(1234)


modello1.3 <- lda(result ~ minute + lastAction + is_weakfoot + X + exp(Y^2) + h_a, data = train_opf)

summary1.3 <- summary(modello1.3)

predict1.3.1 <- predict(modello1.3, newdata = test_opf)

head(predict1.3.1$posterior)

predict1.3 <- predict1.3.1$posterior[, 2]

head(predict1.3)

confusione1.3.1 <- confusionMatrix(as.factor(predict1.3.1$class), as.factor(test_opf$result))

head(new_test_opf$`xG Understat`)

confusione1.3 <- confusionMatrix(as.factor(ifelse(predict1.3>0.3, 1, 0)), as.factor(test_opf$result))

roc1.3 <- roc(test_opf$result ~ predict1.3, plot = T)

cal_plot1.3 <- rms::val.prob(as.numeric(as.character(predict1.3)), as.numeric(as.character(test_opf$result)))

set.seed(1234)

new_train_opf <- train_opf

new_train_opf$Y <- exp(new_train_opf$Y^2)

new_test_opf <- test_opf

new_test_opf$Y <- exp(new_test_opf$Y^2)

# Alberi opf --------------------------------------------------------------

set.seed(1234)

modello1.4 <- ranger(train_opf$result ~ minute + lastAction + is_weakfoot + X + Y + h_a, 
                     data = new_train_opf, num.trees = 500, mtry = 3, seed = 42, probability = T)

predict1.4.1 <- predict(modello1.4, data = new_test_opf)

predict1.4 <- predict1.4.1$predictions[, 2]

confusione1.4 <- confusionMatrix(as.factor(ifelse(predict1.4 >0.3, 1, 0)), as.factor(test_opf$result))

roc1.4 <- roc(test_opf$result ~ as.numeric(predict1.4), plot = T)

cal_plot1.4 <- rms::val.prob(as.numeric(as.character(predict1.4)), as.numeric(as.character(new_test_opf$result)))

set.seed(1234)

modello1.5 <- ranger(train_opf$result ~ minute + lastAction + is_weakfoot + X + Y + h_a, 
                     data = new_train_opf, num.trees = 500, seed = 42, probability = T)

predict1.5.1 <- predict(modello1.5, data = new_test_opf)

predict1.5 <- predict1.5.1$predictions[, 2]

confusione1.5 <- confusionMatrix(as.factor(ifelse(predict1.5>0.3, 1, 0)), as.factor(new_test_opf$result))

roc1.5 <- roc(new_test_opf$result ~ as.numeric(predict1.5), plot = T)

cal_plot1.5 <- rms::val.prob(as.numeric(as.character(predict1.5)), as.numeric(as.character(new_test_opf$result)))


# Boosting 1 --------------------------------------------------------------


variables_to_keep <- c("result", "minute", "lastAction", "is_weakfoot", "X", "Y", "h_a")

boosting_data_opf <- new_train_opf[, variables_to_keep, drop = FALSE]

boosting_test_opf <- new_test_opf[, variables_to_keep, drop = F]
View(boosting_data_opf)

head(boosting_test_opf)

categorical_columns <- c("lastAction", "is_weakfoot", "h_a")

encoded_train_data_opf <- boosting_data_opf %>%
  mutate(across(all_of(categorical_columns), as.factor)) %>%
  model.matrix(~ . - 1, data = .)

encoded_test_opf <- boosting_test_opf %>%
  mutate(across(all_of(categorical_columns), as.factor)) %>%
  model.matrix(~ . - 1, data = .)

encoded_train_data_opf <- encoded_train_data_opf[, -1]
head(encoded_train_data_opf)

encoded_test_opf <- encoded_test_opf[, -1]

encoded_train_data_opf <- as.data.frame(encoded_train_data_opf)

encoded_test_opf <- as.data.frame(encoded_test_opf)

param_grid <- expand.grid(
  nrounds = c(50, 100, 150),
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3),
  gamma = 0,          
  colsample_bytree = c(0.5, 0.75, 1),
  min_child_weight = 1,  
  subsample = c(0.5, 0.75, 1)
)

ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

encoded_train_data_opf$result1 <- as.factor(encoded_train_data_opf$result1)

xgb_model <- train(
  result1 ~ .,
  data = encoded_train_data_opf,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = param_grid,
  metric = "Accuracy"
)

modello1.6 <- xgb_model$finalModel

boosting_test_opf_numeric <- sapply(encoded_test_opf, as.numeric)



if (anyNA(boosting_test_opf_numeric)) {
  
  boosting_test_opf_numeric[is.na(boosting_test_opf_numeric)] <- 0
}


predict1.6.1 <- predict(modello1.6, newdata = encoded_test_opf, type = "response")


# Boosting2 ---------------------------------------------------------------

y_dv<- which(colnames(boosting_data_opf)=="result")
x_iv_start<-y_dv-1
x_iv_end<-y_dv+1
x_iv<-c(1:x_iv_start,x_iv_end:length(boosting_data_opf))

gbm_model <- gbm(
  formula = result1 ~ .,  
  data = encoded_train_data_opf,
  distribution = "bernoulli",  
  n.trees = 500,
  interaction.depth = 4,  
  shrinkage = 0.01,  
)

predict_gbm <- predict.gbm(gbm_model, newdata = encoded_test_opf, type = "response")



View(encoded_test_opf)

View(encoded_train_data_opf)

# SVM OPF ----------------------------------------------------


modello1.7 <- svm(result ~ minute + lastAction + is_weakfoot + X + Y + h_a, data = new_train_opf, kernel = "linear", cost = 10)


# Neural Network opf ------------------------------------------------------


modello1.8 <- nnet(result ~ minute + lastAction + is_weakfoot + X + Y + h_a, data = new_train_opf, size = 10, maxit = 1000, linout = FALSE)

predict1.8 <- predict(modello1.8, newdata = new_test_opf)

confusione1.8 <- confusionMatrix(as.factor(ifelse(predict1.8>0.3, 1, 0)), new_test_opf$result)

roc1.8 <- roc(new_test_opf$result ~ as.numeric(predict1.8), plot = T)

head(predict1.8)

cal_plot1.8 <- rms::val.prob(as.numeric(as.character(predict1.8)), as.numeric(as.character(new_test_opf$result)))


# Risultati Understat opf -------------------------------------------------


confusione1.us <- confusionMatrix(as.factor(ifelse(test_opf$`xG Understat`>0.3, 1, 0)), test_opf$result)

roc1.us <- roc(test_opf$result ~ as.numeric(test_opf$`xG Understat`), plot = T)

cal_plot1.us <- rms::val.prob(as.numeric(as.character(test_opf$`xG Understat`)), as.numeric(as.character(test_opf$result)))


# Cal plots finali opf ----------------------------------------------------



xg_tot_1 <- cbind(train_opf$result, train_opf$`xG Understat`, predict1.1, predict1.2, predict1.3$class, predict1.4$predictions, predict1.5$predictions, predict1.8)
View(xg_tot_1)
xg_tot_1 <- as.data.frame(xg_tot_1)
overall_calibration <- cal_plot_breaks(
  xg_tot_1,
  truth = V1,
  estimate = V8,
  num_breaks = 20,
  conf_level = 0.9,
  event_level = 'second'
)

plot(overall_calibration)
# Modello OpenPlay Head ---------------------------------------------------

set.seed(123)
index_oph <- sample.split(Y = openplay_head$result, SplitRatio = 0.75)
train_oph <- openplay_head[index_oph, ]

indice_test_oph <-  which(index_oph == FALSE)
test_oph <- openplay_head[indice_test_oph, ] 


# Lineare oph -------------------------------------------------------------

modello2.1 <- glm(result ~ minute + lastAction + X + exp(Y^2) + h_a, 
                  data = train_oph, family = binomial)

summary2.1 <- summary(modello2.1)

predict2.1 <- predict.glm(modello2.1, newdata = test_oph, type = "response")

roc2.1 <- roc(test_oph$result ~ predict2.1, plot = T)

confusione2.1 <- confusionMatrix(as.factor(ifelse(predict2.1>0.3, 1, 0)), as.factor(test_oph$result)) 

cal_plot2.1 <- rms::val.prob(as.numeric(as.character(predict2.1)), as.numeric(as.character(test_oph$result)))

step(modello2.1, direction = "both", scope = .~(.)^2, trace = 2)






# LDA oph -----------------------------------------------------------------

set.seed(1234)


modello2.3 <- lda(result ~ minute + lastAction + X + exp(Y^2) + h_a, data = train_oph)

summary2.3 <- summary(modello2.3)

predict2.3.1 <- predict(modello2.3, newdata = test_oph)

head(predict2.3.1$posterior)

predict2.3 <- predict2.3.1$posterior[, 2]

head(predict2.3)

confusione2.3.1 <- confusionMatrix(as.factor(predict2.3.1$class), as.factor(test_oph$result))

head(new_test_oph$`xG Understat`)

confusione2.3 <- confusionMatrix(as.factor(ifelse(predict2.3>0.3, 1, 0)), as.factor(test_oph$result))

roc2.3 <- roc(test_oph$result ~ predict2.3, plot = T)

cal_plot2.3 <- rms::val.prob(as.numeric(as.character(predict2.3)), as.numeric(as.character(test_oph$result)))

set.seed(1234)

new_train_oph <- train_oph

new_train_oph$Y <- exp(new_train_oph$Y^2)

new_test_oph <- test_oph

new_test_oph$Y <- exp(new_test_oph$Y^2)


# Alberi oph --------------------------------------------------------------

set.seed(1234)

modello2.4 <- ranger(train_oph$result ~ minute + lastAction + X + Y + h_a, 
                     data = new_train_oph, num.trees = 500, mtry = 3, seed = 42, probability = T)

predict2.4.1 <- predict(modello2.4, data = new_test_oph)

predict2.4 <- predict2.4.1$predictions[, 2]

confusione2.4 <- confusionMatrix(as.factor(ifelse(predict2.4 >0.3, 1, 0)), as.factor(test_oph$result))

roc2.4 <- roc(test_oph$result ~ as.numeric(predict2.4), plot = T)

cal_plot2.4 <- rms::val.prob(as.numeric(as.character(predict2.4)), as.numeric(as.character(new_test_oph$result)))

set.seed(1234)

modello2.5 <- ranger(train_oph$result ~ minute + lastAction + X + Y + h_a, 
                     data = new_train_oph, num.trees = 500, seed = 42, probability = T)

predict2.5.1 <- predict(modello2.5, data = new_test_oph)

predict2.5 <- predict2.5.1$predictions[, 2]

confusione2.5 <- confusionMatrix(as.factor(ifelse(predict2.5>0.3, 1, 0)), as.factor(new_test_oph$result))

roc2.5 <- roc(new_test_oph$result ~ as.numeric(predict2.5), plot = T)

cal_plot2.5 <- rms::val.prob(as.numeric(as.character(predict2.5)), as.numeric(as.character(new_test_oph$result)))


# Boosting oph ------------------------------------------------------------





# SVM oph -----------------------------------------------------------------


# Neural net oph ----------------------------------------------------------

modello2.8 <- nnet(result ~ minute + lastAction + X + Y + h_a, data = new_train_oph, size = 10, maxit = 1000, linout = FALSE)

predict2.8 <- predict(modello2.8, newdata = new_test_oph)

confusione2.8 <- confusionMatrix(as.factor(ifelse(predict2.8>0.3, 1, 0)), new_test_oph$result)

roc2.8 <- roc(new_test_oph$result ~ as.numeric(predict2.8), plot = T)

cal_plot2.8 <- rms::val.prob(as.numeric(as.character(predict2.8)), as.numeric(as.character(new_test_oph$result)))


# Risultati Understat oph -------------------------------------------------


confusione2.us <- confusionMatrix(as.factor(ifelse(test_oph$`xG Understat`>0.3, 1, 0)), test_oph$result)

roc2.us <- roc(test_oph$result ~ as.numeric(test_oph$`xG Understat`), plot = T)

cal_plot2.us <- rms::val.prob(as.numeric(as.character(test_oph$`xG Understat`)), as.numeric(as.character(test_oph$result)))



# Modello SetPiece Foot ---------------------------------------------------

set.seed(123)
index_spf <- sample.split(Y = setpiece_foot$result, SplitRatio = 0.75)
train_spf <- setpiece_foot[index_spf, ]

indice_test_spf <-  which(index_spf == FALSE)
test_spf <- setpiece_foot[indice_test_spf, ] 


# Lineare spf -------------------------------------------------------------

modello3.1 <- glm(result ~ minute + lastAction + is_weakfoot + X + exp(Y^2) + h_a + situation, 
                  data = train_spf, family = binomial)

summary3.1 <- summary(modello3.1)

predict3.1 <- predict.glm(modello3.1, newdata = test_spf, type = "response")

roc3.1 <- roc(test_spf$result ~ predict3.1, plot = T)

confusione3.1 <- confusionMatrix(as.factor(ifelse(predict3.1>0.3, 1, 0)), as.factor(test_spf$result)) 

cal_plot3.1 <- rms::val.prob(as.numeric(as.character(predict3.1)), as.numeric(as.character(test_spf$result)))



# LDA spf -----------------------------------------------------------------

set.seed(1234)


modello3.3 <- lda(result ~ minute + lastAction + is_weakfoot + X + exp(Y^2) + h_a, data = train_spf)

summary3.3 <- summary(modello3.3)

predict3.3.1 <- predict(modello3.3, newdata = test_spf)

head(predict3.3.1$posterior)

predict3.3 <- predict3.3.1$posterior[, 2]

head(predict3.3)

confusione3.3.1 <- confusionMatrix(as.factor(predict3.3.1$class), as.factor(test_spf$result))

head(new_test_spf$`xG Understat`)

confusione3.3 <- confusionMatrix(as.factor(ifelse(predict3.3>0.3, 1, 0)), as.factor(test_spf$result))

roc3.3 <- roc(test_spf$result ~ predict3.3, plot = T)

cal_plot3.3 <- rms::val.prob(as.numeric(as.character(predict3.3)), as.numeric(as.character(test_spf$result)))

set.seed(1234)

new_train_spf <- train_spf

new_train_spf$Y <- exp(new_train_spf$Y^2)

new_test_spf <- test_spf

new_test_spf$Y <- exp(new_test_spf$Y^2)


# Alberi spf --------------------------------------------------------------
set.seed(1234)

modello3.4 <- ranger(train_spf$result ~ minute + lastAction + is_weakfoot + X + Y + h_a, 
                     data = new_train_spf, num.trees = 500, mtry = 3, seed = 42, probability = T)

predict3.4.1 <- predict(modello3.4, data = new_test_spf)

predict3.4 <- predict3.4.1$predictions[, 2]

confusione3.4 <- confusionMatrix(as.factor(ifelse(predict3.4 >0.3, 1, 0)), as.factor(test_spf$result))

roc3.4 <- roc(test_spf$result ~ as.numeric(predict3.4), plot = T)

cal_plot3.4 <- rms::val.prob(as.numeric(as.character(predict3.4)), as.numeric(as.character(new_test_spf$result)))

set.seed(1234)

modello3.5 <- ranger(train_spf$result ~ minute + lastAction + is_weakfoot + X + Y + h_a, 
                     data = new_train_spf, num.trees = 500, seed = 42, probability = T)

predict3.5.1 <- predict(modello3.5, data = new_test_spf)

predict3.5 <- predict3.5.1$predictions[, 2]

confusione3.5 <- confusionMatrix(as.factor(ifelse(predict3.5>0.3, 1, 0)), as.factor(new_test_spf$result))

roc3.5 <- roc(new_test_spf$result ~ as.numeric(predict3.5), plot = T)

cal_plot3.5 <- rms::val.prob(as.numeric(as.character(predict3.5)), as.numeric(as.character(new_test_spf$result)))



# Boosting spf ------------------------------------------------------------


# SVM spf -----------------------------------------------------------------



# Neural net spf ----------------------------------------------------------

set.seed(1234)

modello3.8 <- nnet(result ~ minute + lastAction + is_weakfoot + X + Y + h_a, data = new_train_spf, size = 10, maxit = 1000, linout = FALSE)

predict3.8 <- predict(modello3.8, newdata = new_test_spf)

confusione3.8 <- confusionMatrix(as.factor(ifelse(predict3.8>0.3, 1, 0)), new_test_spf$result)

roc3.8 <- roc(new_test_spf$result ~ as.numeric(predict3.8), plot = T)

cal_plot3.8 <- rms::val.prob(as.numeric(as.character(predict3.8)), as.numeric(as.character(new_test_spf$result)))


# Risultati Understat spf -------------------------------------------------


confusione3.us <- confusionMatrix(as.factor(ifelse(test_spf$`xG Understat`>0.3, 1, 0)), test_spf$result)

roc3.us <- roc(test_spf$result ~ as.numeric(test_spf$`xG Understat`), plot = T)

cal_plot3.us <- rms::val.prob(as.numeric(as.character(test_spf$`xG Understat`)), as.numeric(as.character(test_spf$result)))


# Modello SetPiece Head ---------------------------------------------------

set.seed(123)
index_sph <- sample.split(Y = setpiece_head$result, SplitRatio = 0.75)
train_sph <- setpiece_head[index_sph, ]

indice_test_sph <-  which(index_sph == FALSE)
test_sph <- setpiece_head[indice_test_sph, ] 


# Lineare sph -------------------------------------------------------------

View(test_sph)

modello4.1 <- glm(result ~ minute + lastAction + X + exp(Y^2) + h_a, 
                  data = train_sph, family = binomial)

summary4.1 <- summary(modello4.1)

predict4.1 <- predict.glm(modello4.1, newdata = test_sph, type = "response")

roc4.1 <- roc(test_sph$result ~ predict4.1, plot = T)

confusione4.1 <- confusionMatrix(as.factor(ifelse(predict4.1>0.3, 1, 0)), as.factor(test_sph$result)) 

cal_plot4.1 <- rms::val.prob(as.numeric(as.character(predict4.1)), as.numeric(as.character(test_sph$result)))


# LDA sph -----------------------------------------------------------------

set.seed(1234)


modello4.3 <- lda(result ~ minute + lastAction + X + exp(Y^2) + h_a, data = train_sph)

summary4.3 <- summary(modello4.3)

predict4.3.1 <- predict(modello4.3, newdata = test_sph)

head(predict4.3.1$posterior)

predict4.3 <- predict4.3.1$posterior[, 2]

head(predict4.3)

confusione4.3.1 <- confusionMatrix(as.factor(predict4.3.1$class), as.factor(test_sph$result))

head(new_test_sph$`xG Understat`)

confusione4.3 <- confusionMatrix(as.factor(ifelse(predict4.3>0.3, 1, 0)), as.factor(test_sph$result))

roc4.3 <- roc(test_sph$result ~ predict4.3, plot = T)

cal_plot4.3 <- rms::val.prob(as.numeric(as.character(predict4.3)), as.numeric(as.character(test_sph$result)))

set.seed(1234)

new_train_sph <- train_sph

new_train_sph$Y <- exp(new_train_sph$Y^2)

new_test_sph <- test_sph

new_test_sph$Y <- exp(new_test_sph$Y^2)



# Alberi sph --------------------------------------------------------------



set.seed(1234)

modello4.4 <- ranger(train_sph$result ~ minute + lastAction + X + Y + h_a, 
                     data = new_train_sph, num.trees = 500, mtry = 3, seed = 42, probability = T)

predict4.4.1 <- predict(modello4.4, data = new_test_sph)

predict4.4 <- predict4.4.1$predictions[, 2]

confusione4.4 <- confusionMatrix(as.factor(ifelse(predict4.4 >0.3, 1, 0)), as.factor(test_sph$result))

roc4.4 <- roc(test_sph$result ~ as.numeric(predict4.4), plot = T)

cal_plot4.4 <- rms::val.prob(as.numeric(as.character(predict4.4)), as.numeric(as.character(new_test_sph$result)))

set.seed(1234)

modello4.5 <- ranger(train_sph$result ~ minute + lastAction + X + Y + h_a, 
                     data = new_train_sph, num.trees = 500, seed = 42, probability = T)

predict4.5.1 <- predict(modello4.5, data = new_test_sph)

predict4.5 <- predict4.5.1$predictions[, 2]

confusione4.5 <- confusionMatrix(as.factor(ifelse(predict4.5>0.3, 1, 0)), as.factor(new_test_sph$result))

roc4.5 <- roc(new_test_sph$result ~ as.numeric(predict4.5), plot = T)

cal_plot4.5 <- rms::val.prob(as.numeric(as.character(predict4.5)), as.numeric(as.character(new_test_sph$result)))


# Boosting sph ------------------------------------------------------------



# SVM sph -----------------------------------------------------------------



# Neural net sph ----------------------------------------------------------

modello4.8 <- nnet(result ~ minute + lastAction + X + Y + h_a, data = new_train_sph, size = 10, maxit = 1000, linout = FALSE)

predict4.8 <- predict(modello4.8, newdata = new_test_sph)

confusione4.8 <- confusionMatrix(as.factor(ifelse(predict4.8>0.3, 1, 0)), new_test_sph$result)

roc4.8 <- roc(new_test_sph$result ~ as.numeric(predict4.8), plot = T)

cal_plot4.8 <- rms::val.prob(as.numeric(as.character(predict4.8)), as.numeric(as.character(new_test_sph$result)))


# Risultati Understat sph -------------------------------------------------


confusione4.us <- confusionMatrix(as.factor(ifelse(test_sph$`xG Understat`>0.3, 1, 0)), test_sph$result)

roc4.us <- roc(test_sph$result ~ as.numeric(test_sph$`xG Understat`), plot = T)

cal_plot4.us <- rms::val.prob(as.numeric(as.character(test_sph$`xG Understat`)), as.numeric(as.character(test_sph$result)))





# Risultati finali --------------------------------------------------------

accuracy_opf <- data.frame(confusione1.us$overall["Accuracy"], confusione1.1$overall["Accuracy"], confusione1.2$overall["Accuracy"], confusione1.3$overall["Accuracy"], confusione1.4$overall["Accuracy"], confusione1.5$overall["Accuracy"], confusione1.8$overall["Accuracy"])
nomi_acc1 <- c("Understat", "Lineare", "Interazione", "Discriminante", "Random_Forest", "Bagging", "Neural_Net")
names(accuracy_opf) <- nomi_acc1
View(accuracy_opf)

auc_opf <- data.frame(roc1.us$auc, roc1.1$auc, roc1.2$auc, roc1.3$auc, roc1.4$auc, roc1.5$auc, roc1.8$auc)
names(auc_opf) <- nomi_acc1
View(auc_opf)

acc_auc1 <- rbind(accuracy_opf, auc_opf)
View(acc_auc1)

accuracy_oph <- data.frame(confusione2.us$overall["Accuracy"], confusione2.1$overall["Accuracy"], confusione2.3$overall["Accuracy"], confusione2.4$overall["Accuracy"], confusione2.5$overall["Accuracy"], confusione2.8$overall["Accuracy"])
nomi_acc2 <- c("Understat", "Lineare", "Discriminante", "Random_Forest", "Bagging", "Neural_Net")
names(accuracy_oph) <- nomi_acc2
View(accuracy_oph)

auc_oph <- data.frame(roc1.us$auc, roc1.1$auc, roc1.3$auc, roc1.4$auc, roc1.5$auc, roc1.8$auc)
names(auc_oph) <- nomi_acc2
View(auc_oph)

acc_auc2 <- rbind.data.frame(accuracy_oph, auc_oph)
View(acc_auc2)

accuracy_spf <- data.frame(confusione3.us$overall["Accuracy"], confusione3.1$overall["Accuracy"], confusione3.3$overall["Accuracy"], confusione3.4$overall["Accuracy"], confusione3.5$overall["Accuracy"], confusione3.8$overall["Accuracy"])
nomi_acc3 <- c("Understat", "Lineare", "Discriminante", "Random_Forest", "Bagging", "Neural_Net")
names(accuracy_spf) <- nomi_acc3
View(accuracy_spf)

auc_spf <- data.frame(roc3.us$auc, roc3.1$auc, roc3.3$auc, roc3.4$auc, roc3.5$auc, roc3.8$auc)
names(auc_spf) <- nomi_acc3
View(auc_spf)

acc_auc3 <- rbind.data.frame(accuracy_spf, auc_spf)
View(acc_auc3)

accuracy_sph <- data.frame(confusione4.us$overall["Accuracy"], confusione4.1$overall["Accuracy"], confusione4.3$overall["Accuracy"], confusione4.4$overall["Accuracy"], confusione4.5$overall["Accuracy"], confusione4.8$overall["Accuracy"])
nomi_acc2 <- c("Understat", "Lineare", "Discriminante", "Random_Forest", "Bagging", "Neural_Net")
names(accuracy_sph) <- nomi_acc2
View(accuracy_sph)

auc_sph <- data.frame(roc4.us$auc, roc4.1$auc, roc4.3$auc, roc4.4$auc, roc4.5$auc, roc4.8$auc)
names(auc_sph) <- nomi_acc2
View(auc_sph)

acc_auc4 <- rbind.data.frame(accuracy_sph, auc_sph)
View(acc_auc4)

# Calibration plot generali -----------------------------------------------

risultati_opf <- data.frame(test_opf$result, test_opf$`xG Understat`, predict1.1, predict1.2, predict1.3, predict1.4, predict1.5, predict1.8)

View(risultati_opf)

names(risultati_opf)[1] <- "result"
names(risultati_opf)[2] <- "xG_understat"
risultati_opf$result <- as.numeric(risultati_opf$result)

ris_cal_plot1 <- data.frame(cal_plot1.us, cal_plot1.1, cal_plot1.2, cal_plot1.3, cal_plot1.4, cal_plot1.5, cal_plot1.8)
View(ris_cal_plot1)
names(ris_cal_plot1) <- nomi_acc1

ris_cal_plot2 <- data.frame(cal_plot2.us, cal_plot2.1, cal_plot2.3, cal_plot2.4, cal_plot2.5, cal_plot2.8)
names(ris_cal_plot2) <- nomi_acc2

ris_cal_plot3 <- data.frame(cal_plot3.us, cal_plot3.1, cal_plot3.3, cal_plot3.4, cal_plot3.5, cal_plot3.8)
names(ris_cal_plot3) <- nomi_acc3

ris_cal_plot4 <- data.frame(cal_plot4.us, cal_plot4.1, cal_plot4.3, cal_plot4.4, cal_plot4.5, cal_plot4.8)
names(ris_cal_plot4) <- nomi_acc3

View(ris_cal_plot2)
View(ris_cal_plot3)
View(ris_cal_plot4)
