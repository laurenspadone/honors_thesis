
# SCN8A

#x1 - Have seizures (yes,no)
seizures_lofplus <- rep(1,100)
seizures_lofminus <- rep(0,100)
seizures_de <- c(rep(1,10),rep(0,90))
seizures_ee <- rep(1,100)
seizures_dee <- rep(1,100)
x1 <- c(seizures_de,seizures_ee,seizures_dee,seizures_lofplus,seizures_lofminus)
x1 <- as.factor(x1)

#x2 - Motor/Focal Seizures (yes,no)
mf_lofplus <- c(rep(1,25),rep(0,75))
mf_lofminus <- rep(0,100)
mf_de <- c(rep(1,75),rep(0,25))
mf_ee <- rep(1,100)
mf_dee <- rep(1,100)
x2 <- c(mf_de,mf_ee,mf_dee,mf_lofplus,mf_lofminus)
x2 <- as.factor(x2)

#x3 - Absence Seizures (yes,no)
ab_lofplus <- c(rep(0,25), rep(1,50),rep(0,25))
ab_lofminus <- rep(0,100)
ab_de <- c(rep(0,30), rep(1,50),rep(0,20))
ab_ee <- rep(0,100)
ab_dee <- rep(0,100)
x3 <- c(ab_de,ab_ee,ab_dee,ab_lofplus,ab_lofminus)
x3 <- as.factor(x3)

#x4 - Age at seizure onset
age_lofplus <- rnorm(100,40,15)
age_lofminus <- rep(75,100)
age_de <- rnorm(100,5,2)
age_ee <- rnorm(100,3,1)
age_dee <- rnorm(100,3,1.5)
x4 <- c(age_de,age_ee,age_dee,age_lofplus,age_lofminus)

#x5 - Severe Developmental Delay (yes,no)
idd_lofplus <- c(rep(1,20), rep(0,80))
idd_lofminus <- c(rep(1,80), rep(0,20))
idd_de <- rep(1,100)
idd_ee <- c(rep(0,97),rep(1,3))
idd_dee <- rep(1,100)
x5 <- c(idd_de,idd_ee,idd_dee,idd_lofplus,idd_lofminus)
x5 <- as.factor(x5)

#x6 - Developmental Quotient
dq_lofplus <- rnorm(100,65.46,36.73)
dq_lofminus <- rnorm(100,19.96,35.17)
dq_de <- rnorm(100,37,27.6)
dq_ee <- rnorm(100,98.79,2.99)
dq_dee <- rnorm(100,13.91,16.69)
x6 <- c(dq_de,dq_ee,dq_dee,dq_lofplus,dq_lofminus)

#x7 - Age at developmental delay onsest
age2_lofplus <- rnorm(100,96.42,114.55)
age2_lofminus <- rnorm(100,10,2.5)
age2_de <- rnorm(100,8.82,7.82)
age2_ee <- rnorm(100,152.8,159.33)
age2_dee <- rnorm(100,5.03,15.69)
x7 <- c(age2_de,age2_ee,age2_dee,age2_lofplus,age2_lofminus)

#y: 1 - DE, 2 - EE, 3 - DEE, 4 - LOF+, 5 - LOF-
y <- c(rep(1,100),rep(2,100),rep(3,100),rep(4,100), rep(5,100))

train_data <- data.frame(y,x1,x2,x3,x4,x5)
train_data2 <-data.frame(y,x1,x2,x3,x4,x5,x6,x7)

five_matrix = matrix(data = 0, ncol = 5, nrow = 5)
five_matrix[3, 1:2] = 1
five_matrix

hasse(apply(five_matrix, 2, as.logical), parameters = list(arrow = "backward"))

y2 <- c(rep(0,200),rep(1,100))
id_group_comp <- sample(seq(1,200,1),100,replace=FALSE)
id <- c(seq(1,200,1),id_group_comp, rep(1,200))

final_predictions = predict_por(train_data, five_matrix, 1, id)

table(y = train_data$y, predictions = final_predictions)

five_matrix_models = train_models(five_matrix, train_data, 1, id)

predict_1 = predict_new_individual(five_matrix_models, train_data[150,], five_matrix, id[150])
full_graph(five_matrix, predict_1$step_0_probs, predict_1$step_1_probs, predict_1$step_2_probs, predict_1$final_probs)
