library(class)
library(caret)
library(ggplot2)
install.packages('ggplot2')
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
set.seed(123)

path = "E:\\Imarticus\\R Final Project\\dataset_diabetes\\diabetic_data.csv "
diab = read.csv(path,header=T)
colnames(diab)

head(diab)
ncol(diab)
nrow(diab)
str(diab)

# check for Nulls, Zeroes for all columns
col_name = colnames(diab) [apply(diab, 2, function(n) any(is.na(n)))]
if(length(col_name) > 0) print("NULLs present") else print("No NULLs")

col_name = colnames(diab) [apply(diab, 2, function(n) any(n == ""))]
if(length(col_name) > 0) print("Blanks present") else print("No Blanks")

col_name = colnames(diab) [apply(diab, 2, function(n) any(n==0))]
if(length(col_name) > 0) print("0's Present") else print("No 0's")
print(col_name)


#according to discription admission_id ,admission_source_id and discharge_disposition ID are categories 
#Therefore converting them to factor format
diab$admission_type_id=as.factor(diab$admission_type_id)
diab$discharge_disposition_id=as.factor(diab$discharge_disposition_id)
diab$admission_source_id=as.factor(diab$admission_source_id)


#according to discharge_disposition_id some of 
#the categories data is for expire person therefore tha data is irrelevant
#Expired patient data is removed
diab <-diab[!(diab$discharge_disposition_id %in% c(11,12,19,20,21)),]


#Univariate Analysis
namesCol=colnames(diab)
#For Race
levels(diab$race)
table(diab$race)
#since ? data is unknown we will put it in category of other
levels(diab$race)[levels(diab$race)=="?"] <- "Other"
#Gender
table(diab$gender)
#Since Unknown values are very less we can replace it with Mode value
levels(diab$gender)[levels(diab$gender)=="Unknown/Invalid"] <- "Female"
#Age
table(diab$age)
#Weight
table(diab$weight)
#since max values are not given we will delete this column
diab$weight=NULL
#payer code
table(diab$payer_code)
levels(diab$payer_code)[levels(diab$payer_code)=="?"] <- "Other"
#medical Speciality
table(diab$medical_specialty)
levels(diab$medical_specialty)[levels(diab$medical_specialty)=="?"] <- "Unknown"


#reducing level based on IDS mapping
diab$admission_type_id=as.factor(diab$admission_type_id)
levels(diab$admission_type_id)[levels(diab$admission_type_id)=='6' | levels(diab$admission_type_id)=='8']= '5'
levels(diab$admission_type_id)[levels(diab$admission_type_id)=='1' | levels(diab$admission_type_id)=='2' | levels(diab$admission_type_id)=='4']= '7'

diab$admission_source_id=as.factor(diab$admission_source_id)
#diab$time_in_hospital=as.factor(diab$time_in_hospital) # converted it to factor variable because of only 14 values present
levels(diab$admission_source_id)[levels(diab$admission_source_id)=='15' | levels(diab$admission_source_id)=='17' | levels(diab$admission_source_id)=='20' | levels(diab$admission_source_id)=='21']='9'
levels(diab$admission_source_id)[levels(diab$admission_source_id)=='2' | levels(diab$admission_source_id)=='3']='1'
levels(diab$admission_source_id)[levels(diab$admission_source_id)=='11' | levels(diab$admission_source_id)=='23' | levels(diab$admission_source_id)=='24']='8'
levels(diab$admission_source_id)[levels(diab$admission_source_id)=='12' | levels(diab$admission_source_id)=='13' | levels(diab$admission_source_id)=='14']='7'
levels(diab$admission_source_id)[levels(diab$admission_source_id)!='1' & levels(diab$admission_source_id)!='8' & levels(diab$admission_source_id)!='7'& levels(diab$admission_source_id)!='9']='4'

table(diab$admission_source_id)


diab$discharge_disposition_id=as.factor(diab$discharge_disposition_id)
levels(diab$discharge_disposition_id)[levels(diab$discharge_disposition_id)=='13']='1'
levels(diab$discharge_disposition_id)[levels(diab$discharge_disposition_id) %in% c('19','20','21')]='11'

levels(diab$discharge_disposition_id)[levels(diab$discharge_disposition_id) %in% c('25','26')]='18'
levels(diab$discharge_disposition_id)[levels(diab$discharge_disposition_id) %in% c('3','4','5','6','8','12','15','10','14','16','17','22','23','24','30','27','28','29')]='2'
table(diab$discharge_disposition_id)


levels(diab$medical_specialty)
100*prop.table(table(diab$medical_specialty))

str(diab)
str(diab$num_medications)
# removing columns which are not required
diab$encounter_id = NULL
diab$patient_nbr = NULL
diab$weight = NULL
diab$payer_code = NULL
diab$medical_specialty = NULL
diab$citoglipton = NULL
diab$examide = NULL

ncol(diab)


str(diab)
cor(diab[8:13])

#Diagnosis 1
table(diab$diag_1)#Since It has too many Variable we will Group the variable
levels(diab$diag_1)[levels(diab$diag_1) %in% c(390:459, 785)] <- "Circulatory"
levels(diab$diag_1)[levels(diab$diag_1) %in% c(460:519, 786)] <- "Respiratory"
levels(diab$diag_1)[levels(diab$diag_1) %in% c(520:579, 787)] <- "Digestive"
levels(diab$diag_1)[levels(diab$diag_1) %in% c(seq.default(from = 250,to = 250.99,by =0.01))] <- "Diabetes"
levels(diab$diag_1)[levels(diab$diag_1) %in% c(800:999)] <- "Injury"
levels(diab$diag_1)[levels(diab$diag_1) %in% c(710:739)] <- "Musculoskeletal"
levels(diab$diag_1)[levels(diab$diag_1) %in% c(580:629,788)] <- "Genitourinary"
levels(diab$diag_1)[levels(diab$diag_1) %in% c(140:239,780,781,784,790:799,240:249,251:279,680:709,782,001:139)] <- "Neoplasms"
Defined=c("Circulatory","Respiratory","Digestive","Diabetes","Injury","Musculoskeletal","Genitourinary","Neoplasms")
levels(diab$diag_1)[!(levels(diab$diag_1) %in% Defined)] <- "Other"
table(diab$diag_1)#Grouped levels by ICD9 codes
#Diagnosis 2
table(diab$diag_2)#Since It has too many Variable we will Group the variable
levels(diab$diag_2)[levels(diab$diag_2) %in% c(390:459, 785)] <- "Circulatory"
levels(diab$diag_2)[levels(diab$diag_2) %in% c(460:519, 786)] <- "Respiratory"
levels(diab$diag_2)[levels(diab$diag_2) %in% c(520:579, 787)] <- "Digestive"
levels(diab$diag_2)[levels(diab$diag_2) %in% c(seq.default(from = 250,to = 250.99,by =0.01))] <- "Diabetes"
levels(diab$diag_2)[levels(diab$diag_2) %in% c(800:999)] <- "Injury"
levels(diab$diag_2)[levels(diab$diag_2) %in% c(710:739)] <- "Musculoskeletal"
levels(diab$diag_2)[levels(diab$diag_2) %in% c(580:629,788)] <- "Genitourinary"
levels(diab$diag_2)[levels(diab$diag_2) %in% c(140:239,780,781,784,790:799,240:249,251:279,680:709,782,001:139)] <- "Neoplasms"
Defined=c("Circulatory","Respiratory","Digestive","Diabetes","Injury","Musculoskeletal","Genitourinary","Neoplasms")
levels(diab$diag_2)[!(levels(diab$diag_2) %in% Defined)] <- "Other"
table(diab$diag_2)#Grouped levels by ICD9 codes
#Diagnosis 3
table(diab$diag_3)#Since It has too many Variable we will Group the variable
levels(diab$diag_3)[levels(diab$diag_3) %in% c(390:459, 785)] <- "Circulatory"
levels(diab$diag_3)[levels(diab$diag_3) %in% c(460:519, 786)] <- "Respiratory"
levels(diab$diag_3)[levels(diab$diag_3) %in% c(520:579, 787)] <- "Digestive"
levels(diab$diag_3)[levels(diab$diag_3) %in% c(seq.default(from = 250,to = 250.99,by =0.01))] <- "Diabetes"
levels(diab$diag_3)[levels(diab$diag_3) %in% c(800:999)] <- "Injury"
levels(diab$diag_3)[levels(diab$diag_3) %in% c(710:739)] <- "Musculoskeletal"
levels(diab$diag_3)[levels(diab$diag_3) %in% c(580:629,788)] <- "Genitourinary"
levels(diab$diag_3)[levels(diab$diag_3) %in% c(140:239,780,781,784,790:799,240:249,251:279,680:709,782,001:139)] <- "Neoplasms"
Defined=c("Circulatory","Respiratory","Digestive","Diabetes","Injury","Musculoskeletal","Genitourinary","Neoplasms")
levels(diab$diag_3)[!(levels(diab$diag_3) %in% Defined)] <- "Other"
table(diab$diag_3)#Grouped levels by ICD9 codes

# randomly shuffle the dataset
grp = runif(nrow(diab))
diab = diab[order(grp),]
View(diab)

ind = sample(seq_len(nrow(diab)), floor(nrow(diab)*0.7)   )
train = diab[ind,]
test = diab[-ind,]
colnames(train)
train$X = NULL
train$X.1 = NULL
train$X.2 = NULL
ncol(train)
nrow(train)
nrow(test)
head(train,3)
head(test,3)

# to check the count of each value of a factor variable against the Y-variable
# ----------------------------------------------------------------------------
100*prop.table(table(train$readmitted))
100*prop.table(table(test$readmitted))
100*prop.table(table(diab$readmitted))
train_x = train[,1:42]
colnames(train_x)
train_y = train[,43]
colnames(train_y)
levels(train_y)
colnames(train_x)
head(train_y)
rf1 = randomForest(train_x, factor(train_y))
rf1
summary(rf1)

test$X=NULL
test$X.1=NULL
test$X.2=NULL
# predict the Classification for the Testing data
# ------------------------------------------------
pdct_rf1 = predict(rf1, test)
table(predicted=pdct_rf1,actual=test$readmitted)
pdct_rf1
confusionMatrix(pdct_rf1,test$readmitted,positive='positive')


### Feature selection
# --------------------
# variables used by the randomforest()
varUsed(rf1, by.tree = F, count=F)

# importance of features/attributes.
# higher the value, more important it is
# used for feature selection to optimise other algorithm
# uses the MeanDecreaseGini
# -------------------------------------------------------------
importance(rf1)

# variable importance - for feature selection
# ----------------------------------------------
varImpPlot(rf1)
### </Feature selection>


# Model with imp variables
# -----------------------------------------
diab_new = diab
head(diab_new)
diab_new$X = NULL
diab_new$X.1 = NULL
diab_new$X.2 = NULL

diab_new$age = NULL
diab_new$time_in_hospital = NULL
diab_new$num_procedures = NULL
diab_new$number_inpatient = NULL
diab_new$diag_1 = NULL
diab_new$diag_2 = NULL
diab_new$diag_3 = NULL
diab_new$number_diagnoses = NULL
diab_new$max_glu_serum = NULL
diab_new$repaglinide = NULL
diab_new$chlorpropamide = NULL
diab_new$tolbutamide = NULL
diab_new$tolazamide = NULL
diab_new$insulin = NULL
diab_new$glipizide.metformin = NULL
diab_new$glimepiride.pioglitazone = NULL
diab_new$metformin.rosiglitazone = NULL
diab_new$metformin.pioglitazone = NULL

# randomly shuffle the dataset
grp = runif(nrow(diab_new))
diab_new = diab_new[order(grp),]
View(diab_new)

ind = sample(seq_len(nrow(diab_new)), floor(nrow(diab_new)*0.7)   )
train = diab_new[ind,]
test = diab_new[-ind,]
ncol(train)
nrow(train)
nrow(test)
head(train,3)
head(test,3)

# to check the count of each value of a factor variable against the Y-variable
# ----------------------------------------------------------------------------
100*prop.table(table(train$readmitted))
100*prop.table(table(test$readmitted))
100*prop.table(table(diab_new$readmitted))

train_x = train[,1:24]
colnames(train_x)
train_y = train[,25]
colnames(train_y)
levels(train_y)
colnames(train_x)
head(train_y)

rf2 = randomForest(train_x, factor(train_y))
rf2
summary(rf2)
colnames(test)

test$X=NULL
test$X.1=NULL
test$X.2=NULL

# predict the Classification for the Testing data
# ------------------------------------------------
pdct_rf2= predict(rf2,test)
table(predicted=pdct_rf2,actual=test$readmitted)
pdct_rf2
confusionMatrix(pdct_rf2,test$readmitted,positive='positive')

# ----------------------------
# The accuracy decreased to 55.45 from 57.87 after 
# building the model only with the important variables. 







