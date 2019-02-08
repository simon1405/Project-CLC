

# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
getword<-function(docs){
docs<-gsub("[\r\n]", "", as.String(docs))
docs<-Corpus(VectorSource(as.character(docs)))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
return(head(d, 20))
}
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

dim(data)
l<-""
for (i in 1:179)
{l<-paste(l,data$Comments__5[i],sep=" ")}
l
docs<-gsub("[\r\n]", "", as.String(l))

table(data$`Gaining Commitment`)


###frequency of table
dim(Dataset_CLCP_180reports_1221)
tabl<-matrix(0,nrow=5,ncol=22)
data<-Dataset_CLCP_180reports_1221[,4:47]
for (i in 1:22){
  for (j in 1:179){
      
    o<-as.double(data[j,i*2-1])
    if (is.na(o)==FALSE){
    tabl[o,i]<-tabl[o,i]+1
   }
  }
}
install.packages("xlsx")
library("xlsx")
write.xlsx(tabl, "c:/freqtable.xlsx")
write.table(tabl,"freqtable.txt")
###wowrking on the slides12

text3<-"During Diane's call today with Dr. Mezzo Diane provided a solid example of Gaining Commitment on a potential Switch opportunity. Diane had identified that Mezzo is a Simplicity Seeker and wanted to focus on Convenience through Safety - as a key lever to pull regarding this type of HCP. Through her discussion today - Diane was able to get Mezzo do discuss an anonymous patient currently on an Oral A - and having side effects. Through her growing development around Patient ID - Diane as able to discuss our product as a potential solution. At the end of the call - in Diane's personal way was able to Gain Commitment that Mezzo would discuss our product during his next consult with this patient. Great example of having compliant conversations about real time patients that may be a candidate for Rebif.Joe has been demonstrating an exceptional skill set in leveraging his relationships within given accounts to drive SRF's. Today as part of Joe's pre call plan he wanted to discuss a patient of Ed's that another HCP told Joe he saw last week that was having issues getting branded GA. Joe wanted to discuss positioning our product if managed care did not allow said patient to go on this product. Joe gained commitment from Ed that if in fact the patient could not access this dmt - that his go to would be our product. Great job in having an actionable intent or close built in your pre call plan and actually being able to execute on it. Well done Joe.
"
text4<-"The objective overall for the field ride was to observe “gaining commitment” and I would say 3.5 is what I observed. You consistently closed for action depending on the objective and call continuum situation. For example, during your lunch with Dr. Blackwell, after going through the call continuum and painting the picture of a patient type that might be considered for wasting, you closed with Dr. Blackwell by asking if he has seen any patients recently that fit the profile. Though he could not think of any patients, he did commit to looking out for this specific patient type moving forward. Obviously by asking for commitment, you have called Dr. Blackwell to action which should lead to future wasting patient identification.As observed during your adult learning principles training to the team at the POA on the critical component of closing and gaining commitment, you stated that if you do not close on action, it will not happen by itself. That said, after going through the call continuum and painting the picture of a patient type that might be wasting, you closed with Dr. Adams asking if he has seen any patients recently that fit the profile. Though he could not think of any patients, he did commit to looking out for this specific patient type moving forward. Obviously by asking for commitment, you have called Dr. Adams to action which should lead to future wasting patient identification.
"
text5<-"The objective for this field ride was to observe gaining commitment. Though the field ride had limited customer facings, you did get good time at a lunch with the entire staff, case management and Dr. Adams at AIDS Project of the Osarks. After going through the call continuum and painting the picture of a patient type that might be wasting, you closed with Dr. Adams asking if he has seen any patients recently that fit the profile. He did commit to a possible patient and you did a nice job following up with the staff to get this patient submitted. Obviously by asking for commitment, you have called Dr. Adams to action which lead to a successful patient identification but also left him with something to look for for future wasting patient identification as well.
"
d3<-getword(text3)
d4<-getword(text4)
d5<-getword(text5)

c<-data.frame(rep(0,20))
d<-data.frame(rep(0,20),rep(0,20))
paste(text3,text4,sep=" ")
out<-c()
saved.comments<-" "
for (i in 1:22){
  saved.comments<-" "
  for (j in 1:179){
    if (is.na(data[j,i*2-1])==F){
    if (data[j,i*2-1]==5){
      saved.comments<-paste(saved.comments, data[j,i*2], sep=" ")
    }
    }
  }
 
 out<-getword(saved.comments)
 if (dim(out)[1]==0) {c<-cbind(c,d)}
 else {colnames(out)[1]<-colnames(data)[i*2-1]
   c<-cbind(c,out)
   }
 #print(dim(out))
 
 }
c
write.table(c,"outputrating5.txt")
table1<-getword(saved.comments)
cbind(table1,table1)


###Taking info from UPACA
getword<-function(docs){
  docs<-gsub("[\r\n]", "", as.String(docs))
  docs<-Corpus(VectorSource(as.character(docs)))
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, stripWhitespace)
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  return(head(d, 40))
}
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

upaca<-Dataset_CLCP_180reports_1227Upaca_RateFreq
dim(upaca)
c<-data.frame(rep(0,40))
d<-data.frame(rep(0,40),rep(0,40))
save.upaca<-""
 for (i in 1:8){
   save.upaca<-""
  for (j in 1:326){
    if (is.na(upaca[j,i*2-1])==F){
      if (upaca[j,i*2-1]==5){
        save.upaca<-paste(save.upaca, upaca[j,i*2], sep=" ")
      }
    }
   }
   out<-getword(save.upaca)
   if (dim(out)[1]==0) {c<-cbind(c,d)}
   else {colnames(out)[1]<-colnames(data)[i*2-1]
   c<-cbind(c,out)}
}

write.table(c,"upacarating5.txt")

#sort by Rating only
scores<-c(rep(0,179))
comments<-c()
k<-0
for (i in 1:8){
  for (j in 1:179){
    if (is.na(Dataset_CLCP_180reports_1227Upaca_RateFreq[j,i*2-1])==FALSE){
      k<-k+1
      docs<-""
      scores[k]<-Dataset_CLCP_180reports_1227Upaca_RateFreq[j,i*2-1]
      docs<-Dataset_CLCP_180reports_1227Upaca_RateFreq[j,i*2]
      docs<-gsub("[\r\n]", " ", as.String(docs))
      docs<-Corpus(VectorSource(as.character(docs)))
      docs <- tm_map(docs, content_transformer(tolower))
      docs <- tm_map(docs, removePunctuation)
      docs <- tm_map(docs, removeWords, stopwords("english"))
      docs <- tm_map(docs, stripWhitespace)
      comments[k]<-docs
    }
  }
}

outfile<-as.data.frame(cbind(scores,as.String(comments)))
outfile$scores<-as.numeric(outfile$scores)
outfile$comments<-as.String(outfile$comments)
write.table(comments,"outcomments.txt")
write.csv(outfile,"outcsv.csv")
View(outfile)
View(as.String(comments))
score<-as.matrix(scores,ncol=1)
write(as.character(comments),"comments.csv")
write(score,ncolumns = 1,"scores.csv")

class(comments[1])

###
comment<-as.character(comments)

corpus <- VCorpus(VectorSource(comment))
tdm <- DocumentTermMatrix(corpus, list(removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))
dim(as.matrix(tdm))

install.packages("stringr")
install.packages("caret")
library(stringr)
library(caret)
install.packages("e1071")
library(e1071)
wordrf <- train(x = as.matrix(tdm),
                     y = rate,
                     method = "nb",
                trControl=trainControl(method='cv',number=10))

install.packages("keras")
library(keras)
install_keras()
summary(wordrf)
rate<-c()
rate[which(score==2)]<-"soso"
rate[which(score==3)]<-"good"
rate[which(score==4)]<-"exce"
rate[which(score==5)]<-"perf"

predict(wordrf$finalModel,as.matrix(tdm))

worrf_20<- train(x = as.matrix(tdm),
                y = rate,
                method = "rf",
               ntree = 20,
               trControl = trainControl(method = "oob"))
table(predict(worrf$finalModel,as.matrix(tdm)),rate)
varImpPlot(worrf$finalModel)

worrf_20
