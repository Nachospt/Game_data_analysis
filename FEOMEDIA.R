## FeoMedia analysis

## Data extraction
file.choose()
DF.votes = read.csv("C:\\Users\\nacho\\Desktop\\Feomedia\\data\\votes.csv")
rbind(head(DF.votes), tail(DF.votes))

## Contingency table of NA
## Na values are converted into "1" value, any other value is converted into "0"
## (somehow crude tables but could be improved if necesary)
convert.0.nonNA=function(x){
  ifelse(!is.na(x),0,1)}
DF.votes.factor = data.frame(sapply(DF.votes, convert.0.nonNA))
DF.votes.factor = data.frame(DF.votes.factor[1-6], stringsAsFactors = TRUE)
table(DF.votes.factor)

## We check the number of rows with NA values and Discard them
row.has.na <- apply(DF.votes, 1, function(x){any(is.na(x))})
sum(row.has.na)
DF.votes1 <- DF.votes[!row.has.na,]

## Number of remaining rows
length(DF.votes1[[1]])

## Questions analysis: grouping by question
library(dplyr)
DF.Questions = summarise(DF.votes1)
by_question = group_by(DF.votes1, question_id)
DF.Questions = summarise(by_question, vote.avg = mean(vote))
rbind(head(DF.Questions), tail(DF.Questions))

## Vote ratings distribution graphic
library(ggplot2)
ggplot(DF.Questions, aes(x = vote.avg)) + geom_histogram()

## Max vote rating of the worst questions (bottom 1%, 2%, 5% and 10%)
Quantiles = c(.01, .02, .05, .10)
Q.Quant = quantile(DF.Questions$vote.avg, Quantiles)
Q.Quant

## Calculation of the samples aceppting 0.01% 0.02% 0.05% 0.1% proportion error.
Error = c(0.01,0.02,0.05,0.10)

## To use the corrected formula we calculate the proportion of the answers of those groups
## If this data was unknown, we would use 0.5 and obtain somewhat bigger samples

## Fist, we create the needed subsets
Sample.1 = DF.Questions[which(DF.Questions$vote.avg < Q.Quant[1]),]
Sample.2 = DF.Questions[which(DF.Questions$vote.avg < Q.Quant[2]),]
Sample.5 = DF.Questions[which(DF.Questions$vote.avg < Q.Quant[3]),]
Sample.10 = DF.Questions[which(DF.Questions$vote.avg < Q.Quant[4]),]

## Then, we calculate the proportion of votes in those 4 subsets
Ratios = c((mean(Sample.1$vote.avg)+1)/2, (mean(Sample.2$vote.avg)+1)/2, (mean(Sample.5$vote.avg)+1)/2, (mean(Sample.10$vote.avg)+1)/2)

## Finally we calculate the sample needed for any combination of error and Ratio.
## We create a table and fill it in each step of a loop
Results.Sample = matrix(rep(0,16),c(4,4))
rownames(Results.Sample) = interaction("Error", Error[1:4])
colnames(Results.Sample) = interaction("Removing", Quantiles[1:4],"%")
for (e in 1:4) {
  for (r in 1:4) {
    Last.Result = sample.size.prop(0.1, P = 0.5, N = Inf, level = 0.95)
    Results.Sample[e,r]=Last.Result$n
  }
}
Results.Sample