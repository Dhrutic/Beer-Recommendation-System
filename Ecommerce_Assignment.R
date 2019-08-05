library(recommenderlab)
library(dplyr)
library(ggplot2)


beer <-read.csv("beer_data.csv")
View(beer)
summary(beer)
colSums(is.na(beer))
str(beer)
table(beer$review_overall)

#Data Cleaning  
#removing records with no profile name
nrow(beer[beer$review_profilename == " ", ])
# 100


# removing duplicates
nrow(beer[duplicated(beer),])
--580

nrow(beer[!duplicated(beer),])
#475404 distinct rows




################Data preparation#######



finalreview <- beer[!duplicated(beer),]
View(finalreview)

highreviewers<-finalreview %>% group_by(review_profilename) %>% top_n(1)

##Choosing beer with review more than 100 and remove duplicates
chooseBeer<-beer %>% group_by(beer_beerid) %>% summarise(freq=n(), na.rm=T) %>% filter(freq > 100) %>% arrange(desc(freq))
View(chooseBeer)
##Beer id 2093 has the highest number of reviews

TotalbeerReviews<- ggplot(chooseBeer,aes(x=freq)) + geom_bar()+xlab("Total Beer Reviews")



#filtering the data
beer_reviews <- beer[beer$beer_beerid %in% chooseBeer$beer_beerid, ]


hr<- beer %>% group_by(review_profilename) %>% summarise(Frequencies=n()) %>% arrange(desc(Frequencies))
View(hr)
#22498 distinct reviewers of the beer

highestreviewer<- beer %>% group_by(review_profilename) %>% summarise(user_review_count=n()) %>% top_n(1)
#profile_name  reviewer count
northyorksammy 1846


reviewcount<-beer %>% group_by(review_overall) %>% summarise(freq=n(), na.rm=T) %>% filter(freq > 100) %>% arrange(desc(freq))
View(reviewcount)

#relevant reiviews
relevantReviewer<- finalreview %>% group_by(review_profilename) %>% summarise(Frequencies=n()) %>% arrange(desc(Frequencies))
View(relevantReviewer)
#18062 distinct reviewers reviews


##Considering Reviewers that have given more than 30 reviews
RfBeer<- finalreview %>% group_by(review_profilename) %>% summarise(freq=n(), na.rm=T) %>% filter(freq > 30) %>% arrange(desc(freq))
View(RfBeer)
##2006 relevant records


##User review
totalReviews<- ggplot(RfBeer,aes(x=freq)) + geom_bar()+xlab("Total User Reviews")


ImpBeer<-merge(beer,chooseBeer,by.x="beer_beerid",by.y="beer_beerid")
##221745View()

ImpBeerReview <- merge(ImpBeer,RfBeer,by.x="review_profilename",by.y="review_profilename")
##143312


ImpBR <- ImpBeerReview[,-c(4,5,6,7)]

ImpBR <- ImpBR[!ImpBR$review_profilename == "", ]
--50

brm  <-as(ImpBeerReview[,c(1,2,3)], "realRatingMatrix")
 head(rowCounts(brm))
#1759Girl   1fastz28   51mmz0rz  99bottles 9InchNails 
#49         33         66         32         54         49 

 head(colCounts(brm))
##5   6   7  10  17  19 
##80 163 129 160 148 104 
 
 brm_df<- as(brm,"data.frame")
 str(brm_df)
 'data.frame':	142514 obs. of  3 variables:
   user  : Factor w/ 2006 levels "","1759Girl",..: 1 1 1 1 1 1 1 1 1 1 ...
 item  : Factor w/ 1004 levels "10","100","1002",..: 969 19 85 147 153 278 414 601 602 640 ...
$ rating: num  4.5 3.5 5 4.5 4.5 4 4.5 3 4 4.5 ...

 
 
 brm1  <-as(ImpBR, "realRatingMatrix")
 brm_df1<- as(brm1,"data.frame")
 str(brm_df1)
 'data.frame':	220843 obs. of  3 variables:
   $ user  : Factor w/ 1004 levels "10","100","1002",..: 733 733 733 733 733 733 733 733 733 733 ...
 $ item  : Factor w/ 18062 levels "","0110x011",..: 223 705 952 1058 1127 1283 1308 1335 1399 1435 ...
 $ rating: num  4 3.5 4 4.5 4 3.5 3 4 3.5 3 ...
 
 userSimilarity <- similarity(brm[1:10, ],method = "cosine",which = "users")
 as.matrix(userSimilarity)
            1759Girl  1fastz28  51mmz0rz 99bottles 9InchNails    aaronh AaronHomoya  AaronRed    aasher
              0.0000000 0.9968966 0.9916478 0.9871421 0.9720384  0.9960406 0.9867553   0.9937978 0.9554095 0.9876234
 1759Girl    0.9968966 0.0000000 0.9971641 0.9727176 0.9797959  0.9952826 0.9977852   1.0000000 0.9938544 0.9899495
 1fastz28    0.9916478 0.9971641 0.0000000 0.9851532 0.9952991  0.9457307 0.9923883   0.9915922 0.9797997 0.9864199
 51mmz0rz    0.9871421 0.9727176 0.9851532 0.0000000 0.9986318  0.9766654 0.9755342   0.9861094 1.0000000 0.9986983
 99bottles   0.9720384 0.9797959 0.9952991 0.9986318 0.0000000  0.9847699 0.9801633   1.0000000 0.9873086 1.0000000
 9InchNails  0.9960406 0.9952826 0.9457307 0.9766654 0.9847699  0.0000000 0.9809645   0.9977852 0.9892306 0.9835296
 aaronh      0.9867553 0.9977852 0.9923883 0.9755342 0.9801633  0.9809645 0.0000000   0.9890283 0.9849311 0.9884471
 AaronHomoya 0.9937978 1.0000000 0.9915922 0.9861094 1.0000000  0.9977852 0.9890283   0.0000000 0.9844197 0.9777876
 AaronRed    0.9554095 0.9938544 0.9797997 1.0000000 0.9873086  0.9892306 0.9849311   0.9844197 0.0000000 0.9578795
 aasher      0.9876234 0.9899495 0.9864199 0.9986983 1.0000000  0.9835296 0.9884471   0.9777876 0.9578795 0.0000000
 
 

RfBeer<- finalreview %>% group_by(beer_beerid) %>% summarise(freq=n(), na.rm=T) %>% filter(freq > 30) %>% arrange(desc(freq))
View(RfBeer)
ggplot(RfBeer,aes(x=freq)) + geom_bar()


a<- ImpBR %>% arrange(desc(beer_beerid))
brm1 <- as(a[,c(2,3)], "realRatingMatrix")
beerSimilarity <- similarity(brm1[1:10, ],method = "cosine",which = "item")
as.matrix((beerSimilarity))
/*
5         6         7        10        17        19        30        31        33        34
5  0.0000000 0.9762410 0.9643010 0.9312075 0.9519402 0.9607689 0.9634705 0.9710609 0.9729230 0.9777217
6  0.9762410 0.0000000 0.9327072 0.9877854 0.9633901 0.9653313 0.9826260 0.9822110 0.9832512 0.9900385
7  0.9643010 0.9327072 0.0000000 0.9511921 0.9589941 0.9067985 0.9382791 0.9180353 0.9442337 0.9553853
10 0.9312075 0.9877854 0.9511921 0.0000000 0.9396008 0.9852062 0.9855751 0.9854066 0.9852036 0.9878820
17 0.9519402 0.9633901 0.9589941 0.9396008 0.0000000 0.9689810 0.9744582 0.9394791 0.9751149 0.9790218
19 0.9607689 0.9653313 0.9067985 0.9852062 0.9689810 0.0000000 0.9893860 0.9827195 0.9731191 0.9905967
30 0.9634705 0.9826260 0.9382791 0.9855751 0.9744582 0.9893860 0.0000000 0.9883339 0.9885702 0.9910076
31 0.9710609 0.9822110 0.9180353 0.9854066 0.9394791 0.9827195 0.9883339 0.0000000 0.9842275 0.9882085
33 0.9729230 0.9832512 0.9442337 0.9852036 0.9751149 0.9731191 0.9885702 0.9842275 0.0000000 0.9910738
34 0.9777217 0.9900385 0.9553853 0.9878820 0.9790218 0.9905967 0.9910076 0.9882085 0.9910738 0.0000000
*?
  

#--------------------------Understand users and ratings----------#

# Visualizing ratings

image(as.matrix(reviewSimilarity), main = "User similarity")


image(as.matrix(beerSimilarity), main = "Beer similarity")


brm_df %>% group_by(rating) %>% summarise(rating_frequency=n()) %>% nrow()
#26 different ratings


##Average Beer Rating
avg_beer_ratings<-brm_df %>% group_by(item) %>% summarise(average_rating=mean(rating)
View(avg_beer_ratings)

avgBR <- ggplot(avg_beer_ratings,aes(x=average_rating)) + geom_histogram() + labs(x="Average Rating", y="Number of Beers") + scale_x_discrete(limits=1:5)

summary(avg_beer_ratings$average_rating)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
1.766   3.729   3.947   3.870   4.108   4.775 

##Average Beer rating is 3.87 and is skewed tot the left

##Average Beer Rating
avg_rvr_ratings<-brm_df1eval %>% group_by(item) %>% summarise(average_rating=mean(rating))
View(avg_rvr_ratings)
                                                         
avgRVR <- ggplot(avg_rvr_ratings,aes(x=average_rating)) + geom_histogram() + labs(x="Average Rating", y="Number of Reviewer") + scale_x_discrete(limits=1:5)
                                                         
summary(avg_rvr_ratings$average_rating)
                                                     
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
1.000   3.750   4.000   3.972   4.333   6.750 

## Average user Rating is 3.9 

avg_beer_reviews<-ImpBeerReview %>% group_by(beer_beerid) %>% summarise(average_reviews=mean(freq.x))
ggplot(avg_beer_reviews,aes(x=average_reviews)) + geom_histogram() + labs(x="Average Beer Rating", y="Number of Beers")

summary(avg_beer_reviews$average_reviews)

##on an average each beer get 220.9 ratings





avg_user_reviews<-ImpBeerReview %>% group_by(review_profilename) %>% summarise(average_reviews=mean(freq.x))
ggplot(avg_user_reviews,aes(x=average_reviews)) + geom_histogram() + labs(x="Average user Rating", y="Number of User")


summary(avg_user_reviews$average_reviews)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
198.1   275.1   298.9   303.0   327.0   466.0 
##On an average each user gives 303 reviews

qplot(getRatings(brm), binwidth = 1, main = "Histogram of ratings", xlab = "Rating")

summary(getRatings(brm)) 
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
1.000   3.500   4.000   3.927   4.500  15.000 


qplot(getRatings(normalize(brm, method = "Z-score")), main = "Histogram of normalized ratings", xlab = "Rating") 
`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

summary(getRatings(normalize(rm, method = "Z-score")))
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-5.8580 -0.4980  0.0793  0.0000  0.6114  9.2196 


Eval <- evaluationScheme(brm, method = "split", train = .75,k = 1, given = -1, goodRating = 4)
 Eval
 
Evaluation scheme using all-but-1 items
Method: 'split' with 1 run(s).
Training set proportion: 0.750
Good ratings: >=4.000000
Data set: 2006 x 1004 rating matrix of class 'realRatingMatrix' with 142514 ratings.


Eval2 <- evaluationScheme(brm, method = "cross-validation",k = 5, given = -1, goodRating = 4)
Eval2
Evaluation scheme using all-but-1 items
Method: 'cross-validation' with 5 run(s).
Good ratings: >=4.000000
Data set: 2006 x 1004 rating matrix of class 'realRatingMatrix' with 142514 ratings.

Output <- evaluate(Eval, algorithms, n=c(1, 3, 5, 10, 15, 20))


Output2 <- evaluate(Eval2, algorithms, n=c(1, 3, 5, 10, 15, 20))

##ROC Curve

plot(Output, annotate = 1:4, legend="topleft")


plot(output2, annotate = 1:4, legend="topleft")


rb <- Recommender(brm, method = "UBCF")

##Beers recommended to differnet users
rb_cokes <- predict(rb, brm['cokes'], n=5)
as(rb_cokes, "list")
$cokes
#"19426" "571"   "1153"  "142"   "7971" 

rb_genog <- predict(rb, brm['genog'], n=5)
as(rb_genog, "list")
$genog
#"2751"  "2093"  "276"   "56973" "1717" 

rb_giblet <- predict(rb, brm['giblet'], n=5)
as(rb_giblet, "list")
$giblet
[1] "47151" "1545"  "571"   "48434" "4083"
