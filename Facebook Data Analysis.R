#section 1: Data Cleaning
fbdata = read.csv("dataset_Facebook.csv",sep = ";")
str(fbdata)
#convert data types
for (col in names(fbdata)[2:7]){
  fbdata[,col] = as.factor(fbdata[,col])
}
str(fbdata)  

names(fbdata)[2:7]

#recode values 
fbdata$Category = as.character(fbdata$Category)
fbdata$Category[fbdata$Category == 1] = "action"
fbdata$Category[fbdata$Category == 2] = "product"
fbdata$Category[fbdata$Category == 3] = "inspiration"

#levels(fbdata$Category) = c("action","product","inspiration)

fbdata$Paid = as.character(fbdata$Paid)
fbdata$Paid[fbdata$Paid == 1] = "paid"
fbdata$Paid[fbdata$Paid == 0] = "non-paid"

# count NAs and remove them
sum(is.na(fbdata))
fbdata = na.omit(fbdata)
sum(is.na(fbdata))

#Section 2: Univariate analysis
#1. min/max, mean/medium, Q1 Q3, standard deviation 
#tells us how well the posts do or its performance on engaging the customers. 
#2. Plot
hist(fbdata$share,
      main = "Number of Shares",
      xlab = "Share",
      ylab = "Post",
      breaks = 100,
      xlim = c(0,200)
      )
# outliers after 100 shares
# most posts tend to fall 0-50 shares
# less posts get more shares
#3. Descriptive and Bar Plot (categorical)
#a, Posts by Month
plot(fbdata$Post.Month,
        main = "Posts by Month",
        ylab = "Number of Post",
        xlab = "Month")
#trend in posting: April, June, July, October, December
#b, Categories of Posts
fbdata$Category = as.factor(fbdata$Category)
plot(fbdata$Category,
     main = "Posts by Category",
     xlab = "Category",
     ylab = "Number of Post")
levels(fbdata$Category)
#c, Paid posts
fbdata$Paid = as.factor(fbdata$Paid)
plot(fbdata$Paid,
     main = "Paid Posts",
     xlab = "Paid or Non-Paid",
     ylab = "Number of Posts"
     )
#less than half of the posts were paid

#Section 3: Bivariate Analysis

#1.scatterplot of shares against likes
plot(fbdata$share,fbdata$like,xlim=c(0,200),ylim=c(0,2000))
#the relationship is direct; more likes, more shares

#2. Boxplot of shares by months
boxplot(share~Post.Month,data=fbdata,ylim = c(0,100))
#the posts that were posted in Feb, May, July and Sep, and Oct were among
#the ones that performed well and got the high shares. 

#3. Analyze the shares of paid post and non-paid posts
## table = xtabs(~share+Paid,data = fbdata)
xtabs(share~Paid, data=fbdata)
## ftable(table)
## aggregate(fbdata$share,by = list(fbdata$Paid), FUN = sum)
aggregate(share~Paid, data=fbdata, summary)
#more share in non-paid than paid posts. non-paid is 8978 shares, paid is
#4518 shares. The company should focus more on non-paid posts
g1 = fbdata$share[fbdata$Paid == "paid"]
g2 = fbdata$share[fbdata$Paid == "non-paid"]
t.test(g1,g2)
#not significant
#4. Multivariate analysis
par(mfrow = c(1,3))
x1 = fbdata$Paid[fbdata$Category == "action"]
y1 = fbdata$share[fbdata$Category == "action"]
plot(x1,y1,
     main = "Paid/non-paid by action")
x2 = fbdata$Paid[fbdata$Category == "product"]
y2 = fbdata$share[fbdata$Category == "product"]
plot(x2,y2,
     main = "Paid/non-paid by product")


x3 = fbdata$Paid[fbdata$Category == "inspiration"]
y3 = fbdata$share[fbdata$Category == "inspiration"]
plot(x3,y3,
     main = "Paid/non-paid by inspiration")

p1 = fbdata$share[fbdata$Paid == "paid" & fbdata$Category == "product"]
p2 = fbdata$share[fbdata$Paid == "non-paid" & fbdata$Category == "product"]
t.test(p1,p2)

#p value is 0.1487 > 0.1 => not significant. non-paid or paid don't make any
#difference. The marketing team shouldn't put too much money on paid posts
#and focus more on inspiration. 
