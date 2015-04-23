# Step 1: 
# Define the variables used throughout the project.
numbers <- 1:50
euro <- 1:10
price = 5
bucket_temp <- data.frame()
bucket <- data.frame()
temp <- data.frame()

# Step 2:
# Draw one million tickets. Embedding 1000 actions under the 1000 gave me better performance.
for (i in 1:1000) {
    for (j in 1:1000) {
        lotto_50 <- sort(sample(numbers, 5))
        lotto_10 <- sort(sample(euro, 2))
        lotto <- c(lotto_50, lotto_10)
        bucket_temp <- rbind(bucket_temp, lotto)
    }
    names(bucket_temp) <- c("one", "two", "three", "four", "five", "euro_1", "euro_2")
    bucket <- rbind(bucket, bucket_temp)
    names(bucket) <- c("one", "two", "three", "four", "five", "euro_1", "euro_2")
    bucket_temp <- data.frame()
    # Remove the comment character below to get updates how the script is progressing
    #print(nrow(bucket))
}

# Step 3:
# Draw the one Jackpot winner numbers of the week
actual_lotto <- sort(sample(numbers, 5))
actual_euro <- sort(sample(euro, 2))

# Step 4:
# Compare the million tickets to the one winner
for (i in nrow(temp)+1:nrow(bucket)) {
    temp1 <- 5 - length(setdiff(actual_lotto, bucket[i,1:5]))
    temp2 <- 2 - length(setdiff(actual_euro, bucket[i,6:7]))
    temp <- rbind(temp, cbind(temp1, temp2))
}
# Remove those tickets where there is not a single match at all.
temp <- temp[!(temp$temp1 == 0 & temp$temp2 == 0),]

# Step 5
# Assign the average prize for each winner ticket. Assign 0 to each ticket prize at first
temp$sum <- 0
# These lines contains all winning combinations with the associated average prize.
for (i in 1:nrow(temp)) {
    if (temp$temp1[i] == 1) {
        if (temp$temp2[i] == 2) {
            temp$sum[i] <- 9.75
    }} else if (temp$temp1[i] == 2) {
        if (temp$temp2[i] == 1) {
            temp$sum[i] <- 7.88
        } else if (temp$temp2[i] == 2) {
            temp$sum[i] <- 16.09
    }} else if (temp$temp1[i] == 3) {
        if (temp$temp2[i] == 1) {
            temp$sum[i] <- 20.3
        } else if (temp$temp2[i] == 2) {
            temp$sum[i] <- 54
        } else if (temp$temp2[i] == 0) {
            temp$sum[i] <- 14.23
    }} else if (temp$temp1[i] == 4) {
        if (temp$temp2[i] == 0) {
            temp$sum[i] <- 113.02
        } else if (temp$temp2[i] == 1) {
            temp$sum[i] <- 227.59
        } else if (temp$temp2[i] == 2) {
            temp$sum[i] <- 3770.26
    }} else if (temp$temp1[i] == 5) {
        if (temp$temp2[i] == 0) {
            temp$sum[i] <- 89148.29
        } else if (temp$temp2[i] == 1) {
            temp$sum[i] <- 582608.68
        } else if (temp$temp2[i] == 2) {
            temp$sum[i] <- 19823369.09
}}}

# Step 6:
# Get some statistics
# What sums have been won:
winner_sums <- sort(unique(temp$sum))
for (i in 1:length(winner_sums)) {
    print(paste0(winner_sums[i], " Euros won: ", length(grep(winner_sums[i], temp$sum))))
}
paste0("Not won anything: ", round((nrow(temp[temp$sum == 0,]) / nrow(bucket)) * 100, 2), "%")
paste0("I won: ", sum(temp$sum[temp$sum > 0]), " Euros with investing ", price * nrow(bucket) , " Euros")
paste0("The difference is: ", sum(temp$sum[temp$sum > 0]) - price * nrow(bucket), " Euros")

#Step 7:
# Calculate odds
for (i in 1:length(winner_sums)) {
    print(paste0("Odds to win ", winner_sums[i], " Euros: 1 in ",
                 round(100 / (nrow(temp[temp$sum == winner_sums[i],]) / nrow(bucket) * 100), 0)))
}
print(paste0("Odds to win any prize: 1 in ",
             round(100 / (length(temp$sum[temp$sum > 0]) / nrow(bucket) * 100), 0)))

#Step 8:
#Show distributions of the drawn lottery numbers
numbers_1_to_50 <- data.frame(numbers = c(bucket[,1], bucket[,2], bucket[,3], bucket[,4], bucket[,5]))
numbers_1_to_10 <- data.frame(numbers = c(bucket[,6], bucket[,7]))

library(ggplot2)
ggplot(numbers_1_to_50, aes(x = numbers)) + geom_bar(binwidth = 1, fill = "lightblue", colour = "darkblue") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_continuous(limits=c(1,50), breaks=seq(0, 50, 10))
ggplot(numbers_1_to_10, aes(x = numbers)) + geom_bar(binwidth = 1, fill = "lightblue", colour = "darkblue") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_continuous(limits=c(1,10), breaks=seq(0, 10, 1))
