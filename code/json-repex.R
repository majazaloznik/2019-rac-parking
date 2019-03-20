## extracting json data for Wales test
library(jsonlite)
library(RJSONIO)

# prepare filters
filter1 <- "Column_ItemName_ENG"
filter1.value <- "Gross%20expenditure"
filter2 <- "Row_ItemName_ENG"
filter2.value <- "Parking%20of%20vehicles"

query <- paste0("http://open.statswales.gov.wales/en-gb/dataset/lgfs0009?$filter=",
                filter1, "%20eq%20%27", filter1.value, "%27%20and%20",
                filter2, "%20eq%20%27", filter2.value, "%27")

# test 1
test1 <- RJSONIO::fromJSON(query)
test1 <- test1[[2]]

test1 <- lapply(test1, function(x) 
{data.frame(matrix(unlist(x), ncol=20, byrow=T))})
test1 <- do.call(rbind, test1)

# test 2
test2 <- RJSONIO::fromJSON(query)
test2 <- test2[[2]]

test2 <- lapply(test2, function(x) 
  {data.frame(matrix(unlist(x), ncol=20, byrow=T))})
test2 <- do.call(rbind, test2)

# test 3
test3 <- jsonlite::fromJSON(query)
test3 <- test3[[2]]

# test 4
test4 <- jsonlite::fromJSON(query)
test4 <- test4[[2]]

# test 5
test5 <- jsonlite::fromJSON(query)
test5 <- test5[[2]]

# test 6
test6 <- jsonlite::fromJSON(query)
test6 <- test6[[2]]
# compare results
unique(test1$X15)
unique(test2$X15)
unique(test3$Year_Code)
unique(test4$Year_Code)
unique(test5$Year_Code)
unique(test6$Year_Code)


# ## here is what i get i.e. a different result (almost) every time
# > unique(test1$X15)
# [1] 200203 200304 200405 200506 200607 200708 200809 200910 201011 201112 201213 201314
# 12 Levels: 200203 200304 200405 200506 200607 200708 200809 200910 201011 ... 201314
# > unique(test2$X15)
# [1] 200203 200304 200405 200506 200607 200708 200809 200910 201011 201112 201213 201314
# [13] 201415 201516 201617
# 15 Levels: 200203 200304 200405 200506 200607 200708 200809 200910 201011 ... 201617
# > unique(test3$Year_Code)
# [1] "200203" "200304" "200405" "200506" "200607" "200708" "200809" "200910" "201011"
# [10] "201112" "201213" "201314" "201415" "201516" "201617" "201718"
# > unique(test4$Year_Code)
# [1] "200203" "200304" "200405" "200506" "200607" "200708" "200809" "200910" "201011"
# [10] "201112" "201213" "201314" "201415" "201516"
# > unique(test5$Year_Code)
# [1] "200203" "200304" "200405" "200506" "200607" "200708" "200809" "200910"
# > unique(test6$Year_Code)
# [1] "200203" "200304" "200405" "200506" "200607" "200708" "200809"