library(dplyr)

df <- read.csv("/Users/evanlindsay/Downloads/faker.csv")

# 1
for(i in 1:nrow(df)){
  if(df$jobTitle[i] == "Regional Metrics Producer"){
    print(df$jobArea[i])
  }
}

# Alternative Option for # 1

select(filter(df, jobTitle == 'Regional Metrics Producer'), jobArea)


#2

for(i in 1:nrow(df)){
  if(df$firstName[i] == 'Daphnee'){
    print(df$name[i])
  }
}
# 2 Alternative
select(filter(df, firstName == 'Daphnee'), name)

#3
for(i in 1:nrow(df)){
  if(df$jobTitle[i] == "District Infrastructure Administrator"){
    print(df$jobDescriptor[i] )
  }
}

#4
for(i in 1:nrow(df)){
  if(df$gender[i]== 'Androgyne'){
    print(df$firstName[i])
    print(df$lastName[i])
  }
}

# 4 Alternative

select(filter(df, gender == 'Androgyne'),firstName)

# Most Common Job Title
Most_Common_Job_Title <- sort(table(df$jobTitle), decreasing = TRUE)[1]
print(Most_Common_Job_Title)


# Distinct Company Names
n_distinct(df$name)


