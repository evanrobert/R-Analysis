library(dplyr)
library(ggplot2)
library(sf)
library(dplyr)
library(tmap)

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

# For Macthes that contain, not exact

count <- 0
for(i in 1: nrow(df)){
  if(grepl("Regional Metrics",df$jobTitle[i])){
    count <- count + 1
  }
}
print(count)

# Exact Match
count <- 0
for(i in 1: nrow(df)){
  if(df$jobTitle[i] == "Central Accounts Liaison"){
  count <- count + 1
  }
}
cat("Count:", count,"Name:" , df$name[i], "Descriptor:" ,df$jobDescriptor[i])


top_descriptors <- df %>%
  count(jobDescriptor, sort = TRUE) %>%
  slice_max(n, n = 5)  # Get the top 5



ggplot(top_descriptors, aes(x = reorder(jobDescriptor, -n), y = n, fill = jobDescriptor)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme(
    panel.background = element_rect(fill = "lightgrey", color = "black"),
    plot.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 12, color = "Red"),
    axis.title = element_text(size = 14, face = "italic")
  ) +
  labs(title = "Top 5 Most Popular Job Descriptors", x = "Job Descriptor", y = "Count")

top_Gender <- df %>%
  count(gender, sort = TRUE) %>%
  slice_max(n,n= 3)

ggplot(top_Gender, aes(x = reorder(gender, -n), y = n, fill = gender)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_minimal() +
  labs(
    title = "Top 3 Most Common Genders",
    x = "Genders",
    y = "Count"
  )

ggplot(top_Gender, aes(x = "", y = n, fill = gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_minimal() +
  labs(
    title = "Top 3 Most Common Genders",
    x = "",
    y = "Count"
  ) +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())




df$Contains_p <- grepl('p', df$firstName, fixed = TRUE)

df$HasJobTitle <- grepl('Regional|Human', df$jobTitle, fixed = FALSE)


for(i in 1:nrow(df)){
  if(df$jobTitle[i] == 'Regional Metrics Producer'){
    print(df$jobTitle[i])
    print(df$firstName[i])

  }
}











