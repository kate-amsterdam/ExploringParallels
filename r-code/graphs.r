  ##total barbies per majors 
  total_barbies_per_major <- total_barbies_per_major %>%
  mutate(majors = ifelse(majors == "NULL", "No Matching Major", majors))%>%
  mutate(majors = ifelse(majors == "Rock Star", "Visual and Performing Arts", majors))
  


    b <- ggplot(total_barbies_per_major, aes(x = reorder(majors, -sum), y = sum)) +
    geom_bar(stat = "identity", fill = "deeppink") +
    labs(title = "Barbie Per Major", x = "Majors", y = "Total Dolls", caption = "Data from 1959-2023") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(panel.grid = element_blank()) +  # Remove grid lines
    coord_flip()
  
  print(b)

  
## total women per majors
  options(scipen = 5)
 
 women_per_degree <- total_women_per_degree_ever %>%
   rename(total_women = sum) %>%
   mutate(total_women = total_women/1000)%>%
   mutate(total_women_formatted = format(total_women, scientific = FALSE),
           degree = factor(degree, levels = unique(degree[order(-total_women)])))
 
 w <- ggplot(women_per_degree, aes(x = degree, y = total_women)) +
   geom_bar(stat = "identity", fill = "darkolivegreen4") +
   labs(title = "Women per Major", x = "Majors", y = "Total Women (thousands)", caption = "Data from 2011-2022") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   theme(panel.grid = element_blank()) +
   coord_flip()+
   scale_y_continuous(expand = expansion(mult = c(0.2, 0.2)))
 
 print(w)
 
 ggsave("women_per_degree.png", w, width = 10, height = 6, dpi = 300)
 
 #### women vs barbie percent dif

## add percent sign and negative numbers to graph
 
 install.packages("scales")
 install.packages("ggplot2")
 library(ggplot2)
 library(scales)
 
 # Convert majors back to a factor

 library(dplyr)
### percent dif
 barbie_vs_women_percentages <- barbie_vs_women_percentages %>%
   mutate(integer_column = as.integer(percent_difference))

 
 pd <- ggplot(barbie_vs_women_percentages, aes(x=reorder(majors, -integer_column), y= integer_column))+
   geom_bar(stat = "identity", fill = "deeppink")+
   labs(title= "Barbie's Representation Disparity Across Women's Majors", x="Majors", y= "Percent Difference", caption = "Positive = Under Representation\nNegative = Over Representation")+
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   theme(plot.title = element_text(hjust = 1.0)) +
   theme(panel.grid = element_blank()) +
  coord_flip()+
  scale_y_continuous(limits = c(-10, 20))
 print(pd)
 
 
 
 ###barbie top careers

 library(ggplot2)
 subset_data <- head(barbiecareers11, 10)
 
 # Create a new data frame with only the relevant columns
 new_data <- data.frame(career = subset_data$career, doll_count = subset_data$doll_count)
 # View the new data
 print(new_data)
 #select first ten rows
 subset_data <- head(barbiecareers11,10)
 new_data <- data.frame(career = subset_data$career, dollcount = subset_data$doll_count)
 view(new_data)
 # Define ggplot object
 bc <- ggplot(new_data, aes(x =career, y = dollcount)) +
   geom_bar(stat = "identity", fill = "deeppink") +
   labs(title = "Barbie's Top Careers", x = "Career Name", y = "Doll Count", caption = "Barbies released between 1959-2023") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   theme(plot.title = element_text(hjust = .5)) +
 
 
 # View the data

 
 # Print the ggplot object
 print(bc)
 
 bc <- ggplot(new_data, aes(x = career, y = dollcount)) +
   geom_bar(stat = "identity", fill = "deeppink") +
   labs(title = "Barbie's Top Careers", x = "Career Name", y = "Doll Count", caption = "Barbies released between 1959-2023") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   theme(plot.title = element_text(hjust = .5))
 
 # Print the ggplot object
 print(bc)
 