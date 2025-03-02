##Data wrangling and visualization using dplyr, ggplot2 and plotly packages for interactive plots##
##Dataset used: https://www.kaggle.com/datasets/dillonmyrick/bike-store-sample-database/data##

#load libraries
library(dplyr) #for data manipulation
library(ggplot2) #for 'static' plots
library(plotly) #for interactive plots
library(patchwork) #to arrange plots
library(stringr) #can detect presence/absence of a match; used together with dplyr
library(purrr) #to apply distinct on a list (sum of all dfs)

#set working directory and load necessary input files
setwd('NA')

data_order_items <- read.csv('./order_items.csv')
data_orders <- read.csv('./orders.csv')
data_products <- read.csv('./products.csv')
data_staffs <- read.csv('./staffs.csv')
data_categories <- read.csv('./categories.csv')

#adjust shipped_date column to get rid of former NULL values as NAs via !is.na()
data_orders$shipped_date <- as.Date(data_orders$shipped_date)

#data wrangling to acquire the sales per quartile per year
#########################################################
######################Q1-2016############################
data_sales_2016_q1 <- data_order_items %>%
                      group_by(order_id) %>%
                      mutate(profit = (list_price*quantity)*(1-discount))%>% #add profit made based on named parameter
                      mutate(profit_lost_by_discounts = (list_price*quantity)-(list_price*quantity)*(1-discount)) %>% #add profit lost due to discounts
                      ungroup() %>%
                      dplyr::inner_join(data_orders, by = "order_id") %>%
                      dplyr::inner_join(data_products, by = c("product_id", "list_price")) %>%
                      dplyr::filter(order_date <= '2016-03-31', !is.na(shipped_date)) %>%
  mutate(Quartile = case_when( #add quartile based on order_date
    str_detect(order_date, "-(01|02|03)-") ~ "Q1",  
    str_detect(order_date, "-(04|05|06)-") ~ "Q2",  
    str_detect(order_date, "-(07|08|09)-") ~ "Q3",  
    str_detect(order_date, "-(10|11|12)-") ~ "Q4",
    TRUE ~ "NA")) %>%
  mutate(Year = case_when( #add year based on year of order_date
    str_detect(order_date, "2016") ~"2016",
    str_detect(order_date, "2017") ~"2017",
    str_detect(order_date, "2018") ~"2018", #use package "stringr" for str_detect()
    TRUE ~ "NA"))%>%
  select(c(Quartile, Year, product_id, product_name, order_date, shipped_date, quantity, list_price, discount, profit_lost_by_discounts, profit))
data_sales_2016_q1

q1_2016 <- data_sales_2016_q1 %>% #add sum of newly introduced columns
           mutate(total_profit_lost_by_discounts = sum(profit_lost_by_discounts, na.rm = TRUE)) %>%
           mutate(total_profit = sum(profit, na.rm = TRUE))
q1_2016
#########################################################
######################Q2-2016############################
data_sales_2016_q2 <- data_order_items %>%
                      group_by(order_id) %>%
                      mutate(profit = (list_price*quantity)*(1-discount))%>%
                      mutate(profit_lost_by_discounts = (list_price*quantity)-(list_price*quantity)*(1-discount)) %>%
                      ungroup() %>%
                      dplyr::inner_join(data_orders, by = "order_id") %>%
                      dplyr::inner_join(data_products, by = c("product_id", "list_price")) %>%
                      dplyr::filter(between(order_date, "2016-04-01", "2016-06-30"),!is.na(shipped_date)) %>%
  mutate(Quartile = case_when(
    str_detect(order_date, "-(01|02|03)-") ~ "Q1",  
    str_detect(order_date, "-(04|05|06)-") ~ "Q2",  
    str_detect(order_date, "-(07|08|09)-") ~ "Q3",  
    str_detect(order_date, "-(10|11|12)-") ~ "Q4",
    TRUE ~ "NA")) %>%
  mutate(Year = case_when(
    str_detect(order_date, "2016") ~"2016",
    str_detect(order_date, "2017") ~"2017",
    str_detect(order_date, "2018") ~"2018",#use package "stringr" for str_detect()
    TRUE ~ "NA"))%>%
  select(c(Quartile, Year, product_id, product_name, order_date, shipped_date, quantity, list_price, discount, profit_lost_by_discounts, profit))
data_sales_2016_q2

q2_2016 <- data_sales_2016_q2 %>%
           mutate(total_profit_lost_by_discounts = sum(profit_lost_by_discounts, na.rm = TRUE)) %>%
           mutate(total_profit = sum(profit, na.rm = TRUE))
q2_2016
#########################################################
######################Q3-2016############################   
data_sales_2016_q3 <- data_order_items %>%
  group_by(order_id) %>%
  mutate(profit = (list_price*quantity)*(1-discount))%>%
  mutate(profit_lost_by_discounts = (list_price*quantity)-(list_price*quantity)*(1-discount)) %>%
  ungroup() %>%
  dplyr::inner_join(data_orders, by = "order_id") %>%
  dplyr::inner_join(data_products, by = c("product_id", "list_price")) %>%
  dplyr::filter(between(order_date, "2016-07-01", "2016-09-30"),!is.na(shipped_date)) %>%
  mutate(Quartile = case_when(
    str_detect(order_date, "-(01|02|03)-") ~ "Q1",  
    str_detect(order_date, "-(04|05|06)-") ~ "Q2",  
    str_detect(order_date, "-(07|08|09)-") ~ "Q3",  
    str_detect(order_date, "-(10|11|12)-") ~ "Q4",
    TRUE ~ "NA")) %>%
  mutate(Year = case_when(
    str_detect(order_date, "2016") ~"2016",
    str_detect(order_date, "2017") ~"2017",
    str_detect(order_date, "2018") ~"2018",#use package "stringr" for str_detect()
    TRUE ~ "NA"))%>%
  select(c(Quartile, Year, product_id, product_name, order_date, shipped_date, quantity, list_price, discount, profit_lost_by_discounts, profit))
data_sales_2016_q3

q3_2016 <- data_sales_2016_q3 %>%
  mutate(total_profit_lost_by_discounts = sum(profit_lost_by_discounts, na.rm = TRUE)) %>%
  mutate(total_profit = sum(profit, na.rm = TRUE))
q3_2016
#########################################################
######################Q4-2016############################
data_sales_2016_q4 <- data_order_items %>%
  group_by(order_id) %>%
  mutate(profit = (list_price*quantity)*(1-discount))%>%
  mutate(profit_lost_by_discounts = (list_price*quantity)-(list_price*quantity)*(1-discount)) %>%
  ungroup() %>%
  dplyr::inner_join(data_orders, by = "order_id") %>%
  dplyr::inner_join(data_products, by = c("product_id", "list_price")) %>%
  dplyr::filter(between(order_date, "2016-10-01", "2016-12-31"),!is.na(shipped_date)) %>%
  mutate(Quartile = case_when(
    str_detect(order_date, "-(01|02|03)-") ~ "Q1",  
    str_detect(order_date, "-(04|05|06)-") ~ "Q2",  
    str_detect(order_date, "-(07|08|09)-") ~ "Q3",  
    str_detect(order_date, "-(10|11|12)-") ~ "Q4",
    TRUE ~ "NA")) %>%
  mutate(Year = case_when(
    str_detect(order_date, "2016") ~"2016",
    str_detect(order_date, "2017") ~"2017",
    str_detect(order_date, "2018") ~"2018",#use package "stringr" for str_detect()
    TRUE ~ "NA"))%>%
  select(c(Quartile, Year, product_id, product_name, order_date, shipped_date, quantity, list_price, discount, profit_lost_by_discounts, profit))
data_sales_2016_q4

q4_2016 <- data_sales_2016_q4 %>%
  mutate(total_profit_lost_by_discounts = sum(profit_lost_by_discounts, na.rm = TRUE)) %>%
  mutate(total_profit = sum(profit, na.rm = TRUE))
q4_2016


#########################################################
######################Q1-2017############################
data_sales_2017_q1 <- data_order_items %>%
  group_by(order_id) %>%
  mutate(profit = (list_price*quantity)*(1-discount))%>%
  mutate(profit_lost_by_discounts = (list_price*quantity)-(list_price*quantity)*(1-discount)) %>%
  ungroup() %>%
  dplyr::inner_join(data_orders, by = "order_id") %>%
  dplyr::inner_join(data_products, by = c("product_id", "list_price")) %>%
  dplyr::filter(between(order_date, "2017-01-01", "2017-03-31"),!is.na(shipped_date)) %>%
  mutate(Quartile = case_when(
    str_detect(order_date, "-(01|02|03)-") ~ "Q1",  
    str_detect(order_date, "-(04|05|06)-") ~ "Q2",  
    str_detect(order_date, "-(07|08|09)-") ~ "Q3",  
    str_detect(order_date, "-(10|11|12)-") ~ "Q4",
    TRUE ~ "NA")) %>%
  mutate(Year = case_when(
    str_detect(order_date, "2016") ~"2016",
    str_detect(order_date, "2017") ~"2017",
    str_detect(order_date, "2018") ~"2018",#use package "stringr" for str_detect()
    TRUE ~ "NA"))%>%
  select(c(Quartile, Year, product_id, product_name, order_date, shipped_date, quantity, list_price, discount, profit_lost_by_discounts, profit))
data_sales_2017_q1

q1_2017 <- data_sales_2017_q1 %>%
  mutate(total_profit_lost_by_discounts = sum(profit_lost_by_discounts, na.rm = TRUE)) %>%
  mutate(total_profit = sum(profit, na.rm = TRUE))
q1_2017
#########################################################
######################Q2-2017############################
data_sales_2017_q2 <- data_order_items %>%
  group_by(order_id) %>%
  mutate(profit = (list_price*quantity)*(1-discount))%>%
  mutate(profit_lost_by_discounts = (list_price*quantity)-(list_price*quantity)*(1-discount)) %>%
  ungroup() %>%
  dplyr::inner_join(data_orders, by = "order_id") %>%
  dplyr::inner_join(data_products, by = c("product_id", "list_price")) %>%
  dplyr::filter(between(order_date, "2017-04-01", "2017-06-30"),!is.na(shipped_date)) %>%
  mutate(Quartile = case_when(
    str_detect(order_date, "-(01|02|03)-") ~ "Q1",  
    str_detect(order_date, "-(04|05|06)-") ~ "Q2",  
    str_detect(order_date, "-(07|08|09)-") ~ "Q3",  
    str_detect(order_date, "-(10|11|12)-") ~ "Q4",
    TRUE ~ "NA")) %>%
  mutate(Year = case_when(
    str_detect(order_date, "2016") ~"2016",
    str_detect(order_date, "2017") ~"2017",
    str_detect(order_date, "2018") ~"2018",#use package "stringr" for str_detect()
    TRUE ~ "NA"))%>%
  select(c(Quartile, Year, product_id, product_name, order_date, shipped_date, quantity, list_price, discount, profit_lost_by_discounts, profit))
data_sales_2017_q2

q2_2017 <- data_sales_2017_q2 %>%
  mutate(total_profit_lost_by_discounts = sum(profit_lost_by_discounts, na.rm = TRUE)) %>%
  mutate(total_profit = sum(profit, na.rm = TRUE))
q2_2017
#########################################################
######################Q3-2017############################   
data_sales_2017_q3 <- data_order_items %>%
  group_by(order_id) %>%
  mutate(profit = (list_price*quantity)*(1-discount))%>%
  mutate(profit_lost_by_discounts = (list_price*quantity)-(list_price*quantity)*(1-discount)) %>%
  ungroup() %>%
  dplyr::inner_join(data_orders, by = "order_id") %>%
  dplyr::inner_join(data_products, by = c("product_id", "list_price")) %>%
  dplyr::filter(between(order_date, "2017-07-01", "2017-09-30"),!is.na(shipped_date)) %>%
  mutate(Quartile = case_when(
    str_detect(order_date, "-(01|02|03)-") ~ "Q1",  
    str_detect(order_date, "-(04|05|06)-") ~ "Q2",  
    str_detect(order_date, "-(07|08|09)-") ~ "Q3",  
    str_detect(order_date, "-(10|11|12)-") ~ "Q4",
    TRUE ~ "NA")) %>%
  mutate(Year = case_when(
    str_detect(order_date, "2016") ~"2016",
    str_detect(order_date, "2017") ~"2017",
    str_detect(order_date, "2018") ~"2018",#use package "stringr" for str_detect()
    TRUE ~ "NA"))%>%
  select(c(Quartile, Year, product_id, product_name, order_date, shipped_date, quantity, list_price, discount, profit_lost_by_discounts, profit))
data_sales_2017_q3

q3_2017 <- data_sales_2017_q3 %>%
  mutate(total_profit_lost_by_discounts = sum(profit_lost_by_discounts, na.rm = TRUE)) %>%
  mutate(total_profit = sum(profit, na.rm = TRUE))
q3_2017
#########################################################
######################Q4-2017############################
data_sales_2017_q4 <- data_order_items %>%
  group_by(order_id) %>%
  mutate(profit = (list_price*quantity)*(1-discount))%>%
  mutate(profit_lost_by_discounts = (list_price*quantity)-(list_price*quantity)*(1-discount)) %>%
  ungroup() %>%
  dplyr::inner_join(data_orders, by = "order_id") %>%
  dplyr::inner_join(data_products, by = c("product_id", "list_price")) %>%
  dplyr::filter(between(order_date, "2017-10-01", "2017-12-31"),!is.na(shipped_date)) %>%
  mutate(Quartile = case_when(
    str_detect(order_date, "-(01|02|03)-") ~ "Q1",  
    str_detect(order_date, "-(04|05|06)-") ~ "Q2",  
    str_detect(order_date, "-(07|08|09)-") ~ "Q3",  
    str_detect(order_date, "-(10|11|12)-") ~ "Q4",
    TRUE ~ "NA")) %>%
  mutate(Year = case_when(
    str_detect(order_date, "2016") ~"2016",
    str_detect(order_date, "2017") ~"2017",
    str_detect(order_date, "2018") ~"2018",#use package "stringr" for str_detect()
    TRUE ~ "NA"))%>%
  select(c(Quartile, Year, product_id, product_name, order_date, shipped_date, quantity, list_price, discount, profit_lost_by_discounts, profit))
data_sales_2017_q4

q4_2017 <- data_sales_2017_q4 %>%
  mutate(total_profit_lost_by_discounts = sum(profit_lost_by_discounts, na.rm = TRUE)) %>%
  mutate(total_profit = sum(profit, na.rm = TRUE))
q4_2017



#########################################################
######################Q1-2018############################
data_sales_2018_q1 <- data_order_items %>%
  group_by(order_id) %>%
  mutate(profit = (list_price*quantity)*(1-discount))%>%
  mutate(profit_lost_by_discounts = (list_price*quantity)-(list_price*quantity)*(1-discount)) %>%
  ungroup() %>%
  dplyr::inner_join(data_orders, by = "order_id") %>%
  dplyr::inner_join(data_products, by = c("product_id", "list_price")) %>%
  dplyr::filter(between(order_date, "2018-01-01", "2018-03-31"),!is.na(shipped_date)) %>%
  mutate(Quartile = case_when(
      str_detect(order_date, "-(01|02|03)-") ~ "Q1",  
      str_detect(order_date, "-(04|05|06)-") ~ "Q2",  
      str_detect(order_date, "-(07|08|09)-") ~ "Q3",  
      str_detect(order_date, "-(10|11|12)-") ~ "Q4",
      TRUE ~ "NA")) %>%
  mutate(Year = case_when(
      str_detect(order_date, "2016") ~"2016",
      str_detect(order_date, "2017") ~"2017",
      str_detect(order_date, "2018") ~"2018", #use package "stringr" for str_detect()
      TRUE ~ "NA"))%>%
  select(c(Quartile, Year, product_id, product_name, order_date, shipped_date, quantity, list_price, discount, profit_lost_by_discounts, profit))
data_sales_2018_q1

q1_2018 <- data_sales_2018_q1 %>%
  mutate(total_profit_lost_by_discounts = sum(profit_lost_by_discounts, na.rm = TRUE)) %>%
  mutate(total_profit = sum(profit, na.rm = TRUE))
q1_2018

###modify dfs to lists and apply distinct function for interesting parameters###
list_all_quartiles <- list(q1_2016,q2_2016, q3_2016, q4_2016,
                           q1_2017,q2_2017, q3_2017, q4_2017,
                           q1_2018)

distinct_list_quartiles <- map(list_all_quartiles, ~ distinct(.x, Quartile, Year, total_profit, total_profit_lost_by_discounts)) #use package purrr to apply distinct on a list of all quartiles
distinct_list_quartiles

distinct_df_all <- data.table::rbindlist(distinct_list_quartiles) #use package data.table to rbind() the whole list to a data frame
distinct_df_all
##############################################
#####visualize total profit using ggplot2#####
distinct_df_all$Quartile <- factor(distinct_df_all$Quartile, levels = c("Q4", "Q3", "Q2", "Q1")) #change order to make it more intuitive

plot_total_profit<- ggplot(distinct_df_all, aes(x=Year, y= total_profit, fill=Quartile))+
  scale_fill_manual(values= c("#219ebc", "#023047", "#ffb703", "#fb8500"))+
  geom_bar(stat="identity")+
  theme_light()+
  scale_y_continuous(breaks = c(0,500000, 1000000, 1500000, 2000000, 2500000, 3000000, 3500000),labels = scales::number_format(accuracy = 1))+
  ylab("Total profit (USD)")+
  geom_label(
    color = "white",
    label = sprintf('%0.2f', distinct_df_all$total_profit), size=4, #adds numbers of total_profit with two decimal places ('%0.2f')
    show.legend = F,
    position = position_stack(vjust = 0.5))+
  theme( #some cosmetics
      axis.text.x = element_text(color = 'black', size = 12), 
      axis.text.y = element_text(color = 'black', size = 12),
      axis.title.x = element_text(color = 'black', size = 15),
      axis.title.y = element_text(color = 'black', size = 15),
      axis.ticks = element_line(color = 'black'),  
      legend.position = 'right'
  )
plot_total_profit
#interactive
ggplotly(plot_total_profit)

#############################################################
###visualize total profit lost by discounts using ggplot2####
plot_profit_lost <- ggplot(distinct_df_all, aes(x=Year, y= total_profit_lost_by_discounts, fill=Quartile))+
  scale_fill_manual(values= c("#219ebc", "#023047", "#ffb703", "#fb8500"))+
  geom_bar(stat="identity")+
  theme_light()+
  scale_y_continuous(breaks = c(0, 100000, 200000, 300000, 400000),labels = scales::number_format(accuracy = 1, decimal.mark = '.'))+
  ylab("Total profit lost by discounts (USD)")+
  geom_label(
    color = "white",
    label = sprintf('%0.2f', distinct_df_all$total_profit_lost_by_discounts), size=4,
    show.legend = F,
    position = position_stack(vjust = 0.5))+
  theme(
    axis.text.x = element_text(color = 'black', size = 12),
    axis.text.y = element_text(color = 'black', size = 12),
    axis.title.x = element_text(color = 'black', size = 15),
    axis.title.y = element_text(color = 'black', size = 15),
    axis.ticks = element_line(color = 'black'),  
    legend.position = 'right'
  )
plot_profit_lost

#interactive
ggplotly(plot_profit_lost)

##combine using patchwork package##
combined <- plot_total_profit+plot_profit_lost+plot_layout(guides = "collect") #combined plots, common legend 
combined


#############################################################
#########best selling products per quartile and year#########
best_sellers <- data_order_items %>%
                inner_join(data_products) %>%
                inner_join(data_categories) %>%
                inner_join(data_orders) %>%
                mutate(Quartile = case_when(
                  str_detect(order_date, "-(01|02|03)-") ~ "Q1",  
                  str_detect(order_date, "-(04|05|06)-") ~ "Q2",  
                  str_detect(order_date, "-(07|08|09)-") ~ "Q3",  
                  str_detect(order_date, "-(10|11|12)-") ~ "Q4",
                  TRUE ~ "NA")) %>%
                mutate(Year = case_when(
                  str_detect(order_date, "2016") ~"2016",
                  str_detect(order_date, "2017") ~"2017",
                  str_detect(order_date, "2018") ~"2018", #use package "stringr" for str_detect()
                  TRUE ~ "NA")) %>%
                dplyr::filter(!is.na(shipped_date))

##add the count of each product and select only top 3 movers of each quartile to keep it more simple
sorted_best_sellers <- best_sellers %>%
                group_by(Year, Quartile) %>% #could also be sorted per quartile and year
                count(product_name, sort = T) %>%
                top_n(3) #select top 3 movers per quartile and year

sorted_best_sellers$product_name <- factor(sorted_best_sellers$product_name, levels = c("Electra Girl's Hawaii 1 (16-inch) - 2015/2016", "Electra Cruiser 1 (24-Inch) - 2016", "Electra Townie Original 7D EQ - 2016", "Electra Townie Original 21D - 2016", "Electra Girl's Hawaii 1 16\" - 2017", "Electra Townie Original 7D - 2017", "Sun Bicycles Cruz 3 - 2017", "Sun Bicycles Lil Bolt Type-R - 2017", "Sun Bicycles ElectroLite - 2017", "Trek Farley Alloy Frameset - 2017", "Trek Precaliber 12 Boys - 2017", "Electra Cruiser 7D (24-Inch) Ladies' - 2016/2018", "Electra Townie Balloon 3i EQ Ladies' - 2018", "Strider Classic 12 Balance Bike - 2018"))
                                           
###2016###
plot_best_sellers_2016 <- ggplot(subset(sorted_best_sellers, Year == "2016"), aes(x=Quartile, y= n, fill=product_name))+
  #scale_fill_manual(values= c("#219ebc", "#023047", "#ffb703", "#fb8500"))+
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("#E63946", "#F4A261", "#E9C46A", "#2A9D8F", "#264653", 
                               "#8D6A9F", "#A67C52", "#D67D3E", "#735D78", "#B2C7C7", 
                               "#C08497", "#6D6875", "#D8A48F", "#403D39"))+
  theme_void()+
  coord_polar(theta = "x") +
  #facet_grid(~Year, space="free_x", scale="free")+
  #scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125))+
  ylab("Number of units sold")+
  guides(fill= guide_legend(ncol = 1))+
  ggtitle("2016")+
  labs(fill="")+ #removes legend title
  geom_label(
     color = "white",
     label = subset(sorted_best_sellers, Year == "2016")$n, size=4,
     show.legend = F,
     position = position_stack(vjust = 0.5))+
  theme(
    axis.text.x = element_text(color = 'black', size = 13, face = "bold"), #adjust angle of axis text as well as vertical and horizontal justification
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.text.x = element_text(color = 'black', size = 15),
    legend.title = element_text(color = 'black', size = 15),
    axis.ticks = element_line(color = 'black'),  
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5),
    legend.position = 'bottom'
  )
plot_best_sellers_2016

###2017###
plot_best_sellers_2017 <- ggplot(subset(sorted_best_sellers, Year == "2017"), aes(x=Quartile, y= n, fill=product_name))+
  #scale_fill_manual(values= c("#219ebc", "#023047", "#ffb703", "#fb8500"))+
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("#E9C46A", "#2A9D8F","brown1","#B2C7C7","#8D6A9F", "#A67C52", "#D67D3E", "#6D6875", "#D8A48F"))+
  theme_void()+
  coord_polar(theta = "x") +
  #facet_grid(~Year, space="free_x", scale="free")+
  #scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125))+
  ylab("Number of units sold")+
  guides(fill= guide_legend(ncol = 1))+
  ggtitle("2017")+
  labs(fill="")+ #removes legend title
  labs(x="Top 3 movers per quartile and year")+
  geom_label(
    color = "white",
    label = subset(sorted_best_sellers, Year == "2017")$n, size=4,
    show.legend = F,
    position = position_stack(vjust = 0.5))+
  theme(
    axis.text.x = element_text(color = 'black', size = 13, face = "bold"), #adjust angle of axis text as well as vertical and horizontal justification
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(color = "black", size = 15, face = "bold"),
    strip.text.x = element_text(color = 'black', size = 15),
    legend.title = element_text(color = 'black', size = 15),
    axis.ticks = element_line(color = 'black'),  
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5),
    legend.position = 'bottom'
  )
plot_best_sellers_2017

###2018###
plot_best_sellers_2018 <- ggplot(subset(sorted_best_sellers, Year == "2018"), aes(x=Quartile, y= n, fill=product_name))+
  #scale_fill_manual(values= c("#219ebc", "#023047", "#ffb703", "#fb8500"))+
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("#2A9D8F", "#264653", "cornflowerblue", "darkseagreen"))+
  theme_void()+
  coord_polar(theta = "x") +
  #facet_grid(~Year, space="free_x", scale="free")+
  #scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125))+
  ylab("Number of units sold")+
  guides(fill= guide_legend(ncol = 1))+
  ggtitle("2018")+
  labs(fill="")+ #removes legend title
  geom_label(
    color = "white",
    label = subset(sorted_best_sellers, Year == "2018")$n, size=4,
    show.legend = F,
    position = position_stack(vjust = 0.5))+
  theme(
    axis.text.x = element_text(color = 'black', size = 13, face = "bold"), #adjust angle of axis text as well as vertical and horizontal justification
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.text.x = element_text(color = 'black', size = 15),
    legend.title = element_text(color = 'black', size = 15),
    axis.ticks = element_line(color = 'black'),  
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5),
    legend.position = 'bottom',
  )
plot_best_sellers_2018

#combine best selling plots
combined_best_sellers <- (plot_best_sellers_2016+plot_best_sellers_2017+plot_best_sellers_2018)+plot_layout(guides = "collect")&theme(legend.position = "bottom")
combined_best_sellers
#combine profit, profit lost and best selling plots
total_combined <- combined/combined_best_sellers
total_combined

###########Which employee realized the highest profit?###########
###########Each employee receives a bonus of 1% of total realized profit###########
employee_bonus <- data_orders %>%
                  dplyr::inner_join(data_order_items) %>%
                  dplyr::inner_join(data_staffs) %>%
                  mutate(Quartile = case_when(
                    str_detect(order_date, "-(01|02|03)-") ~ "Q1",  
                    str_detect(order_date, "-(04|05|06)-") ~ "Q2",  
                    str_detect(order_date, "-(07|08|09)-") ~ "Q3",  
                    str_detect(order_date, "-(10|11|12)-") ~ "Q4",
                    TRUE ~ "NA")) %>%
                  mutate(Year = case_when(
                    str_detect(order_date, "2016") ~"2016",
                    str_detect(order_date, "2017") ~"2017",
                    str_detect(order_date, "2018") ~"2018", #use package "stringr" for str_detect()
                    TRUE ~ "NA")) %>%
                  dplyr::filter(!is.na(shipped_date))%>%
                  group_by(staff_id, Year) %>%
                  mutate(total_profit_per_employee = list_price * (1.0 - discount) * quantity) %>%
                  mutate(bonus_per_employee = (list_price * (1.0 - discount) * quantity) / 100 ) %>%
                  select(c(staff_id, first_name, last_name, Year, Quartile, total_profit_per_employee, bonus_per_employee))

###add sum of total profit and respective bonus###
additional <- employee_bonus %>%
              select(c(staff_id, first_name, last_name, Year, Quartile, total_profit_per_employee, bonus_per_employee)) %>%
              group_by(last_name, Year) %>%
              summarise(across(total_profit_per_employee:bonus_per_employee, sum))

###lollipop chart of annual bonus###
bonus <- ggplot(additional, aes(x=Year, y=bonus_per_employee, colour = last_name)) +
  geom_segment(aes(x=Year, xend=Year, y=0, yend=bonus_per_employee), color="lightgrey", size = 1.5, alpha = 0.7) + #adds the stick of the "lollipop"
  geom_point(size=6, alpha=1) + #adds the actual lollipop
  facet_grid(~last_name)+ #add facets for employee based performance overview, depending on the year
  labs(y="Employee bonus p.a.")+
  scale_y_continuous(breaks = c(0,2500, 5000, 7500, 10000, 12500, 15000))+
  theme_light() +
  scale_color_manual("Employee", values = c("#FFC300", "#D35400", "#6B8E23", "#A52A2A", "#008080","#83769C"))+
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    strip.text.x = element_text(colour = "white", size = 12),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(color = 'black', size = 12), 
    axis.text.y = element_text(color = 'black', size = 12),
    axis.title.x = element_text(color = 'black', size = 15),
    axis.title.y = element_text(color = 'black', size = 15),
    axis.ticks = element_blank()
  )
bonus

#interactive
ggplotly(bonus)