library(readr)
library(readxl)
library(readr)
library(magrittr)
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)
library(knitr)
library(kableExtra)
library(RColorBrewer)
library(stringr)
library(psych)
library(corrplot)
library(gridExtra)
library(robustHD)

#data read from csv dataset

delhivery_data <- read_csv("delhivery_data.csv")

summary(delhivery_data)
str(delhivery_data)
summary(delhivery_data[, sapply(delhivery_data, is.numeric)])
colSums(is.na(delhivery_data))/nrow(delhivery_data)*100

#data cleaning

delhivery_data[delhivery_data=="nan"]<-NA
delhivery_data<-na.omit(delhivery_data)

#changing data time to correct format

delhivery_data$trip_creation_time <- as.POSIXct(delhivery_data$trip_creation_time, format="%d-%m-%Y %H:%M")

delhivery_data$od_start_time <- as.POSIXct(delhivery_data$od_start_time, format="%d-%m-%Y %H:%M")
delhivery_data$od_end_time <- as.POSIXct(delhivery_data$od_end_time, format="%d-%m-%Y %H:%M")


#remove redundant columns

delhivery_data<-delhivery_data[,-c(12,13,14,19,23)]

#grouping event data to sub trip level and aggregating time and measures

data_grouped_center<-delhivery_data%>%
  group_by(trip_uuid,source_center,destination_center)%>%
  summarize(trip_creation_time=unique(trip_creation_time),
            trip_uuid=unique(trip_uuid),
            route_type=unique(route_type),
            source_name=unique(source_name),
            destination_name=unique(destination_name),
            od_start_time=unique(od_start_time),
            od_end_time=unique(od_end_time),
            start_scan_to_end_scan=unique(start_scan_to_end_scan),
            actual_distance_to_destination=max(actual_distance_to_destination),
            actual_time=max(actual_time),
            osrm_time=max(osrm_time),
            osrm_distance=max(osrm_distance))


# grouping data by trip and aggregating time distance, source and destination
data_trip<-data_grouped_center %>% 
  group_by(trip_uuid) %>% 
  summarize(
    trip_created=unique(trip_creation_time),
    route_type=unique(route_type),
    source_id = source_center[!(source_center %in% destination_center)], # filter out groups where source codes are present in destination codes(round trip)
    dest_id = destination_center[!(destination_center %in% source_center)], # filter out groups where source codes are present in destination codes(round trip)
    source = source_name[!(source_name %in% destination_name)],
    destination = destination_name[!(destination_name %in% source_name)],
    scan_start_time=min(od_start_time),
    scan_end_time=max(od_end_time),
    end_to_end_scan_time=sum(start_scan_to_end_scan),
    distance_to_destination_center=sum(actual_distance_to_destination),
    time_to_destination_center=sum(actual_time),
    trip_osrm_time=sum(osrm_time),
    trip_osrm_distance=sum(osrm_distance),
    processing_buffer=as.numeric(scan_start_time-trip_created+end_to_end_scan_time-time_to_destination_center) #feature created
  )
    
#splitting delivery hub address to city state and hub category (D/H/Hub/DDB)

location_split<-function(data,column)
{
split_strings <- str_split(data[[column]], "_|\\s(?=\\()")
city <- sapply(split_strings, function(x) x[1])
state <- sapply(split_strings, function(x) str_remove(x[length(x)], "\\("))%>%str_remove("\\).*")
hubtype <- sapply(split_strings, function(x) {
  if(length(x) > 2) {
    if(grepl("\\d", x[length(x)-1])) {
      if(nchar(x[length(x)-2]) > 3) {
        NA
      } else {
        x[length(x)-2]
      }
    } else {
      if(nchar(x[length(x)-1]) > 3){
        NA
      } else {
        x[length(x)-1]
      }
    }
  } else {
    NA
  }
})
df <- data.frame(city, hubtype, state)
return(df)
}


source_add<-data_trip%>%
  location_split(.,"source")


destination_add<-data_trip%>%
  location_split(.,"destination")


#trip level dataset for analysis

final_data<-cbind(data_trip,source_add,destination_add)
names(final_data)[c(16,17,18,19,20,21)]<-c("src_city","src_type","src_state",
                                   "dst_city","dst_type","dst_state")


describe<-describe(final_data)

data_describe<-describe[(rownames(describe) %in% c("end_to_end_scan_time","distance_to_destination_center",
                                                   "time_to_destination_center","trip_osrm_time","trip_osrm_distance",
                                                   "processing_buffer")),
                        !(colnames(describe) %in% c("vars","n","trimmed","mad"))]

#data descriptive statistics

knitr::kable(data_describe,align="l",caption = "Table 1: Decriptive Statistics",
             digits = 2)%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))%>%
  column_spec(1,bold = TRUE)  

#subset only quantitative data

final_data_quant<-final_data[,c(10,11,12,13,14,15)]

#medcouple for Skewed data outliers

df_mc <- final_data_quant %>% 
  mutate_if(is.numeric, function(x) mc(x))%>%
  unique()

#log transform

df_log <- final_data_quant %>% 
  mutate_if(is.numeric, function(x) log(x))

labels<- c("end_to_end_scan_time","distance_to_destination_center",
           "time_to_destination_center","trip_osrm_time","trip_osrm_distance",
           "processing_buffer")

#density Plot

plots_original <- lapply(final_data_quant, function(x) ggplot(final_data_quant, aes_string(x)) + 
                           geom_density()+theme_classic())
plots_original <- mapply(function(plot, title) {
  plot + ggtitle(title)
}, plot = plots_original, title = labels, SIMPLIFY = FALSE)
grid.arrange(grobs = plots_original, ncol = 3,top="Figure 1: Distribution")

#log transformed Density plot

plots <- lapply(df_log, function(x) ggplot(df_log, aes_string(x)) + 
                  geom_density()+theme_classic())
plots <- mapply(function(plot, title) {
  plot + ggtitle(title)
}, plot = plots, title = labels, SIMPLIFY = FALSE)
grid.arrange(grobs = plots, ncol = 3,top="Figure 2:Log transformed Distribution")

#log transformed Boxplot

plots_box <- lapply(df_log, function(x) ggplot(df_log, aes_string(x)) + 
                  geom_boxplot()+theme_classic())
plots_box <- mapply(function(plot, title) {
  plot + ggtitle(title)
}, plot = plots_box, title = labels, SIMPLIFY = FALSE)
grid.arrange(grobs = plots_box, ncol = 3,top="Figure 3:Log transformed Distribution")



#filter by route type

FTL_trips<-final_data%>%
  filter(route_type=="FTL")
Cart_trips<-final_data%>%
  filter(route_type=="Carting")
colnames(final_data)

#distribuions grouped by Route type

plots <- lapply(names(final_data[, c(3, 10:15)]), function(x) {
  ggplot(final_data[, c(3, 10:15)], aes_string(x)) + 
    geom_density(aes(fill = route_type), alpha = 0.6) +
    theme_classic() +
    labs(title = x) +  # add title to each plot
    guides(color = FALSE)  # remove the legend for the route_type color
})
grid.arrange(grobs = plots, ncol = 3,name="Figure 4: Grouped Distributions")

#processing buffer route_type

ggplot(final_data[, c(3, 10:15)], aes(x = processing_buffer, fill = route_type)) +
  geom_density(alpha = 0.6) +
  scale_fill_discrete(guide = "legend") +
  labs(
    title = "Figure 5: Processing Buffer by Route Type (log)",
    x = "Processing Buffer (minutes)",
    y = "",
    fill = "Route Type"
  ) +
  scale_x_log10() +
  theme_classic()

#Hypothesis test

#Hypothesis testing


actual_trip_time<-final_data_quant$end_to_end_scan_time
osrm_time<-final_data_quant$trip_osrm_time
actual_trip_distance<-final_data_quant$distance_to_destination_center
osrm_distance<-final_data_quant$trip_osrm_distance

#processing buffers Cart Trips and FTL 

t.test(Cart_trips$processing_buffer,FTL_trips$processing_buffer,alternative = "less")

#trip time and distance are less than predicted time and distances by open source software

t.test(actual_trip_time,osrm_time,alternative = "greater")

#Correlation

cor<-cor(final_data_quant)
corrplot(cor,method = "number", type = "full", tl.col = "black")

#Regression

#Regression each level of category
route_type <- unique(final_data$route_type)
models_osrm <- list()
models_ground<- list()
for (i in 1:length(route_type)) {
  route <- route_type[i]
  model_osrm <- lm((trip_osrm_time) ~ (trip_osrm_distance), data =final_data, subset = route_type==route)
  models_osrm[[i]] <- model_osrm
  model_ground <- lm(log(time_to_destination_center) ~ log(distance_to_destination_center), data =final_data, subset = route_type==route)
  models_ground[[i]] <- model_ground
}

#empty data frame to store regression values
coeff_table_osrm <- data.frame(Route_Type = character(),
                               intercept = numeric(),
                               coef = numeric(),
                               p_intercept = numeric(),
                               p_coef = numeric(),
                               R_sq = numeric(),
                               stringsAsFactors = FALSE)

#extracting coefficients, p-values, and R-sq from model_osrm list
for (i in 1:length(models_osrm)) {
  model_osrm <- models_osrm[[i]]
  route <- unique(final_data$route_type)[i]
  intercept <- coef(model_osrm)[1]
  slope <- coef(model_osrm)[2]
  p_intercept <- summary(model_osrm)$coef[1, 4]
  p_slope <- summary(model_osrm)$coef[2, 4]
  R_sq <- summary(model_osrm)$adj.r.squared
  new_row <- data.frame(Route_Type = route, intercept = intercept, coef = slope,
                        p_intercept = p_intercept, P_model = p_slope, R_sq = R_sq)
  coeff_table_osrm <- rbind(coeff_table_osrm, new_row)
}

#regression table
rownames(coeff_table_osrm)<-NULL
knitr::kable(coeff_table_osrm,align="l",caption = "Table 2: Regression Table OSRM Time & Distance")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))%>%
  column_spec(1,bold = TRUE)


#scatterplot grouped OSRM

ggplot(final_data, aes(x = (trip_osrm_distance),
                       y = (trip_osrm_time), color = factor(route_type))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE,color="black",linewidth=1) +
  facet_wrap(~ route_type, ncol = 1) +
  labs(title = "Figure 7: Grouped Regression Analysis (OSRM)", x = "OSRM Distance (Km)",
       y = "OSRM Time(minutes)", color = "Route")




#extracting coefficients, p-values, and R-sq from model_ground list

coeff_table_ground <- data.frame(Route_Type = character(),
                               intercept = numeric(),
                               coef = numeric(),
                               p_intercept = numeric(),
                               p_coef = numeric(),
                               R_sq = numeric(),
                               stringsAsFactors = FALSE)


for (i in 1:length(models_ground)) {
  model_ground <- models_ground[[i]]
  route <- unique(final_data$route_type)[i]
  intercept <- coef(model_ground)[1]
  slope <- coef(model_ground)[2]
  p_intercept <- summary(model_ground)$coef[1, 4]
  p_slope <- summary(model_ground)$coef[2, 4]
  R_sq <- summary(model_ground)$adj.r.squared
  new_row <- data.frame(Route_Type = route, intercept = intercept, coef = slope,
                        p_intercept = p_intercept, P_model = p_slope, R_sq = R_sq)
  coeff_table_ground <- rbind(coeff_table_ground, new_row)
}

#regression table
rownames(coeff_table_ground)<-NULL
knitr::kable(coeff_table_ground,align="l",caption = "Table 3: Regression Table Ground Time & Distance")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))%>%
  column_spec(1,bold = TRUE)

#scatterplot grouped Ground

ggplot(final_data, aes(x = log(distance_to_destination_center),
                       y = log(time_to_destination_center), color = factor(route_type))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE,color="black",linewidth=1) +
  facet_wrap(~ route_type, ncol = 1) +
  labs(title = "Figure 8: Grouped Regression Analysis Ground", x = "Ground Distance (Km)",
       y = "Ground Time (minutes)", color = "Route")



