
# Title: "Modeling of the daily dynamics in bike rental system using weather
# and calendar conditions: A semi-parametric approach"
# Authors: Christopher Odoom, Alexander Boateng, Sarah Fobi Mensah,
# and Daniel Maposa

rm(list = ls())

## Load required packages
required_packages <- c(
  "corrplot", "dbscan", "ggplot2", "mgcv", "readr", "tidyverse"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing required R packages: ",
      paste(missing_packages, collapse = ", "),
      ".\nInstall them with:\ninstall.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))

optional_packages <- c("AER", "gratia", "pander", "yarrr")
available_optional_packages <- vapply(
  optional_packages,
  requireNamespace,
  logical(1),
  quietly = TRUE
)

if (any(!available_optional_packages)) {
  message(
    "Optional packages not installed; related output will be skipped: ",
    paste(names(available_optional_packages)[!available_optional_packages], collapse = ", ")
  )
}

print_table <- function(x, caption = NULL) {
  if (available_optional_packages[["pander"]]) {
    pander::pander(x, caption = caption)
  } else {
    if (!is.null(caption)) {
      message(caption)
    }
    print(x)
  }
}

## Save all generated plots under figures/
figures_dir <- "figures"
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

save_plot <- function(plot, filename, width = 9, height = 6) {
  ggplot2::ggsave(
    filename = file.path(figures_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}

save_base_plot <- function(filename, code, width = 2400, height = 1800) {
  grDevices::png(
    filename = file.path(figures_dir, filename),
    width = width,
    height = height,
    res = 300
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  eval.parent(substitute(code))
}

## Import the processed data. Run this script from the repository root.
data_path <- file.path("data", "paperbike_data.csv")
if (!file.exists(data_path)) {
  stop("Data file not found. Run this script from the repository root.")
}
dat <- read_csv(data_path)
head(dat)

##Renaming some columns
colnames(dat)[colnames(dat)=="Wind"] = "windspeed"
colnames(dat)[colnames(dat)=="Temp"] <- "temp"
colnames(dat)[colnames(dat)=="Humidity"] <- "humidity"
colnames(dat)[colnames(dat)=="Barometer"] <- "atmospres"
colnames(dat)[colnames(dat)=="workinday"] <- "workingday"
colnames(dat)[colnames(dat)=="Visibility"] <- "visibility"
colnames(dat)[colnames(dat)=="Weather"] <- "weather"
colnames(dat)[colnames(dat)=="Year"] <- "year"
colnames(dat)[colnames(dat)=="Weekday"] <- "weekday"
colnames(dat)[colnames(dat)=="...1"] <- "instant"
colnames(dat)[colnames(dat)=="Month"] <- "month"
colnames(dat)

## Converting categorical variables to factor.
dat$season <- as.factor(dat$season)
dat$holiday <- as.factor(dat$holiday)
dat$weekday <- as.factor(dat$weekday)
dat$workingday <- as.factor(dat$workingday)
dat$weather <- as.factor(dat$weather)
#dat$workingday <- as.factor(dat$workingday)
dat$month <- as.factor(dat$month)
dat$year <- as.factor(dat$year)
dat$day <-as.factor(dat$day)

##Performing exploratory Data analysis
fig1 <- dat %>%  ggplot(aes(x = total_rentals)) +
  geom_histogram(aes(y = ..ncount..), bins = 20, fill = "steelblue", col = "black") +
  theme_bw()+
  labs(title = "Distribution of Count of Total Rental Bikes  ",
       x = "Count of Total Bikes Rented ",
       y = "Density")
save_plot(fig1, "01_total_rentals_distribution.png")


save_base_plot("02_rental_type_histograms.png", {
  par(mfrow = c(1, 3))
  hist(dat$total_rentals, col = "palegreen", main = "Total Rental")
  hist(dat$registered, col = "palegreen", main = "Registered Rental")
  hist(dat$casual, col = "palegreen", main = "Casual Rental")
})

fig2 <-dat %>%  ggplot(aes(x = month, y = total_rentals, col = season)) +
  geom_boxplot() +
  theme_bw()+
  labs(title = "Boxplot of Count of Total Rental Bikes Against month",
       x = "Month " ,
       y = "count of total rental bikes")
save_plot(fig2, "03_monthly_rentals_boxplot.png")

fig3 <- dat %>% ggplot(aes(x = season, y = total_rentals, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#00b386", "#0090ac","#10b0ec","#0000ac"))+
  theme_bw()+
  labs(title = "Scatterplot of Count of Total Rental Bikes Against season",
       x = "season " ,
       y = "count of total rental bikes")
save_plot(fig3, "04_seasonal_rentals.png")

fig4 <- dat %>%  ggplot(aes(x = temp, y =total_rentals, col = visibility)) +
  geom_point() +
  geom_smooth() +
  theme_bw()+
  labs(title = "Scatterplot of Count of Total Rental Bikes Against Temperature by Visibility",
       x = "temperature  " ,
       y = "count of total rental bikes")
save_plot(fig4, "05_temperature_visibility.png")

fig41 <- dat %>%  ggplot(aes(x = temp, y =total_rentals, col = humidity)) +
  geom_point() +
  geom_smooth() +
  theme_bw()+
  labs(title = "Scatterplot of Count of Total Rental Bikes Against Temperature by Visibility",
       x = "temperature  " ,
       y = "count of total rental bikes")
save_plot(fig41, "06_temperature_humidity.png")

if (available_optional_packages[["yarrr"]]) {
  save_base_plot("07_holiday_workingday_pirateplot.png", {
    yarrr::pirateplot(
      formula = total_rentals ~ holiday + workingday,
      data = dat,
      cex.names = 0.75,
      main = "Bike rentals by holiday and working-day status",
      xlab = "Holiday and working-day status",
      ylab = "Count of total rental bikes"
    )
  })
}

fig6 <-dat %>%  ggplot(aes(x = registered, y = total_rentals)) +
  geom_point() +
  geom_smooth() +
  theme_bw()+
  labs(title = "Scatterplot of Count of Total Rental Bikes Against Count of Registered Users",
       x = "Count of registered users  " ,
       y = "count of total rental bikes")
save_plot(fig6, "08_registered_vs_total_rentals.png")

fig7 <- dat %>% ggplot(aes(x = casual, y =total_rentals )) +
  geom_point() +
  geom_smooth() +
  theme_bw()+
  labs(title = "Scatterplot of Count of Total Rental Bikes Against Count of Casual Users",
       x = "Count of casual users  " ,
       y = "count of total rental bikes")
save_plot(fig7, "09_casual_vs_total_rentals.png")

fig8 <- dat %>% ggplot(aes(x=windspeed, y=total_rentals, col=weekday))+
  geom_point()
save_plot(fig8, "10_windspeed_by_weekday.png")

# Gather the columns related to "casual" and "registered"
gathered_df <- dat %>%
  select(instant, date, casual, registered, humidity,windspeed,workingday) %>%
  gather(key = "rental_type", value = "rentals", casual:registered)

# Check the gathered data frame
fig9 <- gathered_df %>%
  ggplot(aes(x = rental_type, y = rentals, fill = workingday)) +
  geom_boxplot() +
  xlab("membership type") +
  scale_fill_manual(values = c("green", "yellow"))
save_plot(fig9, "11_rentals_by_membership_type.png")

#### Scatterplot matrix
save_base_plot("12_scatterplot_matrix.png", {
  dat %>%
    dplyr::select(
      temp, humidity, visibility, windspeed, casual, registered, total_rentals
    ) %>%
    pairs()
})


correlation_matrix <- cor(dat[, c("total_rentals", "temp", "windspeed", "humidity", "visibility")])
print(correlation_matrix)
save_base_plot("13_correlation_matrix.png", {
  corrplot::corrplot(correlation_matrix)
})

#### Descriptive 
print_table(
  summary(dat[, c("total_rentals", "temp", "windspeed", "humidity", "visibility")])
)

gat_df <- dat %>%
  select(instant, date, casual, registered, humidity,windspeed,holiday,day) %>%
  gather(key = "rental_type", value = "rentals", casual:registered)
### The goal is to visualise the day effect on rentals
#gat_df$day <-as.numeric(gat_df$day)
fig11 <- gat_df %>% ggplot(aes(x=day, y=rentals, col=rental_type))+
  geom_boxplot()
save_plot(fig11, "14_daily_rentals_by_membership.png")

# Spatial density plot for the total

fig_total_map <- ggplot(dat, aes(x = longitude, y = latitude, color = total_rentals)) +
  geom_point(size = 6) +
  labs(title = "Spatial Density of Bike Rentals", x = "Longitude", y = "Latitude") +
  theme_minimal()
save_plot(fig_total_map, "16_spatial_density_total.png")

# Spatial density plot for registered

fig_registered_map <- ggplot(dat, aes(x = longitude, y = latitude, color = registered)) +
  geom_point(size = 6) +
  labs(title = "Spatial Density of Bike Rentals", x = "Longitude", y = "Latitude") +
  theme_minimal()
save_plot(fig_registered_map, "17_spatial_density_registered.png")

# Spatial density plot for casual 

fig_casual_map <- ggplot(dat, aes(x = longitude, y = latitude, color = casual)) +
  geom_point(size = 6) +
  labs(title = "Spatial Density of Bike Rentals", x = "Longitude", y = "Latitude") +
  theme_minimal()
save_plot(fig_casual_map, "18_spatial_density_casual.png")

# Spatial clustering
coordinates <- dat[, c("longitude", "latitude")]
### Finding suitable DBSCAN parameters
save_base_plot("19_dbscan_k_distance.png", {
  dbscan::kNNdistplot(coordinates, minPts = 2)
  abline(h = 0.0002, col = "red", lty = 3)
})

dbscan_result <- dbscan(coordinates, eps = 0.0004, minPts = 5)
clusters <- dbscan_result$cluster

fig_clusters <- ggplot(
  dat,
  aes(x = longitude, y = latitude, color = factor(clusters))
) +
  geom_point(size = 3) +
  labs(title = "Spatial Clustering of Bike Rentals", x = "Longitude", y = "Latitude") +
  theme_minimal()
save_plot(fig_clusters, "20_spatial_clusters.png")

###+++++++++++++++++++++++++ END OF EDA ++++++++++++++++++###########################

## Modelling total rentals condition on weather conditions and location

##Check conditions(Assumptions) of poisson regression
mean(dat$total_rentals)
var(dat$total_rentals)

##Checking for dispersion using the dispersiontest

dat$day <-as.numeric(dat$day)
mod.initial <- gam(total_rentals~visibility+windspeed+season+
                     workingday+year+s(month, bs="re")+
                     s(longitude,latitude)+s(temp)+
                     s(temp, by=workingday)+s(humidity), data = dat, family = poisson)
summary(mod.initial)

if (available_optional_packages[["AER"]]) {
  print(AER::dispersiontest(mod.initial))
}


#### modeling using Quasi-poisson

dat$day <-as.numeric(dat$day)
mod1 <- gam(total_rentals~visibility+windspeed+season+workingday+year+s(month, bs="re")+s(longitude,latitude)+s(temp)+s(temp, by=workingday)+s(humidity), data = dat, family = quasipoisson)

summary(mod1)

## Table 1: Parameteric Estimates
para1 <- summary(mod1)
print_table(para1$p.table)

## Table 2: Non Parameteric Estimates
nonpara1 <- summary(mod1)

print_table(nonpara1$s.table, caption = "Approximate significance of smooth terms")

## Model1 Performance measure
perm1 <- summary(mod1)
perm1$dev.expl*100

## Drawing the effect 
if (available_optional_packages[["gratia"]]) {
  mod1_map <- gratia::draw(mod1, select = 2)
  save_plot(mod1_map, "21_total_model_spatial_effect.png")

  ## Smooths for other predictors
  mod1_smooths <- gratia::draw(mod1, select = c(3, 4, 5, 6))
  save_plot(mod1_smooths, "22_total_model_smooth_effects.png", width = 12, height = 8)
}

## Checking number of knots
save_base_plot("23_total_model_gam_check.png", {
  gam.check(mod1)
})

if (available_optional_packages[["gratia"]]) {
  mod1_diagnostics <- gratia::appraise(mod1)
  save_plot(mod1_diagnostics, "24_total_model_diagnostics.png", width = 12, height = 8)
}

## Model2 for registered rentals
mod2 <- gam(registered~visibility+windspeed+season+workingday+year+s(month, bs="re")+s(longitude,latitude)+s(temp)+s(temp, by=workingday)+s(humidity), data = dat, family = quasipoisson)

summary(mod2)


## Table 3: Parameteric Estimates
para2 <- summary(mod2)
print_table(para2$p.table)

## Table 4: Non-Parameteric Estimates
nonpara2 <- summary(mod2)
print_table(nonpara2$s.table)


if (available_optional_packages[["gratia"]]) {
  mod2_map <- gratia::draw(mod2, select = 2)
  mod2_smooths <- gratia::draw(mod2, select = c(3, 4, 5, 6))
  mod2_diagnostics <- gratia::appraise(mod2)
  save_plot(mod2_map, "25_registered_model_spatial_effect.png")
  save_plot(
    mod2_smooths,
    "26_registered_model_smooth_effects.png",
    width = 12,
    height = 8
  )
  save_plot(
    mod2_diagnostics,
    "27_registered_model_diagnostics.png",
    width = 12,
    height = 8
  )
}

### Model3 Casual rentals
mod3 <- gam(casual~visibility+windspeed+season+workingday+year+s(month, bs="re")+s(longitude,latitude)+s(temp)+s(temp, by=workingday)+s(humidity), data = dat, family = quasipoisson)

summary(mod3)

## Table 5: Parameteric Estimates
para3 <- summary(mod3)
print_table(para3$p.table)


## Table 6: Parameteric Estimates

nonpara3 <- summary(mod3)
print_table(nonpara3$s.table)

if (available_optional_packages[["gratia"]]) {
  mod3_map <- gratia::draw(mod3, select = 2)
  mod3_smooths <- gratia::draw(mod3, select = c(3, 4, 5, 6))
  mod3_diagnostics <- gratia::appraise(mod3)
  save_plot(mod3_map, "28_casual_model_spatial_effect.png")
  save_plot(
    mod3_smooths,
    "29_casual_model_smooth_effects.png",
    width = 12,
    height = 8
  )
  save_plot(
    mod3_diagnostics,
    "30_casual_model_diagnostics.png",
    width = 12,
    height = 8
  )
}
## Check efficiency of Knots
save_base_plot("31_casual_model_gam_check.png", {
  gam.check(mod3)
})


## Comparng model 2 and model 3
a=para3$p.coeff
b=para2$p.coeff
est <-names(para2$p.coeff)
coff1 <- tibble(a,b,est)
colnames(coff1) <- c("casual", "registered", "Parametric Est")
coff1

fig_coefficients <- coff1 %>%
  gather('Type', 'Effect', -`Parametric Est`)%>%
  dplyr::filter(`Parametric Est`!="(Intercept)")%>% 
  ggplot(aes(x = Effect, y=`Parametric Est`)) +
  geom_col(aes(fill = Type), position = 'dodge')
save_plot(
  fig_coefficients,
  "32_casual_registered_coefficient_comparison.png",
  width = 10,
  height = 7
)
