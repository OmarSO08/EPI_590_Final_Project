# Load the data
load("data/shooting_data.rda")
df<- shooting_data


#create a table of descriptive statistics
library(gtsummary)
summary_table <- tbl_summary(
	data = df,
	by = State,
	include = c(`Killed (includes shooter)`, `Number of Shots Fired`, `Number of Shooters`),
	label = list(
		`Killed (includes shooter)` ~ "Killed (includes shooter)",
		`Number of Shots Fired` ~ "Number of Shots Fired"
	),
	missing_text = "Missing"
)
summary_table


# Fit a linear regression model
library(broom.helpers)
model <- lm(`Killed (includes shooter)` ~ `State`, data = df)
# Create a regression table using tidy_parameters
regression_table <- tbl_regression(
	model,
	title = "Regression Analysis of Total Fatalities (Including Shooter) by State",
	tidy_fun = broom.helpers::tidy_parameters
)

# Print the regression table
regression_table

# Create a figure: Histogram of 'Killed (includes shooter)'
library(ggplot2)
ggplot(df, aes(x = `Killed (includes shooter)`)) +
	geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
	labs(title = "Histogram of Killed (includes shooter)",
			 x = "Number of People Killed (including shooter)",
			 y = "Frequency") +
	theme_minimal()



# Write a function to calculate standard deviation of 'Killed (includes shooter)' for each state
library(dplyr)
calculate_sd_per_state <- function(data) {

	result <- data |>
		group_by(State) |>
		summarise(SD_Killed = sd(`Killed (includes shooter)`, na.rm = TRUE))


	return(result)
}
# Use the function to calculate standard deviation of 'Killed (includes shooter)' for each state
sd_per_state <- calculate_sd_per_state(df)

print(sd_per_state)

