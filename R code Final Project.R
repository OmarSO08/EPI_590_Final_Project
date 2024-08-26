# Load the data
load("data/shooting_data.rda")
df<- shooting_data


#create a table of dicriptive statistics
library(gtsummary)
tbl_summary(
	data = df,
	by = State,
	include = c(`Killed (includes shooter)`, `Number of Shots Fired`),
	label = list(
		`Killed (includes shooter)` ~ "Killed (includes shooter)",
		`Number of Shots Fired` ~ "Number of Shots Fired"
	),
	missing_text = "Missing"
)



# Fit a linear regression model
model <- lm(`Killed (includes shooter)` ~ `Number of Shots Fired`, data = df)



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

	result <- data %>%
		group_by(State) %>%
		summarise(SD_Killed = sd(`Killed (includes shooter)`, na.rm = TRUE))


	return(result)
}
# Use the function to calculate standard deviation of 'Killed (includes shooter)' for each state
sd_per_state <- calculate_sd_per_state(df)

# Print the standard deviations
print(sd_per_state)
