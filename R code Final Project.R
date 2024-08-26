# Load the data
load("data/shooting_data.rda")
df<- shooting_data

library(gtsummary)
#create a table of dicriptive statistics
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

library(ggplot2)

# Histogram of 'Killed (includes shooter)' using ggplot2
ggplot(df, aes(x = `Killed (includes shooter)`)) +
	geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
	labs(title = "Histogram of Killed (includes shooter)",
			 x = "Number of People Killed (including shooter)",
			 y = "Frequency") +
	theme_minimal()
