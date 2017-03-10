library(dplyr)


# returns best split(s) according to both rankings (as list of integer vectors)
split <- function(df) {
	# returns best split(s) according to {colname} ranking (as vector of strings)
	split.by <- function(df, colname) {
		df <- do.call(
			rbind,
			apply(
				# loop through all possible splits
				combn(1:nrow(df), nrow(df) %/% 2), 2,
				# output: team | difference in average skill
				function(x) data.frame(
					team = paste(x, collapse = " "),
					mean.gap = mean(df[x, colname]) - mean(df[-x, colname]) #mean(gaps(df[[colname]], x))
				)
			)
		)

		# return best split(s)
		df %>% filter(abs(mean.gap) == min(abs(df$mean.gap))) %>% .$team
	}

	# players & their skill
	df <- merge(df, read.csv("data.csv"))
	
	# selections that are balanced according to both rankings
	lapply(
		strsplit(intersect(split.by(df, "mrank"), split.by(df, "erank")), " "),
		as.integer
	)
}
