library(dplyr)


# returns vector of balanced selections according to both rankings
split <- function(df) {
	# returns vector of balanced selections according to {colname} ranking
	split.by <- function(df, colname) {
		# returns "gaps" when players {p} vs {-p} given {skill} vector
		gaps <- function(skill, p) {
			# calculates gap between teams with "skill" vectors {x} and {y}
			gap <- function(x, y) sum(x - y)
			
			m1 <- combn(p, 5)
			m2 <- combn(setdiff(1:length(skill), p), 5)
			
			vs <- expand.grid(
				1:ncol(m1),
				1:ncol(m2),
				stringsAsFactors = FALSE
			)
			
			mapply(
				function(i, j) gap(skill[m1[, i]], skill[m2[, j]]),
				vs$Var1,
				vs$Var2
			)
		}

		df <- do.call(
			rbind,
			apply(
				combn(1:nrow(df), nrow(df) %/% 2),
				2,
				function(x) data.frame(
					team = paste(x, collapse = " "),
					mean.gap = mean(gaps(df[[colname]], x))
				)
			)
		)

		df %>% filter(abs(mean.gap) == min(abs(df$mean.gap))) %>% .$team
	}

	# players & their skill
	df <- merge(df %>% filter(playing), read.csv("data.csv"))
	
	# selections that are balanced according to both rankings
	intersect(split.by(df, "mrank"), split.by(df, "erank"))
}
