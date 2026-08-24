library(theorytools)
library(dagitty)
my_theory <- read.csv("DAG.csv", stringsAsFactors = FALSE)[, c(2:3)]
names(my_theory) <- c("from", "to")
# Remove redundant statements
my_theory <- my_theory[!duplicated(my_theory), ]
my_theory <- dagitty(
  paste0("dag {",
         paste0(my_theory$from, " -> ", my_theory$to, collapse = "\n"),
         "}")
)
plot(my_theory)
