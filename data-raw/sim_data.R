library('data.table')

# super naive

set.seed(1789)

n_rounds = 7L
range_students_per_round = c(50, 100)
n_arms_per_round = 2L
levs = 0:4 # add, substract, multiply, divide
delta_levs = 0:2

n_students_per_round = round(runif(
  n_rounds, range_students_per_round[1L], range_students_per_round[2L]))

d = data.table(
  student_id = 1:sum(n_students_per_round),
  round_id = rep(1:n_rounds, n_students_per_round))

d[, arm_id := rep_len(1:n_arms_per_round, .N)]
d[, start_level := levs[sample.int(length(levs), .N, replace = TRUE)]]
d[, delta_level := delta_levs[sample.int(length(delta_levs), .N, replace = TRUE)]]
d[, end_level := pmin(max(levs), start_level + delta_level)]

fwrite(d, 'sim_data.csv')
