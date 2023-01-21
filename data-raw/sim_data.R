library('data.table')

# super naive
set.seed(1789)
outputDir = 'R'

########################################
# ConnectEd

# v = data.table(
#   level_id = 0:4,
#   level_name = c(
#     'Beginner', 'Addition', 'Subtraction', 'Multiplication', 'Division'))
# fwrite(v, file.path(outputDir, 'connected_levels.csv'))

v = fread(file.path(outputDir, 'connected_levels.csv'))

n_rounds = 5L
range_students_per_round = c(50, 100)
n_arms_per_round = 2L
delta_levs = 0:2

n_students_per_round = round(runif(
  n_rounds, range_students_per_round[1L], range_students_per_round[2L]))

d = data.table(
  student_id = 1:sum(n_students_per_round),
  round_id = rep(1:n_rounds, n_students_per_round))

d[, arm_id := rep_len(1:n_arms_per_round, .N)]
d[, start_level := v$level_id[sample.int(nrow(v), .N, replace = TRUE)]]
d[, delta_level := delta_levs[
  sample.int(length(delta_levs), .N, replace = TRUE)]]
d[, end_level := pmin(max(v$level_id), start_level + delta_level)]

r = data.table(round_id = 1:n_rounds)
r[, round_desc := stringi::stri_rand_lipsum(.N, FALSE)]
r[, round_desc := substr(round_desc, 1L, 150L)]
fwrite(r, file.path(outputDir, 'connected_rounds.csv'))

a = unique(d[, .(round_id, arm_id)])
setorder(a)
a[, arm_name := glue::glue('Arm {round_id}-{arm_id}', .envir = .SD)]

fwrite(d, file.path(outputDir, 'connected_data.csv'))
fwrite(a, file.path(outputDir, 'connected_arms.csv'))
