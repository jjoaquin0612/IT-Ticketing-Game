# demo.R
# Scripted Ticket Triage example: supplied actions + fixed seed, so
# the output is identical every run and needs no keyboard input.
# Run from the project folder:  source("demo.R")

source("Tickets.R")
source("TriageGame.R")

# 1. Show the helper functions on their own
tix <- generate_tickets(n = 8, seed = 356)
cat("Generated tickets:\n")
print(tix, row.names = FALSE)
cat("\nPrioritized (now = 0):\n")
print(prioritize_tickets(tix, now = 0), row.names = FALSE)

# 2. Scripted game: ask for a hint, work tickets in a chosen order, and include
#    one invalid action on purpose (ticket IDs are 1..8)
scripted_actions <- c("hint", "1", "2", "oops", "3", "4", "5", "6", "7", "8")
cat("\n\n===== Scripted game =====\n")
player <- play_triage(actions = scripted_actions, seed = 356)

# 3. Compare with the automatic "priority" strategy on the same ticket queue
cat("\n\n===== Automatic priority strategy, same tickets =====\n")
auto <- auto_play(new_triage_game(tickets = tix), "priority")
cat("Scripted player score:", player$score, "| breaches:", player$breaches, "\n")
cat("Priority strategy score:", auto$score, "| breaches:", auto$breaches, "\n")

# 4. Simulation: which strategy does best over many random queues?
res <- compare_strategies(n_games = 500, n_tickets = 8, seed = 356)
print(aggregate(cbind(score, breaches) ~ strategy, data = res, FUN = mean))
plot_strategies(res)

# To play yourself in an interactive R session:
#   play_triage()