# triage_game.R
# Ticket Triage: you are the IT help desk. A queue of tickets is waiting, each with
# an effort (minutes to fix) and an SLA deadline. Choose which ticket to work on;
# the clock advances by that ticket's effort. Resolve tickets before their
# deadlines to earn points; late tickets cost you points.
#
# Game state is a list with:
#   tickets  - data frame from generate_tickets(); status is open/resolved/breached
#   clock    - minutes elapsed
#   score    - current score
#   breaches - number of tickets resolved after their deadline
#   status   - "playing" or "finished"
#   message  - feedback for the player

# Requires R/tickets.R to be sourced first (generate_tickets, prioritize_tickets).

# ---- Setup ------------------------------------------------------------------

new_triage_game <- function(n_tickets = 8, seed = NULL, tickets = NULL) {
  if (is.null(tickets)) tickets <- generate_tickets(n_tickets, seed)
  list(
    tickets  = tickets,
    clock    = 0,
    score    = 0,
    breaches = 0,
    status   = "playing",
    message  = ""
  )
}

# ---- One turn ---------------------------------------------------------------

# Actions:
#   a ticket ID (e.g. "3" or "work 3") - work on that ticket
#   "hint"                             - suggests the next ticket (costs 2 points)
# Returns the new state.
take_triage_turn <- function(state, action) {
  action <- tolower(trimws(action))
  
  if (state$status != "playing") {
    state$message <- "The shift is over. Type 'restart' to play again."
    return(state)
  }
  
  if (action == "hint") {
    top <- prioritize_tickets(state$tickets, state$clock)
    state$score   <- state$score - 2
    state$message <- paste0("Hint (-2 pts): work on ticket #", top$id[1], " next.")
    return(state)
  }
  
  id <- suppressWarnings(as.integer(sub("^work\\s+", "", action)))
  open_ids <- state$tickets$id[state$tickets$status == "open"]
  if (is.na(id) || !(id %in% open_ids)) {
    state$message <- "Invalid action. Enter an open ticket ID, 'hint', 'restart', or 'quit'."
    return(state)
  }
  
  idx    <- which(state$tickets$id == id)
  ticket <- state$tickets[idx, ]
  state$clock <- state$clock + ticket$effort
  points <- priority_points[[ticket$priority]]
  
  if (state$clock <= ticket$deadline) {
    state$score <- state$score + points
    state$tickets$status[idx] <- "resolved"
    state$message <- paste0("Ticket #", id, " resolved on time (+", points, " pts).")
  } else {
    penalty <- points %/% 2
    state$score    <- state$score - penalty
    state$breaches <- state$breaches + 1
    state$tickets$status[idx] <- "breached"
    state$message <- paste0("Ticket #", id, " resolved LATE - SLA breach (-", penalty, " pts).")
  }
  
  # Shift ends when no tickets are left open
  if (!any(state$tickets$status == "open")) {
    state$status <- "finished"
    if (state$breaches == 0) {
      state$score <- state$score + 20
      state$message <- paste0(state$message, " Perfect shift: no breaches (+20 bonus).")
    }
  }
  state
}

# ---- Display ----------------------------------------------------------------

print_queue <- function(state) {
  cat(sprintf("\nClock: %d min | Score: %d | Breaches: %d\n",
              as.integer(state$clock), as.integer(state$score),
              as.integer(state$breaches)))
  open <- state$tickets[state$tickets$status == "open", , drop = FALSE]
  if (nrow(open) == 0) return(invisible(state))
  
  open$time_left <- open$deadline - state$clock
  show <- open[order(open$deadline),
               c("id", "category", "priority", "effort", "deadline", "time_left")]
  names(show) <- c("ID", "Category", "Priority", "Effort", "Deadline", "TimeLeft")
  print(show, row.names = FALSE)
  invisible(state)
}

# ---- Game loop --------------------------------------------------------------

# Interactive: play_triage()
# Scripted:    play_triage(actions = c("hint", "3", "1", "2"), seed = 356)
# Commands:    ticket ID, "hint", "restart", "quit"
play_triage <- function(actions = NULL, seed = NULL, n_tickets = 8) {
  scripted <- !is.null(actions)
  if (!scripted && !interactive()) {
    stop("Interactive play needs an interactive R session. ",
         "Supply `actions` to run a scripted game.")
  }
  
  state <- new_triage_game(n_tickets, seed)
  i <- 0
  
  repeat {
    print_queue(state)
    if (nzchar(state$message)) cat(state$message, "\n")
    
    if (state$status == "finished") {
      cat(sprintf("\nShift over! Final score: %d with %d SLA breach(es).\n",
                  as.integer(state$score), as.integer(state$breaches)))
      break
    }
    
    if (scripted) {
      i <- i + 1
      if (i > length(actions)) {
        cat("\n(End of scripted actions.)\n")
        break
      }
      input <- actions[i]
      cat("> ", input, "\n", sep = "")
    } else {
      input <- readline("Ticket ID, hint, restart, or quit: ")
    }
    
    input <- tolower(trimws(input))
    if (input == "quit") {
      cat("Leaving the shift early. Score:", state$score, "\n")
      break
    }
    if (input == "restart") {
      state <- new_triage_game(n_tickets)
      state$message <- "New shift started."
      next
    }
    state <- take_triage_turn(state, input)
  }
  
  invisible(state)
}

# ---- Strategies and simulation (statistics angle) ---------------------------

# Plays a whole game automatically with a fixed strategy:
#   "priority" - always take the top ticket from prioritize_tickets()
#   "deadline" - earliest deadline first
#   "random"   - pick any open ticket at random
auto_play <- function(state, strategy = c("priority", "deadline", "random")) {
  strategy <- match.arg(strategy)
  while (state$status == "playing") {
    open <- state$tickets[state$tickets$status == "open", , drop = FALSE]
    id <- switch(strategy,
                 priority = prioritize_tickets(state$tickets, state$clock)$id[1],
                 deadline = open$id[which.min(open$deadline)],
                 random   = if (nrow(open) == 1) open$id else sample(open$id, 1)
    )
    state <- take_triage_turn(state, as.character(id))
  }
  state
}

# Plays the SAME random ticket queues with each strategy, so scores are comparable.
# Returns a data frame with columns: game, strategy, score, breaches.
compare_strategies <- function(n_games = 500, n_tickets = 8, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  strategies <- c("priority", "deadline", "random")
  rows <- list()
  for (g in seq_len(n_games)) {
    base <- new_triage_game(n_tickets)
    for (s in strategies) {
      end <- auto_play(base, s)
      rows[[length(rows) + 1]] <- data.frame(
        game = g, strategy = s, score = end$score, breaches = end$breaches,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}
