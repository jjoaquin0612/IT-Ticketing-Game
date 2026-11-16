# tickets.R
# Two helper functions for IT ticketing (Option 1), used by the triage game (Option 2):
#   generate_tickets()    - make a realistic fake ticket queue
#   prioritize_tickets()  - rank open tickets by urgency

# ---- Settings shared by the whole project -----------------------------------

priority_levels   <- c("Low", "Medium", "High", "Critical")
priority_points   <- c(Low = 10, Medium = 20, High = 30, Critical = 50)  # game points
priority_weight   <- c(Low = 1,  Medium = 2,  High = 3,  Critical = 5)   # used for urgency
sla_minutes       <- c(Low = 180, Medium = 120, High = 75, Critical = 45) # base SLA window
ticket_categories <- c("Password reset", "Network", "Hardware",
                       "Software install", "Email", "Printer")

# ---- Function 1: generate_tickets -------------------------------------------

# Returns a data frame with one row per ticket:
#   id        - 1..n
#   category  - type of issue
#   priority  - Low / Medium / High / Critical
#   effort    - minutes needed to resolve the ticket
#   deadline  - SLA deadline, in minutes from the start of the shift
#   status    - "open" (the game later changes this to "resolved" or "breached")
generate_tickets <- function(n = 8, seed = NULL) {
  stopifnot(is.numeric(n), length(n) == 1, n >= 1)
  if (!is.null(seed)) set.seed(seed)
  
  category <- sample(ticket_categories, n, replace = TRUE)
  priority <- sample(priority_levels, n, replace = TRUE,
                     prob = c(0.35, 0.35, 0.20, 0.10))
  effort   <- sample(c(5, 10, 15, 20, 30, 45), n, replace = TRUE)
  
  # Higher priority = tighter deadline; a little random slack on top
  deadline <- unname(sla_minutes[priority]) + sample(c(0, 15, 30), n, replace = TRUE)
  deadline <- pmax(deadline, effort)  # every ticket is at least possible on its own
  
  data.frame(
    id       = seq_len(n),
    category = category,
    priority = priority,
    effort   = effort,
    deadline = deadline,
    status   = "open",
    stringsAsFactors = FALSE
  )
}

# ---- Function 2: prioritize_tickets -----------------------------------------

# Ranks the OPEN tickets, most urgent first.
#   urgency = priority weight / minutes left until the deadline
# Tickets that can no longer be finished on time (effort > time left) are listed
# after all the ones that can still be saved, since they will breach either way.
# `now` is the current clock time in minutes.
# Returns the open tickets plus time_left, can_make_it, and urgency columns.
prioritize_tickets <- function(tickets, now = 0) {
  needed <- c("id", "priority", "effort", "deadline")
  if (!is.data.frame(tickets) || !all(needed %in% names(tickets))) {
    stop("`tickets` must be a data frame with columns: ",
         paste(needed, collapse = ", "))
  }
  if (!all(tickets$priority %in% priority_levels)) {
    stop("`priority` must be one of: ", paste(priority_levels, collapse = ", "))
  }
  
  if ("status" %in% names(tickets)) {
    tickets <- tickets[tickets$status == "open", , drop = FALSE]
  }
  if (nrow(tickets) == 0) return(tickets)
  
  tickets$time_left   <- tickets$deadline - now
  tickets$can_make_it <- tickets$effort <= tickets$time_left
  tickets$urgency     <- unname(priority_weight[tickets$priority]) /
    pmax(tickets$time_left, 1)
  
  # Savable tickets first (by urgency), then the ones already doomed to breach
  ranked <- tickets[order(!tickets$can_make_it, -tickets$urgency, tickets$deadline), ,
                    drop = FALSE]
  rownames(ranked) <- NULL
  ranked
}
