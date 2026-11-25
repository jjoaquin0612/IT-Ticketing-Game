Ticket Triage: an IT help desk game in R
Authors: Jack Joaquin, Imran Pasha
Course: MATH 356 Statistical Programming in R, Fall 2026

DESCRIPTION
You are the IT help desk. A queue of tickets is waiting, and each has an effort
(minutes to fix) and an SLA deadline. Choose which ticket to work on next. Resolve
tickets before their deadlines to earn points; late tickets cost points.

This project combines two directions:
- creating two functions called generate_tickets() and prioritize_tickets()
- Ticket Triage game, built on those two functions


HOW TO RUN
1. Open R (or RStudio) and set the working directory to this folder.
2. Playable version):
     source("R/tickets.R")
     source("R/triage_game.R")
     play_triage() in console
3. Scripted example (no input needed, same output every time):
     source("demo.R")

ACTIONS
<ticket ID>   work on that ticket (e.g., 3 or "work 3"); the clock advances by its effort
hint          suggests the next ticket via prioritize_tickets() (costs 2 points)
restart       start a new shift
quit          leave the game

RULES AND SCORING
- Resolved on time (clock <= deadline): + points by priority
    Low 10, Medium 20, High 30, Critical 50
- Resolved late (SLA breach): - half of those points
- The shift ends when no tickets are left open.
- Perfect shift (no breaches): +20 bonus.
- Invalid actions are rejected and do not change the game.

HELPER FUNCTIONS
generate_tickets(n, seed)       fake ticket queue: id, category, priority, effort, deadline
prioritize_tickets(tickets, now) ranks open tickets by urgency = weight / minutes left;
                                 tickets that can no longer be saved are listed last


FILES
R/tickets.R        generate_tickets, prioritize_tickets, shared settings
R/triage_game.R    game state, turns, play loop, strategies, simulation
demo.R             scripted example plus strategy comparison
