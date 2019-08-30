# Set up dice sides
sides <- list(
    # Positive dice - boost, ability, proficiency
    "b" = 6,
    "a" = 8,
    "p" = 12,
    # Negative dice - setback, difficulty, challenge
    "s" = 6,
    "d" = 8,
    "c" = 12
)

# Define positive dice ---------------------------------------------------------
successes <- list(
    # Positive dice only - boost, ability, proficiency
    "b" = c(0, 0 , 1, 1, 0, 0),
    "a" = c(0, 1, 1, 2, 0, 0, 1, 0),
    "p" = c(0, 1, 1, 2, 2, 0, 1, 1, 1, 0, 0, 1)
)

advantages <- list(
    # Positive dice only - boost, ability, proficiency
    "b" = c(0, 0, 0, 1, 2, 1),
    "a" = c(0, 0, 0, 0, 1, 1, 1, 2),
    "p" = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 0)
)

triumphs <- list("p" = c(rep(0, 11), 1))

# Define positive dice ---------------------------------------------------------
failures <- list(
    "s" = c(0, 0 , 1, 1, 0, 0),
    "d" = c(0, 1, 2, 0, 0, 0, 0, 1),
    "c" = c(0, 1, 1, 2, 2, 0, 0, 1, 1, 0, 0, 1)
)

threat <- list(
    "s" = c(0, 0 , 0, 0, 1, 1),
    "d" = c(0, 0, 0, 1, 1, 1, 2, 1),
    "c" = c(rep(0, 5), rep(1, 4), 2, 2, 0)
)

despair <- list("c" = c(rep(0, 11), 1))


# Functions ---------------------------------------------------------------
roll_die <- function(die) {
    side <- sample(sides[[die]], 1)
    c(
        # Positive results
        .suc = successes[[die]][[side]],
        .adv = advantages[[die]][[side]],
        .tri = triumphs[[die]][[side]],

        # Negative results
        .fail = failures[[die]][[side]],
        .thre = threat[[die]][[side]],
        .desp = despair[[die]][[side]]
    )
}




