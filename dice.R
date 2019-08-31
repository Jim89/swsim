library(purrr)
library(ggplot2)
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

`%||%` <- rlang::`%||%`
# Functions ---------------------------------------------------------------
roll_die <- function(die) {
    side <- sample(sides[[die]], 1)
    c(
        # Positive results
        suc = successes[[die]][[side]] %||% 0,
        adv = advantages[[die]][[side]] %||% 0,
        tri = triumphs[[die]][[side]] %||% 0,

        # Negative results
        fail = failures[[die]][[side]] %||% 0,
        thre = threat[[die]][[side]] %||% 0,
        desp = despair[[die]][[side]] %||% 0
    )
}

roll_dice <- function(die, times) {
    if ( times == 0 ) {
        c(
            # Positive results
            suc = 0,
            adv = 0,
            tri = 0,
            # Negative results
            fail = 0,
            thre = 0,
            desp = 0
        )
    } else {
        .out <- purrr::map(seq_len(times), ~roll_die(die))
        tapply(unlist(.out), names(unlist(.out)), sum, default = 0L)
    }
}



roll_pool <- function(b = 0, a = 0, p = 0, s = 0, d = 0, c = 0) {
    dice <- list(b, a, p, s, d, c)
    dice <- purrr::set_names(dice, c("b", "a", "p", "s", "d", "c"))
    dice %>%
        purrr::imap(~roll_dice(.y, .x)) %>%
        purrr::map(t) %>%
        purrr::map_dfr(tibble::as_tibble) %>%
        colSums()
}

process_pool <- function(pool_results) {
    pool <- pool_results
    net_success <- pool[["suc"]] - pool[["fail"]]
    net_adv <- pool[["adv"]] - pool[["thre"]]
    c(
        success = net_success,
        advantage = net_adv,
        triumph = pool[["tri"]],
        despair = pool[["desp"]]
        )
}

simulate_pool <- function(times = 100, b = 0, a = 0, p = 0, s = 0, d = 0, c = 0) {
    times <- seq_len(times)
    pools <- purrr::map(times, ~roll_pool(b, c, p, s, d, c))
    processed <- purrr::map(pools, process_pool)
    transf <- purrr::map(processed, t)
    purrr::map_dfr(transf, tibble::as_tibble)
}

res <- simulate_pool(1000, a = 2, p = 2, s = 1, d = 2, c = 1)
ggplot(res, aes(success, fill = success > 0)) +
    geom_density() +
    geom_vline(xintercept = 0)


outcomes <- res %>%
    dplyr::mutate(
        so = dplyr::if_else(success > 0, "Success", "Failure"),
        sa = dplyr::case_when(
            advantage == 0 ~ "",
            advantage > 0 ~ "with advantage",
            TRUE ~ "with threat"
        ),
        to = dplyr::if_else(triumph <= 0, "", "and triumph"),
        do = dplyr::if_else(despair <= 0, "", "and despair"),
        outcome = stringr::str_trim(stringr::str_squish(paste(so, sa, to, do)))
    ) %>%
    dplyr::count(outcome) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::mutate(outcome = forcats::fct_reorder(outcome, n))

ggplot(outcomes, aes(outcome, n)) +
    geom_col() +
    coord_flip()




