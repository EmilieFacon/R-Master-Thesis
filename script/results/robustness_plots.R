### INCUMBENCY ADVANTAGE AND FEMALE EXPOSURE - ROBUSTNESS CHECKS ###

## Load functions and data 

source("script/results/rd_results.R")

## Load library

library(xtable)
library(data.table)
# Other libraries loaded when running script

# ---------------------------------------------------------------- #

## Get results for a given row

get_row <- function(df1, df2, status, party1, party2, 
                    outcome_var = "vote share",
                    kernel = "triangular",
                    donut = FALSE,
                    exclude_single_list = FALSE, 
                    placebo = FALSE,
                    na_as_zero = TRUE,
                    coef_plot = FALSE,
                    save_coef_plot = NULL, 
                    graph_name = NULL) {
  
  # Generate the datasets
  
  df <- merge_by_status(df1, df2, status)
  mov <- create_mov_data(df, party1, party2, status)
  
  if (donut == TRUE) {
    mov <- mov %>% 
      filter(!(mov_E01 < 1 & mov_E01 > -1))
  }
  
  if (exclude_single_list == TRUE) {
    mov <- mov %>%
      filter(n_lists_E02.ME != 1)
  }
  
  # Year
  
  year_E01 <- mean(mov$election_E01.ME, na.rm = TRUE)
  year_E02 <- mean(mov$election_E02.ME, na.rm = TRUE)
  details <- max(mov$details.ME, na.rm = TRUE)
  
  default_graph_name <- paste("robustness_plot", year_E02, "-",
                              party1, party2, "-", 
                              outcome_var, "-", details, "-", kernel)
  
  print(default_graph_name)

  # Generate the table for robustness checks 
  if (status == "party_rank") {  
    bw_list <- c(0.5, 1, 1, 2, 5, 5, 10)
    p_list <- c(0, 0, 1, 1, 1, 3, 3)
  } else {
    bw_list <- c(2.5, 5, 2.5, 5, 10, 10, 20)
    p_list <- c(0, 0, 1, 1, 1, 3, 3)
  }
  
  ci_lower <- c() # Confidence intervals 
  ci_upper <- c()
  point_estimate <- c() 
  N <- c()
  bw_p_controls <- c()
  p_value <- c()
  se <- c()
  
  if (year_E01 == 2001 | donut == TRUE | exclude_single_list == TRUE) {
    controls_list <- 0
  } else {
    controls_list <- c(0, 1, 2)
  }
    
  for (controls in controls_list) {
    for (i in seq(1, 7)) {
      
      bw <- bw_list[i]
      p <- p_list[i]
      
      rd_reg <- rd_and_plot(mov, df1, df2, status, party1, party2, 
                            outcome_var, 
                            bandwidth = bw,
                            p = p, 
                            controls = controls,
                            kernel = kernel,
                            na_as_zero = na_as_zero,
                            print_results = FALSE,
                            print_graph = FALSE,
                            save_png = FALSE,
                            save_pdf = FALSE)
      
      ci_lower <- append(ci_lower, rd_reg$ci[1,1])
      ci_upper <- append(ci_upper, rd_reg$ci[1, 2])
      point_estimate <- append(point_estimate, rd_reg$coef[1])
      bw_p_controls <- append(bw_p_controls, 
                              paste0("bw=", bw, "; p=", p, "; c=", controls))
      N <- append(N, sum(rd_reg$N_h))
      p_value <- append(p_value, rd_reg$pv[1])
      se <- append(se, rd_reg$se[1])
    }
  }
  
  if (coef_plot == TRUE) {
    
    ## Graph the parameters and confidence intervals
    
    # Data frame
    
    df_coef <- data.frame(bw_p_controls,
                          p = rep(p_list, length(controls_list)),
                          bw = rep(bw_list, length(controls_list)),
                          controls = factor(rep(controls_list, each = length(bw_list))),
                          point_estimate, ci_lower, ci_upper, N, p_value, se)
    df_coef <- df_coef %>%
      mutate(x_label = paste0("p = ", p, "\nh = ", bw, "\nN = ", N))
    df_coef <- df_coef[order(df_coef$p, df_coef$bw, df_coef$controls), ]
    
    if (length(controls_list) == 3) {
      x <- seq(1, 28)
      x_not <- seq(4, 28, 4)
      x <- x[-x_not]
      df_coef["x"] <- x
    } else {
      x <- seq(1, 7)
      df_coef["x"] <- factor(x)
    }
    
    # Graph title
    
    Outcome_var <- paste(toupper(substring(outcome_var, 1,1)), 
                         substring(outcome_var, 2), sep="", collapse=" ")
    
    if (placebo == TRUE) {
      placebo_title <- "Placebo regressions"
    } else {
      placebo_title <- ""
    }
    
    if (donut == TRUE) {
      donut_title <- "Donut regression (excluding MOV < 1%)"
    } else {
      donut_title <- ""
    }
    
    if (exclude_single_list == TRUE) {
      exclude_title <- "Excluding uncontested elections in 2014"
    } else {
      exclude_title <- ""
    }
    
    if (na_as_zero == FALSE) {
      na_title <- "Excluding observations with missing values"
    } else {
      na_title <- ""
    }
    
    if (status == "party_rank") {
      folder_name <- "incumbency_advantage"
      graph_title <- paste0("Incumbency advantage: ", placebo_title, exclude_title, na_title, "\n", 
                            Outcome_var, " for the ", party1, " in ",
                            year_E02, " (", details, ")")

    } else if (status == "gender_rank") {
      folder_name <- "female_exposure"
      graph_title <- paste0("Female exposure effect: ", placebo_title, donut_title, exclude_title, na_title, "\n",
                            "Female ", outcome_var, " in ", year_E02, " (", details, ")")
      
      if (outcome_var == "number of female candidates" | outcome_var == "proportion of female candidates") {
        graph_title <- paste0("Female exposure effect: ", placebo_title, donut_title, exclude_title, na_title, "\n",
                              Outcome_var, " in ", year_E02, " (", details, ")")
        
      }
    }
    
    # Plot
    
    my_coef_plot <- ggplot(df_coef, aes(x = x, colour = controls))
    
    if (length(controls_list) == 3) {
      my_coef_plot <- my_coef_plot + 
        geom_rect(aes(xmin=4, xmax=8, ymin=-Inf,ymax=Inf), alpha=0.04, color=NA, fill="white") + 
        geom_rect(aes(xmin=12, xmax=16, ymin=-Inf,ymax=Inf), alpha=0.04, color=NA, fill="white") + 
        geom_rect(aes(xmin=20, xmax=24, ymin=-Inf,ymax=Inf), alpha=0.04, color=NA, fill="white") + 
        scale_x_continuous(breaks = seq(2, 28, 4),
                           labels = df_coef[seq(1, 21, 3), 'x_label'])
    } else {
      my_coef_plot <- my_coef_plot +
        scale_x_discrete(labels = df_coef[seq(1, 7), 'x_label'])
    }
    
    my_coef_plot <- my_coef_plot + 
      geom_point(aes(y = point_estimate)) +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
      geom_hline(aes(yintercept = 0)) +
      scale_color_discrete(name = "Controls", 
                           labels = c("(1) Baseline model", 
                                      "(2) Candidate controls", 
                                      "(3) Town and candidate controls")) +
      labs(title = graph_title,
           x = "Model",
           y = "RD estimates")
      
    print(my_coef_plot)
    
    defult_graph_name <- paste("robustness_plot", year_E02, "-",
                               party1, party2, "-", 
                               outcome_var, "-", details, "-", kernel)
    
    
    if (!is.null(save_coef_plot)) {
      
      if (is.null(graph_name)) {
        graph_name <- default_graph_name
      }
      
      ggsave(my_coef_plot,
             filename = paste0("output/", folder_name, "/", graph_name, ".", save_coef_plot),
             width = 7, 
             height = 4.5,
             units = "in")
    }
  }
  
  return(df_coef)
}


# ---------------------------------------------------------------- #

## Get main graphs for incumbency

incumbency_main_graphs <- function() {
  
  # Left - vote share
  
  left_vote_share <- get_row(m2008, m2014_t1, "party_rank", "Left", "Right", "vote share", 
                            kernel = "triangular", donut = FALSE, 
                            coef_plot = TRUE, save_coef_plot = "pdf", 
                            graph_name = "incumbency_main")
  
  # Rigth - vote share
  
  right_vote_share <- get_row(m2008, m2014_t1, "party_rank", "Right", "Left", "vote share", 
                             kernel = "triangular", donut = FALSE, 
                             coef_plot = TRUE, save_coef_plot = "pdf",
                             graph_name = "incumbency_right")
  
  # Left - prob of election
  
  left_prob_of_elect <- get_row(m2008, m2014_t1, "party_rank", "Left", "Right", "probability of election", 
                                kernel = "triangular", donut = FALSE, 
                                coef_plot = TRUE, save_coef_plot = "pdf", 
                                graph_name = "incumbency_prob_of_elect")
  
  # Left - vote share - 2008
  
  left_2008 <- get_row(m2001, m2008_t1, "party_rank", "Left", "Right", "vote share", 
                       kernel = "triangular", donut = FALSE, 
                       coef_plot = TRUE, save_coef_plot = "pdf",
                       graph_name = "incumbency_2008")
  
  # Left - two-party vote share
  
  two_party <- get_row(m2008, m2014_t1, "party_rank", "Left", "Right", "two-party vote share", 
                       kernel = "triangular", donut = FALSE, 
                       coef_plot = TRUE, save_coef_plot = "pdf",
                       graph_name = "incumbency_two_party_vote_share")
  
  
  # Placebo
  
  placebo <- get_row(m2014_t1, m2008, "party_rank", "Left", "Right", "vote share", 
                 kernel = "triangular", donut = FALSE, 
                 placebo = TRUE,
                 coef_plot = TRUE, save_coef_plot = "pdf", 
                 graph_name = "incumbency_placebo")
  
  # NAs as Nas
  
  drop_nas_incumbency <- get_row(m2008, m2014_t1, "party_rank", "Left", "Right", "vote share", 
                      kernel = "triangular", donut = FALSE, 
                      na_as_zero = FALSE,
                      coef_plot = TRUE, save_coef_plot = "pdf", 
                      graph_name = "incumbency_exclude_missing")
  
  # Final results
  
  final_round <- get_row(m2008, m2014, "party_rank", "Left", "Right", "vote share", 
                     kernel = "triangular", donut = FALSE, 
                     placebo = FALSE,
                     coef_plot = TRUE, save_coef_plot = "pdf", 
                     graph_name = "incumbency_final_results")
  
  # Final results - prob of elect
  
  final_round <- get_row(m2008, m2014, "party_rank", "Left", "Right", "probability of election", 
                         kernel = "triangular", donut = FALSE, 
                         placebo = FALSE,
                         coef_plot = TRUE, save_coef_plot = "png", 
                         graph_name = "incumbency_final_prob_of_elect")
  
  # Exclude uncontested elections
  
  exclude_uncontested <- get_row(m2008, m2014_t1, "party_rank", "Left", "Right", "vote share", 
                                 kernel = "triangular", donut = FALSE, 
                                 exclude_single_list = TRUE,
                                 coef_plot = TRUE, save_coef_plot = "pdf",
                                 graph_name = "incumbency_exclude_uncontested")
  
  # Incumbency - vote share - uniform 
  
  res2 <- get_row(m2008, m2014_t1, "party_rank", "Left", "Right", "vote share", 
                  kernel = "uniform", donut = FALSE, 
                  coef_plot = TRUE, save_coef_plot = "pdf", 
                  graph_name = "incumbency_uniform")
}

# ---------------------------------------------------------------- #

## Get main graphs for female exposure effect

female_main_graphs <- function() {
  
  # Vote share
  
  female_main <- get_row(m2008, m2014_t1, "gender_rank", "F", "M", "vote share", 
                         kernel = "triangular", donut = FALSE, 
                         coef_plot = TRUE, save_coef_plot = "pdf",
                         graph_name = "female_main")
  
  # Prob of elect 
  
  prob_of_elect_round_1 <- get_row(m2008, m2014_t1, "gender_rank", "F", "M", "probability of election", 
                           kernel = "triangular", donut = FALSE, 
                           coef_plot = TRUE, save_coef_plot = "pdf",
                           graph_name = "female_prob_of_elect")
  
  # Placebo 
  
  placebo <- get_row(m2014_t1, m2008, "gender_rank", "F", "M", "vote share", 
                     kernel = "triangular", donut = FALSE, 
                     placebo = TRUE,
                     coef_plot = TRUE, save_coef_plot = "pdf",
                     graph_name = "female_placebo")
  
  # Keep NAs
  
  drop_nas <- get_row(m2008, m2014_t1, "gender_rank", "F", "M", "vote share", 
                      kernel = "triangular", donut = FALSE, 
                      na_as_zero = FALSE,
                      coef_plot = TRUE, save_coef_plot = "pdf",
                      graph_name = "female_exclude_missing")
  
  # Keep NAs - Prob of elect
  
  drop_nas_prob_of_elect <- get_row(m2008, m2014_t1, "gender_rank", "F", "M", "probability of election", 
                      kernel = "triangular", donut = FALSE, 
                      na_as_zero = FALSE,
                      coef_plot = TRUE, save_coef_plot = "png",
                      graph_name = "female_exclude_missing_prob_of_elect")
  
  # Keep NAs - Vote share final round
  
  drop_nas_final_round <- get_row(m2008, m2014, "gender_rank", "F", "M", "vote share", 
                                    kernel = "triangular", donut = FALSE, 
                                    na_as_zero = FALSE,
                                    coef_plot = TRUE, save_coef_plot = "png",
                                    graph_name = "female_exclude_missing_final")
  
  # Keep NAs - Prob of elect final round
  
  drop_nas_final_round_prob_elect <- get_row(m2008, m2014, "gender_rank", "F", "M", "probability of election", 
                                    kernel = "triangular", donut = FALSE, 
                                    na_as_zero = FALSE,
                                    coef_plot = TRUE, save_coef_plot = "png",
                                    graph_name = "female_exclude_missing_prob_of_elect_final")
  
  # Vote share - uniform kernel
  
  res <- get_row(m2008, m2014_t1, "gender_rank", "F", "M", "vote share", 
                 kernel = "uniform", donut = FALSE, 
                 coef_plot = TRUE, save_coef_plot = "png",
                 graph_name = "female_uniform")
  
  # Donut 
  
  female_donut <- get_row(m2008, m2014_t1, "gender_rank", "F", "M", "vote share", 
                         kernel = "triangular", donut = TRUE, 
                         coef_plot = TRUE, save_coef_plot = "pdf",
                         graph_name = "female_donut")
  
  # Final-round results
  
  final_round <- get_row(m2008, m2014, "gender_rank", "F", "M", "vote share", 
                         kernel = "triangular", donut = FALSE, 
                         coef_plot = TRUE, save_coef_plot = "pdf",
                         graph_name = "female_final_round")
  
  # Final-round prob of elect
  
  final_round_prob_of_elect <- get_row(m2008, m2014, "gender_rank", "F", "M", "probability of election", 
                         kernel = "triangular", donut = FALSE, 
                         coef_plot = TRUE, save_coef_plot = "pdf",
                         graph_name = "female_final_prob_of_elect")
  
  # NAs as NAs
  
  drop_nas <- get_row(m2008, m2014_t1, "gender_rank", "F", "M", "vote share", 
                      kernel = "triangular", donut = FALSE, 
                      na_as_zero = FALSE,
                      coef_plot = TRUE, save_coef_plot = "png", 
                      graph_name = "female_drop_nas")
  
  
  # Exclude uncontested elections
  
  exclude_uncontested <- get_row(m2008, m2014_t1, "gender_rank", "F", "M", "vote share", 
                                 kernel = "triangular", donut = FALSE, 
                                 exclude_single_list = TRUE,
                                 coef_plot = TRUE, save_coef_plot = "pdf",
                                 graph_name = "female_exclude_uncontested")
  
  # Number of female candidates
  
  n_female <- get_row(m2008, m2014_t1, "gender_rank", "F", "M", "number of female candidates", 
                      kernel = "triangular", donut = FALSE, 
                      na_as_zero = TRUE, 
                      coef_plot = TRUE, save_coef_plot = "pdf",
                      graph_name = "female_number_of_candidates")
  
  # Proportion of female candidates
  
  prop_female <- get_row(m2008, m2014, "gender_rank", "F", "M", "proportion of female candidates", 
                         kernel = "triangular", donut = FALSE, 
                         na_as_zero = TRUE, 
                         coef_plot = TRUE, save_coef_plot = "pdf",
                         graph_name = "female_proportion_of_candidates")
}

# ---------------------------------------------------------------- #

# Call the functions

if (FALSE) {
  incumbency_main_graphs()
  female_main_graphs()
}

# ---------------------------------------------------------------- #

# Other graphs for incumbency
  
if (FALSE) {
  
  ## Incumbency - Right - 2014 ##

  # Incumbency - vote share - uniform 
  
  res2R <- get_row(m2008, m2014_t1, "party_rank", "Right", "Left", "vote share", 
                  kernel = "uniform", donut = FALSE, 
                  coef_plot = TRUE, save_coef_plot = "png")
  
  # Incumbency - prob of election - triangular 
  
  res3R <- get_row(m2008, m2014_t1, "party_rank", "Right", "Left", "probability of election", 
                   kernel = "triangular", donut = FALSE, 
                   coef_plot = TRUE, save_coef_plot = "png")
  
  
  ## Left - 2001/2008 ##
  
  # Incumbency - vote share - uniform 
  
  res <- get_row(m2001, m2008_t1, "party_rank", "Left", "Right", "vote share", 
                  kernel = "uniform", donut = FALSE, 
                  coef_plot = TRUE, save_coef_plot = "png")
  
  # Incumbency - prob of election - triangular 
  
  res <- get_row(m2001, m2008_t1, "party_rank", "Left", "Right", "probability of election", 
                  kernel = "triangular", donut = FALSE, 
                  coef_plot = TRUE, save_coef_plot = "png")
  
  
  ## Right - 2001/2008 ##
  
  # Incumbency - vote share - triangular
  
  res1_2001 <- get_row(m2001, m2008_t1, "party_rank", "Right", "Left", "vote share", 
                       kernel = "triangular", donut = FALSE, 
                       coef_plot = TRUE, save_coef_plot = "png")
  
  # Incumbency - vote share - uniform 
  
  res2_2001 <- get_row(m2001, m2008_t1, "party_rank", "Right", "Left", "vote share", 
                       kernel = "uniform", donut = FALSE, 
                       coef_plot = TRUE, save_coef_plot = "png")
  
  # Incumbency - prob of victory - triangular 
  
  res3_2001 <- get_row(m2001, m2008_t1, "party_rank", "Right", "Left", "probability of election", 
                       kernel = "triangular", donut = FALSE, 
                       coef_plot = TRUE, save_coef_plot = "png")
  
  
  ## Right - two-party vote share - Mirror image ##

  res <- get_row(m2008, m2014_t1, "party_rank", "Right", "Left", "two-party vote share", 
                 kernel = "triangular", donut = FALSE, 
                 coef_plot = TRUE, save_coef_plot = "png")
  
  res <- get_row(m2001, m2008_t1, "party_rank", "Left", "Right", "two-party vote share", 
                 kernel = "triangular", donut = FALSE, 
                 coef_plot = TRUE, save_coef_plot = "png")
  
  
  ## Persistence
  
  res <- get_row(m2001, m2014_t1, "party_rank", "Left", "Right", "vote share", 
                  kernel = "triangular", donut = FALSE, 
                  coef_plot = TRUE, save_coef_plot = "png", 
                  graph_name = "robustness_plot persistence")
  
  res <- get_row(m2001, m2014_t1, "party_rank", "Right", "Left", "vote share", 
                 kernel = "triangular", donut = FALSE, 
                 coef_plot = TRUE, save_coef_plot = "png", 
                 graph_name = "robustness_plot persistence right")

  
  
  ### FEMALE ###
  
  # Two-gender vote share 
  
  res <- get_row(m2008, m2014_t1, "gender_rank", "F", "M", "two-party vote share", 
                 kernel = "triangular", donut = FALSE, 
                 coef_plot = TRUE, save_coef_plot = "png",
                 graph_name = "female_two_party_vote_share")
  
  # Placebo
  
  res <- get_row(m2008, m2014_t1, "gender_rank", "M", "F", "vote share", 
                 kernel = "triangular", donut = FALSE, 
                 coef_plot = TRUE, save_coef_plot = "png",
                 graph_name = "female_m_f")
  
}
