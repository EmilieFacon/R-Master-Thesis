### GET RD PLOTS ###

## Load functions and data 

source("script/results/rd_results.R")

# ---------------------------------------------------------------- #

## Get RD plots

get_rd_plots <- function(df1, df2, status, party1, party2,
                         bandwidth, graph_name) {
  
  df <- merge_by_status(df1, df2, status)
  mov <- create_mov_data(df, party1, party2, status)
  
  rd_reg <- rd_and_plot(mov, df1, df2, status, party1, party2, 
                        outcome_var = "vote share",
                        bandwidth = bandwidth,
                        p = 1,
                        controls = 0,
                        kernel = "triangular",
                        print_results = TRUE,
                        print_graph = TRUE,
                        save_png = FALSE,
                        save_pdf = TRUE,
                        graph_name = graph_name)
}

# Incumbency

get_rd_plots(m2008, m2014_t1, "party_rank", "Left", "Right", bandwidth = 5,
             graph_name = "incumbency_rd_plot")

get_rd_plots(m2014_t1, m2008, "party_rank", "Left", "Right", bandwidth = 5,
             graph_name = "incumbency_rd_plot_placebo")

# Female

get_rd_plots(m2008, m2014_t1, "gender_rank", "F", "M", bandwidth = 5,
             graph_name = "female_rd_plot")

get_rd_plots(m2014_t1, m2008, "gender_rank", "F", "M", bandwidth = 5,
             graph_name = "female_rd_plot_placebo")
