# Copyright Christophe Coupé (Linguistics Department, The University of Hong Kong, Hong Kong) (ccoupe@hku.hk)
library(tidyverse)
library(data.table)
library(R6)
library(glue)

source("BALI/rules - Super class.R")
source("BALI/rules - Rules for groups.R")
source("BALI/rules - Rules for rows.R")


GA <- R6Class("GA",
  private = list(
    env = NULL,

    genome_selection_ratio = 0.2,
    selected_genomes_nb = 20,
    changes_nb = 5,
    
    rules = vector(mode = "list", 1),
    rules_nb = 0,
    
    latest_step = 0,
    rule_scores = NULL,
    normalization_scores = NULL,
    scores = NULL,
    
    optimized_var = c(),
    
    create_groups = function() {
      e <- private$env
      v <- c()
      n <- nrow(e$df)
      
      counter <- 0
      for (i in 1:length(e$groups))
        v <- c(v, rep(i, e$groups[i]))
      v <- c(v, rep(0, n - sum(e$groups)))
      v <- sample(v, n, replace = F)
      
      return(v)
    },
    
    genome_mutate = function(v) {
      nb_changes <- sample(1:private$changes_nb, 1)
      
      indexes <- which(v != 0)
      for (i in 1:nb_changes) {
        index1 <- sample(indexes, 1, replace = F) # We pick one index from the groups
        index2 <- sample(1:length(v), 1, replace = F) # We pick an element from the whole dataset
        v[c(index1, index2)] <- v[c(index2, index1)]
      }
      return (v)
    }
  
  ),
  
  public = list(
    initialize = function(df, groups) {
      stopifnot("You want to create groups which total number of elements exceeds the number of rows in your table" = sum(groups) <= nrow(df))
      
      private$env <- new.env()
      private$env$df <- copy(df) %>% setDT()
      private$env$df[, obs_id := as.factor(1:nrow(df))]
      
      private$env$groups <- groups
      private$env$genomes <- NULL
      private$env$genomes_nb <- 100
    },
  
    get_genome = function(i) {
      return (private$env$genomes[, ..i])
    },

    create_genomes = function() {
      e <- private$env
      n <- nrow(e$df)
      e$genomes <- tibble(id = 1:n)
      
      for (i in 1:e$genomes_nb)
        e$genomes <- e$genomes %>% mutate("g.{i}" := private$create_groups())
      
      genome_names <- paste0("g.", 1:e$genomes_nb)
      e$genomes <- e$genomes %>% select(-id) %>% setDT() 
      e$genome_names <- paste0("g.", 1:e$genomes_nb)
      
      return ()
    },
    
    # On change pour créer les règles à l'extérieur et les ajouter avec contrôle
    
    add_rule = function(type_of_rule, variable_name, target_groups, weight, element = NULL) {
      
      all_group_rules <- c(
        "maximize_values_in_groups", "minimize_values_in_groups",
        "maximize_variance_in_groups", "minimize_variance_in_groups",
        "equalize_means_across_groups", "differentiate_means_across_groups",  
        "equalize_variance_across_groups", "differentiate_variance_across_groups",
        "avoid_classes_in_groups", "require_classes_in_groups",
        #"avoid_items_in_groups", "require_items_in_groups",
        "maximize_class_diversity_in_groups", "minimize_class_diversity_in_groups",
        "equalize_class_distribution_across_groups", "differentiate_class_distribution_across_groups"
        )
      
      all_row_rules <- c(
        "maximize_variance_in_rows", "minimize_variance_in_rows",
        "equalize_means_across_rows", "differentiate_means_across_rows",  
        "equalize_variance_across_rows", "differentiate_variance_across_rows",
        "maximize_class_diversity_in_rows", "minimize_class_diversity_in_rows",
        "equalize_class_distribution_across_rows", "differentiate_class_distribution_across_rows"
      )
      
      stopifnot("This type of rule does not exist" = type_of_rule %in% c(all_group_rules, all_row_rules))
      
      # We need to check if the target_groups exist...
      #if (! target_groups %in% names(private$env$groups)) {
      #  print("At least one of the groups does not exist")
      #  return (NULL)
      #}
      
      if (type_of_rule %in% all_row_rules & length(unique(private$env$groups[target_groups])) > 1) {
        print("Cannot add this rule when groups under consideration are of different sizes")
        return (NULL)
      }
      
      print("Adding the rule")

      r <- switch(type_of_rule,
                  minimize_values_in_groups = Minimize_Or_Maximize_Values_In_Groups_Rule$new(private$env, variable_name, target_groups, weight, F),
                  maximize_values_in_groups = Minimize_Or_Maximize_Values_In_Groups_Rule$new(private$env, variable_name, target_groups, weight, T),
                  
                  minimize_variance_in_groups = Minimize_Or_Maximize_Variance_In_Groups_Rule$new(private$env, variable_name, target_groups, weight, F),
                  maximize_variance_in_groups = Minimize_Or_Maximize_Variance_In_Groups_Rule$new(private$env, variable_name, target_groups, weight, T),
                  
                  equalize_means_across_groups =      Equalize_Or_Differentiate_Means_Across_Groups_Rule$new(private$env, variable_name, target_groups, weight, F),
                  differentiate_means_across_groups = Equalize_Or_Differentiate_Means_Across_Groups_Rule$new(private$env, variable_name, target_groups, weight, T),
                  
                  equalize_variance_across_groups =      Minimize_Or_Maximize_Variance_Across_Groups_Rule$new(private$env, variable_name, target_groups, weight, F),
                  differentiate_variance_across_groups = Minimize_Or_Maximize_Variance_Across_Groups_Rule$new(private$env, variable_name, target_groups, weight, T),

                  avoid_classes_in_groups =   Avoid_Or_Require_Classes_In_Groups_Rule$new(private$env, variable_name, target_groups, weight, F, element),
                  require_classes_in_groups = Avoid_Or_Require_Classes_In_Groups_Rule$new(private$env, variable_name, target_groups, weight, T, element),

                  #avoid_items_in_groups =   Avoid_Or_Require_Items_In_Groups_Rule$new(private$env, variable_name, target_groups, weight, F, element),
                  #require_items_in_groups = Avoid_Or_Require_Items_In_Groups_Rule$new(private$env, variable_name, target_groups, weight, T, element),
                  
                  minimize_class_diversity_in_groups = Minimize_Or_maximize_Class_Diversity_In_Groups_Rule$new(private$env, variable_name, target_groups, weight, F),
                  maximize_class_diversity_in_groups = Minimize_Or_maximize_Class_Diversity_In_Groups_Rule$new(private$env, variable_name, target_groups, weight, T),

                  equalize_class_distribution_across_groups =      Equalize_Or_Differentiate_Class_Distribution_Across_Groups_Rule$new(private$env, variable_name, target_groups, weight, F),
                  differentiate_class_distribution_across_groups = Equalize_Or_Differentiate_Class_Distribution_Across_Groups_Rule$new(private$env, variable_name, target_groups, weight, T),

                  
                  minimize_variance_in_rows = Minimize_Or_Maximize_Variance_In_Rows_Rule$new(private$env, variable_name, target_groups, weight, F),
                  maximize_variance_in_rows = Minimize_Or_Maximize_Variance_In_Rows_Rule$new(private$env, variable_name, target_groups, weight, T),
                  
                  equalize_means_across_rows =      Equalize_Or_Differentiate_Means_Across_Rows_Rule$new(private$env, variable_name, target_groups, weight, F),
                  differentiate_means_across_rows = Equalize_Or_Differentiate_Means_Across_Rows_Rule$new(private$env, variable_name, target_groups, weight, T),
                  
                  equalize_variance_across_rows =      Equalize_Or_Differentiate_Variance_Across_Rows_Rule$new(private$env, variable_name, target_groups, weight, F),
                  differentiate_variance_across_rows = Equalize_Or_Differentiate_Variance_Across_Rows_Rule$new(private$env, variable_name, target_groups, weight, T),
                  
                  minimize_class_diversity_in_rows = Minimize_Or_maximize_Class_Diversity_In_Rows_Rule$new(private$env, variable_name, target_groups, weight, F),
                  maximize_class_diversity_in_rows = Minimize_Or_maximize_Class_Diversity_In_Rows_Rule$new(private$env, variable_name, target_groups, weight, T),
                  
                  equalize_class_distribution_across_rows =      Equalize_Or_Differentiate_Class_Distribution_Across_Rows_Rule$new(private$env, variable_name, target_groups, weight, F),
                  differentiate_class_distribution_across_rows = Equalize_Or_Differentiate_Class_Distribution_Across_Rows_Rule$new(private$env, variable_name, target_groups, weight, T),
      )
      
      private$optimized_var <- c(private$optimized_var, variable_name) %>% unique()
      
      private$rules_nb <- private$rules_nb + 1
      private$rules[[private$rules_nb]] <- r
      
      s <- r$get_description()
      print(glue("Rule added: {s}")) 
      
      return ()
    },
    
    # Faudra gérer et tester les valeurs de ratio pourries....
    
    
    evolve_genomes = function() {
      e <- private$env

      e$genomes <- e$genomes[, 1:private$selected_genomes_nb]

      for (i in 1:(e$genomes_nb / private$selected_genomes_nb - 1))
        e$genomes <- e$genomes %>% cbind(e$genomes[, 1:private$selected_genomes_nb])
      
      colnames(e$genomes) <- e$genome_names
      
      e$previous_genomes <- e$genomes
      
      mutated_genomes <- paste0("g.", (private$selected_genomes_nb + 1):e$genomes_nb)
      e$genomes <- cbind(e$genomes[, 1:private$selected_genomes_nb], e$genomes[, lapply(.SD, private$genome_mutate), .SDcols = mutated_genomes])
    },
    
  
    normalize_rules = function(normalization_steps_nb = 100) {
      e <- private$env
      # We take the rules 1 by 1 and
      private$selected_genomes_nb <- round(e$genomes_nb * private$genome_selection_ratio, 0)
  
      private$normalization_scores = tibble(score = double(), type = character(), rule_id = integer(), step = integer())
      
      for (i in 1:private$rules_nb) {
        self$create_genomes()
        print(paste0("Rule #", i))
        r <- private$rules[[i]]
        selected_var <- c(r$get_variable_name(), "obs_id")

        for (step in 1:normalization_steps_nb) {
          
          e$df_tmp <- e$df[, ..selected_var] %>% cbind(e$genomes) %>%
            melt(id.vars = selected_var, measure.vars = e$genome_names, variable.name = "genome", value.name = "group")
          e$df_tmp <- e$df_tmp[, row_id := 1:.N, by = list(genome, group)]

          rule_scores <- r$compute_scores()

          if (step == 1)
            random_score <- min(rule_scores)
        
          names(rule_scores) <- 1:e$genomes_nb
          rule_scores <- sort(rule_scores)
          score_order <- names(rule_scores) %>% as.integer()
          e$genomes <- e$genomes[, ..score_order]
          colnames(e$genomes) <- e$genome_names
          
          if (rule_scores[1] == r$get_minimal_possible_score())
            break

          if (step != normalization_steps_nb)
            self$evolve_genomes()
          
          # Recording scores
          private$normalization_scores <- private$normalization_scores %>% add_row(score = rule_scores[1], type = "best", rule_id = i, step = step)
          private$normalization_scores <- private$normalization_scores %>% add_row(score = mean(rule_scores[1:private$selected_genomes_nb]), type = "best_20", rule_id = i, step = step)
        }
        r$set_random_score(random_score)
        r$set_best_score(rule_scores[1])
      }
      
      return (private$normalization_scores)
    },
    
    optimize = function(optimization_steps_nb = 100, from_0 = T) {
      
      e <- private$env
      # We take the rules 1 by 1 and
      private$selected_genomes_nb <- round(e$genomes_nb * private$genome_selection_ratio, 0)
      
      sum_weights = 0
      for (r in private$rules)
        sum_weights = sum_weights + r$get_weight()
      
      if (is.null(private$rule_scores))
        print("No optimization process has taken place before. Starting from scratch")

      if (from_0 | is.null(private$rule_scores)) {
        print("Initialization")
        self$create_genomes()
        private$latest_step <- 0
        private$rule_scores <- matrix(0, nrow = private$rules_nb, ncol = e$genomes_nb)
        private$scores = tibble(score = double(), type = character(), rule_id = integer(), step = integer())
      } else {
        print("We continue")
      }

      selected_var <- c(private$optimized_var, "obs_id")
      
      for (step in 1:optimization_steps_nb) {
        all_rules_scores <- rep(0, e$genomes_nb)

        e$df_tmp <- e$df[, ..selected_var] %>%
           cbind(e$genomes) %>%
           melt(id.vars = selected_var, measure.vars = e$genome_names, variable.name = "genome", value.name = "group")
        e$df_tmp <- e$df_tmp[, row_id := 1:.N, by = list(genome, group)]
        
        for (i in 1:private$rules_nb) {
          r <- private$rules[[i]]
    
          private$rule_scores[i,] <- r$compute_normalized_scores()
          # private$rule_scores[i,] <- pmax(private$rule_scores[i,], 0)
          all_rules_scores <- all_rules_scores + r$get_weight() * private$rule_scores[i,] / sum_weights
        }

        names(all_rules_scores) <- 1:e$genomes_nb
        all_rules_scores <- sort(all_rules_scores)
        
        score_order <- names(all_rules_scores) %>% as.integer()

        e$genomes <- e$genomes[, ..score_order]
        colnames(e$genomes) <- paste0("g.", 1:e$genomes_nb)
        private$rule_scores <- private$rule_scores[, score_order]
        if (private$rules_nb == 1)
          private$rule_scores <- private$rule_scores %>% as.matrix() %>% t()

        if (step != optimization_steps_nb)
          self$evolve_genomes()

        # Recording scores
        for (i in 1:private$rules_nb) {
            private$scores <- private$scores %>% add_row(score = private$rule_scores[i,1], type = "best", rule_id = i, step = step + private$latest_step)
            private$scores <- private$scores %>% add_row(score = mean(private$rule_scores[i,1:private$selected_genomes_nb]), type = "best_20", rule_id = i, step = step + private$latest_step)
        }
        
        private$scores <- private$scores %>% add_row(score = all_rules_scores[1], type = "best", rule_id = -1, step = step + private$latest_step)
        private$scores <- private$scores %>% add_row(score = mean(all_rules_scores[1:private$selected_genomes_nb]), type = "best_20", rule_id = -1, step = step + private$latest_step)
      }
      
      private$latest_step = step + private$latest_step
      
      return (private$scores)
    },
    
    get_graph_scores = function() {
      
      new_level_names <- c("Combination", paste0("Rule ", 1:private$rules_nb))
                           
      tmp_df <- private$scores %>%
        filter(type == "best") %>%
        select(-type) %>%
        mutate(rule_id = as.factor(rule_id)) %>%
        rename(Rule = rule_id)
      
      new_levels <- tmp_df %>% pull(Rule) %>% levels()
      names(new_levels) <- new_level_names
      
      p <- tmp_df %>%
        mutate(Rule = fct_recode(Rule, !!!new_levels)) %>%
        ggplot(aes(x = step, y = score, col = Rule)) + 
        geom_line() +
        theme_minimal() +
        labs(title = "Quality of the optimization process", subtitle = "Evolution of the scores of the different rules") + xlab("Steps") + ylab("Score")
      
      return (p)
    },
    
    get_best_distribution = function(var, proba = F) {
      df_tmp <- private$env$df[, ..var] %>% cbind(ga$get_genome(1))
      df_tmp %>% setnames(c(1,2), c("target", "group"))
      df_tmp <- df_tmp[group > 0]
      df_tmp <- df_tmp[, .N, by = .(target, group)]
      df_tmp <- df_tmp %>% dcast(formula = target ~ group, value.var = "N")
      
      old_names <- colnames(df_tmp)[-1]
      new_names <- paste0("g", old_names)

      df_tmp %>% setnames(old = old_names, new = new_names)
      df_tmp[is.na(df_tmp)] <- 0
      
      if (proba)
        df_tmp <- df_tmp[, lapply(.SD, function(x) { x / sum(x)}), .SDcols = new_names]
      
      df_tmp %>% setnames(old = new_names, new = names(private$env$groups)) %>% setDF()
      
      return (df_tmp)
    },
    
    get_best_stats = function(var) {
      if (! class(private$env$df[[var]]) %in% c("integer", "numeric")) {
        print("No stats can be computed for a variable which is not numerical")
        return (NULL)
      }
        
      df_tmp <- private$env$df[, ..var] %>% cbind(self$get_genome(1)) %>% setnames(c(1,2), c("target", "group"))
      df_tmp <- df_tmp[group > 0][, .(av = mean(target), sd = sd(target)), by = group] %>% setDF()
      
      return (df_tmp)
    },
    
    get_best_stats_by_row = function(var) {
      if (! class(private$env$df[[var]]) %in% c("integer", "numeric")) {
        print("No stats can be computed for a variable which is not numerical")
        return (NULL)
      }
      
      n <- private$env$groups[1]
      rvar <- c(var, "gid")
      df_tmp <- private$env$df[, ..rvar] %>% cbind(self$get_genome(1)) %>% setnames(c(1,3), c("target", "group"))
      df_tmp <- df_tmp[group > 0] %>% setDF()
      # df_tmp <- df_tmp[, .(av = sd(target)), by = list(index)] %>% setDF()
      
      return (df_tmp)
    },
    
    get_best_data = function(var) {
      n <- private$env$groups[1]
      rvar <- c(var, "obs_id")
      df_tmp <- private$env$df[, ..rvar] %>% cbind(self$get_genome(1)) %>% setnames(3, "group")
      df_tmp <- df_tmp[group > 0]
      df_tmp <- df_tmp[, row_id := 1:.N, by = .(group)] %>% setDF()

      return (df_tmp)
    },
    
    get_best_grouping = function() {
      output <- self$get_genome(1) %>%
        setnames(1, "group") %>%
        mutate(group = as.character(group))
      
      old_names <- 1:length(private$env$groups) %>% as.character()
      new_names <- names(private$env$groups)
      names(new_names) <- old_names
      new_names
      
      output <- output %>% mutate(group = recode(group, !!!new_names))
      
      return (output)
    }
    
  )
)
