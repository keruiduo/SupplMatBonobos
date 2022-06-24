# Copyright Christophe Coupé (Linguistics Department, The University of Hong Kong, Hong Kong) (ccoupe@hku.hk)

Minimize_Or_Maximize_Variance_In_Rows_Rule <- R6Class("Minimize_Or_Maximize_Variance_In_Rows_Rule",
                                                        inherit = Rule,
                                                        public = list(
                                                          
                                                          compute_scores = function() {
                                                            rvar <- super$get_variable_name()
                                                            e <- super$get_env()
                                                            
                                                            df_tmp <- e$df_tmp[group %in% super$get_target_groups()]
                                                            df_tmp %>% setnames(rvar, "target")
                                                            df_tmp <- df_tmp[, .(target_var = var(target)), by = list(genome, row_id)][, .(row_sum = sum(target_var)), by = genome]
                                                            scores <- df_tmp$row_sum
                                                            
                                                            if (private$is_score_reversed())
                                                              scores = 1 / scores
                                                            
                                                            return(scores)
                                                            

                                                          },
                                                          
                                                          get_description = function() {
                                                            if (private$is_score_reversed())
                                                              s <- "Maximize"
                                                            else
                                                              s <- "Minimize"
                                                            
                                                            s <- glue("{s} the variance of {super$get_variable_name()} in row for groups ") +
                                                              glue_collapse(names(super$get_env()$groups)[super$get_target_groups()], ", ", last = " and ")
                                                            
                                                            return (s)
                                                          }
                                                        )
)

Equalize_Or_Differentiate_Means_Across_Rows_Rule <- R6Class("Equalize_Or_Differentiate_Means_Across_Rows_Rule", 
                                                              inherit = Rule,
                                                              public = list(
                                                                compute_scores = function() {
                                                                  rvar <- super$get_variable_name()
                                                                  e <- super$get_env()
                                                                  
                                                                  df_tmp <- e$df_tmp[group %in% super$get_target_groups()]
                                                                  df_tmp %>% setnames(rvar, "target")
                                                                  df_tmp <- df_tmp[, .(target_sum = sum(target)), by = list(genome, row_id)][, .(row_var = var(target_sum)), by = genome]
                                                                  scores <- df_tmp$row_var

                                                                  if (private$is_score_reversed())
                                                                    scores = 1 / scores
                                                                  
                                                                  return(scores)
                                                                },
                                                                
                                                                get_description = function() {
                                                                  if (private$is_score_reversed())
                                                                    s <- "Maximize"
                                                                  else
                                                                    s <- "Minimize"
                                                                  
                                                                  s <- glue("{s} the variance of {super$get_variable_name()} across rows for groups ") +
                                                                    glue_collapse(names(super$get_env()$groups)[super$get_target_groups()], ", ", last = " and ")
                                                                  
                                                                  return (s)
                                                                }
                                                              )
)

Equalize_Or_Differentiate_Variance_Across_Rows_Rule <- R6Class("Equalize_Or_Differentiate_Variance_Across_Rows_Rule", 
                                                            inherit = Rule,
                                                            public = list(
                                                              compute_scores = function() {
                                                                rvar <- super$get_variable_name()
                                                                e <- super$get_env()

                                                                df_tmp <- e$df_tmp[group %in% super$get_target_groups()]
                                                                df_tmp %>% setnames(rvar, "target")
                                                                df_tmp <- df_tmp[, .(target_var = var(target)), by = list(genome, row_id)][, .(row_var = var(target_var)), by = genome]
                                                                scores <- df_tmp$row_var
                                                                
                                                                if (private$is_score_reversed())
                                                                  scores = 1 / scores
                                                                
                                                                return(scores)
                                                              },
                                                              
                                                              get_description = function() {
                                                                if (private$is_score_reversed())
                                                                  s <- "Maximize"
                                                                else
                                                                  s <- "Minimize"
                                                                
                                                                s <- glue("{s} the variance of {super$get_variable_name()} across rows for groups ") +
                                                                  glue_collapse(names(super$get_env()$groups)[super$get_target_groups()], ", ", last = " and ")
                                                                
                                                                return (s)
                                                              }
                                                            )
)

Minimize_Or_maximize_Class_Diversity_In_Rows_Rule <- R6Class("Minimize_Or_maximize_Class_Diversity_In_Rows_Rule",
                                                               inherit = Rule,
                                                               private = list(
                                                                 entropy = function(freq) {
                                                                   freq <- freq / sum(freq) # Normalizing the frequencies (sum = 1)
                                                                   freq <- freq[freq != Inf]
                                                                   e <- 0 - freq * log2(freq) # Note that we use a log2 rather than a log
  
                                                                   return (sum(e))
                                                                 }
                                                               ),
                                                               public = list(
                                                                 compute_scores = function() {
                                                                   rvar <- super$get_variable_name()
                                                                   e <- super$get_env()
                                                                   
                                                                   df_tmp <- e$df_tmp[group %in% super$get_target_groups()]
                                                                   df_tmp %>% setnames(rvar, "target")
                                                                   df_tmp <- df_tmp[, .N, by = .(target, genome, row_id)]
                                                                   df_tmp <- df_tmp[, prop := N/sum(N), by = list(genome, row_id)][, N := NULL]
                                                                   setnafill(df_tmp, type = "const", fill = 0, cols = "prop")
                                                                   df_tmp <- df_tmp[, .(target_entropy = private$entropy(prop)), by = .(genome, row_id)]
                                                                   df_tmp <- df_tmp[, .(row_sum = sum(target_entropy)), by = genome]
                                                                   scores <- df_tmp$row_sum
                                                                   
                                                                   if (private$is_score_reversed())
                                                                     scores = 1 / scores
                                                                   
                                                                   return(scores)
                                                                 },
                                                                 
                                                                 get_description = function() {
                                                                   if (private$is_score_reversed())
                                                                     s <- "Maximize"
                                                                   else
                                                                     s <- "Minimize"
                                                                   
                                                                   s <- glue("{s} class diversity of {super$get_variable_name()} in rows for groups ") +
                                                                     glue_collapse(names(super$get_env()$groups)[super$get_target_groups()], ", ", last = " and ")
                                                                   
                                                                   
                                                                   return (s)
                                                                 }
                                                               )
)

Equalize_Or_Differentiate_Class_Distribution_Across_Rows_Rule <- R6Class("Equalize_Or_Differentiate_Class_Distribution_Across_Rows_Rule",
                                                                           inherit = Rule,
                                                                           private = list(
                                                                             entropy = function(freq) {
                                                                               freq <- freq / sum(freq) # Normalizing the frequencies (sum = 1)
                                                                               freq <- freq[freq != Inf]
                                                                               e <- 0 - freq * log2(freq) # Note that we use a log2 rather than a log
                                                                               
                                                                               return (sum(e))
                                                                             }
                                                                           ),
                                                                           public = list(
                                                                             compute_scores = function() {
                                                                               rvar <- super$get_variable_name()
                                                                               e <- super$get_env()
                                                                               
                                                                               # We create a data.table with 0 values 
                                                                               DT1 <- data.table(target = unique(e$df[,..rvar])) %>% setnames(1, "target")
                                                                               DT2 <- data.table(genome = e$genome_names)
                                                                               DT3 <- data.table(row_id = 1:e$groups[super$get_target_groups()[1]])
                                                                               DT4 <- data.table(N = 0)
                                                                               DT <- crossing(DT1, DT2, DT3, DT4)

                                                                               df_tmp <- e$df_tmp[group %in% super$get_target_groups()]
                                                                               df_tmp %>% setnames(rvar, "target")
                                                                               df_tmp <- df_tmp[, .N, by = list(target, genome, row_id)]
                                                                               df_tmp <- df_tmp %>% rbind(DT)
                                                                               df_tmp <- df_tmp[, .(N=sum(N)), by = list(target, genome, row_id)]
                                                                               df_tmp <- df_tmp[, prop := N/sum(N), by = list(genome, row_id)][, N := NULL]
                                                                               setnafill(df_tmp, type = "const", fill = 0, cols = "prop")
                                                                               df_tmp <- df_tmp[, .(row_var = var(prop)), by = list(genome, target)]
                                                                               setnafill(df_tmp, type = "const", fill = 0, cols = "row_var")
                                                                               df_tmp <- df_tmp[, .(sum_row_var = sum(row_var)), by = genome]
                                                                               scores <- df_tmp$sum_row_var
                                                                               
                                                                               if (private$is_score_reversed())
                                                                                 scores = 1 / scores
                                                                               
                                                                               return(scores)
                                                                             },
                                                                             
                                                                             get_description = function() {
                                                                               if (private$is_score_reversed())
                                                                                 s <- "Differentiate"
                                                                               else
                                                                                 s <- "Equalize"
                                                                               
                                                                               s <- glue("{s} class distribution of {super$get_variable_name()} across rows for groups ") +
                                                                                 glue_collapse(names(super$get_env()$groups)[super$get_target_groups()], ", ", last = " and ")
                                                                               
                                                                               return (s)
                                                                             }
                                                                           )
)
