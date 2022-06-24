# Copyright Christophe Coupé (Linguistics Department, The University of Hong Kong, Hong Kong) (ccoupe@hku.hk)


Minimize_Or_Maximize_Values_In_Groups_Rule <- R6Class("Minimize_Or_Maximize_Values_In_Groups_Rule",
                              inherit = Rule,
                              public = list(
                                
                                compute_scores = function() {
                                  rvar <- super$get_variable_name()
                                  e <- super$get_env()
                                  
                                  df_tmp <- e$df_tmp[group %in% super$get_target_groups()]
                                  df_tmp %>% setnames(rvar, "target")
                                  df_tmp <- df_tmp[, .(target_sum = sum(target)), by = list(genome, group)][, .(group_sum = sum(target_sum)), by = genome]
                                  scores <- df_tmp$group_sum
                                  
                                  if (private$is_score_reversed())
                                    scores = 1 / scores
                                  
                                  return(scores)
                                },
                                
                                get_description = function() {
                                  if (private$is_score_reversed())
                                    s <- "Maximize"
                                  else
                                    s <- "Minimize"
                                  
                                  s <- glue("{s} the values of {super$get_variable_name()} in group(s) ") +
                                    glue_collapse(names(super$get_env()$groups)[super$get_target_groups()], ", ", last = " and ")
                                  
                                  return (s)
                                }
                              )
)


Minimize_Or_Maximize_Variance_In_Groups_Rule <- R6Class("Minimize_Or_Maximize_Variance_In_Groups_Rule",
                                                        inherit = Rule,
                                                        public = list(
                                                          
                                                          compute_scores = function() {
                                                            rvar <- super$get_variable_name()
                                                            e <- super$get_env()
                                                            
                                                            df_tmp <- e$df_tmp[group %in% super$get_target_groups()]
                                                            df_tmp %>% setnames(rvar, "target")
                                                            df_tmp <- df_tmp[, .(target_var = var(target)), by = list(genome, group)][, .(group_sum = sum(target_var)), by = genome]
                                                            scores <- df_tmp$group_sum
                                                            
                                                            if (private$is_score_reversed())
                                                              scores = 1 / scores
                                                            
                                                            return(scores)
                                                          },
                                                          
                                                          get_description = function() {
                                                            if (private$is_score_reversed())
                                                              s <- "Maximize"
                                                            else
                                                              s <- "Minimize"
                                                            
                                                            e <- super$get_env() 
                                                            
                                                            s <- glue("{s} the variance of {super$get_variable_name()} in group(s) ") +
                                                              glue_collapse(names(super$get_env()$groups)[super$get_target_groups()], ", ", last = " and ")
                                                            
                                                            return (s)
                                                          }
                                                        )
)



Equalize_Or_Differentiate_Means_Across_Groups_Rule <- R6Class("Equalize_Or_Differentiate_Means_Across_Groups_Rule", 
                                                            inherit = Rule,
                                                            public = list(
                                                              compute_scores = function() {
                                                                rvar <- super$get_variable_name()
                                                                e <- super$get_env()
                                                              
                                                                df_tmp <- e$df_tmp[group %in% super$get_target_groups()]
                                                                df_tmp %>% setnames(rvar, "target")
                                                                df_tmp <- df_tmp[, .(target_sum = sum(target)), by = list(genome, group)][, .(group_var = var(target_sum)), by = genome]
                                                                scores <- df_tmp$group_var
                                                                
                                                                if (private$is_score_reversed())
                                                                  scores = 1 / scores
                                                                
                                                                return(scores)
                                                              },
                                                              
                                                              get_description = function() {
                                                                if (private$is_score_reversed())
                                                                  s <- "Differentiate"
                                                                else
                                                                  s <- "Equalize"
                                                                
                                                                s <- glue("{s} the mean of {super$get_variable_name()} across groups ") +
                                                                  glue_collapse(names(super$get_env()$groups)[super$get_target_groups()], ", ", last = " and ")
                                                                
                                                                return (s)
                                                              }
                                                            )
)




Minimize_Or_Maximize_Variance_Across_Groups_Rule <- R6Class("Minimize_Or_Maximize_Variance_Across_Groups_Rule", 
                                                            inherit = Rule,
                                                            public = list(
                                                              compute_scores = function() {
                                                                rvar <- super$get_variable_name()
                                                                e <- super$get_env()
                                                                
                                                                df_tmp <- e$df_tmp[group %in% super$get_target_groups()]
                                                                df_tmp %>% setnames(rvar, "target")
                                                                df_tmp <- df_tmp[, .(target_var = var(target)), by = list(genome, group)][, .(group_var = var(target_var)), by = genome]
                                                                scores <- df_tmp$group_var
                                                                
                                                                if (private$is_score_reversed())
                                                                  scores = 1 / scores
                                                                
                                                                return(scores)
                                                              },
                                                              
                                                              get_description = function() {
                                                                if (private$is_score_reversed())
                                                                  s <- "Maximize"
                                                                else
                                                                  s <- "Minimize"
                                                                
                                                                s <- glue("{s} the variance of {super$get_variable_name()} across groups ") +
                                                                  glue_collapse(names(super$get_env()$groups)[super$get_target_groups()], ", ", last = " and ")
                                                                
                                                                return (s)
                                                              }
                                                            )
)



Avoid_Or_Require_Classes_In_Groups_Rule <- R6Class("Avoid_Or_Require_Classes_In_Groups_Rule",
                          inherit = Rule,
                          private = list(
                            target_classes = NULL
                          ),
                          public = list(
                            initialize = function(e, variable_name, target_groups, weight, reverse_score, classes) {
                              super$initialize(e, variable_name, target_groups, weight, reverse_score)
                              stopifnot("The class does not exist for the chosen variable" = all(classes %in% private$get_env()$df[[super$get_variable_name()]]))
                              private$target_classes = classes
                              print(classes)
                            },
                            
                            compute_scores = function() {
                              rvar <- super$get_variable_name()
                              e <- super$get_env()
                              
                              df_tmp <- e$df_tmp[group %in% super$get_target_groups()]
                              df_tmp %>% setnames(rvar, "target")
                              df_tmp <- df_tmp[, .(sum_occ = sum(target %in% private$target_classes)), by = genome]
                              scores <- df_tmp$sum_occ
                              
                              if (private$is_score_reversed())
                                scores = 1 / scores
                              
                              return(scores)
                            },
                            
                            get_description = function() {
                              
                              if (private$is_score_reversed())
                                s <- "Require"
                              else
                                s <- "Avoid"
                              
                              s <- glue("{s} the class(es) ") + glue_collapse(private$target_classes, ", ", last = " and ") +
                                glue(" of the variable {super$get_variable_name()} in group(s) ") +
                                glue_collapse(names(super$get_env()$groups)[super$get_target_groups()], ", ", last = " and ")
                              
                              return (s)
                            }
                          )
)

# 
# Avoid_Or_Require_Items_In_Groups_Rule <- R6Class("Avoid_Or_Require_Items_In_Groups_Rule",
#                           inherit = Rule,
#                           private = list(
#                             target_items = NULL
#                           ),
#                           public = list(
#                             initialize = function(e, variable_name, target_groups, weight, reverse_score, items) {
#                               super$initialize(e, variable_name, target_groups, weight, reverse_score)
#                               stopifnot("This/ese observation(s) do(es) not exist" = all(items %in% private$get_env()$df[[super$get_variable_name()]]))
#                               
#                               private$target_items = items
#                             },
#                             
#                             compute_scores = function() {
#                               rvar <- super$get_variable_name()
#                               e <- super$get_env()
#                               
#                               df_tmp <- e$df_tmp[group %in% super$get_target_groups()]
#                               df_tmp %>% setnames(rvar, "target")
#                               df_tmp <- df_tmp[, .(sum_occ = sum(obs_id %in% private$target_items)), by = genome]
#                               scores <- df_tmp$sum_occ
#                               
#                               if (private$is_score_reversed())
#                                 scores = 1 / scores
#                               
#                               return(scores)
#                             },
#                             
#                             get_description = function() {
#                               if (private$is_score_reversed())
#                                 s <- "Require"
#                               else
#                                 s <- "Avoid"
#                               
#                               s <- glue("{s} the item(s)  ") + glue_collapse(private$target_items, ", ", last = " and ") +
#                                 glue(" of the variable {super$get_variable_name()} in group(s) ") +
#                                 glue_collapse(names(super$get_env()$groups)[super$get_target_groups()], ", ", last = " and ")
#                               
#                               return (s)
#                             }
#                           )
# )


Minimize_Or_maximize_Class_Diversity_In_Groups_Rule <- R6Class("Minimize_Or_maximize_Class_Diversity_In_Groups_Rule",
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
                                          df_tmp <- df_tmp[, .N, by = .(target, genome, group)]
                                          df_tmp <- df_tmp[, prop := N/sum(N), by = list(genome, group)][, N := NULL]
                                          setnafill(df_tmp, type = "const", fill = 0, cols = "prop")
                                          df_tmp <- df_tmp[, .(target_entropy = private$entropy(prop)), by = list(genome, group)]
                                          df_tmp <- df_tmp[, .(group_sum = sum(target_entropy)), by = genome]
                                          scores <- df_tmp$group_sum
                                          
                                          #final_scores <- df_tmp[, lapply(.SD, private$entropy), by = "genome", .SDcols = "prop"]
                                          
                                          if (private$is_score_reversed())
                                            scores = 1 / scores
                                          
                                          return(scores)
                                        },
                                        
                                        get_description = function() {
                                          if (private$is_score_reversed())
                                            s <- "Maximize"
                                          else
                                            s <- "Minimize"
                                          
                                          s <- glue("{s} class diversity of {super$get_variable_name()} in groups ") +
                                            glue_collapse(names(super$get_env()$groups)[super$get_target_groups()], ", ", last = " and ")
                                          
                                          return (s)
                                        }
                                      )
)




Equalize_Or_Differentiate_Class_Distribution_Across_Groups_Rule <- R6Class("Equalize_Or_Differentiate_Class_Distribution_Across_Groups_Rule",
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
                                         DT3 <- data.table(group = super$get_target_groups())
                                         DT4 <- data.table(N = 0)
                                         DT <- crossing(DT1, DT2, DT3, DT4)

                                         df_tmp <- e$df_tmp[group %in% super$get_target_groups()]
                                         df_tmp %>% setnames(rvar, "target")
                                         df_tmp <- df_tmp[, .N, by = list(target, genome, group)]
                                         df_tmp <- df_tmp %>% rbind(DT)
                                         df_tmp <- df_tmp[, .(N=sum(N)), by = list(target, genome, group)]
                                         df_tmp <- df_tmp[, prop := N/sum(N), by = list(genome, group)][, N := NULL]
                                         setnafill(df_tmp, type = "const", fill = 0, cols = "prop")
                                         df_tmp <- df_tmp[, .(group_var = var(prop)), by = list(genome, target)]
                                         setnafill(df_tmp, type = "const", fill = 0, cols = "group_var")
                                         df_tmp <- df_tmp[, .(sum_group_var = sum(group_var)), by = genome]
                                         scores <- df_tmp$sum_group_var
                                       
                                         if (private$is_score_reversed())
                                           scores = 1 / scores
                                         
                                         return (scores)
                                       },
                                       
                                       get_description = function() {
                                         if (private$is_score_reversed())
                                           s <- "Differentiate"
                                         else
                                           s <- "Equalize"
                                         
                                         s <- glue("{s} class distribution of {super$get_variable_name()} across groups ") +
                                           glue_collapse(names(super$get_env()$groups)[super$get_target_groups()], ", ", last = " and ")
                                         
                                         return (s)
                                       }
                                     )
)


# df_tmp <- df_tmp %>% dcast(formula = get + genome ~ group, value.var = "prop")
# df_tmp %>% setnames(old = c("1","2"), new = c("g1", "g2"))
# df_tmp[is.na(df_tmp)] <- 0
# 
# df_tmp <- df_tmp[, abs := abs(g1-g2)][, lapply(.SD, sum), by = "genome", .SDcols = "abs"]
# 
# scores <- df_tmp$abs


