# Copyright Christophe Coupé (Linguistics Department, The University of Hong Kong, Hong Kong) (ccoupe@hku.hk)

Rule <- R6Class("Rule",
                private = list(
                  env = NULL,
                  rule_type = NULL,
                  random_score = 1,
                  best_score = 0,
                  variable_name = NULL,
                  target_groups = NULL,
                  weight = 1,
                  reverse_score = F,
                  
                  get_env = function() {
                    return (private$env)
                  },
                  
                  is_score_reversed = function() {
                    return (private$reverse_score)
                  }
                ),
                
                public = list(
                  initialize = function(e, variable_name, target_groups, weight, reverse_score) {
                    stopifnot(is.character(variable_name), length(variable_name) == 1, is.character(target_groups), is.numeric(weight))
                    
                    private$env = e
                    private$variable_name <- variable_name
                    private$target_groups <- which(names(e$groups) %in% target_groups)
                    private$weight <- weight
                    private$reverse_score = reverse_score
                    private$rule_type = class(self)[1]
                  },
                  
                  get_variable_name = function() {
                    return (private$variable_name)
                  },
                  
                  get_target_groups = function() {
                    return (private$target_groups)
                  },
                  
                  get_weight = function() {
                    return (private$weight)
                  },
                  
                  get_minimal_possible_score = function() {
                    return (0)
                  },
                  
                  compute_scores = function() {
                    return (NULL)
                  },
                  
                  set_best_score = function(sc) {
                    private$best_score <- sc
                  },
                  
                  set_random_score = function(sc) {
                    private$random_score <- sc
                  },
                  
                  get_rule_type = function() {
                    return (private$rule_type)
                  },
                  
                  get_random_score = function(sc) {
                    return (private$random_score)
                  },
                  
                  get_best_score = function(sc) {
                    return (private$best_score)
                  },
                  
                  compute_normalized_scores = function() {
                    return ((self$compute_scores() - self$get_best_score()) / self$get_random_score())
                  },
                  
                  get_description = function() {
                    return ("")
                  }
                  
                )
)
