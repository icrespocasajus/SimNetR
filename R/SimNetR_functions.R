
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param split PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname strsplit2
#' @export 
strsplit2 <- function (x, split, ...) 
{
  x <- as.character(x)
  n <- length(x)
  s <- strsplit(x, split = split, ...)
  nc <- unlist(lapply(s, length))
  out <- matrix("", n, max(nc))
  for (i in 1:n) {
    if (nc[i]) 
      out[i, 1:nc[i]] <- s[[i]]
  }
  return(out)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname range01.w.max.min
#' @export 
range01.w.max.min <- function(x) {
  min.val <- min(x, na.rm = TRUE)
  max.val <- max(x, na.rm = TRUE)
  norm.val <- (x - min.val) / (max.val - min.val)
  results <- list(norm.val = norm.val, max.val = max.val, min.val = min.val)
  return(results)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname range01
#' @export 
range01 <- function(x) {
  min.val <- min(x, na.rm = TRUE)
  max.val <- max(x, na.rm = TRUE)
  norm.val <- (x - min.val) / (max.val - min.val)
  return(norm.val)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param embeddings PARAM_DESCRIPTION
#' @param query PARAM_DESCRIPTION
#' @param k PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname find.knn
#' @export 
find.knn <- function(embeddings, query, k) {
  library(distances)
  distances.input <- rbind(query, embeddings)
  dist <- distances(distances.input)
  knn <- nearest_neighbor_search(distances = dist, k = k + 1, query_indices = c(1:nrow(query)))
  colnames(knn) <- rownames(query)
  knn.w.labels <- apply(knn, 2, function(x) { rownames(distances.input)[x] })
  return(knn.w.labels[-c(1), , drop = FALSE])
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param nodes PARAM_DESCRIPTION
#' @param at.times PARAM_DESCRIPTION
#' @param duration PARAM_DESCRIPTION
#' @param intensity PARAM_DESCRIPTION
#' @param time.step.size PARAM_DESCRIPTION
#' @param times PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname perturbNodes
#' @export 
perturbNodes <- function(nodes, at.times, duration, intensity, time.step.size, times) {
  pulses <- vars <- values <- NULL
  for (i in 1:length(nodes)) {
    adjusted.time <- (at.times[i] / time.step.size) + 1
    interval <- (duration[i] / time.step.size)
    pulse <- seq(adjusted.time, adjusted.time + interval, by = time.step.size)
    pulses <- c(pulses, times[pulse])
    vars <- c(vars, rep(nodes[i], length(pulse)))
    values <- c(values, rep(intensity[i], length(pulse)))
  }          
  return(data.frame(var = vars, time = pulses, value = values, method = rep("rep", length(pulses))))
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param title PARAM_DESCRIPTION
#' @param xlabel PARAM_DESCRIPTION
#' @param ylabel PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname plotPerturbation
#' @export 
plotPerturbation <- function(data, title, xlabel, ylabel) {
  data <- melt(as.data.frame(data), id.vars = "time")
  p <- ggplot(data = data, aes(x = time, y = value, colour = variable))
  p <- p + geom_point(size = 1.0, alpha = 0) + geom_path(linewidth = 1.5, alpha = 1.0)
  p <- p + theme_tufte()
  p <- p + scale_y_continuous(limits = c(0, 1.0)) + scale_x_continuous(limits = c(0, 30))
  p <- p + labs(list(title = title, x = xlabel, y = ylabel, colour = ""))
  return(p)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param net PARAM_DESCRIPTION
#' @param state PARAM_DESCRIPTION
#' @param mode PARAM_DESCRIPTION, Default: 'fuzzy.logic'
#' @param h PARAM_DESCRIPTION, Default: 50
#' @param gamma PARAM_DESCRIPTION, Default: 1
#' @param time.limit PARAM_DESCRIPTION, Default: 30
#' @param time.step.size PARAM_DESCRIPTION, Default: 0.01
#' @param weights PARAM_DESCRIPTION
#' @param perturbations PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname network.dynamics.ODE
#' @export 
network.dynamics.ODE <- function(net, state, mode = 'fuzzy.logic', h = 50, gamma = 1, time.limit = 30, time.step.size = 0.01, weights, perturbations = NULL) {
  library(deSolve)
  
  # Functions
  SIGMOID <- function(x, w, gamma, h) {
    val <- ((-exp(0.5 * h) + exp(-h * (w - 0.5))) / ((1 - exp(0.5 * h)) * (1 + exp(-h * (w - 0.5))))) - (gamma * x)
    return(val)
  }  
  
  NETWORK.FUZZY.LOGIC <- function(times, state, parameters) {
    with(as.list(c(state, parameters)), {
      # Functions for each type of regulation
      w_activated_inhibited <- function(x) { min((max(state[activators[[x]]])), (1 - max(state[inhibitors[[x]]]))) }
      w_activated <- function(x) { max(state[activators[[x]]]) }
      w_inhibited <- function(x) { 1 - max(state[inhibitors[[x]]]) }
      
      # Calculation of w
      w_ <- list()
      for (node in nodes) {
        if (regulation.type[[node]] == "activated_inhibited") {
          w_[[paste0('w_', node)]] <- w_activated_inhibited(node)
        }
        if (regulation.type[[node]] == "activated") {
          w_[[paste0('w_', node)]] <- w_activated(node)
        }
        if (regulation.type[[node]] == "inhibited") {
          w_[[paste0('w_', node)]] <- w_inhibited(node)
        }
        if (regulation.type[[node]] == "unregulated") {
          w_[[paste0('w_', node)]] <- NULL
        }
      }
      # Calculation of

 dstate
      dstate <- NULL
      for (node in nodes) {
        if (is.null(w_[[paste0('w_', node)]])) {
          dstate <- c(dstate, 0)
        } else {
          dstate <- c(dstate, SIGMOID(state[node], w_[[paste0('w_', node)]], gamma, h))
        }
      }
      # Return list of derivatives
      return(list(dstate))
    })
  }
  
  # Assign model according to mode
  if (mode == 'fuzzy.logic') {
    model <- NETWORK.FUZZY.LOGIC
  } else {
    stop("Invalid mode")
  }
  
  # Initial state
  state <- as.vector(state)
  
  # Time sequence
  times <- seq(0, time.limit, by = time.step.size)
  
  # Solve ODE
  out <- ode(y = state, times = times, func = model, parms = list(parameters = list(nodes = names(state), regulation.type = net$regulation.type, activators = net$activators, inhibitors = net$inhibitors)), method = "ode45")
  
  # Plot perturbations
  if (!is.null(perturbations)) {
    perturbations <- as.data.frame(perturbations)
    for (i in 1:nrow(perturbations)) {
      out <- rbind(out, data.frame(time = perturbations$time[i], out[perturbations$var[i], -1] + perturbations$value[i]))
    }
  }
  
  return(out)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param net PARAM_DESCRIPTION
#' @param state PARAM_DESCRIPTION
#' @param mode PARAM_DESCRIPTION, Default: 'fuzzy.logic'
#' @param h PARAM_DESCRIPTION, Default: 50
#' @param gamma PARAM_DESCRIPTION, Default: 1
#' @param time.limit PARAM_DESCRIPTION, Default: 30
#' @param time.step.size PARAM_DESCRIPTION, Default: 0.01
#' @param weights PARAM_DESCRIPTION
#' @param perturbations PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname network.dynamics.ODE.2
#' @export 
network.dynamics.ODE.2 <- function(net, state, mode = 'fuzzy.logic', h = 50, gamma = 1, time.limit = 30, time.step.size = 0.01, weights, perturbations = NULL) {
  library(deSolve)
  
  # Functions
  SIGMOID <- function(x, w, gamma, h) {
    val <- ((-exp(0.5 * h) + exp(-h * (w - 0.5))) / ((1 - exp(0.5 * h)) * (1 + exp(-h * (w - 0.5))))) - (gamma * x)
    return(val)
  }  
  
  NETWORK.FUZZY.LOGIC <- function(times, state, parameters) {
    with(as.list(c(state, parameters)), {
      # Functions for each type of regulation
      w_activated_inhibited <- function(x) { min((max(state[activators[[x]]])), (1 - max(state[inhibitors[[x]]]))) }
      w_activated <- function(x) { max(state[activators[[x]]]) }
      w_inhibited <- function(x) { 1 - max(state[inhibitors[[x]]]) }
      
      # Calculation of w
      w_ <- list()
      for (node in nodes) {
        if (regulation.type[[node]] == "activated_inhibited") {
          w_[[paste0('w_', node)]] <- w_activated_inhibited(node)
        }
        if (regulation.type[[node]] == "activated") {
          w_[[paste0('w_', node)]] <- w_activated(node)
        }
        if (regulation.type[[node]] == "inhibited") {
          w_[[paste0('w_', node)]] <- w_inhibited(node)
        }
        if (regulation.type[[node]] == "unregulated") {
          w_[[paste0('w_', node)]] <- NULL
        }
      }
      # Calculation of dstate
      dstate <- NULL
      for (node in nodes) {
        if (is.null(w_[[paste0('w_', node)]])) {
          dstate <- c(dstate, 0)
        } else {
          dstate <- c(dstate, SIGMOID(state[node], w_[[paste0('w_', node)]], gamma, h))
        }
      }
      # Return list of derivatives
      return(list(dstate))
    })
  }
  
  # Assign model according to mode
  if (mode == 'fuzzy.logic') {
    model <- NETWORK.FUZZY.LOGIC
  } else {
    stop("Invalid mode")
  }
  
  # Initial state
  state <- as.vector(state)
  
  # Time sequence
  times <- seq(0, time.limit, by = time.step.size)
  
  # Solve ODE
  out <- ode(y = state, times = times, func = model, parms = list(parameters = list(nodes = names(state), regulation.type = net$regulation.type, activators = net$activators, inhibitors = net$inhibitors)), method = "ode45")
  
  # Plot perturbations
  if (!is.null(perturbations)) {
    perturbations <- as.data.frame(perturbations)
    for (i in 1:nrow(perturbations)) {
      out <- rbind(out, data.frame(time = perturbations$time[i], out[perturbations$var[i], -1] + perturbations$value[i]))
    }
  }
  
  return(out)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param v PARAM_DESCRIPTION
#' @param lower_limit PARAM_DESCRIPTION
#' @param upper_limit PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname scale_vector
#' @export 
scale_vector <- function(v, lower_limit, upper_limit) {
  min_val <- min(v)
  max_val <- max(v)
  scaled_vector <- ((v - min_val) / (max_val - min_val)) * (upper_limit - lower_limit) + lower_limit
  return(scaled_vector)
}
