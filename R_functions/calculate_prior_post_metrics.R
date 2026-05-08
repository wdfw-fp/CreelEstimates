# Calculate prior-posterior divergence metrics
calculate_prior_post_metrics <- function(prior_draws, posterior_draws) {
  
  # Get unique parameters
  params <- unique(posterior_draws$parameter)
  
  # Calculate metrics for each parameter
  metrics <- map_df(params, ~{
    prior_vals <- filter(prior_draws, parameter == .x)$value
    post_vals <- filter(posterior_draws, parameter == .x)$value
    
    # Remove NAs
    prior_vals <- prior_vals[!is.na(prior_vals)]
    post_vals <- post_vals[!is.na(post_vals)]
    
    if (length(prior_vals) == 0 | length(post_vals) == 0) {
      return(tibble(
        parameter = .x,
        kl_divergence = NA_real_,
        js_divergence = NA_real_,
        overlap_coefficient = NA_real_,
        prior_post_ratio = NA_real_,
        shrinkage_factor = NA_real_,
        warning = "Insufficient draws"
      ))
    }
    
    # KL divergence (approximated via histogram)
    kl_div <- tryCatch({
      estimate_kl_divergence(post_vals, prior_vals)
    }, error = function(e) NA_real_)
    
    # Jensen-Shannon divergence
    js_div <- tryCatch({
      estimate_js_divergence(post_vals, prior_vals)
    }, error = function(e) NA_real_)
    
    # Overlap coefficient
    overlap <- tryCatch({
      calculate_overlap(post_vals, prior_vals)
    }, error = function(e) NA_real_)
    
    # Prior-posterior variance ratio
    pp_ratio <- var(post_vals) / var(prior_vals)
    
    # Shrinkage factor (ratio of posterior to prior sd)
    shrinkage <- sd(post_vals) / sd(prior_vals)
    
    tibble(
      parameter = .x,
      kl_divergence = kl_div,
      js_divergence = js_div,
      overlap_coefficient = overlap,
      prior_post_ratio = pp_ratio,
      shrinkage_factor = shrinkage,
      prior_mean = mean(prior_vals),
      prior_sd = sd(prior_vals),
      post_mean = mean(post_vals),
      post_sd = sd(post_vals),
      warning = NA_character_
    )
  })
  
  return(metrics)
}

# Helper: Estimate KL divergence using histogram approximation
estimate_kl_divergence <- function(p_samples, q_samples, n_bins = 50) {
  # Create common bins
  all_vals <- c(p_samples, q_samples)
  breaks <- seq(min(all_vals), max(all_vals), length.out = n_bins + 1)
  
  # Calculate densities
  p_hist <- hist(p_samples, breaks = breaks, plot = FALSE)
  q_hist <- hist(q_samples, breaks = breaks, plot = FALSE)
  
  # Normalize to probabilities
  p_prob <- p_hist$counts / sum(p_hist$counts)
  q_prob <- q_hist$counts / sum(q_hist$counts)
  
  # Add small constant to avoid log(0)
  epsilon <- 1e-10
  p_prob <- p_prob + epsilon
  q_prob <- q_prob + epsilon
  
  # Calculate KL divergence
  sum(p_prob * log(p_prob / q_prob))
}

# Helper: Estimate JS divergence
estimate_js_divergence <- function(p_samples, q_samples, n_bins = 50) {
  # Create common bins
  all_vals <- c(p_samples, q_samples)
  breaks <- seq(min(all_vals), max(all_vals), length.out = n_bins + 1)
  
  # Calculate densities
  p_hist <- hist(p_samples, breaks = breaks, plot = FALSE)
  q_hist <- hist(q_samples, breaks = breaks, plot = FALSE)
  
  # Normalize to probabilities
  p_prob <- p_hist$counts / sum(p_hist$counts)
  q_prob <- q_hist$counts / sum(q_hist$counts)
  
  # Add small constant
  epsilon <- 1e-10
  p_prob <- p_prob + epsilon
  q_prob <- q_prob + epsilon
  
  # M distribution
  m_prob <- (p_prob + q_prob) / 2
  
  # Calculate JS divergence
  0.5 * sum(p_prob * log(p_prob / m_prob)) + 
    0.5 * sum(q_prob * log(q_prob / m_prob))
}

# Helper: Calculate overlap coefficient
calculate_overlap <- function(p_samples, q_samples, n_bins = 50) {
  # Create common bins
  all_vals <- c(p_samples, q_samples)
  breaks <- seq(min(all_vals), max(all_vals), length.out = n_bins + 1)
  
  # Calculate densities
  p_hist <- hist(p_samples, breaks = breaks, plot = FALSE)
  q_hist <- hist(q_samples, breaks = breaks, plot = FALSE)
  
  # Normalize to probabilities
  p_prob <- p_hist$counts / sum(p_hist$counts)
  q_prob <- q_hist$counts / sum(q_hist$counts)
  
  # Calculate overlap
  sum(pmin(p_prob, q_prob))
}