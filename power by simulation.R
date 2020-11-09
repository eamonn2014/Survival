

library(survival)
sim = function(n_trt, n_ctrl, m_trt, m_ctrl, t_censoring) {
  # Simulate some data
  t_trt = rexp(n_trt, log(2)/m_trt)
  t_ctrl = rexp(n_ctrl, log(2)/m_ctrl)
  t = c(t_trt, t_ctrl)
  
  # Add censoring
  died = (t <= t_censoring)
  t_after_censoring = pmin(t, t_censoring)
  
  # Fit Cox model
  gr = c(rep(1, n_trt), rep(0, n_ctrl))
  l = coxph(Surv(t_after_censoring, died) ~ gr)
  anova(l)$`Pr(>|Chi|)`[2] # P-value from likelihood ratio test
}



set.seed(123) # Reproducible pseudorandom numbers
n_sim = 1000  # Number of simulations (increase this for more accuracy)
p_med = replicate(n_sim, sim(n_trt = 155, n_ctrl = 78,
                             m_trt = 7, m_ctrl = 4.4,
                             t_censoring = 24))

p_hr = replicate(n_sim, sim(n_trt = 84, n_ctrl = 42,
                            m_trt = 4.4/0.54, m_ctrl = 4.4,
                            t_censoring = 24))

mean(p_med <= 0.05) # Estimated power using median

mean(p_hr <= 0.05)  # Estimated power using control median + HR
