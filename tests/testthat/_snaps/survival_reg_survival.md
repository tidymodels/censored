# survival execution

    Code
      res
    Output
      parsnip model object
      
      Call:
      survival::survreg(formula = Surv(time, status) ~ age + sex, data = data, 
          model = TRUE)
      
      Coefficients:
      (Intercept)         age         sex 
       6.27485306 -0.01225703  0.38208514 
      
      Scale= 0.7540509 
      
      Loglik(model)= -1147.1   Loglik(intercept only)= -1153.9
      	Chisq= 13.59 on 2 degrees of freedom, p= 0.00112 
      n= 228 

