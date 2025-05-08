# arguments

    Code
      translate_args(basic)
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $x
      [1] TRUE
      
      $model
      [1] TRUE
      

---

    Code
      translate_args(set_engine(penalty, "glmnet"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      

---

    Code
      translate_args(set_engine(penalty, "glmnet", path_values = 4:2))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $lambda
      <quosure>
      expr: ^4:2
      env:  empty
      

---

    Code
      translate_args(set_engine(mixture, "glmnet"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $alpha
      <quosure>
      expr: ^0.128
      env:  empty
      

---

    Code
      translate_args(set_engine(mixture_v, "glmnet"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $alpha
      <quosure>
      expr: ^tune()
      env:  empty
      

