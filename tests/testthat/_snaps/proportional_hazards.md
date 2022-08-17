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
      translate_args(penalty %>% set_engine("glmnet"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      

---

    Code
      translate_args(penalty %>% set_engine("glmnet", path_values = 4:2))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $path_values
      <quosure>
      expr: ^4:2
      env:  empty
      

---

    Code
      translate_args(mixture %>% set_engine("glmnet"))
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
      translate_args(mixture_v %>% set_engine("glmnet"))
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
      

