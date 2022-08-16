# arguments

    Code
      translate_args(basic %>% set_engine("flexsurv"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      

---

    Code
      translate_args(basic %>% set_engine("flexsurv", cl = 0.99))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $cl
      <quosure>
      expr: ^0.99
      env:  empty
      

---

    Code
      translate_args(normal %>% set_engine("flexsurv"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $dist
      <quosure>
      expr: ^"lnorm"
      env:  empty
      

---

    Code
      translate_args(dist_v %>% set_engine("flexsurv"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      
      $dist
      <quosure>
      expr: ^tune()
      env:  empty
      

