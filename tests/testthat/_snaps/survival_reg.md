# arguments

    Code
      translate_args(set_engine(basic, "flexsurv"))
    Output
      $formula
      missing_arg()
      
      $data
      missing_arg()
      
      $weights
      missing_arg()
      

---

    Code
      translate_args(set_engine(basic, "flexsurv", cl = 0.99))
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
      translate_args(set_engine(normal, "flexsurv"))
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
      translate_args(set_engine(dist_v, "flexsurv"))
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
      

