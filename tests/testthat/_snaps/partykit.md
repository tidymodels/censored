# survival_prob_partykit() errors informatively on bad input

    Code
      survival_prob_partykit(raw_fit, new_data = lung[1:3, ], eval_time = 100)
    Condition
      Error in `survival_prob_partykit()`:
      ! `object` must be a <model_fit> object, not a <constparty> object.

---

    Code
      survival_prob_partykit(wrong_engine, new_data = lung[1:3, ], eval_time = 100)
    Condition
      Error in `survival_prob_partykit()`:
      ! `object$fit` must be a <party> or <parties> object, not a <coxph> object.

# survival_prob_partykit() fails gracefully for eval_time values it can't handle

    Code
      survival_prob_partykit(mod, new_data = lung[1:2, ], eval_time = numeric(0))
    Condition
      Error in `survival_prob_partykit()`:
      ! `eval_time` can't be empty.

---

    Code
      survival_prob_partykit(mod, new_data = lung[1:2, ], eval_time = c(100, NA))
    Condition
      Error in `survival_prob_partykit()`:
      ! `eval_time` can't contain missing values.

