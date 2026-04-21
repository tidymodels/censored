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

