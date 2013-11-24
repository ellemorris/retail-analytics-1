kfold_random_cv = function(n, k_folds = 5, date_vec = NULL){
  indices = 1 : n
  size = round(n/k_folds)
  idx_remaining = 1 : n 
  idx_list = list()
  for(fold in 1 : k_folds){
    idx_list[[fold]] = list()
    if(fold != k_folds){
      test_idx = sample(idx_remaining, size, replace = F)
      idx_remaining = setdiff(idx_remaining, test_idx)
    }
    else{
      test_idx = idx_remaining
    }
    train_idx = setdiff(indices, test_idx)
    idx_list[[fold]][["train"]] = train_idx
    idx_list[[fold]][["test"]] = test_idx
  }
  idx_list
}