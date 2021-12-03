# TODO

## General

+ NEWS file.

## Existing functions

+ `rang_oob_err()`. Currently only works with classification. rename to rang_num_trees().
+ `coef_to_table()`. 
    + Review/refactor helper functions.
    + Extra tests??
    + Add none_name arg to main func (already in `extract_level()`). Decide how current NAs in table
    should be displayed ("none", "", NA,...?)
    + Warning if name decomposition is ambiguous?
    + Rewrite @return (bullets)?
    
    
## New functions

These may already be in dev_code().

+ pdp functions (already written but not in package)
+ glmnet output functions
