# Using skim_tee prints returns the object

    Code
      skim_object <- skim_tee(chickwts)
    Output
      -- Data Summary ------------------------
                                 Values
      Name                       data  
      Number of rows             71    
      Number of columns          2     
      _______________________          
      Column type frequency:           
        factor                   1     
        numeric                  1     
      ________________________         
      Group variables            None  
      
      -- Variable type: factor -------------------------------------------------------
        skim_variable n_missing complete_rate ordered n_unique
      1 feed                  0             1 FALSE          6
        top_counts                        
      1 soy: 14, cas: 12, lin: 12, sun: 12
      
      -- Variable type: numeric ------------------------------------------------------
        skim_variable n_missing complete_rate mean   sd  p0  p25 p50  p75 p100 hist 
      1 weight                0             1 261. 78.1 108 204. 258 324.  423 ▆▆▇▇▃

# skim_tee prints only selected columns, but returns full object

    Code
      obj <- skim_tee(iris, Species)
    Output
      -- Data Summary ------------------------
                                 Values
      Name                       data  
      Number of rows             150   
      Number of columns          5     
      _______________________          
      Column type frequency:           
        factor                   1     
      ________________________         
      Group variables            None  
      
      -- Variable type: factor -------------------------------------------------------
        skim_variable n_missing complete_rate ordered n_unique
      1 Species               0             1 FALSE          3
        top_counts               
      1 set: 50, ver: 50, vir: 50

# skim_tee supports dplyr helpers

    Code
      obj <- skim_tee(iris, starts_with("Sepal"))
    Output
      -- Data Summary ------------------------
                                 Values
      Name                       data  
      Number of rows             150   
      Number of columns          5     
      _______________________          
      Column type frequency:           
        numeric                  2     
      ________________________         
      Group variables            None  
      
      -- Variable type: numeric ------------------------------------------------------
        skim_variable n_missing complete_rate mean    sd  p0 p25 p50 p75 p100 hist 
      1 Sepal.Length          0             1 5.84 0.828 4.3 5.1 5.8 6.4  7.9 ▆▇▇▅▂
      2 Sepal.Width           0             1 3.06 0.436 2   2.8 3   3.3  4.4 ▁▆▇▂▁

# Skim_tee works with groups

    Code
      obj <- skim_tee(iris_grouped, Sepal.Length, skim_fun = my_skim)
    Output
      -- Data Summary ------------------------
                                 Values 
      Name                       data   
      Number of rows             150    
      Number of columns          5      
      _______________________           
      Column type frequency:            
        numeric                  1      
      ________________________          
      Group variables            Species
      
      -- Variable type: numeric ------------------------------------------------------
        skim_variable Species    n_missing complete_rate mean    sd  p0  p25 p50 p75
      1 Sepal.Length  setosa             0             1 5.01 0.352 4.3 4.8  5   5.2
      2 Sepal.Length  versicolor         0             1 5.94 0.516 4.9 5.6  5.9 6.3
      3 Sepal.Length  virginica          0             1 6.59 0.636 4.9 6.22 6.5 6.9
        p100
      1  5.8
      2  7  
      3  7.9

