# dplyr::filter works as expected

    Code
      dplyr::filter(skimmed_iris, skim_type == "numeric")
    Output
      -- Data Summary ------------------------
                                 Values
      Name                       iris  
      Number of rows             150   
      Number of columns          5     
      _______________________          
      Column type frequency:           
        numeric                  4     
      ________________________         
      Group variables            None  
      
      -- Variable type: numeric ------------------------------------------------------
        skim_variable n_missing complete_rate mean    sd  p0 p25  p50 p75 p100 hist 
      1 Sepal.Length          0             1 5.84 0.828 4.3 5.1 5.8  6.4  7.9 ▆▇▇▅▂
      2 Sepal.Width           0             1 3.06 0.436 2   2.8 3    3.3  4.4 ▁▆▇▂▁
      3 Petal.Length          0             1 3.76 1.77  1   1.6 4.35 5.1  6.9 ▇▁▆▇▂
      4 Petal.Width           0             1 1.20 0.762 0.1 0.3 1.3  1.8  2.5 ▇▁▇▅▃
    Code
      dplyr::filter(skimmed_iris, skim_type == "no_type")
    Output
      # A tibble: 0 x 15
      # i 15 variables: skim_type <chr>, skim_variable <chr>, n_missing <int>,
      #   complete_rate <dbl>, factor.ordered <lgl>, factor.n_unique <int>,
      #   factor.top_counts <chr>, numeric.mean <dbl>, numeric.sd <dbl>,
      #   numeric.p0 <dbl>, numeric.p25 <dbl>, numeric.p50 <dbl>, numeric.p75 <dbl>,
      #   numeric.p100 <dbl>, numeric.hist <chr>

# dplyr::select works as expected

    Code
      with_type
    Output
      # A tibble: 5 x 2
        skim_type skim_variable
        <chr>     <chr>        
      1 factor    Species      
      2 numeric   Sepal.Length 
      3 numeric   Sepal.Width  
      4 numeric   Petal.Length 
      5 numeric   Petal.Width  

---

    Code
      without_type
    Output
      # A tibble: 5 x 1
        numeric.mean
               <dbl>
      1        NA   
      2         5.84
      3         3.06
      4         3.76
      5         1.20

# dplyr::mutate works as expected

    Code
      input
    Output
      -- Data Summary ------------------------
                                 Values
      Name                       iris  
      Number of rows             150   
      Number of columns          5     
      _______________________          
      Column type frequency:           
        factor                   1     
        numeric                  4     
      ________________________         
      Group variables            None  
      
      -- Variable type: factor -------------------------------------------------------
        skim_variable n_missing complete_rate ordered n_unique
      1 Species               0             1 FALSE          3
        top_counts               
      1 set: 50, ver: 50, vir: 50
      
      -- Variable type: numeric ------------------------------------------------------
        skim_variable n_missing complete_rate mean    sd  p0 p25  p50 p75 p100 hist 
      1 Sepal.Length          0             1 5.84 0.828 4.3 5.1 5.8  6.4  7.9 ▆▇▇▅▂
      2 Sepal.Width           0             1 3.06 0.436 2   2.8 3    3.3  4.4 ▁▆▇▂▁
      3 Petal.Length          0             1 3.76 1.77  1   1.6 4.35 5.1  6.9 ▇▁▆▇▂
      4 Petal.Width           0             1 1.20 0.762 0.1 0.3 1.3  1.8  2.5 ▇▁▇▅▃
        mean2
      1 34.1 
      2  9.35
      3 14.1 
      4  1.44

# dplyr::slice works as expected

    Code
      input
    Output
      -- Data Summary ------------------------
                                 Values
      Name                       iris  
      Number of rows             150   
      Number of columns          5     
      _______________________          
      Column type frequency:           
        factor                   1     
        numeric                  2     
      ________________________         
      Group variables            None  
      
      -- Variable type: factor -------------------------------------------------------
        skim_variable n_missing complete_rate ordered n_unique
      1 Species               0             1 FALSE          3
        top_counts               
      1 set: 50, ver: 50, vir: 50
      
      -- Variable type: numeric ------------------------------------------------------
        skim_variable n_missing complete_rate mean    sd  p0 p25 p50 p75 p100 hist 
      1 Sepal.Length          0             1 5.84 0.828 4.3 5.1 5.8 6.4  7.9 ▆▇▇▅▂
      2 Sepal.Width           0             1 3.06 0.436 2   2.8 3   3.3  4.4 ▁▆▇▂▁

# dplyr::arrange works as expected

    Code
      dplyr::arrange(skimmed_iris, desc(numeric.mean))
    Output
      -- Data Summary ------------------------
                                 Values
      Name                       iris  
      Number of rows             150   
      Number of columns          5     
      _______________________          
      Column type frequency:           
        factor                   1     
        numeric                  4     
      ________________________         
      Group variables            None  
      
      -- Variable type: factor -------------------------------------------------------
        skim_variable n_missing complete_rate ordered n_unique
      1 Species               0             1 FALSE          3
        top_counts               
      1 set: 50, ver: 50, vir: 50
      
      -- Variable type: numeric ------------------------------------------------------
        skim_variable n_missing complete_rate mean    sd  p0 p25  p50 p75 p100 hist 
      1 Sepal.Length          0             1 5.84 0.828 4.3 5.1 5.8  6.4  7.9 ▆▇▇▅▂
      2 Petal.Length          0             1 3.76 1.77  1   1.6 4.35 5.1  6.9 ▇▁▆▇▂
      3 Sepal.Width           0             1 3.06 0.436 2   2.8 3    3.3  4.4 ▁▆▇▂▁
      4 Petal.Width           0             1 1.20 0.762 0.1 0.3 1.3  1.8  2.5 ▇▁▇▅▃

