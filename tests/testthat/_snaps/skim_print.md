# Skim prints a header for the entire output and each type

    Code
      input <- skim(iris)
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
    Code
      input$numeric.hist <- NULL
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
        skim_variable n_missing complete_rate mean    sd  p0 p25  p50 p75 p100
      1 Sepal.Length          0             1 5.84 0.828 4.3 5.1 5.8  6.4  7.9
      2 Sepal.Width           0             1 3.06 0.436 2   2.8 3    3.3  4.4
      3 Petal.Length          0             1 3.76 1.77  1   1.6 4.35 5.1  6.9
      4 Petal.Width           0             1 1.20 0.762 0.1 0.3 1.3  1.8  2.5

# Skim prints a special header for grouped data frames

    Code
      skim(dplyr::group_by(iris, Species))
    Output
      -- Data Summary ------------------------
                                 Values                      
      Name                       dplyr::group_by(iris, Spe...
      Number of rows             150                         
      Number of columns          5                           
      _______________________                                
      Column type frequency:                                 
        numeric                  4                           
      ________________________                               
      Group variables            Species                     
      
      -- Variable type: numeric ------------------------------------------------------
         skim_variable Species    n_missing complete_rate  mean    sd  p0  p25  p50
       1 Sepal.Length  setosa             0             1 5.01  0.352 4.3 4.8  5   
       2 Sepal.Length  versicolor         0             1 5.94  0.516 4.9 5.6  5.9 
       3 Sepal.Length  virginica          0             1 6.59  0.636 4.9 6.22 6.5 
       4 Sepal.Width   setosa             0             1 3.43  0.379 2.3 3.2  3.4 
       5 Sepal.Width   versicolor         0             1 2.77  0.314 2   2.52 2.8 
       6 Sepal.Width   virginica          0             1 2.97  0.322 2.2 2.8  3   
       7 Petal.Length  setosa             0             1 1.46  0.174 1   1.4  1.5 
       8 Petal.Length  versicolor         0             1 4.26  0.470 3   4    4.35
       9 Petal.Length  virginica          0             1 5.55  0.552 4.5 5.1  5.55
      10 Petal.Width   setosa             0             1 0.246 0.105 0.1 0.2  0.2 
      11 Petal.Width   versicolor         0             1 1.33  0.198 1   1.2  1.3 
      12 Petal.Width   virginica          0             1 2.03  0.275 1.4 1.8  2   
          p75 p100 hist 
       1 5.2   5.8 ▃▃▇▅▁
       2 6.3   7   ▂▇▆▃▃
       3 6.9   7.9 ▁▃▇▃▂
       4 3.68  4.4 ▁▃▇▅▂
       5 3     3.4 ▁▅▆▇▂
       6 3.18  3.8 ▂▆▇▅▁
       7 1.58  1.9 ▁▃▇▃▁
       8 4.6   5.1 ▂▂▇▇▆
       9 5.88  6.9 ▃▇▇▃▂
      10 0.3   0.6 ▇▂▂▁▁
      11 1.5   1.8 ▅▇▃▆▁
      12 2.3   2.5 ▂▇▆▅▇

# Skim lists print as expected

    Code
      partition(skimmed)
    Output
      $factor
      
      -- Variable type: factor -------------------------------------------------------
        skim_variable n_missing complete_rate ordered n_unique top_counts             
      1 Species               0             1 FALSE          3 set: 50, ver: 50, vir:~
      
      $numeric
      
      -- Variable type: numeric ------------------------------------------------------
        skim_variable n_missing complete_rate mean    sd  p0 p25  p50 p75 p100 hist 
      1 Sepal.Length          0             1 5.84 0.828 4.3 5.1 5.8  6.4  7.9 ▆▇▇▅▂
      2 Sepal.Width           0             1 3.06 0.436 2   2.8 3    3.3  4.4 ▁▆▇▂▁
      3 Petal.Length          0             1 3.76 1.77  1   1.6 4.35 5.1  6.9 ▇▁▆▇▂
      4 Petal.Width           0             1 1.20 0.762 0.1 0.3 1.3  1.8  2.5 ▇▁▇▅▃
      

# knit_print produces expected results

    Code
      cat(input)
    Output
      
      Table: Data summary
      
      |                         |     |
      |:------------------------|:----|
      |Name                     |iris |
      |Number of rows           |150  |
      |Number of columns        |5    |
      |_______________________  |     |
      |Column type frequency:   |     |
      |factor                   |1    |
      |numeric                  |4    |
      |________________________ |     |
      |Group variables          |None |
      
      
      **Variable type: factor**
      
      |skim_variable | n_missing| complete_rate|ordered | n_unique|top_counts                |
      |:-------------|---------:|-------------:|:-------|--------:|:-------------------------|
      |Species       |         0|             1|FALSE   |        3|set: 50, ver: 50, vir: 50 |
      
      
      **Variable type: numeric**
      
      |skim_variable | n_missing| complete_rate| mean|   sd|  p0| p25|  p50| p75| p100|hist  |
      |:-------------|---------:|-------------:|----:|----:|---:|---:|----:|---:|----:|:-----|
      |Sepal.Length  |         0|             1| 5.84| 0.83| 4.3| 5.1| 5.80| 6.4|  7.9|▆▇▇▅▂ |
      |Sepal.Width   |         0|             1| 3.06| 0.44| 2.0| 2.8| 3.00| 3.3|  4.4|▁▆▇▂▁ |
      |Petal.Length  |         0|             1| 3.76| 1.77| 1.0| 1.6| 4.35| 5.1|  6.9|▇▁▆▇▂ |
      |Petal.Width   |         0|             1| 1.20| 0.76| 0.1| 0.3| 1.30| 1.8|  2.5|▇▁▇▅▃ |
      

# knit_print works with skim summaries

    Code
      cat(knitr::knit_print(summarized))
    Output
      Table: Data summary
      
      |                         |     |
      |:------------------------|:----|
      |Name                     |iris |
      |Number of rows           |150  |
      |Number of columns        |5    |
      |_______________________  |     |
      |Column type frequency:   |     |
      |factor                   |1    |
      |numeric                  |4    |
      |________________________ |     |
      |Group variables          |None |

# knit_print appropriately falls back to tibble printing

    Code
      input <- knitr::knit_print(reduced)
    Output
      # A tibble: 5 x 2
        skim_variable numeric.mean
        <chr>                <dbl>
      1 Species              NA   
      2 Sepal.Length          5.84
      3 Sepal.Width           3.06
      4 Petal.Length          3.76
      5 Petal.Width           1.20

# Summaries can be suppressed within knitr

    Code
      cat(knitr::knit_print(skimmed, options = options))
    Output
      
      
      
      **Variable type: factor**
      
      |skim_variable | n_missing| complete_rate|ordered | n_unique|top_counts                |
      |:-------------|---------:|-------------:|:-------|--------:|:-------------------------|
      |Species       |         0|             1|FALSE   |        3|set: 50, ver: 50, vir: 50 |
      
      
      **Variable type: numeric**
      
      |skim_variable | n_missing| complete_rate| mean|   sd|  p0| p25|  p50| p75| p100|hist  |
      |:-------------|---------:|-------------:|----:|----:|---:|---:|----:|---:|----:|:-----|
      |Sepal.Length  |         0|             1| 5.84| 0.83| 4.3| 5.1| 5.80| 6.4|  7.9|▆▇▇▅▂ |
      |Sepal.Width   |         0|             1| 3.06| 0.44| 2.0| 2.8| 3.00| 3.3|  4.4|▁▆▇▂▁ |
      |Petal.Length  |         0|             1| 3.76| 1.77| 1.0| 1.6| 4.35| 5.1|  6.9|▇▁▆▇▂ |
      |Petal.Width   |         0|             1| 1.20| 0.76| 0.1| 0.3| 1.30| 1.8|  2.5|▇▁▇▅▃ |
      

# Skim lists have a separate knit_print method

    Code
      cat(knit_print(skim_list))
    Output
      
      
      
      **Variable type: factor**
      
      |skim_variable | n_missing| complete_rate|ordered | n_unique|top_counts                |
      |:-------------|---------:|-------------:|:-------|--------:|:-------------------------|
      |Species       |         0|             1|FALSE   |        3|set: 50, ver: 50, vir: 50 |
      
      
      **Variable type: numeric**
      
      |skim_variable | n_missing| complete_rate| mean|   sd|  p0| p25|  p50| p75| p100|hist  |
      |:-------------|---------:|-------------:|----:|----:|---:|---:|----:|---:|----:|:-----|
      |Sepal.Length  |         0|             1| 5.84| 0.83| 4.3| 5.1| 5.80| 6.4|  7.9|▆▇▇▅▂ |
      |Sepal.Width   |         0|             1| 3.06| 0.44| 2.0| 2.8| 3.00| 3.3|  4.4|▁▆▇▂▁ |
      |Petal.Length  |         0|             1| 3.76| 1.77| 1.0| 1.6| 4.35| 5.1|  6.9|▇▁▆▇▂ |
      |Petal.Width   |         0|             1| 1.20| 0.76| 0.1| 0.3| 1.30| 1.8|  2.5|▇▁▇▅▃ |
      

# You can yank a type from a skim_df and call knit_print

    Code
      cat(knit_print(skim_one))
    Output
      
      
      **Variable type: factor**
      
      |skim_variable | n_missing| complete_rate|ordered | n_unique|top_counts                |
      |:-------------|---------:|-------------:|:-------|--------:|:-------------------------|
      |Species       |         0|             1|FALSE   |        3|set: 50, ver: 50, vir: 50 |
      
      

# Skim falls back to tibble::print.tbl() appropriately

    Code
      input <- skim(iris)
      dplyr::select(input, numeric.mean)
    Output
      # A tibble: 5 x 1
        numeric.mean
               <dbl>
      1        NA   
      2         5.84
      3         3.06
      4         3.76
      5         1.20

# Print focused objects appropriately

    Code
      focus(skimmed, n_missing)
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
        skim_variable n_missing
      1 Species               0
      
      -- Variable type: numeric ------------------------------------------------------
        skim_variable n_missing
      1 Sepal.Length          0
      2 Sepal.Width           0
      3 Petal.Length          0
      4 Petal.Width           0

# Support for smaller consoles can be set with the width option

    Code
      skim(iris)
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

# Table header width can be controlled by an option

    Code
      skimmed
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

# skimr creates appropriate output for Jupyter

    Code
      skimmed
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

