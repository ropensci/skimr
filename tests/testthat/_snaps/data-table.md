# skim of a simple data.table produces output as expected

    Code
      skimmed_DT_letters
    Output
      -- Data Summary ------------------------
                                 Values    
      Name                       DT_letters
      Number of rows             26        
      Number of columns          1         
      Key                        NULL      
      _______________________              
      Column type frequency:               
        character                1         
      ________________________             
      Group variables            None      
      
      -- Variable type: character ---------------------------------------------------------------
        skim_variable n_missing complete_rate min max empty n_unique whitespace
      1 abc                   0             1   1   1     0       26          0

# skim of data.table produces output as expected

    Code
      skim(DT_factors)
    Output
      -- Data Summary ------------------------
                                 Values    
      Name                       DT_factors
      Number of rows             26        
      Number of columns          3         
      Key                        NULL      
      _______________________              
      Column type frequency:               
        character                1         
        factor                   1         
        numeric                  1         
      ________________________             
      Group variables            None      
      
      -- Variable type: character ---------------------------------------------------------------
        skim_variable n_missing complete_rate min max empty n_unique whitespace
      1 abc                   0             1   1   1     0       26          0
      
      -- Variable type: factor ------------------------------------------------------------------
        skim_variable n_missing complete_rate ordered n_unique top_counts   
      1 grps                  0             1 FALSE          2 AA: 18, BB: 8
      
      -- Variable type: numeric -----------------------------------------------------------------
        skim_variable n_missing complete_rate   mean    sd    p0    p25     p50   p75 p100 hist 
      1 values                0             1 0.0121 0.937 -2.21 -0.335 -0.0306 0.742 1.36 ▂▂▃▇▆

---

    Code
      skim(DT_factors)
    Output
      -- Data Summary ------------------------
                                 Values    
      Name                       DT_factors
      Number of rows             26        
      Number of columns          3         
      Key                        abc, grps 
      _______________________              
      Column type frequency:               
        character                1         
        factor                   1         
        numeric                  1         
      ________________________             
      Group variables            None      
      
      -- Variable type: character ---------------------------------------------------------------
        skim_variable n_missing complete_rate min max empty n_unique whitespace
      1 abc                   0             1   1   1     0       26          0
      
      -- Variable type: factor ------------------------------------------------------------------
        skim_variable n_missing complete_rate ordered n_unique top_counts   
      1 grps                  0             1 FALSE          2 AA: 18, BB: 8
      
      -- Variable type: numeric -----------------------------------------------------------------
        skim_variable n_missing complete_rate   mean    sd    p0    p25     p50   p75 p100 hist 
      1 values                0             1 0.0121 0.937 -2.21 -0.335 -0.0306 0.742 1.36 ▂▂▃▇▆

---

    Code
      skim(DF_factors)
    Output
      -- Data Summary ------------------------
                                 Values    
      Name                       DF_factors
      Number of rows             26        
      Number of columns          3         
      _______________________              
      Column type frequency:               
        character                1         
        factor                   1         
        numeric                  1         
      ________________________             
      Group variables            None      
      
      -- Variable type: character ---------------------------------------------------------------
        skim_variable n_missing complete_rate min max empty n_unique whitespace
      1 abc                   0             1   1   1     0       26          0
      
      -- Variable type: factor ------------------------------------------------------------------
        skim_variable n_missing complete_rate ordered n_unique top_counts   
      1 grps                  0             1 FALSE          2 AA: 18, BB: 8
      
      -- Variable type: numeric -----------------------------------------------------------------
        skim_variable n_missing complete_rate   mean    sd    p0    p25     p50   p75 p100 hist 
      1 values                0             1 0.0121 0.937 -2.21 -0.335 -0.0306 0.742 1.36 ▂▂▃▇▆

---

    Code
      skim(tibble_factors)
    Output
      -- Data Summary ------------------------
                                 Values        
      Name                       tibble_factors
      Number of rows             26            
      Number of columns          3             
      _______________________                  
      Column type frequency:                   
        character                1             
        factor                   1             
        numeric                  1             
      ________________________                 
      Group variables            None          
      
      -- Variable type: character ---------------------------------------------------------------
        skim_variable n_missing complete_rate min max empty n_unique whitespace
      1 abc                   0             1   1   1     0       26          0
      
      -- Variable type: factor ------------------------------------------------------------------
        skim_variable n_missing complete_rate ordered n_unique top_counts   
      1 grps                  0             1 FALSE          2 AA: 18, BB: 8
      
      -- Variable type: numeric -----------------------------------------------------------------
        skim_variable n_missing complete_rate   mean    sd    p0    p25     p50   p75 p100 hist 
      1 values                0             1 0.0121 0.937 -2.21 -0.335 -0.0306 0.742 1.36 ▂▂▃▇▆

