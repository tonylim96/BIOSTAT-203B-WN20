*Tony Lim*

### Overall Grade: 103/110

### Quality of report: 10/10

-   Is the homework submitted (git tag time) before deadline? 

    Yes. `Jan 24, 2020, 11:35 PM PST`.

-   Is the final report in a human readable format html? 

    Yes. `html` file. 

-   Is the report prepared as a dynamic document (R markdown) for better reproducibility?

    Yes. `Rmd`.

-   Is the report clear (whole sentences, typos, grammar)? Do readers have a clear idea what's going on and how are results produced by just reading the report? 

	  Yes. 


### Correctness and efficiency of solution: 56/60

-   Q1 (10/10)

-   Q2 (19/20)
   
    \#4. (-1 pt) Note that you are including the header in counting the number of unique patients. You can use something like the following to remove the first line. 
	
  	  ```
  	  sed 1d ADMISSIONS.csv | awk -F ',' '{print $2}' | sort | uniq | wc -l
  	  ```
    
	
-   Q3 (15/15)

   


-  Q4 (12/15)
	
	 \#3. (-3 pts) Table looks crude. Use `kable` to print the table in the given format. On your table, values for `PrimeAvg` and `SampAvg` are flipped. 

	    
### Usage of Git: 10/10

-   Are branches (`master` and `develop`) correctly set up? Is the hw submission put into the `master` branch? 

    Yes.  

-   Are there enough commits? Are commit messages clear? 

    Yes. 27 commits. 

          
-   Is the hw1 submission tagged? 

    Yes. `hw1`. 

-   Are the folders (`hw1`, `hw2`, ...) created correctly? 

    Yes.
  
-   Do not put a lot auxiliary files into version control. 

	  Yes. 
	  
### Reproducibility: 8/10

-   Are the materials (files and instructions) submitted to the `master` branch sufficient for reproducing all the results? Just click the `knit` button will produce the final `html` on teaching server? (-2 pts)

	  The path `"/home/tonylim/biostat-203b-2020-winter/hw1"` in the last code chunk of `hw1sol.Rmd` is for your own directory on the server. Use relative path for easier reproducibility.
  
-   If necessary, are there clear instructions, either in report or in a separate file, how to reproduce the results?

    Yes.

### R code style: 19/20

-   [Rule 3.](https://google.github.io/styleguide/Rguide.xml#linelength) The maximum line length is 80 characters. (-1 pt)

    Some violations:
      - `autoSim.R`: line 14
  
-   [Rule 4.](https://google.github.io/styleguide/Rguide.xml#indentation) When indenting your code, use two spaces.

-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Place spaces around all binary operators (=, +, -, &lt;-, etc.). 	
	
-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Do not place a space before a comma, but always place one after a comma. 


-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Place a space before left parenthesis, except in a function call.

-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Do not place spaces around code in parentheses or square brackets.
