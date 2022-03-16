

# 1 
1. What organizational/work flow practice that I have discussed do you think is unnecessary or impractical for daily data analytic tasks? Why? 


I think when the data can be analyzed directly with very little preprocessing, or when there's very little data processing code, very little analysis code, very little visualization, You can include reading data, cleaning data, analyzing data, and visualizing data in a single code file without using Multiple scripts. It's more convenient and saves time.


# 2
2. What organizational/work flow practice have I not included that you think would help reduce error or improve reproducibility? Why?


2.1 For example, in earlier versions of read.csv(), the strings As Factors parameter defaults to True.
However, in newer versions such as Version 4.1.1, the strings As Factors parameter of read. CSV defaults to False, so you can specify these parameters explicitly to prevent version-related errors.;
 
2.2 Note The default character set code may vary with the operating system (OS). 
The default character set code of the Windows OS may be different from that of the MAC OS.

2.3. When reading data, use excel format as little as possible, because R funcion generally needs to configure JRE environment to read Excel data.
So you can convert excel data directly to CSV or TXT format, so that you can read the data set with R built-in functions without setting up any environment.

