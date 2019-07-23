## Plans for future:
* adding parallelization (multicore) in extraction on Windows & Linux
* speeding things up
* release on CRAN

## News
##### **2019-07-...**, v1.0.0 (package api shift + release):
* there is only **one** function that downloads just the nested lists - *download_lighthouse()*. Parameters *output_type* & *long_result* are now obsolete (and will break the function) and were moved into specialised extraction functions (see below)
* to extract the data from the export in the form of data frames, use new functions: *extract_lighthouse_...()*. You can choose to export just the category scores, most of the data from all audits or detailed data from choosen audit (including errors & recommendations)
* pagespeedParseR should be able to handle thousands of URLs thanks to the new cache functionality! The results are cached into temporary binary file 'db.llo' saved in a current directory and should not cause memory allocation issues
* you can choose to export the API output as a binary file to location of your choosing and can access its data without loading it up into the global enviroment. You can just use the pointer overloading. See more in *download_lighthouse()* documentation
* if you happen to find a bug, don't hesitate to inform me and/or add an [Issue](https://github.com/Leszek-Sieminski/pagespeedParseR/issues)

##### **2019-07-10**, v0.3.1.9000 (bugfixes):
* small bugfixes to *long_result = T* parameter in *download_lighthouse(..., output_type = "simple")*
* small bugfixes to tests

##### **2019-07-05**, v0.3.0.9000 (Lighthouse overhaul):
* big overhaul of *download_lighthouse()* function. Parsing to data frame with *output_type = "simple"* parameter will now provide much more data. However, **it can generate literally hundreds/thousands of columns** (up to ~2500). What is more, the **number of columns IS NOT STABLE**, because it depends on the number of found errors and/or their type
* to ease the pain of dealing with such data frames, I added *long_result* parameter that defaults to *FALSE*. Setting it to TRUE will force the function to spread the data frame into messy, long-like form that I hope to be easier to comprehend
* the behaviour of download_lighthouse(output_type = "raw") or other functions didn't change

