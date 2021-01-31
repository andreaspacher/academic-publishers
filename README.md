# academic-publishers
A list of academic publishers and their scholarly journals.

The compilation of the list is generated by webscraping the following four sources:

* DOAJ
* Publons
* Scopus
* Sherpa Romeo

(The webscraping occurs in the files 01 to 04 in the `Script`-folder, mainly using R's `rvest`-package.)

The script then takes each publisher's highest journal count (each publisher has up to four journal counts that vary across the four data sources) and orders the publishers by that respective highest journal count; this is done in file 05 in the `Script`-folder.

In a further step, the script harmonizes duplicated names of publishers (based on the data in `Data\03_publishers_harmonization.txt`). 

The full (harmonized) list of the publishers and their journal counts is visible in `Output\allpublishers.xlsx`.

Finally, the publishers' websites are scraped so as to extract all of their journals, including the URL to each journal (in file 06 in the `Script`-folder). To do so, the script uses css selectors as identified for each publisher in `Data\publishers.csv`.

The outcome of this is visible in `Output\alljournals.csv` soon.
