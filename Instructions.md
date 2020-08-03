---
title: "Instructions"
author: "Nisitha Jayatilleke"
date: "30 May 2019"
output: html_document
---
## BiomaRt annotation application

This application was developed to allow easy annotation of genomic data through the biomaRt R package. BiomaRt is an API package which queries the ensembl databases for current annotations against given inputs. 

### Using the applicaiton

Click on the BiomaRt annotate tab at the top of the screen. The application will then begin to load the biomaRt connection. By default it will connect to the Asia server with the human annotation. If the server is unresponsive an error will appear, you may then select a different mirror to access the ensembl database. If the mart is loaded then it is fine to proceed to the next step.

One the mart has been loaded you will be given the option to upload a tab-delimited file containing the information which you wish to annotate. Currently the following annotation targets are supported:

* Entrez ID

If there are any other annotation targets which exist in the ensembl databases that you wish to include please contact the Author of the application (see About section).

Once the user data has been loaded you must then select the column of the file which contains the input data (data to be queried for annotation). Additionally, you must select the type of data that is being input (eg. Gene symbols, entrez IDs, ensembl IDs etc.). 

Finally, select the attributes you wish to annotate by click on the rows in the attribute table. The selected attributes will show up on the right hand side. To remove an attribute from the list, simply deselect the row. NOTE: there are multiple pages to the table containing attributes to annotate. 

Once all the parameters are acceptable, you can submit the job. The job will take a moment to finish. Once the job has finished a table which contains the original data plus the annotated data will appear. Below this table will be a download link that can be used to download the given table. If there is an error while the job is running, a warning message will appear. If this happens, simply change the biomaRt mirror redo the steps from the beginning.

### Troubleshooting

If any bugs or errors are hindering the use of the application, please contact Nisitha Jayatilleke or Chelsea Mayoh (see About section for contact information).
