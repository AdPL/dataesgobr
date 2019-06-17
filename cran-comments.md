## Resubmission
This is a resubmission. In this version I have:

* Replaced \dontrun by \donttest. Annotation: When a function needs to send a request to the API it will be in donttest because I do not know the time necessary for execution the request.

* The examples have been updated and checked. Now is possible to execute them without errors.

* Changed getwd and setwd in functions in order to work with tempdir(). The functions do not change the working directory now.

* Replaced path parameter to outfile parameter. The outfile parameter indicates the output directory.

## Test environments
* local macOS Sierra, R 3.6.0
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
No downstream dependencies
