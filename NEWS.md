# readflexfile (development version)

* Corrected typo `period_of_performance_end_data` to `period_of_performance_end_date` in the `ordersorlots table` (#10).
* Swapped to a JSON parser in `costmisc`. This should correct errors in tables with value 'null' in the raw file.
* Added in the `.coerce_spec` option to `read_ff()` to coerce data types to those from the data model (#11)

# readflexfile 0.2.0

* First public release of readflexfile! This package is available now on GitHub and
is licensed under the GPLv3.
* Run `browseVignettes("readflexfile")` to get started!


# readflexfile 0.1.0

* First external release of readflexfile!
