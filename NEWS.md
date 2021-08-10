# readflexfile (development version)

* Corrected bug with names not being joined in for `units_or_sublots` (#28).

# readflexfile 0.3.1

* Minor bug fixes based on updates to other packages (#23).

# readflexfile 0.3.0

* Added in S3 class for a FlexFile and Quantity Report to track the imported dataset.
* Modified the workflow to flatten and combine data. The function `flatten_ff()` is now deprecated and has been replaced with the generic function `flatten_data()`. See the updated vignettes in `browseVignettes(package = "readflexfile")` (#15). The generic may be relocated to another package in the costverse later, but there should not be a change to the user (#16).
* The `read_ff()` function has been superseded by `read_flexfile()` to be less ambiguous in name.
* Tables missing from the input JSON file will now be added in by `read_flexfile()` with zero rows of data. This ensures that a consistent result is returned (#17).
* Tweaked the FlexFile and Quantity Data Report flatfiles to be more consistent and concise.

# readflexfile 0.2.1

* Corrected typo `period_of_performance_end_data` to `period_of_performance_end_date` in the `ordersorlots table` (#10).
* Swapped to a JSON parser in `costmisc`. This should correct errors in tables with value 'null' in the raw file.
* Added in the `.coerce_spec` option to `read_ff()` to coerce data types to those from the data model (#11).

# readflexfile 0.2.0

* First public release of readflexfile! This package is available now on GitHub and
is licensed under the GPLv3.
* Run `browseVignettes("readflexfile")` to get started!


# readflexfile 0.1.0

* First external release of readflexfile!
