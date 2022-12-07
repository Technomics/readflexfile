# readflexfile (development version)

# readflexfile 0.4.1

* Removed `add_id_col()` and `listindex_to_col()` re-exports from `costmisc` which were required for the legacy workflow.
* Moved `data_model_to_snake()` and `snake_to_data_model()` to `costmisc` and re-exported. Added a more general `change_case_from_spec()` to costmisc as well, also re-exported in `readflexfile`.
* Added functionality to read data from the 3-Part Template using `read_flexfile_3part()`.

# readflexfile 0.4.0

* Added in a new function `normalize_functional_categories()`. This function will add the standard functional categories to the `actualcosthourdata` and `forecastatcompletioncosthourdata` tables when the detailed functional categories are provided. This functionality always occurred when flattening the data using `flatten_data()`, but is now exported as its own function to support other use cases.
* Added in a new function `apply_flexfile()`. This function allows you to apply a function over a single or a list of flexfiles. This provides a convenient way to apply a function without knowing in advance whether the input is a single object of type 'flexfile' or a list of objects. The function is used internally with functions such as `allocate_flexfile()` but is now exported to the user.
* Added in a new function `write_flexfile()`. This function will write an object of class 'flexfile' or 'quantityreport' into the zipped collected of JSON files.
* Added in two new functions (`data_model_to_snake()` and `snake_to_data_model()`) to interchange between naming conventions (native data model notation and snake_case). Note that the formal S3 object at this time must still remain in snake_case or else the downstream functions will not know what to do. Future iterations may support either case.
* Updated the `as_*()` functions so that missing fields and tables are added into the model during the coercion.
* Optional fields (i.e., the Tags) will not be included in the data model if they are empty (#31). This is controlled using the `.drop_optional = TRUE` default in `read_flexfile()` and the `as_*()` functions.

# readflexfile 0.3.2

* Corrected bug with names not being joined in for `units_or_sublots` (#28).
* Moved `flatten_data()` generic to costmisc. This should have no impact on the user.

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
