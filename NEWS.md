# readflexfile 0.5.2

* Bug fix and some minor code re-factoring. (#55)

# readflexfile 0.5.1

* Added a `.check = TRUE` argument to `create_flexfile_family()` to skip checking if the metadata matches between the reports. Added a thin wrapper `force_flexfile_family()` which uses to `.check = FALSE`

# readflexfile 0.5.0

* **MAJOR CHANGE:** Changed the default case in `read_flexfile()` to match that of the data model instead of a transformed snake_case representation. Since the first release of readflexfile, all tables and fields were renamed according to a file specification. This has ultimately led to confusion since users need to know both the original nomenclature from the "native" data model and the new names for use in readflexfile.
  * This means that `read_flexfile()` now returns different results than it has in past versions. To maintain backwards compatibility, you can use the `.data_case = "snake"` argument. This will return the old results. This can be used as a stop gap, but we *strongly recommend rewriting old code to support the new names*. We have done this on our own related tool set. While it a bit annoying, it does not take very long.
  * The function `costmisc::native_to_snake_case()` can also be used to convert from the native case to the legacy snake_case.
  * The 'flexfile' and 'quantitydata' classes will now store the case as an attribute.
  * At some point, we will remove support for the snake_case names.
* Added in support for "Flexfile Families" with the new 'flexfile_family' type. This type stores a list of related reports within the Flexfile Family of reports
* Added in experimental functions to read in additional data formats. These read from the Excel sources rather than JSON at this point, so they may be less reliable. Each format includes it's own S3 type and is compatible for the new FlexFile Family feature.
  * `read_maintrepair()` reads the M&R report from the Excel template into an object of class 'maintrepair'.
  * `read_techdatareport()` reads the M&R report from the Excel template into an object of class 'techdatareport'.

# readflexfile 0.4.2

* Added in a new function `normalize_units_or_sublots()`. This function will add the EndItemID and OrderOrLotID fields to `actualcosthourdata` and `forecastatcompletioncosthourdata` tables when the UnitOrSublotID is provided instead. This functionality always occurred when flattening the data using `flatten_data()`, but is now exported as its own function to support other use cases.

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
