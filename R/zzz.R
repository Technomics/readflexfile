
.onAttach <- function(libname, pkgname) {
  start_msg <- paste0("costverse::", pkgname, ". Copyright \U00A9 2020 by Technomics, Inc. All rights reserved.",
                     "\n\n",
                     "Run 'citation(\"", pkgname, "\")' for information on how to cite in your own work.")

  packageStartupMessage(start_msg)
}
