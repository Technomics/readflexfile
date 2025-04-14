library(dplyr)

TDR_folder_path <- "C:/Users/bellenbogen/OneDrive - Technomics/Task 0003 - Stryker CSDR and Contracts CY21-25/Technical/Validation/Stryker 30 MM ECP_NGC_CY24_TDR (70317)/V1_Raw Files/A006 Tech Data_NGC_30mm Gun-2.14.25-Submission.xlsx"
#TDR_folder_path <- "C:/Users/bellenbogen/OneDrive - Technomics/Task 0003 - Stryker CSDR and Contracts CY21-25/Technical/TDR M&R Dev/Read Files/TDR/15 - Cerberus TDR Interim_Annual Submission 2019.xlsx"
#TDR_folder_path <- "C:/Users/bellenbogen/OneDrive - Technomics/Task 0003 - Stryker CSDR and Contracts CY21-25/Technical/TDR M&R Dev/Read Files/TDR/15 - Cerberus TDR Interim_Annual Submission 2019_test2.xlsx"


# debug(readflexfile::read_techdatareport)
# testl = readflexfile::read_techdatareport(file = TDR_folder_path)

debug(read_techdatareport)
testl = read_techdatareport(file = TDR_folder_path, .coerce_spec = FALSE ,.show_check = TRUE, .data_case = "native", .drop_optional = FALSE)
testl2 = read_techdatareport(file = TDR_folder_path, .coerce_spec = TRUE ,.show_check = TRUE, .data_case = "native", .drop_optional = FALSE)
testl3 = read_techdatareport(file = TDR_folder_path)

View(testl2$`Report Metadata`)

all_equal(testl$`Report Metadata`, testl2$`Report Metadata`)
# colnames(testl2$`Report Metadata`)
# colnames(testl$`Report Metadata`)

readflexfile::techdatareport_spec$tables
readflexfile::techdatareport_spec$fields


