library(dplyr)

TDR_folder_path <- file.path(normalizePath(Sys.getenv("ONEDRIVE"), winslash = "/"), "Stryker/Task 0003 - Stryker CSDR and Contracts CY21-25/Technical/Validation/Stryker 30 MM ECP_NGC_CY24_TDR (70317)/V1_Raw Files/A006 Tech Data_NGC_30mm Gun-2.14.25-Submission.xlsx")
#TDR_folder_path <- "C:/Users/bellenbogen/OneDrive - Technomics/Task 0003 - Stryker CSDR and Contracts CY21-25/Technical/TDR M&R Dev/Read Files/TDR/15 - Cerberus TDR Interim_Annual Submission 2019.xlsx"
#TDR_folder_path <- "C:/Users/bellenbogen/OneDrive - Technomics/Task 0003 - Stryker CSDR and Contracts CY21-25/Technical/TDR M&R Dev/Read Files/TDR/15 - Cerberus TDR Interim_Annual Submission 2019_test2.xlsx"


# debug(readflexfile::read_techdatareport)
# testl = readflexfile::read_techdatareport(file = TDR_folder_path)

debug(read_techdatareport)
testl = read_techdatareport(file = TDR_folder_path, .coerce_spec = FALSE ,.show_check = TRUE, .drop_optional = FALSE)
testl2 = read_techdatareport(file = TDR_folder_path, .coerce_spec = TRUE ,.show_check = TRUE, .data_case = "native", .drop_optional = TRUE)
testl3 = read_techdatareport(file = TDR_folder_path, .coerce_spec = TRUE ,.show_check = TRUE, .data_case = "snake", .drop_optional = TRUE)

View(testl2$`Report Metadata`)

all_equal(testl$`Report Metadata`, testl2$`Report Metadata`)
# colnames(testl2$`Report Metadata`)
# colnames(testl$`Report Metadata`)

readflexfile::techdatareport_spec$tables
readflexfile::techdatareport_spec$fields


MNR_folder_path <- file.path(normalizePath(Sys.getenv("ONEDRIVE"), winslash = "/"), "Stryker/Task 0003 - Stryker CSDR and Contracts CY21-25/Technical/Validation/Stryker 30 MM ECP_NGC_CY24_M&R (70318)/V1_Raw Files/A005 Maintenance Repair Data Model_NGC_30mm Gun - 2.14.25-Submission.xlsx")
debug(read_maintrepair)
testmnr = read_maintrepair(file = MNR_folder_path, .show_check = TRUE, .coerce_spec = TRUE, .drop_optional = TRUE)
testmnr2 = read_maintrepair(file = MNR_folder_path, .show_check = TRUE, .coerce_spec = TRUE, .data_case = "snake", .drop_optional = TRUE)

FF_3part_path_parent = "C:/Users/bellenbogen/OneDrive - Technomics/Task 0003 - Stryker CSDR and Contracts CY21-25/Technical/Validation/Stryker 30 MM ECP_NGC_CY24_FF (70304)/V1_Files PBI/"
FF_3part_path = c(paste0(FF_3part_path_parent, "Stryker Contract Plan Flex File 2.14.25 - CY24 submission-NO COM Import Export - Part 1.xlsx"),
                  paste0(FF_3part_path_parent, "Stryker Contract Plan Flex File 2.14.25 - CY24 submission-NO COM Import Export - Part 2.xlsx"),
                  paste0(FF_3part_path_parent, "Stryker Contract Plan Flex File 2.14.25 - CY24 submission-NO COM Import Export - Part 3.xlsx"))
debug(read_flexfile_3part)
testff = read_flexfile_3part(file = FF_3part_path)


debug(read_flexfile)
FF_zip = "C:/Users/bellenbogen/OneDrive - Technomics/Task 0003 - Stryker CSDR and Contracts CY21-25/Technical/Validation/Stryker 30 MM ECP_NGC_CY24_FF (70304)/V1_Files Raw/Stryker Contract Plan Flex File - Part 1 - 2.14.25 - CY24 submission-NO COM Import.zip"
testffzip = read_flexfile(file = FF_zip)

testff$CostHourTagDefinitions
testffzip$CostHourTagDefinitions
all.equal(testff$ForecastAtCompletionCostHourData, testffzip$ForecastAtCompletionCostHourData)
all_equal(testff$SummaryRemarks, testffzip$SummaryRemarks)
debug(as_flexfile)
class_test = as_flexfile(x = testff)

QDR_zip = "C:/Users/bellenbogen/OneDrive - Technomics/Task 0003 - Stryker CSDR and Contracts CY21-25/Technical/Validation/Stryker 30 MM ECP_NGC_CY24_FF (70304)/V1_Files Raw/A004 QuantityReport_NGC_30mm Gun-2.14.25-Submission Import.zip"
testqdr = read_flexfile(file = QDR_zip)
debug(as_quantityreport)
class_test = as_quantityreport(x = testqdr)
