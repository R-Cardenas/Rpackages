#' Converts MAF to a VCF files.
#'
#' This script will take the MAF file and convert it to a minimal
#' VCF file. This will REMOVE large amounts of VITAL information.
#'
#'
#' @param infile Path MAF file
#' @return A VCF file
#' @export

maf2vcf <- function(maf){
  maf1 <- read.delim(maf,
                     "\t", stringsAsFactors = FALSE, header = TRUE) %>%
    data.frame()
  msg1 <- paste0(maf, " file loaded")
  print(msg1)

  VCF <- data.frame("CHROM" = maf1$Chromosome,
                    "POS"= maf1$Start_Position,
                    "REF"= maf1$Reference_Allele,
                    "ALT"= maf1$Tumor_Seq_Allele2,
                    "QUAL"= ".",
                    "FILTER"= ".",
                    "AF"= maf1$AF,
                    "DP" = maf1$DP)
  print("VCF created")

  VCF2 <- VCF %>%
    mutate(INFO = paste0("AF=",AF,"; DP=",DP)) %>%
    dplyr::select(!c(AF,DP))

  print("AF and DP added")

  name <- str_extract(x, "(?<=\\/)S.*")
  name2 <- str_remove(name,".maf")
  name3 <- paste0(name2,".vcf")
  print("File name created")

  write.table(VCF2, file = name3, row.names=FALSE, sep="\t", quote = FALSE)
  print("table written")
}
