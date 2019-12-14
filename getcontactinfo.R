#' @param schoolID the 6 digit number that tabroom uses to identify the touranament host
get_entry_file = function(schoolID)
{
  url = "https://www.tabroom.com/user/enter/students.mhtml?school_id="
  url = paste(url, schoolID, sep="")
  
}

#' @param entry_file the csv file from tabroom with our entries for a given tournament
#' @param email_file the CCFS student/parent directory
#' @param out_file the name/directory to save the output file to
readInNames = function(entry_file, email_file, out_file)
{
  #table of entries
  names = read.table(file = entry_file, skip = 2, header = F, 
                     fill = T, sep = ",", stringsAsFactors = F)
  rows1 = which(names$V12 != "") #the rows in V11 that have entry names
  rows2 = which(names$V14 != "") #the rows in V13 that have entry names
  
  parsed = names$V12[rows1] #student last names
  parsed2 = names$V14[rows2] #student last names
  
  last_names= c(parsed,parsed2) #all last names
  
  #student/parent contact info table
  info <- read.table(file = email_file, skip = 0, 
                     header=T, fill=T, sep=",", stringsAsFactors = F)
  info_labelled <- info[,-1] #student info with last names removed
  
  row_names <- info[,1] #last names saved in a vector
  last_names <- sub(".* ", "", last_names) #last_names parsed
  
  row.names(info_labelled) <- row_names
  
  parent_emails = vector(mode="character")
  student_emails = vector(mode = "character")
  for(i in 1: length(last_names))
  {
    parent_emails = c(parent_emails, 
                      info_labelled[last_names[i], "Parent.E.mail"])
    student_emails = c(student_emails, info_labelled[last_names[i], "E.Mail.Address"])
  }
  
  write(x = "Parent E-mail Addresses", file = out_file, sep = "\n")
  write(x = parent_emails, file = out_file, sep="\n", append = TRUE)
  write(x = "\n", file = out_file, append = TRUE)
  write(x = "Student E-mail Addresses", file = out_file, sep = "\n", append=TRUE)
  write(x = student_emails, file = out_file, sep="\n", append = TRUE)
}

