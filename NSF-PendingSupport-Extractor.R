# Tabulizes NSF Current and Pending Support from PDF files
# NOTE: not efficient code; just a hack
#

# CONFIGURE THIS:
# ===============
# set path for PDF proposal PDFs
path = '.'
OUTPUT.file = file.path(path, 'output.csv')

# start analysis
library(pdftools)

# find all the proposals
file.list = dir(path, pattern='^NSF.*pdf$')
print(paste("Found",length(file.list),"files."))

# create output
r = data.frame()
for(f in file.list) {
  filename = file.path(path,f)
  print(filename)
  t = readr::read_lines(pdf_text(filename))
  m1 = grep("PI/co-PI/Senior Personnel",t)
  if(length(m1)==0) next;
  
  print(t[m1]) # output PI/co-PI/personnel names
  m1 = c(m1, length(t))
  for(j in 1:(length(m1)-1)) {
    q = t[m1[j]:m1[j+1]]
    title =  gsub(".*?:(.*)","\\1",q[grep("sal Title",q)])
    award =  gsub(".*?:(.*)","\\1",q[grep("Total Award Amount",q)]) 
    if(length(title)>length(award)) { award=c(award,rep("n/a",length(title)-length(award))) }
    startDate = gsub(".*?:(.*)","\\1",q[grep("al.*Start Date",q)])
    endDate = gsub(".*?:(.*)","\\1",q[grep("End Date",q)])
    if(length(title)>length(endDate)) { endDate=c(endDate,rep("n/a",length(title)-length(endDate))) }
    
    r = rbind(r,
              data.frame(
                proposal = f,
                PI.name = t[m1[j]],
                title,
                award,
                startDate =gsub(" ","",startDate),
                endDate = gsub(" ","",endDate)
              ))
  }
}
# remove empty lines
r=r[-which(gsub(" ","", gsub("\\$","", r$award))==""),]
r$PI.name = gsub('.*?:(.*)','\\1',r$PI.name)
r$startDate = format(as.Date(paste0("01/",r$startDate), tryFormats = c("%d/%m/%Y", "%d/%Y/%m")), format="%Y-%m")
r$endDate = format(as.Date(paste0("01/",r$endDate), tryFormats = c("%d/%m/%Y", "%d/%Y/%m")), format="%Y-%m")

write.csv(r, file=file.path(path,'output.csv'), row.names = FALSE)
