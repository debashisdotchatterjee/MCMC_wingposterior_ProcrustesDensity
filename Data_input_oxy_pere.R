# Read the Excel file into a matrix
Excel_file <- as.matrix(read_excel(file_path, col_names = FALSE))

# Example from our data
B1Ox1 <- as.matrix(read_excel(file_path, col_names = FALSE))  # C.oxystoma sample
P1 <- as.matrix(read_excel(file_path, col_names = FALSE))     # C.peregrinus sample
ADIN1 <- as.matrix(read_excel(file_path, col_names = FALSE))  # C.innoxius sample

# Update the specific slice of oq with the values from B1Ox1
for (i in 1:rows) 
{
    for (j in 1:cols) 
    {
     array[i, j, sample_index] <- sampleid[i, j]
    }
}

# Example from our data
B1Ox1 <- as.matrix(B1Ox1)
for(i in 1:11)
{
  for(j in 1:2)
  {
    oq[i,j,1]=B1Ox1[i,j]
  }
}

P1 <- as.matrix(P1)
for(i in 1:11)
{
  for(j in 1:2)
  {
    pq[i,j,1]=P1[i,j]
  }
}
ADIN1 <- as.matrix(ADIN1)
for(i in 1:11)
{
  for(j in 1:2)
  {
    iq[i,j,1]=ADIN1[i,j]
  }
}