library(readxl)

dat <- read_excel("/Users/valeriaduran/Downloads/BCa 2018 & 2019 Data med-N together cm Jun2020.xlsx", sheet = "Bridged Data cm")

# BC vs HC
names(dat)
dat1 <- dat[,11:52]
# initialize a list to store the p_values for BC vs HC
u_test_BCvsHC <- vector("list", nrow(dat1))

for(i in seq_along(1: nrow(dat1))){
  u_test_BCvsHC[i] = wilcox.test(as.matrix(dat1[i,1:15]),as.matrix(dat1[i,16:42]),alternative = "two.sided", exact = FALSE)$p.value
  
}
dat1$u_test_BCvsHC <- u_test_BCvsHC
dat1 <- as.data.frame(dat1)
names(dat1)
write.csv2(dat1, "BC_utest_June19")
remove.packages("data.table")
install.packages("data.table")
dat1 <- apply(dat1,2,as.character)
write.csv(dat1, "BC_utest_June19.csv")
