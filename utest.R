library(readxl)

dat <- read_excel(x", sheet = "")

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

dat1 <- apply(dat1,2,as.character)
write.csv(dat1, "BC")
