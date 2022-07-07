one_sided_diff.test <- function(x,y) {
  #___________________________________________________________________________________
  # Program:    one_sided_diff.test
  # File:       One_side_diff_test.R
  # Version:    V1.0
  # Date:       05.07.22
  # Function:  Selects one-sided (>), non-paired, T.test with equal or unequal variance or a wilcox test
  #            based on conditions below:
  #            
  #          	- CLT (are there 30 samples?)
  #	          - Are there enough (10) samples to test for a normal distribution?
  #            -Is the variance is equal?
  
  # Author:     Zak Downey
  #___________________________________________________________________________________
  
  
  # check samples sizes (CLT 30)
  CLT<-FALSE
  len_x<-length(x)
  len_y<-length(y)
  if (len_x >=30 && len_y >=30) {
    CLT=TRUE
    print("CLT is true")
    
  } else { print("CLT is false")}
  # 10 values?  
  ten_values<-FALSE
  if (len_x >=10 && len_y >=10) {
    
    ten_values=TRUE
    print("Samples have 10 or more values")
  } else { print("Samples have less than 10 values")}
  #Normally distribution?  
  norm_dist<-FALSE
  x_p1<-NULL
  y_p1<-NULL
  if (CLT==FALSE && ten_values==TRUE) {
    x_p1<-c(x_p1,shapiro.test(x)$p.value)
    y_p1<-c(y_p1,shapiro.test(y)$p.value)
    
    
    if (x_p1 >0.05 && y_p1 >0.05) {
      norm_dist=TRUE
      
      print("Data is normally distributed")
    }else{print("Data is NOT normally distributed")}} 
  
  # Equal variance?
  equal_var<-FALSE
  p_val_xy=NULL
  p_val_xy<-c(p_val_xy,var.test(x,y)$p.value)
  if (p_val_xy >0.05) {
    equal_var=TRUE
    print("Variance is equal")
  } else { print("Variance is not equal")}
  
  #t.test equal variance
  ttest_eqVar_p<-NULL
  if (CLT==TRUE && equal_var==TRUE || ten_values==TRUE && norm_dist==TRUE &&
      equal_var==TRUE) {
    ttest_eqVar_p<-c(ttest_eqVar_p, t.test(x,y,alternative ="greater",
                                           paired=FALSE,var.equal=TRUE)$p.value)
    print("t-test /w equal variance, p:")
    print(ttest_eqVar_p)
  }                                   
  #t.test unequal variance  
  ttest_uneqVar_p<-NULL
  if (CLT==TRUE && equal_var==FALSE || ten_values==TRUE && norm_dist==TRUE &&
      equal_var==FALSE) {
    ttest_uneqVar_p<-c(ttest_uneqVar_p, t.test(x,y,alternative ="greater",
                                               paired=FALSE,var.equal=FALSE)$p.value)
    print("t-test /w un-equal variance, p:")
    print(ttest_uneqVar_p)
  }
  
  # wilcox test
  wilcox_p<-NULL
  if (CLT==FALSE && ten_values==TRUE && norm_dist==FALSE ||
      ten_values==FALSE) {
    wilcox_p<-c(wilcox_p,wilcox.test(x,y,alternate="greater",
                                     paired=FALSE,conf.int=0.95)$p.value)
    print("Mann-Whitney test, p:")
    print(wilcox_p)
  }
}

#___________________________________________________________________________________
#Validation 

# 1.  normally distributed data <10 values 
test1<-rnorm(9,2,3)
test2<-rnorm(6,2,3)
one_sided_diff.test(test1,test2)
wilcox.test(test1,test2,alternate="greater",paired=FALSE,conf.int=0.95)$p.value

# 2. normally distributed data >10 values
test3<-rnorm(11,2,3)
test4<-rnorm(30,2,3)
one_sided_diff.test(test3,test4)
t.test(test3,test4,alternative ="greater",paired=FALSE,var.equal=TRUE)$p.value

# 3. non normally distributed data
wildtype <- c(560, 968, 3297, 1200, 858, 646, 992, 2507, 2037, 546, 2929,
              1171, 1389, 1958, 3149, 1165, 2257, 2120, 65, 1571)
knockout <- c(589, 232, 983, 2597, 827, 1363, 634, 12, 643, 1889, 2840,
              1291, 939, 811, 3290, 525, 90, 543, 2400, 3012) 

one_sided_diff.test(wildtype,knockout)
wilcox.test(wildtype,knockout,alternate="greater",
            paired=FALSE,conf.int=0.95)$p.value
#___________________________________________________________________________________




