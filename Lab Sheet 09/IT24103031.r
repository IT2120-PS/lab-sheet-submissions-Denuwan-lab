setwd("C:\\Users\\DENUWAN\\Desktop\\IT24103031")
#Exercise
# i.
bake <- rnorm(25, mean = 45, sd = 2)
bake

#ii. 
result <- t.test(bake, mu = 46, alternative = "less")
result

# Extract specific values
result$statistic  
result$p.value     
result$conf.int    

