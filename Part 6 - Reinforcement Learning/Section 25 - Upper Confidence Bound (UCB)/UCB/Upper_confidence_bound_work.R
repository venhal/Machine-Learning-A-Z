# Upper confidence Bound

# Importing the data
dataset = read.csv("Ads_CTR_Optimisation.csv")

# Implementing UCB
d = 10
N = 10000
ads_selected = 0
numbers_of_selcections = integer(d)
sum_of_rewards = integer(d)
total_reward = 0
for (n in 1 : N){
  max_upper_bound = 0
  ad = 0
  for(i in 1 : d){
    if(numbers_of_selcections[i] > 0){
      average_reward = sum_of_rewards[i] / numbers_of_selcections[i]
      delta_i = sqrt(3/2 * log(n) / numbers_of_selcections[i])
      upper_bound = average_reward + delta_i
    }
    else{
      upper_bound = 1e400
    }
    if(upper_bound > max_upper_bound){
      max_upper_bound = upper_bound
      ad = i
    }
  }
  ads_selected = append(ads_selected, ad)
  numbers_of_selcections[ad] = numbers_of_selcections[ad] + 1
  reward = dataset[n, ad]
  sum_of_rewards[ad] = sum_of_rewards[ad] + reward
  total_reward = total_reward + reward
}

# Visualizing the result with hist
hist(ads_selected,
     col = "blue", 
     main = "Histogram of Ads", 
     xlab = "Ads", 
     ylab = "Number of each ad was selected")