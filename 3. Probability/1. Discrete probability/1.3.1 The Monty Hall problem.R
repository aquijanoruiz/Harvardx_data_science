B<- 10000
stick <- replicate(B, {
  doors<- as.character(1:3) #1, 2, 3 are not numbers but characters
  prize<- sample(c("car","goat","goat")) #Randomly assigns the cars and the two goats to different doors
  prize_door<- doors[prize=="car"] #This indexes the first column with the second and assigns a number to the car.BE CAREFUL
  my_pick<- sample(doors,1) #Randomly picks one of the doors
  show<- sample(doors[!doors %in% c(my_pick, prize_door)],1) #Randomly picks one of the doors not chosen
  stick<- my_pick 
  stick== prize_door
})
mean(stick)

B<- 10000
switch <- replicate(B, {
  doors<- as.character(1:3) #1, 2, 3 are not numbers but characters
  prize<- sample(c("car","goat","goat")) #Randomly assigns the cars and the two goats to different doors
  prize_door<- doors[prize=="car"] #This indexes the first column with the second.BE CAREFUL
  my_pick<- sample(doors,1) #Randomly picks one of the doors
  show<- sample(doors[!doors %in% c(my_pick, prize_door)],1) #Randomly picks one of the doors not chosen
  stick<- my_pick
  switch<- doors[!doors %in% c(my_pick,show)] #Randomly switches to one of the two doors left after the judge shows one
  switch== prize_door
})
mean(switch)
