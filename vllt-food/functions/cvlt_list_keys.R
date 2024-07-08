age_groups <- list(
  "16_19" = c(0, 20),
  "20_29" = c(20, 30),
  "30_44" = c(30, 45),
  "45_59" = c(45, 60),
  "60_69" = c(60, 70)
)

#updated (09/22/23): DE requested to split the intrusion for non-food categories

ListA <- list(
    c("cfh", "brownies", "fries", "cookies", "chips"),
    
    c("cfl", "spinach", "onion", "celery", "cabbage"),
    
    c("cn", "bookcase", "cabinet", "lamp", "desk"),
    
    c("ct", "truck", "motorcycle", "taxi", "boat"),
    
    c("IFH", "intrusion_IFH", 
      "intrusion_IFH (Food High Calories)",
      "food", "candy", "soda",  
      "cheese", "hotdog", "taco", "pizza", "hamburger"),
    
    c("IFL", "intrusion_IFL",
      "intrusion_IFL (Food Low Calories)",
      "potato", "cauliflower", 
      "broccoli", "carrot", "beets", "raspberries",
      "kale", "lettuce", "cucumber", "turnip", "corn",
      "radishes"),
    
    c("IN1",  
      "intrusion_IN (Neutral)",
      "dresser", "drill", "rat", "filling cabinet",
      "cupboard", "trumpet", "balcony", "piano", "cellular",
      "attic", "shelves", "chair", "books", "cabin", "fridge",
      "notebook", "bouquet", "book", "shelf", 
      "chairs",
      "bridge", "bookshelf", "couch", "table", "closet",
      "basement", "garage", "patio", "violin", "guitar",
      "clarient", "saxophone"
      ),
    
    c("IN2",
      "train","car", "bus"),
    
    c("rep", "r", "repeat")
    
  )
  
ListB<- list(c("cfh", "hotdog", "taco", "pizza", "hamburger"),
       c("cfl", "cucumber", "turnip", "corn", "radishes"),
       c("cn", "closet", "basement", "garage", "patio"),
       c("cm", "violin", "guitar", "clarinet", "saxophone"), 
       c("IFH", "intrusion_IFH", 
                  "intrusion_IFH (Food High Calories)",
                  "food", "candy", 
                  "soda", "potato", "cheese",
                  "brownies", "fries", "cookies", "chips" 
                ),
       c("IFL", "intrusion_IFL", 
                  "intrusion_IFL (Food Low Calories)",
                  "cauliflower", "broccoli", 
                  "carrot", "beets", "raspberries","kale", "lettuce",
                  "spinach", "onion", "celery", "cabbage"), 
       c("IN1", "intrusion_IN (Neutral)",
                  "dresser", "drill", "rat", "filing cabinet", "cupboard",
                   "balcony",  "cellar", "attic", "shelves",
                  "car", "chair", "books", "cabin","fridge", "train", 
                  "notebook", "bus", "bouquet", "book", "shelf", "chairs", 
                  "bridge", "bookshelf", "couch", "table",
                  "bookcase", "cabinet", "lamp", "desk", "truck", 
                  "motorcycle", "taxi", "boat"),
       c("IN2", 
         "trumpet","piano"),
       c("rep", "r", "repeat"))


