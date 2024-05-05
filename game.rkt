#lang racket

;;game state data 
(struct game-state (location inventory has-horse goblins))

;; Initial game state
(define initial-state (game-state "start" '() #f #f))



;;Menu
(define (menu-scene state)
  (displayln "Available choices: go to forest, go to cave, go to village, go to castle, manage inventory, describe location, help")
  (let ([choice (read-line)])
    (cond
      [(string=? choice "go to forest") (forest-scene state)]
      [(string=? choice "go to cave") (cave-scene state)]
      [(string=? choice "go to village") (village-scene state)]
      [(string=? choice "go to castle") (castle-scene state)]
      [(string=? choice "manage inventory") (inventory-scene state)]
      [(string=? choice "describe location") (location state)(menu-scene state)]
      [(string=? choice "help")(location state)(inventory state)(menu-scene state)]
      [else (displayln "Invalid choice. Try again.") (menu-scene state)])))




;;Forest
(define (forest-scene state)
  (displayln "You are in a dense forest.")
  (unless (member 'sword (game-state-inventory state))
    (displayln "There is a gleaming sword lying on the ground.")
    (displayln "Do you want to pick up the sword? (yes/no)")
    (let ([choice (read-line)])
      (if (string=? choice "yes")
          (forest-scene2 (game-state "forest" (cons 'sword (game-state-inventory state)) (game-state-has-horse state) (game-state-goblins state)))
          (forest-scene2 state ))))
  (forest-scene2 state))

(define (forest-scene2 state)
  (unless (member 'Lantern (game-state-inventory state))
    (displayln "Deeper into the forest you see a dim light")
    (displayln "There is a Lantern lying on the ground.")
    (displayln "Do you want to pick up the Lantern? (yes/no)")
    (let ([choice (read-line)])
      (if (string=? choice "yes")
          (forest-scene3 (game-state (game-state-location state) (cons 'Lantern (game-state-inventory state)) (game-state-has-horse state) (game-state-goblins state)))
          (forest-scene3 state))))
  (forest-scene3 state))

 (define (forest-scene3 state)
  (if (not (game-state-goblins state))
      (begin
        (displayln "Deep into the forest you meet Goblins")
        (if (member 'sword (game-state-inventory state))
            (begin
              (displayln "With your sword you defeat the Goblins")
              (displayln "The Goblins dropped an axe. You picked it up.")
              (menu-scene (game-state (game-state-location state)(cons 'Axe (game-state-inventory state))(game-state-has-horse state) #t)))
            (begin
              (displayln "Without a sword you are forced to retreat")
              (menu-scene state))))
      (menu-scene state)))



;;Cave
(define (cave-scene state)
  (displayln "You are in a dark cave.")
  (unless (game-state-has-horse state)
    (displayln "There is a horse in the corner.")
    (displayln "Do you want to take the horse? (yes/no)")
    (let ([choice (read-line)])
      (if (string=? choice "yes")
          (cave-scene2 (game-state "cave" (game-state-inventory state) #t (game-state-goblins state)))
          (cave-scene2 state))))
  (cave-scene2 state))

(define (cave-scene2 state)
  (displayln "Do you want to go deeper in the cave? (yes/no)")
  (let ([choice (read-line)])
      (if (string=? choice "yes")
          (begin
            (displayln "Deeper into to the cave it is hard to see")
            (cave-scene3 state))
          (begin
            (displayln "You backed out of the cave")
            (displayln "Are you afraid of adventure?")
            (menu-scene state)))))

(define (cave-scene3 state)
  (displayln "There is a gold coin on the floor")
  (displayln "Do you want to pick it up (yes/no)")
  (let ([choice (read-line)])
    (if (string=? choice "yes")
        (cave-scene4 (game-state (game-state-location state) (cons 'Gold (game-state-inventory state)) (game-state-has-horse state) (game-state-goblins state)))
        (cave-scene4 state))))
  
          
(define (cave-scene4 state)         
  (if (member 'Lantern (game-state-inventory state))
      (begin
        (displayln "With your lantern you are able to navigate the dark cave")
        (displayln "You see an emerald on the floor.")
        (displayln "Do you want to pick it up?")
        (let ([choice (read-line)])
          (if (string=? choice "yes")
              (begin
                (displayln "You got too greedy")
                (displayln "The Emerald was booby trapped.")
                (displayln "You died")
                (gameover-scene))
              (menu-scene state))))
      (begin
        (displayln "Without a lantern you were unable to see.")
        (displayln "You fell into a crack and died")
        (gameover-scene))))
        
          
  
;;Village
(define (village-scene state)
  (displayln "You are in a small village. There is a shop and a tavern.")
  (displayln "Do you want to enter the shop or tavern?")
  (let ([choice (read-line)])
    (cond
      [(string=? choice "shop") (shop-scene state)]
      [(string=? choice "tavern") (tavern-scene state)]
      [else (displayln "Invalid choice. Please enter 'shop' or 'tavern'")(village-scene state)])))
  

(define (shop-scene state)
  (displayln "You enter the shop. There are various items for sale, but you don't have any money.")
  (displayln "You exit the shop and enter the tavern.")
  (tavern-scene state))
  
  

(define (tavern-scene state)
  (displayln "You enter the lively tavern. Villagers are drinking and chatting.")
  (displayln "A fellow traveler asks if you want to go on a quest with him.(yes/no)")
  (let ([choice (read-line)])
      (if (string=? choice "yes")
          (sidequest state)
          (menu-scene (game-state "tavern" (game-state-inventory state) (game-state-has-horse state) (game-state-goblins state))))))
          
  
(define (sidequest state)
  (displayln "")
  (menu-scene state))

;;Castle
(define (castle-scene state)
  (if (and (member 'sword (game-state-inventory state)) (game-state-has-horse state))
      (begin
        (displayln "You approach the looming castle gates. A fearsome dragon guards the entrance.")
        (displayln "You brandish your sword and mount the horse. With your trusty steed and weapon, you slay the dragon!")
        (displayln "Congratulations YOU WON")
        (gameover-scene))
      (begin
        (displayln "You approach the looming castle gates, but without a sword and a horse, you are unable to defeat the dragon.")
        (displayln "You Died Game Over")
        (gameover-scene))))




;;Describe inventory
(define (inventory state)
  (displayln "Your inventory:")
  (if (empty? (game-state-inventory state))
      (displayln "Your inventory is empty.")
      (for ([item (game-state-inventory state)])
        (displayln item))))

;;Manage inventory
(define (inventory-scene state)
  (inventory state)
  (displayln "Available choices: drop item, back")
  (let ([choice (read-line)])
    (cond
      [(string=? choice "drop item")
       (displayln "Enter the name of the item you want to drop:")
       (let ([item-to-drop (string->symbol (read-line))])
         (if (member item-to-drop (game-state-inventory state))
             (let ([new-inventory (remove item-to-drop (game-state-inventory state))])
               (displayln (format "You dropped the ~a." item-to-drop))
               (inventory-scene (game-state (game-state-location state) new-inventory (game-state-has-horse state)(game-state-goblins state))))
             (begin
               (displayln "You don't have that item in your inventory.")
               (inventory-scene state))))]
      [(string=? choice "back")(menu-scene state)]
      [else (displayln "Invalid choice. Try again.") (inventory-scene state)])))


;;Describe Location
(define (location state)
  (cond
    [(string=? (game-state-location state) "start")
                         (displayln "In front of you is a gigantic forest there is a unicorn prancing in the distance")
                         (displayln "Behind you is a large castle with colossal fortifications")
                         (displayln "The castle is empty but there is a mighty dragon soaring above")
                         (displayln "There is a dark daunting cave to your right")
                         (displayln "On your left is a small dainty village")]
   
    [(string=? (game-state-location state) "forest")
                         (displayln "You are leaving a magical forest with tall gnarly trees and buzzing with wild life.")
                         (displayln "You see a dark cave on your left and a quaint village on your right.")
                         (displayln "Up ahead you see a grand castle with towering walls guarded by a dragon hovering overhead.") ]
    
    [(string=? (game-state-location state) "cave")
                         (displayln "Exiting the dark cave you are met with golden rays of light.")
                         (displayln "You see a forest on your right and a majestic castle on your left.")
                         (displayln "Up ahead you see a whimsical village.")]
    
    [(string=? (game-state-location state) "tavern")
     (displayln "As you exit the village there are three paths before you")
     (displayln "Will you go to the castle and embark on your journey to fight the Dragon?")
     (displayln "Will you go to the cave and see if there are any hidden treasures")
     (displayln "Will you go to the forest and find magical creatures")]))



;;GAME OVER
(define (gameover-scene)
  (displayln "Would you like to play again or end the game? (play/end)")
  (let ([choice (read-line)])
    (cond
      [(string=? choice "play") (start)]
      [(string=? choice "end") (displayln "Thank you for playing! Goodbye.")]
      [else (displayln "Invalid choice. Please enter 'play' to start a new game or 'end' to exit.")(gameover-scene)])))

;; Start the game
(define (start)
  (displayln "Welcome to the mythical country of Glendale")
  (displayln "Treasure awaits in the castle but be prepared to face the dragon")
  (displayln "You are a wandering traveler and have reached an intersection in your journey.")
  (menu-scene initial-state))



(start)