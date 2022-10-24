      PROGRAM Zork
      Integer choice
      choice = 0
1     Write(*,*)'You open your eyes to light shining into your room.'
      Write(*,*)'It is an early Tuesday morning.'

      Write(*,*)'Do you go be productive or play some games?'
      Write (*,*)'1 for be productive, 2 to play games'
      Read *,choice
      if(choice == 1) then  
        Write(*,*)'You decide to go to the gym.'
        GO TO 2
      ELSE
        Write(*,*)'You decide to play some video games'
        Write(*,*)'You turn on your computer and start up Valorant'
        GO TO 3
      END if

2     Write(*,*)'After an epic pump sesh, you realize you have class'
      Write(*,*)'But class starts in 20 minutes, so do you'
      Write(*,*)'1, Go back and shower or 2, go eat instead?'
      Read *,choice
      if(choice == 1) then
        Write(*,*)'You decide to go and clean up before class'
        GO TO 10
      ELSE
        Write(*,*)'You decide to go eat, and go to class sweaty.'
        GO TO 10
      END if

3     Write(*,*)'After playing one match of Valorant, you remember'
      Write(*,*)' that it is a trash game, so instead you go to class'
      GO TO 10

10    Write(*,*)'After getting to CMS495, you walk in and sit'
      Write(*,*)'Being so tired from earlier, you dont remember what'
      Write(*,*)'  exactly you saw. Did you see Dr.Fuse:'
      Write(*,*)' 1, Suplex Gus from the 3rd story of bush or'
      Write(*,*)' 2, see Dr.Fuse have a civil conversation with Gus.'
      Read *,choice
      if(choice == 1) then
        Write(*,*)'You watch Dr.Fuse suplex Gus from 3 stories up'
        GO TO 12
      ELSE
        Write(*,*)'A civil discussion occurs with Gus and Dr.Fuse'
        GO TO 13
      END if

12    Write(*,*)'In a fit of rage, Dr.Fuse takes 30 markers and '
      Write(*,*)'starts to throw them at every student'
      Write(*,*)'Do you call the police or fight back?'
      Write(*,*)'1, call the police or 2, fight back'
      Read *,choice
      if(choice == 1) then
        Write(*,*)'You dial the police'
        GO TO 14
      ELSE
        Write(*,*)'You take out the iron broadsword you brought for'
        Write(*,*)' show and tell in CMS495 that day and'
        Write(*,*)' begin your duel with Dr.Fuse'
        GO TO 20
      END if

13    Write(*,*)'After a few minutes of enduring Gus and his witty'
      Write(*,*)' comebacks, Dr.Fuse becomes enraged and threatens'
      Write(*,*)' to make Gus run up and down the stairs'
      Write(*,*)'A passerby witnesses this child abuse and summons'
      Write(*,*)' Campus Safety, who give Dr.Fuse a parking ticket.'
      Write(*,*)'All is well.'
      GO TO 99

14    Write(*,*)'After dialing the police, you soon learn Dr.Fuse is '
      Write(*,*)' a wanted criminal. The operator tells you to hide.'
      Write(*,*)'You begin to run away, but after forcing Jari to'
      Write(*,*)' build a model of a rocket to chase you, Dr.Fuse'
      Write(*,*)' catches up to you as you enter the dining hall'
      Write(*,*)'Do you 1, run down stairs or 2, use a swipe to get'
      Write(*,*)' into the CC?'
      Read *,choice
      if(choice == 1) then
        Write(*,*)'You run downstairs into Daves searching for help'
        Write(*,*)'In Daves, you find Dave himself awaiting you'
        Write(*,*)'Seeing Dave, Dr.Fuse quickly turns and runs away'
        Write(*,*)'Dave then gives you an Angry Bird and vanishes'
        GO TO 99
      ELSE
        Write(*,*)'You use a swipe to enter the CC'
        Write(*,*)'You thank the lady at the door and quickly enter'
        Write(*,*)'Dr.Fuse tries to enter, but he forgets that he is'
        Write(*,*)' out of swipes for the week, and is refused entry'
        Write(*,*)'After trying to force his way in, the nice lady'
        Write(*,*)' takes out an executioners axe and cuts him down.'
        Write(*,*)'You are saved by the CC workers.'
        GO TO 98
      END if

20    Write(*,*)'Wielding your broadsword, Dr.Fuse realizes markers'
      Write(*,*)' will be insufficient for this battle, and so he runs'
      Write(*,*)' To Dr.Murdaughs class where she is hiding her own'
      Write(*,*)' light sabers, and steals one to fight you with.'
      Write(*,*)' Do you 1, continue to duel, or 2, try to run?'
      Read *,choice
      if(choice == 1) then
        Write(*,*)'You decide to continue to duel.'
        Write(*,*)'You both take your stances, tension is rising.'
        Write(*,*)'After one deep breath, you charge Dr.Fuse'
        Write(*,*)'As you begin to swing your sword, Dr.Fuse is pushed'
        Write(*,*)' into the wall by Gus, who discovered how to be'
        Write(*,*)' an airbender from his research that summer.'
        Write(*,*)'Overpowered, Dr.Fuse surrenders.'
        GO TO 99
      ELSE
        Write(*,*)'You decide it is best to try and run away.'
        Write(*,*)'However, with his many computational models,'
        Write(*,*)' Dr.Fuse can easily predict your movements'
        Write(*,*)'He uses the force to start suffocating you'
        Write(*,*)'But as it seems like everything is going dark,'
        Write(*,*)'Dr.Murdaugh appears and begins to shoot lightning'
        Write(*,*)' out of her hands like Palpatine. You are saved.'
        Write(*,*)'She reminds you to always rent the physics'
        Write(*,*)' equipment so that it is fair for everyone.'
        GO TO 98
      END if


98    Write(*,*)' '
      Write(*,*)'Congratulations! You have reached a violent'
      Write(*,*)' ending to this Zork adventure. Try replaying to'
      Write(*,*)' make your way to the peaceful ending!'
      GO TO 100

99    Write(*,*)' '
      Write(*,*)'Congratulations! You have reached a peaceful'
      Write(*,*)' ending to this Zork adventure. Try replaying to'
      Write(*,*)' make your way to the violent ending!'

100   Write(*,*)' '
      STOP
      END 
