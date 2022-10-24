c     Cameron Dolly
c     CMS495 - Scientific Computing
      PROGRAM Midnight
      Real dice1, dice2, dice3, dice4, dice5, dice6, savedD1, savedD2
      Real savedD3, savedD4, savedD5, savedD6
      Integer count, playerAScore, playerBScore, bool1, bool2
      Integer input
1     count = 0
      dice1 = 0.0
      dice2 = 0.0
      dice3 = 0.0
      dice4 = 0.0
      dice5 = 0.0
      dice6 = 0.0
      savedD1 = 0.0
      savedD2 = 0.0
      savedD3 = 0.0
      savedD4 = 0.0
      savedD5 = 0.0
      savedD6 = 0.0
      input = 0
      bool1 = 0
      bool2 = 0
      playerAScore = 0
      playerBScore = 0

      call System_Clock(count)
      call srand(count)
    
      Write(*,*)'Welcome to midnight! This game needs two players.'
      Write(*,*)'Type "1" to begin'

      Read *,input
      Write(*,*)input

      if(input == 1)then
        Write(*,*)'playing!'
      END if

      Write(*,*)"It is player A's turn, rolling the dice now."
      dice1 = floor((rand() * 6)) + 1
      dice2 = floor((rand() * 6)) + 1
      dice3 = floor((rand() * 6)) + 1
      dice4 = floor((rand() * 6)) + 1
      dice5 = floor((rand() * 6)) + 1
      dice6 = floor((rand() * 6)) + 1
      Write(*,*)'Player A rolled the following numbers:'
      Write(*,*)'Dice 1: ',dice1
      Write(*,*)'Dice 2: ',dice2
      Write(*,*)'Dice 3: ',dice3
      Write(*,*)'Dice 4: ',dice4
      Write(*,*)'Dice 5: ',dice5
      Write(*,*)'Dice 6: ',dice6

      Write(*,*)'What dice would you like to bank?'
      Write(*,*)'You must bank at least one die.'

11    if(savedD1.ne.0.0.and.savedD2.ne.0.0.and.savedD3.ne.0.0.and.
     &savedD4.ne.0.0.and.savedD5.ne.0.0.and.savedD6.ne.0.0)then
        GO TO 50 
      END if  
      Read*,input
      if(input == 99)then
        GO TO 20
      END if
      
      if(input >= 7 .or. input <= 0)then 
        Write(*,*)input
        Write(*,*)'Please enter a valid die number'
        GO TO 11
      else if(input == 1) then
        Write(*,*)'Saving dice 1, value of ',dice1
        savedD1 = dice1
        Write(*,*)'What other dice would you like to keep?'
        Write(*,*)'If you wish to reroll remaining dice, type "99"'
        GO TO 11
      else if(input == 2) then
        Write(*,*)'Saving dice 2, value of ',dice2
        savedD2 = dice2
        Write(*,*)'What other dice would you like to keep?'
        Write(*,*)'If you wish to reroll remaining dice, type "99"'
        GO TO 11
      else if(input == 3) then
        Write(*,*)'Saving dice 3, value of ',dice3
        savedD3 = dice3
        Write(*,*)'What other dice would you like to keep?'
        Write(*,*)'If you wish to reroll remaining dice, type "99"'
        GO TO 11
      else if(input == 4) then
        Write(*,*)'Saving dice 4, value of ',dice4
        savedD4 = dice4
        Write(*,*)'What other dice would you like to keep?'
        Write(*,*)'If you wish to reroll remaining dice, type "99"'
        GO TO 11
      else if(input == 5) then
        Write(*,*)'Saving dice 5, value of ',dice5
        savedD5 = dice5
        Write(*,*)'What other dice would you like to keep?'
        Write(*,*)'If you wish to reroll remaining dice, type "99"'
        GO TO 11
      else if(input == 6) then
        Write(*,*)'Saving dice 6, value of ',dice6
        savedD6 = dice6
        Write(*,*)'What other dice would you like to keep?'
        Write(*,*)'If you wish to reroll remaining dice, type "99"'
        GO TO 11    
      END if

20    if(input == 99)then
        Write(*,*)"Player A's saved dice so far"
        if(savedD1.ne.0.0)then
          Write(*,*)'Dice 1:', savedD1
        END if
        if(savedD2.ne.0.0)then
          Write(*,*)'Dice 2:', savedD2
        END if
        if(savedD3.ne.0.0)then
          Write(*,*)'Dice 3:', savedD3
        END if
        if(savedD4.ne.0.0)then
          Write(*,*)'Dice 4:', savedD4
        END if
        if(savedD5.ne.0.0)then
          Write(*,*)'Dice 5:', savedD5
        END if
        if(savedD6.ne.0.0)then
          Write(*,*)'Dice 6:', savedD6
        END if

        Write(*,*)'Reroll remaining dice? 1 to continue'
        Read*,input

        if(savedD1 == 0.0)then
          dice1 = floor((rand() * 6)) + 1
          Write(*,*)'Dice 1: ',dice1
        END if
        if(savedD2 == 0.0)then
          dice2 = floor((rand() * 6)) + 1
          Write(*,*)'Dice 2: ',dice2
        END if
        if(savedD3 == 0.0)then
          dice3 = floor((rand() * 6)) + 1
          Write(*,*)'Dice 3: ',dice3
        END if
        if(savedD4 == 0.0)then
          dice4 = floor((rand() * 6)) + 1
          Write(*,*)'Dice 4: ',dice4
        END if
        if(savedD5 == 0.0)then
          dice5 = floor((rand() * 6)) + 1
          Write(*,*)'Dice 5: ',dice5
        END if
        if(savedD6 == 0.0)then
          dice6 = floor((rand() * 6)) + 1
          Write(*,*)'Dice 6: ',dice6
        END if

        Write(*,*)'What dice would you like to bank?'
        Write(*,*)'You must bank at least one die.'

25      if(savedD1.ne.0.0.and.savedD2.ne.0.0.and.savedD3.ne.0.0.and.
     &  savedD4.ne.0.0.and.savedD5.ne.0.0.and.savedD6.ne.0.0)then
          GO TO 50 
        END if
         Read*,input
        if(input == 99)then
          GO TO 20
        END if

        if(input >= 7 .or. input <= 0)then 
          Write(*,*)input
          Write(*,*)'Please enter a valid die number'
          GO TO 25
        else if(input == 1) then
          Write(*,*)'Saving dice 1, value of ',dice1
          savedD1 = dice1
          Write(*,*)'What other dice would you like to keep?'
          Write(*,*)'If you wish to reroll remaining dice, type "99"'
          GO TO 25
        else if(input == 2) then
          Write(*,*)'Saving dice 2, value of ',dice2
          savedD2 = dice2
          Write(*,*)'What other dice would you like to keep?'
          Write(*,*)'If you wish to reroll remaining dice, type "99"'
          GO TO 25
        else if(input == 3) then
          Write(*,*)'Saving dice 3, value of ',dice3
          savedD3 = dice3
          Write(*,*)'What other dice would you like to keep?'
          Write(*,*)'If you wish to reroll remaining dice, type "99"'
          GO TO 25
        else if(input == 4) then
          Write(*,*)'Saving dice 4, value of ',dice4
          savedD4 = dice4
          Write(*,*)'What other dice would you like to keep?'
          Write(*,*)'If you wish to reroll remaining dice, type "99"'
          GO TO 25
        else if(input == 5) then
          Write(*,*)'Saving dice 5, value of ',dice5
          savedD5 = dice5
          Write(*,*)'What other dice would you like to keep?'
          Write(*,*)'If you wish to reroll remaining dice, type "99"'
          GO TO 25
        else if(input == 6) then
          Write(*,*)'Saving dice 6, value of ',dice6
          savedD6 = dice6
          Write(*,*)'What other dice would you like to keep?'
          Write(*,*)'If you wish to reroll remaining dice, type "99"'
          GO TO 25    
        END if
      END if

50    Write(*,*)"Player A's final dice are:"
      Write(*,*)savedD1
      Write(*,*)savedD2
      Write(*,*)savedD3
      Write(*,*)savedD4
      Write(*,*)savedD5
      Write(*,*)savedD6

      if(savedD1 == 1.0)then
       bool1 = 1
      ELSE if(savedD1 == 4.0)then
      bool2 = 1
      END if
      if(savedD2 == 1.0)then
       bool1 = 1
      ELSE if(savedD2 == 4.0)then
      bool2 = 1
      END if
      if(savedD3 == 1.0)then
       bool1 = 1
      ELSE if(savedD3 == 4.0)then
      bool2 = 1
      END if
      if(savedD4 == 1.0)then
       bool1 = 1
      ELSE if(savedD4 == 4.0)then
      bool2 = 1
      END if
      if(savedD5 == 1.0)then
       bool1 = 1
      ELSE if(savedD5 == 4.0)then
      bool2 = 1
      END if
      if(savedD6 == 1.0)then
       bool1 = 1
      ELSE if(savedD6 == 4.0)then
      bool2 = 1
      END if

      if(bool1 == 1.and.bool2 == 1)then
        playerAScore = savedD1+savedD2+savedD3+savedD4+savedD5+savedD6 
        Write(*,*)"Player A's total score is",playerAScore
      ELSE
        Write(*,*)'Player A did not bank a 1 and 4'
        Write(*,*)"Therefore, player A's score is 0"
        playerAScore = 0
      END if

      count = 0
      dice1 = 0.0
      dice2 = 0.0
      dice3 = 0.0
      dice4 = 0.0
      dice5 = 0.0
      dice6 = 0.0
      savedD1 = 0.0
      savedD2 = 0.0
      savedD3 = 0.0
      savedD4 = 0.0
      savedD5 = 0.0
      savedD6 = 0.0
      input = 0
      bool1 = 0
      bool2 = 0

      Write(*,*)'-----------------------------------------------------'
100   Write(*,*)"Time for player B's turn!"
      Write(*,*)"Type 1 to roll Player B's dice"
      Read*,input

      Write(*,*)"It is player B's turn, rolling the dice now."
      dice1 = floor((rand() * 6)) + 1
      dice2 = floor((rand() * 6)) + 1
      dice3 = floor((rand() * 6)) + 1
      dice4 = floor((rand() * 6)) + 1
      dice5 = floor((rand() * 6)) + 1
      dice6 = floor((rand() * 6)) + 1
      Write(*,*)'Player B rolled the following numbers:'
      Write(*,*)'Dice 1: ',dice1
      Write(*,*)'Dice 2: ',dice2
      Write(*,*)'Dice 3: ',dice3
      Write(*,*)'Dice 4: ',dice4
      Write(*,*)'Dice 5: ',dice5
      Write(*,*)'Dice 6: ',dice6

      Write(*,*)'What dice would you like to bank?'
      Write(*,*)'You must bank at least one die.'

111    if(savedD1.ne.0.0.and.savedD2.ne.0.0.and.savedD3.ne.0.0.and.
     &savedD4.ne.0.0.and.savedD5.ne.0.0.and.savedD6.ne.0.0)then
        GO TO 150 
      END if  
      Read*,input
      if(input == 100)then
        GO TO 120
      END if
      
      if(input >= 7 .or. input <= 0)then 
        Write(*,*)input
        Write(*,*)'Please enter a valid die number'
        GO TO 111
      else if(input == 1) then
        Write(*,*)'Saving dice 1, value of ',dice1
        savedD1 = dice1
        Write(*,*)'What other dice would you like to keep?'
        Write(*,*)'If you wish to reroll remaining dice, type "100"'
        GO TO 111
      else if(input == 2) then
        Write(*,*)'Saving dice 2, value of ',dice2
        savedD2 = dice2
        Write(*,*)'What other dice would you like to keep?'
        Write(*,*)'If you wish to reroll remaining dice, type "100"'
        GO TO 111
      else if(input == 3) then
        Write(*,*)'Saving dice 3, value of ',dice3
        savedD3 = dice3
        Write(*,*)'What other dice would you like to keep?'
        Write(*,*)'If you wish to reroll remaining dice, type "100"'
        GO TO 111
      else if(input == 4) then
        Write(*,*)'Saving dice 4, value of ',dice4
        savedD4 = dice4
        Write(*,*)'What other dice would you like to keep?'
        Write(*,*)'If you wish to reroll remaining dice, type "100"'
        GO TO 111
      else if(input == 5) then
        Write(*,*)'Saving dice 5, value of ',dice5
        savedD5 = dice5
        Write(*,*)'What other dice would you like to keep?'
        Write(*,*)'If you wish to reroll remaining dice, type "100"'
        GO TO 111
      else if(input == 6) then
        Write(*,*)'Saving dice 6, value of ',dice6
        savedD6 = dice6
        Write(*,*)'What other dice would you like to keep?'
        Write(*,*)'If you wish to reroll remaining dice, type "100"'
        GO TO 111    
      END if

120    if(input == 100)then
        Write(*,*)"Player B's saved dice so far"
        if(savedD1.ne.0.0)then
          Write(*,*)'Dice 1:', savedD1
        END if
        if(savedD2.ne.0.0)then
          Write(*,*)'Dice 2:', savedD2
        END if
        if(savedD3.ne.0.0)then
          Write(*,*)'Dice 3:', savedD3
        END if
        if(savedD4.ne.0.0)then
          Write(*,*)'Dice 4:', savedD4
        END if
        if(savedD5.ne.0.0)then
          Write(*,*)'Dice 5:', savedD5
        END if
        if(savedD6.ne.0.0)then
          Write(*,*)'Dice 6:', savedD6
        END if

        Write(*,*)'Reroll remaining dice? 1 to continue'
        Read*,input

        if(savedD1 == 0.0)then
          dice1 = floor((rand() * 6)) + 1
          Write(*,*)'Dice 1: ',dice1
        END if
        if(savedD2 == 0.0)then
          dice2 = floor((rand() * 6)) + 1
          Write(*,*)'Dice 2: ',dice2
        END if
        if(savedD3 == 0.0)then
          dice3 = floor((rand() * 6)) + 1
          Write(*,*)'Dice 3: ',dice3
        END if
        if(savedD4 == 0.0)then
          dice4 = floor((rand() * 6)) + 1
          Write(*,*)'Dice 4: ',dice4
        END if
        if(savedD5 == 0.0)then
          dice5 = floor((rand() * 6)) + 1
          Write(*,*)'Dice 5: ',dice5
        END if
        if(savedD6 == 0.0)then
          dice6 = floor((rand() * 6)) + 1
          Write(*,*)'Dice 6: ',dice6
        END if

        Write(*,*)'What dice would you like to bank?'
        Write(*,*)'You must bank at least one die.'

125      if(savedD1.ne.0.0.and.savedD2.ne.0.0.and.savedD3.ne.0.0.and.
     &  savedD4.ne.0.0.and.savedD5.ne.0.0.and.savedD6.ne.0.0)then
          GO TO 150 
        END if
         Read*,input
        if(input == 100)then
          GO TO 120
        END if

        if(input >= 7 .or. input <= 0)then 
          Write(*,*)input
          Write(*,*)'Please enter a valid die number'
          GO TO 125
        else if(input == 1) then
          Write(*,*)'Saving dice 1, value of ',dice1
          savedD1 = dice1
          Write(*,*)'What other dice would you like to keep?'
          Write(*,*)'If you wish to reroll remaining dice, type "100"'
          GO TO 125
        else if(input == 2) then
          Write(*,*)'Saving dice 2, value of ',dice2
          savedD2 = dice2
          Write(*,*)'What other dice would you like to keep?'
          Write(*,*)'If you wish to reroll remaining dice, type "100"'
          GO TO 125
        else if(input == 3) then
          Write(*,*)'Saving dice 3, value of ',dice3
          savedD3 = dice3
          Write(*,*)'What other dice would you like to keep?'
          Write(*,*)'If you wish to reroll remaining dice, type "100"'
          GO TO 125
        else if(input == 4) then
          Write(*,*)'Saving dice 4, value of ',dice4
          savedD4 = dice4
          Write(*,*)'What other dice would you like to keep?'
          Write(*,*)'If you wish to reroll remaining dice, type "100"'
          GO TO 125
        else if(input == 5) then
          Write(*,*)'Saving dice 5, value of ',dice5
          savedD5 = dice5
          Write(*,*)'What other dice would you like to keep?'
          Write(*,*)'If you wish to reroll remaining dice, type "100"'
          GO TO 125
        else if(input == 6) then
          Write(*,*)'Saving dice 6, value of ',dice6
          savedD6 = dice6
          Write(*,*)'What other dice would you like to keep?'
          Write(*,*)'If you wish to reroll remaining dice, type "100"'
          GO TO 125    
        END if
      END if

150    Write(*,*)"Player B's final dice are:"
      Write(*,*)savedD1
      Write(*,*)savedD2
      Write(*,*)savedD3
      Write(*,*)savedD4
      Write(*,*)savedD5
      Write(*,*)savedD6

      if(savedD1 == 1.0)then
       bool1 = 1
      ELSE if(savedD1 == 4.0)then
      bool2 = 1
      END if
      if(savedD2 == 1.0)then
       bool1 = 1
      ELSE if(savedD2 == 4.0)then
      bool2 = 1
      END if
      if(savedD3 == 1.0)then
       bool1 = 1
      ELSE if(savedD3 == 4.0)then
      bool2 = 1
      END if
      if(savedD4 == 1.0)then
       bool1 = 1
      ELSE if(savedD4 == 4.0)then
      bool2 = 1
      END if
      if(savedD5 == 1.0)then
       bool1 = 1
      ELSE if(savedD5 == 4.0)then
      bool2 = 1
      END if
      if(savedD6 == 1.0)then
       bool1 = 1
      ELSE if(savedD6 == 4.0)then
      bool2 = 1
      END if

      if(bool1 == 1.and.bool2 == 1)then
        playerBScore = savedD1+savedD2+savedD3+savedD4+savedD5+savedD6 
        Write(*,*)"Player B's total score is",playerBScore
      ELSE
        Write(*,*)'Player B did not bank a 1 and 4'
        Write(*,*)"Therefore, player B's score is 0"
        playerBScore = 0
      END if

      Write(*,*)'Time to compare final scores!'
      Write(*,*)'Player A, score of:',playerAScore
      Write(*,*)'Player B, score of:',playerBScore
      if(playerAScore == playerBScore)then
        Write(*,*)'You tied! Time to play again.'
        GO TO 1
      ELSE if(playerAScore > playerBScore) then
        Write(*,*)'With a score of',playerAScore,'player A wins!'
      ELSE if(playerBScore > playerAScore) then
        Write(*,*)'With a score of',playerBScore,'player B wins!'
      END if

      STOP
      END 
