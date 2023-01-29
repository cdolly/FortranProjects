c     Cameron Dolly
c     CMS495 - Scientific Computing
      PROGRAM CMSVirus
      REAL :: g
      INTEGER :: count, running, n, j, num, IDFirstInfection,searching
      INTEGER :: index1,index2,index3,index4,index5,index6, turns
      INTEGER :: count1, count2, count3, count4, count5, count6
      INTEGER :: currentID, sickFound, i, allSick, allGone, counter
      INTEGER :: counterLeft, counterSick, choice
      INTEGER, DIMENSION(29,6) :: people
c     A 2d matrix consisting of 29 people. People 1-20 are the students being simulated. people 21-29 are the stationary people located in each of the rooms.
c     The four peices of information attached to each person are 1-ID number, 2-If Infected, 3-number of turns spent, 4-rules that must be met, IE being able to leave,
c           must go to another room, must stay, etc.
c     5-Turn became sick, 6-Marker to indicate if under restrictions , IE visiting professor X and V
c     The ID numbers for the stationary people are shown below, where for the students they will simply be 1-20
c     21 = Professor X, 22 = Student A, 23 = Student B, 24 = Student C, 25 = Professor Z ,26 = Student F, 27 = Professor V, 28 = Student H, 29 = Student I

      INTEGER, DIMENSION(6, 23) :: rooms
c     A 2d matrix holding the room information and who is in what room. The 1st field contains the room number, and the 2nd field contain the IDs of those who are in the room.
c     The IDs of stationary people will always fill the first few slots depending on how many there are.
      searching = 0
      IDFirstInfection = 0
      num = 0
      running = 0
      count1 = 0
      count2 = 0
      count3 = 0
      count4 = 0
      count5 = 0
      count6 = 0
      currentID = 0
      sickFound = 0
      allSick = 0
      allGone = 0
      counter = 0
      turns = 0
      counterSick = 0
      counterLeft = 0
      choice = 0
      call System_Clock(count)
      call srand(count)
999   FORMAT(A,4x,A,4xA,4x,A)
9990  FORMAT(I3,6x,I3,6x,I3,6x,I3,6x)    
9999  FORMAT(A,1x,I2.1,1x,A)

c     Assigning each student/person their correct ID number, correct default values for all other fields
      do n = 1, 29
c       ID num
        people(n,1) = n

c       If Infected
        people(n,2) = 0

c       Num of turns
        people(n,3) = 0

c       Rules,0 = no restrictions, 1 = can leave, 2 = must stay until 2 others come in, 3 = Must visit professor X, 4 = Must visit Professor V, 5 = Cannot go to room 2, 6 = left the building
        people(n,4) = 0

c       Turn became sick
        people(n,5) = 0

c       Must visit Professor X or V?, 0 = false, 1 = true
        people(n,6) = 0
      END do

c     Assigning correct room values for rooms 
      do n = 1, 6
c       Filling room student IDs with 0s        
        do j = 1, 23
            rooms(n,j) = 0
        END do
      END do


c     Assigning stationary people their rooms, refer to above IDs or assignment for room information
c      Room 1 
      rooms(1,1) = 21
      index1 = 2

c      Room 2
      rooms(2,1) = 22
      rooms(2,2) = 23
      rooms(2,3) = 24
      index2 = 4
      
c      Room 3
      index3 = 1

c      Room 4
      rooms(4,1) = 25
      rooms(4,2) = 26
      index4 = 3

c      Room 5
      rooms(5,1) = 27
      index5 = 2

c      Room 6
      rooms(6,1) = 28
      rooms(6,2) = 29
      index6 = 3

c     Randomly selecting which of the 20 students will begin as infected
      Write(*,*)'***************************************'
      Write(*,*)'Starting simulation of Fuse Science Center'
      num = floor(rand() * 20 + 1)
      IDFirstInfection = num
      Write(*,9999)'Student', num, 'will begin as infected'
      Write(*,*)'***************************************'
      people(num,2) = 1

      running = 1
      
      do while(running == 1)
      if(turns >= 100)then
      running = 0
      GO TO 60
      END if

c     If everyone is sick, stop running
        Write(*,*)'Starting room assignment for turn', turns + 1
        do n = 1, 20
          if(people(n,2) == 1)then
            counterSick = counterSick + 1
            
          END if
        END do
        if(counterSick == 20)then
          Write(*,*)'Everyone is sick, stopping'
          Write(*,*)'A student went', turns,' turns without infection'
          allSick = 1
          running = 0
          counterSick = 0
          GO TO 55
        END if
        counterSick = 0

c       If everyone has left, stop running
        do n = 1,20
          if(people(n,4) == 6)then
            counterLeft = counterLeft + 1

          END if
        END do
        if(counterLeft == 20)then
          Write(*,*)'Everyone has left, stopping'
          allGone = 1
          running = 0
          counterLeft = 0
          GO TO 55
        END if
        counterLeft = 0

c       Deciding which room for each person and increment total turns
        do n = 1, 20

c       Prior checks for restrictions on a person, this is all skipped on the first iteration because no one has been assigned a room yet, and thus, no restrictions

c         if the person exited the building, then skip them
          if(people(n,4) == 6)then
            GO TO 10

c         if the person must stay in room 2, then keep them there for another turn
          else if(people(n,4)== 2)then
            num = 2
            GO TO 1

c         if the person must visit Professor X, send them there  
          else if(people(n,4) == 3)then
            Write(*,*)'Student Visiting professor X'
            num = 8
            GO TO 1

c         if the person must visit Professor V, send them there  
          else if(people(n,4) == 4)then
            Write(*,*)'Student Visiting professor V'
            num = 8
            GO TO 1

c         if the person cannot visit room 2, generate room numbers until one that is not 2 is generated, and then send them to that room.
          else if(people(n,4)== 5)then
             Write(*,*)'Student cannot visit room 2, sending elsewhere'
3            num = floor(rand()* 7 + 1)
            if(num == 2 )then
              GO TO 3
            else
              GO TO 1
            END if

          END if
c         Generate a number designating room, 7 = leave the building

          num = floor(rand() * 7 + 1)

c         If rooom 1, mark the student with the restriction flag and mark them to need to visit the other professor.
1         if(num == 1)then
            rooms(1,index1) = people(n,1)
            people(n,3) = people(n,3) + 1
            people(n,4) = 4
            people(n,6) = 1
            index1 = index1 + 1

c         If rooom 2, assign them normally. Restrictions are handled below during interactions.
          else if(num == 2)then
            rooms(2,index2) = people(n,1)
            people(n,3) = people(n,3) + 1
            index2 = index2 + 1

c         If rooom 3, assign them normally.
          else if(num == 3)then
            rooms(3,index3) = people(n,1)
            people(n,3) = people(n,3) + 1
            index3 = index3 + 1

c         If rooom 4, mark them with the restriction that they cannot visit room 2.
          else if(num == 4)then
            rooms(4,index4) = people(n,1)
            people(n,3) = people(n,3) + 1
            people(n,4) = 5
            index4 = index4 + 1

c         If rooom 5, mark the student with the restriction flag and mark them to need to visit the other professor.
          else if(num == 5)then
            rooms(5,index5) = people(n,1)
            people(n,3) = people(n,3) + 1
            people(n,4) = 3
            people(n,6) = 1
            index5 = index5 + 1

c         If rooom 6, then mark that student as leaving the building and increment their turns.
          else if(num == 6)then
            rooms(6,index6) = people(n,1)
            people(n,3) = people(n,3) + 1
            Write(*,*)'Student left via room 6'
            people(n,4) = 6
            index6 = index6 + 1

          else if(num == 7)then
            if(people(n,3) >= 4 .or. people(n,4)== 1)then
              people(n,4) = 6
              people(n,3) = people(n,3) + 1
              Write(*,*)'Student left via random choice'

            else
                num = floor(rand() * 6 + 1)
                GO TO 1 

            END if

c         The value 8 is used specifically for determining interactions with students who enter Professor X's or Professor V's rooms
c         Specific integer numbers associated with each student ID determine which room the student must go to,
c           For example, a student who must go to see Professor V will have the marker number 4, and Professor X the marker number 3
          else if(num == 8) then 
             if(people(n,4) == 3)then
              rooms(1,index1) = people(n,1)
              people(n,3) = people(n,3) + 1
              people(n,4) = 0
              people(n,6) = 0
              index1 = index1 + 1

            else if(people(n,4) == 4)then
              rooms(5,index5) = people(n,1)
              people(n,3) = people(n,3) + 1
              people(n,4) = 0
              people(n,6) = 0
              index5 = index5 + 1
            END if 

          END if
        GO TO 10
9       people(n,3) = people(n,3) + 1    
10      num = 999
        END do

c       Resetting index values for each room
        index1 = 2
        index2 = 4
        index3 = 1
        index4 = 3
        index5 = 2
        index6 = 3

c       Interpreting interactions for that turn

c       Looping through every room
        do n = 1, 6

c         Looping through every person per room
          do j = 1, 23
            currentID = rooms(n,j)

c           If a sick person is found, set sickFound = 1
            if(people(currentID,2) == 1)then
              sickFound = 1
            END if             

          END do

          if(sickFound == 1)then
c           If a sick person is found, loop through every person in that room and run a chance of them becoming sick
            do j = 1, 23
              currentID = rooms(n,j)
              num = floor(rand() * 2 + 1)
              if(num == 2)then
                people(currentID,2) = 1
                if(people(currentID,5) == 0)then
                  people(currentID,5) = people(currentID,3)
                END if
              END if

            END do
            
          END if
          sickFound = 0
        END do        



55    turns = turns + 1
      Write(*,*)'Turn', turns, 'completed'
      Write(*,*)'----------------------------------'
60    count = count + 1
      END do
      

c     Calculating and printing statistics
      Write(*,*)'***************************************'

      Write(*,*)'The simulation lasted', turns,'turns'

      Write(*,*)'***************************************'

      if(allGone == 1)then
        Write(*,*)'All simulated students left the building'
      END if

      if(allSick == 1)then
        Write(*,*)'All simulated students became sick'
      END if

      if(allGone == 0 .and. allSick == 0)then
        Write(*,*)'Simulation turn-limit reached'
      END if
      Write(*,*)''
      Write(*,*)'Please enter one of the following:'
      Write(*,*)'1 - Debugging output and in-depth stats'
      Write(*,*)'2 - exit'
      Read *, choice
      
      if(choice == 1)then
        Write(*,*)'***************************************************'

      Write(*,999)'ID','Sick?','turns spent','turn became sick'
      do n = 1, 29
        Write(*,9990)people(n,1),people(n,2),people(n,3),people(n,5)
      END do
      Write(*,*)'Note: ID numbers 21-29 represent the occupants'
      Write(*,*)'*****************************************************'

      END if
c     From running this simulation, we have seen that in order for all students to become sick, the CMS Virus has to spread very rapidly early on.
c       Otherwise, at least one student is very likely to leave via room 6 before they can become infected.
c     Typically, it would take about 15-25 turns for all students to leave the building, regardless of infection status.
      

      STOP
      END 
