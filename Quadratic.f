      PROGRAM Quadratic
      REAl a, b, c, posx, negx, term1, term2, term3
      a = 0
      b = 0
      c = 0
      posx = 0
      negx = 0
      term1 = 0
      term2 = 0
      term3 = 0
      
      Write(*,*)'Enter value of a.'
1     Read *, a

      if(a.le.0) then
        GO TO 99
      END IF

      Write(*,*)'Enter value of b.'
      Read *, b
      Write(*,*)'Enter value of c.'
      Read *, c

      
      term1 = -1 * b
      term2 = (-1 * (4 * a * c)) + (b*b)
      term3 = 2 * a

      if(term2.le.0 ) then 
        write(*,*)'Cannot find the squareroot of a number less than 0'
        GO TO 100
      ELSE
        posx = (((term1) + SQRT(term2)))/(term3)
        negx = (((term1) - SQRT(term2)))/(term3)
        GO TO 101
      END IF
99    Write(*,*) 'The a term cannot be equal to 0'
      Write(*,*) 'Please reenter a valid a term.'
      GO TO 1
100   Write(*,*) 'Cannot compute imaginary numbers'
      GO TO 102

101   if(posx == negx) then
        Write(*,*)'There is only one solution, x = ',posx
      ELSE 
        Write(*,*)'There are two solutions, x = ',posx
        Write(*,*)' and x = ', negx
      END IF
102   STOP
      END 