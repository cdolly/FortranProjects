c     Cameron Dolly
c     CMS495 - Scientific Computing
      PROGRAM Zeroes
      Real :: term1, term2, term3, term4, coef1, coef2, coef3
      Real :: coef4, coef5, tolerance, fa, fb, x, a, b, c, fc
      Real :: prevFC
      Integer :: searching, count, foundRoots, maxIterations

      term1 = 0.0
      term2 = 0.0
      term3 = 0.0
      term4 = 0.0
      coef1 = 0.0
      coef2 = 0.0
      coef3 = 0.0
      coef4 = 0.0
      coef5 = 0.0
      tolerance  = 0.0
      fa = 0
      fb = 0
      fc = 0
      x = -5
      a = 0
      b = 0
      c = 0
      searching = 0
      fc = 0
      pos = 1
      prevFC = 0
      foundRoots = 0
      maxIterations = 0



      Write(*,*)'Please input the degree of the first term'
1     Read *, term1



      if(term1 >= 5 .or. term1 < 0)then
        Write(*,*)'Please enter a term less than 4, or greater than -1'
        GO TO 1
      END if

      Write(*,*)'Please input the coefficient of the first term'
      Read *, coef1

      Write(*,*)'Please input the degree of the second term'
2     Read *, term2

       if(term2 > 3 .or. term2 < 0)then
         Write(*,*)'Please input a term less than that of the first'
         GO TO 2
       END if
      
      Write(*,*)'Please input the coefficient of the second term'
      Read *, coef2

      Write(*,*)'Please input the degree of the third term'
3     Read *, term3

      if(term3 > 2 .or. term3 < 0)then
        Write(*,*)'Please input a term less than that of the second'
        GO TO 3
      END if

      Write(*,*)'Please input the coefficient of the third term'
      Read *, coef3

      Write(*,*)'Please input the degree of the fourth term'
4     Read *, term4

      if(term4 > 1 .or. term4 < 0)then
        Write(*,*)'Please input a term less than that of the second'
        GO TO 4
      END if

      Write(*,*)'Please input the coefficient of the fourth term'
      Read *, coef4

      Write(*,*)'Please input the constant'
      Read *, coef5
      
c      Write(*,*)term1,coef1,term2,coef2,term3,coef3,term4,coef4,coef5
      Write(*,*)'Please input the tolerance of the root finder'
      Read *, tolerance

      Write(*,*)'The polynomial input is:'
      Write(*,*)'(',coef1,'^',term1,') + (', coef2,'^',term2,') + (', 
     & coef3,'^', term3,') + (',coef4,'^',term4,') +', coef5

      Write(*,*)'The input tolerance value is', tolerance
      Write(*,*)'Searching for roots between x = -5 ad x = 5'

      do while (x <= 5.0)
        a = x

        b = a + 0.01

c      Calculating f(a)        
        fa = (coef1*(a ** term1)) + (coef2*(a ** term2)) + 
     &   (coef3*(a ** term3))  + (coef4*(a ** term4)) + (coef5)
c      Calculating f(b)    
        fb = (coef1*(b ** term1)) + (coef2*(b ** term2)) + 
     &   (coef3*(b ** term3))  + (coef4*(b ** term4)) + (coef5)
c      Calculating c
        c = ((a * fb) - (b * fa))/ (fb - fa)
c      Calculating f(c)
        fc = (coef1*(c ** term1)) + (coef2*(c ** term2)) + 
     &   (coef3*(c ** term3))  + (coef4*(c ** term4)) + (coef5)

        searching =  1
 
        do while(searching == 1)
        if(maxIterations > 1000) then
          searching = 0
        END if

          if(fc == 0) then
          Write(*,*)fc
            Write(*,*)'Root found at x = ',c 
            foundRoots = foundRoots + 1
            seaching = 0
          else if((ABS(fc - prevFC)) < tolerance) then
            Write(*,*)'Root found at x = ',c 
            foundRoots = foundRoots + 1
            seaching = 0
          else if((fc * fa) < 0) then
            prevFC = fc
            b = c 
          else
            prevFC = fc
            a = c
          END if

c      Calculating f(a)        
        fa = (coef1*(a ** term1)) + (coef2*(a ** term2)) + 
     &   (coef3*(a ** term3))  + (coef4*(a ** term4)) + (coef5)
c      Calculating f(b)    
        fb = (coef1*(b ** term1)) + (coef2*(b ** term2)) + 
     &   (coef3*(b ** term3))  + (coef4*(b ** term4)) + (coef5)
c      Calculating c
        c = ((a * fb) - (b * fa))/ (fb - fa)
c      Calculating f(c)
        fc = (coef1*(c ** term1)) + (coef2*(c ** term2)) + 
     &   (coef3*(c ** term3))  + (coef4*(c ** term4)) + (coef5)
        maxIterations = maxIterations + 1
        END do

        x = x + 0.25
      END do
      if(foundRoots == 0)then
        Write(*,*)'There were no roots found for the polynomial between
     & x = -5 and x = 5'  
      else
        Write(*,*)'There were', foundRoots, 'roots found'
      END if
      STOP
      END 
