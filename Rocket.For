c     Cameron Dolly
c     CMS495 - Scientific Computing      
      PROGRAM Rocket
      Real y0, v0, a, vf, yf, time, pHeight
      
      y0 = 0.0
      v0 = 0.0
      a = -9.8
      time = 0.0

1     Write(*,*) 'Input starting velocity? Must be less than 1500'
      Read *,v0
      if(v0 >= 1501) then
        Write(*,*)'Invalid input, please enter again'
        GO TO 1
      END if

      
        do while(yf >= 0)
          if(yf >= 2000.) then
            yf = y0 + (v0 * time) + ((1./2.) * a * (time * time))
            vf = v0 + (a * time)
            pHeight = yf
            if(pHeight < 0)then
              GO TO 99
            END if
            Write(*,*) 'The current height is ', yf
            Write(*,*) 'The current velocity is ', vf
            time = time + 2.
            Write(*,*)'Time is currently ', time
          END if

          if(yf <= 1999.) then

            yf = y0 + (v0 * time) + ((1./2.) * a * (time * time))
            vf = v0 + (a * time)
            pHeight = yf
            if(pHeight < 0)then
              GO TO 99
            END if
            Write(*,*) 'The current height is ', yf
            Write(*,*) 'The current velocity is ', vf
            time = time + .05
            Write(*,*)'Time is currently ', time
          END if
        END do
      
99    Write(*,*)'The rocket was in the air for roughly'
      Write(*,*)time
      Write(*,*)'seconds'

      STOP
      END 
