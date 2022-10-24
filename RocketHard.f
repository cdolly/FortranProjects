c     Cameron Dolly
c     CMS495 - Scientific Computing
      PROGRAM RocketHard
      Integer n, thrustTime
      Real y0, v0, a, vf, yf, launchTime, thrust, rThrust
      Real xHor, vWind, yMax, pHeight
      
      y0 = 0.0
      v0 = 0.0
      a = -9.8
      launchTime = 0.0
      thrust = 35.0
      thrustTime = 0.0
      rThrust = 0.0
      xHor = 0.0
      vWind = 5.0
      yMax = 0.0
      pHeight = 0.0
      vf = 0.0
      yf = 0.0
      
1     Write(*,*) 'Input starting velocity. Must be less than 1500'
      Read *,v0
      if(v0 >= 1501) then
        Write(*,*)'Invalid input, please enter again'
        GO TO 1
      END if
2     Write(*,*) 'Input Thrust time. Must be less than 15'
      Read *,thrustTime
      if(thrustTime > 15) then
        Write(*,*)'Invalid input, please enter again'
        GO TO 2
      END if
c     Thrust calculation
      y0 = (thrustTime * (thrust - 9.8))
      v0 = (v0) + (thrustTime * (thrust - 9.8))

c     Processing the rocket's movements
      do while(yf >= 0)
            if(yf >= 10000) then
                  pHeight = yMax
                  yf = y0 + (v0 * launchTime) + ((1./2.) * a *
     &                  (launchTime * launchTime))
                  vf = v0 + (a * launchTime)
                  if(yf < 0)then
                        yMax = pHeight
                        GO TO 99
                  END if
                  if(yf > pHeight) then
                        yMax = yf
                  END if
                  launchTime = launchTime + .5
            END if

            if(yf < 10000) then
                  pHeight = yMax
                  yf = y0 + (v0 * launchTime) + ((1./2.) * a *
     &                  (launchTime * launchTime))
                  vf = v0 + (a * launchTime)
                  if(yf < 0)then
                        yMax = pHeight
                        GO TO 99
                  END if
                  if(yf > pHeight) then
                        yMax = yf
                  END if
                  launchTime = launchTime + .05
            END if
      END do
      
99    Write(*,*)'The total flight time was ', launchTime +
     &thrustTime,' seconds'
      Write(*,*)'The maximum height acheived was ', yMax,' meters'
      xHor = vWind * (launchTime + thrustTime)
      Write(*,*)'The rocket was ',xHor,' meters away from launch'

      STOP
      END 
