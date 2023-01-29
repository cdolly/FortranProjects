c     Cameron Dolly
c     CMS495 - Scientific Computing
      PROGRAM ExoplanetModel
      Real :: pi, exoPeriod, exoAu, noise, ERadius, F
      Real :: L, exoRadius, starRadius, d, FractionLightBLocked
      Real :: blockedF, thetaPlanet, time
      Integer :: count, orbits, orbitsEst, iterations, num
      Integer :: input, req

      pi = 4.D0 * DATAN(1.D0)
      exoAu = 1
      exoPeriod = sqrt(exoAu * exoAu * exoAu)
      exoTheta = 0
      noise = 0
c     Luminosity of k2-18
      L = 0.0234
c     distance of k2-18
      d = 1070
c     Radius in meters of k2-18b
      exoRadius = 14535689.9
c     Radius in meters of k2-18
      starRadius = 751464000
      time = 0
      blockedF = 0
      orbits = 0
      orbitsEst = 0
      iterations = 0
      num = 0
      input = 0
      req = 3
      

      Write(*,*)'Program will start with minimum 3 orbits'
      Write(*,*)'  And radius of exoplanet k2-18b'
      Write(*,*)''
      Write(*,*)'If you would like to specify a number of orbits'
      Write(*,*)'or exoplanet radius, enter 1'
      
2     Read *,input
      if(input == 1)then
        Write(*,*)'How many orbits would you like to specify for each
     & planet?'
3       Read*,req
        if(req <= 2)then
          Write(*,*)'Please enter an integer number larger than 2'
          GO TO 3
        END if

        Write(*,*)'What radius would you like to start at, in meters?'
4       Read*,input
        if(input <= 10000)then
          Write(*,*)'Please input a size greater than 10000 meters'
          GO TO 4

        else 
          exoRadius = input  
        END if  

      END if

      call System_Clock(count)
      call srand(count)

      Write(*,*)'Starting radius:',exoRadius
      Write(*,*)'Minimum orbits:',req
      


      open(1, file="output.txt", status = 'new')
      Write(1,*)'Starting radius:',exoRadius
      Write(1,*)'Minimum orbits:',req

      Write(1,*)'Stellar Luminosity         years'

c     The proportion for how much the brightmess should decrease by when a planet of radiusPlanet passes in front of it
5     FractionLightBLocked = (exoRadius/starRadius) * 
     &(exoRadius/starRadius)

c     The apparent brightness of the star 1070 lightyears away
      F = L / (4 * pi * (d * d))
c     What the luminosity should be when blocked by the exoplanet
      blockedF = F * FractionLightBLocked

      do while(time <= 500)
        if(orbits >= req)then
          orbits = 0
          GO TO 10
        else if(blockedF == F)then
          Write(1,*)'Planet no longer detectable'
          Write(1,*)'Radius of planet is',exoRadius
          Write(1,*)'There were',iterations,'iterations'
          GO TO 100
        END if

c       Simulating random noise occurring to brightness
        noise = rand() * 1E-16
        num = floor(rand() * 10)
        if(num > 5)then
          blockedF = blockedF + noise
          F = F + noise
        else
          blockedF = blockedF - noise
          F = F - noise
        END if

        if(blockedF <= .5E-17)then
          GO TO 15
        END if

        exoTheta = ((1/exoPeriod) * 360) * time
        orbitsEst = exoTheta/360
        if(orbitsEst == orbits + 1) then
          if(blockedF < 0)then
            blockedF = abs(blockedF)
            Write(1,*)blockedF, time
          else
            Write(1,*)blockedF, time
          END if
        else
          Write(1,*)F, time

        END if

        if(exoTheta/360 >= orbits + 1)then
          orbits = orbits + 1
        END if
        time = time + .1
      END do

10    Write(1,*)req,'orbits completed'
      Write(1,*)'Rerunning with Radius at 80% of original'
      Write(1,*)''
      iterations = iterations + 1
      exoRadius = exoRadius * .80
      if(exoRadius < 1)then
        GO TO 15
      END if
      time = 0
      GO TO 5

15    Write(1,*)'No difference being observed'
      Write(1,*)'Radius of planet is',exoRadius
      Write(1,*)'There were',iterations,'iterations'

100   close(1)
      Write(*,*)'File written to "output.txt"'
      STOP
      END 
