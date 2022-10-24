c     Cameron Dolly
c     CMS495 - Scientific Computing
      PROGRAM Planets
      Real :: Etheta, time, EarthAU, JupiterAU, NeptuneAU
      Real :: thetaRadians, Eperiod, Jperiod, Nperiod, Jtheta
      Real :: Ntheta, pi, Emass, Jmass, Nmass
      Integer:: io

      Etheta = 0.0
      Jtheta = 0.0
      Ntheta = 0.0
      time = 0.0
      EarthAU = 1.00
      JupiterAU = 5.20
      NeptuneAU = 19.23
      thetaRadians = 0.0
      Eperiod = 0.0
      Jperiod = 0.0
      Nperiod = 0.0
      pi = 4.D0 * DATAN(1.D0)
      Emass = 5.9E24
      Jmass = 1.9E27
      Nmass = 1.0E26
      
      open(1, file="output.txt", status = 'new')

c     Calculating periods of each planet      
      Eperiod = sqrt(EarthAU * EarthAU * EarthAU)
c     Earth's period is 1 year
      Jperiod = sqrt(JupiterAU * JupiterAU * JupiterAU)
c     Jupiter's period is 11.85 years
      Nperiod = sqrt(NeptuneAU * NeptuneAU * NeptuneAU)
c     Neptune's period is 84.32 years

      Write(1,*)'Name          Mass          Theta          Year'
      do while(time <= 500)
c     Determining theta for each planet at given time
        Etheta = ((1/Eperiod) * 360) * time
        Jtheta = ((1/Jperiod) * 360) * time
        Ntheta = ((1/Nperiod) * 360) * time
c     Writing information to file
        Write(1,5)'Earth  ',Emass,Etheta,time
        Write(1,5)'Jupiter',Jmass,Jtheta,time
        Write(1,5)'Neptune',Nmass,Ntheta,time
5       FORMAT(A,ES15.2,F15.2,F15.2)
        Write(1,*)'--------------------------------------------------'
        
        time = time + 10
      END do
      Write(1,6)'Earth made  ',(Etheta/360),'revolutions'
      Write(1,6)'Jupiter made',(Jtheta/360),'revolutions'
      Write(1,6)'Neptune made',(Ntheta/360),'revolutions'
6     FORMAT(A,F6.2,2X,A) 


      
      close(1)
      Write(*,*)'File written to "output.txt"'
      STOP
      END 
