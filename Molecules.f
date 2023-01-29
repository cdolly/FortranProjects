c     Cameron Dolly
c     CMS495 - Scientific Computing
      PROGRAM Molecules
      Real :: size, waterMolecule, x1, y1, x2, y2, x3, y3, F,hypotenuse
      Real :: totalH, avgH, totalO, avgO, k, r, qa, qb, d, x, y
      Integer :: count, n, clouds, i

      size = 0
c     Water molecule is roughly 0.27 nanometers long
      waterMolecule = 0.27
      x1 = 0
      y1 = 0
      x2 = 0 
      y2 = 0
      x3 = 0
      y3 = 0
      n = 0
      totalH = 0
      avgH = 0
      totalO = 0
      avgO = 0
      F = 0
      k = 8.99E9
      r = 0.096
      qa = 0
      qb = 0
      clouds = 0
c     Distance in nanometers between hydrogen atoms in a water molecule
      d = .152
      hypotenuse = 0
      X = 0
      y = 0
      i = 0

100   FORMAT(F5.3,2X,F5.3)
200   FORMAT(A,1X,F5.3,A)
300   FORMAT(A,1X,I5.1,1X,A)
400   FORMAT(A,1X,I5.1,1X,A)

    

      Write(*,*)'Please input the size of the box'
1     Read *,size
      if(size <= .27)then
c       Because a water molecule is .27 nm long, do not allow a size smaller than .27
        Write(*,*)'Please input a number larger than .27'
        GO TO 1
      else if(size > (waterMolecule * 12))then
        Write(*,*)'Please input a number smaller than',
     &waterMolecule * 12
        GO TO 1
      END if

      Write(*,*)'How many iterations would you like to simulate?'
2     Read*,n
      if(n <= 0)then
        Write(*,*)'Please input a number larger than 0'
        GO TO 2   
      END if
      Write(*,*)'---------------------------------------------'
      Write(*,200)'The box is', size,'nm squared'
      Write(*,300)'The program will run', n,'times'
      Write(*,*)'---------------------------------------------'

      call System_Clock(count)
      call srand(count)

      do while(i < n)

c     Generating random x and y coordinates for water molecules
        x1 = rand() * (size - .002) + .001
        y1 = rand() * (size - .002) + .001

        x2 = rand() * (size - .002) + .001
        y2 = rand() * (size - .002) + .001

        x3 = rand() * (size - .002) + .001
        y3 = rand() * (size - .002) + .001

c     Calculating forces on from molecule 1 on molecule 1
c     For oxygen acting on hydrogen
        F = k * ((-1.6E-19 * 1.6E-19)/ (r * r))
        totalH = totalH + F
c     For hydrogen acting on hydrogen
        F = k * ((1.6E-19 * 1.6E-19)/ (d * d))
        totalH = totalH + F

c     For hyrdogen acting on oxygen
        F = k * ((-1.6E-19 * 1.6E-19)/ (r * r))
        totalO = totalO + F
        
c     Figure out distance between molecules
        x = abs(x1 - x2)
        y = abs(y1 - y2)
        hypotenuse = sqrt((x*x) + (y*y))
        if(hypotenuse <= (.5 * waterMolecule))then
            clouds = clouds + 1
        END if
c     For oxygen acting on molecule 1 hydrogen from molecule 2
        F = k * ((-1.6E-19 * 1.6E-19)/((r+hypotenuse)*(r+hypotenuse)))
        totalH = totalH + F
c     For hydrogen acting on molecule 1 hydrogen from molecule 2
c       Two hydrogen atoms, thus two calcs
        F = k * ((1.6E-19 * 1.6E-19)/((r+hypotenuse)*(r+hypotenuse)))
        totalH = totalH + F
        F = k * ((1.6E-19 * 1.6E-19)/((hypotenuse-r)*(hypotenuse-r)))
        totalH = totalH + F
c     For hydrogen acting on molecule 1 oxygen from molecule 2
c       Two hydrogen atoms, thus two calcs
        F = k * ((-1.6E-19 * 1.6E-19)/((r+hypotenuse)*(r+hypotenuse)))
        totalO = totalO + F
        F = k * ((-1.6E-19 * 1.6E-19)/((hypotenuse-r)*(hypotenuse-r)))
        totalO = totalO + F

c     For oxygen acting on molecule 1 oxygen from molecule 2
        F = k * ((1.6E-19 * 1.6E-19)/((r+hypotenuse)*(r+hypotenuse)))
        totalO = totalO + F
        F = k * ((1.6E-19 * 1.6E-19)/((hypotenuse-r)*(hypotenuse-r)))
        totalO = totalO + F
        

c     Figure out distance between molecules
        x = abs(x1 - x3)
        y = abs(y1 - y3)
        hypotenuse = sqrt((x*x) + (y*y))
        if(hypotenuse <= (.5 * waterMolecule))then
            clouds = clouds + 1
        END if
c     For oxygen acting on molecule 1 hydrogen from molecule 3
        F = k * ((-1.6E-19 * 1.6E-19)/((r+hypotenuse)*(r+hypotenuse)))
        totalH = totalH + F
c     For hydrogen acting on molecule 1 hydrogen from molecule 3
c       Two hydrogen atoms, thus two calcs
        F = k * ((1.6E-19 * 1.6E-19)/((r+hypotenuse)*(r+hypotenuse)))
        totalH = totalH + F
        F = k * ((1.6E-19 * 1.6E-19)/((hypotenuse-r)*(hypotenuse-r)))
        totalH = totalH + F
c     For hydrogen acting on molecule 1 oxygen from molecule 3
c       Two hydrogen atoms, thus two calcs
        F = k * ((-1.6E-19 * 1.6E-19)/((r+hypotenuse)*(r+hypotenuse)))
        totalO = totalO + F
        F = k * ((-1.6E-19 * 1.6E-19)/((hypotenuse-r)*(hypotenuse-r)))
        totalO = totalO + F

c     For oxygen acting on molecule 1 oxygen from molecule 3
        F = k * ((1.6E-19 * 1.6E-19)/((r+hypotenuse)*(r+hypotenuse)))
        totalO = totalO + F
        F = k * ((1.6E-19 * 1.6E-19)/((hypotenuse-r)*(hypotenuse-r)))
        totalO = totalO + F
      i = i + 1
      END do

      avgH = totalH / (8 * n)
      avgO = totalO / (8 * n)
      Write(*,400)'The simulation ran', n,'times'
      Write(*,*)'The average force on a hydrogen atom is', avgH,'N'
      Write(*,*)'The average force on an oxygen atom is', avgO,'N'
      Write(*,400)'There were', clouds,'clouds that formed'


      

      

      STOP
      END 
