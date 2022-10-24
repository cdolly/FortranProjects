c     Cameron Dolly
c     CMS495 - Scientific Computing
      PROGRAM ToothPick
      Real L, W, M, pi, xBoundMax, yBoundMax
      Real angleRadians, PiEstimate, angle
      Integer xPos, yPos, count, xEndPos, yEndPos, i, x
      L = 0.0
      W = 0.0
      M = 0.0
      x = 0.0
      pi = 4.D0 * DATAN(1.D0)
      xBoundMax = 0.0
      yBoundMax = 0.0
      xPos = 0
      yPos = 0
      angle = 0
      xEndPos = 0
      yEndPos = 0
      angleRadians = 0.0
      PiEstimate = 0.0

      Write(*,*)'Please input the size of the tile'
1     Read *,W
      if(W <= 0) then
        Write(*,*)'Please enter a size greater than 0'
        GO TO 1
      END if
2     Write(*,*)'Please input the size of the toothpick'
      Write(*,*)'The toothpick cannot be larger than the tile'
      Read *,L
      if(L <= 0) then
        Write(*,*)'Please enter a size greater than 0'
        GO TO 2
      END if

      if(L >= W) then
        Write(*,*)'Please enter a toothpick size smaller than'
        Write(*,*)' the size of the tile'
      GO TO 1
      END if

      Write(*,*)'Please input the number of toothpicks being dropped'
3     Read *,x
      if(x <= 0) then
        Write(*,*)'Number of toothpicks must be greater than 0'
        Write(*,*)'Please input a valid number'
        GO TO 3
      END if

c     Creating the tile boundary, ranging from 0 to W
      yBoundMax = W
      xBoundMax = W

      call System_Clock(count)
      call srand(count)

      do i = 1, x
c     Generating toothpick x and y position
        xPos = floor(rand() * W + 1)
        yPos = floor(rand() * W + 1)
c      Generating toothpick angle
        angle = floor(rand() * 360)
c      Converting angle to radians
        angleRadians = angle * (pi/180)
c     Figuring out where the end of the toothpick is
        yEndPos = yPos + (L * cos(angleRadians))
        xEndPos = xPos + (L * sin(angleRadians))

c     For debugging        
c        Write(*,*) angleRadians , 'angle in radians'
c        Write(*,*) cos(angleRadians), 'cos theta'
c        Write(*,*) angleRadians, 'angle in radians'
c        Write(*,*) sin(angleRadians), 'sin theta'
c        Write(*,*) xPos, 'X position'
c        Write(*,*) yPos, 'y position'
c        Write(*,*) angle, 'degrees'  
c        Write(*,*) xEndPos, 'X end position' 
c        Write(*,*) yEndPos, 'y end position'
         
        if(yEndPos >= yBoundMax .or. yEndPos <= 0) then
          M = M + 1
        else if(xEndPos >= xBoundMax .or. xEndPos <= 0) then
          M = M + 1
        END if
      END do

      Write(*,*)'The floor tiles are ', W, 'meters wide'
      Write(*,*)'The toothpicks are ', L, 'meters long'
      Write(*,*)'There were ', x, 'toothpicks thrown'
c      Write(*,*)'There were ',M, 'toothpicks that crossed the line' 
      PiEstimate = (4 * L) / ((M / x) * W)
      Write(*,*)'Estimate of pi is ', PiEstimate

      STOP
      END 
