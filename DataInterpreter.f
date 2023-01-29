c     Cameron Dolly
c     CMS495 - Scientific Computing
      PROGRAM LineInterpreter
      Real :: fx, m, f, b, xAvg, yAvg, xTotal, yTotal, sumXX, sumXY
      Real :: squaredErrorTotal, MSE
      Real, dimension(100,2) :: x, y, xy
c     100 x 3 matrix, positon 1 refers to predicted value, position 2 error, and position 3 squared error
      Real, dimension(100,3) :: error
      Integer :: i, j

      i = 0 
      fx = 0
      m = 0
      f = 0
      b = 0
      xTotal = 0
      xAvg = 0
      yTotal = 0
      yAvg = 0
      j = 0
      sumXX = 0
      sumXY = 0
      squaredErrorTotal = 0
      MSE = 0

100   FORMAT(1X,A,1X,I3.1,1X,A)
200   FORMAT(3X,A,1X,F8.3,A,1X,F8.3)


      open(1, file = "points.txt", status = "old")
      Write(*,*)'File opened Successfully'
      Read(1,*)
      do i = 1, 100
        Read(1,*, IOSTAT = ios) x(i,1), y(i,1)
        if(ios < 0)then
          Write(*,100)'End of file reached. There are',i-1,'entries.'
          GO TO 10
        END if
      END do
10    close(unit = 1)
      Write(*,*)'Successfuly read file'
      i = i - 1

      do j = 1, i
        xTotal = xTotal + x(j,1)
        yTotal = yTotal + y(j,1)  
      END do

      xAvg = xTotal/i
      yAvg = yTotal/i

      do j = 1, i
        x(j,2) = x(j,1) - xAvg
        y(j,2) = y(j,1) - yAvg
        xy(j,1) = x(j,2) * x(j,2)
        xy(j,2) = x(j,2) * y(j,2)
      END do

      do j = 1, i
        sumXX = sumXX + xy(j,1) 
        sumXY = sumXY + xy(j,2)
      END do

c     Slope of the data
      m = sumXY/sumXX
      
c     Figuring out intercept
      b = yAvg - (m*xAvg)

      Write(*,*)'The line of best fit is:'
      Write(*,200)'y =',m,'x +',b

c     Calculating Mean Squared Error
      do j = 1, i
        error(j,1) = (m * x(j,1)) + b
        error(j,2) = y(j,1) - error(j,1)
        error(j,3) = error(j,2) * error(j,2)
        squaredErrorTotal = squaredErrorTotal + error(j,3)
      END do

      MSE = squaredErrorTotal / i

c     This program uses statistics in order to determine whether a set of data is exponential or linear
c     Mean standard error essentially measures how well a model fits the data set
c     If a data set is linear, the mean standard error is going to be low, or close to 0
c     If a data set is exponential, the mean standard error is going to be extremely high
c     The threshold value I used for this is 50, which is a relatively low value that still allows for some variability in a linear data set
      if(MSE >= 50)then
        Write(*,*)'The data is exponential'

      else if(MSE == 0)then
        Write(*,*)'The data is perfectly linear'
      else
        Write(*,*)'The data is  linear'
      END if

      STOP
      END 
