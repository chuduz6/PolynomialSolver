C     SOLUTION OF EQUATION BY USING REGULA-FALSI METHOD

      FUNCTION F(X,A,B,C,D,E,G)
      F=A*X*X*X*X*X + B*X*X*X*X + C*X*X*X + D*X*X + E*X + G
      RETURN
      END
      PRINT 16
      PRINT 17
      PRINT 18
      PRINT 19
      PRINT 20
      PRINT 20
      
      CHARACTER INFILE

      CALL GETARG(1,INFILE)
      WRITE(*,*)INFILE
      OPEN (UNIT = 2, FILE = INFILE)
      READ(2,15)A,B,C,D,E,G,H
      CLOSE(2)

      WRITE(*,*) 'THE FUNCTION IS: '
      PRINT 11, A, B, C, D, E, G
      PRINT 20
      I =  F(H, A, B, C, D, E, G)
      PRINT 13, H, F(H, A, B, C, D, E, G)
      PRINT 20
      XLOWER=-10
      XUPPER=10
      COUNT=0
  8   IF((XLOWER-XUPPER)>0) THEN
      WRITE(*,*) '1NO ROOT FOUND'
      PRINT 20
      PRINT 21
      STOP
      END IF
      IF(COUNT.EQ.1) THEN
      XLOWER= XLOWER + 0.0001
      END IF
      COUNT=0
      YLOWER=F(XLOWER,A,B,C,D,E,G)
      YUPPER=F(XUPPER,A,B,C,D,E,G)
      IF((F(XLOWER,A,B,C,D,E,G)*F(XUPPER,A,B,C,D,E,G))> 0) THEN
      COUNT = 1
      END IF
      IF(F(XLOWER,A,B,C,D,E,G)*F(XUPPER,A,B,C,D,E,G)) 3,4,8
  4   IF(F(XLOWER,A,B,C,D,E,G).EQ.0) THEN
      WRITE(*,*)'ONE POSSIBLE SOLUTION IS: ',XLOWER
      PRINT 20
      PRINT 21
      ELSE
      WRITE(*,*)'ONE POSSIBLE SOLUTION IS: ',XUPPER
      PRINT 20
      PRINT 21
      STOP
      END IF
 3    TOL = 0.001
 5    X=(XUPPER*YLOWER-XLOWER*YUPPER)/(YLOWER-YUPPER)
      Y=F(X,A,B,C,D,E,G)
      IF(ABS(Y).LE.TOL) THEN
      WRITE(*,*)'ONE POSSIBLE SOLUTION IS: ',X
      PRINT 21
      STOP
      ENDIF
      IF(Y*YLOWER.LT.0) THEN
      XUPPER=X
      YUPPER=Y
      ELSE IF (Y*YUPPER.LT.0) THEN
      XLOWER=X
      YLOWER=Y
      ELSE
      COUNT = 1
      GO TO 8
      END IF
      GOTO 5
1     FORMAT (F4.3)
11    FORMAT ('Y = ',F4.2,'X^5 + ',F4.2,'X^4 + ',F4.2,'X^3 + ',F4.2,
     +     'X^2 + ',F4.2,
     +      'X + ',F4.2)
13    FORMAT ('THE VALUE OF THE FUNCTION AT ', F4.2, ' IS ', F20.10)
16    FORMAT ('MY NAME IS CHUDAMANI ARYAL. ')
17    FORMAT ('MY STUDENT ID IS: 1000692493 ')
18    FORMAT ('THIS IS MY FIRST FORTRAN PROGRAM: LAB 1 OF PL ')
19    FORMAT ('AND I AM LOVING IT ')
20    FORMAT ('')
21    FORMAT ('RUN COMPLETED')
15    FORMAT (7F10.8)
       END

