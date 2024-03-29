C     PROGRAM TO SOLVE UP TO FIFTH DEGREE POLYNOMIAL BY USING REGULA-FALSI METHOD

C     THIS IS A FUNCITON WHICH TAKES THE VALUE OF X
C     AND ALSO ALL THE CO-EEICIENTS AS PARAMATER
C     AND RETURNS AND RETURNS THE VALUE OF THE FUNCTION AT THAT PARTICULAR POINT
      FUNCTION F(X,A,B,C,D,E,G)
      F=A*X*X*X*X*X + B*X*X*X*X + C*X*X*X + D*X*X + E*X + G
      RETURN
      END
C     THIS IS THE START OF THE PROGRAMMING
C     SINCE WE ARE DEFINING THE NAME OF THE FILE IT SHOULD BE DECLARED AT THE FIRST
      CHARACTER *100 INPUT_FILE_NAME

C     ALL THESE PRINT STATEMENTS PRINTS THE NAME, ID AND STUFF
C     AND THE FORMATS OF THESE ARE ON THE BOTTOM OF THE PROGRAM
C     FOR EXAMPLE, PRINT 16 PRINTS WHATEVER THE FORMAT IS IN LABEL 16
      PRINT 16
      PRINT 17
      PRINT 18
      PRINT 19
      PRINT 20
      PRINT 20

C     SINCE THERE WONT BE USERS TO INPUT THE PARAMATERS FOR THE PROGRAM
C     WE ARE USING BACTCH FILE METHOD
C     THIS CALL GETS THE NAME OF THE INPUT FILE THAT IS TO BE DEFINED WHILE RUNNING THE PROGRAM
C     HERE GETARG IS AN IN-BUILT FUNCTION
      CALL GETARG(1,INPUT_FILE_NAME)

      IF(INPUT_FILE_NAME.EQ.' ') THEN
      READ(*,*) INPUT_FILE_NAME
       END IF

      
C     AFTER GETTING THE RIGHT FILENAME, IT OPENS THAT FILE
C     HERE MAXIMUM EXPECTED NUMBER OF INPUT IS SOME DEFUALT VALUE GIVEN BELOW
      OPEN (UNIT = 2, FILE = INPUT_FILE_NAME)
      DO R= 1, 70
C     AFTER OPENING THE RIGHT FILE, IT READS THE CONTENT OF THE FILE
         READ(2,15, END=28) A,B,C,D,E,G,H

C     AFTER FINISH READING THE FILE, IT CLOSES THE FILE

      XLOWER=-10
      XUPPER=10
      COUNT=0
C     AFTER GETTING THE INPUTS NEEDED, IT PRINTS THE FUNCTION
      PRINT 24
      PRINT 11, A, B, C, D, E, G


C     ALSO AS PER THE REQUIREMENT, IT ALSO PRINTS THE VALUE OF FUNCTION AT POINT X

      PRINT 13, H, F(H, A, B, C, D, E, G)


C     ALSO AS PER REQUIREMENT LOWER BOUNDARY AND UPPPER BOUNDARY ARE SET


C     THIS IF STATEMENT MAKES SURE THAT LOWER BOUNDARY NEVER CROSSES THE UPPER BOUNDARY
C     IF THAT HAPPENS THEN IT PRINTS THAT NO ROOTS ARE FOUND
  8   IF((XLOWER-XUPPER)>0) THEN
      PRINT 22
      PRINT 21
      PRINT 20
      GOTO 29
      END IF
      

      IF(COUNT.EQ.1) THEN
      XLOWER= XLOWER + 0.001
      END IF

C     THIS IS STATEMENT HANDLES THE GRAPH WE HAVE SAME SIGNS
C     THAT IS TO SAY IF BOTH FUNCTIONS ARE POSITIVE OR BOTH FUNCTIONS ARE NEGETIVE
C     WHEN THIS HAPPENS LOWER X IS ADJUSTED AND THE 3 WAY IF CONDITION IS CHECKED AGAIN
C     THIS KEEPS ON HAPPENING AS LONG AS BOTH FUNCTIONS HAVE OPPOSITE SIGNS
C      IF(COUNT.EQ.1) THEN
C      XLOWER= XLOWER + 0.00000001
C      END IF
           COUNT=0
      YLOWER=F(XLOWER,A,B,C,D,E,G)
      YUPPER=F(XUPPER,A,B,C,D,E,G)

C     WHEN BOTH FUNCTIONS HAVE SAME SIGN, THIS IF STATEMENTS TURNS ON THE FLAG OF SAME SIGN FUNCTION
      IF((F(XLOWER,A,B,C,D,E,G)*F(XUPPER,A,B,C,D,E,G))> 0) THEN
      COUNT = 1
      END IF

C     THIS IS A 3 WAY IF STATEMENTS WHICH COMPARES WHETHERE TWO FUNCTIONS BOTH SAME OR NOT
C     IT ALSO CHECKS WHETHER ONE OF THE FUNCTIONS IS ZERO OR NOT
      IF(F(XLOWER,A,B,C,D,E,G)*F(XUPPER,A,B,C,D,E,G)) 35,9,8

C     THE PROGRAM REACHES THE LABEL BELOW WHEN THE VALUES OF TWO FUCNTIONS ARE OF SOME SIGNS
C     AFTER THAT WE SET THE TOLERANCE
C     THEN BY USING THE FORMULA OF REGULAR FALSE POSITION, WE CALCULATE A NEW X VALUE AND
 35    TOLERANCE = 0.01
 5    X=(XUPPER*YLOWER-XLOWER*YUPPER)/(YLOWER-YUPPER)
      Y=F(X,A,B,C,D,E,G)

C     AFTER FINDING A NEW X, WE TRY TO CHECK WHETHER ITS FUNCTION VALUE IS CLOSE TO ZERO OR NOT,
C     IF YES, THEN  WE PRINT THE ROOT
      IF(ABS(Y).LE.TOLERANCE) THEN
      PRINT 23, X
      PRINT 21
      PRINT 20
      GOTO 29
      ENDIF


C     THE PROGRAM REACHES THE LABEL BELOW WHEN EITHER ONE OF THE FUCNTIONS HAVE ZERO VALUE
C     AND IF STATEMENT CHECKS WHICH OF THE FUNCTION IS THAT
C     AND AFTER FIND OUT THAT FUNCTION, IT PRINTS IT AS ONE OF THE SOLUTIONS
  9   IF(F(XLOWER,A,B,C,D,E,G).EQ.0) THEN
       PRINT 23,XLOWER
       PRINT 21
       PRINT 20
       GOTO 29
      ELSE
          PRINT 23,XUPPER
          PRINT 21
          PRINT 20
          GOTO 29
      END IF




C     WHEN THE NEW FUNTION VALUE IS NOT CLOSE TO ZERO,
C     THEN WE COMPARE THE NEW FUNTION VALUE WITH THE LOWER FUNCTION VLAUE AND THE UPPER FUNTION VALUE
C     IF NEW FUNCTION VALUE AND LOWER FUNCTION VALUE HAVE OPPOSITE SIGN, THEN WE KNOW THAT THE POSSIBLE ROOT IS BETWEEN NEW X AND LOWER X
C     HENCE WE ADJUST UPPPER X
      IF(Y*YLOWER.GT.0) THEN
      XLOWER=X
      YLOWER=Y

C     IF NEW FUNCTION VALUE AND UPPPER FUNCTION VALUE HAVE OPPOSITE SING, THEN WE KNOW THAT THE POSSIBLE ROOT IS BETWEEN NEW X AND UPPER Y
C     HENCE WE ADJUST UPPER Y
      ELSE
      XUPPER=X
      YUPPER=Y

      END IF

C     IF ELSE PART IS NOT EXECUTED THEN, AFTER THE ADJUSTMENT OF BOUNDARY AGAIN IT GOES TO THE LABEL WHERE NEW X IS CALCULATED
      GOTO 35

C     ALL THESE ARE TH FORMATS TO PRINT WHATEVER IS IN THE RESPECTIVE LABEL
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
22    FORMAT ('NO ROOT FOUND')
23    FORMAT ('ONE OF THE POSSIBLE ROOTS IS: ', F20.10)
24    FORMAT ('THE FUNCTION IS: ')
15    FORMAT (7F10.8)

29          END DO
28          CONTINUE
      CLOSE(2)
      WRITE (*,*) 'END OF PROGRAM!!! BYE!! THANKS FOR USING IT'
      END

