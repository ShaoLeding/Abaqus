      SUBROUTINE USDFLD(FIELD,STATEV,PNEWDT,DIRECT,T,CELENT,TIME,DTIME,
     1 CMNAME,ORNAME,NFIELD,NSTATV,NOEL,NPT,LAYER,KSPT,KSTEP,KINC,
     2 NDI,nshr,coord,jmac,jmtyp,matlayo,laccflg)
C
      INCLUDE 'ABA_PARAM.INC'
C
C STRENGTH PARAMETERS
      PARAMETER(Xc=1458.0,Xt=2688.0,Yc=236.0,Yt=69.5,
     $    Zc=175.0,Zt=55.5,S12=136.0,S13=136.0,S23=95.6)
C
      CHARACTER*80 CMNAME,ORNAME
      CHARACTER*3  FLGRAY(15)
      DIMENSION FIELD(NFIELD),STATEV(NSTATV),DIRECT(3,3),T(3,3),TIME(2)
      DIMENSION ARRAY(15),JARRAY(15),coord(*),jmac(*),jmtyp(*)
C 
C CALCULATE COEFFICIENT OF TSAI-WU
      F1 = 1/Xt-1/Xc
      F2 = 1/Yt-1/Yc
      F3 = 1/Zt-1/Zc
      F11 = 1/(Xt*Xc)
      F22 = 1/(Yt*Yc)
      F33 = 1/(Zt*Zc)
      F44 = 1/S23**2
      F55 = 1/S13**2
      F66 = 1/S12**2
      F12 = -0.5/(Xt*Xc*Yt*Yc)**0.5
      F23 = -0.5/(Yt*Yc*Zt*Zc)**0.5
      F31 = -0.5/(Zt*Zc*Xt*Xc)**0.5
C
C INITIALIZE FAILURE FLAGS FROM STATEV. 
      CR11 = STATEV(1)
      CR22 = STATEV(2)
      CR33 = STATEV(3)
      CR12 = STATEV(4) 
      CR13 = STATEV(5)
      CR23 = STATEV(6)            
C
C GET STRESSES FROM PREVIOUS INCREMENT
      CALL GETVRM('S',ARRAY,JARRAY,FLGRAY,jrcd,
     $     jmac, jmtyp, matlayo, laccflg)
      ST11 = ARRAY(1)
      ST22 = ARRAY(2)
      ST33 = ARRAY(3)
      ST12 = ARRAY(4)
      ST13 = ARRAY(5)
      ST23 = ARRAY(6)
C
C TSAI-WU CRITERION
      CR = F1*ST11 +F2*ST22 + F3*ST33 + F11*ST11**2 + F22*ST22**2 +
     $      F33*ST33**2 + 2*F12*ST11*ST22 + 2*F23*ST22*ST33 + 
     $      2*F31*ST33*ST11 + F44*ST23**2 + F55*ST13**2 + F66*ST12**2
C
C FAILURE TYPE
      IF (CR>=1) THEN
          IF (CR11<1) THEN
              IF (ST11>=Xt .or. ST11<=-Xc) THEN
                  CR11=1
              ELSE
                  CR11=0
              ENDIF
           ENDIF
           IF (CR22<1) THEN
              IF (ST22>=Yt .or. ST22<=-Yc) THEN
                  CR22=1
              ELSE
                  CR22=0
              ENDIF
           ENDIF
           IF (CR33<1) THEN
              IF (ST33>=Zt .or. ST33<=-Zc) THEN
                  CR33=1
              ELSE
                  CR33=0
              ENDIF
           ENDIF
           IF (CR12<1) THEN
              IF (ABS(ST12)>=S12) THEN
                  CR12=1
              ELSE
                  CR12=0
              ENDIF
           ENDIF
           IF (CR13<1) THEN
              IF (ABS(ST13)>=S13) THEN
                  CR13=1
              ELSE
                  CR13=0
              ENDIF
           ENDIF
           IF (CR23<1) THEN
              IF (ABS(ST23)>=S23) THEN
                  CR23=1
              ELSE
                  CR23=0  
              ENDIF
           ENDIF
      ENDIF      
C
C     UPDATE STATEV            
      STATEV(1) = CR11 
      STATEV(2) = CR22
      STATEV(3) = CR33
      STATEV(4) = CR12
      STATEV(5) = CR13
      STATEV(6) = CR23      
C
C     UPDATE FIELD VARIABLES       
      IF (CR11>0) FIELD(1) = 1
      IF (CR22>0) FIELD(2) = 1
      IF (CR33>0) FIELD(3) = 1
      IF (CR12>0) FIELD(4) = 1
      IF (CR13>0) FIELD(5) = 1
      IF (CR23>0) FIELD(6) = 1
      
      RETURN
      END

