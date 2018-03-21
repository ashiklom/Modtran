       SUBROUTINE DRIVER                                                 driv 100
      include 'parameter.list'
      COMMON RELHUM(laydim),HSTOR(laydim),ICH(4),VH(17),TX(65),W(65)    driv 110
      COMMON IMSMX,WPATH(laythr,65),TBBY(laythr),PATM(laythr),NSPEC,     driv 130
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX2(47),AWCCON(5)    driv 140
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON /CARD1/ MODEL,ITYPE,IEMSCT,M1,M2,M3,IM,NOPRT,TBOUND,SALB   driv 160
     1  ,MODTRN                                                         driv 170
      LOGICAL MODTRN                                                    driv 180
      logical ground                                                    driv 190
      logical lsame                                                     driv 200
      COMMON /CARD1A/ M4,M5,M6,MDEF,IRD1,IRD2                           driv 210
      COMMON /CARD2/ IHAZE,ISEASN,IVULCN,ICSTL,ICLD,IVSA,VIS,WSS,WHH,   driv 220
     1    RAINRT                                                        driv 230
      COMMON /CARD2A/ CTHIK,CALT,CEXT                                   driv 240
      COMMON /CARD2D/ IREG(4),ALTB(4),IREGC(4)                          driv 250
      COMMON /CARD3/ H1,H2,ANGLE,RANGE,BETA,RE,LEN                      driv 260
C     COMMON /CARD4/ V1,V2,DV                                           driv 270
      COMMON/CARD4/IV1,IV2,IDV,IFWHM                                    driv 280
      COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     driv 290
      COMMON /CNTRL/ KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT               driv 300
      COMMON /MODEL/ ZM(LAYDIM),PM(LAYDIM),TM(LAYDIM),RFNDX(LAYDIM),
     1  DENSTY(65,LAYDIM),CLDAMT(LAYDIM),RRAMT(LAYDIM),EQLWC(LAYDIM),
     1  HAZEC(LAYDIM)
      COMMON/SOLS/AH1(LAYTWO),ARH(LAYTWO),WPATHS(LAYTHR,65),
     1 PA(LAYTWO),PR(LAYTWO),ATHETA(LAYDIM+1),ADBETA(LAYDIM+1),
     2 LJ(LAYTWO+1),JTURN,ANGSUN,CSZEN(LAYTWO),TBBYS(LAYTHR,12),
     3 PATMS(LAYTHR,12)
      COMMON /MART/ RHH                                                 driv 380
      COMMON /USRDTA/ NANGLS,ANGF(50),F(4,50)                           driv 390
      COMMON /MDLZ/ HMDLZ(8)                                            driv 400
      COMMON /ZVSALY/ ZVSA(10),RHVSA(10),AHVSA(10),IHVSA(10)            driv 410
      CHARACTER*4 HHAZE,HSEASN,HVULCN,BLANK,HMET,HMODEL,HTRRAD          driv 420
      COMMON /TITL/ HHAZE(5,16),HSEASN(5,2),HVULCN(5,8),BLANK,          driv 430
     1  HMET(5,2),HMODEL(5,8),HTRRAD(6,4)                               driv 440
      COMMON /VSBD/ VSB(10)                                             driv 450
      COMMON /PATH/PL(LAYTWO),QTHETA(LAYTWO),ITEST,HI,HF,
     1  AHT(LAYTWO),tph(LAYTWO)
      COMMON /AERTM/TAE7,TAE12,TAE13,TAE14,TAE16                        driv 480
      common /graund/gndalt                                             driv 490
      common /small3/small                                              driv 500
      common /solar/lsame                                               driv 510
      logical dis,sun1
      common /solar1/sun1,asun(50000),asun2(50000)
      common/co2mix /   co2rat, ch4rat,tozonerat, sozonerat,
     $     toffset,h2otscaled,
     $     h2orat
      logical lfirst                                                    driv 530
      data lfirst/.true./                                               driv 540
c                                                                       driv 550
cjd3 v
c
c******************************************
c        variables inserted by NORTH
        double precision uang
        integer nstr
        
c        by archer
        character*80 tmpdir,in5,out6, out7, out8, outs
        character*8 timestamp

c*******************************************
cjd3 
c     lfirst is true when first solar parameters are read in a series   driv 560
c     of runs involving solar parameters.                               driv 570
c                                                                       driv 580
C*****HDATE AND HTIME CARRY THE DATA AND TIME AND MUST BE DOUBLE        driv 590
C*****PRECISION ON A 32 BIT WORD COMPUTER                               driv 600
C@    DOUBLE PRECISION HDATE,HTIME                                      driv 610
      DIMENSION PLST(laytwo),CSENSV(laytwo),QTHETS(laytwo)  
      DATA IRPT / 0 /                                                   driv 630
C*****IRD, IPR, AND IPU ARE UNIT NUMBERS FOR INPUT, OUTPUT, AND         driv 640
C*****IPR1 = OUTPUT OF MOLECULAR TRANSMITTANCE                          driv 650
      MAXGEO = laytwo
      small=    2.0    

      IRD = 5
      IPR = 6
      IPU = 7
      IPR1 = 8
      ISCRCH = 10
c      open (IPU,FILE="/dev/null")
c      open (IPR1,FILE="/dev/null")
c      open (ISCRCH,FILE="/dev/null")


c      IRD = 1                                                           driv 670
c      IPR = 2                                                           driv 680
c      IPU = 7                                                           driv 690
c      IPR1= 8                                                           driv 700
c      ISCRCH = 10                                                       driv 710
ccjv V 6/9 change names to lower case
c      call getarg(1,tmpdir)
c      do iendpos=80,1,-1
c         if(tmpdir(iendpos:iendpos) .NE. ' ' .AND.
c     $      tmpdir(iendpos:iendpos) .NE. '/' ) then
c            goto 99999
c         endif
c      enddo
99999 continue

c      call getarg(2,timestamp)

c      in5 = tmpdir(1:iendpos) // '/tape5.' // timestamp
c      out6 = tmpdir(1:iendpos) // '/tape6.' // timestamp
c      out7 = tmpdir(1:iendpos) // '/tape7.' // timestamp
c      out8 = tmpdir(1:iendpos) // '/tape8.' // timestamp
c      outs = tmpdir(1:iendpos) // '/scrat.' // timestamp
c      print*,out5,out6, out7, out8, out9, outs
c      OPEN (IRD,FILE=in5,STATUS='OLD')                                  driv 720
c      OPEN (IPR,FILE=out6,STATUS='UNKNOWN')                             driv 730
c      OPEN (IPU,FILE=out7,STATUS='UNKNOWN',recl=300)
c      OPEN (IPR1,FILE=out8,STATUS='UNKNOWN')                            driv 750
cc      OPEN (ISCRCH,STATUS=outs,FORM='UNFORMATTED')                      driv 760
c      OPEN (ISCRCH,FILE=outs)  ! ,FORM='UNFORMATTED')                      driv 760

cjv  
C                                                                       driv 770
C
      OPEN (28,FILE='refbkg',STATUS='OLD') 
c
C     ALTITUDE PARAMETERS                                               driv 780
C                                                                       driv 790
C     ZMDL  COMMON/MODEL/  THE ALTITUDES USED IN LOWTRAN                driv 800
C     ZCVSA,ZTVSA,ZIVSA  CARD 3.3 LOWTRAN FOR VSA INPUT                 driv 810
C     ZVSA  NINE ALTITUDES GEN BY VSA ROUTINE                           driv 820
C                                                                       driv 830
      PI=2.0*ASIN(1.0)                                                  driv 840
      CA=PI/180.                                                        driv 850
      DEG= 1.0/CA                                                       driv 860
      RANGE=0.0                                                         driv 870
C*****GCAIR IS THE GAS CONSTANT FOR AIR IN UNITS OF MB/(GM CM-3 K)      driv 880
      GCAIR = 2.87053E+3                                                driv 890
C*****BIGNUM AND BIGEXP ARE THE LARGEST NUMBER AND THE LARGEST ARGUMENT driv 900
C*****EXP ALLOWED AND ARE MACHINE DEPENDENT. THE NUMBERS USED HERE ARE Fdriv 910
C*****A TYPICAL 32 BIT-WORD COMPUTER.                                   driv 920
      BIGNUM = 1.0E35                                                   driv 930
      BIGEXP = 87.0                                                     driv 940
C     THE VALUES FOR BIGNUM AND BIGEXP FOLLOW THE                       driv 950
C     DESCRIPTION UNDER EXP FUNCTION IN "IBM SYSTEM 360/                driv 960
C     AND SYSTEM 370 FORTRAN IV LANGUAGE"                               driv 970
C     BIGNUM = 4.3E68                                                   driv 980
C     BIGEXP = 174.6                                                    driv 990
      KMAX=63                                                           driv1000
C*****NL IS THE NUMBER OF BOUNDARIES IN THE STANDARD MODELS 1 TO 6      driv1010
C*****BOUNDARY 34 (AT 99999 KM) IS NO LONGER USED                       driv1020
      NL = 33                                                           driv1030
C*****CALL TIME AND DATE:                                               driv1040
C*****THE USER MAY WISH TO INCLUDE SUBROUTINES FDATE AND FCLOCK WHICH   driv1050
C*****RETURN THE DATE AND TIME IN MM/DD/YY AND HH.MM.SS FORMATS         driv1060
C*****RESPECTIVELY. THE REQUIRED ROUTINES FOR A CDC 6600 ARE INCLUDED ATdriv1070
C*****THE MAIN PROGRAM IN COMMENT CARDS.                                driv1080
C@    CALL FDATE(HDATE)                                                 driv1090
C@    CALL FCLOCK(HTIME)                                                driv1100
C                                                                       driv1110
C*****START CALCULATION                                                 driv1120
C                                                                       driv1130
C                                                                       driv1140
100   DO 10 II = 1,4                                                    driv1150
10    IREG(II) = 0                                                      driv1160
cjv 9/18      WRITE(IPR,1000)                                           driv1170
cjv 9/18 1000  FORMAT('1',20X,'*****  MODTRAN  *****')                  driv1180
      WRITE(IPR,*) '*****  MODTRAN3 Version 1.3  12/1/95  *****'        driv1180 
C@    WRITE(IPR,1010) HDATE,HTIME                                       driv1190
1010  FORMAT('1',20X,'*****  MODTRAN  *****',10X,2(1X,A8,1X))           driv1200
      DO 80 I=1,4                                                       driv1210
          DO 80 J=1,40                                                  driv1220
              ABSC(I,J)=0.                                              driv1230
              EXTC(I,J)=0.                                              driv1240
 80   ASYM(I,J)=0.                                                      driv1250
      JPRT = 0                                                          driv1260
      IKLO=1                                                            driv1270
C                                                                       driv1280
C*****CARD 1                                                            driv1290
C                                                                       driv1300
C     READ(IRD,1110)MODEL,ITYPE,IEMSCT,IMULT,M1,M2,M3,                  driv1310
C    1  M4,M5,M6,MDEF,IM,NOPRT,TBOUND,SALB                              driv1320

      READ(IRD,'(L1,I4,12I5,F8.3,F7.2)')MODTRN,MODEL,ITYPE,IEMSCT,      driv1330
     1  IMULT,M1,M2,M3,M4,M5,M6,MDEF,IM,NOPRT,TBOUND,SALB               driv1340

 1110 FORMAT(13I5,F8.3,F7.2)                                            driv1350
      tbous =  TBOUND
cjd3 v these lines added
c
c***********************************************
      lsalb = 0
      salbs = salb
      if(salb .lt. 0) then
         lsalb = -salb
      endif
C     WRITE(IPR,1111)MODEL,ITYPE,IEMSCT,IMULT,M1,M2,M3,                 driv1360
C    1  M4,M5,M6,MDEF,IM,NOPRT,TBOUND,SALB                              driv1370
      WRITE(IPR,'(15H0 CARD 1  *****,L1,I4,12I5,F8.3,F7.2)')MODTRN,MODELdriv1380
     1  ,ITYPE,IEMSCT,IMULT,M1,M2,M3,M4,M5,M6,MDEF,IM,NOPRT,TBOUND,SALB driv1390
 1111 FORMAT('0 CARD 1  *****',13I5,F8.3,F7.2)                          driv1400
C     IF(IMULT .EQ. 1 .AND. NOPRT.EQ. 1) NOPRT = 0                      driv1410
C                                                                       driv1420
C     SET THE NUMBER OF SPECIES TREATED WITH THE 1 CM-1 BAND MODEL.     driv1430
C     ALSO, FOR EACH SPECIES, SET THE POINTER WHICH MAPS THE HITRAN     driv1440
C     NUMERICAL LABEL TO THE LOWTRAN NUMERICAL LABEL.                   driv1450
C                                                                       driv1460
      NSPEC=12                                                          driv1470
      KPOINT( 1)=17                                                     driv1480
      KPOINT( 2)=36                                                     driv1490
      KPOINT( 3)=31                                                     driv1500
      KPOINT( 4)=47                                                     driv1510
      KPOINT( 5)=44                                                     driv1520
      KPOINT( 6)=46                                                     driv1530
      KPOINT( 7)=50                                                     driv1540
      KPOINT( 8)=54                                                     driv1550
      KPOINT( 9)=56                                                     driv1560
      KPOINT(10)=55                                                     driv1570
      KPOINT(11)=52                                                     driv1580
      KPOINT(12)=11                                                     driv1590
C                                                                       driv1600
      IRD1 = 0                                                          driv1610
      IRD2 = 0                                                          driv1620
      IF (MODEL.EQ.0) LEN = 0                                           driv1630
      IF((MODEL.EQ.0) .OR. (MODEL.EQ.7)) GO TO 110                      driv1640
      IF(M1.EQ.0) M1=MODEL                                              driv1650
      IF(M2.EQ.0) M2=MODEL                                              driv1660
      IF(M3.EQ.0) M3=MODEL                                              driv1670
      IF(M4.EQ.0) M4=MODEL                                              driv1680
      IF(M5.EQ.0) M5=MODEL                                              driv1690
      IF(M6.EQ.0) M6=MODEL                                              driv1700
      IF(MDEF.EQ.0) MDEF=1                                              driv1710
110   CONTINUE                                                          driv1720
      M=MODEL                                                           driv1730
      NPR = NOPRT                                                       driv1740
cjd3 v these lines added
c
c***********************************************
c         insert by NORTH reading an extra card
c         from tape5, variables dis, and nstr read in
             read(ird,'(2(l1,i4),7f10.3)') dis,nstr,sun1,isun,
     $     co2mx,ch4mx,tozonemx,sozonerat,toffset,h2otscaled,h2orat
             if(co2mx .lt. 0)co2mx = 330.
             if(ch4mx .lt. 0) ch4mx = 1.7
             if(tozonemx .lt. 0) tozonemx = 28
                
             co2rat = co2mx /330.
             ch4rat = ch4mx /1.7
             tozonerat = tozonemx/28.
             print*,'co2mx co2rat ch4rat h2orat t_o3rat s_o3rat',
     $            co2mx,co2rat,ch4rat, h2orat, tozonerat, sozonerat
c

          if(sun1)then
               close(11) 
               open(11,file='sun2',status='old')
               ik = 0
c
c              triangle filter
               mode = 2
c
c              sun block data 100 to 28000 from sun1 
c              isunv = 21 
c              correction factor added feb 16 1995
c
               do 1 i = 1,50000
               asun(i) = 0.
               asun2(i) = 0.
               if(i.gt.50) then 
                   read(11,*,end=11)ivs,asun(i)
                   v = ivs
                   vcor = (1.0E+04/V**2)*.0001  
                   asun(i)= asun(i)/vcor
                   asun2(i) = asun(i)
               endif 
               ik= ik+1
1              continue
11             if(isun.gt.3) call smth(isun,asun,ik,mode,asun2)
           endif
c
cjd3 ^
C*****CARD 2 AEROSOL MODEL                                              driv1750
      READ(IRD,1200)IHAZE,ISEASN,IVULCN,ICSTL,ICLD,IVSA,VIS,WSS,WHH,    driv1760
     1  RAINRT,GNDALT                                                   driv1770
1200  FORMAT(6I5,5F10.3)                                                driv1780
      WRITE(IPR,1201)IHAZE,ISEASN,IVULCN,ICSTL,ICLD,IVSA,VIS,WSS,WHH,   driv1790
     1  RAINRT,GNDALT                                                   driv1800
      IF(GNDALT.GT.0.) WRITE(IPR,1199)GNDALT                            driv1810
1199  FORMAT(1H0,'  GNDALT =',F10.2)                                    driv1820
      IF(GNDALT.GE.6.0) THEN                                            driv1830
          WRITE(IPR,1202)GNDALT                                         driv1840
          GNDALT=0.                                                     driv1850
      ENDIF                                                             driv1860
1201  FORMAT('0 CARD 2  *****',6I5,5F10.3)                              driv1870
1202  FORMAT('0 GNDALT GT 6.0 RESET TO ZERO, GNDALT WAS',F10.3)         driv1880
C                                                                       driv1890
      IF(VIS.LE.0.0.AND.IHAZE.GT.0) VIS=VSB(IHAZE)                      driv1900
      RHH= 0.                                                           driv1910
      IF(MODEL.EQ.0.OR.MODEL.EQ.7) GO TO 205                            driv1920
      IF((MODEL.EQ.3.OR.MODEL.EQ.5).AND.ISEASN.EQ.0) ISEASN=2           driv1930
C                                                                       driv1940
      IF(IVSA.EQ.1 .AND. IHAZE.EQ.3)                                    driv1950
     1  CALL MARINE(VIS,MODEL,WSS,WHH,ICSTL,EXTC,ABSC,1)                driv1960
      ICH(1)=IHAZE                                                      driv1970
      ICH(2)=6                                                          driv1980
      ICH(3)=9+IVULCN                                                   driv1990
205   IF(RAINRT.EQ.0) GO TO 210                                         driv2000
      WRITE(IPR,1205) RAINRT                                            driv2010
1205  FORMAT('0 RAIN MODEL CALLED, RAIN RATE = ',F9.2,' MM/HR')         driv2020
 210  ICH(4)=18                                                         driv2030
      IF(ICH(1).LE.0)ICH(1)=1                                           driv2040
      IF(ICH(3).LE.9)ICH(3)=10                                          driv2050
      IF(ICLD.GE.1 .AND. ICLD.LE.11) THEN                               driv2060
          ICH(4)=ICH(3)                                                 driv2070
          ICH(3)=ICH(2)                                                 driv2080
          ICH(2)=ICLD                                                   driv2090
      END IF                                                            driv2100
      IFLGA=0                                                           driv2110
      IFLGT=0                                                           driv2120
      CTHIK=-99.                                                        driv2130
      CALT=-99.                                                         driv2140
      CEXT=-99.                                                         driv2150
      ISEED=-99                                                         driv2160
      IF(ICLD .LT. 18) GO TO 230                                        driv2170
C*****CARD 2A CIRRUS CLOUDS                                             driv2180
      READ(IRD,1210)CTHIK,CALT,CEXT,ISEED                               driv2190
1210  FORMAT(3F10.3,I10)                                                driv2200
      WRITE(IPR,1211)CTHIK,CALT,CEXT,ISEED                              driv2210
1211  FORMAT('0 CARD 2A *****',3F10.3,I10)                              driv2220
230   CONTINUE                                                          driv2230
C*****CARD 2B VERTICAL STRUCTURE ALGORITHM                              driv2240
      ZCVSA=-99.                                                        driv2250
      ZTVSA=-99.                                                        driv2260
      ZINVSA=-99.                                                       driv2270
C                                                                       driv2280
      IF( IVSA. EQ. 0 ) GO TO 240                                       driv2290
      READ (IRD,1230) ZCVSA,ZTVSA,ZINVSA                                driv2300
1230  FORMAT(3F10.3)                                                    driv2310
      WRITE(IPR,1231)ZCVSA,ZTVSA,ZINVSA                                 driv2320
1231  FORMAT('0 CARD 2B *****',3F10.3)                                  driv2330
C                                                                       driv2340
      CALL VSA(IHAZE,VIS,ZCVSA,ZTVSA,ZINVSA,ZVSA,RHVSA,AHVSA,IHVSA)     driv2350
C                                                                       driv2360
C     END OF VSA MODEL SET-UP                                           driv2370
C                                                                       driv2380
240   IF (MODEL.NE.0 .AND. MODEL.NE.7 ) ML=NL                           driv2390
      MDELS=MODEL                                                       driv2400
      DO 250 I=1,5                                                      driv2410
          IF(MDELS.NE.0)HMODEL(I,7)=HMODEL(I,MDELS)                     driv2420
250   IF(MDELS.EQ.0)HMODEL(I,7)=HMODEL(I,8)                             driv2430
C                                                                       driv2440
      IF(IM .EQ. 1) THEN                                                driv2450
          IF((MODEL.EQ.7.AND.IM.EQ.1) .OR.(MODEL.EQ.0)) THEN            driv2460
C                                                                       driv2470
C*****CARD 2C  USER SUPPLIED ATMOSPHERIC PROFILE                        driv2480
C                                                                       driv2490
              READ (IRD,1250) ML,IRD1,IRD2,(HMODEL(I,7),I=1,5)          driv2500
1250          FORMAT(3I5,18A4)                                          driv2510
              WRITE(IPR,1251)ML,IRD1,IRD2,(HMODEL(I,7),I=1,5)           driv2520
              IF(IVSA.EQ.1)CALL RDNSM                                   driv2530
1251          FORMAT('0 CARD 2C *****',3I5,18A4)                        driv2540
          ENDIF                                                         driv2550
      ENDIF                                                             driv2560
      M=7                                                               driv2570
      CALL AERNSM(JPRT,  GNDALT)                                        driv2580
      IF(ICLD .LT. 20) GO TO 260                                        driv2590
C                                                                       driv2600
C     SET UP CIRRUS MODEL                                               driv2610
C                                                                       driv2620
      IF(CTHIK.NE.0) IFLGT=1                                            driv2630
      IF(CALT.NE.0)  IFLGA=1                                            driv2640
      IF(ISEED.EQ.0) IFLGT=2                                            driv2650
      IF(ISEED.EQ.0) IFLGA=2                                            driv2660
      CALL CIRRUS(CTHIK,CALT,ISEED,CPROB,CEXT)                          driv2670
      WRITE(IPR,1220)                                                   driv2680
1220  FORMAT(15X,'CIRRUS ATTENUATION INCLUDED (N O A A CIRRUS) ')       driv2690
      IF(IFLGT.EQ.0) WRITE(IPR,1221) CTHIK                              driv2700
1221  FORMAT(15X,'CIRRUS ATTENUTION STATISTICALLY DETERMENED TO BE',    driv2710
     1  F10.3,'KM')                                                     driv2720
      IF(IFLGT.EQ.1) WRITE(IPR,1222) CTHIK                              driv2730
1222  FORMAT(15X,'CIRRUS THICKNESS USER DETERMINED TO BE',F10.3,'KM')   driv2740
      IF(IFLGT.EQ.2) WRITE(IPR,1223) CTHIK                              driv2750
1223  FORMAT(15X,'CIRRUS THICKNESS DEFAULTED TO MEAN VALUE OF    ',     driv2760
     1  F10.3,'KM')                                                     driv2770
      IF(IFLGA.EQ.0) WRITE(IPR,1224)CALT                                driv2780
1224  FORMAT(15X,'CIRRUS BASE ALTITUDE STATISCALLY DETERMINED TO BE',   driv2790
     1  F10.3,' KM')                                                    driv2800
      IF(IFLGA.EQ.1) WRITE(IPR,1225) CALT                               driv2810
1225  FORMAT(15X,'CIRRUS BASE ALTITUDE USER DETERMINED TO BE',          driv2820
     1  F10.3,' KM')                                                    driv2830
      IF(IFLGA.EQ.2) WRITE(IPR,1226) CALT                               driv2840
1226  FORMAT(15X,'CIRRUS BASE ALTITUDE DEFAULTED TO MEAN VALUE OF',     driv2850
     1  F10.3,'KM')                                                     driv2860
      WRITE(IPR,1227)CPROB                                              driv2870
1227  FORMAT(15X,'PROBABILTY OF CLOUD OCCURRING IS',F7.1,' PERCENT')    driv2880
C                                                                       driv2890
C       END OF CIRRUS MODEL SET UP                                      driv2900
C                                                                       driv2910
260   CONTINUE                                                          driv2920
C                                                                       driv2930
C                                                                       driv2940
C*****CARD 2E                                                           driv2950
C                                                                       driv2960
      IF((IHAZE.EQ.7).OR.(ICLD.EQ.11)) THEN                             driv2970
C                                                                       driv2980
C*****    CARD 2E USER SUPPLIED AEROSOL EXTINCTION, ABSORPTION, AND     driv2990
C         ASYMMETRY                                                     driv3000
          CALL RDEXA                                                    driv3010
C                                                                       driv3020
      ENDIF                                                             driv3030
300   CONTINUE                                                          driv3040
C                                                                       driv3050
      IPARM =-99                                                        driv3060
      IPH   =-99                                                        driv3070
      IDAY  =-99                                                        driv3080
      ISOURC=-99                                                        driv3090
C                                                                       driv3100
      PARM1 =-99.                                                       driv3110
      PARM2 =-99.                                                       driv3120
      PARM3 =-99.                                                       driv3130
      PARM4 =-99.                                                       driv3140
      TIME  =-99.                                                       driv3150
      PSIPO =-99.                                                       driv3160
      ANGLEM=-99.                                                       driv3170
      G     =-99.                                                       driv3180
C                                                                       driv3190
C*****CARD 3 GEOMETERY PARAMETERS                                       driv3200
C                                                                       driv3210
      IF(IEMSCT.EQ.3) GO TO 315                                         driv3220
      READ(IRD,1312)H1,H2,ANGLE,RANGE,BETA,RO,LEN                       driv3230
1312  FORMAT(6F10.3,I5)                                                 driv3240
      WRITE(IPR,1313)H1,H2,ANGLE,RANGE,BETA,RO,LEN                      driv3250
cjd3 v these lines added
c
c****************************************
c          saving the angle for DISORT-NORTH
             uang=angle
c*****************************************
c
cjd3  
1313  FORMAT('0 CARD 3  *****',6F10.3,I5)                               driv3260
      GO TO 320                                                         driv3270
C*****CARD 3 FOR DIRECTLY TRANSMITTED SOLAR RADIANCE (IEMSCT = 3)       driv3280
  315 CONTINUE                                                          driv3290
      READ(IRD,1316) H1,H2,ANGLE,IDAY,RO,ISOURC,ANGLEM                  driv3300
 1316 FORMAT(3F10.3,I5,5X,F10.3,I5,F10.3)                               driv3310
      WRITE(IPR,1317) H1,H2,ANGLE,IDAY,RO,ISOURC,ANGLEM                 driv3320
 1317 FORMAT('0 CARD 3   *****',3F10.3,I5,5X,F10.3,I5,F10.3)            driv3330
      ITYPE = 3                                                         driv3340
      RANGE = 0.0                                                       driv3350
      BETA = 0.0                                                        driv3360
      LEN = 0                                                           driv3370
C*****RO IS THE RADIUS OF THE EARTH                                     driv3380
320   RE=6371.23                                                        driv3390
C      *********  ERRATA JULY 25                                        driv3400
           IF(H1. LT. ZM(1)  ) THEN                                     driv3410
           WRITE(IPR,905) H1,ZM(1)                                      driv3420
905        FORMAT('  H1 LESS THAN FIRST ALT RESET  ',/                  driv3430
     X     ' H1 WAS ',F10.2,' 1ST ALT = ',F10.2)                        driv3440
            H1 = ZM(1)                                                  driv3450
      ENDIF                                                             driv3460
C        *********  END  ERRATA                                         driv3470
      H1S    = H1                                                       driv3480
      H2S    = H2                                                       driv3490
      ANGLES = ANGLE                                                    driv3500
      RANGS  = RANGE                                                    driv3510
      BETAS  = BETA                                                     driv3520
      ITYPES =ITYPE                                                     driv3530
      LENS   = LEN                                                      driv3540
cc    IRPTS = IRPT                                                      driv3550
      IF (MODEL.EQ.0) RO = RE                                           driv3560
      IF (MODEL.EQ.1) RE=6378.39                                        driv3570
      IF (MODEL.EQ.4) RE=6356.91                                        driv3580
      IF (MODEL.EQ.5) RE=6356.91                                        driv3590
      IF (RO.GT.0.0) RE=RO                                              driv3600
C                                                                       driv3610
      IF (IEMSCT.NE.2) GO TO 330                                        driv3620
C                                                                       driv3630
C*****CARD 3A1                                                          driv3640
C                                                                       driv3650
      READ(IRD,1320) IPARM,IPH,IDAY,ISOURC                              driv3660
1320  FORMAT(4I5)                                                       driv3670
      WRITE(IPR,1321) IPARM,IPH,IDAY,ISOURC                             driv3680
1321  FORMAT('0 CARD 3A1*****',4I5)                                     driv3690
C                                                                       driv3700
C*****CARD 3A2                                                          driv3710
C                                                                       driv3720
      READ(IRD,1322)PARM1,PARM2,PARM3,PARM4,TIME,PSIPO,ANGLEM,G         driv3730
1322  FORMAT(8F10.3)                                                    driv3740
      WRITE(IPR,1323)PARM1,PARM2,PARM3,PARM4,TIME,PSIPO,ANGLEM,G        driv3750
1323  FORMAT('0 CARD 3A2*****',8F10.3)                                  driv3760
C                                                                       driv3770
CSSISSISSISSISSI  CHANGES  BEGIN.                                       driv3780
c                                                                       driv3790
      REWIND(ISCRCH)                                                    driv3800
C                                                                       driv3810
      IF (LFIRST .AND. IMULT .EQ. 1) THEN                               driv3820
C                                                                       driv3830
C        SAVE SOLAR PARAMETERS FOR COMPARING LATER.                     driv3840
C        NOTE THAT LFIRST IS TRUE AND IMULT (MULTIPLE SOLAR SCATTERING) driv3850
         LFIRST = .FALSE.                                               driv3860
         CALL SVSOLA(IPARM,IPH,IDAY,ISOURC,PARM1,PARM2,PARM3,PARM4,     driv3870
     $        TIME,PSIPO,ANGLEM,                                        driv3880
     $        ISAVE1,ISAVE2,ISAVE3,ISAVE4,SAVE1,SAVE2,SAVE3,SAVE4,      driv3890
     $        SAVE5,SAVE6,SAVE7)                                        driv3900
         LSAME = .FALSE.                                                driv3910
C                                                                       driv3920
      ELSEIF (IMULT .EQ. 1 .AND. IRPT .EQ. 3.and. .not.dis) THEN 
C                                                                       driv3940
C        NOW COMPARE SOLAR PARAMETERS; LSAME IS TRUE IF THEY MATCH.     driv3950
         CALL COMPAR(IPARM,IPH,IDAY,ISOURC,PARM1,PARM2,PARM3,PARM4,     driv3960
     $        TIME,PSIPO,ANGLEM,                                        driv3970
     $        ISAVE1,ISAVE2,ISAVE3,ISAVE4,SAVE1,SAVE2,SAVE3,SAVE4,      driv3980
     $        SAVE5,SAVE6,SAVE7,LSAME)                                  driv3990
         CALL SVSOLA(IPARM,IPH,IDAY,ISOURC,PARM1,PARM2,PARM3,PARM4,     driv4000
     $        TIME,PSIPO,ANGLEM,                                        driv4010
     $        ISAVE1,ISAVE2,ISAVE3,ISAVE4,SAVE1,SAVE2,SAVE3,SAVE4,      driv4020
     $        SAVE5,SAVE6,SAVE7)                                        driv4030
      ELSE                                                              driv4040
C                                                                       driv4050
C        GET READY FOR ANOTHER POSSIBLE FORTHCOMING SERIES OF MULTIPLE  driv4060
C        SOLAR SCATTERING RUNS.                                         driv4070
         LFIRST = .TRUE.                                                driv4080
         LSAME = .FALSE.                                                driv4090
      ENDIF                                                             driv4100
c                                                                       driv4110
CSSISSISSISSISSI  CHANGES  END                                          driv4120
C                                                                       driv4130
      IF(IPH. EQ . 0) THEN                                              driv4140
            IF(G. GE.  1.0) G =  .9999                                  driv4150
            IF(G. LE. -1.0) G = -.9999                                  driv4160
      ENDIF                                                             driv4170
      IF (IPH.NE.1) GO TO 330                                           driv4180
C                                                                       driv4190
C*****CARD 3B1 USER DEFINED PHASE FUNCTION                              driv4200
C                                                                       driv4210
C*****READ USER DEFINED PHASE FUNCTION                                  driv4220
C                                                                       driv4230
      READ(IRD,1326)NANGLS                                              driv4240
1326  FORMAT(I5)                                                        driv4250
      WRITE(IPR,1327)NANGLS                                             driv4260
1327  FORMAT(' CARD 3B1*****',I5)                                       driv4270
C                                                                       driv4280
C*****CARD 3B2                                                          driv4290
C                                                                       driv4300
      READ(IRD,1328)(ANGF(I),F(1,I),F(2,I),F(3,I),F(4,I),I=1,NANGLS)    driv4310
1328  FORMAT(5E10.3)                                                    driv4320
      WRITE(IPR,1329)(ANGF(I),F(1,I),F(2,I),F(3,I),F(4,I),I=1,NANGLS)   driv4330
1329  FORMAT('0 CARD 3B2*****',5E10.3)                                  driv4340
C                                                                       driv4350
  330 CONTINUE                                                          driv4360
C                                                                       driv4370
cc      IF (IRPT.EQ.3) GO TO 500                                        driv4380
        IF (IRPT.EQ.3) THEN                                             driv4390
            IF(IPARM .EQ. 1) CALL SUBSOL (PARM3,PARM4,TIME,IDAY)        driv4400
            GO TO 555                                                   driv4410
        ENDIF                                                           driv4420
C                                                                       driv4430
C*****CARD 4 WAVENUMBER                                                 driv4440
C                                                                       driv4450
 400  CONTINUE                                                          driv4460
C     READ(IRD,1400)V1,V2,DV                                            driv4470
C1400 FORMAT(3F10.3)                                                    driv4480
C     WRITE (IPR,1401) V1,V2,DV                                         driv4490
C1401 FORMAT('0 CARD 4  *****',3F10.3)                                  driv4500
      READ(IRD,'(4I10)')IV1,IV2,IDV,IFWHM                               driv4510
cc    IF(IV1.GE.22681)modtrn=.false.      
      if(dis.and.iv2.le.20)dis=.false.
      if(dis.and.iv1.le.10)iv1=10
  401 WRITE(IPR,'(15H0 CARD 4  *****,4I10)')IV1,IV2,IDV,IFWHM           driv4520
      salb = salbs
      if(salb.lt.0)call rhoeps(-1,lsalb,salbv,dumn)
      IF(IDV . LE. 0)THEN                                               driv4530
         PRINT*,' ERROR IN IDV ',IDV                                    driv4540
         IDV = 1                                                        driv4550
      ENDIF                                                             driv4560
      IF(IFWHM . LE. 0)THEN                                             driv4570
         PRINT*,' ERROR IN IFWHM ',IFWHM                                driv4580
         IFWHM = 2                                                      driv4590
      ENDIF                                                             driv4600
      IF(IHAZE.EQ.3) THEN                                               driv4610
C         IF(V1.LT.250.0 .OR. V2.LT.250.0) THEN                         driv4620
          IF(IV1.LT.250)THEN                                            driv4630
              IHAZE=4                                                   driv4640
              WRITE (IPR,1203)                                          driv4650
          ENDIF                                                         driv4660
1203      FORMAT('0**WARNING** NAVY MODEL IS NOT USABLE BELOW 250CM-1', driv4670
     1    /,10X,' PROGRAM WILL SWITCH TO IHAZE=4 LOWTRAN 5 MARITIME',//)driv4680
      END IF                                                            driv4690
      IF (IRPT.EQ.4)  GO TO 550                                         driv4700
cc    IF (IRPT.EQ.-4) GO TO 560                                         driv4710
500   CONTINUE                                                          driv4720
      IF (IRPT.EQ.3) GO TO 555                                          driv4730
      WRITE(IPR,1410) (HTRRAD(I1,IEMSCT+1),I1=1,6)                      driv4740
1410  FORMAT('0 PROGRAM WILL COMPUTE ',6A4)                             driv4750
      IF(ISOURC .EQ. 1) WRITE(IPR,1204)                                 driv4760
1204  FORMAT('   LUNAR SOURCE ONLY  ')                                  driv4770
      IF (IMULT .EQ. 1) THEN                                            driv4780
          IF(IEMSCT.EQ.0 .OR. IEMSCT.EQ.3 ) THEN                        driv4790
              WRITE(IPR,1411)                                           driv4800
1411          FORMAT('0 MULTIPLE SCATTERING HAS BEEN TURNED OFF ')      driv4810
              IMULT=0                                                   driv4820
          ELSE                                                          driv4830
              WRITE(IPR,1412)                                           driv4840
          END IF                                                        driv4850
      END IF                                                            driv4860
1412  FORMAT('0 CALCULATIONS WILL BE DONE USING MULTIPLE SCATTERING ')  driv4870
      MDEL=MODEL                                                        driv4880
      IF(MDEL.EQ.0)MDEL=8                                               driv4890
      MM1=MDEL                                                          driv4900
      MM2=MDEL                                                          driv4910
      MM3=MDEL                                                          driv4920
      IF(M1.NE.0)MM1=M1                                                 driv4930
      IF(M2.NE.0)MM2=M2                                                 driv4940
      IF(M3.NE.0)MM3=M3                                                 driv4950
      IF(MODEL.EQ.0) GO TO 510                                          driv4960
      WRITE(IPR,1500) MM1,(HMODEL(I1,MM1),I1=1,5),MM2,(HMODEL(I2,MM2),  driv4970
     1  I2=1,5),MM3,(HMODEL(I3,MM3),I3=1,5)                             driv4980
1500  FORMAT('0 ATMOSPHERIC MODEL',/,                                   driv4990
     1  10X,'TEMPERATURE = ',I4,5X,5A4,/,                               driv5000
     1  10X,'WATER VAPOR = ',I4,5X,5A4,/,                               driv5010
     1  10X,'OZONE       = ',I4,5X,5A4)                                 driv5020
      WRITE(IPR,1501) M4,M5,M6,MDEF                                     driv5030
1501  FORMAT(20X,'  M4 = ',I5,' M5 = ',I5,' M6 = ',I5,' MDEF = ' ,I5)   driv5040
C                                                                       driv5050
510   IF(JPRT.EQ.0) GO TO 520                                           driv5060
      IF(ISEASN.EQ.0)ISEASN=1                                           driv5070
      IF(IVULCN.LE.0) IVULCN=1                                          driv5080
      IHVUL=IVULCN+10                                                   driv5090
      IF( IVULCN .EQ. 6) IHVUL = 11                                     driv5100
      IF( IVULCN .EQ. 7) IHVUL = 11                                     driv5110
      IF( IVULCN .EQ. 8) IHVUL = 13                                     driv5120
      IHMET=1                                                           driv5130
      IF(IVULCN.GT.1)IHMET=2                                            driv5140
      IF(IHAZE.EQ.0) GO TO 520                                          driv5150
      WRITE(IPR,1510)(HHAZE(I,IHAZE),I=1,5),VIS,(HHAZE(I2,6),I2=1,5),   driv5160
     1  (HHAZE(II,6),II=1,5),(HSEASN(IA,ISEASN),IA=1,5),                driv5170
     2  (HHAZE(I3,IHVUL),I3=1,5),                                       driv5180
     3  (HVULCN(IB,IVULCN),IB=1,5),(HSEASN(IC,ISEASN),IC=1,5),          driv5190
     4  (HHAZE(I4,16),I4=1,5),(HMET(I5,IHMET),I5=1,5)                   driv5200
1510  FORMAT('0 AEROSOL MODEL',/,10X,'REGIME',                          driv5210
     A  T35,'AEROSOL TYPE',T60,'PROFILE',T85,'SEASON',/,/,              driv5220
     B  10X,'BOUNDARY LAYER (0-2 KM)',T35,5A4,T60,F5.1,                 driv5230
     C  ' KM VIS AT SEA LEVEL',/,10X,'TROPOSPHERE  (2-10KM)',T35,       driv5240
     D  5A4,T60,5A4,T85,5A4,/,10X,'STRATOSPHERE (10-30KM)',             driv5250
     E  T35,5A4,T60,5A4,T85,5A4,/,10X,'UPPER ATMOS (30-100KM)',         driv5260
     F  T35,5A4,T60,5A4)                                                driv5270
520   CONTINUE                                                          driv5280
      IF(ITYPE.EQ.1) WRITE(IPR,1515) H1,RANGE                           driv5290
1515  FORMAT('0 HORIZONTAL PATH',/,10X,'ALTITUDE = ',F10.3,' KM',/,     driv5300
     1  10X,'RANGE    = ',F10.3,' KM')                                  driv5310
      IF(ITYPE.EQ.2) WRITE(IPR,1516) H1,H2,ANGLE,RANGE,BETA,LEN         driv5320
1516  FORMAT('0 SLANT PATH, H1 TO H2',/,                                driv5330
     1  10X,'H1    = ',F10.3,' KM',/,10X,'H2    = ',F10.3,' KM',/,      driv5340
     2  10X,'ANGLE = ',F10.3,' DEG',/,10X,'RANGE = ',F10.3,' KM',/,     driv5350
     3  10X,'BETA  = ',F10.3,' DEG',/,10X,'LEN   = ',I6)                driv5360
      IF(ITYPE.EQ.3) WRITE(IPR,1517) H1,H2,ANGLE                        driv5370
1517  FORMAT('0 SLANT PATH TO SPACE',/,                                 driv5380
     1  10X, 'H1    = ',F10.3,' KM',/,10X,'HMIN  = ',F10.3,' KM',/,     driv5390
     2  10X,'ANGLE = ',F10.3,' DEG')                                    driv5400
      IF (IEMSCT.NE.2) GO TO 550                                        driv5410
C                                                                       driv5420
C*****INTREPRET SOLAR SCATTERING PARAMETERS                             driv5430
C                                                                       driv5440
C                                                                       driv5450
      IF (IPARM.EQ.1) CALL SUBSOL (PARM3,PARM4,TIME,IDAY)               driv5460
C                                                                       driv5470
      WRITE (IPR,1530)                                                  driv5480
1530  FORMAT('0 SINGLE SCATTERING CONTROL PARAMETERS SUMMARY '/)        driv5490
      IF(IPARM.NE.2) WRITE (IPR,1532) PARM1,PARM2,PARM3,PARM4,TIME,PSIPOdriv5500
     1,IDAY                                                             driv5510
1532  FORMAT(10X,'OBSERVER LATITUDE =',T35,F10.2,' DEG NORTH OF EQUATOR'driv5520
     1  ,/,10X,'OBSERVER LONGITUDE=',T35,F10.2,' DEG WEST OF GREENWICH',driv5530
     2  /,10X,'SUBSOLAR LATITUDE =',T35,F10.2,' NORTH OF EQUATOR',/,    driv5540
     3  10X,'SUBSOLAR LONGITUDE =',T35,F10.2,' WEST OF GREENWICH',/,    driv5550
     4  10X,'TIME (<0 IS UNDEF)=',T35,F10.3,' GREENWICH TIME',/,        driv5560
     5  10X,'PATH AZIMUTH =',T35,F10.3,' DEG EAST OF NORTH',/,          driv5570
     6  10X,'DAY OF YEAR =',T35,I10)                                    driv5580
      IF (IPARM.EQ.2) WRITE (IPR,1534)PARM1,PARM2,TIME,PSIPO,IDAY       driv5590
1534  FORMAT(10X,'RELATIVE AZIMUTH =',T35,F10.3,' DEG EAST OF NORTH',/, driv5600
     1  10X,'SOLAR ZENITH =',T35,F10.3,' DEG ',/,                       driv5610
     2  10X,'TIME (<0 UNDEF) =',T35,F10.3,' GREENWICH TIME',/,          driv5620
     3  10X,'PATH AZIMUTH =',T35,F10.3,' DEG EAST OF NORTH',/,          driv5630
     4  10X,'DAY OF THE YEAR =',T35,I6)                                 driv5640
      IF (ISOURC.EQ.0) WRITE (IPR,1535)                                 driv5650
1535  FORMAT('0 EXTRATERRESTIAL SOURCE IS THE SUN')                     driv5660
      IF (ISOURC.EQ.1) WRITE (IPR,1536) ANGLEM                          driv5670
1536  FORMAT('0 EXTRATERRESTIAL SOURCE IS THE MOON, MOON PHASE ANGLE =',driv5680
     1  F10.2,' DEG')                                                   driv5690
      IF (IPH.EQ.0) WRITE (IPR,1538) G                                  driv5700
1538  FORMAT('O H-G PHASE FUNCTION ,G=',F10.3)                          driv5710
      IF (IPH.EQ.1) WRITE (IPR,1540)                                    driv5720
1540  FORMAT('0 USER SUPPLIED PHASE FUNCTION')                          driv5730
      IF (IPH.EQ.2) WRITE (IPR,1542)                                    driv5740
1542  FORMAT('0 PHASE FUNCTION FROM MIE DATA BASE')                     driv5750
550   CONTINUE                                                          driv5760
C     V1 =FLOAT(INT(V1/5.0+0.1))*5.0                                    driv5770
C     V2 =FLOAT(INT(V2/5.0+0.1))*5.0                                    driv5780
C     TO AVOID THE DIFFICULTY FOR V1=0                                  driv5790
      ALAM1= 99999.98                                                   driv5800
C     IF(V1.GT.0.)ALAM1=10000./V1                                       driv5810
C     ALAM2=10000./V2                                                   driv5820
C     IF(DV.LT.5.)DV=5.                                                 driv5830
C     DV=FLOAT(INT(DV/5+0.1))*5.0                                       driv5840
C     WRITE (IPR,1555) V1,ALAM1,V2,ALAM2,DV                             driv5850
C1555 FORMAT('0 FREQUENCY RANGE '/,10X,' V1 = ',F12.1,' CM-1  (',       driv5860
C    1  F10.2,' MICROMETERS)',/,10X,' V2 = ',F12.1,' CM-1  (',F10.2,    driv5870
C    2  ' MICROMETERS)',/10X,' DV = ',F12.1,' CM-1')                    driv5880
      IF(.NOT.MODTRN)THEN                                               driv5890
          IV1=5*(IV1/5)                                                 driv5900
          IV2=5*((IV2+4)/5)                                             driv5910
          IDV=5+5*((IDV-5)/5)                                           driv5920
      ENDIF                                                             driv5930
cc    IF(IV1.GE.22681)modtrn=.false.      
      IF(iv1.ge.22681)THEN                                              driv5890
          IV1=5*(IV1/5)                                                 driv5900
          IV2=5*((IV2+4)/5)                                             driv5910
          IDV=5+5*((IDV-5)/5)                                           driv5920
          print*,' idv reset beyond modtran band model'
      ENDIF                                                             driv5930
      IF(IV2.LT.IV1+IDV)THEN                                            driv5940
          WRITE(IPR,'(/41H IV2 WAS LESS THAN IV1 + IDV AND HAS BEEN,    driv5950
     1      6H RESET,/)')                                               driv5960
          IV2=IV1+IDV                                                   driv5970
      ENDIF                                                             driv5980
      IF(MODTRN)THEN                                                    driv5990
          IV1SAV=IV1                                                    driv6000
          IV2SAV=IV2                                                    driv6010
          IDVSAV=IDV                                                    driv6020
      ENDIF                                                             driv6030
      IF(IV1.NE.0)ALAM1=10000./IV1                                      driv6040
      ALAM2=10000./IV2                                                  driv6050
      IF(IFWHM.LT.1)IFWHM=1                                             driv6060
      IF(IFWHM.GT.50)IFWHM=50                                           driv6070
      WRITE(IPR,'(17H0 FREQUENCY RANGE,/10X,8H   IV1 =,I10,8H CM-1  (,  driv6080
     1  F10.2,13H MICROMETERS),/10X,8H   IV2 =,I10,8H CM-1  (,F10.2,    driv6090
     2  13H MICROMETERS),/10X,8H   IDV =,I10,5H CM-1,/10X,8H IFWHM =,   driv6100
     3  I10,5H CM-1)')IV1,ALAM1,IV2,ALAM2,IDV,IFWHM                     driv6110
C                                                                       driv6120
C*****LOAD ATMOSPHERIC PROFILE INTO /MODEL/                             driv6130
C                                                                       driv6140
      call xprofl
      CALL STDMDL                                                       driv6150
C                                                                       driv6160
C    DEFINE COUNTER ITEST TO PREVENT ZENITH ANGLE QTHETA AND LAYER      driv6170
C    PATH LENGTH PL FROM BEING CHANGED DURING SOLAR CALCULATIONS        driv6180
555   continue 
      TBOUND =  tbous
      DO 15 I=1,laythr                                                  driv6190
          DO 16 J=1,KMAX+2                                              driv6200
              WPATH(I,J)=0.0                                            driv6210
              WPATHS(I,J)=0.0                                           driv6220
16    continue
15    continue
C                                                                       driv6230
      ITEST=0                                                           driv6240
C                                                                       driv6250
      IF (IMULT .EQ. 1) THEN                                            driv6260
          H1=ZM(1)                                                      driv6270
          H2=ZM(ML)                                                     driv6280
          ITYPE = 2                                                     driv6290
          ANGLE = 0.                                                    driv6300
          BETA = 0.                                                     driv6310
          RANGE =0.                                                     driv6320
          ISSGS = ISSGEO                                                driv6330
          ISSGEO = 0                                                    driv6340
C         CALL GEO (IERROR,BENDNG,MAXGEO)                               driv6350
          MSOFF=laytwo                                                  driv6360
          CALL GEO (IERROR,BENDNG,MAXGEO,MSOFF)                         driv6370
          W15SV = W(15)                                                 driv6380
C                                                                       driv6390
C         W15SV  IS THE REL HUM FROM 0 TO SPACE                         driv6400
C         THIS REL HUM MAY BE DIFFERENT THAN THE PATH  REL HUM          driv6410
C         WHEN REL HUM ARE DIFFERENT THE ANSWER CAN CHANGE              driv6420
C                                                                       driv6430
          ISSGEO = ISSGS                                                driv6440
          IMSMX=IKMAX                                                   driv6450
          DO 35 N=1,IMSMX                                               driv6460
C             PLST(N)=PL(N)                                             driv6470
C             DO 35 K=1,KMAX                                            driv6480
C35       WPMS(N,K)=WPATH(N,K)                                          driv6490
   35     PLST(N)=PL(N)                                                 driv6500
C                                                                       driv6510
          IF(IEMSCT.EQ.2)  THEN                                         driv6520
              CALL SSGEO(IERROR,IPH,IPARM,PARM1,PARM2,                  driv6530
C    1          PARM3,PARM4,PSIPO,G,MAXGEO)                             driv6540
     1          PARM3,PARM4,PSIPO,G,MAXGEO,MSOFF)                       driv6550
              DO 30 N=1,IKMAX                                           driv6560
                  CSENSV(N) = ABS(CSZEN(N))                             driv6570
                  IF(CSENSV(N) .LT. 0.0174) CSENSV(N) = 0.0174          driv6580
   30         CONTINUE                                                  driv6590
C             DO 45 N=1,ML                                              driv6600
C                 DO 45 K=1,KMAX                                        driv6610
C                     WPMSS(N,K)=WPATHS(N,K)                            driv6620
C  45         CONTINUE                                                  driv6630
          ENDIF                                                         driv6640
      ENDIF                                                             driv6650
      H1     = H1S                                                      driv6660
      H2     = H2S                                                      driv6670
      ANGLE  = ANGLES                                                   driv6680
      RANGE  = RANGS                                                    driv6690
      BETA   = BETAS                                                    driv6700
      ITYPE  = ITYPES                                                   driv6710
      LEN    = LENS                                                     driv6720

cjd3 v Define uang here rather than after reading card3.
c
c****************************************
c          saving the angle for DISORT-NORTH
             uang=angle
c*****************************************
c
cjd3  


C*****TRACE PATH THROUGH THE ATMOSPHERE AND CALCULATE ABSORBER AMOUNTS  driv6730
C                                                                       driv6740
      ISSGEO=0                                                          driv6750
C     RANGE=RN0                                                         driv6760
C     CALL GEO (IERROR,BENDNG,MAXGEO)                                   driv6770
      MSOFF=0                                                           driv6780
      CALL GEO(IERROR,BENDNG,MAXGEO,MSOFF)                              driv6790
      CALL AERTMP                                                       driv6800
      IF(IMULT. NE. 1) W15SV = W(15)                                    driv6810
C                                                                       driv6820
C   SAVE TEMPERATURE AND PATH INFO FOR LATER USE                        driv6830
C                                                                       driv6840
      IF(IMULT .EQ. 1)  THEN                                            driv6850
          DO 25 N=1,IKMAX                                               driv6860
   25     QTHETS(N) = QTHETA(N)                                         driv6870
      ENDIF                                                             driv6880
C                                                                       driv6890
      IF(IERROR.GT.0) GO TO 630                                         driv6900
      IF(IEMSCT.EQ.3 .AND. IERROR.EQ. -5) GO TO 557                     driv6910
      GO TO 558                                                         driv6920
  557 CONTINUE                                                          driv6930
      WRITE(IPR,1557)                                                   driv6940
 1557 FORMAT('0 DIRECT PATH TO SUN INTERSECTS THE EARTH: SKIP TO ',     driv6950
     1    'NEXT CASE')                                                  driv6960
      GO TO 630                                                         driv6970
  558 CONTINUE                                                          driv6980
C                                                                       driv6990
      IF(IEMSCT.EQ.2)CALL SSGEO(IERROR,IPH,IPARM,PARM1,PARM2,PARM3,     driv7000
C    1  PARM4,PSIPO,G,MAXGEO)                                           driv7010
     1  PARM4,PSIPO,G,MAXGEO,MSOFF)                                     driv7020
      W(15) = W15SV                                                     driv7030
C                                                                       driv7040
C     W15SV IS THE REL HUM  (FOR MULT SCAT THIS MAY BE DIFFERENT        driv7050
C     FROM PATH REL HUM)                                                driv7060
C                                                                       driv7070
C     THE SECOND CALL TO SSGEO IS TO GET THE CORRECT ANGLES FOR         driv7080
C     PHASE FUNCTIONS                                                   driv7090
C                                                                       driv7100
C     SAVE SOLAR PATH INFORMATION                                       driv7110
C                                                                       driv7120
      IF(IERROR.GT.0) GO TO 630                                         driv7130
C                                                                       driv7140
      IF(IMULT.EQ.1) THEN                                               driv7150
          DO 60 IK = 1,IMSMX                                            driv7160
              PL(IK)=PLST(IK)                                           driv7180
              IF(IEMSCT.EQ.2) CSZEN(IK)=CSENSV(IK)                      driv7190
60        CONTINUE                                                      driv7200
              DO 70 IK = 1,IKMAX
70            QTHETA(IK) = QTHETS(IK)                                   driv7170
      ENDIF                                                             driv7210
C                                                                       driv7220
C*****LOAD AEROSOL EXTINCTION, ABSORPTION, AND ASYMMETRY COEFFICIENTS   driv7230
C                                                                       driv7240
      CALL EXABIN                                                       driv7250
C                                                                       driv7260
C*****WRITE HEADER DATA TO TAPE 7                                       driv7270
C                                                                       driv7280
C560  WRITE(IPU,1110)MODEL,ITYPE,IEMSCT,IMULT,M1,M2,M3,                 driv7290
C    1  M4,M5,M6,MDEF,IM,NOPRT,TBOUND,SALB                              driv7300
  560 continue
c      WRITE(IPU,'(L1,I4,12I5,F8.3,F7.2)')MODTRN,MODEL                   driv7310
c     1  ,ITYPE,IEMSCT,IMULT,M1,M2,M3,M4,M5,M6,MDEF,IM,NOPRT,TBOUND,SALB driv7320
cC     WRITE(IPR1,1110)MODEL,ITYPE,IEMSCT,IMULT,M1,M2,M3,                driv7330
cC    1  M4,M5,M6,MDEF,IM,NOPRT,TBOUND,SALB                              driv7340
c      WRITE(IPR1,'(L1,I4,12I5,F8.3,F7.2)')MODTRN,MODEL                  driv7350
c     1  ,ITYPE,IEMSCT,IMULT,M1,M2,M3,M4,M5,M6,MDEF,IM,NOPRT,TBOUND,SALB driv7360
c      WRITE(IPU,1200)IHAZE,ISEASN,IVULCN,ICSTL,ICLD,IVSA,VIS,WSS,WHH,   driv7370
c     1  RAINRT,GNDALT                                                   driv7380
c      WRITE(IPR1,1200)IHAZE,ISEASN,IVULCN,ICSTL,ICLD,IVSA,VIS,WSS,WHH,  driv7390
c     1  RAINRT,GNDALT                                                   driv7400
c      WRITE(IPU,1210) CTHIK,CALT,CEXT,ISEED                             driv7410
c      WRITE(IPR1,1210) CTHIK,CALT,CEXT,ISEED                            driv7420
c      WRITE(IPU,1230)ZCVSA,ZTVSA,ZINVSA                                 driv7430
c      WRITE(IPR1,1230)ZCVSA,ZTVSA,ZINVSA                                driv7440
c      WRITE(IPU,1255) ML,(HMODEL(I,7),I=1,5)                            driv7450
c      WRITE(IPR1,1255) ML,(HMODEL(I,7),I=1,5)                           driv7460
1255  FORMAT( I5,18A4)                                                  driv7470
c      IF(MODEL.NE.0)WRITE (IPU,1312) H1,H2,ANGLE,RANGE,BETA,RO,LEN      driv7480
c      IF(MODEL.NE.0)WRITE (IPR1,1312) H1,H2,ANGLE,RANGE,BETA,RO,LEN     driv7490
      HMDLZ(8) = RANGE                                                  driv7500
c      IF(MODEL.EQ.0) WRITE(IPU,1560)(HMDLZ(K),K=1,8)                    driv7510
c      IF(MODEL.EQ.0) WRITE(IPR1,1560)(HMDLZ(K),K=1,8)                   driv7520
1560  FORMAT(3F10.3,5E10.3)                                             driv7530
c      WRITE(IPU,1320)  IPARM,IPH,IDAY,ISOURC                            driv7540
c      WRITE(IPR1,1320) IPARM,IPH,IDAY,ISOURC                            driv7550
c      WRITE(IPU,1322) PARM1,PARM2,PARM3,PARM4,TIME,PSIPO,ANGLEM,G       driv7560
c      WRITE(IPR1,1322)PARM1,PARM2,PARM3,PARM4,TIME,PSIPO,ANGLEM,G       driv7570
cC     WRITE(IPU,1400) V1,V2,DV                                          driv7580
cC     WRITE(IPR1,1400)V1,V2,DV                                          driv7590
c      WRITE(IPU,'(4I10)')IV1,IV2,IDV,IFWHM                              driv7600
c      WRITE(IPR1,'(4I10)')IV1,IV2,IDV,IFWHM                             driv7610
C                                                                       driv7620
      IRAIN=0                                                           driv7630
      IF(RAINRT.GT.0) IRAIN=1                                           driv7640
CCC                                                                     driv7650
CCC   CALCULATE EQUIVALENT LIQUID WATER CONSTANTS                       driv7660
CCC                                                                     driv7670
      CALL EQULWC                                                       driv7680
cc    IF(IRPT .EQ. -4) GO TO 565                                        driv7690
      READ(IRD,1600) IRPT                                               driv7700
      rewind 28
cc    IRPTS = IRPT                                                      driv7710
1600  FORMAT(I5)                                                        driv7720
565   continue
c      WRITE(IPU,1600) IRPT                                              driv7730
c      WRITE(IPR1,1600)IRPT                                              driv7740
C                                                                       driv7750
      ground=.false.                                                    driv7760
      if(h2.le.zm(1))ground=.true.                                      driv7770
cjd3 v the call to trans changed
c
cj      CALL TRANS(IPH,ISOURC,IDAY,ANGLEM,IRPT,ground,lsalb)              driv7780
c
      call trans(dis,nstr,uang,iph,isourc,iday,anglem,irpt,ground,
     x lsalb)
c
cjd3 ^
C                                                                       driv7790
C     TRANS RETURNS IRPT = -4 IF THE SPECTRAL RANGE EXTENDS BEYOND THE  driv7800
C     BAND MODEL TAPE.  IN THIS CASE, A LOWTRAN 7 CALCULATION IS        driv7810
C     PERFORMED FOR THE SHORT WAVELENGTHS AND THEN THE ORIGINAL INPUT   driv7820
C     IS RESTORED.                                                      driv7830
C                                                                       driv7840
C                                                                       driv8040
C*****WRITE END OF FILE ON TAPE 7                                       driv8050
630   IF(IERROR .GT. 0) THEN                                            driv8060
          READ(IRD,1600,END=900) IRPT                                   driv8070
c          WRITE(IPU,1600) IRPT                                          driv8080
c          WRITE(IPR1,1600)IRPT                                          driv8090
      ENDIF                                                             driv8100
c      WRITE(IPU,1620)                                                   driv8110
c      WRITE(IPR1,1620)                                                  driv8120
1620  FORMAT(' -9999.')                                                 driv8130
C                                                                       driv8140
      WRITE(IPR,1630)IRPT                                               driv8150
1630  FORMAT('0 CARD 5 *****',I5)                                       driv8160
      IF (IRPT.EQ.0) GO TO 900                                          driv8170
      IF (IRPT.EQ.4) GO TO 400                                          driv8180
cssi  IF (IRPT.GT.1 .AND. IEMSCT.EQ.3) THEN                             driv8190
cssi  PRINT*,'/!! ERROR IN INPUT IEMSCT EQ 3 IRPT GT 1!'                driv8200
cssi          STOP                                                      driv8210
cssi  ENDIF                                                             driv8220
      salb = salbs
      IF (IRPT.GT.4) GO TO 900                                          driv8230
      GO TO (100,900,300,400), IRPT                                     driv8240
  900 STOP                                                              driv8250
      END                                                               driv8260
      SUBROUTINE SMTH(N,ARRAY,NTOT,MODE,ARRAY2)
C
C       MODE=1 - FLAT FILTER
C       MODE=2 - TRIANGULAR FILTER
C
      DIMENSION ARRAY(NTOT),ARRAY2(NTOT)
C
      IF(N.LT.3)N=3 
44    FORMAT(10F10.2)
      DO 500 I=1,NTOT
      IF(ARRAY(I).GT.10000.)GO TO 501
      IF(ARRAY(I).LT.0.)GO TO 502
      GO TO 500
501   ARRAY(I)=10000.
C     WRITE(4, 503)I,ARRAY(I) 
      GO TO 500
502   CONTINUE
C     WRITE(4, 503)I,ARRAY(I) 
      IF(I.GT.1)ARRAY(I)=ARRAY(I-1)
503   FORMAT(' HELP',I5,F12.2)
500    CONTINUE
      NX=N/2
      NX=NX*2
      IF(NX.EQ.N)N=N+1
      NS=N/2
      JFIRST=NS+1
      JLAST=NTOT-NS-1
      DO 10 J=JFIRST,JLAST
      SUM=ARRAY(J)
      DO 20 K=1,NS
      JK=J+K
      KJ=J-K
      FAC=1.
      RN=NS+1-K
      RN2=NS+1
      IF(MODE.EQ.2)FAC=RN/RN2 
      SUM=SUM+FAC*(ARRAY(JK)+ARRAY(KJ)) 
20    CONTINUE
      DIV=N
      IF(MODE.EQ.2)DIV=NS+1
      SUM=SUM/DIV
      ARRAY2(J)=SUM 
10    CONTINUE
      NSS=NS+1
      DO 40 J=1,NSS 
      ARRAY2(J)=ARRAY2(JFIRST)
      JJ=NTOT-J+1
      ARRAY2(JJ)=ARRAY2(JLAST)
40    CONTINUE
      RETURN
      END 
      FUNCTION   SUN(V)                                                 sun  100
C                                                                       sun  110
C        EVALUATES THE EXTRA-TERRESTRIAL SOLAR IRRADIANCE               sun  120
C                                                                       sun  130
C        INPUT:  V  =  FREQUENCY   (CM-1)                               sun  140
C                VALID RANGE   0 TO 57490 (CM-1)                        sun  150
C                    (EQUIVALENT TO WAVELENGTHS > 0.174 MICROMETERS)    sun  160
C                                                                       sun  170
C        OUTPUT:  SUN  =  SOLAR IRRADIANCE  (WATTS M-2 MICROMETER-1)    sun  180
C                                                                       sun  190
C        WRITES A WARNING MESSAGE TO TAPE6  &  RETURNS  SUN = 0         sun  200
C            IF THE INPUT FREQUENCY IS OUT OF RANGE                     sun  210
C                                                                       sun  220
C       USES  BLOCK DATA SOLAR   WHICH CONTAINS THE VALUES FOR SOLARA + sun  230
C                                                                       sun  240
      COMMON /SUNDAT/ SOLARA(1403), SOLARB(2951)                        sun  250
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      logical sun1
      common /solar1/sun1,asun(50000),asun2(50000)
      DATA  A, B / 3.50187E-13, 3.93281 /                               sun  270
C                                                                       sun  280
C       WM, W0, W1, W2  ARE STATEMENT FUNCTIONS USED BY                 sun  290
C            THE 4 POINT LAGRANGE INTERPOLATION                         sun  300
      WM(P) = P*(P - 1)*(P - 2)                                         sun  310
      W0(P) = 3*(P**2 - 1)*(P - 2)                                      sun  320
      W1(P) = 3*P*(P + 1)*(P - 2)                                       sun  330
      W2(P) = P*(P**2 - 1)                                              sun  340
C                                                                       sun  350
C           IF  V  IS TOO SMALL,  WRITE WARNING  +  RETURN SUN = 0      sun  360
      IF(V .LT. 0.0) THEN                                               sun  370
        SUN = 0.0                                                       sun  380
c        WRITE(ipu, 900) V                                               sun  390
        go to 300                                                       sun  400
C                                                                       sun  410
      ELSEIF( V .GE. 0.0  .AND.  V .LT. 100.0 ) THEN                    sun  420
C         FOR LOW FREQUENCIES USE A POWER LAW APPROXIMATION             sun  430
        SUN = A*V**B                                                    sun  440
        go to 300                                                       sun  450
C                                                                       sun  460
      ELSEIF( V .GE. 100.0  .AND.  V .LT. 28000.0 ) THEN                sun  470
C         USE  4 POINT INTERPOLATION  ON  ARRAY  SOLARA                 sun  480
C               WHICH IS AT  20 CM-1  SPACING  FROM 0 TO 28000 CM-1     sun  490
        I = 1 + INT(V/20.0)                                             sun  500
        P = MOD(V, 20.0)/20.0                                           sun  510
        SUN = ( W2(P)*SOLARA(I+2) - W1(P)*SOLARA(I+1) +                 sun  520
     +              W0(P)*SOLARA(I) - WM(P)*SOLARA(I-1) ) / 6           sun  530
        go to 300                                                          sun  540
C                                                                       sun  550
      ELSEIF( V .GE. 28000.0  .AND.  V .LE. 57470.0 ) THEN              sun  560
C         USE  4 POINT INTERPOLATION  ON  ARRAY  SOLARB                 sun  570
C             WHICH IS AT  10 CM-1  SPACING  FROM 28400 TO 57490 CM-1   sun  580
        I = INT(V/10.0) - 2800  + 2                                     sun  590
        P = MOD(V, 10.0)/10.0                                           sun  600
        SUN = ( W2(P)*SOLARB(I+2) - W1(P)*SOLARB(I+1) +                 sun  610
     +              W0(P)*SOLARB(I) - WM(P)*SOLARB(I-1) ) / 6           sun  620
        go to 300                                                       sun  630
C                                                                       sun  640
      ELSEIF( V .GT. 57470.0 ) THEN                                     sun  650
C           IF  V  IS TOO LARGE,  WRITE WARNING  +  RETURN SUN = 0      sun  660
        SUN = 0.0                                                       sun  670
c        WRITE(ipu, 900) V                                               sun  680
        go to 300                                                       sun  690
C                                                                       sun  700
      ENDIF                                                             sun  710
C                                                                       sun  720
300   if(sun1)then
      if(v.lt.100.)   go to 400
      if(v.gt.49900.) go to 400
      ivi = v
      sun = asun2(ivi)
400   continue
      endif
      RETURN                                                            sun  730
  900 FORMAT('0 *****  WARNING - INPUT FREQUENCY = ', 1PG12.5, 'CM-1',  sun  740
     +  /, '   OUTSIDE VALID RANGE OF 0 TO 57470 CM-1    *******', / )  sun  750
      END                                                               sun  760
      SUBROUTINE FLXADD(iv,ikmx,S0)                                     flxa 100
C                                                                       flxa 110
C     (1) CALCULATES UPWARD, F+ (UPF), AND DOWNWARD, F- (DNF),          flxa 120
C     FLUX PROFILES USING ADDING METHOD                                 flxa 130
C     (2) MS SOURCE FUNCTION EVALUATED FROM LEVEL FLUXES VIA            flxa 140
C     STREAM APPROXIMATION (SEE EQN     , ISAACS ET AL, 1986)           flxa 150
C     (3) BACKSCATTER FOR SOURCE FUNCTION FOR GIVEN G FACTOR            flxa 160
C     AND ZENTIH ANGLE OBTAINED USING FUNCTION BETABS                   flxa 170
C                                                                       flxa 180
C     A.E.R. 1986                                                       flxa 190
      DOUBLE PRECISION AER1,AER2,AERA,AERU,AERV,AERC,AERCX,EX1,EX2,     flxa 200
     $     DENO,DNMO,DNM1                                               flxa 210
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      include 'parameter.list'
      COMMON RELHUM(laydim),HSTOR(laydim),ICH(4),VH(17),TX(65),W(65)  
      COMMON IMSMX,WPATH(laythr,65),TBBY(laythr),PATM(laythr),NSPEC,   
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX2(47),AWCCON(5)  
      COMMON/TRAN/TMOLS(LAYDIM),TAERS(LAYDIM),TCONT(LAYDIM),
     1  DCONT(LAYTWO)
      COMMON/AABBCC/AA(11),BB(11),CC(11),IBND(11),A(11),CPS(11)         flxa 270
      logical modtrn                                                    flxa 280
      COMMON/CARD1/MODEL,ITYPE,IEMSCT,M1,M2,M3,IM,NOPRNT,TBOUND,SALB,   flxa 290
     $     MODTRN                                                       flxa 300
      COMMON /MODEL/ ZM(LAYDIM),PM(LAYDIM),TM(LAYDIM),RFNDX(LAYDIM),
     1  DENSTY(65,LAYDIM),CLDAMT(LAYDIM),RRAMT(LAYDIM),EQLWC(LAYDIM),
     1  HAZEC(LAYDIM)
      COMMON/CNTRL/KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT                 flxa 330
      COMMON/SOLS/AH1(LAYTWO),ARH(LAYTWO),WPATHS(LAYTHR,65),
     1 PA(LAYTWO),PR(LAYTWO),ATHETA(LAYDIM+1),ADBETA(LAYDIM+1),
     2 LJ(LAYTWO+1),JTURN,ANGSUN,CSZEN(LAYTWO),TBBYS(LAYTHR,12),
     3 PATMS(LAYTHR,12)
      COMMON /PATH/PL(LAYTWO),QTHETA(LAYTWO),ITEST,HI,HF,
     1  AHT(LAYTWO),tph(LAYTWO)
      COMMON/CNSTNS/PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                       flxa 380
      COMMON/DIRST/SUN,UMB(laydim),DMB(laydim),UMBS(laydim),
     x DMBS(laydim)    
      COMMON/MSRD/TLE(LAYDIM),COSBAR(LAYDIM),OMEGA0(LAYTWO),
     1  UPF(10,LAYDIM),DNF(10,LAYDIM),TAER(LAYDIM),ASYIK(LAYTWO),
     2  ASYDM(LAYTWO),STRN(0:LAYDIM),DMOLS(LAYTWO),DSTRN(0:LAYTWO),
     3  FDNSRT,FDNTRT,TAUT(LAYDIM),UMF(LAYDIM),DMF(LAYDIM),
     4  UMFS(LAYDIM),DMFS(LAYDIM) 
      common/flux/btop(LAYDIM),EDN(LAYDIM),EUP(LAYDIM),EUPC(LAYDIM),
     1  TDF(LAYDIM),RUPC(LAYDIM),REF(LAYDIM),EDNS(LAYDIM),EUPS(LAYDIM),
     2  EUPCS(LAYDIM),TDFS(LAYDIM),RUPCS(LAYDIM),REFS(LAYDIM),
     3  UPFS(3,LAYDIM),DNFS(3,LAYDIM)  
      common/work/TAUM(3,laydim),TWGP(3,laydim),OMEGAK(laydim),
     $     DPJ(3,laydim),GKWJ(3,11),DPWJ(3,11),CP1S(11),dumdum(4066)
C                                                                       flxa 490
C     INPUT PARAMETERS:                                                 flxa 500
C     ----------------                                                  flxa 510
C     TLE(N)     - TEMPERATURE (KELVIN) OF UPPER EDGE OF LAYER N        flxa 520
C     COSBAR(N)  - ASYMMETRY FACTOR FOR LAYER N                         flxa 530
C     TAER(N)    - TOTAL AEROSOL OPTICAL THICKNESS OF LAYER N           flxa 540
C     TAERS(N)   - AEROSOL SCATTERING OPTICAL THICKNESS OF LAYER N      flxa 550
C     TCONT(N)   - MOLECULAR CONTINUUM OPTICAL THICKNESS FOR LAYER N    flxa 560
C     TMOLS(N)   - RAYLEIGH SCATTERING OPTICAL THICKNESS FOR LAYER N    flxa 570
C     CSZEN(N)   - COSINE OF SOLAR ZENITH ANGLE FOR LAYER N             flxa 580
C     IV         - FREQUENCY (WAVENUMBER)                               flxa 590
C     S0         - SOLAR INTENSITY AT TOP OF ATMOSPHERE                 flxa 600
C     H1         - ALTITUDE OF OBSERVER (KM)                            flxa 610
C     H2         - ALTITUDE OF TARGET (KM)                              flxa 620
C     STRN(N)    - SOLAR TRANSMISSION TO BOTTOM OF LAYER N              flxa 630
C                                                                       flxa 640
C     OUTPUT PARAMETERS:                                                flxa 650
C     -----------------                                                 flxa 660
C     OMEGAK(N)  - SINGLE SCATTERING ALBEDO OF LAYER N FOR VALUE OF K   flxa 670
C     TAUM(K,N)  - MOLECULAR OPTICAL THICKNESS OF LAYER N FOR VALUE OF Kflxa 680
C     BTOP(N)    - BLACK BODY FLUX FOR UPPER EDGE TEMPERATURE OF LAYER Nflxa 690
C     BMID(N)    - BLACK BODY FLUX FOR AVERAGE TEMPERATURE OF LAYER N   flxa 700
C                                                                       flxa 710
C     INTERNAL PARAMETERS:                                              flxa 720
C     -------------------                                               flxa 730
C     TWGP(K,N)  - SUM OF OPTICAL DEPTH * PROBABILITY BY MOLECULE       flxa 740
C     UPF(K,N)   - UPWARD THERMAL FLUX AT UPPER EDGE OF LAYER N FOR     flxa 750
C     VALUE OF K                                                        flxa 760
C     UPFS(K,N)  - UPWARD SOLAR FLUX AT UPPER EDGE OF LAYER N FOR       flxa 770
C     VALUE OF K                                                        flxa 780
C     DNF(K,N)   - DOWNWARD THERMAL FLUX AT UPPER EDGE OF LAYER N FOR   flxa 790
C     VALUE OF K                                                        flxa 800
C     DNFS(K,N)  - DOWNWARD SOLAR FLUX AT UPPER EDGE OF LAYER N FOR     flxa 810
C     VALUE OF K                                                        flxa 820
C     UMF(N)     - MEAN UPWARD THERMAL FLUX OF LAYER N FOR VALUE OF K   flxa 830
C     UMFS(N)    - MEAN UPWARD SOLAR FLUX OF LAYER N FOR VALUE OF K     flxa 840
C     DMF(N)     - MEAN DOWNWARD THERMAL FLUX OF LAYER N FOR VALUE OF K flxa 850
C     DMFS(N)    - MEAN DOWNWARD SOLAR FLUX OF LAYER N FOR VALUE OF K   flxa 860
C     SU(K,N)    - UPWARD THERMAL RADIANCE SOURCE FUNCTION FOR N,K      flxa 870
C     SUS(K,N)   - UPWARD SOLAR RADIANCE SOURCE FUNCTION FOR N,K        flxa 880
C     SD(K,N)    - DOWNWARD THERMAL RADIANCE SOURCE FUNCTION FOR N,K    flxa 890
C     SDS(K,N)   - DOWNWARD SOLAR RADIANCE SOURCE FUNCTION FOR N,K      flxa 900
C     b0         - BACKSCATTER FRACTION ALONG SOLAR PATH AT LAYER N     flxa 910
C                                                                       flxa 920
C     NEW VARABLES          11 MOLECULES  BY JOSEPH H PIERLUISSI        flxa 930
C     DPWJ  PROBABILITY FOR EACH MOLECULE  FIT DOUBLE EXPONENTIAL       flxa 940
C     GKWJ  BAND DEPENDENT  SCALING OF DENSITIES TO GET K AMOUNT        flxa 950
C     CPS IS THE STORED VALUES OF PIERLUISSI BAND MODEL COEFFIECNTS     flxa 960
C     CP1S=10**CPS                                                      flxa 970
C     IBND MAPS BANDS TO MOLECULES                                      flxa 980
C     DPJ REPLACES DPO  IS THE  EFFECTIVE PROBILITY BY LAYER            flxa 990
C     TAUM IS DEFINED AS THE SUM OF THE OPTICAL DEPTHS BY MOLECULE      flxa1000
      DPC=1./3.                                                         flxa1010
      V=FLOAT(iv)                                                       flxa1020
      ng=ml                                                             flxa1030
      NLAYRS=ng-1                                                       flxa1040
      DO 15 MOL=1,11                                                    flxa1050
         CP1S(Mol)=10.**CPS(Mol)                                        flxa1060
         IW=IBND(MOL)                                                   flxa1070
         if(iw.le.0)then                                                flxa1080
            DO 10 K=1,3                                                 flxa1090
               GKWJ(K,MOL)=0.                                           flxa1100
 10         DPWJ(K,MOL)=0.                                              flxa1110
         else                                                           flxa1120
            GKWJ(1,MOL)=CC(MOL)                                         flxa1130
            GKWJ(2,MOL)=.09*CC(MOL)                                     flxa1140
            GKWJ(3,MOL)=.015*CC(MOL)                                    flxa1150
            DPWJ(1,MOL)=AA(MOL)                                         flxa1160
            DPWJ(2,MOL)=BB(MOL)                                         flxa1170
            DPWJ(3,MOL)=1.-AA(MOL)-BB(MOL)                              flxa1180
         endif                                                          flxa1190
 15   CONTINUE                                                          flxa1200
C                                                                       flxa1210
C     ABSORPTION COEFFICIENTS FOR WATER VAPOR, UNIFORMLY MIXED GASES    flxa1220
C     AND OZONE.  EVALUATE THE WEIGHTED K DISTRIBUTION QUANTITIES FOR   flxa1230
C     WATER VAPOR AND THE UNIFORMLY MIXED GASES.                        flxa1240
C                                                                       flxa1250
C     Simultaneously, BLACK BODY FLUX FOR LAYER EDGE TEMPERATURE        flxa1260
      BTOP(Ng)=pi*BBFN(TLE(Ng),V)                                       flxa1270
      DO 40 N=1,NLAYRS                                                  flxa1280
         BTOP(N)=pi*BBFN(TLE(N),V)                                      flxa1290
         smdpj=0.                                                       flxa1300
         DO 30 K=1,3                                                    flxa1310
            TAUM(K,N)=0.                                                flxa1320
            TWGP(K,N)=0.                                                flxa1330
            DO 20 MOL=1,11                                              flxa1340
               IB=IBND(MOL)                                             flxa1350
               IF(IB.LT.0)goto20                                        flxa1360
               W(IB)=DENSTY(IB,ikmx-N)*PL(ikmx-N)*GKWJ(K,MOL)           flxa1370
               TAUM(K,N)=TAUM(K,N)+W(IB)*CP1S(MOL)                      flxa1380
               TWGP(K,N)=TWGP(K,N)+W(IB)*CP1S(MOL)*DPWJ(K,MOL)          flxa1390
 20         CONTINUE                                                    flxa1400
C                                                                       flxa1410
C           EFFECTIVE PROBABILITY BY LAYER DPJ IS BASED ON MOLECULAR    flxa1420
C           PROBABILITY WEIGHTED BY OPTICAL DEPTH                       flxa1430
            DPJ(K,N)=DPC                                                flxa1440
            IF(TAUM(K,N).NE.0)DPJ(K,N)=TWGP(K,N)/TAUM(K,N)              flxa1450
            smdpj=smdpj+dpj(k,n)                                        flxa1460
 30      continue                                                       flxa1470
         DO 41 K=1,3                                                    flxa1480
            DPJ(K,N)=DPJ(K,N)/SMDPJ                                     flxa1490
 41      continue                                                       flxa1500
 40   continue                                                          flxa1520
C                                                                       flxa1530
C     PROBABILITY INTEGRATION LOOP                                      flxa1540
      DO 80 K=1,3                                                       flxa1550
C                                                                       flxa1560
C        COMPOSITE DOWNWARDS REFLECTION                                 flxa1570
         RDNCN=0.                                                       flxa1580
C                                                                       flxa1590
C        ADDING GROUND LAYER                                            flxa1600
C        DEFINE INITIAL UPWARD COMPOSITE SURFACE REFLECTANCE            flxa1610
         RUPCN=SALB                                                     flxa1620
C                                                                       flxa1630
C        SURFACE EMISSION                                               flxa1640
         EUPCN=(1.-RUPCN)*BTOP(NG)                                      flxa1650
         EUP(NG)=EUPCN                                                  flxa1660
         EUPC(NG)=EUPCN                                                 flxa1670
         rupc(ng)=0.                                                    flxa1680
         IF(IEMSCT.EQ.2)THEN                                            flxa1690
            RDNCNS=0.                                                   flxa1700
            RUPCNS=SALB                                                 flxa1710
            EUPCNS=RUPCNS*CSZEN(nlayrs)*STRN(nlayrs)*S0                 flxa1720
            EUPS(NG)=EUPCNS                                             flxa1730
            EUPCS(NG)=EUPCNS                                            flxa1740
            rupcs(ng)=0.                                                flxa1750
         ENDIF                                                          flxa1760
C                                                                       flxa1790
C        UPWARD ADDING LOOP(1-N)                                        flxa1800
         DO 50 N=NLAYRS,1,-1                                            flxa1810
C                                                                       flxa1820
C           CALCULATE MOLECULAR OPTICAL THICKNESS OF EACH LAYER FOR     flxa1830
C           EACH K.  NOTE: TOP LAYER IS LAYER 1                         flxa1840
C                                                                       flxa1850
C           EUP AND EDN ARE UPWARD AND DOWNWARD THERMAL FLUX            flxa1860
C           FOR AN ISOLATED LAYER.  TDF AND REF ARE THE THERMAL         flxa1870
C           TRANSMISSION AND REFLECTANCE FOR AN ISOLATED LAYER          flxa1880
            IF(K.EQ.1)THEN                                              flxa1890
               TAUT(N)=TAUM(K,N)*DPJ(K,N)                               flxa1900
            ELSE                                                        flxa1910
               TAUT(N)=TAUM(K,N)*DPJ(K,N)+TAUT(N)                       flxa1920
               DENOM=(TAUT(N)+TAER(N)+TCONT(N)+TMOLS(N))                flxa1930
               IF(DENOM.LE.0.)DENOM=1.                                  flxa1940
               IF(K.EQ.3)OMEGA0(N)=(TAERS(N)+TMOLS(N))/denom            flxa1950
            ENDIF                                                       flxa1960
C                                                                       flxa1970
C           X IS THE PATH OPTICAL THICKNESS FOR A GIVEN LAYER OF        flxa1980
C           PATH LENGTH PL                                              flxa1990
            X=TAUM(K,N)+TAER(N)+TCONT(N)+TMOLS(N)                       flxa2000
            omegak(n)=1.                                                flxa2010
            IF(X.GT.0.)OMEGAK(N)=(TAERS(N)+TMOLS(N))/X                  flxa2020
            ikmaxn = ikmax-n
            X=X*(ZM(IKMAXN+1)-ZM(IKMAXN))/PL(IKMAXN)                    flxa2030
C                                                                       flxa2040
C           USE TWO STREAM APPROXIMATION FOR THERMAL                    flxa2050
            IF(OMEGAK(N).le..99999)then                                 flxa2060
               AER1=1.-OMEGAK(N)                                        flxa2070
               AER2=1.-OMEGAK(N)*COSBAR(N)                              flxa2080
               AERA=SQRT(AER1/AER2)                                     flxa2090
               AERU=(1.-AERA)/2.                                        flxa2100
               AERV=(1.+AERA)/2.                                        flxa2110
               AERC=SQRT(3.*AER1*AER2)                                  flxa2120
               AERCX=AERC*X                                             flxa2130
               IF(AERCX.LT.BIGEXP) THEN                                 flxa2140
                  EX1=EXP(-AERCX)                                       flxa2150
               ELSE                                                     flxa2160
                  EX1=1./BIGNUM                                         flxa2170
               ENDIF                                                    flxa2180
               EX2=EX1*EX1                                              flxa2190
               DENO=(AERV*AERV-AERU*AERU*EX2)                           flxa2200
               DNMO=(BTOP(N)-BTOP(N+1))                                 flxa2210
c                                                                       flxa2220
     $              /(X*AERC)*(AERV-AERU*EX2-AERA*EX1)                  flxa2230
               DNM1=AERV+AERU*EX2                                       flxa2240
               EUP(N)=(BTOP(N)*DNM1-DNMO-BTOP(N+1)*EX1)/DENO*AERA       flxa2250
               EDN(N)=(BTOP(N+1)*DNM1+DNMO-BTOP(N)*EX1)/DENO*AERA       flxa2260
               IF(EUP(N).LT.0.)EUP(N)=0.                                flxa2300
               IF(EDN(N).LT.0.)EDN(N)=0.                                flxa2310
               REF(N)=AERU*AERV*(1.0-EX2)/DENO                          flxa2320
               TDF(N)=(AERV-AERU)/DENO*EX1                              flxa2330
            else                                                        flxa2340
               TDF(N)=1./(1.+SQRT(3.)*(1.-COSBAR(N))/2.*X)              flxa2350
               REF(N)=1.-TDF(N)                                         flxa2360
               EUP(N)=0.                                                flxa2370
               EDN(N)=0.                                                flxa2380
            endif                                                       flxa2390
C                                                                       flxa2400
C           CALCULATE COMPOSITE FLUXES AND REFLECTANCES                 flxa2410
            DENO=1.-RUPCN*REF(N)                                        flxa2420
            EUPCN=EUP(N)+(EUPCN+EDN(N)*RUPCN)*TDF(N)/DENO               flxa2430
            RUPCN=REF(N)+TDF(N)*RUPCN*TDF(N)/DENO                       flxa2440
            EUPC(N)=EUPCN                                               flxa2450
            RUPC(N)=RUPCN                                               flxa2460
            IF(IEMSCT.EQ.2)THEN                                         flxa2470
C                                                                       flxa2480
C              CALCULATE PARAMETERS FOR SOLAR HYBRID MODIFIED           flxa2490
C              DELTA EDDINGTON 2-STREAM APPROXIMATION                   flxa2500
               SMU=CSZEN(N)                                             flxa2510
               smu2=smu*smu                                             flxa2520
               OME0=OMEGAK(N)                                           flxa2530
               IF(OME0.GE..999999)OME0=.999999                          flxa2540
C                                                                       flxa2550
C              OME0 TEST MAY BE MACHINE DEPENDENT.  RECODED AS BS FC    flxa2560
C              AND AS*EXP(SK*X) FOR IMPROVED ACCURACY IN SOLAR FLUX     flxa2570
C              CALCULATIONS FOR LARGE X(NON MACHINE DEPENDENT)          flxa2580
               G=COSBAR(N)                                              flxa2590
               g2=g*g                                                   flxa2600
               Q=4.*(1.-g2*(1.-SMU))                                    flxa2610
               b0=.5                                                    flxa2620
               if(g.ne.0.)B0=BETABS(smu,g)                              flxa2630
               GM1=(7.-3.*g2-OME0*(4.+3.*G)+OME0*g2*(4.*B0+3.*G))/Q     flxa2640
               GM2=-(1.-g2-OME0*(4.-3.*G)-OME0*g2*(4.*B0+3.*G-4.))/Q    flxa2650
               IF(GM2.EQ.0.)GM2=1.0E-10                                 flxa2660
               SK=(GM1-gm2)*(gm1+gm2)                                   flxa2670
               CS=OME0*(b0/SMU-GM1*b0-GM2*(1-b0))*(smu2/(1-SK*smu2))    flxa2680
               SK=SQRT(sk)                                              flxa2690
               YS=CS*(GM1+1./SMU)-OME0*b0                               flxa2700
C                                                                       flxa2710
C              CALCULATE UPWARD & DOWNWARD FLUX, EACH LAYER ALONE       flxa2720
               EKTL=SK*X                                                flxa2730
               IF(EKTL.LT.BIGEXP)THEN                                   flxa2740
                  EKT=EXP(-EKTL)                                        flxa2750
               ELSE                                                     flxa2760
                  EKT=1./BIGNUM                                         flxa2770
               ENDIF                                                    flxa2780
               EKT2=EKT*EKT                                             flxa2790
               ETMUL=X/SMU                                              flxa2800
               IF(ETMUL.LT.BIGEXP)THEN                                  flxa2810
                  ETMU=EXP(-ETMUL)                                      flxa2820
               ELSE                                                     flxa2830
                  ETMU=1./BIGNUM                                        flxa2840
               ENDIF                                                    flxa2850
               GMPSK=GM1+SK                                             flxa2860
               GMMSK=GM1-SK                                             flxa2870
               DENOM=(EKT2*GMMSK)-GMPSK                                 flxa2880
               ASXP=-GMMSK*(YS*EKT-GMPSK*CS*ETMU)/DENOM                 flxa2890
               ASNEW=-(-GMPSK*CS*ETMU*EKT+YS*EKT2)/DENOM                flxa2900
               BSNEW=(YS -(CS * ETMU * EKT*GMMSK))/DENOM                flxa2910
               EUPS(N)=ASNEW+BSNEW+CS                                   flxa2920
               if(eups(n).lt.0.)eups(n)=0.                              flxa2930
               EDNS(N)=(ASXP+BSNEW*(GMPSK)*EKT+YS*ETMU)/GM2             flxa2940
               if(edns(n).lt.0.)edns(n)=0.                              flxa2950
               FC=1./(CSZEN(N))                                         flxa2960
C                                                                       flxa2970
C              REFS & TDFS ARE REFLECT & TRANSMITT FOR INDIVIDUAL       flxa2980
C              LAYERS,SOLAR                                             flxa2990
               REFS(N)=EUPS(N)*FC                                       flxa3000
               XCSZEN=X/CSZEN(N)                                        flxa3010
               IF(XCSZEN.LT.BIGEXP)THEN                                 flxa3020
                  TDFS(N)=EDNS(N)*FC+EXP(-XCSZEN)                       flxa3030
               ELSE                                                     flxa3040
                  TDFS(N)=EDNS(N)*FC                                    flxa3050
               ENDIF                                                    flxa3060
               s0strn=s0*strn(n-1)                                      flxa3070
               EUPS(N)=EUPS(N)*S0STRN                                   flxa3080
               EDNS(N)=EDNS(N)*S0STRN                                   flxa3090
               DENOS=1.-RUPCNS*REFS(N)                                  flxa3100
               EUPCNS=EUPS(N)+(EUPCNS+EDNS(N)*RUPCNS)*TDFS(N)/DENOS     flxa3110
               RUPCNS=REFS(N)+TDFS(N)*RUPCNS*TDFS(N)/DENOS              flxa3120
               EUPCS(N)=EUPCNS                                          flxa3130
               RUPCS(N)=RUPCNS                                          flxa3140
            ENDIF                                                       flxa3150
 50      continue                                                       flxa3180
C                                                                       flxa3190
C        NOW ADD DOWNWARD FROM TOP LAYER (N=1)                          flxa3200
         EDNCN=0.                                                       flxa3210
         DNF(K,1)=EDNCN                                                 flxa3220
         UPF(K,1)=EUPC(1)                                               flxa3230
         RDNCN=0.                                                       flxa3240
         IF(IEMSCT.EQ.2)THEN                                            flxa3250
            EDNCNS=0.                                                   flxa3260
            DNFS(K,1)=EDNCNS                                            flxa3270
            UPFS(K,1)=EUPCS(1)                                          flxa3280
            RDNCNS=0.                                                   flxa3290
         ENDIF                                                          flxa3300
         DPJ(K,NG)=DPJ(K,NG-1)                                          flxa3310
         nm1=1                                                          flxa3320
         DO 60 N=2,NG                                                   flxa3330
            DENO=1.-RDNCN*REF(nm1)                                      flxa3340
            DENOS=1.-RDNCNS*REFS(nm1)                                   flxa3350
            EDNCN=EDN(nm1)+(EDNCN+EUP(nm1)*RDNCN)*TDF(nm1)/DENO         flxa3360
            RDNCN=REF(nm1)+TDF(nm1)*TDF(nm1)*RDNCN/DENO                 flxa3370
            PEFUP=(EUPC(N)+EDNCN*RUPC(N))/DENO                          flxa3400
            PEFDN=(EDNCN+EUPC(N)*RDNCN)/DENO                            flxa3410
            UPF(K,N)=PEFUP                                              flxa3420
            DNF(K,N)=PEFDN                                              flxa3430
            IF(IEMSCT.EQ.2)THEN                                         flxa3440
               EDNCNS=EDNS(nm1)                                         flxa3450
     $              +(EDNCNS+EUPS(nm1)*RDNCNS)*TDFS(nm1)/DENOS          flxa3460
               RDNCNS=REFS(nm1)+TDFS(nm1)*TDFS(nm1)*RDNCNS/DENOS        flxa3470
               PEFUPS=(EUPCS(N)+EDNCNS*RUPCS(N))/DENOS                  flxa3480
               PEFDNS=(EDNCNS+EUPCS(N)*RDNCNS)/DENOS                    flxa3490
               UPFS(K,N)=PEFUPS                                         flxa3500
               DNFS(K,N)=PEFDNS                                         flxa3510
            ENDIF                                                       flxa3520
            IF(K.EQ.1)THEN                                              flxa3530
               UMF(nm1)=.5*(UPF(K,nm1)+UPF(K,N))*DPJ(K,nm1)             flxa3540
               DMF(nm1)=.5*(DNF(K,nm1)+DNF(K,N))*DPJ(K,nm1)             flxa3550
               UMB(N)=UPF(K,N)*DPJ(K,N)                                 flxa3560
               DMB(nm1)=DNF(K,nm1)*DPJ(K,nm1)                           flxa3570
               if(iemsct.eq.2)then                                      flxa3580
                  UMFS(nm1)=.5*(UPFS(K,nm1)+UPFS(K,N))*DPJ(K,nm1)       flxa3590
                  DMFS(nm1)=.5*(DNFS(K,nm1)+DNFS(K,N))*DPJ(K,nm1)       flxa3600
                  UMBS(N)=UPFS(K,N)*DPJ(K,N)                            flxa3610
                  DMBS(nm1)=DNFS(K,nm1)*DPJ(K,nm1)                      flxa3620
               endif                                                    flxa3630
            ELSE                                                        flxa3640
               UMF(nm1)=UMF(nm1)+.5*(UPF(K,nm1)+UPF(K,N))*DPJ(K,nm1)    flxa3650
               DMF(nm1)=DMF(nm1)+.5*(DNF(K,nm1)+DNF(K,N))*DPJ(K,nm1)    flxa3660
               UMB(N)=UMB(N)+UPF(K,N)*DPJ(K,N)                          flxa3670
               DMB(nm1)=DMB(nm1)+DNF(K,nm1)*DPJ(K,nm1)                  flxa3680
               if(iemsct.eq.2)then                                      flxa3690
                  UMFS(nm1)=UMFS(nm1)                                   flxa3700
     $                 +.5*(UPFS(K,nm1)+UPFS(K,N))*DPJ(K,nm1)           flxa3710
                  DMFS(nm1)=DMFS(nm1)                                   flxa3720
     $                 +.5*(DNFS(K,nm1)+DNFS(K,N))*DPJ(K,nm1)           flxa3730
                  UMBS(N)=UMBS(N)+UPFS(K,N)*DPJ(K,N)                    flxa3740
                  DMBS(nm1)=DMBS(nm1)+DNFS(K,nm1)*DPJ(K,nm1)            flxa3750
               endif                                                    flxa3760
            ENDIF                                                       flxa3770
 60      nm1=n                                                          flxa3780
C                                                                       flxa3840
C        fdnsrt IS THE DOWNWARD SOLAR FLUX AT THE SURFACE               flxa3850
C        fdntrt IS THE DOWNWARD THERMAL FLUX AT THE SURFACE             flxa3860
         IF(K.EQ.1)THEN                                                 flxa3870
            fdntrt=DNF(K,NG)*DPJ(K,NLAYRS)                              flxa3880
            FUPTRF=UPF(K,1)*DPJ(K,1)                                    flxa3890
            if(iemsct.eq.2)then                                         flxa3900
               fdnsrt=DNFS(K,NG)*DPJ(K,NLAYRS)                          flxa3910
               FUPSRF=UPFS(K,1)*DPJ(K,1)                                flxa3920
            endif                                                       flxa3930
         ELSE                                                           flxa3940
            fdnsrt=fdnsrt+DNFS(K,NG)*DPJ(K,NLAYRS)                      flxa3950
            fdntrt=fdntrt+DNF(K,NG)*DPJ(K,NLAYRS)                       flxa3960
            if(iemsct.eq.2)then                                         flxa3970
               FUPSRF=FUPSRF+UPFS(K,1)*DPJ(K,1)                         flxa3980
               FUPTRF=FUPTRF+UPF(K,1)*DPJ(K,1)                          flxa3990
            endif                                                       flxa4000
         ENDIF                                                          flxa4010
 80   CONTINUE                                                          flxa4020
      UMB(1)=FUPTRF                                                     flxa4030
      DMB(NG)=fdntrt                                                    flxa4040
      if(iemsct.ne.2)return                                             flxa4050
      UMBS(1)=FUPSRF                                                    flxa4060
      DMBS(NG)=fdnsrt                                                   flxa4070
      SUN=S0                                                            flxa4080
      RETURN                                                            flxa4090
      END                                                               flxa4100
      SUBROUTINE TRANS(dis,nstr,uang,IPH,ISOURC,IDAY,ANGLEM,
     1  IRPT,ground,lsalb)
cjd3 v changing call line
cj      SUBROUTINE TRANS(IPH,ISOURC,IDAY,ANGLEM,IRPT,ground)

cjd3
C
C     CALCULATES TRANSMITTANCE AND RADIANCE VALUES BETWEEN IV1 AND IV2
C     FOR A GIVEN ATMOSPHERIC SLANT PATH
c
      parameter(nbins=99,iprint=50,maxv=50000)                          tras 140
Clex
Clex  Increase first dimension in SLIT to 65 to insure that all
Clex  transmittances (tx array) are passed through the slit function.
      dimension  WGT(nbins),SLIT(65,nbins)
c
c
C     CONVENTION
C     MMOLX = MAXIMUM NUMBER OF NEW SPECIES (IDENTIFIED BY "X")
C     MMOL  = MAXIMUM NUMBER OF OLD SPECIES (W/O SUFFIX "X")
C     THESE DEFINE THE MAXIMUM ARRAY SIZES.
C
C     THE ACTUAL NUMBER OF PARAMETERS ARE:
C     NSPC = ACTUAL NUMBER OF OLD SPECIES (12), CAN'T EXCEED MMOL
C     NSPECX = ACTUAL NUMBER OF "X" SPECIES,     CAN'T EXCEED MMOLX
C
c     Modtran has 65 as a magic number.  It INCLUDEs the usual 12 species
c     plus a host of other species and sub species.  Many arrays have
c     dimension 65.

      INCLUDE 'parameter.list'
C
C     TRANS VARIABLES
C
      CHARACTER*8 CNAMEX
      COMMON /NAMEX/CNAMEX(MMOLX)
Clex
Clex  Remove unused commons
Clex  COMMON /MDATAX/ WMOLXT(MMOLX,laydim)
Clex  COMMON /MODELX/ DNSTYX(MMOLX,LAYDIM)
Clex  COMMON /NONAME/ TXX(MMOLX), WX(MMOLX), WPATHX(laythr,MMOLX)
c
c
c
      LOGICAL IVTEST,loop0,ground,transm,modtrn
      COMMON RELHUM(LAYDIM),HSTOR(LAYDIM),ICH(4),VH(17),TX(65),W(65)
      COMMON IMSMX,WPATH(LAYTHR,65),TBBY(LAYTHR),PATM(LAYTHR),NSPEC,
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX2(47),AWCCON(5)
      COMMON/IFIL/IRD,IPR,IPU,NPR,IPR1,iscrch
      COMMON/CARD1/MODEL,ITYPE,IEMSCT,M1,M2,M3,IM,NOPRNT,TBOUND,SALB,
     1  MODTRN
      COMMON/CARD4/IV1,IV2,IDV,IFWHM
      COMMON/CNSTNS/PI,CA,DEG,GCAIR,BIGNUM,BIGEXP
      COMMON/CNTRL/KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT
      COMMON/SOLS/AH1(LAYTWO),ARH(LAYTWO),WPATHS(LAYTHR,65),
     1 PA(LAYTWO),PR(LAYTWO),ATHETA(LAYDIM+1),ADBETA(LAYDIM+1),
     2 LJ(LAYTWO+1),JTURN,ANGSUN,CSZEN(LAYTWO),TBBYS(LAYTHR,12),
     3 PATMS(LAYTHR,12)
      COMMON/SRAD/TEB1,TEB2SV                                           tras 290
      COMMON/MSRD/TLE(LAYDIM),COSBAR(LAYDIM),OMEGA0(LAYTWO),
     1  UPF(10,LAYDIM),DNF(10,LAYDIM),TAER(LAYDIM),ASYIK(LAYTWO),
     2  ASYDM(LAYTWO),STRN(0:LAYDIM),DMOLS(LAYTWO),DSTRN(0:LAYTWO),
     3  FDNSRT,FDNTRT,TAUT(LAYDIM),UMF(LAYDIM),DMF(LAYDIM),
     4  UMFS(LAYDIM),DMFS(LAYDIM)
      COMMON/ICLL/ICALL,FPHS,FALB,FORBIT
      common /solar/lsame
      logical lsame
c
cjd3 v
c
c******************************************
c        variables inserted by NORTH
        double precision uang
        integer nstr
        logical dis
c*******************************************
cjd3 ^
c     Initialize slit function array
      salbs = salb
Clex
Clex  Increase dimension in SLIT function initialization
      DO 10 I = 1,65
          DO 10 J = 1,nbins
   10 SLIT(I,J) =0.
c
c     Initialize radiance minimum and maximum parameters
      RADMIN=bignum
      RADMAX=0.
c
c     Initialize ground emissivity (one minus ground albedo)
      emiss = 0.
Clex
Clex  Change first .le. to .ge.
      if(salb.ge.0.0.and.salb.le.1.0) EMISS=1.-SALB
C
C     Store the number of path layers in ikmx
      IKMX=IKMAX
Clex
Clex  Initialize integrated absorption, radiance, solar irradiance sums.
Clex  (RADSUM is used in place of STSOL).
      SUMA=0.
      RADSUM=0.
      SSOL=0.
Clex  STSOL=0.
Clex
Clex  INITIALIZE RADIANCE/IRRADIANCE TERMS 
      BBG=0.
      RFTHRM=0.
      SUMSSS=0.
      SUMSSR=0.
      RFLSS=0.
      RFSURF=0.
      SOLIV=0.
      TSOLIV=0.
c
c     Initialize integration weighting factor
      FACTOR=.5
c
c     Initialize icount, used to determine when header must be printed
      ICOUNT=iprint
c
c     Do not perform a MODTRAN calculation if all sources are continuum
cssi  IF(IV1.GE.22655)modtrn=.false.
cc    IF(IV1.GE.22680)modtrn=.false.
      IF(modtrn.and.iv1.lt.22680)THEN
C
C         WHEN THE band model or line-by-line option is used, call
C         routine "bmdata" to INITIALIZE PARAMETERS AND TO SET THE
C         FREQUENCY STEP SIZE "IDVX" TO THE BAND WIDTH (1 CM-1).
          IDV5=5
          CALL bmdata(IV1,IFWHM,IDVX,IKMX,MXFREQ)
          IWIDM1=IFWHM/IDVX-1
          IV=5*((IV1-IWIDM1)/5)
          IF(IV.LT.0)IV=0
          IVX=IV-IDVX
          IV=IV-5
          IVXMAX=IV2+IWIDM1
          if(iv2.gt.mxfreq)ivmax=mxfreq+iwidm1
      ELSE
          IDV5=IDV
          IDVX=IDV5
          IWIDM1=0
          IV=IV1-IDV5
          IVX=IV
          IVXMAX=IV2+IWIDM1
          IF(IVXMAX.GT.maxv)IVXMAX=maxv
          IF(IDV.LT.5)IDV=5
      ENDIF
      IWRITE=IV1+IWIDM1
      IWIDTH=IWIDM1+1
C
C     PERFORM TRIANGULAR SLIT INITIALIZATION.  TRANSMITTANCES AT A
C     GIVEN FREQUENCY CONTRIBUTE TO 2*IWIDTH-1 TRIANGULAR SLITS.
C     THESE CONTRIBUTIONS ARE STORED IN ARRAY SLIT.  WGT IS THE
C     NORMALIZED WEIGHT USED TO DEFINE THE TRIANGLE.
      NWGT=2*IWIDTH
      WNORM=1./(IWIDTH*IWIDTH)
      DO 20 I=1,IWIDTH
          WGT(I)=I*WNORM
   20 WGT(NWGT-I)=wgt(i)
      NWGT=NWGT-1
      NWGTM1=NWGT-1
c
c     Initialize ICALL (= 0 for initial call to routine source)
      ICALL=0
c
c     Initialize transm (.true. for transmittance only calculations)
      transm=.true.
      IF(IEMSCT.EQ.1 .OR. IEMSCT.EQ.2)transm=.false.
c
c     Print headers
      print*,' iv ',iv
c      IF(IEMSCT.EQ.0)THEN
c          WRITE(IPU,333)
c333   format( '  FREQ   TOTAL     H2O     CO2+ OZONE    TRACE',
c     1        '  N2 CON  H2O CON MOL SCAT AER-HYD HNO3   AER-HYD',
c     1       '  log       CO2    CO       CH4',
c     1         '     N2O    O2     NH3      NO     NO2      SO2')
c          write(ipr1,666)
c666    format( '1/CM  TRANS   TRANS    TRANS  TRANS    TRANS   TRANS',
c     3         'TRANS  TRANS    TRANS  TRANS    TRANS')
c      ELSEIF(IEMSCT.EQ.3)THEN
c          WRITE(IPU,'(32H  FREQ   TRANS     SOL TR  SOLAR)')
c      ELSE
Clex
Clex      Alter file7 format, separating out surface emission
Clex      WRITE(IPU,'(42H  FREQ   TRANS     ATMOS   PATH     SINGLE,
Clex 1      28H   GROUND DIRECT   TOTAL RAD)')
c          WRITE(IPU,'(3A)')'  FREQ   TOT TRANS   PTH THRML   ',
c     1      'SURF EMIS    PTH SCAT   SING SCAT   GRND RFLT   ',
c     2      'DRCT RFLT   TOTAL RAD       TEB1     TEB2SV   DEPTH'
c      ENDIF
c      IF(NOPRNT.EQ.-1)THEN
c          IF(IMULT.EQ.1)THEN
c              WRITE(IPR1,'(37H        V   ALT1    UFLX        UFLXS,
c     1          50H       DFLX        DFLXS       DIRS          TRANS)')
c          ELSE
c              IF(IEMSCT.GT.0)WRITE(IPR1,'(23H        V   ALT1   ALT2,
c     1          30H    B(V,T)     DTAU        TAU)')
c          ENDIF
c      ENDIF
c
c     Initialize layer loop variables
      loop0=.true.
cjd3 v
cj      call loop(loop0,iv,ivx,ikmx,mxfreq,summs,transm,iph,
cj     1  sumssr,ivtest,unif,trace,transx,sumv,isourc,iday,anglem,frac)
c
      call loop(loop0,iv,ivx,ikmx,mxfreq,summs,transm,iph,              tras1390
     1  sumssr,ivtest,unif,trace,transx,sumv,isourc,iday,anglem,
     2  frac,uang,dis,nstr)
cjd3 ^
      loop0=.false.
c
c	gpa call for ratcsz loop
c
c         Determine layer loop maximum                                  tras2040
          IF(transm)THEN                                                tras2050
c                                                                       tras2060
c             For transmission calculations, skip over layer loop in tratras2070
              IKMAX=1                                                   tras2080
          ELSEIF(IMULT.EQ.1 .and. .not. lsame)THEN                      tras2090
c                                                                       tras2100
C             FOR MULTIPLE SCATTERING SET IKMAX TO IMSMX                tras2110
              IKMAX=IMSMX                                               tras2120
              CALL RATCSZ
          ELSE                                                          tras2130
c                                                                       tras2140
C             IF NOT MULTIPLE SCATTERING, RESET IKMAX TO ORIGINAL VALUE tras2150
              IKMAX=IKMX                                                tras2160
          ENDIF                                                         tras2170
C
C     END INITIALIZATION, BEGIN OF FREQUENCY LOOP
C
C     "IVX" IS THE FREQUENCY AT WHICH TRANSMITTANCE WILL BE CALCULATED.
C     DURING THE FIRST PASS, "IVX" AND "IV" MUST BE EQUAL.
   30 IVX=IVX+IDVX
          IF(IV.LT.IVX)THEN
              IV=IV+IDV5
              IVTEST=.TRUE.
          ELSE
              IVTEST=.FALSE.
          ENDIF
C
C         SET INTERPOLATION FRACTION.
          FRAC=FLOAT(IV-IVX)/IDV5
          IF(ICOUNT.EQ.iprint)THEN
          print*,' iv ',iv
c
c             Reinitialize counter and print header
              ICOUNT=0
              IF(IEMSCT.EQ.0)THEN
                  WRITE(IPR,'(1H1,/33H   FREQ WAVELENGTH  TOTAL     H2O,
     1              47H     CO2+     OZONE    TRACE  N2 CONT  H2O CONT,
     2              47H MOL SCAT  AER-HYD  HNO3    AER-HYD  INTEGRATED,
     3              /43H   1/CM  MICRONS    TRANS    TRANS    TRANS,
     4              44H    TRANS    TRANS   TRANS    TRANS    TRANS,
     5              40H     TRANS   TRANS    ABS     ABSORPTION,/)')
c                 WRITE(IPR,111)'ALL MINOR SPECIES',
c    $                 (CNAMEX(KX),KX=1,11),
c    $                 (CNAMEX(KX),KX=12,min(22,nspecx)),
c    $                 (' ',kx=min(22,nspecx)+1,22),
c    $                 ('TRANS',KX=1,12)
 111              FORMAT(8X,A,11(1X,A),/,25X,11(1x,A),
     $                 /,20X,A,8(4X,A),2(3X,A),4X,A/)
              ELSEIF(IEMSCT.EQ.1)THEN
Clex
Clex              Separate out surface emission
Clex              in IEMSCT=1 TAPE6 heading.
Clex              WRITE(IPR,'(1H1,20X,28HRADIANCE(WATTS/CM2-STER-XXX),
Clex 1              /8H0   FREQ,T10,6HWAVLEN,T19,14HATMOS RADIANCE,
Clex 2              T39,9H INTEGRAL,T49,5HTOTAL,/2X,6H(CM-1),
Clex 3              T10,7H(MICRN),T19,6H(CM-1),T29,7H(MICRN),
Clex 4              T39,6H(CM-1),T49,5HTRANS,/)')
                  WRITE(IPR,'(A,45X,A,3(/3A))')
     1              '1','RADIANCE(WATTS/CM2-STER-XXX)',
     2              '0  FREQ   WAVLEN    PATH THERMAL    ',
     3              '  SURFACE EMISSION    SURFACE REFLECTED ',
     4              '    TOTAL RADIANCE   INTEGRAL    TOTAL',
     5              '  (CM-1) (MICRN)    (CM-1)   (MICRN)',
     6              '    (CM-1)   (MICRN)    (CM-1)   (MICRN)',
     7              '    (CM-1)   (MICRN)    (CM-1)   TRANS'
              ELSEIF(IEMSCT.EQ.3)THEN
                  WRITE(IPR,'(1H1,22X,27HIRRADIANCE (WATTS/CM2-XXXX),
     1              /7H0  FREQ,T11,6HWAVLEN,T23,11HTRANSMITTED,
     2              T45,5HSOLAR,T61,10HINTEGRATED,T80,5HTOTAL,
     3              /2X,6H(CM-1),T10,7H(MICRN),T20,6H(CM-1),
     4              T30,7H(MICRN),T40,6H(CM-1),T50,7H(MICRN),
     5              T60,6HTRANS.,T70,5HSOLAR,T80,5HTRANS)')
              ELSEIF(IMULT.EQ.0)THEN
Clex
Clex              Separate out surface emission in
Clex              IEMSCT=2 & IMULT=0 TAPE6 heading.
Clex              WRITE(IPR,'(1H1,45X,28HRADIANCE(WATTS/CM2-STER-XXX),
Clex 1              /7H0  FREQ,T11,6HWAVLEN,T21,14HATMOS RADIANCE,
Clex 2              T41,14HPATH SCATTERED,T61,16HGROUND REFLECTED,
Clex 3              T85,5HTOTAL,T99,8HINTEGRAL,T110,5HTOTAL,
Clex 4              /2X,6H(CM-1),T10,7H(MICRN),T20,6H(CM-1),
Clex 5              T30,7H(MICRN),T40,6H(CM-1),T50,7H(MICRN),
Clex 6              T60,6H(CM-1),T70,7H(MICRN),T80,6H(CM-1),
Clex 7              T90,7H(MICRN),T100,6H(CM-1),T110,5HTRANS,/)')
                  WRITE(IPR,'(A,45X,A,3(/3A))')
     1              '1','RADIANCE(WATTS/CM2-STER-XXX)',
     2              '0  FREQ   WAVLEN    PATH THERMAL     SURFACE ',
     3              'EMISSION    SINGLE SCATTER    GROUND REFLECTED ',
     4              '   TOTAL RADIANCE   INTEGRAL     TOTAL',
     5              '  (CM-1) (MICRN)    (CM-1)  (MICRN)    (CM-1)',
     6              '  (MICRN)    (CM-1)  (MICRN)    (CM-1)  (MICRN)',
     7              '    (CM-1)  (MICRN)  (CM-1)      TRANS'
              ELSE
Clex
Clex              Separate out surface emission in
Clex              IEMSCT=2 & IMULT=1 TAPE6 heading.
Clex              WRITE(IPR,'(1H1,45X,28HRADIANCE(WATTS/CM2-STER-XXX),
Clex 1              //6H0 FREQ,T10,6HWAVLEN,T20,14HATMOS RADIANCE,T40,
Clex 2              4HPATH,19H SCATTERED RADIANCE,T69,
Clex 3              25HGROUND REFLECTED RADIANCE,T100,14HTOTAL RADIANCE,
Clex 4              T118,8HINTEGRAL,T127,5HTOTAL,/T45,5HTOTAL,T59,
Clex 5              6HS SCAT,T75,5HTOTAL,T89,6HDIRECT,/1X,6H(CM-1),T9,
Clex 6              7H(MICRN),T19,6H(CM-1),T29,7H(MICRN),T39,6H(CM-1),
Clex 7              T49,7H(MICRN),T59,6H(CM-1),T69,6H(CM-1),T79,
Clex 8              7H(MICRN),T89,6H(CM-1),T99,6H(CM-1),T109,7H(MICRN),
Clex 9              T119,6H(CM-1),T127,5HTRANS,/)')
                  WRITE(IPR,'(A,45X,A,/4(/3A))')
     1              '1','RADIANCE(WATTS/CM2-STER-XXX)',
     2              '0 FREQ   WAVLEN   PATH THERMAL    SURFACE   ',
     3              'PATH SCATTERED RADIANCE   GROUND REFLECTED ',
     4              'RADIANCE   TOTAL RADIANCE   INTEGRAL    TOTAL',
     5              '                                  EMISSION  ',
     6              '     TOTAL        S SCAT        TOTAL      ',
     7              '  DIRECT                                TRANS',
     8              ' (CM-1) (MICRN)   (CM-1)  (MICRN)   (CM-1)  ',
     9              ' (CM-1)  (MICRN)   (CM-1)   (CM-1)  (MICRN)',
     &              '   (CM-1)   (CM-1)  (MICRN)   (CM-1)'
              ENDIF
          ENDIF
c
c         Determine layer loop maximum
          IF(transm)THEN
c
c             For transmission calculations, skip over layer loop in trans
              IKMAX=1
          ELSEIF(IMULT.EQ.1 .and. .not. lsame)THEN
c
C             FOR MULTIPLE SCATTERING SET IKMAX TO IMSMX
              IKMAX=IMSMX
          ELSE
c
C             IF NOT MULTIPLE SCATTERING, RESET IKMAX TO ORIGINAL VALUE
              IKMAX=IKMX
          ENDIF
          SUMV=0.
c
c         Initialize transmission array
          TX(1)=1.
          TX(2)=1.
          TX(3)=1.
          DO 40 K=4,KMAX
   40     TX(K)=0.
cjd3 v
cj      call loop(loop0,iv,ivx,ikmx,mxfreq,summs,transm,iph,
cj     1  sumssr,ivtest,unif,trace,transx,sumv,isourc,iday,anglem,frac)
c
      call loop(loop0,iv,ivx,ikmx,mxfreq,summs,transm,iph,              tras1390
     1  sumssr,ivtest,unif,trace,transx,sumv,isourc,iday,anglem,
     2  frac,uang,dis,nstr)
cjd3
cjd3
          IF(IEMSCT.NE.0)THEN
Clex
Clex          RADIANCE OR IRRADIANCE CALCULATIONS
              IF(IVX.EQ.0)THEN
                  V=.5*IDVX
              ELSE
                  V=IVX
              ENDIF
Clex
Clex          Emissivity at ivx (not iv) is needed.
          if(salbs.lt.0)call rhoeps(ivX,lsalb,salb,emiss)
          emiss = 1. - salb
Clex
Clex          CONVERT ALL RADIANCE[IRRADIANCE] TERMS TO W/CM2-SR-(CM-1)
Clex          [W/CM2-(CM-1)] BEFORE PASSING OVER TRIANGULAR SLIT.
              CONVRT=1.E-4*V**2
              IF(IEMSCT.EQ.3)THEN
Clex
Clex              EXTRA-TERRESTRIAL SOLAR IRRADIANCE 
                  CALL SOURCE(V,ISOURC,IDAY,ANGLEM,SOLIV)
                  SOLIV=SOLIV/CONVRT
Clex
Clex              TRANSMITTED SOLAR IRRADIANCE
                  TSOLIV=TX(9)*SOLIV
              ELSE
Clex
Clex              PATH THERMAL RADIANCE
                  SUMV=SUMV/CONVRT
Clex
Clex              THERMAL BOUNDARY EMISSION
                  IF(TBOUND.GT.0.)BBG=BBFN(TBOUND,V)*TX(9)*EMISS/CONVRT
Clex
Clex              SURFACE REFLECTED THERMAL SCATTERED RADIANCE
                  RFSURF=0.
                  IF(IMULT.EQ.1 .AND. GROUND)
     1              RFSURF=SALB*FDNTRT*TX(9)/PI/CONVRT
                  IF(IEMSCT.EQ.2)THEN
Clex
Clex                  EXTRA-TERRESTRIAL SOLAR IRRADIANCE 
                      CALL SOURCE(V,ISOURC,IDAY,ANGLEM,SS)
                      SS=SS/CONVRT
Clex
Clex                  SINGLE SCATTER SOLAR RADIANCE
                      SUMSSS=SUMSSR*SS
Clex
Clex                  SINGLE + MULTIPLE SOLAR SCATTERED RADIANCE
                      SUMSSR=SUMSSS+SUMMS/CONVRT
Clex
Clex                  SURFACE REFLECTED SOLAR SCATTER RADIANCES
                      RFLSS=0.
                      IF(GROUND .AND. TEB1.GT.0.)THEN
Clex
Clex                      DIRECT TERM
                          IF(ANGSUN.GE.0.)
     1                      RFLSS=SS*TEB1*SALB*COS(ANGSUN*CA)/PI
Clex
Clex                      SOLAR + THERMAL SURFACE REFLECTED RADIANCE
                          RFSURF=RFSURF+RFLSS
                          IF(IMULT.EQ.1)
     1                      RFSURF=RFSURF+SALB*FDNSRT*TX(9)/PI/CONVRT
                      ENDIF
                  ENDIF
              ENDIF
          ENDIF
C
C         TRANSMITTANCES, RADIANCES [W/CM2-SR-(CM-1)], AND IRRADIANCES
C         [W/CM2-(CM-1)] ARE TEMPORARILY STORED IN "TX" SO THAT THEIR
C         CONVOLUTION OVER THE TRIANGULAR SLIT CAN BE CALCULATED.
          TX(1)=BBG
          TX(2)=UNIF
          TX(3)=TRACE
Clex
Clex      These calculations were performed above and converted to
Clex      proper units (subtracting IWIDM1 from IVX is incorrect).
Clex      if(tbound.gt.0. .and. .not.transm)then
Clex         v = ivx - IWIDM1
Clex         bbg=bbfn(tbound,v)*tx(9)*emiss
Clex         if(imult.eq.1.and.ground)bbg=bbg+salb*fdntrt*tx(9)/pi
Clex         sumv=sumv+bbg
Clex      endif
          TX(8)=SUMV
          TX(12)=SUMSSR
          TX(13)=SUMSSS
          TX(14)=TEB1
	  TX(15)=RFSURF
	  TX(18)=TEB2SV
	  TX(19)=RFLSS
	  TX(20)=TSOLIV
	  TX(21)=SOLIV
          DO 60 K=1,65
              IP1=NWGT
              DO 50 I=NWGTM1,1,-1
                  SLIT(K,IP1)=SLIT(K,I)+WGT(IP1)*tx(k)
   50         IP1=I
              SLIT(K,1)=WGT(1)*tx(k)
   60     TX(K)=SLIT(K,NWGT)
C
C         CHECK IF VALUES ARE TO BE PRINTED
          IF(IVX.LT.IWRITE)GOTO30
          IWRITE=IWRITE+IDV
          IF(IWRITE.GT.IVXMAX)FACTOR=.5
          ICOUNT=ICOUNT+1
C
C         RENORMALIZE IF TRIANGULAR SLIT EXTENDS TO NEGATIVE FREQUENCIES
          IF(IVX.LT.NWGTM1)THEN
              store=1.-.5*(NWGTM1-IVX)*(NWGTM1-IVX+1)*WNORM
              DO 70 K=1,65
   70         TX(K)=TX(K)/store
          ENDIF
	  BBG=TX(1)
          UNIF=TX(2)
          TRACE=TX(3)
          SUMV=TX(8)
          SUMSSR=TX(12)
          SUMSSS=TX(13)
          TEB1=TX(14)
          RFSURF=TX(15)
          TEB2SV=TX(18)
          RFLSS=TX(19)
          TSOLIV=TX(20)
          SOLIV=TX(21)
          V=FLOAT(IVX-IWIDM1)
          ALAM=1.0E+04/(V+.000001)
          SUMA=SUMA+FACTOR*IDV*(1.0-TX(9))
Clex
Clex      ALTX9 is now output using an F format when IEMSCT = 1 or 2,
Clex      so the maximum is reduced to 999.999 (Still absurdly large)
          ALTX9=999.999
          IF(TX(9).GT.0.)ALTX9=-LOG(TX(9))
          GOTO(80,90,90,100),IEMSCT+1
C
C         TRANSMITTANCE ONLY
   80     TX(10)=1.-TX(10)
          TX(7)=TX(7)*TX(16)
          WRITE(IPR,'(F8.0,F8.3,11F9.4,F12.3)')V,ALAM,TX(9),TX(17),     tras2740
     1      UNIF,TX(31),TRACE,TX(4),TX(5),TX(6),TX(7),TX(11),TX(10),SUMAtras2750
c         WRITE(IPR1,'(F7.0,11F8.4,1PE10.3)')V,TX(17),TX(31),TX(36),    tras2760
c    1      TX(44),TX(46),TX(47),TX(50),TX(52),TX(54),TX(64),TX(65)     tras2770
          tx(64) = tx(64) * tx(55)
          tx(65) = tx(65) * tx(56)
Clex
Clex      This write statement is too long - not my doing.
c          WRITE(IPU,'(F7.0,11F8.4,1PE10.3,0p,9f8.4)')
c     1      V,TX(9),TX(17),UNIF,
c     1      TX(31),TRACE,TX(4),TX(5),TX(6),TX(7),TX(11),TX(10),ALTX9,   tras2790
c     1 TX(36),TX(44),TX(46),TX(47),TX(50),TX(52),TX(54),TX(64),TX(65)
          GOTO110
Clex
Clex      Comments are dropped that are no longer appropriate.
Clex
Clex      RADIANCE PATHS
   90     CONTINUE
Clex
Clex      CONVRT is the conversion from W/CM2-SR-(CM-1) to W/CM2-SR-um
          CONVRT=1.E-4*V**2
          IF(V.EQ.0)CONVRT=1.E-4*(.5*IDVX)**2
          IF(IEMSCT.EQ.1)THEN
Clex          RADSUM=RADSUM+IDV*FACTOR*SUMV
Clex          WRITE(IPR,'(F8.0,F8.3,1P3E10.2,0PF9.4)')
Clex 1          V,ALAM,SUMV,SUMVV,RADSUM,TX(9)
Clex          WRITE(IPU,'(F7.0,F8.4,1PE9.2,T96,E10.3)')
Clex 1          V,TX(9),SUMV,ALTX9
Clex          WRITE(IPr1,'(F7.0,11F8.4,1PE10.3)')V,TX(9),TX(17),UNIF,
Clex          1      TX(31),TRACE,TX(4),TX(5),TX(6),TX(7),TX(11),TX(10),ALTX9
Clex          SUMT=SUMV
Clex          SUMTT=SUMVV
Clex
Clex          SUMT is the total spectral radiance, i.e. SUMT
Clex          equals the sum of the direct + multiply scattered
Clex          thermal path radiance (SUMV), the surface emission
Clex          (BBG), and the reflected surface term (RFSURF).
Clex          Each of these terms has units W/CM2-SR-(CM-1).
Clex          RADSUM is the integrated total radiance (W/CM2-SR).
              SUMT=SUMV+BBG+RFSURF
              RADSUM=RADSUM+IDV*FACTOR*SUMT
              WRITE(IPR,'(F8.0,F8.3,1P9E10.2,0PF9.5)')
     1          V,ALAM,SUMV,CONVRT*SUMV,BBG,CONVRT*BBG,RFSURF,
     2          CONVRT*RFSURF,SUMT,CONVRT*SUMT,RADSUM,TX(9)
c              WRITE(IPU,'(0PF7.0,F11.8,1P2E12.5,12X,2(12X,E12.5),
c     1          22X,0PF8.3)')V,TX(9),SUMV,BBG,RFSURF,SUMT,ALTX9
          ELSE
Clex
Clex          SOLAR SCATTERED RADIANCE
Clex          CALL SOURCE(V,ISOURC,IDAY,ANGLEM,SS)
Clex
Clex          MULTIPLY SUMSSR BY THE EXTRATERRESTRIAL SOURCE STRENGTH SS
Clex          SUMSSS=SUMSSR*SS
Clex
Clex          CALCULATE TOTAL SINGLE SCATTERED + MULTIPLE SCATTERED
Clex          SOLAR RADIANCE FOR EACH FREQUENCY [W/CM2-STER-MICROMETER]
Clex          SUMSSR=SUMSSS+SUMMS
Clex          store=0.
Clex          if(v.gt.0.)store=1.e4/v**2
Clex          SUMS=store*SUMSSR
Clex          SUMSSS=store*SUMSSS
Clex
Clex          RFLSOL IS GROUND-REFLECTED DIRECT SOURCE RADIANCE AND
Clex          RFLSOL=0.
Clex          RFLS=0.
Clex          RFLSS=0.
Clex          RFLSSS=0.
Clex          IF(ground .AND. TEB1.GT.0)THEN
Clex              IF(ANGSUN.GE.0.0 .and.ANGSUN.le.90. )
Clex x  RFLSSS=SS*TEB1*SALB*COS(ANGSUN*CA)/PI
Clex              RFLSOL=RFLSSS
Clex
Clex              IF(IMULT.EQ.1)RFLSOL=RFLSOL+SALB*FDNSRT*TX(9)/PI
Clex              RFLS=STORE*RFLSOL
Clex              RFLSS=STORE*RFLSSS
Clex          ENDIF
Clex          SUMT=SUMV+SUMS+RFLS
Clex          SUMTT=SUMVV+SUMSSR+RFLSOL
Clex
Clex          SUMT is the total spectral radiance, i.e. SUMT
Clex          equals the sum of the direct + multiply scattered
Clex          thermal path radiance (SUMV), the surface emission
Clex          (BBG), the single + multiple solar scattered radiance
Clex          (SUMSSR) and the reflected surface term (RFSURF).
Clex          Each of these terms has units W/CM2-SR-(CM-1).
Clex          RADSUM is the integrated total radiance (W/CM2-SR).
              SUMT=SUMV+BBG+SUMSSR+RFSURF
              RADSUM=RADSUM+IDV*FACTOR*SUMT
              IF(IMULT.NE.1)THEN
Clex              WRITE(IPR,'(F8.0,F8.3,1P9E10.2,0PF9.4)')
Clex 1              V,ALAM,SUMV,SUMVV,SUMS,SUMSSR,RFLS,RFLSOL,
Clex 2              SUMT,SUMTT,RADSUM,TX(9)
                  WRITE(IPR,'(F8.0,F8.3,5(1PE10.2,E9.2),E10.2,0PF9.5)')
     1              V,ALAM,SUMV,CONVRT*SUMV,BBG,CONVRT*BBG,
     2              SUMSSR,CONVRT*SUMSSR,RFSURF,CONVRT*RFSURF,
     3              SUMT,CONVRT*SUMT,RADSUM,TX(9)
              ELSE
Clex              WRITE(IPR,'(F7.0,F8.3,1P11E10.2,0PF7.4)')
Clex 1              V,ALAM,SUMV,SUMVV,SUMS,SUMSSR,SUMSSS,RFLS,
Clex 2              RFLSOL,RFLSS,SUMT,SUMTT,RADSUM,TX(9)
                  WRITE(IPR,'(F7.0,F8.3,1P11E9.2,E10.2,0PF8.5)')
     1              V,ALAM,SUMV,CONVRT*SUMV,BBG,SUMSSR,CONVRT*SUMSSR,
     2              SUMSSS,RFSURF,CONVRT*RFSURF,RFLSS,
     3              SUMT,CONVRT*SUMT,RADSUM,TX(9)
              ENDIF
Clex          WRITE(IPU,'(F7.0,F8.4,1P6E9.2,0P2F8.4,T96,1PE10.3)')V,
Clex 1          TX(9),SUMV,SUMS,SUMSSS,RFLS,RFLSS,SUMT,TEB1,TEB2SV,ALTX9
c              WRITE(IPU,'(0PF7.0,F11.8,1P7E12.5,0P2F11.8,F8.3)')
c     1          V,TX(9),SUMV,BBG,SUMSSR,SUMSSS,
c     2          RFSURF,RFLSS,SUMT,TEB1,TEB2SV,ALTX9
          ENDIF
          GOTO110
C
C         DIRECTLY TRANSMITTED SOLAR IRRADIANCE [WATTS/(CM2 MICROMETER)]
  100     CONTINUE
Clex 100  CALL SOURCE(V,ISOURC,IDAY,ANGLEM,SOLIL)
Clex      SOLIV=0.
Clex      IF(V.GT.0.)SOLIV=SOLIL*1.E+4/V**2
Clex      TSOLIV=SOLIV*TX(9)
Clex      TSOLIL=SOLIL*TX(9)
Clex      STSOL=STSOL+TSOLIV*IDV*FACTOR
Clex
Clex      CONVRT is the conversion from W/CM2-(CM-1) to W/CM2-um
          CONVRT=1.E-4*V**2
          IF(V.EQ.0)CONVRT=1.E-4*(.5*IDVX)**2
Clex
Clex      RADSUM is the integrated transmitted solar irradiance and SSOL
Clex      is the integrated extra-terrestrial solar irradiance (W/CM2).
          RADSUM=RADSUM+TSOLIV*IDV*FACTOR
          SSOL=SSOL+SOLIV*IDV*FACTOR
Clex      WRITE(IPR,'(F8.0,F8.3,1P6E10.2,0PF9.4)')
Clex 1      V,ALAM,TSOLIV,TSOLIL,SOLIV,SOLIL,STSOL,SSOL,TX(9)
          WRITE(IPR,'(F8.0,F8.3,1P6E10.2,0PF9.4)')V,ALAM,
     1      TSOLIV,CONVRT*TSOLIV,SOLIV,CONVRT*SOLIV,RADSUM,SSOL,TX(9)
c          WRITE(IPU,'(F7.0,F8.4,1P2E9.2,T96,E10.3)')
c     1      V,TX(9),TSOLIV,SOLIV,ALTX9
          SUMT=TSOLIV
Clex
Clex      Variable STSOL has been replaced with RADSUM
Clex      RADSUM=STSOL
  110     IF(IEMSCT.NE.0)THEN
              IF(SUMT.GE.RADMAX)THEN
                  VRMAX=V
                  RADMAX=SUMT
              ENDIF
              IF(SUMT.LE.RADMIN)THEN
                  VRMIN=V
                  RADMIN=SUMT
              ENDIF
          ENDIF
          FACTOR=1.
      IF(IWRITE.LE.IVXMAX)GOTO30
C
C     END OF FREQUENCY LOOP
C
      IVX=INT(V+.5)
      WRITE(IPR,'(27H0INTEGRATED ABSORPTION FROM,I5,3H TO,I5,7H CM-1 =,
     1  F10.2,5H CM-1,/24H AVERAGE TRANSMITTANCE =,F6.4,/)')
     2  IV1,IVX,SUMA,1.-SUMA/(IVX-IV1)
      IF(IEMSCT.EQ.3)THEN
          WRITE(IPR,'(24h0INTEGRATED IRRADIANCE =,1PE11.3,
     1      11h WATTS CM-2,/24h MINIMUM IRRADIANCE    =,E11.3,
     2      14h WATTS CM-2 AT,0PF11.1,5h CM-1,/10h  MAXIMUM ,
     3      14h RADIANCE    =,1PE11.3,24h WATTS CM-2  (CM-1)-1 AT,
     4      0PF11.1,5H CM-1)')RADSUM,RADMIN,VRMIN,RADMAX,VRMAX
      ELSEIF(IEMSCT.NE.0)THEN
          WRITE(IPR,'(22H0INTEGRATED RADIANCE =,1PE11.3,11H WATTS CM-2,
     1      7H STER-1,/22H MINIMUM RADIANCE    =,E11.3,11H WATTS CM-2,
     2      19H STER-1 (CM-1)-1 AT,0PF11.1,5H CM-1,/8H MAXIMUM,
     3      14H RADIANCE    =,1PE11.3,30H WATTS CM-2 STER-1 (CM-1)-1 AT,
     4      0PF11.1,5H CM-1)')RADSUM,RADMIN,VRMIN,RADMAX,VRMAX
          WRITE(IPR,'(23H BOUNDARY TEMPERATURE =,F11.2,
     1      2H K,/22H BOUNDARY EMISSIVITY =,F12.3)')TBOUND,EMISS
      ENDIF
cc    IF(modtrn .AND. IV2.GT.MXFREQ)THEN
cc        IRPT=-4
cc        IV1=MXFREQ
cc    ENDIF
      RETURN
      END
      BLOCK DATA SOLA1                                                  solr 100
C>    BLOCK DATA                                                        solr 110
C                                                                       solr 120
C                                                                       solr 130
c     COMMON /SUNDAT/ SOLARA(1403), SOLARB(2951)                        sun  250
      COMMON /SUNDAT/   SUNA0(100),SUNA1(100),SUNA2(100),SUNA3(100),
     x   SUNA4(100),SUNA5(100),SUNA6(100),SUNA7(100),SUNA8(100),
     x   SUNA9(100),SUNA10(100),SUNA11(100),SUNA12(100),SUNA13(100),
     x   suna14(3),sunbp(1),sunb28(100),sunb29(100),sunb30(100),
     x   sunb31(100),sunb32(100),sunb33(100),sunb34(100),sunb35(100),
     x   sunb36(100),sunb37(100),sunb38(100),sunb39(100),sunb40(100),
     x   sunb41(100),sunb42(100),sunb43(100),sunb44(100),sunb45(100),
     x   sunb46(100),sunb47(100),sunb48(100),sunb49(100),
     E                              SUNB16(144),SUNB17(144),SUNB18(144),solr 200
     F      SUNB19(144),SUNB20(144),SUNB21( 30)                         solr 210
c
c      this data is a 20cm-1 triangle  sampled every 20cm-1 0 to 28000
c
      data suna0/
     A 0.0000E+00, 4.5756E-08, 7.0100E-07, 3.4580E-06, 1.0728E-05,      solr 250
     x 2.742E-05, 5.570E-05, 1.019E-04, 1.723E-04, 2.743E-04,
     x 4.162E-04, 6.078E-04, 8.589E-04, 1.181E-03, 1.585E-03,
     x 2.085E-03, 2.695E-03, 3.430E-03, 4.304E-03, 5.335E-03,
     x 6.541E-03, 7.940E-03, 9.552E-03, 1.140E-02, 1.349E-02,
     x 1.587E-02, 1.855E-02, 2.155E-02, 2.489E-02, 2.861E-02,
     x 3.272E-02, 3.726E-02, 4.228E-02, 4.774E-02, 5.373E-02,
     x 6.028E-02, 6.737E-02, 7.511E-02, 8.344E-02, 9.249E-02,
     x 1.022E-01, 1.127E-01, 1.239E-01, 1.360E-01, 1.489E-01,
     x 1.628E-01, 1.775E-01, 1.932E-01, 2.099E-01, 2.277E-01,
     x 2.464E-01, 2.663E-01, 2.876E-01, 3.099E-01, 3.336E-01,
     x 3.584E-01, 3.847E-01, 4.125E-01, 4.418E-01, 4.725E-01,
     x 5.046E-01, 5.383E-01, 5.739E-01, 6.114E-01, 6.503E-01,
     x 6.909E-01, 7.336E-01, 7.757E-01, 8.241E-01, 8.726E-01,
     x 9.235E-01, 9.754E-01, 1.030E+00, 1.088E+00, 1.146E+00,
     x 1.209E+00, 1.273E+00, 1.336E+00, 1.407E+00, 1.477E+00,
     x 1.548E+00, 1.627E+00, 1.701E+00, 1.783E+00, 1.864E+00,
     x 1.948E+00, 2.039E+00, 2.131E+00, 2.223E+00, 2.317E+00,
     x 2.416E+00, 2.516E+00, 2.619E+00, 2.728E+00, 2.839E+00,
     x 2.954E+00, 3.071E+00, 3.194E+00, 3.313E+00, 3.455E+00/
      data suna1/
     x 3.576E+00, 3.714E+00, 3.863E+00, 4.006E+00, 4.140E+00,
     x 4.296E+00, 4.437E+00, 4.573E+00, 4.744E+00, 4.871E+00,
     x 5.055E+00, 5.277E+00, 5.441E+00, 5.623E+00, 5.928E+00,
     x 6.158E+00, 6.402E+00, 6.785E+00, 7.009E+00, 7.197E+00,
     x 7.469E+00, 7.719E+00, 7.957E+00, 8.152E+00, 8.457E+00,
     x 8.694E+00, 8.999E+00, 9.297E+00, 9.576E+00, 9.852E+00,
     x 1.017E+01, 1.044E+01, 1.073E+01, 1.100E+01, 1.134E+01,
     x 1.159E+01, 1.203E+01, 1.240E+01, 1.281E+01, 1.314E+01,
     x 1.349E+01, 1.388E+01, 1.427E+01, 1.465E+01, 1.503E+01,
     x 1.546E+01, 1.581E+01, 1.614E+01, 1.666E+01, 1.712E+01,
     x 1.756E+01, 1.796E+01, 1.835E+01, 1.895E+01, 1.941E+01,
     x 1.992E+01, 2.040E+01, 2.091E+01, 2.140E+01, 2.192E+01,
     x 2.243E+01, 2.300E+01, 2.351E+01, 2.405E+01, 2.457E+01,
     x 2.516E+01, 2.583E+01, 2.638E+01, 2.703E+01, 2.763E+01,
     x 2.821E+01, 2.879E+01, 2.950E+01, 3.013E+01, 3.043E+01,
     x 3.146E+01, 3.211E+01, 3.284E+01, 3.354E+01, 3.428E+01,
     x 3.484E+01, 3.530E+01, 3.642E+01, 3.713E+01, 3.787E+01,
     x 3.866E+01, 3.944E+01, 4.009E+01, 4.097E+01, 4.199E+01,
     x 4.255E+01, 4.374E+01, 4.461E+01, 4.493E+01, 4.609E+01,
     x 4.657E+01, 4.741E+01, 4.836E+01, 4.972E+01, 5.033E+01/
      data suna2/
     x 5.162E+01, 5.189E+01, 5.323E+01, 5.439E+01, 5.464E+01,
     x 5.630E+01, 5.730E+01, 5.750E+01, 5.955E+01, 6.018E+01,
     x 6.125E+01, 6.243E+01, 6.291E+01, 6.489E+01, 6.550E+01,
     x 6.605E+01, 6.828E+01, 6.949E+01, 7.011E+01, 7.114E+01,
     x 7.287E+01, 7.362E+01, 7.504E+01, 7.650E+01, 7.828E+01,
     x 7.970E+01, 8.088E+01, 8.229E+01, 8.354E+01, 8.464E+01,
     x 8.590E+01, 8.385E+01, 8.895E+01, 8.995E+01, 9.100E+01,
     x 9.225E+01, 9.324E+01, 9.480E+01, 9.612E+01, 9.675E+01,
     x 9.848E+01, 1.008E+02, 1.021E+02, 1.043E+02, 1.064E+02,
     x 1.076E+02, 1.095E+02, 1.115E+02, 1.139E+02, 1.153E+02,
     x 1.167E+02, 1.185E+02, 1.197E+02, 1.214E+02, 1.239E+02,
     x 1.256E+02, 1.271E+02, 1.212E+02, 1.321E+02, 1.315E+02,
     x 1.346E+02, 1.365E+02, 1.383E+02, 1.403E+02, 1.397E+02,
     x 1.450E+02, 1.454E+02, 1.457E+02, 1.503E+02, 1.518E+02,
     x 1.541E+02, 1.538E+02, 1.570E+02, 1.601E+02, 1.624E+02,
     x 1.558E+02, 1.683E+02, 1.689E+02, 1.706E+02, 1.725E+02,
     x 1.740E+02, 1.755E+02, 1.796E+02, 1.814E+02, 1.825E+02,
     x 1.856E+02, 1.871E+02, 1.907E+02, 1.846E+02, 1.980E+02,
     x 1.964E+02, 2.012E+02, 2.002E+02, 2.047E+02, 2.064E+02,
     x 2.113E+02, 2.116E+02, 2.103E+02, 2.166E+02, 2.193E+02/
      data suna3/
     x 2.212E+02, 2.251E+02, 2.263E+02, 2.278E+02, 2.286E+02,
     x 2.266E+02, 2.371E+02, 2.415E+02, 2.409E+02, 2.410E+02,
     x 2.444E+02, 2.486E+02, 2.498E+02, 2.526E+02, 2.531E+02,
     x 2.551E+02, 2.602E+02, 2.523E+02, 2.673E+02, 2.647E+02,
     x 2.694E+02, 2.690E+02, 2.719E+02, 2.727E+02, 2.770E+02,
     x 2.772E+02, 2.802E+02, 2.816E+02, 2.840E+02, 2.876E+02,
     x 2.891E+02, 2.947E+02, 2.843E+02, 2.976E+02, 2.982E+02,
     x 3.027E+02, 2.894E+02, 3.041E+02, 3.066E+02, 3.047E+02,
     x 3.099E+02, 3.115E+02, 3.130E+02, 3.162E+02, 3.159E+02,
     x 3.215E+02, 3.213E+02, 3.179E+02, 3.291E+02, 3.307E+02,
     x 3.283E+02, 3.313E+02, 3.358E+02, 3.391E+02, 3.359E+02,
     x 3.434E+02, 3.434E+02, 3.430E+02, 3.497E+02, 3.513E+02,
     x 3.531E+02, 3.547E+02, 3.570E+02, 3.577E+02, 3.629E+02,
     x 3.607E+02, 3.663E+02, 3.678E+02, 3.693E+02, 3.707E+02,
     x 3.735E+02, 3.774E+02, 3.805E+02, 3.802E+02, 3.867E+02,
     x 3.886E+02, 3.877E+02, 3.955E+02, 3.992E+02, 4.030E+02,
     x 3.973E+02, 4.031E+02, 4.129E+02, 4.140E+02, 4.148E+02,
     x 4.193E+02, 4.233E+02, 4.262E+02, 4.311E+02, 4.329E+02,
     x 4.011E+02, 4.436E+02, 4.453E+02, 4.460E+02, 4.433E+02,
     x 4.477E+02, 4.507E+02, 4.539E+02, 4.556E+02, 4.577E+02/
      data suna4/
     x 4.636E+02, 4.633E+02, 4.611E+02, 4.668E+02, 4.724E+02,
     x 4.728E+02, 4.769E+02, 4.793E+02, 4.847E+02, 4.856E+02,
     x 4.881E+02, 4.935E+02, 4.952E+02, 4.899E+02, 4.962E+02,
     x 5.009E+02, 5.110E+02, 4.897E+02, 5.102E+02, 5.161E+02,
     x 5.143E+02, 5.149E+02, 5.183E+02, 5.189E+02, 5.223E+02,
     x 5.211E+02, 5.310E+02, 5.332E+02, 5.330E+02, 5.321E+02,
     x 5.395E+02, 5.331E+02, 5.467E+02, 5.492E+02, 5.462E+02,
     x 5.507E+02, 5.572E+02, 5.516E+02, 5.529E+02, 5.562E+02,
     x 5.674E+02, 5.623E+02, 5.694E+02, 5.705E+02, 5.646E+02,
     x 5.814E+02, 5.770E+02, 5.799E+02, 5.829E+02, 5.836E+02,
     x 5.896E+02, 5.924E+02, 5.953E+02, 6.004E+02, 5.992E+02,
     x 5.961E+02, 6.002E+02, 5.758E+02, 6.202E+02, 6.154E+02,
     x 6.080E+02, 6.248E+02, 6.082E+02, 6.286E+02, 6.344E+02,
     x 6.216E+02, 6.257E+02, 6.352E+02, 6.314E+02, 6.386E+02,
     x 6.456E+02, 6.437E+02, 6.412E+02, 6.558E+02, 6.580E+02,
     x 6.588E+02, 6.624E+02, 6.667E+02, 6.576E+02, 6.738E+02,
     x 6.790E+02, 6.784E+02, 6.770E+02, 6.821E+02, 6.836E+02,
     x 6.948E+02, 6.968E+02, 7.023E+02, 7.056E+02, 7.029E+02,
     x 7.073E+02, 7.120E+02, 7.146E+02, 7.225E+02, 7.273E+02,
     x 7.331E+02, 7.361E+02, 7.111E+02, 7.343E+02, 7.501E+02/
      data suna5/
     x 7.481E+02, 7.528E+02, 7.534E+02, 7.575E+02, 7.581E+02,
     x 7.542E+02, 7.659E+02, 7.664E+02, 7.705E+02, 7.777E+02,
     x 7.758E+02, 7.770E+02, 7.790E+02, 7.788E+02, 7.943E+02,
     x 7.930E+02, 7.993E+02, 8.019E+02, 8.020E+02, 8.041E+02,
     x 8.177E+02, 8.214E+02, 8.165E+02, 8.175E+02, 8.098E+02,
     x 8.344E+02, 8.344E+02, 8.425E+02, 8.404E+02, 8.362E+02,
     x 8.397E+02, 8.062E+02, 8.536E+02, 8.613E+02, 8.530E+02,
     x 8.651E+02, 8.665E+02, 8.711E+02, 8.656E+02, 8.839E+02,
     x 8.507E+02, 8.680E+02, 8.664E+02, 8.829E+02, 9.027E+02,
     x 8.910E+02, 9.055E+02, 9.121E+02, 8.988E+02, 9.047E+02,
     x 8.816E+02, 9.216E+02, 9.252E+02, 9.343E+02, 9.021E+02,
     x 9.046E+02, 9.377E+02, 9.332E+02, 9.518E+02, 9.318E+02,
     x 9.240E+02, 9.509E+02, 9.519E+02, 9.509E+02, 9.081E+02,
     x 9.639E+02, 9.651E+02, 9.669E+02, 9.665E+02, 9.818E+02,
     x 9.787E+02, 9.492E+02, 9.725E+02, 9.931E+02, 9.861E+02,
     x 9.638E+02, 9.651E+02, 7.464E+02, 9.908E+02, 1.005E+03,
     x 9.912E+02, 1.003E+03, 9.966E+02, 1.018E+03, 1.006E+03,
     x 7.393E+02, 9.979E+02, 9.876E+02, 9.120E+02, 1.036E+03,
     x 1.015E+03, 1.037E+03, 1.026E+03, 1.035E+03, 1.042E+03,
     x 1.046E+03, 1.016E+03, 1.059E+03, 1.059E+03, 1.042E+03/
      data suna6/
     x 1.033E+03, 1.063E+03, 1.061E+03, 1.076E+03, 1.082E+03,
     x 1.086E+03, 1.074E+03, 1.070E+03, 1.064E+03, 1.074E+03,
     x 1.059E+03, 1.079E+03, 1.105E+03, 1.108E+03, 1.115E+03,
     x 1.116E+03, 1.115E+03, 1.106E+03, 1.103E+03, 1.115E+03,
     x 1.136E+03, 1.099E+03, 1.132E+03, 1.134E+03, 1.143E+03,
     x 1.125E+03, 1.152E+03, 1.153E+03, 1.149E+03, 1.116E+03,
     x 1.123E+03, 1.155E+03, 1.163E+03, 1.174E+03, 1.177E+03,
     x 1.182E+03, 1.170E+03, 1.188E+03, 1.161E+03, 1.195E+03,
     x 1.181E+03, 1.182E+03, 1.203E+03, 1.182E+03, 1.209E+03,
     x 1.155E+03, 1.216E+03, 1.185E+03, 1.189E+03, 1.209E+03,
     x 1.213E+03, 1.224E+03, 1.170E+03, 1.202E+03, 1.246E+03,
     x 1.248E+03, 1.198E+03, 1.240E+03, 1.251E+03, 1.224E+03,
     x 1.248E+03, 1.249E+03, 1.265E+03, 1.277E+03, 1.255E+03,
     x 1.271E+03, 1.277E+03, 1.243E+03, 1.276E+03, 1.286E+03,
     x 1.282E+03, 1.269E+03, 1.297E+03, 1.288E+03, 1.249E+03,
     x 1.283E+03, 1.288E+03, 1.286E+03, 1.320E+03, 1.303E+03,
     x 1.316E+03, 1.337E+03, 1.306E+03, 1.330E+03, 1.302E+03,
     x 1.333E+03, 1.292E+03, 1.342E+03, 1.348E+03, 1.355E+03,
     x 1.352E+03, 1.368E+03, 1.316E+03, 1.354E+03, 1.357E+03,
     x 1.317E+03, 1.331E+03, 1.384E+03, 1.364E+03, 1.339E+03/
      data suna7/
     x 1.391E+03, 1.356E+03, 1.377E+03, 1.382E+03, 1.409E+03,
     x 1.381E+03, 1.417E+03, 1.369E+03, 1.402E+03, 1.423E+03,
     x 1.406E+03, 1.407E+03, 1.392E+03, 1.403E+03, 1.399E+03,
     x 1.436E+03, 1.406E+03, 1.432E+03, 1.435E+03, 1.433E+03,
     x 1.443E+03, 1.438E+03, 1.442E+03, 1.438E+03, 1.464E+03,
     x 1.472E+03, 1.452E+03, 1.477E+03, 1.474E+03, 1.427E+03,
     x 1.487E+03, 1.481E+03, 1.480E+03, 1.489E+03, 1.484E+03,
     x 1.494E+03, 1.501E+03, 1.503E+03, 1.506E+03, 1.515E+03,
     x 1.517E+03, 1.519E+03, 1.529E+03, 1.517E+03, 1.504E+03,
     x 1.535E+03, 1.534E+03, 1.556E+03, 1.552E+03, 1.565E+03,
     x 1.503E+03, 1.565E+03, 1.542E+03, 1.555E+03, 1.566E+03,
     x 1.586E+03, 1.567E+03, 1.589E+03, 1.506E+03, 1.573E+03,
     x 1.543E+03, 1.432E+03, 1.300E+03, 1.560E+03, 1.590E+03,
     x 1.592E+03, 1.611E+03, 1.591E+03, 1.623E+03, 1.554E+03,
     x 1.581E+03, 1.607E+03, 1.617E+03, 1.595E+03, 1.617E+03,
     x 1.604E+03, 1.606E+03, 1.615E+03, 1.655E+03, 1.628E+03,
     x 1.619E+03, 1.584E+03, 1.637E+03, 1.658E+03, 1.690E+03,
     x 1.656E+03, 1.611E+03, 1.676E+03, 1.650E+03, 1.625E+03,
     x 1.700E+03, 1.587E+03, 1.690E+03, 1.640E+03, 1.703E+03,
     x 1.723E+03, 1.686E+03, 1.694E+03, 1.679E+03, 1.604E+03/
      data suna8/
     x 1.664E+03, 1.653E+03, 1.651E+03, 1.733E+03, 1.692E+03,
     x 1.763E+03, 1.725E+03, 1.706E+03, 1.747E+03, 1.731E+03,
     x 1.642E+03, 1.576E+03, 1.684E+03, 1.714E+03, 1.669E+03,
     x 1.730E+03, 1.698E+03, 1.757E+03, 1.759E+03, 1.674E+03,
     x 1.761E+03, 1.756E+03, 1.749E+03, 1.802E+03, 1.728E+03,
     x 1.784E+03, 1.794E+03, 1.792E+03, 1.797E+03, 1.732E+03,
     x 1.717E+03, 1.740E+03, 1.719E+03, 1.777E+03, 1.787E+03,
     x 1.754E+03, 1.777E+03, 1.820E+03, 1.806E+03, 1.792E+03,
     x 1.775E+03, 1.812E+03, 1.808E+03, 1.746E+03, 1.819E+03,
     x 1.755E+03, 1.813E+03, 1.828E+03, 1.630E+03, 1.725E+03,
     x 1.808E+03, 1.848E+03, 1.816E+03, 1.754E+03, 1.821E+03,
     x 1.847E+03, 1.858E+03, 1.854E+03, 1.880E+03, 1.878E+03,
     x 1.865E+03, 1.815E+03, 1.851E+03, 1.816E+03, 1.769E+03,
     x 1.851E+03, 1.857E+03, 1.876E+03, 1.888E+03, 1.799E+03,
     x 1.885E+03, 1.875E+03, 1.874E+03, 1.918E+03, 1.877E+03,
     x 1.783E+03, 1.808E+03, 1.836E+03, 1.870E+03, 1.820E+03,
     x 1.857E+03, 1.902E+03, 1.820E+03, 1.690E+03, 1.820E+03,
     x 1.901E+03, 1.836E+03, 1.867E+03, 1.912E+03, 1.836E+03,
     x 1.719E+03, 1.950E+03, 1.815E+03, 1.842E+03, 1.808E+03,
     x 1.795E+03, 1.891E+03, 1.809E+03, 1.838E+03, 1.894E+03/
      data suna9/
     x 1.836E+03, 1.916E+03, 1.932E+03, 1.847E+03, 1.803E+03,
     x 1.903E+03, 1.932E+03, 1.861E+03, 1.860E+03, 1.889E+03,
     x 1.847E+03, 1.878E+03, 1.845E+03, 1.809E+03, 1.946E+03,
     x 1.844E+03, 1.806E+03, 2.002E+03, 1.859E+03, 1.986E+03,
     x 1.845E+03, 1.919E+03, 1.924E+03, 1.842E+03, 1.780E+03,
     x 1.720E+03, 1.783E+03, 1.889E+03, 1.938E+03, 1.934E+03,
     x 1.983E+03, 1.855E+03, 1.894E+03, 2.023E+03, 1.958E+03,
     x 1.812E+03, 1.755E+03, 1.850E+03, 1.548E+03, 1.887E+03,
     x 1.909E+03, 2.053E+03, 2.034E+03, 1.881E+03, 1.953E+03,
     x 2.002E+03, 1.816E+03, 2.010E+03, 1.808E+03, 1.603E+03,
     x 1.781E+03, 1.894E+03, 1.924E+03, 1.972E+03, 2.026E+03,
     x 1.829E+03, 1.748E+03, 1.995E+03, 1.852E+03, 2.018E+03,
     x 1.756E+03, 1.961E+03, 1.736E+03, 1.845E+03, 1.417E+03,
     x 1.929E+03, 1.576E+03, 1.462E+03, 1.877E+03, 1.995E+03,
     x 1.866E+03, 1.819E+03, 1.753E+03, 1.876E+03, 1.938E+03,
     x 1.791E+03, 1.970E+03, 2.041E+03, 1.936E+03, 1.951E+03,
     x 1.861E+03, 2.053E+03, 2.017E+03, 1.912E+03, 1.756E+03,
     x 1.926E+03, 1.930E+03, 1.956E+03, 2.066E+03, 2.062E+03,
     x 1.879E+03, 1.970E+03, 1.828E+03, 2.013E+03, 1.919E+03,
     x 1.878E+03, 1.815E+03, 1.770E+03, 1.928E+03, 1.869E+03/
      data suna10/
     x 1.906E+03, 1.926E+03, 2.052E+03, 1.658E+03, 1.876E+03,
     x 2.071E+03, 1.884E+03, 2.084E+03, 1.906E+03, 2.075E+03,
     x 2.099E+03, 2.101E+03, 1.831E+03, 1.909E+03, 2.066E+03,
     x 1.872E+03, 1.616E+03, 2.043E+03, 1.779E+03, 2.042E+03,
     x 2.056E+03, 2.109E+03, 1.696E+03, 1.871E+03, 1.974E+03,
     x 1.883E+03, 1.696E+03, 1.869E+03, 1.331E+03, 1.853E+03,
     x 1.969E+03, 1.913E+03, 1.937E+03, 1.945E+03, 2.068E+03,
     x 2.026E+03, 2.042E+03, 2.099E+03, 2.080E+03, 2.010E+03,
     x 2.026E+03, 2.019E+03, 2.111E+03, 2.005E+03, 1.955E+03,
     x 2.077E+03, 2.131E+03, 2.120E+03, 2.062E+03, 1.950E+03,
     x 2.105E+03, 2.017E+03, 2.124E+03, 2.024E+03, 2.035E+03,
     x 1.944E+03, 1.980E+03, 1.913E+03, 2.107E+03, 2.098E+03,
     x 1.935E+03, 2.043E+03, 1.933E+03, 1.904E+03, 1.972E+03,
     x 2.107E+03, 2.028E+03, 1.991E+03, 1.936E+03, 2.133E+03,
     x 2.072E+03, 1.811E+03, 1.973E+03, 2.097E+03, 1.980E+03,
     x 1.874E+03, 2.017E+03, 2.165E+03, 1.884E+03, 2.009E+03,
     x 2.020E+03, 2.092E+03, 1.918E+03, 2.094E+03, 2.027E+03,
     x 2.076E+03, 1.984E+03, 2.055E+03, 2.050E+03, 2.137E+03,
     x 2.006E+03, 1.833E+03, 2.132E+03, 2.106E+03, 2.185E+03,
     x 2.028E+03, 2.142E+03, 1.934E+03, 2.169E+03, 1.953E+03/
      data suna11/
     x 2.039E+03, 2.061E+03, 1.793E+03, 1.885E+03, 1.747E+03,
     x 2.017E+03, 2.035E+03, 2.112E+03, 2.142E+03, 2.243E+03,
     x 2.221E+03, 2.120E+03, 1.878E+03, 1.934E+03, 2.116E+03,
     x 1.906E+03, 2.136E+03, 2.104E+03, 1.953E+03, 1.973E+03,
     x 1.911E+03, 1.750E+03, 1.617E+03, 1.920E+03, 1.983E+03,
     x 1.721E+03, 2.108E+03, 1.732E+03, 1.906E+03, 1.908E+03,
     x 1.880E+03, 2.176E+03, 1.603E+03, 2.084E+03, 1.774E+03,
     x 1.636E+03, 1.683E+03, 1.791E+03, 1.825E+03, 1.717E+03,
     x 1.089E+03, 1.958E+03, 1.790E+03, 1.805E+03, 1.893E+03,
     x 2.152E+03, 2.076E+03, 2.005E+03, 1.769E+03, 1.756E+03,
     x 1.922E+03, 1.636E+03, 1.345E+03, 2.076E+03, 1.949E+03,
     x 1.767E+03, 1.248E+03, 2.033E+03, 2.035E+03, 1.293E+03,
     x 8.340E+02, 1.090E+03, 1.070E+03, 1.250E+03, 1.554E+03,
     x 1.239E+03, 1.544E+03, 1.778E+03, 1.655E+03, 1.591E+03,
     x 1.265E+03, 1.735E+03, 1.885E+03, 1.466E+03, 2.014E+03,
     x 1.641E+03, 1.438E+03, 1.756E+03, 1.773E+03, 1.600E+03,
     x 1.472E+03, 1.820E+03, 1.684E+03, 1.408E+03, 2.048E+03,
     x 1.872E+03, 1.674E+03, 1.899E+03, 1.957E+03, 1.712E+03,
     x 1.577E+03, 1.495E+03, 1.881E+03, 1.835E+03, 1.621E+03,
     x 1.755E+03, 1.900E+03, 1.680E+03, 1.510E+03, 1.783E+03/
      data suna12/
     x 1.963E+03, 1.928E+03, 1.808E+03, 1.682E+03, 1.946E+03,
     x 1.866E+03, 1.578E+03, 1.785E+03, 1.888E+03, 1.511E+03,
     x 1.757E+03, 1.689E+03, 1.815E+03, 1.937E+03, 1.991E+03,
     x 1.899E+03, 1.874E+03, 1.790E+03, 1.514E+03, 1.315E+03,
     x 1.601E+03, 2.041E+03, 1.915E+03, 1.828E+03, 1.544E+03,
     x 1.858E+03, 1.254E+03, 1.827E+03, 1.555E+03, 1.486E+03,
     x 1.090E+03, 1.964E+03, 1.606E+03, 1.710E+03, 1.978E+03,
     x 1.558E+03, 1.496E+03, 1.543E+03, 2.011E+03, 1.539E+03,
     x 1.337E+03, 1.785E+03, 1.604E+03, 1.812E+03, 1.572E+03,
     x 1.827E+03, 1.640E+03, 1.892E+03, 1.343E+03, 1.879E+03,
     x 1.630E+03, 1.567E+03, 1.926E+03, 1.735E+03, 1.578E+03,
     x 1.458E+03, 1.654E+03, 1.344E+03, 8.423E+02, 3.617E+02,
     x 3.343E+02, 8.409E+02, 1.119E+03, 1.294E+03, 1.577E+03,
     x 1.441E+03, 1.176E+03, 1.069E+03, 1.032E+03, 8.098E+02,
     x 3.440E+02, 2.175E+02, 5.348E+02, 9.949E+02, 1.178E+03,
     x 9.734E+02, 1.446E+03, 1.447E+03, 1.469E+03, 1.529E+03,
     x 1.166E+03, 1.352E+03, 1.262E+03, 1.264E+03, 1.384E+03,
     x 9.118E+02, 7.075E+02, 1.190E+03, 9.680E+02, 7.396E+02,
     x 1.222E+03, 5.767E+02, 1.037E+03, 1.110E+03, 1.229E+03,
     x 4.365E+02, 8.674E+02, 9.855E+02, 9.103E+02, 1.227E+03/
      data suna13/
     x 1.130E+03, 9.161E+02, 5.589E+02, 8.018E+02, 4.944E+02,
     x 9.155E+02, 6.693E+02, 6.902E+02, 7.077E+02, 1.250E+03,
     x 7.395E+02, 1.074E+03, 1.484E+03, 1.015E+03, 1.609E+03,
     x 1.343E+03, 6.993E+02, 1.013E+03, 1.211E+03, 9.973E+02,
     x 1.059E+03, 1.662E+03, 1.799E+03, 1.471E+03, 1.410E+03,
     x 1.429E+03, 1.188E+03, 9.182E+02, 8.927E+02, 1.209E+03,
     x 6.029E+02, 1.690E+03, 1.214E+03, 4.508E+02, 7.458E+02,
     x 8.262E+02, 1.450E+03, 9.434E+02, 5.208E+02, 8.201E+02,
     x 1.310E+03, 8.872E+02, 1.197E+03, 6.474E+02, 8.157E+02,
     x 1.482E+03, 1.688E+03, 1.209E+03, 8.614E+02, 7.908E+02,
     x 1.400E+03, 1.437E+03, 1.381E+03, 1.234E+03, 1.481E+03,
     x 1.138E+03, 1.076E+03, 1.232E+03, 1.178E+03, 1.079E+03,
     x 1.414E+03, 1.491E+03, 1.019E+03, 1.224E+03, 1.199E+03,
     x 1.358E+03, 1.537E+03, 1.496E+03, 1.399E+03, 1.138E+03,
     x 8.257E+02, 9.206E+02, 9.144E+02, 1.324E+03, 1.110E+03,
     x 9.981E+02, 8.684E+02, 8.723E+02, 1.455E+03, 1.083E+03,
     x 1.092E+03, 4.441E+02, 1.370E+03, 1.317E+03, 1.096E+03,
     x 6.018E+02, 1.041E+03, 9.447E+02, 1.334E+03, 1.366E+03,
     x 1.038E+03, 1.085E+03, 1.101E+03, 7.879E+02, 3.731E+02,
     x 7.948E+02, 5.462E+02, 9.376E+02, 9.025E+02, 8.996E+02/
      data suna14/
     x 4.723E+02, 9.750E+02, 5.479E+02/
       data sunbp/
     x  8.996E+02/
C         SOLAR SPECTRUM FROM 28000 CM-1, on       IN STEPS OF 10 CM-1  solr 230
       data sunb28/
     x 4.723E+02, 7.307E+02, 9.750E+02, 5.884E+02, 5.479E+02,
     x 1.221E+03, 1.299E+03, 1.043E+03, 1.055E+03, 1.084E+03,
     x 1.303E+03, 1.172E+03, 9.718E+02, 8.814E+02, 1.114E+03,
     x 1.322E+03, 1.327E+03, 1.074E+03, 1.033E+03, 1.283E+03,
     x 1.312E+03, 1.426E+03, 1.095E+03, 1.117E+03, 1.368E+03,
     x 1.250E+03, 9.794E+02, 1.036E+03, 1.148E+03, 1.200E+03,
     x 1.113E+03, 1.255E+03, 1.172E+03, 1.271E+03, 9.808E+02,
     x 6.850E+02, 8.527E+02, 6.627E+02, 1.098E+03, 9.834E+02,
     x 1.178E+03, 1.324E+03, 1.385E+03, 1.192E+03, 8.303E+02,
     x 7.774E+02, 9.631E+02, 1.114E+03, 1.002E+03, 1.327E+03,
     x 1.131E+03, 1.049E+03, 1.228E+03, 1.377E+03, 1.285E+03,
     x 1.328E+03, 1.162E+03, 1.338E+03, 8.279E+02, 7.464E+02,
     x 9.936E+02, 1.081E+03, 8.436E+02, 1.196E+03, 6.786E+02,
     x 1.130E+03, 1.229E+03, 1.533E+03, 1.263E+03, 1.081E+03,
     x 1.073E+03, 1.455E+03, 1.270E+03, 1.145E+03, 1.073E+03,
     x 7.865E+02, 5.761E+02, 8.286E+02, 1.057E+03, 1.069E+03,
     x 1.196E+03, 1.156E+03, 1.112E+03, 1.052E+03, 8.311E+02,
     x 1.022E+03, 1.246E+03, 1.103E+03, 8.610E+02, 1.023E+03,
     x 9.608E+02, 1.119E+03, 1.325E+03, 1.345E+03, 1.105E+03,
     x 7.311E+02, 9.066E+02, 1.232E+03, 1.194E+03, 1.344E+03/
       data sunb29/
     x 9.466E+02, 8.467E+02, 9.294E+02, 5.461E+02, 7.089E+02,
     x 4.778E+02, 6.347E+02, 1.016E+03, 9.104E+02, 9.388E+02,
     x 9.942E+02, 9.058E+02, 8.156E+02, 8.950E+02, 1.041E+03,
     x 1.155E+03, 8.534E+02, 7.471E+02, 7.820E+02, 7.459E+02,
     x 6.686E+02, 8.443E+02, 8.835E+02, 1.095E+03, 1.100E+03,
     x 9.141E+02, 8.032E+02, 6.680E+02, 6.619E+02, 6.336E+02,
     x 9.281E+02, 9.573E+02, 7.457E+02, 9.469E+02, 7.137E+02,
     x 8.613E+02, 8.513E+02, 9.152E+02, 7.563E+02, 1.024E+03,
     x 1.291E+03, 1.003E+03, 1.075E+03, 1.034E+03, 9.225E+02,
     x 7.860E+02, 6.110E+02, 5.797E+02, 8.025E+02, 1.148E+03,
     x 8.619E+02, 8.196E+02, 9.600E+02, 9.581E+02, 8.310E+02,
     x 8.301E+02, 9.588E+02, 5.076E+02, 7.365E+02, 9.452E+02,
     x 1.077E+03, 1.176E+03, 8.990E+02, 7.418E+02, 5.454E+02,
     x 4.866E+02, 5.291E+02, 4.850E+02, 7.172E+02, 8.324E+02,
     x 8.452E+02, 1.061E+03, 9.305E+02, 7.513E+02, 3.201E+02,
     x 2.855E+02, 3.755E+02, 7.654E+02, 1.097E+03, 1.090E+03,
     x 1.046E+03, 1.012E+03, 1.112E+03, 1.080E+03, 7.497E+02,
     x 6.782E+02, 9.717E+02, 1.008E+03, 1.022E+03, 1.083E+03,
     x 9.942E+02, 9.176E+02, 1.098E+03, 9.940E+02, 9.572E+02,
     x 9.689E+02, 6.889E+02, 8.136E+02, 1.067E+03, 1.092E+03/
       data sunb30/
     x 1.005E+03, 1.203E+03, 8.766E+02, 8.832E+02, 1.030E+03,
     x 9.976E+02, 1.252E+03, 1.220E+03, 8.034E+02, 8.511E+02,
     x 1.086E+03, 1.049E+03, 1.082E+03, 1.095E+03, 1.026E+03,
     x 9.456E+02, 7.147E+02, 8.559E+02, 9.577E+02, 9.508E+02,
     x 9.323E+02, 1.043E+03, 9.213E+02, 7.036E+02, 7.103E+02,
     x 1.144E+03, 9.601E+02, 8.039E+02, 1.264E+03, 1.420E+03,
     x 1.052E+03, 1.201E+03, 9.361E+02, 8.267E+02, 1.146E+03,
     x 1.367E+03, 1.194E+03, 1.086E+03, 1.052E+03, 1.203E+03,
     x 8.902E+02, 8.735E+02, 8.506E+02, 9.709E+02, 8.830E+02,
     x 8.481E+02, 9.624E+02, 1.010E+03, 9.083E+02, 9.229E+02,
     x 9.805E+02, 9.800E+02, 1.073E+03, 9.916E+02, 1.239E+03,
     x 1.262E+03, 8.492E+02, 9.670E+02, 1.073E+03, 1.154E+03,
     x 1.057E+03, 1.162E+03, 1.021E+03, 1.105E+03, 1.242E+03,
     x 1.095E+03, 9.314E+02, 1.104E+03, 1.109E+03, 1.118E+03,
     x 1.160E+03, 1.156E+03, 6.896E+02, 7.527E+02, 9.671E+02,
     x 8.432E+02, 8.630E+02, 7.743E+02, 7.165E+02, 9.617E+02,
     x 1.094E+03, 9.818E+02, 7.303E+02, 9.545E+02, 8.400E+02,
     x 8.704E+02, 5.197E+02, 6.954E+02, 7.334E+02, 5.750E+02,
     x 6.765E+02, 5.106E+02, 6.981E+02, 9.480E+02, 6.833E+02,
     x 4.563E+02, 5.365E+02, 4.724E+02, 4.482E+02, 4.881E+02/
       data sunb31/
     x 7.929E+02, 8.188E+02, 6.803E+02, 6.911E+02, 8.867E+02,
     x 7.033E+02, 9.557E+02, 6.845E+02, 8.093E+02, 9.897E+02,
     x 4.646E+02, 4.673E+02, 7.397E+02, 5.817E+02, 5.007E+02,
     x 6.725E+02, 9.806E+02, 1.053E+03, 1.009E+03, 9.891E+02,
     x 1.060E+03, 6.544E+02, 7.652E+02, 8.896E+02, 6.357E+02,
     x 1.113E+03, 1.035E+03, 6.340E+02, 7.373E+02, 7.246E+02,
     x 5.929E+02, 4.338E+02, 5.070E+02, 6.819E+02, 1.224E+03,
     x 1.093E+03, 8.175E+02, 6.173E+02, 7.353E+02, 5.724E+02,
     x 5.779E+02, 5.105E+02, 4.076E+02, 5.547E+02, 4.919E+02,
     x 8.011E+02, 9.214E+02, 1.222E+03, 1.028E+03, 9.416E+02,
     x 8.795E+02, 1.119E+03, 1.041E+03, 1.076E+03, 7.330E+02,
     x 6.037E+02, 7.115E+02, 6.149E+02, 6.221E+02, 6.263E+02,
     x 8.826E+02, 5.847E+02, 4.323E+02, 5.495E+02, 4.596E+02,
     x 4.079E+02, 4.476E+02, 6.052E+02, 8.641E+02, 8.752E+02,
     x 1.127E+03, 8.677E+02, 8.078E+02, 9.080E+02, 9.689E+02,
     x 9.656E+02, 5.116E+02, 7.914E+02, 6.190E+02, 3.012E+02,
     x 2.827E+02, 6.716E+02, 9.715E+02, 8.580E+02, 8.081E+02,
     x 1.009E+03, 1.073E+03, 7.071E+02, 8.879E+02, 6.372E+02,
     x 6.558E+02, 8.097E+02, 7.444E+02, 6.243E+02, 8.926E+02,
     x 7.364E+02, 8.249E+02, 9.546E+02, 7.206E+02, 7.390E+02/
       data sunb32/
     x 9.193E+02, 6.324E+02, 5.667E+02, 8.324E+02, 5.638E+02,
     x 4.976E+02, 6.398E+02, 7.399E+02, 9.958E+02, 9.148E+02,
     x 8.374E+02, 1.114E+03, 1.120E+03, 9.162E+02, 8.862E+02,
     x 1.159E+03, 1.084E+03, 9.765E+02, 9.448E+02, 7.114E+02,
     x 8.492E+02, 8.673E+02, 8.416E+02, 3.078E+02, 2.428E+02,
     x 2.338E+02, 5.766E+02, 7.877E+02, 3.917E+02, 6.906E+02,
     x 8.034E+02, 5.529E+02, 2.938E+02, 5.373E+02, 4.774E+02,
     x 6.448E+02, 7.645E+02, 6.406E+02, 6.388E+02, 8.243E+02,
     x 9.314E+02, 9.477E+02, 5.848E+02, 6.098E+02, 5.535E+02,
     x 7.758E+02, 9.040E+02, 6.657E+02, 6.863E+02, 8.862E+02,
     x 5.535E+02, 6.466E+02, 7.955E+02, 7.542E+02, 6.192E+02,
     x 8.060E+02, 8.032E+02, 8.178E+02, 6.505E+02, 4.297E+02,
     x 4.768E+02, 7.404E+02, 5.348E+02, 5.947E+02, 8.096E+02,
     x 8.337E+02, 7.299E+02, 5.306E+02, 3.254E+02, 3.306E+02,
     x 4.222E+02, 7.681E+02, 6.976E+02, 5.602E+02, 7.177E+02,
     x 1.030E+03, 9.262E+02, 5.910E+02, 8.649E+02, 6.492E+02,
     x 3.334E+02, 5.729E+02, 8.771E+02, 7.632E+02, 7.785E+02,
     x 7.290E+02, 4.481E+02, 5.477E+02, 6.253E+02, 7.471E+02,
     x 6.017E+02, 2.669E+02, 6.499E+02, 8.625E+02, 8.093E+02,
     x 7.721E+02, 8.791E+02, 7.804E+02, 5.072E+02, 6.664E+02/
       data sunb33/
     x 6.512E+02, 8.403E+02, 9.049E+02, 4.740E+02, 3.550E+02,
     x 5.746E+02, 6.007E+02, 6.982E+02, 4.289E+02, 1.362E+02,
     x 2.293E+02, 3.200E+02, 4.067E+02, 4.047E+02, 5.742E+02,
     x 5.889E+02, 5.622E+02, 5.929E+02, 6.265E+02, 4.265E+02,
     x 6.554E+02, 5.964E+02, 3.982E+02, 3.329E+02, 3.604E+02,
     x 4.757E+02, 6.092E+02, 6.312E+02, 3.872E+02, 2.545E+02,
     x 3.628E+02, 2.490E+02, 3.047E+02, 3.275E+02, 6.364E+02,
     x 7.466E+02, 6.464E+02, 5.814E+02, 2.909E+02, 3.672E+02,
     x 6.053E+02, 5.797E+02, 6.764E+02, 7.616E+02, 6.631E+02,
     x 5.694E+02, 3.898E+02, 4.810E+02, 3.566E+02, 3.084E+02,
     x 2.731E+02, 3.638E+02, 4.390E+02, 3.377E+02, 5.732E+02,
     x 5.624E+02, 6.655E+02, 8.900E+02, 8.482E+02, 5.802E+02,
     x 7.065E+02, 6.751E+02, 3.118E+02, 4.102E+02, 6.121E+02,
     x 4.587E+02, 2.779E+02, 5.013E+02, 5.770E+02, 3.075E+02,
     x 2.815E+02, 3.541E+02, 4.788E+02, 7.022E+02, 7.962E+02,
     x 7.149E+02, 5.814E+02, 5.677E+02, 6.634E+02, 7.918E+02,
     x 5.205E+02, 4.795E+02, 6.782E+02, 6.939E+02, 3.481E+02,
     x 5.684E+02, 7.898E+02, 6.787E+02, 6.270E+02, 4.751E+02,
     x 3.496E+02, 2.511E+02, 4.639E+02, 8.082E+02, 6.433E+02,
     x 4.836E+02, 5.887E+02, 7.150E+02, 4.983E+02, 4.620E+02/
       data sunb34/
     x 6.424E+02, 4.767E+02, 5.043E+02, 4.475E+02, 2.432E+02,
     x 5.912E+02, 8.238E+02, 6.528E+02, 5.938E+02, 7.335E+02,
     x 7.125E+02, 6.756E+02, 7.295E+02, 3.214E+02, 4.518E+02,
     x 6.104E+02, 4.200E+02, 4.726E+02, 5.755E+02, 4.384E+02,
     x 6.165E+02, 6.350E+02, 6.638E+02, 5.852E+02, 6.608E+02,
     x 5.999E+02, 5.353E+02, 6.390E+02, 6.318E+02, 5.234E+02,
     x 6.119E+02, 7.006E+02, 8.341E+02, 6.132E+02, 6.172E+02,
     x 6.230E+02, 6.262E+02, 6.092E+02, 6.350E+02, 6.846E+02,
     x 6.433E+02, 7.530E+02, 7.335E+02, 7.991E+02, 6.486E+02,
     x 5.480E+02, 7.545E+02, 7.883E+02, 6.352E+02, 5.801E+02,
     x 5.772E+02, 6.078E+02, 6.262E+02, 5.396E+02, 4.506E+02,
     x 4.592E+02, 4.282E+02, 4.705E+02, 4.353E+02, 4.499E+02,
     x 4.264E+02, 4.600E+02, 4.309E+02, 4.442E+02, 4.632E+02,
     x 5.287E+02, 3.885E+02, 2.990E+02, 2.056E+02, 1.065E+02,
     x 1.414E+02, 2.616E+02, 2.578E+02, 3.539E+02, 4.334E+02,
     x 3.739E+02, 3.436E+02, 4.290E+02, 3.890E+02, 3.906E+02,
     x 4.638E+02, 5.308E+02, 5.009E+02, 4.362E+02, 2.950E+02,
     x 3.393E+02, 3.330E+02, 3.467E+02, 4.706E+02, 3.448E+02,
     x 4.612E+02, 3.843E+02, 3.560E+02, 3.814E+02, 3.535E+02,
     x 3.813E+02, 3.607E+02, 2.488E+02, 2.480E+02, 2.274E+02/
       data sunb35/
     x 1.853E+02, 1.513E+02, 1.332E+02, 9.803E+01, 6.600E+01,
     x 3.863E+01, 6.382E+01, 9.884E+01, 1.189E+02, 1.524E+02,
     x 1.737E+02, 2.209E+02, 2.440E+02, 2.683E+02, 3.736E+02,
     x 2.509E+02, 2.156E+02, 3.713E+02, 3.869E+02, 2.853E+02,
     x 3.242E+02, 3.834E+02, 3.798E+02, 4.131E+02, 3.832E+02,
     x 2.286E+02, 2.730E+02, 3.511E+02, 4.292E+02, 4.211E+02,
     x 3.454E+02, 4.147E+02, 3.574E+02, 4.194E+02, 3.434E+02,
     x 3.237E+02, 3.493E+02, 3.090E+02, 2.458E+02, 3.740E+02,
     x 3.929E+02, 3.115E+02, 3.088E+02, 3.174E+02, 3.130E+02,
     x 3.390E+02, 2.988E+02, 3.093E+02, 2.539E+02, 2.527E+02,
     x 2.664E+02, 2.861E+02, 2.828E+02, 1.967E+02, 1.824E+02,
     x 1.797E+02, 1.735E+02, 1.816E+02, 1.849E+02, 1.662E+02,
     x 1.509E+02, 1.262E+02, 1.183E+02, 1.065E+02, 8.554E+01,
     x 6.792E+01, 6.753E+01, 1.158E+02, 6.109E+01, 6.544E+01,
     x 8.330E+01, 8.782E+01, 7.918E+01, 6.214E+01, 6.080E+01,
     x 6.266E+01, 1.469E+02, 6.205E+01, 5.964E+01, 7.749E+01,
     x 8.809E+01, 9.970E+01, 1.045E+02, 1.193E+02, 1.355E+02,
     x 1.371E+02, 1.434E+02, 1.822E+02, 1.894E+02, 1.889E+02,
     x 1.924E+02, 1.772E+02, 1.570E+02, 2.054E+02, 1.777E+02,
     x 1.807E+02, 1.605E+02, 1.824E+02, 1.657E+02, 2.251E+02/
       data sunb36/
     x 2.327E+02, 2.381E+02, 2.708E+02, 2.980E+02, 3.231E+02,
     x 3.279E+02, 2.224E+02, 3.064E+02, 3.296E+02, 2.881E+02,
     x 2.107E+02, 2.368E+02, 2.635E+02, 2.529E+02, 2.853E+02,
     x 3.490E+02, 3.440E+02, 2.774E+02, 2.471E+02, 1.647E+02,
     x 2.153E+02, 3.088E+02, 2.650E+02, 3.023E+02, 3.932E+02,
     x 2.242E+02, 1.526E+02, 9.058E+01, 1.022E+02, 1.811E+02,
     x 1.725E+02, 2.120E+02, 2.921E+02, 2.295E+02, 1.577E+02,
     x 9.048E+01, 6.756E+01, 1.473E+02, 1.740E+02, 8.718E+01,
     x 8.694E+01, 1.954E+02, 1.891E+02, 1.200E+02, 9.306E+01,
     x 8.904E+01, 1.437E+02, 1.715E+02, 1.608E+02, 8.898E+01,
     x 1.630E+02, 2.050E+02, 1.024E+02, 1.480E+02, 2.410E+02,
     x 2.019E+02, 2.212E+02, 1.452E+02, 2.487E+02, 2.807E+02,
     x 2.865E+02, 2.247E+02, 3.997E+02, 3.571E+02, 2.692E+02,
     x 1.452E+02, 2.259E+02, 2.070E+02, 2.454E+02, 1.887E+02,
     x 1.770E+02, 1.888E+02, 2.313E+02, 2.161E+02, 1.173E+02,
     x 1.339E+02, 1.114E+02, 9.492E+01, 2.162E+02, 2.773E+02,
     x 2.926E+02, 2.486E+02, 2.112E+02, 1.413E+02, 2.863E+02,
     x 3.828E+02, 3.015E+02, 2.380E+02, 2.782E+02, 2.903E+02,
     x 2.644E+02, 2.371E+02, 3.088E+02, 2.537E+02, 2.438E+02,
     x 3.103E+02, 4.158E+02, 2.888E+02, 3.140E+02, 3.230E+02/
       data sunb37/
     x 2.324E+02, 2.597E+02, 4.204E+02, 3.497E+02, 2.100E+02,
     x 2.206E+02, 2.582E+02, 2.752E+02, 1.824E+02, 1.650E+02,
     x 2.595E+02, 3.270E+02, 2.729E+02, 2.736E+02, 2.905E+02,
     x 2.291E+02, 2.105E+02, 1.930E+02, 2.639E+02, 2.365E+02,
     x 2.859E+02, 2.960E+02, 3.094E+02, 2.516E+02, 2.430E+02,
     x 2.569E+02, 2.638E+02, 2.952E+02, 2.743E+02, 2.758E+02,
     x 2.442E+02, 2.151E+02, 1.959E+02, 2.634E+02, 2.231E+02,
     x 3.295E+02, 3.246E+02, 3.423E+02, 3.727E+02, 2.924E+02,
     x 1.848E+02, 1.582E+02, 3.369E+02, 3.404E+02, 2.279E+02,
     x 2.035E+02, 2.631E+02, 2.986E+02, 3.002E+02, 1.650E+02,
     x 2.580E+02, 3.174E+02, 2.675E+02, 1.575E+02, 2.723E+02,
     x 2.665E+02, 2.062E+02, 3.055E+02, 3.081E+02, 3.183E+02,
     x 2.498E+02, 2.730E+02, 3.208E+02, 3.144E+02, 2.570E+02,
     x 3.804E+02, 3.517E+02, 2.588E+02, 2.673E+02, 2.562E+02,
     x 2.265E+02, 2.192E+02, 2.313E+02, 2.182E+02, 1.727E+02,
     x 1.673E+02, 2.264E+02, 2.533E+02, 2.244E+02, 2.686E+02,
     x 2.825E+02, 2.943E+02, 4.015E+02, 3.946E+02, 2.607E+02,
     x 2.294E+02, 2.494E+02, 2.520E+02, 2.989E+02, 3.424E+02,
     x 2.885E+02, 2.201E+02, 2.354E+02, 1.626E+02, 1.870E+02,
     x 1.799E+02, 1.455E+02, 9.900E+01, 7.785E+01, 4.690E+01/
       data sunb38/
     x 6.673E+01, 9.243E+01, 1.120E+02, 7.872E+01, 7.749E+01,
     x 1.171E+02, 1.079E+02, 7.699E+01, 9.402E+01, 1.601E+02,
     x 1.427E+02, 1.333E+02, 1.592E+02, 8.771E+01, 1.248E+02,
     x 1.132E+02, 1.604E+02, 1.223E+02, 9.166E+01, 5.903E+01,
     x 1.043E+02, 1.268E+02, 1.325E+02, 1.161E+02, 6.719E+01,
     x 6.188E+01, 7.759E+01, 5.210E+01, 5.469E+01, 8.284E+01,
     x 1.126E+02, 1.119E+02, 9.899E+01, 9.617E+01, 5.940E+01,
     x 4.957E+01, 6.893E+01, 7.913E+01, 1.223E+02, 1.158E+02,
     x 1.319E+02, 1.367E+02, 1.403E+02, 1.143E+02, 8.296E+01,
     x 4.790E+01, 3.587E+01, 3.985E+01, 5.498E+01, 9.074E+01,
     x 1.094E+02, 1.162E+02, 1.336E+02, 1.088E+02, 6.086E+01,
     x 8.670E+01, 1.454E+02, 1.095E+02, 1.045E+02, 1.513E+02,
     x 2.042E+02, 2.088E+02, 1.277E+02, 1.158E+02, 1.192E+02,
     x 8.113E+01, 4.853E+01, 8.537E+01, 9.469E+01, 1.613E+02,
     x 1.306E+02, 9.292E+01, 2.113E+02, 2.248E+02, 1.494E+02,
     x 1.847E+02, 1.350E+02, 1.554E+02, 1.136E+02, 1.137E+02,
     x 7.767E+01, 6.140E+01, 8.079E+01, 1.201E+02, 1.648E+02,
     x 1.611E+02, 1.263E+02, 1.463E+02, 1.316E+02, 1.305E+02,
     x 1.535E+02, 1.332E+02, 1.181E+02, 1.160E+02, 1.205E+02,
     x 1.091E+02, 1.472E+02, 9.759E+01, 9.587E+01, 7.440E+01/
       data sunb39/
     x 6.132E+01, 5.376E+01, 7.080E+01, 9.296E+01, 9.864E+01,
     x 9.333E+01, 1.038E+02, 1.124E+02, 8.671E+01, 7.249E+01,
     x 7.524E+01, 9.051E+01, 6.955E+01, 8.458E+01, 1.089E+02,
     x 7.730E+01, 6.622E+01, 1.074E+02, 9.343E+01, 6.964E+01,
     x 6.471E+01, 4.077E+01, 5.075E+01, 5.561E+01, 6.641E+01,
     x 5.738E+01, 4.643E+01, 5.427E+01, 6.169E+01, 6.394E+01,
     x 6.801E+01, 7.178E+01, 5.713E+01, 6.124E+01, 4.740E+01,
     x 5.490E+01, 7.146E+01, 5.670E+01, 4.983E+01, 6.257E+01,
     x 5.343E+01, 4.602E+01, 4.512E+01, 4.719E+01, 5.440E+01,
     x 4.979E+01, 5.357E+01, 5.711E+01, 6.005E+01, 6.494E+01,
     x 6.354E+01, 4.788E+01, 3.531E+01, 3.008E+01, 3.254E+01,
     x 3.297E+01, 4.335E+01, 5.053E+01, 4.781E+01, 4.467E+01,
     x 3.416E+01, 3.499E+01, 3.460E+01, 3.702E+01, 4.583E+01,
     x 4.882E+01, 5.417E+01, 4.968E+01, 2.968E+01, 4.014E+01,
     x 3.917E+01, 4.332E+01, 3.851E+01, 2.828E+01, 4.482E+01,
     x 4.298E+01, 3.424E+01, 4.771E+01, 5.953E+01, 5.586E+01,
     x 6.125E+01, 4.934E+01, 5.522E+01, 8.148E+01, 7.308E+01,
     x 7.220E+01, 5.951E+01, 3.937E+01, 3.264E+01, 4.977E+01,
     x 5.493E+01, 7.532E+01, 8.063E+01, 6.647E+01, 6.963E+01,
     x 6.494E+01, 5.004E+01, 3.595E+01, 6.268E+01, 8.553E+01/
       data sunb40/
     x 7.033E+01, 6.984E+01, 6.327E+01, 8.376E+01, 5.702E+01,
     x 5.997E+01, 8.204E+01, 7.714E+01, 5.719E+01, 5.831E+01,
     x 5.310E+01, 4.372E+01, 3.978E+01, 2.279E+01, 2.576E+01,
     x 2.985E+01, 3.677E+01, 3.598E+01, 2.853E+01, 4.663E+01,
     x 5.221E+01, 5.866E+01, 5.847E+01, 4.800E+01, 3.379E+01,
     x 2.635E+01, 2.549E+01, 4.272E+01, 5.345E+01, 6.363E+01,
     x 6.538E+01, 3.890E+01, 3.988E+01, 4.675E+01, 5.098E+01,
     x 6.308E+01, 6.027E+01, 5.922E+01, 7.170E+01, 6.274E+01,
     x 7.070E+01, 7.303E+01, 4.091E+01, 3.379E+01, 5.807E+01,
     x 6.927E+01, 5.520E+01, 5.414E+01, 5.721E+01, 5.917E+01,
     x 5.771E+01, 5.834E+01, 5.410E+01, 4.761E+01, 4.983E+01,
     x 4.398E+01, 5.200E+01, 4.853E+01, 4.831E+01, 3.738E+01,
     x 3.488E+01, 4.641E+01, 4.672E+01, 5.042E+01, 6.035E+01,
     x 4.702E+01, 5.267E+01, 6.404E+01, 5.457E+01, 4.488E+01,
     x 4.554E+01, 5.364E+01, 5.648E+01, 4.991E+01, 4.534E+01,
     x 5.214E+01, 4.615E+01, 4.126E+01, 4.606E+01, 5.340E+01,
     x 5.049E+01, 5.810E+01, 5.662E+01, 5.040E+01, 4.447E+01,
     x 5.945E+01, 6.204E+01, 6.440E+01, 7.076E+01, 6.801E+01,
     x 6.453E+01, 5.639E+01, 7.194E+01, 8.262E+01, 8.032E+01,
     x 8.192E+01, 7.683E+01, 5.740E+01, 4.849E+01, 5.695E+01/
       data sunb41/
     x 7.004E+01, 8.325E+01, 6.535E+01, 5.760E+01, 5.979E+01,
     x 4.129E+01, 4.559E+01, 7.416E+01, 6.805E+01, 5.684E+01,
     x 5.669E+01, 6.932E+01, 7.852E+01, 8.390E+01, 7.437E+01,
     x 6.699E+01, 6.336E+01, 6.529E+01, 8.337E+01, 1.016E+02,
     x 8.240E+01, 6.520E+01, 6.700E+01, 5.412E+01, 5.751E+01,
     x 6.016E+01, 6.405E+01, 9.069E+01, 8.427E+01, 7.009E+01,
     x 7.217E+01, 6.852E+01, 5.966E+01, 6.614E+01, 7.092E+01,
     x 5.283E+01, 6.359E+01, 6.666E+01, 5.566E+01, 5.018E+01,
     x 5.818E+01, 4.353E+01, 2.828E+01, 3.857E+01, 4.947E+01,
     x 3.664E+01, 2.677E+01, 2.381E+01, 3.891E+01, 4.741E+01,
     x 4.756E+01, 5.201E+01, 3.874E+01, 2.750E+01, 2.389E+01,
     x 3.764E+01, 2.772E+01, 1.949E+01, 2.911E+01, 4.473E+01,
     x 5.209E+01, 4.505E+01, 4.901E+01, 5.253E+01, 5.648E+01,
     x 5.081E+01, 3.386E+01, 2.724E+01, 4.186E+01, 5.291E+01,
     x 4.452E+01, 3.239E+01, 2.423E+01, 1.826E+01, 2.564E+01,
     x 3.298E+01, 4.888E+01, 5.434E+01, 5.567E+01, 5.675E+01,
     x 4.704E+01, 6.653E+01, 6.620E+01, 4.626E+01, 3.503E+01,
     x 2.300E+01, 3.395E+01, 4.159E+01, 5.696E+01, 4.901E+01,
     x 5.451E+01, 5.152E+01, 4.146E+01, 3.523E+01, 2.788E+01,
     x 2.237E+01, 2.435E+01, 2.210E+01, 2.665E+01, 3.033E+01/
       data sunb42/
     x 4.543E+01, 3.802E+01, 3.802E+01, 4.639E+01, 5.568E+01,
     x 6.891E+01, 6.886E+01, 5.384E+01, 4.380E+01, 3.915E+01,
     x 4.684E+01, 3.153E+01, 2.621E+01, 4.727E+01, 6.797E+01,
     x 7.098E+01, 6.730E+01, 4.831E+01, 5.303E+01, 5.613E+01,
     x 4.193E+01, 4.007E+01, 4.142E+01, 3.575E+01, 3.224E+01,
     x 4.854E+01, 5.012E+01, 3.184E+01, 3.865E+01, 4.437E+01,
     x 5.932E+01, 7.124E+01, 4.391E+01, 4.310E+01, 4.783E+01,
     x 2.796E+01, 2.843E+01, 3.001E+01, 3.585E+01, 5.868E+01,
     x 6.143E+01, 6.100E+01, 5.896E+01, 7.240E+01, 5.343E+01,
     x 3.287E+01, 4.779E+01, 5.533E+01, 6.003E+01, 6.984E+01,
     x 5.892E+01, 5.587E+01, 5.038E+01, 4.942E+01, 5.508E+01,
     x 4.430E+01, 3.297E+01, 2.217E+01, 2.583E+01, 3.766E+01,
     x 3.692E+01, 3.392E+01, 2.679E+01, 3.109E+01, 2.757E+01,
     x 1.905E+01, 1.979E+01, 3.297E+01, 4.655E+01, 5.850E+01,
     x 4.652E+01, 4.285E+01, 4.215E+01, 3.227E+01, 3.239E+01,
     x 2.571E+01, 2.405E+01, 3.049E+01, 4.313E+01, 5.705E+01,
     x 6.953E+01, 5.766E+01, 4.015E+01, 5.010E+01, 4.362E+01,
     x 2.514E+01, 3.416E+01, 3.551E+01, 3.003E+01, 5.209E+01,
     x 4.669E+01, 5.911E+01, 6.261E+01, 5.592E+01, 5.446E+01,
     x 3.030E+01, 4.004E+01, 3.951E+01, 2.704E+01, 3.528E+01/
       data sunb43/
     x 5.323E+01, 6.096E+01, 9.202E+01, 7.774E+01, 7.102E+01,
     x 5.317E+01, 2.750E+01, 4.025E+01, 3.670E+01, 2.134E+01,
     x 3.827E+01, 5.882E+01, 5.498E+01, 6.004E+01, 3.977E+01,
     x 5.771E+01, 4.743E+01, 6.119E+01, 7.485E+01, 6.184E+01,
     x 3.506E+01, 2.911E+01, 3.548E+01, 3.046E+01, 3.691E+01,
     x 2.749E+01, 2.887E+01, 4.594E+01, 4.037E+01, 4.265E+01,
     x 5.649E+01, 6.127E+01, 5.438E+01, 7.060E+01, 7.438E+01,
     x 8.315E+01, 8.065E+01, 5.365E+01, 5.595E+01, 4.963E+01,
     x 2.961E+01, 3.576E+01, 6.013E+01, 4.930E+01, 4.442E+01,
     x 3.588E+01, 3.359E+01, 3.969E+01, 3.579E+01, 3.384E+01,
     x 2.430E+01, 2.826E+01, 2.762E+01, 3.806E+01, 7.272E+01,
     x 6.462E+01, 5.012E+01, 4.714E+01, 4.730E+01, 5.157E+01,
     x 5.088E+01, 4.231E+01, 4.664E+01, 4.338E+01, 4.137E+01,
     x 3.942E+01, 3.813E+01, 4.770E+01, 5.770E+01, 6.231E+01,
     x 3.224E+01, 2.820E+01, 4.204E+01, 4.827E+01, 7.699E+01,
     x 6.611E+01, 5.685E+01, 4.623E+01, 4.532E+01, 4.889E+01,
     x 7.661E+01, 6.228E+01, 5.952E+01, 6.264E+01, 4.185E+01,
     x 3.536E+01, 5.393E+01, 4.118E+01, 5.004E+01, 4.045E+01,
     x 4.333E+01, 4.379E+01, 2.965E+01, 3.001E+01, 4.606E+01,
     x 4.010E+01, 2.881E+01, 3.455E+01, 3.934E+01, 3.770E+01/
       data sunb44/
     x 2.938E+01, 3.367E+01, 3.340E+01, 2.566E+01, 3.183E+01,
     x 3.162E+01, 2.478E+01, 3.215E+01, 3.363E+01, 2.624E+01,
     x 3.504E+01, 3.633E+01, 3.030E+01, 3.201E+01, 2.668E+01,
     x 3.637E+01, 3.941E+01, 3.577E+01, 4.169E+01, 5.614E+01,
     x 5.073E+01, 4.465E+01, 3.875E+01, 3.148E+01, 3.266E+01,
     x 6.066E+01, 6.799E+01, 3.955E+01, 4.832E+01, 6.501E+01,
     x 4.711E+01, 3.480E+01, 4.560E+01, 4.925E+01, 6.647E+01,
     x 4.426E+01, 3.266E+01, 4.314E+01, 7.540E+01, 6.597E+01,
     x 4.296E+01, 4.052E+01, 5.793E+01, 4.955E+01, 6.094E+01,
     x 3.462E+01, 5.233E+01, 8.252E+01, 7.884E+01, 7.087E+01,
     x 6.155E+01, 6.144E+01, 7.251E+01, 5.781E+01, 3.931E+01,
     x 4.298E+01, 4.888E+01, 4.960E+01, 5.528E+01, 5.185E+01,
     x 4.932E+01, 5.408E+01, 5.275E+01, 7.546E+01, 8.034E+01,
     x 5.917E+01, 4.151E+01, 4.530E+01, 6.505E+01, 7.201E+01,
     x 6.213E+01, 8.296E+01, 8.060E+01, 7.958E+01, 6.619E+01,
     x 7.017E+01, 9.766E+01, 8.298E+01, 7.428E+01, 6.492E+01,
     x 3.952E+01, 3.271E+01, 4.805E+01, 4.870E+01, 5.115E+01,
     x 5.532E+01, 6.026E+01, 4.920E+01, 3.894E+01, 3.988E+01,
     x 3.753E+01, 4.778E+01, 5.809E+01, 4.192E+01, 4.326E+01,
     x 6.624E+01, 4.864E+01, 3.308E+01, 4.704E+01, 5.424E+01/
       data sunb45/
     x 6.434E+01, 5.986E+01, 5.232E+01, 4.543E+01, 6.083E+01,
     x 4.994E+01, 4.206E+01, 2.320E+01, 2.840E+01, 2.462E+01,
     x 1.460E+01, 2.852E+01, 4.096E+01, 5.250E+01, 4.612E+01,
     x 3.413E+01, 4.473E+01, 6.456E+01, 4.948E+01, 2.776E+01,
     x 2.105E+01, 1.674E+01, 1.699E+01, 2.054E+01, 3.239E+01,
     x 5.209E+01, 3.747E+01, 2.592E+01, 3.097E+01, 5.269E+01,
     x 3.681E+01, 4.240E+01, 5.774E+01, 7.265E+01, 4.859E+01,
     x 4.961E+01, 6.878E+01, 7.324E+01, 6.756E+01, 5.846E+01,
     x 6.223E+01, 3.239E+01, 2.968E+01, 2.079E+01, 4.134E+01,
     x 5.264E+01, 6.048E+01, 5.847E+01, 6.668E+01, 5.796E+01,
     x 4.669E+01, 4.499E+01, 2.740E+01, 2.848E+01, 3.933E+01,
     x 3.957E+01, 6.347E+01, 5.697E+01, 6.873E+01, 7.476E+01,
     x 4.088E+01, 2.486E+01, 4.053E+01, 4.105E+01, 4.908E+01,
     x 5.522E+01, 3.211E+01, 2.727E+01, 5.185E+01, 5.068E+01,
     x 3.101E+01, 1.644E+01, 1.908E+01, 4.905E+01, 6.011E+01,
     x 5.899E+01, 3.755E+01, 3.774E+01, 2.810E+01, 5.464E+01,
     x 6.114E+01, 6.567E+01, 5.641E+01, 5.481E+01, 5.862E+01,
     x 4.896E+01, 6.007E+01, 5.418E+01, 4.678E+01, 3.213E+01,
     x 1.903E+01, 2.345E+01, 2.825E+01, 3.877E+01, 3.786E+01,
     x 3.208E+01, 2.447E+01, 2.185E+01, 2.461E+01, 3.285E+01/
       data sunb46/
     x 3.291E+01, 3.923E+01, 4.273E+01, 4.338E+01, 2.662E+01,
     x 3.966E+01, 4.354E+01, 3.147E+01, 2.219E+01, 2.073E+01,
     x 3.553E+01, 3.822E+01, 2.767E+01, 1.744E+01, 1.416E+01,
     x 2.031E+01, 1.841E+01, 3.214E+01, 4.426E+01, 3.730E+01,
     x 2.980E+01, 4.066E+01, 5.018E+01, 3.220E+01, 3.604E+01,
     x 3.959E+01, 4.168E+01, 5.454E+01, 4.219E+01, 2.546E+01,
     x 2.549E+01, 1.822E+01, 2.156E+01, 2.064E+01, 2.024E+01,
     x 2.497E+01, 3.404E+01, 3.273E+01, 3.785E+01, 4.170E+01,
     x 3.573E+01, 4.292E+01, 6.319E+01, 4.490E+01, 4.006E+01,
     x 3.261E+01, 3.440E+01, 3.519E+01, 1.613E+01, 2.907E+01,
     x 5.954E+01, 4.663E+01, 4.425E+01, 4.058E+01, 3.868E+01,
     x 3.487E+01, 3.275E+01, 3.469E+01, 3.441E+01, 3.128E+01,
     x 3.344E+01, 4.514E+01, 4.062E+01, 5.209E+01, 5.171E+01,
     x 4.788E+01, 4.405E+01, 4.339E+01, 4.811E+01, 6.062E+01,
     x 5.613E+01, 4.213E+01, 2.238E+01, 2.501E+01, 1.904E+01,
     x 1.906E+01, 3.163E+01, 5.112E+01, 4.854E+01, 3.368E+01,
     x 2.885E+01, 3.464E+01, 2.487E+01, 1.935E+01, 2.639E+01,
     x 3.679E+01, 3.660E+01, 3.111E+01, 2.457E+01, 3.070E+01,
     x 3.718E+01, 2.700E+01, 2.427E+01, 2.164E+01, 2.222E+01,
     x 4.310E+01, 3.679E+01, 3.612E+01, 3.576E+01, 3.602E+01/
       data sunb47/
     x 3.375E+01, 4.469E+01, 3.517E+01, 2.985E+01, 2.304E+01,
     x 1.599E+01, 1.013E+01, 1.273E+01, 1.470E+01, 2.029E+01,
     x 3.185E+01, 3.017E+01, 3.090E+01, 2.870E+01, 4.593E+01,
     x 6.155E+01, 5.480E+01, 4.029E+01, 3.897E+01, 2.519E+01,
     x 3.792E+01, 4.833E+01, 4.719E+01, 4.850E+01, 4.512E+01,
     x 4.172E+01, 2.503E+01, 1.455E+01, 2.624E+01, 3.801E+01,
     x 2.952E+01, 1.926E+01, 3.947E+01, 4.856E+01, 3.952E+01,
     x 3.713E+01, 2.893E+01, 2.042E+01, 1.694E+01, 2.408E+01,
     x 1.666E+01, 1.621E+01, 2.354E+01, 3.439E+01, 3.212E+01,
     x 3.411E+01, 2.418E+01, 3.640E+01, 4.138E+01, 4.120E+01,
     x 3.377E+01, 2.518E+01, 1.546E+01, 1.401E+01, 1.820E+01,
     x 2.480E+01, 4.491E+01, 3.850E+01, 3.070E+01, 2.214E+01,
     x 1.922E+01, 1.517E+01, 2.333E+01, 1.844E+01, 2.729E+01,
     x 2.905E+01, 2.714E+01, 2.142E+01, 1.608E+01, 2.251E+01,
     x 2.581E+01, 2.578E+01, 1.693E+01, 2.131E+01, 1.394E+01,
     x 1.715E+01, 3.170E+01, 2.849E+01, 2.115E+01, 1.467E+01,
     x 2.189E+01, 1.891E+01, 1.727E+01, 1.400E+01, 1.755E+01,
     x 1.400E+01, 1.392E+01, 2.388E+01, 2.321E+01, 1.353E+01,
     x 1.361E+01, 1.953E+01, 1.701E+01, 1.795E+01, 1.359E+01,
     x 1.565E+01, 1.198E+01, 1.154E+01, 1.408E+01, 1.595E+01/
       data sunb48/
     x 1.374E+01, 1.326E+01, 1.379E+01, 1.261E+01, 1.307E+01,
     x 1.342E+01, 1.412E+01, 1.416E+01, 1.496E+01, 1.229E+01,
     x 1.330E+01, 1.326E+01, 1.318E+01, 1.496E+01, 1.468E+01,
     x 1.387E+01, 1.378E+01, 1.422E+01, 1.322E+01, 1.247E+01,
     x 1.407E+01, 1.435E+01, 1.284E+01, 1.298E+01, 1.196E+01,
     x 1.134E+01, 1.198E+01, 1.325E+01, 1.282E+01, 1.182E+01,
     x 1.142E+01, 1.169E+01, 1.107E+01, 1.155E+01, 1.203E+01,
     x 1.183E+01, 1.245E+01, 1.228E+01, 1.183E+01, 1.043E+01,
     x 1.010E+01, 1.290E+01, 1.240E+01, 1.216E+01, 1.093E+01,
     x 1.147E+01, 1.248E+01, 1.065E+01, 8.401E+00, 8.737E+00,
     x 1.091E+01, 1.253E+01, 1.218E+01, 1.140E+01, 1.299E+01,
     x 1.178E+01, 1.037E+01, 8.452E+00, 1.104E+01, 1.094E+01,
     x 1.240E+01, 1.270E+01, 1.121E+01, 7.531E+00, 9.991E+00,
     x 1.102E+01, 1.168E+01, 1.173E+01, 1.173E+01, 1.169E+01,
     x 1.200E+01, 1.119E+01, 1.167E+01, 1.164E+01, 1.082E+01,
     x 1.141E+01, 1.172E+01, 1.123E+01, 1.036E+01, 1.098E+01,
     x 1.141E+01, 1.111E+01, 9.673E+00, 9.291E+00, 1.198E+01,
     x 1.197E+01, 1.205E+01, 1.243E+01, 1.090E+01, 1.027E+01,
     x 1.067E+01, 9.688E+00, 1.029E+01, 1.140E+01, 1.139E+01,
     x 1.220E+01, 1.110E+01, 1.124E+01, 1.048E+01, 1.080E+01/
       data sunb49/
     x 9.410E+00, 8.460E+00, 8.534E+00, 9.140E+00, 9.568E+00,
     x 1.052E+01, 1.120E+01, 1.059E+01, 1.024E+01, 1.055E+01,
     x 1.071E+01, 1.098E+01, 9.254E+00, 8.493E+00, 7.730E+00,
     x 9.488E+00, 6.869E+00, 8.506E+00, 9.176E+00, 1.027E+01,
     x 1.051E+01, 1.073E+01, 1.106E+01, 1.046E+01, 8.864E+00,
     x 1.017E+01, 8.284E+00, 9.282E+00, 8.652E+00, 9.951E+00,
     x 9.860E+00, 9.642E+00, 8.353E+00, 7.291E+00, 5.806E+00,
     x 4.227E+00, 7.531E+00, 8.929E+00, 8.069E+00, 8.718E+00,
     x 9.976E+00, 1.010E+01, 8.650E+00, 8.282E+00, 7.424E+00,
     x 7.522E+00, 7.766E+00, 8.669E+00, 9.129E+00, 8.400E+00,
     x 9.709E+00, 8.732E+00, 8.522E+00, 8.845E+00, 1.037E+01,
     x 9.848E+00, 8.425E+00, 9.171E+00, 8.228E+00, 7.851E+00,
     x 8.504E+00, 8.029E+00, 8.769E+00, 8.025E+00, 8.650E+00,
     x 8.611E+00, 9.748E+00, 9.185E+00, 9.195E+00, 8.664E+00,
     x 7.216E+00, 6.670E+00, 7.826E+00, 8.498E+00, 9.187E+00,
     x 9.060E+00, 8.405E+00, 7.170E+00, 8.599E+00, 7.617E+00,
     x 7.471E+00, 8.395E+00, 7.852E+00, 7.935E+00, 8.599E+00,
     x 9.155E+00, 8.399E+00, 7.402E+00, 7.478E+00, 7.940E+00,
     x 9.012E+00, 8.035E+00, 7.962E+00, 8.537E+00, 8.275E+00,
     x 8.757E+00, 8.645E+00, 7.4289   , 7.4489   , 7.4059   /  
cc   R 7.4969   , 7.4013   , 7.4289   , 7.4489   , 7.4059   /      
C         SOLAR SPECTRUM FROM  50000 TO  51430 CM-1,  IN STEPS OF 10 CM-solr5290
      DATA SUNB16  /                                                    solr5300
     A 7.4198, 7.5261, 7.5252, 7.3239, 7.1263, 7.1423, 7.3340, 7.5049,  solr5310
     B 7.5484, 7.5319, 7.5163, 7.4995, 7.5728, 7.8104, 8.0588, 8.0948,  solr5320
     C 7.9140, 7.6978, 7.5116, 7.2138, 6.8063, 6.5430, 6.5232, 6.5869,  solr5330
     D 6.5610, 6.3984, 6.1889, 6.0587, 6.0676, 6.1988, 6.3140, 6.2527,  solr5340
     E 6.0929, 6.0277, 6.0941, 6.3031, 6.6594, 6.9398, 6.9566, 6.8310,  solr5350
     F 6.7374, 6.6812, 6.6558, 6.8336, 7.2020, 7.4012, 7.2950, 7.0488,  solr5360
     G 6.7966, 6.6293, 6.5868, 6.5980, 6.6007, 6.6501, 6.7627, 6.7853,  solr5370
     H 6.6321, 6.4856, 6.5198, 6.6486, 6.7271, 6.7227, 6.6696, 6.6189,  solr5380
     I 6.5979, 6.6188, 6.7110, 6.8343, 6.8750, 6.8250, 6.7885, 6.8266,  solr5390
     J 6.8556, 6.8068, 6.8377, 7.0467, 7.2779, 7.4139, 7.4712, 7.4621,  solr5400
     K 7.4071, 7.3592, 7.3372, 7.3220, 7.2938, 7.2531, 7.2052, 7.1335,  solr5410
     L 7.0298, 6.8533, 6.5535, 6.2227, 6.0139, 5.9384, 5.9038, 5.8568,  solr5420
     M 5.7909, 5.7326, 5.7745, 5.9608, 6.1865, 6.3681, 6.4997, 6.5437,  solr5430
     N 6.4637, 6.2708, 6.0451, 5.9557, 6.0855, 6.2542, 6.2454, 6.0795,  solr5440
     O 5.9102, 5.8447, 5.9218, 6.1063, 6.2895, 6.3271, 6.1097, 5.7421,  solr5450
     P 5.4452, 5.2981, 5.3256, 5.4935, 5.6819, 5.8245, 5.8933, 5.9630,  solr5460
     Q 6.1703, 6.4525, 6.6325, 6.6965, 6.7185, 6.6238, 6.3107, 5.9241,  solr5470
     R 5.6987, 5.6651, 5.7428, 5.8790, 5.9715, 5.9618, 5.9674, 6.0754 / solr5480
C         SOLAR SPECTRUM FROM  51440 TO  52870 CM-1,  IN STEPS OF 10 CM-solr5490
      DATA SUNB17  /                                                    solr5500
     A 6.2541, 6.4300, 6.4968, 6.4564, 6.4082, 6.3024, 6.0135, 5.6431,  solr5510
     B 5.3963, 5.2989, 5.2635, 5.2227, 5.1279, 4.9315, 4.6348, 4.3168,  solr5520
     C 4.0151, 3.6625, 3.2906, 3.1028, 3.1349, 3.1994, 3.2596, 3.4144,  solr5530
     D 3.5949, 3.6534, 3.6296, 3.6281, 3.5876, 3.4292, 3.2659, 3.2284,  solr5540
     E 3.2576, 3.3002, 3.4535, 3.7372, 4.0573, 4.3558, 4.5999, 4.7781,  solr5550
     F 4.8855, 4.8999, 4.8392, 4.7624, 4.7059, 4.6981, 4.7666, 4.8453,  solr5560
     G 4.8236, 4.7293, 4.6861, 4.7132, 4.7725, 4.8713, 4.9596, 4.9527,  solr5570
     H 4.8957, 4.9252, 5.0736, 5.2229, 5.2505, 5.1537, 5.0156, 4.8880,  solr5580
     I 4.7686, 4.6549, 4.5534, 4.4828, 4.4661, 4.5040, 4.5905, 4.7033,  solr5590
     J 4.7858, 4.8334, 4.9283, 5.0377, 5.0065, 4.8471, 4.6828, 4.5586,  solr5600
     K 4.4812, 4.4314, 4.3903, 4.3830, 4.4066, 4.3900, 4.2973, 4.1978,  solr5610
     L 4.1462, 4.1084, 4.1495, 4.3897, 4.6859, 4.8206, 4.7938, 4.6781,  solr5620
     M 4.5222, 4.3959, 4.3358, 4.2947, 4.2259, 4.1452, 4.1060, 4.1462,  solr5630
     N 4.2149, 4.2549, 4.3061, 4.3742, 4.3738, 4.2718, 4.1389, 4.0405,  solr5640
     O 3.9457, 3.8127, 3.7099, 3.7344, 3.8589, 3.9598, 3.9525, 3.8377,  solr5650
     P 3.6708, 3.5357, 3.4929, 3.5375, 3.6381, 3.7890, 3.9671, 4.0995,  solr5660
     Q 4.1421, 4.1302, 4.1235, 4.1623, 4.2506, 4.2948, 4.2231, 4.0993,  solr5670
     R 3.9680, 3.9475, 4.1958, 4.5131, 4.6101, 4.5130, 4.3474, 4.1749 / solr5680
C         SOLAR SPECTRUM FROM  52880 TO  54310 CM-1,  IN STEPS OF 10 CM-solr5690
      DATA SUNB18  /                                                    solr5700
     A 4.0467, 3.9956, 4.0078, 4.0374, 4.0255, 3.9379, 3.8192, 3.7529,  solr5710
     B 3.7675, 3.8260, 3.8654, 3.8518, 3.8148, 3.8028, 3.8098, 3.7934,  solr5720
     C 3.7660, 3.7944, 3.8689, 3.8978, 3.8856, 3.8923, 3.8570, 3.6940,  solr5730
     D 3.4693, 3.3222, 3.2824, 3.2887, 3.3039, 3.3222, 3.3313, 3.3326,  solr5740
     E 3.3482, 3.3807, 3.4188, 3.4602, 3.4972, 3.5151, 3.5155, 3.5165,  solr5750
     F 3.5258, 3.5406, 3.5478, 3.5345, 3.5339, 3.5820, 3.6396, 3.6448,  solr5760
     G 3.5872, 3.5112, 3.4804, 3.5257, 3.6238, 3.7290, 3.8023, 3.8024,  solr5770
     H 3.7268, 3.6578, 3.6439, 3.6422, 3.6373, 3.6397, 3.6410, 3.6494,  solr5780
     I 3.6608, 3.6251, 3.5212, 3.4020, 3.2845, 3.1230, 2.9483, 2.8515,  solr5790
     J 2.8432, 2.8638, 2.8967, 2.9505, 3.0025, 3.0552, 3.1106, 3.1178,  solr5800
     K 3.0596, 2.9854, 2.9316, 2.8903, 2.8590, 2.8500, 2.8450, 2.8121,  solr5810
     L 2.7626, 2.7424, 2.7667, 2.8024, 2.8165, 2.8111, 2.8128, 2.8569,  solr5820
     M 2.9659, 3.1062, 3.1990, 3.2128, 3.2088, 3.2391, 3.2661, 3.2364,  solr5830
     N 3.1173, 2.9094, 2.6952, 2.5324, 2.3959, 2.2953, 2.2510, 2.2245,  solr5840
     O 2.1811, 2.1301, 2.1482, 2.3257, 2.5856, 2.7226, 2.6495, 2.4508,  solr5850
     P 2.2444, 2.0850, 1.9891, 1.9843, 2.0816, 2.2233, 2.3248, 2.3551,  solr5860
     Q 2.3479, 2.3606, 2.4296, 2.5361, 2.6128, 2.6216, 2.6069, 2.6196,  solr5870
     R 2.6464, 2.6427, 2.5823, 2.4682, 2.3320, 2.2405, 2.2637, 2.3973 / solr5880
C         SOLAR SPECTRUM FROM  54320 TO  55750 CM-1,  IN STEPS OF 10 CM-solr5890
      DATA SUNB19  /                                                    solr5900
     A 2.5524, 2.6891, 2.8508, 3.0103, 3.0681, 3.0064, 2.9114, 2.8609,  solr5910
     B 2.8517, 2.8374, 2.7894, 2.7288, 2.7138, 2.7729, 2.8707, 2.9536,  solr5920
     C 2.9953, 2.9911, 2.9398, 2.8550, 2.7732, 2.7303, 2.7366, 2.7650,  solr5930
     D 2.7705, 2.7374, 2.6830, 2.6218, 2.5663, 2.5341, 2.5351, 2.5681,  solr5940
     E 2.6124, 2.6305, 2.6024, 2.5431, 2.4840, 2.4546, 2.4684, 2.5100,  solr5950
     F 2.5445, 2.5532, 2.5564, 2.5889, 2.6616, 2.7553, 2.8466, 2.9290,  solr5960
     G 2.9958, 3.0175, 2.9774, 2.8990, 2.8001, 2.6927, 2.6171, 2.5931,  solr5970
     H 2.5809, 2.5276, 2.4284, 2.3365, 2.3162, 2.3855, 2.4872, 2.5455,  solr5980
     I 2.5773, 2.6809, 2.9720, 3.5757, 4.4006, 5.0044, 5.0295, 4.5135,  solr5990
     J 3.7071, 2.9059, 2.3600, 2.1418, 2.1119, 2.0871, 2.0301, 2.0043,  solr6000
     K 2.0361, 2.0963, 2.1520, 2.1878, 2.1955, 2.1864, 2.1899, 2.2170,  solr6010
     L 2.2574, 2.2895, 2.2783, 2.2148, 2.1641, 2.2343, 2.4726, 2.8119,  solr6020
     M 3.1288, 3.2984, 3.2206, 2.8859, 2.4473, 2.1436, 2.0729, 2.1391,  solr6030
     N 2.2171, 2.2580, 2.2654, 2.2481, 2.2103, 2.1657, 2.1356, 2.1321,  solr6040
     O 2.1438, 2.1461, 2.1396, 2.1460, 2.1588, 2.1581, 2.1481, 2.1343,  solr6050
     P 2.1101, 2.0754, 2.0400, 2.0121, 1.9930, 1.9799, 1.9699, 1.9613,  solr6060
     Q 1.9537, 1.9454, 1.9312, 1.9058, 1.8726, 1.8470, 1.8465, 1.8693,  solr6070
     R 1.8844, 1.8635, 1.8143, 1.7618, 1.7188, 1.6853, 1.6656, 1.6708 / solr6080
C         SOLAR SPECTRUM FROM  55760 TO  57190 CM-1,  IN STEPS OF 10 CM-solr6090
      DATA SUNB20  /                                                    solr6100
     A 1.7036, 1.7519, 1.8120, 1.9015, 2.0124, 2.0980, 2.1385, 2.1481,  solr6110
     B 2.1347, 2.1086, 2.0953, 2.1062, 2.1095, 2.0685, 2.0001, 1.9461,  solr6120
     C 1.9194, 1.9088, 1.9023, 1.8977, 1.9049, 1.9300, 1.9588, 1.9635,  solr6130
     D 1.9357, 1.9019, 1.8887, 1.8939, 1.9018, 1.9038, 1.8975, 1.8747,  solr6140
     E 1.8289, 1.7716, 1.7303, 1.7330, 1.7900, 1.8782, 1.9548, 1.9907,  solr6150
     F 1.9807, 1.9430, 1.9173, 1.9218, 1.9203, 1.8717, 1.7832, 1.6965,  solr6160
     G 1.6389, 1.6077, 1.5924, 1.5818, 1.5583, 1.5142, 1.4616, 1.4237,  solr6170
     H 1.4252, 1.4834, 1.5970, 1.7410, 1.8771, 1.9784, 2.0451, 2.0872,  solr6180
     I 2.0909, 2.0384, 1.9573, 1.9002, 1.8824, 1.8663, 1.8193, 1.7540,  solr6190
     J 1.6874, 1.6222, 1.5726, 1.5450, 1.5290, 1.5312, 1.5699, 1.6411,  solr6200
     K 1.7186, 1.7678, 1.7546, 1.6623, 1.5115, 1.3588, 1.2605, 1.2348,  solr6210
     L 1.2611, 1.3091, 1.3588, 1.3884, 1.3800, 1.3482, 1.3224, 1.3159,  solr6220
     M 1.3437, 1.4142, 1.4950, 1.5443, 1.5521, 1.5282, 1.4902, 1.4606,  solr6230
     N 1.4465, 1.4398, 1.4399, 1.4544, 1.4760, 1.4781, 1.4506, 1.4229,  solr6240
     O 1.4185, 1.4221, 1.4119, 1.3908, 1.3779, 1.3813, 1.3933, 1.4087,  solr6250
     P 1.4268, 1.4417, 1.4408, 1.4188, 1.3861, 1.3548, 1.3261, 1.2980,  solr6260
     Q 1.2769, 1.2731, 1.2856, 1.3002, 1.3056, 1.2987, 1.2817, 1.2590,  solr6270
     R 1.2291, 1.1868, 1.1428, 1.1183, 1.1141, 1.1120, 1.1009, 1.0797 / solr6280
C         SOLAR SPECTRUM FROM  57200 TO  57490 CM-1,  IN STEPS OF 10 CM-solr6290
      DATA SUNB21  /                                                    solr6300
     A 1.0523, 1.0284, 1.0251, 1.0577, 1.1195, 1.1791, 1.2061, 1.2013,  solr6310
     B 1.1936, 1.2000, 1.2040, 1.1824, 1.1489, 1.1400, 1.1539, 1.1629,  solr6320
     C 1.1617, 1.1586, 1.1564, 1.1572, 1.1565, 1.1399, 1.1037, 1.0627,  solr6330
     D 1.0341, 1.0223, 1.0199, 1.0188, 1.0174, 1.0163  /                solr6340
      END                                                               solr6350
