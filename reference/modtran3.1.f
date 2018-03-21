      FUNCTION   AB(WAVL,A,CEN,B,C)                                     ab   100
CCC                                                                     ab   110
CCC    DESCRIBES THE IMAGINARY PART OF THE DIELECTRIC CONSTANT          ab   120
CCC                                                                     ab   130
      AB=-A*EXP(-ABS((ALOG10(10000.*WAVL/CEN)/B))**C)                   ab   140
      RETURN                                                            ab   150
      END                                                               ab   160
      BLOCK DATA ABCD                                                   abcd 100
C>    BLOCK DATA                                                        abcd 110
      COMMON /ABC/ FACTOR(3),ANH3(2),ACO2(10),ACO(3),                   abcd 120
     X             ACH4(4),ANO2(3),AN2O(11),AO2(6),AO3(5),              abcd 130
     X             ASO2(4),AH2O(14),ANO,                                abcd 140
     X             AANH3(2),BBNH3(2),CCNH3(2),                          abcd 150
     X             AACO2(10),BBCO2(10),CCCO2(10),                       abcd 160
     X             AACO(3),BBCO(3),CCCO(3),                             abcd 170
     X             AACH4(4),BBCH4(4),CCCH4(4),                          abcd 180
     X             AANO2(3),BBNO2(3),CCNO2(3),                          abcd 190
     X             AAN2O(11),BBN2O(11),CCN2O(11),                       abcd 200
     X             AAO2(6),BBO2(6),CCO2(6),                             abcd 210
     X             AAO3(5),BBO3(5),CCO3(5),                             abcd 220
     X             AASO2(4),BBSO2(4),CCSO2(4),                          abcd 230
     X             AAH2O(14),BBH2O(14),CCH2O(14),                       abcd 240
     X             AANO     ,BBNO     ,CCNO                             abcd 250
      DATA FACTOR/1.0,0.09,0.015/                                       abcd 260
      DATA ANH3/.4704,.6035/                                            abcd 270
      DATA ACO2/.6176,.6810,.6033,.6146,.6513,.6050,                    abcd 280
     1 .6160,.7070,.7070,.7070/                                         abcd 290
      DATA ACO/.6397,.6133,.6133/                                       abcd 300
      DATA ACH4/.5844,.5844,.5844,.5844/                                abcd 310
      DATA ANO/.6613/                                                   abcd 320
      DATA ANO2/.7249,.7249,.7249/                                      abcd 330
      DATA AN2O/.8997,.7201,.7201,.7201,.7201,.7201,                    abcd 340
     1 .6933,.6933,.6933,.6933,.6933/                                   abcd 350
      DATA AO2/.6011,.5641,.5641,.5641,.5641,.5641/                     abcd 360
      DATA AO3/.8559,.7593,.7819,.9175,.7703/                           abcd 370
      DATA ASO2/.8907,.8466,.8466,.8466/                                abcd 380
      DATA AH2O/.5274,.5299,.5416,.5479,.5495,.5464,.5454,              abcd 390
     1 .5474,.5579,.5621,.5847,.6076,.6508,.6570/                       abcd 400
      DATA AANH3/.285772,.134244/                                       abcd 410
      DATA BBNH3/.269839,.353937/                                       abcd 420
      DATA CCNH3/19.9507,27.8458/                                       abcd 430
      DATA AACO2/.120300,.069728,.134448,.123189,.090948,               abcd 440
     1 .132717,.121835,.054348,.054348,.054348/                         abcd 450
      DATA BBCO2/.348172,.303510,.354002,.349583,.327160,               abcd 460
     1 .353435,.348936,.280674,.280674,.280674/                         abcd 470
      DATA CCCO2/29.4277,37.0842,27.8241,29.0834,33.4608,               abcd 480
     1 28.0093,29.2436,40.1951,40.1951,40.1951/                         abcd 490
      DATA AACO/.100401,.124454,.124454/                                abcd 500
      DATA BBCO/.335296,.350165,.350165/                                abcd 510
      DATA CCCO/32.0496,28.9354,28.9354/                                abcd 520
      DATA AACH4/.154447,.154447,.154447,.154447/                       abcd 530
      DATA BBCH4/.357657,.357657,.357657,.357657/                       abcd 540
      DATA CCCH4/25.8920,25.8920,25.8920,25.8920/                       abcd 550
      DATA AANO/.083336/                                                abcd 560
      DATA BBNO/.319585/                                                abcd 570
      DATA CCNO/34.6834/                                                abcd 580
      DATA AANO2/.045281,.045281,.045281/                               abcd 590
      DATA BBNO2/.264248,.264248,.264248/                               abcd 600
      DATA CCNO2/42.2784,42.2784,42.2784/                               abcd 610
      DATA AAN2O/.001679,.047599,.047599,.047599,.047599,               abcd 620
     1 .047599,.062106,.062106,.062106,.062106,.062106/                 abcd 630
      DATA BBN2O/.095621,.268696,.268696,.268696,.268696,               abcd 640
     1 .268696,.292891,.292891,.292891,.292891,.292891/                 abcd 650
      DATA CCN2O/59.3660,41.7251,41.7251,41.7251,41.7251,               abcd 660
     1 41.7251,38.5667,38.5667,38.5667,38.5667,38.5667/                 abcd 670
      DATA AAO2/.136706,.177087,.177087,.177087,.177087,.177087/        abcd 680
      DATA BBO2/.354683,.355447,.355447,.355447,.355447,.355447/        abcd 690
      DATA CCO2/27.5869,24.1314,24.1314,24.1314,24.1314,24.1314/        abcd 700
      DATA AAO3/.006712,.030870,.023278,.000458,.027004/                abcd 710
      DATA BBO3/.138026,.231722,.209952,.078492,.221153/                abcd 720
      DATA CCO3/55.6442,46.1189,48.5155,60.7802,47.2982/                abcd 730
      DATA AASO2/.002468,.008192,.008192,.008192/                       abcd 740
      DATA BBSO2/.104307,.147065,.147065,.147065/                       abcd 750
      DATA CCSO2/58.6298,54.8078,54.8078,54.8078/                       abcd 760
      DATA AAH2O/.219312,.216415,.206349,.196196,.194540,.198500,       abcd 770
     1 .198500,.196196,.184148,.179360,.154120,.130095,.091341,.086549/ abcd 780
      DATA BBH2O/.334884,.336904,.343272,.348610,.349810,.347498,       abcd 790
     1 .347498,.348610,.353429,.354864,.357640,.352497,.327526,.322898/ abcd 800
      DATA CCH2O/21.8352,21.9588,22.4234,22.9517,23.0750,22.8262,       abcd 810
     1 22.8262,22.9517,23.6654,23.9774,25.9207,28.2957,33.3998,34.1575/ abcd 820
      END                                                               abcd 830
      SUBROUTINE ABCDTA(IV)                                             sabc 100
      include 'parameter.list'
      COMMON RELHUM(laydim),HSTOR(laydim),ICH(4),VH(17),TX(65),W(65)  
      COMMON IMSMX,WPATH(laythr,65),TBBY(laythr),PATM(laythr),NSPEC,   
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX2(47),AWCCON(5)  
C                                                                       sabc 150
      COMMON /ABC/ FACTOR(3),ANH3(2),ACO2(10),ACO(3),                   sabc 160
     X             ACH4(4),ANO2(3),AN2O(11),AO2(6),AO3(5),              sabc 170
     X             ASO2(4),AH2O(14),ANO,                                sabc 180
     X             AANH3(2),BBNH3(2),CCNH3(2),                          sabc 190
     X             AACO2(10),BBCO2(10),CCCO2(10),                       sabc 200
     X             AACO(3),BBCO(3),CCCO(3),                             sabc 210
     X             AACH4(4),BBCH4(4),CCCH4(4),                          sabc 220
     X             AANO2(3),BBNO2(3),CCNO2(3),                          sabc 230
     X             AAN2O(11),BBN2O(11),CCN2O(11),                       sabc 240
     X             AAO2(6),BBO2(6),CCO2(6),                             sabc 250
     X             AAO3(5),BBO3(5),CCO3(5),                             sabc 260
     X             AASO2(4),BBSO2(4),CCSO2(4),                          sabc 270
     X             AAH2O(14),BBH2O(14),CCH2O(14),                       sabc 280
     X             AANO     ,BBNO     ,CCNO                             sabc 290
C                                                                       sabc 300
      COMMON /AABBCC/ AA(11),BB(11),CC(11),IBND(11),A(11),CPS(11)       sabc 310
      DIMENSION TJO(10)                                                 sabc 320
      DATA TJO /.9,.8,.7,.6,.5,.4,.3,.2,.1,.02/                         sabc 330
C                                                                       sabc 340
C    MOL                                                                sabc 350
C     1    H2O (ALL REGIONS) (DOUBLE EXPONENTIAL MODELS)                sabc 360
C     2    CO2 (ALL REGIONS) (DOUBLE EXPONENTIAL MODELS)                sabc 370
C     3    O3  (ALL REGIONS) (DOUBLE EXPONENTIAL MODELS)                sabc 380
C     4    N2O (ALL REGIONS) (DOUBLE EXPONENTIAL MODELS)                sabc 390
C     5    CO  (ALL REGIONS) (DOUBLE EXPONENTIAL MODELS)                sabc 400
C     6    CH4 (ALL REGIONS) (DOUBLE EXPONENTIAL MODELS)                sabc 410
C     7    O2  (ALL REGIONS) (DOUBLE EXPONENTIAL MODELS)                sabc 420
C     8    NO  (ALL REGIONS) (DOUBLE EXPONENTIAL MODELS)                sabc 430
C     9    SO2 (ALL REGIONS) (DOUBLE EXPONENTIAL MODELS)                sabc 440
C    10    NO2 (ALL REGIONS) (DOUBLE EXPONENTIAL MODELS)                sabc 450
C    11    NH3 (ALL REGIONS) (DOUBLE EXPONENTIAL MODELS)                sabc 460
C                                                                       sabc 470
C  ---H2O                                                               sabc 480
      IMOL = 1                                                          sabc 490
      IW = -1                                                           sabc 500
      IF(IV.GE.     0.AND.IV.LE.   345) IW = 17                         sabc 510
      IF(IV.GE.   350.AND.IV.LE.  1000) IW = 18                         sabc 520
      IF(IV.GE.  1005.AND.IV.LE.  1640) IW = 19                         sabc 530
      IF(IV.GE.  1645.AND.IV.LE.  2530) IW = 20                         sabc 540
      IF(IV.GE.  2535.AND.IV.LE.  3420) IW = 21                         sabc 550
      IF(IV.GE.  3425.AND.IV.LE.  4310) IW = 22                         sabc 560
      IF(IV.GE.  4315.AND.IV.LE.  6150) IW = 23                         sabc 570
      IF(IV.GE.  6155.AND.IV.LE.  8000) IW = 24                         sabc 580
      IF(IV.GE.  8005.AND.IV.LE.  9615) IW = 25                         sabc 590
      IF(IV.GE.  9620.AND.IV.LE. 11540) IW = 26                         sabc 600
      IF(IV.GE. 11545.AND.IV.LE. 13070) IW = 27                         sabc 610
      IF(IV.GE. 13075.AND.IV.LE. 14860) IW = 28                         sabc 620
      IF(IV.GE. 14865.AND.IV.LE. 16045) IW = 29                         sabc 630
      IF(IV.GE. 16340.AND.IV.LE. 17860) IW = 30                         sabc 640
      IBAND = IW - 16                                                   sabc 650
      IBND(IMOL) = IW                                                   sabc 660
      IF(IW .GT.  0) THEN                                               sabc 670
           A(IMOL)  =   AH2O(IBAND)                                     sabc 680
           AA(IMOL)  = AAH2O(IBAND)                                     sabc 690
           BB(IMOL)  = BBH2O(IBAND)                                     sabc 700
           CC(IMOL)  = CCH2O(IBAND)                                     sabc 710
      ENDIF                                                             sabc 720
C  ---O3                                                                sabc 730
      IMOL = 3                                                          sabc 740
      IW = -1                                                           sabc 750
      IF (IV .GE.     0 .AND. IV .LE.   200)  IW = 31                   sabc 760
      IF (IV .GE.   515 .AND. IV .LE.  1275)  IW = 32                   sabc 770
      IF (IV .GE.  1630 .AND. IV .LE.  2295)  IW = 33                   sabc 780
      IF (IV .GE.  2670 .AND. IV .LE.  2845)  IW = 34                   sabc 790
      IF (IV .GE.  2850 .AND. IV .LE.  3260)  IW = 35                   sabc 800
      IBAND      = IW - 30                                              sabc 810
      IBND(IMOL) = IW                                                   sabc 820
      IF(IW .GT.  0) THEN                                               sabc 830
           A(IMOL)  =  AO3(IBAND)                                       sabc 840
           AA(IMOL) = AAO3(IBAND)                                       sabc 850
           BB(IMOL) = BBO3(IBAND)                                       sabc 860
           CC(IMOL) = CCO3(IBAND)                                       sabc 870
      ENDIF                                                             sabc 880
C  ---CO2                                                               sabc 890
      IMOL = 2                                                          sabc 900
      IW = -1                                                           sabc 910
      IF (IV .GE.   425 .AND. IV .LE.   835)  IW = 36                   sabc 920
      IF (IV .GE.   840 .AND. IV .LE.  1440)  IW = 37                   sabc 930
      IF (IV .GE.  1805 .AND. IV .LE.  2855)  IW = 38                   sabc 940
      IF (IV .GE.  3070 .AND. IV .LE.  3755)  IW = 39                   sabc 950
      IF (IV .GE.  3760 .AND. IV .LE.  4065)  IW = 40                   sabc 960
      IF (IV .GE.  4530 .AND. IV .LE.  5380)  IW = 41                   sabc 970
      IF (IV .GE.  5905 .AND. IV .LE.  7025)  IW = 42                   sabc 980
      IF((IV .GE.  7395 .AND. IV .LE.  7785) .OR.                       sabc 990
     *   (IV .GE.  8030 .AND. IV .LE.  8335) .OR.                       sabc1000
     *   (IV .GE.  9340 .AND. IV .LE.  9670)) IW = 43                   sabc1010
      IBAND = IW - 35                                                   sabc1020
      IBND(IMOL) = IW                                                   sabc1030
      IF(IW .GT.  0) THEN                                               sabc1040
           A(IMOL)  =  ACO2(IBAND)                                      sabc1050
           AA(IMOL) = AACO2(IBAND)                                      sabc1060
           BB(IMOL) = BBCO2(IBAND)                                      sabc1070
           CC(IMOL) = CCCO2(IBAND)                                      sabc1080
      ENDIF                                                             sabc1090
C  ---CO                                                                sabc1100
      IMOL = 5                                                          sabc1110
      IW = -1                                                           sabc1120
      IF (IV .GE.     0 .AND. IV .LE.   175) IW = 44                    sabc1130
      IF((IV .GE.  1940 .AND. IV .LE.  2285) .OR.                       sabc1140
     *   (IV .GE.  4040 .AND. IV .LE.  4370)) IW = 45                   sabc1150
      IBAND = IW - 43                                                   sabc1160
      IBND(IMOL) = IW                                                   sabc1170
      IF(IW .GT.  0) THEN                                               sabc1180
           A(IMOL)  =  ACO(IBAND)                                       sabc1190
           AA(IMOL) = AACO(IBAND)                                       sabc1200
           BB(IMOL) = BBCO(IBAND)                                       sabc1210
           CC(IMOL) = CCCO(IBAND)                                       sabc1220
      ENDIF                                                             sabc1230
C  ---CH4                                                               sabc1240
      IMOL = 6                                                          sabc1250
      IW = -1                                                           sabc1260
      IF((IV .GE.  1065 .AND. IV .LE.  1775) .OR.                       sabc1270
     *   (IV .GE.  2345 .AND. IV .LE.  3230) .OR.                       sabc1280
     *   (IV .GE.  4110 .AND. IV .LE.  4690) .OR.                       sabc1290
     *   (IV .GE.  5865 .AND. IV .LE.  6135))IW = 46                    sabc1300
      IBAND = IW - 45                                                   sabc1310
      IBND(IMOL) = IW                                                   sabc1320
      IF(IW .GT.  0) THEN                                               sabc1330
           A(IMOL)  =  ACH4(IBAND)                                      sabc1340
           AA(IMOL) = AACH4(IBAND)                                      sabc1350
           BB(IMOL) = BBCH4(IBAND)                                      sabc1360
           CC(IMOL) = CCCH4(IBAND)                                      sabc1370
      ENDIF                                                             sabc1380
C  ---N2O                                                               sabc1390
      IMOL = 4                                                          sabc1400
      IW = -1                                                           sabc1410
      IF (IV .GE.     0 .AND. IV .LE.   120)  IW = 47                   sabc1420
      IF((IV .GE.   490 .AND. IV .LE.   775) .OR.                       sabc1430
     *   (IV .GE.   865 .AND. IV .LE.   995) .OR.                       sabc1440
     *   (IV .GE.  1065 .AND. IV .LE.  1385) .OR.                       sabc1450
     *   (IV .GE.  1545 .AND. IV .LE.  2040) .OR.                       sabc1460
     *   (IV .GE.  2090 .AND. IV .LE.  2655)) IW = 48                   sabc1470
      IF((IV .GE.  2705 .AND. IV .LE.  2865) .OR.                       sabc1480
     *   (IV .GE.  3245 .AND. IV .LE.  3925) .OR.                       sabc1490
     *   (IV .GE.  4260 .AND. IV .LE.  4470) .OR.                       sabc1500
     *   (IV .GE.  4540 .AND. IV .LE.  4785) .OR.                       sabc1510
     *   (IV .GE.  4910 .AND. IV .LE.  5165)) IW = 49                   sabc1520
      IBAND = IW - 46                                                   sabc1530
      IBND(IMOL) = IW                                                   sabc1540
c                                                                       sabc1550
      if(iw .eq. 49)iband=7                                             sabc1560
c                                                                       sabc1570
c     this correction is only for n2o as currently written              sabc1580
c                                                                       sabc1590
      IF(IW .GT.  0) THEN                                               sabc1600
           A(IMOL)  =  AN2O(IBAND)                                      sabc1610
           AA(IMOL) = AAN2O(IBAND)                                      sabc1620
           BB(IMOL) = BBN2O(IBAND)                                      sabc1630
           CC(IMOL) = CCN2O(IBAND)                                      sabc1640
      ENDIF                                                             sabc1650
C  ---O2                                                                sabc1660
      IMOL = 7                                                          sabc1670
      IW = -1                                                           sabc1680
      IF (IV .GE.     0 .AND. IV .LE.   265)  IW = 50                   sabc1690
      IF((IV .GE.  7650 .AND. IV .LE.  8080) .OR.                       sabc1700
     *   (IV .GE.  9235 .AND. IV .LE.  9490) .OR.                       sabc1710
     *   (IV .GE. 12850 .AND. IV .LE. 13220) .OR.                       sabc1720
     *   (IV .GE. 14300 .AND. IV .LE. 14600) .OR.                       sabc1730
     *   (IV .GE. 15695 .AND. IV .LE. 15955)) IW = 51                   sabc1740
       IF(IV .GE. 49600 .AND. IV. LE. 52710)  IW = 51                   sabc1750
      IBAND = IW - 49                                                   sabc1760
      IBND(IMOL) = IW                                                   sabc1770
      IF(IW .GT.  0) THEN                                               sabc1780
           A(IMOL)  =  AO2(IBAND)                                       sabc1790
           IF(IV .GE. 49600 .AND. IV. LE. 52710)  A(IMOL)  = .4704      sabc1800
           AA(IMOL) = AAO2(IBAND)                                       sabc1810
           BB(IMOL) = BBO2(IBAND)                                       sabc1820
           CC(IMOL) = CCO2(IBAND)                                       sabc1830
      ENDIF                                                             sabc1840
C  ---NH3                                                               sabc1850
      IMOL = 11                                                         sabc1860
      IW = -1                                                           sabc1870
      IF (IV .GE.     0 .AND. IV .LE.   385)  IW = 52                   sabc1880
      IF (IV .GE.   390 .AND. IV .LE.  2150)  IW = 53                   sabc1890
      IBAND = IW - 51                                                   sabc1900
      IBND(IMOL) = IW                                                   sabc1910
      IF(IW .GT.  0) THEN                                               sabc1920
           A(IMOL)  =  ANH3(IBAND)                                      sabc1930
           AA(IMOL) = AANH3(IBAND)                                      sabc1940
           BB(IMOL) = BBNH3(IBAND)                                      sabc1950
           CC(IMOL) = CCNH3(IBAND)                                      sabc1960
      ENDIF                                                             sabc1970
C  ---NO                                                                sabc1980
      IMOL = 8                                                          sabc1990
      IW = -1                                                           sabc2000
      IF (IV .GE.  1700 .AND. IV .LE.  2005) IW  = 54                   sabc2010
      IBAND = IW - 53                                                   sabc2020
      IBND(IMOL) = IW                                                   sabc2030
      IF(IW .GT.  0) THEN                                               sabc2040
           A(IMOL)  =  ANO                                              sabc2050
           AA(IMOL) = AANO                                              sabc2060
           BB(IMOL) = BBNO                                              sabc2070
           CC(IMOL) = CCNO                                              sabc2080
      ENDIF                                                             sabc2090
C  ---NO2                                                               sabc2100
      IW = -1                                                           sabc2110
      IMOL = 10                                                         sabc2120
      IF((IV .GE.   580 .AND. IV .LE.   925) .OR.                       sabc2130
     *   (IV .GE.  1515 .AND. IV .LE.  1695) .OR.                       sabc2140
     *   (IV .GE.  2800 .AND. IV .LE.  2970)) IW = 55                   sabc2150
      IBAND = IW - 54                                                   sabc2160
      IBND(IMOL) = IW                                                   sabc2170
      IF(IW .GT.  0) THEN                                               sabc2180
           A(IMOL)  =  ANO2(IBAND)                                      sabc2190
           AA(IMOL) = AANO2(IBAND)                                      sabc2200
           BB(IMOL) = BBNO2(IBAND)                                      sabc2210
           CC(IMOL) = CCNO2(IBAND)                                      sabc2220
      ENDIF                                                             sabc2230
C  ---SO2                                                               sabc2240
      IMOL = 9                                                          sabc2250
      IW = -1                                                           sabc2260
      IF (IV .GE.     0 .AND. IV .LE.   185)  IW = 56                   sabc2270
      IF((IV .GE.   400 .AND. IV .LE.   650) .OR.                       sabc2280
     *   (IV .GE.   950 .AND. IV .LE.  1460) .OR.                       sabc2290
     *   (IV .GE.  2415 .AND. IV .LE.  2580)) IW = 57                   sabc2300
      IBAND = IW - 55                                                   sabc2310
      IBND(IMOL) = IW                                                   sabc2320
      IF(IW .GT.  0) THEN                                               sabc2330
           A(IMOL)  =  ASO2(IBAND)                                      sabc2340
           AA(IMOL) = AASO2(IBAND)                                      sabc2350
           BB(IMOL) = BBSO2(IBAND)                                      sabc2360
           CC(IMOL) = CCSO2(IBAND)                                      sabc2370
      ENDIF                                                             sabc2380
      RETURN                                                            sabc2390
      END                                                               sabc2400
      FUNCTION   ABSLIM(TK,AWLWC)                                       absl 100
CCC                                                                     absl 110
CCC    FOR CLOUD OR AEROSOL ATTENUATION AS FREQ APPROACHES ZERO         absl 120
CCC    MODIFIED DEBYE EQUATIONS FROM RAY (1972) APPL. OPTICS VOL 11     absl 130
CCC                                                                     absl 140
CCC    ANO= 8.0*10**(-2)  (CM-4)                                        absl 150
CCC    ALM= 41.*RR**(-0.21)  (CM-1)  RR IN (MM/HR)                      absl 160
CCC                                                                     absl 170
      DATA PI/3.14159265/                                               absl 180
CCC   ANO=0.08                                                          absl 190
CCC   ALM=41./RR**0.21                                                  absl 200
      TC=TK-273.15                                                      absl 210
CCC                                                                     absl 220
      EFIN=5.27137+.0216474*TC-.00131198*TC*TC                          absl 230
      ES=78.54*(1.-4.579E-03*(TC-25.)+1.19E-05*                         absl 240
     1 (TC-25.)**2-2.8E-08*(TC-25.)**3)                                 absl 250
      SLAMBD=3.3836E-04*EXP(2513.98/TK)                                 absl 260
CCC                                                                     absl 270
CCC   VOL=PI*ANO*ALM**(-4)                                              absl 280
      ESMIE2=(ES-EFIN)/(ES+2.0)**2                                      absl 290
CCC                                                                     absl 300
CCC    DIVIDE VOLUME EQUIVALENT LIQUID BY 10 FOR UNITS CONVERSION       absl 310
CCC                                                                     absl 320
      EQLWC=AWLWC/10.0                                                  absl 330
CCC                                                                     absl 340
      ABSLIM=0.6951*TK*36.0*PI*EQLWC*SLAMBD*ESMIE2                      absl 350
CCC                                                                     absl 360
      RETURN                                                            absl 370
      END                                                               absl 380
      SUBROUTINE AEREXT (V)                                             aerx 100
C                                                                       aerx 110
C     INTERPOLATES AEROSOL EXTINCTION, ABSORPTION, AND ASYMMETRY        aerx 120
C     COEFFICIENTS FOR THE WAVENUMBER, V.                               aerx 130
C                                                                       aerx 140
C     MODIFIED FOR ASYMMETRY  - JAN 1986 (A.E.R. INC.)                  aerx 150
C                                                                       aerx 160
      include 'parameter.list'
      COMMON RELHUM(laydim),HSTOR(laydim),ICH(4),VH(17),TX(65),W(65)  
      COMMON IMSMX,WPATH(laythr,65),TBBY(laythr),PATM(laythr),NSPEC,   
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYC(5,47),VX2(47),AWCCON(5)  
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON /CARD1/ MODEL,ITYPE,IEMSCT,M1,M2,M3,IM,NOPRNT,TBOUND,SALB  aerx 180
     1  ,MODTRN                                                         aerx 190
      LOGICAL MODTRN                                                    aerx 200
      COMMON /CARD2/ IHAZE,ISEASN,IVULCN,ICSTL,ICLD,IVSA,VIS,WSS,WHH,   aerx 210
     1    RAINRT                                                        aerx 220
      COMMON /CARD3/ H1,H2,ANGLE,RANGE,BETA,RE,LEN                      aerx 230
C     COMMON /CARD4/ V1,V2,DV                                           aerx 240
      COMMON /CNTRL/ KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT               aerx 250
      COMMON /MODEL/ ZM(LAYDIM),PM(LAYDIM),TM(LAYDIM),RFNDX(LAYDIM),
     1  DENSTY(65,LAYDIM),CLDAMT(LAYDIM),RRAMT(LAYDIM),EQLWC(LAYDIM),
     1  HAZEC(LAYDIM)
      COMMON /AER/ EXTV(5),ABSV(5),ASYV(5)                              aerx 320
      COMMON /AERTM/TAER(5)                                             aerx 330
C                                                                       aerx 340
      DO 5 I=1,5                                                        aerx 350
      EXTV(I)=0.                                                        aerx 360
      ABSV(I)=0.                                                        aerx 370
      ASYV(I)=0.                                                        aerx 380
    5 CONTINUE                                                          aerx 390
      IF(V.LE.1.0E-5) GO TO 50                                          aerx 400
      IF(V.LE.33.333) GO TO 30                                          aerx 410
CCC                                                                     aerx 420
CCC   COMPUTE INFRARED ATTENUATION COEFFICIENT                          aerx 430
CCC                                                                     aerx 440
      IF(V.LE.50.0) THEN                                                aerx 450
           DO 2 MR=1,5                                                  aerx 460
           EXTC(MR,47)=GAMFOG(MR,33.333,TAER(MR),AWCCON(MR))            aerx 470
           ABSC(MR,47)=EXTC(MR,47)                                      aerx 480
           ASYC(MR,47)= 0.                                              aerx 490
2          CONTINUE                                                     aerx 500
      END IF                                                            aerx 510
CC    IF (IHAZE.EQ.0) RETURN                                            aerx 520
      V0=V                                                              aerx 530
      ALAM=1.0E+4/V0                                                    aerx 540
      DO 10 N=2,47                                                      aerx 550
      XD=ALAM-VX2(N)                                                    aerx 560
      IF (XD) 15,10,10                                                  aerx 570
   10 CONTINUE                                                          aerx 580
      N=47                                                              aerx 590
   15 VXD=VX2(N)-VX2(N-1)                                               aerx 600
      DO 20 I=1,5                                                       aerx 610
      EXTV(I)=(EXTC(I,N)-EXTC(I,N-1))*XD/VXD+EXTC(I,N)                  aerx 620
      ABSV(I)=(ABSC(I,N)-ABSC(I,N-1))*XD/VXD+ABSC(I,N)                  aerx 630
      ASYV(I)=(ASYC(I,N)-ASYC(I,N-1))*XD/VXD+ASYC(I,N)                  aerx 640
   20 CONTINUE                                                          aerx 650
      RETURN                                                            aerx 660
CCC                                                                     aerx 670
30    CONTINUE                                                          aerx 680
CCC    COMPUTE MICROWAVE ATTENUATION COEFFICIENTS                       aerx 690
CCC                                                                     aerx 700
      DO 40 I=1,5                                                       aerx 710
      EXTV(I)=GAMFOG( I,V,TAER(I),AWCCON(I))                            aerx 720
      ABSV(I)=EXTV(I)                                                   aerx 730
      ASYV(I)=0.                                                        aerx 740
40    CONTINUE                                                          aerx 750
      RETURN                                                            aerx 760
CCC                                                                     aerx 770
C     V LE 1.0E-5                                                       aerx 780
50    CONTINUE                                                          aerx 790
CCC                                                                     aerx 800
CCC   EQL=EQLWC(IL)                                                     aerx 810
      DO 60 I=1,5                                                       aerx 820
      EXTV(I)=0.0                                                       aerx 830
      ABSV(I)=0.                                                        aerx 840
      ASYV(I)=0.                                                        aerx 850
C     WRITE (ipr,300) I,AWCCON(I)                                       aerx 860
300   FORMAT(5X,'I,AWCCON=',I5,5X,1PE12.5)                              aerx 870
60    CONTINUE                                                          aerx 880
100   FORMAT(5X,'IL,IK,EQL,EXTV=',2I5,1P5E12.5)                         aerx 890
      RETURN                                                            aerx 900
      END                                                               aerx 910
      SUBROUTINE AERNSM(JPRT,  GNDALT)                                  
      include 'parameter.list'
C********************************************************************** 
C     DEFINES ALTITUDE DEPENDENT VARIABLES Z,P,T,WH,WO AND HAZE         
C     CLD RAIN  CLDTYPE                                                 
C     IT ALSO DEFINES ALTITUDE DEPENDENT VARIABLES WAIR,WCO2,WCO,       
C     WCH4,WN2O,WO2,WNH3,WNO,WNO2, AND WSO2                             
C     LOADS HAZE INTO APPROPRATE LOCATION                               
C********************************************************************** 
c
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

C
c
C
C     TRANS VARIABLES
C
      COMMON RELHUM(laydim),whno3(laydim),ICH(4),VH(17),TX(65),W(65)  
      COMMON IMSMX,WPATH(laythr,65),TBBY(laythr),PATM(laythr),NSPEC,   
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX2(47),AWCCON(5)  
      COMMON /MDATA/P(laydim),T(laydim),WH(laydim),WCO2(laydim),
     x WO(laydim),WN2O(laydim),WCO(laydim),WCH4(laydim),WO2(laydim)
c
      COMMON /MDATAX/ WMOLXT(MMOLX,laydim)
      COMMON /MODELX/ DNSTYX(MMOLX,LAYDIM)
c
      COMMON /CLDRR/  ZCLD(16),CLD(16,7),RR(16,7)    
      COMMON /MDATA1/ WNO(LAYDIM),WSO2(LAYDIM),WNO2(LAYDIM),
     x WNH3(LAYDIM),WAIR(LAYDIM)    
      COMMON /TRAC/ ANO(50),ASO2(50),ANO2(50),ANH3(50),ANO3(50),OH(50), aern 400
     X HF(50),HCL(50),HBR(50),HI(50),CLO(50),OCS(50),H2CO(50),          aern 410
     X HOCL(50),AN2(50),HCN(50),CH3CL(50),H2O2(50),C2H2(50),            aern 420
     X C2H6(50),PH3(50)                                                 aern 430
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON /CARD1/ MODEL,ITYPE,IEMSCT,M1,M2,M3,IM,NOPRNT,TBOUND,SALB  aern 230
     1  ,MODTRN                                                         aern 240
      LOGICAL MODTRN                                                    aern 250
      COMMON /CARD1A/ M4,M5,M6,MDEF,IRD1,IRD2                           aern 260
      COMMON /CARD1B/ JUNIT(15),WMOL(12),WAIR1,JLOW                     aern 270
      COMMON /CARD2/ IHAZE,ISEASN,IVULCN,ICSTL,ICLD,IVSA,VIS,WSS,WHH,   aern 280
     1    RAINRT                                                        aern 290
      COMMON /CARD2A/ CTHIK,CALT,CEXT                                   aern 300
      COMMON /CARD2D/ IREG(4),ALTB(4),IREGC(4)                          aern 310
      COMMON /CARD3/ H1,H2,ANGLE,RANGE,BETA,RE,LEN                      aern 320
C     COMMON /CARD4/ V1,V2,DV                                           aern 330
      COMMON /CNTRL/ KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT               aern 340
      COMMON /MART/ RHH                                                 aern 350
      COMMON /MODEL/ ZMdl(LAYDIM),PM(LAYDIM),TM(LAYDIM),
     1  RFNDX(LAYDIM),DENSTY(65,LAYDIM),CLDAMT(LAYDIM),RRAMT(LAYDIM),
     1  EQLWC(LAYDIM),HAZEC(LAYDIM)
      COMMON /ZVSALY/ ZVSA(10),RHVSA(10),AHVSA(10),IHVSA(10)            aern 460
      COMMON /MDLZ/HMDLZ(8)                                             aern 470
      CHARACTER*4  HZ         ,SEASN     ,VULCN     ,BLANK,             
     X            HMET        ,HMODEL     ,HTRRAD                       
      COMMON /TITL/ HZ(5,16),SEASN(5,2),VULCN(5,8),BLANK,               
     X HMET(5,2),HMODEL(5,8),HTRRAD(6,4)                                
      COMMON /VSBD/ VSB(10)           

c     archer added here
      common/co2mix /   co2rat , ch4rat, tozonerat,sozonerat,
     $     toffset,h2otscaled,
     $     h2orat


      CHARACTER*4  AHOL1,AHOL2   ,AHOL3   , AHLVSA   ,AHUS              aern 530
      DIMENSION AHOL1(5),AHOL2(5),AHOL3(5), AHLVSA(5),AHUS(5)           aern 540
      DIMENSION  ITY1(LAYDIM),IH1(LAYDIM),IS1(LAYDIM),IVL1(LAYDIM),
     1    ZGN(LAYDIM)
      CHARACTER*4  AHAHOL   ,HHOL                                       aern 560
      DIMENSION AHAHOL(5,13),HHOL(5)                                    aern 570
      DIMENSION AMWT(35),CLDTOP(10),AHAST(laydim)                       aern 580
c                                                                       
c                                                                       
      CHARACTER*1 JCHAR,BL,jcharx                                             
      common /crd1bx/junitx,wmolx(mmolx)                                
      DIMENSION  JCHAR(15)                                              
      DATA AHLVSA/'VSA ','DEFI','NED ','    ','    '/                   
      DATA  AHUS /'USER',' DEF','INED','    ','    '/                   
      DATA AHAHOL/                                                      
     1 'CUMU','LUS ','    ','    ','    ',                              
     2 'ALTO','STRA','TUS ','    ','    ',                              
     3 'STRA','TUS ','    ','    ','    ',                              
     4 'STRA','TUS ','STRA','TO C','UM  ',                              
     5 'NIMB','OSTR','ATUS','    ','    ',                              
     6 'DRIZ','ZLE ','2.0 ','MM/H','R   ',                              
     7 'LT R','AIN ','5.0 ','MM/H','R   ',                              
     8 'MOD ','RAIN',' 12.','5 MM','/HR ',                              
     9 'HEAV','Y RA','IN 2','5 MM','/HR ',                              
     A 'EXTR','EME ','RAIN',' 75M','M/HR',                              
     B 'USER',' ATM','OSPH','ERE ','    ',                              
     C 'USER',' RAI','N NO',' CLO','UD  ',                              
     D 'CIRR','US C','LOUD','    ','    ' /                             
      DATA AVOGAD/6.022045E+23/,ALOSMT/2.68675E+19/,                    
     1    GASCON/8.31441E+7/,PLANK/6.626176E-27/,BOLTZ/1.380662E-16/,   
     2    CLIGHT/2.99792458E10/                                         
      DATA AIRMWT/28.964/,AMWT/18.015,44.010,47.998,44.01,28.011,       
     1    16.043,31.999,30.01,64.06,46.01,17.03,63.01,17.00,20.01,      
     2    36.46,80.92,127.91,51.45,60.08,30.03,52.46,28.014,            
     3    27.03, 50.49, 34.01, 26.03, 30.07, 34.00, 7*0./               
      DATA CLDTOP / 3.,3.,1.,2.,.66,1.,.66,.66,3.,3./                   
C                                                                       
C     F(A) IS SATURATED WATER WAPOR DENSITY AT TEMP T,A=TZERO/T         
C                                                                       
      F(A)=EXP(18.9766-14.9595*A-2.43882*A*A)*A                         
C                                                                       
C                                                                       
C     ZMDL COMMON /MODEL/ FINAL ALTITUDE FOR LOWTRAN                    
C     ZK  ALTITUDE FOR CLOUD                                            
C     ZSC ALTITUDE FOR AEROSOLS  GNDALT                                 
C                                                                       
C                                                                       
      IREGC(1) = 0                                                      
      IREGC(2) = 0                                                      
      IREGC(3) = 0                                                      
      IREGC(4) = 0                                                      
      ICONV =1                                                          
      IRD0 = 1                                                          
      ICLDL = ICLD                                                      
      IF((MODEL .EQ. 0.) . OR. (MODEL .EQ. 7)) THEN                     
            IF(IM. NE. 1) RETURN                                        
      ENDIF                                                             
      IF((MODEL .GT. 0.) .AND. (MODEL .LT. 7)) IRD0 = 0                 
      IF((IRD0  .EQ. 1)  .AND. (IVSA.EQ.1)   ) THEN                     
           IRD0 = 0                                                     
           IRD1 = 0                                                     
           IRD2 = 0                                                     
           ICONV =0                                                     
           ML = ML + 10 - JLOW                                          
           IF(ML.GT.LAYDIM)WRITE(IPR,910)                                   
           IF(ML.GT.LAYDIM)ML=LAYDIM                                            
           ZVSA(10)=ZVSA(9)+0.01                                        
           RHVSA(10)=0.                                                 
           AHVSA(10)=0.                                                 
           IHVSA(10)=0                                                  
           IF(MODEL.EQ.0)WRITE (IPR,900)                                
900   FORMAT('   ERROR MODEL EQ 0 AND ARMY MODEL CANNOT MIX')           
           IF(MODEL.EQ.0)STOP                                           
910   FORMAT('  ERROR ML GT 24 AND ARMY MODEL TOP LAYER TRUNCATED')     
      ENDIF                                                             
      ICL=0                                                             
      IDSR=0                                                            
      IF(ICLD .EQ. 18 .OR. ICLD. EQ. 19)  THEN                          
           CALL CIRR18                                                  
           CLDD =  0.1 * CTHIK                                          
           CLD0 = CALT - 0.5*CLDD                                       
           IF(CLD0 .LE. 0.) CLD0 = 0.                                   
           CLD1 = CLD0 + CLDD                                           
           CLD2 = CLD1 + CTHIK - CLDD                                   
C          CLD3 = CLD2 + CLDD                                           
C                                                                       
C                                                                       
      ENDIF                                                             
      CALL FLAYZ(ML,MODEL,ICLD,ZMDL,GNDALT,IVSA)                        
      JPRT=1                                                            
      IF(MODEL.EQ.0  .OR. MODEL.EQ.7) JPRT=0                            
      IF(IVSA.EQ.1) JPRT=0                                              
      IF(ICLD.GE.1 .AND.ICLD.LT.20) JPRT=0                              
      IF(RAINRT.GT.0.) JPRT=0                                           
      IF(GNDALT.GT.0.) JPRT=0                                           
C                                                                       
      DO 5 I=1,ML                                                       
      HAZEC(I)=0.0                                                      
5     CONTINUE                                                          
      DO 6 II = 1,4                                                     
6     ALTB(II) = 0.                                                     
      T0=273.15                                                         
      IC1=1                                                             
      N=7                                                               
      IF(IVULCN.LE.0) IVULCN=1                                          
      IF(ISEASN.LE.0) ISEASN=1                                          
      IF(JPRT.EQ.0) THEN                                                
         WRITE(IPR,925) MODEL,ICLD                             
         IF(MODEL .EQ.7)WRITE(IPR,94)                                      
 94      FORMAT(/,10X,' MODEL 0 / 7 USER INPUT DATA ',//)                  
C                                                                       
      ENDIF                                                             
C                                                                       
C                                                                       
      DO 100 K=1,ML                                                     
C                                                                       
C    LOOP OVER LAYERS                                                   
C                                                                       
      RH = 0.                                                           
      WH(K)  =0.                                                        
      WO(K)  =0.                                                        
C                                                                       
      IHA1=0                                                            
      ICLD1=0                                                           
      ISEA1=0                                                           
      IVUL1=0                                                           
      VIS1=0.                                                           
      AHAZE=0.                                                          
      EQLWCZ=0.                                                         
      RRATZ=0.                                                          
      ICHR = 0                                                          
C     NEW                                                               
C                                                                       
      WAIR(K) = 0                                                       
      WCO2(K) = 0                                                       
      WCO(K)  = 0                                                       
      WCH4(K) = 0                                                       
      WN2O(K) = 0                                                       
      WO2(K)  = 0                                                       
      WNH3(K) = 0                                                       
      WNO (K) = 0                                                       
      WNO2(K) = 0                                                       
      WSO2(K) = 0                                                       
      WHNO3(K)= 0                                                       
      DO 10 KM = 1,15                                                   
      JCHAR(KM) = ' '                                                   
      IF(KM. GT. 12) GO TO 10                                           
      WMOL(KM) = 0.                                                     
10    CONTINUE                                                          
      jcharx = ' '
C                                                                       
C                                                                       
C        PARAMETERS - JCHAR = INPUT KEY (SEE BELOW)                     
C                                                                       
C                                                                       
C     ***  ROUTINE ALSO ACCEPTS VARIABLE UNITS ON PRESS AND TEMP        
C                                                                       
C          SEE INPUT KEY BELOW                                          
C                                                                       
C                                                                       
C                                                                       
C     FOR MOLECULAR SPECIES ONLY                                        
C                                                                       
C       JCHAR   JUNIT                                                   
C                                                                       
C     " ",A      10    VOLUME MIXING RATIO (PPMV)                       
C         B      11    NUMBER DENSITY (CM-3)                            
C         C      12    MASS MIXING RATIO (GM(K)/KG(AIR))                
C         D      13    MASS DENSITY (GM M-3)                            
C         E      14    PARTIAL PRESSURE (MB)                            
C         F      15    DEW POINT TEMP (TD IN T(K)) - H2O ONLY           
C         G      16     "    "     "  (TD IN T(C)) - H2O ONLY           
C         H      17    RELATIVE HUMIDITY (RH IN PERCENT) - H2O ONLY     
C         I      18    AVAILABLE FOR USER DEFINITION                    
C        1-6    1-6    DEFAULT TO SPECIFIED MODEL ATMOSPHERE            
C                                                                       
C     ****************************************************************  
C     ****************************************************************  
C                                                                       
C     ***** OTHER 'JCHAR' SPECIFICATIONS -                              
C                                                                       
C       JCHAR   JUNIT                                                   
C                                                                       
C      " ",A     10    PRESSURE IN (MB)                                 
C          B     11       "     "  (ATM)                                
C          C     12       "     "  (TORR)                               
C         1-6   1-6    DEFAULT TO SPECIFIED MODEL ATMOSPHERE            
C                                                                       
C      " ",A     10    AMBIENT TEMPERATURE IN DEG(K)                    
C          B     11       "         "       "  " (C)                    
C          C     12       "         "       "  " (F)                    
C         1-6   1-6    DEFAULT TO SPECIFIED MODEL ATMOSPHERE            
C                                                                       
C     ***** DEFINITION OF "DEFAULT" CHOICES FOR PROFILE SELECTION ***** 
C                                                                       
C      FOR THE USER WHO WISHES TO ENTER ONLY SELECTED ORIGINAL          
C      VERTICAL PROFILES AND WANTS STANDARD ATMOSPHERE SPECIFICATIONS   
C      FOR THE OTHERS, THE FOLLOWING OPTION IS AVAILABLE                
C                                                                       
C     *** JCHAR(P,T OR K) MUST = 1-6 (AS ABOVE)                         
C                                                                       
C      FOR MOLECULES 8-35, ONLY US STD PROFILES ARE AVIALABLE           
C      THEREFORE, WHEN  'JCHAR(K) = 1-5', JCHAR(K) WILL BE RESET TO 6   
C                                                                       
C                                                                       
      IF(IRD0 .EQ. 1) THEN                                              
          READ(IRD,80)ZMDL(K),P(K),T(K),WMOL(1),WMOL(2),WMOL(3),        
     X     (JCHAR(KM),KM=1,15),jcharx                                          
80         FORMAT ( F10.3,5E10.3,15A1,A1)                                  
          WRITE(IPR,81)ZMDL(K),P(K),T(K),WMOL(1),WMOL(2),WMOL(3),       
     X     JCHAR,jcharx
81         FORMAT (F10.3,1P5E10.3,10X,15A1,A1)                            
      ENDIF                                                             
      IF(IRD1 .EQ. 1) THEN                                              
         READ(IRD,83)(WMOL(KM),KM=4,12)                               
 83      FORMAT((8E10.3))                                             
         WRITE(IPR,84)(WMOL(KM),KM=4,12)                              
 84      FORMAT((1P8E10.3))                                           
         if (mdef .eq. 2) then
c           the extra species (i. e., the wmolx species) will be read if
c           mdef .eq. 2 and ird1 .eq. 1.
c           they will be read 8 species at a time until all species are
c           read.
            nrowx = int(real(nspecx)/real(8))+1
            mrowx = mod(nspecx,8)
            if (mrowx .eq. 0) then
               maxrow =  nrowx-1
            else
               maxrow = nrowx
            endif
            do 85 krowx = 1, maxrow
               ibegx = (krowx-1)*8+1
               iendx = ibegx+8-1
               if (krowx .eq. nrowx) iendx = ibegx + mrowx-1
               read(ird,83)(wmolx(kmx),kmx=ibegx,iendx)
               write(ipr,84)(wmolx(kmx),kmx=ibegx,iendx)
 85         continue  
         endif
      ENDIF      
C                                                                       
C                                                                       
C     AHAZE =  AEROSOL VISIBLE EXTINCTION COFF (KM-1)                   
C     AT A WAVELENGTH OF 0.55 MICROMETERS                               
C                                                                       
C     EQLWCZ=LIQUID WATER CONTENT (PPMV) AT ALT Z                       
C            FOR AEROSOL, CLOUD OR FOG MODELS                           
C                                                                       
C     RRATZ=RAIN RATE (MM/HR) AT ALT Z                                  
C                                                                       
C     IHA1 AEROSOL MODEL USED FOR SPECTRAL DEPENDENCE OF EXTINCTION     
C                                                                       
C     IVUL1 STRATOSPHERIC AERSOL MODEL USED FOR SPECTRAL DEPENDENCE     
C     OF EXT AT Z                                                       
C                                                                       
C     ICLD1 CLOUD MODEL USED FOR SPECTRAL DEPENDENCE OF EXT AT Z        
C                                                                       
C     ONLY ONE OF IHA1,ICLD1  OR IVUL1 IS ALLOWED                       
C     IHA1 NE 0 OTHERS IGNORED                                          
C     IHA1 EQ 0 AND ICLD1 NE 0 USE ICLD1                                
C                                                                       
C     IF AHAZE AND EQLWCZ ARE BOUTH ZERO                                
C        DEFAULT PROFILE ARE LOADED FROM IHA1,ICLD1,IVUL1               
C     ISEA1 = AERSOL SEASON CONTROL FOR ALTITUDE Z                      
C                                                                       
C     ICHR  CHANGE AERSOL PROFILE REGION FOR IHA1 = 7                   
C                                                                       
      IF(IRD2 .EQ. 1) THEN                                              
           READ(IRD,82)    AHAZE,EQLWCZ,RRATZ,IHA1,ICLD1,IVUL1,ISEA1,   
     X ICHR                                                             
           WRITE(IPR,82)    AHAZE,EQLWCZ,RRATZ,IHA1,ICLD1,IVUL1,ISEA1,  
     X ICHR                                                             
82         FORMAT(10X,3F10.3,5I5)                                       
      ELSE                                                              
           RRATZ = RAINRT                                               
           IF(ZMDL(K) . GT. 6.) RRATZ = 0                               
      ENDIF                                                             
      ICLDS = ICLD1                                                     
      IF( ICHR.EQ. 1) THEN                                              
         IF(IHA1. EQ. 0) THEN                                           
             IF(ICLD1. NE. 11) ICHR = 0                                 
         ELSE                                                           
             IF(IHA1 . NE.  7) ICHR = 0                                 
         ENDIF                                                          
      ENDIF                                                             
      IF(MODEL .EQ. 0) THEN                                             
           HMDLZ(1) = ZMDL(K)                                           
           HMDLZ(2) =    P(K)                                           
           HMDLZ(3) =    T(K)                                           
           HMDLZ(4) = WMOL(1)                                           
           HMDLZ(5) = WMOL(2)                                           
           HMDLZ(6) = WMOL(3)                                           
           HMDLZ(7) = AHAZE                                             
      ENDIF                                                             
      DO 12 KM = 1,15                                                   
         JUNIT(KM) = JOU(JCHAR(KM))                                        
 12   continue
      junitx = jou(jcharx)
      IF(IRD0 .EQ. 0) THEN                                              
          JUNIT(1) = M1                                                 
          JUNIT(2) = M1                                                 
          JUNIT(3) = M2                                                 
          JUNIT(4) =  6                                                 
          JUNIT(5) = M3                                                 
          JUNIT(6) = M5                                                 
          JUNIT(7) = M6                                                 
          JUNIT(8) = M4                                                 
          JUNIT(9) =  6                                                 
          JUNIT(10)=  6                                                 
          JUNIT(11)=  6                                                 
          JUNIT(12)=  6                                                 
          JUNIT(13)=  6                                                 
          JUNIT(14)=  6                                                 
          JUNIT(15)=  6                                                 
          junitx = 6
      ELSE                                                              
          BL = ' '                                                      
          IF((M1   .GT.0).AND.(JCHAR(1).EQ.BL))                         
     X    JUNIT(1) = M1                                                 
          IF((M1   .GT.0).AND.(JCHAR(2).EQ.BL))                         
     X    JUNIT(2) = M1                                                 
          IF((M2   .GT.0).AND.(JCHAR(3).EQ.BL))                         
     X    JUNIT(3) = M2                                                 
          IF((MDEF .GT.0).AND.(JCHAR(4).EQ.BL))                         
     X    JUNIT(4) = 6                                                  
          IF((M3   .GT.0).AND.(JCHAR(5).EQ.BL))                         
     X    JUNIT(5) = M3                                                 
          IF((M5   .GT.0).AND.(JCHAR(6).EQ.BL))                         
     X    JUNIT(6) = M5                                                 
          IF((M6   .GT.0).AND.(JCHAR(7).EQ.BL))                         
     X    JUNIT(7) = M6                                                 
          IF((M4   .GT.0).AND.(JCHAR(8).EQ.BL))                         
     X    JUNIT(8) = M4                                                 
          IF((MDEF .GT.0).AND.(JCHAR(9).EQ.BL))                         
     X    JUNIT(9) = 6                                                  
          IF((MDEF .GT.0).AND.(JCHAR(10).EQ.BL))                        
     X    JUNIT(10)= 6                                                  
          IF((MDEF .GT.0).AND.(JCHAR(11).EQ.BL))                        
     X    JUNIT(11)= 6                                                  
          IF((MDEF .GT.0).AND.(JCHAR(12).EQ.BL))                        
     X    JUNIT(12)= 6                                                  
          IF((MDEF .GT.0).AND.(JCHAR(13).EQ.BL))                        
     X    JUNIT(13)= 6                                                  
          IF((MDEF .GT.0).AND.(JCHAR(14).EQ.BL))                        
     X    JUNIT(14)= 6                                                  
          IF((MDEF .GT.0).AND.(JCHARx   .EQ.BL))                        
     X    JUNITx   = 6                                                  
      ENDIF  
      IF(ICONV .EQ. 1) THEN                 
         CALL CHECK(P(K),JUNIT(1),1)                                  
         CALL CHECK(T(K),JUNIT(2),2)                   
         CALL DEFALT(ZMDL(K),P(K),T(K))  
         if(p(k) .GT. 250.) then
c           then troposphere
            if(h2otscaled .EQ. 1) then
               wmol(1) = wmol(1)
     $              / 10**(
     $                     (.7859+.03477*t(k))
     $                     / (1+.00412*t(k))
     $                   )
     $              * 10**(
     $                     (.7859+.03477*(t(k)+toffset))
     $                     / (1+.00412*(t(k)+toffset))
     $                   )
            endif
            t(k) = t(k) + toffset
            wmol(3) = wmol(3) * tozonerat
            wmol(1) = wmol(1) * h2orat
         else
c           stratosphere
            wmol(3) = wmol(3) * sozonerat
         endif
         CALL CONVRT (P(K),T(K))                                      
         WH(K)    = WMOL(1)                                          
         WCO2(K)  = WMOL(2)                                           
         WO(K)    = WMOL(3)                                          
         WN2O(K)  = WMOL(4)                                          
         WCO(K)   = WMOL(5)                                          
         WCH4(K)  = WMOL(6)                                          
         WO2(K)   = WMOL(7)                                          
         WNO(K)   = WMOL(8)                                          
         WSO2(K)  = WMOL(9)                                          
         WNO2(K)  = WMOL(10)                                         
         WNH3(K)  = WMOL(11)                                         
         WHNO3(K) = WMOL(12)                                         
         WAIR(K)  = WAIR1                                            
         do 500 i = 1, nspecx
            wmolxt(i,k) = wmolx(i)
 500     continue
      ELSE                                                              
         CALL VSANSM(K,AHAZE,IHA1,znew)                                    
         ZMDL(K) = znew                                               
      ENDIF                      
C                                                                       
C                                                                       
C     GNDALT NOT ZERO                                                   
C                                                                       
      ZSC=ZMDL(K)                                                       
      IF((GNDALT.GT.0.).AND.(ZMDL(K).LT.6.0)) THEN                      
           ASC=6./(6.-GNDALT)                                           
           CON=-ASC*GNDALT                                              
           ZSC=ASC*ZMDL(K)+CON -1.e-4                                   
           IF(ZSC.LT.0.)ZSC=0.                                          
      ENDIF                                                             
      ZGN(K)=ZSC                                                        
C                                                                       
C                                                                       
      IF(ICLD1.EQ.0) ICLD1=ICLD                                         
      ICLDL = ICLD1                                                     
      IF(ICLD1.GT.11) ICLD1=0                                           
      IF(IHA1.NE.0) IVUL1=0                                             
      IF(IHA1.NE.0) ICLD1=0                                             
      IF(ICLD1.NE.0) IVUL1=0                                            
      IF((AHAZE.NE.0.).OR.(EQLWCZ.NE.0.)) GO TO 8                       
C      ********        ERRATA SEPT 19                                   
CC    IF(RRATZ.NE.0.) GO TO 8                                           
C      *****           END ERRATA                                       
      IF((IVSA.EQ.1).AND.(ICLD1.EQ.0)) THEN                             
           IF(MODEL.NE.7)CALL LAYVSA(K,RH,AHAZE,IHA1,ZMDL)              
      ELSE                                                              
           CALL LAYCLD(K,EQLWCZ,RRATZ,ICLD1,GNDALT,RAINRT)              
C          ***********************  ERRATA SEPT 19                      
           IF(RAINRT .GT. 0. AND. ZMDL(K) .LT. 6.)RRATZ = RAINRT        
C           *********************      END ERRATA                       
           IF(ICLD1 . LT.  1) GO TO 8                                   
           IF(ICLD1 . GT. 10) GO TO 8                                   
C     ***************  ERRATA JUNE 21 89 NEXT CARD                      
CC         IF(ZMDL(K). GT.CLDTOP(ICLD1) ) THEN                          
           IF(ZMDL(K). GT.CLDTOP(ICLD1)+ GNDALT ) THEN                  
C     ***************  END ERRATA                                       
              RRATZ = 0.                                                
           ENDIF                                                        
      ENDIF                                                             
8     CONTINUE                                                          
      ICLDC = ICLD                                                      
      IF(ICLDS .NE. 0) ICLDC = ICLDS                                    
C                                                                       
      IF(ICLDS. EQ. 18 .OR. ICLDS .EQ. 19) THEN                         
           IF(AHAZE . GT. 0) THEN                                       
                DENSTY(16,K) = AHAZE                                    
                AHAZE    = 0.                                           
                GO TO 46                                                
           ENDIF                                                        
           IF(EQLWCZ .GT. 0) THEN                                       
                IF(ICLDS .EQ. 18) CON = 3.446E-3                        
                IF(ICLDS .EQ. 19) CON = 5.811E-2                        
                DENSTY(16,K) = EQLWCZ/CON                               
                EQLWCZ = 0                                              
                GO TO 46                                                
           ENDIF                                                        
      ENDIF                                                             
      IF(ICLDC .EQ. 18 .OR. ICLDC.EQ. 19) THEN                          
           DENSTY(16,K) = 0.                                            
           IF(ZMDL(K)  .GE.  CLD1 .AND. ZMDL(K) .LE. CLD2)              
     X     DENSTY(16,K) = CEXT                                          
      ENDIF                                                             
46    CLDAMT(K)=EQLWCZ                                                  
      IF(ICLDS .EQ. 0. AND. CLDAMT(K) .EQ. 0.)ICLD1 = 0                 
      RRAMT(K)=RRATZ                                                    
      IF(MODEL  .EQ. 0 .OR. MODEL .EQ. 7) THEN                          
C     DONT CHANGE RH                                                    
      ELSE                                                              
            IF(EQLWCZ.GT.0.0) RH=100.0                                  
            IF(RRATZ .GT.0.0) RH=100.0                                  
      ENDIF                                                             
      AHAST(K)=AHAZE                                                    
C     IHA1  IS IHAZE FOR THIS LAYER                                     
C     ISEA1 IS ISEASN FOR THIS LAYER                                    
C     IVUL1 IS IVULCN FOR THE LAYER                                     
      IF(ISEA1.EQ.0) ISEA1=ISEASN                                       
      ITYAER=IHAZE                                                      
      IF(IHA1.GT.0) ITYAER=IHA1                                         
      IF(IVUL1.GT.0) IVULCN=IVUL1                                       
      IF(IVUL1.LE.0) IVUL1=IVULCN                                       
C                                                                       
      IF(K.EQ.1) GO TO 23                                               
      IF(ICHR .EQ. 1) GO TO 20                                          
      IF(ICLD1.NE.IREGC(IC1))GO TO 19                                   
      IF(IHA1 .EQ. 0 .AND. ICLD1. EQ. 0) THEN                           
           IF(ZSC.GT.2.)ITYAER=6                                        
           IF(ZSC.GT.10.)ITYAER=IVULCN+10                               
           IF(ZSC.GT.30.) ITYAER=19                                     
           IF(ITYAER.EQ.ICH(IC1))GO TO 23                               
      ENDIF                                                             
      IF(ICLD1 .EQ. 0 .AND. IHA1.EQ. 0) GO TO 20                        
      N = 7                                                             
      IF(IC1 .GT. 1) N= IC1 + 10                                        
      IF(IHA1 .EQ. 0) GO TO 23                                          
      IF(IHA1 .NE.ICH(IC1)) GO TO 20                                    
      GO TO 23                                                          
19    IF(ICLD1 .NE. 0) THEN                                             
           IF(ICLD1. EQ. IREGC(1)) THEN                                 
               N = 7                                                    
               ALTB(1) = ZMDL(K)                                        
               GO TO 24                                                 
           ENDIF                                                        
           IF(IC1 .EQ. 1) GO TO 20                                      
           IF(ICLD1. EQ. IREGC(2)) THEN                                 
               N = 12                                                   
               ALTB(2) = ZMDL(K)                                        
               GO TO 24                                                 
           ENDIF                                                        
           IF(IC1 .EQ. 2) GO TO 20                                      
           IF(ICLD1. EQ. IREGC(3)) THEN                                 
               N = 13                                                   
               ALTB(3) = ZMDL(K)                                        
               GO TO 24                                                 
           ENDIF                                                        
      ELSE                                                              
            IF(IHA1 .EQ. 0 .AND. ICLD1. EQ. 0) THEN                     
                 IF(ZSC.GT.2.)ITYAER=6                                  
                 IF(ZSC.GT.10.)ITYAER=IVULCN+10                         
                 IF(ZSC.GT.30.) ITYAER=19                               
            ENDIF                                                       
           IF(ITYAER.EQ.ICH(  1))THEN                                   
               N = 7                                                    
               ALTB(1) = ZMDL(K)                                        
               GO TO 24                                                 
           ENDIF                                                        
           IF(IC1 .EQ. 1) GO TO 20                                      
           IF(ITYAER.EQ.ICH(  2))THEN                                   
               N = 12                                                   
               ALTB(2) = ZMDL(K)                                        
               GO TO 24                                                 
           ENDIF                                                        
           IF(IC1 .EQ. 2) GO TO 20                                      
           IF(ITYAER.EQ.ICH(  3))THEN                                   
               N = 13                                                   
               ALTB(3) = ZMDL(K)                                        
               GO TO 24                                                 
           ENDIF                                                        
      ENDIF                                                             
20    IC1=IC1+1                                                         
C                                                                       
C                                                                       
C                                                                       
      N=IC1+10                                                          
      IF(RH.GT.0.) RHH=RH                                               
      IF(IC1.LE.4) GO TO 23                                             
      IC1=4                                                             
      N=14                                                              
      ITYAER=ICH(IC1)                                                   
23    ICH(IC1)=ITYAER                                                   
      IREGC(IC1) = ICLD1                                                
      ALTB(IC1) = ZMDL(K)                                               
C                                                                       
C     FOR LVSA OR CLD OR RAIN ONLY                                      
C                                                                       
24    IF (RH.GT.  0.0) THEN                                             
            TA=T0/T(K)                                                  
            WH(K)  =F(TA)*0.01*RH                                       
C                                                                       
      ENDIF                                                             
   40 CONTINUE                                                          
      DENSTY(7,K)=0.                                                    
      DENSTY(12,K)=0.                                                   
      DENSTY(13,K)=0.                                                   
      DENSTY(14,K)=0.                                                   
      DENSTY(15,K)=0.                                                   
      TS=273.15/T(K)                                                    
      WTEMP=WH(K)                                                       
      RELHUM(K)=0.                                                      
      IF(WTEMP.LE.0.) GO TO 45                                          
      RELHUM(K) = 100.0*WTEMP/F(TS)                                     
      IF(RELHUM(K).GT.100.)WRITE(IPR,930)RELHUM(K),ZMDL(K)              
      IF( RELHUM(K) .GT. 100.) RELHUM(K)=100.                           
      IF(RELHUM(K).LT.0.)WRITE(IPR,930)RELHUM(K),ZMDL(K)                
930   FORMAT(' ***ERROR RELHUM ' ,E15.4,'  AT ALT  ',F12.3)             
      IF( RELHUM(K) .LT.   0.) RELHUM(K)=0.                             
45    RHH=RELHUM(K)                                                     
      RH=RHH                                                            
      IF (VIS1.LE.0.0) VIS1=VIS                                         
      IF (AHAZE.EQ.0.0) GO TO 47                                        
      DENSTY(N,K)=AHAZE                                                 
      IF(ITYAER.EQ.3) GO TO 47                                          
      IF(ITYAER.EQ.10)GO TO 47                                          
C     AHAZE IS IN LOWTRAN NUMBER DENSTY UNITS                           
      GO TO 50                                                          
47    CONTINUE                                                          
C                                                                       
C     AHAZE NOT INPUT OR NAVY MARITIME MODEL IS CALLED                  
C                                                                       
C     CHECK IF GNDALT NOT ZERO                                          
C                                                                       
      IF((GNDALT.GT.0.).AND.(ZMDL(K).LT.6.0)) THEN                      
           J=IFIX(ZSC+1.0E-6)+1                                         
           FAC=ZSC-FLOAT(J-1)                                           
      ELSE                                                              
      J=IFIX(ZMDL(K)+1.0E-6)+1                                          
      IF (ZMDL(K).GE.25.0) J=(ZMDL(K)-25.0)/5.0+26.                     
      IF (ZMDL(K).GE.50.0) J=(ZMDL(K)-50.0)/20.0+31.                    
      IF (ZMDL(K).GE.70.0) J=(ZMDL(K)-70.0)/30.0+32.                    
      IF (J.GT.32) J=32                                                 
      FAC=ZMDL(K)-FLOAT(J-1)                                            
      IF (J.LT.26) GO TO 125                                            
      FAC=(ZMDL(K)-5.0*FLOAT(J-26)-25.)/5.                              
      IF (J.GE.31) FAC=(ZMDL(K)-50.0)/20.                               
      IF (J.GE.32) FAC=(ZMDL(K)-70.0)/30.                               
      ENDIF                                                             
125   L=J+1                                                             
      IF (FAC.GT.1.0) FAC=1.0                                           
      IF(ITYAER.EQ.3.AND.ICL.EQ.0)THEN                                  
           CALL MARINE(VIS1,MODEL,WSS,WHH,ICSTL,EXTC,ABSC,IC1)          
           IREG(IC1) = 1                                                
           VIS=VIS1                                                     
           ICL = ICL + 1                                                
      ENDIF                                                             
      IF(ITYAER.EQ.10.AND.IDSR.EQ.0)THEN                                
           CALL DESATT(WSS,VIS1)                                        
           IREG(IC1) = 1                                                
           VIS=VIS1                                                     
           IDSR = IDSR + 1                                              
      ENDIF                                                             
      IF(AHAZE.GT.0.0) GO TO 50                                         
      IF(IHA1.LE.0) IHA1=IHAZE                                          
      CALL CLDPRF(K,ICLD1,IHA1,IC1)                                     
      CALL AERPRF(J,  VIS1,HAZ1,IHA1,      ISEA1,IVUL1,NN)              
      CALL AERPRF(L,  VIS1,HAZ2,IHA1,      ISEA1,IVUL1,NN)              
      HAZE=0.                                                           
      IF ((HAZ1.LE.0.0).OR.(HAZ2.LE.0.0)) GO TO 48                      
      HAZE=HAZ1*(HAZ2/HAZ1)**FAC                                        
48    CONTINUE                                                          
      IF(CLDAMT(K).GT.0.0) HAZE=HAZEC(K)                                
      DENSTY(N,K)=HAZE                                                  
50    CONTINUE                                                          
      ITY1(K)=ITYAER                                                    
      IH1(K)=IHA1                                                       
      IF(AHAZE.NE.0)IH1(K)=-99                                          
      IS1(K)=ISEA1                                                      
      IVL1(K)=IVUL1                                                     
100   CONTINUE                                                          
C                                                                       
C     END OF LOOP                                                       
C                                                                       
      IHH=ICLD                                                          
      IF(IHH.LE.0) IHH=12                                               
      IF(IHH.GT.12)IHH=12                                               
      IF(ICLD.GE.18)IHH=13                                              
      DO 105 II=1,5                                                     
      HHOL(II)=AHAHOL(II,IHH)                                           
      IF(IVSA.NE.0) HHOL(II)=AHLVSA(II)                                 
105   CONTINUE                                                          
      IF(ICLD .NE. 0) THEN                                              
           IF(JPRT.EQ.0)WRITE (IPR,904) HHOL                            
904        FORMAT(//'0 CLOUD AND OR RAIN TYPE CHOSEN IS   ',5A4)        
      ENDIF                                                             
      IF(JPRT.EQ.0)WRITE(IPR,905)                                       
C                                                                       
C 905 FORMAT(//,T7,'Z',T17,'P',T26,'T',T32,'REL H', T41,'H2O',          
905   FORMAT(1H1,//,T7,'Z',T17,'P',T26,'T',T32,'REL H', T41,'H2O',      
     1 T49,'CLD AMT',T59,'RAIN RATE', T90,'AEROSOL'/,                   
     2 T6,'(KM)',T16,'(MB)',T25,'(K)',T33,'(%)',T39,'(GM M-3)',T49,     
     3 '(GM M-3)',T59,'(MM HR-1)',T69,                                  
     4 'TYPE', T90,'PROFILE')                                           
      IF(JPRT.EQ.1) RETURN                                              
      DO 60 KK=1,ML                                                     
      DO 52 IJ=1,5                                                      
      AHOL1(IJ)=BLANK                                                   
      AHOL2(IJ)=BLANK                                                   
52    AHOL3(IJ)=BLANK                                                   
      ITYAER=ITY1(KK)                                                   
      IF(ITYAER.LE.0) ITYAER=1                                          
      IF(ITYAER . EQ.  16) ITYAER = 11                                  
      IF(ITYAER . EQ.  17) ITYAER = 11                                  
      IF(ITYAER . EQ.  18) ITYAER = 13                                  
C     ***************  ERRATA JUNE 21 89 NEXT CARD                      
      IF(ITYAER . EQ.  19) ITYAER = 11                                  
C     ***************  END ERRATA                                       
      IHA1=IH1(KK)                                                      
      ISEA1=IS1(KK)                                                     
      IVUL1=IVL1(KK)                                                    
      DO 54 IJ=1,5                                                      
      AHOL1(IJ)=  HZ(IJ,ITYAER)                                         
      IF(IVSA.EQ.1) AHOL1(IJ)=HHOL(IJ)                                  
      IF(CLDAMT(KK).GT.0.0 .OR. RRAMT(KK).GT.0.0) AHOL1(IJ)=HHOL(IJ)    
      IF(IHAZE.EQ.0) AHOL1(IJ)=HHOL(IJ)                                 
      AHOL2(IJ)=AHUS(IJ)                                                
      IF(AHAST(KK).EQ.0) AHOL2(IJ)=AHOL1(IJ)                            
      IF(CLDAMT(KK).GT.0.0 .OR. RRAMT(KK).GT.0.0) AHOL2(IJ)=HHOL(IJ)    
54    IF (ZGN(KK).GT.2.0) AHOL3(IJ)=SEASN(IJ,ISEA1)                     
60    WRITE(IPR,915)ZMDL(KK),P(KK  ),T(KK  ),RELHUM(KK),WH(KK  ),       
     X CLDAMT(KK),RRAMT(KK),AHOL1,AHOL2,AHOL3                           
915   FORMAT(2F10.3,2F8.2,1P3E10.3,1X,5A4,5A4,5A4)                      
      RETURN                                                            
C                                                                       
925   FORMAT(//,' MODEL ATMOSPHERE NO. ',I5,' ICLD =',I5,//)            
      END                                                               
      SUBROUTINE AERPRF (I,  VIS,HAZE,IHAZE,     ISEASN,IVULCN,N)       aerp 100
C***********************************************************************aerp 110
C     WILL COMPUTE DENSITY    PROFILES FOR AEROSOLS                     aerp 120
C***********************************************************************aerp 130
      COMMON/PRFD  /ZHT(34),HZ2K(34,5),FAWI50(34),
     1FAWI23(34),SPSU50(34),SPSU23(34),BASTFW(34),
     2VUMOFW(34),HIVUFW(34),EXVUFW(34),BASTSS(34),
     3VUMOSS(34),HIVUSS(34),EXVUSS(34),UPNATM(34),
     3VUTONO(34),VUTOEX(34),EXUPAT(34)
      DIMENSION VS(5)                                                   aerp 180
      DATA VS/50.,23.,10.,5.,2./                                        aerp 190
      HAZE=0.                                                           aerp 200
      N=7                                                               aerp 210
      IF (IHAZE.EQ.0) RETURN                                            aerp 220
      IF (ZHT(I).GT.2.0) GO TO 15                                       aerp 230
      DO 5 J=2,5                                                        aerp 240
      IF (VIS.GE.VS(J)) GO TO 10                                        aerp 250
    5 CONTINUE                                                          aerp 260
      J=5                                                               aerp 270
   10 CONST=1./(1./VS(J)-1./VS(J-1))                                    aerp 280
      HAZE=CONST*( (HZ2K(I,J)-HZ2K(I,J-1))/VIS +                        aerp 290
     1 HZ2K(I,J-1)/VS(J) - HZ2K(I,J )/VS(J-1) )                         aerp 300
      IF(ZHT(I).GT.2.0) GO TO 15                                        aerp 310
      RETURN                                                            aerp 320
   15 IF (ZHT(I).GT.10.) GO TO 35                                       aerp 330
      CONST=1./(1./23.-1./50.)                                          aerp 340
      IF (ISEASN.GT.1) GO TO 25                                         aerp 350
      IF (VIS.LE.23.) HAZI=SPSU23(I)                                    aerp 360
      IF (VIS.LE.23.) GO TO 200                                         aerp 370
      IF (ZHT(I).GT.4.0) GO TO 20                                       aerp 380
      HAZI=CONST*((SPSU23(I)-SPSU50(I))/VIS+SPSU50(I)/23.-SPSU23(I)/50.)aerp 390
      GO TO 200                                                         aerp 400
   20 HAZI=SPSU50(I)                                                    aerp 410
      GO TO 200                                                         aerp 420
   25 IF (VIS.LE.23.) HAZI=FAWI23(I)                                    aerp 430
      IF (VIS.LE.23.) GO TO 200                                         aerp 440
      IF (ZHT(I).GT.4.0) GO TO 30                                       aerp 450
      HAZI=CONST*((FAWI23(I)-FAWI50(I))/VIS+FAWI50(I)/23.-FAWI23(I)/50.)aerp 460
      GO TO 200                                                         aerp 470
   30 HAZI=FAWI50(I)                                                    aerp 480
      GO TO 200                                                         aerp 490
   35 IF (ZHT(I).GT.30.0) GO TO 75                                      aerp 500
      HAZI=BASTSS(I)                                                    aerp 510
      IF (ISEASN.GT.1) GO TO 55                                         aerp 520
      IF (IVULCN.EQ.0) HAZI=BASTSS(I)                                   aerp 530
      IF (IVULCN.EQ.0) GO TO 200                                        aerp 540
      GO TO (40,45,50,50,45,45,50,52), IVULCN                           aerp 550
   40 HAZI=BASTSS(I)                                                    aerp 560
      GO TO 200                                                         aerp 570
   45 HAZI=VUMOSS(I)                                                    aerp 580
      GO TO 200                                                         aerp 590
   50 HAZI=HIVUSS(I)                                                    aerp 600
      GO TO 200                                                         aerp 610
52    HAZI=EXVUSS(I)                                                    aerp 620
      GO TO 200                                                         aerp 630
   55 IF (IVULCN.EQ.0) HAZI=BASTFW(I)                                   aerp 640
      IF (IVULCN.EQ.0) GO TO 200                                        aerp 650
      GO TO (60,65,70,70,65,65,70,72), IVULCN                           aerp 660
   60 HAZI=BASTFW(I)                                                    aerp 670
      GO TO 200                                                         aerp 680
   65 HAZI=VUMOFW(I)                                                    aerp 690
      GO TO 200                                                         aerp 700
   70 HAZI=HIVUFW(I)                                                    aerp 710
      GO TO 200                                                         aerp 720
72    HAZI=EXVUFW(I)                                                    aerp 730
      GO TO 200                                                         aerp 740
   75 N=14                                                              aerp 750
      IF (IVULCN.GT.1) GO TO 80                                         aerp 760
      HAZI=UPNATM(I)                                                    aerp 770
      GO TO 200                                                         aerp 780
   80 HAZI=VUTONO(I)                                                    aerp 790
200   IF(HAZI.GT.0) HAZE=HAZI                                           aerp 800
      RETURN                                                            aerp 810
      END                                                               aerp 820
      SUBROUTINE AERTMP                                                 aert 100
      include 'parameter.list'
      COMMON RELHUM(laydim),HSTOR(laydim),ICH(4),VH(17),TX(65),W(65)  
      COMMON IMSMX,WPATH(laythr,65),TBBY(laythr),PATM(laythr),NSPEC,   
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX2(47),AWCCON(5)  
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON /CNTRL/ KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT               aert 160
      COMMON /AERTM/TAE7,TAE12,TAE13,TAE14,TAE16                        aert 170
      SUM7  = 0.                                                        aert 180
      SUM12 = 0.                                                        aert 190
      SUM13 = 0.                                                        aert 200
      SUM14 = 0.                                                        aert 210
      SUM16 = 0.                                                        aert 220
      TAE7  = 0.                                                        aert 230
      TAE12 = 0.                                                        aert 240
      TAE13 = 0.                                                        aert 250
      TAE14 = 0.                                                        aert 260
      TAE16 = 0.                                                        aert 270
      DO 10 KK = 1,IKMAX                                                aert 280
      IF(KK .EQ. 1) THEN                                                aert 290
           WDF7  = WPATH(KK,7)                                          aert 300
           WDF12 = WPATH(KK,12)                                         aert 310
           WDF13 = WPATH(KK,13)                                         aert 320
           WDF14 = WPATH(KK,14)                                         aert 330
           WDF16 = WPATH(KK,16)                                         aert 340
      ELSE                                                              aert 350
           WDF7  = WPATH(KK,7) -WPATH(KK-1,7)                           aert 360
           WDF12 = WPATH(KK,12)-WPATH(KK-1,12)                          aert 370
           WDF13 = WPATH(KK,13)-WPATH(KK-1,13)                          aert 380
           WDF14 = WPATH(KK,14)-WPATH(KK-1,14)                          aert 390
           WDF16 = WPATH(KK,16)-WPATH(KK-1,16)                          aert 400
      ENDIF                                                             aert 410
      SUM7 = SUM7  + WDF7                                               aert 420
      SUM12= SUM12 + WDF12                                              aert 430
      SUM13= SUM13 + WDF13                                              aert 440
      SUM14= SUM14 + WDF14                                              aert 450
      SUM16= SUM16 + WDF16                                              aert 460
      TAE7 = TAE7  + WDF7   * TBBY(KK)                                  aert 470
      TAE12= TAE12 + WDF12  * TBBY(KK)                                  aert 480
      TAE13= TAE13 + WDF13  * TBBY(KK)                                  aert 490
      TAE14= TAE14 + WDF14  * TBBY(KK)                                  aert 500
   10 TAE16= TAE16 + WDF16  * TBBY(KK)                                  aert 510
      IF(SUM7   .GT. 0.) THEN                                           aert 520
            TAE7 = TAE7 /SUM7                                           aert 530
      ELSE                                                              aert 540
            TAE7 = TBBY(1)                                              aert 550
      ENDIF                                                             aert 560
      IF(SUM12 .GT. 0.) THEN                                            aert 570
            TAE12= TAE12/SUM12                                          aert 580
      ELSE                                                              aert 590
            TAE12= TBBY(1)                                              aert 600
      ENDIF                                                             aert 610
      IF(SUM13 .GT. 0.) THEN                                            aert 620
            TAE13= TAE13/SUM13                                          aert 630
      ELSE                                                              aert 640
            TAE13= TBBY(1)                                              aert 650
      ENDIF                                                             aert 660
      IF(SUM14 .GT. 0.) THEN                                            aert 670
            TAE14= TAE14/SUM14                                          aert 680
      ELSE                                                              aert 690
            TAE14= TBBY(1)                                              aert 700
      ENDIF                                                             aert 710
      IF(SUM16 .GT. 0.) THEN                                            aert 720
            TAE16= TAE16/SUM16                                          aert 730
      ELSE                                                              aert 740
            TAE16= TBBY(1)                                              aert 750
      ENDIF                                                             aert 760
      RETURN                                                            aert 770
      END                                                               aert 780
      FUNCTION   AITK(ARG,VAL,X,NDIM)                                   aitk 100
C                                                                       aitk 110
C      IBM SCIENTIFIC SUBROUTINE                                        aitk 120
C     AITKEN INTERPOLATION ROUTINE                                      aitk 130
C                                                                       aitk 140
      DIMENSION ARG(NDIM),VAL(NDIM)                                     aitk 150
      IF(NDIM-1)9,7,1                                                   aitk 160
C                                                                       aitk 170
C     START OF AITKEN-LOOP                                              aitk 180
    1 DO 6 J=2,NDIM                                                     aitk 190
      IEND=J-1                                                          aitk 200
      DO 2 I=1,IEND                                                     aitk 210
      H=ARG(I)-ARG(J)                                                   aitk 220
      IF(H)2,13,2                                                       aitk 230
    2 VAL(J)=(VAL(I)*(X-ARG(J))-VAL(J)*(X-ARG(I)))/H                    aitk 240
    6 CONTINUE                                                          aitk 250
C     END OF AITKEN-LOOP                                                aitk 260
C                                                                       aitk 270
    7 J=NDIM                                                            aitk 280
    8 AITK=VAL(J)                                                       aitk 290
    9 RETURN                                                            aitk 300
C                                                                       aitk 310
C     THERE ARE TWO IDENTICAL ARGUMENT VALUES IN VECTOR ARG             aitk 320
   13 continue
ccc   IER=3                                                             aitk 330
      J=IEND                                                            aitk 340
      GO TO 8                                                           aitk 350
      END                                                               aitk 360
      BLOCK DATA ATMCON                                                   107670
C***********************************************************************  107680
C     THIS SUBROUTINE INITIALIZES THE CONSTANTS USED IN THE               107690
C     PROGRAM. CONSTANTS RELATING TO THE ATMOSPHERIC PROFILES ARE STORED  107700
C     IN BLOCK DATA MLATMB.                                               107710
C***********************************************************************  107720
cc    COMMON /PARMTR/ PI,DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,      107730
cc   1    IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,IPHMID,IPDIM,KDIM,        107740
cc   2    KMXNOM,NMOL                                                     107750
      COMMON /CONSTN/ PZERO,TZERO,AVOGAD,ALOSMT,GASCON,PLANK,BOLTZ,       107760
     1    CLIGHT,ADCON,ALZERO,AVMWT,AIRMWT,AMWT(35)                       107770
      DOUBLE PRECISION HMOLS                                            & 107780
      COMMON/HMOLS/HMOLS(35),JUNIT(35),WMOL(35),JUNITP,JUNITT            <107790
      COMMON/HMOLC/HMOLC(35)
      CHARACTER*8 HMOLC
C
C**   IMDIM IS THE MAX NUMBER OF LEVELS IN THE ATMOSPHERIC PROFILE        107800
C**       STORED IN ZMDL                                                  107810
C**   IOUTDM IS THE MAXIMUN NUMBER OF OUTPUT LAYERS                       107820
C**   IPDIM IS THE MAXIMUM NUMBER OF LEVELS IN THE PROFILE ZPTH OBTAINED  107830
C**       BY MERGING ZMDL AND ZOUT                                        107840
C**   KDIM ID THE MAXIMUM NUMBER OF MOLECULES, KMXNOM IS THE DEFAULT      107850
C**   IBDIM IS THE MAXIMUM NUMBER OF INPUT FASCODE LAYERS                 107860
C
CC    DATA IMDIM/64/,IOUTDM/67/,IPDIM/131/,KDIM/35/,KMXNOM/7/             107870
CC    DATA IBDIM/64/                                                      107880
C
C**   DELTAS IS THE NOMINAL SLANT PATH INCREMENT IN KM.                   107890
C
CC    DATA DELTAS/5.0/                                                    107900
      DATA PZERO/1013.25/,TZERO/273.15/                                   107910
      DATA AVOGAD/6.022045E+23/,ALOSMT/2.68675E+19/,                      107920
     1    GASCON/8.31441E+7/,PLANK/6.626176E-27/,BOLTZ/1.380662E-16/,     107930
     2    CLIGHT/2.99792458E10/                                           107940
C
C**   ALZERO IS THE MEAN LORENTZ HALFWIDTH AT PZERO AND 296.0 K.          107950
C**   AVMWT IS THE MEAN MOLECULAR WEIGHT USED TO AUTOMATICALLY            107960
C**   GENERATE THE FASCODE BOUNDARIES IN AUTLAY                           107970
C
      DATA ALZERO/0.1/,AVMWT/36.0/                                        107980
C
C**   ORDER OF MOLECULES H2O(1), CO2(2), O3(3), N2O(4), CO(5), CH4(6),    107990
C**       O2(7), NO(8), SO2(9), NO2(10), NH3(11), HNO3(12), OH(13),       108000
C**       HF(14 ), HCL(15), HBR(16), HI(17), CLO(18), OCS(19), H2CO(20)   108010
C**       HOCL(21), N2(22), HCN(23), CH3CL(24), H2O2(25), C2H2(26),       108020
C**       C2H6(27), PH3(28),cof2(29),   sf6(30)                            108030
C
       DATA HMOLC   /'  H2O   ','  CO2   ','   O3   ',                    108040
     1    '  N2O   ','   CO   ','  CH4   ','   O2   ',                    108050
     2    '   NO   ','  SO2   ','  NO2   ','  NH3   ',                    108060
     3    ' HNO3   ','   OH   ','   HF   ','  HCL   ',                    108070
     4    '  HBR   ','   HI   ','  CLO   ','  OCS   ',                    108080
     5    ' H2CO   ',' HOCL   ','   N2   ','  HCN   ',                    108090
     6    ' CH3CL  ',' H2O2   ',' C2H2   ',' C2H6   ',                    108100
     7    '  PH3   ',' COF2   ','  SF6   ',' H2S    ',
     8    ' HCOOH  ',3*'        '/                                        108110
C
C**   MOLECULAR WEIGHTS                                                   108120
C
      DATA AIRMWT/28.964/,AMWT/18.015,44.010,47.998,44.01,28.011,         108130
     1    16.043,31.999,30.01,64.06,46.01,17.03,63.01,17.00,20.01,        108140
     2    36.46,80.92,127.91,51.45,60.08,30.03,52.46,28.014,              108150
     3    27.03, 50.49, 34.01, 26.03, 30.07, 34.00,66.0,146., 
     4    34.08,46.016,3*0./                                              108160
      END                                                                 108170
      FUNCTION   BBFN(T,V)                                              bbfn 100
C***********************************************************************bbfn 110
C   PLANCK BLACK BODY FUNCTION IN UNITS OF WATTS/(CM2 STER MICROMETER)  bbfn 120
C***********************************************************************bbfn 130
      COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     bbfn 140
      BBFN = 0.0                                                        bbfn 150
      IF(V. LE. 0.) RETURN                                              bbfn 160
      X = 1.43879*V/T                                                   bbfn 170
C*****PROTECT AGAINST EXPONENTIAL OVERLOW                               bbfn 180
      IF(X.GT.BIGEXP) RETURN                                            bbfn 190
      BBFN = 1.190956E-16*V**5/(EXP(X)-1.0)                             bbfn 200
      RETURN                                                            bbfn 210
      END                                                               bbfn 220
      FUNCTION   BETABS(U,G)                                            beta 100
C                                                                       beta 110
C   FUNCTION BETABS SUPPLIES THE BACK SCATTER FRACTION FOR A GIVEN      beta 120
C   ASYMMETRY FACTOR AND COSINE OF ANGLE                                beta 130
C                                             A.E.R. 1986               beta 140
C                                                                       beta 150
      DIMENSION A9(10,5),U8(9)                                          beta 160
      DATA U8/0.0,.1,.2,.3,.4,.5,.6,.8,1.0/                             beta 170
      DATA A9/.5, .5, .5, .5, .5, .5, .5, .5, .5, .5, .0, .13979292889, beta 180
     1   -.12019000482,   -.46017123188,  -.406828796532, -.3001541656, beta 190
     2   -.553474414782,  -.626794663911, -.84678101,   -.406823676662, beta 200
     3  .0, -1.5989873995, -.2724219928,  1.18747390274, .49409050834,  beta 210
     4   -.35928947292,    .37397957743,   .18057740986, .50718036245,  beta 220
     5    .01406832224, .0, 3.5184116963,  .6385960123, -2.081230849,   beta 230
     6  -1.0144699491,     .1589475781,   -.74761782865, -.37416958202, beta 240
     7   -.374040109,   .1055607702, .0, -2.5592172257,  -.7459840146,  beta 250
     8    .85392041407,    .4272082373,    .00049606046,  .42711266606, beta 260
     9    .32038683614,    .2136746594,   -.2128054157 /                beta 270
C                                                                       beta 280
C     BACKSCATTERING INTERPOLATION                                      beta 290
C                                                                       beta 300
      IQ=0                                                              beta 310
      IF(U .LT. .1) IQ=1                                                beta 320
      IF(U .LT. .2 .AND. U .GE. .1) IQ=2                                beta 330
      IF(U .LT. .3 .AND. U .GE. .2) IQ=3                                beta 340
      IF(U .LT. .4 .AND. U .GE. .3) IQ=4                                beta 350
      IF(U .LT. .5 .AND. U .GE. .4) IQ=5                                beta 360
      IF(U .LT. .6 .AND. U .GE. .5) IQ=6                                beta 370
      IF(U .LT. .8 .AND. U .GE. .6) IQ=7                                beta 380
      IF(U .LE.1.0 .AND. U .GE. .8) IQ=8                                beta 390
      IF (IQ .EQ. 0) THEN                                               beta 400
      WRITE(6,105) U                                                    beta 410
  105 FORMAT(//,' THE VALUE FOR MU OF ',F12.6,' IS > 1.0 ]]')           beta 420
      STOP ' ERROR ***'                                                 beta 430
      END IF                                                            beta 440
C                                                                       beta 450
      Y5=0.0                                                            beta 460
      Y6=0.0                                                            beta 470
      Y7=0.0                                                            beta 480
      IQP1=IQ+1                                                         beta 490
C                                                                       beta 500
      DO 30 J=1,5                                                       beta 510
      JM1=J-1                                                           beta 520
      Y9=A9(IQ,J)*G**JM1                                                beta 530
      Y8=A9(IQP1,J)*G**JM1                                              beta 540
      Y3=A9(10,J)*G**JM1                                                beta 550
      Y7=Y7+Y9                                                          beta 560
      Y6=Y6+Y8                                                          beta 570
      Y5=Y5+Y3                                                          beta 580
  30  CONTINUE                                                          beta 590
      BETABS=Y7+(Y6-Y7)*(U-U8(IQ))/(U8(IQP1)-U8(IQ))                    beta 600
C                                                                       beta 610
      RETURN                                                            beta 620
      END                                                               beta 630
      BLOCK DATA BFH2O                                                  bh2o 100
C>    BLOCK DATA                                                        bh2o 110
C               06/28/82                                                bh2o 120
C               UNITS OF (CM**3/MOL)*1.E-20                             bh2o 130
      COMMON /FH2O/ V1,V2,DV,NPT,F0000(2),                              bh2o 140
     1      F0001(50),F0051(50),F0101(50),F0151(50),F0201(50),F0251(50),bh2o 150
     2      F0301(50),F0351(50),F0401(50),F0451(50),F0501(50),F0551(50),bh2o 160
     3      F0601(50),F0651(50),F0701(50),F0751(50),F0801(50),F0851(50),bh2o 170
     4      F0901(50),F0951(50),F1001(50),F1051(50),F1101(50),F1151(50),bh2o 180
     5      F1201(50),F1251(50),F1301(50),F1351(50),F1401(50),F1451(50),bh2o 190
     6      F1501(50),F1551(50),F1601(50),F1651(50),F1701(50),F1751(50),bh2o 200
     7      F1801(50),F1851(50),F1901(50),F1951(50),F2001(1)            bh2o 210
C                                                                       bh2o 220
C                                                                       bh2o 230
       DATA V1,V2,DV,NPT /                                              bh2o 240
     1      -20.0,     20000.0,       10.0,  2003/                      bh2o 250
C                                                                       bh2o 260
C                                                                       bh2o 270
      DATA F0000/ 1.2859E-02, 1.1715E-02/                               bh2o 280
      DATA F0001/                                                       bh2o 290
     X 1.1038E-02, 1.1715E-02, 1.2859E-02, 1.5326E-02, 1.6999E-02,      bh2o 300
     X 1.8321E-02, 1.9402E-02, 1.9570E-02, 1.9432E-02, 1.7572E-02,      bh2o 310
     X 1.6760E-02, 1.5480E-02, 1.3984E-02, 1.2266E-02, 1.0467E-02,      bh2o 320
     X 9.4526E-03, 8.0485E-03, 6.9484E-03, 6.1416E-03, 5.0941E-03,      bh2o 330
     X 4.4836E-03, 3.8133E-03, 3.4608E-03, 3.1487E-03, 2.4555E-03,      bh2o 340
     X 2.0977E-03, 1.7266E-03, 1.4920E-03, 1.2709E-03, 9.8081E-04,      bh2o 350
     X 8.5063E-04, 6.8822E-04, 5.3809E-04, 4.4679E-04, 3.3774E-04,      bh2o 360
     X 2.7979E-04, 2.1047E-04, 1.6511E-04, 1.2993E-04, 9.3033E-05,      bh2o 370
     X 7.4360E-05, 5.6428E-05, 4.5442E-05, 3.4575E-05, 2.7903E-05,      bh2o 380
     X 2.1374E-05, 1.6075E-05, 1.3022E-05, 1.0962E-05, 8.5959E-06/      bh2o 390
      DATA F0051/                                                       bh2o 400
     X 6.9125E-06, 5.3808E-06, 4.3586E-06, 3.6394E-06, 2.9552E-06,      bh2o 410
     X 2.3547E-06, 1.8463E-06, 1.6036E-06, 1.3483E-06, 1.1968E-06,      bh2o 420
     X 1.0333E-06, 8.4484E-07, 6.7195E-07, 5.0947E-07, 4.2343E-07,      bh2o 430
     X 3.4453E-07, 2.7830E-07, 2.3063E-07, 1.9951E-07, 1.7087E-07,      bh2o 440
     X 1.4393E-07, 1.2575E-07, 1.0750E-07, 8.2325E-08, 5.7524E-08,      bh2o 450
     X 4.4482E-08, 3.8106E-08, 3.4315E-08, 2.9422E-08, 2.5069E-08,      bh2o 460
     X 2.2402E-08, 1.9349E-08, 1.6152E-08, 1.2208E-08, 8.9660E-09,      bh2o 470
     X 7.1322E-09, 6.1028E-09, 5.2938E-09, 4.5350E-09, 3.4977E-09,      bh2o 480
     X 2.9511E-09, 2.4734E-09, 2.0508E-09, 1.8507E-09, 1.6373E-09,      bh2o 490
     X 1.5171E-09, 1.3071E-09, 1.2462E-09, 1.2148E-09, 1.2590E-09/      bh2o 500
      DATA F0101/                                                       bh2o 510
     X 1.3153E-09, 1.3301E-09, 1.4483E-09, 1.6944E-09, 2.0559E-09,      bh2o 520
     X 2.2954E-09, 2.6221E-09, 3.2606E-09, 4.2392E-09, 5.2171E-09,      bh2o 530
     X 6.2553E-09, 8.2548E-09, 9.5842E-09, 1.1280E-08, 1.3628E-08,      bh2o 540
     X 1.7635E-08, 2.1576E-08, 2.4835E-08, 3.0014E-08, 3.8485E-08,      bh2o 550
     X 4.7440E-08, 5.5202E-08, 7.0897E-08, 9.6578E-08, 1.3976E-07,      bh2o 560
     X 1.8391E-07, 2.3207E-07, 2.9960E-07, 4.0408E-07, 5.9260E-07,      bh2o 570
     X 7.8487E-07, 1.0947E-06, 1.4676E-06, 1.9325E-06, 2.6587E-06,      bh2o 580
     X 3.4534E-06, 4.4376E-06, 5.8061E-06, 7.0141E-06, 8.4937E-06,      bh2o 590
     X 1.0186E-05, 1.2034E-05, 1.3837E-05, 1.6595E-05, 1.9259E-05,      bh2o 600
     X 2.1620E-05, 2.3681E-05, 2.7064E-05, 3.2510E-05, 3.5460E-05/      bh2o 610
      DATA F0151/                                                       bh2o 620
     X 3.9109E-05, 4.2891E-05, 4.7757E-05, 5.0981E-05, 5.0527E-05,      bh2o 630
     X 4.8618E-05, 4.4001E-05, 3.7982E-05, 3.2667E-05, 2.7794E-05,      bh2o 640
     X 2.4910E-05, 2.4375E-05, 2.7316E-05, 3.2579E-05, 3.5499E-05,      bh2o 650
     X 3.8010E-05, 4.1353E-05, 4.3323E-05, 4.3004E-05, 3.9790E-05,      bh2o 660
     X 3.7718E-05, 3.6360E-05, 3.2386E-05, 2.7409E-05, 2.3626E-05,      bh2o 670
     X 2.0631E-05, 1.8371E-05, 1.5445E-05, 1.2989E-05, 1.1098E-05,      bh2o 680
     X 9.6552E-06, 8.0649E-06, 7.2365E-06, 5.9137E-06, 5.2759E-06,      bh2o 690
     X 4.8860E-06, 4.1321E-06, 3.5918E-06, 2.7640E-06, 2.4892E-06,      bh2o 700
     X 2.1018E-06, 1.7848E-06, 1.5855E-06, 1.3569E-06, 1.1986E-06,      bh2o 710
     X 9.4693E-07, 7.4097E-07, 6.3443E-07, 4.8131E-07, 4.0942E-07/      bh2o 720
      DATA F0201/                                                       bh2o 730
     X 3.3316E-07, 2.8488E-07, 2.3461E-07, 1.7397E-07, 1.4684E-07,      bh2o 740
     X 1.0953E-07, 8.5396E-08, 6.9261E-08, 5.4001E-08, 4.5430E-08,      bh2o 750
     X 3.2791E-08, 2.5995E-08, 2.0225E-08, 1.5710E-08, 1.3027E-08,      bh2o 760
     X 1.0229E-08, 8.5277E-09, 6.5249E-09, 5.0117E-09, 3.9906E-09,      bh2o 770
     X 3.2332E-09, 2.7847E-09, 2.4570E-09, 2.3359E-09, 2.0599E-09,      bh2o 780
     X 1.8436E-09, 1.6559E-09, 1.4910E-09, 1.2794E-09, 9.8229E-10,      bh2o 790
     X 8.0054E-10, 6.0769E-10, 4.5646E-10, 3.3111E-10, 2.4428E-10,      bh2o 800
     X 1.8007E-10, 1.3291E-10, 9.7974E-11, 7.8271E-11, 6.3833E-11,      bh2o 810
     X 5.4425E-11, 4.6471E-11, 4.0209E-11, 3.5227E-11, 3.1212E-11,      bh2o 820
     X 2.8840E-11, 2.7762E-11, 2.7935E-11, 3.2012E-11, 3.9525E-11/      bh2o 830
      DATA F0251/                                                       bh2o 840
     X 5.0303E-11, 6.8027E-11, 9.3954E-11, 1.2986E-10, 1.8478E-10,      bh2o 850
     X 2.5331E-10, 3.4827E-10, 4.6968E-10, 6.2380E-10, 7.9106E-10,      bh2o 860
     X 1.0026E-09, 1.2102E-09, 1.4146E-09, 1.6154E-09, 1.7510E-09,      bh2o 870
     X 1.8575E-09, 1.8742E-09, 1.8700E-09, 1.8582E-09, 1.9657E-09,      bh2o 880
     X 2.1204E-09, 2.0381E-09, 2.0122E-09, 2.0436E-09, 2.1213E-09,      bh2o 890
     X 2.0742E-09, 1.9870E-09, 2.0465E-09, 2.1556E-09, 2.2222E-09,      bh2o 900
     X 2.1977E-09, 2.1047E-09, 1.9334E-09, 1.7357E-09, 1.5754E-09,      bh2o 910
     X 1.4398E-09, 1.4018E-09, 1.5459E-09, 1.7576E-09, 2.1645E-09,      bh2o 920
     X 2.9480E-09, 4.4439E-09, 5.8341E-09, 8.0757E-09, 1.1658E-08,      bh2o 930
     X 1.6793E-08, 2.2694E-08, 2.9468E-08, 3.9278E-08, 5.2145E-08/      bh2o 940
      DATA F0301/                                                       bh2o 950
     X 6.4378E-08, 7.7947E-08, 8.5321E-08, 9.7848E-08, 1.0999E-07,      bh2o 960
     X 1.1489E-07, 1.2082E-07, 1.2822E-07, 1.4053E-07, 1.5238E-07,      bh2o 970
     X 1.5454E-07, 1.5018E-07, 1.4048E-07, 1.2359E-07, 1.0858E-07,      bh2o 980
     X 9.3486E-08, 8.1638E-08, 7.7690E-08, 8.4625E-08, 1.0114E-07,      bh2o 990
     X 1.1430E-07, 1.2263E-07, 1.3084E-07, 1.3380E-07, 1.3573E-07,      bh2o1000
     X 1.3441E-07, 1.2962E-07, 1.2638E-07, 1.1934E-07, 1.1371E-07,      bh2o1010
     X 1.0871E-07, 9.8843E-08, 9.1877E-08, 9.1050E-08, 9.3213E-08,      bh2o1020
     X 9.2929E-08, 1.0155E-07, 1.1263E-07, 1.2370E-07, 1.3636E-07,      bh2o1030
     X 1.5400E-07, 1.7656E-07, 2.1329E-07, 2.3045E-07, 2.5811E-07,      bh2o1040
     X 2.9261E-07, 3.4259E-07, 4.0770E-07, 4.8771E-07, 5.8081E-07/      bh2o1050
      DATA F0351/                                                       bh2o1060
     X 7.2895E-07, 8.7482E-07, 1.0795E-06, 1.3384E-06, 1.7208E-06,      bh2o1070
     X 2.0677E-06, 2.5294E-06, 3.1123E-06, 3.7900E-06, 4.7752E-06,      bh2o1080
     X 5.6891E-06, 6.6261E-06, 7.6246E-06, 8.7730E-06, 9.6672E-06,      bh2o1090
     X 1.0980E-05, 1.1287E-05, 1.1670E-05, 1.1635E-05, 1.1768E-05,      bh2o1100
     X 1.2039E-05, 1.2253E-05, 1.3294E-05, 1.4005E-05, 1.3854E-05,      bh2o1110
     X 1.3420E-05, 1.3003E-05, 1.2645E-05, 1.1715E-05, 1.1258E-05,      bh2o1120
     X 1.1516E-05, 1.2494E-05, 1.3655E-05, 1.4931E-05, 1.4649E-05,      bh2o1130
     X 1.3857E-05, 1.3120E-05, 1.1791E-05, 1.0637E-05, 8.2760E-06,      bh2o1140
     X 6.5821E-06, 5.1959E-06, 4.0158E-06, 3.0131E-06, 2.0462E-06,      bh2o1150
     X 1.4853E-06, 1.0365E-06, 7.3938E-07, 4.9752E-07, 3.4148E-07/      bh2o1160
      DATA F0401/                                                       bh2o1170
     X 2.4992E-07, 1.8363E-07, 1.4591E-07, 1.1380E-07, 9.0588E-08,      bh2o1180
     X 7.3697E-08, 6.0252E-08, 5.1868E-08, 4.2660E-08, 3.6163E-08,      bh2o1190
     X 3.2512E-08, 2.9258E-08, 2.4238E-08, 2.1209E-08, 1.6362E-08,      bh2o1200
     X 1.3871E-08, 1.2355E-08, 9.6940E-09, 7.7735E-09, 6.2278E-09,      bh2o1210
     X 5.2282E-09, 4.3799E-09, 3.5545E-09, 2.7527E-09, 2.0950E-09,      bh2o1220
     X 1.6344E-09, 1.2689E-09, 1.0403E-09, 8.4880E-10, 6.3461E-10,      bh2o1230
     X 4.7657E-10, 3.5220E-10, 2.7879E-10, 2.3021E-10, 1.6167E-10,      bh2o1240
     X 1.1732E-10, 8.9206E-11, 7.0596E-11, 5.8310E-11, 4.4084E-11,      bh2o1250
     X 3.1534E-11, 2.5068E-11, 2.2088E-11, 2.2579E-11, 2.2637E-11,      bh2o1260
     X 2.5705E-11, 3.2415E-11, 4.6116E-11, 6.5346E-11, 9.4842E-11/      bh2o1270
      DATA F0451/                                                       bh2o1280
     X 1.2809E-10, 1.8211E-10, 2.4052E-10, 3.0270E-10, 3.5531E-10,      bh2o1290
     X 4.2402E-10, 4.6730E-10, 4.7942E-10, 4.6813E-10, 4.5997E-10,      bh2o1300
     X 4.5788E-10, 4.0311E-10, 3.7367E-10, 3.3149E-10, 2.9281E-10,      bh2o1310
     X 2.5231E-10, 2.1152E-10, 1.9799E-10, 1.8636E-10, 1.9085E-10,      bh2o1320
     X 2.0786E-10, 2.2464E-10, 2.3785E-10, 2.5684E-10, 2.7499E-10,      bh2o1330
     X 2.6962E-10, 2.6378E-10, 2.6297E-10, 2.6903E-10, 2.7035E-10,      bh2o1340
     X 2.5394E-10, 2.5655E-10, 2.7184E-10, 2.9013E-10, 3.0585E-10,      bh2o1350
     X 3.0791E-10, 3.1667E-10, 3.4343E-10, 3.7365E-10, 4.0269E-10,      bh2o1360
     X 4.7260E-10, 5.6584E-10, 6.9791E-10, 8.6569E-10, 1.0393E-09,      bh2o1370
     X 1.2067E-09, 1.5047E-09, 1.8583E-09, 2.2357E-09, 2.6498E-09/      bh2o1380
      DATA F0501/                                                       bh2o1390
     X 3.2483E-09, 3.9927E-09, 4.6618E-09, 5.5555E-09, 6.6609E-09,      bh2o1400
     X 8.2139E-09, 1.0285E-08, 1.3919E-08, 1.8786E-08, 2.5150E-08,      bh2o1410
     X 3.3130E-08, 4.5442E-08, 6.3370E-08, 9.0628E-08, 1.2118E-07,      bh2o1420
     X 1.5927E-07, 2.1358E-07, 2.7825E-07, 3.7671E-07, 4.4894E-07,      bh2o1430
     X 5.4442E-07, 6.2240E-07, 7.3004E-07, 8.3384E-07, 8.7933E-07,      bh2o1440
     X 8.8080E-07, 8.6939E-07, 8.6541E-07, 8.2055E-07, 7.7278E-07,      bh2o1450
     X 7.5989E-07, 8.6909E-07, 9.7945E-07, 1.0394E-06, 1.0646E-06,      bh2o1460
     X 1.1509E-06, 1.2017E-06, 1.1915E-06, 1.1259E-06, 1.1549E-06,      bh2o1470
     X 1.1938E-06, 1.2356E-06, 1.2404E-06, 1.1716E-06, 1.1149E-06,      bh2o1480
     X 1.0073E-06, 8.9845E-07, 7.6639E-07, 6.1517E-07, 5.0887E-07/      bh2o1490
      DATA F0551/                                                       bh2o1500
     X 4.1269E-07, 3.2474E-07, 2.5698E-07, 1.8893E-07, 1.4009E-07,      bh2o1510
     X 1.0340E-07, 7.7724E-08, 5.7302E-08, 4.2178E-08, 2.9603E-08,      bh2o1520
     X 2.1945E-08, 1.6301E-08, 1.2806E-08, 1.0048E-08, 7.8970E-09,      bh2o1530
     X 6.1133E-09, 4.9054E-09, 4.1985E-09, 3.6944E-09, 3.2586E-09,      bh2o1540
     X 2.7362E-09, 2.3647E-09, 2.1249E-09, 1.8172E-09, 1.6224E-09,      bh2o1550
     X 1.5158E-09, 1.2361E-09, 1.0682E-09, 9.2312E-10, 7.9220E-10,      bh2o1560
     X 6.8174E-10, 5.6147E-10, 4.8268E-10, 4.1534E-10, 3.3106E-10,      bh2o1570
     X 2.8275E-10, 2.4584E-10, 2.0742E-10, 1.7840E-10, 1.4664E-10,      bh2o1580
     X 1.2390E-10, 1.0497E-10, 8.5038E-11, 6.7008E-11, 5.6355E-11,      bh2o1590
     X 4.3323E-11, 3.6914E-11, 3.2262E-11, 3.0749E-11, 3.0318E-11/      bh2o1600
      DATA F0601/                                                       bh2o1610
     X 2.9447E-11, 2.9918E-11, 3.0668E-11, 3.1315E-11, 3.0329E-11,      bh2o1620
     X 2.8259E-11, 2.6065E-11, 2.3578E-11, 2.0469E-11, 1.6908E-11,      bh2o1630
     X 1.4912E-11, 1.1867E-11, 9.9730E-12, 8.1014E-12, 6.7528E-12,      bh2o1640
     X 6.3133E-12, 5.8599E-12, 6.0145E-12, 6.5105E-12, 7.0537E-12,      bh2o1650
     X 7.4973E-12, 7.8519E-12, 8.5039E-12, 9.1995E-12, 1.0694E-11,      bh2o1660
     X 1.1659E-11, 1.2685E-11, 1.3087E-11, 1.3222E-11, 1.2634E-11,      bh2o1670
     X 1.1077E-11, 9.6259E-12, 8.3202E-12, 7.4857E-12, 6.8069E-12,      bh2o1680
     X 6.7496E-12, 7.3116E-12, 8.0171E-12, 8.6394E-12, 9.2659E-12,      bh2o1690
     X 1.0048E-11, 1.0941E-11, 1.2226E-11, 1.3058E-11, 1.5193E-11,      bh2o1700
     X 1.8923E-11, 2.3334E-11, 2.8787E-11, 3.6693E-11, 4.8295E-11/      bh2o1710
      DATA F0651/                                                       bh2o1720
     X 6.4260E-11, 8.8269E-11, 1.1865E-10, 1.5961E-10, 2.0605E-10,      bh2o1730
     X 2.7349E-10, 3.7193E-10, 4.8216E-10, 6.1966E-10, 7.7150E-10,      bh2o1740
     X 1.0195E-09, 1.2859E-09, 1.6535E-09, 2.0316E-09, 2.3913E-09,      bh2o1750
     X 3.0114E-09, 3.7495E-09, 4.6504E-09, 5.9145E-09, 7.6840E-09,      bh2o1760
     X 1.0304E-08, 1.3010E-08, 1.6441E-08, 2.1475E-08, 2.5892E-08,      bh2o1770
     X 2.9788E-08, 3.3820E-08, 4.0007E-08, 4.4888E-08, 4.5765E-08,      bh2o1780
     X 4.6131E-08, 4.6239E-08, 4.4849E-08, 4.0729E-08, 3.6856E-08,      bh2o1790
     X 3.6164E-08, 3.7606E-08, 4.1457E-08, 4.3750E-08, 5.1150E-08,      bh2o1800
     X 5.6054E-08, 6.1586E-08, 6.4521E-08, 6.6494E-08, 6.9024E-08,      bh2o1810
     X 6.8893E-08, 7.0901E-08, 6.9760E-08, 7.1485E-08, 7.0740E-08/      bh2o1820
      DATA F0701/                                                       bh2o1830
     X 7.3764E-08, 7.6618E-08, 8.4182E-08, 9.3838E-08, 1.0761E-07,      bh2o1840
     X 1.2851E-07, 1.4748E-07, 1.8407E-07, 2.2109E-07, 2.6392E-07,      bh2o1850
     X 2.9887E-07, 3.4493E-07, 4.0336E-07, 4.3551E-07, 4.9231E-07,      bh2o1860
     X 5.0728E-07, 5.3781E-07, 5.3285E-07, 5.4496E-07, 5.5707E-07,      bh2o1870
     X 5.6944E-07, 6.1123E-07, 6.4317E-07, 6.4581E-07, 6.1999E-07,      bh2o1880
     X 6.0191E-07, 5.7762E-07, 5.7241E-07, 5.7013E-07, 6.0160E-07,      bh2o1890
     X 6.6905E-07, 7.4095E-07, 8.2121E-07, 8.0947E-07, 7.6145E-07,      bh2o1900
     X 7.2193E-07, 6.3722E-07, 5.4316E-07, 4.2186E-07, 3.2528E-07,      bh2o1910
     X 2.5207E-07, 1.8213E-07, 1.2658E-07, 8.6746E-08, 6.0216E-08,      bh2o1920
     X 4.1122E-08, 2.8899E-08, 2.1740E-08, 1.7990E-08, 1.5593E-08/      bh2o1930
      DATA F0751/                                                       bh2o1940
     X 1.3970E-08, 1.2238E-08, 1.0539E-08, 9.2386E-09, 7.8481E-09,      bh2o1950
     X 6.8704E-09, 5.7615E-09, 5.0434E-09, 4.6886E-09, 4.3770E-09,      bh2o1960
     X 3.9768E-09, 3.5202E-09, 3.1854E-09, 2.9009E-09, 2.5763E-09,      bh2o1970
     X 2.2135E-09, 1.9455E-09, 1.6248E-09, 1.3368E-09, 1.0842E-09,      bh2o1980
     X 8.4254E-10, 6.7414E-10, 5.4667E-10, 4.5005E-10, 3.4932E-10,      bh2o1990
     X 2.6745E-10, 2.2053E-10, 1.8162E-10, 1.4935E-10, 1.1618E-10,      bh2o2000
     X 9.1888E-11, 8.0672E-11, 6.8746E-11, 6.2668E-11, 5.5715E-11,      bh2o2010
     X 4.5074E-11, 3.7669E-11, 3.2082E-11, 2.8085E-11, 2.4838E-11,      bh2o2020
     X 1.9791E-11, 1.6964E-11, 1.3887E-11, 1.1179E-11, 9.7499E-12,      bh2o2030
     X 7.8255E-12, 6.3698E-12, 5.3265E-12, 4.6588E-12, 4.4498E-12/      bh2o2040
      DATA F0801/                                                       bh2o2050
     X 3.9984E-12, 3.7513E-12, 3.7176E-12, 3.9148E-12, 4.2702E-12,      bh2o2060
     X 5.0090E-12, 6.5801E-12, 8.7787E-12, 1.2718E-11, 1.8375E-11,      bh2o2070
     X 2.5304E-11, 3.5403E-11, 4.8842E-11, 6.4840E-11, 8.0911E-11,      bh2o2080
     X 1.0136E-10, 1.2311E-10, 1.4203E-10, 1.5869E-10, 1.8093E-10,      bh2o2090
     X 2.1370E-10, 2.5228E-10, 2.8816E-10, 3.4556E-10, 3.9860E-10,      bh2o2100
     X 4.4350E-10, 4.7760E-10, 5.2357E-10, 6.0827E-10, 6.3635E-10,      bh2o2110
     X 6.5886E-10, 6.8753E-10, 7.2349E-10, 7.2789E-10, 6.8232E-10,      bh2o2120
     X 6.6081E-10, 6.4232E-10, 6.3485E-10, 6.4311E-10, 7.2235E-10,      bh2o2130
     X 7.7263E-10, 8.1668E-10, 9.0324E-10, 9.7643E-10, 1.0535E-09,      bh2o2140
     X 1.0195E-09, 1.0194E-09, 1.0156E-09, 9.6792E-10, 9.2725E-10/      bh2o2150
      DATA F0851/                                                       bh2o2160
     X 8.7347E-10, 8.4484E-10, 8.2647E-10, 8.4363E-10, 9.1261E-10,      bh2o2170
     X 1.0051E-09, 1.1511E-09, 1.4037E-09, 1.8066E-09, 2.4483E-09,      bh2o2180
     X 3.2739E-09, 4.3194E-09, 5.6902E-09, 7.7924E-09, 9.7376E-09,      bh2o2190
     X 1.2055E-08, 1.4303E-08, 1.6956E-08, 1.9542E-08, 2.2233E-08,      bh2o2200
     X 2.5186E-08, 2.7777E-08, 2.8943E-08, 2.8873E-08, 2.9417E-08,      bh2o2210
     X 2.7954E-08, 2.7524E-08, 2.7040E-08, 3.1254E-08, 3.6843E-08,      bh2o2220
     X 3.7797E-08, 3.8713E-08, 4.0135E-08, 4.2824E-08, 4.3004E-08,      bh2o2230
     X 4.0279E-08, 4.2781E-08, 4.5220E-08, 4.8948E-08, 5.0172E-08,      bh2o2240
     X 4.8499E-08, 4.7182E-08, 4.2204E-08, 3.7701E-08, 3.0972E-08,      bh2o2250
     X 2.4654E-08, 1.9543E-08, 1.4609E-08, 1.1171E-08, 8.3367E-09/      bh2o2260
      DATA F0901/                                                       bh2o2270
     X 6.3791E-09, 5.0790E-09, 4.0655E-09, 3.3658E-09, 2.7882E-09,      bh2o2280
     X 2.4749E-09, 2.2287E-09, 2.0217E-09, 1.8191E-09, 1.5897E-09,      bh2o2290
     X 1.4191E-09, 1.2448E-09, 1.0884E-09, 9.3585E-10, 7.9429E-10,      bh2o2300
     X 7.3214E-10, 6.5008E-10, 5.7549E-10, 5.4300E-10, 4.7251E-10,      bh2o2310
     X 4.3451E-10, 3.8446E-10, 3.5589E-10, 3.4432E-10, 2.8209E-10,      bh2o2320
     X 2.4620E-10, 2.1278E-10, 1.8406E-10, 1.6314E-10, 1.3261E-10,      bh2o2330
     X 1.1696E-10, 9.6865E-11, 7.6814E-11, 6.6411E-11, 5.0903E-11,      bh2o2340
     X 4.0827E-11, 3.0476E-11, 2.3230E-11, 1.7707E-11, 1.3548E-11,      bh2o2350
     X 1.0719E-11, 9.3026E-12, 8.7967E-12, 8.3136E-12, 7.3918E-12,      bh2o2360
     X 6.5293E-12, 5.9243E-12, 5.3595E-12, 3.5266E-12, 2.2571E-12/      bh2o2370
      DATA F0951/                                                       bh2o2380
     X 1.6150E-12, 1.1413E-12, 8.4998E-13, 7.0803E-13, 5.1747E-13,      bh2o2390
     X 4.0694E-13, 3.6528E-13, 3.3670E-13, 3.1341E-13, 2.9390E-13,      bh2o2400
     X 2.8680E-13, 3.1283E-13, 3.7294E-13, 5.0194E-13, 6.7919E-13,      bh2o2410
     X 1.0455E-12, 1.5230E-12, 2.3932E-12, 3.4231E-12, 5.0515E-12,      bh2o2420
     X 7.3193E-12, 9.9406E-12, 1.2193E-11, 1.4742E-11, 1.9269E-11,      bh2o2430
     X 2.1816E-11, 2.2750E-11, 2.2902E-11, 2.3888E-11, 2.4902E-11,      bh2o2440
     X 2.2160E-11, 2.0381E-11, 1.9903E-11, 2.0086E-11, 1.9304E-11,      bh2o2450
     X 2.0023E-11, 2.2244E-11, 2.5450E-11, 3.1228E-11, 3.4560E-11,      bh2o2460
     X 3.6923E-11, 3.7486E-11, 3.8124E-11, 3.8317E-11, 3.4737E-11,      bh2o2470
     X 3.3037E-11, 3.1724E-11, 2.9840E-11, 2.8301E-11, 2.5857E-11/      bh2o2480
      DATA F1001/                                                       bh2o2490
     X 2.3708E-11, 1.9452E-11, 1.6232E-11, 1.5174E-11, 1.4206E-11,      bh2o2500
     X 1.4408E-11, 1.5483E-11, 1.8642E-11, 2.3664E-11, 3.0181E-11,      bh2o2510
     X 4.0160E-11, 5.2287E-11, 7.2754E-11, 1.0511E-10, 1.4531E-10,      bh2o2520
     X 2.0998E-10, 2.6883E-10, 3.3082E-10, 4.2638E-10, 5.3132E-10,      bh2o2530
     X 6.3617E-10, 7.1413E-10, 8.5953E-10, 9.9715E-10, 1.0796E-09,      bh2o2540
     X 1.0978E-09, 1.1052E-09, 1.1095E-09, 1.0641E-09, 9.7881E-10,      bh2o2550
     X 9.6590E-10, 1.0332E-09, 1.1974E-09, 1.3612E-09, 1.5829E-09,      bh2o2560
     X 1.8655E-09, 2.1465E-09, 2.4779E-09, 2.7370E-09, 2.9915E-09,      bh2o2570
     X 3.3037E-09, 3.6347E-09, 3.9587E-09, 4.4701E-09, 5.0122E-09,      bh2o2580
     X 5.8044E-09, 6.1916E-09, 6.9613E-09, 7.7863E-09, 8.2820E-09/      bh2o2590
      DATA F1051/                                                       bh2o2600
     X 9.4359E-09, 9.7387E-09, 1.0656E-08, 1.0746E-08, 1.1210E-08,      bh2o2610
     X 1.1905E-08, 1.2194E-08, 1.3145E-08, 1.3738E-08, 1.3634E-08,      bh2o2620
     X 1.3011E-08, 1.2511E-08, 1.1805E-08, 1.2159E-08, 1.2390E-08,      bh2o2630
     X 1.3625E-08, 1.5678E-08, 1.7886E-08, 1.9933E-08, 1.9865E-08,      bh2o2640
     X 1.9000E-08, 1.7812E-08, 1.5521E-08, 1.2593E-08, 9.5635E-09,      bh2o2650
     X 7.2987E-09, 5.2489E-09, 3.5673E-09, 2.4206E-09, 1.6977E-09,      bh2o2660
     X 1.2456E-09, 9.3744E-10, 7.8379E-10, 6.9960E-10, 6.6451E-10,      bh2o2670
     X 6.8521E-10, 7.4234E-10, 8.6658E-10, 9.4972E-10, 1.0791E-09,      bh2o2680
     X 1.2359E-09, 1.3363E-09, 1.5025E-09, 1.5368E-09, 1.6152E-09,      bh2o2690
     X 1.6184E-09, 1.6557E-09, 1.7035E-09, 1.6916E-09, 1.7237E-09/      bh2o2700
      DATA F1101/                                                       bh2o2710
     X 1.7175E-09, 1.6475E-09, 1.5335E-09, 1.4272E-09, 1.3282E-09,      bh2o2720
     X 1.3459E-09, 1.4028E-09, 1.5192E-09, 1.7068E-09, 1.9085E-09,      bh2o2730
     X 2.1318E-09, 2.1020E-09, 1.9942E-09, 1.8654E-09, 1.6391E-09,      bh2o2740
     X 1.3552E-09, 1.0186E-09, 7.8540E-10, 5.7022E-10, 3.9247E-10,      bh2o2750
     X 2.5441E-10, 1.6699E-10, 1.1132E-10, 6.8989E-11, 4.5255E-11,      bh2o2760
     X 3.1106E-11, 2.3161E-11, 1.7618E-11, 1.4380E-11, 1.1601E-11,      bh2o2770
     X 9.7148E-12, 8.4519E-12, 6.5392E-12, 5.4113E-12, 4.7624E-12,      bh2o2780
     X 4.0617E-12, 3.6173E-12, 2.8608E-12, 2.2724E-12, 1.7436E-12,      bh2o2790
     X 1.3424E-12, 1.0358E-12, 7.3064E-13, 5.4500E-13, 4.0551E-13,      bh2o2800
     X 2.8642E-13, 2.1831E-13, 1.6860E-13, 1.2086E-13, 1.0150E-13/      bh2o2810
      DATA F1151/                                                       bh2o2820
     X 9.3550E-14, 8.4105E-14, 7.3051E-14, 6.9796E-14, 7.9949E-14,      bh2o2830
     X 1.0742E-13, 1.5639E-13, 2.1308E-13, 3.1226E-13, 4.6853E-13,      bh2o2840
     X 6.6917E-13, 1.0088E-12, 1.4824E-12, 2.2763E-12, 3.3917E-12,      bh2o2850
     X 4.4585E-12, 6.3187E-12, 8.4189E-12, 1.1302E-11, 1.3431E-11,      bh2o2860
     X 1.5679E-11, 1.9044E-11, 2.2463E-11, 2.3605E-11, 2.3619E-11,      bh2o2870
     X 2.3505E-11, 2.3805E-11, 2.2549E-11, 1.9304E-11, 1.8382E-11,      bh2o2880
     X 1.7795E-11, 1.8439E-11, 1.9146E-11, 2.1966E-11, 2.6109E-11,      bh2o2890
     X 3.1883E-11, 3.7872E-11, 4.3966E-11, 4.8789E-11, 5.3264E-11,      bh2o2900
     X 5.9705E-11, 6.3744E-11, 7.0163E-11, 7.9114E-11, 8.8287E-11,      bh2o2910
     X 9.9726E-11, 1.1498E-10, 1.3700E-10, 1.6145E-10, 1.9913E-10/      bh2o2920
      DATA F1201/                                                       bh2o2930
     X 2.2778E-10, 2.6216E-10, 2.9770E-10, 3.3405E-10, 3.7821E-10,      bh2o2940
     X 3.9552E-10, 4.1322E-10, 4.0293E-10, 4.0259E-10, 3.8853E-10,      bh2o2950
     X 3.7842E-10, 3.8551E-10, 4.4618E-10, 5.0527E-10, 5.0695E-10,      bh2o2960
     X 5.1216E-10, 5.1930E-10, 5.5794E-10, 5.3320E-10, 5.2008E-10,      bh2o2970
     X 5.6888E-10, 6.1883E-10, 6.9006E-10, 6.9505E-10, 6.6768E-10,      bh2o2980
     X 6.3290E-10, 5.6753E-10, 5.0327E-10, 3.9830E-10, 3.1147E-10,      bh2o2990
     X 2.4416E-10, 1.8860E-10, 1.3908E-10, 9.9156E-11, 7.3779E-11,      bh2o3000
     X 5.6048E-11, 4.2457E-11, 3.4505E-11, 2.9881E-11, 2.7865E-11,      bh2o3010
     X 2.8471E-11, 3.1065E-11, 3.4204E-11, 3.9140E-11, 4.3606E-11,      bh2o3020
     X 4.9075E-11, 5.3069E-11, 5.5236E-11, 5.5309E-11, 5.3832E-11/      bh2o3030
      DATA F1251/                                                       bh2o3040
     X 5.3183E-11, 5.1783E-11, 5.2042E-11, 5.4422E-11, 5.5656E-11,      bh2o3050
     X 5.4409E-11, 5.2659E-11, 5.1696E-11, 5.1726E-11, 4.9003E-11,      bh2o3060
     X 4.9050E-11, 5.1700E-11, 5.6818E-11, 6.3129E-11, 6.6542E-11,      bh2o3070
     X 6.4367E-11, 5.9908E-11, 5.4470E-11, 4.7903E-11, 3.9669E-11,      bh2o3080
     X 2.9651E-11, 2.2286E-11, 1.6742E-11, 1.1827E-11, 7.7739E-12,      bh2o3090
     X 4.8805E-12, 3.1747E-12, 2.0057E-12, 1.2550E-12, 8.7434E-13,      bh2o3100
     X 6.2755E-13, 4.9752E-13, 4.0047E-13, 3.5602E-13, 3.0930E-13,      bh2o3110
     X 2.4903E-13, 1.9316E-13, 1.4995E-13, 1.2059E-13, 8.7242E-14,      bh2o3120
     X 6.4511E-14, 5.3300E-14, 4.3741E-14, 3.4916E-14, 2.6560E-14,      bh2o3130
     X 1.6923E-14, 1.1816E-14, 6.7071E-15, 3.6474E-15, 2.0686E-15/      bh2o3140
      DATA F1301/                                                       bh2o3150
     X 1.1925E-15, 6.8948E-16, 3.9661E-16, 2.2576E-16, 1.2669E-16,      bh2o3160
     X 6.9908E-17, 3.7896E-17, 2.0280E-17, 1.1016E-17, 6.7816E-18,      bh2o3170
     X 6.0958E-18, 8.9913E-18, 1.7201E-17, 3.4964E-17, 7.0722E-17,      bh2o3180
     X 1.4020E-16, 2.7167E-16, 5.1478E-16, 9.5500E-16, 1.7376E-15,      bh2o3190
     X 3.1074E-15, 5.4789E-15, 9.5640E-15, 1.6635E-14, 2.9145E-14,      bh2o3200
     X 5.2179E-14, 8.8554E-14, 1.4764E-13, 2.3331E-13, 3.5996E-13,      bh2o3210
     X 5.2132E-13, 6.3519E-13, 7.3174E-13, 8.3752E-13, 9.8916E-13,      bh2o3220
     X 1.1515E-12, 1.4034E-12, 1.6594E-12, 2.1021E-12, 2.7416E-12,      bh2o3230
     X 3.4135E-12, 4.5517E-12, 5.5832E-12, 7.2303E-12, 9.9484E-12,      bh2o3240
     X 1.2724E-11, 1.6478E-11, 2.0588E-11, 2.5543E-11, 3.3625E-11/      bh2o3250
      DATA F1351/                                                       bh2o3260
     X 4.1788E-11, 5.0081E-11, 6.0144E-11, 6.9599E-11, 8.4408E-11,      bh2o3270
     X 9.7143E-11, 1.0805E-10, 1.1713E-10, 1.2711E-10, 1.3727E-10,      bh2o3280
     X 1.4539E-10, 1.6049E-10, 1.7680E-10, 2.0557E-10, 2.4967E-10,      bh2o3290
     X 3.0096E-10, 3.5816E-10, 4.0851E-10, 4.6111E-10, 5.2197E-10,      bh2o3300
     X 5.5043E-10, 6.0324E-10, 6.4983E-10, 6.7498E-10, 7.0545E-10,      bh2o3310
     X 7.0680E-10, 7.5218E-10, 7.5723E-10, 7.7840E-10, 8.0081E-10,      bh2o3320
     X 8.0223E-10, 7.7271E-10, 7.1676E-10, 6.7819E-10, 6.4753E-10,      bh2o3330
     X 6.5844E-10, 7.0163E-10, 7.7503E-10, 8.8152E-10, 9.9022E-10,      bh2o3340
     X 1.0229E-09, 9.9296E-10, 8.9911E-10, 7.7813E-10, 6.3785E-10,      bh2o3350
     X 4.7491E-10, 3.5280E-10, 2.4349E-10, 1.6502E-10, 1.1622E-10/      bh2o3360
      DATA F1401/                                                       bh2o3370
     X 8.6715E-11, 6.7360E-11, 5.3910E-11, 4.5554E-11, 4.1300E-11,      bh2o3380
     X 3.9728E-11, 3.9000E-11, 3.9803E-11, 4.1514E-11, 4.3374E-11,      bh2o3390
     X 4.6831E-11, 4.8921E-11, 5.1995E-11, 5.7242E-11, 6.2759E-11,      bh2o3400
     X 7.0801E-11, 7.4555E-11, 7.9754E-11, 8.7616E-11, 9.1171E-11,      bh2o3410
     X 1.0349E-10, 1.1047E-10, 1.2024E-10, 1.2990E-10, 1.3725E-10,      bh2o3420
     X 1.5005E-10, 1.5268E-10, 1.5535E-10, 1.5623E-10, 1.5009E-10,      bh2o3430
     X 1.4034E-10, 1.3002E-10, 1.2225E-10, 1.1989E-10, 1.2411E-10,      bh2o3440
     X 1.3612E-10, 1.5225E-10, 1.7202E-10, 1.9471E-10, 1.9931E-10,      bh2o3450
     X 1.9079E-10, 1.7478E-10, 1.5259E-10, 1.2625E-10, 9.3332E-11,      bh2o3460
     X 6.8796E-11, 4.6466E-11, 2.9723E-11, 1.8508E-11, 1.2106E-11/      bh2o3470
      DATA F1451/                                                       bh2o3480
     X 8.0142E-12, 5.4066E-12, 3.9329E-12, 3.1665E-12, 2.7420E-12,      bh2o3490
     X 2.3996E-12, 2.3804E-12, 2.3242E-12, 2.4476E-12, 2.5331E-12,      bh2o3500
     X 2.3595E-12, 2.2575E-12, 2.1298E-12, 2.0088E-12, 1.8263E-12,      bh2o3510
     X 1.6114E-12, 1.4422E-12, 1.2946E-12, 1.0837E-12, 9.1282E-13,      bh2o3520
     X 7.2359E-13, 5.3307E-13, 3.8837E-13, 2.6678E-13, 1.6769E-13,      bh2o3530
     X 1.0826E-13, 7.2364E-14, 4.5201E-14, 3.0808E-14, 2.2377E-14,      bh2o3540
     X 1.7040E-14, 9.2181E-15, 5.2934E-15, 3.5774E-15, 3.1431E-15,      bh2o3550
     X 3.7647E-15, 5.6428E-15, 9.5139E-15, 1.7322E-14, 2.8829E-14,      bh2o3560
     X 4.7708E-14, 6.9789E-14, 9.7267E-14, 1.4662E-13, 1.9429E-13,      bh2o3570
     X 2.5998E-13, 3.6636E-13, 4.7960E-13, 6.5129E-13, 7.7638E-13/      bh2o3580
      DATA F1501/                                                       bh2o3590
     X 9.3774E-13, 1.1467E-12, 1.3547E-12, 1.5686E-12, 1.6893E-12,      bh2o3600
     X 1.9069E-12, 2.1352E-12, 2.3071E-12, 2.4759E-12, 2.8247E-12,      bh2o3610
     X 3.4365E-12, 4.3181E-12, 5.6107E-12, 7.0017E-12, 8.6408E-12,      bh2o3620
     X 1.0974E-11, 1.3742E-11, 1.6337E-11, 2.0157E-11, 2.3441E-11,      bh2o3630
     X 2.6733E-11, 3.0247E-11, 3.3737E-11, 3.8618E-11, 4.1343E-11,      bh2o3640
     X 4.3870E-11, 4.4685E-11, 4.4881E-11, 4.5526E-11, 4.3628E-11,      bh2o3650
     X 4.4268E-11, 4.6865E-11, 5.3426E-11, 5.4020E-11, 5.3218E-11,      bh2o3660
     X 5.4587E-11, 5.6360E-11, 5.7740E-11, 5.6426E-11, 6.0399E-11,      bh2o3670
     X 6.6981E-11, 7.4319E-11, 7.7977E-11, 7.5539E-11, 7.1610E-11,      bh2o3680
     X 6.4606E-11, 5.5498E-11, 4.3944E-11, 3.3769E-11, 2.5771E-11/      bh2o3690
      DATA F1551/                                                       bh2o3700
     X 1.9162E-11, 1.3698E-11, 1.0173E-11, 7.8925E-12, 6.1938E-12,      bh2o3710
     X 4.7962E-12, 4.0811E-12, 3.3912E-12, 2.8625E-12, 2.4504E-12,      bh2o3720
     X 2.2188E-12, 2.2139E-12, 2.2499E-12, 2.2766E-12, 2.3985E-12,      bh2o3730
     X 2.5459E-12, 2.9295E-12, 3.4196E-12, 3.6155E-12, 4.0733E-12,      bh2o3740
     X 4.4610E-12, 4.9372E-12, 5.4372E-12, 5.7304E-12, 6.1640E-12,      bh2o3750
     X 6.1278E-12, 6.2940E-12, 6.4947E-12, 6.8174E-12, 7.5190E-12,      bh2o3760
     X 8.2608E-12, 8.4971E-12, 8.3484E-12, 8.1888E-12, 7.8552E-12,      bh2o3770
     X 7.8468E-12, 7.5943E-12, 7.9096E-12, 8.6869E-12, 9.1303E-12,      bh2o3780
     X 9.2547E-12, 8.9322E-12, 8.2177E-12, 7.3408E-12, 5.7956E-12,      bh2o3790
     X 4.4470E-12, 3.5881E-12, 2.6748E-12, 1.7074E-12, 9.6700E-13/      bh2o3800
      DATA F1601/                                                       bh2o3810
     X 5.2645E-13, 2.9943E-13, 1.7316E-13, 1.0039E-13, 5.7859E-14,      bh2o3820
     X 3.2968E-14, 1.8499E-14, 1.0192E-14, 5.5015E-15, 2.9040E-15,      bh2o3830
     X 1.4968E-15, 7.5244E-16, 3.6852E-16, 1.7568E-16, 8.1464E-17,      bh2o3840
     X 3.6717E-17, 1.6076E-17, 6.8341E-18, 2.8195E-18, 1.1286E-18,      bh2o3850
     X  .0000E+00,  .0000E+00,  .0000E+00,  .0000E+00,  .0000E+00,      bh2o3860
     X  .0000E+00,  .0000E+00,  .0000E+00,  .0000E+00, 1.4070E-18,      bh2o3870
     X 3.0405E-18, 6.4059E-18, 1.3169E-17, 2.6443E-17, 5.1917E-17,      bh2o3880
     X 9.9785E-17, 1.8802E-16, 3.4788E-16, 6.3328E-16, 1.1370E-15,      bh2o3890
     X 2.0198E-15, 3.5665E-15, 6.3053E-15, 1.1309E-14, 2.1206E-14,      bh2o3900
     X 3.2858E-14, 5.5165E-14, 8.6231E-14, 1.2776E-13, 1.7780E-13/      bh2o3910
      DATA F1651/                                                       bh2o3920
     X 2.5266E-13, 3.6254E-13, 5.1398E-13, 6.8289E-13, 8.7481E-13,      bh2o3930
     X 1.1914E-12, 1.6086E-12, 2.0469E-12, 2.5761E-12, 3.4964E-12,      bh2o3940
     X 4.4980E-12, 5.5356E-12, 6.7963E-12, 8.5720E-12, 1.0700E-11,      bh2o3950
     X 1.2983E-11, 1.6270E-11, 1.9609E-11, 2.2668E-11, 2.5963E-11,      bh2o3960
     X 3.0918E-11, 3.4930E-11, 3.9330E-11, 4.4208E-11, 4.6431E-11,      bh2o3970
     X 5.1141E-11, 5.4108E-11, 5.8077E-11, 6.5050E-11, 7.2126E-11,      bh2o3980
     X 8.1064E-11, 8.1973E-11, 8.1694E-11, 8.3081E-11, 8.0240E-11,      bh2o3990
     X 7.9225E-11, 7.6256E-11, 7.8468E-11, 8.0041E-11, 8.1585E-11,      bh2o4000
     X 8.3485E-11, 8.3774E-11, 8.5870E-11, 8.6104E-11, 8.8516E-11,      bh2o4010
     X 9.0814E-11, 9.2522E-11, 8.8913E-11, 7.8381E-11, 6.8568E-11/      bh2o4020
      DATA F1701/                                                       bh2o4030
     X 5.6797E-11, 4.4163E-11, 3.2369E-11, 2.3259E-11, 1.6835E-11,      bh2o4040
     X 1.1733E-11, 8.5273E-12, 6.3805E-12, 4.8983E-12, 3.8831E-12,      bh2o4050
     X 3.2610E-12, 2.8577E-12, 2.5210E-12, 2.2913E-12, 2.0341E-12,      bh2o4060
     X 1.8167E-12, 1.6395E-12, 1.4890E-12, 1.3516E-12, 1.2542E-12,      bh2o4070
     X 1.2910E-12, 1.3471E-12, 1.4689E-12, 1.5889E-12, 1.6989E-12,      bh2o4080
     X 1.8843E-12, 2.0902E-12, 2.3874E-12, 2.7294E-12, 3.3353E-12,      bh2o4090
     X 4.0186E-12, 4.5868E-12, 5.2212E-12, 5.8856E-12, 6.5991E-12,      bh2o4100
     X 7.2505E-12, 7.6637E-12, 8.5113E-12, 9.4832E-12, 9.9678E-12,      bh2o4110
     X 1.0723E-11, 1.0749E-11, 1.1380E-11, 1.1774E-11, 1.1743E-11,      bh2o4120
     X 1.2493E-11, 1.2559E-11, 1.2332E-11, 1.1782E-11, 1.1086E-11/      bh2o4130
      DATA F1751/                                                       bh2o4140
     X 1.0945E-11, 1.1178E-11, 1.2083E-11, 1.3037E-11, 1.4730E-11,      bh2o4150
     X 1.6450E-11, 1.7403E-11, 1.7004E-11, 1.5117E-11, 1.3339E-11,      bh2o4160
     X 1.0844E-11, 8.0915E-12, 5.6615E-12, 3.7196E-12, 2.5194E-12,      bh2o4170
     X 1.6569E-12, 1.1201E-12, 8.2335E-13, 6.0270E-13, 4.8205E-13,      bh2o4180
     X 4.1313E-13, 3.6243E-13, 3.2575E-13, 2.7730E-13, 2.5292E-13,      bh2o4190
     X 2.3062E-13, 2.1126E-13, 2.1556E-13, 2.1213E-13, 2.2103E-13,      bh2o4200
     X 2.1927E-13, 2.0794E-13, 1.9533E-13, 1.6592E-13, 1.4521E-13,      bh2o4210
     X 1.1393E-13, 8.3772E-14, 6.2077E-14, 4.3337E-14, 2.7165E-14,      bh2o4220
     X 1.6821E-14, 9.5407E-15, 5.3093E-15, 3.0320E-15, 1.7429E-15,      bh2o4230
     X 9.9828E-16, 5.6622E-16, 3.1672E-16, 1.7419E-16, 9.3985E-17/      bh2o4240
      DATA F1801/                                                       bh2o4250
     X 4.9656E-17, 2.5652E-17, 1.2942E-17, 6.3695E-18, 3.0554E-18,      bh2o4260
     C 1.4273E-18, -0.       , -0.       , -0.       , -0.       ,      bh2o4270
     C -0.       , 0.        , 0.        , 0.        , 0.        ,      bh2o4280
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4290
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4300
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4310
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4320
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4330
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4340
     C 0.        , 0.        , 0.        , 0.        , 0.        /      bh2o4350
      DATA F1851/                                                       bh2o4360
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4370
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4380
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4390
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4400
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4410
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4420
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4430
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4440
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4450
     C 0.        , 0.        , 0.        , 0.        , 0.        /      bh2o4460
      DATA F1901/                                                       bh2o4470
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4480
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4490
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4500
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4510
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4520
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4530
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4540
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4550
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4560
     C 0.        , 0.        , 0.        , 0.        , 0.        /      bh2o4570
      DATA F1951/                                                       bh2o4580
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4590
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4600
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4610
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4620
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4630
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4640
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4650
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4660
     C 0.        , 0.        , 0.        , 0.        , 0.        ,      bh2o4670
     C 0.        , 0.        , 0.        , 0.        , 0.        /      bh2o4680
      DATA F2001/                                                       bh2o4690
     C 0.        /                                                      bh2o4700
C                                                                       bh2o4710
      END                                                               bh2o4720
      SUBROUTINE BMDATA(IV1,IFWHM,IDVX,IKMX,MXFREQ)                     bmdt 100
C                                                                       bmdt 110
C     BMDATA (CALLED BY TRANS) MAKES THE INITIAL BAND MODEL TAPE READ   bmdt 120
C     AND CALCULATES WAVENUMBER-INDEPENDENT PARAMETERS FOR USE BY BMOD  bmdt 130
C                                                                       bmdt 140
c
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
c
      LOGICAL XTRAN
      CHARACTER*8 CNAMEX                                     
      CHARACTER*129 BMSTR
C
C     TRANS VARIABLES
C
      COMMON/BMDCMX/ IPX,SDZX(5),ODZX(5),IBINX,IMOLX,IALFX
      COMMON /NAMEX/CNAMEX(MMOLX)                                              
      COMMON /MDATAX/ WMOLXT(MMOLX,laydim)                                  
      COMMON /MODELX/ DNSTYX(MMOLX,LAYDIM)                                  
      COMMON /RFRPTX/ DENPX(MMOLX,LAYDIM+1),AMTPX(MMOLX,LAYDIM+1)
      COMMON /NONAME/ TXX(MMOLX), WX(MMOLX), WPATHX(laythr,MMOLX)
      COMMON /SOLSX/  WPTHSX(laythr,MMOLX),TBBYSX(laythr,MMOLX),
     $     PATMSX(laythr,MMOLX)
      DATA XTRAN/.TRUE./
      DATA ITBX/31/
c
c
c
      LOGICAL MODTRN
      COMMON RELHUM(LAYDIM),HSTOR(LAYDIM),ICH(4),VH(17),TX(65),W(65)  
      COMMON IMSMX,WPATH(LAYTHR,65),TBBY(LAYTHR),PATM(LAYTHR),NSPEC,   
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX2(47),AWCCON(5)  
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON/SOLS/AH1(LAYTWO),ARH(LAYTWO),WPATHS(LAYTHR,65),
     1 PA(LAYTWO),PR(LAYTWO),ATHETA(LAYDIM+1),ADBETA(LAYDIM+1),
     2 LJ(LAYTWO+1),JTURN,ANGSUN,CSZEN(LAYTWO),TBBYS(LAYTHR,12),
     3 PATMS(LAYTHR,12)
      COMMON/CARD1/MODEL,ITYPE,IEMSCT,M1,M2,M3,IM,NOPRNT,TBOUND,SALB    bmdt 230
     1  ,MODTRN                                                         bmdt 240
      COMMON/CNTRL/KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT                 bmdt 250
      COMMON/BMDCOM/IBNDWD,IP,IPRM,IBLK,IBLOCK,IPARAM(273),NTEMP,       
     1  TBAND(5),SDZ(5,250),ODZ(5,250),IBIN(250),IMOL(250),IALF(250),   
     2  SD(5,mmolt2),OD(5,mmolt),ALF0(MMOLT),WT(laythr),
     $  WTS(laythr,mmolt),ITB,irecnm,JJ(laythr),FF(laythr),
     $ JJS(laythr,mmolt),FFS(laythr,mmolt),          
     4          DOPFAC(mmolt),DOP0(mmolt),DOPSUM(mmolt),
     $     COLSUM(mmolt),ODSUM(mmolt),SDSUM(mmolt2)                     cfc
      DIMENSION IFREQ(273)                                              bmdt 320
      DATA TZERO/273.15/                                                bmdt 330
C                                                                       bmdt 340
C     OPEN THE UNFORMATTED BAND MODEL TAPE, FILE ITB = 9.  AN ERROR     bmdt 350
C     OCCURS IF THE TAPE WAS OPENED DURING A PREVIOUS LOWTRAN RUN       bmdt 360
C     (IMPLYING IRPT WAS NOT ZERO).  IN THAT CASE, REWIND THE TAPE.     bmdt 370
C                                                                       bmdt 380
      ITB = 9                                                           bmdt 390
c@@    OPEN(ITB,FILE='UFTAPE',STATUS='OLD',FORM='UNFORMATTED')          bmdt 400
       OPEN(ITB,FILE='DIRAC',STATUS='OLD',FORM='UNFORMATTED',           bmdt 410
     x         ACCESS='DIRECT',RECL=13000)
cc 10 REWIND(ITB)                                                       bmdt 460
c
      IF (XTRAN) OPEN(ITBX,FILE='UFTAPX.asc',
     $     STATUS='OLD',FORM='FORMATTED')
      rewind (itbx)
C                                                                       bmdt 470
C     READ THE BAND MODEL TAPE HEADER.  IBNDWD IS THE WIDTH (CM-1) OF   bmdt 480
C     EACH BAND, IBLOCK IS THE NUMBER OF STORAGE BLOCKS, IFREQ(IBLK) IS bmdt 490
C     THE FREQUENCY OF THE LAST BAND MODEL "PARAMETER SET" IN BLOCK IBLKbmdt 500
C     AND IPARAM(IBLK) IS THE NUMBER OF PARAMETER SETS IN THE BLOCK IBLKbmdt 510
C                                                                       bmdt 520
      READ(ITB,rec=1)                                                   bmdt 530
     2  IBNDWD,IBLOCK,(IFREQ(IBLK),IPARAM(IBLK),IBLK=1,IBLOCK),         bmdt 540
     1  NTEMP,(TBAND(IT),IT=1,NTEMP)                                    bmdt 550
C                                                                       bmdt 560
C     PASS THE BAND WIDTH TO TRANS THROUGH IDVX.  ALSO DETERMINE THE    bmdt 570
C     MINIMUM FREQUENCY AT WHICH BAND MODEL DATA IS NEEDED.             bmdt 580
      IDVX=IBNDWD                                                       bmdt 590
      IV=5*((IV1-IFWHM/IBNDWD+1)/5)                                     bmdt 600
C                                                                       bmdt 610
C     PERFORM THE STANDARD LOWTRAN CALCULATION IF NO BAND MODEL DATA    bmdt 620
C     EXISTS FOR THE CHOSEN FREQUENCY RANGE.                            bmdt 630
C                                                                       bmdt 640
      MXFREQ=IFREQ(IBLOCK)                                              bmdt 650
      IF(IV.GT.MXFREQ)THEN                                              bmdt 660
          WRITE(IPR,'(//43H (THE BAND MODEL TAPE DOES NOT HAVE DATA IN, bmdt 670
     1      32H THE REQUESTED WAVENUMBER RANGE))')                      bmdt 680
          RETURN                                                        bmdt 690
      ENDIF                                                             bmdt 700
C                                                                       bmdt 710
C     ADVANCE THE TAPE TO DESIRED FREQUENCY BLOCK                       bmdt 720
C                                                                       bmdt 730
      IBLK=0                                                            bmdt 740
   20 IBLK=IBLK+1                                                       bmdt 750
      irecnm = iblk+1                                                   bmdt 760
      IF(IV.GT.IFREQ(IBLK))THEN                                         bmdt 770
          IPRM=IPARAM(IBLK)                                             bmdt 780
cc        READ(ITB)(IDUMMY,IDUMMY,(DUMMY,IT=1,NTEMP),                   bmdt 790
cc   1      IDUMMY,(DUMMY,IT=1,NTEMP),IP=1,IPRM)                        bmdt 800
          GOTO20                                                        bmdt 810
      ENDIF                                                             bmdt 820
C                                                                       bmdt 830
C     READ THE FIRST BLOCK OF REQUIRED BAND MODEL DATA                  bmdt 840
C                                                                       bmdt 850
      IPRM=IPARAM(IBLK)                                                 bmdt 860
      READ(ITB,rec=irecnm)                                              bmdt 870
     x (IBIN(IP),IMOL(IP),(SDZ(IT,IP),IT=1,NTEMP),IALF(IP),             bmdt 880
     1  (ODZ(IT,IP),IT=1,NTEMP),IP=1,IPRM)                              bmdt 890
C                                                                       bmdt 900
C     DETERMINE FIRST REQUIRED PARAMETER SET                            bmdt 910
C                                                                       bmdt 920
      IP=0                                                              bmdt 930
   30 IP=IP+1                                                           bmdt 940
      IF(IV.GT.IBIN(IP))GOTO30                                          bmdt 950
C
C     READ THE BAND MODEL PARAMETERS (ABSORPTION CROSS-SECTIONS ONLY)
C     AND POSITION AT THE PROPER PLACE OF EACH FILE OF EACH X-SPECIES.
C
      IPX = 0
 35   IPX = 1+IPX
      IBINX =0
      IMOLX=0
      SDZX(1)=0
      SDZX(2)=0
      SDZX(3)=0
      SDZX(4)=0
      SDZX(5)=0
      IALFX=0
      ODZX(1)=0
      ODZX(2)=0
      ODZX(3)=0
      ODZX(4)=0
      ODZX(5)=0
      if(iv.gt.1789) go to 36
      READ(ITBX,925,ERR=36,end=36)BMSTR
925   format(A129)
      CALL GETBMx(BMSTR,IBINX,IMOLX,SDZX,IALFX,ODZX)
      IF (IV .GT. IBINX) GO TO 35
 36   continue
C                                                                       bmdt 960
C     SET MSMAX TO laytwo FOR MULTIPLE SCATTERING                       bmdt 970
C                                                                       bmdt 980
      MSMAX=IMULT*laytwo                                                bmdt 990
C                                                                       bmdt1000
C     SET TEMPERATURE INTERPOLATION INDICES FOR EACH LAYER              bmdt1010
C                                                                       bmdt1020
      IKHI=IKMX                                                         bmdt1030
      DO 70 MSOFF=0,MSMAX,laytwo                                        bmdt1040
          DO 60 IK=1,IKHI                                               bmdt1050
              IKOFF=IK+MSOFF                                            bmdt1060
              TT=TBBY(IKOFF)                                            bmdt1070
              IF(TT.LE.TBAND(1))THEN                                    bmdt1080
                  JJ(IKOFF)=2                                           bmdt1090
                  FF(IKOFF)=1.                                          bmdt1100
              ELSEIF(TT.GE.TBAND(NTEMP))THEN                            bmdt1110
                  JJ(IKOFF)=NTEMP                                       bmdt1120
                  FF(IKOFF)=0.                                          bmdt1130
              ELSE                                                      bmdt1140
                  DO 40 J=2,NTEMP                                       bmdt1150
                      IF(TT.LE.TBAND(J))GOTO50                          bmdt1160
   40             CONTINUE                                              bmdt1170
   50             JJ(IKOFF)=J                                           bmdt1180
                  FF(IKOFF)=(TBAND(J)-TT)/(TBAND(J)-TBAND(J-1))         bmdt1190
              ENDIF                                                     bmdt1200
C                                                                       bmdt1210
C     SET TEMPERATURE SCALING PARAMETERS                                bmdt1220
C                                                                       bmdt1230
   60     WT(IKOFF)=SQRT(TT/TZERO)                                      bmdt1240
          IKHI=IMSMX                                                    bmdt1250
 70    CONTINUE
C                                                                       bmdt1260
C     IF NO SINGLE SCATTER SOLAR, RETURN                                bmdt1270
C                                                                       bmdt1280
      IF(IEMSCT.NE.2)RETURN                                             bmdt1290
C                                                                       bmdt1300
C     SET TEMPERATURE INTERPOLATION INDICES FOR SOLAR LAYERS            bmdt1310
C                                                                       bmdt1320
      IKHI=IKMX+1                                                       bmdt1330
      DO 110 MSOFF=0,MSMAX,laytwo                                           bmdt1340
          DO 100 IK=1,IKHI                                              bmdt1350
              IKOFF=IK+MSOFF                                            bmdt1360
              DO 105 K=1,nspect                                         bmdt1370
                 if ( k .le. nspc) then                                
                    TT=TBBYS(IKOFF,K)                                
                 else
                    tt=tbbysx(ikoff,k-nspc)
                 endif
                    IF(TT.LE.TBAND(1))THEN    
                       JJS(IKOFF,K)=2            
                       FFS(IKOFF,K)=1.                               
                    ELSEIF(TT.GE.TBAND(NTEMP))THEN                      
                       JJS(IKOFF,K)=NTEMP                              
                       FFS(IKOFF,K)=0.                                 
                    ELSE                                                
                       DO 80 J=2,NTEMP                                 
                          IF(TT.LE.TBAND(J))GOTO90                    
 80                    CONTINUE                                        
 90                    JJS(IKOFF,K)=J                                  
                       FFS(IKOFF,K)=(TBAND(J)-TT)/(TBAND(J)-TBAND(J-1))
                    ENDIF                                               
                    WTS(IKOFF,K)=SQRT(TT/TZERO)           
 105           continue
 100        continue
          IKHI=IMSMX+1                                                  bmdt1530
 110   CONTINUE
      RETURN                                                            bmdt1540
      END                                                               bmdt1550
      BLOCK DATA  BMDCO1                                              
c
c
      INCLUDE 'parameter.list'
c
c
c
      COMMON/BMDCOM/IBNDWD,IP,IPRM,IBLK,IBLOCK,IPARAM(273),NTEMP,       bmc1 130
     1  TBAND(5),SDZ(5,250),ODZ(5,250),IBIN(250),IMOL(250),IALF(250),   bmc1 140
     2  SD(5,mmolt2),OD(5,mmolt),ALF0(MMOLT),WT(laythr),
     $  WTS(laythr,mmolt),ITB,irecnm,JJ(laythr),FF(laythr),
     $ JJS(laythr,mmolt),FFS(laythr,mmolt),          
     4          DOPFAC(mmolt),DOP0(mmolt),DOPSUM(mmolt),
     $     COLSUM(mmolt),ODSUM(mmolt),SDSUM(mmolt2)                     cfc
c
      DATA (DOPFAC(i),i=1,nspect)
     $     /1.3945E-6, 0.8922E-6, 0.8543E-6, 0.8921E-6, 1.1183E-6,      bmc1 190
     1 1.4777E-6, 1.0463E-6, 1.0805E-6, 0.7395E-6, 0.8726E-6, 1.4342E-6,bmc1 200
     2 0.7456E-6, 
     $     5.04981E-07, 5.38245E-07, 5.79088E-07, 6.30907E-07,
     $     6.36486E-07, 4.32375E-07, 4.52709E-07, 4.76212E-07,
     $     5.99528E-07,6.65841E-07,5.83391E-07,4.7721E-07,5.69488E-07/
      END                                                     
      SUBROUTINE BMFLUX(IV,S0)                                          bmfx 100
      include 'parameter.list'
C                                                                       bmfx 110
C     MODTRAN VERSION OF FLXADD (NO K LOOP)                             bmfx 120
      DOUBLE PRECISION AER1,AER2,AERA,AERU,AERV,AERC,AERCX,EX1,EX2,     bmfx 130
     1  DENO,DNMO,DNM1                                                  bmfx 140
      logical MODTRN                                                    bmfx 150
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON/CARD1/MODEL,ITYPE,IEMSCT,M1,M2,M3,IM,NOPRNT,TBOUND,SALB,   bmfx 170
     1  MODTRN                                                          bmfx 180
      COMMON/TRAN/TMOLS(LAYDIM),TAERS(LAYDIM),TCONT(LAYDIM),
     1  DCONT(LAYTWO)
      COMMON/CNTRL/KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT                 bmfx 200
      COMMON/SOLS/AH1(LAYTWO),ARH(LAYTWO),WPATHS(LAYTHR,65),
     1 PA(LAYTWO),PR(LAYTWO),ATHETA(LAYDIM+1),ADBETA(LAYDIM+1),
     2 LJ(LAYTWO+1),JTURN,ANGSUN,CSZEN(LAYTWO),TBBYS(LAYTHR,12),
     3 PATMS(LAYTHR,12)
      COMMON/CNSTNS/PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                       bmfx 240
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
C***********************************************************************bmfx 320
C                                                                       bmfx 330
C   INPUT PARAMETERS:                                                   bmfx 340
C   ----------------                                                    bmfx 350
C   TLE(N)     - TEMPERATURE (KELVIN) OF UPPER EDGE OF LAYER N          bmfx 360
C   COSBAR(N)  - ASYMMETRY FACTOR FOR LAYER N                           bmfx 370
C   TAER(N)    - TOTAL AEROSOL OPTICAL THICKNESS OF LAYER N             bmfx 380
C   TAERS(N)   - AEROSOL SCATTERING OPTICAL THICKNESS OF LAYER N        bmfx 390
C   TCONT(N)   - MOLECULAR CONTINUUM OPTICAL THICKNESS FOR LAYER N      bmfx 400
C   TMOLS(N)   - RAYLEIGH SCATTERING OPTICAL THICKNESS FOR LAYER N      bmfx 410
C   CSZEN(N)   - COSINE OF SOLAR ZENITH ANGLE FOR LAYER N               bmfx 420
C   IV         - FREQUENCY (WAVENUMBER)                                 bmfx 430
C   S0         - SOLAR INTENSITY AT TOP OF ATMOSPHERE                   bmfx 440
C   STRN(N)    - SOLAR TRANSMISSION TO BOTTOM OF LAYER N                bmfx 450
C                                                                       bmfx 460
C   OUTPUT PARAMETERS:                                                  bmfx 470
C   -----------------                                                   bmfx 480
C   BTOP(N)    - BLACK BODY FLUX FOR UPPER EDGE TEMPERATURE OF LAYER N  bmfx 490
C   OMEGA0(N)  - SCATTERING ALBEDO FOR LAYER N                          bmfx 500
C   UPF(1,N)   - UPWARD THERMAL FLUX AT UPPER EDGE OF LAYER N           bmfx 510
C   DNF(1,N)   - DOWNWARD THERMAL FLUX AT UPPER EDGE OF LAYER N         bmfx 520
C   UMF(N)     - MEAN UPWARD THERMAL FLUX OF LAYER N                    bmfx 530
C   UMFS(N)    - MEAN UPWARD SOLAR FLUX OF LAYER N                      bmfx 540
C   DMF(N)     - MEAN DOWNWARD THERMAL FLUX OF LAYER N                  bmfx 550
C   DMFS(N)    - MEAN DOWNWARD SOLAR FLUX OF LAYER N                    bmfx 560
C   fdnsrt     - DOWNWARD SOLAR FLUX AT THE SURFACE                     bmfx 570
C   fdntrt     - DOWNWARD THERMAL FLUX AT THE SURFACE                   bmfx 580
C                                                                       bmfx 590
C   INTERNAL PARAMETERS:                                                bmfx 600
C   -------------------                                                 bmfx 610
C   upfs(1,n)  - UPWARD SOLAR FLUX AT UPPER EDGE OF LAYER N             bmfx 620
C   dnfs(1,n)  - DOWNWARD SOLAR FLUX AT UPPER EDGE OF LAYER N           bmfx 630
C   b0         - BACKSCATTER FRACTION ALONG SOLAR PATH AT LAYER N       bmfx 640
C   EUP(N)     - INTRINSIC UPWARD THERMAL FLUX OF LAYER N               bmfx 650
C   EDN(N)     - INTRINSIC DOWNWARD THERMAL FLUX OF LAYER N             bmfx 660
C   TDF(N)     - INTRINSIC THERMAL TRANSMISSION OF LAYER N              bmfx 670
C   REF(N)     - INTRINSIC THERMAL REFLECTANCE OF LAYER N               bmfx 680
C   TDFS(N)    - INTRINSIC SOLAR TRANSMISSION OF LAYER N                bmfx 690
C   REFS(N)    - INTRINSIC SOLAR REFLECTANCE OF LAYER N                 bmfx 700
C   X          - OPTICAL THICKNESS OF A GIVEN LAYER                     bmfx 710
C                                                                       bmfx 720
C***********************************************************************bmfx 730
      V=FLOAT(IV)                                                       bmfx 740
      NG=ML                                                             bmfx 750
      NLAYRS=NG-1                                                       bmfx 760
C                                                                       bmfx 770
C     BLACK BODY FLUX FOR LAYER EDGE TEMPERATURE                        bmfx 780
      DO 10 N=1,ng                                                      bmfx 790
   10 BTOP(N)=pi*BBFN(TLE(N),V)                                         bmfx 800
C                                                                       bmfx 810
C**** COMPOSITE DOWNWARDS REFLECTION                                    bmfx 820
      RDNCN=0.                                                          bmfx 830
C                                                                       bmfx 840
C     ADDING GROUND LAYER                                               bmfx 850
C     DEFINE INITIAL UPWARD COMPOSITE SURFACE REFLECTANCE               bmfx 860
      RUPCN=SALB                                                        bmfx 870
C     SURFACE EMISSION                                                  bmfx 880
C                                                                       bmfx 890
      EUPCN=(1.-RUPCN)*BTOP(NG)                                         bmfx 900
      EUP(NG)=EUPCN                                                     bmfx 910
      EUPC(NG)=EUPCN                                                    bmfx 920
      rupc(ng)=0.                                                       bmfx 930
      IF(IEMSCT.EQ.2)THEN                                               bmfx 940
          RDNCNS=0.                                                     bmfx 950
          RUPCNS=SALB                                                   bmfx 960
          EUPCNS=RUPCNS*CSZEN(NLAYRS)*STRN(NLAYRS)*S0                   bmfx 970
          EUPS(NG)=EUPCNS                                               bmfx 980
          EUPCS(NG)=EUPCNS                                              bmfx 990
          rupcs(ng)=0.                                                  bmfx1000
      ENDIF                                                             bmfx1010
C                                                                       bmfx1020
C     UPWARD ADDING LOOP(1-N)                                           bmfx1030
      DO 20 N=NLAYRS,1,-1                                               bmfx1040
C                                                                       bmfx1050
C         NOTE: TOP LAYER IS LAYER 1                                    bmfx1060
          X=TAUT(N)+TAER(N)+TCONT(N)+TMOLS(N)                           bmfx1070
          OMEGA0(N)=0.                                                  bmfx1080
          IF(X.GT.0.)OMEGA0(N)=(TAERS(N)+TMOLS(N))/X                    bmfx1090
C                                                                       bmfx1100
C         USE TWO STREAM APPROXIMATION FOR THERMAL                      bmfx1110
          IF(OMEGA0(N).le.0.99999)then                                  bmfx1120
              AER1=1.-OMEGA0(N)                                         bmfx1130
              AER2=1.-OMEGA0(N)*COSBAR(N)                               bmfx1140
              AERA=SQRT(AER1/AER2)                                      bmfx1150
              AERU=(1.-AERA)/2.                                         bmfx1160
              AERV=(1.+AERA)/2.                                         bmfx1170
              AERC=SQRT(3.*AER1*AER2)                                   bmfx1180
              IF(X.LT.1.E-4/AERC)THEN                                   bmfx1190
                  AERCX=AERC*X                                          bmfx1200
                  EX1=1.-AERCX+.5*AERCX*AERCX                           bmfx1210
                  DNMO=(BTOP(N)-BTOP(N+1))*(1.-AERCX+.5*AERA*AERCX)     bmfx1220
              ELSEIF(X.LT.BIGEXP/AERC) THEN                             bmfx1230
                  AERCX=AERC*X                                          bmfx1240
                  EX1=EXP(-AERCX)                                       bmfx1250
                  DNMO=(BTOP(N)-BTOP(N+1))                              bmfx1260
     1              *(AERV-EX1*(AERA+EX1*AERU))/AERCX                   bmfx1270
              ELSE                                                      bmfx1280
                  EX1=0.                                                bmfx1290
                  DNMO=((BTOP(N)-BTOP(N+1))*AERV/AERC)/X                bmfx1300
              ENDIF                                                     bmfx1310
              EX2=EX1*EX1                                               bmfx1320
              DNM1=AERV+AERU*EX2                                        bmfx1330
              DENO=AERV*AERV-AERU*AERU*EX2                              bmfx1340
              EUP(N)=(BTOP(N)*DNM1-DNMO-BTOP(N+1)*EX1)/DENO*AERA        bmfx1350
              EDN(N)=(BTOP(N+1)*DNM1+DNMO-BTOP(N)*EX1)/DENO*AERA        bmfx1360
              IF(EUP(N).LT.0)EUP(N)=0.                                  bmfx1370
              IF(EDN(N).LT.0)EDN(N)=0.                                  bmfx1380
              REF(N)=AERU*AERV*(1.-EX2)/DENO                            bmfx1390
              TDF(N)=AERA/DENO*EX1                                      bmfx1400
          else                                                          bmfx1410
              TDF(N)=1./(1.+SQRT(3.)*(1.-COSBAR(N))/2.*X)               bmfx1420
              REF(N)=1.-TDF(N)                                          bmfx1430
              EUP(N)=0.                                                 bmfx1440
              EDN(N)=0.                                                 bmfx1450
          endif                                                         bmfx1460
C                                                                       bmfx1470
C         CALCULATE COMPOSITE FLUXES AND REFLECTANCES                   bmfx1480
          DENO=1.-RUPCN*REF(N)                                          bmfx1490
          EUPCN=EUP(N)+(EUPCN+EDN(N)*RUPCN)*TDF(N)/DENO                 bmfx1500
          RUPCN=REF(N)+TDF(N)*RUPCN*TDF(N)/DENO                         bmfx1510
          EUPC(N)=EUPCN                                                 bmfx1520
          RUPC(N)=RUPCN                                                 bmfx1530
cjv 8/29/95 Lex fix. This line is moved to be after bmfx2110.
cjv          RUPCS(N)=RUPCNS                                               bmfx1540
cjv ^
          IF(IEMSCT.EQ.2)THEN                                           bmfx1550
C                                                                       bmfx1560
C             CALCULATE PARAMETERS FOR SOLAR HYBRID MODIFIED            bmfx1570
C             DELTA EDDINGTON 2-STREAM APPROXIMATION                    bmfx1580
              SMU=CSZEN(N)                                              bmfx1590
              OME0=OMEGA0(N)                                            bmfx1600
              IF(OME0.GE.0.999999)OME0=0.999999                         bmfx1610
              G=COSBAR(N)                                               bmfx1620
              Q=4.*(1.-G**2.*(1.-SMU))                                  bmfx1630
              b0=.5                                                     bmfx1640
              if(g.ne.0.)b0=BETABS(smu,g)                               bmfx1650
              GM1=(7-3*G**2-OME0*(4+3*G)+OME0*G**2*(4*b0+3*G))/Q        bmfx1660
              GM2=-(1-G**2-OME0*(4-3*G)-OME0*G**2*(4*b0+3*G-4))/Q       bmfx1670
              IF(GM2.EQ.0.)GM2=1.E-10                                   bmfx1680
              SK=(GM1-GM2)*(gm1+GM2)                                    bmfx1690
              CS=OME0*(b0-SMU*(GM1*b0+GM2*(1.-b0)))/(1.-SK*SMU*SMU)     bmfx1700
              SK=SQRT(sk)                                               bmfx1710
              YS=CS*(1.+SMU*GM1)-OME0*b0                                bmfx1720
              CS=SMU*CS                                                 bmfx1730
C                                                                       bmfx1740
C             CALCULATE UPWARD & DOWNWARD FLUX, EACH LAYER ALONE        bmfx1750
              EKTL=SK*X                                                 bmfx1760
              IF(EKTL.LT.BIGEXP)THEN                                    bmfx1770
                  EKT=EXP(-EKTL)                                        bmfx1780
              ELSE                                                      bmfx1790
                  EKT=1./BIGNUM                                         bmfx1800
              ENDIF                                                     bmfx1810
              EKT2=EKT*EKT                                              bmfx1820
              IF(X.LT.SMU*BIGEXP)THEN                                   bmfx1830
                  ETMU=EXP(-X/SMU)                                      bmfx1840
              ELSE                                                      bmfx1850
                  ETMU=1./BIGNUM                                        bmfx1860
              ENDIF                                                     bmfx1870
              GMPSK=GM1+SK                                              bmfx1880
              GMMSK=GM1-SK                                              bmfx1890
              DENOM=(EKT2*GMMSK)-GMPSK                                  bmfx1900
              ASXP=-GMMSK*(YS*EKT-GMPSK*CS*ETMU)/DENOM                  bmfx1910
              ASNEW=-(-GMPSK*CS*ETMU*EKT+YS*EKT2)/DENOM                 bmfx1920
              BSNEW=(YS-CS*ETMU*EKT*GMMSK)/DENOM                        bmfx1930
              EUPS(N)=ASNEW+BSNEW+CS                                    bmfx1940
              if(eups(n).lt.0.)eups(n)=0.                               bmfx1950
              EDNS(N)=(ASXP+BSNEW*GMPSK*EKT+YS*ETMU)/GM2                bmfx1960
              if(edns(n).lt.0.)edns(n)=0.                               bmfx1970
              FC=1./(CSZEN(N))                                          bmfx1980
              REFS(N)=EUPS(N)*FC                                        bmfx1990
              IF(X.LT.BIGEXP*CSZEN(N))THEN                              bmfx2000
                  TDFS(N)=EDNS(N)*FC+EXP(-X/CSZEN(N))                   bmfx2010
              ELSE                                                      bmfx2020
                  TDFS(N)=EDNS(N)*FC                                    bmfx2030
              ENDIF                                                     bmfx2040
              s0strn=s0*strn(n-1)                                       bmfx2050
              EUPS(N)=EUPS(N)*S0STRN                                    bmfx2060
              EDNS(N)=EDNS(N)*S0STRN                                    bmfx2070
              DENOS=1.-RUPCNS*REFS(N)                                   bmfx2080
              EUPCNS=EUPS(N)+(EUPCNS+EDNS(N)*RUPCNS)*TDFS(N)/DENOS      bmfx2090
              RUPCNS=REFS(N)+TDFS(N)*RUPCNS*TDFS(N)/DENOS               bmfx2100
              EUPCS(N)=EUPCNS                                           bmfx2110
cjv 8/29/95 Lex fix. The line bmfx1540 should be here.
          RUPCS(N)=RUPCNS                                               bmfx1540
cjv ^
          ENDIF                                                         bmfx2120
   20 continue                                                          bmfx2130
C                                                                       bmfx2140
C     NOW ADD DOWNWARD FROM TOP LAYER (N=1)                             bmfx2150
      EDNCN=0.                                                          bmfx2160
      DNF(1,1)=EDNCN                                                    bmfx2170
      UPF(1,1)=EUPC(1)                                                  bmfx2180
      RDNCN=0.                                                          bmfx2190
      IF(IEMSCT.EQ.2)THEN                                               bmfx2200
          EDNCNS=0.                                                     bmfx2210
          dnfs(1,1)=EDNCNS                                              bmfx2220
          upfs(1,1)=EUPCS(1)                                            bmfx2230
          RDNCNS=0.                                                     bmfx2240
      ENDIF                                                             bmfx2250
      nm1=1                                                             bmfx2260
      DO 30 N=2,NG                                                      bmfx2270
          DENO=1.-RDNCN*REF(nm1)                                        bmfx2280
          EDNCN=EDN(nm1)+(EDNCN+EUP(nm1)*RDNCN)*TDF(nm1)/DENO           bmfx2290
          RDNCN=REF(nm1)+TDF(nm1)*TDF(nm1)*RDNCN/DENO                   bmfx2300
          PEFUP=(EUPC(N)+EDNCN*RUPC(N))/DENO                            bmfx2310
          PEFDN=(EDNCN+EUPC(N)*RDNCN)/DENO                              bmfx2320
          UPF(1,N)=PEFUP                                                bmfx2330
          DNF(1,N)=PEFDN                                                bmfx2340
          UMF(nm1)=.5*(UPF(1,nm1)+UPF(1,N))                             bmfx2350
          DMF(nm1)=.5*(DNF(1,nm1)+DNF(1,N))                             bmfx2360
          UMB(N)=UPF(1,N)                                               bmfx2370
          DMB(nm1)=DNF(1,nm1)                                           bmfx2380
          IF(IEMSCT.EQ.2)THEN                                           bmfx2390
              DENOS=1.-RDNCNS*REFS(nm1)                                 bmfx2400
              EDNCNS=EDNS(nm1)+(EDNCNS+EUPS(nm1)*RDNCNS)*TDFS(nm1)/DENOSbmfx2410
              RDNCNS=REFS(nm1)+TDFS(nm1)*TDFS(nm1)*RDNCNS/DENOS         bmfx2420
              upfs(1,N)=(EUPCS(N)+EDNCNS*RUPCS(N))/DENOS                bmfx2430
              dnfs(1,N)=(EDNCNS+EUPCS(N)*RDNCNS)/DENOS                  bmfx2440
              UMFS(nm1)=.5*(upfs(1,nm1)+upfs(1,N))                      bmfx2450
              DMFS(nm1)=.5*(dnfs(1,nm1)+dnfs(1,N))                      bmfx2460
              UMBS(N)=upfs(1,N)                                         bmfx2470
              DMBS(nm1)=dnfs(1,nm1)                                     bmfx2480
          ENDIF                                                         bmfx2490
   30 nm1=n                                                             bmfx2500
      fdntrt=DNF(1,NG)                                                  bmfx2510
      UMB(1)=UPF(1,1)                                                   bmfx2520
      DMB(NG)=fdntrt                                                    bmfx2530
      if(iemsct.ne.2)return                                             bmfx2540
      SUN=S0                                                            bmfx2550
      fdnsrt=dnfs(1,NG)                                                 bmfx2560
      UMBS(1)=upfs(1,1)                                                 bmfx2570
      DMBS(NG)=fdnsrt                                                   bmfx2580
      RETURN                                                            bmfx2590
      END                                                               bmfx2600
      SUBROUTINE BMLDX                                                 
C     
C     THIS ROUTINE IS THE EQUIVALENT OF BMLOAD FOR THE X-MOLECULES.
C     
C     THIS SUBROUTINE (CALLED BY BMOD) LOADS BAND MODEL DATA FOR A SINGLE 
C     PARAMETER SET INTO THE MATRICES SDX, ODX AND ALF0X (FOUND IN /BMDCMX/).
C
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
c     modtran has 65 as magic number.  It INCLUDEs the usual 12 species
c     plus a host of other species and sub species.  Many
c     arrays have dimension 65.

      INCLUDE 'parameter.list'
      CHARACTER*8 CNAMEX                                       
C
C     TRANS VARIABLES
C
      COMMON/BMDCMX/ IPX,SDZX(5),ODZX(5),IBINX,IMOLX,IALFX
c
      COMMON /NAMEX/CNAMEX(MMOLX)                                               
      COMMON /MDATAX/ WMOLXT(MMOLX,laydim)                                  
      COMMON /MODELX/ DNSTYX(MMOLX,LAYDIM)                                  
      COMMON /RFRPTX/ DENPX(MMOLX,laydim+1),AMTPX(MMOLX,laydim+1)
      COMMON /NONAME/ TXX(MMOLX), WX(MMOLX), WPATHX(laythr,MMOLX)
      COMMON /SOLSX/  WPTHSX(laythr,MMOLX),TBBYSX(laythr,MMOLX),
     $     PATMSX(laythr,MMOLX)
      DATA ITBX/31/
c
c
c
      COMMON/BMDCOM/IBNDWD,IP,IPRM,IBLK,IBLOCK,IPARAM(273),NTEMP,      
     1  TBAND(5),SDZ(5,250),ODZ(5,250),IBIN(250),IMOL(250),IALF(250),  
     2  SD(5,mmolt2),OD(5,mmolt),ALF0(MMOLT),WT(laythr),
     $  WTS(laythr,mmolt),ITB,irecnm,JJ(laythr),FF(laythr),
     $ JJS(laythr,mmolt),FFS(laythr,mmolt),          
     4          DOPFAC(mmolt),DOP0(mmolt),DOPSUM(mmolt),
     $     COLSUM(mmolt),ODSUM(mmolt),SDSUM(mmolt2)                     cfc
C     
C     FILL THE SDX, ODX AND ALF0X MATRICES.                                  
C     
      IMX = IMOLX
      nsped=nspc+nspc
      IF(IMX.LE.NSPECx)THEN                                             
C        
C        LINE CENTER CONTRIBUTIONS                                    
C        
         IMTAIL=IMX+NSPECX
         DO 10 IT=1,NTEMP                                             
            SD(IT,IMX+nsped)=SDZX(IT)
            OD(IT,IMX+nspc)=ODZX(IT)
            SD(IT,IMTAIL+nsped)=0.                                   
 10      CONTINUE
         ALF0(IMX+nspc)=1.E-04*IALFX
      ELSE                                                             
C        
C        LINE TAIL CONTRIBUTIONS                                      
C        
         JMX=IALFX
         IF(JMX.EQ.0)THEN                                              
C           
C           ONE TAIL                                                 
C            
            DO 20 IT=1,NTEMP                                         
               SD(IT,IMX+nsped)=SDZX(IT)
 20         CONTINUE
         ELSE                                                         
C           
C           TWO TAILS                                                
C           
            DO 30 IT=1,NTEMP                                         
               SD(IT,JMX+nsped)=ODZX(IT)
               SD(IT,IMX+nsped)=SDZX(IT)
 30         CONTINUE
         ENDIF                                                        
      ENDIF                                                            
      RETURN                                                           
      END                                                              
      SUBROUTINE BMLOAD                                                 
C                                                                       
C  THIS SUBROUTINE (CALLED BY BMOD) LOADS BAND MODEL DATA FOR A SINGLE  
C  PARAMETER SET INTO THE MATRICES SD, OD AND ALF0 (FOUND IN /BMDCOM/). 
C                                                                       
C  ORIGINALLY, THE BAND MODEL PARAMETERS ARE STORE IN COMPACT PARAMETER 
C  SETS.  THERE ARE TWO TYPES OF PARAMETER SETS CORRESPONDING TO LINE   
C  CENTER AND LINE TAIL CONTRIBUTIONS.  IF IMOL(IP) IS 12 OR LESS, THE  
C  PARAMETER SET IP CONTAINS LINE CENTER MOLECULAR ABSORPTION INFO:     
C      IBIN(IP) = THE BIN NUMBER (MULTIPLIED BY IBNDWD GIVES THE        
C                                 FREQUENCY OF THE BIN CENTER)          
C      IMOL(IP) =  1 FOR H2O                                            
C               =  2 FOR CO2                                            
C               =  3 FOR O3                                             
C               =  4 FOR N2O                                            
C               =  5 FOR CO                                             
C               =  6 FOR CH4                                            
C               =  7 FOR O2                                             
C               =  8 FOR NO                                             
C               =  9 FOR SO2                                            
C               = 10 FOR NO2                                            
C               = 11 FOR NH3                                            
C               = 12 FOR HNO3                                           
C    SDZ(IT,IP) = AVERAGE MOLECULAR ABSORPTION COEFFICIENT PARAMETER    
C                 FOR THE IT'TH TEMPERATURE (CM-1/AMAGAT)               
C      IALF(IP) = LORENTZ LINE WIDTH PARAMETER AT STANDARD PRESSURE AND 
C                 TEMPERATURE (1.E-04 CM-1/ATM)                         
C    ODZ(IT,IP) = LINE DENSITY PARAMETER FOR THE TEMPERATURE IT (CM-1)  
C                                                                       
C  IF IMOL(IP) IS 12 OR MORE, THE PARAMETER SET IP CONTAINS CONTINUUM   
C  ABSORPTION INFORMATION AND                                           
C              IBIN(IP) = THE BIN NUMBER                                
C     IMOL(IP),IALF(IP) = 13 FOR  H2O TAIL CONTRIBUTION                 
C                       = 14 FOR  CO2 TAIL CONTRIBUTION                 
C                       = 15 FOR   O3 TAIL CONTRIBUTION                 
C                       = 16 FOR  N2O TAIL CONTRIBUTION                 
C                       = 17 FOR   CO TAIL CONTRIBUTION                 
C                       = 18 FOR  CH4 TAIL CONTRIBUTION                 
C                       = 19 FOR   O2 TAIL CONTRIBUTION                 
C                       = 20 FOR   NO TAIL CONTRIBUTION                 
C                       = 21 FOR  SO2 TAIL CONTRIBUTION                 
C                       = 22 FOR  NO2 TAIL CONTRIBUTION                 
C                       = 23 FOR  NH3 TAIL CONTRIBUTION                 
C                       = 24 FOR HNO3 TAIL CONTRIBUTION                 
C            SDZ(IT,IP) = MOLECULAR LINE WING CONTINUUM ABSORPTION      
C                         COEFFICIENT FOR SPECIES IMOL(IP) AT THE       
C                         IT'TH TEMPERATURE (CM-1/AMAGAT)               
C            ODZ(IT,IP) = MOLECULAR LINE WING CONTINUUM ABSORPTION      
C                         COEFFICIENT FOR SPECIES IALF(IP) AT THE       
C                         IT'TH TEMPERATURE (CM-1/AMAGAT)               
C         
c
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
c
      COMMON/BMDCOM/IBNDWD,IP,IPRM,IBLK,IBLOCK,IPARAM(273),NTEMP,       
     1  TBAND(5),SDZ(5,250),ODZ(5,250),IBIN(250),IMOL(250),IALF(250),
     2  SD(5,mmolt2),OD(5,mmolt),ALF0(MMOLT),WT(laythr),
     $  WTS(laythr,mmolt),ITB,irecnm,JJ(laythr),FF(laythr),
     $ JJS(laythr,mmolt),FFS(laythr,mmolt),          
     4          DOPFAC(mmolt),DOP0(mmolt),DOPSUM(mmolt),
     $     COLSUM(mmolt),ODSUM(mmolt),SDSUM(mmolt2)                     cfc
C                                                                       
C  FILL THE SD, OD AND ALF0 MATRICES.                                   
C                                                                       
      IM=IMOL(IP)                                                       
      IF(IM.LE.nspc)THEN                                               
C                                                                       
C         LINE CENTER CONTRIBUTIONS                                     
C                                                                       
          IMTAIL=IM+nspc                                               
          DO 10 IT=1,NTEMP                                              
              SD(IT,IM)=SDZ(IT,IP)                                      
              OD(IT,IM)=ODZ(IT,IP)                                      
   10     SD(IT,IMTAIL)=0.                                              
          ALF0(IM)=1.E-04*IALF(IP)                                      
      ELSE                                                              
C                                                                       
C         LINE TAIL CONTRIBUTIONS                                       
C                                                                       
          JM=IALF(IP)                                                   
          IF(JM.EQ.0)THEN                                               
C                                                                       
C             ONE TAIL                                                  
C                                                                       
              DO 20 IT=1,NTEMP                                          
   20         SD(IT,IM)=SDZ(IT,IP)                                      
          ELSE                                                          
C                                                                       
C             TWO TAILS                                                 
C                                                                       
              DO 30 IT=1,NTEMP                                          
                  SD(IT,JM)=ODZ(IT,IP)                                  
   30         SD(IT,IM)=SDZ(IT,IP)                                      
          ENDIF                                                         
      ENDIF                                                             
      RETURN                                                            
      END                                                               
      SUBROUTINE BMOD(IK,IKMX,IPATH,IV,MSOFF,MXFREQ)
C
C     THIS ROUTINE RETURNS THE TRANSMITTANCE AT A SPECTRAL RESOLUTION
C     OF 1 CM-1 FOR THE "NSPC" SPECIES
C           K = ( 1, 13)      H2O LINE (CENTERS, TAILS)
C             = ( 2, 14)      CO2 LINE (CENTERS, TAILS)
C             = ( 3, 15)       O3 LINE (CENTERS, TAILS)
C             = ( 4, 16)      N2O LINE (CENTERS, TAILS)
C             = ( 5, 17)       CO LINE (CENTERS, TAILS)
C             = ( 6, 18)      CH4 LINE (CENTERS, TAILS)
C             = ( 7, 19)       O2 LINE (CENTERS, TAILS)
C             = ( 8, 20)       NO LINE (CENTERS, TAILS)
C             = ( 9, 21)      SO2 LINE (CENTERS, TAILS)
C             = (10, 22)      NO2 LINE (CENTERS, TAILS)
C             = (11, 23)      NH3 LINE (CENTERS, TAILS)
C             = (12, 24)     HNO3 LINE (CENTERS, TAILS)
C
c
c
C
C     DECLARATIVE MODIFICATIONS DUE TO ADDING EXTRA 
C     MOLECULES (NAMELY, CFC'S).  ALSO INCLUDES SOME CLEANING UP.
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
c     modtran has 65 as magic number.  It INCLUDEs the usual 12 species
c     plus a host of other species and sub species.  Many
c     arrays have dimension 65.
      INCLUDE 'parameter.list'
      LOGICAL XTRAN,MLTRNX
      CHARACTER*129 BMSTR
C
C     TRANS VARIABLES
C
      COMMON /BMDCMX/ IPX,SDZX(5),ODZX(5),IBINX,IMOLX,IALFX
      COMMON /MDATAX/ WMOLXT(MMOLX,laydim)                                  
      COMMON /MODELX/ DNSTYX(MMOLX,LAYDIM)                                  
      COMMON /RFRPTX/ DENPX(MMOLX,laydim+1),AMTPX(MMOLX,laydim+1)
      COMMON /NONAME/ TXX(MMOLX), WX(MMOLX), WPATHX(laythr,MMOLX)
      COMMON /SOLSX/  WPTHSX(laythr,MMOLX),TBBYSX(laythr,MMOLX),
     $     PATMSX(laythr,MMOLX)
      DATA XTRAN/.TRUE./
      DATA ITBX/31/
c
c     end declarations due to the extra molecules.
c
c
      SAVE MOLTRN
      save colo3
      LOGICAL MODTRN,MOLTRN
      COMMON RELHUM(LAYDIM),HSTOR(LAYDIM),ICH(4),VH(17),TX(65),W(65)  
      COMMON IMSMX,WPATH(LAYTHR,65),TBBY(LAYTHR),PATM(LAYTHR),NSPEC,   
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX2(47),AWCCON(5)  
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON/SOLS/AH1(LAYTWO),ARH(LAYTWO),WPATHS(LAYTHR,65),
     1 PA(LAYTWO),PR(LAYTWO),ATHETA(LAYDIM+1),ADBETA(LAYDIM+1),
     2 LJ(LAYTWO+1),JTURN,ANGSUN,CSZEN(LAYTWO),TBBYS(LAYTHR,12),
     3 PATMS(LAYTHR,12)
      COMMON/CARD1/MODEL,ITYPE,IEMSCT,M1,M2,M3,IM,NOPRNT,TBOUND,SALB    bmdt 230
     1  ,MODTRN                                                         bmdt 240
      COMMON/BMDCOM/IBNDWD,IP,IPRM,IBLK,IBLOCK,IPARAM(273),NTEMP,       cssi
     1  TBAND(5),SDZ(5,250),ODZ(5,250),IBIN(250),IMOL(250),IALF(250),
     2  SD(5,mmolt2),OD(5,mmolt),ALF0(MMOLT),WT(laythr),
     $  WTS(laythr,mmolt),ITB,irecnm,JJ(laythr),FF(laythr),
     $ JJS(laythr,mmolt),FFS(laythr,mmolt),          
     4          DOPFAC(mmolt),DOP0(mmolt),DOPSUM(mmolt),
     $     COLSUM(mmolt),ODSUM(mmolt),SDSUM(mmolt2)                     cfc
C
C     DOPFAC (UNITLESS) EQUALS SQRT(2 LN2 R T / M)/C WHERE T IS THE
C     STANDARD TEMPERATURE (273.15K) AND M IS MOLECULAR WEIGHT.
C
      DATA DV/1./,ipath0/0/
C
C     IF IV IS BEYOND BAND MODEL TAPE RANGE, SET TRANSMITTANCES TO 1
C
      IF(IV.GT.MXFREQ)THEN
          DO 10 K=1,NSPC
             TX(KPOINT(K))=1.
 10       CONTiNUE
C
          DO 15 KX=1,NSPECX
             TXX(KX)=1.
 15       CONTiNUE
          RETURN
      ENDIF
C
C     IK=0 IS THE INITIAL CALL FOR EACH WAVENUMBER AND IS MADE PRIOR
C     TO THE LOOP OVER LAYERS.  ik=-1 is the initial call to bmod
c     after the multiple scattering loop is complete
C
      IF(IK.EQ.0)THEN
C
C         IS THERE MOLECULAR DATA FOR FREQUENCY IV?
C
          IF(IBIN(IP).NE.IV)THEN
              MOLTRN=.FALSE.
              RETURN
          ENDIF
          MOLTRN=.TRUE.
c
          if(ibinx .ne. iv) then
             mltrnx=.false.
          else
             mltrnx = .true.
          endif
C
C         FOR EACH SPECIES, SET THE 'NO DATA' INDICATOR
C
          DO 20 IML=1,nspect2
   20     SD(1,IML)=0.
c
C         LOAD MOLECULAR DATA FOR PARAMETER SET IP
C
   30     CALL BMLOAD
C
C         IS IP THE LAST PARAMETER SET IN BLOCK IBLK?
C
          IF(IP.LT.IPRM)THEN
              IP=IP+1
              IF(IBIN(IP).EQ.IV)GOTO30
          ELSE
C
C                 READ THE NEXT BLOCK OF THE BAND MODEL TAPE
C
              IF(IBLK.LT.IBLOCK)THEN
                  IBLK=IBLK+1
                  IPRM=IPARAM(IBLK)
                  irecnm = iblk +1
                  if(irecnm.lt.273)
     x            READ(ITB,rec=irecnm)(IBIN(IP),IMOL(IP),              
     1              (SDZ(IT,IP),IT=1,NTEMP),IALF(IP),
     2              (ODZ(IT,IP),IT=1,NTEMP),IP=1,IPRM)
                  IP=1
              ENDIF
          ENDIF
c
c
c         load molecular data for parameter set ipx
c
ccc       if (xtran .and. mltrnx) then
          if ( mltrnx) then
 27          call bmldx
             ipx = ipx+1
             IBINX =0
             IMOLX=0
             SDZX(1)=0
             SDZX(2)=0
             SDZX(3)=0
             SDZX(4)=0
             SDZX(5)=0
             IALFX=0
             ODZX(1)=0
             ODZX(2)=0
             ODZX(3)=0
             ODZX(4)=0
             ODZX(5)=0
             if(iv.gt.1789) go to 29
             READ(ITBX,925,ERR=29,end=29)BMSTR
925          format(A129)
             call GETBMX(BMSTR,IBINX,IMOLX,SDZX,IALFX,ODZX)             
             if (ibinx .eq. iv) go to 27
          endif            
 29       continue
c
          DV=IBNDWD
C
C         ZERO LAYER LOOP QUANTITIES; DEFINE STANDARD DOPPLER WIDTH
C
          DO 40 K=1,nspect
              DOP0(K)=IV*DOPFAC(K)
              COLSUM(K)=0.
              DOPSUM(K)=0.
              ODSUM(K)=0.
              SDSUM(K)=0.
              SDSUM(K+nspect)=0.
 40        continue
              colo3=0.
c
           return
      ELSEif(ik.eq.-1)then
          DO 50 K=1,nspect
              COLSUM(K)=0.
              DOPSUM(K)=0.
              ODSUM(K)=0.
              SDSUM(K)=0.
              SDSUM(K+Nspect)=0.
 50        continue
c
              colo3=0.
          RETURN
      ENDIF
      IF(.NOT.MOLTRN)THEN
          DO 60 K=1,NSPC
   60     TX(KPOINT(K))=1.
          RETURN
      ENDIF
C
C     START CALCULATION OF MOLECULAR TRANSMITTANCE
C
      IF(IEMSCT.EQ.0 .OR. IEMSCT.EQ.3)THEN
C         LOOP OVER ALL LAYERS FOR TRANSMITTANCE CALCULATIONS
          LMAX=IKMX
      ELSEIF(IEMSCT.EQ.1)THEN
C         LOOP OVER SINGLE LAYER FOR RADIANCE CALCULATION WITHOUT SOLAR
          LMAX=IK
      ELSE
          GOTO(70,80,90),IPATH
C         SKIP LAYER LOOP COMPLETELY FOR FIRST SOLAR PATH WITH IPATH=1
   70     LMAX=0
          LS=1
          GOTO100
C         LOOP OVER SINGLE LAYER ONLY FOR "L PATH" WITH IPATH=2
   80     LMAX=IK
          IF(MSOFF.GT.0)LMAX=0
          LS=IK+1
          GOTO100
C         LOOP OVER SINGLE LAYER IF NOT PERFORMED WHEN IPATH EQUALED 2
   90     LMAX=0
          IF(MSOFF.GT.0 .or. ipath0.ne.2)LMAX=IK
  100     ipath0=ipath
      ENDIF
C
C     START SPECIES LOOP
C
      nsped=nspc+nspc 
      DO 130 Knew=1,nspect
          k = knew
          if (knew .le. nspc) then
             ksd = k
             KP=KPOINT(K)
             KTAIL=K+NSPC
          else
             kp = knew-nspc
             ksd = kp+nsped
             ktail=ksd+nspecx
          endif
C
C         CHECK IF LINE CENTER CONTRIBUTES TO ABSORPTION
C
          IF(SD(1,Ksd).GT.0.)THEN
C
C             LOOP OVER LAYERS
C
              DO 110 L = IK,LMAX
C
C                 INTERPOLATE BAND MODEL PARAMETERS OVER TEMPERATURE
C
                  MSOFFL=MSOFF+L
                  J=JJ(MSOFFL)
                  F=FF(MSOFFL)
                  JM1=J-1
                  STORE=SD(J,Ksd)
                  ABSM=STORE+F*(SD(JM1,Ksd)-STORE)
                  STORE=OD(J,K)
                  DINV=STORE+F*(OD(JM1,K)-STORE)
                  STORE=SD(J,KTAIL)
                  TAIL=STORE+F*(SD(JM1,KTAIL)-STORE)
C
C                 PERFORM CURTIS-GODSON SUMS
C
                  PL=PATM(MSOFFL)
                  if (knew .gt. nspc) PL = 1.
                  WL=WT(MSOFFL)
                  if (knew .le. nspc) STORE=WPATH(MSOFFL,KP)
                  if (knew .gt. nspc) STORE=WPATHx(MSOFFL,KP)
                  SDSUM(KTAIL)=SDSUM(KTAIL)+STORE*TAIL*PL
                  STORE=ABSM*STORE
                  SDSUM(Ksd)=SDSUM(Ksd)+STORE
                  STORE=DINV*STORE
                  ODSUM(K)=ODSUM(K)+STORE 
                  DOPSUM(K)=DOPSUM(K)+STORE*WL
                  STORE=STORE*PL/WL
                  IF(K.EQ.2)THEN
                      COLSUM(K)=COLSUM(K)+STORE/SQRT(WL)
                  ELSE
                      COLSUM(K)=COLSUM(K)+STORE
                  ENDIF
                  if(k.eq.3)colo3=colo3+store*dinv*pl/wl
  110         CONTINUE
              tdepth=sdsum(ktail)
              depth=SDSUM(Ksd)
              odbar=odsum(k)
              adbar=dopsum(k)
              acbar=colsum(k)
              acbar2=0.
              if(k.eq.3)acbar2=colo3
C
C             IF SOLAR PATH, CALCULATE ADDITIONAL LAYER
C
              IF(IEMSCT.EQ.2 .AND. IPATH.NE.3)THEN
                  MSOFFL=MSOFF+LS
                  J=JJS(MSOFFL,K)
                  F=FFS(MSOFFL,K)
                  JM1=J-1
                  STORE=SD(J,Ksd)
                  ABSM=STORE+F*(SD(JM1,Ksd)-STORE)
                  STORE=OD(J,K)
                  DINV=STORE+F*(OD(JM1,K)-STORE)
                  STORE=SD(J,KTAIL)
                  TAIL=STORE+F*(SD(JM1,KTAIL)-STORE)
                  if (knew .le. nspc) PL=PATMS(MSOFFL,K)
                  if (knew .gt. nspc) PL=1.
                  WL=WTS(MSOFFL,K)
                  if (knew .le. nspc) STORE=WPATHS(MSOFFL,KP)
                  if (knew .gt. nspc) STORE=WPTHSx(MSOFFL,KP)
                  if(msoff.eq.0)then
c
c                     L-shaped path
                      tdepth=tdepth+STORE*TAIL*PL
                      store=absm*store
                      depth=depth+STORE
                      store=DINV*store
                      odbar=odbar+store
                      adbar=adbar+store*wl
                      store=store*pl/wl
                      IF(K.EQ.2)store=store/SQRT(WL)
                      acbar=acbar+store
                      if(k.eq.3)acbar2=acbar2+store*dinv*pl/wl
                  else
c
c                     solar path only
                      tdepth=STORE*TAIL*PL
                      depth=ABSM*STORE
                      odbar=DINV*depth
                      adbar=odbar*WL
                      acbar=odbar*PL/WL
                      IF(K.EQ.2)acbar=acbar/SQRT(WL)
                      if(k.eq.3)acbar2=acbar*dinv*pl/wl
                  endif
              ENDIF
C
C             CHECK FOR WEAK LINE
C
              IF(depth.LT.0.001)THEN
                  TRANSM=1.-depth
                  tx(14)=tx(14)+depth
              ELSE
C
C                 CALCULATE EQUIVALENT WIDTH TRANSMITTANCE
C
                  ODBAR=odbar/depth
                  ADBAR=DOP0(K)*adbar/depth
                  ACBAR=ALF0(K)*acbar/depth
                  if(acbar2.ne.0.)acbar2=acbar2/depth*ALF0(k)**2
                  CALL BMTRAN(depth,ACBAR,ADBAR,ODBAR,DV,TRANSM,acbar2)
                  if(msoff.eq.0)then
                      if(transm.gt.0.)then
                          tx(14)=tx(14)-log(transm)
                      else
                          tx(14)=tx(14)+depth
                      endif
                  endif
              ENDIF
C
C         ADD LINE TAIL CONTRIBUTIONS
C
              if (knew .le. nspc) TX(KP)=TRANSM*EXP(-tdepth)
              tx(14)=tx(14)+tdepth
              if (knew .gt. nspc) TXx(KP)=TRANSM*EXP(-tdepth)
c
C         CHECK IF LINE TAILS CONTRIBUTE TO ABSORPTION
C
          ELSEIF(SD(1,KTAIL).GT.0.)THEN
C
C             LOOP OVER LAYERS
C
              DO 120 L = IK,LMAX
C
C                 INTERPOLATE BAND MODEL PARAMETERS OVER TEMPERATURE
C
                  MSOFFL=MSOFF+L
                  J=JJ(MSOFFL)
                  STORE=SD(J,KTAIL)
                  TAIL=STORE+FF(MSOFFL)*(SD(J-1,KTAIL)-STORE)
C
C                 PERFORM CURTIS-GODSON SUMS
C
                  if (knew .le. nspc)
     $                 STORE=WPATH(MSOFFL,KP)*TAIL*PATM(MSOFFL)
                  if (knew .gt. nspc)
     $                 STORE=WPATHx(MSOFFL,KP)*TAIL
                  SDSUM(KTAIL)=SDSUM(KTAIL)+STORE
  120         CONTINUE
              tdepth=sdsum(ktail)
C
C             IF SOLAR PATH, CALCULATE ADDITIONAL LAYER.
C
              IF(IEMSCT.EQ.2 .AND. IPATH.NE.3)THEN
                  MSOFFL=MSOFF+LS
                  J=JJS(MSOFFL,K)
                  STORE=SD(J,KTAIL)
                  TAIL=STORE+FFS(MSOFFL,K)*(SD(J-1,KTAIL)-STORE)
                  if(msoff.eq.0)then
c
c                     L-shaped path
                      if (knew .le. nspc)
     $                    tdepth=tdepth+WPATHS(ls,KP)*TAIL*PATMS(ls,K)
                      if (knew .gt. nspc)
     $                    tdepth=tdepth+WPTHSx(ls,KP)*TAIL

                  else
c
c                     solar path only
                      if (knew .le. nspc)
     $                  tdepth=WPATHS(MSOFFL,KP)*TAIL*PATMS(MSOFFL,K)
                      if (knew .gt. nspc)
     $                  tdepth=WPTHSx(MSOFFL,KP)*TAIL
                  endif
              ENDIF
C
C             CALCULATE LINE TAIL TRANSMITTANCE
C
              if (knew .le. nspc) then
                 TX(KP)=EXP(-tdepth)
                 tx(14)=tx(14)+tdepth
                 IF(TX(KP) .GT. 1.)TX(KP) = 1.
              else
                 TXx(KP)=EXP(-tdepth)
                 IF(TXx(KP) .GT. 1.)TXx(KP) = 1.
              endif
          ELSE
              if (knew .le. nspc) then
                 TX(KP)=1.
              else
                 TXx(KP)=1.
              endif
          ENDIF
  130 CONTINUE
      RETURN
      END
      BLOCK DATA BO3HH0                                                 bhh0 100
C>    BLOCK DATA                                                        bhh0 110
C                                                                       bhh0 120
C                                                                       bhh0 130
C     O3HH0 CONTAINS O3 HARTLEY HUGGINS CROSS SECTIONS FOR 273K         bhh0 140
C               UNITS OF (CM**2/MOL)*1.E-20                             bhh0 150
C                                                                       bhh0 160
C     NOW INCLUDES MOLINA & MOLINA AT 273K WITH THE TEMPERATURE         bhh0 170
C     DEPENDENCE DETERMINED FROM THE 195K HARVARD MEASUREMENTS,         bhh0 180
C     EMPLOYING THE BASS ALGORITHM (CO(1+C1*T+C2*T2); THIS IS           bhh0 190
C     ONLY FOR THE WAVELENGTH RANGE FROM .34 TO .35 MICRONS;            bhh0 200
C     OTHERWISE, THE BASS DATA ALONE HAVE BEEN EMPLOYED BETWEEN         bhh0 210
C     .34 AND .245 MICRONS.                                             bhh0 220
C                                                                       bhh0 230
C     NEW T-DEPENDENT X-SECTIONS BETWEEN .345 AND .36 MICRONS           bhh0 240
C     HAVE NOW BEEN ADDED, BASED ON WORK BY CACCIANI, DISARRA           bhh0 250
C     AND FIOCCO, UNIVERSITY OF ROME, 1987.  QUADRATIC TEMP             bhh0 260
C     HAS BEEN DERIVED, AS ABOVE.                                       bhh0 270
C                                                                       bhh0 280
C     MOLINA & MOLINA HAVE AGAIN BEEN USED BETWEEN .245 AND .185        bhh0 290
C     MICRONS (NO TEMPERATURE DEPENDENCE)                               bhh0 300
C                                                                       bhh0 310
C     AGREEMENT AMONGST THE FOUR DATA SETS IS REASONABLE (<10%)         bhh0 320
C     AND OFTEN EXCELLENT (0-3%)                                        bhh0 330
C                                                                       bhh0 340
C                                                                       bhh0 350
      COMMON /O3HH0/  V1C,V2C,DVC,NC,                                   bhh0 360
     X           O30001(80),O30081(80),O30161(80),O30241(80),O30321(80),bhh0 370
     X           O30401( 7),                                            bhh0 380
     X           C00001(80),C00081(80),C00161(80),C00241(80),C00321(80),bhh0 390
     X           C00401(80),C00481(80),C00561(80),C00641(80),C00721(80),bhh0 400
     X           C00801(80),C00881(80),C00961(80),C01041(80),C01121(80),bhh0 410
     X           C01201(80),C01281(80),C01361(80),C01441(80),C01521(80),bhh0 420
     X           C01601(80),C01681(80),C01761(80),C01841(80),C01921(80),bhh0 430
     X           C02001(80),C02081(80),C02161(80),C02241(40)            bhh0 440
C                                                                       bhh0 450
C     DATA V1C  /27370./,V2C  /29400./,DVC  /5./,NC  /407/ INN & TANAKA bhh0 460
C         DATA FROM INN & TANAKA, HANDBOOK OF GEOPHYSICS, 1957, P 16-24 bhh0 470
C                LINEARLY INTERPOLATED BY SAC, JUNE 1985                bhh0 480
C                CONVERSION: (I&T)/(LOSCHMIDT 1 1987*1.2)               bhh0 490
C                                                                       bhh0 500
C     DATA V1C /29405./, V2C /40800./ ,DVC /5./, NC /2280/  BASS        bhh0 510
C         DATA FROM BASS, JUNE 1985                                     bhh0 520
C                                                                       bhh0 530
      DATA V1C /27370./, V2C /40800./ ,DVC /5./, NC /2687/              bhh0 540
C                                                                       bhh0 550
      DATA O30001/                                                      bhh0 560
C    X 2.08858E-03, 1.98947E-03, 1.89037E-03, 1.79126E-03, 1.69215E-03, bhh0 570
C     THIS LINE OF DATA HAS BEEN REPLACED BY MONTONICALLY DECREASING    bhh0 580
C     VALUES                                                            bhh0 590
     X 1.00000E-03, 1.15000E-03, 1.25000E-03, 1.40000E-03, 1.50000E-03, bhh0 600
     X 1.59304E-03, 1.62396E-03, 1.76216E-03, 1.90036E-03, 2.03856E-03, bhh0 610
     X 2.16538E-03, 2.02324E-03, 1.88110E-03, 1.73896E-03, 1.59682E-03, bhh0 620
     X 1.45468E-03, 1.31253E-03, 1.17039E-03, 1.02825E-03, 8.86108E-04, bhh0 630
     X 7.43963E-04, 6.01821E-04, 4.59679E-04, 5.14820E-04, 5.73044E-04, bhh0 640
     X 6.31269E-04, 6.89493E-04, 7.47718E-04, 8.05942E-04, 8.64167E-04, bhh0 650
     X 9.22392E-04, 9.80617E-04, 1.03884E-03, 1.09707E-03, 1.15528E-03, bhh0 660
     X 1.21351E-03, 1.27173E-03, 1.32996E-03, 1.38818E-03, 1.44641E-03, bhh0 670
     X 1.50463E-03, 1.56286E-03, 1.62108E-03, 1.67931E-03, 1.73753E-03, bhh0 680
     X 1.79575E-03, 1.85398E-03, 1.91220E-03, 1.97043E-03, 2.02865E-03, bhh0 690
     X 2.08688E-03, 2.14510E-03, 2.20333E-03, 2.26155E-03, 2.31978E-03, bhh0 700
     X 2.37800E-03, 2.43623E-03, 2.49444E-03, 2.55267E-03, 2.61089E-03, bhh0 710
     X 2.66912E-03, 2.72734E-03, 2.78557E-03, 2.84379E-03, 2.90202E-03, bhh0 720
     X 2.96024E-03, 3.01847E-03, 3.07669E-03, 3.13491E-03, 3.19313E-03, bhh0 730
     X 3.25136E-03, 3.30958E-03, 3.36781E-03, 3.31660E-03, 3.21583E-03, bhh0 740
     X 3.11505E-03, 3.22165E-03, 3.46058E-03, 3.69953E-03, 3.93846E-03/ bhh0 750
      DATA O30081/                                                      bhh0 760
     X 4.17739E-03, 4.41633E-03, 4.42256E-03, 4.13791E-03, 4.17894E-03, bhh0 770
     X 4.25583E-03, 4.33273E-03, 4.40963E-03, 4.49259E-03, 4.44532E-03, bhh0 780
     X 4.17540E-03, 3.84814E-03, 3.41823E-03, 3.11003E-03, 2.86548E-03, bhh0 790
     X 2.73912E-03, 2.70800E-03, 2.70882E-03, 2.70866E-03, 2.70816E-03, bhh0 800
     X 2.71228E-03, 2.78044E-03, 2.86135E-03, 3.00163E-03, 3.15222E-03, bhh0 810
     X 3.33394E-03, 3.48231E-03, 3.64966E-03, 3.83242E-03, 3.97733E-03, bhh0 820
     X 4.10299E-03, 4.26332E-03, 4.41165E-03, 4.54040E-03, 4.65544E-03, bhh0 830
     X 4.91897E-03, 5.23429E-03, 5.45390E-03, 5.74420E-03, 5.96314E-03, bhh0 840
     X 6.07198E-03, 6.07338E-03, 5.99162E-03, 5.95079E-03, 6.04655E-03, bhh0 850
     X 6.18239E-03, 6.56998E-03, 6.93885E-03, 7.38561E-03, 7.73029E-03, bhh0 860
     X 7.90493E-03, 7.72072E-03, 7.40226E-03, 6.53860E-03, 5.30328E-03, bhh0 870
     X 4.23000E-03, 3.45735E-03, 3.21167E-03, 3.16694E-03, 3.30966E-03, bhh0 880
     X 3.47431E-03, 3.68089E-03, 3.92006E-03, 4.05246E-03, 4.16408E-03, bhh0 890
     X 4.08710E-03, 3.98224E-03, 4.07316E-03, 4.19498E-03, 4.44990E-03, bhh0 900
     X 4.77881E-03, 5.08270E-03, 5.37384E-03, 5.70240E-03, 5.91906E-03, bhh0 910
     X 5.96745E-03, 5.92363E-03, 5.80363E-03, 5.60812E-03, 5.37450E-03/ bhh0 920
      DATA O30161/                                                      bhh0 930
     X 5.16202E-03, 4.98389E-03, 4.95294E-03, 5.04930E-03, 5.17576E-03, bhh0 940
     X 5.26042E-03, 5.22957E-03, 5.32404E-03, 5.39630E-03, 5.53353E-03, bhh0 950
     X 5.68057E-03, 5.78679E-03, 5.83795E-03, 5.93810E-03, 6.09330E-03, bhh0 960
     X 6.40001E-03, 6.69056E-03, 7.04863E-03, 7.41339E-03, 7.87421E-03, bhh0 970
     X 8.35570E-03, 8.97672E-03, 9.58486E-03, 1.01972E-02, 1.08463E-02, bhh0 980
     X 1.14105E-02, 1.18935E-02, 1.22404E-02, 1.25053E-02, 1.28759E-02, bhh0 990
     X 1.32169E-02, 1.37796E-02, 1.46488E-02, 1.57324E-02, 1.68897E-02, bhh01000
     X 1.78560E-02, 1.87101E-02, 1.92197E-02, 1.94106E-02, 1.90711E-02, bhh01010
     X 1.86585E-02, 1.82149E-02, 1.82219E-02, 1.85639E-02, 1.91924E-02, bhh01020
     X 2.01342E-02, 2.12312E-02, 2.26362E-02, 2.39610E-02, 2.55156E-02, bhh01030
     X 2.71338E-02, 2.87904E-02, 3.04268E-02, 3.17055E-02, 3.28248E-02, bhh01040
     X 3.36026E-02, 3.36867E-02, 3.26393E-02, 2.99356E-02, 2.56607E-02, bhh01050
     X 2.11545E-02, 1.79508E-02, 1.59757E-02, 1.49569E-02, 1.46214E-02, bhh01060
     X 1.46214E-02, 1.48217E-02, 1.51379E-02, 1.53816E-02, 1.58087E-02, bhh01070
     X 1.62186E-02, 1.66627E-02, 1.70961E-02, 1.76101E-02, 1.81759E-02, bhh01080
     X 1.86154E-02, 1.88889E-02, 1.89577E-02, 1.89316E-02, 1.88826E-02/ bhh01090
      DATA O30241/                                                      bhh01100
     X 1.90915E-02, 1.95550E-02, 2.02707E-02, 2.11620E-02, 2.21844E-02, bhh01110
     X 2.30920E-02, 2.37270E-02, 2.37422E-02, 2.33578E-02, 2.20358E-02, bhh01120
     X 1.96239E-02, 1.73329E-02, 1.57013E-02, 1.50566E-02, 1.49248E-02, bhh01130
     X 1.52044E-02, 1.57658E-02, 1.63436E-02, 1.68986E-02, 1.74180E-02, bhh01140
     X 1.78192E-02, 1.80677E-02, 1.79927E-02, 1.77900E-02, 1.75599E-02, bhh01150
     X 1.74982E-02, 1.76674E-02, 1.81633E-02, 1.87826E-02, 1.96898E-02, bhh01160
     X 2.06898E-02, 2.17167E-02, 2.28231E-02, 2.40702E-02, 2.55084E-02, bhh01170
     X 2.69701E-02, 2.86915E-02, 3.05796E-02, 3.22328E-02, 3.42637E-02, bhh01180
     X 3.61708E-02, 3.79118E-02, 3.94418E-02, 4.07333E-02, 4.17158E-02, bhh01190
     X 4.17081E-02, 4.01127E-02, 3.65411E-02, 3.25123E-02, 2.98737E-02, bhh01200
     X 2.83616E-02, 2.79907E-02, 2.80571E-02, 2.84778E-02, 2.91698E-02, bhh01210
     X 2.99500E-02, 3.07468E-02, 3.13903E-02, 3.19811E-02, 3.24616E-02, bhh01220
     X 3.26503E-02, 3.26829E-02, 3.27688E-02, 3.36446E-02, 3.55133E-02, bhh01230
     X 3.88447E-02, 4.28854E-02, 4.55381E-02, 4.77161E-02, 4.93567E-02, bhh01240
     X 4.95127E-02, 5.00492E-02, 5.06233E-02, 5.12739E-02, 5.20327E-02, bhh01250
     X 5.29001E-02, 5.38677E-02, 5.49272E-02, 5.60703E-02, 5.72886E-02/ bhh01260
      DATA O30321/                                                      bhh01270
     X 5.85739E-02, 5.99178E-02, 6.13170E-02, 6.28474E-02, 6.46499E-02, bhh01280
     X 6.68672E-02, 6.96421E-02, 7.31174E-02, 7.74361E-02, 8.27413E-02, bhh01290
     X 8.91756E-02, 9.67018E-02, 1.04844E-01, 1.13063E-01, 1.20818E-01, bhh01300
     X 1.27567E-01, 1.32771E-01, 1.35888E-01, 1.36377E-01, 1.33780E-01, bhh01310
     X 1.28385E-01, 1.20887E-01, 1.11978E-01, 1.02354E-01, 9.27108E-02, bhh01320
     X 8.37418E-02, 7.61423E-02, 7.06032E-02, 6.74255E-02, 6.62092E-02, bhh01330
     X 6.64813E-02, 6.77689E-02, 6.95995E-02, 7.15004E-02, 7.29991E-02, bhh01340
     X 7.36229E-02, 7.29641E-02, 7.11015E-02, 6.83345E-02, 6.49638E-02, bhh01350
     X 6.12897E-02, 5.76125E-02, 5.42326E-02, 5.14504E-02, 4.95645E-02, bhh01360
     X 4.87078E-02, 4.87234E-02, 4.94254E-02, 5.06280E-02, 5.21454E-02, bhh01370
     X 5.37919E-02, 5.53818E-02, 5.67293E-02, 5.76709E-02, 5.82319E-02, bhh01380
     X 5.85334E-02, 5.86968E-02, 5.88439E-02, 5.90963E-02, 5.95756E-02, bhh01390
     X 6.04035E-02, 6.17016E-02, 6.35548E-02, 6.59664E-02, 6.89282E-02, bhh01400
     X 7.24326E-02, 7.64718E-02, 8.10380E-02, 8.61236E-02, 9.17211E-02, bhh01410
     X 9.78192E-02, 1.04353E-01, 1.11218E-01, 1.18308E-01, 1.25519E-01, bhh01420
     X 1.32745E-01, 1.39881E-01, 1.46821E-01, 1.53461E-01, 1.59687E-01/ bhh01430
      DATA O30401/                                                      bhh01440
C    X 1.64187E-01, 1.69368E-01, 1.74549E-01, 1.79731E-01, 1.84912E-01, bhh01450
C      1.90094E-01, 1.95275E-01/                                        bhh01460
C   THE VALUE AT 29400. HAS BEEN CHANGED TO PROVIDE A SMOOTH TRANSITION bhh01470
C    X 1.90094E-01, 1.85275E-01/                                        bhh01480
     X 1.65365E-01, 1.70353E-01, 1.74507E-01, 1.77686E-01, 1.79748E-01, bhh01490
     X 1.80549E-01, 1.79948E-01/                                        bhh01500
C                                                                       bhh01510
C                                                                       bhh01520
C    FOLLOWING DATA ARE FROM BASS JUNE 1985                             bhh01530
C                                                                       bhh01540
      DATA C00001 /                                                     bhh01550
     X 1.81094E-01, 1.57760E-01, 1.37336E-01, 1.19475E-01, 1.17191E-01, bhh01560
     X 1.14331E-01, 1.15984E-01, 1.10412E-01, 1.12660E-01, 1.16014E-01, bhh01570
     X 1.15060E-01, 1.12041E-01, 1.11611E-01, 1.00378E-01, 9.54850E-02, bhh01580
     X 9.87528E-02, 9.46153E-02, 9.53093E-02, 9.72653E-02, 9.66468E-02, bhh01590
     X 9.39750E-02, 1.03552E-01, 1.01361E-01, 1.04315E-01, 1.12842E-01, bhh01600
     X 1.02800E-01, 1.09576E-01, 1.05577E-01, 1.17334E-01, 1.25763E-01, bhh01610
     X 1.27597E-01, 1.34267E-01, 1.44799E-01, 1.57366E-01, 1.67369E-01, bhh01620
     X 1.81778E-01, 1.89207E-01, 2.01376E-01, 2.10310E-01, 2.21721E-01, bhh01630
     X 2.43162E-01, 2.55542E-01, 2.75312E-01, 2.88576E-01, 3.02505E-01, bhh01640
     X 3.15141E-01, 3.28908E-01, 3.49000E-01, 3.56620E-01, 3.59852E-01, bhh01650
     X 3.57517E-01, 3.12924E-01, 2.63610E-01, 2.50854E-01, 2.25642E-01, bhh01660
     X 2.15954E-01, 2.12099E-01, 2.13039E-01, 2.12286E-01, 2.17214E-01, bhh01670
     X 2.28784E-01, 2.28276E-01, 2.34677E-01, 2.30730E-01, 2.16107E-01, bhh01680
     X 1.99471E-01, 1.85629E-01, 1.72730E-01, 1.56229E-01, 1.38156E-01, bhh01690
     X 1.37641E-01, 1.33169E-01, 1.32759E-01, 1.30102E-01, 1.35396E-01, bhh01700
     X 1.37976E-01, 1.41571E-01, 1.46448E-01, 1.44508E-01, 1.47612E-01/ bhh01710
      DATA C00081 /                                                     bhh01720
     X 1.47424E-01, 1.48173E-01, 1.52936E-01, 1.58908E-01, 1.58808E-01, bhh01730
     X 1.59860E-01, 1.73936E-01, 1.84109E-01, 1.95143E-01, 2.08267E-01, bhh01740
     X 2.19256E-01, 2.31653E-01, 2.46400E-01, 2.60437E-01, 2.70792E-01, bhh01750
     X 2.79749E-01, 2.91068E-01, 2.98080E-01, 3.10421E-01, 3.24540E-01, bhh01760
     X 3.39003E-01, 3.58322E-01, 3.81520E-01, 4.02798E-01, 4.35972E-01, bhh01770
     X 4.56220E-01, 4.79037E-01, 5.02597E-01, 5.24648E-01, 5.33964E-01, bhh01780
     X 5.39211E-01, 5.43613E-01, 5.28793E-01, 4.94103E-01, 4.34481E-01, bhh01790
     X 3.76792E-01, 3.37161E-01, 3.15750E-01, 3.11042E-01, 3.08745E-01, bhh01800
     X 3.09195E-01, 3.05859E-01, 3.01443E-01, 2.88111E-01, 2.81303E-01, bhh01810
     X 2.75329E-01, 2.60812E-01, 2.59337E-01, 2.45576E-01, 2.40470E-01, bhh01820
     X 2.39705E-01, 2.45389E-01, 2.49801E-01, 2.53235E-01, 2.54387E-01, bhh01830
     X 2.64311E-01, 2.74146E-01, 2.89737E-01, 2.96673E-01, 3.07337E-01, bhh01840
     X 3.24380E-01, 3.42266E-01, 3.59522E-01, 3.78005E-01, 3.97178E-01, bhh01850
     X 4.23351E-01, 4.45925E-01, 4.63029E-01, 4.94843E-01, 5.19418E-01, bhh01860
     X 5.49928E-01, 5.69115E-01, 6.02396E-01, 6.43471E-01, 6.76401E-01, bhh01870
     X 7.14024E-01, 7.42425E-01, 7.60916E-01, 7.83319E-01, 7.98299E-01/ bhh01880
      DATA C00161 /                                                     bhh01890
     X 7.76672E-01, 7.22769E-01, 6.45967E-01, 5.80850E-01, 5.76514E-01, bhh01900
     X 5.79380E-01, 5.90359E-01, 6.21721E-01, 6.37540E-01, 6.52572E-01, bhh01910
     X 6.63442E-01, 6.69026E-01, 6.69038E-01, 6.53319E-01, 6.21950E-01, bhh01920
     X 5.47619E-01, 4.58994E-01, 4.14888E-01, 3.97736E-01, 3.88775E-01, bhh01930
     X 3.87424E-01, 3.93567E-01, 4.03442E-01, 4.05217E-01, 4.12848E-01, bhh01940
     X 4.12246E-01, 4.16620E-01, 4.13195E-01, 4.08467E-01, 4.13104E-01, bhh01950
     X 4.24498E-01, 4.32002E-01, 4.46361E-01, 4.61131E-01, 4.77228E-01, bhh01960
     X 4.96519E-01, 5.16764E-01, 5.38966E-01, 5.54187E-01, 5.73748E-01, bhh01970
     X 6.07260E-01, 6.34358E-01, 6.60286E-01, 6.95533E-01, 7.37090E-01, bhh01980
     X 7.83894E-01, 8.19557E-01, 8.49244E-01, 8.91832E-01, 9.44885E-01, bhh01990
     X 9.86271E-01, 1.02262E+00, 1.07242E+00, 1.12162E+00, 1.18287E+00, bhh02000
     X 1.22402E+00, 1.24978E+00, 1.24392E+00, 1.19668E+00, 1.11562E+00, bhh02010
     X 1.03983E+00, 9.31884E-01, 8.35307E-01, 7.92620E-01, 7.81980E-01, bhh02020
     X 7.89623E-01, 8.05987E-01, 8.27344E-01, 8.57514E-01, 8.66302E-01, bhh02030
     X 8.72092E-01, 8.66840E-01, 8.40536E-01, 7.87360E-01, 7.35743E-01, bhh02040
     X 6.92039E-01, 6.64032E-01, 6.48360E-01, 6.46288E-01, 6.49505E-01/ bhh02050
      DATA C00241 /                                                     bhh02060
     X 6.69937E-01, 6.81006E-01, 7.00969E-01, 7.19834E-01, 7.26964E-01, bhh02070
     X 7.50591E-01, 7.73600E-01, 8.00673E-01, 8.20347E-01, 8.37855E-01, bhh02080
     X 8.66780E-01, 9.04297E-01, 9.46300E-01, 9.69134E-01, 9.97928E-01, bhh02090
     X 1.06388E+00, 1.11032E+00, 1.15221E+00, 1.21324E+00, 1.24462E+00, bhh02100
     X 1.31978E+00, 1.35617E+00, 1.38792E+00, 1.39196E+00, 1.35161E+00, bhh02110
     X 1.29381E+00, 1.30295E+00, 1.32965E+00, 1.37024E+00, 1.44064E+00, bhh02120
     X 1.50484E+00, 1.57200E+00, 1.62097E+00, 1.67874E+00, 1.72676E+00, bhh02130
     X 1.73383E+00, 1.66091E+00, 1.54936E+00, 1.35454E+00, 1.20070E+00, bhh02140
     X 1.14609E+00, 1.13642E+00, 1.13784E+00, 1.14609E+00, 1.14531E+00, bhh02150
     X 1.16024E+00, 1.16891E+00, 1.16111E+00, 1.14192E+00, 1.09903E+00, bhh02160
     X 1.05745E+00, 1.02341E+00, 1.00121E+00, 1.00036E+00, 1.00576E+00, bhh02170
     X 1.02405E+00, 1.04379E+00, 1.07623E+00, 1.11347E+00, 1.17305E+00, bhh02180
     X 1.20016E+00, 1.22697E+00, 1.27479E+00, 1.32572E+00, 1.38690E+00, bhh02190
     X 1.43768E+00, 1.48379E+00, 1.55317E+00, 1.64020E+00, 1.71268E+00, bhh02200
     X 1.77183E+00, 1.85824E+00, 1.95131E+00, 2.04609E+00, 2.13151E+00, bhh02210
     X 2.17777E+00, 2.22832E+00, 2.26886E+00, 2.19775E+00, 2.05087E+00/ bhh02220
      DATA C00321 /                                                     bhh02230
     X 1.96103E+00, 1.95554E+00, 1.98037E+00, 2.05440E+00, 2.11629E+00, bhh02240
     X 2.17893E+00, 2.24384E+00, 2.30464E+00, 2.32525E+00, 2.29945E+00, bhh02250
     X 2.21712E+00, 2.03430E+00, 1.82139E+00, 1.70354E+00, 1.64631E+00, bhh02260
     X 1.62164E+00, 1.61356E+00, 1.63900E+00, 1.66313E+00, 1.67409E+00, bhh02270
     X 1.69143E+00, 1.70181E+00, 1.69165E+00, 1.67699E+00, 1.67879E+00, bhh02280
     X 1.67312E+00, 1.68133E+00, 1.70002E+00, 1.72500E+00, 1.76308E+00, bhh02290
     X 1.80634E+00, 1.87548E+00, 1.94924E+00, 1.99812E+00, 2.05333E+00, bhh02300
     X 2.14035E+00, 2.21847E+00, 2.27412E+00, 2.29752E+00, 2.30750E+00, bhh02310
     X 2.36165E+00, 2.44394E+00, 2.52782E+00, 2.61343E+00, 2.71640E+00, bhh02320
     X 2.81613E+00, 2.93679E+00, 3.01577E+00, 3.15995E+00, 3.15931E+00, bhh02330
     X 2.96658E+00, 2.73295E+00, 2.67480E+00, 2.66652E+00, 2.69393E+00, bhh02340
     X 2.75102E+00, 2.86503E+00, 2.99163E+00, 2.99576E+00, 3.02603E+00, bhh02350
     X 2.98415E+00, 2.79309E+00, 2.65337E+00, 2.50962E+00, 2.43207E+00, bhh02360
     X 2.34812E+00, 2.34872E+00, 2.35186E+00, 2.39477E+00, 2.42629E+00, bhh02370
     X 2.48068E+00, 2.55087E+00, 2.55952E+00, 2.56497E+00, 2.64323E+00, bhh02380
     X 2.67961E+00, 2.66263E+00, 2.70243E+00, 2.74911E+00, 2.81786E+00/ bhh02390
      DATA C00401 /                                                     bhh02400
     X 2.88684E+00, 2.97790E+00, 3.04305E+00, 3.13053E+00, 3.23857E+00, bhh02410
     X 3.35582E+00, 3.40654E+00, 3.38117E+00, 3.36296E+00, 3.39480E+00, bhh02420
     X 3.49066E+00, 3.60832E+00, 3.71817E+00, 3.83924E+00, 3.96355E+00, bhh02430
     X 4.03656E+00, 4.00518E+00, 3.90389E+00, 3.74790E+00, 3.61385E+00, bhh02440
     X 3.57066E+00, 3.59438E+00, 3.66182E+00, 3.71176E+00, 3.75255E+00, bhh02450
     X 3.79101E+00, 3.85278E+00, 3.85027E+00, 3.81112E+00, 3.72553E+00, bhh02460
     X 3.61017E+00, 3.54384E+00, 3.52406E+00, 3.54097E+00, 3.59375E+00, bhh02470
     X 3.66312E+00, 3.72632E+00, 3.76825E+00, 3.86798E+00, 3.92916E+00, bhh02480
     X 3.95610E+00, 4.00120E+00, 4.05865E+00, 4.11981E+00, 4.14634E+00, bhh02490
     X 4.19109E+00, 4.20317E+00, 4.25754E+00, 4.35131E+00, 4.48573E+00, bhh02500
     X 4.58716E+00, 4.67462E+00, 4.78228E+00, 4.91196E+00, 5.01871E+00, bhh02510
     X 5.10663E+00, 5.17780E+00, 5.21393E+00, 5.18144E+00, 5.04379E+00, bhh02520
     X 4.86504E+00, 4.78569E+00, 4.72717E+00, 4.69132E+00, 4.65797E+00, bhh02530
     X 4.60305E+00, 4.59798E+00, 4.65300E+00, 4.69707E+00, 4.74790E+00, bhh02540
     X 4.82581E+00, 4.80953E+00, 4.80517E+00, 4.82685E+00, 4.82321E+00, bhh02550
     X 4.84806E+00, 4.88591E+00, 4.91759E+00, 4.98074E+00, 5.07071E+00/ bhh02560
      DATA C00481 /                                                     bhh02570
     X 5.18733E+00, 5.30567E+00, 5.38670E+00, 5.43942E+00, 5.51797E+00, bhh02580
     X 5.62652E+00, 5.71228E+00, 5.82347E+00, 5.91434E+00, 6.00171E+00, bhh02590
     X 6.06977E+00, 6.13040E+00, 6.21990E+00, 6.29980E+00, 6.37206E+00, bhh02600
     X 6.48233E+00, 6.53068E+00, 6.53275E+00, 6.56858E+00, 6.54577E+00, bhh02610
     X 6.50472E+00, 6.41504E+00, 6.33853E+00, 6.31184E+00, 6.21253E+00, bhh02620
     X 6.22034E+00, 6.26918E+00, 6.28982E+00, 6.29461E+00, 6.35418E+00, bhh02630
     X 6.40956E+00, 6.38020E+00, 6.39784E+00, 6.45383E+00, 6.50134E+00, bhh02640
     X 6.56808E+00, 6.58850E+00, 6.58882E+00, 6.65097E+00, 6.75259E+00, bhh02650
     X 6.83256E+00, 6.92593E+00, 6.98083E+00, 7.03632E+00, 7.11147E+00, bhh02660
     X 7.15622E+00, 7.21106E+00, 7.27319E+00, 7.33382E+00, 7.38601E+00, bhh02670
     X 7.48971E+00, 7.61459E+00, 7.70134E+00, 7.76194E+00, 7.85534E+00, bhh02680
     X 7.99519E+00, 8.12227E+00, 8.25461E+00, 8.34670E+00, 8.42733E+00, bhh02690
     X 8.51806E+00, 8.57638E+00, 8.56481E+00, 8.55461E+00, 8.55593E+00, bhh02700
     X 8.58756E+00, 8.50070E+00, 8.54400E+00, 8.57575E+00, 8.62083E+00, bhh02710
     X 8.60684E+00, 8.67824E+00, 8.72069E+00, 8.79127E+00, 8.85479E+00, bhh02720
     X 8.86770E+00, 8.90574E+00, 8.91531E+00, 8.94800E+00, 9.00167E+00/ bhh02730
      DATA C00561 /                                                     bhh02740
     X 9.14051E+00, 9.25421E+00, 9.39694E+00, 9.50896E+00, 9.53190E+00, bhh02750
     X 9.55977E+00, 9.53482E+00, 9.49662E+00, 9.53359E+00, 9.54007E+00, bhh02760
     X 9.49809E+00, 9.49373E+00, 9.53282E+00, 9.63757E+00, 9.67855E+00, bhh02770
     X 9.67633E+00, 9.67045E+00, 9.79481E+00, 9.93420E+00, 1.00234E+01, bhh02780
     X 1.01372E+01, 1.02577E+01, 1.05056E+01, 1.07873E+01, 1.09967E+01, bhh02790
     X 1.10873E+01, 1.11624E+01, 1.13006E+01, 1.14875E+01, 1.16106E+01, bhh02800
     X 1.16744E+01, 1.17582E+01, 1.17709E+01, 1.18537E+01, 1.19623E+01, bhh02810
     X 1.19763E+01, 1.19879E+01, 1.20384E+01, 1.20763E+01, 1.20826E+01, bhh02820
     X 1.20449E+01, 1.19747E+01, 1.20227E+01, 1.21805E+01, 1.23134E+01, bhh02830
     X 1.24042E+01, 1.25614E+01, 1.26828E+01, 1.26645E+01, 1.26963E+01, bhh02840
     X 1.28226E+01, 1.28720E+01, 1.28981E+01, 1.29462E+01, 1.29363E+01, bhh02850
     X 1.29199E+01, 1.29797E+01, 1.28860E+01, 1.29126E+01, 1.30205E+01, bhh02860
     X 1.31327E+01, 1.31722E+01, 1.31901E+01, 1.33189E+01, 1.34833E+01, bhh02870
     X 1.36228E+01, 1.37474E+01, 1.38548E+01, 1.39450E+01, 1.40926E+01, bhh02880
     X 1.43099E+01, 1.44836E+01, 1.46257E+01, 1.47755E+01, 1.49163E+01, bhh02890
     X 1.51038E+01, 1.53308E+01, 1.54194E+01, 1.54852E+01, 1.55968E+01/ bhh02900
      DATA C00641 /                                                     bhh02910
     X 1.57025E+01, 1.58667E+01, 1.60365E+01, 1.61427E+01, 1.62967E+01, bhh02920
     X 1.64735E+01, 1.66123E+01, 1.67268E+01, 1.67673E+01, 1.67825E+01, bhh02930
     X 1.68898E+01, 1.68178E+01, 1.68216E+01, 1.68574E+01, 1.68799E+01, bhh02940
     X 1.70317E+01, 1.70767E+01, 1.71508E+01, 1.72965E+01, 1.73421E+01, bhh02950
     X 1.73937E+01, 1.74420E+01, 1.74535E+01, 1.75110E+01, 1.75497E+01, bhh02960
     X 1.75149E+01, 1.75955E+01, 1.78260E+01, 1.78271E+01, 1.79750E+01, bhh02970
     X 1.80600E+01, 1.81597E+01, 1.83454E+01, 1.85243E+01, 1.87382E+01, bhh02980
     X 1.88904E+01, 1.90395E+01, 1.92759E+01, 1.95398E+01, 1.97712E+01, bhh02990
     X 1.98487E+01, 1.99522E+01, 2.02363E+01, 2.03271E+01, 2.07090E+01, bhh03000
     X 2.09195E+01, 2.10974E+01, 2.11702E+01, 2.12964E+01, 2.14339E+01, bhh03010
     X 2.15764E+01, 2.17351E+01, 2.18486E+01, 2.19700E+01, 2.21663E+01, bhh03020
     X 2.24244E+01, 2.24813E+01, 2.25248E+01, 2.26357E+01, 2.26457E+01, bhh03030
     X 2.27249E+01, 2.27172E+01, 2.27123E+01, 2.26859E+01, 2.27216E+01, bhh03040
     X 2.29306E+01, 2.30711E+01, 2.31374E+01, 2.31815E+01, 2.33423E+01, bhh03050
     X 2.33810E+01, 2.36430E+01, 2.36807E+01, 2.36676E+01, 2.38607E+01, bhh03060
     X 2.41559E+01, 2.43413E+01, 2.44401E+01, 2.45968E+01, 2.47927E+01/ bhh03070
      DATA C00721 /                                                     bhh03080
     X 2.50743E+01, 2.53667E+01, 2.55749E+01, 2.57357E+01, 2.58927E+01, bhh03090
     X 2.61523E+01, 2.64110E+01, 2.66650E+01, 2.68829E+01, 2.70635E+01, bhh03100
     X 2.72797E+01, 2.75064E+01, 2.77229E+01, 2.80341E+01, 2.82003E+01, bhh03110
     X 2.83346E+01, 2.83909E+01, 2.86212E+01, 2.88006E+01, 2.89577E+01, bhh03120
     X 2.90965E+01, 2.91834E+01, 2.93224E+01, 2.94094E+01, 2.94848E+01, bhh03130
     X 2.96584E+01, 2.96749E+01, 2.97760E+01, 2.99163E+01, 3.00238E+01, bhh03140
     X 3.01290E+01, 3.02307E+01, 3.03663E+01, 3.05897E+01, 3.07937E+01, bhh03150
     X 3.10403E+01, 3.11778E+01, 3.13271E+01, 3.15799E+01, 3.18435E+01, bhh03160
     X 3.21614E+01, 3.25097E+01, 3.27701E+01, 3.29600E+01, 3.32583E+01, bhh03170
     X 3.36348E+01, 3.40282E+01, 3.41751E+01, 3.44128E+01, 3.46199E+01, bhh03180
     X 3.49363E+01, 3.52087E+01, 3.54056E+01, 3.55596E+01, 3.56694E+01, bhh03190
     X 3.58104E+01, 3.60276E+01, 3.62818E+01, 3.63505E+01, 3.66069E+01, bhh03200
     X 3.67544E+01, 3.70664E+01, 3.72525E+01, 3.73491E+01, 3.76006E+01, bhh03210
     X 3.77102E+01, 3.78970E+01, 3.81254E+01, 3.82728E+01, 3.81720E+01, bhh03220
     X 3.82781E+01, 3.84982E+01, 3.87202E+01, 3.89958E+01, 3.94148E+01, bhh03230
     X 3.98434E+01, 3.98952E+01, 4.01573E+01, 4.06014E+01, 4.09651E+01/ bhh03240
      DATA C00801 /                                                     bhh03250
     X 4.12821E+01, 4.16849E+01, 4.19899E+01, 4.22719E+01, 4.27736E+01, bhh03260
     X 4.32254E+01, 4.33883E+01, 4.39831E+01, 4.39414E+01, 4.42613E+01, bhh03270
     X 4.46503E+01, 4.49027E+01, 4.50384E+01, 4.52929E+01, 4.57269E+01, bhh03280
     X 4.56433E+01, 4.57350E+01, 4.60128E+01, 4.60487E+01, 4.61183E+01, bhh03290
     X 4.64397E+01, 4.68211E+01, 4.70706E+01, 4.72821E+01, 4.74972E+01, bhh03300
     X 4.78253E+01, 4.81615E+01, 4.84480E+01, 4.85703E+01, 4.87397E+01, bhh03310
     X 4.90015E+01, 4.93673E+01, 4.97291E+01, 4.99836E+01, 5.02975E+01, bhh03320
     X 5.05572E+01, 5.08226E+01, 5.13433E+01, 5.17112E+01, 5.19703E+01, bhh03330
     X 5.23128E+01, 5.27305E+01, 5.30599E+01, 5.34555E+01, 5.39625E+01, bhh03340
     X 5.43627E+01, 5.45446E+01, 5.49263E+01, 5.53511E+01, 5.57270E+01, bhh03350
     X 5.60904E+01, 5.63875E+01, 5.68475E+01, 5.73172E+01, 5.81134E+01, bhh03360
     X 5.86399E+01, 5.90384E+01, 5.91417E+01, 5.90883E+01, 5.93610E+01, bhh03370
     X 5.95794E+01, 5.99600E+01, 5.98493E+01, 5.99441E+01, 6.02748E+01, bhh03380
     X 6.04778E+01, 6.05233E+01, 6.07194E+01, 6.11589E+01, 6.13324E+01, bhh03390
     X 6.17685E+01, 6.23166E+01, 6.31055E+01, 6.38211E+01, 6.42320E+01, bhh03400
     X 6.45195E+01, 6.51125E+01, 6.56765E+01, 6.59286E+01, 6.62716E+01/ bhh03410
      DATA C00881 /                                                     bhh03420
     X 6.65693E+01, 6.68906E+01, 6.72246E+01, 6.75177E+01, 6.78476E+01, bhh03430
     X 6.82599E+01, 6.84400E+01, 6.89072E+01, 6.95720E+01, 7.01410E+01, bhh03440
     X 7.05519E+01, 7.09367E+01, 7.13975E+01, 7.22128E+01, 7.28222E+01, bhh03450
     X 7.33808E+01, 7.38828E+01, 7.44496E+01, 7.49983E+01, 7.54178E+01, bhh03460
     X 7.60554E+01, 7.62484E+01, 7.67892E+01, 7.71262E+01, 7.76235E+01, bhh03470
     X 7.81413E+01, 7.85694E+01, 7.91248E+01, 7.94715E+01, 7.96200E+01, bhh03480
     X 8.00270E+01, 8.03783E+01, 8.07100E+01, 8.11929E+01, 8.17375E+01, bhh03490
     X 8.18410E+01, 8.23341E+01, 8.26754E+01, 8.30893E+01, 8.34232E+01, bhh03500
     X 8.35533E+01, 8.36017E+01, 8.38589E+01, 8.43366E+01, 8.47593E+01, bhh03510
     X 8.51614E+01, 8.55271E+01, 8.58979E+01, 8.64892E+01, 8.74367E+01, bhh03520
     X 8.82440E+01, 8.89105E+01, 8.90980E+01, 8.97266E+01, 9.04886E+01, bhh03530
     X 9.12709E+01, 9.21243E+01, 9.26673E+01, 9.31331E+01, 9.38190E+01, bhh03540
     X 9.44877E+01, 9.50636E+01, 9.57445E+01, 9.65211E+01, 9.68623E+01, bhh03550
     X 9.75356E+01, 9.81991E+01, 9.88881E+01, 9.94554E+01, 9.99292E+01, bhh03560
     X 1.00357E+02, 1.00670E+02, 1.01227E+02, 1.01529E+02, 1.01889E+02, bhh03570
     X 1.02033E+02, 1.02254E+02, 1.02731E+02, 1.02914E+02, 1.03120E+02/ bhh03580
      DATA C00961 /                                                     bhh03590
     X 1.03674E+02, 1.03768E+02, 1.04146E+02, 1.04850E+02, 1.05525E+02, bhh03600
     X 1.06263E+02, 1.06653E+02, 1.07084E+02, 1.07461E+02, 1.08052E+02, bhh03610
     X 1.08793E+02, 1.09395E+02, 1.09811E+02, 1.10079E+02, 1.10656E+02, bhh03620
     X 1.11575E+02, 1.12544E+02, 1.13453E+02, 1.14440E+02, 1.15292E+02, bhh03630
     X 1.15869E+02, 1.16925E+02, 1.17854E+02, 1.18723E+02, 1.19574E+02, bhh03640
     X 1.19940E+02, 1.21108E+02, 1.21807E+02, 1.22490E+02, 1.23278E+02, bhh03650
     X 1.24094E+02, 1.24816E+02, 1.25469E+02, 1.26217E+02, 1.26878E+02, bhh03660
     X 1.27536E+02, 1.28168E+02, 1.28682E+02, 1.29076E+02, 1.30171E+02, bhh03670
     X 1.30667E+02, 1.31242E+02, 1.31665E+02, 1.31961E+02, 1.32347E+02, bhh03680
     X 1.32805E+02, 1.33152E+02, 1.33869E+02, 1.34261E+02, 1.34498E+02, bhh03690
     X 1.35028E+02, 1.36049E+02, 1.36577E+02, 1.37491E+02, 1.38078E+02, bhh03700
     X 1.38389E+02, 1.38819E+02, 1.39653E+02, 1.39770E+02, 1.40812E+02, bhh03710
     X 1.40926E+02, 1.41267E+02, 1.41872E+02, 1.42233E+02, 1.43447E+02, bhh03720
     X 1.44641E+02, 1.45500E+02, 1.45996E+02, 1.47040E+02, 1.48767E+02, bhh03730
     X 1.48785E+02, 1.49525E+02, 1.50266E+02, 1.50814E+02, 1.51443E+02, bhh03740
     X 1.52272E+02, 1.52846E+02, 1.54000E+02, 1.54629E+02, 1.54907E+02/ bhh03750
      DATA C01041 /                                                     bhh03760
     X 1.55527E+02, 1.56642E+02, 1.57436E+02, 1.59036E+02, 1.59336E+02, bhh03770
     X 1.59661E+02, 1.60287E+02, 1.61202E+02, 1.62410E+02, 1.63040E+02, bhh03780
     X 1.62872E+02, 1.63248E+02, 1.63776E+02, 1.64313E+02, 1.65782E+02, bhh03790
     X 1.65692E+02, 1.66049E+02, 1.66701E+02, 1.67786E+02, 1.69150E+02, bhh03800
     X 1.69996E+02, 1.71634E+02, 1.71137E+02, 1.71372E+02, 1.72525E+02, bhh03810
     X 1.73816E+02, 1.75219E+02, 1.76091E+02, 1.78260E+02, 1.79299E+02, bhh03820
     X 1.79904E+02, 1.81718E+02, 1.83807E+02, 1.85488E+02, 1.85929E+02, bhh03830
     X 1.86787E+02, 1.88282E+02, 1.89546E+02, 1.91489E+02, 1.92646E+02, bhh03840
     X 1.93399E+02, 1.93838E+02, 1.94406E+02, 1.95829E+02, 1.96745E+02, bhh03850
     X 1.96978E+02, 1.97243E+02, 1.97636E+02, 1.98025E+02, 1.98227E+02, bhh03860
     X 1.99552E+02, 2.00304E+02, 2.01031E+02, 2.01788E+02, 2.02432E+02, bhh03870
     X 2.03817E+02, 2.04866E+02, 2.05561E+02, 2.06180E+02, 2.07024E+02, bhh03880
     X 2.08303E+02, 2.09426E+02, 2.10575E+02, 2.11637E+02, 2.12559E+02, bhh03890
     X 2.13361E+02, 2.14191E+02, 2.15264E+02, 2.16366E+02, 2.17316E+02, bhh03900
     X 2.17717E+02, 2.17154E+02, 2.19172E+02, 2.20346E+02, 2.20849E+02, bhh03910
     X 2.21539E+02, 2.22810E+02, 2.22740E+02, 2.22824E+02, 2.23285E+02/ bhh03920
      DATA C01121 /                                                     bhh03930
     X 2.23696E+02, 2.23864E+02, 2.23968E+02, 2.23544E+02, 2.24804E+02, bhh03940
     X 2.25953E+02, 2.26753E+02, 2.27732E+02, 2.29505E+02, 2.30108E+02, bhh03950
     X 2.31232E+02, 2.32552E+02, 2.33979E+02, 2.36677E+02, 2.38481E+02, bhh03960
     X 2.41797E+02, 2.44025E+02, 2.45113E+02, 2.47373E+02, 2.47258E+02, bhh03970
     X 2.48617E+02, 2.49790E+02, 2.50562E+02, 2.51198E+02, 2.51289E+02, bhh03980
     X 2.52509E+02, 2.54136E+02, 2.55335E+02, 2.55808E+02, 2.56567E+02, bhh03990
     X 2.57977E+02, 2.58987E+02, 2.59622E+02, 2.60170E+02, 2.61127E+02, bhh04000
     X 2.60655E+02, 2.62129E+02, 2.64020E+02, 2.65659E+02, 2.67086E+02, bhh04010
     X 2.67615E+02, 2.69800E+02, 2.71452E+02, 2.73314E+02, 2.76972E+02, bhh04020
     X 2.78005E+02, 2.79815E+02, 2.81709E+02, 2.84043E+02, 2.87070E+02, bhh04030
     X 2.88842E+02, 2.90555E+02, 2.92401E+02, 2.94314E+02, 2.96074E+02, bhh04040
     X 2.97103E+02, 2.98037E+02, 2.98113E+02, 2.97705E+02, 2.97350E+02, bhh04050
     X 2.97329E+02, 2.97016E+02, 2.96752E+02, 2.96599E+02, 2.96637E+02, bhh04060
     X 2.97057E+02, 2.97585E+02, 2.98179E+02, 2.98997E+02, 3.00012E+02, bhh04070
     X 3.00806E+02, 3.00908E+02, 3.02369E+02, 3.04063E+02, 3.05325E+02, bhh04080
     X 3.06737E+02, 3.08066E+02, 3.09694E+02, 3.11530E+02, 3.13132E+02/ bhh04090
      DATA C01201 /                                                     bhh04100
     X 3.13296E+02, 3.15513E+02, 3.16887E+02, 3.17682E+02, 3.18296E+02, bhh04110
     X 3.18654E+02, 3.18912E+02, 3.19236E+02, 3.19626E+02, 3.20020E+02, bhh04120
     X 3.20186E+02, 3.20709E+02, 3.21628E+02, 3.22625E+02, 3.23504E+02, bhh04130
     X 3.25479E+02, 3.26825E+02, 3.28146E+02, 3.29404E+02, 3.30512E+02, bhh04140
     X 3.32634E+02, 3.34422E+02, 3.35602E+02, 3.36833E+02, 3.39372E+02, bhh04150
     X 3.43446E+02, 3.46374E+02, 3.48719E+02, 3.50881E+02, 3.53160E+02, bhh04160
     X 3.54890E+02, 3.57162E+02, 3.59284E+02, 3.60876E+02, 3.62295E+02, bhh04170
     X 3.63987E+02, 3.64835E+02, 3.65257E+02, 3.65738E+02, 3.65904E+02, bhh04180
     X 3.65976E+02, 3.66460E+02, 3.67087E+02, 3.67377E+02, 3.69079E+02, bhh04190
     X 3.70694E+02, 3.70940E+02, 3.70557E+02, 3.72693E+02, 3.73852E+02, bhh04200
     X 3.75679E+02, 3.77863E+02, 3.79964E+02, 3.81368E+02, 3.82716E+02, bhh04210
     X 3.85556E+02, 3.89072E+02, 3.91796E+02, 3.92766E+02, 3.96551E+02, bhh04220
     X 3.97833E+02, 3.97285E+02, 4.01929E+02, 4.02158E+02, 4.04553E+02, bhh04230
     X 4.06451E+02, 4.06236E+02, 4.08135E+02, 4.07797E+02, 4.08415E+02, bhh04240
     X 4.10111E+02, 4.11781E+02, 4.12735E+02, 4.11547E+02, 4.11606E+02, bhh04250
     X 4.13548E+02, 4.12557E+02, 4.12923E+02, 4.12866E+02, 4.13009E+02/ bhh04260
      DATA C01281 /                                                     bhh04270
     X 4.14447E+02, 4.16032E+02, 4.17032E+02, 4.19064E+02, 4.22458E+02, bhh04280
     X 4.26021E+02, 4.25192E+02, 4.25684E+02, 4.27536E+02, 4.29972E+02, bhh04290
     X 4.31994E+02, 4.36037E+02, 4.39132E+02, 4.40363E+02, 4.40716E+02, bhh04300
     X 4.40342E+02, 4.42063E+02, 4.44408E+02, 4.45454E+02, 4.47835E+02, bhh04310
     X 4.48256E+02, 4.48831E+02, 4.50257E+02, 4.51427E+02, 4.52373E+02, bhh04320
     X 4.53899E+02, 4.55496E+02, 4.56311E+02, 4.57314E+02, 4.59922E+02, bhh04330
     X 4.61048E+02, 4.59840E+02, 4.62144E+02, 4.63152E+02, 4.64565E+02, bhh04340
     X 4.66715E+02, 4.69380E+02, 4.70751E+02, 4.72012E+02, 4.73482E+02, bhh04350
     X 4.75524E+02, 4.79307E+02, 4.82035E+02, 4.84423E+02, 4.86712E+02, bhh04360
     X 4.88754E+02, 4.90102E+02, 4.92047E+02, 4.94150E+02, 4.95375E+02, bhh04370
     X 4.95828E+02, 4.97555E+02, 4.98559E+02, 4.97618E+02, 4.99265E+02, bhh04380
     X 4.99979E+02, 5.00681E+02, 5.01386E+02, 5.00868E+02, 5.01935E+02, bhh04390
     X 5.03151E+02, 5.04329E+02, 5.05546E+02, 5.08259E+02, 5.09222E+02, bhh04400
     X 5.09818E+02, 5.11397E+02, 5.12391E+02, 5.13326E+02, 5.14329E+02, bhh04410
     X 5.15443E+02, 5.16533E+02, 5.21417E+02, 5.25071E+02, 5.26581E+02, bhh04420
     X 5.27762E+02, 5.29274E+02, 5.31704E+02, 5.34310E+02, 5.35727E+02/ bhh04430
      DATA C01361 /                                                     bhh04440
     X 5.36838E+02, 5.37082E+02, 5.36733E+02, 5.36170E+02, 5.36063E+02, bhh04450
     X 5.36451E+02, 5.37870E+02, 5.40475E+02, 5.42268E+02, 5.41972E+02, bhh04460
     X 5.42532E+02, 5.44764E+02, 5.46844E+02, 5.47525E+02, 5.49150E+02, bhh04470
     X 5.52049E+02, 5.55423E+02, 5.56259E+02, 5.57424E+02, 5.59189E+02, bhh04480
     X 5.61167E+02, 5.64512E+02, 5.66753E+02, 5.68183E+02, 5.69628E+02, bhh04490
     X 5.73474E+02, 5.76192E+02, 5.78058E+02, 5.79588E+02, 5.81619E+02, bhh04500
     X 5.83530E+02, 5.84852E+02, 5.85326E+02, 5.88130E+02, 5.90570E+02, bhh04510
     X 5.91785E+02, 5.91371E+02, 5.90931E+02, 5.90942E+02, 5.91168E+02, bhh04520
     X 5.91291E+02, 5.89791E+02, 5.91146E+02, 5.90804E+02, 5.87847E+02, bhh04530
     X 5.89067E+02, 5.91027E+02, 5.90951E+02, 5.89227E+02, 5.93389E+02, bhh04540
     X 5.92921E+02, 5.92739E+02, 5.94544E+02, 5.98941E+02, 6.02302E+02, bhh04550
     X 6.03908E+02, 6.04265E+02, 6.06737E+02, 6.08560E+02, 6.11272E+02, bhh04560
     X 6.14992E+02, 6.18595E+02, 6.20930E+02, 6.22107E+02, 6.22957E+02, bhh04570
     X 6.26710E+02, 6.28657E+02, 6.30132E+02, 6.31543E+02, 6.33043E+02, bhh04580
     X 6.36932E+02, 6.38248E+02, 6.37126E+02, 6.41648E+02, 6.48274E+02, bhh04590
     X 6.52638E+02, 6.53922E+02, 6.56647E+02, 6.59351E+02, 6.60525E+02/ bhh04600
      DATA C01441 /                                                     bhh04610
     X 6.60130E+02, 6.61375E+02, 6.62660E+02, 6.63976E+02, 6.65181E+02, bhh04620
     X 6.64820E+02, 6.64458E+02, 6.64927E+02, 6.66555E+02, 6.66759E+02, bhh04630
     X 6.68218E+02, 6.70323E+02, 6.72703E+02, 6.76085E+02, 6.79180E+02, bhh04640
     X 6.80850E+02, 6.80017E+02, 6.79928E+02, 6.80886E+02, 6.82038E+02, bhh04650
     X 6.82271E+02, 6.84057E+02, 6.85309E+02, 6.86816E+02, 6.90180E+02, bhh04660
     X 6.93205E+02, 6.95870E+02, 6.98794E+02, 7.03776E+02, 7.04010E+02, bhh04670
     X 7.05041E+02, 7.07254E+02, 7.07432E+02, 7.10736E+02, 7.13791E+02, bhh04680
     X 7.15542E+02, 7.16468E+02, 7.17412E+02, 7.17783E+02, 7.17340E+02, bhh04690
     X 7.18184E+02, 7.18716E+02, 7.18809E+02, 7.18282E+02, 7.20317E+02, bhh04700
     X 7.18568E+02, 7.16274E+02, 7.19119E+02, 7.20852E+02, 7.21727E+02, bhh04710
     X 7.22607E+02, 7.26369E+02, 7.26412E+02, 7.27101E+02, 7.29404E+02, bhh04720
     X 7.30786E+02, 7.30910E+02, 7.30656E+02, 7.30566E+02, 7.33408E+02, bhh04730
     X 7.37064E+02, 7.39178E+02, 7.36713E+02, 7.37365E+02, 7.40861E+02, bhh04740
     X 7.45281E+02, 7.46178E+02, 7.46991E+02, 7.48035E+02, 7.49777E+02, bhh04750
     X 7.54665E+02, 7.56585E+02, 7.57408E+02, 7.58131E+02, 7.58155E+02, bhh04760
     X 7.60838E+02, 7.64792E+02, 7.68161E+02, 7.69263E+02, 7.73166E+02/ bhh04770
      DATA C01521 /                                                     bhh04780
     X 7.79006E+02, 7.82037E+02, 7.83109E+02, 7.84674E+02, 7.87444E+02, bhh04790
     X 7.89510E+02, 7.90130E+02, 7.91364E+02, 7.95225E+02, 8.03599E+02, bhh04800
     X 8.06340E+02, 8.05105E+02, 8.05120E+02, 8.08515E+02, 8.10907E+02, bhh04810
     X 8.11388E+02, 8.13432E+02, 8.12579E+02, 8.10564E+02, 8.08719E+02, bhh04820
     X 8.07682E+02, 8.05009E+02, 8.01754E+02, 8.01013E+02, 7.99926E+02, bhh04830
     X 7.99067E+02, 7.98369E+02, 7.94090E+02, 7.92883E+02, 7.94244E+02, bhh04840
     X 7.98220E+02, 7.98201E+02, 7.98332E+02, 7.99289E+02, 8.02355E+02, bhh04850
     X 8.03621E+02, 8.05302E+02, 8.08368E+02, 8.09983E+02, 8.11529E+02, bhh04860
     X 8.13068E+02, 8.14717E+02, 8.16441E+02, 8.19241E+02, 8.22944E+02, bhh04870
     X 8.23768E+02, 8.25030E+02, 8.26103E+02, 8.26374E+02, 8.28331E+02, bhh04880
     X 8.32620E+02, 8.38618E+02, 8.43666E+02, 8.45212E+02, 8.46324E+02, bhh04890
     X 8.48536E+02, 8.50192E+02, 8.53083E+02, 8.56653E+02, 8.59614E+02, bhh04900
     X 8.62000E+02, 8.64593E+02, 8.67678E+02, 8.70908E+02, 8.73408E+02, bhh04910
     X 8.74779E+02, 8.74005E+02, 8.76718E+02, 8.80445E+02, 8.84365E+02, bhh04920
     X 8.83806E+02, 8.84292E+02, 8.85539E+02, 8.87474E+02, 8.84905E+02, bhh04930
     X 8.84039E+02, 8.85105E+02, 8.83733E+02, 8.82224E+02, 8.79865E+02/ bhh04940
      DATA C01601 /                                                     bhh04950
     X 8.75663E+02, 8.75575E+02, 8.73144E+02, 8.68602E+02, 8.70278E+02, bhh04960
     X 8.69659E+02, 8.68701E+02, 8.69250E+02, 8.71057E+02, 8.72860E+02, bhh04970
     X 8.74361E+02, 8.74458E+02, 8.77576E+02, 8.81613E+02, 8.84358E+02, bhh04980
     X 8.87440E+02, 8.91549E+02, 8.96568E+02, 8.99836E+02, 9.02880E+02, bhh04990
     X 9.05428E+02, 9.06891E+02, 9.07349E+02, 9.10151E+02, 9.15917E+02, bhh05000
     X 9.16197E+02, 9.18571E+02, 9.21219E+02, 9.20292E+02, 9.21949E+02, bhh05010
     X 9.24509E+02, 9.27454E+02, 9.29474E+02, 9.31348E+02, 9.32818E+02, bhh05020
     X 9.32658E+02, 9.36280E+02, 9.39512E+02, 9.39667E+02, 9.44078E+02, bhh05030
     X 9.47196E+02, 9.48291E+02, 9.46150E+02, 9.46918E+02, 9.49093E+02, bhh05040
     X 9.51372E+02, 9.53109E+02, 9.56308E+02, 9.61335E+02, 9.58214E+02, bhh05050
     X 9.56188E+02, 9.55660E+02, 9.58633E+02, 9.57541E+02, 9.54879E+02, bhh05060
     X 9.51663E+02, 9.52839E+02, 9.52055E+02, 9.49253E+02, 9.50187E+02, bhh05070
     X 9.50323E+02, 9.50937E+02, 9.54362E+02, 9.55855E+02, 9.56350E+02, bhh05080
     X 9.55908E+02, 9.57963E+02, 9.61866E+02, 9.66948E+02, 9.69786E+02, bhh05090
     X 9.74302E+02, 9.79061E+02, 9.82465E+02, 9.86019E+02, 9.89930E+02, bhh05100
     X 9.94294E+02, 9.97011E+02, 9.98207E+02, 9.98607E+02, 1.00175E+03/ bhh05110
      DATA C01681 /                                                     bhh05120
     X 1.00275E+03, 1.00284E+03, 1.00294E+03, 1.00485E+03, 1.00593E+03, bhh05130
     X 1.00524E+03, 1.00415E+03, 1.00335E+03, 1.00278E+03, 1.00185E+03, bhh05140
     X 9.99982E+02, 9.98177E+02, 9.97959E+02, 9.99161E+02, 9.98810E+02, bhh05150
     X 9.95415E+02, 9.94342E+02, 9.92998E+02, 9.91340E+02, 9.90900E+02, bhh05160
     X 9.90407E+02, 9.89232E+02, 9.85447E+02, 9.86312E+02, 9.87461E+02, bhh05170
     X 9.86090E+02, 9.86670E+02, 9.85534E+02, 9.81877E+02, 9.84946E+02, bhh05180
     X 9.86392E+02, 9.86709E+02, 9.88086E+02, 9.90269E+02, 9.92566E+02, bhh05190
     X 9.94029E+02, 9.95795E+02, 9.97788E+02, 1.00005E+03, 1.00287E+03, bhh05200
     X 1.00566E+03, 1.00833E+03, 1.00982E+03, 1.01348E+03, 1.01862E+03, bhh05210
     X 1.02322E+03, 1.02786E+03, 1.03179E+03, 1.03339E+03, 1.03833E+03, bhh05220
     X 1.04317E+03, 1.04598E+03, 1.04753E+03, 1.04981E+03, 1.05321E+03, bhh05230
     X 1.05492E+03, 1.05721E+03, 1.05978E+03, 1.06033E+03, 1.06107E+03, bhh05240
     X 1.06155E+03, 1.06035E+03, 1.05838E+03, 1.05649E+03, 1.05553E+03, bhh05250
     X 1.05498E+03, 1.05387E+03, 1.05171E+03, 1.04877E+03, 1.04725E+03, bhh05260
     X 1.04748E+03, 1.04733E+03, 1.04704E+03, 1.04643E+03, 1.04411E+03, bhh05270
     X 1.04435E+03, 1.04520E+03, 1.04233E+03, 1.04047E+03, 1.03992E+03/ bhh05280
      DATA C01761 /                                                     bhh05290
     X 1.04192E+03, 1.04171E+03, 1.04140E+03, 1.04197E+03, 1.04415E+03, bhh05300
     X 1.04548E+03, 1.04533E+03, 1.04616E+03, 1.04705E+03, 1.04800E+03, bhh05310
     X 1.05025E+03, 1.05219E+03, 1.05412E+03, 1.05808E+03, 1.06062E+03, bhh05320
     X 1.06292E+03, 1.06780E+03, 1.07219E+03, 1.07610E+03, 1.07913E+03, bhh05330
     X 1.08405E+03, 1.08798E+03, 1.08835E+03, 1.09140E+03, 1.09447E+03, bhh05340
     X 1.09676E+03, 1.10015E+03, 1.10272E+03, 1.10410E+03, 1.10749E+03, bhh05350
     X 1.10991E+03, 1.11121E+03, 1.10981E+03, 1.10981E+03, 1.11063E+03, bhh05360
     X 1.10714E+03, 1.10500E+03, 1.10357E+03, 1.10093E+03, 1.09898E+03, bhh05370
     X 1.09679E+03, 1.09188E+03, 1.09088E+03, 1.09040E+03, 1.08586E+03, bhh05380
     X 1.08178E+03, 1.07752E+03, 1.07243E+03, 1.07178E+03, 1.07084E+03, bhh05390
     X 1.06693E+03, 1.06527E+03, 1.06405E+03, 1.06285E+03, 1.06287E+03, bhh05400
     X 1.06276E+03, 1.06221E+03, 1.06464E+03, 1.06579E+03, 1.06498E+03, bhh05410
     X 1.06596E+03, 1.06812E+03, 1.07159E+03, 1.07361E+03, 1.07556E+03, bhh05420
     X 1.07751E+03, 1.08128E+03, 1.08523E+03, 1.08927E+03, 1.09193E+03, bhh05430
     X 1.09612E+03, 1.10133E+03, 1.10435E+03, 1.10781E+03, 1.11168E+03, bhh05440
     X 1.11641E+03, 1.12217E+03, 1.12839E+03, 1.13298E+03, 1.13575E+03/ bhh05450
      DATA C01841 /                                                     bhh05460
     X 1.13742E+03, 1.13929E+03, 1.14132E+03, 1.14340E+03, 1.14518E+03, bhh05470
     X 1.14742E+03, 1.14943E+03, 1.14935E+03, 1.14975E+03, 1.15086E+03, bhh05480
     X 1.15420E+03, 1.15267E+03, 1.15007E+03, 1.15155E+03, 1.14982E+03, bhh05490
     X 1.14663E+03, 1.14301E+03, 1.13986E+03, 1.13676E+03, 1.13307E+03, bhh05500
     X 1.12898E+03, 1.12516E+03, 1.12284E+03, 1.12068E+03, 1.11855E+03, bhh05510
     X 1.11632E+03, 1.11464E+03, 1.11318E+03, 1.11180E+03, 1.11163E+03, bhh05520
     X 1.11160E+03, 1.11035E+03, 1.11178E+03, 1.11395E+03, 1.11447E+03, bhh05530
     X 1.11439E+03, 1.11440E+03, 1.11582E+03, 1.11560E+03, 1.11478E+03, bhh05540
     X 1.11448E+03, 1.11454E+03, 1.11494E+03, 1.11607E+03, 1.11736E+03, bhh05550
     X 1.11854E+03, 1.11875E+03, 1.11989E+03, 1.12165E+03, 1.12427E+03, bhh05560
     X 1.12620E+03, 1.12758E+03, 1.12774E+03, 1.12870E+03, 1.13001E+03, bhh05570
     X 1.13006E+03, 1.13078E+03, 1.13172E+03, 1.12971E+03, 1.12857E+03, bhh05580
     X 1.12810E+03, 1.12740E+03, 1.12659E+03, 1.12564E+03, 1.12338E+03, bhh05590
     X 1.12117E+03, 1.11902E+03, 1.11878E+03, 1.11855E+03, 1.11828E+03, bhh05600
     X 1.11791E+03, 1.11784E+03, 1.11815E+03, 1.11957E+03, 1.12046E+03, bhh05610
     X 1.12042E+03, 1.11929E+03, 1.12074E+03, 1.12708E+03, 1.12600E+03/ bhh05620
      DATA C01921 /                                                     bhh05630
     X 1.12538E+03, 1.12871E+03, 1.13167E+03, 1.13388E+03, 1.13444E+03, bhh05640
     X 1.13595E+03, 1.13801E+03, 1.14096E+03, 1.14230E+03, 1.14304E+03, bhh05650
     X 1.14421E+03, 1.14580E+03, 1.14767E+03, 1.15000E+03, 1.15126E+03, bhh05660
     X 1.15181E+03, 1.15197E+03, 1.15364E+03, 1.15626E+03, 1.15538E+03, bhh05670
     X 1.15636E+03, 1.15908E+03, 1.16024E+03, 1.16188E+03, 1.16411E+03, bhh05680
     X 1.16310E+03, 1.16430E+03, 1.16927E+03, 1.17035E+03, 1.17052E+03, bhh05690
     X 1.17013E+03, 1.16968E+03, 1.16969E+03, 1.17106E+03, 1.17123E+03, bhh05700
     X 1.17006E+03, 1.16536E+03, 1.16087E+03, 1.15691E+03, 1.15608E+03, bhh05710
     X 1.15388E+03, 1.15077E+03, 1.14967E+03, 1.14793E+03, 1.14554E+03, bhh05720
     X 1.14212E+03, 1.13908E+03, 1.13654E+03, 1.13499E+03, 1.13308E+03, bhh05730
     X 1.13033E+03, 1.13051E+03, 1.13073E+03, 1.12898E+03, 1.12941E+03, bhh05740
     X 1.13051E+03, 1.13086E+03, 1.13189E+03, 1.13304E+03, 1.13192E+03, bhh05750
     X 1.13131E+03, 1.13110E+03, 1.13499E+03, 1.13914E+03, 1.14359E+03, bhh05760
     X 1.14383E+03, 1.14390E+03, 1.14435E+03, 1.14540E+03, 1.14646E+03, bhh05770
     X 1.14716E+03, 1.14880E+03, 1.15062E+03, 1.15170E+03, 1.15093E+03, bhh05780
     X 1.14926E+03, 1.15133E+03, 1.15167E+03, 1.15043E+03, 1.15134E+03/ bhh05790
      DATA C02001 /                                                     bhh05800
     X 1.15135E+03, 1.15000E+03, 1.15087E+03, 1.15118E+03, 1.14935E+03, bhh05810
     X 1.14780E+03, 1.14647E+03, 1.14560E+03, 1.14404E+03, 1.14238E+03, bhh05820
     X 1.14406E+03, 1.14245E+03, 1.13781E+03, 1.13664E+03, 1.13653E+03, bhh05830
     X 1.13778E+03, 1.13813E+03, 1.13794E+03, 1.13681E+03, 1.13515E+03, bhh05840
     X 1.13328E+03, 1.13132E+03, 1.13080E+03, 1.13130E+03, 1.13400E+03, bhh05850
     X 1.13526E+03, 1.13494E+03, 1.13193E+03, 1.12898E+03, 1.12654E+03, bhh05860
     X 1.12739E+03, 1.12849E+03, 1.12774E+03, 1.12733E+03, 1.12733E+03, bhh05870
     X 1.12943E+03, 1.13014E+03, 1.12967E+03, 1.12731E+03, 1.12671E+03, bhh05880
     X 1.12885E+03, 1.13050E+03, 1.13201E+03, 1.13345E+03, 1.13488E+03, bhh05890
     X 1.13605E+03, 1.13530E+03, 1.13737E+03, 1.14186E+03, 1.14250E+03, bhh05900
     X 1.14305E+03, 1.14383E+03, 1.14510E+03, 1.14659E+03, 1.14848E+03, bhh05910
     X 1.14949E+03, 1.14995E+03, 1.14934E+03, 1.15058E+03, 1.15368E+03, bhh05920
     X 1.15435E+03, 1.15422E+03, 1.15296E+03, 1.15228E+03, 1.15189E+03, bhh05930
     X 1.15198E+03, 1.15081E+03, 1.14881E+03, 1.14562E+03, 1.14276E+03, bhh05940
     X 1.14030E+03, 1.13637E+03, 1.13254E+03, 1.12942E+03, 1.12653E+03, bhh05950
     X 1.12362E+03, 1.11987E+03, 1.11712E+03, 1.11522E+03, 1.11403E+03/ bhh05960
      DATA C02081 /                                                     bhh05970
     X 1.11226E+03, 1.10947E+03, 1.10956E+03, 1.10976E+03, 1.10748E+03, bhh05980
     X 1.10673E+03, 1.10688E+03, 1.10675E+03, 1.10533E+03, 1.10230E+03, bhh05990
     X 1.10384E+03, 1.10496E+03, 1.10274E+03, 1.10197E+03, 1.10196E+03, bhh06000
     X 1.10278E+03, 1.10257E+03, 1.10147E+03, 1.10205E+03, 1.10308E+03, bhh06010
     X 1.10478E+03, 1.10358E+03, 1.10197E+03, 1.10305E+03, 1.10390E+03, bhh06020
     X 1.10456E+03, 1.10526E+03, 1.10588E+03, 1.10640E+03, 1.10747E+03, bhh06030
     X 1.10904E+03, 1.11214E+03, 1.11350E+03, 1.11359E+03, 1.11604E+03, bhh06040
     X 1.11706E+03, 1.11594E+03, 1.11600E+03, 1.11616E+03, 1.11561E+03, bhh06050
     X 1.11556E+03, 1.11547E+03, 1.11370E+03, 1.11289E+03, 1.11276E+03, bhh06060
     X 1.11338E+03, 1.11437E+03, 1.11595E+03, 1.11309E+03, 1.10958E+03, bhh06070
     X 1.10887E+03, 1.10573E+03, 1.10068E+03, 1.10194E+03, 1.10165E+03, bhh06080
     X 1.09813E+03, 1.09973E+03, 1.10233E+03, 1.10121E+03, 1.10097E+03, bhh06090
     X 1.10149E+03, 1.10162E+03, 1.10222E+03, 1.10389E+03, 1.10315E+03, bhh06100
     X 1.10158E+03, 1.10193E+03, 1.10186E+03, 1.10135E+03, 1.10336E+03, bhh06110
     X 1.10500E+03, 1.10459E+03, 1.10592E+03, 1.10784E+03, 1.10076E+03, bhh06120
     X 1.09615E+03, 1.09496E+03, 1.09422E+03, 1.09350E+03, 1.09244E+03/ bhh06130
      DATA C02161 /                                                     bhh06140
     X 1.08955E+03, 1.08535E+03, 1.08379E+03, 1.08184E+03, 1.07889E+03, bhh06150
     X 1.07563E+03, 1.07238E+03, 1.07042E+03, 1.06882E+03, 1.06761E+03, bhh06160
     X 1.06816E+03, 1.06772E+03, 1.06327E+03, 1.06313E+03, 1.06563E+03, bhh06170
     X 1.06254E+03, 1.06072E+03, 1.06095E+03, 1.06173E+03, 1.06269E+03, bhh06180
     X 1.06361E+03, 1.06438E+03, 1.06501E+03, 1.06465E+03, 1.06481E+03, bhh06190
     X 1.06685E+03, 1.06642E+03, 1.06447E+03, 1.06701E+03, 1.06791E+03, bhh06200
     X 1.06612E+03, 1.06471E+03, 1.06403E+03, 1.06774E+03, 1.06823E+03, bhh06210
     X 1.06524E+03, 1.06479E+03, 1.06453E+03, 1.06346E+03, 1.06175E+03, bhh06220
     X 1.05958E+03, 1.05941E+03, 1.05936E+03, 1.05938E+03, 1.05736E+03, bhh06230
     X 1.05449E+03, 1.05307E+03, 1.05180E+03, 1.05074E+03, 1.04810E+03, bhh06240
     X 1.04536E+03, 1.04477E+03, 1.04389E+03, 1.04272E+03, 1.04006E+03, bhh06250
     X 1.03739E+03, 1.03533E+03, 1.03476E+03, 1.03516E+03, 1.03275E+03, bhh06260
     X 1.03093E+03, 1.03062E+03, 1.02997E+03, 1.02919E+03, 1.02993E+03, bhh06270
     X 1.02983E+03, 1.02837E+03, 1.02611E+03, 1.02386E+03, 1.02426E+03, bhh06280
     X 1.02542E+03, 1.02750E+03, 1.02638E+03, 1.02496E+03, 1.02608E+03, bhh06290
     X 1.02568E+03, 1.02388E+03, 1.02522E+03, 1.02692E+03, 1.02834E+03/ bhh06300
      DATA C02241 /                                                     bhh06310
     X 1.02828E+03, 1.02716E+03, 1.02667E+03, 1.02607E+03, 1.02503E+03, bhh06320
     X 1.02723E+03, 1.03143E+03, 1.02881E+03, 1.02646E+03, 1.02500E+03, bhh06330
     X 1.02569E+03, 1.02743E+03, 1.02608E+03, 1.02548E+03, 1.02620E+03, bhh06340
     X 1.02733E+03, 1.02839E+03, 1.02575E+03, 1.02432E+03, 1.02471E+03, bhh06350
     X 1.02392E+03, 1.02267E+03, 1.02077E+03, 1.01964E+03, 1.01957E+03, bhh06360
     X 1.01848E+03, 1.01704E+03, 1.01524E+03, 1.01352E+03, 1.01191E+03, bhh06370
     X 1.01066E+03, 1.00952E+03, 1.00849E+03, 1.00660E+03, 1.00368E+03, bhh06380
     X 9.99713E+02, 9.95921E+02, 9.94845E+02, 9.93286E+02, 9.91204E+02/ bhh06390
C                                                                       bhh06400
      END                                                               bhh06410
      BLOCK DATA BO3HH1                                                 bhh1 100
C>    BLOCK DATA                                                        bhh1 110
C                                                                       bhh1 120
C     RATIO (C1/C0)                                                     bhh1 130
C     DATA FROM BASS 1985                                               bhh1 140
C                                                                       bhh1 150
C     NOW INCLUDES MOLINA & MOLINA AT 273K WITH THE TEMPERATURE         bhh1 160
C     DEPENDENCE DETERMINED FROM THE 195K HARVARD MEASUREMENTS,         bhh1 170
C     EMPLOYING THE BASS ALGORITHM (CO(1+C1*T+C2*T2); THIS IS           bhh1 180
C     ONLY FOR THE WAVELENGTH RANGE FROM .34 TO .35 MICRONS;            bhh1 190
C     OTHERWISE, THE BASS DATA ALONE HAVE BEEN EMPLOYED BETWEEN         bhh1 200
C     .34 AND .245 MICRONS.                                             bhh1 210
C                                                                       bhh1 220
C     NEW T-DEPENDENT X-SECTIONS BETWEEN .345 AND .36 MICRONS           bhh1 230
C     HAVE NOW BEEN ADDED, BASED ON WORK BY CACCIANI, DISARRA           bhh1 240
C     AND FIOCCO, UNIVERSITY OF ROME, 1987.  QUADRATIC TEMP             bhh1 250
C     HAS BEEN DERIVED, AS ABOVE.                                       bhh1 260
C                                                                       bhh1 270
C     AGREEMENT AMONGST THE FOUR DATA SETS IS REASONABLE (<10%)         bhh1 280
C     AND OFTEN EXCELLENT (0-3%)                                        bhh1 290
C                                                                       bhh1 300
C                                                                       bhh1 310
      COMMON /O3HH1/  V1C,V2C,DVC,NC,                                   bhh1 320
     X           O31001(85),C10086(80),C10166(80),C10246(65),C10311(16),bhh1 330
     X           C10327(80),C10407(1),                                  bhh1 340
     X           C10001(80),C10081(80),C10161(80),C10241(80),C10321(80),bhh1 350
     X           C10401(80),C10481(80),C10561(80),C10641(80),C10721(80),bhh1 360
     X           C10801(80),C10881(80),C10961(80),C11041(80),C11121(80),bhh1 370
     X           C11201(80),C11281(80),C11361(80),C11441(80),C11521(80),bhh1 380
     X           C11601(80),C11681(80),C11761(80),C11841(80),C11921(80),bhh1 390
     X           C12001(80),C12081(80),C12161(80),C12241(40)            bhh1 400
C                                                                       bhh1 410
C     DATA V1C /29405./, V2C /40800./ ,DVC /5./, NC /2280/   BASS       bhh1 420
      DATA V1C /27370./, V2C /40800./ ,DVC /5./, NC /2687/              bhh1 430
C                                                                       bhh1 440
      DATA O31001/85*1.3E-3/                                            bhh1 450
                                                                        bhh1 460
      DATA C10086/                                                      bhh1 470
     X 1.37330E-03, 1.62821E-03, 2.01703E-03, 2.54574E-03, 3.20275E-03, bhh1 480
     X 3.89777E-03, 4.62165E-03, 5.26292E-03, 5.86986E-03, 6.41494E-03, bhh1 490
     X 6.96761E-03, 7.48539E-03, 7.89600E-03, 7.87305E-03, 7.81981E-03, bhh1 500
     X 7.63864E-03, 7.67455E-03, 7.72586E-03, 7.69784E-03, 7.57367E-03, bhh1 510
     X 7.27336E-03, 7.14064E-03, 7.24207E-03, 7.09851E-03, 6.93654E-03, bhh1 520
     X 6.89385E-03, 7.05768E-03, 6.85578E-03, 6.58301E-03, 6.50848E-03, bhh1 530
     X 6.52083E-03, 6.46590E-03, 6.70692E-03, 6.92053E-03, 7.17734E-03, bhh1 540
     X 7.05364E-03, 6.63440E-03, 6.54702E-03, 6.27173E-03, 5.98150E-03, bhh1 550
     X 5.66579E-03, 5.51549E-03, 5.50291E-03, 5.93271E-03, 6.36950E-03, bhh1 560
     X 7.18562E-03, 7.51767E-03, 6.53815E-03, 7.22341E-03, 8.63056E-03, bhh1 570
     X 9.11740E-03, 8.80903E-03, 8.59902E-03, 7.74287E-03, 7.33509E-03, bhh1 580
     X 7.50180E-03, 7.81686E-03, 7.85635E-03, 8.08554E-03, 7.21968E-03, bhh1 590
     X 7.99028E-03, 9.90724E-03, 1.29121E-02, 1.54686E-02, 1.60876E-02, bhh1 600
     X 1.59530E-02, 1.57040E-02, 1.59499E-02, 1.63961E-02, 1.72670E-02, bhh1 610
     X 1.81634E-02, 1.95519E-02, 2.14181E-02, 2.28670E-02, 2.33506E-02, bhh1 620
     X 2.22736E-02, 2.14296E-02, 2.15271E-02, 2.30730E-02, 2.36220E-02/ bhh1 630
      DATA C10166/                                                      bhh1 640
     X 2.44466E-02, 2.44476E-02, 2.39223E-02, 2.41386E-02, 2.53687E-02, bhh1 650
     X 2.67491E-02, 2.80425E-02, 2.77558E-02, 2.82626E-02, 2.86776E-02, bhh1 660
     X 2.88781E-02, 2.89248E-02, 2.89983E-02, 2.85534E-02, 2.87102E-02, bhh1 670
     X 2.83695E-02, 2.76719E-02, 2.76091E-02, 2.90733E-02, 2.80388E-02, bhh1 680
     X 2.73706E-02, 2.65055E-02, 2.61268E-02, 2.45892E-02, 2.37213E-02, bhh1 690
     X 2.22542E-02, 2.10116E-02, 2.02852E-02, 1.97635E-02, 1.94079E-02, bhh1 700
     X 1.90997E-02, 1.85598E-02, 1.79221E-02, 1.77887E-02, 1.73709E-02, bhh1 710
     X 1.67263E-02, 1.60932E-02, 1.50775E-02, 1.39563E-02, 1.23691E-02, bhh1 720
     X 1.07402E-02, 9.35859E-03, 8.43786E-03, 7.92075E-03, 7.33239E-03, bhh1 730
     X 6.73638E-03, 6.28740E-03, 5.85640E-03, 5.85384E-03, 6.10577E-03, bhh1 740
     X 7.26050E-03, 9.66384E-03, 1.29629E-02, 1.69596E-02, 2.03465E-02, bhh1 750
     X 2.26429E-02, 2.39653E-02, 2.47970E-02, 2.51993E-02, 2.51383E-02, bhh1 760
     X 2.52014E-02, 2.47766E-02, 2.47171E-02, 2.47478E-02, 2.43986E-02, bhh1 770
     X 2.43498E-02, 2.40537E-02, 2.40574E-02, 2.40446E-02, 2.40847E-02, bhh1 780
     X 2.39400E-02, 2.42127E-02, 2.47123E-02, 2.52914E-02, 2.52103E-02, bhh1 790
     X 2.51421E-02, 2.43229E-02, 2.37902E-02, 2.30865E-02, 2.28174E-02/ bhh1 800
      DATA C10246/                                                      bhh1 810
     X 2.28830E-02, 2.33671E-02, 2.38274E-02, 2.46699E-02, 2.56739E-02, bhh1 820
     X 2.61408E-02, 2.62898E-02, 2.64228E-02, 2.55561E-02, 2.47095E-02, bhh1 830
     X 2.39071E-02, 2.34319E-02, 2.28738E-02, 2.23434E-02, 2.18888E-02, bhh1 840
     X 2.13639E-02, 2.11937E-02, 2.10110E-02, 2.07672E-02, 2.00697E-02, bhh1 850
     X 1.97605E-02, 1.91208E-02, 1.82056E-02, 1.73945E-02, 1.64542E-02, bhh1 860
     X 1.53969E-02, 1.41816E-02, 1.35665E-02, 1.27109E-02, 1.18254E-02, bhh1 870
     X 1.11489E-02, 1.03984E-02, 1.00760E-02, 9.86649E-03, 9.76766E-03, bhh1 880
     X 9.41662E-03, 9.19082E-03, 9.44272E-03, 1.04547E-02, 1.24713E-02, bhh1 890
     X 1.49310E-02, 1.70272E-02, 1.86057E-02, 1.93555E-02, 1.98350E-02, bhh1 900
     X 2.00041E-02, 2.01233E-02, 2.01917E-02, 1.98918E-02, 1.96649E-02, bhh1 910
     X 1.95162E-02, 2.01044E-02, 2.06711E-02, 2.08881E-02, 2.04812E-02, bhh1 920
     X 1.92249E-02, 1.80188E-02, 1.69496E-02, 1.60488E-02, 1.52865E-02, bhh1 930
     X 1.46940E-02, 1.41067E-02, 1.35675E-02, 1.31094E-02, 1.27542E-02/ bhh1 940
      DATA C10311/                                                      bhh1 950
     X                                                     1.3073E-02,  bhh1 960
     X 1.2795E-02,  1.2753E-02,  1.2868E-02,  1.2885E-02,  1.2554E-02,  bhh1 970
     X 1.2106E-02,  1.1616E-02,  1.1394E-02,  1.1092E-02,  1.0682E-02,  bhh1 980
     X 1.0519E-02,  9.7219E-03,  9.3434E-03,  8.5260E-03,  8.3333E-03/  bhh1 990
      DATA C10327/                                                      bhh11000
     X 7.8582E-03,  6.8295E-03,  6.7963E-03,  6.7516E-03,  6.2930E-03,  bhh11010
     X 6.1615E-03,  6.1250E-03,  5.9011E-03,  5.7823E-03,  5.4688E-03,  bhh11020
     X 5.0978E-03,  4.4526E-03,  3.8090E-03,  3.2310E-03,  3.0128E-03,  bhh11030
     X 3.9063E-03,  6.7911E-03,  9.3161E-03,  1.0256E-02,  1.0183E-02,  bhh11040
     X 9.8289E-03,  9.5683E-03,  9.0406E-03,  8.7148E-03,  8.5284E-03,  bhh11050
     X 8.6149E-03,  8.7238E-03,  9.3679E-03,  1.0683E-02,  1.2016E-02,  bhh11060
     X 1.3097E-02,  1.3610E-02,  1.3588E-02,  1.3805E-02,  1.3928E-02,  bhh11070
     X 1.3903E-02,  1.3446E-02,  1.3258E-02,  1.3194E-02,  1.2703E-02,  bhh11080
     X 1.2393E-02,  1.2487E-02,  1.2341E-02,  1.2388E-02,  1.2061E-02,  bhh11090
     X 1.2122E-02,  1.1850E-02,  1.2032E-02,  1.1806E-02,  1.1810E-02,  bhh11100
     X 1.1572E-02,  1.1397E-02,  1.0980E-02,  1.1012E-02,  1.0524E-02,  bhh11110
     X 1.0518E-02,  1.0227E-02,  9.6837E-03,  9.6425E-03,  8.9938E-03,  bhh11120
     X 9.1488E-03,  8.8595E-03,  8.5976E-03,  8.4447E-03,  8.0731E-03,  bhh11130
     X 8.0283E-03,  7.7827E-03,  7.7638E-03,  7.2438E-03,  6.8246E-03,  bhh11140
     X 6.3457E-03,  5.6632E-03,  5.2500E-03,  4.3593E-03,  3.9431E-03,  bhh11150
     X 3.1580E-03,  2.2298E-03,  1.7818E-03,  1.4513E-03,  1.3188E-03/  bhh11160
      DATA C10407/                                                      bhh11170
     X 2.1034E-03/                                                      bhh11180
      DATA C10001 /                                                     bhh11190
     X 6.45621E-03, 7.11308E-03, 1.06130E-02, 1.36338E-02, 1.27746E-02, bhh11200
     X 1.42152E-02, 1.41144E-02, 1.64830E-02, 1.67110E-02, 1.57368E-02, bhh11210
     X 1.54644E-02, 1.45248E-02, 1.43206E-02, 1.56946E-02, 1.54268E-02, bhh11220
     X 1.37500E-02, 1.50224E-02, 1.60919E-02, 1.49099E-02, 1.53960E-02, bhh11230
     X 1.61871E-02, 1.46539E-02, 1.38258E-02, 1.32571E-02, 1.21580E-02, bhh11240
     X 1.39596E-02, 1.16029E-02, 1.47042E-02, 1.07441E-02, 1.08999E-02, bhh11250
     X 1.05562E-02, 1.00589E-02, 9.60711E-03, 9.36950E-03, 7.65303E-03, bhh11260
     X 6.86216E-03, 7.05344E-03, 6.90728E-03, 6.78627E-03, 6.97435E-03, bhh11270
     X 5.75456E-03, 5.81685E-03, 5.00915E-03, 4.90259E-03, 4.42545E-03, bhh11280
     X 4.14633E-03, 3.61657E-03, 3.08178E-03, 2.91680E-03, 2.94554E-03, bhh11290
     X 3.35794E-03, 5.49025E-03, 7.09867E-03, 6.82592E-03, 8.84835E-03, bhh11300
     X 9.15718E-03, 9.17935E-03, 8.31848E-03, 7.79481E-03, 7.75125E-03, bhh11310
     X 6.95844E-03, 7.34506E-03, 7.53823E-03, 7.03272E-03, 7.57051E-03, bhh11320
     X 9.20239E-03, 1.10864E-02, 1.16188E-02, 1.30029E-02, 1.44364E-02, bhh11330
     X 1.29292E-02, 1.36031E-02, 1.35967E-02, 1.30412E-02, 1.29874E-02, bhh11340
     X 1.14829E-02, 1.18009E-02, 1.20829E-02, 1.17831E-02, 1.21489E-02/ bhh11350
      DATA C10081 /                                                     bhh11360
     X 1.27019E-02, 1.25557E-02, 1.23812E-02, 1.20158E-02, 1.26749E-02, bhh11370
     X 1.17139E-02, 1.14552E-02, 1.11268E-02, 9.79143E-03, 8.79741E-03, bhh11380
     X 8.85709E-03, 8.57653E-03, 8.93908E-03, 8.46205E-03, 8.56506E-03, bhh11390
     X 8.14319E-03, 8.14415E-03, 7.74205E-03, 7.80727E-03, 7.49886E-03, bhh11400
     X 7.71114E-03, 6.55963E-03, 6.87550E-03, 6.39162E-03, 5.55359E-03, bhh11410
     X 5.43275E-03, 4.90649E-03, 4.41165E-03, 4.21875E-03, 3.62592E-03, bhh11420
     X 3.40700E-03, 2.40267E-03, 2.61479E-03, 2.75677E-03, 4.10842E-03, bhh11430
     X 5.79601E-03, 7.10708E-03, 8.07826E-03, 8.16166E-03, 8.72620E-03, bhh11440
     X 8.85878E-03, 8.72755E-03, 8.25811E-03, 8.12100E-03, 7.78534E-03, bhh11450
     X 7.39762E-03, 8.43880E-03, 8.53789E-03, 9.90072E-03, 1.01668E-02, bhh11460
     X 1.00827E-02, 9.73556E-03, 9.57462E-03, 1.01289E-02, 1.10670E-02, bhh11470
     X 1.03508E-02, 1.00929E-02, 9.10236E-03, 9.39459E-03, 8.79601E-03, bhh11480
     X 8.67936E-03, 8.53862E-03, 7.95459E-03, 8.04037E-03, 7.95361E-03, bhh11490
     X 7.87432E-03, 6.99165E-03, 7.37107E-03, 6.09187E-03, 6.21030E-03, bhh11500
     X 5.33277E-03, 5.04633E-03, 4.45811E-03, 4.34153E-03, 3.98596E-03, bhh11510
     X 3.84225E-03, 3.41943E-03, 3.60535E-03, 2.81691E-03, 2.49771E-03/ bhh11520
      DATA C10161 /                                                     bhh11530
     X 2.35046E-03, 2.50947E-03, 3.75462E-03, 4.92349E-03, 5.09294E-03, bhh11540
     X 4.98312E-03, 5.19325E-03, 4.41827E-03, 4.25192E-03, 4.46745E-03, bhh11550
     X 4.08731E-03, 3.84776E-03, 3.67507E-03, 3.76845E-03, 3.69210E-03, bhh11560
     X 4.59864E-03, 6.42677E-03, 7.83255E-03, 7.89247E-03, 8.10883E-03, bhh11570
     X 8.00825E-03, 8.40322E-03, 7.97108E-03, 8.24714E-03, 8.39006E-03, bhh11580
     X 8.68787E-03, 8.61108E-03, 8.81552E-03, 9.36996E-03, 9.08243E-03, bhh11590
     X 9.69116E-03, 9.66185E-03, 9.22856E-03, 9.65086E-03, 9.35398E-03, bhh11600
     X 9.06358E-03, 8.76851E-03, 8.43072E-03, 7.85659E-03, 7.93936E-03, bhh11610
     X 7.49712E-03, 7.20199E-03, 6.94581E-03, 6.64086E-03, 6.12627E-03, bhh11620
     X 6.13967E-03, 5.67310E-03, 5.09928E-03, 4.59112E-03, 3.95257E-03, bhh11630
     X 3.67652E-03, 3.28781E-03, 2.77471E-03, 2.74494E-03, 2.15529E-03, bhh11640
     X 1.95283E-03, 1.75043E-03, 1.60419E-03, 1.82688E-03, 2.34667E-03, bhh11650
     X 2.92502E-03, 3.88322E-03, 4.39984E-03, 4.67814E-03, 4.80395E-03, bhh11660
     X 4.69130E-03, 4.54564E-03, 4.46773E-03, 4.59178E-03, 4.37498E-03, bhh11670
     X 4.12706E-03, 4.18299E-03, 4.57267E-03, 5.60127E-03, 6.51936E-03, bhh11680
     X 7.10498E-03, 7.49870E-03, 7.89554E-03, 7.97428E-03, 8.21044E-03/ bhh11690
      DATA C10241 /                                                     bhh11700
     X 8.06324E-03, 7.76648E-03, 7.62238E-03, 7.77675E-03, 7.46905E-03, bhh11710
     X 7.61115E-03, 7.42715E-03, 7.28461E-03, 7.51514E-03, 7.38782E-03, bhh11720
     X 6.97206E-03, 6.52738E-03, 6.10147E-03, 5.87553E-03, 5.49218E-03, bhh11730
     X 4.94873E-03, 4.47920E-03, 4.25005E-03, 3.98094E-03, 3.92084E-03, bhh11740
     X 3.41707E-03, 3.30501E-03, 3.09208E-03, 3.19686E-03, 3.55283E-03, bhh11750
     X 4.20775E-03, 4.11155E-03, 3.72193E-03, 3.52000E-03, 3.13572E-03, bhh11760
     X 2.87629E-03, 2.64251E-03, 2.33451E-03, 2.22426E-03, 2.05800E-03, bhh11770
     X 1.75214E-03, 2.32530E-03, 2.68651E-03, 3.66315E-03, 4.93904E-03, bhh11780
     X 5.32850E-03, 5.43978E-03, 5.32656E-03, 5.15649E-03, 5.42096E-03, bhh11790
     X 5.37193E-03, 5.23454E-03, 5.34557E-03, 5.50533E-03, 6.13216E-03, bhh11800
     X 6.65129E-03, 7.09357E-03, 7.46042E-03, 7.68605E-03, 7.91866E-03, bhh11810
     X 7.52953E-03, 7.48272E-03, 7.17800E-03, 6.80060E-03, 6.60427E-03, bhh11820
     X 6.43049E-03, 6.45975E-03, 6.20534E-03, 5.93094E-03, 5.67360E-03, bhh11830
     X 5.38584E-03, 5.19364E-03, 4.92599E-03, 4.60655E-03, 4.24669E-03, bhh11840
     X 3.94253E-03, 3.55894E-03, 3.24256E-03, 2.92974E-03, 2.62760E-03, bhh11850
     X 2.52238E-03, 2.24714E-03, 2.26350E-03, 2.44380E-03, 3.03798E-03/ bhh11860
      DATA C10321 /                                                     bhh11870
     X 3.50000E-03, 3.55416E-03, 3.43661E-03, 3.19814E-03, 3.02155E-03, bhh11880
     X 2.73890E-03, 2.50078E-03, 2.34595E-03, 2.18282E-03, 2.19285E-03, bhh11890
     X 2.49482E-03, 3.13434E-03, 4.18947E-03, 4.72069E-03, 5.29712E-03, bhh11900
     X 5.39004E-03, 5.44846E-03, 5.37952E-03, 5.09935E-03, 5.08741E-03, bhh11910
     X 5.05257E-03, 5.10339E-03, 5.17968E-03, 5.31841E-03, 5.58106E-03, bhh11920
     X 5.65031E-03, 5.65680E-03, 5.76184E-03, 5.71213E-03, 5.48515E-03, bhh11930
     X 5.32168E-03, 5.18505E-03, 4.99640E-03, 4.78746E-03, 4.57244E-03, bhh11940
     X 4.32728E-03, 4.14464E-03, 3.97659E-03, 4.01874E-03, 4.10588E-03, bhh11950
     X 3.99644E-03, 3.84584E-03, 3.64222E-03, 3.39590E-03, 3.00386E-03, bhh11960
     X 2.73790E-03, 2.45095E-03, 2.29068E-03, 1.64530E-03, 1.68602E-03, bhh11970
     X 2.32934E-03, 3.14851E-03, 3.65706E-03, 3.70878E-03, 3.75103E-03, bhh11980
     X 3.79183E-03, 3.32032E-03, 2.42604E-03, 2.48775E-03, 2.34603E-03, bhh11990
     X 2.36349E-03, 3.33744E-03, 3.44617E-03, 4.27280E-03, 4.61076E-03, bhh12000
     X 5.20165E-03, 5.14851E-03, 5.22909E-03, 5.08278E-03, 5.16125E-03, bhh12010
     X 5.01572E-03, 4.51685E-03, 4.67541E-03, 4.83421E-03, 4.57546E-03, bhh12020
     X 4.55111E-03, 5.03093E-03, 4.67838E-03, 4.44282E-03, 4.40774E-03/ bhh12030
      DATA C10401 /                                                     bhh12040
     X 4.48123E-03, 4.24410E-03, 4.03559E-03, 3.73969E-03, 3.45458E-03, bhh12050
     X 3.18217E-03, 3.16115E-03, 3.36877E-03, 3.62026E-03, 3.69898E-03, bhh12060
     X 3.49845E-03, 3.13839E-03, 2.77731E-03, 2.40106E-03, 2.03935E-03, bhh12070
     X 1.84377E-03, 2.07757E-03, 2.39550E-03, 2.86272E-03, 3.27900E-03, bhh12080
     X 3.42304E-03, 3.50211E-03, 3.29197E-03, 3.24784E-03, 3.20864E-03, bhh12090
     X 3.28063E-03, 3.01328E-03, 3.00379E-03, 3.19562E-03, 3.45113E-03, bhh12100
     X 3.75149E-03, 3.98520E-03, 4.19181E-03, 4.15773E-03, 4.02490E-03, bhh12110
     X 3.95936E-03, 3.79001E-03, 3.77647E-03, 3.48528E-03, 3.55768E-03, bhh12120
     X 3.62812E-03, 3.48650E-03, 3.35434E-03, 3.20088E-03, 3.25316E-03, bhh12130
     X 3.04467E-03, 3.12633E-03, 3.23602E-03, 3.07723E-03, 2.80070E-03, bhh12140
     X 2.72498E-03, 2.74752E-03, 2.58943E-03, 2.32482E-03, 2.20218E-03, bhh12150
     X 2.10846E-03, 2.05991E-03, 2.01844E-03, 2.16224E-03, 2.48456E-03, bhh12160
     X 2.88022E-03, 2.93939E-03, 3.01176E-03, 2.98886E-03, 2.96947E-03, bhh12170
     X 3.38082E-03, 3.61657E-03, 3.42654E-03, 3.41274E-03, 3.22475E-03, bhh12180
     X 2.97658E-03, 3.21944E-03, 3.32032E-03, 3.33273E-03, 3.58854E-03, bhh12190
     X 3.67023E-03, 3.64069E-03, 3.74557E-03, 3.77703E-03, 3.64042E-03/ bhh12200
      DATA C10481 /                                                     bhh12210
     X 3.39468E-03, 3.22657E-03, 3.16466E-03, 3.24224E-03, 3.24801E-03, bhh12220
     X 3.19487E-03, 3.40155E-03, 3.16940E-03, 2.92293E-03, 3.00998E-03, bhh12230
     X 2.82851E-03, 2.60381E-03, 2.59242E-03, 2.48530E-03, 2.76677E-03, bhh12240
     X 2.45506E-03, 2.21845E-03, 2.30407E-03, 2.28136E-03, 2.37278E-03, bhh12250
     X 2.25313E-03, 2.47836E-03, 2.77858E-03, 2.89803E-03, 2.86131E-03, bhh12260
     X 3.14118E-03, 3.14119E-03, 2.88881E-03, 3.19502E-03, 2.99538E-03, bhh12270
     X 2.91212E-03, 3.22739E-03, 3.05960E-03, 3.18901E-03, 3.05805E-03, bhh12280
     X 3.12205E-03, 2.95636E-03, 3.24111E-03, 3.29433E-03, 3.09206E-03, bhh12290
     X 3.06696E-03, 2.97735E-03, 2.90897E-03, 2.88979E-03, 2.75105E-03, bhh12300
     X 2.92156E-03, 3.03445E-03, 2.91664E-03, 2.85559E-03, 2.98405E-03, bhh12310
     X 2.95376E-03, 2.80234E-03, 2.78349E-03, 2.73421E-03, 2.70035E-03, bhh12320
     X 2.60074E-03, 2.34840E-03, 2.37626E-03, 2.32927E-03, 2.20842E-03, bhh12330
     X 2.31080E-03, 2.42771E-03, 2.43339E-03, 2.53280E-03, 2.37093E-03, bhh12340
     X 2.37377E-03, 2.73453E-03, 2.60836E-03, 2.55568E-03, 2.44062E-03, bhh12350
     X 2.71093E-03, 2.64421E-03, 2.66969E-03, 2.55560E-03, 2.71800E-03, bhh12360
     X 2.79534E-03, 2.59070E-03, 2.55373E-03, 2.45272E-03, 2.55571E-03/ bhh12370
      DATA C10561 /                                                     bhh12380
     X 2.54606E-03, 2.57349E-03, 2.46807E-03, 2.35634E-03, 2.44470E-03, bhh12390
     X 2.47050E-03, 2.57131E-03, 2.71649E-03, 2.58800E-03, 2.54524E-03, bhh12400
     X 2.69505E-03, 2.89122E-03, 2.77399E-03, 2.63306E-03, 2.82269E-03, bhh12410
     X 2.95684E-03, 3.07415E-03, 2.70594E-03, 2.65650E-03, 2.90613E-03, bhh12420
     X 2.96666E-03, 2.94767E-03, 2.81765E-03, 2.64829E-03, 2.43062E-03, bhh12430
     X 2.33816E-03, 2.38210E-03, 2.45701E-03, 2.38508E-03, 2.40746E-03, bhh12440
     X 2.49779E-03, 2.28209E-03, 2.26185E-03, 2.26604E-03, 2.19232E-03, bhh12450
     X 2.19160E-03, 2.32246E-03, 2.11108E-03, 2.26220E-03, 2.26849E-03, bhh12460
     X 2.34787E-03, 2.49323E-03, 2.46872E-03, 2.52974E-03, 2.35858E-03, bhh12470
     X 2.36865E-03, 2.33533E-03, 2.21338E-03, 2.24610E-03, 2.24776E-03, bhh12480
     X 2.24423E-03, 2.29276E-03, 2.18487E-03, 2.27621E-03, 2.31141E-03, bhh12490
     X 2.44095E-03, 2.45198E-03, 2.56919E-03, 2.56823E-03, 2.41982E-03, bhh12500
     X 2.39968E-03, 2.62447E-03, 2.55339E-03, 2.51556E-03, 2.47477E-03, bhh12510
     X 2.50276E-03, 2.48381E-03, 2.48484E-03, 2.48316E-03, 2.38541E-03, bhh12520
     X 2.41183E-03, 2.55888E-03, 2.42810E-03, 2.43356E-03, 2.25996E-03, bhh12530
     X 2.34736E-03, 2.10305E-03, 2.13870E-03, 2.17472E-03, 2.05354E-03/ bhh12540
      DATA C10641 /                                                     bhh12550
     X 2.11572E-03, 2.19557E-03, 2.09545E-03, 2.07831E-03, 1.94425E-03, bhh12560
     X 1.89333E-03, 1.98025E-03, 1.98328E-03, 2.01702E-03, 1.98333E-03, bhh12570
     X 2.01150E-03, 2.02484E-03, 2.10759E-03, 2.11892E-03, 2.10175E-03, bhh12580
     X 2.05314E-03, 2.13338E-03, 2.25764E-03, 2.19055E-03, 2.10818E-03, bhh12590
     X 2.05100E-03, 2.05685E-03, 2.10843E-03, 2.10228E-03, 2.10646E-03, bhh12600
     X 2.22640E-03, 2.31253E-03, 2.31230E-03, 2.21885E-03, 2.19568E-03, bhh12610
     X 2.23583E-03, 2.34754E-03, 2.28622E-03, 2.21876E-03, 2.26679E-03, bhh12620
     X 2.30828E-03, 2.24944E-03, 2.13851E-03, 2.02938E-03, 1.96770E-03, bhh12630
     X 2.05953E-03, 2.13814E-03, 2.03158E-03, 2.24655E-03, 1.95119E-03, bhh12640
     X 2.12979E-03, 2.08581E-03, 2.02434E-03, 1.98926E-03, 1.98792E-03, bhh12650
     X 1.97237E-03, 1.93397E-03, 1.92360E-03, 1.90805E-03, 1.89300E-03, bhh12660
     X 1.83548E-03, 1.87215E-03, 1.85589E-03, 1.85718E-03, 1.79361E-03, bhh12670
     X 1.77984E-03, 1.91506E-03, 2.04256E-03, 2.04095E-03, 1.94031E-03, bhh12680
     X 1.90447E-03, 2.02049E-03, 1.98360E-03, 2.04364E-03, 2.02519E-03, bhh12690
     X 2.20802E-03, 1.96964E-03, 1.94559E-03, 2.09922E-03, 2.11184E-03, bhh12700
     X 2.05706E-03, 2.02257E-03, 2.01781E-03, 2.01055E-03, 1.86538E-03/ bhh12710
      DATA C10721 /                                                     bhh12720
     X 1.86899E-03, 1.76798E-03, 1.85871E-03, 1.95363E-03, 1.96404E-03, bhh12730
     X 1.84169E-03, 1.82851E-03, 1.84582E-03, 1.81997E-03, 1.76461E-03, bhh12740
     X 1.68384E-03, 1.65530E-03, 1.73550E-03, 1.62463E-03, 1.68793E-03, bhh12750
     X 1.60472E-03, 1.67560E-03, 1.67431E-03, 1.61779E-03, 1.66446E-03, bhh12760
     X 1.66403E-03, 1.55724E-03, 1.62351E-03, 1.71545E-03, 1.69645E-03, bhh12770
     X 1.59540E-03, 1.62948E-03, 1.66784E-03, 1.66416E-03, 1.66131E-03, bhh12780
     X 1.71502E-03, 1.76555E-03, 1.75182E-03, 1.72327E-03, 1.72338E-03, bhh12790
     X 1.69993E-03, 1.78819E-03, 1.73517E-03, 1.74802E-03, 1.81751E-03, bhh12800
     X 1.70973E-03, 1.65075E-03, 1.70784E-03, 1.73655E-03, 1.71670E-03, bhh12810
     X 1.67367E-03, 1.69338E-03, 1.61772E-03, 1.54914E-03, 1.56009E-03, bhh12820
     X 1.59467E-03, 1.60761E-03, 1.57117E-03, 1.54045E-03, 1.53102E-03, bhh12830
     X 1.44516E-03, 1.49898E-03, 1.56048E-03, 1.60087E-03, 1.62636E-03, bhh12840
     X 1.62472E-03, 1.53931E-03, 1.55536E-03, 1.61649E-03, 1.66493E-03, bhh12850
     X 1.86915E-03, 1.59984E-03, 1.60483E-03, 1.66549E-03, 1.73449E-03, bhh12860
     X 1.73673E-03, 1.68393E-03, 1.67434E-03, 1.77880E-03, 1.76154E-03, bhh12870
     X 1.43028E-03, 1.69651E-03, 1.60934E-03, 1.69413E-03, 1.70514E-03/ bhh12880
      DATA C10801 /                                                     bhh12890
     X 1.62471E-03, 1.74854E-03, 1.76480E-03, 1.63495E-03, 1.59364E-03, bhh12900
     X 1.39603E-03, 1.47897E-03, 1.49509E-03, 1.70002E-03, 1.63048E-03, bhh12910
     X 1.44807E-03, 1.45071E-03, 1.53998E-03, 1.45276E-03, 1.29129E-03, bhh12920
     X 1.52900E-03, 1.64444E-03, 1.37450E-03, 1.42574E-03, 1.47355E-03, bhh12930
     X 1.51202E-03, 1.54376E-03, 1.51421E-03, 1.43989E-03, 1.45732E-03, bhh12940
     X 1.42912E-03, 1.59906E-03, 1.56748E-03, 1.52383E-03, 1.47665E-03, bhh12950
     X 1.51465E-03, 1.55582E-03, 1.54521E-03, 1.55189E-03, 1.56772E-03, bhh12960
     X 1.45401E-03, 1.55775E-03, 1.43120E-03, 1.39659E-03, 1.41451E-03, bhh12970
     X 1.45157E-03, 1.48303E-03, 1.42540E-03, 1.26387E-03, 1.37479E-03, bhh12980
     X 1.46381E-03, 1.38134E-03, 1.32733E-03, 1.38030E-03, 1.44619E-03, bhh12990
     X 1.41344E-03, 1.31982E-03, 1.24944E-03, 1.20096E-03, 1.21107E-03, bhh13000
     X 1.27999E-03, 1.22523E-03, 1.22193E-03, 1.35957E-03, 1.41427E-03, bhh13010
     X 1.35679E-03, 1.15438E-03, 1.41184E-03, 1.49093E-03, 1.32193E-03, bhh13020
     X 1.25009E-03, 1.37625E-03, 1.49022E-03, 1.44180E-03, 1.27628E-03, bhh13030
     X 1.29670E-03, 1.31636E-03, 1.28874E-03, 1.31177E-03, 1.35732E-03, bhh13040
     X 1.33854E-03, 1.30253E-03, 1.31374E-03, 1.27379E-03, 1.18339E-03/ bhh13050
      DATA C10881 /                                                     bhh13060
     X 1.22016E-03, 1.26551E-03, 1.26371E-03, 1.28180E-03, 1.36024E-03, bhh13070
     X 1.45759E-03, 1.29413E-03, 1.35858E-03, 1.26528E-03, 1.18623E-03, bhh13080
     X 1.21812E-03, 1.28799E-03, 1.37028E-03, 1.29268E-03, 1.27639E-03, bhh13090
     X 1.19487E-03, 1.23542E-03, 1.25010E-03, 1.17418E-03, 1.13914E-03, bhh13100
     X 1.21951E-03, 1.13780E-03, 1.16443E-03, 1.17883E-03, 1.11982E-03, bhh13110
     X 1.05708E-03, 1.04865E-03, 1.05884E-03, 1.06599E-03, 1.13828E-03, bhh13120
     X 1.10373E-03, 1.07739E-03, 1.04632E-03, 1.06118E-03, 1.15445E-03, bhh13130
     X 1.17300E-03, 1.00675E-03, 1.04235E-03, 1.08398E-03, 1.06587E-03, bhh13140
     X 1.05536E-03, 1.08614E-03, 1.09026E-03, 1.09141E-03, 1.13051E-03, bhh13150
     X 1.08667E-03, 1.04016E-03, 1.04897E-03, 1.08894E-03, 1.09682E-03, bhh13160
     X 1.09638E-03, 9.79254E-04, 1.00668E-03, 1.02569E-03, 1.00581E-03, bhh13170
     X 9.74433E-04, 9.66321E-04, 9.78440E-04, 9.01587E-04, 1.02149E-03, bhh13180
     X 9.87464E-04, 9.41872E-04, 9.05021E-04, 8.59547E-04, 9.03963E-04, bhh13190
     X 8.66415E-04, 8.84726E-04, 8.77087E-04, 8.70584E-04, 8.81338E-04, bhh13200
     X 8.97658E-04, 8.97586E-04, 9.19028E-04, 8.82438E-04, 9.00710E-04, bhh13210
     X 9.54329E-04, 9.54490E-04, 9.10940E-04, 9.95472E-04, 9.50134E-04/ bhh13220
      DATA C10961 /                                                     bhh13230
     X 9.17127E-04, 9.70916E-04, 9.87575E-04, 9.65026E-04, 9.71779E-04, bhh13240
     X 1.00967E-03, 1.00053E-03, 9.26063E-04, 9.34721E-04, 9.76354E-04, bhh13250
     X 9.78436E-04, 9.36012E-04, 9.64448E-04, 9.95903E-04, 9.89960E-04, bhh13260
     X 9.41143E-04, 9.04393E-04, 8.84719E-04, 8.41396E-04, 8.67234E-04, bhh13270
     X 8.55864E-04, 8.63314E-04, 8.72317E-04, 8.40899E-04, 7.79593E-04, bhh13280
     X 7.88481E-04, 8.21075E-04, 7.38342E-04, 7.56537E-04, 7.57278E-04, bhh13290
     X 7.35854E-04, 7.32765E-04, 6.67398E-04, 7.45338E-04, 7.33094E-04, bhh13300
     X 7.01840E-04, 6.85595E-04, 6.95740E-04, 7.24015E-04, 7.00907E-04, bhh13310
     X 7.28498E-04, 6.89410E-04, 6.91728E-04, 7.40601E-04, 7.62775E-04, bhh13320
     X 7.40912E-04, 7.35021E-04, 7.07799E-04, 7.54113E-04, 8.44845E-04, bhh13330
     X 8.53956E-04, 6.42186E-04, 7.40557E-04, 7.54340E-04, 7.55544E-04, bhh13340
     X 7.88986E-04, 7.97902E-04, 6.98460E-04, 7.74873E-04, 6.81178E-04, bhh13350
     X 7.15567E-04, 7.56723E-04, 7.98438E-04, 8.83150E-04, 8.45671E-04, bhh13360
     X 7.40924E-04, 7.35498E-04, 7.77829E-04, 6.93566E-04, 5.10188E-04, bhh13370
     X 7.52717E-04, 6.94185E-04, 6.71928E-04, 6.73286E-04, 6.89415E-04, bhh13380
     X 7.22917E-04, 7.89448E-04, 8.53812E-04, 7.45132E-04, 7.68732E-04/ bhh13390
      DATA C11041 /                                                     bhh13400
     X 8.10104E-04, 7.55615E-04, 7.09145E-04, 6.80676E-04, 7.54594E-04, bhh13410
     X 7.89416E-04, 7.88579E-04, 7.49805E-04, 6.13534E-04, 7.22491E-04, bhh13420
     X 7.95410E-04, 7.80604E-04, 7.74283E-04, 7.93224E-04, 6.86522E-04, bhh13430
     X 8.06038E-04, 8.30285E-04, 8.37763E-04, 8.03863E-04, 7.33526E-04, bhh13440
     X 7.42588E-04, 6.31046E-04, 8.16153E-04, 8.95391E-04, 8.61330E-04, bhh13450
     X 8.38726E-04, 8.16761E-04, 8.16118E-04, 6.37058E-04, 6.30868E-04, bhh13460
     X 7.26410E-04, 7.03464E-04, 5.93454E-04, 6.01985E-04, 6.51157E-04, bhh13470
     X 6.68569E-04, 6.56297E-04, 6.58732E-04, 5.99721E-04, 5.34301E-04, bhh13480
     X 5.33271E-04, 5.57992E-04, 5.70096E-04, 5.59932E-04, 5.32110E-04, bhh13490
     X 5.64713E-04, 6.25026E-04, 6.38973E-04, 6.05323E-04, 7.17460E-04, bhh13500
     X 6.19407E-04, 5.90228E-04, 5.43682E-04, 5.38446E-04, 6.56146E-04, bhh13510
     X 6.09081E-04, 6.04737E-04, 6.45526E-04, 6.46978E-04, 5.89738E-04, bhh13520
     X 5.63852E-04, 6.18018E-04, 5.71768E-04, 5.75433E-04, 6.05766E-04, bhh13530
     X 5.93065E-04, 5.31708E-04, 5.41187E-04, 5.76985E-04, 5.78176E-04, bhh13540
     X 5.75339E-04, 6.85426E-04, 5.51038E-04, 6.02049E-04, 6.20406E-04, bhh13550
     X 5.80169E-04, 5.36399E-04, 5.59608E-04, 5.46575E-04, 5.66979E-04/ bhh13560
      DATA C11121 /                                                     bhh13570
     X 5.94982E-04, 6.18469E-04, 6.56281E-04, 8.22124E-04, 7.81716E-04, bhh13580
     X 7.29616E-04, 7.14460E-04, 7.08969E-04, 6.53794E-04, 7.33138E-04, bhh13590
     X 8.29513E-04, 8.99395E-04, 9.05526E-04, 7.98257E-04, 7.86935E-04, bhh13600
     X 6.10797E-04, 4.63912E-04, 4.05675E-04, 3.66230E-04, 4.86472E-04, bhh13610
     X 5.31818E-04, 5.15865E-04, 4.87344E-04, 4.99857E-04, 5.35479E-04, bhh13620
     X 5.27561E-04, 4.99000E-04, 4.77056E-04, 4.74242E-04, 4.66595E-04, bhh13630
     X 4.66325E-04, 4.94704E-04, 5.12842E-04, 5.01795E-04, 4.80789E-04, bhh13640
     X 5.73709E-04, 5.65214E-04, 5.11321E-04, 4.55242E-04, 4.29330E-04, bhh13650
     X 5.09792E-04, 4.70489E-04, 4.82859E-04, 4.99195E-04, 4.07724E-04, bhh13660
     X 4.99951E-04, 4.55755E-04, 4.42528E-04, 4.19433E-04, 3.31325E-04, bhh13670
     X 3.70517E-04, 3.77708E-04, 2.97923E-04, 2.27470E-04, 2.47389E-04, bhh13680
     X 2.38324E-04, 2.56706E-04, 2.45046E-04, 2.62539E-04, 3.37054E-04, bhh13690
     X 3.33930E-04, 3.01390E-04, 3.08028E-04, 3.41464E-04, 3.70574E-04, bhh13700
     X 3.47893E-04, 3.28433E-04, 3.46976E-04, 3.60351E-04, 3.50559E-04, bhh13710
     X 3.56070E-04, 3.62782E-04, 3.37330E-04, 3.33763E-04, 3.57046E-04, bhh13720
     X 3.08784E-04, 2.93898E-04, 2.80842E-04, 2.54114E-04, 2.38198E-04/ bhh13730
      DATA C11201 /                                                     bhh13740
     X 3.48753E-04, 2.97334E-04, 2.82929E-04, 2.94150E-04, 3.07875E-04, bhh13750
     X 3.21129E-04, 3.38335E-04, 3.49826E-04, 3.47647E-04, 3.35438E-04, bhh13760
     X 3.58145E-04, 3.72391E-04, 3.59372E-04, 3.64755E-04, 4.16867E-04, bhh13770
     X 3.43614E-04, 3.34932E-04, 3.12782E-04, 3.28220E-04, 4.32595E-04, bhh13780
     X 3.49513E-04, 3.51861E-04, 3.81166E-04, 3.91194E-04, 3.38944E-04, bhh13790
     X 2.63445E-04, 2.49520E-04, 2.46184E-04, 2.33203E-04, 2.16315E-04, bhh13800
     X 1.89536E-04, 1.95730E-04, 1.99664E-04, 1.77139E-04, 1.27969E-04, bhh13810
     X 5.17216E-05, 7.60445E-05, 1.24418E-04, 1.30989E-04, 2.31539E-04, bhh13820
     X 2.21334E-04, 2.08757E-04, 2.18351E-04, 2.46202E-04, 2.29824E-04, bhh13830
     X 2.28909E-04, 2.88826E-04, 3.58039E-04, 2.60800E-04, 2.33025E-04, bhh13840
     X 2.52667E-04, 2.61394E-04, 2.31384E-04, 2.29388E-04, 2.54701E-04, bhh13850
     X 2.21158E-04, 1.61506E-04, 1.36752E-04, 1.69481E-04, 8.64539E-05, bhh13860
     X 1.64407E-04, 3.65674E-04, 3.18233E-04, 4.00755E-04, 3.33375E-04, bhh13870
     X 2.62930E-04, 2.87052E-04, 2.51395E-04, 2.85274E-04, 2.66915E-04, bhh13880
     X 2.10866E-04, 1.89517E-04, 1.67378E-04, 2.79951E-04, 2.97224E-04, bhh13890
     X 1.89222E-04, 3.33825E-04, 3.56386E-04, 3.89727E-04, 4.30407E-04/ bhh13900
      DATA C11281 /                                                     bhh13910
     X 4.45922E-04, 4.23446E-04, 4.41347E-04, 4.06723E-04, 3.00181E-04, bhh13920
     X 1.85243E-04, 3.13176E-04, 4.08991E-04, 4.24776E-04, 3.56412E-04, bhh13930
     X 3.84760E-04, 2.30602E-04, 1.77702E-04, 2.62329E-04, 2.49442E-04, bhh13940
     X 3.76212E-04, 3.69176E-04, 2.97681E-04, 2.71662E-04, 2.05694E-04, bhh13950
     X 2.11418E-04, 2.25439E-04, 2.27013E-04, 2.47845E-04, 3.14603E-04, bhh13960
     X 2.68802E-04, 2.04334E-04, 2.77399E-04, 2.68273E-04, 2.04991E-04, bhh13970
     X 2.24441E-04, 3.55074E-04, 2.90135E-04, 3.35680E-04, 3.59358E-04, bhh13980
     X 3.44716E-04, 3.24496E-04, 3.48146E-04, 3.49042E-04, 3.54848E-04, bhh13990
     X 3.86418E-04, 3.59198E-04, 3.47608E-04, 3.20522E-04, 2.78401E-04, bhh14000
     X 2.64579E-04, 2.23694E-04, 2.34370E-04, 2.52559E-04, 1.88475E-04, bhh14010
     X 2.01258E-04, 1.63979E-04, 1.45384E-04, 1.91215E-04, 1.76958E-04, bhh14020
     X 1.69167E-04, 1.71767E-04, 1.86595E-04, 2.14969E-04, 2.48345E-04, bhh14030
     X 2.46691E-04, 2.25234E-04, 2.26755E-04, 1.64112E-04, 1.87750E-04, bhh14040
     X 2.22984E-04, 2.00443E-04, 2.38863E-04, 2.77590E-04, 2.91953E-04, bhh14050
     X 2.80611E-04, 3.08215E-04, 1.79095E-04, 1.46920E-04, 2.29177E-04, bhh14060
     X 2.54685E-04, 2.68866E-04, 2.13346E-04, 1.20122E-04, 5.55240E-05/ bhh14070
      DATA C11361 /                                                     bhh14080
     X 5.99017E-05, 1.07768E-04, 1.67810E-04, 2.06886E-04, 2.36232E-04, bhh14090
     X 2.24598E-04, 2.30792E-04, 2.71274E-04, 1.29062E-04, 1.92624E-04, bhh14100
     X 2.38438E-04, 1.98994E-04, 1.81687E-04, 2.55733E-04, 2.84379E-04, bhh14110
     X 2.54459E-04, 2.30884E-04, 2.68873E-04, 3.07231E-04, 3.15063E-04, bhh14120
     X 2.46725E-04, 2.60370E-04, 2.66391E-04, 2.50708E-04, 2.04296E-04, bhh14130
     X 1.66011E-04, 1.19164E-04, 1.06700E-04, 1.77576E-04, 1.91741E-04, bhh14140
     X 1.66618E-04, 1.49824E-04, 1.80699E-04, 2.20905E-04, 1.38754E-04, bhh14150
     X 6.27971E-05, 7.52567E-05, 1.89995E-04, 1.72489E-04, 1.40424E-04, bhh14160
     X 1.52384E-04, 1.63942E-04, 1.19901E-04, 1.49234E-04, 2.68313E-04, bhh14170
     X 2.08815E-04, 1.17218E-04, 1.42235E-04, 2.71237E-04, 1.38192E-04, bhh14180
     X 2.15643E-04, 2.84476E-04, 2.78117E-04, 2.19234E-04, 1.59128E-04, bhh14190
     X 1.78819E-04, 2.67785E-04, 2.66786E-04, 2.58545E-04, 2.68476E-04, bhh14200
     X 2.88542E-04, 2.59726E-04, 3.00936E-04, 3.11237E-04, 2.61275E-04, bhh14210
     X 1.37136E-04, 2.76566E-04, 3.82888E-04, 3.97564E-04, 4.43655E-04, bhh14220
     X 3.15415E-04, 2.60869E-04, 3.19171E-04, 3.34205E-04, 2.02914E-04, bhh14230
     X 1.16223E-04, 1.14737E-04, 6.10978E-05,-8.03695E-06,-1.07062E-05/ bhh14240
      DATA C11441 /                                                     bhh14250
     X 6.50664E-05, 1.12586E-04, 1.56727E-04, 1.57927E-04, 1.05762E-04, bhh14260
     X 1.03646E-04, 1.72520E-04, 2.23668E-04, 2.12775E-04, 2.33525E-04, bhh14270
     X 2.75558E-04, 2.34256E-04, 5.10062E-05, 1.76007E-04, 1.70850E-04, bhh14280
     X 1.43266E-04, 1.89626E-04, 2.97283E-04, 3.02773E-04, 2.74401E-04, bhh14290
     X 3.00754E-04, 3.66813E-04, 3.54383E-04, 2.90580E-04, 2.32206E-04, bhh14300
     X 1.58405E-04, 1.54663E-04, 1.84598E-04, 1.26408E-04, 2.14481E-04, bhh14310
     X 2.00791E-04, 1.05796E-04, 2.39794E-04, 1.66105E-04, 7.88615E-05, bhh14320
     X 4.30615E-05, 7.37518E-05, 1.24926E-04, 1.38295E-04, 8.54356E-05, bhh14330
     X 6.12641E-05, 6.54466E-05, 6.17727E-05, 1.30688E-05, 6.00462E-05, bhh14340
     X 1.52612E-04, 2.11656E-04, 9.67692E-05, 8.67858E-05, 1.34888E-04, bhh14350
     X 1.90899E-04, 1.03234E-04, 1.03837E-04, 1.49767E-04, 2.19058E-04, bhh14360
     X 2.26549E-04, 2.11506E-04, 1.85238E-04, 1.53774E-04, 1.32313E-04, bhh14370
     X 6.10658E-05, 2.37782E-05, 1.24450E-04, 1.87610E-04, 1.44775E-04, bhh14380
     X 5.60937E-05, 6.64032E-05, 1.28073E-04, 1.77512E-04, 1.84684E-04, bhh14390
     X 5.73677E-05, 5.29679E-05, 9.95510E-05, 1.61423E-04, 3.19036E-04, bhh14400
     X 3.17383E-04, 2.36505E-04, 1.80844E-04, 1.63722E-04, 1.21478E-04/ bhh14410
      DATA C11521 /                                                     bhh14420
     X 6.85823E-05, 7.42058E-05, 1.14838E-04, 1.21131E-04, 8.01009E-05, bhh14430
     X 1.52058E-04, 2.18368E-04, 2.53416E-04, 2.27116E-04, 1.25336E-04, bhh14440
     X 6.26421E-05, 5.32471E-05, 1.34705E-04, 2.07005E-05,-5.18630E-05, bhh14450
     X-3.25696E-05,-8.06171E-05,-1.09430E-04,-1.05637E-04,-4.96066E-05, bhh14460
     X-7.76138E-05,-4.85930E-05, 3.65111E-06,-2.86933E-05,-4.61366E-05, bhh14470
     X-4.88820E-05,-3.08816E-05, 8.43778E-05, 1.40484E-04, 1.31125E-04, bhh14480
     X 3.55198E-05, 8.47412E-05, 1.23408E-04, 1.36799E-04, 1.21147E-04, bhh14490
     X 1.25585E-04, 1.32337E-04, 1.34092E-04, 1.26652E-04, 1.12131E-04, bhh14500
     X 1.00927E-04, 1.13828E-04, 1.06053E-04, 9.43643E-05, 8.33628E-05, bhh14510
     X 8.65842E-05, 7.59315E-05, 8.28623E-05, 1.39681E-04, 1.80492E-04, bhh14520
     X 1.65779E-04, 1.03843E-04, 3.10284E-05, 1.94408E-05, 4.57525E-05, bhh14530
     X 1.02436E-04, 1.39750E-04, 1.43342E-04, 1.11999E-04, 2.94197E-05, bhh14540
     X 2.76980E-05, 5.51685E-05, 9.39909E-05, 1.16108E-04, 7.72703E-05, bhh14550
     X 4.37409E-05, 1.13925E-04, 8.18872E-05, 2.87657E-05,-2.41413E-05, bhh14560
     X 1.24699E-05, 2.19589E-05,-5.88247E-06,-9.66151E-05,-2.06255E-05, bhh14570
     X-1.83148E-06,-5.63625E-05,-8.65590E-05,-8.26020E-05,-5.06239E-05/ bhh14580
      DATA C11601 /                                                     bhh14590
     X 1.28065E-05,-1.34669E-05, 1.59701E-05, 9.44755E-05, 1.63032E-05, bhh14600
     X 2.51304E-05, 7.38226E-05, 1.28405E-04, 1.17413E-04, 9.92387E-05, bhh14610
     X 9.51533E-05, 2.17008E-04, 2.25854E-04, 1.90448E-04, 1.77207E-04, bhh14620
     X 1.80844E-04, 1.53501E-04, 9.80430E-05, 1.27404E-04, 1.16465E-04, bhh14630
     X 9.98611E-05, 1.25556E-04, 1.73627E-04, 1.12347E-04,-7.73523E-05, bhh14640
     X 5.66599E-05, 5.36347E-05, 1.20227E-06, 6.96325E-05, 4.79010E-05, bhh14650
     X-1.09886E-05,-9.16457E-05,-7.09170E-05,-5.31410E-05,-2.68376E-05, bhh14660
     X 6.32641E-05, 8.06052E-06,-4.99262E-05,-2.56644E-05,-8.76854E-05, bhh14670
     X-8.21360E-05,-5.02403E-06, 4.66629E-05, 6.93127E-05, 5.53828E-05, bhh14680
     X-2.32399E-05,-2.07514E-05,-7.33240E-05,-2.10483E-04,-1.53757E-04, bhh14690
     X-7.13861E-05,-1.07356E-05,-1.26578E-04,-7.48854E-05, 3.25418E-06, bhh14700
     X 2.97068E-05, 3.35685E-05, 3.15022E-05, 2.68904E-05, 3.87401E-05, bhh14710
     X 5.12522E-05, 5.12172E-05, 1.05053E-05, 1.65321E-05, 3.47537E-05, bhh14720
     X 5.62503E-05, 4.18666E-05, 3.13970E-05, 3.11750E-05, 7.21547E-05, bhh14730
     X 2.55262E-05,-2.76061E-05, 5.43449E-06,-5.20575E-05,-1.08627E-04, bhh14740
     X-1.40475E-04,-1.59926E-04,-1.32237E-04,-8.15458E-05,-1.31738E-04/ bhh14750
      DATA C11681 /                                                     bhh14760
     X-1.64036E-04,-1.69351E-04,-1.24797E-04,-1.61950E-04,-2.01904E-04, bhh14770
     X-2.22995E-04,-1.87647E-04,-1.70817E-04,-1.64583E-04,-1.12811E-04, bhh14780
     X-8.38306E-05,-8.62707E-05,-1.54362E-04,-1.98090E-04,-2.12920E-04, bhh14790
     X-1.89358E-04,-2.02988E-04,-1.72791E-04,-1.02863E-04,-1.09877E-04, bhh14800
     X-1.04257E-04,-8.20734E-05,-2.18346E-05,-2.94593E-05,-4.18226E-05, bhh14810
     X-1.86891E-05,-6.14620E-05,-3.21912E-05, 1.00844E-04, 6.92419E-05, bhh14820
     X 3.16713E-05, 5.62042E-07, 5.18900E-05, 7.48835E-05, 8.03381E-05, bhh14830
     X 7.24685E-05, 9.55588E-05, 9.22801E-05, 2.87159E-05, 2.26234E-05, bhh14840
     X 2.62790E-05, 3.58332E-05, 6.23297E-05, 5.01998E-05, 1.81446E-05, bhh14850
     X 3.33564E-05, 3.97765E-06,-2.60624E-05, 7.01802E-06,-4.16797E-05, bhh14860
     X-8.70108E-05,-8.22182E-05,-6.64886E-05,-7.88704E-05,-1.28305E-04, bhh14870
     X-1.29990E-04,-1.12646E-04,-8.68394E-05,-1.29584E-04,-1.44352E-04, bhh14880
     X-1.42082E-04,-1.33790E-04,-1.27963E-04,-1.21233E-04,-1.09965E-04, bhh14890
     X-1.02233E-04,-1.03804E-04,-1.19503E-04,-7.74707E-05,-4.66805E-05, bhh14900
     X-3.52201E-05,-4.07406E-05,-4.66887E-05,-5.05962E-05,-3.30333E-05, bhh14910
     X-3.47981E-05,-3.60962E-05, 1.44242E-05, 4.10478E-05, 3.68984E-05/ bhh14920
      DATA C11761 /                                                     bhh14930
     X-2.81300E-05, 2.83171E-05, 7.48062E-05, 4.29333E-05, 8.50076E-06, bhh14940
     X 4.98135E-06, 4.44854E-05, 2.51860E-05, 3.12189E-05, 6.39424E-05, bhh14950
     X 7.20715E-05, 9.89688E-05, 1.33768E-04, 1.07781E-04, 9.76731E-05, bhh14960
     X 9.21479E-05, 6.72624E-05, 5.41295E-05, 4.89022E-05, 5.28039E-05, bhh14970
     X-4.48737E-06,-5.15409E-05,-3.57396E-05,-1.94752E-05,-2.09521E-05, bhh14980
     X-5.13096E-05,-2.62781E-05,-2.75451E-05,-6.98423E-05,-1.25462E-04, bhh14990
     X-1.68362E-04,-1.97456E-04,-1.90669E-04,-2.06890E-04,-2.36699E-04, bhh15000
     X-1.97732E-04,-1.76504E-04,-1.67505E-04,-1.60694E-04,-1.85851E-04, bhh15010
     X-2.01567E-04,-9.82507E-05,-1.33338E-04,-1.95199E-04,-1.40781E-04, bhh15020
     X-8.90988E-05,-3.63239E-05, 2.16510E-05,-1.56807E-05,-4.21285E-05, bhh15030
     X 5.50505E-06, 6.78937E-07, 3.12346E-06, 3.64202E-05, 3.50651E-05, bhh15040
     X 6.20423E-05, 1.38667E-04, 7.74738E-05, 6.77036E-05, 1.38367E-04, bhh15050
     X 1.17359E-04, 1.06637E-04, 1.12404E-04, 9.78586E-05, 1.03178E-04, bhh15060
     X 1.28717E-04, 1.56642E-04, 1.62544E-04, 1.50109E-04, 1.43214E-04, bhh15070
     X 1.33651E-04, 1.24352E-04, 1.41420E-04, 1.36340E-04, 1.18769E-04, bhh15080
     X 1.31656E-04, 8.81533E-05, 1.55214E-05,-3.68736E-07,-1.76213E-05/ bhh15090
      DATA C11841 /                                                     bhh15100
     X-2.85341E-05, 4.65155E-06, 5.41350E-06,-7.01247E-06, 6.57918E-06, bhh15110
     X-2.45784E-05,-6.89104E-05,-6.90953E-05,-6.23937E-05,-6.72978E-05, bhh15120
     X-1.39547E-04,-1.44228E-04,-1.42543E-04,-2.31080E-04,-2.12756E-04, bhh15130
     X-1.62089E-04,-1.66063E-04,-1.61872E-04,-1.59764E-04,-1.80217E-04, bhh15140
     X-1.38355E-04,-8.45661E-05,-7.58308E-05,-4.65144E-05,-2.76855E-05, bhh15150
     X-7.48714E-05,-8.28561E-05,-6.45277E-05,-7.08509E-06,-1.05566E-05, bhh15160
     X-1.96352E-05, 3.55561E-05, 2.24676E-05,-1.25648E-05,-1.87661E-05, bhh15170
     X 6.99061E-06, 2.33676E-05,-5.25111E-05,-3.86758E-05, 1.03585E-06, bhh15180
     X-1.65901E-05,-1.04855E-05, 5.03694E-06, 1.25937E-05,-8.31340E-06, bhh15190
     X-4.37906E-05,-7.91444E-05,-4.62167E-05, 5.14238E-06,-4.52863E-05, bhh15200
     X-5.86455E-05,-4.98093E-05,-3.03495E-05,-5.09377E-05,-8.88116E-05, bhh15210
     X-6.21360E-05,-7.38148E-05,-1.07502E-04,-7.55276E-05,-6.39257E-05, bhh15220
     X-6.86921E-05,-8.05504E-05,-9.24178E-05,-1.03991E-04,-1.00468E-04, bhh15230
     X-6.71447E-05,-3.84897E-06,-5.99067E-06,-2.21894E-05,-5.21766E-05, bhh15240
     X-3.93796E-05,-4.06712E-05,-6.21649E-05,-1.13073E-04,-1.20560E-04, bhh15250
     X-5.92397E-05, 5.24432E-05, 9.41628E-05,-3.47458E-07, 5.33267E-05/ bhh15260
      DATA C11921 /                                                     bhh15270
     X 8.92961E-05, 2.75694E-05,-7.48460E-06,-2.15504E-05, 1.05501E-06, bhh15280
     X 6.30910E-06, 5.94620E-07,-2.45194E-05,-1.59657E-05, 7.93610E-07, bhh15290
     X-1.05319E-05,-2.36584E-05,-3.95700E-05,-6.57225E-05,-5.23797E-05, bhh15300
     X-1.82588E-05,-1.43240E-05,-3.29989E-05,-6.48909E-05,-2.41326E-05, bhh15310
     X-1.89195E-05,-4.64607E-05,-1.00739E-05,-1.35033E-05,-6.49945E-05, bhh15320
     X-5.19986E-05,-6.68505E-05,-1.31530E-04,-1.45464E-04,-1.46815E-04, bhh15330
     X-1.39684E-04,-1.23205E-04,-1.26738E-04,-1.93822E-04,-2.37508E-04, bhh15340
     X-2.52917E-04,-1.91110E-04,-1.36217E-04,-9.41093E-05,-1.20601E-04, bhh15350
     X-1.17295E-04,-9.57420E-05,-1.57227E-04,-1.62795E-04,-1.12201E-04, bhh15360
     X-1.20419E-04,-1.10597E-04,-7.61223E-05,-6.27167E-05,-5.54733E-05, bhh15370
     X-5.50437E-05,-5.14148E-05,-3.59591E-05, 1.09906E-05, 5.94396E-06, bhh15380
     X-1.38597E-05,-8.80857E-06,-3.13101E-05,-6.31715E-05,-4.04264E-05, bhh15390
     X-1.66405E-05, 7.94396E-06,-3.41772E-06,-4.03175E-05,-1.06888E-04, bhh15400
     X-9.50526E-05,-7.46111E-05,-5.09617E-05,-6.70981E-05,-7.93529E-05, bhh15410
     X-5.58423E-05,-1.01523E-04,-1.62269E-04,-1.69958E-04,-1.37786E-04, bhh15420
     X-8.79862E-05,-1.46838E-04,-1.66938E-04,-1.51380E-04,-1.62184E-04/ bhh15430
      DATA C12001 /                                                     bhh15440
     X-1.61105E-04,-1.42088E-04,-1.57033E-04,-1.65294E-04,-1.45079E-04, bhh15450
     X-9.76982E-05,-6.09891E-05,-1.01719E-04,-1.03049E-04,-8.85546E-05, bhh15460
     X-1.47754E-04,-1.44542E-04,-8.34620E-05,-8.99440E-05,-7.11901E-05, bhh15470
     X-1.57480E-05,-8.81797E-05,-1.56314E-04,-1.65952E-04,-1.80986E-04, bhh15480
     X-2.04610E-04,-2.58669E-04,-2.16016E-04,-1.21582E-04,-1.44929E-04, bhh15490
     X-1.72886E-04,-2.05950E-04,-1.93829E-04,-1.67518E-04,-1.22969E-04, bhh15500
     X-1.13060E-04,-1.14854E-04,-1.26198E-04,-1.24288E-04,-1.19519E-04, bhh15510
     X-1.50456E-04,-1.53286E-04,-1.32231E-04,-7.42672E-05,-2.23129E-05, bhh15520
     X 1.79115E-05, 1.42073E-05,-1.21676E-05,-7.56567E-05,-1.03423E-04, bhh15530
     X-1.10373E-04,-8.77244E-05,-6.43485E-05,-4.05156E-05,-6.24405E-05, bhh15540
     X-5.70375E-05,-2.36695E-06,-3.75929E-05,-7.97119E-05,-6.70419E-05, bhh15550
     X-6.99475E-05,-8.19748E-05,-1.06895E-04,-1.31422E-04,-1.55438E-04, bhh15560
     X-1.61937E-04,-1.62626E-04,-1.54977E-04,-1.77814E-04,-2.00386E-04, bhh15570
     X-1.87407E-04,-2.07243E-04,-2.44672E-04,-2.19014E-04,-2.13695E-04, bhh15580
     X-2.32440E-04,-1.85194E-04,-1.51172E-04,-1.69834E-04,-1.73780E-04, bhh15590
     X-1.75232E-04,-2.00698E-04,-1.82826E-04,-1.27786E-04,-1.33633E-04/ bhh15600
      DATA C12081 /                                                     bhh15610
     X-1.21317E-04,-7.50390E-05,-1.06743E-04,-1.40805E-04,-1.06336E-04, bhh15620
     X-9.46654E-05,-9.78182E-05,-1.19906E-04,-1.14160E-04,-7.28186E-05, bhh15630
     X-1.07652E-04,-1.20978E-04,-3.79658E-05,-3.16113E-05,-6.02417E-05, bhh15640
     X-7.51148E-05,-5.56145E-05,-6.77421E-06,-1.74321E-05,-4.67952E-05, bhh15650
     X-1.05000E-04,-6.29932E-05,-4.74356E-06,-2.83397E-05,-4.65192E-05, bhh15660
     X-6.04574E-05,-4.33970E-05,-3.18311E-05,-3.02321E-05,-4.49667E-05, bhh15670
     X-6.85347E-05,-1.11375E-04,-1.16293E-04,-9.38757E-05,-1.38594E-04, bhh15680
     X-1.60483E-04,-1.48344E-04,-1.33436E-04,-1.27387E-04,-1.59508E-04, bhh15690
     X-1.74026E-04,-1.72170E-04,-1.49196E-04,-1.33233E-04,-1.22382E-04, bhh15700
     X-1.78156E-04,-2.21349E-04,-2.41846E-04,-2.06549E-04,-1.68283E-04, bhh15710
     X-1.89512E-04,-1.44523E-04,-4.67953E-05,-1.00334E-04,-1.23478E-04, bhh15720
     X-8.14024E-05,-9.18016E-05,-1.17536E-04,-1.36160E-04,-1.38780E-04, bhh15730
     X-1.27749E-04,-1.45598E-04,-1.55964E-04,-1.45120E-04,-1.25544E-04, bhh15740
     X-1.05692E-04,-1.17639E-04,-1.24142E-04,-1.24749E-04,-1.63878E-04, bhh15750
     X-1.97021E-04,-1.98617E-04,-2.69136E-04,-3.68357E-04,-2.33702E-04, bhh15760
     X-1.61830E-04,-1.78578E-04,-2.01839E-04,-2.28731E-04,-2.63606E-04/ bhh15770
      DATA C12161 /                                                     bhh15780
     X-2.44698E-04,-1.86451E-04,-2.20546E-04,-2.22752E-04,-1.55169E-04, bhh15790
     X-1.25100E-04,-1.09794E-04,-9.59016E-05,-1.03857E-04,-1.35573E-04, bhh15800
     X-1.73780E-04,-1.82457E-04,-9.39821E-05,-1.18245E-04,-2.11563E-04, bhh15810
     X-1.37392E-04,-9.28173E-05,-9.71073E-05,-9.72535E-05,-9.39557E-05, bhh15820
     X-7.50117E-05,-6.70754E-05,-7.01186E-05,-5.76151E-05,-5.18785E-05, bhh15830
     X-7.14209E-05,-7.01682E-05,-5.61614E-05,-8.92769E-05,-1.06238E-04, bhh15840
     X-9.70294E-05,-6.70229E-05,-4.69214E-05,-1.53105E-04,-2.02326E-04, bhh15850
     X-1.90395E-04,-2.04367E-04,-2.16787E-04,-2.08725E-04,-1.78119E-04, bhh15860
     X-1.31043E-04,-1.32204E-04,-1.51522E-04,-2.05143E-04,-1.77144E-04, bhh15870
     X-1.16130E-04,-1.44440E-04,-1.66010E-04,-1.78206E-04,-1.61163E-04, bhh15880
     X-1.46351E-04,-1.96722E-04,-2.27027E-04,-2.37243E-04,-2.25235E-04, bhh15890
     X-1.99552E-04,-1.40238E-04,-1.26311E-04,-1.42746E-04,-1.19028E-04, bhh15900
     X-1.18750E-04,-1.72076E-04,-1.72120E-04,-1.48285E-04,-1.85116E-04, bhh15910
     X-1.98602E-04,-1.74016E-04,-1.37913E-04,-1.01221E-04,-9.69581E-05, bhh15920
     X-1.08794E-04,-1.39433E-04,-1.38575E-04,-1.32088E-04,-1.37431E-04, bhh15930
     X-1.30033E-04,-1.10829E-04,-1.35604E-04,-1.66515E-04,-1.98167E-04/ bhh15940
      DATA C12241 /                                                     bhh15950
     X-1.97716E-04,-1.74019E-04,-1.64719E-04,-1.64779E-04,-1.85725E-04, bhh15960
     X-2.28526E-04,-2.84329E-04,-1.82449E-04,-1.30747E-04,-1.93620E-04, bhh15970
     X-2.28529E-04,-2.47361E-04,-1.90001E-04,-1.66278E-04,-2.02540E-04, bhh15980
     X-2.31811E-04,-2.53772E-04,-2.08629E-04,-1.85021E-04,-1.93989E-04, bhh15990
     X-2.16568E-04,-2.38288E-04,-1.94453E-04,-1.87154E-04,-2.30493E-04, bhh16000
     X-2.34696E-04,-2.30351E-04,-2.60562E-04,-2.86427E-04,-3.06699E-04, bhh16010
     X-2.79131E-04,-2.49392E-04,-3.03389E-04,-3.10346E-04,-2.61782E-04, bhh16020
     X-2.30905E-04,-2.11669E-04,-2.37680E-04,-2.38194E-04,-2.10955E-04/ bhh16030
      END                                                               bhh16040
      BLOCK DATA BO3HH2                                                 bhh2 100
C>    BLOCK DATA                                                        bhh2 110
C                                                                       bhh2 120
C     RATIO (C2/C0)                                                     bhh2 130
C     DATA FROM BASS 1985                                               bhh2 140
C                                                                       bhh2 150
C     NOW INCLUDES MOLINA & MOLINA AT 273K WITH THE TEMPERATURE         bhh2 160
C     DEPENDENCE DETERMINED FROM THE 195K HARVARD MEASUREMENTS,         bhh2 170
C     EMPLOYING THE BASS ALGORITHM (CO(1+C1*T+C2*T2); THIS IS           bhh2 180
C     ONLY FOR THE WAVELENGTH RANGE FROM .34 TO .35 MICRONS;            bhh2 190
C     OTHERWISE, THE BASS DATA ALONE HAVE BEEN EMPLOYED BETWEEN         bhh2 200
C     .34 AND .245 MICRONS.                                             bhh2 210
C                                                                       bhh2 220
C     NEW T-DEPENDENT X-SECTIONS BETWEEN .345 AND .36 MICRONS           bhh2 230
C     HAVE NOW BEEN ADDED, BASED ON WORK BY CACCIANI, DISARRA           bhh2 240
C     AND FIOCCO, UNIVERSITY OF ROME, 1987.  QUADRATIC TEMP             bhh2 250
C     HAS BEEN DERIVED, AS ABOVE.                                       bhh2 260
C                                                                       bhh2 270
C     AGREEMENT AMONGST THE FOUR DATA SETS IS REASONABLE (<10%)         bhh2 280
C     AND OFTEN EXCELLENT (0-3%)                                        bhh2 290
C                                                                       bhh2 300
C                                                                       bhh2 310
      COMMON /O3HH2/  V1C,V2C,DVC,NC,                                   bhh2 320
     X           O32001(85),C20086(80),C20166(80),C20246(65),C20311(16),bhh2 330
     X           C20327(80),C20407(1),                                  bhh2 340
     X           C20001(80),C20081(80),C20161(80),C20241(80),C20321(80),bhh2 350
     X           C20401(80),C20481(80),C20561(80),C20641(80),C20721(80),bhh2 360
     X           C20801(80),C20881(80),C20961(80),C21041(80),C21121(80),bhh2 370
     X           C21201(80),C21281(80),C21361(80),C21441(80),C21521(80),bhh2 380
     X           C21601(80),C21681(80),C21761(80),C21841(80),C21921(80),bhh2 390
     X           C22001(80),C22081(80),C22161(80),C22241(40)            bhh2 400
C                                                                       bhh2 410
C     DATA V1C /29405./, V2C /40800./ ,DVC /5./, NC /2280/   BASS       bhh2 420
      DATA V1C /27370./, V2C /40800./ ,DVC /5./, NC /2687/              bhh2 430
C                                                                       bhh2 440
      DATA O32001/85*1.0E-5/                                            bhh2 450
                                                                        bhh2 460
      DATA C20086/                                                      bhh2 470
     X 1.29359E-05, 1.55806E-05, 2.00719E-05, 2.64912E-05, 3.48207E-05, bhh2 480
     X 4.36986E-05, 5.31318E-05, 6.13173E-05, 6.89465E-05, 7.56793E-05, bhh2 490
     X 8.26345E-05, 8.90916E-05, 9.38759E-05, 9.22998E-05, 9.03184E-05, bhh2 500
     X 8.65369E-05, 8.58531E-05, 8.55635E-05, 8.40418E-05, 8.11983E-05, bhh2 510
     X 7.58246E-05, 7.29282E-05, 7.32629E-05, 7.04060E-05, 6.71451E-05, bhh2 520
     X 6.56515E-05, 6.68943E-05, 6.32785E-05, 5.88386E-05, 5.70860E-05, bhh2 530
     X 5.64435E-05, 5.49441E-05, 5.70845E-05, 5.89357E-05, 6.14433E-05, bhh2 540
     X 5.91790E-05, 5.31727E-05, 5.14007E-05, 4.74318E-05, 4.35356E-05, bhh2 550
     X 3.93903E-05, 3.70963E-05, 3.63867E-05, 4.05296E-05, 4.48891E-05, bhh2 560
     X 5.37190E-05, 5.70440E-05, 4.60408E-05, 5.25778E-05, 6.81728E-05, bhh2 570
     X 7.27275E-05, 6.81353E-05, 6.48386E-05, 5.46521E-05, 4.93098E-05, bhh2 580
     X 5.04438E-05, 5.30309E-05, 5.28788E-05, 5.47387E-05, 4.52523E-05, bhh2 590
     X 5.29451E-05, 7.42215E-05, 1.08971E-04, 1.40085E-04, 1.46553E-04, bhh2 600
     X 1.43526E-04, 1.39051E-04, 1.40983E-04, 1.45564E-04, 1.55589E-04, bhh2 610
     X 1.66142E-04, 1.82840E-04, 2.06486E-04, 2.24339E-04, 2.29268E-04, bhh2 620
     X 2.13109E-04, 2.00305E-04, 1.99955E-04, 2.18566E-04, 2.24182E-04/ bhh2 630
      DATA C20166/                                                      bhh2 640
     X 2.33505E-04, 2.31824E-04, 2.22666E-04, 2.23905E-04, 2.38131E-04, bhh2 650
     X 2.54322E-04, 2.69548E-04, 2.62953E-04, 2.67609E-04, 2.70567E-04, bhh2 660
     X 2.70689E-04, 2.68251E-04, 2.66029E-04, 2.60053E-04, 2.61689E-04, bhh2 670
     X 2.56582E-04, 2.43655E-04, 2.38792E-04, 2.45309E-04, 2.31061E-04, bhh2 680
     X 2.22837E-04, 2.16440E-04, 2.19032E-04, 1.85634E-04, 1.74638E-04, bhh2 690
     X 1.51767E-04, 1.38480E-04, 1.32506E-04, 1.28317E-04, 1.26855E-04, bhh2 700
     X 1.27123E-04, 1.24040E-04, 1.19202E-04, 1.28649E-04, 1.36271E-04, bhh2 710
     X 1.42080E-04, 1.47804E-04, 1.39534E-04, 1.27284E-04, 1.09554E-04, bhh2 720
     X 8.69470E-05, 6.72096E-05, 5.23407E-05, 5.12433E-05, 5.15794E-05, bhh2 730
     X 4.94683E-05, 4.95809E-05, 4.07499E-05, 3.14984E-05, 1.46457E-05, bhh2 740
     X 6.98660E-06, 1.85313E-05, 5.48879E-05, 1.09447E-04, 1.52536E-04, bhh2 750
     X 1.78778E-04, 1.91128E-04, 1.99161E-04, 2.02937E-04, 1.95527E-04, bhh2 760
     X 1.92214E-04, 1.83376E-04, 1.81710E-04, 1.82283E-04, 1.75182E-04, bhh2 770
     X 1.72406E-04, 1.68170E-04, 1.67400E-04, 1.69469E-04, 1.69092E-04, bhh2 780
     X 1.65985E-04, 1.66912E-04, 1.74226E-04, 1.85036E-04, 1.85517E-04, bhh2 790
     X 1.85805E-04, 1.73809E-04, 1.67628E-04, 1.57690E-04, 1.54952E-04/ bhh2 800
      DATA C20246/                                                      bhh2 810
     X 1.53707E-04, 1.57710E-04, 1.58175E-04, 1.67253E-04, 1.82079E-04, bhh2 820
     X 1.91285E-04, 1.96564E-04, 2.03822E-04, 1.93736E-04, 1.82924E-04, bhh2 830
     X 1.73610E-04, 1.69904E-04, 1.66725E-04, 1.63747E-04, 1.63129E-04, bhh2 840
     X 1.62435E-04, 1.67218E-04, 1.69507E-04, 1.70744E-04, 1.65839E-04, bhh2 850
     X 1.72077E-04, 1.67734E-04, 1.51487E-04, 1.43770E-04, 1.37435E-04, bhh2 860
     X 1.25172E-04, 1.12395E-04, 1.07991E-04, 1.00345E-04, 9.36611E-05, bhh2 870
     X 9.59763E-05, 9.26600E-05, 1.00120E-04, 1.04746E-04, 1.10222E-04, bhh2 880
     X 1.03308E-04, 8.97457E-05, 7.91634E-05, 7.50275E-05, 8.30832E-05, bhh2 890
     X 1.01191E-04, 1.21560E-04, 1.34840E-04, 1.38712E-04, 1.41746E-04, bhh2 900
     X 1.39578E-04, 1.37052E-04, 1.33850E-04, 1.26641E-04, 1.21342E-04, bhh2 910
     X 1.17669E-04, 1.25973E-04, 1.33623E-04, 1.33839E-04, 1.24427E-04, bhh2 920
     X 1.02462E-04, 8.76101E-05, 8.27912E-05, 8.29040E-05, 7.78590E-05, bhh2 930
     X 7.39042E-05, 6.45765E-05, 5.70151E-05, 5.11846E-05, 4.83163E-05/ bhh2 940
      DATA C20311/                                                      bhh2 950
     X                                                     5.4470E-05,  bhh2 960
     X 5.3312E-05,  5.3135E-05,  5.3619E-05,  5.3686E-05,  5.2308E-05,  bhh2 970
     X 5.0441E-05,  4.8402E-05,  4.7476E-05,  4.6215E-05,  4.4507E-05,  bhh2 980
     X 4.3830E-05,  4.0508E-05,  3.8931E-05,  3.5525E-05,  3.4722E-05/  bhh2 990
      DATA C20327/                                                      bhh21000
     X 3.2743E-05,  2.8456E-05,  2.8318E-05,  2.8132E-05,  2.6221E-05,  bhh21010
     X 2.5673E-05,  2.5521E-05,  2.4588E-05,  2.4093E-05,  2.2787E-05,  bhh21020
     X 2.1241E-05,  1.8553E-05,  1.5871E-05,  1.3462E-05,  1.2553E-05,  bhh21030
     X 1.6276E-05,  2.8296E-05,  3.8817E-05,  4.2733E-05,  4.2429E-05,  bhh21040
     X 4.0954E-05,  3.9868E-05,  3.7669E-05,  3.6312E-05,  3.5535E-05,  bhh21050
     X 3.5895E-05,  3.6349E-05,  3.9033E-05,  4.4512E-05,  5.0066E-05,  bhh21060
     X 5.4572E-05,  5.6710E-05,  5.6615E-05,  5.7520E-05,  5.8034E-05,  bhh21070
     X 5.7927E-05,  5.6027E-05,  5.5242E-05,  5.4974E-05,  5.2927E-05,  bhh21080
     X 5.1638E-05,  5.2027E-05,  5.1420E-05,  5.1618E-05,  5.0253E-05,  bhh21090
     X 5.0509E-05,  4.9376E-05,  5.0135E-05,  4.9191E-05,  4.9210E-05,  bhh21100
     X 4.8216E-05,  4.7487E-05,  4.5749E-05,  4.5884E-05,  4.3852E-05,  bhh21110
     X 4.3824E-05,  4.2612E-05,  4.0349E-05,  4.0177E-05,  3.7474E-05,  bhh21120
     X 3.8120E-05,  3.6915E-05,  3.5823E-05,  3.5186E-05,  3.3638E-05,  bhh21130
     X 3.3451E-05,  3.2428E-05,  3.2349E-05,  3.0183E-05,  2.8436E-05,  bhh21140
     X 2.6440E-05,  2.3597E-05,  2.1875E-05,  1.8164E-05,  1.6430E-05,  bhh21150
     X 1.3159E-05,  9.2907E-06,  7.4243E-06,  6.0469E-06,  5.4951E-06/  bhh21160
      DATA C20407/                                                      bhh21170
     X 8.7642E-06/                                                      bhh21180
      DATA C20001 /                                                     bhh21190
     X 2.16295E-05, 1.69111E-05, 5.39633E-05, 1.01866E-04, 8.28657E-05, bhh21200
     X 9.16593E-05, 8.88666E-05, 1.37764E-04, 1.44322E-04, 1.20659E-04, bhh21210
     X 1.10332E-04, 1.01317E-04, 9.09964E-05, 1.17148E-04, 1.18000E-04, bhh21220
     X 7.21801E-05, 1.10550E-04, 1.32672E-04, 1.02474E-04, 1.10434E-04, bhh21230
     X 1.38759E-04, 8.92135E-05, 9.18239E-05, 9.08256E-05, 7.02969E-05, bhh21240
     X 1.12827E-04, 8.25561E-05, 1.39555E-04, 6.72239E-05, 7.82804E-05, bhh21250
     X 8.56258E-05, 8.61068E-05, 7.16732E-05, 6.25720E-05, 5.23957E-05, bhh21260
     X 3.78801E-05, 4.37281E-05, 4.99821E-05, 5.96976E-05, 7.19070E-05, bhh21270
     X 3.89579E-05, 5.30171E-05, 3.92507E-05, 4.93901E-05, 4.53047E-05, bhh21280
     X 4.89955E-05, 4.61649E-05, 3.75742E-05, 3.14124E-05, 2.37893E-05, bhh21290
     X 3.34899E-06, 3.08080E-05, 5.35883E-05, 3.39838E-05, 7.02334E-05, bhh21300
     X 7.24784E-05, 7.46533E-05, 6.22257E-05, 6.38945E-05, 6.73423E-05, bhh21310
     X 4.51321E-05, 5.91854E-05, 5.51601E-05, 4.41923E-05, 3.59217E-05, bhh21320
     X 4.08520E-05, 6.15981E-05, 6.66549E-05, 8.26031E-05, 1.13556E-04, bhh21330
     X 8.72988E-05, 9.71052E-05, 9.31839E-05, 8.73745E-05, 8.61717E-05, bhh21340
     X 6.05645E-05, 6.51131E-05, 6.93393E-05, 7.01096E-05, 6.43565E-05/ bhh21350
      DATA C20081 /                                                     bhh21360
     X 7.36929E-05, 7.66881E-05, 7.60815E-05, 7.13570E-05, 8.40487E-05, bhh21370
     X 8.51489E-05, 7.54168E-05, 6.72694E-05, 4.75508E-05, 3.59379E-05, bhh21380
     X 4.24698E-05, 4.17850E-05, 4.56047E-05, 4.12779E-05, 4.55933E-05, bhh21390
     X 4.27941E-05, 4.42230E-05, 3.68525E-05, 3.83392E-05, 3.83722E-05, bhh21400
     X 4.64904E-05, 3.33878E-05, 3.53027E-05, 3.54694E-05, 2.36233E-05, bhh21410
     X 2.99641E-05, 2.56097E-05, 2.14134E-05, 2.74403E-05, 2.83896E-05, bhh21420
     X 3.17082E-05, 1.75526E-05, 2.80382E-05, 3.18009E-05, 4.08715E-05, bhh21430
     X 4.77807E-05, 5.00609E-05, 5.12459E-05, 4.44062E-05, 4.74942E-05, bhh21440
     X 4.99882E-05, 5.18837E-05, 5.03246E-05, 5.55168E-05, 5.35853E-05, bhh21450
     X 4.81834E-05, 6.66231E-05, 5.26670E-05, 6.84700E-05, 6.53412E-05, bhh21460
     X 5.71740E-05, 4.61076E-05, 3.90239E-05, 4.72924E-05, 6.32194E-05, bhh21470
     X 5.20868E-05, 4.81039E-05, 3.71748E-05, 4.37492E-05, 3.63959E-05, bhh21480
     X 3.79823E-05, 3.72225E-05, 3.02360E-05, 3.22961E-05, 3.43398E-05, bhh21490
     X 3.57176E-05, 2.65446E-05, 3.29388E-05, 1.65455E-05, 2.66173E-05, bhh21500
     X 1.74277E-05, 1.74324E-05, 1.27879E-05, 1.46247E-05, 1.92378E-05, bhh21510
     X 2.20049E-05, 1.44790E-05, 2.49244E-05, 2.29209E-05, 1.76192E-05/ bhh21520
      DATA C20161 /                                                     bhh21530
     X 1.84528E-05, 2.54350E-05, 3.33972E-05, 3.69190E-05, 2.92139E-05, bhh21540
     X 2.47666E-05, 2.86764E-05, 1.48163E-05, 1.80461E-05, 2.84545E-05, bhh21550
     X 2.41064E-05, 2.85721E-05, 3.31996E-05, 3.75973E-05, 3.73874E-05, bhh21560
     X 4.69293E-05, 5.12665E-05, 5.35607E-05, 4.64577E-05, 3.59887E-05, bhh21570
     X 3.39168E-05, 3.89746E-05, 3.12196E-05, 3.70907E-05, 3.95172E-05, bhh21580
     X 4.61642E-05, 4.26029E-05, 4.17856E-05, 4.51437E-05, 4.04189E-05, bhh21590
     X 4.19251E-05, 4.53977E-05, 3.69860E-05, 4.20904E-05, 3.69735E-05, bhh21600
     X 3.57898E-05, 3.47729E-05, 3.14280E-05, 2.71197E-05, 3.34380E-05, bhh21610
     X 2.69843E-05, 2.88036E-05, 2.51912E-05, 2.45699E-05, 2.23184E-05, bhh21620
     X 2.50563E-05, 2.24493E-05, 1.77101E-05, 1.64763E-05, 1.34978E-05, bhh21630
     X 1.57081E-05, 1.45966E-05, 1.02722E-05, 2.07177E-05, 1.47662E-05, bhh21640
     X 1.50721E-05, 1.24431E-05, 1.51572E-05, 1.92210E-05, 2.06047E-05, bhh21650
     X 2.02921E-05, 3.22062E-05, 2.37112E-05, 1.94803E-05, 2.40726E-05, bhh21660
     X 2.11531E-05, 1.89158E-05, 2.46957E-05, 2.63175E-05, 2.57747E-05, bhh21670
     X 2.22047E-05, 2.52755E-05, 2.80848E-05, 3.75157E-05, 4.09915E-05, bhh21680
     X 4.04853E-05, 3.21661E-05, 3.15652E-05, 3.21576E-05, 3.67060E-05/ bhh21690
      DATA C20241 /                                                     bhh21700
     X 3.13071E-05, 2.84939E-05, 2.71169E-05, 2.99559E-05, 2.94631E-05, bhh21710
     X 3.26716E-05, 2.99028E-05, 2.60045E-05, 3.15375E-05, 3.12895E-05, bhh21720
     X 2.77767E-05, 2.43976E-05, 2.10764E-05, 2.22725E-05, 2.04581E-05, bhh21730
     X 1.63509E-05, 1.60028E-05, 1.60294E-05, 1.62366E-05, 1.89293E-05, bhh21740
     X 1.79675E-05, 1.89259E-05, 1.68300E-05, 1.99460E-05, 2.42370E-05, bhh21750
     X 2.64738E-05, 1.93137E-05, 1.39460E-05, 1.32222E-05, 1.38752E-05, bhh21760
     X 1.62071E-05, 1.79652E-05, 1.63772E-05, 1.56251E-05, 1.81918E-05, bhh21770
     X 1.46111E-05, 2.92174E-05, 2.94263E-05, 2.46180E-05, 2.93333E-05, bhh21780
     X 3.13657E-05, 2.97686E-05, 2.78387E-05, 2.40924E-05, 2.93369E-05, bhh21790
     X 2.93747E-05, 2.77665E-05, 3.00814E-05, 3.01068E-05, 3.62275E-05, bhh21800
     X 3.56613E-05, 3.66913E-05, 3.56280E-05, 3.52856E-05, 3.63928E-05, bhh21810
     X 2.96738E-05, 2.90314E-05, 2.62972E-05, 2.15250E-05, 1.97910E-05, bhh21820
     X 2.02314E-05, 2.20209E-05, 2.05131E-05, 2.12017E-05, 1.96689E-05, bhh21830
     X 1.61907E-05, 1.57662E-05, 1.58239E-05, 1.54650E-05, 1.46376E-05, bhh21840
     X 1.32891E-05, 1.30511E-05, 1.17635E-05, 1.28585E-05, 1.12887E-05, bhh21850
     X 1.32627E-05, 1.31833E-05, 1.68679E-05, 1.98092E-05, 2.70744E-05/ bhh21860
      DATA C20321 /                                                     bhh21870
     X 2.22033E-05, 1.63430E-05, 1.61104E-05, 1.50865E-05, 1.54382E-05, bhh21880
     X 1.55654E-05, 1.67924E-05, 1.89185E-05, 1.96791E-05, 2.14894E-05, bhh21890
     X 2.76137E-05, 2.67339E-05, 2.79423E-05, 2.54664E-05, 3.10707E-05, bhh21900
     X 2.72745E-05, 2.60940E-05, 2.47736E-05, 2.21105E-05, 2.20357E-05, bhh21910
     X 2.26499E-05, 2.34137E-05, 2.29537E-05, 2.36157E-05, 2.48244E-05, bhh21920
     X 2.26667E-05, 2.07781E-05, 2.11702E-05, 1.91214E-05, 1.62172E-05, bhh21930
     X 1.61285E-05, 1.63952E-05, 1.68156E-05, 1.61236E-05, 1.56611E-05, bhh21940
     X 1.47697E-05, 1.50856E-05, 1.44169E-05, 1.63816E-05, 1.74283E-05, bhh21950
     X 1.49853E-05, 1.62444E-05, 1.70007E-05, 1.60371E-05, 1.22713E-05, bhh21960
     X 1.45518E-05, 1.35051E-05, 1.40787E-05,-1.54925E-05,-2.15204E-05, bhh21970
     X-4.04516E-06, 2.22439E-05, 3.21262E-05, 3.83792E-05, 4.44462E-05, bhh21980
     X 4.44192E-05, 2.77328E-05, 4.10549E-06, 4.48758E-06,-1.27771E-05, bhh21990
     X-2.17204E-05,-1.23979E-05,-1.04928E-05, 7.43085E-06, 1.55350E-05, bhh22000
     X 3.15204E-05, 3.17601E-05, 2.93677E-05, 3.42485E-05, 3.87087E-05, bhh22010
     X 3.61242E-05, 2.62406E-05, 3.31686E-05, 3.54314E-05, 2.50625E-05, bhh22020
     X 2.60444E-05, 4.10729E-05, 3.47247E-05, 3.31716E-05, 3.34778E-05/ bhh22030
      DATA C20401 /                                                     bhh22040
     X 4.03029E-05, 4.09241E-05, 3.96717E-05, 3.53410E-05, 2.81048E-05, bhh22050
     X 1.98891E-05, 1.92314E-05, 2.82525E-05, 3.76641E-05, 4.34135E-05, bhh22060
     X 4.24570E-05, 3.98429E-05, 3.29417E-05, 2.16679E-05, 8.88085E-06, bhh22070
     X-5.05319E-06,-8.14815E-06,-5.01930E-06, 7.13565E-06, 2.00949E-05, bhh22080
     X 2.65988E-05, 2.77656E-05, 2.09299E-05, 1.98968E-05, 2.04835E-05, bhh22090
     X 1.75254E-05, 6.48674E-06, 3.14323E-06, 1.93242E-06, 3.86745E-06, bhh22100
     X 1.39727E-05, 2.10731E-05, 2.66432E-05, 2.69551E-05, 2.57453E-05, bhh22110
     X 2.72834E-05, 2.58860E-05, 2.51266E-05, 1.76048E-05, 2.03072E-05, bhh22120
     X 2.61960E-05, 2.36230E-05, 1.81172E-05, 1.33972E-05, 1.60959E-05, bhh22130
     X 1.61081E-05, 2.34099E-05, 2.64979E-05, 2.36894E-05, 2.13665E-05, bhh22140
     X 2.16774E-05, 2.52566E-05, 1.99785E-05, 1.40414E-05, 1.39948E-05, bhh22150
     X 1.32637E-05, 7.24742E-06, 1.11395E-06,-1.27323E-06, 4.56637E-07, bhh22160
     X 6.93250E-06, 5.07198E-06, 7.90632E-06, 9.08149E-06, 1.03602E-05, bhh22170
     X 2.17425E-05, 2.71741E-05, 2.16875E-05, 1.95088E-05, 1.56568E-05, bhh22180
     X 8.41152E-06, 1.26749E-05, 1.17673E-05, 9.96037E-06, 1.21982E-05, bhh22190
     X 1.31854E-05, 1.50216E-05, 1.72214E-05, 2.02773E-05, 2.09625E-05/ bhh22200
      DATA C20481 /                                                     bhh22210
     X 1.66656E-05, 1.45666E-05, 1.66608E-05, 2.04989E-05, 2.21395E-05, bhh22220
     X 2.35993E-05, 2.69390E-05, 2.13921E-05, 1.72643E-05, 1.70995E-05, bhh22230
     X 1.78241E-05, 1.85308E-05, 1.80360E-05, 1.48619E-05, 1.90369E-05, bhh22240
     X 1.51089E-05, 1.22705E-05, 1.62608E-05, 1.41637E-05, 1.23786E-05, bhh22250
     X 7.02677E-06, 8.89811E-06, 1.07379E-05, 1.23677E-05, 1.48196E-05, bhh22260
     X 2.05770E-05, 1.70994E-05, 1.00072E-05, 1.76119E-05, 1.41779E-05, bhh22270
     X 1.34358E-05, 1.58674E-05, 1.65837E-05, 1.69569E-05, 1.40381E-05, bhh22280
     X 1.46118E-05, 1.30556E-05, 1.97204E-05, 1.97488E-05, 1.64524E-05, bhh22290
     X 1.73764E-05, 1.66355E-05, 1.64419E-05, 1.65486E-05, 1.21523E-05, bhh22300
     X 1.51513E-05, 1.60354E-05, 1.38528E-05, 1.45538E-05, 1.71702E-05, bhh22310
     X 1.56336E-05, 1.31279E-05, 1.47346E-05, 1.70719E-05, 1.75588E-05, bhh22320
     X 1.55187E-05, 1.29598E-05, 1.38463E-05, 1.35382E-05, 1.16062E-05, bhh22330
     X 1.37014E-05, 1.34487E-05, 1.15536E-05, 1.33597E-05, 9.24478E-06, bhh22340
     X 7.28477E-06, 1.40321E-05, 1.31518E-05, 1.03118E-05, 8.59764E-06, bhh22350
     X 1.57138E-05, 1.20792E-05, 1.49440E-05, 1.34375E-05, 1.54686E-05, bhh22360
     X 1.65346E-05, 1.33823E-05, 1.37238E-05, 1.36128E-05, 1.46206E-05/ bhh22370
      DATA C20561 /                                                     bhh22380
     X 1.40777E-05, 1.59980E-05, 1.30180E-05, 1.01390E-05, 1.12366E-05, bhh22390
     X 9.86099E-06, 1.10702E-05, 1.26783E-05, 9.51072E-06, 8.07299E-06, bhh22400
     X 1.22955E-05, 1.53506E-05, 1.29711E-05, 9.78759E-06, 1.28800E-05, bhh22410
     X 1.39702E-05, 1.64832E-05, 1.06473E-05, 1.15419E-05, 1.63795E-05, bhh22420
     X 1.69837E-05, 1.72726E-05, 1.77231E-05, 1.62337E-05, 1.20881E-05, bhh22430
     X 1.13210E-05, 1.20531E-05, 1.31374E-05, 1.22259E-05, 1.27802E-05, bhh22440
     X 1.38962E-05, 8.87355E-06, 9.42264E-06, 1.02075E-05, 7.91816E-06, bhh22450
     X 9.66835E-06, 1.24921E-05, 8.43227E-06, 1.10637E-05, 1.03958E-05, bhh22460
     X 9.40996E-06, 1.22922E-05, 1.21088E-05, 1.30116E-05, 1.18776E-05, bhh22470
     X 1.42245E-05, 1.34745E-05, 1.11165E-05, 1.29914E-05, 1.29801E-05, bhh22480
     X 1.10895E-05, 1.12331E-05, 9.03490E-06, 9.33726E-06, 9.63923E-06, bhh22490
     X 1.11299E-05, 9.53481E-06, 1.21708E-05, 1.11951E-05, 7.22558E-06, bhh22500
     X 6.66928E-06, 1.08926E-05, 1.07870E-05, 9.23485E-06, 8.50452E-06, bhh22510
     X 9.41914E-06, 8.74027E-06, 8.93322E-06, 9.79061E-06, 8.26490E-06, bhh22520
     X 8.37630E-06, 1.17064E-05, 1.10176E-05, 1.11587E-05, 9.45563E-06, bhh22530
     X 1.18352E-05, 7.79327E-06, 9.22766E-06, 1.01868E-05, 8.23925E-06/ bhh22540
      DATA C20641 /                                                     bhh22550
     X 9.23706E-06, 1.04428E-05, 8.80392E-06, 9.37098E-06, 7.43126E-06, bhh22560
     X 7.01424E-06, 9.29360E-06, 8.97171E-06, 9.31718E-06, 9.87118E-06, bhh22570
     X 8.11419E-06, 8.77416E-06, 9.96927E-06, 8.87533E-06, 9.33163E-06, bhh22580
     X 7.41505E-06, 9.39988E-06, 1.17932E-05, 1.03287E-05, 9.17415E-06, bhh22590
     X 8.43035E-06, 8.00040E-06, 8.33346E-06, 7.66787E-06, 7.18411E-06, bhh22600
     X 1.06236E-05, 1.05559E-05, 8.49187E-06, 9.22472E-06, 8.16512E-06, bhh22610
     X 8.35687E-06, 1.06325E-05, 9.80273E-06, 9.01599E-06, 9.20499E-06, bhh22620
     X 9.98417E-06, 9.23191E-06, 6.98769E-06, 5.17748E-06, 4.57130E-06, bhh22630
     X 8.18492E-06, 9.98095E-06, 7.52148E-06, 1.33038E-05, 8.17630E-06, bhh22640
     X 1.02454E-05, 9.62706E-06, 9.44304E-06, 8.86704E-06, 8.88116E-06, bhh22650
     X 8.79062E-06, 8.20042E-06, 8.55789E-06, 9.26249E-06, 1.00467E-05, bhh22660
     X 7.96012E-06, 9.08773E-06, 1.01481E-05, 8.84360E-06, 7.94928E-06, bhh22670
     X 6.68425E-06, 8.56576E-06, 1.05282E-05, 1.10647E-05, 9.91625E-06, bhh22680
     X 7.95356E-06, 8.66443E-06, 9.13551E-06, 1.04870E-05, 9.79244E-06, bhh22690
     X 1.26214E-05, 8.42148E-06, 8.13468E-06, 1.11338E-05, 1.06780E-05, bhh22700
     X 8.54578E-06, 7.82119E-06, 8.33258E-06, 8.23644E-06, 5.95583E-06/ bhh22710
      DATA C20721 /                                                     bhh22720
     X 5.85592E-06, 4.05898E-06, 6.39260E-06, 8.43280E-06, 8.76251E-06, bhh22730
     X 6.70423E-06, 6.81368E-06, 7.43506E-06, 7.14376E-06, 6.51065E-06, bhh22740
     X 5.65633E-06, 5.42394E-06, 7.10817E-06, 4.78831E-06, 6.29380E-06, bhh22750
     X 4.87344E-06, 6.81764E-06, 6.51611E-06, 5.70526E-06, 6.50590E-06, bhh22760
     X 6.61568E-06, 5.39248E-06, 6.32002E-06, 7.98976E-06, 7.73795E-06, bhh22770
     X 4.85788E-06, 5.83443E-06, 6.11694E-06, 5.40408E-06, 5.00946E-06, bhh22780
     X 5.62153E-06, 6.30263E-06, 6.05764E-06, 5.53274E-06, 5.80664E-06, bhh22790
     X 5.18684E-06, 6.85555E-06, 6.22889E-06, 6.06959E-06, 6.49228E-06, bhh22800
     X 5.64064E-06, 4.92690E-06, 5.77661E-06, 7.18450E-06, 7.38658E-06, bhh22810
     X 6.77379E-06, 5.74668E-06, 6.68355E-06, 6.13655E-06, 6.43266E-06, bhh22820
     X 7.08896E-06, 7.71187E-06, 7.37273E-06, 6.75882E-06, 6.39307E-06, bhh22830
     X 4.59520E-06, 5.10323E-06, 5.80178E-06, 6.88172E-06, 6.68825E-06, bhh22840
     X 7.50416E-06, 6.14975E-06, 6.51422E-06, 7.74942E-06, 8.11492E-06, bhh22850
     X 1.19607E-05, 7.92722E-06, 4.47848E-06, 6.02524E-06, 9.74067E-06, bhh22860
     X 1.02429E-05, 8.60819E-06, 8.57044E-06, 1.09196E-05, 1.02048E-05, bhh22870
     X 3.86222E-06, 9.26104E-06, 7.33341E-06, 9.08181E-06, 1.05569E-05/ bhh22880
      DATA C20801 /                                                     bhh22890
     X 1.06776E-05, 1.10247E-05, 1.04520E-05, 8.78328E-06, 7.60679E-06, bhh22900
     X 7.27896E-06, 9.72776E-06, 5.16039E-06, 1.03134E-05, 1.09088E-05, bhh22910
     X 8.12575E-06, 7.61685E-06, 8.16346E-06, 5.91269E-06, 3.61448E-06, bhh22920
     X 8.74336E-06, 1.03990E-05, 6.25691E-06, 7.04541E-06, 7.94348E-06, bhh22930
     X 8.39807E-06, 8.67342E-06, 8.32173E-06, 7.56015E-06, 8.31782E-06, bhh22940
     X 6.36556E-06, 6.99328E-06, 6.24490E-06, 6.73080E-06, 6.95852E-06, bhh22950
     X 7.55508E-06, 7.74168E-06, 7.90414E-06, 8.94934E-06, 7.99809E-06, bhh22960
     X 6.12528E-06, 9.04115E-06, 7.14535E-06, 5.88625E-06, 6.43941E-06, bhh22970
     X 7.11566E-06, 7.47425E-06, 8.23805E-06, 6.19919E-06, 7.31614E-06, bhh22980
     X 8.24852E-06, 6.82172E-06, 5.45362E-06, 6.66115E-06, 8.44300E-06, bhh22990
     X 8.07530E-06, 7.22735E-06, 5.85614E-06, 5.13900E-06, 6.03215E-06, bhh23000
     X 6.59491E-06, 4.81592E-06, 4.48587E-06, 7.11525E-06, 8.36201E-06, bhh23010
     X 7.11669E-06, 2.80033E-06, 6.50756E-06, 9.43974E-06, 5.22402E-06, bhh23020
     X 3.82334E-06, 7.29963E-06, 8.62313E-06, 7.42018E-06, 4.56506E-06, bhh23030
     X 5.29972E-06, 5.62787E-06, 4.63852E-06, 5.18329E-06, 7.01884E-06, bhh23040
     X 7.24888E-06, 5.18157E-06, 5.40219E-06, 5.92412E-06, 4.97977E-06/ bhh23050
      DATA C20881 /                                                     bhh23060
     X 5.29040E-06, 5.33812E-06, 4.76620E-06, 4.65759E-06, 5.10546E-06, bhh23070
     X 6.49525E-06, 4.43416E-06, 5.30223E-06, 3.27044E-06, 2.55324E-06, bhh23080
     X 4.85017E-06, 7.46556E-06, 8.04448E-06, 5.14009E-06, 6.09755E-06, bhh23090
     X 5.38381E-06, 6.41959E-06, 6.59233E-06, 4.83160E-06, 3.81289E-06, bhh23100
     X 5.37013E-06, 5.69212E-06, 5.54983E-06, 5.73495E-06, 4.00639E-06, bhh23110
     X 2.33817E-06, 2.55751E-06, 3.29627E-06, 3.59845E-06, 6.20623E-06, bhh23120
     X 4.47088E-06, 3.49267E-06, 3.09273E-06, 3.32506E-06, 4.83353E-06, bhh23130
     X 6.39001E-06, 3.78074E-06, 4.07848E-06, 4.01811E-06, 3.19767E-06, bhh23140
     X 3.34053E-06, 4.34246E-06, 3.68003E-06, 3.01090E-06, 3.98545E-06, bhh23150
     X 2.72338E-06, 1.90024E-06, 2.77553E-06, 3.73381E-06, 2.58685E-06, bhh23160
     X 1.70987E-06,-5.48480E-07, 1.64591E-06, 2.43481E-06, 2.52116E-06, bhh23170
     X 2.19316E-06, 1.32392E-06, 1.75370E-06, 2.65409E-07, 2.22278E-06, bhh23180
     X 2.53079E-06, 2.87260E-06, 1.87600E-06,-3.84453E-07, 1.80836E-06, bhh23190
     X 9.28123E-07, 1.94986E-06, 2.40483E-06, 2.79865E-06, 2.86361E-06, bhh23200
     X 2.63868E-06, 3.34704E-06, 3.32132E-06, 2.58463E-06, 2.45684E-06, bhh23210
     X 3.35043E-06, 3.19848E-06, 1.73037E-06, 2.98206E-06, 2.77491E-06/ bhh23220
      DATA C20961 /                                                     bhh23230
     X 6.51674E-07, 2.52219E-06, 2.97136E-06, 1.96700E-06, 2.29350E-06, bhh23240
     X 3.01956E-06, 3.20811E-06, 1.30467E-06, 1.68172E-06, 2.56264E-06, bhh23250
     X 2.46167E-06, 1.78221E-06, 2.31647E-06, 2.69480E-06, 2.63619E-06, bhh23260
     X 1.81319E-06, 1.83448E-06, 2.23432E-06, 8.14045E-07, 8.75863E-07, bhh23270
     X 1.61350E-06, 1.59796E-06, 2.08419E-06, 1.89665E-06, 6.93584E-07, bhh23280
     X 1.09880E-06, 3.79031E-07,-3.36470E-07, 1.04326E-06, 1.06497E-06, bhh23290
     X 2.15108E-07, 3.28774E-07,-5.17613E-07, 1.27762E-06, 8.22924E-07, bhh23300
     X 4.92835E-07, 2.24698E-08,-1.99111E-07, 1.30262E-06,-3.81299E-07, bhh23310
     X 9.55084E-07, 2.17641E-07,-6.03874E-08, 8.44121E-07, 1.72391E-06, bhh23320
     X 1.66921E-06, 2.19855E-06, 1.17655E-06, 1.79637E-06, 3.31670E-06, bhh23330
     X 3.40206E-06, 6.05670E-07, 2.08299E-06, 2.10121E-06, 1.68598E-06, bhh23340
     X 2.21155E-06, 2.43221E-06, 5.81282E-08, 1.62613E-06,-5.49850E-07, bhh23350
     X 2.14143E-07, 1.21751E-06, 2.30470E-06, 4.27911E-06, 2.96622E-06, bhh23360
     X 8.67534E-07, 9.12041E-07, 2.48797E-06, 9.43519E-07,-3.60949E-06, bhh23370
     X 2.01928E-06, 1.88873E-06, 8.06749E-07, 7.33519E-07, 1.17440E-06, bhh23380
     X 1.69744E-06, 3.64492E-06, 3.11556E-06, 8.89471E-07, 1.93064E-06/ bhh23390
      DATA C21041 /                                                     bhh23400
     X 3.02787E-06, 1.92575E-06, 1.73720E-06,-1.32700E-07, 1.41743E-06, bhh23410
     X 2.24632E-06, 2.47945E-06, 2.05151E-06,-9.56031E-07, 2.57317E-07, bhh23420
     X 3.00980E-06, 3.07981E-06, 2.78202E-06, 3.02555E-06, 5.48784E-09, bhh23430
     X 2.37693E-06, 2.90011E-06, 2.93608E-06, 2.14837E-06, 6.55832E-07, bhh23440
     X 3.41155E-07,-2.13884E-06, 2.52553E-06, 4.27109E-06, 3.33766E-06, bhh23450
     X 3.07708E-06, 2.66405E-06, 3.22850E-06,-5.78879E-07,-6.06194E-07, bhh23460
     X 1.72864E-06, 1.57072E-06,-3.39701E-07, 7.21540E-08, 1.67012E-06, bhh23470
     X 2.48568E-06, 2.70214E-06, 3.62383E-06, 2.20408E-06, 1.19395E-06, bhh23480
     X 1.53825E-06, 2.37511E-06, 2.66754E-06, 1.77020E-06, 5.40420E-07, bhh23490
     X 2.01156E-06, 3.27498E-06, 3.04720E-06, 1.96213E-06, 3.71633E-06, bhh23500
     X 2.07886E-06, 1.60069E-06, 5.33370E-07, 1.33966E-07, 2.16073E-06, bhh23510
     X 8.81457E-07, 1.12880E-06, 2.40509E-06, 2.94252E-06, 2.22899E-06, bhh23520
     X 1.80941E-06, 2.68577E-06, 2.44584E-06, 2.51720E-06, 2.64857E-06, bhh23530
     X 2.24182E-06, 1.62007E-06, 2.60421E-06, 3.09782E-06, 3.11099E-06, bhh23540
     X 3.81513E-06, 6.91606E-06, 3.28767E-06, 3.44175E-06, 4.16771E-06, bhh23550
     X 3.75452E-06, 2.21050E-06, 2.99939E-06, 2.86993E-06, 2.47080E-06/ bhh23560
      DATA C21121 /                                                     bhh23570
     X 2.33607E-06, 2.68568E-06, 3.39344E-06, 6.09518E-06, 5.10422E-06, bhh23580
     X 4.04027E-06, 4.01363E-06, 4.53142E-06, 2.94424E-06, 4.76694E-06, bhh23590
     X 6.44206E-06, 7.86435E-06, 8.55564E-06, 6.00857E-06, 5.48073E-06, bhh23600
     X 1.56287E-06,-1.16619E-06,-1.85215E-06,-3.04762E-06,-3.45420E-07, bhh23610
     X 2.48111E-07,-1.39302E-07,-6.27593E-07,-5.26792E-07, 4.81454E-08, bhh23620
     X-3.08631E-08,-1.02976E-06,-1.54919E-06,-9.34044E-07,-1.02507E-06, bhh23630
     X-1.39794E-06,-1.15709E-06,-1.04875E-06,-1.64379E-06,-2.97514E-06, bhh23640
     X-3.22236E-07,-1.18978E-06,-2.85325E-06,-3.93143E-06,-4.15349E-06, bhh23650
     X-2.33228E-06,-3.27125E-06,-2.44987E-06,-1.44460E-06,-3.59727E-06, bhh23660
     X-7.18516E-07,-1.53237E-06,-1.53526E-06,-1.56450E-06,-2.91088E-06, bhh23670
     X-8.52134E-07,-1.44575E-07,-1.50350E-06,-2.92806E-06,-2.47710E-06, bhh23680
     X-9.71202E-07,-9.82754E-07,-1.09924E-06,-6.08199E-07, 3.62885E-07, bhh23690
     X-6.67372E-07,-1.00033E-06,-1.12001E-06,-1.06624E-06,-9.23789E-07, bhh23700
     X-9.83788E-07,-2.11656E-06,-2.45001E-06,-2.75874E-06,-3.36003E-06, bhh23710
     X-3.38364E-06,-2.63747E-06,-3.11047E-06,-3.75258E-06,-3.83211E-06, bhh23720
     X-3.52833E-06,-3.48464E-06,-3.77021E-06,-4.26887E-06,-4.23917E-06/ bhh23730
      DATA C21201 /                                                     bhh23740
     X-1.42438E-06,-2.48477E-06,-2.84719E-06,-2.70247E-06,-2.50588E-06, bhh23750
     X-2.22900E-06,-1.78393E-06,-1.76826E-06,-2.16396E-06,-2.67543E-06, bhh23760
     X-2.23706E-06,-2.31793E-06,-2.87590E-06,-3.07803E-06,-2.50493E-06, bhh23770
     X-4.54223E-06,-5.15511E-06,-5.39690E-06,-4.89633E-06,-3.33710E-06, bhh23780
     X-4.56583E-06,-4.78877E-06,-3.93508E-06,-3.29027E-06,-4.95668E-06, bhh23790
     X-6.01801E-06,-5.76016E-06,-5.34657E-06,-5.29080E-06,-5.57133E-06, bhh23800
     X-5.73135E-06,-5.39374E-06,-5.09808E-06,-5.12874E-06,-5.20269E-06, bhh23810
     X-7.30702E-06,-7.04220E-06,-5.96514E-06,-5.74802E-06,-4.53961E-06, bhh23820
     X-4.42127E-06,-4.63922E-06,-4.80622E-06,-4.69659E-06,-5.96786E-06, bhh23830
     X-6.29800E-06,-4.75452E-06,-2.85907E-06,-5.33662E-06,-5.31681E-06, bhh23840
     X-5.04646E-06,-5.21729E-06,-5.93409E-06,-5.73462E-06,-5.44926E-06, bhh23850
     X-6.43325E-06,-7.74451E-06,-7.83147E-06,-5.51568E-06,-7.37048E-06, bhh23860
     X-4.25726E-06, 2.32917E-06,-5.61131E-07, 2.05234E-06, 3.74631E-07, bhh23870
     X-7.66493E-07, 1.42689E-06,-7.79683E-07, 9.06809E-07, 5.13642E-07, bhh23880
     X-1.52504E-06,-2.12058E-06,-2.50316E-06, 1.03637E-08, 5.60002E-07, bhh23890
     X-1.48075E-06, 1.94155E-06, 1.91846E-06, 2.78507E-06, 3.90146E-06/ bhh23900
      DATA C21281 /                                                     bhh23910
     X 3.61409E-06, 3.23677E-06, 4.00022E-06, 3.19157E-06, 4.03034E-07, bhh23920
     X-2.03929E-06, 1.23366E-06, 3.28589E-06, 3.94168E-06, 3.94672E-06, bhh23930
     X 3.84619E-06, 2.30400E-07,-2.07799E-06,-1.75115E-06,-5.71958E-07, bhh23940
     X 2.33425E-06, 2.01664E-06, 6.05673E-07, 9.57363E-07,-8.89924E-07, bhh23950
     X-4.71331E-07, 2.82826E-07, 5.10859E-07, 3.63512E-07, 9.86288E-07, bhh23960
     X-4.86309E-07,-2.23163E-06,-1.23370E-06,-2.43131E-07,-2.11498E-06, bhh23970
     X-1.56756E-06, 2.70905E-06, 1.87606E-08, 7.83721E-08, 1.58444E-06, bhh23980
     X 2.88574E-06, 1.40306E-06, 2.40883E-06, 2.84063E-06, 3.13820E-06, bhh23990
     X 3.71016E-06, 3.12975E-06, 3.21981E-06, 2.56191E-06, 1.04624E-06, bhh24000
     X 1.87464E-07, 7.25329E-07, 1.03650E-06, 7.23663E-07,-4.18739E-07, bhh24010
     X 9.95744E-07,-1.80878E-07,-1.04044E-06, 3.86965E-07,-9.36186E-07, bhh24020
     X-4.02271E-07,-2.00231E-07,-5.94965E-07, 4.94038E-07, 3.34585E-07, bhh24030
     X 4.82255E-07, 1.12599E-06, 2.11763E-06, 2.66807E-07, 2.29324E-07, bhh24040
     X 7.07005E-07, 3.41907E-07,-1.17115E-07, 9.03089E-07, 1.76844E-06, bhh24050
     X 1.87134E-06, 2.64057E-06, 4.00395E-07,-4.19679E-07, 6.30769E-07, bhh24060
     X 1.02725E-06, 1.05876E-06,-4.08660E-07,-2.32668E-06,-2.73468E-06/ bhh24070
      DATA C21361 /                                                     bhh24080
     X-2.40600E-06,-1.81203E-06,-7.96431E-07, 7.40789E-07, 2.73188E-07, bhh24090
     X 1.68367E-07,-1.27227E-07,-1.05041E-06,-3.51726E-06,-1.64956E-06, bhh24100
     X-5.63840E-07,-1.61242E-06,-1.33264E-06, 1.56604E-06, 2.35083E-06, bhh24110
     X 9.26708E-07, 5.41983E-07, 3.54277E-07, 8.53743E-07, 1.54196E-06, bhh24120
     X 1.19902E-06, 1.10552E-06, 1.63179E-06, 1.96366E-06, 7.82848E-07, bhh24130
     X-3.34741E-08,-7.90842E-07,-6.45131E-07, 1.36158E-06, 1.62453E-06, bhh24140
     X 6.68965E-07,-4.86203E-08, 6.83561E-07, 1.89652E-06,-2.80988E-07, bhh24150
     X-2.30536E-06,-1.90777E-06, 1.31617E-06, 1.27309E-06, 5.90825E-07, bhh24160
     X 5.65686E-07, 1.23631E-07,-1.70279E-06,-1.60768E-06, 9.69543E-07, bhh24170
     X 1.01108E-07,-2.02473E-06,-1.75146E-06, 6.33201E-07,-3.59110E-06, bhh24180
     X-9.71706E-07, 9.16822E-07, 1.40681E-07,-7.16745E-07,-2.11376E-06, bhh24190
     X-1.00951E-06, 2.12465E-06, 1.06982E-06, 1.44032E-06, 1.49692E-06, bhh24200
     X 1.07277E-06, 1.37006E-06, 1.66932E-06, 1.75820E-06, 1.41859E-06, bhh24210
     X-5.84947E-08, 2.17349E-06, 4.27053E-06, 5.27286E-06, 5.87085E-06, bhh24220
     X 2.42692E-06, 2.39305E-06, 6.19573E-06, 5.12518E-06, 1.27171E-06, bhh24230
     X-6.81963E-07, 4.16199E-08,-1.36608E-06,-2.53272E-06,-2.37700E-06/ bhh24240
      DATA C21441 /                                                     bhh24250
     X-7.96719E-07, 3.85367E-07,-1.08393E-07,-9.04587E-07,-1.54917E-06, bhh24260
     X-3.11945E-06,-5.58484E-07, 1.61347E-06, 1.11736E-06, 2.11889E-06, bhh24270
     X 2.43534E-06, 1.46709E-06,-1.05429E-06, 1.09978E-06, 7.22493E-07, bhh24280
     X 8.53307E-08, 1.22733E-06, 2.99380E-06, 3.62416E-06, 3.81404E-06, bhh24290
     X 4.46735E-06, 4.70753E-06, 4.54494E-06, 3.83002E-06, 2.28067E-06, bhh24300
     X 2.03102E-06, 2.43844E-06, 2.93132E-06, 2.17555E-06, 3.92919E-06, bhh24310
     X 3.53089E-06, 1.61388E-06, 5.09498E-06, 3.40067E-06, 1.58876E-06, bhh24320
     X 1.17367E-06, 1.13344E-06, 1.17798E-06, 1.10976E-06, 7.90635E-07, bhh24330
     X-4.15989E-07,-1.00581E-06,-9.60236E-07,-1.79111E-07,-5.70733E-07, bhh24340
     X 1.49766E-06, 3.44374E-06, 6.45914E-07, 1.00532E-06, 2.01068E-06, bhh24350
     X 2.59092E-06, 9.35770E-08, 6.00121E-07, 1.54409E-06, 2.03537E-06, bhh24360
     X 8.10358E-07, 1.34126E-06, 1.88873E-06, 1.43283E-06,-2.05029E-07, bhh24370
     X-1.09782E-06,-6.56149E-07, 2.01650E-06, 1.84770E-06, 4.39586E-08, bhh24380
     X-2.03588E-06,-1.46366E-06,-3.45189E-07, 4.02577E-07, 3.10362E-07, bhh24390
     X-2.16073E-06,-1.91861E-06,-2.90520E-07, 2.03692E-06, 3.47996E-06, bhh24400
     X 4.21761E-06, 3.89000E-06, 1.86138E-06, 1.56143E-06, 4.88964E-07/ bhh24410
      DATA C21521 /                                                     bhh24420
     X-9.28184E-07,-4.34315E-07, 8.74954E-07, 1.58417E-06, 1.36880E-06, bhh24430
     X 2.65016E-06, 4.62623E-06, 5.81990E-06, 4.72139E-06, 1.95905E-06, bhh24440
     X 1.54151E-06, 2.95768E-06, 4.71536E-06, 2.62359E-06, 9.11513E-07, bhh24450
     X 4.75677E-07,-1.53801E-06,-2.32382E-06,-2.25220E-06,-1.46641E-06, bhh24460
     X-2.23014E-06,-2.12604E-06,-1.66259E-06,-2.48856E-06,-2.38895E-06, bhh24470
     X-2.18158E-06,-1.95841E-06, 4.43899E-07, 1.08517E-06, 1.66370E-07, bhh24480
     X-2.42342E-06,-7.19331E-07, 3.19532E-07, 3.58690E-07,-2.01979E-07, bhh24490
     X 5.07242E-07, 1.10562E-06, 1.00419E-06, 1.22379E-06, 7.05180E-07, bhh24500
     X 1.42283E-07, 8.61092E-07, 8.95236E-07, 1.18043E-07,-1.23589E-06, bhh24510
     X-6.16316E-07,-1.18947E-06,-1.45838E-06,-1.47522E-09, 1.33867E-06, bhh24520
     X 9.18310E-07,-8.98949E-07,-2.27314E-06,-1.71510E-06,-7.16704E-07, bhh24530
     X 8.60666E-09, 5.68015E-07, 1.31219E-06, 1.75478E-06, 5.11790E-07, bhh24540
     X 3.35270E-07, 5.39243E-07, 9.08467E-07, 1.39382E-06, 1.08806E-06, bhh24550
     X 1.18589E-06, 3.58461E-06, 2.78668E-06, 1.25964E-06,-2.72255E-07, bhh24560
     X 1.72305E-06, 1.82937E-06, 7.46252E-07,-1.10555E-06, 2.24967E-07, bhh24570
     X 6.45674E-07,-1.87591E-07,-8.84068E-07,-1.75433E-06,-2.17670E-06/ bhh24580
      DATA C21601 /                                                     bhh24590
     X-1.37112E-06,-2.31722E-06,-2.23860E-06,-1.16796E-06,-2.23765E-06, bhh24600
     X-1.86406E-06,-1.03517E-06,-5.90824E-07,-6.57710E-07,-7.00941E-07, bhh24610
     X-4.46064E-07, 1.77205E-06, 2.45066E-06, 2.39371E-06, 2.30736E-06, bhh24620
     X 2.35355E-06, 1.85070E-06, 9.62711E-07, 2.59644E-06, 2.05304E-06, bhh24630
     X 9.70090E-07, 1.50942E-06, 3.79439E-06, 2.94597E-06,-1.91789E-06, bhh24640
     X 6.44324E-08,-3.92094E-07,-1.55398E-06, 4.46701E-08,-4.78760E-07, bhh24650
     X-1.70061E-06,-3.17252E-06,-2.93173E-06,-2.01455E-06,-7.76298E-07, bhh24660
     X-2.74577E-07,-1.39907E-06,-2.16470E-06,-1.26010E-06,-2.76845E-06, bhh24670
     X-2.38226E-06,-5.49068E-08, 9.65258E-07, 1.08650E-06, 5.64738E-07, bhh24680
     X-5.78379E-07,-5.68918E-07,-1.90177E-06,-5.08874E-06,-3.03648E-06, bhh24690
     X-1.30527E-06,-4.87669E-07,-2.83326E-06,-1.97823E-06,-5.94313E-07, bhh24700
     X-1.50961E-07,-1.15908E-06,-1.43260E-06,-9.29331E-07,-1.39459E-06, bhh24710
     X-1.27237E-06,-1.50189E-06,-3.79292E-06,-3.92038E-06,-3.58490E-06, bhh24720
     X-3.26439E-06,-2.42138E-06,-2.70516E-06,-3.58080E-06,-1.71822E-06, bhh24730
     X-2.41567E-06,-3.50193E-06,-2.62394E-06,-3.08424E-06,-3.89604E-06, bhh24740
     X-4.84127E-06,-4.41385E-06,-3.22673E-06,-1.80987E-06,-2.93027E-06/ bhh24750
      DATA C21681 /                                                     bhh24760
     X-3.17366E-06,-2.79721E-06,-1.78848E-06,-2.80254E-06,-3.55572E-06, bhh24770
     X-3.34632E-06,-2.83979E-06,-2.48022E-06,-2.15090E-06,-1.08311E-06, bhh24780
     X-6.15216E-07,-7.13008E-07,-1.70841E-06,-2.96098E-06,-3.57134E-06, bhh24790
     X-3.04405E-06,-3.35280E-06,-2.97780E-06,-1.97966E-06,-2.33197E-06, bhh24800
     X-2.76708E-06,-2.70409E-06,-4.51094E-07,-1.43068E-06,-2.83719E-06, bhh24810
     X-2.98921E-06,-4.14949E-06,-3.63780E-06,-8.10138E-07,-1.61597E-06, bhh24820
     X-2.25394E-06,-2.58110E-06,-1.57781E-06,-1.71520E-06,-2.30016E-06, bhh24830
     X-2.61268E-06,-1.96696E-06,-1.86744E-06,-3.15645E-06,-3.59354E-06, bhh24840
     X-3.61015E-06,-3.21793E-06,-2.57436E-06,-2.74347E-06,-3.33319E-06, bhh24850
     X-2.93400E-06,-3.25986E-06,-3.46384E-06,-2.22114E-06,-2.92650E-06, bhh24860
     X-3.73666E-06,-3.70485E-06,-2.75963E-06,-2.40652E-06,-2.93107E-06, bhh24870
     X-1.77517E-06,-1.57096E-06,-2.17533E-06,-2.80190E-06,-2.27942E-06, bhh24880
     X-1.37371E-06,-1.65974E-06,-1.26079E-06,-8.08050E-07,-8.41278E-07, bhh24890
     X-1.53860E-06,-1.66687E-06,-6.56592E-07,-3.05110E-08, 1.08623E-07, bhh24900
     X-2.87222E-07,-2.63555E-07,-7.89575E-07,-1.56059E-06,-6.42174E-07, bhh24910
     X-9.43333E-07,-1.38671E-06, 6.50443E-07, 1.35301E-06, 9.27981E-07/ bhh24920
      DATA C21761 /                                                     bhh24930
     X-1.21705E-06,-9.63848E-08, 8.73593E-07,-3.47278E-08,-1.79042E-06, bhh24940
     X-2.15544E-06,-4.48668E-07,-1.17414E-06,-1.35437E-06,-8.90688E-07, bhh24950
     X-4.54757E-07, 2.41484E-09, 3.88010E-07,-1.85349E-08, 1.58011E-07, bhh24960
     X 3.70566E-07,-7.30268E-07,-8.42354E-07,-4.13738E-07, 3.96796E-07, bhh24970
     X-5.55763E-07,-1.26877E-06,-2.89854E-07, 5.78676E-07, 9.51356E-07, bhh24980
     X 5.56912E-07, 1.05014E-06, 9.75896E-07, 5.91573E-08,-6.15073E-07, bhh24990
     X-1.48803E-06,-2.53397E-06,-1.77027E-06,-2.08546E-06,-3.10452E-06, bhh25000
     X-1.65227E-06,-1.15981E-06,-1.25849E-06,-9.65711E-07,-1.90319E-06, bhh25010
     X-2.71831E-06,-5.71559E-08,-1.20368E-06,-3.16820E-06,-2.22766E-06, bhh25020
     X-1.19828E-06,-2.82573E-07, 2.53850E-07,-9.10547E-07,-1.65529E-06, bhh25030
     X-6.00138E-07,-4.98898E-07,-3.45799E-07, 2.25160E-07, 1.14332E-07, bhh25040
     X 3.16082E-07, 1.12681E-06,-6.04876E-07,-7.24616E-07, 1.48177E-06, bhh25050
     X 1.05680E-06, 5.91076E-07, 2.07187E-07, 3.82385E-07, 5.91560E-07, bhh25060
     X 8.26519E-07, 1.22139E-06, 1.63501E-06, 2.06423E-06, 2.50038E-06, bhh25070
     X 2.38037E-06, 1.91688E-06, 2.46702E-06, 2.45066E-06, 2.16732E-06, bhh25080
     X 3.13517E-06, 2.68221E-06, 1.39877E-06, 8.58945E-07, 6.83181E-07/ bhh25090
      DATA C21841 /                                                     bhh25100
     X 8.46816E-07, 1.73491E-06, 1.98732E-06, 1.94059E-06, 2.19284E-06, bhh25110
     X 1.73215E-06, 1.06865E-06, 1.14117E-06, 1.43213E-06, 1.42275E-06, bhh25120
     X-4.15449E-07,-2.39911E-07, 3.46498E-08,-2.75022E-06,-2.43736E-06, bhh25130
     X-1.06489E-06,-7.81941E-07,-8.04801E-07,-1.04984E-06,-1.65734E-06, bhh25140
     X-1.03167E-06,-3.18255E-08, 5.70283E-07, 6.19050E-07, 2.92257E-07, bhh25150
     X-6.01436E-07,-7.04005E-07,-3.70875E-07, 4.12830E-07, 1.31319E-07, bhh25160
     X-1.61570E-07, 9.76170E-07, 7.99907E-07, 1.41860E-07,-1.98022E-07, bhh25170
     X 3.13766E-07, 7.43982E-07,-6.11287E-07,-5.21146E-07, 1.11156E-07, bhh25180
     X 3.91719E-07, 5.45566E-07, 6.39059E-07, 7.29515E-07, 4.59167E-07, bhh25190
     X 6.13179E-08,-3.48146E-08, 5.32046E-07, 1.19736E-06, 3.83982E-07, bhh25200
     X 1.73267E-07, 3.54304E-07, 9.34657E-07, 5.53819E-07,-2.86678E-07, bhh25210
     X 2.01853E-08,-1.56159E-07,-6.08130E-07,-2.14929E-07, 1.66317E-08, bhh25220
     X 9.32462E-08,-4.83623E-07,-9.16323E-07,-1.22772E-06,-1.61586E-06, bhh25230
     X-1.27409E-06,-1.98119E-07,-3.69182E-08,-1.41061E-07,-5.12562E-07, bhh25240
     X-4.55495E-07,-8.12132E-07,-1.71772E-06,-2.70741E-06,-2.98751E-06, bhh25250
     X-2.19520E-06, 3.01900E-07, 1.17806E-06,-1.23067E-06, 4.17086E-07/ bhh25260
      DATA C21921 /                                                     bhh25270
     X 1.68113E-06, 4.81677E-07,-1.55187E-07,-3.35287E-07, 2.94916E-07, bhh25280
     X 4.57124E-07, 3.38692E-07,-2.49203E-07,-3.62585E-07,-2.39653E-07, bhh25290
     X 3.72675E-08,-7.79964E-09,-2.83285E-07,-9.74713E-07,-6.91171E-07, bhh25300
     X 1.21925E-07, 3.39940E-07, 3.68441E-08,-5.82188E-07, 2.12605E-07, bhh25310
     X 4.65144E-07, 2.17190E-07, 7.50119E-07, 8.62008E-07, 4.63016E-07, bhh25320
     X 1.25620E-06, 1.04567E-06,-8.17037E-07,-1.20023E-06,-1.06224E-06, bhh25330
     X-3.77100E-07,-1.28057E-07,-2.76183E-07,-1.24304E-06,-2.56776E-06, bhh25340
     X-3.36699E-06,-1.49408E-06,-1.01189E-07, 7.41870E-07,-6.45425E-07, bhh25350
     X-7.47111E-07, 4.79055E-10,-1.32339E-06,-1.86135E-06,-1.61074E-06, bhh25360
     X-1.82039E-06,-1.68040E-06,-1.08025E-06,-8.61965E-07,-7.00131E-07, bhh25370
     X-5.63105E-07,-8.09843E-07,-8.09221E-07, 1.69474E-07,-1.33941E-07, bhh25380
     X-7.49558E-07,-5.19013E-07,-8.53534E-07,-1.33703E-06,-3.11161E-07, bhh25390
     X 8.99037E-07, 2.25330E-06, 1.44822E-06, 3.07437E-07,-1.22366E-06, bhh25400
     X-7.64217E-07, 2.13156E-08, 1.07909E-06, 6.10755E-07, 1.81483E-07, bhh25410
     X 8.12405E-07,-9.13283E-08,-1.35885E-06,-1.58366E-06,-7.88594E-07, bhh25420
     X 4.48283E-07,-1.23754E-06,-1.65105E-06,-8.93014E-07,-1.48622E-06/ bhh25430
      DATA C22001 /                                                     bhh25440
     X-1.67948E-06,-1.24310E-06,-1.54411E-06,-1.65677E-06,-1.04998E-06, bhh25450
     X-1.46985E-07, 4.61778E-07,-4.87832E-07,-4.89452E-07,-1.24840E-07, bhh25460
     X-1.70101E-06,-1.66976E-06,-1.48528E-07,-1.12621E-07,-2.30607E-08, bhh25470
     X 1.82301E-07,-8.58152E-07,-1.89794E-06,-2.46464E-06,-2.32745E-06, bhh25480
     X-2.02112E-06,-2.07656E-06,-1.43824E-06,-5.16583E-07,-1.80702E-06, bhh25490
     X-2.93490E-06,-3.89216E-06,-3.36211E-06,-2.41393E-06,-9.53406E-07, bhh25500
     X-1.16269E-06,-1.66431E-06,-1.77150E-06,-1.82496E-06,-1.93095E-06, bhh25510
     X-2.75759E-06,-2.83618E-06,-2.27908E-06,-6.33348E-07, 5.61257E-07, bhh25520
     X 1.00142E-06, 7.73337E-07, 3.17721E-07,-3.69804E-07,-8.82058E-07, bhh25530
     X-1.17364E-06,-4.53480E-07,-2.47824E-07,-4.79624E-07,-5.17032E-07, bhh25540
     X-3.46498E-07, 1.42669E-07,-1.59168E-07,-5.06580E-07,-3.18573E-07, bhh25550
     X-2.74092E-07,-2.68860E-07, 1.32811E-07,-2.35567E-09,-6.71971E-07, bhh25560
     X-9.75302E-07,-8.70978E-07,-3.59071E-08,-3.01726E-07,-8.27641E-07, bhh25570
     X-1.14899E-06,-1.50160E-06,-1.83660E-06,-1.26290E-06,-1.07659E-06, bhh25580
     X-1.34878E-06,-5.24626E-07,-7.85094E-08,-8.79473E-07,-1.19291E-06, bhh25590
     X-1.33298E-06,-1.59750E-06,-1.31836E-06,-5.73079E-07,-1.10349E-06/ bhh25600
      DATA C22081 /                                                     bhh25610
     X-1.11807E-06,-1.99530E-07,-8.10496E-07,-1.42679E-06,-5.34617E-07, bhh25620
     X-2.05001E-07,-2.51690E-07,-1.01740E-06,-1.02841E-06,-7.48750E-08, bhh25630
     X-1.01770E-06,-1.50413E-06, 1.80898E-07, 3.63788E-07,-1.97900E-07, bhh25640
     X-1.16721E-06,-1.05497E-06,-2.07218E-08,-1.90590E-07,-8.25501E-07, bhh25650
     X-2.21142E-06,-1.19905E-06, 2.16271E-07,-2.52574E-07,-4.35837E-07, bhh25660
     X-3.95272E-07, 5.97065E-08, 2.76639E-07, 9.22569E-08, 1.20142E-07, bhh25670
     X-2.95030E-09,-1.08216E-06,-1.32386E-06,-9.62248E-07,-1.99430E-06, bhh25680
     X-2.13890E-06,-9.56082E-07,-6.94022E-07,-7.75721E-07,-1.31048E-06, bhh25690
     X-1.50080E-06,-1.35873E-06,-7.48378E-07,-4.83436E-07,-4.69624E-07, bhh25700
     X-1.51156E-06,-2.48221E-06,-3.30134E-06,-2.79114E-06,-2.08976E-06, bhh25710
     X-2.24768E-06,-1.06947E-06, 1.17462E-06,-2.51423E-07,-7.85729E-07, bhh25720
     X 5.37467E-07,-9.39876E-08,-1.11303E-06,-7.46860E-07,-9.36220E-07, bhh25730
     X-1.59880E-06,-1.61420E-06,-1.54368E-06,-1.41036E-06,-7.20350E-07, bhh25740
     X 1.35544E-07, 3.14481E-07, 6.29265E-07, 1.09161E-06,-1.36044E-07, bhh25750
     X-1.22932E-06,-1.29847E-06,-3.26429E-06,-6.01062E-06,-2.09945E-06, bhh25760
     X 1.26878E-07,-2.88050E-08,-6.82802E-07,-1.39340E-06,-1.82986E-06/ bhh25770
      DATA C22161 /                                                     bhh25780
     X-1.67208E-06,-1.07994E-06,-1.89195E-06,-2.10782E-06,-1.04519E-06, bhh25790
     X-3.27672E-07, 1.95516E-07, 1.63838E-07,-2.29575E-07,-1.01609E-06, bhh25800
     X-2.19286E-06,-2.71850E-06,-9.77485E-07,-1.48830E-06,-3.37826E-06, bhh25810
     X-1.59130E-06,-5.74498E-07,-8.27962E-07,-9.92211E-07,-1.14422E-06, bhh25820
     X-1.41420E-06,-1.11629E-06,-2.51575E-07, 1.60805E-07, 1.82934E-07, bhh25830
     X-7.28868E-07,-2.57062E-07, 1.06520E-06, 4.16488E-07, 2.97049E-08, bhh25840
     X 6.62797E-08, 8.29435E-07, 1.29657E-06,-2.27961E-06,-3.40386E-06, bhh25850
     X-1.88594E-06,-2.29732E-06,-2.72594E-06,-2.09847E-06,-1.31771E-06, bhh25860
     X-4.23693E-07,-4.96348E-07,-9.40209E-07,-2.08707E-06,-1.21368E-06, bhh25870
     X 4.79409E-07,-1.12548E-08,-4.57316E-07,-8.40885E-07,-5.03210E-07, bhh25880
     X-1.61036E-07,-1.05835E-06,-1.66417E-06,-1.97827E-06,-1.63737E-06, bhh25890
     X-1.11711E-06,-3.16081E-07,-6.81746E-07,-1.82599E-06,-1.12895E-06, bhh25900
     X-9.19712E-07,-1.91707E-06,-2.14767E-06,-2.03629E-06,-2.86441E-06, bhh25910
     X-3.07735E-06,-2.28656E-06,-1.40256E-06,-5.50649E-07,-3.11627E-07, bhh25920
     X-7.90261E-07,-2.10728E-06,-1.89739E-06,-1.53762E-06,-2.39947E-06, bhh25930
     X-2.28765E-06,-1.27564E-06,-2.15154E-06,-3.17932E-06,-3.84234E-06/ bhh25940
      DATA C22241 /                                                     bhh25950
     X-3.65102E-06,-2.84055E-06,-2.48744E-06,-2.27683E-06,-2.33087E-06, bhh25960
     X-3.44460E-06,-5.19613E-06,-2.85882E-06,-1.39921E-06,-2.00579E-06, bhh25970
     X-2.80593E-06,-3.65940E-06,-2.39526E-06,-1.70389E-06,-2.03532E-06, bhh25980
     X-2.71522E-06,-3.42227E-06,-2.23606E-06,-1.77845E-06,-2.42071E-06, bhh25990
     X-2.61515E-06,-2.56413E-06,-1.49601E-06,-1.23245E-06,-2.08440E-06, bhh26000
     X-2.11121E-06,-1.93424E-06,-2.27439E-06,-2.58183E-06,-2.84705E-06, bhh26010
     X-2.32183E-06,-1.80966E-06,-3.04089E-06,-3.14334E-06,-1.91331E-06, bhh26020
     X-1.51037E-06,-1.43610E-06,-2.11316E-06,-2.45184E-06,-2.42262E-06/ bhh26030
      END                                                               bhh26040
      SUBROUTINE BS(I,A,B,N,S)                                          bs   100
C********************************************************************** bs   110
      DIMENSION B(9)                                                    bs   120
C                                                                       bs   130
C             THIS SUBROUTINE DOES THE BINARY SEARCH FOR THE INDEX I    bs   140
C             SUCH THAT A IS IN BETWEEN B(I) AND B(I+1)                 bs   150
C             AND CALCULATES THE INTERPOLATION PARAMETER S              bs   160
C             SUCH THAT A=S*B(I+1)+(1.-S)*B(I)                          bs   170
C                                                                       bs   180
      I=1                                                               bs   190
      J=N                                                               bs   200
   10 M=(I+J)/2                                                         bs   210
      IF(A.LE.B(M)) THEN                                                bs   220
      J=M                                                               bs   230
      ELSE                                                              bs   240
      I=M                                                               bs   250
      END IF                                                            bs   260
      IF(J.GT.I+1) GO TO 10                                             bs   270
      S=(A-B(I))/(B(I+1)-B(I))                                          bs   280
      RETURN                                                            bs   290
      END                                                               bs   300
      BLOCK DATA C4D                                                    c4d  100
C>    BLOCK DATA                                                        c4d  110
      COMMON /C4C8/ C401(114),C4115(19),C8(102)                         c4d  120
C        N2 CONTINUUM ABSORPTION COEFFICIENTS                           c4d  130
C     C4 LOCATION  1    V =  2080 CM-1                                  c4d  140
C     C4 LOCATION  133  V =  2740 CM-1                                  c4d  150
      DATA C401 /                                                       c4d  160
     1 2.93E-04, 3.86E-04, 5.09E-04, 6.56E-04, 8.85E-04, 1.06E-03,      c4d  170
     2 1.31E-03, 1.73E-03, 2.27E-03, 2.73E-03, 3.36E-03, 3.95E-03,      c4d  180
     3 5.46E-03, 7.19E-03, 9.00E-03, 1.13E-02, 1.36E-02, 1.66E-02,      c4d  190
     4 1.96E-02, 2.16E-02, 2.36E-02, 2.63E-02, 2.90E-02, 3.15E-02,      c4d  200
     5 3.40E-02, 3.66E-02, 3.92E-02, 4.26E-02, 4.60E-02, 4.95E-02,      c4d  210
     6 5.30E-02, 5.65E-02, 6.00E-02, 6.30E-02, 6.60E-02, 6.89E-02,      c4d  220
     7 7.18E-02, 7.39E-02, 7.60E-02, 7.84E-02, 8.08E-02, 8.39E-02,      c4d  230
     8 8.70E-02, 9.13E-02, 9.56E-02, 1.08E-01, 1.20E-01, 1.36E-01,      c4d  240
     9 1.52E-01, 1.60E-01, 1.69E-01, 1.60E-01, 1.51E-01, 1.37E-01,      c4d  250
     $ 1.23E-01, 1.19E-01, 1.16E-01, 1.14E-01, 1.12E-01, 1.12E-01,      c4d  260
     $ 1.11E-01, 1.11E-01, 1.12E-01, 1.14E-01, 1.13E-01, 1.12E-01,      c4d  270
     $ 1.09E-01, 1.07E-01, 1.02E-01, 9.90E-02, 9.50E-02, 9.00E-02,      c4d  280
     $ 8.65E-02, 8.20E-02, 7.65E-02, 7.05E-02, 6.50E-02, 6.10E-02,      c4d  290
     $ 5.50E-02, 4.95E-02, 4.50E-02, 4.00E-02, 3.75E-02, 3.50E-02,      c4d  300
     $ 3.10E-02, 2.65E-02, 2.50E-02, 2.20E-02, 1.95E-02, 1.75E-02,      c4d  310
     $ 1.60E-02, 1.40E-02, 1.20E-02, 1.05E-02, 9.50E-03, 9.00E-03,      c4d  320
     $ 8.00E-03, 7.00E-03, 6.50E-03, 6.00E-03, 5.50E-03, 4.75E-03,      c4d  330
     $ 4.00E-03, 3.75E-03, 3.50E-03, 3.00E-03, 2.50E-03, 2.25E-03,      c4d  340
     $ 2.00E-03, 1.85E-03, 1.70E-03, 1.60E-03, 1.50E-03, 1.50E-03/      c4d  350
      DATA C4115 /                                                      c4d  360
     1 1.54E-03, 1.50E-03, 1.47E-03, 1.34E-03, 1.25E-03, 1.06E-03,      c4d  370
     2 9.06E-04, 7.53E-04, 6.41E-04, 5.09E-04, 4.04E-04, 3.36E-04,      c4d  380
     3 2.86E-04, 2.32E-04, 1.94E-04, 1.57E-04, 1.31E-04, 1.02E-04,      c4d  390
     4 8.07E-05/                                                        c4d  400
C        4M  H2O CONTINUUM                                              c4d  410
C        OZONE U.V. + VISIBLE BAND MODEL ABSORPTION COEFF               c4d  420
C     C8 LOCATION  1    V =  13000  CM-1                                c4d  430
C     C8 LOCATION  56   V =  24200  CM-1                                c4d  440
C        DV = 200  CM-1                                                 c4d  450
C     C8 LOCATION  57   V =  27500  CM-1                                c4d  460
C     C8 LOCATION  102  V =  50000  CM-1                                c4d  470
C        DV = 500  CM-1                                                 c4d  480
      DATA C8 /                                                         c4d  490
     1 4.50E-03, 8.00E-03, 1.07E-02, 1.10E-02, 1.27E-02, 1.71E-02,      c4d  500
     2 2.00E-02, 2.45E-02, 3.07E-02, 3.84E-02, 4.78E-02, 5.67E-02,      c4d  510
     3 6.54E-02, 7.62E-02, 9.15E-02, 1.00E-01, 1.09E-01, 1.20E-01,      c4d  520
     4 1.28E-01, 1.12E-01, 1.11E-01, 1.16E-01, 1.19E-01, 1.13E-01,      c4d  530
     5 1.03E-01, 9.24E-02, 8.28E-02, 7.57E-02, 7.07E-02, 6.58E-02,      c4d  540
     6 5.56E-02, 4.77E-02, 4.06E-02, 3.87E-02, 3.82E-02, 2.94E-02,      c4d  550
     7 2.09E-02, 1.80E-02, 1.91E-02, 1.66E-02, 1.17E-02, 7.70E-03,      c4d  560
     8 6.10E-03, 8.50E-03, 6.10E-03, 3.70E-03, 3.20E-03, 3.10E-03,      c4d  570
     9 2.55E-03, 1.98E-03, 1.40E-03, 8.25E-04, 2.50E-04, 0.      ,      c4d  580
     $ 0.      , 0.      , 5.65E-04, 2.04E-03, 7.35E-03, 2.03E-02,      c4d  590
     $ 4.98E-02, 1.18E-01, 2.46E-01, 5.18E-01, 1.02E+00, 1.95E+00,      c4d  600
     $ 3.79E+00, 6.65E+00, 1.24E+01, 2.20E+01, 3.67E+01, 5.95E+01,      c4d  610
     $ 8.50E+01, 1.26E+02, 1.68E+02, 2.06E+02, 2.42E+02, 2.71E+02,      c4d  620
     $ 2.91E+02, 3.02E+02, 3.03E+02, 2.94E+02, 2.77E+02, 2.54E+02,      c4d  630
     $ 2.26E+02, 1.96E+02, 1.68E+02, 1.44E+02, 1.17E+02, 9.75E+01,      c4d  640
     $ 7.65E+01, 6.04E+01, 4.62E+01, 3.46E+01, 2.52E+01, 2.00E+01,      c4d  650
     $ 1.57E+01, 1.20E+01, 1.00E+01, 8.80E+00, 8.30E+00, 8.60E+00/      c4d  660
      END                                                               c4d  670
      SUBROUTINE C4DTA (C4L,V)                                          c4dt 100
C **  N2 CONTINUUM                                                      c4dt 110
      COMMON /C4C8/ C4(133),C8(102)                                     c4dt 120
      C4L=0.                                                            c4dt 130
      IF(V.LT.2080.) RETURN                                             c4dt 140
      IF(V.GT.2740.) RETURN                                             c4dt 150
      IV=V                                                              c4dt 160
      L=(IV-2080)/5+1                                                   c4dt 170
      C4L=C4(L)                                                         c4dt 180
      RETURN                                                            c4dt 190
      END                                                               c4dt 200
      SUBROUTINE C6DTA(C6L,V)                                           c6dt 100
C     CALCULATES MOLECULAR RAYLEIGH SCATTERING COEFFICIENT              c6dt 110
C     USES APPROXIMATION OF SHETTLE ET AL., 1980 (APPL OPT, 2873-4)     c6dt 120
C        WITH THE DEPOLARIZATION = 0.0279 (INSTED OF 0.035)             c6dt 130
C     INPUT:  V = FREQUENCY IN WAVENUMBERS (CM-1)                       c6dt 140
C     OUTPUT: C6L = MOLECULAR SCATTERING COEFFICIENT (KM-1)             c6dt 150
C                    FOR TEMPERATURE = 273 K & PRESSURE 1 ATM.          c6dt 160
C                                                                       c6dt 170
      C6L=V**4/(9.38076E+18  -1.08426E+09 * V**2)                       c6dt 180
      RETURN                                                            c6dt 190
      END                                                               c6dt 200
      SUBROUTINE C8DTA (C8L,V)                                          c8dt 100
C **  OZONE U.V + VISIBLE                                               c8dt 110
      COMMON /C4C8/ C4(133),C8(102)                                     c8dt 120
      C8L=0.                                                            c8dt 130
      IF(V.LT.13000.) RETURN                                            c8dt 140
      IF(V.GT.24200.) RETURN                                            c8dt 150
      IV=V                                                              c8dt 160
CC    IF(IV.GT.24200.AND.IV.LT.27500) RETURN                            c8dt 170
      XI=(V-13000.0)/200.0+1.                                           c8dt 180
      IF(IV.GE.27500) XI=(V-27500.0)/500.+57.                           c8dt 190
      N=XI+1.001                                                        c8dt 200
      XD=XI-FLOAT(N)                                                    c8dt 210
      C8L=C8(N)+XD*(C8(N)-C8(N-1))                                      c8dt 220
      RETURN                                                            c8dt 230
      END                                                               c8dt 240
      SUBROUTINE CHECK(V,IV,KEY)                                        chck 100
C                                                                       chck 110
C      UNITS CONVERSION FOR P AND T                                     chck 120
C                                                                       chck 130
C     V = P OR T     AND  IV =JUNITP(I.E. MB,ATM,TORR)                  chck 140
C                            =JUNITT(I.E. DEG K OR C)                   chck 150
C                            =JUNITR(I.E. KM,M,OR CM)                   chck 160
C                                                                       chck 170
      DATA PMB/1013.25/,PTORR/760./,DEGK/273.15/                        chck 180
      IF(IV.LE.10) RETURN                                               chck 190
      GO TO(100,200,300) KEY                                            chck 200
C                                                                       chck 210
C      PRESSURE CONVERSIONS                                             chck 220
C                                                                       chck 230
  100 IF(IV.EQ.11)GO TO 110                                             chck 240
      IF(IV.EQ.12)GO TO 120                                             chck 250
      STOP'CHECK(P)'                                                    chck 260
  110 V=V*PMB                                                           chck 270
      RETURN                                                            chck 280
  120 V=V*PMB/PTORR                                                     chck 290
      RETURN                                                            chck 300
C                                                                       chck 310
C      TEMPERATURE COMVERSIONS                                          chck 320
C                                                                       chck 330
  200 IF(IV.GT.11)STOP'CHECK(T)'                                        chck 340
      V=V+DEGK                                                          chck 350
      RETURN                                                            chck 360
C                                                                       chck 370
C      RANGE CONVERSIONS                                                chck 380
C                                                                       chck 390
  300 IF(IV.EQ.11)GO TO 310                                             chck 400
      IF(IV.EQ.12)GO TO 320                                             chck 410
      STOP'CHECK(R)'                                                    chck 420
  310 V=V/1.E3                                                          chck 430
      RETURN                                                            chck 440
  320 V=V/1.E5                                                          chck 450
      RETURN                                                            chck 460
      END                                                               chck 470
      SUBROUTINE CIRR18                                                 ci18 100
C*********************************************************************  ci18 110
C*  ROUTINE TO SET CTHIK CALT CEXT  FOR  CIRRUS CLOUDS 18 19        **  ci18 120
C*  INPUTS]                                                         **  ci18 130
C*           CHTIK    -  CIRRUS THICKNESS (KM)                      **  ci18 140
C*                       0 = USE THICKNESS STATISTICS               **  ci18 150
C*                       .NE. 0 = USER DEFINES THICKNESS            **  ci18 160
C*                                                                  **  ci18 170
C*           CALT     -  CIRRUS BASE ALTITUDE (KM)                  **  ci18 180
C*                       0 = USE CALCULATED VALUE                   **  ci18 190
C*                       .NE. 0 = USER DEFINES BASE ALTITUDE        **  ci18 200
C*                                                                  **  ci18 210
C*           ICLD     -  CIRRUS PRESENCE FLAG                       **  ci18 220
C*                       0 = NO CIRRUS                              **  ci18 230
C*                       18  19 = USE CIRRUS PROFILE                **  ci18 240
C*                                                                  **  ci18 250
C*           MODEL    -  ATMOSPHERIC MODEL                          **  ci18 260
C*                       1-5  AS IN MAIN PROGRAM                    **  ci18 270
C*                       MODEL = 0,6,7 NOT USED SET TO 2            **  ci18 280
C*                                                                  **  ci18 290
C*  OUTPUTS]                                                        **  ci18 300
C*         CTHIK        -  CIRRUS THICKNESS (KM)                    **  ci18 310
C*         CALT         -  CIRRUS BASE ALTITUDE (KM)                **  ci18 320
C          CEXT IS THE EXTINCTION COEFFIENT(KM-1) AT 0.55               ci18 330
C               DEFAULT VALUE 0.14*CTHIK                                ci18 340
C*                                                                  **  ci18 350
C*********************************************************************  ci18 360
C                                                                       ci18 370
      COMMON /CARD1/ MODEL,ITYPE,IEMSCT,M1,M2,M3,IM,NOPRNT,TBOUND,SALB  ci18 380
     1  ,MODTRN                                                         ci18 390
      LOGICAL MODTRN                                                    ci18 400
      COMMON /CARD2/ IHAZE,ISEASN,IVULCN,ICSTL,ICLD,IVSA,VIS,WSS,WHH,   ci18 410
     1    RAINRT                                                        ci18 420
      COMMON /CARD2A/ CTHIK,CALT,CEXT                                   ci18 430
      include 'parameter.list'
      COMMON RELHUM(laydim),HSTOR(laydim),ICH(4),VH(17),TX(65),W(65)  
      COMMON IMSMX,WPATH(laythr,65),TBBY(laythr),PATM(laythr),NSPEC,   
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX2(47),AWCCON(5)  
      COMMON /CNTRL/ KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT               ci18 480
C     COMMON /CARD4/ V1,V2,DV                                           ci18 490
      COMMON /MODEL/ ZM(LAYDIM),PM(LAYDIM),TM(LAYDIM),RFNDX(LAYDIM),
     1  DENSTY(65,LAYDIM),CLDAMT(LAYDIM),RRAMT(LAYDIM),EQLWC(LAYDIM),
     1  HAZEC(LAYDIM)
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      DIMENSION CBASE(5,2),TSTAT(11),PTAB(5),CAMEAN(5)                  ci18 530
      DIMENSION CBASE1(5),CBASE2(5)                                     ci18 540
      EQUIVALENCE (CBASE1(1),CBASE(1,1)),(CBASE2(1),CBASE(1,2))         ci18 550
C                                                                       ci18 560
      DATA  CAMEAN           / 11.0, 10.0, 8.0, 7.0, 5.0 /              ci18 570
      DATA  PTAB           / 0.8, 0.4, 0.5, 0.45, 0.4/                  ci18 580
      DATA  CBASE1            / 7.5, 7.3, 4.5, 4.5, 2.5 /               ci18 590
      DATA  CBASE2            /16.5,13.5,14.0, 9.5,10.0 /               ci18 600
      DATA  TSTAT             / 0.0,.291,.509,.655,.764,.837,.892,      ci18 610
     + 0.928, 0.960, 0.982, 1.00 /                                      ci18 620
      MDL = MODEL                                                       ci18 630
C                                                                       ci18 640
C  CHECK IF USER WANTS TO USE A THICKNESS VALUE HE PROVIDES             ci18 650
C  DEFAULTED MEAN CIRRUS THICKNESS IS 1.0KM  OR 0.2 KM.                 ci18 660
C                                                                       ci18 670
      IF ( CTHIK .GT. 0.0 ) GO TO 25                                    ci18 680
      IF(ICLD.EQ.18) CTHIK=1.0                                          ci18 690
      IF(ICLD.EQ.19) CTHIK=0.2                                          ci18 700
25    IF(CEXT .EQ. 0.) CEXT = 0.14 * CTHIK                              ci18 710
C                                                                       ci18 720
C  BASE HEIGHT CALCULATIONS                                             ci18 730
C                                                                       ci18 740
      IF ( MODEL .LT. 1  .OR.  MODEL .GT. 5 ) MDL = 2                   ci18 750
C                                                                       ci18 760
      HMAX = CBASE(MDL,2) - CTHIK                                       ci18 770
C     BRANGE = HMAX - CBASE(MDL,1)                                      ci18 780
      IF ( CALT .GT. 0.0 ) GO TO 27                                     ci18 790
      CALT = CAMEAN(MDL)                                                ci18 800
C                                                                       ci18 810
 27   IF(ICLD. EQ. 18) WRITE(IPR,1219)                                  ci18 820
1219  FORMAT(15X,'CIRRUS ATTENUATION INCLUDED   (STANDARD CIRRUS)')     ci18 830
      IF(ICLD. EQ. 19) WRITE(IPR,1220)                                  ci18 840
1220  FORMAT(15X,'CIRRUS ATTENUATION INCLUDED   (THIN     CIRRUS)')     ci18 850
      WRITE(IPR,1221) CTHIK                                             ci18 860
1221  FORMAT(15X,'CIRRUS THICKNESS ',                                   ci18 870
     X F10.3,'KM')                                                      ci18 880
      WRITE(IPR,1224)CALT                                               ci18 890
1224  FORMAT(15X,'CIRRUS BASE ALTITUDE ',                               ci18 900
     X F10.3,' KM')                                                     ci18 910
       WRITE(IPR,1226) CEXT                                             ci18 920
1226    FORMAT(15X,'CIRRUS PROFILE EXTINCT ',F10.3)                     ci18 930
C                                                                       ci18 940
C       END OF CIRRUS MODEL SET UP                                      ci18 950
      RETURN                                                            ci18 960
      END                                                               ci18 970
      SUBROUTINE CIRRUS(CTHIK,CALT,ISEED,CPROB,CEXT)                    cius 100
C*********************************************************************  cius 110
C*  ROUTINE TO GENERATE ALTITUDE PROFILES OF CIRRUS DENSITY         **  cius 120
C*  PROGRAMMED BY   M.J. POST                                       **  cius 130
C*                  R.A. RICHTER        NOAA/WPL                    **  cius 140
C*                                      BOULDER, COLORADO           **  cius 150
C*                                      01/27/1981                  **  cius 160
C*                                                                  **  cius 170
C*  INPUTS!                                                         **  cius 180
C*           CHTIK    -  CIRRUS THICKNESS (KM)                      **  cius 190
C*                       0 = USE THICKNESS STATISTICS               **  cius 200
C*                       .NE. 0 = USER DEFINES THICKNESS            **  cius 210
C*                                                                  **  cius 220
C*           CALT     -  CIRRUS BASE ALTITUDE (KM)                  **  cius 230
C*                       0 = USE CALCULATED VALUE                   **  cius 240
C*                       .NE. 0 = USER DEFINES BASE ALTITUDE        **  cius 250
C*                                                                  **  cius 260
C*           ICIR     -  CIRRUS PRESENCE FLAG                       **  cius 270
C*                       0 = NO CIRRUS                              **  cius 280
C*                       .NE. 0 = USE CIRRUS PROFILE                **  cius 290
C*                                                                  **  cius 300
C*           MODEL    -  ATMOSPHERIC MODEL                          **  cius 310
C*                       1-5  AS IN MAIN PROGRAM                    **  cius 320
C*                       MODEL = 0,6,7 NOT USED SET TO 2            **  cius 330
C*                                                                  **  cius 340
C*           ISEED    -  RANDOM NUMBER INITIALIZATION FLAG.         **  cius 350
C*                       0 = USE DEFAULT MEAN VALUES FOR CIRRUS     **  cius 360
C*                       .NE. 0 = INITIAL VALUE OF SEED FOR RANF    **  cius 370
C*                       FUNCTION. CHANGE SEED VALUE EACH RUN FOR   **  cius 380
C*                       DIFFERENT RANDOM NUMBER SEQUENCES. THIS    **  cius 390
C*                       PROVIDES FOR STATISTICAL DETERMINATION     **  cius 400
C*                       OF CIRRUS BASE ALTITUDE AND THICKNESS.     **  cius 410
C*                                                                  **  cius 420
C*  OUTPUTS!                                                        **  cius 430
C*         CTHIK        -  CIRRUS THICKNESS (KM)                    **  cius 440
C*         CALT         -  CIRRUS BASE ALTITUDE (KM)                **  cius 450
C*         DENSTY(16,I) -  ARRAY, ALTITUDE PROFILE OF CIRRUS DENSITY**  cius 460
C*         CPROB        -  CIRRUS PROBABILITY                       **  cius 470
C*                                                                  **  cius 480
C*********************************************************************  cius 490
C                                                                       cius 500
      COMMON /CARD1/ MODEL,ITYPE,IEMSCT,M1,M2,M3,IM,NOPRNT,TBOUND,SALB  cius 510
     1  ,MODTRN                                                         cius 520
      LOGICAL MODTRN                                                    cius 530
      COMMON /CARD2/ IHAZE,ISEASN,IVULCN,ICSTL,ICLD,IVSA,VIS,WSS,WHH,   cius 540
     1    RAINRT                                                        cius 550
      include 'parameter.list'
      COMMON RELHUM(laydim),HSTOR(laydim),ICH(4),VH(17),TX(65),W(65)  
      COMMON IMSMX,WPATH(laythr,65),TBBY(laythr),PATM(laythr),NSPEC,   
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX2(47),AWCCON(5)  
      COMMON /CNTRL/ KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT               cius 600
C     COMMON /CARD4/ V1,V2,DV                                           cius 610
      COMMON /MODEL/  Z(LAYDIM),PM(LAYDIM),TM(LAYDIM),RFNDX(LAYDIM),
     1  DENSTY(65,LAYDIM),CLDAMT(LAYDIM),RRAMT(LAYDIM),EQLWC(LAYDIM),
     1  HAZEC(LAYDIM)
      DIMENSION CBASE(5,2),TSTAT(11),PTAB(5),CAMEAN(5)                  cius 640
      DIMENSION CBASE1(5),CBASE2(5)                                     cius 650
      EQUIVALENCE (CBASE1(1),CBASE(1,1)),(CBASE2(1),CBASE(1,2))         cius 660
C                                                                       cius 670
      DATA  CAMEAN           / 11.0, 10.0, 8.0, 7.0, 5.0 /              cius 680
      DATA  PTAB           / 0.8, 0.4, 0.5, 0.45, 0.4/                  cius 690
      DATA  CBASE1            / 7.5, 7.3, 4.5, 4.5, 2.5 /               cius 700
      DATA  CBASE2            /16.5,13.5,14.0, 9.5,10.0 /               cius 710
      DATA  TSTAT             / 0.0,.291,.509,.655,.764,.837,.892,      cius 720
     + 0.928, 0.960, 0.982, 1.00 /                                      cius 730
C                                                                       cius 740
C  SET CIRRUS PROBABILITY AND PROFILE TO ALL ZEROES                     cius 750
C                                                                       cius 760
      CPROB = 0.0                                                       cius 770
      MDL = MODEL                                                       cius 780
C                                                                       cius 790
      DO 10 I=1,ml                                                      cius 800
10    DENSTY(16,I)=0.                                                   cius 810
C                                                                       cius 820
C  CHECK IF USER WANTS TO USE A THICKNESS VALUE HE PROVIDES, CALCULATE  cius 830
C  A STATISTICAL THICKNESS, OR USE A MEAN THICKNESS (ISEED = 0).        cius 840
C  DEFAULTED MEAN CIRRUS THICKNESS IS 1.0 KM.                           cius 850
C                                                                       cius 860
      IF ( CTHIK .GT. 0.0 ) GO TO 25                                    cius 870
      IF ( ISEED .NE. 0 ) GO TO 15                                      cius 880
      CTHIK = 1.0                                                       cius 890
      GO TO 25                                                          cius 900
C                                                                       cius 910
C  CALCULATE CLOUD THICKNESS USING LOWTRAN CIRRUS THICKNESS STATISTICS  cius 920
C  NOTE - THIS ROUTINE USES A UNIFORM RANDOM NUMBER GENERATOR           cius 930
C  FUNCTION (RANF) WHICH RETURNS A NUMBER BETWEEN 0 AND 1.              cius 940
C  THIS FEATURE IS MACHINE DEPENDENT!!                                  cius 950
C                                                                       cius 960
   15 CALL RANSET(ISEED)                                                cius 970
      URN = RANFUN(IDUM)                                                cius 980
      DO 20 I = 1, 10                                                   cius 990
         IF (URN .GE. TSTAT(I) .AND. URN .LT. TSTAT(I+1)) CTHIK = I-1   cius1000
   20 CONTINUE                                                          cius1010
      CTHIK = CTHIK / 2.0  +  RANFUN(IDUM) / 2.0                        cius1020
C                                                                       cius1030
C  DENCIR IS CIRRUS DENSITY IN KM-1                                     cius1040
C                                                                       cius1050
25    IF(CEXT .GT. 0.) THEN                                             cius1060
           DENCIR = CEXT / 2.                                           cius1070
      ELSE                                                              cius1080
           DENCIR = 0.07 * CTHIK                                        cius1090
      ENDIF                                                             cius1100
C                                                                       cius1110
C  BASE HEIGHT CALCULATIONS                                             cius1120
C                                                                       cius1130
      IF ( MODEL .LT. 1  .OR.  MODEL .GT. 5 ) MDL = 2                   cius1140
      CPROB = 100.0 * PTAB(MDL)                                         cius1150
C                                                                       cius1160
      HMAX = CBASE(MDL,2) - CTHIK                                       cius1170
      BRANGE = HMAX - CBASE(MDL,1)                                      cius1180
      IF ( CALT .GT. 0.0 ) GO TO 27                                     cius1190
      IF ( ISEED .NE. 0 ) GO TO 26                                      cius1200
      CALT = CAMEAN(MDL)                                                cius1210
      GO TO 27                                                          cius1220
   26 CALT = BRANGE * RANFUN(IDUM)+ CBASE(MDL,1)                        cius1230
C                                                                       cius1240
C  PUT CIRRUS DENSITY IN CORRECT ALTITUDE BINS. IF MODEL = 7,           cius1250
C  INTERPOLATE EH(16,I) FOR NON-STANDARD ALTITUDE BOUNDARIES.           cius1260
C                                                                       cius1270
   27 IF(MODEL .EQ. 7) GO TO 60                                         cius1280
      IV1=INT(CALT )                                                    cius1290
      IV2=INT(CALT+CTHIK )                                              cius1300
      DO 30 I = 2, 16                                                   cius1310
         IF(I .GE. IV1 .AND. I .LE. IV2) DENSTY(16,I+1) =  DENCIR       cius1320
   30 CONTINUE                                                          cius1330
C                                                                       cius1340
C  ADJUST FIRST AND LAST CIRRUS LEVEL IF CLOUD DOES NOT ENTIRELY        cius1350
C  FILL EACH LEVEL.                                                     cius1360
C                                                                       cius1370
      IHGT1 = INT( CALT )                                               cius1380
      IHGT2 = INT( CALT + CTHIK)                                        cius1390
      IF( IHGT1 . NE . IHGT2 ) GO TO 35                                 cius1400
      DENSTY(16,IHGT1+1) = DENSTY( 16,IHGT1+1)*CTHIK                    cius1410
      RETURN                                                            cius1420
   35 PCT1  = 1.0 - ( CALT - IHGT1 )                                    cius1430
      DENSTY(16,IHGT1+1) = DENSTY(16,IHGT1+1) * PCT1                    cius1440
      PCT2 =  ( CALT + CTHIK) - IHGT2                                   cius1450
      DENSTY(16,IHGT2+1) = DENSTY(16,IHGT2+1) * PCT2                    cius1460
      RETURN                                                            cius1470
C                                                                       cius1480
C  INTERPOLATE DENSTY(16,I) FOR USER SUPPLIED ALTITUDE BOUNDARIES       cius1490
C                                                                       cius1500
   60 TOP = CALT + CTHIK                                                cius1510
      BOTTOM = CALT                                                     cius1520
      IF (TOP .LT. Z(1)) RETURN                                         cius1530
      IF (BOTTOM .GT. Z(ML)) RETURN                                     cius1540
      IML = ML - 1                                                      cius1550
      DO 70 I=1,IML                                                     cius1560
         ZMIN = Z(I)                                                    cius1570
         ZMAX = Z(I+1)                                                  cius1580
         DENOM = ZMAX - ZMIN                                            cius1590
         IF(BOTTOM.LE.ZMIN .AND. TOP.GE.ZMAX) DENSTY(16,I) = DENCIR     cius1600
         IF(BOTTOM.GE.ZMIN .AND. TOP.LT.ZMAX)                           cius1610
     +        DENSTY(16,I) = DENCIR * CTHIK/DENOM                       cius1620
         IF(BOTTOM.GE.ZMIN .AND. TOP.GE.ZMAX .AND. BOTTOM.LT.ZMAX)      cius1630
     +        DENSTY(16,I) = DENCIR * (ZMAX - BOTTOM)/ DENOM            cius1640
         IF(BOTTOM.LT.ZMIN .AND. TOP.LE.ZMAX .AND.TOP.GT.ZMIN)          cius1650
     +        DENSTY(16,I) = DENCIR * (TOP - ZMIN) / DENOM              cius1660
   70 CONTINUE                                                          cius1670
      RETURN                                                            cius1680
      END                                                               cius1690
      SUBROUTINE CLDPRF (K,ICLD,IHA1,IC1)                               clpr 100
C***********************************************************************clpr 110
C     WILL COMPUTE DENSITY    PROFILES FOR CLOUDS                       clpr 120
C***********************************************************************clpr 130
      REAL MDLWC                                                        clpr 140
      include 'parameter.list'
      COMMON RELHUM(laydim),HSTOR(laydim),ICH(4),VH(17),TX(65),W(65)  
      COMMON IMSMX,WPATH(laythr,65),TBBY(laythr),PATM(laythr),NSPEC,   
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX2(47),AWCCON(5)  
      COMMON /MODEL/ ZM(LAYDIM),PM(LAYDIM),TM(LAYDIM),RFNDX(LAYDIM),
     1  DENSTY(65,LAYDIM),CLDAMT(LAYDIM),RRAMT(LAYDIM),EQLWC(LAYDIM),
     1  HAZEC(LAYDIM)
      DIMENSION RHZONE(4)                                               clpr 210
      DIMENSION ELWCR(4),ELWCU(4),ELWCM(4),ELWCT(4)                     clpr 220
      DATA RHZONE/0.,70.,80.,99./                                       clpr 230
      DATA ELWCR/3.517E-04,3.740E-04,4.439E-04,9.529E-04/               clpr 240
      DATA ELWCM/4.675E-04,6.543E-04,1.166E-03,3.154E-03/               clpr 250
      DATA ELWCU/3.102E-04,3.802E-04,4.463E-04,9.745E-04/               clpr 260
      DATA ELWCT/1.735E-04,1.820E-04,2.020E-04,2.408E-04/               clpr 270
      DATA AFLWC/1.295E-02/,RFLWC/1.804E-03/,CULWC/7.683E-03/           clpr 280
      DATA ASLWC/4.509E-03/,STLWC/5.272E-03/,SCLWC/4.177E-03/           clpr 290
      DATA SNLWC/7.518E-03/,BSLWC/1.567E-04/,FVLWC/5.922E-04/           clpr 300
      DATA AVLWC/1.675E-04/,MDLWC/4.775E-04/                            clpr 310
      DATA TNLWC/3.446E-3/ ,TKLWC/5.811E-2/                             clpr 320
      IF(CLDAMT(K).LE.0.) GO TO 15                                      clpr 330
      IH=ICLD                                                           clpr 340
      IF(IH .EQ. 0) GO TO 200                                           clpr 350
      IF(ICLD .EQ. 18) THEN                                             clpr 360
           HAZEC(K) = CLDAMT(K)/TNLWC                                   clpr 370
           RETURN                                                       clpr 380
      ENDIF                                                             clpr 390
      IF(ICLD .EQ. 19) THEN                                             clpr 400
           HAZEC(K) = CLDAMT(K)/TKLWC                                   clpr 410
           RETURN                                                       clpr 420
      ENDIF                                                             clpr 430
      IF(ICLD .EQ. 20) THEN                                             clpr 440
           RETURN                                                       clpr 450
      ENDIF                                                             clpr 460
      GO TO(114,115,116,117,118,116,118,118,114,114,114),IH             clpr 470
114   HAZEC(K)=CLDAMT(K)/CULWC                                          clpr 480
      RETURN                                                            clpr 490
115   HAZEC(K)=CLDAMT(K)/ASLWC                                          clpr 500
      RETURN                                                            clpr 510
116   HAZEC(K)=CLDAMT(K)/STLWC                                          clpr 520
      RETURN                                                            clpr 530
117   HAZEC(K)=CLDAMT(K)/SCLWC                                          clpr 540
      RETURN                                                            clpr 550
118   HAZEC(K)=CLDAMT(K)/SNLWC                                          clpr 560
 15   RETURN                                                            clpr 570
200   IF(IHA1 .GT. 0) GO TO 205                                         clpr 580
      PRINT*,' WARNING ICLD NOT SET '                                   clpr 590
      RETURN                                                            clpr 600
205   CONTINUE                                                          clpr 610
      WRH = RELHUM(K)                                                   clpr 620
C                                                                       clpr 630
      M = IC1                                                           clpr 640
      IF (ICH(M).EQ.6.AND.M.NE.1) WRH=70.                               clpr 650
C     THIS CODING  DOES NOT ALLOW TROP RH DEPENDENT  ABOVE EH(7,I)      clpr 660
C     DEFAULTS TO TROPOSPHERIC AT 70. PERCENT                           clpr 670
      DO 210 I=2,4                                                      clpr 680
      IF (WRH.LT.RHZONE(I)) GO TO 215                                   clpr 690
  210 CONTINUE                                                          clpr 700
      I=4                                                               clpr 710
  215 II=I-1                                                            clpr 720
      IF(WRH.GT.0.0.AND.WRH.LT.99.)X=ALOG(100.0-WRH)                    clpr 730
      X1=ALOG(100.0-RHZONE(II))                                         clpr 740
      X2=ALOG(100.0-RHZONE(I))                                          clpr 750
      IF (WRH.GE.99.0) X=X2                                             clpr 760
      IF (WRH.LE.0.0) X=X1                                              clpr 770
      ITA=ICH(M)                                                        clpr 780
      IF(ITA.EQ.3. AND. M.EQ.1) GO TO 218                               clpr 790
      IF(ITA.GT.6) GO TO 245                                            clpr 800
CC                                                                      clpr 810
CC    MICROWAVE                                                         clpr 820
      N = 41                                                            clpr 830
CC                                                                      clpr 840
218   IF(N.GE.41. AND. ITA.EQ.3) ITA = 4                                clpr 850
C     RH DEPENDENT AEROSOLS                                             clpr 860
      GO TO (220,220,222,225,230,235), ITA                              clpr 870
 220  E2=ALOG(ELWCR(I))                                                 clpr 880
      E1=ALOG(ELWCR(II))                                                clpr 890
      GO TO 240                                                         clpr 900
 222  IF(M.GT.1) GO TO 225                                              clpr 910
      E2=ALOG(ELWCM(I))                                                 clpr 920
      E1=ALOG(ELWCM(II))                                                clpr 930
      GO TO 240                                                         clpr 940
  225 E2=ALOG(ELWCM(I))                                                 clpr 950
      E1=ALOG(ELWCM(II))                                                clpr 960
      GO TO 240                                                         clpr 970
  230 E2=ALOG(ELWCU(I))                                                 clpr 980
      E1=ALOG(ELWCU(II))                                                clpr 990
      GO TO 240                                                         clpr1000
  235 E2=ALOG(ELWCT(I))                                                 clpr1010
      E1=ALOG(ELWCT(II))                                                clpr1020
  240 EC=E1+(E2-E1)*(X-X1)/(X2-X1)                                      clpr1030
      CON=EXP(EC)                                                       clpr1040
      HAZEC(K) = CLDAMT(K)/CON                                          clpr1050
      RETURN                                                            clpr1060
  245 IF (ITA.GT.19) GO TO 275                                          clpr1070
      ITC=ICH(M)-7                                                      clpr1080
      IF (ITC.LT.1) RETURN                                              clpr1090
      GO TO (250,255,280,260,265,270,265,270,260,260,270,275), ITC      clpr1100
250   CON=AFLWC                                                         clpr1110
      GO TO 280                                                         clpr1120
255   CON=RFLWC                                                         clpr1130
      GO TO 280                                                         clpr1140
260   CON=BSLWC                                                         clpr1150
      GO TO 280                                                         clpr1160
265   CON=AVLWC                                                         clpr1170
      GO TO 280                                                         clpr1180
270   CON=FVLWC                                                         clpr1190
      GO TO 280                                                         clpr1200
275   CON=MDLWC                                                         clpr1210
280   CONTINUE                                                          clpr1220
      HAZEC(K) = CLDAMT(K)/CON                                          clpr1230
      RETURN                                                            clpr1240
      END                                                               clpr1250
      SUBROUTINE COMPAR(IPARM,IPH,IDAY,ISOURC,PARM1,PARM2,PARM3,PARM4,
     $     TIME,PSIPO,ANGLEM,
     $     ISAVE1,ISAVE2,ISAVE3,ISAVE4,SAVE1,SAVE2,SAVE3,SAVE4,
     $     SAVE5,SAVE6,SAVE7,LSAME)
C
C     IPARM, IPH, ETC ARE THE CURRENT SOLAR PARAMETERS.
C     ISAVE1, ISAVE2, ETC. ARE THE SOLAR PARAMETERS OF THE              DEC94SSI
C     IMMEDIATELY PRECEEDING RUN.                                       DEC94SSI
C     LSAME IS A LOGICAL WHICH IS TRUE ONLY IF THE OLD AND THE NEW MATCH
C     LSAME IS MEANINGFUL ONLY IF BOTH THE CURRENT AND THE IMMEDIATELY
C     PRECEEDING RUNS USED SOLAR PARAMETERS.
C
C     IMPLICIT UNDEFINED(A-Z)
      INTEGER IPARM,IDAY,IPH,ISOURC,ISAVE1,ISAVE2,ISAVE3,ISAVE4
      REAL PARM1,PARM2,PARM3,PARM4,TIME,PSIPO,ANGLEM,
     $     SAVE1,SAVE2,SAVE3,SAVE4,SAVE5,SAVE6,SAVE7
      LOGICAL LSAME
C
      LSAME = .FALSE.
      IF (ISAVE1 .NE. IPARM) RETURN
      IF (ISAVE2 .NE. IPH) RETURN
      IF (ISAVE3 .NE. IDAY) RETURN
      IF (ISAVE4 .NE. ISOURC) RETURN
      IF (SAVE1 .NE. PARM1 .AND. IPARM.NE.2) RETURN                     DEC94SSI
      IF (SAVE2 .NE. PARM2) RETURN
      IF (SAVE3 .NE. PARM3) RETURN
      IF (SAVE4 .NE. PARM4) RETURN
      IF (SAVE5 .NE. TIME) RETURN
C           REMOVED SAVE6 COMPARISON                                    DEC94SSI
      IF (SAVE7 .NE. ANGLEM) RETURN
      LSAME = .TRUE.
      RETURN
      END
      SUBROUTINE CONVRT (P,T)                                           conv 100
C*************************************************************          conv 110
C                                                                       conv 120
C     WRITTEN APR, 1985 TO ACCOMMODATE 'JCHAR' DEFINITIONS FOR          conv 130
C     UNIFORM DATA INPUT -                                              conv 140
C                                                                       conv 150
C     JCHAR    JUNIT                                                    conv 160
C                                                                       conv 170
C     " ",A       10    VOLUME MIXING RATIO (PPMV)                      conv 180
C     B       11    NUMBER DENSITY (CM-3)                               conv 190
C     C       12    MASS MIXING RATIO (GM(K)/KG(AIR))                   conv 200
C     D       13    MASS DENSITY (GM M-3)                               conv 210
C     E       14    PARTIAL PRESSURE (MB)                               conv 220
C     F       15    DEW POINT TEMP (TD IN T(K)) - H2O ONLY              conv 230
C     G       16     "    "     "  (TD IN T(C)) - H2O ONLY              conv 240
C     H       17    RELATIVE HUMIDITY (RH IN PERCENT) - H2O ONLY        conv 250
C     I       18    AVAILABLE FOR USER DEFINITION                       conv 260
C     J       19    REQUEST DEFAULT TO SPECIFIED MODEL ATMOSPHERE       conv 270
C                                                                       conv 280
C***************************************************************        conv 290
C                                                                       conv 300
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON /CONSTN/ PZERO,TZERO,AVOGAD,ALOSMT,GASCON,PLANK,BOLTZ,     conv 320
     1     CLIGHT,ADCON,ALZERO,AVMWT,AIRMWT,AMWT(35)                    conv 330
      COMMON /CARD1B/ JUNITP,JUNITT,JUNIT1(13),WMOL(12),WAIR1,JLOW      cfcconv 340
c
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
c     modtran has 65 as magic number.  It INCLUDEs the usual 12 species
c     plus a host of other species and sub species.  Many
c     arrays have dimension 65.
      INCLUDE 'parameter.list'
      REAL AMWTX(MMOLX)                                                 cfc
      common /ATMWTX/amwtx                                              cfc
      COMMON /CRD1BX/JUNITX, WMOLX(MMOLX)                               cfc
C*****                                                                  conv370
C     
C     VARIABLES ENDING WITH "X" ARE INTRODUCED TO DEAL WITH THE
c     NSPECX EXTRA MOLECULES.
C     
      RHOAIR = ALOSMT*(P/PZERO)*(TZERO/T)                               conv 380
c
      DO 200 K = 1,NSPC+NSPECX
         if (k .le. NSPC) then
            B = AVOGAD/AMWT(K)     
            R = AIRMWT/AMWT(K)     
            JUNIT = JUNIT1(K)         
            WHOLD  = WMOL(K)          
         else IF ( K .GT. NSPC) THEN                                              
            j = k-NSPC
            B = AVOGAD/AMWTX(J)                                        
            R = AIRMWT/AMWTX(J)                                        
            JUNIT = JUNITX
            WHOLD  = WMOLX(J)                                           
         ENDIF
C        
         IF(K.EQ.1) then
            CALL WATVAP(P,T)  
            go to 200
         endif
c        
         IF (JUNIT .LE. 10) then
C           ***** GIVEN VOL. MIXING RATIO   
            go to 200
         else IF (JUNIT .eq. 11) then
C           *****GIVEN NUMBER DENSITY (CM-3) 
            if (k .le. NSPC) WMOL(K)=WHOLD/(RHOAIR*1.E-6) 
            if (k .gt. NSPC) WMOLx(j)=WHOLD/(RHOAIR*1.E-6) 
            go to 200
cjv 12/7/95  Correction from Ron Alley. This units test on atmospheric 
cjv	profile units  should not have been changed to nspc. 
cjv         else IF (JUNIT .eq. NSPC) then
         else IF (JUNIT .eq. 12) then
cjv
c
C           *****GIVEN MASS MIXING RATIO (GM KG-1)
            if (k .le. NSPC) WMOL(K)= R*WHOLD*1.0E+3   
            if (k .gt. NSPC) WMOLx(j)= R*WHOLD*1.0E+3   
            go to 200
         else IF (JUNIT .eq. 13) then
c           *****GIVEN MASS DENSITY (GM M-3)
            if (k .le. NSPC) WMOL(K) = B*WHOLD/RHOAIR     
            if (k .gt. NSPC) WMOLX(j)= B*WHOLD/RHOAIR                        
            go to 200
         else IF (JUNIT .eq. 14) then
c           *****GIVEN  PARTIAL PRESSURE (MB)  
            WTEM    = ALOSMT*(WHOLD/PZERO)*(TZERO/T)  
            IF (K . le. NSPC) WMOL(K)= WTEM/(RHOAIR*1.E-6)     
            IF (K . gt. NSPC) WMOLX(j)=WTEM/(RHOAIR*1.E-6) 
            go to 200
         else 
            WRITE (IPR,951)JUNIT                                      
 951        FORMAT(/,'   **** ERROR IN CONVERT ****, JUNIT = ',I5)   
            go to 200
         endif
 200  continue
      end
      FUNCTION DBLTX(W,CPRIME,QA,txlog)
c
c     modified by lex berk on October 1993
      txlog=0.
      DBLTX=1.
      IF(W.LT.1.E-20 .or. cprime.lt.-20.)RETURN
      QAWS=QA*(ALOG10(W)+CPRIME)
      IF(QAWS.le.-6.)return
      if(qaws.ge.2.)then
          txlog=100.
          DBLTX=0.
      else
          txlog=10**qaws
          DBLTX=EXP(-txlog)
      endif
      RETURN
      END
      SUBROUTINE DEFALT  (Z,P,T)                                        
C                                                                       
C     ******************************************************************
C                                                                       
C     THIS SUBROUTINE LOADS ONE OF THE 6 BUILT IN ATMOSPHERIC PROFILES  
C     FROM WHICH IT WILL INTERPOLATE "DEFAULT" VALUES FOR ALTITUDE "Z"  
C                                                                       
C                                                                       
C      ***  THIS SUBROUTINE IS CALLED BY "RDUNIT" WHICH                 
C      ***  READS USER SUPPLIED INPUT PROFILES OR SINGLE VALUES         
C      ***  UNDER "MODEL = 0     " SPECIFICATIONS                       
C                                                                       
C      *** SEE DOCUMENTATION FOR CLARIFICATION ***                      
C                                                                       
C     SUBROUTINE "DEFALT"IS TRIGGERRED WHENEVER ANY ONE OF              
C     THE INPUT PARAMETERS JCHARP, JCART, (JCHAR(K),K=1,NMOL) IS = 1-6  
C                                                                       
C     FOR SIMPLICITY, ALL INTERPOLATIONS ARE DONE AT ONE TIME BECAUSE   
C     THE LAGRANGE WEIGHTS (4PT), BASED ON (ALT-Z), REMAIN UNCHANGED    
C                                                                       
C                   JCHAR(K) FOR K<8 ALLOW MODEL-DEPENDENT CHOICES      
C                                                                       
C                   JCHAR=JUNIT                                         
C                                                                       
C                        1       CHOOSES TROPICAL                       
C                        2         "     MID-LATITUDE SUMMER            
C                        3         "     MID-LATITUDE WINTER            
C                        4         "     HIGH-LAT SUMMER                
C                        5         "     HIGH-LAT WINTER                
C                        6         "     US STANDARD                    
C                                                                       
C                                                                       
C    JUNIT(K) FOR K>7 CHOOSES FROM THE SINGLE TRACE CONSTITUENT         
C        PROFILES, ALL APPRORIATE FOR THE US STD ATMOSPHERE             
C                                                                       
C     ***  NOTE ***  T<0 WILL ALSO PRINT OUT A MESSAGE INDICATING       
C     ***  A POSSIBLE MISAPPLICATION OF TEMPERATURE UNITS, (K) VS (C)   
C                                                                       
C     ******************************************************************
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON /CARD1B/ JUNITP,JUNITT,JUNIT(13),WMOL(12),WAIR,JLOW        
      COMMON /MLATM / ALT(50),PMATM(50,6),TMATM(50,6),AMOL(50,8,6)      
      COMMON /MLATMX/ LAYXMX,ALTX(50),AMOLX(50,35)                      
      COMMON /TRAC/ TRAC(50,21)                                         
c
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
c     Modtran has 65 as a magic number.  It INCLUDEs the usual 12 specie
c     plus a host of other species and sub species.  Many arrays have 
c     dimension 65.

      INCLUDE 'parameter.list'
c
      common/co2mix / co2rat , ch4rat, tozonerat,sozonerat,
     $     toffset,h2otscaled,
     $     h2orat
      common /crd1bx/junitx,wmolx(mmolx)                                
C
C     THE COMMON MLATMX IS FOR CFC PROFILES.  ONLY 8 CFC WILL BE DEALT  
C     WITH.  SO AMOLX COULD HAVE BEEN DIMENSIONED AMOLX(50,8) BECAUSE   
C     IN THE PROFILE BLOCK DATA FILE (XMLATM.F) THE FIRST EIGHT AMOLN,  
C     N = 1, 2, ..., 35, ARRAYS CONTAIN THE RELEVANT CFC PROFILES.      
C                                                                       
      DATA PZERO /1013.25/,TZERO/273.15/,XLOSCH/2.6868E19/              
C                                                                       
C     *** 4PT INTERPOLATION FUNCTION                                    
C                                                                       
      VAL(A1,A2,A3,A4,X1,X2,X3,X4)=A1*X1+A2*X2+A3*X3+A4*X4              
C                                                                       
C                                                                       
      ILOWER=0                                                          
      IUPPER=0                                                          
      IM50=50                                                           
      DO 15 IM=2,IM50                                                   
      I2=IM                                                             
      IF (ALT(IM).GE.Z) GO TO 20                                        
   15 CONTINUE                                                          
      I2=IM50                                                           
   20 I1=I2-1                                                           
      I0=I2-2                                                           
      I3=I2+1                                                           
      IF(I0.LT.1) GO TO 25                                              
      IF(I3.GT.IM50) GO TO 26                                           
C                                                                       
      GO TO 28                                                          
C                                                                       
C     LOWER ENDPOINT CORRECTION                                         
C                                                                       
25    CONTINUE                                                          
      ILOWER=1                                                          
      I0=I1                                                             
      I1=I2                                                             
      I2=I3                                                             
      I3=I3+1                                                           
      GO TO 28                                                          
C                                                                       
C     UPPER ENDPOINT CORRECTION                                         
C                                                                       
26    CONTINUE                                                          
      IUPPER=1                                                          
      IF(Z.GT.ALT(IM50))GO TO 29                                        
      I3=I2                                                             
      I2=I1                                                             
      I1=I0                                                             
      I0=I1-1                                                           
      GO TO 28                                                          
C                                                                       
C      UPPER ENDPOINT EXTRAPOLATION                                     
C                                                                       
29    CONTINUE                                                          
      Z0=ALT(I0)                                                        
      Z1=ALT(I1)                                                        
      Z2=ALT(I2)                                                        
      Z3=Z2+2.*(Z-Z2)                                                   
      IUPPER=2                                                          
      WRITE(IPR,80)Z                                                    
80    FORMAT(/,'   *** Z IS GREATER THAN 120 KM ***, Z = ',F10.3)       
      STOP'DEFAULTZ'                                                    
C     I3=I2                                                             
C     GO TO 31                                                          
C                                                                       
C     LAGRANGE CONTINUATION                                             
C                                                                       
28    CONTINUE                                                          
C                                                                       
C      LAGRANGE COEF DETERMINATION                                      
C                                                                       
      Z1=ALT(I1)                                                        
      Z2=ALT(I2)                                                        
      Z0=ALT(I0)                                                        
      Z3=ALT(I3)                                                        
31    CONTINUE                                                          
      DEN1=(Z0-Z1)*(Z0-Z2)*(Z0-Z3)                                      
      DEN2=(Z1-Z2)*(Z1-Z3)*(Z1-Z0)                                      
      DEN3=(Z2-Z3)*(Z2-Z0)*(Z2-Z1)                                      
      DEN4=(Z3-Z0)*(Z3-Z1)*(Z3-Z2)                                      
      A1=((Z-Z1)*(Z-Z2)*(Z-Z3))/DEN1                                    
      A2=((Z-Z2)*(Z-Z3)*(Z-Z0))/DEN2                                    
      A3=((Z-Z3)*(Z-Z0)*(Z-Z1))/DEN3                                    
      A4=((Z-Z0)*(Z-Z1)*(Z-Z2))/DEN4                                    
C                                                                       
C                                                                       
C     TEST INPUT PARAMETERS (JUNIT'S) SEQUENTIALLY FOR TRIGGER          
C      I.E.  JUNIT(P,T,K) = 1-6                                         
C                                                                       
      IF (JUNITP.GT.6) GO TO 35                                         
      MATM=JUNITP                                                       
C     WRITE (IPR,60) Z,MATM                                             
      X1=ALOG(PMATM(I0,MATM))                                           
      X2=ALOG(PMATM(I1,MATM))                                           
      X3=ALOG(PMATM(I2,MATM))                                           
      X4=ALOG(PMATM(I3,MATM))                                           
      IF(IUPPER.EQ.2)X4=X3+2*(X3-X2)                                    
      P=VAL(A1,A2,A3,A4,X1,X2,X3,X4)                                    
      P=EXP(P)                                                          
   35 IF (JUNITT.GT.6) GO TO 40                                         
      MATM=JUNITT                                                       
C     WRITE (IPR,65) Z,MATM                                             
      X1=TMATM(I0,MATM)                                                 
      X2=TMATM(I1,MATM)                                                 
      X3=TMATM(I2,MATM)                                                 
      X4=TMATM(I3,MATM)                                                 
      T=VAL(A1,A2,A3,A4,X1,X2,X3,X4)                                    
  40  DO 55 K=1,NSPC+NSPECX                                            
      IF (k .le. nspc .and. JUNIT(K).GT.6) GO TO 55                    
C                                                                       
      IF (K.GT.7) GO TO 45                                              
      MATM=JUNIT(K)                                                     
C                                                                       
      X1=AMOL(I0,K,MATM)                                                
      X2=AMOL(I1,K,MATM)                                                
      X3=AMOL(I2,K,MATM)                                                
      X4=AMOL(I3,K,MATM)                                                
      GO TO 50                                                          
   45 CONTINUE                                                          
      IF (K .LE. NSPC) THEN
         ITR=K-7                                                        
         MATM=6                                                         
C                                                                       
         X1=TRAC(I0,ITR)                                                
         X2=TRAC(I1,ITR)                                                
         X3=TRAC(I2,ITR)                                                
         X4=TRAC(I3,ITR)                                                
      ELSE
         if (junitx .gt. 6) go to 55
         ITR = K-NSPC
         MATM=6                                                         
         X1=AMOLX(I0,ITR)
         X2=AMOLX(I1,ITR)
         X3=AMOLX(I2,ITR)
         X4=AMOLX(I3,ITR)
         WMOLX(ITR)=VAL(A1,A2,A3,A4,X1,X2,X3,X4)
cj db
         if(WMOLX(ITR).lt.0.)WMOLX(ITR)=0.
cj
         GO TO 55
      ENDIF
   50 CONTINUE                                                          
      WMOL(K)=VAL(A1,A2,A3,A4,X1,X2,X3,X4)                              
cj db
         if(WMOL(k).lt.0.)WMOL(k)=0.
cj
c      IF(K.EQ.1)WMOL(1) = WMOL(1) * h2orat
      IF(K.EQ.2)WMOL(2) = WMOL(2) * co2rat
      IF(K.EQ.6)WMOL(6) = WMOL(6) * ch4rat
      JUNIT(K)=10                                                       
      GO TO 55                                                          
C  53 JUNIT(K)=10                                                       
C     WRITE(IPR,54)K                                                    
C  54 FORMAT('  **** INCONSISTENCY IN THE USER SPECIFICATION',          
C    A ' , JUNIT = 9 AND WMOL(K) = 0 , K =',I2,/,                       
C    B '  ****   DENNUM(K) HAS BEEN SET TO 0, NOT DEFAULT VALUE')       
   55 CONTINUE                                                          
      WMOL(12)   =WMOL(12)   *1.0E+3                                    
C     THE UNIT FOR NEW PROFILE IS PPMV.                                 
      RETURN                                                            
C100  CONTINUE                                                          
  110 FORMAT(/,'  *** FATAL INPUT ERROR ***',/,                         
     A '  DEFAULT SPECIFICATIONS EXCEED MODEL = 6',/,                   
     B  0PF10.2,' KM',1PE10.3,' MB',0PF10.3,' K',/,                     
     C (5(I5,1PE10.3)))                                                 
C     STOP'DEFAULT'                                                     
C                                                                       
   60 FORMAT ('   DEFAULT PRESSURE AT Z=',F7.2,' KM FROM MODEL',I5)     
   65 FORMAT ('   DEFAULT TEMPERATURE AT Z=',F7.2,' KM FROM MODEL',I5)  
   70 FORMAT ('   DEFAULT,K =',I3,',',A10,',','AT Z=',F7.2,             
     A ' KM FROM MODEL',I5)                                             
      END                                                               
      BLOCK DATA CPH2O                                                  cph2 100
C>    BLOCK DATA                                                        cph2 110
C                                                                       cph2 120
C     C' FOR WATER VAPOR (H2O)                                          cph2 130
      COMMON /H2O/                                                      cph2 140
     +        C11H2O( 70),                                              cph2 150
     +        C21H2O(126),C22H2O(  5),                                  cph2 160
     +        C31H2O(126),C32H2O(  2),                                  cph2 170
     +        C41H2O(126),C42H2O( 52),                                  cph2 180
     +        C51H2O(126),C52H2O( 52),                                  cph2 190
     +        C61H2O(126),C62H2O( 52),                                  cph2 200
     +        C71H2O(126),C72H2O(126),C73H2O(116),                      cph2 210
     +        C81H2O(126),C82H2O(126),C83H2O(118),                      cph2 220
     +        C91H2O(126),C92H2O(126),C93H2O( 71),                      cph2 230
     +        CA1H2O(126),CA2H2O(126),CA3H2O(126),CA4H2O(7),            cph2 240
     +        CB1H2O(126),CB2H2O(126),CB3H2O( 54),                      cph2 250
     +        CC1H2O(126),CC2H2O(126),CC3H2O(106),                      cph2 260
     +        CD1H2O(126),CD2H2O(111),                                  cph2 270
     +        CE1H2O(126),CE2H2O(126),CE3H2O( 53)                       cph2 280
C=H2O ====C' FOR   14 BAND MODELS                                       cph2 290
C=H2O ====    0-  345                                                   cph2 300
      DATA C11H2O/                                                      cph2 310
     X -.59366, -.16679,  .42846,  .87819, 1.26357, 1.59247, 1.86372,   cph2 320
     X 2.11483, 2.31810, 2.44040, 2.55998, 2.69879, 2.79810, 2.89747,   cph2 330
     X 2.98118, 3.04863, 3.09568, 3.15381, 3.22984, 3.23785, 3.20991,   cph2 340
     X 3.14246, 3.03461, 2.98864, 3.03520, 3.08981, 3.10027, 3.11302,   cph2 350
     X 3.10266, 3.05765, 3.06166, 3.01593, 2.95500, 2.95328, 2.95297,   cph2 360
     X 2.91497, 2.83753, 2.74642, 2.70474, 2.75606, 2.84097, 2.89052,   cph2 370
     X 2.89886, 2.86150, 2.78032, 2.67212, 2.52752, 2.39301, 2.38109,   cph2 380
     8 2.43965, 2.46195, 2.39329, 2.22943, 2.15815, 2.16157, 2.29683,   cph2 390
     9 2.40335, 2.35569, 2.29239, 2.12968, 2.03781, 1.94313, 1.86282,   cph2 400
     X 1.87312, 1.88177, 1.95321, 1.94145, 1.92602, 1.92812, 1.90587/   cph2 410
C=H2O ====  350- 1000                                                   cph2 420
      DATA C21H2O/                                                      cph2 430
     X 2.04943, 1.95396, 1.78078, 1.60325, 1.55071, 1.49473, 1.46485,   cph2 440
     X 1.50231, 1.39831, 1.30664, 1.14704,  .96109,  .93139, 1.00613,   cph2 450
     X 1.11827, 1.13529, 1.07767,  .96652,  .90777,  .91973,  .90622,   cph2 460
     X  .93883,  .90861,  .81968,  .79852,  .69385,  .56997,  .49693,   cph2 470
     X  .40867,  .37846,  .44490,  .53554,  .59020,  .59196,  .50771,   cph2 480
     X  .34361,  .20796,  .15417,  .13600,  .14235,  .12700,  .08853,   cph2 490
     X  .06715,  .11430,  .15016,  .15016,  .13964,  .04897, -.04476,   cph2 500
     8 -.16953, -.30196, -.39901, -.42462, -.39340, -.35671, -.30771,   cph2 510
     9 -.31570, -.35021, -.47016, -.62308, -.77946, -.85086, -.82482,   cph2 520
     X -.83468, -.83991, -.89726, -.90918, -.84484, -.71025, -.62777,   cph2 530
     1 -.66324, -.76848,-1.03341,-1.27044,-1.49576,-1.61769,-1.53549,   cph2 540
     X-1.47958,-1.33160,-1.29625,-1.40768,-1.52411,-1.72765,-1.82510,   cph2 550
     X-1.76468,-1.70983,-1.59977,-1.50730,-1.46683,-1.39464,-1.43093,   cph2 560
     X-1.58947,-1.78778,-2.06146,-2.33634,-2.40749,-2.49065,-2.44182,   cph2 570
     X-2.25150,-2.19801,-2.08624,-2.10309,-2.27174,-2.36492,-2.45781,   cph2 580
     X-2.44508,-2.36196,-2.38101,-2.48058,-2.61957,-2.74895,-2.74245,   cph2 590
     X-2.63961,-2.61588,-2.61569,-2.71770,-2.92220,-3.01021,-2.99432,   cph2 600
     X-2.89456,-2.79847,-2.73359,-2.69055,-2.65898,-2.60837,-2.63170/   cph2 610
      DATA C22H2O/                                                      cph2 620
     X-2.79096,-2.97394,-3.15934,-3.17057,-2.95258/                     cph2 630
C=H2O ==== 1005- 1640                                                   cph2 640
      DATA C31H2O/                                                      cph2 650
     C-2.78308,-2.69196,-2.60867,-2.62239,-2.62637,-2.62950,-2.71010,   cph2 660
     C-2.72574,-2.71317,-2.61321,-2.51967,-2.42437,-2.38734,-2.45056,   cph2 670
     C-2.47843,-2.58702,-2.56472,-2.44706,-2.30814,-2.12582,-2.02697,   cph2 680
     C-1.99880,-2.05659,-2.05701,-2.06643,-2.04721,-1.90723,-1.90946,   cph2 690
     C-1.92812,-1.86522,-1.88820,-1.77270,-1.60669,-1.51740,-1.40182,   cph2 700
     C-1.38758,-1.38799,-1.41620,-1.43182,-1.37124,-1.28249,-1.09992,   cph2 710
     C -.99724, -.97950, -.99952,-1.09066,-1.09980,-1.00750, -.87259,   cph2 720
     8 -.70131, -.48309, -.30502, -.20407, -.13886, -.19661, -.24505,   cph2 730
     9 -.28415, -.34466, -.34496, -.28657, -.09485,  .16770,  .38311,   cph2 740
     C  .48553,  .49475,  .49074,  .52493,  .57439,  .60303,  .66919,   cph2 750
     1  .75656,  .90385, 1.04976, 1.13836, 1.20132, 1.21963, 1.30344,   cph2 760
     C 1.41212, 1.46770, 1.47630, 1.45559, 1.43315, 1.49679, 1.62749,   cph2 770
     C 1.68517, 1.70120, 1.66090, 1.59891, 1.64107, 1.76792, 1.93419,   cph2 780
     C 2.09362, 2.13280, 2.07959, 2.01987, 1.96835, 2.03073, 2.17591,   cph2 790
     C 2.32257, 2.49261, 2.60881, 2.66112, 2.68139, 2.70360, 2.70568,   cph2 800
     C 2.67997, 2.66478, 2.63655, 2.59716, 2.57555, 2.58781, 2.58940,   cph2 810
     C 2.50826, 2.28771, 1.95070, 1.59144, 1.31269, 1.21786, 1.22507,   cph2 820
     8 1.31945, 1.53875, 1.78543, 2.02655, 2.22881, 2.32061, 2.34163/   cph2 830
      DATA C32H2O/                                                      cph2 840
     C 2.39432, 2.43073/                                                cph2 850
C=H2O ==== 1645- 2530                                                   cph2 860
      DATA C41H2O/                                                      cph2 870
     C 2.53438, 2.55861, 2.51156, 2.46499, 2.46254, 2.51561, 2.56373,   cph2 880
     C 2.62430, 2.67999, 2.68386, 2.68780, 2.68227, 2.59536, 2.42505,   cph2 890
     C 2.29307, 2.17816, 2.11945, 2.20521, 2.32197, 2.38083, 2.38052,   cph2 900
     C 2.25417, 2.11473, 2.06142, 2.02788, 2.01508, 1.97680, 1.91586,   cph2 910
     C 1.87253, 1.83706, 1.80766, 1.67367, 1.45528, 1.29956, 1.18809,   cph2 920
     C 1.20246, 1.33650, 1.45778, 1.48886, 1.40546, 1.22716, 1.01444,   cph2 930
     C  .91282,  .87247,  .83576,  .80170,  .71481,  .66927,  .65846,   cph2 940
     C  .66839,  .68503,  .66215,  .72413,  .78703,  .77831,  .71136,   cph2 950
     C  .51200,  .35931,  .30680,  .33365,  .36267,  .32095,  .25710,   cph2 960
     C  .12363, -.02266, -.18001, -.28048, -.27808, -.19047, -.08151,   cph2 970
     C -.09169, -.16662, -.24404, -.27238, -.27345, -.32244, -.42037,   cph2 980
     C -.54071, -.63500, -.69930, -.77174, -.83521, -.86639, -.82329,   cph2 990
     C -.78820, -.82340, -.83838, -.91387, -.96524, -.96364,-1.05757,   cph21000
     C-1.12747,-1.19973,-1.27071,-1.30173,-1.34436,-1.35556,-1.35990,   cph21010
     C-1.30386,-1.26726,-1.28022,-1.32843,-1.43599,-1.55929,-1.69416,   cph21020
     C-1.79362,-1.86416,-1.90037,-1.91305,-1.94866,-1.95483,-1.92284,   cph21030
     C-1.87535,-1.83065,-1.86043,-1.93470,-2.01410,-2.07677,-2.07980,   cph21040
     C-2.01822,-1.96078,-1.95185,-1.96638,-2.05704,-2.17667,-2.24120/   cph21050
      DATA C42H2O/                                                      cph21060
     C-2.27833,-2.33268,-2.37375,-2.43075,-2.54346,-2.60789,-2.68442,   cph21070
     C-2.78402,-2.83736,-2.89622,-2.95598,-3.03170,-3.13338,-3.26736,   cph21080
     C-3.41725,-3.51456,-3.61586,-3.67210,-3.67841,-3.72135,-3.74941,   cph21090
     C-3.78822,-3.85868,-3.90419,-3.91592,-3.97897,-4.00562,-4.08675,   cph21100
     C-4.18795,-4.15833,-4.18094,-4.18872,-4.25849,-4.42026,-4.57444,   cph21110
     C-4.64021,-4.58636,-4.51788,-4.46274,-4.44165,-4.45450,-4.42101,   cph21120
     C-4.35067,-4.30493,-4.23157,-4.11952,-4.01918,-3.93341,-3.81424,   cph21130
     C-3.70572,-3.62484,-3.48143/                                       cph21140
C=H2O ==== 2535- 3420                                                   cph21150
      DATA C51H2O/                                                      cph21160
     C-3.35886,-3.26514,-3.15517,-3.02814,-2.95147,-2.83444,-2.68908,   cph21170
     C-2.62390,-2.50458,-2.39841,-2.35516,-2.24360,-2.18204,-2.16652,   cph21180
     C-2.08381,-2.02597,-1.99880,-1.90122,-1.84045,-1.82575,-1.74889,   cph21190
     C-1.70489,-1.66792,-1.60475,-1.59789,-1.59221,-1.60854,-1.66569,   cph21200
     C-1.68527,-1.72998,-1.79886,-1.81356,-1.82715,-1.79425,-1.61106,   cph21210
     C-1.40549,-1.24369,-1.15433,-1.23589,-1.44178,-1.64717,-1.78560,   cph21220
     C-1.84622,-1.77824,-1.69071,-1.66066,-1.58765,-1.54222,-1.51960,   cph21230
     8-1.45477,-1.39881,-1.38659,-1.37586,-1.36025,-1.39179,-1.36927,   cph21240
     C-1.35455,-1.38734,-1.40292,-1.45598,-1.51545,-1.56173,-1.62478,   cph21250
     C-1.69200,-1.75192,-1.81120,-1.83354,-1.87063,-1.89006,-1.88485,   cph21260
     C-1.90298,-1.85403,-1.82001,-1.82495,-1.82901,-1.90076,-1.93649,   cph21270
     C-1.83304,-1.70268,-1.52380,-1.41443,-1.41301,-1.39373,-1.34561,   cph21280
     C-1.20932,-1.03186, -.85296, -.71145, -.59825, -.51884, -.51690,   cph21290
     C -.51723, -.52224, -.50043, -.40989, -.32204, -.24881, -.18653,   cph21300
     C -.17548, -.22729, -.32885, -.46183, -.47994, -.36042, -.23072,   cph21310
     6 -.12160, -.06422, -.14924, -.21674, -.17913, -.15803, -.04515,   cph21320
     C  .14450,  .28118,  .39718,  .49818,  .51040,  .44761,  .29666,   cph21330
     8  .01147, -.32421, -.66518, -.96090,-1.13017,-1.18009,-1.08032/   cph21340
      DATA C52H2O/                                                      cph21350
     C -.80133, -.52001, -.33748, -.22519, -.20871, -.26962, -.22592,   cph21360
     C -.15919, -.07358,  .09367,  .20019,  .25965,  .27816,  .28577,   cph21370
     C  .22305,  .17722,  .14469,  .06694,  .07268,  .10103,  .14554,   cph21380
     C  .20352,  .25681,  .25790,  .21316,  .15965,  .08703,  .01638,   cph21390
     C -.03529, -.03274, -.08812, -.12524, -.13536, -.23808, -.28262,   cph21400
     C -.30082, -.29252, -.13320,  .05226,  .17657,  .21670,  .12268,   cph21410
     C  .00438, -.03051, -.00359,  .02967,  .04460, -.01109, -.06041,   cph21420
     C -.07485, -.02511,  .07116/                                       cph21430
C=H2O ==== 3425- 4310                                                   cph21440
      DATA C61H2O/                                                      cph21450
     C  .18506,  .27668,  .32130,  .35452,  .39867,  .36470,  .34978,   cph21460
     C  .36519,  .38993,  .47009,  .54349,  .60193,  .67101,  .73253,   cph21470
     C  .84100,  .92974, 1.00406, 1.06301, 1.07261, 1.09629, 1.10790,   cph21480
     C 1.10959, 1.11710, 1.15716, 1.24152, 1.34834, 1.45152, 1.53939,   cph21490
     C 1.59331, 1.60894, 1.63833, 1.67031, 1.74144, 1.82069, 1.90463,   cph21500
     C 1.98593, 2.02996, 2.10254, 2.16357, 2.16140, 2.11190, 2.06655,   cph21510
     C 2.02241, 2.02978, 2.06771, 2.04985, 2.02048, 1.99566, 2.01593,   cph21520
     8 2.11269, 2.22805, 2.27037, 2.23480, 2.16907, 2.09990, 2.08096,   cph21530
     C 2.10710, 2.15298, 2.19061, 2.25811, 2.34221, 2.43200, 2.59765,   cph21540
     C 2.72007, 2.77243, 2.71671, 2.56246, 2.33896, 2.14412, 1.97864,   cph21550
     C 1.79640, 1.73371, 1.71380, 1.74950, 1.91932, 2.10063, 2.26262,   cph21560
     C 2.36884, 2.42988, 2.47605, 2.51875, 2.53371, 2.51476, 2.47425,   cph21570
     C 2.40051, 2.39254, 2.39540, 2.35342, 2.33460, 2.26830, 2.17169,   cph21580
     C 2.09605, 2.04747, 2.01127, 1.89721, 1.74928, 1.55948, 1.38069,   cph21590
     C 1.34831, 1.35751, 1.35809, 1.34286, 1.25929, 1.16743, 1.09595,   cph21600
     6 1.00365,  .87965,  .76257,  .64206,  .56343,  .49943,  .40691,   cph21610
     C  .29104,  .18437,  .12690,  .09157,  .13377,  .18899,  .20257,   cph21620
     8  .19155,  .09384, -.01238, -.14283, -.26122, -.31851, -.45610/   cph21630
      DATA C62H2O/                                                      cph21640
     C -.58273, -.65867, -.73100, -.66169, -.52264, -.46798, -.50258,   cph21650
     C -.59104, -.72925, -.81067, -.80914, -.86943, -.92975, -.92524,   cph21660
     C -.88289, -.79203, -.69250, -.68167, -.75444, -.86193, -.97556,   cph21670
     C-1.10473,-1.20018,-1.24824,-1.27702,-1.22693,-1.18773,-1.13552,   cph21680
     C-1.14015,-1.21589,-1.26394,-1.39464,-1.46192,-1.52629,-1.64635,   cph21690
     C-1.71511,-1.78752,-1.79358,-1.77801,-1.75599,-1.77196,-1.83224,   cph21700
     C-1.89985,-1.98528,-2.09408,-2.24126,-2.37607,-2.43218,-2.43830,   cph21710
     C-2.38400,-2.33538,-2.43573/                                       cph21720
C=H2O ==== 4315- 6150                                                   cph21730
      DATA C71H2O/                                                      cph21740
     X-2.52275,-2.67290,-2.83451,-2.93019,-3.01749,-3.02463,-2.99666,   cph21750
     X-2.95414,-2.91300,-2.96493,-3.07471,-3.25693,-3.47657,-3.67222,   cph21760
     X-3.88925,-3.97727,-3.94079,-3.81920,-3.66194,-3.59739,-3.64351,   cph21770
     X-3.74016,-3.90037,-4.04679,-4.07663,-4.03256,-3.91836,-3.80990,   cph21780
     X-3.76032,-3.77951,-3.84240,-3.90305,-3.92223,-3.82628,-3.65450,   cph21790
     X-3.44339,-3.25756,-3.09919,-3.00901,-2.95747,-2.88271,-2.82108,   cph21800
     X-2.72633,-2.59367,-2.46775,-2.36235,-2.28438,-2.27343,-2.30886,   cph21810
     8-2.33620,-2.27813,-2.20677,-2.16170,-2.14594,-2.24245,-2.36299,   cph21820
     X-2.42996,-2.50866,-2.55678,-2.50968,-2.47465,-2.42796,-2.37981,   cph21830
     X-2.34092,-2.30518,-2.26753,-2.27390,-2.44156,-2.72384,-3.06108,   cph21840
     X-3.38056,-3.48970,-3.41674,-3.36528,-3.27790,-3.15495,-3.01945,   cph21850
     X-2.81869,-2.66003,-2.56096,-2.49017,-2.46335,-2.51454,-2.59743,   cph21860
     X-2.67025,-2.78841,-2.77863,-2.63881,-2.54169,-2.40240,-2.37146,   cph21870
     X-2.46253,-2.54291,-2.65346,-2.69467,-2.69130,-2.65025,-2.59152,   cph21880
     X-2.56343,-2.50785,-2.44665,-2.41418,-2.34553,-2.28223,-2.25278,   cph21890
     6-2.20694,-2.16892,-2.14295,-2.14341,-2.16443,-2.24853,-2.38594,   cph21900
     X-2.49449,-2.58047,-2.55462,-2.41673,-2.35641,-2.32619,-2.34603,   cph21910
     8-2.40102,-2.30576,-2.20532,-2.09307,-2.00782,-2.00039,-1.91252/   cph21920
      DATA C72H2O/                                                      cph21930
     X-1.80383,-1.65749,-1.55728,-1.59262,-1.70939,-1.83569,-1.84895,   cph21940
     X-1.71457,-1.53813,-1.41904,-1.37588,-1.39458,-1.39135,-1.35232,   cph21950
     X-1.30470,-1.24821,-1.20394,-1.19607,-1.15995,-1.13948,-1.11024,   cph21960
     X-1.03785, -.99804, -.95430, -.92707, -.93592, -.93528, -.86881,   cph21970
     X -.75121, -.55836, -.35056, -.22085, -.13412, -.12673, -.13867,   cph21980
     X -.11656, -.07357,  .01888,  .11050,  .20428,  .29291,  .35923,   cph21990
     X  .43608,  .47266,  .49792,  .54978,  .60489,  .67778,  .71787,   cph22000
     8  .73606,  .74796,  .75193,  .81728,  .87972,  .95990, 1.07451,   cph22010
     X 1.13098, 1.17565, 1.19031, 1.20334, 1.27687, 1.35910, 1.41924,   cph22020
     X 1.37988, 1.28213, 1.16286, 1.08658, 1.06554, 1.03702, 1.01290,   cph22030
     X  .95519,  .94231,  .94216,  .95764, 1.03405, 1.11309, 1.27076,   cph22040
     X 1.48131, 1.66125, 1.76502, 1.68299, 1.50126, 1.28195, 1.13724,   cph22050
     X 1.09863, 1.12031, 1.23502, 1.34328, 1.39556, 1.40851, 1.40939,   cph22060
     X 1.40259, 1.39505, 1.38427, 1.33724, 1.29860, 1.34354, 1.43194,   cph22070
     X 1.50874, 1.54493, 1.48740, 1.37260, 1.26973, 1.21297, 1.11026,   cph22080
     6  .97625,  .87238,  .76100,  .71825,  .73936,  .69604,  .64138,   cph22090
     X  .59585,  .51097,  .44903,  .40524,  .29892,  .21583,  .19145,   cph22100
     8  .15378,  .13759,  .09412, -.04455, -.18870, -.28538, -.37204/   cph22110
      DATA C73H2O/                                                      cph22120
     X -.46390, -.57884, -.70647, -.78911, -.79511, -.76645, -.76146,   cph22130
     X -.80163, -.83155, -.86672, -.92994, -.99971,-1.10990,-1.25701,   cph22140
     X-1.32841,-1.33350,-1.35269,-1.31799,-1.35095,-1.48830,-1.57874,   cph22150
     X-1.67539,-1.72874,-1.68087,-1.67518,-1.73066,-1.77654,-1.79238,   cph22160
     X-1.81386,-1.77187,-1.73774,-1.78673,-1.82129,-1.86174,-1.87867,   cph22170
     X-1.92986,-1.95895,-1.98042,-2.10738,-2.14350,-2.22883,-2.35165,   cph22180
     X-2.30593,-2.31343,-2.23607,-2.17791,-2.29047,-2.40740,-2.60466,   cph22190
     8-2.70413,-2.67647,-2.64479,-2.62274,-2.66727,-2.67591,-2.66531,   cph22200
     X-2.64576,-2.69566,-2.79611,-2.90809,-2.99381,-2.94495,-2.94833,   cph22210
     X-2.97002,-3.01283,-3.07907,-3.08348,-3.06412,-3.08084,-3.20105,   cph22220
     X-3.32453,-3.49652,-3.63219,-3.65897,-3.69476,-3.63741,-3.54369,   cph22230
     X-3.44992,-3.41310,-3.43168,-3.48306,-3.57513,-3.59385,-3.59684,   cph22240
     X-3.60814,-3.50612,-3.41284,-3.34107,-3.27248,-3.26950,-3.31027,   cph22250
     X-3.32205,-3.29589,-3.29768,-3.28777,-3.29950,-3.39843,-3.43784,   cph22260
     X-3.47042,-3.54250,-3.55457,-3.69278,-3.82390,-3.91709,-4.02428,   cph22270
     6-3.97802,-4.04945,-3.99837,-3.96096,-4.01515,-4.01286,-4.27890,   cph22280
     7-4.64526,-4.92520,-5.20714,-5.02961/                              cph22290
C=H2O ==== 6155- 8000                                                   cph22300
      DATA C81H2O/                                                      cph22310
     X-4.88315,-4.85584,-4.76921,-4.54440,-4.33075,-4.16671,-4.04406,   cph22320
     X-4.09564,-4.11792,-4.14522,-4.19109,-4.14906,-4.22221,-4.35301,   cph22330
     X-4.47867,-4.50537,-4.41913,-4.24856,-4.05892,-3.91396,-3.73977,   cph22340
     X-3.60042,-3.52610,-3.50040,-3.55218,-3.66025,-3.77097,-3.87835,   cph22350
     X-3.96454,-3.93046,-3.92926,-3.96805,-3.99038,-4.10179,-4.21981,   cph22360
     X-4.24013,-4.26190,-4.27753,-4.25594,-4.28500,-4.29071,-4.26155,   cph22370
     X-4.16114,-4.04160,-3.91756,-3.82524,-3.76258,-3.74207,-3.77017,   cph22380
     8-3.80666,-3.92858,-4.01356,-4.10145,-4.16708,-4.09123,-4.00345,   cph22390
     X-3.88032,-3.81171,-3.80771,-3.83212,-3.88507,-3.81399,-3.70048,   cph22400
     X-3.58376,-3.46350,-3.42785,-3.41629,-3.40329,-3.36172,-3.26599,   cph22410
     X-3.16908,-3.10954,-3.03394,-2.95828,-2.85536,-2.71469,-2.60076,   cph22420
     X-2.48946,-2.38513,-2.32220,-2.30051,-2.34186,-2.37590,-2.33267,   cph22430
     X-2.21087,-2.03216,-1.91013,-1.82328,-1.77996,-1.76714,-1.72488,   cph22440
     X-1.71325,-1.67669,-1.62963,-1.60411,-1.54027,-1.47681,-1.37155,   cph22450
     X-1.25978,-1.23494,-1.26986,-1.33751,-1.37220,-1.28322,-1.14853,   cph22460
     6-1.03021, -.89832, -.84340, -.83317, -.78856, -.76905, -.69209,   cph22470
     X -.53147, -.37401, -.25508, -.21755, -.22627, -.23936, -.22223,   cph22480
     8 -.17345, -.11880, -.10331, -.15444, -.20353, -.25350, -.26628/   cph22490
      DATA C82H2O/                                                      cph22500
     X -.13441,  .02358,  .13657,  .22032,  .19637,  .12621,  .07999,   cph22510
     X  .04393, -.01900, -.06543, -.08129, -.14847, -.17765, -.23113,   cph22520
     X -.29309, -.28723, -.27521, -.20013, -.11575, -.00428,  .10976,   cph22530
     X  .16530,  .18309,  .13200,  .10610,  .10394,  .13621,  .17117,   cph22540
     X  .17251,  .18671,  .16161,  .16640,  .18417,  .18573,  .24876,   cph22550
     X  .26103,  .28476,  .33612,  .30642,  .30150,  .27173,  .21976,   cph22560
     X  .23130,  .27376,  .30887,  .34334,  .34765,  .31180,  .30774,   cph22570
     8  .31256,  .35423,  .42454,  .44493,  .43846,  .44507,  .43684,   cph22580
     X  .49327,  .53868,  .51933,  .54592,  .54951,  .63201,  .74737,   cph22590
     X  .80266,  .88719,  .87874,  .84412,  .84352,  .81737,  .86380,   cph22600
     X  .94765,  .95553,  .93965,  .90241,  .91481, 1.00917, 1.11552,   cph22610
     X 1.15202, 1.06885,  .96737,  .85164,  .80701,  .82571,  .87391,   cph22620
     X  .98520, 1.07042, 1.18051, 1.29004, 1.37755, 1.48278, 1.47663,   cph22630
     X 1.40851, 1.27508, 1.11986,  .98454,  .88260,  .82338,  .79509,   cph22640
     X  .83355,  .91046, 1.04503, 1.21868, 1.36672, 1.46155, 1.47085,   cph22650
     6 1.46520, 1.42619, 1.37940, 1.41333, 1.43128, 1.45974, 1.54526,   cph22660
     X 1.53032, 1.48103, 1.39624, 1.26267, 1.17261, 1.09232, 1.05888,   cph22670
     8 1.01929,  .94626,  .87615,  .73334,  .61962,  .52576,  .40124/   cph22680
      DATA C83H2O/                                                      cph22690
     X  .32424,  .20042,  .05769, -.09325, -.27407, -.40779, -.52559,   cph22700
     X -.58490, -.57916, -.54457, -.50743, -.45937, -.41861, -.41520,   cph22710
     X -.39164, -.36510, -.30857, -.23157, -.18280, -.15878, -.21295,   cph22720
     X -.29332, -.39457, -.54826, -.71006, -.87700, -.96819, -.98703,   cph22730
     X -.93748, -.83916, -.78698, -.76209, -.80754, -.93347,-1.06076,   cph22740
     X-1.15801,-1.16256,-1.09618,-1.03195,-1.05522,-1.13586,-1.23387,   cph22750
     X-1.33214,-1.32682,-1.33648,-1.38038,-1.42553,-1.49769,-1.52950,   cph22760
     8-1.54445,-1.56745,-1.61707,-1.69148,-1.76787,-1.82556,-1.84347,   cph22770
     X-1.86221,-1.87097,-1.84614,-1.88659,-1.98535,-2.12108,-2.27740,   cph22780
     X-2.39335,-2.39886,-2.33846,-2.30442,-2.27409,-2.29854,-2.39124,   cph22790
     X-2.56427,-2.73609,-2.88840,-3.00443,-3.02685,-3.09379,-3.16003,   cph22800
     X-3.13090,-3.06189,-3.00807,-2.95169,-3.01568,-3.11918,-3.18931,   cph22810
     X-3.35446,-3.46712,-3.51002,-3.48618,-3.36603,-3.29278,-3.32935,   cph22820
     X-3.47177,-3.61763,-3.68930,-3.67420,-3.62078,-3.67644,-3.76717,   cph22830
     X-3.78944,-3.79818,-3.75336,-3.74321,-3.86778,-3.96899,-4.05004,   cph22840
     6-4.15451,-4.17979,-4.22704,-4.28851,-4.25560,-4.21920,-4.27564,   cph22850
     7-4.42921,-4.58506,-4.70967,-4.80136,-4.64650,-4.65341/            cph22860
C=H2O ==== 8005- 9615                                                   cph22870
      DATA C91H2O/                                                      cph22880
     X-4.51995,-4.42433,-4.42137,-4.44853,-4.44819,-4.49132,-4.49176,   cph22890
     X-4.52929,-4.58468,-4.60533,-4.62362,-4.60168,-4.59803,-4.45292,   cph22900
     X-4.26920,-4.09891,-3.92615,-3.86016,-3.69436,-3.53699,-3.38584,   cph22910
     X-3.23356,-3.19281,-3.14232,-3.11326,-3.04386,-2.90514,-2.80270,   cph22920
     X-2.68808,-2.62726,-2.61349,-2.57111,-2.54465,-2.47142,-2.42795,   cph22930
     X-2.40936,-2.37936,-2.41255,-2.40417,-2.41017,-2.39774,-2.33861,   cph22940
     X-2.23985,-2.08388,-2.00350,-1.93767,-1.91020,-1.92815,-1.89802,   cph22950
     8-1.85648,-1.84229,-1.86062,-1.89799,-1.95863,-2.01858,-2.05596,   cph22960
     X-2.06508,-2.02824,-1.93392,-1.83965,-1.74890,-1.71252,-1.72275,   cph22970
     X-1.71193,-1.68781,-1.66945,-1.64316,-1.63675,-1.69286,-1.70297,   cph22980
     X-1.72751,-1.75100,-1.73714,-1.79804,-1.84371,-1.86235,-1.88812,   cph22990
     X-1.83704,-1.77649,-1.70661,-1.60188,-1.50341,-1.43505,-1.46076,   cph23000
     X-1.51651,-1.57911,-1.61619,-1.55812,-1.49706,-1.45230,-1.42832,   cph23010
     X-1.44314,-1.52138,-1.60752,-1.62106,-1.64265,-1.64250,-1.64573,   cph23020
     X-1.74951,-1.80667,-1.76036,-1.68790,-1.57515,-1.53228,-1.57292,   cph23030
     6-1.61350,-1.65583,-1.63563,-1.58694,-1.56417,-1.53128,-1.54079,   cph23040
     X-1.55014,-1.53022,-1.53190,-1.50230,-1.50260,-1.49991,-1.45992,   cph23050
     8-1.41944,-1.31703,-1.21850,-1.14990,-1.08809,-1.04748,-1.01748/   cph23060
      DATA C92H2O/                                                      cph23070
     X -.95109, -.84680, -.74538, -.60472, -.50362, -.46372, -.42447,   cph23080
     X -.44838, -.44419, -.40683, -.38084, -.33053, -.32846, -.33572,   cph23090
     X -.31158, -.29906, -.20305, -.13083, -.09973, -.06963, -.12740,   cph23100
     X -.20199, -.29978, -.35896, -.38843, -.41730, -.45017, -.51507,   cph23110
     X -.56213, -.57297, -.50844, -.42276, -.29372, -.08843,  .09240,   cph23120
     X  .25840,  .28311,  .13891, -.06768, -.28207, -.39760, -.40444,   cph23130
     X -.31138, -.14305, -.02128,  .04782,  .08894,  .10200,  .09648,   cph23140
     8  .10814,  .09787,  .04275,  .07559,  .12150,  .14186,  .19034,   cph23150
     X  .13856,  .07934,  .05903, -.00117, -.04140, -.11747, -.21938,   cph23160
     X -.28241, -.37335, -.49225, -.58631, -.68229, -.75086, -.77623,   cph23170
     X -.84652, -.93691,-1.00829,-1.07836,-1.10936,-1.10990,-1.10672,   cph23180
     X-1.07623,-1.03447,-1.01613,-1.00369, -.99511,-1.06778,-1.12221,   cph23190
     X-1.14258,-1.19379,-1.17257,-1.15262,-1.17033,-1.16389,-1.14503,   cph23200
     X-1.13643,-1.12323,-1.19203,-1.33003,-1.47540,-1.65339,-1.68424,   cph23210
     X-1.66968,-1.67118,-1.61782,-1.65910,-1.73337,-1.81449,-1.93135,   cph23220
     6-2.03554,-2.03827,-1.99609,-2.00710,-2.03895,-2.19678,-2.30931,   cph23230
     X-2.30301,-2.23226,-2.07787,-2.03277,-2.03851,-2.10514,-2.23452,   cph23240
     8-2.33474,-2.44465,-2.43944,-2.37675,-2.35973,-2.37611,-2.48915/   cph23250
      DATA C93H2O/                                                      cph23260
     X-2.59681,-2.62562,-2.61907,-2.61274,-2.73225,-2.84636,-2.91882,   cph23270
     X-2.95084,-2.84617,-2.83687,-2.84531,-2.82928,-2.88406,-2.93621,   cph23280
     X-3.00526,-3.09956,-3.16051,-3.18338,-3.25056,-3.38003,-3.56102,   cph23290
     X-3.72396,-3.80811,-3.82369,-3.79760,-3.90921,-4.04910,-4.14132,   cph23300
     X-4.22416,-4.16634,-4.21193,-4.37375,-4.54004,-4.54848,-4.34009,   cph23310
     X-4.10097,-3.93945,-3.99014,-4.18155,-4.46321,-4.84035,-4.95672,   cph23320
     X-4.88529,-4.92967,-5.09480,-5.27981,-5.39165,-5.32774,-5.16805,   cph23330
     8-5.26308,-5.53619,-5.93153,-6.48485,-6.38350,-6.02883,-5.76237,   cph23340
     X-5.65535,-5.58220,-5.58090,-5.69939,-5.87562,-6.23761,-6.45380,   cph23350
     X-6.50710,-6.40861,-6.18069,-6.15034,-6.12957,-6.08168,-6.05912,   cph23360
     1-6.20029/                                                         cph23370
C=H2O ==== 9620-11540                                                   cph23380
      DATA CA1H2O/                                                      cph23390
     X-6.35916,-6.63834,-7.22799,-6.87579,-6.38557,-6.05701,-5.77145,   cph23400
     X-5.71889,-5.54063,-5.34887,-5.20440,-5.01687,-4.88229,-4.75732,   cph23410
     X-4.61829,-4.47540,-4.40382,-4.22901,-4.07893,-3.91067,-3.71540,   cph23420
     X-3.66982,-3.60413,-3.59635,-3.66139,-3.67630,-3.61574,-3.49060,   cph23430
     X-3.33033,-3.18950,-3.19004,-3.27293,-3.43811,-3.58539,-3.69658,   cph23440
     X-3.64411,-3.52966,-3.51758,-3.45900,-3.56858,-3.67516,-3.75396,   cph23450
     X-3.80574,-3.77074,-3.74231,-3.63809,-3.64323,-3.59911,-3.62673,   cph23460
     8-3.64385,-3.54801,-3.49160,-3.38461,-3.33358,-3.21719,-3.04173,   cph23470
     X-2.89493,-2.77334,-2.79171,-2.91085,-3.04844,-3.23627,-3.31742,   cph23480
     X-3.35484,-3.39756,-3.34285,-3.36017,-3.34117,-3.26031,-3.20256,   cph23490
     X-3.07615,-2.98533,-3.01199,-3.13943,-3.33780,-3.54162,-3.64413,   cph23500
     X-3.59251,-3.59490,-3.60162,-3.66139,-3.81236,-3.87304,-4.04749,   cph23510
     X-4.11623,-4.09447,-4.12708,-3.91916,-3.77960,-3.62012,-3.44890,   cph23520
     X-3.42739,-3.42156,-3.36932,-3.34675,-3.22941,-3.12258,-3.12447,   cph23530
     X-3.07216,-3.06608,-3.04637,-2.99581,-3.00597,-2.94524,-2.83430,   cph23540
     6-2.69244,-2.53460,-2.44553,-2.36211,-2.25128,-2.12504,-1.99329,   cph23550
     X-1.94694,-1.96858,-2.02552,-2.02890,-1.95458,-1.83064,-1.68469,   cph23560
     8-1.63148,-1.63055,-1.64868,-1.68433,-1.65098,-1.54445,-1.45543/   cph23570
      DATA CA2H2O/                                                      cph23580
     X-1.39405,-1.35500,-1.38974,-1.43708,-1.49729,-1.58141,-1.63709,   cph23590
     X-1.71988,-1.74834,-1.78729,-1.81439,-1.79445,-1.80727,-1.78446,   cph23600
     X-1.77116,-1.69515,-1.57106,-1.41358,-1.22505,-1.11749,-1.06719,   cph23610
     X-1.05722,-1.05923,-1.08022,-1.08249,-1.05940,-1.05527, -.97884,   cph23620
     X -.90009, -.86984, -.84202, -.84891, -.86571, -.87771, -.86436,   cph23630
     X -.89675, -.95811, -.95681, -.98685, -.91920, -.79481, -.73405,   cph23640
     X -.63486, -.61580, -.66083, -.69059, -.75323, -.74477, -.65052,   cph23650
     8 -.58475, -.56151, -.61494, -.70313, -.70147, -.64776, -.57626,   cph23660
     X -.52669, -.56405, -.57813, -.57452, -.57656, -.52371, -.48121,   cph23670
     X -.47066, -.44204, -.42321, -.43939, -.40019, -.34592, -.36666,   cph23680
     X -.36117, -.41494, -.53334, -.63311, -.73668, -.83196, -.91543,   cph23690
     X -.92801, -.91893, -.82619, -.64369, -.45814, -.28838, -.20295,   cph23700
     X -.12845, -.12789, -.14668, -.10804, -.12206, -.08664, -.05495,   cph23710
     X -.09929, -.16477, -.24481, -.32305, -.39276, -.44000, -.52873,   cph23720
     X -.60139, -.69141, -.79857, -.89923,-1.00968,-1.08832,-1.14958,   cph23730
     6-1.21303,-1.28067,-1.38492,-1.47822,-1.51729,-1.55518,-1.53633,   cph23740
     X-1.51062,-1.50327,-1.51801,-1.57645,-1.65941,-1.73134,-1.75165,   cph23750
     8-1.72655,-1.71606,-1.73263,-1.74728,-1.79286,-1.73848,-1.66180/   cph23760
      DATA CA3H2O/                                                      cph23770
     X-1.56283,-1.40366,-1.32738,-1.25309,-1.25065,-1.26987,-1.24009,   cph23780
     X-1.22822,-1.19404,-1.20867,-1.23645,-1.19332,-1.13591,-1.08205,   cph23790
     X-1.04976,-1.14128,-1.23489,-1.27858,-1.33065,-1.28360,-1.22682,   cph23800
     X-1.18706,-1.15823,-1.14067,-1.16633,-1.17506,-1.15970,-1.19126,   cph23810
     X-1.19843,-1.30385,-1.42862,-1.58004,-1.72327,-1.78743,-1.86895,   cph23820
     X-1.85190,-1.80529,-1.69422,-1.49103,-1.32529,-1.20009,-1.13762,   cph23830
     X-1.11678,-1.13199,-1.16550,-1.16402,-1.17932,-1.17405,-1.15184,   cph23840
     8-1.20924,-1.29157,-1.34831,-1.38571,-1.42632,-1.43812,-1.50800,   cph23850
     X-1.62119,-1.70590,-1.86161,-2.00714,-2.11745,-2.25960,-2.34777,   cph23860
     X-2.44254,-2.64264,-2.83979,-3.04320,-3.29364,-3.41153,-3.49359,   cph23870
     X-3.60572,-3.67873,-3.78090,-3.85398,-3.88200,-3.83753,-3.77740,   cph23880
     X-3.85401,-3.79646,-3.72746,-3.70451,-3.59083,-3.70223,-3.88363,   cph23890
     X-4.03077,-4.20725,-4.19594,-4.16725,-4.13410,-4.16791,-4.12138,   cph23900
     X-4.08875,-4.14355,-4.10163,-4.19018,-4.25695,-4.31184,-4.47906,   cph23910
     X-4.51148,-4.57929,-4.59458,-4.62081,-4.83031,-5.02522,-5.15710,   cph23920
     6-5.27403,-5.22837,-5.32058,-5.55260,-5.72630,-5.86735,-5.86402,   cph23930
     X-5.84419,-5.89720,-6.15533,-6.51283,-6.98011,-7.28495,-7.08784,   cph23940
     8-6.77605,-6.49215,-6.43947,-6.42083,-6.59354,-6.78419,-6.98883/   cph23950
      DATA CA4H2O/                                                      cph23960
     X-7.11018,-6.93420,-6.83581,-6.87136,-6.96133,-7.28561,-8.27079/   cph23970
C=H2O ====11545-13070                                                   cph23980
      DATA CB1H2O/                                                      cph23990
     X-8.59451,-9.45197,-8.33631,-8.21424,-6.89777,-6.27923,-5.89945,   cph24000
     X-5.66364,-5.69459,-5.87082,-5.81185,-5.70141,-5.45890,-5.24048,   cph24010
     X-5.30703,-5.32430,-5.18694,-5.03410,-4.82279,-4.72208,-4.55097,   cph24020
     X-4.36284,-4.20326,-4.04534,-4.05883,-4.01183,-3.93857,-3.83212,   cph24030
     X-3.66113,-3.56021,-3.45969,-3.38518,-3.33373,-3.32721,-3.34771,   cph24040
     X-3.35412,-3.34584,-3.22701,-3.14293,-3.09481,-3.05706,-3.13587,   cph24050
     X-3.18156,-3.26336,-3.34559,-3.38498,-3.39054,-3.33695,-3.34959,   cph24060
     8-3.36191,-3.53258,-3.66238,-3.68946,-3.69155,-3.52990,-3.48606,   cph24070
     X-3.41160,-3.34144,-3.31933,-3.26341,-3.22609,-3.18298,-3.12800,   cph24080
     X-3.02166,-2.93903,-2.84135,-2.69864,-2.63582,-2.60853,-2.59699,   cph24090
     X-2.64799,-2.71846,-2.70856,-2.67797,-2.67978,-2.58432,-2.57052,   cph24100
     X-2.57883,-2.48977,-2.47541,-2.43446,-2.39253,-2.42823,-2.44830,   cph24110
     X-2.49704,-2.54904,-2.54865,-2.51223,-2.39407,-2.28582,-2.22341,   cph24120
     X-2.18280,-2.17308,-2.15234,-2.10486,-2.08564,-2.08578,-2.09615,   cph24130
     X-2.11136,-2.10200,-2.06832,-2.04134,-2.00194,-1.95185,-1.92025,   cph24140
     6-1.85931,-1.85988,-1.91696,-2.01129,-2.15097,-2.20539,-2.21302,   cph24150
     X-2.22309,-2.24412,-2.30471,-2.33847,-2.25445,-2.08096,-1.85098,   cph24160
     8-1.61538,-1.45841,-1.42089,-1.54484,-1.74234,-1.96839,-2.18038/   cph24170
      DATA CB2H2O/                                                      cph24180
     X-2.28394,-2.31701,-2.24125,-2.05966,-1.88694,-1.78105,-1.69960,   cph24190
     X-1.64107,-1.62909,-1.58168,-1.56599,-1.59412,-1.56739,-1.56346,   cph24200
     X-1.54456,-1.55619,-1.61000,-1.67072,-1.75312,-1.82511,-1.87588,   cph24210
     X-1.89436,-1.94377,-1.96038,-2.02291,-2.14131,-2.19637,-2.27114,   cph24220
     X-2.33418,-2.36152,-2.44688,-2.53819,-2.61011,-2.69105,-2.73774,   cph24230
     X-2.76700,-2.82031,-2.85910,-2.88525,-2.95422,-2.99210,-3.06247,   cph24240
     X-3.12280,-3.12274,-3.13972,-3.09998,-3.11771,-3.10670,-3.00116,   cph24250
     8-2.91302,-2.75838,-2.66379,-2.65726,-2.62212,-2.59431,-2.55351,   cph24260
     X-2.49874,-2.47486,-2.52563,-2.54608,-2.54311,-2.54938,-2.49038,   cph24270
     X-2.49664,-2.52688,-2.58688,-2.67000,-2.71830,-2.77113,-2.80244,   cph24280
     X-2.84845,-2.87062,-2.83368,-2.69338,-2.52225,-2.40864,-2.34429,   cph24290
     X-2.40612,-2.55941,-2.73915,-2.94409,-3.12344,-3.27308,-3.32104,   cph24300
     X-3.27077,-3.13129,-2.92504,-2.78515,-2.71007,-2.66733,-2.62518,   cph24310
     X-2.62279,-2.59906,-2.56745,-2.59548,-2.53657,-2.50849,-2.47640,   cph24320
     X-2.46021,-2.53747,-2.62220,-2.76449,-2.88041,-2.96357,-3.02153,   cph24330
     6-3.06178,-3.14581,-3.25318,-3.44687,-3.69634,-3.90497,-4.09399,   cph24340
     X-4.22346,-4.29749,-4.51957,-4.79572,-5.03708,-5.27140,-5.34657,   cph24350
     8-5.44757,-5.52207,-5.57087,-5.64385,-5.80294,-5.90763,-5.94797/   cph24360
      DATA CB3H2O/                                                      cph24370
     X-5.85439,-5.62721,-5.45121,-5.40324,-5.38540,-5.39409,-5.59404,   cph24380
     X-5.69955,-5.76877,-5.86764,-5.78129,-5.88887,-6.12206,-6.37505,   cph24390
     X-6.85575,-7.13884,-6.98622,-6.96112,-6.84830,-6.72456,-6.67530,   cph24400
     X-6.65251,-6.66033,-6.88151,-7.11199,-7.33922,-7.61766,-7.66585,   cph24410
     X-7.87464,-8.59519,-9.04047,-9.30602,-9.51273,-8.93688,-9.43151,   cph24420
     X-8.84005,-10.0000,-9.65151,-8.98420,-10.0000,-9.43151,-9.68331,   cph24430
     X-10.0000,-9.43067,-9.90527,-10.0000,-9.98810,-9.65151,-9.74384,   cph24440
     8-9.20004,-10.0000,-9.20004,-10.0000,-10.0000/                     cph24450
C=H2O ====13075-14860                                                   cph24460
      DATA CC1H2O/                                                      cph24470
     X-9.85239,-8.09585,-7.66916,-7.89183,-8.46587,-8.79342,-8.93440,   cph24480
     X-8.68356,-8.83423,-8.01626,-7.91911,-8.27604,-9.44864,-9.69462,   cph24490
     X-10.0000,-9.69462,-8.31857,-7.91867,-7.86404,-8.32240,-8.68705,   cph24500
     X-9.61515,-9.25284,-8.68705,-8.28789,-7.63730,-8.25919,-10.0000,   cph24510
     X-9.51758,-10.0000,-10.0000,-9.51758,-8.05261,-7.76848,-8.24255,   cph24520
     X-9.34171,-9.19941,-8.56505,-7.78955,-7.23750,-6.64136,-6.41854,   cph24530
     X-6.14335,-5.86704,-5.70840,-5.48179,-5.23814,-5.00650,-4.80407,   cph24540
     8-4.69986,-4.70404,-4.80846,-4.99355,-5.19947,-5.33300,-5.30550,   cph24550
     X-5.17017,-5.05309,-4.95685,-4.79959,-4.65496,-4.54077,-4.44407,   cph24560
     X-4.43768,-4.47167,-4.40429,-4.30236,-4.22549,-4.15283,-4.06900,   cph24570
     X-3.99244,-3.87562,-3.76949,-3.78198,-3.77003,-3.67364,-3.52061,   cph24580
     X-3.34459,-3.20426,-3.15993,-3.13851,-3.09692,-3.07747,-3.02936,   cph24590
     X-3.02192,-2.96720,-2.90584,-2.79069,-2.65042,-2.62072,-2.53133,   cph24600
     X-2.50554,-2.48745,-2.41611,-2.43532,-2.40913,-2.38723,-2.33337,   cph24610
     X-2.21812,-2.15072,-2.08313,-2.06151,-2.10585,-2.13670,-2.18757,   cph24620
     6-2.23995,-2.26992,-2.34229,-2.38059,-2.38087,-2.33246,-2.21618,   cph24630
     X-2.14795,-2.12707,-2.09130,-2.05043,-1.95550,-1.77077,-1.66044,   cph24640
     8-1.58224,-1.51702,-1.54004,-1.54074,-1.53712,-1.54938,-1.52352/   cph24650
      DATA CC2H2O/                                                      cph24660
     X-1.49404,-1.51985,-1.57774,-1.64393,-1.65332,-1.56238,-1.45105,   cph24670
     X-1.39264,-1.40635,-1.46167,-1.50365,-1.47751,-1.47268,-1.45992,   cph24680
     X-1.46654,-1.50223,-1.47576,-1.47730,-1.46977,-1.45630,-1.44490,   cph24690
     X-1.43097,-1.43664,-1.49313,-1.63372,-1.81848,-1.97938,-2.06009,   cph24700
     X-2.10124,-2.02376,-1.95095,-1.86835,-1.70161,-1.54835,-1.37614,   cph24710
     X-1.25130,-1.17408,-1.17670,-1.19790,-1.21344,-1.27252,-1.28030,   cph24720
     X-1.31031,-1.34321,-1.34056,-1.35264,-1.38137,-1.44648,-1.56735,   cph24730
     8-1.72096,-1.88960,-2.06793,-2.19583,-2.29229,-2.34400,-2.34120,   cph24740
     X-2.36407,-2.39688,-2.45450,-2.53132,-2.58327,-2.60585,-2.60773,   cph24750
     X-2.60606,-2.64243,-2.71853,-2.78182,-2.84459,-2.83665,-2.78522,   cph24760
     X-2.71157,-2.61628,-2.53314,-2.45692,-2.41679,-2.41535,-2.45500,   cph24770
     X-2.51120,-2.57918,-2.62854,-2.63617,-2.61204,-2.53833,-2.43992,   cph24780
     X-2.37490,-2.34880,-2.34476,-2.36650,-2.36553,-2.34094,-2.33633,   cph24790
     X-2.30483,-2.26933,-2.25141,-2.22841,-2.27610,-2.33673,-2.37913,   cph24800
     X-2.44271,-2.48571,-2.57146,-2.64200,-2.61103,-2.47198,-2.27897,   cph24810
     6-2.15874,-2.06557,-2.05371,-2.06333,-2.04641,-2.04429,-2.01040,   cph24820
     X-2.00804,-1.99416,-2.05499,-2.09948,-2.09706,-2.10517,-2.01267,   cph24830
     8-1.99934,-2.03719,-2.12114,-2.29537,-2.44295,-2.55926,-2.66007/   cph24840
      DATA CC3H2O/                                                      cph24850
     X-2.73808,-2.75972,-2.78032,-2.67031,-2.44995,-2.27133,-2.11654,   cph24860
     X-2.02598,-2.01402,-2.04264,-2.04511,-2.02975,-2.00687,-1.94840,   cph24870
     X-1.93975,-1.97104,-2.01554,-2.09336,-2.15829,-2.26705,-2.40356,   cph24880
     X-2.55216,-2.78858,-3.00697,-3.22778,-3.44413,-3.55439,-3.66412,   cph24890
     X-3.73884,-3.92224,-4.18922,-4.41150,-4.55122,-4.48637,-4.29339,   cph24900
     X-4.19248,-4.28419,-4.41178,-4.60369,-4.81645,-4.83716,-4.93818,   cph24910
     X-4.87772,-4.65255,-4.40133,-4.14378,-4.05431,-4.02425,-4.04257,   cph24920
     8-4.11794,-4.12153,-4.16374,-4.17459,-4.10274,-4.04733,-4.00176,   cph24930
     X-4.01760,-4.13393,-4.29085,-4.38409,-4.39975,-4.33916,-4.31515,   cph24940
     X-4.35426,-4.44628,-4.51842,-4.52596,-4.53399,-4.54590,-4.63864,   cph24950
     X-4.75657,-4.86504,-4.95580,-5.03365,-5.14879,-5.33868,-5.62972,   cph24960
     X-5.92584,-6.30299,-6.62922,-6.70773,-6.97810,-7.35919,-7.64909,   cph24970
     X-8.62765,-8.55378,-7.76305,-7.47054,-7.07789,-7.11538,-7.34052,   cph24980
     X-7.75694,-9.17126,-10.0000,-9.86547,-8.71424,-8.66552,-8.31602,   cph24990
     X-8.41339,-7.92192,-8.66385,-8.99856,-9.65437,-9.36822,-9.46517,   cph25000
     6-9.43986/                                                         cph25010
C=H2O ====14865-16045                                                   cph25020
      DATA CD1H2O/                                                      cph25030
     X-8.65941,-10.0000,-10.0000,-8.82641,-8.56244,-7.93689,-7.68823,   cph25040
     X-7.55818,-7.05113,-6.76446,-6.49313,-6.24749,-6.12617,-6.05220,   cph25050
     X-6.13798,-6.07909,-5.86845,-5.69141,-5.50496,-5.48376,-5.56108,   cph25060
     X-5.42768,-5.29615,-5.10664,-4.88111,-4.78669,-4.62385,-4.52174,   cph25070
     X-4.49073,-4.45792,-4.54129,-4.54480,-4.51341,-4.47258,-4.27643,   cph25080
     X-4.18091,-4.09557,-4.04222,-4.11247,-4.14851,-4.16970,-4.11065,   cph25090
     X-4.04809,-4.00745,-3.99879,-4.07978,-4.12451,-4.19723,-4.17393,   cph25100
     8-4.09022,-4.02101,-3.87998,-3.79109,-3.66411,-3.50066,-3.40580,   cph25110
     X-3.32713,-3.30194,-3.35131,-3.35137,-3.29933,-3.20658,-3.06263,   cph25120
     X-2.97995,-2.98759,-2.99176,-3.00756,-2.97359,-2.85849,-2.81640,   cph25130
     X-2.77094,-2.75469,-2.77297,-2.71165,-2.69187,-2.64524,-2.60542,   cph25140
     X-2.60059,-2.57842,-2.59991,-2.58577,-2.60792,-2.66006,-2.70803,   cph25150
     X-2.79094,-2.81048,-2.79532,-2.79499,-2.84578,-2.90638,-2.96270,   cph25160
     X-2.90633,-2.71535,-2.54313,-2.37822,-2.31125,-2.35246,-2.49011,   cph25170
     X-2.68215,-2.83136,-2.96357,-2.95873,-2.90544,-2.84387,-2.70352,   cph25180
     6-2.58329,-2.49207,-2.41735,-2.35522,-2.30279,-2.25786,-2.22067,   cph25190
     X-2.20741,-2.19735,-2.20181,-2.22358,-2.27247,-2.33737,-2.39631,   cph25200
     8-2.45029,-2.49867,-2.56939,-2.64313,-2.77129,-2.92580,-3.05513/   cph25210
      DATA CD2H2O/                                                      cph25220
     X-3.23728,-3.31415,-3.33588,-3.39544,-3.43947,-3.57455,-3.69955,   cph25230
     X-3.77227,-3.76260,-3.70753,-3.70942,-3.73899,-3.82827,-3.93052,   cph25240
     X-4.10437,-4.24931,-4.35000,-4.42069,-4.25644,-4.21454,-4.17061,   cph25250
     X-4.11168,-4.16038,-4.16686,-4.19465,-4.23251,-4.27305,-4.21672,   cph25260
     X-4.13963,-4.07622,-3.97332,-3.96263,-3.95541,-3.97392,-4.03776,   cph25270
     X-4.07778,-4.01771,-3.87070,-3.70710,-3.59495,-3.62243,-3.69528,   cph25280
     X-3.76171,-3.76582,-3.65793,-3.61555,-3.59708,-3.63113,-3.63691,   cph25290
     8-3.57465,-3.55435,-3.47507,-3.49075,-3.53253,-3.57495,-3.68837,   cph25300
     X-3.68628,-3.68771,-3.64979,-3.60831,-3.56633,-3.48250,-3.37856,   cph25310
     X-3.22908,-3.14286,-3.11346,-3.13691,-3.26625,-3.44333,-3.64611,   cph25320
     X-3.86925,-4.08285,-4.22794,-4.25115,-4.14282,-3.85944,-3.59027,   cph25330
     X-3.43514,-3.31856,-3.24442,-3.22555,-3.18795,-3.20363,-3.30589,   cph25340
     X-3.41248,-3.60718,-3.70563,-3.65431,-3.57332,-3.47347,-3.47521,   cph25350
     X-3.53388,-3.72003,-3.97569,-4.31048,-4.87330,-5.39648,-6.27322,   cph25360
     X-8.18185,-8.07588,-8.20933,-8.60643,-8.83713,-9.01727,-9.15690,   cph25370
     6-9.41970,-9.51520,-9.63843,-9.87539,-9.94314,-10.0000/            cph25380
C=H2O ====16340-17860                                                   cph25390
      DATA CE1H2O/                                                      cph25400
     X-10.0000,-9.99542,-9.97748,-9.94374,-9.93287,-9.90450,-9.85082,   cph25410
     X-9.82140,-9.73549,-9.64536,-9.59412,-9.54635,-9.26735,-9.23243,   cph25420
     X-9.05763,-8.93240,-8.74549,-8.52992,-8.25637,-8.13836,-7.66071,   cph25430
     X-7.35897,-7.37375,-7.09925,-6.98326,-6.89298,-6.79545,-6.97172,   cph25440
     X-6.67558,-6.37369,-6.21189,-5.94606,-5.84975,-5.83536,-5.82878,   cph25450
     X-5.78456,-5.68334,-5.44809,-5.28421,-5.06970,-4.89514,-4.80192,   cph25460
     X-4.73588,-4.78558,-4.78127,-4.73462,-4.54889,-4.33093,-4.18543,   cph25470
     8-4.09190,-4.11204,-4.13402,-4.13401,-3.97210,-3.79621,-3.65860,   cph25480
     X-3.55511,-3.57549,-3.57633,-3.53833,-3.46143,-3.34082,-3.23729,   cph25490
     X-3.17300,-3.14437,-3.10547,-3.05061,-2.96941,-2.86694,-2.79500,   cph25500
     X-2.75350,-2.75307,-2.77146,-2.79530,-2.76451,-2.68758,-2.63931,   cph25510
     X-2.57797,-2.58894,-2.59717,-2.52817,-2.47282,-2.42360,-2.45382,   cph25520
     X-2.56145,-2.61304,-2.59963,-2.52689,-2.46472,-2.46461,-2.45407,   cph25530
     X-2.39432,-2.25523,-2.14408,-2.05525,-2.01888,-2.07413,-2.12889,   cph25540
     X-2.25990,-2.39692,-2.48925,-2.54855,-2.53415,-2.54460,-2.50455,   cph25550
     6-2.46921,-2.42259,-2.28066,-2.22625,-2.17393,-2.13289,-2.19687,   cph25560
     X-2.21326,-2.23949,-2.27620,-2.26819,-2.29009,-2.29281,-2.25201,   cph25570
     8-2.17355,-2.07947,-2.03121,-2.01967,-2.04954,-2.08143,-2.06833/   cph25580
      DATA CE2H2O/                                                      cph25590
     X-2.05240,-2.05599,-2.06967,-2.12334,-2.21510,-2.29897,-2.40035,   cph25600
     X-2.52428,-2.62702,-2.73003,-2.87671,-2.99894,-3.10548,-3.25316,   cph25610
     X-3.32982,-3.39709,-3.53992,-3.63406,-3.74020,-3.92706,-3.96893,   cph25620
     X-3.93910,-3.93559,-3.82934,-3.82006,-3.87551,-3.89939,-3.94509,   cph25630
     X-3.95617,-3.96332,-3.96114,-3.99122,-4.01273,-4.01717,-4.02888,   cph25640
     X-4.04697,-4.10112,-4.14864,-4.27169,-4.32135,-4.33175,-4.41165,   cph25650
     X-4.36331,-4.39914,-4.42505,-4.40381,-4.48901,-4.44885,-4.38473,   cph25660
     8-4.32458,-4.19760,-4.16511,-4.15683,-4.14102,-4.11365,-4.10673,   cph25670
     X-4.13026,-4.13652,-4.19636,-4.19684,-4.14832,-4.05676,-3.96205,   cph25680
     X-3.90165,-3.84404,-3.86524,-3.83773,-3.69609,-3.55481,-3.42043,   cph25690
     X-3.33841,-3.37637,-3.44611,-3.49193,-3.52932,-3.44601,-3.36757,   cph25700
     X-3.31227,-3.23777,-3.21254,-3.19842,-3.22310,-3.28352,-3.27914,   cph25710
     X-3.23481,-3.12437,-3.04729,-3.06777,-3.09818,-3.19530,-3.24569,   cph25720
     X-3.24974,-3.30729,-3.27728,-3.25317,-3.22055,-3.15996,-3.17334,   cph25730
     X-3.17694,-3.12288,-3.04593,-2.99049,-2.98361,-3.06492,-3.19818,   cph25740
     6-3.31628,-3.42190,-3.47775,-3.55095,-3.56669,-3.53409,-3.38883,   cph25750
     X-3.17115,-3.00955,-2.89158,-2.83770,-2.86055,-2.86096,-2.83436,   cph25760
     8-2.82886,-2.78602,-2.80289,-2.85454,-2.89629,-2.99573,-3.11206/   cph25770
      DATA CE3H2O/                                                      cph25780
     X-3.27394,-3.47183,-3.64849,-3.79741,-3.91130,-4.08705,-4.24317,   cph25790
     X-4.41275,-4.55729,-4.55082,-4.66958,-4.82149,-4.94204,-5.13772,   cph25800
     X-5.22105,-5.20710,-5.18691,-5.09729,-5.03217,-4.96344,-4.97810,   cph25810
     X-5.03506,-5.05380,-5.08007,-5.10835,-5.13285,-5.24491,-5.44530,   cph25820
     X-5.65236,-5.80563,-5.75192,-5.58691,-5.38023,-5.31721,-5.30923,   cph25830
     X-5.34087,-5.39044,-5.38089,-5.43438,-5.52124,-5.79590,-6.25048,   cph25840
     X-6.78272,-8.29899,-8.50913,-8.77871,-8.91512,-9.13453,-9.37455,   cph25850
     8-9.56578,-9.71290,-9.89385,-10.0000/                              cph25860
      END                                                               cph25870
      BLOCK DATA CPO3                                                   cpo3 100
C>    BLOCK DATA                                                        cpo3 110
C                                                                       cpo3 120
C     C' FOR O3                                                         cpo3 130
      COMMON /O3/  C11O3(  41),                                         cpo3 140
     +             C21O3( 126),C22O3(  27),                             cpo3 150
     +             C31O3( 126),C32O3(   8),                             cpo3 160
     +             C41O3(  36),                                         cpo3 170
     +             C51O3(  83)                                          cpo3 180
C=O3  ====C' FOR    5 BAND MODELS                                       cpo3 190
C=O3  ====    0-  200                                                   cpo3 200
      DATA C11O3/                                                       cpo3 210
     1 -2.0427, -1.8966, -1.6263, -1.3896, -1.2170, -1.0996, -1.0214,   cpo3 220
     2 -0.9673, -0.9249, -0.8896, -0.8612, -0.8417, -0.8360, -0.8483,   cpo3 230
     3 -0.8785, -0.9273, -0.9932, -1.0720, -1.1639, -1.2662, -1.3771,   cpo3 240
     4 -1.4976, -1.6274, -1.7712, -1.9289, -2.1027, -2.2948, -2.4987,   cpo3 250
     5 -2.7321, -2.9992, -3.3045, -3.6994, -4.1022, -4.6467, -5.1328,   cpo3 260
     6 -5.6481, -6.1634, -6.6787, -7.1940, -7.7093, -8.0000/            cpo3 270
C=O3  ====  515- 1275                                                   cpo3 280
      DATA C21O3/                                                       cpo3 290
     1 -7.9274, -7.6418, -7.3562, -7.0706, -6.7850, -6.4994, -6.2138,   cpo3 300
     2 -5.9282, -5.6426, -5.3570, -5.0714, -4.7858, -4.5002, -4.2146,   cpo3 310
     3 -3.9290, -3.6213, -3.3407, -3.0722, -2.8226, -2.5914, -2.3778,   cpo3 320
     4 -2.1823, -2.0057, -1.8456, -1.6991, -1.5659, -1.4436, -1.3323,   cpo3 330
     5 -1.2319, -1.1407, -1.0550, -0.9733, -0.9033, -0.8584, -0.8527,   cpo3 340
     6 -0.8838, -0.9219, -0.9360, -0.9025, -0.8402, -0.7913, -0.7794,   cpo3 350
     7 -0.8123, -0.8750, -0.9484, -1.0206, -1.0864, -1.1520, -1.2202,   cpo3 360
     8 -1.2928, -1.3745, -1.4641, -1.5611, -1.6669, -1.7816, -1.9051,   cpo3 370
     9 -2.0383, -2.1796, -2.3312, -2.4906, -2.6569, -2.8354, -3.0179,   cpo3 380
     $ -3.2121, -3.4106, -3.6208, -3.8332, -4.0584, -4.2854, -4.4979,   cpo3 390
     1 -4.7175, -4.9109, -5.1246, -5.3344, -5.5442, -5.7540, -5.9638,   cpo3 400
     2 -6.1736, -6.3834, -6.5932, -6.8030, -7.0128, -6.9011, -6.2590,   cpo3 410
     3 -5.8119, -5.1603, -4.3327, -3.6849, -3.1253, -2.6304, -2.1903,   cpo3 420
     4 -1.8019, -1.4585, -1.1533, -0.8770, -0.6166, -0.3630, -0.1102,   cpo3 430
     5  0.1336,  0.3525,  0.5326,  0.6678,  0.7510,  0.7752,  0.7826,   cpo3 440
     6  0.7874,  0.8006,  0.8241,  0.7614,  0.5662,  0.1949, -0.2770,   cpo3 450
     7 -0.6199, -0.8347, -0.9586, -1.0168, -1.0501, -1.0816, -1.0980,   cpo3 460
     8 -1.0833, -1.0424, -0.9972, -0.9724, -0.9855, -1.0365, -1.1187/   cpo3 470
      DATA C22O3/                                                       cpo3 480
     1 -1.2150, -1.3142, -1.4103, -1.4998, -1.5933, -1.6938, -1.8061,   cpo3 490
     2 -1.9332, -2.0737, -2.2279, -2.3966, -2.5787, -2.7755, -2.9855,   cpo3 500
     3 -3.2090, -3.4465, -3.6967, -3.9633, -4.2461, -4.5502, -4.8912,   cpo3 510
     4 -5.2845, -5.7654, -6.4194, -6.9288, -7.4382, -7.9476/            cpo3 520
C=O3  ==== 1630- 2295                                                   cpo3 530
      DATA C31O3/                                                       cpo3 540
     1 -8.0000, -7.5432, -6.9273, -6.3115, -5.5431, -4.9563, -4.4640,   cpo3 550
     2 -4.0371, -3.6533, -3.3069, -2.9877, -2.7042, -2.4507, -2.2355,   cpo3 560
     3 -2.0651, -1.9477, -1.8705, -1.8422, -1.8235, -1.7782, -1.7367,   cpo3 570
     4 -1.7012, -1.7208, -1.8353, -2.0331, -2.3077, -2.5996, -2.7517,   cpo3 580
     5 -2.7263, -2.6671, -2.6415, -2.6449, -2.6613, -2.6589, -2.6083,   cpo3 590
     6 -2.5250, -2.4529, -2.4157, -2.4298, -2.4906, -2.5823, -2.6873,   cpo3 600
     7 -2.7808, -2.8612, -2.9303, -3.0022, -3.0873, -3.1844, -3.2929,   cpo3 610
     8 -3.4158, -3.5361, -3.6710, -3.8062, -3.9520, -4.1140, -4.2635,   cpo3 620
     9 -4.4395, -4.6138, -4.8372, -5.0837, -5.3302, -5.3665, -5.4358,   cpo3 630
     $ -5.0651, -4.8416, -4.5293, -4.2547, -4.0039, -3.7818, -3.5850,   cpo3 640
     1 -3.4091, -3.2509, -3.0934, -2.9485, -2.8055, -2.6705, -2.5482,   cpo3 650
     2 -2.4362, -2.3380, -2.2486, -2.1645, -2.0834, -2.0035, -1.9081,   cpo3 660
     3 -1.7681, -1.5768, -1.3615, -1.1463, -0.9482, -0.7800, -0.6336,   cpo3 670
     4 -0.5092, -0.4105, -0.3495, -0.3274, -0.3133, -0.3023, -0.2859,   cpo3 680
     5 -0.3055, -0.4374, -0.6972, -1.1064, -1.4904, -1.9687, -2.4498,   cpo3 690
     7 -2.5971, -2.5220, -2.4301, -2.3467, -2.2901, -2.2746, -2.3021,   cpo3 700
     7 -2.3635, -2.4420, -2.5088, -2.5485, -2.5617, -2.5656, -2.5771,   cpo3 710
     8 -2.6134, -2.6822, -2.7885, -2.9379, -3.1200, -3.3260, -3.5464/   cpo3 720
      DATA C32O3/                                                       cpo3 730
     1 -3.7736, -4.0311, -4.3651, -4.7794, -5.5152, -6.1240, -7.2193,   cpo3 740
     2 -8.0000/                                                         cpo3 750
C=O3  ==== 2670- 2845                                                   cpo3 760
      DATA C41O3/                                                       cpo3 770
     1 -7.9721, -7.6118, -7.2515, -6.8913, -6.5310, -6.1707, -5.8105,   cpo3 780
     2 -5.4502, -5.0899, -4.7297, -4.3694, -3.9462, -3.6022, -3.2886,   cpo3 790
     3 -3.0234, -2.7863, -2.5797, -2.4073, -2.2760, -2.1894, -2.1359,   cpo3 800
     4 -2.1160, -2.0808, -2.0151, -1.9666, -1.9409, -1.9868, -2.1450,   cpo3 810
     5 -2.3965, -2.8042, -3.5500, -4.8275, -5.6378, -6.4482, -7.2585,   cpo3 820
     6 -8.0000/                                                         cpo3 830
C=O3  ==== 2850- 3260                                                   cpo3 840
      DATA C51O3/                                                       cpo3 850
     1 -8.0000, -7.6278, -7.2556, -6.8834, -6.5111, -6.1389, -5.7667,   cpo3 860
     2 -5.3945, -5.0223, -4.6501, -4.2779, -3.9056, -3.5334, -3.3828,   cpo3 870
     3 -3.2452, -3.1411, -3.0403, -2.9428, -2.8436, -2.7573, -2.6853,   cpo3 880
     4 -2.6040, -2.5218, -2.4121, -2.3547, -2.1970, -2.0668, -1.9121,   cpo3 890
     5 -1.7617, -1.6153, -1.4688, -1.4022, -1.3447, -1.2669, -1.1902,   cpo3 900
     6 -1.1805, -1.1707, -1.1609, -1.1609, -1.1805, -1.1999, -1.4214,   cpo3 910
     7 -1.6348, -1.7519, -1.9730, -2.2078, -2.4608, -2.5337, -2.5923,   cpo3 920
     8 -2.6616, -2.6384, -2.6271, -2.6154, -2.5570, -2.4983, -2.4480,   cpo3 930
     9 -2.3890, -2.3663, -2.3431, -2.3314, -2.3200, -2.3200, -2.3314,   cpo3 940
     $ -2.3431, -2.3547, -2.3777, -2.4004, -2.5218, -2.6499, -2.7694,   cpo3 950
     1 -2.9057, -3.0286, -3.1543, -3.3696, -3.6053, -4.1977, -4.7811,   cpo3 960
     2 -5.2933, -5.7554, -6.4542, -7.0239, -7.5937, -8.0000/            cpo3 970
      END                                                               cpo3 980
      BLOCK DATA CPTRCG                                                 cpcg 100
C>    BLOCK DATA                                                        cpcg 110
C                                                                       cpcg 120
C     C' FOR TRACE GASES (NH3, NO, NO2, SO2)                            cpcg 130
      COMMON /TRACEG/  C11NH3( 78),                                     cpcg 140
     +                 C21NH3(126),C22NH3(126),C23NH3(101),             cpcg 150
     +                 C11NO ( 62),                                     cpcg 160
     +                 C11NO2(126),C12NO2( 16),                         cpcg 170
     +                 C11SO2( 38),                                     cpcg 180
     +                 C21SO2(126),C22SO2( 62)                          cpcg 190
C=NH3 ====C' FOR    2 BAND MODEL                                        cpcg 200
C=NH3 ====    0-  385                                                   cpcg 210
      DATA C11NH3/                                                      cpcg 220
     1 -5.7142, -5.2854, -4.5163, -3.9795, -3.4393, -2.8735, -2.4947,   cpcg 230
     2 -2.2290, -2.0624, -1.9616, -1.8707, -1.7712, -1.6473, -1.5376,   cpcg 240
     3 -1.4315, -1.3328, -1.2391, -1.1768, -1.1302, -1.0755, -1.0272,   cpcg 250
     4 -0.9884, -0.9501, -0.9287, -0.9101, -0.8982, -0.8888, -0.8709,   cpcg 260
     5 -0.8620, -0.8645, -0.8676, -0.8910, -0.9084, -0.9328, -0.9546,   cpcg 270
     6 -0.9743, -0.9983, -1.0202, -1.0569, -1.0824, -1.1086, -1.1475,   cpcg 280
     7 -1.1790, -1.2059, -1.2668, -1.3237, -1.3801, -1.4271, -1.4920,   cpcg 290
     8 -1.5403, -1.5848, -1.6498, -1.7382, -1.8294, -1.9203, -2.0694,   cpcg 300
     9 -2.2134, -2.3622, -2.5516, -2.7633, -2.9344, -3.1172, -3.3543,   cpcg 310
     $ -3.5671, -3.7504, -3.9884, -4.2633, -4.5505, -4.7837, -5.0350,   cpcg 320
     1 -5.3733, -5.6478, -5.8856, -6.1041, -6.3375, -6.5709, -6.8043,   cpcg 330
     2 -7.0377/                                                         cpcg 340
C=NH3 ====  390- 2150                                                   cpcg 350
      DATA C21NH3/                                                      cpcg 360
     1 -7.2620, -7.0950, -6.9279, -6.7608, -6.5938, -6.4267, -6.2597,   cpcg 370
     2 -6.0926, -5.8842, -5.7560, -5.5844, -5.4248, -5.2573, -5.0771,   cpcg 380
     3 -4.9244, -4.7903, -4.6512, -4.5169, -4.3961, -4.2607, -4.1705,   cpcg 390
     4 -4.1294, -4.0611, -3.9538, -3.8821, -3.7592, -3.6754, -3.6830,   cpcg 400
     5 -3.6977, -3.6925, -3.6632, -3.5899, -3.5218, -3.5265, -3.6535,   cpcg 410
     6 -3.8068, -3.9818, -4.0574, -3.9789, -3.8858, -3.8120, -3.8927,   cpcg 420
     7 -3.8799, -3.8623, -3.3984, -2.8857, -2.5814, -2.4066, -2.3850,   cpcg 430
     8 -2.5415, -2.8161, -3.2265, -3.7177, -3.9932, -4.0683, -4.0785,   cpcg 440
     9 -3.9912, -3.7418, -3.4742, -3.2651, -3.0715, -2.9500, -2.8669,   cpcg 450
     $ -2.7723, -2.6614, -2.5613, -2.4372, -2.3085, -2.1696, -2.0302,   cpcg 460
     1 -1.9166, -1.8071, -1.7221, -1.6370, -1.5453, -1.4487, -1.3539,   cpcg 470
     2 -1.2570, -1.1618, -1.1131, -1.0824, -1.0559, -1.0190, -0.9721,   cpcg 480
     3 -0.9218, -0.8680, -0.8556, -0.8568, -0.8713, -0.8984, -0.9076,   cpcg 490
     4 -0.9024, -0.8882, -0.8968, -0.9492, -1.0089, -1.0846, -1.1556,   cpcg 500
     5 -1.1792, -1.1946, -1.1964, -1.2173, -1.2424, -1.1744, -0.9743,   cpcg 510
     6 -0.6350, -0.2975, -0.0705,  0.0144, -0.0978, -0.3536, -0.5630,   cpcg 520
     7 -0.5479, -0.3784, -0.1797, -0.1151, -0.3085, -0.6180, -0.9718,   cpcg 530
     8 -1.2926, -1.2748, -1.1217, -1.0197, -0.9300, -0.8817, -0.8723/   cpcg 540
      DATA C22NH3/                                                      cpcg 550
     1 -0.8309, -0.7804, -0.7075, -0.6431, -0.6176, -0.6012, -0.6079,   cpcg 560
     2 -0.6272, -0.6304, -0.6193, -0.6026, -0.5882, -0.6029, -0.6317,   cpcg 570
     3 -0.6862, -0.7447, -0.7921, -0.8275, -0.8595, -0.8856, -0.9236,   cpcg 580
     4 -0.9934, -1.0693, -1.1460, -1.2100, -1.2863, -1.3593, -1.4292,   cpcg 590
     5 -1.5029, -1.6054, -1.7067, -1.8110, -1.9350, -2.0346, -2.1305,   cpcg 600
     6 -2.2294, -2.3724, -2.4917, -2.6218, -2.8056, -2.9693, -3.1101,   cpcg 610
     7 -3.2790, -3.5315, -3.7011, -3.8952, -4.1527, -4.4121, -4.5244,   cpcg 620
     8 -4.8599, -5.1940, -5.5589, -5.8170, -6.1402, -6.4633, -6.7865,   cpcg 630
     9 -7.1096, -7.4328, -7.7559, -8.0000, -7.8199, -7.5988, -7.3778,   cpcg 640
     $ -7.1567, -6.9357, -6.7146, -6.4936, -6.2725, -6.0515, -5.8304,   cpcg 650
     1 -5.5963, -5.3883, -5.2319, -5.0536, -4.9029, -4.7789, -4.5867,   cpcg 660
     2 -4.3414, -4.1399, -3.9784, -3.7553, -3.5773, -3.4123, -3.2254,   cpcg 670
     3 -3.0384, -2.9243, -2.7755, -2.5809, -2.4726, -2.3206, -2.1209,   cpcg 680
     4 -2.0331, -1.9016, -1.7458, -1.6927, -1.5958, -1.4863, -1.4492,   cpcg 690
     5 -1.3730, -1.2859, -1.2554, -1.2129, -1.1689, -1.1802, -1.1948,   cpcg 700
     6 -1.1882, -1.2185, -1.2464, -1.2522, -1.2946, -1.3587, -1.3971,   cpcg 710
     7 -1.4488, -1.5261, -1.5495, -1.5478, -1.4926, -1.3115, -1.0455,   cpcg 720
     8 -0.7987, -0.5972, -0.4664, -0.4244, -0.4426, -0.4952, -0.5772/   cpcg 730
      DATA C23NH3/                                                      cpcg 740
     1 -0.6845, -0.8097, -0.9443, -1.0904, -1.2232, -1.2853, -1.2949,   cpcg 750
     2 -1.2708, -1.1896, -1.1467, -1.1187, -1.0700, -1.0392, -1.0227,   cpcg 760
     3 -1.0178, -1.0089, -1.0021, -0.9706, -0.9569, -0.9928, -1.0310,   cpcg 770
     4 -1.0767, -1.1053, -1.1241, -1.1717, -1.2203, -1.2772, -1.3356,   cpcg 780
     5 -1.3855, -1.4734, -1.5701, -1.6572, -1.7638, -1.8652, -1.9918,   cpcg 790
     6 -2.1449, -2.2388, -2.3251, -2.3936, -2.4525, -2.5998, -2.7147,   cpcg 800
     7 -2.7704, -2.7852, -2.7524, -2.7646, -2.8507, -3.0422, -3.2642,   cpcg 810
     8 -3.5201, -3.6328, -3.7624, -3.9505, -4.1399, -4.3087, -4.3859,   cpcg 820
     9 -4.4295, -4.4493, -4.3317, -4.1892, -4.0545, -3.9356, -3.9117,   cpcg 830
     $ -4.0001, -4.0627, -4.0833, -4.0997, -4.0659, -4.0264, -4.0893,   cpcg 840
     1 -4.1832, -4.2522, -4.3182, -4.3949, -4.4191, -4.4580, -4.5997,   cpcg 850
     2 -4.7282, -4.8370, -5.0041, -5.1644, -5.2101, -5.4145, -5.5114,   cpcg 860
     3 -5.6986, -5.8057, -5.9529, -6.1000, -6.2472, -6.3943, -6.5415,   cpcg 870
     4 -6.6886, -6.8358, -6.9829, -7.1301, -7.2772, -7.4244, -7.5715,   cpcg 880
     5 -7.7187, -7.8658, -8.0000/                                       cpcg 890
C=NO  ====C' FOR    1 BAND MODEL                                        cpcg 900
C=NO  ==== 1700- 2005                                                   cpcg 910
      DATA C11NO/                                                       cpcg 920
     1 -7.9265, -7.5649, -7.2033, -6.8418, -6.4802, -6.0647, -5.7193,   cpcg 930
     2 -5.3955, -5.1475, -4.8233, -4.5194, -4.3184, -3.9664, -3.7045,   cpcg 940
     3 -3.3398, -3.0368, -2.7282, -2.4448, -2.1791, -1.9315, -1.7046,   cpcg 950
     4 -1.4984, -1.3133, -1.1486, -1.0036, -0.8776, -0.7699, -0.6811,   cpcg 960
     5 -0.6124, -0.5663, -0.5488, -0.5673, -0.6076, -0.6791, -0.7553,   cpcg 970
     6 -0.7811, -0.7711, -0.6840, -0.5704, -0.4791, -0.4138, -0.3950,   cpcg 980
     7 -0.4189, -0.4794, -0.5751, -0.7062, -0.8751, -1.0852, -1.3406,   cpcg 990
     8 -1.6473, -2.0068, -2.4335, -2.9068, -3.4595, -4.0370, -4.6795,   cpcg1000
     9 -5.2704, -5.8613, -6.4522, -7.0431, -7.6340, -8.0000/            cpcg1010
C=NO2 ====C' FOR    1 BAND MODEL                                        cpcg1020
C=NO2 ====  580-  925, 1515- 1695, 2800- 2970                           cpcg1030
      DATA C11NO2/                                                      cpcg1040
     1 -6.0000, -5.8419, -5.5313, -5.1048, -4.9512, -4.5830, -4.2676,   cpcg1050
     2 -3.9783, -3.7150, -3.4782, -3.2541, -3.0597, -2.8625, -2.6989,   cpcg1060
     3 -2.5323, -2.3904, -2.2561, -2.1346, -2.0320, -1.9284, -1.8584,   cpcg1070
     4 -1.7778, -1.7222, -1.6776, -1.6024, -1.5658, -1.4917, -1.4117,   cpcg1080
     5 -1.3706, -1.3045, -1.2914, -1.3292, -1.3666, -1.4268, -1.4564,   cpcg1090
     6 -1.4076, -1.3284, -1.2804, -1.2497, -1.2519, -1.3123, -1.3704,   cpcg1100
     7 -1.4192, -1.4878, -1.5301, -1.5575, -1.5912, -1.6250, -1.6544,   cpcg1110
     8 -1.6849, -1.7340, -1.7748, -1.8171, -1.8679, -1.9256, -1.9809,   cpcg1120
     9 -2.0386, -2.1112, -2.1769, -2.2462, -2.3199, -2.4129, -2.5156,   cpcg1130
     $ -2.6575, -2.8825, -3.1831, -3.6209, -4.2271, -5.5290, -6.0000,   cpcg1140
     1 -6.0000, -5.5415, -4.8964, -4.2513, -3.6063, -2.9612, -2.1733,   cpcg1150
     2 -1.5514, -1.0260, -0.5817, -0.2030,  0.1231,  0.4098,  0.6653,   cpcg1160
     3  0.8885,  1.0716,  1.2025,  1.2697,  1.2926,  1.3006,  1.3128,   cpcg1170
     4  1.3449,  1.3656,  1.3245,  1.1868,  0.9310,  0.5907,  0.2056,   cpcg1180
     5 -0.2337, -0.7633, -1.4541, -2.4451, -3.1822, -3.9193, -4.6565,   cpcg1190
     6 -5.3936, -6.0000, -6.0000, -5.7606, -5.3422, -4.9238, -4.5055,   cpcg1200
     7 -4.0871, -3.6687, -3.2504, -2.8320, -2.3736, -1.9565, -1.5769,   cpcg1210
     8 -1.2400, -0.9384, -0.6781, -0.4630, -0.2944, -0.1783, -0.1213/   cpcg1220
      DATA C12NO2/                                                      cpcg1230
     1 -0.1033, -0.0934, -0.0723, -0.0267,  0.0016, -0.0394, -0.1700,   cpcg1240
     2 -0.4141, -0.7861, -1.2951, -2.0379, -3.0984, -3.8692, -4.6399,   cpcg1250
     3 -5.4107, -6.0000/                                                cpcg1260
C=SO2 ====C' FOR    2 BAND MODEL                                        cpcg1270
C=SO2 ====    0-  185                                                   cpcg1280
      DATA C11SO2/                                                      cpcg1290
     1 -0.9312, -0.8101, -0.5729, -0.3590, -0.2016, -0.0971, -0.0333,   cpcg1300
     2  0.0048,  0.0228,  0.0214, -0.0044, -0.0567, -0.1334, -0.2315,   cpcg1310
     3 -0.3451, -0.4741, -0.6198, -0.7854, -0.9764, -1.1922, -1.4326,   cpcg1320
     4 -1.6951, -1.9687, -2.2788, -2.6034, -2.9398, -3.3551, -3.7704,   cpcg1330
     5 -4.1857, -4.6010, -5.0163, -5.4316, -5.8469, -6.2622, -6.6775,   cpcg1340
     6 -7.0928, -7.5081, -7.9234/                                       cpcg1350
C=SO2 ====  400-  650,  950- 1460, 2415- 2580                           cpcg1360
      DATA C21SO2/                                                      cpcg1370
     1 -8.0000, -7.4209, -6.6994, -5.9778, -5.2563, -4.4248, -3.7369,   cpcg1380
     2 -3.0917, -2.5200, -2.0303, -1.6307, -1.3056, -1.0373, -0.8189,   cpcg1390
     3 -0.6395, -0.4880, -0.3574, -0.2369, -0.1237, -0.0261,  0.0250,   cpcg1400
     4  0.0186, -0.0194, -0.0659, -0.0638, -0.0065,  0.0468,  0.0682,   cpcg1410
     5  0.0355, -0.0431, -0.1334, -0.2175, -0.2954, -0.3738, -0.4588,   cpcg1420
     6 -0.5571, -0.6729, -0.8131, -0.9805, -1.1831, -1.4334, -1.7354,   cpcg1430
     7 -2.1065, -2.5705, -3.1238, -3.7691, -4.5793, -5.7012, -6.5603,   cpcg1440
     8 -7.4195, -8.0000, -7.9302, -7.6563, -7.3824, -7.1085, -6.8346,   cpcg1450
     9 -6.5607, -6.2868, -6.0129, -5.7390, -5.4651, -5.1912, -4.9173,   cpcg1460
     $ -4.6434, -4.3695, -4.0956, -3.8217, -3.5478, -3.2739, -3.0000,   cpcg1470
     1 -2.7261, -2.4522, -2.1783, -1.9317, -1.7073, -1.5004, -1.3136,   cpcg1480
     2 -1.1444, -0.9901, -0.8505, -0.7238, -0.6083, -0.5025, -0.4016,   cpcg1490
     3 -0.3047, -0.2112, -0.1263, -0.0656, -0.0414, -0.0509, -0.0731,   cpcg1500
     4 -0.0802, -0.0483,  0.0032,  0.0339,  0.0249, -0.0296, -0.1170,   cpcg1510
     5 -0.2141, -0.3069, -0.3968, -0.4881, -0.5881, -0.7019, -0.8299,   cpcg1520
     6 -0.9729, -1.1305, -1.3036, -1.4924, -1.7000, -1.9306, -2.1906,   cpcg1530
     7 -2.4959, -2.8613, -3.3176, -3.9236, -4.6847, -5.2561, -4.7082,   cpcg1540
     8 -4.1110, -3.6582, -3.1963, -2.7063, -1.9643, -1.3089, -0.6856/   cpcg1550
      DATA C22SO2/                                                      cpcg1560
     1 -0.0412,  0.3678,  0.6712,  0.9031,  1.0577,  1.1145,  1.1272,   cpcg1570
     2  1.1300,  1.1237,  1.1459,  1.1047,  0.9617,  0.7107,  0.3254,   cpcg1580
     3 -0.2322, -1.0612, -1.7715, -2.6089, -3.0225, -3.3542, -3.7339,   cpcg1590
     4 -4.1986, -4.7852, -5.6390, -6.2740, -6.9091, -7.5441, -8.0000,   cpcg1600
     5 -8.0000, -7.5698, -6.8815, -6.1933, -5.3530, -4.8602, -4.1286,   cpcg1610
     6 -2.9922, -2.3525, -1.8905, -1.5178, -1.2295, -1.0082, -0.8484,   cpcg1620
     7 -0.7634, -0.7340, -0.7203, -0.7167, -0.7097, -0.7297, -0.8391,   cpcg1630
     8 -1.0472, -1.3607, -1.7720, -2.2957, -3.0566, -4.1073, -4.5337,   cpcg1640
     9 -4.9481, -5.4542, -6.2445, -6.8148, -7.3850, -7.9553/            cpcg1650
      END                                                               cpcg1660
      BLOCK DATA CPUMIX                                                 cpmx 100
C>    BLOCK DATA                                                        cpmx 110
C                                                                       cpmx 120
C     C' FOR UNIFORMLY MIXED GASES (CO2, CO, CH4, N2O, O2)              cpmx 130
      COMMON /UFMIX1/                                                   cpmx 140
     +        C11CO2( 83),                                              cpmx 150
     +        C21CO2(121),                                              cpmx 160
     +        C31CO2(126),C32CO2( 85),                                  cpmx 170
     +        C41CO2(126),C42CO2( 12),                                  cpmx 180
     +        C51CO2( 62),                                              cpmx 190
     +        C61CO2(126),C62CO2( 45),                                  cpmx 200
     +        C71CO2(126),C72CO2( 99),                                  cpmx 210
     +        C81CO2(126),C82CO2( 82)                                   cpmx 220
      COMMON /UFMIX2/                                                   cpmx 230
     +        C11CO(  36),                                              cpmx 240
     +        C21CO( 126),C22CO(  11),                                  cpmx 250
     +        C11CH4(126),C12CH4(126),C13CH4(126),C14CH4(115),          cpmx 260
     +        C11N2O( 25),                                              cpmx 270
     +        C21N2O(126),C22N2O(126),C23N2O(112),                      cpmx 280
     +        C31N2O(126),C32N2O(126),C33N2O( 63),                      cpmx 290
     +        C11O2(  54),                                              cpmx 300
     +        C21O2( 126),C22O2( 126),C23O2(  76)                       cpmx 310
C=CO2 ====C' FOR    8 BAND MODELS                                       cpmx 320
C=CO2 ====  425-  835                                                   cpmx 330
      DATA C11CO2/                                                      cpmx 340
     1 -9.8495, -9.6484, -9.4472, -9.2461, -9.0449, -8.9544, -8.6127,   cpmx 350
     2 -8.4076, -8.2710, -8.0391, -7.9485, -7.9638, -7.7849, -7.6278,   cpmx 360
     3 -7.1418, -6.7823, -6.3826, -6.0323, -5.7501, -5.5249, -5.3304,   cpmx 370
     4 -5.0105, -4.7703, -4.5714, -4.3919, -4.2974, -4.1370, -3.8761,   cpmx 380
     5 -3.5936, -3.2852, -3.0016, -2.7303, -2.4868, -2.2741, -2.0936,   cpmx 390
     6 -1.9424, -1.8092, -1.6843, -1.5372, -1.3803, -1.2043, -0.9930,   cpmx 400
     7 -0.7724, -0.5509, -0.3465, -0.1785, -0.0470,  0.0449,  0.1114,   cpmx 410
     8  0.1367,  0.0910,  0.0066, -0.1269, -0.2994, -0.4934, -0.7101,   cpmx 420
     9 -0.9087, -1.1004, -1.2694, -1.4064, -1.5622, -1.6810, -1.7841,   cpmx 430
     $ -1.8973, -2.0274, -2.2079, -2.4264, -2.6763, -2.9312, -3.1896,   cpmx 440
     1 -3.4262, -3.5979, -3.7051, -3.7372, -3.7983, -3.9154, -4.0520,   cpmx 450
     2 -4.2567, -4.4661, -4.6670, -4.9226, -5.2203, -5.5597/            cpmx 460
C=CO2 ====  840- 1440                                                   cpmx 470
      DATA C21CO2/                                                      cpmx 480
     1 -5.6403, -5.7039, -5.7674, -5.8310, -5.8948, -5.9503, -6.0217,   cpmx 490
     2 -6.0392, -5.9855, -5.8620, -5.6834, -5.5083, -5.3473, -5.2028,   cpmx 500
     3 -5.0799, -4.9628, -4.8379, -4.7032, -4.5584, -4.4213, -4.3198,   cpmx 510
     4 -4.2786, -4.2843, -4.3099, -4.3210, -4.2769, -4.2229, -4.2179,   cpmx 520
     5 -4.2950, -4.4789, -4.7550, -5.0902, -5.4329, -5.6689, -5.6608,   cpmx 530
     6 -5.4582, -5.1969, -4.9419, -4.7106, -4.5084, -4.3409, -4.2211,   cpmx 540
     7 -4.1563, -4.1259, -4.1108, -4.0803, -4.0211, -3.9824, -4.0053,   cpmx 550
     8 -4.1221, -4.3504, -4.6741, -5.0826, -5.5857, -6.2301, -7.0829,   cpmx 560
     9 -8.1344, -8.8601, -9.0457, -9.1231, -9.0728, -9.1413, -9.1221,   cpmx 570
     $ -9.1882, -9.2752, -9.2237, -9.3604, -9.3058, -9.5455, -9.5567,   cpmx 580
     1 -9.3754, -8.7756, -8.0904, -7.4827, -6.9585, -6.5095, -6.1194,   cpmx 590
     2 -5.7824, -5.4910, -5.2532, -5.0840, -4.9920, -4.9577, -4.9638,   cpmx 600
     3 -4.9741, -4.9555, -4.9466, -4.9774, -5.0719, -5.2558, -5.5213,   cpmx 610
     4 -5.8633, -6.2877, -6.7878, -7.2602, -7.2940, -6.8524, -6.3372,   cpmx 620
     5 -5.8854, -5.5065, -5.2011, -4.9776, -4.8471, -4.7885, -4.7783,   cpmx 630
     6 -4.7815, -4.7538, -4.7228, -4.7259, -4.7860, -4.9231, -5.1270,   cpmx 640
     7 -5.3831, -5.6849, -6.0351, -6.4437, -6.9160, -7.4815, -8.1437,   cpmx 650
     8 -8.9449, -9.8564/                                                cpmx 660
C=CO2 ==== 1805- 2855                                                   cpmx 670
      DATA C31CO2/                                                      cpmx 680
     1 -9.8903, -9.4365, -8.9826, -8.5288, -8.1184, -7.6555, -7.1673,   cpmx 690
     2 -6.7226, -6.3423, -6.0410, -5.8154, -5.6519, -5.5186, -5.3859,   cpmx 700
     3 -5.2279, -5.0238, -4.7865, -4.5343, -4.2846, -4.0560, -3.8717,   cpmx 710
     4 -3.7624, -3.7231, -3.7335, -3.8312, -3.9854, -4.1930, -4.4895,   cpmx 720
     5 -4.7394, -4.8892, -4.9499, -4.9392, -4.9787, -5.1129, -5.3330,   cpmx 730
     6 -5.6093, -5.8862, -6.0581, -6.0274, -5.8356, -5.5989, -5.3738,   cpmx 740
     7 -5.1661, -4.9472, -4.7020, -4.4354, -4.1439, -3.8561, -3.5944,   cpmx 750
     8 -3.3694, -3.2100, -3.1041, -3.0411, -3.0471, -3.1077, -3.2305,   cpmx 760
     9 -3.4274, -3.6115, -3.7542, -3.8666, -3.9338, -4.0079, -4.0962,   cpmx 770
     $ -4.2142, -4.1433, -4.2870, -4.4796, -4.6618, -4.8204, -4.9499,   cpmx 780
     1 -4.9862, -5.0171, -5.0282, -5.0580, -5.0398, -4.9465, -4.7816,   cpmx 790
     2 -4.5538, -4.2975, -4.0286, -3.7528, -3.4715, -3.1899, -2.9041,   cpmx 800
     3 -2.6127, -2.3212, -2.0435, -1.7894, -1.5531, -1.3382, -1.1515,   cpmx 810
     4 -0.9990, -0.8833, -0.8006, -0.7227, -0.6288, -0.4977, -0.3249,   cpmx 820
     5 -0.1349,  0.0576,  0.2487,  0.4386,  0.6260,  0.8081,  0.9681,   cpmx 830
     6  1.0859,  1.1522,  1.1861,  1.2039,  1.2255,  1.2587,  1.2473,   cpmx 840
     7  1.1457,  0.9139,  0.5250,  0.0173, -0.5796, -1.3944, -2.3841,   cpmx 850
     8 -2.7244, -2.9264, -3.0689, -3.2120, -3.3353, -3.4510, -3.5566/   cpmx 860
      DATA C32CO2/                                                      cpmx 870
     1 -3.6518, -3.7460, -3.8500, -3.9680, -4.0981, -4.2259, -4.3369,   cpmx 880
     2 -4.4329, -4.5305, -4.6264, -4.7438, -4.8842, -5.0248, -5.1448,   cpmx 890
     3 -5.2371, -5.2781, -5.3299, -5.3766, -5.4233, -5.4699, -5.5166,   cpmx 900
     4 -5.5633, -5.6646, -5.7593, -5.8461, -5.9229, -5.9818, -6.0065,   cpmx 910
     5 -5.9747, -5.8741, -5.7230, -5.5620, -5.4389, -5.3788, -5.3679,   cpmx 920
     6 -5.3827, -5.3837, -5.3460, -5.3186, -5.3394, -5.4320, -5.6095,   cpmx 930
     7 -5.8446, -6.0992, -6.3399, -6.5499, -6.7434, -6.9359, -7.1219,   cpmx 940
     8 -7.2818, -7.3984, -7.4881, -7.5452, -7.5994, -7.6445, -7.6734,   cpmx 950
     9 -7.6422, -7.5057, -7.2650, -6.9975, -6.7749, -6.6398, -6.5875,   cpmx 960
     $ -6.5912, -6.6192, -6.6155, -6.5866, -6.5851, -6.6382, -6.7736,   cpmx 970
     1 -7.0009, -7.2896, -7.6327, -7.9767, -8.2633, -8.4744, -8.5455,   cpmx 980
     2 -8.5813, -8.6025, -8.6459, -8.8948, -9.1436, -9.3925, -9.6413,   cpmx 990
     3 -9.8902/                                                         cpmx1000
C=CO2 ==== 3070- 3755                                                   cpmx1010
      DATA C41CO2/                                                      cpmx1020
     1 -9.8006, -9.5049, -9.1947, -8.7254, -8.4410, -8.1781, -8.0182,   cpmx1030
     2 -7.9381, -7.8793, -7.7636, -7.5549, -7.2962, -7.0244, -6.7556,   cpmx1040
     3 -6.4888, -6.2443, -6.0422, -5.9088, -5.8590, -5.8890, -5.9850,   cpmx1050
     4 -6.0949, -6.1164, -6.0207, -5.8592, -5.7110, -5.6328, -5.6369,   cpmx1060
     5 -5.7274, -5.9069, -6.1720, -6.5203, -6.9586, -7.4776, -8.0607,   cpmx1070
     6 -8.5514, -8.7011, -8.4232, -7.9274, -7.6159, -7.3836, -7.1969,   cpmx1080
     7 -7.0523, -6.7685, -6.4022, -6.0354, -5.7125, -5.4659, -5.3088,   cpmx1090
     8 -5.2546, -5.2991, -5.3819, -5.4615, -5.4117, -5.2107, -5.0103,   cpmx1100
     9 -4.8232, -4.7071, -4.6850, -4.7385, -4.8797, -5.1024, -5.4015,   cpmx1110
     $ -5.7758, -6.2225, -6.6681, -6.9127, -6.8919, -6.6972, -6.5012,   cpmx1120
     1 -6.3123, -6.1091, -5.8641, -5.5889, -5.3057, -5.0340, -4.7826,   cpmx1130
     2 -4.5476, -4.3277, -4.1224, -3.9333, -3.7675, -3.6324, -3.5163,   cpmx1140
     3 -3.4043, -3.2744, -3.1180, -2.9557, -2.8254, -2.7359, -2.6721,   cpmx1150
     4 -2.6084, -2.5105, -2.3772, -2.2317, -2.0866, -1.9521, -1.8292,   cpmx1160
     5 -1.7110, -1.5992, -1.4873, -1.3646, -1.2260, -1.0721, -0.9281,   cpmx1170
     6 -0.8379, -0.8123, -0.8261, -0.8483, -0.8305, -0.7792, -0.7626,   cpmx1180
     7 -0.8228, -0.9908, -1.2503, -1.5347, -1.7934, -1.9837, -2.0715,   cpmx1190
     8 -2.0375, -1.8975, -1.6906, -1.4497, -1.2048, -0.9831, -0.8125/   cpmx1200
      DATA C42CO2/                                                      cpmx1210
     1 -0.7157, -0.6707, -0.6532, -0.6297, -0.5706, -0.5263, -0.5489,   cpmx1220
     2 -0.6857, -0.9793, -1.3962, -1.8673, -2.3655/                     cpmx1230
C=CO2 ==== 3760- 4065                                                   cpmx1240
      DATA C51CO2/                                                      cpmx1250
     1 -3.5436, -4.0424, -4.4084, -4.6848, -4.8663, -4.9516, -4.9790,   cpmx1260
     2 -4.9923, -5.0207, -5.0596, -5.0958, -5.1018, -5.0636, -5.0354,   cpmx1270
     3 -5.0546, -5.1454, -5.3274, -5.5863, -5.8889, -6.1770, -6.3555,   cpmx1280
     4 -6.4096, -6.4371, -6.5112, -6.6680, -6.9183, -7.2418, -7.5827,   cpmx1290
     5 -7.8704, -8.0551, -8.1705, -8.2500, -8.3554, -8.3961, -8.4354,   cpmx1300
     6 -8.3920, -8.2785, -8.0499, -7.7437, -7.4130, -7.1153, -6.8861,   cpmx1310
     7 -6.7422, -6.6786, -6.6774, -6.7053, -6.7090, -6.6794, -6.6055,   cpmx1320
     8 -6.4827, -6.3454, -6.2401, -6.1992, -6.2676, -6.4833, -6.8490,   cpmx1330
     9 -7.4310, -8.4606, -9.7364, -9.8771, -9.8840, -9.9559/            cpmx1340
C=CO2 ==== 4530- 5380                                                   cpmx1350
      DATA C61CO2/                                                      cpmx1360
     1 -9.9489, -9.6003, -9.0910, -8.5793, -8.2059, -7.9099, -7.7157,   cpmx1370
     2 -7.6145, -7.5964, -7.5942, -7.5256, -7.3190, -6.9986, -6.6884,   cpmx1380
     3 -6.4102, -6.1769, -5.9882, -5.8421, -5.7499, -5.7201, -5.7189,   cpmx1390
     4 -5.7108, -5.6669, -5.5955, -5.5686, -5.6287, -5.8000, -6.0855,   cpmx1400
     5 -6.4398, -6.7793, -6.9427, -6.9205, -6.8363, -6.7059, -6.5272,   cpmx1410
     6 -6.2903, -6.0085, -5.7224, -5.4722, -5.2772, -5.1501, -5.0768,   cpmx1420
     7 -5.0219, -4.9579, -4.8555, -4.7213, -4.5868, -4.4594, -4.3387,   cpmx1430
     8 -4.2219, -4.1002, -3.9812, -3.8876, -3.8207, -3.7673, -3.7120,   cpmx1440
     9 -3.6223, -3.4912, -3.3444, -3.1983, -3.0732, -3.0262, -3.0078,   cpmx1450
     $ -3.0123, -3.0213, -2.9957, -2.9261, -2.8770, -2.8887, -2.9853,   cpmx1460
     1 -3.1609, -3.3643, -3.5468, -3.6759, -3.7488, -3.7704, -3.7535,   cpmx1470
     2 -3.7113, -3.6368, -3.5277, -3.3812, -3.2020, -3.0043, -2.8020,   cpmx1480
     3 -2.6122, -2.4524, -2.3405, -2.2838, -2.2521, -2.2319, -2.1960,   cpmx1490
     4 -2.1562, -2.1732, -2.2913, -2.5476, -2.9382, -3.3966, -3.8525,   cpmx1500
     5 -4.2541, -4.5682, -4.7376, -4.7524, -4.6733, -4.5170, -4.3123,   cpmx1510
     6 -4.0891, -3.8565, -3.6218, -3.3909, -3.1785, -3.0100, -2.9105,   cpmx1520
     7 -2.8588, -2.8286, -2.7912, -2.7207, -2.6729, -2.6858, -2.7745,   cpmx1530
     8 -2.9414, -3.1445, -3.3617, -3.5954, -3.8508, -4.1739, -4.5122/   cpmx1540
      DATA C62CO2/                                                      cpmx1550
     1 -4.8985, -5.3426, -5.8737, -6.4734, -7.0715, -7.5042, -7.6034,   cpmx1560
     2 -7.5143, -7.4358, -7.4089, -7.3969, -7.3813, -7.3018, -7.1858,   cpmx1570
     3 -7.0633, -6.9962, -6.9905, -7.0319, -7.1331, -7.2054, -7.1856,   cpmx1580
     4 -7.0561, -6.7966, -6.4771, -6.1996, -5.9593, -5.7560, -5.5370,   cpmx1590
     5 -5.2836, -5.0966, -4.9583, -4.9126, -5.0022, -5.1370, -5.3465,   cpmx1600
     6 -5.6279, -5.9364, -6.3695, -6.9602, -7.6823, -8.2701, -8.6427,   cpmx1610
     7 -9.0728, -9.5366, -9.9588/                                       cpmx1620
C=CO2 ==== 5905- 7025                                                   cpmx1630
      DATA C71CO2/                                                      cpmx1640
     1 -9.9871, -9.6762, -9.3358, -8.9954, -8.5140, -8.2066, -7.9742,   cpmx1650
     2 -7.8579, -7.8073, -7.7894, -7.7466, -7.7009, -7.6393, -7.5889,   cpmx1660
     3 -7.5697, -7.5200, -7.3908, -7.1796, -6.9610, -6.7869, -6.6972,   cpmx1670
     4 -6.6735, -6.6775, -6.6495, -6.5292, -6.3435, -6.1371, -5.9268,   cpmx1680
     5 -5.7254, -5.5433, -5.4023, -5.3292, -5.3090, -5.3171, -5.3193,   cpmx1690
     6 -5.2705, -5.2085, -5.1835, -5.2186, -5.3367, -5.5305, -5.7725,   cpmx1700
     7 -6.0228, -6.2150, -6.2857, -6.2634, -6.2250, -6.2234, -6.2616,   cpmx1710
     8 -6.2931, -6.2508, -6.0971, -5.8679, -5.6195, -5.3906, -5.1944,   cpmx1720
     9 -5.0216, -4.8566, -4.6919, -4.5255, -4.3785, -4.2879, -4.2583,   cpmx1730
     $ -4.2636, -4.2768, -4.2484, -4.1853, -4.1586, -4.2079, -4.3651,   cpmx1740
     1 -4.6407, -5.0141, -5.4719, -6.0015, -6.5173, -6.7829, -6.6805,   cpmx1750
     2 -6.4180, -6.0793, -5.7404, -5.4204, -5.1265, -4.8634, -4.6378,   cpmx1760
     3 -4.4559, -4.3360, -4.2752, -4.2461, -4.2257, -4.1768, -4.1068,   cpmx1770
     4 -4.0743, -4.1193, -4.2732, -4.5464, -4.9256, -5.4090, -6.0184,   cpmx1780
     5 -6.7985, -7.7078, -8.3457, -8.5160, -8.6106, -8.8175, -9.1922,   cpmx1790
     6 -9.6775, -9.7423, -9.1980, -8.4120, -7.7499, -7.1685, -6.6817,   cpmx1800
     7 -6.2701, -5.9301, -5.6567, -5.4521, -5.3289, -5.2776, -5.2630,   cpmx1810
     8 -5.2547, -5.2083, -5.1296, -5.0823, -5.0914, -5.1806, -5.3503/   cpmx1820
      DATA C72CO2/                                                      cpmx1830
     1 -5.5600, -5.7877, -5.9936, -6.1720, -6.3801, -6.6371, -6.9964,   cpmx1840
     2 -7.5010, -8.1628, -8.9951, -9.8931,-10.0000,-10.0000,-10.0000,   cpmx1850
     3-10.0000,-10.0000,-10.0000,-10.0000,-10.0000, -9.4967, -8.9198,   cpmx1860
     4 -8.5081, -8.1255, -7.8286, -7.5478, -7.1487, -6.7853, -6.5537,   cpmx1870
     5 -6.3931, -6.4107, -6.5087, -6.6607, -6.9026, -7.2104, -7.4445,   cpmx1880
     6 -7.6303, -7.6346, -7.4521, -7.2211, -7.0043, -6.7903, -6.5666,   cpmx1890
     7 -6.3499, -6.1534, -5.9988, -5.9033, -5.8760, -5.8693, -5.8277,   cpmx1900
     8 -5.7282, -5.6262, -5.5865, -5.6665, -5.9228, -6.3399, -7.0180,   cpmx1910
     9 -8.4230,-10.0000,-10.0000,-10.0000, -9.4090, -8.8272, -8.3057,   cpmx1920
     $ -7.8885, -7.5044, -7.1560, -6.8292, -6.5250, -6.2461, -5.9904,   cpmx1930
     1 -5.7533, -5.5295, -5.3135, -5.1058, -4.9152, -4.7463, -4.6054,   cpmx1940
     2 -4.4937, -4.3928, -4.2838, -4.1626, -4.0387, -3.9295, -3.8612,   cpmx1950
     3 -3.8501, -3.8647, -3.8625, -3.8099, -3.7351, -3.7179, -3.8549,   cpmx1960
     4 -4.2312, -4.7632, -5.4270, -6.4200, -8.1414, -9.0451, -9.5326,   cpmx1970
     5 -9.8301/                                                         cpmx1980
C=CO2 ==== 7395- 7785, 8030- 8335, 9340- 9670                           cpmx1990
      DATA C81CO2/                                                      cpmx2000
     1 -9.9472, -9.8274, -8.9797, -8.4298, -7.8906, -7.4477, -7.0750,   cpmx2010
     2 -6.7698, -6.5338, -6.3739, -6.2980, -6.2739, -6.2726, -6.2555,   cpmx2020
     3 -6.1989, -6.1529, -6.1654, -6.2584, -6.4610, -6.7805, -7.2235,   cpmx2030
     4 -7.8191, -8.5850, -9.6084,-10.0000,-10.0000, -9.9199, -9.1093,   cpmx2040
     5 -8.4490, -7.9158, -7.4364, -7.0400, -6.6958, -6.4131, -6.1855,   cpmx2050
     6 -6.0158, -5.9123, -5.8700, -5.8530, -5.8340, -5.7866, -5.7224,   cpmx2060
     7 -5.7048, -5.7653, -5.9281, -6.2234, -6.6646, -7.2957, -8.2799,   cpmx2070
     8 -9.9457,-10.0000,-10.0000,-10.0000,-10.0000,-10.0000,-10.0000,   cpmx2080
     9-10.0000, -9.2766, -8.6201, -8.0764, -7.6374, -7.2752, -6.9802,   cpmx2090
     $ -6.7578, -6.6163, -6.5546, -6.5392, -6.5397, -6.5132, -6.4531,   cpmx2100
     1 -6.4161, -6.4482, -6.5683, -6.8086, -7.1762, -7.6772, -8.3574,   cpmx2110
     2 -9.2188,-10.0000,-10.0000, -9.5350, -8.9686, -8.5329, -8.1920,   cpmx2120
     3 -7.9237, -7.6797, -7.5039, -7.3667, -7.2856, -7.1969, -7.0745,   cpmx2130
     4 -6.9330, -6.7926, -6.6818, -6.6144, -6.5643, -6.5183, -6.4910,   cpmx2140
     5 -6.4481, -6.3567, -6.2177, -6.0566, -5.9096, -5.7975, -5.7093,   cpmx2150
     6 -5.6165, -5.5127, -5.4124, -5.3426, -5.3061, -5.2648, -5.1864,   cpmx2160
     7 -5.0876, -5.0226, -5.0397, -5.1905, -5.4858, -5.9101, -6.4851,   cpmx2170
     8 -6.7862, -6.5368, -6.2765, -6.0398, -5.8260, -5.6397, -5.4799/   cpmx2180
      DATA C82CO2/                                                      cpmx2190
     1 -5.3438, -5.2274, -5.1411, -5.0917, -5.0473, -4.9820, -4.9114,   cpmx2200
     2 -4.8634, -4.8844, -5.0363, -5.3351, -5.7802, -6.5387, -8.3735,   cpmx2210
     3 -9.9977, -9.7506, -9.1887, -8.6824, -8.3488, -8.0533, -7.8664,   cpmx2220
     4 -7.7346, -7.6934, -7.6674, -7.6268, -7.5451, -7.4677, -7.4520,   cpmx2230
     5 -7.5471, -7.7913, -8.1917, -8.8835,-10.0000,-10.0000,-10.0000,   cpmx2240
     6-10.0000,-10.0000, -9.7234, -8.9969, -8.5776, -8.1737, -7.8640,   cpmx2250
     7 -7.5729, -7.3186, -7.0973, -6.9131, -6.7782, -6.7073, -6.6768,   cpmx2260
     8 -6.6303, -6.5406, -6.4509, -6.3950, -6.4345, -6.6270, -6.9507,   cpmx2270
     9 -7.5028, -8.6428,-10.0000,-10.0000,-10.0000,-10.0000, -9.5303,   cpmx2280
     $ -8.9369, -8.4952, -8.1465, -7.8567, -7.6177, -7.4249, -7.2876,   cpmx2290
     1 -7.2206, -7.1948, -7.1552, -7.0773, -6.9884, -6.9402, -6.9839,   cpmx2300
     2 -7.1773, -7.4999, -8.0643, -9.1480,-10.0000/                     cpmx2310
C=CO  ====C' FOR    2 BAND MODEL                                        cpmx2320
C=CO  ====    0-  175                                                   cpmx2330
      DATA C11CO/                                                       cpmx2340
     1 -4.6868, -4.4127, -3.9461, -3.5662, -3.2921, -3.1081, -2.9807,   cpmx2350
     2 -2.8977, -2.8580, -2.8461, -2.8587, -2.9029, -2.9646, -3.0480,   cpmx2360
     3 -3.1589, -3.2836, -3.4277, -3.5993, -3.7963, -4.0164, -4.2799,   cpmx2370
     4 -4.5750, -4.8722, -5.2741, -5.6819, -6.0799, -6.4828, -6.8857,   cpmx2380
     5 -7.2886, -7.6915, -8.0944, -8.4973, -8.9002, -9.3031, -9.7060,   cpmx2390
     6-10.0000/                                                         cpmx2400
C=CO  ==== 1940- 2285, 4040- 4370                                       cpmx2410
      DATA C21CO/                                                       cpmx2420
     1-10.0000, -9.5312, -8.8977, -8.2642, -7.5767, -6.9972, -6.5408,   cpmx2430
     2 -6.1219, -5.6734, -5.2658, -4.8686, -4.4918, -4.1423, -3.8133,   cpmx2440
     3 -3.4998, -3.2104, -2.9443, -2.7138, -2.5084, -2.3109, -2.1245,   cpmx2450
     4 -1.9387, -1.7608, -1.6054, -1.4733, -1.3594, -1.2540, -1.1480,   cpmx2460
     5 -1.0341, -0.9216, -0.8189, -0.7235, -0.6362, -0.5549, -0.4856,   cpmx2470
     6 -0.4401, -0.4268, -0.4657, -0.5571, -0.6573, -0.7404, -0.7523,   cpmx2480
     7 -0.6601, -0.5380, -0.4211, -0.3367, -0.3167, -0.3320, -0.3753,   cpmx2490
     8 -0.4489, -0.5438, -0.6653, -0.8052, -0.9690, -1.1506, -1.3522,   cpmx2500
     9 -1.5791, -1.8248, -2.1073, -2.4246, -2.7877, -3.2152, -3.7089,   cpmx2510
     $ -4.2832, -4.9518, -5.7251, -6.5319, -7.4879, -9.0885,-10.0000,   cpmx2520
     1-10.0000, -9.5611, -9.0875, -8.6139, -7.9747, -7.5250, -7.1931,   cpmx2530
     2 -6.8596, -6.5741, -6.2922, -6.0098, -5.7669, -5.5345, -5.3229,   cpmx2540
     3 -5.1461, -4.9882, -4.8493, -4.7239, -4.6064, -4.5009, -4.4071,   cpmx2550
     4 -4.3322, -4.2661, -4.1926, -4.0956, -3.9611, -3.7984, -3.6314,   cpmx2560
     5 -3.4757, -3.3408, -3.2237, -3.1219, -3.0325, -2.9494, -2.8765,   cpmx2570
     6 -2.8117, -2.7531, -2.7023, -2.6635, -2.6440, -2.6550, -2.7225,   cpmx2580
     7 -2.8161, -2.9015, -2.9241, -2.8228, -2.6726, -2.5320, -2.4291,   cpmx2590
     8 -2.3772, -2.3732, -2.3995, -2.4574, -2.5486, -2.6664, -2.8209/   cpmx2600
      DATA C22CO/                                                       cpmx2610
     1 -3.0129, -3.2516, -3.5482, -3.9165, -4.3714, -4.9326, -5.6394,   cpmx2620
     2 -6.5163, -7.6063, -9.3575,-10.0000/                              cpmx2630
C=CH4 ====C' FOR    1 BAND MODEL                                        cpmx2640
C=CH4 ==== 1065- 1775, 2345- 3230, 4110- 4690, 5865- 6135               cpmx2650
      DATA C11CH4/                                                      cpmx2660
     1-10.0000, -9.4577, -8.8866, -8.2246, -7.7940, -7.1734, -6.7965,   cpmx2670
     2 -6.5695, -6.1929, -5.9169, -5.7452, -5.4731, -5.3001, -5.1872,   cpmx2680
     3 -4.9672, -4.8474, -4.6939, -4.5210, -4.3377, -4.1346, -3.9322,   cpmx2690
     4 -3.7339, -3.5077, -3.2719, -3.0296, -2.8124, -2.6199, -2.4479,   cpmx2700
     5 -2.2502, -2.0541, -1.8800, -1.7092, -1.5791, -1.4379, -1.2992,   cpmx2710
     6 -1.1735, -1.0510, -0.9646, -0.8779, -0.8002, -0.7574, -0.7356,   cpmx2720
     7 -0.7478, -0.7512, -0.6906, -0.5594, -0.4417, -0.4019, -0.5027,   cpmx2730
     8 -0.7628, -0.9625, -1.0431, -1.0068, -0.8781, -0.7559, -0.6628,   cpmx2740
     9 -0.6128, -0.6118, -0.6575, -0.7620, -0.9217, -1.1264, -1.3660,   cpmx2750
     $ -1.6352, -1.9264, -2.2266, -2.5123, -2.7472, -2.8820, -2.9129,   cpmx2760
     1 -2.9145, -2.8854, -2.8508, -2.8512, -2.8202, -2.8023, -2.8004,   cpmx2770
     2 -2.7800, -2.8175, -2.8413, -2.8943, -2.9876, -3.0688, -3.2424,   cpmx2780
     3 -3.4064, -3.5759, -3.7630, -3.8925, -4.0774, -4.3243, -4.5964,   cpmx2790
     4 -3.8654, -3.0974, -2.5967, -2.2482, -2.1016, -2.1488, -2.3261,   cpmx2800
     5 -2.6448, -3.0446, -3.3958, -3.6510, -3.7049, -3.7240, -3.5992,   cpmx2810
     6 -3.4937, -3.3676, -3.2230, -3.1630, -3.0691, -3.0776, -3.0872,   cpmx2820
     7 -3.0974, -3.1223, -3.1285, -3.1212, -3.1333, -3.1674, -3.1668,   cpmx2830
     8 -3.2433, -3.2398, -3.3135, -3.3975, -3.4427, -3.6434, -3.7528/   cpmx2840
      DATA C12CH4/                                                      cpmx2850
     1 -3.9466, -4.1940, -4.3362, -4.5539, -4.7410, -4.9155, -5.1345,   cpmx2860
     2 -5.3908, -5.5592, -5.8270, -6.0289, -6.2365, -6.6730, -7.0538,   cpmx2870
     3 -7.6216, -8.5697, -9.8483,-10.0000, -9.3577, -8.5950, -7.8323,   cpmx2880
     4 -7.0696, -6.3069, -5.5442, -5.1501, -4.8853, -4.6900, -4.5262,   cpmx2890
     5 -4.3957, -4.2823, -4.2736, -4.2054, -4.1168, -3.9986, -3.8712,   cpmx2900
     6 -3.8692, -3.8777, -3.8965, -3.9092, -3.8788, -3.7661, -3.6900,   cpmx2910
     7 -3.6239, -3.5597, -3.5193, -3.4906, -3.4415, -3.3730, -3.3579,   cpmx2920
     8 -3.3427, -3.3208, -3.3048, -3.3136, -3.2904, -3.2545, -3.2241,   cpmx2930
     9 -3.1453, -3.0187, -2.9427, -2.8630, -2.8146, -2.8604, -2.8922,   cpmx2940
     $ -2.9650, -2.9959, -2.8920, -2.7989, -2.7028, -2.6506, -2.7285,   cpmx2950
     1 -2.8420, -2.9304, -2.9622, -2.8726, -2.7566, -2.6745, -2.6337,   cpmx2960
     2 -2.6533, -2.6800, -2.7098, -2.7479, -2.6859, -2.6216, -2.5701,   cpmx2970
     3 -2.4683, -2.4426, -2.4463, -2.4194, -2.4578, -2.4894, -2.4639,   cpmx2980
     4 -2.4825, -2.4998, -2.4381, -2.4123, -2.3654, -2.2698, -2.2387,   cpmx2990
     5 -2.2364, -2.2029, -2.1780, -2.1433, -2.0355, -1.9458, -1.8723,   cpmx3000
     6 -1.7936, -1.7639, -1.7782, -1.8022, -1.8115, -1.7818, -1.6986,   cpmx3010
     7 -1.6169, -1.5975, -1.6545, -1.7742, -1.8937, -1.9544, -1.8942,   cpmx3020
     8 -1.7761, -1.6392, -1.5236, -1.4551, -1.4221, -1.4245, -1.4174/   cpmx3030
      DATA C13CH4/                                                      cpmx3040
     1 -1.4177, -1.3776, -1.3349, -1.2909, -1.2470, -1.2162, -1.1850,   cpmx3050
     2 -1.1677, -1.1449, -1.1229, -1.1031, -1.0795, -1.0687, -1.0692,   cpmx3060
     3 -1.0904, -1.1166, -1.1511, -1.1951, -1.2321, -1.2831, -1.2716,   cpmx3070
     4 -1.1902, -0.9715, -0.6654, -0.4103, -0.3011, -0.5049, -0.8659,   cpmx3080
     5 -1.1777, -1.3847, -1.4359, -1.3908, -1.2992, -1.1923, -1.0951,   cpmx3090
     6 -1.0213, -0.9578, -0.9299, -0.9207, -0.9292, -0.9725, -1.0126,   cpmx3100
     7 -1.0750, -1.1149, -1.1636, -1.2059, -1.2638, -1.3327, -1.4079,   cpmx3110
     8 -1.4983, -1.5711, -1.6872, -1.7870, -1.9266, -2.0774, -2.2119,   cpmx3120
     9 -2.3875, -2.5155, -2.6822, -2.8372, -3.0032, -3.2413, -3.5058,   cpmx3130
     $ -3.9508, -4.5133, -5.3536, -8.0815, -8.9081, -9.8155,-10.0000,   cpmx3140
     1 -7.4757, -5.1602, -4.2454, -3.7640, -3.3256, -3.0103, -2.7726,   cpmx3150
     2 -2.5510, -2.3849, -2.2318, -2.1080, -2.0086, -1.9290, -1.8902,   cpmx3160
     3 -1.8750, -1.8700, -1.8476, -1.7390, -1.5724, -1.4284, -1.3425,   cpmx3170
     4 -1.3791, -1.5132, -1.6508, -1.7283, -1.6684, -1.5432, -1.4447,   cpmx3180
     5 -1.3773, -1.3490, -1.3642, -1.4016, -1.4713, -1.5836, -1.6984,   cpmx3190
     6 -1.8085, -1.8486, -1.7464, -1.6338, -1.5555, -1.5552, -1.6935,   cpmx3200
     7 -1.8165, -1.8417, -1.7697, -1.6346, -1.5589, -1.5466, -1.5604,   cpmx3210
     8 -1.6307, -1.6867, -1.7593, -1.8051, -1.8167, -1.8518, -1.8559/   cpmx3220
      DATA C14CH4/                                                      cpmx3230
     1 -1.8547, -1.8907, -1.8851, -1.8933, -1.9081, -1.9025, -1.9451,   cpmx3240
     2 -1.9924, -2.0321, -2.0816, -2.1026, -2.1137, -2.1351, -2.1629,   cpmx3250
     3 -2.1876, -2.2340, -2.2960, -2.3747, -2.4970, -2.6244, -2.7641,   cpmx3260
     4 -2.8912, -3.0328, -3.1944, -3.3877, -3.4566, -3.1662, -2.7253,   cpmx3270
     5 -2.3992, -2.2214, -2.2022, -2.3978, -2.7449, -3.2639, -3.9311,   cpmx3280
     6 -4.1470, -3.9351, -3.7471, -3.6245, -3.4791, -3.4710, -3.4210,   cpmx3290
     7 -3.4125, -3.4475, -3.4140, -3.4908, -3.5164, -3.5944, -3.7403,   cpmx3300
     8 -3.8192, -4.0177, -4.1833, -4.3518, -4.6486, -4.8778, -5.2542,   cpmx3310
     9 -5.7834, -6.3451, -7.7212,-10.0000, -9.9134, -7.9181, -6.0815,   cpmx3320
     $ -5.4397, -4.9875, -4.6154, -4.4846, -4.3541, -4.3037, -4.3073,   cpmx3330
     1 -4.2471, -4.2593, -4.1984, -4.1895, -4.1697, -4.1578, -4.1950,   cpmx3340
     2 -4.1878, -4.2299, -4.2209, -4.2646, -4.3123, -4.3911, -4.4588,   cpmx3350
     3 -4.1873, -3.8353, -3.5282, -3.3055, -3.3351, -3.5671, -3.8750,   cpmx3360
     4 -4.2645, -4.4786, -4.4293, -4.3183, -4.1996, -4.0879, -4.0169,   cpmx3370
     5 -3.9787, -3.9536, -3.9454, -3.9283, -3.9166, -3.9152, -3.9336,   cpmx3380
     6 -3.9561, -3.9932, -4.0934, -4.2317, -4.5084, -4.9460, -5.4958,   cpmx3390
     7 -6.5492, -8.5604, -9.6202/                                       cpmx3400
C=N2O ====C' FOR    3 BAND MODEL                                        cpmx3410
C=N2O ====    0-  120                                                   cpmx3420
      DATA C11N2O/                                                      cpmx3430
     1 -2.8003, -2.6628, -2.4313, -2.2579, -2.1700, -2.1702, -2.2490,   cpmx3440
     2 -2.4003, -2.6264, -2.9219, -3.2954, -3.7684, -4.2621, -4.7558,   cpmx3450
     3 -5.2495, -5.7432, -6.2369, -6.7306, -7.2243, -7.7180, -8.2117,   cpmx3460
     4 -8.7054, -9.1991, -9.6928,-10.0000/                              cpmx3470
C=N2O ====  490-  775,  865-  995, 1065- 1385, 1545- 2040, 2090- 2655   cpmx3480
      DATA C21N2O/                                                      cpmx3490
     1 -9.7185, -8.8926, -8.0667, -7.2307, -6.4149, -5.4872, -4.7083,   cpmx3500
     2 -4.0319, -3.4752, -3.0155, -2.6046, -2.2057, -1.8137, -1.4741,   cpmx3510
     3 -1.1914, -0.9603, -0.7923, -0.6629, -0.5849, -0.5402, -0.4975,   cpmx3520
     4 -0.5148, -0.5592, -0.6521, -0.8148, -1.0186, -1.2764, -1.5873,   cpmx3530
     5 -1.9638, -2.3881, -2.8083, -3.2392, -3.6934, -4.0682, -4.1366,   cpmx3540
     6 -3.9423, -3.7143, -3.4975, -3.2602, -3.0976, -2.9815, -2.9153,   cpmx3550
     7 -2.9596, -3.0281, -3.1264, -3.2650, -3.3906, -3.5717, -3.8312,   cpmx3560
     8 -4.1706, -4.6077, -5.1839, -5.9224, -6.9862, -7.6901, -8.3940,   cpmx3570
     9 -9.0979, -9.8018, -9.9154, -9.2271, -8.5388, -7.8504, -7.1621,   cpmx3580
     $ -6.2428, -5.6051, -5.0971, -4.7237, -4.4104, -4.2050, -4.0681,   cpmx3590
     1 -4.0278, -4.0307, -4.0492, -4.0333, -3.9710, -3.9249, -3.9360,   cpmx3600
     2 -4.0316, -4.2317, -4.5414, -4.9787, -5.5623, -6.3335, -7.9968,   cpmx3610
     3 -9.6601, -9.5486, -8.8517, -8.1548, -7.4579, -6.7610, -6.0641,   cpmx3620
     4 -5.3672, -4.6703, -3.6918, -3.0656, -2.5796, -2.1876, -1.8646,   cpmx3630
     5 -1.5919, -1.3587, -1.1684, -1.0286, -0.9470, -0.9271, -0.9442,   cpmx3640
     6 -0.9695, -0.9753, -0.9573, -0.9550, -1.0000, -1.1070, -1.2791,   cpmx3650
     7 -1.4976, -1.7281, -1.9277, -2.0227, -1.9577, -1.7625, -1.5020,   cpmx3660
     8 -1.2186, -0.9270, -0.6326, -0.3429, -0.0768,  0.1500,  0.3215/   cpmx3670
      DATA C22N2O/                                                      cpmx3680
     1  0.4104,  0.4385,  0.4288,  0.4185,  0.4570,  0.4972,  0.4987,   cpmx3690
     2  0.4216,  0.2360, -0.0319, -0.3714, -0.7539, -1.1534, -1.5855,   cpmx3700
     3 -2.0610, -2.6068, -3.2635, -4.1038, -5.2761, -6.1437, -7.0079,   cpmx3710
     4 -7.9440, -8.8801, -9.8162,-10.0000, -9.5951, -9.1305, -8.6659,   cpmx3720
     5 -8.2013, -7.7367, -7.2721, -6.8075, -6.1598, -5.8695, -5.3510,   cpmx3730
     6 -4.9491, -4.6310, -4.3846, -4.0784, -3.7763, -3.5901, -3.4607,   cpmx3740
     7 -3.4386, -3.5481, -3.7014, -3.9310, -4.2251, -4.4593, -4.8210,   cpmx3750
     8 -5.3494, -6.1286, -7.5981,-10.0000,-10.0000,-10.0000,-10.0000,   cpmx3760
     9 -6.3743, -5.5592, -5.0129, -4.6075, -4.3171, -4.0928, -3.7537,   cpmx3770
     $ -3.5406, -3.3869, -3.2913, -3.3633, -3.4932, -3.6924, -4.0074,   cpmx3780
     1 -4.2504, -4.5389, -4.9425, -5.4741, -6.2069, -7.5981,-10.0000,   cpmx3790
     2-10.0000,-10.0000, -6.9215, -6.0798, -5.1934, -4.6288, -4.1316,   cpmx3800
     3 -3.7322, -3.4089, -3.1573, -2.9573, -2.7298, -2.5615, -2.4382,   cpmx3810
     4 -2.3523, -2.3774, -2.4508, -2.5755, -2.7757, -2.9904, -3.2733,   cpmx3820
     5 -3.6524, -4.1599, -4.7952, -5.7004, -6.8762, -6.9822, -6.2484,   cpmx3830
     6 -5.7613, -5.2586, -4.8674, -4.6633, -4.5332, -4.5158, -4.6593,   cpmx3840
     7 -4.8427, -5.0917, -5.5781, -6.0645, -6.5509, -7.0373, -7.5237,   cpmx3850
     8 -8.0101, -8.4965, -8.9829, -9.4693, -9.9557, -9.7130, -8.6609/   cpmx3860
      DATA C23N2O/                                                      cpmx3870
     1 -7.6089, -6.5568, -5.0880, -4.4527, -3.9302, -3.4438, -2.9701,   cpmx3880
     2 -2.5423, -2.1616, -1.8076, -1.4763, -1.1580, -0.8445, -0.5455,   cpmx3890
     3 -0.2506,  0.0234,  0.2775,  0.5113,  0.7154,  0.8929,  1.0359,   cpmx3900
     4  1.1306,  1.1697,  1.1807,  1.1803,  1.1974,  1.2466,  1.2629,   cpmx3910
     5  1.2068,  1.0472,  0.7695,  0.4083, -0.0244, -0.5477, -1.2202,   cpmx3920
     6 -2.1067, -2.9508, -3.2107, -3.1587, -2.9600, -2.7641, -2.6324,   cpmx3930
     7 -2.5671, -2.5664, -2.6088, -2.6425, -2.6606, -2.6895, -2.7551,   cpmx3940
     8 -2.8837, -3.0884, -3.3746, -3.7078, -4.0975, -4.6272, -5.2484,   cpmx3950
     9-10.0000,-10.0000,-10.0000, -7.3571, -5.0287, -4.3047, -3.6431,   cpmx3960
     $ -3.1026, -2.6122, -2.1941, -1.8454, -1.5726, -1.3829, -1.2818,   cpmx3970
     1 -1.2505, -1.2579, -1.2731, -1.2502, -1.2092, -1.2044, -1.2577,   cpmx3980
     2 -1.3942, -1.6262, -1.9347, -2.2830, -2.5386, -2.4801, -2.1671,   cpmx3990
     3 -1.8061, -1.4726, -1.1797, -0.9377, -0.7542, -0.6392, -0.5899,   cpmx4000
     4 -0.5743, -0.5669, -0.5339, -0.4745, -0.4471, -0.4779, -0.5877,   cpmx4010
     5 -0.7964, -1.0942, -1.4812, -1.9593, -2.5140, -3.1350, -3.8102,   cpmx4020
     6 -4.5825, -5.5982, -6.4193, -7.2403, -8.0614, -8.8825, -9.7035/   cpmx4030
C=N2O ==== 2705- 2865, 3245- 3925, 4260- 4470, 4540- 4785, 4910- 5165   cpmx4040
      DATA C31N2O/                                                      cpmx4050
     1 -9.8910, -8.9876, -8.0843, -7.1809, -6.1501, -5.3742, -4.7352,   cpmx4060
     2 -4.2051, -3.7525, -3.3562, -2.9916, -2.6649, -2.3872, -2.1499,   cpmx4070
     3 -1.9747, -1.7982, -1.6518, -1.5582, -1.4838, -1.5004, -1.5821,   cpmx4080
     4 -1.6912, -1.8673, -2.0756, -2.3351, -2.7020, -3.1921, -3.8409,   cpmx4090
     5 -4.7085, -5.9588, -6.5829, -8.5585, -9.8584, -9.9723, -9.4215,   cpmx4100
     6 -8.8707, -8.3199, -7.7691, -7.2183, -6.5567, -6.4345, -5.6448,   cpmx4110
     7 -5.0529, -4.4643, -3.9624, -3.5231, -3.1395, -2.8067, -2.5232,   cpmx4120
     8 -2.2858, -2.0820, -1.9049, -1.7554, -1.6485, -1.5959, -1.5838,   cpmx4130
     9 -1.5961, -1.5997, -1.5734, -1.5615, -1.5974, -1.7059, -1.9034,   cpmx4140
     $ -2.1631, -2.4181, -2.5427, -2.4592, -2.2513, -2.0187, -1.7879,   cpmx4150
     1 -1.5612, -1.3399, -1.1265, -0.9226, -0.7379, -0.5790, -0.4573,   cpmx4160
     2 -0.3952, -0.3683, -0.3511, -0.3216, -0.2556, -0.2126, -0.2593,   cpmx4170
     3 -0.4361, -0.7702, -1.2089, -1.7060, -2.2937, -3.1133, -4.4419,   cpmx4180
     4 -6.0119, -6.9457,-10.0000,-10.0000,-10.0000,-10.0000, -7.0394,   cpmx4190
     5 -5.9637, -5.2317, -4.6419, -4.1663, -3.7874, -3.5000, -3.3086,   cpmx4200
     6 -3.2143, -3.1926, -3.2105, -3.2308, -3.1971, -3.1510, -3.1402,   cpmx4210
     7 -3.1969, -3.3477, -3.6005, -3.9534, -4.4117, -4.9729, -5.6009,   cpmx4220
     8 -6.2179, -5.9845, -5.5502, -4.9010, -4.3401, -3.8232, -3.3802/   cpmx4230
      DATA C32N2O/                                                      cpmx4240
     1 -2.9972, -2.6747, -2.4143, -2.2209, -2.1080, -2.0682, -2.0687,   cpmx4250
     2 -2.0775, -2.0485, -1.9847, -1.9531, -1.9870, -2.1110, -2.3366,   cpmx4260
     3 -2.6293, -2.8922, -2.9474, -2.7627, -2.4999, -2.2554, -2.0537,   cpmx4270
     4 -1.9062, -1.8268, -1.7941, -1.7766, -1.7468, -1.6767, -1.6130,   cpmx4280
     5 -1.6085, -1.6849, -1.8599, -2.1258, -2.4538, -2.8205, -3.2028,   cpmx4290
     6 -3.5988, -4.0691, -4.7117, -5.6320, -6.4806, -7.3731, -8.2602,   cpmx4300
     7 -9.1474,-10.0000,-10.0000, -9.5340, -9.0282, -8.5224, -8.0166,   cpmx4310
     8 -7.5109, -7.0051, -6.4117, -6.0148, -5.4878, -5.1742, -4.8859,   cpmx4320
     9 -4.4873, -4.2249, -4.0285, -3.8669, -3.8247, -3.7652, -3.6521,   cpmx4330
     $ -3.4906, -3.2613, -3.0307, -2.8156, -2.6172, -2.4264, -2.2442,   cpmx4340
     1 -2.0775, -1.9432, -1.8703, -1.8523, -1.8552, -1.8443, -1.7814,   cpmx4350
     2 -1.7104, -1.7043, -1.7952, -2.0205, -2.3968, -2.9374, -3.7689,   cpmx4360
     3 -5.3159, -7.4139, -9.5119, -9.7965, -9.1511, -8.5057, -7.8603,   cpmx4370
     4 -7.2149, -6.5695, -6.2415, -5.5829, -5.0296, -4.5660, -4.1722,   cpmx4380
     5 -3.8364, -3.5551, -3.3398, -3.1970, -3.1363, -3.1232, -3.1257,   cpmx4390
     6 -3.0999, -3.0288, -2.9746, -2.9875, -3.0925, -3.3137, -3.6496,   cpmx4400
     7 -4.0276, -4.1958, -3.9760, -3.6179, -3.2725, -2.9653, -2.6962,   cpmx4410
     8 -2.4677, -2.2828, -2.1547, -2.0949, -2.0763, -2.0606, -2.0142/   cpmx4420
      DATA C33N2O/                                                      cpmx4430
     1 -1.9239, -1.8618, -1.8813, -2.0099, -2.2825, -2.7071, -3.3277,   cpmx4440
     2 -4.3300, -6.2151, -8.3543,-10.0000, -9.7275, -9.1257, -8.5239,   cpmx4450
     3 -7.9221, -7.3203, -6.7185, -6.6089, -5.8877, -5.4527, -5.0879,   cpmx4460
     4 -4.6598, -4.3806, -4.1830, -4.0426, -4.0175, -4.0178, -3.9811,   cpmx4470
     5 -3.9244, -3.8056, -3.6968, -3.6435, -3.6326, -3.6339, -3.6157,   cpmx4480
     6 -3.5478, -3.4826, -3.4807, -3.5665, -3.7650, -4.0718, -4.3980,   cpmx4490
     7 -4.5075, -4.3358, -4.0765, -3.8674, -3.7221, -3.6588, -3.6429,   cpmx4500
     8 -3.6371, -3.6014, -3.5209, -3.4616, -3.4774, -3.5957, -3.8481,   cpmx4510
     9 -4.2598, -4.8784, -5.8266, -6.7468, -8.1352, -9.2208,-10.0000/   cpmx4520
C=O2  ====C' FOR    2 BAND MODEL                                        cpmx4530
C=O2  ====    0-  265                                                   cpmx4540
      DATA C11O2/                                                       cpmx4550
     1 -6.1363, -6.1794, -6.2538, -6.3705, -6.5110, -6.6162, -6.7505,   cpmx4560
     2 -6.7896, -6.8305, -6.8471, -6.8282, -6.8772, -6.8680, -6.9332,   cpmx4570
     3 -6.9511, -7.0048, -7.0662, -7.1043, -7.2055, -7.2443, -7.3520,   cpmx4580
     4 -7.4079, -7.4998, -7.5924, -7.6682, -7.7993, -7.8712, -8.0161,   cpmx4590
     5 -8.1102, -8.2485, -8.3758, -8.4942, -8.6532, -8.7554, -8.9453,   cpmx4600
     6 -9.0665, -9.2631, -9.4387, -9.6325, -9.8757,-10.0628,-10.3761,   cpmx4610
     7-10.5478,-10.9147,-11.2052,-11.5129,-11.8206,-12.1283,-12.4360,   cpmx4620
     8-12.7437,-13.0514,-13.3591,-13.6668,-13.9745/                     cpmx4630
C=O2  ==== 7650- 8080, 9235- 9490,12850-13220,14300-14600,15695-15955   cpmx4640
      DATA C21O2/                                                       cpmx4650
     1-13.9458,-13.7692,-13.5048,-13.1422,-13.0242,-12.6684,-12.3571,   cpmx4660
     2-12.2428,-11.8492,-11.6427,-11.5173,-11.2108,-11.1584,-11.0196,   cpmx4670
     3-10.8040,-10.8059,-10.5828,-10.4580,-10.4170,-10.1823,-10.1435,   cpmx4680
     4-10.0030, -9.8136, -9.7772, -9.5680, -9.4595, -9.3502, -9.1411,   cpmx4690
     5 -9.0476, -8.8628, -8.7051, -8.5838, -8.4282, -8.3271, -8.1958,   cpmx4700
     6 -8.0838, -7.9652, -7.8371, -7.7476, -7.6431, -7.5736, -7.5149,   cpmx4710
     7 -7.4194, -7.2688, -7.0722, -6.8815, -6.7627, -6.8055, -6.9114,   cpmx4720
     8 -6.9936, -7.0519, -7.0597, -7.0680, -7.1242, -7.2088, -7.3265,   cpmx4730
     9 -7.4673, -7.6326, -7.8110, -8.0096, -8.2104, -8.4036, -8.5853,   cpmx4740
     $ -8.7252, -8.8511, -8.9427, -9.0375, -9.1228, -9.2246, -9.3291,   cpmx4750
     1 -9.4436, -9.5716, -9.6951, -9.8408, -9.9759,-10.1489,-10.3027,   cpmx4760
     2-10.5178,-10.7265,-10.9787,-11.2939,-11.5552,-11.9595,-12.2436,   cpmx4770
     3-12.6942,-13.2011,-13.8191,-13.9216,-13.7293,-13.5370,-13.3447,   cpmx4780
     4-13.1523,-12.9600,-12.7677,-12.5754,-12.3830,-12.1907,-11.9948,   cpmx4790
     5-11.7759,-11.5926,-11.4214,-11.2493,-11.1094,-10.9477,-10.8332,   cpmx4800
     6-10.7323,-10.6380,-10.5725,-10.4409,-10.2013, -9.8839, -9.6546,   cpmx4810
     7 -9.5053, -9.4638, -9.5526, -9.6558, -9.7430, -9.7958, -9.7896,   cpmx4820
     8 -9.8320, -9.9447,-10.1221,-10.3707,-10.6623,-10.9761,-11.2271/   cpmx4830
      DATA C22O2/                                                       cpmx4840
     1-11.4091,-11.4921,-11.6015,-11.6945,-11.8333,-11.9985,-12.1788,   cpmx4850
     2-12.3822,-12.6605,-13.0796,-13.3528,-13.6463,-13.9398,-13.7034,   cpmx4860
     3-13.3150,-13.1177,-12.6462,-12.4868,-12.2205,-11.9650,-11.6941,   cpmx4870
     4-11.4377,-11.2136,-10.9567,-10.7980,-10.5546,-10.3952,-10.2403,   cpmx4880
     5-10.0491, -9.9226, -9.7871, -9.6557, -9.6106, -9.5142, -9.4763,   cpmx4890
     6 -9.4163, -9.2348, -9.1088, -8.7946, -8.5876, -8.3128, -8.0945,   cpmx4900
     7 -7.9127, -7.7229, -7.5860, -7.4215, -7.2726, -7.1179, -6.9516,   cpmx4910
     8 -6.8075, -6.6413, -6.5043, -6.3519, -6.2112, -6.0839, -5.9337,   cpmx4920
     9 -5.8321, -5.6969, -5.5923, -5.5076, -5.4002, -5.3413, -5.2826,   cpmx4930
     $ -5.2458, -5.2877, -5.3743, -5.4654, -5.5262, -5.4429, -5.2430,   cpmx4940
     1 -5.0284, -4.8464, -4.7534, -4.7825, -4.9462, -5.2290, -5.6440,   cpmx4950
     2 -6.1889, -6.8427, -7.7731, -9.1688, -9.6893,-10.1853,-10.7670,   cpmx4960
     3-11.4611,-12.3081,-13.1476,-13.8192,-13.5871,-13.2189,-12.9705,   cpmx4970
     4-12.4825,-12.1301,-11.9430,-11.6636,-11.3197,-11.1678,-10.8967,   cpmx4980
     5-10.6002,-10.4857,-10.1986, -9.9731, -9.8547, -9.5817, -9.4382,   cpmx4990
     6 -9.3042, -9.0755, -8.9944, -8.8060, -8.6543, -8.5441, -8.3556,   cpmx5000
     7 -8.2557, -8.0959, -7.9717, -7.8453, -7.7076, -7.5910, -7.4567,   cpmx5010
     8 -7.3439, -7.2248, -7.1236, -7.0209, -6.9345, -6.8404, -6.7560/   cpmx5020
      DATA C23O2/                                                       cpmx5030
     1 -6.6744, -6.5870, -6.5278, -6.4809, -6.5042, -6.5797, -6.6564,   cpmx5040
     2 -6.6939, -6.5912, -6.3776, -6.1438, -6.0062, -6.0469, -6.3081,   cpmx5050
     3 -6.8199, -7.4307, -8.1345, -9.1190,-10.4203,-11.4698,-12.5942,   cpmx5060
     4-13.5316,-13.8693,-13.9392,-13.6885,-13.4377,-13.1869,-12.9362,   cpmx5070
     5-12.6854,-12.3720,-12.2852,-11.9331,-11.7575,-11.6297,-11.3290,   cpmx5080
     6-11.1205,-11.0084,-10.7243,-10.5543,-10.4485,-10.1764,-10.0759,   cpmx5090
     7 -9.9304, -9.7196, -9.6630, -9.4774, -9.3638, -9.2675, -9.1121,   cpmx5100
     8 -9.0368, -8.9025, -8.8028, -8.7012, -8.5909, -8.5121, -8.4141,   cpmx5110
     9 -8.3444, -8.2687, -8.2003, -8.1571, -8.1141, -8.1261, -8.1848,   cpmx5120
     $ -8.2395, -8.2478, -8.0877, -7.7880, -7.5611, -7.4487, -7.4880,   cpmx5130
     1 -7.7644, -8.2142, -8.8765,-10.1091,-12.4483,-13.7228/            cpmx5140
      END                                                               cpmx5150
      function   CXDTA(V,IWL,IWH,CP,IND)                                fcxd 100
c                                                                       fcxd 110
c     this function is a bit different from and replaces                fcxd 120
c     the old routine, a subroutine, cxdta                              fcxd 130
c                                                                       fcxd 140
C     THIS SUBROUTINE FINDS THE C' FOR THE WAVENUMBER V.                fcxd 150
C     INPUT:         V --- WAVENUMBER                                   fcxd 160
C            (IWL,IWH) --- WAVENUMBER PAIR SPECIFIES THE ABSORPTION     fcxd 170
C                          REGION. BOTH ARE ARRAYS AND TERMINATED       fcxd 180
C                          WITH THE VALUE -999                          fcxd 190
C                   CP --- ARRAY CONTAINS THE C's                       fcxd 200
C     I/O:         IND --- INDICATOR INDICATES THE ABSORPTION REGION    fcxd 210
C                          WHERE THE WAVENUMBER IS EXPECTED TO BE IN    fcxd 220
C                          OR NEARBY (IT SERVES FOR THE PURPOSE         fcxd 230
C                          TO SPEED UP THE SEARCHING PROCESS)           fcxd 240
      DIMENSION IWL(*),IWH(*),CP(*)                                     fcxd 250
      IV=V                                                              fcxd 260
      cxdta=-20.0                                                       fcxd 270
      IF (IWL(IND+1) .EQ. -999 .AND. IV .GT. IWH(IND)) RETURN           fcxd 280
      IF (IV .LT. IWL(1)) RETURN                                        fcxd 290
      IC=0                                                              fcxd 300
  100 IF (IV .GE. IWL(IND) .AND. IV .LE. IWH(IND)) GO TO 200            fcxd 310
      IF (IV .GT. IWH(IND) .AND. IV .LT. IWL(IND+1)) RETURN             fcxd 320
      IND=IND+1                                                         fcxd 330
      IF (IWL(IND) .NE. -999) GO TO 100                                 fcxd 340
      IND=IND-1                                                         fcxd 350
      IF (IV .GT. IWH(IND)) RETURN                                      fcxd 360
      IND=1                                                             fcxd 370
      GO TO 100                                                         fcxd 380
  200 IF (IND .EQ. 1) GO TO 400                                         fcxd 390
      INDM1=IND-1                                                       fcxd 400
      DO 300 I=1,INDM1                                                  fcxd 410
        IC=IC+(IWH(I)-IWL(I))/5+1                                       fcxd 420
  300 CONTINUE                                                          fcxd 430
  400 IC=IC+(IV-IWL(IND))/5+1                                           fcxd 440
      cxdta=CP(IC)                                                      fcxd 450
      RETURN                                                            fcxd 460
      END                                                               fcxd 470
      SUBROUTINE DEBYE(WAVL,TC,KEY,RE,AI)                               dbye 100
CCC                                                                     dbye 110
CCC    CALCULATES WAVENUMBER DEPENDENCE OF DIELECTRIC CONSTANT          dbye 120
CCC    OF WATER                                                         dbye 130
CCC                                                                     dbye 140
      COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     dbye 150
      T=TC+273.15                                                       dbye 160
      IF(KEY.NE.0) GO TO 1                                              dbye 170
      GO TO 2                                                           dbye 180
    1 EFIN=5.27137+.0216474*TC-.00131198*TC*TC                          dbye 190
      ALPHA=-16.8129/T+.0609265                                         dbye 200
      TAU=.00033836*EXP(2513.98/T)                                      dbye 210
      SIG=12.5664E+08                                                   dbye 220
      ES=78.54*(1.-.004579*(TC-25.)+.0000119*(TC-25.)**2-.000000028*    dbye 230
     1(TC-25.)**3)                                                      dbye 240
      GO TO 3                                                           dbye 250
    2 EFIN=3.168                                                        dbye 260
      ALPHA=.00023*TC*TC+.0052*TC+.288                                  dbye 270
      T125 = 12500./(T*1.9869)                                          dbye 280
      IF(T125. LE.BIGEXP) THEN                                          dbye 290
           SIG=1.26*EXP(-T125)                                          dbye 300
      ELSE                                                              dbye 310
           SIG = 0.                                                     dbye 320
      ENDIF                                                             dbye 330
      TAU=9.990288E-05*EXP(13200./(T*1.9869))                           dbye 340
      ES=3.168+.15*TC*TC+2.5*TC+200.                                    dbye 350
    3 C1=TAU/WAVL                                                       dbye 360
CCC                                                                     dbye 370
CCC    TEMPORARY FIX TO CLASSICAL DEBYE EQUATION                        dbye 380
CCC    TO HANDLE ZERO CM-1 PROBLEM                                      dbye 390
CCC                                                                     dbye 400
      ALPHA=0.0                                                         dbye 410
      SIG=0.0                                                           dbye 420
CCC                                                                     dbye 430
      C2=1.5708*ALPHA                                                   dbye 440
      DEM=1.+2.*C1**(1.-ALPHA)*SIN(C2)+C1**(2.*(1.-ALPHA))              dbye 450
      E1=EFIN+(ES-EFIN)*(1.+(C1**(1.-ALPHA)*SIN(C2)))/DEM               dbye 460
      IF(KEY.NE.0.AND.WAVL.GE.300.) E1=87.53-0.3956*TC                  dbye 470
      IF(KEY.NE.0 .AND. WAVL.GE.300.) E1=ES                             dbye 480
      E2=(ES-EFIN)*C1**(1.-ALPHA)*COS(C2)/DEM+SIG*WAVL/18.8496E+10      dbye 490
CCC                                                                     dbye 500
CCC    PERMANENT FIX TO CLSSICAL DEBYE EQUATION                         dbye 510
CCC    TO HANDLE ZERO CM-1 PROBLEM                                      dbye 520
CCC                                                                     dbye 530
      E1=EFIN+(ES-EFIN)/(1.0+C1**2)                                     dbye 540
CCC                                                                     dbye 550
      E2=((ES-EFIN)*C1)/(1.0+C1**2)                                     dbye 560
CCC                                                                     dbye 570
      RE=SQRT((E1+SQRT(E1*E1+E2*E2))/2.)                                dbye 580
      AI=-E2/(2.*RE)                                                    dbye 590
      RETURN                                                            dbye 600
      END                                                               dbye 610
      FUNCTION   DEL(PSIO,DELO,BETA,IARBO)                              del  100
C                                                                       del  110
C     FUNCTION DEL RETURNS THE VALUE OF THE SUN'S ZENITH ANGLE          del  120
C     AT ANY POINT ALONG THE OPTICAL PATH BASED UPON STRAIGHT           del  130
C     LINE GEOMETRY (NO REFRACTION). THIS ANGLE IS USED TO SPECIFY      del  140
C     THE SCATTERING POINT TO SUN PATHS. THE BENDING DUE TO REFRACTION  del  150
C     ALONG THIS PATH IS DETERMINED BY THE GEO ROUTINES. IF THE BENDING del  160
C     IS GREATER THAN ONE DEGREE THE ZENITH ANGLE IS CORRECTED ACCORDINGdel  170
C     AND THE PATH CALCULATION IS REPEATED.                             del  180
      COMMON /PARMTR/ RE,DELTAS,ZMAX,IMAX,IMOD,IBMAX,IPATH              del  190
      COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     del  200
      DATA  EPSILN/1.0E-5/                                              del  210
C                                                                       del  220
      IF(IARBO.EQ.0) GO TO 10                                           del  230
C     SPECIAL CASES IF PSIO IS ARBITRARY                                del  240
      IF(IARBO.EQ.1) DEL=DELO                                           del  250
      IF(IARBO.EQ.2) DEL=BETA                                           del  260
      IF(IARBO.EQ.3) DEL=0.0                                            del  270
      RETURN                                                            del  280
10    CONTINUE                                                          del  290
      PSIOR=PSIO/DEG                                                    del  300
      DELOR=DELO/DEG                                                    del  310
      BETAR=BETA/DEG                                                    del  320
C     GENERAL CASE                                                      del  330
      X=COS(DELOR)*COS(BETAR)+SIN(DELOR)*SIN(BETAR)*COS(PSIOR)          del  340
      DEL=DEG*ACOS(X)                                                   del  350
      RETURN                                                            del  360
      END                                                               del  370
      SUBROUTINE DESATT(WSPD,VIS)                                       dest 100
C********************************************************************** dest 110
C*                                                                    * dest 120
C*    THIS SUBROUTINE CALCULATES THE ATTENUATION COEFFICIENTS AND     * dest 130
C*    ASYMMETRY PARAMETER FOR THE DESERT AEROSOL BASED ON THE WIND    * dest 140
C*    SPEED AND METEOROLOGICAL RANGE                                  * dest 150
C*                                                                    * dest 160
C*                                                                    * dest 170
C*                                                                    * dest 180
C*    PROGRAMMED BY:  D. R. LONGTIN         OPTIMETRICS, INC.         * dest 190
C*                                          BURLINGTON, MASSACHUSETTS * dest 200
C*                                          JULY 1987                 * dest 210
C*                                                                    * dest 220
C*                                                                    * dest 230
C*    INPUTS:    WSPD    -  WIND SPEED (IN M/S) AT 10 M               * dest 240
C*               VIS     -  METEOROLOGICAL RANGE (KM)                 * dest 250
C*                                                                    * dest 260
C*    OUTPUTS:   DESEXT  -  EXTINCTION COEFFICIENT AT 47 WAVELENGTHS  * dest 270
C*               DESSCA  -  SCATTERING COEFFICIENT AT 47 WAVELENGTHS  * dest 280
C*    *****      DESABS  -  ABSORPTION COEFFICIENT AT 47 WAVELENGTHS  * dest 290
C*               DESG    -  ASYMMETRY PARAMETER AT 47 WAVELENGTHS     * dest 300
C*                                                                    * dest 310
C********************************************************************** dest 320
C                                                                       dest 330
      include 'parameter.list'
      COMMON RELHUM(laydim),HSTOR(laydim),ICH(4),VH(17),TX(65),W(65)  
      COMMON IMSMX,WPATH(laythr,65),TBBY(laythr),PATM(laythr),NSPEC,   
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX2(47),AWCCON(5)  
      COMMON/ DESAER/EXT(47,4),ABS(47,4),G(47,4)                        dest 380
      DIMENSION DESEXT(47),DESSCA(47),DESABS(47),DESG(47),WIND(4)       dest 390
      REAL      DESEXT    ,DESSCA    ,DESABS    ,DESG    ,WIND          dest 400
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      INTEGER WAVEL                                                     dest 420
      DATA WIND/0., 10., 20., 30./                                      dest 430
      DATA RAYSCT / 0.01159 /                                           dest 440
      IF(WSPD .LT. 0.) WSPD = 10.                                       dest 450
C                                                                       dest 460
      NWSPD = INT(WSPD/10) + 1                                          dest 470
      IF (NWSPD.GE.4) WRITE(ipr,999)                                    dest 480
      IF (NWSPD.GE.4) NWSPD = 3                                         dest 490
C                                                                       dest 500
C     INTERPOLATE THE RADIATIVE PROPERTIES AT WIND SPEED WSPD           dest 510
C                                                                       dest 520
      DO 100 WAVEL = 1,47                                               dest 530
C                                                                       dest 540
C     EXTINCTION COEFFICIENT                                            dest 550
C                                                                       dest 560
         SLOPE = LOG(EXT(WAVEL,NWSPD+1)/EXT(WAVEL,NWSPD))/              dest 570
     *           (WIND(NWSPD+1)-WIND(NWSPD))                            dest 580
         B = LOG(EXT(WAVEL,NWSPD+1)) - SLOPE*WIND(NWSPD+1)              dest 590
         DESEXT(WAVEL) = EXP(SLOPE*WSPD + B)                            dest 600
C                                                                       dest 610
C     ABSORPTION COEFFICIENT                                            dest 620
C                                                                       dest 630
         SLOPE = LOG(ABS(WAVEL,NWSPD+1)/ABS(WAVEL,NWSPD))/              dest 640
     *           (WIND(NWSPD+1)-WIND(NWSPD))                            dest 650
         B = LOG(ABS(WAVEL,NWSPD+1)) - SLOPE*WIND(NWSPD+1)              dest 660
         DESABS(WAVEL) = EXP(SLOPE*WSPD + B)                            dest 670
C                                                                       dest 680
C     SCATTERING COEFFICIENT                                            dest 690
C                                                                       dest 700
         DESSCA(WAVEL) = DESEXT(WAVEL) - DESABS(WAVEL)                  dest 710
C                                                                       dest 720
C     ASYMMETRY PARAMETER                                               dest 730
C                                                                       dest 740
         SLOPE = (G(WAVEL,NWSPD+1)-G(WAVEL,NWSPD))/(WIND(NWSPD+1)-      dest 750
     *           WIND(NWSPD))                                           dest 760
         B = G(WAVEL,NWSPD+1) - SLOPE*(WIND(NWSPD+1))                   dest 770
         DESG(WAVEL) = SLOPE*WSPD + B                                   dest 780
100   CONTINUE                                                          dest 790
C                                                                       dest 800
          EXT55 = DESEXT(4)                                             dest 810
C                                                                       dest 820
C         DETERMINE METEROLOGICAL RANGE FROM 0.55 EXTINCTION            dest 830
C          AND KOSCHMIEDER FORMULA                                      dest 840
C                                                                       dest 850
      IF (VIS. LE .0.) THEN                                             dest 860
               VIS = 3.912/(DESEXT(4) + RAYSCT )                        dest 870
      ENDIF                                                             dest 880
C                                                                       dest 890
C        RENORMALIZE ATTENUATION COEFFICIENTS TO 1.0 KM-1 AT            dest 900
C        0.55 MICRONS FOR CAPABILTY WITH LOWTRAN                        dest 910
C                                                                       dest 920
          DO 200 WAVEL=1,47                                             dest 930
            EXTC(1,WAVEL) = DESEXT(WAVEL)       /EXT55                  dest 940
CC          DESSCA(WAVEL) = DESSCA(WAVEL)       /EXT55                  dest 950
            ABSC(1,WAVEL) = DESABS(WAVEL)       /EXT55                  dest 960
            ASYM(1,WAVEL) = DESG(WAVEL)                                 dest 970
200      CONTINUE                                                       dest 980
       WRITE(ipr,900) VIS,WSPD                                          dest 990
900    FORMAT(//,'  VIS = ',F10.3,' WIND = ',F10.3)                     dest1000
       RETURN                                                           dest1010
C                                                                       dest1020
999    FORMAT(' WARNING: WIND SPEED IS BEYOND 30 M/S; RADIATIVE',       dest1030
     *'PROPERTIES',/,'OF THE DESERT AEROSOL HAVE BEEN EXTRAPOLATED')    dest1040
      END                                                               dest1050
      FUNCTION   DOP(WAVL,A,CEN1,B,C,CEN2,D,E,CEN3,F,G)                 fdop 100
CCC                                                                     fdop 110
CCC    DESCRIBES THE REAL PART OF THE DIELECTRIC CONSTANT               fdop 120
CCC                                                                     fdop 130
      V=1./WAVL                                                         fdop 140
      V2=V*V                                                            fdop 150
      H1=CEN1**2-V2                                                     fdop 160
      H2=CEN2**2-V2                                                     fdop 170
      H3=CEN3**2-V2                                                     fdop 180
      DOP=SQRT(A+B*H1/(H1*H1+C*V2)+D*H2/(H2*H2+E*V2)+F*H3/(H3*H3+G*V2)) fdop 190
      RETURN                                                            fdop 200
      END                                                               fdop 210
      DOUBLE PRECISION FUNCTION DPANDX(H,SH,GAMMA)                      dpan 100
C***********************************************************************dpan 110
C     DOUBLE PRECISION (DP) VERSION OF THE ROUTINE PREVIOUSLY CALLED ANDdpan 120
C     COMPUTES THE INDEX OF REFRACTION AT HEIGHT H, SH IS THE           dpan 130
C     SCALE HEIGHT, GAMMA IS THE VALUE AT H=0 OF THE REFRACTIVITY = INDEdpan 140
C***********************************************************************dpan 150
      REAL PI, CA, DEG,GCAIR,BIGNUM,BIGEXP                              dpan 160
      DOUBLE PRECISION H, SH, GAMMA, HSH                                dpan 170
      COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     dpan 180
      IF(SH.EQ.0.0) GO TO 10                                            dpan 190
      IF(H. LE. 0.) GO TO 10                                            dpan 200
C     HSH = H/SH                                                        dpan 210
C     IF(HSH .GT.BIGEXP) GO TO 10                                       dpan 220
C     DPANDX = 1.0+GAMMA*EXP(-HSH )                                     dpan 230
      HSH=H/SH-LOG(GAMMA)                                               dpan 240
      DPANDX=1.                                                         dpan 250
      IF(HSH.GT.BIGEXP)RETURN                                           dpan 260
      DPANDX=1.+EXP(-HSH)                                               dpan 270
      RETURN                                                            dpan 280
   10 DPANDX = 1.0+GAMMA                                                dpan 290
      RETURN                                                            dpan 300
      END                                                               dpan 310
      SUBROUTINE DPEXNT(X,X1,X2,A)                                      dpex 100
C                                                                       dpex 110
C     DOUBLE PRECISION VERSION OF THE ROUTINE EXPINT                    dpex 120
C                                                                       dpex 130
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)                               dpex 140
C     EXPONENTIAL INTERPOLATION                                         dpex 150
cj db
         if(x1.lt.0.)x1=0.
         if(x2.lt.0.)x2=0.
cj
      IF(X1.EQ.0.0 .OR. X2.EQ.0.0)  GO TO 100                           dpex 160
      X = X1*(X2/X1)**A                                                 dpex 170
      RETURN                                                            dpex 180
  100 X = X1+(X2-X1)*A                                                  dpex 190
      RETURN                                                            dpex 200
      END                                                               dpex 210
      SUBROUTINE DPFILL(HA,HB,JNEXT)                                    dpfl 100
C********************************************************************   dpfl 110
C     THIS SUBROUTINE DEFINES THE ATMOSPHERIC BOUNDARIES OF THE PATH    dpfl 120
C     FROM HA TO HB AND INTERPOLATES (EXTRAPOLATES) THE DENSITIES TO    dpfl 130
C     THESE BOUNDARIES ASSUMING THE DENSITIES VARY EXPONENTIALLY        dpfl 140
C     WITH HEIGHT                                                       dpfl 150
C********************************************************************   dpfl 160
      INTEGER IRD, IPR, IPU, NPR, IPR1, KMAX, M, IKMAX, NL, ML,IKLO,    dpfl 170
     $     ISSGEO, IMULT,                                               dpfl 180
     $     I, J, JNEXT,IA,IB,I2,IPATH,IMAX,IMOD,IBMAX,K,I1              dpfl 190
      LOGICAL LSMALL                                                    dpfl 200
      DOUBLE PRECISION ZP,PP,TP,RFNDXP,                                 dpfl 210
     $     SP,PPSUM,TPSUM,RHOPSM,DENP,AMTP                              dpfl 220
      INCLUDE 'parameter.list'
      DOUBLE PRECISION                                                  dpfl 230
     $     HA, HB, Z(LAYDIM), P(LAYDIM), T(LAYDIM), RFNDX(LAYDIM),
     $     DENSTY(65, LAYDIM),A                                         dpfl 250
c                                                                       cfc
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
c     modtran has 65 as magic number.  It INCLUDEs the usual 12 species
c     plus a host of other species and sub species.  Many
c     arrays have dimension 65.
c
      DOUBLE PRECISION DI1X, DI2X, DJNXTX
C
      COMMON /MODELX/ DNSTYX(MMOLX,LAYDIM)                                  
      COMMON /RFRPTX/ DENPX(MMOLX,laydim+1),AMTPX(MMOLX,laydim+1)
c
c                                                                       cfc
C                                                                       dpfl 260
C     SSI COMMENTS ON DOUBLE PRECISION VARIABLES:                       dpfl 270
C     RFRPTH IS THE OLD COMMON BLOCK IN SINGLE PRECISION.               dpfl 280
C     DPRFRP IS THE SAME COMMON BLOCK IN DOUBLE PRECISION; IT IS NEW.   dpfl 290
C     IN THIS ROUTINE SP IS USED AS A PREFIX TO DENOTE THE              dpfl 300
C     SINGLE PRECISION VARIABLES OF RFRPTH.                             dpfl 310
C     THE FOLLOWING ARE THE EXCEPTIONS:                                 dpfl 320
C     SPRFN  STANDS FOR THE OLD SINGLE PRECISION RFNDXP                 dpfl 330
C     SPPPSU STANDS FOR THE OLD SINGLE PRECISION PPSUM                  dpfl 340
C     SPTPSU STANDS FOR THE OLD SINGLE PRECISION TPSUM                  dpfl 350
C     SPRHOP STANDS FOR THE OLD SINGLE PRECISION RHOPSM                 dpfl 360
C     THE VARIABLES OF THE DOUBLE PRECISION BLOCK DPRFRP HAVE THE       dpfl 370
C     SAME NAMES AS THOSE OF THE ORIGINAL SINGLE PRECiSION BLOCK;       dpfl 380
C     THAT IS, WITHOUT ANY PREFIXES.                                    dpfl 390
C                                                                       dpfl 400
C     SOME VARIABLES IN THE COMMON BLOCK "MODEL" WERE NEEDED            dpfl 410
C     IN DP.  SP WAS PREFIXED TO FORM SPZ (SP+Z),                       dpfl 420
C     SPP (SP+P), SPT (SP+T), SPRFND (SP+RFNDXP), AND                   dpfl 430
C     SPDENS (SP+DENSTY).                                               dpfl 440
C     SPZ, SPP, SPT, SPRFND AND SPDENS REPLACED Z, P, T, RFNDXP         dpfl 450
C     AND DENSTY IN THE COMMON BLOCK MODEL WHICH REMAINED IN SINGLE     dpfl 460
C     PRECISION.  Z, P, T, RFNDXP AND DENSTY WERE DECLARED TO           dpfl 470
C     BE DOUBLE.                                                        dpfl 480
C                                                                       dpfl 490
C     SOME OTHER VARIABLES WERE DECLARED IN DOUBLE PRECISION.           dpfl 500
C     THEIR SP COUNTERPARTS WERE PREFIXED WITH "SP".                    dpfl 510
C     SINCE THESE DO NOT INVOLVE COMMON BLOCKS NOTHING MORE ABOUT       dpfl 520
C     THEM IS SAID.  SEE THE DECLARATIONS ABOVE FOR THE SPECIFIC        dpfl 530
C     VARIABLES.                                                        dpfl 540
C                                                                       dpfl 550
      COMMON /IFIL/ IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON /MODEL/SPZ(LAYDIM),SPP(LAYDIM),SPT(LAYDIM),SPRFND(LAYDIM),
     $  SPDENS(65,LAYDIM),CLDAMT(LAYDIM),RRAMT(LAYDIM),EQLWC(LAYDIM),
     $  HAZEC(LAYDIM)
      COMMON /PARMTR/ RE,DELTAS,ZMAX,IMAX,IMOD,IBMAX,IPATH              dpfl 590
C     COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     dpfl 600
      COMMON /CNTRL/ KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT               dpfl 610
      COMMON /RFRPTH/ SPZP(LAYDIM+1),SPPP(LAYDIM+1),SPTP(LAYDIM+1),
     $ SPRFN(LAYDIM+1),SPSP(LAYDIM+1),SPPPSU(LAYDIM+1),SPTPSU(LAYDIM+1)
     $ ,SPRHOP(LAYDIM+1),SPDENP(65,LAYDIM+1),
     $     SPAMTP(65,laydim+1)                                          dpfl 640
      COMMON /DPRFRP/ ZP(LAYDIM+1),PP(LAYDIM+1),TP(LAYDIM+1),
     $ RFNDXP(LAYDIM+1),SP(LAYDIM+1), PPSUM(LAYDIM+1),TPSUM(LAYDIM+1),
     $ RHOPSM(LAYDIM+1),DENP(65,LAYDIM+1), AMTP(65,LAYDIM+1) 
      COMMON /SMALL2/LSMALL                                             dpfl 680
      DO 9 I = 1, LAYDIM                                                    dpfl 690
         Z(I) = SPZ(I)                                                  dpfl 700
         P(I) = SPP(I)                                                  dpfl 710
         T(I) = SPT(I)                                                  dpfl 720
         RFNDX(I) = SPRFND(I)                                           dpfl 730
         DO 9999 J = 1, kmax + 2                                        dpfl 740
            DENSTY(J,I) = SPDENS(J,I)                                   dpfl 750
 9999    CONTINUE                                                       dpfl 760
C        FOR CFC'S, WE ALWAYS USE SINGLE PRECISION VARIABLES, SO NO     CFC
C        NEED FOR THE ABOVE LOOP FOR CFC'S.                             CFC
C                                                                       CFC
 9    CONTINUE                                                          dpfL 770
C                                                                       cfc
      DO 99 I = 1, LAYDIM+1                                                   dpfl 780
         ZP(I) = SPZP(I)                                                dpfl 790
         PP(I) = SPPP(I)                                                dpfl 800
         TP(I) = SPTP(I)                                                dpfl 810
         RFNDXP(I) = SPRFN(I)                                           dpfl 820
         DO 999 J = 1, kmax +2                                          dpfl 830
            DENP(J,I) = SPDENP(J,I)                                     dpfl 840
 999     CONTINUE                                                       dpfl 850
C        FOR CFC'S, WE ALWAYS USE SINGLE PRECISION VARIABLES, SO NO     CFC
C        NEED FOR THE ABOVE LOOP FOR CFC'S.                             CFC
C                                                                       cfc
 99   CONTINUE                                                          dpfl 860
c
c     IF(HA.GE.HB .AND. LSMALL .NE. .TRUE.) THEN                        dpfl 870
      IF(HA.GE.HB .AND.    (.not. LSMALL )) THEN                        dpfl 870
         WRITE(IPR,22) HA,HB,JNEXT                                      dpfl 880
 22      FORMAT('0SUBROUTINE DPFILL- ERROR, HA .GE. HB',//,             dpfl 890
     $        10X,'HA, HB, JNEXT = ',2E25.15,I6)                        dpfl 900
         STOP                                                           dpfl 910
      ENDIF                                                             dpfl 920
C***  FIND Z(IA):  THE SMALLEST Z(I).GT.HA                              dpfl 930
      DO 100 I=1,IMAX                                                   dpfl 940
         IF(HA.LT.Z(I)) THEN                                            dpfl 950
            IA = I                                                      dpfl 960
            GO TO 110                                                   dpfl 970
         ENDIF                                                          dpfl 980
 100  CONTINUE                                                          dpfl 990
      IA = IMAX+1                                                       dpfl1000
      IB = IA                                                           dpfl1010
      GO TO 130                                                         dpfl1020
C***  FIND Z(IB):  THE SMALLEST Z(I).GE.HB                              dpfl1030
 110  CONTINUE                                                          dpfl1040
      DO 120 I=IA,IMAX                                                  dpfl1050
         IF(HB-Z(I).LE. .0001) THEN                                     dpfl1060
            IB = I                                                      dpfl1070
            GO TO 130                                                   dpfl1080
         ENDIF                                                          dpfl1090
 120  CONTINUE                                                          dpfl1100
      IB = IMAX+1                                                       dpfl1110
 130  CONTINUE                                                          dpfl1120
C***  INTERPOLATE DENSITIES TO HA,HB                                    dpfl1130
      ZP(JNEXT) = HA                                                    dpfl1140
      I2 = IA                                                           dpfl1150
      IF(I2.EQ.1) I2 = 2                                                dpfl1160
      IF(I2.GT.IMAX) I2 = IMAX                                          dpfl1170
      I1 = I2-1                                                         dpfl1180
      A = (HA-Z(I1))/(Z(I2)-Z(I1))                                      dpfl1190
      CALL DPEXNT(PP(JNEXT),P(I1),P(I2),A)                              dpfl1200
      TP(JNEXT) = T(I1)+(T(I2)-T(I1))*A                                 dpfl1210
      CALL DPEXNT(RFNDXP(JNEXT),RFNDX(I1),RFNDX(I2),A)                  dpfl1220
      DO 140 K=1,KMAX +2                                                dpfl1230
         CALL DPEXNT(DENP(K,JNEXT),DENSTY(K,I1),DENSTY(K,I2),A)         dpfl1240
 140  CONTINUE                                                          dpfl1250
C                                                                       CFC
      DO 145 KX = 1, NSPECX                                             CFC
C        STORE SINGLE PRECISION VARIABLES IN DOUBLE PRECISION.          CFC
         DI1X = DNSTYX(KX,I1)                                           CFC
         DI2X = DNSTYX(KX,I2)                                           CFC
         CALL DPEXNT(DJNXTX, DI1X, DI2X, A)                             CFC
         DENPX(KX,JNEXT) = DJNXTX                                       CFC
c        back to single precision
 145  CONTINUE                                                          CFC
C                                                                       CFC
      IF(IA.NE.IB) THEN                                                 dpfl1260
C***     FILL IN DENSITIES BETWEEN HA AND HB                            dpfl1270
         I1 = IA                                                        dpfl1280
         I2 = IB-1                                                      dpfl1290
         DO 151 I=I1,I2                                                 dpfl1300
            JNEXT = JNEXT+1                                             dpfl1310
            ZP(JNEXT) = Z(I)                                            dpfl1320
            PP(JNEXT) = P(I)                                            dpfl1330
            TP(JNEXT) = T(I)                                            dpfl1340
            RFNDXP(JNEXT) = RFNDX(I)                                    dpfl1350
            DO 150 K=1,KMAX+2                                           dpfl1360
               DENP(K,JNEXT) = DENSTY(K,I)                              dpfl1370
 150        CONTINUE                                                    dpfl1380
c                                                                       cfc
            DO 155 Kx=1,nspecx                                          cfc
               DENPx(Kx,JNEXT) = DNSTYx(Kx,I)                           cfc
 155        CONTINUE                                                    cfc
c                                                                       cfc
 151     CONTINUE                                                       dpfl1390
      ENDIF                                                             dpfl1400
C***  INTERPOLATE THE DENSITIES TO HB                                   dpfl1410
      JNEXT = JNEXT+1                                                   dpfl1420
      ZP(JNEXT) = HB                                                    dpfl1430
      I2 = IB                                                           dpfl1440
      IF(I2.EQ.1) I2 = 2                                                dpfl1450
      IF(I2.GT.IMAX) I2 = IMAX                                          dpfl1460
      I1 = I2-1                                                         dpfl1470
      A = (HB-Z(I1))/(Z(I2)-Z(I1))                                      dpfl1480
      CALL DPEXNT(PP(JNEXT),P(I1),P(I2),A)                              dpfl1490
      TP(JNEXT) = T(I1)+(T(I2)-T(I1))*A                                 dpfl1500
      CALL DPEXNT(RFNDXP(JNEXT),RFNDX(I1),RFNDX(I2),A)                  dpfl1510
      DO 170 K=1,KMAX+2                                                 dpfl1520
         CALL DPEXNT(DENP(K,JNEXT),DENSTY(K,I1),DENSTY(K,I2),A)         dpfl1530
 170  CONTINUE                                                          dpfl1540
C                                                                       CFC
      DO 175 KX = 1, NSPECX                                             CFC
C        STORE SINGLE PRECISION VARIABLES IN DOUBLE PRECISION.          CFC
         DI1X = DNSTYX(KX,I1)                                           CFC
         DI2X = DNSTYX(KX,I2)                                           CFC
         CALL DPEXNT(DJNXTX, DI1X, DI2X, A)                             CFC
         DENPX(KX,JNEXT) = DJNXTX                                       CFC
c        back to single precision
 175  CONTINUE                                                          CFC
C                                                                       CFC
      DO 1 I = 1, LAYDIM                                                dpfl1550
         SPZ(I) = Z(I)                                                  dpfl1560
         SPP(I) = P(I)                                                  dpfl1570
         SPT(I) = T(I)                                                  dpfl1580
         SPRFND(I) = RFNDX(I)                                           dpfl1590
         DO 1111 J = 1, kmax+2                                          dpfl1600
            SPDENS(J,I) = DENSTY(J,I)                                   dpfl1610
 1111    CONTINUE                                                       dpfl1620
C        NO NEED FOR A LOOP CORREPONDING TO ABOVE FOR CFC'S.            CFC
 1    CONTINUE                                                          dpfl1630
      DO 11 I = 1, laydim+1                                             dpfl1640
         SPZP(I) = ZP(I)                                                dpfl1650
         SPPP(I) = PP(I)                                                dpfl1660
         SPTP(I) = TP(I)                                                dpfl1670
         SPRFN(I) = RFNDXP(I)                                           dpfl1680
         DO 111 J = 1, kmax+2                                           dpfl1690
            SPDENP(J,I) = DENP(J,I)                                     dpfl1700
 111     CONTINUE                                                       dpfl1710
C        NO NEED FOR A LOOP CORREPONDING TO ABOVE FOR CFC'S.            CFC
 11   CONTINUE                                                          dpfl1720
      RETURN                                                            dpfl1730
      END                                                               dpfl1740
      SUBROUTINE DPFISH(H,SH,GAMMA)                                     dpfl1750
C                                                                       dpfl1760
C     DOUBLE PRECISION VERSION OF THE PREVIOUS ROUTINE FINDSH.          dpfl1770
C                                                                       dpfl1780
C*****GIVEN AN ALTITUDE H, THIS SUBROUTINE FINDS THE LAYER BOUNDARIES   dpfl1790
C*****ZM(I1) AND ZM(I2) WHICH CONTAIN H,  THEN CALCULATES THE SCALE     dpfl1800
C*****HEIGHT (SH) AND THE VALUE AT THE GROUND (GAMMA+1) FOR THE         dpfl1810
C*****INDEX OF REFRACTION                                               dpfl1820
C                                                                       dpfl1830
      include 'parameter.list'
      DOUBLE PRECISION H, SH, GAMMA, Z1,Z2,RFNDX1,RFNDX2                dpfl1840
      COMMON /PARMTR/ RE,DELTAS,ZMAX,IMAX,IMOD,IBMAX,IPATH              dpfl1850
C     COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     dpfl1860
      COMMON /MODEL/ Z(LAYDIM),PM(LAYDIM),TM(LAYDIM),RFNDX(LAYDIM),
     1  DENSTY(65,LAYDIM),CLDAMT(LAYDIM),RRAMT(LAYDIM),EQLWC(LAYDIM),
     1  HAZEC(LAYDIM)
      DO 100 IM=2,IMOD                                                  dpfl1890
      I2 = IM                                                           dpfl1900
      IF(Z(IM).GE.REAL(H))  GO TO 110                                   dpfl1910
  100 CONTINUE                                                          dpfl1920
      I2 = IMOD                                                         dpfl1930
  110 CONTINUE                                                          dpfl1940
      I1 = I2-1                                                         dpfl1950
      Z1 = Z(I1)                                                        dpfl1960
      Z2 = Z(I2)                                                        dpfl1970
      RFNDX1 = RFNDX(I1)                                                dpfl1980
      RFNDX2 = RFNDX(I2)                                                dpfl1990
      CALL DPSCHT(Z1,Z2,RFNDX1,RFNDX2,SH,GAMMA)                         dpfl2000
      RETURN                                                            dpfl2010
      END                                                               dpfl2020
      SUBROUTINE DPFNMN(H1,ANGLE,H2,LEN,ITER,HMIN,PHI,IERROR)           dpfn 100
C***********************************************************************dpfn 110
C     DOUBLE PRECISION VERSION OF THE FORMER ROUTINE FNDHMN.            dpfn 120
C                                                                       dpfn 130
C     THIS SUBROUTINE CALCULATES THE MINIMUM ALTITUDE HMIN ALONG        dpfn 140
C     THE REFRACTED PATH AND THE FINAL ZENITH ANGLE PHI.                dpfn 150
C     THE PARAMETER LEN INDICATES WHETHER THE PATH GOES THROUGH         dpfn 160
C     A TANGENT HEIGHT (LEN=1) OR NOT (LEN=0).  IF ANGLE > 90 AND       dpfn 170
C     H1 > H2, THEN LEN CAN EITHER BE 1 OR 0, AND THE CHOICE IS         dpfn 180
C     LEFT TO THE USER.                                                 dpfn 190
C     THE (INDEX OF REFRACTION - 1.0) IS MODELED AS AN EXPONENTIAL      dpfn 200
C     BETWEEN THE LAYER BOUNDARIES, WITH A SCALE HEIGHT SH AND AN       dpfn 210
C     AMOUNT AT THE GROUND GAMMA.                                       dpfn 220
C     CPATH IS THE REFRACTIVE CONSTANT FOR THIS PATH AND                dpfn 230
C     EQUALS  INDEX(H1)*(RE+H1)*SIN(ANGLE).                             dpfn 240
C***********************************************************************dpfn 250
      INTEGER IRD, IPR, IPU, NPR, IPR1,IMAX,IMOD,IBMAX,IPATH,KMAX,      dpfn 260
     $     M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT,LEN,ITER,IERROR,N            dpfn 270
      DOUBLE PRECISION H1, ANGLE, H2, HMIN, PHI,CRFRCT,H,DPANDX,        dpfn 280
     $     SH,GAMMA,CPATH,CH2,CMIN,HT1,CT1,HT2,CT2,HT3,CT3,DC,HT,       dpfn 290
     $     HTAN                                                         dpfn 300
C                                                                       dpfn 310
C     HTAN IS USED CALCULATE TANGENT HEIGHT FOR PRINTING ERROR MESSAGES dpfn 320
C     HTAN IS NOT MEANT TO REPLACE HT WHICH HOLDS THE TANGENT HEIGHT IN dpfn 330
C     THIS ROUTINE AS FAR AS MODTRAN GOES.  FOR SOME INPUT ERRORS       dpfn 340
C     HT IS CALCULATED.                                                 dpfn 350
C                                                                       dpfn 360
      REAL RE, DELTAS, ZMAX,PI,CA,DEG,GCAIR,BIGNUM,BIGEXP,DH,ETA        dpfn 370
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON /PARMTR/ RE,DELTAS,ZMAX,IMAX,IMOD,IBMAX,IPATH              dpfn 390
      COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     dpfn 400
      COMMON /CNTRL/ KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT               dpfn 410
      DATA DH/1.0/,ETA/5.0E-8/                                          dpfn 420
C***  ETA MAY BE TOO SMALL FOR SOME COMPUTERS. TRY 1.0E-7 FOR 32 BIT    dpfn 430
C***  WORD MACHINES                                                     dpfn 440
C     CRFRCT IS REFRACTIVE CONSTANT FOR THE PATH                        dpfn 450
      CRFRCT(H) = (RE+H)*DPANDX(H,SH,GAMMA)                             dpfn 460
C                                                                       dpfn 470
      N = 0                                                             dpfn 480
      CALL DPFISH(H1,SH,GAMMA)                                          dpfn 490
      CPATH = CRFRCT(H1)*SIN(ANGLE/DEG)                                 dpfn 500
      CALL DPFISH(H2,SH,GAMMA)                                          dpfn 510
      CH2 = CRFRCT(H2)                                                  dpfn 520
      IF(ABS(CPATH/CH2).LE.1.0) THEN                                    dpfn 530
         IF(ANGLE.LE.90.0)  THEN                                        dpfn 540
            LEN = 0                                                     dpfn 550
            HMIN = H1                                                   dpfn 560
C***        CALCULATE THE ZENITH ANGLE PHI AT H2                        dpfn 570
            PHI = ASIN(CPATH/CH2)*DEG                                   dpfn 580
            IF(ANGLE.LE.90.0 .OR. LEN.EQ.1)  PHI = 180.0-PHI            dpfn 590
            RETURN                                                      dpfn 600
         ENDIF                                                          dpfn 610
         IF(H1.LE.H2)  LEN = 1                                          dpfn 620
         IF(LEN.NE.1)  THEN                                             dpfn 630
            LEN = 0                                                     dpfn 640
            HMIN = H2                                                   dpfn 650
C***        CALCULATE THE ZENITH ANGLE PHI AT H2                        dpfn 660
            PHI = ASIN(CPATH/CH2)*DEG                                   dpfn 670
            IF(ANGLE.LE.90.0 .OR. LEN.EQ.1)  PHI = 180.0-PHI            dpfn 680
            RETURN                                                      dpfn 690
         ENDIF                                                          dpfn 700
C***     LONG PATH THROUGH A TANGENT HEIGHT.                            dpfn 710
C***     SOLVE ITERATIVELY FOR THE TANGENT HEIGHT HT.                   dpfn 720
C***     HT IS THE HEIGHT FOR WHICH  INDEX(HT)*(RE+HT) = CPATH.         dpfn 730
         CALL DPFISH(0.0D0,SH,GAMMA)                                    dpfn 740
         CMIN = CRFRCT(0.0D0)                                           dpfn 750
C***     FOR BETA CASES (ITER>0), ALLOW FOR HT < 0.0                    dpfn 760
         IF(ITER.NE.0 .OR. CPATH.GE.CMIN)  THEN                         dpfn 770
            HT1 = (RE+H1)*SIN(ANGLE/DEG)-RE                             dpfn 780
            CALL DPFISH(HT1,SH,GAMMA)                                   dpfn 790
            CT1 = CRFRCT(HT1)                                           dpfn 800
            HT2 = HT1-DH                                                dpfn 810
            CALL DPFISH(HT2,SH,GAMMA)                                   dpfn 820
            CT2 = CRFRCT(HT2)                                           dpfn 830
C***        ITERATE TO FIND HT                                          dpfn 840
            N = 2                                                       dpfn 850
 120        CONTINUE                                                    dpfn 860
            IF(CT2 .NE. CT1) THEN                                       dpfn 870
               N = N+1                                                  dpfn 880
               HT3 = HT2+(HT2-HT1)*(CPATH-CT2)/(CT2-CT1)                dpfn 890
               CALL DPFISH(HT3,SH,GAMMA)                                dpfn 900
               CT3 = CRFRCT(HT3)                                        dpfn 910
               DC = CPATH-CT3                                           dpfn 920
               IF(ABS((CPATH-CT3)/CPATH).LT.ETA) THEN                   dpfn 930
                  HT = HT3                                              dpfn 940
                  HMIN = HT                                             dpfn 950
C***              CALCULATE THE ZENITH ANGLE PHI AT H2                  dpfn 960
                  PHI = ASIN(CPATH/CH2)*DEG                             dpfn 970
                  IF(ANGLE.LE.90.0 .OR. LEN.EQ.1)  PHI = 180.0-PHI      dpfn 980
                  RETURN                                                dpfn 990
               ENDIF                                                    dpfn1000
               IF(N.GT.15) THEN                                         dpfn1010
                  DC = CPATH-CT3                                        dpfn1020
                  WRITE(IPR,24)  N,CPATH,CT3,DC,HT3                     dpfn1030
                  GO TO 25                                              dpfn1040
               ENDIF                                                    dpfn1050
               HT1 = HT2                                                dpfn1060
               CT1 = CT2                                                dpfn1070
               HT2 = HT3                                                dpfn1080
               CT2 = CT3                                                dpfn1090
               GO TO 120                                                dpfn1100
            ENDIF                                                       dpfn1110
            HT3 = HT2                                                   dpfn1120
            HT = HT3                                                    dpfn1130
            HMIN = HT                                                   dpfn1140
C***        CALCULATE THE ZENITH ANGLE PHI AT H2                        dpfn1150
            PHI = ASIN(CPATH/CH2)*DEG                                   dpfn1160
            IF(ANGLE.LE.90.0 .OR. LEN.EQ.1)  PHI = 180.0-PHI            dpfn1170
            RETURN                                                      dpfn1180
         ENDIF                                                          dpfn1190
         IF(ISSGEO.NE.0) THEN                                           dpfn1200
            IERROR=-5.                                                  dpfn1210
            RETURN                                                      dpfn1220
         ENDIF                                                          dpfn1230
C***     TANGENT PATH INTERSECTS EARTH                                  dpfn1240
         H2 = 0.0                                                       dpfn1250
         HMIN = 0.0                                                     dpfn1260
         LEN = 0                                                        dpfn1270
         CH2 = CMIN                                                     dpfn1280
         IF(ISSGEO.NE.1)WRITE(IPR,22) H1,ANGLE                          dpfn1290
C***     CALCULATE THE ZENITH ANGLE PHI AT H2                           dpfn1300
         PHI = ASIN(CPATH/CH2)*DEG                                      dpfn1310
         IF(ANGLE.LE.90.0 .OR. LEN.EQ.1)  PHI = 180.0-PHI               dpfn1320
         RETURN                                                         dpfn1330
      ENDIF                                                             dpfn1340
C                                                                       dpfn1350
C***  H2 LT TANGENT HEIGHT FOR THIS H1 AND ANGLE                        dpfn1360
      IERROR = 2                                                        dpfn1370
 25   CALL TANHT(CPATH,HTAN,H1)                                         dpfn1380
      WRITE(IPR,20)HTAN                                                 dpfn1390
C                                                                       dpfn1400
 20   FORMAT('0H2 IS LESS THAN THE TANGENT HEIGHT FOR THIS PATH AND ',  dpfn1410
     $'CAN''T BE REACHED;',/,' TANGENT HEIGHT = ',1X,F7.3,/)            dpfn1420
 22   FORMAT(///,' TANGENT PATH WITH H1 = ',F10.3,' AND ANGLE = ',      dpfn1430
     $     F10.3,' INTERSECTS THE EARTH',//,10X,'H2 HAS BEEN RESET ',   dpfn1440
     $     'TO 0.0 AND LEN TO 0')                                       dpfn1450
 24   FORMAT(///,'0FROM SUBROUTINE FNDHMN :',//,                        dpfn1460
     $     10X,'THE PROCEEDURE TO FIND THE TANGENT HEIGHT DID NOT ',    dpfn1470
     $     'CONVERGE AFTER ',I3,'  ITERATIONS',//,                      dpfn1480
     $     10X,'CPATH   = ',F12.5,' KM',//,10X,'CT3     = ',F12.5,' KM',dpfn1490
     $     //,10X,'DC      = ',E12.3,' KM',//,                          dpfn1500
     $     10X,'HT3     = ',F12.5,' KM')                                dpfn1510
      END                                                               dpfn1520
      SUBROUTINE DPLAYR(J,SINAI,COSAI,CPATH,SH,GAMMA,IAMT,S,BEND,       dpla 100
     $     RNG)                                                         dpla 110
      INCLUDE 'parameter.list'
c                                                                       dpla 120
C     DOUBLE PRECISION VERSION OF THE PREVIOUS ROUTINE "LAYER"          dpla 130
C*****************************************************************      dpla 140
C     THIS SUBROUTINE CALCULATES THE REFRACTED PATH FROM Z1 TO Z2       dpla 150
C     WITH THE SIN OF THE INITIAL INCIDENCE ANGLE SINAI                 dpla 160
C*****************************************************************      dpla 170
      DOUBLE PRECISION ZP,PP,TP,RFNDXP,SP,PPSUM,TPSUM,RHOPSM,DENP,      dpla 180
     $     AMTP                                                         dpla 190
      DOUBLE PRECISION HDEN(65),DENA(65),DENB(65),DPANDX                dpla 200
      REAL RE, DELTAS,ZMAX,PI,CA,DEG,GCAIR,BIGNUM,BIGEXP,EPSILN,        dpla 210
     $     SPZP,SPPP,SPTP,SPDENP,                                       dpla 220
     $     SPSP,SPPPSU,SPTPSU,SPAMTP                                    dpla 230
      DOUBLE PRECISION Z1,Z2,H1,R1,DHMIN,SINAI1,COSAI1,Y1,Y3,X1,SINAI,  dpla 240
     $     COSAI,CPATH,SH,GAMMA,BEND,RATIO1,S,DSDX1,DBNDX1,PA,PB,TA,    dpla 250
     $     TB,RHOA,RHOB,DZ,HP,HRHO,DH,H3,R3,H2,R2,SINAI2,SINAI3,RATIO2, dpla 260
     $     RATIO3,COSAI3,X3,DX,W1,W2,W3,COSAI2,X2,D31,D32,D21,          dpla 270
     $     DSDX2,DSDX3,DS,DBEND,DSDZ,DHHP,DHRH,H3Z1,                    dpla 280
     $     DPRARF,DBNDX2,DBNDX3,RNG                                     dpla 290
      INTEGER N,IMAX,IMOD,IBMAX,IPATH,KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,   dpla 300
     $     IMULT,J,IAMT,K
      LOGICAL LSMALL                                                    dpla 320
C                                                                       dpla 330
C     SSI COMMENTS ON DOUBLE PRECISION VARIABLES:                       dpla 340
C     RFRPTH IS THE OLD COMMON BLOCK IN SINGLE PRECISION.               dpla 350
C     DPRFRP IS THE SAME COMMON BLOCK IN DOUBLE PRECISION; IT IS NEW.   dpla 360
C     IN THIS ROUTINE SP IS USED AS A PREFIX TO DENOTE THE              dpla 370
C     SINGLE PRECISION VARIABLES OF RFRPTH.                             dpla 380
C     THE FOLLOWING ARE THE EXCEPTIONS:                                 dpla 390
C     SPRFN  STANDS FOR THE OLD SINGLE PRECISION RFNDXP                 dpla 400
C     SPPPSU STANDS FOR THE OLD SINGLE PRECISION PPSUM                  dpla 410
C     SPTPSU STANDS FOR THE OLD SINGLE PRECISION TPSUM                  dpla 420
C     SPRHOP STANDS FOR THE OLD SINGLE PRECISION RHOPSM                 dpla 430
C     THE VARIABLES OF THE DOUBLE PRECISION BLOCK DPRFRP HAVE THE       dpla 440
C     SAME NAMES AS THOSE OF THE ORIGINAL SINGLE PRECISION BLOCK;       dpla 450
C     THAT IS, WITHOUT ANY PREFIXES.                                    dpla 460
C                                                                       dpla 470
      COMMON /PARMTR/ RE,DELTAS,ZMAX,IMAX,IMOD,IBMAX,IPATH              dpla 480
      COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     dpla 490
      COMMON /CNTRL/ KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT               dpla 500
      COMMON /RFRPTH/ SPZP(LAYDIM+1),SPPP(LAYDIM+1),SPTP(LAYDIM+1),
     $     SPRFN(LAYDIM+1),SPSP(LAYDIM+1),
     $     SPPPSU(LAYDIM+1),SPTPSU(LAYDIM+1),SPRHOP(LAYDIM+1),
     $     SPDENP(65,LAYDIM+1),SPAMTP(65,LAYDIM+1)
      COMMON /DPRFRP/ ZP(LAYDIM+1),PP(LAYDIM+1),TP(LAYDIM+1),
     $     RFNDXP(LAYDIM+1),SP(LAYDIM+1),PPSUM(LAYDIM+1),
     $     TPSUM(LAYDIM+1),RHOPSM(LAYDIM+1),DENP(65,LAYDIM+1),   
     $     AMTP(65,LAYDIM+1)                                     
      COMMON /SMALL2/LSMALL                                             dpla 570
c                                                                       cfc
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

      dimension  DENAX(MMOLX),DENBX(MMOLX),HDENX(MMOLX)
C
      COMMON /MDATAX/ WMOLXT(MMOLX,laydim)                                  
      COMMON /MODELX/ DNSTYX(MMOLX,LAYDIM)                                  
      COMMON /RFRPTX/ DENPX(MMOLX,laydim+1),AMTPX(MMOLX,laydim+1)
c
c
c
C     DIMENSION HDEN(65),DENA(65),DENB(65)                              dpla 580
      DATA EPSILN/1.0E-5/                                               dpla 590
      data kmax2 /65/
C***  INITIALIZE LOOP                                                   dpla 600
      N = 0                                                             dpla 610
      Z1 = ZP(J)                                                        dpla 620
      Z2 = ZP(J+1)                                                      dpla 630
      H1 = Z1                                                           dpla 640
      R1 = RE+H1                                                        dpla 650
      DHMIN = DELTAS**2/(2.0*R1)                                        dpla 660
      SINAI1 = SINAI                                                    dpla 670
      COSAI1 = COSAI                                                    dpla 680
      Y1 = COSAI1**2/2.0+COSAI1**4/8.0+COSAI1**6*3.0/48.0               dpla 690
      Y3 = 0.0                                                          dpla 700
      X1 = -R1*COSAI1                                                   dpla 710
      RATIO1 = R1/DPRARF(H1,SH,GAMMA)                                   dpla 720
      DSDX1 = 1.0/(1.0-RATIO1*SINAI1**2)                                dpla 730
      DBNDX1 = DSDX1*SINAI1*RATIO1/R1                                   dpla 740
      S = 0.0                                                           dpla 750
      BEND = 0.0                                                        dpla 760
      IF(IAMT.NE.2) THEN                                                dpla 770
C***  INITIALIZE THE VARIABLES FOR THE CALCULATION OF THE               dpla 780
C***  ABSORBER AMOUNTS                                                  dpla 790
      PA = PP(J)                                                        dpla 800
      PB = PP(J+1)                                                      dpla 810
      TA = TP(J)                                                        dpla 820
      TB = TP(J+1)                                                      dpla 830
      RHOA = PA/(GCAIR*TA)                                              dpla 840
      RHOB = PB/(GCAIR*TB)                                              dpla 850
      DZ = ZP(J+1)-ZP(J)                                                dpla 860
      IF (LSMALL .AND. ABS(DZ) .LT. 1E-15) THEN                         dpla 870
      DZ = 1D-20                                                        dpla 880
      HP = 1D-20                                                        dpla 890
      ELSEIF( .NOT. LSMALL .AND. (ABS(PB/PA) .GE. 0.99999)) THEN        dpla 900
         HP = 1D-20                                                     dpla 910
      ELSE                                                              dpla 920
         HP = -DZ/LOG(PB/PA)                                            dpla 930
      ENDIF                                                             dpla 940
      IF(ABS(RHOB/RHOA-1.0).GE.EPSILN)  THEN                            dpla 950
      HRHO = -DZ/LOG(RHOB/RHOA)                                         dpla 960
      ELSE                                                              dpla 970
         HRHO = 1.0E30                                                  dpla 980
      ENDIF                                                             dpla 990
c     DO 105 K=1,KMAX                                                   dpla1000
      DO 105 K=1,KMAX2+NSPECX                                           cfc
         IF ( K .GT. KMAX2) THEN
            KX = K-KMAX2
            DENAX(KX) = DENPX(KX,J) 
            DENBX(KX) = DENPX(KX,J+1)  
            IF(DENAX(KX).LE.0.0 .OR. DENBX(KX).LE.0.0 )THEN
C***           USE LINEAR INTERPOLATION               
               HDENX(KX) = 0.0                          
               GO TO 105
            ENDIF
            IF(ABS(1.0-DENAX(KX)/DENBX(KX)).GT.EPSILN) THEN  
C***           USE EXPONENTIAL INTERPOLATION               
               HDENX(KX) = -DZ/LOG(DENBX(KX)/DENAX(KX))          
            ELSE                                           
C***           USE LINEAR INTERPOLATION                    
               HDENX(KX) = 0.0                               
            ENDIF                                          
            GO TO 105
         ENDIF
c        
         DENA(K) = DENP(K,J)                                            dpla1010
         DENB(K) = DENP(K,J+1)                                          dpla1020
c        IF(DENA(K).GT.0.0 .AND. DENB(K).GT.0.0 .AND.                   dpla1030
c        $           ABS(1.0-DENA(K)/DENB(K)).GT.EPSILN) THEN           dpla1040
c        
         IF(DENA(K).le.0.0 .or. DENB(K).le.0.0 )then                    dpla
C***     USE LINEAR INTERPOLATION                                       dpla
         HDEN(K) = 0.0                                                  dpla
         go to 105
      endif
      if(ABS(1.0-DENA(K)/DENB(K)).GT.EPSILN) THEN                       dpla1040
C***  USE EXPONENTIAL INTERPOLATION                                     dpla1050
      HDEN(K) = -DZ/LOG(DENB(K)/DENA(K))                                dpla1060
      ELSE                                                              dpla1070
C***     USE LINEAR INTERPOLATION                                       dpla1080
         HDEN(K) = 0.0                                                  dpla1090
      ENDIF                                                             dpla1100
 105  CONTINUE                                                          dpla1110
      ENDIF                                                             dpla1120
C***                                                                    dpla1130
C***  LOOP THROUGH PATH                                                 dpla1140
C***  INTEGRATE PATH QUANTITIES USING QUADRATIC INTEGRATION WITH        dpla1150
C***  UNEQUALLY SPACED POINTS                                           dpla1160
C***                                                                    dpla1170
 115  CONTINUE                                                          dpla1180
      N = N+1                                                           dpla1190
      DH = -DELTAS*COSAI1                                               dpla1200
      IF(DH.LT.DHMIN) DH = DHMIN                                        dpla1210
      H3 = H1+DH                                                        dpla1220
      IF(H3.GT.Z2) H3 = Z2                                              dpla1230
      DH = H3-H1                                                        dpla1240
      R3 = RE+H3                                                        dpla1250
      H2 = H1+DH/2.0                                                    dpla1260
      R2 = RE+H2                                                        dpla1270
      SINAI2 = CPATH/(DPANDX(H2,SH,GAMMA)*R2)                           dpla1280
      SINAI3 = CPATH/(DPANDX(H3,SH,GAMMA)*R3)                           dpla1290
      RATIO2 = R2/DPRARF(H2,SH,GAMMA)                                   dpla1300
      RATIO3 = R3/DPRARF(H3,SH,GAMMA)                                   dpla1310
      IF((1.0-SINAI2).LE.EPSILN)  THEN                                  dpla1320
C***  NEAR A TANGENT HEIGHT, COSAI = -SQRT(1-SINAI**2) LOSES            dpla1330
C***  PRECISION.  USE THE FOLLOWING ALGORITHM TO GET COSAI.             dpla1340
      Y3 = Y1+(SINAI1*(1.0-RATIO1)/R1+4.0*SINAI2*(1.0-RATIO2)/R2+       dpla1350
     $     SINAI3*(1.0-RATIO3)/R3)*DH/6.0                               dpla1360
      COSAI3 = -SQRT(2.0*Y3-Y3**2)                                      dpla1370
      X3 = -R3*COSAI3                                                   dpla1380
      DX = X3-X1                                                        dpla1390
      W1 = 0.5*DX                                                       dpla1400
      W2 = 0.0                                                          dpla1410
      W3 = 0.5*DX                                                       dpla1420
      GO TO 118                                                         dpla1430
C***                                                                    dpla1440
      ENDIF                                                             dpla1450
      COSAI2 = -SQRT(1.0-SINAI2**2)                                     dpla1460
      COSAI3 = -SQRT(1.0-SINAI3**2)                                     dpla1470
      X2 = -R2*COSAI2                                                   dpla1480
      X3 = -R3*COSAI3                                                   dpla1490
C***  CALCULATE WEIGHTS                                                 dpla1500
      D31 = X3-X1                                                       dpla1510
      D32 = X3-X2                                                       dpla1520
      D21 = X2-X1                                                       dpla1530
      IF(D32.EQ.0.0 .OR. D21.EQ.0.0) THEN                               dpla1540
      W1 = 0.5*D31                                                      dpla1550
      W2 = 0.0                                                          dpla1560
      W3 = 0.5*D31                                                      dpla1570
      ELSE                                                              dpla1580
         W1 = (2-D32/D21)*D31/6.0                                       dpla1590
         W2 = D31**3/(D32*D21*6.0)                                      dpla1600
         W3 = (2.0-D21/D32)*D31/6.0                                     dpla1610
      ENDIF                                                             dpla1620
C***                                                                    dpla1630
 118  CONTINUE                                                          dpla1640
      DSDX2 = 1.0/(1.0-RATIO2*SINAI2**2)                                dpla1650
      DSDX3 = 1.0/(1.0-RATIO3*SINAI3**2)                                dpla1660
      DBNDX2 = DSDX2*SINAI2*RATIO2/R2                                   dpla1670
      DBNDX3 = DSDX3*SINAI3*RATIO3/R3                                   dpla1680
C***  INTEGRATE                                                         dpla1690
      DS = W1*DSDX1+W2*DSDX2+W3*DSDX3                                   dpla1700
      IF (LSMALL) DS = RNG                                              dpla1710
      S = S+DS                                                          dpla1720
      DBEND = W1*DBNDX1+W2*DBNDX2+W3*DBNDX3                             dpla1730
      IF (LSMALL) DBEND = 0.0                                           dpla1740
      BEND = BEND+DBEND                                                 dpla1750
      IF(IAMT.NE.2)  THEN                                               dpla1760
C***  CALCULATE AMOUNTS                                                 dpla1770
      IF (LSMALL .AND. ABS(DZ) .LT. 1E-15 ) THEN                        dpla1780
      DSDZ = BIGEXP + 1                                                 dpla1790
      DHHP = 0                                                          dpla1800
      ELSE                                                              dpla1810
         DSDZ = DS/DH                                                   dpla1820
         DHHP = DH/HP                                                   dpla1830
      ENDIF                                                             dpla1840
      IF(DHHP .LE. BIGEXP) THEN                                         dpla1850
      PB = PA*EXP(-DHHP )                                               dpla1860
      ELSE                                                              dpla1870
         PB = 0.                                                        dpla1880
      ENDIF                                                             dpla1890
      DHRH = DH/HRHO                                                    dpla1900
      IF(DHRH .LE. BIGEXP) THEN                                         dpla1910
      RHOB = RHOA*EXP(-DHRH   )                                         dpla1920
      ELSE                                                              dpla1930
         RHOB = 0.                                                      dpla1940
      ENDIF                                                             dpla1950
      IF((DH/HRHO).GE.EPSILN)  THEN                                     dpla1960
      PPSUM(J) = PPSUM(J)+                                              dpla1970
     $     DSDZ*(HP/(1.0+HP/HRHO))*(PA*RHOA-PB*RHOB)                    dpla1980
      TPSUM(J) = TPSUM(J)+DSDZ*HP*(PA-PB)/GCAIR                         dpla1990
      RHOPSM(J) = RHOPSM(J)+DSDZ*HRHO*(RHOA-RHOB)                       dpla2000
      SPPPSU(J) = PPSUM(J)                                              dpla2010
      SPTPSU(J) = TPSUM(J)                                              dpla2020
      SPRHOP(J) = RHOPSM(J)                                             dpla2030
      ELSE                                                              dpla2040
         PPSUM(J) = PPSUM(J)+0.5*DS*(PA*RHOA+PB*RHOB)                   dpla2050
         TPSUM(J) = TPSUM(J)+0.5*DS*(PA+PB)/GCAIR                       dpla2060
         RHOPSM(J) = RHOPSM(J)+0.5*DS*(RHOA+RHOB)                       dpla2070
         SPPPSU(J) = PPSUM(J)                                           dpla2080
         SPTPSU(J) = TPSUM(J)                                           dpla2090
         SPRHOP(J) = RHOPSM(J)                                          dpla2100
      ENDIF                                                             dpla2110
      DO 140 K=1,KMAX2+NSPECX 
C        
         IF ( K .GT. KMAX2) THEN
            kx = k -kmax2
C           IF(ABS(HDENX(KX)).NE.0.0 .AND. DH/HDENX(KX).GE.EPSILN) THEN 
            IF(ABS(HDENX(KX)).EQ.0.0 ) THEN                              
C***           LINEAR INTERPOLATION                                    
               DENBX(KX) = DENPX(KX,J)+
     $              (DENPX(KX,J+1)-DENPX(KX,J))*(H3-Z1)/DZ  
               AMTPX(KX,J) = AMTPX(KX,J)+0.5*(DENAX(KX)+DENBX(KX))*DS          
            ELSE IF(DH/HDENX(KX).GE.EPSILN) THEN                        
C***           EXPONENTIAL INTERPOLATION                              
               H3Z1 =  (H3-Z1)/HDENX(KX)                               
               IF(H3Z1 .LE.BIGEXP) THEN                               
                  DENBX(KX) = DENPX(KX,J)*EXP(- H3Z1 )                    
               ELSE                                                   
                  DENBX(KX) = 0.                                        
               ENDIF                                                 
               AMTPX(KX,J) = AMTPX(KX,J)+
     $              DSDZ*HDENX(KX)*(DENAX(KX)-DENBX(KX))   
            ELSE                                                      
C***           LINEAR INTERPOLATION                                  
               DENBX(KX) = DENPX(KX,J)+
     $              (DENPX(KX,J+1)-DENPX(KX,J))*(H3-Z1)/DZ
               AMTPX(KX,J) = AMTPX(KX,J)+0.5*(DENAX(KX)+DENBX(KX))*DS        
            ENDIF  
            GO TO 140
         ENDIF
C
c        IF(ABS(HDEN(K)).NE.0.0 .AND. DH/HDEN(K).GE.EPSILN) THEN        dpla2130
         IF(ABS(HDEN(K)).eq.0.0 ) THEN                                  dpla
C***        LINEAR INTERPOLATION                                        dpla
            DENB(K) = DENP(K,J)+(DENP(K,J+1)-DENP(K,J))*(H3-Z1)/DZ      dpla
            AMTP(K,J) = AMTP(K,J)+0.5*(DENA(K)+DENB(K))*DS              dpla
            SPAMTP(K,J) = AMTP(K,J)                                     dpla
         else IF(DH/HDEN(K).GE.EPSILN) THEN                             dpla
C***        EXPONENTIAL INTERPOLATION                                   dpla2140
            H3Z1 =  (H3-Z1)/HDEN(K)                                     dpla2150
            IF(H3Z1 .LE.BIGEXP) THEN                                    dpla2160
               DENB(K) = DENP(K,J)*EXP(- H3Z1 )                         dpla2170
            ELSE                                                        dpla2180
               DENB(K) = 0.                                             dpla2190
            ENDIF                                                       dpla2200
            AMTP(K,J) = AMTP(K,J)+DSDZ*HDEN(K)*(DENA(K)-DENB(K))        dpla2210
            SPAMTP(K,J) = AMTP(K,J)                                     dpla2220
         ELSE                                                           dpla2230
C***        LINEAR INTERPOLATION                                        dpla2240
            DENB(K) = DENP(K,J)+(DENP(K,J+1)-DENP(K,J))*(H3-Z1)/DZ      dpla2250
            AMTP(K,J) = AMTP(K,J)+0.5*(DENA(K)+DENB(K))*DS              dpla2260
            SPAMTP(K,J) = AMTP(K,J)                                     dpla2270
         ENDIF              
 140     CONTINUE                                                       dpla2290
         PA = PB                                                        dpla2300
         RHOA = RHOB                                                    dpla2310
c        DO 145 K=1,KMAX                                                dpla2320
         DO 145 K=1,KMAX2+NSPECX
            if (k .le. kmax2) then
               DENA(K) = DENB(K)    
            else
               denax(k-kmax2) = denbx(k-kmax2)
            endif
 145     CONTINUE                                                       dpla2340
      ENDIF                                                             dpla2350
      IF(H3.LT.Z2) THEN                                                 dpla2360
         H1 = H3                                                        dpla2370
         R1 = R3                                                        dpla2380
         SINAI1 = SINAI3                                                dpla2390
         RATIO1 = RATIO3                                                dpla2400
         Y1=Y3                                                          dpla2410
         COSAI1 = COSAI3                                                dpla2420
         X1 = X3                                                        dpla2430
         DSDX1 = DSDX3                                                  dpla2440
         DBNDX1 = DBNDX3                                                dpla2450
         IF (.NOT. LSMALL) GO TO 115                                    dpla2460
      ENDIF                                                             dpla2470
      SINAI = SINAI3                                                    dpla2480
      COSAI = COSAI3                                                    dpla2490
      SP(J) = S                                                         dpla2500
      SPSP(J) = S                                                       dpla2510
      RETURN                                                            dpla2520
      END                                                               dpla2530
      DOUBLE PRECISION FUNCTION DPRARF(H,SH,GAMMA)                      dpra 100
C***********************************************************************dpra 110
C     DOUBLE PRECISION (DP) VERSION OF THE ROUTINE PREVIOUSLY CALLED RADdpra 120
C     COMPUTES THE RADIUS OF CURVATURE OF THE REFRACTED RAY FOR         dpra 130
C     HORIZONTAL PATH:  DPRARF = DPANDX/ D(DPANDX)/D(RADIUS)            dpra 140
C     DPANDX = DOUBLE PRECISION (DP) VERSION OF ANDEX                   dpra 150
C***********************************************************************dpra 160
      REAL BIGNUM,BIGEXP,PI,CA,DEG,GCAIR                                dpra 170
      DOUBLE PRECISION H,SH,GAMMA,HSH                                   dpra 180
      COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     dpra 190
      IF(SH.EQ.0.0) GO TO 20                                            dpra 200
      HSH = H/SH                                                        dpra 210
      IF(HSH .GT. BIGEXP) GO TO 20                                      dpra 220
      DPRARF = SH*(1.0+EXP(HSH )/GAMMA)                                 dpra 230
      RETURN                                                            dpra 240
   20 DPRARF = BIGNUM                                                   dpra 250
      RETURN                                                            dpra 260
      END                                                               dpra 270
      SUBROUTINE DPRFPA                                                 dpfp 100
     $     (H1,H2,ANGLE,PHI,LEN,HMIN,IAMT,BETA,RANGE,BENDNG)            dpfp 110
      include 'parameter.list'
C***********************************************************************dpfp 120
C     THIS ROUTINE IS A DOUBLE PRECISION VERSION OF THE                 dpfp 130
C     PREVIOUS ROUTINE "RFPATH".                                        dpfp 140
C                                                                       dpfp 150
C     THIS SUBROUTINE  TRACES THE REFRACTED RAY FROM H1 WITH A          dpfp 160
C     INITIAL ZENITH ANGLE ANGLE TO H2 WHERE THE ZENITH ANGLE IS PHI,   dpfp 170
C     AND CALCULATES THE ABSORBER AMOUNTS (IF IAMT.EQ.1) ALONG          dpfp 180
C     THE PATH.  IT STARTS FROM THE LOWEST POINT ALONG THE PATH         dpfp 190
C     (THE TANGENT HEIGHT HMIN IF LEN = 1 OR HA = MIN(H1,H2) IF LEN = 0)dpfp 200
C     AND PROCEEDS TO THE HIGHEST POINT.  BETA AND RANGE ARE THE        dpfp 210
C     EARTH CENTERED ANGLE AND THE TOTAL DISTANCE RESPECTIVELY          dpfp 220
C     FOR THE REFRACTED PATH FROM H1 TO H2                              dpfp 230
C***********************************************************************dpfp 240
      REAL RE,DELTAS,ZMAX,PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                 dpfp 250
      INTEGER IRD, IPR, IPU,NPR,IPR1,IMAX, IMOD,IBMAX,IPATH,            dpfp 260
     $     IHIGH,JA,KA,JD,J2A,J2B,ISSGEO,JO,JL,LEN,                     dpfp 270
     $     IAMT,ITEST,JTURN,IORDER,MAX,I,JNEXT,JHA,JMAX,                dpfp 280
     $     J2,J,IHLOW,J1, KMAX,M,IKMAX,NL,ML,IKLO,IMULT                 dpfp 290
      logical lsmall, lprint                                            dpfp 300
      DOUBLE PRECISION ZP,PP,TP,RFNDXP,                                 dpfp 310
     $     SP,PPSUM,TPSUM,RHOPSM,DENP,AMTP                              dpfp 320
      double precision Dpandx,H1,H2,ANGLE,PHI,HMIN,BETA,RANGE,BENDNG,   dpfp 330
     $     HA,HB,SH,GAMMA,CPATH,S,SINAI,THETA,DS,                       dpfp 340
     $     DBEND,DBETA,COSAI,ANGLEA,RHOBAR,TBAR,PBAR,DPRNG2,dhalfr,     dpfp 350
     $     savcos,savsin,rng,smh1, smh2,trange                          dpfp 360
C                                                                       dpfp 370
C     SSI COMMENTS ON DOUBLE PRECISION VARIABLES:                       dpfp 380
C     RFRPTH IS THE OLD COMMON BLOCK IN SINGLE PRECISION.               dpfp 390
C     DPRFRP IS THE SAME COMMON BLOCK IN DOUBLE PRECISION; IT IS NEW.   dpfp 400
C     IN THIS ROUTINE SP IS USED AS A PREFIX TO DENOTE THE              dpfp 410
C     SINGLE PRECISION VARIABLES OF RFRPTH.                             dpfp 420
C     THE FOLLOWING ARE THE EXCEPTIONS:                                 dpfp 430
C     SPRFN  STANDS FOR THE OLD SINGLE PRECISION RFNDXP                 dpfp 440
C     SPPPSU STANDS FOR THE OLD SINGLE PRECISION PPSUM                  dpfp 450
C     SPTPSU STANDS FOR THE OLD SINGLE PRECISION TPSUM                  dpfp 460
C     SPRHOP STANDS FOR THE OLD SINGLE PRECISION RHOPSM                 dpfp 470
C     THE VARIABLES OF THE DOUBLE PRECISION BLOCK DPRFRP HAVE THE       dpfp 480
C     SAME NAMES AS THOSE OF THE ORIGINAL SINGLE PRECISION BLOCK;       dpfp 490
C     THAT IS, WITHOUT ANY PREFIXES.                                    dpfp 500
C                                                                       dpfp 510
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
C     COMMON /CARD1/ MODEL,ITYPE,IEMSCT,M1,M2,M3,IM,NOPRNT,TBOUND,SALB,Mdpfp 530
      LOGICAL MODTRN                                                    dpfp 540
      COMMON /PARMTR/ RE,DELTAS,ZMAX,IMAX,IMOD,IBMAX,IPATH              dpfp 550
      COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     dpfp 560
      COMMON /RFRPTH/ SPZP(laydim+1),SPPP(laydim+1),SPTP(laydim+1),
     $     SPRFN(laydim+1),SPSP(laydim+1),
     $     SPPPSU(laydim+1),SPTPSU(laydim+1),SPRHOP(laydim+1),
     $     SPDENP(65,laydim+1),SPAMTP(65,laydim+1)
      COMMON /DPRFRP/ ZP(LAYDIM+1),PP(LAYDIM+1),TP(LAYDIM+1),
     $     RFNDXP(LAYDIM+1),SP(LAYDIM+1),PPSUM(LAYDIM+1),
     $     TPSUM(LAYDIM+1),RHOPSM(LAYDIM+1),DENP(65,LAYDIM+1),   
     $     AMTP(65,LAYDIM+1)                                     
      COMMON/SOLS/AH1(LAYTWO),ARH(LAYTWO),WPATHS(LAYTHR,65),
     1 PA(LAYTWO),PR(LAYTWO),ATHETA(LAYDIM+1),ADBETA(LAYDIM+1),
     2 LJ(LAYTWO+1),JTURN,ANGSUN,CSZEN(LAYTWO),TBBYS(LAYTHR,12),
     3 PATMS(LAYTHR,12)
      COMMON /CNTRL/ KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT               dpfp 680
      COMMON /PATH/PL(LAYTWO),QTHETA(LAYTWO),ITEST,HI,HF,
     1  AHT(LAYTWO),tph(LAYTWO)
      common /small1/dhalfr,dprng2                                      dpfp 700
      common /small2/lsmall                                             dpfp 710
      common /small3/small                                              dpfp 720
      common /small4/smh1, smh2, trange                                 dpfp 730
      common /cprint/lprint                                             dpfp 740
      DIMENSION HLOW(2)                                                 dpfp 750
      CHARACTER*2 HLOW                                                  dpfp 760
      DATA HLOW/'H1','H2'/                                              dpfp 770
      if (lsmall) then                                                  dpfp 780
c        if (lsmall) set h1 and h2 equal to double precision            dpfp 790
c        values saved via common statements.                            dpfp 800
         h1 = smh1                                                      dpfp 810
         h2 = smh2                                                      dpfp 820
      endif                                                             dpfp 830
      DO 9 I = 1, 35                                                    dpfp 840
         ZP(I) = SPZP(I)                                                dpfp 850
         RFNDXP(I) = SPRFN(I)                                           dpfp 860
         PPSUM(I) = SPPPSU(I)                                           dpfp 870
         TPSUM(I) = SPTPSU(I)                                           dpfp 880
         RHOPSM(I) = SPRHOP(I)                                          dpfp 890
 9    CONTINUE                                                          dpfp 900
      MAX = 0                                                           dpfp 910
      IF(H1.LE.H2) THEN                                                 dpfp 920
         IORDER = 1                                                     dpfp 930
         HA = H1                                                        dpfp 940
         HB = H2                                                        dpfp 950
         ANGLEA = ANGLE                                                 dpfp 960
      ELSE                                                              dpfp 970
         IORDER = -1                                                    dpfp 980
         HA = H2                                                        dpfp 990
         HB = H1                                                        dpfp1000
         ANGLEA = PHI                                                   dpfp1010
      ENDIF                                                             dpfp1020
      if (lsmall) then                                                  dpfp1030
         savsin =  SIN(ANGLEA/DEG)                                      dpfp1050
         savcos = -cos(ANGLEA/DEG)                                      dpfp1050
         sinai = sin(anglea/deg)
         cosai = -cos(anglea/deg)
      endif                                                             dpfp1060
      JNEXT = 1                                                         dpfp1070
C     IF(IAMT.EQ.1 .AND. NPR.NE.1 .and. lprint)  WRITE(IPR,20)          dpfp1080
      IF(IAMT.EQ.1 .AND. NPR.LT.1 .and. lprint)  WRITE(IPR,20)          dpfp1090
 20   FORMAT('1CALCULATION OF THE REFRACTED PATH THROUGH THE ',         dpfp1100
     $     'ATMOSPHERE',///,                                            dpfp1110
     $     T3,'I',T11,'ALTITUDE',T27,'THETA',T34,'DRANGE',T44,'RANGE',  dpfp1120
     $     T53,'DBETA',T63,'BETA',T72,'PHI',T80,'DBEND',T87,'BENDING',  dpfp1130
     $     T 98,'PBAR',T106,'TBAR',T113,'RHOBAR',/,                     dpfp1140
     $     T07,'FROM',T18,'TO',/,T07,'(KM)',T17,'(KM)',T27,'(DEG)',     dpfp1150
     $     T36,'(KM)',T45,'(KM)',T53,'(DEG)',T62,'(DEG)',T71,'(DEG)',   dpfp1160
     $     T80,'(DEG)',T89,'(DEG)',T098,'(MB)',T106,'(K)',              dpfp1170
     $     T111,'(GM CM-3)',/)                                          dpfp1180
      IF(LEN.EQ.1) THEN                                                 dpfp1190
C***                                                                    dpfp1200
C        LONG PATH:  FILL IN THE SYMMETRIC PART FROM THE TANGENT HEIGHT dpfp1210
C***     TO HA                                                          dpfp1220
         CALL DPFILL(HMIN,HA,JNEXT)                                     dpfp1230
         JHA = JNEXT                                                    dpfp1240
      ENDIF                                                             dpfp1250
C***  IF LEN = 0, OR IF LEN = 1 TO FILL IN THE REMAINING PATH FROM HA TOdpfp1260
      IF(HA.NE.HB)  CALL DPFILL(HA,HB,JNEXT)                            dpfp1270
      JMAX = JNEXT                                                      dpfp1280
      IPATH = JMAX                                                      dpfp1290
C***  INTEGRATE EACH SEGMENT OF THE PATH                                dpfp1300
C***  CALCULATE CPATH SEPERATELY FOR LEN = 0,1                          dpfp1310
      IF(LEN.EQ.0)  THEN                                                dpfp1320
         CALL DPFISH(HA,SH,GAMMA)                                       dpfp1330
         CPATH = (RE+HA)*dpandx(HA,SH,GAMMA)*SIN(ANGLEA/DEG)            dpfp1340
      ELSE IF (LEN .EQ. 1) THEN                                         dpfp1350
         CALL DPFISH(HMIN,SH,GAMMA)                                     dpfp1360
         CPATH = (RE+HMIN)*Dpandx(HMIN,SH,GAMMA)                        dpfp1370
      ENDIF                                                             dpfp1380
      BETA = 0.0                                                        dpfp1390
      S = 0.0                                                           dpfp1400
      BENDNG = 0.0                                                      dpfp1410
      IF(LEN.EQ.0) THEN                                                 dpfp1420
C***     SHORT PATH                                                     dpfp1430
         JNEXT = 1                                                      dpfp1440
C***     ANGLEA IS THE ZENITH ANGLE AT HA IN DEG                        dpfp1450
C***     SINAI IS SIN OF THE INCIDENCE ANGLE                            dpfp1460
C***     COSAI IS CARRIED SEPERATELY TO AVOID A PRECISION PROBLEM       dpfp1470
C***     WHEN SINAI IS CLOSE TO 1.0                                     dpfp1480
         THETA = ANGLEA                                                 dpfp1490
         SINAI = COS((90.0-ANGLEA)/DEG)                                 dpfp1500
         COSAI = -SIN((90.0-ANGLEA)/DEG)                                dpfp1510
         if(SINAI.gt.  1.)SINAI =  1.
         if(SINAI.lt. -1.)SINAI = -1.
      ELSE                                                              dpfp1520
C***     DO SYMMETRIC PART,FROM TANGENT HEIGHT(HMIN) TO HA              dpfp1530
         IHLOW = 1                                                      dpfp1540
         IF(IORDER.EQ.-1)  IHLOW = 2                                    dpfp1550
c        IF(IAMT.EQ.1 .AND. NPR.NE.1 .and. lprint)                      dpfp1560
c     $       WRITE(IPR,24) HLOW(IHLOW)                                 dpfp1570
         IF(IAMT.EQ.1 .AND. NPR.LT.1 .and. lprint)                      dpfp1580
     $        WRITE(IPR,24) HLOW(IHLOW)                                 dpfp1590
 24      FORMAT(' ',T7,'TANGENT',T17,A2,/,T7,'HEIGHT',/)                dpfp1600
         SINAI = 1.0                                                    dpfp1610
         COSAI = 0.0                                                    dpfp1620
         THETA = 90.0                                                   dpfp1630
         J2 = JHA-1                                                     dpfp1640
         rng = dhalfr/j2                                                dpfp1650
         DO 120 J=1,J2                                                  dpfp1660
            CALL DPSCHT(ZP(J),ZP(J+1),RFNDXP(J),RFNDXP(J+1),SH,GAMMA)   dpfp1670
            CALL DPlayr(J,SINAI,COSAI,CPATH,SH,GAMMA,IAMT,DS,DBEND,     dpfp1680
     $           rng)                                                   dpfp1690
            if (lsmall) then                                            dpfp1700
               sinai = savsin                                           dpfp1710
               cosai = savcos                                           dpfp1720
            endif                                                       dpfp1730
            if(SINAI.gt.  1.)SINAI =  1.
            if(SINAI.lt. -1.)SINAI = -1.
            DBEND = DBEND*DEG                                           dpfp1740
            PHI = ASIN(SINAI)*DEG                                       dpfp1750
            DBETA = THETA-PHI+DBEND                                     dpfp1760
            if (lsmall) dbeta = (deg*2*savsin*ds)/(zp(j)+zp(j+1)+2*re)  dpfp1770
            PHI = 180.0-PHI                                             dpfp1780
            S = S+DS                                                    dpfp1790
C                                                                       dpfp1800
C           SAVE REFRACTED RAY PATH LENGTH FOR USE WITH MULTIPLE SCATTERdpfp1810
C                                                                       dpfp1820
            BENDNG = BENDNG+DBEND                                       dpfp1830
            BETA = BETA+DBETA                                           dpfp1840
            IF(IAMT.EQ.1) THEN                                          dpfp1850
               PBAR = PPSUM(J)/RHOPSM(J)                                dpfp1860
               TBAR = TPSUM(J)/RHOPSM(J)                                dpfp1870
               RHOBAR = RHOPSM(J)/DS                                    dpfp1880
c              IF(IAMT.EQ.1 .AND. NPR.NE.1 .and. lprint)                dpfp1890
c    $              WRITE(IPR,22) J,ZP(J),ZP(J+1)                       dpfp1900
               IF(IAMT.EQ.1 .AND. NPR.LT.1 .and. lprint)                dpfp1910
     $              WRITE(IPR,22)                                       dpfp1920
     $              J,ZP(J),ZP(J+1),THETA,DS,S,                         dpfp1930
     $              DBETA,BETA,PHI,DBEND,BENDNG,PBAR,TBAR,RHOBAR        dpfp1940
 22            FORMAT(' ',I2,2F9.3,9F9.3,F8.2,1PE9.2)                   dpfp1950
               J2A = J2 - J +  1                                        dpfp1960
               J2B = J2 + J                                             dpfp1970
CC             QTHETA(J2A)=  PHI  *CA                                   dpfp1980
CC             QTHETA(J2B) = THETA*CA                                   dpfp1990
               QTHETA(J2A)=cos(PHI*CA)                                  dpfp2000
               QTHETA(J2B)=cos(THETA*CA)                                dpfp2010
               MAX = J2B                                                dpfp2020
            ENDIF                                                       dpfp2030
            IF(ISSGEO.NE.1) THEN                                        dpfp2040
               ATHETA(J)=THETA                                          dpfp2050
               ADBETA(J)=DBETA                                          dpfp2060
            ENDIF                                                       dpfp2070
            THETA = 180.0-PHI                                           dpfp2080
 120     CONTINUE                                                       dpfp2090
C***     DOUBLE PATH QUANTITIES FOR THE OTHER PART OF THE SYMMETRIC PATHdpfp2100
         BENDNG = 2.0*BENDNG                                            dpfp2110
         BETA = 2.0*BETA                                                dpfp2120
         S = 2.0*S                                                      dpfp2130
c        IF(IAMT.EQ.1 .AND. NPR.NE.1 .and. lprint)                      dpfp2140
c    $        WRITE(IPR,26) S,BETA,BENDNG                               dpfp2150
         IF(IAMT.EQ.1 .AND. NPR.LT.1 .and. lprint)                      dpfp2160
     $        WRITE(IPR,26) S,BETA,BENDNG                               dpfp2170
 26      FORMAT('0',T10,'DOUBLE RANGE, BETA, BENDING',/,                dpfp2180
     $        T10,'FOR SYMMETRIC PART OF PATH',T40,F9.3,T58,F9.3,       dpfp2190
     $        T85,F9.3,/)                                               dpfp2200
         JNEXT = JHA                                                    dpfp2210
      ENDIF                                                             dpfp2220
C***  DO PATH FROM HA TO HB                                             dpfp2230
      IF(HA.NE.HB) THEN                                                 dpfp2240
         JO = MAX - JNEXT + 1                                           dpfp2250
         J1 = JNEXT                                                     dpfp2260
         J2 = JMAX-1                                                    dpfp2270
C                                                                       dpfp2280
         IF(H1 .GT. H2) THEN                                            dpfp2290
            JD =  J2 - J1 + 1                                           dpfp2300
            DO  1158 J = 1,MAX                                          dpfp2310
               KA = MAX - J + 1                                         dpfp2320
               JA = KA + JD                                             dpfp2330
               QTHETA(JA) = QTHETA(KA)                                  dpfp2340
 1158       CONTINUE                                                    dpfp2350
            MAX =  MAX + JD                                             dpfp2360
            JO = 0                                                      dpfp2370
         ENDIF                                                          dpfp2380
C                                                                       dpfp2390
         IHLOW = 1                                                      dpfp2400
         IF(IORDER.EQ.-1)  IHLOW = 2                                    dpfp2410
         IHIGH = MOD(IHLOW,2)+1                                         dpfp2420
C        IF(IAMT.EQ.1 .AND. NPR.NE.1 .and. lprint)                      dpfp2430
c    $        WRITE(IPR,28) HLOW(IHLOW),                                dpfp2440
         IF(IAMT.EQ.1 .AND. NPR.LT.1 .and. lprint)                      dpfp2450
     $        WRITE(IPR,28) HLOW(IHLOW),                                dpfp2460
     $        HLOW(IHIGH)                                               dpfp2470
 28      FORMAT(' ',T11,A2,' TO ',A2,/)                                 dpfp2480
c        rng = dprng2/(j2-j1+1)                                         dpfp2490
         DO 160 J=J1,J2                                                 dpfp2500
            CALL DPSCHT(ZP(J),ZP(J+1),RFNDXP(J),RFNDXP(J+1),SH,GAMMA)   dpfp2510
            rng = dprng2*((zp(j+1)-zp(j))/(zp(j2+1)-zp(j1)))            dpfp2520
            CALL DPlayr(J,SINAI,COSAI,CPATH,SH,GAMMA,IAMT,DS,DBEND,     dpfp2530
     $           rng)                                                   dpfp2540
            if (lsmall) then                                            dpfp2550
               sinai = savsin                                           dpfp2560
               cosai = savcos                                           dpfp2570
            endif                                                       dpfp2580
            if(SINAI.gt.  1.)SINAI =  1.
            if(SINAI.lt. -1.)SINAI = -1.
            DBEND = DBEND*DEG                                           dpfp2590
            PHI = ASIN(SINAI)*DEG                                       dpfp2600
            DBETA = THETA-PHI+DBEND                                     dpfp2610
            if (lsmall) dbeta = (deg*2*savsin*ds)/(zp(j)+zp(j+1)+2*re)  dpfp2620
            PHI = 180.0-PHI                                             dpfp2630
            S = S+DS                                                    dpfp2640
C                                                                       dpfp2650
C           SAVE LAYER REFRACTED PATH LENGTH FOR USE WITH MULTIPLE SCATTdpfp2660
C                                                                       dpfp2670
            PL(J)=DS                                                    dpfp2680
            BENDNG = BENDNG+DBEND                                       dpfp2690
            BETA = BETA+DBETA                                           dpfp2700
            IF(IAMT.EQ.1) THEN                                          dpfp2710
               PBAR = PPSUM(J)/RHOPSM(J)                                dpfp2720
               TBAR = TPSUM(J)/RHOPSM(J)                                dpfp2730
               RHOBAR = RHOPSM(J)/DS                                    dpfp2740
c              IF(IAMT.EQ.1 .AND. NPR.NE.1 .and. lprint)                dpfp2750
c    $              WRITE(IPR,22) J,ZP(J),ZP(J+1),                      dpfp2760
               IF(IAMT.EQ.1 .AND. NPR.LT.1 .and. lprint)                dpfp2770
     $              WRITE(IPR,22)                                       dpfp2780
     $              J,ZP(J),ZP(J+1),THETA,DS,S,                         dpfp2790
     $              DBETA,BETA,PHI,DBEND,BENDNG,PBAR,TBAR,RHOBAR        dpfp2800
            ENDIF                                                       dpfp2810
C                                                                       dpfp2820
C           SAVE LAYER REFRACTED PATH ANGLE FOR USE WITH MULTIPLE SCATERdpfp2830
C                                                                       dpfp2840
C                                                                       dpfp2850
            IF(H2 .GT. H1)  THEN                                        dpfp2860
CC            QTHETA(J + JO)=THETA*CA                                   dpfp2870
              QTHETA(J + JO)=COS(THETA*CA)                              dpfp2880
               MAX  = J + JO                                            dpfp2890
            ELSE                                                        dpfp2900
               J2B  = J2 - J + 1                                        dpfp2910
CC             QTHETA(J2B)= PHI * CA                                    dpfp2920
               QTHETA(J2B)= COS(PHI * CA)                               dpfp2930
            ENDIF                                                       dpfp2940
C                                                                       dpfp2950
            IF(ISSGEO.NE.1) THEN                                        dpfp2960
               ADBETA(J)=DBETA                                          dpfp2970
               ATHETA(J)=THETA                                          dpfp2980
            ENDIF                                                       dpfp2990
            THETA = 180.0-PHI                                           dpfp3000
 160     CONTINUE                                                       dpfp3010
      ENDIF                                                             dpfp3020
      IF(ISSGEO.EQ.0) ATHETA(JMAX)=THETA                                dpfp3030
      IF(IORDER.EQ.-1) PHI = ANGLEA                                     dpfp3040
      RANGE = S                                                         dpfp3050
      DO 1 I = 1, 35                                                    dpfp3060
         SPZP(I) = ZP(I)                                                dpfp3070
         SPRFN(I) = RFNDXP(I)                                           dpfp3080
         SPPPSU(I) = PPSUM(I)                                           dpfp3090
         SPTPSU(I) = TPSUM(I)                                           dpfp3100
         SPRHOP(I) = RHOPSM(I)                                          dpfp3110
 1    CONTINUE                                                          dpfp3120
      RETURN                                                            dpfp3130
      END                                                               dpfp3140
      SUBROUTINE DPSCHT(Z1,Z2,RFNDX1,RFNDX2,SH,GAMMA)                   dpsc 100
C***********************************************************************dpsc 110
C     THE DOUBLE PRECISION VERSION OF THE PREVIOUS ROUTINE SCALHT.      dpsc 120
C                                                                       dpsc 130
C     THIS SUBROUTINE CALCULATES THE SCALE HEIGHT SH OF THE (INDEX OF   dpsc 140
C     REFRACTION-1.0) FROM THE VALUES OF THE INDEX AT THE ALTITUDES Z1  dpsc 150
C     AND Z2 ( Z1 < Z2). IT ALSO CALCULATES THE EXTRAPOLATED VALUE      dpsc 160
C     GAMMA OF THE (INDEX-1.0) AT Z = 0.0                               dpsc 170
C***********************************************************************dpsc 180
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               dpsc 190
      RF1 = RFNDX1+1.0E-20                                              dpsc 200
      RF2 = RFNDX2+1.0E-20                                              dpsc 210
      RATIO = RF1/RF2                                                   dpsc 220
      IF(ABS(RATIO-1.0).LT.1.0E-05)  GO TO 100                          dpsc 230
C     SH = (Z2-Z1)/ALOG(RATIO)                                          dpsc 240
      SH = (Z2-Z1)/LOG(RATIO)                                           dpsc 250
      GAMMA = RF1*(RF2/RF1)**(-Z1/(Z2-Z1))                              dpsc 260
      GO TO 110                                                         dpsc 270
 100  CONTINUE                                                          dpsc 280
C*****THEVARIATION IN THE INDEX OF REFRACTION WITH HEIGHT IS            dpsc 290
C*****INSIGNIFICANTOR ZERO                                              dpsc 300
      SH = 0.0                                                          dpsc 310
      GAMMA = RFNDX1                                                    dpsc 320
 110  CONTINUE                                                          dpsc 330
      RETURN                                                            dpsc 340
      END                                                               dpsc 350
      BLOCK DATA DSTDTA                                                 dsta 100
C>    BLOCK DATA                                                        dsta 110
C********************************************************************** dsta 120
C*                                                                    * dsta 130
C*    DESERT AEROSOL EXTINCTION COEFFICIENTS, ABSORPTION COEFFICIENTS * dsta 140
C*    AND ASYMMETRY PARAMETERS FOR FOUR WIND SPEEDS: 0 M/S, 10 M/S,   * dsta 150
C*    20 M/S AND 30 M/S                                               * dsta 160
C*                                                                    * dsta 170
C*    PROGRAMMED BY:  D. R. LONGTIN         OPTIMETRICS, INC.         * dsta 180
C*                                          BURLINGTON, MASSACHUSETTS * dsta 190
C*                                          FEB  1988                 * dsta 200
C*                                                                    * dsta 210
C********************************************************************** dsta 220
C                                                                       dsta 230
      COMMON /DESAER/DESEX1(47),DESEX2(47),DESEX3(47),DESEX4(47),       dsta 240
     *DESAB1(47),DESAB2(47),DESAB3(47),DESAB4(47),DESG1(47),DESG2(47),  dsta 250
     *DESG3(47),DESG4(47)                                               dsta 260
C                                                                       dsta 270
C     EXTINCTION COEFFICIENTS                                           dsta 280
C                                                                       dsta 290
      DATA DESEX1 /                                                     dsta 300
     1 8.7330E-2, 7.1336E-2, 6.5754E-2, 4.0080E-2, 2.8958E-2, 1.4537E-2,dsta 310
     1 7.1554E-3, 4.3472E-3, 3.5465E-3, 2.9225E-3, 2.5676E-3, 4.3573E-3,dsta 320
     1 5.7479E-3, 2.9073E-3, 2.0109E-3, 1.8890E-3, 1.8525E-3, 1.8915E-3,dsta 330
     1 1.9503E-3, 2.3256E-3, 4.9536E-3, 2.0526E-3, 2.6738E-3, 9.2804E-3,dsta 340
     1 1.5352E-2, 6.9396E-3, 2.2455E-3, 1.9840E-3, 1.9452E-3, 1.9019E-3,dsta 350
     1 1.8551E-3, 1.9661E-3, 1.9865E-3, 2.4089E-3, 1.7485E-3, 1.4764E-3,dsta 360
     1 2.2604E-3, 2.1536E-3, 2.3008E-3, 2.9272E-3, 2.6943E-3, 2.4319E-3,dsta 370
     1 1.9199E-3, 1.4887E-3, 8.0630E-4, 4.6950E-4, 2.0792E-4/           dsta 380
      DATA DESEX2 /                                                     dsta 390
     2 1.0419E-1, 8.8261E-2, 8.2699E-2, 5.7144E-2, 4.6078E-2, 3.1831E-2,dsta 400
     2 2.4638E-2, 2.1952E-2, 2.1254E-2, 2.0743E-2, 2.0397E-2, 2.2340E-2,dsta 410
     2 2.3848E-2, 2.1104E-2, 2.0422E-2, 2.0462E-2, 2.0591E-2, 2.0843E-2,dsta 420
     2 2.1030E-2, 2.1630E-2, 2.2880E-2, 1.9075E-2, 2.0928E-2, 2.9835E-2,dsta 430
     2 3.8025E-2, 2.7349E-2, 2.1502E-2, 2.1475E-2, 2.1563E-2, 2.1726E-2,dsta 440
     2 2.2265E-2, 2.2580E-2, 2.2708E-2, 2.1705E-2, 2.1230E-2, 2.0523E-2,dsta 450
     2 2.6686E-2, 2.5461E-2, 2.3785E-2, 2.6033E-2, 2.6484E-2, 2.6464E-2,dsta 460
     2 2.5318E-2, 2.3341E-2, 1.7824E-2, 1.3092E-2, 7.2020E-3/           dsta 470
      DATA DESEX3 /                                                     dsta 480
     3 2.7337E-1, 2.5795E-1, 2.5252E-1, 2.2773E-1, 2.1710E-1, 2.0402E-1,dsta 490
     3 1.9809E-1, 1.9664E-1, 1.9635E-1, 1.9655E-1, 1.9661E-1, 1.9907E-1,dsta 500
     3 2.0164E-1, 1.9957E-1, 2.0013E-1, 2.0142E-1, 2.0270E-1, 2.0400E-1,dsta 510
     3 2.0501E-1, 2.0665E-1, 2.0573E-1, 1.9165E-1, 2.0121E-1, 2.2402E-1,dsta 520
     3 2.4718E-1, 2.2503E-1, 2.0749E-1, 2.0910E-1, 2.0999E-1, 2.1165E-1,dsta 530
     3 2.1784E-1, 2.1727E-1, 2.1803E-1, 2.0995E-1, 2.1214E-1, 2.1308E-1,dsta 540
     3 2.5226E-1, 2.4234E-1, 2.2638E-1, 2.3991E-1, 2.4680E-1, 2.5176E-1,dsta 550
     3 2.5655E-1, 2.5505E-1, 2.3610E-1, 2.1047E-1, 1.5938E-1/           dsta 560
      DATA DESEX4 /                                                     dsta 570
     4 1.9841E0, 1.9721E0, 1.9676E0, 1.9488E0, 1.9424E0, 1.9377E0,      dsta 580
     4 1.9374E0, 1.9484E0, 1.9509E0, 1.9549E0, 1.9570E0, 1.9642E0,      dsta 590
     4 1.9737E0, 1.9764E0, 1.9860E0, 1.9944E0, 2.0020E0, 2.0113E0,      dsta 600
     4 2.0148E0, 2.0245E0, 2.0283E0, 1.9397E0, 1.9973E0, 2.1039E0,      dsta 610
     4 2.2246E0, 2.1587E0, 2.0409E0, 2.0520E0, 2.0613E0, 2.0651E0,      dsta 620
     4 2.1194E0, 2.1065E0, 2.1104E0, 2.0651E0, 2.0926E0, 2.1155E0,      dsta 630
     4 2.3696E0, 2.2931E0, 2.1828E0, 2.2708E0, 2.3304E0, 2.3762E0,      dsta 640
     4 2.4533E0, 2.4915E0, 2.5118E0, 2.4463E0, 2.2122E0/                dsta 650
C                                                                       dsta 660
C     ABSORPTION COEFFICIENTS                                           dsta 670
C                                                                       dsta 680
      DATA DESAB1 /                                                     dsta 690
     1 6.4942E-4, 6.1415E-4, 5.8584E-4, 4.4211E-4, 1.3415E-4, 7.8142E-5,dsta 700
     1 5.7566E-5, 8.3848E-5, 7.6988E-5, 4.4486E-5, 8.9604E-5, 2.4887E-3,dsta 710
     1 3.3444E-3, 6.8781E-4, 1.6387E-4, 3.5236E-4, 3.5340E-4, 4.0930E-4,dsta 720
     1 5.0526E-4, 8.2146E-4, 3.7647E-3, 1.0162E-3, 1.3525E-3, 7.7761E-3,dsta 730
     1 1.3108E-2, 5.1252E-3, 1.0973E-3, 6.8573E-4, 5.7622E-4, 5.1268E-4,dsta 740
     1 7.6834E-4, 5.3793E-4, 5.0611E-4, 1.2828E-3, 6.7827E-4, 4.3826E-4,dsta 750
     1 5.1221E-4, 8.8642E-4, 9.5535E-4, 1.0000E-3, 7.5646E-4, 6.1552E-4,dsta 760
     1 4.6087E-4, 3.5642E-4, 2.3556E-4, 1.7596E-4, 1.1699E-4/           dsta 770
      DATA DESAB2 /                                                     dsta 780
     2 4.3569E-3, 4.3413E-3, 4.3277E-3, 4.0649E-3, 3.9091E-4, 8.4594E-5,dsta 790
     2 5.8501E-5, 8.4412E-5, 7.7547E-5, 4.6817E-5, 9.2721E-5, 2.5389E-3,dsta 800
     2 3.3588E-3, 7.9414E-4, 8.5079E-4, 4.6002E-3, 4.4872E-3, 4.6200E-3,dsta 810
     2 5.2973E-3, 4.8910E-3, 8.9899E-3, 5.4745E-3, 3.6375E-3, 1.1862E-2,dsta 820
     2 1.5179E-2, 7.0015E-3, 8.4693E-3, 6.9516E-3, 6.3008E-3, 6.3684E-3,dsta 830
     2 8.4992E-3, 6.9625E-3, 6.5192E-3, 7.8955E-3, 7.7192E-3, 5.8540E-3,dsta 840
     2 5.3263E-3, 9.3004E-3, 7.4848E-3, 3.0952E-3, 1.8219E-3, 1.3078E-3,dsta 850
     2 1.0653E-3, 5.5231E-4, 3.2311E-4, 2.2422E-4, 1.3839E-4/           dsta 860
      DATA DESAB3 /                                                     dsta 870
     3 4.1552E-2, 4.1671E-2, 4.1781E-2, 4.1125E-2, 5.0552E-3, 2.1085E-4,dsta 880
     3 7.5703E-5, 9.5531E-5, 8.8354E-5, 9.0588E-5, 1.5058E-4, 3.4972E-3,dsta 890
     3 3.6310E-3, 2.6709E-3, 1.2558E-2, 5.9184E-2, 5.8289E-2, 5.9206E-2,dsta 900
     3 6.5487E-2, 5.8707E-2, 7.4669E-2, 5.2152E-2, 2.5783E-2, 4.7971E-2,dsta 910
     3 3.2378E-2, 2.4739E-2, 8.1225E-2, 7.5085E-2, 7.1232E-2, 7.3042E-2,dsta 920
     3 8.0638E-2, 7.8255E-2, 7.4882E-2, 7.8853E-2, 8.1412E-2, 6.5722E-2,dsta 930
     3 4.8565E-2, 8.4983E-2, 7.1273E-2, 3.0870E-2, 1.7031E-2, 1.1455E-2,dsta 940
     3 1.0554E-2, 4.0418E-3, 2.1509E-3, 1.4115E-3, 7.9698E-4/           dsta 950
      DATA DESAB4 /                                                     dsta 960
     4 4.1777E-1, 4.1880E-1, 4.2000E-1, 4.1846E-1, 8.6452E-2, 2.6538E-3,dsta 970
     4 4.0804E-4, 3.1418E-4, 2.9996E-4, 9.3018E-4, 1.2814E-3, 2.1436E-2,dsta 980
     4 8.7553E-3, 3.7670E-2, 2.0849E-1, 7.0914E-1, 7.0420E-1, 7.1379E-1,dsta 990
     4 7.6309E-1, 7.1128E-1, 8.2992E-1, 5.3585E-1, 2.4456E-1, 3.8103E-1,dsta1000
     4 1.7784E-1, 1.9305E-1, 7.9910E-1, 7.8987E-1, 7.7502E-1, 7.9400E-1,dsta1010
     4 7.6332E-1, 8.3629E-1, 8.1581E-1, 8.3122E-1, 8.4901E-1, 7.0150E-1,dsta1020
     4 4.4205E-1, 7.7354E-1, 7.1088E-1, 3.9328E-1, 2.3337E-1, 1.6258E-1,dsta1030
     4 1.5289E-1, 5.8849E-2, 3.5576E-2, 2.4463E-2, 1.4525E-2/           dsta1040
C                                                                       dsta1050
C     ASYMMETRY PARAMETER                                               dsta1060
C                                                                       dsta1070
      DATA DESG1 /                                                      dsta1080
     1 0.6603, 0.6581, 0.6547, 0.6383, 0.6276, 0.5997, 0.5829, 0.5873,  dsta1090
     1 0.5967, 0.6130, 0.6323, 0.6850, 0.6068, 0.6312, 0.6816, 0.7298,  dsta1100
     1 0.7574, 0.7874, 0.8124, 0.8424, 0.8301, 0.8107, 0.6143, 0.6167,  dsta1110
     1 0.4892, 0.4917, 0.6662, 0.6334, 0.6298, 0.6498, 0.7470, 0.6711,  dsta1120
     1 0.6751, 0.7538, 0.8054, 0.7797, 0.5522, 0.6575, 0.4702, 0.3719,  dsta1130
     1 0.3626, 0.3690, 0.3790, 0.3805, 0.3766, 0.3639, 0.3281/          dsta1140
      DATA DESG2 /                                                      dsta1150
     2 0.6836, 0.6879, 0.6877, 0.6919, 0.6901, 0.7045, 0.7279, 0.7466,  dsta1160
     2 0.7522, 0.7568, 0.7629, 0.7700, 0.7567, 0.7617, 0.7781, 0.8289,  dsta1170
     2 0.8360, 0.8465, 0.8624, 0.8707, 0.9524, 0.8292, 0.6202, 0.6425,  dsta1180
     2 0.5777, 0.5623, 0.7610, 0.7310, 0.7247, 0.7419, 0.7782, 0.7481,  dsta1190
     2 0.7446, 0.8090, 0.8415, 0.8110, 0.6120, 0.7106, 0.5739, 0.4421,  dsta1200
     2 0.4089, 0.3979, 0.3917, 0.3853, 0.3842, 0.3829, 0.3797/          dsta1210
      DATA DESG3 /                                                      dsta1220
     3 0.7718, 0.7865, 0.7907, 0.8077, 0.7801, 0.7827, 0.7871, 0.7880,  dsta1230
     3 0.7887, 0.7888, 0.7894, 0.7909, 0.7882, 0.7934, 0.8103, 0.8729,  dsta1240
     3 0.8766, 0.8844, 0.8979, 0.8997, 0.9698, 0.8318, 0.6197, 0.6420,  dsta1250
     3 0.5797, 0.5698, 0.8014, 0.7938, 0.7901, 0.8069, 0.7894, 0.8139,  dsta1260
     3 0.8086, 0.8546, 0.8691, 0.8288, 0.6394, 0.7400, 0.6495, 0.5235,  dsta1270
     3 0.4793, 0.4583, 0.4376, 0.4169, 0.4006, 0.3941, 0.3875/          dsta1280
      DATA DESG4 /                                                      dsta1290
     4 0.8290, 0.8407, 0.8443, 0.8500, 0.8087, 0.7994, 0.7988, 0.7987,  dsta1300
     4 0.7988, 0.7989, 0.7998, 0.8023, 0.8011, 0.8076, 0.8331, 0.9045,  dsta1310
     4 0.9083, 0.9149, 0.9266, 0.9263, 0.9783, 0.8321, 0.6168, 0.6379,  dsta1320
     4 0.5706, 0.5673, 0.8196, 0.8324, 0.8347, 0.8549, 0.7940, 0.8621,  dsta1330
     4 0.8588, 0.8918, 0.8922, 0.8407, 0.6488, 0.7557, 0.7021, 0.6024,  dsta1340
     4 0.5533, 0.5280, 0.5016, 0.4711, 0.4396, 0.4230, 0.4058/          dsta1350
      END                                                               dsta1360
      SUBROUTINE EQULWC                                                 eqwc 100
CCC                                                                     eqwc 110
CCC   EQUIVALENT LIQUID  WATER CONSTANTS FOR BEXT (0.55UM)=1.0KM-1      eqwc 120
CCC   AWCCON(1-4) IS SET TO ONE OF THE CONSTANTS FOR EACH AEROSOL       eqwc 130
CCC   IN SUBROUTINE EXABIN AND MULTIPLIED BY THE BEXT (DENSTY(N,I))     eqwc 140
CCC   WHERE N=7,12,13 OR 14 AND I IS THE NUMBER OF LAYERS               eqwc 150
CCC                                                                     eqwc 160
      include 'parameter.list'
      COMMON RELHUM(laydim),HSTOR(laydim),ICH(4),VH(17),TX(65),W(65)  
      COMMON IMSMX,WPATH(laythr,65),TBBY(laythr),PATM(laythr),NSPEC,   
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX2(47),AWCCON(5)  
      COMMON /CNTRL/ KMAX,M,IKMAX,NL,ML,IKLO,ISS,IMULT                  eqwc 210
      COMMON /MODEL/ ZM(LAYDIM),PM(LAYDIM),TM(LAYDIM),RFNDX(LAYDIM),
     1  DENSTY(65,LAYDIM),CLDAMT(LAYDIM),RRAMT(LAYDIM),EQLWC(LAYDIM),
     1  HAZEC(LAYDIM)
      DO 140 I=1,ML                                                     eqwc 240
      IF(DENSTY(7,I).NE.0.0) EQLWC(I)=DENSTY(7,I)*AWCCON(1)             eqwc 250
      IF(DENSTY(12,I).NE.0.0) EQLWC(I)=DENSTY(12,I)*AWCCON(2)           eqwc 260
      IF(DENSTY(13,I).NE.0.0) EQLWC(I)=DENSTY(13,I)*AWCCON(3)           eqwc 270
      IF(DENSTY(14,I).NE.0.0) EQLWC(I)=DENSTY(14,I)*AWCCON(4)           eqwc 280
140   CONTINUE                                                          eqwc 290
      RETURN                                                            eqwc 300
      END                                                               eqwc 310
      SUBROUTINE EXABIN                                                 exab 100
      include 'parameter.list'
C                                                                       exab 110
C      LOADS EXTINCTION, ABSORPTION AND ASYMMETRY COEFFICIENTS          exab 120
C      FOR THE FOUR AEROSOL ALTITUDE REGIONS                            exab 130
C                                                                       exab 140
C      MODIFIED FOR ASYMMETRY - JAN 1986 (A.E.R. INC.)                  exab 150
C                                                                       exab 160
      COMMON /CARD1/ MODEL,ITYPE,IEMSCT,M1,M2,M3,IM,NOPRNT,TBOUND,SALB  exab 170
     1  ,MODTRN                                                         exab 180
      LOGICAL MODTRN                                                    exab 190
      COMMON /CARD2/ IHAZE,ISEASN,IVULCN,ICSTL,ICLD,IVSA,VIS,WSS,WHH,   exab 200
     1    RAINRT                                                        exab 210
      COMMON /CARD2D/ IREG(4),ALTB(4),IREGC(4)                          exab 220
      COMMON /CARD3/ H1,H2,ANGLE,RANGE,BETA,RE,LEN                      exab 230
C     COMMON /CARD4/ V1,V2,DV                                           exab 240
      COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     exab 250
C                                                                       exab 260
      COMMON RELHUM(laydim),HSTOR(laydim),ICH(4),VH(17),TX(65),W(65)  
      COMMON IMSMX,WPATH(laythr,65),TBBY(laythr),PATM(laythr),NSPEC,   
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX0(47),AWCCON(5)  
      COMMON /EXTD  /  VX2(47),RUREXT(47,4),RURABS(47,4),RURSYM(47,4),  exab 310
     1 URBEXT(47,4),URBABS(47,4),URBSYM(47,4),OCNEXT(47,4),             exab 320
     2 OCNABS(47,4),OCNSYM(47,4),TROEXT(47,4),TROABS(47,4),             exab 330
     3 TROSYM(47,4),FG1EXT(47),FG1ABS(47),FG1SYM(47),                   exab 340
     4 FG2EXT(47),FG2ABS(47),FG2SYM(47),BSTEXT(47),BSTABS(47),          exab 350
     5 BSTSYM(47),AVOEXT(47),AVOABS(47),AVOSYM(47),FVOEXT(47),          exab 360
     6 FVOABS(47),FVOSYM(47),DMEEXT(47),DMEABS(47),DMESYM(47),          exab 370
     7 CCUEXT(47),CCUABS(47),CCUSYM(47),CALEXT(47),CALABS(47),          exab 380
     8 CALSYM(47),CSTEXT(47),CSTABS(47),CSTSYM(47),CSCEXT(47),          exab 390
     9 CSCABS(47),CSCSYM(47),CNIEXT(47),CNIABS(47),CNISYM(47)           exab 400
      COMMON/CIRR/ CI32XT(47),CI32AB(47),CI32G(47),                     exab 410
     B             CIR2XT(47),CIR2AB(47),CIR2G(47)                      exab 420
      DIMENSION RHZONE(4)                                               exab 430
      DIMENSION ELWCR(4),ELWCU(4),ELWCM(4),ELWCT(4)                     exab 440
      REAL MDLWC                                                        exab 450
      DATA RHZONE/0.,70.,80.,99./                                       exab 460
      DATA ELWCR/3.517E-04,3.740E-04,4.439E-04,9.529E-04/               exab 470
      DATA ELWCM/4.675E-04,6.543E-04,1.166E-03,3.154E-03/               exab 480
      DATA ELWCU/3.102E-04,3.802E-04,4.463E-04,9.745E-04/               exab 490
      DATA ELWCT/1.735E-04,1.820E-04,2.020E-04,2.408E-04/               exab 500
      DATA AFLWC/1.295E-02/,RFLWC/1.804E-03/,CULWC/7.683E-03/           exab 510
      DATA ASLWC/4.509E-03/,STLWC/5.272E-03/,SCLWC/4.177E-03/           exab 520
      DATA SNLWC/7.518E-03/,BSLWC/1.567E-04/,FVLWC/5.922E-04/           exab 530
      DATA AVLWC/1.675E-04/,MDLWC/4.775E-04/                            exab 540
      DATA TNLWC/3.446E-3/ ,TKLWC/5.811E-2/                             exab 550
      DO 2 I = 1,4                                                      exab 560
2     AWCCON(I) = 0                                                     exab 570
      DO 5 I=1,47                                                       exab 580
    5 VX0(I)=VX2(I)                                                     exab 590
CC    I1=1                                                              exab 600
      NB=1                                                              exab 610
      NE=46                                                             exab 620
C                                                                       exab 630
C     47 VALUE CALCULATED IN AEREXT                                     exab 640
C                                                                       exab 650
CC    IF (IHAZE.EQ.7) I1=2                                              exab 660
CC    IF(IHAZE.EQ.3)  I1 = 2                                            exab 670
      DO 185 M=1 ,4                                                     exab 680
      IF(IREG(M) .NE. 0)        GO TO 185                               exab 690
      ITA=ICH(M)                                                        exab 700
      ITC=ICH(M)-7                                                      exab 710
      ITAS = ITA                                                        exab 720
      IF(IREGC(M) .NE. 0) GO TO 100                                     exab 730
12    CONTINUE                                                          exab 740
      WRH=W(15)                                                         exab 750
      IF (ICH(M).EQ.6.AND.M.NE.1) WRH=70.                               exab 760
C     THIS CODING  DOES NOT ALLOW TROP RH DEPENDENT  ABOVE EH(7,I)      exab 770
C     DEFAULTS TO TROPOSPHERIC AT 70. PERCENT                           exab 780
      DO 10 I=2,4                                                       exab 790
      IF (WRH.LT.RHZONE(I)) GO TO 15                                    exab 800
   10 CONTINUE                                                          exab 810
      I=4                                                               exab 820
   15 II=I-1                                                            exab 830
      IF(WRH.GT.0.0.AND.WRH.LT.99.)X=ALOG(100.0-WRH)                    exab 840
      X1=ALOG(100.0-RHZONE(II))                                         exab 850
      X2=ALOG(100.0-RHZONE(I))                                          exab 860
      IF (WRH.GE.99.0) X=X2                                             exab 870
      IF (WRH.LE.0.0) X=X1                                              exab 880
17    DO 80 N=NB,NE                                                     exab 890
      ITA = ITAS                                                        exab 900
      IF(ITA.EQ.3. AND. M.EQ.1) GO TO 18                                exab 910
      ABSC(M,N)=0.                                                      exab 920
      EXTC(M,N)=0.                                                      exab 930
      ASYM(M,N)=0.                                                      exab 940
      IF(ITA.GT.6) GO TO 45                                             exab 950
      IF(ITA.LE.0) GO TO 80                                             exab 960
18    IF(N.GE.41. AND. ITA.EQ.3) ITA = 4                                exab 970
C     RH DEPENDENT AEROSOLS                                             exab 980
      GO TO (20,20,22,25,30,35), ITA                                    exab 990
   20 Y2=ALOG(RUREXT(N,I))                                              exab1000
      Y1=ALOG(RUREXT(N,II))                                             exab1010
      Z2=ALOG(RURABS(N,I))                                              exab1020
      Z1=ALOG(RURABS(N,II))                                             exab1030
      A2=ALOG(RURSYM(N,I))                                              exab1040
      A1=ALOG(RURSYM(N,II))                                             exab1050
      E2=ALOG(ELWCR(I))                                                 exab1060
      E1=ALOG(ELWCR(II))                                                exab1070
      GO TO 40                                                          exab1080
  22  IF(M.GT.1) GO TO 25                                               exab1090
      A2=ALOG(OCNSYM(N,I))                                              exab1100
      A1=ALOG(OCNSYM(N,II))                                             exab1110
      A=A1+(A2-A1)*(X-X1)/(X2-X1)                                       exab1120
      ASYM(M,N)=EXP(A)                                                  exab1130
      E2=ALOG(ELWCM(I))                                                 exab1140
      E1=ALOG(ELWCM(II))                                                exab1150
C                                                                       exab1160
C     NAVY MARITIME AEROSOL CHANGES TO MARINE IN MICROWAVE              exab1170
C     NO NEED TO DEFINE EQUIVALENT WATER                                exab1180
C                                                                       exab1190
      GO TO 80                                                          exab1200
   25 Y2=ALOG(OCNEXT(N,I))                                              exab1210
      Y1=ALOG(OCNEXT(N,II))                                             exab1220
      Z2=ALOG(OCNABS(N,I))                                              exab1230
      Z1=ALOG(OCNABS(N,II))                                             exab1240
      A2=ALOG(OCNSYM(N,I))                                              exab1250
      A1=ALOG(OCNSYM(N,II))                                             exab1260
      E2=ALOG(ELWCM(I))                                                 exab1270
      E1=ALOG(ELWCM(II))                                                exab1280
      GO TO 40                                                          exab1290
   30 Y2=ALOG(URBEXT(N,I))                                              exab1300
      Y1=ALOG(URBEXT(N,II))                                             exab1310
      Z2=ALOG(URBABS(N,I))                                              exab1320
      Z1=ALOG(URBABS(N,II))                                             exab1330
      A2=ALOG(URBSYM(N,I))                                              exab1340
      A1=ALOG(URBSYM(N,II))                                             exab1350
      E2=ALOG(ELWCU(I))                                                 exab1360
      E1=ALOG(ELWCU(II))                                                exab1370
      GO TO 40                                                          exab1380
   35 Y2=ALOG(TROEXT(N,I))                                              exab1390
      Y1=ALOG(TROEXT(N,II))                                             exab1400
      Z2=ALOG(TROABS(N,I))                                              exab1410
      Z1=ALOG(TROABS(N,II))                                             exab1420
      A2=ALOG(TROSYM(N,I))                                              exab1430
      A1=ALOG(TROSYM(N,II))                                             exab1440
      E2=ALOG(ELWCT(I))                                                 exab1450
      E1=ALOG(ELWCT(II))                                                exab1460
   40 Y=Y1+(Y2-Y1)*(X-X1)/(X2-X1)                                       exab1470
      ZK=Z1+(Z2-Z1)*(X-X1)/(X2-X1)                                      exab1480
      A=A1+(A2-A1)*(X-X1)/(X2-X1)                                       exab1490
      ABSC(M,N)=EXP(ZK)                                                 exab1500
      EXTC(M,N)=EXP(Y)                                                  exab1510
      ASYM(M,N)=EXP(A)                                                  exab1520
      IF(N.EQ.1) EC=E1+(E2-E1)*(X-X1)/(X2-X1)                           exab1530
      IF(N.EQ.1) AWCCON(M)=EXP(EC)                                      exab1540
      GO TO 80                                                          exab1550
   45 IF (ITA.GT.19) GO TO 75                                           exab1560
      IF (ITC.LT.1) GO TO 80                                            exab1570
      GO TO (50,55,80,60,65,70,65,70,60,60,70,75), ITC                  exab1580
   50 ABSC(M,N)=FG1ABS(N)                                               exab1590
      EXTC(M,N)=FG1EXT(N)                                               exab1600
      ASYM(M,N)=FG1SYM(N)                                               exab1610
      IF(N.EQ.1) AWCCON(M)=AFLWC                                        exab1620
      GO TO 80                                                          exab1630
   55 ABSC(M,N)=FG2ABS(N)                                               exab1640
      EXTC(M,N)=FG2EXT(N)                                               exab1650
      ASYM(M,N)=FG2SYM(N)                                               exab1660
      IF(N.EQ.1) AWCCON(M)=RFLWC                                        exab1670
      GO TO 80                                                          exab1680
   60 ABSC(M,N)=BSTABS(N)                                               exab1690
      EXTC(M,N)=BSTEXT(N)                                               exab1700
      ASYM(M,N)=BSTSYM(N)                                               exab1710
      IF(N.EQ.1) AWCCON(M)=BSLWC                                        exab1720
      GO TO 80                                                          exab1730
   65 ABSC(M,N)=AVOABS(N)                                               exab1740
      EXTC(M,N)=AVOEXT(N)                                               exab1750
      ASYM(M,N)=AVOSYM(N)                                               exab1760
      IF(N.EQ.1) AWCCON(M)=AVLWC                                        exab1770
      GO TO 80                                                          exab1780
   70 ABSC(M,N)=FVOABS(N)                                               exab1790
      EXTC(M,N)=FVOEXT(N)                                               exab1800
      ASYM(M,N)=FVOSYM(N)                                               exab1810
      IF(N.EQ.1) AWCCON(M)=FVLWC                                        exab1820
      GO TO 80                                                          exab1830
   75 ABSC(M,N)=DMEABS(N)                                               exab1840
      EXTC(M,N)=DMEEXT(N)                                               exab1850
      ASYM(M,N)=DMESYM(N)                                               exab1860
      IF(N.EQ.1) AWCCON(M)=MDLWC                                        exab1870
   80 CONTINUE                                                          exab1880
      GO TO 185                                                         exab1890
100   CONTINUE                                                          exab1900
CCC                                                                     exab1910
CCC       SECTION TO LOAD EXTINCTION AND ABSORPTION COEFFICIENTS        exab1920
CCC       FOR CLOUD AND OR RAIN MODELS                                  exab1930
CCC                                                                     exab1940
      DO 150 N=NB,NE                                                    exab1950
      ABSC(M,N)=0.0                                                     exab1960
      EXTC(M,N)=0.0                                                     exab1970
      ASYM(M,N)=0.0                                                     exab1980
      IC=ICLD                                                           exab1990
      GO TO (125,130,135,140,145,135,145,145,125,125,125), IC           exab2000
125   ABSC(M,N)=CCUABS(N)                                               exab2010
      EXTC(M,N)=CCUEXT(N)                                               exab2020
      ASYM(M,N)=CCUSYM(N)                                               exab2030
      IF(N.EQ.1) AWCCON(M)=CULWC                                        exab2040
      GO TO 150                                                         exab2050
130   ABSC(M,N)=CALABS(N)                                               exab2060
      EXTC(M,N)=CALEXT(N)                                               exab2070
      ASYM(M,N)=CALSYM(N)                                               exab2080
      IF(N.EQ.1) AWCCON(M)=ASLWC                                        exab2090
      GO TO 150                                                         exab2100
135   ABSC(M,N)=CSTABS(N)                                               exab2110
      EXTC(M,N)=CSTEXT(N)                                               exab2120
      ASYM(M,N)=CSTSYM(N)                                               exab2130
      IF(N.EQ.1) AWCCON(M)=STLWC                                        exab2140
      GO TO 150                                                         exab2150
140   ABSC(M,N)=CSCABS(N)                                               exab2160
      EXTC(M,N)=CSCEXT(N)                                               exab2170
      ASYM(M,N)=CSCSYM(N)                                               exab2180
      IF(N.EQ.1) AWCCON(M)=SCLWC                                        exab2190
      GO TO 150                                                         exab2200
145   ABSC(M,N)=CNIABS(N)                                               exab2210
      EXTC(M,N)=CNIEXT(N)                                               exab2220
      ASYM(M,N)=CNISYM(N)                                               exab2230
      IF(N.EQ.1) AWCCON(M)=SNLWC                                        exab2240
150   CONTINUE                                                          exab2250
185   CONTINUE                                                          exab2260
      DO 200 N=1,47                                                     exab2270
      ABSC(5,N)=0.                                                      exab2280
      EXTC(5,N)=0.                                                      exab2290
      ASYM(5,N)=0.                                                      exab2300
      AWCCON(5)=0.                                                      exab2310
      IF(ICLD .EQ. 18) THEN                                             exab2320
           EXTC(5,N)= CI32XT(N)                                         exab2330
           ABSC(5,N)= CI32AB(N)                                         exab2340
           ASYM(5,N)= CI32G(N)                                          exab2350
           AWCCON(5)=3.446E-3                                           exab2360
      ENDIF                                                             exab2370
      IF(ICLD .EQ. 19) THEN                                             exab2380
           EXTC(5,N)= CIR2XT(N)                                         exab2390
           ABSC(5,N)= CIR2AB(N)                                         exab2400
           ASYM(5,N)= CIR2G(N)                                          exab2410
           AWCCON(5)=5.811E-2                                           exab2420
      ENDIF                                                             exab2430
200   CONTINUE                                                          exab2440
      RETURN                                                            exab2450
C                                                                       exab2460
      END                                                               exab2470
      SUBROUTINE EXPINT(X,X1,X2,A)                                      expt 100
C     EXPONENTIAL INTERPOLATION                                         expt 110
      IF(X1.EQ.0.0 .OR. X2.EQ.0.0)  GO TO 100                           expt 120
      X = X1*(X2/X1)**A                                                 expt 130
      RETURN                                                            expt 140
  100 X = X1+(X2-X1)*A                                                  expt 150
      RETURN                                                            expt 160
      END                                                               expt 170
      BLOCK DATA EXTDTA                                                 exdt 100
C>    BLOCK DATA                                                        exdt 110
CCC                                                                     exdt 120
CCC   ALTITUDE REGIONS FOR AEROSOL EXTINCTION COEFFICIENTS              exdt 130
CCC                                                                     exdt 140
CCC                                                                     exdt 150
CCC         0-2KM                                                       exdt 160
CCC           RUREXT=RURAL EXTINCTION   RURABS=RURAL ABSORPTION         exdt 170
CCC           RURSYM=RURAL ASYMMETRY FACTORS                            exdt 180
CCC           URBEXT=URBAN EXTINCTION   URBABS=URBAN ABSORPTION         exdt 190
CCC           URBSYM=URBAN ASYMMETRY FACTORS                            exdt 200
CCC           OCNEXT=MARITIME EXTINCTION  OCNABS=MARITIME ABSORPTION    exdt 210
CCC           OCNSYM=MARITIME ASYMMETRY FACTORS                         exdt 220
CCC           TROEXT=TROPSPHER EXTINCTION  TROABS=TROPOSPHER ABSORPTION exdt 230
CCC           TROSYM=TROPSPHERIC ASYMMETRY FACTORS                      exdt 240
CCC           FG1EXT=FOG1 .2KM VIS EXTINCTION  FG1ABS=FOG1 ABSORPTION   exdt 250
CCC           FG1SYM=FOG1 ASYMMETRY FACTORS                             exdt 260
CCC           FG2EXT=FOG2 .5KM VIS EXTINCTION  FG2ABS=FOG2 ABSORPTION   exdt 270
CCC           FG2SYM=FOG2 ASYMMETRY FACTORS                             exdt 280
CCC         >2-10KM                                                     exdt 290
CCC           TROEXT=TROPOSPHER EXTINCTION  TROABS=TROPOSPHER ABSORPTIONexdt 300
CCC           TROSYM=TROPOSPHERIC ASYMMETRY FACTORS                     exdt 310
CCC         >10-30KM                                                    exdt 320
CCC           BSTEXT=BACKGROUND STRATOSPHERIC EXTINCTION                exdt 330
CCC           BSTABS=BACKGROUND STRATOSPHERIC ABSORPTION                exdt 340
CCC           BSTSYM=BACKGROUND STRATOSPHERIC ASYMMETRY FACTORS         exdt 350
CCC           AVOEXT=AGED VOLCANIC EXTINCTION                           exdt 360
CCC           AVOABS=AGED VOLCANIC ABSORPTION                           exdt 370
CCC           AVOSYM=AGED VOLCANIC ASYMMETRY FACTORS                    exdt 380
CCC           FVOEXT=FRESH VOLCANIC EXTINCTION                          exdt 390
CCC           FVOABS=FRESH VOLCANIC ABSORPTION                          exdt 400
CCC           FVOSYM=FRESH VOLCANIC ASYMMETRY FACTORS                   exdt 410
CCC         >30-100KM                                                   exdt 420
CCC           DMEEXT=METEORIC DUST EXTINCTION                           exdt 430
CCC           DMEABS=METEORIC DUST ABSORPTION                           exdt 440
CCC           DMESYM=METEORIC DUST ASYMMETRY FACTORS                    exdt 450
C                                                                       exdt 460
C     AEROSOL EXTINCTION AND ABSORPTION DATA                            exdt 470
C                                                                       exdt 480
C     MODIFIED TO INCLUDE ASYMMETRY DATA - JAN 1986 (A.E.R. INC.)       exdt 490
C                                                                       exdt 500
C     COMMON /EXTD  /VX2(47),RUREXT(47,4),RURABS(47,4),URBEXT(47,4),    exdt 510
C    1URBABS(47,4),OCNEXT(47,4),OCNABS(47,4),TROEXT(47,4),TROABS(47,4), exdt 520
C    2FG1EXT(47),FG1ABS(47),FG2EXT(47),FG2ABS(47),                      exdt 530
C    3   BSTEXT(47),BSTABS(47),AVOEXT(47),AVOABS(47),FVOEXT(47)         exdt 540
C    4),FVOABS(47),DMEEXT(47),DMEABS(47),                               exdt 550
C                                                                       exdt 560
C                                                                       exdt 570
      COMMON /EXTD  / VX2(47),RURE1(47),RURE2(47),RURE3(47),RURE4(47),  exdt 580
     X RURA1(47),RURA2(47),RURA3(47),RURA4(47),                         exdt 590
     X RURG1(47),RURG2(47),RURG3(47),RURG4(47),                         exdt 600
     X URBE1(47),URBE2(47),URBE3(47),URBE4(47),                         exdt 610
     X URBA1(47),URBA2(47),URBA3(47),URBA4(47),                         exdt 620
     X URBG1(47),URBG2(47),URBG3(47),URBG4(47),                         exdt 630
     X OCNE1(47),OCNE2(47),OCNE3(47),OCNE4(47),                         exdt 640
     X OCNA1(47),OCNA2(47),OCNA3(47),OCNA4(47),                         exdt 650
     X OCNG1(47),OCNG2(47),OCNG3(47),OCNG4(47),                         exdt 660
     X TROE1(47),TROE2(47),TROE3(47),TROE4(47),                         exdt 670
     X TROA1(47),TROA2(47),TROA3(47),TROA4(47),                         exdt 680
     X TROG1(47),TROG2(47),TROG3(47),TROG4(47),                         exdt 690
     2 FG1EXT(47),FG1ABS(47),FG1SYM(47),FG2EXT(47),FG2ABS(47),          exdt 700
     3 FG2SYM(47),BSTEXT(47),BSTABS(47),BSTSYM(47),AVOEXT(47),          exdt 710
     4 AVOABS(47),AVOSYM(47),FVOEXT(47),FVOABS(47),FVOSYM(47),          exdt 720
     5 DMEEXT(47),DMEABS(47),DMESYM(47),CCUEXT(47),CCUABS(47),          exdt 730
     6 CCUSYM(47),CALEXT(47),CALABS(47),CALSYM(47),CSTEXT(47),          exdt 740
     7 CSTABS(47),CSTSYM(47),CSCEXT(47),CSCABS(47),CSCSYM(47),          exdt 750
     8 CNIEXT(47),CNIABS(47),CNISYM(47)                                 exdt 760
C                                                                       exdt 770
C         CI64--    STANDARD  CIRRUS  CLOUD  MODEL                      exdt 780
C              ICE 64 MICRON MODE RADIUS CIRRUS CLOUD MODEL             exdt 790
C                                                                       exdt 800
C         CIR4--    OPTICALLY  THIN  CIRRUS  MODEL                      exdt 810
C              ICE  4 MICRON MODE RADIUS CIRRUS CLOUD MODEL             exdt 820
C                                                                       exdt 830
       COMMON/CIRR/ CI64XT(47),CI64AB(47),CI64G(47),                    exdt 840
     B              CIR4XT(47),CIR4AB(47),CIR4G(47)                     exdt 850
      DATA VX2 /                                                        exdt 860
     *   .2000,   .3000,   .3371,   .5500,   .6943,  1.0600,  1.5360,   exdt 870
     *  2.0000,  2.2500,  2.5000,  2.7000,  3.0000,  3.3923,  3.7500,   exdt 880
     *  4.5000,  5.0000,  5.5000,  6.0000,  6.2000,  6.5000,  7.2000,   exdt 890
     *  7.9000,  8.2000,  8.7000,  9.0000,  9.2000, 10.0000, 10.5910,   exdt 900
     * 11.0000, 11.5000, 12.5000, 14.8000, 15.0000, 16.4000, 17.2000,   exdt 910
     * 18.5000, 21.3000, 25.0000, 30.0000, 40.0000, 50.0000, 60.0000,   exdt 920
     * 80.0000, 100.000, 150.000, 200.000, 300.000/                     exdt 930
      DATA RURE1 /                                                      exdt 940
     1 2.09291, 1.74582, 1.60500, 1.00000,  .75203,  .41943,  .24070,   exdt 950
     2  .14709,  .13304,  .12234,  .13247,  .11196,  .10437,  .09956,   exdt 960
     3  .09190,  .08449,  .07861,  .07025,  .07089,  .07196,  .07791,   exdt 970
     4  .04481,  .04399,  .12184,  .12658,  .12829,  .09152,  .08076,   exdt 980
     5  .07456,  .06880,  .06032,  .04949,  .05854,  .06000,  .06962,   exdt 990
     6  .05722,  .06051,  .05177,  .04589,  .04304,                     exdt1000
     7  .03582,  .03155,  .02018,  .01469,  .00798,  .00551, 0./        exdt1010
      DATA RURE2 /                                                      exdt1020
     1 2.09544, 1.74165, 1.59981, 1.00000,  .75316,  .42171,  .24323,   exdt1030
     2  .15108,  .13608,  .12430,  .13222,  .13823,  .11076,  .10323,   exdt1040
     3  .09475,  .08728,  .08076,  .07639,  .07797,  .07576,  .07943,   exdt1050
     4  .04899,  .04525,  .12165,  .12741,  .12778,  .09032,  .07962,   exdt1060
     5  .07380,  .06880,  .06329,  .05791,  .06646,  .06639,  .07443,   exdt1070
     6  .06304,  .06443,  .05538,  .04867,  .04519,                     exdt1080
     7  .03821,  .03374,  .02173,  .01587,  .00862,  .00594, 0./        exdt1090
      DATA RURE3 /                                                      exdt1100
     1 2.07082, 1.71456, 1.57962, 1.00000,  .76095,  .43228,  .25348,   exdt1110
     2  .16456,  .14677,  .13234,  .13405,  .20316,  .12873,  .11506,   exdt1120
     3  .10481,  .09709,  .08918,  .09380,  .09709,  .08791,  .08601,   exdt1130
     4  .06247,  .05601,  .11905,  .12595,  .12348,  .08741,  .07703,   exdt1140
     5  .07266,  .07044,  .07443,  .08146,  .08810,  .08563,  .08962,   exdt1150
     6  .08051,  .07677,  .06658,  .05747,  .05184,                     exdt1160
     7  .04572,  .04074,  .02689,  .01981,  .01084,  .00714, 0./        exdt1170
      DATA RURE4 /                                                      exdt1180
     1 1.66076, 1.47886, 1.40139, 1.00000,  .80652,  .50595,  .32259,   exdt1190
     2  .23468,  .20772,  .18532,  .17348,  .35114,  .20006,  .17386,   exdt1200
     3  .16139,  .15424,  .14557,  .16215,  .16766,  .14994,  .14032,   exdt1210
     4  .12968,  .12601,  .13551,  .13582,  .13228,  .11070,  .09994,   exdt1220
     5  .09873,  .10418,  .13241,  .15924,  .16139,  .15949,  .15778,   exdt1230
     6  .15184,  .13848,  .12563,  .11076,  .09601,                     exdt1240
     7  .09312,  .08720,  .06644,  .05264,  .03181,  .02196, 0.0/       exdt1250
      DATA RURA1 /                                                      exdt1260
     1  .67196,  .11937,  .08506,  .05930,  .05152,  .05816,  .05006,   exdt1270
     2  .01968,  .02070,  .02101,  .05652,  .02785,  .01316,  .00867,   exdt1280
     3  .01462,  .01310,  .01627,  .02013,  .02165,  .02367,  .03538,   exdt1290
     4  .02823,  .03962,  .06778,  .07285,  .08120,  .04032,  .03177,   exdt1300
     5  .02557,  .02342,  .02177,  .02627,  .03943,  .03114,  .03696,   exdt1310
     6  .02956,  .03500,  .03241,  .03297,  .03380,                     exdt1320
     7  .03170,  .02794,  .01769,  .01305,  .00730,  .00518, 0.0/       exdt1330
      DATA RURA2 /                                                      exdt1340
     1  .62968,  .10816,  .07671,  .05380,  .04684,  .05335,  .04614,   exdt1350
     2  .01829,  .01899,  .01962,  .05525,  .06816,  .01652,  .00867,   exdt1360
     3  .01544,  .01373,  .01627,  .02892,  .02829,  .02532,  .03487,   exdt1370
     4  .02835,  .03854,  .06684,  .07272,  .08038,  .03987,  .03247,   exdt1380
     5  .02816,  .02816,  .03101,  .03741,  .04829,  .04032,  .04399,   exdt1390
     6  .03734,  .03956,  .03601,  .03525,  .03563,                     exdt1400
     7 .03357,  .02965,  .01887,  .01395,  .00782,  .00555, 0.0/        exdt1410
      DATA RURA3 /                                                      exdt1420
     1  .51899,  .08278,  .05816,  .04082,  .03570,  .04158,  .03620,   exdt1430
     2  .01513,  .01481,  .01633,  .05278,  .13690,  .02494,  .00886,   exdt1440
     3  .01804,  .01582,  .01677,  .04816,  .04367,  .03013,  .03443,   exdt1450
     4  .02930,  .03677,  .06209,  .06911,  .07475,  .03892,  .03494,   exdt1460
     5  .03513,  .03968,  .05152,  .06241,  .06937,  .06203,  .06215,   exdt1470
     6  .05614,  .05209,  .04608,  .04196,  .04095,                     exdt1480
     7  .03916,  .03486,  .02262,  .01686,  .00951,  .00674, 0.0/       exdt1490
      DATA RURA4 /                                                      exdt1500
     1  .21943,  .02848,  .01943,  .01342,  .01171,  .01437,  .01323,   exdt1510
     2  .01152,  .00696,  .01329,  .06108,  .24690,  .05323,  .01430,   exdt1520
     3  .03361,  .02949,  .02652,  .09437,  .08506,  .05348,  .04627,   exdt1530
     4  .04380,  .04557,  .05380,  .05715,  .05899,  .04861,  .05253,   exdt1540
     5  .06171,  .07437,  .10152,  .12019,  .12190,  .11734,  .11411,   exdt1550
     6  .10766,  .09487,  .08430,  .07348,  .06861,                     exdt1560
     7  .06936,  .06458,  .04735,  .03761,  .02313,  .01668, 0.0/       exdt1570
      DATA RURG1 /                                                      exdt1580
     1  .7581,   .6785,   .6712,   .6479,   .6342,   .6176,   .6334,    exdt1590
     2  .7063,   .7271,   .7463,   .7788,   .7707,   .7424,   .7312,    exdt1600
     3  .7442,   .7516,   .7662,   .7940,   .7886,   .7797,   .7664,    exdt1610
     4  .8525,   .8700,   .5846,   .5570,   .5992,   .6159,   .6271,    exdt1620
     5  .6257,   .6374,   .6546,   .6861,   .6859,   .6120,   .5570,    exdt1630
     6  .5813,   .5341,   .5284,   .5137,   .4348,   .4223,   .3775,    exdt1640
     7  .3435,   .3182,   .2791,   .2494,   .0000/                      exdt1650
      DATA RURG2 /                                                      exdt1660
     1  .7632,   .6928,   .6865,   .6638,   .6498,   .6314,   .6440,    exdt1670
     2  .7098,   .7303,   .7522,   .7903,   .7804,   .7380,   .7319,    exdt1680
     3  .7508,   .7584,   .7738,   .8071,   .7929,   .7843,   .7747,    exdt1690
     4  .8507,   .8750,   .6112,   .5851,   .6272,   .6466,   .6616,    exdt1700
     5  .6653,   .6798,   .6965,   .7026,   .6960,   .6360,   .5848,    exdt1710
     6  .6033,   .5547,   .5445,   .5274,   .4518,   .4318,   .3863,    exdt1720
     7  .3516,   .3257,   .2853,   .2548,   .0000/                      exdt1730
      DATA RURG3 /                                                      exdt1740
     1  .7725,   .7240,   .7197,   .6997,   .6858,   .6650,   .6702,    exdt1750
     2  .7181,   .7378,   .7653,   .8168,   .7661,   .7286,   .7336,    exdt1760
     3  .7654,   .7735,   .7910,   .8303,   .8025,   .7957,   .7946,    exdt1770
     4  .8468,   .8734,   .6831,   .6619,   .6994,   .7250,   .7449,    exdt1780
     5  .7547,   .7665,   .7644,   .7265,   .7170,   .6769,   .6409,    exdt1790
     6  .6442,   .6031,   .5854,   .5646,   .4977,   .4602,   .4127,    exdt1800
     7  .3751,   .3476,   .3048,   .2721,   .0000/                      exdt1810
      DATA RURG4 /                                                      exdt1820
     1  .7778,   .7793,   .7786,   .7717,   .7628,   .7444,   .7365,    exdt1830
     2  .7491,   .7609,   .7921,   .8688,   .7537,   .7294,   .7413,    exdt1840
     3  .7928,   .8016,   .8225,   .8761,   .8359,   .8285,   .8385,    exdt1850
     4  .8559,   .8654,   .8414,   .8415,   .8527,   .8740,   .8903,    exdt1860
     5  .8952,   .8923,   .8611,   .8033,   .7989,   .7758,   .7632,    exdt1870
     6  .7508,   .7314,   .7091,   .6867,   .6419,   .5790,   .5259,    exdt1880
     7  .4749,   .4415,   .3886,   .3489,   .0000/                      exdt1890
      DATA URBE1 /                                                      exdt1900
     1 1.88816, 1.63316, 1.51867, 1.00000,  .77785,  .47095,  .30006,   exdt1910
     2  .21392,  .19405,  .17886,  .18127,  .16133,  .14785,  .14000,   exdt1920
     3  .12715,  .11880,  .11234,  .10601,  .10500,  .10361,  .10342,   exdt1930
     4  .08766,  .08652,  .11937,  .12139,  .12297,  .09797,  .09057,   exdt1940
     5  .08595,  .08196,  .07563,  .06696,  .07209,  .06842,  .07177,   exdt1950
     6  .06354,  .06177,  .05373,  .04728,  .04051,                     exdt1960
     7  .03154,  .02771,  .01759,  .01278,  .00693,  .00480, 0.0/       exdt1970
      DATA URBE2 /                                                      exdt1980
     1 1.95582, 1.64994, 1.53070, 1.00000,  .77614,  .46639,  .29487,   exdt1990
     2  .21051,  .18943,  .17285,  .17209,  .21418,  .15354,  .14051,   exdt2000
     3  .12728,  .11861,  .11089,  .11329,  .11323,  .10563,  .10247,   exdt2010
     4  .08696,  .08361,  .12013,  .12418,  .12304,  .09614,  .08842,   exdt2020
     5  .08487,  .08285,  .08361,  .08430,  .08880,  .08449,  .08601,   exdt2030
     6  .07835,  .07323,  .06367,  .05500,  .04747,                     exdt2040
     7  .03901,  .03454,  .02240,  .01638,  .00891,  .00612, 0.0/       exdt2050
      DATA URBE3 /                                                      exdt2060
     1 1.96430, 1.64032, 1.52392, 1.00000,  .77709,  .46253,  .28690,   exdt2070
     2  .20310,  .17981,  .16101,  .15614,  .26475,  .15456,  .13563,   exdt2080
     3  .12215,  .11361,  .10500,  .11715,  .11753,  .10392,  .09766,   exdt2090
     4  .08443,  .08057,  .10943,  .11342,  .11063,  .08703,  .08025,   exdt2100
     5  .07886,  .08032,  .09101,  .10070,  .10386,  .09943,  .09886,   exdt2110
     6  .09152,  .08247,  .07152,  .06089,  .05253,                     exdt2120
     7  .04582,  .04091,  .02717,  .02008,  .01103,  .00754, 0.0/       exdt2130
      DATA URBE4 /                                                      exdt2140
     1 1.41266, 1.33816, 1.29114, 1.00000,  .83646,  .55025,  .35342,   exdt2150
     2  .25285,  .21576,  .18310,  .16215,  .37854,  .20494,  .16665,   exdt2160
     3  .14778,  .13892,  .12943,  .15525,  .15709,  .13513,  .12481,   exdt2170
     4  .11759,  .11494,  .11487,  .11329,  .11108,  .09911,  .09209,   exdt2180
     5  .09342,  .10120,  .13177,  .15696,  .15766,  .15513,  .15203,   exdt2190
     6  .14532,  .13038,  .11785,  .10411,  .09101,                     exdt2200
     7  .08907,  .08399,  .06579,  .05337,  .03372,  .02379, 0.0/       exdt2210
      DATA URBA1 /                                                      exdt2220
     1  .78437,  .58975,  .54285,  .36184,  .29222,  .20886,  .15658,   exdt2230
     2  .12329,  .11462,  .10747,  .11797,  .10025,  .08759,  .08184,   exdt2240
     3  .07506,  .07006,  .06741,  .06601,  .06544,  .06449,  .06665,   exdt2250
     4  .06278,  .06949,  .07316,  .07462,  .08101,  .05753,  .05272,   exdt2260
     5  .04899,  .04734,  .04494,  .04443,  .05133,  .04348,  .04443,   exdt2270
     6  .03994,  .03981,  .03633,  .03468,  .03146,                     exdt2280
     7  .02809,  .02471,  .01556,  .01145,  .00639,  .00454, 0.0/       exdt2290
      DATA URBA2 /                                                      exdt2300
     1  .69032,  .49367,  .45165,  .29741,  .24070,  .17399,  .13146,   exdt2310
     2  .10354,  .09589,  .09025,  .10411,  .15101,  .07880,  .06949,   exdt2320
     3  .06570,  .06095,  .05829,  .07171,  .06797,  .05975,  .06013,   exdt2330
     4  .05589,  .06051,  .07139,  .07494,  .07956,  .05525,  .05184,   exdt2340
     5  .05089,  .05291,  .05886,  .06380,  .06880,  .06127,  .06019,   exdt2350
     6  .05525,  .05070,  .04500,  .04076,  .03741,                     exdt2360
     7  .03400,  .03010,  .01926,  .01427,  .00800,  .00567, 0.0/       exdt2370
      DATA URBA3 /                                                      exdt2380
     1  .54848,  .37101,  .33734,  .21949,  .17785,  .12968,  .09854,   exdt2390
     2  .07804,  .07165,  .06791,  .08563,  .19639,  .06722,  .05316,   exdt2400
     3  .05316,  .04886,  .04620,  .07570,  .06899,  .05291,  .05101,   exdt2410
     4  .04734,  .05025,  .06171,  .06570,  .06854,  .04892,  .04797,   exdt2420
     5  .05057,  .05665,  .07127,  .08095,  .08411,  .07728,  .07475,   exdt2430
     6  .06886,  .06019,  .05222,  .04538,  .04171,                     exdt2440
     7  .03911,  .03486,  .02271,  .01697,  .00961,  .00681, 0.0/       exdt2450
      DATA URBA4 /                                                      exdt2460
     1  .15975,  .10000,  .09013,  .05785,  .04671,  .03424,  .02633,   exdt2470
     2  .02525,  .01975,  .02354,  .06241,  .26690,  .05810,  .02285,   exdt2480
     3  .03810,  .03386,  .03044,  .09627,  .08557,  .05405,  .04576,   exdt2490
     4  .04392,  .04424,  .04671,  .04791,  .04861,  .04684,  .05177,   exdt2500
     5  .06158,  .07475,  .10342,  .12146,  .12177,  .11734,  .11335,   exdt2510
     6  .10608,  .09171,  .08063,  .06968,  .06475,                     exdt2520
     7  .06559,  .06131,  .04591,  .03714,  .02365,  .01734, 0.0/       exdt2530
      DATA URBG1 /                                                      exdt2540
     1  .7785,   .7182,   .7067,   .6617,   .6413,   .6166,   .6287,    exdt2550
     2  .6883,   .7070,   .7243,   .7370,   .7446,   .7391,   .7371,    exdt2560
     3  .7414,   .7435,   .7466,   .7543,   .7498,   .7424,   .7270,    exdt2570
     4  .7674,   .7850,   .5880,   .5616,   .5901,   .6159,   .6238,    exdt2580
     5  .6240,   .6281,   .6306,   .6298,   .6252,   .5785,   .5378,    exdt2590
     6  .5512,   .5072,   .4930,   .4709,   .4009,   .4110,   .3672,    exdt2600
     7  .3344,   .3093,   .2717,   .2426,   .0000/                      exdt2610
      DATA URBG2 /                                                      exdt2620
     1  .7906,   .7476,   .7385,   .6998,   .6803,   .6536,   .6590,    exdt2630
     2  .7066,   .7258,   .7484,   .7769,   .7405,   .7351,   .7459,    exdt2640
     3  .7625,   .7673,   .7759,   .7910,   .7732,   .7703,   .7644,    exdt2650
     4  .7966,   .8142,   .6635,   .6428,   .6700,   .6935,   .7050,    exdt2660
     5  .7092,   .7145,   .7094,   .6762,   .6684,   .6316,   .5997,    exdt2670
     6  .6013,   .5625,   .5433,   .5198,   .4552,   .4387,   .3928,    exdt2680
     7  .3575,   .3310,   .2899,   .2588,   .0000/                      exdt2690
      DATA URBG3 /                                                      exdt2700
     1  .7949,   .7713,   .7650,   .7342,   .7162,   .6873,   .6820,    exdt2710
     2  .7131,   .7312,   .7583,   .8030,   .7171,   .7185,   .7400,    exdt2720
     3  .7698,   .7778,   .7923,   .8142,   .7864,   .7867,   .7891,    exdt2730
     4  .8147,   .8298,   .7276,   .7136,   .7361,   .7590,   .7729,    exdt2740
     5  .7783,   .7808,   .7624,   .7094,   .7022,   .6714,   .6480,    exdt2750
     6  .6417,   .6104,   .5887,   .5651,   .5058,   .4692,   .4212,    exdt2760
     7  .3825,   .3549,   .3112,   .2778,   .0000/                      exdt2770
      DATA URBG4 /                                                      exdt2780
     1  .7814,   .7993,   .7995,   .7948,   .7870,   .7682,   .7751,    exdt2790
     2  .7501,   .7565,   .7809,   .8516,   .7137,   .7039,   .7241,    exdt2800
     3  .7728,   .7846,   .8093,   .8576,   .8125,   .8140,   .8304,    exdt2810
     4  .8472,   .8549,   .8525,   .8569,   .8640,   .8853,   .9017,    exdt2820
     5  .9061,   .9021,   .8685,   .8126,   .8091,   .7897,   .7802,    exdt2830
     6  .7691,   .7550,   .7353,   .7146,   .6754,   .6134,   .5601,    exdt2840
     7  .5056,   .4701,   .4134,   .3714,   .0000/                      exdt2850
      DATA OCNE1 /                                                      exdt2860
     1 1.47576, 1.32614, 1.26171, 1.00000,  .88133,  .70297,  .56487,   exdt2870
     2  .46006,  .42044,  .38310,  .35076,  .42266,  .32278,  .28810,   exdt2880
     3  .24905,  .21184,  .16734,  .14791,  .21532,  .15076,  .12057,   exdt2890
     4  .10038,  .10703,  .15070,  .15665,  .14639,  .10228,  .08367,   exdt2900
     5  .07373,  .06829,  .05044,  .04373,  .04962,  .06158,  .07703,   exdt2910
     6  .07234,  .06297,  .05481,  .05329,  .08741,                     exdt2920
     7  .04608,  .03959,  .02382,  .01712,  .00936,  .00665, 0.0/       exdt2930
      DATA OCNE2 /                                                      exdt2940
     1 1.36924, 1.25443, 1.20835, 1.00000,  .91367,  .77089,  .64987,   exdt2950
     2  .54886,  .50247,  .45038,  .38209,  .50589,  .43766,  .38076,   exdt2960
     3  .31658,  .27475,  .22215,  .21019,  .27570,  .21057,  .16949,   exdt2970
     4  .14209,  .14215,  .16956,  .17082,  .16025,  .11665,  .09759,   exdt2980
     5  .09215,  .09373,  .10532,  .12570,  .13000,  .13633,  .14291,   exdt2990
     6  .13506,  .11475,  .09658,  .08291,  .10348,                     exdt3000
     7  .06693,  .05786,  .03522,  .02519,  .01358,  .00954, 0.0/       exdt3010
      DATA OCNE3 /                                                      exdt3020
     1 1.22259, 1.14627, 1.11842, 1.00000,  .94766,  .87538,  .80418,   exdt3030
     2  .72930,  .68582,  .62165,  .49962,  .67949,  .66468,  .59253,   exdt3040
     3  .49551,  .44671,  .37886,  .35924,  .43367,  .37019,  .30842,   exdt3050
     4  .26437,  .25228,  .24905,  .23975,  .22766,  .17804,  .15316,   exdt3060
     5  .15373,  .16791,  .22361,  .28348,  .28677,  .29082,  .29038,   exdt3070
     6  .27810,  .23867,  .20209,  .16430,  .14943,                     exdt3080
     7  .12693,  .11177,  .07095,  .05084,  .02690,  .01838, 0.0/       exdt3090
      DATA OCNE4 /                                                      exdt3100
     1 1.09133, 1.06601, 1.05620, 1.00000,  .97506,  .94791,  .94203,   exdt3110
     2  .93671,  .92867,  .90411,  .80253,  .89222,  .94462,  .92146,   exdt3120
     3  .85797,  .82595,  .76747,  .68646,  .78209,  .75266,  .68658,   exdt3130
     4  .62722,  .60228,  .56335,  .53728,  .51861,  .43449,  .37196,   exdt3140
     5  .35899,  .37316,  .46854,  .58234,  .58690,  .60348,  .60563,   exdt3150
     6  .60000,  .55392,  .50367,  .43576,  .35949,                     exdt3160
     7  .34729,  .32254,  .23600,  .17953,  .10071,  .06714, 0.0/       exdt3170
      DATA OCNA1 /                                                      exdt3180
     1  .30987,  .04354,  .02880,  .01797,  .01468,  .01766,  .01582,   exdt3190
     2  .00816,  .01146,  .01677,  .03310,  .03380,  .00715,  .00443,   exdt3200
     3  .00500,  .00601,  .00753,  .01595,  .02943,  .00994,  .01367,   exdt3210
     4  .01671,  .02538,  .03481,  .03405,  .03601,  .01608,  .01310,   exdt3220
     5  .01152,  .01082,  .01070,  .01563,  .02063,  .03171,  .03810,   exdt3230
     6  .03741,  .03804,  .03759,  .04209,  .07892,                     exdt3240
     7  .04347,  .03754,  .02269,  .01649,  .00917,  .00657, 0.0/       exdt3250
      DATA OCNA2 /                                                      exdt3260
     1  .23367,  .03127,  .02070,  .01297,  .01063,  .01285,  .01190,   exdt3270
     2  .00937,  .00911,  .01576,  .05576,  .23487,  .03949,  .00905,   exdt3280
     3  .02057,  .01816,  .01665,  .08025,  .08044,  .03677,  .03139,   exdt3290
     4  .03190,  .03766,  .04532,  .04544,  .04715,  .03405,  .03614,   exdt3300
     5  .04329,  .05424,  .07823,  .09728,  .10057,  .10247,  .10222,   exdt3310
     6  .09551,  .08241,  .07158,  .06506,  .09203,                     exdt3320
     7  .06133,  .05332,  .03258,  .02366,  .01308,  .00932, 0.0/       exdt3330
      DATA OCNA3 /                                                      exdt3340
     1  .13025,  .01557,  .01013,  .00646,  .00532,  .00665,  .00722,   exdt3350
     2  .01335,  .00728,  .01810,  .09835,  .37329,  .09703,  .01968,   exdt3360
     3  .05114,  .04342,  .03709,  .17456,  .16468,  .08785,  .06880,   exdt3370
     4  .06589,  .06791,  .07247,  .07329,  .07449,  .07025,  .07962,   exdt3380
     5  .09899,  .12481,  .17867,  .22019,  .22228,  .22051,  .21595,   exdt3390
     6  .20335,  .17278,  .14677,  .12171,  .12430,                     exdt3400
     7  .10890,  .09644,  .06106,  .04465,  .02457,  .01732, 0.0/       exdt3410
      DATA OCNA4 /                                                      exdt3420
     1  .03506,  .00323,  .00215,  .00139,  .00114,  .00171,  .00532,   exdt3430
     2  .03082,  .01101,  .03741,  .20101,  .47608,  .21165,  .05234,   exdt3440
     3  .12886,  .11215,  .09684,  .32810,  .31778,  .20513,  .16658,   exdt3450
     4  .15956,  .15842,  .15905,  .15968,  .16051,  .16506,  .18323,   exdt3460
     5  .21709,  .25652,  .33222,  .39639,  .39854,  .40297,  .40025,   exdt3470
     6  .39025,  .35468,  .32006,  .27715,  .25348,                     exdt3480
     7  .25632,  .23876,  .17092,  .13198,  .07692,  .05407, 0.0/       exdt3490
      DATA OCNG1 /                                                      exdt3500
     1  .7516,   .6960,   .6920,   .6756,   .6767,   .6844,   .6936,    exdt3510
     2  .7055,   .7110,   .7177,   .7367,   .6287,   .6779,   .6784,    exdt3520
     3  .6599,   .6659,   .6859,   .6887,   .6095,   .6558,   .6665,    exdt3530
     4  .6697,   .6594,   .5851,   .5644,   .5760,   .5903,   .5991,    exdt3540
     5  .6024,   .5979,   .6087,   .5837,   .5763,   .5348,   .4955,    exdt3550
     6  .4821,   .4635,   .4373,   .3944,   .2344,   .2754,   .2447,    exdt3560
     7  .2266,   .2088,   .1766,   .1481,   .0000/                      exdt3570
      DATA OCNG2 /                                                      exdt3580
     1  .7708,   .7288,   .7243,   .7214,   .7211,   .7330,   .7445,    exdt3590
     2  .7579,   .7649,   .7790,   .8182,   .7673,   .7171,   .7205,    exdt3600
     3  .7235,   .7251,   .7397,   .7537,   .6934,   .7137,   .7193,    exdt3610
     4  .7206,   .7151,   .6732,   .6620,   .6696,   .6821,   .6895,    exdt3620
     5  .6898,   .6819,   .6556,   .5925,   .5869,   .5511,   .5284,    exdt3630
     6  .5124,   .4912,   .4646,   .4302,   .3124,   .3101,   .2752,    exdt3640
     7  .2529,   .2335,   .2021,   .1738,   .0000/                      exdt3650
      DATA OCNG3 /                                                      exdt3660
     1  .7954,   .7782,   .7752,   .7717,   .7721,   .7777,   .7872,    exdt3670
     2  .8013,   .8089,   .8301,   .8844,   .8332,   .7557,   .7597,    exdt3680
     3  .7823,   .7822,   .7944,   .8157,   .7712,   .7738,   .7784,    exdt3690
     4  .7807,   .7800,   .7682,   .7659,   .7692,   .7780,   .7828,    exdt3700
     5  .7776,   .7621,   .7115,   .6342,   .6294,   .5999,   .5854,    exdt3710
     6  .5700,   .5512,   .5265,   .4996,   .4236,   .3765,   .3357,    exdt3720
     7  .3066,   .2830,   .2466,   .2184,   .0000/                      exdt3730
      DATA OCNG4 /                                                      exdt3740
     1  .8208,   .8270,   .8260,   .8196,   .8176,   .8096,   .8096,    exdt3750
     2  .8202,   .8255,   .8520,   .9228,   .8950,   .7965,   .7847,    exdt3760
     3  .8242,   .8244,   .8376,   .8857,   .8463,   .8332,   .8379,    exdt3770
     4  .8441,   .8467,   .8502,   .8534,   .8562,   .8688,   .8789,    exdt3780
     5  .8785,   .8683,   .8252,   .7562,   .7519,   .7261,   .7141,    exdt3790
     6  .6980,   .6789,   .6540,   .6294,   .5783,   .5100,   .4595,    exdt3800
     7  .4164,   .3868,   .3404,   .3042,   .0000/                      exdt3810
      DATA TROE1 /                                                      exdt3820
     1 2.21222, 1.82753, 1.67032, 1.00000,  .72424,  .35272,  .15234,   exdt3830
     2  .05165,  .03861,  .02994,  .04671,  .02462,  .01538,  .01146,   exdt3840
     3  .01032,  .00816,  .00861,  .00994,  .01057,  .01139,  .01747,   exdt3850
     4  .01494,  .02418,  .03165,  .03386,  .04247,  .01601,  .01215,   exdt3860
     5  .00937,  .00861,  .00823,  .01139,  .01924,  .01234,  .01348,   exdt3870
     6  .01114,  .01297,  .01266,  .01418,  .01487,                     exdt3880
     7  .01543,  .01321,  .00793,  .00582,  .00330,  .00239, 0.0/       exdt3890
      DATA TROE2 /                                                      exdt3900
     1 2.21519, 1.82266, 1.66557, 1.00000,  .72525,  .35481,  .15449,   exdt3910
     2  .05475,  .04044,  .03082,  .04620,  .05272,  .01867,  .01266,   exdt3920
     3  .01127,  .00886,  .00886,  .01449,  .01399,  .01228,  .01728,   exdt3930
     4  .01475,  .02285,  .03215,  .03494,  .04285,  .01652,  .01304,   exdt3940
     5  .01101,  .01120,  .01297,  .01753,  .02468,  .01741,  .01766,   exdt3950
     6  .01513,  .01557,  .01456,  .01532,  .01582,                     exdt3960
     7  .01619,  .01386,  .00832,  .00610,  .00346,  .00251, 0.0/       exdt3970
      DATA TROE3 /                                                      exdt3980
     1 2.19082, 1.79462, 1.64456, 1.00000,  .73297,  .36443,  .16278,   exdt3990
     2  .06468,  .04658,  .03399,  .04538,  .11892,  .02835,  .01646,   exdt4000
     3  .01386,  .01076,  .00968,  .02551,  .02222,  .01468,  .01690,   exdt4010
     4  .01437,  .01994,  .03127,  .03513,  .04076,  .01722,  .01513,   exdt4020
     5  .01519,  .01791,  .02538,  .03272,  .03816,  .03038,  .02886,   exdt4030
     6  .02551,  .02228,  .01937,  .01804,  .01791,                     exdt4040
     7  .01798,  .01539,  .00924,  .00678,  .00384,  .00278, 0.0/       exdt4050
      DATA TROE4 /                                                      exdt4060
     1 1.75696, 1.54829, 1.45962, 1.00000,  .77816,  .43139,  .21778,   exdt4070
     2  .11329,  .08101,  .05506,  .04943,  .25291,  .06816,  .03703,   exdt4080
     3  .02601,  .01968,  .01468,  .04962,  .04247,  .02234,  .01797,   exdt4090
     4  .01532,  .01633,  .02259,  .02487,  .02595,  .01728,  .01892,   exdt4100
     5  .02399,  .03247,  .05285,  .06462,  .06608,  .05930,  .05525,   exdt4110
     6  .04861,  .03753,  .02968,  .02348,  .02165,                     exdt4120
     7  .02152,  .01841,  .01104,  .00809,  .00458,  .00332, 0.0/       exdt4130
      DATA TROA1 /                                                      exdt4140
     1  .69671,  .09905,  .06563,  .04101,  .03354,  .03627,  .02810,   exdt4150
     2  .00873,  .00918,  .00930,  .03215,  .01285,  .00513,  .00316,   exdt4160
     3  .00557,  .00494,  .00646,  .00867,  .00937,  .01025,  .01646,   exdt4170
     4  .01481,  .02418,  .02886,  .03070,  .04032,  .01494,  .01139,   exdt4180
     5  .00873,  .00816,  .00797,  .01133,  .01911,  .01215,  .01329,   exdt4190
     6  .01101,  .01291,  .01266,  .01418,  .01487,                     exdt4200
     7  .01543,  .01321,  .00793,  .00582,  .00330,  .00239, 0.0/       exdt4210
      DATA TROA2 /                                                      exdt4220
     1  .65000,  .08791,  .05816,  .03652,  .02994,  .03278,  .02557,   exdt4230
     2  .00810,  .00842,  .00867,  .03139,  .03949,  .00646,  .00316,   exdt4240
     3  .00595,  .00519,  .00646,  .01304,  .01247,  .01095,  .01620,   exdt4250
     4  .01449,  .02278,  .02930,  .03184,  .04063,  .01544,  .01234,   exdt4260
     5  .01044,  .01076,  .01272,  .01741,  .02462,  .01722,  .01747,   exdt4270
     6  .01506,  .01551,  .01456,  .01532,  .01582,                     exdt4280
     7  .01619,  .01386,  .00832,  .00610,  .00346,  .00251, 0.0/       exdt4290
      DATA TROA3 /                                                      exdt4300
     1  .52804,  .06367,  .04158,  .02633,  .02184,  .02443,  .01937,   exdt4310
     2  .00658,  .00646,  .00709,  .02949,  .10013,  .00968,  .00310,   exdt4320
     3  .00677,  .00582,  .00646,  .02361,  .01994,  .01266,  .01544,   exdt4330
     4  .01386,  .01968,  .02848,  .03203,  .03854,  .01620,  .01449,   exdt4340
     5  .01462,  .01747,  .02513,  .03253,  .03797,  .03019,  .02861,   exdt4350
     6  .02538,  .02215,  .01930,  .01797,  .01791,                     exdt4360
     7  .01797,  .01539,  .00924,  .00677,  .00384,  .00278, 0.0/       exdt4370
      DATA TROA4 /                                                      exdt4380
     1  .19829,  .01842,  .01215,  .00791,  .00665,  .00778,  .00652,   exdt4390
     2  .00361,  .00253,  .00399,  .02570,  .20690,  .01715,  .00316,   exdt4400
     3  .00873,  .00728,  .00658,  .04481,  .03525,  .01646,  .01405,   exdt4410
     4  .01310,  .01468,  .01956,  .02184,  .02367,  .01608,  .01816,   exdt4420
     5  .02342,  .03203,  .05234,  .06399,  .06538,  .05867,  .05456,   exdt4430
     6  .04810,  .03715,  .02949,  .02335,  .02158,                     exdt4440
     7  .02149,  .01840,  .01104,  .00809,  .00458,  .00332, 0.0/       exdt4450
      DATA TROG1 /                                                      exdt4460
     1  .7518,   .6710,   .6638,   .6345,   .6152,   .5736,   .5280,    exdt4470
     2  .4949,   .4700,   .4467,   .4204,   .4028,   .3777,   .3563,    exdt4480
     3  .3150,   .2919,   .2695,   .2465,   .2402,   .2313,   .2101,    exdt4490
     4  .1760,   .1532,   .2091,   .2079,   .1843,   .1811,   .1687,    exdt4500
     5  .1626,   .1526,   .1356,   .1030,   .0962,   .1024,   .1086,    exdt4510
     6  .0928,   .0836,   .0643,   .0451,   .0290,   .0156,   .0118,    exdt4520
     7  .0076,   .0050,   .0024,   .0015,   .0000/                      exdt4530
      DATA TROG2 /                                                      exdt4540
     1  .7571,   .6858,   .6790,   .6510,   .6315,   .5887,   .5418,    exdt4550
     2  .5075,   .4829,   .4598,   .4338,   .4043,   .3890,   .3680,    exdt4560
     3  .3259,   .3026,   .2800,   .2541,   .2494,   .2414,   .2196,    exdt4570
     4  .1873,   .1657,   .2123,   .2110,   .1890,   .1836,   .1709,    exdt4580
     5  .1640,   .1534,   .1354,   .1044,   .0984,   .1026,   .1073,    exdt4590
     6  .0935,   .0842,   .0661,   .0477,   .0309,   .0171,   .0129,    exdt4600
     7  .0084,   .0056,   .0027,   .0017,   .0000/                      exdt4610
      DATA TROG3 /                                                      exdt4620
     1  .7667,   .7176,   .7128,   .6879,   .6690,   .6255,   .5769,    exdt4630
     2  .5403,   .5167,   .4947,   .4703,   .4143,   .4190,   .3993,    exdt4640
     3  .3563,   .3325,   .3095,   .2767,   .2751,   .2693,   .2464,    exdt4650
     4  .2175,   .1992,   .2247,   .2215,   .2042,   .1952,   .1814,    exdt4660
     5  .1726,   .1604,   .1398,   .1111,   .1065,   .1068,   .1086,    exdt4670
     6  .0984,   .0888,   .0724,   .0549,   .0358,   .0216,   .0166,    exdt4680
     7  .0109,   .0073,   .0036,   .0023,   .0000/                      exdt4690
      DATA TROG4 /                                                      exdt4700
     1  .7696,   .7719,   .7710,   .7606,   .7478,   .7142,   .6727,    exdt4710
     2  .6381,   .6201,   .6050,   .5912,   .4849,   .5137,   .5019,    exdt4720
     3  .4625,   .4389,   .4169,   .3696,   .3707,   .3708,   .3473,    exdt4730
     4  .3232,   .3112,   .3022,   .2938,   .2850,   .2675,   .2494,    exdt4740
     5  .2347,   .2165,   .1857,   .1536,   .1509,   .1441,   .1416,    exdt4750
     6  .1354,   .1245,   .1088,   .0905,   .0614,   .0440,   .0354,    exdt4760
     7  .0257,   .0179,   .0089,   .0059,   .0000/                      exdt4770
      DATA FG1EXT /                                                     exdt4780
     1  .98519,  .99158,  .99089, 1.00000, 1.00576, 1.01747, 1.03177,   exdt4790
     2 1.04146, 1.04696, 1.05323, 1.05886, 1.04899, 1.06823, 1.07804,   exdt4800
     3 1.09272, 1.10367, 1.11684, 1.10430, 1.11367, 1.12899, 1.14987,   exdt4810
     4 1.17209, 1.18278, 1.20133, 1.21266, 1.21949, 1.22677, 1.15589,   exdt4820
     5 1.05684,  .98291, 1.01120, 1.10911, 1.11462, 1.14671, 1.16247,   exdt4830
     6 1.18540, 1.21580, 1.24610, 1.26840, 1.20500, 1.20850, 1.23340,   exdt4840
     7 1.19560, 1.06530,  .68949,  .42888, 0.00000/                     exdt4850
      DATA FG1ABS /                                                     exdt4860
     1  .00013, 0.00000, 0.00000, 0.00000, 0.00000,  .00095,  .01513,   exdt4870
     2  .10861,  .03892,  .13272,  .47133,  .49696,  .45785,  .17918,   exdt4880
     3  .37373,  .34601,  .31867,  .55190,  .55025,  .49987,  .46342,   exdt4890
     4  .45943,  .45918,  .46089,  .46241,  .46386,  .47196,  .48905,   exdt4900
     5  .51468,  .53101,  .55266,  .58665,  .58899,  .60367,  .61158,   exdt4910
     6  .62335,  .64120,  .65627,  .66278,  .66393,  .69344,  .71087,   exdt4920
     7  .67625,  .61180,  .42130,  .29086, 0.00000/                     exdt4930
      DATA FG1SYM /                                                     exdt4940
     1  .8578,   .8726,   .8722,   .8717,   .8703,   .8652,   .8618,    exdt4950
     2  .8798,   .8689,   .8918,   .9641,   .9502,   .9297,   .8544,    exdt4960
     3  .9007,   .8885,   .8812,   .9604,   .9470,   .9193,   .9039,    exdt4970
     4  .9039,   .9057,   .9110,   .9158,   .9194,   .9381,   .9537,    exdt4980
     5  .9595,   .9587,   .9418,   .9101,   .9081,   .8957,   .8898,    exdt4990
     6  .8812,   .8685,   .8491,   .8246,   .7815,   .7148,   .6480,    exdt5000
     7  .5481,   .4725,   .3457,   .2575,   .0000/                      exdt5010
      DATA FG2EXT /                                                     exdt5020
     1  .94791,  .96215,  .97063, 1.00000, 1.00937, 1.05177, 1.12519,   exdt5030
     2 1.29570, 1.39203, 1.41120, 1.04715, 1.10816, 1.43285, 1.45272,   exdt5040
     3 1.18709, 1.04367,  .82354,  .71747,  .92405,  .79342,  .60266,   exdt5050
     4  .47677,  .43171,  .36734,  .33259,  .31184,  .24139,  .21601,   exdt5060
     5  .24006,  .28816,  .42671,  .56861,  .57266,  .58089,  .57165,   exdt5070
     6  .54247,  .43983,  .34475,  .24907,  .19291,  .18500,  .15586,   exdt5080
     7  .09047,  .06445,  .03533,  .02529, 0.00000/                     exdt5090
      DATA FG2ABS /                                                     exdt5100
     1 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,  .00013,  .00247,   exdt5110
     2  .01987,  .00620,  .02323,  .17209,  .57930,  .19810,  .03475,   exdt5120
     3  .09639,  .08000,  .06582,  .34589,  .32703,  .17025,  .12633,   exdt5130
     4  .11816,  .11627,  .11519,  .11538,  .11601,  .12329,  .14468,   exdt5140
     5  .18633,  .24057,  .35411,  .44886,  .45095,  .45215,  .44278,   exdt5150
     6  .41778,  .34433,  .27826,  .21066,  .17864,  .17626,  .15028,   exdt5160
     7  .08844,  .06358,  .03515,  .02523, 0.00000/                     exdt5170
      DATA FG2SYM /                                                     exdt5180
     1  .8388,   .8459,   .8419,   .8286,   .8224,   .7883,   .7763,    exdt5190
     2  .8133,   .8393,   .8767,   .9258,   .8982,   .7887,   .8082,    exdt5200
     3  .8319,   .8243,   .8210,   .8282,   .8037,   .7904,   .7728,    exdt5210
     4  .7528,   .7436,   .7274,   .7171,   .7100,   .6790,   .6520,    exdt5220
     5  .6305,   .6020,   .5475,   .4577,   .4511,   .4084,   .3872,    exdt5230
     6  .3566,   .2976,   .2340,   .1711,   .0956,   .0623,   .0454,    exdt5240
     7  .0286,   .0190,   .0090,   .0052,   .0000/                      exdt5250
      DATA BSTEXT /                                                     exdt5260
     E 2.0752E0,  1.8656E0,  1.7246E0,  1.0000E0,  7.0156E-1, 3.0170E-1,exdt5270
     E 1.1440E-1, 5.1225E-2, 3.4285E-2, 2.3475E-2, 1.6878E-2, 6.6506E-2,exdt5280
     E 1.0943E-1, 8.9653E-2, 6.7609E-2, 5.2855E-2, 6.7496E-2, 5.7975E-2,exdt5290
     E 4.2471E-2, 2.4176E-2, 4.6102E-2, 1.2339E-1, 1.7699E-1, 1.2389E-1,exdt5300
     E 9.0220E-2, 8.5793E-2, 3.2838E-2, 2.6528E-2, 5.0703E-2, 1.9471E-2,exdt5310
     E 1.1710E-2, 1.6106E-2, 1.7716E-2, 3.9533E-2, 3.7954E-2, 5.4871E-3,exdt5320
     E 8.8409E-3, 1.2289E-3, 1.0647E-3, 3.3151E-3, 4.5164E-3, 4.1496E-3,exdt5330
     E 3.2801E-3, 2.4481E-3, 1.0714E-3, 5.0381E-4, 1.2101E-4/           exdt5340
      DATA BSTABS /                                                     exdt5350
     A 2.4347E-7, 1.4949E-7, 1.3020E-7, 6.8716E-8, 1.0053E-7, 4.2384E-6,exdt5360
     A 2.3075E-4, 1.4889E-3, 1.8377E-3, 3.3645E-3, 4.6858E-3, 5.9424E-2,exdt5370
     A 1.0129E-1, 8.2396E-2, 6.3621E-2, 5.0327E-2, 6.5798E-2, 5.6149E-2,exdt5380
     A 4.0939E-2, 2.3226E-2, 4.5800E-2, 1.2276E-1, 1.7568E-1, 1.2216E-1,exdt5390
     A 8.9058E-2, 8.4861E-2, 3.1979E-2, 2.6097E-2, 5.0244E-2, 1.9042E-2,exdt5400
     A 1.1497E-2, 1.6024E-2, 1.7639E-2, 3.9452E-2, 3.7847E-2, 5.4251E-3,exdt5410
     A 8.8160E-3, 1.2118E-3, 1.0579E-3, 3.3122E-3, 4.5150E-3, 4.1489E-3,exdt5420
     A 3.2799E-3, 2.4481E-3, 1.0713E-3, 5.0381E-4, 1.2101E-4/           exdt5430
      DATA BSTSYM /                                                     exdt5440
     G 0.6749, 0.6943, 0.6991, 0.6846, 0.6572, 0.5861, 0.4965, 0.4164,  exdt5450
     G 0.3772, 0.3385, 0.3069, 0.2599, 0.2234, 0.2028, 0.1554, 0.1291,  exdt5460
     G 0.1057, 0.0962, 0.0909, 0.0802, 0.0595, 0.0458, 0.0413, 0.0479,  exdt5470
     G 0.0483, 0.0451, 0.0504, 0.0379, 0.0346, 0.0365, 0.0273, 0.0180,  exdt5480
     G 0.0174, 0.0144, 0.0151, 0.0141, 0.0094, 0.0075, 0.0049, 0.0031,  exdt5490
     G 0.0020, 0.0014, 0.0008, 0.0005, 0.0002, 0.0001, 0.0001/          exdt5500
      DATA AVOEXT /                                                     exdt5510
     1 1.14880, 1.19171, 1.18013, 1.00000,  .84873,  .53019,  .27968,   exdt5520
     2  .14551,  .11070,  .08633,  .07184,  .06076,  .04506,  .03399,   exdt5530
     3  .02095,  .01538,  .01266,  .01019,  .00994,  .01044,  .01361,   exdt5540
     4  .01791,  .02278,  .02918,  .03108,  .03234,  .03456,  .03184,   exdt5550
     5  .02772,  .02475,  .01715,  .01563,  .01665,  .01646,  .01734,   exdt5560
     6  .01772,  .01076,  .01051,  .01133,  .01329,                     exdt5570
     7  .01492,  .01277,  .00766,  .00562,  .00318,  .00231, 0.0/       exdt5580
      DATA AVOABS /                                                     exdt5590
     1  .44816,  .11259,  .08500,  .05272,  .04082,  .02449,  .01487,   exdt5600
     2  .01019,  .00867,  .00842,  .00842,  .00949,  .00741,  .00487,   exdt5610
     3  .00316,  .00335,  .00399,  .00449,  .00525,  .00665,  .01114,   exdt5620
     4  .01652,  .02177,  .02437,  .02506,  .02658,  .03006,  .02861,   exdt5630
     5  .02513,  .02285,  .01620,  .01532,  .01633,  .01620,  .01709,   exdt5640
     6  .01741,  .01057,  .01038,  .01127,  .01329,                     exdt5650
     7  .01492,  .01277,  .00766,  .00562,  .00318,  .00231, 0.0/       exdt5660
      DATA AVOSYM /                                                     exdt5670
     1  .8272,   .7148,   .7076,   .6978,   .6886,   .6559,   .6062,    exdt5680
     2  .5561,   .5255,   .4958,   .4729,   .4401,   .4015,   .3699,    exdt5690
     3  .3125,   .2773,   .2472,   .2173,   .2054,   .1908,   .1623,    exdt5700
     4  .1348,   .1233,   .1615,   .1757,   .1712,   .1521,   .1326,    exdt5710
     5  .1230,   .1081,   .0801,   .0528,   .0514,   .0461,   .0446,    exdt5720
     6  .0449,   .0415,   .0330,   .0198,   .0097,   .0044,   .0032,    exdt5730
     7  .0020,   .0013,   .0006,   .0004,   .0000/                      exdt5740
      DATA FVOEXT /                                                     exdt5750
     1  .88715,  .92532,  .94013, 1.00000, 1.03013, 1.05975, 1.01171,   exdt5760
     2  .88677,  .82538,  .76361,  .71563,  .67424,  .60589,  .55057,   exdt5770
     3  .45222,  .37646,  .32316,  .25519,  .22728,  .20525,  .17810,   exdt5780
     4  .14481,  .14152,  .37639,  .44551,  .44405,  .42222,  .36462,   exdt5790
     5  .32551,  .27519,  .16728,  .10627,  .10861,  .10886,  .11665,   exdt5800
     6  .13127,  .10108,  .08557,  .06411,  .05741,                     exdt5810
     7  .05531,  .04707,  .02792,  .02028,  .01136,  .00820, 0.0/       exdt5820
      DATA FVOABS /                                                     exdt5830
     1  .41582,  .22892,  .19108,  .14468,  .12475,  .09158,  .06601,   exdt5840
     2  .04943,  .04367,  .04342,  .04399,  .05076,  .04133,  .02829,   exdt5850
     3  .01924,  .01981,  .02297,  .02475,  .02778,  .03411,  .05335,   exdt5860
     4  .07133,  .08816,  .15342,  .18506,  .19354,  .20791,  .18449,   exdt5870
     5  .16101,  .13759,  .08456,  .06886,  .07278,  .07367,  .07956,   exdt5880
     6  .08785,  .06032,  .05747,  .05133,  .05323,                     exdt5890
     7  .05453,  .04657,  .02773,  .02020,  .01135,  .00820, 0.0/       exdt5900
      DATA FVOSYM /                                                     exdt5910
     1  .9295,   .8115,   .7897,   .7473,   .7314,   .7132,   .7113,    exdt5920
     2  .7238,   .7199,   .7165,   .7134,   .6989,   .6840,   .6687,    exdt5930
     3  .6409,   .6325,   .6199,   .6148,   .6142,   .6072,   .5853,    exdt5940
     4  .5632,   .5486,   .4753,   .4398,   .4329,   .4091,   .4105,    exdt5950
     5  .4120,   .4136,   .4140,   .3637,   .3577,   .3344,   .3220,    exdt5960
     6  .3052,   .2957,   .2564,   .2055,   .1229,   .0632,   .0483,    exdt5970
     7  .0321,   .0216,   .0103,   .0059,   .0000/                      exdt5980
      DATA DMEEXT /                                                     exdt5990
     1 1.05019, 1.05880, 1.05259, 1.00000,  .94949,  .81456,  .66051,   exdt6000
     2  .54380,  .49133,  .44677,  .41671,  .38063,  .34778,  .32804,   exdt6010
     3  .29722,  .27506,  .25082,  .22620,  .21652,  .20253,  .17266,   exdt6020
     4  .14905,  .14234,  .14082,  .15057,  .16399,  .23608,  .24481,   exdt6030
     5  .27791,  .25076,  .15272,  .09601,  .09456,  .14576,  .12373,   exdt6040
     6  .18348,  .12190,  .12924,  .08538,  .04108,                     exdt6050
     7  .04714,  .04069,  .02480,  .01789,  .00980,  .00693, 0.0/       exdt6060
      DATA DMEABS /                                                     exdt6070
     1  .00063,  .00152,  .00184,  .00506,  .00791,  .01829,  .03728,   exdt6080
     2  .06158,  .07538,  .08943,  .10051,  .11614,  .13310,  .14348,   exdt6090
     3  .14633,  .13728,  .12462,  .11184,  .10709,  .10076,  .09006,   exdt6100
     4  .08734,  .09000,  .10304,  .11905,  .13437,  .19551,  .20095,   exdt6110
     5  .22494,  .18418,  .09285,  .06665,  .06823,  .12329,  .10551,   exdt6120
     6  .16184,  .09835,  .10582,  .06759,  .03247,                     exdt6130
     7  .04405,  .03816,  .02327,  .01696,  .00946,  .00677, 0.0/       exdt6140
      DATA DMESYM /                                                     exdt6150
     1  .7173,   .7039,   .7020,   .6908,   .6872,   .6848,   .6891,    exdt6160
     2  .6989,   .7046,   .7099,   .7133,   .7159,   .7134,   .7058,    exdt6170
     3  .6827,   .6687,   .6583,   .6513,   .6494,   .6475,   .6467,    exdt6180
     4  .6496,   .6506,   .6461,   .6334,   .6177,   .5327,   .5065,    exdt6190
     5  .4632,   .4518,   .5121,   .5450,   .5467,   .4712,   .4853,    exdt6200
     6  .3984,   .4070,   .3319,   .3427,   .3766,   .3288,   .2969,    exdt6210
     7  .2808,   .2661,   .2409,   .2098,   .0000/                      exdt6220
      DATA CCUEXT /                                                     exdt6230
     1  .98081,  .98746,  .98915, 1.00000, 1.00650, 1.02230, 1.04180,   exdt6240
     2 1.05830, 1.06780, 1.07870, 1.09780, 1.06440, 1.09750, 1.11300,   exdt6250
     3 1.14320, 1.16660, 1.20540, 1.15420, 1.17610, 1.21910, 1.26990,   exdt6260
     4 1.30300, 1.31090, 1.31060, 1.29940, 1.28640, 1.16620,  .98693,   exdt6270
     5  .88130,  .83429,  .92012, 1.07340, 1.08150, 1.12680, 1.14770,   exdt6280
     6 1.17600, 1.19210, 1.19120, 1.14510,  .97814,  .96308,  .94390,   exdt6290
     7  .75994,  .56647,  .26801,  .15748, 0.00000/                     exdt6300
      DATA CCUABS /                                                     exdt6310
     1  .00007,  .00001,  .00000,  .00000,  .00001,  .00059,  .00956,   exdt6320
     2  .07224,  .02502,  .08913,  .41512,  .51824,  .41304,  .12614,   exdt6330
     3  .29826,  .26739,  .23672,  .55428,  .55642,  .44494,  .38433,   exdt6340
     4  .37277,  .37000,  .36872,  .36896,  .36984,  .37868,  .40498,   exdt6350
     5  .44993,  .48941,  .54799,  .60964,  .61302,  .63227,  .64074,   exdt6360
     6  .65112,  .65367,  .64760,  .61924,  .59000,  .61601,  .61058,   exdt6370
     7  .49236,  .38532,  .20641,  .13474, 0.00000/                     exdt6380
      DATA CCUSYM /                                                     exdt6390
     1  .8557,   .8676,   .8680,   .8658,   .8630,   .8557,   .8496,    exdt6400
     2  .8566,   .8464,   .8627,   .9417,   .9458,   .8891,   .8136,    exdt6410
     3  .8503,   .8400,   .8453,   .9428,   .9168,   .8759,   .8733,    exdt6420
     4  .8841,   .8894,   .8986,   .9044,   .9082,   .9239,   .9342,    exdt6430
     5  .9367,   .9331,   .9119,   .8719,   .8692,   .8515,   .8424,    exdt6440
     6  .8287,   .8059,   .7742,   .7354,   .6554,   .5557,   .4720,    exdt6450
     7  .3713,   .2990,   .1846,   .1156,   .0000/                      exdt6460
      DATA CALEXT /                                                     exdt6470
     1  .97331,  .98106,  .98472, 1.00000, 1.00850, 1.03090, 1.05770,   exdt6480
     2 1.08070, 1.09390, 1.11530, 1.20260, 1.08250, 1.13480, 1.16770,   exdt6490
     3 1.26750, 1.33520, 1.41110, 1.18200, 1.28390, 1.38040, 1.38430,   exdt6500
     4 1.31200, 1.26540, 1.17160, 1.10410, 1.05640,  .83383,  .66530,   exdt6510
     5  .61995,  .62907,  .77190,  .96660,  .97609, 1.02520, 1.04380,   exdt6520
     6 1.06270, 1.02550,  .95714,  .82508,  .63464,  .60962,  .54998,   exdt6530
     7  .34165,  .22587,  .10647,  .07067, 0.00000/                     exdt6540
      DATA CALABS /                                                     exdt6550
     1  .00004,  .00000,  .00000,  .00000,  .00000,  .00036,  .00607,   exdt6560
     2  .04771,  .01579,  .05734,  .33199,  .54434,  .35157,  .08528,   exdt6570
     3  .21785,  .18813,  .15982,  .52068,  .52125,  .35294,  .28359,   exdt6580
     4  .26999,  .26668,  .26477,  .26484,  .26565,  .27546,  .30540,   exdt6590
     5  .36011,  .41780,  .51479,  .60420,  .60818,  .62781,  .63339,   exdt6600
     6  .63544,  .60762,  .56843,  .50067,  .44739,  .45910,  .42486,   exdt6610
     7  .27527,  .19352,  .09932,  .06832, 0.00000/                     exdt6620
      DATA CALSYM /                                                     exdt6630
     1  .8523,   .8632,   .8623,   .8573,   .8532,   .8422,   .8297,    exdt6640
     2  .8252,   .8145,   .8317,   .9312,   .9383,   .8291,   .7640,    exdt6650
     3  .8202,   .8276,   .8547,   .9224,   .8859,   .8621,   .8706,    exdt6660
     4  .8780,   .8804,   .8833,   .8849,   .8858,   .8889,   .8899,    exdt6670
     5  .8872,   .8790,   .8513,   .7984,   .7944,   .7683,   .7545,    exdt6680
     6  .7333,   .6939,   .6405,   .5727,   .4313,   .3156,   .2437,    exdt6690
     7  .1693,   .1185,   .0574,   .0332,   .0000/                      exdt6700
      DATA CSTEXT /                                                     exdt6710
     1  .97430,  .98324,  .98570, 1.00000, 1.00890, 1.03100, 1.05590,   exdt6720
     2 1.08130, 1.09760, 1.12170, 1.16390, 1.07880, 1.13660, 1.16990,   exdt6730
     3 1.22930, 1.26720, 1.31080, 1.15290, 1.23270, 1.29770, 1.31180,   exdt6740
     4 1.27830, 1.25190, 1.19190, 1.14390, 1.10790,  .91743,  .74497,   exdt6750
     5  .68246,  .67604,  .80234,  .98329,  .99219, 1.03880, 1.05710,   exdt6760
     6 1.07730, 1.05460, 1.00640,  .90146,  .71967,  .69823,  .65179,   exdt6770
     7  .44906,  .30781,  .14114,  .08913, 0.00000/                     exdt6780
      DATA CSTABS /                                                     exdt6790
     1  .00005,  .00001,  .00000,  .00000,  .00000,  .00042,  .00681,   exdt6800
     2  .05317,  .01779,  .06484,  .35033,  .53843,  .36321,  .09457,   exdt6810
     3  .23629,  .20663,  .17789,  .52440,  .52484,  .37331,  .30681,   exdt6820
     4  .29375,  .29057,  .28880,  .28887,  .28969,  .29913,  .32789,   exdt6830
     5  .37961,  .43212,  .51866,  .60025,  .60398,  .62285,  .62874,   exdt6840
     6  .63229,  .61185,  .58151,  .52536,  .47993,  .49571,  .47074,   exdt6850
     7  .33104,  .24066,  .12346,  .08312, 0.00000/                     exdt6860
      DATA CSTSYM /                                                     exdt6870
     1  .8519,   .8633,   .8629,   .8590,   .8546,   .8432,   .8328,    exdt6880
     2  .8330,   .8251,   .8439,   .9332,   .9388,   .8422,   .7823,    exdt6890
     3  .8288,   .8291,   .8482,   .9255,   .8906,   .8613,   .8675,    exdt6900
     4  .8772,   .8810,   .8869,   .8905,   .8927,   .9016,   .9069,    exdt6910
     5  .9060,   .8989,   .8714,   .8204,   .8168,   .7932,   .7811,    exdt6920
     6  .7628,   .7319,   .6905,   .6401,   .5324,   .4233,   .3459,    exdt6930
     7  .2636,   .2027,   .1120,   .0663,   .0000/                      exdt6940
      DATA CSCEXT /                                                     exdt6950
     1  .96965,  .97960,  .98266, 1.00000, 1.01040, 1.03530, 1.06590,   exdt6960
     2 1.09980, 1.12280, 1.16020, 1.20330, 1.08630, 1.16840, 1.21860,   exdt6970
     3 1.28860, 1.32310, 1.33780, 1.11630, 1.24450, 1.30260, 1.26260,   exdt6980
     4 1.17670, 1.12990, 1.04180,  .98070,  .93828,  .74401,  .59962,   exdt6990
     5  .56489,  .57976,  .72193,  .90905,  .91772,  .96075,  .97500,   exdt7000
     6  .98623,  .93761,  .86388,  .73722,  .56926,  .54699,  .49341,   exdt7010
     7  .31131,  .20846,  .09872,  .06531, 0.00000/                     exdt7020
      DATA CSCABS /                                                     exdt7030
     1  .00004,  .00000,  .00000,  .00000,  .00000,  .00035,  .00553,   exdt7040
     2  .04382,  .01430,  .05271,  .30881,  .54982,  .32983,  .07796,   exdt7050
     3  .20033,  .17269,  .14662,  .49557,  .49304,  .32632,  .26104,   exdt7060
     4  .24829,  .24525,  .24349,  .24358,  .24437,  .25378,  .28239,   exdt7070
     5  .33510,  .39227,  .49203,  .58265,  .58638,  .60338,  .60677,   exdt7080
     6  .60472,  .56954,  .52556,  .45708,  .40717,  .41646,  .38375,   exdt7090
     7  .25009,  .17726,  .09148,  .06291, 0.00000/                     exdt7100
      DATA CSCSYM /                                                     exdt7110
     1  .8495,   .8597,   .8594,   .8535,   .8479,   .8349,   .8214,    exdt7120
     2  .8192,   .8151,   .8395,   .9321,   .9329,   .8156,   .7722,    exdt7130
     3  .8270,   .8319,   .8533,   .9138,   .8772,   .8562,   .8628,    exdt7140
     4  .8691,   .8713,   .8742,   .8759,   .8768,   .8805,   .8818,    exdt7150
     5  .8783,   .8685,   .8362,   .7776,   .7734,   .7458,   .7317,    exdt7160
     6  .7106,   .6738,   .6250,   .5655,   .4409,   .3338,   .2655,    exdt7170
     7  .1947,   .1427,   .0727,   .0422,   .0000/                      exdt7180
      DATA CNIEXT /                                                     exdt7190
     1  .97967,  .98623,  .98795, 1.00000, 1.00710, 1.02340, 1.04300,   exdt7200
     2 1.06100, 1.07130, 1.08440, 1.10650, 1.06540, 1.10200, 1.12040,   exdt7210
     3 1.15490, 1.17990, 1.21730, 1.15000, 1.18140, 1.22610, 1.26770,   exdt7220
     4 1.28840, 1.29070, 1.28200, 1.26650, 1.25130, 1.12860,  .95670,   exdt7230
     5  .85784,  .81564,  .90486, 1.05950, 1.06760, 1.11240, 1.13250,   exdt7240
     6 1.15910, 1.16960, 1.16290, 1.11130,  .94771,  .93251,  .91151,   exdt7250
     7  .73279,  .55018,  .26554,  .15656, 0.00000/                     exdt7260
      DATA CNIABS /                                                     exdt7270
     1  .00007,  .00001,  .00000,  .00000,  .00001,  .00058,  .00948,   exdt7280
     2  .07084,  .02436,  .08711,  .40714,  .52024,  .40688,  .12335,   exdt7290
     3  .29163,  .26107,  .23098,  .54886,  .55047,  .43579,  .37552,   exdt7300
     4  .36411,  .36140,  .36017,  .36043,  .36132,  .37019,  .39640,   exdt7310
     5  .44146,  .48184,  .54304,  .60651,  .60988,  .62882,  .63682,   exdt7320
     6  .64613,  .64572,  .63682,  .60584,  .57559,  .60014,  .59283,   exdt7330
     7  .47587,  .37364,  .20267,  .13269, 0.00000/                     exdt7340
      DATA CNISYM /                                                     exdt7350
     1  .8550,   .8670,   .8677,   .8645,   .8616,   .8538,   .8474,    exdt7360
     2  .8534,   .8439,   .8609,   .9411,   .9449,   .8822,   .8101,    exdt7370
     3  .8486,   .8403,   .8475,   .9405,   .9134,   .8749,   .8732,    exdt7380
     4  .8833,   .8882,   .8968,   .9025,   .9061,   .9217,   .9322,    exdt7390
     5  .9346,   .9308,   .9086,   .8669,   .8641,   .8457,   .8364,    exdt7400
     6  .8222,   .7992,   .7677,   .7298,   .6525,   .5558,   .4752,    exdt7410
     7  .3796,   .3105,   .1995,   .1287,   .0000/                      exdt7420
C          EXTINCTION  COEFFICIENTS                                     exdt7430
      DATA CI64XT/                                                      exdt7440
     1   9.947E-01,  9.968E-01,  9.972E-01,  1.000E+00,  1.002E+00,     exdt7450
     2   1.005E+00,  1.010E+00,  1.013E+00,  1.016E+00,  1.018E+00,     exdt7460
     3   1.019E+00,  1.016E+00,  1.023E+00,  1.026E+00,  1.030E+00,     exdt7470
     4   1.033E+00,  1.036E+00,  1.037E+00,  1.038E+00,  1.040E+00,     exdt7480
     5   1.043E+00,  1.047E+00,  1.049E+00,  1.051E+00,  1.052E+00,     exdt7490
     6   1.053E+00,  1.055E+00,  1.032E+00,  1.034E+00,  1.047E+00,     exdt7500
     7   1.060E+00,  1.074E+00,  1.075E+00,  1.081E+00,  1.085E+00,     exdt7510
     8   1.090E+00,  1.102E+00,  1.117E+00,  1.131E+00,  1.094E+00,     exdt7520
     9   1.168E+00,  1.187E+00,  1.244E+00,  1.297E+00,  1.475E+00,     exdt7530
     X   1.695E+00,  1.556E+00 /                                        exdt7540
C         ABSORPTION  COEFFICIENTS                                      exdt7550
      DATA CI64AB/                                                      exdt7560
     1   7.893E-05,  1.914E-05,  1.450E-05,  5.904E-06,  3.905E-05,     exdt7570
     2   1.917E-03,  2.604E-01,  3.732E-01,  8.623E-02,  2.253E-01,     exdt7580
     3   4.152E-01,  4.460E-01,  4.660E-01,  4.589E-01,  4.848E-01,     exdt7590
     4   4.786E-01,  4.915E-01,  4.944E-01,  4.936E-01,  4.947E-01,     exdt7600
     5   4.978E-01,  5.012E-01,  5.028E-01,  5.070E-01,  5.095E-01,     exdt7610
     6   5.111E-01,  5.205E-01,  5.126E-01,  4.969E-01,  4.868E-01,     exdt7620
     7   4.836E-01,  4.982E-01,  4.999E-01,  5.097E-01,  5.126E-01,     exdt7630
     8   5.188E-01,  5.108E-01,  4.915E-01,  5.559E-01,  5.515E-01,     exdt7640
     9   5.600E-01,  5.948E-01,  6.225E-01,  6.348E-01,  5.693E-01,     exdt7650
     X   3.306E-01,  8.661E-02 /                                        exdt7660
C         ASYMMETRY  PARAMETER  -  G                                    exdt7670
      DATA CI64G/                                                       exdt7680
     1   .8626,  .8824,  .8851,  .8893,  .8904,  .8913,  .9332,  .9549, exdt7690
     2   .9141,  .9407,  .9763,  .9428,  .9509,  .9580,  .9699,  .9679, exdt7700
     3   .9735,  .9737,  .9717,  .9712,  .9712,  .9715,  .9721,  .9744, exdt7710
     4   .9756,  .9764,  .9822,  .9849,  .9721,  .9530,  .9341,  .9352, exdt7720
     5   .9366,  .9426,  .9425,  .9448,  .9365,  .9256,  .9485,  .9417, exdt7730
     6   .8868,  .8983,  .8589,  .8115,  .6810,  .5923,  .5703 /        exdt7740
C          EXTINCTION COEFFICIENTS                                      exdt7750
      DATA CIR4XT/                                                      exdt7760
     1   9.685E-01,  9.803E-01,  9.826E-01,  1.000E+00,  1.011E+00,     exdt7770
     2   1.038E+00,  1.066E+00,  1.090E+00,  1.118E+00,  1.201E+00,     exdt7780
     3   1.374E+00,  1.019E+00,  1.143E+00,  1.198E+00,  1.331E+00,     exdt7790
     4   1.434E+00,  1.424E+00,  1.283E+00,  1.298E+00,  1.326E+00,     exdt7800
     5   1.287E+00,  1.230E+00,  1.191E+00,  1.048E+00,  9.634E-01,     exdt7810
     6   9.093E-01,  6.067E-01,  5.216E-01,  6.953E-01,  8.902E-01,     exdt7820
     7   1.083E+00,  1.228E+00,  1.214E+00,  1.076E+00,  1.032E+00,     exdt7830
     8   8.881E-01,  6.275E-01,  3.462E-01,  2.118E-01,  3.955E-01,     exdt7840
     9   5.089E-01,  3.012E-01,  1.235E-01,  5.377E-02,  2.068E-02,     exdt7850
     X   6.996E-03,  1.560E-03 /                                        exdt7860
C          ABSORPTION  COEFFICIENTS                                     exdt7870
      DATA CIR4AB/                                                      exdt7880
     1   5.316E-06,  1.461E-06,  9.045E-07,  4.431E-07,  2.746E-06,     exdt7890
     2   1.413E-04,  2.920E-02,  5.578E-02,  6.844E-03,  2.151E-02,     exdt7900
     3   6.322E-02,  5.051E-01,  4.578E-01,  1.360E-01,  3.269E-01,     exdt7910
     4   1.572E-01,  2.246E-01,  4.176E-01,  4.282E-01,  3.802E-01,     exdt7920
     5   3.517E-01,  3.037E-01,  2.543E-01,  2.410E-01,  2.432E-01,     exdt7930
     6   2.438E-01,  2.346E-01,  3.747E-01,  4.839E-01,  5.722E-01,     exdt7940
     7   6.368E-01,  5.303E-01,  5.085E-01,  3.920E-01,  3.437E-01,     exdt7950
     8   2.481E-01,  1.175E-01,  7.172E-02,  1.108E-01,  3.459E-01,     exdt7960
     9   4.044E-01,  2.545E-01,  9.594E-02,  4.410E-02,  1.887E-02,     exdt7970
     X   6.433E-03,  1.456E-03 /                                        exdt7980
C        ASYMMETRY  PARAMETER  -  G                                     exdt7990
      DATA CIR4G/                                                       exdt8000
     1   .8517,  .8654,  .8661,  .8615,  .8574,  .8447,  .8321,  .8248, exdt8010
     2   .8227,  .8612,  .9363,  .9231,  .8419,  .7550,  .8481,  .8358, exdt8020
     3   .8718,  .8953,  .8884,  .8786,  .8731,  .8660,  .8625,  .8652, exdt8030
     4   .8659,  .8658,  .8676,  .8630,  .8434,  .8194,  .7882,  .7366, exdt8040
     5   .7339,  .7161,  .7015,  .6821,  .6383,  .5823,  .4845,  .2977, exdt8050
     6   .2295,  .1716,  .1228,  .0748,  .0329,  .0186,  .0081 /        exdt8060
      END                                                               exdt8070
      SUBROUTINE FDBETA(H1,H2,BETA,ANGLE,PHI,LEN,                       fdbt 100
     $     HMIN,IERROR)                                                 fdbt 110
C********************************************************************** fdbt 120
C     GIVEN H1,H2,AND BETA (THE EARTH CENTERED ANGLE), THIS SUBROUTINE  fdbt 130
C     CALCULATES THE ZENITH ANGLE AT H1 (ANGLE) AND AT H2 (PHI).        fdbt 140
c     BASED ON A NEWTRON-RAPHSON METHOD.                                fdbt 150
C********************************************************************** fdbt 160
      REAL PI, CA, DEG, GCAIR, BIGNUM, BIGEXP, RE, DELTAS, ZMAX         fdbt 170
      INTEGER IRD, IPR, IPU, NPR, IPR1, IMAX, IMOD, IBMAX, IPATH,       fdbt 180
     $     LEN, IERROR, ITERMX, IFLAG, IORDER, IAMTB, ITER              fdbt 190
      DOUBLE PRECISION H1, H2, ANGLE, PHI, BETA,TOLRNC,                 fdbt 200
     $     HMIN, HA, HB, BETA1, RANGE, BENDNG,ANGLE1,                   fdbt 210
     $     RATIOA,RATIOB,STORE,DENOM,DERIV,APREV,AWGHTD,DBPREV,DBETA    fdbt 220
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON /PARMTR/ RE,DELTAS,ZMAX,IMAX,IMOD,IBMAX,IPATH              fdbt 240
      COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     fdbt 250
      DATA TOLRNC/2.0D-4/,ITERMX/35/                                    fdbt 260
C                                                                       fdbt 270
      IF(BETA.EQ.0.)THEN                                                fdbt 280
         LEN=0                                                          fdbt 290
         ANGLE=0.                                                       fdbt 300
         HMIN=H1                                                        fdbt 310
         IF(H1.GT.H2)THEN                                               fdbt 320
            ANGLE=180.                                                  fdbt 330
            HMIN=H2                                                     fdbt 340
         ENDIF                                                          fdbt 350
         PHI=180.-ANGLE                                                 fdbt 360
      ENDIF                                                             fdbt 370
      IF(H1.GT.H2)THEN                                                  fdbt 380
         HA=H2                                                          fdbt 390
         HB=H1                                                          fdbt 400
      ELSE                                                              fdbt 410
         HA=H1                                                          fdbt 420
         HB=H2                                                          fdbt 430
      ENDIF                                                             fdbt 440
C                                                                       fdbt 450
C***  SET PARAMETER TO SUPPRESS CALCULATION OF AMOUNTS IN ROUTINE LAYER fdbt 460
      IAMTB = 2                                                         fdbt 470
C                                                                       fdbt 480
C***  GUESS AT ANGLE,INTEGRATETO FIND BETA, TEST FOR CONVERGENCE, AND   fdbt 490
C***  ITERATE FIRST GUESS AT ANGLE: USE GEOMETRIC SOLN (NO REFRACTION)  fdbt 500
      WRITE(IPR,'(///30H CASE 2D: GIVEN H1, H2,  BETA:,//               fdbt 510
     $     42H ITERATE AROUND ANGLE UNTIL BETA CONVERGES,//             fdbt 520
     $     14H ITER    ANGLE,T21,4HBETA,T30,5HDBETA,T40,5HRANGE,T51,    fdbt 530
     $     4HHMIN,T61,3HPHI,T70,7HBENDING,/T10,5H(DEG),T21,5H(DEG),T30, fdbt 540
     $     5H(DEG),T41,4H(KM),T51,4H(KM),T60,5H(DEG),T71,5H(DEG),/)')   fdbt 550
C                                                                       fdbt 560
C     CALCULATE ANGLE1, A GUESS VALUE FOR ANGLE                         fdbt 570
      RATIOA=(HB-HA)/(RE+HA)                                            fdbt 580
      RATIOB=(HB-HA)/(RE+HB)                                            fdbt 590
      STORE=2.*SIN(.5*BETA/DEG)**2                                      fdbt 600
      DENOM=RATIOB-STORE                                                fdbt 610
      ANGLE1=90.                                                        fdbt 620
      IF(DENOM.NE.0.)ANGLE1=DEG*ATAN(SIN(BETA/DEG)/DENOM)               fdbt 630
      IF(ANGLE1.LT.0.)ANGLE1=ANGLE1+180.                                fdbt 640
C                                                                       fdbt 650
C     CALCULATE THE DERIVATIVE D(ANGLE)/D(BETA)                         fdbt 660
      DERIV=(RATIOA+STORE)/(RATIOA*RATIOB+2*STORE)                      fdbt 670
C                                                                       fdbt 680
C     DERIV TENDS TO OVERSHOOT VALUE.  FUDGE (=.6) SPEEDS UP CONVERGENCEfdbt 690
      DERIV=.6*DERIV                                                    fdbt 700
C                                                                       fdbt 710
C     BEGIN ITERATIVE PROCEDURE                                         fdbt 720
      ITER=0                                                            fdbt 730
 10   ITER=ITER+1                                                       fdbt 740
      IF(ITER.GT.ITERMX)THEN                                            fdbt 750
         WRITE(IPR,'(40H0FDBETA, CASE 2D (H1,H2,BETA): SOLUTION ,       fdbt 760
     $        16HDID NOT CONVERGE,//10X,4HH1 =,F13.6,4X,4HH2 =,F13.6,   fdbt 770
     $        4X,6HBETA =,F13.6,4X,12HITERATIONS =,I5,//10X,4HLAST,     fdbt 780
     $        10H ITERATION,//10X,7HANGLE =,F16.9,4X,6HBETA =,F16.9)')  fdbt 790
     $        H1,H2,BETA,ITER,ANGLE1,BETA1                              fdbt 800
         IERROR=1                                                       fdbt 810
         RETURN                                                         fdbt 820
      ENDIF                                                             fdbt 830
C                                                                       fdbt 840
C     DETERMINE BETA1, THE BETA CORRESPONDING TO ANGLE1                 fdbt 850
      CALL DPFNMN(HA,ANGLE1,HB,LEN,ITER,HMIN,PHI,IERROR)                fdbt 860
      CALL DPRFPA(HA,HB,ANGLE1,PHI,LEN,HMIN,                            fdbt 870
     $     IAMTB,BETA1,RANGE,BENDNG)                                    fdbt 880
      WRITE(IPR,'(I5,3F10.4,2F10.3,2F10.4)')                            fdbt 890
     $     ITER,ANGLE1,BETA1,BETA-BETA1,RANGE,HMIN,PHI,BENDNG           fdbt 900
      DBETA = BETA1-BETA                                                fdbt 910
      AWGHTD=(ANGLE1*ABS(DBETA)+APREV*ABS(DBPREV))                      fdbt 920
     $     /(ABS(DBETA)+ABS(DBPREV))                                    fdbt 930
C                                                                       fdbt 940
C     CHECK FOR CONVERGENCE                                             fdbt 950
      IF(ABS(BETA-BETA1).LT.TOLRNC)THEN                                 fdbt 960
         IF(HMIN.LT.0.)THEN                                             fdbt 970
            WRITE(IPR,'(3A,//9X,A)')'0FDBETA,',                         fdbt 980
     $           ' CASE 2D(H1,H2,BETA): REFRACTED TANGENT HEIGHT',      fdbt 990
     $           ' IS LESS THAN ZERO-PATH INTERSECTS THE EARTH',        fdbt1000
     $           ' BETA IS TOO LARGE FOR THIS H1 AND H2'                fdbt1010
            IERROR=1                                                    fdbt1020
         ELSEIF(H1.LE.H2)THEN                                           fdbt1030
            BETA=BETA1                                                  fdbt1040
            ANGLE=ANGLE1                                                fdbt1050
         ELSE                                                           fdbt1060
            BETA=BETA1                                                  fdbt1070
            ANGLE=PHI                                                   fdbt1080
            PHI=ANGLE1                                                  fdbt1090
         ENDIF                                                          fdbt1100
         RETURN                                                         fdbt1110
      ENDIF                                                             fdbt1120
C                                                                       fdbt1130
      APREV =  ANGLE1                                                   fdbt1140
      IF (DBPREV*DBETA .LT. 0.0D00 .AND. ITER .GT. 5) THEN              fdbt1150
         ANGLE1=AWGHTD                                                  fdbt1160
         DBPREV = BETA1 - BETA                                          fdbt1170
      ELSE                                                              fdbt1180
         DBPREV = BETA1 - BETA                                          fdbt1190
         ANGLE1=ANGLE1-DERIV*DBPREV                                     fdbt1200
      ENDIF                                                             fdbt1210
      GOTO10                                                            fdbt1220
C                                                                       fdbt1230
      END                                                               fdbt1240
      SUBROUTINE FLAYZ(ML,MODEL,ICLD,ZMDL,GNDALT,IVSA)                  flay 100
      include 'parameter.list'
C                                                                       flay 110
C     SUBROUTINE TO CREATE FINAL LOWTRAN BOUNDARIES                     flay 120
C                                                                       flay 130
C     ZMDL COMMON /MODEL/ FINAL ALTITUDE FOR LOWTRAN                    flay 140
C     ZCLD CLOUD ALTITUDE                                               flay 150
C     ZK1 USED WITH VSA                                                 flay 160
C     ZNEW ALTITUDES ABOVE THE CLOUD                                    flay 170
C     ZNEWV ALTITUDES ABOVE THE 1ST 9 VSA ALTITUDES                     flay 180
C     ZTST  =ZCLD(J)                                                    flay 190
C     ZVSA  VSA ALTITUDES                                               flay 200
C                                                                       flay 210
      COMMON /CARD2A/ CTHIK,CALT,CEXT                                   flay 220
      COMMON /ZVSALY/ ZVSA(10),RHVSA(10),AHVSA(10),IHVSA(10)            flay 230
      DIMENSION ZNEWV(24),ZMDL( *)                                      flay 240
      DIMENSION ZNEW(17),ZCLD(16),ZAER(34),ZST(laydim)
      DATA ZCLD/ 0.0,0.16,0.33,0.66,1.0,1.5,2.0,2.4,2.7,                flay 260
     1 3.0,3.5,4.0,4.5,5.0,5.5,6.0/                                     flay 270
      DATA ZNEWV/1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,                flay 280
     C 14.,16.,18.,20.,22.,25.,30.,35.,40.,50.,70.,100./                flay 290
      DATA ZNEW/ 7.,8.,9.,10.,12.,14.,16.,18.,20.,22.,25.,30.,          flay 300
     1 35.,40.,50.,70.,100./                                            flay 310
      DATA ZAER / 0., 1., 2., 3., 4., 5., 6., 7., 8., 9.,               flay 320
     1           10.,11.,12.,13.,14.,15.,16.,17.,18.,19.,               flay 330
     1           20.,21.,22.,23.,24.,25.,30.,35.,40.,45.,               flay 340
     1           50.,70.,100.,   1000./                                 flay 350
      DATA DELZ /0.02/                                                  flay 360
      IF(MODEL .EQ.7) GO TO 600                                         flay 370
C                                                                       flay 380
      IF(MODEL.EQ.0) GO TO 600                                          flay 390
      IF(IVSA.EQ.1) THEN                                                flay 400
           DO 2 I=1,9                                                   flay 410
2          ZMDL(I)=ZVSA(I)                                              flay 420
C                                                                       flay 430
           HMXVSA=ZVSA(9)                                               flay 440
           ZK1=HMXVSA+0.01                                              flay 450
           IF(HMXVSA.LT.2.)ML=33                                        flay 460
           IF(HMXVSA.LT.1.)ML=34                                        flay 470
           IF(HMXVSA.EQ.2.)ML=32                                        flay 480
           MDEL=34-ML                                                   flay 490
           DO 4 K=1,ML                                                  flay 500
           IK=K-10+MDEL                                                 flay 510
           IF(IK.GE.1)ZMDL(K)=ZNEWV(IK)                                 flay 520
           IF(K.EQ.10)ZMDL(K)=ZK1                                       flay 530
4          CONTINUE                                                     flay 540
C                                                                       flay 550
           RETURN                                                       flay 560
      ENDIF                                                             flay 570
C                                                                       flay 580
      IF(ICLD.GE.1.AND.ICLD.LE.11) GO TO 18                             flay 590
      DO 10 I=1,ML                                                      flay 600
      IF(ZAER(I) .GT.100.) GO TO 12                                     flay 610
      IL = I                                                            flay 620
10    ZMDL(I)=ZAER(I)                                                   flay 630
12    ML = IL                                                           flay 640
C     IF(IEMSCT.NE.0) ZMDL(ML)=100.                                     flay 650
C                                                                       flay 660
      IF(GNDALT.LE.0.) GO TO  90                                        flay 670
      DALT=(6.-GNDALT)/6.                                               flay 680
      IF(DALT.LE.0.) GO TO  90                                          flay 690
C                                                                       flay 700
      DO 15 I=1,6                                                       flay 710
15    ZMDL(I)=FLOAT(I-1)*DALT+GNDALT                                    flay 720
90    IF(ICLD .EQ. 18 .OR .ICLD. EQ. 19) THEN                           flay 730
           CLDD =  0.1 * CTHIK                                          flay 740
           CLD0 = CALT - 0.5*CLDD                                       flay 750
           IF(CLD0 .LE. 0.) CLD0 = 0.                                   flay 760
           CLD1 = CLD0 + CLDD                                           flay 770
           CLD2 = CLD1 + CTHIK - CLDD                                   flay 780
           CLD3 = CLD2 + CLDD                                           flay 790
           DO 100 I = 1,ML                                              flay 800
           IJ = I                                                       flay 810
           IF(ZMDL(I) . LT. CLD0) GO TO 100                             flay 820
           GO TO 120                                                    flay 830
100        CONTINUE                                                     flay 840
           GO TO 600                                                    flay 850
120        ML1 = ML - IJ                                                flay 860
           DO 130 I = 1 ,ML1                                            flay 870
130        ZST(I)   = ZMDL(IJ+I-1)                                      flay 880
           ZMDL(IJ) = CLD0                                              flay 890
           ZMDL(IJ+1) = CLD1                                            flay 900
           ZMDL(IJ+2) = CLD2                                            flay 910
           ZMDL(IJ+3) = CLD3                                            flay 920
           II  =  3                                                     flay 930
           DO 140 I = 1,ML1                                             flay 940
           IF(ZST(I) . LT. CLD3) GO TO 140                              flay 950
           II  = II + 1                                                 flay 960
           IF((IJ + II) . EQ. ML) GO TO 145                             flay 970
           ZMDL(IJ + II) = ZST(I)                                       flay 980
           IJII = IJ + II                                               flay 990
140        CONTINUE                                                     flay1000
           ML = IJII +1                                                 flay1010
145        ZMDL(ML) = 100.                                              flay1020
      ENDIF                                                             flay1030
      GO TO 600                                                         flay1040
C                                                                       flay1050
C     STAND CLOUD                                                       flay1060
C                                                                       flay1070
18    DO 20 I=1,16                                                      flay1080
20    ZMDL(I)=ZCLD(I)+GNDALT                                            flay1090
      I=16                                                              flay1100
C                                                                       flay1110
      DO 30 K=17,ML                                                     flay1120
      J=K-16                                                            flay1130
      IF(ZNEW(J).LE.ZMDL(16)) GO TO 30                                  flay1140
      I=I+1                                                             flay1150
      ZMDL(I)=ZNEW(J)                                                   flay1160
30    CONTINUE                                                          flay1170
      ML = I                                                            flay1180
      GO TO 600                                                         flay1190
C                                                                       flay1200
C     MODEL 7                                                           flay1210
600   RETURN                                                            flay1220
      END                                                               flay1230
