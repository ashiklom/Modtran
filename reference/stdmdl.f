       PROGRAM MODTRN3                                                  modt 100
C                                                                       modt 
C     THIS VERSION WAS UPDATED  Aug  04 1995                            modt 120
C     NOW CONTAINS NEW VSA AND WATER VAPOR CONTINUM CORRECTION          modt 130
c     correction transmission for radiance with solar                   modt 140
c     improved radiance algorithim  
c     spectrally  dependent albedo
c     use neg values of albedo to trigger
c     common and dimensions via parameter list
c     added x_sections
c     correct block data
C     ratcsz     july 11
c     more x_sections aug 1  (13 in all)
c     disort          aug19
c     new solar       sept 21 1994
c     co2 continum  now part of 1995 version of DIRAC
c
c--------------------------------------------------------------
C     MODTRAN IS A COMPUTER CODE DEVELOPED BY SPECTRAL SCIENCES, INC.   modt 160
C     AND DESIGNED TO DETERMINE ATMOSPHERIC TRANSMISSION AND RADIANCE   modt 170
C     AT MODERATE RESOLUTION (FWHM = 1 CM-1) FROM 0 TO 50,000 CM-1.     modt 180
C     MODTRAN IS BASED ON AFGL'S LOWTRAN 7 CODE.  UNLESS THE CODE IS    modt 190
C     USED TO RUN LOWTRAN 7, AN UNFORMATTED BAND MODEL TAPE (UNIT=9),   modt 200
C     UFTAPE, MUST EXIST.  THE PROGRAM "MAKEUF" CREATES THIS TAPE FROM  modt 210
C     THE FORMATTED BAND MODEL TAPE, BMTAPE.                            modt 220
C                                                                       modt 230
C     MOST OF THE ROUTINES FROM LOWTRAN 7 REMAIN UNCHANGED.  SOME       modt 240
C     ROUTINES HAVE BEEN CHANGED ONLY IN THAT A FEW OF THEIR COMMONS    modt 250
C     STATEMENTS HAVE BEEN MODIFIED AND THE LOGICAL VARIABLE "MODTRN"   modt 260
C     HAS BEEN DECLARED:                                                modt 270
C                                                                       modt 280
C        ROUTINE    COMMONS                                             modt 290
C        -------    -------                                             modt 300
C        ABCDTA     "BLANK"                                             modt 310
C        AEREXT     "BLANK", CARD1                                      modt 320
C        AERNSM     "BLANK", CARD1                                      modt 330
C        AERTMP     "BLANK"                                             modt 340
C        CIRR18     "BLANK", CARD1, CARD4                               modt 350
C        CIRRUS     "BLANK", CARD1, CARD4                               modt 360
C        CLDPRF     "BLANK"                                             modt 370
C        DESATT     "BLANK"                                             modt 380
C        EQULWC     "BLANK"                                             modt 390
C        EXABIN     "BLANK", CARD1, CARD4                               modt 400
C        FLXADD     "BLANK", CARD1, TRAN                                modt 410
C        LAYVSA     CARD1                                               modt 420
C        PHASEF     CARD1                                               modt 430
C        RDEXA      "BLANK"                                             modt 440
C        RDNSM      CARD1                                               modt 450
C        DPRFPA (FORMERLY RFPATH)                                       modt 460
c                   CARD1, SOLS                                         modt 470
C        SSRAD      "BLANK", CARD1, SOLS                                modt 480
C        VSANSM     "BLANK", CARD1                                      modt 490
C                                                                       modt 500
C     THE OTHER ROUTINES WITH MINOR CHANGES ARE:                        modt 510
C                                                                       modt 520
C        ROUTINE    NUMBER OF ADDED AND SUBTRACTED LINES                modt 530
C        -------    ------------------------------------                modt 540
C        GEO        108                                                 modt 550
C        MSRAD      34                                                  modt 560
C        SSGEO      53                                                  modt 570
C        STDMDL     39                                                  modt 580
C                                                                       modt 590
C     THE MOST SIGNIFICANT CHANGES WERE MADE TO THE MAIN ROUTINE AND    modt 600
C     THE SUBROUTINE TRANS.  THE MAIN HAS BEEN SPLIT INTO 2 ROUTINES.   modt 610
C     THE NEW MAIN ROUTINE JUST CONTAINS COMMENT CARDS AND A CALL TO    modt 620
C     ROUTINE DRIVER.  SUBROUTINE DRIVER IS THE DRIVER FOR MODTRAN.     modt 630
C     SUBROUTINE TRANS HAS BEEN COMPLETELY REVAMPED WITH ITS NEW FORM   modt 640
C     CONTAINING 1012 LINES.  TRANS NOW STEPS IN 1 CM-1 INCREMENTS,     modt 650
C     CALLING THE BAND MODEL ROUTINES, INTERPOLATING BETWEEN 5 CM-1     modt 660
C     CONTINUUM TRANSMITTANCES, AND PERFORMING THE TRIANGULAR SLIT      modt 670
C     CALCULATION.                                                      modt 680
C                                                                       modt 690
C     A NUMBER OF NEW ROUTINES HAS BEEN ADDED TO ACCOMMODATE THE HIGHER modt 700
C     RESOLUTION OPTION:                                                modt 710
C                                                                       modt 720
C        ROUTINE (# LINES)    PURPOSE                                   modt 730
C        -----------------    --------------------------                modt 740
C        BMDATA    (135)      MAKE INITIAL CALLS TO BAND MODEL TAPE     modt 750
C        BMERFU     (14)      RETURNS EXP(-Y*Y) + Y*SQRT(PI)*ERF(Y) - 1 modt 760
C        BMFLUX    (277)      PERFORMS ADDING METHOD CALCULATION        modt 770
C        BMLOAD     (91)      LOADS THE BAND MODEL PARAMETERS           modt 780
C        BMOD      (287)      PERFORMS CURTIS-GODSON SUMS               modt 790
C        BMTRAN     (57)      CALCULATES MOLECULAR TRANSMITTANCE        modt 800
C                                                                       modt 810
C     TWO CARDS FROM THE LOWTRAN 7 INPUT STREAM HAVE BEEN CHANGED.      modt 820
C     LOGICAL VARIABLE, MODTRN, IS NOW READ IN ON CARD 1, AS SHOWN:     modt 830
C                                                                       modt 840
C     READ(IRD,'(L1,I4,12I5,F8.3,F7.2)')MODTRN,MODEL,ITYPE,IEMSCT,      modt 850
C    1  IMULT,M1,M2,M3,M4,M5,M6,MDEF,IM,NOPRT,TBOUND,SALB               modt 860
C                                                                       modt 870
C     IF MODTRN IS FALSE, THE CODE DEFAULTS TO LOWTRAN 7.  WHEN MODTRN  modt 880
C     IS TRUE, THE MODERATE RESOLUTION OPTION IS INVOKED.  CARD 4 HAS   modt 890
C     ALSO BEEN CHANGED.  INSTEAD OF READING THE REAL VARIABLES, V1, V2 modt 900
C     AND DV, THE INTEGERS IV1, IV2, IDV AND IFWHM ARE READ, AS SHOWN:  modt 910
C                                                                       modt 920
C     READ(IRD,'(4I10)')IV1,IV2,IDV,IFWHM                               modt 930
C                                                                       modt 940
C     IV1 AND IV2 ARE THE LOWER AND UPPER BOUNDS ON THE FREQUENCY       modt 950
C     (CM-1) RANGE, RESPECTIVELY.  IV1 IS A NON-NEGATIVE INTEGER AND    modt 960
C     IV2 MUST EXCEED IV1.  IDV, A POSITIVE INTEGER, IS THE STEP SIZE   modt 970
C     USED FOR PRINTING OF TRANSMITTANCES AND RADIANCES.  FINALLY,      modt 980
C     IFWHM IS THE FULL WIDTH AT HALF MAXIMUM USED BY THE TRIANGULAR    modt 990
C     SLIT FUNCTION.  IFWHM MUST BE A POSITIVE INTEGER NOT EXCEEDING 50 modt1000
C                                                                       modt1010
C***********************************************************************modt1020
C     LOWTRAN7  (LAST REVISED JAN  30 1989) REVISION 3.6                modt1030
C                                                                       modt1040
C               AUTHORS                                                 modt1050
C                                                                       modt1060
C               F.X.KNEIZYS                                             modt1070
C               E. P. SHETTLE                                           modt1080
C               G.P. ANDERSON                                           modt1090
C               L. W. ABREU                                             modt1100
C               J. H. CHETWYND                                          modt1110
C               J. E. A. SELBY    (GRUMMAN AEROSPACE)                   modt1120
C               S. A. CLOUGH      (AER INC)                             modt1130
C               W. O. GALLERY     (OPTIMETRICS)                         modt1140
C                                                                       modt1150
C   PROGRAM LOWTRAN  CALCULATES THE TRANSMITTANCE AND/OR RADIANCE       modt1160
C   OF THE ATMOSPHERE  FROM   0 CM-1 TO 50000 CM-1 (0.20 TO INFINITY    modt1170
C   MICRONS) AT 20 CM-1 SPECTRAL RESOLUTION ON A LINEAR                 modt1180
C   WAVENUMBER SCALE WITH 5CM-1 SAMPLING                                modt1190
C                                                                       modt1200
C   LOWTRAN 7 IS A LOW-RESOLUTION PROPAGATION MODEL FOR CALCULATING     modt1210
C   ATMOSPHERIC TRANSMITTANCE AND BACKGROUND RADIANCE FROM 0 TO         modt1220
C   50,000 CM-1 AT A RESOLUTION OF 20 CM-1 WITH A MINIMUM OF 5 CM-1     modt1230
C   SAMPLING.  THE MODEL IS BASED ON THE LOWTRAN 6 (1983) MODEL.        modt1240
C   THE PROGRAM CALCULATES SINGLE SCATTERED SOLAR (OR LUNAR)            modt1250
C   RADIATION.  MULTIPLE SCATTERED RADIATION HAS BEEN ADDED TO THE      modt1260
C   MODEL AS WELL AS NEW MOLECULAR BAND MODEL PARAMETERS AND NEW OR     modt1270
C   UPDATED OZONE AND MOLECULAR OXYGEN ABSORPTION PARAMETERS FOR THE    modt1280
C   UV.  OTHER MODIFICATIONS INCLUDE A WIND-DEPENDENT DESERT MODEL, NEW modt1290
C   CIRRUS CLOUD MODELS, AND NEW CLOUD AND RAIN MODELS.  THE MODEL ALSO modt1300
C   INCLUDES NEW REPRESENTATIVE (GEOGRAPHICAL AND SEASONAL) ATMOSPHERIC modt1310
C   MODELS AND UPDATED AEROSOL MODELS WITH OPTIONS TO REPLACE THEM WITH modt1320
C   USER-DERIVED VALUES.  SIX MODES OF PROGRAM EXECUTION ARE ALLOWED    modt1330
C   WITH THE NEW MODEL AND COMPUTER CODE FOR A GIVEN SLANT PATH         modt1340
C   UTILIZING SPHERICAL-REFRACTIVE GEOMETRY.  THE ARMY VERTICAL         modt1350
C   STRUCTURE ALGORITHM HAS BEEN MODIFIED TO INCLUDE THE NEW PEDESTAL   modt1360
C   MODEL BELOW THE CLOUD BASE.  A NEW OPTION HAS BEEN  ADDED TO        modt1370
C   MODIFY THE AEROSOL PROFILE, IF THE GROUND IS NOT AT SEA LEVEL.      modt1380
C                                                                       modt1390
C***********************************************************************modt1400
C                                                                       modt1410
C     THE FOLLOWING INFORMATION SHOULD BE PROVIDED BY THE USER          modt1420
C     AND MAILED TO   L.W ABREU  ,AFGL/OPI,HANSCOM AFB,MASS 01731       modt1430
C     THIS WILL BE USED TO UPDATE THE AFGL MAILING LIST                 modt1440
C     AND FOR NOTIFICATION TO THE USER OF ERRORS IN THE CODE            modt1450
C                                                                       modt1460
C                                                                       modt1470
C           MY NAME IS                                                  modt1480
C           COMPANY                                                     modt1490
C           ADDRESS                                                     modt1500
C           MY COMPUTER IS                                              modt1510
C                                                                       modt1520
C                                                                       modt1530
C***********************************************************************modt1540
C   THE USE OF THE WORD 'CARD' IS EQUIVALENT TO EDITING WITH 80 COLUMNS modt1550
C                                                                       modt1560
C     PROGRAM ACTIVATED BY SUBMISSION OF A FIVE  (OR MORE)              modt1570
C      CARD SEQUENCE AS FOLLOWS                                         modt1580
C                                                                       modt1590
C     CARD 1    MODEL,ITYPE,IEMSCT,IMULT,M1,M2,M3,                      modt1600
C               M4,M5,M6,MDEF,IM,NOPRT,TBOUND,SALB                      modt1610
C                          FORMAT(13I5,F8.3,F7.2)                       modt1620
C                                                                       modt1630
C     CARD 2    IHAZE,ISEASN,IVULCN,ICSTL,ICLD,IVSA,VIS,WSS,WHH,RAINRT, modt1640
C               GNDALT                                                  modt1650
C                          FORMAT(6I5,5F10.3)                           modt1660
C                                                                       modt1670
C               CARD 2A    CTHIK,CALT,CEXT,ISEED       (ICLD=18,19,20)  modt1680
C                          FORMAT(3F10.3,I10)                           modt1690
C                                                                       modt1700
C               CARD 2B    ZCVSA,ZTVSA,ZINVSA    (IVSA=1)               modt1710
C                          FORMAT(3F10.3)                               modt1720
C                                                                       modt1730
C               CARD 2C    ML,IRD1,IRD2,TITLE (MODEL=0 / 7,IM=1)        modt1740
C                          FORMAT(3I5,18A4)                             modt1750
C                                                                       modt1760
CC------------------------ BEGIN ML LOOP                                modt1770
CC-                                                                     modt1780
CC-             CARD 2C1   ZMDL,P,T,WMOL(1),WMOL(2),WMOL(3),JCHAR       modt1790
CC-                        FORMAT(F10.3,5E10.3,15A1)                    modt1800
CC-                                                                     modt1810
CC-             CARD 2C2   (WMOL(J),J=4,11)                             modt1820
CC-                        FORMAT(8E10.3)                               modt1830
CC-                                                                     modt1840
CC-             CARD 2C2   WMOL(12)                                     modt1850
CC-                        FORMAT(8E10.3)                               modt1860
CC-                                                                     modt1870
CC-             CARD 2C3   AHAZE,EQLWCZ,RRATZ,IHA1,ICLD1,               modt1880
CC-                        IVUL1,ISEA1,ICHR                             modt1890
CC-                        FORMAT(10X,3F10.3,5I5)                       modt1900
CC-                                                                     modt1910
CC------------------------ END ML LOOP                                  modt1920
C                                                                       modt1930
C               CARD 2D    IREG(1 TO 4) (IHAZE=7 OR ICLD = 11)          modt1940
C                          FORMAT(4I5)                                  modt1950
C                                                                       modt1960
C               CARD 2D1   AWCCON,TITLE                                 modt1970
C                          FORMAT(E10.3,18A4)                           modt1980
C                                                                       modt1990
C               CARD 2D2   (VX(I),EXTC(N,I),ABSC(N,I),ASYM(N,I),I=1,47) modt2000
C                          (IHAZE=7 OR ICLD=11)                         modt2010
C                          FORMAT(3(F6.2,2F7.5,F6.4))                   modt2020
C                                                                       modt2030
C     CARD 3    H1,H2,ANGLE,RANGE,BETA,RO,LEN                           modt2040
C                          FORMAT(6F10.3,I5)                            modt2050
C                                                                       modt2060
C               ALTERNATE  CARD 3 (IEMSCT=3)                            modt2070
C                          H1,H2,ANGLE,IDAY,RO,ISOURC,ANGLEM            modt2080
C                          FORMAT(3F10.3,I5,5X,F10.3,I5,F10.3)          modt2090
C                                                                       modt2100
C               CARD 3A1   IPARM,IPH,IDAY,ISOURC           (IEMSCT=2)   modt2110
C                          FORMAT(4I5)                                  modt2120
C                                                                       modt2130
C               CARD 3A2    PARM1,PARM2,PARM3,PARM4,TIME,PSIPO,ANGLEM,G modt2140
C                           FORMAT(8F10.3)               (IEMSCT=2)     modt2150
C                                                                       modt2160
C               CARD 3B1    NANGLS       (IPH=1)                        modt2170
C                           FORMAT(I5)                                  modt2180
C                                                                       modt2190
C               CARD 3B2(1 TO NANGLS)    (IPH=1)                        modt2200
C                      (ANGF(I),F(1,I),F(2,I),F(3,I),F(4,I),I=1,NANGLS) modt2210
C                           FORMAT(5E10.3)                              modt2220
C                                                                       modt2230
C     CARD 4    V1, V2, DV                                              modt2240
C                           FORMAT(3F10.3)                              modt2250
C                                                                       modt2260
C     CARD 5    IRPT                                                    modt2270
C                           FORMAT(I5)                                  modt2280
C                                                                       modt2290
C***********************************************************************modt2300
C   ** FOLLOWING IS A FULL DESCRIPTION OF EACH CARD                     modt2310
C                                                                       modt2320
C     CARD 1    MODEL,ITYPE,IEMSCT,IMULT,M1,M2,M3,                      modt2330
C               M4,M5,M6,MDEF,IM,NOPRT,TBOUND,SALB                      modt2340
C                         FORMAT(13I5,F8.3,F7.2)                        modt2350
C                                                                       modt2360
C             'MODEL' SELECTS ONE OF SIX GEOGRAPHICAL MODEL ATMOSPHERES modt2370
C              OR SPECIFIES THAT USER-DEFINED METEOROLOGICAL            modt2380
C              DATA ARE TO BE USED.                                     modt2390
C                                                                       modt2400
C                                                                       modt2410
C     MODEL=0 IF METEOROLOGICAL DATA ARE SPECIFIED(HORIZONTAL PATH ONLY)modt2420
C           1 TROPICAL ATMOSPHERE                                       modt2430
C           2 MIDLATITUDE SUMMER                                        modt2440
C           3 MIDLATITUDE WINTER                                        modt2450
C           4 SUBARCTIC   SUMMER                                        modt2460
C           5 SUBARCTIC   WINTER                                        modt2470
C           6 1976 U.S. STANDARD ATMOSPHERE                             modt2480
C           7 IF A NEW MODEL ATMOSPHERE( OR RADIOSONDE DATA) IS TO BE   modt2490
C             READ IN.                                                  modt2500
C                                                                       modt2510
C     [NOTE: MODEL=0  USED FOR HORIZONTAL PATH ONLY]                    modt2520
C                                                                       modt2530
C                                                                       modt2540
C           'ITYPE' INDICATES THE TYPE OF ATMOSPHERIC PATH              modt2550
C                                                                       modt2560
C     ITYPE=1 FOR A HORIZONTAL (CONSTANT-PRESSURE) PATH                 modt2570
C           2 VERTICAL OR SLANT PATH BETWEEN TWO ALTITUDES              modt2580
C           3 FOR A VERTICAL OR SLANT PATH TO SPACE                     modt2590
C                                                                       modt2600
C                                                                       modt2610
C           'IEMSCT' DETERMINES THE MODE OF EXECUTION OF THE PROGRAM    modt2620
C                                                                       modt2630
C     IEMSCT=0    PROGRAM EXECUTION IN TRANSMITTANCE MODE.              modt2640
C            1    PROGRAM EXECUTION IN RADIANCE MODE.                   modt2650
C            2    PROGRAM EXECUTION IN RADIANCE MODE WITH SOLAR/LUNAR   modt2660
C                  SCATTERED RADIANCE INCLUDED.                         modt2670
C            3    DIRECT SOLAR IRRADIANCE                               modt2680
C                                                                       modt2690
C           'IMULT' DETERMINES EXECUTION WITH MULTIPLE SCATTERING       modt2700
C                                                                       modt2710
C     IMULT = 0 PROGRAM EXECUTED WITHOUT MULTIPLE SCATTERING            modt2720
C             1 PROGRAM EXECUTED WITH MULTIPLE SCATTERING               modt2730
C              [NOTE: IEMSCT MUST EQUAL 1 OR 2 FOR MULTIPLE SCATTERING] modt2740
C                                                                       modt2750
C                                                                       modt2760
C           'M1,M2,M3' ARE USED TO MODIFY OR SUPPLEMENT THE ALTITUDE    modt2770
C            PROFILES OF TEMPERATURE AND PRESSURE,WATER VAPOR,AND OZONE modt2780
C                                                                       modt2790
C           'M4,M5,M6'  SEASONAL DEPENDENCE CH4,N2O,CO                  modt2800
C           'MDEF'       USE DEFAULT FOR OTHER GASES                    modt2810
C                                                                       modt2820
C     FOR NORMAL OPERATION OF PROGRAM   (MODEL 1 TO 6)                  modt2830
C     SET M1=M2=M3=0 , M4=M5=M6=MDEF = 0                                modt2840
C                                                                       modt2850
C     THESE PARAMETERS ARE RESET TO DEFAULT VALUES BY MODEL             modt2860
C     WHEN THEY ARE EQUAL TO ZERO                                       modt2870
C                                                                       modt2880
C      EXCEPT FOR MODEL 0 AND 7                                         modt2890
C      WHEN M1 = 0 M1 RESET TO 'MODEL'                                  modt2900
C      WHEN M2 = 0 M2 RESET TO 'MODEL'                                  modt2910
C      WHEN M3 = 0 M3 RESET TO 'MODEL'                                  modt2920
C      WHEN M4 = 0 M4 RESET TO 'MODEL'                                  modt2930
C      WHEN M5 = 0 M5 RESET TO 'MODEL'                                  modt2940
C      WHEN M6 = 0 M6 RESET TO 'MODEL'                                  modt2950
C      WHEN MDEF=0 MDEF RESET TO 1  FOR ALL REMAINING                   modt2960
C                                                                       modt2970
C     M1=1-6 DEFAULT TEMP. AND PRESSURE TO SPECIFIED MODEL ATM.         modt2980
C                                                                       modt2990
C     M2=1-6 DEFAULT H2O   TO SPECIFIED MODEL ATM.                      modt3000
C                                                                       modt3010
C     M3=1-6 DEFAULT OZONE TO SPECIFIED MODEL ATM.                      modt3020
C                                                                       modt3030
C     M4=1-6 DEFAULT CH4   TO SPECIFIED MODEL ATM.                      modt3040
C                                                                       modt3050
C     M5=1-6 DEFAULT N2O   TO SPECIFIED MODEL ATM.                      modt3060
C                                                                       modt3070
C     M6=1-6 DEFAULT CO    TO SPECIFIED MODEL ATM.                      modt3080
C                                                                       modt3090
C     MDEF=1     USE DEFAULT   PROFILE  FOR CO2,O2,NO,SO2,NO2,NH3,HNO3  modt3100
C                NOT NEEDED WITH MODEL 1 TO 6                           modt3110
C                                                                       modt3120
C                                                                       modt3130
C     IF 'MODEL' 0 OR 'MODEL' 7  THE PROGRAM EXPECTS TO READ            modt3140
C     "USER SUPPLIED" ATMOSPHERIC PROFILES. SET:'IM' = 1 FOR            modt3150
C      FIRST RUN. TO RERUN THE SAME "USER-ATMOSPHERE" FOR A SERIES      modt3160
C      OF CASES SET:'IM' = 0 TO REUSE THE PREVIOUSLY READ DATA.         modt3170
C                                                                       modt3180
C     IM=0    FOR  NORMAL OPERATION OF PROGRAM OR WHEN SUBSEQUENT       modt3190
C                  CALCULATIONS ARE TO BE RUN WITH MODEL =7             modt3200
C        1    WHEN RADIOSONDE DATA ARE TO BE READ INITIALLY.            modt3210
C                                                                       modt3220
C     NOPRT=0 FOR NORMAL OPERATION OF PROGRAM.                          modt3230
C                                                                       modt3240
C           1 TO MINIMIZE PRINTING OF TRANSMITTANCE /OR RADIANCE TABLE  modt3250
C                   AND ATMOSPHERIC PROFILES                            modt3260
C                                                                       modt3270
C                                                                       modt3280
C     TBOUND =BOUNDARY TEMPERATURE ( K),USED IN THE RADIATION MODE      modt3290
C             (IEMSCT = 1 OR 2) FOR SLANT PATHS THAT INTERSECT THE      modt3300
C             EARTH OR TERMINATE AT A GREY BOUNDARY (FOR EXAMPLE        modt3310
C             CLOUD,TARGET).  IF TBOUND IS LEFT BLANK AND THE PATH      modt3320
C             INTERSECTS THE EARTH, THE PROGRAM WILL USE THE            modt3330
C             TEMPERATURE OF THE FIRST ATMOSPHERIC LEVEL AS THE         modt3340
C             BOUNDARY TEMPERATURE.                                     modt3350
C                                                                       modt3360
C      SALB = SURFACE ALBEDO OF THE EARTH AT THE LOCATION               modt3370
C             AND AVERAGE FREQUENCY OF THE CALCULATION (0 TO 1.)        modt3380
C             IF SALB IS LEFT BLANK THE PROGRAM ASSUMES                 modt3390
C             THE SURFACE IS A BLACKBODY.                               modt3400
c             negitive value use spectrally dependent values from
c             refbkg  salb  -1 uses the 1st file 
c
C***********************************************************************modt3410
C                                                                       modt3420
C     CARD 2   IHAZE,ISEASN,IVULCN,ICSTL,ICLD,IVSA,VIS,WSS,WHH,RAINRT,  modt3430
C              GNDALT                                                   modt3440
C                          FORMAT(6I5,5F10.3)                           modt3450
C                                                                       modt3460
C     'IHAZE' SELECTS THE TYPE OF EXTINCTION AND A DEFAULT              modt3470
C     METEOROLOGICAL RANGE FOR THE BOUNDARY-LAYER AEROSOL MODEL         modt3480
C     (0 TO 2KM ALTITUDE)                                               modt3490
C     IF 'VIS' IS ALSO SPECIFIED ON CARD 2 IT WILL OVERRIDE THE         modt3500
C     DEFAULT 'IHAZE' VALUE  FOR METEOROLOGICAL RANGE                   modt3510
C                                                                       modt3520
C     IHAZE=0  NO AEROSOL ATTENUATION INCLUDED IN CALCULATION.          modt3530
C          =1  RURAL EXTINCTION, 23-KM VIS.                             modt3540
C          =2  RURAL EXTINCTION, 5-KM VIS.                              modt3550
C          =3  NAVY MARITIME EXTINCTION,SETS OWN VIS.                   modt3560
C          =4  MARITIME EXTINCTION, 23-KM VIS.    (LOWTRAN 5 MODEL)     modt3570
C          =5  URBAN EXTINCTION, 5-KM VIS.                              modt3580
C          =6  TROPOSPHERIC EXTINCTION, 50-KM VIS.                      modt3590
C          =7  USER DEFINED  AEROSOL EXTINCTION COEFFICIENTS            modt3600
C              TRIGGERS READING IREG FOR UP TO 4 REGIONS OF             modt3610
C              USER DEFINED EXTINCTION ABSORPTION AND ASYMMETRY         modt3620
C          =8  FOG1 (ADVECTIVE FOG) EXTINCTION, 0.2-KM VIS.             modt3630
C          =9  FOG2 (RADIATIVE FOG) EXTINCTION, 0.5-KM VIS.             modt3640
C          =10 DESERT EXTINCTION  SETS OWN VISIBILITY FROM WIND SPEED   modt3650
C                                                                       modt3660
C     'ISEASN' SELECTS THE SEASONAL DEPENDENCE OF THE PROFILES          modt3670
C     FOR BOTH THE TROPOSPHERIC (2 TO 10 KM) AND                        modt3680
C     STRATOSPHERIC(10 TO 30 KM) AEROSOLS.                              modt3690
C                                                                       modt3700
C     ISEASN=0 DEFAULTS TO SEASON OF 'MODEL'                            modt3710
C              (MODEL 0,1,2,4,6,7) SUMMER                               modt3720
C              (MODEL 3,5)         WINTER                               modt3730
C           =1 SPRING-SUMMER                                            modt3740
C           =2 FALL - WINTER                                            modt3750
C                                                                       modt3760
C     'IVULCN' SELECTS BOTH THE PROFILE AND EXTINCTION TYPE             modt3770
C     FOR THE STRATOSPHERIC AEROSOLS AND DETERMINES TRANSITION          modt3780
C     PROFILES ABOVE THE STRATOSPHERE TO 100 KM.                        modt3790
C                                                                       modt3800
C     IVULCN=0 DEFAULT TO STRATOSPHERIC BACKGROUND                      modt3810
C           =1 STRATOSPHERIC BACKGROUND                                 modt3820
C           =2 AGED VOLCANIC TYPE/MODERATE VOLCANIC PROFILE             modt3830
C           =3 FRESH VOLCANIC TYPE/HIGH VOLCANIC PROFILE                modt3840
C           =4 AGED VOLCANIC TYPE/HIGH VOLCANIC PROFILE                 modt3850
C           =5 FRESH VOLCANIC TYPE/MODERATE VOLCANIC PROFILE            modt3860
C           =6 BACKGROUND STRATOSPHERIC TYPE/MODERATE VOLCANIC PROFILE  modt3870
C           =7 BACKGROUND STRATOSPHERIC TYPE/HIGH VOLCANIC PROFILE      modt3880
C           =8 FRESH VOLCANIC TYPE/EXTREME VOLCANIC PROFILE             modt3890
C                                                                       modt3900
C     'ICSTL' IS THE AIR MASS CHARACTER(1 TO 10) ONLY USED WITH         modt3910
C     NAVY MARITIME MODEL(IHAZE=3)                                      modt3920
C                                                                       modt3930
C     ICSTL = 1 OPEN OCEAN                                              modt3940
C            .                                                          modt3950
C            .                                                          modt3960
C            .                                                          modt3970
C           10 STRONG CONTINENTAL INFLUENCE                             modt3980
C                                                                       modt3990
C                                                                       modt4000
C     'ICLD' SPECIFIES WHICH OF THE CLOUD MODELS AND THE RAIN RATES     modt4010
C     ARE USED                                                          modt4020
C                                                                       modt4030
C     ICLD  FOR CLOUD AND OR RAIN                                       modt4040
C     ICLD = 0   NO CLOUDS OR RAIN                                      modt4050
C          = 1  CUMULUS CLOUD BASE .66KM TOP 2.7KM                      modt4060
C          = 2  ALTOSTRATUS CLOUD BASE 2.4KM TOP 3.0KM                  modt4070
C          = 3  STRATUS CLOUD BASE .33KM TOP 1.0KM                      modt4080
C          = 4  STRATUS/STRATO CU BASE .66KM TOP 2.0KM                  modt4090
C          = 5  NIMBOSTRATUS CLOUD BASE .16KM TOP .66KM                 modt4100
C          = 6  2.0MM/HR DRIZZLE (MODELED WITH CLOUD  3)                modt4110
C               RAIN  2. MM HR AT 0KM TO .22 MM HR AT 1.5KM             modt4120
C          = 7  5.0MM/HR LIGHT RAIN (MODELED WITH CLOUD  5)             modt4130
C               RAIN  5. MM HR AT 0KM TO .2  MM HR AT 1.5KM             modt4140
C          = 8  12.5MM/HR MODERATE RAIN (MODELED WITH CLOUD  5)         modt4150
C               RAIN 12.5MM HR AT 0KM TO .2  MM HR AT 2.0KM             modt4160
C          = 9  25.0MM/HR HEAVY RAIN (MODELED WITH CLOUD  1)            modt4170
C               RAIN 25. MM HR AT 0KM TO .2  MM HR AT 3.0KM             modt4180
C          =10  75.0MM/HR EXTREME RAIN (MODELED WITH CLOUD  1)          modt4190
C               RAIN 75. MM HR AT 0KM TO .2  MM HR AT 3.5KM             modt4200
C          =11  READ IN USER DEFINED CLOUD EXTINCTION AND ABSORPTION    modt4210
C              USER DEFINED  AEROSOL EXTINCTION COEFFICIENTS            modt4220
C              TRIGGERS READING IREG FOR UP TO 4 REGIONS OF             modt4230
C              USER DEFINED EXTINCTION ABSORPTION AND ASYMMETRY         modt4240
C          =18  STANDARD   CIRRUS MODEL                                 modt4250
C          =19  SUB VISUAL CIRRUS MODEL                                 modt4260
C          =20  NOAA       CIRRUS MODEL  (LOWTRAN 6 MODEL)              modt4270
C                                                                       modt4280
C                                                                       modt4290
C     IVSA DETERMINES THE USE OF THE ARMY VERTICAL STRUCTURE            modt4300
C     ALGORITHM FOR AEROSOLS IN THE BOUNDARY LAYER.                     modt4310
C                                                                       modt4320
C     IVSA=0   NOT USED                                                 modt4330
C         =1   VERTICAL STRUCTURE ALGORITHM                             modt4340
C                                                                       modt4350
C     'VIS'   SPECIFIES THE METEOROLIGICAL RANGE                        modt4360
C     VIS =    METEOROLOGICAL RANGE (KM) (WHEN SPECIFIED,SUPERSEDES     modt4370
C              DEFAULT VALUE SET BY IHAZE)                              modt4380
C                                                                       modt4390
C     'WSS'     SPECIFIES THE CURRENT WIND SPEED                        modt4400
C     WSS =    CURRENT WIND SPEED (M/S).    WITH (IHAZE=3/IHAZE=10)     modt4410
C                                                                       modt4420
C     'WHH'    SPECIFIES THE 24 HOUR AVERAGE WIND SPEED                 modt4430
C     WHH =    24 HOUR AVERAGE WIND SPEED (M/S).  ONLY WITH (IHAZE=3)   modt4440
C                                                                       modt4450
C     'RAINRT' SPECIFIES THE RAIN RATE                                  modt4460
C     RAINRT = RAIN RATE (MM/HR).             DEFAULT VALUE IS ZERO.    modt4470
C     USED  TO TOP OF CLOUD WHEN CLOUD IS PRESENT                       modt4480
C     WHEN NO CLOUDS RAIN RATE USED TO 6KM                              modt4490
C                                                                       modt4500
C     'GNDALT' SPECIFIES THE ALTITUDE OF SURFACE RELATIVE TO SEA LEVEL  modt4510
C     GNDALT = ALTITUDE OF SURFACE RELATIVE TO SEA LEVEL (KM)           modt4520
C              USED TO MODIFY  AEROSOL PROFILES BELOW 6 KM ALTITUDE     modt4530
C                                                                       modt4540
C***********************************************************************modt4550
C                                                                       modt4560
C     OPTIONAL INPUT CARDS AFTER CARD 2                                 modt4570
C     SELECTED BY PARAMETERS ICLD,IVSA,MODEL,AND IHAZE ON CARDS 2       modt4580
C                                                                       modt4590
C                                                                       modt4600
C     CARD 2A   CTHIK,CALT,CEXT,ISEED     (ICLD=18,19,20)               modt4610
C                          FORMAT(3F10.3,I10)                           modt4620
C                   INPUT CARD FOR CIRRUS ALTITUDE PROFILE              modt4630
C                   SUBROUTINE WHEN ICLD = 18,19,20                     modt4640
C                                                                       modt4650
C     CHTIK    = CIRRUS THICKNESS (KM)                                  modt4660
C                0  USE THICKNESS STATISTICS                            modt4670
C                                                                       modt4680
C     CALT     = CIRRUS BASE ALTITUDE(KM)                               modt4690
C                0 USE DEFAULT DETERMINED BY 'MODEL'                    modt4700
C                                                                       modt4710
C     CEXT     = EXTINCTION COEFFIENT(KM-1) AT 0.55                     modt4720
C                0 USE 0.14 * CTHIK                                     modt4730
C                                                                       modt4740
C     ISEED    = RANDOM NUMBER INITIALIZATION FLAG.                     modt4750
C                0 USE DEFAULT MEAN VALUES FOR CIRRUS                   modt4760
C                .NE. 0 INITIAL VALUE OF SEED FOR                       modt4770
C                RANDOM NUMBER GENERATOR FUNCTION                       modt4780
C                CHANGE SEED VALUE EACH RUN FOR DIFFERENT               modt4790
C                RANDOM NUMBER SEQUENCES.  THIS PROVIDES FOR            modt4800
C                STATISTICAL DETERMINATION OF CIRRUS BASE               modt4810
C                ALTITUDE AND THICKNESS.                                modt4820
C                                                                       modt4830
C   NOTE: RANDOM NUMBERS GENERATION IS SYSTEM DEPENDENT                 modt4840
C                                                                       modt4850
C***********************************************************************modt4860
C                                                                       modt4870
C     CARD 2B             ZCVSA,ZTVSA,ZINVSA     (IVSA=1)               modt4880
C                          FORMAT(3F10.3)                               modt4890
C               INPUT CARD FOR ARMY VERTICAL STRUCTURE                  modt4900
C               ALGORITHM SUBROUTINE WHEN IVSA=1.                       modt4910
C                                                                       modt4920
C     ZCVSA = CLOUD CEILING HEIGHT (KM)                                 modt4930
C             LT 0 NO CLOUD CEILING                                     modt4940
C             GT 0 KNOWN CLOUD CEILING                                  modt4950
C                0 UNKNOWN CLOUD CEILING HEIGHT                         modt4960
C                  PROGRAM CALCULATES CLOUD HEIGHT                      modt4970
C                                                                       modt4980
C     ZTVSA = THICKNESS OF CLOUD OR FOG (KM),                           modt4990
C               0 DEFAULTS TO 200 METERS                                modt5000
C                                                                       modt5010
C     ZINVSA= HEIGHT OF THE INVERSION (KM)                              modt5020
C                 0 DEFAULTS TO 100 METERS                              modt5030
C             LT  0 NO INVERSION LAYER                                  modt5040
C                                                                       modt5050
C***********************************************************************modt5060
C                                                                       modt5070
C     CARD 2C  ML,IRD1,IRD2,TITLE   (MODEL=0 / 7,IM=1)                  modt5080
C                          FORMAT(3I5,18A4)                             modt5090
C              ADDITIONAL ATMOSPHERIC MODEL       (MODEL=7)             modt5100
C              NEW MODEL ATMOSPHERE CAN BE INSERTED PROVIDED THE        modt5110
C              PARAMETERS 'MODEL' AND 'IM' ARE SET EQUAL TO 7 AND 1     modt5120
C              RESPECTIVELY ON CARD 1.                                  modt5130
C                                                                       modt5140
C     ML=      NUMBER OF ATMOSPHERIC LEVELS TO BE INSERTED              modt5150
C                   (MAXIMUM OF laydim)                                 modt5160
C                                                                       modt5170
C     CARD 2C1 IS READ AUTOMATICALLY FOR MODEL 0 AND 7                  modt5180
C                                                                       modt5190
C     IRD1 CONTROL READING WN2O,WCO ... AND WNH3,WHNO3      CARD        modt5200
C                                                                       modt5210
C     IRD1 = 0     NO READ  (MOLECULAR DENSITIES BY LAYER)              modt5220
C     IRD1 = 1     READ                                                 modt5230
C                                                                       modt5240
C     IRD2  CONTROL READING AHAZE,EQLWCZ,... CARD                       modt5250
C                                                                       modt5260
C     IRD2 = 0     NO READ  (AEROSOL CONTROL BY LAYER)                  modt5270
C     IRD2 = 1     READ                                                 modt5280
C                                                                       modt5290
C     JCHAR  ON CARD 2C1 IS USUALLY USED TO DEFINE MOLECULES 4 TO 12    modt5300
C     IHAZE  ON CARD 2   IS USUALLY USED TO DEFINE AEROSOL PROFILES     modt5310
C     IRD1 = 1 OR IRD2 = 1   SELDOM USED                                modt5320
C                                                                       modt5330
C     TITLE=   IDENTIFICATION OF NEW MODEL ATMOSPHERE                   modt5340
C                                                                       modt5350
C                                                                       modt5360
C     THE FOLLOWING CARDS ARE READ IN SUBROUTINE AERNSM                 modt5370
C                                                                       modt5380
CC------------------------ BEGIN ML LOOP                                modt5390
CC-                                                                     modt5400
CC-   CARD 2C1 ZMDL,P,T,WMOL(1),WMOL(2),WMOL(3),JCHAR                   modt5410
CC-   LAYER VARIABLES        WH,   WCO2,     WO,JCHAR (1 TO 13)         modt5420
CC-                        FORMAT(F10.3,5E10.3,15A1)                    modt5430
CC-                                                                     modt5440
CC-   ZMDL     ALTITUDE OF LAYER (KM)                                   modt5450
CC-   P        PRESSURE AT LAYER                                        modt5460
CC-   T        TEMPERATURE                                              modt5470
CC-   WMOL     READ, INTERPRETED AND MOVED INTO LAYER VARIABLES         modt5480
CC-   WH =     WATER VAPOR                                              modt5490
CC-   WCO2 =   CO2                                                      modt5500
CC-   WO =     OZONE                                                    modt5510
CC-                                                                     modt5520
CC-   JCHAR    FLAGS TO SPECIFY UNITS OR DEFAULTS FOR                   modt5530
CC-   P,T,WH,WCO2,WO,WN2O,WCO,.. AND WNH3,WHNO3                         modt5540
CC-   JCHAR BLANK DEFAULT TO M1,M2,M3,M4,M5,M6,MDEF WHEN AMOUNT ZERO    modt5550
CC-                                                                     modt5560
CC-      PARAMETERS - JCHAR = INPUT KEY                                 modt5570
CC-                                                                     modt5580
CC-   **  ACCEPTS VARIABLE UNITS ON PRESS AND TEMP                      modt5590
CC-                                                                     modt5600
CC-     JCHAR(1)                                                        modt5610
CC-                                                                     modt5620
CC-    " ",A           PRESSURE IN (MB)    OR BLANK                     modt5630
CC-        B              "     "  (ATM)                                modt5640
CC-        C              "     "  (TORR)                               modt5650
CC-       1-6          DEFAULT TO SPECIFIED MODEL ATMOSPHERE            modt5660
CC-                                                                     modt5670
CC-     JCHAR(2)                                                        modt5680
CC-    " ",A           AMBIENT TEMPERATURE IN DEG(K)  OR BLANK          modt5690
CC-        B              "         "       "  " (C)                    modt5700
CC-       1-6          DEFAULT TO SPECIFIED MODEL ATMOSPHERE            modt5710
CC-                                                                     modt5720
CC-   ****************************************************************  modt5730
CC-   FOR MOLECULAR SPECIES ONLY                                        modt5740
CC-                                                                     modt5750
CC-     JCHAR             JCHAR(3-13)                                   modt5760
CC-                                                                     modt5770
CC-   " ",A            VOLUME MIXING RATIO (PPMV)                       modt5780
CC-       B            NUMBER DENSITY (CM-3)                            modt5790
CC-       C            MASS MIXING RATIO (GM(K)/KG(AIR))                modt5800
CC-       D            MASS DENSITY (GM M-3)                            modt5810
CC-       E            PARTIAL PRESSURE (MB)                            modt5820
CC-       F            DEW POINT TEMP (TD IN T(K)) - H2O ONLY           modt5830
CC-       G             "    "     "  (TD IN T(C)) - H2O ONLY           modt5840
CC-       H            RELATIVE HUMIDITY (RH IN PERCENT) - H2O ONLY (3) modt5850
CC-       I            AVAILABLE FOR USER DEFINITION                    modt5860
CC-      1-6           DEFAULT TO SPECIFIED MODEL ATMOSPHERE            modt5870
CC-                                                                     modt5880
CC-   ****************************************************************  modt5890
CC-                                                                     modt5900
CC-   CARD 2C2   (WMOL(J),J=4,11)                                       modt5910
CC-   VARIABLES  WN2O,WCO,WCH4,WO2,WNO,WSO2,WNO2,WNH3                   modt5920
CC-                        FORMAT(8E10.3)                               modt5930
CC-                                                                     modt5940
CC-   CARD 2C2   WMOL(12)             (CONT)                            modt5950
CC-   VARIABLE   WHNO3                                                  modt5960
CC-                        FORMAT(8E10.3)                               modt5970
CC-                                                                     modt5980
CC-                                                                     modt5990
CC-   WMOL     READ, INTERPRETED AND MOVED INTO LAYER VARIABLES         modt6000
CC-   WN2O =   N2O                                                      modt6010
CC-   WCO  =   CO                                                       modt6020
CC-   WCH4 =   CH4                                                      modt6030
CC-   WO2  =   O2                                                       modt6040
CC-   WNO  =   NO                                                       modt6050
CC-   WSO2 =   SO2                                                      modt6060
CC-   WNO2 =   NO2                                                      modt6070
CC-   WNH3 =   NH3                                                      modt6080
CC-   WHNO3 =  HNO3                                                     modt6090
CC-                                                                     modt6100
CC-   ****************************************************************  modt6110
CC-                                                                     modt6120
CC- CARD 2C3     AHAZE,EQLWCZ,RRATZ,IHA1,ICLD1,IVUL1,ISEA1,ICHR         modt6130
CC-                        FORMAT(10X,3F10.3,5I5)                       modt6140
CC-                                                                     modt6150
CC-  'AHAZE' AEROSOL SCALING FACTOR (EQUAL TO THE VISIBLE [0.55UM]      modt6160
CC-          EXTINCTION COEFFICIENT AT ALTZ)                            modt6170
CC-                                                                     modt6180
CC-          [NOTE ** ONE OF AHAZE OR EQLWCZ IS ALLOWED ]               modt6190
CC-                                                                     modt6200
CC-  'EQLWCZ' EQUIVALENT LIQUID WATER CONTENT ( GM/M3) AT ALT Z         modt6210
CC-           FOR THE AEROSOL, CLOUD OR FOG MODELS                      modt6220
CC-                                                                     modt6230
CC-   RRATZ=RAIN RATE (MM/HR) AT ALT Z                                  modt6240
CC-                                                                     modt6250
CC-   'IHA1' AEROSOL EXTINCTION AND METEOROLOGICAL RANGE CONTROL FOR    modt6260
CC-          THE ALTITUDE, Z                                            modt6270
CC-                                                                     modt6280
CC-   'ICLD1' CLOUD EXTINCTION CONTROL FOR THE ALTITUDE, Z              modt6290
CC-                                                                     modt6300
CC-   WHEN USING 'ICLD1' IT IS NECESSARY TO SET 'ICLD' (NON ZERO)       modt6310
CC-                                                                     modt6320
CC-   'IVUL1' STRATOSPHERIC AEROSOL PROFILE AND EXTINCTION CONTROL FOR  modt6330
CC-   THE ALTITUDE Z                                                    modt6340
CC-                                                                     modt6350
CC-   ONLY ONE OF 'IHA1','ICLD1' OR 'IVUL1' IS ALLOWED                  modt6360
CC-   IF 'IHA1' NE 0  THEN OTHERS IGNORED                               modt6370
CC-   IF 'IHA1' EQ 0 AND 'ICLD1' NE 0 THEN USE 'ICLD1'                  modt6380
CC-                                                                     modt6390
CC-   IF 'AHAZE' AND 'EQLWCZ' ARE BOTH ZERO  DEFAULT PROFILE LOADED     modt6400
CC-      FROM 'IHAZ1','ICLD1','IVUL1'                                   modt6410
CC-                                                                     modt6420
CC-   'ISEA1'  AEROSOL SEASON CONTROL FOR THE ALTITUDE,Z                modt6430
CC-                                                                     modt6440
CC-   'ICHR '  CHANGE PROFILE REGION  AT ALTITUDE Z                     modt6450
CC-            USED WHEN IHA1 IS 7 IN TWO ADJACENT ALTITUDE REGIMES     modt6460
CC-                                                                     modt6470
CC------------- END ML LOOP                                             modt6480
C***********************************************************************modt6490
C                                                                       modt6500
C   IHAZE = 7 OR ICLD = 11 INPUT                                        modt6510
C                                                                       modt6520
C   CARD 2D  (IREG(II),II=1,4)                                          modt6530
C                        FORMAT(4I5)                                    modt6540
C                                                                       modt6550
C             'IREG' SPECIFIES WHICH OF THE FOUR AEROSOL REGIONS        modt6560
C              A USER DEFINED AEROSOL MODEL IS USED (IHAZE=7/ICLD=11)   modt6570
C                                                                       modt6580
C             [NOTE   REGIONS DEFAULT TO                                modt6590
C             0-2 ,3-10,11-30,35-100 KM                                 modt6600
C             AND CAN BE OVERRIDDEN WITH 'IHA1' SETTINGS IN MODEL 7]    modt6610
C                                                                       modt6620
C             IREG = 0  USE DEFAULT VALUES FOR THIS REGION              modt6630
C                                                                       modt6640
C             IREG = 1  READ EXTINCTION ABSORPTION ASYMMETRY            modt6650
C             FOR A REGION                                              modt6660
C                                                                       modt6670
C   CARD 2D1 AWCCON,TITLE                                               modt6680
C                        FORMAT(E10.3,18A4)                             modt6690
C                                                                       modt6700
C            'AWWCON' EQUIVALENT LIQUID WATER CONTENT(GM/M3)            modt6710
C             FOR A REGION                                              modt6720
C                                                                       modt6730
C             'TITLE' FOR A AEROSOL REGION                              modt6740
C                                                                       modt6750
C    CARD 2D2 (VX(I),EXTC(N,I),ABSC(N,I),ASYM(N,I),I=1,47)              modt6760
C                   FORMAT(4(F6.2,2F7.5,F6.4))                          modt6770
C                                                                       modt6780
C             WHERE  N = IREG(II)   FOR UP TO 4 ALTITUDE REGIONS        modt6790
C             USER DEFINED AEROSOL  OR CLOUD EXTINCTION AND ABSORPTION  modt6800
C              COEFFICIENTS WHEN IHAZE = 7 OR ICLD = 11                 modt6810
C                                                                       modt6820
C     VX(I)    = WAVELENGTH OF AEROSOL COEFFICIENT                      modt6830
C                   (NOT USED BY PROGRAM BUT CORRESPONDING TO           modt6840
C                   WAVELENGTHS DEFINED IN ARRAY VX2                    modt6850
C                   IN SUBROUTINE EXTDTA)                               modt6860
C                                                                       modt6870
C     EXTC(N,I) = AEROSOL EXTINCTION COEFFICIENT                        modt6880
C     ABSC(N,I) = AEROSOL ABSORPTION COEFFICIENT                        modt6890
C     ASYM(N,I) = AEROSOL ASYMMETRY PARAMETER                           modt6900
C     WHERE  N = IREG(II)   FOR UP TO 4 ALTITUDE REGIONS                modt6910
C                                                                       modt6920
C***********************************************************************modt6930
C                                                                       modt6940
C     CARD 3    H1,H2,ANGLE,RANGE,BETA,RO,LEN    FORMAT(6F10.3,I5)      modt6950
C            USED TO DEFINE THE GEOMETRICAL PATH PARAMETERS FOR A GIVEN modt6960
C            PROBLEM.                                                   modt6970
C                                                                       modt6980
C     OR IF IEMSCT=3 ; CARD 3 H1,H2,ANGLE,IDAY,RO,ISOURC,ANGLEM         modt6990
C                                                                       modt7000
C     H1  =  INITIAL ALTITUDE(KM)                                       modt7010
C     H2  =  FINAL ALTITUDE(KM)                                         modt7020
C                                                                       modt7030
C                   IN THE RADIANCE MODE OF THE PROGRAM EXECUTION       modt7040
C            H1, THE INITIAL ALTITUDE,ALWAYS DEFINES THE POSITION OF    modt7050
C            THE OBSERVER (OR SENSOR).                                  modt7060
C                                                                       modt7070
C     ANGLE =INITIAL ZENITH ANGLE (DEGREES) AS MEASURED FROM H1         modt7080
C     [NOTE: ANGLE = 0 LOOKS STRAIGHT UP.                               modt7090
C            ANGLE IS DEFINED  FROM 0 TO 180 DEGREES ]                  modt7100
C                                                                       modt7110
C     RANGE =PATH LENGTH (KM)                                           modt7120
C     BETA  =EARTH CENTER ANGLE SUBTENDED BY H1 AND H2 (DEGREES)        modt7130
C                                                                       modt7140
C     RO =   RADIUS OF THE EARTH (KM) AT THE PARTICULAR GEOGRAPHICAL    modt7150
C            LOCATION AT WHICH THE CALCULATION IS TO BE PERFORMED.      modt7160
C              IF RO BLANK PROGRAM USES RADIUS FOR APPROPRIATE MODEL    modt7170
C              ATMOSPHERE. (MODEL 0 OR 7 DEFAULT = 6371.23 KM)          modt7180
C                                                                       modt7190
C     LEN =0 FOR NORMAL OPERATION OF PROGRAM                            modt7200
C         =1 LONG PATH THROUGH THE TANGENT HEIGHT                       modt7210
C                                                                       modt7220
C            IT IS NOT NECESSARY TO SPECIFY EVERY QUANTITY GIVEN ABOVE  modt7230
C            ONLY THOSE THAT ADEQUATELY DESCRIBE THE PROBLEM ACCORDING  modt7240
C            TO THE PARAMETER ITYPE                                     modt7250
C                                                                       modt7260
C            ITYPE=1 READ H1,RANGE                                      modt7270
C                 =2 READ H1,H2,ANGLE   OR H1,H2,RANGE   OR H1,H2,BETA  modt7280
C                    OR H1,ANGLE,RANGE                                  modt7290
C                 =3 READ H1,ANGLE OR H1,H2                             modt7300
C                    [NOTE: H2 IS INTERPRETED AS HMIN FOR THIS CASE]    modt7310
C                                                                       modt7320
C--------------                                                         modt7330
C     CARD 3    OPTION  IEMSCT = 3                                      modt7340
C    'IDAY'     DAY OF THE YEAR, I.E. FROM 1 TO 365  (IEMSCT = 3)       modt7350
C                                                                       modt7360
C     ISOURC=0  EXTRATERRESTRIAL SOURCE IS THE SUN                      modt7370
C           =1  EXTRATERRESTRIAL SOURCE IS THE MOON                     modt7380
C                                                                       modt7390
C     ANGLEM=PHASE ANGLE OF THE MOON, I.E. THE ANGLE FORMED             modt7400
C            BY THE SUN, MOON AND EARTH (REQUIRED IF ISOURC=1)          modt7410
C                                                                       modt7420
C***********************************************************************modt7430
C                                                                       modt7440
C     CARD 3A1   IPARM,IPH,IDAY,ISOURC           (IEMSCT=2)             modt7450
C                          FORMAT(4I5)                                  modt7460
C              INPUT CARD FOR SOLAR/LUNAR SCATTERED RADIATION WHEN      modt7470
C              IEMSCT = 2                                               modt7480
C                                                                       modt7490
C              IPARM =0,1,2 AND CONTROLS THE METHOD OF SPECIFYING THE   modt7500
C              SOLAR/LUNAR GEOMETRY ON CARD 3A2.                        modt7510
C                     (SEE DEFINITION BELOW FOR CARD 3A2)               modt7520
C                                                                       modt7530
C              IPH DETERMINES THE TYPE OF PHASE FUNCTION USED IN THE    modt7540
C               CALCULATION                                             modt7550
C                                                                       modt7560
C     IPH=0     HENYEY-GREENSTEIN AEROSOL PHASE FUNCTION                modt7570
C        =1     USER SUPPLIED AEROSOL PHASE FUNCTION (SEE CARD 3B)      modt7580
C        =2     MIE GENERATED DATA BASE OF AEROSOL PHASE FUNCTIONS FOR  modt7590
C               THE LOWTRAN MODELS.                                     modt7600
C                                                                       modt7610
C     IDAY=     DAY OF THE YEAR, I.E. FROM 1 TO 365   (REQUIRED)        modt7620
C                                                                       modt7630
C     ISOURC=0  EXTRATERRESTRIAL SOURCE IS THE SUN                      modt7640
C           =1  EXTRATERRESTRIAL SOURCE IS THE MOON                     modt7650
C                                                                       modt7660
C***********************************************************************modt7670
C                                                                       modt7680
C     CARD 3A2  PARM1,PARM2,PARM3,PARM4,TIME,PSIPO,ANGLEM,G             modt7690
C                          FORMAT(8F10.3)                (IEMSCT=2)     modt7700
C              INPUT CARD FOR SOLAR/LUNAR SCATTERED RADIATION WHEN      modt7710
C              IEMSCT = 2                                               modt7720
C              DEFINITIONS OF PARM1,PARM2,PARM3,PARM4 DETERMINED BY     modt7730
C              VALUE OF IPARM ON CARD 3A1.                              modt7740
C                                                                       modt7750
C                       FOR IPARM=0                                     modt7760
C                                                                       modt7770
C     PARM1= OBSERVER LATITUDE (-90 TO +90)                             modt7780
C          NOTE- IF ABS(PARM1) IS GREATER THAN 89.5 THE OBSERVER IS     modt7790
C          ASSUMED TO BE AT EITHER THE NORTH OR THE SOUTH POLE.  IN     modt7800
C          THAT CASE THE PATH AZIMUTH IS UNDEFINED.  THE DIRECTION OF   modt7810
C          LINE OF SIGHT MUST BE SPECIFIED AS THE LONGITUDE ALONG WHICH modt7820
C          THE PATH LIES. THIS QUANTITY RATHER THAN THE USUAL AZIMUTH   modt7830
C          IS READ IN                                                   modt7840
C     PARM2= OBSERVER LONGITUDE (0 TO 360)                              modt7850
C     PARM3= SOURCE (SUN OR MOON) LATITUDE                              modt7860
C     PARM4= SOURCE (SUN OR MOON) LONGITUDE                             modt7870
C                                                                       modt7880
C                       FOR IPARM=1                                     modt7890
C        (IDAY AND TIME MUST BE SPECIFIED,CANNOT BE USED WITH ISOURC=1) modt7900
C                                                                       modt7910
C                                                                       modt7920
C     PARM1= OBSERVER LATITUDE (-90 TO +90)                             modt7930
C     PARM2= OBSERVER LONGITUDE (0 TO 360)                              modt7940
C             PARM3,PARM4 ARE NOT REQUIRED                              modt7950
C                                                                       modt7960
C     [NOTE: THAT THE CALCULATED APPARENT SOLAR ZENITH                  modt7970
C            ANGLE IS THE ZENITH ANGLE AT H1 OF THE REFRACTED           modt7980
C            PATH TO THE SUN AND IS LESS THAN THE ASTRONOMICAL          modt7990
C            SOLAR ZENITH ANGLE.  THE DIFFERENCE BETWEEN THE            modt8000
C            TWO ANGLES IS NEGLIGIBLE FOR ANGLES LESS THAN 80           modt8010
C            DEGREES.]                                                  modt8020
C                                                                       modt8030
C                        FOR IPARM=2                                    modt8040
C                                                                       modt8050
C     PARM1= AZIMUTHAL ANGLE BETWEEN THE OBSERVER'S LINE OF SIGHT       modt8060
C            AND THE OBSERVER-TO-SUN PATH, MEASURED FROM THE LINE       modt8070
C            OF SIGHT, POSITIVE EAST OF NORTH, BETWEEN -180 AND 180     modt8080
C     PARM2= THE SUN'S ZENITH ANGLE                                     modt8090
C                                                                       modt8100
C              PARM3,PARM4 ARE NOT REQUIRED                             modt8110
C                                                                       modt8120
C                                                                       modt8130
C              REMAINING CONTROL PARAMETERS                             modt8140
C                                                                       modt8150
C     TIME=  GREENWICH TIME IN DECIMAL HOURS, I.E. 8:45 AM IS 8.75,     modt8160
C            5:20 PM IS 17.33 ETC. (USED WITH IPARM = 1)                modt8170
C                                                                       modt8180
C     PSIPO= PATH AZIMUTH (DEGREES EAST OF NORTH, I.E. DUE NORTH IS 0.0 modt8190
C            DUE EAST IS 90.0 ETC.  (USED WITH IPARM = 0 OR 1)          modt8200
C                                                                       modt8210
C     ANGLEM=PHASE ANGLE OF THE MOON, I.E. THE ANGLE FORMED             modt8220
C            BY THE SUN, MOON AND EARTH (REQUIRED IF ISOURC=1)          modt8230
C                                                                       modt8240
C     G=     ASYMMETRY FACTOR FOR USE WITH HENYEY-GREENSTEIN            modt8250
C            PHASE FUNCTION (USED WITH IPH = 0)                         modt8260
C                                                                       modt8270
C***********************************************************************modt8280
C     CARD 3B1 NANGLS          (IPH=1)                                  modt8290
C                   FORMAT(I5)                                          modt8300
C                                                                       modt8310
C              INPUT CARD FOR USER DEFINED PHASE FUNCTIONS WHEN IPH=1.  modt8320
C                                                                       modt8330
C     NANGLS=  NUMBER OF ANGLES FOR THE USER DEFINED PHASE              modt8340
C              FUNCTIONS(MAXIMUM OF 50)                                 modt8350
C                                                                       modt8360
C***********************************************************************modt8370
C                                                                       modt8380
C     CARD 3B2(1 TO NANGLS)    (IPH=1)                                  modt8390
C             (ANGF(I),F(1,I),F(2,I),F(3,I),F(4,I),I=1,NANGLS)          modt8400
C                   FORMAT(5E10.3)                                      modt8410
C              INPUT CARD FOR USER DEFINED PHASE FUNCTION WHEN IPH=1.   modt8420
C              FOR AVERAGE FREQUENCY OF CALCULATION                     modt8430
C                                                                       modt8440
C     ANGF(I)= PHASE ANGLE IN DECIMAL DEGREES                           modt8450
C                   (0.0 TO 180.0)                                      modt8460
C                                                                       modt8470
C     F(1,I)=  USER DEFINED PHASE FUNCTION AT ANGF(I)                   modt8480
C              BOUNDARY LAYER DEFAULTS TO (0 TO 2KM))                   modt8490
C                                                                       modt8500
C     F(2,I)=  USER DEFINED PHASE FUNCTION AT ANGF(I)                   modt8510
C              TROPOSPHERE DEFAULTS TO (2 TO 10 KM)                     modt8520
C                                                                       modt8530
C     F(3,I)=  USER DEFINED PHASE FUNCTION AT ANGF(I)                   modt8540
C              STRATOSPHERE DEFAULTS TO (10 TO 30 KM)                   modt8550
C                                                                       modt8560
C     F(4,I)=  USER DEFINED PHASE FUNCTION AT ANGF(I)                   modt8570
C              MESOSPHERE DEFAULTS TO (30 TO 100 KM)                    modt8580
C                                                                       modt8590
C***********************************************************************modt8600
C                                                                       modt8610
C     CARD 4    V1, V2, DV                       FORMAT(3F10.3)         modt8620
C                                                                       modt8630
C              THE SPECTRAL RANGE OVER WHICH DATA ARE REQUIRED AND      modt8640
C              THE SPECTRAL INCREMENTS AT WHICH THE DATA ARE TO BE      modt8650
C              CALCULATED  IS DETERMINED BY CARD 4.                     modt8660
C                                                                       modt8670
C     V1 =     INITIAL FREQUENCY (WAVENUMBER CM-1 )                     modt8680
C     V2 =     FINAL FREQUENCY(WAVENUMBER CM-1 )                        modt8690
C     DV =     FREQUENCY INCREMENT (OR STEP SIZE) (CM-1)                modt8700
C              NOTE: DV MUST BE A MULTIPLE OF 5 CM-1                    modt8710
C              ANY STEP SIZE .GT. 5 CM-1 WILL UNDERSAMPLE THE RESULTS   modt8720
C                                                                       modt8730
C              SCANNING FUNCTION IS AVAILABLE TO PROPERLY HANDLE DATA   modt8740
C              WITH LOWER RESOLUTION THAN 20CM-1 LOWTRAN 7              modt8750
C                                                                       modt8760
C***********************************************************************modt8770
C                                                                       modt8780
C     CARD 5    IRPT                             FORMAT(I5)             modt8790
C     IRPT=0  TO END PROGRAM                                            modt8800
C         =1  READ ALL DATA CARDS (1,2,3,4,5)                           modt8810
C         =2  NOT USED  (WILL STOP PROGRAM)                             modt8820
C         =3  READ CARD 3   THE GEOMETRY CARD AND CARD 5                modt8830
C         =4  READ CARD 4 TO CHANGE FREQUENCY AND CARD 5                modt8840
C     GT 4 OR IRPT=2 WILL CAUSE PROGRAM TO STOP                         modt8850
C                                                                       modt8860
C     IRPT GE 1 USED FOR MULTIPLE RUNS OF LOWTRAN                       modt8870
C     WARNING IRPT 3 CANNOT BE USED WHEN RUNNING MULTIPLE SCATTERING    modt8880
C     CASES WITH SOLAR SCATTERING                                       modt8890
C                                                                       modt8900
C     REFERENCES                                                        modt8910
C                                                                       modt8920
C       (1980) ATMOSPHERIC TRANSMITTANCE/RADIANCE- COMPUTER CODE        modt8930
C       LOWTRAN 5 AFGL-TR-80-0067                                       modt8940
C       KNEIZYS, F. X.,SHETTLE, E. P. ,GALLERY, W. O.,CHETWYND, J. H.,  modt8950
C       ABREU, L. W., SELBY, J. E. A., FENN, R. W. ,MCCLATCHEY R. A.    modt8960
C                                                                       modt8970
C       (1983) ATMOSPHERIC TRANSMITTANCE/RADIANCE- COMPUTER CODE        modt8980
C       LOWTRAN 6  AFGL TR 83 0187                                      modt8990
C       KNEIZYS, F. X.,SHETTLE, E. P. ,GALLERY, W. O.,CHETWYND, J. H.,  modt9000
C       ABREU, L. W., SELBY, J. E. A., CLOUGH, S. A., FENN, R. W.       modt9010
C                                                                       modt9020
C       (1988) ATMOSPHERIC TRANSMITTANCE/RADIANCE- COMPUTER CODE        modt9030
C       LOWTRAN 7 AFGL-TR-88-XXXX                                       modt9040
C       KNEIZYS, F. X.,SHETTLE, E. P. ,ANDERSON G. P.,ABREU ,L. W.      modt9050
C       CHETWYND, J H,SELBY, J. E. A. ,CLOUGH, S. A.,GALLERY, W. O      modt9060
C                                                                       modt9070
C       (1988) LOWTRAN 7 COMPUTER CODE : USER'S MANUAL AFGL-TR-88-XXXX  modt9080
C       KNEIZYS, F. X.,SHETTLE, E. P. ,ANDERSON G. P.,ABREU ,L. W.      modt9090
C       CHETWYND, J H,SELBY, J. E. A. ,CLOUGH, S. A.,GALLERY, W. O      modt9100
C                                                                       modt9110
C       MOLECULAR TRANSMISSION BAND MODELS FOR LOWTRAN AFGL-TR-86-0272  modt9120
C       PIERLUISSI, J. H., MARAGOUDAKIS, C. E.                          modt9130
C                                                                       modt9140
C       MULTIPLE SCATTERING TREATMENT FOR USE IN                        modt9150
C       THE LOWTRAN AND FASCODE MODELS  AFGL-TR-86-0073                 modt9160
C       ISAACS, R. G., WANG, W. C., WORSHAM, R. D.,GOLDENBERG S.        modt9170
C                                                                       modt9180
C       AFGL ATMOSPHERIC CONSTITUENT PROFILES (0 TO 120KM)              modt9190
C                                              AFGL-TR-86-0110          modt9200
C       ANDERSON, G. P., CLOUGH, S. A., KNEIZYS, F. X.,                 modt9210
C       CHETWYND, J. H., SHETTLE, E. P.                                 modt9220
C                                                                       modt9230
C       PROGRAM FOR ATMOSPHERIC TRANSMITTANCE RADIANCE/CALCULATIONS     modt9240
C       FSCATM                                  AFGL-TR-83-0065         modt9250
C       GALLERY, W. O., KNEIZYS, F. X., AND CLOUGH, S. A.               modt9260
C                                                                       modt9270
C       AFGL HANDBOOK OF GEOPHYSICS AND THE SPACE ENVIRONMENT           modt9280
C       EDITOR, A. S. JURSA  CHAPTER 18 1985                            modt9290
C                                                                       modt9300
C       MODELS OF THE AEROSOLS OF THE LOWER ATMOSPHERE AND THE EFFECTS  modt9310
C       OF HUMIDITY VARIATIONS ON THEIR OPTICAL PROPERTIES              modt9320
C       SHETTLE, E.P. AND FENN, R. W.            AFGL-TR-79-0214        modt9330
C                                                                       modt9340
C       OPTICAL PROPAGATION IN THE ATMOSPHERE    AGARD-CP-183  1975     modt9350
C       SHETTLE, E. P., AND FENN, R. W.          NTIS (NO AD-A028-615)  modt9360
C                                                                       modt9370
C                                                                       modt9380
C       ATMOSPHERIC ATTENUATION OF MILLIMETER AND SUBMILLIMETER         modt9390
C       WAVES:  MODEL AND COMPUTER CODE          AFGL-TR-79-0253        modt9400
C       FALCONE,V. J. JR.,ABREU,L. W. AND SHETTLE, E. P.                modt9410
C                                                                       modt9420
C       LOWTRAN  PLUS ULTRAVIOLET O2 ABSORPTION                         modt9430
C                                                                       modt9440
C       REFERENCES- JOHNSTON, ET AL, J GEOPHYS RES, 89,11661-11665,1984.modt9450
C                                                                       modt9460
C       FREQUENCY RANGE: 50000-36000CM-1 FOR HERZBERG CALCULATION       modt9470
C                                                                       modt9480
C       THE SCHUMANN-RUNGE BANDS (PARTICULARLY THE 1,0 AND 0,0) ARE NOT modt9490
C       INCLUDED IN THE CALCULATIONS (50000 AND 49400 CM-1).            modt9500
C       THE HERZBERG BANDS ARE APPROXIMATED BY AN EXTRAPOLATION OF THE  modt9510
C       HERZBERG CONTINUUM (41322-36000 CM-1).                          modt9520
C                                                                       modt9530
C***********************************************************************modt9540
      CALL DRIVER                                                       modt9550
      STOP                                                              modt9560
C@    END                                                               modt9570
C@    THE FOLLOWING TIME AND DATE SUBROUTINES APPLY TO A CDC 6600       modt9580
C@    SUBROUTINE FDATE(HDATE)                                           modt9590
C@    CALL DATE(GDATE)                                                  modt9600
C@    HDATE=SHIFT(GDATE,6)                                              modt9610
C@    RETURN                                                            modt9620
C@    END                                                               modt9630
C@    SUBROUTINE FCLOCK(HTIME)                                          modt9640
C@    CALL CLOCK(GTIME)                                                 modt9650
C@    HTIME=SHIFT(GTIME,6)                                              modt9660
C@    RETURN                                                            modt9670
      END                                                               modt9680
      SUBROUTINE STDMDL                                                 stdm 100
      INCLUDE 'parameter.list'
C***********************************************************************stdm 110
C     THIS SUBROUTINE LOADS ONE OF THE 6 STANDARD ATMOSPHERIC PROFILES  stdm 120
C     INTO COMMON/MODEL/ AND CALCULATES THE DENSITIES OF THE            stdm 130
C     VARIOUS ABSORBING GASES AND AEROSOLS                              stdm 140
C***********************************************************************stdm 150
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

      CHARACTER*8 CNAMEX                                       
C
C     TRANS VARIABLES
C                                             
      COMMON /NAMEX/CNAMEX(MMOLX)                                              
      COMMON /MDATAX/ WMOLXT(MMOLX,laydim)                                  
      COMMON /MODELX/ DNSTYX(MMOLX,LAYDIM)                                  
c
c
c
      COMMON RELHUM(LAYDIM),HSTOR(LAYDIM),ICH(4),VH(17),TX(65),W(65)  
      COMMON IMSMX,WPATH(LAYTHR,65),TBBY(LAYTHR),PATM(LAYTHR),NSPEC,   
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX2(47),AWCCON(5)  
      COMMON /MODEL/ ZM(LAYDIM),PM(LAYDIM),TM(LAYDIM),RFNDX(LAYDIM),
     1  DENSTY(65,LAYDIM),CLDAMT(LAYDIM),RRAMT(LAYDIM),EQLWC(LAYDIM),
     1  HAZEC(LAYDIM)
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,iscrch
      COMMON /CARD1/ MODEL,ITYPE,IEMSCT,M1,M2,M3,IM,NOPRNT,TBOUND,SALB  stdm 170
     1  ,MODTRN                                                         stdm 180
      LOGICAL MODTRN                                                    stdm 190
      COMMON /CARD2/ IHAZE,ISEASN,IVULCN,ICSTL,ICLD,IVSA,VIS,WSS,WHH,   stdm 200
     1    RAINRT                                                        stdm 210
      COMMON /CARD3/ H1,H2,ANGLE,RANGE,BETA,RE,LEN                      stdm 220
C     COMMON /CARD4/ V1,V2,DV                                           stdm 230
      COMMON/CARD4/IV1,IV2,IDV,IFWHM                                    stdm 240
      COMMON /MDATA/P(LAYDIM),T(LAYDIM),WH(LAYDIM),WCO2(LAYDIM),
     x WO(LAYDIM),WN2O(LAYDIM),WCO(LAYDIM),WCH4(LAYDIM),WO2(LAYDIM)
      COMMON /MDATA1/ WNO(LAYDIM),WSO2(LAYDIM),WNO2(LAYDIM),
     x WNH3(LAYDIM),WAIR(LAYDIM)    
      COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     stdm 290
      COMMON /CNTRL/ KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT               stdm 300
      COMMON /DEAMT/ DENM(35,laytwo),DENP(35,laythr+1)
      COMMON /MLATM / ALT(50),PMATM(50,6),TMATM(50,6),AMOL(50,8,6)      
C     XLOSCH = LOSCHMIDT'S NUMBER,MOLECULES CM-2,KM-1                   stdm 370
      DATA PZERO /1013.25/,TZERO/273.15/,XLOSCH/2.6868E24/              stdm 380
C     RV GAS CONSTANT FOR WATER IN MB/(GM M-3 K)                        stdm 390
C     CON CONVERTS WATER VAPOR FROM GM M-3 TO MOLECULES CM-2 KM-1       stdm 400
      DATA RV/4.6152E-3/,CON/3.3429E21/                                 stdm 410
C     CONSTANTS FOR INDEX OF REFRACTION, AFTER EDLEN, 1965              stdm 420
      DATA A0/83.42/,A1/185.08/,A2/4.11/,                               stdm 430
     X     B1/1.140E5/,B2/6.24E4/,C0/43.49/,C1/1.70E4/                  stdm 440
C                                                                       stdm 450
C     F(A) IS SATURATED WATER WAPOR DENSITY AT TEMP T,A=TZERO/T         stdm 460
      F(A)=EXP(18.9766-14.9595*A-2.43882*A*A)*A                         stdm 470
C                                                                       stdm 480
C     CONJOE=(1/XLOSCH)*1.E5*1.E-6 WITH                                 stdm 490
C        1.E5 ARISING FROM CM TO KM CONVERSION AND                      stdm 500
C        1.E-6  "       "  PPMV                                         stdm 510
C                                                                       stdm 520
      CONJOE = 3.7194E-21                                               stdm 530
C                                                                       stdm 540
C     H20 CONTINUUM IS STORED AT 296 K RHZERO IS AIR DENSITY AT 296 K   stdm 550
C     IN UNITS OF LOSCHMIDT'S                                           stdm 560
C                                                                       stdm 570
      RHZERO=(273.15/296.0)                                             stdm 580
C                                                                       stdm 590
C     LOAD ATMOSPHERE PROFILE INTO /MODEL/                              stdm 600
cc      mdl = model
cc      if(mdl.eq.0) mdl = 6
cc      if(mdl.eq.7) mdl = 6
        DO 25 I=1,ML                                                      stdm 610
cc        do 26 kk = 1,49
cc        kj = kk
cc        if(zm(i). eq . alt(kk) ) go to 27
cc        if(zm(i). gt . alt(kk) ) go to 26
cc        go to 27
cc26      continue
cc27      y1 = amol(kj  ,8,mdl)
cc        y2 = amol(kj+1,8,mdl)
cc        x1 = alt(kj)         
cc        x2 = alt(kj+1)         
cc        am = (y2-y1)/(x2-x1)
cc        c  = y1 - am*x1
cc      denc =  zm(i) * am + c
      PM(I)=P(I)                                                        stdm 620
      TM(I)=T(I)                                                        stdm 630
      PP=PM(I)                                                          stdm 640
      TT=TM(I)                                                          stdm 650
      PSS=PP/PZERO                                                      stdm 660
      TSS=TZERO/TT                                                      stdm 670
      F1=(PP/PZERO)/(TT/TZERO)                                          stdm 680
      F2=(PP/PZERO)*SQRT(TZERO/TT)                                      stdm 690
cjv 8/24/95
cjv      F3=(PP/PZERO)/(296.15/tt)
      F3=(PP/PZERO)/(tt/296.15)
cjv ^
      WTEMP=WH(I)                                                       stdm 700
C     SCALED H2O DENSITY                                                stdm 710
      IF (ZM(I).GT.2.0) GO TO 15                                        stdm 720
      TS = TZERO / TT                                                   stdm 730
15    CONTINUE                                                          stdm 740
C     UNIFORMALLY MIXED GASES DENSITIES                                 stdm 750
C     ORIGINAL TRANSMITTANCE MODEL FOR CO2+ HAS BEEN REPLACED.          stdm 760
C     DENSTY(2,I)=F1*F2**0.75                                           stdm 770
C     UV OZONE                                                          stdm 780
C     THE UNIT FOR O3 HAS BEEN CHANGED FROM G/M**3 TO PPMV.             stdm 790
      DENSTY(8,I)= CONJOE   *WAIR(I)    *WO(I)                          stdm 800
C     IR OZONE                                                          stdm 810
C     ORIGINAL TRANSMITTANCE MODEL HAS BEEN REPLACED.                   stdm 820
C     DENSTY(3,I)= DENSTY(8,I)*F2**0.4                                  stdm 830
C     N2 CONTINUUM                                                      stdm 840
cc    DENSTY(4,I)=0.781*F1*F2                                           stdm 850
      DENSTY(4,I)=0.781*F3*F3                                           stdm 850
      RHOAIR = F1                                                       stdm 870
      RHOH2O = CON *WTEMP/XLOSCH                                        stdm 880
      RHOFRN = RHOAIR - RHOH2O                                          stdm 890
      DENSTY(5,I)= XLOSCH*RHOH2O**2/RHZERO                              stdm 900
C     FOREIGN BROADENED                                                 stdm 910
      DENSTY(10,I)= XLOSCH*RHOH2O*RHOFRN/RHZERO                         stdm 920
C     MOLECULAR SCATTERING                                              stdm 930
      DENSTY(6,I) = F1                                                  stdm 940
C     RELITIVE HUMIDITY WEIGHTED BY BOUNDRY LAYER AEROSOL (0 TO 2 KM)   stdm 950
C                                                                       stdm 960
C     LOG WEIGHTING OF REL HUMIDITY                                     stdm 970
C                                                                       stdm 980
      RELH = RELHUM(I)                                                  stdm 990
      IF(RELHUM(I). GT.99.) RELH = 99.                                  stdm1000
      RHLOG = ALOG(100. - RELH)                                         stdm1010
C     DENSTY(15,I)=RELHUM(I)*DENSTY(7,I)                                stdm1020
      DENSTY(15,I)=RHLOG    *DENSTY(7,I)                                stdm1030
C     DENSITY (9,I) TEMP DEP OF WATER SET IN GEO                        stdm1040
      DENSTY(9,I)=0.                                                    stdm1050
C     IF(ICH(1).GT.7) DENSTY(15,I)=RELHUM(I)*DENSTY(12,I)               stdm1060
      IF(ICH(1).GT.7) DENSTY(15,I)=RHLOG    *DENSTY(12,I)               stdm1070
C     HNO3 IN ATM * CM /KM                                              stdm1080
C     NEW PROFILE IS IN UNIT OF PART PER 10**6 BY VOLUME                stdm1090
C     DENSTY(11,I)= F1* HMIX(I)*1.0E-4                                  stdm1100
C     IF(MODEL.EQ.7) DENSTY(11,I)=F1*HSTOR(I)*1.0E-4                    stdm1110
                     DENSTY(11,I)=F1*HSTOR(I)*1.0E-4                    stdm1120
C                                                                       stdm1130
C      O2 TEMP DEP                                                      stdm1140
C                                                                       stdm1150
      DT = TT  - 220.                                                   stdm1160
      WO2D       = CONJOE   *WAIR(I)    *WO2(I)  * PSS                  stdm1170
C                                                                       stdm1180
C     DT CAN BE NEGATIVE                                                stdm1190
C     EFFECTIVE DT CALCULATED IN TRANS                                  stdm1200
C                                                                       stdm1210
      DENSTY(1,I)  = WO2D * TT                                          stdm1220
      DENSTY(2,I)  = WO2D * DT * DT                                     stdm1230
      DENSTY(63,I) = WO2D                                               stdm1240
C                                                                       stdm1250
C   NEW  MICROWAVE TEMP  RAIN                                           stdm1260
C                                                                       stdm1270
      DENSTY(3,I) = 0.                                                  stdm1280
      DENSTY(61,I)= 0.                                                  stdm1290
      DENSTY(62,I)= 0.                                                  stdm1300
      IF(RRAMT(I) .NE. 0.) THEN                                         stdm1310
           DENSTY(3,I) = RRAMT(I)**(.63)                                stdm1320
           DENSTY(61,I)= DENSTY(3,I) * T(I)                             stdm1330
           DENSTY(62,I)= 1.                                             stdm1340
      ENDIF                                                             stdm1350
c     modtran densities. i e actual denisities.  in lowtran
c     some densities are scaled.  but we need the actual ones for uv-vis
c     stuff.  therefore density array's first dimension was extended by 2
c     density(64,i) will hold no2
c     density(65,i)will hold so2
      densty(64,i)=conjoe*wair(i)*wno2(i)
      densty(65,i)=conjoe*wair(i)*wso2(i)
C                                                                       stdm1360
C       CIRRIUS CLOUD                                                   stdm1370
       IF(ICLD.LT.18) DENSTY(16,I) = 0.                                 stdm1380
C                                                                       stdm1390
C     WHEN MODERATE RESOLUTION OPTION IS USED, ACTUAL DENSITIES ARE     stdm1400
C     NEEDED.  FOR ALL SPECIES, DENSITIES ARE STORED IN AMAGATS-CM/KM   stdm1410
C     WHERE 1 AMAGAT = 1 ATM AT STP.  FOR WATER, THE G/CM2 VALUE,       stdm1420
C     .1*WH(I), IS CONVERTED TO AMAGAT-CM BY MULTIPLYING BY             stdm1430
C     RT/(MOL WT), WHICH EQUALS 1244.2 CM3-ATM/GM/K.                    stdm1440
C                                                                       stdm1450
      CONO2 = CONJOE   *WAIR(I)  *WO2 (I)                               stdm1460
      IF(MODTRN)THEN                                                    stdm1470
          DENSTY(17,I)=WH(I)*124.42                                     stdm1480
          DENSTY(18,I) =0                                               stdm1490
          DENSTY(19,I) =0                                               stdm1500
          DENSTY(20,I) =0                                               stdm1510
          DENSTY(21,I) =0                                               stdm1520
          DENSTY(22,I) =0                                               stdm1530
          DENSTY(23,I) =0                                               stdm1540
          DENSTY(24,I) =0                                               stdm1550
          DENSTY(25,I) =0                                               stdm1560
          DENSTY(26,I) =0                                               stdm1570
          DENSTY(27,I) =0                                               stdm1580
          DENSTY(28,I) =0                                               stdm1590
          DENSTY(29,I) =0                                               stdm1600
          DENSTY(30,I) =0                                               stdm1610
          STORE=CONJOE*WAIR(I)                                          stdm1620
          DENSTY(31,I)=STORE*WO(I)                                      stdm1630
          DENSTY(32,I) =0                                               stdm1640
          DENSTY(33,I) =0                                               stdm1650
          DENSTY(34,I) =0                                               stdm1660
          DENSTY(35,I) =0                                               stdm1670
          DENSTY(36,I)=STORE*WCO2(I)                                    stdm1680
c         co2  continum
c         co2 continum now through band model
c
c         co2 = denc *WCO2(I)* 1.0e-6
cc        DENSTY(37,I)=co2 * pss * 1.e-15 * 296./tt 
          DENSTY(37,I) =0 
          DENSTY(38,I) =0                                               stdm1700
          DENSTY(39,I) =0                                               stdm1710
          DENSTY(40,I) =0                                               stdm1720
          DENSTY(41,I) =0                                               stdm1730
          DENSTY(42,I) =0                                               stdm1740
          DENSTY(43,I) =0                                               stdm1750
          DENSTY(44,I)=STORE*WCO(I)                                     stdm1760
          DENSTY(45,I) =0                                               stdm1770
          DENSTY(46,I)=STORE*WCH4(I)                                    stdm1780
          DENSTY(47,I)=STORE*WN2O(I)                                    stdm1790
          DENSTY(48,I) =0                                               stdm1800
          DENSTY(49,I) =0                                               stdm1810
          DENSTY(50,I)=STORE*WO2(I)                                     stdm1820
          DENSTY(51,I)=CONO2 *PSS**0.9353*TSS**( 0.1936)                stdm1830
CC        DENSTY(51,I) =0                                               stdm1840
          DENSTY(52,I)=STORE*WNH3(I)                                    stdm1850
          DENSTY(53,I) =0                                               stdm1860
          DENSTY(54,I)=STORE*WNO(I)                                     stdm1870
          DENSTY(55,I)=STORE*WNO2(I)                                    stdm1880
          DENSTY(56,I)=STORE*WSO2(I)                                    stdm1890
          DENSTY(57,I) =0                                               stdm1900
c
c         stuff the dnstyx array with the profile info for the
c         extra species. that is the additional species beyond
c         modtran's 12 regular species.
          do 400 ix = 1,nspecx
             dnstyx(ix,i)=store*wmolxt(ix,i)
 400      continue
c
      ELSE                                                              stdm1910
C  --- FOR H2O -----                                                    stdm1920
      CONH2O=WH(I)  *.1                                                 stdm1930
      DENSTY(17,I)=CONH2O*PSS**0.9810*TSS**( 0.3324)                    stdm1940
      DENSTY(18,I)=CONH2O*PSS**1.1406*TSS**(-2.6343)                    stdm1950
      DENSTY(19,I)=CONH2O*PSS**0.9834*TSS**(-2.5294)                    stdm1960
      DENSTY(20,I)=CONH2O*PSS**1.0443*TSS**(-2.4359)                    stdm1970
      DENSTY(21,I)=CONH2O*PSS**0.9681*TSS**(-1.9537)                    stdm1980
      DENSTY(22,I)=CONH2O*PSS**0.9555*TSS**(-1.5378)                    stdm1990
      DENSTY(23,I)=CONH2O*PSS**0.9362*TSS**(-1.6338)                    stdm2000
      DENSTY(24,I)=CONH2O*PSS**0.9233*TSS**(-0.9398)                    stdm2010
      DENSTY(25,I)=CONH2O*PSS**0.8658*TSS**(-0.1034)                    stdm2020
      DENSTY(26,I)=CONH2O*PSS**0.8874*TSS**(-0.2576)                    stdm2030
      DENSTY(27,I)=CONH2O*PSS**0.7982*TSS**( 0.0588)                    stdm2040
      DENSTY(28,I)=CONH2O*PSS**0.8088*TSS**( 0.2816)                    stdm2050
      DENSTY(29,I)=CONH2O*PSS**0.6642*TSS**( 0.2764)                    stdm2060
      DENSTY(30,I)=CONH2O*PSS**0.6656*TSS**( 0.5061)                    stdm2070
C  --- FOR O3                                                           stdm2080
      CONO3 = CONJOE   *WAIR(I)    *WO(I)                               stdm2090
      DENSTY(31,I)=CONO3 *PSS**0.4200*TSS**( 1.3909)                    stdm2100
      DENSTY(32,I)=CONO3 *PSS**0.4221*TSS**( 0.7678)                    stdm2110
      DENSTY(33,I)=CONO3 *PSS**0.3739*TSS**( 0.1225)                    stdm2120
      DENSTY(34,I)=CONO3 *PSS**0.1770*TSS**( 0.9827)                    stdm2130
      DENSTY(35,I)=CONO3 *PSS**0.3921*TSS**( 0.1942)                    stdm2140
C  --- FOR CO2                                                          stdm2150
      CONCO2= CONJOE   *WAIR(I)  *WCO2(I)                               stdm2160
      DENSTY(36,I)=CONCO2*PSS**0.6705*TSS**(-2.2560)                    stdm2170
      DENSTY(37,I)=CONCO2*PSS**0.7038*TSS**(-5.0768)                    stdm2180
      DENSTY(38,I)=CONCO2*PSS**0.7258*TSS**(-1.6740)                    stdm2190
      DENSTY(39,I)=CONCO2*PSS**0.6982*TSS**(-1.8107)                    stdm2200
      DENSTY(40,I)=CONCO2*PSS**0.8867*TSS**(-0.5327)                    stdm2210
      DENSTY(41,I)=CONCO2*PSS**0.7883*TSS**(-1.3244)                    stdm2220
      DENSTY(42,I)=CONCO2*PSS**0.6899*TSS**(-0.8152)                    stdm2230
      DENSTY(43,I)=CONCO2*PSS**0.6035*TSS**( 0.6026)                    stdm2240
C  --- FOR CO                                                           stdm2250
      CONCO = CONJOE   *WAIR(I)  *WCO (I)                               stdm2260
      DENSTY(44,I)=CONCO *PSS**0.7589*TSS**( 0.6911)                    stdm2270
      DENSTY(45,I)=CONCO *PSS**0.9267*TSS**( 0.1716)                    stdm2280
C  --- FOR CH4                                                          stdm2290
      CONCH4= CONJOE   *WAIR(I)  *WCH4(I)                               stdm2300
      DENSTY(46,I)=CONCH4*PSS**0.7139*TSS**(-0.4185)                    stdm2310
C  --- FOR N2O                                                          stdm2320
      CONN2O= CONJOE   *WAIR(I)  *WN2O(I)                               stdm2330
      DENSTY(47,I)=CONN2O*PSS**0.3783*TSS**( 0.9399)                    stdm2340
      DENSTY(48,I)=CONN2O*PSS**0.7203*TSS**(-0.1836)                    stdm2350
      DENSTY(49,I)=CONN2O*PSS**0.7764*TSS**( 1.1931)                    stdm2360
C  --- FOR O2                                                           stdm2370
      CONO2 = CONJOE   *WAIR(I)  *WO2 (I)                               stdm2380
      DENSTY(50,I)=CONO2 *PSS**1.1879*TSS**( 2.9738)                    stdm2390
      DENSTY(51,I)=CONO2 *PSS**0.9353*TSS**( 0.1936)                    stdm2400
C  --- FOR NH3                                                          stdm2410
      CONNH3= CONJOE   *WAIR(I)  *WNH3(I)                               stdm2420
      DENSTY(52,I)=CONNH3*PSS**0.8023*TSS**(-0.9111)                    stdm2430
      DENSTY(53,I)=CONNH3*PSS**0.6968*TSS**( 0.3377)                    stdm2440
C  --- FOR NO                                                           stdm2450
      CONNO = CONJOE   *WAIR(I)  *WNO (I)                               stdm2460
      DENSTY(54,I)=CONNO *PSS**0.5265*TSS**(-0.4702)                    stdm2470
C  --- FOR NO2                                                          stdm2480
      CONNO2= CONJOE   *WAIR(I)  *WNO2(I)                               stdm2490
      DENSTY(55,I)=CONNO2*PSS**0.3956*TSS**(-0.0545)                    stdm2500
C  --- FOR SO2                                                          stdm2510
      CONSO2= CONJOE   *WAIR(I)  *WSO2(I)                               stdm2520
      DENSTY(56,I)=CONSO2*PSS**0.2943*TSS**( 1.2316)                    stdm2530
      DENSTY(57,I)=CONSO2*PSS**0.2135*TSS**( 0.0733)                    stdm2540
C     
C     STUFF THE DNSTYX ARRAY WITH THE PROFILE INFO FOR THE
C     EXTRA SPECIES. THAT IS, THE ADDITIONAL SPECIES BEYOND
C     MODTRAN'S 12 REGULAR SPECIES.
C     DNSTYX ARRAY IS STUFFED THE SAME WAY IN LOWTRAN AS IN MODTRAN.
      DO 450 IX = 1, NSPECX
         DNSTYX(IX,I)=CONJOE*WAIR(I)*WMOLXT(IX,I)
 450  CONTINUE
      ENDIF                                                             stdm2550
C***********************************************************************stdm2560
C   HERZBERG CONTINUUM PRESSURE DEPENDENCE CALCULATION, SHARDANAND 1977 stdm2570
C      AND   YOSHINO ET AL 1988                                         stdm2580
C                                                                       stdm2590
C     OXYGEN                                                            stdm2600
C                                                                       stdm2610
C      ********       ERRATA JULY 25                                    stdm2620
C     DENSTY(58,I)=(1.+.73*F1)*CONO2                                    stdm2630
      DENSTY(58,I)=(1.+.83*F1)*CONO2                                    stdm2640
C       ********      END  ERRATA                                       stdm2650
      DENSTY(59,I) = 0.                                                 stdm2660
      DENSTY(60,I) = 0.                                                 stdm2670
c     these (densty(55,i) for n2o and densty(56,i) for so2 are
c     modtran densities. i e actual denisities.  in lowtran
c     some densities are scaled.  but we need the actual ones for uv-vis
c     stuff.  therefore density array's first dimension was extended by 2
c     density(64,i) will hold no2
c     density(65,i)will hold so2
      densty(64,i)=conjoe*wair(i)*wno2(i)
      densty(65,i)=conjoe*wair(i)*wso2(i)
C***********************************************************************stdm2680
C                                                                       stdm2690
C     RFNDX = REFRAXTIVITY 1-INDEX OF REFRACTION                        stdm2700
C     FROM EDLEN, 1966                                                  stdm2710
      PPW=RV*WTEMP*TT                                                   stdm2720
C     AVW=0.5*(V1+V2)                                                   stdm2730
      AVW=0.5*(IV1+IV2)                                                 stdm2740
      RFNDX(I)=((A0+A1/(1.-(AVW/B1)**2) +A2/(1.0-(AVW/B2)**2))*         stdm2750
     X (PP/PZERO)*(TZERO+15.0)/TT-(C0-(AVW/C1)**2)*PPW/PZERO)*1.E-6     stdm2760
25    CONTINUE                                                          stdm2770
      IF(NPR.EQ.1) GO TO 40                                             stdm2780
      WRITE(IPR,910)                                                    stdm2790
      DO 30 I=1,ML                                                      stdm2800
       WRITE(IPR,905) I,ZM(I),PM(I),TM(I),(DENSTY(K,I),K=4,6),RFNDX(I), stdm2810
     X DENSTY(8,I),DENSTY(58,I)                                         stdm2820
30    CONTINUE                                                          stdm2830
      WRITE (IPR,915)                                                   stdm2840
      DO 35 I=1,ML                                                      stdm2850
      WRITE(IPR,905) I,ZM(I),PM(I),TM(I),(DENSTY(K,I),K=10,11),         stdm2860
     X DENSTY(7,I),DENSTY(12,I),DENSTY(13,I),DENSTY(14,I),DENSTY(15,I), stdm2870
     X DENSTY(16,I),RELHUM(I)                                           stdm2880
35    CONTINUE                                                          stdm2890
      IF(MODTRN)THEN                                                    stdm2900
          WRITE(IPR,'(1H1,/22H  ATMOSPHERIC PROFILES,//11H   I      Z,  stdm2910
     1      51H       P       T      H2O      O3       CO2      CO,     stdm2920
     2      55H       CH4      N2O      O2       NH3      NO       NO2, stdm2930
     3      9H      SO2,/42H         (KM)    (MB)    (K)  (           , stdm2940
     4      55H                      ATM CM/KM                        , stdm2950
     5      30H                             ))')                        stdm2960
      ELSE                                                              stdm2970
      WRITE(IPR,920)                                                    stdm2980
      ENDIF                                                             stdm2990
      DO 39 I = 1,ML                                                    stdm3000
      WRITE(IPR,906)I,ZM(I),PM(I),TM(I),DENSTY(17,I),DENSTY(31,I),      stdm3010
     + DENSTY(36,I),DENSTY(44,I),DENSTY(46,I),DENSTY(47,I),             stdm3020
     + DENSTY(50,I),DENSTY(52,I),DENSTY(54,I),DENSTY(55,I),DENSTY(56,I) stdm3030
   39 CONTINUE                                                          stdm3040
      WRITE(IPR,'(1H1,/22H  ATMOSPHERIC PROFILES)')                     cfc
cjv 7/17/95 testing correction from Andy Young.
cjv      WRITE(IPR,'(/30H   I      Z       P       T   ,11(1x,a8))')       cfc
cjv     $    ,(cnamex(kx),kx=1,11)                                         cfc
      WRITE(IPR,'(/30H   I      Z       P       T   ,11(1x,a8))')       cfc
     $    (cnamex(kx),kx=1,11)                                          cfc
cjv ^^^^^^^^^^^^^^^^^^
      WRITE(IPR,'(30X,11(1x,a8))')(cnamex(kx),kx=12,min(22,nspecx))     cfc
c     it takes too much space to write out more than 12-22 species
      write(ipr,'(42H         (KM)    (MB)    (K)  (           ,        cfc
     1      55H                      ATM CM/KM                        , cfc
     2      30H                             ))')                        cfc
      DO 37 I = 1,ML                                                    cfc
         WRITE(IPR,906)I,ZM(I),PM(I),TM(I),                             cfc
     $        (DNSTYX(ix,i),ix=1,11)                                    cfc
         WRITE(IPR,907)(DNSTYX(ix,i),ix=12,min(22,nspecx))              cfc
c     it takes too much space to write out more than 22 species
   37 CONTINUE                                                          cfc
40    CONTINUE                                                          stdm3050
      RETURN                                                            stdm3060
  905 FORMAT (I4,0PF9.2,F9.3,F7.1,1X,1P9E10.3)                          stdm3070
  906 FORMAT (I4,0PF9.2,F9.3,F7.1,1X,1P11E9.2)                          stdm3080
  907 FORMAT (30x,1P11E9.2)   
  910 FORMAT('1',/,'  ATMOSPHERIC PROFILES',//,                         stdm3090
     1 3X,'I',T10,'Z',T18,'P',T26,'T',                                  stdm3100
     2 T35,'N2',T44,'CNTMSLF',T52,'MOL SCAT',T65,'N-1', T73,            stdm3110
     3 'O3 (UV)',T82,'O2 (UV)',/,                                       stdm3120
     4 T9,'(KM)',T17,'(MB)',T25,'(K)',                                  stdm3130
     5 T40,'(MOL/CM2 KM)',T55,'(-)',T65,'(-)',T 70,                     stdm3140
     6 '(ATM CM/KM)',T82,'(ATM CM/KM)',/)                               stdm3150
  915 FORMAT('1',/,'  ATMOSPHERIC PROFILES',//,                         stdm3160
     1 3X,'I',T10,'Z',T18,'P',T26,'T',T33,'CNTMFRN',T45,'HNO3',         stdm3170
     2 T53,'AEROSOL 1',T63,'AEROSOL 2', T73,'AEROSOL 3',T83,            stdm3180
     3 'AEROSOL 4',T93,'AER1*RH',T103,'CIRRUS',T118,'RH'/,              stdm3190
     4 T9,'(KM)',T17,'(MB)',T25,'(K)',T31,'MOL/CM2 KM',T42,             stdm3200
     5 'ATM CM/KM',T54,'(-)',T64,'(-)',T74,'(-)',T84,'(-)',T94,         stdm3210
     6 '(-)',T104,'(-)',T113,'(PERCNT)',/)                              stdm3220
  920 FORMAT('1',/,'  ATMOSPHERIC PROFILES',//,                         stdm3230
     + '  (IF A MOLECULE HAS MORE THAN ONE BAND, THEN THE DATA FOR THE',stdm3240
     + ' FIRST BAND ARE SHOWN.)'//                                      stdm3250
     1 '   I      Z       P       T      H2O      O3       CO2',        stdm3260
     2 '      CO       CH4      N2O      O2       NH3      NO ',        stdm3270
     3 '      NO2      SO2'/                                            stdm3280
     4 '         (KM)    (MB)    (K) G/CM**2/KM  (            ',        stdm3290
     5 '          ATM CM/KM                                   ',        stdm3300
     6 '                  )')                                           stdm3310
      END                                                               stdm3320
      subroutine frq5dt(loop0,iv)                                       frq5 100
c                                                                       frq5 110
c     This routine defines the layer independent 5 cm-1 data            frq5 120
      logical loop0,modtrn                                              frq5 130
      COMMON/CNSTNS/PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                       frq5 140
      COMMON/CARD1/MODEL,ITYPE,IEMSCT,M1,M2,M3,IM,NOPRNT,TBOUND,SALB,   frq5 150
     1  MODTRN                                                          frq5 160
      COMMON/AABBCC/A1(11),B1(11),C1(11),IBND(11),A(11),CPS(11)         frq5 170
      COMMON/WNLOHI/IWLH2O(15),IWLO3(6),IWLCO2(11),IWLCO(4),IWLCH4(5),  frq5 180
     1  IWLN2O(12),IWLO2(7),IWLNH3(3),IWLNO(2),IWLNO2(4),IWLSO2(5),     frq5 190
     2              IWHH2O(15),IWHO3(6),IWHCO2(11),IWHCO(4),IWHCH4(5),  frq5 200
     3  IWHN2O(12),IWHO2(7),IWHNH3(3),IWHNO(2),IWHNO2(4),IWHSO2(5)      frq5 210
      COMMON/H2O/CPH2O(3515)                                            frq5 220
      COMMON/O3/CPO3(447)                                               frq5 230
      COMMON/UFMIX1/CPCO2(1219)                                         frq5 240
      COMMON/UFMIX2/CPCO(173),CPCH4(493),CPN2O(704),CPO2(382)           frq5 250
      COMMON/TRACEG/CPNH3(431),CPNO(62),CPNO2(142),CPSO2(226)           frq5 260
      common/frq5/sigo20,sigo2a,sigo2b,c0,ct1,ct2,abb(19),crsno2,crsso2
c     Initialize data                                                   frq5 290
c       alpha2   parameter used to defined water continuum              frq5 300
      if(loop0)then                                                     frq5 320
c                                                                       frq5 330
c         Initialize frequency region index for each molecule           frq5 340
          INDH2O=1                                                      frq5 350
          INDO3 =1                                                      frq5 360
          INDCO2=1                                                      frq5 370
          INDCO =1                                                      frq5 380
          INDCH4=1                                                      frq5 390
          INDN2O=1                                                      frq5 400
          INDO2 =1                                                      frq5 410
          INDNH3=1                                                      frq5 420
          INDNO =1                                                      frq5 430
          INDNO2=1                                                      frq5 440
          INDSO2=1                                                      frq5 450
          return                                                        frq5 460
      endif                                                             frq5 470
      V=FLOAT(IV)                                                       frq5 480
      IF(.not.modtrn)THEN                                               frq5 490
          CPS( 1)=CXDTA(V,IWLH2O,IWHH2O,CPH2O,INDH2O)                   frq5 500
          CPS( 2)=CXDTA(V,IWLCO2,IWHCO2,CPCO2,INDCO2)                   frq5 510
          CPS( 3)=CXDTA(V,IWLO3 ,IWHO3 ,CPO3 ,INDO3 )                   frq5 520
          CPS( 4)=CXDTA(V,IWLN2O,IWHN2O,CPN2O,INDN2O)                   frq5 530
          CPS( 5)=CXDTA(V,IWLCO, IWHCO, CPCO, INDCO )                   frq5 540
          CPS( 6)=CXDTA(V,IWLCH4,IWHCH4,CPCH4,INDCH4)                   frq5 550
          CPS( 7)=CXDTA(V,IWLO2, IWHO2, CPO2, INDO2 )                   frq5 560
          CPS( 8)=CXDTA(V,IWLNO, IWHNO, CPNO, INDNO )                   frq5 570
          CPS( 9)=CXDTA(V,IWLSO2,IWHSO2,CPSO2,INDSO2)                   frq5 580
          CPS(10)=CXDTA(V,IWLNO2,IWHNO2,CPNO2,INDNO2)                   frq5 590
          CPS(11)=CXDTA(V,IWLNH3,IWHNH3,CPNH3,INDNH3)                   frq5 600
      ENDIF                                                             frq5 610
C                                                                       frq5 620
C     N2 CONTINUUM                                                      frq5 630
      CALL C4DTA(ABB(4),V)                                              frq5 640
      CALL ABCDTA(IV)                                                   frq5 650
C                                                                         F00290
      CALL SLF296(V,SH2OT0)                                             frq5 680
      CALL SLF260(V,SH2OT1)                                             frq5 690
      CALL FRN296(V,FH2O)                                               frq5 700
C
C                                                                         F00720
C     RADIATION FIELD                                                     F00730
C                                                                         F00740

      VTEMP=1.438786*V                                                  frq5 760
      IF(VTEMP/260..GT.BIGEXP)THEN                                      frq5 770
          RADFN0=V                                                      frq5 780
          RADFN1=V                                                      frq5 790
      ELSE                                                              frq5 800
          store=EXP(-VTEMP/296.)                                        frq5 810
          RADFN0=V*(1.-store)/(1.+store)                                frq5 820
          store=EXP(-VTEMP/260.)                                        frq5 830
          RADFN1=V*(1.-store)/(1.+store)                                frq5 840
      ENDIF                                                             frq5 850
      ABB(5)=SH2OT0*RADFN0                                              frq5 860
c
c   co2 continum
c
cc    CALL cco2  (V,Fco2)
C                                                                       frq5 900
cc    fco2 = fco2 * radfn0
      CALL C6DTA(ABB(6),V)                                              frq5 870
cc    ABB(9)=SH2OT1*RADFN1-SH2OT0*RADFN0                                frq5 880
      ABB(9)=SH2OT1*RADFN1
      ABB(10)=FH2O*RADFN0                                               frq5 890
C     HNO3 ABSORPTION CALCULATION                                       frq5 910
      abb(11)=0.                                                        frq5 920
      IF(.not.modtrn)CALL HNO3(V,ABB(11))                               frq5 930
      CALL AEREXT(V)                                                    frq5 940
c                                                                       frq5 950
c     O2 Continuum Contributions                                        frq5 960
          CALL HERTDA(ABB(17),V)                                        frq5 970
      CALL O2CONT(V,SIGO20,SIGO2A,SIGO2B)                               frq5 980
      IF(V.GT.49600.)CALL SCHRUN(V,CPS(7))                              frq5 990
C                                                                       frq51000
C     DIFFUSE OZONE                                                     frq51010
cssi  CALL C8DTA(ABB(8),V)                                              frq51020
      c0=0.                                                             frq51030
      IF(V.GE.40800.)THEN                                               frq51040
          CALL O3UV(V,C0)                                               frq51050
          ct1=0.                                                        frq51060
          ct2=0.                                                        frq51070
          ABB(8)=0.                                                     frq51080
ccc   ELSEIF(V.GT.24370.)THEN                                           frq51090
      ELSEIF(V.GT.24565.)THEN                                           frq51090
          CALL O3HHT0(V,C0)                                             frq51100
          CALL O3HHT1(V,CT1)                                            frq51110
          CALL O3HHT2(V,CT2)                                            frq51120
          ABB(8)=0.                                                     frq51130
      else
          call o3chap(v,c0,ct1,ct2)
          abb(8)=0.
      ENDIF                                                             frq51140
c
c     no2 cross-sections (14000 cm-1 to 50000 cm-1)
      call no2xs(v,crsno2)
c
c     so2 cross-sections (14000 cm-1 to 50000 cm-1)
      call so2xs(v,crsso2)
      return                                                            frq51150
      end                                                               frq51160
      subroutine  ckd(sh2ot0,sh2ot1,tave,sh2o,fh2o,v)
c                                                                       frq5 280
C                                                                       frq5 660
C     revision:  3.3
C     created:   28 Apr 1994  10:14:28
C     presently: 28 Apr 1994  10:15:53
c     clough ckd_2.1
c     hvrcnt = '3.3'
c
C                                                                         F00260
      DATA P0 / 1013. /,T0 / 296. /                                       F00270
      DATA XLOSMT / 2.68675E+19 /                                         F00280
c     
c     These are self-continuum modification factors from 700-1200 cm-1
c
      DIMENSION XFAC(0:50)
c
      DATA (XFAC(I),I=0,50)/
     1    1.00000,1.01792,1.03767,1.05749,1.07730,1.09708,
     2    1.10489,1.11268,1.12047,1.12822,1.13597,1.14367,
     3    1.15135,1.15904,1.16669,1.17431,1.18786,1.20134,
     4    1.21479,1.22821,1.24158,1.26580,1.28991,1.28295,
     5    1.27600,1.26896,1.25550,1.24213,1.22879,1.21560,
     6    1.20230,1.18162,1.16112,1.14063,1.12016,1.10195,
     7    1.09207,1.08622,1.08105,1.07765,1.07398,1.06620,
     8    1.05791,1.04905,1.03976,1.02981,1.00985,1.00000,
     9    1.00000,1.00000,1.00000/
C                                                                         F00290
c
      data s260/-12345678./, s296/-12345678./
      save s260, s296, vfac, sfac, wdth 
C--------------------------------------------------------------------
C                             SELF

      ALPHA2 = 200.**2                                                    F00550
C
      ALPHS2= 120.**2
      BETAS = 5.E-06
      V0S=1310.
      FACTRS= 0.15
C
C--------------------------------------------------------------------
C
C                             FOREIGN
      HWSQF= 330.**2
      BETAF = 8.  E-11
      V0F =1130.
      FACTRF = 0.97
C
      V0F2 =1900.
      HWSQF2 = 150.**2
      BETA2 = 3.E-06
C
C--------------------------------------------------------------------
         vj = v
         VS2 = (VJ-V0S)**2
         SH2O = 0.                                                      F00580
         IF(SH2Ot0.GT.0.)THEN                                           frq5 710
         TFAC = (TAVE-T0)/(260.-T0)                                     F00540
            SH2O = SH2OT0*(SH2OT1/SH2OT0)**TFAC  
         sfac = 1.
         if (vj.ge.700. .and.  vj.le.1200.) then 
            jfac = (vj-700.)/10. + 0.00001
            sfac = xfac(jfac)
         endif
C                                                                         F00610
C     CORRECTION TO SELF CONTINUUM (1 SEPT 85); FACTOR OF 0.78 AT 1000    F00620
C                             AND  .......
C                                                                         F00630
      SH2O = sfac * SH2O*(1.-0.2333*(ALPHA2/((VJ-1050.)**2+ALPHA2))) *     F00640
     C                 (1.-FACTRS*(ALPHS2/(VS2+(BETAS*VS2**2)+ALPHS2))) 
         ENDIF                                                            F00650
c-------------------------------------------
c
C                                                                         F00660
C     CORRECTION TO FOREIGN CONTINUUM                                     F00670
C                                                                         F00680
        VF2 = (VJ-V0F)**2
        VF6 = VF2 * VF2 * VF2
        FSCAL  = (1.-FACTRF*(HWSQF/(VF2+(BETAF*VF6)+HWSQF)))
        VF2 = (VJ-V0F2)**2
        VF4 = VF2*VF2
        FSCAL = FSCAL* (1.- 0.6*(HWSQF2/(VF2 + BETA2*VF4 + HWSQF2)))
c
        FH2O=FH2O*fscal    
        return
        end
      subroutine lay5dt(v,msoff,ik,ikoff,ipath)                         
C     MODIFIED ON sept  1994.
c                                                                       
c     This routine defines the layer dependent 5 cm-1 data              
c                                                                       
c     The txnew and txold arrays contain:                               
c        1   Asymmetry Parameter Weighted by Scattering Depth           
c        2   Incremental Aerosol Scattering Optical Depth               
c        3   Total O2 Continuum Transmittance                           
c        4   N2 Continuum Transmittance                                 
c        5   Total H2O Continuum Transmittance                          
c        6   Rayleigh Molecular Scattered Transmittance                 
c        7   Aerosol Extinction                                         
c        8   Total Ozone Continuum Transmittance                        
c        9   Product of All Continuum Transmittances Except O2 and HNO3 
c       10   Aerosol Absorption                                         
c       11   HNO3 Transmittance                                         
c       12   Molecular Continuum Optical Depth                          
c       13   Incremental Aerosol Extinction Optical Depth               
c       14   Total Continuum Optical Depth                              
c       15   Rayleigh Molecular Scattering Optical Depth                
c       16   Cirrus Cloud Transmittance (icld = 20 only)                
c                                                                       
c     For LOWTRAN runs, tx contains molecular line center transmittances
c       17=H2O  36=CO2  31=O3   47=N2O  44=CO   46=CH4                  
c       50=O2   54=NO   56=SO2  55=O2   52=NH3                          
c
c
      INCLUDE 'parameter.list'
      COMMON RELHUM(LAYDIM),HSTOR(LAYDIM),ICH(4),VH(17),TX(65),W(65)  
      COMMON IMSMX,WPATH(LAYTHR,65),TBBY(LAYTHR),PATM(LAYTHR),NSPEC,   
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX2(47),AWCCON(5)  
      COMMON/CNSTNS/PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                       
      logical modtrn                                                    
      COMMON/CARD1/MODEL,ITYPE,IEMSCT,M1,M2,M3,IM,NOPRNT,TBOUND,SALB,   
     1  MODTRN                                                          
      COMMON/CARD2/IHAZE,ISEASN,IVULCN,ICSTL,ICLD,IVSA,VIS,WSS,WHH,     
     1  RAINRT                                                          
      COMMON/AER/X1,X2,X3,X4,X5,Y1,Y2,Y3,Y4,Y5,Z1,Z2,Z3,Z4,Z5           
      COMMON/AABBCC/A1(11),B1(11),C1(11),IBND(11),A(11),CPS(11)         
      common/frq5/sigo20,sigo2a,sigo2b,c0,ct1,ct2,abb(19),crsno2,crsso2
      common/lay5/txnew(18,LAYTHR,3),txold(18,LAYTHR,3)                   
      save sctold,extold                                             
c                                                                     
C     CONTRIBUTION OF RAIN to extinction, scattering and absorption     
      EXT=0.                                                            
      ABT=0.                                                            
      sct=0.                                                            
      IF(W(3).NE.0.)call rain(v,ext,abt,sct,asymr)                      
c                                                                       
c     calculate incremental optical depth variables                     
c     IF(MSOFF.GT.0 .and. ipath.eq.3)THEN                               
      IF(ipath.eq.3)THEN                               
          IF(IK.EQ.1)THEN                                               
              sctold=0.                                                 
              extold=0.                                                 
              dw7=W(7)                                                  
              dw12=W(12)                                                
              dw13=W(13)                                                
              dw14=W(14)                                                
              dw16=W(16)                                                
          ELSE                                                          
              IKOFFM=IKOFF-1                                            
              dw7=w(7)-WPATH(IKOFFM,7)                                  
              dw12=W(12)-WPATH(IKOFFM,12)                               
              dw13=W(13)-WPATH(IKOFFM,13)                               
              dw14=W(14)-WPATH(IKOFFM,14)                               
              dw16=W(16)-WPATH(IKOFFM,16)                               
          ENDIF                                                         
          d1=(x1-y1)*dW7                                                
          d2=(x2-y2)*dW12                                               
          d3=(x3-y3)*dW13                                               
          d4=(x4-y4)*dW14                                               
          d5=(x5-y5)*dW16                                               
C                                                                       
C         ASYMMETRY FACTOR IS WEIGHTED AVERAGE                          
          dsct=sct-sctold                                               
          TXOLD(1,IKOFF,IPATH)=TXNEW(1,IKOFF,IPATH)                     
          TXOLD(2,IKOFF,IPATH)=TXNEW(2,IKOFF,IPATH)                     
          TXOLD(13,IKOFF,IPATH)=TXNEW(13,IKOFF,IPATH)                   
          TXNEW(1,IKOFF,IPATH)=Z1*d1+Z2*d2+Z3*d3+Z4*d4+Z5*d5+dsct*ASYMr 
          TXNEW(2,IKOFF,IPATH)=d1+d2+d3+d4+d5+dsct                      
          TXNEW(13,IKOFF,IPATH)=x1*dw7+x2*dw12+x3*dw13+x4*dw14+x5*dw16  
     1      +(ext-extold)                                               
          sctold=sct                                                    
          extold=ext                                                    
      ENDIF                                                             
c                                                                       
c     Add aerosol extinction and absorption to rain contribution        
      EXT=ext+X1*W(7)+X2*W(12)+X3*W(13)+X4*W(14)+X5*W(16)               
      ABT=abt+Y1*W(7)+Y2*W(12)+Y3*W(13)+Y4*W(14)+Y5*W(16)               
c                                                                       
c     Determine Cumulative Optical Depths                               
c       tx(4)   N2 Continuum                                            
c       tx(5)   Sum of H2O Continuum Contributions                      
c       tx(6)   Rayleigh Molecular Scattering                           
c       tx(8)   Visible and Ultraviolet O3 (= 0 if abbuv > 0)           
c       tx(11)  HNO3 Absortion (LOWTRAN calculations only)              
c       tx(16)  Cirrus Clouds                                           
c       tx(9)   Sum of Continuum contributions excluding O2 and HNO3    
c       tx(3)   O2 Continuum Contributions                              
      TX(4)=ABB(4)*W(4)                                                 
cc    TX(5)=1.E-20*(ABB(5)*W(5)+ABB(9)*W(9)+ABB(10)*W(10))              
cjv Change from Lex on 7/24/95. 'TAVE will have a divide by zero is there is no water.'
      TX(5)=0.
      IF(W(5).GT.0.)THEN
          SH2OT0=ABB(5)
          SH2OT1=ABB(9)
          FH2O=ABB(10)
          TAVE=296.-(W(9)/W(5))*(296.-260)
          CALL CKD(SH2OT0,SH2OT1,TAVE,SH2O,FH2O,V)
          TX(5)=1.E-20*(SH2O*W(5)+FH2O*W(10))
      ENDIF
c
cjv      sh2ot0 = abb(5)
cjv      sh2ot1 = abb(9)
cjv      fh2o = abb(10)
cjv      if(w(5).ne.0.)then
cjv        tave = 296. -( w(9)/w(5))*(296.-260)
cjv      else
cjv         tave = 260.
cjv      endif 
cjv      call ckd(sh2ot0,sh2ot1,tave,sh2o,fh2o,v)
cjv     tx(5)= 1.e-20*(sh2o*w(5)+fh2o*w(10))
c
cjv ^^^^^^^^^^^^^^^
c
      TX(6)=ABB(6)*W(6)                                                 
      TX(8)=ABB(8)*W(8)+C0*(.269*w(8)+CT1*W(59)+CT2*W(60))              
      TX(11)=ABB(11)*W(11)                                              
      tx(16)=0.                                                         
      if(icld.eq.20)TX(16)=2*W(16)                                      
      TX(3)=W(58)*ABB(17)+                                              
     1  SIGO20*(W(63)+SIGO2A*(W(1)-220.*W(63))+SIGO2B*W(2))             
      tx(64) = crsno2 * w(64)
      tx(65) = crsso2 * w(65)
cc    tco2 = fco2 * w(37)
      tx(9)=TX(4)+TX(5)+TX(6)+TX(8)+EXT+tx(16)+tx(64)+tx(65)                          
C                                                                       
C     STORE CUMULATIVE AEROSOL PARAMETERS FOR DIFFERENT VERTICAL REGIONS
      TX(10)=ABT                                                        
      TX(7)=EXT                                                         
C                                                                       
C     STORE CUMULATIVE OPTICAL THICKNESS PARAMETERS                     
      TXOLD(12,IKOFF,IPATH)=TXNEW(12,IKOFF,IPATH)                       
      TXNEW(12,IKOFF,IPATH)=TX(4)+TX(5)+TX(8)+TX(3)                     
      txold(14,ikoff,ipath)=txnew(14,ikoff,ipath)                       
      txnew(14,ikoff,ipath)=tx(9)+tx(11)+tx(3)                          
      TXOLD(15,IKOFF,IPATH)=TXNEW(15,IKOFF,IPATH)                       
      TXNEW(15,IKOFF,IPATH)=TX(6)                                       
      DO 10 K=3,11                                                      
          TXOLD(K,IKOFF,IPATH)=TXNEW(K,IKOFF,IPATH)                     
          IF(TX(K).LE.BIGEXP)THEN                                       
              TXNEW(K,IKOFF,IPATH)=EXP(-TX(K))                          
          ELSE                                                          
              TXNEW(K,IKOFF,IPATH)=1./BIGNUM                            
          ENDIF                                                         
   10 CONTINUE                                                          
      TXOLD(16,IKOFF,IPATH)=TXNEW(16,IKOFF,IPATH)                       
      IF(TX(16).LE.BIGEXP)THEN                                          
          TXNEW(16,IKOFF,IPATH)=EXP(-TX(16))                            
      ELSE                                                              
          TXNEW(16,IKOFF,IPATH)=1./BIGNUM                               
      ENDIF                                                             
      txold(17,ikoff,ipath)=txnew(17,ikoff,ipath)
      IF(TX(64).LE.BIGEXP)THEN                                          
          TXNEW(17,IKOFF,IPATH)=EXP(-TX(64))                            
      ELSE                                                              
          TXNEW(17,IKOFF,IPATH)=1./BIGNUM                               
      ENDIF                                                             
c
      txold(18,ikoff,ipath)=txnew(18,ikoff,ipath)
      IF(TX(65).LE.BIGEXP)THEN                                          
          TXNEW(18,IKOFF,IPATH)=EXP(-TX(65))                            
      ELSE                                                              
          TXNEW(18,IKOFF,IPATH)=1./BIGNUM                               
      ENDIF                                                             
c
      IF(.not.modtrn)THEN                                               
C                                                                       
C         MOLECULAR ABSORBERS                                           
          DO 20 IMOL=1,11                                               
              IF(CPS(IMOL).EQ.-20.)THEN                                 
                  TX(KPOINT(IMOL))=1.                                   
              ELSE                                                      
                  TX(KPOINT(IMOL))=                                     
     1              DBLTX(W(IBND(IMOL)),CPS(IMOL),A(IMOL),txlog)        
                  txnew(14,ikoff,ipath)=txnew(14,ikoff,ipath)+txlog
              ENDIF                                                     
   20     CONTINUE                                                      
      ENDIF                                                             
      return                                                            
      end                                                               
      subroutine loop(loop0,iv,ivx,ikmx,mxfreq,summs,transm,iph,        
     1  sumssr,ivtest,unif,trace,transx,sumv,isourc,iday,anglem,          
     2  frac,uang,dis,nstr )
c                                                                       
c     This routine performs the loop over layers for each frequency     
      INCLUDE 'parameter.list'
      logical ivtest,loop0,transm,modtrn                
cj 6/17
cj      real BET(68)                                                      
      real BET(laytwo)                                                      
cj ^
cjd3 v add this common
      common /saveopt/imap(laytwo)
cjd3 ^
c
c
c
C     CONVENTION
C     MMOLX = MAXIMUM NUMBER OF NEW SPECIES (IDENTIFIED BY "X")
C     MMOL  = MAXIMUM NUMBER OF OLD SPECIES (W/O SUFFIX "X")
C     THESE DEFINE THE MAXIMUM ARRAY SIZES.
C
C     THE ACTUAL NUMBER OF PARAMETERS ARE:
C     NSPEC = ACTUAL NUMBER OF OLD SPECIES (12), CAN'T EXCEED MMOL
C     NSPECX = ACTUAL NUMBER OF "X" SPECIES,     CAN'T EXCEED MMOLX
C
c     Modtran has 65 as a magic number.  It includes the usual 12 species
c     plus a host of other species and sub species.  Many arrays have 
c     dimension 65.

      LOGICAL XTRAN
C
C     TRANS VARIABLES
C
      COMMON /RFRPTX/ DENPX(MMOLX,laydim+1),AMTPX(MMOLX,laydim+1)
      COMMON /NONAME/ TXX(MMOLX), WX(MMOLX), WPATHX(laythr,MMOLX)
      COMMON /SOLSX/  WPTHSX(laythr,MMOLX),TBBYSX(laythr,MMOLX),
     $     PATMSX(laythr,MMOLX)
      DATA XTRAN/.TRUE./
c
c
c
      COMMON RELHUM(LAYDIM),HSTOR(LAYDIM),ICH(4),VH(17),TX(65),W(65)  
      COMMON IMSMX,WPATH(LAYTHR,65),TBBY(LAYTHR),PATM(LAYTHR),NSPEC,   
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX2(47),AWCCON(5)  
      COMMON/CNSTNS/PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                       
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON/CARD1/MODEL,ITYPE,IEMSCT,M1,M2,M3,IM,NOPRNT,TBOUND,SALB,   
     1  MODTRN                                                          
      COMMON/CNTRL/KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT                 
      COMMON /PATH/PL(LAYTWO),QTHETA(LAYTWO),ITEST,HI,HF,
     1  AHT(LAYTWO),tph(LAYTWO)
      COMMON/SOLS/AH1(LAYTWO),ARH(LAYTWO),WPATHS(LAYTHR,65),
     1 PA(LAYTWO),PR(LAYTWO),ATHETA(LAYDIM+1),ADBETA(LAYDIM+1),
     2 LJ(LAYTWO+1),JTURN,ANGSUN,CSZEN(LAYTWO),TBBYS(LAYTHR,12),
     3 PATMS(LAYTHR,12)
      COMMON/SRAD/TEB1,TEB2SV                                           
      COMMON/DIRST/SUN,UMB(laydim),DMB(laydim),UMBS(laydim),
     1  DMBS(laydim)                
      COMMON/MSRD/TLE(LAYDIM),COSBAR(LAYDIM),OMEGA0(LAYTWO),
     1  UPF(10,LAYDIM),DNF(10,LAYDIM),TAER(LAYDIM),ASYIK(LAYTWO),
     2  ASYDM(LAYTWO),STRN(0:LAYDIM),DMOLS(LAYTWO),DSTRN(0:LAYTWO),
     3  FDNSRT,FDNTRT,TAUT(LAYDIM),UMF(LAYDIM),DMF(LAYDIM),
     4  UMFS(LAYDIM),DMFS(LAYDIM) 
      COMMON/TRAN/TMOLS(laydim),TAERS(laydim),TCONT(laydim),
     1 DCONT(laytwo)               
      common/frq5/sigo20,sigo2a,sigo2b,c0,ct1,ct2,abb(19),crsno2,crsso2
      common/lay5/txnew(18,LAYTHR,3),txold(18,LAYTHR,3)                   
c                                                                       
      common /solar/lsame                                               
      logical lsame                                                     
c                                                                       
cjd3 v
c******************************************
c        variables inserted by NORTH
cj        parameter(maxumu=1 ,maxulv=34)
        parameter(maxumu=1 ,maxulv=laydim)
        double precision t0cms(maxumu,maxulv),s0cms(maxumu,maxulv),
     1  sms,smss,uang
        integer nstr
        logical dis
c*******************************************

cjd3 ^
c                                                                       
c     loop0 is .true. for first call from routine trans                 
      sumj = 0
      total=1.
cc    fco2 =0.
      if(loop0)then                                                     
c                                                                       
c         Initialize transmission arrays                                
          DO 10 I=1,18                                                  
cj 6/17
cj              DO 10 J=1,102                                             
              DO 10 J=1,laythr                                             
cj ^
                  DO 10 K=1,3
                      txold(i,j,k)=0.                                            
                      TXNEW(I,J,K)=0.                                   
   10     continue                                               
          do 12 kx = 1, nspecx                                          
          txx(kx) = 1.                                    
 12       continue                                                      
c                                                                       
c         Initialization for multiple scattering calculations           
          IF(IMULT.EQ.1)CALL MAPMS(ML,IKMAX,IMAP)                       
c                                                                       
c         Initialization for LOWTRAN calculations                       
          if(.not.modtrn)call frq5dt(loop0,iv)                          
          return                                                        
      endif                                                             
c                                                                       
c     Define the layer independent 5 cm-1 data                          
      IF(IVTEST)call frq5dt(loop0,iv)                                   
      V=IVX                                                             
      IPATH=1                                                           
C                                                                       
C     MSOFF IS EQUAL laydim*2 FOR THE MULTIPLE SCATTERING LAYER LOOP.         
      IF(.NOT.LSAME)THEN                                                
         MSOFF=IMULT*laydim*2                                                 
      ELSE                                                              
C                                                                       
C        IF (LSAME) SOLAR PARAMETERS ARE UNCHANGED FROM THE             
C        PREVIOUS RUN.  SO READ THE "PURE" SOLAR STUFF                  
C        FROM THE SCRATCH FILE CONTAINING THE RESULTS                   
C        FROM THE PREVIOUS RUN                                          
C        AND PERFORM THE FOLLOWING DO-LOOP WHICH                        
C        USES A PATH-DEPENDENT QUANTITY QTHETA.                         
C        SET MSOFF TO ZERO TO SKIP SOLAR MULTIPLE                       
C        SCATTERING CALCULATION.                                        
         MSOFF=0                                                        
c                                                                       
         read(iscrch)(UMF(ik),DMF(ik),UMFS(ik),dmfs(ik),cosbar(ik),         
     $     ik=1,laydim),(OMEGA0(ik2),ik2=1,laytwo),fdnsrt,fdntrt                  
         DO 111 IK=1,IKMAX                                              
            N=ML-IMAP(IK)                                               
C                                                                       
C           BET(N): BACKSCATTER FRACTION ALONG OPTICAL PATH AT LAYER N  
            store=ABS(QTHETA(IK))                                       
C                                                                       
C           CALCULATE BACKSCATTER PARAMETERS                            
            IF(COSBAR(N).EQ.0.)THEN                                     
               BET(IK)=.5                                               
            ELSE                                                        
               BET(IK)=BETABS(store,COSBAR(N))                          
            ENDIF                                                       
 111     CONTINUE                                                       
         BET(IKMAX1)=.5                                                 
      ENDIF                                                             
C                                                                       
C     FOR EACH WAVENUMBER, AN INITIAL CALL (IK=0) TO bMOD IS REQUIRED   
      IK=0                                                              
   20 IF(modtrn)CALL bMOD(IK,ikmx,IPATH,IVX,MSOFF,MXFREQ)               
c                                                                       
c     Initialize parameters                                             
      IPATH=1                                                           
      TEB1=-99.                                                         
      TEB2SV=-99.                                                       
      tOLD=1.                                                           
      depold=0.                                                         
      DSTRN(0)=1.                                                       
      SUMMS=0.                                                          
C                                                                       
C     MSOFF IS A LAYER OFFSET PARAMETER EQUAL TO 0 FOR THE LINE OF      
C     SIGHT PATH AND EQUAL TO 68 FOR THE MULTIPLE SCATTERING            
C     VERTICAL PATH FROM GROUND TO SPACE.                               
C                                                                       
C     BEGINNING OF LAYER LOOP                                           
      DO 100 IK=1,IKMAX                                                 
          IKOFF=IK+MSOFF                                                
c                                                                       
c         For transmission runs, w(k) was defined in routine geo.       
          IF(transm)GOTO80                                              
C                                                                       
C         LOAD APPROPRIATE ABSORBER AMOUNTS INTO W(K)                   
          IF(IEMSCT.eq.1)GOTO60                                         
          if(ipath.eq.1)then                                            
C                                                                       
C             LOAD W(K) WITH THE FIRST SOLAR PATH ABSORBER AMOUNT       
              index=MSOFF+1                                             
              DO 30 K=1,KMAX+2                                            
   30         W(K)=WPATHS(index,K)                                      
c                                                                       
c             In SSGEO, the W's were set to -5 if the sun was blocked   
              IF(W(36).GE.0.)GOTO80                                     
              index=0                                                   
              call shade(ik,msoff,index,ml,iph,ipath,v,sumssr)          
          endif                                                         
   40     IPATH=2                                                       
          index=MSOFF+IK+1                                              
C                                                                       
C         LOAD W(K) WITH L PATH ABSORBER AMOUNT FOR MSOFF=0 AND WITH    
C         SOLAR PATH ABSORBER AMOUNT FOR MSOFF>0                        
C                                                                       
          DO 50 K=1,KMAX+2                                                
              W(K)=WPATHS(index,K)                                      
              IF(MSOFF.EQ.0)W(K)=W(K)+WPATH(IK,K)                       
   50     CONTINUE                                                      
c
          DO 55 KX=1,NSPECX                                             
              WX(KX)=WPTHSX(INDEX,KX)                                   
              IF(MSOFF.EQ.0)WX(KX)=WX(KX)+WPATHX(IK,KX)                 
   55     CONTINUE                                                      
c                                                                       
c         In SSGEO, the W's were set to -5 if the sun was blocked       
          IF(WPATHS(index,36).GT.0.)GOTO80                              
          index=ik                                                      
          call shade(ik,msoff,index,ml,iph,ipath,v,sumssr)              
C                                                                       
C         LOAD W(K) WITH OPTICAL PATH ABSORBER AMOUNT                   
C                                                                       
   60     IPATH=3                                                       
          DO 70 K=1,KMAX+2                                                
   70     W(K)=WPATH(IKOFF,K)                                           
c
          DO 75 KX=1,NSPECX                                             
             WX(KX)=WPATHX(IKOFF,KX)                                    
 75       CONTINUE                                                      
c                                                                       
c         Define the layer dependent 5 cm-1 data                        
   80     IF(IVTEST)call lay5dt(v,msoff,ik,ikoff,ipath)                 
c                                                                       
c         Define tx array                                               
c           1   Asymmetry Parameter Weighted by Scattering Depth        
c           2   Incremental Aerosol Scattering Optical Depth            
c           3   Total O2 Continuum Transmittance                        
c           4   N2 Continuum Transmittance                              
c           5   Total H2O Continuum Transmittance                       
c           6   Rayleigh Molecular Scattered Transmittance              
c           7   Aerosol Extinction                                      
c           8   Total Ozone Continuum Transmittance                     
c           9   Product of All Continuum Transmittances Except O2 & HNO3
c          10   Aerosol Absorption                                      
c          11   HNO3 Transmittance                                      
c          12   Molecular Continuum Optical Depth                       
c          13   Incremental Aerosol Extinction Optical Depth            
c          14   Total Continuum Optical Depth                           
c          15   Rayleigh Molecular Scattering Optical Depth             
c          16   Cirrus Cloud Transmittance (icld = 20 only)             
c          ---  MOLECULAR LINE CENTER TRANSMITTANCE  ---                
c          17=H2O  36=CO2  31=O3   47=N2O  44=CO   46=CH4               
c          50=O2   54=NO   56=SO2  55=O2   52=NH3  11=HNO3              
          DO 90 K=1,16                                                  
              STORE=TXNEW(K,IKOFF,IPATH)                                
              TX(K)=STORE+FRAC*(TXOLD(K,IKOFF,IPATH)-STORE)                 
90        continue
          store = txnew(17,ikoff,ipath)
          tx(64)=store+frac*(txold(17,ikoff,ipath)-store)
          store = txnew(18,ikoff,ipath)
          tx(65)=store+frac*(txold(18,ikoff,ipath)-store)
          IF(modtrn)CALL bMOD(IK,ikmx,IPATH,IVX,MSOFF,MXFREQ)           
C                                                                       
C         UNIF    UNIFORMLY MIXED GASES TRANSMITTANCE                   
C         TRACE   TRACE GASES TRANSMITTANCE                             
          UNIF=TX(36)*TX(44)*TX(46)*TX(47)*TX(50)                       
          TRACE=TX(52)*TX(54)*TX(55)*TX(56)*tx(11)                      
c
          transx = 1.0
          do 95 kx = 1, nspecx                                          
             transx = transx*txx(kx)                                    
 95       continue                                                      
c
          IF(MSOFF.GT.0)THEN                                            
              IF(IK.GT.1)THEN                                           
                  ASYIK(IK-1)=TX(1)                                     
                  ASYDM(IK-1)=TX(2)                                     
              ENDIF                                                     
              ASYIK(IKMAX)=ASYIK(1)                                     
              ASYDM(IKMAX)=ASYDM(1)                                     
              DCONT(IK)=TX(12)                                          
              taer(IKmax+1-ik)=TX(13)                                   
              tAERS(IKmax+1-ik)=TX(2)                                   
              DMOLS(IK)=TX(15)                                          
              if(modtrn)TAUT(IKMAX+1-IK)=UNIF*TRACE*TX(17)*TX(31)*transx       
          ENDIF                                                         
          UNIF=unif*tx(3)                                               
          TX(9)=TX(17)*UNIF*TX(31)*TX(9)*TRACE*transx                          
          tx(50)=tx(50)*tx(3)                                           
c                                                                       
c         Ozone contributions are strictly continuum at high frequencies

cjv 11/27/95 Correction from JL. The absorbtion data in O3CHAP is from 9170 to 
cjv	24565 cm-1. The 13000 needs to be changed to 9170 such that the ozone 
cjv 	transmittance is 1.0 for wavenumbers <9170 not 13000.
cjv          if(iv.ge.13000)TX(31)=TX(8)                                   
          if(iv.ge.9170)TX(31)=TX(8)                                   
cjv ^

c                                                                       
c         If transmission only calculation, return                      
          if(transm)return                                              
C                                                                       
C         DSTRN IS CUMULATIVE SOLAR TRANSMISSION, USED IN CALCULATION   
C         OF SOLAR TRANSMISSION TO LAYER N                              
          IF(IEMSCT.EQ.1)THEN                                           
              DSTRN(IK)=1.                                              
          ELSEIF(IPATH.EQ.1)THEN                                        
              DSTRN(0)=TX(9)                                            
          ELSEIF(IPATH.EQ.2)THEN                                        
              DSTRN(IK)=TX(9)                                           
          ENDIF                                                         
          IF(IEMSCT.EQ.2)THEN                                           
              IF(MSOFF.EQ.0 .AND. IK.LT.ML)                             
     1          CALL SSRAD(IPH,IK,IPATH,V,SUMSSR)                       
              IF(IPATH.NE.3)GOTO(40,60),IPATH                           
          ENDIF                                                         
c                                                                       
c         Calculate LTE incremental optical depth                       
          deplte=tx(14)-depold                                          
          if(deplte.lt.0.)deplte=0.                                     
          depold=tx(14)                                                 
C                                                                       
C         CALCULATE the BLACK BODY FUNCTION & incremental transmittance 
          BBIK=BBFN(TBBY(IKOFF),V)                                      
          bba = bbfn(tph(ik),v)
          tlnew = tx(9)
          DTAU=tOLD-tx(9)                                               
          if(imult.eq.1)dtau=total-tx(9)
          total=tx(9)
          IF(DTAU.LE.0.)DTAU=0.                                         
          IF(IMULT.EQ.0)THEN                                            
          if(told. gt. 0.) then
             trat =tlnew/told
             odvi = 40.
             if(trat.gt.1.e-36)odvi = -(alog(trat))
             if(odvi .le. 0.) then
                trat = 1.
                odvi = 0.
             endif
          else
             trat = 0
             odvi = bigexp
          endif
          xxa = 0.2 * odvi
             tx(50)=tx(50)*tx(58)
             SUMj=SUMj+BBIK*DTAU                                        
             if(odvi .lt.1.e-6)then
                sumii = odvi *(bbik + xxa*bba)/(1.+xxa)
             else
                sumii = (1.-trat)*(bbik+xxa*bba)/(1.+xxa)
             endif
             sumv = sumv + told * sumii
c                                                                       
c             if noprnt=-1, print weighting function information        
c              IF(NOPRNT.EQ.-1)WRITE(IPR1,'(F10.0,2F7.2,1P2E12.5,        
c     1          0P,2F12.9,1p,4e10.3,0p,2f10.3)')
c     1         V,AHT(IK),AHT(IK+1),BBIK,DTAU,tx(9),trat,
c     1         sumj,sumv,bbik*dtau,told*sumii,tbby(ikoff),tph(ik)
              tOLD=tx(9)                                                
cc            IF(DTAU.LT.1.E-5 .AND. tx(9).LT.1.E-5)THEN                
cc                IF(NOPRNT.EQ.-1)                                      
cc   1              WRITE(IPR1,'(5X,5H-999.,T65,16HDTAU   EXIT LOOP)')  
cc                return                                                
cc            ENDIF                                                     
C                                                                       
C             RADIANCE / CONSERVATIVE SCATTERING                        
              go to 100
          ELSEIF(MSOFF.eq.0)then                                        
              N1=ML-IMAP(IK)                                            
              IF(N1.GE.ML)N1 = ML - 1                                   
              IF(NOPRNT.EQ.-1)THEN                                      
                  N=N1+1                                                
                  if(iemsct.ne.2)then                                   
                      umbs(n)=0.                                        
                      dmbs(n)=0.                                        
                  endif                                                 
c                  WRITE(IPR1,'(F10.0,F7.2,1P5E12.5,0PF10.5)')V,AHT(IK+1)
c     1              ,UMB(N),UMBS(N),DMB(N),DMBS(N),SUN*TX(9),TX(9)      
              ENDIF                                                     
cc            IF(DTAU.LT.1.E-5 .AND. TX(9).LT.1.E-5)THEN                
cc                IF(NOPRNT.EQ.-1)                                      
cc   1              WRITE(IPR1,'(5X,5H-999.,T65,16HDTAU   EXIT LOOP)')  
cc                return                                                
cc            ENDIF                                                     
c
cjd3 v commenting out the modtran3 section and adding the disort section
c
cj              IF(QTHETA(IK).ge.0.)THEN                                  
c                                                                       
C                 UP                                                    
cj                  sumv=sumv+dtau*(BBIK*(1.-OMEGA0(N1))+OMEGA0(N1)*      
cj     1              (DMF(N1)*(1.-BET(IK))+UMF(N1)*BET(IK))/PI)          
c      write(6,'(3h n1,i3,3h ik,i3,5h sumv,1pe10.3,5h dtau,e10.3,       
c     1  5h bbik,e10.3,/7h omega0,e10.3,4h dmf,e10.3,4h bet,e10.3,      
c     2  4h umf,e10.3,3h pi,e10.3)')                                    
c     3  n1,ik,sumv,dtau,bbik,omega0(n1),dmf(n1),bet(ik),umf(n1),pi     
cj                  summs=summs+dtau*(OMEGA0(N1)/PI*                      
cj     1              (DMFS(N1)*(1.-BET(IK))+UMFS(N1)*BET(IK)))           
cj              ELSE                                                      
c                                                                       
C                 DOWN                                                  
cj                  sumv=sumv+dtau*(BBIK*(1.-OMEGA0(N1))+OMEGA0(N1)*      
cj     1              (UMF(N1)*(1.-BET(IK))+DMF(N1)*BET(IK))/PI)          
c      write(6,'(3h n1,i3,3h ik,i3,5h sumv,1pe10.3,5h dtau,e10.3,       
c     1  5h bbik,e10.3,/7h omega0,e10.3,4h dmf,e10.3,4h bet,e10.3,      
c     2  4h umf,e10.3,3h pi,e10.3)')                                    
c     3  n1,ik,sumv,dtau,bbik,omega0(n1),dmf(n1),bet(ik),umf(n1),pi     
cj                  summs=summs+dtau*(OMEGA0(N1)/PI*                      
cj     1              (UMFS(N1)*(1.-BET(IK))+DMFS(N1)*BET(IK)))           
cj	ENDIF                                                     
c
c***************************************************************
c                 MODTRAN1 adding scheme used here - NORTH
              if(qtheta(ik).ge.0)then
c                 up                                                    tra 7590
                  sms=bbik*(1.-omega0(n1))+omega0(n1)/pi*               tra 7600
     1              (dmf(n1)*(1.-bet(ik))+umf(n1)*bet(ik))              tra 7610

                  smss=omega0(n1)/pi*                                   tra 7620
     1              (dmfs(n1)*(1.-bet(ik))+umfs(n1)*bet(ik))            tra 7630
              else                                                      tra 7640
c                 down                                                  tra 7650
                  sms=bbik*(1.-omega0(n1))+omega0(n1)/pi*               tra 7660
     1              (umf(n1)*(1.-bet(ik))+dmf(n1)*bet(ik))              tra 7670
                  smss=omega0(n1)/pi*                                   tra 7680
     1              (umfs(n1)*(1.-bet(ik))+dmfs(n1)*bet(ik))            tra 7690
              endif                                                     tra 7700
c****************************************************
c             insert for MS source and THERMAL source
c             functions returned
c             from DISORT - NORTH
               if (dis) then
                      smss=s0cms(1,n1)
                      sms=t0cms(1,n1)
               endif
              sumv=sumv+sms*dtau                                        tra 7710
              summs=summs+smss*dtau                                     tra 7720
c              if (omega0(n1).gt.1.) write(63,*) v,n1,omega0(n1)
c             
cj                if (v.eq.28571) then
cj                 write(81,*) n1,v,smss
cj                endif
c                      
cj                if (n1.eq.1) then
cj                 write(90,*) n1,v,1e7/v,smss
cj                endif
cj                if (n1.eq.10) then  
cj                 write(91,*) n1,v,1e7/v,smss
cj                endif
c                      
cj                if (n1.eq.32) then
cj                 write(92,*) n1,v,1e7/v,smss
cj                endif
c************************************************************
cjd3 ^              
          ENDIF                                                         
C                                                                       
C     END OF LAYER LOOP                                                 
c
 100  continue
      IF(MSOFF.eq.0)return                                              
c                                                                       
C     MSRAD RETURNS SOLAR AND THERMAL CONTRIBUTIONS TO RADIANCE         
C     AND THERMAL (FDNTRT) DIFFUSE FLUX FOR SURFACE REFLECTION.         
C     (THESE QUANTITIES IN COMMON /MSRD/)                               
c                                                                       
cjd3 v
cj      CALL  MSRAD(IVX,V,ISOURC,IDAY,ANGLEM)                             
        CALL MSRAD(dis,uang,nstr,IVX,V,ISOURC,IDAY,ANGLEM
     1     ,s0cms,t0cms)

cjd3 ^
c                                                                       
      IF(NOPRNT.EQ.-1)then                                              
          if(iemsct.ne.2)then                                           
              umbs(1)=0.                                                
              dmbs(1)=0.                                                
          endif                                                         
c          WRITE(IPR1,'(F10.0,F7.2,1P5E12.5,0PF10.5)')                   
c     1      V,AHT(1),UMB(1),UMBS(1),DMB(1),DMBS(1),SUN,1.               
      endif                                                             
C                                                                       
C     IKMAX AND IKMAX+1 NEED BE DEFINED FOR VIEW TO SPACE               
      IKMAX1=IKMAX-1                                                    
      COSBAR(IKMAX)=COSBAR(IKMAX1)                                      
      OMEGA0(IKMAX)=OMEGA0(IKMAX1)                                      
      UMF(IKMAX)=UMF(IKMAX1)                                            
      UMFS(IKMAX)=UMFS(IKMAX1)                                          
      DMFS(IKMAX)=DMFS(IKMAX1)                                          
      IKMAX1=IKMAX+1                                                    
      COSBAR(IKMAX1)=COSBAR(IKMAX)                                      
      OMEGA0(IKMAX1)=OMEGA0(IKMAX)                                      
      UMF(IKMAX1)=UMF(IKMAX)                                            
      UMFS(IKMAX1)=UMFS(IKMAX)                                          
      DMFS(IKMAX1)=DMFS(IKMAX)                                          
      IKMAX=ikmx                                                        

      DO 110 IK=1,IKMAX                                                 
          N=ML-IMAP(IK)                                                 
C                                                                       
C         BET(N): BACKSCATTER FRACTION ALONG OPTICAL PATH AT LAYER N    
          store=ABS(QTHETA(IK))                                         
C                                                                       
C         CALCULATE BACKSCATTER PARAMETERS                              
          IF(COSBAR(N).EQ.0.)THEN                                       
              BET(IK)=.5                                                
          ELSE                                                          
              BET(IK)=BETABS(store,COSBAR(N))                           
          ENDIF                                                         
  110 CONTINUE                                                          
      BET(IKMAX1)=.5                                                    
C                                                                       
C     Vertical path complete.  Now perform optical path calculations    
      msoff=0                                                           
C     WRITE OUT SOLAR STUFF FOR POSSIBLE FUTURE USE.                    
c      if(.not.dis)
c     $ WRITE(ISCRCH)(UMF(ik),DMF(ik),UMFS(ik),dmfs(ik),cosbar(ik),           
c     $     ik=1,laydim),(OMEGA0(ik),ik=1,laytwo),fdnsrt,fdntrt                  
      ik=-1                                                             
      total=1.
      GOTO20                                                            
      end                                                               
