      SUBROUTINE FNDPTH(CPATH,H1,HTAN,H2,RANGEI,BETA,LEN,ANGLE,         fndp 100
     $     PHI)                                                         fndp 110
C                                                                       fndp 120
C     THIS ROUTINE DETERMINES H2, BETA AND LEN.                         fndp 130
C     INPUTS ARE H1, HTAN (TANGENT HEIGHT), RANGE (RANGEI) AND THE      fndp 140
C     PATH CONSTANT, CPATH.                                             fndp 150
C     RANGEO IS THE OUTPUT RANGE WHICH SHOULD EQUAL THE INPUT RANGE.    fndp 160
C     THE ALGORITHM IS IN THE LOWTRAN 6 MANUAL, AFGL-TR-83-0187, P14-17.fndp 170
C                                                                       fndp 180
C     THE VARIABLES HAVE THEIR USUAL MEANING AS DESCRIBED IN THE REFERENfndp 190
C     SOME NOT-TOO-OBVIOUS ONES ARE:                                    fndp 200
C     CAPRJ IS FOR CAPITAL R WITH SUBSCRIPT J                           fndp 210
C     PNTGRN IS THE INTEGRAND OF EQUATION 21.                           fndp 220
C                                                                       fndp 230
      DOUBLE PRECISION R1, R2, R, DR, RPLDR,CPATH, RANGEO, PNTGRN,SAVE, fndp 240
     $     RX,RATIO,CAPRJ,XJ,XJPL1,DX,CTHETA,STHETA,BETA,DBETA,PHI,     fndp 250
     $     H1, H2, HTAN,RANGEI,Z,DZ,ANGLE,DEG, BASE,PERP,DRNG,DIFF      fndp 260
      REAL RE, DELTAS, ZMAX                                             fndp 270
      INTEGER IMAX, IMOD, IBMAX, IPATH                                  fndp 280
      INTEGER LEN, I                                                    fndp 290
      COMMON /PARMTR/ RE,DELTAS,ZMAX,IMAX,IMOD,IBMAX,IPATH              fndp 300
      DATA DR, DEG/0.005, 57.2957795/                                   fndp 310
      IF (RANGEI .LT. DR) STOP'STOPPED IN FNDPTH'                       fndp 320
C     (RANGEI .LT. DR) SHOULD NOT HAPPEN; SO THIS CHECK IS REDUNDANT.   fndp 330
      RANGEO = 0                                                        fndp 340
      BETA = 0                                                          fndp 350
      DO 200 I = 1, 2                                                   fndp 360
C                                                                       fndp 370
         IF (ANGLE .LE. 90.0D00 .AND. I .EQ. 1) GO TO 200               fndp 380
C        IF (ANGLE .LE. 90.0D00) THE PATH DOES NOT GO THROUGH HTAN.     fndp 390
C        IF (ANGLE .LE. 90.0D00) THE I = 1 CALCULATION SHOULD NOT BE DONfndp 400
C        IF (ANGLE .LE. 90.0D00) FOR I = 2, R1 = RE + H1.               fndp 410
C                                                                       fndp 420
         IF (I .EQ. 1) THEN                                             fndp 430
            R1 = RE+H1                                                  fndp 440
            R2 = RE+HTAN                                                fndp 450
         ELSEIF (I .EQ. 2) THEN                                         fndp 460
            IF (HTAN .LT. 0.001 .AND. ANGLE .GT. 90) GO TO 200          fndp 470
C           IF (HTAN APPROXIMATELY 0) THEN YOU ARE ABOUT TO HIT THE EARTfndp 480
            R2 = RE + ZMAX                                              fndp 490
            IF (ANGLE .LE. 90.0D00) THEN                                fndp 500
               R1 = RE + H1                                             fndp 510
            ELSE                                                        fndp 520
               R1 =RE + HTAN                                            fndp 530
            ENDIF                                                       fndp 540
         ENDIF                                                          fndp 550
         IF (R2 .LT. R1) THEN                                           fndp 560
            DZ = -DR                                                    fndp 570
         ELSE                                                           fndp 580
            DZ = DR                                                     fndp 590
         ENDIF                                                          fndp 600
         DO 100 R = R1, R2-DZ, DZ                                       fndp 610
            Z = R - RE                                                  fndp 620
            CALL IRFXN(Z,RX,RATIO)                                      fndp 630
            STHETA = CPATH/(RX*R)                                       fndp 640
            IF(STHETA .GT. 1.0)STHETA = 1.
            IF(STHETA .LT.-1.0)STHETA =-1.
            SAVE = STHETA                                               fndp 650
            CTHETA = SQRT(1-STHETA**2)                                  fndp 660
            IF (R1 .GT. R2) CTHETA = -CTHETA                            fndp 670
C           IF (R1 .GT. R2) THEN CTHETA IS NEGATIVE BECAUSE THETA .GT. 9fndp 680
            XJ = R*CTHETA                                               fndp 690
            CAPRJ = -R/RATIO                                            fndp 700
            PNTGRN = 1/(1-CAPRJ*STHETA*STHETA)                          fndp 710
            RPLDR = R+DZ                                                fndp 720
            Z = RPLDR - RE                                              fndp 730
            CALL IRFXN(Z,RX,RATIO)                                      fndp 740
            STHETA = CPATH/(RX*RPLDR)                                   fndp 750
            CTHETA = SQRT(1-STHETA**2)                                  fndp 760
            IF (R1 .GT. R2) CTHETA = -CTHETA                            fndp 770
            XJPL1 = RPLDR*CTHETA                                        fndp 780
            DX = XJPL1 - XJ                                             fndp 790
            DRNG = PNTGRN*DX                                            fndp 800
            RANGEO = RANGEO + DRNG                                      fndp 810
C                                                                       fndp 820
            DBETA = (((SAVE+STHETA)*0.5) * (PNTGRN*DX)) /               fndp 830
     $           (R -0.5*DZ)                                            fndp 840
            BETA = BETA+DBETA                                           fndp 850
            IF (RANGEO .GE. RANGEI) THEN                                fndp 860
               DIFF = (RANGEI-(RANGEO-DRNG))                            fndp 870
               H2 = R -RE + (DZ/DRNG)*DIFF                              fndp 880
C              H2 = R - RE + DZ*0.5                                     fndp 890
               BETA = BETA*DEG                                          fndp 900
               IF (I .EQ. 2) THEN                                       fndp 910
                  LEN = 1                                               fndp 920
                  IF (ANGLE .LE. 90.0D00) LEN = 0                       fndp 930
                  IF (H2 .LT. HTAN) THEN                                fndp 940
C                    THIS WILL BE THE CASE IF I = 2, AND YOU HAVE       fndp 950
C                    GONE THROUGH THE R-LOOP BARELY (ONLY) ONCE.        fndp 960
                     H2 = HTAN                                          fndp 970
                     LEN = 0                                            fndp 980
                  ENDIF                                                 fndp 990
               ELSE                                                     fndp1000
                  LEN = 0                                               fndp1010
               ENDIF                                                    fndp1020
C                                                                       fndp1030
C              CORRECTION FOR VERY SHORT PATHS; HERE IT IS ABOUT 5 KM ORfndp1040
               IF (RANGEI .LT. 5.0 .AND. RANGEO/RANGEI .GT. 1.05) THEN  fndp1050
C                 CALCULATE BETA BY STARIGHT LINE GEOMETRY.             fndp1060
                  PERP  = SIN(ANGLE/DEG)*RANGEI                         fndp1070
                  BASE = COS(ANGLE/DEG)*RANGEI + RE+H1                  fndp1080
                  BETA = ATAN(PERP/BASE)*DEG                            fndp1090
                  RANGEO = RANGEI                                       fndp1100
                  H2 = BASE - RE                                        fndp1110
               ENDIF                                                    fndp1120
               PHI = 180 - ACOS(CTHETA)*DEG                             fndp1130
               RETURN                                                   fndp1140
            ENDIF                                                       fndp1150
 100     CONTINUE                                                       fndp1160
 200  CONTINUE                                                          fndp1170
C                                                                       fndp1180
C     COMES HERE IF YOU HAVE REACHED ZMAX, BUT YOUR RANGEI IS STILL     fndp1190
C     NOT EQUAL TO OUTPUT VALUE.                                        fndp1200
C     IN THIS CASE DO THE FOLLOWING.                                    fndp1210
C                                                                       fndp1220
      RANGEI = RANGEO                                                   fndp1230
      H2 = ZMAX                                                         fndp1240
      IF (ANGLE .LE. 90) THEN                                           fndp1250
         LEN = 0                                                        fndp1260
      ELSE                                                              fndp1270
         LEN = 1                                                        fndp1280
      ENDIF                                                             fndp1290
      IF (HTAN .LT. 0.001 .AND. ANGLE .GT. 90) THEN                     fndp1300
C        YOU HAVE HIT THE EARTH IF YOU ARE AT THIS POINT OF THE CODE    fndp1310
         LEN = 0                                                        fndp1320
         H2 = 0                                                         fndp1330
      ENDIF                                                             fndp1340
      BETA = BETA*DEG                                                   fndp1350
      PHI = 180 - ACOS(CTHETA)*DEG                                      fndp1360
      RETURN                                                            fndp1370
      END                                                               fndp1380
      SUBROUTINE FRN296(V1C,FH2O)                                       frn2 100
C     LOADS FOREIGN CONTINUUM  296K                                     frn2 110
      COMMON /FH2O/ V1,V2,DV,NPT,F296(2003)                             frn2 120
      CALL SINT(V1,V1C,DV,NPT,F296,FH2O)                                frn2 130
      RETURN                                                            frn2 140
      END                                                               frn2 150
      FUNCTION   GAMFOG(MR,FREQ,T,RHO)                                  gmfg 100
C                                                                       gmfg 110
C        COMPUTES ATTENUATION OF EQUIVALENT LIQUID WATER CONTENT        gmfg 120
C       IN CLOUDS OR FOG IN DB/KM                                       gmfg 130
C       CONVERTED TO NEPERS BY NEW CONSTANT 1.885                       gmfg 140
C                                                                       gmfg 150
C        FREQ = WAVENUMBER (INVERSE CM)                                 gmfg 160
C        T    = TEMPERATURE (DEGREES KELVIN)                            gmfg 170
C        RHO  = EQUIVALENT LIQUID CONTENT  (G/CUBIC METER)              gmfg 180
C      CINDEX=COMPLEX DIELECTRIC CONSTANT M  FROM INDEX                 gmfg 190
C      WAVL = WAVELENGTH IN CM                                          gmfg 200
C                                                                       gmfg 210
      COMPLEX CINDEX                                                    gmfg 220
      IF(RHO.GT.0.) GO TO 2                                             gmfg 230
      GAMFOG=0.                                                         gmfg 240
      RETURN                                                            gmfg 250
    2 CONTINUE                                                          gmfg 260
      KEY=1                                                             gmfg 270
      IF(MR. GE. 5) KEY = 0                                             gmfg 280
      WAVL=1.0/FREQ                                                     gmfg 290
      TC=T-273.2                                                        gmfg 300
CCC                                                                     gmfg 310
CCC    CHANGE TEMP SO THAT MINIMUM IS -20.0 CENT.                       gmfg 320
CCC                                                                     gmfg 330
      IF(TC.LT.-20.0) TC=-20.0                                          gmfg 340
      CALL INDX (WAVL,TC,KEY,REIL,AIMAK)                                gmfg 350
      CINDEX=CMPLX(REIL,AIMAK)                                          gmfg 360
CCC                                                                     gmfg 370
CCC   ATTENUATION = 6.0*PI*FREQ*RHO*IMAG(-K)                            gmfg 380
CCC    6.0*PI/10. = 1.885 (THE FACTOR OF 10 IS FOR UNITS CONVERSION)    gmfg 390
CCC                                                                     gmfg 400
C     GAMFOG=8.1888*FREQ*RHO*AIMAG( -  (CINDEX**2-1)/(CINDEX**2+2))     gmfg 410
      GAMFOG=1.885 *FREQ*RHO*AIMAG( -  (CINDEX**2-1)/(CINDEX**2+2))     gmfg 420
      RETURN                                                            gmfg 430
      END                                                               gmfg 440
      SUBROUTINE GEO(IERROR,BENDNG,MAXGEO,MSOFF)                        geo  110
      INCLUDE 'parameter.list'
C*********************************************************************  geo  120
C     THIS SUBROUTINE SERVES AS AN INTERFACE BETWEEN THE MAIN           geo  130
C     LOWTRAN7 PROGRAM 'LWTRN7' AND THE NEW SET OF SUBROUTINES,         geo  140
C     INCLUDING 'GEOINP', 'REDUCE', 'FDBETA', 'EXPINT', 'DPEXNT', 'DPFNMgeo  150
C     'DPFISH', 'DPSCHT', 'DPANDX', 'DPRARF', 'DPRFPA', 'DPFILL',       geo  160
C     AND 'DPLAYR',  WHICH CALCULATE THE ABSORBER                       geo  170
C     AMOUNTS FOR A REFRACTED PATH THROUGH THE ATMOSPHERE.              geo  180
C     THE INPUT PARAMETERS ITYPE, H1, H2, ANGLE, RANGE, BETA, AND LEN   geo  190
C     ALL FUNCTION IN THE SAME WAY IN THE NEW ROUTINES AS IN THE OLD.   geo  200
C*********************************************************************  geo  210
C     *** IN COLM 73-76 MARK IMPROVED 32 BIT CONVERGENCE OF BETA        geo  220
C     SUGESTED BY TONY WARRIC             MCDONNELL DOUGLAS             geo  230
C     AND  MAJ ROBERT G. HUGES            AFWAL/WEA                     geo  240
C***********************************************************************geo  250
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

      CHARACTER*8 CNAMEX                                       
C
C     TRANS VARIABLES
C
      COMMON /NAMEX/CNAMEX(MMOLX)                                               
      COMMON /MDATAX/ WMOLXT(MMOLX,laydim)                                  
      COMMON /MODELX/ DNSTYX(MMOLX,LAYDIM)                                  
      COMMON /RFRPTX/ DENPX(MMOLX,laydim+1),AMTPX(MMOLX,laydim+1)
      COMMON /NONAME/ TXX(MMOLX),WX(MMOLX),WPATHX(laythr,MMOLX)
      COMMON /SOLSX/  WPTHSX(laythr,MMOLX),TBBYSX(laythr,MMOLX),
     $     PATMSX(laythr,MMOLX)
c
c
c
      DOUBLE PRECISION DPH1, DPH2 , DPANGL, DPPHI, DPHMIN,              geo  260
     $     DPBETA, DPBEND,DPRANG,DHALFR, DPRNG2,SMMIN                   geo  270
      DOUBLE PRECISION DPZP,DPPP,DPTP,DPRFN,                            geo  280
     $     DPSP,DPTPSU,DPRHOP,DPDENP,DPAMTP,DPPPSU                      geo  290
      LOGICAL LSMALL,LSAVE,LPRINT,LNOGEO                                geo  300
c                                                                       geo  310
C     SSI COMMENTS ON DOUBLE PRECISION VARIABLES:                       geo  320
C     RFRPTH IS THE OLD COMMON BLOCK IN SINGLE PRECISION.               geo  330
C     DPRFRP IS THE SAME COMMON BLOCK IN DOUBLE PRECISION; IT IS NEW.   geo  340
C     IN THIS ROUTINE DP IS USED AS A PREFIX TO DENOTE THE              geo  350
C     SINGLE PRECISION VARIABLES OF DPRFRP.                             geo  360
C     THE FOLLOWING ARE THE EXCEPTIONS:                                 geo  370
C     DPRFN  STANDS FOR THE OLD DOUBLE PRECISION RFNDXP                 geo  380
C     DPPPSU STANDS FOR THE OLD DOUBLE PRECISION PPSUM                  geo  390
C     DPTPSU STANDS FOR THE OLD DOUBLE PRECISION TPSUM                  geo  400
C     DPRHOP STANDS FOR THE OLD DOUBLE PRECISION RHOPSM                 geo  410
c                                                                       geo  420
C     SOME OTHER VARIABLES WERE DECLARED IN DOUBLE PRECISION.           geo  430
C     THEY ARE identified by the prefix dp.                             geo  440
C     SINCE THESE DO NOT INVOLVE COMMON BLOCKS NOTHING MORE ABOUT       geo  450
C     THEM IS SAID.  SEE THE DECLARATIONS ABOVE FOR THE SPECIFIC        geo  460
C     VARIABLES.                                                        geo  470
c                                                                       geo  480
      COMMON RELHUM(LAYDIM),HSTOR(LAYDIM),ICH(4),VH(17),TX(65),W(65)  
      COMMON IMSMX,WPATH(LAYTHR,65),TBBY(LAYTHR),PATM(LAYTHR),NSPEC,   
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX2(47),AWCCON(5)  
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON /CARD1/ MODEL,ITYPE,IEMSCT,M1,M2,M3,IM,NOPRNT,TBOUND,SALB  geo  540
     1  ,MODTRN                                                         geo  550
      LOGICAL MODTRN                                                    geo  560
      LOGICAL LOGLOS                                                    geo  570
      COMMON /CARD2/ IHAZE,ISEASN,IVULCN,ICSTL,ICLD,IVSA,VIS,WSS,WHH,   geo  580
     1    RAINRT                                                        geo  590
      COMMON /CARD3/ H1,H2,ANGLE,RANGE,BETA,REE,LEN                     geo  600
C     COMMON /CARD4/ V1,V2,DV                                           geo  610
      COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     geo  620
      COMMON /CNTRL/ KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT               geo  630
      COMMON /MODEL/ ZM(LAYDIM),PM(LAYDIM),TM(LAYDIM),RFNDX(LAYDIM),
     1  DENSTY(65,LAYDIM),CLDAMT(LAYDIM),RRAMT(LAYDIM),EQLWC(LAYDIM),
     1  HAZEC(LAYDIM)
      COMMON /PARMTR/ RE,DELTAS,ZMAX,IMAX,IMOD,IBMAX,IPATH              geo  660
      COMMON /RFRPTH/ ZP(LAYDIM+1),PP(LAYDIM+1),TP(LAYDIM+1),
     $     rfndxp(LAYDIM+1),SP(LAYDIM+1),
     $     PPSUm(LAYDIM+1),TPSUm(LAYDIM+1),RHOPsm(LAYDIM+1),
     $     DENP(65,LAYDIM+1),AMTP(65,LAYDIM+1)
      COMMON /DPRFRP/ DPZP(LAYDIM+1),DPPP(LAYDIM+1),DPTP(LAYDIM+1),
     $  DPRFN(LAYDIM+1),DPSP(LAYDIM+1),DPPPSU(LAYDIM+1),
     $  DPTPSU(LAYDIM+1),DPRHOP(LAYDIM+1),DPDENP(65,LAYDIM+1),
     $     DPAMTP(65,LAYDIM+1)
      COMMON/SOLS/AH1(LAYTWO),ARH(LAYTWO),WPATHS(LAYTHR,65),
     1 PA(LAYTWO),PR(LAYTWO),ATHETA(LAYDIM+1),ADBETA(LAYDIM+1),
     2 LJ(LAYTWO+1),JTURN,ANGSUN,CSZEN(LAYTWO),TBBYS(LAYTHR,12),
     3 PATMS(LAYTHR,12)
      COMMON /PATH/PL(LAYTWO),QTHETA(LAYTWO),ITEST,HI,HF,
     1  AHT(LAYTWO),tph(LAYTWO)
      COMMON /SMALL1/DHALFR, DPRNG2                                     geo  790
      COMMON /SMALL2/ LSMALL                                            geo  800
      COMMON /SMALL3/SMALL                                              geo  810
      COMMON /CPRINT/LPRINT                                             geo  820
cssi  COMMON /GRoUND/GNDALT                                             geo  830
      COMMON /GRAUND/GNDALT                                             geo  840
      DATA TLRNCE/0.001/                                                geo  850
      DIMENSION KMOL(17)                                                geo  860
      DATA PZERO/1013.25/                                               geo  870
      data kmax2 /65/
c
C*****KMOL(K) IS A POINTER USED TO REORDER THE AMOUNTS WHEN PRINTING    geo  930
      DATA KMOL/1,2,3,11,8,5,9,10,4,6,7,12,13,14,16,15,17/              GEO  940
C
      LSMALL = .FALSE.                                                  geo  890
      LPRINT = .TRUE.                                                   geo  900
      LNOGEO = .FALSE.                                                  geo  910
C                                                                       geo  920
C*****INITIALIZE CONSTANTS AND CLEAR CUMULATIVE VARIABLES               geo  950
C*****DELTAS IS THE NOMINAL PATH LENGTH INCRENMENT USED IN THE RAY TRACEgeo  960
cc      if(h1.lt.zm(1))h1=zm(1)
cc      if(h2.lt.zm(1))h2=zm(1)
      if(gndalt.gt.zm(1)) then
         print*,' gndalt is gt 1st altitude ',gndalt,zm(1),model
      endif
      DELTAS = 5.0                                                      geo  970
      JMAXST=1                                                          geo  980
      IERROR = 0                                                        geo  990
      RE = REE                                                          geo 1000
      IMOD = ML                                                         geo 1010
      IMAX = ML                                                         geo 1020
C*****ZERO OUT CUMULATIVE VARIABLES                                     geo 1030
      DO 100 I=1,laydim+1                                               geo 1040
      LJ(I)=0                                                           geo 1050
      SP(I)=0.0                                                         geo 1060
      PPSUM(I) = 0.0                                                    geo 1070
      TPSUM(I) = 0.0                                                    geo 1080
      RHOPSM(I) = 0.0                                                   geo 1090
      DPSP(I)=0.0                                                       geo 1100
      DPPPSU(I) = 0.0                                                   geo 1110
      DPTPSU(I) = 0.0                                                   geo 1120
      DPRHOP(I) = 0.0                                                   geo 1130
      DO 100 K=1,KMAX+2+NSPECX
         if ( k .gt. kmax+2) then
            kx = k - (kmax+2)
            amtpx(kx,I) = 0.0
            go to 100
         endif
         AMTP(K,I) = 0.0      
         DPAMTP(K,I) = 0.0       
  100 CONTINUE                                                          geo 1170
      ZMAX=ZM(IMAX)                                                     geo 1180
      IF(ISSGEO.EQ.1) GO TO 200                                         geo 1190
      IF(ITYPE.GE.2)  GO TO 200                                         geo 1200
C*****HORIZONTAL PATH, MODEL EQ 1 TO 7:  INTERPOLATE PROFILE TO H1      geo 1210
      ZP(1)=H1                                                          geo 1220
      DPZP(1) = ZP(1)                                                   geo 1230
      IF(ML   .EQ.1)  THEN                                              geo 1240
          PP(1)=PM(1)                                                   geo 1250
          DPPP(1)=PM(1)                                                 geo 1260
          TP(1)=TM(1)                                                   geo 1270
          tph(1) = tp(1)
          DPTP(1)=TM(1)                                                 geo 1280
          LJ(1)=1                                                       geo 1290
          SP(1)=RANGE                                                   geo 1300
          DPSP(1)=RANGE                                                 geo 1310
          PL(1)=RANGE                                                   geo 1320
      ELSE                                                              geo 1330
          IF(H1.LT.ZM(1))  then                                         geo 1370
             print*,' error horiz path h1 lt zm(1) ',h1,zm(1)
             IERROR = 1 
             return
          endif
          IF(MODEL.EQ.0)  GO TO 145                                     geo 1340
          DO 120 I=2,ML                                                 geo 1350
          I2 = I                                                        geo 1360
          IF(H1.LT.ZM(I))  GO TO 130                                    geo 1370
  120     CONTINUE                                                      geo 1380
  130     CONTINUE                                                      geo 1390
          I1 = I2-1                                                     geo 1400
          FAC = (H1-ZM(I1))/(ZM(I2)-ZM(I1))                             geo 1410
          CALL EXPINT(PP(1),PM(I1),PM(I2),FAC)                          geo 1420
          TP(1) = TM(I1)+(TM(I2)-TM(I1))*FAC                            geo 1430
          tph(1) = tp(1)
          DPTP(1) = TM(I1)+(TM(I2)-TM(I1))*FAC                          geo 1440
          II1=I1                                                        geo 1450
          IF(FAC.GT.0.5) II1=I2                                         geo 1460
          LJ(1)=II1                                                     geo 1470
          SP(II1)=RANGE                                                 geo 1480
          DPSP(II1)=RANGE                                               geo 1490
          DO 140 K=1,KMAX+2+NSPECX
             IF ( K .LE. KMAX+2) THEN
                CALL EXPINT(DENP(K,1),DENSTY(K,I1),DENSTY(K,I2),FAC)
             ELSE
                KX = K - (KMAX+2)
                CALL EXPINT(DENPX(KX,1),DNSTYX(KX,I1),DNSTYX(KX,I2),FAC)
             ENDIF
  140     CONTINUE              
      END IF                                                            geo 1530
C*****CALCULATE ABSORBER AMOUNTS FOR A HORIZONTAL PATH                  geo 1540
 145  WRITE(IPR,36)  H1,RANGE,MODEL                                     geo 1550
   36 FORMAT('0HORIZONTAL PATH AT ALTITUDE = ',F10.3,                   geo 1560
     1   ' KM WITH RANGE = ',F10.3,' KM, MODEL = ',I3)                  geo 1570
      IKMAX = 1                                                         geo 1580
      IMMAX = 1                                                         geo 1590
C     IF(MODEL.EQ.0) TP(1)=TM(1)                                        geo 1600
C     TBBY(1) = TP(1)                                                   geo 1610
      JMAX=1                                                            geo 1620
      IF(MODEL.EQ.0)THEN                                                geo 1630
          TP(1)=TM(1)                                                   geo 1640
          PP(1)=PM(1)                                                   geo 1650
          DPTP(1)=TM(1)                                                 geo 1660
          DPPP(1)=PM(1)                                                 geo 1670
      ENDIF                                                             geo 1680
      MSOFF1=MSOFF+1                                                    geo 1690
      PATM(MSOFF1)=PP(1)/PZERO                                          geo 1700
      TBBY(MSOFF1) = TP(1)                                              geo 1710
      DO 160 K=1,KMAX+2+NSPECX                                          geo 1720
         IF (K .LE. KMAX+2) THEN
            CONTINUE            
            IF(ML.EQ.1)  THEN          
               DENP(K,1) = DENSTY(K,1)    
               DPDENP(K,1) = DENSTY(K,1)  
            ENDIF                         
            W(K) = DENP(K,1)*RANGE           
            WPATH(MSOFF1,K) = W(K)                 
         ELSE
            KX = K - (KMAX+2)
            IF(ML.EQ.1) DENPX(KX,1) = DNSTYX(KX,1)    
            WX(KX) = DENPX(KX,1)*RANGE           
            WPATHX(MSOFF1,KX) = WX(KX)                
         ENDIF
  160 CONTINUE                                                          geo 1800
      WTEM = (296.0-TP(1))/(296.0-260.0)                                geo 1810
cc    IF(WTEM.LT.0.)WTEM=0.                                             geo 1820
cc    IF(WTEM.GT.1)WTEM=1.                                              geo 1830
      W(9)=W(5)*WTEM                                                    geo 1840
      W(59) = W(8) * .269 *(TP(1)-273.15)                               geo 1850
      W(60) = W(8) * .269 *(TP(1)-273.15)**2                            geo 1860
      WPATH(MSOFF1,9)=W(9)                                              geo 1900
      WPATH(MSOFF1,59)=W(59)                                            geo 1910
      WPATH(MSOFF1,60)=W(60)                                            geo 1920
      GO TO 320                                                         geo 1930
  200 CONTINUE                                                          geo 1940
C*****SLANT PATH SELECTED                                               geo 1950
C*****INTERPRET SLANT PATH PARAMETERS                                   geo 1960
C                                                                       geo 1970
C                                                                       geo 1980
C                                                                       geo 1990
C     LOGLOS IS A LOGICAL VARIABLE THAT IS TRUE ONLY WHEN YOU ARE       geo 2000
C     DEALING WITH THE LOS OF ITYPE = 2, NO SUN OR NOTHING SOLAR.       geo 2010
C                                                                       geo 2020
      IF(ITYPE.EQ.2 .AND. ISSGEO.EQ.0 .AND. MSOFF.EQ.0) THEN            geo 2030
         LOGLOS=.TRUE.                                                  geo 2040
      ELSE                                                              geo 2050
         LOGLOS=.FALSE.                                                 geo 2060
      ENDIF                                                             geo 2070
C                                                                       geo 2080
      IF (LOGLOS) THEN                                                  geo 2090
C$$$                                                                    geo 2100
C$$$         IF (H1 .NE. 0 .AND. RANGE .NE. 0) THEN                     geo 2110
C$$$            IF (H2 .EQ. 0 .AND. ANGLE .EQ. 0) H2 = 1E-4             geo 2120
C$$$C           IF H1 .NE. 0 .AND. RANGE .NE. 0, BUT EVERYTHING ELSE .EQgeo 2130
C$$$C           THEN MAKE IT A (H1, H2 , RANGE) OR CASE 2C PATH.        geo 2140
C$$$C           THIS IS WHAT LOWTRAN DOES.                              geo 2150
C$$$         ENDIF                                                      geo 2160
         CALL PSLCT(H1,H2,ANGLE,RANGE,BETA,ISLCT)                       geo 2170
         IF (ISLCT .NE. 21) THEN
C           21 IDENTIFIES PATH 2A.  IT DOES NOT NEED SPECIAL TREATMENT.
c           BUT ALL OTHER SMALL PATHS DO.
            CALL SMPREP(H1,H2,ANGLE,RANGE,BETA,LEN,ISLCT)                 
            IF (RANGE .GT. 0 .AND. RANGE .LE. SMALL)THEN                  
               LSMALL = .TRUE.                                            
               SAVER = RANGE                                              
               ISLCT = 23                                                 
C              SMALL PATHS TREATED AS CASE 2C                             
               GO TO 12                                                   
            ENDIF                                          
         ENDIF               
      ELSE                                                             
         LSMALL = .FALSE.                                              
      ENDIF                                                             geo 2280
 1    CONTINUE                                                          geo 2290
C$$$      IF (ITYPE .EQ. 2 .AND. H2.EQ. 0. .AND. BETA .EQ. 0) THEN      geo 2300
      IF (ITYPE .EQ. 2 .AND. ISLCT .EQ. 22 .AND. LOGLOS) THEN           geo 2310
C        CASE 2B:  H2.EQ. 0. .AND. BETA .EQ. 0                          geo 2320
C                                                                       geo 2330
C                                                                       geo 2340
C     IF (PATH TYPE .EQ. INTERNAL CASE 2B) MAKE SURE THAT               geo 2350
C     THE RANGE USED IN THE CALCULATION EQUALS THE DESIRED INPUT.       geo 2360
C     THIS MEANS FIGURING OUT A NEW H2 WHICH IS DIFFERENT FROM          geo 2370
C     THE USUSAL MODTRAN H2.                                            geo 2380
C     ALSO REGARDLESS OF THE INPUT LEN VALUE, SET LEN = 0 OR 1, DEPENDINgeo 2390
C     WHAT IT TAKES TO MAKE OUTPUT AND INPUT RANGES EQUAL.              geo 2400
C     THIS SEGMENT OF THE CODE INCLUDING THE SUBROUTINE NEWH2 WILL DO THgeo 2410
         LENSAV = LEN                                                   geo 2420
         SAVER = RANGE                                                  geo 2430
         CALL NEWH2(H1,H2,ANGLE,RANGE,BETA,LEN,HMIN,TMPPHI)             geo 2440
         IF (LEN .EQ. 0) HMIN = MIN(H2,H1)                              geo 2450
         DPH1 = H1                                                      geo 2460
         DPH2 = H2                                                      geo 2470
         DPANGL = ANGLE                                                 geo 2480
         DPPHI = TMPPHI                                                 geo 2490
         DPRANG = RANGE                                                 geo 2500
C        JULY 9, 92---LPRNT = .FALSE.                                   geo 2510
         LPRINT = .FALSE.                                               geo 2520
         IAMT = 2                                                       geo 2530
         DPHMIN = HMIN                                                  geo 2540
         CALL DPRFPA(DPH1,DPH2,DPANGL,DPPHI,                            geo 2550
     $        LEN,DPHMIN,IAMT,DPBETA,DPRANG,DPBEND)                     geo 2560
C        JULY 9, 92---LPRNT = .TRUE.                                    geo 2570
         LPRINT = .TRUE.                                                geo 2580
         PRCNT = 100*ABS(DPRANG-SAVER)/SAVER                            geo 2590
C        WRITE '1' IS CONTROL CHARACTER - TOP OF NEXT PAGE
         WRITE(IPR,'(A1)')'1'
         WRITE(IPR,*)'SOME INTERNAL DETAILS:'
         WRITE(IPR,*)
         WRITE(IPR,*)'LOS IS INTERNAL CASE 2B (H1, ANGLE, RANGE).'      GEO 2610
         WRITE(IPR,*)'USING H2 OBTAINED FROM SUBROUTINE NEWH2:'
         WRITE(IPR,*)                                                   geo 2640
         WRITE(IPR,*)'H1                        = ', H1                 geo 2650
         WRITE(IPR,*)'H2                        = ', H2                 geo 2660
         WRITE(IPR,*)'ANGLE                     = ', ANGLE              geo 2670
         WRITE(IPR,*)'PHI                       = ', TMPPHI             geo 2680
         WRITE(IPR,*)'BETA                      = ', BETA               geo 2690
         WRITE(IPR,*)'HMIN (MINIMUM ALTITUDE)   = ',HMIN                geo 2700
         WRITE(IPR,*)'RANGE (OUTPUT)            = ', DPRANG             geo 2710
         WRITE(IPR,*)'RANGE (INPUT)             = ', SAVER              geo 2720
         WRITE(IPR,*)'PERCENT DIFFERENCE        = ',PRCNT               geo 2730
         WRITE(IPR,*)'LEN                       = ', LEN                geo 2740
         WRITE(IPR,*)                                                   geo 2750
         IF (ANGLE .GT. 90 .AND. ABS(H2) .LT. TLRNCE) THEN              geo 2760
            LNOGEO = .TRUE.                                             geo 2770
            PHI = TMPPHI                                                geo 2780
            ANGLE = DPANGL                                              geo 2790
            HMIN = DPHMIN                                               geo 2800
            BETA = DPBETA                                               geo 2810
            RANGE = DPRANG                                              geo 2820
            WRITE(IPR,*)                                                geo 2830
            WRITE(IPR,*)'***** WARNING *****  PATH HITS THE EARTH'      geo 2840
            WRITE(IPR,*)                                                geo 2850
         ELSEIF (ANGLE .LE. 90 .AND. ABS(H2-ZMAX) .LT. TLRNCE) THEN     geo 2860
            LNOGEO = .TRUE.                                             geo 2870
            PHI = TMPPHI                                                geo 2880
            ANGLE = DPANGL                                              geo 2890
            HMIN = DPHMIN                                               geo 2900
            BETA = DPBETA                                               geo 2910
            RANGE = DPRANG                                              geo 2920
            WRITE(IPR,*)                                                geo 2930
            WRITE(IPR,*)'** WARNING **  PATH HITS UPPERMOST LYR BNDRY'  geo 2940
            WRITE(IPR,*)                                                geo 2950
         ELSEIF (PRCNT .LE. 1.0) THEN                                   geo 2960
            WRITE(IPR,*)
     $           'PERCENT DIFFERENCE BEING .LT. 1, THESE PATH',
     $           ' PARAMETERS WILL BE USED '
            WRITE(IPR,*)'WITHOUT CALLING GEOINP.'
            WRITE(IPR,*)                                                geo 3010
            LNOGEO = .TRUE.                                             geo 3020
            PHI = TMPPHI                                                geo 3030
            ANGLE = DPANGL                                              geo 3040
            HMIN = DPHMIN                                               geo 3050
            BETA = DPBETA                                               geo 3060
            RANGE = DPRANG                                              geo 3070
        ELSE                                                            geo 3080
            WRITE(IPR,*)'SINCE THE PERCENT DIFFERENCE IS .GT. 1,'       geo 3090
            WRITE(IPR,*)'''EQUIVALENT'' INTERNAL CASE 2C WILL BE USED.' geo 3100
            WRITE(IPR,*)                                                geo 3110
            LNOGEO = .FALSE.                                            geo 3120
            RANGE = SAVER                                               geo 3130
            BETA = 0                                                    geo 3140
            HMIN = 0                                                    geo 3150
            PHI = 0                                                     geo 3160
            LEN = LENSAV                                                geo 3170
            ANGLE = 0                                                   geo 3180
            CALL PSLCT(H1,H2,ANGLE,RANGE,BETA,ISLCT)                    geo 3190
        ENDIF                                                           geo 3200
      ENDIF                                                             geo 3210
C                                                                       geo 3220
C                                                                       geo 3230
 12   CONTINUE                                                          geo 3240
      LSAVE = LSMALL                                                    geo 3250
      LSMALL = .FALSE.                                                  geo 3260
      IF (.NOT. LNOGEO)                                                 geo 3270
     $     CALL GEOINP(H1,H2,ANGLE,RANGE,BETA,ITYPE,LEN,HMIN,PHI,       geo 3280
     $     IERROR,ISLCT,LOGLOS)                                         geo 3290
      LSMALL = LSAVE                                                    geo 3300
      IF (LSMALL) THEN                                                  geo 3310
         WRITE(IPR,*)                                                   geo 3320
         WRITE(IPR,*)'NOTE:  MESSAGE FROM GEO:  RE:'                    geo 3330
         WRITE(IPR,*)'PRECEDING TWO SETS OF CALCULATIONS FROM GEOINP:'  geo 3340
         WRITE(IPR,*)'THE RANGE IS SMALL, THAT IS, LSMALL = .TRUE.'     geo 3350
         WRITE(IPR,*)'ABOVE CALCULATIONS, INCLUDING ITERATIONS AROUND'  geo 3360
         WRITE(IPR,*)'ANGLE UNTIL BETA CONVERGED, WERE DONE IN'         geo 3370
         WRITE(IPR,*)'... <--- FDBETA <--- GEOINP <--- GEO. '           geo 3380
         WRITE(IPR,*)                                                   geo 3390
         WRITE(IPR,*)'THEY ARE NOT USED EXCEPT FOR LEN & IERROR.'       geo 3400
         WRITE(IPR,*)                                                   geo 3410
         WRITE(IPR,*)'SEE SUMMARY OF GEOMETRY CALCULATION FOR ACCURATE' geo 3420
         WRITE(IPR,*)'VALUES OF GEOMETRY PARAMETERS.'                   geo 3430
         WRITE(IPR,*)                                                   geo 3440
      ENDIF                                                             geo 3450
C                                                                       geo 3460
C     CHECK FOR IERROR                                                  geo 3470
      IF (IERROR .NE. 0) GO TO 4                                        geo 3480
      IF (LSMALL) THEN                                                  geo 3490
         RANGE = SAVER                                                  geo 3500
         CALL SMGEO(H1,H2,ANGLE,RANGE,BETA,PHI,DHALFR,DPRNG2,           geo 3510
     $        BENDNG,LEN,SMMIN)                                         geo 3520
         HMIN = SMMIN                                                   geo 3530
         GO TO 11                                                       geo 3540
      ENDIF                                                             geo 3550
C                                                                       geo 3560
C                                                                       geo 3570
 4    CONTINUE                                                          geo 3580
      IF(IERROR.EQ.0) GO TO 210                                         geo 3590
      IF(ISSGEO.NE.1)WRITE(IPR,38)                                      geo 3600
   38 FORMAT('0GEO:  IERROR NE 0: END THIS CALCULATION AND SKIP TO'     geo 3610
     1    ,' THE NEXT CASE')                                            geo 3620
      RETURN                                                            geo 3630
  210 CONTINUE                                                          geo 3640
C*****CALCULATE THE PATH THROUGH THE ATMOSPHERE                         geo 3650
 11   CONTINUE                                                          geo 3660
      IAMT = 1                                                          geo 3670
      DPH1 = H1                                                         geo 3680
      DPH2 = H2                                                         geo 3690
      DPANGL = ANGLE                                                    geo 3700
      DPPHI = PHI                                                       geo 3710
      DPHMIN = HMIN                                                     geo 3720
      DPBETA = BETA                                                     geo 3730
      DPRANG = RANGE                                                    geo 3740
      DPBEND = BENDNG                                                   geo 3750
C     CALL RFPATH(H1,H2,ANGLE,PHI,LEN,HMIN,IAMT,BETA,RANGE,BENDNG)      geo 3760
      IF (ABS(ANGLE - 90.0) .LT. .001 .AND. ABS(HMIN - H1).LT.TLRNCE    geo 3770
     $     .AND. LEN .EQ. 1 .AND. SAVER .GT. SMALL) LEN = 0             geo 3780
C     WHAT HAPPENS IF (ABS(ANGLE - 90.0) .LT. 0.0001 .AND.              geo 3790
C     ABS(HMIN - H1) .LT. 0.0001 .AND. LEN .EQ. 1 .AND. SAVER .GT. SMALLgeo 3800
C     YOU HAVE A "RIGHT TRIANGLE" PATH WHERE H1, H2 AND THE EARTH       geo 3810
C     CENTER FORM A RIGHT TRIANGLE.                                     geo 3820
C     IN THIS CASE YOU DON'T WANT LEN = 1 BECAUSE THAT MAY              geo 3830
C     MAKE HMIN = H1 WHICH GIVES PROBLEMS IN SUBROUTINE FILL.           geo 3840
C     THE CHECK ON THE VARIABLE SMALL SIMPLY POINTS OUT THE FACT        geo 3850
C     SMALL PATHS ARE ALREADY TAKEN INTO ACCOUNT BY OTHER METHODS.      geo 3860
      CALL DPRFPA(DPH1,DPH2,DPANGL,DPPHI,                               geo 3870
     $     LEN,DPHMIN,IAMT,DPBETA,DPRANG,DPBEND)                        geo 3880
      H1 = DPH1                                                         geo 3890
      H2 = DPH2                                                         geo 3900
      ANGLE = DPANGL                                                    geo 3910
      PHI = DPPHI                                                       geo 3920
      HMIN = DPHMIN                                                     geo 3930
      IF (HMIN .LT. GNDALT-TLRNCE .AND.                                 geo 3940
     $     (LOGLOS .OR. ITYPE .EQ. 3)) THEN                             geo 3950
C        LOGLOS ONLY DEALS WITH ITYPE = 2.                              geo 3960
C        THEREFORE, THE CHECK FOR ITYPE = 3.                            geo 3970
         WRITE(IPR,*)                                                   geo 3980
         WRITE(IPR,*)'GNDALT = ',GNDALT                                 geo 3990
         WRITE(IPR,*)'HMIN = ',HMIN                                     geo 4000
         WRITE(IPR,*)'HMIN IS .LT. THAN GNDALT'                         geo 4010
         WRITE(IPR,*)'THIS RUN ABORTED, NEXT RUN ATTEMPTED'             geo 4020
         WRITE(IPR,*)                                                   geo 4030
         IERROR = 1                                                     geo 4040
         RETURN                                                         geo 4050
      ENDIF                                                             geo 4060
      BETA = DPBETA                                                     geo 4070
      RANGE = DPRANG                                                    geo 4080
      BENDNG = DPBEND                                                   geo 4090
C*****UNFOLD LAYER AMOUNTS IN AMTP INTO THE CUMULATIVE                  geo 4100
C*****AMOUNTS IN WPATH FROM H1 TO H2                                    geo 4110
      DO 220 I=1,IPATH                                                  geo 4120
      IF(H1.EQ.ZP(I)) IH1 = I                                           geo 4130
      IF(H2.EQ.ZP(I)) IH2 = I                                           geo 4140
  220 CONTINUE                                                          geo 4150
      JMAX = (IPATH-1)+LEN*(MIN0(IH1,IH2)-1)                            geo 4160
      IKMAX = JMAX                                                      geo 4170
C*****DETERMINE LJ(J), WHICH IS THE NUMBER OF THE LAYER IN AMTP(K,L),   geo 4180
C*****STARTING FROM HMIN, WHICH CORRESPONDS TO THE LAYER J IN           geo 4190
C*****WPATH(J,K), STARTING FROM H1                                      geo 4200
C*****WPATH(J+MSOFF,K), STARTING FROM H1                                geo 4210
C*****INITIAL DIRECTION OF PATH IS DOWN                                 geo 4220
      L = IH1                                                           geo 4230
      LDEL = -1                                                         geo 4240
      IF(LEN.EQ.1 .OR. H1.GT.H2)  GO TO 230                             geo 4250
C*****INITIAL DIRECTION OF PATH IS UP                                   geo 4260
      L = 0                                                             geo 4270
      LDEL = 1                                                          geo 4280
  230 CONTINUE                                                          geo 4290
      JTURN = 0                                                         geo 4300
      JMAXP1=JMAX+1                                                     geo 4310
      DO 250 J=1,JMAXP1                                                 geo 4320
C*****TEST FOR REVERSING DIRECTION OF PATH FROM DOWN TO UP              geo 4330
      IF(L.NE.1 .OR. LDEL.NE.-1)  GO TO 240                             geo 4340
      JTURN = J                                                         geo 4350
      L = 0                                                             geo 4360
      LDEL = +1                                                         geo 4370
  240 CONTINUE                                                          geo 4380
      L = L+LDEL                                                        geo 4390
      LJ(J) = L                                                         geo 4400
  250 CONTINUE                                                          geo 4410
C*****LOAD TBBY AND WPATH                                               geo 4420
C*****TBBY IS DENSITY WEIGHTED MEAN TEMPERATURE                         geo 4430
      AMTTOT=0.                                                         geo 4440
      MSOFF1=MSOFF+1                                                    geo 4450
      DO 255 K=1,KMAX+2+NSPECX    
         IF ( K .LE. KMAX+2) THEN
C           WPATH(1,K) = 0.0         
            WPATH(MSOFF1,K)=0.    
         ELSE   
            WPATHX(MSOFF1,K-(KMAX+2))=0.
         ENDIF
 255  CONTINUE
      IMAX = 0                                                          geo 4490
      DO 265 J=1,JMAX                                                   geo 4500
      L = LJ(J)                                                         geo 4510
      IF(L.GE.ML)L = ML                                                 geo 4520
      IF(L.GE.IMAX) IMAX = L                                            geo 4530
C     TBBY(J) = TPSUM (L)/RHOPSM(L)                                     geo 4540
      MSOFFJ=MSOFF+J                                                    geo 4550
      TBBY(MSOFFJ)=TPSUM(L)/RHOPSM(L)                                   geo 4560
      PATM(MSOFFJ)=PPSUM(L)/(PZERO*RHOPSM(L))                           geo 4570
      AMTTOT=AMTTOT+RHOPSM(L)                                           geo 4580
      J1 = J-1                                                          geo 4590
      IF(J1.EQ.0)  J1 = 1                                               geo 4600
      DO 260 K=1,KMAX+2+NSPECX                                            CFC
         IF(K.EQ.9) GO TO 260                                           CFC
         IF ( K .LE. KMAX+2) THEN                                         CFC
C           WPATH(J,K) = WPATH(J1,K)+AMTP(K,L)                          CFC
            WPATH(MSOFFJ,K)=WPATH(MSOFF+J1,K)+AMTP(K,L)                 CFC
         ELSE                                                           CFC
            KX = K-(KMAX+2)                                                 CFC
            WPATHX(MSOFFJ,KX)=WPATHX(MSOFF+J1,KX)+AMTPX(KX,L)           CFC
         ENDIF                                                          CFC
  260 CONTINUE                                                          geo 4650
      WPATH(MSOFFJ,59)=WPATH(MSOFF+J1,59) +                             geo 4710
     1                       AMTP(8,L)*.269*(TBBY(MSOFFJ)-273.15)       geo 4720
      WPATH(MSOFFJ,60)=WPATH(MSOFF+J1,60) +                             geo 4730
     1                       AMTP(8,L)*.269*(TBBY(MSOFFJ)-273.15)**2    geo 4740
      WTEM=(296.-TBBY(MSOFFJ))/(296.-260.)                              geo 4750
cc    IF(WTEM.LT.0.0) WTEM = 0.                                         geo 4760
cc    IF(WTEM.GT.1.0) WTEM = 1.0                                        geo 4770
C     WPATH(J,9) = WPATH(J1,9)+WTEM*AMTP(5,L)                           geo 4780
      WPATH(MSOFFJ,9)=WPATH(MSOFF+J1,9)+WTEM*AMTP(5,L)                  geo 4790
265   CONTINUE                                                          geo 4800
      MSOFFJ=MSOFF+JMAX                                                 geo 4810
      DO 270 K=1,KMAX+NSPECX+2
         IF ( K .LE. KMAX+2) THEN
C           W(K) = WPATH(JMAX,K)                                            
            W(K) = WPATH(MSOFFJ,K)     
         ELSE
            KX = K - (KMAX+2)                                       
            WX(KX) = WPATHX(MSOFFJ,KX)
         ENDIF
  270 CONTINUE                                                          geo 4850
      JMAXST=JMAX                                                       geo 4860
CC    JMAX = IMAX                                                       geo 4870
      IMMAX = IMAX                                                      geo 4880
      IKMAX=JMAX                                                        geo 4890
C*****INCLUDE BOUNDARY EMISSION IF:                                     geo 4900
C*****    1. NON ZERO TBOUND IS READ IN ON CARD 1                       geo 4910
C*****    2. SLANT PATH INTERSECTS THE EARTH (TBOUND                    geo 4920
C*****       SET TO TEMPERATURE OF LOWEST BOUNDARY)                     geo 4930
      IF(TBOUND.EQ.0.0.AND.H2.EQ.ZM(1)) TBOUND=TM(1)                    geo 4940
C*****PRINT CUMULATIVE ABSORBER AMOUNTS                                 geo 4950
CC    IF(NPR.EQ.1) GO TO 315                                            geo 4960
CC    WRITE(IPR,42)                                                     geo 4970
C  42 FORMAT(////,' CUMULATIVE ABSORBER AMOUNTS FOR THE PATH FROM'      geo 4980
      IF(NPR.EQ.2) GO TO 315                                            geo 4990
      IF(NPR.NE.1)WRITE(IPR,42)                                         geo 5000
   42 FORMAT(////,'1CUMULATIVE ABSORBER AMOUNTS FOR THE PATH FROM'      geo 5010
     1    ,' H1 TO Z',//,T3,'J',T9,'Z',T18,'TBAR',T27,                  geo 5020
     2    'HNO3', T39,'O3 UV',T50,'CNTMSLF1 ',                          geo 5030
     +    T061,'CNTMSLF2',T73,'CNTMFRN',                                geo 5040
     +    T86,'O2',                                                     geo 5050
     3              /,T8,'(KM)',T19,'(K)',T25,                          geo 5060
     4        '(ATM CM)',T37,'(ATM CM)',                                geo 5070
     5    T49,'(MOL CM-2)',T61,'(MOL CM-2)',T73,'(MOL CM-2)',           geo 5080
     +    T83,'(MOL CM-2)'/)                                            geo 5090
C*****GOING DOWN, LP = 0,    GOING UP, LP = 1                           geo 5100
      LP = 1                                                            geo 5110
      IF(LEN.EQ.1 .OR. H1.GT.H2)  LP = 0                                geo 5120
      AHT(1) = H1                                                       geo 5130
      ilj = 1
      do 280 j = 1 ,ml-1
      if(h1.lt.zm(j))go to 280
      ilj = j
280   continue
      if(ilj.eq.ml) ilj = ml-1
      ij1 = 0
      call interp(ij1,h1,zm(ilj),zm(ilj+1),tmph1,tm(ilj),tm(ilj+1))
      tph(1) = tmph1
      DO 300 J=1,JMAX                                                   geo 5140
      L = LJ(J)                                                         geo 5150
      IF(J.EQ.JTURN)  LP = 1                                            geo 5160
      LZ = L+LP                                                         geo 5170
      AHT(J+1) = ZP(LZ)                                                 geo 5180
      tph(j+1) = tp(lz)
C     IF(NPR.NE.1)WRITE(IPR,44)J,ZP(LZ),TBBY(J),(WPATH(J,KMOL(K)),K=    geo 5190
C    X 4,8),WPATH(J,58)                                                 geo 5200
      MSOFFJ=MSOFF+J                                                    geo 5210
      IF(NPR.NE.1)WRITE(IPR,'(I3,F9.3,F9.2,1P8E12.3)')J,ZP(LZ),         geo 5220
     1  TBBY(MSOFFJ),(WPATH(MSOFFJ,KMOL(K)),K=4,8),WPATH(MSOFFJ,58)     geo 5230
   44 FORMAT(I3,F9.3, F9.2,1P8E12.3)                                    geo 5240
  300 CONTINUE                                                          geo 5250
      IF(NPR.NE.1)WRITE(IPR,46)                                         geo 5260
C  46 FORMAT(///,T3,'J',T09,'Z',T17,'N2 CONT',T28,'MOL SCAT',T43,       geo 5270
   46 FORMAT(///,'1 J',T09,'Z',T17,'N2 CONT',T28,'MOL SCAT',T43,        geo 5280
     1    'AER 1',T55,'AER 2',T67,'AER 3',T79,'AER 4',T091,'CIRRUS',/   geo 5290
     2    ,T8,'(KM)',/)                                                 geo 5300
      LP = 1                                                            geo 5310
      IF(LEN.EQ.1  .OR. H1.GT.H2) LP = 0                                geo 5320
      DO 310 J=1,JMAX                                                   geo 5330
      L = LJ(J)                                                         geo 5340
      IF(J.EQ.JTURN) LP = 1                                             geo 5350
      LZ = L+LP                                                         geo 5360
C     IF(NPR.NE.1)WRITE(IPR,48) J,ZP(LZ),(WPATH(J,KMOL(K)),K=9,15)      geo 5370
      MSOFFJ=MSOFF+J                                                    geo 5380
      IF(NPR.NE.1)WRITE(IPR,'(I3,F9.3,1P7E12.3)')J,ZP(LZ),              geo 5390
     1  (WPATH(MSOFFJ,KMOL(K)),K=9,15)                                  geo 5400
   48 FORMAT(I3,F9.3,1P7E12.3)                                          geo 5410
  310 CONTINUE                                                          geo 5420
C*****PRINT PATH SUMMARY                                                geo 5430
315   IF(ISSGEO.EQ.1) GO TO 320                                         geo 5440
C     IF(NPR.NE.1)WRITE(IPR,47)                                         geo 5450
C  47 FORMAT(///'   J    Z       H2O       O3        CO2       CO    ', geo 5460
      IF(NPR.LT.1)THEN                                                  geo 5470
          IF(MODTRN)THEN                                                geo 5480
              WRITE(IPR,'(///39H1  J    Z       H2O       O3        CO2,geo 5490
     1          50H       CO        CH4       N2O       O2        NH3,  geo 5500
     2          30H       NO        NO2       SO2,/                     geo 5510
     3          50H      (KM)    (                                   ,  geo 5520
     4          50H                   ATM CM                         ,  geo 5530
     5          21H                    ),/)')                           geo 5540
          ELSE                                                          geo 5550
              WRITE(IPR,47)                                             geo 5560
          ENDIF                                                         geo 5570
      ENDIF                                                             geo 5580
   47 FORMAT(///'1  J    Z       H2O       O3        CO2       CO    ', geo 5590
     +'    CH4       N2O       O2        NH3       NO        NO2   ',   geo 5600
     +'    SO2'/'      (KM)   (G/CM**2)  (                           ', geo 5610
     +'                 ATM CM                                     ',   geo 5620
     +'        )'/)                                                     geo 5630
      LP = 1                                                            geo 5640
      IF(LEN.EQ.1  .OR. H1.GT.H2) LP = 0                                geo 5650
      DO 312 J=1,JMAX                                                   geo 5660
      L = LJ(J)                                                         geo 5670
      IF(J.EQ.JTURN) LP = 1                                             geo 5680
      LZ = L+LP                                                         geo 5690
C     IF(NPR.NE.1)WRITE(IPR,49) J,ZP(LZ),WPATH(J,17),WPATH(J,31),       geo 5700
C    + WPATH(J,36),WPATH(J,44),WPATH(J,46),WPATH(J,47),WPATH(J,50),     geo 5710
C    + WPATH(J,52),WPATH(J,54),WPATH(J,55),WPATH(J,56)                  geo 5720
          MSOFFJ=MSOFF+J                                                geo 5730
          IF(NPR.LT.1)WRITE(IPR,49)J,ZP(LZ),WPATH(MSOFFJ,17),           geo 5740
     1      WPATH(MSOFFJ,31),WPATH(MSOFFJ,36),WPATH(MSOFFJ,44),         geo 5750
     2      WPATH(MSOFFJ,46),WPATH(MSOFFJ,47),WPATH(MSOFFJ,50),         geo 5760
     3      WPATH(MSOFFJ,52),WPATH(MSOFFJ,54),WPATH(MSOFFJ,55),         geo 5770
     4      WPATH(MSOFFJ,56)                                            geo 5780
  312 CONTINUE                                                          geo 5790
C                                                                       CFC
      IF (NPR .LT. 1) THEN                                              CFC
         WRITE(IPR,'(///12H1  J    Z   ,11(2x,a8))')(cnamex(kx),kx=1,11)cfc
         WRITE(IPR,'(12X,11(2x,a8))')(cnamex(kx),kx=12,min(22,nspecx))  cfc
         write(ipr,'(47H      (KM)    (                                ,CFC
     1        53H                   ATM CM                            , CFC
     2        21H                    ),/)')                             CFC    
         DO 314 J=1,JMAX                                                CFC
            L = LJ(J)                                                   CFC
            IF(J.EQ.JTURN) LP = 1                                       CFC   
            LZ = L+LP                                                   CFC   
            MSOFFJ=MSOFF+J                                              CFC
            WRITE(IPR,49)J,ZP(LZ),(WPATHX(MSOFFJ,KX),KX=1,11)           cfc
            WRITE(IPR,51)(WPATHX(MSOFFJ,KX),KX=12,min(22,nspecx))       CFC
 314     CONTINUE                                                       CFC
      ENDIF                                                             CFC
C
   49 FORMAT(I4,F8.2,1P11E10.2)                                         geo 5800
      WRITE(IPR,40)H1,H2,ANGLE,RANGE,BETA,PHI,HMIN,BENDNG,LEN           geo 5810
   40 FORMAT(//,'0SUMMARY OF THE GEOMETRY CALCULATION',//,              geo 5820
     1 10X,'H1      = ',F10.3,' KM',/,10X,'H2      = ',F10.3,' KM',/,   geo 5830
     110X,'ANGLE   = ',F10.3,' DEG',/,10X,'RANGE   = ',F10.3,' KM',/,   geo 5840
     310X,'BETA    = ',F10.3,' DEG',/,10X,'PHI     = ',F10.3,' DEG',/,  geo 5850
     4 10X,'HMIN    = ',F10.3,' KM',/,10X,'BENDING = ',F10.3,' DEG',/,  geo 5860
     5 10X,'LEN     = ',I10)                                            geo 5870
  320 CONTINUE                                                          geo 5880
C*****CALCULATE THE AEROSOL WEIGHTED MEAN RH                            geo 5890
      IF(W(7).GT.0.0 .AND. ICH(1).LE.7)  W15   = W(15)/W(7)             geo 5900
      IF(W(12).GT.0.0 .AND. ICH(1).GT.7)  W15   = W(15)/W(12)           geo 5910
C                                                                       geo 5920
C     INVERSE OF LOG REL HUM                                            geo 5930
C                                                                       geo 5940
      IF(W(7).GT.0.0 .AND. ICH(1).LE.7)  THEN                           geo 5950
           W(15) = 100. - EXP(W15)                                      geo 5960
      ENDIF                                                             geo 5970
      IF(W(12).GT.0.0 .AND. ICH(1).GT.7)  THEN                          geo 5980
           W(15) = 100. - EXP(W15)                                      geo 5990
      ENDIF                                                             geo 6000
      IF (W(7).LE.0.0 .AND. ICH(1).LE.7 )W(15) = 0.                     geo 6010
      IF(W(12).LE.0.0 .AND. ICH(1).GT.7)  W(15) = 0.                    geo 6020
C  CONVERT WPATH TO INCREMENTAL PATH AMOUNTS FOR MODTRAN RUNS           geo 6030
      IF(MODTRN)THEN                                                    geo 6040
          JMAXM1=JMAX-1                                                 geo 6050
          MSOFFP=MSOFF+JMAX                                             geo 6060
          DO 340 J=JMAXM1,1,-1                                          geo 6070
              MSOFFJ=MSOFF+J                                            geo 6080
              DO 330 K=1,NSPC                                           geo 6090
                 KP=KPOINT(K)                                           geo 6100
                 WPATH(MSOFFP,KP)=WPATH(MSOFFP,KP)-WPATH(MSOFFJ,KP)     geo 6110
 330          continue
              DO 335 Kx=1,Nspecx                                        cfc
                  WPATHx(MSOFFP,Kx)=WPATHx(MSOFFP,Kx)-WPATHx(MSOFFJ,Kx) cfc
 335       continue                                                     cfc
  340     MSOFFP=MSOFFJ                                                 geo 6120
      ENDIF                                                             geo 6130
C                                                                       geo 6140
C*****PRINT TOTAL PATH AMOUNTS                                          geo 6150
      IF(H1.LT.ZM(1))  then                                             geo 1370
         print*,' error horiz path h1 lt zm(1) ',h1,zm(1)
         IERROR = 1 
         return
      endif
      IF(H2.LT.ZM(1).and.itype.ne.1)  then                                             geo 1370
         print*,' error horiz path h2 lt zm(1) ',h2,zm(1)
         IERROR = 1 
         return
      endif
      IF(ISSGEO.EQ.1) RETURN                                            geo 6160
      WRITE(IPR,50)  (W(KMOL(K)),K=4,16)                                geo 6170
C  50 FORMAT(////,' EQUIVALENT SEA LEVEL TOTAL ABSORBER AMOUNTS',//,    geo 6180
   50 FORMAT(////,'1EQUIVALENT SEA LEVEL TOTAL ABSORBER AMOUNTS',//,    geo 6190
     1    T18,'HNO3',T28,'O3 UV',                                       geo 6200
     2    T40,'CNTMSLF1',T52,'CNTMSLF2',T63,'CNTMFRN',/,                geo 6210
     3                   T16,'(ATM CM)',T27,'(ATM CM)',T40,             geo 6220
     4    '(MOL CM-2)',T52,'(MOL CM-2)',T64,'(MOL CM-2)',               geo 6230
     +    T76,'(MOL CM-2)',                                             geo 6240
     +    //,10X,1P5E12.3,///,                                          geo 6250
     5    T15,'N2 CONT',T26,'MOL SCAT',T41,'AER 1', T53,'AER 2',        geo 6260
     6    T65,'AER 3',T77, 'AER 4',T87,'CIRRUS',T99,'MEAN RH'/,         geo 6270
     7    T99,'(PRCNT)',//,10X,1P7E12.3,0PF12.2)                        geo 6280
 51   format(12x,1P11E10.2)                                             cfc
C                                                                       geo 6290
      IF(MODTRN)THEN                                                    geo 6300
          WRITE(IPR,'(//T18,3HH2O,T29,2HO3,T41,3HCO2,T53,2HCO,T65,3HCH4,geo 6310
     1      T77,3HN2O,T90,2HO2,/T16,1H(,T57,6HATM CM,T91,1H),//10X,     geo 6320
     2      1P7E12.3,//T18,3HNH3,T29,2HNO,T41,3HNO2,T53,3HSO2,/T18,1H(, geo 6330
     3      T34,6HATM CM,T55,1H),//10X,1P4E12.3)')W(17),W(31),W(36),    geo 6340
     4      W(44),W(46),W(47),W(50),W(52),W(54),W(55),W(56)             geo 6350
      ELSE                                                              geo 6360
      WRITE(IPR,60) W(17),W(31),W(36),W(44),W(46),W(47),W(50),          geo 6370
     +              W(52),W(54),W(55),W(56)                             geo 6380
      ENDIF                                                             geo 6390
C
      MAXWRT = MIN(7,NSPECX)
      WRITE(IPR,111)(CNAMEX(IX),IX=1,MAXWRT)
      WRITE(IPR,222)(WX(IX),IX=1,MAXWRT)
      MAXWRT = MIN(14,NSPECX)
      WRITE(IPR,111)(CNAMEX(IX),IX=8,MAXWRT)
      WRITE(IPR,222)(WX(IX),IX=8,MAXWRT)
 111  FORMAT(/10X,7(2X,A,2X))
 222  FORMAT(T18,1H(,T57,6HATM CM,T91,1H),//10X,1P7E12.3)
C
   60 FORMAT(                                                           geo 6400
     1 //T18,'H2O',T29,'O3',T41,'CO2',T53,'CO',T65,'CH4',T77,'N2O',     geo 6410
     +   T90,'O2',                                                      geo 6420
     2  /T15,'(G/CM**2)',T29,'(',T57,'ATM CM',T91,')',//10X,1P7E12.3,   geo 6430
     3 //T18,'NH3',T29,'NO',T41,'NO2',T53,'SO2',/T18,'(',T34,'ATM CM',  geo 6440
     +   T55,')',                                                       geo 6450
     4 //10X,1P4E12.3)                                                  geo 6460
      IF(JMAXST .GT. MAXGEO) THEN                                       geo 6470
      WRITE(IPR,900)MAXGEO,JMAXST                                       geo 6480
 900  FORMAT(//'  CURRENT GEOMETRY DIMENSION ',I5 ,/                    geo 6490
     X,' JMAXST = ',I5,' RESET AVTRAT TDIFF1 TDIFF2 TO 2. 10. 20.')     geo 6500
      STOP 'GEO :JMAXST .GT. MAXGEO'                                    geo 6510
      ENDIF                                                             geo 6520
cssi  DO 350 IK=1,JMAXST                                                geo 6530
cssi  IL=LJ(IK)                                                         geo 6540
cssi  RNPATH(IK)=SP(IL)                                                 geo 6550
cssi  RRAMTK(IK)=RRAMT(IL)                                              geo 6560
cssi350CONTINUE                                                         geo 6570
C                                                                       geo 6580
500   FORMAT(/10X,'ICH(1),W(15),W(7),W(12)=',I5,1P3E12.3/)              geo 6590
      RETURN                                                            geo 6600
      END                                                               geo 6610
      SUBROUTINE GEOINP(SPH1,SPH2,SPANGL,SPRANG,SPBETA,                 geoi 100
     $     ITYPE,LEN,SPHMIN,SPPHI,IERROR,ISLCT,LOGLOS)                  geoi 110
C***********************************************************************geoi 120
C     GEOINP INTERPRETS THE ALLOWABLE COMBINATIONS OF INPUT PATH        geoi 130
C     PARAMETERS INTO THE STANDARD SET H1,H2,ANGLE,PHI,HMIN, AND LEN.   geoi 140
C     THE ALLOWABLE COMBINATIONS OF INPUT PARAMETERS ARE- FOR ITYPE = 2,geoi 150
C     (SLANT PATH H1 TO H2) A. H1, H2, AND ANGLE, B. H1, ANGLE, AND     geoi 160
C     RANGE, C. H1, H2, AND RANGE, D. H1, H2, AND BETA -                geoi 170
C     FOR ITYPE = 3 (SLANT PATH H1 TO SPACE H2 = 100 KM),               geoi 180
C     A. H1 AND ANGLE, B. H1 AND HMIN (INPUT AS H2).                    geoi 190
C     THE SUBROUTINE ALSO DETECTS BAD INPUT (IMPOSSIBLE GEOMETRY) AND   geoi 200
C     ITYPE = 2 CASES WHICH INTERSECT THE EARTH, AND RETURNS THESE      geoi 210
C     CASES WITH ERROR FLAGS.                                           geoi 220
C     THE SUBROUTINE DPFNMN (PREVIOUSLY, FNDHMN) IS CALLED TO CALCULATE geoi 230
c     THE MINIMUM HEIGHT ALONG THE PATH, & PHI, THE ZENITH ANGLE AT H2, geoi 240
c     USING THE ATMOSPHERIC PROFILE STORED IN /MODEL/                   geoi 250
C***********************************************************************geoi 260
c                                                                       geoi 270
c     sp denotes single precision variable.                             geoi 280
C     SP___ ALSO SUGGESTS THAT ___ IS A DOUBLE PRECISION VARIABLE.      geoi 290
c                                                                       geoi 300
      REAL SPH1, SPH2, SPANGL, SPRANG, SPBETA, SPHMIN,SPPHI,            geoi 310
     $     RE, DELTAS, ZMAX, PI, CA, DEG, GCAIR, BIGNUM, BIGEXP         geoi 320
      INTEGER IRD, IPR, IPU, NPR, IPR1, IMAX, IMOD,IBMAX,IPATH,         geoi 330
     $     ITYPE, LEN,IERROR,ITER,ISLCT                                 geoi 340
      double precision H1, H2, ANGLE, RANGE, BETA,HMIN,PHI,H2ST,        geoi 350
     $     R1,R2,ZARG1,ERARG1,ZARG2,ERARG2,ZARG3,ERARG3,STORE           geoi 360
      LOGICAL LOGLOS                                                    geoi 370
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON /PARMTR/ RE,DELTAS,ZMAX,IMAX,IMOD,IBMAX,IPATH              geoi 390
      COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     geoi 400
C                                                                       geoi 410
C     LOGLOS IS A LOGICAL VARIABLE THAT IS TRUE ONLY WHEN YOU ARE       geoi 420
C     DEALING WITH THE LOS OF ITYPE = 2, NO SUN OR NOTHING SOLAR.       geoi 430
C     DEFINED IN GEO                                                    geoi 440
C                                                                       geoi 450
      H1 = SPH1                                                         geoi 460
      H2 = SPH2                                                         geoi 470
      ANGLE = SPANGL                                                    geoi 480
      RANGE = SPRANG                                                    geoi 490
      BETA = SPBETA                                                     geoi 500
      HMIN = SPHMIN                                                     geoi 510
      PHI = SPPHI                                                       geoi 520
C                                                                       geoi 530
      ITER = 0                                                          geoi 540
      IF(ITYPE.NE.3) GO TO 120                                          geoi 550
C***  SLANT PATH TO SPACE                                               geoi 560
C***  NOTE:  IF BOTH HMIN AND ANGLE ARE ZERO, THEN ANGLE IS             geoi 570
C***  ASSUMED SPECIFIED                                                 geoi 580
      IF(H2.NE.0.0)  GO TO 110                                          geoi 590
C***  CASE 3A: H1, SPACE, ANGLE                                         geoi 600
C     IF(NPR.NE.1)WRITE(IPR,10)                                         geoi 610
      IF(NPR.LT.1)WRITE(IPR,10)                                         geoi 620
 10   FORMAT(//,' CASE 3A: GIVEN H1,H2=SPACE,ANGLE')                    geoi 630
      H2 =  ZMAX                                                        geoi 640
      CALL DPFNMN(H1,ANGLE,H2,LEN,ITER,HMIN,PHI,IERROR)                 geoi 650
C     IF(IERROR.EQ.-5) RETURN                                           geoi 660
      IF(IERROR.EQ.-5) GO TO 999                                        geoi 670
      GO TO 200                                                         geoi 680
 110  CONTINUE                                                          geoi 690
C***  CASE 3B:  H1, HMIN, SPACE                                         geoi 700
C     IF(NPR.NE.1)WRITE(IPR,12)                                         geoi 710
      IF(NPR.LT.1)WRITE(IPR,12)                                         geoi 720
 12   FORMAT(//,' CASE 3B: GIVEN H1, HMIN, H2=SPACE')                   geoi 730
      HMIN = H2                                                         geoi 740
      H2 = ZMAX                                                         geoi 750
      IF(H1.LT.HMIN) GO TO 9001                                         geoi 760
      STORE = 90.0                                                      geoi 770
      CALL DPFNMN(HMIN,STORE,H1,LEN,ITER,HMIN,ANGLE,IERROR)             geoi 780
      STORE = 90.0                                                      geoi 790
      CALL DPFNMN(HMIN,STORE,H2,LEN,ITER,HMIN,PHI,IERROR)               geoi 800
      IF(HMIN.LT.H1) LEN = 1                                            geoi 810
      GO TO 200                                                         geoi 820
 120  CONTINUE                                                          geoi 830
      IF(ITYPE.NE.2) GO TO 9002                                         geoi 840
      IF (.NOT. LOGLOS) THEN                                            geoi 850
         IF(RANGE.NE.0.0.OR.BETA.NE.0.0) GO TO 130                      geoi 860
      ELSE                                                              geoi 870
         IF(ISLCT .NE. 21) GO TO 130                                    geoi 880
      ENDIF                                                             geoi 890
C***  CASE 2A:  H1, H2, ANGLE                                           geoi 900
C     IF(NPR.NE.1)WRITE(IPR,16)                                         geoi 910
      IF(NPR.LT.1)WRITE(IPR,16)                                         geoi 920
 16   FORMAT(//, ' CASE 2A: GIVEN H1, H2, ANGLE')                       geoi 930
      IF(H1.GE.H2.AND.ANGLE.LE.90.0) GO TO 9004                         geoi 940
      IF(H1.EQ.0.0 .AND. ANGLE.GT.90.0) GO TO 9007                      geoi 950
C     IF(H2.LT.H1.AND.ANGLE.GT.90.0.AND.NPR.NE.1) WRITE(IPR,15) LEN     geoi 960
      IF(H2.LT.H1.AND.ANGLE.GT.90.0.AND.NPR.LT.1) WRITE(IPR,15) LEN     geoi 970
 15   FORMAT(//,' EITHER A SHORT PATH (LEN=0) OR A LONG PATH ',         geoi 980
     1     'THROUGH A TANGENT HEIGHT (LEN=1) IS POSSIBLE: LEN = ',      geoi 990
     2     I3)                                                          geoi1000
      H2ST = H2                                                         geoi1010
      CALL DPFNMN(H1,ANGLE,H2,LEN,ITER,HMIN,PHI,IERROR)                 geoi1020
      IF(H2.NE.H2ST)  GO TO 9007                                        geoi1030
      GO TO 200                                                         geoi1040
 130  CONTINUE                                                          geoi1050
      IF (.NOT. LOGLOS) THEN                                            geoi1060
         IF(BETA.EQ.0.0) GO TO 133                                      geoi1070
      ELSE                                                              geoi1080
         IF(ISLCT .NE. 24) GO TO 133                                    geoi1090
      ENDIF                                                             geoi1100
      CALL FDBETA(H1,H2,BETA,ANGLE,PHI,LEN,HMIN,IERROR)                 geoi1110
      GO TO 200                                                         geoi1120
 133  CONTINUE                                                          geoi1130
      IF (.NOT. LOGLOS) THEN                                            geoi1140
         IF(ANGLE.EQ.0.0) GO TO 140                                     geoi1150
      ELSE                                                              geoi1160
         IF(ISLCT .EQ. 23) GO TO 140                                    geoi1170
      ENDIF                                                             geoi1180
C***  CASE 2B:  H1, ANGLE, RANGE                                        geoi1190
C***  ASSUME NO REFRACTION                                              geoi1200
C     IF(NPR.NE.1)WRITE(IPR,18)                                         geoi1210
      IF(NPR.LT.1)WRITE(IPR,18)                                         geoi1220
 18   FORMAT(//,' CASE 2B: GIVEN H1, ANGLE, RANGE',//                   geoi1230
     1     10X,'NOTE: H2 IS COMPUTED FROM H1, ANGLE, AND RANGE ',       geoi1240
     2     'ASSUMING NO REFRACTION')                                    geoi1250
      R1 = RE+H1                                                        geoi1260
      R2 = SQRT(R1**2+RANGE**2+2.0*R1*RANGE*COS(+ANGLE/DEG))            geoi1270
      H2 = R2-RE                                                        geoi1280
      IF(H2.GE.0.0)  GO TO 135                                          geoi1290
      H2 = 0.0                                                          geoi1300
      R2 = RE+H2                                                        geoi1310
      RANGE = -R1*COS(ANGLE/DEG)-SQRT(R2**2-R1**2*(SIN(ANGLE/DEG))**2)  geoi1320
C     IF(NPR.NE.1)WRITE(IPR,17) RANGE                                   geoi1330
C     IF(NPR.LT.1)WRITE(IPR,17) RANGE                                   geoi1340
 17   FORMAT(//,10X,'CALCULATED H2 IS LESS THAN ZERO:',/,               geoi1350
     1     10X,'RESET H2 = 0.0 AND RANGE = ',F10.3)                     geoi1360
 135  CONTINUE                                                          geoi1370
C***  NOTE:  GEOMETRIC PHI IS NEEDED TO DETERMINE LEN (0 OR 1).         geoi1380
C***  PHI IS THEN RECOMPUTED IN dpfnmn (previously, FNDHMN)             geoi1390
C     ZARG1=(R2**2+RANGE**2-R1**2)/(2.0*R2*RANGE)                       geoi1400
      ZARG1 = ((R2+R1)*(R2-R1))/(2.0*R2*RANGE) + RANGE/(2.0*R2)         geoi1410
      ERARG1=ABS(ZARG1)-1.0                                             geoi1420
      IF((ERARG1.GT.1.0E-6).OR.(ERARG1.LT.0.0)) GO TO 137               geoi1430
      IF (ZARG1.LT.0.0) GO TO 138                                       geoi1440
      ZARG1=1.0                                                         geoi1450
      GO TO 137                                                         geoi1460
 138  ZARG1=-1.0                                                        geoi1470
 137  PHI = 180.0-ACOS(ZARG1)*DEG                                       geoi1480
      LEN = 0                                                           geoi1490
      IF(ANGLE.GT.90.0.AND.PHI.GT.90.0) LEN = 1                         geoi1500
      CALL DPFNMN(H1,ANGLE,H2,LEN,ITER,HMIN,PHI,IERROR)                 geoi1510
      GO TO 200                                                         geoi1520
 140  CONTINUE                                                          geoi1530
C***  CASE 2C:  H1, H2, RANGE                                           geoi1540
C     IF(NPR.NE.1)WRITE(IPR,19)                                         geoi1550
      IF(NPR.LT.1)WRITE(IPR,19)                                         geoi1560
 19   FORMAT(//,' CASE 2C: GIVEN H1, H2, RANGE',//,                     geoi1570
     1     10X,'NOTE: ANGLE IS COMPUTED FROM H1, H2, AND RANGE ',       geoi1580
     2     'ASSUMING NO REFRACTION')                                    geoi1590
      SMDTA = 0.000001
      IF(ABS(H1-H2).GT.RANGE+SMDTA) GO TO 9003                          geoi1600
      R1 = H1+RE                                                        geoi1610
      R2 = H2+RE                                                        geoi1620
C%    PHI = 180.0-ACOS((R2**2+RANGE**2-R1**2)/(2.0*R2*RANGE))*DEG       geoi1630
      ZARG2=(R1**2+RANGE**2-R2**2)/(2.0*R1*RANGE)                       geoi1640
      ZARG2 = ((R1+R2)*(R1-R2))/(2.0*R1*RANGE) + RANGE/(2.0*R1)         geoi1650
      ERARG2=ABS(ZARG2)-1.0                                             geoi1660
      IF((ERARG2.GT.1.0E-6).OR.(ERARG2.LT.0.0)) GO TO 187               geoi1670
      IF (ZARG2.LT.0.0) GO TO 188                                       geoi1680
      ZARG2=1.0                                                         geoi1690
      GO TO 187                                                         geoi1700
 188  ZARG2=-1.0                                                        geoi1710
 187  ANGLE = 180.0-ACOS(ZARG2)*DEG                                     geoi1720
      ZARG3=(R2**2+RANGE**2-R1**2)/(2.0*R2*RANGE)                       geoi1730
      ZARG3 = ((R2+R1)*(R2-R1))/(2.0*R2*RANGE) + RANGE/(2.0*R2)         geoi1740
      ERARG3=ABS(ZARG3)-1.0                                             geoi1750
      IF((ERARG3.GT.1.0E-6).OR.(ERARG3.LT.0.0)) GO TO 197               geoi1760
      IF (ZARG3.LT.0.0) GO TO 198                                       geoi1770
      ZARG3=1.0                                                         geoi1780
      GO TO 197                                                         geoi1790
 198  ZARG3=-1.0                                                        geoi1800
 197  PHI = 180.0-ACOS(ZARG3)*DEG                                       geoi1810
      BETA = PHI +ANGLE -180.                                           geoi1820
C#    RANGE = 0.                                                        geoi1830
      IF(RANGE .GE. 0.01 .AND. BETA .GT. 0)THEN                         geoi1840
         CALL FDBETA(H1,H2,BETA,ANGLE,PHI,LEN,HMIN,IERROR)              geoi1850
      ELSE                                                              geoi1860
CJ                                                                      geoi1870
         LEN = 0                                                        geoi1880
         IF(ANGLE.GT.90.0.AND.PHI.GT.90.0) LEN = 1                      geoi1890
         CALL DPFNMN(H1,ANGLE,H2,LEN,ITER,HMIN,PHI,IERROR)              geoi1900
      ENDIF                                                             geoi1910
 200  CONTINUE                                                          geoi1920
C***  TEST IERROR AND RECHECK LEN                                       geoi1930
C     IF(IERROR.NE.0)  RETURN                                           geoi1940
      IF(IERROR.NE.0)  GO TO 999                                        geoi1950
      LEN = 0                                                           geoi1960
cc    IF(HMIN.LT.MIN(H1,H2)) LEN = 1                                    geoi1970
      IF(abs(HMIN-MIN(H1,H2)).gt.0.00005) LEN = 1 
C***  REDUCE PATH END POINTS ABOVE ZMAX TO ZMAX                         geoi1980
      IF(HMIN.GE.ZMAX) GO TO 9008                                       geoi1990
      IF(H1.GT.ZMAX .OR. H2.GT.ZMAX)  CALL REDUCE(H1,H2,ANGLE,PHI,ITER) geoi2000
C***  AT THIS POINT THE FOLLOWING PARAMETERS ARE DEFINED:               geoi2010
C***  H1,H2,ANGLE,PHI,HMIN,LEN                                          geoi2020
C     IF(NPR.NE.1)WRITE(IPR,20)  H1,H2,ANGLE,PHI,HMIN,LEN               geoi2030
      IF(NPR.LT.1)WRITE(IPR,20)  H1,H2,ANGLE,PHI,HMIN,LEN               geoi2040
 20   FORMAT(///,' SLANT PATH PARAMETERS IN STANDARD FORM',//,          geoi2050
     1     10X,'H1      = ',F10.3,                                      geoi2060
     $     ' KM',/,10X,'H2      = ',F10.3,' KM',/,                      geoi2070
     2     10X,'ANGLE   = ',F10.3,                                      geoi2080
     $     ' DEG',/,10X,'PHI     = ',F10.3,' DEG',/,                    geoi2090
     3     10X,'HMIN    = ',F10.3,' KM',/,10X,'LEN     = ',I10)         geoi2100
      GO TO 999                                                         geoi2110
C***                                                                    geoi2120
C***  ERROR MESAGES                                                     geoi2130
C***                                                                    geoi2140
 9001 CONTINUE                                                          geoi2150
      WRITE(IPR,40) H1,HMIN                                             geoi2160
 40   FORMAT('0GEOINP, CASE 3B (H1,HMIN,SPACE): ERROR IN INPUT DATA',   geoi2170
     1     //,10X,'H1 = ',F12.6,'    IS LESS THAN HMIN = ',F12.6)       geoi2180
      GO TO 9900                                                        geoi2190
 9002 WRITE(IPR,42) ITYPE,ITYPE                                         geoi2200
 42   FORMAT('0GEOINP: ERROR IN INPUT DATA, ITYPE NOT EQUAL TO ',       geoi2210
     1     ' 2, OR 3.   ITYPE = ',I10,E23.14)                           geoi2220
      GO TO 9900                                                        geoi2230
 9003 WRITE(IPR,43) H1,H2,RANGE                                         geoi2240
 43   FORMAT('0GEOINP, CASE 2C (H1,H2,RANGE): ERROR IN INPUT DATA',//,  geoi2250
     1     10X,'ABS(H1-H2) GT RANGE;  H1 = ',F12.6,'    H2 = ',F12.6,   geoi2260
     2     '    RANGE = ',F12.6)                                        geoi2270
      GO TO 9900                                                        geoi2280
 9004 CONTINUE                                                          geoi2290
      WRITE(IPR,44)  H1,H2,ANGLE                                        geoi2300
 44   FORMAT('0GEOINP, CASE 2A (H1,H2,ANGLE): ERROR IN INPUT DATA',     geoi2310
     1     //,10X,'H1 = ',F12.6,                                        geoi2320
     $     '    IS GREATER THAN OR EQUAL TO H2 = ',                     geoi2330
     2     F12.6 ,/,10X,'AND ANGLE = ',F12.6,'    IS LESS THAN OR ',    geoi2340
     3     'EQUAL TO 90.0')                                             geoi2350
      GO TO 9900                                                        geoi2360
 9007 WRITE(IPR,48)                                                     geoi2370
 48   FORMAT('0GEOINP, ITYPE = 2: SLANT PATH INTERSECTS THE EARTH',     geoi2380
     1     ' OR GNDALT, AND CANNOT REACH H2')                           geoi2390
      GO TO 9900                                                        geoi2400
 9008 WRITE(IPR,50) ZMAX,H1,H2,HMIN                                     geoi2410
 50   FORMAT(' GEOINP-  THE ENTIRE PATH LIES ABOVE THE TOP ZMAX ',      geoi2420
     1     'OF THE ATMOSPHERIC PROFILE',//,10X,'ZMAX = ',G12.6,5X,      geoi2430
     2     '  H1 = ',G12.6,5X,'  H2 = ',G12.6,'  HMIN = ',G12.6)        geoi2440
 9900 IERROR = 1                                                        geoi2450
C                                                                       geoi2460
 999  SPH1 = H1                                                         geoi2470
      SPH2 = H2                                                         geoi2480
      SPANGL = ANGLE                                                    geoi2490
      SPRANG = RANGE                                                    geoi2500
      SPBETA = BETA                                                     geoi2510
      SPHMIN = HMIN                                                     geoi2520
      SPPHI = PHI                                                       geoi2530
C                                                                       geoi2540
      RETURN                                                            geoi2550
      END                                                               geoi2560
      FUNCTION   GMRAIN(FREQ,T,RATE)                                    gmrn 100
C                                                                       gmrn 110
C        COMPUTES ATTENUATION OF CONDENSED WATER IN FORM OF RAIN        gmrn 120
C                                                                       gmrn 130
C        FREQ = WAVENUMBER (CM-1)                                       gmrn 140
C        T    = TEMPERATURE (DEGREES KELVIN)                            gmrn 150
C        RATE = PRECIPITATION RATE (MM/HR)                              gmrn 160
C       WVLTH = WAVELENGTH IN CM                                        gmrn 170
C                                                                       gmrn 180
C     TABLES ATTAB AND FACTOR CALCULATED FROM FULL MIE THEORY           gmrn 190
C     UTILIZING MARSHALL-PALMER SIZE DISTRIBUTION WITH RAYS INDEX       gmrn 200
C     OF REFRACTION                                                     gmrn 210
C                                                                       gmrn 220
C     ATTAB IS ATTENUATION DATA TABLE IN NEPERS FOR 20 DEG CELSIUS      gmrn 230
C     WITH RADIATION FIELD REMOVED                                      gmrn 240
C                                                                       gmrn 250
C     WVNTBL IS WAVENUMBER TABLE FOR WAVENUMBERS USED IN TABLE ATTAB    gmrn 260
C     TMPTAB IS INTERPOLATION DATA TABLE FOR TEMPERATURES IN DEG KELVIN gmrn 270
C                                                                       gmrn 280
C     TLMDA IS INTERPOLATION DATA TABLE FOR WAVELENGTH IN CM            gmrn 290
C     TFREQ IS INTERPOLATION DATA TABLE FOR WAVENUMBER IN CM-1          gmrn 300
C                                                                       gmrn 310
C     RATTAB IS RAIN RATE TABLE IN MM/HR                                gmrn 320
C                                                                       gmrn 330
C     FACTOR IS TABLE OF TEMPERATURE CORRECTION FACTORS FOR             gmrn 340
C     TABLE ATTAB FOR REPRESENTATIVE RAINS WITHOUT RADIATION FIELD      gmrn 350
C                                                                       gmrn 360
C                                                                       gmrn 370
C     AITKEN INTERPOLATION SCHEME WRITTEN BY                            gmrn 380
C           E.T. FLORANCE O.N.R. PASADENA CA.                           gmrn 390
C                                                                       gmrn 400
C                                                                       gmrn 410
      DIMENSION ATTAB1(35),ATTAB2(35),ATTAB3(35),ATTAB4(35),ATTAB5(35)  gmrn 420
      DIMENSION ATTAB6(35),ATTAB7(35),ATTAB8(35),ATTAB9(35)             gmrn 430
      DIMENSION ATTAB(35,9),WVLTAB(27),RATTAB(9),FACTOR(5,8,5)          gmrn 440
      DIMENSION X(4),Y(4),ATTN(4),RATES(4)                              gmrn 450
CCC   DIMENSION X(3),Y(3),ATTN(3),RATES(3)                              gmrn 460
      DIMENSION TMPTAB(5),TLMDA(6),FACIT(5),TFACT(5)                    gmrn 470
      DIMENSION TFREQ(8),WVNTBL(35)                                     gmrn 480
      DIMENSION FACEQ1(5,8),FACEQ2(5,8),FACEQ3(5,8),FACEQ4(5,8)         gmrn 490
      DIMENSION FACEQ5(5,8)                                             gmrn 500
      EQUIVALENCE (ATTAB1(1),ATTAB(1,1)),(ATTAB2(1),ATTAB(1,2))         gmrn 510
      EQUIVALENCE (ATTAB3(1),ATTAB(1,3)),(ATTAB4(1),ATTAB(1,4))         gmrn 520
      EQUIVALENCE (ATTAB5(1),ATTAB(1,5)),(ATTAB6(1),ATTAB(1,6))         gmrn 530
      EQUIVALENCE (ATTAB7(1),ATTAB(1,7)),(ATTAB8(1),ATTAB(1,8))         gmrn 540
      EQUIVALENCE (ATTAB9(1),ATTAB(1,9))                                gmrn 550
      EQUIVALENCE (FACEQ1(1,1),FACTOR(1,1,1))                           gmrn 560
      EQUIVALENCE (FACEQ2(1,1),FACTOR(1,1,2))                           gmrn 570
      EQUIVALENCE (FACEQ3(1,1),FACTOR(1,1,3))                           gmrn 580
      EQUIVALENCE (FACEQ4(1,1),FACTOR(1,1,4))                           gmrn 590
      EQUIVALENCE (FACEQ5(1,1),FACTOR(1,1,5))                           gmrn 600
      DATA WVLTAB/.03,.033,.0375,.043,.05,.06,.075,.1,.15,.2,.25,.3,.5, gmrn 610
     1.8,1.,2.,3.,4.,5.,5.5,6.,6.5,7.,8.,9.,10.,15./                    gmrn 620
      DATA WVNTBL/ 0.0000,                                              gmrn 630
     1    .0667,.1000,.1111,.1250,.1429,.1538,                          gmrn 640
     2  .1667,.1818,.2000,.2500,.3333,.5000,1.0000,                     gmrn 650
     3 1.2500,2.0000,3.3333,4.0000,5.0000,6.6667,10.0000,               gmrn 660
     4 13.3333,16.6667,20.0000,23.2558,26.6667,30.3030,33.3333,         gmrn 670
     5 50.0,80.0,120.0,180.0,250.0,300.0,350.0/                         gmrn 680
      DATA RATTAB /.25,1.25,2.5,5.,12.5,25.,50.,100.,150./              gmrn 690
      DATA TLMDA/.03,.1,.5,1.25,3.2,10./                                gmrn 700
      DATA TFREQ/0.0,0.1,0.3125,0.8,2.0,10.0,33.3333,350.0/             gmrn 710
      DATA TMPTAB/273.15,283.15,293.15,303.15,313.15/                   gmrn 720
      DATA ATTAB1/                                                      gmrn 730
     1 1.272E+00,1.332E+00,1.361E+00,1.368E+00,1.393E+00,1.421E+00,     gmrn 740
     2 1.439E+00,1.466E+00,1.499E+00,1.541E+00,1.682E+00,1.951E+00,     gmrn 750
     3 2.571E+00,3.575E+00,3.808E+00,4.199E+00,3.665E+00,3.161E+00,     gmrn 760
     4 2.462E+00,1.632E+00,8.203E-01,4.747E-01,3.052E-01,2.113E-01,     gmrn 770
     5 1.551E-01,1.168E-01,8.958E-02,7.338E-02,3.174E-02,1.178E-02,     gmrn 780
     6 5.016E-03,2.116E-03,1.123E-03,8.113E-04,6.260E-04/               gmrn 790
      DATA ATTAB2/                                                      gmrn 800
     1 4.915E+00,5.257E+00,5.518E+00,5.632E+00,5.807E+00,6.069E+00,     gmrn 810
     2 6.224E+00,6.452E+00,6.756E+00,7.132E+00,8.453E+00,1.132E+01,     gmrn 820
     3 1.685E+01,2.177E+01,2.246E+01,2.156E+01,1.470E+01,1.167E+01,     gmrn 830
     4 8.333E+00,5.089E+00,2.356E+00,1.320E+00,8.315E-01,5.705E-01,     gmrn 840
     5 4.151E-01,3.119E-01,2.385E-01,1.955E-01,8.373E-02,3.138E-02,     gmrn 850
     6 1.351E-02,5.789E-03,3.090E-03,2.236E-03,1.725E-03/               gmrn 860
      DATA ATTAB3/                                                      gmrn 870
     1 8.798E+00,9.586E+00,1.023E+01,1.049E+01,1.093E+01,1.159E+01,     gmrn 880
     2 1.205E+01,1.263E+01,1.343E+01,1.450E+01,1.832E+01,2.627E+01,     gmrn 890
     3 3.904E+01,4.664E+01,4.702E+01,4.152E+01,2.542E+01,1.959E+01,     gmrn 900
     4 1.363E+01,8.087E+00,3.660E+00,2.028E+00,1.274E+00,8.710E-01,     gmrn 910
     5 6.340E-01,4.757E-01,3.634E-01,2.971E-01,1.275E-01,4.795E-02,     gmrn 920
     6 2.072E-02,8.936E-03,4.780E-03,3.460E-03,2.670E-03/               gmrn 930
      DATA ATTAB4/                                                      gmrn 940
     1 1.575E+01,1.750E+01,1.914E+01,1.991E+01,2.108E+01,2.276E+01,     gmrn 950
     2 2.399E+01,2.561E+01,2.785E+01,3.097E+01,4.204E+01,6.334E+01,     gmrn 960
     3 8.971E+01,9.853E+01,9.609E+01,7.718E+01,4.290E+01,3.220E+01,     gmrn 970
     4 2.188E+01,1.271E+01,5.641E+00,3.110E+00,1.947E+00,1.327E+00,     gmrn 980
     5 9.657E-01,7.242E-01,5.539E-01,4.528E-01,1.942E-01,7.335E-02,     gmrn 990
     6 3.181E-02,1.380E-02,7.394E-03,5.354E-03,4.132E-03/               gmrn1000
      DATA ATTAB5/                                                      gmrn1010
     1 3.400E+01,3.927E+01,4.523E+01,4.796E+01,5.207E+01,5.886E+01,     gmrn1020
     2 6.383E+01,7.060E+01,8.005E+01,9.360E+01,1.381E+02,2.069E+02,     gmrn1030
     3 2.620E+02,2.534E+02,2.366E+02,1.673E+02,8.285E+01,6.059E+01,     gmrn1040
     4 4.013E+01,2.280E+01,9.939E+00,5.439E+00,3.400E+00,2.315E+00,     gmrn1050
     5 1.685E+00,1.263E+00,9.664E-01,7.914E-01,3.397E-01,1.288E-01,     gmrn1060
     6 5.611E-02,2.450E-02,1.316E-02,9.536E-03,7.360E-03/               gmrn1070
      DATA ATTAB6/                                                      gmrn1080
     1 6.087E+01,7.347E+01,8.886E+01,9.653E+01,1.081E+02,1.283E+02,     gmrn1090
     2 1.435E+02,1.649E+02,1.947E+02,2.346E+02,3.543E+02,4.991E+02,     gmrn1100
     3 5.705E+02,5.048E+02,4.510E+02,2.900E+02,1.335E+02,9.607E+01,     gmrn1110
     4 6.269E+01,3.520E+01,1.519E+01,8.295E+00,5.182E+00,3.529E+00,     gmrn1120
     5 2.569E+00,1.927E+00,1.474E+00,1.208E+00,5.191E-01,1.975E-01,     gmrn1130
     6 8.627E-02,3.784E-02,2.037E-02,1.476E-02,1.139E-02/               gmrn1140
      DATA ATTAB7/                                                      gmrn1150
     1 1.090E+02,1.396E+02,1.811E+02,2.029E+02,2.396E+02,3.039E+02,     gmrn1160
     2 3.536E+02,4.189E+02,5.081E+02,6.217E+02,9.038E+02,1.165E+03,     gmrn1170
     3 1.212E+03,9.731E+02,8.330E+02,4.901E+02,2.123E+02,1.507E+02,     gmrn1180
     4 9.718E+01,5.408E+01,2.316E+01,1.264E+01,7.896E+00,5.377E+00,     gmrn1190
     5 3.915E+00,2.939E+00,2.249E+00,1.844E+00,7.940E-01,3.029E-01,     gmrn1200
     6 1.327E-01,5.846E-02,3.151E-02,2.284E-02,1.763E-02/               gmrn1210
      DATA ATTAB8/                                                      gmrn1220
     1 1.950E+02,2.703E+02,3.904E+02,4.614E+02,5.825E+02,7.909E+02,     gmrn1230
     2 9.475E+02,1.142E+03,1.380E+03,1.656E+03,2.237E+03,2.610E+03,     gmrn1240
     3 2.500E+03,1.820E+03,1.491E+03,8.103E+02,3.336E+02,2.344E+02,     gmrn1250
     4 1.495E+02,8.273E+01,3.524E+01,1.922E+01,1.203E+01,8.182E+00,     gmrn1260
     5 5.961E+00,4.477E+00,3.429E+00,2.812E+00,1.216E+00,4.651E-01,     gmrn1270
     6 2.043E-01,9.033E-02,4.874E-02,3.534E-02,2.728E-02/               gmrn1280
      DATA ATTAB9/                                                      gmrn1290
     1 2.742E+02,4.012E+02,6.353E+02,7.829E+02,1.027E+03,1.439E+03,     gmrn1300
     2 1.725E+03,2.071E+03,2.475E+03,2.909E+03,3.738E+03,4.104E+03,     gmrn1310
     3 3.776E+03,2.589E+03,2.070E+03,1.078E+03,4.326E+02,3.023E+02,     gmrn1320
     4 1.918E+02,1.059E+02,4.499E+01,2.454E+01,1.539E+01,1.045E+01,     gmrn1330
     5 7.615E+00,5.722E+00,4.384E+00,3.596E+00,1.561E+00,5.978E-01,     gmrn1340
     6 2.630E-01,1.165E-01,6.292E-02,4.562E-02,3.522E-02/               gmrn1350
      DATA FACEQ1/                                                      gmrn1360
     1 1.606,1.252,1.000, .816, .680,1.603,1.246,1.000, .817, .684,     gmrn1370
     2 1.444,1.207,1.000, .838, .694,1.016, .985,1.000,1.034,1.058,     gmrn1380
     3  .950, .976,1.000,1.034,1.068, .922, .956,1.000,1.044,1.090,     gmrn1390
     4  .932, .966,1.000,1.034,1.068, .957, .978,1.000,1.022,1.044/     gmrn1400
      DATA FACEQ2/                                                      gmrn1410
     1 1.606,1.252,1.000, .816, .680,1.612,1.256,1.000, .817, .684,     gmrn1420
     2 1.193,1.101,1.000, .889, .769, .885, .927,1.000,1.086,1.175,     gmrn1430
     3  .941, .976,1.000,1.024,1.047, .932, .966,1.000,1.034,1.079,     gmrn1440
     4  .932, .966,1.000,1.034,1.068, .957, .978,1.000,1.022,1.044/     gmrn1450
      DATA FACEQ3/                                                      gmrn1460
     1 1.606,1.252,1.000, .816, .680,1.621,1.256,1.000, .817, .673,     gmrn1470
     2  .969, .995,1.000, .982, .940, .895, .937,1.000,1.075,1.143,     gmrn1480
     3  .950, .976,1.000,1.024,1.036, .932, .966,1.000,1.034,1.079,     gmrn1490
     4  .932, .966,1.000,1.034,1.068, .957, .978,1.000,1.022,1.044/     gmrn1500
      DATA FACEQ4/                                                      gmrn1510
     1 1.606,1.252,1.000, .816, .680,1.631,1.265,1.000, .807, .662,     gmrn1520
     2  .848, .927,1.000,1.044,1.079, .922, .956,1.000,1.055,1.111,     gmrn1530
     3  .950, .976,1.000,1.013,1.036, .932, .966,1.000,1.034,1.079,     gmrn1540
     4  .932, .966,1.000,1.034,1.068, .957, .978,1.000,1.022,1.044/     gmrn1550
      DATA FACEQ5/                                                      gmrn1560
     1 1.606,1.252,1.000, .816, .680,1.603,1.265,1.000, .807, .662,     gmrn1570
     2  .820, .918,1.000,1.075,1.132, .941, .966,1.000,1.034,1.079,     gmrn1580
     3  .960, .976,1.000,1.013,1.036, .932, .966,1.000,1.034,1.079,     gmrn1590
     4  .932, .966,1.000,1.034,1.068, .957, .978,1.000,1.022,1.044/     gmrn1600
      DATA RATLIM /.05/                                                 gmrn1610
C         GIVE ZERO ATTN IF RATE FALLS BELOW LIMIT                      gmrn1620
      IF(RATE.GT.RATLIM) GO TO 12                                       gmrn1630
      GMRAIN = 0.                                                       gmrn1640
      RETURN                                                            gmrn1650
12    continue
cc 12 WVLTH =  1.0  /FREQ                                               gmrn1660
CCC   JMAX=3                                                            gmrn1670
      JMAX=4                                                            gmrn1680
CCC   IF(WVLTH.GT.WVLTAB(1)) GO TO      14                              gmrn1690
CCC   ILOW=0                                                            gmrn1700
CCC   JMAX=2                                                            gmrn1710
CCC   GO TO 18                                                          gmrn1720
CCC   THIS DO LOOP IS 2 LESS THAN NO. OF WVLTAB INPUT                   gmrn1730
CCC14 DO 15 I=2,25                                                      gmrn1740
14    DO 15 I=3,33                                                      gmrn1750
CCC   IF(WVLTH.LT.(.5*(WVLTAB(I)+WVLTAB(I+1)))) GO TO 16                gmrn1760
      IF(FREQ.LT.WVNTBL(I)) GO TO 16                                    gmrn1770
   15 CONTINUE                                                          gmrn1780
CCC   SET ILOW EQUAL TO 1 LESS THAN DO MAX                              gmrn1790
CCC   ILOW=24                                                           gmrn1800
      I=34                                                              gmrn1810
CCC   GO TO 18                                                          gmrn1820
CCC16 ILOW = I-2                                                        gmrn1830
16    ILOW=I-3                                                          gmrn1840
   18 CONTINUE                                                          gmrn1850
CCC   DO 190 I=2,7                                                      gmrn1860
      DO 190 K=3,7                                                      gmrn1870
CCC   IF (RATE. LT.(.5*(RATTAB(I)+RATTAB(I+1))))GO TO 195               gmrn1880
      IF(RATE.LT.RATTAB(K)) GO TO 195                                   gmrn1890
  190 CONTINUE                                                          gmrn1900
CCC   KMIN=6                                                            gmrn1910
      K=8                                                               gmrn1920
CCC   GO TO 198                                                         gmrn1930
CC195 KMIN=I-2                                                          gmrn1940
195   KMIN=K-3                                                          gmrn1950
  198 CONTINUE                                                          gmrn1960
      DO 20 J=1,JMAX                                                    gmrn1970
      IJ = ILOW + J                                                     gmrn1980
      X(J) =       WVNTBL(IJ)                                           gmrn1990
   20 CONTINUE                                                          gmrn2000
C        INTERPOLATE                                                    gmrn2010
CCC   Z = -ALOG(FREQ)                                                   gmrn2020
CCC   DO 25 K=1,3                                                       gmrn2030
      DO 25 K=1,4                                                       gmrn2040
      KJ=KMIN+K                                                         gmrn2050
      RATES(K)=RATTAB(KJ)                                               gmrn2060
      DO 24 J=1,JMAX                                                    gmrn2070
      IJ = ILOW + J                                                     gmrn2080
      Y(J)= ALOG(ATTAB(IJ,KJ))                                          gmrn2090
   24 CONTINUE                                                          gmrn2100
      ATTN(K)=EXP(AITK(X,Y,FREQ,JMAX) )                                 gmrn2110
   25 CONTINUE                                                          gmrn2120
C        APPLY TEMPERATURE CORRECTION                                   gmrn2130
      DO 31 I=2,5                                                       gmrn2140
      IF(T.LT.TMPTAB(I)) GO TO 33                                       gmrn2150
   31 CONTINUE                                                          gmrn2160
      ILOW = 4                                                          gmrn2170
      GO TO 35                                                          gmrn2180
   33 ILOW = I-1                                                        gmrn2190
   35 CONTINUE                                                          gmrn2200
      DO 41 J=2,8                                                       gmrn2210
      IF(FREQ.LT.TFREQ(J)) GO TO 43                                     gmrn2220
   41 CONTINUE                                                          gmrn2230
CCC   JLOW IS 2 LESS THAN DO MAX                                        gmrn2240
      JLOW=6                                                            gmrn2250
      GO TO 45                                                          gmrn2260
   43 JLOW = J-2                                                        gmrn2270
   45 CONTINUE                                                          gmrn2280
      DO 50 K=1,2                                                       gmrn2290
      DO 49 J=1,2                                                       gmrn2300
C        INTERPOLATE IN TEMPERATURE                                     gmrn2310
CCC   KJ=(KMIN/2)+K                                                     gmrn2320
      KJ=K+(KMIN+1)/2                                                   gmrn2330
      JI = JLOW + J                                                     gmrn2340
      FAC = ((TMPTAB(ILOW)-T)*FACTOR(ILOW+1,JI,KJ)+(T-TMPTAB(ILOW+1))*  gmrn2350
     1 FACTOR(ILOW,JI,KJ))/(TMPTAB(ILOW)-TMPTAB(ILOW+1))                gmrn2360
      JI = JLOW +3-J                                                    gmrn2370
      FACIT(J) = (TFREQ(JI)-FREQ )*FAC                                  gmrn2380
   49 CONTINUE                                                          gmrn2390
      TFACT(K) = (FACIT(2)-FACIT(1))/(TFREQ(JLOW+1)-TFREQ(JLOW+2))      gmrn2400
   50 CONTINUE                                                          gmrn2410
C        COMPUTE ATTENUATION (DB/KM)                                    gmrn2420
CCC   KJ=2*KMIN/2+1                                                     gmrn2430
      KJ=2*((KMIN+1)/2)+1                                               gmrn2440
CCC   GMRAIN=AITK(RATES,ATTN,RATE,3)*                                   gmrn2450
      GMRAIN=AITK(RATES,ATTN,RATE,4)*                                   gmrn2460
     1((RATE-RATTAB(KJ))*TFACT(2)+(RATTAB(KJ+2)-RATE)*                  gmrn2470
     2TFACT(1))/(RATTAB(KJ+2)-RATTAB(KJ))                               gmrn2480
CCC                                                                     gmrn2490
CCC    APPLY CONVERSION TO NEPERS                                       gmrn2500
CCC                                                                     gmrn2510
      RETURN                                                            gmrn2520
      END                                                               gmrn2530
      SUBROUTINE GETBMX(BMSTR,IBINX,IMOLX,SDZX,IALFX,ODZX)
      CHARACTER*129 BMSTR
      REAL SDZX(5), ODZX(5)
      INTEGER IBINX, IMOLX, IALFX
C     
      READ(BMSTR(1:5),'(I5)')IBINX
      READ(BMSTR(6:11),'(I6)')IMOLX
      READ(BMSTR(14:22),'(1PE9.2)')SDZX(1)
      READ(BMSTR(25:33),'(1PE9.2)')SDZX(2)
      READ(BMSTR(36:44),'(1PE9.2)')SDZX(3)
      READ(BMSTR(47:55),'(1PE9.2)')SDZX(4)
      READ(BMSTR(58:66),'(1PE9.2)')SDZX(5)
      READ(BMSTR(67:72),'(I6)')IALFX
      READ(BMSTR(75:83),'(1PE9.2)')ODZX(1)
      READ(BMSTR(86:94),'(1PE9.2)')ODZX(2)
      READ(BMSTR(97:105),'(1PE9.2)')ODZX(3)
      READ(BMSTR(108:116),'(1PE9.2)')ODZX(4)
      READ(BMSTR(119:127),'(1PE9.2)')ODZX(5)
      END
      SUBROUTINE HENGNS(ZZ,SANGLE,PHFA)                                 heng 100
C                                                                       heng 110
C    CALCULATES THE PHASE FUNCTION USING THE HENREY GREENSTEIN          heng 120
C    METHOD FOR CLOUDS, DESERT AERSOL, RAIN OR USER READ IN MODEL       heng 130
C                                                                       heng 140
      COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     heng 150
C                                                                       heng 160
      PFHG(GG,X)=(1.0-GG**2)/(4.*PI*(1.0+GG**2-2.0*GG*X)**1.5)          heng 170
      PHFA = PFHG(ZZ,SANGLE)                                            heng 180
      RETURN                                                            heng 190
      END                                                               heng 200
      SUBROUTINE HERTDA(HERZ,V)                                         hert 100
C                                                                       hert 110
C     HERZBERG O2 ABSORPTION                                            hert 120
C     HALL,1987 PRIVATE COMMUNICATION, BASED ON:                        hert 130
C                                                                       hert 140
C     REF. JOHNSTON ET.AL, JGR,89,11661-11665,1984                      hert 150
C         NICOLET, 1987 (RECENT STUDIES IN ATOMIC & MOLECULAR PROCESSES,hert 160
C                        PLEMUN PUBLISHING CORP, NY 1987)               hert 170
C     AND YOSHINO, ET.AL., 1988 (PREPRINT OF "IMPROVED ABSORPTION       hert 180
C         CROSS SECTIONS OF OXYGEN IN THE WAVELENGTH REGION 205-240NM   hert 190
C         OF THE HERZBERG CONTINUUM")                                   hert 200
C                                                                       hert 210
      COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     hert 220
C                                                                       hert 230
      HERZ=0.0                                                          hert 240
      IF(V.LE.36000.00) RETURN                                          hert 250
C                                                                       hert 260
C     EXTRAPOLATE SMOOTHLY THROUGH THE HERZBERG BAND REGION             hert 270
C     NOTE: HERZBERG BANDS ARE NOT CORRECTLY INCLUDED                   hert 280
C                                                                       hert 290
      CORR=0.                                                           hert 300
      IF(V.LE.40000.)CORR=((40000.-V)/4000.)*7.917E-27                  hert 310
C                                                                       hert 320
C     CONVERSION TO ATM-CM /KM                                          hert 330
C                                                                       hert 340
      RLOSCH = 2.6868 E24 * 1.0E-5                                      hert 350
C                                                                       hert 360
C     HALL'S NEW HERZBERG  (LEAST SQRS FIT, LN(P))                      hert 370
C                                                                       hert 380
C     YRATIO=2048.7/WL(I)  ****IN ANGSTOMS****                          hert 390
C           =.20487/WN(I)     IN MICRONS                                hert 400
C           =WCM(I)/48811.0   IN CM-1                                   hert 410
C                                                                       hert 420
      YRATIO= V    /48811.0                                             hert 430
      HERZ=6.884E-24*(YRATIO)*EXP(-69.738*(ALOG(YRATIO))**2)-CORR       hert 440
      HERZ = HERZ * RLOSCH                                              hert 450
      RETURN                                                            hert 460
      END                                                               hert 470
      SUBROUTINE HNO3 (V,HABS)                                          hno3 100
C                                                                       hno3 110
C     HNO3  STATISTICAL BAND PARAMETERS                                 hno3 120
C                                                                       hno3 130
      DIMENSION H1(15), H2(16), H3(13)                                  hno3 140
C     ARRAY H1 CONTAINS HNO3 ABS, COEF(CM-1ATM-1) FROM  850 TO 920 CM-1 hno3 150
      DATA H1/2.197,3.911,6.154,8.150,9.217,9.461,11.56,11.10,11.17,12.4hno3 160
     10,10.49,7.509,6.136,4.899,2.866/                                  hno3 170
C     ARRAY H2 CONTAINS HNO3 ABS, COEF(CM-1ATM-1) FROM 1275 TO1350 CM-1 hno3 180
      DATA H2/2.828,4.611,6.755,8.759,10.51,13.74,18.00,21.51,23.09,21.6hno3 190
     18,21.32,16.82,16.42,17.87,14.86,8.716/                            hno3 200
C     ARRAY H3 CONTAINS HNO3 ABS, COEF(CM-1ATM-1) FROM 1675 TO1735 CM-1 hno3 210
      DATA H3/5.003,8.803,14.12,19.83,23.31,23.58,23.22,21.09,26.99,25.8hno3 220
     14,24.79,17.68,9.420/                                              hno3 230
      HABS=0.                                                           hno3 240
      IF (V.GE.850.0.AND.V.LE.920.0) GO TO 5                            hno3 250
      IF (V.GE.1275.0.AND.V.LE.1350.0) GO TO 10                         hno3 260
      IF (V.GE.1675.0.AND.V.LE.1735.0) GO TO 15                         hno3 270
      RETURN                                                            hno3 280
    5 I=(V-845.)/5.                                                     hno3 290
      HABS=H1(I)                                                        hno3 300
      RETURN                                                            hno3 310
   10 I=(V-1270.)/5.                                                    hno3 320
      HABS=H2(I)                                                        hno3 330
      RETURN                                                            hno3 340
   15 I=(V-1670.)/5.                                                    hno3 350
      HABS=H3(I)                                                        hno3 360
      RETURN                                                            hno3 370
      END                                                               hno3 380
      SUBROUTINE INDX (WAVL,TC,KEY,REIL,AIMAG)                          indx 100
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * indx 110
C * *                                                                   indx 120
C * * WAVELENGTH IS IN CENTIMETERS.  TEMPERATURE IS IN DEG. C.      * * indx 130
C * *                                                                   indx 140
C * * KEY IS SET TO 1 IN SUBROUTINE GAMFOG                          * * indx 150
C * *                                                                   indx 160
C * * KEY IS SET TO 0 IN SUBROUTINE GAMFOG    FOR CIRRUS            * * indx 170
C * *                                                                   indx 180
C * * REAL IS THE REAL PART OF THE REFRACTIVE INDEX.                * * indx 190
C * *                                                                   indx 200
C * * AIMAG IS THE IMAGINARY PART OF THE REFRACTIVE INDEX IT IS     * * indx 210
C * *                                                                   indx 220
C * * RETURNED NEG. I.E.  M= REAL - I*AIMAG  .                      * * indx 230
C * *                                                                   indx 240
C * * A SERIES OF CHECKS ARE MADE AND WARNINGS GIVEN.               * * indx 250
C * *                                                                   indx 260
C * * RAY APPLIED OPTICS VOL 11,NO.8,AUG 72, PG. 1836-1844          * * indx 270
C * *                                                                   indx 280
C * * CORRECTIONS HAVE BEEN MADE TO RAYS ORIGINAL PAPER             * * indx 290
C * *                                                                   indx 300
C * *                                                                   indx 310
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * indx 320
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      R1=0.0                                                            indx 340
      R2=0.0                                                            indx 350
      IF(WAVL.LT..0001) WRITE(ipr,1)                                    indx 360
      IF(TC.LT.-20.) WRITE(ipr,2)                                       indx 370
    1 FORMAT(///,30X,'ATTEMPTING TO EVALUATE FOR A WAVELENGTH LESS THAN indx 380
     1ONE MICRON',//)                                                   indx 390
    2 FORMAT(///,30X,'ATTEMPTING TO EVALUATE FOR A TEMPERATURE LESS THANindx 400
     2 -20. DEGREES CENTIGRADE',//)                                     indx 410
      CALL DEBYE(WAVL,TC,KEY,REIL,AIMAG)                                indx 420
C * *  TABLE 3 WATER PG. 1840                                           indx 430
    4 IF(WAVL.GT..034) GO TO 5                                          indx 440
      GO TO 7                                                           indx 450
    5 IF(WAVL.GT..1) GO TO 11                                           indx 460
    6  R2 =DOP(WAVL,1.83899,1639.,52340.4,10399.2,588.24,345005.,       indx 470
     3259913.,161.29,43319.7,27661.2)                                   indx 480
      R2=R2+R2*(TC-25.)*.0001*EXP((.000025*WAVL)**.25)                  indx 490
      REIL=REIL*(WAVL-.034)/.066+R2*(.1-WAVL)/.066                      indx 500
      GO TO 11                                                          indx 510
    7 IF(WAVL.GT..0006) GO TO 8                                         indx 520
      GO TO 10                                                          indx 530
    8 REIL=DOP(WAVL,1.83899,1639.,52340.4,10399.2,588.24,345005.,       indx 540
     4259913.,161.29,43319.7,27661.2)                                   indx 550
      REIL=REIL+REIL*(TC-25.)*.0001*EXP((.000025*WAVL)**.25)            indx 560
      IF(WAVL.GT..0007) GO TO 11                                        indx 570
    9 R1=DOP(WAVL,1.79907,3352.27,99.914E+04,15.1963E+04,1639.,50483.5, indx 580
     59246.27,588.24,84.4697E+04,10.7615E+05)                           indx 590
      R1=R1+R1*(TC-25.)*.0001*EXP((.000025*WAVL)**.25)                  indx 600
      REIL=R1*(.0007-WAVL)/.0001+REIL*(WAVL-.0006)/.0001                indx 610
      GO TO 11                                                          indx 620
   10 REIL=DOP(WAVL,1.79907,3352.27,99.914E+04,15.1963E+04,1639.,       indx 630
     650483.5,9246.27,588.24,84.4697E+04,10.7615E+05)                   indx 640
      REIL=REIL+REIL*(TC-25.)*.0001*EXP((.000025*WAVL)**.25)            indx 650
C * *  TABLE 2 WATER PG. 1840                                           indx 660
   11 IF(WAVL.GE..3) GO TO 57                                           indx 670
      IF(WAVL.GE..03) GO TO 12                                          indx 680
      GO TO 13                                                          indx 690
   12 AIMAG=AIMAG+AB(WAVL,.25,300.,.47,3.)+AB(WAVL,.39,17.,.45,1.3)+    indx 700
     7AB(WAVL,.41,62.,.35,1.7)                                          indx 710
      GO TO 57                                                          indx 720
   13 IF(WAVL.GE..0062) GO TO 14                                        indx 730
      GO TO 15                                                          indx 740
   14 AIMAG=AIMAG+AB(WAVL,.41,62.,.35,1.7)+AB(WAVL,.39,17.,.45,1.3)+    indx 750
     8AB(WAVL,.25,300.,.4,2.)                                           indx 760
      GO TO 57                                                          indx 770
   15 IF(WAVL.GE..0017) GO TO 16                                        indx 780
      GO TO 17                                                          indx 790
   16 AIMAG=AIMAG+AB(WAVL,.39,17.,.45,1.3)+AB(WAVL,.41,62.,.22,1.8)+    indx 800
     9AB(WAVL,.25,300.,.4,2.)                                           indx 810
      GO TO 57                                                          indx 820
   17 IF(WAVL.GE..00061) GO TO 18                                       indx 830
      GO TO 19                                                          indx 840
   18 AIMAG=AIMAG+AB(WAVL,.12,6.1,.042,.6)+AB(WAVL,.39,17.,.165,2.4)+   indx 850
     1AB(WAVL,.41,62.,.22,1.8)                                          indx 860
      GO TO 57                                                          indx 870
   19 IF(WAVL.GE..000495) GO TO 20                                      indx 880
      GO TO 21                                                          indx 890
   20 AIMAG=AIMAG+AB(WAVL,.01,4.95,.05,1.)+AB(WAVL,.12,6.1,.009,2.)     indx 900
      GO TO 57                                                          indx 910
   21 IF(WAVL.GE..000297) GO TO 22                                      indx 920
      GO TO 23                                                          indx 930
   22 AIMAG=AIMAG+AB(WAVL,.27,2.97,.04,2.)+AB(WAVL,.01,4.95,.06,1.)     indx 940
      GO TO 57                                                          indx 950
   23 AIMAG=AIMAG+AB(WAVL,.27,2.97,.025,2.)+AB(WAVL,.01,4.95,.06,1.)    indx 960
   57 CONTINUE                                                          indx 970
      RETURN                                                            indx 980
      END                                                               indx 990
      SUBROUTINE INTERP(INTYPE,X,X1,X2,F,F1,F2)                         intp 100
C     SUBROUTINE INTERP INTERPOLATES TO DETERMINE THE VALUE OF F        intp 110
C     AT X, GIVEN F1 AT X1 AND F2 AT X2.                                intp 120
C     INTYPE=1 FOR LINEAR INTERPOLATION                                 intp 130
C     INTYPE=2 FOR LOGARITHMIC INTERPOLATION                            intp 140
      ITYPE=INTYPE                                                      intp 150
      IF(F1.LE.0.0.OR.F2.LE.0.0) ITYPE=1                                intp 160
      IF(ITYPE.EQ.2) GO TO 100                                          intp 170
C     LINEAR INTERPOLATION                                              intp 180
      F=F1+(X-X1)*(F2-F1)/(X2-X1)                                       intp 190
      RETURN                                                            intp 200
100   CONTINUE                                                          intp 210
      A1=ALOG(F1)                                                       intp 220
      A2=ALOG(F2)                                                       intp 230
      A=A1+(X-X1)*(A2-A1)/(X2-X1)                                       intp 240
      F=EXP(A)                                                          intp 250
      RETURN                                                            intp 260
      END                                                               intp 270
      SUBROUTINE IRFXN(X,RX,RATIO)                                      irfx 100
      include 'parameter.list'
C                                                                       irfx 110
C     THIS ROUTINE FINDS INDEX OF REFRACTION AND ITS DERIVATIVE AT HEIGHirfx 120
C                                                                       irfx 130
C     X = HEIGHT FROM THE SURFACE OF THE EARTH                          irfx 140
C     RX = INDEX OF REFRACTION                                          irfx 150
C     DRX = DERIVATIVE                                                  irfx 160
C     RATIO = RX/DRX                                                    irfx 170
C                                                                       irfx 180
      DOUBLE PRECISION  X, RX, RATIO, XN, H                             irfx 190
      INTEGER I, J, KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT                irfx 200
C                                                                       irfx 210
      COMMON /MODEL/ ZM(LAYDIM),PM(LAYDIM),TM(LAYDIM),RFNDX(LAYDIM),
     1  DENSTY(65,LAYDIM),CLDAMT(LAYDIM),RRAMT(LAYDIM),EQLWC(LAYDIM),
     1  HAZEC(LAYDIM)
      COMMON /CNTRL/ KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT               irfx 240
C                                                                       irfx 250
      IF (X .GT. ZM(ML)) THEN                                           irfx 260
         J = ML                                                         irfx 270
         GO TO 200                                                      irfx 280
      ENDIF                                                             irfx 290
      DO 300 I = 2, ML                                                  irfx 300
         IF (X .LE. ZM(I)) THEN                                         irfx 310
            J = I                                                       irfx 320
            GO TO 200                                                   irfx 330
         ENDIF                                                          irfx 340
 300  CONTINUE                                                          irfx 350
C                                                                       irfx 360
 200  I = J -1                                                          irfx 370
C                                                                       irfx 380
      H = -(ZM(J)-ZM(I))/LOG(RFNDX(J)/RFNDX(I))                         irfx 390
      XN = RFNDX(I)*EXP(-(X-ZM(I))/H)                                   irfx 400
      RX = 1 + XN                                                       irfx 410
      RATIO = -(RX*H)/XN                                                irfx 420
      RETURN                                                            irfx 430
      END                                                               irfx 440
      FUNCTION   JOU(CHAR)                                              irfx 450
      COMMON /IFIL/ IRD,IPR,IPU,NOPR,NFHDRF,iscrch                      irfx 460
C                                                                       irfx 470
      CHARACTER*1 CHAR,HOLVEC(22)                                       irfx 480
      DIMENSION INDX1(22)                                               irfx 490
      DATA  HOLVEC                                                      irfx 500
     X /'1','2','3','4','5','6','0','0','0','0',' ','A',                irfx 510
     X  'B','C','D','E','F','G','H','I','J','K'/                        irfx 520
      DATA  INDX1                                                       irfx 530
     X /  1,  2,  3,  4,  5,  6,  0,  0,  0,  0, 10, 10,                irfx 540
     X   11, 12, 13, 14, 15, 16, 17, 18, 19, 20/                        irfx 550
C                                                                       irfx 560
       INDX=0                                                           irfx 570
      DO 100 I=1,22                                                     irfx 580
       IF (HOLVEC(I) .NE. CHAR) GO TO 100                               irfx 590
       INDX=INDX1(I)                                                    irfx 600
       GO TO 110                                                        irfx 610
100   CONTINUE                                                          irfx 620
110   IF (INDX .EQ. 0) THEN                                             irfx 630
        WRITE(IPR,910) CHAR                                             irfx 640
910     FORMAT('0 INVALID PARAMETER :',2X,A1)                           irfx 650
        STOP ' JOU: BAD PARAM '                                         irfx 660
      END IF                                                            irfx 670
920   FORMAT(5X,A1,I5)                                                  irfx 680
      JOU=INDX                                                          irfx 690
                    RETURN                                              irfx 700
      END                                                               irfx 710
      SUBROUTINE LAYCLD(K,CLDATZ,RRATZ,ICLD1,GNDALT,RAINRT)             lay51550
      include 'parameter.list'
C                                                                       lay51560
C      THIS SUBROUTINE RESTRUCTURES THE ATMOSPHERIC PROFILE             lay51570
C      TO PROFIDE FINER LAYERING WITHIN THE FIRST 6 KM.                 lay51580
C                                                                       lay51590
C     ZMDL COMMON /MODEL/ FINAL ALTITUDE FOR LOWTRAN                    lay51600
C     ZK  EFFECTIVE CLOUD ALTITUDES                                     lay51610
C     ZCLD CLOUD ALTITUDE ARRAY                                         lay51620
C     ZDIF  ALT DIFF OF 2 LAYERS                                        lay51630
C     ZDA COMMON /MDATA/ CLD AND RAIN INFO IN THIS COMMON               lay51640
C                                                                       lay51650
      COMMON /MDATA/P(laydim),T(laydim),WH(laydim),WCO2(laydim),
     x WO(laydim),WN2O(laydim),WCO(laydim),WCH4(laydim),WO2(laydim)
      COMMON /MDATA1/ WNO(laydim),WSO2(laydim),WNO2(laydim),
     x WNH3(laydim),WAIR(laydim)     
      COMMON /CLDRR/  ZCLD(16),CLD(16,7),RR(16,7)    
      COMMON /MODEL/ZMdl(LAYDIM),PM(LAYDIM),TM(LAYDIM),RFNDX(LAYDIM),
     1  DENSTY(65,LAYDIM),CLDAMT(LAYDIM),RRAMT(LAYDIM),EQLWC(LAYDIM),
     1  HAZEC(LAYDIM)
      DATA CLDTP/6.0001/                                                lay51750
      DATA DELZ /0.02/                                                  lay51760
      ICLD=ICLD1                                                        lay51770
      IF(ICLD .EQ. 0) RETURN                                            lay51780
      IF(ICLD .GT. 11)RETURN                                            lay51790
      ZK=ZMDL(K)-GNDALT                                                 lay51800
      IF(ZK.LT.0.) ZK=0.                                                lay51810
      IF(ZMDL(K).GT.6.) ZK=ZMDL(K)                                      lay51820
      IF(ICLD.GT.5 )  GO TO 15                                          lay51830
CCC                                                                     lay51840
CCC    ICLD  IS  1- 5 ONE OF 5 SPECIFIC CLOUD MODELS IS CHOSEN          lay51850
CCC                                                                     lay51860
      MC=ICLD                                                           lay51870
      MR=6                                                              lay51880
      GO TO 25                                                          lay51890
15    CONTINUE                                                          lay51900
CCC                                                                     lay51910
CCC   ICLD  IS  6-10 ONE OF 5 SPECIFIC CLOUD/RAIN MODELS CHOSEN         lay51920
CCC                                                                     lay51930
      IF(ICLD .EQ. 6) MC=3                                              lay51940
      IF(ICLD .EQ. 7 .OR. ICLD .EQ. 8) MC=5                             lay51950
      IF(ICLD .GT. 8) MC=1                                              lay51960
      MR=ICLD-5                                                         lay51970
25    IF(  ZK.GT.CLDTP) GO TO 30                                        lay51980
      CLDATZ=0.                                                         lay51990
      RRATZ=0.                                                          lay52000
      IF(ZK.LE.10.)RRATZ=RAINRT                                         lay52010
      IF(MC.LT.1) GO TO 29                                              lay52020
      DO 26 MK=1,15                                                     lay52030
      IF(  ZK.GE.ZCLD(MK+1)) GO TO 26                                   lay52040
      IF(  ZK.LT.ZCLD(MK)) GO TO 26                                     lay52050
      IF(ABS(  ZK-ZCLD(MK)).LT.DELZ) GO TO 27                           lay52060
      GO TO 28                                                          lay52070
27     CLDATZ=CLD(MK,MC)                                                lay52080
       RRATZ=RR(MK,MR)                                                  lay52090
      GO TO 29                                                          lay52100
28    ZDIF=ZCLD(MK+1)-ZCLD(MK)                                          lay52110
      IF(ZDIF.LT.DELZ) GO TO 27                                         lay52120
      FAC=(ZCLD(MK+1)-  ZK)/ZDIF                                        lay52130
      CLDATZ=CLD(MK+1,MC)+FAC*(CLD(MK,MC)-CLD(MK+1,MC))                 lay52140
      RRATZ=RR(MK+1,MR)+FAC*(RR(MK,MR)-RR(MK+1,MR))                     lay52150
      GO TO 29                                                          lay52160
26    CONTINUE                                                          lay52170
29    CLDAMT(K)=CLDATZ                                                  lay52180
      CLD(K,7)=CLDATZ                                                   lay52190
      RR(K,7)=RRATZ                                                     lay52200
      RRAMT(K)=RRATZ                                                    lay52210
      RETURN                                                            lay52220
30    CONTINUE                                                          lay52230
      CLDAMT(K)=0.0                                                     lay52240
      RRAMT(K)=0.0                                                      lay52250
      CLDATZ=0.0                                                        lay52260
      RRATZ=0.0                                                         lay52270
      RETURN                                                            lay52280
C100  CONTINUE                                                          lay52290
C     RETURN                                                            lay52300
      END                                                               lay52310
      SUBROUTINE LAYVSA(K,RH,AHAZE,IHA1,ZNEW)                           layv 100
C                                                                       layv 110
C     RETURNS HAZE FOR VSA OPTION                                       layv 120
C                                                                       layv 130
      include 'parameter.list'
      COMMON /CNTRL/ KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT               layv 140
      COMMON /MODEL/   Z(LAYDIM),PM(LAYDIM),TM(LAYDIM),RFNDX(LAYDIM),
     1  DENSTY(65,LAYDIM),CLDAMT(LAYDIM),RRAMT(LAYDIM),EQLWC(LAYDIM),
     1  HAZEC(LAYDIM)
      COMMON /CARD1/ MODEL,ITYPE,IEMSCT,M1,M2,M3,IM,NOPRNT,TBOUND,SALB  layv 170
     1  ,MODTRN                                                         layv 180
      LOGICAL MODTRN                                                    layv 190
      COMMON /CARD2/ IHAZE,ISEASN,IVULCN,ICSTL,ICLD,IVSA,VIS,WSS,WHH,   layv 200
     1    RAINRT                                                        layv 210
      COMMON /MDATA/P(laydim),T(laydim),WH(laydim),WCO2(laydim),
     x WO(laydim),WN2O(laydim),WCO(laydim),WCH4(laydim),WO2(laydim)
      COMMON /MDATA1/ WNO(laydim),WSO2(laydim),WNO2(laydim),
     x WNH3(laydim),WAIR(laydim)    
      COMMON /ZVSALY/ ZVSA(10),RHVSA(10),AHVSA(10),IHVSA(10)            layv 260
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
C                                                                       layv 280
      DIMENSION ZNEW(laydim)                                            layv 290
      RH=0.                                                             layv 300
      AHAZE=0                                                           layv 310
      IHA1=0                                                            layv 320
C     HMXVSA=ZVSA(9)                                                    layv 330
      IF(MODEL.EQ.0.OR.MODEL.EQ.7) RETURN                               layv 340
      IF(K.GT.9) RETURN                                                 layv 350
      Z(K)=ZVSA(K)                                                      layv 360
      RH=RHVSA(K)                                                       layv 370
      AHAZE=AHVSA(K)                                                    layv 380
      IHA1=IHVSA(K)                                                     layv 390
      RETURN                                                            layv 400
      END                                                               layv 410
      SUBROUTINE MAPMS(ml,ikmax,imap)                                   loop3480
      include 'parameter.list'
      dimension  imap(laytwo) 
      COMMON /MODEL/ ZM(LAYDIM),PM(LAYDIM),TM(LAYDIM),RFNDX(LAYDIM),
     1  DENSTY(65,LAYDIM),CLDAMT(LAYDIM),RRAMT(LAYDIM),EQLWC(LAYDIM),
     1  HAZEC(LAYDIM)
      COMMON /CARD3/ H1,H2,ANGLE,RANGE,BETA,RE,LEN                      loop3520
      COMMON /PATH/PL(LAYTWO),QTHETA(LAYTWO),ITEST,HI,HF,
     1  AHT(LAYTWO),tph(LAYTWO)
      COMMON/MSRD/TLE(LAYDIM),COSBAR(LAYDIM),OMEGA0(LAYTWO),
     1  UPF(10,LAYDIM),DNF(10,LAYDIM),TAER(LAYDIM),ASYIK(LAYTWO),
     2  ASYDM(LAYTWO),STRN(0:LAYDIM),DMOLS(LAYTWO),DSTRN(0:LAYTWO),
     3  FDNSRT,FDNTRT,TAUT(LAYDIM),UMF(LAYDIM),DMF(LAYDIM),
     4  UMFS(LAYDIM),DMFS(LAYDIM) 
C                                                                       loop3570
C     ZM MODEL ALTITUDES                                                loop3580
C     AHT LINE OF SIGHT ALTITUDES                                       loop3590
C                                                                       loop3600
C     MULTISCATERING SET UP LOOKING UP                                  loop3610
C     GROUND TO SPACE AT 0 DEG                                          loop3620
c                                                                       loop3630
c     ioff is an offset equal to one for downward looking paths         loop3640
      IOFF=0                                                            loop3650
      IF(H1.GE.H2 .OR. ANGLE.GT.90.)IOFF=1                              loop3660
      DO 20 J=1,IKMAX                                                   loop3670
          imap(j)=1                                                     loop3680
          DO 10 I=2,ML                                                  loop3690
              IF(ZM(I).GT.AHT(J))GOTO20                                 loop3700
   10     IMAP(J)=I                                                     loop3710
   20 imap(j)=imap(j)-ioff                                              loop3720
C                                                                       loop3730
C     SET UP LAYER EDGE TEMPERATURES FOR USE IN MS FLUX ADDING          loop3740
      DO 30 IK=1,ML                                                     loop3750
   30 TLE(IK)=TM(ML-IK+1)                                               loop3760
      TLE(ML+1)=TLE(ML)                                                 loop3770
      RETURN                                                            loop3780
      END                                                               loop3790
      BLOCK DATA MARDTA                                                 mard 100
C>    BLOCK DATA                                                        mard 110
C                                                                       mard 120
C     MARINE AEROSOL EXTINCTION AND ABSORPTION DATA                     mard 130
C            CODED BY STU GATHMAN                  -  NRL               mard 140
C                                                                       mard 150
      COMMON/A/T1QEXT(40,4),T2QEXT(40,4),T3QEXT(40,4),                  mard 160
     1T1QABS(40,4),T2QABS(40,4),T3QABS(40,4),ALAM(40),AREL(4)           mard 170
      DIMENSION A1(40),A2(40),A3(40),A4(40)                             mard 180
      DIMENSION B1(40),B2(40),B3(40),B4(40)                             mard 190
      DIMENSION C1(40),C2(40),C3(40),C4(40)                             mard 200
      DIMENSION D1(40),D2(40),D3(40),D4(40)                             mard 210
      DIMENSION E1(40),E2(40),E3(40),E4(40)                             mard 220
      DIMENSION F1(40),F2(40),F3(40),F4(40)                             mard 230
      EQUIVALENCE (A1(1), T1QEXT(1,1)), (A2(1), T1QEXT(1,2)),           mard 240
     +            (A3(1), T1QEXT(1,3)), (A4(1), T1QEXT(1,4))            mard 250
      EQUIVALENCE (B1(1), T2QEXT(1,1)), (B2(1), T2QEXT(1,2)),           mard 260
     +            (B3(1), T2QEXT(1,3)), (B4(1), T2QEXT(1,4))            mard 270
      EQUIVALENCE (C1(1), T3QEXT(1,1)), (C2(1), T3QEXT(1,2)),           mard 280
     +            (C3(1), T3QEXT(1,3)), (C4(1), T3QEXT(1,4))            mard 290
      EQUIVALENCE (D1(1), T1QABS(1,1)), (D2(1), T1QABS(1,2)),           mard 300
     +            (D3(1), T1QABS(1,3)), (D4(1), T1QABS(1,4))            mard 310
      EQUIVALENCE (E1(1), T2QABS(1,1)), (E2(1), T2QABS(1,2)),           mard 320
     +            (E3(1), T2QABS(1,3)), (E4(1), T2QABS(1,4))            mard 330
      EQUIVALENCE (F1(1), T3QABS(1,1)), (F2(1), T3QABS(1,2)),           mard 340
     +            (F3(1), T3QABS(1,3)), (F4(1), T3QABS(1,4))            mard 350
      DATA AREL/50.,85.,95.,98./                                        mard 360
      DATA ALAM/                                                        mard 370
     * 0.2000,   0.3000,   0.3371,   0.5500,   0.6943,   1.0600,        mard 380
     * 1.5360,   2.0000,   2.2500,   2.5000,   2.7000,   3.0000,        mard 390
     * 3.3923,   3.7500,   4.5000,   5.0000,   5.5000,   6.0000,        mard 400
     * 6.2000,   6.5000,   7.2000,   7.9000,   8.2000,   8.7000,        mard 410
     * 9.0000,   9.2000,  10.0000,  10.5910,  11.0000,  11.5000,        mard 420
     *12.5000,  14.8000,  15.0000,  16.4000,  17.2000,  18.5000,        mard 430
     *21.3000,  25.0000,  30.0000,  40.0000/                            mard 440
      DATA A1/                                                          mard 450
     *-3.2949,  -3.4662,  -3.5275,  -3.8505,  -4.0388,  -4.4410,        mard 460
     *-4.8584,  -5.1720,  -5.3272,  -5.4342,  -5.2765,  -4.5101,        mard 470
     *-5.3730,  -5.7468,  -5.7579,  -5.8333,  -5.8552,  -5.1780,        mard 480
     *-5.2910,  -5.5959,  -5.6295,  -5.6748,  -5.6051,  -5.5363,        mard 490
     *-5.5330,  -5.5136,  -5.6568,  -5.6040,  -5.5221,  -5.3902,        mard 500
     *-5.1724,  -5.0903,  -5.0901,  -5.1285,  -5.1444,  -5.1963,        mard 510
     *-5.3101,  -5.3994,  -5.4873,  -5.4779/                            mard 520
      DATA A2/                                                          mard 530
     *-2.8302,  -2.9446,  -2.9904,  -3.2510,  -3.4104,  -3.7635,        mard 540
     *-4.1452,  -4.4466,  -4.6160,  -4.7772,  -4.7030,  -3.8461,        mard 550
     *-4.6466,  -5.0105,  -5.0747,  -5.1810,  -5.2705,  -4.5537,        mard 560
     *-4.6594,  -4.9872,  -5.0872,  -5.1229,  -5.0985,  -5.0623,        mard 570
     *-5.0544,  -5.0407,  -5.0793,  -4.9796,  -4.8748,  -4.7298,        mard 580
     *-4.5063,  -4.4260,  -4.4280,  -4.4650,  -4.4912,  -4.5474,        mard 590
     *-4.6672,  -4.7711,  -4.8814,  -4.9073/                            mard 600
      DATA A3/                                                          mard 610
     *-2.3712,  -2.4231,  -2.4512,  -2.6377,  -2.7631,  -3.0569,        mard 620
     *-3.3918,  -3.6682,  -3.8305,  -4.0111,  -4.0467,  -3.2055,        mard 630
     *-3.8717,  -4.1908,  -4.3282,  -4.4495,  -4.5780,  -3.9249,        mard 640
     *-4.0136,  -4.3349,  -4.4674,  -4.5088,  -4.5083,  -4.4973,        mard 650
     *-4.4923,  -4.4845,  -4.4753,  -4.3617,  -4.2509,  -4.1029,        mard 660
     *-3.8779,  -3.7963,  -3.7989,  -3.8345,  -3.8639,  -3.9215,        mard 670
     *-4.0438,  -4.1532,  -4.2719,  -4.3120/                            mard 680
      DATA A4/                                                          mard 690
     *-1.9911,  -1.9989,  -2.0126,  -2.1342,  -2.2283,  -2.4663,        mard 700
     *-2.7552,  -3.0036,  -3.1528,  -3.3328,  -3.4468,  -2.6649,        mard 710
     *-3.1986,  -3.4769,  -3.6571,  -3.7821,  -3.9284,  -3.3776,        mard 720
     *-3.4435,  -3.7436,  -3.8910,  -3.9455,  -3.9573,  -3.9633,        mard 730
     *-3.9639,  -3.9610,  -3.9427,  -3.8304,  -3.7203,  -3.5733,        mard 740
     *-3.3489,  -3.2650,  -3.2675,  -3.3017,  -3.3317,  -3.3893,        mard 750
     *-3.5126,  -3.6243,  -3.7467,  -3.7927/                            mard 760
      DATA B1/                                                          mard 770
     *-0.5781,  -0.5525,  -0.5484,  -0.5147,  -0.5094,  -0.5324,        mard 780
     *-0.6138,  -0.7139,  -0.7776,  -0.8624,  -0.9838,  -0.7720,        mard 790
     *-0.8542,  -0.9535,  -1.0873,  -1.1624,  -1.2647,  -1.2123,        mard 800
     *-1.1811,  -1.2905,  -1.4126,  -1.4643,  -1.5227,  -1.4560,        mard 810
     *-1.4177,  -1.4144,  -1.5746,  -1.6348,  -1.6431,  -1.6023,        mard 820
     *-1.4648,  -1.3910,  -1.3898,  -1.4056,  -1.4196,  -1.4655,        mard 830
     *-1.5795,  -1.6825,  -1.7924,  -1.8224/                            mard 840
      DATA B2/                                                          mard 850
     *-0.1809,  -0.1651,  -0.1566,  -0.1258,  -0.1113,  -0.1046,        mard 860
     *-0.1468,  -0.2157,  -0.2679,  -0.3480,  -0.4988,  -0.2657,        mard 870
     *-0.2991,  -0.3924,  -0.5266,  -0.5983,  -0.7037,  -0.6671,        mard 880
     *-0.6074,  -0.7134,  -0.8352,  -0.9080,  -0.9577,  -0.9579,        mard 890
     *-0.9542,  -0.9629,  -1.0867,  -1.1219,  -1.1032,  -1.0330,        mard 900
     *-0.8663,  -0.7677,  -0.7667,  -0.7768,  -0.7919,  -0.8304,        mard 910
     *-0.9354,  -1.0400,  -1.1640,  -1.2357/                            mard 920
      DATA B3/                                                          mard 930
     * 0.2483,   0.2574,   0.2626,   0.2887,   0.3055,   0.3312,        mard 940
     * 0.3262,   0.2922,   0.2589,   0.1989,   0.0548,   0.2322,        mard 950
     * 0.2487,   0.1816,   0.0685,   0.0090,  -0.0846,  -0.0876,        mard 960
     *-0.0110,  -0.0936,  -0.2013,  -0.2799,  -0.3216,  -0.3575,        mard 970
     *-0.3769,  -0.3944,  -0.5018,  -0.5379,  -0.5179,  -0.4473,        mard 980
     *-0.2822,  -0.1730,  -0.1713,  -0.1737,  -0.1850,  -0.2141,        mard 990
     *-0.3046,  -0.4002,  -0.5221,  -0.6163/                            mard1000
      DATA B4/                                                          mard1010
     * 0.6276,   0.6324,   0.6363,   0.6570,   0.6715,   0.7006,        mard1020
     * 0.7172,   0.7091,   0.6925,   0.6543,   0.5356,   0.6473,        mard1030
     * 0.6924,   0.6516,   0.5661,   0.5206,   0.4440,   0.4091,        mard1040
     * 0.4902,   0.4325,   0.3427,   0.2691,   0.2336,   0.1872,        mard1050
     * 0.1593,   0.1386,   0.0348,  -0.0131,  -0.0031,   0.0566,        mard1060
     * 0.2093,   0.3214,   0.3238,   0.3278,   0.3211,   0.3007,        mard1070
     * 0.2257,   0.1426,   0.0304,  -0.0739/                            mard1080
      DATA C1/                                                          mard1090
     * 2.1434,   2.1454,   2.1469,   2.1539,   2.1577,   2.1673,        mard1100
     * 2.1812,   2.1970,   2.2030,   2.2115,   2.2149,   2.1931,        mard1110
     * 2.2220,   2.2326,   2.2425,   2.2479,   2.2494,   2.2203,        mard1120
     * 2.2382,   2.2473,   2.2380,   2.2373,   2.2179,   2.2310,        mard1130
     * 2.2417,   2.2421,   2.2244,   2.1950,   2.1686,   2.1370,        mard1140
     * 2.1193,   2.1454,   2.1477,   2.1703,   2.1725,   2.1729,        mard1150
     * 2.1580,   2.1324,   2.0878,   2.0131/                            mard1160
      DATA C2/                                                          mard1170
     * 2.5480,   2.5512,   2.5511,   2.5562,   2.5601,   2.5669,        mard1180
     * 2.5792,   2.5874,   2.5950,   2.6022,   2.6081,   2.5875,        mard1190
     * 2.6093,   2.6184,   2.6319,   2.6391,   2.6439,   2.6138,        mard1200
     * 2.6319,   2.6437,   2.6442,   2.6421,   2.6336,   2.6336,        mard1210
     * 2.6353,   2.6325,   2.6075,   2.5680,   2.5340,   2.5025,        mard1220
     * 2.5122,   2.5652,   2.5681,   2.5869,   2.5925,   2.5986,        mard1230
     * 2.5947,   2.5835,   2.5566,   2.4949/                            mard1240
      DATA C3/                                                          mard1250
     * 2.9825,   2.9831,   2.9847,   2.9893,   2.9929,   2.9976,        mard1260
     * 3.0090,   3.0130,   3.0179,   3.0233,   3.0294,   3.0148,        mard1270
     * 3.0293,   3.0357,   3.0481,   3.0563,   3.0627,   3.0410,        mard1280
     * 3.0532,   3.0646,   3.0713,   3.0733,   3.0716,   3.0701,        mard1290
     * 3.0681,   3.0662,   3.0457,   3.0067,   2.9733,   2.9460,        mard1300
     * 2.9643,   3.0156,   3.0182,   3.0337,   3.0399,   3.0477,        mard1310
     * 3.0511,   3.0501,   3.0384,   2.9943/                            mard1320
      DATA C4/                                                          mard1330
     * 3.3635,   3.3621,   3.3652,   3.3699,   3.3729,   3.3768,        mard1340
     * 3.3868,   3.3888,   3.3916,   3.3952,   3.4000,   3.3911,        mard1350
     * 3.4013,   3.4056,   3.4152,   3.4218,   3.4280,   3.4148,        mard1360
     * 3.4222,   3.4312,   3.4393,   3.4442,   3.4452,   3.4463,        mard1370
     * 3.4455,   3.4452,   3.4329,   3.4016,   3.3719,   3.3468,        mard1380
     * 3.3617,   3.4046,   3.4068,   3.4198,   3.4255,   3.4334,        mard1390
     * 3.4402,   3.4447,   3.4428,   3.4144/                            mard1400
      DATA D1/                                                          mard1410
     *-7.7562,  -7.8498,  -7.8630,  -7.8493,  -7.7889,  -7.5044,        mard1420
     *-7.0058,  -6.3955,  -6.3210,  -6.0026,  -5.4176,  -4.5443,        mard1430
     *-5.6380,  -6.2635,  -5.9512,  -5.9860,  -5.9526,  -5.1907,        mard1440
     *-5.3115,  -5.6289,  -5.6502,  -5.6922,  -5.6157,  -5.5462,        mard1450
     *-5.5437,  -5.5234,  -5.6647,  -5.6087,  -5.5250,  -5.3918,        mard1460
     *-5.1733,  -5.0909,  -5.0907,  -5.1291,  -5.1450,  -5.1968,        mard1470
     *-5.3105,  -5.3997,  -5.4875,  -5.4779/                            mard1480
      DATA D2/                                                          mard1490
     *-7.5869,  -7.6977,  -7.7070,  -7.6883,  -7.6227,  -7.2788,        mard1500
     *-6.6637,  -5.9117,  -6.0351,  -5.6292,  -4.8814,  -3.8947,        mard1510
     *-5.0236,  -5.7607,  -5.3390,  -5.4052,  -5.4335,  -4.5711,        mard1520
     *-4.6910,  -5.0400,  -5.1263,  -5.1522,  -5.1200,  -5.0797,        mard1530
     *-5.0708,  -5.0554,  -5.0883,  -4.9842,  -4.8775,  -4.7313,        mard1540
     *-4.5074,  -4.4271,  -4.4290,  -4.4661,  -4.4923,  -4.5484,        mard1550
     *-4.6679,  -4.7716,  -4.8817,  -4.9075/                            mard1560
      DATA D3/                                                          mard1570
     *-7.3806,  -7.5324,  -7.5421,  -7.5190,  -7.4456,  -6.9683,        mard1580
     *-6.1934,  -5.3374,  -5.6261,  -5.1328,  -4.2936,  -3.2785,        mard1590
     *-4.3895,  -5.1770,  -4.7151,  -4.7944,  -4.8513,  -3.9542,        mard1600
     *-4.0698,  -4.4296,  -4.5444,  -4.5647,  -4.5533,  -4.5320,        mard1610
     *-4.5225,  -4.5111,  -4.4899,  -4.3685,  -4.2548,  -4.1053,        mard1620
     *-3.8800,  -3.7987,  -3.8013,  -3.8369,  -3.8663,  -3.9238,        mard1630
     *-4.0456,  -4.1545,  -4.2728,  -4.3123/                            mard1640
      DATA D4/                                                          mard1650
     *-7.1591,  -7.3911,  -7.3998,  -7.3737,  -7.2891,  -6.6133,        mard1660
     *-5.7137,  -4.8091,  -5.1828,  -4.6408,  -3.7712,  -2.7644,        mard1670
     *-3.8361,  -4.6426,  -4.1724,  -4.2573,  -4.3263,  -3.4249,        mard1680
     *-3.5341,  -3.8962,  -4.0222,  -4.0421,  -4.0386,  -4.0258,        mard1690
     *-4.0169,  -4.0077,  -3.9676,  -3.8419,  -3.7270,  -3.5776,        mard1700
     *-3.3529,  -3.2698,  -3.2724,  -3.3066,  -3.3365,  -3.3940,        mard1710
     *-3.5164,  -3.6272,  -3.7486,  -3.7935/                            mard1720
      DATA E1/                                                          mard1730
     *-4.1531,  -4.2017,  -4.0836,  -4.1441,  -4.0515,  -3.7234,        mard1740
     *-3.2022,  -2.5924,  -2.5215,  -2.2244,  -1.7099,  -1.0243,        mard1750
     *-1.8178,  -2.4304,  -2.1483,  -2.1897,  -2.1768,  -1.5025,        mard1760
     *-1.5770,  -1.8688,  -1.9132,  -1.9550,  -1.9023,  -1.8200,        mard1770
     *-1.8019,  -1.7822,  -1.9415,  -1.9082,  -1.8419,  -1.7290,        mard1780
     *-1.5359,  -1.4523,  -1.4511,  -1.4744,  -1.4875,  -1.5339,        mard1790
     *-1.6446,  -1.7377,  -1.8338,  -1.8404/                            mard1800
      DATA E2/                                                          mard1810
     *-4.0237,  -4.0786,  -4.0596,  -4.0117,  -3.9167,  -3.5334,        mard1820
     *-2.8890,  -2.1314,  -2.2533,  -1.8686,  -1.2114,  -0.5112,        mard1830
     *-1.2226,  -1.9313,  -1.5503,  -1.6190,  -1.6646,  -0.9328,        mard1840
     *-0.9892,  -1.2921,  -1.3909,  -1.4236,  -1.4060,  -1.3666,        mard1850
     *-1.3550,  -1.3429,  -1.3966,  -1.3198,  -1.2346,  -1.1147,        mard1860
     *-0.9248,  -0.8332,  -0.8328,  -0.8490,  -0.8658,  -0.9072,        mard1870
     *-1.0110,  -1.1088,  -1.2210,  -1.2642/                            mard1880
      DATA E3/                                                          mard1890
     *-3.8225,  -3.9189,  -3.8934,  -3.8788,  -3.7792,  -3.2584,        mard1900
     *-2.4500,  -1.5859,  -1.8664,  -1.3920,  -0.6602,  -0.0250,        mard1910
     *-0.6305,  -1.3614,  -0.9442,  -1.0200,  -1.0892,  -0.3681,        mard1920
     *-0.4088,  -0.6976,  -0.8140,  -0.8430,  -0.8410,  -0.8268,        mard1930
     *-0.8209,  -0.8142,  -0.8176,  -0.7305,  -0.6447,  -0.5305,        mard1940
     *-0.3534,  -0.2582,  -0.2574,  -0.2661,  -0.2802,  -0.3137,        mard1950
     *-0.4046,  -0.4954,  -0.6063,  -0.6635/                            mard1960
      DATA E4/                                                          mard1970
     *-3.6380,  -3.8218,  -3.8158,  -3.6544,  -3.6442,  -2.9366,        mard1980
     *-1.9981,  -1.0852,  -1.4468,  -0.9222,  -0.1746,   0.3789,        mard1990
     *-0.1326,  -0.8516,  -0.4270,  -0.5021,  -0.5774,   0.1072,        mard2000
     * 0.0779,  -0.1890,  -0.3060,  -0.3330,  -0.3362,  -0.3320,        mard2010
     *-0.3290,  -0.3248,  -0.3123,  -0.2275,  -0.1469,  -0.0421,        mard2020
     * 0.1192,   0.2136,   0.2149,   0.2122,   0.2019,   0.1760,        mard2030
     * 0.0989,   0.0190,  -0.0836,  -0.1437/                            mard2040
      DATA F1/                                                          mard2050
     *-0.5486,  -0.6082,  -0.5956,  -0.5356,  -0.4402,  -0.0871,        mard2060
     * 0.4527,   1.0366,   1.1096,   1.3655,   1.7101,   1.8903,        mard2070
     * 1.6543,   1.2291,   1.4722,   1.4553,   1.4742,   1.8427,        mard2080
     * 1.8260,   1.6925,   1.6714,   1.6561,   1.6818,   1.7408,        mard2090
     * 1.7604,   1.7735,   1.6870,   1.6975,   1.7266,   1.7732,        mard2100
     * 1.8476,   1.8953,   1.8977,   1.9100,   1.9121,   1.9074,        mard2110
     * 1.8820,   1.8553,   1.8167,   1.8034/                            mard2120
      DATA F2/                                                          mard2130
     *-0.4081,  -0.4784,  -0.4660,  -0.4117,  -0.3046,   0.0831,        mard2140
     * 0.7409,   1.4609,   1.3780,   1.7134,   2.1471,   2.2808,        mard2150
     * 2.1315,   1.6742,   1.9804,   1.9449,   1.9238,   2.2748,        mard2160
     * 2.2689,   2.1587,   2.1154,   2.1037,   2.1124,   2.1387,        mard2170
     * 2.1490,   2.1552,   2.1238,   2.1535,   2.1840,   2.2226,        mard2180
     * 2.2790,   2.3247,   2.3268,   2.3387,   2.3422,   2.3439,        mard2190
     * 2.3339,   2.3198,   2.2926,   2.2751/                            mard2200
      DATA F3/                                                          mard2210
     *-0.2242,  -0.3289,  -0.3406,  -0.2786,  -0.1532,   0.3414,        mard2220
     * 1.1618,   1.9783,   1.7412,   2.1629,   2.6182,   2.6999,        mard2230
     * 2.6101,   2.1844,   2.4931,   2.4589,   2.4253,   2.7204,        mard2240
     * 2.7182,   2.6391,   2.5975,   2.5896,   2.5918,   2.6017,        mard2250
     * 2.6055,   2.6086,   2.6049,   2.6334,   2.6574,   2.6843,        mard2260
     * 2.7225,   2.7601,   2.7620,   2.7734,   2.7780,   2.7832,        mard2270
     * 2.7839,   2.7809,   2.7677,   2.7571/                            mard2280
      DATA F4/                                                          mard2290
     *-0.0119,  -0.2110,  -0.2063,  -0.1444,  -0.0667,   0.6542,        mard2300
     * 1.5923,   2.4405,   2.1326,   2.5924,   3.0247,   3.0696,        mard2310
     * 3.0154,   2.6365,   2.9257,   2.8957,   2.8634,   3.1026,        mard2320
     * 3.1009,   3.0465,   3.0135,   3.0090,   3.0097,   3.0147,        mard2330
     * 3.0167,   3.0193,   3.0233,   3.0464,   3.0635,   3.0808,        mard2340
     * 3.1047,   3.1337,   3.1354,   3.1458,   3.1506,   3.1572,        mard2350
     * 3.1639,   3.1680,   3.1651,   3.1624/                            mard2360
      END                                                               mard2370
      SUBROUTINE MARINE(VIS,MODEL,WS,WH,ICSTL,BEXT,BABS,NL)             mari 100
C                                                                       mari 110
C        THIS SUBROUTINE DETERMINES AEROSOL EXT + ABS COEFFICIENTS      mari 120
C          FOR THE NAVY MARITIME MODEL                                  mari 130
C            CODED BY STU GATHMAN                  -  NRL               mari 140
C                                                                       mari 150
C        INPUTS-                                                        mari 160
C        WSS = CURRENT WIND SPEED (M/S)                                 mari 170
C        WHH = 24 HOUR AVERAGE WIND SPEED (M/S)                         mari 180
C        RHH = RELATIVE HUMIDITY (PERCENTAGE)                           mari 190
C        VIS = METEOROLOGICAL RANGE (KM)                                mari 200
C        ICTL = AIR MASS CHARACTER  1 = OPEN OCEAN                      mari 210
C                      10 = STRONG CONTINENTAL INFLUENCE                mari 220
C        MODEL = MODEL ATMOSPHERE                                       mari 230
C                                                                       mari 240
C        OUTPUTS-                                                       mari 250
C        BEXT = EXTINCTION COEFFICIENT (KM-1)                           mari 260
C        BABS = ABSORPTION COEFFICIENT (KM-1)                           mari 270
C                                                                       mari 280
      COMMON /MART/ RHH                                                 mari 290
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     mari 310
      COMMON/A/T1QEXT(40,4),T2QEXT(40,4),T3QEXT(40,4),                  mari 320
     1T1QABS(40,4),T2QABS(40,4),T3QABS(40,4),ALAM(40),AREL(4)           mari 330
      DIMENSION WSPD(8), BEXT(5,47), BABS(5,47)                         mari 350
      DIMENSION RHD(8)                                                  mari 360
      DATA WSPD/6.9, 4.1, 4.1, 10.29, 6.69, 12.35, 7.2, 6.9/            mari 370
      DATA RHD/80., 75.63, 76.2, 77.13, 75.24, 80.53, 45.89, 80./       mari 380
      PISC = PI/1000.0                                                  mari 390
      WRITE(IPR,890)                                                    mari 400
C                                                                       mari 410
C     CHECK LIMITS OF MODEL VALIDITY                                    mari 420
C                                                                       mari 430
      RH = RHH                                                          mari 440
      IF(RHH.GT.0.) GO TO 10                                            mari 450
      RH=RHD(MODEL+1)                                                   mari 460
10    IF(WS.GT.20.0) WS=20.                                             mari 470
      IF(WH.GT.20.0) WH = 20.                                           mari 480
      IF(RH.GT.98.0) RH = 98.                                           mari 490
      IF(RH.LT.50.0.AND.RH.GE.0.0) RH = 50.                             mari 500
      IF(ICSTL.LT.1.OR.ICSTL.GT.10) ICSTL = 3                           mari 510
C                                                                       mari 520
C     FIND SIZE DISTRIBUTION PARAMETERS FROM METEOROLOGY INPUT          mari 530
C                                                                       mari 540
      IF(WH.LE.0.) WRITE(IPR,920)                                       mari 550
      IF(WH .LE. 0.0) WH = WSPD(MODEL + 1)                              mari 560
      IF(WS.LE.0.) WRITE(IPR,930)                                       mari 570
      IF(WS.LE.0.0)WS=WH                                                mari 580
      WRITE(IPR,910)WS,WH,RH,ICSTL                                      mari 590
C                                                                       mari 600
C        F IS A RELATIVE HUMIDITY DEPENDENT GROWTH CORRECTION           mari 610
C        TO THE ATTENUATION COEFFICIENT.                                mari 620
C                                                                       mari 630
      F=((2.-RH/100.)/(6.*(1.-RH/100.)))**0.33333                       mari 640
      A1=2000.0*ICSTL*ICSTL                                             mari 650
      A2 = AMAX1(5.866*(WH-2.2), 0.5)                                   mari 660
CCC   A3 = AMAX1(0.01527*(WS-2.2), 1.14E-5)                             mari 670
      A3 = 10**(0.06*WS-2.8)                                            mari 680
C                                                                       mari 690
C     FIND EXTINCTION AT 0.55 MICRONS AND NORMALIZE TO 1.               mari 700
C                                                                       mari 710
C     INTERPOLATE FOR RELATIVE HUMIDITY                                 mari 720
C                                                                       mari 730
      DO 40   J=2,4                                                     mari 740
      IF(RH.LE.AREL(J)) GO TO 42                                        mari 750
 40   CONTINUE                                                          mari 760
 42   DELRH=AREL(J)-AREL(J-1)                                           mari 770
      DELRHV=RH-AREL(J-1)                                               mari 780
      RATIO=DELRHV/DELRH                                                mari 790
      QE1=T1QEXT(4,J-1)+(T1QEXT(4,J)-T1QEXT(4,J-1))*RATIO               mari 800
      QE2=T2QEXT(4,J-1)+(T2QEXT(4,J)-T2QEXT(4,J-1))*RATIO               mari 810
      QE3=T3QEXT(4,J-1)+(T3QEXT(4,J)-T3QEXT(4,J-1))*RATIO               mari 820
      TOTAL = A1*10.**QE1 + A2*10.**QE2 + A3*10.**QE3                   mari 830
      EXT55=PISC*TOTAL/F                                                mari 840
C                                                                       mari 850
C     IF METEOROLOLICAL RANGE NOT SPECIFIED,FIND FROM METEOR DATA       mari 860
C                                                                       mari 870
      IF(VIS.LE.0.) VIS=3.912/(EXT55+0.01159)                           mari 880
      C=(1./EXT55)*(PISC/F)                                             mari 890
      A1=C*A1                                                           mari 900
      A2=C*A2                                                           mari 910
      A3=C*A3                                                           mari 920
C                                                                       mari 930
C     CALCULATE NORMALIZED ATTENUATION COEFICIENTS                      mari 940
C                                                                       mari 950
      DO 45   I=1,40                                                    mari 960
      T1XV = T1QEXT(I,J-1) + (T1QEXT(I,J) - T1QEXT(I,J-1))*RATIO        mari 970
      T2XV = T2QEXT(I,J-1) + (T2QEXT(I,J) - T2QEXT(I,J-1))*RATIO        mari 980
      T3XV = T3QEXT(I,J-1) + (T3QEXT(I,J) - T3QEXT(I,J-1))*RATIO        mari 990
      T1AV = T1QABS(I,J-1) + (T1QABS(I,J) - T1QABS(I,J-1))*RATIO        mari1000
      T2AV = T2QABS(I,J-1) + (T2QABS(I,J) - T2QABS(I,J-1))*RATIO        mari1010
      T3AV = T3QABS(I,J-1) + (T3QABS(I,J) - T3QABS(I,J-1))*RATIO        mari1020
      BEXT(NL,I)=A1*10**(T1XV)+A2*10**(T2XV)+A3*10**(T3XV)              mari1030
      BABS(NL,I)=A1*10**(T1AV)+A2*10**(T2AV)+A3*10**(T3AV)              mari1040
 45   CONTINUE                                                          mari1050
      WRITE(IPR,900) VIS                                                mari1060
      RETURN                                                            mari1070
890   FORMAT('0MARINE AEROSOL MODEL USED')                              mari1080
900   FORMAT('0',T10,'VIS = ',F10.2,' KM')                              mari1090
910   FORMAT(T10,'WIND SPEED = ',F8.2,' M/SEC',/,T10,                   mari1100
     1 'WIND SPEED (24 HR AVERAGE) = ',F8.2,' M/SEC',/,                 mari1110
     2 T10,'RELATIVE HUMIDITY = ',F8.2,' PERCENT',/,                    mari1120
     3 T10,'AIRMASS CHARACTER =' ,I3)                                   mari1130
920   FORMAT('0  WS NOT SPECIFIED, A DEFAULT VALUE IS USED')            mari1140
930   FORMAT('0  WH NOT SPECIFIED, A DEFAULT VALUE IS USED')            mari1150
      END                                                               mari1160
      BLOCK DATA MDTA                                                   mdta 100
C>    BLOCK DATA                                                        mdta 110
C                                                                       mdta 120
C     CLOUD AND RAIN   DATA                                             mdta 130
C                                                                       mdta 140
      COMMON /cldrr/ zcld(16),
     x CLD1(16),CLD2(16),CLD3(16),CLD4(16),CLD5(16),CLD6(16),CLD7(16)   mdta 200
     X ,RR1(16), RR2(16), RR3(16), RR4(16), RR5(16), RR6(16), RR7(16)   mdta 210
CCC   CLOUD MODELS 1-5                                                  mdta 340
      DATA ZCLD/ 0.0, 0.16, 0.33, 0.66,  1.0,  1.5,  2.0,  2.4,         CDT  280
     &           2.7,  3.0,  3.5,  4.0,  4.5,  5.0,  5.5,  6.0/         CDT  290
      DATA CLD1/ 0.0,0.0,0.0,0.2,0.35,1.0,1.0,1.0,0.3,0.15, 6*0.0/      mdta 350
      DATA CLD2/ 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.3,0.4,0.3, 6*0.0/        mdta 360
      DATA CLD3/ 0.0,0.0,0.15,0.30,0.15,11*0.0/                         mdta 370
      DATA CLD4/ 0.0,0.0,0.0,0.10,0.15,0.15,0.10, 9*0.0/                mdta 380
      DATA CLD5/ 0.0,0.30,0.65,0.40,12*0.0/                             mdta 390
      DATA CLD6/ 16*0.0/                                                mdta 400
      DATA CLD7/ 16*0.0/                                                mdta 410
CCC   RAIN MODELS 1-5                                                   mdta 420
      DATA RR1/ 2.0,1.78,1.43,1.22,0.86,0.22,10*0.0/                    mdta 430
      DATA RR2/ 5.0,4.0,3.4,2.6,0.8,0.2,10*0.0/                         mdta 440
      DATA RR3/ 12.5,10.5,8.0,6.0,2.5,0.8,0.2, 9*0.0/                   mdta 450
      DATA RR4/ 25.0,21.5,17.5,12.0,7.5,4.2,2.5,1.0,0.7,0.2, 6*0.0/     mdta 460
      DATA RR5/ 75.0,70.0,65.0,60.0,45.0,20.0,12.5,7.0,3.5,             mdta 470
     + 1.0,0.2, 5*0.0/                                                  mdta 480
      DATA RR6/ 16*0.0/                                                 mdta 490
      DATA RR7/ 16*0.0/                                                 mdta 500
C     DATA CO2       /                                                  mdta 510
      END                                                               mdta 520
      BLOCK DATA MLATMB                                                 mlat 100
C>    BLOCK DATA                                                        mlat 110
C***********************************************************************mlat 120
C     THIS SUBROUTINE INITIALIZES THE 6 BUILT-IN ATMOSPHERIC PROFILES   mlat 130
C     (FROM 'OPTICAL PROPERTIES OF THE ATMOSPHERE, THIRD EDITION'       mlat 140
C     AFCRL-72-0497 (AD 753 075), 'U.S. STANDARD ATMOSPHERE 1976' AND   mlat 150
C     'SUPPLEMENTS 1966'), PLUS COLLECTED CONSTITUENT PROFILES (REF)    mlat 160
C     AND SETS OTHER CONSTANTS RELATED TO THE ATMOSPHERIC PROFILES      mlat 170
C***********************************************************************mlat 180
      COMMON /MLATM/  ALT(50),P1(50),P2(50),P3(50),P4(50),P5(50),P6(50),mlat 190
     +T1(50),T2(50),T3(50),T4(50),T5(50),T6(50),                        mlat 200
     +AMOL11(50),AMOL12(50),AMOL13(50),AMOL14(50),AMOL15(50),AMOL16(50),mlat 210
     +AMOL17(50),AMOL18(50),                                            mlat 220
     +AMOL21(50),AMOL22(50),AMOL23(50),AMOL24(50),AMOL25(50),AMOL26(50),mlat 230
     +AMOL27(50),AMOL28(50),                                            mlat 240
     +AMOL31(50),AMOL32(50),AMOL33(50),AMOL34(50),AMOL35(50),AMOL36(50),mlat 250
     +AMOL37(50),AMOL38(50),                                            mlat 260
     +AMOL41(50),AMOL42(50),AMOL43(50),AMOL44(50),AMOL45(50),AMOL46(50),mlat 270
     +AMOL47(50),AMOL48(50),                                            mlat 280
     +AMOL51(50),AMOL52(50),AMOL53(50),AMOL54(50),AMOL55(50),AMOL56(50),mlat 290
     +AMOL57(50),AMOL58(50),                                            mlat 300
     +AMOL61(50),AMOL62(50),AMOL63(50),AMOL64(50),AMOL65(50),AMOL66(50),mlat 310
     +AMOL67(50),AMOL68(50)                                             mlat 320
C     COMMON /TRAC/ TRAC(50,21)                                         mlat 330
      COMMON /TRAC/ ANO(50),SO2(50),ANO2(50),ANH3(50),HNO3(50),OH(50),  mlat 340
     X HF(50),HCL(50),HBR(50),HI(50),CLO(50),OCS(50),H2CO(50),          mlat 350
     X HOCL(50),AN2(50),HCN(50),CH3CL(50),H2O2(50),C2H2(50),            mlat 360
     X C2H6(50),PH3(50)                                                 mlat 370
C     DATA ALT (KM)  /                                                  mlat 380
      DATA ALT/                                                         mlat 390
     C       0.0,       1.0,       2.0,       3.0,       4.0,           mlat 400
     C       5.0,       6.0,       7.0,       8.0,       9.0,           mlat 410
     C      10.0,      11.0,      12.0,      13.0,      14.0,           mlat 420
     C      15.0,      16.0,      17.0,      18.0,      19.0,           mlat 430
     C      20.0,      21.0,      22.0,      23.0,      24.0,           mlat 440
     C      25.0,      27.5,      30.0,      32.5,      35.0,           mlat 450
     C      37.5,      40.0,      42.5,      45.0,      47.5,           mlat 460
     C      50.0,      55.0,      60.0,      65.0,      70.0,           mlat 470
     C      75.0,      80.0,      85.0,      90.0,      95.0,           mlat 480
     C     100.0,     105.0,     110.0,     115.0,     120.0/           mlat 490
C     DATA PRESSURE  /                                                  mlat 500
      DATA P1/                                                          mlat 510
     C 1.013E+03, 9.040E+02, 8.050E+02, 7.150E+02, 6.330E+02,           mlat 520
     C 5.590E+02, 4.920E+02, 4.320E+02, 3.780E+02, 3.290E+02,           mlat 530
     C 2.860E+02, 2.470E+02, 2.130E+02, 1.820E+02, 1.560E+02,           mlat 540
     C 1.320E+02, 1.110E+02, 9.370E+01, 7.890E+01, 6.660E+01,           mlat 550
     C 5.650E+01, 4.800E+01, 4.090E+01, 3.500E+01, 3.000E+01,           mlat 560
     C 2.570E+01, 1.763E+01, 1.220E+01, 8.520E+00, 6.000E+00,           mlat 570
     C 4.260E+00, 3.050E+00, 2.200E+00, 1.590E+00, 1.160E+00,           mlat 580
     C 8.540E-01, 4.560E-01, 2.390E-01, 1.210E-01, 5.800E-02,           mlat 590
     C 2.600E-02, 1.100E-02, 4.400E-03, 1.720E-03, 6.880E-04,           mlat 600
     C 2.890E-04, 1.300E-04, 6.470E-05, 3.600E-05, 2.250E-05/           mlat 610
      DATA P2/                                                          mlat 620
     C 1.013E+03, 9.020E+02, 8.020E+02, 7.100E+02, 6.280E+02,           mlat 630
     C 5.540E+02, 4.870E+02, 4.260E+02, 3.720E+02, 3.240E+02,           mlat 640
     C 2.810E+02, 2.430E+02, 2.090E+02, 1.790E+02, 1.530E+02,           mlat 650
     C 1.300E+02, 1.110E+02, 9.500E+01, 8.120E+01, 6.950E+01,           mlat 660
     C 5.950E+01, 5.100E+01, 4.370E+01, 3.760E+01, 3.220E+01,           mlat 670
     C 2.770E+01, 1.907E+01, 1.320E+01, 9.300E+00, 6.520E+00,           mlat 680
     C 4.640E+00, 3.330E+00, 2.410E+00, 1.760E+00, 1.290E+00,           mlat 690
     C 9.510E-01, 5.150E-01, 2.720E-01, 1.390E-01, 6.700E-02,           mlat 700
     C 3.000E-02, 1.200E-02, 4.480E-03, 1.640E-03, 6.250E-04,           mlat 710
     C 2.580E-04, 1.170E-04, 6.110E-05, 3.560E-05, 2.270E-05/           mlat 720
      DATA P3/                                                          mlat 730
     C 1.018E+03, 8.973E+02, 7.897E+02, 6.938E+02, 6.081E+02,           mlat 740
     C 5.313E+02, 4.627E+02, 4.016E+02, 3.473E+02, 2.993E+02,           mlat 750
     C 2.568E+02, 2.199E+02, 1.882E+02, 1.611E+02, 1.378E+02,           mlat 760
     C 1.178E+02, 1.007E+02, 8.610E+01, 7.360E+01, 6.280E+01,           mlat 770
     C 5.370E+01, 4.580E+01, 3.910E+01, 3.340E+01, 2.860E+01,           mlat 780
     C 2.440E+01, 1.646E+01, 1.110E+01, 7.560E+00, 5.180E+00,           mlat 790
     C 3.600E+00, 2.530E+00, 1.800E+00, 1.290E+00, 9.400E-01,           mlat 800
     C 6.830E-01, 3.620E-01, 1.880E-01, 9.500E-02, 4.700E-02,           mlat 810
     C 2.220E-02, 1.030E-02, 4.560E-03, 1.980E-03, 8.770E-04,           mlat 820
     C 4.074E-04, 2.000E-04, 1.057E-04, 5.980E-05, 3.600E-05/           mlat 830
      DATA P4/                                                          mlat 840
     C 1.010E+03, 8.960E+02, 7.929E+02, 7.000E+02, 6.160E+02,           mlat 850
     C 5.410E+02, 4.740E+02, 4.130E+02, 3.590E+02, 3.108E+02,           mlat 860
     C 2.677E+02, 2.300E+02, 1.977E+02, 1.700E+02, 1.460E+02,           mlat 870
     C 1.260E+02, 1.080E+02, 9.280E+01, 7.980E+01, 6.860E+01,           mlat 880
     C 5.900E+01, 5.070E+01, 4.360E+01, 3.750E+01, 3.228E+01,           mlat 890
     C 2.780E+01, 1.923E+01, 1.340E+01, 9.400E+00, 6.610E+00,           mlat 900
     C 4.720E+00, 3.400E+00, 2.480E+00, 1.820E+00, 1.340E+00,           mlat 910
     C 9.870E-01, 5.370E-01, 2.880E-01, 1.470E-01, 7.100E-02,           mlat 920
     C 3.200E-02, 1.250E-02, 4.510E-03, 1.610E-03, 6.060E-04,           mlat 930
     C 2.480E-04, 1.130E-04, 6.000E-05, 3.540E-05, 2.260E-05/           mlat 940
      DATA P5/                                                          mlat 950
     C 1.013E+03, 8.878E+02, 7.775E+02, 6.798E+02, 5.932E+02,           mlat 960
     C 5.158E+02, 4.467E+02, 3.853E+02, 3.308E+02, 2.829E+02,           mlat 970
     C 2.418E+02, 2.067E+02, 1.766E+02, 1.510E+02, 1.291E+02,           mlat 980
     C 1.103E+02, 9.431E+01, 8.058E+01, 6.882E+01, 5.875E+01,           mlat 990
     C 5.014E+01, 4.277E+01, 3.647E+01, 3.109E+01, 2.649E+01,           mlat1000
     C 2.256E+01, 1.513E+01, 1.020E+01, 6.910E+00, 4.701E+00,           mlat1010
     C 3.230E+00, 2.243E+00, 1.570E+00, 1.113E+00, 7.900E-01,           mlat1020
     C 5.719E-01, 2.990E-01, 1.550E-01, 7.900E-02, 4.000E-02,           mlat1030
     C 2.000E-02, 9.660E-03, 4.500E-03, 2.022E-03, 9.070E-04,           mlat1040
     C 4.230E-04, 2.070E-04, 1.080E-04, 6.000E-05, 3.590E-05/           mlat1050
      DATA P6/                                                          mlat1060
     C 1.013E+03, 8.988E+02, 7.950E+02, 7.012E+02, 6.166E+02,           mlat1070
     C 5.405E+02, 4.722E+02, 4.111E+02, 3.565E+02, 3.080E+02,           mlat1080
     C 2.650E+02, 2.270E+02, 1.940E+02, 1.658E+02, 1.417E+02,           mlat1090
     C 1.211E+02, 1.035E+02, 8.850E+01, 7.565E+01, 6.467E+01,           mlat1100
     C 5.529E+01, 4.729E+01, 4.047E+01, 3.467E+01, 2.972E+01,           mlat1110
     C 2.549E+01, 1.743E+01, 1.197E+01, 8.010E+00, 5.746E+00,           mlat1120
     C 4.150E+00, 2.871E+00, 2.060E+00, 1.491E+00, 1.090E+00,           mlat1130
     C 7.978E-01, 4.250E-01, 2.190E-01, 1.090E-01, 5.220E-02,           mlat1140
     C 2.400E-02, 1.050E-02, 4.460E-03, 1.840E-03, 7.600E-04,           mlat1150
     C 3.200E-04, 1.450E-04, 7.100E-05, 4.010E-05, 2.540E-05/           mlat1160
C     DATA TEMPERATUR/                                                  mlat1170
      DATA T1/                                                          mlat1180
     C    299.70,    293.70,    287.70,    283.70,    277.00,           mlat1190
     C    270.30,    263.60,    257.00,    250.30,    243.60,           mlat1200
     C    237.00,    230.10,    223.60,    217.00,    210.30,           mlat1210
     C    203.70,    197.00,    194.80,    198.80,    202.70,           mlat1220
     C    206.70,    210.70,    214.60,    217.00,    219.20,           mlat1230
     C    221.40,    227.00,    232.30,    237.70,    243.10,           mlat1240
     C    248.50,    254.00,    259.40,    264.80,    269.60,           mlat1250
     C    270.20,    263.40,    253.10,    236.00,    218.90,           mlat1260
     C    201.80,    184.80,    177.10,    177.00,    184.30,           mlat1270
     C    190.70,    212.00,    241.60,    299.70,    380.00/           mlat1280
      DATA T2/                                                          mlat1290
     C    294.20,    289.70,    285.20,    279.20,    273.20,           mlat1300
     C    267.20,    261.20,    254.70,    248.20,    241.70,           mlat1310
     C    235.30,    228.80,    222.30,    215.80,    215.70,           mlat1320
     C    215.70,    215.70,    215.70,    216.80,    217.90,           mlat1330
     C    219.20,    220.40,    221.60,    222.80,    223.90,           mlat1340
     C    225.10,    228.45,    233.70,    239.00,    245.20,           mlat1350
     C    251.30,    257.50,    263.70,    269.90,    275.20,           mlat1360
     C    275.70,    269.30,    257.10,    240.10,    218.10,           mlat1370
     C    196.10,    174.10,    165.10,    165.00,    178.30,           mlat1380
     C    190.50,    222.20,    262.40,    316.80,    380.00/           mlat1390
      DATA T3/                                                          mlat1400
     C    272.20,    268.70,    265.20,    261.70,    255.70,           mlat1410
     C    249.70,    243.70,    237.70,    231.70,    225.70,           mlat1420
     C    219.70,    219.20,    218.70,    218.20,    217.70,           mlat1430
     C    217.20,    216.70,    216.20,    215.70,    215.20,           mlat1440
     C    215.20,    215.20,    215.20,    215.20,    215.20,           mlat1450
     C    215.20,    215.50,    217.40,    220.40,    227.90,           mlat1460
     C    235.50,    243.20,    250.80,    258.50,    265.10,           mlat1470
     C    265.70,    260.60,    250.80,    240.90,    230.70,           mlat1480
     C    220.40,    210.10,    199.80,    199.50,    208.30,           mlat1490
     C    218.60,    237.10,    259.50,    293.00,    333.00/           mlat1500
      DATA T4/                                                          mlat1510
     C    287.20,    281.70,    276.30,    270.90,    265.50,           mlat1520
     C    260.10,    253.10,    246.10,    239.20,    232.20,           mlat1530
     C    225.20,    225.20,    225.20,    225.20,    225.20,           mlat1540
     C    225.20,    225.20,    225.20,    225.20,    225.20,           mlat1550
     C    225.20,    225.20,    225.20,    225.20,    226.60,           mlat1560
     C    228.10,    231.00,    235.10,    240.00,    247.20,           mlat1570
     C    254.60,    262.10,    269.50,    273.60,    276.20,           mlat1580
     C    277.20,    274.00,    262.70,    239.70,    216.60,           mlat1590
     C    193.60,    170.60,    161.70,    161.60,    176.80,           mlat1600
     C    190.40,    226.00,    270.10,    322.70,    380.00/           mlat1610
      DATA T5/                                                          mlat1620
     C    257.20,    259.10,    255.90,    252.70,    247.70,           mlat1630
     C    240.90,    234.10,    227.30,    220.60,    217.20,           mlat1640
     C    217.20,    217.20,    217.20,    217.20,    217.20,           mlat1650
     C    217.20,    216.60,    216.00,    215.40,    214.80,           mlat1660
     C    214.20,    213.60,    213.00,    212.40,    211.80,           mlat1670
     C    211.20,    213.60,    216.00,    218.50,    222.30,           mlat1680
     C    228.50,    234.70,    240.80,    247.00,    253.20,           mlat1690
     C    259.30,    259.10,    250.90,    248.40,    245.40,           mlat1700
     C    234.70,    223.90,    213.10,    202.30,    211.00,           mlat1710
     C    218.50,    234.00,    252.60,    288.50,    333.00/           mlat1720
      DATA T6/                                                          mlat1730
     C    288.20,    281.70,    275.20,    268.70,    262.20,           mlat1740
     C    255.70,    249.20,    242.70,    236.20,    229.70,           mlat1750
     C    223.30,    216.80,    216.70,    216.70,    216.70,           mlat1760
     C    216.70,    216.70,    216.70,    216.70,    216.70,           mlat1770
     C    216.70,    217.60,    218.60,    219.60,    220.60,           mlat1780
     C    221.60,    224.00,    226.50,    230.00,    236.50,           mlat1790
     C    242.90,    250.40,    257.30,    264.20,    270.60,           mlat1800
     C    270.70,    260.80,    247.00,    233.30,    219.60,           mlat1810
     C    208.40,    198.60,    188.90,    186.90,    188.40,           mlat1820
     C    195.10,    208.80,    240.00,    300.00,    360.00/           mlat1830
C     DATA  H2O      /                                                  mlat1840
      DATA AMOL11/                                                      mlat1850
     C 2.593E+04, 1.949E+04, 1.534E+04, 8.600E+03, 4.441E+03,           mlat1860
     C 3.346E+03, 2.101E+03, 1.289E+03, 7.637E+02, 4.098E+02,           mlat1870
     C 1.912E+02, 7.306E+01, 2.905E+01, 9.900E+00, 6.220E+00,           mlat1880
     C 4.000E+00, 3.000E+00, 2.900E+00, 2.750E+00, 2.600E+00,           mlat1890
     C 2.600E+00, 2.650E+00, 2.800E+00, 2.900E+00, 3.200E+00,           mlat1900
     C 3.250E+00, 3.600E+00, 4.000E+00, 4.300E+00, 4.600E+00,           mlat1910
     C 4.900E+00, 5.200E+00, 5.500E+00, 5.700E+00, 5.900E+00,           mlat1920
     C 6.000E+00, 6.000E+00, 6.000E+00, 5.400E+00, 4.500E+00,           mlat1930
     C 3.300E+00, 2.100E+00, 1.300E+00, 8.500E-01, 5.400E-01,           mlat1940
     C 4.000E-01, 3.400E-01, 2.800E-01, 2.400E-01, 2.000E-01/           mlat1950
      DATA AMOL21/                                                      mlat1960
     C 1.876E+04, 1.378E+04, 9.680E+03, 5.984E+03, 3.813E+03,           mlat1970
     C 2.225E+03, 1.510E+03, 1.020E+03, 6.464E+02, 4.129E+02,           mlat1980
     C 2.472E+02, 9.556E+01, 2.944E+01, 8.000E+00, 5.000E+00,           mlat1990
     C 3.400E+00, 3.300E+00, 3.200E+00, 3.150E+00, 3.200E+00,           mlat2000
     C 3.300E+00, 3.450E+00, 3.600E+00, 3.850E+00, 4.000E+00,           mlat2010
     C 4.200E+00, 4.450E+00, 4.700E+00, 4.850E+00, 4.950E+00,           mlat2020
     C 5.000E+00, 5.100E+00, 5.300E+00, 5.450E+00, 5.500E+00,           mlat2030
     C 5.500E+00, 5.350E+00, 5.000E+00, 4.400E+00, 3.700E+00,           mlat2040
     C 2.950E+00, 2.100E+00, 1.330E+00, 8.500E-01, 5.400E-01,           mlat2050
     C 4.000E-01, 3.400E-01, 2.800E-01, 2.400E-01, 2.000E-01/           mlat2060
      DATA AMOL31/                                                      mlat2070
     C 4.316E+03, 3.454E+03, 2.788E+03, 2.088E+03, 1.280E+03,           mlat2080
     C 8.241E+02, 5.103E+02, 2.321E+02, 1.077E+02, 5.566E+01,           mlat2090
     C 2.960E+01, 1.000E+01, 6.000E+00, 5.000E+00, 4.800E+00,           mlat2100
     C 4.700E+00, 4.600E+00, 4.500E+00, 4.500E+00, 4.500E+00,           mlat2110
     C 4.500E+00, 4.500E+00, 4.530E+00, 4.550E+00, 4.600E+00,           mlat2120
     C 4.650E+00, 4.700E+00, 4.750E+00, 4.800E+00, 4.850E+00,           mlat2130
     C 4.900E+00, 4.950E+00, 5.000E+00, 5.000E+00, 5.000E+00,           mlat2140
     C 4.950E+00, 4.850E+00, 4.500E+00, 4.000E+00, 3.300E+00,           mlat2150
     C 2.700E+00, 2.000E+00, 1.330E+00, 8.500E-01, 5.400E-01,           mlat2160
     C 4.000E-01, 3.400E-01, 2.800E-01, 2.400E-01, 2.000E-01/           mlat2170
      DATA AMOL41/                                                      mlat2180
     C 1.194E+04, 8.701E+03, 6.750E+03, 4.820E+03, 3.380E+03,           mlat2190
     C 2.218E+03, 1.330E+03, 7.971E+02, 3.996E+02, 1.300E+02,           mlat2200
     C 4.240E+01, 1.330E+01, 6.000E+00, 4.450E+00, 4.000E+00,           mlat2210
     C 4.000E+00, 4.000E+00, 4.050E+00, 4.300E+00, 4.500E+00,           mlat2220
     C 4.600E+00, 4.700E+00, 4.800E+00, 4.830E+00, 4.850E+00,           mlat2230
     C 4.900E+00, 4.950E+00, 5.000E+00, 5.000E+00, 5.000E+00,           mlat2240
     C 5.000E+00, 5.000E+00, 5.000E+00, 5.000E+00, 5.000E+00,           mlat2250
     C 4.950E+00, 4.850E+00, 4.500E+00, 4.000E+00, 3.300E+00,           mlat2260
     C 2.700E+00, 2.000E+00, 1.330E+00, 8.500E-01, 5.400E-01,           mlat2270
     C 4.000E-01, 3.400E-01, 2.800E-01, 2.400E-01, 2.000E-01/           mlat2280
      DATA AMOL51/                                                      mlat2290
     C 1.405E+03, 1.615E+03, 1.427E+03, 1.166E+03, 7.898E+02,           mlat2300
     C 4.309E+02, 2.369E+02, 1.470E+02, 3.384E+01, 2.976E+01,           mlat2310
     C 2.000E+01, 1.000E+01, 6.000E+00, 4.450E+00, 4.500E+00,           mlat2320
     C 4.550E+00, 4.600E+00, 4.650E+00, 4.700E+00, 4.750E+00,           mlat2330
     C 4.800E+00, 4.850E+00, 4.900E+00, 4.950E+00, 5.000E+00,           mlat2340
     C 5.000E+00, 5.000E+00, 5.000E+00, 5.000E+00, 5.000E+00,           mlat2350
     C 5.000E+00, 5.000E+00, 5.000E+00, 5.000E+00, 5.000E+00,           mlat2360
     C 4.950E+00, 4.850E+00, 4.500E+00, 4.000E+00, 3.300E+00,           mlat2370
     C 2.700E+00, 2.000E+00, 1.330E+00, 8.500E-01, 5.400E-01,           mlat2380
     C 4.000E-01, 3.400E-01, 2.800E-01, 2.400E-01, 2.000E-01/           mlat2390
      DATA AMOL61/                                                      mlat2400
     C 7.745E+03, 6.071E+03, 4.631E+03, 3.182E+03, 2.158E+03,           mlat2410
     C 1.397E+03, 9.254E+02, 5.720E+02, 3.667E+02, 1.583E+02,           mlat2420
     C 6.996E+01, 3.613E+01, 1.906E+01, 1.085E+01, 5.927E+00,           mlat2430
     C 5.000E+00, 3.950E+00, 3.850E+00, 3.825E+00, 3.850E+00,           mlat2440
     C 3.900E+00, 3.975E+00, 4.065E+00, 4.200E+00, 4.300E+00,           mlat2450
     C 4.425E+00, 4.575E+00, 4.725E+00, 4.825E+00, 4.900E+00,           mlat2460
     C 4.950E+00, 5.025E+00, 5.150E+00, 5.225E+00, 5.250E+00,           mlat2470
     C 5.225E+00, 5.100E+00, 4.750E+00, 4.200E+00, 3.500E+00,           mlat2480
     C 2.825E+00, 2.050E+00, 1.330E+00, 8.500E-01, 5.400E-01,           mlat2490
     C 4.000E-01, 3.400E-01, 2.800E-01, 2.400E-01, 2.000E-01/           mlat2500
C     DATA CO2       /                                                  mlat2510
      DATA AMOL12/                                                      mlat2520
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2530
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2540
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2550
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2560
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2570
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2580
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2590
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2600
     C 3.300E+02, 3.280E+02, 3.200E+02, 3.100E+02, 2.700E+02,           mlat2610
     C 1.950E+02, 1.100E+02, 6.000E+01, 4.000E+01, 3.500E+01/           mlat2620
C     DATA CO2       /                                                  mlat2630
      DATA AMOL22/                                                      mlat2640
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2650
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2660
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2670
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2680
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2690
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2700
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2710
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2720
     C 3.300E+02, 3.280E+02, 3.200E+02, 3.100E+02, 2.700E+02,           mlat2730
     C 1.950E+02, 1.100E+02, 6.000E+01, 4.000E+01, 3.500E+01/           mlat2740
C     DATA CO2       /                                                  mlat2750
      DATA AMOL32/                                                      mlat2760
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2770
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2780
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2790
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2800
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2810
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2820
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2830
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2840
     C 3.300E+02, 3.280E+02, 3.200E+02, 3.100E+02, 2.700E+02,           mlat2850
     C 1.950E+02, 1.100E+02, 6.000E+01, 4.000E+01, 3.500E+01/           mlat2860
C     DATA CO2       /                                                  mlat2870
      DATA AMOL42/                                                      mlat2880
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2890
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2900
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2910
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2920
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2930
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2940
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2950
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat2960
     C 3.300E+02, 3.280E+02, 3.200E+02, 3.100E+02, 2.700E+02,           mlat2970
     C 1.950E+02, 1.100E+02, 6.000E+01, 4.000E+01, 3.500E+01/           mlat2980
C     DATA CO2       /                                                  mlat2990
      DATA AMOL52/                                                      mlat3000
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat3010
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat3020
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat3030
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat3040
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat3050
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat3060
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat3070
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat3080
     C 3.300E+02, 3.280E+02, 3.200E+02, 3.100E+02, 2.700E+02,           mlat3090
     C 1.950E+02, 1.100E+02, 6.000E+01, 4.000E+01, 3.500E+01/           mlat3100
C     DATA CO2       /                                                  mlat3110
      DATA AMOL62/                                                      mlat3120
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat3130
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat3140
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat3150
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat3160
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat3170
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat3180
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat3190
     C 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02, 3.300E+02,           mlat3200
     C 3.300E+02, 3.280E+02, 3.200E+02, 3.100E+02, 2.700E+02,           mlat3210
     C 1.950E+02, 1.100E+02, 6.000E+01, 4.000E+01, 3.500E+01/           mlat3220
C     DATA OZONE     /                                                  mlat3230
      DATA AMOL13/                                                      mlat3240
     C 2.869E-02, 3.150E-02, 3.342E-02, 3.504E-02, 3.561E-02,           mlat3250
     C 3.767E-02, 3.989E-02, 4.223E-02, 4.471E-02, 5.000E-02,           mlat3260
     C 5.595E-02, 6.613E-02, 7.815E-02, 9.289E-02, 1.050E-01,           mlat3270
     C 1.256E-01, 1.444E-01, 2.500E-01, 5.000E-01, 9.500E-01,           mlat3280
     C 1.400E+00, 1.800E+00, 2.400E+00, 3.400E+00, 4.300E+00,           mlat3290
     C 5.400E+00, 7.800E+00, 9.300E+00, 9.850E+00, 9.700E+00,           mlat3300
     C 8.800E+00, 7.500E+00, 5.900E+00, 4.500E+00, 3.450E+00,           mlat3310
     C 2.800E+00, 1.800E+00, 1.100E+00, 6.500E-01, 3.000E-01,           mlat3320
     C 1.800E-01, 3.300E-01, 5.000E-01, 5.200E-01, 5.000E-01,           mlat3330
     C 4.000E-01, 2.000E-01, 5.000E-02, 5.000E-03, 5.000E-04/           mlat3340
      DATA AMOL23/                                                      mlat3350
     C 3.017E-02, 3.337E-02, 3.694E-02, 4.222E-02, 4.821E-02,           mlat3360
     C 5.512E-02, 6.408E-02, 7.764E-02, 9.126E-02, 1.111E-01,           mlat3370
     C 1.304E-01, 1.793E-01, 2.230E-01, 3.000E-01, 4.400E-01,           mlat3380
     C 5.000E-01, 6.000E-01, 7.000E-01, 1.000E+00, 1.500E+00,           mlat3390
     C 2.000E+00, 2.400E+00, 2.900E+00, 3.400E+00, 4.000E+00,           mlat3400
     C 4.800E+00, 6.000E+00, 7.000E+00, 8.100E+00, 8.900E+00,           mlat3410
     C 8.700E+00, 7.550E+00, 5.900E+00, 4.500E+00, 3.500E+00,           mlat3420
     C 2.800E+00, 1.800E+00, 1.300E+00, 8.000E-01, 4.000E-01,           mlat3430
     C 1.900E-01, 2.000E-01, 5.700E-01, 7.500E-01, 7.000E-01,           mlat3440
     C 4.000E-01, 2.000E-01, 5.000E-02, 5.000E-03, 5.000E-04/           mlat3450
      DATA AMOL33/                                                      mlat3460
     C 2.778E-02, 2.800E-02, 2.849E-02, 3.200E-02, 3.567E-02,           mlat3470
     C 4.720E-02, 5.837E-02, 7.891E-02, 1.039E-01, 1.567E-01,           mlat3480
     C 2.370E-01, 3.624E-01, 5.232E-01, 7.036E-01, 8.000E-01,           mlat3490
     C 9.000E-01, 1.100E+00, 1.400E+00, 1.800E+00, 2.300E+00,           mlat3500
     C 2.900E+00, 3.500E+00, 3.900E+00, 4.300E+00, 4.700E+00,           mlat3510
     C 5.100E+00, 5.600E+00, 6.100E+00, 6.800E+00, 7.100E+00,           mlat3520
     C 7.200E+00, 6.900E+00, 5.900E+00, 4.600E+00, 3.700E+00,           mlat3530
     C 2.750E+00, 1.700E+00, 1.000E-00, 5.500E-01, 3.200E-01,           mlat3540
     C 2.500E-01, 2.300E-01, 5.500E-01, 8.000E-01, 8.000E-01,           mlat3550
     C 4.000E-01, 2.000E-01, 5.000E-02, 5.000E-03, 5.000E-04/           mlat3560
      DATA AMOL43/                                                      mlat3570
     C 2.412E-02, 2.940E-02, 3.379E-02, 3.887E-02, 4.478E-02,           mlat3580
     C 5.328E-02, 6.564E-02, 7.738E-02, 9.114E-02, 1.420E-01,           mlat3590
     C 1.890E-01, 3.050E-01, 4.100E-01, 5.000E-01, 6.000E-01,           mlat3600
     C 7.000E-01, 8.500E-01, 1.000E+00, 1.300E+00, 1.700E+00,           mlat3610
     C 2.100E+00, 2.700E+00, 3.300E+00, 3.700E+00, 4.200E+00,           mlat3620
     C 4.500E+00, 5.300E+00, 5.700E+00, 6.900E+00, 7.700E+00,           mlat3630
     C 7.800E+00, 7.000E+00, 5.400E+00, 4.200E+00, 3.200E+00,           mlat3640
     C 2.500E+00, 1.700E+00, 1.200E+00, 8.000E-01, 4.000E-01,           mlat3650
     C 2.000E-01, 1.800E-01, 6.500E-01, 9.000E-01, 8.000E-01,           mlat3660
     C 4.000E-01, 2.000E-01, 5.000E-02, 5.000E-03, 5.000E-04/           mlat3670
      DATA AMOL53/                                                      mlat3680
     C 1.802E-02, 2.072E-02, 2.336E-02, 2.767E-02, 3.253E-02,           mlat3690
     C 3.801E-02, 4.446E-02, 7.252E-02, 1.040E-01, 2.100E-01,           mlat3700
     C 3.000E-01, 3.500E-01, 4.000E-01, 6.500E-01, 9.000E-01,           mlat3710
     C 1.200E+00, 1.500E+00, 1.900E+00, 2.450E+00, 3.100E+00,           mlat3720
     C 3.700E+00, 4.000E+00, 4.200E+00, 4.500E+00, 4.600E+00,           mlat3730
     C 4.700E+00, 4.900E+00, 5.400E+00, 5.900E+00, 6.200E+00,           mlat3740
     C 6.250E+00, 5.900E+00, 5.100E+00, 4.100E+00, 3.000E+00,           mlat3750
     C 2.600E+00, 1.600E+00, 9.500E-01, 6.500E-01, 5.000E-01,           mlat3760
     C 3.300E-01, 1.300E-01, 7.500E-01, 8.000E-01, 8.000E-01,           mlat3770
     C 4.000E-01, 2.000E-01, 5.000E-02, 5.000E-03, 5.000E-04/           mlat3780
      DATA AMOL63/                                                      mlat3790
     C 2.660E-02, 2.931E-02, 3.237E-02, 3.318E-02, 3.387E-02,           mlat3800
     C 3.768E-02, 4.112E-02, 5.009E-02, 5.966E-02, 9.168E-02,           mlat3810
     C 1.313E-01, 2.149E-01, 3.095E-01, 3.846E-01, 5.030E-01,           mlat3820
     C 6.505E-01, 8.701E-01, 1.187E+00, 1.587E+00, 2.030E+00,           mlat3830
     C 2.579E+00, 3.028E+00, 3.647E+00, 4.168E+00, 4.627E+00,           mlat3840
     C 5.118E+00, 5.803E+00, 6.553E+00, 7.373E+00, 7.837E+00,           mlat3850
     C 7.800E+00, 7.300E+00, 6.200E+00, 5.250E+00, 4.100E+00,           mlat3860
     C 3.100E+00, 1.800E+00, 1.100E+00, 7.000E-01, 3.000E-01,           mlat3870
     C 2.500E-01, 3.000E-01, 5.000E-01, 7.000E-01, 7.000E-01,           mlat3880
     C 4.000E-01, 2.000E-01, 5.000E-02, 5.000E-03, 5.000E-04/           mlat3890
C     DATA  N2O      /                                                  mlat3900
      DATA AMOL14/                                                      mlat3910
     C 3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01,           mlat3920
     C 3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01, 3.195E-01,           mlat3930
     C 3.179E-01, 3.140E-01, 3.095E-01, 3.048E-01, 2.999E-01,           mlat3940
     C 2.944E-01, 2.877E-01, 2.783E-01, 2.671E-01, 2.527E-01,           mlat3950
     C 2.365E-01, 2.194E-01, 2.051E-01, 1.967E-01, 1.875E-01,           mlat3960
     C 1.756E-01, 1.588E-01, 1.416E-01, 1.165E-01, 9.275E-02,           mlat3970
     C 6.693E-02, 4.513E-02, 2.751E-02, 1.591E-02, 9.378E-03,           mlat3980
     C 4.752E-03, 3.000E-03, 2.065E-03, 1.507E-03, 1.149E-03,           mlat3990
     C 8.890E-04, 7.056E-04, 5.716E-04, 4.708E-04, 3.932E-04,           mlat4000
     C 3.323E-04, 2.837E-04, 2.443E-04, 2.120E-04, 1.851E-04/           mlat4010
C     DATA  N2O      /                                                  mlat4020
      DATA AMOL24/                                                      mlat4030
     C 3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01,           mlat4040
     C 3.200E-01, 3.200E-01, 3.200E-01, 3.195E-01, 3.163E-01,           mlat4050
     C 3.096E-01, 2.989E-01, 2.936E-01, 2.860E-01, 2.800E-01,           mlat4060
     C 2.724E-01, 2.611E-01, 2.421E-01, 2.174E-01, 1.843E-01,           mlat4070
     C 1.607E-01, 1.323E-01, 1.146E-01, 1.035E-01, 9.622E-02,           mlat4080
     C 8.958E-02, 8.006E-02, 6.698E-02, 4.958E-02, 3.695E-02,           mlat4090
     C 2.519E-02, 1.736E-02, 1.158E-02, 7.665E-03, 5.321E-03,           mlat4100
     C 3.215E-03, 2.030E-03, 1.397E-03, 1.020E-03, 7.772E-04,           mlat4110
     C 6.257E-04, 5.166E-04, 4.352E-04, 3.727E-04, 3.237E-04,           mlat4120
     C 2.844E-04, 2.524E-04, 2.260E-04, 2.039E-04, 1.851E-04/           mlat4130
C     DATA  N2O      /                                                  mlat4140
      DATA AMOL34/                                                      mlat4150
     C 3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01,           mlat4160
     C 3.200E-01, 3.200E-01, 3.200E-01, 3.195E-01, 3.163E-01,           mlat4170
     C 3.096E-01, 2.989E-01, 2.936E-01, 2.860E-01, 2.800E-01,           mlat4180
     C 2.724E-01, 2.611E-01, 2.421E-01, 2.174E-01, 1.843E-01,           mlat4190
     C 1.621E-01, 1.362E-01, 1.230E-01, 1.124E-01, 1.048E-01,           mlat4200
     C 9.661E-02, 8.693E-02, 7.524E-02, 6.126E-02, 5.116E-02,           mlat4210
     C 3.968E-02, 2.995E-02, 2.080E-02, 1.311E-02, 8.071E-03,           mlat4220
     C 4.164E-03, 2.629E-03, 1.809E-03, 1.321E-03, 1.007E-03,           mlat4230
     C 7.883E-04, 6.333E-04, 5.194E-04, 4.333E-04, 3.666E-04,           mlat4240
     C 3.140E-04, 2.717E-04, 2.373E-04, 2.089E-04, 1.851E-04/           mlat4250
C     DATA  N2O      /                                                  mlat4260
      DATA AMOL44/                                                      mlat4270
     C 3.100E-01, 3.100E-01, 3.100E-01, 3.100E-01, 3.079E-01,           mlat4280
     C 3.024E-01, 2.906E-01, 2.822E-01, 2.759E-01, 2.703E-01,           mlat4290
     C 2.651E-01, 2.600E-01, 2.549E-01, 2.494E-01, 2.433E-01,           mlat4300
     C 2.355E-01, 2.282E-01, 2.179E-01, 2.035E-01, 1.817E-01,           mlat4310
     C 1.567E-01, 1.350E-01, 1.218E-01, 1.102E-01, 9.893E-02,           mlat4320
     C 8.775E-02, 7.327E-02, 5.941E-02, 4.154E-02, 3.032E-02,           mlat4330
     C 1.949E-02, 1.274E-02, 9.001E-03, 6.286E-03, 4.558E-03,           mlat4340
     C 2.795E-03, 1.765E-03, 1.214E-03, 8.866E-04, 6.756E-04,           mlat4350
     C 5.538E-04, 4.649E-04, 3.979E-04, 3.459E-04, 3.047E-04,           mlat4360
     C 2.713E-04, 2.439E-04, 2.210E-04, 2.017E-04, 1.851E-04/           mlat4370
C     DATA  N2O      /                                                  mlat4380
      DATA AMOL54/                                                      mlat4390
     C 3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01,           mlat4400
     C 3.200E-01, 3.200E-01, 3.200E-01, 3.195E-01, 3.163E-01,           mlat4410
     C 3.096E-01, 2.989E-01, 2.936E-01, 2.860E-01, 2.800E-01,           mlat4420
     C 2.724E-01, 2.611E-01, 2.421E-01, 2.174E-01, 1.843E-01,           mlat4430
     C 1.621E-01, 1.362E-01, 1.230E-01, 1.122E-01, 1.043E-01,           mlat4440
     C 9.570E-02, 8.598E-02, 7.314E-02, 5.710E-02, 4.670E-02,           mlat4450
     C 3.439E-02, 2.471E-02, 1.631E-02, 1.066E-02, 7.064E-03,           mlat4460
     C 3.972E-03, 2.508E-03, 1.726E-03, 1.260E-03, 9.602E-04,           mlat4470
     C 7.554E-04, 6.097E-04, 5.024E-04, 4.210E-04, 3.579E-04,           mlat4480
     C 3.080E-04, 2.678E-04, 2.350E-04, 2.079E-04, 1.851E-04/           mlat4490
C     DATA  N2O      /                                                  mlat4500
      DATA AMOL64/                                                      mlat4510
     C 3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01,           mlat4520
     C 3.200E-01, 3.200E-01, 3.200E-01, 3.200E-01, 3.195E-01,           mlat4530
     C 3.179E-01, 3.140E-01, 3.095E-01, 3.048E-01, 2.999E-01,           mlat4540
     C 2.944E-01, 2.877E-01, 2.783E-01, 2.671E-01, 2.527E-01,           mlat4550
     C 2.365E-01, 2.194E-01, 2.051E-01, 1.967E-01, 1.875E-01,           mlat4560
     C 1.756E-01, 1.588E-01, 1.416E-01, 1.165E-01, 9.275E-02,           mlat4570
     C 6.693E-02, 4.513E-02, 2.751E-02, 1.591E-02, 9.378E-03,           mlat4580
     C 4.752E-03, 3.000E-03, 2.065E-03, 1.507E-03, 1.149E-03,           mlat4590
     C 8.890E-04, 7.056E-04, 5.716E-04, 4.708E-04, 3.932E-04,           mlat4600
     C 3.323E-04, 2.837E-04, 2.443E-04, 2.120E-04, 1.851E-04/           mlat4610
C     DATA CO        /                                                  mlat4620
      DATA AMOL15/                                                      mlat4630
     C 1.500E-01, 1.450E-01, 1.399E-01, 1.349E-01, 1.312E-01,           mlat4640
     C 1.303E-01, 1.288E-01, 1.247E-01, 1.185E-01, 1.094E-01,           mlat4650
     C 9.962E-02, 8.964E-02, 7.814E-02, 6.374E-02, 5.025E-02,           mlat4660
     C 3.941E-02, 3.069E-02, 2.489E-02, 1.966E-02, 1.549E-02,           mlat4670
     C 1.331E-02, 1.232E-02, 1.232E-02, 1.307E-02, 1.400E-02,           mlat4680
     C 1.521E-02, 1.722E-02, 1.995E-02, 2.266E-02, 2.487E-02,           mlat4690
     C 2.738E-02, 3.098E-02, 3.510E-02, 3.987E-02, 4.482E-02,           mlat4700
     C 5.092E-02, 5.985E-02, 6.960E-02, 9.188E-02, 1.938E-01,           mlat4710
     C 5.688E-01, 1.549E+00, 3.849E+00, 6.590E+00, 1.044E+01,           mlat4720
     C 1.705E+01, 2.471E+01, 3.358E+01, 4.148E+01, 5.000E+01/           mlat4730
C     DATA CO        /                                                  mlat4740
      DATA AMOL25/                                                      mlat4750
     C 1.500E-01, 1.450E-01, 1.399E-01, 1.349E-01, 1.312E-01,           mlat4760
     C 1.303E-01, 1.288E-01, 1.247E-01, 1.185E-01, 1.094E-01,           mlat4770
     C 9.962E-02, 8.964E-02, 7.814E-02, 6.374E-02, 5.025E-02,           mlat4780
     C 3.941E-02, 3.069E-02, 2.489E-02, 1.966E-02, 1.549E-02,           mlat4790
     C 1.331E-02, 1.232E-02, 1.232E-02, 1.307E-02, 1.400E-02,           mlat4800
     C 1.521E-02, 1.722E-02, 1.995E-02, 2.266E-02, 2.487E-02,           mlat4810
     C 2.716E-02, 2.962E-02, 3.138E-02, 3.307E-02, 3.487E-02,           mlat4820
     C 3.645E-02, 3.923E-02, 4.673E-02, 6.404E-02, 1.177E-01,           mlat4830
     C 2.935E-01, 6.815E-01, 1.465E+00, 2.849E+00, 5.166E+00,           mlat4840
     C 1.008E+01, 1.865E+01, 2.863E+01, 3.890E+01, 5.000E+01/           mlat4850
C     DATA CO        /                                                  mlat4860
      DATA AMOL35/                                                      mlat4870
     C 1.500E-01, 1.450E-01, 1.399E-01, 1.349E-01, 1.312E-01,           mlat4880
     C 1.303E-01, 1.288E-01, 1.247E-01, 1.185E-01, 1.094E-01,           mlat4890
     C 9.962E-02, 8.964E-02, 7.814E-02, 6.374E-02, 5.025E-02,           mlat4900
     C 3.941E-02, 3.069E-02, 2.489E-02, 1.966E-02, 1.549E-02,           mlat4910
     C 1.331E-02, 1.232E-02, 1.232E-02, 1.307E-02, 1.400E-02,           mlat4920
     C 1.498E-02, 1.598E-02, 1.710E-02, 1.850E-02, 1.997E-02,           mlat4930
     C 2.147E-02, 2.331E-02, 2.622E-02, 3.057E-02, 3.803E-02,           mlat4940
     C 6.245E-02, 1.480E-01, 2.926E-01, 5.586E-01, 1.078E+00,           mlat4950
     C 1.897E+00, 2.960E+00, 4.526E+00, 6.862E+00, 1.054E+01,           mlat4960
     C 1.709E+01, 2.473E+01, 3.359E+01, 4.149E+01, 5.000E+01/           mlat4970
C     DATA CO        /                                                  mlat4980
      DATA AMOL45/                                                      mlat4990
     C 1.500E-01, 1.450E-01, 1.399E-01, 1.349E-01, 1.312E-01,           mlat5000
     C 1.303E-01, 1.288E-01, 1.247E-01, 1.185E-01, 1.094E-01,           mlat5010
     C 9.962E-02, 8.964E-02, 7.814E-02, 6.374E-02, 5.025E-02,           mlat5020
     C 3.941E-02, 3.069E-02, 2.489E-02, 1.966E-02, 1.549E-02,           mlat5030
     C 1.331E-02, 1.232E-02, 1.232E-02, 1.307E-02, 1.400E-02,           mlat5040
     C 1.510E-02, 1.649E-02, 1.808E-02, 1.997E-02, 2.183E-02,           mlat5050
     C 2.343E-02, 2.496E-02, 2.647E-02, 2.809E-02, 2.999E-02,           mlat5060
     C 3.220E-02, 3.650E-02, 4.589E-02, 6.375E-02, 1.176E-01,           mlat5070
     C 3.033E-01, 7.894E-01, 1.823E+00, 3.402E+00, 5.916E+00,           mlat5080
     C 1.043E+01, 1.881E+01, 2.869E+01, 3.892E+01, 5.000E+01/           mlat5090
C     DATA CO        /                                                  mlat5100
      DATA AMOL55/                                                      mlat5110
     C 1.500E-01, 1.450E-01, 1.399E-01, 1.349E-01, 1.312E-01,           mlat5120
     C 1.303E-01, 1.288E-01, 1.247E-01, 1.185E-01, 1.094E-01,           mlat5130
     C 9.962E-02, 8.964E-02, 7.814E-02, 6.374E-02, 5.025E-02,           mlat5140
     C 3.941E-02, 3.069E-02, 2.489E-02, 1.966E-02, 1.549E-02,           mlat5150
     C 1.331E-02, 1.232E-02, 1.232E-02, 1.307E-02, 1.400E-02,           mlat5160
     C 1.521E-02, 1.722E-02, 2.037E-02, 2.486E-02, 3.168E-02,           mlat5170
     C 4.429E-02, 6.472E-02, 1.041E-01, 1.507E-01, 2.163E-01,           mlat5180
     C 3.141E-01, 4.842E-01, 7.147E-01, 1.067E+00, 1.516E+00,           mlat5190
     C 2.166E+00, 3.060E+00, 4.564E+00, 6.877E+00, 1.055E+01,           mlat5200
     C 1.710E+01, 2.473E+01, 3.359E+01, 4.149E+01, 5.000E+01/           mlat5210
C     DATA CO        /                                                  mlat5220
      DATA AMOL65/                                                      mlat5230
     C 1.500E-01, 1.450E-01, 1.399E-01, 1.349E-01, 1.312E-01,           mlat5240
     C 1.303E-01, 1.288E-01, 1.247E-01, 1.185E-01, 1.094E-01,           mlat5250
     C 9.962E-02, 8.964E-02, 7.814E-02, 6.374E-02, 5.025E-02,           mlat5260
     C 3.941E-02, 3.069E-02, 2.489E-02, 1.966E-02, 1.549E-02,           mlat5270
     C 1.331E-02, 1.232E-02, 1.232E-02, 1.307E-02, 1.400E-02,           mlat5280
     C 1.498E-02, 1.598E-02, 1.710E-02, 1.850E-02, 2.009E-02,           mlat5290
     C 2.220E-02, 2.497E-02, 2.824E-02, 3.241E-02, 3.717E-02,           mlat5300
     C 4.597E-02, 6.639E-02, 1.073E-01, 1.862E-01, 3.059E-01,           mlat5310
     C 6.375E-01, 1.497E+00, 3.239E+00, 5.843E+00, 1.013E+01,           mlat5320
     C 1.692E+01, 2.467E+01, 3.356E+01, 4.148E+01, 5.000E+01/           mlat5330
C     DATA  CH4      /                                                  mlat5340
      DATA AMOL16/                                                      mlat5350
     C 1.700E+00, 1.700E+00, 1.700E+00, 1.700E+00, 1.700E+00,           mlat5360
     C 1.700E+00, 1.700E+00, 1.699E+00, 1.697E+00, 1.693E+00,           mlat5370
     C 1.685E+00, 1.675E+00, 1.662E+00, 1.645E+00, 1.626E+00,           mlat5380
     C 1.605E+00, 1.582E+00, 1.553E+00, 1.521E+00, 1.480E+00,           mlat5390
     C 1.424E+00, 1.355E+00, 1.272E+00, 1.191E+00, 1.118E+00,           mlat5400
     C 1.055E+00, 9.870E-01, 9.136E-01, 8.300E-01, 7.460E-01,           mlat5410
     C 6.618E-01, 5.638E-01, 4.614E-01, 3.631E-01, 2.773E-01,           mlat5420
     C 2.100E-01, 1.651E-01, 1.500E-01, 1.500E-01, 1.500E-01,           mlat5430
     C 1.500E-01, 1.500E-01, 1.500E-01, 1.400E-01, 1.300E-01,           mlat5440
     C 1.200E-01, 1.100E-01, 9.500E-02, 6.000E-02, 3.000E-02/           mlat5450
C     DATA  CH4      /                                                  mlat5460
      DATA AMOL26/                                                      mlat5470
     C 1.700E+00, 1.700E+00, 1.700E+00, 1.700E+00, 1.697E+00,           mlat5480
     C 1.687E+00, 1.672E+00, 1.649E+00, 1.629E+00, 1.615E+00,           mlat5490
     C 1.579E+00, 1.542E+00, 1.508E+00, 1.479E+00, 1.451E+00,           mlat5500
     C 1.422E+00, 1.390E+00, 1.356E+00, 1.323E+00, 1.281E+00,           mlat5510
     C 1.224E+00, 1.154E+00, 1.066E+00, 9.730E-01, 8.800E-01,           mlat5520
     C 7.888E-01, 7.046E-01, 6.315E-01, 5.592E-01, 5.008E-01,           mlat5530
     C 4.453E-01, 3.916E-01, 3.389E-01, 2.873E-01, 2.384E-01,           mlat5540
     C 1.944E-01, 1.574E-01, 1.500E-01, 1.500E-01, 1.500E-01,           mlat5550
     C 1.500E-01, 1.500E-01, 1.500E-01, 1.400E-01, 1.300E-01,           mlat5560
     C 1.200E-01, 1.100E-01, 9.500E-02, 6.000E-02, 3.000E-02/           mlat5570
C     DATA  CH4      /                                                  mlat5580
      DATA AMOL36/                                                      mlat5590
     C 1.700E+00, 1.700E+00, 1.700E+00, 1.700E+00, 1.697E+00,           mlat5600
     C 1.687E+00, 1.672E+00, 1.649E+00, 1.629E+00, 1.615E+00,           mlat5610
     C 1.579E+00, 1.542E+00, 1.508E+00, 1.479E+00, 1.451E+00,           mlat5620
     C 1.422E+00, 1.390E+00, 1.356E+00, 1.323E+00, 1.281E+00,           mlat5630
     C 1.224E+00, 1.154E+00, 1.066E+00, 9.730E-01, 8.800E-01,           mlat5640
     C 7.931E-01, 7.130E-01, 6.438E-01, 5.746E-01, 5.050E-01,           mlat5650
     C 4.481E-01, 3.931E-01, 3.395E-01, 2.876E-01, 2.386E-01,           mlat5660
     C 1.944E-01, 1.574E-01, 1.500E-01, 1.500E-01, 1.500E-01,           mlat5670
     C 1.500E-01, 1.500E-01, 1.500E-01, 1.400E-01, 1.300E-01,           mlat5680
     C 1.200E-01, 1.100E-01, 9.500E-02, 6.000E-02, 3.000E-02/           mlat5690
C     DATA  CH4      /                                                  mlat5700
      DATA AMOL46/                                                      mlat5710
     C 1.700E+00, 1.700E+00, 1.700E+00, 1.700E+00, 1.697E+00,           mlat5720
     C 1.687E+00, 1.672E+00, 1.649E+00, 1.629E+00, 1.615E+00,           mlat5730
     C 1.579E+00, 1.542E+00, 1.506E+00, 1.471E+00, 1.434E+00,           mlat5740
     C 1.389E+00, 1.342E+00, 1.290E+00, 1.230E+00, 1.157E+00,           mlat5750
     C 1.072E+00, 9.903E-01, 9.170E-01, 8.574E-01, 8.013E-01,           mlat5760
     C 7.477E-01, 6.956E-01, 6.442E-01, 5.888E-01, 5.240E-01,           mlat5770
     C 4.506E-01, 3.708E-01, 2.992E-01, 2.445E-01, 2.000E-01,           mlat5780
     C 1.660E-01, 1.500E-01, 1.500E-01, 1.500E-01, 1.500E-01,           mlat5790
     C 1.500E-01, 1.500E-01, 1.500E-01, 1.400E-01, 1.300E-01,           mlat5800
     C 1.200E-01, 1.100E-01, 9.500E-02, 6.000E-02, 3.000E-02/           mlat5810
C     DATA  CH4      /                                                  mlat5820
      DATA AMOL56/                                                      mlat5830
     C 1.700E+00, 1.700E+00, 1.700E+00, 1.700E+00, 1.697E+00,           mlat5840
     C 1.687E+00, 1.672E+00, 1.649E+00, 1.629E+00, 1.615E+00,           mlat5850
     C 1.579E+00, 1.542E+00, 1.506E+00, 1.471E+00, 1.434E+00,           mlat5860
     C 1.389E+00, 1.342E+00, 1.290E+00, 1.230E+00, 1.161E+00,           mlat5870
     C 1.084E+00, 1.014E+00, 9.561E-01, 9.009E-01, 8.479E-01,           mlat5880
     C 7.961E-01, 7.449E-01, 6.941E-01, 6.434E-01, 5.883E-01,           mlat5890
     C 5.238E-01, 4.505E-01, 3.708E-01, 3.004E-01, 2.453E-01,           mlat5900
     C 1.980E-01, 1.590E-01, 1.500E-01, 1.500E-01, 1.500E-01,           mlat5910
     C 1.500E-01, 1.500E-01, 1.500E-01, 1.400E-01, 1.300E-01,           mlat5920
     C 1.200E-01, 1.100E-01, 9.500E-02, 6.000E-02, 3.000E-02/           mlat5930
C     DATA  CH4      /                                                  mlat5940
      DATA AMOL66/                                                      mlat5950
     C 1.700E+00, 1.700E+00, 1.700E+00, 1.700E+00, 1.700E+00,           mlat5960
     C 1.700E+00, 1.700E+00, 1.699E+00, 1.697E+00, 1.693E+00,           mlat5970
     C 1.685E+00, 1.675E+00, 1.662E+00, 1.645E+00, 1.626E+00,           mlat5980
     C 1.605E+00, 1.582E+00, 1.553E+00, 1.521E+00, 1.480E+00,           mlat5990
     C 1.424E+00, 1.355E+00, 1.272E+00, 1.191E+00, 1.118E+00,           mlat6000
     C 1.055E+00, 9.870E-01, 9.136E-01, 8.300E-01, 7.460E-01,           mlat6010
     C 6.618E-01, 5.638E-01, 4.614E-01, 3.631E-01, 2.773E-01,           mlat6020
     C 2.100E-01, 1.650E-01, 1.500E-01, 1.500E-01, 1.500E-01,           mlat6030
     C 1.500E-01, 1.500E-01, 1.500E-01, 1.400E-01, 1.300E-01,           mlat6040
     C 1.200E-01, 1.100E-01, 9.500E-02, 6.000E-02, 3.000E-02/           mlat6050
C     DATA O2        /                                                  mlat6060
      DATA AMOL17/                                                      mlat6070
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6080
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6090
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6100
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6110
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6120
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6130
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6140
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6150
     C 2.090E+05, 2.090E+05, 2.000E+05, 1.900E+05, 1.800E+05,           mlat6160
     C 1.600E+05, 1.400E+05, 1.200E+05, 9.400E+04, 7.250E+04/           mlat6170
C     DATA O2        /                                                  mlat6180
      DATA AMOL27/                                                      mlat6190
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6200
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6210
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6220
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6230
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6240
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6250
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6260
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6270
     C 2.090E+05, 2.090E+05, 2.000E+05, 1.900E+05, 1.800E+05,           mlat6280
     C 1.600E+05, 1.400E+05, 1.200E+05, 9.400E+04, 7.250E+04/           mlat6290
C     DATA O2        /                                                  mlat6300
      DATA AMOL37/                                                      mlat6310
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6320
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6330
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6340
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6350
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6360
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6370
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6380
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6390
     C 2.090E+05, 2.090E+05, 2.000E+05, 1.900E+05, 1.800E+05,           mlat6400
     C 1.600E+05, 1.400E+05, 1.200E+05, 9.400E+04, 7.250E+04/           mlat6410
C     DATA O2        /                                                  mlat6420
      DATA AMOL47/                                                      mlat6430
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6440
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6450
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6460
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6470
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6480
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6490
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6500
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6510
     C 2.090E+05, 2.090E+05, 2.000E+05, 1.900E+05, 1.800E+05,           mlat6520
     C 1.600E+05, 1.400E+05, 1.200E+05, 9.400E+04, 7.250E+04/           mlat6530
C     DATA O2        /                                                  mlat6540
      DATA AMOL57/                                                      mlat6550
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6560
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6570
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6580
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6590
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6600
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6610
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6620
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6630
     C 2.090E+05, 2.090E+05, 2.000E+05, 1.900E+05, 1.800E+05,           mlat6640
     C 1.600E+05, 1.400E+05, 1.200E+05, 9.400E+04, 7.250E+04/           mlat6650
C     DATA O2        /                                                  mlat6660
      DATA AMOL67/                                                      mlat6670
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6680
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6690
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6700
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6710
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6720
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6730
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6740
     C 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05, 2.090E+05,           mlat6750
     C 2.090E+05, 2.090E+05, 2.000E+05, 1.900E+05, 1.800E+05,           mlat6760
     C 1.600E+05, 1.400E+05, 1.200E+05, 9.400E+04, 7.250E+04/           mlat6770
C     DATA DENSITY   /                                                  mlat6780
      DATA AMOL18/                                                      mlat6790
     C 2.450E+19, 2.231E+19, 2.028E+19, 1.827E+19, 1.656E+19,           mlat6800
     C 1.499E+19, 1.353E+19, 1.218E+19, 1.095E+19, 9.789E+18,           mlat6810
     C 8.747E+18, 7.780E+18, 6.904E+18, 6.079E+18, 5.377E+18,           mlat6820
     C 4.697E+18, 4.084E+18, 3.486E+18, 2.877E+18, 2.381E+18,           mlat6830
     C 1.981E+18, 1.651E+18, 1.381E+18, 1.169E+18, 9.920E+17,           mlat6840
     C 8.413E+17, 5.629E+17, 3.807E+17, 2.598E+17, 1.789E+17,           mlat6850
     C 1.243E+17, 8.703E+16, 6.147E+16, 4.352E+16, 3.119E+16,           mlat6860
     C 2.291E+16, 1.255E+16, 6.844E+15, 3.716E+15, 1.920E+15,           mlat6870
     C 9.338E+14, 4.314E+14, 1.801E+14, 7.043E+13, 2.706E+13,           mlat6880
     C 1.098E+13, 4.445E+12, 1.941E+12, 8.706E+11, 4.225E+11/           mlat6890
      DATA AMOL28/                                                      mlat6900
     C 2.496E+19, 2.257E+19, 2.038E+19, 1.843E+19, 1.666E+19,           mlat6910
     C 1.503E+19, 1.351E+19, 1.212E+19, 1.086E+19, 9.716E+18,           mlat6920
     C 8.656E+18, 7.698E+18, 6.814E+18, 6.012E+18, 5.141E+18,           mlat6930
     C 4.368E+18, 3.730E+18, 3.192E+18, 2.715E+18, 2.312E+18,           mlat6940
     C 1.967E+18, 1.677E+18, 1.429E+18, 1.223E+18, 1.042E+18,           mlat6950
     C 8.919E+17, 6.050E+17, 4.094E+17, 2.820E+17, 1.927E+17,           mlat6960
     C 1.338E+17, 9.373E+16, 6.624E+16, 4.726E+16, 3.398E+16,           mlat6970
     C 2.500E+16, 1.386E+16, 7.668E+15, 4.196E+15, 2.227E+15,           mlat6980
     C 1.109E+15, 4.996E+14, 1.967E+14, 7.204E+13, 2.541E+13,           mlat6990
     C 9.816E+12, 3.816E+12, 1.688E+12, 8.145E+11, 4.330E+11/           mlat7000
      DATA AMOL38/                                                      mlat7010
     C 2.711E+19, 2.420E+19, 2.158E+19, 1.922E+19, 1.724E+19,           mlat7020
     C 1.542E+19, 1.376E+19, 1.225E+19, 1.086E+19, 9.612E+18,           mlat7030
     C 8.472E+18, 7.271E+18, 6.237E+18, 5.351E+18, 4.588E+18,           mlat7040
     C 3.931E+18, 3.368E+18, 2.886E+18, 2.473E+18, 2.115E+18,           mlat7050
     C 1.809E+18, 1.543E+18, 1.317E+18, 1.125E+18, 9.633E+17,           mlat7060
     C 8.218E+17, 5.536E+17, 3.701E+17, 2.486E+17, 1.647E+17,           mlat7070
     C 1.108E+17, 7.540E+16, 5.202E+16, 3.617E+16, 2.570E+16,           mlat7080
     C 1.863E+16, 1.007E+16, 5.433E+15, 2.858E+15, 1.477E+15,           mlat7090
     C 7.301E+14, 3.553E+14, 1.654E+14, 7.194E+13, 3.052E+13,           mlat7100
     C 1.351E+13, 6.114E+12, 2.952E+12, 1.479E+12, 7.836E+11/           mlat7110
      DATA AMOL48/                                                      mlat7120
     C 2.549E+19, 2.305E+19, 2.080E+19, 1.873E+19, 1.682E+19,           mlat7130
     C 1.508E+19, 1.357E+19, 1.216E+19, 1.088E+19, 9.701E+18,           mlat7140
     C 8.616E+18, 7.402E+18, 6.363E+18, 5.471E+18, 4.699E+18,           mlat7150
     C 4.055E+18, 3.476E+18, 2.987E+18, 2.568E+18, 2.208E+18,           mlat7160
     C 1.899E+18, 1.632E+18, 1.403E+18, 1.207E+18, 1.033E+18,           mlat7170
     C 8.834E+17, 6.034E+17, 4.131E+17, 2.839E+17, 1.938E+17,           mlat7180
     C 1.344E+17, 9.402E+16, 6.670E+16, 4.821E+16, 3.516E+16,           mlat7190
     C 2.581E+16, 1.421E+16, 7.946E+15, 4.445E+15, 2.376E+15,           mlat7200
     C 1.198E+15, 5.311E+14, 2.022E+14, 7.221E+13, 2.484E+13,           mlat7210
     C 9.441E+12, 3.624E+12, 1.610E+12, 7.951E+11, 4.311E+11/           mlat7220
      DATA AMOL58/                                                      mlat7230
     C 2.855E+19, 2.484E+19, 2.202E+19, 1.950E+19, 1.736E+19,           mlat7240
     C 1.552E+19, 1.383E+19, 1.229E+19, 1.087E+19, 9.440E+18,           mlat7250
     C 8.069E+18, 6.898E+18, 5.893E+18, 5.039E+18, 4.308E+18,           mlat7260
     C 3.681E+18, 3.156E+18, 2.704E+18, 2.316E+18, 1.982E+18,           mlat7270
     C 1.697E+18, 1.451E+18, 1.241E+18, 1.061E+18, 9.065E+17,           mlat7280
     C 7.742E+17, 5.134E+17, 3.423E+17, 2.292E+17, 1.533E+17,           mlat7290
     C 1.025E+17, 6.927E+16, 4.726E+16, 3.266E+16, 2.261E+16,           mlat7300
     C 1.599E+16, 8.364E+15, 4.478E+15, 2.305E+15, 1.181E+15,           mlat7310
     C 6.176E+14, 3.127E+14, 1.531E+14, 7.244E+13, 3.116E+13,           mlat7320
     C 1.403E+13, 6.412E+12, 3.099E+12, 1.507E+12, 7.814E+11/           mlat7330
      DATA AMOL68/                                                      mlat7340
     C 2.548E+19, 2.313E+19, 2.094E+19, 1.891E+19, 1.704E+19,           mlat7350
     C 1.532E+19, 1.373E+19, 1.228E+19, 1.094E+19, 9.719E+18,           mlat7360
     C 8.602E+18, 7.589E+18, 6.489E+18, 5.546E+18, 4.739E+18,           mlat7370
     C 4.050E+18, 3.462E+18, 2.960E+18, 2.530E+18, 2.163E+18,           mlat7380
     C 1.849E+18, 1.575E+18, 1.342E+18, 1.144E+18, 9.765E+17,           mlat7390
     C 8.337E+17, 5.640E+17, 3.830E+17, 2.524E+17, 1.761E+17,           mlat7400
     C 1.238E+17, 8.310E+16, 5.803E+16, 4.090E+16, 2.920E+16,           mlat7410
     C 2.136E+16, 1.181E+16, 6.426E+15, 3.386E+15, 1.723E+15,           mlat7420
     C 8.347E+14, 3.832E+14, 1.711E+14, 7.136E+13, 2.924E+13,           mlat7430
     C 1.189E+13, 5.033E+12, 2.144E+12, 9.688E+11, 5.114E+11/           mlat7440
                                                                        mlat7450
      DATA ANO        /                                                 mlat7460
     C  3.00E-04,  3.00E-04,  3.00E-04,  3.00E-04,  3.00E-04,           mlat7470
     C  3.00E-04,  3.00E-04,  3.00E-04,  3.00E-04,  3.00E-04,           mlat7480
     C  3.00E-04,  3.00E-04,  3.00E-04,  2.99E-04,  2.95E-04,           mlat7490
     C  2.83E-04,  2.68E-04,  2.52E-04,  2.40E-04,  2.44E-04,           mlat7500
     C  2.55E-04,  2.77E-04,  3.07E-04,  3.60E-04,  4.51E-04,           mlat7510
     C  6.85E-04,  1.28E-03,  2.45E-03,  4.53E-03,  7.14E-03,           mlat7520
     C  9.34E-03,  1.12E-02,  1.19E-02,  1.17E-02,  1.10E-02,           mlat7530
     C  1.03E-02,  1.01E-02,  1.01E-02,  1.03E-02,  1.15E-02,           mlat7540
     C  1.61E-02,  2.68E-02,  7.01E-02,  2.13E-01,  7.12E-01,           mlat7550
     C  2.08E+00,  4.50E+00,  7.98E+00,  1.00E+01,  1.00E+01/           mlat7560
      DATA SO2       /                                                  mlat7570
     C  3.00E-04,  2.74E-04,  2.36E-04,  1.90E-04,  1.46E-04,           mlat7580
     C  1.18E-04,  9.71E-05,  8.30E-05,  7.21E-05,  6.56E-05,           mlat7590
     C  6.08E-05,  5.79E-05,  5.60E-05,  5.59E-05,  5.64E-05,           mlat7600
     C  5.75E-05,  5.75E-05,  5.37E-05,  4.78E-05,  3.97E-05,           mlat7610
     C  3.19E-05,  2.67E-05,  2.28E-05,  2.07E-05,  1.90E-05,           mlat7620
     C  1.75E-05,  1.54E-05,  1.34E-05,  1.21E-05,  1.16E-05,           mlat7630
     C  1.21E-05,  1.36E-05,  1.65E-05,  2.10E-05,  2.77E-05,           mlat7640
     C  3.56E-05,  4.59E-05,  5.15E-05,  5.11E-05,  4.32E-05,           mlat7650
     C  2.83E-05,  1.33E-05,  5.56E-06,  2.24E-06,  8.96E-07,           mlat7660
     C  3.58E-07,  1.43E-07,  5.73E-08,  2.29E-08,  9.17E-09/           mlat7670
      DATA ANO2       /                                                 mlat7680
     C  2.30E-05,  2.30E-05,  2.30E-05,  2.30E-05,  2.30E-05,           mlat7690
     C  2.30E-05,  2.30E-05,  2.30E-05,  2.30E-05,  2.32E-05,           mlat7700
     C  2.38E-05,  2.62E-05,  3.15E-05,  4.45E-05,  7.48E-05,           mlat7710
     C  1.71E-04,  3.19E-04,  5.19E-04,  7.71E-04,  1.06E-03,           mlat7720
     C  1.39E-03,  1.76E-03,  2.16E-03,  2.58E-03,  3.06E-03,           mlat7730
     C  3.74E-03,  4.81E-03,  6.16E-03,  7.21E-03,  7.28E-03,           mlat7740
     C  6.26E-03,  4.03E-03,  2.17E-03,  1.15E-03,  6.66E-04,           mlat7750
     C  4.43E-04,  3.39E-04,  2.85E-04,  2.53E-04,  2.31E-04,           mlat7760
     C  2.15E-04,  2.02E-04,  1.92E-04,  1.83E-04,  1.76E-04,           mlat7770
     C  1.70E-04,  1.64E-04,  1.59E-04,  1.55E-04,  1.51E-04/           mlat7780
      DATA ANH3       /                                                 mlat7790
     C  5.00E-04,  5.00E-04,  4.63E-04,  3.80E-04,  2.88E-04,           mlat7800
     C  2.04E-04,  1.46E-04,  9.88E-05,  6.48E-05,  3.77E-05,           mlat7810
     C  2.03E-05,  1.09E-05,  6.30E-06,  3.12E-06,  1.11E-06,           mlat7820
     C  4.47E-07,  2.11E-07,  1.10E-07,  6.70E-08,  3.97E-08,           mlat7830
     C  2.41E-08,  1.92E-08,  1.72E-08,  1.59E-08,  1.44E-08,           mlat7840
     C  1.23E-08,  9.37E-09,  6.35E-09,  3.68E-09,  1.82E-09,           mlat7850
     C  9.26E-10,  2.94E-10,  8.72E-11,  2.98E-11,  1.30E-11,           mlat7860
     C  7.13E-12,  4.80E-12,  3.66E-12,  3.00E-12,  2.57E-12,           mlat7870
     C  2.27E-12,  2.04E-12,  1.85E-12,  1.71E-12,  1.59E-12,           mlat7880
     C  1.48E-12,  1.40E-12,  1.32E-12,  1.25E-12,  1.19E-12/           mlat7890
      DATA HNO3      /                                                  mlat7900
     C  5.00E-05,  5.96E-05,  6.93E-05,  7.91E-05,  8.87E-05,           mlat7910
     C  9.75E-05,  1.11E-04,  1.26E-04,  1.39E-04,  1.53E-04,           mlat7920
     C  1.74E-04,  2.02E-04,  2.41E-04,  2.76E-04,  3.33E-04,           mlat7930
     C  4.52E-04,  7.37E-04,  1.31E-03,  2.11E-03,  3.17E-03,           mlat7940
     C  4.20E-03,  4.94E-03,  5.46E-03,  5.74E-03,  5.84E-03,           mlat7950
     C  5.61E-03,  4.82E-03,  3.74E-03,  2.59E-03,  1.64E-03,           mlat7960
     C  9.68E-04,  5.33E-04,  2.52E-04,  1.21E-04,  7.70E-05,           mlat7970
     C  5.55E-05,  4.45E-05,  3.84E-05,  3.49E-05,  3.27E-05,           mlat7980
     C  3.12E-05,  3.01E-05,  2.92E-05,  2.84E-05,  2.78E-05,           mlat7990
     C  2.73E-05,  2.68E-05,  2.64E-05,  2.60E-05,  2.57E-05/           mlat8000
      DATA OH        /                                                  mlat8010
     C  4.40E-08,  4.40E-08,  4.40E-08,  4.40E-08,  4.40E-08,           mlat8020
     C  4.40E-08,  4.40E-08,  4.41E-08,  4.45E-08,  4.56E-08,           mlat8030
     C  4.68E-08,  4.80E-08,  4.94E-08,  5.19E-08,  5.65E-08,           mlat8040
     C  6.75E-08,  8.25E-08,  1.04E-07,  1.30E-07,  1.64E-07,           mlat8050
     C  2.16E-07,  3.40E-07,  5.09E-07,  7.59E-07,  1.16E-06,           mlat8060
     C  2.18E-06,  5.00E-06,  1.17E-05,  3.40E-05,  8.35E-05,           mlat8070
     C  1.70E-04,  2.85E-04,  4.06E-04,  5.11E-04,  5.79E-04,           mlat8080
     C  6.75E-04,  9.53E-04,  1.76E-03,  3.74E-03,  7.19E-03,           mlat8090
     C  1.12E-02,  1.13E-02,  6.10E-03,  1.51E-03,  2.42E-04,           mlat8100
     C  4.47E-05,  1.77E-05,  1.19E-05,  1.35E-05,  2.20E-05/           mlat8110
      DATA HF        /                                                  mlat8120
     C  1.00E-08,  1.00E-08,  1.23E-08,  1.97E-08,  3.18E-08,           mlat8130
     C  5.63E-08,  9.18E-08,  1.53E-07,  2.41E-07,  4.04E-07,           mlat8140
     C  6.57E-07,  1.20E-06,  1.96E-06,  3.12E-06,  4.62E-06,           mlat8150
     C  7.09E-06,  1.05E-05,  1.69E-05,  2.57E-05,  4.02E-05,           mlat8160
     C  5.77E-05,  7.77E-05,  9.90E-05,  1.23E-04,  1.50E-04,           mlat8170
     C  1.82E-04,  2.30E-04,  2.83E-04,  3.20E-04,  3.48E-04,           mlat8180
     C  3.72E-04,  3.95E-04,  4.10E-04,  4.21E-04,  4.24E-04,           mlat8190
     C  4.25E-04,  4.25E-04,  4.25E-04,  4.25E-04,  4.25E-04,           mlat8200
     C  4.25E-04,  4.25E-04,  4.25E-04,  4.25E-04,  4.25E-04,           mlat8210
     C  4.25E-04,  4.25E-04,  4.25E-04,  4.25E-04,  4.25E-04/           mlat8220
      DATA HCL       /                                                  mlat8230
     C  1.00E-03,  7.49E-04,  5.61E-04,  4.22E-04,  3.19E-04,           mlat8240
     C  2.39E-04,  1.79E-04,  1.32E-04,  9.96E-05,  7.48E-05,           mlat8250
     C  5.68E-05,  4.59E-05,  4.36E-05,  6.51E-05,  1.01E-04,           mlat8260
     C  1.63E-04,  2.37E-04,  3.13E-04,  3.85E-04,  4.42E-04,           mlat8270
     C  4.89E-04,  5.22E-04,  5.49E-04,  5.75E-04,  6.04E-04,           mlat8280
     C  6.51E-04,  7.51E-04,  9.88E-04,  1.28E-03,  1.57E-03,           mlat8290
     C  1.69E-03,  1.74E-03,  1.76E-03,  1.79E-03,  1.80E-03,           mlat8300
     C  1.80E-03,  1.80E-03,  1.80E-03,  1.80E-03,  1.80E-03,           mlat8310
     C  1.80E-03,  1.80E-03,  1.80E-03,  1.80E-03,  1.80E-03,           mlat8320
     C  1.80E-03,  1.80E-03,  1.80E-03,  1.80E-03,  1.80E-03/           mlat8330
      DATA HBR       /                                                  mlat8340
     C  1.70E-06,  1.70E-06,  1.70E-06,  1.70E-06,  1.70E-06,           mlat8350
     C  1.70E-06,  1.70E-06,  1.70E-06,  1.70E-06,  1.70E-06,           mlat8360
     C  1.70E-06,  1.70E-06,  1.70E-06,  1.70E-06,  1.70E-06,           mlat8370
     C  1.70E-06,  1.70E-06,  1.70E-06,  1.70E-06,  1.70E-06,           mlat8380
     C  1.70E-06,  1.70E-06,  1.70E-06,  1.70E-06,  1.70E-06,           mlat8390
     C  1.71E-06,  1.76E-06,  1.90E-06,  2.26E-06,  2.82E-06,           mlat8400
     C  3.69E-06,  4.91E-06,  6.13E-06,  6.85E-06,  7.08E-06,           mlat8410
     C  7.14E-06,  7.15E-06,  7.15E-06,  7.15E-06,  7.15E-06,           mlat8420
     C  7.15E-06,  7.15E-06,  7.15E-06,  7.15E-06,  7.15E-06,           mlat8430
     C  7.15E-06,  7.15E-06,  7.15E-06,  7.15E-06,  7.15E-06/           mlat8440
      DATA HI        /                                                  mlat8450
     C  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,           mlat8460
     C  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,           mlat8470
     C  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,           mlat8480
     C  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,           mlat8490
     C  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,           mlat8500
     C  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,           mlat8510
     C  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,           mlat8520
     C  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,           mlat8530
     C  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,           mlat8540
     C  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06,  3.00E-06/           mlat8550
      DATA CLO       /                                                  mlat8560
     C  1.00E-08,  1.00E-08,  1.00E-08,  1.00E-08,  1.00E-08,           mlat8570
     C  1.00E-08,  1.00E-08,  1.00E-08,  1.01E-08,  1.05E-08,           mlat8580
     C  1.21E-08,  1.87E-08,  3.18E-08,  5.61E-08,  9.99E-08,           mlat8590
     C  1.78E-07,  3.16E-07,  5.65E-07,  1.04E-06,  2.04E-06,           mlat8600
     C  4.64E-06,  8.15E-06,  1.07E-05,  1.52E-05,  2.24E-05,           mlat8610
     C  3.97E-05,  8.48E-05,  1.85E-04,  3.57E-04,  5.08E-04,           mlat8620
     C  6.07E-04,  5.95E-04,  4.33E-04,  2.51E-04,  1.56E-04,           mlat8630
     C  1.04E-04,  7.69E-05,  6.30E-05,  5.52E-05,  5.04E-05,           mlat8640
     C  4.72E-05,  4.49E-05,  4.30E-05,  4.16E-05,  4.03E-05,           mlat8650
     C  3.93E-05,  3.83E-05,  3.75E-05,  3.68E-05,  3.61E-05/           mlat8660
      DATA OCS       /                                                  mlat8670
     C  6.00E-04,  5.90E-04,  5.80E-04,  5.70E-04,  5.62E-04,           mlat8680
     C  5.55E-04,  5.48E-04,  5.40E-04,  5.32E-04,  5.25E-04,           mlat8690
     C  5.18E-04,  5.09E-04,  4.98E-04,  4.82E-04,  4.60E-04,           mlat8700
     C  4.26E-04,  3.88E-04,  3.48E-04,  3.09E-04,  2.74E-04,           mlat8710
     C  2.41E-04,  2.14E-04,  1.88E-04,  1.64E-04,  1.37E-04,           mlat8720
     C  1.08E-04,  6.70E-05,  2.96E-05,  1.21E-05,  4.31E-06,           mlat8730
     C  1.60E-06,  6.71E-07,  4.35E-07,  3.34E-07,  2.80E-07,           mlat8740
     C  2.47E-07,  2.28E-07,  2.16E-07,  2.08E-07,  2.03E-07,           mlat8750
     C  1.98E-07,  1.95E-07,  1.92E-07,  1.89E-07,  1.87E-07,           mlat8760
     C  1.85E-07,  1.83E-07,  1.81E-07,  1.80E-07,  1.78E-07/           mlat8770
      DATA H2CO      /                                                  mlat8780
     C  2.40E-03,  1.07E-03,  4.04E-04,  2.27E-04,  1.40E-04,           mlat8790
     C  1.00E-04,  7.44E-05,  6.04E-05,  5.01E-05,  4.22E-05,           mlat8800
     C  3.63E-05,  3.43E-05,  3.39E-05,  3.50E-05,  3.62E-05,           mlat8810
     C  3.62E-05,  3.58E-05,  3.50E-05,  3.42E-05,  3.39E-05,           mlat8820
     C  3.43E-05,  3.68E-05,  4.03E-05,  4.50E-05,  5.06E-05,           mlat8830
     C  5.82E-05,  7.21E-05,  8.73E-05,  1.01E-04,  1.11E-04,           mlat8840
     C  1.13E-04,  1.03E-04,  7.95E-05,  4.82E-05,  1.63E-05,           mlat8850
     C  5.10E-06,  2.00E-06,  1.05E-06,  6.86E-07,  5.14E-07,           mlat8860
     C  4.16E-07,  3.53E-07,  3.09E-07,  2.76E-07,  2.50E-07,           mlat8870
     C  2.30E-07,  2.13E-07,  1.98E-07,  1.86E-07,  1.75E-07/           mlat8880
      DATA HOCL      /                                                  mlat8890
     C  7.70E-06,  1.06E-05,  1.22E-05,  1.14E-05,  9.80E-06,           mlat8900
     C  8.01E-06,  6.42E-06,  5.42E-06,  4.70E-06,  4.41E-06,           mlat8910
     C  4.34E-06,  4.65E-06,  5.01E-06,  5.22E-06,  5.60E-06,           mlat8920
     C  6.86E-06,  8.77E-06,  1.20E-05,  1.63E-05,  2.26E-05,           mlat8930
     C  3.07E-05,  4.29E-05,  5.76E-05,  7.65E-05,  9.92E-05,           mlat8940
     C  1.31E-04,  1.84E-04,  2.45E-04,  2.96E-04,  3.21E-04,           mlat8950
     C  3.04E-04,  2.48E-04,  1.64E-04,  9.74E-05,  4.92E-05,           mlat8960
     C  2.53E-05,  1.50E-05,  1.05E-05,  8.34E-06,  7.11E-06,           mlat8970
     C  6.33E-06,  5.78E-06,  5.37E-06,  5.05E-06,  4.78E-06,           mlat8980
     C  4.56E-06,  4.37E-06,  4.21E-06,  4.06E-06,  3.93E-06/           mlat8990
      DATA AN2        /                                                 mlat9000
     C  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,           mlat9010
     C  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,           mlat9020
     C  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,           mlat9030
     C  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,           mlat9040
     C  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,           mlat9050
     C  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,           mlat9060
     C  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,           mlat9070
     C  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,  7.81E+05,           mlat9080
     C  7.81E+05,  7.81E+05,  7.81E+05,  7.80E+05,  7.79E+05,           mlat9090
     C  7.77E+05,  7.74E+05,  7.70E+05,  7.65E+05,  7.60E+05/           mlat9100
      DATA HCN       /                                                  mlat9110
     C  1.70E-04,  1.65E-04,  1.63E-04,  1.61E-04,  1.60E-04,           mlat9120
     C  1.60E-04,  1.60E-04,  1.60E-04,  1.60E-04,  1.60E-04,           mlat9130
     C  1.60E-04,  1.60E-04,  1.60E-04,  1.59E-04,  1.57E-04,           mlat9140
     C  1.55E-04,  1.52E-04,  1.49E-04,  1.45E-04,  1.41E-04,           mlat9150
     C  1.37E-04,  1.34E-04,  1.30E-04,  1.25E-04,  1.19E-04,           mlat9160
     C  1.13E-04,  1.05E-04,  9.73E-05,  9.04E-05,  8.46E-05,           mlat9170
     C  8.02E-05,  7.63E-05,  7.30E-05,  7.00E-05,  6.70E-05,           mlat9180
     C  6.43E-05,  6.21E-05,  6.02E-05,  5.88E-05,  5.75E-05,           mlat9190
     C  5.62E-05,  5.50E-05,  5.37E-05,  5.25E-05,  5.12E-05,           mlat9200
     C  5.00E-05,  4.87E-05,  4.75E-05,  4.62E-05,  4.50E-05/           mlat9210
      DATA CH3CL     /                                                  mlat9220
     C  7.00E-04,  6.70E-04,  6.43E-04,  6.22E-04,  6.07E-04,           mlat9230
     C  6.02E-04,  6.00E-04,  6.00E-04,  5.98E-04,  5.94E-04,           mlat9240
     C  5.88E-04,  5.79E-04,  5.66E-04,  5.48E-04,  5.28E-04,           mlat9250
     C  5.03E-04,  4.77E-04,  4.49E-04,  4.21E-04,  3.95E-04,           mlat9260
     C  3.69E-04,  3.43E-04,  3.17E-04,  2.86E-04,  2.48E-04,           mlat9270
     C  1.91E-04,  1.10E-04,  4.72E-05,  1.79E-05,  7.35E-06,           mlat9280
     C  3.03E-06,  1.32E-06,  8.69E-07,  6.68E-07,  5.60E-07,           mlat9290
     C  4.94E-07,  4.56E-07,  4.32E-07,  4.17E-07,  4.05E-07,           mlat9300
     C  3.96E-07,  3.89E-07,  3.83E-07,  3.78E-07,  3.73E-07,           mlat9310
     C  3.69E-07,  3.66E-07,  3.62E-07,  3.59E-07,  3.56E-07/           mlat9320
      DATA H2O2      /                                                  mlat9330
     C  2.00E-04,  1.95E-04,  1.92E-04,  1.89E-04,  1.84E-04,           mlat9340
     C  1.77E-04,  1.66E-04,  1.49E-04,  1.23E-04,  9.09E-05,           mlat9350
     C  5.79E-05,  3.43E-05,  1.95E-05,  1.08E-05,  6.59E-06,           mlat9360
     C  4.20E-06,  2.94E-06,  2.30E-06,  2.24E-06,  2.68E-06,           mlat9370
     C  3.68E-06,  5.62E-06,  1.03E-05,  1.97E-05,  3.70E-05,           mlat9380
     C  6.20E-05,  1.03E-04,  1.36E-04,  1.36E-04,  1.13E-04,           mlat9390
     C  8.51E-05,  6.37E-05,  5.17E-05,  4.44E-05,  3.80E-05,           mlat9400
     C  3.48E-05,  3.62E-05,  5.25E-05,  1.26E-04,  3.77E-04,           mlat9410
     C  1.12E-03,  2.00E-03,  1.68E-03,  4.31E-04,  4.98E-05,           mlat9420
     C  6.76E-06,  8.38E-07,  9.56E-08,  1.00E-08,  1.00E-09/           mlat9430
      DATA C2H2      /                                                  mlat9440
     C  3.00E-04,  1.72E-04,  9.57E-05,  6.74E-05,  5.07E-05,           mlat9450
     C  3.99E-05,  3.19E-05,  2.80E-05,  2.55E-05,  2.40E-05,           mlat9460
     C  2.27E-05,  2.08E-05,  1.76E-05,  1.23E-05,  7.32E-06,           mlat9470
     C  4.52E-06,  2.59E-06,  1.55E-06,  8.63E-07,  5.30E-07,           mlat9480
     C  3.10E-07,  1.89E-07,  1.04E-07,  5.75E-08,  2.23E-08,           mlat9490
     C  8.51E-09,  4.09E-09,  2.52E-09,  1.86E-09,  1.52E-09,           mlat9500
     C  1.32E-09,  1.18E-09,  1.08E-09,  9.97E-10,  9.34E-10,           mlat9510
     C  8.83E-10,  8.43E-10,  8.10E-10,  7.83E-10,  7.60E-10,           mlat9520
     C  7.40E-10,  7.23E-10,  7.07E-10,  6.94E-10,  6.81E-10,           mlat9530
     C  6.70E-10,  6.59E-10,  6.49E-10,  6.40E-10,  6.32E-10/           mlat9540
      DATA C2H6      /                                                  mlat9550
     C  2.00E-03,  2.00E-03,  2.00E-03,  2.00E-03,  1.98E-03,           mlat9560
     C  1.95E-03,  1.90E-03,  1.85E-03,  1.79E-03,  1.72E-03,           mlat9570
     C  1.58E-03,  1.30E-03,  9.86E-04,  7.22E-04,  4.96E-04,           mlat9580
     C  3.35E-04,  2.14E-04,  1.49E-04,  1.05E-04,  7.96E-05,           mlat9590
     C  6.01E-05,  4.57E-05,  3.40E-05,  2.60E-05,  1.89E-05,           mlat9600
     C  1.22E-05,  5.74E-06,  2.14E-06,  8.49E-07,  3.42E-07,           mlat9610
     C  1.34E-07,  5.39E-08,  2.25E-08,  1.04E-08,  6.57E-09,           mlat9620
     C  4.74E-09,  3.79E-09,  3.28E-09,  2.98E-09,  2.79E-09,           mlat9630
     C  2.66E-09,  2.56E-09,  2.49E-09,  2.43E-09,  2.37E-09,           mlat9640
     C  2.33E-09,  2.29E-09,  2.25E-09,  2.22E-09,  2.19E-09/           mlat9650
      DATA PH3       /                                                  mlat9660
     C  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,           mlat9670
     C  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,           mlat9680
     C  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,           mlat9690
     C  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,           mlat9700
     C  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,           mlat9710
     C  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,           mlat9720
     C  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,           mlat9730
     C  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,           mlat9740
     C  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,           mlat9750
     C  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14,  1.00E-14/           mlat9760
      END                                                               mlat9770
        subroutine msrad(dis,uang,nstr,iv,v,isourc,iday,
     2     anglem,s0cms,t0cms)
cjd3  SUBROUTINE MSRAD(IV,V,ISOURC,IDAY,ANGLEM)
C
C         (1) SETS UP OPTICAL PROPERTIES PROFILES FOR VERTICAL
C             PATH
C         (2) CALLS FLXADD WHICH RETURNS MS SOURCE FUNCTION, J,
C             FOR PATH
C         (3) EVALUATES PATH INTEGRAL OF SOURCE FUNCTION, I.E.
C             MS RADIANCE CONTRIBUTION
C
C                                 A.E.R. 1986
      logical modtrn
      COMMON/CARD1/MODEL,ITYPE,IEMSCT,M1,M2,M3,IM,NOPRNT,TBOUND,SALB,
     1  MODTRN
      COMMON/CARD2/IHAZE,ISEASN,IVULCN,ICSTL,ICLD,IVSA,VIS,WSS,WHH,
     1    RAINRT
      COMMON/CNTRL/KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT
      INCLUDE 'parameter.list'
      COMMON RELHUM(LAYDIM),HSTOR(LAYDIM),ICH(4),VH(17),TX(65),W(65)  
      COMMON IMSMX,WPATH(LAYTHR,65),TBBY(LAYTHR),PATM(LAYTHR),NSPEC,   
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX2(47),AWCCON(5)  
      COMMON/AER/XX1,XX2,XX3,XX4,XX5,
     1  YY1,YY2,YY3,YY4,YY5,ZZ1,ZZ2,ZZ3,ZZ4,ZZ5
      COMMON/TRAN/TMOLS(LAYDIM),TAERS(LAYDIM),TCONT(LAYDIM),
     1  DCONT(LAYTWO)
      COMMON/MSRD/TLE(LAYDIM),COSBAR(LAYDIM),OMEGA0(LAYTWO),
     1  UPF(10,LAYDIM),DNF(10,LAYDIM),TAER(LAYDIM),ASYIK(LAYTWO),
     2  ASYDM(LAYTWO),STRN(0:LAYDIM),DMOLS(LAYTWO),DSTRN(0:LAYTWO),
     3  FDNSRT,FDNTRT,TAUT(LAYDIM),UMF(LAYDIM),DMF(LAYDIM),
     4  UMFS(LAYDIM),DMFS(LAYDIM) 
      COMMON/CNSTNS/PI,CA,DEG,GCAIR,BIGNUM,BIGEXP
cjd3 v

c*********************************************************
c   variables and common blocks for disort insert - NORTH
         COMMON/CARD3/ H1,H2,ANGLE,RANGE,BETA,RE,LEN
         COMMON/CARD4/IV1,IV2,IDV,IFWHM
cj v changing to parameterization: 34=laydim, 68=laytwo, 102=laythr, 64>65 in wpaths
c          
      COMMON/SOLS/AH1(laytwo),ARH(laytwo),                                      tras 260
     1  WPATHS(laythr,65),PA(laytwo),PR(laytwo),ATHETA(laydim+1),
     x  ADBETA(laydim+1),LJ(laytwo+1),                                          tras 270
     2  JTURN,ANGSUN,CSZEN(laytwo),TBBYS(laythr,12),PATMS(laythr,12)              tras 280
         
       parameter(maxulv=laydim,maxumu=1,maxphi=1,maxcly=laydim,
     x  maxcmu=16 )
          
c      COMMON/SOLS/AH1(68),ARH(68),                                      tras 260
c     1  WPATHS(102,63),PA(68),PR(68),ATHETA(35),ADBETA(35),LJ(69),      tras 270
c     2  JTURN,ANGSUN,CSZEN(68),TBBYS(102,12),PATMS(102,12)              tras 280
c         
c       parameter(maxulv=34,maxumu=1,maxphi=1,maxcly=34,maxcmu=16 )
cj 
       character header*127
       logical dis,deltam,lamber,plank,onlyfl,prnt(7),usrang,
     1 msflag,usrtau
       integer ibcnd,nlyr,numu,nstr,nphi,ntau
       double precision
     $       accur,albedo,btemp,dtauc(maxcly),fbeam,fisot,
     1        hl(0:maxcmu),phi(maxphi),pmom(0:maxcmu,maxcly),
     2        phi0,ssalb(maxcly),temper(0:maxcly),temis,ttemp,
     3        wvnmlo,wvnmhi,umu(maxumu),umu0,utau(maxulv),
     4        rfldir(maxulv),rfldn(maxulv),flup(maxulv),
     5        uavg(maxulv),dfdt(maxulv),u0u(maxumu,maxulv),
     6        uu(maxumu,maxulv,maxphi),albmed(maxumu),
     7        trnmed(maxumu),s0cms(maxumu,maxulv),
     8        wn0,uang,t0cms(maxumu,maxulv)
c*********************************************************

cjd3 
      IKMAX=IKMAX+1
      IKM=IKMAX-1
C
C     FOR H1 < H2, CALCULATE DOWNWARD RADIANCE
      DO 10 IK=1,IKM-1
C
C         AEROSOL PATH FOR EACH LAYER (NOT CUMULATIVE)
C
C         SOLAR TRANSMITTANCE FROM TOP OF ATMOSPHERE
          STRN(IK-1)=DSTRN(IKMAX-IK)
          IF(IHAZE.GT.0)THEN
C
C             CALCULATE AEROSOL ASYMMETRY FACTOR FOR GIVEN LAYER
              IF(asydm(ik).LE.0.)THEN
                  COSBAR(IKM-IK)=0.
              ELSE
                  COSBAR(IKM-IK)=ASYIK(IK)/asydm(ik)
              ENDIF
          ENDIF
C
C         CONVERT TAUT FROM CUMULATIVE TRANSMITTANCE TO LAYER OPTICAL
C         DEPTH for modtran runs
          if(modtrn)then
              IF(TAUT(IK).GT.0.AND.TAUT(IK+1).GT.0.)THEN
                  TAUT(IK)=-LOG(TAUT(IK)/TAUT(IK+1))
              ELSE
                  TAUT(IK)=BIGEXP
              ENDIF
              IF(TAUT(IK).LT.0.)TAUT(IK)=0.
          endif
C
C         TAER, TAERS AND TMOLS ARE TOTAL AEROSOL, AEROSOL SCATTERING
C         AND MOLECULAR SCATTERING OPTICAL THICKNESS FOR GIVEN LAYER
          TMOLS(IKM-IK)=DMOLS(IK+1)-DMOLS(IK)
C
C         WEIGHT ASYMMETRY FACTOR
          IF(TAERS(IKM-IK).LE.0.)THEN
              COSBAR(IKM-IK)=0.
          ELSE
              COSBAR(IKM-IK)=COSBAR(IKM-IK)*TAERS(IKM-IK)/
     1          (TAERS(IKM-IK)+TMOLS(IKM-IK))
          ENDIF
C
C         TCONT IS MOLECULAR CONTINUUM OPTICAL THICKNESS FOR GIVEN LAYER
   10 TCONT(IKM-IK)=DCONT(IK+1)-DCONT(IK)
      IF(IHAZE.GT.0)THEN
C
C         CALCULATE ASYMMETRY FACTOR FOR BOTTOM OF ATMOSPHERE
cj v change the 1 to ik.
C         CALCULATE ASYMMETRY FACTOR FOR BOTTOM OF ATMOSPHERE
cj          IF(asydm(1).LE.0.)THEN
cj              COSBAR(IKM)=0.
cj          ELSE
clex:1-18-93:COSBAR(IKM)=ASYIK(ik)/asydm(1) replaced by
cj             COSBAR(IKM)=ASYIK(1)/asydm(1)
cj          ENDIF
c
C         CALCULATE ASYMMETRY FACTOR FOR BOTTOM OF ATMOSPHERE
          IF(asydm(ik).LE.0.)THEN
              COSBAR(IKM)=0.
          ELSE
clex:1-18-93:COSBAR(IKM)=ASYIK(ik)/asydm(1) replaced by
             COSBAR(IKM)=ASYIK(ik)/asydm(ik)
          ENDIF
cj ^
      ENDIF
      STRN(IKM-1)=DSTRN(IKMAX-IKM)
      STRN(IKM)=DSTRN(0)
      TMOLS(IKM)=DMOLS(1)
      if(modtrn)then
          IF(TAUT(IKM).GT.0.)THEN
              TAUT(IKM)=-LOG(TAUT(IKM))
          ELSE
              TAUT(IKM)=BIGEXP
          ENDIF
          IF(TAUT(IKM).LT.0.)TAUT(IKM)=0.
      endif
C
C     WEIGHT ASYMMETRY FACTOR
      IF(TAERS(IKM).LE.0.)THEN
          COSBAR(IKM)=0.
      ELSE
          COSBAR(IKM)=COSBAR(IKM)*TAERS(IKM)/(TAERS(IKM)+TMOLS(IKM))
      ENDIF
      TCONT(IKM)=DCONT(1)
C
C   FOR H1 > H2, CALCULATE UPWARD RADIANCE
C
C   CALCULATE SOLAR INTENSITY AT TOP OF ATMOSPHERE
      S0=0.
      IF(IEMSCT.EQ.2)CALL SOURCE(V,ISOURC,IDAY,ANGLEM,S0)
C
cjd3 v adding the section that calls DISORT
               
           if (dis) then
c*******************************************************************************
c          insert for call to disort - NORTH
c            if dis=.true. then disort will be called.  
c            if dis=.false. then code bmflux or fluxadd used.
             nlyrs=ml-1   
c-----------------------------------------------------
c         set up temperature profile for
               do 101 n=0,nlyrs
                   temper(n)=tle(n+1)
101            continue   
c------------------------------------------------------ 
c         clean out arrays containing source functions
          do 102 n=1,nlyrs
                s0cms(1,n)=0.0
                t0cms(1,n)=0.0
102       continue
c------------------------------------------------------
c         calculate single scattering albedo & optical depth
          do 103 n=1,nlyrs
              dtauc(n)=taut(n)+taer(n)+tcont(n)+tmols(n)
              ssalb(n)=0.0
              if(taers(n).lt.0) taers(n)=0.0
              if(dtauc(n).gt.0.)ssalb(n)=(taers(n)+tmols(n))/dtauc(n)
               if (ssalb(n).gt.1.) ssalb(n)=1.
              omega0(n)=ssalb(n)
103       continue
c-------------------------------------------------
c         H-G function moments of Legendre Polynomials  
               do 100 kw=1,nlyrs
                   pmom(0,kw)=1.0
                   do 100 kwi=1,nstr
                       pmom(kwi,kw)=cosbar(kw)**kwi
100            continue
c           other disort variables defined
            msflag=.true.
            wn0=v
            wvnmlo=v-.5*idv
            wvnmhi=v+.5*idv
            ntau=nlyrs+1
              utau(1)=0.0
            do 105 lc=1,nlyrs
                utau(lc+1)=utau(lc)+dtauc(lc)
105         continue
            numu=1
            umu(1)=cos((180.0+uang)*pi/180.)
            phi0=0.0
            btemp=temper(nlyrs)
            ttemp=temper(0)
            plank=.true. 
            fbeam=s0
            ibcnd=0
            umu0=CSZEN(1)
            albedo=salb
            lamber=.true.
            temis=0.0
c             onlyfl=.f. for using disort for radiative transfer
c             computations, and it should be left as onlyfl=.t.
c             for the merged package.
            
            onlyfl=.true.
            
            accur=0.0  
            header='*'   
            deltam=.true.
            fisot=0.0
            usrtau=.false.
            usrang=.true.
            nphi=1
            phi(1)=0
c------------------------------------------------------
c       set print statements in disort
            do 106 ks=1,7
                 prnt(ks)=.false.
106         continue 
            
c-------------------------------------------------------
        call disort(nlyrs,dtauc,ssalb,pmom,temper,wvnmlo,wvnmhi,
     1      usrtau, ntau, utau, nstr, usrang, numu, umu, nphi,
     2      phi, ibcnd, fbeam, umu0, phi0, fisot, lamber, albedo,
     3      hl, btemp, ttemp, temis, deltam, plank, onlyfl,
     4      accur, prnt, header, maxcly, maxulv, maxumu, maxcmu,
     5      maxphi, rfldir, rfldn, flup, dfdt, uavg, uu, u0u,
     6      albmed,trnmed,msflag,wn0,s0cms,t0cms,fdnsrt,fdntrt)
c------------------------------------------------------------
        return
       endif
cjd3 ^
C   CALL FLUX ADDING SUBROUTINE
      IF(modtrn)THEN
          CALL BMFLUX(IV,S0)
      ELSE
          CALL FLXADD(IV,IKMAX,S0)
      ENDIF
      RETURN
      END
      SUBROUTINE NEWH2(SH1,SH2,SANGLE,SRANGE,SBETA,LEN,SHTAN,SPHI)      newh 100
C                                                                       newh 110
C     THIS NEW ROUTINE DETERMINES H2, BETA, TANGENT HEIGHT AND LEN.     newh 120
C     MODTRAN ALREADY CAN DO THIS USING OTHER ROUTINES.                 newh 130
C     HOWEVER, THIS IS MORE MODULAR AND TRANSPARENT THAN                newh 140
C     WHAT IS ALREADY THERE.                                            newh 150
C                                                                       newh 160
C     INPUTS ARE: H1, ZENTIH ANGLE (ANGLE) AND RANGE.                   newh 170
C     LEN = 1 IF THE PATH GOES THROUGH HTAN.                            newh 180
C                                                                       newh 190
C     PREFIX S IN THE ARGUMENT LIST DENOTES THAT THE                    newh 200
C     INPUT  AND OUTPUT VARIABLES ARE IN SINGLE PRECISION.              newh 210
C     ALL CLACULATIONS ARE DONE IN DOUBLE PRECISION.                    newh 220
C     H1, H2, ANGLE, RANGE, BETA, HTAN, RX, CPATH, DEG, RATIO,          newh 230
C     & PHI ARE LOCAL TO THE THE ROUTINE AND ARE IN DOUBLE PRECISION.   newh 240
C                                                                       newh 250
      COMMON /PARMTR/ RE,DELTAS,ZMAX,IMAX,IMOD,IBMAX,IPATH              newh 260
C     SINGLE PRECISION INPUT VARIABLES                                  newh 270
      REAL SH1, SH2, SANGLE,SRANGE,SBETA,SHTAN,SPHI                     newh 280
      REAL RE, DELTAS, ZMAX                                             newh 290
      DOUBLE PRECISION H1, H2, ANGLE, RANGE, BETA, HTAN, RX,            newh 300
     $     CPATH,DEG,RATIO,PHI                                          newh 310
      INTEGER LEN, IMAX, IMOD, IBMAX,IPATH                              newh 320
      DATA DEG/57.2957795131/                                           newh 330
C                                                                       newh 340
C     MAKE INPUTS DOUBLE PRECISION                                      newh 350
      H1 = SH1                                                          newh 360
      H2 = SH2                                                          newh 370
      ANGLE = SANGLE                                                    newh 380
      RANGE = SRANGE                                                    newh 390
      BETA = SBETA                                                      newh 400
      HTAN = SHTAN                                                      newh 410
C                                                                       newh 420
C     COMPUTE CPATH OR PATH CONSTANT                                    newh 430
      CALL IRFXN(H1, RX, RATIO)                                         newh 440
      CPATH = RX*SIN(ANGLE/DEG)*(H1+RE)                                 newh 450
      CALL TANHT(CPATH,HTAN,H1)                                         newh 460
C                                                                       newh 470
C     FIND H2, BETA AND LEN                                             newh 480
      CALL FNDPTH(CPATH,H1, HTAN, H2, RANGE, BETA, LEN,ANGLE,           newh 490
     $     PHI)                                                         newh 500
      SH1 = H1                                                          newh 510
      SH2 = H2                                                          newh 520
      SANGLE = ANGLE                                                    newh 530
      SPHI = PHI                                                        newh 540
      SRANGE = RANGE                                                    newh 550
      SBETA = BETA                                                      newh 560
      SHTAN = HTAN                                                      newh 570
C     IF (ANGLE .LT. 90.0) SHTAN CARRIES HMIN NOT HTAN                  newh 580
      IF (ANGLE .LE. 90.0) SHTAN = MIN(H1,H2)                           newh 590
      END                                                               newh 600
      SUBROUTINE NO2XS(V,CROSS)
      INTEGER NMAX
      PARAMETER (NMAX=7176)
      dimension CRSNO2(7176)
C     BEGINNING AND ENDING FREQUENCIES (CM-1):    14095  49970
C     BIN WIDTH AND TOTAL ENTRIES:        5   7176
C     CROSS-SECTIONS IN CM^2 TIMES 2.6868E19
      DATA VBEG, VEND, VINCR /14095.0, 49970.0, 5.0/
      DATA (CRSNO2(I),I=    1,  100)/
     1     2.187E-01, 1.535E-01, 1.026E-01, 9.893E-02, 1.009E-01,
     2     1.171E-01, 1.353E-01, 2.303E-01, 2.137E-01, 2.268E-01,
     3     1.248E-01, 1.596E-01, 1.773E-01, 1.380E-01, 1.437E-01,
     4     1.078E-01, 1.385E-01, 1.747E-01, 1.659E-01, 1.435E-01,
     5     1.122E-01, 1.387E-01, 1.095E-01, 9.848E-02, 1.292E-01,
     6     1.335E-01, 4.234E-02, 8.134E-02, 7.137E-02, 1.073E-01,
     7     1.103E-01, 7.682E-02, 7.669E-02, 6.166E-02, 5.656E-02,
     8     1.016E-01, 9.995E-02, 1.172E-01, 9.483E-02, 9.305E-02,
     9     3.115E-02, 2.633E-02, 1.053E-01, 6.741E-02, 1.517E-02,
     $     6.836E-02, 9.529E-02, 5.382E-02, 1.689E-02, 4.787E-02,
     1     4.899E-02, 3.848E-02, 6.239E-02, 5.901E-02, 4.349E-02,
     2     6.058E-02, 7.849E-02, 6.572E-02, 2.851E-02, 8.506E-02,
     3     7.100E-02, 8.237E-02, 5.719E-02, 2.472E-02, 6.307E-02,
     4     1.950E-02, 2.154E-02, 4.849E-02, 4.495E-02, 6.848E-02,
     5     6.195E-02, 6.561E-02, 6.729E-02, 9.270E-02, 5.591E-02,
     6     1.049E-01, 5.964E-02, 1.618E-02, 6.340E-02, 5.316E-02,
     7     9.263E-02, 8.107E-02, 4.596E-02, 6.632E-02, 4.788E-02,
     8     7.965E-02, 6.563E-02, 9.042E-02, 2.667E-02, 7.322E-02,
     9     8.035E-02, 6.220E-02, 7.409E-02, 9.548E-02, 1.411E-01,
     $     1.783E-01, 1.590E-01, 1.400E-01, 1.592E-01, 1.303E-01/
      DATA (CRSNO2(I),I=  101,  200)/
     1     1.072E-01, 1.117E-01, 1.317E-01, 1.095E-01, 1.135E-01,
     2     8.546E-02, 8.870E-02, 8.289E-02, 1.199E-01, 1.119E-01,
     3     9.036E-02, 7.293E-02, 1.189E-01, 1.269E-01, 1.229E-01,
     4     1.214E-01, 1.140E-01, 1.188E-01, 1.422E-01, 1.450E-01,
     5     1.425E-01, 1.330E-01, 1.418E-01, 1.653E-01, 1.809E-01,
     6     1.507E-01, 1.584E-01, 1.759E-01, 1.397E-01, 1.066E-01,
     7     1.864E-01, 1.380E-01, 1.636E-01, 2.350E-01, 2.023E-01,
     8     2.157E-01, 2.447E-01, 2.452E-01, 2.136E-01, 1.957E-01,
     9     2.157E-01, 2.314E-01, 1.889E-01, 2.134E-01, 2.080E-01,
     $     1.913E-01, 2.290E-01, 2.312E-01, 2.600E-01, 2.212E-01,
     1     2.960E-01, 3.402E-01, 2.436E-01, 2.260E-01, 2.137E-01,
     2     2.442E-01, 2.169E-01, 2.113E-01, 2.094E-01, 2.238E-01,
     3     2.362E-01, 2.416E-01, 1.995E-01, 1.545E-01, 2.270E-01,
     4     2.650E-01, 2.675E-01, 2.543E-01, 2.536E-01, 2.176E-01,
     5     2.526E-01, 2.417E-01, 1.867E-01, 1.958E-01, 2.441E-01,
     6     2.642E-01, 2.180E-01, 1.795E-01, 1.886E-01, 2.050E-01,
     7     2.105E-01, 1.977E-01, 1.911E-01, 1.807E-01, 1.883E-01,
     8     1.460E-01, 1.500E-01, 1.401E-01, 1.137E-01, 1.143E-01,
     9     1.275E-01, 1.160E-01, 6.466E-02, 6.873E-02, 8.635E-02,
     $     8.132E-02, 8.656E-02, 8.974E-02, 5.564E-02, 9.207E-02/
      DATA (CRSNO2(I),I=  201,  300)/
     1     5.698E-02, 8.596E-02, 8.385E-02, 7.201E-02, 8.996E-02,
     2     8.037E-02, 1.062E-01, 1.149E-01, 1.275E-01, 1.160E-01,
     3     1.278E-01, 1.654E-01, 1.604E-01, 1.768E-01, 1.710E-01,
     4     1.415E-01, 1.623E-01, 1.525E-01, 1.101E-01, 1.388E-01,
     5     1.453E-01, 1.373E-01, 1.513E-01, 1.402E-01, 1.421E-01,
     6     1.798E-01, 1.908E-01, 1.804E-01, 2.035E-01, 2.246E-01,
     7     2.642E-01, 2.312E-01, 1.804E-01, 1.974E-01, 2.192E-01,
     8     2.307E-01, 2.451E-01, 2.191E-01, 2.141E-01, 2.224E-01,
     9     2.117E-01, 2.291E-01, 2.145E-01, 2.413E-01, 2.427E-01,
     $     2.583E-01, 2.752E-01, 2.603E-01, 2.758E-01, 2.699E-01,
     1     2.694E-01, 3.187E-01, 2.739E-01, 3.005E-01, 2.903E-01,
     2     2.874E-01, 3.192E-01, 2.869E-01, 2.563E-01, 2.151E-01,
     3     2.361E-01, 2.355E-01, 2.521E-01, 2.581E-01, 2.343E-01,
     4     2.577E-01, 3.285E-01, 4.370E-01, 5.318E-01, 5.971E-01,
     5     5.877E-01, 5.574E-01, 6.636E-01, 6.242E-01, 6.256E-01,
     6     5.217E-01, 5.258E-01, 4.251E-01, 3.790E-01, 3.875E-01,
     7     3.638E-01, 3.854E-01, 3.853E-01, 3.184E-01, 2.876E-01,
     8     2.998E-01, 3.462E-01, 3.562E-01, 4.263E-01, 4.864E-01,
     9     4.874E-01, 4.260E-01, 4.665E-01, 5.070E-01, 5.525E-01,
     $     4.845E-01, 4.399E-01, 4.475E-01, 3.702E-01, 3.335E-01/
      DATA (CRSNO2(I),I=  301,  400)/
     1     3.630E-01, 3.573E-01, 3.547E-01, 3.586E-01, 3.407E-01,
     2     2.952E-01, 3.584E-01, 3.750E-01, 3.224E-01, 2.803E-01,
     3     3.131E-01, 3.176E-01, 3.202E-01, 2.984E-01, 2.833E-01,
     4     3.359E-01, 3.797E-01, 3.837E-01, 3.446E-01, 3.393E-01,
     5     3.065E-01, 3.373E-01, 3.362E-01, 3.184E-01, 3.393E-01,
     6     3.552E-01, 2.941E-01, 2.942E-01, 3.247E-01, 2.996E-01,
     7     2.945E-01, 2.596E-01, 2.848E-01, 3.371E-01, 3.528E-01,
     8     3.566E-01, 3.011E-01, 3.141E-01, 3.192E-01, 3.016E-01,
     9     2.604E-01, 2.473E-01, 2.559E-01, 2.749E-01, 3.137E-01,
     $     3.060E-01, 3.060E-01, 2.981E-01, 2.942E-01, 3.185E-01,
     1     2.743E-01, 2.655E-01, 2.560E-01, 2.385E-01, 2.140E-01,
     2     2.139E-01, 2.292E-01, 2.494E-01, 2.529E-01, 2.813E-01,
     3     3.166E-01, 3.098E-01, 3.041E-01, 3.016E-01, 2.854E-01,
     4     2.894E-01, 3.037E-01, 3.054E-01, 3.403E-01, 3.381E-01,
     5     3.557E-01, 3.781E-01, 3.955E-01, 4.319E-01, 3.807E-01,
     6     4.183E-01, 4.364E-01, 4.759E-01, 4.681E-01, 5.019E-01,
     7     5.265E-01, 5.194E-01, 4.907E-01, 4.968E-01, 5.157E-01,
     8     5.080E-01, 4.880E-01, 5.362E-01, 5.480E-01, 5.563E-01,
     9     5.373E-01, 5.098E-01, 4.643E-01, 5.135E-01, 5.002E-01,
     $     4.607E-01, 4.266E-01, 4.658E-01, 5.009E-01, 5.327E-01/
      DATA (CRSNO2(I),I=  401,  500)/
     1     6.159E-01, 6.867E-01, 7.306E-01, 8.251E-01, 8.224E-01,
     2     8.078E-01, 7.311E-01, 5.973E-01, 6.369E-01, 6.258E-01,
     3     5.962E-01, 5.288E-01, 5.197E-01, 4.947E-01, 4.773E-01,
     4     5.202E-01, 6.037E-01, 6.965E-01, 8.256E-01, 8.396E-01,
     5     8.105E-01, 8.427E-01, 8.972E-01, 1.018E+00, 1.079E+00,
     6     9.813E-01, 8.836E-01, 8.620E-01, 7.896E-01, 8.197E-01,
     7     7.303E-01, 6.593E-01, 6.441E-01, 6.284E-01, 6.332E-01,
     8     6.962E-01, 7.586E-01, 8.364E-01, 7.626E-01, 8.953E-01,
     9     8.087E-01, 7.704E-01, 7.731E-01, 8.281E-01, 1.077E+00,
     $     1.465E+00, 1.400E+00, 1.147E+00, 1.255E+00, 1.027E+00,
     1     9.299E-01, 9.742E-01, 9.467E-01, 7.526E-01, 7.007E-01,
     2     5.869E-01, 4.717E-01, 4.595E-01, 4.551E-01, 4.529E-01,
     3     4.376E-01, 4.354E-01, 4.290E-01, 4.320E-01, 4.517E-01,
     4     4.401E-01, 4.247E-01, 4.522E-01, 4.560E-01, 4.214E-01,
     5     4.915E-01, 5.073E-01, 4.681E-01, 4.076E-01, 3.982E-01,
     6     3.783E-01, 4.263E-01, 4.383E-01, 4.120E-01, 3.722E-01,
     7     3.408E-01, 3.674E-01, 3.965E-01, 4.863E-01, 4.731E-01,
     8     4.782E-01, 4.316E-01, 3.773E-01, 3.328E-01, 3.263E-01,
     9     3.648E-01, 3.466E-01, 3.693E-01, 3.975E-01, 4.162E-01,
     $     4.107E-01, 4.491E-01, 5.405E-01, 6.249E-01, 6.830E-01/
      DATA (CRSNO2(I),I=  501,  600)/
     1     7.251E-01, 8.205E-01, 8.394E-01, 8.702E-01, 1.031E+00,
     2     9.676E-01, 1.054E+00, 1.130E+00, 1.059E+00, 1.041E+00,
     3     1.113E+00, 1.136E+00, 1.173E+00, 1.121E+00, 1.103E+00,
     4     1.140E+00, 1.057E+00, 1.004E+00, 8.658E-01, 8.865E-01,
     5     9.201E-01, 9.069E-01, 8.252E-01, 7.949E-01, 7.161E-01,
     6     6.857E-01, 7.559E-01, 7.967E-01, 7.352E-01, 7.294E-01,
     7     7.273E-01, 7.120E-01, 6.438E-01, 6.333E-01, 6.781E-01,
     8     7.841E-01, 6.860E-01, 5.924E-01, 5.814E-01, 5.967E-01,
     9     6.066E-01, 6.515E-01, 7.209E-01, 7.807E-01, 8.288E-01,
     $     8.924E-01, 8.665E-01, 8.571E-01, 9.640E-01, 1.205E+00,
     1     1.366E+00, 1.350E+00, 1.354E+00, 1.780E+00, 1.983E+00,
     2     1.821E+00, 1.642E+00, 1.659E+00, 1.973E+00, 1.861E+00,
     3     1.682E+00, 1.768E+00, 1.640E+00, 1.544E+00, 1.259E+00,
     4     1.248E+00, 1.392E+00, 1.442E+00, 1.550E+00, 1.621E+00,
     5     1.373E+00, 1.310E+00, 1.242E+00, 1.228E+00, 1.201E+00,
     6     1.248E+00, 1.004E+00, 9.996E-01, 9.129E-01, 9.123E-01,
     7     9.068E-01, 9.175E-01, 8.885E-01, 9.650E-01, 9.456E-01,
     8     1.053E+00, 1.050E+00, 1.093E+00, 1.211E+00, 1.211E+00,
     9     1.253E+00, 1.200E+00, 1.279E+00, 1.248E+00, 1.246E+00,
     $     1.265E+00, 1.494E+00, 1.603E+00, 1.371E+00, 1.388E+00/
      DATA (CRSNO2(I),I=  601,  700)/
     1     1.335E+00, 1.313E+00, 1.257E+00, 1.090E+00, 1.035E+00,
     2     9.203E-01, 9.819E-01, 8.464E-01, 8.252E-01, 8.073E-01,
     3     8.613E-01, 8.905E-01, 8.568E-01, 7.859E-01, 7.370E-01,
     4     7.139E-01, 7.300E-01, 8.005E-01, 8.211E-01, 8.852E-01,
     5     8.598E-01, 8.171E-01, 8.214E-01, 8.775E-01, 9.882E-01,
     6     1.060E+00, 1.190E+00, 1.285E+00, 1.261E+00, 1.276E+00,
     7     1.358E+00, 1.294E+00, 1.294E+00, 1.311E+00, 1.357E+00,
     8     1.430E+00, 1.398E+00, 1.361E+00, 1.201E+00, 1.053E+00,
     9     9.180E-01, 8.906E-01, 9.512E-01, 8.880E-01, 7.660E-01,
     $     7.634E-01, 8.405E-01, 7.940E-01, 7.345E-01, 7.304E-01,
     1     7.310E-01, 7.577E-01, 7.651E-01, 7.814E-01, 8.488E-01,
     2     9.205E-01, 1.009E+00, 1.102E+00, 1.037E+00, 1.184E+00,
     3     1.186E+00, 1.074E+00, 1.146E+00, 1.203E+00, 1.207E+00,
     4     1.228E+00, 1.380E+00, 1.448E+00, 1.472E+00, 1.506E+00,
     5     1.728E+00, 1.805E+00, 1.761E+00, 1.663E+00, 1.911E+00,
     6     1.998E+00, 2.223E+00, 2.500E+00, 2.404E+00, 2.312E+00,
     7     2.430E+00, 2.282E+00, 2.314E+00, 2.542E+00, 2.354E+00,
     8     2.671E+00, 2.744E+00, 2.413E+00, 2.057E+00, 2.162E+00,
     9     2.044E+00, 1.900E+00, 1.732E+00, 1.654E+00, 1.714E+00,
     $     1.831E+00, 1.828E+00, 1.653E+00, 1.747E+00, 1.928E+00/
      DATA (CRSNO2(I),I=  701,  800)/
     1     2.089E+00, 2.198E+00, 1.946E+00, 1.905E+00, 1.806E+00,
     2     1.749E+00, 1.812E+00, 1.397E+00, 1.272E+00, 1.333E+00,
     3     1.397E+00, 1.374E+00, 1.223E+00, 1.309E+00, 1.469E+00,
     4     1.592E+00, 1.948E+00, 2.301E+00, 2.441E+00, 2.342E+00,
     5     2.307E+00, 2.531E+00, 2.679E+00, 3.042E+00, 3.138E+00,
     6     3.260E+00, 2.644E+00, 2.363E+00, 2.581E+00, 3.252E+00,
     7     2.927E+00, 2.659E+00, 2.302E+00, 2.093E+00, 2.219E+00,
     8     2.088E+00, 2.032E+00, 1.824E+00, 1.689E+00, 1.685E+00,
     9     1.732E+00, 1.736E+00, 1.764E+00, 1.676E+00, 1.571E+00,
     $     1.639E+00, 1.622E+00, 1.505E+00, 1.509E+00, 1.415E+00,
     1     1.471E+00, 1.353E+00, 1.390E+00, 1.295E+00, 1.180E+00,
     2     1.242E+00, 1.273E+00, 1.138E+00, 1.112E+00, 1.250E+00,
     3     1.356E+00, 1.404E+00, 1.424E+00, 1.553E+00, 1.685E+00,
     4     1.699E+00, 1.587E+00, 1.506E+00, 1.451E+00, 1.539E+00,
     5     1.495E+00, 1.529E+00, 1.554E+00, 1.495E+00, 1.630E+00,
     6     1.607E+00, 1.540E+00, 1.435E+00, 1.366E+00, 1.458E+00,
     7     1.423E+00, 1.622E+00, 1.736E+00, 1.756E+00, 1.725E+00,
     8     1.758E+00, 1.856E+00, 2.005E+00, 2.196E+00, 2.289E+00,
     9     2.362E+00, 2.352E+00, 2.503E+00, 2.365E+00, 2.208E+00,
     $     2.482E+00, 2.900E+00, 2.914E+00, 3.069E+00, 2.636E+00/
      DATA (CRSNO2(I),I=  801,  900)/
     1     2.383E+00, 2.412E+00, 2.648E+00, 2.430E+00, 2.141E+00,
     2     2.190E+00, 2.242E+00, 2.240E+00, 2.151E+00, 2.086E+00,
     3     2.087E+00, 2.186E+00, 2.435E+00, 2.410E+00, 2.415E+00,
     4     2.568E+00, 2.536E+00, 2.624E+00, 2.577E+00, 2.657E+00,
     5     3.012E+00, 3.131E+00, 3.324E+00, 3.598E+00, 3.315E+00,
     6     3.110E+00, 3.330E+00, 3.301E+00, 3.079E+00, 3.323E+00,
     7     3.384E+00, 3.376E+00, 3.464E+00, 3.437E+00, 3.360E+00,
     8     3.211E+00, 2.989E+00, 2.696E+00, 2.574E+00, 2.569E+00,
     9     2.611E+00, 2.721E+00, 2.944E+00, 2.966E+00, 3.011E+00,
     $     3.141E+00, 3.418E+00, 3.561E+00, 3.559E+00, 3.811E+00,
     1     3.894E+00, 3.473E+00, 3.257E+00, 3.291E+00, 3.132E+00,
     2     3.028E+00, 3.098E+00, 3.334E+00, 3.297E+00, 3.407E+00,
     3     3.600E+00, 3.810E+00, 3.646E+00, 3.728E+00, 3.881E+00,
     4     3.679E+00, 3.696E+00, 3.562E+00, 3.544E+00, 3.634E+00,
     5     3.532E+00, 3.451E+00, 3.216E+00, 3.119E+00, 3.021E+00,
     6     2.589E+00, 2.323E+00, 2.291E+00, 2.136E+00, 2.079E+00,
     7     2.043E+00, 2.035E+00, 2.142E+00, 2.246E+00, 2.350E+00,
     8     2.709E+00, 2.737E+00, 2.838E+00, 2.809E+00, 3.040E+00,
     9     3.225E+00, 3.226E+00, 3.173E+00, 3.043E+00, 3.063E+00,
     $     3.233E+00, 3.160E+00, 2.876E+00, 2.811E+00, 2.459E+00/
      DATA (CRSNO2(I),I=  901, 1000)/
     1     2.125E+00, 2.087E+00, 2.084E+00, 2.147E+00, 2.155E+00,
     2     2.054E+00, 2.028E+00, 2.092E+00, 2.210E+00, 2.355E+00,
     3     2.220E+00, 2.301E+00, 2.158E+00, 2.034E+00, 2.055E+00,
     4     2.068E+00, 2.182E+00, 2.263E+00, 2.276E+00, 2.509E+00,
     5     2.821E+00, 2.842E+00, 2.874E+00, 3.129E+00, 3.126E+00,
     6     3.045E+00, 3.271E+00, 3.141E+00, 3.200E+00, 3.316E+00,
     7     3.427E+00, 3.675E+00, 3.862E+00, 3.683E+00, 3.748E+00,
     8     3.960E+00, 4.063E+00, 4.086E+00, 4.098E+00, 4.032E+00,
     9     3.902E+00, 3.901E+00, 3.852E+00, 3.933E+00, 3.834E+00,
     $     3.712E+00, 3.806E+00, 3.743E+00, 3.740E+00, 3.757E+00,
     1     3.735E+00, 3.824E+00, 3.942E+00, 3.907E+00, 3.795E+00,
     2     3.842E+00, 4.047E+00, 4.100E+00, 4.060E+00, 4.043E+00,
     3     4.106E+00, 4.148E+00, 4.215E+00, 4.268E+00, 4.464E+00,
     4     4.684E+00, 4.594E+00, 4.746E+00, 4.796E+00, 5.046E+00,
     5     4.825E+00, 4.725E+00, 4.676E+00, 4.622E+00, 4.328E+00,
     6     4.563E+00, 5.072E+00, 5.596E+00, 5.868E+00, 5.962E+00,
     7     5.785E+00, 5.877E+00, 5.734E+00, 5.517E+00, 5.833E+00,
     8     6.359E+00, 6.329E+00, 6.109E+00, 5.893E+00, 5.672E+00,
     9     4.978E+00, 4.750E+00, 4.552E+00, 4.473E+00, 4.192E+00,
     $     4.148E+00, 4.051E+00, 3.907E+00, 3.852E+00, 3.784E+00/
      DATA (CRSNO2(I),I= 1001, 1100)/
     1     3.662E+00, 3.670E+00, 4.364E+00, 4.261E+00, 4.449E+00,
     2     4.189E+00, 4.152E+00, 4.162E+00, 3.957E+00, 3.758E+00,
     3     3.805E+00, 3.932E+00, 4.063E+00, 3.881E+00, 3.928E+00,
     4     4.069E+00, 3.901E+00, 3.861E+00, 4.301E+00, 4.401E+00,
     5     4.446E+00, 4.495E+00, 4.535E+00, 4.847E+00, 5.174E+00,
     6     5.200E+00, 5.008E+00, 4.886E+00, 5.147E+00, 5.040E+00,
     7     5.137E+00, 5.041E+00, 4.759E+00, 4.648E+00, 4.807E+00,
     8     5.387E+00, 5.552E+00, 5.421E+00, 5.541E+00, 5.124E+00,
     9     4.688E+00, 4.336E+00, 4.259E+00, 4.082E+00, 3.538E+00,
     $     3.545E+00, 3.711E+00, 3.668E+00, 3.429E+00, 3.356E+00,
     1     3.596E+00, 3.648E+00, 3.943E+00, 4.132E+00, 4.041E+00,
     2     3.893E+00, 4.138E+00, 4.209E+00, 4.258E+00, 4.168E+00,
     3     4.014E+00, 4.100E+00, 4.243E+00, 4.307E+00, 4.467E+00,
     4     4.643E+00, 4.635E+00, 4.713E+00, 4.801E+00, 4.719E+00,
     5     4.430E+00, 4.646E+00, 4.889E+00, 5.082E+00, 5.065E+00,
     6     5.037E+00, 5.047E+00, 5.125E+00, 5.337E+00, 5.633E+00,
     7     5.616E+00, 5.995E+00, 6.903E+00, 6.720E+00, 6.625E+00,
     8     6.273E+00, 6.189E+00, 6.048E+00, 5.766E+00, 5.695E+00,
     9     6.108E+00, 6.811E+00, 6.514E+00, 6.598E+00, 6.608E+00,
     $     6.536E+00, 6.430E+00, 6.516E+00, 6.724E+00, 6.958E+00/
      DATA (CRSNO2(I),I= 1101, 1200)/
     1     7.028E+00, 7.371E+00, 7.816E+00, 7.205E+00, 6.660E+00,
     2     6.548E+00, 6.749E+00, 7.208E+00, 6.835E+00, 6.488E+00,
     3     6.083E+00, 5.983E+00, 6.318E+00, 6.379E+00, 5.818E+00,
     4     5.496E+00, 5.347E+00, 5.525E+00, 5.320E+00, 5.144E+00,
     5     4.924E+00, 5.236E+00, 5.197E+00, 5.464E+00, 5.553E+00,
     6     5.783E+00, 5.915E+00, 5.895E+00, 5.921E+00, 5.927E+00,
     7     6.184E+00, 6.303E+00, 6.149E+00, 6.093E+00, 6.147E+00,
     8     6.506E+00, 7.065E+00, 7.637E+00, 7.867E+00, 8.263E+00,
     9     9.105E+00, 9.590E+00, 8.672E+00, 7.992E+00, 7.958E+00,
     $     7.568E+00, 7.143E+00, 6.484E+00, 6.016E+00, 5.842E+00,
     1     5.758E+00, 5.816E+00, 5.624E+00, 5.843E+00, 6.305E+00,
     2     6.416E+00, 6.975E+00, 8.036E+00, 7.534E+00, 7.008E+00,
     3     6.601E+00, 6.060E+00, 5.945E+00, 5.663E+00, 5.416E+00,
     4     5.238E+00, 5.135E+00, 5.158E+00, 5.165E+00, 5.060E+00,
     5     4.938E+00, 5.005E+00, 5.219E+00, 5.097E+00, 4.618E+00,
     6     4.558E+00, 4.543E+00, 4.395E+00, 4.279E+00, 4.194E+00,
     7     4.259E+00, 4.222E+00, 4.213E+00, 4.089E+00, 4.206E+00,
     8     4.270E+00, 4.437E+00, 4.703E+00, 4.741E+00, 4.750E+00,
     9     4.970E+00, 4.942E+00, 5.011E+00, 5.080E+00, 4.942E+00,
     $     5.131E+00, 5.788E+00, 5.952E+00, 6.091E+00, 6.668E+00/
      DATA (CRSNO2(I),I= 1201, 1300)/
     1     6.879E+00, 7.233E+00, 7.636E+00, 8.268E+00, 8.435E+00,
     2     8.741E+00, 8.910E+00, 8.782E+00, 8.821E+00, 8.542E+00,
     3     8.448E+00, 8.478E+00, 8.596E+00, 8.627E+00, 8.560E+00,
     4     8.676E+00, 8.730E+00, 8.435E+00, 8.038E+00, 8.138E+00,
     5     8.270E+00, 8.562E+00, 8.776E+00, 9.297E+00, 9.346E+00,
     6     9.155E+00, 9.108E+00, 8.892E+00, 8.585E+00, 8.502E+00,
     7     8.250E+00, 7.973E+00, 7.852E+00, 7.958E+00, 7.861E+00,
     8     7.518E+00, 7.113E+00, 6.688E+00, 6.482E+00, 6.670E+00,
     9     7.007E+00, 7.092E+00, 7.187E+00, 6.911E+00, 6.591E+00,
     $     6.233E+00, 5.987E+00, 5.939E+00, 5.998E+00, 6.081E+00,
     1     6.120E+00, 6.070E+00, 6.005E+00, 5.948E+00, 6.132E+00,
     2     6.312E+00, 6.880E+00, 7.234E+00, 7.693E+00, 8.070E+00,
     3     8.663E+00, 8.380E+00, 8.404E+00, 8.663E+00, 9.130E+00,
     4     9.525E+00, 9.611E+00, 1.035E+01, 1.058E+01, 1.056E+01,
     5     1.026E+01, 1.098E+01, 1.245E+01, 1.277E+01, 1.147E+01,
     6     9.867E+00, 8.447E+00, 7.712E+00, 7.315E+00, 6.987E+00,
     7     6.808E+00, 6.791E+00, 6.537E+00, 6.503E+00, 7.025E+00,
     8     7.042E+00, 6.837E+00, 6.695E+00, 6.579E+00, 7.166E+00,
     9     7.758E+00, 8.016E+00, 7.948E+00, 7.922E+00, 7.898E+00,
     $     7.903E+00, 7.789E+00, 7.581E+00, 7.530E+00, 7.473E+00/
      DATA (CRSNO2(I),I= 1301, 1400)/
     1     7.362E+00, 7.178E+00, 7.255E+00, 7.518E+00, 7.444E+00,
     2     7.485E+00, 7.390E+00, 7.287E+00, 7.306E+00, 7.326E+00,
     3     7.168E+00, 6.910E+00, 6.848E+00, 6.719E+00, 6.422E+00,
     4     6.377E+00, 6.411E+00, 6.363E+00, 6.698E+00, 7.034E+00,
     5     7.217E+00, 7.371E+00, 7.587E+00, 7.466E+00, 7.568E+00,
     6     7.759E+00, 8.075E+00, 8.387E+00, 8.688E+00, 8.929E+00,
     7     9.255E+00, 9.544E+00, 1.004E+01, 9.680E+00, 9.542E+00,
     8     9.208E+00, 9.200E+00, 9.276E+00, 9.159E+00, 8.854E+00,
     9     8.733E+00, 8.966E+00, 9.138E+00, 9.324E+00, 9.348E+00,
     $     9.368E+00, 9.403E+00, 9.712E+00, 1.041E+01, 1.091E+01,
     1     1.132E+01, 1.122E+01, 1.113E+01, 1.099E+01, 1.017E+01,
     2     9.554E+00, 8.911E+00, 8.550E+00, 8.243E+00, 8.183E+00,
     3     8.656E+00, 8.949E+00, 8.415E+00, 8.617E+00, 8.836E+00,
     4     8.647E+00, 8.449E+00, 8.686E+00, 8.587E+00, 8.425E+00,
     5     8.516E+00, 8.792E+00, 8.634E+00, 9.165E+00, 8.684E+00,
     6     8.919E+00, 9.337E+00, 9.551E+00, 9.428E+00, 9.759E+00,
     7     9.703E+00, 9.754E+00, 9.859E+00, 9.873E+00, 9.587E+00,
     8     9.747E+00, 1.018E+01, 1.063E+01, 1.096E+01, 1.143E+01,
     9     1.172E+01, 1.170E+01, 1.196E+01, 1.231E+01, 1.269E+01,
     $     1.324E+01, 1.252E+01, 1.240E+01, 1.315E+01, 1.241E+01/
      DATA (CRSNO2(I),I= 1401, 1500)/
     1     1.075E+01, 9.850E+00, 9.746E+00, 9.286E+00, 8.833E+00,
     2     8.918E+00, 8.997E+00, 9.063E+00, 8.721E+00, 8.418E+00,
     3     8.445E+00, 8.849E+00, 8.961E+00, 8.976E+00, 9.164E+00,
     4     9.423E+00, 9.172E+00, 8.865E+00, 8.304E+00, 8.403E+00,
     5     8.269E+00, 7.850E+00, 8.318E+00, 8.476E+00, 8.244E+00,
     6     8.385E+00, 8.578E+00, 8.633E+00, 8.809E+00, 8.794E+00,
     7     8.917E+00, 9.158E+00, 9.177E+00, 9.538E+00, 9.099E+00,
     8     8.688E+00, 8.806E+00, 8.617E+00, 7.935E+00, 8.608E+00,
     9     8.793E+00, 9.132E+00, 9.083E+00, 9.139E+00, 9.364E+00,
     $     9.166E+00, 9.225E+00, 1.024E+01, 1.115E+01, 1.115E+01,
     1     1.120E+01, 1.113E+01, 1.041E+01, 1.073E+01, 1.080E+01,
     2     1.076E+01, 1.120E+01, 1.154E+01, 1.215E+01, 1.203E+01,
     3     1.107E+01, 1.101E+01, 1.007E+01, 9.418E+00, 9.040E+00,
     4     8.343E+00, 7.772E+00, 7.805E+00, 7.578E+00, 7.562E+00,
     5     7.772E+00, 7.671E+00, 7.959E+00, 8.467E+00, 9.105E+00,
     6     9.432E+00, 9.852E+00, 1.020E+01, 1.042E+01, 1.066E+01,
     7     1.106E+01, 1.157E+01, 1.183E+01, 1.124E+01, 1.172E+01,
     8     1.220E+01, 1.211E+01, 1.250E+01, 1.321E+01, 1.414E+01,
     9     1.457E+01, 1.485E+01, 1.472E+01, 1.433E+01, 1.521E+01,
     $     1.534E+01, 1.410E+01, 1.213E+01, 1.139E+01, 1.056E+01/
      DATA (CRSNO2(I),I= 1501, 1600)/
     1     1.017E+01, 9.986E+00, 9.983E+00, 1.001E+01, 9.747E+00,
     2     9.738E+00, 9.854E+00, 1.024E+01, 1.062E+01, 1.095E+01,
     3     1.120E+01, 1.177E+01, 1.167E+01, 1.129E+01, 1.210E+01,
     4     1.236E+01, 1.236E+01, 1.312E+01, 1.373E+01, 1.299E+01,
     5     1.247E+01, 1.231E+01, 1.174E+01, 1.174E+01, 1.109E+01,
     6     1.106E+01, 1.124E+01, 1.113E+01, 1.142E+01, 1.114E+01,
     7     1.061E+01, 1.063E+01, 1.001E+01, 1.019E+01, 1.112E+01,
     8     1.184E+01, 1.216E+01, 1.264E+01, 1.302E+01, 1.273E+01,
     9     1.227E+01, 1.278E+01, 1.384E+01, 1.378E+01, 1.354E+01,
     $     1.305E+01, 1.299E+01, 1.313E+01, 1.267E+01, 1.183E+01,
     1     1.086E+01, 1.015E+01, 9.737E+00, 9.343E+00, 9.013E+00,
     2     8.907E+00, 9.006E+00, 9.687E+00, 1.041E+01, 1.057E+01,
     3     1.108E+01, 1.154E+01, 1.152E+01, 1.163E+01, 1.106E+01,
     4     1.074E+01, 1.069E+01, 1.118E+01, 1.133E+01, 1.144E+01,
     5     1.187E+01, 1.213E+01, 1.181E+01, 1.213E+01, 1.247E+01,
     6     1.254E+01, 1.198E+01, 1.067E+01, 1.012E+01, 9.908E+00,
     7     9.931E+00, 9.881E+00, 1.020E+01, 1.026E+01, 1.057E+01,
     8     1.135E+01, 1.195E+01, 1.221E+01, 1.247E+01, 1.215E+01,
     9     1.167E+01, 1.131E+01, 1.151E+01, 1.207E+01, 1.268E+01,
     $     1.275E+01, 1.273E+01, 1.230E+01, 1.193E+01, 1.208E+01/
      DATA (CRSNO2(I),I= 1601, 1700)/
     1     1.229E+01, 1.278E+01, 1.241E+01, 1.207E+01, 1.179E+01,
     2     1.101E+01, 1.089E+01, 1.127E+01, 1.157E+01, 1.161E+01,
     3     1.176E+01, 1.184E+01, 1.183E+01, 1.228E+01, 1.249E+01,
     4     1.214E+01, 1.218E+01, 1.228E+01, 1.216E+01, 1.160E+01,
     5     1.101E+01, 1.063E+01, 1.054E+01, 1.060E+01, 1.089E+01,
     6     1.139E+01, 1.131E+01, 1.183E+01, 1.238E+01, 1.256E+01,
     7     1.256E+01, 1.289E+01, 1.366E+01, 1.432E+01, 1.399E+01,
     8     1.385E+01, 1.520E+01, 1.650E+01, 1.826E+01, 1.917E+01,
     9     1.923E+01, 1.929E+01, 1.961E+01, 1.809E+01, 1.672E+01,
     $     1.616E+01, 1.484E+01, 1.356E+01, 1.279E+01, 1.219E+01,
     1     1.177E+01, 1.152E+01, 1.160E+01, 1.100E+01, 1.081E+01,
     2     1.073E+01, 1.090E+01, 1.084E+01, 1.118E+01, 1.157E+01,
     3     1.269E+01, 1.279E+01, 1.270E+01, 1.296E+01, 1.297E+01,
     4     1.357E+01, 1.396E+01, 1.410E+01, 1.449E+01, 1.453E+01,
     5     1.554E+01, 1.607E+01, 1.681E+01, 1.753E+01, 1.841E+01,
     6     1.792E+01, 1.727E+01, 1.663E+01, 1.496E+01, 1.427E+01,
     7     1.458E+01, 1.375E+01, 1.319E+01, 1.246E+01, 1.206E+01,
     8     1.187E+01, 1.220E+01, 1.224E+01, 1.215E+01, 1.216E+01,
     9     1.176E+01, 1.143E+01, 1.141E+01, 1.148E+01, 1.146E+01,
     $     1.155E+01, 1.114E+01, 1.071E+01, 1.065E+01, 1.045E+01/
      DATA (CRSNO2(I),I= 1701, 1800)/
     1     1.036E+01, 1.047E+01, 1.066E+01, 1.061E+01, 9.759E+00,
     2     9.459E+00, 9.534E+00, 1.007E+01, 1.011E+01, 1.000E+01,
     3     1.026E+01, 1.046E+01, 1.089E+01, 1.155E+01, 1.161E+01,
     4     1.164E+01, 1.143E+01, 1.143E+01, 1.177E+01, 1.155E+01,
     5     1.171E+01, 1.252E+01, 1.344E+01, 1.477E+01, 1.549E+01,
     6     1.569E+01, 1.560E+01, 1.599E+01, 1.566E+01, 1.612E+01,
     7     1.723E+01, 1.762E+01, 1.860E+01, 1.885E+01, 1.813E+01,
     8     1.701E+01, 1.801E+01, 1.829E+01, 1.698E+01, 1.651E+01,
     9     1.573E+01, 1.474E+01, 1.284E+01, 1.158E+01, 1.102E+01,
     $     1.071E+01, 1.049E+01, 1.046E+01, 1.066E+01, 1.086E+01,
     1     1.087E+01, 1.088E+01, 1.087E+01, 1.112E+01, 1.135E+01,
     2     1.142E+01, 1.164E+01, 1.190E+01, 1.258E+01, 1.312E+01,
     3     1.359E+01, 1.387E+01, 1.375E+01, 1.423E+01, 1.448E+01,
     4     1.455E+01, 1.486E+01, 1.534E+01, 1.508E+01, 1.513E+01,
     5     1.583E+01, 1.616E+01, 1.610E+01, 1.605E+01, 1.621E+01,
     6     1.701E+01, 1.855E+01, 1.888E+01, 1.875E+01, 1.965E+01,
     7     1.842E+01, 1.833E+01, 1.682E+01, 1.542E+01, 1.460E+01,
     8     1.445E+01, 1.435E+01, 1.422E+01, 1.451E+01, 1.516E+01,
     9     1.576E+01, 1.564E+01, 1.554E+01, 1.556E+01, 1.499E+01,
     $     1.499E+01, 1.484E+01, 1.434E+01, 1.382E+01, 1.365E+01/
      DATA (CRSNO2(I),I= 1801, 1900)/
     1     1.343E+01, 1.311E+01, 1.251E+01, 1.214E+01, 1.213E+01,
     2     1.156E+01, 1.158E+01, 1.201E+01, 1.239E+01, 1.241E+01,
     3     1.254E+01, 1.297E+01, 1.353E+01, 1.407E+01, 1.416E+01,
     4     1.468E+01, 1.514E+01, 1.527E+01, 1.549E+01, 1.559E+01,
     5     1.565E+01, 1.531E+01, 1.613E+01, 1.669E+01, 1.655E+01,
     6     1.612E+01, 1.576E+01, 1.600E+01, 1.598E+01, 1.557E+01,
     7     1.426E+01, 1.371E+01, 1.343E+01, 1.250E+01, 1.185E+01,
     8     1.154E+01, 1.137E+01, 1.148E+01, 1.154E+01, 1.157E+01,
     9     1.184E+01, 1.223E+01, 1.229E+01, 1.247E+01, 1.315E+01,
     $     1.373E+01, 1.397E+01, 1.388E+01, 1.381E+01, 1.433E+01,
     1     1.524E+01, 1.563E+01, 1.563E+01, 1.578E+01, 1.652E+01,
     2     1.656E+01, 1.592E+01, 1.617E+01, 1.608E+01, 1.535E+01,
     3     1.561E+01, 1.628E+01, 1.613E+01, 1.562E+01, 1.455E+01,
     4     1.468E+01, 1.440E+01, 1.337E+01, 1.240E+01, 1.177E+01,
     5     1.174E+01, 1.198E+01, 1.199E+01, 1.205E+01, 1.218E+01,
     6     1.195E+01, 1.192E+01, 1.195E+01, 1.254E+01, 1.254E+01,
     7     1.286E+01, 1.329E+01, 1.366E+01, 1.414E+01, 1.433E+01,
     8     1.395E+01, 1.408E+01, 1.543E+01, 1.697E+01, 1.720E+01,
     9     1.741E+01, 1.762E+01, 1.825E+01, 1.779E+01, 1.701E+01,
     $     1.640E+01, 1.728E+01, 1.750E+01, 1.657E+01, 1.567E+01/
      DATA (CRSNO2(I),I= 1901, 2000)/
     1     1.571E+01, 1.579E+01, 1.562E+01, 1.546E+01, 1.545E+01,
     2     1.452E+01, 1.405E+01, 1.419E+01, 1.440E+01, 1.452E+01,
     3     1.454E+01, 1.465E+01, 1.495E+01, 1.513E+01, 1.510E+01,
     4     1.500E+01, 1.506E+01, 1.545E+01, 1.579E+01, 1.592E+01,
     5     1.567E+01, 1.511E+01, 1.436E+01, 1.425E+01, 1.397E+01,
     6     1.387E+01, 1.406E+01, 1.435E+01, 1.472E+01, 1.507E+01,
     7     1.532E+01, 1.542E+01, 1.555E+01, 1.480E+01, 1.454E+01,
     8     1.467E+01, 1.473E+01, 1.498E+01, 1.502E+01, 1.500E+01,
     9     1.451E+01, 1.434E+01, 1.447E+01, 1.493E+01, 1.525E+01,
     $     1.510E+01, 1.521E+01, 1.468E+01, 1.461E+01, 1.473E+01,
     1     1.470E+01, 1.390E+01, 1.332E+01, 1.322E+01, 1.324E+01,
     2     1.340E+01, 1.370E+01, 1.454E+01, 1.474E+01, 1.460E+01,
     3     1.426E+01, 1.399E+01, 1.414E+01, 1.425E+01, 1.370E+01,
     4     1.350E+01, 1.394E+01, 1.413E+01, 1.369E+01, 1.320E+01,
     5     1.307E+01, 1.339E+01, 1.361E+01, 1.390E+01, 1.365E+01,
     6     1.316E+01, 1.288E+01, 1.248E+01, 1.229E+01, 1.225E+01,
     7     1.235E+01, 1.249E+01, 1.225E+01, 1.217E+01, 1.232E+01,
     8     1.246E+01, 1.238E+01, 1.212E+01, 1.215E+01, 1.239E+01,
     9     1.235E+01, 1.202E+01, 1.201E+01, 1.224E+01, 1.268E+01,
     $     1.315E+01, 1.355E+01, 1.413E+01, 1.452E+01, 1.478E+01/
      DATA (CRSNO2(I),I= 2001, 2100)/
     1     1.465E+01, 1.427E+01, 1.436E+01, 1.439E+01, 1.448E+01,
     2     1.450E+01, 1.427E+01, 1.365E+01, 1.344E+01, 1.368E+01,
     3     1.390E+01, 1.444E+01, 1.499E+01, 1.565E+01, 1.682E+01,
     4     1.745E+01, 1.834E+01, 1.879E+01, 1.868E+01, 1.904E+01,
     5     1.809E+01, 1.752E+01, 1.742E+01, 1.725E+01, 1.637E+01,
     6     1.558E+01, 1.527E+01, 1.522E+01, 1.494E+01, 1.440E+01,
     7     1.363E+01, 1.344E+01, 1.309E+01, 1.312E+01, 1.316E+01,
     8     1.322E+01, 1.337E+01, 1.327E+01, 1.348E+01, 1.340E+01,
     9     1.299E+01, 1.303E+01, 1.337E+01, 1.362E+01, 1.389E+01,
     $     1.419E+01, 1.451E+01, 1.494E+01, 1.556E+01, 1.604E+01,
     1     1.600E+01, 1.600E+01, 1.654E+01, 1.719E+01, 1.707E+01,
     2     1.704E+01, 1.702E+01, 1.610E+01, 1.515E+01, 1.474E+01,
     3     1.457E+01, 1.460E+01, 1.477E+01, 1.532E+01, 1.581E+01,
     4     1.593E+01, 1.591E+01, 1.576E+01, 1.535E+01, 1.495E+01,
     5     1.500E+01, 1.568E+01, 1.606E+01, 1.599E+01, 1.592E+01,
     6     1.650E+01, 1.694E+01, 1.743E+01, 1.782E+01, 1.791E+01,
     7     1.794E+01, 1.694E+01, 1.564E+01, 1.526E+01, 1.571E+01,
     8     1.517E+01, 1.421E+01, 1.404E+01, 1.340E+01, 1.292E+01,
     9     1.283E+01, 1.239E+01, 1.231E+01, 1.241E+01, 1.230E+01,
     $     1.227E+01, 1.244E+01, 1.233E+01, 1.237E+01, 1.234E+01/
      DATA (CRSNO2(I),I= 2101, 2200)/
     1     1.252E+01, 1.309E+01, 1.317E+01, 1.353E+01, 1.406E+01,
     2     1.441E+01, 1.414E+01, 1.388E+01, 1.417E+01, 1.403E+01,
     3     1.396E+01, 1.403E+01, 1.433E+01, 1.481E+01, 1.523E+01,
     4     1.572E+01, 1.641E+01, 1.712E+01, 1.707E+01, 1.636E+01,
     5     1.563E+01, 1.564E+01, 1.603E+01, 1.569E+01, 1.474E+01,
     6     1.438E+01, 1.436E+01, 1.455E+01, 1.469E+01, 1.488E+01,
     7     1.503E+01, 1.518E+01, 1.539E+01, 1.552E+01, 1.532E+01,
     8     1.494E+01, 1.437E+01, 1.381E+01, 1.384E+01, 1.375E+01,
     9     1.349E+01, 1.339E+01, 1.328E+01, 1.334E+01, 1.333E+01,
     $     1.354E+01, 1.365E+01, 1.351E+01, 1.336E+01, 1.335E+01,
     1     1.349E+01, 1.362E+01, 1.378E+01, 1.380E+01, 1.373E+01,
     2     1.392E+01, 1.428E+01, 1.465E+01, 1.475E+01, 1.502E+01,
     3     1.525E+01, 1.532E+01, 1.544E+01, 1.545E+01, 1.561E+01,
     4     1.572E+01, 1.589E+01, 1.603E+01, 1.623E+01, 1.627E+01,
     5     1.625E+01, 1.647E+01, 1.643E+01, 1.625E+01, 1.630E+01,
     6     1.649E+01, 1.683E+01, 1.674E+01, 1.650E+01, 1.698E+01,
     7     1.692E+01, 1.696E+01, 1.702E+01, 1.658E+01, 1.609E+01,
     8     1.579E+01, 1.544E+01, 1.518E+01, 1.503E+01, 1.471E+01,
     9     1.441E+01, 1.396E+01, 1.405E+01, 1.399E+01, 1.410E+01,
     $     1.415E+01, 1.445E+01, 1.519E+01, 1.588E+01, 1.620E+01/
      DATA (CRSNO2(I),I= 2201, 2300)/
     1     1.654E+01, 1.716E+01, 1.735E+01, 1.696E+01, 1.661E+01,
     2     1.658E+01, 1.639E+01, 1.631E+01, 1.623E+01, 1.600E+01,
     3     1.531E+01, 1.502E+01, 1.462E+01, 1.411E+01, 1.371E+01,
     4     1.368E+01, 1.367E+01, 1.368E+01, 1.391E+01, 1.396E+01,
     5     1.389E+01, 1.399E+01, 1.398E+01, 1.393E+01, 1.381E+01,
     6     1.392E+01, 1.402E+01, 1.421E+01, 1.448E+01, 1.477E+01,
     7     1.488E+01, 1.520E+01, 1.539E+01, 1.578E+01, 1.630E+01,
     8     1.613E+01, 1.584E+01, 1.569E+01, 1.584E+01, 1.608E+01,
     9     1.603E+01, 1.591E+01, 1.548E+01, 1.509E+01, 1.483E+01,
     $     1.456E+01, 1.452E+01, 1.424E+01, 1.395E+01, 1.360E+01,
     1     1.345E+01, 1.360E+01, 1.359E+01, 1.366E+01, 1.369E+01,
     2     1.368E+01, 1.373E+01, 1.397E+01, 1.416E+01, 1.414E+01,
     3     1.398E+01, 1.396E+01, 1.409E+01, 1.404E+01, 1.390E+01,
     4     1.371E+01, 1.353E+01, 1.334E+01, 1.342E+01, 1.358E+01,
     5     1.378E+01, 1.416E+01, 1.455E+01, 1.488E+01, 1.526E+01,
     6     1.523E+01, 1.508E+01, 1.522E+01, 1.516E+01, 1.518E+01,
     7     1.523E+01, 1.510E+01, 1.513E+01, 1.534E+01, 1.569E+01,
     8     1.620E+01, 1.665E+01, 1.690E+01, 1.682E+01, 1.631E+01,
     9     1.596E+01, 1.546E+01, 1.506E+01, 1.478E+01, 1.492E+01,
     $     1.502E+01, 1.506E+01, 1.490E+01, 1.478E+01, 1.470E+01/
      DATA (CRSNO2(I),I= 2301, 2400)/
     1     1.468E+01, 1.479E+01, 1.493E+01, 1.487E+01, 1.505E+01,
     2     1.530E+01, 1.538E+01, 1.537E+01, 1.519E+01, 1.498E+01,
     3     1.506E+01, 1.515E+01, 1.502E+01, 1.488E+01, 1.479E+01,
     4     1.463E+01, 1.482E+01, 1.510E+01, 1.555E+01, 1.553E+01,
     5     1.558E+01, 1.568E+01, 1.573E+01, 1.551E+01, 1.527E+01,
     6     1.492E+01, 1.479E+01, 1.478E+01, 1.464E+01, 1.449E+01,
     7     1.448E+01, 1.452E+01, 1.446E+01, 1.461E+01, 1.474E+01,
     8     1.475E+01, 1.489E+01, 1.501E+01, 1.527E+01, 1.523E+01,
     9     1.518E+01, 1.505E+01, 1.497E+01, 1.506E+01, 1.510E+01,
     $     1.499E+01, 1.494E+01, 1.482E+01, 1.485E+01, 1.480E+01,
     1     1.466E+01, 1.444E+01, 1.436E+01, 1.422E+01, 1.394E+01,
     2     1.379E+01, 1.369E+01, 1.358E+01, 1.359E+01, 1.374E+01,
     3     1.375E+01, 1.387E+01, 1.398E+01, 1.416E+01, 1.450E+01,
     4     1.471E+01, 1.504E+01, 1.536E+01, 1.545E+01, 1.510E+01,
     5     1.475E+01, 1.461E+01, 1.461E+01, 1.473E+01, 1.465E+01,
     6     1.465E+01, 1.486E+01, 1.510E+01, 1.524E+01, 1.562E+01,
     7     1.585E+01, 1.560E+01, 1.538E+01, 1.539E+01, 1.530E+01,
     8     1.508E+01, 1.500E+01, 1.469E+01, 1.464E+01, 1.478E+01,
     9     1.494E+01, 1.507E+01, 1.513E+01, 1.485E+01, 1.451E+01,
     $     1.423E+01, 1.408E+01, 1.401E+01, 1.388E+01, 1.409E+01/
      DATA (CRSNO2(I),I= 2401, 2500)/
     1     1.405E+01, 1.404E+01, 1.385E+01, 1.372E+01, 1.365E+01,
     2     1.355E+01, 1.365E+01, 1.374E+01, 1.360E+01, 1.348E+01,
     3     1.331E+01, 1.336E+01, 1.348E+01, 1.379E+01, 1.404E+01,
     4     1.441E+01, 1.456E+01, 1.456E+01, 1.438E+01, 1.420E+01,
     5     1.439E+01, 1.463E+01, 1.455E+01, 1.453E+01, 1.437E+01,
     6     1.437E+01, 1.449E+01, 1.469E+01, 1.461E+01, 1.448E+01,
     7     1.463E+01, 1.461E+01, 1.471E+01, 1.492E+01, 1.495E+01,
     8     1.509E+01, 1.533E+01, 1.542E+01, 1.537E+01, 1.516E+01,
     9     1.522E+01, 1.527E+01, 1.545E+01, 1.552E+01, 1.536E+01,
     $     1.559E+01, 1.570E+01, 1.585E+01, 1.589E+01, 1.597E+01,
     1     1.572E+01, 1.585E+01, 1.563E+01, 1.511E+01, 1.489E+01,
     2     1.453E+01, 1.429E+01, 1.392E+01, 1.379E+01, 1.369E+01,
     3     1.360E+01, 1.359E+01, 1.337E+01, 1.340E+01, 1.324E+01,
     4     1.313E+01, 1.326E+01, 1.321E+01, 1.313E+01, 1.292E+01,
     5     1.283E+01, 1.285E+01, 1.288E+01, 1.306E+01, 1.318E+01,
     6     1.319E+01, 1.314E+01, 1.297E+01, 1.295E+01, 1.302E+01,
     7     1.304E+01, 1.299E+01, 1.314E+01, 1.329E+01, 1.360E+01,
     8     1.371E+01, 1.398E+01, 1.426E+01, 1.434E+01, 1.462E+01,
     9     1.507E+01, 1.536E+01, 1.544E+01, 1.587E+01, 1.599E+01,
     $     1.601E+01, 1.631E+01, 1.631E+01, 1.619E+01, 1.610E+01/
      DATA (CRSNO2(I),I= 2501, 2600)/
     1     1.597E+01, 1.580E+01, 1.537E+01, 1.492E+01, 1.485E+01,
     2     1.486E+01, 1.490E+01, 1.488E+01, 1.475E+01, 1.477E+01,
     3     1.470E+01, 1.447E+01, 1.437E+01, 1.412E+01, 1.391E+01,
     4     1.357E+01, 1.324E+01, 1.312E+01, 1.295E+01, 1.296E+01,
     5     1.287E+01, 1.272E+01, 1.275E+01, 1.270E+01, 1.280E+01,
     6     1.299E+01, 1.330E+01, 1.343E+01, 1.353E+01, 1.357E+01,
     7     1.379E+01, 1.364E+01, 1.369E+01, 1.366E+01, 1.347E+01,
     8     1.324E+01, 1.324E+01, 1.322E+01, 1.332E+01, 1.344E+01,
     9     1.352E+01, 1.352E+01, 1.357E+01, 1.371E+01, 1.377E+01,
     $     1.385E+01, 1.416E+01, 1.427E+01, 1.442E+01, 1.455E+01,
     1     1.486E+01, 1.494E+01, 1.502E+01, 1.507E+01, 1.514E+01,
     2     1.511E+01, 1.494E+01, 1.500E+01, 1.508E+01, 1.509E+01,
     3     1.495E+01, 1.507E+01, 1.481E+01, 1.460E+01, 1.437E+01,
     4     1.414E+01, 1.387E+01, 1.357E+01, 1.350E+01, 1.321E+01,
     5     1.313E+01, 1.314E+01, 1.312E+01, 1.275E+01, 1.253E+01,
     6     1.246E+01, 1.250E+01, 1.245E+01, 1.273E+01, 1.269E+01,
     7     1.278E+01, 1.303E+01, 1.328E+01, 1.334E+01, 1.325E+01,
     8     1.333E+01, 1.353E+01, 1.342E+01, 1.350E+01, 1.339E+01,
     9     1.310E+01, 1.302E+01, 1.299E+01, 1.306E+01, 1.317E+01,
     $     1.341E+01, 1.351E+01, 1.334E+01, 1.347E+01, 1.349E+01/
      DATA (CRSNO2(I),I= 2601, 2700)/
     1     1.339E+01, 1.320E+01, 1.307E+01, 1.308E+01, 1.316E+01,
     2     1.312E+01, 1.296E+01, 1.286E+01, 1.302E+01, 1.307E+01,
     3     1.328E+01, 1.322E+01, 1.335E+01, 1.344E+01, 1.337E+01,
     4     1.352E+01, 1.368E+01, 1.367E+01, 1.377E+01, 1.375E+01,
     5     1.379E+01, 1.378E+01, 1.366E+01, 1.362E+01, 1.383E+01,
     6     1.378E+01, 1.377E+01, 1.378E+01, 1.387E+01, 1.361E+01,
     7     1.342E+01, 1.326E+01, 1.305E+01, 1.302E+01, 1.276E+01,
     8     1.257E+01, 1.251E+01, 1.251E+01, 1.238E+01, 1.232E+01,
     9     1.241E+01, 1.241E+01, 1.266E+01, 1.277E+01, 1.297E+01,
     $     1.325E+01, 1.355E+01, 1.386E+01, 1.404E+01, 1.424E+01,
     1     1.451E+01, 1.452E+01, 1.454E+01, 1.475E+01, 1.486E+01,
     2     1.496E+01, 1.512E+01, 1.509E+01, 1.494E+01, 1.490E+01,
     3     1.489E+01, 1.461E+01, 1.447E+01, 1.426E+01, 1.406E+01,
     4     1.396E+01, 1.386E+01, 1.374E+01, 1.361E+01, 1.337E+01,
     5     1.319E+01, 1.301E+01, 1.294E+01, 1.285E+01, 1.281E+01,
     6     1.266E+01, 1.266E+01, 1.245E+01, 1.233E+01, 1.205E+01,
     7     1.202E+01, 1.216E+01, 1.226E+01, 1.239E+01, 1.259E+01,
     8     1.247E+01, 1.249E+01, 1.272E+01, 1.272E+01, 1.275E+01,
     9     1.264E+01, 1.269E+01, 1.282E+01, 1.288E+01, 1.298E+01,
     $     1.286E+01, 1.271E+01, 1.286E+01, 1.293E+01, 1.280E+01/
      DATA (CRSNO2(I),I= 2701, 2800)/
     1     1.274E+01, 1.292E+01, 1.286E+01, 1.266E+01, 1.272E+01,
     2     1.263E+01, 1.264E+01, 1.255E+01, 1.260E+01, 1.255E+01,
     3     1.255E+01, 1.253E+01, 1.256E+01, 1.253E+01, 1.268E+01,
     4     1.283E+01, 1.289E+01, 1.295E+01, 1.301E+01, 1.312E+01,
     5     1.313E+01, 1.306E+01, 1.321E+01, 1.342E+01, 1.342E+01,
     6     1.326E+01, 1.319E+01, 1.314E+01, 1.295E+01, 1.280E+01,
     7     1.262E+01, 1.245E+01, 1.226E+01, 1.198E+01, 1.199E+01,
     8     1.181E+01, 1.169E+01, 1.164E+01, 1.152E+01, 1.144E+01,
     9     1.142E+01, 1.145E+01, 1.150E+01, 1.149E+01, 1.150E+01,
     $     1.149E+01, 1.157E+01, 1.149E+01, 1.156E+01, 1.147E+01,
     1     1.144E+01, 1.159E+01, 1.158E+01, 1.150E+01, 1.150E+01,
     2     1.149E+01, 1.155E+01, 1.174E+01, 1.182E+01, 1.199E+01,
     3     1.207E+01, 1.210E+01, 1.217E+01, 1.238E+01, 1.244E+01,
     4     1.242E+01, 1.265E+01, 1.282E+01, 1.289E+01, 1.296E+01,
     5     1.305E+01, 1.315E+01, 1.301E+01, 1.308E+01, 1.313E+01,
     6     1.346E+01, 1.367E+01, 1.385E+01, 1.393E+01, 1.382E+01,
     7     1.402E+01, 1.408E+01, 1.407E+01, 1.396E+01, 1.392E+01,
     8     1.367E+01, 1.359E+01, 1.329E+01, 1.321E+01, 1.300E+01,
     9     1.266E+01, 1.250E+01, 1.233E+01, 1.221E+01, 1.208E+01,
     $     1.194E+01, 1.181E+01, 1.172E+01, 1.160E+01, 1.145E+01/
      DATA (CRSNO2(I),I= 2801, 2900)/
     1     1.142E+01, 1.149E+01, 1.118E+01, 1.133E+01, 1.141E+01,
     2     1.144E+01, 1.159E+01, 1.180E+01, 1.175E+01, 1.186E+01,
     3     1.192E+01, 1.197E+01, 1.212E+01, 1.228E+01, 1.238E+01,
     4     1.274E+01, 1.278E+01, 1.288E+01, 1.305E+01, 1.333E+01,
     5     1.342E+01, 1.353E+01, 1.366E+01, 1.365E+01, 1.375E+01,
     6     1.378E+01, 1.375E+01, 1.377E+01, 1.386E+01, 1.356E+01,
     7     1.334E+01, 1.297E+01, 1.286E+01, 1.252E+01, 1.235E+01,
     8     1.209E+01, 1.190E+01, 1.163E+01, 1.140E+01, 1.116E+01,
     9     1.113E+01, 1.094E+01, 1.075E+01, 1.053E+01, 1.044E+01,
     $     1.023E+01, 1.010E+01, 1.029E+01, 1.023E+01, 1.012E+01,
     1     1.008E+01, 1.038E+01, 1.056E+01, 1.081E+01, 1.092E+01,
     2     1.101E+01, 1.102E+01, 1.123E+01, 1.132E+01, 1.155E+01,
     3     1.145E+01, 1.133E+01, 1.118E+01, 1.099E+01, 1.097E+01,
     4     1.086E+01, 1.077E+01, 1.077E+01, 1.085E+01, 1.095E+01,
     5     1.110E+01, 1.120E+01, 1.139E+01, 1.148E+01, 1.154E+01,
     6     1.176E+01, 1.184E+01, 1.203E+01, 1.201E+01, 1.183E+01,
     7     1.183E+01, 1.153E+01, 1.153E+01, 1.144E+01, 1.153E+01,
     8     1.130E+01, 1.129E+01, 1.142E+01, 1.119E+01, 1.118E+01,
     9     1.097E+01, 1.083E+01, 1.075E+01, 1.044E+01, 1.041E+01,
     $     1.063E+01, 1.048E+01, 1.041E+01, 1.034E+01, 1.023E+01/
      DATA (CRSNO2(I),I= 2901, 3000)/
     1     1.034E+01, 1.024E+01, 1.036E+01, 1.030E+01, 1.017E+01,
     2     1.027E+01, 1.029E+01, 1.057E+01, 1.067E+01, 1.073E+01,
     3     1.095E+01, 1.113E+01, 1.134E+01, 1.144E+01, 1.183E+01,
     4     1.229E+01, 1.237E+01, 1.255E+01, 1.276E+01, 1.290E+01,
     5     1.300E+01, 1.309E+01, 1.348E+01, 1.338E+01, 1.328E+01,
     6     1.333E+01, 1.314E+01, 1.283E+01, 1.249E+01, 1.200E+01,
     7     1.178E+01, 1.157E+01, 1.120E+01, 1.107E+01, 1.110E+01,
     8     1.114E+01, 1.107E+01, 1.093E+01, 1.076E+01, 1.097E+01,
     9     1.081E+01, 1.099E+01, 1.113E+01, 1.099E+01, 1.098E+01,
     $     1.119E+01, 1.114E+01, 1.146E+01, 1.137E+01, 1.146E+01,
     1     1.155E+01, 1.142E+01, 1.165E+01, 1.146E+01, 1.130E+01,
     2     1.123E+01, 1.141E+01, 1.139E+01, 1.138E+01, 1.126E+01,
     3     1.114E+01, 1.125E+01, 1.128E+01, 1.106E+01, 1.091E+01,
     4     1.071E+01, 1.077E+01, 1.059E+01, 1.054E+01, 1.055E+01,
     5     1.034E+01, 1.044E+01, 1.027E+01, 1.043E+01, 1.046E+01,
     6     1.039E+01, 1.053E+01, 1.073E+01, 1.058E+01, 1.056E+01,
     7     1.054E+01, 1.026E+01, 1.026E+01, 1.016E+01, 1.025E+01,
     8     1.032E+01, 1.015E+01, 1.021E+01, 1.026E+01, 1.017E+01,
     9     9.947E+00, 1.003E+01, 9.930E+00, 9.982E+00, 1.002E+01,
     $     9.994E+00, 9.959E+00, 1.016E+01, 1.010E+01, 1.027E+01/
      DATA (CRSNO2(I),I= 3001, 3100)/
     1     1.030E+01, 1.019E+01, 1.010E+01, 9.993E+00, 1.002E+01,
     2     1.018E+01, 1.004E+01, 9.885E+00, 9.728E+00, 9.684E+00,
     3     9.524E+00, 9.276E+00, 9.343E+00, 9.151E+00, 9.220E+00,
     4     9.217E+00, 9.249E+00, 9.188E+00, 9.194E+00, 9.446E+00,
     5     9.471E+00, 9.395E+00, 9.532E+00, 9.485E+00, 9.469E+00,
     6     9.573E+00, 9.610E+00, 9.742E+00, 9.903E+00, 1.005E+01,
     7     1.029E+01, 1.037E+01, 1.038E+01, 1.032E+01, 1.046E+01,
     8     1.048E+01, 1.070E+01, 1.066E+01, 1.057E+01, 1.071E+01,
     9     1.094E+01, 1.096E+01, 1.100E+01, 1.073E+01, 1.106E+01,
     $     1.116E+01, 1.099E+01, 1.091E+01, 1.109E+01, 1.092E+01,
     1     1.079E+01, 1.058E+01, 1.052E+01, 1.060E+01, 1.025E+01,
     2     1.038E+01, 1.008E+01, 9.973E+00, 9.994E+00, 1.010E+01,
     3     1.007E+01, 1.004E+01, 9.885E+00, 9.898E+00, 9.870E+00,
     4     9.789E+00, 9.659E+00, 9.928E+00, 1.002E+01, 1.021E+01,
     5     9.919E+00, 9.685E+00, 9.868E+00, 9.667E+00, 9.775E+00,
     6     1.015E+01, 1.042E+01, 1.038E+01, 1.022E+01, 1.040E+01,
     7     1.025E+01, 1.026E+01, 1.019E+01, 1.035E+01, 1.047E+01,
     8     1.039E+01, 1.054E+01, 1.027E+01, 1.039E+01, 1.048E+01,
     9     1.038E+01, 1.040E+01, 1.037E+01, 1.009E+01, 9.929E+00,
     $     9.706E+00, 9.638E+00, 9.312E+00, 9.188E+00, 9.001E+00/
      DATA (CRSNO2(I),I= 3101, 3200)/
     1     8.834E+00, 8.578E+00, 8.616E+00, 8.507E+00, 8.583E+00,
     2     8.545E+00, 8.508E+00, 8.292E+00, 8.648E+00, 8.579E+00,
     3     8.733E+00, 8.816E+00, 8.772E+00, 9.010E+00, 8.870E+00,
     4     8.896E+00, 9.057E+00, 8.950E+00, 9.233E+00, 9.279E+00,
     5     9.185E+00, 9.080E+00, 9.177E+00, 9.118E+00, 9.272E+00,
     6     9.355E+00, 9.090E+00, 9.268E+00, 9.404E+00, 9.305E+00,
     7     9.295E+00, 9.115E+00, 9.417E+00, 9.186E+00, 8.946E+00,
     8     8.857E+00, 8.855E+00, 8.855E+00, 8.826E+00, 8.935E+00,
     9     8.891E+00, 8.805E+00, 8.585E+00, 8.820E+00, 8.907E+00,
     $     8.533E+00, 8.954E+00, 8.744E+00, 8.899E+00, 9.277E+00,
     1     9.129E+00, 9.317E+00, 9.528E+00, 9.226E+00, 9.370E+00,
     2     9.441E+00, 9.283E+00, 9.016E+00, 8.586E+00, 8.374E+00,
     3     8.676E+00, 8.864E+00, 8.403E+00, 8.468E+00, 8.051E+00,
     4     8.239E+00, 8.081E+00, 8.152E+00, 8.237E+00, 8.278E+00,
     5     8.048E+00, 8.402E+00, 8.562E+00, 8.592E+00, 8.484E+00,
     6     8.494E+00, 8.725E+00, 8.988E+00, 8.709E+00, 8.901E+00,
     7     8.941E+00, 8.987E+00, 9.457E+00, 9.411E+00, 9.416E+00,
     8     9.251E+00, 9.190E+00, 9.569E+00, 1.007E+01, 1.002E+01,
     9     9.672E+00, 9.741E+00, 9.657E+00, 9.750E+00, 9.684E+00,
     $     9.431E+00, 9.250E+00, 9.289E+00, 9.291E+00, 9.045E+00/
      DATA (CRSNO2(I),I= 3201, 3300)/
     1     8.714E+00, 8.403E+00, 8.621E+00, 8.262E+00, 8.270E+00,
     2     8.270E+00, 8.246E+00, 8.187E+00, 8.089E+00, 7.838E+00,
     3     8.047E+00, 8.006E+00, 7.940E+00, 8.180E+00, 8.362E+00,
     4     8.359E+00, 8.151E+00, 8.333E+00, 8.322E+00, 8.236E+00,
     5     7.939E+00, 8.045E+00, 7.980E+00, 8.238E+00, 8.387E+00,
     6     8.076E+00, 7.908E+00, 7.984E+00, 8.000E+00, 8.000E+00,
     7     8.114E+00, 7.562E+00, 7.845E+00, 7.811E+00, 7.813E+00,
     8     7.900E+00, 7.853E+00, 7.975E+00, 7.863E+00, 8.140E+00,
     9     8.133E+00, 8.131E+00, 8.084E+00, 8.277E+00, 8.097E+00,
     $     8.591E+00, 8.608E+00, 8.225E+00, 8.274E+00, 8.354E+00,
     1     8.377E+00, 8.523E+00, 8.647E+00, 8.412E+00, 8.411E+00,
     2     8.266E+00, 8.404E+00, 8.334E+00, 7.968E+00, 7.907E+00,
     3     7.859E+00, 8.233E+00, 8.113E+00, 7.678E+00, 7.806E+00,
     4     7.609E+00, 7.726E+00, 7.653E+00, 7.714E+00, 7.623E+00,
     5     7.467E+00, 7.376E+00, 7.438E+00, 7.719E+00, 7.924E+00,
     6     7.983E+00, 7.814E+00, 7.514E+00, 7.597E+00, 7.586E+00,
     7     7.617E+00, 7.915E+00, 8.189E+00, 7.923E+00, 7.719E+00,
     8     7.739E+00, 7.881E+00, 7.701E+00, 7.528E+00, 7.482E+00,
     9     7.637E+00, 7.497E+00, 7.485E+00, 7.696E+00, 7.987E+00,
     $     7.590E+00, 7.520E+00, 7.464E+00, 7.612E+00, 7.337E+00/
      DATA (CRSNO2(I),I= 3301, 3400)/
     1     7.427E+00, 7.531E+00, 7.785E+00, 7.191E+00, 7.458E+00,
     2     7.428E+00, 7.767E+00, 7.866E+00, 7.702E+00, 7.622E+00,
     3     7.935E+00, 8.067E+00, 7.754E+00, 7.541E+00, 7.401E+00,
     4     7.406E+00, 7.518E+00, 7.425E+00, 7.407E+00, 7.483E+00,
     5     7.135E+00, 7.554E+00, 7.578E+00, 7.510E+00, 7.499E+00,
     6     7.465E+00, 7.263E+00, 7.504E+00, 7.689E+00, 7.822E+00,
     7     7.496E+00, 7.226E+00, 7.464E+00, 7.826E+00, 7.819E+00,
     8     7.593E+00, 7.774E+00, 7.311E+00, 7.188E+00, 7.107E+00,
     9     7.142E+00, 7.146E+00, 7.060E+00, 7.111E+00, 7.130E+00,
     $     7.052E+00, 7.083E+00, 7.047E+00, 6.970E+00, 7.058E+00,
     1     7.064E+00, 7.044E+00, 6.934E+00, 6.964E+00, 6.937E+00,
     2     6.974E+00, 7.088E+00, 7.095E+00, 7.134E+00, 7.197E+00,
     3     7.118E+00, 7.181E+00, 7.075E+00, 6.968E+00, 7.082E+00,
     4     7.215E+00, 7.191E+00, 7.146E+00, 7.292E+00, 7.270E+00,
     5     7.122E+00, 7.096E+00, 7.119E+00, 7.209E+00, 7.308E+00,
     6     7.210E+00, 7.235E+00, 7.212E+00, 7.118E+00, 7.096E+00,
     7     7.089E+00, 7.076E+00, 7.093E+00, 6.990E+00, 7.021E+00,
     8     6.911E+00, 7.035E+00, 7.016E+00, 6.914E+00, 6.817E+00,
     9     6.712E+00, 6.695E+00, 6.788E+00, 6.767E+00, 6.797E+00,
     $     6.732E+00, 6.716E+00, 6.718E+00, 6.640E+00, 6.710E+00/
      DATA (CRSNO2(I),I= 3401, 3500)/
     1     6.592E+00, 6.694E+00, 6.571E+00, 6.520E+00, 6.512E+00,
     2     6.411E+00, 6.514E+00, 6.628E+00, 6.547E+00, 6.363E+00,
     3     6.492E+00, 6.515E+00, 6.569E+00, 6.675E+00, 6.698E+00,
     4     6.606E+00, 6.591E+00, 6.653E+00, 6.818E+00, 6.815E+00,
     5     6.898E+00, 6.965E+00, 6.908E+00, 6.888E+00, 7.003E+00,
     6     7.015E+00, 6.861E+00, 6.844E+00, 6.866E+00, 6.842E+00,
     7     6.884E+00, 6.844E+00, 6.722E+00, 6.651E+00, 6.837E+00,
     8     6.767E+00, 6.544E+00, 6.534E+00, 6.523E+00, 6.574E+00,
     9     6.643E+00, 6.373E+00, 6.265E+00, 6.292E+00, 6.353E+00,
     $     6.281E+00, 6.333E+00, 6.263E+00, 6.319E+00, 6.210E+00,
     1     6.160E+00, 6.275E+00, 6.170E+00, 6.347E+00, 6.378E+00,
     2     6.410E+00, 6.363E+00, 6.383E+00, 6.267E+00, 6.197E+00,
     3     6.304E+00, 6.443E+00, 6.410E+00, 6.411E+00, 6.342E+00,
     4     6.408E+00, 6.537E+00, 6.516E+00, 6.544E+00, 6.438E+00,
     5     6.497E+00, 6.634E+00, 6.545E+00, 6.440E+00, 6.484E+00,
     6     6.584E+00, 6.622E+00, 6.528E+00, 6.476E+00, 6.570E+00,
     7     6.565E+00, 6.514E+00, 6.411E+00, 6.388E+00, 6.253E+00,
     8     6.140E+00, 6.245E+00, 6.194E+00, 6.118E+00, 6.103E+00,
     9     6.178E+00, 6.146E+00, 6.038E+00, 5.888E+00, 5.883E+00,
     $     5.869E+00, 5.863E+00, 5.736E+00, 5.698E+00, 5.796E+00/
      DATA (CRSNO2(I),I= 3501, 3600)/
     1     5.817E+00, 5.694E+00, 5.787E+00, 5.691E+00, 5.604E+00,
     2     5.721E+00, 5.819E+00, 5.743E+00, 5.636E+00, 5.657E+00,
     3     5.636E+00, 5.785E+00, 5.754E+00, 5.549E+00, 5.391E+00,
     4     5.501E+00, 5.661E+00, 5.730E+00, 5.706E+00, 5.679E+00,
     5     5.765E+00, 5.769E+00, 5.811E+00, 5.849E+00, 5.808E+00,
     6     5.849E+00, 5.917E+00, 5.958E+00, 5.920E+00, 5.866E+00,
     7     5.999E+00, 6.087E+00, 6.147E+00, 5.999E+00, 5.773E+00,
     8     5.793E+00, 5.752E+00, 5.633E+00, 5.543E+00, 5.453E+00,
     9     5.516E+00, 5.651E+00, 5.638E+00, 5.493E+00, 5.351E+00,
     $     5.340E+00, 5.384E+00, 5.478E+00, 5.319E+00, 5.132E+00,
     1     5.187E+00, 5.366E+00, 5.213E+00, 5.183E+00, 5.237E+00,
     2     5.219E+00, 4.992E+00, 5.201E+00, 5.255E+00, 5.188E+00,
     3     5.213E+00, 5.242E+00, 5.024E+00, 5.136E+00, 5.261E+00,
     4     5.338E+00, 5.470E+00, 5.343E+00, 5.313E+00, 5.230E+00,
     5     5.501E+00, 5.407E+00, 5.364E+00, 5.297E+00, 5.372E+00,
     6     5.369E+00, 5.408E+00, 5.567E+00, 5.597E+00, 5.554E+00,
     7     5.463E+00, 5.462E+00, 5.625E+00, 5.536E+00, 5.520E+00,
     8     5.664E+00, 5.621E+00, 5.361E+00, 5.262E+00, 5.458E+00,
     9     5.434E+00, 5.233E+00, 5.099E+00, 4.971E+00, 4.969E+00,
     $     5.106E+00, 5.157E+00, 5.021E+00, 5.080E+00, 5.285E+00/
      DATA (CRSNO2(I),I= 3601, 3700)/
     1     5.104E+00, 5.050E+00, 4.974E+00, 4.941E+00, 5.200E+00,
     2     5.102E+00, 5.098E+00, 5.068E+00, 5.003E+00, 5.234E+00,
     3     5.288E+00, 4.950E+00, 4.957E+00, 5.178E+00, 5.111E+00,
     4     5.057E+00, 5.230E+00, 5.092E+00, 5.008E+00, 5.039E+00,
     5     4.976E+00, 4.972E+00, 5.000E+00, 4.920E+00, 4.862E+00,
     6     4.903E+00, 5.001E+00, 5.062E+00, 4.901E+00, 4.817E+00,
     7     4.994E+00, 4.962E+00, 4.920E+00, 4.986E+00, 4.811E+00,
     8     4.787E+00, 4.893E+00, 4.971E+00, 4.847E+00, 4.947E+00,
     9     5.048E+00, 5.010E+00, 5.048E+00, 5.070E+00, 5.033E+00,
     $     5.073E+00, 5.087E+00, 5.045E+00, 5.031E+00, 4.990E+00,
     1     4.949E+00, 4.958E+00, 4.903E+00, 4.829E+00, 4.766E+00,
     2     4.762E+00, 4.691E+00, 4.724E+00, 4.636E+00, 4.551E+00,
     3     4.604E+00, 4.514E+00, 4.507E+00, 4.423E+00, 4.300E+00,
     4     4.321E+00, 4.359E+00, 4.326E+00, 4.306E+00, 4.270E+00,
     5     4.232E+00, 4.281E+00, 4.341E+00, 4.318E+00, 4.300E+00,
     6     4.272E+00, 4.280E+00, 4.359E+00, 4.385E+00, 4.484E+00,
     7     4.438E+00, 4.449E+00, 4.414E+00, 4.360E+00, 4.468E+00,
     8     4.550E+00, 4.467E+00, 4.495E+00, 4.543E+00, 4.526E+00,
     9     4.535E+00, 4.535E+00, 4.452E+00, 4.363E+00, 4.416E+00,
     $     4.498E+00, 4.516E+00, 4.494E+00, 4.351E+00, 4.421E+00/
      DATA (CRSNO2(I),I= 3701, 3800)/
     1     4.303E+00, 4.303E+00, 4.321E+00, 4.296E+00, 4.316E+00,
     2     4.321E+00, 4.317E+00, 4.273E+00, 4.212E+00, 4.221E+00,
     3     4.167E+00, 4.168E+00, 4.161E+00, 4.017E+00, 3.953E+00,
     4     4.083E+00, 4.164E+00, 4.131E+00, 4.125E+00, 4.025E+00,
     5     4.059E+00, 4.131E+00, 4.136E+00, 4.132E+00, 4.156E+00,
     6     4.172E+00, 4.140E+00, 4.073E+00, 4.116E+00, 4.231E+00,
     7     4.169E+00, 4.179E+00, 4.216E+00, 4.247E+00, 4.227E+00,
     8     4.250E+00, 4.233E+00, 4.207E+00, 4.203E+00, 4.231E+00,
     9     4.307E+00, 4.231E+00, 4.236E+00, 4.232E+00, 4.235E+00,
     $     4.186E+00, 4.164E+00, 4.155E+00, 4.107E+00, 4.126E+00,
     1     4.155E+00, 4.185E+00, 4.179E+00, 4.133E+00, 4.140E+00,
     2     4.089E+00, 4.045E+00, 4.044E+00, 4.012E+00, 3.956E+00,
     3     3.974E+00, 4.058E+00, 4.052E+00, 4.028E+00, 4.112E+00,
     4     4.185E+00, 4.120E+00, 4.038E+00, 4.038E+00, 4.083E+00,
     5     4.150E+00, 4.133E+00, 4.108E+00, 4.105E+00, 4.107E+00,
     6     4.194E+00, 4.193E+00, 4.122E+00, 4.117E+00, 4.148E+00,
     7     4.098E+00, 4.102E+00, 4.073E+00, 3.988E+00, 4.011E+00,
     8     4.011E+00, 3.960E+00, 3.886E+00, 3.836E+00, 3.833E+00,
     9     3.818E+00, 3.762E+00, 3.750E+00, 3.808E+00, 3.827E+00,
     $     3.745E+00, 3.897E+00, 3.953E+00, 3.879E+00, 3.827E+00/
      DATA (CRSNO2(I),I= 3801, 3900)/
     1     3.837E+00, 3.807E+00, 3.735E+00, 3.698E+00, 3.646E+00,
     2     3.573E+00, 3.530E+00, 3.586E+00, 3.701E+00, 3.692E+00,
     3     3.545E+00, 3.486E+00, 3.479E+00, 3.461E+00, 3.437E+00,
     4     3.385E+00, 3.349E+00, 3.349E+00, 3.309E+00, 3.377E+00,
     5     3.353E+00, 3.298E+00, 3.268E+00, 3.278E+00, 3.328E+00,
     6     3.377E+00, 3.339E+00, 3.252E+00, 3.278E+00, 3.342E+00,
     7     3.323E+00, 3.270E+00, 3.234E+00, 3.250E+00, 3.286E+00,
     8     3.279E+00, 3.214E+00, 3.191E+00, 3.301E+00, 3.290E+00,
     9     3.274E+00, 3.274E+00, 3.275E+00, 3.266E+00, 3.275E+00,
     $     3.318E+00, 3.389E+00, 3.465E+00, 3.418E+00, 3.402E+00,
     1     3.431E+00, 3.384E+00, 3.391E+00, 3.423E+00, 3.461E+00,
     2     3.497E+00, 3.500E+00, 3.440E+00, 3.404E+00, 3.452E+00,
     3     3.397E+00, 3.367E+00, 3.360E+00, 3.346E+00, 3.348E+00,
     4     3.365E+00, 3.325E+00, 3.209E+00, 3.233E+00, 3.364E+00,
     5     3.380E+00, 3.301E+00, 3.257E+00, 3.253E+00, 3.271E+00,
     6     3.294E+00, 3.271E+00, 3.200E+00, 3.255E+00, 3.275E+00,
     7     3.218E+00, 3.226E+00, 3.303E+00, 3.341E+00, 3.307E+00,
     8     3.325E+00, 3.337E+00, 3.273E+00, 3.260E+00, 3.262E+00,
     9     3.315E+00, 3.365E+00, 3.377E+00, 3.375E+00, 3.407E+00,
     $     3.479E+00, 3.493E+00, 3.532E+00, 3.599E+00, 3.607E+00/
      DATA (CRSNO2(I),I= 3901, 4000)/
     1     3.561E+00, 3.509E+00, 3.522E+00, 3.632E+00, 3.608E+00,
     2     3.510E+00, 3.503E+00, 3.494E+00, 3.463E+00, 3.457E+00,
     3     3.452E+00, 3.429E+00, 3.547E+00, 3.362E+00, 3.316E+00,
     4     3.315E+00, 3.317E+00, 3.338E+00, 3.302E+00, 3.211E+00,
     5     3.215E+00, 3.199E+00, 3.209E+00, 3.205E+00, 3.185E+00,
     6     3.110E+00, 3.040E+00, 3.004E+00, 2.965E+00, 2.918E+00,
     7     2.895E+00, 2.910E+00, 2.879E+00, 2.911E+00, 2.898E+00,
     8     2.820E+00, 2.758E+00, 2.740E+00, 2.754E+00, 2.666E+00,
     9     2.618E+00, 2.624E+00, 2.642E+00, 2.648E+00, 2.638E+00,
     $     2.612E+00, 2.647E+00, 2.654E+00, 2.671E+00, 2.645E+00,
     1     2.626E+00, 2.647E+00, 2.684E+00, 2.694E+00, 2.768E+00,
     2     2.704E+00, 2.762E+00, 2.805E+00, 2.799E+00, 2.804E+00,
     3     2.891E+00, 2.947E+00, 2.797E+00, 2.830E+00, 2.872E+00,
     4     2.849E+00, 2.844E+00, 2.873E+00, 2.806E+00, 2.753E+00,
     5     2.743E+00, 2.786E+00, 2.817E+00, 2.803E+00, 2.745E+00,
     6     2.672E+00, 2.637E+00, 2.602E+00, 2.589E+00, 2.553E+00,
     7     2.560E+00, 2.584E+00, 2.587E+00, 2.570E+00, 2.552E+00,
     8     2.521E+00, 2.554E+00, 2.554E+00, 2.559E+00, 2.558E+00,
     9     2.504E+00, 2.468E+00, 2.543E+00, 2.593E+00, 2.547E+00,
     $     2.483E+00, 2.452E+00, 2.464E+00, 2.503E+00, 2.585E+00/
      DATA (CRSNO2(I),I= 4001, 4100)/
     1     2.583E+00, 2.522E+00, 2.525E+00, 2.581E+00, 2.628E+00,
     2     2.628E+00, 2.653E+00, 2.670E+00, 2.729E+00, 2.790E+00,
     3     2.805E+00, 2.769E+00, 2.762E+00, 2.830E+00, 2.810E+00,
     4     2.775E+00, 2.741E+00, 2.747E+00, 2.803E+00, 2.831E+00,
     5     2.814E+00, 2.779E+00, 2.770E+00, 2.758E+00, 2.747E+00,
     6     2.751E+00, 2.757E+00, 2.711E+00, 2.729E+00, 2.709E+00,
     7     2.657E+00, 2.635E+00, 2.693E+00, 2.744E+00, 2.712E+00,
     8     2.725E+00, 2.662E+00, 2.669E+00, 2.710E+00, 2.635E+00,
     9     2.614E+00, 2.682E+00, 2.597E+00, 2.547E+00, 2.522E+00,
     $     2.517E+00, 2.576E+00, 2.530E+00, 2.492E+00, 2.510E+00,
     1     2.486E+00, 2.453E+00, 2.459E+00, 2.419E+00, 2.390E+00,
     2     2.425E+00, 2.409E+00, 2.385E+00, 2.359E+00, 2.294E+00,
     3     2.286E+00, 2.329E+00, 2.361E+00, 2.361E+00, 2.362E+00,
     4     2.379E+00, 2.305E+00, 2.296E+00, 2.273E+00, 2.292E+00,
     5     2.351E+00, 2.393E+00, 2.311E+00, 2.295E+00, 2.345E+00,
     6     2.379E+00, 2.372E+00, 2.343E+00, 2.333E+00, 2.361E+00,
     7     2.346E+00, 2.333E+00, 2.287E+00, 2.249E+00, 2.271E+00,
     8     2.312E+00, 2.238E+00, 2.234E+00, 2.280E+00, 2.289E+00,
     9     2.226E+00, 2.187E+00, 2.236E+00, 2.182E+00, 2.179E+00,
     $     2.202E+00, 2.218E+00, 2.211E+00, 2.216E+00, 2.215E+00/
      DATA (CRSNO2(I),I= 4101, 4200)/
     1     2.197E+00, 2.187E+00, 2.193E+00, 2.196E+00, 2.145E+00,
     2     2.117E+00, 2.104E+00, 2.103E+00, 2.108E+00, 2.099E+00,
     3     2.063E+00, 2.124E+00, 2.110E+00, 2.075E+00, 2.039E+00,
     4     1.990E+00, 2.085E+00, 2.123E+00, 2.111E+00, 2.064E+00,
     5     2.029E+00, 2.110E+00, 2.139E+00, 2.091E+00, 2.070E+00,
     6     2.051E+00, 2.017E+00, 1.995E+00, 2.044E+00, 2.065E+00,
     7     1.983E+00, 1.948E+00, 2.040E+00, 2.058E+00, 2.041E+00,
     8     1.984E+00, 1.980E+00, 2.012E+00, 2.040E+00, 2.025E+00,
     9     1.981E+00, 1.964E+00, 1.957E+00, 1.943E+00, 1.910E+00,
     $     1.919E+00, 1.972E+00, 1.940E+00, 1.934E+00, 1.990E+00,
     1     2.051E+00, 2.045E+00, 2.024E+00, 1.984E+00, 1.966E+00,
     2     1.991E+00, 1.975E+00, 2.022E+00, 2.053E+00, 2.048E+00,
     3     2.026E+00, 2.032E+00, 2.083E+00, 2.100E+00, 2.107E+00,
     4     2.125E+00, 2.143E+00, 2.097E+00, 2.069E+00, 2.027E+00,
     5     2.039E+00, 2.074E+00, 2.079E+00, 2.083E+00, 2.122E+00,
     6     2.132E+00, 2.133E+00, 2.124E+00, 2.063E+00, 1.992E+00,
     7     1.991E+00, 2.012E+00, 2.042E+00, 2.065E+00, 2.023E+00,
     8     2.028E+00, 2.026E+00, 2.008E+00, 1.981E+00, 1.992E+00,
     9     1.937E+00, 2.000E+00, 2.036E+00, 2.012E+00, 1.948E+00,
     $     1.952E+00, 1.937E+00, 1.885E+00, 1.848E+00, 1.881E+00/
      DATA (CRSNO2(I),I= 4201, 4300)/
     1     1.926E+00, 1.943E+00, 1.962E+00, 1.975E+00, 1.967E+00,
     2     1.899E+00, 1.839E+00, 1.827E+00, 1.859E+00, 1.908E+00,
     3     1.914E+00, 1.812E+00, 1.830E+00, 1.889E+00, 1.931E+00,
     4     1.886E+00, 1.799E+00, 1.814E+00, 1.840E+00, 1.850E+00,
     5     1.831E+00, 1.830E+00, 1.859E+00, 1.830E+00, 1.810E+00,
     6     1.833E+00, 1.846E+00, 1.819E+00, 1.811E+00, 1.793E+00,
     7     1.754E+00, 1.708E+00, 1.794E+00, 1.754E+00, 1.669E+00,
     8     1.597E+00, 1.650E+00, 1.669E+00, 1.670E+00, 1.660E+00,
     9     1.647E+00, 1.650E+00, 1.603E+00, 1.621E+00, 1.659E+00,
     $     1.696E+00, 1.697E+00, 1.619E+00, 1.565E+00, 1.550E+00,
     1     1.563E+00, 1.583E+00, 1.553E+00, 1.512E+00, 1.510E+00,
     2     1.527E+00, 1.513E+00, 1.469E+00, 1.508E+00, 1.533E+00,
     3     1.526E+00, 1.479E+00, 1.491E+00, 1.522E+00, 1.548E+00,
     4     1.561E+00, 1.548E+00, 1.512E+00, 1.534E+00, 1.550E+00,
     5     1.551E+00, 1.549E+00, 1.579E+00, 1.584E+00, 1.580E+00,
     6     1.580E+00, 1.611E+00, 1.598E+00, 1.606E+00, 1.606E+00,
     7     1.594E+00, 1.634E+00, 1.658E+00, 1.639E+00, 1.612E+00,
     8     1.609E+00, 1.698E+00, 1.623E+00, 1.623E+00, 1.653E+00,
     9     1.655E+00, 1.648E+00, 1.645E+00, 1.642E+00, 1.632E+00,
     $     1.598E+00, 1.619E+00, 1.611E+00, 1.591E+00, 1.577E+00/
      DATA (CRSNO2(I),I= 4301, 4400)/
     1     1.611E+00, 1.585E+00, 1.550E+00, 1.545E+00, 1.581E+00,
     2     1.560E+00, 1.574E+00, 1.581E+00, 1.590E+00, 1.606E+00,
     3     1.580E+00, 1.530E+00, 1.513E+00, 1.507E+00, 1.448E+00,
     4     1.459E+00, 1.500E+00, 1.564E+00, 1.632E+00, 1.576E+00,
     5     1.510E+00, 1.481E+00, 1.491E+00, 1.549E+00, 1.504E+00,
     6     1.528E+00, 1.531E+00, 1.517E+00, 1.514E+00, 1.532E+00,
     7     1.533E+00, 1.543E+00, 1.577E+00, 1.620E+00, 1.610E+00,
     8     1.603E+00, 1.598E+00, 1.586E+00, 1.545E+00, 1.512E+00,
     9     1.518E+00, 1.559E+00, 1.544E+00, 1.546E+00, 1.508E+00,
     $     1.461E+00, 1.438E+00, 1.475E+00, 1.493E+00, 1.493E+00,
     1     1.489E+00, 1.543E+00, 1.487E+00, 1.475E+00, 1.464E+00,
     2     1.414E+00, 1.410E+00, 1.419E+00, 1.412E+00, 1.392E+00,
     3     1.408E+00, 1.374E+00, 1.361E+00, 1.363E+00, 1.382E+00,
     4     1.376E+00, 1.334E+00, 1.317E+00, 1.327E+00, 1.270E+00,
     5     1.286E+00, 1.300E+00, 1.313E+00, 1.335E+00, 1.332E+00,
     6     1.342E+00, 1.340E+00, 1.319E+00, 1.357E+00, 1.350E+00,
     7     1.346E+00, 1.345E+00, 1.334E+00, 1.379E+00, 1.373E+00,
     8     1.354E+00, 1.341E+00, 1.300E+00, 1.308E+00, 1.310E+00,
     9     1.293E+00, 1.276E+00, 1.304E+00, 1.306E+00, 1.282E+00,
     $     1.219E+00, 1.227E+00, 1.240E+00, 1.250E+00, 1.245E+00/
      DATA (CRSNO2(I),I= 4401, 4500)/
     1     1.234E+00, 1.283E+00, 1.284E+00, 1.214E+00, 1.251E+00,
     2     1.265E+00, 1.251E+00, 1.218E+00, 1.203E+00, 1.246E+00,
     3     1.251E+00, 1.229E+00, 1.175E+00, 1.266E+00, 1.248E+00,
     4     1.226E+00, 1.249E+00, 1.263E+00, 1.229E+00, 1.193E+00,
     5     1.169E+00, 1.219E+00, 1.225E+00, 1.215E+00, 1.195E+00,
     6     1.182E+00, 1.247E+00, 1.230E+00, 1.182E+00, 1.171E+00,
     7     1.178E+00, 1.168E+00, 1.159E+00, 1.165E+00, 1.135E+00,
     8     1.188E+00, 1.238E+00, 1.252E+00, 1.237E+00, 1.241E+00,
     9     1.256E+00, 1.274E+00, 1.196E+00, 1.215E+00, 1.223E+00,
     $     1.215E+00, 1.228E+00, 1.209E+00, 1.208E+00, 1.217E+00,
     1     1.190E+00, 1.153E+00, 1.157E+00, 1.177E+00, 1.165E+00,
     2     1.165E+00, 1.175E+00, 1.195E+00, 1.237E+00, 1.209E+00,
     3     1.186E+00, 1.167E+00, 1.155E+00, 1.199E+00, 1.239E+00,
     4     1.253E+00, 1.200E+00, 1.207E+00, 1.204E+00, 1.200E+00,
     5     1.199E+00, 1.239E+00, 1.232E+00, 1.227E+00, 1.253E+00,
     6     1.260E+00, 1.253E+00, 1.233E+00, 1.185E+00, 1.133E+00,
     7     1.133E+00, 1.151E+00, 1.183E+00, 1.160E+00, 1.147E+00,
     8     1.137E+00, 1.130E+00, 1.158E+00, 1.135E+00, 1.118E+00,
     9     1.132E+00, 1.160E+00, 1.134E+00, 1.112E+00, 1.116E+00,
     $     1.090E+00, 1.074E+00, 1.061E+00, 1.050E+00, 1.084E+00/
      DATA (CRSNO2(I),I= 4501, 4600)/
     1     1.118E+00, 1.122E+00, 1.054E+00, 1.165E+00, 1.200E+00,
     2     1.193E+00, 1.130E+00, 1.107E+00, 1.092E+00, 1.085E+00,
     3     1.097E+00, 1.032E+00, 1.027E+00, 1.051E+00, 1.098E+00,
     4     1.082E+00, 1.048E+00, 1.024E+00, 1.103E+00, 1.078E+00,
     5     1.042E+00, 1.006E+00, 1.033E+00, 1.064E+00, 1.065E+00,
     6     1.037E+00, 9.849E-01, 9.669E-01, 9.723E-01, 1.002E+00,
     7     1.024E+00, 9.925E-01, 9.766E-01, 9.978E-01, 9.890E-01,
     8     9.540E-01, 9.222E-01, 9.073E-01, 9.162E-01, 8.923E-01,
     9     8.655E-01, 8.683E-01, 8.631E-01, 9.025E-01, 9.605E-01,
     $     9.774E-01, 9.614E-01, 9.529E-01, 9.561E-01, 1.016E+00,
     1     9.688E-01, 9.434E-01, 9.553E-01, 9.753E-01, 9.291E-01,
     2     8.991E-01, 9.259E-01, 9.542E-01, 9.952E-01, 1.024E+00,
     3     9.867E-01, 8.940E-01, 8.900E-01, 9.324E-01, 9.731E-01,
     4     9.985E-01, 9.870E-01, 9.359E-01, 8.835E-01, 9.244E-01,
     5     9.525E-01, 9.354E-01, 9.305E-01, 9.320E-01, 9.342E-01,
     6     9.325E-01, 9.009E-01, 9.237E-01, 9.799E-01, 1.020E+00,
     7     1.021E+00, 1.008E+00, 9.775E-01, 9.229E-01, 8.781E-01,
     8     8.680E-01, 9.453E-01, 9.320E-01, 9.328E-01, 9.493E-01,
     9     1.018E+00, 1.022E+00, 1.008E+00, 9.772E-01, 9.552E-01,
     $     9.661E-01, 9.746E-01, 9.674E-01, 9.627E-01, 9.433E-01/
      DATA (CRSNO2(I),I= 4601, 4700)/
     1     9.212E-01, 9.251E-01, 9.871E-01, 1.007E+00, 9.899E-01,
     2     9.804E-01, 9.675E-01, 9.498E-01, 9.182E-01, 9.933E-01,
     3     1.008E+00, 9.936E-01, 9.491E-01, 9.737E-01, 9.681E-01,
     4     9.169E-01, 8.819E-01, 9.033E-01, 9.382E-01, 9.832E-01,
     5     9.625E-01, 9.322E-01, 8.926E-01, 8.785E-01, 8.613E-01,
     6     8.596E-01, 8.879E-01, 8.865E-01, 8.785E-01, 8.732E-01,
     7     9.018E-01, 8.703E-01, 8.496E-01, 8.407E-01, 8.669E-01,
     8     8.408E-01, 8.024E-01, 7.594E-01, 7.776E-01, 7.929E-01,
     9     8.079E-01, 8.108E-01, 8.672E-01, 8.996E-01, 8.462E-01,
     $     8.208E-01, 8.218E-01, 8.388E-01, 8.665E-01, 8.397E-01,
     1     8.055E-01, 7.568E-01, 7.540E-01, 7.768E-01, 8.114E-01,
     2     8.135E-01, 7.789E-01, 7.663E-01, 7.988E-01, 8.418E-01,
     3     8.561E-01, 8.612E-01, 8.673E-01, 8.389E-01, 8.124E-01,
     4     7.834E-01, 7.766E-01, 7.748E-01, 7.748E-01, 7.766E-01,
     5     7.537E-01, 7.471E-01, 7.728E-01, 7.803E-01, 8.018E-01,
     6     8.254E-01, 7.807E-01, 7.665E-01, 7.467E-01, 6.980E-01,
     7     7.004E-01, 6.985E-01, 6.968E-01, 7.292E-01, 7.095E-01,
     8     7.004E-01, 7.368E-01, 7.226E-01, 7.066E-01, 6.913E-01,
     9     7.638E-01, 7.604E-01, 7.508E-01, 7.670E-01, 7.317E-01,
     $     7.322E-01, 7.682E-01, 7.457E-01, 7.120E-01, 6.812E-01/
      DATA (CRSNO2(I),I= 4701, 4800)/
     1     6.949E-01, 6.837E-01, 6.878E-01, 7.222E-01, 7.423E-01,
     2     7.394E-01, 7.247E-01, 6.980E-01, 7.210E-01, 7.472E-01,
     3     7.742E-01, 7.072E-01, 7.117E-01, 7.745E-01, 7.630E-01,
     4     7.660E-01, 7.655E-01, 7.242E-01, 7.045E-01, 7.119E-01,
     5     7.658E-01, 7.632E-01, 7.714E-01, 7.868E-01, 7.923E-01,
     6     7.733E-01, 7.593E-01, 7.763E-01, 8.103E-01, 8.125E-01,
     7     7.776E-01, 7.740E-01, 7.428E-01, 6.991E-01, 6.957E-01,
     8     7.211E-01, 7.373E-01, 7.189E-01, 7.442E-01, 7.465E-01,
     9     7.225E-01, 6.743E-01, 6.768E-01, 6.985E-01, 7.306E-01,
     $     7.463E-01, 7.519E-01, 7.335E-01, 6.768E-01, 6.652E-01,
     1     6.987E-01, 7.011E-01, 7.224E-01, 7.570E-01, 7.992E-01,
     2     7.319E-01, 6.607E-01, 6.157E-01, 6.503E-01, 6.844E-01,
     3     7.321E-01, 7.335E-01, 7.282E-01, 7.199E-01, 7.419E-01,
     4     7.247E-01, 6.977E-01, 6.635E-01, 6.572E-01, 6.699E-01,
     5     7.298E-01, 7.025E-01, 6.830E-01, 6.640E-01, 6.816E-01,
     6     7.071E-01, 7.407E-01, 7.668E-01, 7.337E-01, 6.973E-01,
     7     6.814E-01, 7.625E-01, 8.009E-01, 7.669E-01, 7.089E-01,
     8     7.015E-01, 7.305E-01, 6.986E-01, 6.822E-01, 6.704E-01,
     9     6.877E-01, 7.160E-01, 7.371E-01, 7.144E-01, 6.893E-01,
     $     6.767E-01, 6.850E-01, 6.696E-01, 6.763E-01, 7.090E-01/
      DATA (CRSNO2(I),I= 4801, 4900)/
     1     6.446E-01, 6.238E-01, 6.171E-01, 6.054E-01, 6.060E-01,
     2     6.118E-01, 6.353E-01, 6.415E-01, 6.546E-01, 7.091E-01,
     3     6.839E-01, 6.573E-01, 6.170E-01, 5.720E-01, 5.734E-01,
     4     6.184E-01, 6.083E-01, 6.194E-01, 6.437E-01, 6.421E-01,
     5     6.484E-01, 6.540E-01, 6.299E-01, 6.451E-01, 6.567E-01,
     6     6.269E-01, 6.315E-01, 6.238E-01, 5.774E-01, 5.377E-01,
     7     5.389E-01, 5.985E-01, 6.119E-01, 6.333E-01, 6.737E-01,
     8     6.673E-01, 6.427E-01, 6.062E-01, 6.623E-01, 6.435E-01,
     9     6.059E-01, 6.152E-01, 6.054E-01, 5.926E-01, 5.793E-01,
     $     6.061E-01, 6.330E-01, 6.621E-01, 6.379E-01, 6.230E-01,
     1     6.255E-01, 6.503E-01, 6.513E-01, 6.166E-01, 5.804E-01,
     2     5.689E-01, 5.729E-01, 5.808E-01, 5.650E-01, 5.326E-01,
     3     6.467E-01, 6.567E-01, 6.270E-01, 5.800E-01, 5.760E-01,
     4     5.818E-01, 5.833E-01, 5.856E-01, 5.907E-01, 6.161E-01,
     5     6.210E-01, 6.219E-01, 6.146E-01, 5.932E-01, 5.782E-01,
     6     5.853E-01, 6.097E-01, 6.275E-01, 6.375E-01, 5.854E-01,
     7     5.603E-01, 5.778E-01, 6.368E-01, 6.510E-01, 6.020E-01,
     8     5.738E-01, 5.786E-01, 6.224E-01, 6.294E-01, 6.148E-01,
     9     5.690E-01, 5.710E-01, 5.796E-01, 5.996E-01, 6.451E-01,
     $     6.490E-01, 6.285E-01, 6.240E-01, 6.022E-01, 5.622E-01/
      DATA (CRSNO2(I),I= 4901, 5000)/
     1     5.802E-01, 5.791E-01, 5.672E-01, 5.210E-01, 5.232E-01,
     2     5.451E-01, 5.694E-01, 5.738E-01, 5.722E-01, 5.628E-01,
     3     5.522E-01, 5.391E-01, 5.400E-01, 5.412E-01, 5.420E-01,
     4     5.342E-01, 5.385E-01, 5.431E-01, 5.049E-01, 5.491E-01,
     5     6.146E-01, 6.102E-01, 5.981E-01, 5.796E-01, 5.474E-01,
     6     5.570E-01, 5.737E-01, 5.527E-01, 5.268E-01, 5.012E-01,
     7     5.174E-01, 5.331E-01, 5.558E-01, 6.289E-01, 6.579E-01,
     8     6.726E-01, 5.720E-01, 5.931E-01, 6.430E-01, 6.432E-01,
     9     6.718E-01, 7.063E-01, 6.351E-01, 5.816E-01, 5.249E-01,
     $     5.659E-01, 5.662E-01, 5.580E-01, 6.091E-01, 6.267E-01,
     1     6.353E-01, 5.703E-01, 5.322E-01, 4.933E-01, 5.643E-01,
     2     5.735E-01, 5.591E-01, 5.658E-01, 5.566E-01, 5.320E-01,
     3     4.922E-01, 4.929E-01, 5.191E-01, 5.396E-01, 5.411E-01,
     4     5.274E-01, 5.144E-01, 5.079E-01, 5.021E-01, 5.256E-01,
     5     5.391E-01, 5.521E-01, 5.449E-01, 5.354E-01, 5.119E-01,
     6     5.399E-01, 5.573E-01, 5.640E-01, 5.052E-01, 4.670E-01,
     7     4.655E-01, 5.087E-01, 5.359E-01, 4.977E-01, 5.192E-01,
     8     5.365E-01, 4.942E-01, 5.287E-01, 5.752E-01, 6.162E-01,
     9     5.966E-01, 5.627E-01, 5.607E-01, 5.607E-01, 5.615E-01,
     $     5.638E-01, 5.879E-01, 6.346E-01, 6.122E-01, 5.958E-01/
      DATA (CRSNO2(I),I= 5001, 5100)/
     1     5.712E-01, 5.817E-01, 5.644E-01, 4.967E-01, 5.009E-01,
     2     5.090E-01, 5.309E-01, 5.502E-01, 5.704E-01, 6.201E-01,
     3     5.917E-01, 5.626E-01, 5.306E-01, 5.383E-01, 5.527E-01,
     4     5.776E-01, 5.744E-01, 5.672E-01, 5.941E-01, 5.936E-01,
     5     5.822E-01, 5.336E-01, 5.051E-01, 4.684E-01, 5.127E-01,
     6     5.413E-01, 5.813E-01, 5.301E-01, 4.994E-01, 4.779E-01,
     7     4.630E-01, 4.566E-01, 4.739E-01, 4.957E-01, 5.190E-01,
     8     5.556E-01, 5.359E-01, 5.022E-01, 4.628E-01, 4.823E-01,
     9     5.354E-01, 5.344E-01, 5.126E-01, 4.538E-01, 4.778E-01,
     $     4.917E-01, 5.038E-01, 4.922E-01, 4.894E-01, 5.187E-01,
     1     4.985E-01, 4.799E-01, 5.029E-01, 4.933E-01, 4.793E-01,
     2     5.241E-01, 5.206E-01, 4.917E-01, 4.699E-01, 4.792E-01,
     3     5.301E-01, 5.142E-01, 5.025E-01, 4.879E-01, 5.128E-01,
     4     5.329E-01, 5.334E-01, 5.241E-01, 5.148E-01, 5.439E-01,
     5     5.567E-01, 5.693E-01, 5.365E-01, 5.150E-01, 4.826E-01,
     6     5.148E-01, 5.296E-01, 5.250E-01, 5.490E-01, 5.608E-01,
     7     4.993E-01, 4.600E-01, 4.227E-01, 4.866E-01, 5.060E-01,
     8     5.177E-01, 5.146E-01, 5.175E-01, 5.317E-01, 5.105E-01,
     9     4.983E-01, 5.022E-01, 4.684E-01, 4.325E-01, 3.910E-01,
     $     4.065E-01, 4.453E-01, 4.553E-01, 4.672E-01, 4.934E-01/
      DATA (CRSNO2(I),I= 5101, 5200)/
     1     4.922E-01, 4.921E-01, 4.960E-01, 5.280E-01, 5.525E-01,
     2     5.187E-01, 4.952E-01, 4.667E-01, 4.726E-01, 4.874E-01,
     3     5.235E-01, 5.133E-01, 4.967E-01, 4.418E-01, 4.948E-01,
     4     5.402E-01, 5.276E-01, 5.227E-01, 5.178E-01, 5.190E-01,
     5     5.216E-01, 5.282E-01, 4.978E-01, 4.734E-01, 4.266E-01,
     6     4.665E-01, 5.092E-01, 5.109E-01, 4.693E-01, 3.967E-01,
     7     4.834E-01, 5.159E-01, 5.281E-01, 5.413E-01, 5.462E-01,
     8     5.228E-01, 4.908E-01, 4.612E-01, 5.156E-01, 5.178E-01,
     9     4.937E-01, 5.061E-01, 5.156E-01, 5.336E-01, 5.303E-01,
     $     5.258E-01, 5.203E-01, 5.374E-01, 5.675E-01, 5.799E-01,
     1     5.933E-01, 6.263E-01, 6.094E-01, 5.929E-01, 5.679E-01,
     2     5.780E-01, 5.941E-01, 5.730E-01, 5.580E-01, 5.348E-01,
     3     5.549E-01, 5.618E-01, 5.356E-01, 5.508E-01, 5.715E-01,
     4     5.722E-01, 5.907E-01, 6.366E-01, 6.463E-01, 6.596E-01,
     5     7.081E-01, 6.927E-01, 6.731E-01, 6.659E-01, 6.563E-01,
     6     6.375E-01, 6.330E-01, 6.494E-01, 7.275E-01, 7.631E-01,
     7     7.961E-01, 8.388E-01, 8.348E-01, 8.069E-01, 7.983E-01,
     8     7.988E-01, 8.202E-01, 8.720E-01, 9.276E-01, 9.949E-01,
     9     1.012E+00, 1.020E+00, 1.045E+00, 1.079E+00, 1.200E+00,
     $     1.288E+00, 1.372E+00, 1.410E+00, 1.434E+00, 1.477E+00/
      DATA (CRSNO2(I),I= 5201, 5300)/
     1     1.548E+00, 1.590E+00, 1.629E+00, 1.655E+00, 1.684E+00,
     2     1.750E+00, 1.761E+00, 1.739E+00, 1.555E+00, 1.399E+00,
     3     1.080E+00, 9.256E-01, 7.430E-01, 6.110E-01, 5.554E-01,
     4     5.103E-01, 5.003E-01, 4.929E-01, 4.854E-01, 4.821E-01,
     5     4.783E-01, 4.826E-01, 4.796E-01, 4.576E-01, 4.363E-01,
     6     4.144E-01, 4.151E-01, 4.233E-01, 4.438E-01, 4.317E-01,
     7     4.216E-01, 4.047E-01, 4.045E-01, 4.081E-01, 4.107E-01,
     8     4.177E-01, 4.400E-01, 4.561E-01, 4.711E-01, 4.836E-01,
     9     4.949E-01, 5.148E-01, 5.402E-01, 5.506E-01, 5.292E-01,
     $     5.098E-01, 4.832E-01, 4.727E-01, 4.748E-01, 4.958E-01,
     1     5.177E-01, 5.382E-01, 5.535E-01, 5.458E-01, 5.164E-01,
     2     5.232E-01, 5.266E-01, 5.180E-01, 5.039E-01, 4.839E-01,
     3     5.361E-01, 5.595E-01, 5.729E-01, 5.642E-01, 5.539E-01,
     4     5.534E-01, 5.591E-01, 5.760E-01, 5.757E-01, 5.712E-01,
     5     5.287E-01, 5.212E-01, 5.229E-01, 5.709E-01, 5.985E-01,
     6     6.192E-01, 6.312E-01, 6.455E-01, 6.514E-01, 6.472E-01,
     7     6.161E-01, 6.284E-01, 6.465E-01, 6.957E-01, 7.194E-01,
     8     7.505E-01, 7.501E-01, 7.467E-01, 7.193E-01, 7.146E-01,
     9     7.180E-01, 7.624E-01, 7.962E-01, 8.410E-01, 8.526E-01,
     $     8.640E-01, 8.972E-01, 9.209E-01, 9.737E-01, 1.017E+00/
      DATA (CRSNO2(I),I= 5301, 5400)/
     1     1.076E+00, 1.124E+00, 1.152E+00, 1.201E+00, 1.239E+00,
     2     1.281E+00, 1.332E+00, 1.357E+00, 1.387E+00, 1.436E+00,
     3     1.483E+00, 1.466E+00, 1.431E+00, 1.318E+00, 1.180E+00,
     4     1.057E+00, 9.240E-01, 8.623E-01, 7.774E-01, 7.484E-01,
     5     7.272E-01, 7.066E-01, 7.114E-01, 7.335E-01, 7.516E-01,
     6     7.616E-01, 7.499E-01, 7.596E-01, 7.854E-01, 8.170E-01,
     7     8.432E-01, 8.914E-01, 9.099E-01, 9.335E-01, 9.758E-01,
     8     1.008E+00, 1.080E+00, 1.132E+00, 1.215E+00, 1.275E+00,
     9     1.317E+00, 1.382E+00, 1.425E+00, 1.490E+00, 1.559E+00,
     $     1.602E+00, 1.634E+00, 1.650E+00, 1.673E+00, 1.673E+00,
     1     1.661E+00, 1.586E+00, 1.520E+00, 1.402E+00, 1.320E+00,
     2     1.259E+00, 1.149E+00, 1.094E+00, 1.016E+00, 9.767E-01,
     3     9.489E-01, 9.091E-01, 8.966E-01, 8.825E-01, 8.902E-01,
     4     8.942E-01, 8.885E-01, 8.927E-01, 9.038E-01, 8.778E-01,
     5     8.591E-01, 8.515E-01, 8.674E-01, 9.129E-01, 9.549E-01,
     6     9.867E-01, 9.933E-01, 1.023E+00, 1.114E+00, 1.133E+00,
     7     1.143E+00, 1.084E+00, 1.068E+00, 1.066E+00, 1.117E+00,
     8     1.171E+00, 1.238E+00, 1.272E+00, 1.323E+00, 1.333E+00,
     9     1.345E+00, 1.410E+00, 1.447E+00, 1.504E+00, 1.557E+00,
     $     1.631E+00, 1.690E+00, 1.723E+00, 1.754E+00, 1.807E+00/
      DATA (CRSNO2(I),I= 5401, 5500)/
     1     1.905E+00, 1.918E+00, 1.934E+00, 1.999E+00, 2.020E+00,
     2     2.048E+00, 2.061E+00, 2.070E+00, 2.064E+00, 2.051E+00,
     3     2.016E+00, 2.056E+00, 2.100E+00, 2.099E+00, 2.115E+00,
     4     2.181E+00, 2.253E+00, 2.353E+00, 2.426E+00, 2.478E+00,
     5     2.629E+00, 2.703E+00, 2.812E+00, 2.936E+00, 3.038E+00,
     6     3.215E+00, 3.327E+00, 3.538E+00, 3.629E+00, 3.722E+00,
     7     3.870E+00, 3.954E+00, 4.110E+00, 4.185E+00, 4.273E+00,
     8     4.319E+00, 4.324E+00, 4.232E+00, 4.162E+00, 4.039E+00,
     9     3.861E+00, 3.711E+00, 3.422E+00, 3.242E+00, 2.877E+00,
     $     2.649E+00, 2.399E+00, 2.066E+00, 1.891E+00, 1.624E+00,
     1     1.496E+00, 1.318E+00, 1.244E+00, 1.191E+00, 1.108E+00,
     2     1.060E+00, 9.686E-01, 9.679E-01, 9.693E-01, 9.508E-01,
     3     9.392E-01, 9.141E-01, 8.935E-01, 8.648E-01, 8.946E-01,
     4     9.114E-01, 9.133E-01, 9.224E-01, 9.464E-01, 9.477E-01,
     5     9.503E-01, 1.015E+00, 1.036E+00, 1.032E+00, 1.023E+00,
     6     1.007E+00, 1.007E+00, 1.015E+00, 1.074E+00, 1.059E+00,
     7     9.783E-01, 1.016E+00, 1.065E+00, 1.088E+00, 1.109E+00,
     8     1.174E+00, 1.187E+00, 1.197E+00, 1.202E+00, 1.206E+00,
     9     1.216E+00, 1.230E+00, 1.276E+00, 1.283E+00, 1.292E+00,
     $     1.341E+00, 1.380E+00, 1.481E+00, 1.508E+00, 1.530E+00/
      DATA (CRSNO2(I),I= 5501, 5600)/
     1     1.530E+00, 1.530E+00, 1.552E+00, 1.571E+00, 1.625E+00,
     2     1.624E+00, 1.605E+00, 1.669E+00, 1.724E+00, 1.744E+00,
     3     1.764E+00, 1.827E+00, 1.861E+00, 1.913E+00, 2.012E+00,
     4     2.080E+00, 2.130E+00, 2.161E+00, 2.228E+00, 2.283E+00,
     5     2.356E+00, 2.366E+00, 2.384E+00, 2.477E+00, 2.535E+00,
     6     2.660E+00, 2.722E+00, 2.795E+00, 2.884E+00, 2.940E+00,
     7     3.045E+00, 3.110E+00, 3.240E+00, 3.305E+00, 3.374E+00,
     8     3.433E+00, 3.465E+00, 3.519E+00, 3.539E+00, 3.567E+00,
     9     3.505E+00, 3.440E+00, 3.450E+00, 3.426E+00, 3.262E+00,
     $     3.173E+00, 3.010E+00, 2.897E+00, 2.779E+00, 2.624E+00,
     1     2.537E+00, 2.378E+00, 2.341E+00, 2.307E+00, 2.255E+00,
     2     2.202E+00, 2.164E+00, 2.153E+00, 2.180E+00, 2.188E+00,
     3     2.199E+00, 2.271E+00, 2.351E+00, 2.383E+00, 2.418E+00,
     4     2.560E+00, 2.598E+00, 2.638E+00, 2.703E+00, 2.784E+00,
     5     2.875E+00, 2.930E+00, 3.013E+00, 3.069E+00, 3.196E+00,
     6     3.258E+00, 3.340E+00, 3.463E+00, 3.551E+00, 3.715E+00,
     7     3.775E+00, 3.858E+00, 3.935E+00, 4.055E+00, 4.020E+00,
     8     4.002E+00, 4.072E+00, 4.123E+00, 4.267E+00, 4.304E+00,
     9     4.350E+00, 4.409E+00, 4.472E+00, 4.632E+00, 4.695E+00,
     $     4.707E+00, 4.728E+00, 4.782E+00, 4.814E+00, 4.848E+00/
      DATA (CRSNO2(I),I= 5601, 5700)/
     1     4.788E+00, 4.753E+00, 4.716E+00, 4.716E+00, 4.743E+00,
     2     4.686E+00, 4.584E+00, 4.563E+00, 4.546E+00, 4.510E+00,
     3     4.483E+00, 4.400E+00, 4.365E+00, 4.303E+00, 4.243E+00,
     4     4.179E+00, 4.166E+00, 4.158E+00, 4.146E+00, 4.113E+00,
     5     4.005E+00, 3.977E+00, 3.943E+00, 3.907E+00, 3.883E+00,
     6     3.912E+00, 3.929E+00, 3.974E+00, 4.000E+00, 4.052E+00,
     7     4.038E+00, 4.020E+00, 4.075E+00, 4.104E+00, 4.096E+00,
     8     4.094E+00, 4.092E+00, 4.096E+00, 4.103E+00, 4.080E+00,
     9     4.058E+00, 4.047E+00, 4.035E+00, 3.977E+00, 3.926E+00,
     $     3.786E+00, 3.726E+00, 3.639E+00, 3.622E+00, 3.608E+00,
     1     3.590E+00, 3.581E+00, 3.570E+00, 3.587E+00, 3.646E+00,
     2     3.713E+00, 3.814E+00, 3.878E+00, 3.925E+00, 3.962E+00,
     3     4.004E+00, 4.182E+00, 4.275E+00, 4.466E+00, 4.512E+00,
     4     4.569E+00, 4.694E+00, 4.790E+00, 4.928E+00, 5.007E+00,
     5     5.198E+00, 5.274E+00, 5.412E+00, 5.467E+00, 5.539E+00,
     6     5.558E+00, 5.571E+00, 5.566E+00, 5.545E+00, 5.426E+00,
     7     5.371E+00, 5.261E+00, 5.192E+00, 5.088E+00, 4.963E+00,
     8     4.852E+00, 4.638E+00, 4.532E+00, 4.376E+00, 4.296E+00,
     9     4.118E+00, 4.052E+00, 3.951E+00, 3.842E+00, 3.730E+00,
     $     3.511E+00, 3.393E+00, 3.258E+00, 3.216E+00, 3.174E+00/
      DATA (CRSNO2(I),I= 5701, 5800)/
     1     3.144E+00, 3.083E+00, 3.017E+00, 2.940E+00, 2.964E+00,
     2     2.978E+00, 2.964E+00, 2.962E+00, 2.977E+00, 2.978E+00,
     3     2.973E+00, 2.977E+00, 2.982E+00, 2.973E+00, 2.969E+00,
     4     3.049E+00, 3.089E+00, 3.138E+00, 3.160E+00, 3.207E+00,
     5     3.208E+00, 3.201E+00, 3.241E+00, 3.294E+00, 3.361E+00,
     6     3.405E+00, 3.416E+00, 3.427E+00, 3.475E+00, 3.506E+00,
     7     3.586E+00, 3.611E+00, 3.650E+00, 3.654E+00, 3.660E+00,
     8     3.695E+00, 3.724E+00, 3.830E+00, 3.881E+00, 3.984E+00,
     9     4.012E+00, 4.051E+00, 4.085E+00, 4.149E+00, 4.282E+00,
     $     4.422E+00, 4.531E+00, 4.600E+00, 4.704E+00, 4.763E+00,
     1     4.924E+00, 4.984E+00, 5.100E+00, 5.195E+00, 5.365E+00,
     2     5.466E+00, 5.577E+00, 5.697E+00, 5.780E+00, 5.924E+00,
     3     5.993E+00, 6.130E+00, 6.199E+00, 6.365E+00, 6.454E+00,
     4     6.618E+00, 6.706E+00, 6.811E+00, 6.860E+00, 6.902E+00,
     5     7.024E+00, 7.092E+00, 7.250E+00, 7.297E+00, 7.350E+00,
     6     7.373E+00, 7.419E+00, 7.432E+00, 7.451E+00, 7.466E+00,
     7     7.479E+00, 7.457E+00, 7.440E+00, 7.373E+00, 7.347E+00,
     8     7.310E+00, 7.296E+00, 7.268E+00, 7.242E+00, 7.188E+00,
     9     7.147E+00, 7.094E+00, 7.087E+00, 7.079E+00, 7.032E+00,
     $     7.009E+00, 6.993E+00, 6.992E+00, 7.011E+00, 7.016E+00/
      DATA (CRSNO2(I),I= 5801, 5900)/
     1     7.022E+00, 7.035E+00, 7.063E+00, 7.108E+00, 7.167E+00,
     2     7.192E+00, 7.214E+00, 7.281E+00, 7.316E+00, 7.358E+00,
     3     7.386E+00, 7.476E+00, 7.512E+00, 7.588E+00, 7.607E+00,
     4     7.637E+00, 7.695E+00, 7.781E+00, 7.827E+00, 7.868E+00,
     5     7.897E+00, 7.915E+00, 7.941E+00, 7.956E+00, 7.994E+00,
     6     8.028E+00, 8.144E+00, 8.173E+00, 8.217E+00, 8.224E+00,
     7     8.233E+00, 8.241E+00, 8.250E+00, 8.250E+00, 8.252E+00,
     8     8.289E+00, 8.303E+00, 8.272E+00, 8.263E+00, 8.261E+00,
     9     8.246E+00, 8.191E+00, 8.165E+00, 8.110E+00, 8.063E+00,
     $     7.985E+00, 7.937E+00, 7.881E+00, 7.812E+00, 7.759E+00,
     1     7.692E+00, 7.658E+00, 7.618E+00, 7.599E+00, 7.550E+00,
     2     7.529E+00, 7.481E+00, 7.451E+00, 7.381E+00, 7.359E+00,
     3     7.324E+00, 7.312E+00, 7.295E+00, 7.244E+00, 7.198E+00,
     4     7.125E+00, 7.079E+00, 6.993E+00, 6.953E+00, 6.877E+00,
     5     6.846E+00, 6.780E+00, 6.748E+00, 6.668E+00, 6.643E+00,
     6     6.595E+00, 6.551E+00, 6.475E+00, 6.422E+00, 6.356E+00,
     7     6.297E+00, 6.243E+00, 6.148E+00, 6.090E+00, 6.031E+00,
     8     6.012E+00, 6.041E+00, 6.042E+00, 6.000E+00, 5.978E+00,
     9     5.918E+00, 5.903E+00, 5.882E+00, 5.901E+00, 5.945E+00,
     $     5.995E+00, 6.069E+00, 6.116E+00, 6.165E+00, 6.196E+00/
      DATA (CRSNO2(I),I= 5901, 6000)/
     1     6.221E+00, 6.268E+00, 6.297E+00, 6.370E+00, 6.408E+00,
     2     6.513E+00, 6.554E+00, 6.640E+00, 6.670E+00, 6.730E+00,
     3     6.753E+00, 6.802E+00, 6.846E+00, 6.936E+00, 6.992E+00,
     4     7.079E+00, 7.131E+00, 7.195E+00, 7.286E+00, 7.364E+00,
     5     7.431E+00, 7.473E+00, 7.499E+00, 7.513E+00, 7.551E+00,
     6     7.574E+00, 7.655E+00, 7.693E+00, 7.794E+00, 7.817E+00,
     7     7.841E+00, 7.843E+00, 7.841E+00, 7.854E+00, 7.887E+00,
     8     7.910E+00, 7.951E+00, 7.971E+00, 7.997E+00, 7.988E+00,
     9     7.980E+00, 7.976E+00, 7.974E+00, 7.994E+00, 8.005E+00,
     $     8.012E+00, 8.012E+00, 7.982E+00, 7.973E+00, 7.981E+00,
     1     7.986E+00, 8.008E+00, 8.002E+00, 7.957E+00, 7.942E+00,
     2     7.909E+00, 7.880E+00, 7.810E+00, 7.777E+00, 7.717E+00,
     3     7.715E+00, 7.716E+00, 7.720E+00, 7.726E+00, 7.756E+00,
     4     7.786E+00, 7.821E+00, 7.851E+00, 7.911E+00, 7.953E+00,
     5     8.053E+00, 8.109E+00, 8.217E+00, 8.272E+00, 8.409E+00,
     6     8.467E+00, 8.591E+00, 8.639E+00, 8.748E+00, 8.801E+00,
     7     8.953E+00, 9.022E+00, 9.200E+00, 9.270E+00, 9.427E+00,
     8     9.480E+00, 9.585E+00, 9.641E+00, 9.745E+00, 9.842E+00,
     9     9.999E+00, 1.014E+01, 1.032E+01, 1.045E+01, 1.059E+01,
     $     1.072E+01, 1.084E+01, 1.097E+01, 1.107E+01, 1.124E+01/
      DATA (CRSNO2(I),I= 6001, 6100)/
     1     1.135E+01, 1.153E+01, 1.162E+01, 1.176E+01, 1.182E+01,
     2     1.194E+01, 1.199E+01, 1.211E+01, 1.216E+01, 1.226E+01,
     3     1.229E+01, 1.235E+01, 1.237E+01, 1.242E+01, 1.244E+01,
     4     1.248E+01, 1.250E+01, 1.256E+01, 1.257E+01, 1.262E+01,
     5     1.262E+01, 1.263E+01, 1.262E+01, 1.257E+01, 1.253E+01,
     6     1.246E+01, 1.239E+01, 1.228E+01, 1.219E+01, 1.206E+01,
     7     1.197E+01, 1.185E+01, 1.174E+01, 1.162E+01, 1.146E+01,
     8     1.132E+01, 1.116E+01, 1.103E+01, 1.088E+01, 1.077E+01,
     9     1.061E+01, 1.052E+01, 1.039E+01, 1.032E+01, 1.022E+01,
     $     1.016E+01, 1.006E+01, 1.000E+01, 9.832E+00, 9.754E+00,
     1     9.600E+00, 9.536E+00, 9.416E+00, 9.369E+00, 9.276E+00,
     2     9.244E+00, 9.183E+00, 9.159E+00, 9.096E+00, 9.073E+00,
     3     9.015E+00, 8.991E+00, 8.927E+00, 8.906E+00, 8.856E+00,
     4     8.845E+00, 8.830E+00, 8.836E+00, 8.866E+00, 8.874E+00,
     5     8.891E+00, 8.892E+00, 8.891E+00, 8.897E+00, 8.914E+00,
     6     8.922E+00, 8.944E+00, 8.953E+00, 8.974E+00, 9.001E+00,
     7     9.068E+00, 9.105E+00, 9.182E+00, 9.203E+00, 9.240E+00,
     8     9.260E+00, 9.299E+00, 9.333E+00, 9.396E+00, 9.423E+00,
     9     9.469E+00, 9.476E+00, 9.485E+00, 9.505E+00, 9.538E+00,
     $     9.559E+00, 9.591E+00, 9.584E+00, 9.573E+00, 9.571E+00/
      DATA (CRSNO2(I),I= 6101, 6200)/
     1     9.570E+00, 9.588E+00, 9.614E+00, 9.631E+00, 9.655E+00,
     2     9.654E+00, 9.653E+00, 9.645E+00, 9.635E+00, 9.611E+00,
     3     9.579E+00, 9.550E+00, 9.515E+00, 9.512E+00, 9.509E+00,
     4     9.512E+00, 9.516E+00, 9.503E+00, 9.488E+00, 9.474E+00,
     5     9.457E+00, 9.446E+00, 9.433E+00, 9.435E+00, 9.437E+00,
     6     9.433E+00, 9.429E+00, 9.414E+00, 9.398E+00, 9.397E+00,
     7     9.396E+00, 9.405E+00, 9.416E+00, 9.430E+00, 9.447E+00,
     8     9.487E+00, 9.537E+00, 9.600E+00, 9.678E+00, 9.722E+00,
     9     9.776E+00, 9.803E+00, 9.838E+00, 9.872E+00, 9.919E+00,
     $     9.983E+00, 1.007E+01, 1.014E+01, 1.024E+01, 1.029E+01,
     1     1.036E+01, 1.043E+01, 1.053E+01, 1.058E+01, 1.067E+01,
     2     1.070E+01, 1.075E+01, 1.080E+01, 1.087E+01, 1.093E+01,
     3     1.104E+01, 1.108E+01, 1.116E+01, 1.119E+01, 1.125E+01,
     4     1.129E+01, 1.136E+01, 1.140E+01, 1.147E+01, 1.152E+01,
     5     1.163E+01, 1.169E+01, 1.184E+01, 1.189E+01, 1.201E+01,
     6     1.204E+01, 1.210E+01, 1.213E+01, 1.222E+01, 1.227E+01,
     7     1.238E+01, 1.242E+01, 1.254E+01, 1.258E+01, 1.269E+01,
     8     1.273E+01, 1.282E+01, 1.285E+01, 1.295E+01, 1.297E+01,
     9     1.304E+01, 1.305E+01, 1.308E+01, 1.310E+01, 1.314E+01,
     $     1.316E+01, 1.323E+01, 1.325E+01, 1.329E+01, 1.330E+01/
      DATA (CRSNO2(I),I= 6201, 6300)/
     1     1.330E+01, 1.330E+01, 1.333E+01, 1.334E+01, 1.336E+01,
     2     1.337E+01, 1.340E+01, 1.341E+01, 1.346E+01, 1.348E+01,
     3     1.351E+01, 1.352E+01, 1.353E+01, 1.353E+01, 1.355E+01,
     4     1.356E+01, 1.356E+01, 1.357E+01, 1.355E+01, 1.353E+01,
     5     1.351E+01, 1.349E+01, 1.349E+01, 1.349E+01, 1.350E+01,
     6     1.353E+01, 1.353E+01, 1.354E+01, 1.353E+01, 1.350E+01,
     7     1.349E+01, 1.346E+01, 1.344E+01, 1.338E+01, 1.335E+01,
     8     1.328E+01, 1.325E+01, 1.320E+01, 1.319E+01, 1.317E+01,
     9     1.315E+01, 1.308E+01, 1.304E+01, 1.291E+01, 1.287E+01,
     $     1.276E+01, 1.271E+01, 1.263E+01, 1.259E+01, 1.251E+01,
     1     1.247E+01, 1.235E+01, 1.229E+01, 1.215E+01, 1.206E+01,
     2     1.194E+01, 1.185E+01, 1.175E+01, 1.167E+01, 1.162E+01,
     3     1.157E+01, 1.152E+01, 1.147E+01, 1.142E+01, 1.135E+01,
     4     1.133E+01, 1.130E+01, 1.128E+01, 1.125E+01, 1.122E+01,
     5     1.117E+01, 1.115E+01, 1.110E+01, 1.107E+01, 1.098E+01,
     6     1.095E+01, 1.086E+01, 1.083E+01, 1.077E+01, 1.075E+01,
     7     1.070E+01, 1.068E+01, 1.067E+01, 1.067E+01, 1.067E+01,
     8     1.067E+01, 1.063E+01, 1.061E+01, 1.060E+01, 1.060E+01,
     9     1.062E+01, 1.064E+01, 1.064E+01, 1.062E+01, 1.061E+01,
     $     1.058E+01, 1.059E+01, 1.063E+01, 1.067E+01, 1.076E+01/
      DATA (CRSNO2(I),I= 6301, 6400)/
     1     1.079E+01, 1.084E+01, 1.085E+01, 1.089E+01, 1.091E+01,
     2     1.099E+01, 1.101E+01, 1.106E+01, 1.108E+01, 1.112E+01,
     3     1.114E+01, 1.118E+01, 1.120E+01, 1.125E+01, 1.128E+01,
     4     1.134E+01, 1.139E+01, 1.145E+01, 1.152E+01, 1.156E+01,
     5     1.163E+01, 1.165E+01, 1.169E+01, 1.171E+01, 1.177E+01,
     6     1.180E+01, 1.189E+01, 1.190E+01, 1.193E+01, 1.194E+01,
     7     1.192E+01, 1.192E+01, 1.195E+01, 1.196E+01, 1.198E+01,
     8     1.199E+01, 1.197E+01, 1.195E+01, 1.197E+01, 1.198E+01,
     9     1.202E+01, 1.206E+01, 1.206E+01, 1.205E+01, 1.206E+01,
     $     1.208E+01, 1.210E+01, 1.218E+01, 1.220E+01, 1.225E+01,
     1     1.227E+01, 1.233E+01, 1.236E+01, 1.246E+01, 1.251E+01,
     2     1.260E+01, 1.265E+01, 1.274E+01, 1.280E+01, 1.286E+01,
     3     1.292E+01, 1.297E+01, 1.305E+01, 1.310E+01, 1.319E+01,
     4     1.322E+01, 1.330E+01, 1.334E+01, 1.343E+01, 1.345E+01,
     5     1.353E+01, 1.356E+01, 1.364E+01, 1.366E+01, 1.373E+01,
     6     1.376E+01, 1.381E+01, 1.384E+01, 1.386E+01, 1.389E+01,
     7     1.390E+01, 1.391E+01, 1.393E+01, 1.396E+01, 1.399E+01,
     8     1.405E+01, 1.409E+01, 1.420E+01, 1.423E+01, 1.432E+01,
     9     1.435E+01, 1.440E+01, 1.442E+01, 1.446E+01, 1.448E+01,
     $     1.453E+01, 1.457E+01, 1.460E+01, 1.464E+01, 1.465E+01/
      DATA (CRSNO2(I),I= 6401, 6500)/
     1     1.466E+01, 1.466E+01, 1.465E+01, 1.463E+01, 1.457E+01,
     2     1.456E+01, 1.452E+01, 1.451E+01, 1.451E+01, 1.450E+01,
     3     1.445E+01, 1.443E+01, 1.437E+01, 1.433E+01, 1.430E+01,
     4     1.426E+01, 1.424E+01, 1.420E+01, 1.419E+01, 1.418E+01,
     5     1.418E+01, 1.420E+01, 1.419E+01, 1.415E+01, 1.414E+01,
     6     1.407E+01, 1.404E+01, 1.404E+01, 1.403E+01, 1.404E+01,
     7     1.404E+01, 1.400E+01, 1.393E+01, 1.391E+01, 1.386E+01,
     8     1.383E+01, 1.376E+01, 1.374E+01, 1.365E+01, 1.363E+01,
     9     1.354E+01, 1.351E+01, 1.344E+01, 1.339E+01, 1.334E+01,
     $     1.330E+01, 1.327E+01, 1.322E+01, 1.319E+01, 1.312E+01,
     1     1.308E+01, 1.295E+01, 1.291E+01, 1.275E+01, 1.270E+01,
     2     1.258E+01, 1.253E+01, 1.247E+01, 1.243E+01, 1.237E+01,
     3     1.230E+01, 1.224E+01, 1.213E+01, 1.210E+01, 1.200E+01,
     4     1.198E+01, 1.193E+01, 1.192E+01, 1.187E+01, 1.185E+01,
     5     1.180E+01, 1.177E+01, 1.175E+01, 1.174E+01, 1.174E+01,
     6     1.175E+01, 1.174E+01, 1.171E+01, 1.170E+01, 1.165E+01,
     7     1.165E+01, 1.168E+01, 1.170E+01, 1.174E+01, 1.176E+01,
     8     1.176E+01, 1.177E+01, 1.178E+01, 1.181E+01, 1.183E+01,
     9     1.187E+01, 1.187E+01, 1.187E+01, 1.187E+01, 1.184E+01,
     $     1.184E+01, 1.188E+01, 1.190E+01, 1.195E+01, 1.199E+01/
      DATA (CRSNO2(I),I= 6501, 6600)/
     1     1.200E+01, 1.201E+01, 1.200E+01, 1.198E+01, 1.198E+01,
     2     1.200E+01, 1.201E+01, 1.206E+01, 1.208E+01, 1.210E+01,
     3     1.211E+01, 1.211E+01, 1.211E+01, 1.213E+01, 1.218E+01,
     4     1.219E+01, 1.223E+01, 1.224E+01, 1.224E+01, 1.225E+01,
     5     1.227E+01, 1.228E+01, 1.230E+01, 1.231E+01, 1.236E+01,
     6     1.242E+01, 1.249E+01, 1.263E+01, 1.267E+01, 1.279E+01,
     7     1.282E+01, 1.288E+01, 1.289E+01, 1.289E+01, 1.289E+01,
     8     1.295E+01, 1.301E+01, 1.307E+01, 1.317E+01, 1.320E+01,
     9     1.328E+01, 1.330E+01, 1.332E+01, 1.332E+01, 1.331E+01,
     $     1.331E+01, 1.333E+01, 1.335E+01, 1.340E+01, 1.347E+01,
     1     1.351E+01, 1.361E+01, 1.363E+01, 1.365E+01, 1.365E+01,
     2     1.359E+01, 1.357E+01, 1.366E+01, 1.372E+01, 1.379E+01,
     3     1.389E+01, 1.393E+01, 1.402E+01, 1.404E+01, 1.411E+01,
     4     1.413E+01, 1.419E+01, 1.422E+01, 1.429E+01, 1.435E+01,
     5     1.440E+01, 1.448E+01, 1.452E+01, 1.462E+01, 1.466E+01,
     6     1.476E+01, 1.479E+01, 1.483E+01, 1.485E+01, 1.490E+01,
     7     1.495E+01, 1.497E+01, 1.500E+01, 1.497E+01, 1.490E+01,
     8     1.488E+01, 1.477E+01, 1.474E+01, 1.468E+01, 1.465E+01,
     9     1.461E+01, 1.457E+01, 1.455E+01, 1.450E+01, 1.446E+01,
     $     1.434E+01, 1.431E+01, 1.423E+01, 1.420E+01, 1.418E+01/
      DATA (CRSNO2(I),I= 6601, 6700)/
     1     1.416E+01, 1.414E+01, 1.411E+01, 1.410E+01, 1.406E+01,
     2     1.405E+01, 1.404E+01, 1.403E+01, 1.400E+01, 1.399E+01,
     3     1.396E+01, 1.393E+01, 1.390E+01, 1.385E+01, 1.384E+01,
     4     1.380E+01, 1.379E+01, 1.375E+01, 1.374E+01, 1.373E+01,
     5     1.373E+01, 1.369E+01, 1.364E+01, 1.358E+01, 1.342E+01,
     6     1.340E+01, 1.334E+01, 1.332E+01, 1.330E+01, 1.329E+01,
     7     1.325E+01, 1.321E+01, 1.318E+01, 1.313E+01, 1.311E+01,
     8     1.304E+01, 1.302E+01, 1.295E+01, 1.292E+01, 1.284E+01,
     9     1.277E+01, 1.270E+01, 1.258E+01, 1.254E+01, 1.242E+01,
     $     1.238E+01, 1.227E+01, 1.223E+01, 1.222E+01, 1.220E+01,
     1     1.214E+01, 1.205E+01, 1.203E+01, 1.196E+01, 1.195E+01,
     2     1.192E+01, 1.190E+01, 1.184E+01, 1.179E+01, 1.176E+01,
     3     1.171E+01, 1.171E+01, 1.169E+01, 1.169E+01, 1.170E+01,
     4     1.170E+01, 1.171E+01, 1.171E+01, 1.167E+01, 1.161E+01,
     5     1.158E+01, 1.150E+01, 1.149E+01, 1.147E+01, 1.146E+01,
     6     1.141E+01, 1.138E+01, 1.134E+01, 1.129E+01, 1.129E+01,
     7     1.129E+01, 1.129E+01, 1.130E+01, 1.130E+01, 1.126E+01,
     8     1.123E+01, 1.126E+01, 1.131E+01, 1.132E+01, 1.135E+01,
     9     1.136E+01, 1.139E+01, 1.140E+01, 1.149E+01, 1.155E+01,
     $     1.155E+01, 1.155E+01, 1.157E+01, 1.162E+01, 1.163E+01/
      DATA (CRSNO2(I),I= 6701, 6800)/
     1     1.166E+01, 1.167E+01, 1.170E+01, 1.172E+01, 1.177E+01,
     2     1.188E+01, 1.189E+01, 1.191E+01, 1.192E+01, 1.196E+01,
     3     1.198E+01, 1.192E+01, 1.187E+01, 1.189E+01, 1.194E+01,
     4     1.195E+01, 1.199E+01, 1.200E+01, 1.199E+01, 1.198E+01,
     5     1.198E+01, 1.198E+01, 1.200E+01, 1.206E+01, 1.208E+01,
     6     1.216E+01, 1.219E+01, 1.223E+01, 1.226E+01, 1.228E+01,
     7     1.233E+01, 1.236E+01, 1.247E+01, 1.251E+01, 1.261E+01,
     8     1.267E+01, 1.271E+01, 1.277E+01, 1.279E+01, 1.285E+01,
     9     1.286E+01, 1.287E+01, 1.287E+01, 1.292E+01, 1.296E+01,
     $     1.298E+01, 1.302E+01, 1.304E+01, 1.314E+01, 1.316E+01,
     1     1.315E+01, 1.314E+01, 1.313E+01, 1.312E+01, 1.314E+01,
     2     1.321E+01, 1.323E+01, 1.325E+01, 1.325E+01, 1.325E+01,
     3     1.324E+01, 1.327E+01, 1.333E+01, 1.334E+01, 1.335E+01,
     4     1.335E+01, 1.342E+01, 1.346E+01, 1.350E+01, 1.356E+01,
     5     1.357E+01, 1.358E+01, 1.358E+01, 1.360E+01, 1.361E+01,
     6     1.365E+01, 1.369E+01, 1.368E+01, 1.367E+01, 1.367E+01,
     7     1.365E+01, 1.364E+01, 1.364E+01, 1.363E+01, 1.365E+01,
     8     1.368E+01, 1.367E+01, 1.362E+01, 1.361E+01, 1.355E+01,
     9     1.351E+01, 1.346E+01, 1.338E+01, 1.337E+01, 1.336E+01,
     $     1.335E+01, 1.319E+01, 1.310E+01, 1.302E+01, 1.292E+01/
      DATA (CRSNO2(I),I= 6801, 6900)/
     1     1.289E+01, 1.282E+01, 1.279E+01, 1.270E+01, 1.265E+01,
     2     1.257E+01, 1.249E+01, 1.247E+01, 1.244E+01, 1.243E+01,
     3     1.234E+01, 1.231E+01, 1.223E+01, 1.216E+01, 1.212E+01,
     4     1.202E+01, 1.201E+01, 1.196E+01, 1.195E+01, 1.196E+01,
     5     1.196E+01, 1.195E+01, 1.192E+01, 1.191E+01, 1.191E+01,
     6     1.191E+01, 1.189E+01, 1.188E+01, 1.185E+01, 1.178E+01,
     7     1.177E+01, 1.175E+01, 1.174E+01, 1.167E+01, 1.162E+01,
     8     1.160E+01, 1.156E+01, 1.155E+01, 1.148E+01, 1.146E+01,
     9     1.140E+01, 1.136E+01, 1.131E+01, 1.123E+01, 1.122E+01,
     $     1.121E+01, 1.120E+01, 1.116E+01, 1.113E+01, 1.111E+01,
     1     1.107E+01, 1.107E+01, 1.111E+01, 1.112E+01, 1.110E+01,
     2     1.109E+01, 1.110E+01, 1.110E+01, 1.110E+01, 1.108E+01,
     3     1.107E+01, 1.107E+01, 1.108E+01, 1.108E+01, 1.108E+01,
     4     1.109E+01, 1.114E+01, 1.116E+01, 1.125E+01, 1.134E+01,
     5     1.134E+01, 1.134E+01, 1.133E+01, 1.131E+01, 1.130E+01,
     6     1.128E+01, 1.126E+01, 1.128E+01, 1.134E+01, 1.135E+01,
     7     1.138E+01, 1.139E+01, 1.139E+01, 1.139E+01, 1.138E+01,
     8     1.135E+01, 1.135E+01, 1.138E+01, 1.140E+01, 1.143E+01,
     9     1.147E+01, 1.145E+01, 1.137E+01, 1.135E+01, 1.131E+01,
     $     1.129E+01, 1.135E+01, 1.145E+01, 1.149E+01, 1.165E+01/
      DATA (CRSNO2(I),I= 6901, 7000)/
     1     1.169E+01, 1.157E+01, 1.147E+01, 1.153E+01, 1.165E+01,
     2     1.167E+01, 1.170E+01, 1.171E+01, 1.174E+01, 1.177E+01,
     3     1.178E+01, 1.182E+01, 1.181E+01, 1.175E+01, 1.172E+01,
     4     1.181E+01, 1.193E+01, 1.192E+01, 1.186E+01, 1.185E+01,
     5     1.184E+01, 1.183E+01, 1.183E+01, 1.183E+01, 1.186E+01,
     6     1.199E+01, 1.204E+01, 1.209E+01, 1.214E+01, 1.218E+01,
     7     1.228E+01, 1.230E+01, 1.232E+01, 1.233E+01, 1.235E+01,
     8     1.237E+01, 1.238E+01, 1.240E+01, 1.241E+01, 1.244E+01,
     9     1.245E+01, 1.245E+01, 1.245E+01, 1.246E+01, 1.249E+01,
     $     1.251E+01, 1.251E+01, 1.251E+01, 1.249E+01, 1.244E+01,
     1     1.242E+01, 1.229E+01, 1.222E+01, 1.226E+01, 1.232E+01,
     2     1.233E+01, 1.233E+01, 1.234E+01, 1.234E+01, 1.234E+01,
     3     1.238E+01, 1.248E+01, 1.248E+01, 1.242E+01, 1.240E+01,
     4     1.240E+01, 1.241E+01, 1.239E+01, 1.232E+01, 1.231E+01,
     5     1.230E+01, 1.229E+01, 1.236E+01, 1.250E+01, 1.251E+01,
     6     1.247E+01, 1.245E+01, 1.244E+01, 1.243E+01, 1.240E+01,
     7     1.226E+01, 1.222E+01, 1.221E+01, 1.220E+01, 1.217E+01,
     8     1.211E+01, 1.208E+01, 1.193E+01, 1.186E+01, 1.187E+01,
     9     1.189E+01, 1.189E+01, 1.187E+01, 1.186E+01, 1.170E+01,
     $     1.157E+01, 1.152E+01, 1.137E+01, 1.136E+01, 1.137E+01/
      DATA (CRSNO2(I),I= 7001, 7100)/
     1     1.137E+01, 1.139E+01, 1.141E+01, 1.140E+01, 1.130E+01,
     2     1.128E+01, 1.130E+01, 1.132E+01, 1.127E+01, 1.113E+01,
     3     1.111E+01, 1.114E+01, 1.116E+01, 1.117E+01, 1.119E+01,
     4     1.117E+01, 1.105E+01, 1.101E+01, 1.098E+01, 1.094E+01,
     5     1.095E+01, 1.100E+01, 1.101E+01, 1.089E+01, 1.080E+01,
     6     1.079E+01, 1.077E+01, 1.076E+01, 1.066E+01, 1.062E+01,
     7     1.060E+01, 1.057E+01, 1.059E+01, 1.067E+01, 1.069E+01,
     8     1.067E+01, 1.065E+01, 1.061E+01, 1.046E+01, 1.044E+01,
     9     1.055E+01, 1.063E+01, 1.066E+01, 1.072E+01, 1.072E+01,
     $     1.066E+01, 1.063E+01, 1.070E+01, 1.082E+01, 1.082E+01,
     1     1.082E+01, 1.081E+01, 1.072E+01, 1.061E+01, 1.064E+01,
     2     1.076E+01, 1.079E+01, 1.081E+01, 1.082E+01, 1.079E+01,
     3     1.072E+01, 1.072E+01, 1.081E+01, 1.087E+01, 1.093E+01,
     4     1.106E+01, 1.107E+01, 1.108E+01, 1.108E+01, 1.112E+01,
     5     1.119E+01, 1.121E+01, 1.134E+01, 1.137E+01, 1.143E+01,
     6     1.150E+01, 1.154E+01, 1.169E+01, 1.171E+01, 1.163E+01,
     7     1.157E+01, 1.158E+01, 1.161E+01, 1.162E+01, 1.170E+01,
     8     1.175E+01, 1.170E+01, 1.162E+01, 1.162E+01, 1.171E+01,
     9     1.175E+01, 1.185E+01, 1.201E+01, 1.200E+01, 1.186E+01,
     $     1.182E+01, 1.180E+01, 1.177E+01, 1.175E+01, 1.168E+01/
      DATA (CRSNO2(I),I= 7101, 7176)/
     1     1.167E+01, 1.171E+01, 1.177E+01, 1.182E+01, 1.200E+01,
     2     1.204E+01, 1.205E+01, 1.205E+01, 1.204E+01, 1.202E+01,
     3     1.201E+01, 1.199E+01, 1.198E+01, 1.199E+01, 1.202E+01,
     4     1.203E+01, 1.212E+01, 1.217E+01, 1.222E+01, 1.232E+01,
     5     1.233E+01, 1.227E+01, 1.225E+01, 1.229E+01, 1.236E+01,
     6     1.238E+01, 1.250E+01, 1.255E+01, 1.252E+01, 1.247E+01,
     7     1.244E+01, 1.231E+01, 1.225E+01, 1.211E+01, 1.192E+01,
     8     1.189E+01, 1.180E+01, 1.177E+01, 1.176E+01, 1.174E+01,
     9     1.172E+01, 1.165E+01, 1.163E+01, 1.159E+01, 1.156E+01,
     $     1.154E+01, 1.148E+01, 1.146E+01, 1.141E+01, 1.136E+01,
     1     1.133E+01, 1.125E+01, 1.123E+01, 1.122E+01, 1.120E+01,
     2     1.119E+01, 1.114E+01, 1.113E+01, 1.110E+01, 1.107E+01,
     3     1.108E+01, 1.111E+01, 1.111E+01, 1.110E+01, 1.110E+01,
     4     1.110E+01, 1.110E+01, 1.110E+01, 1.108E+01, 1.107E+01,
     5     1.107E+01, 1.108E+01, 1.108E+01, 1.108E+01, 1.109E+01,
     6     1.110E+01/
c
      if (v .lt. vbeg .or. v .gt. vend) then
         cross = 0.0
      else if (v .eq. vend) then
c        This check prevents n from exceeding nmax in the algorithm below.
         cross = crsno2(nmax)
      else
         xi = (v-vbeg)/vincr+1.
         n = xi+1.+1.E-6
         xd = xi-n
         cross = crsno2(n)+xd*(crsno2(n)-crsno2(n-1))
      endif   
      return
      end
      SUBROUTINE O3CHAP(V,C0,CT1,CT2)
C
C     CHAPPUIS AND WULF BAND
C
C     BEGINNING AND ENDING FREQUENCIES (CM-1):   9170.0 24565.0
C     CROSS-SECTIONS IN CM^2 TIMES 1.0E20
C     FORMULA FOR CROSS SECTION:  X+Y*DT+Z*DT*DT, DT=T-273.15
C     THE OUTPUT OF THIS ROUTINE IS C0=X, CT1=Y AND CT2=Z.
C
      PARAMETER (NMAX=3080)
      dimension    X(3080), Y(3080), Z(3080)
c
      DATA VBEG, VEND, VINCR /9170.0, 24565.0, 5.0/      
      DATA (X(I),I=    1,  100)/
     1      4.276E-05,  4.775E-05,  5.825E-05,  6.908E-05,  7.299E-05,
     2      7.116E-05,  7.388E-05,  7.965E-05,  7.689E-05,  6.900E-05,
     3      7.008E-05,  6.945E-05,  7.083E-05,  7.053E-05,  6.908E-05,
     4      6.923E-05,  6.770E-05,  7.146E-05,  7.749E-05,  8.464E-05,
     5      8.441E-05,  8.754E-05,  8.795E-05,  9.971E-05,  9.632E-05,
     6      9.539E-05,  1.037E-04,  1.085E-04,  1.058E-04,  1.077E-04,
     7      1.121E-04,  1.193E-04,  1.292E-04,  1.364E-04,  1.526E-04,
     8      1.658E-04,  1.808E-04,  1.861E-04,  1.786E-04,  1.804E-04,
     9      1.885E-04,  1.972E-04,  2.218E-04,  2.408E-04,  2.317E-04,
     $      2.098E-04,  1.938E-04,  1.851E-04,  1.896E-04,  1.875E-04,
     1      1.708E-04,  1.710E-04,  1.796E-04,  1.865E-04,  1.943E-04,
     2      1.881E-04,  1.885E-04,  2.136E-04,  2.255E-04,  2.267E-04,
     3      2.234E-04,  2.418E-04,  2.695E-04,  2.710E-04,  2.738E-04,
     4      3.066E-04,  3.269E-04,  3.465E-04,  3.986E-04,  4.410E-04,
     5      4.719E-04,  5.051E-04,  5.211E-04,  5.132E-04,  5.125E-04,
     6      5.159E-04,  5.549E-04,  6.562E-04,  7.168E-04,  6.502E-04,
     7      5.140E-04,  4.161E-04,  3.620E-04,  3.264E-04,  3.004E-04,
     8      2.815E-04,  2.650E-04,  2.527E-04,  2.424E-04,  2.292E-04,
     9      2.155E-04,  2.072E-04,  1.992E-04,  1.943E-04,  1.914E-04,
     $      1.855E-04,  1.813E-04,  1.724E-04,  1.687E-04,  1.676E-04/
      DATA (X(I),I=  101,  200)/
     1      1.601E-04,  1.503E-04,  1.518E-04,  1.436E-04,  1.455E-04,
     2      1.448E-04,  1.410E-04,  1.406E-04,  1.425E-04,  1.407E-04,
     3      1.405E-04,  1.436E-04,  1.369E-04,  1.355E-04,  1.331E-04,
     4      1.328E-04,  1.350E-04,  1.394E-04,  1.372E-04,  1.444E-04,
     5      1.490E-04,  1.455E-04,  1.460E-04,  1.523E-04,  1.559E-04,
     6      1.654E-04,  1.766E-04,  1.843E-04,  1.911E-04,  1.881E-04,
     7      1.894E-04,  1.927E-04,  2.043E-04,  2.106E-04,  2.215E-04,
     8      2.268E-04,  2.249E-04,  2.230E-04,  2.302E-04,  2.408E-04,
     9      2.518E-04,  2.625E-04,  2.753E-04,  2.788E-04,  2.701E-04,
     $      2.746E-04,  2.935E-04,  3.173E-04,  3.457E-04,  3.452E-04,
     1      3.329E-04,  3.443E-04,  3.706E-04,  4.079E-04,  4.403E-04,
     2      4.343E-04,  4.172E-04,  4.448E-04,  5.132E-04,  5.635E-04,
     3      5.590E-04,  5.419E-04,  6.007E-04,  6.912E-04,  7.258E-04,
     4      7.146E-04,  7.529E-04,  8.706E-04,  9.465E-04,  9.923E-04,
     5      1.134E-03,  1.286E-03,  1.351E-03,  1.485E-03,  1.709E-03,
     6      1.897E-03,  2.086E-03,  2.186E-03,  2.195E-03,  2.185E-03,
     7      2.199E-03,  2.336E-03,  2.666E-03,  3.076E-03,  3.075E-03,
     8      2.543E-03,  1.920E-03,  1.498E-03,  1.283E-03,  1.165E-03,
     9      1.070E-03,  9.833E-04,  9.018E-04,  8.207E-04,  7.451E-04,
     $      6.811E-04,  6.178E-04,  5.661E-04,  5.199E-04,  4.868E-04/
      DATA (X(I),I=  201,  300)/
     1      4.541E-04,  4.291E-04,  4.135E-04,  3.990E-04,  3.878E-04,
     2      3.815E-04,  3.722E-04,  3.691E-04,  3.726E-04,  3.711E-04,
     3      3.744E-04,  3.778E-04,  3.808E-04,  3.826E-04,  3.852E-04,
     4      3.919E-04,  3.975E-04,  3.990E-04,  4.053E-04,  4.176E-04,
     5      4.232E-04,  4.291E-04,  4.414E-04,  4.541E-04,  4.723E-04,
     6      4.887E-04,  5.058E-04,  5.226E-04,  5.501E-04,  5.836E-04,
     7      6.059E-04,  6.238E-04,  6.469E-04,  6.711E-04,  7.046E-04,
     8      7.448E-04,  7.794E-04,  8.054E-04,  8.222E-04,  8.371E-04,
     9      8.538E-04,  8.612E-04,  8.698E-04,  8.914E-04,  9.122E-04,
     $      9.305E-04,  9.562E-04,  9.844E-04,  1.018E-03,  1.053E-03,
     1      1.091E-03,  1.136E-03,  1.187E-03,  1.233E-03,  1.289E-03,
     2      1.336E-03,  1.372E-03,  1.405E-03,  1.435E-03,  1.470E-03,
     3      1.504E-03,  1.517E-03,  1.511E-03,  1.541E-03,  1.619E-03,
     4      1.728E-03,  1.848E-03,  1.955E-03,  2.044E-03,  2.128E-03,
     5      2.254E-03,  2.396E-03,  2.527E-03,  2.660E-03,  2.832E-03,
     6      3.010E-03,  3.182E-03,  3.340E-03,  3.504E-03,  3.673E-03,
     7      3.822E-03,  3.923E-03,  3.997E-03,  4.042E-03,  4.061E-03,
     8      4.035E-03,  3.979E-03,  3.901E-03,  3.785E-03,  3.642E-03,
     9      3.494E-03,  3.339E-03,  3.173E-03,  3.004E-03,  2.849E-03,
     $      2.703E-03,  2.556E-03,  2.432E-03,  2.310E-03,  2.191E-03/
      DATA (X(I),I=  301,  400)/
     1      2.076E-03,  1.969E-03,  1.883E-03,  1.818E-03,  1.753E-03,
     2      1.705E-03,  1.672E-03,  1.643E-03,  1.617E-03,  1.616E-03,
     3      1.629E-03,  1.648E-03,  1.662E-03,  1.667E-03,  1.669E-03,
     4      1.664E-03,  1.655E-03,  1.645E-03,  1.643E-03,  1.642E-03,
     5      1.632E-03,  1.629E-03,  1.632E-03,  1.638E-03,  1.644E-03,
     6      1.647E-03,  1.646E-03,  1.642E-03,  1.638E-03,  1.632E-03,
     7      1.628E-03,  1.626E-03,  1.628E-03,  1.635E-03,  1.642E-03,
     8      1.649E-03,  1.653E-03,  1.656E-03,  1.660E-03,  1.669E-03,
     9      1.685E-03,  1.705E-03,  1.730E-03,  1.755E-03,  1.779E-03,
     $      1.804E-03,  1.830E-03,  1.861E-03,  1.896E-03,  1.931E-03,
     1      1.962E-03,  1.991E-03,  2.024E-03,  2.068E-03,  2.131E-03,
     2      2.207E-03,  2.285E-03,  2.357E-03,  2.423E-03,  2.490E-03,
     3      2.564E-03,  2.649E-03,  2.743E-03,  2.842E-03,  2.943E-03,
     4      3.044E-03,  3.146E-03,  3.248E-03,  3.350E-03,  3.452E-03,
     5      3.555E-03,  3.664E-03,  3.785E-03,  3.927E-03,  4.083E-03,
     6      4.250E-03,  4.418E-03,  4.570E-03,  4.708E-03,  4.835E-03,
     7      4.961E-03,  5.088E-03,  5.218E-03,  5.348E-03,  5.471E-03,
     8      5.594E-03,  5.713E-03,  5.828E-03,  5.933E-03,  6.026E-03,
     9      6.100E-03,  6.152E-03,  6.186E-03,  6.193E-03,  6.182E-03,
     $      6.149E-03,  6.093E-03,  6.011E-03,  5.914E-03,  5.799E-03/
      DATA (X(I),I=  401,  500)/
     1      5.676E-03,  5.553E-03,  5.438E-03,  5.330E-03,  5.233E-03,
     2      5.151E-03,  5.080E-03,  5.025E-03,  4.987E-03,  4.972E-03,
     3      4.976E-03,  4.991E-03,  5.013E-03,  5.032E-03,  5.043E-03,
     4      5.043E-03,  5.032E-03,  5.010E-03,  4.980E-03,  4.950E-03,
     5      4.913E-03,  4.879E-03,  4.838E-03,  4.786E-03,  4.723E-03,
     6      4.652E-03,  4.578E-03,  4.503E-03,  4.433E-03,  4.366E-03,
     7      4.306E-03,  4.247E-03,  4.191E-03,  4.135E-03,  4.083E-03,
     8      4.035E-03,  3.997E-03,  3.968E-03,  3.945E-03,  3.923E-03,
     9      3.904E-03,  3.886E-03,  3.867E-03,  3.856E-03,  3.848E-03,
     $      3.845E-03,  3.848E-03,  3.860E-03,  3.878E-03,  3.897E-03,
     1      3.915E-03,  3.941E-03,  3.971E-03,  4.008E-03,  4.057E-03,
     2      4.113E-03,  4.176E-03,  4.243E-03,  4.325E-03,  4.418E-03,
     3      4.518E-03,  4.626E-03,  4.723E-03,  4.812E-03,  4.891E-03,
     4      4.972E-03,  5.058E-03,  5.155E-03,  5.263E-03,  5.382E-03,
     5      5.516E-03,  5.657E-03,  5.810E-03,  5.974E-03,  6.145E-03,
     6      6.331E-03,  6.532E-03,  6.752E-03,  6.993E-03,  7.247E-03,
     7      7.507E-03,  7.768E-03,  8.036E-03,  8.304E-03,  8.579E-03,
     8      8.862E-03,  9.148E-03,  9.442E-03,  9.744E-03,  1.006E-02,
     9      1.038E-02,  1.071E-02,  1.104E-02,  1.137E-02,  1.168E-02,
     $      1.195E-02,  1.220E-02,  1.242E-02,  1.264E-02,  1.283E-02/
      DATA (X(I),I=  501,  600)/
     1      1.303E-02,  1.322E-02,  1.339E-02,  1.356E-02,  1.371E-02,
     2      1.385E-02,  1.398E-02,  1.408E-02,  1.415E-02,  1.417E-02,
     3      1.415E-02,  1.408E-02,  1.395E-02,  1.376E-02,  1.353E-02,
     4      1.326E-02,  1.295E-02,  1.262E-02,  1.228E-02,  1.194E-02,
     5      1.161E-02,  1.128E-02,  1.097E-02,  1.067E-02,  1.038E-02,
     6      1.011E-02,  9.859E-03,  9.625E-03,  9.409E-03,  9.208E-03,
     7      9.022E-03,  8.843E-03,  8.668E-03,  8.505E-03,  8.348E-03,
     8      8.207E-03,  8.088E-03,  7.987E-03,  7.909E-03,  7.842E-03,
     9      7.782E-03,  7.727E-03,  7.675E-03,  7.619E-03,  7.570E-03,
     $      7.526E-03,  7.488E-03,  7.459E-03,  7.440E-03,  7.429E-03,
     1      7.429E-03,  7.429E-03,  7.440E-03,  7.455E-03,  7.474E-03,
     2      7.500E-03,  7.529E-03,  7.563E-03,  7.593E-03,  7.622E-03,
     3      7.649E-03,  7.675E-03,  7.715E-03,  7.771E-03,  7.846E-03,
     4      7.939E-03,  8.039E-03,  8.147E-03,  8.255E-03,  8.367E-03,
     5      8.482E-03,  8.605E-03,  8.746E-03,  8.903E-03,  9.078E-03,
     6      9.271E-03,  9.472E-03,  9.677E-03,  9.889E-03,  1.011E-02,
     7      1.034E-02,  1.059E-02,  1.085E-02,  1.113E-02,  1.143E-02,
     8      1.174E-02,  1.207E-02,  1.242E-02,  1.277E-02,  1.313E-02,
     9      1.350E-02,  1.388E-02,  1.425E-02,  1.464E-02,  1.503E-02,
     $      1.544E-02,  1.586E-02,  1.628E-02,  1.670E-02,  1.713E-02/
      DATA (X(I),I=  601,  700)/
     1      1.755E-02,  1.796E-02,  1.837E-02,  1.875E-02,  1.911E-02,
     2      1.945E-02,  1.975E-02,  2.002E-02,  2.028E-02,  2.050E-02,
     3      2.070E-02,  2.089E-02,  2.104E-02,  2.117E-02,  2.126E-02,
     4      2.132E-02,  2.135E-02,  2.135E-02,  2.130E-02,  2.123E-02,
     5      2.114E-02,  2.101E-02,  2.087E-02,  2.072E-02,  2.053E-02,
     6      2.032E-02,  2.010E-02,  1.986E-02,  1.963E-02,  1.939E-02,
     7      1.915E-02,  1.891E-02,  1.868E-02,  1.845E-02,  1.821E-02,
     8      1.798E-02,  1.773E-02,  1.746E-02,  1.719E-02,  1.692E-02,
     9      1.666E-02,  1.643E-02,  1.621E-02,  1.598E-02,  1.576E-02,
     $      1.558E-02,  1.542E-02,  1.529E-02,  1.519E-02,  1.509E-02,
     1      1.501E-02,  1.493E-02,  1.484E-02,  1.477E-02,  1.473E-02,
     2      1.471E-02,  1.469E-02,  1.468E-02,  1.468E-02,  1.470E-02,
     3      1.473E-02,  1.475E-02,  1.476E-02,  1.477E-02,  1.480E-02,
     4      1.484E-02,  1.489E-02,  1.497E-02,  1.507E-02,  1.520E-02,
     5      1.533E-02,  1.543E-02,  1.551E-02,  1.557E-02,  1.563E-02,
     6      1.569E-02,  1.575E-02,  1.581E-02,  1.591E-02,  1.602E-02,
     7      1.614E-02,  1.625E-02,  1.637E-02,  1.654E-02,  1.676E-02,
     8      1.698E-02,  1.719E-02,  1.740E-02,  1.762E-02,  1.784E-02,
     9      1.805E-02,  1.827E-02,  1.852E-02,  1.878E-02,  1.906E-02,
     $      1.935E-02,  1.962E-02,  1.985E-02,  2.005E-02,  2.024E-02/
      DATA (X(I),I=  701,  800)/
     1      2.047E-02,  2.073E-02,  2.106E-02,  2.137E-02,  2.167E-02,
     2      2.194E-02,  2.223E-02,  2.256E-02,  2.287E-02,  2.316E-02,
     3      2.344E-02,  2.373E-02,  2.405E-02,  2.445E-02,  2.490E-02,
     4      2.542E-02,  2.592E-02,  2.640E-02,  2.684E-02,  2.729E-02,
     5      2.778E-02,  2.828E-02,  2.876E-02,  2.914E-02,  2.948E-02,
     6      2.979E-02,  3.010E-02,  3.039E-02,  3.066E-02,  3.091E-02,
     7      3.116E-02,  3.138E-02,  3.153E-02,  3.155E-02,  3.152E-02,
     8      3.146E-02,  3.144E-02,  3.138E-02,  3.126E-02,  3.110E-02,
     9      3.092E-02,  3.073E-02,  3.054E-02,  3.033E-02,  3.008E-02,
     $      2.980E-02,  2.947E-02,  2.910E-02,  2.870E-02,  2.832E-02,
     1      2.795E-02,  2.765E-02,  2.735E-02,  2.706E-02,  2.680E-02,
     2      2.656E-02,  2.637E-02,  2.620E-02,  2.604E-02,  2.587E-02,
     3      2.570E-02,  2.551E-02,  2.533E-02,  2.520E-02,  2.514E-02,
     4      2.513E-02,  2.513E-02,  2.512E-02,  2.509E-02,  2.506E-02,
     5      2.504E-02,  2.501E-02,  2.498E-02,  2.494E-02,  2.493E-02,
     6      2.497E-02,  2.508E-02,  2.522E-02,  2.535E-02,  2.545E-02,
     7      2.550E-02,  2.558E-02,  2.568E-02,  2.578E-02,  2.587E-02,
     8      2.592E-02,  2.598E-02,  2.605E-02,  2.619E-02,  2.631E-02,
     9      2.621E-02,  2.617E-02,  2.629E-02,  2.642E-02,  2.654E-02,
     $      2.669E-02,  2.685E-02,  2.700E-02,  2.716E-02,  2.734E-02/
      DATA (X(I),I=  801,  900)/
     1      2.752E-02,  2.772E-02,  2.792E-02,  2.813E-02,  2.834E-02,
     2      2.858E-02,  2.885E-02,  2.913E-02,  2.941E-02,  2.973E-02,
     3      3.005E-02,  3.038E-02,  3.075E-02,  3.117E-02,  3.159E-02,
     4      3.202E-02,  3.246E-02,  3.290E-02,  3.335E-02,  3.384E-02,
     5      3.438E-02,  3.493E-02,  3.547E-02,  3.603E-02,  3.660E-02,
     6      3.718E-02,  3.772E-02,  3.826E-02,  3.879E-02,  3.931E-02,
     7      3.987E-02,  4.042E-02,  4.098E-02,  4.151E-02,  4.199E-02,
     8      4.243E-02,  4.287E-02,  4.316E-02,  4.344E-02,  4.369E-02,
     9      4.392E-02,  4.405E-02,  4.417E-02,  4.429E-02,  4.436E-02,
     $      4.436E-02,  4.438E-02,  4.437E-02,  4.427E-02,  4.416E-02,
     1      4.405E-02,  4.394E-02,  4.383E-02,  4.372E-02,  4.359E-02,
     2      4.344E-02,  4.329E-02,  4.312E-02,  4.299E-02,  4.289E-02,
     3      4.278E-02,  4.269E-02,  4.258E-02,  4.242E-02,  4.227E-02,
     4      4.213E-02,  4.202E-02,  4.194E-02,  4.183E-02,  4.178E-02,
     5      4.179E-02,  4.177E-02,  4.175E-02,  4.174E-02,  4.174E-02,
     6      4.175E-02,  4.177E-02,  4.183E-02,  4.191E-02,  4.199E-02,
     7      4.207E-02,  4.214E-02,  4.219E-02,  4.226E-02,  4.232E-02,
     8      4.239E-02,  4.247E-02,  4.254E-02,  4.261E-02,  4.270E-02,
     9      4.279E-02,  4.287E-02,  4.298E-02,  4.311E-02,  4.323E-02,
     $      4.338E-02,  4.357E-02,  4.375E-02,  4.393E-02,  4.413E-02/
      DATA (X(I),I=  901, 1000)/
     1      4.433E-02,  4.454E-02,  4.475E-02,  4.499E-02,  4.525E-02,
     2      4.551E-02,  4.579E-02,  4.613E-02,  4.645E-02,  4.678E-02,
     3      4.712E-02,  4.749E-02,  4.785E-02,  4.820E-02,  4.856E-02,
     4      4.894E-02,  4.928E-02,  4.963E-02,  4.991E-02,  5.016E-02,
     5      5.042E-02,  5.072E-02,  5.109E-02,  5.144E-02,  5.183E-02,
     6      5.218E-02,  5.251E-02,  5.282E-02,  5.315E-02,  5.351E-02,
     7      5.391E-02,  5.430E-02,  5.471E-02,  5.510E-02,  5.548E-02,
     8      5.588E-02,  5.628E-02,  5.673E-02,  5.722E-02,  5.771E-02,
     9      5.821E-02,  5.874E-02,  5.927E-02,  5.980E-02,  6.034E-02,
     $      6.088E-02,  6.144E-02,  6.197E-02,  6.250E-02,  6.303E-02,
     1      6.352E-02,  6.404E-02,  6.452E-02,  6.493E-02,  6.537E-02,
     2      6.578E-02,  6.617E-02,  6.653E-02,  6.688E-02,  6.722E-02,
     3      6.747E-02,  6.768E-02,  6.788E-02,  6.808E-02,  6.827E-02,
     4      6.842E-02,  6.859E-02,  6.875E-02,  6.884E-02,  6.889E-02,
     5      6.896E-02,  6.900E-02,  6.915E-02,  6.927E-02,  6.942E-02,
     6      6.956E-02,  6.972E-02,  6.990E-02,  7.009E-02,  7.024E-02,
     7      7.043E-02,  7.062E-02,  7.081E-02,  7.102E-02,  7.127E-02,
     8      7.151E-02,  7.175E-02,  7.199E-02,  7.225E-02,  7.248E-02,
     9      7.274E-02,  7.300E-02,  7.325E-02,  7.351E-02,  7.375E-02,
     $      7.402E-02,  7.429E-02,  7.458E-02,  7.488E-02,  7.514E-02/
      DATA (X(I),I= 1001, 1100)/
     1      7.546E-02,  7.575E-02,  7.605E-02,  7.634E-02,  7.667E-02,
     2      7.698E-02,  7.732E-02,  7.767E-02,  7.803E-02,  7.841E-02,
     3      7.879E-02,  7.916E-02,  7.959E-02,  8.001E-02,  8.042E-02,
     4      8.082E-02,  8.118E-02,  8.152E-02,  8.188E-02,  8.224E-02,
     5      8.270E-02,  8.318E-02,  8.367E-02,  8.415E-02,  8.467E-02,
     6      8.518E-02,  8.569E-02,  8.621E-02,  8.673E-02,  8.725E-02,
     7      8.779E-02,  8.831E-02,  8.887E-02,  8.945E-02,  9.003E-02,
     8      9.060E-02,  9.123E-02,  9.187E-02,  9.254E-02,  9.317E-02,
     9      9.382E-02,  9.444E-02,  9.506E-02,  9.570E-02,  9.634E-02,
     $      9.702E-02,  9.769E-02,  9.838E-02,  9.904E-02,  9.968E-02,
     1      1.003E-01,  1.010E-01,  1.016E-01,  1.022E-01,  1.028E-01,
     2      1.033E-01,  1.039E-01,  1.046E-01,  1.053E-01,  1.060E-01,
     3      1.067E-01,  1.075E-01,  1.082E-01,  1.089E-01,  1.096E-01,
     4      1.103E-01,  1.110E-01,  1.117E-01,  1.125E-01,  1.132E-01,
     5      1.139E-01,  1.147E-01,  1.154E-01,  1.162E-01,  1.169E-01,
     6      1.177E-01,  1.184E-01,  1.191E-01,  1.197E-01,  1.203E-01,
     7      1.209E-01,  1.215E-01,  1.221E-01,  1.227E-01,  1.232E-01,
     8      1.238E-01,  1.244E-01,  1.249E-01,  1.254E-01,  1.259E-01,
     9      1.263E-01,  1.268E-01,  1.273E-01,  1.278E-01,  1.283E-01,
     $      1.288E-01,  1.292E-01,  1.296E-01,  1.300E-01,  1.305E-01/
      DATA (X(I),I= 1101, 1200)/
     1      1.309E-01,  1.314E-01,  1.319E-01,  1.324E-01,  1.329E-01,
     2      1.335E-01,  1.340E-01,  1.345E-01,  1.351E-01,  1.356E-01,
     3      1.362E-01,  1.368E-01,  1.374E-01,  1.380E-01,  1.386E-01,
     4      1.392E-01,  1.399E-01,  1.406E-01,  1.413E-01,  1.420E-01,
     5      1.427E-01,  1.434E-01,  1.442E-01,  1.449E-01,  1.457E-01,
     6      1.465E-01,  1.472E-01,  1.479E-01,  1.487E-01,  1.495E-01,
     7      1.502E-01,  1.509E-01,  1.516E-01,  1.523E-01,  1.530E-01,
     8      1.539E-01,  1.547E-01,  1.555E-01,  1.563E-01,  1.571E-01,
     9      1.580E-01,  1.588E-01,  1.596E-01,  1.605E-01,  1.614E-01,
     $      1.623E-01,  1.632E-01,  1.641E-01,  1.649E-01,  1.658E-01,
     1      1.666E-01,  1.675E-01,  1.684E-01,  1.692E-01,  1.701E-01,
     2      1.710E-01,  1.719E-01,  1.728E-01,  1.737E-01,  1.746E-01,
     3      1.756E-01,  1.764E-01,  1.774E-01,  1.783E-01,  1.792E-01,
     4      1.801E-01,  1.810E-01,  1.820E-01,  1.829E-01,  1.838E-01,
     5      1.848E-01,  1.857E-01,  1.866E-01,  1.876E-01,  1.885E-01,
     6      1.893E-01,  1.902E-01,  1.911E-01,  1.920E-01,  1.928E-01,
     7      1.936E-01,  1.945E-01,  1.953E-01,  1.961E-01,  1.969E-01,
     8      1.978E-01,  1.986E-01,  1.994E-01,  2.002E-01,  2.010E-01,
     9      2.018E-01,  2.026E-01,  2.034E-01,  2.041E-01,  2.049E-01,
     $      2.057E-01,  2.065E-01,  2.073E-01,  2.081E-01,  2.089E-01/
      DATA (X(I),I= 1201, 1300)/
     1      2.097E-01,  2.105E-01,  2.113E-01,  2.121E-01,  2.129E-01,
     2      2.137E-01,  2.146E-01,  2.154E-01,  2.163E-01,  2.172E-01,
     3      2.180E-01,  2.190E-01,  2.198E-01,  2.207E-01,  2.216E-01,
     4      2.225E-01,  2.234E-01,  2.243E-01,  2.251E-01,  2.260E-01,
     5      2.269E-01,  2.277E-01,  2.285E-01,  2.294E-01,  2.302E-01,
     6      2.311E-01,  2.320E-01,  2.328E-01,  2.337E-01,  2.346E-01,
     7      2.355E-01,  2.364E-01,  2.372E-01,  2.381E-01,  2.390E-01,
     8      2.398E-01,  2.407E-01,  2.416E-01,  2.424E-01,  2.432E-01,
     9      2.440E-01,  2.448E-01,  2.456E-01,  2.464E-01,  2.473E-01,
     $      2.482E-01,  2.491E-01,  2.500E-01,  2.509E-01,  2.517E-01,
     1      2.525E-01,  2.533E-01,  2.541E-01,  2.550E-01,  2.559E-01,
     2      2.568E-01,  2.577E-01,  2.587E-01,  2.597E-01,  2.607E-01,
     3      2.617E-01,  2.626E-01,  2.636E-01,  2.645E-01,  2.654E-01,
     4      2.663E-01,  2.672E-01,  2.682E-01,  2.692E-01,  2.703E-01,
     5      2.713E-01,  2.724E-01,  2.734E-01,  2.744E-01,  2.754E-01,
     6      2.764E-01,  2.774E-01,  2.784E-01,  2.795E-01,  2.806E-01,
     7      2.816E-01,  2.827E-01,  2.838E-01,  2.850E-01,  2.861E-01,
     8      2.872E-01,  2.884E-01,  2.895E-01,  2.907E-01,  2.918E-01,
     9      2.930E-01,  2.942E-01,  2.954E-01,  2.967E-01,  2.980E-01,
     $      2.993E-01,  3.005E-01,  3.017E-01,  3.029E-01,  3.041E-01/
      DATA (X(I),I= 1301, 1400)/
     1      3.052E-01,  3.064E-01,  3.076E-01,  3.088E-01,  3.100E-01,
     2      3.112E-01,  3.124E-01,  3.136E-01,  3.149E-01,  3.161E-01,
     3      3.173E-01,  3.185E-01,  3.196E-01,  3.208E-01,  3.219E-01,
     4      3.230E-01,  3.242E-01,  3.253E-01,  3.265E-01,  3.277E-01,
     5      3.289E-01,  3.300E-01,  3.312E-01,  3.323E-01,  3.334E-01,
     6      3.345E-01,  3.356E-01,  3.367E-01,  3.378E-01,  3.389E-01,
     7      3.400E-01,  3.410E-01,  3.421E-01,  3.431E-01,  3.441E-01,
     8      3.452E-01,  3.462E-01,  3.472E-01,  3.482E-01,  3.493E-01,
     9      3.503E-01,  3.513E-01,  3.524E-01,  3.534E-01,  3.545E-01,
     $      3.555E-01,  3.566E-01,  3.577E-01,  3.588E-01,  3.599E-01,
     1      3.611E-01,  3.622E-01,  3.632E-01,  3.643E-01,  3.653E-01,
     2      3.664E-01,  3.674E-01,  3.683E-01,  3.692E-01,  3.702E-01,
     3      3.711E-01,  3.721E-01,  3.732E-01,  3.740E-01,  3.751E-01,
     4      3.762E-01,  3.774E-01,  3.781E-01,  3.792E-01,  3.800E-01,
     5      3.811E-01,  3.818E-01,  3.826E-01,  3.837E-01,  3.844E-01,
     6      3.853E-01,  3.863E-01,  3.870E-01,  3.881E-01,  3.889E-01,
     7      3.898E-01,  3.908E-01,  3.916E-01,  3.928E-01,  3.937E-01,
     8      3.946E-01,  3.957E-01,  3.967E-01,  3.976E-01,  3.983E-01,
     9      3.995E-01,  4.002E-01,  4.013E-01,  4.023E-01,  4.032E-01,
     $      4.042E-01,  4.050E-01,  4.062E-01,  4.073E-01,  4.084E-01/
      DATA (X(I),I= 1401, 1500)/
     1      4.095E-01,  4.106E-01,  4.117E-01,  4.130E-01,  4.144E-01,
     2      4.155E-01,  4.165E-01,  4.178E-01,  4.189E-01,  4.200E-01,
     3      4.215E-01,  4.226E-01,  4.241E-01,  4.252E-01,  4.266E-01,
     4      4.280E-01,  4.293E-01,  4.306E-01,  4.321E-01,  4.335E-01,
     5      4.347E-01,  4.362E-01,  4.376E-01,  4.388E-01,  4.403E-01,
     6      4.418E-01,  4.433E-01,  4.450E-01,  4.465E-01,  4.480E-01,
     7      4.495E-01,  4.510E-01,  4.524E-01,  4.540E-01,  4.555E-01,
     8      4.571E-01,  4.585E-01,  4.603E-01,  4.619E-01,  4.634E-01,
     9      4.652E-01,  4.668E-01,  4.683E-01,  4.700E-01,  4.716E-01,
     $      4.734E-01,  4.750E-01,  4.764E-01,  4.782E-01,  4.797E-01,
     1      4.812E-01,  4.830E-01,  4.845E-01,  4.861E-01,  4.875E-01,
     2      4.893E-01,  4.908E-01,  4.920E-01,  4.935E-01,  4.950E-01,
     3      4.964E-01,  4.976E-01,  4.990E-01,  5.003E-01,  5.016E-01,
     4      5.028E-01,  5.043E-01,  5.054E-01,  5.064E-01,  5.073E-01,
     5      5.082E-01,  5.094E-01,  5.104E-01,  5.111E-01,  5.119E-01,
     6      5.126E-01,  5.133E-01,  5.141E-01,  5.146E-01,  5.149E-01,
     7      5.154E-01,  5.159E-01,  5.162E-01,  5.166E-01,  5.167E-01,
     8      5.168E-01,  5.170E-01,  5.171E-01,  5.171E-01,  5.169E-01,
     9      5.166E-01,  5.162E-01,  5.159E-01,  5.155E-01,  5.151E-01,
     $      5.146E-01,  5.139E-01,  5.133E-01,  5.128E-01,  5.120E-01/
      DATA (X(I),I= 1501, 1600)/
     1      5.113E-01,  5.103E-01,  5.092E-01,  5.080E-01,  5.070E-01,
     2      5.059E-01,  5.048E-01,  5.036E-01,  5.022E-01,  5.011E-01,
     3      4.997E-01,  4.984E-01,  4.969E-01,  4.954E-01,  4.939E-01,
     4      4.924E-01,  4.909E-01,  4.893E-01,  4.877E-01,  4.863E-01,
     5      4.845E-01,  4.831E-01,  4.814E-01,  4.798E-01,  4.781E-01,
     6      4.766E-01,  4.748E-01,  4.732E-01,  4.718E-01,  4.703E-01,
     7      4.688E-01,  4.673E-01,  4.658E-01,  4.643E-01,  4.630E-01,
     8      4.615E-01,  4.600E-01,  4.586E-01,  4.572E-01,  4.559E-01,
     9      4.548E-01,  4.536E-01,  4.524E-01,  4.512E-01,  4.501E-01,
     $      4.491E-01,  4.483E-01,  4.475E-01,  4.468E-01,  4.459E-01,
     1      4.450E-01,  4.444E-01,  4.438E-01,  4.431E-01,  4.424E-01,
     2      4.416E-01,  4.412E-01,  4.409E-01,  4.405E-01,  4.401E-01,
     3      4.397E-01,  4.394E-01,  4.392E-01,  4.390E-01,  4.389E-01,
     4      4.386E-01,  4.386E-01,  4.384E-01,  4.384E-01,  4.385E-01,
     5      4.385E-01,  4.385E-01,  4.385E-01,  4.387E-01,  4.387E-01,
     6      4.387E-01,  4.387E-01,  4.387E-01,  4.387E-01,  4.390E-01,
     7      4.391E-01,  4.394E-01,  4.398E-01,  4.398E-01,  4.402E-01,
     8      4.406E-01,  4.410E-01,  4.413E-01,  4.417E-01,  4.421E-01,
     9      4.425E-01,  4.428E-01,  4.432E-01,  4.440E-01,  4.443E-01,
     $      4.448E-01,  4.452E-01,  4.459E-01,  4.467E-01,  4.471E-01/
      DATA (X(I),I= 1601, 1700)/
     1      4.479E-01,  4.486E-01,  4.491E-01,  4.498E-01,  4.505E-01,
     2      4.512E-01,  4.519E-01,  4.525E-01,  4.532E-01,  4.539E-01,
     3      4.547E-01,  4.554E-01,  4.562E-01,  4.569E-01,  4.577E-01,
     4      4.584E-01,  4.592E-01,  4.599E-01,  4.606E-01,  4.614E-01,
     5      4.621E-01,  4.629E-01,  4.636E-01,  4.640E-01,  4.648E-01,
     6      4.655E-01,  4.662E-01,  4.670E-01,  4.675E-01,  4.682E-01,
     7      4.689E-01,  4.697E-01,  4.701E-01,  4.708E-01,  4.712E-01,
     8      4.718E-01,  4.724E-01,  4.729E-01,  4.735E-01,  4.739E-01,
     9      4.742E-01,  4.745E-01,  4.748E-01,  4.751E-01,  4.753E-01,
     $      4.755E-01,  4.757E-01,  4.757E-01,  4.757E-01,  4.756E-01,
     1      4.756E-01,  4.756E-01,  4.753E-01,  4.752E-01,  4.749E-01,
     2      4.747E-01,  4.744E-01,  4.741E-01,  4.737E-01,  4.734E-01,
     3      4.730E-01,  4.725E-01,  4.721E-01,  4.715E-01,  4.708E-01,
     4      4.701E-01,  4.693E-01,  4.686E-01,  4.681E-01,  4.673E-01,
     5      4.663E-01,  4.657E-01,  4.649E-01,  4.641E-01,  4.632E-01,
     6      4.623E-01,  4.615E-01,  4.606E-01,  4.596E-01,  4.588E-01,
     7      4.579E-01,  4.569E-01,  4.561E-01,  4.551E-01,  4.542E-01,
     8      4.532E-01,  4.524E-01,  4.513E-01,  4.506E-01,  4.498E-01,
     9      4.487E-01,  4.479E-01,  4.472E-01,  4.461E-01,  4.454E-01,
     $      4.443E-01,  4.435E-01,  4.428E-01,  4.418E-01,  4.411E-01/
      DATA (X(I),I= 1701, 1800)/
     1      4.400E-01,  4.388E-01,  4.380E-01,  4.368E-01,  4.357E-01,
     2      4.347E-01,  4.338E-01,  4.328E-01,  4.316E-01,  4.305E-01,
     3      4.294E-01,  4.283E-01,  4.272E-01,  4.261E-01,  4.249E-01,
     4      4.235E-01,  4.222E-01,  4.212E-01,  4.201E-01,  4.186E-01,
     5      4.171E-01,  4.159E-01,  4.145E-01,  4.130E-01,  4.115E-01,
     6      4.100E-01,  4.085E-01,  4.070E-01,  4.057E-01,  4.042E-01,
     7      4.028E-01,  4.014E-01,  3.998E-01,  3.982E-01,  3.967E-01,
     8      3.950E-01,  3.935E-01,  3.919E-01,  3.904E-01,  3.892E-01,
     9      3.878E-01,  3.863E-01,  3.848E-01,  3.833E-01,  3.818E-01,
     $      3.803E-01,  3.789E-01,  3.775E-01,  3.761E-01,  3.746E-01,
     1      3.731E-01,  3.718E-01,  3.706E-01,  3.694E-01,  3.681E-01,
     2      3.669E-01,  3.657E-01,  3.646E-01,  3.635E-01,  3.624E-01,
     3      3.613E-01,  3.603E-01,  3.592E-01,  3.581E-01,  3.571E-01,
     4      3.561E-01,  3.550E-01,  3.540E-01,  3.530E-01,  3.520E-01,
     5      3.510E-01,  3.503E-01,  3.495E-01,  3.487E-01,  3.479E-01,
     6      3.472E-01,  3.464E-01,  3.457E-01,  3.450E-01,  3.443E-01,
     7      3.436E-01,  3.429E-01,  3.422E-01,  3.415E-01,  3.409E-01,
     8      3.403E-01,  3.398E-01,  3.392E-01,  3.386E-01,  3.380E-01,
     9      3.375E-01,  3.369E-01,  3.364E-01,  3.358E-01,  3.353E-01,
     $      3.347E-01,  3.342E-01,  3.337E-01,  3.332E-01,  3.327E-01/
      DATA (X(I),I= 1801, 1900)/
     1      3.323E-01,  3.318E-01,  3.313E-01,  3.308E-01,  3.304E-01,
     2      3.300E-01,  3.295E-01,  3.291E-01,  3.286E-01,  3.282E-01,
     3      3.278E-01,  3.275E-01,  3.271E-01,  3.267E-01,  3.264E-01,
     4      3.260E-01,  3.256E-01,  3.251E-01,  3.245E-01,  3.240E-01,
     5      3.235E-01,  3.230E-01,  3.224E-01,  3.219E-01,  3.213E-01,
     6      3.207E-01,  3.202E-01,  3.196E-01,  3.190E-01,  3.185E-01,
     7      3.179E-01,  3.174E-01,  3.169E-01,  3.163E-01,  3.158E-01,
     8      3.152E-01,  3.146E-01,  3.139E-01,  3.132E-01,  3.125E-01,
     9      3.117E-01,  3.110E-01,  3.102E-01,  3.095E-01,  3.087E-01,
     $      3.079E-01,  3.071E-01,  3.063E-01,  3.055E-01,  3.048E-01,
     1      3.039E-01,  3.031E-01,  3.022E-01,  3.014E-01,  3.005E-01,
     2      2.996E-01,  2.988E-01,  2.979E-01,  2.970E-01,  2.961E-01,
     3      2.952E-01,  2.944E-01,  2.935E-01,  2.927E-01,  2.920E-01,
     4      2.913E-01,  2.906E-01,  2.900E-01,  2.893E-01,  2.886E-01,
     5      2.880E-01,  2.874E-01,  2.869E-01,  2.863E-01,  2.858E-01,
     6      2.852E-01,  2.847E-01,  2.842E-01,  2.838E-01,  2.834E-01,
     7      2.830E-01,  2.826E-01,  2.822E-01,  2.818E-01,  2.815E-01,
     8      2.813E-01,  2.811E-01,  2.809E-01,  2.807E-01,  2.805E-01,
     9      2.803E-01,  2.802E-01,  2.803E-01,  2.803E-01,  2.803E-01,
     $      2.803E-01,  2.803E-01,  2.804E-01,  2.804E-01,  2.805E-01/
      DATA (X(I),I= 1901, 2000)/
     1      2.806E-01,  2.807E-01,  2.808E-01,  2.809E-01,  2.810E-01,
     2      2.810E-01,  2.809E-01,  2.808E-01,  2.808E-01,  2.807E-01,
     3      2.806E-01,  2.805E-01,  2.804E-01,  2.801E-01,  2.799E-01,
     4      2.796E-01,  2.794E-01,  2.791E-01,  2.789E-01,  2.785E-01,
     5      2.780E-01,  2.775E-01,  2.770E-01,  2.765E-01,  2.760E-01,
     6      2.755E-01,  2.749E-01,  2.741E-01,  2.734E-01,  2.726E-01,
     7      2.718E-01,  2.710E-01,  2.703E-01,  2.694E-01,  2.684E-01,
     8      2.674E-01,  2.664E-01,  2.654E-01,  2.644E-01,  2.634E-01,
     9      2.624E-01,  2.612E-01,  2.601E-01,  2.589E-01,  2.578E-01,
     $      2.566E-01,  2.555E-01,  2.543E-01,  2.530E-01,  2.517E-01,
     1      2.503E-01,  2.490E-01,  2.477E-01,  2.463E-01,  2.450E-01,
     2      2.436E-01,  2.423E-01,  2.410E-01,  2.396E-01,  2.383E-01,
     3      2.370E-01,  2.356E-01,  2.342E-01,  2.328E-01,  2.314E-01,
     4      2.299E-01,  2.285E-01,  2.271E-01,  2.257E-01,  2.243E-01,
     5      2.230E-01,  2.218E-01,  2.205E-01,  2.192E-01,  2.179E-01,
     6      2.166E-01,  2.153E-01,  2.140E-01,  2.126E-01,  2.113E-01,
     7      2.100E-01,  2.086E-01,  2.073E-01,  2.060E-01,  2.048E-01,
     8      2.036E-01,  2.025E-01,  2.013E-01,  2.001E-01,  1.989E-01,
     9      1.977E-01,  1.967E-01,  1.957E-01,  1.947E-01,  1.937E-01,
     $      1.927E-01,  1.917E-01,  1.907E-01,  1.898E-01,  1.890E-01/
      DATA (X(I),I= 2001, 2100)/
     1      1.883E-01,  1.875E-01,  1.867E-01,  1.859E-01,  1.851E-01,
     2      1.844E-01,  1.836E-01,  1.829E-01,  1.821E-01,  1.813E-01,
     3      1.806E-01,  1.798E-01,  1.791E-01,  1.786E-01,  1.781E-01,
     4      1.776E-01,  1.771E-01,  1.766E-01,  1.761E-01,  1.756E-01,
     5      1.751E-01,  1.747E-01,  1.743E-01,  1.739E-01,  1.735E-01,
     6      1.731E-01,  1.726E-01,  1.722E-01,  1.718E-01,  1.714E-01,
     7      1.710E-01,  1.706E-01,  1.703E-01,  1.698E-01,  1.695E-01,
     8      1.691E-01,  1.686E-01,  1.682E-01,  1.677E-01,  1.673E-01,
     9      1.668E-01,  1.664E-01,  1.660E-01,  1.655E-01,  1.650E-01,
     $      1.646E-01,  1.642E-01,  1.637E-01,  1.633E-01,  1.628E-01,
     1      1.624E-01,  1.619E-01,  1.615E-01,  1.611E-01,  1.607E-01,
     2      1.602E-01,  1.598E-01,  1.593E-01,  1.588E-01,  1.583E-01,
     3      1.578E-01,  1.574E-01,  1.569E-01,  1.564E-01,  1.560E-01,
     4      1.555E-01,  1.551E-01,  1.548E-01,  1.544E-01,  1.540E-01,
     5      1.536E-01,  1.532E-01,  1.528E-01,  1.525E-01,  1.523E-01,
     6      1.520E-01,  1.518E-01,  1.516E-01,  1.514E-01,  1.511E-01,
     7      1.510E-01,  1.511E-01,  1.513E-01,  1.514E-01,  1.516E-01,
     8      1.518E-01,  1.519E-01,  1.521E-01,  1.523E-01,  1.526E-01,
     9      1.528E-01,  1.531E-01,  1.534E-01,  1.537E-01,  1.540E-01,
     $      1.543E-01,  1.547E-01,  1.551E-01,  1.555E-01,  1.560E-01/
      DATA (X(I),I= 2101, 2200)/
     1      1.564E-01,  1.568E-01,  1.572E-01,  1.575E-01,  1.579E-01,
     2      1.581E-01,  1.584E-01,  1.586E-01,  1.589E-01,  1.592E-01,
     3      1.594E-01,  1.596E-01,  1.598E-01,  1.599E-01,  1.600E-01,
     4      1.601E-01,  1.602E-01,  1.603E-01,  1.604E-01,  1.604E-01,
     5      1.602E-01,  1.600E-01,  1.598E-01,  1.596E-01,  1.594E-01,
     6      1.592E-01,  1.590E-01,  1.585E-01,  1.580E-01,  1.574E-01,
     7      1.568E-01,  1.562E-01,  1.555E-01,  1.549E-01,  1.543E-01,
     8      1.535E-01,  1.527E-01,  1.518E-01,  1.510E-01,  1.501E-01,
     9      1.493E-01,  1.484E-01,  1.475E-01,  1.464E-01,  1.453E-01,
     $      1.442E-01,  1.431E-01,  1.420E-01,  1.409E-01,  1.398E-01,
     1      1.386E-01,  1.374E-01,  1.362E-01,  1.350E-01,  1.338E-01,
     2      1.326E-01,  1.314E-01,  1.302E-01,  1.290E-01,  1.278E-01,
     3      1.265E-01,  1.253E-01,  1.241E-01,  1.229E-01,  1.217E-01,
     4      1.204E-01,  1.192E-01,  1.180E-01,  1.169E-01,  1.157E-01,
     5      1.145E-01,  1.133E-01,  1.122E-01,  1.110E-01,  1.099E-01,
     6      1.089E-01,  1.079E-01,  1.069E-01,  1.060E-01,  1.050E-01,
     7      1.040E-01,  1.031E-01,  1.021E-01,  1.013E-01,  1.005E-01,
     8      9.973E-02,  9.897E-02,  9.820E-02,  9.743E-02,  9.664E-02,
     9      9.588E-02,  9.524E-02,  9.462E-02,  9.400E-02,  9.339E-02,
     $      9.279E-02,  9.217E-02,  9.158E-02,  9.098E-02,  9.049E-02/
      DATA (X(I),I= 2201, 2300)/
     1      9.002E-02,  8.958E-02,  8.913E-02,  8.869E-02,  8.827E-02,
     2      8.783E-02,  8.742E-02,  8.712E-02,  8.690E-02,  8.670E-02,
     3      8.648E-02,  8.629E-02,  8.607E-02,  8.588E-02,  8.568E-02,
     4      8.547E-02,  8.525E-02,  8.503E-02,  8.482E-02,  8.462E-02,
     5      8.440E-02,  8.418E-02,  8.397E-02,  8.379E-02,  8.369E-02,
     6      8.359E-02,  8.349E-02,  8.341E-02,  8.332E-02,  8.322E-02,
     7      8.316E-02,  8.305E-02,  8.288E-02,  8.269E-02,  8.251E-02,
     8      8.232E-02,  8.214E-02,  8.195E-02,  8.178E-02,  8.158E-02,
     9      8.133E-02,  8.108E-02,  8.083E-02,  8.057E-02,  8.031E-02,
     $      8.003E-02,  7.976E-02,  7.949E-02,  7.917E-02,  7.874E-02,
     1      7.830E-02,  7.789E-02,  7.744E-02,  7.704E-02,  7.662E-02,
     2      7.620E-02,  7.579E-02,  7.549E-02,  7.519E-02,  7.490E-02,
     3      7.460E-02,  7.432E-02,  7.404E-02,  7.377E-02,  7.347E-02,
     4      7.333E-02,  7.329E-02,  7.323E-02,  7.318E-02,  7.315E-02,
     5      7.310E-02,  7.307E-02,  7.303E-02,  7.304E-02,  7.320E-02,
     6      7.336E-02,  7.351E-02,  7.369E-02,  7.386E-02,  7.402E-02,
     7      7.417E-02,  7.435E-02,  7.458E-02,  7.481E-02,  7.505E-02,
     8      7.529E-02,  7.556E-02,  7.582E-02,  7.607E-02,  7.634E-02,
     9      7.661E-02,  7.695E-02,  7.728E-02,  7.763E-02,  7.797E-02,
     $      7.830E-02,  7.864E-02,  7.895E-02,  7.929E-02,  7.952E-02/
      DATA (X(I),I= 2301, 2400)/
     1      7.975E-02,  7.996E-02,  8.018E-02,  8.039E-02,  8.061E-02,
     2      8.081E-02,  8.102E-02,  8.115E-02,  8.105E-02,  8.096E-02,
     3      8.086E-02,  8.074E-02,  8.062E-02,  8.048E-02,  8.033E-02,
     4      8.019E-02,  7.988E-02,  7.946E-02,  7.907E-02,  7.866E-02,
     5      7.824E-02,  7.784E-02,  7.743E-02,  7.702E-02,  7.658E-02,
     6      7.607E-02,  7.552E-02,  7.497E-02,  7.442E-02,  7.386E-02,
     7      7.328E-02,  7.271E-02,  7.214E-02,  7.149E-02,  7.072E-02,
     8      6.995E-02,  6.917E-02,  6.840E-02,  6.762E-02,  6.684E-02,
     9      6.605E-02,  6.527E-02,  6.453E-02,  6.382E-02,  6.311E-02,
     $      6.240E-02,  6.169E-02,  6.099E-02,  6.027E-02,  5.956E-02,
     1      5.886E-02,  5.817E-02,  5.747E-02,  5.676E-02,  5.607E-02,
     2      5.537E-02,  5.469E-02,  5.400E-02,  5.331E-02,  5.265E-02,
     3      5.205E-02,  5.146E-02,  5.088E-02,  5.028E-02,  4.970E-02,
     4      4.912E-02,  4.853E-02,  4.794E-02,  4.738E-02,  4.685E-02,
     5      4.634E-02,  4.583E-02,  4.532E-02,  4.480E-02,  4.431E-02,
     6      4.380E-02,  4.331E-02,  4.291E-02,  4.260E-02,  4.230E-02,
     7      4.199E-02,  4.168E-02,  4.137E-02,  4.107E-02,  4.078E-02,
     8      4.047E-02,  4.029E-02,  4.016E-02,  4.004E-02,  3.991E-02,
     9      3.980E-02,  3.970E-02,  3.959E-02,  3.949E-02,  3.937E-02,
     $      3.934E-02,  3.933E-02,  3.933E-02,  3.934E-02,  3.935E-02/
      DATA (X(I),I= 2401, 2500)/
     1      3.935E-02,  3.936E-02,  3.936E-02,  3.936E-02,  3.931E-02,
     2      3.925E-02,  3.918E-02,  3.912E-02,  3.905E-02,  3.897E-02,
     3      3.889E-02,  3.881E-02,  3.874E-02,  3.866E-02,  3.855E-02,
     4      3.846E-02,  3.837E-02,  3.826E-02,  3.818E-02,  3.807E-02,
     5      3.795E-02,  3.786E-02,  3.769E-02,  3.748E-02,  3.727E-02,
     6      3.706E-02,  3.686E-02,  3.664E-02,  3.643E-02,  3.622E-02,
     7      3.601E-02,  3.581E-02,  3.561E-02,  3.542E-02,  3.522E-02,
     8      3.503E-02,  3.484E-02,  3.465E-02,  3.446E-02,  3.427E-02,
     9      3.407E-02,  3.386E-02,  3.364E-02,  3.343E-02,  3.322E-02,
     $      3.301E-02,  3.280E-02,  3.259E-02,  3.238E-02,  3.221E-02,
     1      3.209E-02,  3.198E-02,  3.186E-02,  3.175E-02,  3.164E-02,
     2      3.153E-02,  3.143E-02,  3.132E-02,  3.126E-02,  3.136E-02,
     3      3.148E-02,  3.159E-02,  3.170E-02,  3.182E-02,  3.194E-02,
     4      3.206E-02,  3.219E-02,  3.232E-02,  3.253E-02,  3.275E-02,
     5      3.298E-02,  3.320E-02,  3.343E-02,  3.366E-02,  3.389E-02,
     6      3.412E-02,  3.435E-02,  3.455E-02,  3.475E-02,  3.495E-02,
     7      3.515E-02,  3.535E-02,  3.555E-02,  3.573E-02,  3.593E-02,
     8      3.612E-02,  3.625E-02,  3.628E-02,  3.631E-02,  3.634E-02,
     9      3.637E-02,  3.639E-02,  3.641E-02,  3.643E-02,  3.646E-02,
     $      3.646E-02,  3.636E-02,  3.623E-02,  3.611E-02,  3.599E-02/
      DATA (X(I),I= 2501, 2600)/
     1      3.586E-02,  3.572E-02,  3.559E-02,  3.545E-02,  3.531E-02,
     2      3.509E-02,  3.481E-02,  3.453E-02,  3.425E-02,  3.398E-02,
     3      3.369E-02,  3.341E-02,  3.312E-02,  3.284E-02,  3.254E-02,
     4      3.217E-02,  3.180E-02,  3.143E-02,  3.106E-02,  3.069E-02,
     5      3.031E-02,  2.994E-02,  2.956E-02,  2.919E-02,  2.882E-02,
     6      2.845E-02,  2.808E-02,  2.771E-02,  2.734E-02,  2.696E-02,
     7      2.660E-02,  2.622E-02,  2.585E-02,  2.549E-02,  2.512E-02,
     8      2.476E-02,  2.440E-02,  2.404E-02,  2.368E-02,  2.332E-02,
     9      2.297E-02,  2.261E-02,  2.225E-02,  2.193E-02,  2.164E-02,
     $      2.135E-02,  2.106E-02,  2.076E-02,  2.048E-02,  2.019E-02,
     1      1.990E-02,  1.962E-02,  1.935E-02,  1.916E-02,  1.898E-02,
     2      1.881E-02,  1.863E-02,  1.846E-02,  1.828E-02,  1.811E-02,
     3      1.794E-02,  1.777E-02,  1.764E-02,  1.757E-02,  1.749E-02,
     4      1.742E-02,  1.735E-02,  1.728E-02,  1.721E-02,  1.714E-02,
     5      1.708E-02,  1.701E-02,  1.699E-02,  1.699E-02,  1.699E-02,
     6      1.699E-02,  1.699E-02,  1.699E-02,  1.699E-02,  1.699E-02,
     7      1.699E-02,  1.699E-02,  1.700E-02,  1.702E-02,  1.703E-02,
     8      1.704E-02,  1.706E-02,  1.707E-02,  1.708E-02,  1.709E-02,
     9      1.710E-02,  1.710E-02,  1.706E-02,  1.701E-02,  1.696E-02,
     $      1.692E-02,  1.687E-02,  1.683E-02,  1.678E-02,  1.673E-02/
      DATA (X(I),I= 2601, 2700)/
     1      1.668E-02,  1.661E-02,  1.651E-02,  1.642E-02,  1.632E-02,
     2      1.622E-02,  1.612E-02,  1.602E-02,  1.592E-02,  1.582E-02,
     3      1.572E-02,  1.560E-02,  1.545E-02,  1.531E-02,  1.517E-02,
     4      1.503E-02,  1.489E-02,  1.474E-02,  1.460E-02,  1.446E-02,
     5      1.432E-02,  1.420E-02,  1.408E-02,  1.397E-02,  1.386E-02,
     6      1.375E-02,  1.363E-02,  1.352E-02,  1.341E-02,  1.329E-02,
     7      1.318E-02,  1.313E-02,  1.310E-02,  1.308E-02,  1.305E-02,
     8      1.303E-02,  1.300E-02,  1.298E-02,  1.295E-02,  1.292E-02,
     9      1.290E-02,  1.293E-02,  1.297E-02,  1.302E-02,  1.307E-02,
     $      1.311E-02,  1.316E-02,  1.320E-02,  1.325E-02,  1.330E-02,
     1      1.334E-02,  1.341E-02,  1.349E-02,  1.357E-02,  1.366E-02,
     2      1.374E-02,  1.382E-02,  1.390E-02,  1.398E-02,  1.406E-02,
     3      1.414E-02,  1.421E-02,  1.427E-02,  1.433E-02,  1.438E-02,
     4      1.444E-02,  1.450E-02,  1.456E-02,  1.462E-02,  1.467E-02,
     5      1.473E-02,  1.475E-02,  1.474E-02,  1.473E-02,  1.472E-02,
     6      1.471E-02,  1.470E-02,  1.468E-02,  1.467E-02,  1.466E-02,
     7      1.465E-02,  1.461E-02,  1.452E-02,  1.442E-02,  1.433E-02,
     8      1.423E-02,  1.414E-02,  1.405E-02,  1.395E-02,  1.386E-02,
     9      1.377E-02,  1.367E-02,  1.356E-02,  1.344E-02,  1.333E-02,
     $      1.321E-02,  1.310E-02,  1.298E-02,  1.287E-02,  1.275E-02/
      DATA (X(I),I= 2701, 2800)/
     1      1.264E-02,  1.252E-02,  1.236E-02,  1.220E-02,  1.204E-02,
     2      1.187E-02,  1.171E-02,  1.155E-02,  1.138E-02,  1.122E-02,
     3      1.106E-02,  1.089E-02,  1.072E-02,  1.055E-02,  1.038E-02,
     4      1.021E-02,  1.004E-02,  9.872E-03,  9.701E-03,  9.531E-03,
     5      9.361E-03,  9.190E-03,  9.029E-03,  8.896E-03,  8.763E-03,
     6      8.634E-03,  8.503E-03,  8.370E-03,  8.240E-03,  8.108E-03,
     7      7.977E-03,  7.847E-03,  7.717E-03,  7.622E-03,  7.540E-03,
     8      7.457E-03,  7.373E-03,  7.291E-03,  7.209E-03,  7.126E-03,
     9      7.041E-03,  6.961E-03,  6.878E-03,  6.813E-03,  6.782E-03,
     $      6.751E-03,  6.721E-03,  6.690E-03,  6.658E-03,  6.628E-03,
     1      6.599E-03,  6.567E-03,  6.536E-03,  6.508E-03,  6.499E-03,
     2      6.495E-03,  6.493E-03,  6.490E-03,  6.488E-03,  6.484E-03,
     3      6.480E-03,  6.478E-03,  6.474E-03,  6.470E-03,  6.471E-03,
     4      6.473E-03,  6.476E-03,  6.480E-03,  6.482E-03,  6.486E-03,
     5      6.489E-03,  6.493E-03,  6.496E-03,  6.499E-03,  6.501E-03,
     6      6.488E-03,  6.465E-03,  6.444E-03,  6.422E-03,  6.401E-03,
     7      6.381E-03,  6.359E-03,  6.337E-03,  6.316E-03,  6.294E-03,
     8      6.266E-03,  6.203E-03,  6.135E-03,  6.067E-03,  6.002E-03,
     9      5.932E-03,  5.867E-03,  5.797E-03,  5.732E-03,  5.664E-03,
     $      5.596E-03,  5.528E-03,  5.457E-03,  5.388E-03,  5.318E-03/
      DATA (X(I),I= 2801, 2900)/
     1      5.248E-03,  5.177E-03,  5.107E-03,  5.036E-03,  4.966E-03,
     2      4.895E-03,  4.825E-03,  4.781E-03,  4.755E-03,  4.729E-03,
     3      4.703E-03,  4.675E-03,  4.648E-03,  4.622E-03,  4.595E-03,
     4      4.569E-03,  4.542E-03,  4.516E-03,  4.514E-03,  4.517E-03,
     5      4.520E-03,  4.523E-03,  4.527E-03,  4.530E-03,  4.533E-03,
     6      4.539E-03,  4.541E-03,  4.545E-03,  4.550E-03,  4.568E-03,
     7      4.588E-03,  4.609E-03,  4.628E-03,  4.649E-03,  4.669E-03,
     8      4.689E-03,  4.710E-03,  4.731E-03,  4.750E-03,  4.771E-03,
     9      4.796E-03,  4.822E-03,  4.847E-03,  4.869E-03,  4.896E-03,
     $      4.921E-03,  4.945E-03,  4.970E-03,  4.995E-03,  5.020E-03,
     1      5.038E-03,  5.040E-03,  5.038E-03,  5.037E-03,  5.036E-03,
     2      5.036E-03,  5.034E-03,  5.034E-03,  5.031E-03,  5.031E-03,
     3      5.031E-03,  5.019E-03,  4.979E-03,  4.934E-03,  4.892E-03,
     4      4.848E-03,  4.805E-03,  4.763E-03,  4.718E-03,  4.676E-03,
     5      4.632E-03,  4.590E-03,  4.541E-03,  4.475E-03,  4.405E-03,
     6      4.336E-03,  4.268E-03,  4.198E-03,  4.130E-03,  4.060E-03,
     7      3.990E-03,  3.922E-03,  3.852E-03,  3.782E-03,  3.715E-03,
     8      3.646E-03,  3.577E-03,  3.508E-03,  3.439E-03,  3.370E-03,
     9      3.301E-03,  3.232E-03,  3.163E-03,  3.094E-03,  3.026E-03,
     $      2.971E-03,  2.919E-03,  2.868E-03,  2.816E-03,  2.764E-03/
      DATA (X(I),I= 2901, 3000)/
     1      2.712E-03,  2.661E-03,  2.609E-03,  2.557E-03,  2.505E-03,
     2      2.454E-03,  2.416E-03,  2.386E-03,  2.356E-03,  2.326E-03,
     3      2.297E-03,  2.267E-03,  2.237E-03,  2.207E-03,  2.177E-03,
     4      2.148E-03,  2.118E-03,  2.096E-03,  2.087E-03,  2.078E-03,
     5      2.070E-03,  2.061E-03,  2.052E-03,  2.043E-03,  2.034E-03,
     6      2.025E-03,  2.016E-03,  2.007E-03,  2.000E-03,  2.000E-03,
     7      2.001E-03,  2.002E-03,  2.003E-03,  2.004E-03,  2.005E-03,
     8      2.006E-03,  2.007E-03,  2.007E-03,  2.008E-03,  2.009E-03,
     9      2.008E-03,  2.006E-03,  2.003E-03,  2.001E-03,  1.999E-03,
     $      1.997E-03,  1.994E-03,  1.992E-03,  1.990E-03,  1.988E-03,
     1      1.985E-03,  1.980E-03,  1.968E-03,  1.956E-03,  1.944E-03,
     2      1.932E-03,  1.919E-03,  1.907E-03,  1.895E-03,  1.883E-03,
     3      1.871E-03,  1.859E-03,  1.846E-03,  1.827E-03,  1.805E-03,
     4      1.783E-03,  1.761E-03,  1.740E-03,  1.718E-03,  1.696E-03,
     5      1.674E-03,  1.652E-03,  1.631E-03,  1.609E-03,  1.585E-03,
     6      1.557E-03,  1.529E-03,  1.500E-03,  1.472E-03,  1.444E-03,
     7      1.416E-03,  1.388E-03,  1.359E-03,  1.331E-03,  1.303E-03,
     8      1.275E-03,  1.251E-03,  1.228E-03,  1.206E-03,  1.183E-03,
     9      1.161E-03,  1.139E-03,  1.116E-03,  1.094E-03,  1.072E-03,
     $      1.049E-03,  1.027E-03,  1.007E-03,  1.003E-03,  9.991E-04/
      DATA (X(I),I= 3001, 3080)/
     1      9.959E-04,  9.926E-04,  9.891E-04,  9.857E-04,  9.826E-04,
     2      9.790E-04,  9.758E-04,  9.725E-04,  9.692E-04,  9.720E-04,
     3      9.842E-04,  9.967E-04,  1.009E-03,  1.022E-03,  1.034E-03,
     4      1.047E-03,  1.059E-03,  1.071E-03,  1.084E-03,  1.096E-03,
     5      1.109E-03,  1.118E-03,  1.126E-03,  1.133E-03,  1.141E-03,
     6      1.148E-03,  1.156E-03,  1.163E-03,  1.171E-03,  1.178E-03,
     7      1.186E-03,  1.193E-03,  1.200E-03,  1.194E-03,  1.185E-03,
     8      1.176E-03,  1.166E-03,  1.157E-03,  1.148E-03,  1.138E-03,
     9      1.129E-03,  1.120E-03,  1.110E-03,  1.101E-03,  1.091E-03,
     $      1.076E-03,  1.060E-03,  1.044E-03,  1.028E-03,  1.012E-03,
     1      9.955E-04,  9.794E-04,  9.632E-04,  9.473E-04,  9.310E-04,
     2      9.151E-04,  8.982E-04,  8.770E-04,  8.556E-04,  8.339E-04,
     3      8.121E-04,  7.907E-04,  7.689E-04,  7.471E-04,  7.255E-04,
     4      7.038E-04,  6.822E-04,  6.606E-04,  6.376E-04,  6.083E-04,
     5      5.783E-04,  5.478E-04,  5.177E-04,  4.875E-04,  4.574E-04,
     6      4.272E-04,  3.969E-04,  3.668E-04,  3.365E-04,  3.063E-04/
C
      DATA (Y(I),I=    1,  100)/
     1      7.656E-13,  2.726E-13,  3.310E-14,  4.764E-13, -1.707E-13,
     2     -3.745E-13, -1.019E-13,  3.745E-13,  2.038E-13,  4.764E-13,
     3      4.076E-13, -3.310E-14,  6.471E-13,  3.745E-13,  4.764E-13,
     4      8.152E-13,  6.880E-14,  5.783E-13,  3.745E-13, -1.157E-12,
     5      3.745E-13,  3.414E-13,  1.707E-13,  5.452E-13, -1.707E-13,
     6      1.294E-12, -2.700E-13, -2.038E-13,  8.152E-13,  4.076E-13,
     7      5.452E-13,  1.906E-12, -4.076E-13,  8.152E-13,   .000E+00,
     8     -1.972E-12,  2.181E-12,  1.906E-12,  6.620E-14,  1.564E-12,
     9       .000E+00,  1.157E-12, -4.076E-13,  2.038E-12, -1.223E-12,
     $       .000E+00,  2.247E-12,  1.498E-12,  1.839E-12,  6.828E-13,
     1      6.828E-13,  1.498E-12,  4.076E-13,  4.738E-13,  1.498E-12,
     2      4.076E-13,  8.152E-13,  6.828E-13,   .000E+00, -6.828E-13,
     3      1.906E-12,  1.498E-12,  1.906E-12, -8.152E-13,  1.773E-12,
     4      1.498E-12,  5.442E-12,  1.630E-12,  4.076E-12,  1.895E-12,
     5      1.895E-12,  1.101E-12, -1.366E-12,   .000E+00,  1.916E-12,
     6      7.072E-12, -2.181E-12, -4.097E-12,  3.526E-12, -5.992E-12,
     7      3.261E-12,  4.362E-12,  5.442E-12,  1.366E-12,   .000E+00,
     8     -1.324E-13,  2.313E-12,  1.223E-12, -4.076E-13,  2.181E-12,
     9       .000E+00,  1.223E-12,  2.038E-12,  1.498E-12,  4.076E-13,
     $      3.414E-13, -4.076E-13,  1.839E-12,  2.313E-12, -1.324E-13/
      DATA (Y(I),I=  101,  200)/
     1      1.564E-12,  8.152E-13,  4.738E-13,  1.906E-12,  1.498E-12,
     2     -6.620E-14,  8.152E-13,  1.702E-12,  4.076E-13, -8.866E-13,
     3      7.490E-13,  2.038E-13,  9.528E-13, -4.076E-13,  8.152E-13,
     4     -2.038E-13,  2.588E-12,  6.114E-13,   .000E+00,   .000E+00,
     5      1.630E-12, -7.490E-13,  2.313E-12, -6.828E-13,  3.414E-13,
     6      6.166E-13,  7.490E-13,  1.090E-12, -4.076E-13,  3.128E-12,
     7       .000E+00,  2.655E-12,  2.752E-13,  8.152E-13,  2.446E-12,
     8      1.498E-12,  2.313E-12,  4.351E-12,  2.588E-12,  2.313E-12,
     9     -4.076E-13, -9.476E-13,  9.580E-13,  2.752E-13,  1.223E-12,
     $      1.630E-12,  6.257E-12,  3.944E-12,  2.446E-12,   .000E+00,
     1      2.313E-12,  8.152E-13,  5.442E-12,  5.177E-12,  3.811E-12,
     2      2.648E-13,  8.152E-13,  6.807E-12,   .000E+00,  3.546E-12,
     3      2.996E-12,  6.257E-12,   .000E+00, -4.626E-12,   .000E+00,
     4      2.648E-13,  4.362E-12,  6.522E-12,  3.261E-12, -9.783E-12,
     5      9.253E-12,  1.198E-11,  9.253E-12, -9.253E-12,  3.261E-12,
     6     -8.723E-12, -1.084E-11,  2.071E-11,  2.503E-11,  1.419E-11,
     7     -8.723E-12, -9.783E-12,  1.957E-11,  1.957E-11,  1.198E-11,
     8      1.198E-11,  9.783E-12, -5.463E-12,  5.992E-12,  1.088E-11,
     9      8.723E-12,  9.253E-12, -1.630E-12,  5.463E-12,  5.992E-12,
     $      6.257E-12,  1.101E-12,  1.366E-12,  5.992E-12,  8.152E-13/
      DATA (Y(I),I=  201,  300)/
     1      4.076E-12,  4.076E-12, -1.630E-12,  7.072E-12,  5.177E-12,
     2      2.313E-12,  3.811E-12, -1.630E-12,  2.996E-12,  7.072E-12,
     3     -1.366E-12,  3.393E-12,  1.498E-12,  2.313E-12, -6.828E-13,
     4      2.996E-12,  1.366E-12,  7.072E-12,  1.366E-12,  2.181E-12,
     5     -2.181E-12,  4.076E-12,   .000E+00,  4.076E-12, -8.152E-13,
     6     -1.630E-12,  2.181E-12,  3.811E-12,  2.446E-12,  2.731E-12,
     7      2.996E-12,  4.626E-12,  7.623E-12, -1.895E-12,  2.996E-12,
     8     -2.648E-13, -1.630E-12,  3.832E-12,  5.992E-12,  7.623E-12,
     9      7.051E-12,   .000E+00,  8.723E-12,  2.731E-12,  2.160E-12,
     $      9.253E-12,  1.688E-11,  1.361E-11,  5.713E-13,  1.577E-11,
     1      1.851E-11, -1.630E-12,  1.361E-11,   .000E+00,  7.051E-12,
     2      1.525E-11,  5.992E-12,  6.522E-12, -8.723E-12,  1.059E-12,
     3      3.261E-12, -1.198E-11,  1.357E-11,  1.904E-11,  5.463E-12,
     4      1.304E-11, -5.463E-12, -6.522E-12,  3.261E-12, -1.059E-12,
     5      1.198E-11,  4.320E-12,  1.304E-11,  6.522E-12, -6.522E-12,
     6      6.522E-12,  4.353E-11, -2.118E-12,  3.049E-11,  2.609E-11,
     7       .000E+00,  8.807E-12,  4.353E-11,  5.446E-11, -4.403E-12,
     8     -3.049E-11,  6.522E-12, -8.640E-12,  5.217E-11, -4.403E-12,
     9      1.957E-11,   .000E+00,  1.851E-11,  3.913E-11, -5.463E-12,
     $     -1.851E-11,  6.522E-12,  6.522E-12, -6.522E-12,  4.403E-12/
      DATA (Y(I),I=  301,  400)/
     1       .000E+00,  2.829E-11,  1.304E-11,  7.581E-12, -1.304E-11,
     2      1.851E-11,  5.463E-12,  1.198E-11, -7.581E-12,  2.177E-11,
     3      1.093E-11,  1.851E-11,  1.304E-11, -2.202E-12,   .000E+00,
     4      1.957E-11, -6.522E-12,  1.304E-11,  2.071E-11,  1.304E-11,
     5      6.522E-12,  2.503E-11, -6.522E-12,  1.851E-11,  1.851E-11,
     6      6.522E-12,  8.723E-12,  1.525E-11,  8.723E-12,  6.522E-12,
     7     -8.723E-12,  1.525E-11,  1.851E-11,  1.304E-11,  1.525E-11,
     8      1.304E-11,  9.783E-12,  6.522E-12,  2.503E-11,   .000E+00,
     9     -5.463E-12, -1.059E-12,  1.525E-11,  5.463E-12,  2.829E-11,
     $     -3.261E-12,  1.525E-11, -5.463E-12, -3.261E-12,  8.723E-12,
     1      1.198E-11,  9.783E-12,  7.664E-12,  5.463E-12,  3.049E-11,
     2      2.283E-11,  8.723E-12,  2.202E-12,  5.463E-12, -6.522E-12,
     3      3.049E-11,  6.522E-12,  2.503E-11,  3.489E-11,  2.943E-11,
     4     -1.059E-12,  5.992E-11,  3.261E-11,  1.957E-11, -1.304E-11,
     5      2.609E-11,  6.098E-11,  5.217E-11, -1.516E-11,  2.397E-11,
     6      4.403E-12,  3.049E-11, -4.403E-12,  4.353E-11,  2.609E-11,
     7     -2.185E-11,  1.093E-11,  2.609E-11,  2.609E-11,  7.191E-11,
     8     -4.237E-12,  3.701E-11, -1.516E-11,  8.707E-11,  4.794E-11,
     9      6.098E-11,  3.913E-11,  7.402E-11,  1.516E-11, -3.701E-11,
     $      5.006E-11, -3.278E-11,  1.093E-11, -2.118E-12, -2.397E-11/
      DATA (Y(I),I=  401,  500)/
     1      3.701E-11,  8.807E-12, -2.821E-11,  3.489E-11, -8.807E-12,
     2     -1.516E-11, -3.489E-11,  1.516E-11, -2.118E-12,  2.397E-11,
     3     -1.304E-11,  2.609E-11, -2.118E-12,  1.304E-11,  3.489E-11,
     4      3.489E-11,  1.304E-11,  3.489E-11, -1.304E-11,  3.278E-11,
     5      1.093E-11,  1.093E-11,  1.304E-11,  2.118E-12,  3.489E-11,
     6      1.304E-11,  4.403E-12,  2.168E-11,  5.446E-11,  2.397E-11,
     7      1.304E-11,  5.658E-11,  1.093E-11,  3.489E-11,  2.397E-11,
     8     -3.049E-11,  4.353E-11,  2.397E-11,  3.489E-11,  8.807E-12,
     9      1.093E-11,  1.093E-11,  4.353E-11,  2.397E-11,  6.522E-12,
     $      5.446E-11,  6.522E-12,  1.093E-11,  4.353E-11,  3.278E-11,
     1      3.049E-11,  6.522E-12,  2.609E-11,  4.142E-11,  4.353E-11,
     2      6.522E-12,  5.006E-11,  3.701E-11,   .000E+00,  3.049E-11,
     3      2.185E-11,  1.093E-11,  3.489E-11,  2.837E-11,  2.821E-11,
     4      2.397E-11,  2.821E-11,  5.886E-11, -3.701E-11,  5.006E-11,
     5      1.304E-11, -2.397E-11, -1.304E-11,  4.794E-11, -2.118E-12,
     6      8.807E-12,   .000E+00, -1.304E-11, -2.185E-11, -2.609E-11,
     7      7.402E-11,  3.066E-11,   .000E+00,  2.609E-11,  6.098E-11,
     8      8.283E-11,  6.522E-11,  7.402E-11,  4.794E-11,  7.402E-11,
     9      9.587E-11,  1.220E-10,  1.480E-10,   .000E+00, -9.587E-11,
     $     -2.185E-11,  1.220E-10,  1.523E-10,  9.587E-11,  2.609E-11/
      DATA (Y(I),I=  501,  600)/
     1      1.480E-10,  4.370E-11,  7.826E-11,  3.456E-11,  7.826E-11,
     2     -4.370E-11,  1.135E-10,  4.370E-11,  2.524E-10, -8.473E-12,
     3      8.473E-12,  4.370E-11, -2.609E-11, -5.217E-11,  1.043E-10,
     4      4.370E-11,  1.761E-11,   .000E+00,  6.979E-11, -2.185E-11,
     5      9.587E-11,  4.237E-12,  1.177E-10,  9.587E-11,  9.587E-11,
     6      4.794E-11, -1.761E-11,  5.674E-11,  3.066E-11,  9.164E-11,
     7     -6.098E-11, -2.609E-11, -2.609E-11,  1.001E-10, -2.609E-11,
     8     -1.001E-10, -6.522E-11, -3.913E-11, -1.728E-11,  6.098E-11,
     9     -7.859E-11,  1.043E-10,  1.304E-11,  1.304E-11,  1.043E-10,
     $      2.185E-11,  4.337E-11, -6.522E-11,  8.283E-11,  4.237E-12,
     1      4.237E-12,  4.237E-12,  8.283E-11,  5.674E-11, -3.032E-11,
     2       .000E+00,  4.337E-11,  7.402E-11,  1.761E-11,  6.098E-11,
     3      1.304E-11,  1.304E-11,  1.089E-10,  2.185E-11,  1.761E-11,
     4      6.522E-11,  9.587E-11,  8.807E-12,  1.001E-10,  9.587E-11,
     5     -1.728E-11,  1.089E-10,  9.587E-11,  1.304E-11, -3.032E-11,
     6      2.609E-11,  1.220E-10,  2.609E-11,  2.185E-11,  5.217E-11,
     7     -4.237E-12,  2.185E-11, -2.609E-11,  6.979E-11,  2.185E-11,
     8     -1.001E-10,  1.043E-10, -2.185E-11,  1.917E-10,  6.131E-11,
     9     -2.609E-11, -9.587E-11,  2.609E-11,  2.609E-11,  1.220E-10,
     $      4.370E-11,  9.140E-12,  1.043E-10,  2.002E-10, -8.473E-12/
      DATA (Y(I),I=  601,  700)/
     1      1.741E-10,  1.220E-10,  2.002E-10, -4.370E-11,  1.917E-10,
     2     -8.473E-12,   .000E+00,  2.002E-10, -6.065E-11,  1.565E-10,
     3      1.565E-10, -8.473E-12,  1.396E-10,  6.065E-11,  8.740E-11,
     4      6.065E-11,  2.439E-10,   .000E+00,  8.740E-11, -8.473E-12,
     5      8.740E-11,  4.370E-11,  1.043E-10,  1.043E-10,  1.043E-10,
     6     -1.396E-10,  6.065E-11,  9.587E-11,  9.587E-11,  1.565E-10,
     7      8.473E-12,  1.761E-11,  2.439E-10,  1.657E-10,  1.043E-10,
     8      1.043E-10,  3.574E-10,  9.587E-11,  2.002E-10,  9.587E-11,
     9       .000E+00,  4.370E-11, -2.609E-11,  1.480E-10,  2.700E-10,
     $      1.396E-10,  9.587E-11,  1.826E-10,  3.523E-11, -1.761E-11,
     1      1.565E-10,  9.587E-11, -2.609E-11,  1.826E-10,  1.220E-10,
     2      4.370E-11,  8.473E-12,  1.135E-10,  1.761E-11,  5.217E-11,
     3      1.220E-10, -2.609E-11,  1.304E-10,  1.826E-10, -8.473E-12,
     4     -2.609E-11,  1.304E-10,   .000E+00,  4.370E-11, -5.217E-11,
     5      1.657E-10, -1.220E-10, -5.217E-11,  1.761E-11,  7.826E-11,
     6      2.609E-11,  4.370E-11,  9.587E-11,  2.785E-10,  1.396E-10,
     7      7.826E-11,  1.043E-10,  1.480E-10,  1.565E-10,  1.128E-10,
     8       .000E+00,  2.002E-10,   .000E+00, -1.761E-11,  9.587E-11,
     9       .000E+00,  2.002E-10, -1.761E-11,  1.135E-10, -1.220E-10,
     $     -1.480E-10, -5.217E-11, -8.473E-12, -8.473E-12,  8.740E-11/
      DATA (Y(I),I=  701,  800)/
     1      2.002E-10,  5.217E-11, -1.396E-10,  1.565E-10, -1.396E-10,
     2     -1.311E-10,  1.917E-10,  1.917E-10,  8.473E-12,  3.523E-11,
     3      1.043E-10,  3.483E-10,  9.587E-11,  2.876E-10,  5.217E-11,
     4      2.961E-10,  4.004E-10, -6.912E-11,  2.961E-10,  1.396E-10,
     5      3.523E-11,   .000E+00,  1.043E-10,  3.483E-10,  1.396E-10,
     6      3.130E-10,  1.213E-10, -1.695E-11,  4.357E-10,  3.835E-10,
     7      3.835E-10,  5.217E-11,  1.043E-10, -1.695E-11,  5.400E-10,
     8      1.043E-10,  1.695E-11,  5.217E-11,  1.396E-10,  1.396E-10,
     9     -5.231E-10, -2.791E-10,  5.217E-11,  2.439E-10,  1.695E-11,
     $      7.045E-11,  3.483E-10,  1.213E-10, -8.740E-11,  3.523E-11,
     1      2.270E-10,   .000E+00, -3.523E-11,  8.740E-11,  1.565E-10,
     2      2.439E-10,  8.740E-11, -6.912E-11,  7.045E-11, -2.609E-10,
     3     -3.523E-11,  3.523E-11,  9.587E-11,  9.587E-11,  1.565E-10,
     4      7.893E-11,  7.893E-11,  9.587E-11,  1.396E-10,  1.917E-10,
     5      1.565E-10,  2.087E-10,  2.354E-10,  2.002E-10,   .000E+00,
     6      2.087E-10,  2.961E-10,  9.587E-11,  2.524E-10,  1.043E-10,
     7      6.912E-11,  3.835E-10, -5.217E-11, -3.523E-11, -2.609E-10,
     8      1.565E-10, -1.213E-10,  2.961E-10,  4.526E-10,  2.754E-06,
     9      1.907E-05,  3.104E-05,  3.136E-05,  3.152E-05,  3.186E-05,
     $      3.213E-05,  3.229E-05,  3.206E-05,  3.156E-05,  3.063E-05/
      DATA (Y(I),I=  801,  900)/
     1      3.098E-05,  3.197E-05,  3.271E-05,  3.315E-05,  3.262E-05,
     2      3.201E-05,  3.129E-05,  3.148E-05,  3.206E-05,  3.175E-05,
     3      3.148E-05,  3.167E-05,  3.159E-05,  3.120E-05,  3.117E-05,
     4      3.109E-05,  3.041E-05,  2.995E-05,  3.011E-05,  3.004E-05,
     5      2.972E-05,  2.933E-05,  2.887E-05,  2.801E-05,  2.731E-05,
     6      2.735E-05,  2.656E-05,  2.712E-05,  2.576E-05,  2.565E-05,
     7      2.449E-05,  2.450E-05,  2.454E-05,  2.414E-05,  2.488E-05,
     8      2.413E-05,  2.297E-05,  2.298E-05,  2.335E-05,  2.259E-05,
     9      2.142E-05,  2.257E-05,  2.259E-05,  2.220E-05,  2.259E-05,
     $      2.336E-05,  2.299E-05,  2.262E-05,  2.298E-05,  2.298E-05,
     1      2.298E-05,  2.337E-05,  2.414E-05,  2.491E-05,  2.570E-05,
     2      2.492E-05,  2.531E-05,  2.567E-05,  2.647E-05,  2.839E-05,
     3      2.916E-05,  2.917E-05,  2.916E-05,  2.956E-05,  3.033E-05,
     4      3.110E-05,  3.108E-05,  3.071E-05,  3.226E-05,  3.303E-05,
     5      3.302E-05,  3.417E-05,  3.419E-05,  3.458E-05,  3.458E-05,
     6      3.457E-05,  3.613E-05,  3.769E-05,  3.768E-05,  3.845E-05,
     7      3.843E-05,  3.882E-05,  3.920E-05,  3.959E-05,  3.998E-05,
     8      4.037E-05,  4.036E-05,  4.074E-05,  4.074E-05,  4.036E-05,
     9      4.115E-05,  4.192E-05,  4.192E-05,  4.269E-05,  4.270E-05,
     $      4.346E-05,  4.346E-05,  4.346E-05,  4.463E-05,  4.423E-05/
      DATA (Y(I),I=  901, 1000)/
     1      4.502E-05,  4.539E-05,  4.501E-05,  4.465E-05,  4.388E-05,
     2      4.503E-05,  4.578E-05,  4.617E-05,  4.657E-05,  4.657E-05,
     3      4.617E-05,  4.657E-05,  4.655E-05,  4.695E-05,  4.808E-05,
     4      4.807E-05,  4.768E-05,  4.771E-05,  4.807E-05,  4.808E-05,
     5      4.808E-05,  4.731E-05,  4.731E-05,  4.772E-05,  4.808E-05,
     6      4.846E-05,  4.769E-05,  4.807E-05,  4.846E-05,  4.847E-05,
     7      4.845E-05,  4.885E-05,  4.886E-05,  4.848E-05,  4.810E-05,
     8      4.848E-05,  4.769E-05,  4.773E-05,  4.656E-05,  4.690E-05,
     9      4.770E-05,  4.730E-05,  4.690E-05,  4.728E-05,  4.687E-05,
     $      4.728E-05,  4.649E-05,  4.730E-05,  4.690E-05,  4.766E-05,
     1      4.804E-05,  4.727E-05,  4.649E-05,  4.727E-05,  4.647E-05,
     2      4.723E-05,  4.760E-05,  4.800E-05,  4.836E-05,  4.762E-05,
     3      4.760E-05,  4.834E-05,  4.837E-05,  4.796E-05,  4.796E-05,
     4      4.796E-05,  4.876E-05,  4.955E-05,  5.030E-05,  5.067E-05,
     5      5.106E-05,  5.183E-05,  5.104E-05,  5.185E-05,  5.224E-05,
     6      5.340E-05,  5.301E-05,  5.340E-05,  5.456E-05,  5.532E-05,
     7      5.532E-05,  5.531E-05,  5.608E-05,  5.762E-05,  5.799E-05,
     8      5.877E-05,  5.837E-05,  5.839E-05,  5.878E-05,  5.916E-05,
     9      6.032E-05,  6.109E-05,  6.226E-05,  6.342E-05,  6.305E-05,
     $      6.305E-05,  6.265E-05,  6.226E-05,  6.298E-05,  6.452E-05/
      DATA (Y(I),I= 1001, 1100)/
     1      6.531E-05,  6.572E-05,  6.572E-05,  6.614E-05,  6.611E-05,
     2      6.574E-05,  6.690E-05,  6.648E-05,  6.688E-05,  6.688E-05,
     3      6.764E-05,  6.841E-05,  6.843E-05,  6.918E-05,  6.919E-05,
     4      6.959E-05,  6.955E-05,  6.993E-05,  6.990E-05,  7.031E-05,
     5      6.914E-05,  6.835E-05,  6.912E-05,  7.105E-05,  7.222E-05,
     6      7.261E-05,  7.258E-05,  7.258E-05,  7.258E-05,  7.260E-05,
     7      7.179E-05,  7.257E-05,  7.180E-05,  7.142E-05,  7.181E-05,
     8      7.217E-05,  7.256E-05,  7.217E-05,  7.217E-05,  7.219E-05,
     9      7.256E-05,  7.180E-05,  7.178E-05,  7.255E-05,  7.216E-05,
     $      7.175E-05,  7.175E-05,  7.137E-05,  7.177E-05,  7.214E-05,
     1      7.175E-05,  7.134E-05,  7.094E-05,  7.014E-05,  7.014E-05,
     2      7.016E-05,  7.013E-05,  6.937E-05,  6.938E-05,  6.900E-05,
     3      6.863E-05,  6.860E-05,  6.859E-05,  6.741E-05,  6.700E-05,
     4      6.700E-05,  6.740E-05,  6.742E-05,  6.704E-05,  6.665E-05,
     5      6.549E-05,  6.432E-05,  6.427E-05,  6.463E-05,  6.581E-05,
     6      6.662E-05,  6.625E-05,  6.552E-05,  6.473E-05,  6.473E-05,
     7      6.471E-05,  6.469E-05,  6.391E-05,  6.433E-05,  6.475E-05,
     8      6.513E-05,  6.554E-05,  6.552E-05,  6.592E-05,  6.552E-05,
     9      6.553E-05,  6.476E-05,  6.440E-05,  6.483E-05,  6.443E-05,
     $      6.443E-05,  6.522E-05,  6.522E-05,  6.599E-05,  6.599E-05/
      DATA (Y(I),I= 1101, 1200)/
     1      6.560E-05,  6.520E-05,  6.521E-05,  6.558E-05,  6.556E-05,
     2      6.557E-05,  6.596E-05,  6.596E-05,  6.596E-05,  6.634E-05,
     3      6.595E-05,  6.555E-05,  6.555E-05,  6.594E-05,  6.515E-05,
     4      6.553E-05,  6.590E-05,  6.551E-05,  6.474E-05,  6.549E-05,
     5      6.589E-05,  6.628E-05,  6.709E-05,  6.672E-05,  6.634E-05,
     6      6.555E-05,  6.478E-05,  6.475E-05,  6.395E-05,  6.472E-05,
     7      6.472E-05,  6.510E-05,  6.509E-05,  6.508E-05,  6.511E-05,
     8      6.434E-05,  6.357E-05,  6.356E-05,  6.356E-05,  6.278E-05,
     9      6.239E-05,  6.161E-05,  6.199E-05,  6.163E-05,  6.161E-05,
     $      6.161E-05,  6.085E-05,  5.931E-05,  5.888E-05,  5.888E-05,
     1      5.846E-05,  5.805E-05,  5.809E-05,  5.890E-05,  5.892E-05,
     2      5.896E-05,  5.823E-05,  5.746E-05,  5.709E-05,  5.708E-05,
     3      5.707E-05,  5.668E-05,  5.709E-05,  5.666E-05,  5.627E-05,
     4      5.628E-05,  5.552E-05,  5.550E-05,  5.552E-05,  5.588E-05,
     5      5.627E-05,  5.588E-05,  5.627E-05,  5.591E-05,  5.477E-05,
     6      5.515E-05,  5.517E-05,  5.558E-05,  5.599E-05,  5.591E-05,
     7      5.668E-05,  5.507E-05,  5.429E-05,  5.311E-05,  5.279E-05,
     8      5.281E-05,  5.402E-05,  5.406E-05,  5.485E-05,  5.562E-05,
     9      5.562E-05,  5.481E-05,  5.484E-05,  5.483E-05,  5.443E-05,
     $      5.404E-05,  5.365E-05,  5.442E-05,  5.444E-05,  5.521E-05/
      DATA (Y(I),I= 1201, 1300)/
     1      5.481E-05,  5.443E-05,  5.363E-05,  5.286E-05,  5.283E-05,
     2      5.358E-05,  5.356E-05,  5.317E-05,  5.240E-05,  5.279E-05,
     3      5.240E-05,  5.241E-05,  5.321E-05,  5.322E-05,  5.327E-05,
     4      5.329E-05,  5.331E-05,  5.329E-05,  5.290E-05,  5.251E-05,
     5      5.213E-05,  5.135E-05,  5.057E-05,  5.056E-05,  5.056E-05,
     6      5.094E-05,  5.133E-05,  5.133E-05,  5.132E-05,  5.133E-05,
     7      5.054E-05,  4.934E-05,  4.815E-05,  4.733E-05,  4.735E-05,
     8      4.777E-05,  4.819E-05,  4.784E-05,  4.825E-05,  4.790E-05,
     9      4.830E-05,  4.793E-05,  4.873E-05,  4.874E-05,  4.790E-05,
     $      4.709E-05,  4.627E-05,  4.467E-05,  4.427E-05,  4.469E-05,
     1      4.434E-05,  4.431E-05,  4.473E-05,  4.477E-05,  4.365E-05,
     2      4.369E-05,  4.527E-05,  4.528E-05,  4.603E-05,  4.641E-05,
     3      4.520E-05,  4.480E-05,  4.363E-05,  4.404E-05,  4.444E-05,
     4      4.408E-05,  4.448E-05,  4.452E-05,  4.336E-05,  4.184E-05,
     5      4.223E-05,  4.263E-05,  4.301E-05,  4.262E-05,  4.299E-05,
     6      4.143E-05,  4.105E-05,  3.989E-05,  3.995E-05,  3.919E-05,
     7      3.999E-05,  4.118E-05,  4.039E-05,  3.997E-05,  3.880E-05,
     8      3.762E-05,  3.645E-05,  3.493E-05,  3.337E-05,  3.222E-05,
     9      3.296E-05,  3.412E-05,  3.255E-05,  3.257E-05,  3.103E-05,
     $      3.029E-05,  2.876E-05,  2.878E-05,  2.800E-05,  2.883E-05/
      DATA (Y(I),I= 1301, 1400)/
     1      2.881E-05,  2.806E-05,  2.729E-05,  2.650E-05,  2.574E-05,
     2      2.380E-05,  2.262E-05,  2.108E-05,  2.031E-05,  1.842E-05,
     3      1.765E-05,  1.648E-05,  1.646E-05,  1.685E-05,  1.529E-05,
     4      1.451E-05,  1.258E-05,  1.104E-05,  9.506E-06,  9.546E-06,
     5      8.010E-06,  6.431E-06,  4.851E-06,  4.067E-06,  2.472E-06,
     6      8.919E-07, -2.698E-07, -2.356E-07, -6.024E-07, -1.335E-06,
     7     -2.450E-06, -3.996E-06, -5.582E-06, -6.779E-06, -7.956E-06,
     8     -9.542E-06, -1.072E-05, -1.150E-05, -1.227E-05, -1.305E-05,
     9     -1.382E-05, -1.500E-05, -1.580E-05, -1.738E-05, -1.779E-05,
     $     -1.823E-05, -1.900E-05, -2.050E-05, -2.086E-05, -2.159E-05,
     1     -2.191E-05, -2.268E-05, -2.308E-05, -2.276E-05, -2.393E-05,
     2     -2.551E-05, -2.669E-05, -2.709E-05, -2.632E-05, -2.709E-05,
     3     -2.709E-05, -2.749E-05, -3.016E-05, -2.709E-05, -2.709E-05,
     4     -2.709E-05, -2.708E-05, -2.709E-05, -2.709E-05, -2.709E-05,
     5     -2.709E-05, -2.709E-05, -2.709E-05, -2.709E-05, -2.709E-05,
     6     -3.113E-05, -2.709E-05, -2.709E-05, -3.477E-05, -2.709E-05,
     7     -2.685E-05, -3.453E-05, -2.684E-05, -2.685E-05, -3.065E-05,
     8     -2.684E-05, -2.684E-05, -3.065E-05, -2.685E-05, -2.685E-05,
     9     -2.685E-05, -2.684E-05, -2.684E-05, -3.065E-05, -2.685E-05,
     $     -3.453E-05, -2.685E-05, -2.685E-05, -2.684E-05, -2.685E-05/
      DATA (Y(I),I= 1401, 1500)/
     1     -2.684E-05, -2.684E-05, -2.685E-05, -3.086E-05, -3.466E-05,
     2     -3.467E-05, -3.453E-05, -3.854E-05, -3.854E-05, -3.072E-05,
     3     -3.854E-05, -3.840E-05, -3.854E-05, -3.840E-05, -3.452E-05,
     4     -4.221E-05, -3.840E-05, -4.221E-05, -4.221E-05, -3.833E-05,
     5     -4.220E-05, -4.221E-05, -3.833E-05, -4.221E-05, -4.220E-05,
     6     -4.221E-05, -4.221E-05, -3.833E-05, -3.833E-05, -3.833E-05,
     7     -3.834E-05, -4.601E-05, -4.601E-05, -3.833E-05, -3.833E-05,
     8     -4.221E-05, -4.221E-05, -4.601E-05, -4.221E-05, -4.221E-05,
     9     -4.221E-05, -4.626E-05, -4.625E-05, -4.650E-05, -5.054E-05,
     $     -4.650E-05, -5.054E-05, -5.040E-05, -4.650E-05, -5.041E-05,
     1     -5.027E-05, -4.637E-05, -4.636E-05, -5.041E-05, -5.026E-05,
     2     -5.405E-05, -5.404E-05, -5.027E-05, -5.041E-05, -5.041E-05,
     3     -4.637E-05, -5.027E-05, -5.027E-05, -5.432E-05, -5.028E-05,
     4     -5.419E-05, -6.200E-05, -5.418E-05, -6.224E-05, -5.418E-05,
     5     -5.443E-05, -5.442E-05, -6.248E-05, -6.248E-05, -5.480E-05,
     6     -5.480E-05, -6.249E-05, -6.248E-05, -5.443E-05, -5.442E-05,
     7     -5.834E-05, -6.235E-05, -6.222E-05, -6.221E-05, -5.429E-05,
     8     -5.819E-05, -5.452E-05, -5.429E-05, -5.429E-05, -6.220E-05,
     9     -5.453E-05, -5.453E-05, -5.452E-05, -5.453E-05, -5.453E-05,
     $     -6.246E-05, -5.477E-05, -5.845E-05, -5.477E-05, -5.477E-05/
      DATA (Y(I),I= 1501, 1600)/
     1     -5.478E-05, -5.453E-05, -4.671E-05, -4.672E-05, -4.633E-05,
     2     -4.633E-05, -4.647E-05, -4.671E-05, -4.633E-05, -4.647E-05,
     3     -4.269E-05, -4.672E-05, -4.671E-05, -4.671E-05, -4.671E-05,
     4     -4.671E-05, -4.671E-05, -4.695E-05, -4.291E-05, -4.304E-05,
     5     -4.266E-05, -4.280E-05, -3.899E-05, -3.512E-05, -3.875E-05,
     6     -3.488E-05, -3.107E-05, -2.720E-05, -2.720E-05, -2.719E-05,
     7     -2.720E-05, -2.719E-05, -2.720E-05, -2.719E-05, -3.121E-05,
     8     -3.120E-05, -3.121E-05, -2.353E-05, -2.755E-05, -1.961E-05,
     9     -2.730E-05, -2.730E-05, -2.328E-05, -1.156E-05, -1.169E-05,
     $     -1.561E-05, -1.170E-05, -1.938E-05, -1.951E-05, -1.975E-05,
     1     -1.574E-05, -1.975E-05, -1.951E-05, -1.951E-05, -1.183E-05,
     2     -1.183E-05, -7.924E-06, -7.916E-06, -7.921E-06, -7.921E-06,
     3     -7.924E-06, -7.925E-06, -1.194E-05, -7.919E-06, -1.169E-05,
     4     -7.920E-06, -4.014E-06, -1.193E-05, -8.024E-06, -7.778E-06,
     5     -7.778E-06, -7.778E-06, -7.778E-06, -7.540E-06, -1.145E-05,
     6     -1.145E-05, -1.145E-05, -3.770E-06, -3.770E-06, -7.538E-06,
     7     -3.765E-06, -1.145E-05, -7.543E-06, -3.772E-06, -3.772E-06,
     8     -3.768E-06, -3.769E-06, -3.769E-06, -3.769E-06, -3.767E-06,
     9     -3.766E-06, -3.769E-06, -3.772E-06, -1.159E-05, -1.145E-05,
     $     -7.817E-06, -7.815E-06, -7.816E-06, -1.563E-05, -7.577E-06/
      DATA (Y(I),I= 1601, 1700)/
     1     -7.570E-06, -1.525E-05, -7.328E-06, -7.572E-06, -7.574E-06,
     2     -1.525E-05, -1.120E-05, -1.145E-05, -1.145E-05, -1.145E-05,
     3     -1.145E-05, -1.913E-05, -1.145E-05, -1.145E-05, -1.145E-05,
     4     -1.145E-05, -1.145E-05, -1.145E-05, -1.145E-05, -2.304E-05,
     5     -2.304E-05, -2.304E-05, -2.304E-05, -1.536E-05, -1.536E-05,
     6     -1.536E-05, -2.304E-05, -2.304E-05, -1.924E-05, -1.923E-05,
     7     -2.692E-05, -2.692E-05, -1.924E-05, -2.691E-05, -1.923E-05,
     8     -2.716E-05, -2.328E-05, -2.716E-05, -3.096E-05, -3.096E-05,
     9     -3.096E-05, -3.120E-05, -3.120E-05, -3.107E-05, -2.301E-05,
     $     -3.107E-05, -2.315E-05, -2.315E-05, -2.315E-05, -2.301E-05,
     1     -2.301E-05, -3.069E-05, -2.301E-05, -3.069E-05, -2.301E-05,
     2     -3.093E-05, -3.055E-05, -3.056E-05, -2.287E-05, -2.301E-05,
     3     -3.069E-05, -3.093E-05, -3.093E-05, -3.069E-05, -2.301E-05,
     4     -2.301E-05, -2.301E-05, -2.301E-05, -3.107E-05, -3.106E-05,
     5     -2.301E-05, -2.314E-05, -3.083E-05, -3.083E-05, -3.107E-05,
     6     -2.706E-05, -3.083E-05, -2.681E-05, -2.301E-05, -2.682E-05,
     7     -2.706E-05, -1.913E-05, -2.682E-05, -1.914E-05, -2.277E-05,
     8     -1.913E-05, -1.522E-05, -1.509E-05, -1.522E-05, -2.290E-05,
     9     -1.509E-05, -2.290E-05, -2.290E-05, -1.522E-05, -1.522E-05,
     $     -1.910E-05, -1.522E-05, -1.522E-05, -1.155E-05, -1.155E-05/
      DATA (Y(I),I= 1701, 1800)/
     1     -1.156E-05, -1.142E-05, -7.541E-06, -7.536E-06, -7.544E-06,
     2     -1.142E-05, -1.522E-05, -7.542E-06, -7.538E-06, -7.539E-06,
     3     -7.541E-06, -7.541E-06, -7.545E-06, -7.538E-06, -3.636E-06,
     4      1.426E-07, -3.494E-06, -3.633E-06, -3.630E-06, -3.631E-06,
     5     -3.634E-06,  4.149E-07, -3.629E-06, -3.629E-06, -3.633E-06,
     6     -3.632E-06, -3.634E-06, -3.628E-06, -3.385E-06,  4.292E-06,
     7      2.437E-07,  2.431E-07,  4.155E-06, -3.766E-06, -3.771E-06,
     8      2.430E-07,  2.438E-07,  3.915E-06,  3.909E-06,  7.824E-06,
     9      3.913E-06,  3.911E-06,  3.913E-06,  3.909E-06,  3.910E-06,
     $      3.910E-06,  3.914E-06,  4.150E-06,  1.184E-05,  1.184E-05,
     1      1.183E-05,  1.108E-05,  1.031E-05,  9.561E-06,  9.950E-06,
     2      1.227E-05,  1.420E-05,  1.534E-05,  1.572E-05,  1.571E-05,
     3      1.492E-05,  1.490E-05,  1.445E-05,  1.442E-05,  1.525E-05,
     4      1.487E-05,  1.452E-05,  1.604E-05,  1.840E-05,  2.070E-05,
     5      2.146E-05,  2.144E-05,  2.105E-05,  2.104E-05,  2.101E-05,
     6      2.100E-05,  2.175E-05,  2.095E-05,  2.097E-05,  1.981E-05,
     7      1.982E-05,  2.058E-05,  2.095E-05,  2.098E-05,  1.979E-05,
     8      1.860E-05,  1.782E-05,  1.934E-05,  1.972E-05,  2.083E-05,
     9      2.201E-05,  2.281E-05,  2.476E-05,  2.480E-05,  2.480E-05,
     $      2.407E-05,  2.371E-05,  2.295E-05,  2.218E-05,  2.141E-05/
      DATA (Y(I),I= 1801, 1900)/
     1      2.218E-05,  2.294E-05,  2.371E-05,  2.369E-05,  2.328E-05,
     2      2.287E-05,  2.169E-05,  2.128E-05,  2.126E-05,  2.004E-05,
     3      2.004E-05,  2.085E-05,  2.045E-05,  2.045E-05,  2.006E-05,
     4      2.085E-05,  2.082E-05,  2.121E-05,  2.198E-05,  2.275E-05,
     5      2.238E-05,  2.083E-05,  2.046E-05,  1.928E-05,  1.893E-05,
     6      1.815E-05,  1.815E-05,  1.812E-05,  1.929E-05,  2.046E-05,
     7      2.087E-05,  2.244E-05,  2.283E-05,  2.322E-05,  2.327E-05,
     8      2.291E-05,  2.329E-05,  2.482E-05,  2.558E-05,  2.593E-05,
     9      2.514E-05,  2.436E-05,  2.279E-05,  2.282E-05,  2.363E-05,
     $      2.441E-05,  2.524E-05,  2.599E-05,  2.759E-05,  2.874E-05,
     1      2.954E-05,  3.028E-05,  3.064E-05,  3.063E-05,  3.021E-05,
     2      2.866E-05,  2.788E-05,  2.788E-05,  2.749E-05,  2.711E-05,
     3      2.866E-05,  3.016E-05,  3.250E-05,  3.323E-05,  3.366E-05,
     4      3.328E-05,  3.367E-05,  3.366E-05,  3.483E-05,  3.521E-05,
     5      3.599E-05,  3.603E-05,  3.606E-05,  3.685E-05,  3.764E-05,
     6      3.960E-05,  4.076E-05,  4.079E-05,  4.041E-05,  3.926E-05,
     7      3.848E-05,  4.004E-05,  4.161E-05,  4.350E-05,  4.426E-05,
     8      4.386E-05,  4.305E-05,  4.344E-05,  4.457E-05,  4.570E-05,
     9      4.685E-05,  4.567E-05,  4.450E-05,  4.337E-05,  4.297E-05,
     $      4.414E-05,  4.527E-05,  4.644E-05,  4.721E-05,  4.798E-05/
      DATA (Y(I),I= 1901, 2000)/
     1      4.873E-05,  4.952E-05,  4.797E-05,  4.798E-05,  4.681E-05,
     2      4.721E-05,  4.760E-05,  4.724E-05,  4.802E-05,  4.805E-05,
     3      4.766E-05,  4.690E-05,  4.727E-05,  4.800E-05,  4.911E-05,
     4      4.947E-05,  4.636E-05,  4.401E-05,  4.164E-05,  4.089E-05,
     5      4.167E-05,  4.245E-05,  4.248E-05,  4.136E-05,  3.944E-05,
     6      3.832E-05,  3.753E-05,  3.867E-05,  3.982E-05,  4.022E-05,
     7      3.829E-05,  3.714E-05,  3.481E-05,  3.365E-05,  3.438E-05,
     8      3.478E-05,  3.474E-05,  3.395E-05,  3.277E-05,  3.085E-05,
     9      2.966E-05,  3.045E-05,  3.044E-05,  3.123E-05,  3.042E-05,
     $      2.965E-05,  2.969E-05,  2.966E-05,  3.082E-05,  3.273E-05,
     1      3.388E-05,  3.310E-05,  3.195E-05,  3.196E-05,  3.080E-05,
     2      3.004E-05,  3.004E-05,  2.927E-05,  2.927E-05,  3.044E-05,
     3      3.081E-05,  3.198E-05,  3.197E-05,  3.237E-05,  3.200E-05,
     4      3.236E-05,  3.277E-05,  3.239E-05,  3.238E-05,  3.276E-05,
     5      3.199E-05,  3.161E-05,  3.200E-05,  3.199E-05,  3.236E-05,
     6      3.352E-05,  3.351E-05,  3.429E-05,  3.388E-05,  3.350E-05,
     7      3.426E-05,  3.541E-05,  3.618E-05,  3.697E-05,  3.697E-05,
     8      3.696E-05,  3.737E-05,  3.737E-05,  3.736E-05,  3.854E-05,
     9      3.893E-05,  3.738E-05,  3.624E-05,  3.395E-05,  3.318E-05,
     $      3.550E-05,  3.819E-05,  4.092E-05,  4.204E-05,  4.245E-05/
      DATA (Y(I),I= 2001, 2100)/
     1      4.284E-05,  4.325E-05,  4.324E-05,  4.363E-05,  4.403E-05,
     2      4.442E-05,  4.365E-05,  4.284E-05,  4.246E-05,  4.244E-05,
     3      4.360E-05,  4.552E-05,  4.629E-05,  4.548E-05,  4.431E-05,
     4      4.316E-05,  4.238E-05,  4.353E-05,  4.428E-05,  4.504E-05,
     5      4.581E-05,  4.621E-05,  4.543E-05,  4.583E-05,  4.620E-05,
     6      4.659E-05,  4.622E-05,  4.661E-05,  4.621E-05,  4.583E-05,
     7      4.583E-05,  4.503E-05,  4.582E-05,  4.541E-05,  4.578E-05,
     8      4.577E-05,  4.655E-05,  4.691E-05,  4.769E-05,  4.693E-05,
     9      4.693E-05,  4.539E-05,  4.462E-05,  4.462E-05,  4.425E-05,
     $      4.351E-05,  4.391E-05,  4.468E-05,  4.588E-05,  4.741E-05,
     1      4.818E-05,  5.011E-05,  5.046E-05,  5.198E-05,  5.124E-05,
     2      5.086E-05,  4.931E-05,  4.776E-05,  4.700E-05,  4.623E-05,
     3      4.469E-05,  4.472E-05,  4.625E-05,  4.856E-05,  5.087E-05,
     4      5.239E-05,  5.318E-05,  5.474E-05,  5.547E-05,  5.626E-05,
     5      5.706E-05,  5.668E-05,  5.704E-05,  5.629E-05,  5.361E-05,
     6      5.208E-05,  5.017E-05,  5.209E-05,  5.554E-05,  5.864E-05,
     7      6.132E-05,  6.251E-05,  6.253E-05,  6.293E-05,  6.371E-05,
     8      6.489E-05,  6.606E-05,  6.608E-05,  6.644E-05,  6.453E-05,
     9      6.336E-05,  6.178E-05,  6.177E-05,  6.327E-05,  6.557E-05,
     $      6.711E-05,  6.864E-05,  6.901E-05,  6.937E-05,  6.975E-05/
      DATA (Y(I),I= 2101, 2200)/
     1      6.821E-05,  6.704E-05,  6.554E-05,  6.438E-05,  6.398E-05,
     2      6.318E-05,  6.319E-05,  6.280E-05,  6.048E-05,  5.933E-05,
     3      5.817E-05,  5.660E-05,  5.620E-05,  5.619E-05,  5.616E-05,
     4      5.501E-05,  5.226E-05,  4.917E-05,  4.647E-05,  4.528E-05,
     5      4.565E-05,  4.602E-05,  4.641E-05,  4.524E-05,  4.213E-05,
     6      3.943E-05,  3.713E-05,  3.711E-05,  3.866E-05,  3.982E-05,
     7      4.137E-05,  3.944E-05,  3.674E-05,  3.368E-05,  3.139E-05,
     8      3.100E-05,  3.139E-05,  3.212E-05,  3.251E-05,  3.135E-05,
     9      2.904E-05,  2.633E-05,  2.441E-05,  2.478E-05,  2.480E-05,
     $      2.557E-05,  2.594E-05,  2.477E-05,  2.404E-05,  2.404E-05,
     1      2.325E-05,  2.365E-05,  2.286E-05,  2.365E-05,  2.442E-05,
     2      2.404E-05,  2.406E-05,  2.401E-05,  2.403E-05,  2.480E-05,
     3      2.480E-05,  2.598E-05,  2.675E-05,  2.675E-05,  2.793E-05,
     4      2.793E-05,  2.793E-05,  2.796E-05,  2.758E-05,  2.761E-05,
     5      2.721E-05,  2.875E-05,  2.991E-05,  3.147E-05,  3.301E-05,
     6      3.266E-05,  3.187E-05,  3.148E-05,  3.110E-05,  3.261E-05,
     7      3.417E-05,  3.532E-05,  3.686E-05,  3.684E-05,  3.606E-05,
     8      3.568E-05,  3.493E-05,  3.645E-05,  3.797E-05,  4.030E-05,
     9      4.144E-05,  4.221E-05,  4.181E-05,  4.184E-05,  4.142E-05,
     $      4.181E-05,  4.257E-05,  4.372E-05,  4.412E-05,  4.334E-05/
      DATA (Y(I),I= 2201, 2300)/
     1      4.220E-05,  4.146E-05,  3.991E-05,  4.067E-05,  4.223E-05,
     2      4.377E-05,  4.608E-05,  4.609E-05,  4.608E-05,  4.571E-05,
     3      4.569E-05,  4.532E-05,  4.608E-05,  4.648E-05,  4.684E-05,
     4      4.764E-05,  4.725E-05,  4.803E-05,  4.805E-05,  4.726E-05,
     5      4.806E-05,  4.806E-05,  4.810E-05,  4.770E-05,  4.731E-05,
     6      4.617E-05,  4.577E-05,  4.424E-05,  4.464E-05,  4.502E-05,
     7      4.577E-05,  4.538E-05,  4.619E-05,  4.696E-05,  4.696E-05,
     8      4.696E-05,  4.655E-05,  4.657E-05,  4.540E-05,  4.537E-05,
     9      4.618E-05,  4.770E-05,  4.847E-05,  5.041E-05,  5.041E-05,
     $      4.928E-05,  4.773E-05,  4.619E-05,  4.502E-05,  4.582E-05,
     1      4.658E-05,  4.581E-05,  4.658E-05,  4.734E-05,  4.813E-05,
     2      4.809E-05,  4.927E-05,  4.848E-05,  4.814E-05,  4.813E-05,
     3      4.736E-05,  4.931E-05,  4.968E-05,  5.124E-05,  5.317E-05,
     4      5.242E-05,  5.087E-05,  5.013E-05,  4.899E-05,  4.819E-05,
     5      5.129E-05,  5.321E-05,  5.514E-05,  5.667E-05,  5.666E-05,
     6      5.669E-05,  5.745E-05,  5.708E-05,  5.744E-05,  5.746E-05,
     7      5.823E-05,  5.862E-05,  5.745E-05,  5.706E-05,  5.593E-05,
     8      5.513E-05,  5.512E-05,  5.512E-05,  5.586E-05,  5.663E-05,
     9      5.701E-05,  5.662E-05,  5.619E-05,  5.657E-05,  5.618E-05,
     $      5.502E-05,  5.385E-05,  5.307E-05,  5.191E-05,  5.113E-05/
      DATA (Y(I),I= 2301, 2400)/
     1      5.074E-05,  5.033E-05,  5.036E-05,  4.994E-05,  4.801E-05,
     2      4.723E-05,  4.609E-05,  4.531E-05,  4.724E-05,  4.992E-05,
     3      5.185E-05,  5.298E-05,  5.028E-05,  4.607E-05,  4.225E-05,
     4      3.800E-05,  3.608E-05,  3.610E-05,  3.454E-05,  3.455E-05,
     5      3.339E-05,  3.302E-05,  3.263E-05,  3.264E-05,  3.186E-05,
     6      3.262E-05,  3.417E-05,  3.493E-05,  3.647E-05,  3.530E-05,
     7      3.375E-05,  3.221E-05,  3.068E-05,  3.031E-05,  2.992E-05,
     8      3.029E-05,  3.029E-05,  3.108E-05,  3.108E-05,  3.031E-05,
     9      3.031E-05,  3.031E-05,  3.031E-05,  3.031E-05,  3.070E-05,
     $      2.994E-05,  3.111E-05,  3.109E-05,  3.184E-05,  3.300E-05,
     1      3.377E-05,  3.379E-05,  3.378E-05,  3.378E-05,  3.380E-05,
     2      3.456E-05,  3.535E-05,  3.572E-05,  3.651E-05,  3.650E-05,
     3      3.766E-05,  3.766E-05,  3.843E-05,  3.959E-05,  3.961E-05,
     4      4.037E-05,  4.039E-05,  4.038E-05,  4.077E-05,  3.923E-05,
     5      3.806E-05,  3.654E-05,  3.537E-05,  3.690E-05,  3.804E-05,
     6      3.956E-05,  4.189E-05,  4.264E-05,  4.228E-05,  4.150E-05,
     7      4.113E-05,  4.152E-05,  4.189E-05,  4.189E-05,  4.265E-05,
     8      4.304E-05,  4.227E-05,  4.189E-05,  4.035E-05,  3.921E-05,
     9      3.844E-05,  3.920E-05,  3.996E-05,  4.072E-05,  4.265E-05,
     $      4.265E-05,  4.265E-05,  4.189E-05,  4.148E-05,  4.151E-05/
      DATA (Y(I),I= 2401, 2500)/
     1      4.151E-05,  4.188E-05,  4.188E-05,  4.188E-05,  4.306E-05,
     2      4.226E-05,  4.304E-05,  4.385E-05,  4.357E-05,  4.342E-05,
     3      4.250E-05,  4.239E-05,  4.224E-05,  4.160E-05,  4.217E-05,
     4      4.281E-05,  4.298E-05,  4.359E-05,  4.302E-05,  4.287E-05,
     5      4.195E-05,  4.139E-05,  4.195E-05,  4.233E-05,  4.270E-05,
     6      4.229E-05,  4.268E-05,  4.234E-05,  4.191E-05,  4.138E-05,
     7      4.084E-05,  4.065E-05,  4.062E-05,  4.052E-05,  4.045E-05,
     8      4.053E-05,  4.100E-05,  4.151E-05,  4.206E-05,  4.260E-05,
     9      4.292E-05,  4.304E-05,  4.320E-05,  4.333E-05,  4.345E-05,
     $      4.377E-05,  4.404E-05,  4.432E-05,  4.460E-05,  4.453E-05,
     1      4.380E-05,  4.316E-05,  4.239E-05,  4.170E-05,  4.214E-05,
     2      4.302E-05,  4.387E-05,  4.472E-05,  4.523E-05,  4.454E-05,
     3      4.369E-05,  4.300E-05,  4.215E-05,  4.239E-05,  4.346E-05,
     4      4.450E-05,  4.554E-05,  4.650E-05,  4.638E-05,  4.595E-05,
     5      4.568E-05,  4.529E-05,  4.501E-05,  4.520E-05,  4.527E-05,
     6      4.534E-05,  4.541E-05,  4.559E-05,  4.584E-05,  4.613E-05,
     7      4.626E-05,  4.628E-05,  4.530E-05,  4.413E-05,  4.292E-05,
     8      4.171E-05,  4.101E-05,  4.133E-05,  4.153E-05,  4.180E-05,
     9      4.208E-05,  4.158E-05,  4.078E-05,  4.003E-05,  3.923E-05,
     $      3.854E-05,  3.888E-05,  3.936E-05,  3.993E-05,  4.038E-05/
      DATA (Y(I),I= 2501, 2600)/
     1      4.018E-05,  3.852E-05,  3.686E-05,  3.515E-05,  3.349E-05,
     2      3.280E-05,  3.284E-05,  3.285E-05,  3.277E-05,  3.273E-05,
     3      3.205E-05,  3.128E-05,  3.044E-05,  2.968E-05,  2.902E-05,
     4      2.895E-05,  2.891E-05,  2.895E-05,  2.895E-05,  2.876E-05,
     5      2.837E-05,  2.803E-05,  2.760E-05,  2.726E-05,  2.707E-05,
     6      2.707E-05,  2.700E-05,  2.700E-05,  2.689E-05,  2.685E-05,
     7      2.681E-05,  2.674E-05,  2.667E-05,  2.651E-05,  2.628E-05,
     8      2.602E-05,  2.575E-05,  2.552E-05,  2.544E-05,  2.567E-05,
     9      2.591E-05,  2.614E-05,  2.633E-05,  2.610E-05,  2.557E-05,
     $      2.511E-05,  2.457E-05,  2.416E-05,  2.450E-05,  2.512E-05,
     1      2.578E-05,  2.635E-05,  2.678E-05,  2.655E-05,  2.628E-05,
     2      2.594E-05,  2.563E-05,  2.548E-05,  2.606E-05,  2.667E-05,
     3      2.725E-05,  2.779E-05,  2.802E-05,  2.768E-05,  2.737E-05,
     4      2.699E-05,  2.661E-05,  2.681E-05,  2.746E-05,  2.816E-05,
     5      2.877E-05,  2.939E-05,  2.951E-05,  2.935E-05,  2.924E-05,
     6      2.913E-05,  2.897E-05,  2.916E-05,  2.936E-05,  2.951E-05,
     7      2.978E-05,  2.990E-05,  2.997E-05,  3.001E-05,  3.004E-05,
     8      3.004E-05,  3.000E-05,  2.981E-05,  2.957E-05,  2.938E-05,
     9      2.918E-05,  2.903E-05,  2.918E-05,  2.930E-05,  2.937E-05,
     $      2.948E-05,  2.944E-05,  2.898E-05,  2.848E-05,  2.806E-05/
      DATA (Y(I),I= 2601, 2700)/
     1      2.756E-05,  2.733E-05,  2.744E-05,  2.751E-05,  2.763E-05,
     2      2.767E-05,  2.751E-05,  2.716E-05,  2.682E-05,  2.635E-05,
     3      2.605E-05,  2.585E-05,  2.581E-05,  2.578E-05,  2.570E-05,
     4      2.574E-05,  2.566E-05,  2.554E-05,  2.551E-05,  2.543E-05,
     5      2.535E-05,  2.535E-05,  2.544E-05,  2.548E-05,  2.560E-05,
     6      2.564E-05,  2.569E-05,  2.585E-05,  2.589E-05,  2.597E-05,
     7      2.601E-05,  2.617E-05,  2.641E-05,  2.656E-05,  2.671E-05,
     8      2.695E-05,  2.710E-05,  2.730E-05,  2.746E-05,  2.769E-05,
     9      2.785E-05,  2.800E-05,  2.812E-05,  2.831E-05,  2.842E-05,
     $      2.858E-05,  2.878E-05,  2.889E-05,  2.908E-05,  2.920E-05,
     1      2.939E-05,  2.935E-05,  2.935E-05,  2.931E-05,  2.923E-05,
     2      2.927E-05,  2.919E-05,  2.915E-05,  2.915E-05,  2.911E-05,
     3      2.903E-05,  2.895E-05,  2.864E-05,  2.837E-05,  2.814E-05,
     4      2.790E-05,  2.763E-05,  2.740E-05,  2.716E-05,  2.693E-05,
     5      2.666E-05,  2.639E-05,  2.612E-05,  2.584E-05,  2.550E-05,
     6      2.526E-05,  2.499E-05,  2.464E-05,  2.437E-05,  2.414E-05,
     7      2.391E-05,  2.360E-05,  2.336E-05,  2.313E-05,  2.293E-05,
     8      2.266E-05,  2.247E-05,  2.227E-05,  2.196E-05,  2.177E-05,
     9      2.157E-05,  2.130E-05,  2.118E-05,  2.107E-05,  2.095E-05,
     $      2.075E-05,  2.063E-05,  2.044E-05,  2.036E-05,  2.017E-05/
      DATA (Y(I),I= 2701, 2800)/
     1      2.005E-05,  1.994E-05,  1.982E-05,  1.971E-05,  1.959E-05,
     2      1.944E-05,  1.933E-05,  1.925E-05,  1.906E-05,  1.898E-05,
     3      1.887E-05,  1.876E-05,  1.864E-05,  1.856E-05,  1.849E-05,
     4      1.830E-05,  1.827E-05,  1.815E-05,  1.807E-05,  1.800E-05,
     5      1.788E-05,  1.778E-05,  1.770E-05,  1.766E-05,  1.762E-05,
     6      1.755E-05,  1.755E-05,  1.751E-05,  1.740E-05,  1.736E-05,
     7      1.736E-05,  1.728E-05,  1.725E-05,  1.721E-05,  1.717E-05,
     8      1.712E-05,  1.712E-05,  1.709E-05,  1.704E-05,  1.693E-05,
     9      1.693E-05,  1.685E-05,  1.685E-05,  1.680E-05,  1.680E-05,
     $      1.672E-05,  1.672E-05,  1.672E-05,  1.672E-05,  1.668E-05,
     1      1.660E-05,  1.659E-05,  1.659E-05,  1.651E-05,  1.655E-05,
     2      1.655E-05,  1.655E-05,  1.651E-05,  1.647E-05,  1.647E-05,
     3      1.647E-05,  1.643E-05,  1.643E-05,  1.636E-05,  1.636E-05,
     4      1.640E-05,  1.640E-05,  1.636E-05,  1.640E-05,  1.640E-05,
     5      1.640E-05,  1.637E-05,  1.637E-05,  1.641E-05,  1.637E-05,
     6      1.641E-05,  1.638E-05,  1.626E-05,  1.622E-05,  1.619E-05,
     7      1.611E-05,  1.611E-05,  1.607E-05,  1.604E-05,  1.600E-05,
     8      1.596E-05,  1.589E-05,  1.573E-05,  1.562E-05,  1.550E-05,
     9      1.538E-05,  1.531E-05,  1.519E-05,  1.499E-05,  1.496E-05,
     $      1.480E-05,  1.469E-05,  1.461E-05,  1.445E-05,  1.437E-05/
      DATA (Y(I),I= 2801, 2900)/
     1      1.422E-05,  1.418E-05,  1.403E-05,  1.399E-05,  1.383E-05,
     2      1.375E-05,  1.360E-05,  1.356E-05,  1.356E-05,  1.360E-05,
     3      1.360E-05,  1.364E-05,  1.360E-05,  1.360E-05,  1.360E-05,
     4      1.360E-05,  1.356E-05,  1.364E-05,  1.364E-05,  1.368E-05,
     5      1.380E-05,  1.384E-05,  1.388E-05,  1.392E-05,  1.403E-05,
     6      1.403E-05,  1.411E-05,  1.411E-05,  1.419E-05,  1.423E-05,
     7      1.431E-05,  1.427E-05,  1.435E-05,  1.439E-05,  1.439E-05,
     8      1.443E-05,  1.451E-05,  1.454E-05,  1.454E-05,  1.458E-05,
     9      1.458E-05,  1.450E-05,  1.450E-05,  1.446E-05,  1.446E-05,
     $      1.446E-05,  1.438E-05,  1.442E-05,  1.441E-05,  1.433E-05,
     1      1.429E-05,  1.422E-05,  1.418E-05,  1.406E-05,  1.398E-05,
     2      1.387E-05,  1.379E-05,  1.367E-05,  1.360E-05,  1.352E-05,
     3      1.340E-05,  1.329E-05,  1.313E-05,  1.310E-05,  1.290E-05,
     4      1.275E-05,  1.263E-05,  1.248E-05,  1.233E-05,  1.225E-05,
     5      1.210E-05,  1.198E-05,  1.187E-05,  1.171E-05,  1.156E-05,
     6      1.144E-05,  1.133E-05,  1.118E-05,  1.106E-05,  1.098E-05,
     7      1.081E-05,  1.073E-05,  1.055E-05,  1.046E-05,  1.035E-05,
     8      1.025E-05,  1.013E-05,  1.003E-05,  9.912E-06,  9.800E-06,
     9      9.691E-06,  9.582E-06,  9.470E-06,  9.362E-06,  9.253E-06,
     $      9.175E-06,  9.097E-06,  9.027E-06,  8.950E-06,  8.883E-06/
      DATA (Y(I),I= 2901, 3000)/
     1      8.802E-06,  8.732E-06,  8.658E-06,  8.588E-06,  8.510E-06,
     2      8.440E-06,  8.385E-06,  8.342E-06,  8.307E-06,  8.267E-06,
     3      8.228E-06,  8.189E-06,  8.149E-06,  8.106E-06,  8.063E-06,
     4      8.024E-06,  7.984E-06,  7.957E-06,  7.949E-06,  7.944E-06,
     5      7.928E-06,  7.920E-06,  7.908E-06,  7.907E-06,  7.895E-06,
     6      7.883E-06,  7.874E-06,  7.870E-06,  7.854E-06,  7.858E-06,
     7      7.853E-06,  7.850E-06,  7.853E-06,  7.849E-06,  7.853E-06,
     8      7.849E-06,  7.845E-06,  7.852E-06,  7.848E-06,  7.844E-06,
     9      7.836E-06,  7.825E-06,  7.810E-06,  7.798E-06,  7.787E-06,
     $      7.776E-06,  7.753E-06,  7.741E-06,  7.726E-06,  7.715E-06,
     1      7.703E-06,  7.684E-06,  7.642E-06,  7.604E-06,  7.565E-06,
     2      7.527E-06,  7.492E-06,  7.454E-06,  7.416E-06,  7.370E-06,
     3      7.332E-06,  7.297E-06,  7.251E-06,  7.197E-06,  7.132E-06,
     4      7.070E-06,  7.005E-06,  6.935E-06,  6.870E-06,  6.805E-06,
     5      6.743E-06,  6.674E-06,  6.608E-06,  6.543E-06,  6.481E-06,
     6      6.393E-06,  6.312E-06,  6.223E-06,  6.143E-06,  6.058E-06,
     7      5.977E-06,  5.897E-06,  5.816E-06,  5.723E-06,  5.643E-06,
     8      5.558E-06,  5.493E-06,  5.420E-06,  5.358E-06,  5.285E-06,
     9      5.212E-06,  5.150E-06,  5.073E-06,  5.004E-06,  4.939E-06,
     $      4.866E-06,  4.804E-06,  4.743E-06,  4.731E-06,  4.727E-06/
      DATA (Y(I),I= 3001, 3080)/
     1      4.723E-06,  4.719E-06,  4.715E-06,  4.707E-06,  4.699E-06,
     2      4.699E-06,  4.695E-06,  4.691E-06,  4.680E-06,  4.695E-06,
     3      4.745E-06,  4.798E-06,  4.844E-06,  4.890E-06,  4.944E-06,
     4      4.986E-06,  5.040E-06,  5.086E-06,  5.136E-06,  5.189E-06,
     5      5.236E-06,  5.262E-06,  5.270E-06,  5.289E-06,  5.297E-06,
     6      5.305E-06,  5.324E-06,  5.332E-06,  5.347E-06,  5.355E-06,
     7      5.370E-06,  5.381E-06,  5.389E-06,  5.347E-06,  5.297E-06,
     8      5.231E-06,  5.181E-06,  5.127E-06,  5.065E-06,  5.011E-06,
     9      4.950E-06,  4.896E-06,  4.838E-06,  4.780E-06,  4.726E-06,
     $      4.661E-06,  4.595E-06,  4.525E-06,  4.456E-06,  4.390E-06,
     1      4.328E-06,  4.255E-06,  4.193E-06,  4.127E-06,  4.058E-06,
     2      3.988E-06,  3.926E-06,  3.849E-06,  3.760E-06,  3.679E-06,
     3      3.598E-06,  3.520E-06,  3.439E-06,  3.357E-06,  3.272E-06,
     4      3.191E-06,  3.118E-06,  3.033E-06,  2.944E-06,  2.824E-06,
     5      2.693E-06,  2.561E-06,  2.434E-06,  2.306E-06,  2.179E-06,
     6      2.043E-06,  1.917E-06,  1.790E-06,  1.663E-06,  1.533E-06/
C
      DATA (Z(I),I=    1,  100)/
     1      6.369E-15,   .000E+00,   .000E+00,  1.274E-14, -1.274E-14,
     2      6.369E-15, -6.369E-15,  1.274E-14,   .000E+00,  1.274E-14,
     3       .000E+00,  6.369E-15,  1.274E-14,  1.274E-14,  1.274E-14,
     4      1.911E-14,  1.274E-14,  1.911E-14,  2.548E-14, -1.274E-14,
     5      2.548E-14,  1.274E-14,  1.274E-14, -1.274E-14,   .000E+00,
     6      2.548E-14,  1.274E-14,  1.274E-14,  5.095E-14, -1.274E-14,
     7     -1.274E-14,  5.095E-14, -1.274E-14,  2.548E-14, -2.548E-14,
     8       .000E+00,  5.095E-14,  2.548E-14,   .000E+00,  2.548E-14,
     9      2.548E-14,  2.548E-14, -2.548E-14,  2.548E-14, -2.548E-14,
     $      2.548E-14,  2.548E-14,  2.548E-14,   .000E+00,  5.095E-14,
     1      2.548E-14,  2.548E-14,   .000E+00,  7.643E-14,  5.095E-14,
     2      2.548E-14,   .000E+00, -2.548E-14,  2.548E-14,   .000E+00,
     3      7.643E-14,   .000E+00,  7.643E-14, -7.643E-14,  1.019E-13,
     4      1.019E-13,  1.019E-13,   .000E+00,  1.019E-13,  5.095E-14,
     5      1.019E-13,  1.019E-13,  5.095E-14,  1.529E-13,  1.529E-13,
     6      1.529E-13, -5.095E-14, -1.019E-13, -1.019E-13,   .000E+00,
     7      1.529E-13,  1.019E-13,  5.095E-14, -5.095E-14,   .000E+00,
     8      2.548E-14,  2.548E-14,  2.548E-14,  2.548E-14,  7.643E-14,
     9       .000E+00,   .000E+00,  2.548E-14,  5.095E-14,   .000E+00,
     $      5.095E-14, -2.548E-14,  7.643E-14,  2.548E-14,   .000E+00/
      DATA (Z(I),I=  101,  200)/
     1      2.548E-14,  5.095E-14,  2.548E-14,  7.643E-14,  3.821E-14,
     2      3.821E-14,  3.821E-14,  6.369E-14,   .000E+00, -3.821E-14,
     3      1.274E-14,  1.274E-14,  2.548E-14, -3.821E-14,  1.274E-14,
     4      3.821E-14,  6.369E-14,  3.821E-14,   .000E+00,  1.274E-14,
     5      2.548E-14, -1.274E-14,  5.095E-14,  2.548E-14,  5.095E-14,
     6      5.095E-14,  5.095E-14,  7.643E-14,   .000E+00,  7.643E-14,
     7      2.548E-14,  7.643E-14,   .000E+00,   .000E+00,  7.643E-14,
     8       .000E+00,  5.095E-14,  2.548E-14,  5.095E-14,  7.643E-14,
     9     -2.548E-14,   .000E+00,  2.548E-14,  2.548E-14,   .000E+00,
     $       .000E+00,  1.529E-13,  1.019E-13,   .000E+00,   .000E+00,
     1      1.019E-13,  5.095E-14,  5.095E-14,  1.529E-13,  1.529E-13,
     2       .000E+00,  1.019E-13,  1.529E-13,  1.529E-13,  1.019E-13,
     3      5.095E-14,  1.019E-13,   .000E+00, -1.019E-13,  1.019E-13,
     4      1.019E-13,  1.019E-13,  1.019E-13,  4.076E-13, -1.019E-13,
     5      2.038E-13,  2.038E-13,  2.038E-13,  2.038E-13,   .000E+00,
     6     -2.038E-13,  2.038E-13,  8.152E-13,  1.427E-12,  8.152E-13,
     7       .000E+00, -4.076E-13,  4.076E-13,   .000E+00,  1.630E-12,
     8      4.076E-13,  2.038E-13,  6.114E-13,  4.076E-13,  2.038E-13,
     9      3.057E-13,  2.038E-13,   .000E+00,  1.019E-13,  3.057E-13,
     $      2.038E-13,  2.038E-13,  5.095E-14,  1.019E-13,  1.019E-13/
      DATA (Z(I),I=  201,  300)/
     1       .000E+00,  1.529E-13,  5.095E-14,  2.038E-13,  1.529E-13,
     2      5.095E-14,  5.095E-14,  5.095E-14,  1.529E-13,  1.529E-13,
     3      1.529E-13,  5.095E-14,  5.095E-14,  5.095E-14,   .000E+00,
     4      5.095E-14,  1.019E-13,  2.038E-13,  5.095E-14,  1.019E-13,
     5       .000E+00,  1.529E-13,  5.095E-14,   .000E+00,   .000E+00,
     6      5.095E-14,  1.529E-13,  1.019E-13,  1.019E-13,  2.038E-13,
     7      2.038E-13,  2.038E-13,  2.038E-13,  1.019E-13,  2.038E-13,
     8      2.038E-13,   .000E+00,  2.038E-13, -1.019E-13,  3.057E-13,
     9      1.019E-13,  1.019E-13,  3.057E-13,  3.057E-13, -1.019E-13,
     $      1.019E-13,  4.076E-13,  5.095E-13,  2.038E-13,  5.095E-13,
     1      4.076E-13, -2.038E-13,  3.057E-13, -2.038E-13, -2.038E-13,
     2      2.038E-13,  4.076E-13,  4.076E-13,   .000E+00, -4.076E-13,
     3      2.038E-13, -4.076E-13,  2.038E-13,  2.038E-13,   .000E+00,
     4       .000E+00, -2.038E-13,  2.038E-13,  4.076E-13,  4.076E-13,
     5      2.038E-13,   .000E+00,  8.152E-13,  4.076E-13,   .000E+00,
     6      4.076E-13,  1.630E-12,  8.152E-13,  8.152E-13,  1.223E-12,
     7      8.152E-13,  8.152E-13,  4.076E-13,  1.630E-12,  4.076E-13,
     8     -8.152E-13,  4.076E-13,  8.152E-13,  1.630E-12,  4.076E-13,
     9       .000E+00,   .000E+00,  8.152E-13,  4.076E-13,   .000E+00,
     $     -4.076E-13,  4.076E-13,  4.076E-13,  4.076E-13,  6.114E-13/
      DATA (Z(I),I=  301,  400)/
     1      2.038E-13,  6.114E-13,  4.076E-13,  2.038E-13,   .000E+00,
     2      6.114E-13,  2.038E-13, -2.038E-13,  2.038E-13,  6.114E-13,
     3      6.114E-13,  2.038E-13,  4.076E-13,  4.076E-13,   .000E+00,
     4      8.152E-13,   .000E+00,  4.076E-13,  6.114E-13,  2.038E-13,
     5      2.038E-13,  6.114E-13,   .000E+00,   .000E+00,  2.038E-13,
     6      4.076E-13,  2.038E-13,  2.038E-13,   .000E+00,  2.038E-13,
     7       .000E+00,  4.076E-13,  6.114E-13,   .000E+00,  2.038E-13,
     8      2.038E-13,  2.038E-13,  2.038E-13,  8.152E-13,   .000E+00,
     9     -2.038E-13,  6.114E-13,  6.114E-13,  2.038E-13,  6.114E-13,
     $       .000E+00,  6.114E-13,   .000E+00, -2.038E-13,  8.152E-13,
     1      4.076E-13,   .000E+00,  6.114E-13,  2.038E-13,  4.076E-13,
     2      6.114E-13,  6.114E-13,   .000E+00,  4.076E-13,   .000E+00,
     3       .000E+00,  4.076E-13,  4.076E-13,  1.630E-12,  8.152E-13,
     4      8.152E-13,  4.076E-13,  8.152E-13,  8.152E-13,  4.076E-13,
     5      1.223E-12,  1.630E-12,  1.630E-12,  4.076E-13,   .000E+00,
     6      1.630E-12, -4.076E-13, -4.076E-13,  2.038E-12,   .000E+00,
     7       .000E+00,  8.152E-13,   .000E+00,  8.152E-13,  1.630E-12,
     8      8.152E-13,   .000E+00, -8.152E-13,  8.152E-13,  8.152E-13,
     9      2.446E-12,   .000E+00,  8.152E-13,  8.152E-13,  8.152E-13,
     $      1.630E-12, -8.152E-13,  2.446E-12,  8.152E-13, -8.152E-13/
      DATA (Z(I),I=  401,  500)/
     1      1.630E-12,  8.152E-13,  8.152E-13,  1.630E-12,   .000E+00,
     2      8.152E-13,   .000E+00,   .000E+00,  8.152E-13,  8.152E-13,
     3       .000E+00,  1.630E-12,  8.152E-13,   .000E+00,  8.152E-13,
     4      8.152E-13,   .000E+00,  8.152E-13,   .000E+00,  8.152E-13,
     5       .000E+00,  1.630E-12,  8.152E-13,  8.152E-13,  8.152E-13,
     6      8.152E-13,  8.152E-13,  4.076E-13,  1.223E-12,  1.223E-12,
     7     -4.076E-13,  1.223E-12,  1.630E-12,  2.038E-12,   .000E+00,
     8     -8.152E-13,  4.076E-13,   .000E+00,  1.630E-12,  8.152E-13,
     9      8.152E-13,   .000E+00,  4.076E-13, -8.152E-13,  4.076E-13,
     $      1.223E-12,  4.076E-13,  4.076E-13,  1.630E-12,  1.223E-12,
     1       .000E+00,   .000E+00,  4.076E-13,  8.152E-13,  1.630E-12,
     2      1.223E-12,  4.076E-13,  8.152E-13,   .000E+00, -4.076E-13,
     3      1.223E-12, -4.076E-13,  8.152E-13,  1.630E-12,   .000E+00,
     4      8.152E-13,   .000E+00,  1.630E-12, -8.152E-13,  8.152E-13,
     5       .000E+00,  1.630E-12,  8.152E-13,  8.152E-13,   .000E+00,
     6      1.630E-12,   .000E+00,   .000E+00,  8.152E-13,  8.152E-13,
     7      1.630E-12,  1.630E-12,   .000E+00,  1.630E-12,  8.152E-13,
     8      3.261E-12,  8.152E-13,  2.446E-12,  3.261E-12,  3.261E-12,
     9      1.630E-12,   .000E+00,  3.261E-12, -1.630E-12, -1.630E-12,
     $     -1.630E-12,  4.891E-12,  1.630E-12,  1.630E-12,  3.261E-12/
      DATA (Z(I),I=  501,  600)/
     1      4.891E-12,  1.630E-12,  4.891E-12,   .000E+00,   .000E+00,
     2     -3.261E-12,  4.891E-12,  3.261E-12,  4.891E-12, -1.630E-12,
     3       .000E+00,  3.261E-12,  1.630E-12, -3.261E-12,  1.630E-12,
     4      3.261E-12, -3.261E-12,  4.891E-12,  3.261E-12,   .000E+00,
     5      3.261E-12, -1.630E-12,  1.630E-12,  1.630E-12,  1.630E-12,
     6      1.630E-12, -1.630E-12,  3.261E-12,  1.630E-12,  1.630E-12,
     7     -8.152E-13,  1.630E-12,   .000E+00,  2.446E-12,   .000E+00,
     8     -8.152E-13, -8.152E-13, -8.152E-13, -8.152E-13,  3.261E-12,
     9       .000E+00,  3.261E-12,   .000E+00,  8.152E-13,  3.261E-12,
     $      1.630E-12,   .000E+00,   .000E+00,  3.261E-12,   .000E+00,
     1       .000E+00,   .000E+00,  3.261E-12,  8.152E-13, -1.630E-12,
     2       .000E+00,  1.630E-12,  8.152E-13,  4.076E-12,  8.152E-13,
     3       .000E+00,   .000E+00,  3.261E-12,   .000E+00,  1.630E-12,
     4      3.261E-12,  4.076E-12,  8.152E-13,  4.076E-12,  2.446E-12,
     5     -1.630E-12,  3.261E-12,  2.446E-12, -8.152E-13,   .000E+00,
     6      1.630E-12,  2.446E-12,  1.630E-12,  1.630E-12,  3.261E-12,
     7      3.261E-12,   .000E+00, -1.630E-12,  1.630E-12,  1.630E-12,
     8       .000E+00, -1.630E-12,  1.630E-12,  6.522E-12,  3.261E-12,
     9       .000E+00,   .000E+00,  1.630E-12, -1.630E-12,  6.522E-12,
     $      1.630E-12,  1.630E-12, -1.630E-12,  1.630E-12,   .000E+00/
      DATA (Z(I),I=  601,  700)/
     1      1.630E-12,  3.261E-12,  6.522E-12,  3.261E-12,  4.891E-12,
     2      3.261E-12,  3.261E-12,  3.261E-12,  3.261E-12,  3.261E-12,
     3      3.261E-12,   .000E+00,  6.522E-12, -3.261E-12,  3.261E-12,
     4       .000E+00,  3.261E-12, -3.261E-12,  3.261E-12,  6.522E-12,
     5       .000E+00,  3.261E-12,   .000E+00,  6.522E-12,   .000E+00,
     6       .000E+00,   .000E+00,  3.261E-12,  6.522E-12,   .000E+00,
     7      3.261E-12,   .000E+00,  6.522E-12,  6.522E-12,  4.891E-12,
     8      3.261E-12,  8.152E-12,  4.891E-12,  3.261E-12,  4.891E-12,
     9      1.630E-12,   .000E+00,   .000E+00,  4.891E-12,  6.522E-12,
     $      1.630E-12, -3.261E-12,  4.891E-12,  8.152E-12,  3.261E-12,
     1      1.630E-12,  1.630E-12, -1.630E-12,  3.261E-12,  1.630E-12,
     2     -3.261E-12,  4.891E-12,  1.630E-12,  4.891E-12,  1.630E-12,
     3      1.630E-12,   .000E+00,  4.891E-12,  3.261E-12,  1.630E-12,
     4     -1.630E-12,  3.261E-12,  3.261E-12,  1.630E-12,  1.630E-12,
     5      4.891E-12, -1.630E-12, -3.261E-12,   .000E+00,  1.630E-12,
     6      3.261E-12,  1.630E-12,  1.630E-12,  4.891E-12,  6.522E-12,
     7      1.630E-12,   .000E+00,  4.891E-12,  4.891E-12,  1.630E-12,
     8      1.630E-12,  3.261E-12,  1.630E-12,  1.630E-12, -1.630E-12,
     9      1.630E-12,  4.891E-12,   .000E+00,   .000E+00,  1.630E-12,
     $     -6.522E-12,  3.261E-12,  3.261E-12,  3.261E-12,  3.261E-12/
      DATA (Z(I),I=  701,  800)/
     1      3.261E-12,   .000E+00,  3.261E-12,  3.261E-12, -3.261E-12,
     2     -3.261E-12,  3.261E-12,  6.522E-12,  3.261E-12, -3.261E-12,
     3      3.261E-12,  3.261E-12,  6.522E-12,  3.261E-12, -3.261E-12,
     4      9.783E-12,  3.261E-12,  3.261E-12,  3.261E-12,  6.522E-12,
     5       .000E+00,   .000E+00,  9.783E-12,  3.261E-12,  6.522E-12,
     6      9.783E-12,  6.522E-12,   .000E+00,  6.522E-12,   .000E+00,
     7      9.783E-12,  6.522E-12,  6.522E-12, -3.261E-12,  1.304E-11,
     8       .000E+00, -3.261E-12,  6.522E-12,   .000E+00,   .000E+00,
     9     -3.261E-12, -6.522E-12,  3.261E-12,  6.522E-12, -3.261E-12,
     $      6.522E-12,  9.783E-12,  6.522E-12,   .000E+00, -6.522E-12,
     1      9.783E-12,  6.522E-12,  3.261E-12,  3.261E-12,  6.522E-12,
     2      3.261E-12,  3.261E-12,   .000E+00,  9.783E-12, -3.261E-12,
     3      3.261E-12,   .000E+00,  6.522E-12,  6.522E-12,  3.261E-12,
     4       .000E+00,   .000E+00,  3.261E-12,  6.522E-12,  3.261E-12,
     5       .000E+00,  3.261E-12,  6.522E-12,  6.522E-12,   .000E+00,
     6       .000E+00,  3.261E-12, -3.261E-12,  9.783E-12,  3.261E-12,
     7      3.261E-12,  6.522E-12,  3.261E-12, -3.261E-12,   .000E+00,
     8      3.261E-12, -3.261E-12,  9.783E-12,  1.304E-11, -3.003E-08,
     9     -2.136E-07, -3.476E-07, -3.507E-07, -3.498E-07, -3.441E-07,
     $     -3.405E-07, -3.384E-07, -3.439E-07, -3.538E-07, -3.699E-07/
      DATA (Z(I),I=  801,  900)/
     1     -3.651E-07, -3.462E-07, -3.292E-07, -3.177E-07, -3.239E-07,
     2     -3.321E-07, -3.447E-07, -3.404E-07, -3.284E-07, -3.301E-07,
     3     -3.295E-07, -3.214E-07, -3.194E-07, -3.230E-07, -3.202E-07,
     4     -3.182E-07, -3.245E-07, -3.272E-07, -3.181E-07, -3.133E-07,
     5     -3.115E-07, -3.134E-07, -3.144E-07, -3.226E-07, -3.292E-07,
     6     -3.239E-07, -3.248E-07, -3.088E-07, -3.269E-07, -3.166E-07,
     7     -3.283E-07, -3.143E-07, -3.064E-07, -3.140E-07, -2.938E-07,
     8     -2.938E-07, -3.014E-07, -2.873E-07, -2.809E-07, -2.809E-07,
     9     -3.092E-07, -2.748E-07, -2.809E-07, -2.744E-07, -2.809E-07,
     $     -2.669E-07, -2.666E-07, -2.870E-07, -2.806E-07, -2.806E-07,
     1     -2.806E-07, -2.870E-07, -2.730E-07, -2.590E-07, -2.512E-07,
     2     -2.792E-07, -2.856E-07, -2.860E-07, -2.781E-07, -2.565E-07,
     3     -2.425E-07, -2.626E-07, -2.767E-07, -2.624E-07, -2.484E-07,
     4     -2.343E-07, -2.484E-07, -2.689E-07, -2.268E-07, -2.329E-07,
     5     -2.470E-07, -2.193E-07, -2.254E-07, -2.318E-07, -2.318E-07,
     6     -2.459E-07, -2.240E-07, -2.021E-07, -2.161E-07, -2.021E-07,
     7     -2.162E-07, -2.226E-07, -2.022E-07, -2.086E-07, -2.083E-07,
     8     -2.148E-07, -2.288E-07, -2.285E-07, -2.286E-07, -2.288E-07,
     9     -2.209E-07, -2.069E-07, -2.069E-07, -2.131E-07, -2.131E-07,
     $     -1.991E-07, -1.991E-07, -1.991E-07, -1.707E-07, -1.851E-07/
      DATA (Z(I),I=  901, 1000)/
     1     -1.772E-07, -1.775E-07, -1.912E-07, -1.976E-07, -2.117E-07,
     2     -1.974E-07, -1.772E-07, -1.837E-07, -1.693E-07, -1.693E-07,
     3     -1.769E-07, -1.693E-07, -1.632E-07, -1.489E-07, -1.352E-07,
     4     -1.492E-07, -1.495E-07, -1.556E-07, -1.492E-07, -1.352E-07,
     5     -1.352E-07, -1.492E-07, -1.492E-07, -1.349E-07, -1.285E-07,
     6     -1.147E-07, -1.287E-07, -1.150E-07, -1.147E-07, -1.007E-07,
     7     -9.455E-08, -8.022E-08, -6.618E-08, -7.990E-08, -8.016E-08,
     8     -7.992E-08, -8.779E-08, -7.986E-08, -1.015E-07, -9.567E-08,
     9     -7.373E-08, -8.135E-08, -9.567E-08, -7.520E-08, -8.955E-08,
     $     -7.521E-08, -1.033E-07, -8.133E-08, -8.895E-08, -7.493E-08,
     1     -5.446E-08, -6.850E-08, -9.655E-08, -6.846E-08, -9.040E-08,
     2     -7.640E-08, -7.669E-08, -6.239E-08, -5.600E-08, -7.612E-08,
     3     -6.998E-08, -4.985E-08, -5.597E-08, -7.032E-08, -7.032E-08,
     4     -7.031E-08, -6.243E-08, -5.453E-08, -5.458E-08, -4.817E-08,
     5     -5.461E-08, -4.059E-08, -6.867E-08, -4.671E-08, -5.316E-08,
     6     -3.889E-08, -3.915E-08, -3.888E-08, -3.131E-08, -1.732E-08,
     7     -1.729E-08, -3.134E-08, -1.735E-08, -9.496E-09, -9.835E-09,
     8     -1.596E-08, -3.031E-08, -3.644E-08, -4.288E-08, -2.243E-08,
     9     -1.488E-08, -8.478E-10,  2.750E-08,  3.508E-08,  3.537E-08,
     $      3.540E-08,  2.780E-08, -6.711E-09, -6.391E-10,  2.739E-08/
      DATA (Z(I),I= 1001, 1100)/
     1      3.530E-08,  4.290E-08,  4.289E-08,  5.107E-08,  4.316E-08,
     2      4.345E-08,  5.102E-08,  2.937E-08,  3.696E-08,  3.699E-08,
     3      3.695E-08,  5.099E-08,  4.486E-08,  4.480E-08,  4.479E-08,
     4      5.240E-08,  4.448E-08,  6.496E-08,  7.107E-08,  8.540E-08,
     5      5.706E-08,  2.900E-08,  4.300E-08,  7.128E-08,  9.295E-08,
     6      9.322E-08,  9.936E-08,  9.935E-08,  9.934E-08,  1.134E-07,
     7      9.146E-08,  1.195E-07,  1.055E-07,  9.177E-08,  9.207E-08,
     8      9.848E-08,  9.876E-08,  9.846E-08,  9.846E-08,  1.125E-07,
     9      1.122E-07,  1.122E-07,  1.183E-07,  1.324E-07,  1.321E-07,
     $      1.245E-07,  1.245E-07,  1.108E-07,  1.251E-07,  1.315E-07,
     1      1.380E-07,  1.303E-07,  1.227E-07,  1.148E-07,  1.148E-07,
     2      1.289E-07,  1.350E-07,  1.210E-07,  1.351E-07,  1.556E-07,
     3      1.559E-07,  1.479E-07,  1.681E-07,  1.465E-07,  1.591E-07,
     4      1.591E-07,  1.667E-07,  1.808E-07,  1.670E-07,  1.667E-07,
     5      1.592E-07,  1.376E-07,  1.498E-07,  1.562E-07,  1.986E-07,
     6      2.206E-07,  2.209E-07,  2.147E-07,  2.069E-07,  2.069E-07,
     7      2.130E-07,  1.990E-07,  2.051E-07,  2.268E-07,  2.552E-07,
     8      2.689E-07,  2.973E-07,  3.034E-07,  3.177E-07,  3.101E-07,
     9      3.242E-07,  3.102E-07,  3.038E-07,  3.053E-07,  2.909E-07,
     $      2.909E-07,  2.988E-07,  2.988E-07,  3.128E-07,  3.128E-07/
      DATA (Z(I),I= 1101, 1200)/
     1      3.193E-07,  3.257E-07,  3.257E-07,  3.461E-07,  3.321E-07,
     2      3.321E-07,  3.257E-07,  3.256E-07,  3.257E-07,  3.394E-07,
     3      3.391E-07,  3.248E-07,  3.248E-07,  3.251E-07,  3.172E-07,
     4      3.175E-07,  3.239E-07,  3.236E-07,  3.028E-07,  3.229E-07,
     5      3.306E-07,  3.309E-07,  3.528E-07,  3.531E-07,  3.394E-07,
     6      3.315E-07,  3.175E-07,  3.236E-07,  3.017E-07,  3.157E-07,
     7      3.157E-07,  3.295E-07,  3.154E-07,  3.154E-07,  3.093E-07,
     8      2.952E-07,  2.812E-07,  2.671E-07,  2.672E-07,  2.734E-07,
     9      2.731E-07,  2.792E-07,  2.997E-07,  2.933E-07,  2.792E-07,
     $      2.792E-07,  2.653E-07,  2.372E-07,  2.289E-07,  2.290E-07,
     1      2.276E-07,  2.132E-07,  2.211E-07,  2.431E-07,  2.369E-07,
     2      2.448E-07,  2.388E-07,  2.247E-07,  2.043E-07,  2.043E-07,
     3      1.903E-07,  1.967E-07,  2.251E-07,  2.235E-07,  2.300E-07,
     4      2.301E-07,  2.234E-07,  2.295E-07,  2.368E-07,  2.365E-07,
     5      2.368E-07,  2.089E-07,  2.093E-07,  1.894E-07,  1.757E-07,
     6      1.895E-07,  2.035E-07,  2.319E-07,  2.394E-07,  2.236E-07,
     7      2.376E-07,  1.937E-07,  1.658E-07,  1.374E-07,  1.456E-07,
     8      1.395E-07,  1.690E-07,  1.769E-07,  1.849E-07,  1.988E-07,
     9      1.988E-07,  1.769E-07,  1.708E-07,  1.976E-07,  2.042E-07,
     $      2.105E-07,  2.171E-07,  2.310E-07,  2.249E-07,  2.389E-07/
      DATA (Z(I),I= 1201, 1300)/
     1      2.246E-07,  2.109E-07,  1.822E-07,  1.682E-07,  1.536E-07,
     2      1.467E-07,  1.529E-07,  1.593E-07,  1.521E-07,  1.866E-07,
     3      1.930E-07,  1.729E-07,  1.947E-07,  1.745E-07,  1.623E-07,
     4      1.562E-07,  1.636E-07,  1.629E-07,  1.626E-07,  1.623E-07,
     5      1.688E-07,  1.750E-07,  1.676E-07,  1.878E-07,  1.946E-07,
     6      2.151E-07,  2.086E-07,  2.086E-07,  1.946E-07,  2.087E-07,
     7      2.007E-07,  1.784E-07,  1.630E-07,  1.613E-07,  1.551E-07,
     8      1.425E-07,  1.507E-07,  1.242E-07,  1.184E-07,  1.259E-07,
     9      1.605E-07,  1.540E-07,  1.962E-07,  2.102E-07,  1.804E-07,
     $      1.585E-07,  1.225E-07,  9.271E-08,  7.834E-08,  1.067E-07,
     1      1.144E-07,  1.205E-07,  1.489E-07,  1.568E-07,  1.162E-07,
     2      1.039E-07,  1.399E-07,  1.196E-07,  1.263E-07,  1.267E-07,
     3      9.704E-08,  8.956E-08,  6.793E-08,  9.629E-08,  1.241E-07,
     4      1.383E-07,  1.662E-07,  1.742E-07,  1.256E-07,  7.070E-08,
     5      6.420E-08,  3.083E-08,  5.131E-08,  5.781E-08,  7.816E-08,
     6      6.300E-08,  8.356E-08,  7.602E-08,  9.796E-08,  9.801E-08,
     7      1.402E-07,  1.758E-07,  1.478E-07,  1.261E-07,  6.339E-08,
     8      2.789E-08, -2.071E-08, -2.791E-08, -4.312E-08, -3.657E-08,
     9     -9.756E-09,  3.890E-08,  2.843E-09, -3.339E-09, -3.133E-08,
     $     -5.142E-08, -7.951E-08, -5.144E-08, -4.518E-08, -9.156E-09/
      DATA (Z(I),I= 1301, 1400)/
     1      1.096E-08,  1.085E-08,  3.110E-08,  5.755E-08,  7.769E-08,
     2      6.960E-08,  6.819E-08,  4.017E-08,  2.609E-08, -8.452E-09,
     3     -2.233E-08, -4.403E-08, -1.706E-08,  1.748E-08,  2.296E-09,
     4      1.513E-08, -6.365E-09, -3.449E-08, -6.250E-08, -5.452E-08,
     5     -8.259E-08, -1.186E-07, -1.342E-07, -1.281E-07, -1.579E-07,
     6     -1.736E-07, -1.812E-07, -1.666E-07, -1.662E-07, -1.655E-07,
     7     -1.787E-07, -1.864E-07, -2.023E-07, -2.179E-07, -2.260E-07,
     8     -2.417E-07, -2.499E-07, -2.504E-07, -2.510E-07, -2.516E-07,
     9     -2.657E-07, -2.738E-07, -2.889E-07, -3.182E-07, -3.259E-07,
     $     -3.346E-07, -3.486E-07, -3.754E-07, -3.819E-07, -3.947E-07,
     1     -3.998E-07, -4.139E-07, -4.214E-07, -4.162E-07, -4.378E-07,
     2     -4.672E-07, -4.888E-07, -4.964E-07, -4.825E-07, -4.964E-07,
     3     -4.964E-07, -5.040E-07, -5.524E-07, -4.964E-07, -4.963E-07,
     4     -4.963E-07, -4.963E-07, -4.963E-07, -4.964E-07, -4.963E-07,
     5     -4.964E-07, -4.963E-07, -4.963E-07, -4.963E-07, -4.963E-07,
     6     -5.724E-07, -4.964E-07, -4.963E-07, -6.365E-07, -4.963E-07,
     7     -5.577E-07, -6.978E-07, -5.575E-07, -5.576E-07, -6.950E-07,
     8     -5.577E-07, -5.576E-07, -6.951E-07, -5.577E-07, -5.577E-07,
     9     -5.577E-07, -5.576E-07, -5.576E-07, -6.950E-07, -5.577E-07,
     $     -6.978E-07, -5.576E-07, -5.577E-07, -5.576E-07, -5.577E-07/
      DATA (Z(I),I= 1401, 1500)/
     1     -5.576E-07, -5.577E-07, -5.576E-07, -7.010E-07, -8.381E-07,
     2     -8.383E-07, -6.977E-07, -8.411E-07, -8.411E-07, -5.604E-07,
     3     -8.411E-07, -7.005E-07, -8.410E-07, -7.006E-07, -6.978E-07,
     4     -8.379E-07, -7.006E-07, -8.381E-07, -8.378E-07, -8.349E-07,
     5     -8.380E-07, -8.380E-07, -8.351E-07, -8.379E-07, -8.379E-07,
     6     -8.380E-07, -8.379E-07, -8.350E-07, -8.350E-07, -8.350E-07,
     7     -8.351E-07, -9.751E-07, -9.751E-07, -8.350E-07, -8.351E-07,
     8     -8.380E-07, -8.379E-07, -9.751E-07, -8.377E-07, -8.379E-07,
     9     -8.379E-07, -9.140E-07, -9.139E-07, -8.527E-07, -9.286E-07,
     $     -8.526E-07, -9.286E-07, -7.881E-07, -8.526E-07, -7.882E-07,
     1     -6.477E-07, -7.122E-07, -7.121E-07, -7.883E-07, -6.475E-07,
     2     -8.524E-07, -8.522E-07, -6.478E-07, -7.881E-07, -7.883E-07,
     3     -7.124E-07, -6.475E-07, -6.476E-07, -7.238E-07, -6.478E-07,
     4     -5.834E-07, -8.637E-07, -5.832E-07, -8.025E-07, -5.832E-07,
     5     -5.219E-07, -5.218E-07, -7.412E-07, -7.413E-07, -6.010E-07,
     6     -6.011E-07, -7.413E-07, -7.410E-07, -5.221E-07, -5.219E-07,
     7     -4.576E-07, -6.007E-07, -4.604E-07, -4.603E-07, -3.814E-07,
     8     -3.169E-07, -3.199E-07, -3.813E-07, -3.813E-07, -4.601E-07,
     9     -3.201E-07, -3.203E-07, -3.201E-07, -3.200E-07, -3.202E-07,
     $     -3.991E-07, -2.587E-07, -2.560E-07, -2.589E-07, -2.587E-07/
      DATA (Z(I),I= 1501, 1600)/
     1     -2.591E-07, -3.202E-07, -3.950E-08, -3.960E-08,  3.970E-08,
     2      3.965E-08, -1.009E-07, -3.950E-08,  3.965E-08, -1.009E-07,
     3      1.038E-07, -3.955E-08, -3.950E-08, -3.955E-08, -3.960E-08,
     4     -3.950E-08, -3.955E-08,  2.176E-08,  9.783E-08, -4.263E-08,
     5      3.668E-08, -1.039E-07,  3.329E-08,  3.605E-08, -2.791E-08,
     6     -2.515E-08,  1.121E-07,  1.149E-07,  1.150E-07,  1.150E-07,
     7      1.148E-07,  1.150E-07,  1.149E-07,  1.150E-07, -2.838E-08,
     8     -2.823E-08, -2.833E-08,  1.118E-07, -3.157E-08,  4.727E-08,
     9     -9.266E-08, -9.276E-08,  5.061E-08,  2.666E-07,  1.263E-07,
     $      1.906E-07,  1.262E-07, -1.403E-08, -1.544E-07, -9.308E-08,
     1      5.024E-08, -9.318E-08, -1.543E-07, -1.545E-07, -1.435E-08,
     2     -1.440E-08, -7.873E-08, -7.863E-08, -7.878E-08, -7.883E-08,
     3     -7.878E-08, -7.878E-08, -2.221E-07, -7.873E-08, -2.833E-07,
     4     -7.878E-08, -1.432E-07, -2.220E-07, -2.865E-07, -3.478E-07,
     5     -3.478E-07, -3.478E-07, -3.478E-07, -4.091E-07, -3.448E-07,
     6     -3.448E-07, -3.448E-07, -2.046E-07, -2.046E-07, -4.090E-07,
     7     -2.044E-07, -3.446E-07, -4.092E-07, -2.045E-07, -2.046E-07,
     8     -2.045E-07, -2.046E-07, -2.046E-07, -2.044E-07, -2.047E-07,
     9     -2.045E-07, -2.046E-07, -2.046E-07, -4.852E-07, -3.446E-07,
     $     -2.806E-07, -2.806E-07, -2.805E-07, -5.611E-07, -3.418E-07/
      DATA (Z(I),I= 1601, 1700)/
     1     -3.416E-07, -4.820E-07, -4.031E-07, -3.418E-07, -3.418E-07,
     2     -4.819E-07, -4.059E-07, -3.447E-07, -3.447E-07, -3.447E-07,
     3     -3.447E-07, -4.848E-07, -3.447E-07, -3.446E-07, -3.448E-07,
     4     -3.447E-07, -3.446E-07, -3.447E-07, -3.447E-07, -4.202E-07,
     5     -4.203E-07, -4.205E-07, -4.204E-07, -2.801E-07, -2.802E-07,
     6     -2.801E-07, -4.203E-07, -4.203E-07, -2.830E-07, -2.830E-07,
     7     -4.230E-07, -4.233E-07, -2.831E-07, -4.230E-07, -2.829E-07,
     8     -3.619E-07, -3.590E-07, -3.618E-07, -4.992E-07, -4.991E-07,
     9     -4.992E-07, -4.378E-07, -4.378E-07, -2.973E-07, -7.816E-08,
     $     -2.974E-07, -2.186E-07, -2.186E-07, -2.186E-07, -7.800E-08,
     1     -7.800E-08, -2.181E-07, -7.816E-08, -2.182E-07, -7.810E-08,
     2     -1.568E-07, -7.758E-08, -7.774E-08,  6.250E-08, -7.810E-08,
     3     -2.183E-07, -1.568E-07, -1.569E-07, -2.179E-07, -7.810E-08,
     4     -7.800E-08, -7.795E-08, -7.800E-08, -2.973E-07, -2.973E-07,
     5     -7.805E-08, -2.185E-07, -3.586E-07, -3.588E-07, -2.973E-07,
     6     -1.541E-07, -3.586E-07, -2.152E-07, -7.805E-08, -2.154E-07,
     7     -1.541E-07, -7.518E-08, -2.154E-07, -7.523E-08, -1.393E-07,
     8     -7.518E-08, -1.396E-07,  7.826E-10, -1.396E-07, -2.798E-07,
     9      9.391E-10, -2.798E-07, -2.799E-07, -1.396E-07, -1.396E-07,
     $     -1.425E-07, -1.397E-07, -1.397E-07, -1.430E-07, -1.427E-07/
      DATA (Z(I),I= 1701, 1800)/
     1     -1.430E-07, -2.609E-09,  4.174E-10,  5.217E-10,  3.652E-10,
     2     -2.348E-09, -1.397E-07,  3.652E-10,  5.739E-10,  4.696E-10,
     3      4.696E-10,  4.696E-10,  2.609E-10,  4.696E-10, -6.407E-08,
     4      1.406E-07,  7.654E-08, -6.402E-08, -6.402E-08, -6.412E-08,
     5     -6.402E-08,  1.205E-08, -6.396E-08, -6.407E-08, -6.402E-08,
     6     -6.407E-08, -6.402E-08, -6.402E-08, -1.252E-07,  1.487E-08,
     7     -6.130E-08, -6.130E-08, -1.256E-07, -2.046E-07, -2.045E-07,
     8     -6.130E-08, -6.130E-08, -6.438E-08, -6.449E-08, -1.288E-07,
     9     -6.438E-08, -6.449E-08, -6.449E-08, -6.443E-08, -6.443E-08,
     $     -6.438E-08, -6.438E-08, -1.258E-07,  1.435E-08,  1.440E-08,
     1      1.440E-08, -2.645E-08, -4.043E-08, -8.129E-08, -8.791E-08,
     2     -7.268E-08, -5.113E-08, -5.770E-08, -7.821E-08, -9.219E-08,
     3     -1.203E-07, -1.483E-07, -1.639E-07, -1.921E-07, -1.559E-07,
     4     -1.422E-07, -1.278E-07, -7.962E-08, -1.560E-08,  4.664E-08,
     5      6.057E-08,  4.649E-08,  1.883E-08,  4.696E-09, -2.332E-08,
     6     -3.751E-08, -3.751E-08, -5.937E-08, -6.553E-08, -7.983E-08,
     7     -7.983E-08, -6.584E-08, -5.932E-08, -6.548E-08, -1.079E-07,
     8     -1.436E-07, -1.717E-07, -1.576E-07, -1.782E-07, -1.719E-07,
     9     -1.502E-07, -1.489E-07, -9.939E-08, -9.814E-08, -9.819E-08,
     $     -1.110E-07, -1.174E-07, -1.313E-07, -1.454E-07, -1.594E-07/
      DATA (Z(I),I= 1801, 1900)/
     1     -1.454E-07, -1.314E-07, -1.173E-07, -1.044E-07, -1.121E-07,
     2     -1.198E-07, -1.554E-07, -1.630E-07, -1.502E-07, -1.798E-07,
     3     -1.797E-07, -1.578E-07, -1.653E-07, -1.654E-07, -1.657E-07,
     4     -1.578E-07, -1.517E-07, -1.514E-07, -1.374E-07, -1.233E-07,
     5     -1.297E-07, -1.516E-07, -1.513E-07, -1.798E-07, -1.793E-07,
     6     -1.933E-07, -1.934E-07, -2.013E-07, -1.796E-07, -1.513E-07,
     7     -1.437E-07, -1.145E-07, -1.007E-07, -9.318E-08, -9.198E-08,
     8     -9.835E-08, -1.049E-07, -9.078E-08, -9.083E-08, -9.845E-08,
     9     -1.266E-07, -1.545E-07, -2.108E-07, -2.169E-07, -1.949E-07,
     $     -2.011E-07, -1.853E-07, -1.853E-07, -1.555E-07, -1.413E-07,
     1     -1.333E-07, -1.474E-07, -1.478E-07, -1.619E-07, -1.834E-07,
     2     -2.255E-07, -2.536E-07, -2.536E-07, -2.472E-07, -2.677E-07,
     3     -2.255E-07, -2.055E-07, -1.555E-07, -1.495E-07, -1.480E-07,
     4     -1.684E-07, -1.748E-07, -1.955E-07, -1.880E-07, -1.945E-07,
     5     -2.007E-07, -1.926E-07, -1.989E-07, -1.910E-07, -1.831E-07,
     6     -1.469E-07, -1.393E-07, -1.454E-07, -1.658E-07, -2.003E-07,
     7     -2.352E-07, -2.133E-07, -1.981E-07, -1.844E-07, -1.705E-07,
     8     -1.780E-07, -1.932E-07, -1.930E-07, -1.724E-07, -1.521E-07,
     9     -1.176E-07, -1.393E-07, -1.676E-07, -1.812E-07, -1.955E-07,
     $     -1.672E-07, -1.536E-07, -1.252E-07, -1.113E-07, -9.715E-08/
      DATA (Z(I),I= 1901, 2000)/
     1     -9.725E-08, -6.918E-08, -1.112E-07, -9.720E-08, -1.256E-07,
     2     -1.111E-07, -8.343E-08, -8.309E-08, -5.502E-08, -2.687E-08,
     3     -2.721E-08, -2.723E-08, -2.079E-08, -1.463E-08,  5.113E-09,
     4      1.158E-08, -5.241E-08, -1.165E-07, -1.602E-07, -1.805E-07,
     5     -1.456E-07, -1.176E-07, -1.169E-07, -1.166E-07, -1.583E-07,
     6     -1.580E-07, -1.658E-07, -1.382E-07, -1.103E-07, -8.259E-08,
     7     -1.109E-07, -1.252E-07, -1.611E-07, -1.754E-07, -1.626E-07,
     8     -1.483E-07, -1.561E-07, -1.574E-07, -1.790E-07, -2.005E-07,
     9     -2.363E-07, -2.081E-07, -2.082E-07, -1.801E-07, -2.020E-07,
     $     -2.160E-07, -2.082E-07, -2.020E-07, -1.741E-07, -1.325E-07,
     1     -1.047E-07, -9.861E-08, -1.127E-07, -9.884E-08, -1.131E-07,
     2     -1.270E-07, -1.406E-07, -1.546E-07, -1.478E-07, -1.194E-07,
     3     -1.333E-07, -1.050E-07, -1.190E-07, -1.114E-07, -1.319E-07,
     4     -1.321E-07, -1.246E-07, -1.450E-07, -1.590E-07, -1.656E-07,
     5     -1.935E-07, -2.140E-07, -2.205E-07, -2.345E-07, -2.550E-07,
     6     -2.475E-07, -2.615E-07, -2.335E-07, -2.411E-07, -2.414E-07,
     7     -2.273E-07, -1.928E-07, -1.788E-07, -1.709E-07, -1.709E-07,
     8     -1.850E-07, -1.909E-07, -1.908E-07, -2.049E-07, -1.967E-07,
     9     -2.032E-07, -2.452E-07, -2.589E-07, -3.211E-07, -3.352E-07,
     $     -3.133E-07, -2.710E-07, -2.275E-07, -2.278E-07, -2.203E-07/
      DATA (Z(I),I= 2001, 2100)/
     1     -2.267E-07, -2.392E-07, -2.600E-07, -2.665E-07, -2.729E-07,
     2     -2.793E-07, -2.934E-07, -3.153E-07, -3.291E-07, -3.229E-07,
     3     -3.153E-07, -2.871E-07, -2.730E-07, -2.950E-07, -3.166E-07,
     4     -3.510E-07, -3.791E-07, -3.446E-07, -3.446E-07, -3.447E-07,
     5     -3.307E-07, -3.231E-07, -3.512E-07, -3.436E-07, -3.439E-07,
     6     -3.503E-07, -3.500E-07, -3.565E-07, -3.708E-07, -3.845E-07,
     7     -3.845E-07, -4.065E-07, -3.986E-07, -4.061E-07, -4.065E-07,
     8     -4.205E-07, -3.925E-07, -3.861E-07, -3.580E-07, -3.720E-07,
     9     -3.720E-07, -4.001E-07, -4.141E-07, -4.140E-07, -4.138E-07,
     $     -4.338E-07, -4.263E-07, -4.123E-07, -3.900E-07, -3.620E-07,
     1     -3.480E-07, -3.265E-07, -3.408E-07, -3.268E-07, -3.470E-07,
     2     -3.674E-07, -4.095E-07, -4.515E-07, -4.723E-07, -4.863E-07,
     3     -5.143E-07, -5.205E-07, -4.925E-07, -4.571E-07, -4.151E-07,
     4     -4.011E-07, -3.932E-07, -3.713E-07, -3.652E-07, -3.574E-07,
     5     -3.495E-07, -3.632E-07, -3.635E-07, -3.977E-07, -4.602E-07,
     6     -5.084E-07, -5.569E-07, -5.286E-07, -4.930E-07, -4.431E-07,
     7     -4.216E-07, -4.061E-07, -4.262E-07, -4.187E-07, -4.248E-07,
     8     -4.234E-07, -4.018E-07, -4.219E-07, -4.223E-07, -4.640E-07,
     9     -4.924E-07, -5.283E-07, -5.424E-07, -5.223E-07, -4.802E-07,
     $     -4.522E-07, -4.241E-07, -4.178E-07, -4.181E-07, -3.976E-07/
      DATA (Z(I),I= 2101, 2200)/
     1     -4.256E-07, -4.472E-07, -4.674E-07, -4.750E-07, -4.825E-07,
     2     -4.702E-07, -4.562E-07, -4.498E-07, -4.716E-07, -4.792E-07,
     3     -4.867E-07, -4.884E-07, -4.820E-07, -4.618E-07, -4.355E-07,
     4     -4.290E-07, -4.664E-07, -5.023E-07, -5.177E-07, -5.191E-07,
     5     -4.852E-07, -4.446E-07, -4.101E-07, -3.975E-07, -4.272E-07,
     6     -4.426E-07, -4.505E-07, -4.235E-07, -3.815E-07, -3.330E-07,
     7     -2.910E-07, -3.124E-07, -3.279E-07, -3.699E-07, -3.978E-07,
     8     -3.914E-07, -3.569E-07, -3.166E-07, -2.821E-07, -2.896E-07,
     9     -3.115E-07, -3.269E-07, -3.485E-07, -3.280E-07, -3.140E-07,
     $     -2.999E-07, -2.863E-07, -3.079E-07, -3.139E-07, -3.139E-07,
     1     -3.218E-07, -3.075E-07, -3.496E-07, -3.417E-07, -3.277E-07,
     2     -3.415E-07, -3.476E-07, -3.695E-07, -3.757E-07, -3.617E-07,
     3     -3.617E-07, -3.603E-07, -3.463E-07, -3.463E-07, -3.448E-07,
     4     -3.448E-07, -3.448E-07, -3.509E-07, -3.714E-07, -3.775E-07,
     5     -3.918E-07, -3.840E-07, -3.764E-07, -3.545E-07, -3.265E-07,
     6     -3.531E-07, -3.812E-07, -4.156E-07, -4.361E-07, -4.362E-07,
     7     -4.143E-07, -4.208E-07, -3.928E-07, -4.068E-07, -4.349E-07,
     8     -4.553E-07, -4.895E-07, -4.755E-07, -4.616E-07, -4.256E-07,
     9     -4.119E-07, -4.181E-07, -4.325E-07, -4.386E-07, -4.670E-07,
     $     -4.734E-07, -4.734E-07, -4.659E-07, -4.723E-07, -5.004E-07/
      DATA (Z(I),I= 2201, 2300)/
     1     -5.141E-07, -5.342E-07, -5.763E-07, -5.623E-07, -5.404E-07,
     2     -5.124E-07, -4.905E-07, -4.905E-07, -4.905E-07, -5.109E-07,
     3     -5.250E-07, -5.455E-07, -5.315E-07, -5.379E-07, -5.382E-07,
     4     -5.303E-07, -5.239E-07, -4.958E-07, -5.020E-07, -5.098E-07,
     5     -4.879E-07, -4.879E-07, -4.800E-07, -4.876E-07, -4.879E-07,
     6     -5.016E-07, -5.159E-07, -5.439E-07, -5.296E-07, -5.158E-07,
     7     -5.159E-07, -5.094E-07, -4.875E-07, -4.735E-07, -4.735E-07,
     8     -4.735E-07, -4.811E-07, -4.671E-07, -4.954E-07, -4.893E-07,
     9     -4.673E-07, -4.534E-07, -4.394E-07, -4.037E-07, -4.037E-07,
     $     -4.241E-07, -4.662E-07, -4.942E-07, -5.226E-07, -5.147E-07,
     1     -5.007E-07, -5.147E-07, -5.007E-07, -5.007E-07, -4.928E-07,
     2     -5.008E-07, -4.791E-07, -5.072E-07, -5.198E-07, -5.338E-07,
     3     -5.478E-07, -5.324E-07, -5.327E-07, -5.108E-07, -4.892E-07,
     4     -5.093E-07, -5.514E-07, -5.715E-07, -6.060E-07, -6.481E-07,
     5     -5.982E-07, -5.766E-07, -5.483E-07, -5.203E-07, -5.344E-07,
     6     -5.405E-07, -5.265E-07, -5.469E-07, -5.405E-07, -5.466E-07,
     7     -5.326E-07, -5.324E-07, -5.607E-07, -5.542E-07, -5.747E-07,
     8     -5.826E-07, -5.966E-07, -5.966E-07, -5.765E-07, -5.625E-07,
     9     -5.487E-07, -5.423E-07, -5.438E-07, -5.300E-07, -5.236E-07,
     $     -5.311E-07, -5.527E-07, -5.466E-07, -5.541E-07, -5.480E-07/
      DATA (Z(I),I= 2301, 2400)/
     1     -5.415E-07, -5.289E-07, -5.009E-07, -4.883E-07, -5.099E-07,
     2     -5.037E-07, -4.973E-07, -4.911E-07, -4.286E-07, -3.660E-07,
     3     -3.035E-07, -2.489E-07, -2.844E-07, -3.340E-07, -3.760E-07,
     4     -4.335E-07, -4.410E-07, -4.269E-07, -4.488E-07, -4.348E-07,
     5     -4.423E-07, -4.286E-07, -4.221E-07, -4.081E-07, -4.019E-07,
     6     -4.019E-07, -3.599E-07, -3.459E-07, -3.179E-07, -3.395E-07,
     7     -3.473E-07, -3.754E-07, -4.034E-07, -4.031E-07, -4.033E-07,
     8     -3.969E-07, -3.969E-07, -3.890E-07, -3.891E-07, -4.031E-07,
     9     -4.031E-07, -4.031E-07, -4.031E-07, -4.031E-07, -4.095E-07,
     $     -4.235E-07, -4.019E-07, -4.160E-07, -3.958E-07, -3.882E-07,
     1     -3.742E-07, -3.804E-07, -3.944E-07, -4.011E-07, -4.073E-07,
     2     -4.073E-07, -3.994E-07, -3.997E-07, -3.919E-07, -4.059E-07,
     3     -3.983E-07, -3.983E-07, -4.045E-07, -3.902E-07, -3.963E-07,
     4     -3.963E-07, -4.025E-07, -4.165E-07, -4.230E-07, -4.510E-07,
     5     -4.793E-07, -5.276E-07, -5.492E-07, -5.212E-07, -5.074E-07,
     6     -4.935E-07, -4.576E-07, -4.576E-07, -4.640E-07, -4.921E-07,
     7     -4.985E-07, -4.982E-07, -4.985E-07, -4.985E-07, -4.944E-07,
     8     -4.952E-07, -5.176E-07, -5.243E-07, -5.594E-07, -5.868E-07,
     9     -6.078E-07, -6.009E-07, -5.939E-07, -5.869E-07, -5.502E-07,
     $     -5.515E-07, -5.487E-07, -5.599E-07, -5.661E-07, -5.695E-07/
      DATA (Z(I),I= 2401, 2500)/
     1     -5.667E-07, -5.574E-07, -5.560E-07, -5.532E-07, -5.221E-07,
     2     -5.314E-07, -5.061E-07, -4.870E-07, -4.811E-07, -4.715E-07,
     3     -4.773E-07, -4.683E-07, -4.601E-07, -4.733E-07, -4.546E-07,
     4     -4.414E-07, -4.385E-07, -4.191E-07, -4.363E-07, -4.309E-07,
     5     -4.395E-07, -4.499E-07, -4.353E-07, -4.183E-07, -4.019E-07,
     6     -4.052E-07, -3.913E-07, -3.900E-07, -3.902E-07, -3.931E-07,
     7     -3.953E-07, -3.960E-07, -3.944E-07, -3.943E-07, -3.948E-07,
     8     -3.911E-07, -3.817E-07, -3.704E-07, -3.583E-07, -3.475E-07,
     9     -3.403E-07, -3.394E-07, -3.356E-07, -3.333E-07, -3.309E-07,
     $     -3.257E-07, -3.206E-07, -3.148E-07, -3.096E-07, -3.108E-07,
     1     -3.247E-07, -3.364E-07, -3.517E-07, -3.635E-07, -3.555E-07,
     2     -3.400E-07, -3.238E-07, -3.082E-07, -3.003E-07, -3.170E-07,
     3     -3.359E-07, -3.518E-07, -3.707E-07, -3.698E-07, -3.543E-07,
     4     -3.388E-07, -3.232E-07, -3.092E-07, -3.154E-07, -3.275E-07,
     5     -3.359E-07, -3.486E-07, -3.578E-07, -3.578E-07, -3.607E-07,
     6     -3.643E-07, -3.672E-07, -3.668E-07, -3.645E-07, -3.628E-07,
     7     -3.626E-07, -3.652E-07, -3.859E-07, -4.088E-07, -4.338E-07,
     8     -4.595E-07, -4.744E-07, -4.748E-07, -4.758E-07, -4.761E-07,
     9     -4.772E-07, -4.909E-07, -5.102E-07, -5.308E-07, -5.501E-07,
     $     -5.653E-07, -5.538E-07, -5.375E-07, -5.190E-07, -5.028E-07/
      DATA (Z(I),I= 2501, 2600)/
     1     -4.997E-07, -5.220E-07, -5.441E-07, -5.685E-07, -5.907E-07,
     2     -5.939E-07, -5.815E-07, -5.711E-07, -5.615E-07, -5.499E-07,
     3     -5.515E-07, -5.545E-07, -5.589E-07, -5.625E-07, -5.621E-07,
     4     -5.540E-07, -5.438E-07, -5.328E-07, -5.212E-07, -5.144E-07,
     5     -5.105E-07, -5.065E-07, -5.034E-07, -4.994E-07, -4.926E-07,
     6     -4.837E-07, -4.761E-07, -4.672E-07, -4.611E-07, -4.521E-07,
     7     -4.445E-07, -4.370E-07, -4.294E-07, -4.233E-07, -4.186E-07,
     8     -4.159E-07, -4.126E-07, -4.093E-07, -4.018E-07, -3.901E-07,
     9     -3.776E-07, -3.659E-07, -3.542E-07, -3.515E-07, -3.558E-07,
     $     -3.600E-07, -3.643E-07, -3.651E-07, -3.546E-07, -3.379E-07,
     1     -3.204E-07, -3.058E-07, -2.933E-07, -2.947E-07, -2.975E-07,
     2     -3.010E-07, -3.052E-07, -3.066E-07, -2.933E-07, -2.793E-07,
     3     -2.667E-07, -2.563E-07, -2.499E-07, -2.561E-07, -2.603E-07,
     4     -2.686E-07, -2.762E-07, -2.712E-07, -2.599E-07, -2.459E-07,
     5     -2.360E-07, -2.240E-07, -2.218E-07, -2.260E-07, -2.294E-07,
     6     -2.328E-07, -2.370E-07, -2.342E-07, -2.306E-07, -2.292E-07,
     7     -2.250E-07, -2.243E-07, -2.236E-07, -2.216E-07, -2.216E-07,
     8     -2.210E-07, -2.225E-07, -2.246E-07, -2.290E-07, -2.318E-07,
     9     -2.354E-07, -2.376E-07, -2.313E-07, -2.245E-07, -2.198E-07,
     $     -2.129E-07, -2.102E-07, -2.160E-07, -2.210E-07, -2.253E-07/
      DATA (Z(I),I= 2601, 2700)/
     1     -2.297E-07, -2.306E-07, -2.237E-07, -2.176E-07, -2.121E-07,
     2     -2.066E-07, -2.062E-07, -2.077E-07, -2.092E-07, -2.143E-07,
     3     -2.144E-07, -2.140E-07, -2.119E-07, -2.085E-07, -2.058E-07,
     4     -2.010E-07, -1.997E-07, -1.970E-07, -1.936E-07, -1.915E-07,
     5     -1.896E-07, -1.874E-07, -1.838E-07, -1.816E-07, -1.794E-07,
     6     -1.778E-07, -1.750E-07, -1.706E-07, -1.698E-07, -1.662E-07,
     7     -1.648E-07, -1.632E-07, -1.602E-07, -1.580E-07, -1.586E-07,
     8     -1.557E-07, -1.549E-07, -1.527E-07, -1.525E-07, -1.489E-07,
     9     -1.487E-07, -1.479E-07, -1.486E-07, -1.484E-07, -1.491E-07,
     $     -1.489E-07, -1.488E-07, -1.494E-07, -1.493E-07, -1.485E-07,
     1     -1.484E-07, -1.518E-07, -1.526E-07, -1.547E-07, -1.569E-07,
     2     -1.575E-07, -1.611E-07, -1.632E-07, -1.640E-07, -1.654E-07,
     3     -1.682E-07, -1.696E-07, -1.740E-07, -1.783E-07, -1.811E-07,
     4     -1.855E-07, -1.885E-07, -1.927E-07, -1.956E-07, -1.992E-07,
     5     -2.028E-07, -2.051E-07, -2.067E-07, -2.069E-07, -2.099E-07,
     6     -2.094E-07, -2.109E-07, -2.126E-07, -2.141E-07, -2.136E-07,
     7     -2.131E-07, -2.140E-07, -2.135E-07, -2.124E-07, -2.105E-07,
     8     -2.093E-07, -2.081E-07, -2.055E-07, -2.058E-07, -2.039E-07,
     9     -2.028E-07, -2.023E-07, -1.996E-07, -1.984E-07, -1.951E-07,
     $     -1.959E-07, -1.940E-07, -1.928E-07, -1.901E-07, -1.889E-07/
      DATA (Z(I),I= 2701, 2800)/
     1     -1.870E-07, -1.843E-07, -1.823E-07, -1.782E-07, -1.755E-07,
     2     -1.728E-07, -1.702E-07, -1.647E-07, -1.634E-07, -1.593E-07,
     3     -1.566E-07, -1.518E-07, -1.491E-07, -1.464E-07, -1.436E-07,
     4     -1.423E-07, -1.381E-07, -1.368E-07, -1.334E-07, -1.299E-07,
     5     -1.287E-07, -1.258E-07, -1.232E-07, -1.203E-07, -1.163E-07,
     6     -1.157E-07, -1.129E-07, -1.088E-07, -1.081E-07, -1.047E-07,
     7     -1.019E-07, -1.006E-07, -9.780E-08, -9.514E-08, -9.455E-08,
     8     -9.330E-08, -9.128E-08, -8.923E-08, -8.864E-08, -8.879E-08,
     9     -8.536E-08, -8.615E-08, -8.414E-08, -8.288E-08, -8.226E-08,
     $     -8.305E-08, -8.305E-08, -8.244E-08, -8.183E-08, -8.198E-08,
     1     -8.478E-08, -8.417E-08, -8.356E-08, -8.435E-08, -8.370E-08,
     2     -8.370E-08, -8.432E-08, -8.575E-08, -8.651E-08, -8.651E-08,
     3     -8.510E-08, -8.648E-08, -8.648E-08, -8.788E-08, -8.928E-08,
     4     -8.785E-08, -8.645E-08, -8.770E-08, -8.627E-08, -8.627E-08,
     5     -8.627E-08, -8.685E-08, -8.685E-08, -8.609E-08, -8.606E-08,
     6     -8.462E-08, -8.257E-08, -8.394E-08, -8.330E-08, -8.186E-08,
     7     -8.265E-08, -8.186E-08, -8.122E-08, -7.978E-08, -7.913E-08,
     8     -7.910E-08, -7.641E-08, -7.517E-08, -7.453E-08, -7.326E-08,
     9     -7.060E-08, -6.998E-08, -6.732E-08, -6.886E-08, -6.541E-08,
     $     -6.417E-08, -6.353E-08, -6.230E-08, -6.308E-08, -6.246E-08/
      DATA (Z(I),I= 2801, 2900)/
     1     -6.325E-08, -6.126E-08, -6.205E-08, -6.000E-08, -6.078E-08,
     2     -5.815E-08, -5.893E-08, -5.896E-08, -5.896E-08, -5.960E-08,
     3     -5.960E-08, -5.823E-08, -6.027E-08, -6.013E-08, -6.048E-08,
     4     -6.090E-08, -6.196E-08, -6.042E-08, -6.145E-08, -6.167E-08,
     5     -5.982E-08, -6.005E-08, -6.021E-08, -6.029E-08, -5.844E-08,
     6     -6.004E-08, -5.883E-08, -5.981E-08, -5.916E-08, -5.857E-08,
     7     -5.779E-08, -5.930E-08, -5.790E-08, -5.801E-08, -5.863E-08,
     8     -5.798E-08, -5.719E-08, -5.717E-08, -5.717E-08, -5.714E-08,
     9     -5.653E-08, -5.933E-08, -5.872E-08, -5.808E-08, -5.948E-08,
     $     -5.887E-08, -5.965E-08, -5.963E-08, -5.901E-08, -6.121E-08,
     1     -6.056E-08, -6.135E-08, -5.991E-08, -6.146E-08, -6.146E-08,
     2     -6.222E-08, -6.300E-08, -6.376E-08, -6.315E-08, -6.455E-08,
     3     -6.530E-08, -6.465E-08, -6.544E-08, -6.339E-08, -6.414E-08,
     4     -6.493E-08, -6.367E-08, -6.506E-08, -6.377E-08, -6.377E-08,
     5     -6.315E-08, -6.390E-08, -6.283E-08, -6.306E-08, -6.258E-08,
     6     -6.160E-08, -6.109E-08, -6.061E-08, -6.010E-08, -5.830E-08,
     7     -5.820E-08, -5.667E-08, -5.671E-08, -5.521E-08, -5.468E-08,
     8     -5.347E-08, -5.241E-08, -5.126E-08, -5.032E-08, -4.923E-08,
     9     -4.822E-08, -4.715E-08, -4.619E-08, -4.512E-08, -4.411E-08,
     $     -4.294E-08, -4.198E-08, -4.082E-08, -3.972E-08, -3.848E-08/
      DATA (Z(I),I= 2901, 3000)/
     1     -3.765E-08, -3.656E-08, -3.531E-08, -3.421E-08, -3.319E-08,
     2     -3.209E-08, -3.131E-08, -3.082E-08, -3.010E-08, -2.947E-08,
     3     -2.890E-08, -2.833E-08, -2.776E-08, -2.718E-08, -2.669E-08,
     4     -2.626E-08, -2.569E-08, -2.517E-08, -2.505E-08, -2.494E-08,
     5     -2.504E-08, -2.485E-08, -2.481E-08, -2.454E-08, -2.444E-08,
     6     -2.453E-08, -2.435E-08, -2.416E-08, -2.425E-08, -2.419E-08,
     7     -2.441E-08, -2.454E-08, -2.448E-08, -2.470E-08, -2.463E-08,
     8     -2.491E-08, -2.499E-08, -2.485E-08, -2.506E-08, -2.514E-08,
     9     -2.521E-08, -2.494E-08, -2.501E-08, -2.488E-08, -2.488E-08,
     $     -2.460E-08, -2.475E-08, -2.468E-08, -2.470E-08, -2.462E-08,
     1     -2.442E-08, -2.429E-08, -2.417E-08, -2.398E-08, -2.373E-08,
     2     -2.353E-08, -2.314E-08, -2.295E-08, -2.276E-08, -2.264E-08,
     3     -2.245E-08, -2.212E-08, -2.200E-08, -2.163E-08, -2.146E-08,
     4     -2.114E-08, -2.091E-08, -2.081E-08, -2.064E-08, -2.047E-08,
     5     -2.009E-08, -1.999E-08, -1.982E-08, -1.959E-08, -1.907E-08,
     6     -1.885E-08, -1.841E-08, -1.804E-08, -1.760E-08, -1.724E-08,
     7     -1.674E-08, -1.630E-08, -1.579E-08, -1.564E-08, -1.513E-08,
     8     -1.483E-08, -1.431E-08, -1.401E-08, -1.350E-08, -1.320E-08,
     9     -1.289E-08, -1.238E-08, -1.228E-08, -1.190E-08, -1.146E-08,
     $     -1.116E-08, -1.064E-08, -1.033E-08, -1.040E-08, -1.028E-08/
      DATA (Z(I),I= 3001, 3080)/
     1     -1.042E-08, -1.036E-08, -1.023E-08, -1.037E-08, -1.045E-08,
     2     -1.025E-08, -1.033E-08, -1.026E-08, -1.040E-08, -1.054E-08,
     3     -1.080E-08, -1.111E-08, -1.150E-08, -1.190E-08, -1.207E-08,
     4     -1.260E-08, -1.272E-08, -1.317E-08, -1.342E-08, -1.368E-08,
     5     -1.399E-08, -1.426E-08, -1.466E-08, -1.493E-08, -1.533E-08,
     6     -1.568E-08, -1.586E-08, -1.621E-08, -1.654E-08, -1.688E-08,
     7     -1.714E-08, -1.741E-08, -1.775E-08, -1.805E-08, -1.794E-08,
     8     -1.819E-08, -1.814E-08, -1.810E-08, -1.848E-08, -1.844E-08,
     9     -1.868E-08, -1.864E-08, -1.874E-08, -1.885E-08, -1.887E-08,
     $     -1.864E-08, -1.835E-08, -1.819E-08, -1.789E-08, -1.760E-08,
     1     -1.722E-08, -1.706E-08, -1.663E-08, -1.639E-08, -1.610E-08,
     2     -1.580E-08, -1.536E-08, -1.480E-08, -1.459E-08, -1.403E-08,
     3     -1.346E-08, -1.304E-08, -1.248E-08, -1.199E-08, -1.157E-08,
     4     -1.115E-08, -1.045E-08, -1.002E-08, -9.538E-09, -8.929E-09,
     5     -8.535E-09, -8.001E-09, -7.532E-09, -6.920E-09, -6.422E-09,
     6     -6.094E-09, -5.472E-09, -4.954E-09, -4.367E-09, -3.874E-09/
c
      if (v .lt. vbeg .or. v .gt. vend) then
         c0=0.0
         ct1=0.0
         ct2=0.0
      else if (v .eq. vend) then
c        This check prevents n from exceeding nmax in the algorithm below.
         c0=x(nmax)
         ct1=y(nmax)
         ct2=z(nmax)
      else
         xi = (v-vbeg)/vincr+1.
         n = xi+1.+1.E-6
         xd = xi-n
         c0 = x(n)+xd*(x(n)-x(n-1))
         ct1 = y(n)+xd*(y(n)-y(n-1))
         ct2 = z(n)+xd*(z(n)-z(n-1))
      endif   
      return
      end   
      SUBROUTINE O2CONT(V,SIGMA,ALPHA,BETA)                             o2ct 100
C                                                                       o2ct 110
C     THIS ROUTINE IS DRIVEN BY FREQUENCY, RETURNING ONLY THE           o2ct 120
C     O2 COEFFICIENTS, INDEPENDENT OF TEMPERATURE.                      o2ct 130
C                                                                       o2ct 140
C  *******************************************************************  o2ct 150
C  *  THESE COMMENTS APPLY TO THE COLUME ARRAYS FOR:                 *  o2ct 160
C  *       PBAR*UBAR(O2)                                             *  o2ct 170
C  *       PBAR*UBAR(O2)*DT                                          *  o2ct 180
C  *   AND PBAR*UBAR(O2)*DT*DT    WHERE:  DT=TBAR-220.               *  o2ct 190
C  *  THAT HAVE BEEN COMPILED IN OTHER PARTS OF THE LOWTRAN CODE     *  o2ct 200
C  *                                                                 *  o2ct 210
C  *  LOWTRAN7 COMPATIBLE:                                           *  o2ct 220
C  *  O2 CONTINUUM SUBROUTINE FOR 1395-1760CM-1                      *  o2ct 230
C  *  MODIFIED BY G.P. ANDERSON, APRIL '88                           *  o2ct 240
C  *                                                                 *  o2ct 250
C  *  THE EXPONENTIAL TEMPERATURE EMPLOYED IN THE FASCOD2 ALGORITHM  *  o2ct 260
C  *  (SEE BELOW) IS NOT READILY SUITABLE FOR LOWTRAN.  THEREFORE    *  o2ct 270
C  *  THE EXPONENTIALS HAVE BEEN LINEARLY EXPANDED, KEEPING ONLY THE *  o2ct 280
C  *  LINEAR AND QUADRATIC TERMS:                                    *  o2ct 290
C  *                                                                 *  o2ct 300
C  *  EXP(A*DT)=1.+ A*DT + (A*DT)**2/2. + ....                       *  o2ct 310
C  *                                                                 *  o2ct 320
C  *     EXP(B*DT*DT)=1.+ B*DT*DT + (B*DT*DT)**2/2. + ....           *  o2ct 330
C  *                                                                 *  o2ct 340
C  *  THE PRODUCT OF THE TWO TERMS IS:                               *  o2ct 350
C  *                                                                 *  o2ct 360
C  *     (1. + A*DT + (A*A/2. + B)*DT*DT )                           *  o2ct 370
C  *                                                                 *  o2ct 380
C  *  THIS EXPANSION ONLY WORKS WELL FOR SMALL VALUES OF X IN EXP(X) *  o2ct 390
C  *                                                                 *  o2ct 400
C  *  SINCE DT = T-220., THE APPROXIMATION IS VERY GOOD UNTIL        *  o2ct 410
C  *  T.GT.260. OR DT.GT.40.   AT T=280, THE MAXIMUM ERRORS ARE STILL*  o2ct 420
C  *  LESS THAN 10% BUT AT T=300, THOSE ERRORS ARE AS LARGE AS 20%   *  o2ct 430
C  *******************************************************************  o2ct 440
C                                                                       o2ct 450
C     THE FOLLOWING COMMENTS ARE EXCERPTED DIRECTLY FROM FASCOD2        o2ct 460
C                                                                       o2ct 470
C      THIS SUBROUTINE CONTAINS THE ROGERS AND WALSHAW                  o2ct 480
C      EQUIVALENT COEFFICIENTS DERIVED FROM THE THEORETICAL             o2ct 490
C      VALUES SUPPLIED BY ROLAND DRAYSON. THESE VALUES USE              o2ct 500
C      THE SAME DATA AS TIMOFEYEV AND AGREE WITH TIMOFEYEV'S RESULTS.   o2ct 510
C      THE DATA ARE IN THE FORM OF STRENGTHS(O2SO) AND TWO              o2ct 520
C      COEFFICIENTS (O2A & O2B),  WHICH ARE USED TO CORRECT FOR         o2ct 530
C      TEMPERATURE. THE DEPENDENCY ON PRESSURE SQUARED                  o2ct 540
C      IS CONTAINED IN THE P*WO2 PART OF THE CONSTANT.                  o2ct 550
C      NOTE THAT SINCE THE COEFFICIENTS ARE FOR AIR, THE                o2ct 560
C      THE STRENGTHS ARE DIVIDED BY THE O2 MIXING RATIO FOR             o2ct 570
C      DRY AIR OF 0.20946 (THIS IS ASSUMED CONSTANT).                   o2ct 580
C      ORIGINAL FORMULATION OF THE COEFFICIENTS WAS BY LARRY GORDLEY.   o2ct 590
C      THIS VERSION WRITTEN BY EARL THOMPSON, JULY 1984.                o2ct 600
C                                                                       o2ct 610
C                                                                       o2ct 620
      COMMON/O2C/ O2DRAY(74),O2C001(74),O2S0(74),O2A(74),O2B(74),       o2ct 630
     X V1O2,V2O2,DVO2,NPTO2                                             o2ct 640
      SIGMA =0                                                          o2ct 650
      ALPHA =0                                                          o2ct 660
      BETA  =0                                                          o2ct 670
      IF(V .LT. 1395) GO TO 30                                          o2ct 680
      IF(V .GT. 1760) GO TO 30                                          o2ct 690
C                                                                       o2ct 700
C                                                                       o2ct 710
      CALL O2INT(V,V1O2,DVO2,NPTO2,C,O2S0,A,O2A,B,O2B)                  o2ct 720
C                                                                       o2ct 730
C                                                                       o2ct 740
C                                                                       o2ct 750
C     OLD 'FASCOD2' TEMPERATURE DEPENDENCE USING BLOCK DATA ARRAYS      o2ct 760
C                                                                       o2ct 770
C     C(J)=O2S0(I)* EXP(O2A(I)*TD+O2B(I)*TD*TD) /(0.20946*VJ)           o2ct 780
C                                                                       o2ct 790
C     NEW COEFFICIENT DEFINITIONS FOR LOWTRAN FORMULATION               o2ct 800
C                                                                       o2ct 810
cjv 12/95    modtran correction as per Risland reference. Sigma is now
cjv	 multipled by o2fac following fascod3p giving a 20% change in 
cjv	the O2 continuum.
c
C THE COEFFICIENTS FOR O2 HAVE BEEN MULTIPLIED BY A FACTOR OF 0.78
C     RINSLAND ET AL, 1989: JGR 94; 16,303 - 16,322.
C
      O2FAC = 0.78
C
cjv	The O2 continuum equation is in stdmdl.f line 1740, subroutine lay5dt.
cjv      TX(3)=W(58)*ABB(17)+
cjv    1  SIGO20*(W(63)+SIGO2A*(W(1)-220.*W(63))+SIGO2B*W(2))
cjv
cjv   where: sigma = sigo20,  beta = sigo2b,   alpha = sigo2b 
c
      ALPHA= A                                                          o2ct 820
      BETA=A**2/2.+B                                                    o2ct 830
      SIGMA=C/0.20946                                                    o2ct 840
cjv
cjv   modtran correction as per Risland reference above.
      SIGMA=sigma*o2fac                                                   o2ct 840
cjv  ^
C                                                                       o2ct 850
C     NEW 'LOWTRAN7' TEMPERATURE DEPENDENCE                             o2ct 860
C                                                                       o2ct 870
C     THIS WOULD BE THE CODING FOR THE LOWTRAN7 FORMULATION, BUT        o2ct 880
C       BECAUSE THE T-DEPENDENCE IS INCLUDED IN THE AMOUNTS, ONLY       o2ct 890
C       THE COEFFICIENTS (SIGMA, ALPHA & BETA) ARE BEING RETURNED       o2ct 900
C                                                                       o2ct 910
C     C(J)=SIGMA*(1.+ALPHA*TD+BETA*TD*TD)                               o2ct 920
C                                                                       o2ct 930
30    RETURN                                                            o2ct 940
      END                                                               o2ct 950
      SUBROUTINE O2INT(V1C,V1,DV,NPT,C,CARRAY,A,AARRAY,B,BARRAY)        o2in 100
C                                                                       o2in 110
C     INTERPOLATION FOR O2 PRESSURE INDUCED CONTINUUM, NECESSARY FOR    o2in 120
C          LOWTRAN7 FORMULATION  (MODELED AFTER THE LOWTRAN UV-O3 BANDS)o2in 130
C                                                                       o2in 140
      DIMENSION CARRAY(74),AARRAY(74),BARRAY(74)                        o2in 150
      C=0.                                                              o2in 160
      A=0.                                                              o2in 170
      B=0.                                                              o2in 180
      I=(V1C-V1)/DV+1.00001                                             o2in 190
      IF(I.LT.1  )GO TO 10                                              o2in 200
      IF(I.GT.NPT)GO TO 10                                              o2in 210
      C=CARRAY(I)                                                       o2in 220
      A=AARRAY(I)                                                       o2in 230
      B=BARRAY(I)                                                       o2in 240
10    CONTINUE                                                          o2in 250
      RETURN                                                            o2in 260
      END                                                               o2in 270
      SUBROUTINE O3HHT0(V,C)                                            o3t0 100
      COMMON /O3HH0/ V1S,V2S,DVS,NPTS,S(2687)                           o3t0 110
C                                                                       o3t0 120
      CALL O3INT(V ,V1S,DVS,NPTS,S,C)                                   o3t0 130
      RETURN                                                            o3t0 140
      END                                                               o3t0 150
      SUBROUTINE O3HHT1(V,C)                                            o3t1 100
C     SUBROUTINE O3HHT1(V1C,V2C,DVC,NPTC,C)                             o3t1 110
      COMMON /O3HH1/ V1S,V2S,DVS,NPTS,S(2690)                           o3t1 120
C                                                                       o3t1 130
      CALL O3INT(V ,V1S,DVS,NPTS,S,C)                                   o3t1 140
C                                                                       o3t1 150
      RETURN                                                            o3t1 160
      END                                                               o3t1 170
      SUBROUTINE O3HHT2(V,C)                                            o3t2 100
      COMMON /O3HH2/ V1S,V2S,DVS,NPTS,S(2690)                           o3t2 110
C                                                                       o3t2 120
      CALL O3INT(V ,V1S,DVS,NPTS,S,C)                                   o3t2 130
C                                                                       o3t2 140
      RETURN                                                            o3t2 150
      END                                                               o3t2 160
      SUBROUTINE O3INT(V1C,V1,DV,NPT,CONTI,CONTO)                       o3it 100
C                                                                       o3it 110
C     INTERPOLATION  FOR  O3 CONTINUUM WITH LOWTRAN                     o3it 120
C                                                                       o3it 130
      DIMENSION CONTI(2687)                                             o3it 140
      CONTO=0.                                                          o3it 150
      I=(V1C-V1)/DV+1.00001                                             o3it 160
      IF(I.LT.1  )GO TO 10                                              o3it 170
      IF(I.GT.NPT)GO TO 10                                              o3it 180
      CONTO=CONTI(I)                                                    o3it 190
10    CONTINUE                                                          o3it 200
      RETURN                                                            o3it 210
      END                                                               o3it 220
      SUBROUTINE O3UV(V,C)                                              o3uv 100
      COMMON /O3UVF/ V1 ,V2 ,DV ,NPT ,S(133)                            o3uv 110
C                                                                       o3uv 120
C     INTERPOLATION  FOR  O3 CONTINUUM WITH LOWTRAN                     o3uv 130
C                                                                       o3uv 140
      C    =0.                                                          o3uv 150
      I=(V  -V1)/DV+1.00001                                             o3uv 160
      IF(I.LT.1   )GO TO 10                                             o3uv 170
      IF(I.GT.NPT )GO TO 10                                             o3uv 180
      VR = (I-1)*DV + V1                                                o3uv 190
      IF(VR. LE. (V+.1) .AND .VR.GE. (V-.1)) GO TO 5                    o3uv 200
      IF(I .EQ. NPT ) I=NPT-1                                           o3uv 210
      AM = (S(I+1) -S(I))/DV                                            o3uv 220
      C0 = S(I) - AM * VR                                               o3uv 230
      C  = AM * V + C0                                                  o3uv 240
      GO TO 10                                                          o3uv 250
5     C    =    S(I)                                                    o3uv 260
10    CONTINUE                                                          o3uv 270
C                                                                       o3uv 280
      RETURN                                                            o3uv 290
      END                                                               o3uv 300
      BLOCK DATA O3UVFD                                                 o3fd 100
C>    BLOCK DATA                                                        o3fd 110
      COMMON /O3UVF / V1O1,V2O1,DVO1,NPT1,C02281(80),C02361(53)         o3fd 120
C                                                                       o3fd 130
C        OZONE UV  VISIBLE ABSORPTION COEFFICIENTS                      o3fd 140
C                     (CM-ATM)-1                                        o3fd 150
C     DATA DERIVED FROM MOLINA & MOLINA, JGR,91,14501-14508,1986.       o3fd 160
C     VALUES BETWEEN 245 AND 185NM (40800 AND 54054CM-1) USED AS        o3fd 170
C     DIRECT AVERAGE WITH NO TEMPERATURE DEPENDENCE.                    o3fd 180
C                                                                       o3fd 190
C     O3 LOCATION  1    V =  40800  CM-1                                o3fd 200
C     O3 LOCATION  133  V =  54054  CM-1                                o3fd 210
C        DV = 100  CM-1                                                 o3fd 220
C                                                                       o3fd 230
      DATA V1O1,V2O1,DVO1,NPT1/ 40800.,54000.,100.,133/                 o3fd 240
      DATA C02281/                                                      o3fd 250
     C 9.91204E+02, 9.76325E+02, 9.72050E+02, 9.51049E+02, 9.23530E+02, o3fd 260
     C 9.02306E+02, 8.90510E+02, 8.60115E+02, 8.39094E+02, 8.27926E+02, o3fd 270
     C 7.95525E+02, 7.73583E+02, 7.55018E+02, 7.31076E+02, 7.10415E+02, o3fd 280
     C 6.87747E+02, 6.66639E+02, 6.39484E+02, 6.27101E+02, 6.01019E+02, o3fd 290
     C 5.77594E+02, 5.60403E+02, 5.40837E+02, 5.21289E+02, 4.99329E+02, o3fd 300
     C 4.81742E+02, 4.61608E+02, 4.45707E+02, 4.28261E+02, 4.09672E+02, o3fd 310
     C 3.93701E+02, 3.77835E+02, 3.61440E+02, 3.45194E+02, 3.30219E+02, o3fd 320
     C 3.15347E+02, 3.01164E+02, 2.87788E+02, 2.74224E+02, 2.61339E+02, o3fd 330
     C 2.48868E+02, 2.36872E+02, 2.25747E+02, 2.14782E+02, 2.03997E+02, o3fd 340
     C 1.94281E+02, 1.84525E+02, 1.75275E+02, 1.67151E+02, 1.58813E+02, o3fd 350
     C 1.50725E+02, 1.43019E+02, 1.35825E+02, 1.28878E+02, 1.22084E+02, o3fd 360
     C 1.15515E+02, 1.09465E+02, 1.03841E+02, 9.83780E+01, 9.31932E+01, o3fd 370
     C 8.83466E+01, 8.38631E+01, 7.96631E+01, 7.54331E+01, 7.13805E+01, o3fd 380
     C 6.78474E+01, 6.44340E+01, 6.13104E+01, 5.81777E+01, 5.53766E+01, o3fd 390
     C 5.27036E+01, 5.03555E+01, 4.82633E+01, 4.61483E+01, 4.42014E+01, o3fd 400
     C 4.23517E+01, 4.07774E+01, 3.93060E+01, 3.80135E+01, 3.66348E+01/ o3fd 410
      DATA C02361/                                                      o3fd 420
     C 3.53665E+01, 3.47884E+01, 3.39690E+01, 3.34288E+01, 3.29135E+01, o3fd 430
     C 3.23104E+01, 3.18875E+01, 3.16800E+01, 3.15925E+01, 3.12932E+01, o3fd 440
     C 3.12956E+01, 3.15522E+01, 3.14950E+01, 3.15924E+01, 3.19059E+01, o3fd 450
     C 3.23109E+01, 3.27873E+01, 3.33788E+01, 3.39804E+01, 3.44925E+01, o3fd 460
     C 3.50502E+01, 3.55853E+01, 3.59416E+01, 3.68933E+01, 3.78284E+01, o3fd 470
     C 3.86413E+01, 3.98049E+01, 4.04700E+01, 4.12958E+01, 4.23482E+01, o3fd 480
     C 4.31203E+01, 4.41885E+01, 4.52651E+01, 4.61492E+01, 4.70493E+01, o3fd 490
     C 4.80497E+01, 4.90242E+01, 4.99652E+01, 5.10316E+01, 5.21510E+01, o3fd 500
     C 5.32130E+01, 5.43073E+01, 5.56207E+01, 5.61756E+01, 5.66799E+01, o3fd 510
     C 5.85545E+01, 5.92409E+01, 5.96168E+01, 6.12497E+01, 6.20231E+01, o3fd 520
     C 6.24621E+01, 6.34160E+01, 6.43622E+01/                           o3fd 530
      END                                                               o3fd 540
      FUNCTION   PF(NN,I,J)                                             pf   100
C     CALL THE APPROPRIATE PHASE FUNCTION                               pf   110
      COMMON/MNMPHS/ MNUM(27,26),PHSFNC(34,70)                          pf   120
      M=MNUM(I,NN)                                                      pf   130
      PF=PHSFNC(J,M)                                                    pf   140
      RETURN                                                            pf   150
      END                                                               pf   160
      SUBROUTINE PHASEF(V,ALT,SANGLE,RH,PHFA)                           phaf 100
      include 'parameter.list'
c                                                                       phaf 110
c     this routine is a bit different from and replaces                 phaf 120
c     the old phasef                                                    phaf 130
C                                                                       phaf 140
C     RETURNS THE AEROSOL PHASE FUNCTION FROM THE STORED DATA BASE      phaf 150
C                                                                       phaf 160
C     THE TRUTH TABLE MNUM(27,26) STORED IN COMMON/MNMPHS/              phaf 170
C     IN SUBROUTINE PHSDTA IS QUERIED TO DETERMINE THE PROPER PHASE     phaf 180
C     FUNCTION NEEDED.                                                  phaf 190
C     THE 27 POSITIONS REPRESENT THE 27 SPECIFIC FREQUENCIES SHOWN IN   phaf 200
C     DATA STATEMENT WAVE  .2-40 MICRONS.                               phaf 210
C     THE NUMBERS STORED IN THESE 27 POSITIONS REPRESENT THE CORRECT    phaf 220
C     PHASE FUNCTIONS CHOSEN FROM THE DATA STATEMENT PHSFNC'S 1-70      phaf 230
C     POSSIBLE CHOICES.                                                 phaf 240
C     THE 26 DATA STATEMENTS EACH HAVING 27 FREQUENCIES REPRESENT THE   phaf 250
C     FOLLOWING 26 MODELS;                                              phaf 260
C      1=RURAL     0%RH   2=RURAL    70%RH   3=RURAL    80%RH           phaf 270
C      4=RURAL    99%RH   5=MARITIME  0%RH   6=MARITIME 70%RH           phaf 280
C      7=MARITIME 80%RH   8=MARITIME 99%RH   9=URBAN     0%RH           phaf 290
C     10=URBAN    70%RH  11=URBAN    80%RH  12=URBAN    99%RH           phaf 300
C     13=OCEANIC   0%RH  14=OCEANIC  70%RH  15=OCEANIC  80%RH           phaf 310
C     16=OCEANIC  99%RH  17=TROPOSPH  0%RH  18=TROPOSPH 70%RH           phaf 320
C     19=TROPOSPH 80%RH  20=TROPOSPH 99%RH  21=STRATOSPHERIC            phaf 330
C     22=AGED VOLCANIC   23=FRESH VOLCANIC  24=RADIATION FOG            phaf 340
C     25=ADVECTIVE FOG   26=METEORIC DUST                               phaf 350
C                                                                       phaf 360
C     IN THE PRESENT VERSION THE 4 OCEANIC MODELS 13-16                 phaf 370
C     ARE NOT UTILIZED.                                                 phaf 380
      COMMON/CNTRL/KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT    
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON/CNSTNS/PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                       phaf 400
      COMMON /CARD1/ MODEL,ITYPE,IEMSCT,M1,M2,M3,IM,NOPRNT,TBOUND,SALB  phaf 410
     1  ,MODTRN                                                         phaf 420
      LOGICAL MODTRN                                                    phaf 430
      COMMON/CARD2/IHAZE,ISEASN,IVULCN,ICSTL,ICLD,IVSA,VIS,WSS,WHH,     phaf 440
     1  RAINRT                                                          phaf 450
      COMMON/CARD2D/IREG(4),ALTB(4),IREGC(4)                            phaf 460
      COMMON/MNMPHS/MNUM(27,26),PHSFNC(34,70)                           phaf 470
      COMMON /MODEL/ ZM(LAYDIM),PM(LAYDIM),TM(LAYDIM),RFNDX(LAYDIM),
     1  DENSTY(65,LAYDIM),CLDAMT(LAYDIM),RRAMT(LAYDIM),EQLWC(LAYDIM),
     1  HAZEC(LAYDIM)
      COMMON/AER/XX1,XX2,XX3,XX4,XX5,                                   phaf 500
     1  YY1,YY2,YY3,YY4,YY5,ZZ1,ZZ2,ZZ3,ZZ4,ZZ5                         phaf 510
      dimension  RHPTS(4),WAVE(27),ANG(34)                              phaf 520
      DATA ANG/0.,2.,4.,6.,8.,10.,12.,16.,20.,24.,28.,32.,36.,40.,      phaf 530
     1  50.,60.,70.,80.,90.,100.,110.,120.,125.,130.,135.,140.,145.,    phaf 540
     2  150.,155.,160.,165.,170.,175.,180./                             phaf 550
      DATA WAVE/.2,.3,.55,.6943,1.06,1.536,2.0,2.5,2.7,3.,3.2,3.39,5.,  phaf 560
     1  6.,7.2,7.9,8.7,9.2,10.0,10.59,12.5,15.0,17.2,18.5,21.3,30.,40./ phaf 570
      DATA RHPTS /0.,70.,80.,99./                                       phaf 580
      PHFA=0.                                                           phaf 590
      ALAM=1.E4/(V+1.e-5)                                               phaf 600
      IF(SANGLE.LT.0. .OR. SANGLE.GT.180.)GOTO900                       phaf 610
cc    IF(ALAM-WAVE(1).LT.-1.E-6)GOTO910                                 phaf 620
      IF(ALAM.GT.WAVE(27))THEN                                          phaf 630
          COSANG=COS(CA*SANGLE)                                         phaf 640
          CALL HENGNS(ZZ1,COSANG,PHFA)                                  phaf 650
          RETURN                                                        phaf 660
      ENDIF                                                             phaf 670
C                                                                       phaf 680
C     DETERMINE THE AEROSOL MODEL NUMBER                                phaf 690
      IF(ALT.GT.ALTB(1))GOTO95                                          phaf 700
      IF(IHAZE.EQ.0)GOTO400                                             phaf 710
      IF(IHAZE .EQ.10) THEN                                             phaf 720
           COSANG=COS(CA*SANGLE)                                        phaf 730
           CALL HENGNS(ZZ1,COSANG,PHFA)                                 phaf 740
           RETURN                                                       phaf 750
      ENDIF                                                             phaf 760
C                                                                       phaf 770
C     CHECK IF CLOUD,RAIN OR DESERT MODEL IS REQUESTED                  phaf 780
      IF(ICLD.GT.0 .OR. IHAZE.EQ.7)THEN                                 phaf 790
           COSANG=COS(CA*SANGLE)                                        phaf 800
           CALL HENGNS(ZZ1,COSANG,PHFA)                                 phaf 810
           RETURN                                                       phaf 820
      ENDIF                                                             phaf 830
      IF(IHAZE.GE.8)goto90                                              phaf 840
C                                                                       phaf 850
C     0-2KM BOUNDARY LAYER MODELS, RH DEPENDENT                         phaf 860
      DO 50 I1=1,4                                                      phaf 870
          I=I1                                                          phaf 880
          IF(RHPTS(I).EQ.RH) goto70                                     phaf 890
          IF(RHPTS(I).GT.RH) goto60                                     phaf 900
50    CONTINUE                                                          phaf 910
60    IRHLO=I-1                                                         phaf 920
      IRHHI=I                                                           phaf 930
      goto80                                                            phaf 940
70    IRHLO=I                                                           phaf 950
      IRHHI=I                                                           phaf 960
80    CONTINUE                                                          phaf 970
C                                                                       phaf 980
C     RURAL MODEL                                                       phaf 990
      IF(IHAZE.EQ.1 .OR. IHAZE.EQ.2)NN0=0                               phaf1000
C                                                                       phaf1010
C     MARITIME MODEL                                                    phaf1020
      IF(IHAZE.EQ.3 .OR. IHAZE.EQ.4)NN0=4                               phaf1030
C                                                                       phaf1040
C     URBAN MODEL                                                       phaf1050
      IF(IHAZE.EQ.5)NN0=8                                               phaf1060
C                                                                       phaf1070
C     TROPOSPHERIC MODEL                                                phaf1080
      IF(IHAZE.EQ.6)NN0=16                                              phaf1090
      NN=NN0+IRHLO                                                      phaf1100
      goto130                                                           phaf1110
C     0-2KM FOG MODELS, NO RH DEPENDENCE                                phaf1120
90    IF(IHAZE.EQ.8)NN=24                                               phaf1130
      IF(IHAZE.EQ.9)NN=25                                               phaf1140
      goto130                                                           phaf1150
95    IF(ALT.GT.ALTB(2)) goto110                                        phaf1160
C     2-10KM TROPOSPHERIC MODEL                                         phaf1170
       IF(ICLD .GT. 0.  OR. IHAZE .EQ. 7 )  THEN                        phaf1180
           COSANG=COS(CA*SANGLE)                                        phaf1190
C                                                                       phaf1200
           CALL HENGNS(ZZ2,COSANG,PHFA)                                 phaf1210
              RETURN                                                    phaf1220
        ENDIF                                                           phaf1230
      NN=18                                                             phaf1240
      goto130                                                           phaf1250
110   IF(ALT.GT.ALTB(3)) goto120                                        phaf1260
C     10-30KM STRATOSPHERIC MODELS                                      phaf1270
C     BACKGROUND MODEL                                                  phaf1280
      IF(IVULCN.EQ.0.OR.IVULCN.EQ.1) NN=21                              phaf1290
C     AGED VOLCANIC MODEL                                               phaf1300
      IF(IVULCN.EQ.2.OR.IVULCN.EQ.4) NN=22                              phaf1310
C     FRESH VOLCANIC                                                    phaf1320
      IF(IVULCN.EQ.3.OR.IVULCN.EQ.5 .OR.IVULCN.EQ.8) NN=23              phaf1330
C     BACKGROUND STRATO                                                 phaf1340
      IF(IVULCN.EQ.6.OR.IVULCN.EQ.7) NN=21                              phaf1350
      goto130                                                           phaf1360
C     30-100KM METEORIC MODEL                                           phaf1370
120   NN=26                                                             phaf1380
130   IRH=0                                                             phaf1390
C                                                                       phaf1400
C     DETERMINE THE BOUNDING ANGLE INDICES                              phaf1410
140   DO 210 I1=1,ml                                                    phaf1420
      I=I1                                                              phaf1430
      IF(ANG(I).EQ.SANGLE) goto230                                      phaf1440
      IF(ANG(I).GT.SANGLE) goto220                                      phaf1450
210   CONTINUE                                                          phaf1460
220   IANG1=I-1                                                         phaf1470
      IANG2=I                                                           phaf1480
      goto240                                                           phaf1490
230   IANG1=I                                                           phaf1500
      IANG2=I                                                           phaf1510
240   CONTINUE                                                          phaf1520
C                                                                       phaf1530
C     DETERMINE THE BOUNDING WAVELENGTH INDICES                         phaf1540
      DO 250 I1=1,27                                                    phaf1550
      I=I1                                                              phaf1560
      IF(WAVE(I).EQ.ALAM) goto270                                       phaf1570
      IF(WAVE(I).GT.ALAM) goto260                                       phaf1580
250   CONTINUE                                                          phaf1590
260   IWAV1=I-1                                                         phaf1600
      IWAV2=I                                                           phaf1610
      if(iwav1.lt.1) then 
          iwav1=1
      endif
      goto280                                                           phaf1620
270   IWAV1=I                                                           phaf1630
      IWAV2=I                                                           phaf1640
280   CONTINUE                                                          phaf1650
C                                                                       phaf1660
C     FUNCTION PF CHOOSES DESIRED PHASE FUNCTION FROM LOOK UP TABLE     phaf1670
C     MNUM(IWAV,NN)  WHERE IWAV IS FREQ. AND NN IS MODEL NO.            phaf1680
C                                                                       phaf1690
C     WAVELENGTH INTERPOLATION ONLY USES PF11 AND PF21                  phaf1700
C     ANGLE INTERPOLATION ONLY USES PF11 AND PF12                       phaf1710
C     WAVELENGTH AND ANGLE INTERPOLATION USES PF11,PF21 AND PF12,PF22.  phaf1720
C                                                                       phaf1730
      PF11=PF(NN,IWAV1,IANG1)                                           phaf1740
      PF21=PF(NN,IWAV2,IANG1)                                           phaf1750
      PF12=PF(NN,IWAV1,IANG2)                                           phaf1760
      PF22=PF(NN,IWAV2,IANG2)                                           phaf1770
C     INTERPOLATE IN WAVELENGTH THEN ANGLE                              phaf1780
      IF(IWAV1.EQ.IWAV2) goto310                                        phaf1790
      IF(IANG1.EQ.IANG2) goto290                                        phaf1800
C     BOTH INTERPOLATIONS ARE NECESSARY                                 phaf1810
      CALL INTERP(2,ALAM,WAVE(IWAV1),WAVE(IWAV2),YANG1,                 phaf1820
     1PF11,PF21)                                                        phaf1830
      CALL INTERP(2,ALAM,WAVE(IWAV1),WAVE(IWAV2),YANG2,                 phaf1840
     1PF12,PF22)                                                        phaf1850
      CALL INTERP(2,SANGLE,ANG(IANG1),ANG(IANG2),Y,YANG1,YANG2)         phaf1860
      goto330                                                           phaf1870
C     ONLY WAVELENGTH INTERPOLATION IS NECESSARY                        phaf1880
290   CALL INTERP(2,ALAM,WAVE(IWAV1),WAVE(IWAV2),Y,PF11,                phaf1890
     1PF21)                                                             phaf1900
      goto330                                                           phaf1910
310   IF(IANG1.EQ.IANG2) goto320                                        phaf1920
C     ONLY ANGLE INTERPOLATION IS NECESSARY                             phaf1930
      CALL INTERP(2,SANGLE,ANG(IANG1),ANG(IANG2),Y,PF11,                phaf1940
     1PF12)                                                             phaf1950
      goto330                                                           phaf1960
C     NO INTERPOLATION IS NECESSARY                                     phaf1970
320   Y=PF(NN,IWAV1,IANG1)                                              phaf1980
330   CONTINUE                                                          phaf1990
      PHFA=Y                                                            phaf2000
C                                                                       phaf2010
C     HUMIDITY DEPENDENCE                                               phaf2020
      IF(ALT.GT.ALTB(1).OR.NN.GE.17.OR.IRHLO.EQ.IRHHI) goto400          phaf2030
      IF(IRH.EQ.1) goto340                                              phaf2040
      NN=NN0+IRHHI                                                      phaf2050
      PHFA1=PHFA                                                        phaf2060
      IRH=1                                                             phaf2070
      goto280                                                           phaf2080
340   CONTINUE                                                          phaf2090
      PHFA2=PHFA                                                        phaf2100
      CALL INTERP(1,RH,RHPTS(IRHLO),RHPTS(IRHHI),                       phaf2110
     CPHFA,PHFA1,PHFA2)                                                 phaf2120
400   CONTINUE                                                          phaf2130
      RETURN                                                            phaf2140
  900 WRITE(IPR,901) SANGLE                                             phaf2150
  901 FORMAT('0FROM PHASEF- SCATTERING ANGLE IS OUT OF RANGE, '         phaf2160
     1    ,'ANGLE = ',E12.5)                                            phaf2170
      STOP                                                              phaf2180
  910 WRITE(IPR,911) ALAM                                               phaf2190
  911 FORMAT('0FROM PHASEF- ALAM IS OUT OF RANGE, ALAM = ',E12.5)       phaf2200
      STOP                                                              phaf2210
      END                                                               phaf2220
      BLOCK DATA PHSDTA                                                 phsd 100
C>    BLOCK DATA                                                        phsd 110
CCC                                                                     phsd 120
CCC   ROUTINE TO STORE TABLE TO CALL UP PROPER PHASE FUNCTION           phsd 130
CCC   AND 70 AVERAGE PHASE FUNCTIONS                                    phsd 140
CCC                                                                     phsd 150
C     COMMON/MNMPHS/ MNUM(27,26),PHSFNC(34,70)                          phsd 160
      COMMON/MNMPHS/MUM1(27),MUM2(27),MUM3(27),MUM4(27),MUM5(27),       phsd 170
     1MUM6(27),MUM7(27),MUM8(27),MUM9(27),MUM10(27),MUM11(27),MUM12(27),phsd 180
     2MUM13(27),MUM14(27),MUM15(27),MUM16(27),MUM17(27),MUM18(27),      phsd 190
     3MUM19(27),MUM20(27),MUM21(27),MUM22(27),MUM23(27),MUM24(27),      phsd 200
     4MUM25(27),MUM26(27),PHSF1(34),PHSF2(34),PHSF3(34),PHSF4(34),      phsd 210
     5PHSF5(34),PHSF6(34),PHSF7(34),PHSF8(34),PHSF9(34),                phsd 220
     6PHSF10(34),PHSF11(34),PHSF12(34),PHSF13(34),PHSF14(34),PHSF15(34),phsd 230
     7PHSF16(34),PHSF17(34),PHSF18(34),PHSF19(34),PHSF20(34),PHSF21(34),phsd 240
     8PHSF22(34),PHSF23(34),PHSF24(34),PHSF25(34),PHSF26(34),PHSF27(34),phsd 250
     9PHSF28(34),PHSF29(34),PHSF30(34),PHSF31(34),PHSF32(34),PHSF33(34),phsd 260
     9PHSF34(34),PHSF35(34),PHSF36(34),PHSF37(34),PHSF38(34),PHSF39(34),phsd 270
     1PHSF40(34),PHSF41(34),PHSF42(34),PHSF43(34),PHSF44(34),PHSF45(34),phsd 280
     2PHSF46(34),PHSF47(34),PHSF48(34),PHSF49(34),PHSF50(34),PHSF51(34),phsd 290
     3PHSF52(34),PHSF53(34),PHSF54(34),PHSF55(34),PHSF56(34),PHSF57(34),phsd 300
     4PHSF58(34),PHSF59(34),PHSF60(34),PHSF61(34),PHSF62(34),PHSF63(34),phsd 310
     5PHSF64(34),PHSF65(34),PHSF66(34),PHSF67(34),PHSF68(34),PHSF69(34),phsd 320
     6PHSF70(34)                                                        phsd 330
      DATA MUM1/ 3, 5, 4, 4, 4, 6, 8,22,21,22,26,26,                    phsd 340
     C26,27,29, 1,34,34,34,34,33,33,36,36,36,36,23/                     phsd 350
      DATA MUM2/ 3, 5, 4, 4, 4, 6, 8,22,21,21,22,26,                    phsd 360
     C26,27,29, 1,34,33,33,33,46,46,34,34,36,36,23/                     phsd 370
      DATA MUM3/ 3, 5, 5, 5,19, 6, 8,21, 7,21,22, 8,                    phsd 380
     C26, 1,27, 1,46,46,46,29,29,46,34,34,34,36,23/                     phsd 390
      DATA MUM4/58,58,62,62,62,63,63,63,60,64,64,64,                    phsd 400
     C21,70,65,65,65,65,66,66,65,27,29,29,46,33,34/                     phsd 410
      DATA MUM5/59,11,11,11,20,20,20,28,28,16,16,16,                    phsd 420
     C16,37,37,37,32,36,32,32,32,36,23,23,23,38,25/                     phsd 430
      DATA MUM6/ 9,59,11,13,13,26,26,26,27,29,46,28,                    phsd 440
     C28,29,28,28,37,37,37,37,34,36,36,36,23,23,38/                     phsd 450
      DATA MUM7/ 9, 9,14,14,14,15,15,15,66,65,27,26,                    phsd 460
     C29,27,29,29,29,29,29,29,46,34,36,36,36,23,23/                     phsd 470
      DATA MUM8/57,57,69,69,69,68,68,68,61,70,60,21,                    phsd 480
     C 7,66, 1, 1, 1, 1,66,66, 1,29,46,46,33,34,36/                     phsd 490
      DATA MUM9/ 2,18,18,19,19, 6,22,22,22,22,22,22,                    phsd 500
     C29,29,46,29,34,34,34,34,34,34,36,36,36,23,23/                     phsd 510
      DATA MUM10/ 2, 3,18,18,19, 6,22,22,21,22,22,22,                   phsd 520
     C27,27,29,27,33,33,46,46,46,33,34,34,36,36,23/                     phsd 530
      DATA MUM11/ 2, 3,18,18,19, 6,22,21, 7,22,22,22,                   phsd 540
     C27,27,27,27,46,29,29,29,29,46,33,34,34,36,36/                     phsd 550
      DATA MUM12/58,58,62,62,62,62,63,63,60,64,64,64,                   phsd 560
     C21,60, 7,65,65,65,66,67,65, 7,27,27,29,46,33/                     phsd 570
      DATA MUM13/10,59,11,11,13,20,20,28,28,16,16,16,                   phsd 580
     C16,37,37,37,32,36,32,32,32,36,23,23,23,38,25/                     phsd 590
      DATA MUM14/10,10,14,14,13,13,26,26,27,29,29,28,                   phsd 600
     C28,29,28,28,37,37,37,37,33,32,36,36,23,23,38/                     phsd 610
      DATA MUM15/10,69,14,14,14,15,15, 7,66,65,27,26,                   phsd 620
     C29, 1,29,29,29,29,29,29,46,34,36,36,36,23,23/                     phsd 630
      DATA MUM16/47,57,69,69,69,68,68,68,61,70,60,21,                   phsd 640
     C 7,66, 1, 1, 1, 1,66,66, 1,29,46,46,33,34,36/                     phsd 650
      DATA MUM17/29,16,32,32,36,36,23,23,23,38,38,38,                   phsd 660
     C25,25,25,25,25,25,25,25,35,35,35,35,35,40,39/                     phsd 670
      DATA MUM18/29,16,32,32,32,36,23,23,23,38,38,38,                   phsd 680
     C38,25,25,25,25,25,25,25,35,35,35,35,35,40,39/                     phsd 690
      DATA MUM19/29,28,37,37,32,36,36,23,23,23,23,23,                   phsd 700
     C38,25,25,25,25,25,25,25,35,35,35,35,35,40,40/                     phsd 710
      DATA MUM20/15,26,28,28,37,37,32,32,32,23,23,23,                   phsd 720
     C23,38,38,38,38,25,25,25,25,35,35,35,35,35,40/                     phsd 730
      DATA MUM21/20,20,37,37,24,23,38,25,25,35,35,35,                   phsd 740
     C40,40,39,39,39,39,39,39,39,39,39,39,39,39,39/                     phsd 750
      DATA MUM22/ 7,20,16,37,32,32,24,23,23,23,38,38,                   phsd 760
     C25,25,35,35,35,25,35,35,40,40,40,40,40,39,39/                     phsd 770
      DATA MUM23/17,51,13,13,20,20,28,28,37,37,37,37,                   phsd 780
     C32,32,24,24,23,23,38,38,38,38,38,38,38,25,35/                     phsd 790
      DATA MUM24/47,30,55,55,55,13,15,12,42,12, 1,26,                   phsd 800
     C44,52,44,44,45,45,45,45,24,23,38,38,38,25,35/                     phsd 810
      DATA MUM25/48,53,31,31,31,41,41,41,49,17,17,17,                   phsd 820
     C56,50,43,43,67,61,42,54,42,12,12,12,12, 1,44/                     phsd 830
      DATA MUM26/59,59,11,11,20,20,28,28,46,46,46,46,                   phsd 840
     C33,33,34,34,34,34,36,36,36,36,23,23,23,38,38/                     phsd 850
      DATA PHSF1/                                                       phsd 860
     C     4.81387,     4.26047,     3.42600,     2.74953,     2.21493, phsd 870
     C     1.78847,     1.44553,      .94765,      .62704,      .42183, phsd 880
     C      .28987,      .20375,      .14632,      .10720,      .05307, phsd 890
     C      .02882,      .01703,      .01093,      .00764,      .00582, phsd 900
     C      .00482,      .00431,      .00418,      .00411,      .00409, phsd 910
     C      .00412,      .00419,      .00428,      .00436,      .00442, phsd 920
     C      .00443,      .00433,      .00421,      .00435/              phsd 930
      DATA PHSF2/                                                       phsd 940
     C   100.88000,     4.36700,     2.73667,     2.10567,     1.68067, phsd 950
     C     1.36333,     1.11900,      .77513,      .55317,      .40393, phsd 960
     C      .30047,      .22690,      .17367,      .13437,      .07407, phsd 970
     C      .04350,      .02714,      .01795,      .01266,      .00958, phsd 980
     C      .00780,      .00683,      .00656,      .00639,      .00633, phsd 990
     C      .00636,      .00649,      .00669,      .00682,      .00671, phsd1000
     C      .00626,      .00582,      .00606,      .00646/              phsd1010
      DATA PHSF3/                                                       phsd1020
     C    75.07600,     3.77580,     2.33100,     1.82620,     1.49120, phsd1030
     C     1.23720,     1.03680,      .74470,      .54766,      .41000, phsd1040
     C      .31120,      .23880,      .18506,      .14476,      .08148, phsd1050
     C      .04851,      .03047,      .02022,      .01431,      .01086, phsd1060
     C      .00886,      .00785,      .00763,      .00756,      .00762, phsd1070
     C      .00780,      .00809,      .00843,      .00868,      .00861, phsd1080
     C      .00813,      .00790,      .00871,      .00948/              phsd1090
      DATA PHSF4/                                                       phsd1100
     C    14.11167,     3.29400,     1.58300,     1.07677,      .85635, phsd1110
     C      .73207,      .64758,      .52827,      .43880,      .36565, phsd1120
     C      .30452,      .25337,      .21070,      .17523,      .11108, phsd1130
     C      .07169,      .04755,      .03283,      .02388,      .01855, phsd1140
     C      .01557,      .01420,      .01396,      .01397,      .01424, phsd1150
     C      .01473,      .01545,      .01641,      .01758,      .01894, phsd1160
     C      .01999,      .02139,      .02376,      .02512/              phsd1170
      DATA PHSF5/                                                       phsd1180
     C    31.33400,     3.01260,     1.63600,     1.27260,     1.08020, phsd1190
     C      .94204,      .83026,      .65326,      .51696,      .40994, phsd1200
     C      .32558,      .25926,      .20702,      .16590,      .09733, phsd1210
     C      .05921,      .03764,      .02534,      .01809,      .01382, phsd1220
     C      .01155,      .01059,      .01051,      .01070,      .01115, phsd1230
     C      .01192,      .01303,      .01447,      .01610,      .01727, phsd1240
     C      .01780,      .01838,      .02091,      .02258/              phsd1250
      DATA PHSF6/                                                       phsd1260
     C    13.63333,     5.76067,     2.86717,     1.73500,     1.19267, phsd1270
     C      .89500,      .71377,      .50818,      .39290,      .31612, phsd1280
     C      .25947,      .21512,      .17950,      .15035,      .09782, phsd1290
     C      .06489,      .04418,      .03126,      .02336,      .01872, phsd1300
     C      .01625,      .01521,      .01510,      .01520,      .01550, phsd1310
     C      .01600,      .01670,      .01758,      .01860,      .01958, phsd1320
     C      .02003,      .01962,      .01969,      .02072/              phsd1330
      DATA PHSF7/                                                       phsd1340
     C    10.69213,     7.32563,     4.66850,     3.15175,     2.23700, phsd1350
     C     1.65050,     1.25550,      .77671,      .51168,      .35194, phsd1360
     C      .24998,      .18219,      .13565,      .10291,      .05530, phsd1370
     C      .03232,      .02036,      .01381,      .01015,      .00813, phsd1380
     C      .00711,      .00672,      .00671,      .00680,      .00698, phsd1390
     C      .00723,      .00753,      .00783,      .00813,      .00841, phsd1400
     C      .00864,      .00843,      .00793,      .00865/              phsd1410
      DATA PHSF8/                                                       phsd1420
     C    10.29225,     5.75700,     3.34100,     2.19450,     1.56850, phsd1430
     C     1.18950,      .94123,      .64033,      .46530,      .35098, phsd1440
     C      .27068,      .21203,      .16803,      .13445,      .07989, phsd1450
     C      .04975,      .03251,      .02247,      .01667,      .01344, phsd1460
     C      .01188,      .01148,      .01166,      .01210,      .01285, phsd1470
     C      .01392,      .01522,      .01656,      .01776,      .01897, phsd1480
     C      .02003,      .01932,      .01776,      .02043/              phsd1490
      DATA PHSF9/                                                       phsd1500
     C   232.30000,    13.73667,     3.92300,     1.97167,     1.29767, phsd1510
     C      .97050,      .77753,      .55393,      .41583,      .31763, phsd1520
     C      .24957,      .19523,      .15503,      .12260,      .06875, phsd1530
     C      .04015,      .02414,      .01485,      .01040,      .00803, phsd1540
     C      .00674,      .00569,      .00549,      .00550,      .00615, phsd1550
     C      .00775,      .01205,      .01820,      .02141,      .02076, phsd1560
     C      .02123,      .02409,      .03397,      .05439/              phsd1570
      DATA PHSF10/                                                      phsd1580
     C   286.85000,    20.19500,     5.34175,     2.25050,     1.24273, phsd1590
     C      .81960,      .60855,      .40505,      .30698,      .24148, phsd1600
     C      .19903,      .16210,      .13158,      .10705,      .06313, phsd1610
     C      .03807,      .02275,      .01373,      .00983,      .00704, phsd1620
     C      .00512,      .00400,      .00412,      .00401,      .00456, phsd1630
     C      .00580,      .00954,      .01947,      .03101,      .03784, phsd1640
     C      .04018,      .04524,      .06312,      .08295/              phsd1650
      DATA PHSF11/                                                      phsd1660
     C    15.36571,     6.08614,     3.15543,     1.98500,     1.39671, phsd1670
     C     1.05749,      .84297,      .58904,      .44120,      .34216, phsd1680
     C      .27140,      .21837,      .17669,      .14371,      .08723, phsd1690
     C      .05498,      .03568,      .02444,      .01753,      .01339, phsd1700
     C      .01087,      .00986,      .00983,      .01019,      .01094, phsd1710
     C      .01243,      .01473,      .01871,      .02423,      .03204, phsd1720
     C      .03592,      .03792,      .04492,      .05004/              phsd1730
      DATA PHSF12/                                                      phsd1740
     C     5.64100,     5.44440,     4.90680,     4.15680,     3.34180, phsd1750
     C     2.57840,     1.93040,     1.03104,      .54702,      .30330, phsd1760
     C      .18026,      .11514,      .07816,      .05557,      .02729, phsd1770
     C      .01547,      .00986,      .00697,      .00542,      .00457, phsd1780
     C      .00413,      .00392,      .00387,      .00385,      .00385, phsd1790
     C      .00385,      .00385,      .00386,      .00391,      .00401, phsd1800
     C      .00400,      .00368,      .00352,      .00365/              phsd1810
      DATA PHSF13/                                                      phsd1820
     C     8.90311,     6.06467,     3.93056,     2.63178,     1.82878, phsd1830
     C     1.32733,     1.00856,      .65342,      .46637,      .34863, phsd1840
     C      .26662,      .20709,      .16222,      .12811,      .07347, phsd1850
     C      .04425,      .02794,      .01867,      .01336,      .01033, phsd1860
     C      .00874,      .00818,      .00837,      .00894,      .01001, phsd1870
     C      .01171,      .01423,      .01759,      .02129,      .02494, phsd1880
     C      .02767,      .02690,      .02658,      .03550/              phsd1890
      DATA PHSF14/                                                      phsd1900
     C    41.20875,    11.84363,     5.00750,     2.73850,     1.74425, phsd1910
     C     1.23475,      .93516,      .60986,      .43641,      .32733, phsd1920
     C      .24819,      .19145,      .14831,      .11555,      .06356, phsd1930
     C      .03649,      .02176,      .01390,      .00974,      .00759, phsd1940
     C      .00657,      .00643,      .00671,      .00741,      .00906, phsd1950
     C      .01210,      .01618,      .01839,      .01979,      .02104, phsd1960
     C      .02356,      .02652,      .02694,      .04004/              phsd1970
      DATA PHSF15/                                                      phsd1980
     C     7.98100,     5.58389,     3.81067,     2.75011,     2.05900, phsd1990
     C     1.58178,     1.24011,      .80424,      .55144,      .39021, phsd2000
     C      .28203,      .20701,      .15369,      .11570,      .05985, phsd2010
     C      .03329,      .01999,      .01295,      .00920,      .00726, phsd2020
     C      .00645,      .00646,      .00683,      .00743,      .00833, phsd2030
     C      .00942,      .01038,      .01112,      .01187,      .01290, phsd2040
     C      .01451,      .01415,      .01176,      .01646/              phsd2050
      DATA PHSF16/                                                      phsd2060
     C     1.52473,     1.44645,     1.30800,     1.16736,     1.03716, phsd2070
     C      .92088,      .81802,      .64725,      .51405,      .40964, phsd2080
     C      .32742,      .26255,      .21129,      .17075,      .10236, phsd2090
     C      .06361,      .04130,      .02819,      .02041,      .01583, phsd2100
     C      .01330,      .01222,      .01213,      .01235,      .01288, phsd2110
     C      .01373,      .01491,      .01635,      .01784,      .01906, phsd2120
     C      .02003,      .02225,      .02715,      .03082/              phsd2130
      DATA PHSF17/                                                      phsd2140
     C   143.02500,    55.03750,    11.00250,     3.10675,     1.29675, phsd2150
     C      .67675,      .40423,      .18580,      .10582,      .06884, phsd2160
     C      .04883,      .03665,      .02858,      .02291,      .01418, phsd2170
     C      .00942,      .00664,      .00497,      .00397,      .00337, phsd2180
     C      .00302,      .00283,      .00277,      .00273,      .00272, phsd2190
     C      .00275,      .00283,      .00298,      .00314,      .00324, phsd2200
     C      .00332,      .00347,      .00373,      .00359/              phsd2210
      DATA PHSF18/                                                      phsd2220
     C    26.74167,     3.70000,     1.73800,     1.24150,     1.02157, phsd2230
     C      .88398,      .78022,      .62040,      .49727,      .39937, phsd2240
     C      .32113,      .25863,      .20880,      .16910,      .10153, phsd2250
     C      .06289,      .04050,      .02733,      .01952,      .01491, phsd2260
     C      .01225,      .01083,      .01046,      .01025,      .01017, phsd2270
     C      .01020,      .01030,      .01043,      .01053,      .01059, phsd2280
     C      .01061,      .01071,      .01107,      .01136/              phsd2290
      DATA PHSF19/                                                      phsd2300
     C    16.76800,     4.67840,     2.10000,     1.29420,      .94964, phsd2310
     C      .76794,      .65496,      .51386,      .41986,      .34756, phsd2320
     C      .28896,      .24060,      .20054,      .16730,      .10712, phsd2330
     C      .06978,      .04668,      .03245,      .02379,      .01863, phsd2340
     C      .01572,      .01429,      .01397,      .01384,      .01389, phsd2350
     C      .01407,      .01438,      .01481,      .01529,      .01572, phsd2360
     C      .01603,      .01613,      .01637,      .01668/              phsd2370
      DATA PHSF20/                                                      phsd2380
     C     3.35750,     2.77033,     2.20467,     1.77208,     1.43950, phsd2390
     C     1.18167,      .98083,      .69731,      .51368,      .38864, phsd2400
     C      .29989,      .23472,      .18552,      .14785,      .08680, phsd2410
     C      .05339,      .03443,      .02340,      .01689,      .01308, phsd2420
     C      .01097,      .01015,      .01022,      .01062,      .01145, phsd2430
     C      .01289,      .01512,      .01829,      .02228,      .02580, phsd2440
     C      .02615,      .02523,      .02990,      .03617/              phsd2450
      DATA PHSF21/                                                      phsd2460
     C    13.35245,     7.86691,     4.51555,     2.89018,     1.99645, phsd2470
     C     1.45664,     1.10655,      .69522,      .47093,      .33483, phsd2480
     C      .24620,      .18557,      .14266,      .11148,      .06369, phsd2490
     C      .03890,      .02523,      .01742,      .01294,      .01044, phsd2500
     C      .00921,      .00881,      .00886,      .00906,      .00940, phsd2510
     C      .00985,      .01039,      .01098,      .01163,      .01232, phsd2520
     C      .01277,      .01220,      .01131,      .01247/              phsd2530
      DATA PHSF22/                                                      phsd2540
     C     9.49578,     6.01133,     3.71372,     2.49106,     1.77633, phsd2550
     C     1.32572,     1.02483,      .66094,      .45784,      .33285, phsd2560
     C      .25044,      .19334,      .15224,      .12179,      .07358, phsd2570
     C      .04717,      .03185,      .02271,      .01730,      .01421, phsd2580
     C      .01264,      .01208,      .01207,      .01220,      .01245, phsd2590
     C      .01280,      .01321,      .01365,      .01405,      .01436, phsd2600
     C      .01447,      .01422,      .01407,      .01456/              phsd2610
      DATA PHSF23/                                                      phsd2620
     C      .45555,      .45332,      .44686,      .43683,      .42405, phsd2630
     C      .40929,      .39319,      .35887,      .32388,      .28985, phsd2640
     C      .25771,      .22804,      .20101,      .17671,      .12722, phsd2650
     C      .09162,      .06691,      .05037,      .03979,      .03345, phsd2660
     C      .03009,      .02875,      .02860,      .02871,      .02902, phsd2670
     C      .02946,      .03000,      .03059,      .03120,      .03179, phsd2680
     C      .03232,      .03276,      .03305,      .03315/              phsd2690
      DATA PHSF24/                                                      phsd2700
     C      .51072,      .50922,      .50476,      .49744,      .48752, phsd2710
     C      .47514,      .46072,      .42698,      .38890,      .34888, phsd2720
     C      .30902,      .27072,      .23504,      .20254,      .13652, phsd2730
     C      .09070,      .06070,      .04186,      .03042,      .02372, phsd2740
     C      .02000,      .01816,      .01773,      .01753,      .01753, phsd2750
     C      .01767,      .01792,      .01826,      .01866,      .01907, phsd2760
     C      .01948,      .01984,      .02009,      .02019/              phsd2770
      DATA PHSF25/                                                      phsd2780
     C      .21327,      .21298,      .21208,      .21060,      .20856, phsd2790
     C      .20606,      .20305,      .19588,      .18741,      .17798, phsd2800
     C      .16793,      .15754,      .14702,      .13659,      .11191, phsd2810
     C      .09060,      .07368,      .06147,      .05381,      .05020, phsd2820
     C      .04990,      .05207,      .05380,      .05581,      .05801, phsd2830
     C      .06028,      .06253,      .06468,      .06664,      .06834, phsd2840
     C      .06973,      .07077,      .07140,      .07161/              phsd2850
      DATA PHSF26/                                                      phsd2860
     C     4.10720,     3.38127,     2.61113,     2.06147,     1.66320, phsd2870
     C     1.36400,     1.13140,      .79725,      .57362,      .41947, phsd2880
     C      .31103,      .23368,      .17766,      .13659,      .07413, phsd2890
     C      .04288,      .02647,      .01753,      .01255,      .00978, phsd2900
     C      .00837,      .00791,      .00800,      .00831,      .00888, phsd2910
     C      .00974,      .01087,      .01219,      .01347,      .01448, phsd2920
     C      .01468,      .01320,      .01237,      .01467/              phsd2930
      DATA PHSF27/                                                      phsd2940
     C     4.37885,     3.78265,     2.97685,     2.33720,     1.85900, phsd2950
     C     1.49815,     1.22035,      .83140,      .58200,      .41624, phsd2960
     C      .30308,      .22426,      .16839,      .12816,      .06844, phsd2970
     C      .03932,      .02424,      .01607,      .01151,      .00894, phsd2980
     C      .00753,      .00684,      .00668,      .00661,      .00661, phsd2990
     C      .00668,      .00679,      .00692,      .00706,      .00715, phsd3000
     C      .00715,      .00705,      .00703,      .00721/              phsd3010
      DATA PHSF28/                                                      phsd3020
     C     1.99262,     1.84586,     1.62814,     1.42586,     1.24733, phsd3030
     C     1.09157,      .95572,      .73435,      .56567,      .43699, phsd3040
     C      .33874,      .26371,      .20635,      .16235,      .09181, phsd3050
     C      .05440,      .03397,      .02253,      .01601,      .01230, phsd3060
     C      .01031,      .00946,      .00938,      .00951,      .00985, phsd3070
     C      .01043,      .01123,      .01222,      .01328,      .01405, phsd3080
     C      .01405,      .01340,      .01391,      .01521/              phsd3090
      DATA PHSF29/                                                      phsd3100
     C     2.72707,     2.52017,     2.15725,     1.81463,     1.52555, phsd3110
     C     1.28665,     1.08932,      .78963,      .58023,      .43161, phsd3120
     C      .32473,      .24695,      .18977,      .14727,      .08150, phsd3130
     C      .04783,      .02980,      .01981,      .01414,      .01091, phsd3140
     C      .00911,      .00819,      .00796,      .00784,      .00781, phsd3150
     C      .00787,      .00798,      .00812,      .00825,      .00831, phsd3160
     C      .00830,      .00829,      .00847,      .00868/              phsd3170
      DATA PHSF30/                                                      phsd3180
     C   183.60000,    32.48000,     3.18400,     1.30100,      .85020, phsd3190
     C      .63540,      .54470,      .41140,      .33110,      .26080, phsd3200
     C      .20850,      .16570,      .12700,      .09546,      .04755, phsd3210
     C      .02460,      .01160,      .00650,      .00370,      .00264, phsd3220
     C      .00234,      .00355,      .00326,      .00308,      .00528, phsd3230
     C      .01403,      .02285,      .01565,      .01398,      .01362, phsd3240
     C      .01240,      .01544,      .02607,      .04655/              phsd3250
      DATA PHSF31/                                                      phsd3260
     C  1565.50000,     9.96867,     1.73233,      .92547,      .70240, phsd3270
     C      .59477,      .52513,      .42173,      .33793,      .26700, phsd3280
     C      .20973,      .16157,      .11973,      .09043,      .04400, phsd3290
     C      .01947,      .00793,      .00346,      .00177,      .00138, phsd3300
     C      .00166,      .00254,      .00438,      .00358,      .00653, phsd3310
     C      .02986,      .01362,      .01200,      .00992,      .00867, phsd3320
     C      .00765,      .00818,      .00986,      .05756/              phsd3330
      DATA PHSF32/                                                      phsd3340
     C      .79503,      .78714,      .76605,      .73601,      .70017, phsd3350
     C      .66092,      .61994,      .53736,      .45896,      .38796, phsd3360
     C      .32563,      .27208,      .22676,      .18879,      .11979, phsd3370
     C      .07727,      .05125,      .03541,      .02586,      .02025, phsd3380
     C      .01717,      .01576,      .01552,      .01551,      .01571, phsd3390
     C      .01607,      .01656,      .01710,      .01764,      .01814, phsd3400
     C      .01872,      .01965,      .02093,      .02164/              phsd3410
      DATA PHSF33/                                                      phsd3420
     C     1.56125,     1.49306,     1.34900,     1.19444,     1.05048, phsd3430
     C      .92302,      .81176,      .63177,      .49621,      .39304, phsd3440
     C      .31378,      .25238,      .20435,      .16659,      .10271, phsd3450
     C      .06594,      .04424,      .03123,      .02338,      .01868, phsd3460
     C      .01592,      .01439,      .01392,      .01360,      .01339, phsd3470
     C      .01325,      .01315,      .01310,      .01308,      .01314, phsd3480
     C      .01330,      .01355,      .01381,      .01392/              phsd3490
      DATA PHSF34/                                                      phsd3500
     C     1.22457,     1.18085,     1.08610,      .98058,      .87944, phsd3510
     C      .78708,      .70433,      .56565,      .45677,      .37107, phsd3520
     C      .30321,      .24916,      .20585,      .17099,      .11000, phsd3530
     C      .07323,      .05066,      .03666,      .02794,      .02253, phsd3540
     C      .01924,      .01727,      .01663,      .01614,      .01577, phsd3550
     C      .01550,      .01532,      .01522,      .01523,      .01536, phsd3560
     C      .01562,      .01596,      .01627,      .01640/              phsd3570
      DATA PHSF35/                                                      phsd3580
     C      .15944,      .15931,      .15893,      .15825,      .15734, phsd3590
     C      .15620,      .15479,      .15131,      .14701,      .14199, phsd3600
     C      .13638,      .13030,      .12389,      .11727,      .10065, phsd3610
     C      .08527,      .07250,      .06322,      .05782,      .05621, phsd3620
     C      .05792,      .06222,      .06505,      .06817,      .07146, phsd3630
     C      .07479,      .07805,      .08112,      .08390,      .08631, phsd3640
     C      .08826,      .08970,      .09058,      .09088/              phsd3650
      DATA PHSF36/                                                      phsd3660
     C      .72999,      .72086,      .69632,      .66227,      .62369, phsd3670
     C      .58366,      .54390,      .46850,      .40091,      .34178, phsd3680
     C      .29081,      .24724,      .21025,      .17893,      .12047, phsd3690
     C      .08253,      .05810,      .04253,      .03278,      .02688, phsd3700
     C      .02352,      .02183,      .02142,      .02122,      .02118, phsd3710
     C      .02127,      .02146,      .02171,      .02202,      .02235, phsd3720
     C      .02273,      .02314,      .02351,      .02367/              phsd3730
      DATA PHSF37/                                                      phsd3740
     C     1.15457,     1.13481,     1.08637,     1.02208,      .95035, phsd3750
     C      .87581,      .80158,      .66139,      .53812,      .43402, phsd3760
     C      .34834,      .27905,      .22360,      .17950,      .10538, phsd3770
     C      .06398,      .04056,      .02712,      .01935,      .01490, phsd3780
     C      .01250,      .01143,      .01126,      .01130,      .01153, phsd3790
     C      .01191,      .01242,      .01301,      .01357,      .01399, phsd3800
     C      .01422,      .01450,      .01526,      .01585/              phsd3810
      DATA PHSF38/                                                      phsd3820
     C      .31056,      .30982,      .30766,      .30415,      .29944, phsd3830
     C      .29365,      .28698,      .27149,      .25411,      .23575, phsd3840
     C      .21708,      .19865,      .18081,      .16385,      .12623, phsd3850
     C      .09621,      .07365,      .05766,      .04714,      .04092, phsd3860
     C      .03792,      .03722,      .03748,      .03804,      .03881, phsd3870
     C      .03972,      .04071,      .04172,      .04270,      .04358, phsd3880
     C      .04433,      .04490,      .04526,      .04539/              phsd3890
      DATA PHSF39/                                                      phsd3900
     C      .12377,      .12369,      .12346,      .12308,      .12254, phsd3910
     C      .12185,      .12102,      .11893,      .11632,      .11324, phsd3920
     C      .10974,      .10592,      .10181,      .09752,      .08650, phsd3930
     C      .07613,      .06762,      .06192,      .05964,      .06097, phsd3940
     C      .06565,      .07304,      .07747,      .08219,      .08708, phsd3950
     C      .09196,      .09670,      .10114,      .10516,      .10862, phsd3960
     C      .11144,      .11349,      .11477,      .11519/              phsd3970
      DATA PHSF40/                                                      phsd3980
     C      .13400,      .13391,      .13367,      .13321,      .13259, phsd3990
     C      .13180,      .13085,      .12842,      .12541,      .12185, phsd4000
     C      .11785,      .11344,      .10872,      .10383,      .09125, phsd4010
     C      .07940,      .06955,      .06267,      .05932,      .05961, phsd4020
     C      .06323,      .06951,      .07336,      .07751,      .08181, phsd4030
     C      .08613,      .09032,      .09426,      .09782,      .10091, phsd4040
     C      .10339,      .10522,      .10634,      .10673/              phsd4050
      DATA PHSF41/                                                      phsd4060
     C   227.93333,    32.03000,     4.13167,     1.54367,      .95123, phsd4070
     C      .71613,      .58840,      .43330,      .33043,      .25093, phsd4080
     C      .18920,      .14303,      .10800,      .08045,      .03915, phsd4090
     C      .01854,      .00866,      .00431,      .00239,      .00168, phsd4100
     C      .00157,      .00266,      .00434,      .00724,      .01132, phsd4110
     C      .01476,      .00983,      .00835,      .00785,      .00711, phsd4120
     C      .00717,      .00803,      .01265,      .02951/              phsd4130
      DATA PHSF42/                                                      phsd4140
     C    10.12167,     9.39467,     7.61833,     5.59200,     3.87533, phsd4150
     C     2.62733,     1.78200,      .85247,      .43620,      .23983, phsd4160
     C      .14187,      .08952,      .05950,      .04115,      .01854, phsd4170
     C      .00956,      .00552,      .00354,      .00251,      .00195, phsd4180
     C      .00163,      .00145,      .00139,      .00134,      .00129, phsd4190
     C      .00125,      .00122,      .00120,      .00120,      .00120, phsd4200
     C      .00120,      .00115,      .00105,      .00106/              phsd4210
      DATA PHSF43/                                                      phsd4220
     C    23.70500,    19.81000,    12.14500,     6.16400,     2.96850, phsd4230
     C     1.51300,      .86515,      .39850,      .24525,      .17225, phsd4240
     C      .12825,      .09809,      .07598,      .05932,      .03272, phsd4250
     C      .01871,      .01120,      .00710,      .00485,      .00363, phsd4260
     C      .00303,      .00286,      .00290,      .00302,      .00318, phsd4270
     C      .00334,      .00345,      .00349,      .00354,      .00388, phsd4280
     C      .00510,      .00600,      .00344,      .00472/              phsd4290
      DATA PHSF44/                                                      phsd4300
     C     1.13310,     1.12586,     1.10518,     1.07182,     1.02708, phsd4310
     C      .97348,      .91266,      .77952,      .64406,      .51826, phsd4320
     C      .40884,      .31800,      .24506,      .18784,      .09626, phsd4330
     C      .05060,      .02818,      .01701,      .01126,      .00818, phsd4340
     C      .00651,      .00563,      .00539,      .00526,      .00522, phsd4350
     C      .00525,      .00536,      .00551,      .00571,      .00595, phsd4360
     C      .00624,      .00656,      .00681,      .00692/              phsd4370
      DATA PHSF45/                                                      phsd4380
     C      .61443,      .61273,      .60783,      .59967,      .58847, phsd4390
     C      .57447,      .55783,      .51803,      .47153,      .42127, phsd4400
     C      .36970,      .31913,      .27140,      .22780,      .14030, phsd4410
     C      .08293,      .04868,      .02942,      .01892,      .01327, phsd4420
     C      .01027,      .00875,      .00834,      .00811,      .00800, phsd4430
     C      .00801,      .00810,      .00824,      .00842,      .00861, phsd4440
     C      .00879,      .00893,      .00903,      .00906/              phsd4450
      DATA PHSF46/                                                      phsd4460
     C     2.06615,     1.93640,     1.69345,     1.45610,     1.24945, phsd4470
     C     1.07421,      .92677,      .69739,      .53175,      .41021, phsd4480
     C      .31969,      .25144,      .19943,      .15944,      .09421, phsd4490
     C      .05837,      .03804,      .02624,      .01931,      .01523, phsd4500
     C      .01288,      .01160,      .01123,      .01099,      .01084, phsd4510
     C      .01077,      .01073,      .01072,      .01070,      .01068, phsd4520
     C      .01069,      .01078,      .01095,      .01105/              phsd4530
      DATA PHSF47/                                                      phsd4540
     C  1888.10000,    16.32500,     2.35900,     1.00950,      .67300, phsd4550
     C      .53970,      .46415,      .37245,      .30310,      .24865, phsd4560
     C      .20715,      .15820,      .13160,      .10710,      .05669, phsd4570
     C      .02778,      .01325,      .00679,      .00344,      .00306, phsd4580
     C      .00420,      .00189,      .00154,      .00137,      .00145, phsd4590
     C      .00265,      .01227,      .03321,      .02025,      .01971, phsd4600
     C      .01869,      .02153,      .03166,      .09376/              phsd4610
      DATA PHSF48/                                                      phsd4620
     C 18500.00000,     2.83400,      .85070,      .58200,      .49050, phsd4630
     C      .44240,      .40380,      .35560,      .31400,      .25730, phsd4640
     C      .19510,      .17010,      .12770,      .10750,      .05190, phsd4650
     C      .02554,      .01105,      .00365,      .00203,      .00200, phsd4660
     C      .00370,      .00118,      .00112,      .00112,      .00113, phsd4670
     C      .00113,      .00549,      .02603,      .02019,      .01751, phsd4680
     C      .01439,      .01308,      .01407,      .10720/              phsd4690
      DATA PHSF49/                                                      phsd4700
     C   190.90000,    60.01000,     8.56500,     2.26600,      .98870, phsd4710
     C      .55520,      .36100,      .19580,      .12640,      .08822, phsd4720
     C      .06382,      .04693,      .03478,      .02590,      .01264, phsd4730
     C      .00642,      .00348,      .00208,      .00143,      .00120, phsd4740
     C      .00124,      .00131,      .00123,      .00108,      .00097, phsd4750
     C      .00092,      .00090,      .00089,      .00091,      .00096, phsd4760
     C      .00107,      .00136,      .00251,      .00246/              phsd4770
      DATA PHSF50/                                                      phsd4780
     C    44.01000,    33.47000,    16.38000,     6.54600,     2.66800, phsd4790
     C     1.24800,      .68020,      .27930,      .14670,      .08818, phsd4800
     C      .05760,      .03977,      .02856,      .02111,      .01087, phsd4810
     C      .00620,      .00387,      .00265,      .00198,      .00161, phsd4820
     C      .00141,      .00129,      .00125,      .00122,      .00120, phsd4830
     C      .00119,      .00118,      .00117,      .00117,      .00117, phsd4840
     C      .00118,      .00118,      .00113,      .00113/              phsd4850
      DATA PHSF51/                                                      phsd4860
     C    45.72000,    22.68000,     8.45200,     3.68600,     1.89400, phsd4870
     C     1.11400,      .72990,      .39640,      .26220,      .19430, phsd4880
     C      .15350,      .12410,      .10220,      .08533,      .05455, phsd4890
     C      .03546,      .02333,      .01580,      .01103,      .00801, phsd4900
     C      .00615,      .00530,      .00517,      .00509,      .00547, phsd4910
     C      .00614,      .00763,      .01038,      .01534,      .02235, phsd4920
     C      .02692,      .02902,      .03071,      .03320/              phsd4930
      DATA PHSF52/                                                      phsd4940
     C     1.35800,     1.34900,     1.32400,     1.28300,     1.22900, phsd4950
     C     1.16200,     1.08700,      .91840,      .74460,      .58190, phsd4960
     C      .44060,      .32530,      .23580,      .16900,      .07319, phsd4970
     C      .03392,      .01766,      .01037,      .00676,      .00484, phsd4980
     C      .00377,      .00317,      .00298,      .00284,      .00274, phsd4990
     C      .00269,      .00267,      .00268,      .00274,      .00283, phsd5000
     C      .00294,      .00306,      .00314,      .00317/              phsd5010
      DATA PHSF53/                                                      phsd5020
     C  8232.00000,     3.82700,      .95120,      .65890,      .52640, phsd5030
     C      .49710,      .52530,      .38980,      .33700,      .25360, phsd5040
     C      .21110,      .16430,      .12260,      .09209,      .04939, phsd5050
     C      .01954,      .00754,      .00272,      .00161,      .00134, phsd5060
     C      .00166,      .00334,      .00383,      .00124,      .00127, phsd5070
     C      .01906,      .01831,      .01420,      .01077,      .00904, phsd5080
     C      .00782,      .00667,      .00690,      .04228/              phsd5090
      DATA PHSF54/                                                      phsd5100
     C    13.90000,    12.72000,     9.87700,     6.74400,     4.24600, phsd5110
     C     2.58600,     1.58500,      .66190,      .32580,      .18150, phsd5120
     C      .10950,      .06977,      .04630,      .03174,      .01380, phsd5130
     C      .00682,      .00376,      .00228,      .00153,      .00111, phsd5140
     C      .00088,      .00073,      .00068,      .00064,      .00061, phsd5150
     C      .00059,      .00057,      .00056,      .00055,      .00055, phsd5160
     C      .00056,      .00054,      .00050,      .00050/              phsd5170
      DATA PHSF55/                                                      phsd5180
     C    36.42000,    23.44333,     8.52800,     3.11067,     1.49100, phsd5190
     C      .91783,      .67607,      .46263,      .34257,      .26417, phsd5200
     C      .20367,      .15540,      .12417,      .09463,      .05233, phsd5210
     C      .02803,      .01629,      .00958,      .00583,      .00472, phsd5220
     C      .00438,      .00458,      .00534,      .00777,      .01141, phsd5230
     C      .01545,      .01958,      .01880,      .01607,      .01696, phsd5240
     C      .01907,      .03025,      .03881,      .05215/              phsd5250
      DATA PHSF56/                                                      phsd5260
     C    46.49000,    31.63000,    12.30000,     4.19500,     1.72400, phsd5270
     C      .91960,      .59580,      .34070,      .23500,      .17450, phsd5280
     C      .13450,      .10490,      .08291,      .06559,      .03717, phsd5290
     C      .02135,      .01252,      .00762,      .00490,      .00346, phsd5300
     C      .00283,      .00278,      .00305,      .00366,      .00464, phsd5310
     C      .00584,      .00670,      .00683,      .00640,      .00651, phsd5320
     C      .00846,      .01345,      .01103,      .01440/              phsd5330
      DATA PHSF57/                                                      phsd5340
     C  1698.00000,    15.47000,     3.11967,     1.46900,      .97483, phsd5350
     C      .76003,      .62317,      .46853,      .36270,      .28193, phsd5360
     C      .22163,      .17287,      .13357,      .10470,      .05409, phsd5370
     C      .02799,      .01396,      .00794,      .00479,      .00390, phsd5380
     C      .00409,      .00407,      .00363,      .00324,      .00464, phsd5390
     C      .01060,      .01826,      .01957,      .01599,      .01547, phsd5400
     C      .01597,      .01678,      .02321,      .05917/              phsd5410
      DATA PHSF58/                                                      phsd5420
     C   525.35000,     5.54850,     3.39200,     2.40225,     1.79275, phsd5430
     C     1.38725,     1.10400,      .74203,      .52213,      .37803, phsd5440
     C      .27970,      .20945,      .15915,      .12258,      .06615, phsd5450
     C      .03783,      .02289,      .01510,      .01074,      .00837, phsd5460
     C      .00717,      .00653,      .00661,      .00699,      .00781, phsd5470
     C      .00922,      .01127,      .01364,      .01588,      .01743, phsd5480
     C      .01838,      .01583,      .01491,      .02015/              phsd5490
      DATA PHSF59/                                                      phsd5500
     C    56.57000,    10.86983,     4.30450,     2.32817,     1.48817, phsd5510
     C     1.05608,      .80615,      .53590,      .39175,      .30123, phsd5520
     C      .23833,      .19167,      .15528,      .12653,      .07717, phsd5530
     C      .04870,      .03126,      .02128,      .01493,      .01091, phsd5540
     C      .00856,      .00765,      .00767,      .00787,      .00840, phsd5550
     C      .00973,      .01199,      .01623,      .02446,      .03802, phsd5560
     C      .04421,      .04801,      .05991,      .06067/              phsd5570
      DATA PHSF60/                                                      phsd5580
     C    37.27000,    14.69200,     6.78420,     3.81000,     2.39800, phsd5590
     C     1.62640,     1.16200,      .66076,      .41350,      .27594, phsd5600
     C      .19292,      .13976,      .10406,      .07926,      .04328, phsd5610
     C      .02568,      .01635,      .01116,      .00822,      .00657, phsd5620
     C      .00569,      .00527,      .00516,      .00510,      .00508, phsd5630
     C      .00511,      .00518,      .00528,      .00537,      .00537, phsd5640
     C      .00527,      .00528,      .00536,      .00549/              phsd5650
      DATA PHSF61/                                                      phsd5660
     C    24.80333,    14.28000,     8.35167,     5.06833,     3.14833, phsd5670
     C     2.03700,     1.38367,      .72373,      .42163,      .26167, phsd5680
     C      .16947,      .11343,      .07807,      .05503,      .02512, phsd5690
     C      .01283,      .00726,      .00455,      .00317,      .00247, phsd5700
     C      .00214,      .00196,      .00187,      .00177,      .00168, phsd5710
     C      .00159,      .00153,      .00150,      .00151,      .00161, phsd5720
     C      .00185,      .00209,      .00168,      .00168/              phsd5730
      DATA PHSF62/                                                      phsd5740
     C   101.15286,     5.39657,     2.28843,     1.62671,     1.32757, phsd5750
     C     1.13054,      .97696,      .73870,      .56023,      .42529, phsd5760
     C      .32319,      .24639,      .18874,      .14550,      .07827, phsd5770
     C      .04428,      .02652,      .01699,      .01182,      .00901, phsd5780
     C      .00763,      .00715,      .00718,      .00742,      .00797, phsd5790
     C      .00901,      .01007,      .01036,      .01076,      .01129, phsd5800
     C      .01176,      .01204,      .01243,      .01444/              phsd5810
      DATA PHSF63/                                                      phsd5820
     C    48.02600,    10.67620,     3.98180,     2.14360,     1.42080, phsd5830
     C     1.06302,      .85178,      .60524,      .45450,      .34880, phsd5840
     C      .26996,      .21070,      .16526,      .13048,      .07415, phsd5850
     C      .04380,      .02714,      .01784,      .01271,      .00998, phsd5860
     C      .00877,      .00871,      .00917,      .00990,      .01086, phsd5870
     C      .01174,      .01200,      .01185,      .01202,      .01250, phsd5880
     C      .01377,      .01588,      .01523,      .01909/              phsd5890
      DATA PHSF64/                                                      phsd5900
     C    34.21833,    12.06500,     4.94933,     2.57983,     1.56550, phsd5910
     C     1.05695,      .77238,      .48407,      .34568,      .26490, phsd5920
     C      .21097,      .17168,      .14155,      .11773,      .07609, phsd5930
     C      .05056,      .03467,      .02483,      .01884,      .01536, phsd5940
     C      .01352,      .01276,      .01267,      .01273,      .01293, phsd5950
     C      .01326,      .01371,      .01427,      .01491,      .01553, phsd5960
     C      .01582,      .01535,      .01512,      .01573/              phsd5970
      DATA PHSF65/                                                      phsd5980
     C     8.59757,     6.79429,     4.60071,     3.23457,     2.36214, phsd5990
     C     1.77586,     1.36529,      .84849,      .55343,      .37399, phsd6000
     C      .25995,      .18501,      .13442,      .09951,      .05039, phsd6010
     C      .02794,      .01684,      .01103,      .00787,      .00614, phsd6020
     C      .00521,      .00476,      .00465,      .00460,      .00460, phsd6030
     C      .00464,      .00470,      .00479,      .00490,      .00502, phsd6040
     C      .00509,      .00498,      .00479,      .00491/              phsd6050
      DATA PHSF66/                                                      phsd6060
     C     7.73378,     6.31989,     4.61211,     3.41189,     2.56922, phsd6070
     C     1.96389,     1.51900,      .93560,      .59521,      .38946, phsd6080
     C      .26146,      .17967,      .12618,      .09038,      .04241, phsd6090
     C      .02194,      .01241,      .00766,      .00519,      .00386, phsd6100
     C      .00314,      .00278,      .00267,      .00259,      .00254, phsd6110
     C      .00251,      .00251,      .00252,      .00256,      .00261, phsd6120
     C      .00268,      .00274,      .00268,      .00273/              phsd6130
      DATA PHSF67/                                                      phsd6140
     C    13.24333,    11.06100,     7.42300,     4.70567,     2.98533, phsd6150
     C     1.97200,     1.37267,      .75370,      .45857,      .29573, phsd6160
     C      .19823,      .13693,      .09689,      .07004,      .03369, phsd6170
     C      .01789,      .01040,      .00660,      .00460,      .00353, phsd6180
     C      .00298,      .00273,      .00268,      .00266,      .00265, phsd6190
     C      .00265,      .00265,      .00266,      .00270,      .00282, phsd6200
     C      .00308,      .00319,      .00262,      .00275/              phsd6210
      DATA PHSF68/                                                      phsd6220
     C    37.10000,    12.65667,     5.63100,     3.16800,     2.05233, phsd6230
     C     1.45750,     1.09982,      .69772,      .47462,      .33515, phsd6240
     C      .24170,      .17635,      .13072,      .09747,      .04922, phsd6250
     C      .02644,      .01502,      .00935,      .00640,      .00503, phsd6260
     C      .00468,      .00549,      .00635,      .00767,      .00953, phsd6270
     C      .01124,      .01140,      .01090,      .01055,      .01125, phsd6280
     C      .01355,      .01701,      .01486,      .02554/              phsd6290
      DATA PHSF69/                                                      phsd6300
     C   256.51429,    19.73571,     5.24714,     2.30771,     1.36100, phsd6310
     C      .94431,      .72756,      .50319,      .37583,      .28504, phsd6320
     C      .22174,      .17010,      .13151,      .10155,      .05367, phsd6330
     C      .02848,      .01561,      .00902,      .00592,      .00443, phsd6340
     C      .00434,      .00493,      .00535,      .00629,      .00897, phsd6350
     C      .01636,      .01849,      .01647,      .01539,      .01496, phsd6360
     C      .01673,      .02178,      .02754,      .05075/              phsd6370
      DATA PHSF70/                                                      phsd6380
     C    27.44667,    14.67667,     7.63033,     4.46200,     2.82633, phsd6390
     C     1.89500,     1.32600,      .71323,      .41983,      .26377, phsd6400
     C      .17420,      .11977,      .08514,      .06224,      .03144, phsd6410
     C      .01785,      .01118,      .00766,      .00573,      .00466, phsd6420
     C      .00407,      .00377,      .00368,      .00363,      .00360, phsd6430
     C      .00359,      .00360,      .00361,      .00363,      .00365, phsd6440
     C      .00366,      .00367,      .00367,      .00369/              phsd6450
      END                                                               phsd6460
      subroutine rhoeps (iv,ibkg,salb,bkeps)
c
c     jun 93
c         this subroutine is adapted from the spectral sciences target
c     ir signature code (sstirs).
c
c         v  .le. 0    rhoeps reads the background reflectivity file
c             gt. 0    interpolates surface albed (salb) to value of v
c         ibkg         label for requested background data file
c         bkgmdl       character descriptor of background data file
c         salb         returned value of surface albedo
c         bkeps            "      "   "     "    emissivity  (not used)
c
c        the background emissivity is not used, because modtran is assuming
c     that it always equals (1 - salb).  if the user desires to directly use
c     bkeps, he must make the appropriate changes to bemiss in subroutine
c     trans plus several places in bmflux & flxadd where rupcn=salb and
c     (1-salb) is used for the surface emissivity.  (note that bkeps or
c     bemiss is not passed to bmflux & flxadd, but salb is.)
c       
c
c     file  nf09   background data file
c           nout   tape6 file
c
cc    save          vin,r,e,kdata,ibkg,bkgmdl
      character*4   header(9)
      character*15  bkgmdl(3)
      common /contrl/  iprnt
      common  /units/   nout,nf09
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
c
      dimension   vin(52),r(52,6),e(52,6),temp1(52),temp2(52)
c
      data      kmax,ndeg  / 51, 1 /
c
c ... file identifier starts in column 1
c         header  =  back   -  background rho-eps file
c                 =  end    -  end of this data set
c...the end of each reflectivity file is identified by
c     a blank card with a a non-zero integer in column one.
c
c
      nf09 = 28
      nout = ipr
      iflag = 0
c
      v = iv
      if (v .gt. 0.) go to 80
c
c              **  **  **  **  **  **  **  **  **  **  **  **  **  **
c              **  **                                          **  **
c              **  **   read  background  reflectance  data    **  **
c              **  **                                          **  **
c              **  **  **  **  **  **  **  **  **  **  **  **  **  **
c
c ... read title card (not used)
      read (nf09,'(9a4)')    header
c
c ... first, initialize various parameters
c               ntest = file number for background spectral albedos
c                   j = index for /refeps/
c              jmaxbk = number of background files
c                   k = data set number
      ntest = ibkg
      j = 1
      jmaxbk = 1
      kk = 1
c
c ... read number of background files & check validity of ntest
      read (nf09,'(i2)') jfile
      if (jfile .le. 0) jfile = 1
      nlow = 1
c                                       ** ** **  read data  ** ** **
c
c                             ... skip to requested file
      do l = nlow,jfile
cc       read (nf09,'(13x,i2,5x,3a15)',end=16) lbkg,bkgmdl  
         read (nf09,'(13x,i2)',end=16) lbkg 
         read (nf09,'(i1)'   ,end=16) nu
         if (lbkg .eq. ntest) then
            go to 20       
         else
   13       read (nf09,  '(i1)'  ,end=16) n
            if (n .eq. 0) go to 13
         end if
      end do
c
c                              ... problems -- did not find matching file
   16 write (nout,17) ntest,ibkg,l,jfile
   17 format (5('**  '),3x,'stop  in  subroutine  rhoeps',3x,5('  **')/
     &       10x,'no match was found for the requested background ',
     &       'file number (=',i3,') ---- suggest checking columns '/
     &       10x,'14 & 15 in the background data files (fortran ',
     &       'file: nf09)'/20x,'ibkg=',i4,5x,'file',i3,
     &       ' is last background file read  (of',i3,' files')
      stop  'sub.rhoeps -- background reflectance file'
c
c                                                    ... read data
   20 read(nf09,22)  jdum,vin(kk),r(kk,j),e(kk,j)
   22 format (i1,f9.0,6(2x,2f4.2))
c
c                                        ... check for end of file
      if   (jdum .eq. 0)  then
         if (kk .gt. kmax) then
            iflag = 1
         else
            kk = kk + 1
         end if
         go to 20
      end if
      kdata = kk - 1
c
c       ** ** ** **  if needed, convert to wavenumbers (cm-1) ** ** ** **
      if (nu .eq. 0) then
         do k = 1,kdata
           temp1(k) = r(k,j)
           temp2(k) = e(k,j)
         end do
         do k = 1,kdata
           kk      = kdata + 1 - k
           r(kk,j) = temp1(k)
           e(kk,j) = temp2(k)
         end do
         do k = 1,kdata
            if (vin(k) .le. 0.)
     1             stop 'rhoeps:  cannot have zero wavenumber in nf09'
            temp1(k) = 10000./vin(k)
         end do
c                                       ... round wavenumbers to 5 cm-1
         do k = 1,kdata
            kk       = kdata + 1 - k
            vin(kk) = float(5*int(.2*temp1(k)+.5))
         end do
      end if
c
c
c              **  **  **  **  **  **  **  **  **  **  **  **  **  **
c              **  **                                       **  **
c              **  **   interpolate to desired wavenumber   **  **
c              **  **                                       **  **
c              **  **  **  **  **  **  **  **  **  **  **  **  **  **
c
c
      return
   80 continue
      v     = float(iv)
      salb  = xterp(v,vin,r(1,j),ndeg,kdata,dint,ier)
      bkeps = xterp(v,vin,e(1,j),ndeg,kdata,dint,ier)
      if(v.lt.vin(1)) then
         salb  = r(1,1)
         bkeps = e(1,1)
      endif
      if(v.gt.vin(kdata)) then
         salb  = r(kdata,1)
         bkeps = e(kdata,1)
      endif
      if(salb .gt. 1.0) then
        print*,' v salb ',v,salb
        stop
      endif
c
c
      return
      end
      BLOCK DATA PRFDTA                                                 prfd 100
C>    BLOCK DATA                                                        prfd 110
C                                                                       prfd 120
C        AEROSOL PROFILE DATA                                           prfd 130
C                                                                       prfd 140
CCC         0-2KM                                                       prfd 150
CCC           HZ2K=5 VIS PROFILES- 50KM,23KM,10KM,5KM,2KM               prfd 160
CCC         >2-10KM                                                     prfd 170
CCC           FAWI50=FALL/WINTER   50KM VIS                             prfd 180
CCC           FAWI23=FALL/WINTER    23KM VIS                            prfd 190
CCC           SPSU50=SPRING/SUMMER  50KM VIS                            prfd 200
CCC           SPSU23=SPRING/SUMMER  23KM VIS                            prfd 210
CCC         >10-30KM                                                    prfd 220
CCC           BASTFW=BACKGROUND STRATOSPHERIC   FALL/WINTER             prfd 230
CCC           VUMOFW=MODERATE VOLCANIC          FALL/WINTER             prfd 240
CCC           HIVUFW=HIGH VOLCANIC              FALL/WINTER             prfd 250
CCC           EXVUFW=EXTREME VOLCANIC           FALL/WINTER             prfd 260
CCC           BASTSS,VUMOSS,HIVUSS,EXVUSS=      SPRING/SUMMER           prfd 270
CCC         >30-100KM                                                   prfd 280
CCC           UPNATM=NORMAL UPPER ATMOSPHERIC                           prfd 290
CCC           VUTONO=TRANSITION FROM VOLCANIC TO NORMAL                 prfd 300
CCC           VUTOEX=TRANSITION FROM VOLCANIC TO EXTREME                prfd 310
CCC           EXUPAT=EXTREME UPPER ATMOSPHERIC                          prfd 320
      COMMON/PRFD  /ZHT(34),HZ2K(34,5),FAWI50(34),FAWI23(34),SPSU50(34),prfd 330
     1SPSU23(34),BASTFW(34),VUMOFW(34),HIVUFW(34),EXVUFW(34),BASTSS(34),prfd 340
     2VUMOSS(34),HIVUSS(34),EXVUSS(34),UPNATM(34),VUTONO(34),           prfd 350
     3VUTOEX(34),EXUPAT(34)                                             prfd 360
      DATA ZHT/                                                         prfd 370
     *    0.,    1.,    2.,    3.,    4.,    5.,    6.,    7.,    8.,   prfd 380
     *    9.,   10.,   11.,   12.,   13.,   14.,   15.,   16.,   17.,   prfd 390
     *   18.,   19.,   20.,   21.,   22.,   23.,   24.,   25.,   30.,   prfd 400
     *   35.,   40.,   45.,   50.,   70.,  100.,99999./                 prfd 410
       DATA HZ2K(1,1),HZ2K(1,2),HZ2K(1,3),HZ2K(1,4),HZ2K(1,5)/          prfd 420
     1 6.62E-02, 1.58E-01, 3.79E-01, 7.70E-01, 1.94E+00/                prfd 430
       DATA HZ2K(2,1),HZ2K(2,2),HZ2K(2,3),HZ2K(2,4),HZ2K(2,5)/          prfd 440
     1 4.15E-02, 9.91E-02, 3.79E-01, 7.70E-01, 1.94E+00/                prfd 450
       DATA HZ2K(3,1),HZ2K(3,2),HZ2K(3,3),HZ2K(3,4),HZ2K(3,5)/          prfd 460
     1 2.60E-02, 6.21E-02, 6.21E-02, 6.21E-02, 6.21E-02/                prfd 470
      DATA FAWI50  /3*0.,                                               prfd 480
     1 1.14E-02, 6.43E-03, 4.85E-03, 3.54E-03, 2.31E-03, 1.41E-03,      prfd 490
     2 9.80E-04,7.87E-04,23*0./                                         prfd 500
      DATA FAWI23              /3*0.,                                   prfd 510
     1 2.72E-02, 1.20E-02, 4.85E-03, 3.54E-03, 2.31E-03, 1.41E-03,      prfd 520
     2 9.80E-04,7.87E-04,23*0./                                         prfd 530
      DATA  SPSU50              / 3*0.,                                 prfd 540
     1 1.46E-02, 1.02E-02, 9.31E-03, 7.71E-03, 6.23E-03, 3.37E-03,      prfd 550
     2 1.82E-03  ,1.14E-03,23*0./                                       prfd 560
      DATA  SPSU23              / 3*0.,                                 prfd 570
     1 3.46E-02, 1.85E-02, 9.31E-03, 7.71E-03, 6.23E-03, 3.37E-03,      prfd 580
     2 1.82E-03  ,1.14E-03,23*0./                                       prfd 590
      DATA BASTFW       /11*0.,                                         prfd 600
     1           7.14E-04, 6.64E-04, 6.23E-04, 6.45E-04, 6.43E-04,      prfd 610
     2 6.41E-04, 6.00E-04, 5.62E-04, 4.91E-04, 4.23E-04, 3.52E-04,      prfd 620
     3 2.95E-04, 2.42E-04, 1.90E-04, 1.50E-04, 3.32E-05 ,7*0./          prfd 630
      DATA    VUMOFW       /11*0.,                                      prfd 640
     1           1.79E-03, 2.21E-03, 2.75E-03, 2.89E-03, 2.92E-03,      prfd 650
     2 2.73E-03, 2.46E-03, 2.10E-03, 1.71E-03, 1.35E-03, 1.09E-03,      prfd 660
     3 8.60E-04, 6.60E-04, 5.15E-04, 4.09E-04, 7.60E-05 ,7*0./          prfd 670
      DATA    HIVUFW       /11*0.,                                      prfd 680
     1           2.31E-03, 3.25E-03, 4.52E-03, 6.40E-03, 7.81E-03,      prfd 690
     2 9.42E-03, 1.07E-02, 1.10E-02, 8.60E-03, 5.10E-03, 2.70E-03,      prfd 700
     3 1.46E-03, 8.90E-04, 5.80E-04, 4.09E-04, 7.60E-05 ,7*0./          prfd 710
      DATA    EXVUFW       /11*0.,                                      prfd 720
     1           2.31E-03, 3.25E-03, 4.52E-03, 6.40E-03, 1.01E-02,      prfd 730
     2 2.35E-02, 6.10E-02, 1.00E-01, 4.00E-02, 9.15E-03, 3.13E-03,      prfd 740
     3 1.46E-03, 8.90E-04, 5.80E-04, 4.09E-04, 7.60E-05 ,7*0./          prfd 750
      DATA    BASTSS       /11*0.,                                      prfd 760
     1           7.99E-04, 6.41E-04, 5.17E-04, 4.42E-04, 3.95E-04,      prfd 770
     2 3.82E-04, 4.25E-04, 5.20E-04, 5.81E-04, 5.89E-04, 5.02E-04,      prfd 780
     3 4.20E-04, 3.00E-04, 1.98E-04, 1.31E-04, 3.32E-05 ,7*0./          prfd 790
      DATA    VUMOSS       /11*0.,                                      prfd 800
     1           2.12E-03, 2.45E-03, 2.80E-03, 2.89E-03, 2.92E-03,      prfd 810
     2 2.73E-03, 2.46E-03, 2.10E-03, 1.71E-03, 1.35E-03, 1.09E-03,      prfd 820
     3 8.60E-04, 6.60E-04, 5.15E-04, 4.09E-04, 7.60E-05 ,7*0./          prfd 830
      DATA    HIVUSS       /11*0.,                                      prfd 840
     1           2.12E-03, 2.45E-03, 2.80E-03, 3.60E-03, 5.23E-03,      prfd 850
     2 8.11E-03, 1.20E-02, 1.52E-02, 1.53E-02, 1.17E-02, 7.09E-03,      prfd 860
     3 4.50E-03, 2.40E-03, 1.28E-03, 7.76E-04, 7.60E-05 ,7*0./          prfd 870
      DATA    EXVUSS       /11*0.,                                      prfd 880
     1           2.12E-03, 2.45E-03, 2.80E-03, 3.60E-03, 5.23E-03,      prfd 890
     2 8.11E-03, 1.27E-02, 2.32E-02, 4.85E-02, 1.00E-01, 5.50E-02,      prfd 900
     3 6.10E-03, 2.40E-03, 1.28E-03, 7.76E-04, 7.60E-05 ,7*0./          prfd 910
      DATA UPNATM       /26*0.,                                         prfd 920
     1 3.32E-05, 1.64E-05, 7.99E-06, 4.01E-06, 2.10E-06, 1.60E-07,      prfd 930
     2 9.31E-10, 0.      /                                              prfd 940
      DATA VUTONO       /26*0.,                                         prfd 950
     1 7.60E-05, 2.45E-05, 7.99E-06, 4.01E-06, 2.10E-06, 1.60E-07,      prfd 960
     2 9.31E-10, 0.      /                                              prfd 970
      DATA VUTOEX       /26*0.,                                         prfd 980
     1 7.60E-05, 7.20E-05, 6.95E-05, 6.60E-05, 5.04E-05, 1.03E-05,      prfd 990
     2 4.50E-07, 0.      /                                              prfd1000
      DATA EXUPAT       /26*0.,                                         prfd1010
     1 3.32E-05, 4.25E-05, 5.59E-05, 6.60E-05, 5.04E-05, 1.03E-05,      prfd1020
     2 4.50E-07, 0.      /                                              prfd1030
      END                                                               prfd1040
      FUNCTION   PSI(PSIO,DELO,BETA,IARB,IARBO)                         psi  100
C                                                                       psi  110
C     FUNCTION PSI RETURNS THE VALUE OF SOLAR AZIMUTH RELATIVE TO       psi  120
C     THE LINE OF SIGHT, AT THE CURRENT SCATTERING LOCATION             psi  130
      COMMON /PARMTR/ RE,DELTAS,ZMAX,IMAX,IMOD,IBMAX,IPATH              psi  140
      COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     psi  150
      DATA  EPSILN/1.0E-5/                                              psi  160
      DELOR=DELO/DEG                                                    psi  170
      BETAR=BETA/DEG                                                    psi  180
      IF(IARBO.EQ.0) GO TO 5                                            psi  190
C     SPECIAL CASES WHEN PSIO IS ARBITRARY                              psi  200
      IARB=IARBO                                                        psi  210
      PSI = 0.
      IF(IARBO.EQ.1.OR.IARBO.EQ.3) RETURN                               psi  220
      IF(BETA.LE.EPSILN) RETURN                                         psi  230
C     PSI=180.0 (MOVED OUT FROM UNDER THE SUN)                          psi  240
      IARB=0                                                            psi  250
      PSI=180.0                                                         psi  260
      RETURN                                                            psi  270
5     CONTINUE                                                          psi  280
C     GENERAL CASE                                                      psi  290
      PSIOR=PSIO/DEG                                                    psi  300
      IARB=0                                                            psi  310
      ANUMER=SIN(DELOR)*SIN(PSIOR)                                      psi  320
      DENOM=COS(BETAR)*SIN(DELOR)*COS(PSIOR)-SIN(BETAR)*COS(DELOR)      psi  330
C     SPECIAL CASES                                                     psi  340
C     NUMERATOR GOES TO ZERO IN THE FOLLOWING 3 CASES                   psi  350
C 1)  DELO=0.0                                                          psi  360
      IF(DELO.GT.EPSILN) GO TO 20                                       psi  370
      IF(BETA.GT.EPSILN) GO TO 10                                       psi  380
      IARB=2                                                            psi  390
      RETURN                                                            psi  400
10    PSI=180.0                                                         psi  410
      RETURN                                                            psi  420
C 2)  PSIO=0.0                                                          psi  430
20    IF(ABS(PSIO).GT.EPSILN) GO TO 40                                  psi  440
      IF(ABS(BETA-DELO).GE.EPSILN) GO TO 30                             psi  450
C     SCATTERING POINT IS DIRECTLY UNDER THE SUN                        psi  460
      IARB=2                                                            psi  470
      RETURN                                                            psi  480
30    IF(BETA.LT.DELO) PSI=0.0                                          psi  490
      IF(BETA.GT.DELO) PSI=180.0                                        psi  500
      RETURN                                                            psi  510
C 3)  PSIO=180.0                                                        psi  520
40    IF(ABS(PSIO).LT.(180.0-EPSILN)) GO TO 60                          psi  530
      PSI=180.0                                                         psi  540
      RETURN                                                            psi  550
60    CONTINUE                                                          psi  560
C     DENOMINATOR CAN GO TO ZERO FOR THE FOLLOWING 2 CASES              psi  570
C 1)  BETA=DELO AND PSIO=0.0                                            psi  580
C     THIS CASE WAS HANDLED EARLIER                                     psi  590
C 2)  GENERAL CASE                                                      psi  600
      IF(ABS(DENOM).GT.EPSILN) GO TO 80                                 psi  610
      IF(PSIO.LT.0.0) PSI=-90.0                                         psi  620
      IF(PSIO.GT.0.0) PSI=90.0                                          psi  630
      RETURN                                                            psi  640
80    CONTINUE                                                          psi  650
      PSI=DEG*ATAN(ANUMER/DENOM)                                        psi  660
C     NOTE ATAN RETURNS ARGUMENTS BETWEEN -90 AND 90, PSI               psi  670
C     AND PSIO SHOULD BE OF THE SAME SIGN.                              psi  680
      IF(PSIO.GT.0.0.AND.PSI.LT.0.0) PSI=PSI+180.                       psi  690
      IF(PSIO.LT.0.0.AND.PSI.GT.0.0) PSI=PSI-180.                       psi  700
      RETURN                                                            psi  710
      END                                                               psi  720
cjv 11/95 Replace subroutine PSIDEL with PSIECA.
      SUBROUTINE PSIECA(OLAT,OLONG,SLAT,SLONG,PSI,ECA)
C
C     THIS ROUTINE CALCULATES THE EARTH CENTER ANGLE AND THE PATH
C     AZIMUTH FROM AN OBSERVER TO A SOURCE POINT GIVEN LATITUDE AND
C     LONGITUDE INFORMATION.
C
C     INPUT
C       OLAT   OBSERVER LATITUDE [DEG NORTH, -90 TO 90]
C       OLONG  OBSERVER LONGITUDE [DEG WEST, 0 TO 360]
C       SLAT   SOURCE POINT LATITUDE [DEG NORTH, -90 TO 90]
C       SLONG  SOURCE POINT LONGITUDE [DEG WEST, 0 TO 360]
C
C     OUTPUT
C       PSI    PATH AZIMUTH AT OBSERVER [DEG EAST OF NORTH, -180 TO 180]
C       ECA    EARTH CENTER ANGLE [DEG, 0 TO 180]
C
C     NOTE:  OLONG AND PSI ARE NOT WELL DEFINED AT THE NORTH (OLAT=90)
C            AND SOUTH (OLAT=-90) POLES.  THIS ROUTINE RETURNS THE
C            VALUE OF PATH AZIMUTH OBTAINED IN THE LIMIT AS |OLAT|
C            APPROACHES 90 FROM BELOW.
C
      DATA DEG/57.2957795/
C
C     TREAT SAME LONGITUDE AS SPECIAL CASE
      IF(OLONG.EQ.SLONG)THEN
          IF(OLAT.LE.SLAT)THEN
              ECA=SLAT-OLAT
              PSI=0.
          ELSE
              ECA=OLAT-SLAT
              PSI=180.
          ENDIF
          RETURN
      ENDIF
      SOLAT=SIN(OLAT/DEG)
      COLAT=COS(OLAT/DEG)
      SSLAT=SIN(SLAT/DEG)
      CSLAT=COS(SLAT/DEG)
      DLONG=(OLONG-SLONG)/DEG
      SDLONG=SIN(DLONG)
      CDLONG=COS(DLONG)
      COSECA=COLAT*CSLAT*CDLONG+SOLAT*SSLAT
      IF(COSECA.GE.1.)THEN
          ECA=0.
      ELSEIF(COSECA.GT.-1.)THEN
          ECA=DEG*ACOS(COSECA)
      ELSE
          ECA=180.
      ENDIF
      X=COLAT*SSLAT-SOLAT*CSLAT*CDLONG
      Y=CSLAT*SDLONG
      IF(X.EQ.0.)THEN
          PSI=90.
          IF(Y.LT.0.)PSI=-90.
      ELSE
          PSI=DEG*ATAN(Y/X)
          IF(X.LT.0)PSI=PSI+180.
          IF(PSI.GT.180.)PSI=PSI-360.
      ENDIF
      RETURN
      END
cjv ^
      SUBROUTINE PSLCT(H1,H2,ANGLE,RANGE,BETA,ISLCT)                    pslc 100
C                                                                       pslc 110
C     SOME, IF NOT ALL, ITYPE = 2 PATHS WILL ENTER THIS ROUTINE.        pslc 120
C     THEY WILL RETURN WITH ISLCT = 21, IF IT IS A CASE 2A PATH,        pslc 130
C     --------------------- ISLCT = 22, IF IT IS A CASE 2B PATH,        pslc 140
C     --------------------- ISLCT = 23, IF IT IS A CASE 2C PATH,        pslc 150
C     --------------------- ISLCT = 24, IF IT IS A CASE 2D PATH.        pslc 160
C                                                                       pslc 170
C     H1     H2     ANGLE     RANGE      BETA        CASE  ISLCT        pslc 180
C--------------------------------------------     ----------------      pslc 190
C     X      X      X                                 2A     21         pslc 200
C                                                                       pslc 210
C     X             X         X                       2B     22         pslc 220
C                                                                       pslc 230
C     X      X                X                       2C     23         pslc 240
C                                                                       pslc 250
C     X      X                           X            2D     24         pslc 260
C                                                                       pslc 270
      INTEGER ISLCT, IRD,IPR,IPU,NPR,IPR1                               pslc 280
      REAL H1, H2, ANGLE, RANGE, BETA                                   pslc 290
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      IF (RANGE .GT. 0.0) THEN                                          pslc 310
C        MUST BE EITHER CASE 2B OR 2C; ALSO RANGE CANNOT BE ZERO FOR 2B pslc 320
         IF (H2 .GT. 0) THEN                                            pslc 330
C           CASE 2C                                                     pslc 340
            ISLCT = 23                                                  pslc 350
         ELSEIF (ANGLE .EQ. 0) THEN                                     pslc 360
            WRITE(IPR,*)                                                pslc 370
            WRITE(IPR,*)'FROM PSLCT: '                                  pslc 380
            WRITE(IPR,*)'H1 AND RANGE ARE BOTH GREATER THAN ZERO,'      pslc 390
            WRITE(IPR,*)'BUT H2 AND ANGLE BOTH EQUAL ZERO.'             pslc 400
            WRITE(IPR,*)'PATH CAN BE EITHER 2B OR 2C.'                  pslc 410
            WRITE(IPR,*)'ASSUMED TO BE 2c.'                             pslc 420
            WRITE(IPR,*)'IF CASE 2b IS DESIRED, THE VALUE OF ISLCT'     pslc 430
            WRITE(IPR,*)'MUST BE CHANGED FROM 23 TO 22.'                pslc 440
            ISLCT = 23                                                  pslc 450
         ELSE                                                           pslc 460
C           DEFINITELY CASE 2B                                          pslc 470
            ISLCT = 22                                                  pslc 480
         ENDIF                                                          pslc 490
      ELSEIF (BETA .GT. 0.0) THEN                                       pslc 500
C        MUST BE CASE 2D; ALSO BETA CANNOT BE ZERO FOR 2D               pslc 510
         ISLCT = 24                                                     pslc 520
      ELSE                                                              pslc 530
C        IF RANGE AND BETA BOTH ARE ZERO, CASE 2A                       pslc 540
         ISLCT = 21                                                     pslc 550
      ENDIF                                                             pslc 560
      RETURN                                                            pslc 570
      END                                                               pslc 580
      SUBROUTINE ratcsz
      include 'parameter.list'
      COMMON/CNSTNS/PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                       tras 240
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1                                 FLX  230
      COMMON/CNTRL/KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT                 loop 240
      COMMON /MODEL/ ZM(LAYDIM),PM(LAYDIM),TM(LAYDIM),dum(LAYDIM),
     1  DENSTY(65,LAYDIM),CLDAMT(LAYDIM),RRAMT(LAYDIM),EQLWC(LAYDIM),
     1  HAZEC(LAYDIM)
      COMMON/SOLS/AH1(LAYTWO),ARH(LAYTWO),WPATHS(LAYTHR,65),
     1 PA(LAYTWO),PR(LAYTWO),ATHETA(LAYDIM+1),ADBETA(LAYDIM+1),
     2 LJ(LAYTWO+1),JTURN,ANGSUN,CSZEN(LAYTWO),TBBYS(LAYTHR,12),
     3 PATMS(LAYTHR,12)
      DATA PZERO /1013.25/,TZERO/273.15/,XLOSCH/2.68675E24/             STD  340
      DATA AVOGAD/6.022045E+23/,ALOSMT/2.68675E+19/,                    aern 770
     1    GASCON/8.31441E+7/,PLANK/6.626176E-27/,BOLTZ/1.380662E-16/,   aern 780
     2    CLIGHT/2.99792458E10/                                         aern 790
      data avms /28.966/ ,grav /980./,dyne /1000./ 
C     avcon =  AVOGAD * dyne / (avms * grav * XLOSCH * .9988684 )
      avcon =  AVOGAD * dyne / (avms * grav * XLOSCH * .9961450)
      re = 6371.15
      P100=PM(ML) * AVCON * (RE/(RE+100.))**2
      CSZENH=COS(ANGSUN*CA)
      do 1300 i=1,ikmax
      cszen(i)=-5.
      rcora  = re /(re+zm(i))
      rcor = rcora**2 
      PP=PM(I) * avcon * rcor 
c     PP=PM(I) * avcon * rcor - P100
      wp = wpaths(i+laytwo,6)
      if(wp.lt. 0.)go to 1300	 
c     print 15, i, zm(i),pp,wp
      if(wp.ge.pp.and.wp.gt.0.) then
          ratio = pp  / wp
          if(ratio. gt. 0.)cszen (i) = ratio
      else
          CSZEN(I)=CSZENH
      endif
1300  continue 
C     stop  
      END                                                               FLX 4830
      subroutine rain(v,ext,abt,sct,asymr)                              rain 100
      integer phase,dist                                                rain 110
      include 'parameter.list'
      COMMON RELHUM(laydim),HSTOR(laydim),ICH(4),VH(17),TX(65),W(65)  
      COMMON IMSMX,WPATH(laythr,65),TBBY(laythr),PATM(laythr),NSPEC,   
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX2(47),AWCCON(5)  
      COMMON/CNSTNS/PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                       rain 150
      COMMON/CARD2/IHAZE,ISEASN,IVULCN,ICSTL,ICLD,IVSA,VIS,WSS,WHH,     rain 160
     1  RAINRT                                                          rain 170
      TMPRN=W(61)/W(3)                                                  rain 180
      vtemp=1.438786*v                                                  rain 190
      IF(VTEMP.GE.TMPRN*BIGEXP)THEN                                     rain 200
          RFD=V                                                         rain 210
      ELSE                                                              rain 220
          store=EXP(-VTEMP/TMPRN)                                       rain 230
          RFD=V*(1.-store)/(1.+store)                                   rain 240
      ENDIF                                                             rain 250
      RAINAV=(W(3)/W(62))**(1./.63)                                     rain 260
      IF(V.LT.250.)THEN                                                 rain 270
          PHASE=1                                                       rain 280
          IF(ICLD.GT.11)PHASE=2                                         rain 290
          DIST=1                                                        rain 300
C                                                                       rain 310
C         CALL SCATTERING ROUTINE TO OBTAIN ASYMMETRY                   rain 320
C         FACTOR AND RATIO OF ABSORPTION TO EXTINCTION DUE              rain 330
C         TO RAIN WITHIN RANGE OF 19 TO 231 GHZ.                        rain 340
C         EXTRAPOLATE ABOVE AND BELOW THAT FREQ RANGE                   rain 350
          CALL RNSCAT(V,RAINAV,TMPRN,PHASE,DIST,CSSA,ASYMR)             rain 360
      ELSE                                                              rain 370
          CSSA=.5                                                       rain 380
          ASYMR=.85                                                     rain 390
      ENDIF                                                             rain 400
      rnexph=TNRAIN(RAINAV,V,TMPRN,RFD)*w(62)                           rain 410
      ext=rnexph                                                        rain 420
      ABT=rnexph*CSSA                                                   rain 430
      sct=rnexph*(1.-cssa)                                              rain 440
      return                                                            rain 450
      end                                                               rain 460
      FUNCTION RANDOM(I)                                                rain 470
      DOUBLE PRECISION DRND,Z                                           rain 480
      DATA DRND/0.574914365D0/                                          rain 490
      DRND=97301.D0*DRND                                                rain 500
      K=DRND                                                            rain 510
      Z=K                                                               rain 520
      DRND=DRND-Z                                                       rain 530
      RANDOM=DRND                                                       rain 540
      RETURN                                                            rain 550
      END                                                               rain 560
      FUNCTION   RANFUN(IDUM)                                           ranf 100
      RANFUN=RANDOM(IDUM)                                               ranf 110
      RETURN                                                            ranf 120
      END                                                               ranf 130
      SUBROUTINE RANSET(I)                                              rans 100
      COMMON/RST/II                                                     rans 110
CSSI  RANET=RAN(I)                                                      rans 120
      RANET=RANDOM(I)                                                   rans 130
      II=RANET                                                          rans 140
      RETURN                                                            rans 150
      END                                                               rans 160
cc    FUNCTION   RAYSCT(V)                                              rays 100
C     RADIATION FLD OUT                                                 rays 110
C **  MOLECULAR SCATTERING                                              rays 120
cc    RAYSCT=0.                                                         rays 130
cc    IF(V.LE.3000.) RETURN                                             rays 140
cc    RAYSCT=V**3/(9.26799E+18-1.07123E+09*V**2)                        rays 150
C     V**4 FOR RADIATION FLD IN                                         rays 160
cc    RETURN                                                            rays 170
cc    END                                                               rays 180
      SUBROUTINE RDEXA                                                  rays 190
C                                                                       rays 200
C     READ IN USER DEFINED EXTINCTION ABSORPTION COEFFICIENTS AND       rays 210
C     ASYMMETRY PARAMETERS                                              rays 220
C                                                                       rays 230
      include 'parameter.list'
      COMMON RELHUM(laydim),HSTOR(laydim),ICH(4),VH(17),TX(65),W(65)  
      COMMON IMSMX,WPATH(laythr,65),TBBY(laythr),PATM(laythr),NSPEC,   
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX2(47),AWCCON(5)  
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON /CARD2D/ IREG(4),ALTB(4),IREGC(4)                          rays 290
      DIMENSION TITLE(18),VX(47)                                        rays 300
C                                                                       rays 310
      READ (IRD,1200) (IREG(IK),IK=1,4)                                 rays 320
1200  FORMAT(4I5)                                                       rays 330
      WRITE(IPR,1210) (IREG(IK),IK=1,4)                                 rays 340
1210  FORMAT('0 CARD 2D *****',4I5)                                     rays 350
C                                                                       rays 360
      DO 1300 IHC = 1,4                                                 rays 370
C                                                                       rays 380
      IF(IREG(IHC) .EQ. 0) GO TO 1300                                   rays 390
      READ(IRD,1220) AWCCON(IHC),TITLE                                  rays 400
1220  FORMAT(E10.3,18A4)                                                rays 410
      WRITE(IPR,1230) AWCCON(IHC),TITLE                                 rays 420
1230  FORMAT('0 CARD 2D1 **** EQUIVALENT WATER = ',1PE10.3,18A4)        rays 430
      WRITE(IPR,1250)                                                   rays 440
1250  FORMAT('0 CARD 2D2 ****')                                         rays 450
C                                                                       rays 460
      READ(IRD,1260)(VX(I),EXTC(IHC,I),ABSC(IHC,I),ASYM(IHC,I),I=1,47)  rays 470
1260  FORMAT(3(F6.2,2F7.5,F6.4))                                        rays 480
      WRITE(IPR,1270)(VX(I),EXTC(IHC,I),ABSC(IHC,I),ASYM(IHC,I),I=1,47) rays 490
1270  FORMAT(2X,F6.2,2F7.5,F6.4,F6.2,2F7.5,F6.4,F6.2,2F7.5,F6.4)        rays 500
1300  CONTINUE                                                          rays 510
      RETURN                                                            rays 520
      END                                                               rays 530
      SUBROUTINE RDNSM                                                  rdns 100
C                                                                       rdns 110
C     THIS SUBROUTINE READS MODEL 7 DATA WHEN ISVA EQ 1                 rdns 120
C                                                                       rdns 130
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON /CARD1/ MODEL,ITYPE,IEMSCT,M1,M2,M3,IM,NOPRNT,TBOUND,SALB  rdns 150
     1  ,MODTRN                                                         rdns 160
      LOGICAL MODTRN                                                    rdns 170
      COMMON /CARD1A/ M4,M5,M6,MDEF,IRD1,IRD2                           rdns 180
      COMMON /CARD1B/ JUNIT(15),WMOL(12),WAIR1,JLOW                     rdns 190
      COMMON /CARD2/ IHAZE,ISEASN,IVULCN,ICSTL,ICIR,IVSA,VIS,WSS,WHH,   rdns 200
     1    RAINRT                                                        rdns 210
      COMMON /CNTRL/ KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT               rdns 220
      COMMON /NSINP/ ZMDL(40),P(40),T(40),WMDL(40,13)                   rdns 230
      CHARACTER*1 JCHAR                                                 rdns 240
      DIMENSION  JCHAR(15)                                              rdns 250
      IF(ML. GT. 24) THEN                                               rdns 260
         WRITE(IPR,900) ML                                              rdns 270
900      FORMAT('  ML = ',I5,'  GT 24 ML RESET TO 24')                  rdns 280
         ML = 24                                                        rdns 290
      ENDIF                                                             rdns 300
      JLOW = 1                                                          rdns 310
      DO 200   K=1,ML                                                   rdns 320
      DO 10 KM = 1,15                                                   rdns 330
      JCHAR(KM) = ' '                                                   rdns 340
      IF(KM. GT. 12) GO TO 10                                           rdns 350
      WMOL(KM) = 0.                                                     rdns 360
10    CONTINUE                                                          rdns 370
      IRD0 = 1                                                          rdns 380
      ICONV = 1                                                         rdns 390
      IF((MODEL .GT. 0.) .AND. (MODEL .LT. 7)) IRD0 = 0                 rdns 400
      IF((IRD0  .EQ. 1)  .AND. (IVSA.EQ.1)   ) THEN                     rdns 410
           IRD0 = 0                                                     rdns 420
           ICONV =0                                                     rdns 430
      ENDIF                                                             rdns 440
C                                                                       rdns 450
C        PARAMETERS - JCHAR = INPUT KEY (SEE BELOW)                     rdns 460
C                                                                       rdns 470
C                                                                       rdns 480
C     ***  ROUTINE ALSO ACCEPTS VARIABLE UNITS ON PRESS AND TEMP        rdns 490
C                                                                       rdns 500
C          SEE INPUT KEY BELOW                                          rdns 510
C                                                                       rdns 520
C                                                                       rdns 530
C                                                                       rdns 540
C     FOR MOLECULAR SPECIES ONLY                                        rdns 550
C                                                                       rdns 560
C       JCHAR   JUNIT                                                   rdns 570
C                                                                       rdns 580
C     " ",A      10    VOLUME MIXING RATIO (PPMV)                       rdns 590
C         B      11    NUMBER DENSITY (CM-3)                            rdns 600
C         C      12    MASS MIXING RATIO (GM(K)/KG(AIR))                rdns 610
C         D      13    MASS DENSITY (GM M-3)                            rdns 620
C         E      14    PARTIAL PRESSURE (MB)                            rdns 630
C         F      15    DEW POINT TEMP (TD IN T(K)) - H2O ONLY           rdns 640
C         G      16     "    "     "  (TD IN T(C)) - H2O ONLY           rdns 650
C         H      17    RELATIVE HUMIDITY (RH IN PERCENT) - H2O ONLY     rdns 660
C         I      18    AVAILABLE FOR USER DEFINITION                    rdns 670
C        1-6    1-6    DEFAULT TO SPECIFIED MODEL ATMOSPHERE            rdns 680
C                                                                       rdns 690
C     ****************************************************************  rdns 700
C     ****************************************************************  rdns 710
C                                                                       rdns 720
C     ***** OTHER 'JCHAR' SPECIFICATIONS -                              rdns 730
C                                                                       rdns 740
C       JCHAR   JUNIT                                                   rdns 750
C                                                                       rdns 760
C      " ",A     10    PRESSURE IN (MB)                                 rdns 770
C          B     11       "     "  (ATM)                                rdns 780
C          C     12       "     "  (TORR)                               rdns 790
C         1-6   1-6    DEFAULT TO SPECIFIED MODEL ATMOSPHERE            rdns 800
C                                                                       rdns 810
C      " ",A     10    AMBIENT TEMPERATURE IN DEG(K)                    rdns 820
C          B     11       "         "       "  " (C)                    rdns 830
C          C     12       "         "       "  " (F)                    rdns 840
C         1-6   1-6    DEFAULT TO SPECIFIED MODEL ATMOSPHERE            rdns 850
C                                                                       rdns 860
C     ***** DEFINITION OF "DEFAULT" CHOICES FOR PROFILE SELECTION ***** rdns 870
C                                                                       rdns 880
C      FOR THE USER WHO WISHES TO ENTER ONLY SELECTED ORIGINAL          rdns 890
C      VERTICAL PROFILES AND WANTS STANDARD ATMOSPHERE SPECIFICATIONS   rdns 900
C      FOR THE OTHERS, THE FOLLOWING OPTION IS AVAILABLE                rdns 910
C                                                                       rdns 920
C     *** JCHAR(P,T OR K) MUST = 1-6 (AS ABOVE)                         rdns 930
C                                                                       rdns 940
C      FOR MOLECULES 8-35, ONLY US STD PROFILES ARE AVIALABLE           rdns 950
C      THEREFORE, WHEN  'JCHAR(K) = 1-5', JCHAR(K) WILL BE RESET TO 6   rdns 960
C                                                                       rdns 970
C                                                                       rdns 980
      READ(IRD,80)ZMDL(K),P(K),T(K),WMOL(1),WMOL(2),WMOL(3),            rdns 990
     X (JCHAR(KM),KM=1,15)                                              rdns1000
80    FORMAT ( F10.3,5E10.3,15A1)                                       rdns1010
       WRITE(IPR,81)ZMDL(K),P(K),T(K),WMOL(1),WMOL(2),WMOL(3),          rdns1020
     X (JCHAR(KM),KM=1,15)                                              rdns1030
81    FORMAT ( F10.3,1P5E10.3,10X,15A1)                                 rdns1040
      IF(ZMDL(K) .LE. 2.0)JLOW = K                                      rdns1050
      IF(IRD1 .EQ. 1) THEN                                              rdns1060
           READ(IRD,83)(WMOL(KM),KM=4,12)                               rdns1070
83         FORMAT(8E10.3)                                               rdns1080
           WRITE(IPR,84)(WMOL(KM),KM=4,12)                              rdns1090
84         FORMAT(1P8E10.3)                                             rdns1100
      ENDIF                                                             rdns1110
C                                                                       rdns1120
C                                                                       rdns1130
C     AHAZE =  AEROSOL VISIBLE EXTINCTION COFF (KM-1)                   rdns1140
C     AT A WAVELENGTH OF 0.55 MICROMETERS                               rdns1150
C                                                                       rdns1160
C     EQLWCZ=LIQUID WATER CONTENT (PPMV) AT ALT Z                       rdns1170
C            FOR AEROSOL, CLOUD OR FOG MODELS                           rdns1180
C                                                                       rdns1190
C     RRATZ=RAIN RATE (MM/HR) AT ALT Z                                  rdns1200
C                                                                       rdns1210
C     IHA1 AEROSOL MODEL USED FOR SPECTRAL DEPENDENCE OF EXTINCTION     rdns1220
C                                                                       rdns1230
C     IVUL1 STRATOSPHERIC AERSOL MODEL USED FOR SPECTRAL DEPENDENCE     rdns1240
C     OF EXT AT Z                                                       rdns1250
C                                                                       rdns1260
C     ICLD1 CLOUD MODEL USED FOR SPECTRAL DEPENDENCE OF EXT AT Z        rdns1270
C                                                                       rdns1280
C     ONLY ONE OF IHA1,ICLD1  OR IVUL1 IS ALLOWED                       rdns1290
C     IHA1 NE 0 OTHERS IGNORED                                          rdns1300
C     IHA1 EQ 0 AND ICLD1 NE 0 USE ICLD1                                rdns1310
C                                                                       rdns1320
C     IF AHAZE AND EQLWCZ ARE BOUTH ZERO                                rdns1330
C        DEFAULT PROFILE ARE LOADED FROM IHA1,ICLD1,IVUL1               rdns1340
C     ISEA1 = AERSOL SEASON CONTROL FOR ALTITUDE Z                      rdns1350
C                                                                       rdns1360
      IF(IRD2 .EQ. 1) THEN                                              rdns1370
           READ(IRD,82)    AHAZE,EQLWCZ,RRATZ,IHA1,ICLD1,IVUL1,ISEA1    rdns1380
           WRITE(IPR,82)    AHAZE,EQLWCZ,RRATZ,IHA1,ICLD1,IVUL1,ISEA1   rdns1390
82         FORMAT(10X,3F10.3,4I5)                                       rdns1400
      ENDIF                                                             rdns1410
      DO 12 KM = 1,15                                                   rdns1420
12    JUNIT(KM) = JOU(JCHAR(KM))                                        rdns1430
      IF(M1 .NE. 0) JUNIT(1) = M1                                       rdns1440
      IF(M1 .NE. 0) JUNIT(2) = M1                                       rdns1450
      CALL CHECK(P(K),JUNIT(1),1)                                       rdns1460
      CALL CHECK(T(K),JUNIT(2),2)                                       rdns1470
      CALL DEFALT(ZMDL(K),P(K),T(K))                                    rdns1480
      CALL CONVRT (P(K),T(K) )                                          rdns1490
      DO 20 KM = 1,12                                                   rdns1500
20    WMDL(K,KM) = WMOL(KM)                                             rdns1510
      WMDL(K,13) = WAIR1                                                rdns1520
200   CONTINUE                                                          rdns1530
      RETURN                                                            rdns1540
      END                                                               rdns1550
      SUBROUTINE REDUCE(H1,H2,ANGLE,PHI,ITER)                           redu 100
C***********************************************************************redu 110
C     ZMAX IS THE HIGHEST LEVEL IN THE ATMOSPHERIC PROFILE STORED IN    redu 120
C     COMMON /MODEL/.  IF H1 AND/OR H2 ARE GREATER THAN ZMAX, THIS      redu 130
C     SUBROUTINE REDUCES THEM TO ZMAX AND RESETS ANGLE AND/OR PHI       redu 140
C     AS NECESSARY. THIS REDUCTION IS NECESSARY,FOR EXAMPLE FOR         redu 150
C     SATELLITE ALTITUDES, BECAUSE (1) THE DENSITY PROFILES ARE         redu 160
C     POORLY DEFINED ABOVE ZMAX AND (2) THE CALCULATION TIME FOR        redu 170
C     PATHS ABOVE ZMAX CAN BE EXCESSIVE ( EG. FOR GEOSYNCHRONOUS        redu 180
C     ALTITUDES)                                                        redu 190
C***********************************************************************redu 200
      real re, deltas,zmax,PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                redu 210
      integer ird, ipr, ipu, npr, ipr1, imax, imod,ibmax,ipath,iter     redu 220
      double precision h1, h2, angle, phi,DPANDX,cpath,czmax,angmax,    redu 230
     $     sh,gamma                                                     redu 240
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON /PARMTR/ RE,DELTAS,ZMAX,IMAX,IMOD,IBMAX,IPATH              redu 260
      COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     redu 270
      IF(H1.LE.ZMAX .AND. H2.LE.ZMAX)  RETURN                           redu 280
      CALL DPFISH(H1,SH,GAMMA)                                          redu 290
      CPATH = DPANDX(H1,SH,GAMMA)*(RE+H1)*SIN(ANGLE/DEG)                redu 300
      CALL dpfish(dble(ZMAX),SH,GAMMA)                                  redu 310
      CZMAX = DPANDX(dble(ZMAX),SH,GAMMA)*(RE+ZMAX)                     redu 320
      ANGMAX = 180.0-ASIN(CPATH/CZMAX)*DEG                              redu 330
      IF(H1.LE.ZMAX) GO TO 120                                          redu 340
      H1 = ZMAX                                                         redu 350
      ANGLE = ANGMAX                                                    redu 360
  120 CONTINUE                                                          redu 370
      IF(H2.LE.ZMAX)  GO TO 130                                         redu 380
      H2 = ZMAX                                                         redu 390
      PHI = ANGMAX                                                      redu 400
  130 CONTINUE                                                          redu 410
       WRITE(IPR,20) ZMAX,ANGMAX                                        redu 420
   20 FORMAT(///,' FROM SUBROUTINE REDUCE: ',/,                         redu 430
     1   4X,'ONE OR BOTH OF H1 AND H2 ARE ABOVE THE TOP OF THE ',       redu 440
     2   'ATMOSPHERIC PROFILE ZMAX = ',F10.3,' AND HAVE BEEN RESET ',   redu 450
     3   'TO ZMAX',/,4X,'ANGLE AND/OR PHI HAVE ALSO BEEN RESET TO ',    redu 460
     4    'THE ZENITH ANGLE AT ZMAX = ',F10.3,' DEG')                   redu 470
  200 CONTINUE                                                          redu 480
      RETURN                                                            redu 490
      END                                                               redu 500
      SUBROUTINE RNSCAT(V,R,TT,PHASE,DIST,CSSA,ASYMR)                   rnsc 100
CC    SUBROUTINE RNSCAT(V,R,TT,PHASE,DIST,IK,CSSA,ASYMR,IENT)           rnsc 110
C********************************************************************** rnsc 120
      INTEGER PHASE,DIST                                                rnsc 130
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      DIMENSION SC(3,4)                                                 rnsc 150
C                                                                       rnsc 160
C          ARGUMENTS:                                                   rnsc 170
C                                                                       rnsc 180
C          F = FREQUENCY (GHZ)                                          rnsc 190
C          R = RAINFALL RATE (MM/HR)                                    rnsc 200
C          T = TEMPERATURE (DEGREES CELSIUS)                            rnsc 210
C          PHASE = PHASE PARAMETER (1=WATER, 2=ICE)                     rnsc 220
C          DIST = DROP SIZE DISTRIBUTION PARAMETER                      rnsc 230
C                     (1=MARSHALL-PALMER, 2=BEST)                       rnsc 240
C                                                                       rnsc 250
C          RESULTS:                                                     rnsc 260
C                                                                       rnsc 270
C          SC(1) = ABSORPTION COEFFICIENT (1/KM)                        rnsc 280
C          SC(2) = EXTINCTION COEFFICIENT (1/KM)                        rnsc 290
C          SC(I),I=3,NSC = LEGENDRE COEFFICIENTS #I-3  (NSC=10)         rnsc 300
C          ERR = ERROR RETURN CODE: 0=NO ERROR, 1=BAD FREQUENCY,        rnsc 310
C                2=BAD RAINFALL RATE, 3=BAD TEMPERATURE,                rnsc 320
C                4=BAD PHASE PARAMETER, 5=BAD DROP SIZE DISTRIBUTION    rnsc 330
C                                                                       rnsc 340
C          THE INTERNAL DATA:                                           rnsc 350
C                                                                       rnsc 360
      DIMENSION FR(9),TEMP(9)                                           rnsc 370
C                                                                       rnsc 380
C          FR(I),I=1,NF = TABULATED FREQUENCIES (GHZ)  (NF=9)           rnsc 390
C          TEMP(I),I=1,NT = TABULATED TEMPERATURES  (NT=3)              rnsc 400
C                                                                       rnsc 410
C          THE BLOCK-DATA SECTION                                       rnsc 420
C                                                                       rnsc 430
      DATA RMIN,RMAX/0.,50./,NF/9/,NT/3/,NSC/4/,MAXI/3/                 rnsc 440
      DATA TK/273.15/,CMT0/1.0/,C7500/0.5/,G0/0.0/,G7500/0.85/          rnsc 450
      DATA (TEMP(I),I=1,3)/-10.,0.,10./                                 rnsc 460
      DATA (FR(I),I=1,9)/19.35,37.,50.3,89.5,100.,118.,130.,183.,231./  rnsc 470
C                                                                       rnsc 480
C      THIS SUBROUTINE REQUIRES FREQUENCIES IN GHZ                      rnsc 490
      NOPR = 0                                                          rnsc 500
CC    IF(IK .EQ. 1) NOPR = 1                                            rnsc 510
CC    IF(IENT .GT.1) NOPR = 0                                           rnsc 520
      F= V *29.97925                                                    rnsc 530
      FSAV=F                                                            rnsc 540
      RSAV=R                                                            rnsc 550
      TSAV=T                                                            rnsc 560
      INT=0                                                             rnsc 570
C      CONVERT TEMP TO DEGREES CELSIUS                                  rnsc 580
      T=TT-TK                                                           rnsc 590
C      FREQ RANGE OF DATA 19.35-231 GHZ IF LESS THAN 19.35              rnsc 600
C      SET UP PARAMETERS FOR INTERPOLATION                              rnsc 610
      IF(F.LT.FR(1)) THEN                                               rnsc 620
        FL=0.0                                                          rnsc 630
        FM=FR(1)                                                        rnsc 640
        INT=1                                                           rnsc 650
        IF(NOPR .GT. 0) WRITE (ipr,801)                                 rnsc 660
      END IF                                                            rnsc 670
C      IF MORE THAN 231 GHZ SET UP PARAMETERS FOR EXTRAPOLATION         rnsc 680
       IF(F.GT.FR(NF)) THEN                                             rnsc 690
         FL=FR(NF)                                                      rnsc 700
         FM=7500.                                                       rnsc 710
         INT=2                                                          rnsc 720
         IF(NOPR .GT. 0) WRITE (ipr,801)                                rnsc 730
      END IF                                                            rnsc 740
C      TEMP RANGE OF DATA IS -10 TO +10 DEGREES CELCIUS                 rnsc 750
C      IF BELOW OR ABOVE EXTREME SET AND DO CALCULATIONS AT EXTREME     rnsc 760
      IF (T.LT.TEMP(1)) THEN                                            rnsc 770
        T=TEMP(1)                                                       rnsc 780
        IF(NOPR .GT. 0) WRITE (ipr,802)                                 rnsc 790
      END IF                                                            rnsc 800
C                                                                       rnsc 810
      IF (T.GT.TEMP(3)) THEN                                            rnsc 820
        T=TEMP(3)                                                       rnsc 830
        IF(NOPR .GT. 0) WRITE (ipr,802)                                 rnsc 840
      END IF                                                            rnsc 850
C                                                                       rnsc 860
C      RAIN RATE OF DATA IS FOR 0-50 MM/HR                              rnsc 870
C      IF GT 50 TREAT CALCULATIONS AS IF 50 MM/HR WAS INPUT             rnsc 880
      IF(R.GT.50) THEN                                                  rnsc 890
        R=50.                                                           rnsc 900
        IF(NOPR .GT. 0) WRITE (ipr,803)                                 rnsc 910
      END IF                                                            rnsc 920
C                                                                       rnsc 930
      KI=1                                                              rnsc 940
C             FIGURE OUT THE SECOND INDEX                               rnsc 950
   10 J=PHASE+2*DIST                                                    rnsc 960
C                                                                       rnsc 970
C                                                                       rnsc 980
C             GET THE TEMPERATURE INTERPOLATION PARAMETER ST            rnsc 990
C             IF NEEDED AND AMEND THE SECOND INDEX                      rnsc1000
      CALL BS(J,T,TEMP,NT,ST)                                           rnsc1010
C                                                                       rnsc1020
C             FIGURE OUT THE THIRD INDEX AND THE FREQUENCY INTERPOLATIONrnsc1030
C             PARAMETER SF                                              rnsc1040
      CALL BS(K,F,FR,NF,SF)                                             rnsc1050
C                                                                       rnsc1060
C             INITIALIZE SC                                             rnsc1070
      DO 11 I=1,NSC                                                     rnsc1080
      SC(KI,I)=0.                                                       rnsc1090
   11 CONTINUE                                                          rnsc1100
      SC(KI,3)=1.                                                       rnsc1110
C                                                                       rnsc1120
C             NOW DO THE CALCULATIONS                                   rnsc1130
C                                                                       rnsc1140
C             THE WATER CONTENT IS                                      rnsc1150
      IF(DIST.EQ.1) THEN                                                rnsc1160
      WC=.0889*R**.84                                                   rnsc1170
      ELSE                                                              rnsc1180
      WC=.067*R**.846                                                   rnsc1190
      END IF                                                            rnsc1200
C                                                                       rnsc1210
C             FOR A TEMPERATURE DEPENDENT CASE, I.E.                    rnsc1220
      IF(J.LT.3) THEN                                                   rnsc1230
      S1=(1.-SF)*(1.-ST)                                                rnsc1240
      S2=(1.-SF)*ST                                                     rnsc1250
      S3=SF*(1.-ST)                                                     rnsc1260
      S4=SF*ST                                                          rnsc1270
      DO 14 I=1,MAXI                                                    rnsc1280
      IF(I.LE.2) THEN                                                   rnsc1290
      ISC=I                                                             rnsc1300
      ELSE                                                              rnsc1310
      ISC=I+1                                                           rnsc1320
      END IF                                                            rnsc1330
      SC(KI,ISC)=S1*TAB(I,J,K,WC)+S2*TAB(I,J+1,K,WC)+                   rnsc1340
     *             S3*TAB(I,J,K+1,WC)+S4*TAB(I,J+1,K+1,WC)              rnsc1350
   14 CONTINUE                                                          rnsc1360
C                                                                       rnsc1370
C             FOR A TEMPERATURE INDEPENDENT CASE                        rnsc1380
      ELSE                                                              rnsc1390
      S1=1.-SF                                                          rnsc1400
      S2=SF                                                             rnsc1410
      DO 17 I=1,MAXI                                                    rnsc1420
      IF(I.LE.2) THEN                                                   rnsc1430
      ISC=I                                                             rnsc1440
      ELSE                                                              rnsc1450
      ISC=I+1                                                           rnsc1460
      END IF                                                            rnsc1470
      SC(KI,ISC)=S1*TAB(I,J,K,WC)+S2*TAB(I,J,K+1,WC)                    rnsc1480
   17 CONTINUE                                                          rnsc1490
      END IF                                                            rnsc1500
      F=FSAV                                                            rnsc1510
      IF(INT.EQ.3) GO TO 20                                             rnsc1520
      IF(INT.EQ.4) GO TO 30                                             rnsc1530
      IF(INT.EQ.0) THEN                                                 rnsc1540
        CSSA=SC(KI,1)/SC(KI,2)                                          rnsc1550
        IF(CSSA.GT.1.0) CSSA=1.0                                        rnsc1560
        ASYMR=SC(KI,4)/3.0                                              rnsc1570
        F=FSAV                                                          rnsc1580
        R=RSAV                                                          rnsc1590
        T=TSAV                                                          rnsc1600
      RETURN                                                            rnsc1610
      END IF                                                            rnsc1620
      IF(INT.EQ.1) THEN                                                 rnsc1630
        INT=3                                                           rnsc1640
        F=FM                                                            rnsc1650
        KI=2                                                            rnsc1660
      END IF                                                            rnsc1670
      IF(INT.EQ.2) THEN                                                 rnsc1680
        INT=4                                                           rnsc1690
        F=FL                                                            rnsc1700
        KI=3                                                            rnsc1710
      END IF                                                            rnsc1720
      GO TO 10                                                          rnsc1730
   20 CONTINUE                                                          rnsc1740
      FDIF=FM-F                                                         rnsc1750
      FTOT=FM-FL                                                        rnsc1760
      CM=SC(KI,1)/SC(KI,2)                                              rnsc1770
      IF(CM.GT.1.0) CM=1.0                                              rnsc1780
      CL=CMT0                                                           rnsc1790
      AM=SC(KI,4)/3.0                                                   rnsc1800
      AL=G0                                                             rnsc1810
      GO TO 40                                                          rnsc1820
   30 CONTINUE                                                          rnsc1830
      FDIF=FM-F                                                         rnsc1840
      FTOT=FM-FL                                                        rnsc1850
      CM=C7500                                                          rnsc1860
      CL=SC(KI,1)/SC(KI,2)                                              rnsc1870
      IF(CL.GT.1.0) CL=1.0                                              rnsc1880
      AM=G7500                                                          rnsc1890
      AL=SC(KI,4)/3.0                                                   rnsc1900
   40 CTOT=CM-CL                                                        rnsc1910
      CAMT=FDIF*CTOT/FTOT                                               rnsc1920
      CSSA=CM-CAMT                                                      rnsc1930
      ATOT=AM-AL                                                        rnsc1940
      AAMT=FDIF*ATOT/FTOT                                               rnsc1950
      ASYMR=AM-AAMT                                                     rnsc1960
      F=FSAV                                                            rnsc1970
      R=RSAV                                                            rnsc1980
      T=TSAV                                                            rnsc1990
      RETURN                                                            rnsc2000
801   FORMAT(2X,'***  THE ASYMMETRY PARAMETER DUE TO RAIN IS BASED ON', rnsc2010
     1 'DATA BETWEEN 19 AND 231 GHZ',                                   rnsc2020
     2 /2X,'***  EXTRAPOLATION IS USED FOR FREQUENCIES LOWER AND',      rnsc2030
     3 'HIGHER THAN THIS RANGE')                                        rnsc2040
802   FORMAT(2X,'***  TEMPERATURE RANGE OF DATA IS -10 TO +10 ',        rnsc2050
     1'DEGREES CELSIUS',/2X,'***  BEYOND THESE VALUES IT IS ',          rnsc2060
     2'TREATED AS IF AT THE EXTREMES')                                  rnsc2070
803   FORMAT(2X,'***  RAIN RATES BETWEEN 0 AND 50 MM/HR ARE',           rnsc2080
     1'WITHIN THIS DATA RANGE',/2X,'***  ABOVE THAT THE ASYMMETRY',     rnsc2090
     2' PARAMETER IS CALCULATED FOR 50 MM/HR')                          rnsc2100
      END                                                               rnsc2110
      DOUBLE PRECISION FUNCTION RTBIS(X1,X2,CPATH)                      rtbs 100
C                                                                       rtbs 110
C     THIS FUNCTION FINDS THE ROOT OF FUNC(X) = X*REFRACTIVE INDEX - CPArtbs 120
C     THE ROOT IS ACTUALLY THE TANGENT HEIGHT.                          rtbs 130
C     IT IS SANDWICHED BETWEEN X1 AND X2.                               rtbs 140
C     THIS ROUTINE IS FROM "NUMERICAL RECIPES" BY PRESS ET AL.          rtbs 150
C                                                                       rtbs 160
      DOUBLE PRECISION X1, X2, CPATH                                    rtbs 170
      REAL RE, DELTAS, ZMAX                                             rtbs 180
      INTEGER IMAX, IMOD, IBMAX, IPATH                                  rtbs 190
      DOUBLE PRECISION F, FMID,DX,X,XMID,XACC, F1, F2, RATIO            rtbs 200
      INTEGER J, JMAX                                                   rtbs 210
      COMMON /PARMTR/ RE,DELTAS,ZMAX,IMAX,IMOD,IBMAX,IPATH              rtbs 220
      DATA XACC/1D-5/                                                   rtbs 230
      PARAMETER (JMAX=40)                                               rtbs 240
C                                                                       rtbs 250
      CALL IRFXN(X2,FMID, RATIO)                                        rtbs 260
      FMID = FMID*(X2+RE)-CPATH                                         rtbs 270
      CALL IRFXN(X1,F, RATIO)                                           rtbs 280
      F = F*(X1+RE)-CPATH                                               rtbs 290
      IF(F*FMID.GE.0.) STOP 'ROOT MUST BE BRACKETED FOR BISECTION.'     rtbs 300
      IF(F.LT.0.)THEN                                                   rtbs 310
         RTBIS=X1                                                       rtbs 320
         DX=X2-X1                                                       rtbs 330
      ELSE                                                              rtbs 340
         RTBIS=X2                                                       rtbs 350
         DX=X1-X2                                                       rtbs 360
      ENDIF                                                             rtbs 370
      DO 11 J=1,JMAX                                                    rtbs 380
         DX=DX*.5                                                       rtbs 390
         XMID=RTBIS+DX                                                  rtbs 400
         CALL IRFXN(XMID,FMID, RATIO)                                   rtbs 410
         FMID = FMID*(XMID+RE)-CPATH                                    rtbs 420
         IF(FMID.LE.0.)RTBIS=XMID                                       rtbs 430
         IF(ABS(DX).LT.XACC .OR. FMID.EQ.0.) RETURN                     rtbs 440
 11   CONTINUE                                                          rtbs 450
C                                                                       rtbs 460
C     COMES HERE IF UNABLE TO SOLVE.                                    rtbs 470
C                                                                       rtbs 480
      CALL IRFXN(X2,F2, RATIO)                                          rtbs 490
      F2 = F2*(X2+RE)-CPATH                                             rtbs 500
      CALL IRFXN(X1,F1, RATIO)                                          rtbs 510
      F1 = F1*(X1+RE)-CPATH                                             rtbs 520
      IF (ABS(F2) .LT. ABS(F1)) THEN                                    rtbs 530
         RTBIS = X2                                                     rtbs 540
      ELSE                                                              rtbs 550
         RTBIS = X1                                                     rtbs 560
      ENDIF                                                             rtbs 570
      END                                                               rtbs 580
      SUBROUTINE SCHRUN(V,CPRUN)                                        rtbs 590
      COMMON /SHUR/ SHN(430)                                            rtbs 600
      DATA V1,V2,DV,INUM /49600.,51710.,5.,425/                         rtbs 610
      CPRUN = -20.                                                      rtbs 620
      IF(V .LT. V1) GO TO 20                                            rtbs 630
      IF(V .GT. V2) GO TO 20                                            rtbs 640
      IND = (V - V1)/DV + 1.0001                                        rtbs 650
      IF(IND . GT. INUM) THEN                                           rtbs 660
            PRINT*,'  IND GT INUM  V IND ',V,IND                        rtbs 670
            GO TO 20                                                    rtbs 680
      ENDIF                                                             rtbs 690
      CPRUN = SHN(IND)                                                  rtbs 700
20    RETURN                                                            rtbs 710
      END                                                               rtbs 720
      FUNCTION   SCTANG(ANGLST,THTST,PSIST,IARB)                        ftng 100
C                                                                       ftng 110
C     FUNCTION SCTANG RETURNS THE SCATTERING ANGLE (THAT IS, THE        ftng 120
C     ANGLE BETWEEN THE SUN'S RAYS AND THE LINE OF SIGHT) AT ANY        ftng 130
C     POINT ALONG THE OPTICAL PATH.                                     ftng 140
      COMMON /PARMTR/ RE,DELTAS,ZMAX,IMAX,IMOD,IBMAX,IPATH              ftng 150
      COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     ftng 160
C                                                                       ftng 170
      SUNZEN=ANGLST/DEG                                                 ftng 180
      PTHZEN=THTST/DEG                                                  ftng 190
      IF(IARB.EQ.0) GO TO 10                                            ftng 200
C     SPECIAL CASES IF PSI IS ARBITRARY                                 ftng 210
      cosang= COS(SUNZEN)*COS(PTHZEN)
      if(cosang.gt. 1.)cosang= 1.
      if(cosang.lt.-1.)cosang=-1.
      SCTANG=DEG*ACOS(cosang)                                           ftng 220
      RETURN                                                            ftng 230
10    CONTINUE                                                          ftng 240
      PSI=PSIST/DEG                                                     ftng 250
C     GENERAL CASE                                                      ftng 260
      X=SIN(SUNZEN)*SIN(PTHZEN)*COS(PSI)+COS(SUNZEN)*COS(PTHZEN)        ftng 270
      if(x.gt. 1.)x= 1.-1.e-6
      if(x.lt.-1.)x=-1.+1.e-6
      SCTANG=DEG*ACOS(X)                                                ftng 280
      RETURN                                                            ftng 290
      END                                                               ftng 300
      BLOCK DATA SF260                                                  s260 100
C>    BLOCK DATA                                                        s260 110
C               06/28/82                                                s260 120
C               UNITS OF (CM**3/MOL) * 1.E-20                           s260 130
      COMMON /S260/ V1,V2,DV,NPT,S0000(2),                              s260 140
     1      S0001(50),S0051(50),S0101(50),S0151(50),S0201(50),S0251(50),s260 150
     2      S0301(50),S0351(50),S0401(50),S0451(50),S0501(50),S0551(50),s260 160
     3      S0601(50),S0651(50),S0701(50),S0751(50),S0801(50),S0851(50),s260 170
     4      S0901(50),S0951(50),S1001(50),S1051(50),S1101(50),S1151(50),s260 180
     5      S1201(50),S1251(50),S1301(50),S1351(50),S1401(50),S1451(50),s260 190
     6      S1501(50),S1551(50),S1601(50),S1651(50),S1701(50),S1751(50),s260 200
     7      S1801(50),S1851(50),S1901(50),S1951(50),S2001(1)            s260 210
C                                                                       s260 220
C                                                                       s260 230
       DATA V1,V2,DV,NPT /                                              s260 240
     1      -20.0,     20000.0,       10.0,  2003/                      s260 250
C                                                                       s260 260
C                                                                       s260 270
      DATA S0000/ 1.7750E-01, 1.7045E-01/                               s260 280
      DATA S0001/                                                       s260 290
     C 1.6457E-01, 1.7045E-01, 1.7750E-01, 2.0036E-01, 2.1347E-01,      s260 300
     C 2.2454E-01, 2.3428E-01, 2.3399E-01, 2.3022E-01, 2.0724E-01,      s260 310
     C 1.9712E-01, 1.8317E-01, 1.6724E-01, 1.4780E-01, 1.2757E-01,      s260 320
     C 1.1626E-01, 1.0098E-01, 8.9033E-02, 7.9770E-02, 6.7416E-02,      s260 330
     C 5.9588E-02, 5.1117E-02, 4.6218E-02, 4.2179E-02, 3.4372E-02,      s260 340
     C 2.9863E-02, 2.5252E-02, 2.2075E-02, 1.9209E-02, 1.5816E-02,      s260 350
     C 1.3932E-02, 1.1943E-02, 1.0079E-02, 8.7667E-03, 7.4094E-03,      s260 360
     C 6.4967E-03, 5.5711E-03, 4.8444E-03, 4.2552E-03, 3.6953E-03,      s260 370
     C 3.2824E-03, 2.9124E-03, 2.6102E-03, 2.3370E-03, 2.1100E-03,      s260 380
     C 1.9008E-03, 1.7145E-03, 1.5573E-03, 1.4206E-03, 1.2931E-03/      s260 390
      DATA S0051/                                                       s260 400
     C 1.1803E-03, 1.0774E-03, 9.8616E-04, 9.0496E-04, 8.3071E-04,      s260 410
     C 7.6319E-04, 7.0149E-04, 6.4637E-04, 5.9566E-04, 5.4987E-04,      s260 420
     C 5.0768E-04, 4.6880E-04, 4.3317E-04, 4.0037E-04, 3.7064E-04,      s260 430
     C 3.4325E-04, 3.1809E-04, 2.9501E-04, 2.7382E-04, 2.5430E-04,      s260 440
     C 2.3630E-04, 2.1977E-04, 2.0452E-04, 1.9042E-04, 1.7740E-04,      s260 450
     C 1.6544E-04, 1.5442E-04, 1.4425E-04, 1.3486E-04, 1.2618E-04,      s260 460
     C 1.1817E-04, 1.1076E-04, 1.0391E-04, 9.7563E-05, 9.1696E-05,      s260 470
     C 8.6272E-05, 8.1253E-05, 7.6607E-05, 7.2302E-05, 6.8311E-05,      s260 480
     C 6.4613E-05, 6.1183E-05, 5.8001E-05, 5.5048E-05, 5.2307E-05,      s260 490
     C 4.9761E-05, 4.7395E-05, 4.5197E-05, 4.3155E-05, 4.1256E-05/      s260 500
      DATA S0101/                                                       s260 510
     C 3.9491E-05, 3.7849E-05, 3.6324E-05, 3.4908E-05, 3.3594E-05,      s260 520
     C 3.2374E-05, 3.1244E-05, 3.0201E-05, 2.9240E-05, 2.8356E-05,      s260 530
     C 2.7547E-05, 2.6814E-05, 2.6147E-05, 2.5551E-05, 2.5029E-05,      s260 540
     C 2.4582E-05, 2.4203E-05, 2.3891E-05, 2.3663E-05, 2.3531E-05,      s260 550
     C 2.3483E-05, 2.3516E-05, 2.3694E-05, 2.4032E-05, 2.4579E-05,      s260 560
     C 2.5234E-05, 2.6032E-05, 2.7119E-05, 2.8631E-05, 3.0848E-05,      s260 570
     C 3.3262E-05, 3.6635E-05, 4.0732E-05, 4.5923E-05, 5.3373E-05,      s260 580
     C 6.1875E-05, 7.2031E-05, 8.5980E-05, 9.8642E-05, 1.1469E-04,      s260 590
     C 1.3327E-04, 1.5390E-04, 1.7513E-04, 2.0665E-04, 2.3609E-04,      s260 600
     C 2.6220E-04, 2.8677E-04, 3.2590E-04, 3.8624E-04, 4.1570E-04/      s260 610
      DATA S0151/                                                       s260 620
     C 4.5207E-04, 4.9336E-04, 5.4500E-04, 5.8258E-04, 5.8086E-04,      s260 630
     C 5.6977E-04, 5.3085E-04, 4.8020E-04, 4.3915E-04, 4.0343E-04,      s260 640
     C 3.7853E-04, 3.7025E-04, 3.9637E-04, 4.4675E-04, 4.7072E-04,      s260 650
     C 4.9022E-04, 5.2076E-04, 5.3676E-04, 5.2755E-04, 4.8244E-04,      s260 660
     C 4.5473E-04, 4.3952E-04, 3.9614E-04, 3.4086E-04, 2.9733E-04,      s260 670
     C 2.6367E-04, 2.3767E-04, 2.0427E-04, 1.7595E-04, 1.5493E-04,      s260 680
     C 1.3851E-04, 1.1874E-04, 1.0735E-04, 9.0490E-05, 8.1149E-05,      s260 690
     C 7.4788E-05, 6.5438E-05, 5.8248E-05, 4.8076E-05, 4.3488E-05,      s260 700
     C 3.7856E-05, 3.3034E-05, 2.9592E-05, 2.6088E-05, 2.3497E-05,      s260 710
     C 2.0279E-05, 1.7526E-05, 1.5714E-05, 1.3553E-05, 1.2145E-05/      s260 720
      DATA S0201/                                                       s260 730
     C 1.0802E-05, 9.7681E-06, 8.8196E-06, 7.8291E-06, 7.1335E-06,      s260 740
     C 6.4234E-06, 5.8391E-06, 5.3532E-06, 4.9079E-06, 4.5378E-06,      s260 750
     C 4.1716E-06, 3.8649E-06, 3.5893E-06, 3.3406E-06, 3.1199E-06,      s260 760
     C 2.9172E-06, 2.7348E-06, 2.5644E-06, 2.4086E-06, 2.2664E-06,      s260 770
     C 2.1359E-06, 2.0159E-06, 1.9051E-06, 1.8031E-06, 1.7074E-06,      s260 780
     C 1.6185E-06, 1.5356E-06, 1.4584E-06, 1.3861E-06, 1.3179E-06,      s260 790
     C 1.2545E-06, 1.1951E-06, 1.1395E-06, 1.0873E-06, 1.0384E-06,      s260 800
     C 9.9250E-07, 9.4935E-07, 9.0873E-07, 8.7050E-07, 8.3446E-07,      s260 810
     C 8.0046E-07, 7.6834E-07, 7.3800E-07, 7.0931E-07, 6.8217E-07,      s260 820
     C 6.5648E-07, 6.3214E-07, 6.0909E-07, 5.8725E-07, 5.6655E-07/      s260 830
      DATA S0251/                                                       s260 840
     C 5.4693E-07, 5.2835E-07, 5.1077E-07, 4.9416E-07, 4.7853E-07,      s260 850
     C 4.6381E-07, 4.5007E-07, 4.3728E-07, 4.2550E-07, 4.1450E-07,      s260 860
     C 4.0459E-07, 3.9532E-07, 3.8662E-07, 3.7855E-07, 3.7041E-07,      s260 870
     C 3.6254E-07, 3.5420E-07, 3.4617E-07, 3.3838E-07, 3.3212E-07,      s260 880
     C 3.2655E-07, 3.1865E-07, 3.1203E-07, 3.0670E-07, 3.0252E-07,      s260 890
     C 2.9749E-07, 2.9184E-07, 2.8795E-07, 2.8501E-07, 2.8202E-07,      s260 900
     C 2.7856E-07, 2.7509E-07, 2.7152E-07, 2.6844E-07, 2.6642E-07,      s260 910
     C 2.6548E-07, 2.6617E-07, 2.6916E-07, 2.7372E-07, 2.8094E-07,      s260 920
     C 2.9236E-07, 3.1035E-07, 3.2854E-07, 3.5481E-07, 3.9377E-07,      s260 930
     C 4.4692E-07, 5.0761E-07, 5.7715E-07, 6.7725E-07, 8.0668E-07/      s260 940
      DATA S0301/                                                       s260 950
     C 9.3716E-07, 1.0797E-06, 1.1689E-06, 1.3217E-06, 1.4814E-06,      s260 960
     C 1.5627E-06, 1.6519E-06, 1.7601E-06, 1.9060E-06, 2.0474E-06,      s260 970
     C 2.0716E-06, 2.0433E-06, 1.9752E-06, 1.8466E-06, 1.7526E-06,      s260 980
     C 1.6657E-06, 1.5870E-06, 1.5633E-06, 1.6520E-06, 1.8471E-06,      s260 990
     C 1.9953E-06, 2.0975E-06, 2.2016E-06, 2.2542E-06, 2.3081E-06,      s2601000
     C 2.3209E-06, 2.2998E-06, 2.3056E-06, 2.2757E-06, 2.2685E-06,      s2601010
     C 2.2779E-06, 2.2348E-06, 2.2445E-06, 2.3174E-06, 2.4284E-06,      s2601020
     C 2.5290E-06, 2.7340E-06, 2.9720E-06, 3.2332E-06, 3.5392E-06,      s2601030
     C 3.9013E-06, 4.3334E-06, 4.9088E-06, 5.3428E-06, 5.9142E-06,      s2601040
     C 6.6106E-06, 7.4709E-06, 8.5019E-06, 9.6835E-06, 1.0984E-05/      s2601050
      DATA S0351/                                                       s2601060
     C 1.2831E-05, 1.4664E-05, 1.7080E-05, 2.0103E-05, 2.4148E-05,      s2601070
     C 2.7948E-05, 3.2855E-05, 3.9046E-05, 4.6429E-05, 5.6633E-05,      s2601080
     C 6.6305E-05, 7.6048E-05, 8.7398E-05, 1.0034E-04, 1.1169E-04,      s2601090
     C 1.2813E-04, 1.3354E-04, 1.3952E-04, 1.4204E-04, 1.4615E-04,      s2601100
     C 1.5144E-04, 1.5475E-04, 1.6561E-04, 1.7135E-04, 1.6831E-04,      s2601110
     C 1.6429E-04, 1.6353E-04, 1.6543E-04, 1.5944E-04, 1.5404E-04,      s2601120
     C 1.5458E-04, 1.6287E-04, 1.7277E-04, 1.8387E-04, 1.7622E-04,      s2601130
     C 1.6360E-04, 1.5273E-04, 1.3667E-04, 1.2364E-04, 9.7576E-05,      s2601140
     C 7.9140E-05, 6.4241E-05, 5.1826E-05, 4.1415E-05, 3.1347E-05,      s2601150
     C 2.5125E-05, 2.0027E-05, 1.6362E-05, 1.3364E-05, 1.1117E-05/      s2601160
      DATA S0401/                                                       s2601170
     C 9.4992E-06, 8.1581E-06, 7.1512E-06, 6.2692E-06, 5.5285E-06,      s2601180
     C 4.9000E-06, 4.3447E-06, 3.8906E-06, 3.4679E-06, 3.1089E-06,      s2601190
     C 2.8115E-06, 2.5496E-06, 2.2982E-06, 2.0861E-06, 1.8763E-06,      s2601200
     C 1.7035E-06, 1.5548E-06, 1.4107E-06, 1.2839E-06, 1.1706E-06,      s2601210
     C 1.0709E-06, 9.8099E-07, 8.9901E-07, 8.2394E-07, 7.5567E-07,      s2601220
     C 6.9434E-07, 6.3867E-07, 5.8845E-07, 5.4263E-07, 5.0033E-07,      s2601230
     C 4.6181E-07, 4.2652E-07, 3.9437E-07, 3.6497E-07, 3.3781E-07,      s2601240
     C 3.1292E-07, 2.9011E-07, 2.6915E-07, 2.4989E-07, 2.3215E-07,      s2601250
     C 2.1582E-07, 2.0081E-07, 1.8700E-07, 1.7432E-07, 1.6264E-07,      s2601260
     C 1.5191E-07, 1.4207E-07, 1.3306E-07, 1.2484E-07, 1.1737E-07/      s2601270
      DATA S0451/                                                       s2601280
     C 1.1056E-07, 1.0451E-07, 9.9060E-08, 9.4135E-08, 8.9608E-08,      s2601290
     C 8.5697E-08, 8.1945E-08, 7.8308E-08, 7.4808E-08, 7.1686E-08,      s2601300
     C 6.8923E-08, 6.5869E-08, 6.3308E-08, 6.0840E-08, 5.8676E-08,      s2601310
     C 5.6744E-08, 5.5016E-08, 5.3813E-08, 5.2792E-08, 5.2097E-08,      s2601320
     C 5.1737E-08, 5.1603E-08, 5.1656E-08, 5.1989E-08, 5.2467E-08,      s2601330
     C 5.2918E-08, 5.3589E-08, 5.4560E-08, 5.5869E-08, 5.7403E-08,      s2601340
     C 5.8968E-08, 6.0973E-08, 6.3432E-08, 6.6245E-08, 6.9353E-08,      s2601350
     C 7.2686E-08, 7.6541E-08, 8.0991E-08, 8.5950E-08, 9.1429E-08,      s2601360
     C 9.7851E-08, 1.0516E-07, 1.1349E-07, 1.2295E-07, 1.3335E-07,      s2601370
     C 1.4488E-07, 1.5864E-07, 1.7412E-07, 1.9140E-07, 2.1078E-07/      s2601380
      DATA S0501/                                                       s2601390
     C 2.3369E-07, 2.5996E-07, 2.8848E-07, 3.2169E-07, 3.5991E-07,      s2601400
     C 4.0566E-07, 4.5969E-07, 5.3094E-07, 6.1458E-07, 7.1155E-07,      s2601410
     C 8.3045E-07, 9.9021E-07, 1.2042E-06, 1.4914E-06, 1.8145E-06,      s2601420
     C 2.2210E-06, 2.7831E-06, 3.4533E-06, 4.4446E-06, 5.1989E-06,      s2601430
     C 6.2289E-06, 7.1167E-06, 8.3949E-06, 9.6417E-06, 1.0313E-05,      s2601440
     C 1.0485E-05, 1.0641E-05, 1.0898E-05, 1.0763E-05, 1.0506E-05,      s2601450
     C 1.0497E-05, 1.1696E-05, 1.2654E-05, 1.3029E-05, 1.3175E-05,      s2601460
     C 1.4264E-05, 1.4985E-05, 1.4999E-05, 1.4317E-05, 1.4616E-05,      s2601470
     C 1.4963E-05, 1.5208E-05, 1.4942E-05, 1.3879E-05, 1.3087E-05,      s2601480
     C 1.1727E-05, 1.0515E-05, 9.0073E-06, 7.3133E-06, 6.1181E-06/      s2601490
      DATA S0551/                                                       s2601500
     C 5.0623E-06, 4.1105E-06, 3.3915E-06, 2.6711E-06, 2.1464E-06,      s2601510
     C 1.7335E-06, 1.4302E-06, 1.1847E-06, 9.9434E-07, 8.2689E-07,      s2601520
     C 7.0589E-07, 6.0750E-07, 5.3176E-07, 4.6936E-07, 4.1541E-07,      s2601530
     C 3.6625E-07, 3.2509E-07, 2.9156E-07, 2.6308E-07, 2.3819E-07,      s2601540
     C 2.1421E-07, 1.9366E-07, 1.7626E-07, 1.5982E-07, 1.4567E-07,      s2601550
     C 1.3354E-07, 1.2097E-07, 1.1029E-07, 1.0063E-07, 9.2003E-08,      s2601560
     C 8.4245E-08, 7.7004E-08, 7.0636E-08, 6.4923E-08, 5.9503E-08,      s2601570
     C 5.4742E-08, 5.0450E-08, 4.6470E-08, 4.2881E-08, 3.9550E-08,      s2601580
     C 3.6541E-08, 3.3803E-08, 3.1279E-08, 2.8955E-08, 2.6858E-08,      s2601590
     C 2.4905E-08, 2.3146E-08, 2.1539E-08, 2.0079E-08, 1.8746E-08/      s2601600
      DATA S0601/                                                       s2601610
     C 1.7517E-08, 1.6396E-08, 1.5369E-08, 1.4426E-08, 1.3543E-08,      s2601620
     C 1.2724E-08, 1.1965E-08, 1.1267E-08, 1.0617E-08, 1.0010E-08,      s2601630
     C 9.4662E-09, 8.9553E-09, 8.4988E-09, 8.0807E-09, 7.7043E-09,      s2601640
     C 7.3721E-09, 7.0707E-09, 6.8047E-09, 6.5702E-09, 6.3634E-09,      s2601650
     C 6.1817E-09, 6.0239E-09, 5.8922E-09, 5.7824E-09, 5.7019E-09,      s2601660
     C 5.6368E-09, 5.5940E-09, 5.5669E-09, 5.5583E-09, 5.5653E-09,      s2601670
     C 5.5837E-09, 5.6243E-09, 5.6883E-09, 5.7800E-09, 5.8964E-09,      s2601680
     C 6.0429E-09, 6.2211E-09, 6.4282E-09, 6.6634E-09, 6.9306E-09,      s2601690
     C 7.2336E-09, 7.5739E-09, 7.9562E-09, 8.3779E-09, 8.8575E-09,      s2601700
     C 9.3992E-09, 1.0004E-08, 1.0684E-08, 1.1450E-08, 1.2320E-08/      s2601710
      DATA S0651/                                                       s2601720
     C 1.3311E-08, 1.4455E-08, 1.5758E-08, 1.7254E-08, 1.8927E-08,      s2601730
     C 2.0930E-08, 2.3348E-08, 2.6074E-08, 2.9221E-08, 3.2770E-08,      s2601740
     C 3.7485E-08, 4.2569E-08, 4.8981E-08, 5.5606E-08, 6.2393E-08,      s2601750
     C 7.1901E-08, 8.2921E-08, 9.5513E-08, 1.1111E-07, 1.3143E-07,      s2601760
     C 1.5971E-07, 1.8927E-07, 2.2643E-07, 2.7860E-07, 3.2591E-07,      s2601770
     C 3.7024E-07, 4.2059E-07, 4.9432E-07, 5.5543E-07, 5.7498E-07,      s2601780
     C 5.9210E-07, 6.1005E-07, 6.1577E-07, 5.9193E-07, 5.6602E-07,      s2601790
     C 5.7403E-07, 6.0050E-07, 6.4723E-07, 6.7073E-07, 7.5415E-07,      s2601800
     C 8.0982E-07, 8.7658E-07, 9.1430E-07, 9.4459E-07, 9.8347E-07,      s2601810
     C 9.8768E-07, 1.0153E-06, 1.0066E-06, 1.0353E-06, 1.0353E-06/      s2601820
      DATA S0701/                                                       s2601830
     C 1.0722E-06, 1.1138E-06, 1.1923E-06, 1.2947E-06, 1.4431E-06,      s2601840
     C 1.6537E-06, 1.8662E-06, 2.2473E-06, 2.6464E-06, 3.1041E-06,      s2601850
     C 3.4858E-06, 4.0167E-06, 4.6675E-06, 5.0983E-06, 5.7997E-06,      s2601860
     C 6.0503E-06, 6.4687E-06, 6.5396E-06, 6.7986E-06, 7.0244E-06,      s2601870
     C 7.2305E-06, 7.6732E-06, 7.9783E-06, 7.9846E-06, 7.7617E-06,      s2601880
     C 7.7657E-06, 7.7411E-06, 7.8816E-06, 7.8136E-06, 8.0051E-06,      s2601890
     C 8.5799E-06, 9.1659E-06, 9.8646E-06, 9.4920E-06, 8.7670E-06,      s2601900
     C 8.2034E-06, 7.2297E-06, 6.2324E-06, 4.9315E-06, 3.9128E-06,      s2601910
     C 3.1517E-06, 2.4469E-06, 1.8815E-06, 1.4627E-06, 1.1698E-06,      s2601920
     C 9.4686E-07, 7.8486E-07, 6.6970E-07, 5.8811E-07, 5.2198E-07/      s2601930
      DATA S0751/                                                       s2601940
     C 4.6809E-07, 4.1671E-07, 3.7006E-07, 3.3066E-07, 2.9387E-07,      s2601950
     C 2.6415E-07, 2.3409E-07, 2.0991E-07, 1.9132E-07, 1.7519E-07,      s2601960
     C 1.5939E-07, 1.4368E-07, 1.3050E-07, 1.1883E-07, 1.0772E-07,      s2601970
     C 9.6884E-08, 8.7888E-08, 7.8956E-08, 7.1024E-08, 6.3824E-08,      s2601980
     C 5.7256E-08, 5.1769E-08, 4.7037E-08, 4.2901E-08, 3.8970E-08,      s2601990
     C 3.5467E-08, 3.2502E-08, 2.9827E-08, 2.7389E-08, 2.5111E-08,      s2602000
     C 2.3056E-08, 2.1267E-08, 1.9610E-08, 1.8133E-08, 1.6775E-08,      s2602010
     C 1.5491E-08, 1.4329E-08, 1.3265E-08, 1.2300E-08, 1.1420E-08,      s2602020
     C 1.0593E-08, 9.8475E-09, 9.1585E-09, 8.5256E-09, 7.9525E-09,      s2602030
     C 7.4226E-09, 6.9379E-09, 6.4950E-09, 6.0911E-09, 5.7242E-09/      s2602040
      DATA S0801/                                                       s2602050
     C 5.3877E-09, 5.0821E-09, 4.8051E-09, 4.5554E-09, 4.3315E-09,      s2602060
     C 4.1336E-09, 3.9632E-09, 3.8185E-09, 3.7080E-09, 3.6296E-09,      s2602070
     C 3.5804E-09, 3.5776E-09, 3.6253E-09, 3.7115E-09, 3.8151E-09,      s2602080
     C 3.9804E-09, 4.1742E-09, 4.3581E-09, 4.5306E-09, 4.7736E-09,      s2602090
     C 5.1297E-09, 5.5291E-09, 5.9125E-09, 6.4956E-09, 7.0362E-09,      s2602100
     C 7.5318E-09, 7.9947E-09, 8.6438E-09, 9.7227E-09, 1.0130E-08,      s2602110
     C 1.0549E-08, 1.1064E-08, 1.1702E-08, 1.2043E-08, 1.1781E-08,      s2602120
     C 1.1838E-08, 1.1917E-08, 1.2131E-08, 1.2476E-08, 1.3611E-08,      s2602130
     C 1.4360E-08, 1.5057E-08, 1.6247E-08, 1.7284E-08, 1.8420E-08,      s2602140
     C 1.8352E-08, 1.8722E-08, 1.9112E-08, 1.9092E-08, 1.9311E-08/      s2602150
      DATA S0851/                                                       s2602160
     C 1.9411E-08, 1.9884E-08, 2.0508E-08, 2.1510E-08, 2.3143E-08,      s2602170
     C 2.5050E-08, 2.7596E-08, 3.1231E-08, 3.6260E-08, 4.3410E-08,      s2602180
     C 5.2240E-08, 6.3236E-08, 7.7522E-08, 9.8688E-08, 1.1859E-07,      s2602190
     C 1.4341E-07, 1.6798E-07, 1.9825E-07, 2.2898E-07, 2.6257E-07,      s2602200
     C 2.9884E-07, 3.3247E-07, 3.4936E-07, 3.5583E-07, 3.7150E-07,      s2602210
     C 3.6580E-07, 3.7124E-07, 3.7030E-07, 4.1536E-07, 4.6656E-07,      s2602220
     C 4.6677E-07, 4.7507E-07, 4.9653E-07, 5.3795E-07, 5.4957E-07,      s2602230
     C 5.2238E-07, 5.4690E-07, 5.6569E-07, 5.9844E-07, 5.9835E-07,      s2602240
     C 5.6522E-07, 5.4123E-07, 4.7904E-07, 4.2851E-07, 3.5603E-07,      s2602250
     C 2.8932E-07, 2.3655E-07, 1.8592E-07, 1.4943E-07, 1.1971E-07/      s2602260
      DATA S0901/                                                       s2602270
     C 9.8482E-08, 8.3675E-08, 7.1270E-08, 6.2496E-08, 5.4999E-08,      s2602280
     C 4.9821E-08, 4.5387E-08, 4.1340E-08, 3.7453E-08, 3.3298E-08,      s2602290
     C 3.0120E-08, 2.7032E-08, 2.4236E-08, 2.1500E-08, 1.8988E-08,      s2602300
     C 1.7414E-08, 1.5706E-08, 1.4192E-08, 1.3204E-08, 1.1759E-08,      s2602310
     C 1.0737E-08, 9.6309E-09, 8.8179E-09, 8.2619E-09, 7.2264E-09,      s2602320
     C 6.4856E-09, 5.8037E-09, 5.2093E-09, 4.7205E-09, 4.1749E-09,      s2602330
     C 3.7852E-09, 3.3915E-09, 3.0089E-09, 2.7335E-09, 2.4398E-09,      s2602340
     C 2.2031E-09, 1.9786E-09, 1.7890E-09, 1.6266E-09, 1.4830E-09,      s2602350
     C 1.3576E-09, 1.2518E-09, 1.1587E-09, 1.0726E-09, 9.9106E-10,      s2602360
     C 9.1673E-10, 8.5084E-10, 7.9147E-10, 7.2882E-10, 6.7342E-10/      s2602370
      DATA S0951/                                                       s2602380
     C 6.2593E-10, 5.8294E-10, 5.4435E-10, 5.0997E-10, 4.7806E-10,      s2602390
     C 4.4931E-10, 4.2357E-10, 4.0023E-10, 3.7909E-10, 3.5999E-10,      s2602400
     C 3.4285E-10, 3.2776E-10, 3.1468E-10, 3.0377E-10, 2.9479E-10,      s2602410
     C 2.8877E-10, 2.8512E-10, 2.8617E-10, 2.8976E-10, 3.0001E-10,      s2602420
     C 3.1718E-10, 3.3898E-10, 3.5857E-10, 3.8358E-10, 4.3131E-10,      s2602430
     C 4.5741E-10, 4.6948E-10, 4.7594E-10, 4.9529E-10, 5.1563E-10,      s2602440
     C 4.9475E-10, 4.8369E-10, 4.8829E-10, 5.0047E-10, 5.0203E-10,      s2602450
     C 5.1954E-10, 5.5352E-10, 5.9928E-10, 6.7148E-10, 7.1121E-10,      s2602460
     C 7.4317E-10, 7.6039E-10, 7.8313E-10, 8.0684E-10, 7.8553E-10,      s2602470
     C 7.8312E-10, 7.8537E-10, 7.8872E-10, 8.0185E-10, 8.1004E-10/      s2602480
      DATA S1001/                                                       s2602490
     C 8.2608E-10, 8.2525E-10, 8.3857E-10, 8.7920E-10, 9.2451E-10,      s2602500
     C 9.8661E-10, 1.0629E-09, 1.1659E-09, 1.2922E-09, 1.4387E-09,      s2602510
     C 1.6254E-09, 1.8425E-09, 2.1428E-09, 2.5477E-09, 3.0379E-09,      s2602520
     C 3.7570E-09, 4.4354E-09, 5.1802E-09, 6.2769E-09, 7.4894E-09,      s2602530
     C 8.7474E-09, 9.8037E-09, 1.1582E-08, 1.3293E-08, 1.4471E-08,      s2602540
     C 1.5025E-08, 1.5580E-08, 1.6228E-08, 1.6413E-08, 1.6020E-08,      s2602550
     C 1.6393E-08, 1.7545E-08, 1.9590E-08, 2.1449E-08, 2.3856E-08,      s2602560
     C 2.7050E-08, 3.0214E-08, 3.3733E-08, 3.6487E-08, 3.9353E-08,      s2602570
     C 4.2660E-08, 4.6385E-08, 4.9955E-08, 5.5313E-08, 6.0923E-08,      s2602580
     C 6.8948E-08, 7.3649E-08, 8.2602E-08, 9.2212E-08, 9.9080E-08/      s2602590
      DATA S1051/                                                       s2602600
     C 1.1319E-07, 1.1790E-07, 1.2941E-07, 1.3199E-07, 1.3914E-07,      s2602610
     C 1.4843E-07, 1.5300E-07, 1.6419E-07, 1.7095E-07, 1.6988E-07,      s2602620
     C 1.6494E-07, 1.6327E-07, 1.6067E-07, 1.6909E-07, 1.7118E-07,      s2602630
     C 1.8106E-07, 1.9857E-07, 2.1696E-07, 2.3385E-07, 2.2776E-07,      s2602640
     C 2.1402E-07, 1.9882E-07, 1.7362E-07, 1.4308E-07, 1.1158E-07,      s2602650
     C 8.8781E-08, 6.8689E-08, 5.2062E-08, 4.0427E-08, 3.2669E-08,      s2602660
     C 2.7354E-08, 2.3200E-08, 2.0580E-08, 1.8676E-08, 1.7329E-08,      s2602670
     C 1.6621E-08, 1.6433E-08, 1.6953E-08, 1.7134E-08, 1.7948E-08,      s2602680
     C 1.9107E-08, 1.9875E-08, 2.1416E-08, 2.1556E-08, 2.2265E-08,      s2602690
     C 2.2171E-08, 2.2534E-08, 2.3029E-08, 2.2828E-08, 2.3143E-08/      s2602700
      DATA S1101/                                                       s2602710
     C 2.2965E-08, 2.2223E-08, 2.1108E-08, 2.0265E-08, 1.9516E-08,      s2602720
     C 1.9941E-08, 2.0312E-08, 2.1080E-08, 2.2611E-08, 2.4210E-08,      s2602730
     C 2.6069E-08, 2.5097E-08, 2.3318E-08, 2.1543E-08, 1.8942E-08,      s2602740
     C 1.5960E-08, 1.2386E-08, 9.9340E-09, 7.7502E-09, 5.9462E-09,      s2602750
     C 4.5113E-09, 3.5523E-09, 2.8844E-09, 2.3394E-09, 1.9584E-09,      s2602760
     C 1.6749E-09, 1.4624E-09, 1.2809E-09, 1.1359E-09, 1.0087E-09,      s2602770
     C 9.0166E-10, 8.1079E-10, 7.2219E-10, 6.4922E-10, 5.8803E-10,      s2602780
     C 5.3290E-10, 4.8590E-10, 4.4111E-10, 4.0184E-10, 3.6644E-10,      s2602790
     C 3.3529E-10, 3.0789E-10, 2.8286E-10, 2.6089E-10, 2.4125E-10,      s2602800
     C 2.2355E-10, 2.0783E-10, 1.9370E-10, 1.8088E-10, 1.6948E-10/      s2602810
      DATA S1151/                                                       s2602820
     C 1.5929E-10, 1.5013E-10, 1.4193E-10, 1.3470E-10, 1.2841E-10,      s2602830
     C 1.2307E-10, 1.1865E-10, 1.1502E-10, 1.1243E-10, 1.1099E-10,      s2602840
     C 1.1066E-10, 1.1216E-10, 1.1529E-10, 1.2171E-10, 1.3128E-10,      s2602850
     C 1.4153E-10, 1.5962E-10, 1.8048E-10, 2.0936E-10, 2.3165E-10,      s2602860
     C 2.5746E-10, 2.9600E-10, 3.3707E-10, 3.5267E-10, 3.5953E-10,      s2602870
     C 3.6822E-10, 3.8363E-10, 3.8286E-10, 3.5883E-10, 3.6154E-10,      s2602880
     C 3.6653E-10, 3.8507E-10, 4.0250E-10, 4.4435E-10, 4.9889E-10,      s2602890
     C 5.6932E-10, 6.3599E-10, 7.0281E-10, 7.5777E-10, 8.1279E-10,      s2602900
     C 8.8910E-10, 9.3400E-10, 1.0076E-09, 1.0945E-09, 1.1898E-09,      s2602910
     C 1.3108E-09, 1.4725E-09, 1.7028E-09, 1.9619E-09, 2.3527E-09/      s2602920
      DATA S1201/                                                       s2602930
     C 2.6488E-09, 3.0327E-09, 3.4396E-09, 3.8797E-09, 4.4115E-09,      s2602940
     C 4.6853E-09, 4.9553E-09, 4.9551E-09, 5.1062E-09, 5.0996E-09,      s2602950
     C 5.1119E-09, 5.2283E-09, 5.8297E-09, 6.3439E-09, 6.2675E-09,      s2602960
     C 6.3296E-09, 6.5173E-09, 7.1685E-09, 7.0528E-09, 6.8856E-09,      s2602970
     C 7.3182E-09, 7.6990E-09, 8.3461E-09, 8.1946E-09, 7.7153E-09,      s2602980
     C 7.2411E-09, 6.4511E-09, 5.7336E-09, 4.6105E-09, 3.6962E-09,      s2602990
     C 2.9944E-09, 2.4317E-09, 1.9399E-09, 1.5331E-09, 1.2633E-09,      s2603000
     C 1.0613E-09, 9.0136E-10, 7.9313E-10, 7.1543E-10, 6.6485E-10,      s2603010
     C 6.4225E-10, 6.3980E-10, 6.4598E-10, 6.7428E-10, 7.0270E-10,      s2603020
     C 7.4694E-10, 7.7946E-10, 7.9395E-10, 7.8716E-10, 7.6933E-10/      s2603030
      DATA S1251/                                                       s2603040
     C 7.6220E-10, 7.4825E-10, 7.4805E-10, 7.6511E-10, 7.6492E-10,      s2603050
     C 7.4103E-10, 7.1979E-10, 7.1686E-10, 7.3403E-10, 7.1142E-10,      s2603060
     C 7.0212E-10, 7.1548E-10, 7.5253E-10, 8.0444E-10, 8.2378E-10,      s2603070
     C 7.8004E-10, 7.1712E-10, 6.4978E-10, 5.7573E-10, 4.8675E-10,      s2603080
     C 3.7945E-10, 3.0118E-10, 2.4241E-10, 1.9100E-10, 1.4816E-10,      s2603090
     C 1.1567E-10, 9.4183E-11, 7.7660E-11, 6.5270E-11, 5.6616E-11,      s2603100
     C 4.9576E-11, 4.4137E-11, 3.9459E-11, 3.5759E-11, 3.2478E-11,      s2603110
     C 2.9419E-11, 2.6703E-11, 2.4365E-11, 2.2412E-11, 2.0606E-11,      s2603120
     C 1.9067E-11, 1.7800E-11, 1.6695E-11, 1.5729E-11, 1.4887E-11,      s2603130
     C 1.4135E-11, 1.3519E-11, 1.2992E-11, 1.2563E-11, 1.2223E-11/      s2603140
      DATA S1301/                                                       s2603150
     C 1.1962E-11, 1.1775E-11, 1.1657E-11, 1.1605E-11, 1.1619E-11,      s2603160
     C 1.1697E-11, 1.1839E-11, 1.2046E-11, 1.2319E-11, 1.2659E-11,      s2603170
     C 1.3070E-11, 1.3553E-11, 1.4113E-11, 1.4754E-11, 1.5480E-11,      s2603180
     C 1.6298E-11, 1.7214E-11, 1.8236E-11, 1.9372E-11, 2.0635E-11,      s2603190
     C 2.2036E-11, 2.3590E-11, 2.5317E-11, 2.7242E-11, 2.9400E-11,      s2603200
     C 3.1849E-11, 3.4654E-11, 3.7923E-11, 4.1695E-11, 4.6055E-11,      s2603210
     C 5.0940E-11, 5.5624E-11, 6.0667E-11, 6.6261E-11, 7.2692E-11,      s2603220
     C 7.9711E-11, 8.7976E-11, 9.6884E-11, 1.0775E-10, 1.2093E-10,      s2603230
     C 1.3531E-10, 1.5404E-10, 1.7315E-10, 1.9862E-10, 2.3341E-10,      s2603240
     C 2.7014E-10, 3.1716E-10, 3.6957E-10, 4.3233E-10, 5.2566E-10/      s2603250
      DATA S1351/                                                       s2603260
     C 6.2251E-10, 7.2149E-10, 8.3958E-10, 9.5931E-10, 1.1388E-09,      s2603270
     C 1.2973E-09, 1.4442E-09, 1.5638E-09, 1.6974E-09, 1.8489E-09,      s2603280
     C 1.9830E-09, 2.1720E-09, 2.3662E-09, 2.6987E-09, 3.1697E-09,      s2603290
     C 3.6907E-09, 4.2625E-09, 4.7946E-09, 5.3848E-09, 6.0897E-09,      s2603300
     C 6.4730E-09, 7.1483E-09, 7.7432E-09, 8.0851E-09, 8.5013E-09,      s2603310
     C 8.5909E-09, 9.1890E-09, 9.3124E-09, 9.5936E-09, 9.8787E-09,      s2603320
     C 9.9036E-09, 9.6712E-09, 9.2036E-09, 9.0466E-09, 8.9380E-09,      s2603330
     C 9.1815E-09, 9.5092E-09, 1.0027E-08, 1.0876E-08, 1.1744E-08,      s2603340
     C 1.1853E-08, 1.1296E-08, 1.0134E-08, 8.8245E-09, 7.3930E-09,      s2603350
     C 5.7150E-09, 4.4884E-09, 3.4027E-09, 2.6054E-09, 2.0790E-09/      s2603360
      DATA S1401/                                                       s2603370
     C 1.7267E-09, 1.4724E-09, 1.2722E-09, 1.1234E-09, 1.0186E-09,      s2603380
     C 9.4680E-10, 8.8854E-10, 8.5127E-10, 8.3157E-10, 8.2226E-10,      s2603390
     C 8.3395E-10, 8.3294E-10, 8.4725E-10, 8.8814E-10, 9.3697E-10,      s2603400
     C 1.0112E-09, 1.0412E-09, 1.0948E-09, 1.1810E-09, 1.2267E-09,      s2603410
     C 1.3690E-09, 1.4512E-09, 1.5568E-09, 1.6552E-09, 1.7321E-09,      s2603420
     C 1.8797E-09, 1.9210E-09, 1.9686E-09, 1.9917E-09, 1.9357E-09,      s2603430
     C 1.8486E-09, 1.7575E-09, 1.7113E-09, 1.7163E-09, 1.7623E-09,      s2603440
     C 1.8536E-09, 1.9765E-09, 2.1334E-09, 2.3237E-09, 2.3259E-09,      s2603450
     C 2.1833E-09, 1.9785E-09, 1.7308E-09, 1.4596E-09, 1.1198E-09,      s2603460
     C 8.7375E-10, 6.5381E-10, 4.8677E-10, 3.6756E-10, 2.9155E-10/      s2603470
      DATA S1451/                                                       s2603480
     C 2.3735E-10, 1.9590E-10, 1.6638E-10, 1.4549E-10, 1.2947E-10,      s2603490
     C 1.1511E-10, 1.0548E-10, 9.6511E-11, 9.0469E-11, 8.5170E-11,      s2603500
     C 7.7804E-11, 7.1971E-11, 6.6213E-11, 6.1063E-11, 5.5881E-11,      s2603510
     C 5.0508E-11, 4.5932E-11, 4.1997E-11, 3.7672E-11, 3.3972E-11,      s2603520
     C 3.0318E-11, 2.6769E-11, 2.3874E-11, 2.1336E-11, 1.9073E-11,      s2603530
     C 1.7313E-11, 1.5904E-11, 1.4684E-11, 1.3698E-11, 1.2873E-11,      s2603540
     C 1.2175E-11, 1.1542E-11, 1.1024E-11, 1.0602E-11, 1.0267E-11,      s2603550
     C 1.0012E-11, 9.8379E-12, 9.7482E-12, 9.7564E-12, 9.8613E-12,      s2603560
     C 1.0092E-11, 1.0418E-11, 1.0868E-11, 1.1585E-11, 1.2351E-11,      s2603570
     C 1.3372E-11, 1.4841E-11, 1.6457E-11, 1.8681E-11, 2.0550E-11/      s2603580
      DATA S1501/                                                       s2603590
     C 2.2912E-11, 2.5958E-11, 2.9137E-11, 3.2368E-11, 3.4848E-11,      s2603600
     C 3.8462E-11, 4.2190E-11, 4.5629E-11, 4.9022E-11, 5.4232E-11,      s2603610
     C 6.1900E-11, 7.1953E-11, 8.5368E-11, 9.9699E-11, 1.1734E-10,      s2603620
     C 1.4185E-10, 1.7017E-10, 1.9813E-10, 2.3859E-10, 2.7304E-10,      s2603630
     C 3.0971E-10, 3.5129E-10, 3.9405E-10, 4.5194E-10, 4.8932E-10,      s2603640
     C 5.2436E-10, 5.4098E-10, 5.5542E-10, 5.7794E-10, 5.6992E-10,      s2603650
     C 5.8790E-10, 6.1526E-10, 6.8034E-10, 6.7956E-10, 6.6864E-10,      s2603660
     C 6.9329E-10, 7.2971E-10, 7.6546E-10, 7.5078E-10, 7.8406E-10,      s2603670
     C 8.3896E-10, 9.0111E-10, 9.1994E-10, 8.7189E-10, 8.1426E-10,      s2603680
     C 7.3097E-10, 6.3357E-10, 5.1371E-10, 4.0936E-10, 3.2918E-10/      s2603690
      DATA S1551/                                                       s2603700
     C 2.6255E-10, 2.0724E-10, 1.6879E-10, 1.4165E-10, 1.1989E-10,      s2603710
     C 1.0125E-10, 8.9629E-11, 7.8458E-11, 6.8826E-11, 6.0935E-11,      s2603720
     C 5.5208E-11, 5.2262E-11, 5.0260E-11, 4.8457E-11, 4.7888E-11,      s2603730
     C 4.8032E-11, 5.0838E-11, 5.4668E-11, 5.5790E-11, 6.0056E-11,      s2603740
     C 6.3811E-11, 6.8848E-11, 7.4590E-11, 7.8249E-11, 8.3371E-11,      s2603750
     C 8.3641E-11, 8.6591E-11, 8.9599E-11, 9.3487E-11, 1.0066E-10,      s2603760
     C 1.0765E-10, 1.0851E-10, 1.0619E-10, 1.0557E-10, 1.0460E-10,      s2603770
     C 1.0796E-10, 1.0523E-10, 1.0674E-10, 1.1261E-10, 1.1431E-10,      s2603780
     C 1.1408E-10, 1.0901E-10, 9.9105E-11, 8.8077E-11, 6.9928E-11,      s2603790
     C 5.4595E-11, 4.5401E-11, 3.6313E-11, 2.6986E-11, 1.9463E-11/      s2603800
      DATA S1601/                                                       s2603810
     C 1.4577E-11, 1.1583E-11, 9.5492E-12, 8.0770E-12, 6.9642E-12,      s2603820
     C 6.0966E-12, 5.4046E-12, 4.8431E-12, 4.3815E-12, 3.9987E-12,      s2603830
     C 3.6790E-12, 3.4113E-12, 3.1868E-12, 2.9992E-12, 2.8434E-12,      s2603840
     C 2.7153E-12, 2.6120E-12, 2.5311E-12, 2.4705E-12, 2.4290E-12,      s2603850
     C 2.4053E-12, 2.3988E-12, 2.4087E-12, 2.4349E-12, 2.4771E-12,      s2603860
     C 2.5355E-12, 2.6103E-12, 2.7019E-12, 2.8110E-12, 2.9383E-12,      s2603870
     C 3.0848E-12, 3.2518E-12, 3.4405E-12, 3.6527E-12, 3.8902E-12,      s2603880
     C 4.1555E-12, 4.4510E-12, 4.7801E-12, 5.1462E-12, 5.5539E-12,      s2603890
     C 6.0086E-12, 6.5171E-12, 7.0884E-12, 7.7357E-12, 8.4831E-12,      s2603900
     C 9.3096E-12, 1.0282E-11, 1.1407E-11, 1.2690E-11, 1.4148E-11/      s2603910
      DATA S1651/                                                       s2603920
     C 1.5888E-11, 1.7992E-11, 2.0523E-11, 2.3342E-11, 2.6578E-11,      s2603930
     C 3.0909E-11, 3.6228E-11, 4.2053E-11, 4.9059E-11, 5.9273E-11,      s2603940
     C 7.0166E-11, 8.2298E-11, 9.7071E-11, 1.1673E-10, 1.4010E-10,      s2603950
     C 1.6621E-10, 2.0127E-10, 2.3586E-10, 2.7050E-10, 3.0950E-10,      s2603960
     C 3.6584E-10, 4.1278E-10, 4.6591E-10, 5.2220E-10, 5.5246E-10,      s2603970
     C 6.1500E-10, 6.5878E-10, 7.1167E-10, 7.9372E-10, 8.6975E-10,      s2603980
     C 9.6459E-10, 9.7368E-10, 9.8142E-10, 1.0202E-09, 1.0200E-09,      s2603990
     C 1.0356E-09, 1.0092E-09, 1.0269E-09, 1.0366E-09, 1.0490E-09,      s2604000
     C 1.0717E-09, 1.0792E-09, 1.1016E-09, 1.0849E-09, 1.0929E-09,      s2604010
     C 1.0971E-09, 1.0969E-09, 1.0460E-09, 9.2026E-10, 8.1113E-10/      s2604020
      DATA S1701/                                                       s2604030
     C 6.8635E-10, 5.5369E-10, 4.2908E-10, 3.3384E-10, 2.6480E-10,      s2604040
     C 2.0810E-10, 1.6915E-10, 1.4051E-10, 1.1867E-10, 1.0158E-10,      s2604050
     C 8.8990E-11, 7.9175E-11, 7.0440E-11, 6.3453E-11, 5.7009E-11,      s2604060
     C 5.1662E-11, 4.7219E-11, 4.3454E-11, 4.0229E-11, 3.7689E-11,      s2604070
     C 3.6567E-11, 3.5865E-11, 3.5955E-11, 3.5928E-11, 3.6298E-11,      s2604080
     C 3.7629E-11, 3.9300E-11, 4.1829E-11, 4.4806E-11, 5.0534E-11,      s2604090
     C 5.6672E-11, 6.2138E-11, 6.8678E-11, 7.6111E-11, 8.4591E-11,      s2604100
     C 9.2634E-11, 9.8085E-11, 1.0830E-10, 1.1949E-10, 1.2511E-10,      s2604110
     C 1.3394E-10, 1.3505E-10, 1.4342E-10, 1.4874E-10, 1.4920E-10,      s2604120
     C 1.5872E-10, 1.5972E-10, 1.5821E-10, 1.5425E-10, 1.4937E-10/      s2604130
      DATA S1751/                                                       s2604140
     C 1.5089E-10, 1.5521E-10, 1.6325E-10, 1.6924E-10, 1.8265E-10,      s2604150
     C 1.9612E-10, 2.0176E-10, 1.9359E-10, 1.7085E-10, 1.5197E-10,      s2604160
     C 1.2646E-10, 9.8552E-11, 7.4530E-11, 5.5052E-11, 4.2315E-11,      s2604170
     C 3.2736E-11, 2.6171E-11, 2.1909E-11, 1.8286E-11, 1.5752E-11,      s2604180
     C 1.3859E-11, 1.2288E-11, 1.1002E-11, 9.7534E-12, 8.8412E-12,      s2604190
     C 8.0169E-12, 7.2855E-12, 6.8734E-12, 6.4121E-12, 6.1471E-12,      s2604200
     C 5.7780E-12, 5.3478E-12, 4.9652E-12, 4.4043E-12, 3.9862E-12,      s2604210
     C 3.4684E-12, 2.9681E-12, 2.5791E-12, 2.2339E-12, 1.9247E-12,      s2604220
     C 1.6849E-12, 1.4863E-12, 1.3291E-12, 1.2021E-12, 1.0947E-12,      s2604230
     C 1.0015E-12, 9.1935E-13, 8.4612E-13, 7.8036E-13, 7.2100E-13/      s2604240
      DATA S1801/                                                       s2604250
     C 6.6718E-13, 6.1821E-13, 5.7353E-13, 5.3269E-13, 4.9526E-13,      s2604260
     C 4.6093E-13, 4.2937E-13, 4.0034E-13, 3.7361E-13, 3.4895E-13,      s2604270
     C 3.2621E-13, 3.0520E-13, 2.8578E-13, 2.6782E-13, 2.5120E-13,      s2604280
     C 2.3581E-13, 2.2154E-13, 2.0832E-13, 1.9605E-13, 1.8466E-13,      s2604290
     C 1.7408E-13, 1.6425E-13, 1.5511E-13, 1.4661E-13, 1.3869E-13,      s2604300
     C 1.3131E-13, 1.2444E-13, 1.1803E-13, 1.1205E-13, 1.0646E-13,      s2604310
     C 1.0124E-13, 9.6358E-14, 9.1789E-14, 8.7509E-14, 8.3498E-14,      s2604320
     C 7.9735E-14, 7.6202E-14, 7.2882E-14, 6.9760E-14, 6.6822E-14,      s2604330
     C 6.4053E-14, 6.1442E-14, 5.8978E-14, 5.6650E-14, 5.4448E-14,      s2604340
     C 5.2364E-14, 5.0389E-14, 4.8516E-14, 4.6738E-14, 4.5048E-14/      s2604350
      DATA S1851/                                                       s2604360
     C 4.3441E-14, 4.1911E-14, 4.0453E-14, 3.9063E-14, 3.7735E-14,      s2604370
     C 3.6467E-14, 3.5254E-14, 3.4093E-14, 3.2980E-14, 3.1914E-14,      s2604380
     C 3.0891E-14, 2.9909E-14, 2.8965E-14, 2.8058E-14, 2.7185E-14,      s2604390
     C 2.6344E-14, 2.5535E-14, 2.4755E-14, 2.4002E-14, 2.3276E-14,      s2604400
     C 2.2576E-14, 2.1899E-14, 2.1245E-14, 2.0613E-14, 2.0002E-14,      s2604410
     C 1.9411E-14, 1.8839E-14, 1.8285E-14, 1.7749E-14, 1.7230E-14,      s2604420
     C 1.6727E-14, 1.6240E-14, 1.5768E-14, 1.5310E-14, 1.4867E-14,      s2604430
     C 1.4436E-14, 1.4019E-14, 1.3614E-14, 1.3221E-14, 1.2840E-14,      s2604440
     C 1.2471E-14, 1.2112E-14, 1.1764E-14, 1.1425E-14, 1.1097E-14,      s2604450
     C 1.0779E-14, 1.0469E-14, 1.0169E-14, 9.8775E-15, 9.5943E-15/      s2604460
      DATA S1901/                                                       s2604470
     C 9.3193E-15, 9.0522E-15, 8.7928E-15, 8.5409E-15, 8.2962E-15,      s2604480
     C 8.0586E-15, 7.8278E-15, 7.6036E-15, 7.3858E-15, 7.1742E-15,      s2604490
     C 6.9687E-15, 6.7691E-15, 6.5752E-15, 6.3868E-15, 6.2038E-15,      s2604500
     C 6.0260E-15, 5.8533E-15, 5.6856E-15, 5.5226E-15, 5.3642E-15,      s2604510
     C 5.2104E-15, 5.0610E-15, 4.9158E-15, 4.7748E-15, 4.6378E-15,      s2604520
     C 4.5047E-15, 4.3753E-15, 4.2497E-15, 4.1277E-15, 4.0091E-15,      s2604530
     C 3.8939E-15, 3.7820E-15, 3.6733E-15, 3.5677E-15, 3.4651E-15,      s2604540
     C 3.3655E-15, 3.2686E-15, 3.1746E-15, 3.0832E-15, 2.9944E-15,      s2604550
     C 2.9082E-15, 2.8244E-15, 2.7431E-15, 2.6640E-15, 2.5872E-15,      s2604560
     C 2.5126E-15, 2.4401E-15, 2.3697E-15, 2.3014E-15, 2.2349E-15/      s2604570
      DATA S1951/                                                       s2604580
     C 2.1704E-15, 2.1077E-15, 2.0468E-15, 1.9877E-15, 1.9302E-15,      s2604590
     C 1.8744E-15, 1.8202E-15, 1.7675E-15, 1.7164E-15, 1.6667E-15,      s2604600
     C 1.6184E-15, 1.5716E-15, 1.5260E-15, 1.4818E-15, 1.4389E-15,      s2604610
     C 1.3971E-15, 1.3566E-15, 1.3172E-15, 1.2790E-15, 1.2419E-15,      s2604620
     C 1.2058E-15, 1.1708E-15, 1.1368E-15, 1.1037E-15, 1.0716E-15,      s2604630
     C 1.0405E-15, 1.0102E-15, 9.8079E-16, 9.5224E-16, 9.2451E-16,      s2604640
     C 8.9758E-16, 8.7142E-16, 8.4602E-16, 8.2136E-16, 7.9740E-16,      s2604650
     C 7.7414E-16, 7.5154E-16, 7.2961E-16, 7.0830E-16, 6.8761E-16,      s2604660
     C 6.6752E-16, 6.4801E-16, 6.2906E-16, 6.1066E-16, 5.9280E-16,      s2604670
     C 5.7545E-16, 5.5860E-16, 5.4224E-16, 5.2636E-16, 5.1094E-16/      s2604680
      DATA S2001/                                                       s2604690
     C 4.9596E-16/                                                      s2604700
C                                                                       s2604710
      END                                                               s2604720
      BLOCK DATA SF296                                                  s296 100
C>    BLOCK DATA                                                        s296 110
C               06/28/82                                                s296 120
C               UNITS OF (CM**3/MOL) * 1.E-20                           s296 130
      COMMON /SH2O/ V1,V2,DV,NPT,S0000(2),                              s296 140
     1      S0001(50),S0051(50),S0101(50),S0151(50),S0201(50),S0251(50),s296 150
     2      S0301(50),S0351(50),S0401(50),S0451(50),S0501(50),S0551(50),s296 160
     3      S0601(50),S0651(50),S0701(50),S0751(50),S0801(50),S0851(50),s296 170
     4      S0901(50),S0951(50),S1001(50),S1051(50),S1101(50),S1151(50),s296 180
     5      S1201(50),S1251(50),S1301(50),S1351(50),S1401(50),S1451(50),s296 190
     6      S1501(50),S1551(50),S1601(50),S1651(50),S1701(50),S1751(50),s296 200
     7      S1801(50),S1851(50),S1901(50),S1951(50),S2001(1)            s296 210
C                                                                       s296 220
C                                                                       s296 230
       DATA V1,V2,DV,NPT /                                              s296 240
     1      -20.0,     20000.0,       10.0,  2003/                      s296 250
C                                                                       s296 260
C                                                                       s296 270
      DATA S0000/ 1.1109E-01 ,1.0573E-01/                               s296 280
      DATA S0001/                                                       s296 290
     C 1.0162E-01, 1.0573E-01, 1.1109E-01, 1.2574E-01, 1.3499E-01,      s296 300
     C 1.4327E-01, 1.5065E-01, 1.5164E-01, 1.5022E-01, 1.3677E-01,      s296 310
     C 1.3115E-01, 1.2253E-01, 1.1271E-01, 1.0070E-01, 8.7495E-02,      s296 320
     C 8.0118E-02, 6.9940E-02, 6.2034E-02, 5.6051E-02, 4.7663E-02,      s296 330
     C 4.2450E-02, 3.6690E-02, 3.3441E-02, 3.0711E-02, 2.5205E-02,      s296 340
     C 2.2113E-02, 1.8880E-02, 1.6653E-02, 1.4626E-02, 1.2065E-02,      s296 350
     C 1.0709E-02, 9.1783E-03, 7.7274E-03, 6.7302E-03, 5.6164E-03,      s296 360
     C 4.9089E-03, 4.1497E-03, 3.5823E-03, 3.1124E-03, 2.6414E-03,      s296 370
     C 2.3167E-03, 2.0156E-03, 1.7829E-03, 1.5666E-03, 1.3928E-03,      s296 380
     C 1.2338E-03, 1.0932E-03, 9.7939E-04, 8.8241E-04, 7.9173E-04/      s296 390
      DATA S0051/                                                       s296 400
     C 7.1296E-04, 6.4179E-04, 5.8031E-04, 5.2647E-04, 4.7762E-04,      s296 410
     C 4.3349E-04, 3.9355E-04, 3.5887E-04, 3.2723E-04, 2.9919E-04,      s296 420
     C 2.7363E-04, 2.5013E-04, 2.2876E-04, 2.0924E-04, 1.9193E-04,      s296 430
     C 1.7618E-04, 1.6188E-04, 1.4891E-04, 1.3717E-04, 1.2647E-04,      s296 440
     C 1.1671E-04, 1.0786E-04, 9.9785E-05, 9.2350E-05, 8.5539E-05,      s296 450
     C 7.9377E-05, 7.3781E-05, 6.8677E-05, 6.3993E-05, 5.9705E-05,      s296 460
     C 5.5788E-05, 5.2196E-05, 4.8899E-05, 4.5865E-05, 4.3079E-05,      s296 470
     C 4.0526E-05, 3.8182E-05, 3.6025E-05, 3.4038E-05, 3.2203E-05,      s296 480
     C 3.0511E-05, 2.8949E-05, 2.7505E-05, 2.6170E-05, 2.4933E-05,      s296 490
     C 2.3786E-05, 2.2722E-05, 2.1736E-05, 2.0819E-05, 1.9968E-05/      s296 500
      DATA S0101/                                                       s296 510
     C 1.9178E-05, 1.8442E-05, 1.7760E-05, 1.7127E-05, 1.6541E-05,      s296 520
     C 1.5997E-05, 1.5495E-05, 1.5034E-05, 1.4614E-05, 1.4230E-05,      s296 530
     C 1.3883E-05, 1.3578E-05, 1.3304E-05, 1.3069E-05, 1.2876E-05,      s296 540
     C 1.2732E-05, 1.2626E-05, 1.2556E-05, 1.2544E-05, 1.2604E-05,      s296 550
     C 1.2719E-05, 1.2883E-05, 1.3164E-05, 1.3581E-05, 1.4187E-05,      s296 560
     C 1.4866E-05, 1.5669E-05, 1.6717E-05, 1.8148E-05, 2.0268E-05,      s296 570
     C 2.2456E-05, 2.5582E-05, 2.9183E-05, 3.3612E-05, 3.9996E-05,      s296 580
     C 4.6829E-05, 5.5055E-05, 6.5897E-05, 7.5360E-05, 8.7213E-05,      s296 590
     C 1.0046E-04, 1.1496E-04, 1.2943E-04, 1.5049E-04, 1.6973E-04,      s296 600
     C 1.8711E-04, 2.0286E-04, 2.2823E-04, 2.6780E-04, 2.8766E-04/      s296 610
      DATA S0151/                                                       s296 620
     C 3.1164E-04, 3.3640E-04, 3.6884E-04, 3.9159E-04, 3.8712E-04,      s296 630
     C 3.7433E-04, 3.4503E-04, 3.1003E-04, 2.8027E-04, 2.5253E-04,      s296 640
     C 2.3408E-04, 2.2836E-04, 2.4442E-04, 2.7521E-04, 2.9048E-04,      s296 650
     C 3.0489E-04, 3.2646E-04, 3.3880E-04, 3.3492E-04, 3.0987E-04,      s296 660
     C 2.9482E-04, 2.8711E-04, 2.6068E-04, 2.2683E-04, 1.9996E-04,      s296 670
     C 1.7788E-04, 1.6101E-04, 1.3911E-04, 1.2013E-04, 1.0544E-04,      s296 680
     C 9.4224E-05, 8.1256E-05, 7.3667E-05, 6.2233E-05, 5.5906E-05,      s296 690
     C 5.1619E-05, 4.5140E-05, 4.0273E-05, 3.3268E-05, 3.0258E-05,      s296 700
     C 2.6440E-05, 2.3103E-05, 2.0749E-05, 1.8258E-05, 1.6459E-05,      s296 710
     C 1.4097E-05, 1.2052E-05, 1.0759E-05, 9.1400E-06, 8.1432E-06/      s296 720
      DATA S0201/                                                       s296 730
     C 7.1460E-06, 6.4006E-06, 5.6995E-06, 4.9372E-06, 4.4455E-06,      s296 740
     C 3.9033E-06, 3.4740E-06, 3.1269E-06, 2.8059E-06, 2.5558E-06,      s296 750
     C 2.2919E-06, 2.0846E-06, 1.8983E-06, 1.7329E-06, 1.5929E-06,      s296 760
     C 1.4631E-06, 1.3513E-06, 1.2461E-06, 1.1519E-06, 1.0682E-06,      s296 770
     C 9.9256E-07, 9.2505E-07, 8.6367E-07, 8.0857E-07, 7.5674E-07,      s296 780
     C 7.0934E-07, 6.6580E-07, 6.2580E-07, 5.8853E-07, 5.5333E-07,      s296 790
     C 5.2143E-07, 4.9169E-07, 4.6431E-07, 4.3898E-07, 4.1564E-07,      s296 800
     C 3.9405E-07, 3.7403E-07, 3.5544E-07, 3.3819E-07, 3.2212E-07,      s296 810
     C 3.0714E-07, 2.9313E-07, 2.8003E-07, 2.6777E-07, 2.5628E-07,      s296 820
     C 2.4551E-07, 2.3540E-07, 2.2591E-07, 2.1701E-07, 2.0866E-07/      s296 830
      DATA S0251/                                                       s296 840
     C 2.0082E-07, 1.9349E-07, 1.8665E-07, 1.8027E-07, 1.7439E-07,      s296 850
     C 1.6894E-07, 1.6400E-07, 1.5953E-07, 1.5557E-07, 1.5195E-07,      s296 860
     C 1.4888E-07, 1.4603E-07, 1.4337E-07, 1.4093E-07, 1.3828E-07,      s296 870
     C 1.3569E-07, 1.3270E-07, 1.2984E-07, 1.2714E-07, 1.2541E-07,      s296 880
     C 1.2399E-07, 1.2102E-07, 1.1878E-07, 1.1728E-07, 1.1644E-07,      s296 890
     C 1.1491E-07, 1.1305E-07, 1.1235E-07, 1.1228E-07, 1.1224E-07,      s296 900
     C 1.1191E-07, 1.1151E-07, 1.1098E-07, 1.1068E-07, 1.1109E-07,      s296 910
     C 1.1213E-07, 1.1431E-07, 1.1826E-07, 1.2322E-07, 1.3025E-07,      s296 920
     C 1.4066E-07, 1.5657E-07, 1.7214E-07, 1.9449E-07, 2.2662E-07,      s296 930
     C 2.6953E-07, 3.1723E-07, 3.7028E-07, 4.4482E-07, 5.3852E-07/      s296 940
      DATA S0301/                                                       s296 950
     C 6.2639E-07, 7.2175E-07, 7.7626E-07, 8.7248E-07, 9.6759E-07,      s296 960
     C 1.0102E-06, 1.0620E-06, 1.1201E-06, 1.2107E-06, 1.2998E-06,      s296 970
     C 1.3130E-06, 1.2856E-06, 1.2350E-06, 1.1489E-06, 1.0819E-06,      s296 980
     C 1.0120E-06, 9.4795E-07, 9.2858E-07, 9.8060E-07, 1.0999E-06,      s296 990
     C 1.1967E-06, 1.2672E-06, 1.3418E-06, 1.3864E-06, 1.4330E-06,      s2961000
     C 1.4592E-06, 1.4598E-06, 1.4774E-06, 1.4726E-06, 1.4820E-06,      s2961010
     C 1.5050E-06, 1.4984E-06, 1.5181E-06, 1.5888E-06, 1.6850E-06,      s2961020
     C 1.7690E-06, 1.9277E-06, 2.1107E-06, 2.3068E-06, 2.5347E-06,      s2961030
     C 2.8069E-06, 3.1345E-06, 3.5822E-06, 3.9051E-06, 4.3422E-06,      s2961040
     C 4.8704E-06, 5.5351E-06, 6.3454E-06, 7.2690E-06, 8.2974E-06/      s2961050
      DATA S0351/                                                       s2961060
     C 9.7609E-06, 1.1237E-05, 1.3187E-05, 1.5548E-05, 1.8784E-05,      s2961070
     C 2.1694E-05, 2.5487E-05, 3.0092E-05, 3.5385E-05, 4.2764E-05,      s2961080
     C 4.9313E-05, 5.5800E-05, 6.2968E-05, 7.1060E-05, 7.7699E-05,      s2961090
     C 8.7216E-05, 8.9335E-05, 9.2151E-05, 9.2779E-05, 9.4643E-05,      s2961100
     C 9.7978E-05, 1.0008E-04, 1.0702E-04, 1.1026E-04, 1.0828E-04,      s2961110
     C 1.0550E-04, 1.0432E-04, 1.0428E-04, 9.8980E-05, 9.4992E-05,      s2961120
     C 9.5159E-05, 1.0058E-04, 1.0738E-04, 1.1550E-04, 1.1229E-04,      s2961130
     C 1.0596E-04, 1.0062E-04, 9.1742E-05, 8.4492E-05, 6.8099E-05,      s2961140
     C 5.6295E-05, 4.6502E-05, 3.8071E-05, 3.0721E-05, 2.3297E-05,      s2961150
     C 1.8688E-05, 1.4830E-05, 1.2049E-05, 9.6754E-06, 7.9192E-06/      s2961160
      DATA S0401/                                                       s2961170
     C 6.6673E-06, 5.6468E-06, 4.8904E-06, 4.2289E-06, 3.6880E-06,      s2961180
     C 3.2396E-06, 2.8525E-06, 2.5363E-06, 2.2431E-06, 1.9949E-06,      s2961190
     C 1.7931E-06, 1.6164E-06, 1.4431E-06, 1.2997E-06, 1.1559E-06,      s2961200
     C 1.0404E-06, 9.4300E-07, 8.4597E-07, 7.6133E-07, 6.8623E-07,      s2961210
     C 6.2137E-07, 5.6345E-07, 5.1076E-07, 4.6246E-07, 4.1906E-07,      s2961220
     C 3.8063E-07, 3.4610E-07, 3.1554E-07, 2.8795E-07, 2.6252E-07,      s2961230
     C 2.3967E-07, 2.1901E-07, 2.0052E-07, 1.8384E-07, 1.6847E-07,      s2961240
     C 1.5459E-07, 1.4204E-07, 1.3068E-07, 1.2036E-07, 1.1095E-07,      s2961250
     C 1.0237E-07, 9.4592E-08, 8.7530E-08, 8.1121E-08, 7.5282E-08,      s2961260
     C 6.9985E-08, 6.5189E-08, 6.0874E-08, 5.6989E-08, 5.3530E-08/      s2961270
      DATA S0451/                                                       s2961280
     C 5.0418E-08, 4.7745E-08, 4.5367E-08, 4.3253E-08, 4.1309E-08,      s2961290
     C 3.9695E-08, 3.8094E-08, 3.6482E-08, 3.4897E-08, 3.3500E-08,      s2961300
     C 3.2302E-08, 3.0854E-08, 2.9698E-08, 2.8567E-08, 2.7600E-08,      s2961310
     C 2.6746E-08, 2.5982E-08, 2.5510E-08, 2.5121E-08, 2.4922E-08,      s2961320
     C 2.4909E-08, 2.5013E-08, 2.5216E-08, 2.5589E-08, 2.6049E-08,      s2961330
     C 2.6451E-08, 2.6978E-08, 2.7687E-08, 2.8600E-08, 2.9643E-08,      s2961340
     C 3.0701E-08, 3.2058E-08, 3.3695E-08, 3.5558E-08, 3.7634E-08,      s2961350
     C 3.9875E-08, 4.2458E-08, 4.5480E-08, 4.8858E-08, 5.2599E-08,      s2961360
     C 5.7030E-08, 6.2067E-08, 6.7911E-08, 7.4579E-08, 8.1902E-08,      s2961370
     C 8.9978E-08, 9.9870E-08, 1.1102E-07, 1.2343E-07, 1.3732E-07/      s2961380
      DATA S0501/                                                       s2961390
     C 1.5394E-07, 1.7318E-07, 1.9383E-07, 2.1819E-07, 2.4666E-07,      s2961400
     C 2.8109E-07, 3.2236E-07, 3.7760E-07, 4.4417E-07, 5.2422E-07,      s2961410
     C 6.1941E-07, 7.4897E-07, 9.2041E-07, 1.1574E-06, 1.4126E-06,      s2961420
     C 1.7197E-06, 2.1399E-06, 2.6266E-06, 3.3424E-06, 3.8418E-06,      s2961430
     C 4.5140E-06, 5.0653E-06, 5.8485E-06, 6.5856E-06, 6.8937E-06,      s2961440
     C 6.9121E-06, 6.9005E-06, 6.9861E-06, 6.8200E-06, 6.6089E-06,      s2961450
     C 6.5809E-06, 7.3496E-06, 8.0311E-06, 8.3186E-06, 8.4260E-06,      s2961460
     C 9.0644E-06, 9.4965E-06, 9.4909E-06, 9.0160E-06, 9.1494E-06,      s2961470
     C 9.3629E-06, 9.5944E-06, 9.5459E-06, 8.9919E-06, 8.6040E-06,      s2961480
     C 7.8613E-06, 7.1567E-06, 6.2677E-06, 5.1899E-06, 4.4188E-06/      s2961490
      DATA S0551/                                                       s2961500
     C 3.7167E-06, 3.0636E-06, 2.5573E-06, 2.0317E-06, 1.6371E-06,      s2961510
     C 1.3257E-06, 1.0928E-06, 8.9986E-07, 7.4653E-07, 6.1111E-07,      s2961520
     C 5.1395E-07, 4.3500E-07, 3.7584E-07, 3.2633E-07, 2.8413E-07,      s2961530
     C 2.4723E-07, 2.1709E-07, 1.9294E-07, 1.7258E-07, 1.5492E-07,      s2961540
     C 1.3820E-07, 1.2389E-07, 1.1189E-07, 1.0046E-07, 9.0832E-08,      s2961550
     C 8.2764E-08, 7.4191E-08, 6.7085E-08, 6.0708E-08, 5.4963E-08,      s2961560
     C 4.9851E-08, 4.5044E-08, 4.0916E-08, 3.7220E-08, 3.3678E-08,      s2961570
     C 3.0663E-08, 2.7979E-08, 2.5495E-08, 2.3286E-08, 2.1233E-08,      s2961580
     C 1.9409E-08, 1.7770E-08, 1.6260E-08, 1.4885E-08, 1.3674E-08,      s2961590
     C 1.2543E-08, 1.1551E-08, 1.0655E-08, 9.8585E-09, 9.1398E-09/      s2961600
      DATA S0601/                                                       s2961610
     C 8.4806E-09, 7.8899E-09, 7.3547E-09, 6.8670E-09, 6.4131E-09,      s2961620
     C 5.9930E-09, 5.6096E-09, 5.2592E-09, 4.9352E-09, 4.6354E-09,      s2961630
     C 4.3722E-09, 4.1250E-09, 3.9081E-09, 3.7118E-09, 3.5372E-09,      s2961640
     C 3.3862E-09, 3.2499E-09, 3.1324E-09, 3.0313E-09, 2.9438E-09,      s2961650
     C 2.8686E-09, 2.8050E-09, 2.7545E-09, 2.7149E-09, 2.6907E-09,      s2961660
     C 2.6724E-09, 2.6649E-09, 2.6642E-09, 2.6725E-09, 2.6871E-09,      s2961670
     C 2.7056E-09, 2.7357E-09, 2.7781E-09, 2.8358E-09, 2.9067E-09,      s2961680
     C 2.9952E-09, 3.1020E-09, 3.2253E-09, 3.3647E-09, 3.5232E-09,      s2961690
     C 3.7037E-09, 3.9076E-09, 4.1385E-09, 4.3927E-09, 4.6861E-09,      s2961700
     C 5.0238E-09, 5.4027E-09, 5.8303E-09, 6.3208E-09, 6.8878E-09/      s2961710
      DATA S0651/                                                       s2961720
     C 7.5419E-09, 8.3130E-09, 9.1952E-09, 1.0228E-08, 1.1386E-08,      s2961730
     C 1.2792E-08, 1.4521E-08, 1.6437E-08, 1.8674E-08, 2.1160E-08,      s2961740
     C 2.4506E-08, 2.8113E-08, 3.2636E-08, 3.7355E-08, 4.2234E-08,      s2961750
     C 4.9282E-08, 5.7358E-08, 6.6743E-08, 7.8821E-08, 9.4264E-08,      s2961760
     C 1.1542E-07, 1.3684E-07, 1.6337E-07, 2.0056E-07, 2.3252E-07,      s2961770
     C 2.6127E-07, 2.9211E-07, 3.3804E-07, 3.7397E-07, 3.8205E-07,      s2961780
     C 3.8810E-07, 3.9499E-07, 3.9508E-07, 3.7652E-07, 3.5859E-07,      s2961790
     C 3.6198E-07, 3.7871E-07, 4.0925E-07, 4.2717E-07, 4.8241E-07,      s2961800
     C 5.2008E-07, 5.6530E-07, 5.9531E-07, 6.1994E-07, 6.5080E-07,      s2961810
     C 6.6355E-07, 6.9193E-07, 6.9930E-07, 7.3058E-07, 7.4678E-07/      s2961820
      DATA S0701/                                                       s2961830
     C 7.9193E-07, 8.3627E-07, 9.1267E-07, 1.0021E-06, 1.1218E-06,      s2961840
     C 1.2899E-06, 1.4447E-06, 1.7268E-06, 2.0025E-06, 2.3139E-06,      s2961850
     C 2.5599E-06, 2.8920E-06, 3.3059E-06, 3.5425E-06, 3.9522E-06,      s2961860
     C 4.0551E-06, 4.2818E-06, 4.2892E-06, 4.4210E-06, 4.5614E-06,      s2961870
     C 4.6739E-06, 4.9482E-06, 5.1118E-06, 5.0986E-06, 4.9417E-06,      s2961880
     C 4.9022E-06, 4.8449E-06, 4.8694E-06, 4.8111E-06, 4.9378E-06,      s2961890
     C 5.3231E-06, 5.7362E-06, 6.2350E-06, 6.0951E-06, 5.7281E-06,      s2961900
     C 5.4585E-06, 4.9032E-06, 4.3009E-06, 3.4776E-06, 2.8108E-06,      s2961910
     C 2.2993E-06, 1.7999E-06, 1.3870E-06, 1.0750E-06, 8.5191E-07,      s2961920
     C 6.7951E-07, 5.5336E-07, 4.6439E-07, 4.0243E-07, 3.5368E-07/      s2961930
      DATA S0751/                                                       s2961940
     C 3.1427E-07, 2.7775E-07, 2.4486E-07, 2.1788E-07, 1.9249E-07,      s2961950
     C 1.7162E-07, 1.5115E-07, 1.3478E-07, 1.2236E-07, 1.1139E-07,      s2961960
     C 1.0092E-07, 9.0795E-08, 8.2214E-08, 7.4691E-08, 6.7486E-08,      s2961970
     C 6.0414E-08, 5.4584E-08, 4.8754E-08, 4.3501E-08, 3.8767E-08,      s2961980
     C 3.4363E-08, 3.0703E-08, 2.7562E-08, 2.4831E-08, 2.2241E-08,      s2961990
     C 1.9939E-08, 1.8049E-08, 1.6368E-08, 1.4863E-08, 1.3460E-08,      s2962000
     C 1.2212E-08, 1.1155E-08, 1.0185E-08, 9.3417E-09, 8.5671E-09,      s2962010
     C 7.8292E-09, 7.1749E-09, 6.5856E-09, 6.0588E-09, 5.5835E-09,      s2962020
     C 5.1350E-09, 4.7395E-09, 4.3771E-09, 4.0476E-09, 3.7560E-09,      s2962030
     C 3.4861E-09, 3.2427E-09, 3.0240E-09, 2.8278E-09, 2.6531E-09/      s2962040
      DATA S0801/                                                       s2962050
     C 2.4937E-09, 2.3511E-09, 2.2245E-09, 2.1133E-09, 2.0159E-09,      s2962060
     C 1.9330E-09, 1.8669E-09, 1.8152E-09, 1.7852E-09, 1.7752E-09,      s2962070
     C 1.7823E-09, 1.8194E-09, 1.8866E-09, 1.9759E-09, 2.0736E-09,      s2962080
     C 2.2083E-09, 2.3587E-09, 2.4984E-09, 2.6333E-09, 2.8160E-09,      s2962090
     C 3.0759E-09, 3.3720E-09, 3.6457E-09, 4.0668E-09, 4.4541E-09,      s2962100
     C 4.7976E-09, 5.0908E-09, 5.4811E-09, 6.1394E-09, 6.3669E-09,      s2962110
     C 6.5714E-09, 6.8384E-09, 7.1918E-09, 7.3741E-09, 7.2079E-09,      s2962120
     C 7.2172E-09, 7.2572E-09, 7.3912E-09, 7.6188E-09, 8.3291E-09,      s2962130
     C 8.7885E-09, 9.2412E-09, 1.0021E-08, 1.0752E-08, 1.1546E-08,      s2962140
     C 1.1607E-08, 1.1949E-08, 1.2346E-08, 1.2516E-08, 1.2826E-08/      s2962150
      DATA S0851/                                                       s2962160
     C 1.3053E-08, 1.3556E-08, 1.4221E-08, 1.5201E-08, 1.6661E-08,      s2962170
     C 1.8385E-08, 2.0585E-08, 2.3674E-08, 2.7928E-08, 3.3901E-08,      s2962180
     C 4.1017E-08, 4.9595E-08, 6.0432E-08, 7.6304E-08, 9.0764E-08,      s2962190
     C 1.0798E-07, 1.2442E-07, 1.4404E-07, 1.6331E-07, 1.8339E-07,      s2962200
     C 2.0445E-07, 2.2288E-07, 2.3083E-07, 2.3196E-07, 2.3919E-07,      s2962210
     C 2.3339E-07, 2.3502E-07, 2.3444E-07, 2.6395E-07, 2.9928E-07,      s2962220
     C 3.0025E-07, 3.0496E-07, 3.1777E-07, 3.4198E-07, 3.4739E-07,      s2962230
     C 3.2696E-07, 3.4100E-07, 3.5405E-07, 3.7774E-07, 3.8285E-07,      s2962240
     C 3.6797E-07, 3.5800E-07, 3.2283E-07, 2.9361E-07, 2.4881E-07,      s2962250
     C 2.0599E-07, 1.7121E-07, 1.3641E-07, 1.1111E-07, 8.9413E-08/      s2962260
      DATA S0901/                                                       s2962270
     C 7.3455E-08, 6.2078E-08, 5.2538E-08, 4.5325E-08, 3.9005E-08,      s2962280
     C 3.4772E-08, 3.1203E-08, 2.8132E-08, 2.5250E-08, 2.2371E-08,      s2962290
     C 2.0131E-08, 1.7992E-08, 1.6076E-08, 1.4222E-08, 1.2490E-08,      s2962300
     C 1.1401E-08, 1.0249E-08, 9.2279E-09, 8.5654E-09, 7.6227E-09,      s2962310
     C 6.9648E-09, 6.2466E-09, 5.7252E-09, 5.3800E-09, 4.6960E-09,      s2962320
     C 4.2194E-09, 3.7746E-09, 3.3813E-09, 3.0656E-09, 2.6885E-09,      s2962330
     C 2.4311E-09, 2.1572E-09, 1.8892E-09, 1.7038E-09, 1.4914E-09,      s2962340
     C 1.3277E-09, 1.1694E-09, 1.0391E-09, 9.2779E-10, 8.3123E-10,      s2962350
     C 7.4968E-10, 6.8385E-10, 6.2915E-10, 5.7784E-10, 5.2838E-10,      s2962360
     C 4.8382E-10, 4.4543E-10, 4.1155E-10, 3.7158E-10, 3.3731E-10/      s2962370
      DATA S0951/                                                       s2962380
     C 3.0969E-10, 2.8535E-10, 2.6416E-10, 2.4583E-10, 2.2878E-10,      s2962390
     C 2.1379E-10, 2.0073E-10, 1.8907E-10, 1.7866E-10, 1.6936E-10,      s2962400
     C 1.6119E-10, 1.5424E-10, 1.4847E-10, 1.4401E-10, 1.4068E-10,      s2962410
     C 1.3937E-10, 1.3943E-10, 1.4281E-10, 1.4766E-10, 1.5701E-10,      s2962420
     C 1.7079E-10, 1.8691E-10, 2.0081E-10, 2.1740E-10, 2.4847E-10,      s2962430
     C 2.6463E-10, 2.7087E-10, 2.7313E-10, 2.8352E-10, 2.9511E-10,      s2962440
     C 2.8058E-10, 2.7227E-10, 2.7356E-10, 2.8012E-10, 2.8034E-10,      s2962450
     C 2.9031E-10, 3.1030E-10, 3.3745E-10, 3.8152E-10, 4.0622E-10,      s2962460
     C 4.2673E-10, 4.3879E-10, 4.5488E-10, 4.7179E-10, 4.6140E-10,      s2962470
     C 4.6339E-10, 4.6716E-10, 4.7024E-10, 4.7931E-10, 4.8503E-10/      s2962480
      DATA S1001/                                                       s2962490
     C 4.9589E-10, 4.9499E-10, 5.0363E-10, 5.3184E-10, 5.6451E-10,      s2962500
     C 6.0932E-10, 6.6469E-10, 7.4076E-10, 8.3605E-10, 9.4898E-10,      s2962510
     C 1.0935E-09, 1.2593E-09, 1.4913E-09, 1.8099E-09, 2.1842E-09,      s2962520
     C 2.7284E-09, 3.2159E-09, 3.7426E-09, 4.5226E-09, 5.3512E-09,      s2962530
     C 6.1787E-09, 6.8237E-09, 7.9421E-09, 9.0002E-09, 9.6841E-09,      s2962540
     C 9.9558E-09, 1.0232E-08, 1.0591E-08, 1.0657E-08, 1.0441E-08,      s2962550
     C 1.0719E-08, 1.1526E-08, 1.2962E-08, 1.4336E-08, 1.6150E-08,      s2962560
     C 1.8417E-08, 2.0725E-08, 2.3426E-08, 2.5619E-08, 2.7828E-08,      s2962570
     C 3.0563E-08, 3.3438E-08, 3.6317E-08, 4.0400E-08, 4.4556E-08,      s2962580
     C 5.0397E-08, 5.3315E-08, 5.9185E-08, 6.5311E-08, 6.9188E-08/      s2962590
      DATA S1051/                                                       s2962600
     C 7.7728E-08, 7.9789E-08, 8.6598E-08, 8.7768E-08, 9.1773E-08,      s2962610
     C 9.7533E-08, 1.0007E-07, 1.0650E-07, 1.0992E-07, 1.0864E-07,      s2962620
     C 1.0494E-07, 1.0303E-07, 1.0031E-07, 1.0436E-07, 1.0537E-07,      s2962630
     C 1.1184E-07, 1.2364E-07, 1.3651E-07, 1.4881E-07, 1.4723E-07,      s2962640
     C 1.4118E-07, 1.3371E-07, 1.1902E-07, 1.0007E-07, 7.9628E-08,      s2962650
     C 6.4362E-08, 5.0243E-08, 3.8133E-08, 2.9400E-08, 2.3443E-08,      s2962660
     C 1.9319E-08, 1.6196E-08, 1.4221E-08, 1.2817E-08, 1.1863E-08,      s2962670
     C 1.1383E-08, 1.1221E-08, 1.1574E-08, 1.1661E-08, 1.2157E-08,      s2962680
     C 1.2883E-08, 1.3295E-08, 1.4243E-08, 1.4240E-08, 1.4614E-08,      s2962690
     C 1.4529E-08, 1.4685E-08, 1.4974E-08, 1.4790E-08, 1.4890E-08/      s2962700
      DATA S1101/                                                       s2962710
     C 1.4704E-08, 1.4142E-08, 1.3374E-08, 1.2746E-08, 1.2172E-08,      s2962720
     C 1.2336E-08, 1.2546E-08, 1.3065E-08, 1.4090E-08, 1.5215E-08,      s2962730
     C 1.6540E-08, 1.6144E-08, 1.5282E-08, 1.4358E-08, 1.2849E-08,      s2962740
     C 1.0998E-08, 8.6956E-09, 7.0881E-09, 5.5767E-09, 4.2792E-09,      s2962750
     C 3.2233E-09, 2.5020E-09, 1.9985E-09, 1.5834E-09, 1.3015E-09,      s2962760
     C 1.0948E-09, 9.4141E-10, 8.1465E-10, 7.1517E-10, 6.2906E-10,      s2962770
     C 5.5756E-10, 4.9805E-10, 4.3961E-10, 3.9181E-10, 3.5227E-10,      s2962780
     C 3.1670E-10, 2.8667E-10, 2.5745E-10, 2.3212E-10, 2.0948E-10,      s2962790
     C 1.8970E-10, 1.7239E-10, 1.5659E-10, 1.4301E-10, 1.3104E-10,      s2962800
     C 1.2031E-10, 1.1095E-10, 1.0262E-10, 9.5130E-11, 8.8595E-11/      s2962810
      DATA S1151/                                                       s2962820
     C 8.2842E-11, 7.7727E-11, 7.3199E-11, 6.9286E-11, 6.5994E-11,      s2962830
     C 6.3316E-11, 6.1244E-11, 5.9669E-11, 5.8843E-11, 5.8832E-11,      s2962840
     C 5.9547E-11, 6.1635E-11, 6.4926E-11, 7.0745E-11, 7.8802E-11,      s2962850
     C 8.6724E-11, 1.0052E-10, 1.1575E-10, 1.3626E-10, 1.5126E-10,      s2962860
     C 1.6751E-10, 1.9239E-10, 2.1748E-10, 2.2654E-10, 2.2902E-10,      s2962870
     C 2.3240E-10, 2.4081E-10, 2.3930E-10, 2.2378E-10, 2.2476E-10,      s2962880
     C 2.2791E-10, 2.4047E-10, 2.5305E-10, 2.8073E-10, 3.1741E-10,      s2962890
     C 3.6592E-10, 4.1495E-10, 4.6565E-10, 5.0990E-10, 5.5607E-10,      s2962900
     C 6.1928E-10, 6.6779E-10, 7.3350E-10, 8.1434E-10, 8.9635E-10,      s2962910
     C 9.9678E-10, 1.1256E-09, 1.2999E-09, 1.4888E-09, 1.7642E-09/      s2962920
      DATA S1201/                                                       s2962930
     C 1.9606E-09, 2.2066E-09, 2.4601E-09, 2.7218E-09, 3.0375E-09,      s2962940
     C 3.1591E-09, 3.2852E-09, 3.2464E-09, 3.3046E-09, 3.2710E-09,      s2962950
     C 3.2601E-09, 3.3398E-09, 3.7446E-09, 4.0795E-09, 4.0284E-09,      s2962960
     C 4.0584E-09, 4.1677E-09, 4.5358E-09, 4.4097E-09, 4.2744E-09,      s2962970
     C 4.5449E-09, 4.8147E-09, 5.2656E-09, 5.2476E-09, 5.0275E-09,      s2962980
     C 4.7968E-09, 4.3654E-09, 3.9530E-09, 3.2447E-09, 2.6489E-09,      s2962990
     C 2.1795E-09, 1.7880E-09, 1.4309E-09, 1.1256E-09, 9.1903E-10,      s2963000
     C 7.6533E-10, 6.3989E-10, 5.5496E-10, 4.9581E-10, 4.5722E-10,      s2963010
     C 4.3898E-10, 4.3505E-10, 4.3671E-10, 4.5329E-10, 4.6827E-10,      s2963020
     C 4.9394E-10, 5.1122E-10, 5.1649E-10, 5.0965E-10, 4.9551E-10/      s2963030
      DATA S1251/                                                       s2963040
     C 4.8928E-10, 4.7947E-10, 4.7989E-10, 4.9071E-10, 4.8867E-10,      s2963050
     C 4.7260E-10, 4.5756E-10, 4.5400E-10, 4.5993E-10, 4.4042E-10,      s2963060
     C 4.3309E-10, 4.4182E-10, 4.6735E-10, 5.0378E-10, 5.2204E-10,      s2963070
     C 5.0166E-10, 4.6799E-10, 4.3119E-10, 3.8803E-10, 3.3291E-10,      s2963080
     C 2.6289E-10, 2.1029E-10, 1.7011E-10, 1.3345E-10, 1.0224E-10,      s2963090
     C 7.8207E-11, 6.2451E-11, 5.0481E-11, 4.1507E-11, 3.5419E-11,      s2963100
     C 3.0582E-11, 2.6900E-11, 2.3778E-11, 2.1343E-11, 1.9182E-11,      s2963110
     C 1.7162E-11, 1.5391E-11, 1.3877E-11, 1.2619E-11, 1.1450E-11,      s2963120
     C 1.0461E-11, 9.6578E-12, 8.9579E-12, 8.3463E-12, 7.8127E-12,      s2963130
     C 7.3322E-12, 6.9414E-12, 6.6037E-12, 6.3285E-12, 6.1095E-12/      s2963140
      DATA S1301/                                                       s2963150
     C 5.9387E-12, 5.8118E-12, 5.7260E-12, 5.6794E-12, 5.6711E-12,      s2963160
     C 5.7003E-12, 5.7670E-12, 5.8717E-12, 6.0151E-12, 6.1984E-12,      s2963170
     C 6.4232E-12, 6.6918E-12, 7.0065E-12, 7.3705E-12, 7.7873E-12,      s2963180
     C 8.2612E-12, 8.7972E-12, 9.4009E-12, 1.0079E-11, 1.0840E-11,      s2963190
     C 1.1692E-11, 1.2648E-11, 1.3723E-11, 1.4935E-11, 1.6313E-11,      s2963200
     C 1.7905E-11, 1.9740E-11, 2.1898E-11, 2.4419E-11, 2.7426E-11,      s2963210
     C 3.0869E-11, 3.4235E-11, 3.7841E-11, 4.1929E-11, 4.6776E-11,      s2963220
     C 5.2123E-11, 5.8497E-11, 6.5294E-11, 7.4038E-11, 8.4793E-11,      s2963230
     C 9.6453E-11, 1.1223E-10, 1.2786E-10, 1.4882E-10, 1.7799E-10,      s2963240
     C 2.0766E-10, 2.4523E-10, 2.8591E-10, 3.3386E-10, 4.0531E-10/      s2963250
      DATA S1351/                                                       s2963260
     C 4.7663E-10, 5.4858E-10, 6.3377E-10, 7.1688E-10, 8.4184E-10,      s2963270
     C 9.5144E-10, 1.0481E-09, 1.1356E-09, 1.2339E-09, 1.3396E-09,      s2963280
     C 1.4375E-09, 1.5831E-09, 1.7323E-09, 1.9671E-09, 2.2976E-09,      s2963290
     C 2.6679E-09, 3.0777E-09, 3.4321E-09, 3.8192E-09, 4.2711E-09,      s2963300
     C 4.4903E-09, 4.8931E-09, 5.2253E-09, 5.4040E-09, 5.6387E-09,      s2963310
     C 5.6704E-09, 6.0345E-09, 6.1079E-09, 6.2576E-09, 6.4039E-09,      s2963320
     C 6.3776E-09, 6.1878E-09, 5.8616E-09, 5.7036E-09, 5.5840E-09,      s2963330
     C 5.6905E-09, 5.8931E-09, 6.2478E-09, 6.8291E-09, 7.4528E-09,      s2963340
     C 7.6078E-09, 7.3898E-09, 6.7573E-09, 5.9827E-09, 5.0927E-09,      s2963350
     C 4.0099E-09, 3.1933E-09, 2.4296E-09, 1.8485E-09, 1.4595E-09/      s2963360
      DATA S1401/                                                       s2963370
     C 1.2017E-09, 1.0164E-09, 8.7433E-10, 7.7108E-10, 7.0049E-10,      s2963380
     C 6.5291E-10, 6.1477E-10, 5.9254E-10, 5.8150E-10, 5.7591E-10,      s2963390
     C 5.8490E-10, 5.8587E-10, 5.9636E-10, 6.2408E-10, 6.5479E-10,      s2963400
     C 7.0480E-10, 7.2313E-10, 7.5524E-10, 8.0863E-10, 8.3386E-10,      s2963410
     C 9.2342E-10, 9.6754E-10, 1.0293E-09, 1.0895E-09, 1.1330E-09,      s2963420
     C 1.2210E-09, 1.2413E-09, 1.2613E-09, 1.2671E-09, 1.2225E-09,      s2963430
     C 1.1609E-09, 1.0991E-09, 1.0600E-09, 1.0570E-09, 1.0818E-09,      s2963440
     C 1.1421E-09, 1.2270E-09, 1.3370E-09, 1.4742E-09, 1.4946E-09,      s2963450
     C 1.4322E-09, 1.3210E-09, 1.1749E-09, 1.0051E-09, 7.8387E-10,      s2963460
     C 6.1844E-10, 4.6288E-10, 3.4164E-10, 2.5412E-10, 1.9857E-10/      s2963470
      DATA S1451/                                                       s2963480
     C 1.5876E-10, 1.2966E-10, 1.0920E-10, 9.4811E-11, 8.3733E-11,      s2963490
     C 7.3906E-11, 6.7259E-11, 6.1146E-11, 5.7119E-11, 5.3546E-11,      s2963500
     C 4.8625E-11, 4.4749E-11, 4.1089E-11, 3.7825E-11, 3.4465E-11,      s2963510
     C 3.1018E-11, 2.8109E-11, 2.5610E-11, 2.2859E-11, 2.0490E-11,      s2963520
     C 1.8133E-11, 1.5835E-11, 1.3949E-11, 1.2295E-11, 1.0799E-11,      s2963530
     C 9.6544E-12, 8.7597E-12, 7.9990E-12, 7.3973E-12, 6.9035E-12,      s2963540
     C 6.4935E-12, 6.1195E-12, 5.8235E-12, 5.5928E-12, 5.4191E-12,      s2963550
     C 5.2993E-12, 5.2338E-12, 5.2272E-12, 5.2923E-12, 5.4252E-12,      s2963560
     C 5.6523E-12, 5.9433E-12, 6.3197E-12, 6.9016E-12, 7.5016E-12,      s2963570
     C 8.2885E-12, 9.4050E-12, 1.0605E-11, 1.2257E-11, 1.3622E-11/      s2963580
      DATA S1501/                                                       s2963590
     C 1.5353E-11, 1.7543E-11, 1.9809E-11, 2.2197E-11, 2.4065E-11,      s2963600
     C 2.6777E-11, 2.9751E-11, 3.2543E-11, 3.5536E-11, 3.9942E-11,      s2963610
     C 4.6283E-11, 5.4556E-11, 6.5490E-11, 7.6803E-11, 9.0053E-11,      s2963620
     C 1.0852E-10, 1.2946E-10, 1.4916E-10, 1.7748E-10, 2.0073E-10,      s2963630
     C 2.2485E-10, 2.5114E-10, 2.7715E-10, 3.1319E-10, 3.3305E-10,      s2963640
     C 3.5059E-10, 3.5746E-10, 3.6311E-10, 3.7344E-10, 3.6574E-10,      s2963650
     C 3.7539E-10, 3.9434E-10, 4.3510E-10, 4.3340E-10, 4.2588E-10,      s2963660
     C 4.3977E-10, 4.6062E-10, 4.7687E-10, 4.6457E-10, 4.8578E-10,      s2963670
     C 5.2344E-10, 5.6752E-10, 5.8702E-10, 5.6603E-10, 5.3784E-10,      s2963680
     C 4.9181E-10, 4.3272E-10, 3.5681E-10, 2.8814E-10, 2.3320E-10/      s2963690
      DATA S1551/                                                       s2963700
     C 1.8631E-10, 1.4587E-10, 1.1782E-10, 9.8132E-11, 8.2528E-11,      s2963710
     C 6.9174E-11, 6.1056E-11, 5.3459E-11, 4.7116E-11, 4.1878E-11,      s2963720
     C 3.8125E-11, 3.6347E-11, 3.5071E-11, 3.3897E-11, 3.3541E-11,      s2963730
     C 3.3563E-11, 3.5469E-11, 3.8111E-11, 3.8675E-11, 4.1333E-11,      s2963740
     C 4.3475E-11, 4.6476E-11, 4.9761E-11, 5.1380E-11, 5.4135E-11,      s2963750
     C 5.3802E-11, 5.5158E-11, 5.6864E-11, 5.9311E-11, 6.3827E-11,      s2963760
     C 6.7893E-11, 6.8230E-11, 6.6694E-11, 6.6018E-11, 6.4863E-11,      s2963770
     C 6.5893E-11, 6.3813E-11, 6.4741E-11, 6.8630E-11, 7.0255E-11,      s2963780
     C 7.0667E-11, 6.8810E-11, 6.4104E-11, 5.8136E-11, 4.7242E-11,      s2963790
     C 3.7625E-11, 3.1742E-11, 2.5581E-11, 1.8824E-11, 1.3303E-11/      s2963800
      DATA S1601/                                                       s2963810
     C 9.6919E-12, 7.5353E-12, 6.0986E-12, 5.0742E-12, 4.3094E-12,      s2963820
     C 3.7190E-12, 3.2520E-12, 2.8756E-12, 2.5680E-12, 2.3139E-12,      s2963830
     C 2.1025E-12, 1.9257E-12, 1.7777E-12, 1.6539E-12, 1.5508E-12,      s2963840
     C 1.4657E-12, 1.3966E-12, 1.3417E-12, 1.2998E-12, 1.2700E-12,      s2963850
     C 1.2514E-12, 1.2437E-12, 1.2463E-12, 1.2592E-12, 1.2823E-12,      s2963860
     C 1.3157E-12, 1.3596E-12, 1.4144E-12, 1.4806E-12, 1.5588E-12,      s2963870
     C 1.6497E-12, 1.7544E-12, 1.8738E-12, 2.0094E-12, 2.1626E-12,      s2963880
     C 2.3354E-12, 2.5297E-12, 2.7483E-12, 2.9941E-12, 3.2708E-12,      s2963890
     C 3.5833E-12, 3.9374E-12, 4.3415E-12, 4.8079E-12, 5.3602E-12,      s2963900
     C 5.9816E-12, 6.7436E-12, 7.6368E-12, 8.6812E-12, 9.8747E-12/      s2963910
      DATA S1651/                                                       s2963920
     C 1.1350E-11, 1.3181E-11, 1.5406E-11, 1.7868E-11, 2.0651E-11,      s2963930
     C 2.4504E-11, 2.9184E-11, 3.4159E-11, 3.9979E-11, 4.8704E-11,      s2963940
     C 5.7856E-11, 6.7576E-11, 7.9103E-11, 9.4370E-11, 1.1224E-10,      s2963950
     C 1.3112E-10, 1.5674E-10, 1.8206E-10, 2.0576E-10, 2.3187E-10,      s2963960
     C 2.7005E-10, 3.0055E-10, 3.3423E-10, 3.6956E-10, 3.8737E-10,      s2963970
     C 4.2630E-10, 4.5154E-10, 4.8383E-10, 5.3582E-10, 5.8109E-10,      s2963980
     C 6.3741E-10, 6.3874E-10, 6.3870E-10, 6.5818E-10, 6.5056E-10,      s2963990
     C 6.5291E-10, 6.3159E-10, 6.3984E-10, 6.4549E-10, 6.5444E-10,      s2964000
     C 6.7035E-10, 6.7665E-10, 6.9124E-10, 6.8451E-10, 6.9255E-10,      s2964010
     C 6.9923E-10, 7.0396E-10, 6.7715E-10, 6.0371E-10, 5.3774E-10/      s2964020
      DATA S1701/                                                       s2964030
     C 4.6043E-10, 3.7635E-10, 2.9484E-10, 2.2968E-10, 1.8185E-10,      s2964040
     C 1.4191E-10, 1.1471E-10, 9.4790E-11, 7.9613E-11, 6.7989E-11,      s2964050
     C 5.9391E-11, 5.2810E-11, 4.7136E-11, 4.2618E-11, 3.8313E-11,      s2964060
     C 3.4686E-11, 3.1669E-11, 2.9110E-11, 2.6871E-11, 2.5074E-11,      s2964070
     C 2.4368E-11, 2.3925E-11, 2.4067E-11, 2.4336E-11, 2.4704E-11,      s2964080
     C 2.5823E-11, 2.7177E-11, 2.9227E-11, 3.1593E-11, 3.5730E-11,      s2964090
     C 4.0221E-11, 4.3994E-11, 4.8448E-11, 5.3191E-11, 5.8552E-11,      s2964100
     C 6.3458E-11, 6.6335E-11, 7.2457E-11, 7.9091E-11, 8.2234E-11,      s2964110
     C 8.7668E-11, 8.7951E-11, 9.2952E-11, 9.6157E-11, 9.5926E-11,      s2964120
     C 1.0120E-10, 1.0115E-10, 9.9577E-11, 9.6633E-11, 9.2891E-11/      s2964130
      DATA S1751/                                                       s2964140
     C 9.3315E-11, 9.5584E-11, 1.0064E-10, 1.0509E-10, 1.1455E-10,      s2964150
     C 1.2443E-10, 1.2963E-10, 1.2632E-10, 1.1308E-10, 1.0186E-10,      s2964160
     C 8.5880E-11, 6.7863E-11, 5.1521E-11, 3.7780E-11, 2.8842E-11,      s2964170
     C 2.2052E-11, 1.7402E-11, 1.4406E-11, 1.1934E-11, 1.0223E-11,      s2964180
     C 8.9544E-12, 7.9088E-12, 7.0675E-12, 6.2222E-12, 5.6051E-12,      s2964190
     C 5.0502E-12, 4.5578E-12, 4.2636E-12, 3.9461E-12, 3.7599E-12,      s2964200
     C 3.5215E-12, 3.2467E-12, 3.0018E-12, 2.6558E-12, 2.3928E-12,      s2964210
     C 2.0707E-12, 1.7575E-12, 1.5114E-12, 1.2941E-12, 1.1004E-12,      s2964220
     C 9.5175E-13, 8.2894E-13, 7.3253E-13, 6.5551E-13, 5.9098E-13,      s2964230
     C 5.3548E-13, 4.8697E-13, 4.4413E-13, 4.0600E-13, 3.7188E-13/      s2964240
      DATA S1801/                                                       s2964250
     C 3.4121E-13, 3.1356E-13, 2.8856E-13, 2.6590E-13, 2.4533E-13,      s2964260
     C 2.2663E-13, 2.0960E-13, 1.9407E-13, 1.7990E-13, 1.6695E-13,      s2964270
     C 1.5512E-13, 1.4429E-13, 1.3437E-13, 1.2527E-13, 1.1693E-13,      s2964280
     C 1.0927E-13, 1.0224E-13, 9.5767E-14, 8.9816E-14, 8.4335E-14,      s2964290
     C 7.9285E-14, 7.4626E-14, 7.0325E-14, 6.6352E-14, 6.2676E-14,      s2964300
     C 5.9274E-14, 5.6121E-14, 5.3195E-14, 5.0479E-14, 4.7953E-14,      s2964310
     C 4.5602E-14, 4.3411E-14, 4.1367E-14, 3.9456E-14, 3.7670E-14,      s2964320
     C 3.5996E-14, 3.4427E-14, 3.2952E-14, 3.1566E-14, 3.0261E-14,      s2964330
     C 2.9030E-14, 2.7868E-14, 2.6770E-14, 2.5730E-14, 2.4745E-14,      s2964340
     C 2.3809E-14, 2.2921E-14, 2.2076E-14, 2.1271E-14, 2.0504E-14/      s2964350
      DATA S1851/                                                       s2964360
     C 1.9772E-14, 1.9073E-14, 1.8404E-14, 1.7764E-14, 1.7151E-14,      s2964370
     C 1.6564E-14, 1.6000E-14, 1.5459E-14, 1.4939E-14, 1.4439E-14,      s2964380
     C 1.3958E-14, 1.3495E-14, 1.3049E-14, 1.2620E-14, 1.2206E-14,      s2964390
     C 1.1807E-14, 1.1422E-14, 1.1050E-14, 1.0691E-14, 1.0345E-14,      s2964400
     C 1.0010E-14, 9.6870E-15, 9.3747E-15, 9.0727E-15, 8.7808E-15,      s2964410
     C 8.4986E-15, 8.2257E-15, 7.9617E-15, 7.7064E-15, 7.4594E-15,      s2964420
     C 7.2204E-15, 6.9891E-15, 6.7653E-15, 6.5488E-15, 6.3392E-15,      s2964430
     C 6.1363E-15, 5.9399E-15, 5.7499E-15, 5.5659E-15, 5.3878E-15,      s2964440
     C 5.2153E-15, 5.0484E-15, 4.8868E-15, 4.7303E-15, 4.5788E-15,      s2964450
     C 4.4322E-15, 4.2902E-15, 4.1527E-15, 4.0196E-15, 3.8907E-15/      s2964460
      DATA S1901/                                                       s2964470
     C 3.7659E-15, 3.6451E-15, 3.5281E-15, 3.4149E-15, 3.3052E-15,      s2964480
     C 3.1991E-15, 3.0963E-15, 2.9967E-15, 2.9004E-15, 2.8071E-15,      s2964490
     C 2.7167E-15, 2.6293E-15, 2.5446E-15, 2.4626E-15, 2.3833E-15,      s2964500
     C 2.3064E-15, 2.2320E-15, 2.1600E-15, 2.0903E-15, 2.0228E-15,      s2964510
     C 1.9574E-15, 1.8942E-15, 1.8329E-15, 1.7736E-15, 1.7163E-15,      s2964520
     C 1.6607E-15, 1.6069E-15, 1.5548E-15, 1.5044E-15, 1.4557E-15,      s2964530
     C 1.4084E-15, 1.3627E-15, 1.3185E-15, 1.2757E-15, 1.2342E-15,      s2964540
     C 1.1941E-15, 1.1552E-15, 1.1177E-15, 1.0813E-15, 1.0461E-15,      s2964550
     C 1.0120E-15, 9.7900E-16, 9.4707E-16, 9.1618E-16, 8.8628E-16,      s2964560
     C 8.5734E-16, 8.2933E-16, 8.0223E-16, 7.7600E-16, 7.5062E-16/      s2964570
      DATA S1951/                                                       s2964580
     C 7.2606E-16, 7.0229E-16, 6.7929E-16, 6.5703E-16, 6.3550E-16,      s2964590
     C 6.1466E-16, 5.9449E-16, 5.7498E-16, 5.5610E-16, 5.3783E-16,      s2964600
     C 5.2015E-16, 5.0305E-16, 4.8650E-16, 4.7049E-16, 4.5500E-16,      s2964610
     C 4.4002E-16, 4.2552E-16, 4.1149E-16, 3.9792E-16, 3.8479E-16,      s2964620
     C 3.7209E-16, 3.5981E-16, 3.4792E-16, 3.3642E-16, 3.2530E-16,      s2964630
     C 3.1454E-16, 3.0413E-16, 2.9406E-16, 2.8432E-16, 2.7490E-16,      s2964640
     C 2.6579E-16, 2.5697E-16, 2.4845E-16, 2.4020E-16, 2.3223E-16,      s2964650
     C 2.2451E-16, 2.1705E-16, 2.0984E-16, 2.0286E-16, 1.9611E-16,      s2964660
     C 1.8958E-16, 1.8327E-16, 1.7716E-16, 1.7126E-16, 1.6555E-16,      s2964670
     C 1.6003E-16, 1.5469E-16, 1.4952E-16, 1.4453E-16, 1.3970E-16/      s2964680
      DATA S2001/                                                       s2964690
     C 1.3503E-16/                                                      s2964700
C                                                                       s2964710
      END                                                               s2964720
      subroutine shade(ik,msoff,index,ml,iph,ipath,v,sumssr)            
c                                                                       
c     last modified on October 1993.
c     Routine SHADE sets the solar transmission terms to zero for       
c     shaded scattering points                                          
      INCLUDE 'parameter.list'
      COMMON RELHUM(LAYDIM),HSTOR(LAYDIM),ICH(4),VH(17),TX(65),W(65)  
      COMMON IMSMX,WPATH(LAYTHR,65),TBBY(LAYTHR),PATM(LAYTHR),NSPEC,   
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX2(47),AWCCON(5)  
      COMMON/MSRD/TLE(LAYDIM),COSBAR(LAYDIM),OMEGA0(LAYTWO),
     1  UPF(10,LAYDIM),DNF(10,LAYDIM),TAER(LAYDIM),ASYIK(LAYTWO),
     2  ASYDM(LAYTWO),STRN(0:LAYDIM),DMOLS(LAYTWO),DSTRN(0:LAYTWO),
     3  FDNSRT,FDNTRT,TAUT(LAYDIM),UMF(LAYDIM),DMF(LAYDIM),
     4  UMFS(LAYDIM),DMFS(LAYDIM) 
C                                                                       shad 200
      COMMON/CNSTNS/PI,CA,DEG,GCAIR,BIGNUM,BIGEXP
C                                                                       
C     SUN CAN NOT BE SEEN                                               
      TX(9)=0.                                                          
      TX(14)=bignum
      IF(MSOFF.GT.0)THEN                                                
          dstrn(index)=0.                                               
      else                                                              
          IF(IK.LT.ML)CALL SSRAD(IPH,IK,IPATH,V,SUMSSR)                 
      ENDIF                                                             
      return                                                            
      end                                                               
      BLOCK DATA SHUMG                                                  shad 330
C>    BLOCK DATA                                                        shad 340
C                                                                       shad 350
C     SCHUMANN-RUNGE O2 BAND MODEL - SAMPLE CODING                      shad 360
C                                                                       shad 370
       COMMON /SHUR/ SHN001(70),SHN076(75),SHN151(75),                  shad 380
     X  SHN226(75),SHN301(75),SHN376(54),SHDUM(6)                       shad 390
       DATA SHN001/                                                     shad 400
     X  -8.00000,  -8.00000,  -8.00000,  -6.30103,  -6.00000,           shad 410
     X  -5.94896,  -5.94896,  -5.55139,  -5.17613,  -4.90612,           shad 420
     X  -4.56059,  -4.30010,  -4.30739,  -4.34455,  -4.35231,           shad 430
     X  -4.50777,  -4.41705,  -3.93569,  -3.70298,  -3.84254,           shad 440
     X  -4.01007,  -4.13821,  -3.92122,  -3.55006,  -3.22681,           shad 450
     X  -3.17363,  -3.55224,  -3.66208,  -3.32331,  -3.00804,           shad 460
     X  -2.99732,  -3.30951,  -3.20867,  -2.69141,  -2.49670,           shad 470
     X  -2.74355,  -2.69834,  -2.67293,  -2.67371,  -2.89603,           shad 480
     X  -3.13808,  -3.45473,  -3.54812,  -3.00636,  -2.72446,           shad 490
     X  -2.90208,  -2.93847,  -3.03693,  -3.15944,  -3.43764,           shad 500
     X  -3.67262,  -3.53970,  -2.84182,  -2.51909,  -2.50557,           shad 510
     X  -2.44459,  -2.72040,  -2.95979,  -3.02842,  -2.92391,           shad 520
     X  -2.61329,  -2.24839,  -2.03988,  -1.98147,  -1.97078,           shad 530
     X  -2.14548,  -2.51734,  -2.47024,  -2.02579,  -1.70360/           shad 540
       DATA SHN076/                                                     shad 550
     X  -1.64178,  -2.05789,  -2.41111,  -2.30034,  -1.91818,           shad 560
     X  -1.50450,  -1.32084,  -1.80380,  -2.13878,  -1.94658,           shad 570
     X  -1.61627,  -1.55771,  -1.88813,  -1.64415,  -1.43970,           shad 580
     X  -1.72633,  -1.50064,  -1.29499,  -1.47224,  -1.42286,           shad 590
     X  -1.84903,  -2.42249,  -2.95877,  -3.43342,  -3.85023,           shad 600
     X  -4.92183,  -4.92959,  -4.80852,  -4.67030,  -4.72573,           shad 610
     X  -4.84445,  -4.86951,  -4.90354,  -4.80891,  -4.61211,           shad 620
     X  -4.48205,  -4.51391,  -4.66502,  -4.84670,  -4.88606,           shad 630
     X  -4.82391,  -4.69897,  -4.51203,  -4.13960,  -3.87805,           shad 640
     X  -3.80311,  -3.77114,  -3.88260,  -4.14615,  -4.39649,           shad 650
     X  -4.62899,  -4.78494,  -4.69514,  -4.27200,  -3.92731,           shad 660
     X  -3.72681,  -3.60335,  -3.49142,  -3.38223,  -3.52349,           shad 670
     X  -3.64037,  -3.58526,  -3.48978,  -3.36320,  -3.37270,           shad 680
     X  -3.58359,  -3.83908,  -4.06157,  -3.96920,  -3.24875,           shad 690
     X  -2.78627,  -2.54861,  -2.56192,  -2.79838,  -2.89008/           shad 700
       DATA SHN151/                                                     shad 710
     X  -2.97200,  -2.91496,  -2.85783,  -3.00554,  -3.22285,           shad 720
     X  -3.17575,  -2.82405,  -2.44375,  -2.24512,  -2.13519,           shad 730
     X  -2.17638,  -2.12548,  -2.24833,  -2.42286,  -2.48889,           shad 740
     X  -2.57284,  -2.67481,  -2.84576,  -2.57849,  -2.23621,           shad 750
     X  -1.97914,  -2.01655,  -2.08918,  -2.25852,  -2.60669,           shad 760
     X  -2.91101,  -3.24343,  -3.54870,  -3.05507,  -2.41260,           shad 770
     X  -1.97192,  -1.74591,  -1.70757,  -1.86170,  -2.21955,           shad 780
     X  -2.52520,  -2.86220,  -2.96082,  -2.42138,  -1.96791,           shad 790
     X  -1.71099,  -1.68871,  -1.86617,  -2.21148,  -2.51694,           shad 800
     X  -2.77760,  -2.37949,  -1.89083,  -1.58900,  -1.52710,           shad 810
     X  -1.68850,  -2.03635,  -2.31319,  -2.17366,  -1.60655,           shad 820
     X  -1.27097,  -1.14262,  -1.34089,  -1.68119,  -1.78236,           shad 830
     X  -1.45853,  -1.19063,  -1.11210,  -1.38628,  -1.48342,           shad 840
     X  -1.12039,   -.85543,   -.77060,  -1.05684,  -1.05423,           shad 850
     X   -.93689,   -.86922,   -.94306,   -.76850,   -.59062/           shad 860
       DATA SHN226/                                                     shad 870
     X   -.50208,   -.53499,   -.88884,  -1.18360,  -1.52243,           shad 880
     X  -1.84564,  -2.17740,  -2.50559,  -2.83351,  -3.15308,           shad 890
     X  -3.41587,  -3.41025,  -3.23752,  -3.13656,  -3.30149,           shad 900
     X  -3.55280,  -3.77885,  -3.71929,  -3.36467,  -3.06275,           shad 910
     X  -2.83782,  -2.68294,  -2.55793,  -2.63001,  -2.90714,           shad 920
     X  -3.18561,  -3.46714,  -3.70067,  -3.62895,  -3.02605,           shad 930
     X  -2.65584,  -2.46195,  -2.48991,  -2.44044,  -2.29494,           shad 940
     X  -2.28839,  -2.29827,  -2.22063,  -2.12801,  -2.18940,           shad 950
     X  -2.48029,  -2.74669,  -2.83833,  -2.45937,  -2.16507,           shad 960
     X  -2.02067,  -2.03314,  -1.80888,  -1.51479,  -1.38580,           shad 970
     X  -1.37993,  -1.63534,  -1.83905,  -1.87999,  -1.82492,           shad 980
     X  -1.89398,  -1.90149,  -1.78545,  -1.65285,  -1.40144,           shad 990
     X  -1.17488,  -1.07228,  -1.15343,  -1.37759,  -1.70025,           shad1000
     X  -2.01075,  -2.33004,  -2.62771,  -2.87105,  -2.84082,           shad1010
     X  -2.00293,  -1.31932,   -.92860,   -.76253,   -.84790/           shad1020
       DATA SHN301/                                                     shad1030
     X  -1.16306,  -1.46677,  -1.79051,  -2.09491,  -2.34556,           shad1040
     X  -2.13867,  -1.37321,   -.82048,   -.53990,   -.47636,           shad1050
     X   -.72816,  -1.03484,  -1.33688,  -1.61955,  -1.78843,           shad1060
     X  -1.43388,   -.81369,   -.44878,   -.28512,   -.40431,           shad1070
     X   -.72200,  -1.00945,  -1.28895,  -1.31856,   -.85686,           shad1080
     X   -.42072,   -.19421,   -.18317,   -.46858,   -.73309,           shad1090
     X   -.93390,   -.77552,   -.37922,   -.12965,   -.05480,           shad1100
     X   -.26659,   -.48423,   -.50987,   -.24666,   -.01742,           shad1110
     X    .07660,   -.06367,   -.20185,   -.11253,    .06726,           shad1120
     X    .17955,    .14879,    .15975,    .28769,    .41632,           shad1130
     X    .49995,    .61664,    .76706,    .82624,    .76615,           shad1140
     X    .43165,    .13821,   -.18926,   -.48784,   -.77913,           shad1150
     X  -1.08972,  -1.39948,  -1.70006,  -1.94700,  -1.96249,           shad1160
     X  -1.67500,  -1.41241,  -1.29981,  -1.40100,  -1.69529,           shad1170
     X  -1.96904,  -2.25253,  -2.44942,  -2.13985,  -1.80460/           shad1180
       DATA SHN376/                                                     shad1190
     X  -1.60216,  -1.72517,  -1.98472,  -2.08115,  -1.62632,           shad1200
     X  -1.12971,   -.86160,   -.81141,  -1.07504,  -1.34407,           shad1210
     X  -1.50074,  -1.47345,  -1.41077,  -1.59810,  -1.67103,           shad1220
     X  -1.53208,  -1.36215,  -1.26724,   -.91307,   -.50826,           shad1230
     X   -.27840,   -.24468,   -.46373,   -.76619,  -1.07304,           shad1240
     X  -1.37968,  -1.66148,  -1.89046,  -2.02811,  -1.97679,           shad1250
     X  -1.55840,   -.94089,   -.46463,   -.21757,   -.16985,           shad1260
     X   -.41642,   -.69469,   -.98624,  -1.26028,  -1.48661,           shad1270
     X  -1.58100,  -1.42675,  -1.01563,   -.52312,   -.13686,           shad1280
     X    .06300,    .07682,   -.16825,   -.42809,   -.69506,           shad1290
     X   -.91898,  -1.03253,   -.90609,   -.42809           /           shad1300
      END                                                               shad1310
      SUBROUTINE SINT(V1,V1C,DV,NPT,CONTI,CONTO)                        sint 100
C                                                                       sint 110
C     INTERPOLATION  FOR CONTINUUM WITH LOWTRAN                         sint 120
C                                                                       sint 130
      DIMENSION CONTI(2003)                                             sint 140
      CONTO=0.                                                          sint 150
      I=(V1C-V1)/DV+1.00001                                             sint 160
      IF(I.GE.NPT)GO TO 10                                              sint 170
      CONTO=CONTI(I)                                                    sint 180
      IMOD=AMOD(V1C,10.)                                                sint 190
      IF(IMOD.GT.0) CONTO=(CONTI(I)+CONTI(I+1))/2.                      sint 200
10    CONTINUE                                                          sint 210
      RETURN                                                            sint 220
      END                                                               sint 230
      SUBROUTINE SLF260(V1C,SH2OT1)                                     l260 100
C     LOADS SELF CONTINUUM  260K                                        l260 110
      COMMON /S260/ V1,V2,DV,NPT,S260(2003)                             l260 120
      CALL SINT(V1,V1C,DV,NPT,S260,SH2OT1)                              l260 130
      RETURN                                                            l260 140
      END                                                               l260 150
      SUBROUTINE SLF296(V1C,SH2OT0)                                     l296 100
C     LOADS SELF CONTINUUM  296K                                        l296 110
      COMMON /SH2O/ V1,V2,DV,NPT,S296(2003)                             l296 120
      CALL SINT(V1,V1C,DV,NPT,S296,SH2OT0)                              l296 130
      RETURN                                                            l296 140
      END                                                               l296 150
      SUBROUTINE SMGEO(SPH1,SPH2,SPANGL,SPRANG,SPBETA,SPPHI,DHALFR,     smgo 100
     $     DPRNG2,BENDNG,LEN,SMMIN)                                     smgo 110
      REAL SPH1, SPH2, SPANGL, SPRANG, SPPHI, BENDNG, SPBETA,           smgo 120
     $     RE, DELTAS, ZMAX, AORIG                                      smgo 130
      DOUBLE PRECISION DHALFR, DPRNG2,X,Y,BETA,RANGE,H1, H2,            smgo 140
     $     DEG,A, B, PHI, ANGLE,SMMIN                                   smgo 150
      INTEGER IMAX, IMOD, IBMAX, IPATH, LEN                             smgo 160
      DATA DEG /57.2957795131/                                          smgo 170
      COMMON /PARMTR/RE, DELTAS, ZMAX, IMAX, IMOD, IBMAX, IPATH         smgo 180
      COMMON /SMALL4/H1, H2, RANGE                                      smgo 190
      COMMON /SMALL5/AORIG                                              smgo 200
C                                                                       smgo 210
C     H1, H2, AND RANGE ARE ALSO IN COMMON (BLOCK NAME IS SMALL4) IN SMGsmgo 220
C     THEY ARE CALLED SMH1, SMH2 AND TRANGE IN DPRFPA (FORMERLY RFPATH).smgo 230
C     THEY DO NOT OVERLAP WITH H1 AND H2 OF OTHER ROUTINES WHERE THEY ARsmgo 240
C     THEY ARE THE MOST IMPORTANT INPUTS TO THIS ROUTINE.               smgo 250
C     THEY ARE IN DOUBLE PRECISION AND ARE "PREPARED" IN SMPREP.        smgo 260
C     THE  CORRESPONDING ARGUMENTS OF THIS SUBROUTINE ARE ALSO PREPARED smgo 270
C     BUT THEY ARE IN SINGLE PRECISION.                                 smgo 280
C                                                                       smgo 290
C     AORIG IS THE ORIGINAL INPUT ANGLE IN SINGLE PRECISION             smgo 300
C                                                                       smgo 310
      X = ABS(H1-H2)                                                    smgo 320
      Y = SQRT((RANGE+X)*(RANGE-X))                                     smgo 330
      IF (H1 .EQ. H2) THEN                                              smgo 340
         BETA = 2*ASIN(RANGE/(2*(H2+RE)))*DEG                           smgo 350
         A = (180-BETA)*0.5                                             smgo 360
         PHI = A+BETA                                                   smgo 370
         ANGLE = PHI                                                    smgo 380
      ELSEIF (H1 .GT. H2) THEN                                          smgo 390
         BETA = (Y/(H2+RE))*DEG                                         smgo 400
         A = (180-BETA)*0.5                                             smgo 410
         B = ASIN(X/RANGE)*DEG                                          smgo 420
         PHI = 180- A -B                                                smgo 430
         ANGLE = 90 + B                                                 smgo 440
      ELSE                                                              smgo 450
         BETA = (Y/(H1+RE))*DEG                                         smgo 460
         A = (180-BETA)*0.5                                             smgo 470
         B = ASIN(X/RANGE)*DEG                                          smgo 480
         PHI = 90 + B                                                   smgo 490
         ANGLE = 180 - A -B                                             smgo 500
      ENDIF                                                             smgo 510
      BENDNG = 0.0                                                      smgo 520
      IF ((ANGLE .GT. 90.D00 .AND. PHI .GT. 90.D00) .AND.               smgo 530
     $     AORIG .NE. 90.0000000) THEN                                  smgo 540
C        THE CONDITION ON AORIG IS TO MAKE SURE THAT PATHS WHOSE        smgo 550
C        ORIGINAL INPUT ZENITH IS 90 DEGREES ARE TREATED AS             smgo 560
C        LEN = 0 PATHS.  CALCULATED ANGLE WILL LIKELY                   smgo 570
C        DIFFER FROM AORIG.                                             smgo 580
         LEN = 1                                                        smgo 590
         DHALFR = (MIN(H1,H2) + RE) *                                   smgo 600
     $        COS(DBLE(180-ANGLE)/DEG)                                  smgo 610
         DPRNG2 = RANGE-2*DHALFR                                        smgo 620
         IF (DPRNG2 .LE. 1.0D-06) THEN                                  smgo 630
            DPRNG2 = 0.0                                                smgo 640
            DHALFR = RANGE/2                                            smgo 650
         ENDIF                                                          smgo 660
         SMMIN = (RE+H1)*SIN(DBLE(180-ANGLE)/DEG)-RE                    smgo 670
      ELSE                                                              smgo 680
         LEN = 0                                                        smgo 690
         DHALFR = 0.0                                                   smgo 700
         DPRNG2 = RANGE                                                 smgo 710
         SMMIN = MIN(H1,H2)                                             smgo 720
      ENDIF                                                             smgo 730
      SPANGL = ANGLE                                                    smgo 740
      SPBETA = BETA                                                     smgo 750
      SPPHI = PHI                                                       smgo 760
      return                                                            smgo 770
      END                                                               smgo 780
      SUBROUTINE SMPREP(SPH1,SPH2,SPANGL,SPRANG,SPBETA,LEN,ISLCT)       smep 100
C                                                                       smep 110
CSSI  THIS ROUTINE IS COMPLETELY NEW.                                   smep 120
C                                                                       smep 130
C     THIS SUBROUTINE PREPS OR PREPROCESSES PATH GEOMETRY PARAMETERS.   smep 140
C     THE FIRST PURPOSE IS TO SEE IS IF THE RANGE IS SMALL.             smep 150
C     IF IT IS, WE HAVE A SMALL PATH CASE.                              smep 160
C     THIS CASE IS HANDLED IN A SPECIAL WAY.                            smep 170
C                                                                       smep 180
C     IF THE RANGE IS NOT SMALL THIS ROUTINE DOES NOT DO MUCH.          smep 190
C                                                                       smep 200
C     ALL SMALL PATH CASES ARE CAST INTO THE EQUIVALENT CASE 2C.        smep 210
C     THE CURRENT SUBROUTINE DOES THAT BY COMPUTING H1, H2, AND RANGE.  smep 220
C     ALL OTHER VARIABLES ARE SET TO ZERO.                              smep 230
C                                                                       smep 240
C     CASE 2A:  H1, H2, ANGLE                                           smep 250
C     CASE 2B:  H1, ANGLE (ZENITH), RANGE                               smep 260
C     CASE 2C:  H1, H2, RANGE                                           smep 270
C     CASE 2D:  H1, H2, BETA                                            smep 280
C                                                                       smep 290
      REAL SPH1, SPH2, SPANGL, SPRANG, SPBETA,RE,DELTAS,ZMAX,           smep 300
     $     SAVER,SAVEA,SAVEB,SAVEH1,SAVEH2,SMALL,AORIG                  smep 310
      DOUBLE PRECISION BETA,RANGE,H1, H2,DEG,A,B,C,ANGLE,               smep 320
     $     RMIN,RMAX,R1,R2,ARG                                          smep 330
      INTEGER IMAX,IMOD,IBMAX,IPATH,LEN,ISLCT,IRD,IPR,IPU,NPR,IPR1      smep 340
      DATA DEG /57.2957795131/                                          smep 350
      COMMON /PARMTR/RE, DELTAS, ZMAX, IMAX, IMOD, IBMAX, IPATH         smep 360
      COMMON /SMALL3/SMALL                                              smep 370
      COMMON /SMALL4/H1, H2, RANGE                                      smep 380
      COMMON /SMALL5/AORIG                                              smep 390
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
C                                                                       smep 410
C     H1, H2, AND RANGE ARE ALSO IN COMMON (BLOCK NAME IS SMALL4) IN SMGsmep 420
c     AND DPRFPA (FORMERLY RFPATH).                                     smep 430
C     THEY ARE CALLED SMH1, SMH2 AND TRANGE AND DPRFPA (FORMERLY RFPATH)smep 440
C     THEY DO NOT OVERLAP WITH H1 AND H2 OF OTHER ROUTINES WHERE THEY ARsmep 450
C     THEY ARE THE MOST IMPORTANT INPUTS TO THIS ROUTINE.               smep 460
C     THEY ARE IN DOUBLE PRECISION AND ARE "PREPARED" IN SMPREP.        smep 470
C     THE  CORRESPONDING ARGUMENTS OF THIS SUBROUTINE ARE ALSO PREPARED smep 480
C     BUT THEY ARE IN SINGLE PRECISION.                                 smep 490
C                                                                       smep 500
C     AORIG IS THE ORIGINAL INPUT ANGLE IN SINGLE PRECISION             smep 510
C                                                                       smep 520
      SAVER = SPRANG                                                    smep 530
      SAVEB = SPBETA                                                    smep 540
      SAVEA = SPANGL                                                    smep 550
      SAVEH1 = SPH1                                                     smep 560
      SAVEH2 = SPH2                                                     smep 570
      AORIG = SPANGL                                                    smep 580
C                                                                       smep 590
      H1 = DBLE(SPH1)                                                   smep 600
      H2 = DBLE(SPH2)                                                   smep 610
      RANGE = DBLE(SPRANG)                                              smep 620
      ANGLE = DBLE(SPANGL)                                              smep 630
      BETA = DBLE(SPBETA)/DEG                                           smep 640
C                                                                       smep 650
      R1 = H1+RE                                                        smep 660
      R2 = H2+RE                                                        smep 670
      RMIN = MIN(R1,R2)                                                 smep 680
      RMAX = MAX(R1,R2)                                                 smep 690
C                                                                       smep 700
C     IF (ISLCT .EQ. 23) ALREADY IN CASE 2C (H1, H2, RANGE).            smep 710
      IF (ISLCT .EQ. 23) RETURN                                         smep 720
C                                                                       smep 730
      IF (ISLCT .EQ. 24) THEN                                           smep 740
C        CASE 2D (H1, H2, BETA)                                         smep 750
         RANGE = SQRT(R1**2+R2**2-2.0*R1*R2*COS(BETA))                  smep 760
         IF (BETA .LT. 0.01)                                            smep 770
     $        RANGE = SQRT(DBLE(H1-H2)**2-2.*R1*R2*(-BETA**6/720.       smep 780
     $        +BETA**4/24.-BETA**2/2.0))                                smep 790
      ENDIF                                                             smep 800
C                                                                       smep 810
      IF (ISLCT .EQ. 22) THEN                                           smep 820
C        CASE 2B (H1, ANGLE, RANGE)                                     smep 830
         R2 = SQRT(-2*R1*RANGE*COS((180-ANGLE)/DEG)+RANGE**2+R1**2)     smep 840
         H2 = R2-RE                                                     smep 850
      ENDIF                                                             smep 860
C                                                                       smep 870
      IF (ISLCT .EQ. 21) THEN                                           smep 880
C        CASE 2A (H1, H2, ANGLE)                                        smep 890
C        USE LAW OF COSINES TO OBTAIN A QUADRATIC EQUATION FOR RANGE.   smep 900
C        A = COEFF. OF THE QUADRATIC TERM                               smep 910
C        B = COEFF. OF THE LINEAR TERM                                  smep 920
C        C = CONSTANT TERM                                              smep 930
         C = (H1-H2)*(RMAX+RMIN)                                        smep 940
         A = 1                                                          smep 950
         B = -2*R1*COS((180-ANGLE)/DEG)                                 smep 960
         ARG = B*B - 4*A*C                                              smep 970
         IF (ARG .LT. 0.0) THEN                                         smep 980
            WRITE(IPR,*)                                                smep 990
            WRITE(IPR,*)'FROM SMPREP:  FOR THESE PATH PARAMETERS,'      smep1000
            WRITE(IPR,*)'RANGE COULD NOT BE FOUND BY STRAIGHT'          smep1010
            WRITE(IPR,*)'LINE TRIANGULATION - IMPOSSIBLE TRIANGLE.'     smep1020
            WRITE(IPR,*)'PROCEED AT YOUR OWN RISK.'                     smep1030
            RETURN                                                      smep1040
         ENDIF                                                          smep1050
         IF (H1 .GT. H2) THEN                                           smep1060
            IF (ANGLE .LE. 90.0) THEN                                   smep1070
C              THE PRECEEDING CHECK WOULD ACTUALLY HAVE CAUGHT IT.      smep1080
               WRITE(IPR,*)                                             smep1090
               WRITE(IPR,*)'FROM SMPREP:'                               smep1100
               WRITE(IPR,*)'H1 > H2, & ANGLE <= 90:  IMPOSSIBLE.'       smep1110
               RANGE = 0                                                smep1120
               RETURN                                                   smep1130
            ELSEIF (LEN .EQ. 1) THEN                                    smep1140
C              LEN SHOULD BE INPUT FOR H1 .GT. H2 AND ANGLE .GT. 90 DEG.smep1150
               RANGE = -B/(2*A) + SQRT(ARG)/(2*A)                       smep1160
            ELSEIF (LEN .EQ. 0) THEN                                    smep1170
C              LEN SHOULD BE INPUT FOR H1 .GT. H2 AND ANGLE .GT. 90 DEG.smep1180
               RANGE = -B/(2*A) - SQRT(ARG)/(2*A)                       smep1190
            ENDIF                                                       smep1200
         ELSE                                                           smep1210
            RANGE = -B/(2*A) + SQRT(ARG)/(2*A)                          smep1220
            LEN = 0                                                     smep1230
         ENDIF                                                          smep1240
      ENDIF                                                             smep1250
C                                                                       smep1260
      IF (RANGE .LE. SMALL .AND. RANGE .GT. 0.0) THEN                   smep1270
         SPANGL = 0.0                                                   smep1280
         SPBETA = 0.0                                                   smep1290
         SPRANG = RANGE                                                 smep1300
         SPH1 = H1                                                      smep1310
         SPH2 = H2                                                      smep1320
         WRITE(IPR,*)                                                   smep1330
         WRITE(IPR,*)'FROM SMPREP:  RANGE .LE. THE PARAMETER SMALL = ', smep1340
     $        SMALL                                                     smep1350
         WRITE(IPR,*)'SO THE PRESENT CASE''LL BE TREATED AS CASE 2C W/' smep1360
         WRITE(IPR,*)'H1 = ', SPH1, 'H2 = ',SPH2,' & RANGE = ', RANGE   smep1370
      ELSEIF (RANGE .LE.0) THEN                                         smep1380
         WRITE(IPR,*)                                                   smep1390
         WRITE(IPR,*)'FROM SMPREP: RANGE .LE. 0'                        smep1400
         STOP                                                           smep1410
      ELSE                                                              smep1420
         SPRANG = SAVER                                                 smep1430
         SPH1 = SAVEH1                                                  smep1440
         SPH2 = SAVEH2                                                  smep1450
         SPBETA = SAVEB                                                 smep1460
         SPANGL = SAVEA                                                 smep1470
      ENDIF                                                             smep1480
      RETURN                                                            smep1490
      END                                                               smep1500
      SUBROUTINE SO2XS(V,CROSS)
      INTEGER NMAX
      PARAMETER (NMAX=5562)
      dimension  SO2CRS(5562)
C     BEGINNING AND ENDING FREQUENCIES (CM-1):    24820  52625
C     BIN WIDTH AND TOTAL ENTRIES:        5   5562
C     CROSS-SECTIONS IN CM^2 TIMES 2.6868E19
      DATA VBEG, VEND, VINCR /24820.0, 52625.0, 5.0/
      DATA (SO2CRS(I),I=    1,  100)/
     1     8.124E-05, 8.447E-05, 8.780E-05, 9.070E-05, 9.346E-05,
     2     9.665E-05, 9.921E-05, 1.017E-04, 1.047E-04, 1.077E-04,
     3     1.102E-04, 1.127E-04, 1.152E-04, 1.177E-04, 1.205E-04,
     4     1.236E-04, 1.262E-04, 1.287E-04, 1.311E-04, 1.336E-04,
     5     1.363E-04, 1.395E-04, 1.421E-04, 1.446E-04, 1.474E-04,
     6     1.505E-04, 1.530E-04, 1.555E-04, 1.580E-04, 1.605E-04,
     7     1.629E-04, 1.654E-04, 1.679E-04, 1.703E-04, 1.728E-04,
     8     1.753E-04, 1.778E-04, 1.802E-04, 1.827E-04, 1.847E-04,
     9     1.866E-04, 1.889E-04, 1.907E-04, 1.923E-04, 1.944E-04,
     $     1.968E-04, 1.993E-04, 2.017E-04, 2.036E-04, 2.052E-04,
     1     2.071E-04, 2.094E-04, 2.112E-04, 2.134E-04, 2.162E-04,
     2     2.192E-04, 2.218E-04, 2.249E-04, 2.282E-04, 2.311E-04,
     3     2.335E-04, 2.361E-04, 2.391E-04, 2.424E-04, 2.457E-04,
     4     2.488E-04, 2.513E-04, 2.537E-04, 2.575E-04, 2.639E-04,
     5     2.739E-04, 2.867E-04, 3.043E-04, 3.293E-04, 3.602E-04,
     6     3.916E-04, 4.202E-04, 4.464E-04, 4.705E-04, 4.931E-04,
     7     5.149E-04, 5.272E-04, 5.326E-04, 5.287E-04, 5.100E-04,
     8     4.754E-04, 4.308E-04, 3.908E-04, 3.557E-04, 3.139E-04,
     9     2.802E-04, 2.585E-04, 2.427E-04, 2.294E-04, 2.237E-04,
     $     2.183E-04, 2.131E-04, 2.081E-04, 2.026E-04, 1.976E-04/
      DATA (SO2CRS(I),I=  101,  200)/
     1     1.928E-04, 1.880E-04, 1.832E-04, 1.784E-04, 1.729E-04,
     2     1.678E-04, 1.630E-04, 1.581E-04, 1.526E-04, 1.476E-04,
     3     1.428E-04, 1.380E-04, 1.332E-04, 1.284E-04, 1.237E-04,
     4     1.196E-04, 1.173E-04, 1.163E-04, 1.155E-04, 1.151E-04,
     5     1.151E-04, 1.151E-04, 1.151E-04, 1.155E-04, 1.165E-04,
     6     1.182E-04, 1.212E-04, 1.243E-04, 1.275E-04, 1.306E-04,
     7     1.338E-04, 1.370E-04, 1.394E-04, 1.410E-04, 1.425E-04,
     8     1.435E-04, 1.447E-04, 1.465E-04, 1.489E-04, 1.514E-04,
     9     1.598E-04, 1.760E-04, 2.001E-04, 2.269E-04, 2.460E-04,
     $     2.597E-04, 2.707E-04, 2.816E-04, 2.920E-04, 3.030E-04,
     1     3.150E-04, 3.277E-04, 3.403E-04, 3.543E-04, 3.704E-04,
     2     3.896E-04, 4.126E-04, 4.398E-04, 4.705E-04, 5.053E-04,
     3     5.468E-04, 6.070E-04, 6.867E-04, 7.834E-04, 8.873E-04,
     4     1.025E-03, 1.173E-03, 1.316E-03, 1.455E-03, 1.591E-03,
     5     1.727E-03, 1.882E-03, 2.098E-03, 2.343E-03, 2.631E-03,
     6     2.973E-03, 3.390E-03, 3.893E-03, 4.239E-03, 4.424E-03,
     7     4.500E-03, 4.510E-03, 4.439E-03, 4.247E-03, 3.916E-03,
     8     3.690E-03, 3.485E-03, 3.280E-03, 3.075E-03, 2.867E-03,
     9     2.656E-03, 2.448E-03, 2.251E-03, 2.062E-03, 1.893E-03,
     $     1.739E-03, 1.596E-03, 1.446E-03, 1.324E-03, 1.217E-03/
      DATA (SO2CRS(I),I=  201,  300)/
     1     1.111E-03, 1.006E-03, 9.066E-04, 8.150E-04, 7.364E-04,
     2     6.737E-04, 6.193E-04, 5.724E-04, 5.342E-04, 5.033E-04,
     3     4.756E-04, 4.530E-04, 4.390E-04, 4.340E-04, 4.366E-04,
     4     4.454E-04, 4.593E-04, 4.745E-04, 4.914E-04, 5.072E-04,
     5     5.193E-04, 5.301E-04, 5.414E-04, 5.534E-04, 5.664E-04,
     6     5.800E-04, 5.946E-04, 6.100E-04, 6.261E-04, 6.440E-04,
     7     6.660E-04, 6.943E-04, 7.323E-04, 7.756E-04, 8.314E-04,
     8     9.039E-04, 9.966E-04, 1.106E-03, 1.233E-03, 1.373E-03,
     9     1.519E-03, 1.665E-03, 1.809E-03, 1.951E-03, 2.092E-03,
     $     2.268E-03, 2.452E-03, 2.619E-03, 2.822E-03, 3.083E-03,
     1     3.401E-03, 3.784E-03, 4.302E-03, 4.856E-03, 5.389E-03,
     2     5.834E-03, 6.097E-03, 6.114E-03, 5.876E-03, 5.573E-03,
     3     5.305E-03, 5.077E-03, 4.882E-03, 4.692E-03, 4.455E-03,
     4     4.202E-03, 3.983E-03, 3.667E-03, 3.317E-03, 2.965E-03,
     5     2.632E-03, 2.293E-03, 2.005E-03, 1.800E-03, 1.612E-03,
     6     1.460E-03, 1.339E-03, 1.246E-03, 1.146E-03, 1.069E-03,
     7     1.012E-03, 9.568E-04, 9.125E-04, 8.800E-04, 8.586E-04,
     8     8.389E-04, 8.234E-04, 8.105E-04, 7.986E-04, 7.886E-04,
     9     7.805E-04, 7.737E-04, 7.672E-04, 7.633E-04, 7.612E-04,
     $     7.605E-04, 7.604E-04, 7.612E-04, 7.638E-04, 7.699E-04/
      DATA (SO2CRS(I),I=  301,  400)/
     1     7.794E-04, 7.915E-04, 8.056E-04, 8.204E-04, 8.352E-04,
     2     8.497E-04, 8.643E-04, 8.832E-04, 9.071E-04, 9.358E-04,
     3     9.694E-04, 1.010E-03, 1.058E-03, 1.120E-03, 1.201E-03,
     4     1.288E-03, 1.378E-03, 1.467E-03, 1.560E-03, 1.664E-03,
     5     1.780E-03, 1.894E-03, 2.004E-03, 2.121E-03, 2.269E-03,
     6     2.478E-03, 2.755E-03, 3.063E-03, 3.119E-03, 3.134E-03,
     7     3.147E-03, 3.154E-03, 3.128E-03, 3.049E-03, 2.909E-03,
     8     2.802E-03, 2.687E-03, 2.566E-03, 2.450E-03, 2.340E-03,
     9     2.223E-03, 2.094E-03, 1.976E-03, 1.884E-03, 1.818E-03,
     $     1.773E-03, 1.743E-03, 1.721E-03, 1.710E-03, 1.722E-03,
     1     1.746E-03, 1.779E-03, 1.827E-03, 1.891E-03, 1.971E-03,
     2     2.075E-03, 2.233E-03, 2.416E-03, 2.612E-03, 2.819E-03,
     3     3.066E-03, 3.380E-03, 3.818E-03, 4.284E-03, 4.722E-03,
     4     5.106E-03, 5.533E-03, 6.001E-03, 6.483E-03, 6.889E-03,
     5     7.141E-03, 7.303E-03, 7.430E-03, 7.362E-03, 7.141E-03,
     6     6.796E-03, 6.513E-03, 6.287E-03, 6.083E-03, 5.808E-03,
     7     5.470E-03, 5.091E-03, 4.681E-03, 4.268E-03, 3.849E-03,
     8     3.436E-03, 3.110E-03, 2.853E-03, 2.650E-03, 2.488E-03,
     9     2.332E-03, 2.184E-03, 2.058E-03, 1.971E-03, 1.911E-03,
     $     1.878E-03, 1.864E-03, 1.859E-03, 1.859E-03, 1.859E-03/
      DATA (SO2CRS(I),I=  401,  500)/
     1     1.859E-03, 1.857E-03, 1.850E-03, 1.835E-03, 1.816E-03,
     2     1.788E-03, 1.741E-03, 1.694E-03, 1.656E-03, 1.624E-03,
     3     1.595E-03, 1.567E-03, 1.540E-03, 1.521E-03, 1.505E-03,
     4     1.491E-03, 1.482E-03, 1.479E-03, 1.483E-03, 1.495E-03,
     5     1.512E-03, 1.535E-03, 1.570E-03, 1.618E-03, 1.682E-03,
     6     1.765E-03, 1.864E-03, 1.988E-03, 2.161E-03, 2.324E-03,
     7     2.477E-03, 2.622E-03, 2.781E-03, 2.972E-03, 3.218E-03,
     8     3.536E-03, 3.935E-03, 4.433E-03, 5.054E-03, 5.754E-03,
     9     6.509E-03, 7.261E-03, 7.800E-03, 8.182E-03, 8.535E-03,
     $     8.938E-03, 9.326E-03, 9.564E-03, 9.228E-03, 8.783E-03,
     1     8.601E-03, 8.153E-03, 7.604E-03, 7.043E-03, 6.575E-03,
     2     6.165E-03, 5.749E-03, 4.918E-03, 4.260E-03, 3.903E-03,
     3     3.645E-03, 3.391E-03, 3.140E-03, 2.951E-03, 2.755E-03,
     4     2.562E-03, 2.422E-03, 2.290E-03, 2.158E-03, 2.024E-03,
     5     1.924E-03, 1.845E-03, 1.776E-03, 1.707E-03, 1.640E-03,
     6     1.578E-03, 1.522E-03, 1.472E-03, 1.424E-03, 1.380E-03,
     7     1.340E-03, 1.305E-03, 1.279E-03, 1.262E-03, 1.257E-03,
     8     1.267E-03, 1.281E-03, 1.295E-03, 1.318E-03, 1.347E-03,
     9     1.382E-03, 1.427E-03, 1.481E-03, 1.549E-03, 1.644E-03,
     $     1.739E-03, 1.827E-03, 1.916E-03, 2.013E-03, 2.120E-03/
      DATA (SO2CRS(I),I=  501,  600)/
     1     2.249E-03, 2.386E-03, 2.530E-03, 2.677E-03, 2.887E-03,
     2     3.157E-03, 3.445E-03, 3.585E-03, 3.594E-03, 3.595E-03,
     3     3.568E-03, 3.508E-03, 3.405E-03, 3.272E-03, 3.118E-03,
     4     2.957E-03, 2.838E-03, 2.759E-03, 2.696E-03, 2.657E-03,
     5     2.644E-03, 2.670E-03, 2.730E-03, 2.815E-03, 2.892E-03,
     6     2.976E-03, 3.072E-03, 3.191E-03, 3.337E-03, 3.512E-03,
     7     3.736E-03, 3.959E-03, 4.181E-03, 4.442E-03, 4.750E-03,
     8     5.103E-03, 5.459E-03, 5.736E-03, 5.901E-03, 5.950E-03,
     9     5.968E-03, 5.975E-03, 5.980E-03, 5.967E-03, 5.912E-03,
     $     5.730E-03, 5.564E-03, 5.482E-03, 5.386E-03, 5.282E-03,
     1     5.174E-03, 5.056E-03, 4.922E-03, 4.764E-03, 4.642E-03,
     2     4.518E-03, 4.352E-03, 4.180E-03, 4.008E-03, 3.843E-03,
     3     3.673E-03, 3.488E-03, 3.264E-03, 3.074E-03, 2.915E-03,
     4     2.791E-03, 2.682E-03, 2.579E-03, 2.461E-03, 2.354E-03,
     5     2.262E-03, 2.177E-03, 2.095E-03, 2.017E-03, 1.948E-03,
     6     1.897E-03, 1.872E-03, 1.866E-03, 1.878E-03, 1.918E-03,
     7     1.967E-03, 2.022E-03, 2.078E-03, 2.142E-03, 2.213E-03,
     8     2.304E-03, 2.409E-03, 2.530E-03, 2.684E-03, 2.899E-03,
     9     3.188E-03, 3.627E-03, 3.996E-03, 4.251E-03, 4.545E-03,
     $     4.824E-03, 5.055E-03, 5.171E-03, 5.180E-03, 5.030E-03/
      DATA (SO2CRS(I),I=  601,  700)/
     1     4.908E-03, 4.767E-03, 4.512E-03, 4.301E-03, 4.147E-03,
     2     4.105E-03, 4.095E-03, 4.106E-03, 4.159E-03, 4.239E-03,
     3     4.347E-03, 4.464E-03, 4.581E-03, 4.696E-03, 4.812E-03,
     4     4.927E-03, 5.031E-03, 5.074E-03, 5.077E-03, 5.058E-03,
     5     5.033E-03, 4.998E-03, 4.928E-03, 4.796E-03, 4.578E-03,
     6     4.264E-03, 3.942E-03, 3.667E-03, 3.474E-03, 3.324E-03,
     7     3.195E-03, 3.034E-03, 2.859E-03, 2.729E-03, 2.609E-03,
     8     2.490E-03, 2.362E-03, 2.232E-03, 2.102E-03, 1.991E-03,
     9     1.885E-03, 1.772E-03, 1.672E-03, 1.584E-03, 1.517E-03,
     $     1.452E-03, 1.390E-03, 1.357E-03, 1.334E-03, 1.318E-03,
     1     1.304E-03, 1.292E-03, 1.284E-03, 1.286E-03, 1.294E-03,
     2     1.301E-03, 1.308E-03, 1.316E-03, 1.324E-03, 1.331E-03,
     3     1.339E-03, 1.345E-03, 1.352E-03, 1.360E-03, 1.370E-03,
     4     1.382E-03, 1.396E-03, 1.413E-03, 1.433E-03, 1.460E-03,
     5     1.487E-03, 1.509E-03, 1.546E-03, 1.594E-03, 1.653E-03,
     6     1.715E-03, 1.781E-03, 1.849E-03, 1.919E-03, 1.991E-03,
     7     2.072E-03, 2.160E-03, 2.261E-03, 2.372E-03, 2.491E-03,
     8     2.617E-03, 2.752E-03, 2.902E-03, 3.045E-03, 3.182E-03,
     9     3.317E-03, 3.471E-03, 3.655E-03, 3.927E-03, 4.201E-03,
     $     4.433E-03, 4.715E-03, 5.020E-03, 5.314E-03, 5.519E-03/
      DATA (SO2CRS(I),I=  701,  800)/
     1     5.600E-03, 5.366E-03, 4.921E-03, 4.198E-03, 3.716E-03,
     2     3.385E-03, 3.132E-03, 2.885E-03, 2.635E-03, 2.411E-03,
     3     2.191E-03, 1.964E-03, 1.795E-03, 1.668E-03, 1.568E-03,
     4     1.457E-03, 1.319E-03, 1.086E-03, 8.464E-04, 6.889E-04,
     5     4.589E-04, 2.139E-04, 3.041E-04, 5.345E-04, 8.927E-04,
     6     1.101E-03, 1.224E-03, 1.281E-03, 1.358E-03, 1.465E-03,
     7     1.573E-03, 1.670E-03, 1.743E-03, 1.786E-03, 1.810E-03,
     8     1.823E-03, 1.836E-03, 1.848E-03, 1.861E-03, 1.873E-03,
     9     1.883E-03, 1.887E-03, 1.887E-03, 1.876E-03, 1.864E-03,
     $     1.851E-03, 1.850E-03, 1.859E-03, 1.881E-03, 1.906E-03,
     1     1.932E-03, 1.965E-03, 2.004E-03, 2.050E-03, 2.102E-03,
     2     2.158E-03, 2.217E-03, 2.274E-03, 2.324E-03, 2.380E-03,
     3     2.439E-03, 2.463E-03, 2.465E-03, 2.438E-03, 2.389E-03,
     4     2.324E-03, 2.246E-03, 2.155E-03, 2.045E-03, 1.946E-03,
     5     1.850E-03, 1.741E-03, 1.621E-03, 1.486E-03, 1.352E-03,
     6     1.217E-03, 1.079E-03, 9.417E-04, 8.030E-04, 6.603E-04,
     7     5.335E-04, 4.611E-04, 5.192E-04, 6.871E-04, 8.005E-04,
     8     8.945E-04, 9.754E-04, 1.052E-03, 1.128E-03, 1.209E-03,
     9     1.300E-03, 1.410E-03, 1.521E-03, 1.628E-03, 1.708E-03,
     $     1.782E-03, 1.857E-03, 1.939E-03, 2.025E-03, 2.111E-03/
      DATA (SO2CRS(I),I=  801,  900)/
     1     2.197E-03, 2.281E-03, 2.249E-03, 2.150E-03, 2.046E-03,
     2     1.958E-03, 1.899E-03, 1.823E-03, 1.739E-03, 1.664E-03,
     3     1.581E-03, 1.479E-03, 1.348E-03, 1.199E-03, 1.039E-03,
     4     8.984E-04, 7.998E-04, 6.785E-04, 5.503E-04, 4.636E-04,
     5     3.874E-04, 3.173E-04, 2.535E-04, 1.990E-04, 1.840E-04,
     6     1.814E-04, 1.891E-04, 1.989E-04, 2.100E-04, 2.221E-04,
     7     2.354E-04, 2.512E-04, 2.745E-04, 3.059E-04, 3.606E-04,
     8     4.281E-04, 5.150E-04, 5.952E-04, 6.677E-04, 7.312E-04,
     9     8.003E-04, 8.953E-04, 9.786E-04, 1.049E-03, 1.143E-03,
     $     1.242E-03, 1.329E-03, 1.401E-03, 1.452E-03, 1.484E-03,
     1     1.500E-03, 1.481E-03, 1.458E-03, 1.433E-03, 1.401E-03,
     2     1.363E-03, 1.312E-03, 1.252E-03, 1.180E-03, 1.087E-03,
     3     9.794E-04, 8.785E-04, 7.813E-04, 6.902E-04, 6.055E-04,
     4     5.277E-04, 4.631E-04, 4.053E-04, 3.566E-04, 3.160E-04,
     5     2.864E-04, 2.806E-04, 2.906E-04, 3.250E-04, 3.670E-04,
     6     4.161E-04, 4.676E-04, 5.188E-04, 5.504E-04, 5.797E-04,
     7     6.182E-04, 6.713E-04, 7.412E-04, 8.294E-04, 9.124E-04,
     8     9.325E-04, 9.381E-04, 9.329E-04, 9.197E-04, 8.991E-04,
     9     8.563E-04, 7.909E-04, 6.749E-04, 5.614E-04, 4.548E-04,
     $     3.609E-04, 2.753E-04, 2.072E-04, 1.509E-04, 1.125E-04/
      DATA (SO2CRS(I),I=  901, 1000)/
     1     9.024E-05, 7.923E-05, 7.706E-05, 7.911E-05, 8.760E-05,
     2     1.006E-04, 1.187E-04, 1.425E-04, 1.700E-04, 2.046E-04,
     3     2.513E-04, 3.218E-04, 3.930E-04, 4.646E-04, 5.451E-04,
     4     6.306E-04, 7.254E-04, 8.222E-04, 9.202E-04, 9.923E-04,
     5     1.047E-03, 1.082E-03, 1.108E-03, 1.120E-03, 1.125E-03,
     6     1.122E-03, 1.112E-03, 1.098E-03, 1.066E-03, 1.006E-03,
     7     8.856E-04, 7.586E-04, 6.296E-04, 5.239E-04, 4.308E-04,
     8     3.571E-04, 2.864E-04, 2.163E-04, 1.604E-04, 1.169E-04,
     9     8.453E-05, 5.856E-05, 4.541E-05, 3.570E-05, 2.946E-05,
     $     2.419E-05, 1.961E-05, 1.560E-05, 1.202E-05, 9.583E-06,
     1     7.515E-06, 5.876E-06, 4.298E-06, 2.812E-06, 1.995E-06,
     2     1.492E-06, 1.474E-06, 1.775E-06, 2.561E-06, 4.123E-06,
     3     6.388E-06, 1.056E-05, 1.618E-05, 2.567E-05, 3.882E-05,
     4     5.822E-05, 8.705E-05, 1.245E-04, 1.811E-04, 2.504E-04,
     5     3.503E-04, 4.719E-04, 6.321E-04, 8.318E-04, 1.069E-03,
     6     1.367E-03, 1.698E-03, 2.096E-03, 2.525E-03, 3.009E-03,
     7     3.519E-03, 4.056E-03, 4.597E-03, 5.138E-03, 5.653E-03,
     8     6.144E-03, 6.577E-03, 6.964E-03, 7.288E-03, 7.549E-03,
     9     7.764E-03, 7.919E-03, 8.049E-03, 8.141E-03, 8.225E-03,
     $     8.301E-03, 8.380E-03, 8.462E-03, 8.547E-03, 8.631E-03/
      DATA (SO2CRS(I),I= 1001, 1100)/
     1     8.703E-03, 8.763E-03, 8.795E-03, 8.805E-03, 8.787E-03,
     2     8.750E-03, 8.703E-03, 8.671E-03, 8.660E-03, 8.723E-03,
     3     8.849E-03, 9.092E-03, 9.455E-03, 9.941E-03, 1.059E-02,
     4     1.133E-02, 1.222E-02, 1.317E-02, 1.421E-02, 1.525E-02,
     5     1.630E-02, 1.730E-02, 1.824E-02, 1.907E-02, 1.983E-02,
     6     2.044E-02, 2.097E-02, 2.136E-02, 2.167E-02, 2.188E-02,
     7     2.203E-02, 2.212E-02, 2.217E-02, 2.219E-02, 2.218E-02,
     8     2.217E-02, 2.217E-02, 2.217E-02, 2.220E-02, 2.225E-02,
     9     2.233E-02, 2.244E-02, 2.257E-02, 2.272E-02, 2.287E-02,
     $     2.303E-02, 2.316E-02, 2.325E-02, 2.328E-02, 2.326E-02,
     1     2.312E-02, 2.293E-02, 2.259E-02, 2.217E-02, 2.159E-02,
     2     2.092E-02, 2.012E-02, 1.920E-02, 1.819E-02, 1.710E-02,
     3     1.597E-02, 1.481E-02, 1.368E-02, 1.265E-02, 1.172E-02,
     4     1.100E-02, 1.052E-02, 1.033E-02, 1.057E-02, 1.110E-02,
     5     1.224E-02, 1.368E-02, 1.579E-02, 1.820E-02, 2.121E-02,
     6     2.449E-02, 2.817E-02, 3.203E-02, 3.605E-02, 4.000E-02,
     7     4.390E-02, 4.741E-02, 5.069E-02, 5.330E-02, 5.555E-02,
     8     5.703E-02, 5.802E-02, 5.833E-02, 5.811E-02, 5.747E-02,
     9     5.646E-02, 5.530E-02, 5.412E-02, 5.303E-02, 5.236E-02,
     $     5.204E-02, 5.251E-02, 5.361E-02, 5.558E-02, 5.815E-02/
      DATA (SO2CRS(I),I= 1101, 1200)/
     1     6.123E-02, 6.516E-02, 6.926E-02, 7.121E-02, 7.282E-02,
     2     7.558E-02, 7.956E-02, 8.665E-02, 9.234E-02, 9.530E-02,
     3     9.524E-02, 9.232E-02, 8.941E-02, 8.650E-02, 8.359E-02,
     4     8.068E-02, 7.778E-02, 7.430E-02, 6.870E-02, 6.550E-02,
     5     6.819E-02, 7.109E-02, 7.399E-02, 7.689E-02, 7.979E-02,
     6     8.269E-02, 8.559E-02, 8.849E-02, 9.082E-02, 8.989E-02,
     7     8.813E-02, 8.532E-02, 8.427E-02, 8.704E-02, 9.057E-02,
     8     9.491E-02, 1.001E-01, 1.059E-01, 1.137E-01, 1.224E-01,
     9     1.322E-01, 1.420E-01, 1.496E-01, 1.559E-01, 1.591E-01,
     $     1.611E-01, 1.612E-01, 1.600E-01, 1.572E-01, 1.535E-01,
     1     1.492E-01, 1.449E-01, 1.404E-01, 1.329E-01, 1.245E-01,
     2     1.183E-01, 1.132E-01, 1.113E-01, 1.109E-01, 1.135E-01,
     3     1.173E-01, 1.230E-01, 1.299E-01, 1.385E-01, 1.463E-01,
     4     1.535E-01, 1.616E-01, 1.703E-01, 1.810E-01, 1.924E-01,
     5     2.039E-01, 2.153E-01, 2.267E-01, 2.360E-01, 2.381E-01,
     6     2.374E-01, 2.319E-01, 2.268E-01, 2.225E-01, 2.224E-01,
     7     2.267E-01, 2.344E-01, 2.444E-01, 2.564E-01, 2.689E-01,
     8     2.784E-01, 2.859E-01, 2.848E-01, 2.818E-01, 2.777E-01,
     9     2.737E-01, 2.708E-01, 2.688E-01, 2.687E-01, 2.703E-01,
     $     2.745E-01, 2.860E-01, 3.058E-01, 3.257E-01, 3.454E-01/
      DATA (SO2CRS(I),I= 1201, 1300)/
     1     3.605E-01, 3.731E-01, 3.838E-01, 3.931E-01, 3.962E-01,
     2     3.956E-01, 3.812E-01, 3.643E-01, 3.474E-01, 3.323E-01,
     3     3.233E-01, 3.141E-01, 3.029E-01, 2.896E-01, 2.728E-01,
     4     2.596E-01, 2.512E-01, 2.524E-01, 2.622E-01, 2.712E-01,
     5     2.798E-01, 2.940E-01, 3.110E-01, 3.289E-01, 3.480E-01,
     6     3.765E-01, 4.090E-01, 4.531E-01, 5.042E-01, 5.794E-01,
     7     6.520E-01, 7.030E-01, 7.392E-01, 7.399E-01, 7.301E-01,
     8     7.027E-01, 6.769E-01, 6.545E-01, 6.422E-01, 6.421E-01,
     9     6.488E-01, 6.612E-01, 6.638E-01, 6.595E-01, 6.489E-01,
     $     6.348E-01, 6.189E-01, 6.021E-01, 5.843E-01, 5.665E-01,
     1     5.518E-01, 5.395E-01, 5.387E-01, 5.451E-01, 5.786E-01,
     2     6.198E-01, 6.764E-01, 7.333E-01, 7.850E-01, 8.371E-01,
     3     8.911E-01, 9.469E-01, 1.006E+00, 1.052E+00, 1.073E+00,
     4     1.088E+00, 1.095E+00, 1.061E+00, 9.809E-01, 9.314E-01,
     5     9.092E-01, 8.823E-01, 8.522E-01, 8.445E-01, 8.501E-01,
     6     8.584E-01, 8.686E-01, 8.892E-01, 9.156E-01, 9.665E-01,
     7     1.029E+00, 1.129E+00, 1.237E+00, 1.324E+00, 1.404E+00,
     8     1.463E+00, 1.509E+00, 1.504E+00, 1.491E+00, 1.483E+00,
     9     1.481E+00, 1.505E+00, 1.529E+00, 1.538E+00, 1.534E+00,
     $     1.492E+00, 1.433E+00, 1.336E+00, 1.246E+00, 1.177E+00/
      DATA (SO2CRS(I),I= 1301, 1400)/
     1     1.118E+00, 1.079E+00, 1.050E+00, 1.038E+00, 1.024E+00,
     2     1.005E+00, 9.783E-01, 9.404E-01, 9.321E-01, 9.611E-01,
     3     1.028E+00, 1.138E+00, 1.234E+00, 1.318E+00, 1.415E+00,
     4     1.523E+00, 1.647E+00, 1.782E+00, 1.875E+00, 1.938E+00,
     5     2.054E+00, 2.204E+00, 2.360E+00, 2.516E+00, 2.476E+00,
     6     2.331E+00, 2.207E+00, 2.096E+00, 2.048E+00, 2.030E+00,
     7     2.036E+00, 2.051E+00, 2.028E+00, 1.991E+00, 2.024E+00,
     8     2.083E+00, 2.151E+00, 2.227E+00, 2.360E+00, 2.509E+00,
     9     2.589E+00, 2.638E+00, 2.574E+00, 2.478E+00, 2.433E+00,
     $     2.403E+00, 2.383E+00, 2.367E+00, 2.369E+00, 2.379E+00,
     1     2.415E+00, 2.448E+00, 2.383E+00, 2.287E+00, 2.151E+00,
     2     2.013E+00, 1.971E+00, 1.956E+00, 1.957E+00, 1.966E+00,
     3     2.018E+00, 2.081E+00, 2.155E+00, 2.237E+00, 2.372E+00,
     4     2.523E+00, 2.704E+00, 2.901E+00, 3.182E+00, 3.471E+00,
     5     3.611E+00, 3.694E+00, 3.591E+00, 3.446E+00, 3.383E+00,
     6     3.349E+00, 3.386E+00, 3.451E+00, 3.602E+00, 3.767E+00,
     7     3.790E+00, 3.754E+00, 3.484E+00, 3.152E+00, 3.010E+00,
     8     2.937E+00, 2.959E+00, 3.015E+00, 3.097E+00, 3.181E+00,
     9     3.115E+00, 2.992E+00, 2.962E+00, 2.969E+00, 2.906E+00,
     $     2.810E+00, 2.648E+00, 2.457E+00, 2.340E+00, 2.263E+00/
      DATA (SO2CRS(I),I= 1401, 1500)/
     1     2.220E+00, 2.199E+00, 2.270E+00, 2.397E+00, 2.499E+00,
     2     2.583E+00, 2.738E+00, 2.946E+00, 3.113E+00, 3.244E+00,
     3     3.236E+00, 3.097E+00, 3.110E+00, 3.286E+00, 3.517E+00,
     4     3.815E+00, 4.155E+00, 4.554E+00, 4.960E+00, 5.380E+00,
     5     5.764E+00, 6.084E+00, 6.403E+00, 6.723E+00, 6.906E+00,
     6     6.769E+00, 6.533E+00, 6.068E+00, 5.603E+00, 5.175E+00,
     7     4.765E+00, 4.407E+00, 4.126E+00, 4.148E+00, 4.173E+00,
     8     4.040E+00, 3.887E+00, 3.743E+00, 3.610E+00, 3.535E+00,
     9     3.482E+00, 3.524E+00, 3.587E+00, 3.633E+00, 3.674E+00,
     $     3.694E+00, 3.703E+00, 3.616E+00, 3.490E+00, 3.384E+00,
     1     3.290E+00, 3.296E+00, 3.363E+00, 3.457E+00, 3.571E+00,
     2     3.753E+00, 3.996E+00, 4.281E+00, 4.615E+00, 4.973E+00,
     3     5.367E+00, 5.727E+00, 6.024E+00, 6.312E+00, 6.582E+00,
     4     6.838E+00, 7.056E+00, 7.260E+00, 7.417E+00, 7.644E+00,
     5     8.218E+00, 8.824E+00, 9.356E+00, 9.797E+00, 9.570E+00,
     6     9.172E+00, 8.506E+00, 7.758E+00, 6.942E+00, 6.115E+00,
     7     5.744E+00, 5.595E+00, 5.544E+00, 5.554E+00, 5.532E+00,
     8     5.483E+00, 5.310E+00, 4.996E+00, 4.819E+00, 4.851E+00,
     9     4.898E+00, 4.975E+00, 5.040E+00, 5.073E+00, 5.073E+00,
     $     4.962E+00, 4.856E+00, 4.828E+00, 4.822E+00, 4.895E+00/
      DATA (SO2CRS(I),I= 1501, 1600)/
     1     4.995E+00, 5.210E+00, 5.458E+00, 5.743E+00, 6.044E+00,
     2     6.414E+00, 6.818E+00, 7.347E+00, 7.960E+00, 8.635E+00,
     3     9.370E+00, 1.013E+01, 1.092E+01, 1.175E+01, 1.267E+01,
     4     1.348E+01, 1.397E+01, 1.417E+01, 1.332E+01, 1.230E+01,
     5     1.096E+01, 9.597E+00, 8.465E+00, 7.440E+00, 7.193E+00,
     6     7.193E+00, 6.891E+00, 6.446E+00, 5.963E+00, 5.454E+00,
     7     5.137E+00, 5.016E+00, 4.987E+00, 5.100E+00, 5.159E+00,
     8     5.094E+00, 5.032E+00, 5.000E+00, 4.987E+00, 5.067E+00,
     9     5.165E+00, 5.320E+00, 5.494E+00, 5.783E+00, 6.111E+00,
     $     6.497E+00, 6.910E+00, 7.246E+00, 7.526E+00, 7.849E+00,
     1     8.221E+00, 8.605E+00, 9.008E+00, 9.509E+00, 1.028E+01,
     2     1.110E+01, 1.210E+01, 1.318E+01, 1.460E+01, 1.605E+01,
     3     1.707E+01, 1.793E+01, 1.791E+01, 1.749E+01, 1.676E+01,
     4     1.580E+01, 1.481E+01, 1.380E+01, 1.278E+01, 1.178E+01,
     5     1.081E+01, 9.918E+00, 9.031E+00, 8.135E+00, 7.265E+00,
     6     6.616E+00, 6.027E+00, 5.623E+00, 5.290E+00, 5.057E+00,
     7     4.885E+00, 4.728E+00, 4.586E+00, 4.539E+00, 4.653E+00,
     8     4.816E+00, 5.109E+00, 5.403E+00, 5.665E+00, 5.928E+00,
     9     6.239E+00, 6.563E+00, 6.937E+00, 7.329E+00, 7.701E+00,
     $     8.059E+00, 8.521E+00, 9.099E+00, 9.756E+00, 1.056E+01/
      DATA (SO2CRS(I),I= 1601, 1700)/
     1     1.144E+01, 1.250E+01, 1.356E+01, 1.448E+01, 1.538E+01,
     2     1.621E+01, 1.703E+01, 1.793E+01, 1.888E+01, 1.990E+01,
     3     2.098E+01, 2.135E+01, 2.058E+01, 1.924E+01, 1.634E+01,
     4     1.356E+01, 1.184E+01, 1.030E+01, 9.133E+00, 8.080E+00,
     5     7.418E+00, 6.934E+00, 6.613E+00, 6.426E+00, 6.272E+00,
     6     6.167E+00, 6.121E+00, 6.233E+00, 6.334E+00, 6.332E+00,
     7     6.333E+00, 6.514E+00, 6.744E+00, 7.001E+00, 7.272E+00,
     8     7.598E+00, 7.977E+00, 8.417E+00, 8.969E+00, 9.517E+00,
     9     1.005E+01, 1.058E+01, 1.109E+01, 1.161E+01, 1.240E+01,
     $     1.329E+01, 1.435E+01, 1.553E+01, 1.678E+01, 1.813E+01,
     1     1.940E+01, 2.049E+01, 2.155E+01, 2.250E+01, 2.341E+01,
     2     2.403E+01, 2.455E+01, 2.419E+01, 2.337E+01, 2.172E+01,
     3     1.919E+01, 1.670E+01, 1.431E+01, 1.214E+01, 1.087E+01,
     4     9.699E+00, 8.545E+00, 7.410E+00, 6.763E+00, 6.352E+00,
     5     6.139E+00, 6.122E+00, 6.143E+00, 6.241E+00, 6.388E+00,
     6     6.724E+00, 7.090E+00, 7.560E+00, 8.052E+00, 8.366E+00,
     7     8.591E+00, 9.120E+00, 9.974E+00, 1.073E+01, 1.125E+01,
     8     1.177E+01, 1.235E+01, 1.293E+01, 1.333E+01, 1.369E+01,
     9     1.424E+01, 1.490E+01, 1.579E+01, 1.698E+01, 1.826E+01,
     $     1.979E+01, 2.131E+01, 2.267E+01, 2.402E+01, 2.549E+01/
      DATA (SO2CRS(I),I= 1701, 1800)/
     1     2.702E+01, 2.833E+01, 2.947E+01, 3.029E+01, 3.053E+01,
     2     3.064E+01, 3.028E+01, 2.971E+01, 2.752E+01, 2.488E+01,
     3     2.065E+01, 1.555E+01, 1.213E+01, 1.087E+01, 9.751E+00,
     4     9.008E+00, 8.374E+00, 8.311E+00, 8.360E+00, 8.575E+00,
     5     8.860E+00, 9.337E+00, 9.998E+00, 1.073E+01, 1.161E+01,
     6     1.250E+01, 1.336E+01, 1.422E+01, 1.515E+01, 1.610E+01,
     7     1.672E+01, 1.707E+01, 1.772E+01, 1.898E+01, 2.013E+01,
     8     2.069E+01, 2.118E+01, 2.162E+01, 2.204E+01, 2.235E+01,
     9     2.258E+01, 2.274E+01, 2.279E+01, 2.287E+01, 2.308E+01,
     $     2.335E+01, 2.417E+01, 2.516E+01, 2.604E+01, 2.684E+01,
     1     2.718E+01, 2.666E+01, 2.601E+01, 2.484E+01, 2.361E+01,
     2     2.232E+01, 2.101E+01, 1.967E+01, 1.830E+01, 1.692E+01,
     3     1.548E+01, 1.404E+01, 1.260E+01, 1.121E+01, 1.067E+01,
     4     1.043E+01, 1.031E+01, 1.029E+01, 1.037E+01, 1.067E+01,
     5     1.107E+01, 1.211E+01, 1.323E+01, 1.367E+01, 1.381E+01,
     6     1.425E+01, 1.507E+01, 1.603E+01, 1.739E+01, 1.867E+01,
     7     1.894E+01, 1.896E+01, 1.920E+01, 1.957E+01, 2.054E+01,
     8     2.239E+01, 2.427E+01, 2.620E+01, 2.795E+01, 2.866E+01,
     9     2.918E+01, 2.896E+01, 2.824E+01, 2.741E+01, 2.642E+01,
     $     2.539E+01, 2.415E+01, 2.288E+01, 2.172E+01, 2.059E+01/
      DATA (SO2CRS(I),I= 1801, 1900)/
     1     1.942E+01, 1.820E+01, 1.703E+01, 1.600E+01, 1.508E+01,
     2     1.477E+01, 1.456E+01, 1.398E+01, 1.315E+01, 1.259E+01,
     3     1.242E+01, 1.231E+01, 1.255E+01, 1.286E+01, 1.326E+01,
     4     1.369E+01, 1.420E+01, 1.483E+01, 1.531E+01, 1.538E+01,
     5     1.543E+01, 1.559E+01, 1.579E+01, 1.633E+01, 1.710E+01,
     6     1.794E+01, 1.899E+01, 2.004E+01, 2.098E+01, 2.190E+01,
     7     2.246E+01, 2.277E+01, 2.325E+01, 2.398E+01, 2.470E+01,
     8     2.537E+01, 2.602E+01, 2.634E+01, 2.642E+01, 2.626E+01,
     9     2.573E+01, 2.528E+01, 2.534E+01, 2.549E+01, 2.559E+01,
     $     2.567E+01, 2.543E+01, 2.474E+01, 2.348E+01, 2.060E+01,
     1     1.771E+01, 1.633E+01, 1.547E+01, 1.493E+01, 1.485E+01,
     2     1.485E+01, 1.504E+01, 1.526E+01, 1.574E+01, 1.631E+01,
     3     1.689E+01, 1.751E+01, 1.823E+01, 1.928E+01, 2.033E+01,
     4     2.120E+01, 2.200E+01, 2.265E+01, 2.309E+01, 2.344E+01,
     5     2.350E+01, 2.355E+01, 2.367E+01, 2.382E+01, 2.411E+01,
     6     2.462E+01, 2.504E+01, 2.519E+01, 2.535E+01, 2.578E+01,
     7     2.631E+01, 2.665E+01, 2.673E+01, 2.665E+01, 2.608E+01,
     8     2.547E+01, 2.469E+01, 2.385E+01, 2.287E+01, 2.170E+01,
     9     2.062E+01, 2.013E+01, 1.968E+01, 1.854E+01, 1.716E+01,
     $     1.629E+01, 1.616E+01, 1.609E+01, 1.644E+01, 1.685E+01/
      DATA (SO2CRS(I),I= 1901, 2000)/
     1     1.765E+01, 1.870E+01, 1.935E+01, 1.940E+01, 1.945E+01,
     2     1.957E+01, 1.970E+01, 2.001E+01, 2.046E+01, 2.088E+01,
     3     2.126E+01, 2.163E+01, 2.209E+01, 2.257E+01, 2.307E+01,
     4     2.358E+01, 2.424E+01, 2.537E+01, 2.651E+01, 2.733E+01,
     5     2.803E+01, 2.793E+01, 2.665E+01, 2.534E+01, 2.395E+01,
     6     2.256E+01, 2.200E+01, 2.203E+01, 2.213E+01, 2.232E+01,
     7     2.255E+01, 2.293E+01, 2.333E+01, 2.341E+01, 2.326E+01,
     8     2.288E+01, 2.183E+01, 2.079E+01, 2.044E+01, 2.033E+01,
     9     2.011E+01, 1.975E+01, 1.940E+01, 1.914E+01, 1.890E+01,
     $     1.876E+01, 1.870E+01, 1.872E+01, 1.896E+01, 1.925E+01,
     1     1.995E+01, 2.078E+01, 2.154E+01, 2.217E+01, 2.279E+01,
     2     2.329E+01, 2.377E+01, 2.437E+01, 2.506E+01, 2.574E+01,
     3     2.641E+01, 2.708E+01, 2.774E+01, 2.839E+01, 2.886E+01,
     4     2.909E+01, 2.909E+01, 2.777E+01, 2.622E+01, 2.456E+01,
     5     2.282E+01, 2.127E+01, 2.027E+01, 1.934E+01, 1.901E+01,
     6     1.888E+01, 1.892E+01, 1.921E+01, 1.953E+01, 2.001E+01,
     7     2.051E+01, 2.079E+01, 2.091E+01, 2.106E+01, 2.127E+01,
     8     2.149E+01, 2.194E+01, 2.246E+01, 2.277E+01, 2.280E+01,
     9     2.282E+01, 2.284E+01, 2.286E+01, 2.295E+01, 2.308E+01,
     $     2.333E+01, 2.388E+01, 2.446E+01, 2.495E+01, 2.542E+01/
      DATA (SO2CRS(I),I= 2001, 2100)/
     1     2.570E+01, 2.572E+01, 2.574E+01, 2.577E+01, 2.579E+01,
     2     2.586E+01, 2.600E+01, 2.618E+01, 2.659E+01, 2.704E+01,
     3     2.734E+01, 2.754E+01, 2.779E+01, 2.816E+01, 2.851E+01,
     4     2.835E+01, 2.802E+01, 2.697E+01, 2.384E+01, 2.071E+01,
     5     1.949E+01, 1.895E+01, 1.857E+01, 1.846E+01, 1.838E+01,
     6     1.852E+01, 1.869E+01, 1.898E+01, 1.936E+01, 1.970E+01,
     7     1.991E+01, 2.009E+01, 2.025E+01, 2.040E+01, 2.066E+01,
     8     2.122E+01, 2.178E+01, 2.225E+01, 2.267E+01, 2.310E+01,
     9     2.352E+01, 2.393E+01, 2.428E+01, 2.459E+01, 2.484E+01,
     $     2.499E+01, 2.517E+01, 2.557E+01, 2.600E+01, 2.633E+01,
     1     2.651E+01, 2.673E+01, 2.718E+01, 2.767E+01, 2.818E+01,
     2     2.871E+01, 2.905E+01, 2.834E+01, 2.744E+01, 2.617E+01,
     3     2.465E+01, 2.349E+01, 2.343E+01, 2.343E+01, 2.335E+01,
     4     2.322E+01, 2.298E+01, 2.243E+01, 2.188E+01, 2.159E+01,
     5     2.138E+01, 2.123E+01, 2.122E+01, 2.121E+01, 2.116E+01,
     6     2.109E+01, 2.091E+01, 2.058E+01, 2.025E+01, 2.001E+01,
     7     1.981E+01, 1.980E+01, 2.006E+01, 2.033E+01, 2.056E+01,
     8     2.079E+01, 2.117E+01, 2.179E+01, 2.233E+01, 2.243E+01,
     9     2.245E+01, 2.247E+01, 2.250E+01, 2.255E+01, 2.277E+01,
     $     2.303E+01, 2.359E+01, 2.462E+01, 2.556E+01, 2.601E+01/
      DATA (SO2CRS(I),I= 2101, 2200)/
     1     2.637E+01, 2.681E+01, 2.735E+01, 2.775E+01, 2.734E+01,
     2     2.680E+01, 2.509E+01, 2.170E+01, 1.870E+01, 1.807E+01,
     3     1.785E+01, 1.787E+01, 1.826E+01, 1.863E+01, 1.889E+01,
     4     1.913E+01, 1.928E+01, 1.930E+01, 1.933E+01, 1.935E+01,
     5     1.937E+01, 1.943E+01, 1.953E+01, 1.965E+01, 1.994E+01,
     6     2.025E+01, 2.060E+01, 2.100E+01, 2.141E+01, 2.179E+01,
     7     2.216E+01, 2.254E+01, 2.291E+01, 2.329E+01, 2.369E+01,
     8     2.410E+01, 2.451E+01, 2.489E+01, 2.528E+01, 2.563E+01,
     9     2.598E+01, 2.626E+01, 2.643E+01, 2.659E+01, 2.672E+01,
     $     2.683E+01, 2.690E+01, 2.682E+01, 2.671E+01, 2.636E+01,
     1     2.592E+01, 2.531E+01, 2.423E+01, 2.313E+01, 2.193E+01,
     2     2.068E+01, 1.971E+01, 1.957E+01, 1.948E+01, 1.944E+01,
     3     1.942E+01, 1.941E+01, 1.948E+01, 1.957E+01, 1.973E+01,
     4     1.993E+01, 2.016E+01, 2.050E+01, 2.087E+01, 2.116E+01,
     5     2.137E+01, 2.157E+01, 2.170E+01, 2.180E+01, 2.201E+01,
     6     2.237E+01, 2.276E+01, 2.360E+01, 2.458E+01, 2.536E+01,
     7     2.554E+01, 2.569E+01, 2.560E+01, 2.535E+01, 2.485E+01,
     8     2.361E+01, 2.233E+01, 2.101E+01, 1.965E+01, 1.840E+01,
     9     1.778E+01, 1.728E+01, 1.726E+01, 1.796E+01, 1.866E+01,
     $     1.939E+01, 2.012E+01, 2.088E+01, 2.169E+01, 2.250E+01/
      DATA (SO2CRS(I),I= 2201, 2300)/
     1     2.319E+01, 2.385E+01, 2.431E+01, 2.417E+01, 2.398E+01,
     2     2.322E+01, 2.205E+01, 2.088E+01, 1.970E+01, 1.852E+01,
     3     1.767E+01, 1.729E+01, 1.700E+01, 1.721E+01, 1.750E+01,
     4     1.774E+01, 1.791E+01, 1.809E+01, 1.848E+01, 1.893E+01,
     5     1.938E+01, 1.984E+01, 2.029E+01, 2.075E+01, 2.120E+01,
     6     2.165E+01, 2.206E+01, 2.246E+01, 2.272E+01, 2.278E+01,
     7     2.282E+01, 2.254E+01, 2.215E+01, 2.179E+01, 2.154E+01,
     8     2.130E+01, 2.106E+01, 2.080E+01, 2.047E+01, 1.967E+01,
     9     1.879E+01, 1.841E+01, 1.874E+01, 1.908E+01, 1.940E+01,
     $     1.972E+01, 2.007E+01, 2.050E+01, 2.095E+01, 2.122E+01,
     1     2.138E+01, 2.146E+01, 2.106E+01, 2.059E+01, 2.011E+01,
     2     1.964E+01, 1.917E+01, 1.883E+01, 1.853E+01, 1.841E+01,
     3     1.880E+01, 1.923E+01, 1.969E+01, 2.018E+01, 2.062E+01,
     4     2.082E+01, 2.097E+01, 2.093E+01, 2.061E+01, 2.025E+01,
     5     1.937E+01, 1.831E+01, 1.744E+01, 1.709E+01, 1.678E+01,
     6     1.667E+01, 1.669E+01, 1.671E+01, 1.675E+01, 1.679E+01,
     7     1.687E+01, 1.701E+01, 1.715E+01, 1.735E+01, 1.756E+01,
     8     1.777E+01, 1.797E+01, 1.817E+01, 1.836E+01, 1.853E+01,
     9     1.869E+01, 1.877E+01, 1.882E+01, 1.887E+01, 1.891E+01,
     $     1.894E+01, 1.902E+01, 1.914E+01, 1.926E+01, 1.939E+01/
      DATA (SO2CRS(I),I= 2301, 2400)/
     1     1.953E+01, 1.962E+01, 1.962E+01, 1.960E+01, 1.950E+01,
     2     1.936E+01, 1.922E+01, 1.913E+01, 1.905E+01, 1.901E+01,
     3     1.901E+01, 1.901E+01, 1.899E+01, 1.898E+01, 1.894E+01,
     4     1.877E+01, 1.857E+01, 1.819E+01, 1.755E+01, 1.690E+01,
     5     1.635E+01, 1.583E+01, 1.536E+01, 1.503E+01, 1.472E+01,
     6     1.449E+01, 1.439E+01, 1.430E+01, 1.429E+01, 1.430E+01,
     7     1.435E+01, 1.452E+01, 1.471E+01, 1.514E+01, 1.593E+01,
     8     1.672E+01, 1.735E+01, 1.791E+01, 1.837E+01, 1.852E+01,
     9     1.864E+01, 1.874E+01, 1.878E+01, 1.883E+01, 1.892E+01,
     $     1.903E+01, 1.919E+01, 1.951E+01, 1.984E+01, 2.023E+01,
     1     2.070E+01, 2.115E+01, 2.116E+01, 2.103E+01, 2.077E+01,
     2     2.014E+01, 1.948E+01, 1.881E+01, 1.813E+01, 1.747E+01,
     3     1.723E+01, 1.713E+01, 1.704E+01, 1.701E+01, 1.698E+01,
     4     1.697E+01, 1.699E+01, 1.701E+01, 1.703E+01, 1.705E+01,
     5     1.707E+01, 1.708E+01, 1.709E+01, 1.709E+01, 1.706E+01,
     6     1.703E+01, 1.699E+01, 1.693E+01, 1.687E+01, 1.681E+01,
     7     1.675E+01, 1.669E+01, 1.661E+01, 1.653E+01, 1.647E+01,
     8     1.643E+01, 1.639E+01, 1.645E+01, 1.655E+01, 1.665E+01,
     9     1.678E+01, 1.691E+01, 1.703E+01, 1.710E+01, 1.718E+01,
     $     1.723E+01, 1.728E+01, 1.730E+01, 1.720E+01, 1.707E+01/
      DATA (SO2CRS(I),I= 2401, 2500)/
     1     1.687E+01, 1.658E+01, 1.628E+01, 1.600E+01, 1.574E+01,
     2     1.551E+01, 1.549E+01, 1.550E+01, 1.549E+01, 1.538E+01,
     3     1.527E+01, 1.499E+01, 1.445E+01, 1.393E+01, 1.362E+01,
     4     1.338E+01, 1.317E+01, 1.314E+01, 1.313E+01, 1.315E+01,
     5     1.320E+01, 1.325E+01, 1.331E+01, 1.336E+01, 1.341E+01,
     6     1.352E+01, 1.365E+01, 1.378E+01, 1.392E+01, 1.405E+01,
     7     1.419E+01, 1.432E+01, 1.445E+01, 1.467E+01, 1.494E+01,
     8     1.521E+01, 1.550E+01, 1.579E+01, 1.601E+01, 1.601E+01,
     9     1.600E+01, 1.591E+01, 1.572E+01, 1.553E+01, 1.504E+01,
     $     1.437E+01, 1.372E+01, 1.324E+01, 1.279E+01, 1.242E+01,
     1     1.227E+01, 1.214E+01, 1.210E+01, 1.221E+01, 1.231E+01,
     2     1.246E+01, 1.263E+01, 1.280E+01, 1.293E+01, 1.306E+01,
     3     1.320E+01, 1.336E+01, 1.353E+01, 1.361E+01, 1.357E+01,
     4     1.353E+01, 1.336E+01, 1.310E+01, 1.285E+01, 1.267E+01,
     5     1.251E+01, 1.236E+01, 1.228E+01, 1.221E+01, 1.227E+01,
     6     1.252E+01, 1.277E+01, 1.339E+01, 1.427E+01, 1.513E+01,
     7     1.571E+01, 1.618E+01, 1.657E+01, 1.641E+01, 1.617E+01,
     8     1.562E+01, 1.461E+01, 1.360E+01, 1.300E+01, 1.268E+01,
     9     1.235E+01, 1.196E+01, 1.156E+01, 1.117E+01, 1.094E+01,
     $     1.073E+01, 1.059E+01, 1.063E+01, 1.069E+01, 1.077E+01/
      DATA (SO2CRS(I),I= 2501, 2600)/
     1     1.087E+01, 1.097E+01, 1.115E+01, 1.137E+01, 1.158E+01,
     2     1.175E+01, 1.191E+01, 1.203E+01, 1.193E+01, 1.180E+01,
     3     1.165E+01, 1.145E+01, 1.126E+01, 1.111E+01, 1.103E+01,
     4     1.096E+01, 1.099E+01, 1.107E+01, 1.117E+01, 1.137E+01,
     5     1.160E+01, 1.187E+01, 1.234E+01, 1.284E+01, 1.325E+01,
     6     1.339E+01, 1.351E+01, 1.355E+01, 1.347E+01, 1.339E+01,
     7     1.320E+01, 1.293E+01, 1.266E+01, 1.221E+01, 1.171E+01,
     8     1.126E+01, 1.111E+01, 1.101E+01, 1.099E+01, 1.118E+01,
     9     1.138E+01, 1.163E+01, 1.192E+01, 1.222E+01, 1.235E+01,
     $     1.236E+01, 1.236E+01, 1.230E+01, 1.221E+01, 1.209E+01,
     1     1.176E+01, 1.139E+01, 1.103E+01, 1.068E+01, 1.034E+01,
     2     1.005E+01, 9.856E+00, 9.661E+00, 9.549E+00, 9.560E+00,
     3     9.572E+00, 9.655E+00, 9.788E+00, 9.922E+00, 1.009E+01,
     4     1.026E+01, 1.045E+01, 1.071E+01, 1.098E+01, 1.121E+01,
     5     1.130E+01, 1.138E+01, 1.132E+01, 1.105E+01, 1.078E+01,
     6     1.058E+01, 1.047E+01, 1.036E+01, 1.030E+01, 1.027E+01,
     7     1.026E+01, 1.049E+01, 1.080E+01, 1.112E+01, 1.142E+01,
     8     1.173E+01, 1.199E+01, 1.204E+01, 1.204E+01, 1.202E+01,
     9     1.194E+01, 1.185E+01, 1.169E+01, 1.144E+01, 1.118E+01,
     $     1.089E+01, 1.059E+01, 1.028E+01, 1.015E+01, 1.013E+01/
      DATA (SO2CRS(I),I= 2601, 2700)/
     1     1.011E+01, 1.006E+01, 1.001E+01, 9.926E+00, 9.720E+00,
     2     9.493E+00, 9.262E+00, 8.999E+00, 8.731E+00, 8.527E+00,
     3     8.508E+00, 8.500E+00, 8.518E+00, 8.573E+00, 8.629E+00,
     4     8.699E+00, 8.790E+00, 8.880E+00, 8.829E+00, 8.683E+00,
     5     8.533E+00, 8.337E+00, 8.124E+00, 7.915E+00, 7.747E+00,
     6     7.593E+00, 7.485E+00, 7.652E+00, 7.864E+00, 8.062E+00,
     7     8.175E+00, 8.274E+00, 8.360E+00, 8.406E+00, 8.450E+00,
     8     8.477E+00, 8.477E+00, 8.477E+00, 8.477E+00, 8.477E+00,
     9     8.477E+00, 8.479E+00, 8.483E+00, 8.486E+00, 8.505E+00,
     $     8.534E+00, 8.563E+00, 8.604E+00, 8.649E+00, 8.688E+00,
     1     8.641E+00, 8.564E+00, 8.483E+00, 8.374E+00, 8.260E+00,
     2     8.168E+00, 8.202E+00, 8.258E+00, 8.313E+00, 8.368E+00,
     3     8.422E+00, 8.485E+00, 8.572E+00, 8.660E+00, 8.754E+00,
     4     8.856E+00, 8.958E+00, 8.998E+00, 8.948E+00, 8.897E+00,
     5     8.820E+00, 8.724E+00, 8.628E+00, 8.545E+00, 8.469E+00,
     6     8.394E+00, 8.320E+00, 8.246E+00, 8.179E+00, 8.229E+00,
     7     8.318E+00, 8.411E+00, 8.555E+00, 8.716E+00, 8.867E+00,
     8     8.956E+00, 9.035E+00, 9.116E+00, 9.209E+00, 9.305E+00,
     9     9.380E+00, 9.331E+00, 9.261E+00, 9.141E+00, 8.876E+00,
     $     8.602E+00, 8.373E+00, 8.275E+00, 8.186E+00, 8.089E+00/
      DATA (SO2CRS(I),I= 2701, 2800)/
     1     7.970E+00, 7.850E+00, 7.784E+00, 7.797E+00, 7.811E+00,
     2     7.840E+00, 7.894E+00, 7.948E+00, 7.958E+00, 7.905E+00,
     3     7.851E+00, 7.805E+00, 7.763E+00, 7.722E+00, 7.697E+00,
     4     7.681E+00, 7.666E+00, 7.634E+00, 7.590E+00, 7.546E+00,
     5     7.519E+00, 7.504E+00, 7.488E+00, 7.462E+00, 7.434E+00,
     6     7.406E+00, 7.389E+00, 7.375E+00, 7.362E+00, 7.348E+00,
     7     7.333E+00, 7.319E+00, 7.305E+00, 7.290E+00, 7.277E+00,
     8     7.273E+00, 7.273E+00, 7.273E+00, 7.273E+00, 7.273E+00,
     9     7.273E+00, 7.273E+00, 7.273E+00, 7.273E+00, 7.273E+00,
     $     7.273E+00, 7.273E+00, 7.273E+00, 7.273E+00, 7.271E+00,
     1     7.260E+00, 7.246E+00, 7.233E+00, 7.219E+00, 7.204E+00,
     2     7.187E+00, 7.149E+00, 7.107E+00, 7.066E+00, 7.023E+00,
     3     6.980E+00, 6.938E+00, 6.898E+00, 6.859E+00, 6.819E+00,
     4     6.781E+00, 6.742E+00, 6.695E+00, 6.620E+00, 6.544E+00,
     5     6.469E+00, 6.395E+00, 6.322E+00, 6.245E+00, 6.158E+00,
     6     6.070E+00, 5.985E+00, 5.910E+00, 5.836E+00, 5.772E+00,
     7     5.738E+00, 5.706E+00, 5.681E+00, 5.680E+00, 5.680E+00,
     8     5.683E+00, 5.693E+00, 5.704E+00, 5.720E+00, 5.752E+00,
     9     5.785E+00, 5.824E+00, 5.879E+00, 5.936E+00, 5.990E+00,
     $     6.037E+00, 6.083E+00, 6.130E+00, 6.177E+00, 6.225E+00/
      DATA (SO2CRS(I),I= 2801, 2900)/
     1     6.276E+00, 6.336E+00, 6.397E+00, 6.443E+00, 6.446E+00,
     2     6.446E+00, 6.446E+00, 6.446E+00, 6.446E+00, 6.444E+00,
     3     6.433E+00, 6.421E+00, 6.401E+00, 6.335E+00, 6.263E+00,
     4     6.192E+00, 6.132E+00, 6.074E+00, 6.017E+00, 5.970E+00,
     5     5.924E+00, 5.876E+00, 5.812E+00, 5.746E+00, 5.680E+00,
     6     5.615E+00, 5.551E+00, 5.485E+00, 5.409E+00, 5.332E+00,
     7     5.256E+00, 5.192E+00, 5.133E+00, 5.074E+00, 5.023E+00,
     8     4.975E+00, 4.926E+00, 4.865E+00, 4.801E+00, 4.737E+00,
     9     4.677E+00, 4.619E+00, 4.561E+00, 4.502E+00, 4.442E+00,
     $     4.382E+00, 4.319E+00, 4.253E+00, 4.188E+00, 4.162E+00,
     1     4.162E+00, 4.162E+00, 4.180E+00, 4.210E+00, 4.240E+00,
     2     4.299E+00, 4.400E+00, 4.501E+00, 4.598E+00, 4.689E+00,
     3     4.779E+00, 4.880E+00, 4.995E+00, 5.111E+00, 5.226E+00,
     4     5.341E+00, 5.456E+00, 5.562E+00, 5.640E+00, 5.716E+00,
     5     5.793E+00, 5.872E+00, 5.952E+00, 6.020E+00, 6.055E+00,
     6     6.088E+00, 6.106E+00, 6.040E+00, 5.959E+00, 5.880E+00,
     7     5.806E+00, 5.733E+00, 5.660E+00, 5.603E+00, 5.550E+00,
     8     5.497E+00, 5.453E+00, 5.411E+00, 5.368E+00, 5.323E+00,
     9     5.276E+00, 5.229E+00, 5.186E+00, 5.147E+00, 5.107E+00,
     $     5.064E+00, 5.019E+00, 4.974E+00, 4.931E+00, 4.894E+00/
      DATA (SO2CRS(I),I= 2901, 3000)/
     1     4.856E+00, 4.820E+00, 4.785E+00, 4.751E+00, 4.714E+00,
     2     4.670E+00, 4.625E+00, 4.581E+00, 4.541E+00, 4.500E+00,
     3     4.461E+00, 4.426E+00, 4.392E+00, 4.359E+00, 4.334E+00,
     4     4.311E+00, 4.287E+00, 4.263E+00, 4.239E+00, 4.214E+00,
     5     4.180E+00, 4.142E+00, 4.105E+00, 4.080E+00, 4.065E+00,
     6     4.050E+00, 4.031E+00, 4.009E+00, 3.987E+00, 3.965E+00,
     7     3.942E+00, 3.920E+00, 3.898E+00, 3.877E+00, 3.856E+00,
     8     3.835E+00, 3.814E+00, 3.793E+00, 3.771E+00, 3.745E+00,
     9     3.717E+00, 3.690E+00, 3.669E+00, 3.649E+00, 3.629E+00,
     $     3.614E+00, 3.600E+00, 3.587E+00, 3.579E+00, 3.572E+00,
     1     3.565E+00, 3.544E+00, 3.512E+00, 3.480E+00, 3.456E+00,
     2     3.443E+00, 3.431E+00, 3.413E+00, 3.389E+00, 3.365E+00,
     3     3.343E+00, 3.330E+00, 3.317E+00, 3.306E+00, 3.299E+00,
     4     3.293E+00, 3.287E+00, 3.281E+00, 3.275E+00, 3.268E+00,
     5     3.258E+00, 3.246E+00, 3.234E+00, 3.226E+00, 3.220E+00,
     6     3.214E+00, 3.204E+00, 3.187E+00, 3.170E+00, 3.155E+00,
     7     3.142E+00, 3.129E+00, 3.121E+00, 3.126E+00, 3.132E+00,
     8     3.138E+00, 3.144E+00, 3.150E+00, 3.156E+00, 3.170E+00,
     9     3.187E+00, 3.204E+00, 3.221E+00, 3.239E+00, 3.257E+00,
     $     3.283E+00, 3.321E+00, 3.360E+00, 3.413E+00, 3.488E+00/
      DATA (SO2CRS(I),I= 3001, 3100)/
     1     3.564E+00, 3.626E+00, 3.649E+00, 3.669E+00, 3.691E+00,
     2     3.723E+00, 3.756E+00, 3.789E+00, 3.802E+00, 3.809E+00,
     3     3.815E+00, 3.801E+00, 3.774E+00, 3.746E+00, 3.711E+00,
     4     3.666E+00, 3.620E+00, 3.575E+00, 3.531E+00, 3.486E+00,
     5     3.441E+00, 3.393E+00, 3.344E+00, 3.296E+00, 3.254E+00,
     6     3.213E+00, 3.173E+00, 3.134E+00, 3.095E+00, 3.057E+00,
     7     3.016E+00, 2.973E+00, 2.931E+00, 2.891E+00, 2.859E+00,
     8     2.828E+00, 2.797E+00, 2.763E+00, 2.728E+00, 2.694E+00,
     9     2.660E+00, 2.627E+00, 2.594E+00, 2.567E+00, 2.544E+00,
     $     2.521E+00, 2.500E+00, 2.483E+00, 2.465E+00, 2.447E+00,
     1     2.430E+00, 2.412E+00, 2.394E+00, 2.374E+00, 2.352E+00,
     2     2.331E+00, 2.314E+00, 2.297E+00, 2.280E+00, 2.255E+00,
     3     2.223E+00, 2.191E+00, 2.170E+00, 2.166E+00, 2.162E+00,
     4     2.156E+00, 2.141E+00, 2.125E+00, 2.110E+00, 2.095E+00,
     5     2.080E+00, 2.065E+00, 2.050E+00, 2.035E+00, 2.021E+00,
     6     2.005E+00, 1.988E+00, 1.971E+00, 1.958E+00, 1.953E+00,
     7     1.950E+00, 1.947E+00, 1.947E+00, 1.947E+00, 1.946E+00,
     8     1.944E+00, 1.940E+00, 1.937E+00, 1.935E+00, 1.935E+00,
     9     1.935E+00, 1.934E+00, 1.931E+00, 1.928E+00, 1.925E+00,
     $     1.924E+00, 1.924E+00, 1.924E+00, 1.924E+00, 1.924E+00/
      DATA (SO2CRS(I),I= 3101, 3200)/
     1     1.924E+00, 1.922E+00, 1.919E+00, 1.915E+00, 1.911E+00,
     2     1.904E+00, 1.898E+00, 1.892E+00, 1.891E+00, 1.891E+00,
     3     1.891E+00, 1.886E+00, 1.879E+00, 1.872E+00, 1.866E+00,
     4     1.860E+00, 1.853E+00, 1.847E+00, 1.840E+00, 1.834E+00,
     5     1.827E+00, 1.820E+00, 1.814E+00, 1.808E+00, 1.804E+00,
     6     1.801E+00, 1.798E+00, 1.793E+00, 1.787E+00, 1.780E+00,
     7     1.774E+00, 1.768E+00, 1.761E+00, 1.755E+00, 1.752E+00,
     8     1.749E+00, 1.746E+00, 1.741E+00, 1.735E+00, 1.729E+00,
     9     1.724E+00, 1.721E+00, 1.718E+00, 1.715E+00, 1.709E+00,
     $     1.703E+00, 1.696E+00, 1.688E+00, 1.679E+00, 1.670E+00,
     1     1.665E+00, 1.662E+00, 1.659E+00, 1.655E+00, 1.646E+00,
     2     1.637E+00, 1.629E+00, 1.626E+00, 1.623E+00, 1.620E+00,
     3     1.619E+00, 1.619E+00, 1.619E+00, 1.622E+00, 1.631E+00,
     4     1.639E+00, 1.647E+00, 1.656E+00, 1.665E+00, 1.674E+00,
     5     1.685E+00, 1.697E+00, 1.708E+00, 1.717E+00, 1.723E+00,
     6     1.729E+00, 1.736E+00, 1.747E+00, 1.760E+00, 1.772E+00,
     7     1.782E+00, 1.792E+00, 1.801E+00, 1.810E+00, 1.820E+00,
     8     1.830E+00, 1.839E+00, 1.849E+00, 1.858E+00, 1.868E+00,
     9     1.877E+00, 1.887E+00, 1.897E+00, 1.905E+00, 1.912E+00,
     $     1.919E+00, 1.923E+00, 1.920E+00, 1.917E+00, 1.914E+00/
      DATA (SO2CRS(I),I= 3201, 3300)/
     1     1.905E+00, 1.895E+00, 1.885E+00, 1.866E+00, 1.841E+00,
     2     1.816E+00, 1.789E+00, 1.757E+00, 1.725E+00, 1.693E+00,
     3     1.666E+00, 1.641E+00, 1.616E+00, 1.589E+00, 1.561E+00,
     4     1.533E+00, 1.508E+00, 1.490E+00, 1.472E+00, 1.455E+00,
     5     1.432E+00, 1.408E+00, 1.384E+00, 1.360E+00, 1.335E+00,
     6     1.311E+00, 1.287E+00, 1.270E+00, 1.254E+00, 1.238E+00,
     7     1.224E+00, 1.209E+00, 1.195E+00, 1.180E+00, 1.164E+00,
     8     1.149E+00, 1.133E+00, 1.118E+00, 1.103E+00, 1.088E+00,
     9     1.074E+00, 1.062E+00, 1.049E+00, 1.037E+00, 1.024E+00,
     $     1.011E+00, 9.986E-01, 9.868E-01, 9.752E-01, 9.636E-01,
     1     9.522E-01, 9.411E-01, 9.299E-01, 9.189E-01, 9.081E-01,
     2     8.974E-01, 8.867E-01, 8.763E-01, 8.660E-01, 8.557E-01,
     3     8.456E-01, 8.358E-01, 8.260E-01, 8.170E-01, 8.121E-01,
     4     8.080E-01, 8.039E-01, 7.991E-01, 7.937E-01, 7.883E-01,
     5     7.833E-01, 7.793E-01, 7.753E-01, 7.715E-01, 7.695E-01,
     6     7.682E-01, 7.668E-01, 7.650E-01, 7.624E-01, 7.597E-01,
     7     7.572E-01, 7.546E-01, 7.521E-01, 7.496E-01, 7.470E-01,
     8     7.445E-01, 7.420E-01, 7.398E-01, 7.385E-01, 7.373E-01,
     9     7.360E-01, 7.319E-01, 7.270E-01, 7.220E-01, 7.185E-01,
     $     7.173E-01, 7.161E-01, 7.151E-01, 7.150E-01, 7.150E-01/
      DATA (SO2CRS(I),I= 3301, 3400)/
     1     7.150E-01, 7.150E-01, 7.150E-01, 7.150E-01, 7.150E-01,
     2     7.150E-01, 7.150E-01, 7.150E-01, 7.150E-01, 7.150E-01,
     3     7.150E-01, 7.150E-01, 7.150E-01, 7.150E-01, 7.150E-01,
     4     7.150E-01, 7.150E-01, 7.150E-01, 7.150E-01, 7.150E-01,
     5     7.150E-01, 7.150E-01, 7.150E-01, 7.150E-01, 7.150E-01,
     6     7.150E-01, 7.150E-01, 7.150E-01, 7.150E-01, 7.150E-01,
     7     7.150E-01, 7.150E-01, 7.150E-01, 7.150E-01, 7.150E-01,
     8     7.145E-01, 7.134E-01, 7.123E-01, 7.112E-01, 7.109E-01,
     9     7.109E-01, 7.109E-01, 7.109E-01, 7.109E-01, 7.109E-01,
     $     7.109E-01, 7.109E-01, 7.109E-01, 7.109E-01, 7.109E-01,
     1     7.109E-01, 7.109E-01, 7.109E-01, 7.109E-01, 7.109E-01,
     2     7.109E-01, 7.109E-01, 7.109E-01, 7.109E-01, 7.106E-01,
     3     7.094E-01, 7.081E-01, 7.069E-01, 7.066E-01, 7.066E-01,
     4     7.066E-01, 7.066E-01, 7.066E-01, 7.066E-01, 7.066E-01,
     5     7.057E-01, 7.046E-01, 7.035E-01, 7.026E-01, 7.026E-01,
     6     7.026E-01, 7.025E-01, 7.016E-01, 7.005E-01, 6.993E-01,
     7     6.981E-01, 6.970E-01, 6.959E-01, 6.948E-01, 6.937E-01,
     8     6.925E-01, 6.914E-01, 6.899E-01, 6.876E-01, 6.854E-01,
     9     6.832E-01, 6.818E-01, 6.806E-01, 6.794E-01, 6.783E-01,
     $     6.772E-01, 6.762E-01, 6.751E-01, 6.740E-01, 6.729E-01/
      DATA (SO2CRS(I),I= 3401, 3500)/
     1     6.718E-01, 6.703E-01, 6.681E-01, 6.660E-01, 6.638E-01,
     2     6.616E-01, 6.594E-01, 6.572E-01, 6.550E-01, 6.529E-01,
     3     6.508E-01, 6.487E-01, 6.466E-01, 6.443E-01, 6.421E-01,
     4     6.400E-01, 6.380E-01, 6.360E-01, 6.339E-01, 6.318E-01,
     5     6.297E-01, 6.276E-01, 6.251E-01, 6.221E-01, 6.191E-01,
     6     6.161E-01, 6.131E-01, 6.100E-01, 6.069E-01, 6.039E-01,
     7     6.010E-01, 5.981E-01, 5.953E-01, 5.938E-01, 5.928E-01,
     8     5.919E-01, 5.909E-01, 5.900E-01, 5.890E-01, 5.881E-01,
     9     5.871E-01, 5.861E-01, 5.852E-01, 5.840E-01, 5.821E-01,
     $     5.803E-01, 5.784E-01, 5.765E-01, 5.746E-01, 5.727E-01,
     1     5.711E-01, 5.701E-01, 5.692E-01, 5.683E-01, 5.674E-01,
     2     5.664E-01, 5.655E-01, 5.645E-01, 5.636E-01, 5.627E-01,
     3     5.618E-01, 5.605E-01, 5.586E-01, 5.567E-01, 5.550E-01,
     4     5.540E-01, 5.532E-01, 5.524E-01, 5.511E-01, 5.494E-01,
     5     5.476E-01, 5.458E-01, 5.440E-01, 5.423E-01, 5.406E-01,
     6     5.389E-01, 5.371E-01, 5.352E-01, 5.334E-01, 5.304E-01,
     7     5.270E-01, 5.237E-01, 5.207E-01, 5.190E-01, 5.174E-01,
     8     5.157E-01, 5.136E-01, 5.111E-01, 5.087E-01, 5.065E-01,
     9     5.062E-01, 5.062E-01, 5.062E-01, 5.069E-01, 5.085E-01,
     $     5.101E-01, 5.118E-01, 5.141E-01, 5.165E-01, 5.190E-01/
      DATA (SO2CRS(I),I= 3501, 3600)/
     1     5.215E-01, 5.240E-01, 5.266E-01, 5.291E-01, 5.301E-01,
     2     5.301E-01, 5.301E-01, 5.311E-01, 5.352E-01, 5.395E-01,
     3     5.437E-01, 5.470E-01, 5.496E-01, 5.522E-01, 5.548E-01,
     4     5.575E-01, 5.602E-01, 5.629E-01, 5.656E-01, 5.684E-01,
     5     5.711E-01, 5.739E-01, 5.766E-01, 5.794E-01, 5.822E-01,
     6     5.849E-01, 5.878E-01, 5.906E-01, 5.934E-01, 5.958E-01,
     7     5.977E-01, 5.996E-01, 6.016E-01, 6.035E-01, 6.055E-01,
     8     6.074E-01, 6.097E-01, 6.124E-01, 6.152E-01, 6.179E-01,
     9     6.208E-01, 6.238E-01, 6.268E-01, 6.297E-01, 6.318E-01,
     $     6.338E-01, 6.358E-01, 6.370E-01, 6.370E-01, 6.370E-01,
     1     6.370E-01, 6.370E-01, 6.370E-01, 6.370E-01, 6.365E-01,
     2     6.346E-01, 6.326E-01, 6.306E-01, 6.292E-01, 6.282E-01,
     3     6.273E-01, 6.260E-01, 6.233E-01, 6.203E-01, 6.173E-01,
     4     6.137E-01, 6.091E-01, 6.045E-01, 5.999E-01, 5.925E-01,
     5     5.832E-01, 5.738E-01, 5.639E-01, 5.508E-01, 5.370E-01,
     6     5.233E-01, 5.100E-01, 4.974E-01, 4.848E-01, 4.723E-01,
     7     4.620E-01, 4.525E-01, 4.429E-01, 4.334E-01, 4.241E-01,
     8     4.147E-01, 4.054E-01, 3.993E-01, 3.980E-01, 3.968E-01,
     9     3.955E-01, 3.952E-01, 3.952E-01, 3.952E-01, 3.952E-01,
     $     3.952E-01, 3.952E-01, 3.952E-01, 3.955E-01, 3.961E-01/
      DATA (SO2CRS(I),I= 3601, 3700)/
     1     3.967E-01, 3.973E-01, 3.989E-01, 4.009E-01, 4.028E-01,
     2     4.052E-01, 4.089E-01, 4.126E-01, 4.164E-01, 4.217E-01,
     3     4.293E-01, 4.369E-01, 4.447E-01, 4.562E-01, 4.690E-01,
     4     4.818E-01, 4.949E-01, 5.096E-01, 5.246E-01, 5.396E-01,
     5     5.553E-01, 5.719E-01, 5.886E-01, 6.052E-01, 6.217E-01,
     6     6.382E-01, 6.547E-01, 6.716E-01, 6.915E-01, 7.118E-01,
     7     7.321E-01, 7.528E-01, 7.742E-01, 7.957E-01, 8.171E-01,
     8     8.407E-01, 8.657E-01, 8.907E-01, 9.158E-01, 9.420E-01,
     9     9.683E-01, 9.946E-01, 1.022E+00, 1.053E+00, 1.083E+00,
     $     1.114E+00, 1.144E+00, 1.172E+00, 1.201E+00, 1.228E+00,
     1     1.237E+00, 1.239E+00, 1.241E+00, 1.216E+00, 1.115E+00,
     2     1.009E+00, 9.034E-01, 8.394E-01, 8.363E-01, 8.338E-01,
     3     8.313E-01, 8.310E-01, 8.324E-01, 8.337E-01, 8.445E-01,
     4     9.118E-01, 9.885E-01, 1.065E+00, 1.143E+00, 1.224E+00,
     5     1.305E+00, 1.386E+00, 1.472E+00, 1.560E+00, 1.649E+00,
     6     1.736E+00, 1.805E+00, 1.867E+00, 1.929E+00, 1.985E+00,
     7     1.998E+00, 2.005E+00, 2.011E+00, 1.983E+00, 1.904E+00,
     8     1.825E+00, 1.747E+00, 1.672E+00, 1.600E+00, 1.529E+00,
     9     1.458E+00, 1.399E+00, 1.345E+00, 1.290E+00, 1.266E+00,
     $     1.420E+00, 1.605E+00, 1.789E+00, 2.159E+00, 2.799E+00/
      DATA (SO2CRS(I),I= 3701, 3800)/
     1     3.442E+00, 4.083E+00, 4.513E+00, 4.799E+00, 5.085E+00,
     2     5.357E+00, 5.427E+00, 5.427E+00, 5.427E+00, 5.427E+00,
     3     5.427E+00, 5.427E+00, 5.427E+00, 5.398E+00, 5.327E+00,
     4     5.256E+00, 5.184E+00, 5.027E+00, 4.813E+00, 4.599E+00,
     5     4.407E+00, 4.570E+00, 4.856E+00, 5.142E+00, 5.383E+00,
     6     5.356E+00, 5.285E+00, 5.213E+00, 5.072E+00, 4.727E+00,
     7     4.370E+00, 4.013E+00, 3.656E+00, 3.299E+00, 2.942E+00,
     8     2.586E+00, 2.314E+00, 2.100E+00, 1.885E+00, 1.697E+00,
     9     1.914E+00, 2.271E+00, 2.628E+00, 2.976E+00, 3.271E+00,
     $     3.556E+00, 3.842E+00, 4.076E+00, 4.161E+00, 4.236E+00,
     1     4.312E+00, 4.471E+00, 4.686E+00, 4.901E+00, 5.109E+00,
     2     5.215E+00, 5.286E+00, 5.357E+00, 5.418E+00, 5.427E+00,
     3     5.427E+00, 5.427E+00, 5.440E+00, 5.526E+00, 5.625E+00,
     4     5.724E+00, 5.806E+00, 5.837E+00, 5.866E+00, 5.894E+00,
     5     5.715E+00, 5.233E+00, 4.749E+00, 4.266E+00, 4.028E+00,
     6     3.956E+00, 3.885E+00, 3.804E+00, 3.571E+00, 3.285E+00,
     7     2.999E+00, 2.740E+00, 2.642E+00, 2.571E+00, 2.499E+00,
     8     2.376E+00, 2.100E+00, 1.814E+00, 1.528E+00, 1.358E+00,
     9     1.357E+00, 1.357E+00, 1.357E+00, 1.398E+00, 1.465E+00,
     $     1.533E+00, 1.603E+00, 1.885E+00, 2.314E+00, 2.742E+00/
      DATA (SO2CRS(I),I= 3801, 3900)/
     1     3.180E+00, 3.771E+00, 4.413E+00, 5.056E+00, 5.654E+00,
     2     5.984E+00, 6.270E+00, 6.556E+00, 6.789E+00, 6.870E+00,
     3     6.941E+00, 7.013E+00, 7.193E+00, 7.530E+00, 7.870E+00,
     4     8.209E+00, 8.642E+00, 9.212E+00, 9.783E+00, 1.036E+01,
     5     1.131E+01, 1.253E+01, 1.374E+01, 1.489E+01, 1.498E+01,
     6     1.471E+01, 1.444E+01, 1.407E+01, 1.228E+01, 9.998E+00,
     7     7.713E+00, 5.722E+00, 5.499E+00, 5.570E+00, 5.642E+00,
     8     5.762E+00, 6.024E+00, 6.296E+00, 6.567E+00, 6.772E+00,
     9     6.784E+00, 6.784E+00, 6.784E+00, 6.813E+00, 6.884E+00,
     $     6.956E+00, 7.028E+00, 7.218E+00, 7.490E+00, 7.761E+00,
     1     8.031E+00, 8.141E+00, 8.141E+00, 8.141E+00, 8.141E+00,
     2     8.141E+00, 8.141E+00, 8.141E+00, 8.144E+00, 8.198E+00,
     3     8.270E+00, 8.341E+00, 8.446E+00, 8.752E+00, 9.091E+00,
     4     9.430E+00, 9.763E+00, 1.005E+01, 1.034E+01, 1.063E+01,
     5     1.086E+01, 1.094E+01, 1.100E+01, 1.107E+01, 1.121E+01,
     6     1.155E+01, 1.191E+01, 1.227E+01, 1.262E+01, 1.296E+01,
     7     1.330E+01, 1.364E+01, 1.395E+01, 1.424E+01, 1.453E+01,
     8     1.481E+01, 1.493E+01, 1.493E+01, 1.493E+01, 1.493E+01,
     9     1.497E+01, 1.504E+01, 1.511E+01, 1.519E+01, 1.541E+01,
     $     1.569E+01, 1.596E+01, 1.620E+01, 1.606E+01, 1.579E+01/
      DATA (SO2CRS(I),I= 3901, 4000)/
     1     1.552E+01, 1.521E+01, 1.417E+01, 1.288E+01, 1.160E+01,
     2     1.039E+01, 9.634E+00, 8.955E+00, 8.277E+00, 7.632E+00,
     3     7.191E+00, 6.784E+00, 6.377E+00, 6.101E+00, 6.613E+00,
     4     7.255E+00, 7.898E+00, 8.467E+00, 8.819E+00, 9.159E+00,
     5     9.498E+00, 9.854E+00, 1.026E+01, 1.066E+01, 1.107E+01,
     6     1.151E+01, 1.205E+01, 1.259E+01, 1.313E+01, 1.356E+01,
     7     1.365E+01, 1.373E+01, 1.380E+01, 1.398E+01, 1.431E+01,
     8     1.465E+01, 1.499E+01, 1.533E+01, 1.567E+01, 1.601E+01,
     9     1.635E+01, 1.669E+01, 1.703E+01, 1.737E+01, 1.771E+01,
     $     1.805E+01, 1.839E+01, 1.872E+01, 1.906E+01, 1.946E+01,
     1     1.993E+01, 2.041E+01, 2.088E+01, 2.144E+01, 2.212E+01,
     2     2.279E+01, 2.347E+01, 2.359E+01, 2.288E+01, 2.217E+01,
     3     2.145E+01, 2.060E+01, 1.965E+01, 1.870E+01, 1.774E+01,
     4     1.639E+01, 1.476E+01, 1.313E+01, 1.151E+01, 1.081E+01,
     5     1.075E+01, 1.068E+01, 1.061E+01, 1.026E+01, 9.715E+00,
     6     9.172E+00, 8.627E+00, 7.802E+00, 6.784E+00, 5.767E+00,
     7     4.756E+00, 4.545E+00, 4.885E+00, 5.224E+00, 5.563E+00,
     8     5.943E+00, 6.350E+00, 6.757E+00, 7.166E+00, 7.815E+00,
     9     8.629E+00, 9.444E+00, 1.026E+01, 1.136E+01, 1.265E+01,
     $     1.393E+01, 1.522E+01, 1.623E+01, 1.704E+01, 1.786E+01/
      DATA (SO2CRS(I),I= 4001, 4100)/
     1     1.867E+01, 1.900E+01, 1.900E+01, 1.900E+01, 1.900E+01,
     2     1.913E+01, 1.947E+01, 1.981E+01, 2.015E+01, 2.093E+01,
     3     2.235E+01, 2.377E+01, 2.520E+01, 2.615E+01, 2.643E+01,
     4     2.670E+01, 2.697E+01, 2.716E+01, 2.723E+01, 2.730E+01,
     5     2.737E+01, 2.754E+01, 2.788E+01, 2.823E+01, 2.857E+01,
     6     2.893E+01, 2.931E+01, 2.970E+01, 3.008E+01, 3.050E+01,
     7     3.104E+01, 3.158E+01, 3.213E+01, 3.242E+01, 3.199E+01,
     8     3.151E+01, 3.104E+01, 3.008E+01, 2.773E+01, 2.529E+01,
     9     2.285E+01, 2.051E+01, 1.849E+01, 1.649E+01, 1.449E+01,
     $     1.275E+01, 1.262E+01, 1.275E+01, 1.289E+01, 1.296E+01,
     1     1.262E+01, 1.221E+01, 1.180E+01, 1.152E+01, 1.198E+01,
     2     1.256E+01, 1.314E+01, 1.372E+01, 1.417E+01, 1.457E+01,
     3     1.498E+01, 1.540E+01, 1.596E+01, 1.657E+01, 1.718E+01,
     4     1.778E+01, 1.832E+01, 1.884E+01, 1.936E+01, 1.988E+01,
     5     2.077E+01, 2.193E+01, 2.308E+01, 2.423E+01, 2.498E+01,
     6     2.545E+01, 2.593E+01, 2.641E+01, 2.714E+01, 2.804E+01,
     7     2.894E+01, 2.985E+01, 3.063E+01, 3.124E+01, 3.185E+01,
     8     3.247E+01, 3.301E+01, 3.347E+01, 3.392E+01, 3.438E+01,
     9     3.502E+01, 3.621E+01, 3.743E+01, 3.865E+01, 4.002E+01,
     $     4.180E+01, 4.360E+01, 4.540E+01, 4.724E+01, 4.931E+01/
      DATA (SO2CRS(I),I= 4101, 4200)/
     1     5.141E+01, 5.351E+01, 5.584E+01, 5.950E+01, 6.338E+01,
     2     6.727E+01, 7.093E+01, 7.110E+01, 7.006E+01, 6.902E+01,
     3     6.796E+01, 6.411E+01, 5.834E+01, 5.257E+01, 4.680E+01,
     4     4.116E+01, 3.561E+01, 3.005E+01, 2.450E+01, 2.086E+01,
     5     2.003E+01, 1.921E+01, 1.840E+01, 1.760E+01, 1.682E+01,
     6     1.605E+01, 1.527E+01, 1.501E+01, 1.620E+01, 1.750E+01,
     7     1.879E+01, 2.007E+01, 2.130E+01, 2.252E+01, 2.374E+01,
     8     2.484E+01, 2.516E+01, 2.535E+01, 2.555E+01, 2.567E+01,
     9     2.464E+01, 2.322E+01, 2.180E+01, 2.038E+01, 1.958E+01,
     $     1.919E+01, 1.880E+01, 1.841E+01, 1.862E+01, 1.970E+01,
     1     2.079E+01, 2.187E+01, 2.341E+01, 2.560E+01, 2.780E+01,
     2     3.000E+01, 3.180E+01, 3.246E+01, 3.304E+01, 3.362E+01,
     3     3.417E+01, 3.452E+01, 3.484E+01, 3.516E+01, 3.553E+01,
     4     3.668E+01, 3.810E+01, 3.952E+01, 4.096E+01, 4.477E+01,
     5     5.019E+01, 5.562E+01, 6.104E+01, 6.508E+01, 6.818E+01,
     6     7.129E+01, 7.439E+01, 7.765E+01, 8.114E+01, 8.463E+01,
     7     8.813E+01, 8.969E+01, 8.565E+01, 8.125E+01, 7.686E+01,
     8     7.196E+01, 6.406E+01, 5.566E+01, 4.726E+01, 3.902E+01,
     9     3.326E+01, 2.834E+01, 2.343E+01, 1.854E+01, 1.605E+01,
     $     1.521E+01, 1.437E+01, 1.353E+01, 1.345E+01, 1.447E+01/
      DATA (SO2CRS(I),I= 4201, 4300)/
     1     1.551E+01, 1.654E+01, 1.761E+01, 1.876E+01, 1.993E+01,
     2     2.109E+01, 2.217E+01, 2.277E+01, 2.329E+01, 2.380E+01,
     3     2.429E+01, 2.432E+01, 2.419E+01, 2.406E+01, 2.393E+01,
     4     2.376E+01, 2.357E+01, 2.338E+01, 2.318E+01, 2.364E+01,
     5     2.506E+01, 2.648E+01, 2.791E+01, 2.912E+01, 2.974E+01,
     6     3.033E+01, 3.091E+01, 3.147E+01, 3.192E+01, 3.234E+01,
     7     3.277E+01, 3.320E+01, 3.430E+01, 3.586E+01, 3.741E+01,
     8     3.897E+01, 4.146E+01, 4.533E+01, 4.920E+01, 5.308E+01,
     9     5.692E+01, 6.067E+01, 6.441E+01, 6.816E+01, 7.187E+01,
     $     7.539E+01, 7.888E+01, 8.238E+01, 8.588E+01, 8.962E+01,
     1     9.343E+01, 9.724E+01, 1.010E+02, 1.029E+02, 1.035E+02,
     2     1.040E+02, 1.046E+02, 1.044E+02, 1.021E+02, 9.962E+01,
     3     9.717E+01, 9.397E+01, 8.631E+01, 7.791E+01, 6.951E+01,
     4     6.118E+01, 5.400E+01, 4.722E+01, 4.044E+01, 3.365E+01,
     5     2.907E+01, 2.768E+01, 2.632E+01, 2.497E+01, 2.418E+01,
     6     2.504E+01, 2.601E+01, 2.698E+01, 2.791E+01, 2.862E+01,
     7     2.930E+01, 2.998E+01, 3.065E+01, 3.073E+01, 3.041E+01,
     8     3.009E+01, 2.977E+01, 2.940E+01, 2.895E+01, 2.849E+01,
     9     2.804E+01, 2.765E+01, 2.745E+01, 2.727E+01, 2.709E+01,
     $     2.703E+01, 2.893E+01, 3.152E+01, 3.410E+01, 3.668E+01/
      DATA (SO2CRS(I),I= 4301, 4400)/
     1     3.857E+01, 3.999E+01, 4.141E+01, 4.282E+01, 4.445E+01,
     2     4.667E+01, 4.894E+01, 5.120E+01, 5.341E+01, 5.526E+01,
     3     5.705E+01, 5.883E+01, 6.062E+01, 6.161E+01, 6.206E+01,
     4     6.250E+01, 6.295E+01, 6.387E+01, 6.547E+01, 6.707E+01,
     5     6.868E+01, 7.073E+01, 7.546E+01, 8.062E+01, 8.579E+01,
     6     9.107E+01, 9.797E+01, 1.054E+02, 1.129E+02, 1.204E+02,
     7     1.264E+02, 1.304E+02, 1.345E+02, 1.385E+02, 1.405E+02,
     8     1.306E+02, 1.187E+02, 1.068E+02, 9.491E+01, 8.306E+01,
     9     7.122E+01, 5.938E+01, 4.754E+01, 3.893E+01, 3.501E+01,
     $     3.113E+01, 2.725E+01, 2.463E+01, 2.563E+01, 2.687E+01,
     1     2.810E+01, 2.933E+01, 3.042E+01, 3.147E+01, 3.252E+01,
     2     3.357E+01, 3.435E+01, 3.472E+01, 3.508E+01, 3.545E+01,
     3     3.591E+01, 3.692E+01, 3.802E+01, 3.912E+01, 4.029E+01,
     4     4.271E+01, 4.555E+01, 4.839E+01, 5.123E+01, 5.421E+01,
     5     5.742E+01, 6.062E+01, 6.383E+01, 6.699E+01, 6.987E+01,
     6     7.270E+01, 7.554E+01, 7.835E+01, 7.911E+01, 7.847E+01,
     7     7.782E+01, 7.718E+01, 7.672E+01, 7.653E+01, 7.635E+01,
     8     7.617E+01, 7.629E+01, 7.827E+01, 8.055E+01, 8.283E+01,
     9     8.513E+01, 8.884E+01, 9.353E+01, 9.821E+01, 1.029E+02,
     $     1.072E+02, 1.102E+02, 1.132E+02, 1.161E+02, 1.191E+02/
      DATA (SO2CRS(I),I= 4401, 4500)/
     1     1.233E+02, 1.279E+02, 1.325E+02, 1.370E+02, 1.401E+02,
     2     1.411E+02, 1.421E+02, 1.431E+02, 1.432E+02, 1.381E+02,
     3     1.321E+02, 1.261E+02, 1.202E+02, 1.157E+02, 1.123E+02,
     4     1.090E+02, 1.056E+02, 9.965E+01, 8.642E+01, 7.273E+01,
     5     5.904E+01, 4.578E+01, 3.935E+01, 3.528E+01, 3.121E+01,
     6     2.714E+01, 2.534E+01, 2.686E+01, 2.840E+01, 2.994E+01,
     7     3.140E+01, 3.229E+01, 3.309E+01, 3.389E+01, 3.470E+01,
     8     3.664E+01, 3.935E+01, 4.206E+01, 4.477E+01, 4.748E+01,
     9     5.020E+01, 5.292E+01, 5.563E+01, 5.838E+01, 6.156E+01,
     $     6.489E+01, 6.821E+01, 7.154E+01, 7.417E+01, 7.579E+01,
     1     7.739E+01, 7.900E+01, 8.050E+01, 8.137E+01, 8.213E+01,
     2     8.290E+01, 8.366E+01, 8.507E+01, 8.741E+01, 8.975E+01,
     3     9.210E+01, 9.476E+01, 9.937E+01, 1.043E+02, 1.092E+02,
     4     1.142E+02, 1.177E+02, 1.204E+02, 1.230E+02, 1.257E+02,
     5     1.285E+02, 1.316E+02, 1.347E+02, 1.378E+02, 1.409E+02,
     6     1.444E+02, 1.482E+02, 1.520E+02, 1.557E+02, 1.605E+02,
     7     1.681E+02, 1.759E+02, 1.836E+02, 1.910E+02, 1.927E+02,
     8     1.924E+02, 1.921E+02, 1.918E+02, 1.804E+02, 1.529E+02,
     9     1.252E+02, 9.752E+01, 7.074E+01, 5.791E+01, 4.989E+01,
     $     4.187E+01, 3.385E+01, 2.932E+01, 2.987E+01, 3.046E+01/
      DATA (SO2CRS(I),I= 4501, 4600)/
     1     3.105E+01, 3.165E+01, 3.240E+01, 3.320E+01, 3.400E+01,
     2     3.480E+01, 3.587E+01, 3.735E+01, 3.882E+01, 4.030E+01,
     3     4.182E+01, 4.355E+01, 4.532E+01, 4.709E+01, 4.886E+01,
     4     5.146E+01, 5.528E+01, 5.911E+01, 6.294E+01, 6.652E+01,
     5     6.860E+01, 7.042E+01, 7.225E+01, 7.408E+01, 7.604E+01,
     6     7.819E+01, 8.035E+01, 8.251E+01, 8.472E+01, 8.732E+01,
     7     8.998E+01, 9.263E+01, 9.529E+01, 9.882E+01, 1.036E+02,
     8     1.084E+02, 1.132E+02, 1.179E+02, 1.214E+02, 1.247E+02,
     9     1.280E+02, 1.313E+02, 1.357E+02, 1.417E+02, 1.477E+02,
     $     1.537E+02, 1.600E+02, 1.701E+02, 1.817E+02, 1.932E+02,
     1     2.047E+02, 2.139E+02, 2.196E+02, 2.253E+02, 2.309E+02,
     2     2.358E+02, 2.285E+02, 2.169E+02, 2.053E+02, 1.938E+02,
     3     1.837E+02, 1.781E+02, 1.727E+02, 1.674E+02, 1.619E+02,
     4     1.554E+02, 1.486E+02, 1.418E+02, 1.349E+02, 1.248E+02,
     5     1.050E+02, 8.454E+01, 6.413E+01, 4.382E+01, 3.647E+01,
     6     3.802E+01, 3.956E+01, 4.111E+01, 4.243E+01, 4.312E+01,
     7     4.377E+01, 4.442E+01, 4.508E+01, 4.671E+01, 4.901E+01,
     8     5.131E+01, 5.361E+01, 5.599E+01, 5.880E+01, 6.169E+01,
     9     6.459E+01, 6.748E+01, 6.982E+01, 7.135E+01, 7.288E+01,
     $     7.442E+01, 7.596E+01, 7.776E+01, 7.965E+01, 8.153E+01/
      DATA (SO2CRS(I),I= 4601, 4700)/
     1     8.342E+01, 8.554E+01, 8.834E+01, 9.118E+01, 9.402E+01,
     2     9.691E+01, 1.006E+02, 1.046E+02, 1.087E+02, 1.127E+02,
     3     1.168E+02, 1.213E+02, 1.257E+02, 1.302E+02, 1.347E+02,
     4     1.409E+02, 1.482E+02, 1.555E+02, 1.628E+02, 1.698E+02,
     5     1.743E+02, 1.784E+02, 1.825E+02, 1.867E+02, 1.913E+02,
     6     1.966E+02, 2.019E+02, 2.072E+02, 2.123E+02, 2.147E+02,
     7     2.161E+02, 2.175E+02, 2.189E+02, 2.211E+02, 2.255E+02,
     8     2.301E+02, 2.346E+02, 2.391E+02, 2.379E+02, 2.328E+02,
     9     2.276E+02, 2.225E+02, 2.137E+02, 1.833E+02, 1.492E+02,
     $     1.151E+02, 8.097E+01, 5.884E+01, 5.415E+01, 4.961E+01,
     1     4.508E+01, 4.086E+01, 4.171E+01, 4.431E+01, 4.690E+01,
     2     4.950E+01, 5.185E+01, 5.275E+01, 5.339E+01, 5.404E+01,
     3     5.469E+01, 5.546E+01, 5.640E+01, 5.735E+01, 5.830E+01,
     4     5.927E+01, 6.075E+01, 6.241E+01, 6.406E+01, 6.571E+01,
     5     6.778E+01, 7.107E+01, 7.442E+01, 7.778E+01, 8.114E+01,
     6     8.422E+01, 8.711E+01, 8.999E+01, 9.288E+01, 9.584E+01,
     7     9.984E+01, 1.042E+02, 1.086E+02, 1.129E+02, 1.173E+02,
     8     1.215E+02, 1.258E+02, 1.300E+02, 1.343E+02, 1.390E+02,
     9     1.440E+02, 1.490E+02, 1.540E+02, 1.592E+02, 1.651E+02,
     $     1.711E+02, 1.772E+02, 1.832E+02, 1.895E+02, 1.963E+02/
      DATA (SO2CRS(I),I= 4701, 4800)/
     1     2.031E+02, 2.100E+02, 2.168E+02, 2.260E+02, 2.368E+02,
     2     2.476E+02, 2.584E+02, 2.698E+02, 2.849E+02, 3.006E+02,
     3     3.162E+02, 3.319E+02, 3.384E+02, 3.184E+02, 2.966E+02,
     4     2.749E+02, 2.532E+02, 2.306E+02, 2.074E+02, 1.842E+02,
     5     1.611E+02, 1.381E+02, 1.195E+02, 1.023E+02, 8.517E+01,
     6     6.801E+01, 5.543E+01, 5.617E+01, 5.776E+01, 5.934E+01,
     7     6.093E+01, 6.270E+01, 6.476E+01, 6.683E+01, 6.889E+01,
     8     7.091E+01, 7.236E+01, 7.360E+01, 7.484E+01, 7.608E+01,
     9     7.757E+01, 8.052E+01, 8.370E+01, 8.689E+01, 9.008E+01,
     $     9.340E+01, 9.690E+01, 1.004E+02, 1.039E+02, 1.074E+02,
     1     1.096E+02, 1.109E+02, 1.122E+02, 1.135E+02, 1.147E+02,
     2     1.154E+02, 1.160E+02, 1.166E+02, 1.173E+02, 1.190E+02,
     3     1.239E+02, 1.290E+02, 1.340E+02, 1.391E+02, 1.440E+02,
     4     1.485E+02, 1.530E+02, 1.574E+02, 1.620E+02, 1.684E+02,
     5     1.753E+02, 1.823E+02, 1.892E+02, 1.969E+02, 2.090E+02,
     6     2.217E+02, 2.345E+02, 2.473E+02, 2.598E+02, 2.715E+02,
     7     2.832E+02, 2.948E+02, 3.064E+02, 3.073E+02, 3.009E+02,
     8     2.945E+02, 2.882E+02, 2.816E+02, 2.723E+02, 2.620E+02,
     9     2.517E+02, 2.415E+02, 2.314E+02, 2.226E+02, 2.140E+02,
     $     2.054E+02, 1.968E+02, 1.856E+02, 1.667E+02, 1.473E+02/
      DATA (SO2CRS(I),I= 4801, 4900)/
     1     1.279E+02, 1.085E+02, 9.514E+01, 9.052E+01, 8.598E+01,
     2     8.144E+01, 7.726E+01, 7.897E+01, 8.270E+01, 8.644E+01,
     3     9.017E+01, 9.357E+01, 9.498E+01, 9.605E+01, 9.713E+01,
     4     9.820E+01, 9.882E+01, 9.811E+01, 9.731E+01, 9.652E+01,
     5     9.572E+01, 9.598E+01, 9.778E+01, 9.959E+01, 1.014E+02,
     6     1.032E+02, 1.054E+02, 1.079E+02, 1.104E+02, 1.129E+02,
     7     1.154E+02, 1.188E+02, 1.225E+02, 1.262E+02, 1.298E+02,
     8     1.337E+02, 1.386E+02, 1.437E+02, 1.488E+02, 1.539E+02,
     9     1.590E+02, 1.641E+02, 1.692E+02, 1.744E+02, 1.795E+02,
     $     1.841E+02, 1.878E+02, 1.914E+02, 1.951E+02, 1.988E+02,
     1     2.077E+02, 2.200E+02, 2.323E+02, 2.446E+02, 2.567E+02,
     2     2.644E+02, 2.706E+02, 2.767E+02, 2.829E+02, 2.904E+02,
     3     3.056E+02, 3.221E+02, 3.386E+02, 3.551E+02, 3.671E+02,
     4     3.522E+02, 3.328E+02, 3.135E+02, 2.941E+02, 2.686E+02,
     5     2.250E+02, 1.804E+02, 1.358E+02, 9.111E+01, 6.496E+01,
     6     6.575E+01, 6.677E+01, 6.779E+01, 6.881E+01, 7.070E+01,
     7     7.319E+01, 7.567E+01, 7.816E+01, 8.061E+01, 8.253E+01,
     8     8.427E+01, 8.600E+01, 8.774E+01, 8.958E+01, 9.308E+01,
     9     9.714E+01, 1.012E+02, 1.053E+02, 1.092E+02, 1.123E+02,
     $     1.152E+02, 1.182E+02, 1.211E+02, 1.245E+02, 1.292E+02/
      DATA (SO2CRS(I),I= 4901, 5000)/
     1     1.339E+02, 1.387E+02, 1.434E+02, 1.469E+02, 1.484E+02,
     2     1.499E+02, 1.514E+02, 1.530E+02, 1.552E+02, 1.584E+02,
     3     1.616E+02, 1.648E+02, 1.680E+02, 1.715E+02, 1.751E+02,
     4     1.788E+02, 1.824E+02, 1.861E+02, 1.927E+02, 2.013E+02,
     5     2.099E+02, 2.184E+02, 2.271E+02, 2.373E+02, 2.481E+02,
     6     2.588E+02, 2.695E+02, 2.806E+02, 2.942E+02, 3.081E+02,
     7     3.220E+02, 3.359E+02, 3.460E+02, 3.331E+02, 3.165E+02,
     8     2.998E+02, 2.831E+02, 2.670E+02, 2.525E+02, 2.381E+02,
     9     2.237E+02, 2.093E+02, 1.990E+02, 2.002E+02, 2.023E+02,
     $     2.043E+02, 2.063E+02, 2.049E+02, 1.938E+02, 1.821E+02,
     1     1.704E+02, 1.587E+02, 1.474E+02, 1.365E+02, 1.256E+02,
     2     1.148E+02, 1.039E+02, 9.816E+01, 9.980E+01, 1.015E+02,
     3     1.032E+02, 1.049E+02, 1.075E+02, 1.108E+02, 1.140E+02,
     4     1.173E+02, 1.205E+02, 1.224E+02, 1.234E+02, 1.244E+02,
     5     1.254E+02, 1.263E+02, 1.273E+02, 1.283E+02, 1.292E+02,
     6     1.302E+02, 1.312E+02, 1.333E+02, 1.357E+02, 1.382E+02,
     7     1.406E+02, 1.431E+02, 1.466E+02, 1.505E+02, 1.543E+02,
     8     1.582E+02, 1.622E+02, 1.694E+02, 1.776E+02, 1.858E+02,
     9     1.940E+02, 2.019E+02, 2.061E+02, 2.091E+02, 2.120E+02,
     $     2.150E+02, 2.179E+02, 2.209E+02, 2.240E+02, 2.270E+02/
      DATA (SO2CRS(I),I= 5001, 5100)/
     1     2.301E+02, 2.334E+02, 2.385E+02, 2.440E+02, 2.494E+02,
     2     2.548E+02, 2.602E+02, 2.651E+02, 2.700E+02, 2.748E+02,
     3     2.797E+02, 2.856E+02, 2.975E+02, 3.105E+02, 3.235E+02,
     4     3.365E+02, 3.453E+02, 3.289E+02, 3.082E+02, 2.875E+02,
     5     2.668E+02, 2.446E+02, 2.131E+02, 1.801E+02, 1.471E+02,
     6     1.141E+02, 8.541E+01, 8.228E+01, 8.342E+01, 8.456E+01,
     7     8.570E+01, 8.688E+01, 8.830E+01, 8.977E+01, 9.124E+01,
     8     9.271E+01, 9.422E+01, 9.602E+01, 9.786E+01, 9.970E+01,
     9     1.015E+02, 1.034E+02, 1.052E+02, 1.071E+02, 1.089E+02,
     $     1.108E+02, 1.128E+02, 1.162E+02, 1.197E+02, 1.232E+02,
     1     1.267E+02, 1.305E+02, 1.357E+02, 1.411E+02, 1.465E+02,
     2     1.520E+02, 1.567E+02, 1.570E+02, 1.565E+02, 1.561E+02,
     3     1.557E+02, 1.551E+02, 1.542E+02, 1.533E+02, 1.523E+02,
     4     1.513E+02, 1.510E+02, 1.543E+02, 1.584E+02, 1.624E+02,
     5     1.664E+02, 1.706E+02, 1.761E+02, 1.818E+02, 1.875E+02,
     6     1.932E+02, 1.992E+02, 2.073E+02, 2.157E+02, 2.241E+02,
     7     2.325E+02, 2.410E+02, 2.511E+02, 2.617E+02, 2.722E+02,
     8     2.828E+02, 2.936E+02, 3.067E+02, 3.206E+02, 3.345E+02,
     9     3.484E+02, 3.610E+02, 3.515E+02, 3.345E+02, 3.174E+02,
     $     3.004E+02, 2.834E+02, 2.668E+02, 2.503E+02, 2.338E+02/
      DATA (SO2CRS(I),I= 5101, 5200)/
     1     2.174E+02, 2.010E+02, 1.915E+02, 1.868E+02, 1.820E+02,
     2     1.773E+02, 1.725E+02, 1.614E+02, 1.459E+02, 1.305E+02,
     3     1.150E+02, 9.961E+01, 9.359E+01, 9.401E+01, 9.443E+01,
     4     9.485E+01, 9.526E+01, 9.670E+01, 9.962E+01, 1.026E+02,
     5     1.055E+02, 1.084E+02, 1.108E+02, 1.123E+02, 1.138E+02,
     6     1.153E+02, 1.168E+02, 1.183E+02, 1.196E+02, 1.208E+02,
     7     1.220E+02, 1.233E+02, 1.247E+02, 1.265E+02, 1.284E+02,
     8     1.303E+02, 1.322E+02, 1.347E+02, 1.412E+02, 1.484E+02,
     9     1.555E+02, 1.627E+02, 1.700E+02, 1.778E+02, 1.857E+02,
     $     1.937E+02, 2.016E+02, 2.094E+02, 2.141E+02, 2.178E+02,
     1     2.215E+02, 2.252E+02, 2.291E+02, 2.365E+02, 2.451E+02,
     2     2.537E+02, 2.623E+02, 2.709E+02, 2.772E+02, 2.819E+02,
     3     2.866E+02, 2.913E+02, 2.961E+02, 2.984E+02, 2.990E+02,
     4     2.997E+02, 3.004E+02, 3.011E+02, 3.005E+02, 2.982E+02,
     5     2.959E+02, 2.936E+02, 2.912E+02, 2.868E+02, 2.762E+02,
     6     2.652E+02, 2.542E+02, 2.432E+02, 2.335E+02, 2.274E+02,
     7     2.215E+02, 2.156E+02, 2.097E+02, 2.023E+02, 1.858E+02,
     8     1.678E+02, 1.498E+02, 1.318E+02, 1.147E+02, 1.123E+02,
     9     1.150E+02, 1.176E+02, 1.203E+02, 1.230E+02, 1.251E+02,
     $     1.268E+02, 1.285E+02, 1.302E+02, 1.319E+02, 1.340E+02/
      DATA (SO2CRS(I),I= 5201, 5300)/
     1     1.364E+02, 1.388E+02, 1.413E+02, 1.437E+02, 1.472E+02,
     2     1.523E+02, 1.573E+02, 1.624E+02, 1.675E+02, 1.711E+02,
     3     1.707E+02, 1.700E+02, 1.693E+02, 1.687E+02, 1.681E+02,
     4     1.683E+02, 1.686E+02, 1.689E+02, 1.692E+02, 1.697E+02,
     5     1.716E+02, 1.741E+02, 1.766E+02, 1.791E+02, 1.816E+02,
     6     1.843E+02, 1.872E+02, 1.901E+02, 1.930E+02, 1.958E+02,
     7     2.003E+02, 2.069E+02, 2.136E+02, 2.203E+02, 2.270E+02,
     8     2.341E+02, 2.428E+02, 2.516E+02, 2.603E+02, 2.691E+02,
     9     2.777E+02, 2.859E+02, 2.939E+02, 3.020E+02, 3.101E+02,
     $     3.181E+02, 3.260E+02, 3.338E+02, 3.416E+02, 3.494E+02,
     1     3.570E+02, 3.486E+02, 3.291E+02, 3.096E+02, 2.901E+02,
     2     2.706E+02, 2.536E+02, 2.402E+02, 2.268E+02, 2.135E+02,
     3     2.001E+02, 1.866E+02, 1.725E+02, 1.584E+02, 1.443E+02,
     4     1.302E+02, 1.184E+02, 1.197E+02, 1.233E+02, 1.269E+02,
     5     1.304E+02, 1.340E+02, 1.356E+02, 1.358E+02, 1.361E+02,
     6     1.363E+02, 1.366E+02, 1.377E+02, 1.399E+02, 1.422E+02,
     7     1.444E+02, 1.466E+02, 1.487E+02, 1.503E+02, 1.519E+02,
     8     1.535E+02, 1.550E+02, 1.565E+02, 1.571E+02, 1.577E+02,
     9     1.582E+02, 1.588E+02, 1.594E+02, 1.606E+02, 1.622E+02,
     $     1.639E+02, 1.656E+02, 1.672E+02, 1.704E+02, 1.756E+02/
      DATA (SO2CRS(I),I= 5301, 5400)/
     1     1.808E+02, 1.860E+02, 1.912E+02, 1.964E+02, 2.013E+02,
     2     2.062E+02, 2.111E+02, 2.161E+02, 2.211E+02, 2.271E+02,
     3     2.334E+02, 2.398E+02, 2.462E+02, 2.525E+02, 2.602E+02,
     4     2.688E+02, 2.773E+02, 2.859E+02, 2.944E+02, 2.953E+02,
     5     2.850E+02, 2.746E+02, 2.642E+02, 2.538E+02, 2.432E+02,
     6     2.314E+02, 2.193E+02, 2.073E+02, 1.952E+02, 1.839E+02,
     7     1.848E+02, 1.900E+02, 1.951E+02, 2.002E+02, 2.053E+02,
     8     2.090E+02, 2.107E+02, 2.123E+02, 2.139E+02, 2.155E+02,
     9     2.146E+02, 2.064E+02, 1.977E+02, 1.890E+02, 1.803E+02,
     $     1.719E+02, 1.679E+02, 1.654E+02, 1.629E+02, 1.604E+02,
     1     1.579E+02, 1.559E+02, 1.542E+02, 1.525E+02, 1.509E+02,
     2     1.492E+02, 1.486E+02, 1.512E+02, 1.539E+02, 1.566E+02,
     3     1.593E+02, 1.618E+02, 1.615E+02, 1.602E+02, 1.589E+02,
     4     1.576E+02, 1.563E+02, 1.555E+02, 1.551E+02, 1.547E+02,
     5     1.543E+02, 1.539E+02, 1.537E+02, 1.540E+02, 1.543E+02,
     6     1.546E+02, 1.549E+02, 1.554E+02, 1.596E+02, 1.650E+02,
     7     1.704E+02, 1.758E+02, 1.813E+02, 1.863E+02, 1.908E+02,
     8     1.953E+02, 1.998E+02, 2.043E+02, 2.083E+02, 2.109E+02,
     9     2.134E+02, 2.159E+02, 2.184E+02, 2.211E+02, 2.269E+02,
     $     2.337E+02, 2.406E+02, 2.474E+02, 2.542E+02, 2.599E+02/
      DATA (SO2CRS(I),I= 5401, 5500)/
     1     2.639E+02, 2.679E+02, 2.720E+02, 2.760E+02, 2.778E+02,
     2     2.667E+02, 2.535E+02, 2.402E+02, 2.270E+02, 2.137E+02,
     3     1.999E+02, 1.857E+02, 1.714E+02, 1.572E+02, 1.430E+02,
     4     1.326E+02, 1.334E+02, 1.349E+02, 1.365E+02, 1.380E+02,
     5     1.395E+02, 1.421E+02, 1.449E+02, 1.478E+02, 1.506E+02,
     6     1.535E+02, 1.563E+02, 1.590E+02, 1.617E+02, 1.645E+02,
     7     1.672E+02, 1.690E+02, 1.655E+02, 1.610E+02, 1.566E+02,
     8     1.522E+02, 1.478E+02, 1.427E+02, 1.373E+02, 1.319E+02,
     9     1.265E+02, 1.210E+02, 1.182E+02, 1.229E+02, 1.281E+02,
     $     1.332E+02, 1.384E+02, 1.436E+02, 1.493E+02, 1.550E+02,
     1     1.608E+02, 1.666E+02, 1.724E+02, 1.783E+02, 1.843E+02,
     2     1.903E+02, 1.964E+02, 2.024E+02, 2.082E+02, 2.130E+02,
     3     2.177E+02, 2.223E+02, 2.269E+02, 2.315E+02, 2.301E+02,
     4     2.245E+02, 2.190E+02, 2.135E+02, 2.079E+02, 2.022E+02,
     5     1.959E+02, 1.895E+02, 1.831E+02, 1.767E+02, 1.703E+02,
     6     1.667E+02, 1.651E+02, 1.635E+02, 1.619E+02, 1.603E+02,
     7     1.603E+02, 1.651E+02, 1.701E+02, 1.751E+02, 1.801E+02,
     8     1.852E+02, 1.905E+02, 1.959E+02, 2.014E+02, 2.068E+02,
     9     2.122E+02, 2.124E+02, 2.051E+02, 1.977E+02, 1.903E+02,
     $     1.828E+02, 1.756E+02, 1.711E+02, 1.675E+02, 1.640E+02/
      DATA (SO2CRS(I),I= 5501, 5562)/
     1     1.604E+02, 1.568E+02, 1.522E+02, 1.461E+02, 1.400E+02,
     2     1.339E+02, 1.278E+02, 1.222E+02, 1.237E+02, 1.278E+02,
     3     1.319E+02, 1.360E+02, 1.400E+02, 1.437E+02, 1.469E+02,
     4     1.501E+02, 1.532E+02, 1.564E+02, 1.584E+02, 1.532E+02,
     5     1.468E+02, 1.404E+02, 1.340E+02, 1.276E+02, 1.255E+02,
     6     1.297E+02, 1.339E+02, 1.381E+02, 1.423E+02, 1.466E+02,
     7     1.515E+02, 1.564E+02, 1.614E+02, 1.663E+02, 1.712E+02,
     8     1.759E+02, 1.803E+02, 1.846E+02, 1.889E+02, 1.932E+02,
     9     1.974E+02, 1.993E+02, 2.005E+02, 2.018E+02, 2.030E+02,
     $     2.042E+02, 2.065E+02, 2.103E+02, 2.142E+02, 2.181E+02,
     1     2.220E+02, 2.255E+02, 2.246E+02, 2.222E+02, 2.197E+02,
     2     2.172E+02, 2.148E+02, 2.097E+02, 2.007E+02, 1.917E+02,
     3     1.827E+02, 1.736E+02/
      if (v .lt. vbeg .or. v .gt. vend) then
         cross = 0.0
      else if (v .eq. vend) then
c        This check prevents n from exceeding nmax in the algorithm below.
         cross = so2crs(nmax)
      else
         xi = (v-vbeg)/vincr+1.
         n = xi+1.+1.E-6
         xd = xi-float(n)
         cross = so2crs(n)+xd*(so2crs(n)-so2crs(n-1))
      endif   
      return
      end
      SUBROUTINE SOURCE(VV,ISOURC,IDAY,ANGLE,SS)                        sorc 100
      COMMON /ICLL/ ICALL,FPHS,FALB,FORBIT                              sorc 110
C     SUBROUITNE SOURCE CONTAINS THE SOLAR INTENSITY DATA AS A          sorc 120
C     FUNTION OF WAVELENGTH. THIS ROUTINE IS ALSO CAPABLE OF CALCULATINGsorc 130
C     LUNAR INTENSITY BASED ON THE PHASE ANGLE BETWEEN THE SUN, MOON ANDsorc 140
C     EARTH. CORRECTIONS ARE MADE FOR THE SUN'S ELLIPTIC ORBIT.         sorc 150
C                                                                       sorc 160
      DIMENSION NDAY(13),RAT(13),PHS(17),ALB(29)                        sorc 170
      DATA NDAY/1,32,60,91,121,152,182,213,244,274,305,335,366/         sorc 180
      DATA RAT/1.034,1.030,1.019,1.001,.985,.972,.967,.971,.982,        sorc 190
     1    .998,1.015,1.029,1.034/                                       sorc 200
      DATA PHS/100.,73.2,57.8,42.3,32.0,23.3,16.7,12.4,8.7,6.7,         sorc 210
     1    4.7,3.6,2.4,1.2,0.9,0.4,.002/                                 sorc 220
      DATA ALB/.001,.01,.03,.075,.1,.13,.155,.17,.178,.185,.2,.211,     sorc 230
     1    .231,.25,.275,.289,.285,.287,.3,.29,.3,.31,.313,.319,.329     sorc 240
     1    ,.339,.345,.350,.4/                                           sorc 250
      IF(VV.LE.0.) THEN                                                 sorc 260
           V = 1.0E+38                                                  sorc 270
      ELSE                                                              sorc 280
           V=10000./VV                                                  sorc 290
      ENDIF                                                             sorc 300
      IF(ISOURC.NE.1) GO TO 50                                          sorc 310
      IF(ICALL.EQ.1) GO TO 20                                           sorc 320
C                                                                       sorc 330
C     MOON PHASE ANGLE FACTOR                                           sorc 340
C                                                                       sorc 350
      FPHS=0.0                                                          sorc 360
      IF(ANGLE.GT.160.) GO TO 20                                        sorc 370
      IP=ANGLE/10.                                                      sorc 380
      IF(FLOAT(IP*10).EQ.ANGLE) GO TO 10                                sorc 390
      FPHS=PHS(IP+1)+(ANGLE-10.*IP)*(PHS(IP+2)-PHS(IP+1))/10.           sorc 400
      GO TO 20                                                          sorc 410
10    FPHS=PHS(IP+1)                                                    sorc 420
C                                                                       sorc 430
C     GEOMETRICAL ALBEDO OF THE MOON                                    sorc 440
C                                                                       sorc 450
20    FALB=0.4                                                          sorc 460
      IF(V.GE.5.) GO TO 40                                              sorc 470
      IF(V.GT.2.8) GO TO 30                                             sorc 480
      I1=V*10                                                           sorc 490
      FALB=ALB(I1)+(ALB(I1+1)-ALB(I1))*(V-I1*0.1)*10.                   sorc 500
      GO TO 40                                                          sorc 510
30    FALB=ALB(28)+(ALB(29)-ALB(28))*(V-2.8)/2.2                        sorc 520
40    CONTINUE                                                          sorc 530
C                                                                       sorc 540
C     SUN ELLIPTIC ORBIT FACTOR                                         sorc 550
C                                                                       sorc 560
50    IF(ICALL.EQ.1) GO TO 90                                           sorc 570
      FORBIT=0.0                                                        sorc 580
      IF(IDAY.GT.0 .AND. IDAY.LT.367) GO TO 55                          sorc 590
      FORBIT = 1.0                                                      sorc 600
      GO TO 90                                                          sorc 610
55    CONTINUE                                                          sorc 620
      DO 60 I=1,13                                                      sorc 630
      IF(NDAY(I).EQ.IDAY) GO TO 80                                      sorc 640
      IF(NDAY(I).GT.IDAY) GO TO 70                                      sorc 650
60    CONTINUE                                                          sorc 660
70    FORBIT=RAT(I-1)+(IDAY-NDAY(I-1))*(RAT(I)-RAT(I-1))/(NDAY(I)       sorc 670
     1   -NDAY(I-1))                                                    sorc 680
      GO TO 90                                                          sorc 690
80    FORBIT=RAT(I)                                                     sorc 700
90    CONTINUE                                                          sorc 710
      ICALL=1                                                           sorc 720
C                                                                       sorc 730
C     SOLAR INTENSITY                                                   sorc 740
C                                                                       sorc 750
      SS = SUN(VV)*FORBIT                                               sorc 760
C     CONVERT W/M-2-MICRON TO W/CM-2-MICRON                             sorc 770
      IF(ISOURC. EQ. 1) SS = SS * FPHS * FALB * 2.04472E-7              sorc 780
      SS=SS*.0001                                                       sorc 790
      RETURN                                                            sorc 800
      END                                                               sorc 810
      SUBROUTINE SSGEO(IERROR,IPH,IPARM,PARM1,PARM2,PARM3,PARM4,PSIPO,G,
C    X MAXGEO)                                                          
     1  MAXGEO,MSOFF)                                                   
C                                                                       
C     THIS ROUTINE DRIVES THE LOWTRAN GEOMETRY ROUTINES REPEATEDLY      
C     TO OBTAIN THE ABSORBER AMOUNTS FROM THE SCATTERING POINTS ON      
C     THE OPTICAL PATH TO THE EXTRATERRESTRIAL SOURCE, NECESSARY        
C     TO DO THE LAYER BY LAYER SINGLE SCATTERING RADIANCE CALCULATION.  
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

C
C     TRANS VARIABLES
C
      INCLUDE 'parameter.list'
      COMMON /MDATAX/ WMOLXT(MMOLX,laydim)                                  
      COMMON /MODELX/ DNSTYX(MMOLX,LAYDIM)                                  
      COMMON /NONAME/ TXX(MMOLX), WX(MMOLX), WPATHX(laythr,MMOLX)
      COMMON /SOLSX/  WPTHSX(laythr,MMOLX),TBBYSX(laythr,MMOLX),
     $     PATMSX(laythr,MMOLX)
c
c
c
      COMMON RELHUM(LAYDIM),HSTOR(LAYDIM),ICH(4),VH(17),TX(65),W(65)  
      COMMON IMSMX,WPATH(LAYTHR,65),TBBY(LAYTHR),PATM(LAYTHR),NSPEC,   
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX2(47),AWCCON(5)  
      COMMON/SOLS/AH1(LAYTWO),ARH(LAYTWO),WPATHS(LAYTHR,65),
     1 PA(LAYTWO),PR(LAYTWO),ATHETA(LAYDIM+1),ADBETA(LAYDIM+1),
     2 LJ(LAYTWO+1),JTURN,ANGSUN,CSZEN(LAYTWO),TBBYS(LAYTHR,12),
     3 PATMS(LAYTHR,12)
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON /CARD1/ MODEL,ITYPE,IEMSCT,M1,M2,M3,IM,NOPRNT,TBOUND,SALB  
     1  ,MODTRN                                                         
      LOGICAL MODTRN                                                    
      COMMON /CARD2/ IHAZE,ISEASN,IVULCN,ICSTL,ICLD,IVSA,VIS,WSS,WHH,   
     1    RAINRT                                                        
      COMMON /CARD3/ H1,H2,ANGLE,RANGE,BETA,REE,LEN                     
C     COMMON /CARD4/ V1,V2,DV                                           
      COMMON /CNTRL/ KMAX,MM,IKMAX,NL,ML,IKLO,ISSGEO,IMULT              
      COMMON /MODEL/ ZM(LAYDIM),PM(LAYDIM),TM(LAYDIM),RFNDX(LAYDIM),
     1  DENSTY(65,LAYDIM),CLDAMT(LAYDIM),RRAMT(LAYDIM),EQLWC(LAYDIM),
     1  HAZEC(LAYDIM)
      COMMON /PARMTR/ RE,DELTAS,ZMAX,IMAX,IMOD,IBMAX,IPATH              
      COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     
      COMMON /RFRPTH/ ZP(LAYDIM+1),PP(LAYDIM+1),TP(LAYDIM+1),
     $     rfndxp(LAYDIM+1),SP(LAYDIM+1),
     $     ppsum(LAYDIM+1),tpsum(LAYDIM+1),RHOPsm(LAYDIM+1),
     $     DENP(65,LAYDIM+1),AMTP(65,LAYDIM+1)
      COMMON /USRDTA/ NANGLS,ANGF(50),F(4,50)                           
      DIMENSION WPDUM(LAYTWO,65),TBDUM(LAYTWO),LJD(LAYTWO),
     x AZ(LAYDIM+1),RHD(LAYDIM+1),PDUM(LAYTWO),WDUM(65)
c
      dimension wpdumx(laythr,mmolx),wdumx(mmolx)
c
C     MOLECULAR AND HENYEY-GREENSTEIN PHASE FUNCTIONS                   
C     NOTE; UNITS ARE (STER-1), X=COS(SCATTERING ANGLE)                 
      PFMOL(X)=.06050402+.0572197*X**2                                  
      PFHG(GG,X)=(1.0-GG**2)/(4.*PI*(1.0+GG**2-2.0*GG*X)**1.5)          
      IKLO=1                                                            
      msoffx = 0                                                        
C     NPR = 1                                                           
      NPR = 2                                                           
      ISSGEO=1                                                          
C     SPECIFY THE GEOMETRICAL CONFIGURATION                             
      IF(IPARM.EQ.2) GO TO 1                                            
      THETAO=PARM1                                                      
      PHIO=PARM2                                                        
      THETAS=PARM3                                                      
      PHIS=PARM4                                                        
      GO TO 2                                                           
1     CONTINUE                                                          
      PSIO=PARM1                                                        
      DELO=PARM2                                                        
2     IF(IPARM.NE.0) GO TO 5                                            
      IF(ABS(THETAO).LT.89.5) GO TO 5                                   
      IF(THETAO.GT.0.0) GO TO 4                                         
C     OBSERVER IS AT OR NEAR THE SOUTH POLE, REMAP TO EQUATOR           
      WRITE(IPR,960)                                                    
      PSIPO=PSIPO-PHIS                                                  
      THETAO=0.0                                                        
      PHIO=0.0                                                          
      THETAS=0.0                                                        
      PHIS=90.+THETAS                                                   
      GO TO 5                                                           
4     CONTINUE                                                          
C     OBSERVER IS AT OR NEAR THE NORTH POLE, REMAP TO EQUATOR           
      WRITE(IPR,961)                                                    
      PSIPO=PHIS-PSIPO                                                  
      THETAO=0.0                                                        
      PHIO=0.0                                                          
      THETAS=0.0                                                        
      PHIS=90.-THETAS                                                   
5     CONTINUE                                                          
      WRITE(IPR,900)                                                    
C                                                                       
C     SAVE OPTICAL PATH PARAMETERS AND AMOUNTS                          
C                                                                       
      JTURND=JTURN                                                      
      IKMAXD=IKMAX+1                                                    
      H1D=H1                                                            
      H2D=H2                                                            
      ANGD=ANGLE                                                        
      RNGD=RANGE                                                        
      BETD=BETA                                                         
      BETA=0.                                                           
      LEND=LEN                                                          
      ITD=ITYPE                                                         
      IMAX=ML                                                           
      DO 10 J=1,IMAX                                                    
      AZ(J)=ZP(J)                                                       
10    RHD(J)=RELHUM(J)                                                  
      DO 11 J=1,IKMAXD                                                  
C     TBDUM(J)=TBBY(J)                                                  
      MSOFFJ=MSOFF+J                                                    
      TBDUM(J)=TBBY(MSOFFJ)                                             
      PDUM(J)=PATM(MSOFFJ)                                              
      LJD(J)=LJ(J)                                                      
      IF(LJD(J).GT.IMAX)LJD(J)=IMAX                                     
      DO 11 K=1,KMAX+2                                                    
C  11 WPDUM(J,K)=WPATH(J,K)                                             
   11 WPDUM(J,K)=WPATH(MSOFFJ,K)                                        
      DO 12 K=1,KMAX+2                                                    
   12 WDUM(K)=W(K)                                                      
c
      DO 21 KX=1,NSPECX                                                  cfc
         WPDUMX(J,KX)=WPATHX(MSOFFJ,KX)                                  cfc 
         WDUMX(KX)=WX(KX)                                                cfc
 21   CONTINUE                                                           cfc
c
      IMAX=IMAX-1                                                       
C                                                                       
C     ESTABLISH PSIO AND DELO                                           
      IARBO=0                                                           
      IF(ANGLE.LT.0.01.OR.ANGLE.GT.179.99) IARBO=1                      
C          
cjv 11/95 Lex fix. Replace PSIDEL with SAMM subroutine PSIECA to fix problems 
cjv	with the angle geometry between the sun and observer.
c                                                             
cjv      IF(IPARM.NE.2)                                                    
cjv     1CALL PSIDEL(THETAS,PHIS,THETAO,PHIO,PSIPO,PSIO,DELO,IARBO)        
cjv C     INITIAL CONDITIONS AT THE OBSERVER                                
cjv      IARB=IARBO                                                        
cjv      BETAST=0.0                                                        
cjv      IF(IARBO.EQ.0) THEN                                               
cjv         PSIST=PSIO                                                     
cjv      ELSE                                                              
cjv         PSIST = 0                                                      
cjv      ENDIF                                                             
c
c
      IARB=IARBO
      BETAST=0.0
      IF(IPARM.NE.2)then
          CALL PSIECA(THETAO,PHIO,THETAS,PHIS,PSIPOS,DELO)
          PSIO=PSIPOS-PSIPO
          IF(PSIO.GT.180.)PSIO=PSIO-360.
          IF(PSIO.LT.-180.)PSIO=PSIO+360.
      ENDIF
      PSIST=PSIO
c
cjv ^^^^^
      ANGL0=DELO                                                        
C                                                                       
C     LOOP OVER THE POINT TO SUN PATHS TO OBTAIN AMOUNTS                
C                                                                       
      WRITE(IPR,950)                                                    
      WRITE(IPR,952)                                                    
950   FORMAT(' SCTTR SCTTR SUBTENDED  SOLAR   PATH  RELATIVE  SCTTR   MO
     1LECULAR  ')                                                       
952   FORMAT(' POINT  ALT    ANGLE   ZENITH  ZENITH AZIMUTH   ANGLE   PH
     1ASE F    '/)                                                      
      DO 130 L=IKLO,IKMAXD                                              
      IF(LEND.EQ.1.OR.JTURND.NE.0) GO TO 20                             
C     SHORT PATH, UP                                                    
      H1=AZ(L)                                                          
      RELH=RHD(L)                                                       
      THTST=ATHETA(L)                                                   
      IF(L.GE.2) BETAST=BETAST+ADBETA(L-1)                              
      GO TO 30                                                          
20    CONTINUE                                                          
C     LONG PATH, OR SHORT PATH DOWN                                     
      IF(L.GE.2) BETAST=BETAST+ADBETA(LJD(L-1))                         
      IF(L.GE.JTURND) GO TO 25                                          
      LJP1=LJD(L)+1                                                     
      H1=AZ(LJP1)                                                       
      RELH=RHD(LJP1)                                                    
      THTST=180.-ATHETA(LJP1)                                           
      GO TO 30                                                          
25    LJDL=LJD(L)                                                       
      H1=AZ(LJDL)                                                       
      RELH=RHD(LJDL)                                                    
      THTST=ATHETA(LJDL)                                                
      IF(L.EQ.JTURND)THTST=180.-ATHETA(LJDL)                            
30    CONTINUE                                                          
      AH1(L)=H1                                                         
      ARH(L)=RELH                                                       
      IF(L.LT.2) GO TO 35                                               
      PSIST=PSI(PSIO,DELO,BETAST,IARB,IARBO)                            
      ANGL0=DEL(PSIO,DELO,BETAST,IARBO)                                 
clex35CORR=0.0                                                          
   35 angerr=0.                                                         
      angmx=angl0                                                       
      bendng=0.                                                         
      angle=angl0                                                       
C     RANGE=UNKNOWN                                                     
      ITYPE=3                                                           
clex  DO 90 JITER=1,4                                                   
      DO 90 JITER=1,12                                                  
      H2 = 0.0                                                          
clex  ANGLE=ANGL0-CORR                                                  
      LEN=0                                                             
      IF(ANGLE.LE.90.0) GO TO 40                                        
C
      IF (ANGLE .LE. 90.0001) THEN                              
C        CORRECTION FOR MULTIPLE SCATTERING FOR SOLAR PATHS.
C        FROM POINTS ON THE PATH TO THE SUN.
C        OTHERWISE, HA = HB AND THERE ARE PROBLEMS IN DPFILL.
         ANGLE = 90.
         GO TO 40
      ENDIF
C
      LEN=1                                                             
      WRITE(IPR,955) L                                                  
955   FORMAT('  SUN PATH ',I3,' PASSES THROUGH A TANGENT HEIGHT')       
40    CONTINUE                                                          
      HTOP=ZMAX                                                         
      IF(H1.LT.HTOP.OR.LEN.EQ.1) GO TO 60                               
C     SCATTERING POINT IS AT OR ABOVE HTOP AND LEN=0,                   
C     SET W(K)=0.0 AND CONTINUE                                         
      DO 50 K=1,KMAX+2                                                    
50    W(K)=0.0 
c
      DO 55 KX=1,NSPECX                                                 CFC
         WX(Kx)=0.0                                                     CFC
 55   CONTINUE                                                          CFC
c                                                         
      GO TO 100                                                         
C                                                                       
C  60 CALL GEO(IERROR,BENDNG,MAXGEO)                                    
   60 angsav=angle                                                      
      CALL GEO(IERROR,BENDNG,MAXGEO,MSOFFX)                             
      angle=angsav                                                      
C                                                                       
C     IERROR=-5 IF SCATTERING POINT IS IN THE SHADE, ALSO SET W(K)=-5.0 
      IF(IERROR.NE.-5) GO TO 80                                         
      WRITE(IPR,970) L                                                  
970   FORMAT('  SCATTERING POINT ',I3,' IS IN THE SHADE')               
      DO 70 K=1,KMAX+2                                                    
70    W(K)=-5.0
c
      DO 75 Kx=1, nspecx                                                 cfc
         Wx(Kx)=-5.0                                                     cfc
 75   continue                                                           cfc
c
      IERROR=0                                                          
      GO TO 100                                                         
  80  CONTINUE                                                          
Clex  SOLAR ZENITH BENDING CORRECTION                                   
clex  IF(JITER.GT.1) WRITE(IPR,917) CORR                                
clex  IF(ABS(CORR-BENDNG).LT..1) GO TO 100                              
clex90CORR=BENDNG                                                       
c     solar zenith error                                                
      angerr=angle+bendng-angl0                                         
      if(jiter.gt.1)write(ipr,'(2a,f12.5,a)')'  THE SOLAR ZENITH',      
     1  ' EXITING THE ATMOSPHERE IS IN ERROR BY',angerr,'DEG'           
      if(abs(angerr).lt..001)goto100                                    
c     calculate angle using the bisection method.                       
c     angl0 is a maximum and angl0 minus its bending is a minimum.      
      if(jiter.eq.1)then                                                
          angmn=angl0-bendng                                            
          angle=angmn                                                   
      else                                                              
          if(angerr.gt.0)then                                           
              angmx=angle                                               
          else                                                          
              angmn=angle                                               
          endif                                                         
          angle=.5*(angmx+angmn)                                        
      endif                                                             
   90 continue                                                          
100   CONTINUE                                                          
      SANGLE=SCTANG(ANGLE,THTST,PSIST,IARB)                             
      COSANG=COS(CA*SANGLE)                                             
C     LOAD MOLECULAR PHASE FUNCTION ARRAY                               
      PR(L)=PFMOL(COSANG)                                               
C     LOAD AEROSOL PHASE FUNCTION ARRAY                                 
C     HENYEY-GREENSTEIN                                                 
      IF(IPH.NE.0) GO TO 105                                            
      PA(L)=PFHG(G,COSANG)                                              
      GO TO 115                                                         
105   CONTINUE                                                          
      IF(IPH.NE.1) GO TO 110                                            
C     USER SUPPLIED PHASE FUNCTION                                      
C     DETERMINE ALTITUDE AND ANGLE INDICES                              
      M=4                                                               
      IF(H1.LE.30.) M=3                                                 
      IF(H1.LE.10.) M=2                                                 
      IF(H1.LE.2.) M=1                                                  
      DO 106 LL=1,NANGLS                                                
C                                                                       
C    INTERPOLATION CORRECTION FOR ANGLE = TO ANGLES READ IN             
C                                                                       
      IF(ANGF(LL).EQ.SANGLE) THEN                                       
      PA(L)=F(M,LL)                                                     
      GO TO 115                                                         
      END IF                                                            
      IF(ANGF(LL).GT.SANGLE) GO TO 107                                  
106   CONTINUE                                                          
107   LP1=LL                                                            
      LL=LL-1                                                           
      CALL INTERP(2,SANGLE,ANGF(LL),ANGF(LP1),PA(L),F(M,LL),F(M,LP1))   
      GO TO 115                                                         
110   CONTINUE                                                          
C     V DEPENDENT MIE DATA BASE, SAVE SCATTERING ANGLE INSTEAD          
      PA(L)=SANGLE                                                      
115   CONTINUE                                                          
C     LOAD AMOUNTS FROM W(K) INTO WPATHS(L,K)                           
      MSOFFL=MSOFF+L                                                    
      DO 120 K=1,KMAX+2                                                   
C 120 WPATHS(L,K)=W(K)                                                  
  120 WPATHS(MSOFFL,K)=W(K)                                             
c
      DO 125 KX=1,NSPECX                                                 CFC
         WPTHSX(MSOFFL,KX)=WX(KX)                                        CFC
 125  CONTINUE                                                           cfc
c
C         WHEN THE MODERATE RESOLUTION OPTION IS USED, A CURTIS-GODSON  
C         AVERAGE PRESSURE, PATMS, AND TEMPERATURE, TBBYS, IS DEFINED   
C         FOR THE SCATTERING POINT TO EXTRATERRESTRIAL SOURCE "LAYER"   
          IF(MODTRN)THEN                                                
              DO 126 K=1,NSPECT                                          
                  PNUM=0.                                               
                  TNUM=0.                                               
                  DEN=0.       
                 if (k .le. nspc) then                                     
                    KP=KPOINT(K)                                          
                    DO 123 IK=1,IKMAX                                     
                       MSOFFK=MSOFFX+IK                                  
                       WPTH=WPATH(MSOFFK,KP)                             
                       IF(WPTH.LE.0)GOTO123                              
                       PNUM=PNUM+PATM(MSOFFK)*WPTH                       
                       TNUM=TNUM+TBBY(MSOFFK)*WPTH                       
                       DEN=DEN+WPTH                                      
 123                CONTINUE                       
                 else
                    DO 124 IK=1,IKMAX                                    cfc
                       MSOFFK=MSOFFX+IK                                  cfc
                       WPTH=WPATHx(MSOFFK,K-nspc)                       cfc
                       IF(WPTH.LE.0)GOTO124                              cfc
                       PNUM=PNUM+PATM(MSOFFK)*WPTH                       cfc
                       TNUM=TNUM+TBBY(MSOFFK)*WPTH                       cfc
                       DEN=DEN+WPTH                                      cfc
 124                CONTINUE                                             cfc
                 endif
                 if ( k .le. nspc) then                       
                    IF(DEN.NE. 0)PATMS(MSOFFL,K)=PNUM/DEN                     
                    IF(DEN.NE. 0)TBBYS(MSOFFL,K)=TNUM/DEN                     
                 else
                    IF(DEN.NE. 0)PATMSx(MSOFFL,K-nspc)=PNUM/DEN         
                    IF(DEN.NE. 0)TBBYSx(MSOFFL,K-nspc)=TNUM/DEN             
                 endif
 126          continue
           ENDIF                                                         
C     REVERSE SIGN CONVENTION (TO + E OF N) FOR PRINTED OUTPUT          
      PSIST2=-PSIST                                                     
C                                                                       
C    CSZEN IS COSINE OF SOLAR ZENTIH FOR EACH LAYER                     
C                                                                       
      CSZEN(L)=COS(ANGLE*CA)                                            
      WRITE(IPR,951)L,H1,BETAST,ANGLE,THTST,PSIST2,SANGLE,PR(L)         
951   FORMAT(1X,I3,6(1X,F7.2), (1X,E10.3))                              
130   CONTINUE                                                          
C                                                                       
C     RESTORE OPTICAL PATH AMOUNTS                                      
C                                                                       
      IKMAX=IKMAXD-1                                                    
      H1=H1D                                                            
      H2=H2D                                                            
      ANGSUN=ANGLE                                                      
      ANGLE=ANGD                                                        
      RANGE=RNGD                                                        
      BETA=BETD                                                         
      LEN=LEND                                                          
      ITYPE=ITD                                                         
      DO 160 J=1,IKMAXD                                                 
C     TBBY(J)=TBDUM(J)                                                  
      MSOFFJ=MSOFF+J                                                    
      TBBY(MSOFFJ)=TBDUM(J)                                             
      PATM(MSOFFJ)=PDUM(J)                                              
      LJ(J)=LJD(J)                                                      
      DO 160 K=1,KMAX+2                                                   
C 160 WPATH(J,K)=WPDUM(J,K)                                             
  160 WPATH(MSOFFJ,K)=WPDUM(J,K)                                        
      DO 170 K=1,KMAX+2                                                   
  170 W(K)=WDUM(K)                                                      
c
      DO 175 KX=1,NSPECX                                                 cfc
         WPATHX(MSOFFJ,KX)=WPDUMX(J,KX)                                  cfc  
         WX(KX)=WDUMX(KX)                                                cfc 
 175  CONTINUE                                                           cfc
c
      NPR = NOPRNT                                                      
C                                                                       
C     FORMATS                                                           
C                                                                       
900   FORMAT(2X,//,' SINGLE SCATTERING POINT TO SOURCE PATHS ')         
917   FORMAT('  SOLAR ZENITH CORRECTION FOR BENDING = ',F10.3)          
920   FORMAT(2X,'*** CUMMULATIVE POINT-TO-SOURCE AMOUNTS ***')          
925   FORMAT(/,2X,'L                 WPATHS(L,K) K=1,7')                
930   FORMAT(1X,I2,7(2X,E10.3))                                         
931   FORMAT(8E10.3)                                                    
940   FORMAT(2X,'L                 WPATHS(L,K) K=8,15')                 
960   FORMAT(2X,'THETAO < 89.5, OBSERVER ASSUMED TO BE AT THE SOUTH     
     1POLE, PROBLEM HAS BEEN REMAPPED TO THE EQUATOR')                  
961   FORMAT(2X,'THETAO > 89.5, OBSERVER ASSUMED TO BE AT THE NORTH     
     1POLE, PROBLEM HAS BEEN REMAPPED TO THE EQUATOR')                  
      RETURN                                                            
      END                                                               
      SUBROUTINE SSRAD(IPH,IK,IPATH,V,SUMSSR)
C     
c     modified by lex berk in October, 1993
c
C     SSRAD PERFORMS THE LAYER BY LAYER SINGLE SCATTERING RADIANCE SUM.
c
c     INPUTS
c       IPH    SCATTERING PHASE FUNCTION SWITCH
c              =2 for LOWTRAN Mie functions
c       IK     LAYER INDEX
c       IPATH  PATH TYPE SWITCH
c              =1 for direct sun to observer path only
c              =2 for sun to scattering point to observer L-shaped paths
c              =3 for observer to scattering point line-of-sight paths
c       V      SPECTRAL FREQUENCY (CM-1)
c
c     OUTPUTS
c       SUMSSR   LAYER BY LAYER SINGLE SCATTERING RADIANCE SUM
c
      INCLUDE 'parameter.list'
      COMMON RELHUM(LAYDIM),HSTOR(LAYDIM),ICH(4),VH(17),TX(65),W(65)  
      COMMON IMSMX,WPATH(LAYTHR,65),TBBY(LAYTHR),PATM(LAYTHR),NSPEC,   
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX2(47),AWCCON(5)  
      COMMON/SOLS/AH1(LAYTWO),ARH(LAYTWO),WPATHS(LAYTHR,65),
     1 PA(LAYTWO),PR(LAYTWO),ATHETA(LAYDIM+1),ADBETA(LAYDIM+1),
     2 LJ(LAYTWO+1),JTURN,ANGSUN,CSZEN(LAYTWO),TBBYS(LAYTHR,12),
     3 PATMS(LAYTHR,12)
      COMMON/SRAD/TEB1,TEB2SV
      save pmol1,paer1,teb2,pmol2,paer2,tx15,tx14s,tx14
      goto(10,20,30),ipath
C
C     IPATH=1.  Initialize the SINGLE SCATTERING RADIANCE SUM.
   10 SUMSSR=0.
C
C     No optical depth along the line-of-sight for ipath=1
      tx15=0.
C
C     Store the direct sun to observer path total transmittance, tx(9),
c     and total cumulative optical depth, tx(14).
      TEB2=tx(9)
      tx14s=tx(14)
C
C     Store Rayleigh AND AEROSOL PHASE FUNCTIONS at the observer.
      PMOL2=PR(IK)
      PAER2=PA(IK)
      IF(IPH.EQ.2)CALL PHASEF(V,AH1(IK),PA(IK),ARH(IK),PAER2)
      RETURN
C
C     IPATH=2.  Store the CURRENT L PATH total TRANSMITTANCE, tx(9),
c     and total cumulative optical depth, tx(14).
   20 TEB1=tx(9)
      tx14=tx(14)
C
C     Store the Rayleigh AND AEROSOL PHASE FUNCTIONS at the current
c     scattering point.
      ikp1=IK+1
      PMOL1=PR(IKp1)
      PAER1=PA(IKp1)
      IF(IPH.EQ.2)CALL PHASEF(V,AH1(ikp1),PA(ikp1),ARH(ikp1),PAER1)
      RETURN
C
C     IPATH=3.  For the current layer, calculate the change in total
c     optical depth for the L-shaped paths, dtx14, and the incremental
c     Rayleigh scattering optical depth, dtx15, along the line-of-sight
   30 dtx15=tx(15)-tx15
      dtx14=tx14-tx14s
c
c                  1
c                  /              z
c     coef = teb2  |  (teb1/teb2)    dz
c                  /
c                  0
      coef=teb2
c
cjv 9/7/95 A change from Lex. "If the change in optical depth is less than .00001, 
cjv	the L-path transmittances are constant for all practical purposes."
c
cjv      if(dtx14.ne.0.)coef=(teb2-teb1)/dtx14
      if(abs(dtx14).gt..00001)coef=(teb2-teb1)/dtx14
cjv ^
c
c     Calculate the single scatter radiance assuming the scattering and
c     absorption scale heights for the layer are all approximately equal
      sumssr=sumssr+coef*.5*(tx(2)*(paer1+paer2)+dtx15*(pmol1+pmol2))
c
c     Save
c       tx(15) = current cumulative Rayleigh scattering optical depth
c       tx14   = current L-path total optical depth
c       paer1  = current aerosol scattering phase function
c       pmol1  = current Rayleigh scattering phase function
c       teb2   = old L-path total transmittance (output to TAPE7)
c       teb1   = current L-path total transmittance
      tx15=tx(15)
      tx14s=tx14
      PAER2=PAER1
      PMOL2=PMOL1
      teb2sv=teb2
      TEB2=TEB1
      RETURN
      END
      SUBROUTINE SUBSOL(THETAS,PHIS,TIME,IDAY)                          subs 100
C                                                                       subs 110
C     SUBROUTINE SUBSOL CALCULATES THE SUBSOLAR POINT ANGLES            subs 120
C     THETAS AND PHIS BASED UPON IDAY AND TIME. SINCE EACH              subs 130
C     YEAR IS 365.25 DAYS LONG THE EXACT VALUE OF THE DECLINATION       subs 140
C     ANGLE CHANGES FROM YEAR TO YEAR.  FOR PRECISE VALUES CONSULT      subs 150
C     ' THE AMERICAN EPHEMERIS AND NAUTICAL ALMANAC' PUBLISHED YEARLY   subs 160
C     BY THE U.S. GOVT. PRINTING OFFICE.  ALSO, THE SOLAR POSITION      subs 170
C     IS CHARACTERIZED BY 25 POINTS BELOW; THIS SHOULD PREDICT THE SUBSOsubs 180
C     ANGLES WITHIN ONE DEGREE.  FOR INCREASED ACCURACY ADD MORE DATA   subs 190
C     POINTS                                                            subs 200
C                                                                       subs 210
C     THE EQUATION OF TIME, EQT, IS IN MINUTES                          subs 220
C     THE DECLINATION ANGLE, DEC IS IN DEGREES                          subs 230
C                                                                       subs 240
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      DIMENSION NDAY(25),EQT(25),DEC(25)                                subs 280
      DATA NDAY /1,9,21,32,44,60,91,121,141,152,160,172,182,            subs 290
     1 190,202,213,244,274,305,309,325,335,343,355,366/                 subs 300
      DATA DEC /-23.07,-22.22,-20.08,-17.32,-13.62,-7.88,4.23,          subs 310
     1 14.83, 20.03,21.95,22.87,23.45,23.17,22.47,20.63,18.23,8.58,     subs 320
     2 -2.88,-14.18,-15.45,-19.75,-21.68,-22.75,-23.43,-23.07/          subs 330
      DATA EQT /-3.23,-6.83,-11.17,-13.57,-14.33,-12.63,-4.2,           subs 340
     1 2.83,3.57,2.45,1.10,-1.42,-3.52,-4.93,-6.25,-6.28,-0.25,         subs 350
     2 10.02,16.35,16.38,14.3,11.27,8.02,2.32,-3.23/                    subs 360
      IF(IDAY.LT.1.OR.IDAY.GT.366)  GO TO 900                           subs 370
      IF(TIME.LT.0.0.OR.TIME.GT.24.0)  GO TO 910                        subs 380
      DO 10 I=1,25                                                      subs 390
      IF(NDAY(I).EQ.IDAY) GO TO 30                                      subs 400
10    IF(NDAY(I).GT.IDAY) GO TO 20                                      subs 410
20    I=I-1                                                             subs 420
      EQTIME=EQT(I)+(EQT(I+1)-EQT(I))*(IDAY-NDAY(I))/(NDAY(I+1)-NDAY(I))subs 430
      DECANG=DEC(I)+(DEC(I+1)-DEC(I))*(IDAY-NDAY(I))/(NDAY(I+1)-NDAY(I))subs 440
      GO TO 40                                                          subs 450
30    EQTIME=EQT(I)                                                     subs 460
      DECANG=DEC(I)                                                     subs 470
40    THETAS=DECANG                                                     subs 480
      EQTIME=EQTIME/60.0                                                subs 490
      PHIS=15.0*(TIME+EQTIME)-180.0                                     subs 500
      IF(PHIS.LT.0.0) PHIS=PHIS+360.0                                   subs 510
      RETURN                                                            subs 520
900   WRITE(IPR,901) IDAY                                               subs 530
901   FORMAT(' FROM SUBSOL - IDAY OUT OF RANGE, IDAY=',I6)              subs 540
      STOP                                                              subs 550
910   WRITE(IPR,902) TIME                                               subs 560
902   FORMAT(' FROM SUBSOL - TIME OUT OF RANGE, TIME=',E12.5)           subs 570
      STOP                                                              subs 580
      END                                                               subs 590
      SUBROUTINE SVSOLA(IPARM,IPH,IDAY,ISOURC,PARM1,PARM2,PARM3,PARM4,  sola 100
     $     TIME,PSIPO,ANGLEM,                                           sola 110
     $     ISAVE1,ISAVE2,ISAVE3,ISAVE4,SAVE1,SAVE2,SAVE3,SAVE4,         sola 120
     $     SAVE5,SAVE6,SAVE7)                                           sola 130
C                                                                       sola 140
C     SAVE THE CURRENT SOLAR PARAMETERS                                 sola 150
C                                                                       sola 160
C     IMPLICIT UNDEFINED(A-Z)                                           sola 170
      INTEGER IPARM,IDAY,ISOURC,ISAVE1,ISAVE2,ISAVE3,IPH,ISAVE4         sola 180
      REAL PARM1,PARM2,PARM3,PARM4,TIME,PSIPO,ANGLEM,                   sola 190
     $     SAVE1,SAVE2,SAVE3,SAVE4,SAVE5,SAVE6,SAVE7                    sola 200
      ISAVE1 = IPARM                                                    sola 210
      ISAVE2 = IPH                                                      sola 220
      ISAVE3 = IDAY                                                     sola 230
      ISAVE4 = ISOURC                                                   sola 240
      SAVE1 = PARM1                                                     sola 250
      SAVE2 = PARM2                                                     sola 260
      SAVE3 = PARM3                                                     sola 270
      SAVE4 = PARM4                                                     sola 280
      SAVE5 = TIME                                                      sola 290
      SAVE6 = PSIPO                                                     sola 300
      SAVE7 = ANGLEM                                                    sola 310
      RETURN                                                            sola 320
      END                                                               sola 330
      FUNCTION   TAB(I1,J1,K1,WC)                                       ftab 100
C********************************************************************** ftab 110
C                                                                       ftab 120
C          THE INTERNAL DATA:                                           ftab 130
C                                                                       ftab 140
      DIMENSION A(9,6,9),ALPHA(9,6,9),A1(5),A2(5),ALPHA1(5),            ftab 150
     *    MAXI(6,9)                                                     ftab 160
C                                                                       ftab 170
C          A(1,J,K),J=1,3 = POWER LAW COEFFICIENT FOR THE ABSORPTION    ftab 180
C            COEFICIENT FOR THE MARSHALL-PALMER WATER DROP SIZE         ftab 190
C            DISTRIBUTION FOR TEMPERATURE=10.*(J-2) AND FREQUENCY=FR(K) ftab 200
C          A(2,J,K),J=1,3 = THE SAME FOR THE EXTINCTION COEFFICIENT     ftab 210
C          A(I,J,K),J=1,3,I=3,9 = THE SAME FOR THE LEGENDRE             ftab 220
C                                      COEFFICIENT #I-2                 ftab 230
C          A(I,4,K),I=1,9 = THE SAME AS A(I,2,K), BUT FOR ICE           ftab 240
C                             (NO TEMPERATURE DEPENDENCE)               ftab 250
C          A(I,5,K),I=1,9 = THE SAME AS A(I,2,K), BUT FOR THE BEST DROP ftab 260
C            SIZE DISTRIBUTION (NO TEMPRATURE DEPENDENCE)               ftab 270
C          A(I,6,K),I=1,9 = THE SAME AS A(I,5,K), BUT FOR ICE           ftab 280
C          ALPHA(I,J,K) = THE POWER EXPONENET CORRESPONDING TO A(I,J,K) ftab 290
C          MAXI(J,K): TAB(I,J,K,WC)=0. IF I.GT.MAXI(J,K)                ftab 300
C          A1, A2 AND ALPHA1 = THE POWER-LINEAR LAW COEFFICIENTS AND    ftab 310
C                 EXPONENT FOR THE EXCEPTIONAL CASES                    ftab 320
C                                                                       ftab 330
C          THE FORMULA:                                                 ftab 340
C                                                                       ftab 350
C          SC=A*WC**ALPHA IF ABS(A).GT.10.**-8,                         ftab 360
C          SC=A1*WC**ALPHA1+A2*WC IF ABS(A).LE.10.**-8,                 ftab 370
C                    A1, A2 AND ALPHA1 ARE INDEXED BY INT(ALPHA)        ftab 380
C                                                                       ftab 390
C          THE BLOCK-DATA SECTION                                       ftab 400
C                                                                       ftab 410
      DATA ((MAXI(J,K),J=1,6),K=1,9)/4*6,14*7,36*9/                     ftab 420
      DATA (A1(I),A2(I),ALPHA1(I),I=1,5)/.611,-.807,1.18,.655,-.772,1.08ftab 430
     * ,.958,-1.,.99,.538,-.696,1.27,1.58,-1.50,1.02/                   ftab 440
      DATA ((A(I,J,1),J=1,6),I=1,7)/.284,.285,.294,.001336,.36,.00146,  ftab 450
     *.363,.365,.375,.0148,.528,.0317,3*0.,.3147,0.,.438,               ftab 460
     *.4908,.487,.482,.528,.478,.538,3*.0350,.0470,.0482,.0647,         ftab 470
     *.002,.00205,.00208,.00285,.0037,.0048,4*0.,.00021,.00016/         ftab 480
      DATA ((ALPHA(I,J,1),J=1,6),I=1,7)/1.214,1.233,1.25,1.035,1.22,    ftab 490
     *1.076,1.291,1.31,1.323,1.63,1.334,1.74,3.1,2.1,1.1,5.005,4.1,.555,ftab 500
     *-.009,-.013,-.016,.028,-.019,.031,.398,.399,.4,.473,.461,.525,    ftab 510
     *1.06,.97,1.03,1.03,1.18,1.16,4*0.,1.3,1.3/                        ftab 520
      DATA ((A(I,J,2),J=1,6),I=1,7)/.8,.77,.73,.00344,.76,.0043,        ftab 530
     *1.28,1.27,1.24,.162,1.43,.332,.254,.172,0.,.93,.32,1.29,          ftab 540
     *.5,.486,.4706,.69,.481,.8,.0965,.0936,.09,.159,.151,.234,         ftab 550
     *.0234,.0228,.0221,.034,.057,.065,2*.0037,.0035,.005,.011,.0106/   ftab 560
      DATA ((ALPHA(I,J,2),J=1,6),I=1,7)/2*1.1,1.09,1.13,1.02,1.19,      ftab 570
     *2*1.20,1.15,1.66,1.14,1.7,.29,.42,5.1,.39,.66,.44,                ftab 580
     *0.,-.01,-.0199,.12,-.01,.17,.386,.378,.2,.48,.485,.56,            ftab 590
     *.92,.91,.90,.97,1.15,1.13,1.32,1.26,1.32,1.41,1.69,1.67/          ftab 600
      DATA ((A(I,J,3),J=1,6),I=1,7)/1.11,1.07,1.02,.0059,.92,.00775,    ftab 610
     *1.88,1.89,1.87,.43,1.80,.77,.512,.425,.336,1.25,.677,1.55,        ftab 620
     *.561,.534,.506,.867,.6,1.07,.175,.165,.156,.300,.292,.49,         ftab 630
     *.066,.064,.061,.105,.16,.22,                                      ftab 640
     *.0169,.0162,.0156,.023,.055,.056/                                 ftab 650
      DATA ((ALPHA(I,J,3),J=1,6),I=1,7)/2*1.01,1.,1.18,.92,1.23,        ftab 660
     *3*1.1,1.58,1.,1.57,.264,.320,.445,.27,.416,.27,                   ftab 670
     *.048,.033,.018,.168,.09,.224,.429,.417,.402,.501,.528,.62,        ftab 680
     *2*.83,.82,.9,1.01,1.11,1.22,1.21,1.2,1.23,1.51,1.53/              ftab 690
      DATA ((A(I,J,4),J=1,6),I=1,9)/1.51,1.49,1.44,.0163,1.12,.0194,    ftab 700
     *2.73,2.77,2.79,1.61,2.18,1.9,1.14,1.054,.961,1.57,1.36,1.66,      ftab 710
     *.99,.93,.87,1.31,1.33,1.63,.594,.557,.516,.77,1.02,1.16,          ftab 720
     *.352,.334,.315,.43,.73,.8,.171,.163,.154,.18,.47,.43,             ftab 730
     *.084,.081,.077,.106,.29,.32,.037,.036,.034,.029,.16,.11/          ftab 740
      DATA ((ALPHA(I,J,4),J=1,6),I=1,9)/.87,.86,.85,1.181,.79,1.16,     ftab 750
     *.93,.92,.91,1.3,.84,1.18,.188,.21,.24,.09,.21,.06,                ftab 760
     *2*.2,.19,.175,.275,.2,2*.461,.459,.39,.51,.41,                    ftab 770
     *2*.66,.65,.58,.70,.64,2*.94,.93,.84,1.03,1.01,                    ftab 780
     *3*1.22,1.09,1.37,1.4,1.58,1.56,1.54,1.5,1.8,1.9/                  ftab 790
      DATA ((A(I,J,5),J=1,6),I=1,9)/1.55,1.53,1.49,.0194,1.14,.0225,    ftab 800
     *2.82,2.87,2.90,1.91,2.22,2.,1.266,1.184,1.093,1.60,1.48,1.65,     ftab 810
     *1.13,1.07,1.,1.4,1.51,1.69,.74,.698,.649,.87,1.24,1.23,           ftab 820
     *.465,.444,.418,.52,.94,.91,.248,.238,.225,.24,.65,.53,            ftab 830
     *.132,.128,.122,.15,.43,.47,.065,.063,.06,.045,.26,.16/            ftab 840
      DATA ((ALPHA(I,J,5),J=1,6),I=1,9)/.85,.84,.83,1.168,.78,1.15,     ftab 850
     *.9,.89,.88,1.23,.82,1.11,.172,.191,.216,.071,.181,.04,            ftab 860
     *.222,.221,.22,.165,.274,.17,.452,.454,.456,.35,.48,.33,           ftab 870
     *.63,.68,.63,.52,.66,.55,3*.89,.76,.94,.86,                        ftab 880
     *1.14,1.13,1.12,.96,1.24,1.1,1.44,1.41,1.43,1.31,1.6,1.6/          ftab 890
      DATA ((A(I,J,6),J=1,6),I=1,9)/2*1.58,1.54,.0248,1.15,.0279,       ftab 900
     *2.94,2.97,3.,2.34,2.25,2.2,1.447,1.374,1.288,1.62,1.64,1.63,      ftab 910
     *1.37,1.31,1.234,1.52,1.8,1.77,1.,.96,.898,1.01,1.6,1.3,           ftab 920
     *.68,.66,.62,.66,1.31,1.07,.41,.4,.38,.33,.99,.66,                 ftab 930
     *.25,.24,.23,.23,.71,.56,.136,.133,.127,.081,.49,.26/              ftab 940
      DATA ((ALPHA(I,J,6),J=1,6),I=1,9)/.83,.81,.8,1.145,.762,1.120,    ftab 950
     *.87,.86,.85,1.14,.799,1.,.149,.165,.184,.046,.148,.014,           ftab 960
     *.232,.236,.238,.146,.255,.13,.428,.433,.438,.28,.44,.23,          ftab 970
     *3*.59,.44,.59,.43,3*.81,.64,.83,.66,                              ftab 980
     *1.02,2*1.01,.81,1.06,.89,2*1.25,1.24,1.07,1.36,1.3/               ftab 990
      DATA ((A(I,J,7),J=1,6),I=1,9)/1.60,1.59,1.56,.0285,1.16,.0314,    ftab1000
     *2.98,3.02,3.05,2.6,2.26,2.3,1.546,1.481,1.4,1.63,1.72,1.62,       ftab1010
     *1.52,1.464,1.388,1.58,1.97,1.8,1.18,1.13,1.07,1.08,1.82,1.33,     ftab1020
     *.84,.82,.78,.75,1.55,1.16,.54,.53,.5,.4,1.22,.74,                 ftab1030
     *.34,.33,.32,.3,.93,.67,2*.2,.19,.112,.67,.33/                     ftab1040
      DATA ((ALPHA(I,J,7),J=1,6),I=1,9)/.81,.80,.788,1.132,.753,1.105,  ftab1050
     *.85,.84,.83,1.09,.788,.95,.136,.153,.167,.033,.131,.004,          ftab1060
     *.232,.236,.241,.133,.24,.11,.411,.416,.422,.25,.40,.19,           ftab1070
     *3*.56,.4,.55,.38,2*.77,.76,.58,.76,.56,                           ftab1080
     *3*.95,.74,.97,.78,1.17,2*1.16,.98,1.23,1.11/                      ftab1090
      DATA ((A(I,J,8),J=1,6),I=1,9)/2*1.60,1.58,.045,1.15,.0461,        ftab1100
     *3.08,3.09,3.1,3.3,2.27,2.32,1.849,1.81,1.75,1.628,1.98,1.606,     ftab1110
     *2.07,2.04,1.98,1.78,2.5,1.946,1.89,1.86,1.81,1.30,2.6,1.508,      ftab1120
     *1.58,1.56,1.52,1.11,2.49,1.57,1.22,1.21,1.18,.68,2.2,1.11,        ftab1130
     *2*.91,.89,.61,2.,1.18,2*.65,.64,.299,1.6,.73/                     ftab1140
      DATA ((ALPHA(I,J,8),J=1,6),I=1,9)/.777,.764,.752,1.092,.729,1.057,ftab1150
     *.796,.79,.784,.96,.756,.81,.1,.108,.117,.004,.089,-.006,          ftab1160
     *.207,.210,.215,.093,.182,.075,2*.34,.35,.15,.30,.122,             ftab1170
     *3*.46,.3,.41,.28,3*.61,.42,.55,.394,                              ftab1180
     *3*.75,.56,.7,.55,2*.91,.9,.76,.87,.79/                            ftab1190
      DATA ((A(I,J,9),J=1,6),I=1,9)/2*1.58,1.56,.0587,1.13,.0579,       ftab1200
     *3.09,2*3.08,3.39,2.26,2.33,2.009,1.99,1.95,1.624,2.11,1.64,       ftab1210
     *2.43,2.42,2.38,1.902,2.80,2.078,2*2.42,2.38,1.454,3.09,1.7,       ftab1220
     *2*2.2,2.17,1.4,3.1,1.91,1.87,1.88,1.85,.94,3.,1.46,               ftab1230
     *2*1.54,1.52,.93,2.8,1.64,2*1.22,1.21,.53,2.5,1.17/                ftab1240
      DATA ((ALPHA(I,J,9),J=1,6),I=1,9)/.757,.746,.736,1.06,.717,1.024, ftab1250
     *.766,.764,.761,.86,.74,.763,.084,.087,.092,-.0018,.069,.007,      ftab1260
     *.183,.182,.184,.078,.148,.075,3*.29,.128,.24,.13,                 ftab1270
     *.4,2*.39,.264,.33,.256,2*.52,.51,.367,.44,.360,                   ftab1280
     *2*.63,.62,.49,.55,.47,.76,2*.75,.67,.67,.66/                      ftab1290
      i = i1                                                            ftab1300
      j = j1                                                            ftab1310
      k = k1                                                            ftab1320
C                                                                       ftab1330
C                                                                       ftab1340
      IF(I.GT.MAXI(J,K)) THEN                                           ftab1350
      TAB=0.                                                            ftab1360
      RETURN                                                            ftab1370
      END IF                                                            ftab1380
      IF(ABS(A(I,J,K)).GT.1.E-8) THEN                                   ftab1390
      TAB=A(I,J,K)*WC**ALPHA(I,J,K)                                     ftab1400
      ELSE                                                              ftab1410
      L=ALPHA(I,J,K)                                                    ftab1420
      TAB=A1(L)*WC**ALPHA1(L)+A2(L)*WC                                  ftab1430
      END IF                                                            ftab1440
      RETURN                                                            ftab1450
      END                                                               ftab1460
      SUBROUTINE TANHT(CPATH, HTAN, H1)                                 tanh 100
      include 'parameter.list'
C                                                                       tanh 110
C     THIS ROUTINE FINDS THE TANGENT HEIGHT GIVEN CPATH (THE PATH CONSTAtanh 120
C     SEE ATMOSPHERIC TRANSMITTANCE/RADIANCE:  LOWTRAN 6, AFGL-TR-83-018tanh 130
C                                                                       tanh 140
      INTEGER IMAX, IMOD, IBMAX,IPATH, J,JMAX,                          tanh 150
     $     KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT                         tanh 160
      REAL RE, DELTAS, ZMAX                                             tanh 170
      DOUBLE PRECISION CPATH,HTAN,RTBIS,H1,CP(laydim),R(laydim),        tanh 180
     $     X,RX,RATIO,X1,X2                                             tanh 190
C                                                                       tanh 200
      COMMON /PARMTR/ RE,DELTAS,ZMAX,IMAX,IMOD,IBMAX,IPATH              tanh 210
      COMMON /MODEL/ ZM(LAYDIM),PM(LAYDIM),TM(LAYDIM),RFNDX(LAYDIM),
     1  DENSTY(65,LAYDIM),CLDAMT(LAYDIM),RRAMT(LAYDIM),EQLWC(LAYDIM),
     1  HAZEC(LAYDIM)
      COMMON /CNTRL/ KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT               tanh 240
C                                                                       tanh 250
      DO 100 J = 1, ML                                                  tanh 260
         R(J) = RE+ZM(J)                                                tanh 270
         X = ZM(J)                                                      tanh 280
         CALL IRFXN(X, RX, RATIO)                                       tanh 290
         CP(J) = R(J)*RX                                                tanh 300
         JMAX = J + 1                                                   tanh 310
         IF (H1 .LE. ZM(J)) GO TO 150                                   tanh 320
 100  CONTINUE                                                          tanh 330
 150  CONTINUE                                                          tanh 340
      DO 200 J = JMAX, 1, -1                                            tanh 350
         IF (J .EQ. 1) THEN                                             tanh 360
            HTAN = 0.0D00                                               tanh 370
            RETURN                                                      tanh 380
         ELSE IF ((CPATH .LE. CP(J)) .AND. (CPATH .GE. CP(J-1))) THEN   tanh 390
            X1 = ZM(J-1)                                                tanh 400
            X2 = ZM(J)                                                  tanh 410
            HTAN = RTBIS(X1, X2,CPATH)                                  tanh 420
            RETURN                                                      tanh 430
         ENDIF                                                          tanh 440
 200  CONTINUE                                                          tanh 450
      END                                                               tanh 460
      BLOCK DATA TITLE                                                  btit 100
C>    BLOCK DATA                                                        btit 110
C     TITLE INFORMATION                                                 btit 120
      CHARACTER*4  HHAZE      ,HSEASN     ,HVULCN     ,BLANK,           btit 130
     X            HMET        ,HMODEL     ,HTRRAD                       btit 140
      COMMON /TITL/ HHAZE(5,16),HSEASN(5,2),HVULCN(5,8),BLANK,          btit 150
     X HMET(5,2),HMODEL(5,8),HTRRAD(6,4)                                btit 160
      COMMON /VSBD/ VSB(10)                                             btit 170
      DATA VSB /23.,5.,0.,23.,5.,50.,23.,0.2,0.5,0./                    btit 180
      DATA BLANK/'    '/                                                btit 190
      DATA HHAZE /                                                      btit 200
     1 'RURA','L   ','    ','    ','    ',                              btit 210
     2 'RURA','L   ','    ','    ','    ',                              btit 220
     3 'NAVY',' MAR','ITIM','E   ','    ',                              btit 230
     4 'MARI','TIME','    ','    ','    ',                              btit 240
     5 'URBA','N   ','    ','    ','    ',                              btit 250
     6 'TROP','OSPH','ERIC','    ','    ',                              btit 260
     7 'USER',' DEF','INED','    ','    ',                              btit 270
     8 'FOG1',' (AD','VECT','TION',')   ',                              btit 280
     9 'FOG2','(RAD','IATI','0N) ','    ',                              btit 290
     X 'DESE','RT A','EROS','OL  ','    ',                              btit 300
     A 'BACK','GROU','ND S','TRAT','O   ',                              btit 310
     B 'AGED',' VOL','CANI','C   ','    ',                              btit 320
     C 'FRES','H VO','LCAN','IC  ','    ',                              btit 330
     D 'AGED',' VOL','CANI','C   ','    ',                              btit 340
     E 'FRES','H VO','LCAN','IC  ','    ',                              btit 350
     F 'METE','ORIC',' DUS','T   ','    '/                              btit 360
      DATA HSEASN /                                                     btit 370
     1 'SPRI','NG-S','UMME','R   ','    ',                              btit 380
     2 'FALL','-WIN','TER ','    ','    ' /                             btit 390
      DATA HVULCN /                                                     btit 400
     1 'BACK','GROU','ND S','TRAT','O   ',                              btit 410
     2 'MODE','RATE',' VOL','CANI','C   ',                              btit 420
     3 'HIGH','    ',' VOL','CANI','C   ',                              btit 430
     4 'HIGH','    ',' VOL','CANI','C   ',                              btit 440
     5 'MODE','RATE',' VOL','CANI','C   ',                              btit 450
     6 'MODE','RATE',' VOL','CANI','C   ',                              btit 460
     7 'HIGH','    ',' VOL','CANI','C   ',                              btit 470
     8 'EXTR','EME ',' VOL','CANI','C   '/                              btit 480
      DATA HMET/                                                        btit 490
     1 'NORM','AL  ','    ','    ','    ',                              btit 500
     2 'TRAN','SITI','ON  ','    ','    '/                              btit 510
      DATA HMODEL /                                                     btit 520
     1 'TROP','ICAL',' MOD','EL  ','    ',                              btit 530
     2 'MIDL','ATIT','UDE ','SUMM','ER  ',                              btit 540
     3 'MIDL','ATIT','UDE ','WINT','ER  ',                              btit 550
     4 'SUBA','RCTI','C   ','SUMM','ER  ',                              btit 560
     5 'SUBA','RCTI','C   ','WINT','ER  ',                              btit 570
     6 '1976',' U S',' STA','NDAR','D   ',                              btit 580
     7 '   ','    ','    ','    ','    ',                               btit 590
     8 'MODE','L =0','HORI','ZONT','AL  '/                              btit 600
      DATA HTRRAD/                                                      btit 610
     1 'TRAN','SMIT','TANC','E   ','    ','    ',                       btit 620
     2 'RADI','ANCE','    ','    ','    ','    ',                       btit 630
     3 'RADI','ANCE','+SOL','AR S','CATT','ERNG',                       btit 640
     4 'TRAN','SMIT','TED ','SOLA','R IR','RAD.'/                       btit 650
      END                                                               btit 660
      FUNCTION   TNRAIN(RR,V,TM,RADFLD)                                 tnrn 100
CCC                                                                     tnrn 110
      COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     tnrn 120
      COMMON /CARD3/ H1,H2,ANGLE,RANGE,BETA,RE,LEN                      tnrn 130
CCC   CALCULATES TRANSMISSION DUE TO RAIN AS A FUNCTION OF              tnrn 140
CCC   RR=RAIN RATE IN MM/HR                                             tnrn 150
CCC   OR WITHIN 350CM-1 USES THE MICROWAVE TABLE ROUTINE TO             tnrn 160
CCC   OBTAIN THE EXTINCTION DUE TO RAIN                                 tnrn 170
CCC   RANGE=SLANT RANGE KM                                              tnrn 180
CCC                                                                     tnrn 190
CCC   ASSUMES A MARSHALL-PALMER RAIN DROP SIZE DISTRIBUTION             tnrn 200
CCC   N(D)=NZERO*EXP(-A*D)                                              tnrn 210
CCC   NZERO=8.E3 (MM-1)  (M-3)                                          tnrn 220
CCC   A=41.*RR**(-0.21)                                                 tnrn 230
CCC   D=DROP DIAMETER (CM)                                              tnrn 240
CCC                                                                     tnrn 250
      REAL NZERO                                                        tnrn 260
      DATA NZERO /8000./                                                tnrn 270
CCC                                                                     tnrn 280
      A=41./RR**0.21                                                    tnrn 290
CCC                                                                     tnrn 300
      IF(RR.LE.0)TNRAIN=1.                                              tnrn 310
      IF(RR.LE.0)RETURN                                                 tnrn 320
CCC                                                                     tnrn 330
      IF(V.GE.350.0) THEN                                               tnrn 340
       TNRAIN=PI*NZERO/A**3                                             tnrn 350
      ELSE                                                              tnrn 360
       TNRAIN=GMRAIN(V,TM,RR)                                           tnrn 370
       TNRAIN = TNRAIN * RADFLD                                         tnrn 380
      END IF                                                            tnrn 390
      RETURN                                                            tnrn 400
      END                                                               tnrn 410
      SUBROUTINE VSA(IHAZE,VIS,CEILHT,DEPTH,ZINVHT,Z,RH,AHAZE,IH)       vsa  100
C                                                                       vsa  110
C     VERTICAL STRUCTURE ALGORITHM                                      vsa  120
C                                                                       vsa  130
C     FROM U.S. ARMY ATMOSPHERIC SCIENCES LAB                           vsa  140
C     WHITE SANDS MISSILE RANGE, NM                                     vsa  150
C                                                                       vsa  160
C     CREATES A PROFILE OF AEROSOL DENSITY NEAR THE GROUND,INCLUDING    vsa  170
C     CLOUDS AND FOG                                                    vsa  180
C                                                                       vsa  190
C     THESE PROFILES ARE AT 9 HEIGHTS BETWEEN 0 KM AND 2 KM             vsa  200
C                                                                       vsa  210
C                                                                       vsa  220
C  ***VISIBILITY IS ASSUMED TO BE THE SURFACE VISIBILITY***             vsa  230
C                                                                       vsa  240
C     IHAZE  = THE TYPE OF AEROSOL                                      vsa  250
C     VIS    = VISIBILITY IN KM AT THE SURFACE                          vsa  260
C     CEILHT = THE CLOUD CEILING HEIGHT IN KM                           vsa  270
C     DEPTH  = THE CLOUD/FOG DEPTH IN KM                                vsa  280
C     ZINVHT = THE HEIGHT OF INVERSION OR BOUNDARY LAYER IN KM          vsa  290
C                                                                       vsa  300
C     VARIABLES USED IN VSA                                             vsa  310
C                                                                       vsa  320
C     ZC     = CLOUD CEILING HEIGHT IN M                                vsa  330
C     ZT     = CLOUD DEPTH IN M                                         vsa  340
C     ZINV   = INVERSION HEIGHT IN M                                    vsa  350
C           SEE BELOW FOR MORE INFORMATION ABOUT ZC, ZT, AND ZINV       vsa  360
C     D      = INITIAL EXTINCTION AT THE SURFACE (D=3.912/VIS-0.012)    vsa  370
C     ZALGO  = THE DEPTH OF THE LAYER FOR THE ALGORITHM                 vsa  380
C                                                                       vsa  390
C     OUTPUT FROM VSA:                                                  vsa  400
C                                                                       vsa  410
C     Z      = HEIGHT IN KM                                             vsa  420
C     RH     = RELATIVE HUMIDITY AT HEIGHT Z IN PERCENT                 vsa  430
C     AHAZE  = EXTINCTION AT HEIGHT Z IN KM**-1                         vsa  440
C     IH     = AEROSAL TYPE FOR HEIGHT Z                                vsa  450
C     HMAX   = MAXIMUM HEIGHT IN KM USED IN VSA, NOT NECESSARILY 2.0 KM vsa  460
C                                                                       vsa  470
C                                                                       vsa  480
C     THE SLANT PATH CALCULATION USES THE FOLLOWING FUNCTION:           vsa  490
C                                                                       vsa  500
C                 EXT55=A*EXP(B*EXP(C*Z))                               vsa  510
C                                                                       vsa  520
C     WHERE 'Z' IS THE HEIGHT IN KILOMETERS,                            vsa  530
C           'A' IS A FUNCTION OF EXT55 AT Z=0.0 AND IS ALWAYS POSITIVE, vsa  540
C           'B' AND 'C' ARE FUNCTIONS OF CLOUD CONDITIONS AND SURFACE   vsa  550
C               VISIBILITY (EITHER A OR B CAN BE POSITIVE OR NEGATIVE), vsa  560
C           'EXT55' IS THE VISIBILE EXTINCTION COEFFICIENT IN KM**-1.   vsa  570
C                                                                       vsa  580
C     THEREFORE, THERE ARE 4 CASES DEPENDING ON THE SIGNS OF 'B' AND 'C'vsa  590
C     CEILHT AND ZINVHT ARE USED AS SWITCHES TO DETERMINE WHICH CASE    vsa  600
C     TO USE.  THE SURFACE EXTINCTION 'D' IS CALCULATED FROM THE        vsa  610
C     VISIBILITY USING  D=3.912/VIS-0.012 AS FOLLOWS-                   vsa  620
C                                                                       vsa  630
C         CASE=1  FOG/CLOUD CONDITIONS                                  vsa  640
C                 'B' LT 0.0, 'C' LT 0.0                                vsa  650
C                 'D' GE 7.0   KM**-1                                   vsa  660
C                 FOR A CLOUD 7.    KM**-1 IS THE BOUNDARY VALUE AT     vsa  670
C                 THE CLOUD BASE AND 'Z' IS THE VERTICAL DISTANCE       vsa  680
C                 INTO THE CLOUD.                                       vsa  690
C                 VARIABLE USED:   DEPTH                                vsa  700
C                 ** DEFAULT:  DEPTH OF FOG/CLOUD IS 0.2 KM WHEN        vsa  710
C                              'DEPTH' IS 0.0                           vsa  720
C                                                                       vsa  730
C             =2  CLOUD CEILING PRESENT                                 vsa  740
C                 'B' GT 0.0, 'C' GT 0.0                                vsa  750
C                 VARIABLE USED:   CEILHT (MUST BE GE 0.0)              vsa  760
C                 ** DEFAULTS:  CASE 2 - CEILHT IS CALCULATED FROM      vsa  770
C                               SURFACE EXTINCTION                      vsa  780
C                                                                       vsa  790
C             =3  RADIATION FOG OR INVERSION OR BOUNDARY LAYER PRESENT  vsa  800
C                 'B' LT 0.0, 'C' GT 0.0                                vsa  810
C                 VIS LE 2.0 KM DEFAULTS TO A RADIATION FOG AT THE      vsa  820
C                     GROUND AND OVERRIDES INPUT BOUNDARY AEROSOL TYPE  vsa  830
C                 VIS GT 2.0 KM FOR AN INVERSION OR BOUNDARY LAYER      vsa  840
C                     WITH INPUT BOUNDARY AEROSOL TYPE                  vsa  850
C                 ** IHAZE=9 (RADIATION FOG) ALWAYS DEFAULTS TO A       vsa  860
C                    RADIATION FOG NO MATTER WHAT THE VISIBILITY IS.    vsa  870
C                 SWITCH VARIABLE: CEILHT (MUST BE LT 0.0)              vsa  880
C                 VARIABLE USED:   ZINVHT (MUST BE GE 0.0)              vsa  890
C                 ** CEILHT MUST BE LT 0.0 FOR ZINVHT TO BE USED **     vsa  900
C                    HOWEVER, IF DEPTH IS GT 0.0 AND ZINVHT IS EQ 0.0,  vsa  910
C                    THE PROGRAM WILL SUBSTITUTE DEPTH FOR ZINVHT.      vsa  920
C                 ** DEFAULT:  FOR A RADIATION FOG ZINVHT IS 0.05 K     vsa  930
C                              FOR AN INVERSION LAYER ZINVHT IS 2.0 KM  vsa  940
C                                                                       vsa  950
C           NOTE: IF IHAZE = 9, BUT VIS GT 2.0 KM RECOMEND              vsa  960
C           THAT IHAZE DEFAULT TO RURAL AEROSOL                         vsa  970
C                                                                       vsa  980
C             =4  NO CLOUD CEILING, INVERSION LAYER, OR BOUNDARY        vsa  990
C                 LAYER PRESENT, I.E. CLEAR SKIES                       vsa 1000
C                 EXTINCTION PROFILE CONSTANT WITH HEIGHT A SHORT       vsa 1010
C                 DISTANCE ABOVE THE SURFACE                            vsa 1020
C                                                                       vsa 1030
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      DIMENSION Z(10),RH(10),AHAZE(10),IH(10)                           vsa 1050
      DIMENSION AA(2),CC(3),EE(4),A(2),B(2),C(2),FAC1(9),FAC2(9)        vsa 1060
      REAL KMTOM                                                        vsa 1070
      DATA AA/92.1,0.3981/,CC/-0.014,0.0125,-0.03 /,KMTOM/1000.0/       vsa 1080
C     THE LAST 3 VALUES OF EE BELOW ARE EXTINCTIONS FOR VISIBILITIES    vsa 1090
C     EQUAL TO 5.0, 23.0, AND 50.0 KM, RESPECTIVELY.                    vsa 1100
      DATA EE/7.0  ,0.7824,0.17009,0.012  /                             vsa 1110
      DATA FAC1/0.0,0.03,0.05,0.075,0.1,0.18,0.3,0.45,1.0/              vsa 1120
      DATA FAC2/0.0,0.03,0.1,0.18,0.3,0.45,0.6,0.78,1.0/                vsa 1130
      WRITE(IPR,599)                                                    vsa 1140
C                                                                       vsa 1150
C     UPPER LIMIT ON VERTICAL DISTANCE - 2 KM                           vsa 1160
      ZHIGH=2000.                                                       vsa 1170
      HMAX=ZHIGH                                                        vsa 1180
      IF(VIS.GT.0.0)GO TO 5                                             vsa 1190
C     DEFAULT FOR VISIBILITY DEPENDS ON THE VALUE OF IHAZE.             vsa 1200
      IF(IHAZE.EQ.8)VIS=0.2                                             vsa 1210
      IF(IHAZE.EQ.9)VIS=0.5                                             vsa 1220
      IF(IHAZE.EQ.2.OR.IHAZE.EQ.5)VIS=5.0                               vsa 1230
      IF(IHAZE.EQ.1.OR.IHAZE.EQ.4.OR.IHAZE.EQ.7)VIS=23.0                vsa 1240
      IF(IHAZE.EQ.6)VIS=50.0                                            vsa 1250
C     IF(IHAZE.EQ.3)VIS= OR IHAZE = 10 VIS IS DETERMINED ELSEWHERE      vsa 1260
   5  D=3.912/VIS-0.012                                                 vsa 1270
C                                                                       vsa 1280
      ZC=CEILHT*KMTOM                                                   vsa 1290
      ZT=DEPTH*KMTOM                                                    vsa 1300
      ZINV=ZINVHT*KMTOM                                                 vsa 1310
C     IHAZE=9 (RADIATION FOG) IS ALWAYS CALCULATED AS A RADIATION FOG.  vsa 1320
      IF(IHAZE.EQ.9)ZC=-1.0                                             vsa 1330
C     ALSO, CHECK TO SEE IF THE FOG DEPTH FOR A RADIATION FOG           vsa 1340
C     WAS INPUT TO DEPTH INSTEAD OF THE CORRECT VARIABLE ZINVHT.        vsa 1350
      IF(IHAZE.EQ.9.AND.ZT.GT.0.0.AND.ZINV.EQ.0.0)ZINV=ZT               vsa 1360
C                                                                       vsa 1370
C     'IC' DEFINES WHICH CASE TO USE.                                   vsa 1380
      IC=2                                                              vsa 1390
      IF(D.GE.EE(1).AND.ZC.GE.0.0)IC=1                                  vsa 1400
C                                                                       vsa 1410
      IF(ZC.LT.0.0.AND.IC.EQ.2)IC=3                                     vsa 1420
      IF(ZINV.LT.0.0.AND.IC.EQ.3)IC=4                                   vsa 1430
      K=1                                                               vsa 1440
      GO TO (10,20,40,50),IC                                            vsa 1450
C                                                                       vsa 1460
C     CASE 1:  DEPTH FOG/CLOUD; INCREASING EXTINCTION WITH HEIGHT FROM  vsa 1470
C              CLOUD/FOG BASE TO CLOUD/FOG TOP.                         vsa 1480
 10   CONTINUE                                                          vsa 1490
      IF(ZC.LT.HMAX.AND.IC.EQ.2)K=2                                     vsa 1500
C     IC=-1 WHEN A CLOUD IS PRESENT AND THE PATH GOES INTO IT.          vsa 1510
C     USE CASE 2 OR 2' BELOW CLOUD AND CASE 1 INSIDE IT.                vsa 1520
      IF(K.EQ.2)IC=(-1)                                                 vsa 1530
C     THE BASE OF THE CLOUD HAS AN EXTINCTION COEFFICIENT OF 7.0   KM-1.vsa 1540
      IF(K.EQ.2)D=EE(1)                                                 vsa 1550
      A(K)=AA(1)                                                        vsa 1560
C     IF THE SURFACE EXTINCTION IS GREATER THAN THE UPPER LIMIT OF 92.1 vsa 1570
C     KM**-1, RUN THE ALGORITHM WITH AN UPPER LIMIT OF 'D+10'.          vsa 1580
      IF(D.GE.AA(1))A(K)=D+10.0                                         vsa 1590
      C(K)=CC(1)                                                        vsa 1600
      IF(ZT.LE.0.0)WRITE(IPR  ,603)                                     vsa 1610
      IF(ZT.LE.0.0)WRITE(IPR  ,604)                                     vsa 1620
      IF(ZT.GT.0.0)WRITE(IPR  ,611)ZT                                   vsa 1630
C     IF THE DISTANCE FROM THE GROUND TO THE CLOUD/FOG TOP IS LESS      vsa 1640
C     THAN 2.0 KM, VSA WILL ONLY CALCULATE UP TO THE CLOUD TOP.         vsa 1650
      IF(ZT.LE.0.0)ZT=200.                                              vsa 1660
      HMAX=AMIN1(ZT+ZC,HMAX)                                            vsa 1670
      GO TO 60                                                          vsa 1680
C                                                                       vsa 1690
C     CASE 2:  CLEAR/HAZY/LIGHTLY FOGGY; INCREASING EXTINCTION WITH HEIGvsa 1700
C              UP TO THE CLOUD BASE.                                    vsa 1710
 20   A(K)=AA(2)                                                        vsa 1720
      E=EE(1)                                                           vsa 1730
      IF(ZC.EQ.0.0)WRITE(IPR  ,600)                                     vsa 1740
      IF(ZC.EQ.0.0)THEN                                                 vsa 1750
        EAK =  ALOG(E/A(K) )                                            vsa 1760
        DAK =  ALOG(D/A(K) )                                            vsa 1770
        ANUM = EAK / DAK                                                vsa 1780
        IF(ANUM . GT. 0) THEN                                           vsa 1790
              CEIL = ALOG(ANUM)/CC(2)                                   vsa 1800
         ELSE                                                           vsa 1810
               CEIL = 2000.                                             vsa 1820
          ENDIF                                                         vsa 1830
      ENDIF                                                             vsa 1840
CC    IF(ZC.EQ.0.0)CEIL=ALOG(ALOG(E/A(K))/(ALOG(D/A(K))))/CC(2)         vsa 1850
      IF(ZC.EQ.0.0)WRITE(IPR  ,602)CEIL                                 vsa 1860
      IF(ZC.GT.0.0)WRITE(IPR  ,610)ZC                                   vsa 1870
      IF(ZC.EQ.0.0)ZC=CEIL                                              vsa 1880
      F = (VIS * ZC/350.0)**2                                           vsa 1890
C                                                                       vsa 1900
C     F IS A SCALING FACTOR USED IN CASE 2                              vsa 1910
C                                                                       vsa 1920
      DF = D/F                                                          vsa 1930
      IF(DF .LT. 1.0E-5) THEN                                           vsa 1940
           A(K)=D - (D*D/(2.*F))                                        vsa 1950
      ELSE                                                              vsa 1960
           A(K) = F*(1.0 - EXP(-D/F))                                   vsa 1970
      ENDIF                                                             vsa 1980
C                                                                       vsa 1990
C     THE COEFFICIENT A IS RECALCULATED BASED UPON THE SCALING FACTOR   vsa 2000
C                                                                       vsa 2010
      GO TO 60                                                          vsa 2020
C                                                                       vsa 2030
C                                                                       vsa 2040
C     CASE 3:  NO CLOUD CEILING BUT A RADIATION FOG OR AN INVERSION     vsa 2050
C              OR BOUNDARY LAYER PRESENT; DECREASING EXTINCTION WITH    vsa 2060
C              HEIGHT UP TO THE HEIGHT OF THE FOG OR LAYER.             vsa 2070
 40   A(K)=D*1.1                                                        vsa 2080
      E=EE(3)                                                           vsa 2090
      IF(IHAZE.EQ.2.OR.IHAZE.EQ.5)E=EE(2)                               vsa 2100
      IF(IHAZE.EQ.6.OR.(VIS.GT.2.0.AND.IHAZE.NE.9))E=EE(4)              vsa 2110
      IF(E.GT.D)E=D*0.99999                                             vsa 2120
      IF(ZT.GT.0.0.AND.ZINV.EQ.0.0.AND.VIS.LE.2.0)ZINV=ZT               vsa 2130
      IF(ZINV.EQ.0.0.AND.VIS.GT.2.0.AND.IHAZE.NE.9)WRITE(IPR  ,601)     vsa 2140
      IF(ZINV.EQ.0.0.AND.(VIS.LE.2.0.OR.IHAZE.EQ.9))WRITE(IPR  ,605)    vsa 2150
      IF(ZINV.EQ.0.0.AND.(VIS.LE.2.0.OR.IHAZE.EQ.9))WRITE(IPR  ,604)    vsa 2160
      IF(ZINV.GT.0.0.AND.VIS.GT.2.0.AND.IHAZE.NE.9)WRITE(IPR  ,612)ZINV vsa 2170
      IF(ZINV.GT.0.0.AND.(VIS.LE.2.0.OR.IHAZE.EQ.9))WRITE(IPR,614)ZINV  vsa 2180
      IF(ZINV.EQ.0.0.AND.VIS.GT.2.0.AND.IHAZE.NE.9)ZINV=2000            vsa 2190
      IF(ZINV.EQ.0.0.AND.(VIS.LE.2.0.OR.IHAZE.EQ.9))ZINV= 50            vsa 2200
      HMAX=AMIN1(ZINV,HMAX)                                             vsa 2210
      ZC=0.0                                                            vsa 2220
      GO TO 60                                                          vsa 2230
C                                                                       vsa 2240
C     CASE 4:  NO CLOUD CEILING OR INVERSION LAYER;                     vsa 2250
C              CONSTANT EXTINCTION WITH HEIGHT.                         vsa 2260
C                                                                       vsa 2270
50     A(K) = EE(4)                                                     vsa 2280
       C(K) = CC(3)                                                     vsa 2290
C                                                                       vsa 2300
60               B(K)=ALOG(D/A(K))                                      vsa 2310
      IF(IC.EQ.2)C(K)=ALOG(ALOG(E/A(K))/B(K))/ZC                        vsa 2320
      IF(IC.EQ.3)C(K)=ALOG(ALOG(E/A(K))/B(K))/ZINV                      vsa 2330
      IF(ZC.LT.HMAX.AND.K.EQ.1.AND.IC.EQ.2)GO TO 10                     vsa 2340
      IF(IC.EQ.2)HMAX=AMIN1(ZC,HMAX)                                    vsa 2350
      ZALGO=HMAX                                                        vsa 2360
      IF(IC.LT.0)ZALGO=ZC                                               vsa 2370
      WRITE(IPR  ,619)                                                  vsa 2380
      IF(IC.LT.0)K=1                                                    vsa 2390
C                                                                       vsa 2400
      DO 70 I=1,9                                                       vsa 2410
      IF(IC.LT.0.AND.I.EQ.5)K=2                                         vsa 2420
      IF(IC.LT.0.AND.I.EQ.5)ZALGO=HMAX-ZC                               vsa 2430
      Z(I)=ZALGO*(1.0-FAC2(10-I))                                       vsa 2440
      IF(IC.EQ.1)Z(I)=ZALGO*FAC1(I)                                     vsa 2450
      IF(IC.EQ.4)Z(I)=ZALGO*FLOAT(I-1)/8.0                              vsa 2460
      IF(IC.LT.0.AND.I.LT.5)Z(I)=ZALGO*(1.0-FAC2(11-2*I))               vsa 2470
      IF(IC.LT.0.AND.I.GE.5)Z(I)=ZALGO*FAC1(2*I-9)                      vsa 2480
C     IF(IC.LT.0.AND.(I.EQ.7.OR.I.EQ.8))Z(I)=ZALGO*FAC1(2*I-10)         vsa 2490
                 AHAZE(I)=A(K)*EXP(B(K)*EXP(C(K)*Z(I)))                 vsa 2500
      IF(IC.LE.0.AND.I.GE.5)Z(I)=Z(I)+ZC                                vsa 2510
      Z(I)=Z(I)/KMTOM                                                   vsa 2520
      RH(I)=6.953*ALOG(AHAZE(I))+86.407                                 vsa 2530
      IF(AHAZE(I).GE.EE(1))RH(I)=100.0                                  vsa 2540
      VISIB=3.912/(AHAZE(I)+0.012)                                      vsa 2550
      IH(I)=IHAZE                                                       vsa 2560
C     IF A RADIATION FOG IS PRESENT (I.E. VIS<=2.0 KM AND IC=3),        vsa 2570
C     IH IS SET TO 9 FOR ALL LEVELS.                                    vsa 2580
      IF(VISIB.LE.2.0.AND.IC.EQ.3)IH(I)=9                               vsa 2590
C     FOR A DEPTH FOG/CLOUD CASE, IH=8 DENOTING AN ADVECTION FOG.       vsa 2600
      IF(IC.EQ.1.OR.(IC.LT.0.AND.I.GE.5))IH(I)=8                        vsa 2610
      WRITE(IPR  ,620)Z(I),RH(I),AHAZE(I),VISIB,IH(I)                   vsa 2620
   70 CONTINUE                                                          vsa 2630
      HMAX=HMAX/KMTOM                                                   vsa 2640
      RETURN                                                            vsa 2650
C                                                                       vsa 2660
599   FORMAT('0 VERTICAL STRUCTURE ALGORITHM (VSA) USED')               vsa 2670
600   FORMAT(1H ,50X,28HCLOUD CEILING HEIGHT UNKNOWN)                   vsa 2680
601   FORMAT(1H ,50X,42HINVERSION OR BOUNDARY LAYER HEIGHT UNKNOWN,/,   vsa 2690
     1  1H ,50X,39HVSA WILL USE A DEFAULT OF 2000.0 METERS,/)           vsa 2700
605   FORMAT(1H ,50X,27HRADIATION FOG DEPTH UNKNOWN)                    vsa 2710
619   FORMAT(5X,10HHEIGHT(KM),5X,7HR.H.(%),5X,16HEXTINCTION(KM-1),      vsa 2720
     1   5X,15HVIS(3.912/EXTN),5X,5HIHAZE,/)                            vsa 2730
620   FORMAT(7X,F7.4,7X,F5.1,8X,E12.4,11X,F7.4,10X,I2)                  vsa 2740
602   FORMAT(1H ,39X,35HVSA WILL USE A CALCULATED VALUE OF ,F7.1,       vsa 2750
     1       7H METERS,/)                                               vsa 2760
603   FORMAT(1H ,50X,19HCLOUD DEPTH UNKNOWN)                            vsa 2770
604   FORMAT(1H ,50X,38HVSA WILL USE A DEFAULT OF 200.0 METERS,/)       vsa 2780
610   FORMAT(1H ,50X,24HCLOUD CEILING HEIGHT IS ,F9.1,7H METERS,/)      vsa 2790
611   FORMAT(1H ,50X,15HCLOUD DEPTH IS ,F14.1,7H METERS,/)              vsa 2800
612   FORMAT(1H ,50X,38HINVERSION OR BOUNDARY LAYER HEIGHT IS ,F7.1,    vsa 2810
     1 7H METERS,/)                                                     vsa 2820
614   FORMAT(1H ,50X,26HDEPTH OF RADIATION FOG IS ,F7.1,7H METERS,/)    vsa 2830
613   FORMAT(1H ,50X,43HTHERE IS NO INVERSION OR BOUNDARY LAYER OR ,    vsa 2840
     1 13HCLOUD PRESENT,/)                                              vsa 2850
      END                                                               vsa 2860
      SUBROUTINE VSANSM(K,AHAZE,IHA1,ZNEW)                              vsam 100
      include 'parameter.list'
      COMMON RELHUM(laydim),whno3(laydim),ICH(4),VH(17),TX(65),W(65)  
      COMMON IMSMX,WPATH(laythr,65),TBBY(laythr),PATM(laythr),NSPEC,   
     x KPOINT(12),ABSC(5,47),EXTC(5,47),ASYM(5,47),VX2(47),AWCCON(5)  
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON /CNTRL/ KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT               vsam 160
      COMMON /CARD1/ MODEL,ITYPE,IEMSCT,M1,M2,M3,IM,NOPRNT,TBOUND,SALB  vsam 170
     1  ,MODTRN                                                         vsam 180
      LOGICAL MODTRN                                                    vsam 190
      COMMON /CARD1B/ JUNIT(15),WMOLI(12),WAIR1,JLOW                    vsam 200
      COMMON /CARD2/ IHAZE,ISEASN,IVULCN,ICSTL,ICIR,IVSA,VIS,WSS,WHH,   vsam 210
     1    RAINRT                                                        vsam 220
      COMMON /ZVSALY/ ZVSA(10),RHVSA(10),AHVSA(10),IHVSA(10)            vsam 230
      COMMON /NSINP/ ZMDL(40),PM(40),TM(40),WMDL(40,13)                 vsam 240
C                                                                       vsam 250
      COMMON /MDATA/P(laydim),T(laydim),WH(laydim),WCO2(laydim),
     x WO(laydim),WN2O(laydim),WCO(laydim),WCH4(laydim),WO2(laydim)
      COMMON /MDATA1/ WNO(laydim),WSO2(laydim),WNO2(laydim),
     x WNH3(laydim),WAIR(laydim)    
      DIMENSION WMOL(13)                                                vsam 300
C                                                                       vsam 310
C     OUTPUT COMMON MDATA AND MDATA1                                    vsam 320
C                                                                       vsam 330
C                                                                       vsam 340
C     MODEL 7 CODING                                                    vsam 350
C     OLD LAYERS  AEROSOL RETURNED                                      vsam 360
C     NEW LAYERS P,T,DP,AEROSOL                                         vsam 370
C                                                                       vsam 380
C                                                                       vsam 390
C                                                                       vsam 400
      JML=ML                                                            vsam 410
      J=1                                                               vsam 420
      KN=K                                                              vsam 430
110   IF(KN.GT.10)GO TO 140                                             vsam 440
      JL=J-1                                                            vsam 450
      IF(JL.LT.1)JL=1                                                   vsam 460
      JP=JL+1                                                           vsam 470
      JLS = JL                                                          vsam 480
      IF(ZVSA(KN).EQ.ZMDL  (JL))GO TO 140                               vsam 490
      JLS = JP                                                          vsam 500
      IF(ZVSA(KN).EQ.ZMDL  (JP))GO TO 140                               vsam 510
      IF(ZVSA(KN).GT.ZMDL  (JL).AND.ZVSA(KN).LT.ZMDL  (JP))GO TO 115    vsam 520
      IF(J. GE. JML) GO TO 115                                          vsam 530
      J = J + 1                                                         vsam 540
      GO TO 110                                                         vsam 550
115   ZNEW=ZVSA(KN)                                                     vsam 560
      DIF=ZMDL  (JP)-ZMDL  (JL)                                         vsam 570
      DZ=ZVSA(KN)-ZMDL  (JL)                                            vsam 580
      DLIN=DZ/DIF                                                       vsam 590
      P(K)  = (PM(JP)-PM(JL))*DLIN+PM(JL)                               vsam 600
      T(K)   =(TM(JP)-TM(JL))*DLIN+TM(JL)                               vsam 610
      DO 120 KM = 1,13                                                  vsam 620
      WMOL(KM)=(WMDL(JP,KM)-WMDL(JL,KM))*DLIN+WMDL(JL,KM)               vsam 630
120   CONTINUE                                                          vsam 640
      IHA1  =IHVSA(KN)                                                  vsam 650
      AHAZE  =AHVSA(KN)                                                 vsam 660
      FAC=(ZVSA(KN)-ZMDL  (JL))/DIF                                     vsam 670
      IF(PM(JP).GT.0.0.AND.PM(JL).GT.0.) THEN                           vsam 680
           P(K)  =PM(JL)*(PM(JP)/PM(JL))**FAC                           vsam 690
      ENDIF                                                             vsam 700
      IF(TM(JP).GT.0.0.AND.TM(JL).GT.0.) THEN                           vsam 710
           T(K)   =TM(JL)*(TM(JP)/TM(JL))**FAC                          vsam 720
      ENDIF                                                             vsam 730
      DO 130 KM = 1,13                                                  vsam 740
      IF(WMDL(JP,KM) .GT.0.0.AND.WMDL(JL,KM).GT.0.0) THEN               vsam 750
           WMOL(KM)=(WMDL(JL,KM)*(WMDL(JP,KM))/WMDL(JL,KM))**FAC        vsam 760
      ENDIF                                                             vsam 770
130   CONTINUE                                                          vsam 780
       WH(K)    = WMOL(1)                                               vsam 790
       WCO2(K)  = WMOL(2)                                               vsam 800
       WO(K)    = WMOL(3)                                               vsam 810
       WN2O(K)  = WMOL(4)                                               vsam 820
       WCO(K)   = WMOL(5)                                               vsam 830
       WCH4(K)  = WMOL(6)                                               vsam 840
       WO2(K)   = WMOL(7)                                               vsam 850
       WNO(K)   = WMOL(8)                                               vsam 860
       WSO2(K)  = WMOL(9)                                               vsam 870
       WNO2(K)  = WMOL(10)                                              vsam 880
       WNH3(K)  = WMOL(11)                                              vsam 890
       WHNO3(K) = WMOL(12)                                              vsam 900
       WAIR(K)  = WMOL(13)                                              vsam 910
      RETURN                                                            vsam 920
140   CONTINUE                                                          vsam 930
      J = JLS                                                           vsam 940
      IF(K.GT.10) THEN                                                  vsam 950
         J = K - 10 + JLOW                                              vsam 960
         IHA1  =0                                                       vsam 970
         AHAZE  =0.                                                     vsam 980
      ENDIF                                                             vsam 990
      ZNEW = ZMDL(J)                                                    vsam1000
      P(K)  =PM(J)                                                      vsam1010
      T(K) = TM(J)                                                      vsam1020
      DO 135 KM = 1,13                                                  vsam1030
      WMOL(KM)= WMDL(J,KM)                                              vsam1040
135   CONTINUE                                                          vsam1050
       WH(K)    = WMOL(1)                                               vsam1060
       WCO2(K)  = WMOL(2)                                               vsam1070
       WO(K)    = WMOL(3)                                               vsam1080
       WN2O(K)  = WMOL(4)                                               vsam1090
       WCO(K)   = WMOL(5)                                               vsam1100
       WCH4(K)  = WMOL(6)                                               vsam1110
       WO2(K)   = WMOL(7)                                               vsam1120
       WNO(K)   = WMOL(8)                                               vsam1130
       WSO2(K)  = WMOL(9)                                               vsam1140
       WNO2(K)  = WMOL(10)                                              vsam1150
       WNH3(K)  = WMOL(11)                                              vsam1160
       WHNO3(K) = WMOL(12)                                              vsam1170
       WAIR(K)  = WMOL(13)                                              vsam1180
      IF(KN.LE.9) IHA1  =IHVSA(KN)                                      vsam1190
      IF(KN.LE.9)AHAZE  =AHVSA(KN)                                      vsam1200
      RETURN                                                            vsam1210
      END                                                               vsam1220
      SUBROUTINE WATVAP(P,T)                                            watp 100
C*************************************************************          watp 110
C                                                                       watp 120
C        WRITTEN APR, 1985 TO ACCOMMODATE 'JCHAR' DEFINITIONS FOR       watp 130
C        UNIFORM DATA INPUT -                                           watp 140
C                                                                       watp 150
C     JCHAR    JUNIT                                                    watp 160
C                                                                       watp 170
C    " ",A       10    VOLUME MIXING RATIO (PPMV)                       watp 180
C        B       11    NUMBER DENSITY (CM-3)                            watp 190
C        C       12    MASS MIXING RATIO (GM(K)/KG(AIR))                watp 200
C        D       13    MASS DENSITY (GM M-3)                            watp 210
C        E       14    PARTIAL PRESSURE (MB)                            watp 220
C        F       15    DEW POINT TEMP (TD IN T(K)) - H2O ONLY           watp 230
C        G       16     "    "     "  (TD IN T(C)) - H2O ONLY           watp 240
C        H       17    RELATIVE HUMIDITY (RH IN PERCENT) - H2O ONLY     watp 250
C        I       18    AVAILABLE FOR USER DEFINITION                    watp 260
C        J       19    REQUEST DEFAULT TO SPECIFIED MODEL ATMOSPHERE    watp 270
C                                                                       watp 280
C     THIS SUBROUTINE COMPUTES THE WATERVAPOR NUMBER DENSITY (MOL CM-3) watp 290
C     GIVE HUMIDITY  # TD = DEW POINT TEMP(K,C), RH = RELATIVE          watp 300
C     (PERCENT), PPH2O = WATER VAPOR PARTIAL PRESSURE (MB), DENH2O =    watp 310
C     WATER VAPOR MASS DENSITY (GM M-3),AMSMIX = MASS MIXING RATIO      watp 320
C     (GM/KG).                                                          watp 330
C                     THE FUNCTION DENSAT FOR THE SATURATION            watp 340
C     WATER VAPOR DENSITY OVER WATER IS ACCURATE TO BETTER THAN 1       watp 350
C     PERCENT FROM -50 TO +50 DEG C. (SEE THE LOWTRAN3 OR 5 REPORT)     watp 360
C                                                                       watp 370
C       'JUNIT' GOVERNS CHOICE OF UNITS -                               watp 380
C                                                                       watp 390
C***********************************************************************watp 400
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON /CARD1B/ JUNITP,JUNITT,JUNIT1(13),WMOL1(12),WAIR,JLOW      watp 420
      COMMON /CONSTN/ PZERO,TZERO,AVOGAD,ALOSMT,GASCON,PLANK,BOLTZ,     watp 430
     1    CLIGHT,ADCON,ALZERO,AVMWT,AIRMWT,AMWT(35)                     watp 440
      DATA C1/18.9766/,C2/-14.9595/,C3/-2.43882/                        watp 450
      DATA XLOSCH/2.6868E19/                                            watp 460
      DENSAT(ATEMP) = ATEMP*B*EXP(C1+C2*ATEMP+C3*ATEMP**2)*1.0E-6       watp 470
C*****                                                                  watp 480
      RHOAIR = ALOSMT*(P/PZERO)*(TZERO/T)                               watp 490
      PSS = P/PZERO                                                     watp 500
      A = TZERO/T                                                       watp 510
      WAIR = XLOSCH * PSS * A                                           watp 520
      B = AVOGAD/AMWT(1)                                                watp 530
      R = AIRMWT/AMWT(1)                                                watp 540
      JUNIT = JUNIT1(1)                                                 watp 550
      WMOL  = WMOL1(1)                                                  watp 560
      IF(JUNIT.NE.10) GO TO 110                                         watp 570
C*****GIVEN VOL. MIXING RATIO                                           watp 580
CC    WMOL1(1)=WMOL*RHOAIR*1.E-6                                        watp 590
      GO TO 200                                                         watp 600
110   IF(JUNIT.NE.11) GO TO 120                                         watp 610
C*****GIVEN NUMBER DENSITY (CM-3)                                       watp 620
      WMOL1(1) = WMOL/(RHOAIR*1.E-6)                                    watp 630
      GO TO 200                                                         watp 640
120   CONTINUE                                                          watp 650
      IF(JUNIT.NE.12) GO TO 130                                         watp 660
C*****GIVEN MASS MIXING RATIO (GM KG-1)                                 watp 670
CC    WMOL1(1) = R*WMOL*1.0E-3*RHOAIR                                   watp 680
      WMOL1(1) = R*WMOL*1.0E+3                                          watp 690
      GO TO 200                                                         watp 700
130   CONTINUE                                                          watp 710
      IF(JUNIT.NE.13) GO TO 140                                         watp 720
C*****GIVEN MASS DENSITY (GM M-3)                                       watp 730
CC    WMOL1(1) = B*WMOL*1.0E-6                                          watp 740
      WMOL1(1) = B*WMOL /RHOAIR                                         watp 750
      GO TO 200                                                         watp 760
140   CONTINUE                                                          watp 770
      IF(JUNIT.NE.14) GO TO 150                                         watp 780
C*****GIVEN WATER VAPOR PARTIAL PRESSURE (MB)                           watp 790
CC    WMOL1(1) = ALOSMT*(WMOL/PZERO)*(TZERO/T)                          watp 800
      WTEM     = ALOSMT*(WMOL/PZERO)*(TZERO/T)                          watp 810
      WMOL1(1) = WTEM/(RHOAIR*1.E-6)                                    watp 820
      GO TO 200                                                         watp 830
150   CONTINUE                                                          watp 840
      IF(JUNIT.NE.15) GO TO 160                                         watp 850
C*****GIVEN DEWPOINT (DEG K)                                            watp 860
      ATD = TZERO/(WMOL)                                                watp 870
CC    WMOL1(1)= DENSAT(ATD)*(WMOL)/T                                    watp 880
      WTEM    = DENSAT(ATD)*(WMOL)/T                                    watp 890
      WMOL1(1) = WTEM/(RHOAIR*1.E-6)                                    watp 900
      GO TO 200                                                         watp 910
160   CONTINUE                                                          watp 920
      IF(JUNIT.NE.16) GO TO 170                                         watp 930
C*****GIVEN DEWPOINT (DEG C)                                            watp 940
      ATD = TZERO/(TZERO+WMOL)                                          watp 950
CC    WMOL1(1) = DENSAT(ATD)*(TZERO+WMOL)/T                             watp 960
      WTEM     = DENSAT(ATD)*(TZERO+WMOL)/T                             watp 970
      WMOL1(1) = WTEM/(RHOAIR*1.E-6)                                    watp 980
      GO TO 200                                                         watp 990
170   CONTINUE                                                          watp1000
      IF(JUNIT.NE.17) GO TO 199                                         watp1010
C*****GIVEN RELATIVE HUMIDITY (PERCENT)                                 watp1020
C     DENNUM = DENSAT(A)*(WMOL/100.0)/(1.0-(1.0-WMOL/100.0)*DENSAT(A)/  watp1030
C    1    RHOAIR)                                                       watp1040
CC    WMOL1(1) = DENSAT(A)*(WMOL/100.0)                                 watp1050
      WTEM     = DENSAT(A)*(WMOL/100.0)                                 watp1060
      WMOL1(1) = WTEM/(RHOAIR*1.E-6)                                    watp1070
      GO TO 200                                                         watp1080
 199   WRITE(IPR,951)JUNIT                                              watp1090
 951  FORMAT(/,'  **** ERROR IN WATVAP ****, JUNIT = ',I5)              watp1100
      STOP'JUNIT'                                                       watp1110
  200 CONTINUE                                                          watp1120
      WMOL1(1)=2.989E-23 *WMOL1(1) *WAIR                                watp1130
      DENST = DENSAT(A)                                                 watp1140
      DENNUM = WMOL1(1)                                                 watp1150
C     RHP = 100.0*(DENNUM/DENST)*((RHOAIR-DENST)/(RHOAIR-DENNUM))       watp1160
      RHP = 100.0*(DENNUM/DENST)                                        watp1170
   12 FORMAT(   8X,'RH = ',F6.2)                                        watp1180
      IF(RHP.LE.100.0) GO TO 230                                        watp1190
      WRITE(IPR,10) RHP                                                 watp1200
   10 FORMAT(/,' ********WARNING (FROM WATVAP) # RELATIVE HUMIDTY = ',  watp1210
     1    G10.3,' IS GREATER THAN 100 PERCENT')                         watp1220
  230 CONTINUE                                                          watp1230
      RETURN                                                            watp1240
      END                                                               watp1250
      BLOCK DATA WVBNRG                                                 wvbn 100
C>    BLOCK DATA                                                        wvbn 110
C     WAVENUMBER-LOW AND WAVENUMBER-HIGH SPECIFY A BAND REGION          wvbn 120
C     FOR A MOLECULAR ABSORBER.                                         wvbn 130
C     THE UNIT FOR WAVENUMBER IS 1/CM.                                  wvbn 140
C     -999 IS AN INDICATOR TO INDICATE THE END OF ABSORPTION BANDS      wvbn 150
C     FOR ANY SPECIFIC ABSORBER.                                        wvbn 160
      COMMON /WNLOHI/                                                   wvbn 170
     L   IWLH2O(15),IWLO3 ( 6),IWLCO2(11),IWLCO ( 4),IWLCH4( 5),        wvbn 180
     L   IWLN2O(12),IWLO2 ( 7),IWLNH3( 3),IWLNO ( 2),IWLNO2( 4),        wvbn 190
     L   IWLSO2( 5),                                                    wvbn 200
     H   IWHH2O(15),IWHO3 ( 6),IWHCO2(11),IWHCO ( 4),IWHCH4( 5),        wvbn 210
     H   IWHN2O(12),IWHO2 ( 7),IWHNH3( 3),IWHNO ( 2),IWHNO2( 4),        wvbn 220
     H   IWHSO2( 5)                                                     wvbn 230
C                                                                       wvbn 240
      DATA IWLH2O/   0,    350,   1005,   1645,   2535,   3425,   4315, wvbn 250
     L    6155,   8005,   9620,  11545,  13075,  14865,  16340,   -999/ wvbn 260
      DATA IWHH2O/ 345,   1000,   1640,   2530,   3420,   4310,   6150, wvbn 270
     H    8000,   9615,  11540,  13070,  14860,  16045,  17860,   -999/ wvbn 280
C                                                                       wvbn 290
      DATA IWLO3 /   0,    515,   1630,   2670,   2850,   -999/         wvbn 300
      DATA IWHO3 / 200,   1275,   2295,   2845,   3260,   -999/         wvbn 310
C                                                                       wvbn 320
      DATA IWLCO2/ 425,    840,   1805,   3070,   3760,   4530,   5905, wvbn 330
     L    7395,   8030,   9340,   -999/                                 wvbn 340
      DATA IWHCO2/ 835,   1440,   2855,   3755,   4065,   5380,   7025, wvbn 350
     H    7785,   8335,   9670,   -999/                                 wvbn 360
C                                                                       wvbn 370
      DATA IWLCO /   0,   1940,   4040,   -999/                         wvbn 380
      DATA IWHCO / 175,   2285,   4370,   -999/                         wvbn 390
C                                                                       wvbn 400
      DATA IWLCH4/1065,   2345,   4110,   5865,   -999/                 wvbn 410
      DATA IWHCH4/1775,   3230,   4690,   6135,   -999/                 wvbn 420
C                                                                       wvbn 430
      DATA IWLN2O/   0,    490,    865,   1065,   1545,   2090,   2705, wvbn 440
     L    3245,   4260,   4540,   4910,   -999/                         wvbn 450
      DATA IWHN2O/ 120,    775,    995,   1385,   2040,   2655,   2865, wvbn 460
     H    3925,   4470,   4785,   5165,   -999/                         wvbn 470
C                                                                       wvbn 480
      DATA IWLO2 /   0,   7650,   9235,  12850,  14300,  15695,   -999/ wvbn 490
      DATA IWHO2 / 265,   8080,   9490,  13220,  14600,  15955,   -999/ wvbn 500
C                                                                       wvbn 510
      DATA IWLNH3/   0,    390,   -999/                                 wvbn 520
      DATA IWHNH3/ 385,   2150,   -999/                                 wvbn 530
C                                                                       wvbn 540
      DATA IWLNO /1700,   -999/                                         wvbn 550
      DATA IWHNO /2005,   -999/                                         wvbn 560
C                                                                       wvbn 570
      DATA IWLNO2/ 580,   1515,   2800,   -999/                         wvbn 580
      DATA IWHNO2/ 925,   1695,   2970,   -999/                         wvbn 590
C                                                                       wvbn 600
      DATA IWLSO2/   0,    400,    950,   2415,   -999/                 wvbn 610
      DATA IWHSO2/ 185,    650,   1460,   2580,   -999/                 wvbn 620
      END                                                               wvbn 630
      BLOCK DATA XSECTB
      IMPLICIT DOUBLE PRECISION (V)                                     [ 300010     18400
C
C**   XSNAME=NAMES, ALIAS=ALIASES OF THE CROSS-SECTION MOLECULES
C
      CHARACTER*10 XSFILE,XSNAME,ALIAS
      COMMON /XSECTF/ XSFILE(6,5,35),XSNAME(35),ALIAS(4,35) 
      COMMON /XSECTR/ V1FX(5,35),V2FX(5,35),DVFX(5,35),WXM(35),
     X                NTEMPF(5,35),NSPECR(35),IXFORM(35),NUMXS
C
      DATA (ALIAS(1,I),I=1,35)/
     1    'CLONO2    ', 'HNO4      ', 'CHCL2F    ', 'CCL4      ',
     2    'CCL3F     ', 'CCL2F2    ', 'C2CL2F4   ', 'C2CL3F3   ',
     3    'N2O5      ', 'HNO3      ', 'CF4', 'CHF2CL   ',
     4    23*' ZZZZZZZZ '/
      DATA (ALIAS(2,I),I=1,35)/
     1    'CLNO3     ', 'ZZZZZZZZ  ', 'ZZZZZZZZ  ', 'ZZZZZZZZ  ',
     2    'CFCL3     ', 'CF2CL2    ', 'C2F4CL2   ', 'C2F3CL3   ',
     3    'ZZZZZZZZ  ', 'ZZZZZZZZ  ', 'ZZZZZZZZ   ', 'ZZZZZZZ  ',
     4    23*' ZZZZZZZZ '/
      DATA (ALIAS(3,I),I=1,35)/
     1    ' ZZZZZZZZ ', ' ZZZZZZZZ ', 'CFC21     ', ' ZZZZZZZZ ',
     2    'CFC11     ', 'CFC12     ', 'CFC114    ', 'CFC113    ',
     3    ' ZZZZZZZZ ', ' ZZZZZZZZ ', 'CFC14     ',
     4    24*' ZZZZZZZZ ' /
      DATA (ALIAS(4,I),I=1,35)/
     1    ' ZZZZZZZZ ', ' ZZZZZZZZ ', 'F21       ', ' ZZZZZZZZ ',
     2    'F11       ', 'F12       ', 'F114      ', 'F113      ',
     3    ' ZZZZZZZZ ', ' ZZZZZZZZ ', 'F14       ',
     4    24*' ZZZZZZZZ ' /
C
      DATA V1FX/175*0.0/,V2FX/175*0.0/,DVFX/175*0.0/,WXM/35*0.0/
      DATA NTEMPF/175*0/,NSPECR/35*0/,IXFORM/35*0/,NUMXS/0/ 
C
      END                                                                    *
      SUBROUTINE XPROFL
      include 'parameter.list'
C
C**********************************************************************
C     THIS SUBROUTINE GENERATES THE DENSITY PROFILES OF THE CROSS-
C     SECTION MOLECULES.  IT STORES THE PROFILES IN THE ARRAY DENM IN 
C     /DEAMT/ AT THE ALTITUDES ZMDL, WHICH ARE THE SAME ALTITUDES THAT
C     THE PROFILES OF THE MOLECULAR AMOUNTS ARE DEFINED ON.  (NOTE: THE
C     ACTUAL ALTITUDES USED ARE FROM ZST WHICH IS A COPY OF ZMDL.)
C     IPRFL IS A FLAG INDICATING THAT THE STANDARD PROFILES (0) OR A
C     USER-INPUT PROFILE (1) IS TO BE USED.
C**********************************************************************
C
C**   IXMAX=MAX NUMBER OF X-SECTION MOLECULES, IXMOLS=NUMBER OF THESE 
C**   MOLECULES SELECTED, IXINDX=INDEX VALUES OF SELECTED MOLECULES
C**   (E.G. 1=CLONO2), XAMNT(I,L)=LAYER AMOUNTS FOR I'TH MOLECULE FOR 
C**   L'TH LAYER, ANALOGOUS TO AMOUNT IN /PATHD/ FOR THE STANDARD
C**   MOLECULES.
C
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON /PATHX/ IXMAX,IXMOLS,IXINDX(35),XAMNT(35,67)
      COMMON /DEAMT/ DENM(35,laytwo),DENP(35,laythr+1)
C
C**   COMMON BLOCKS AND PARAMETERS FOR THE PROFILES AND DENSITIES
C**   FOR THE CROSS-SECTION MOLECULES.
C**   XSNAME=NAMES, ALIAS=ALIASES OF THE CROSS-SECTION MOLECULES
C
      CHARACTER*10 XSFILE,XSNAME,ALIAS
      COMMON /XSECTF/ XSFILE(6,5,35),XSNAME(35),ALIAS(4,35) 
C
C**   AMOLX(L,I)=MIXING RATIO (PPMV) OF THE I'TH MOLECULE FOR THE L'TH
C**   LEVEL, ALTX(L)= ALTITUDE OF THE L'TH LEVEL, LAYXMX LEVELS MAX
C
      COMMON /MLATMX/ LAYXMX,ALTX(50),AMOLX(50,35)
      common /xpdem/ZX(laydim),DTMP(35),DENX(35,laydim)
      CHARACTER JCHAR(35)*1,XTITLE*50
C
C**   LOAD THE PROFILES OF ALTITUDE, PRESSURE, AND TEMPERATURE THAT
C**   WERE USED TO CALCULATE THE MOLECULAR AMOUNTS BACK INTO THE
C**   ARRAYS ZMDL, PM, AND TM FROM THE ARRAYS ZST, PST, AND TST
C
C
C
C**       A STANDARD PROFILE FOR X-MOLECULES DENSITY PROFILES HAS BEEN
C**       SELECTED. THE PROFILES OF VOLUMNE MIXING RATIO ARE IN AMOLX 
C**       STORED AT THE LEVELS ALTX. LOAD THE ALTITUDES INTO ZX AND
C**       DENX RESPECTIVELY.
          ixmols = 3
          ixindx(1) = 4
cc        ccl3f   ie  f11
          ixindx(2) = 5
cc        ccl2f2  ie  f12
          ixindx(3) = 6
C
          LAYX=LAYXMX
          DO 210 L=1,LAYX
              ZX(L) = altx(L) 
              DO 200 K=1,IXMOLS
                  DENX(K,L) = AMOLX(L,IXINDX(K))
  200         CONTINUE
  210     CONTINUE
C
C
C**   INTERPOLATE THE DENSITY PROFILE DENX DEFINED ON ZX TO DENM
C**   DEFINED ON ZMDL, THEN CONVERT MIXING RATIO TO NUMBER DENSITY.
C
      CALL XINTRP(LAYX,IXMOLS)
C
      RETURN
      END 
      SUBROUTINE XINTRP(LAYX,IXMOLS)
      include 'parameter.list'
C
C***********************************************************************
C     THIS SUBROUTINE INTERPLOLATES THE PROFILE DENX ON THE ALTITUDE
C     GRID ZX INTO DENM ON THE GRID ZMDL.  EXPONENTIAL INTERPOLATION
C     IS USED.
C***********************************************************************
C
C**   IFIL CARRIES FILE INFORMATION
C
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      COMMON /CNTRL/ KMAX,M,IKMAX,NL,ML,IKLO,ISSGEO,IMULT               driv 300
      COMMON /CONSTN/ PZERO,TZERO,AVOGAD,ALOSMT,GASCON,PLANK,BOLTZ,
     1    CLIGHT,ADCON,ALZERO,AVMWT,AIRMWT,AMWT(35)
C
C**   LAMCHN CARRIES HARDWARE SPECIFIC PARAMETERS 
C
cc    COMMON /LAMCHN/ ONEPL,ONEMI,EXPMIN,ARGMIN
cc    COMMON /MDATA/zmdl(50),Pm(50),Tm(50),WH(50),WCO2(50),WO(50),      aern 360
cc   X WN2O(50),WCO(50),WCH4(50),WO2(50),                               aern 370
cc   X CLD(50,7),RR(50,7)                                               aern 380
      COMMON /MDATA/Pm(laydim),Tm(laydim),WH(laydim),WCO2(laydim),
     x WO(laydim),WN2O(laydim),WCO(laydim),WCH4(laydim),WO2(laydim)
      COMMON /MODEL/ ZMdl(LAYDIM),P(LAYDIM),T(LAYDIM),
     1  RFNDX(LAYDIM),DENSTY(65,LAYDIM),CLDAMT(LAYDIM),RRAMT(LAYDIM),
     1  EQLWC(LAYDim),HAZEC(LAYDIM)
      COMMON /DEAMT/ DENM(35,laytwo),DENP(35,laythr+1)
cc    COMMON /PARMTR/ PI,DEG,GCAIR,RE,DELTAS,ZMIN,ZMAX,NOPRNT,IMMAX,
cc   1    IMDIM,IBMAX,IBDIM,IOUTMX,IOUTDM,IPMAX,IPHMID,IPDIM,KDIM,
cc   2    KMXNOM,NMOL
C
      common /xpdem/ZX(laydim),DTMP(35),DENX(35,laydim)
C
      LX = 2
      immax = ml
      DO 200 L=1,IMMAX
C
C**       FIND THE SMALLEST ZX GE ZMDL(L)
C
  100     CONTINUE
          IF(ZMDL(L) .LE. ZX(LX) .OR. LX .EQ. LAYX) THEN
              A = (ZMDL(L)-ZX(LX-1))/(ZX(LX)-ZX(LX-1))
              IF(A .LT. 0.0 .OR. A .GT. 1.0) THEN 
                   WRITE(IPR,'(//,''  XINTPL: CAUTION- EXTRAPOLATING'',
     1               '' X-SECTION PROFILE'')')
              ENDIF 
C
C**           CALCULATE THE NUMBER DENSITY OF AIR 
C
              RHOAIR = ALOSMT*(PM(L)/PZERO)/(TM(L)/TZERO)
C
              DO 110 K=1,IXMOLS
                  CALL EXPINT(DENM(K,L),DENX(K,LX-1),DENX(K,LX),A)
C
C**               CONVERT MIXING RATIO (PPMV) TO NUMBER DENSITY
C
                  DENM(K,L) = RHOAIR*DENM(K,L)*1.0E-6
  110         CONTINUE
              GO TO 190
          ELSE
              LX = LX+1
          ENDIF
          GO TO 100 
C
  190     CONTINUE
  200     CONTINUE
C
          RETURN
          END
      BLOCK DATA XMLATM
C     
C************************************************************************
C     THIS BLOCK DATA SUBROUTINE INITIALIZES THE STANDARD PROFILES
C     FOR THE "CROSS-SECTION" MOLECULES, THAT IS, THE MOLECULES FOR
C     WHICH THE SPECTRAL DATA IS IN THE FORM OF CROSS-SECTIONS
C     (ABSORPTION COEFFICIENTS) INSTEAD OF LINE PARAMETERS.
C     THE PROFILES OF VOLUME MIXING RATIOS GIVEN HERE ARE FROM:
C     
C     M. ALLEN, SPRING EQUINOX, DIUNRNAL AVERAGE, 1990.
C     (PRIVATE COMMUNICATION)
C************************************************************************
C     
C**   COMMON BLOCKS AND PARAMETERS FOR THE PROFILES AND DENSITIES
C**   FOR THE CROSS-SECTION MOLECULES.
C     
C**   AMOLX(L,I)=MIXING RATIO (PPMV) OF THE I'TH MOLECULE FOR THE L'TH
C**   LEVEL, ALTX(L)= ALTITUDE OF THE L'TH LEVEL, LAYXMX LEVELS MAX
C     
C     
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
c     modtran has 65 as magic number.  It includes the usual 12 species
c     plus a host of other species and sub species.  Many
c     arrays have dimension 65.

      include 'parameter.list'
c     PARAMETER (MMOLX=18)
c     PARAMETER (MMOLX2=2*MMOLX)
c     PARAMETER (MMOL=12, MMOL2=2*MMOL)
c     PARAMETER (MMOLT=MMOL+MMOLX, MMOLT2=2*MMOLT)
C
c     PARAMETER(NSPEC=12, NSPECX=13, NSPECT=NSPEC+NSPECX,
c    $     NSPEC2=2*NSPEC, NSPECX2=2*NSPECX, NSPECT2=2*NSPECT)
c
      CHARACTER*8 CNAMEX(MMOLX)                                       
C
c
c
      REAL AMWTX(MMOLX)
      REAL ALTX(50),
     +     AMOLX1(50), AMOLX2(50), AMOLX3(50), AMOLX4(50), AMOLX5(50),
     +     AMOLX6(50), AMOLX7(50), AMOLX8(50), AMOLX9(50), AMOLX10(50),
     +     AMOLX11(50),AMOLX12(50),AMOLX13(50),AMOLX14(50),AMOLX15(50),
     +     AMOLX16(50),AMOLX17(50),AMOLX18(50),AMOLX19(50),AMOLX20(50),
     +     AMOLX21(50),AMOLX22(50),AMOLX23(50),AMOLX24(50),AMOLX25(50),
     +     AMOLX26(50),AMOLX27(50),AMOLX28(50),AMOLX29(50),AMOLX30(50),
     +     AMOLX31(50),AMOLX32(50),AMOLX33(50),AMOLX34(50),AMOLX35(50)
C     
      COMMON /NAMEX/CNAMEX
      COMMON /ATMWTX/AMWTX
      COMMON /MLATMX/ LAYXMX,ALTX,
     +     AMOLX1,  AMOLX2,  AMOLX3,  AMOLX4,  AMOLX5,
     +     AMOLX6,  AMOLX7,  AMOLX8,  AMOLX9,  AMOLX10,
     +     AMOLX11, AMOLX12, AMOLX13, AMOLX14, AMOLX15,
     +     AMOLX16, AMOLX17, AMOLX18, AMOLX19, AMOLX20,
     +     AMOLX21, AMOLX22, AMOLX23, AMOLX24, AMOLX25,
     +     AMOLX26, AMOLX27, AMOLX28, AMOLX29, AMOLX30,
     +     AMOLX31, AMOLX32, AMOLX33, AMOLX34, AMOLX35
C
      DATA (CNAMEX(I),I=1,NSPECX)/' CFC-11 ',
     $     ' CFC-12 ',
     $     ' CFC-13 ',
     $     ' CFC-14 ',
     $     ' CFC-22 ',
     $     ' CFC-113',
     $     ' CFC-114',
     $     ' CFC-115',
     $     ' CLONO2 ',
     $     '  HNO4  ',
     $     ' CHCL2F ',
     $     '  CCL4  ',
     $     '  N2O5  '/
C
C
C     DATA AMWTX/CFC-11,CFC-12,CFC-13,CFC-14,CFC-22,CFC-113,CFC-114,CFC-115,
C     CLONO2,HNO4,CHCL2F,CCL4,N2O5/
      DATA (AMWTX(I),I=1,NSPECX)
     $     /137.3683, 120.9138, 104.4592, 88.00461, 86.4687,
     $     187.3762, 170.9216, 154.467,
     $     97.45790, 79.01227, 102.9239, 153.8235,108.0104/
C
      DATA LAYXMX/50/
C     
      DATA ALTX/
     1     0.0,    1.0,   2.0,   3.0,   4.0,  5.0,   6.0,   7.0,   8.0,
     2     9.0,   10.0,  11.0,  12.0,  13.0, 14.0,  15.0,  16.0,  17.0,
     3     18.0,  19.0,  20.0,  21.0,  22.0, 23.0,  24.0,  25.0,  27.5,
     4     30.0,  32.5,  35.0,  37.5,  40.0, 42.5,  45.0,  47.5,  50.0,
     5     55.0,  60.0,  65.0,  70.0,  75.0, 80.0,  85.0,  90.0,  95.0,
     6     100.0,105.0, 110.0, 115.0, 120.0/
C     
C     DATA AMOLX1 / CCL3F, AKA CFC-11 /
C     
      DATA AMOLX1 /
     X     1.400E-04, 1.400E-04, 1.400E-04, 1.400E-04, 1.400E-04,
     X     1.400E-04, 1.400E-04, 1.400E-04, 1.400E-04, 1.398E-04,
     X     1.395E-04, 1.393E-04, 1.390E-04, 1.350E-04, 1.310E-04,
     X     1.270E-04, 1.230E-04, 1.123E-04, 1.016E-04, 9.090E-05,
     X     8.020E-05, 6.865E-05, 5.710E-05, 4.555E-05, 3.400E-05,
     X     2.728E-05, 1.048E-05, 3.902E-06, 6.031E-07, 1.984E-07,
     X     2.351E-08, 1.850E-09, 7.681E-10, 9.182E-11, 2.389E-11,
     X     5.965E-12, 6.108E-13, 4.240E-14, 4.972E-15, 5.090E-16,
     X     4.235E-17, 1.980E-18, 1.862E-19, 1.540E-20, 1.015E-21,
     X     3.110E-23, 2.073E-24, 1.356E-25, 7.233E-27, 5.570E-28/
C     
C     DATA AMOLX2 / CCL2F2, AKA CFC-12 /
C     
      DATA AMOLX2 /
     X     2.400E-04, 2.400E-04, 2.400E-04, 2.400E-04, 2.400E-04,
     X     2.400E-04, 2.400E-04, 2.400E-04, 2.400E-04, 2.398E-04,
     X     2.395E-04, 2.393E-04, 2.390E-04, 2.355E-04, 2.320E-04,
     X     2.285E-04, 2.250E-04, 2.148E-04, 2.045E-04, 1.943E-04,
     X     1.840E-04, 1.710E-04, 1.580E-04, 1.450E-04, 1.320E-04,
     X     1.188E-04, 8.580E-05, 5.885E-05, 3.564E-05, 2.133E-05,
     X     1.203E-05, 6.090E-06, 3.834E-06, 2.133E-06, 1.264E-06,
     X     8.520E-07, 4.160E-07, 1.940E-07, 9.038E-08, 3.905E-08,
     X     1.525E-08, 5.190E-09, 1.829E-09, 5.670E-10, 1.450E-10,
     X     2.480E-11, 4.303E-12, 6.140E-13, 6.613E-14, 9.590E-15/
C     
C     DATA AMOLX3 / CCLF3, AKA CFC-13  /
C     
      DATA AMOLX3 /
     X     50*1.E-12                                           /
C     
C     DATA AMOLX4 / CF4, AKA CFC-14 /
C     
      DATA AMOLX4 /
     X     50*1.E-12                                            /
C     
C     DATA AMOLX5 / CHF2CL, AKA CFC-22 /  ????
C     
      DATA AMOLX5 /
     X     6.000E-05, 5.995E-05, 5.990E-05, 5.985E-05, 5.980E-05,
     X     5.978E-05, 5.975E-05, 5.973E-05, 5.970E-05, 5.965E-05,
     X     5.960E-05, 5.955E-05, 5.950E-05, 5.893E-05, 5.835E-05,
     X     5.778E-05, 5.720E-05, 5.563E-05, 5.405E-05, 5.248E-05,
     X     5.090E-05, 4.893E-05, 4.695E-05, 4.498E-05, 4.300E-05,
     X     4.085E-05, 3.548E-05, 3.025E-05, 2.520E-05, 2.070E-05,
     X     1.703E-05, 1.390E-05, 1.196E-05, 1.038E-05, 9.330E-06,
     X     8.780E-06, 8.118E-06, 7.700E-06, 7.423E-06, 7.185E-06,
     X     6.910E-06, 6.520E-06, 5.838E-06, 4.790E-06, 3.360E-06,
     X     1.790E-06, 7.138E-07, 2.235E-07, 5.860E-08, 1.920E-08/
C     
C     DATA AMOLX5 / CHCLF2, AKA CFC-22 /  ????
C     DATA AMOLX5 /
C     X     6.000E-05, 5.994E-05, 5.987E-05, 5.982E-05, 5.977E-05,
C     X     5.974E-05, 5.970E-05, 5.968E-05, 5.966E-05, 5.963E-05,
C     X     5.960E-05, 5.955E-05, 5.949E-05, 5.921E-05, 5.893E-05,
C     X     5.808E-05, 5.723E-05, 5.582E-05, 5.441E-05, 5.265E-05,
C     X     5.089E-05, 4.897E-05, 4.705E-05, 4.502E-05, 4.298E-05,
C     X     4.084E-05, 3.548E-05, 3.021E-05, 2.514E-05, 2.062E-05,
C     X     1.686E-05, 1.392E-05, 1.184E-05, 1.036E-05, 9.356E-06,
C     X     8.784E-06, 8.163E-06, 7.741E-06, 7.449E-06, 7.201E-06,
C     X     6.919E-06, 6.524E-06, 5.872E-06, 4.867E-06, 3.396E-06,
C     X     1.808E-06, 6.935E-07, 2.066E-07, 5.485E-08, 1.930E-08/
C     
C     DATA AMOLX6 / C2CL3F3, AKA CFC-113 /
C     
      DATA AMOLX6 /
     X     1.900E-05, 1.900E-05, 1.900E-05, 1.900E-05, 1.900E-05,
     X     1.900E-05, 1.900E-05, 1.900E-05, 1.900E-05, 1.898E-05,
     X     1.895E-05, 1.893E-05, 1.890E-05, 1.855E-05, 1.820E-05,
     X     1.785E-05, 1.750E-05, 1.653E-05, 1.555E-05, 1.458E-05,
     X     1.360E-05, 1.238E-05, 1.116E-05, 9.932E-06, 8.710E-06,
     X     7.603E-06, 4.834E-06, 2.915E-06, 1.411E-06, 7.168E-07,
     X     3.205E-07, 1.230E-07, 7.013E-08, 3.215E-08, 1.653E-08,
     X     9.905E-09, 3.920E-09, 1.420E-09, 5.268E-10, 1.757E-10,
     X     5.143E-11, 1.230E-11, 3.263E-12, 7.440E-13, 1.344E-13,
     X     1.310E-14, 1.238E-15, 7.866E-17, 2.326E-18, 7.910E-20/
C     
C     DATA AMOLX7 / C2CL2F4, AKA CFC-114 /
C     
      DATA AMOLX7 /
     X     1.200E-05, 1.200E-05, 1.200E-05, 1.200E-05, 1.200E-05,
     X     1.200E-05, 1.200E-05, 1.200E-05, 1.200E-05, 1.200E-05,
     X     1.200E-05, 1.200E-05, 1.200E-05, 1.188E-05, 1.175E-05,
     X     1.163E-05, 1.150E-05, 1.118E-05, 1.085E-05, 1.053E-05,
     X     1.020E-05, 9.755E-06, 9.310E-06, 8.865E-06, 8.420E-06,
     X     7.948E-06, 6.766E-06, 5.635E-06, 4.559E-06, 3.653E-06,
     X     2.926E-06, 2.320E-06, 1.939E-06, 1.618E-06, 1.386E-06,
     X     1.250E-06, 1.040E-06, 8.690E-07, 7.193E-07, 5.855E-07,
     X     4.645E-07, 3.550E-07, 2.535E-07, 1.590E-07, 7.688E-08,
     X     2.130E-08, 3.514E-09, 3.470E-10, 1.612E-11, 8.970E-13/
C     
C     DATA AMOLX8 / C2CLF5, AKA CFC-115 /
C     
      DATA AMOLX8 /
     X     50*1.E-12                                            /
C     
C     DATA AMOLX9 / CLONO2 /
C     
      DATA AMOLX9 /
     X     5.750E-06, 5.128E-06, 4.505E-06, 3.883E-06, 3.260E-06,
     X     2.755E-06, 2.250E-06, 1.745E-06, 1.240E-06, 1.208E-06,
     X     1.175E-06, 1.143E-06, 1.110E-06, 8.007E-06, 1.491E-05,
     X     2.180E-05, 2.870E-05, 1.015E-04, 1.744E-04, 2.472E-04,
     X     3.200E-04, 4.600E-04, 6.000E-04, 7.400E-04, 8.800E-04,
     X     9.725E-04, 1.204E-03, 1.140E-03, 9.456E-04, 5.238E-04,
     X     2.379E-04, 4.270E-05, 1.739E-05, 1.680E-06, 3.544E-07,
     X     4.668E-08, 1.194E-09, 1.710E-11, 8.568E-13, 7.235E-14,
     X     5.415E-15, 4.040E-17, 3.793E-20, 1.172E-22, 8.674E-25,
     X     8.080E-28, 8.438E-30, 7.600E-32, 0.       , 0.       /
C     
C     DATA AMOLX10 / HNO4 /
C     
      DATA AMOLX10 /
     X     4.340E-07, 2.136E-06, 3.837E-06, 5.539E-06, 7.240E-06,
     X     1.816E-05, 2.907E-05, 3.999E-05, 5.090E-05, 4.910E-05,
     X     4.730E-05, 4.550E-05, 4.370E-05, 5.655E-05, 6.940E-05,
     X     8.225E-05, 9.510E-05, 1.426E-04, 1.901E-04, 2.375E-04,
     X     2.850E-04, 3.518E-04, 4.185E-04, 4.853E-04, 5.520E-04,
     X     5.808E-04, 6.526E-04, 5.100E-04, 3.187E-04, 1.469E-04,
     X     5.261E-05, 9.960E-06, 4.435E-06, 8.735E-07, 2.573E-07,
     X     7.630E-08, 6.803E-09, 4.790E-10, 7.730E-11, 3.210E-11,
     X     1.266E-11, 8.430E-13, 1.883E-15, 1.734E-18, 4.466E-21,
     X     3.330E-24, 6.306E-26, 1.522E-27, 3.538E-29, 2.240E-30/
C     
C     DATA AMOLX11 / CHCL2F, AKA CFC-21/
C     
      DATA AMOLX11 /
     X     50*1.E-12/
C     
C     DATA AMOLX12 / CCL4 /
C     
      DATA AMOLX12 /
     X     1.300E-04, 1.300E-04, 1.300E-04, 1.300E-04, 1.300E-04,
     X     1.298E-04, 1.295E-04, 1.293E-04, 1.290E-04, 1.290E-04,
     X     1.290E-04, 1.290E-04, 1.290E-04, 1.250E-04, 1.210E-04,
     X     1.170E-04, 1.130E-04, 1.018E-04, 9.060E-05, 7.940E-05,
     X     6.820E-05, 5.715E-05, 4.610E-05, 3.505E-05, 2.400E-05,
     X     1.884E-05, 5.940E-06, 1.755E-06, 1.316E-07, 3.954E-08,
     X     1.715E-09, 3.960E-11, 1.531E-11, 5.572E-13, 1.100E-13,
     X     1.093E-14, 3.775E-16, 5.250E-18, 2.454E-19, 9.875E-21,
     X     3.051E-22, 2.930E-24, 1.112E-25, 3.611E-27, 8.790E-29,
     X     5.100E-31, 1.358E-32, 0.       , 0.       , 0.       /
C     
C     DATA AMOLX13 / N2O5 /
C     
      DATA AMOLX13 /
C     
     X     2.420E-10, 9.540E-10, 1.666E-09, 2.378E-09, 3.090E-09,
     X     3.608E-09, 4.125E-09, 4.643E-09, 5.160E-09, 5.763E-09,
     X     6.365E-09, 6.968E-09, 7.570E-09, 4.407E-07, 8.738E-07,
     X     1.307E-06, 1.740E-06, 8.780E-06, 1.582E-05, 2.286E-05,
     X     2.990E-05, 4.145E-05, 5.300E-05, 6.455E-05, 7.610E-05,
     X     7.703E-05, 7.934E-05, 5.520E-05, 2.730E-05, 1.082E-05,
     X     2.724E-06, 2.130E-07, 8.313E-08, 3.938E-09, 7.580E-10,
     X     6.289E-11, 1.044E-12, 5.120E-15, 1.355E-16, 8.200E-18,
     X     7.258E-19, 5.090E-21, 3.961E-24, 2.392E-27, 8.388E-30,
     X     0.       , 0.       , 0.       , 0.       , 0.       /
C     
C     DATA AMOLX14 / HNO3 /
C     
      DATA AMOLX14 /
     X     5.550E-05, 6.403E-05, 7.255E-05, 8.108E-05, 8.960E-05,
     X     9.320E-05, 9.680E-05, 1.004E-04, 1.040E-04, 1.205E-04,
     X     1.370E-04, 1.535E-04, 1.700E-04, 4.800E-04, 7.900E-04,
     X     1.100E-03, 1.410E-03, 2.163E-03, 2.915E-03, 3.668E-03,
     X     4.420E-03, 5.010E-03, 5.600E-03, 6.190E-03, 6.780E-03,
     X     6.790E-03, 6.815E-03, 5.875E-03, 4.643E-03, 3.205E-03,
     X     1.981E-03, 9.000E-04, 4.356E-04, 1.229E-04, 3.756E-05,
     X     1.170E-05, 1.048E-06, 7.040E-08, 1.099E-08, 4.315E-09,
     X     9.327E-10, 1.320E-10, 1.112E-12, 4.992E-15, 2.180E-17,
     X     1.840E-20, 3.313E-22, 9.205E-24, 5.043E-25, 1.260E-25/
C     
C     DATA AMOLX15 / ?????? /
C     
      DATA AMOLX15 /
     X     50*-99.                                              /
C     
C     DATA AMOLX16 / ?????? /
C     
      DATA AMOLX16 /
     X     50*-99.                                              /
C     
C     DATA AMOLX17 / ?????? /
C     
      DATA AMOLX17 /
     X     50*-99.                                              /
C     
C     DATA AMOLX18 / ?????? /
C     
      DATA AMOLX18 /
     X     50*-99.                                              /
C     
C     DATA AMOLX19 / ?????? /
C     
      DATA AMOLX19 /
     X     50*-99.                                              /
C     
C     DATA AMOLX20 / ?????? /
C     
      DATA AMOLX20 /
     X     50*-99.                                              /
C     
C     DATA AMOLX21 / ?????? /
C     
      DATA AMOLX21 /
     X     50*-99.                                              /
C     
C     DATA AMOLX22 / ?????? /
C     
      DATA AMOLX22 /
     X     50*-99.                                              /
C     
C     DATA AMOLX23 / ?????? /
C     
      DATA AMOLX23 /
     X     50*-99.                                              /
C     
C     DATA AMOLX24 / ?????? /
C     
      DATA AMOLX24 /
     X     50*-99.                                              /
C     
C     DATA AMOLX25 / ?????? /
C     
      DATA AMOLX25 /
     X     50*-99.                                              /
C     
C     DATA AMOLX26 / ?????? /
C     
      DATA AMOLX26 /
     X     50*-99.                                              /
C     
C     DATA AMOLX27 / ?????? /
C     
      DATA AMOLX27 /
     X     50*-99.                                              /
C     
C     DATA AMOLX28 / ?????? /
C     
      DATA AMOLX28 /
     X     50*-99.                                              /
C     
C     DATA AMOLX29 / ?????? /
C     
      DATA AMOLX29 /
     X     50*-99.                                              /
C     
C     DATA AMOLX30 / ?????? /
C     
      DATA AMOLX30 /
     X     50*-99.                                              /
C     
C     DATA AMOLX31 / ?????? /
C     
      DATA AMOLX31 /
     X     50*-99.                                              /
C     
C     DATA AMOLX32 / ?????? /
C     
      DATA AMOLX32 /
     X     50*-99.                                              /
C     
C     DATA AMOLX33 / ?????? /
C     
      DATA AMOLX33 /
     X     50*-99.                                              /
C     
C     DATA AMOLX34 / ?????? /
C     
      DATA AMOLX34 /
     X     50*-99.                                              /
C     
      DATA AMOLX35 /
     X     50*-99.                                              /
C     
      END
      function   xterp(xcc,x,y,ndeg,npts,dint,ier)
c
c   function performs newtons interpolation for discrete data
c            as a function of one variable
c
c   where  xc - independent variable at which the interpolated value
c               of the dependent variable is desired
c           x - table of independent variable values in increasing
c               order
c           y - corresponding table of dependent variable values
c        ndeg - order of the interpolating polynomial used (max - 10)
c        npts - number of entries in x and y
c        dint - derivitive at xc
c         ier - return code:
c                      0 = interpolation was performed
c                     -1 = extrapolation below table values
c                      1 = extrapolation above table values
c
c  routine modified from 'the computing technology center numerical
c    analysis library', o.r.n.l.
c
      COMMON /IFIL/IRD,IPR,IPU,NPR,IPR1,ISCRCH
      dimension x(52),y(52),y1(11),pi(12)
      integer hi
      data pi/12*1./
c
      xc=xcc
      nfit=ndeg + 1
      n=nfit
      mflag=0
      if(n .gt. npts) n=npts
      if(xc-x(1)) 50,20,10
10    if(xc .gt. x(npts)) go to 70
20    ier=0
      do 30 i=1,npts
      if(xc - x(i)) 40,120,30
120   xc=xc + .000001
      mflag=i
30    continue
40    low=i - (n+1)/2
      if(low .le. 0) go to 60
      hi=low + n - 1
      if(hi .gt. npts) go to 80
      go to 90
c
c ... xc lt x(1)
50    ier=-1
60    hi=n
      low=1
      go to 90
c
c ... xc gt x(npts)
70    ier=1
80    low=npts - n + 1
      hi=npts
c
c ... interpolate
90    con=1.
      dint=0.
      y1(1)=y(low)
      xterp=y(low)
      im=low - 1
      il=low + 1
      do 110 k=il,hi
      val=xterp
      ia=k - low
      is=ia + 1
      y1(is)=y(k)
      do 100 i=1,ia
      ir=im + i
      if(x(ir).eq.x(k))go to 100
      y1(is) = (y1(i)-y1(is))/(x(ir)-x(k))
  100 continue
      con=con*(xc-x(k-1))
      pi(ia+1)=con
      con1=pi(ia)
      if(ia .lt. 2) go to 112
      do 111 i=2,ia
      if(pi(i).eq.0.0)go to 111
      con1=con1 + con*pi(i-1)/pi(i)
  111 continue
  112 dint=dint + con1*y1(is)
  110 xterp=val + con*y1(is)
      if(mflag.ne.0)xterp=y(mflag)
      return
      end
