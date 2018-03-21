      SUBROUTINE BMTRAN(XSTAR,ACBAR,ADBAR,ODBAR,DV,TTRANS,ACBAR2)
C
C ... EQUIVALENT WIDTH OF A VOIGT LINE IN A FINITE SPECTRAL INTERVAL.
C     IT USES THE NASA CURVE-OF-GROWTH FORMULATION
C        XSTAR     AVERAGE VALUE OF  (S/D)U
C        ACBAR        "      "   "   LORENTZ LINE WIDTH / LINE DENSITY
C        ADBAR        "      "   "   DOPPLER  "     "   "   "     "
C        ODBAR        "      "   "   (1/D)
C         DV       WIDTH OF SPECTRAL INTERVAL
C         WSL      EQUIVALENT WIDTH OF A SINGLE LINE
C        TTRANS    TRANSMITTANCE OF THE INTERVAL
C
C ... LIST DATA
C        P5LN2 = .5 * LN(2)
C        C     = 2. / SQRT(PI)
      DATA P5LN2,C/0.346573590,1.128379167/
C
C     CALCULATE SINGLE (AVERAGE) LINE STRENGTH
      XS=XSTAR/ODBAR
C
C ... CALCULATE (LORENTZ EQUIVALENT WIDTH / LINE STRENGTH) SQUARED
      STORE=XSTAR/ACBAR
      RATIO=1.
      IF(ACBAR2.NE.0.)THEN
          RHO=ACBAR2/ACBAR**2
          DENOM=(RHO+1)*(RHO**2+RHO+1)
          F1=RHO*(RHO-1)*(RHO**2+1)/DENOM
          DENOM=DENOM*(RHO+1)**2
          F2=4*RHO**2*(RHO**2+1)/DENOM
c         F3=1.0088+(RHO-3)*(.4524+(RHO-3.25)*(.0028-.0064*(RHO-3.5)))
c         if(rho.gt.3)F3=1.0088+(RHO-3)*.4509 + (rho-3)**2*.0076
c    1      + (rho-3)**3*.015 
          F3=0.1886+(RHO-1)*.7200
          STR2PI=.5*STORE/3.1416
          STRNEW=STORE-3.1416*STR2PI**2*F1/(1+STR2PI*F3+.5*STR2PI**2*F2)
          RATIO=STRNEW/STORE
          STORE=STRNEW
      ENDIF
      DENOM=4.+STORE
C
C ... CALCULATE (DOPPLER EQUIVALENT WIDTH / LINE STRENGTH) SQUARED
      WD=XSTAR/ADBAR
      WD=P5LN2*WD*WD
      IF(WD.LT..0001)THEN
          WSL=XS*(1.-.25*WD*(STORE+(1-RATIO)*(1+RATIO))/DENOM)
      ELSEIF(WD.LT..01)THEN
          STORE=WD*(.25-WD*(.16666667-.125*WD))*
     1      (STORE+(1-RATIO)*(1+RATIO))/DENOM
          WSL=XS*(1.-STORE*(1.+.5*STORE*(1.+STORE)))
      ELSE
          WD=LOG(1.+WD)/WD
          WL=4./DENOM*RATIO**2
          WSL=XS*SQRT(WD+WL-WD*WL)
      ENDIF
      WSL0=WSL
C
C ... SUBTRACT TAIL CONTRIBUTION IF SIGNIFICANT
      XS=XS*RATIO
C     IF(XS*XS/DENOM.GT..000025)THEN
          U0=C*SQRT(XS*ACBAR/ODBAR)/DV
          WSL=WSL-DV*(.25*BMERFU(2*U0)+.75*BMERFU(U0/1.5))
C     ENDIF
C
C ... CALCULATE TRANSMITTANCE
      IF(WSL.GE.DV)THEN
          TTRANS=0.
      ELSE
          TTRANS=(1.-WSL/DV)**(DV*ODBAR)
      ENDIF
C
      RETURN
      END
      FUNCTION    BMERFU(Y)
C
C ... APPROXIMATION FOR EXP(-Y*Y) + SQRT(PI) * Y * ERF(Y) - 1
      DATA P,A1,A2,A3,A4,A5,RTPI/.3275911,.451673692,-.504257335,
     1  2.519390259,-2.575644906,1.881292140,1.772453851/
      IF(Y.GE.2.33)THEN
          BMERFU=RTPI*Y-1.
          IF(Y.LT.3.2)THEN
              T=Y*Y
	      BMERFU=BMERFU+(1.75+T*.5)/(3.75+T*(5.+T))*EXP(-T)
	  ENDIF
	  RETURN
      ENDIF
      T=1./(1.+P*Y)
      BMERFU=(1.-Y*T*(A1+T*(A2+T*(A3+T*(A4+T*A5)))))*EXP(-Y*Y)+RTPI*Y-1.
      RETURN
      END
      BLOCK DATA BO2C                                                   bo2c 100
C>    BLOCK DATA                                                        bo2c 110
C                                                                       bo2c 120
C     BLOCK DATA   (IDENTICAL TO BLOCK DATA IN FASCOD2)                 bo2c 130
C                                                                       bo2c 140
      COMMON/O2C/ O2DRAY(74),O2C001(74),O2S0(74),O2A(74),O2B(74),       bo2c 150
     X V1O2,V2O2,DVO2,NPTO2                                             bo2c 160
      DATA V1O2,V2O2,DVO2,NPTO2 /1395.0,1760.0,5.0,74/                  bo2c 170
      DATA O2S0/                                                        bo2c 180
     A0.       ,                                                        bo2c 190
     +  .110E-8, .220E-8, .440E-8, .881E-8, .176E-7, .353E-7, .705E-7,  bo2c 200
     B .141E-06, .158E-06, .174E-06, .190E-06, .207E-06, .253E-06,      bo2c 210
     B .307E-06, .357E-06, .401E-06, .445E-06, .508E-06, .570E-06,      bo2c 220
     B .599E-06, .627E-06, .650E-06, .672E-06, .763E-06, .873E-06,      bo2c 230
     B .101E-05, .109E-05, .121E-05, .133E-05, .139E-05, .145E-05,      bo2c 240
     B .148E-05, .140E-05, .134E-05, .126E-05, .118E-05, .114E-05,      bo2c 250
     B .109E-05, .105E-05, .105E-05, .105E-05, .104E-05, .103E-05,      bo2c 260
     B .992E-06, .945E-06, .876E-06, .806E-06, .766E-06, .726E-06,      bo2c 270
     B .640E-06, .555E-06, .469E-06, .416E-06, .364E-06, .311E-06,      bo2c 280
     B .266E-06, .222E-06, .177E-06, .170E-06, .162E-06, .155E-06,      bo2c 290
     B .143E-06, .130E-06, .118E-06, .905E-07, .629E-07,                bo2c 300
     + .316E-7, .157E-7, .786E-8, .393E-8, .196E-8, .982E-9,            bo2c 310
     + 0./                                                              bo2c 320
      DATA O2A /                                                        bo2c 330
     A 0.       ,                                                       bo2c 340
     +   .147E-3, .147E-3, .147E-3,  .147E-3, .147E-3, .147E-3, .147E-3,bo2c 350
     B  .147E-03,  .122E-02,  .204E-02,  .217E-02,  .226E-02,  .126E-02,bo2c 360
     B  .362E-03, -.198E-02, -.545E-02, -.786E-02, -.624E-02, -.475E-02,bo2c 370
     B -.506E-02, -.533E-02, -.586E-02, -.635E-02, -.644E-02, -.679E-02,bo2c 380
     B -.741E-02, -.769E-02, -.780E-02, -.788E-02, -.844E-02, -.894E-02,bo2c 390
     B -.899E-02, -.922E-02, -.892E-02, -.857E-02, -.839E-02, -.854E-02,bo2c 400
     B -.871E-02, -.889E-02, -.856E-02, -.823E-02, -.796E-02, -.768E-02,bo2c 410
     B -.715E-02, -.638E-02, -.570E-02, -.491E-02, -.468E-02, -.443E-02,bo2c 420
     B -.333E-02, -.184E-02,  .313E-03, -.164E-04, -.417E-03, -.916E-03,bo2c 430
     B -.206E-02, -.343E-02, -.515E-02, -.365E-02, -.172E-02,  .926E-03,bo2c 440
     B  .168E-02,  .262E-02,  .380E-02,  .551E-02,  .889E-02,           bo2c 450
     + .889E-2,  .889E-2, .889E-2, .889E-2, .889E-2, .889E-2,           bo2c 460
     +  0./                                                             bo2c 470
      DATA O2B  /                                                       bo2c 480
     A 0.       ,                                                       bo2c 490
     + .306E-4,-.306E-4,-.306E-4,-.306E-4,-.306E-4,-.306E-4,-.306E-4,   bo2c 500
     B -.306E-04, -.218E-04, -.159E-04, -.346E-05,  .642E-05,  .360E-05,bo2c 510
     B -.140E-05,  .157E-04,  .471E-04,  .656E-04,  .303E-04, -.192E-05,bo2c 520
     B  .705E-05,  .149E-04,  .200E-04,  .245E-04,  .158E-04,  .841E-05,bo2c 530
     B  .201E-05,  .555E-05,  .108E-04,  .150E-04,  .193E-04,  .230E-04,bo2c 540
     B  .243E-04,  .226E-04,  .184E-04,  .157E-04,  .169E-04,  .197E-04,bo2c 550
     B  .226E-04,  .258E-04,  .235E-04,  .212E-04,  .185E-04,  .156E-04,bo2c 560
     B  .125E-04,  .872E-05,  .760E-05,  .577E-05,  .334E-07, -.652E-05,bo2c 570
     B -.977E-05, -.157E-04, -.273E-04, -.180E-04, -.641E-05,  .817E-05,bo2c 580
     B  .326E-04,  .626E-04,  .101E-03,  .755E-04,  .430E-04, -.113E-05,bo2c 590
     B -.578E-05, -.120E-04, -.208E-04, -.235E-04, -.364E-04,           bo2c 600
     + .364E-4, -.364E-4,-.364E-4,-.364E-4,-.364E-4,-.364E-4,           bo2c 610
     + 0./                                                              bo2c 620
C                                                                       bo2c 630
      END                                                               bo2c 640
