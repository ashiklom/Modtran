      SUBROUTINE  DISORT( NLYR, DTAUC, SSALB, PMOM, TEMPER, WVNMLO,
     $                    WVNMHI, USRTAU, NTAU, UTAU, NSTR, USRANG,
     $                    NUMU, UMU, NPHI, PHI, IBCND, FBEAM, UMU0,
     $                    PHI0, FISOT, LAMBER, ALBEDO, HL, BTEMP,
     $                    TTEMP, TEMIS, DELTAM, PLANK, ONLYFL,
     $                    ACCUR, PRNT, HEADER, MAXCLY, MAXULV,
     $                    MAXUMU, MAXCMU, MAXPHI, RFLDIR, RFLDN,
     $                    FLUP, DFDT, UAVG, UU, U0U, ALBMED, TRNMED,
     $                    msflag, wn0, s0cms, t0cms,sfdnsrt,sfdntrt )
c=====>        lower case variables added
              implicit double precision (a-h, o-z)


C **********************************************************************
C       Plane-parallel discrete ordinates radiative transfer program
C             ( see DISORT.DOC for complete documentation )
C **********************************************************************

C+---------------------------------------------------------------------+
C------------------    I/O VARIABLE SPECIFICATIONS     -----------------
C+---------------------------------------------------------------------+
C+---------------------------------------------------------------------+
C  LOCAL SYMBOLIC DIMENSIONS (have big effect on storage requirements):

C       MXCLY  = Max no. of computational layers
C       MXULV  = Max no. of output levels
C       MXCMU  = Max no. of computation polar angles
C       MXUMU  = Max no. of output polar angles
C       MXPHI  = Max no. of output azimuthal angles

cj 7/26 need to make MXCLY and MXULV consistent with the parameterization
cj      in MODTRAN3 by including parameter.list.
c
cj      PARAMETER ( MXCLY = 34, MXULV = 34, MXCMU = 16, MXUMU = 1,
cj     $            MXPHI = 1, MI = MXCMU/2, MI9M2 = 9*MI-2,
cj     $            NNLYRI = MXCMU*MXCLY )
c
         include 'parameter.list'
c
      PARAMETER (MXCLY =laydim, MXULV =laydim+1, MXCMU =16, MXUMU=1,
     $            MXPHI = 1, MI = MXCMU/2, MI9M2 = 9*MI-2,
     $            NNLYRI = MXCMU*MXCLY )
c
cj ^

C+---------------------------------------------------------------------+

      CHARACTER  HEADER*127
      LOGICAL  DELTAM, LAMBER, PLANK, ONLYFL, PRNT(7), USRANG, USRTAU
      INTEGER  IBCND, MAXCLY, MAXUMU, MAXULV, MAXCMU, MAXPHI, NLYR,
     $         NUMU, NSTR, NPHI, NTAU
      real*8     ACCUR, ALBEDO, BTEMP, DTAUC( MAXCLY ), FBEAM, FISOT,
     $         HL( 0:MAXCMU ), PHI( MAXPHI ), PMOM( 0:MAXCMU, MAXCLY ),
     $         PHI0, SSALB( MAXCLY ), TEMPER( 0:MAXCLY ), TEMIS, TTEMP,
     $         WVNMLO, WVNMHI, UMU( MAXUMU ), UMU0, UTAU( MAXULV )

      real*8     RFLDIR( MAXULV ), RFLDN( MAXULV ), FLUP( MAXULV ),
     $         UAVG( MAXULV ), DFDT( MAXULV ), U0U( MAXUMU, MAXULV ),
     $         UU( MAXUMU, MAXULV, MAXPHI ), ALBMED( MAXUMU ),
     $         TRNMED( MAXUMU ),
     $         s0cms( maxumu,maxulv ), t0cms( maxumu,maxulv ),
     $         fdnsrt, fdntrt, wn0

          real sfdnsrt,sfdntrt 

c=====>        lower case variables added

C+---------------------------------------------------------------------+
C      Routines called (IN ORDER):  SLFTST, ZEROAL, CHEKIN, SETDIS,
C                                   PRTINP, ALBTRN, LEPOLY, SURFAC,
C                                   SOLEIG, UPBEAM, UPISOT, TERPEV,
C                                   TERPSO, SETMTX, SOLVE0, FLUXES,
C                                   USRINT, PRAVIN, PRTINT
C+---------------------------------------------------------------------+

C  Index conventions (for all DO-loops and all variable descriptions):

C     IU     :  for user polar angles

C  IQ,JQ,KQ  :  for computational polar angles ('quadrature angles')

C   IQ/2     :  for half the computational polar angles (just the ones
C               in either 0-90 degrees, or 90-180 degrees)

C     J      :  for user azimuthal angles

C     K,L    :  for Legendre expansion coefficients or, alternatively,
C               subscripts of associated Legendre polynomials

C     LU     :  for user levels

C     LC     :  for computational layers (each having a different
C               single-scatter albedo and/or phase function)

C    LEV     :  for computational levels

C    MAZIM   :  for azimuthal components in Fourier cosine expansion
C               of intensity and phase function

C+---------------------------------------------------------------------+
C               I N T E R N A L    V A R I A B L E S

C   AMB(IQ/2,IQ/2)    First matrix factor in reduced eigenvalue problem
C                     of Eqs. SS(12), STWJ(8E)  (used only in SOLEIG)

C   APB(IQ/2,IQ/2)    Second matrix factor in reduced eigenvalue problem
C                     of Eqs. SS(12), STWJ(8E)  (used only in SOLEIG)

C   ARRAY(IQ,IQ)      Scratch matrix for SOLEIG, UPBEAM and UPISOT
C                     (see each subroutine for definition)

C   B()               Right-hand side vector of Eq. SC(5) going into
C                     SOLVE0,1;  returns as solution vector
C                     vector  L, the constants of integration

C   BDR(IQ/2,0:IQ/2)  Bottom-boundary bidirectional reflectivity for a
C                     given azimuthal component.  First index always
C                     refers to a computational angle.  Second index:
C                     if zero, refers to incident beam angle UMU0;
C                     if non-zero, refers to a computational angle.

C   BEM(IQ/2)         Bottom-boundary directional emissivity at compu-
C                     tational angles.

C   BPLANK            Intensity emitted from bottom boundary

C   CBAND()           Matrix of left-hand side of the linear system
C                     Eq. SC(5), scaled by Eq. SC(12);  in banded
C                     form required by LINPACK solution routines

C   CC(IQ,IQ)         C-sub-IJ in Eq. SS(5)

C   CMU(IQ)           Computational polar angles (Gaussian)

C   CWT(IQ)           Quadrature weights corresponding to CMU

C   DELM0             Kronecker delta, delta-sub-M0, where M = MAZIM
C                     is the number of the Fourier component in the
C                     azimuth cosine expansion

C   EMU(IU)           Bottom-boundary directional emissivity at user
c                     angles.

C   EVAL(IQ)          Temporary storage for eigenvalues of Eq. SS(12)

C   EVECC(IQ,IQ)      Complete eigenvectors of SS(7) on return from
C                     SOLEIG; stored permanently in  GC

C   EXPBEA(LC)        Transmission of direct beam in delta-M optical
C                     depth coordinates

C   FLYR(LC)          Truncated fraction in delta-M method

C   GL(K,LC)          Phase function Legendre polynomial expansion
C                     coefficients, calculated from PMOM by
C                     including single-scattering albedo, factor
C                     2K+1, and (if DELTAM=TRUE) the delta-M
C                     scaling

C   GC(IQ,IQ,LC)      Eigenvectors at polar quadrature angles,
C                     g  in Eq. SC(1)

C   GU(IU,IQ,LC)      Eigenvectors interpolated to user polar angles
C                     ( g  in Eqs. SC(3) and S1(8-9), i.e.
C                       G without the L factor )

C   HLPR()            Legendre coefficients of bottom bidirectional
C                     reflectivity (after inclusion of 2K+1 factor)

C   IPVT(LC*IQ)       Integer vector of pivot indices for LINPACK
C                     routines

C   KK(IQ,LC)         Eigenvalues of coeff. matrix in Eq. SS(7)

C   KCONV             Counter in azimuth convergence test

C   LAYRU(LU)         Computational layer in which user output level
C                     UTAU(LU) is located

C   LL(IQ,LC)         Constants of integration L in Eq. SC(1),
C                     obtained by solving scaled version of Eq. SC(5)

C   LYRCUT            TRUE, radiation is assumed zero below layer
C                     NCUT because of almost complete absorption

C   NAZ               Number of azimuthal components considered

C   NCUT              Computational layer number in which absorption
c                     optical depth first exceeds ABSCUT

C   OPRIM(LC)         Single scattering albedo after delta-M scaling

C   PASS1             TRUE on first entry, FALSE thereafter

C   PKAG(0:LC)        Integrated Planck function for internal emission

C   PSIX(IQ)          Sum just after square bracket in  Eq. SD(9)

C   RMU(IU,0:IQ)      Bottom-boundary bidirectional reflectivity for a
c                     given azimuthal component.  First index always
c                     refers to a user angle.  Second index:
C                     if zero, refers to incident beam angle UMU0;
C                     if non-zero, refers to a computational angle.

C   TAUC(0:LC)        Cumulative optical depth (un-delta-M-scaled)

C   TAUCPR(0:LC)      Cumulative optical depth (delta-M-scaled if
C                     DELTAM = TRUE, otherwise equal to TAUC)

C   TPLANK            Intensity emitted from top boundary

C   UUM(IU,LU,MAZIM)  Components of the intensity (u-super-M) when
C                     expanded in Fourier cosine series in azimuth angle

C   U0C(IQ,LU)        Azimuthally-averaged intensity

C   UTAUPR(LU)        Optical depths of user output levels in delta-M
C                     coordinates;  equal to  UTAU(LU) if no delta-M

C   WK()              scratch array

C   XR0(LC)           X-sub-zero in expansion of thermal source func-
C                     tion preceding Eq. SS(14) (has no mu-dependence)

C   XR1(LC)           X-sub-one in expansion of thermal source func-
C                     tion;  see  Eqs. SS(14-16)

C   YLM0(L)           Normalized associated Legendre polynomial
C                     of subscript L at the beam angle (not saved
C                     as function of superscipt M)

C   YLMC(L,IQ)        Normalized associated Legendre polynomial
C                     of subscript L at the computational angles
C                     (not saved as function of superscipt M)

C   YLMU(L,IU)        Normalized associated Legendre polynomial
C                     of subscript L at the user angles
C                     (not saved as function of superscipt M)

C   Z()               scratch array used in  SOLVE0,1  to solve a
C                     linear system for the constants of integration

C   Z0(IQ)            Solution vectors Z-sub-zero of Eq. SS(16)

C   Z0U(IU,LC)        Z-sub-zero in Eq. SS(16) interpolated to user
C                     angles from an equation derived from SS(16)

C   Z1(IQ)            Solution vectors Z-sub-one  of Eq. SS(16)

C   Z1U(IU,LC)        Z-sub-one in Eq. SS(16) interpolated to user
C                     angles from an equation derived from SS(16)

C   ZBEAM(IU,LC)      Particular solution for beam source

C   ZJ(IQ)            Right-hand side vector  X-sub-zero in
C                     Eq. SS(19), also the solution vector
C                     Z-sub-zero after solving that system

C   ZZ(IQ,LC)         Permanent storage for the beam source vectors ZJ

C   ZPLK0(IQ,LC)      Permanent storage for the thermal source
C                     vectors  Z0  obtained by solving  Eq. SS(16)

C   ZPLK1(IQ,LC)      Permanent storage for the thermal source
C                     vectors  Z1  obtained by solving  Eq. SS(16)


      LOGICAL LYRCUT, PASS1
      INTEGER IPVT( NNLYRI ), LAYRU( MXULV )
      real*8    AMB( MI,MI ), APB( MI,MI ),
     $        ARRAY( MXCMU,MXCMU ), B( NNLYRI ), BDR( MI,0:MI ),
     $        BEM( MI ), CBAND( MI9M2,NNLYRI ), CC( MXCMU,MXCMU ),
     $        CMU( MXCMU ), CWT( MXCMU ), EMU( MXUMU ), EVAL( MI ),
     $        EVECC( MXCMU, MXCMU ), EXPBEA( 0:MXCLY ), FLYR( MXCLY ),
     $        FLDN( MXULV ), FLDIR( MXULV ), GL( 0:MXCMU,MXCLY ),
     $        GC( MXCMU,MXCMU,MXCLY ), GU( MXUMU,MXCMU,MXCLY ),
     $        HLPR( 0:MXCMU ), KK( MXCMU,MXCLY ), LL( MXCMU,MXCLY ),
     $        OPRIM( MXCLY ), PHIRAD( MXPHI ), PKAG( 0:MXCLY ),
     $        PSIX( MXCMU ), RMU( MXUMU,0:MI ), TAUC( 0:MXCLY ),
     $        TAUCPR( 0:MXCLY ), U0C( MXCMU,MXULV ), UTAUPR( MXULV ),
     $        UUM( MXUMU,MXULV,0:MXCMU ), WK( MXCMU ), XR0( MXCLY ),
     $        XR1( MXCLY ), YLM0( 0:MXCMU ), YLMC( 0:MXCMU,MXCMU ),
     $        YLMU( 0:MXCMU,MXUMU ), Z( NNLYRI ), Z0( MXCMU ),
     $        Z0U( MXUMU,MXCLY ), Z1( MXCMU ),
     $        Z1U( MXUMU,MXCLY ), ZJ( MXCMU ), ZZ( MXCMU,MXCLY ),
     $        ZPLK0( MXCMU,MXCLY ), ZPLK1( MXCMU,MXCLY ),
     $        ZBEAM( MXUMU,MXCLY )
c---- ------------------------------------------------------------------
c==== >       lower case variables added
      real*8  cbandt( mi9m2,nnlyri ), cbands( mi9m2,nnlyri ),
     $        dummy0( mxcmu,mxcly ), dummy1( mxcmu,mxcly ),
     $        dumbem( mi ), usave( mxumu ), z0ums( mxumu,mxcly ),
     $        z1ums( mxumu,mxcly ), zbeamms( mxumu,mxcly )
      logical msflag
c-----------------------------------------------------------------------

      REAL R1MACH
      DOUBLE PRECISION   D1MACH
      DOUBLE PRECISION   AAD( MI,MI ), EVALD( MI ) , EVECCD( MI,MI ),
     $                   WKD( MXCMU )

      SAVE  PASS1, PI, EPSIL, RPD
      DATA  PASS1 / .TRUE. /

           pass1=.false.
         PI = 2. * dasin(1.0d0)
         EPSIL = 10.*d1mach(3)
         RPD = PI / 180.0
      IF ( PASS1 )  THEN
C                                ** Insert input values for self-test
C                                   NOTE: self-test must not use IBCND=1

         CALL  SLFTST( ACCUR, ALBEDO, BTEMP, DELTAM, DTAUC( 1 ), FBEAM,
     $                 FISOT, IBCND, LAMBER, NLYR, PLANK, NPHI,
     $                 NUMU, NSTR, NTAU, ONLYFL, PHI( 1 ), PHI0, PMOM,
     $                 PRNT, SSALB( 1 ), TEMIS, TEMPER, TTEMP, UMU( 1 ),
     $                 USRANG, USRTAU, UTAU( 1 ), UMU0, WVNMHI, WVNMLO,
     $                 .FALSE., DUM, DUM, DUM, DUM )
      END IF

   1  CONTINUE

c-----------------------------------------------------------------------
c=====>      lower case variables added
c            **  Initialize for multiple scattering source functions use
       if ( (.not.pass1).and. msflag)  then 
          msflag=.true.
          do 2 iu = 1, numu
2         usave(iu) = umu(iu)
          call  zeroit( zbeamms,mxumu*mxcly )
          call  zeroit( z0ums,mxumu*mxcly )
          call  zeroit( z1ums,mxumu*mxcly )
       end if
c-----------------------------------------------------------------------

      IF ( PRNT(1) )  WRITE( *,1010 )  HEADER

C                         ** Zero some arrays (not strictly necessary,
C                            but otherwise unused parts of arrays
C                            collect garbage)
      DO 10 I = 1, NNLYRI
         IPVT(I) = 0
10    CONTINUE
      CALL ZEROAL( MXCLY, XR0, XR1, TAUC(1),
     $             MXCMU, CMU, CWT, PSIX, EVAL, WK, Z0, Z1, ZJ,
     $             MXCMU+1, HLPR, YLM0,
     $             MXCMU**2, ARRAY, CC, EVECC, YLMC,
     $             (MXCMU+1)*MXUMU, YLMU,
     $             MI**2, AMB, APB,
     $             MXCMU*MXCLY, KK, LL, ZZ, ZPLK0, ZPLK1,
     $             MXUMU*MXCLY, Z0U, Z1U, ZBEAM,
     $             MXCMU**2*MXCLY, GC,
     $             MXUMU*MXCMU*MXCLY, GU,
     $             NNLYRI, Z )

C                                  ** Calculate cumulative optical depth
C                                     and dither single-scatter albedo
C                                     to improve numerical behavior of
C                                     eigenvalue/vector computation
      TAUC( 0 ) = 0.
      DO 20  LC = 1, NLYR
         IF( SSALB(LC).EQ.1.0 )  SSALB(LC) = 1.0 - EPSIL
         TAUC(LC) = TAUC(LC-1) + DTAUC(LC)
20    CONTINUE
C                                ** Check input dimensions and variables

      CALL  CHEKIN( NLYR, DTAUC, SSALB, PMOM, TEMPER, WVNMLO,
     $              WVNMHI, USRTAU, NTAU, UTAU, NSTR, USRANG,
     $              NUMU, UMU, NPHI, PHI, IBCND, FBEAM, UMU0,
     $              PHI0, FISOT, LAMBER, ALBEDO, HL, BTEMP,
     $              TTEMP, TEMIS, PLANK, ONLYFL, ACCUR, MAXCLY,
     $              MAXULV, MAXUMU, MAXCMU, MAXPHI, MXCLY,
     $              MXULV,  MXUMU,  MXCMU,  MXPHI, TAUC )

C                                 ** Perform various setup operations

      CALL  SETDIS( CMU, CWT, DELTAM, DTAUC, EXPBEA, FBEAM, FLYR,
     $              GL, HL, HLPR, IBCND, LAMBER, LAYRU, LYRCUT,
     $              MAXUMU, MAXCMU, MXCMU, NCUT, NLYR, NTAU, NN,
     $              NSTR, PLANK, NUMU, ONLYFL, OPRIM, PMOM, SSALB,
     $              TAUC, TAUCPR, UTAU, UTAUPR, UMU, UMU0, USRTAU,
     $              USRANG )

C                                             ** Print input information
      IF ( PRNT(1) )
     $     CALL PRTINP( NLYR, DTAUC, SSALB, PMOM, TEMPER, WVNMLO,
     $                  WVNMHI, NTAU, UTAU, NSTR, NUMU, UMU, NPHI, PHI,
     $                  IBCND, FBEAM, UMU0, PHI0, FISOT, LAMBER,
     $                  ALBEDO, HL, BTEMP, TTEMP, TEMIS, DELTAM, PLANK,
     $                  ONLYFL, ACCUR, FLYR, LYRCUT, OPRIM, TAUC,
     $                  TAUCPR, MAXCMU, PRNT(7) )


      IF ( IBCND.EQ.1 )  THEN
C                              ** Handle special case for getting albedo
c                                 and transmissivity of medium for many
c                                 beam angles at once

         CALL  ALBTRN( ALBEDO, AMB, APB, ARRAY, B, BDR, CBAND, CC,
     $                 CMU, CWT, EVAL, EVECC, GL, GC, GU, IPVT,
     $                 KK, LL, NLYR, NN, NSTR, NUMU, PRNT, TAUCPR,
     $                 UMU, U0U, WK, YLMC, YLMU, Z, AAD, EVALD,
     $                 EVECCD, WKD, MI, MI9M2, MAXULV, MAXUMU,
     $                 MXCMU, MXUMU, NNLYRI, ALBMED, TRNMED )
         RETURN
      ENDIF
C                                   ** Calculate Planck functions
      IF ( .NOT.PLANK )  THEN
         BPLANK = 0.0
         TPLANK = 0.0
         CALL  ZEROIT( PKAG, MXCLY+1 )
      ELSE
c                      **  Use different Planck functions PLKAVG or BBFN
c=====>    lower case variables added
           if ( pass1 .or. (.not.msflag) )  then
            TPLANK = TEMIS * PLKAVG( WVNMLO, WVNMHI, TTEMP )
            BPLANK =         PLKAVG( WVNMLO, WVNMHI, BTEMP )
            DO 40  LEV = 0, NLYR
40            PKAG( LEV ) = PLKAVG( WVNMLO, WVNMHI, TEMPER(LEV) )
           else
              tplank = temis * disbbfn( ttemp, wn0 )
              bplank =         disbbfn( btemp, wn0 )
              do 41  lev = 0, nlyr
            pkag( lev ) = disbbfn( temper(lev), wn0 )
41           continue
           end if
        END IF
C ========  BEGIN LOOP TO SUM AZIMUTHAL COMPONENTS OF INTENSITY  =======
C ========  (EQ STWJ 5)

      KCONV = 0
      NAZ = NSTR-1
C                                            ** azimuth-independent case

      IF ( FBEAM.EQ.0.0 .OR. (1.-UMU0).LT.1.E-5 .OR. ONLYFL .OR.
     $     (NUMU.EQ.1.AND.(1.-UMU(1)).LT.1.E-5 ) )
     $   NAZ = 0

      CALL  ZEROIT( UU, MAXUMU*MAXULV*MAXPHI )
      DO  200  MAZIM = 0, NAZ

      IF ( MAZIM.EQ.0 )  DELM0 = 1.0
      IF ( MAZIM.GT.0 )  DELM0 = 0.0
C                                  ** Get normalized associated Legendre
C                                     polynomials for incident beam
C                                     angle cosine
      IF ( FBEAM.GT.0.0 )
     $     CALL  LEPOLY( 1, MAZIM, MXCMU, NSTR-1, -UMU0, YLM0 )

C                                  ** Get normalized associated Legendre
C                                         polynomials for computational
C                                         and user polar angle cosines
      IF ( (.NOT.ONLYFL .AND. USRANG) )
     $     CALL  LEPOLY( NUMU, MAZIM, MXCMU, NSTR-1, UMU, YLMU )
         CALL  LEPOLY( NN,   MAZIM, MXCMU, NSTR-1, CMU, YLMC )

c=====> lower case variables added
       if ( (.not.pass1) .and. msflag )  then
           do 42 iu = 1, numu
42         umu(iu) = usave(iu)
           call  lepoly( numu, mazim, mxcmu, nstr-1, umu, ylmu )
        end if
C                       ** Evaluate normalized associated Legendre
C                          polynomials with negative  CMU  from those
C                          with positive  CMU; Dave/Armstrong Eq. (15)
      SGN  = - 1.0
      DO  50  L = MAZIM, NSTR-1
         SGN = - SGN
         DO  50  IQ = NN+1, NSTR
            YLMC( L,IQ ) = SGN * YLMC( L,IQ-NN )
 50   CONTINUE
C                                 ** Specify users bottom reflectivity
C                                    and emissivity properties
      IF ( .NOT.LYRCUT )
     $   CALL  SURFAC( ALBEDO, DELM0, FBEAM, HLPR, LAMBER,
     $                 MI, MAZIM, MXCMU, MXUMU, NN, NUMU, NSTR, ONLYFL,
     $                 UMU, USRANG, YLM0, YLMC, YLMU, BDR, EMU, BEM,
     $                 RMU )
C ===================  BEGIN LOOP ON COMPUTATIONAL LAYERS  =============

      DO 100  LC = 1, NCUT

C                        ** Solve eigenfunction problem in Eq. STWJ(8B);
C                           return eigenvalues and eigenvectors

         CALL  SOLEIG( AMB, APB, ARRAY, CMU, CWT, GL(0,LC), MI, MAZIM,
     $                 MXCMU, NN, NSTR, WK, YLMC, CC, EVECC, EVAL,
     $                 KK(1,LC), GC(1,1,LC), AAD, WKD, EVECCD, EVALD )
C                                  ** Calculate particular solutions of
C                                     Eq.SS(18) for incident beam source
         IF ( FBEAM.GT.0.0 )
     $        CALL  UPBEAM( ARRAY, CC, CMU, DELM0, FBEAM, GL(0,LC),
     $                      IPVT, MAZIM, MXCMU, NN, NSTR, PI, UMU0, WK,
     $                      YLM0, YLMC, ZJ, ZZ(1,LC) )
         IF ( PLANK .AND. MAZIM.EQ.0 ) THEN

C                              ** Calculate particular solutions of
C                                 Eq. SS(15) for thermal emission source

            DELTAT = TAUCPR(LC) - TAUCPR(LC-1)
            XR1( LC ) = 0.0

            IF ( DELTAT.GT.EPSIL ) XR1( LC ) = ( PKAG(LC) - PKAG(LC-1) )
     $                                       / DELTAT
            XR0( LC ) = PKAG(LC-1) - XR1(LC) * TAUCPR(LC-1)
            CALL UPISOT( ARRAY, CC, CMU, IPVT, MXCMU, NN, NSTR,
     $                     OPRIM(LC), WK, XR0(LC), XR1(LC), Z0, Z1,
     $                     ZPLK0(1,LC), ZPLK1(1,LC) )
         END IF
c=====>    lower case variables added
         IF ( (.NOT.ONLYFL .AND. USRANG) .or. msflag ) THEN

C                                            ** Interpolate eigenvectors
C                                               to user angles

            CALL  TERPEV( CWT, EVECC, GL(0,LC), GU(1,1,LC), MAZIM, 
     $                    MXCMU, MXUMU, NN, NSTR, NUMU, WK, YLMC, YLMU)

C                                            ** Interpolate source terms
C                                               to user angles

            CALL  TERPSO( CWT, DELM0, FBEAM, GL(0,LC), MAZIM,
     $                    MXCMU, PLANK, NUMU, NSTR, OPRIM(LC),
     $                    PI, YLM0, YLMC, YLMU, PSIX, XR0(LC), XR1(LC),
     $                    Z0, ZJ, ZBEAM(1,LC), Z0U(1,LC), Z1U(1,LC),
     $                    z0ums(1,lc), z1ums(1,lc), zbeamms(1,lc) )
c=====>    lower case variables added
         END IF
100   CONTINUE

C ===================  END LOOP ON COMPUTATIONAL LAYERS  ===============

C                      ** Set coefficient matrix of equations combining
C                         boundary and layer interface conditions
      CALL  SETMTX( BDR, CBAND, CMU, CWT, DELM0, GC, KK, LAMBER,
     $              LYRCUT, MI, MI9M2, MXCMU, NCOL, NCUT, NNLYRI,
     $              NN, NSTR, TAUCPR, WK )
c=====>        lower case variables added
        do 110 jk = 1, mi9m2
           do 110 ik = 1, nnlyri
           cbands(jk,ik)=cband(jk,ik)
           cbandt(jk,ik)=cband(jk,ik)
110     continue

C                      ** Solve for constants of integration in homo-
C                         geneous solution (general boundary conditions)
c               **  Combined solar and thermal sources for selftest case
      CALL  SOLVE0( B, BDR, BEM, BPLANK, CBAND, CMU, CWT, EXPBEA,
     $              FBEAM, FISOT, IPVT, LAMBER, LL, LYRCUT,
     $              MAZIM, MI, MI9M2, MXCMU, NCOL, NCUT, NN, NSTR,
     $              NNLYRI, PI, TPLANK, TAUCPR, UMU0, Z, ZZ,
     $              ZPLK0, ZPLK1 )

C                                  ** Compute upward and downward fluxes
      IF ( MAZIM.EQ.0 )
     $     CALL FLUXES( CMU, CWT, FBEAM, GC, KK, LAYRU, LL, LYRCUT,
     $                  MXCMU, MXULV, NCUT, NN, NSTR, NTAU, PI,
     $                  PRNT, SSALB, TAUCPR, UMU0, UTAU, UTAUPR,
     $                  XR0, XR1, ZZ, ZPLK0, ZPLK1, DFDT, FLUP,
     $                  FLDN, FLDIR, RFLDIR, RFLDN, UAVG, U0C, MAXULV )
c-----------------------------------------------------------------------
c            **  Compute multiple scattering source functions separately
c=====>      lower case variables added
       if ( (.not.pass1) .and. msflag )  then

           call zeroit( dummy0, mxcmu*mxcly )
c                            **  First call for solar MS source function

           if ( fbeam.gt.0. )  then
           call zeroit( dummy1, mxcmu*mxcly )
           call zeroit( dumbem, mi )
           call  solve0( b,bdr,dumbem, 0.d0, cbands, cmu, cwt, expbea,
     $                   fbeam, fisot, ipvt, lamber, ll, lyrcut,
     $                   mazim, mi, mi9m2, mxcmu, ncol, ncut, nn, nstr,
     $                   nnlyri, pi, 0.d0, taucpr, umu0, z, zz,
     $                   dummy0, dummy1 )
           call  mssolr( cmu, cwt, fbeam, gc, gu, kk, layru, ll,
     $                   lyrcut, maxumu, mxcmu, mxumu, ncut, nn,
     $                   nstr, ntau, numu, pi, taucpr, umu0, utau,
     $                   utaupr, zbeamms, zz, fdnsrt, s0cms )
           end if

c                         **  Second call for thermal MS source function

           if ( plank )  then
           call zeroit(dummy0,mxcmu*mxcly)
           do 111 jk = 0, mxcly
111        expbea(jk) = 0.0 
           call  solve0( b, bdr, bem, bplank, cbandt, cmu, cwt, expbea,
     $                   fbeam, fisot, ipvt, lamber, ll, lyrcut,
     $                   mazim, mi, mi9m2, mxcmu, ncol, ncut, nn, nstr,
     $                   nnlyri, pi, tplank, taucpr, umu0, z, dummy0,
     $                   zplk0, zplk1 )

           call  msthml( cmu, cwt, gc, gu, kk, layru, ll, lyrcut,
     $                   maxumu, mxcmu, mxumu, ncut, nn, nstr, ntau,
     $                   numu, oprim, pi, taucpr, umu0, utaupr,
     $                   xr0, xr1, z0ums, z1ums, zplk0, zplk1,
     $                   fdntrt, t0cms )

           end if

        end if
c-----------------------------------------------------------------------

           IF ( ONLYFL )  THEN
         IF( MAXUMU.GE.NSTR )  THEN
C                                         ** Save azim-avgd intensities
C                                            at quadrature angles
            DO 120 LU = 1, NTAU
               DO 120 IQ = 1, NSTR
                  U0U( IQ,LU ) = U0C( IQ,LU )
120         CONTINUE
         ELSE
               CALL  ZEROIT( U0U, MAXUMU*MAXULV )
         ENDIF
         GO TO 210
      ENDIF

      IF ( USRANG ) THEN
C                          ** Compute azimuthal intensity
C                                        components at user angles
         CALL  USRINT( BPLANK, CMU, CWT, DELM0, EMU, EXPBEA,
     $                 FBEAM, FISOT, GC, GU, KK, LAMBER, LAYRU, LL,
     $                 LYRCUT, MAZIM, MXCMU, MXULV, MXUMU, NCUT,
     $                 NLYR, NN, NSTR, PLANK, NUMU, NTAU, PI, RMU,
     $                 TAUCPR, TPLANK, UMU, UMU0, UTAUPR, WK,
     $                 ZBEAM, Z0U, Z1U, ZZ, ZPLK0, ZPLK1, UUM )

      ELSE
C                                     ** Compute azimuthal intensity
C                                        components at quadrature angles

         CALL  CMPINT( FBEAM, GC, KK, LAYRU, LL, LYRCUT, MAZIM,
     $                 MXCMU, MXULV, MXUMU, NCUT, NN, NSTR,
     $                 PLANK, NTAU, TAUCPR, UMU0, UTAUPR,
     $                 ZZ, ZPLK0, ZPLK1, UUM )
      END IF

      IF( MAZIM.EQ.0 ) THEN

         DO  140  J = 1, NPHI
            PHIRAD( J ) = RPD * ( PHI(J) - PHI0 )
 140     CONTINUE
C                               ** Save azimuthally averaged intensities
         DO 160  LU = 1, NTAU
            DO 160  IU = 1, NUMU
               U0U( IU,LU ) = UUM( IU,LU,0 )
 160     CONTINUE
C                              ** Print azimuthally averaged intensities
C                                 at user angles
         IF ( PRNT(4) )
     $        CALL PRAVIN( UMU, NUMU, MAXUMU, UTAU, NTAU, U0U )

      END IF
C                                ** Increment intensity by current
C                                   azimuthal component (Fourier
C                                   cosine series);  Eq SD(2)
      AZERR = 0.0
      DO 180  J = 1, NPHI
         COSPHI = COS( MAZIM * PHIRAD(J) )
         DO 180  LU = 1, NTAU
            DO 180  IU = 1, NUMU
               AZTERM = UUM( IU,LU,MAZIM ) * COSPHI
               UU( IU,LU,J ) = UU( IU,LU,J ) + AZTERM
               AZERR = dmax1( RATIO( dabs(AZTERM), dabs(UU(IU,LU,J)) ),
     $                        AZERR )
180   CONTINUE
      IF ( AZERR.LE.ACCUR )  KCONV = KCONV + 1
      IF ( KCONV.GE.2 )      GOTO 210

200   CONTINUE

C ===================  END LOOP ON AZIMUTHAL COMPONENTS  ===============


C                                                 ** Print intensities

 210  IF ( PRNT(5) .AND. .NOT.ONLYFL )
     $     CALL  PRTINT( UU, UTAU, NTAU, UMU, NUMU, PHI, NPHI,
     $                   MAXULV, MAXUMU )


      IF ( PASS1 )  THEN
C                                    ** Compare test case results with
c                                       correct answers and abort if bad

         CALL SLFTST( ACCUR, ALBEDO, BTEMP, DELTAM, DTAUC( 1 ), FBEAM,
     $                FISOT, IBCND, LAMBER, NLYR, PLANK, NPHI,
     $                NUMU, NSTR, NTAU, ONLYFL, PHI( 1 ), PHI0, PMOM,
     $                PRNT, SSALB( 1 ), TEMIS, TEMPER, TTEMP, UMU( 1 ),
     $                USRANG, USRTAU, UTAU( 1 ), UMU0, WVNMHI, WVNMLO,
     $                .TRUE., FLUP( 1 ), RFLDIR( 1 ), RFLDN( 1 ),
     $                UU( 1,1,1 ) )
         PASS1 = .FALSE.
         GO TO 1
      END IF


                sfdntrt=fdntrt 
                sfdnsrt=fdnsrt 



      RETURN

1010  FORMAT ( ////, 1X, 120('*'), /, 25X,
     $  'DISCRETE ORDINATES RADIATIVE TRANSFER PROGRAM, VERSION 1.0',
     $  /, 1X, A, /, 1X, 120('*') )
      END
      SUBROUTINE  ALBTRN( ALBEDO, AMB, APB, ARRAY, B, BDR, CBAND, CC,
     $                    CMU, CWT, EVAL, EVECC, GL, GC, GU, IPVT,
     $                    KK, LL, NLYR, NN, NSTR, NUMU, PRNT, TAUCPR,
     $                    UMU, U0U, WK, YLMC, YLMU, Z, AAD, EVALD,
     $                    EVECCD, WKD, MI, MI9M2, MAXULV, MAXUMU,
     $                    MXCMU, MXUMU, NNLYRI, ALBMED, TRNMED )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )


C        SPECIAL CASE TO GET ONLY ALBEDO AND TRANSMISSIVITY
C        OF ENTIRE MEDIUM AS A FUNCTION OF INCIDENT BEAM ANGLE
C        (MANY SIMPLIFICATIONS BECAUSE BOUNDARY CONDITION IS JUST
C        ISOTROPIC ILLUMINATION, THERE ARE NO THERMAL SOURCES, AND
C        PARTICULAR SOLUTIONS DO NOT NEED TO BE COMPUTED).  SEE
C        REF. S2 AND REFERENCES THEREIN FOR THEORY.

C        ROUTINES CALLED:  ALTRIN, LEPOLY, PRALTR, SETMTX, SOLVE1,
C                          SOLEIG, ZEROIT

      LOGICAL  PRNT(*)
      INTEGER  NLYR, NUMU, NSTR
      real*8     UMU(*), U0U( MAXUMU,* )

      INTEGER IPVT(*)
      real*8    ALBMED(*), AMB( MI,* ), APB( MI,*), ARRAY( MXCMU,* ),
     $        B(*), BDR( MI,0:* ), CBAND( MI9M2,* ), CC( MXCMU,* ),
     $        CMU(*), CWT(*), EVAL(*), EVECC( MXCMU,* ),
     $        GL( 0:MXCMU,* ), GC( MXCMU,MXCMU,* ), GU( MXUMU,MXCMU,* ),
     $        KK( MXCMU,* ), LL( MXCMU,* ), TAUCPR( 0:* ), TRNMED(*),
     $        WK(*), YLMC( 0:MXCMU,* ), YLMU( 0:MXCMU,* ), Z(*)
      DOUBLE PRECISION   AAD( MI,* ), EVALD(*) , EVECCD( MI,* ), WKD(*)

      LOGICAL  LAMBER, LYRCUT


C                    ** SET DISORT VARIABLES THAT ARE IGNORED IN THIS
C                    ** SPECIAL CASE BUT ARE NEEDED BELOW IN ARGUMENT
C                    ** LISTS OF SUBROUTINES SHARED WITH GENERAL CASE
      NCUT = NLYR
      LYRCUT = .FALSE.
      FISOT = 1.0
      LAMBER = .TRUE.

      MAZIM = 0
      DELM0 = 1.0
C                          ** GET LEGENDRE POLYNOMIALS FOR COMPUTATIONAL
C                          ** AND USER POLAR ANGLE COSINES

      CALL  LEPOLY( NUMU, MAZIM, MXCMU, NSTR-1, UMU, YLMU )
      CALL  LEPOLY( NN,   MAZIM, MXCMU, NSTR-1, CMU, YLMC )

C                       ** EVALUATE LEGENDRE POLYNOMIALS WITH NEGATIVE
C                       ** -CMU- FROM THOSE WITH POSITIVE -CMU-;
C                       ** DAVE/ARMSTRONG EQ. (15)
      SGN  = -1.0
      DO  5  L = MAZIM, NSTR-1
         SGN = - SGN
         DO  5  IQ = NN+1, NSTR
            YLMC( L,IQ ) = SGN * YLMC( L,IQ-NN )
    5 CONTINUE
C                                  ** ZERO BOTTOM REFLECTIVITY
C                                  ** (-ALBEDO- IS USED ONLY IN ANALYTIC
C                                  ** FORMULAE INVOLVING ALBEDO = 0
C                                  ** SOLUTIONS; EQS 16-17 OF REF S2)
      CALL  ZEROIT( BDR, MI*(MI+1) )


C ===================  BEGIN LOOP ON COMPUTATIONAL LAYERS  =============

      DO 100  LC = 1, NLYR

C                        ** SOLVE EIGENFUNCTION PROBLEM IN EQ. STWJ(8B)

         CALL  SOLEIG( AMB, APB, ARRAY, CMU, CWT, GL(0,LC), MI, MAZIM,
     $                 MXCMU, NN, NSTR, WK, YLMC, CC, EVECC, EVAL,
     $                 KK(1,LC), GC(1,1,LC), AAD, WKD, EVECCD, EVALD)

C                          ** INTERPOLATE EIGENVECTORS TO USER ANGLES

         CALL  TERPEV( CWT, EVECC, GL(0,LC), GU(1,1,LC), MAZIM, MXCMU,
     $                 MXUMU, NN, NSTR, NUMU, WK, YLMC, YLMU )
100   CONTINUE

C ===================  END LOOP ON COMPUTATIONAL LAYERS  ===============

C                      ** SET COEFFICIENT MATRIX OF EQUATIONS COMBINING
C                      ** BOUNDARY AND LAYER INTERFACE CONDITIONS

      CALL  SETMTX( BDR, CBAND, CMU, CWT, DELM0, GC, KK, LAMBER,
     $              LYRCUT, MI, MI9M2, MXCMU, NCOL, NCUT, NNLYRI,
     $              NN, NSTR, TAUCPR, WK )

      CALL  ZEROIT( U0U, MAXUMU*MAXULV )

      NHOM = 2
      IF( NLYR.EQ.1 )  NHOM = 1
      SPHALB = 0.0
      SPHTRN = 0.0
      DO 200  IHOM = 1, NHOM
C                             ** SOLVE FOR CONSTANTS OF INTEGRATION IN
C                             ** HOMOGENEOUS SOLUTION FOR ILLUMINATION
C                             ** FROM TOP (IHOM=1), THEN BOTTOM (IHOM=2)

         CALL  SOLVE1( B, CBAND, FISOT, IHOM, IPVT, LL, MI9M2, MXCMU,
     $                 NCOL, NLYR, NN, NNLYRI, NSTR, Z )

C                             ** COMPUTE AZIMUTHALLY-AVERAGED INTENSITY
C                             ** AT USER ANGLES; GIVES ALBEDO IF MULTI-
C                             ** LAYER (EQ. 9 OF REF S2); GIVES BOTH
C                             ** ALBEDO AND TRANSMISSIVITY IF SINGLE
C                             ** LAYER (EQS. 3-4 OF REF S2)

         CALL  ALTRIN( GU, KK, LL, MXCMU, MXUMU, MAXUMU, NLYR,
     $                 NN, NSTR, NUMU, TAUCPR, UMU, U0U, WK )

         IF ( IHOM.EQ.1 )  THEN
C                                   ** SAVE ALBEDOS;  FLIP TRANSMISSIV.
C                                   ** END OVER END TO CORRESPOND TO
C                                   ** POSITIVE -UMU- INST. OF NEGATIVE
            DO 120  IU = 1, NUMU/2
               ALBMED(IU) = U0U( IU + NUMU/2, 1 )
               IF( NLYR.EQ.1 )  TRNMED(IU) = U0U( NUMU/2+1-IU, 2 )
     $                          + EXP( - TAUCPR(NLYR) / UMU(IU+NUMU/2) )
120         CONTINUE
C                                    ** GET SPHERICAL ALBEDO AND, FOR 1
C                                    ** LAYER, SPHERICAL TRANSMISSIVITY
            IF( ALBEDO.GT.0.0 )
     $          CALL SPALTR( CMU, CWT, GC, KK, LL, MXCMU, NLYR,
     $                       NN, NSTR, TAUCPR, SPHALB, SPHTRN )

         ELSE IF ( IHOM.EQ.2 )  THEN
C                                      ** SAVE TRANSMISSIVITIES
            DO 140  IU = 1, NUMU/2
               TRNMED(IU) = U0U( IU + NUMU/2, 1 )
     $                      + EXP( - TAUCPR(NLYR) / UMU(IU+NUMU/2) )
140         CONTINUE
C                             ** GET SPHERICAL ALBEDO AND TRANSMISSIVITY
            IF( ALBEDO.GT.0.0 )
     $          CALL SPALTR( CMU, CWT, GC, KK, LL, MXCMU, NLYR,
     $                       NN, NSTR, TAUCPR, SPHTRN, SPHALB )
         END IF
200   CONTINUE

      IF ( ALBEDO.GT.0.0 )  THEN
C                                ** REF. S2, EQS. 16-17 (THESE EQS. HAVE
C                                ** A SIMPLE PHYSICAL INTERPRETATION
C                                ** LIKE THAT OF THE DOUBLING EQS.)
         DO 220  IU = 1, NUMU
            ALBMED(IU) = ALBMED(IU) + ( ALBEDO / (1.-ALBEDO*SPHALB) )
     $                                * SPHTRN * TRNMED(IU)
            TRNMED(IU) = TRNMED(IU) + ( ALBEDO / (1.-ALBEDO*SPHALB) )
     $                                * SPHALB * TRNMED(IU)
220      CONTINUE
      END IF
C                          ** RETURN -UMU- TO ALL POSITIVE VALUES, TO
C                          ** AGREE WITH ORDERING IN -ALBMED,TRNMED-
      NUMU = NUMU / 2
      DO 230  IU = 1, NUMU
        UMU(IU) = UMU(IU+NUMU)
 230  CONTINUE

      IF ( PRNT(6) )  CALL  PRALTR( UMU, NUMU, ALBMED, TRNMED )

      RETURN
      END
      SUBROUTINE  ALTRIN( GU, KK, LL, MXCMU, MXUMU, MAXUMU, NLYR,
     $                    NN, NSTR, NUMU, TAUCPR, UMU, U0U, WK )
c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )

C       COMPUTES AZIMUTHALLY-AVERAGED INTENSITY AT TOP AND BOTTOM
C       OF MEDIUM (RELATED TO ALBEDO AND TRANSMISSION OF MEDIUM BY
C       RECIPROCITY PRINCIPLES;  SEE REF S2).  USER POLAR ANGLES ARE
C       USED AS INCIDENT BEAM ANGLES. (THIS IS A VERY SPECIALIZED
C       VERSION OF 'USRINT')

C       ** NOTE **  USER INPUT VALUES OF -UMU- (ASSUMED POSITIVE) ARE
C                   TEMPORARILY IN UPPER LOCATIONS OF  -UMU-  AND
C                   CORRESPONDING NEGATIVES ARE IN LOWER LOCATIONS
C                   (THIS MAKES -GU- COME OUT RIGHT).  I.E. THE CONTENTS
C                   OF THE TEMPORARY -UMU- ARRAY ARE:

C                     -UMU(NUMU),..., -UMU(1), UMU(1),..., UMU(NUMU)

C   I N P U T    V A R I A B L E S:

C       GU     :  EIGENVECTORS INTERPOLATED TO USER POLAR ANGLES
C                 (i.e., g IN EQ. SC(1) )
C       KK     :  EIGENVALUES OF COEFF. MATRIX IN EQ. SS(7)
C       LL     :  CONSTANTS OF INTEGRATION IN EQ. SC(1), OBTAINED
C                 BY SOLVING SCALED VERSION OF EQ. SC(5);
C                 EXPONENTIAL TERM OF EQ. SC(12) NOT INCLUDED
C       NN     :  ORDER OF DOUBLE-GAUSS QUADRATURE (NSTR/2)
C       TAUCPR :  CUMULATIVE OPTICAL DEPTH (DELTA-M-SCALED)
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)

C   O U T P U T    V A R I A B L E:

C       U0U  :    DIFFUSE AZIMUTHALLY-AVERAGED INTENSITY AT TOP AND
C                 BOTTOM OF MEDIUM (DIRECTLY TRANSMITTED COMPONENT,
C                 CORRESPONDING TO -BNDINT- IN 'USRINT', IS OMITTED).

C   I N T E R N A L    V A R I A B L E S:

C       DTAU   :  OPTICAL DEPTH OF A COMPUTATIONAL LAYER
C       PALINT :  NON-BOUNDARY-FORCED INTENSITY COMPONENT
C       UTAUPR :  OPTICAL DEPTHS OF USER OUTPUT LEVELS (DELTA-M SCALED)
C       WK     :  SCRATCH VECTOR FOR SAVING 'EXP' EVALUATIONS
C       ALL THE EXPONENTIAL FACTORS (i.e., EXP1, EXPN,... etc.)
C       COME FROM THE SUBSTITUTION OF CONSTANTS OF INTEGRATION IN
C       EQ. SC(12) INTO EQS. S1(8-9).  ALL HAVE NEGATIVE ARGUMENTS.
C+---------------------------------------------------------------------+

      real*8     UTAUPR( 2 )
      real*8     GU( MXUMU,MXCMU,* ), KK( MXCMU,* ), LL( MXCMU,* ), MU,
     $         TAUCPR( 0:* ), UMU(*), U0U( MAXUMU,* ), WK(*)


      UTAUPR(1) = 0.0
      UTAUPR(2) = TAUCPR( NLYR )
      DO 100  LU = 1, 2
         IF ( LU.EQ.1 )  THEN
            IUMIN = NUMU / 2 + 1
            IUMAX = NUMU
            SGN = 1.0
         ELSE
            IUMIN = 1
            IUMAX = NUMU / 2
            SGN = - 1.0
         END IF
C                                   ** LOOP OVER POLAR ANGLES AT WHICH
C                                   ** ALBEDOS/TRANSMISSIVITIES DESIRED
C                                   ** ( UPWARD ANGLES AT TOP BOUNDARY,
C                                   ** DOWNWARD ANGLES AT BOTTOM )
         DO 50  IU = IUMIN, IUMAX
            MU = UMU(IU)
C                                     ** INTEGRATE FROM TOP TO BOTTOM
C                                     ** COMPUTATIONAL LAYER
            PALINT = 0.0
            DO 30  LC = 1, NLYR

               DTAU = TAUCPR(LC) - TAUCPR(LC-1)
               EXP1 =  dexp( (UTAUPR(LU) - TAUCPR(LC-1)) / MU )
               EXP2 =  dexp( (UTAUPR(LU) - TAUCPR( LC )) / MU )

C                                      ** -KK- IS NEGATIVE
               DO 20  IQ = 1, NN
                  WK(IQ) = dexp( KK(IQ,LC) * DTAU )
                  DENOM = 1.0 + MU * KK(IQ,LC)
                  IF ( dabs(DENOM).LT.0.0001 ) THEN
C                                                   ** L'HOSPITAL LIMIT
                     EXPN = DTAU / MU * EXP2
                  ELSE
                     EXPN = ( EXP1 * WK(IQ) - EXP2 ) * SGN / DENOM
                  END IF
                  PALINT = PALINT + GU(IU,IQ,LC) * LL(IQ,LC) * EXPN
20             CONTINUE
C                                      ** -KK- IS POSITIVE
               DO 21  IQ = NN+1, NSTR
                  DENOM = 1.0 + MU * KK(IQ,LC)
                  IF ( dabs(DENOM).LT.0.0001 ) THEN
                     EXPN = - DTAU / MU * EXP1
                  ELSE
                     EXPN = ( EXP1 - EXP2 * WK(NSTR+1-IQ) ) *SGN / DENOM
                  END IF
                  PALINT = PALINT + GU(IU,IQ,LC) * LL(IQ,LC) * EXPN
21             CONTINUE

30          CONTINUE

            U0U( IU, LU ) = PALINT

 50      CONTINUE
100   CONTINUE

      RETURN
      END
      SUBROUTINE  ASYMTX( A, EVEC, EVAL, M, IA, IEVEC, IER, WK,
     $                    AAD, EVECD, EVALD, WKD )
c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )

C    =======  D O U B L E    P R E C I S I O N    V E R S I O N  ======

c       Solves eigenfunction problem for real asymmetric matrix
c       for which it is known a priori that the eigenvalues are real.

c       This is an adaptation of a subroutine EIGRF in the IMSL
c       library to use real instead of complex arithmetic, accounting
c       for the known fact that the eigenvalues and eigenvectors in
c       the discrete ordinate solution are real.  Other changes include
c       putting all the called subroutines in-line, deleting the
c       performance index calculation, updating many DO-loops
c       to Fortran77, and in calculating the machine precision
c       TOL instead of specifying it in a data statement.

c       EIGRF is based primarily on EISPACK routines.  The matrix is
c       first balanced using the parlett-reinsch algorithm.  Then
c       the Martin-Wilkinson algorithm is applied.

C       References:
C          Dongarra, J. and C. Moler, EISPACK -- A Package for Solving
C             Matrix Eigenvalue Problems, in Cowell, ed., 1984:
C             Sources and Development of Mathematical Software,
C             Prentice-Hall, Englewood Cliffs, NJ
C         Parlett and Reinsch, 1969: Balancing a Matrix for Calculation
C             of Eigenvalues and Eigenvectors, Num. Math. 13, 293-304
C         Wilkinson, J., 1965: The Algebraic Eigenvalue Problem,
C             Clarendon Press, Oxford

C   I N P U T    V A R I A B L E S:

C        A    :  input asymmetric matrix, destroyed after solved
C        M    :  order of  A
C       IA    :  first dimension of  A
C    IEVEC    :  first dimension of  EVEC

C   O U T P U T    V A R I A B L E S:

C       EVEC  :  (unnormalized) eigenvectors of  A 
C                   ( column J corresponds to EVAL(J) )

C       EVAL  :  (unordered) eigenvalues of  A ( dimension at least M )

C       IER   :  if .NE. 0, signals that EVAL(IER) failed to converge;
C                   in that case eigenvalues IER+1,IER+2,...,M  are
C                   correct but eigenvalues 1,...,IER are set to zero.

C   S C R A T C H   V A R I A B L E S:

C       WK    :  WORK AREA ( DIMENSION AT LEAST 2*M )
C       AAD   :  DOUBLE PRECISION STAND-IN FOR -A-
C       EVECD :  DOUBLE PRECISION STAND-IN FOR -EVEC-
C       EVALD :  DOUBLE PRECISION STAND-IN FOR -EVAL-
C       WKD   :  DOUBLE PRECISION STAND-IN FOR -WK-
C+---------------------------------------------------------------------+

c      IMPLICIT DOUBLE PRECISION ( A-H, O-Z )
      real*8              A( IA,* ),   WK(*),  EVAL(*),  EVEC( IEVEC,* )
      DOUBLE PRECISION  AAD( IA,* ), WKD(*), EVALD(*), EVECD( IA,* )
      DOUBLE PRECISION  D1MACH
      LOGICAL           NOCONV, NOTLAS
      DATA     C1 / 0.4375D0 /, C2/ 0.5D0 /, C3/ 0.75D0 /, C4/ 0.95D0 /,
     $         C5/ 16.D0 /, C6/ 256.D0 /, ZERO / 0.D0 /, ONE / 1.D0 /


      IER = 0
      TOL = d1mach(3)
      IF ( M.LT.1 .OR. IA.LT.M .OR. IEVEC.LT.M )
     $     CALL ERRMSG( 'ASYMTX--bad input variable(s)', .TRUE. )

C                           ** HANDLE 1X1 AND 2X2 SPECIAL CASES
      IF ( M.EQ.1 )  THEN
         EVAL(1) = A(1,1)
         EVEC(1,1) = 1.0
         RETURN

      ELSE IF ( M.EQ.2 )  THEN
         DISCRI = ( A(1,1) - A(2,2) )**2 + 4. * A(1,2) * A(2,1)
         IF ( DISCRI.LT.0.0 )
     $        CALL ERRMSG( 'ASYMTX--COMPLEX EVALS IN 2X2 CASE', .TRUE. )
         SGN = 1.0
         IF ( A(1,1).LT.A(2,2) )  SGN = - 1.0
         EVAL(1) = 0.5 * ( A(1,1) + A(2,2) + SGN*dsqrt(DISCRI) )
         EVAL(2) = 0.5 * ( A(1,1) + A(2,2) - SGN*dsqrt(DISCRI) )
         EVEC(1,1) = 1.0
         EVEC(2,2) = 1.0
         IF ( A(1,1).EQ.A(2,2) .AND. (A(2,1).EQ.0.0.OR.A(1,2).EQ.0.0) )
     $        THEN
       RNORM=dabs(A(1,1))+dabs(A(1,2))+dabs(A(2,1))+dabs(A(2,2))
            W = TOL * RNORM
            EVEC(2,1) = A(2,1) / W
            EVEC(1,2) = - A(1,2) / W
         ELSE
            EVEC(2,1) = A(2,1) / ( EVAL(1) - A(2,2) )
            EVEC(1,2) = A(1,2) / ( EVAL(2) - A(1,1) )
         ENDIF
         RETURN
      END IF
C                               ** PUT S.P. MATRIX INTO D.P. MATRIX
      DO 1  J = 1, M
         DO 1  K = 1, M
            AAD( J,K ) = DBLE( A(J,K) )
    1 CONTINUE
C                                        ** INITIALIZE OUTPUT VARIABLES
      DO 20 I = 1, M
         EVALD(I) = ZERO
         DO 10 J = 1, M
            EVECD(I,J) = ZERO
10       CONTINUE
         EVECD(I,I) = ONE
20    CONTINUE
C                  ** BALANCE THE INPUT MATRIX AND REDUCE ITS NORM BY
C                  ** DIAGONAL SIMILARITY TRANSFORMATION STORED IN WK;
C                  ** THEN SEARCH FOR ROWS ISOLATING AN EIGENVALUE
C                  ** AND PUSH THEM DOWN
      RNORM = ZERO
      L  = 1
      K  = M

30    KKK = K
         DO 70  J = KKK, 1, -1
            ROW = ZERO
            DO 40 I = 1, K
               IF ( I.NE.J ) ROW = ROW + dabs( AAD(J,I) )
40          CONTINUE
            IF ( ROW.EQ.ZERO ) THEN
               WKD(K) = J
               IF ( J.NE.K ) THEN
                  DO 50 I = 1, K
                     REPL   = AAD(I,J)
                     AAD(I,J) = AAD(I,K)
                     AAD(I,K) = REPL
50                CONTINUE
                  DO 60 I = L, M
                     REPL   = AAD(J,I)
                     AAD(J,I) = AAD(K,I)
                     AAD(K,I) = REPL
60                CONTINUE
               END IF
               K = K - 1
               GO TO 30
            END IF
70       CONTINUE
C                                     ** SEARCH FOR COLUMNS ISOLATING AN
C                                       ** EIGENVALUE AND PUSH THEM LEFT
80    LLL = L
         DO 120 J = LLL, K
            COL = ZERO
            DO 90 I = L, K
               IF ( I.NE.J ) COL = COL + dabs( AAD(I,J) )
90          CONTINUE
            IF ( COL.EQ.ZERO ) THEN
               WKD(L) = J
               IF ( J.NE.L ) THEN
                  DO 100 I = 1, K
                     REPL   = AAD(I,J)
                     AAD(I,J) = AAD(I,L)
                     AAD(I,L) = REPL
100               CONTINUE
                  DO 110 I = L, M
                     REPL   = AAD(J,I)
                     AAD(J,I) = AAD(L,I)
                     AAD(L,I) = REPL
110               CONTINUE
               END IF
               L = L + 1
               GO TO 80
            END IF
120      CONTINUE
C                           ** BALANCE THE SUBMATRIX IN ROWS L THROUGH K
      DO 130 I = L, K
         WKD(I) = ONE
130   CONTINUE

140   NOCONV = .FALSE.
         DO 200 I = L, K
            COL = ZERO
            ROW = ZERO
            DO 150 J = L, K
               IF ( J.NE.I ) THEN
                  COL = COL + dabs( AAD(J,I) )
                  ROW = ROW + dabs( AAD(I,J) )
               END IF
150         CONTINUE
            F = ONE
            G = ROW / C5
            H = COL + ROW
160         IF ( COL.LT.G ) THEN
               F   = F * C5
               COL = COL * C6
               GO TO 160
            END IF
            G = ROW * C5
170         IF ( COL.GE.G ) THEN
               F   = F / C5
               COL = COL / C6
               GO TO 170
            END IF
C                                                         ** NOW BALANCE
            IF ( (COL+ROW)/F .LT. C4*H ) THEN
               WKD(I)  = WKD(I) * F
               NOCONV = .TRUE.
               DO 180 J = L, M
                  AAD(I,J) = AAD(I,J) / F
180            CONTINUE
               DO 190 J = 1, K
                  AAD(J,I) = AAD(J,I) * F
190            CONTINUE
            END IF
200      CONTINUE

      IF ( NOCONV ) GO TO 140
C                                  ** IS -A- ALREADY IN HESSENBERG FORM?
      IF ( K-1 .LT. L+1 ) GO TO 350
C                                   ** TRANSFER -A- TO A HESSENBERG FORM
      DO 290 N = L+1, K-1
         H        = ZERO
         WKD(N+M) = ZERO
         SCALE    = ZERO
C                                                        ** SCALE COLUMN
         DO 210 I = N, K
            SCALE = SCALE + dabs(AAD(I,N-1))
210      CONTINUE
         IF ( SCALE.NE.ZERO ) THEN
            DO 220 I = K, N, -1
               WKD(I+M) = AAD(I,N-1) / SCALE
               H = H + WKD(I+M)**2
220         CONTINUE
            G = - SIGN( dsqrt(H), WKD(N+M) )
            H = H - WKD(N+M) * G
            WKD(N+M) = WKD(N+M) - G
C                                                 ** FORM (I-(U*UT)/H)*A
            DO 250 J = N, M
               F = ZERO
               DO 230  I = K, N, -1
                  F = F + WKD(I+M) * AAD(I,J)
230            CONTINUE
               DO 240 I = N, K
                  AAD(I,J) = AAD(I,J) - WKD(I+M) * F / H
240            CONTINUE
250         CONTINUE
C                                    ** FORM (I-(U*UT)/H)*A*(I-(U*UT)/H)
            DO 280 I = 1, K
               F = ZERO
               DO 260  J = K, N, -1
                  F = F + WKD(J+M) * AAD(I,J)
260            CONTINUE
               DO 270 J = N, K
                  AAD(I,J) = AAD(I,J) - WKD(J+M) * F / H
270            CONTINUE
280         CONTINUE
            WKD(N+M)  = SCALE * WKD(N+M)
            AAD(N,N-1) = SCALE * G
         END IF
290   CONTINUE

      DO 340  N = K-2, L, -1
         N1 = N + 1
         N2 = N + 2
         F  = AAD(N1,N)
         IF ( F.NE.ZERO ) THEN
            F  = F * WKD(N1+M)
            DO 300 I = N2, K
               WKD(I+M) = AAD(I,N)
300         CONTINUE
            IF ( N1.LE.K ) THEN
               DO 330 J = 1, M
                  G = ZERO
                  DO 310 I = N1, K
                     G = G + WKD(I+M) * EVECD(I,J)
310               CONTINUE
                  G = G / F
                  DO 320 I = N1, K
                     EVECD(I,J) = EVECD(I,J) + G * WKD(I+M)
320               CONTINUE
330            CONTINUE
            END IF
         END IF
340   CONTINUE

350   CONTINUE
      N = 1
      DO 370 I = 1, M
         DO 360 J = N, M
            RNORM = RNORM + dabs(AAD(I,J))
360      CONTINUE
         N = I
         IF ( I.LT.L .OR. I.GT.K ) EVALD(I) = AAD(I,I)
370   CONTINUE
      N = K
      T = ZERO
C                                         ** SEARCH FOR NEXT EIGENVALUES
380   IF ( N.LT.L ) GO TO 530
      IN = 0
      N1 = N - 1
      N2 = N - 2
C                          ** LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT
390   CONTINUE
      DO 400 I = L, N
         LB = N+L - I
         IF ( LB.EQ.L ) GO TO 410
         S = dabs( AAD(LB-1,LB-1) ) + dabs( AAD(LB,LB) )
         IF ( S.EQ.ZERO ) S = RNORM
         IF ( dabs(AAD(LB,LB-1)) .LE. TOL*S ) GO TO 410
400   CONTINUE

410   X = AAD(N,N)
      IF ( LB.EQ.N ) THEN
C                                        ** ONE EIGENVALUE FOUND
         AAD(N,N)  = X + T
         EVALD(N) = AAD(N,N)
         N = N1
         GO TO 380
      END IF

      Y = AAD(N1,N1)
      W = AAD(N,N1) * AAD(N1,N)
      IF ( LB.EQ.N1 ) THEN
C                                        ** TWO EIGENVALUES FOUND
         P = (Y-X) * C2
         Q = P**2 + W
         Z = dsqrt( dabs(Q) )
         AAD(N,N) = X + T
         X = AAD(N,N)
         AAD(N1,N1) = Y + T
C                                        ** REAL PAIR
         Z = P + dsign(Z,P)
         EVALD(N1) = X + Z
         EVALD(N)  = EVALD(N1)
         IF ( Z.NE.ZERO ) EVALD(N) = X - W / Z
         X = AAD(N,N1)
C                                  ** EMPLOY SCALE FACTOR IN CASE
C                                  ** X AND Z ARE VERY SMALL
         R = dsqrt( X*X + Z*Z )
         P = X / R
         Q = Z / R
C                                             ** ROW MODIFICATION
         DO 420 J = N1, M
            Z = AAD(N1,J)
            AAD(N1,J) = Q * Z + P * AAD(N,J)
            AAD(N,J)  = Q * AAD(N,J) - P * Z
420      CONTINUE
C                                             ** COLUMN MODIFICATION
         DO 430 I = 1, N
            Z = AAD(I,N1)
            AAD(I,N1) = Q * Z + P * AAD(I,N)
            AAD(I,N)  = Q * AAD(I,N) - P * Z
430      CONTINUE
C                                          ** ACCUMULATE TRANSFORMATIONS
         DO 440 I = L, K
            Z = EVECD(I,N1)
            EVECD(I,N1) = Q * Z + P * EVECD(I,N)
            EVECD(I,N)  = Q * EVECD(I,N) - P * Z
440      CONTINUE

         N = N2
         GO TO 380
      END IF

      IF ( IN.EQ.30 ) THEN
C                    ** NO CONVERGENCE AFTER 30 ITERATIONS; SET ERROR
C                    ** INDICATOR TO THE INDEX OF THE CURRENT EIGENVALUE
         IER = N
         GO TO 670
      END IF
C                                                          ** FORM SHIFT
      IF ( IN.EQ.10 .OR. IN.EQ.20 ) THEN
         T = T + X
         DO 450 I = L, N
            AAD(I,I) = AAD(I,I) - X
450      CONTINUE
         S = dabs(AAD(N,N1)) + dabs(AAD(N1,N2))
         X = C3 * S
         Y = X
         W = - C1 * S**2
      END IF

      IN = IN + 1
C                ** LOOK FOR TWO CONSECUTIVE SMALL SUB-DIAGONAL ELEMENTS

      DO 460 J = LB, N2
         I = N2+LB - J
         Z = AAD(I,I)
         R = X - Z
         S = Y - Z
         P = ( R * S - W ) / AAD(I+1,I) + AAD(I,I+1)
         Q = AAD(I+1,I+1) - Z - R - S
         R = AAD(I+2,I+1)
         S = dabs(P) + dabs(Q) + dabs(R)
         P = P / S
         Q = Q / S
         R = R / S
         IF ( I.EQ.LB ) GO TO 470
         UU = dabs( AAD(I,I-1) ) * ( dabs(Q) + dabs(R) )
         VV=dabs(P)*(dabs(AAD(I-1,I-1))+dabs(Z)+dabs(AAD(I+1,I+1)))
         IF ( UU .LE. TOL*VV ) GO TO 470
460   CONTINUE

470   CONTINUE
      AAD(I+2,I) = ZERO
      DO 480 J = I+3, N
         AAD(J,J-2) = ZERO
         AAD(J,J-3) = ZERO
480   CONTINUE

C             ** DOUBLE QR STEP INVOLVING ROWS K TO N AND COLUMNS M TO N

      DO 520 KA = I, N1
         NOTLAS = KA.NE.N1
         IF ( KA.EQ.I ) THEN
            S = dsign( dsqrt( P*P + Q*Q + R*R ), P )
            IF ( LB.NE.I ) AAD(KA,KA-1) = - AAD(KA,KA-1)
         ELSE
            P = AAD(KA,KA-1)
            Q = AAD(KA+1,KA-1)
            R = ZERO
            IF ( NOTLAS ) R = AAD(KA+2,KA-1)
            X = dabs(P) + dabs(Q) + dabs(R)
            IF ( X.EQ.ZERO ) GO TO 520
            P = P / X
            Q = Q / X
            R = R / X
            S = dsign( dsqrt( P*P + Q*Q + R*R ), P )
            AAD(KA,KA-1) = - S * X
         END IF
         P = P + S
         X = P / S
         Y = Q / S
         Z = R / S
         Q = Q / P
         R = R / P
C                                                    ** ROW MODIFICATION
         DO 490 J = KA, M
            P = AAD(KA,J) + Q * AAD(KA+1,J)
            IF ( NOTLAS ) THEN
               P = P + R * AAD(KA+2,J)
               AAD(KA+2,J) = AAD(KA+2,J) - P * Z
            END IF
            AAD(KA+1,J) = AAD(KA+1,J) - P * Y
            AAD(KA,J)   = AAD(KA,J)   - P * X
490      CONTINUE
C                                                 ** COLUMN MODIFICATION
         DO 500 II = 1, MIN0(N,KA+3)
            P = X * AAD(II,KA) + Y * AAD(II,KA+1)
            IF ( NOTLAS ) THEN
               P = P + Z * AAD(II,KA+2)
               AAD(II,KA+2) = AAD(II,KA+2) - P * R
            END IF
            AAD(II,KA+1) = AAD(II,KA+1) - P * Q
            AAD(II,KA)   = AAD(II,KA) - P
500      CONTINUE
C                                          ** ACCUMULATE TRANSFORMATIONS
         DO 510 II = L, K
            P = X * EVECD(II,KA) + Y * EVECD(II,KA+1)
            IF ( NOTLAS ) THEN
               P = P + Z * EVECD(II,KA+2)
               EVECD(II,KA+2) = EVECD(II,KA+2) - P * R
            END IF
            EVECD(II,KA+1) = EVECD(II,KA+1) - P * Q
            EVECD(II,KA)   = EVECD(II,KA) - P
510      CONTINUE

520   CONTINUE
      GO TO 390
C                     ** ALL EVALS FOUND, NOW BACKSUBSTITUTE REAL VECTOR
530   CONTINUE
      IF ( RNORM.NE.ZERO ) THEN
         DO 560  N = M, 1, -1
            N2 = N
            AAD(N,N) = ONE
            DO 550  I = N-1, 1, -1
               W = AAD(I,I) - EVALD(N)
               IF ( W.EQ.ZERO ) W = TOL * RNORM
               R = AAD(I,N)
               DO 540 J = N2, N-1
                  R = R + AAD(I,J) * AAD(J,N)
540            CONTINUE
               AAD(I,N) = - R / W
               N2 = I
550         CONTINUE
560      CONTINUE
C                      ** END BACKSUBSTITUTION VECTORS OF ISOLATED EVALS

         DO 580 I = 1, M
            IF ( I.LT.L .OR. I.GT.K ) THEN
               DO 570 J = I, M
                  EVECD(I,J) = AAD(I,J)
570            CONTINUE
            END IF
580      CONTINUE
C                                   ** MULTIPLY BY TRANSFORMATION MATRIX
         IF ( K.NE.0 ) THEN
            DO 600  J = M, L, -1
               DO 600 I = L, K
                  Z = ZERO
                  DO 590 N = L, MIN0(J,K)
                     Z = Z + EVECD(I,N) * AAD(N,J)
590               CONTINUE
                  EVECD(I,J) = Z
600         CONTINUE
         END IF

      END IF

      DO 620 I = L, K
         DO 620 J = 1, M
            EVECD(I,J) = EVECD(I,J) * WKD(I)
620   CONTINUE
C                           ** INTERCHANGE ROWS IF PERMUTATIONS OCCURRED
      DO 640  I = L-1, 1, -1
         J = WKD(I)
         IF ( I.NE.J ) THEN
            DO 630 N = 1, M
               REPL       = EVECD(I,N)
               EVECD(I,N) = EVECD(J,N)
               EVECD(J,N) = REPL
630         CONTINUE
         END IF
640   CONTINUE

      DO 660 I = K+1, M
         J = WKD(I)
         IF ( I.NE.J ) THEN
            DO 650 N = 1, M
               REPL       = EVECD(I,N)
               EVECD(I,N) = EVECD(J,N)
               EVECD(J,N) = REPL
650         CONTINUE
         END IF
660   CONTINUE
C                         ** PUT RESULTS INTO OUTPUT ARRAYS
  670 CONTINUE
      DO 680 J = 1, M
         EVAL( J ) = EVALD(J)
         DO 680 K = 1, M
            EVEC( J,K ) = EVECD(J,K)
680   CONTINUE

      RETURN
      END
      SUBROUTINE  CHEKIN( NLYR, DTAUC, SSALB, PMOM, TEMPER, WVNMLO,
     $                    WVNMHI, USRTAU, NTAU, UTAU, NSTR, USRANG,
     $                    NUMU, UMU, NPHI, PHI, IBCND, FBEAM, UMU0,
     $                    PHI0, FISOT, LAMBER, ALBEDO, HL, BTEMP,
     $                    TTEMP, TEMIS, PLANK, ONLYFL, ACCUR, MAXCLY,
     $                    MAXULV, MAXUMU, MAXCMU, MAXPHI, MXCLY,
     $                    MXULV,  MXUMU,  MXCMU,  MXPHI, TAUC )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C           Checks the input dimensions and variables

      LOGICAL  WRTBAD, WRTDIM
      LOGICAL  LAMBER, PLANK, ONLYFL, USRANG, USRTAU, InpErr
      INTEGER  IBCND, MAXCLY, MAXUMU, MAXULV, MAXCMU, MAXPHI, NLYR,
     $         NUMU, NSTR, NPHI, NTAU, MXCMU, MXUMU, MXPHI, MXCLY,
     $         MXULV
      real*8     ACCUR, ALBEDO, BTEMP, DTAUC( MAXCLY ), FBEAM, FISOT,
     $         HL( 0:MAXCMU ), PHI( MAXPHI ), PMOM( 0:MAXCMU, MAXCLY ),
     $         PHI0, SSALB( MAXCLY ), TEMPER( 0:MAXCLY ), TEMIS, TTEMP,
     $         WVNMLO, WVNMHI, UMU( MAXUMU ), UMU0, UTAU( MAXULV ),
     $         TAUC( 0:* )


      InpErr = .FALSE.
      IF ( NLYR.LT.1 ) InpErr = WRTBAD( 'NLYR' )
      IF ( NLYR.GT.MAXCLY ) InpErr = WRTBAD( 'MAXCLY' )

      DO 10  LC = 1, NLYR
         IF ( DTAUC(LC).LT.0.0 ) InpErr = WRTBAD( 'DTAUC' )
         IF ( SSALB(LC).LT.0.0 .OR. SSALB(LC).GT.1.0 )
     $        InpErr = WRTBAD( 'SSALB' )
         IF ( PLANK .AND. IBCND.NE.1 )  THEN
            IF( LC.EQ.1 .AND. TEMPER(0).LT.0.0 )
     $          InpErr = WRTBAD( 'TEMPER' )
            IF( TEMPER(LC).LT.0.0 ) InpErr = WRTBAD( 'TEMPER' )
         ENDIF
         DO 5  K = 0, NSTR
            IF( PMOM(K,LC).LT.-1.0 .OR. PMOM(K,LC).GT.1.0 )
     $          InpErr = WRTBAD( 'PMOM' )
 5       CONTINUE
10    CONTINUE

      IF ( IBCND.EQ.1 )  THEN
         IF ( MAXULV.LT.2 ) InpErr = WRTBAD( 'MAXULV' )
      ELSE IF ( USRTAU )  THEN
         IF ( NTAU.LT.1 ) InpErr = WRTBAD( 'NTAU' )
         IF ( MAXULV.LT.NTAU ) InpErr = WRTBAD( 'MAXULV' )
         DO 20  LU = 1, NTAU
         IF(dabs(UTAU(LU)-TAUC(NLYR)).LE.1.E-4)UTAU(LU)=TAUC(NLYR)
            IF( UTAU(LU).LT.0.0 .OR. UTAU(LU).GT.TAUC(NLYR) )
     $          InpErr = WRTBAD( 'UTAU' )
20       CONTINUE
      ELSE
         IF ( MAXULV.LT.NLYR+1 ) InpErr = WRTBAD( 'MAXULV' )
      END IF

      IF ( NSTR.LT.2 .OR. MOD(NSTR,2).NE.0 ) InpErr = WRTBAD( 'NSTR' )
      IF ( NSTR.GT.MAXCMU ) InpErr = WRTBAD( 'MAXCMU' )

      IF ( USRANG )  THEN
         IF ( NUMU.LT.0 ) InpErr = WRTBAD( 'NUMU' )
         IF ( .NOT.ONLYFL .AND. NUMU.EQ.0 ) InpErr = WRTBAD( 'NUMU'  )
         IF ( NUMU.GT.MAXUMU ) InpErr = WRTBAD( 'MAXUMU' )
         IF ( IBCND.EQ.1 .AND. 2*NUMU.GT.MAXUMU )
     $        InpErr = WRTBAD( 'MAXUMU' )
         DO 30  IU = 1, NUMU
            IF( UMU(IU).LT.-1.0 .OR. UMU(IU).GT.1.0 .OR. UMU(IU).EQ.0.0)
     $           InpErr = WRTBAD( 'UMU' )
            IF( IBCND.EQ.1 .AND. UMU(IU).LT.0.0 )
     $           InpErr = WRTBAD( 'UMU' )
            IF( IU.GT.1 .AND. UMU(IU).LT.UMU(IU-1) )
     $           InpErr = WRTBAD( 'UMU' )
30       CONTINUE
      ELSE
         IF( MAXUMU.LT.NSTR ) InpErr = WRTBAD( 'MAXUMU' )
      END IF

      IF ( .NOT.ONLYFL .AND. IBCND.NE.1 )  THEN
         IF ( NPHI.LE.0 ) InpErr = WRTBAD( 'NPHI' )
         IF ( NPHI.GT.MAXPHI ) InpErr = WRTBAD( 'MAXPHI' )
         DO 40  J = 1, NPHI
            IF ( PHI(J).LT.0.0 .OR. PHI(J).GT.360.0 )
     $           InpErr = WRTBAD( 'PHI' )
40       CONTINUE
      END IF

      IF ( IBCND.LT.0 .OR. IBCND.GT.1 ) InpErr = WRTBAD( 'IBCND' )
      IF ( IBCND.EQ.0 )  THEN
         IF ( FBEAM.LT.0.0 ) InpErr = WRTBAD( 'FBEAM' )
         IF ( FBEAM.GT.0.0 .AND. ( UMU0.LE.0.0 .OR. UMU0.GT.1.0 ) )
     $        InpErr = WRTBAD( 'UMU0' )
         IF ( FBEAM.GT.0.0 .AND. ( PHI0.LT.0.0 .OR. PHI0.GT.360.0 ) )
     $        InpErr = WRTBAD( 'PHI0' )
         IF ( FISOT.LT.0.0 ) InpErr = WRTBAD( 'FISOT' )
         IF ( LAMBER )  THEN
            IF ( ALBEDO.LT.0.0 .OR. ALBEDO.GT.1.0 )
     $           InpErr = WRTBAD( 'ALBEDO' )
         ELSE
C                    ** Make sure flux albedo at dense mesh of incident
C                       angles does not assume unphysical values

            DO 50  RMU = 0.0, 1.0, 0.01
               FLXALB = DREF( RMU, HL, NSTR )
               IF ( FLXALB.LT.0.0 .OR. FLXALB.GT.1.0 )
     $              InpErr = WRTBAD( 'HL' )
50          CONTINUE
         ENDIF

      ELSE IF ( IBCND.EQ.1 )  THEN
         IF ( ALBEDO.LT.0.0 .OR. ALBEDO.GT.1.0 )
     $        InpErr = WRTBAD( 'ALBEDO' )
      END IF

      IF ( PLANK .AND. IBCND.NE.1 )  THEN
         IF ( WVNMLO.LT.0.0 .OR. WVNMHI.LE.WVNMLO )
     $        InpErr = WRTBAD( 'WVNMLO,HI' )
         IF ( TEMIS.LT.0.0 .OR. TEMIS.GT.1.0 )
     $        InpErr = WRTBAD( 'TEMIS' )
         IF ( BTEMP.LT.0.0 ) InpErr = WRTBAD( 'BTEMP' )
         IF ( TTEMP.LT.0.0 ) InpErr = WRTBAD( 'TTEMP' )
      END IF

      IF ( ACCUR.LT.0.0 .OR. ACCUR.GT.1.E-2 )
     $     InpErr = WRTBAD( 'ACCUR' )

      IF ( MXCLY.LT.NLYR ) InpErr = WRTDIM( 'MXCLY', NLYR )
      IF ( IBCND.NE.1 )  THEN
         IF ( USRTAU .AND. MXULV.LT.NTAU )
     $        InpErr = WRTDIM( 'MXULV', NTAU )
         IF ( .NOT.USRTAU .AND. MXULV.LT.NLYR+1 )
     $        InpErr = WRTDIM( 'MXULV', NLYR+1 )
      ELSE
         IF ( MXULV.LT.2 ) InpErr = WRTDIM( 'MXULV', 2 )
      END IF
      IF ( MXCMU.LT.NSTR ) InpErr = WRTDIM( 'MXCMU', NSTR )
      IF ( USRANG .AND. MXUMU.LT.NUMU )
     $     InpErr = WRTDIM( 'MXUMU', NUMU )
      IF ( USRANG .AND. IBCND.EQ.1 .AND. MXUMU.LT.2*NUMU )
     $     InpErr = WRTDIM( 'MXUMU', NUMU )
      IF ( .NOT.USRANG .AND. MXUMU.LT.NSTR )
     $      InpErr = WRTDIM( 'MXUMU', NSTR )
      IF ( .NOT.ONLYFL .AND. IBCND.NE.1 .AND. MXPHI.LT.NPHI )
     $      InpErr = WRTDIM( 'MXPHI', NPHI )

      IF ( InpErr )
     $   CALL ERRMSG( 'DISORT--input and/or dimension errors', .True. )

cjv 10/95 comment out this error message that doesn't apply to modtran
cjv      IF ( PLANK )  THEN
cjv         DO 100  LC = 1, NLYR
cjv         IF ( dabs(TEMPER(LC)-TEMPER(LC-1)) .GT. 20.0 )
cjv     $        CALL ERRMSG( 'CHEKIN--vertical temperature step may'
cjv     $                  // ' be too large for good accuracy', .False. )
cjv 100      CONTINUE
cjv      END IF
cjv ^

      RETURN
      END
      SUBROUTINE  CMPINT( FBEAM, GC, KK, LAYRU, LL, LYRCUT, MAZIM,
     $                    MXCMU, MXULV, MXUMU, NCUT, NN, NSTR,
     $                    PLANK, NTAU, TAUCPR, UMU0, UTAUPR,
     $                    ZZ, ZPLK0, ZPLK1, UUM )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C       CALCULATES THE FOURIER INTENSITY COMPONENTS AT THE QUADRATURE
C       ANGLES FOR AZIMUTHAL EXPANSION TERMS (MAZIM) IN EQ. SD(2)

C    I N P U T    V A R I A B L E S:

C       KK      :  EIGENVALUES OF COEFF. MATRIX IN EQ. SS(7)
C       GC      :  EIGENVECTORS AT POLAR QUADRATURE ANGLES, SC(1)
C       LL      :  CONSTANTS OF INTEGRATION IN EQ. SC(1), OBTAINED
C                  BY SOLVING SCALED VERSION OF EQ. SC(5);
C                  EXPONENTIAL TERM OF EQ. SC(12) NOT INCLUDED
C       LYRCUT  :  LOGICAL FLAG FOR TRUNCATION OF COMPUT. LAYER
C       MAZIM   :  ORDER OF AZIMUTHAL COMPONENT
C       NCUT    :  NUMBER OF COMPUTATIONAL LAYER WHERE ABSORPTION
C                  OPTICAL DEPTH EXCEEDS -ABSCUT-
C       NN      :  ORDER OF DOUBLE-GAUSS QUADRATURE (NSTR/2)
C       TAUCPR  :  CUMULATIVE OPTICAL DEPTH (DELTA-M-SCALED)
C       UTAUPR  :  OPTICAL DEPTHS OF USER OUTPUT LEVELS IN DELTA-M
C                  COORDINATES;  EQUAL TO -UTAU- IF NO DELTA-M
C       ZZ      :  BEAM SOURCE VECTORS IN EQ. SS(19)
C       ZPLK0   :  THERMAL SOURCE VECTORS -Z0-, BY SOLVING EQ. SS(16)
C       ZPLK1   :  THERMAL SOURCE VECTORS -Z1-, BY SOLVING EQ. SS(16)
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)

C    O U T P U T   V A R I A B L E S:

C       UUM     :  FOURIER COMPONENTS OF THE INTENSITY IN EQ.  SD(12)
C                   ( AT POLAR QUADRATURE ANGLES )

C    I N T E R N A L   V A R I A B L E S:

C       FACT    :  EXP( - UTAUPR / UMU0 )
C       ZINT    :  INTENSITY OF M=0 CASE, IN EQ. SC(1)
C+----------------------------------------------------------------------

       LOGICAL  LYRCUT, PLANK
       INTEGER  LAYRU(*)
       real*8     UUM( MXUMU, MXULV, 0:* )
       real*8     GC( MXCMU,MXCMU,* ), KK( MXCMU,* ), LL( MXCMU,* ),
     $          TAUCPR( 0:* ), UTAUPR(*), ZZ( MXCMU, *),
     $          ZPLK0( MXCMU,* ), ZPLK1( MXCMU,* )


C                                                  ** ZERO OUTPUT ARRAY
       CALL ZEROIT( UUM, MXUMU*MXULV*(MXCMU + 1) )

C                                       ** LOOP OVER USER LEVELS
       DO 100  LU = 1, NTAU

          LYU = LAYRU(LU)
          IF ( LYRCUT .AND. LYU.GT.NCUT )  GO TO 100

          DO 20  IQ = 1, NSTR
             ZINT = 0.0
             DO 10  JQ = 1, NN
               ZINT = ZINT + GC(IQ,JQ,LYU) * LL(JQ,LYU) *
     $                 dexp( - KK(JQ,LYU)*(UTAUPR(LU) - TAUCPR(LYU)) )
10           CONTINUE
             DO 11  JQ = NN+1, NSTR
                ZINT = ZINT + GC(IQ,JQ,LYU) * LL(JQ,LYU) *
     $              dexp( - KK(JQ,LYU)*(UTAUPR(LU) - TAUCPR(LYU-1)) )
11           CONTINUE

             UUM(IQ,LU,MAZIM) = ZINT
             IF ( FBEAM.GT.0.0 )
     $            UUM(IQ,LU,MAZIM) = ZINT + ZZ(IQ,LYU)
     $                                    * dexp( - UTAUPR(LU) / UMU0 )
             IF ( PLANK .AND. MAZIM.EQ.0 )
     $            UUM(IQ,LU,MAZIM) = UUM(IQ,LU,MAZIM) + ZPLK0(IQ,LYU) +
     $                             ZPLK1(IQ,LYU) * UTAUPR(LU)
20        CONTINUE

100   CONTINUE

      RETURN
      END
      real*8 FUNCTION  DREF( MU, HL, NSTR )
c           insert for double precision - NORTH
            implicit double precision ( a-h, o-z )
C        EXACT FLUX ALBEDO FOR GIVEN ANGLE OF INCIDENCE, GIVEN
C        A BIDIRECTIONAL REFLECTIVITY CHARACTERIZED BY ITS
C        LEGENDRE COEFFICIENTS ( NOTE** THESE WILL ONLY AGREE
C        WITH BOTTOM-BOUNDARY ALBEDOS CALCULATED BY 'DISORT' IN
C        THE LIMIT AS NUMBER OF STREAMS GO TO INFINITY, BECAUSE
C        'DISORT' EVALUATES THE INTEGRAL 'CL' ONLY APPROXIMATELY,
C        BY QUADRATURE, WHILE THIS ROUTINE CALCULATES IT EXACTLY. )

C      INPUT :   MU     COSINE OF INCIDENCE ANGLE
C                HL     LEGENDRE COEFFICIENTS OF BIDIRECTIONAL REF'Y
C              NSTR     NUMBER OF ELEMENTS OF 'HL' TO CONSIDER

C      INTERNAL VARIABLES (P-SUB-L IS THE L-TH LEGENDRE POLYNOMIAL) :

C              CL    INTEGRAL FROM 0 TO 1 OF  MU * P-SUB-L(MU)
C                       (VANISHES FOR  L = 3, 5, 7, ... )
C              PL    P-SUB-L
C            PLM1    P-SUB-(L-1)
C            PLM2    P-SUB-(L-2)

      PARAMETER  ( MAXTRM = 100 )
      LOGICAL      PASS1
      real*8         MU, HL( 0:* )
      real*8         C( MAXTRM )
      SAVE  PASS1, C
      DATA  PASS1 / .TRUE. /


      IF ( PASS1 )  THEN
         PASS1 = .FALSE.
         CL = 0.125
         C(2) = 10. * CL
         DO 1  L = 4, MAXTRM, 2
            CL = - CL * (L-3) / (L+2)
            C(L) = 2. * (2*L+1) * CL
    1    CONTINUE
      END IF

      IF ( NSTR.GT.MAXTRM )  CALL
     $     ERRMSG( 'DREF--PARAMETER MAXTRM TOO SMALL', .TRUE. )

      DREF = HL(0) - 2.*HL(1) * MU
      PLM2 = 1.0
      PLM1 = - MU
      DO 10  L = 2, NSTR-1
C                                ** LEGENDRE POLYNOMIAL RECURRENCE

         PL = ( (2*L-1) * (-MU) * PLM1 - (L-1) * PLM2 ) / L
         IF( MOD(L,2).EQ.0 )  DREF = DREF + C(L) * HL(L) * PL
         PLM2 = PLM1
         PLM1 = PL
   10 CONTINUE

      RETURN
      END
      SUBROUTINE  FLUXES( CMU, CWT, FBEAM, GC, KK, LAYRU, LL, LYRCUT,
     $                    MXCMU, MXULV, NCUT, NN, NSTR, NTAU, PI,
     $                    PRNT, SSALB, TAUCPR, UMU0, UTAU, UTAUPR,
     $                    XR0, XR1, ZZ, ZPLK0, ZPLK1, DFDT, FLUP,
     $                    FLDN, FLDIR, RFLDIR, RFLDN, UAVG, U0C,
     $                    MAXULV )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C       CALCULATES THE RADIATIVE FLUXES, MEAN INTENSITY, AND FLUX
C       DERIVATIVE WITH RESPECT TO OPTICAL DEPTH FROM THE M=0 INTENSITY
C       COMPONENTS (THE AZIMUTHALLY-AVERAGED INTENSITY)

C    I N P U T     V A R I A B L E S:

C       CMU      :  ABSCISSAE FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       CWT      :  WEIGHTS FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       GC       :  EIGENVECTORS AT POLAR QUADRATURE ANGLES, SC(1)
C       KK       :  EIGENVALUES OF COEFF. MATRIX IN EQ. SS(7)
C       LAYRU    :  LAYER NUMBER OF USER LEVEL -UTAU-
C       LL       :  CONSTANTS OF INTEGRATION IN EQ. SC(1), OBTAINED
C                   BY SOLVING SCALED VERSION OF EQ. SC(5);
C                   EXPONENTIAL TERM OF EQ. SC(12) NOT INCLUDED
C       LYRCUT   :  LOGICAL FLAG FOR TRUNCATION OF COMPUT. LAYER
C       NN       :  ORDER OF DOUBLE-GAUSS QUADRATURE (NSTR/2)
C       NCUT     :  NUMBER OF COMPUTATIONAL LAYER WHERE ABSORPTION
C                     OPTICAL DEPTH EXCEEDS -ABSCUT-
C       TAUCPR   :  CUMULATIVE OPTICAL DEPTH (DELTA-M-SCALED)
C       UTAUPR   :  OPTICAL DEPTHS OF USER OUTPUT LEVELS IN DELTA-M
C                     COORDINATES;  EQUAL TO  -UTAU- IF NO DELTA-M
C       XR0      :  EXPANSION OF THERMAL SOURCE FUNCTION IN EQ. SS(14)
C       XR1      :  EXPANSION OF THERMAL SOURCE FUNCTION EQS. SS(16)
C       ZZ       :  BEAM SOURCE VECTORS IN EQ. SS(19)
C       ZPLK0    :  THERMAL SOURCE VECTORS -Z0-, BY SOLVING EQ. SS(16)
C       ZPLK1    :  THERMAL SOURCE VECTORS -Z1-, BY SOLVING EQ. SS(16)
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)

C   O U T P U T     V A R I A B L E S:

C       U0C      :  AZIMUTHALLY AVERAGED INTENSITIES
C                   ( AT POLAR QUADRATURE ANGLES )
C       (RFLDIR, RFLDN, FLUP, DFDT, UAVG ARE 'DISORT' OUTPUT VARIABLES)

C   I N T E R N A L       V A R I A B L E S:

C       DIRINT   :  DIRECT INTENSITY ATTENUATED
C       FDNTOT   :  TOTAL DOWNWARD FLUX (DIRECT + DIFFUSE)
C       FLDIR    :  DIRECT-BEAM FLUX (DELTA-M SCALED)
C       FLDN     :  DIFFUSE DOWN-FLUX (DELTA-M SCALED)
C       FNET     :  NET FLUX (TOTAL-DOWN - DIFFUSE-UP)
C       FACT     :  EXP( - UTAUPR / UMU0 )
C       PLSORC   :  PLANCK SOURCE FUNCTION (THERMAL)
C       ZINT     :  INTENSITY OF m = 0 CASE, IN EQ. SC(1)
C+---------------------------------------------------------------------+

      LOGICAL LYRCUT, PRNT(*)
      real*8 DFDT(*),FLUP(*),FLDIR(*),FLDN(*),RFLDIR(*),RFLDN(* ),
     $        U0C( MXCMU,MXULV ), UAVG(*)
      INTEGER LAYRU(*)
      real*8    CMU(*), CWT(*), GC( MXCMU,MXCMU,* ), KK( MXCMU,* ),
     $        LL( MXCMU,* ), SSALB(*), TAUCPR( 0:* ),
     $        UTAU(*), UTAUPR(*), XR0(*), XR1(*), ZZ( MXCMU,* ),
     $        ZPLK0( MXCMU,* ), ZPLK1( MXCMU,* )


      IF ( PRNT(2) )  WRITE( *,1010 )
C                                          ** ZERO DISORT OUTPUT ARRAYS
      CALL  ZEROIT( U0C, MXULV*MXCMU )
      CALL  ZEROIT( RFLDIR, MAXULV )
      CALL  ZEROIT( FLDIR,  MXULV )
      CALL  ZEROIT( RFLDN,  MAXULV )
      CALL  ZEROIT( FLDN,   MXULV )
      CALL  ZEROIT( FLUP,   MAXULV )
      CALL  ZEROIT( UAVG,   MAXULV )
      CALL  ZEROIT( DFDT,   MAXULV )
C                                        ** LOOP OVER USER LEVELS
      DO 100  LU = 1, NTAU

         LYU = LAYRU(LU)

         IF ( LYRCUT .AND. LYU.GT.NCUT ) THEN
C                                                ** NO RADIATION REACHES
C                                                ** THIS LEVEL
            FDNTOT = 0.0
            FNET   = 0.0
            PLSORC = 0.0
            GO TO 90
         END IF

         IF ( FBEAM.GT.0.0 )  THEN
            FACT  = dexp( - UTAUPR(LU) / UMU0 )
            DIRINT = FBEAM * FACT
            FLDIR(  LU ) = UMU0 * ( FBEAM * FACT )
            RFLDIR( LU ) = UMU0 * FBEAM * dexp( - UTAU( LU ) / UMU0 )
         ELSE
            DIRINT = 0.0
            FLDIR(  LU ) = 0.0
            RFLDIR( LU ) = 0.0
         END IF

         DO 20  IQ = 1, NN

            ZINT = 0.0
            DO 10  JQ = 1, NN
               ZINT = ZINT + GC(IQ,JQ,LYU) * LL(JQ,LYU) *
     $                dexp( - KK(JQ,LYU) * (UTAUPR(LU) - TAUCPR(LYU)) )
10          CONTINUE
            DO 11  JQ = NN+1, NSTR
               ZINT = ZINT + GC(IQ,JQ,LYU) * LL(JQ,LYU) *
     $           dexp( - KK(JQ,LYU) * (UTAUPR(LU) - TAUCPR(LYU-1)) )
11          CONTINUE

            U0C( IQ,LU ) = ZINT
            IF ( FBEAM.GT.0.0 )  U0C( IQ,LU ) = ZINT + ZZ(IQ,LYU) * FACT
            U0C( IQ,LU ) = U0C( IQ,LU ) + ZPLK0(IQ,LYU)
     $                     + ZPLK1(IQ,LYU) * UTAUPR(LU)
            UAVG(LU) = UAVG(LU) + CWT(NN+1-IQ) * U0C( IQ,LU )
            FLDN(LU) = FLDN(LU) + CWT(NN+1-IQ)*CMU(NN+1-IQ) * U0C(IQ,LU)
20       CONTINUE

         DO 40  IQ = NN+1, NSTR

            ZINT = 0.0
            DO 30  JQ = 1, NN
               ZINT = ZINT + GC(IQ,JQ,LYU) * LL(JQ,LYU) *
     $                dexp( - KK(JQ,LYU) * (UTAUPR(LU) - TAUCPR(LYU)) )
30          CONTINUE
            DO 31  JQ = NN+1, NSTR
               ZINT = ZINT + GC(IQ,JQ,LYU) * LL(JQ,LYU) *
     $           dexp( - KK(JQ,LYU) * (UTAUPR(LU) - TAUCPR(LYU-1)) )
31          CONTINUE

            U0C( IQ,LU ) = ZINT
            IF ( FBEAM.GT.0.0 )  U0C( IQ,LU ) = ZINT + ZZ(IQ,LYU) * FACT
            U0C( IQ,LU ) = U0C( IQ,LU ) + ZPLK0(IQ,LYU)
     $                     + ZPLK1(IQ,LYU) * UTAUPR(LU)
            UAVG(LU) = UAVG(LU) + CWT(IQ-NN) * U0C( IQ,LU )
            FLUP(LU) = FLUP(LU) + CWT(IQ-NN) * CMU(IQ-NN) * U0C( IQ,LU )
40       CONTINUE

         FLUP( LU )  = 2.0 * PI * FLUP( LU )
         FLDN( LU )  = 2.0 * PI * FLDN( LU )
         FDNTOT = FLDN( LU ) + FLDIR( LU )
         FNET   = FDNTOT - FLUP( LU )
         RFLDN( LU ) = FDNTOT - RFLDIR( LU )
         UAVG( LU ) = ( 2.0 * PI * UAVG(LU) + DIRINT ) / ( 4.*PI )
         PLSORC =  XR0(LYU) + XR1(LYU) * UTAUPR(LU)
         DFDT( LU ) = ( 1.0-SSALB(LYU) ) * 4.*PI* ( UAVG(LU) - PLSORC )
 90      IF( PRNT(2) )  WRITE( *,1020 ) UTAU(LU), LYU, RFLDIR(LU),
     $                                 RFLDN(LU), FDNTOT, FLUP(LU),
     $                                 FNET, UAVG(LU), PLSORC, DFDT(LU)
100   CONTINUE

      IF ( PRNT(3) )  THEN
         WRITE ( *,1100 )
         DO 200  LU = 1, NTAU
            WRITE( *,1110 )  UTAU( LU )
            DO  200  IQ = 1, NN
               ANG1 = 180./PI * ACOS( CMU(2*NN-IQ+1) )
               ANG2 = 180./PI * ACOS( CMU(IQ) )
               WRITE( *,1120 ) ANG1, CMU(2*NN-IQ+1), U0C(IQ,LU),
     $                         ANG2, CMU(IQ),        U0C(IQ+NN,LU)
200      CONTINUE
      END IF

1010  FORMAT( //, 21X,
     $ '<----------------------- FLUXES ----------------------->', /,
     $ '   OPTICAL  COMPU    DOWNWARD    DOWNWARD    DOWNWARD     ',
     $ ' UPWARD                    MEAN      PLANCK   D(NET FLUX)', /,
     $ '     DEPTH  LAYER      DIRECT     DIFFUSE       TOTAL     ',
     $ 'DIFFUSE         NET   INTENSITY      SOURCE   / D(OP DEP)', / )
1020  FORMAT( F10.4, I7, 1P,7E12.3, E14.3 )
1100  FORMAT( //, ' ******** AZIMUTHALLY AVERAGED INTENSITIES',
     $      ' ( AT POLAR QUADRATURE ANGLES ) *******' )
1110  FORMAT( /, ' OPTICAL DEPTH =', F10.4, //,
     $  '     ANGLE (DEG)   COS(ANGLE)     INTENSITY',
     $  '     ANGLE (DEG)   COS(ANGLE)     INTENSITY' )
1120  FORMAT( 2( 0P,F16.4, F13.5, 1P,E14.3 ) )

      RETURN
      END
      SUBROUTINE  LEPOLY( NMU, M, MAXMU, TWONM1, MU, YLM )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C       COMPUTES THE NORMALIZED ASSOCIATED LEGENDRE POLYNOMIAL,
C       DEFINED IN TERMS OF THE ASSOCIATED LEGENDRE POLYNOMIAL
C       PLM = P-SUB-L-SUPER-M AS

C             YLM(MU) = SQRT( (L-M)!/(L+M)! ) * PLM(MU)

C       FOR FIXED ORDER -M- AND ALL DEGREES FROM L = M TO TWONM1.
C       WHEN M.GT.0, ASSUMES THAT Y-SUB(M-1)-SUPER(M-1) IS AVAILABLE
C       FROM A PRIOR CALL TO THE ROUTINE.

C       REFERENCE: Dave, J.V. and B.H. Armstrong, Computations of
C                  High-Order Associated Legendre Polynomials,
C                  J. Quant. Spectrosc. Radiat. Transfer 10,
C                  557-562, 1970.  (hereafter D/A)

C       METHOD: Varying degree recurrence relationship.

C       NOTE 1: The D/A formulas are transformed by
C               setting  M = n-1; L = k-1.
C       NOTE 2: Assumes that routine is called first with  M = 0,
C               then with  M = 1, etc. up to  M = TWONM1.
C       NOTE 3: Loops are written in such a way as to vectorize.

C  I N P U T     V A R I A B L E S:

C       NMU    :  NUMBER OF ARGUMENTS OF -YLM-
C       M      :  ORDER OF -YLM-
C       MAXMU  :  FIRST DIMENSION OF -YLM-
C       TWONM1 :  MAX DEGREE OF -YLM-
C       MU(I)  :  I = 1 TO NMU, ARGUMENTS OF -YLM-
C       IF M.GT.0, YLM(M-1,I) FOR I = 1 TO NMU IS REQUIRED

C  O U T P U T     V A R I A B L E:

C       YLM(L,I) :  L = M TO TWONM1, NORMALIZED ASSOCIATED LEGENDRE
C                   POLYNOMIALS EVALUATED AT ARGUMENT -MU(I)-
C+---------------------------------------------------------------------+
      real*8     MU(*), YLM( 0:MAXMU,* )
      INTEGER  M, NMU, TWONM1
      PARAMETER  ( MAXSQT = 1000 )
      real*8     SQT( MAXSQT )
      LOGICAL  PASS1
      SAVE  SQT, PASS1
      DATA  PASS1 / .TRUE. /


      IF ( PASS1 )  THEN
         PASS1 = .FALSE.
         DO 1  NS = 1, MAXSQT
            SQT( NS ) = dsqrt( dble(NS) )
    1    CONTINUE
      ENDIF

      IF ( 2*TWONM1 .GT. MAXSQT )
     $   CALL ERRMSG( 'LEPOLY--NEED TO INCREASE PARAM MAXSQT', .TRUE. )

      IF ( M .EQ. 0 )  THEN
C                             ** UPWARD RECURRENCE FOR ORDINARY
C                             ** LEGENDRE POLYNOMIALS
         DO  10  I = 1, NMU
            YLM( 0,I ) = 1.
            YLM( 1,I ) = MU( I )
  10     CONTINUE

         DO  20  L = 2, TWONM1
            DO  20  I = 1, NMU
               YLM( L,I ) = ( ( 2*L-1 ) * MU(I) * YLM( L-1,I )
     $                      - ( L-1 ) * YLM( L-2,I ) ) / L
  20     CONTINUE

      ELSE

         DO  30  I = 1, NMU
C                               ** Y-SUB-M-SUPER-M; DERIVED FROM
C                               ** D/A EQS. (11,12)

            YLM( M,I) = - SQT( 2*M-1 ) / SQT( 2*M )
     $                  * dsqrt( 1. - MU(I)**2 ) * YLM( M-1,I )

C                              ** Y-SUB-(M+1)-SUPER-M; DERIVED FROM
C                              ** D/A EQS. (13,14) USING EQS. (11,12)

            YLM( M+1,I ) = SQT( 2*M+1 ) * MU(I) * YLM( M,I )
30       CONTINUE
C                                   ** UPWARD RECURRENCE; D/A EQ. (10)
         DO  40  L = M+2, TWONM1
            TMP1 = SQT( L-M ) * SQT( L+M )
            TMP2 = SQT( L-M-1 ) * SQT( L+M-1 )
            DO  40  I = 1, NMU
               YLM( L,I ) = ( ( 2*L-1 ) * MU(I) * YLM( L-1,I )
     $                        - TMP2 * YLM( L-2,I ) ) / TMP1
40       CONTINUE

      END IF

      RETURN
      END
      SUBROUTINE  PRALTR( UMU, NUMU, ALBMED, TRNMED )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C        PRINT PLANAR ALBEDO AND TRANSMISSIVITY OF MEDIUM
C        AS A FUNCTION OF INCIDENT BEAM ANGLE

      real*8     UMU(*), ALBMED(*), TRNMED(*)


      WRITE( *,110 )
      DO 20  IU = 1, NUMU
         ANGL = 180.0/3.14159265 * dacos( UMU(IU) )
         WRITE(*,111)  ANGL, UMU(IU), ALBMED(IU), TRNMED(IU)
 20   CONTINUE

      RETURN

110   FORMAT( ///, ' *******  FLUX ALBEDO AND/OR TRANSMISSIVITY OF ',
     $ 'ENTIRE MEDIUM  ********', //,
     $ ' BEAM ZEN ANG  dcos(BEAM ZEN ANG)      ALBEDO   TRANSMISSIVITY')
111   FORMAT( 0P,F13.4, F20.6, F12.5, 1P,E17.4 )
      END
      SUBROUTINE  PRAVIN( UMU, NUMU, MAXUMU, UTAU, NTAU, U0U )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C        PRINT AZIMUTHALLY AVERAGED INTENSITIES AT USER ANGLES

      real*8     UMU(*), UTAU(*), U0U( MAXUMU,* )


      WRITE ( *, '(//,A)' )
     $         ' *********  AZIMUTHALLY AVERAGED INTENSITIES '
     $       // '(USER POLAR ANGLES)  *********'
      LENFMT = 8
      NPASS = 1 + NUMU / LENFMT
      IF ( MOD(NUMU,LENFMT) .EQ. 0 )  NPASS = NPASS - 1
      DO 10  NP = 1, NPASS
         IUMIN = 1 + LENFMT * (NP-1)
         IUMAX = MIN0( LENFMT*NP, NUMU )
         WRITE ( *,101 )  ( UMU(IU), IU = IUMIN, IUMAX )
         DO 10  LU = 1, NTAU
            WRITE( *,102 ) UTAU(LU), ( U0U(IU,LU), IU=IUMIN,IUMAX)
 10   CONTINUE

      RETURN

101   FORMAT( /, 3X,'OPTICAL   POLAR ANGLE COSINES',
     $        /, 3X,'  DEPTH', 8F14.5 )
102   FORMAT( 0P,F10.4, 1P,8E14.4 )
      END
      SUBROUTINE  PRTINP( NLYR, DTAUC, SSALB, PMOM, TEMPER, WVNMLO,
     $                    WVNMHI, NTAU, UTAU, NSTR, NUMU, UMU,
     $                    NPHI, PHI, IBCND, FBEAM, UMU0, PHI0,
     $                    FISOT, LAMBER, ALBEDO, HL, BTEMP, TTEMP,
     $                    TEMIS, DELTAM, PLANK, ONLYFL, ACCUR,
     $                    FLYR, LYRCUT, OPRIM, TAUC, TAUCPR,
     $                    MAXCMU, PRTMOM )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C        PRINT VALUES OF INPUT VARIABLES

      LOGICAL  DELTAM, LAMBER, LYRCUT, PLANK, ONLYFL, PRTMOM
      real*8     UMU(*), FLYR(*), DTAUC(*), OPRIM(*), PHI(*),
     $         PMOM( 0:MAXCMU,* ), SSALB(*), UTAU(*), TAUC( 0:* ),
     $         TAUCPR( 0:* ), TEMPER( 0:* ), HL( 0:MAXCMU )


      WRITE( *,1010 )  NSTR, NLYR
      IF ( IBCND.NE.1 ) WRITE( *,1030 )  NTAU, (UTAU(LU), LU = 1, NTAU)
      IF ( .NOT.ONLYFL )
     $      WRITE( *,1040 )  NUMU, ( UMU(IU), IU = 1, NUMU )
      IF ( .NOT.ONLYFL .AND. IBCND.NE.1 )
     $      WRITE( *,1050 )  NPHI, ( PHI(J), J = 1, NPHI )
      IF ( .NOT.PLANK .OR. IBCND.EQ.1 )  WRITE( *,1100 )
      WRITE( *,1055 )  IBCND
      IF ( IBCND.EQ.0 )  THEN
         WRITE( *,1060 ) FBEAM, UMU0, PHI0, FISOT
         IF ( LAMBER )   WRITE( *,1080 ) ALBEDO
         IF ( .NOT.LAMBER )  WRITE( *,1090 ) ( HL(K), K = 0, NSTR )
         IF ( PLANK )  WRITE( *,1110 ) WVNMLO, WVNMHI, BTEMP, 
     $                                 TTEMP, TEMIS
      ELSE IF ( IBCND.EQ.1 )  THEN
         WRITE( *,1070 )
         WRITE( *,1080 ) ALBEDO
      ENDIF
      IF ( DELTAM )      WRITE( *,1120 )
      IF ( .NOT.DELTAM ) WRITE( *,1130 )
      IF ( IBCND.EQ.1 )  THEN
         WRITE( *,1135 )
      ELSE IF ( ONLYFL )  THEN
         WRITE( *,1140 )
      ELSE
         WRITE( *,1150 )
      ENDIF
      WRITE( *,1160 )  ACCUR
      IF ( LYRCUT )  WRITE( *,1170 )
      IF ( PLANK )  WRITE ( *,1190 )
      IF ( .NOT.PLANK )  WRITE ( *,1191 )
      YESSCT = 0.0
      DO 10 LC = 1, NLYR
         YESSCT = YESSCT + SSALB(LC)
         IF( PLANK )
     $       WRITE( *,1200 )  LC, DTAUC(LC), TAUC(LC), SSALB(LC),
     $                    FLYR(LC), TAUCPR(LC)-TAUCPR(LC-1), TAUCPR(LC),
     $                    OPRIM(LC), PMOM(1,LC), TEMPER(LC-1)
         IF( .NOT.PLANK )
     $       WRITE( *,1200 )  LC, DTAUC(LC), TAUC(LC), SSALB(LC),
     $                    FLYR(LC), TAUCPR(LC)-TAUCPR(LC-1), TAUCPR(LC),
     $                    OPRIM(LC), PMOM(1,LC)
 10   CONTINUE
      IF( PLANK )  WRITE( *,1210 ) TEMPER(NLYR)

      IF( PRTMOM .AND. YESSCT.GT.0.0 )  THEN
         WRITE( *, '(/,A)' )  ' LAYER   PHASE FUNCTION MOMENTS'
         DO 20 LC = 1, NLYR
            IF( SSALB(LC).GT.0.0 )
     $          WRITE( *,1300 )  LC, ( PMOM(K,LC), K = 0, NSTR )
 20      CONTINUE
      ENDIF

      RETURN

1010  FORMAT ( /, ' NO. STREAMS =', I4,
     $  '     NO. COMPUTATIONAL LAYERS =', I4 )
1030  FORMAT( I4,' USER OPTICAL DEPTHS :',10F10.4, /, (26X,10F10.4) )
1040  FORMAT( I4,' USER POLAR ANGLE COSINES :',10F9.5,/,(31X,10F9.5) )
1050  FORMAT( I4,' USER AZIMUTHAL ANGLES :', 10F9.2, /, (28X,10F9.2) )
1055  FORMAT( ' BOUNDARY CONDITION FLAG: IBCND =', I2 )
1060  FORMAT( '    INCIDENT BEAM WITH INTENSITY =', 1P,E11.3, ' AND',
     $ ' POLAR ANGLE COSINE = ', 0P,F8.5,'  AND AZIMUTH ANGLE =', F7.2,
     $ /,'    PLUS ISOTROPIC INCIDENT INTENSITY =', 1P,E11.3 )
1070  FORMAT( '    ISOTROPIC ILLUMINATION FROM TOP AND BOTTOM' )
1080  FORMAT( '    BOTTOM ALBEDO (LAMBERTIAN) =', 0P,F8.4 )
1090  FORMAT( '    LEGENDRE COEFFS OF BOTTOM BIDIRECTIONAL',
     $ ' REFLECTIVITY :', /, (10X,10F9.5) )
1100  FORMAT( ' NO THERMAL EMISSION' )
1110  FORMAT( '    THERMAL EMISSION IN WAVENUMBER INTERVAL :', 2F14.4,/,
     $   '    BOTTOM TEMPERATURE =', F10.2, '     TOP TEMPERATURE =',
     $   F10.2,'    TOP EMISSIVITY =', F8.4 )
1120  FORMAT( ' USES DELTA-M METHOD' )
1130  FORMAT( ' DOES NOT USE DELTA-M METHOD' )
1135  FORMAT( ' CALCULATE ALBEDO AND TRANSMISSIVITY OF MEDIUM',
     $   ' VS. INCIDENT BEAM ANGLE' )
1140  FORMAT( ' CALCULATE FLUXES AND AZIM-AVERAGED INTENSITIES ONLY' )
1150  FORMAT( ' CALCULATE FLUXES AND INTENSITIES' )
1160  FORMAT( ' RELATIVE CONVERGENCE CRITERION FOR AZIMUTH SERIES =',
     $   1P,E11.2 )
1170  FORMAT( ' SETS RADIATION = 0 BELOW ABSORPTION OPTICAL DEPTH 10' )
1190  FORMAT( /, 37X, '<------------- DELTA-M --------------->', /,
     $'                   TOTAL    SINGLE                           ',
     $               'TOTAL    SINGLE', /,
     $'       OPTICAL   OPTICAL   SCATTER   TRUNCATED   ',
     $   'OPTICAL   OPTICAL   SCATTER    ASYMM', /,
     $'         DEPTH     DEPTH    ALBEDO    FRACTION     ',
     $     'DEPTH     DEPTH    ALBEDO   FACTOR   TEMPERATURE' )
1191  FORMAT( /, 37X, '<------------- DELTA-M --------------->', /,
     $'                   TOTAL    SINGLE                           ',
     $               'TOTAL    SINGLE', /,
     $'       OPTICAL   OPTICAL   SCATTER   TRUNCATED   ',
     $   'OPTICAL   OPTICAL   SCATTER    ASYMM', /,
     $'         DEPTH     DEPTH    ALBEDO    FRACTION     ',
     $     'DEPTH     DEPTH    ALBEDO   FACTOR' )
1200  FORMAT( I4, 2F10.4, F10.5, F12.5, 2F10.4, F10.5, F9.4,F14.3 )
1210  FORMAT( 85X, F14.3 )
1300  FORMAT( I6, 10F11.6, /, (6X,10F11.6) )

      END
      SUBROUTINE  PRTINT( UU, UTAU, NTAU, UMU, NUMU, PHI, NPHI,
     $                    MAXULV, MAXUMU )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C         PRINTS THE INTENSITY AT USER POLAR AND AZIMUTHAL ANGLES

C     ALL ARGUMENTS ARE DISORT INPUT OR OUTPUT VARIABLES

C+---------------------------------------------------------------------+
      real*8   PHI(*), UMU(*), UTAU(*), UU( MAXUMU, MAXULV, * )


      WRITE ( *, '(//,A)' )
     $         ' *********  I N T E N S I T I E S  *********'
      LENFMT = 10
      NPASS = 1 + NPHI / LENFMT
      IF ( MOD(NPHI,LENFMT) .EQ. 0 )  NPASS = NPASS - 1
      DO 10  LU = 1, NTAU
         DO 10  NP = 1, NPASS
            JMIN = 1 + LENFMT * (NP-1)
            JMAX = MIN0( LENFMT*NP, NPHI )
            WRITE( *,101 )  ( PHI(J), J = JMIN, JMAX )
            DO 10  IU = 1, NUMU
               IF( IU.EQ.1 )  WRITE( *,102 )  UTAU(LU), UMU(IU),
     $           ( UU( IU,LU,J ), J = JMIN, JMAX )
               IF( IU.GT.1 )  WRITE( *,103 )  UMU(IU),
     $           ( UU( IU,LU,J ), J = JMIN, JMAX )
10    CONTINUE

      RETURN

101   FORMAT( /, 3X,'          POLAR   AZIMUTH ANGLES (DEGREES)',
     $        /, 3X,'OPTICAL   ANGLE',
     $        /, 3X,' DEPTH   COSINE', 10F11.2 )
102   FORMAT( F10.4, F8.4, 1P,10E11.3 )
103   FORMAT( 10X,   F8.4, 1P,10E11.3 )
      END
      SUBROUTINE  QGAUSN( M, GMU, GWT )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C       Compute weights and abscissae for ordinary gaussian quadrature
C       (no weight function inside integral) on the interval (0,1)

C   INPUT :    M                     order of quadrature rule

C   OUTPUT :  GMU(I)  I = 1 TO M,    array of abscissae
C             GWT(I)  I = 1 TO M,    array of weights

C   REFERENCE:  Davis, P.J. and P. Rabinowitz, Methods of Numerical
C                   Integration, Academic Press, New York, pp. 87, 1975.

C   METHOD:  Compute the abscissae as roots of the Legendre
C            polynomial P-sub-M using a cubically convergent
C            refinement of Newton's method.  Compute the
C            weights from EQ. 2.7.3.8 of Davis/Rabinowitz.  Note
C            that Newton's method can very easily diverge; only a
C            very good initial guess can guarantee convergence.
C            The initial guess used here has never led to divergence
C            even for M up to 1000.

C   ACCURACY:  at least 13 significant digits

C   INTERNAL VARIABLES:

C    ITER      : number of Newton Method iterations
C    MAXIT     : maximum allowed iterations of Newton Method
C    PM2,PM1,P : 3 successive Legendre polynomials
C    PPR       : derivative of Legendre polynomial
C    P2PRI     : 2nd derivative of Legendre polynomial
C    TOL       : convergence criterion for Legendre poly root iteration
C    X,XI      : successive iterates in cubically-convergent version
C                of Newtons Method (seeking roots of Legendre poly.)
C+---------------------------------------------------------------------+
      real*8     CONA, GMU(*), GWT(*), PI, T
      INTEGER  ITER, LIM, M, MAXIT, NP1
      DOUBLE   PRECISION  D1MACH
      DOUBLE   PRECISION  EN, NNP1, ONE, P, PM1, PM2, PPR, P2PRI, PROD,
     $                    TMP, TOL, TWO, X, XI
      SAVE     PI, TOL
      DATA     PI / 0.0 /,  MAXIT / 1000 /,  ONE / 1.D0 /,  TWO / 2.D0 /


      IF ( PI.EQ.0.0 )  THEN
         PI = 2. * dasin(1.0d0)
         TOL = 10. * d1mach(3)
      END IF

      IF ( M.LT.1 )  CALL ErrMsg( 'QGAUSN--Bad value of M', .TRUE. )
      IF ( M.EQ.1 )  THEN
         GMU( 1 ) = 0.5
         GWT( 1 ) = 1.0
         RETURN
      END IF

      EN   = M
      NP1  = M + 1
      NNP1 = M * NP1
      CONA = dble( M-1 ) / ( 8 * M**3 )

      LIM  = M / 2
      DO 30  K = 1, LIM
C                                        ** initial guess for k-th root
C                                        ** of Legendre polynomial, from
C                                        ** Davis/Rabinowitz (2.7.3.3a)
         T = ( 4*K - 1 ) * PI / ( 4*M + 2 )
         X = dcos ( T + CONA / dtan( T ) )
         ITER = 0
C                                        ** upward recurrence for
C                                        ** Legendre polynomials
   10    ITER = ITER + 1
         PM2 = ONE
         PM1 = X
         DO 20 NN = 2, M
            P   = ( ( 2*NN - 1 ) * X * PM1 - ( NN-1 ) * PM2 ) / NN
            PM2 = PM1
            PM1 = P
   20    CONTINUE
C                                              ** Newton Method
         TMP   = ONE / ( ONE - X**2 )
         PPR   = EN * ( PM2 - X * P ) * TMP
         P2PRI = ( TWO * X * PPR - NNP1 * P ) * TMP
         XI    = X - ( P / PPR ) * ( ONE +
     $               ( P / PPR ) * P2PRI / ( TWO * PPR ) )

C                                              ** check for convergence
         IF ( dabs(XI-X) .GT. TOL ) THEN
            IF( ITER.GT.MAXIT )
     $          CALL ERRMSG( 'QGAUSN--MAX ITERATION COUNT', .TRUE. )
            X = XI
            GO TO 10
         END IF
C                             ** iteration finished--calculate weights,
C                             ** abscissae for (-1,1)
         GMU( K ) = - X
         GWT( K ) = TWO / ( TMP * ( EN * PM2 )**2 )
         GMU( NP1 - K ) = - GMU( K )
         GWT( NP1 - K ) =   GWT( K )
  30  CONTINUE
C                                    ** set middle abscissa and weight
C                                    ** for rules of odd order
      IF ( MOD( M,2 ) .NE. 0 )  THEN
         GMU( LIM + 1 ) = 0.0
         PROD = ONE
         DO 40 K = 3, M, 2
            PROD = PROD * K / ( K-1 )
  40     CONTINUE
         GWT( LIM + 1 ) = TWO / PROD**2
      END IF
C                                        ** convert from (-1,1) to (0,1)
      DO 50  K = 1, M
         GMU( K ) = 0.5 * GMU( K ) + 0.5
         GWT( K ) = 0.5 * GWT( K )
  50  CONTINUE

      RETURN
      END
      real*8 FUNCTION  RATIO( A, B )
c             insert for double precision modification - NORTH
                implicit double precision ( a-h, o-z)
C        CALCULATE RATIO  A/B  WITH OVER- AND UNDER-FLOW PROTECTION

         IF ( dabs(A).LT.1.0E-8 .AND. dabs(B).LT.1.0E-8 )  THEN
            RATIO = 1.0
         ELSE IF ( B.EQ.0.0 )  THEN
            RATIO = 1.E+20
         ELSE
            RATIO = A / B
         END IF

      RETURN
      END
      SUBROUTINE  SETDIS( CMU, CWT, DELTAM, DTAUC, EXPBEA, FBEAM, FLYR,
     $                    GL, HL, HLPR, IBCND, LAMBER, LAYRU, LYRCUT,
     $                    MAXUMU, MAXCMU, MXCMU, NCUT, NLYR, NTAU, NN,
     $                    NSTR, PLANK, NUMU, ONLYFL, OPRIM, PMOM,SSALB,
     $                    TAUC, TAUCPR, UTAU, UTAUPR, UMU, UMU0, USRTAU,
     $                    USRANG )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C          Perform miscellaneous setting-up operations

C       Routines called:  ERRMSG, QGAUSN, ZEROIT

C       INPUT :  all are DISORT input variables (see DOC file)

C       OUTPUT:  NTAU,UTAU   if USRTAU = FALSE
C                NUMU,UMU    if USRANG = FALSE
C                CMU,CWT     computational polar angles and
C                               corresponding quadrature weights
C                EXPBEA      transmission of direct beam
C                FLYR        truncated fraction in delta-M method
C                GL          phase function Legendre coefficients multi-
C                              plied by (2L+1) and single-scatter albedo
C                HLPR        Legendre moments of surface bidirectional
C                              reflectivity, times 2K+1
C                LAYRU       Computational layer in which UTAU falls
C                LYRCUT      flag as to whether radiation will be zeroed
c                              below layer NCUT
C                NCUT        computational layer where absorption
c                              optical depth first exceeds  ABSCUT
C                NN          NSTR / 2
C                OPRIM       delta-M-scaled single-scatter albedo
C                TAUCPR      delta-M-scaled optical depth
C                UTAUPR      delta-M-scaled version of  UTAU

      LOGICAL  DELTAM, LAMBER, LYRCUT, PLANK, ONLYFL, USRTAU, USRANG
      INTEGER  LAYRU(*)
      real*8     CMU(*), CWT(*), DTAUC(*), EXPBEA(0:*), FLYR(*),
     $         GL(0:MXCMU,*), HL(0:*), HLPR(0:*), OPRIM(*),
     $         PMOM(0:MAXCMU,*), SSALB(*), TAUC(0:*), TAUCPR(0:*),
     $         UTAU(*), UTAUPR(*), UMU(*)
      DATA  ABSCUT / 10. /
      IF ( .NOT.USRTAU ) THEN
C                              ** SET OUTPUT LEVELS AT COMPUTATIONAL
C                              ** LAYER BOUNDARIES
         NTAU = NLYR + 1
         DO 30  LC = 0, NTAU-1
            UTAU(LC+1) = TAUC(LC)
30       CONTINUE
      END IF
C                        ** APPLY DELTA-M SCALING AND MOVE DESCRIPTION
C                        ** OF COMPUTATIONAL LAYERS TO LOCAL VARIABLES
      EXPBEA( 0 ) = 1.0
      ABSTAU = 0.0
      DO  60  LC = 1, NLYR
         PMOM(0,LC) = 1.0
         IF ( ABSTAU.LT.ABSCUT )  NCUT = LC
         ABSTAU = ABSTAU + ( 1. - SSALB(LC) ) * DTAUC(LC)

         IF ( .NOT.DELTAM )  THEN
            OPRIM(LC) = SSALB(LC)
            TAUCPR(LC) = TAUC(LC)
            DO 40  K = 0, NSTR-1
               GL(K,LC) = (2*K+1) * OPRIM(LC) * PMOM(K,LC)
 40         CONTINUE
            F = 0.0
         ELSE
C                                    ** DO DELTA-M TRANSFORMATION
            F = PMOM( NSTR,LC )
            OPRIM(LC) = SSALB(LC) * ( 1. - F ) / ( 1. - F * SSALB(LC) )
            TAUCPR(LC) = TAUCPR(LC-1) + ( 1. - F*SSALB(LC) ) * DTAUC(LC)
            DO 50  K = 0, NSTR-1
               GL(K,LC) = (2*K+1) * OPRIM(LC) * (PMOM(K,LC)-F) / (1.-F)
 50         CONTINUE
         ENDIF

         FLYR(LC) = F
         EXPBEA(LC) = 0.0
         IF ( FBEAM.GT.0.0 )  EXPBEA(LC) = dexp( - TAUCPR(LC) / UMU0 )
60    CONTINUE
C                      ** IF NO THERMAL EMISSION, CUT OFF MEDIUM BELOW
C                      ** ABSORPTION OPTICAL DEPTH = ABSCUT ( NOTE THAT
C                      ** DELTA-M TRANSFORMATION LEAVES ABSORPTION
C                      ** OPTICAL DEPTH INVARIANT ).  NOT WORTH THE
C                      ** TROUBLE FOR ONE-LAYER PROBLEMS, THOUGH.
      LYRCUT = .FALSE.
      IF ( ABSTAU.GE.ABSCUT .AND. .NOT.PLANK .AND. IBCND.NE.1
     $     .AND. NLYR.GT.1 )  LYRCUT =.TRUE.
      IF ( .NOT.LYRCUT )  NCUT = NLYR

C                             ** SET ARRAYS DEFINING LOCATION OF USER
C                             ** OUTPUT LEVELS WITHIN DELTA-M-SCALED
C                             ** COMPUTATIONAL MESH
      DO 90  LU = 1, NTAU
         DO 70 LC = 1, NLYR
            IF ( UTAU(LU).GE.TAUC(LC-1) .AND. UTAU(LU).LE.TAUC(LC) )
     $           GO TO 80
70       CONTINUE
         LC = NLYR

80       UTAUPR(LU) = UTAU(LU)
         IF(DELTAM) UTAUPR(LU) = TAUCPR(LC-1) + (1.-SSALB(LC)*FLYR(LC))
     $                                        * (UTAU(LU) - TAUC(LC-1))
         LAYRU(LU) = LC
90    CONTINUE
C                      ** CALCULATE COMPUTATIONAL POLAR ANGLE COSINES
C                      ** AND ASSOCIATED QUADRATURE WEIGHTS FOR GAUSSIAN
C                      ** QUADRATURE ON THE INTERVAL (0,1) (UPWARD)
      NN = NSTR / 2
      CALL  QGAUSN( NN, CMU, CWT )
C                                  ** DOWNWARD (NEG) ANGLES AND WEIGHTS
      DO 100  IQ = 1, NN
         CMU(IQ+NN) = - CMU(IQ)
         CWT(IQ+NN) =   CWT(IQ)
100   CONTINUE

      IF ( FBEAM.GT.0.0 )  THEN
C                               ** COMPARE BEAM ANGLE TO COMPU'L ANGLES
         DO 110  IQ = 1, NN
            IF ( dabs(UMU0-CMU(IQ))/UMU0 .LT. 1.E-4 )  CALL ERRMSG
     $         ( 'SETDIS--BEAM ANGLE=COMPUTATIONAL ANGLE; CHANGE NSTR',
     $            .TRUE. )
  110    CONTINUE
      END IF
      IF ( .NOT.USRANG .OR. (ONLYFL .AND. MAXUMU.GE.NSTR) )  THEN

C                                   ** SET OUTPUT POLAR ANGLES TO
C                                   ** COMPUTATIONAL POLAR ANGLES
            NUMU = NSTR
            DO 120  IU = 1, NN
               UMU(IU) = - CMU(NN+1-IU)
120         CONTINUE
            DO 121  IU = NN+1, NSTR
               UMU(IU) = CMU(IU-NN)
121         CONTINUE
      END IF
             IF ( USRANG .AND. IBCND.EQ.1 )  THEN

C                               ** SHIFT POSITIVE USER ANGLE COSINES TO
C                               ** UPPER LOCATIONS AND PUT NEGATIVES
C                               ** IN LOWER LOCATIONS
         DO 140  IU = 1, NUMU
            UMU(IU+NUMU) = UMU(IU)
140      CONTINUE
         DO 141  IU = 1, NUMU
            UMU(IU) = - UMU( 2*NUMU+1-IU)
141      CONTINUE
         NUMU = 2*NUMU
      END IF

      IF ( .NOT.LYRCUT .AND. .NOT.LAMBER )  THEN
         DO 160  K = 0, NSTR
            HLPR(K) = (2*K+1) * HL(K)
160      CONTINUE
      END IF

      RETURN
      END
      SUBROUTINE  SETMTX( BDR, CBAND, CMU, CWT, DELM0, GC, KK, LAMBER,
     $                    LYRCUT, MI, MI9M2, MXCMU, NCOL, NCUT, NNLYRI,
     $                    NN, NSTR, TAUCPR, WK )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C        CALCULATE COEFFICIENT MATRIX FOR THE SET OF EQUATIONS
C        OBTAINED FROM THE BOUNDARY CONDITIONS AND THE CONTINUITY-
C        OF-INTENSITY-AT-LAYER-INTERFACE EQUATIONS;  STORE IN THE
C        SPECIAL BANDED-MATRIX FORMAT REQUIRED BY LINPACK ROUTINES

C     ROUTINES CALLED:  ZEROIT

C     I N P U T      V A R I A B L E S:

C       BDR      :  SURFACE BIDIRECTIONAL REFLECTIVITY
C       CMU      :  ABSCISSAE FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       CWT      :  WEIGHTS FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       DELM0    :  KRONECKER DELTA, DELTA-SUB-M0
C       GC       :  EIGENVECTORS AT POLAR QUADRATURE ANGLES, SC(1)
C       KK       :  EIGENVALUES OF COEFF. MATRIX IN EQ. SS(7)
C       LYRCUT   :  LOGICAL FLAG FOR TRUNCATION OF COMPUT. LAYER
C       NN       :  NUMBER OF STREAMS IN A HEMISPHERE (NSTR/2)
C       NCUT     :  TOTAL NUMBER OF COMPUTATIONAL LAYERS CONSIDERED
C       TAUCPR   :  CUMULATIVE OPTICAL DEPTH (DELTA-M-SCALED)
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)

C   O U T P U T     V A R I A B L E S:

C       CBAND    :  LEFT-HAND SIDE MATRIX OF LINEAR SYSTEM EQ. SC(5),
C                   SCALED BY EQ. SC(12); IN BANDED FORM REQUIRED
C                   BY LINPACK SOLUTION ROUTINES
C       NCOL     :  COUNTS OF COLUMNS IN -CBAND-

C   I N T E R N A L    V A R I A B L E S:

C       IROW     :  POINTS TO ROW IN  -CBAND-
C       JCOL     :  POINTS TO POSITION IN LAYER BLOCK
C       LDA      :  ROW DIMENSION OF -CBAND-
C       NCD      :  NUMBER OF DIAGONALS BELOW OR ABOVE MAIN DIAGONAL
C       NCOL     :  COUNTS OF COLUMNS IN -CBAND-
C       NSHIFT   :  FOR POSITIONING NUMBER OF ROWS IN BAND STORAGE
C       WK       :  TEMPORARY STORAGE FOR 'EXP' EVALUATIONS
C ---------------------------------------------------------------------+
      LOGICAL LAMBER, LYRCUT
      real*8    BDR( MI,0:* ), CBAND( MI9M2,NNLYRI ), CMU(*), CWT(*),
     $        GC( MXCMU,MXCMU,* ), KK( MXCMU,* ), TAUCPR( 0:* ), WK(*)


      CALL  ZEROIT( CBAND, MI9M2*NNLYRI )
      NCD    = 3*NN - 1
      LDA    = 3*NCD + 1
      NSHIFT = LDA - 2*NSTR + 1
      NCOL   = 0
C                         ** USE CONTINUITY CONDITIONS OF EQ. STWJ(17)
C                         ** TO FORM COEFFICIENT MATRIX IN STWJ(20);
C                         ** EMPLOY SCALING TRANSFORMATION STWJ(22)
      DO 30  LC = 1, NCUT

         DO 4  IQ = 1, NN
            WK(IQ) = dexp( KK(IQ,LC) * (TAUCPR(LC) - TAUCPR(LC-1)) )
 4       CONTINUE

         JCOL = 0
         DO 10  IQ = 1, NN
            NCOL = NCOL + 1
            IROW = NSHIFT - JCOL
            DO 5  JQ = 1, NSTR
               CBAND(IROW+NSTR,NCOL) =   GC(JQ,IQ,LC)
               CBAND(IROW,     NCOL) = - GC(JQ,IQ,LC) * WK(IQ)
               IROW = IROW + 1
 5          CONTINUE
            JCOL = JCOL + 1
10       CONTINUE

         DO 20  IQ = NN+1, NSTR
            NCOL = NCOL + 1
            IROW = NSHIFT - JCOL
            DO 15  JQ = 1, NSTR
               CBAND(IROW+NSTR,NCOL) =   GC(JQ,IQ,LC) * WK(NSTR+1-IQ)
               CBAND(IROW,     NCOL) = - GC(JQ,IQ,LC)
               IROW = IROW + 1
15          CONTINUE
            JCOL = JCOL + 1
20       CONTINUE

30    CONTINUE
C                  ** USE TOP BOUNDARY CONDITION OF STWJ(20A) FOR
C                  ** FIRST LAYER
      JCOL = 0
      DO 40  IQ = 1, NN
         EXPA = dexp( KK(IQ,1) * TAUCPR(1) )
         IROW = NSHIFT - JCOL + NN
         DO 35  JQ = NN, 1, -1
            CBAND(IROW,JCOL+1) = GC(JQ,IQ,1) * EXPA
            IROW = IROW+1
35       CONTINUE
         JCOL = JCOL+1
40    CONTINUE

      DO 50  IQ = NN+1, NSTR
         IROW = NSHIFT - JCOL + NN
         DO 45  JQ = NN, 1, -1
            CBAND(IROW,JCOL+1) = GC(JQ,IQ,1)
            IROW = IROW+1
45       CONTINUE
         JCOL = JCOL+1
50    CONTINUE
C                           ** USE BOTTOM BOUNDARY CONDITION OF
C                           ** STWJ(20C) FOR LAST LAYER
      NNCOL = NCOL - NSTR
      JCOL  = 0
      DO 70  IQ = 1, NN
         NNCOL = NNCOL + 1
         IROW  = NSHIFT - JCOL + NSTR

         DO 60  JQ = NN+1, NSTR
            IF ( LYRCUT .OR. (LAMBER .AND. DELM0.EQ.0) ) THEN

C                          ** NO AZIMUTHAL-DEPENDENT INTENSITY IF LAM-
C                          ** BERT SURFACE; NO INTENSITY COMPONENT IF
C                          ** TRUNCATED BOTTOM LAYER

               CBAND(IROW,NNCOL) = GC(JQ,IQ,NCUT)
            ELSE
               SUM = 0.0
               DO 55  K = 1, NN
                  SUM = SUM + CWT(K) * CMU(K) * BDR(JQ-NN,K)
     $                        * GC(NN+1-K,IQ,NCUT)
55             CONTINUE
               CBAND(IROW,NNCOL) = GC(JQ,IQ,NCUT) - (1.+DELM0) * SUM
            END IF

            IROW = IROW + 1
60       CONTINUE
         JCOL = JCOL + 1
70    CONTINUE

      DO 90  IQ = NN+1, NSTR
         NNCOL = NNCOL + 1
         IROW  = NSHIFT - JCOL + NSTR
         EXPA = WK(NSTR+1-IQ)

         DO 80  JQ = NN+1, NSTR

            IF ( LYRCUT .OR. (LAMBER .AND. DELM0.EQ.0) ) THEN
               CBAND(IROW,NNCOL) = GC(JQ,IQ,NCUT) * EXPA
            ELSE
               SUM = 0.0
               DO 75  K = 1, NN
                  SUM = SUM + CWT(K) * CMU(K) * BDR(JQ-NN,K)
     $                        * GC(NN+1-K,IQ,NCUT)
75             CONTINUE
               CBAND(IROW,NNCOL) = ( GC(JQ,IQ,NCUT)
     $                               - (1.+DELM0) * SUM ) * EXPA
            END IF

            IROW = IROW + 1
80       CONTINUE
         JCOL = JCOL + 1
90    CONTINUE

      RETURN
      END
      SUBROUTINE  SLFTST( ACCUR, ALBEDO, BTEMP, DELTAM, DTAUC, FBEAM,
     $                    FISOT, IBCND, LAMBER, NLYR, PLANK, NPHI,
     $                    NUMU, NSTR, NTAU, ONLYFL, PHI, PHI0, PMOM,
     $                    PRNT, SSALB, TEMIS, TEMPER, TTEMP, UMU,
     $                    USRANG, USRTAU, UTAU, UMU0, WVNMHI, WVNMLO,
     $                    COMPAR, FLUP, RFLDIR, RFLDN, UU )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C       IF  COMPAR = FALSE, SAVE USER INPUT VALUES THAT WOULD OTHERWISE
C       BE DESTROYED AND REPLACE THEM WITH INPUT VALUES FOR SELF-TEST.
C       IF  COMPAR = TRUE, COMPARE SELF-TEST CASE RESULTS WITH CORRECT
C       ANSWERS AND RESTORE USER INPUT VALUES IF TEST IS PASSED.

C       (SEE FILE 'DISORT.DOC' FOR VARIABLE DEFINITIONS.)

C     I N T E R N A L    V A R I A B L E S:

C         ACC     RELATIVE ACCURACY REQUIRED FOR PASSING SELF-TEST
C         ERRORn  RELATIVE ERRORS IN 'DISORT' OUTPUT VARIABLES
C         OK      LOGICAL VARIABLE FOR DETERMINING FAILURE OF SELF-TEST
C         ALL VARIABLES ENDING IN 'S':  TEMPORARY 'S'TORAGE FOR INPUT
C+---------------------------------------------------------------------+
      real*8     PMOM( 0:* ), TEMPER( 0:* )
      LOGICAL  COMPAR, DELTAM, LAMBER, PLANK, OK, ONLYFL, PRNT(*),
     $         USRANG, USRTAU
      real*8     PMOMS( 0:4 ), TEMPES (0:1 )
      LOGICAL  DELTAS, LAMBES, NOPLNS, ONLYFS, PRNTS ( 7 ), USRANS,
     $         USRTAS, TSTBAD
      SAVE     NLYRS, DTAUCS, SSALBS, PMOMS, NSTRS, USRANS, NUMUS,
     $         UMUS, USRTAS, NTAUS, UTAUS, NPHIS, PHIS, IBCNDS,
     $         FBEAMS, UMU0S, PHI0S, FISOTS, LAMBES, ALBEDS, DELTAS,
     $         ONLYFS, ACCURS, NOPLNS, WVNMLS, WVNMHS, BTEMPS, TTEMPS,
     $         TEMISS, TEMPES, PRNTS
      DATA     ACC / 1.E-4 /


      IF  ( .NOT.COMPAR )  THEN
C                                              ** SAVE USER INPUT VALUES
         NLYRS = NLYR
         DTAUCS = DTAUC
         SSALBS = SSALB
         DO 1  N = 0, 4
            PMOMS(N) = PMOM(N)
 1       CONTINUE
         NSTRS = NSTR
         USRANS = USRANG
         NUMUS  = NUMU
         UMUS  = UMU
         USRTAS = USRTAU
         NTAUS = NTAU
         UTAUS  = UTAU
         NPHIS = NPHI
         PHIS  = PHI
         IBCNDS = IBCND
         FBEAMS = FBEAM
         UMU0S = UMU0
         PHI0S = PHI0
         FISOTS  = FISOT
         LAMBES = LAMBER
         ALBEDS = ALBEDO
         DELTAS = DELTAM
         ONLYFS = ONLYFL
         ACCURS = ACCUR
         NOPLNS = PLANK
         WVNMLS = WVNMLO
         WVNMHS = WVNMHI
         BTEMPS = BTEMP
         TTEMPS = TTEMP
         TEMISS = TEMIS
         TEMPES( 0 ) = TEMPER( 0 )
         TEMPES( 1 ) = TEMPER( 1 )
         DO 3 I = 1, 7
            PRNTS( I ) = PRNT( I )
    3    CONTINUE
C                                     ** SET INPUT VALUES FOR SELF-TEST
         NLYR = 1
         DTAUC = 1.0
         SSALB = 0.9
C                          ** HAZE L MOMENTS
         PMOM(0) = 1.0
         PMOM(1) = 0.8042
         PMOM(2) = 0.646094
         PMOM(3) = 0.481851
         PMOM(4) = 0.359056
         NSTR = 4
         USRANG = .TRUE.
         NUMU  = 1
         UMU  = 0.5
         USRTAU = .TRUE.
         NTAU = 1
         UTAU  = 0.5
         NPHI = 1
         PHI  = 90.0
         IBCND = 0
         FBEAM = 3.14159265
         UMU0 = 0.866
         PHI0 = 0.0
         FISOT  = 1.0
         LAMBER = .TRUE.
         ALBEDO = 0.7
         DELTAM = .TRUE.
         ONLYFL = .FALSE.
         ACCUR = 1.E-4
         PLANK = .TRUE.
         WVNMLO = 0.0
         WVNMHI = 50000.
         BTEMP = 300.0
         TTEMP = 100.0
         TEMIS = 0.8
         TEMPER( 0 ) = 210.0
         TEMPER( 1 ) = 200.0
         DO 5 I = 1, 7
            PRNT( I ) = .FALSE.
    5    CONTINUE

      ELSE
C                                    ** COMPARE TEST CASE RESULTS WITH
C                                    ** CORRECT ANSWERS AND ABORT IF BAD
         OK = .TRUE.
         ERROR1 = ( UU  - 47.86005 ) / 47.86005
         ERROR2 = ( RFLDIR - 1.527286 ) / 1.527286
         ERROR3 = ( RFLDN - 28.37223 ) / 28.37223
         ERROR4 = ( FLUP   - 152.5853 ) / 152.5853
         IF( dabs(ERROR1).GT.ACC ) OK = TSTBAD( 'UU',     ERROR1 )
         IF( dabs(ERROR2).GT.ACC ) OK = TSTBAD( 'RFLDIR', ERROR2 )
         IF( dabs(ERROR3).GT.ACC ) OK = TSTBAD( 'RFLDN',  ERROR3 )
         IF( dabs(ERROR4).GT.ACC ) OK = TSTBAD( 'FLUP',   ERROR4 )

         IF( .NOT. OK )
     $       CALL ERRMSG( 'DISORT--SELF-TEST FAILED', .TRUE. )

C                                           ** RESTORE USER INPUT VALUES
         NLYR = NLYRS
         DTAUC = DTAUCS
         SSALB = SSALBS
         DO 11  N = 0, 4
            PMOM(N) = PMOMS(N)
 11      CONTINUE
         NSTR = NSTRS
         USRANG = USRANS
         NUMU  = NUMUS
         UMU  = UMUS
         USRTAU = USRTAS
         NTAU = NTAUS
         UTAU  = UTAUS
         NPHI = NPHIS
         PHI  = PHIS
         IBCND = IBCNDS
         FBEAM = FBEAMS
         UMU0 = UMU0S
         PHI0 = PHI0S
         FISOT  = FISOTS
         LAMBER = LAMBES
         ALBEDO = ALBEDS
         DELTAM = DELTAS
         ONLYFL = ONLYFS
         ACCUR = ACCURS
         PLANK = NOPLNS
         WVNMLO = WVNMLS
         WVNMHI = WVNMHS
         BTEMP = BTEMPS
         TTEMP = TTEMPS
         TEMIS = TEMISS
         TEMPER( 0 ) = TEMPES( 0 )
         TEMPER( 1 ) = TEMPES( 1 )
         DO 13  I = 1, 7
            PRNT( I ) = PRNTS( I )
   13    CONTINUE
      END IF

      RETURN
      END
      SUBROUTINE  SOLEIG( AMB, APB, ARRAY, CMU, CWT, GL, MI, MAZIM,
     $                    MXCMU, NN, NSTR, WK, YLMC, CC, EVECC, EVAL,
     $                    KK, GC, AAD, WKD, EVECCD, EVALD )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C         SOLVES EIGENVALUE/VECTOR PROBLEM NECESSARY TO CONSTRUCT
C         HOMOGENEOUS PART OF DISCRETE ORDINATE SOLUTION; STWJ(8B)
C         ** NOTE ** EIGENVALUE PROBLEM IS DEGENERATE WHEN SINGLE
C                    SCATTERING ALBEDO = 1;  PRESENT WAY OF DOING IT
C                    SEEMS NUMERICALLY MORE STABLE THAN ALTERNATIVE
C                    METHODS THAT WE TRIED

C     ROUTINES CALLED:  ASYMTX

C   I N P U T     V A R I A B L E S:

C       GL     :  DELTA-M SCALED LEGENDRE COEFFICIENTS OF PHASE FUNCTION
C                    (INCLUDING FACTORS 2L+1 AND SINGLE-SCATTER ALBEDO)
C       CMU    :  COMPUTATIONAL POLAR ANGLE COSINES
C       CWT    :  WEIGHTS FOR QUADRATURE OVER POLAR ANGLE COSINE
C       MAZIM  :  ORDER OF AZIMUTHAL COMPONENT
C       NN     :  HALF THE TOTAL NUMBER OF STREAMS
C       YLMC   :  NORMALIZED ASSOCIATED LEGENDRE POLYNOMIAL
C                 AT THE QUADRATURE ANGLES -CMU-
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)

C   O U T P U T    V A R I A B L E S:

C       CC     :  CAPITAL-C-SUB-IJ IN EQ. SS(5); NEEDED IN SS(15&18)
C       EVAL   :  -NN- EIGENVALUES OF EQ. SS(12) ON RETURN FROM 'ASYMTX'
C                    BUT THEN SQUARE ROOTS TAKEN
C       EVECC  :  -NN- EIGENVECTORS  (G+) - (G-)  ON RETURN
C                    FROM 'ASYMTX' ( COLUMN J CORRESPONDS TO -EVAL(J)- )
C                    BUT THEN  (G+) + (G-)  IS CALCULATED FROM SS(10),
C                    G+  AND  G-  ARE SEPARATED, AND  G+  IS STACKED ON
C                    TOP OF  G-  TO FORM -NSTR- EIGENVECTORS OF SS(7)
C       GC     :  PERMANENT STORAGE FOR ALL -NSTR- EIGENVECTORS, BUT
C                    IN AN ORDER CORRESPONDING TO -KK-
C       KK     :  PERMANENT STORAGE FOR ALL -NSTR- EIGENVALUES OF SS(7),
C                    BUT RE-ORDERED WITH NEGATIVE VALUES FIRST ( SQUARE
C                    ROOTS OF -EVAL- TAKEN AND NEGATIVES ADDED )

C   I N T E R N A L   V A R I A B L E S:

C       AMB,APB :  MATRICES (ALPHA-BETA), (ALPHA+BETA) IN REDUCED
C                    EIGENVALUE PROBLEM
C       ARRAY   :  COMPLETE COEFFICIENT MATRIX OF REDUCED EIGENVALUE
C                    PROBLEM: (ALFA+BETA)*(ALFA-BETA)
C       GPPLGM  :  (G+) + (G-) (CF. EQS. SS(10-11))
C       GPMIGM  :  (G+) - (G-) (CF. EQS. SS(10-11))
C       WK      :  SCRATCH ARRAY REQUIRED BY 'ASYMTX'
C+---------------------------------------------------------------------+
      real*8    AMB( MI,* ), APB( MI,* ), ARRAY( MI,* ), CC( MXCMU,* ),
     $        CMU(*), CWT(*), EVAL(*), EVECC( MXCMU,* ), GC( MXCMU,* ),
     $        GL(0:*), KK(*), WK(*), YLMC( 0:MXCMU,* )
      DOUBLE PRECISION   EVECCD( MI,* ), EVALD(*), WKD(*), AAD( MI,* )


C                             ** CALCULATE QUANTITIES IN EQS. SS(5-6)
      DO 40 IQ  = 1, NN

         DO 20  JQ = 1, NSTR
            SUM = 0.0
            DO 10  L = MAZIM, NSTR-1
               SUM = SUM + GL(L) * YLMC(L,IQ) * YLMC(L,JQ)
10          CONTINUE
            CC(IQ,JQ) = 0.5 * SUM * CWT(JQ)
20       CONTINUE

         DO 30  JQ = 1, NN
C                             ** FILL REMAINDER OF ARRAY USING SYMMETRY
C                             ** RELATIONS  C(-MUI,MUJ) = C(MUI,-MUJ)
C                             ** AND        C(-MUI,-MUJ) = C(MUI,MUJ)

            CC(IQ+NN,JQ) = CC(IQ,JQ+NN)
            CC(IQ+NN,JQ+NN) = CC(IQ,JQ)
C                                      ** GET FACTORS OF COEFF. MATRIX
C                                      ** OF REDUCED EIGENVALUE PROBLEM
            ALPHA =   CC(IQ,JQ) / CMU(IQ)
            BETA = CC(IQ,JQ+NN) / CMU(IQ)
            AMB(IQ,JQ) = ALPHA - BETA
            APB(IQ,JQ) = ALPHA + BETA
30       CONTINUE
         AMB(IQ,IQ) = AMB(IQ,IQ) - 1.0 / CMU(IQ)
         APB(IQ,IQ) = APB(IQ,IQ) - 1.0 / CMU(IQ)

C                                insert to eliminate singularity, if any
         if ( amb(iq,iq).eq.0. ) amb(iq,iq) = 1.e-6 
         if ( apb(iq,iq).eq.0. ) apb(iq,iq) = 1.e-6 

40    CONTINUE
C                      ** FINISH CALCULATION OF COEFFICIENT MATRIX OF
C                      ** REDUCED EIGENVALUE PROBLEM:  GET MATRIX
C                      ** PRODUCT (ALFA+BETA)*(ALFA-BETA); SS(12)
      DO 70  IQ = 1, NN
         DO 70  JQ = 1, NN
            SUM = 0.
            DO 60  KQ = 1, NN
               SUM = SUM + APB(IQ,KQ) * AMB(KQ,JQ)
60          CONTINUE
            ARRAY(IQ,JQ) = SUM
70    CONTINUE
C                      ** FIND (REAL) EIGENVALUES AND EIGENVECTORS

      CALL  ASYMTX( ARRAY, EVECC, EVAL, NN, MI, MXCMU, IER, WK,
     $              AAD, EVECCD, EVALD, WKD )

      IF ( IER.GT.0 )  THEN
         WRITE( *, '(//,A,I4,A)' )  ' ASYMTX--EIGENVALUE NO. ', IER,
     $     '  DIDNT CONVERGE.  LOWER-NUMBERED EIGENVALUES WRONG.'
         CALL  ERRMSG( 'ASYMTX--CONVERGENCE PROBLEMS', .TRUE. )
      END IF

CDIR$ IVDEP
      DO 75  IQ = 1, NN
C                                insert to eliminate singularity, if any
           if ( eval(iq).eq.0. ) eval(iq) = 1.e-8
         EVAL(IQ) = dsqrt( dabs( EVAL(IQ) ) )
         KK( IQ+NN ) = EVAL(IQ)
C                                             ** ADD NEGATIVE EIGENVALUE
         KK( NN+1-IQ ) = - EVAL(IQ)
75    CONTINUE
C                          ** FIND EIGENVECTORS (G+) + (G-) FROM SS(10)
C                          ** AND STORE TEMPORARILY IN -APB- ARRAY
      DO 90  JQ = 1, NN
         DO 90  IQ = 1, NN
            SUM = 0.
            DO 80  KQ = 1,NN
               SUM = SUM + AMB(IQ,KQ) * EVECC(KQ,JQ)
80          CONTINUE
            APB(IQ,JQ) = SUM / EVAL(JQ)
90    CONTINUE

      DO 100  JQ = 1, NN
CDIR$ IVDEP
         DO 100  IQ = 1, NN
            GPPLGM = APB(IQ,JQ)
            GPMIGM = EVECC(IQ,JQ)
C                                ** RECOVER EIGENVECTORS G+,G- FROM
C                                   THEIR SUM AND DIFFERENCE; STACK THEM
C                                   TO GET EIGENVECTORS OF FULL SYSTEM
C                                   SS(7) (JQ = EIGENVECTOR NUMBER)

            EVECC(IQ,      JQ) = 0.5 * ( GPPLGM + GPMIGM )
            EVECC(IQ+NN,   JQ) = 0.5 * ( GPPLGM - GPMIGM )

C                                ** EIGENVECTORS CORRESPONDING TO
C                                ** NEGATIVE EIGENVALUES (CORRESP. TO
C                                ** REVERSING SIGN OF 'K' IN SS(10) )
            GPPLGM = - GPPLGM
            EVECC(IQ,   JQ+NN) = 0.5 * ( GPPLGM + GPMIGM )
            EVECC(IQ+NN,JQ+NN) = 0.5 * ( GPPLGM - GPMIGM )
            GC( IQ+NN,   JQ+NN )   = EVECC( IQ,    JQ )
            GC( NN+1-IQ, JQ+NN )   = EVECC( IQ+NN, JQ )
            GC( IQ+NN,   NN+1-JQ ) = EVECC( IQ,    JQ+NN )
            GC( NN+1-IQ, NN+1-JQ ) = EVECC( IQ+NN, JQ+NN )
100   CONTINUE

      RETURN
      END
      SUBROUTINE  SOLVE0( B, BDR, BEM, BPLANK, CBAND, CMU, CWT, EXPBEA,
     $                    FBEAM, FISOT, IPVT, LAMBER, LL, LYRCUT,
     $                    MAZIM, MI, MI9M2, MXCMU, NCOL, NCUT, NN, NSTR,
     $                    NNLYRI, PI, TPLANK, TAUCPR, UMU0, Z, ZZ,
     $                    ZPLK0, ZPLK1 )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C        CONSTRUCT RIGHT-HAND SIDE VECTOR -B- FOR GENERAL BOUNDARY
C        CONDITIONS STWJ(17) AND SOLVE SYSTEM OF EQUATIONS OBTAINED
C        FROM THE BOUNDARY CONDITIONS AND THE
C        CONTINUITY-OF-INTENSITY-AT-LAYER-INTERFACE EQUATIONS.
C        THERMAL EMISSION CONTRIBUTES ONLY IN AZIMUTHAL INDEPENDENCE.

C     ROUTINES CALLED:  dGBCO, dGBSL, ZEROIT

C     I N P U T      V A R I A B L E S:

C       BDR      :  SURFACE BIDIRECTIONAL REFLECTIVITY
C       BEM      :  SURFACE BIDIRECTIONAL EMISSIVITY
C       BPLANK   :  BOTTOM BOUNDARY THERMAL EMISSION
C       CBAND    :  LEFT-HAND SIDE MATRIX OF LINEAR SYSTEM EQ. SC(5),
C                   SCALED BY EQ. SC(12); IN BANDED FORM REQUIRED
C                   BY LINPACK SOLUTION ROUTINES
C       CMU      :  ABSCISSAE FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       CWT      :  WEIGHTS FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       EXPBEA   :  TRANSMISSION OF INCIDENT BEAM, EXP(-TAUCPR/UMU0)
C       LYRCUT   :  LOGICAL FLAG FOR TRUNCATION OF COMPUT. LAYER
C       MAZIM    :  ORDER OF AZIMUTHAL COMPONENT
C       NCOL     :  COUNTS OF COLUMNS IN -CBAND-
C       NN       :  ORDER OF DOUBLE-GAUSS QUADRATURE (NSTR/2)
C       NCUT     :  TOTAL NUMBER OF COMPUTATIONAL LAYERS CONSIDERED
C       TPLANK   :  TOP BOUNDARY THERMAL EMISSION
C       TAUCPR   :  CUMULATIVE OPTICAL DEPTH (DELTA-M-SCALED)
C       ZZ       :  BEAM SOURCE VECTORS IN EQ. SS(19)
C       ZPLK0    :  THERMAL SOURCE VECTORS -Z0-, BY SOLVING EQ. SS(16)
C       ZPLK1    :  THERMAL SOURCE VECTORS -Z1-, BY SOLVING EQ. SS(16)
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)

C   O U T P U T     V A R I A B L E S:

C       B        :  RIGHT-HAND SIDE VECTOR OF EQ. SC(5) GOING INTO
C                   *dGBSL*; RETURNS AS SOLUTION VECTOR OF EQ.
C                   SC(12), CONSTANTS OF INTEGRATION WITHOUT
C                   EXPONENTIAL TERM
C      LL        :  PERMANENT STORAGE FOR -B-, BUT RE-ORDERED

C   I N T E R N A L    V A R I A B L E S:

C       IPVT     :  INTEGER VECTOR OF PIVOT INDICES
C       IT       :  POINTER FOR POSITION IN  -B-
C       NCD      :  NUMBER OF DIAGONALS BELOW OR ABOVE MAIN DIAGONAL
C       RCOND    :  INDICATOR OF SINGULARITY FOR -CBAND-
C       Z        :  SCRATCH ARRAY REQUIRED BY *dGBCO*
C+---------------------------------------------------------------------+

      LOGICAL  LAMBER, LYRCUT
      INTEGER  IPVT(*)
      real*8     B(*), BDR( MI,0:* ), BEM(*), CBAND( MI9M2,NNLYRI ),
     $         CMU(*), CWT(*), EXPBEA(0:*), LL( MXCMU,* ),
     $         TAUCPR( 0:* ), Z(*), ZZ( MXCMU,* ), ZPLK0( MXCMU,* ),
     $         ZPLK1( MXCMU,* )

      CALL  ZEROIT( B, NNLYRI )
C                             ** CONSTRUCT -B-,  STWJ(20A,C) FOR
C                             ** PARALLEL BEAM + BOTTOM REFLECTION +
C                             ** THERMAL EMISSION AT TOP AND/OR BOTTOM
      IF ( MAZIM.GT.0 .AND. FBEAM.GT.0.0 )  THEN
C                                         ** AZIMUTH-DEPENDENT CASE
C                                         ** (NEVER CALLED IF FBEAM = 0)
         IF ( LYRCUT .OR. LAMBER ) THEN
C               ** NO AZIMUTHAL-DEPENDENT INTENSITY FOR LAMBERT SURFACE;
C               ** NO INTENSITY COMPONENT FOR TRUNCATED BOTTOM LAYER

            DO 10  IQ = 1, NN
C                                                     ** TOP BOUNDARY
               B(IQ) = - ZZ(NN+1-IQ,1)
C                                                  ** BOTTOM BOUNDARY
               B(NCOL-NN+IQ) = - ZZ(IQ+NN,NCUT) * EXPBEA(NCUT)
10          CONTINUE

         ELSE

            DO 20  IQ = 1, NN
               B(IQ) = - ZZ(NN+1-IQ,1)

               SUM = 0.
               DO 15  JQ = 1, NN
                  SUM = SUM + CWT(JQ) * CMU(JQ) * BDR(IQ,JQ)
     $                        * ZZ(NN+1-JQ,NCUT) * EXPBEA(NCUT)
15             CONTINUE
               B(NCOL-NN+IQ) = SUM
               IF ( FBEAM.GT.0.0 )
     $              B(NCOL-NN+IQ) = SUM + ( BDR(IQ,0) * UMU0*FBEAM/PI
     $                                 - ZZ(IQ+NN,NCUT) ) * EXPBEA(NCUT)
20          CONTINUE


         END IF


C                             ** CONTINUITY CONDITION FOR LAYER
C                             ** INTERFACES OF EQ. STWJ(20B)
         IT = NN
         DO 40  LC = 1, NCUT-1
            DO 30  IQ = 1, NSTR

               IT    = IT + 1
               B(IT) = ( ZZ(IQ,LC+1) - ZZ(IQ,LC) ) * EXPBEA(LC)
30          CONTINUE
40       CONTINUE
      ELSE
C                                   ** AZIMUTH-INDEPENDENT CASE
         IF ( FBEAM.EQ.0.0 )  THEN

            DO 50 IQ = 1, NN
C                                      ** TOP BOUNDARY
               B(IQ) = - ZPLK0(NN+1-IQ,1) + FISOT + TPLANK
50          CONTINUE

            IF ( LYRCUT ) THEN
C                               ** NO INTENSITY COMPONENT FOR TRUNCATED
C                               ** BOTTOM LAYER
               DO 60 IQ = 1, NN
C                                      ** BOTTOM BOUNDARY
                  B(NCOL-NN+IQ) = - ZPLK0(IQ+NN,NCUT)
     $                            - ZPLK1(IQ+NN,NCUT) * TAUCPR(NCUT)
60             CONTINUE

            ELSE

               DO 80 IQ = 1, NN
                  SUM = 0.
                 DO 70 JQ = 1, NN


                     SUM = SUM + CWT(JQ) * CMU(JQ) * BDR(IQ,JQ)
     $                          * ( ZPLK0(NN+1-JQ,NCUT)
     $                            + ZPLK1(NN+1-JQ,NCUT) * TAUCPR(NCUT) )
70                CONTINUE
                  B(NCOL-NN+IQ) = 2.*SUM + BEM(IQ) * BPLANK
     $                            - ZPLK0(IQ+NN,NCUT)
     $                            - ZPLK1(IQ+NN,NCUT) * TAUCPR(NCUT)
80             CONTINUE
            END IF
C                             ** CONTINUITY CONDITION FOR LAYER
C                             ** INTERFACES, STWJ(20B)

            IT = NN
            DO 100  LC = 1, NCUT-1
               DO 90  IQ = 1, NSTR
                  IT    = IT + 1
                  B(IT) = ZPLK0(IQ,LC+1) - ZPLK0(IQ,LC) +
     $                  ( ZPLK1(IQ,LC+1) - ZPLK1(IQ,LC) ) * TAUCPR(LC)
90             CONTINUE
100         CONTINUE

         ELSE
            DO 150 IQ = 1, NN
               


               B(IQ) = - ZZ(NN+1-IQ,1) - ZPLK0(NN+1-IQ,1) +FISOT +TPLANK

150         CONTINUE

            IF ( LYRCUT ) THEN
               DO 160 IQ = 1, NN
                  B(NCOL-NN+IQ) = - ZZ(IQ+NN,NCUT) * EXPBEA(NCUT)
     $                            - ZPLK0(IQ+NN,NCUT)
     $                            - ZPLK1(IQ+NN,NCUT) * TAUCPR(NCUT)
160            CONTINUE

            ELSE

               DO 180 IQ = 1, NN
                  SUM = 0.
                  DO 170 JQ = 1, NN
                     SUM = SUM + CWT(JQ) * CMU(JQ) * BDR(IQ,JQ)
     $                          * ( ZZ(NN+1-JQ,NCUT) * EXPBEA(NCUT)
     $                            + ZPLK0(NN+1-JQ,NCUT)
     $                            + ZPLK1(NN+1-JQ,NCUT) * TAUCPR(NCUT) )
170               CONTINUE
                  B(NCOL-NN+IQ) = 2.*SUM + ( BDR(IQ,0) * UMU0*FBEAM/PI
     $                                 - ZZ(IQ+NN,NCUT) ) * EXPBEA(NCUT)
     $                            + BEM(IQ) * BPLANK
     $                            - ZPLK0(IQ+NN,NCUT)
     $                            - ZPLK1(IQ+NN,NCUT) * TAUCPR(NCUT)
180            CONTINUE
            END IF
            IT = NN
            DO 200  LC = 1, NCUT-1
               DO 190  IQ = 1, NSTR
                  IT    = IT + 1
                  B(IT) = ( ZZ(IQ,LC+1) - ZZ(IQ,LC) ) * EXPBEA(LC)
     $                    + ZPLK0(IQ,LC+1) - ZPLK0(IQ,LC) +
     $                    ( ZPLK1(IQ,LC+1) - ZPLK1(IQ,LC) ) * TAUCPR(LC)
190            CONTINUE
200         CONTINUE
         END IF

      END IF
C                     ** FIND L-U (LOWER/UPPER TRIANGULAR) DECOMPOSITION
C                     ** OF BAND MATRIX -CBAND- AND TEST IF IT IS NEARLY
C                     ** SINGULAR (NOTE: -CBAND- IS DESTROYED)
C                     ** (-CBAND- IS IN LINPACK PACKED FORMAT)
      RCOND = 0.0
      NCD = 3*NN - 1
      CALL  dgbco( CBAND, MI9M2, NCOL, NCD, NCD, IPVT, RCOND, Z )
      IF ( 1.0+RCOND .EQ. 1.0 )  CALL  ERRMSG
     $   ( 'SOLVE0--dGBCO SAYS MATRIX NEAR SINGULAR',.FALSE.)
C                   ** SOLVE LINEAR SYSTEM WITH COEFF MATRIX -CBAND-
C                   ** AND R.H. SIDE(S) -B- AFTER -CBAND- HAS BEEN L-U
C                   ** DECOMPOSED.  SOLUTION IS RETURNED IN -B-.
      CALL  dGBSL( CBAND, MI9M2, NCOL, NCD, NCD, IPVT, B, 0 )
C                   ** ZERO -CBAND- (IT MAY CONTAIN 'FOREIGN'
C                   ** ELEMENTS UPON RETURNING FROM LINPACK);
C                   ** NECESSARY TO PREVENT ERRORS

      CALL  ZEROIT( CBAND, MI9M2*NNLYRI )
      DO 220  LC = 1, NCUT
         IPNT = LC*NSTR - NN
         DO 220  IQ = 1, NN
            LL(NN+1-IQ,LC) = B(IPNT+1-IQ)
            LL(IQ+NN,  LC) = B(IQ+IPNT)
220   CONTINUE
      RETURN
      END
      SUBROUTINE  SOLVE1( B, CBAND, FISOT, IHOM, IPVT, LL, MI9M2, MXCMU,
     $                    NCOL, NCUT, NN, NNLYRI, NSTR, Z )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C        CONSTRUCT RIGHT-HAND SIDE VECTOR -B- FOR ISOTROPIC INCIDENCE
C        (ONLY) ON EITHER TOP OR BOTTOM BOUNDARY AND SOLVE SYSTEM
C        OF EQUATIONS OBTAINED FROM THE BOUNDARY CONDITIONS AND THE
C        CONTINUITY-OF-INTENSITY-AT-LAYER-INTERFACE EQUATIONS

C     ROUTINES CALLED:  dGBCO, dGBSL, ZEROIT

C     I N P U T      V A R I A B L E S:

C       CBAND    :  LEFT-HAND SIDE MATRIX OF LINEAR SYSTEM EQ. SC(5),
C                   SCALED BY EQ. SC(12); IN BANDED FORM REQUIRED
C                   BY LINPACK SOLUTION ROUTINES
C       IHOM     :  DIRECTION OF ILLUMINATION FLAG
C       NCOL     :  COUNTS OF COLUMNS IN -CBAND-
C       NN       :  ORDER OF DOUBLE-GAUSS QUADRATURE (NSTR/2)
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)

C    O U T P U T     V A R I A B L E S:

C       B        :  RIGHT-HAND SIDE VECTOR OF EQ. SC(5) GOING INTO
C                   *dGBSL*; RETURNS AS SOLUTION VECTOR OF EQ.
C                   SC(12), CONSTANTS OF INTEGRATION WITHOUT
C                   EXPONENTIAL TERM
C       LL      :   PERMANENT STORAGE FOR -B-, BUT RE-ORDERED

C   I N T E R N A L    V A R I A B L E S:

C       IPVT     :  INTEGER VECTOR OF PIVOT INDICES
C       NCD      :  NUMBER OF DIAGONALS BELOW OR ABOVE MAIN DIAGONAL
C       RCOND    :  INDICATOR OF SINGULARITY FOR -CBAND-
C       Z        :  SCRATCH ARRAY REQUIRED BY *dGBCO*
C----------------------------------------------------------------------+
      INTEGER  IPVT(*)
      real*8     B( NNLYRI ), CBAND( MI9M2,NNLYRI ), LL( MXCMU,* ), Z(*)


      CALL  ZEROIT( B, NNLYRI )
      NCD = 3*NN - 1

      IF ( IHOM.EQ.1 )  THEN
C                             ** BECAUSE THERE ARE NO BEAM OR EMISSION
C                             ** SOURCES, REMAINDER OF -B- ARRAY IS ZERO
         DO 10  I = 1, NN
            B(I) = FISOT
            B( NCOL-NN+I ) = 0.0
10       CONTINUE

         RCOND = 0.0
         CALL  dGBCO( CBAND, MI9M2, NCOL, NCD, NCD, IPVT, RCOND, Z )
         IF ( 1.0+RCOND .EQ. 1.0 )  CALL  ERRMSG
     $         ( 'SOLVE1--dGBCO SAYS MATRIX NEAR SINGULAR', .FALSE. )

      ELSE IF ( IHOM.EQ.2 )  THEN

         DO 20 I = 1, NN
            B(I) = 0.0
            B( NCOL-NN+I ) = FISOT
20       CONTINUE

      END IF

      CALL  dGBSL( CBAND, MI9M2, NCOL, NCD, NCD, IPVT, B, 0 )

C                          ** ZERO -CBAND- TO GET RID OF 'FOREIGN'
C                          ** ELEMENTS PUT IN BY LINPACK
      DO 30  LC = 1, NCUT
         IPNT = LC*NSTR - NN
         DO 30  IQ = 1, NN
            LL( NN+1-IQ, LC) = B( IPNT+1-IQ )
            LL( IQ+NN,   LC) = B( IQ+IPNT )
30    CONTINUE

      RETURN
      END
      SUBROUTINE  SPALTR( CMU, CWT, GC, KK, LL, MXCMU, NLYR,
     $                    NN, NSTR, TAUCPR, SFLUP, SFLDN )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C       CALCULATES SPHERICAL ALBEDO AND TRANSMISSIVITY FOR THE ENTIRE
C       MEDIUM FROM THE M=0 INTENSITY COMPONENTS
C       (THIS IS A VERY SPECIALIZED VERSION OF 'FLUXES')

C    I N P U T    V A R I A B L E S:

C       CMU     :  ABSCISSAE FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       CWT     :  WEIGHTS FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       KK      :  EIGENVALUES OF COEFF. MATRIX IN EQ. SS(7)
C       GC      :  EIGENVECTORS AT POLAR QUADRATURE ANGLES, SC(1)
C       LL      :  CONSTANTS OF INTEGRATION IN EQ. SC(1), OBTAINED
C                  BY SOLVING SCALED VERSION OF EQ. SC(5);
C                  EXPONENTIAL TERM OF EQ. SC(12) NOT INCLUDED
C       NN      :  ORDER OF DOUBLE-GAUSS QUADRATURE (NSTR/2)
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)

C    O U T P U T   V A R I A B L E S:

C       SFLUP   :  UP-FLUX AT TOP (EQUIVALENT TO SPHERICAL ALBEDO DUE TO
C                  RECIPROCITY).  FOR ILLUMINATION FROM BELOW IT GIVES
C                  SPHERICAL TRANSMISSIVITY
C       SFLDN   :  DOWN-FLUX AT BOTTOM (FOR SINGLE LAYER
C                  EQUIVALENT TO SPHERICAL TRANSMISSIVITY
C                  DUE TO RECIPROCITY)

C    I N T E R N A L   V A R I A B L E S:

C       ZINT    :  INTENSITY OF M=0 CASE, IN EQ. SC(1)
C+----------------------------------------------------------------------

      real*8  CMU(*), CWT(*), GC( MXCMU,MXCMU,* ), KK( MXCMU,* ),
     $      LL( MXCMU,* ), TAUCPR( 0:* )


      SFLUP = 0.0
      DO 20  IQ = NN+1, NSTR
         ZINT  = 0.0
         DO 10   JQ = 1, NN
            ZINT = ZINT + GC(IQ,JQ,1) * LL(JQ,1) *
     $                    dexp( KK(JQ,1) * TAUCPR(1) )
10       CONTINUE
         DO 11  JQ = NN+1, NSTR
            ZINT = ZINT + GC(IQ,JQ,1) * LL(JQ,1)
11       CONTINUE

         SFLUP = SFLUP + CWT(IQ-NN) * CMU(IQ-NN) * ZINT
20    CONTINUE

      SFLDN  = 0.0
      DO 40  IQ = 1, NN
         ZINT   = 0.0
         DO 30  JQ = 1, NN
             ZINT = ZINT + GC(IQ,JQ,NLYR) * LL(JQ,NLYR)
30       CONTINUE
         DO 31  JQ = NN+1, NSTR
             ZINT = ZINT + GC(IQ,JQ,NLYR) * LL(JQ,NLYR) *
     $         dexp( - KK(JQ,NLYR)*(TAUCPR(NLYR) - TAUCPR(NLYR-1)) )
31       CONTINUE

         SFLDN = SFLDN + CWT(NN+1-IQ) * CMU(NN+1-IQ) * ZINT
40    CONTINUE

      SFLUP = 2.0 * SFLUP
      SFLDN = 2.0 * SFLDN

      RETURN
      END
      SUBROUTINE  SURFAC( ALBEDO, DELM0, FBEAM, HLPR, LAMBER,
     $                    MI, MAZIM, MXCMU, MXUMU, NN, NUMU, NSTR, 
     $                    ONLYFL, UMU, USRANG, YLM0, YLMC, YLMU, BDR, 
     $                    EMU, BEM, RMU )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C       SPECIFIES USER'S SURFACE BIDIRECTIONAL PROPERTIES, STWJ(21)

C   I N P U T     V A R I A B L E S:

C       DELM0  :  KRONECKER DELTA, DELTA-SUB-M0
C       HLPR   :  LEGENDRE MOMENTS OF SURFACE BIDIRECTIONAL REFLECTIVITY
C                    (WITH 2K+1 FACTOR INCLUDED)
C       MAZIM  :  ORDER OF AZIMUTHAL COMPONENT
C       NN     :  ORDER OF DOUBLE-GAUSS QUADRATURE (NSTR/2)
C       YLM0   :  NORMALIZED ASSOCIATED LEGENDRE POLYNOMIAL
C                 AT THE BEAM ANGLE
C       YLMC   :  NORMALIZED ASSOCIATED LEGENDRE POLYNOMIALS
C                 AT THE QUADRATURE ANGLES
C       YLMU   :  NORMALIZED ASSOCIATED LEGENDRE POLYNOMIALS
C                 AT THE USER ANGLES
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)

C    O U T P U T     V A R I A B L E S:

C       BDR :  SURFACE BIDIRECTIONAL REFLECTIVITY (COMPUTATIONAL ANGLES)
C       RMU :  SURFACE BIDIRECTIONAL REFLECTIVITY (USER ANGLES)
C       BEM :  SURFACE DIRECTIONAL EMISSIVITY (COMPUTATIONAL ANGLES)
C       EMU :  SURFACE DIRECTIONAL EMISSIVITY (USER ANGLES)

C    I N T E R N A L     V A R I A B L E S:

C       DREF      DIRECTIONAL REFLECTIVITY
C       NMUG   :  NUMBER OF ANGLE COSINE QUADRATURE POINTS
C                 ON (0,1) FOR INTEGRATING BIDIRECTIONAL REFLECTIVITY
C                 TO GET DIRECTIONAL EMISSIVITY (IT IS NECESSARY TO USE
C                 A QUADRATURE SET DISTINCT FROM THE COMPUTATIONAL
C                 ANGLES, BECAUSE THE COMPUTATIONAL ANGLES MAY NOT BE
C                 DENSE ENOUGH -- I.E. 'NSTR' MAY BE TOO SMALL-- TO GIVE
C                 AN ACCURATE APPROXIMATION FOR THE INTEGRATION).
C       GMU    :  THE 'NMUG' ANGLE COSINE QUADRATURE POINTS ON (0,1)
C       GWT    :  THE 'NMUG' ANGLE COSINE QUADRATURE WEIGHTS ON (0,1)
C       YLMG   :  NORMALIZED ASSOCIATED LEGENDRE POLYNOMIALS
C                 AT THE 'NMUG' QUADRATURE ANGLES
C+---------------------------------------------------------------------+
      LOGICAL  LAMBER, ONLYFL, USRANG
      real*8     BDR( MI,0:* ), BEM(*), EMU(*),
     $         HLPR(0:*), RMU( MXUMU,0:* ), UMU(*),
     $         YLM0(0:*), YLMC( 0:MXCMU,* ), YLMU( 0:MXCMU,* )
      PARAMETER  ( NMUG = 10, MAXSTR = 100 )
      LOGICAL  PASS1
      real*8     GMU( NMUG ), GWT( NMUG ), YLMG( 0:MAXSTR, NMUG )
      SAVE  PASS1, GMU, GWT, YLMG
      DATA  PASS1 / .TRUE. /


      IF ( PASS1 )  THEN
         PASS1 = .FALSE.
         CALL QGAUSN( NMUG, GMU, GWT )

         CALL LEPOLY( NMUG, 0, MAXSTR, MAXSTR, GMU, YLMG )
C                       ** CONVERT LEGENDRE POLYS. TO NEGATIVE -GMU-
         SGN  = - 1.0
         DO 1  K = 0, MAXSTR
            SGN = - SGN
            DO 1  JG = 1, NMUG
               YLMG( K,JG ) = SGN * YLMG( K,JG )
 1       CONTINUE

      END IF

      CALL  ZEROIT( BDR, MI*(MI+1) )
      CALL  ZEROIT( BEM, MI )

      IF ( LAMBER .AND. MAZIM.EQ.0 ) THEN

         DO 20 IQ = 1, NN
            BEM(IQ) = 1.0 - ALBEDO
            DO 20 JQ = 0, NN
               BDR(IQ,JQ) = ALBEDO
20       CONTINUE

      ELSE IF ( .NOT.LAMBER ) THEN
C                                  ** COMPUTE SURFACE BIDIRECTIONAL
C                                  ** PROPERTIES AT COMPUTATIONAL ANGLES
         DO 60 IQ = 1, NN

            DO 40 JQ = 1, NN
              SUM = 0.0
              DO 30 K = MAZIM, NSTR-1
                 SUM = SUM + HLPR(K) * YLMC(K,IQ) * YLMC(K,JQ+NN)
30            CONTINUE
              BDR(IQ,JQ) = (2.-DELM0) * SUM
40          CONTINUE

            IF ( FBEAM.GT.0.0 )  THEN
               SUM = 0.0
               DO 50 K = MAZIM, NSTR-1
                  SUM = SUM + HLPR(K) * YLMC(K,IQ) * YLM0(K)
50             CONTINUE
               BDR(IQ,0) = (2.-DELM0) * SUM
            ENDIF

60       CONTINUE

         IF ( MAZIM.EQ.0 ) THEN

            IF ( NSTR.GT.MAXSTR )  CALL
     $           ERRMSG( 'SURFAC--PARAMETER MAXSTR TOO SMALL', .TRUE. )

C                              ** INTEGRATE BIDIRECTIONAL REFLECTIVITY
C                              ** AT REFLECTION POLAR ANGLES -CMU- AND
C                              ** INCIDENT ANGLES -GMU- TO GET
C                              ** DIRECTIONAL EMISSIVITY AT
C                              ** COMPUTATIONAL ANGLES -CMU-.
            DO 100  IQ = 1, NN
               DREF = 0.0
               DO 90  JG = 1, NMUG
                  SUM = 0.0
                  DO 80  K = 0, NSTR-1
                     SUM = SUM + HLPR(K) * YLMC(K,IQ) * YLMG(K,JG)
80                CONTINUE
                  DREF = DREF + 2.* GWT(JG) * GMU(JG) * SUM
90             CONTINUE
               BEM(IQ) = 1.0 - DREF
100         CONTINUE

         END IF

      END IF
C                                       ** COMPUTE SURFACE BIDIRECTIONAL
C                                       ** PROPERTIES AT USER ANGLES

      IF ( .NOT.ONLYFL .AND. USRANG )  THEN

         CALL  ZEROIT( EMU, MXUMU )
         CALL  ZEROIT( RMU, MXUMU*(MI+1) )

         DO 170 IU = 1, NUMU
            IF ( UMU(IU).GT.0.0 )  THEN

               IF ( LAMBER .AND. MAZIM.EQ.0 )  THEN
                  DO 110 IQ = 0, NN
                     RMU(IU,IQ) = ALBEDO
110               CONTINUE
                  EMU(IU) = 1.0 - ALBEDO

               ELSE IF ( .NOT.LAMBER ) THEN
                  DO 130 IQ = 1, NN
                     SUM = 0.0
                     DO 120 K = MAZIM, NSTR-1
                        SUM = SUM + HLPR(K) * YLMU(K,IU) * YLMC(K,IQ+NN)
120                  CONTINUE
                     RMU(IU,IQ) = (2.-DELM0) * SUM
130               CONTINUE

                  IF ( FBEAM.GT.0.0 )  THEN
                     SUM = 0.0
                     DO 140 K = MAZIM, NSTR-1
                        SUM = SUM + HLPR(K) * YLMU(K,IU) * YLM0(K)
140                  CONTINUE
                     RMU(IU,0) = (2.-DELM0) * SUM
                  END IF

                  IF ( MAZIM.EQ.0 ) THEN

C                               ** INTEGRATE BIDIRECTIONAL REFLECTIVITY
C                               ** AT REFLECTION ANGLES -UMU- AND
C                               ** INCIDENT ANGLES -GMU- TO GET
C                               ** DIRECTIONAL EMISSIVITY AT
C                               ** USER ANGLES -UMU-.
                     DREF = 0.0
                     DO 160 JG = 1, NMUG
                        SUM = 0.0
                        DO 150 K = 0, NSTR-1
                           SUM = SUM + HLPR(K) * YLMU(K,IU) * YLMG(K,JG)
150                     CONTINUE
                        DREF = DREF + 2.* GWT(JG) * GMU(JG) * SUM
160                  CONTINUE

                     EMU(IU) = 1.0 - DREF
                  END IF

               END IF
            END IF
170      CONTINUE

      END IF

      RETURN
      END
      SUBROUTINE  TERPEV( CWT, EVECC, GL, GU, MAZIM, MXCMU, MXUMU,
     $                    NN, NSTR, NUMU, WK, YLMC, YLMU )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C         INTERPOLATE EIGENVECTORS TO USER ANGLES; EQ SD(8)

      real*8  CWT(*), EVECC( MXCMU,* ), GL(0:*), GU(  MXUMU,* ), WK(*),
     $      YLMC(  0:MXCMU,* ), YLMU(  0:MXCMU,* )


      DO 50  IQ = 1, NSTR

         DO 20  L = MAZIM, NSTR-1
C                                       ** INNER SUM IN SD(8) TIMES ALL
C                                   ** FACTORS IN OUTER SUM BUT PLM(MU)
            SUM = 0.0
            DO 10  JQ = 1, NSTR
               SUM = SUM + CWT(JQ) * YLMC(L,JQ) * EVECC(JQ,IQ)
10          CONTINUE
            WK(L+1) = 0.5 * GL(L) * SUM
20       CONTINUE
C                                    ** FINISH OUTER SUM IN SD(8)
C                                    ** AND STORE EIGENVECTORS
         DO 40  IU = 1, NUMU
            SUM = 0.
            DO 30  L = MAZIM, NSTR-1
               SUM = SUM + WK(L+1) * YLMU(L,IU)
30          CONTINUE
            IF ( IQ.LE.NN )  GU( IU, IQ+NN     ) = SUM
            IF ( IQ.GT.NN )  GU( IU, NSTR+1-IQ ) = SUM
40       CONTINUE

50    CONTINUE

      RETURN
      END
      SUBROUTINE  TERPSO( CWT, DELM0, FBEAM, GL, MAZIM, MXCMU,
     $                      PLANK, NUMU, NSTR, OPRIM, PI, YLM0, YLMC,
     $                      YLMU, PSIX, XR0, XR1, Z0, ZJ, ZBEAM, Z0U,
     $                      Z1U, z0ums, z1ums, zbeamms )
c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
c       lower case variables added

C         INTERPOLATES SOURCE FUNCTIONS TO USER ANGLES

C    I N P U T      V A R I A B L E S:

C       CWT    :  WEIGHTS FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       DELM0  :  KRONECKER DELTA, DELTA-SUB-M0
C       GL     :  DELTA-M SCALED LEGENDRE COEFFICIENTS OF PHASE FUNCTION
C                    (INCLUDING FACTORS 2L+1 AND SINGLE-SCATTER ALBEDO)
C       MAZIM  :  ORDER OF AZIMUTHAL COMPONENT
C       OPRIM  :  SINGLE SCATTERING ALBEDO
C       XR0    :  EXPANSION OF THERMAL SOURCE FUNCTION
C       XR1    :  EXPANSION OF THERMAL SOURCE FUNCTION EQS.SS(14-16)
C       YLM0   :  NORMALIZED ASSOCIATED LEGENDRE POLYNOMIAL
C                 AT THE BEAM ANGLE
C       YLMC   :  NORMALIZED ASSOCIATED LEGENDRE POLYNOMIAL
C                 AT THE QUADRATURE ANGLES
C       YLMU   :  NORMALIZED ASSOCIATED LEGENDRE POLYNOMIAL
C                 AT THE USER ANGLES
C       Z0     :  SOLUTION VECTORS Z-SUB-ZERO OF EQ. SS(16)
C       ZJ     :  SOLUTION VECTOR CAPITAL -Z-SUB-ZERO AFTER SOLVING
C                 EQ. SS(19)
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)

C    O U T P U T     V A R I A B L E S:

C       ZBEAM  :  INCIDENT-BEAM SOURCE FUNCTION AT USER ANGLES
C       Z0U,Z1U:  COMPONENTS OF A LINEAR-IN-OPTICAL-DEPTH-DEPENDENT
C                    SOURCE (APPROXIMATING THE PLANCK EMISSION SOURCE)

C   I N T E R N A L       V A R I A B L E S:

C       PSIX    :  SUM JUST AFTER SQUARE BRACKET IN  EQ. SD(9)
C+---------------------------------------------------------------------+
      LOGICAL  PLANK
      real*8   CWT(*), GL(0:*), PSIX(*),  YLM0(0:*), YLMC( 0:MXCMU,* ),
     $         YLMU( 0:MXCMU,*), Z0(*), ZJ(*), ZBEAM(*), Z0U(*),
     $         Z1U(*), zbeamms(*), z1ums(*), z0ums(*), sum1
c              lower case variables added


      IF ( FBEAM.GT.0.0 )  THEN
C                                  ** BEAM SOURCE TERMS; EQ. SD(9)
         DO 20  IQ = MAZIM, NSTR-1
            PSUM = 0.
            DO 10  JQ = 1, NSTR
               PSUM = PSUM + CWT(JQ) * YLMC(IQ,JQ) * ZJ(JQ)
10          CONTINUE
            PSIX(IQ+1) = 0.5 * GL(IQ) * PSUM
20       CONTINUE

         FACT = ( 2. - DELM0 ) * FBEAM / (4.0*PI)
         DO 40  IU = 1, NUMU
            SUM = 0.
c             lower case variables added
              sum1= 0.
            DO 30 IQ = MAZIM, NSTR-1
               SUM = SUM + YLMU(IQ,IU) *
     $                    ( PSIX(IQ+1) + FACT * GL(IQ) * YLM0(IQ) )
c                lower case variables added
                 sum1 = sum1 + ylmu(iq,iu) * ( psix(iq+1) )
30            CONTINUE
            ZBEAM(IU) = SUM
c             lower case variables added
              zbeamms(iu)=sum1 
40         CONTINUE
      END IF

      IF ( PLANK .AND. MAZIM.EQ.0 )  THEN

C                                ** THERMAL SOURCE TERMS, STWJ(27C)
         DO 80  IQ = MAZIM, NSTR-1
            PSUM = 0.0
            DO 70  JQ = 1, NSTR
               PSUM = PSUM + CWT(JQ) * YLMC(IQ,JQ) * Z0(JQ)
 70           CONTINUE
            PSIX(IQ+1) = 0.5 * GL(IQ) * PSUM
 80        CONTINUE

         DO 100  IU = 1, NUMU
            SUM = 0.0
            DO 90   IQ = MAZIM, NSTR-1
               SUM = SUM + YLMU(IQ,IU) * PSIX(IQ+1)
90            CONTINUE
c             lower case variables added
              z0ums(iu)=sum
              z1ums(iu)=oprim*xr1
            Z0U(IU) = SUM + (1.-OPRIM) * XR0
            Z1U(IU) = XR1
100        CONTINUE

      END IF

      RETURN
      END
      SUBROUTINE  UPBEAM( ARRAY, CC, CMU, DELM0, FBEAM, GL, IPVT, MAZIM,
     $                    MXCMU, NN, NSTR, PI, UMU0, WK, YLM0, YLMC, ZJ,
     $                    ZZ )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C         FINDS THE INCIDENT-BEAM PARTICULAR SOLUTION  OF SS(18)

C     ROUTINES CALLED:  dGECO, dGESL

C   I N P U T    V A R I A B L E S:

C       CC     :  CAPITAL-C-SUB-IJ IN EQ. SS(5)
C       CMU    :  ABSCISSAE FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       DELM0  :  KRONECKER DELTA, DELTA-SUB-M0
C       GL     :  DELTA-M SCALED LEGENDRE COEFFICIENTS OF PHASE FUNCTION
C                    (INCLUDING FACTORS 2L+1 AND SINGLE-SCATTER ALBEDO)
C       MAZIM  :  ORDER OF AZIMUTHAL COMPONENT
C       YLM0   :  NORMALIZED ASSOCIATED LEGENDRE POLYNOMIAL
C                 AT THE BEAM ANGLE
C       YLMC   :  NORMALIZED ASSOCIATED LEGENDRE POLYNOMIAL
C                 AT THE QUADRATURE ANGLES
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)

C   O U T P U T    V A R I A B L E S:

C       ZJ     :  RIGHT-HAND SIDE VECTOR CAPITAL-X-SUB-ZERO IN SS(19);
C                 ALSO THE SOLUTION VECTOR CAPITAL-Z-SUB-ZERO
C                 AFTER SOLVING THAT SYSTEM
C       ZZ     :  PERMANENT STORAGE FOR -ZJ-, BUT RE-ORDERED

C   I N T E R N A L    V A R I A B L E S:

C       ARRAY  :  COEFFICIENT MATRIX IN LEFT-HAND SIDE OF EQ. SS(19)
C       IPVT   :  INTEGER VECTOR OF PIVOT INDICES REQUIRED BY *LINPACK*
C       WK     :  SCRATCH ARRAY REQUIRED BY *LINPACK*
C+---------------------------------------------------------------------+

      INTEGER  IPVT(*)
      real*8     ARRAY( MXCMU,* ), CC( MXCMU,* ), CMU(*), GL(0:*),
     $         WK(*), YLM0(0:*), YLMC( 0:MXCMU,* ), ZJ(*), ZZ(*)

      DO 40  IQ = 1, NSTR

         DO 10  JQ = 1, NSTR
            ARRAY(IQ,JQ) = - CC(IQ,JQ)
10       CONTINUE
         ARRAY(IQ,IQ) = 1. + CMU(IQ) / UMU0 + ARRAY(IQ,IQ)

         SUM = 0.
         DO 20  K = MAZIM, NSTR-1
            SUM = SUM + GL(K) * YLMC(K,IQ) * YLM0(K)
20       CONTINUE
         ZJ(IQ) = ( 2. - DELM0 ) * FBEAM * SUM / (4.0*PI)
40    CONTINUE

C                  ** FIND L-U (LOWER/UPPER TRIANGULAR) DECOMPOSITION
C                  ** OF -ARRAY- AND SEE IF IT IS NEARLY SINGULAR
C                  ** (NOTE:  -ARRAY- IS DESTROYED)
      RCOND = 0.0
      CALL  dgeco( ARRAY, MXCMU, NSTR, IPVT, RCOND, WK )
      IF ( 1.0+RCOND .EQ. 1.0 )  CALL  ERRMSG
     $   ( 'UPBEAM--dGECO SAYS MATRIX NEAR SINGULAR',.FALSE.)

C                ** SOLVE LINEAR SYSTEM WITH COEFF MATRIX -ARRAY-
C                ** (ASSUMED ALREADY L-U DECOMPOSED) AND R.H. SIDE(S)
C                ** -ZJ-;  RETURN SOLUTION(S) IN -ZJ-
      JOB =   0
      CALL  dGESL( ARRAY, MXCMU, NSTR, IPVT, ZJ, JOB )
      DO 50  IQ = 1, NN
         ZZ( IQ+NN )   = ZJ( IQ )
         ZZ( NN+1-IQ ) = ZJ( IQ+NN )
50    CONTINUE
      RETURN
      END
      SUBROUTINE  UPISOT( ARRAY, CC, CMU, IPVT, MXCMU, NN, NSTR, OPRIM,
     $                    WK, XR0, XR1, Z0, Z1, ZPLK0, ZPLK1 )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C       FINDS THE PARTICULAR SOLUTION OF THERMAL RADIATION OF SS(15)

C     ROUTINES CALLED:  dGECO, dGESL

C   I N P U T     V A R I A B L E S:

C       CC     :  CAPITAL-C-SUB-IJ IN EQ. SS(5)
C       CMU    :  ABSCISSAE FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       OPRIM  :  DELTA-M SCALED SINGLE SCATTERING ALBEDO
C       XR0    :  EXPANSION OF THERMAL SOURCE FUNCTION
C       XR1    :  EXPANSION OF THERMAL SOURCE FUNCTION EQS. SS(14-16)
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)

C    O U T P U T    V A R I A B L E S:

C       Z0     :  SOLUTION VECTORS Z-SUB-ZERO OF EQ. SS(16)
C       Z1     :  SOLUTION VECTORS Z-SUB-ONE  OF EQ. SS(16)
C       ZPLK0, :  PERMANENT STORAGE FOR -Z0,Z1-, BUT RE-ORDERED
C        ZPLK1

C   I N T E R N A L    V A R I A B L E S:

C       ARRAY  :  COEFFICIENT MATRIX IN LEFT-HAND SIDE OF EQ. SS(16)
C       IPVT   :  INTEGER VECTOR OF PIVOT INDICES REQUIRED BY *LINPACK*
C       WK     :  SCRATCH ARRAY REQUIRED BY *LINPACK*
C+---------------------------------------------------------------------+

      INTEGER IPVT(*)
      real*8    ARRAY( MXCMU,* ), CC( MXCMU,* ), CMU(*), WK(*),
     $        Z0(*), Z1(*), ZPLK0(*), ZPLK1(*)


      DO 20 IQ = 1, NSTR

         DO 10 JQ = 1, NSTR
            ARRAY(IQ,JQ) = - CC(IQ,JQ)
10       CONTINUE
         ARRAY(IQ,IQ) = 1.0 + ARRAY(IQ,IQ)

         Z1(IQ) = XR1
         Z0(IQ) = (1.-OPRIM) * XR0 + CMU(IQ) * Z1(IQ)
20    CONTINUE
C                       ** SOLVE LINEAR EQUATIONS: SAME AS IN *UPBEAM*,
C                       ** EXCEPT -ZJ- REPLACED BY -Z0-
      RCOND = 0.0
      CALL  dGECO( ARRAY, MXCMU, NSTR, IPVT, RCOND, WK )
      IF ( 1.0+RCOND .EQ. 1.0 )  CALL  ERRMSG
     $   ( 'UPISOT--dGECO SAYS MATRIX NEAR SINGULAR',.FALSE.)

      CALL  dGESL( ARRAY, MXCMU, NSTR, IPVT, Z0, 0 )

      DO 30  IQ = 1, NN
         ZPLK0( IQ+NN )   = Z0( IQ )
         ZPLK1( IQ+NN )   = Z1( IQ )
         ZPLK0( NN+1-IQ ) = Z0( IQ+NN )
         ZPLK1( NN+1-IQ ) = Z1( IQ+NN )
30    CONTINUE

      RETURN
      END
      SUBROUTINE  USRINT( BPLANK, CMU, CWT, DELM0, EMU, EXPBEA,
     $                    FBEAM, FISOT, GC, GU, KK, LAMBER, LAYRU, LL,
     $                    LYRCUT, MAZIM, MXCMU, MXULV, MXUMU, NCUT,
     $                    NLYR, NN, NSTR, PLANK, NUMU, NTAU, PI, RMU,
     $                    TAUCPR, TPLANK, UMU, UMU0, UTAUPR, WK,
     $                    ZBEAM, Z0U, Z1U, ZZ, ZPLK0, ZPLK1, UUM )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C       COMPUTES INTENSITY COMPONENTS AT USER OUTPUT ANGLES
C       FOR AZIMUTHAL EXPANSION TERMS IN EQ. SD(2)

C   I N P U T    V A R I A B L E S:

C       BPLANK :  INTEGRATED PLANCK FUNCTION FOR EMISSION FROM
C                 BOTTOM BOUNDARY
C       CMU    :  ABSCISSAE FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       CWT    :  WEIGHTS FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       DELM0  :  KRONECKER DELTA, DELTA-SUB-M0
C       EMU    :  SURFACE DIRECTIONAL EMISSIVITY (USER ANGLES)
C       EXPBEA :  TRANSMISSION OF INCIDENT BEAM, EXP(-TAUCPR/UMU0)
C       GC     :  EIGENVECTORS AT POLAR QUADRATURE ANGLES, SC(1)
C       GU     :  EIGENVECTORS INTERPOLATED TO USER POLAR ANGLES
C                 (i.e., g IN EQ. SC(1) )
C       KK     :  EIGENVALUES OF COEFF. MATRIX IN EQ. SS(7)
C       LAYRU  :  LAYER NUMBER OF USER LEVEL -UTAU-
C       LL     :  CONSTANTS OF INTEGRATION IN EQ. SC(1), OBTAINED
C                 BY SOLVING SCALED VERSION OF EQ. SC(5);
C                 EXPONENTIAL TERM OF EQ. SC(12) NOT INCLUDED
C       LYRCUT :  LOGICAL FLAG FOR TRUNCATION OF COMPUT. LAYER
C       MAZIM  :  ORDER OF AZIMUTHAL COMPONENT
C       NCUT   :  TOTAL NUMBER OF COMPUTATIONAL LAYERS CONSIDERED
C       NN     :  ORDER OF DOUBLE-GAUSS QUADRATURE (NSTR/2)
C       RMU    :  SURFACE BIDIRECTIONAL REFLECTIVITY (USER ANGLES)
C       TAUCPR :  CUMULATIVE OPTICAL DEPTH (DELTA-M-SCALED)
C       TPLANK :  INTEGRATED PLANCK FUNCTION FOR EMISSION FROM
C                 TOP BOUNDARY
C       UTAUPR :  OPTICAL DEPTHS OF USER OUTPUT LEVELS IN DELTA-M
C                    COORDINATES;  EQUAL TO  -UTAU- IF NO DELTA-M
C       Z0U    :  Z-SUB-ZERO IN EQ. SS(16) INTERPOLATED TO USER
C                 ANGLES FROM AN EQUATION DERIVED FROM SS(16)
C       Z1U    :  Z-SUB-ONE IN EQ. SS(16) INTERPOLATED TO USER
C                 ANGLES FROM AN EQUATION DERIVED FROM SS(16)
C       ZZ     :  BEAM SOURCE VECTORS IN EQ. SS(19)
C       ZPLK0  :  THERMAL SOURCE VECTORS -Z0-, BY SOLVING EQ. SS(16)
C       ZPLK1  :  THERMAL SOURCE VECTORS -Z1-, BY SOLVING EQ. SS(16)
C       ZBEAM  :  INCIDENT-BEAM SOURCE VECTORS
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)

C   O U T P U T    V A R I A B L E S:

C       UUM  :  AZIMUTHAL COMPONENTS OF THE INTENSITY IN EQ. STWJ(5)

C   I N T E R N A L    V A R I A B L E S:

C       BNDDIR :  DIRECT INTENSITY DOWN AT THE BOTTOM BOUNDARY
C       BNDDFU :  DIFFUSE INTENSITY DOWN AT THE BOTTOM BOUNDARY
C       BNDINT :  INTENSITY ATTENUATED AT BOTH BOUNDARIES, STWJ(25-6)
C       DTAU   :  OPTICAL DEPTH OF A COMPUTATIONAL LAYER
C       LYREND :  END LAYER OF INTEGRATION
C       LYRSTR :  START LAYER OF INTEGRATION
C       PALINT :  INTENSITY COMPONENT FROM PARALLEL BEAM
C       PLKINT :  INTENSITY COMPONENT FROM PLANCK SOURCE
C       WK     :  SCRATCH VECTOR FOR SAVING 'EXP' EVALUATIONS
C       ALL THE EXPONENTIAL FACTORS ( EXP1, EXPN,... etc.)
C       COME FROM THE SUBSTITUTION OF CONSTANTS OF INTEGRATION IN
C       EQ. SC(12) INTO EQS. S1(8-9).  THEY ALL HAVE NEGATIVE
C       ARGUMENTS SO THERE SHOULD NEVER BE OVERFLOW PROBLEMS.
C+---------------------------------------------------------------------+

      LOGICAL  LAMBER, LYRCUT, PLANK, NEGUMU
      INTEGER  LAYRU(*)
      real*8 CMU(*),CWT(*),EMU(*),EXPBEA(0:*),GC( MXCMU,MXCMU,* ),
     $         GU( MXUMU,MXCMU,* ), KK( MXCMU,* ), LL( MXCMU,* ),
     $         RMU( MXUMU,0:* ), TAUCPR( 0:* ), UUM( MXUMU,MXULV,0:* ),
     $         UMU(*), UTAUPR(*), WK(*), Z0U( MXUMU,* ), Z1U( MXUMU,* ),
     $         ZBEAM( MXUMU,* ), ZZ( MXCMU,* ), ZPLK0( MXCMU,* ),
     $         ZPLK1( MXCMU,* )


      CALL  ZEROIT( UUM, MXUMU*MXULV*(MXCMU+1) )

C                          ** INCORPORATE CONSTANTS OF INTEGRATION INTO
C                          ** INTERPOLATED EIGENVECTORS
      DO 10  LC = 1, NCUT
         DO  10  IQ = 1, NSTR
            DO 10  IU = 1, NUMU
               GU(IU,IQ,LC) = GU(IU,IQ,LC) * LL(IQ,LC)
10    CONTINUE
C                           ** LOOP OVER LEVELS AT WHICH INTENSITIES
C                           ** ARE DESIRED ('USER OUTPUT LEVELS')
      DO 200  LU = 1, NTAU

         IF ( FBEAM .GT. 0.0 )  EXP0 = dexp( - UTAUPR(LU) / UMU0 )
         LYU = LAYRU(LU)
C                              ** LOOP OVER POLAR ANGLES AT WHICH
C                              ** INTENSITIES ARE DESIRED
         DO 100  IU = 1, NUMU
            IF ( LYRCUT .AND. LYU.GT.NCUT )  GO TO 100
            NEGUMU = UMU(IU).LT.0.0
            IF( NEGUMU )  THEN
               LYRSTR = 1
               LYREND = LYU - 1
               SGN = - 1.0
            ELSE
               LYRSTR = LYU + 1
               LYREND = NCUT
               SGN = 1.0
            END IF
C                          ** FOR DOWNWARD INTENSITY, INTEGRATE FROM TOP
C                          ** TO 'LYU-1' IN EQ. S1(8); FOR UPWARD,
C                          ** INTEGRATE FROM BOTTOM TO 'LYU+1' IN S1(9)
            PALINT = 0.0
            PLKINT = 0.0
            DO 30  LC = LYRSTR, LYREND

               DTAU = TAUCPR(LC) - TAUCPR(LC-1)
               EXP1 =  dexp( (UTAUPR(LU) - TAUCPR(LC-1)) / UMU(IU) )
               EXP2 =  dexp( (UTAUPR(LU) - TAUCPR( LC )) / UMU(IU) )

               IF ( PLANK .AND. MAZIM.EQ.0 )
     $           PLKINT = PLKINT + SGN * ( Z0U(IU,LC) * (EXP1 - EXP2) +
     $                    Z1U(IU,LC) * ( (TAUCPR(LC-1) + UMU(IU))*EXP1 -
     $                                   (TAUCPR(LC) + UMU(IU))*EXP2 ) )

               IF ( FBEAM.GT.0.0 )  THEN
                  DENOM = 1.0 + UMU(IU) / UMU0
                  IF ( dabs(DENOM).LT.0.0001 ) THEN
C                                                   ** L'HOSPITAL LIMIT
                     EXPN = ( DTAU / UMU0 ) * EXP0
                  ELSE
                     EXPN = ( EXP1 * EXPBEA(LC-1) - EXP2 * EXPBEA(LC) )
     $                      * SGN / DENOM
                  END IF
                  PALINT = PALINT + ZBEAM(IU,LC) * EXPN
               ENDIF
C                                                   ** -KK- IS NEGATIVE
               DO 20  IQ = 1, NN
                  WK(IQ) = dexp( KK(IQ,LC) * DTAU )
                  DENOM = 1.0 + UMU(IU) * KK(IQ,LC)
                  IF ( dabs(DENOM).LT.0.0001 ) THEN
C                                                   ** L'HOSPITAL LIMIT
                     EXPN = DTAU / UMU(IU) * EXP2
                  ELSE
                     EXPN = SGN * ( EXP1 * WK(IQ) - EXP2 ) / DENOM
                  END IF
                  PALINT = PALINT + GU(IU,IQ,LC) * EXPN
20             CONTINUE
C                                                   ** -KK- IS POSITIVE
               DO 21  IQ = NN+1, NSTR
                  DENOM = 1.0 + UMU(IU) * KK(IQ,LC)
                  IF ( dabs(DENOM).LT.0.0001 ) THEN
C                                                   ** L'HOSPITAL LIMIT
                     EXPN = - DTAU / UMU(IU) * EXP1
                  ELSE
                     EXPN = SGN *( EXP1 - EXP2 * WK(NSTR+1-IQ) ) / DENOM
                  END IF
                  PALINT = PALINT + GU(IU,IQ,LC) * EXPN
21             CONTINUE

30          CONTINUE
C                           ** CALCULATE CONTRIBUTION FROM USER
C                           ** OUTPUT LEVEL TO NEXT COMPUTATIONAL LEVEL

            DTAU1 = UTAUPR(LU) - TAUCPR(LYU-1)
            DTAU2 = UTAUPR(LU) - TAUCPR(LYU)
            IF( dabs(DTAU1).LT.1.E-6 .AND. NEGUMU )  GO TO 50
            IF( dabs(DTAU2).LT.1.E-6 .AND. (.NOT.NEGUMU) )  GO TO 50
            IF( NEGUMU ) EXP1 = dexp( DTAU1 / UMU(IU) )
            IF( .NOT.NEGUMU ) EXP2 = dexp( DTAU2 / UMU(IU) )

            IF ( FBEAM.GT.0.0 )  THEN
               DENOM = 1.0 + UMU(IU) / UMU0
               IF ( dabs(DENOM).LT.0.0001 ) THEN
                  EXPN =  ( DTAU1 / UMU0 ) * EXP0
               ELSE IF ( NEGUMU ) THEN
                  EXPN = ( EXP0 - EXPBEA(LYU-1) * EXP1 ) / DENOM
               ELSE
                  EXPN = ( EXP0 - EXPBEA(LYU) * EXP2 ) / DENOM
               END IF
               PALINT = PALINT + ZBEAM(IU,LYU) * EXPN
            ENDIF
C                                                   ** -KK- IS NEGATIVE
            DTAU = TAUCPR(LYU) - TAUCPR(LYU-1)
            DO 40  IQ = 1, NN
               DENOM = 1.0 + UMU(IU) * KK(IQ,LYU)
               IF ( dabs(DENOM).LT.0.0001 ) THEN
                  EXPN = - DTAU2 / UMU(IU) * EXP2
               ELSE IF ( NEGUMU ) THEN
                  EXPN = ( dexp( - KK(IQ,LYU) * DTAU2 ) -
     $                     dexp( KK(IQ,LYU) * DTAU ) * EXP1 ) / DENOM
               ELSE
                  EXPN = ( dexp( - KK(IQ,LYU) * DTAU2 ) - EXP2 ) / DENOM
               END IF
               PALINT = PALINT + GU(IU,IQ,LYU) * EXPN
40          CONTINUE
C                                                   ** -KK- IS POSITIVE
            DO 41  IQ = NN+1, NSTR
               DENOM = 1.0 + UMU(IU) * KK(IQ,LYU)
               IF ( dabs(DENOM).LT.0.0001 ) THEN
                  EXPN = - DTAU1 / UMU(IU) * EXP1
               ELSE IF ( NEGUMU ) THEN
                  EXPN = ( dexp(- KK(IQ,LYU) * DTAU1 ) - EXP1 ) / DENOM
               ELSE
                  EXPN = ( dexp( - KK(IQ,LYU) * DTAU1 ) -
     $                     dexp( - KK(IQ,LYU) * DTAU ) * EXP2 ) / DENOM
               END IF
               PALINT = PALINT + GU(IU,IQ,LYU) * EXPN
41          CONTINUE

            IF ( PLANK .AND. MAZIM.EQ.0 )  THEN
              IF ( NEGUMU ) THEN
                 EXPN = EXP1
                 FACT = TAUCPR(LYU-1) + UMU(IU)
              ELSE
                 EXPN = EXP2
                 FACT = TAUCPR( LYU ) + UMU(IU)
              END IF
              PLKINT = PLKINT + Z0U(IU,LYU) * ( 1.- EXPN ) +
     $                 Z1U(IU,LYU) *( UTAUPR(LU) + UMU(IU) - FACT*EXPN )
            END IF
C                            ** CALCULATE INTENSITY COMPONENTS
C                            ** ATTENUATED AT BOTH BOUNDARIES.
C                            ** NOTE:: NO AZIMUTHAL INTENSITY
C                            ** COMPONENT FOR ISOTROPIC SURFACE
50          BNDINT = 0.0
            IF ( NEGUMU .AND. MAZIM.EQ.0 ) THEN
              BNDINT = ( FISOT + TPLANK ) * dexp( UTAUPR(LU) / UMU(IU) )
            ELSE IF ( .NOT.NEGUMU ) THEN
              IF ( LYRCUT .OR. (LAMBER .AND. MAZIM.GT.0) )  GO TO 90
              DO 60  JQ = NN+1, NSTR
           WK(JQ) = dexp(-KK(JQ,NLYR)*(TAUCPR(NLYR)-TAUCPR(NLYR-1)))
60            CONTINUE
              BNDDFU = 0.0
              DO 80  IQ = NN, 1, -1
                 DFUINT = 0.0
                 DO 70  JQ = 1, NN
                    DFUINT = DFUINT + GC(IQ,JQ,NLYR) * LL(JQ,NLYR)
70               CONTINUE
                 DO 71  JQ = NN+1, NSTR
                    DFUINT = DFUINT + GC(IQ,JQ,NLYR) * LL(JQ,NLYR)
     $                                * WK(JQ)
71               CONTINUE
                 IF ( FBEAM.GT.0.0 )
     $                DFUINT = DFUINT + ZZ(IQ,NLYR) * EXPBEA(NLYR)
                 DFUINT = DFUINT + DELM0 * ( ZPLK0(IQ,NLYR)
     $                                + ZPLK1(IQ,NLYR) * TAUCPR(NLYR) )
                 BNDDFU = BNDDFU + ( 1. + DELM0 ) * RMU(IU,NN+1-IQ)
     $                           * CMU(NN+1-IQ) * CWT(NN+1-IQ) * DFUINT
80            CONTINUE

              BNDDIR = 0.0
              IF (FBEAM.GT.0.0) BNDDIR = UMU0*FBEAM/PI * RMU(IU,0)
     $                                   * EXPBEA(NLYR)
              BNDINT = ( BNDDFU + BNDDIR + DELM0 * EMU(IU) * BPLANK )
     $                 * dexp( (UTAUPR(LU)-TAUCPR(NLYR)) / UMU(IU) )
            END IF

90          UUM( IU, LU, MAZIM ) = PALINT + PLKINT + BNDINT

100      CONTINUE
200   CONTINUE

      RETURN
      END
      SUBROUTINE  ZEROAL( ND1, XR0, XR1, TAUC,
     $                    ND2, CMU, CWT, PSIX, EVAL, WK, Z0, Z1, ZJ,
     $                    ND3, HLPR, YLM0,
     $                    ND4, ARRAY, CC, EVECC, YLMC,
     $                    ND5, YLMU,
     $                    ND6, AMB, APB,
     $                    ND7, KK, LL, ZZ, ZPLK0, ZPLK1,
     $                    ND8, Z0U, Z1U, ZBEAM,
     $                    ND9, GC,
     $                    ND10, GU,
     $                    ND11, Z )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C            ZERO ARRAYS

      real*8  AMB(*), APB(*), ARRAY(*), CC(*), CMU(*), CWT(*),
     $      EVAL(*), EVECC(*), GC(*), GU(*), HLPR(0:*), KK(*),
     $      LL(*), PSIX(*), TAUC(*), WK(*), XR0(*), XR1(*),
     $      YLM0(0:*), YLMC(*), YLMU(*), Z(*), Z0(*), Z1(*),
     $      Z0U(*), Z1U(*), ZJ(*), ZZ(*), ZPLK0(*), ZPLK1(*),
     $      ZBEAM(*)


      DO 2  N = 1, ND1
         XR0(N) = 0.0
         XR1(N) = 0.0
         TAUC(N) = 0.0
 2    CONTINUE

      DO 4  N = 1, ND2
         CMU(N) = 0.0
         CWT(N) = 0.0
         PSIX(N) = 0.0
         EVAL(N) = 0.0
         WK(N) = 0.0
         Z0(N) = 0.0
         Z1(N) = 0.0
         ZJ(N) = 0.0
 4    CONTINUE

      DO 6  N = 1, ND3
         HLPR(N) = 0.0
         YLM0(N) = 0.0
 6    CONTINUE

      DO 8  N = 1, ND4
         ARRAY(N) = 0.0
         CC(N) = 0.0
         EVECC(N) = 0.0
 8    CONTINUE

      DO 10  N = 1, ND5
         YLMC(N) = 0.0
         YLMU(N) = 0.0
 10   CONTINUE

      DO 12  N = 1, ND6
         AMB(N) = 0.0
         APB(N) = 0.0
 12   CONTINUE

      DO 14  N = 1, ND7
         KK(N) = 0.0
         LL(N) = 0.0
         ZZ(N) = 0.0
         ZPLK0(N) = 0.0
         ZPLK1(N) = 0.0
 14   CONTINUE

      DO 16  N = 1, ND8
         Z0U(N) = 0.0
         Z1U(N) = 0.0
         ZBEAM(N) = 0.0
 16   CONTINUE

      DO 18  N = 1, ND9
         GC(N) = 0.0
 18   CONTINUE

      DO 20  N = 1, ND10
         GU(N) = 0.0
 20   CONTINUE

      DO 22  N = 1, ND11
         Z(N) = 0.0
 22   CONTINUE

      RETURN
      END
      SUBROUTINE  ZEROIT( A, LENGTH )
c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )

C         ZEROS A REAL ARRAY -A- HAVING -LENGTH- ELEMENTS

      real*8  A(*)

      DO 10  L = 1, LENGTH
         A( L ) = 0.0
10    CONTINUE

      RETURN
      END
      real*8 FUNCTION  PLKAVG ( WNUMLO, WNUMHI, T )

C        COMPUTES PLANCK FUNCTION INTEGRATED BETWEEN TWO WAVENUMBERS
c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )

C  NOTE ** CHANGE 'R1MACH' TO 'D1MACH' TO RUN IN DOUBLE PRECISION

C  I N P U T :  WNUMLO : LOWER WAVENUMBER ( INV CM ) OF SPECTRAL
C                           INTERVAL
C               WNUMHI : UPPER WAVENUMBER
C               T      : TEMPERATURE (K)

C  O U T P U T :  PLKAVG : INTEGRATED PLANCK FUNCTION ( WATTS/SQ M )
C                           = INTEGRAL (WNUMLO TO WNUMHI) OF
C                              2H C**2  NU**3 / ( EXP(HC NU/KT) - 1)
C                              (WHERE H=PLANCKS CONSTANT, C=SPEED OF
C                              LIGHT, NU=WAVENUMBER, T=TEMPERATURE,
C                              AND K = BOLTZMANN CONSTANT)

C  REFERENCE : SPECIFICATIONS OF THE PHYSICAL WORLD: NEW VALUE
C                 OF THE FUNDAMENTAL CONSTANTS, DIMENSIONS/N.B.S.,
C                 JAN. 1974

C  METHOD :  FOR  -WNUMLO-  CLOSE TO  -WNUMHI-, A SIMPSON-RULE
C            QUADRATURE IS DONE TO AVOID ILL-CONDITIONING; OTHERWISE

C            (1)  FOR 'WNUMLO' OR 'WNUMHI' SMALL,
C                 INTEGRAL(0 TO WNUMLO/HI) IS CALCULATED BY EXPANDING
C                 THE INTEGRAND IN A POWER SERIES AND INTEGRATING
C                 TERM BY TERM;

C            (2)  OTHERWISE, INTEGRAL(WNUMLO/HI TO INFINITY) IS
C                 CALCULATED BY EXPANDING THE DENOMINATOR OF THE
C                 INTEGRAND IN POWERS OF THE EXPONENTIAL AND
C                 INTEGRATING TERM BY TERM.

C  ACCURACY :  AT LEAST 6 SIGNIFICANT DIGITS, ASSUMING THE
C              PHYSICAL CONSTANTS ARE INFINITELY ACCURATE

C  ERRORS WHICH ARE NOT TRAPPED:

C      * POWER OR EXPONENTIAL SERIES MAY UNDERFLOW, GIVING NO
C        SIGNIFICANT DIGITS.  THIS MAY OR MAY NOT BE OF CONCERN,
C        DEPENDING ON THE APPLICATION.

C      * SIMPSON-RULE SPECIAL CASE IS SKIPPED WHEN DENOMINATOR OF
C        INTEGRAND WILL CAUSE OVERFLOW.  IN THAT CASE THE NORMAL
C        PROCEDURE IS USED, WHICH MAY BE INACCURATE IF THE
C        WAVENUMBER LIMITS (WNUMLO, WNUMHI) ARE CLOSE TOGETHER.
C ----------------------------------------------------------------------
C                                   *** ARGUMENTS
      real*8     T, WNUMLO, WNUMHI
C                                   *** LOCAL VARIABLES

C        A1,2,... :  POWER SERIES COEFFICIENTS
C        C2       :  H * C / K, IN UNITS CM*K (H = PLANCKS CONSTANT,
C                      C = SPEED OF LIGHT, K = BOLTZMANN CONSTANT)
C        D(I)     :  EXPONENTIAL SERIES EXPANSION OF INTEGRAL OF
C                       PLANCK FUNCTION FROM WNUMLO (I=1) OR WNUMHI
C                       (I=2) TO INFINITY
C        EPSIL    :  SMALLEST NUMBER SUCH THAT 1+EPSIL .GT. 1 ON
C                       COMPUTER
C        EX       :  EXP( - V(I) )
C        EXM      :  EX**M
C        MMAX     :  NO. OF TERMS TO TAKE IN EXPONENTIAL SERIES
C        MV       :  MULTIPLES OF 'V(I)'
C        P(I)     :  POWER SERIES EXPANSION OF INTEGRAL OF
C                       PLANCK FUNCTION FROM ZERO TO WNUMLO (I=1) OR
C                       WNUMHI (I=2)
C        PI       :  3.14159...
C        SIGMA    :  STEFAN-BOLTZMANN CONSTANT (W/M**2/K**4)
C        SIGDPI   :  SIGMA / PI
C        SMALLV   :  NUMBER OF TIMES THE POWER SERIES IS USED (0,1,2)
C        V(I)     :  C2 * (WNUMLO(I=1) OR WNUMHI(I=2)) / TEMPERATURE
C        VCUT     :  POWER-SERIES CUTOFF POINT
C        VCP      :  EXPONENTIAL SERIES CUTOFF POINTS
C        VMAX     :  LARGEST ALLOWABLE ARGUMENT OF 'EXP' FUNCTION

      PARAMETER  ( A1 = 1./3., A2 = -1./8., A3 = 1./60., A4 = -1./5040.,
     $             A5 = 1./272160., A6 = -1./13305600. )
      INTEGER  SMALLV
      real*8     C2, CONC, D(2), EPSIL, EX, MV, P(2), SIGMA, SIGDPI,
     $         V(2), VCUT, VCP(7), VSQ
      DOUBLE PRECISION   D1MACH
      SAVE     PI, CONC, VMAX, EPSIL, SIGDPI
      DATA     C2 / 1.438786 /,  SIGMA / 5.67032E-8 /,
     $         VCUT / 1.5 /, VCP / 10.25, 5.7, 3.9, 2.9, 2.3, 1.9, 0.0 /
      DATA     PI / 0.0 /
      F(X) = X**3 / ( dexp(X) - 1 )


      IF ( PI.EQ.0.0 )  THEN
         PI = 2. * dasin( 1.0d0 )
         VMAX = dlog( d1mach(2) )
         EPSIL = d1mach(3)
         SIGDPI = SIGMA / PI
         CONC = 15. / PI**4
      END IF

      IF( T.LT.0.0 .OR. WNUMHI.LE.WNUMLO .OR. WNUMLO.LT.0. )
     $    CALL ERRMSG( 'PLKAVG--TEMPERATURE OR WAVENUMS. WRONG', .TRUE.)

      IF ( T.LT.1.E-4 )  THEN
         PLKAVG = 0.0
         RETURN
      ENDIF

      V(1) = C2 * WNUMLO / T
      V(2) = C2 * WNUMHI / T
      IF ( V(1).GT.EPSIL .AND. V(2).LT.VMAX .AND.
     $     (WNUMHI-WNUMLO)/WNUMHI .LT. 1.E-2 )  THEN

C                          ** WAVENUMBERS ARE VERY CLOSE.  GET INTEGRAL
C                          ** BY ITERATING SIMPSON RULE TO CONVERGENCE.
         HH = V(2) - V(1)
         OLDVAL = 0.0
         VAL0 = F( V(1) ) + F( V(2) )

         DO  2  N = 1, 10
            DEL = HH / (2*N)
            VAL = VAL0
            DO  1  K = 1, 2*N-1
               VAL = VAL + 2*(1+MOD(K,2)) * F( V(1) + K*DEL )
    1       CONTINUE
            VAL = DEL/3. * VAL
            IF ( dabs( (VAL-OLDVAL)/VAL ) .LE. 1.E-6 )  GO TO 3
            OLDVAL = VAL
    2    CONTINUE
         CALL ERRMSG( 'PLKAVG--SIMPSON RULE DIDNT CONVERGE', .FALSE. )

    3    PLKAVG = SIGDPI * T**4 * CONC * VAL
         RETURN
      END IF

      SMALLV = 0
      DO  50  I = 1, 2

         IF( V(I).LT.VCUT )  THEN
C                                   ** USE POWER SERIES
            SMALLV = SMALLV + 1
            VSQ = V(I)**2
            P(I) =  CONC * VSQ * V(I) * ( A1 + V(I) * ( A2 + V(I) *
     $                ( A3 + VSQ * ( A4 + VSQ * ( A5 + VSQ*A6 ) ) ) ) )
         ELSE
C                    ** USE EXPONENTIAL SERIES
            MMAX = 0
C                                ** FIND UPPER LIMIT OF SERIES
   20       MMAX = MMAX + 1
               IF ( V(I).LT.VCP( MMAX ) )  GO TO 20

            EX = dexp( - V(I) )
            EXM = 1.0
            D(I) = 0.0

            DO  30  M = 1, MMAX
               MV = M * V(I)
               EXM = EX * EXM
               D(I) = D(I) +
     $                EXM * ( 6. + MV*( 6. + MV*( 3. + MV ) ) ) / M**4
   30       CONTINUE

            D(I) = CONC * D(I)
         END IF

   50 CONTINUE

      IF ( SMALLV .EQ. 2 ) THEN
C                                    ** WNUMLO AND WNUMHI BOTH SMALL
         PLKAVG = P(2) - P(1)

      ELSE IF ( SMALLV .EQ. 1 ) THEN
C                                    ** WNUMLO SMALL, WNUMHI LARGE
         PLKAVG = 1. - P(1) - D(2)

      ELSE
C                                    ** WNUMLO AND WNUMHI BOTH LARGE
         PLKAVG = D(1) - D(2)

      END IF

      PLKAVG = SIGDPI * T**4 * PLKAVG
      IF( PLKAVG.EQ.0.0 )
     $    CALL ERRMSG( 'PLKAVG--RETURNS ZERO; POSSIBLE UNDERFLOW',
     $                 .FALSE. )

      RETURN
      END
      SUBROUTINE  ERRMSG( MESSAG, FATAL )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C        PRINT OUT A WARNING OR ERROR MESSAGE;  ABORT IF ERROR

      LOGICAL       FATAL, ONCE
      CHARACTER*(*) MESSAG
      INTEGER       MAXMSG, NUMMSG
      SAVE          MAXMSG, NUMMSG, ONCE
      DATA NUMMSG / 0 /,  MAXMSG / 100 /,  ONCE / .FALSE. /


      IF ( FATAL )  THEN
         WRITE ( *, '(/,2A)' )  ' ******* ERROR >>>>>>  ', MESSAG
         STOP
      END IF

      NUMMSG = NUMMSG + 1
      IF ( NUMMSG.GT.MAXMSG )  THEN
         IF ( .NOT.ONCE )  WRITE ( *,99 )
         ONCE = .TRUE.
      ELSE
         WRITE ( *, '(/,2A)' )  ' ******* WARNING >>>>>>  ', MESSAG
      ENDIF

      RETURN

   99 FORMAT( ///,' >>>>>>  TOO MANY WARNING MESSAGES --  ',
     $   'THEY WILL NO LONGER BE PRINTED  <<<<<<<', /// )
      END
      LOGICAL FUNCTION  WRTBAD ( VARNAM )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C          WRITE NAMES OF ERRONEOUS VARIABLES AND RETURN 'TRUE'

C      INPUT :   VARNAM = NAME OF ERRONEOUS VARIABLE TO BE WRITTEN
C                         ( CHARACTER, ANY LENGTH )
C ----------------------------------------------------------------------
      CHARACTER*(*)  VARNAM
      INTEGER        MAXMSG, NUMMSG
      SAVE  NUMMSG, MAXMSG
      DATA  NUMMSG / 0 /,  MAXMSG / 50 /


      WRTBAD = .TRUE.
      NUMMSG = NUMMSG + 1
      WRITE ( *, '(3A)' )  ' ****  INPUT VARIABLE  ', VARNAM,
     $                     '  IN ERROR  ****'
      IF ( NUMMSG.EQ.MAXMSG )
     $   CALL  ERRMSG ( 'TOO MANY INPUT ERRORS.  ABORTING...$', .TRUE. )
      RETURN
      END
      LOGICAL FUNCTION  WRTDIM ( DIMNAM, MINVAL )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C          WRITE NAME OF TOO-SMALL SYMBOLIC DIMENSION AND
C          THE VALUE IT SHOULD BE INCREASED TO;  RETURN 'TRUE'

C      INPUT :  DIMNAM = NAME OF SYMBOLIC DIMENSION WHICH IS TOO SMALL
C                        ( CHARACTER, ANY LENGTH )
C               MINVAL = VALUE TO WHICH THAT DIMENSION SHOULD BE
C                        INCREASED (AT LEAST)
C ----------------------------------------------------------------------
      CHARACTER*(*)  DIMNAM
      INTEGER        MINVAL


      WRITE ( *, '(3A,I7)' )  ' ****  SYMBOLIC DIMENSION  ', DIMNAM,
     $                     '  SHOULD BE INCREASED TO AT LEAST ', MINVAL
      WRTDIM = .TRUE.
      RETURN
      END
      LOGICAL FUNCTION  TSTBAD( VARNAM, RELERR )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C       WRITE NAME (-VARNAM-) OF VARIABLE FAILING SELF-TEST AND ITS
C       PERCENT ERROR FROM THE CORRECT VALUE;  RETURN  'FALSE'.

      CHARACTER*(*)  VARNAM
      real*8           RELERR


      TSTBAD = .FALSE.
      WRITE( *, '(/,3A,1P,E11.2,A)' )
     $       ' OUTPUT VARIABLE  ', VARNAM,'  DIFFERED BY', 100.*RELERR,
     $       '  PER CENT FROM CORRECT VALUE.  SELF-TEST FAILED.'
      RETURN
      END
        SUBROUTINE  MSSOLR( CMU, CWT, FBEAM, GC, GU, KK, LAYRU, LL,
     $                      LYRCUT, MAXUMU, MXCMU, MXUMU, NCUT, NN,
     $                      NSTR, NTAU, NUMU, PI, TAUCPR, UMU0, UTAU,
     $                      UTAUPR, ZBEAMMS, ZZ, FDNSRT, S0CMS )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C    I N P U T     V A R I A B L E S:

C       CMU      :  ABSCISSAE FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       CWT      :  WEIGHTS FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       GC       :  EIGENVECTORS AT POLAR QUADRATURE ANGLES, SC(1)
C       GU       :  EIGENVECTORS INTERPOLATED TO USER POLAR ANGLES
C                   (i.e., -G- IN EQ. SC(1))
C       KK       :  EIGENVALUES OF COEFF. MATRIX IN EQ. SS(7)
C       LAYRU    :  LAYER NUMBER OF USER LEVEL -UTAU-
C       LL       :  CONSTANTS OF INTEGRATION IN EQ. SC(1), OBTAINED
C                   BY SOLVING SCALED VERSION OF EQ. SC(5);
C                   EXPONENTIAL TERM OF EQ. SC(12) NOT INCLUDED
C       LYRCUT   :  LOGICAL FLAG FOR TRUNCATION OF COMPUT. LAYER
C       NN       :  ORDER OF DOUBLE-GAUSS QUADRATURE (NSTR/2)
C       NCUT     :  NUMBER OF COMPUTATIONAL LAYER WHERE ABSORPTION
C                     OPTICAL DEPTH EXCEEDS -ABSCUT-
C       TAUCPR   :  CUMULATIVE OPTICAL DEPTH (DELTA-M-SCALED)
C       UTAU     :  OPTICAL DEPTHS OF USER OUTPUT LEVELS
C       UTAUPR   :  OPTICAL DEPTHS OF USER OUTPUT LEVELS IN DELTA-M
C                     COORDINATES;  EQUAL TO  -UTAU- IF NO DELTA-M
C       ZZ       :  BEAM SOURCE VECTORS IN EQ. SS(19)

C   I N T E R N A L       V A R I A B L E S:

C       FLDIR    :  DIRECT-BEAM FLUX (DELTA-M SCALED)
C       FACT     :  EXP( - UTAUPR / UMU0 )
C       ZINT     :  INTENSITY OF M = 0 CASE, IN EQ. SC(1)
C      RFLDIR    :  DIRECT-BEAM FLUX (NOT DELTA-M SCALED)

C   O U T P U T    V A R I A B L E S:

C       S0CMS    :  MULTIPLE SCATTERING SOLAR SOURCE FUNCTION
C      FDNSRT    :  DOWNWARD DIFFUSE SOLAR FLUX AT SURFACE

      LOGICAL LYRCUT
      real*8    S0CMS( MAXUMU,* )
      INTEGER LAYRU( * )
      real*8    CMU( * ), CWT( * ), GC( MXCMU,MXCMU,* ),
     $        GU( MXUMU,MXCMU,* ), KK( MXCMU,* ), LL( MXCMU,* ),
     $        TAUCPR( 0:* ), UTAU( * ), UTAUPR( * ),
     $        ZBEAMMS( MXUMU,* ), ZZ( MXCMU,* )

C                                               ** Loop over user levels
      DO 30 LU = 1, NTAU

         LYU = LAYRU(LU)
         IF ( LYRCUT .AND. LYU.GT.NCUT ) GOTO 30

C                                     ** No radiation reaches this level

         FACT  = dexp( - UTAUPR(LU) / UMU0 )
C                                               ** Loop over user angles
         DO 20 IU = 1, NUMU
            ZINT = 0.0
            DO 10 JQ = 1, NN
               ZINT = ZINT + GU(IU,JQ,LYU) * LL(JQ,LYU) *
     $                dexp( - KK(JQ,LYU) * (UTAUPR(LU) - TAUCPR(LYU)) )
10          CONTINUE
            DO 11 JQ = NN+1, NSTR
               ZINT = ZINT + GU(IU,JQ,LYU) * LL(JQ,LYU) *
     $         dexp( - KK(JQ,LYU) * (UTAUPR(LU) - TAUCPR(LYU-1)) )
11          CONTINUE

C                   **  MS source functions calculated stw(30) m.s. term

            S0CMS(IU,LU) = ZINT + ZBEAMMS(IU,LYU) * FACT
20       CONTINUE
30    CONTINUE

C                                  **  Layer average of S0CMS as MODTRAN
      DO 40 IU = 1, NUMU
         DO 40 LU = 1, NTAU-1
         S0CMS(IU,LU) = ( S0CMS(IU,LU) + S0CMS(IU,LU+1) ) / 2.
40    CONTINUE

      LU = NTAU
      LYU = LAYRU(LU)
      FDNSRT = 0.

      IF ( .NOT.( LYRCUT .AND. LYU.GT.NCUT ) )  THEN
c                                           ** radiation reaches surface
         FACT  = dexp( - UTAUPR(LU) / UMU0 )
         FLDIR = UMU0 * ( FBEAM * FACT )
         RFLDIR= UMU0 * FBEAM * EXP( - UTAU( LU ) / UMU0 )

         DO 60  IQ = 1, NN
            ZINT = 0.0
            DO 50  JQ = 1, NN
               ZINT = ZINT + GC(IQ,JQ,LYU) * LL(JQ,LYU) *
     $                dexp( - KK(JQ,LYU) * (UTAUPR(LU) - TAUCPR(LYU)) )
50          CONTINUE
            DO 51  JQ = NN+1, NSTR
               ZINT = ZINT + GC(IQ,JQ,LYU) * LL(JQ,LYU) *
     $          dexp( - KK(JQ,LYU) * (UTAUPR(LU) - TAUCPR(LYU-1)) )
51          CONTINUE
            FDNSRT = FDNSRT + CWT(NN+1-IQ) * CMU(NN+1-IQ) *
     $                        ( ZINT + ZZ(IQ,LYU)*FACT )
60       CONTINUE

         FDNSRT = 2.0 * PI * FDNSRT + FLDIR - RFLDIR
         IF ( FDNSRT.LT.0. )  FDNSRT = 0.
      END IF

      RETURN
      END
        SUBROUTINE  MSTHML( CMU, CWT, GC, GU, KK, LAYRU, LL, LYRCUT,
     $                      MAXUMU, MXCMU, MXUMU, NCUT, NN, NSTR, NTAU,
     $                      NUMU, OPRIM, PI, TAUCPR, UMU0, UTAUPR,
     $                      XR0, XR1, Z0UMS, Z1UMS, ZPLK0, ZPLK1,
     $                      FDNTRT, T0CMS )

c               inserted line to do double precision - NORTH
                  implicit double precision ( a-h, o-z )
C    I N P U T     V A R I A B L E S:

C       CMU      :  ABSCISSAE FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       CWT      :  WEIGHTS FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       GC       :  EIGENVECTORS AT POLAR QUADRATURE ANGLES, SC(1)
C       GU       :  EIGENVECTORS INTERPOLATED TO USER POLAR ANGLES
C                   (i.e., -G- IN EQ. SC(1))
C       KK       :  EIGENVALUES OF COEFF. MATRIX IN EQ. SS(7)
C       LAYRU    :  LAYER NUMBER OF USER LEVEL -UTAU-
C       LL       :  CONSTANTS OF INTEGRATION IN EQ. SC(1), OBTAINED
C                   BY SOLVING SCALED VERSION OF EQ. SC(5);
C                   EXPONENTIAL TERM OF EQ. SC(12) NOT INCLUDED
C       LYRCUT   :  LOGICAL FLAG FOR TRUNCATION OF COMPUT. LAYER
C       NN       :  ORDER OF DOUBLE-GAUSS QUADRATURE (NSTR/2)
C       NCUT     :  NUMBER OF COMPUTATIONAL LAYER WHERE ABSORPTION
C                     OPTICAL DEPTH EXCEEDS -ABSCUT-
C       OPRIM    :  SINGLE SCATTERING ALBEDO
C       TAUCPR   :  CUMULATIVE OPTICAL DEPTH (DELTA-M-SCALED)
C       UTAUPR   :  OPTICAL DEPTHS OF USER OUTPUT LEVELS IN DELTA-M
C                     COORDINATES;  EQUAL TO  -UTAU- IF NO DELTA-M
C       XR0      :  EXPANSION OF THERMAL SOURCE FUNCTION
C       XR1      :  EXPANSION OF THERMAL SOURCE FUNCTION EQS.SS(14-16)
C       ZPLK0    :  THERMAL SOURCE VECTORS -Z0-, BY SOLVING EQ. SS(16)
C       ZPLK1    :  THERMAL SOURCE VECTORS -Z1-, BY SOLVING EQ. SS(16)

C   I N T E R N A L       V A R I A B L E S:

C       ZINT     :  INTENSITY OF M = 0 CASE, IN EQ. SC(1)

C   O U T P U T    V A R I A B L E S:

C       T0CMS    :  MULTIPLE SCATTERING THERMAL SOURCE FUNCTION
C      FDNTRT    :  DOWNWARD DIFFUSE THERMAL FLUX AT SURFACE

      LOGICAL LYRCUT
      real*8    T0CMS( MAXUMU,* )
      INTEGER LAYRU( * )
      real*8    T0C,CMU( * ), CWT( * ), GC( MXCMU,MXCMU,* ),
     $        GU( MXUMU,MXCMU,* ), KK( MXCMU,* ), LL( MXCMU,* ),
     $        OPRIM( * ), TAUCPR( 0:* ), UTAUPR( * ), XR0( * ),
     $        XR1( * ), Z0UMS( MXUMU,* ), Z1UMS( MXUMU,* ),
     $        FDNTRT,ZPLK0( MXCMU,* ), ZPLK1( MXCMU,* )
C                                               ** Loop over user levels
      DO 30 LU = 1, NTAU
         LYU = LAYRU(LU)
         IF ( LYRCUT .AND. LYU.GT.NCUT ) GOTO 30

C                                     ** No radiation reaches this level

         T0C = (1. - OPRIM(LYU)) * ( XR0(LYU) + XR1(LYU)*UTAUPR(LU) )
C                                ** Loop over user angles
         DO 20 IU = 1, NUMU
            ZINT = 0.0
            DO 10 JQ = 1, NN
               ZINT = ZINT + GU(IU,JQ,LYU) * LL(JQ,LYU) *
     $                dexp( - KK(JQ,LYU) * (UTAUPR(LU) - TAUCPR(LYU)) )
10          CONTINUE
            DO 11 JQ = NN+1, NSTR
               ZINT = ZINT + GU(IU,JQ,LYU) * LL(JQ,LYU) *
     $         dexp( - KK(JQ,LYU) * (UTAUPR(LU) - TAUCPR(LYU-1)) )
11          CONTINUE

C                   **  MS source functions calculated stw(30) m.s. term

            T0CMS(IU,LU) = T0C + ZINT + Z0UMS(IU,LYU) +
     $                                  Z1UMS(IU,LYU) * UTAUPR(LU)
20       CONTINUE
30    CONTINUE
C                                  **  Layer average of T0CMS as MODTRAN
      DO 40 IU = 1, NUMU
         DO 40 LU = 1, NTAU-1


         T0CMS(IU,LU) = ( T0CMS(IU,LU) + T0CMS(IU,LU+1) ) / 2.
40    CONTINUE
      LU = NTAU

      LYU = LAYRU(LU)

c      FDNTRT = 0.0
      FDNTRT = 0.d0

      DO 60  IQ = 1, NN


         ZINT = 0.0
         DO 50  JQ = 1, NN
            ZINT = ZINT + GC(IQ,JQ,LYU) * LL(JQ,LYU) *
     $             dexp( - KK(JQ,LYU) * (UTAUPR(LU) - TAUCPR(LYU)) )
50       CONTINUE
         DO 51  JQ = NN+1, NSTR
            ZINT = ZINT + GC(IQ,JQ,LYU) * LL(JQ,LYU) *
     $             dexp( - KK(JQ,LYU) * (UTAUPR(LU) - TAUCPR(LYU-1)) )
51       CONTINUE
         FDNTRT = FDNTRT + CWT(NN+1-IQ) * CMU(NN+1-IQ) * ( ZINT +
     $                      ZPLK0(IQ,LYU)+ZPLK1(IQ,LYU)*UTAUPR(LU) )
60       CONTINUE

         FDNTRT = 2.0 * PI * FDNTRT
         IF ( FDNTRT.LT.0. )  FDNTRT = 0.
      RETURN
      END
      real*8 FUNCTION   DISBBFN(T,WN0)                                    bbfn 100
C***********************************************************************bbfn 110
C   PLANCK BLACK BODY FUNCTION IN UNITS OF WATTS/(CM2 STER MICROMETER)  bbfn 120
C***********************************************************************bbfn 130
c               inserted line to do double precision - NORTH
                  implicit double precision ( t,w )
      COMMON /CNSTNS/ PI,CA,DEG,GCAIR,BIGNUM,BIGEXP                     bbfn 140
      real*8 T,WN0
      DISBBFN = 0.D0                                                    bbfn 150

      IF(WN0. LE. 0.) RETURN                                            bbfn 160

      X = 1.43879 * WN0 / T                                             bbfn 170
C*****PROTECT AGAINST EXPONENTIAL OVERLOW                               bbfn 180
      IF(X.GT.BIGEXP) RETURN                                            bbfn 190
      DISBBFN = 1.190956E-16*WN0**5/(EXP(X)-1.0)                        bbfn 200


      RETURN                                                            bbfn 210
      END                                                               bbfn 220
