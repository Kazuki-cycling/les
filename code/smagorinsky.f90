      module MF_SF

      INTEGER(4), parameter :: NX=50    !ITO CHANGE !x-direction
      INTEGER(4), parameter :: NY=44    !ITO CHANGE  !y-direction
      INTEGER(4), parameter :: NZ=30    !ITO CHANGE !z-direction

      INTEGER ALLOC_ERR

      REAL(8), PARAMETER :: PHI=3.14159

      CHARACTER num*6

!CCCCCCC////// MAINROUTINE ////////CCCCCC

      INTEGER(4) :: NMAX,NWRT,N1,N,HDD,HDA,NMIN
      INTEGER(4) :: I,J,K,II,JJ,KK,III,JJJ,KKK
      INTEGER(4) :: LOOP
      INTEGER(4) :: ITC,ITC1
      INTEGER(4) :: Ipier_U,Ipier_D,Kpier_R,Kpier_L,Ipier_C    !SUMO23/05/04
      INTEGER(4) :: Ipier_U1,Ipier_D1,Kpier_R1,Kpier_L1
      INTEGER(4) :: Ipier_U2,Ipier_D2,Kpier_R2,Kpier_L2
      INTEGER(4) :: Ipier_U3,Ipier_D3,Kpier_R3,Kpier_L3
      INTEGER(4) :: Ipier_U4,Ipier_D4,Kpier_R4,Kpier_L4
      INTEGER(4) :: IGL(NX+4,NZ+4),IETA(NX+4,NZ+4)    !SUMO23/05/04
      INTEGER(4) :: naka
      REAL(8) :: pier,UT00
      REAL(8) :: DT,TIME,DT1,DDT,HSTIME
      REAL(8) :: GOSAC,GOSAP,GOSAQ,GOSAR,GOSAE,GOSAS,GOSAD
      REAL(8) :: RED,AL,CONST1,CONST2,CONST3,EPS1,EPS2,EPS3,EPS4
      REAL(8) :: DEPTH,FLUX_IN_I,FLUX_OUT_I
      CHARACTER FNAME*10,FNAME2*4
      CHARACTER ch10_7*1, ch10_6*1, ch10_5*1, ch10_4*1,  &
                ch10_3*1, ch10_2*1, ch10_1*1, ch10_0*1

!CCCCCCC////// INIT & COMMON ////////CCCCCC

      REAL(8) :: GX(NX+4),GXX(NX+4)
      REAL(8) :: EY(NY+4),EYY(NY+4)
      REAL(8) :: DZ(NZ+4),DZZ(NZ+4)
      REAL(8) :: X(NX+4,NY+4,NZ+4),Y(NX+4,NY+4,NZ+4)
      REAL(8) :: Z(NX+4,NY+4,NZ+4)
      REAL(8) :: U(NX+4,NY+4,NZ+4),U1(NX+4,NY+4,NZ+4)
      REAL(8) :: V(NX+4,NY+4,NZ+4),V1(NX+4,NY+4,NZ+4)
      REAL(8) :: W(NX+4,NY+4,NZ+4),W1(NX+4,NY+4,NZ+4)
      REAL(8) :: US(NX+4,NY+4,NZ+4),US1(NX+4,NY+4,NZ+4)
      REAL(8) :: VS(NX+4,NY+4,NZ+4),VS1(NX+4,NY+4,NZ+4)
      REAL(8) :: WS(NX+4,NY+4,NZ+4),WS1(NX+4,NY+4,NZ+4)
      REAL(8) :: U2(NX+4,NY+4,NZ+4),US2(NX+4,NY+4,NZ+4)
      REAL(8) :: U3(NX+4,NY+4,NZ+4)
      REAL(8) :: V2(NX+4,NY+4,NZ+4),VS2(NX+4,NY+4,NZ+4)
      REAL(8) :: W2(NX+4,NY+4,NZ+4),WS2(NX+4,NY+4,NZ+4)
      REAL(8) :: HK(NX+4,NY+4,NZ+4),HK1(NX+4,NY+4,NZ+4)
      REAL(8) :: MYT(NX+4,NY+4,NZ+4),RE1(NX+4,NY+4,NZ+4)
      REAL(8) :: RE1G(NX+4,NY+4,NZ+4),RE1ENE(NX+4,NY+4,NZ+4)
      REAL(8) :: DET(NX+4,NY+4,NZ+4),P(NX+4,NY+4,NZ+4)
      REAL(8) :: C(NX+4,NY+4,NZ+4),C1(NX+4,NY+4,NZ+4)
      REAL(8) :: USR(NX+4,NY+4,NZ+4),USR1(NX+4,NY+4,NZ+4)
      REAL(8) :: VSR(NX+4,NY+4,NZ+4),VSR1(NX+4,NY+4,NZ+4)
      REAL(8) :: WSR(NX+4,NY+4,NZ+4),WSR1(NX+4,NY+4,NZ+4)
      REAL(8) :: UUSR(NX+4,NY+4,NZ+4),UUSR1(NX+4,NY+4,NZ+4)
      REAL(8) :: VVSR(NX+4,NY+4,NZ+4),VVSR1(NX+4,NY+4,NZ+4)
      REAL(8) :: WWSR(NX+4,NY+4,NZ+4),WWSR1(NX+4,NY+4,NZ+4)
      REAL(8) :: XM(NX+4,NY+4,NZ+4),YM(NX+4,NY+4,NZ+4)
      REAL(8) :: XG1(NX+4,NY+4,NZ+4),YG(NX+4,NY+4,NZ+4)
      REAL(8) :: ZG(NX+4,NY+4,NZ+4),ZM(NX+4,NY+4,NZ+4)
      REAL(8) :: CM(NX+4,NY+4,NZ+4)
      REAL(8) :: ROU(NX+4,NY+4,NZ+4),ROUA(NX+4,NY+4,NZ+4)
      REAL(8) :: SAL(NX+4,NY+4,NZ+4),SAL1(NX+4,NY+4,NZ+4)
      REAL(8) :: ROU1(NX+4,NY+4,NZ+4),SALA(NX+4,NY+4,NZ+4)
      REAL(8) :: USTA(NX+4),USTA1(NX+4)
      REAL(8) :: VSTA(NX+4),VSTA1(NX+4)
      REAL(8) :: USTAu(NX+4),USTAu1(NX+4)
      REAL(8) :: VSTAu(NX+4),VSTAu1(NX+4)
      REAL(8) :: APP(NX+4),APP1(NX+4),Q(NX+4),Q1(NX+4)
      REAL(8) :: P1(NX+4,NY+4,NZ+4),PSI(NX+4,NY+4,NZ+4)
      REAL(8) :: TEMP(NX+4,NY+4,NZ+4),TEMP1(NX+4,NY+4,NZ+4)
      REAL(8) :: API(NX+4,NY+4,NZ+4),AA,T0
      REAL(8) :: UUNISUM,DEPCOUNT,UUNI(NZ+4),SLOPE,RNM(NX+4,NZ+4),DEPC
      REAL(8) :: QQQ(NX+4),CFF(NX+4,NZ+4),USTAB(NX+4,NZ+4),WSTAB(NX+4,NZ+4) &
                ,USTABRW(NX+4,NY+4),USTABRW1(NX+4,NY+4),USTABLW(NX+4,NY+4),USTABLW1(NX+4,NY+4) &
                ,USTABRW_P(NX+4,NY+4),USTABRW_P1(NX+4,NY+4),USTABLW_P(NX+4,NY+4),USTABLW_P1(NX+4,NY+4) &
                ,WSTABFW_P(NY+4,NZ+4),WSTABFW_P1(NY+4,NZ+4),WSTABBW_P(NY+4,NZ+4),WSTABBW_P1(NY+4,NZ+4)
      REAL(8) :: DUUDX(NX+4,NZ+4),DWWDZ(NX+4,NZ+4),DETADX(NX+4,NZ+4),DETADZ(NX+4,NZ+4)
      REAL(8) :: Qin,SODO(NX+4,NZ+4),YY(NX+4,NY+4,NZ+4)
      INTEGER(4) :: IMAX,JMAX,KMAX
      REAL(8) :: DR,RC,RS,GG,CL0,htp,CM0
      REAL(8) :: FACT1,FACT2,FACT3,FACT4,FACT5,SS,G,D50,CMAX
      REAL(8) :: arfa1,beta1,n2
      REAL(8) :: CS,CE,arfa,CAR,Z0,AS,AC0
      REAL(8) :: UUU,MYU0
      REAL(8) :: ww0,vv0,qq0
      REAL(8) :: XHG(NX+4),GHX(NX+4)
      REAL(8) :: CPUTIME01,CPUTIME02,CPUTIME03,CPUTIME04,CPUTIME05
      REAL(8) :: CPUTIME06,CPUTIME07,CPUTIME08,CPUTIME09,CPUTIME10
      REAL(8) :: IT1,IT2,IT3,IT4,IT5,IT6,IT7,IT8,IT9,IT10
      REAL(8) :: IT11,IT12,IT13,IT14,IT15,IT16,IT17,IT18,IT19,IT20
      REAL(8) :: DEP(NX+4,NZ+4),DEP1(NX+4,NZ+4),DEPMODE
      REAL(8) :: ETA(NX+4,NZ+4),ETA1(NX+4,NZ+4),REETA(NX+4,NZ+4)
      REAL(8) :: GL(NX+4,NZ+4),GL1(NX+4,NZ+4)
      REAL(8) :: h00(NX+4,NZ+4),SSS(NX+4,NZ+4)
      REAL(8) :: UT(NX+4,NZ+4),UT1(NX+4,NZ+4)
      REAL(8) :: WT(NX+4,NZ+4),WT1(NX+4,NZ+4)
      REAL(8) :: DUDX(NX+4,NZ+4),DWDZ(NX+4,NZ+4),SSD(NX+4,NZ+4)
      REAL(8) :: BCMODE,MEAN,STD_DEV,r1,r2
      REAL(8) :: HH(NX+4,NZ+4),ELES(NX+4,NZ+4),ELES1(NX+4,NZ+4)
      REAL(8) :: CORIC(NX+4,NY+4,NZ+4),CORICM(NX+4,NY+4,NZ+4)
      !REAL(8) :: Qvalue(NX+4,NY+4,NZ+4)
      REAL(8) :: UB(NX+4,NY+4,NZ+4),VB(NX+4,NY+4,NZ+4)
      REAL(8) :: WB(NX+4,NY+4,NZ+4)
      REAL(8) :: DETA(NX+4,NZ+4)
!C
!CCCCCCC////// INIT ////////CCCCCC
!C
      REAL(8) :: Umm,aaa,bbb,B00    !B0??øΩ?øΩ??øΩ?øΩB00, SUMO23/05/04
!
!C----------Original Dimension----------C
      REAL(8) :: RA1(NX+4,NY+4,NZ+4),RA2(NX+4,NY+4,NZ+4)
      REAL(8) :: RA3(NX+4,NY+4,NZ+4),XG(NX+4),YE(NY+4),ZD(NZ+4)
!
!//////////allocate??øΩ?øΩz??øΩ?øΩ??øΩ?øΩ…ïœùX//////////!
!
!      REAL(8), allocatable :: &
!      RA1(:,:,:),RA2(:,:,:),RA3(:,:,:),XG(:),YE(:),ZD(:)   !221027TI
!C
!CCCCCCC////// GAL ////////CCCCCC
!C
      REAL(8) :: h,h0,A,HHK,SG,WC
      REAL(8) :: AG1,AG2,AG3
      REAL(8) :: myufs,myufd,CS0,CSmin,CSmax,ee,sita0,CINT,sigmf
      REAL(8) :: FX1,FX2,FX3
      REAL(8) :: ZANC,AC
!
!C----------Original Dimension----------C
!      REAL(8) :: 
!     ,fx(NX+4,NY+4,NZ+4,3,3,3),XF(NX+4,NY+4,NZ+4,3,3,3)&
!     ,fy(NX+4,NY+4,NZ+4,3,3,3),YF(NX+4,NY+4,NZ+4,3,3,3)&
!     ,fz(NX+4,NY+4,NZ+4,3,3,3),ZF(NX+4,NY+4,NZ+4,3,3,3)&
!     ,CT(NX+4,NY+4,NZ+3),TLT(NX+4,NY+4,NZ+4)&
!     ,UST(NX+4,NY+4,NZ+4),VST(NX+4,NY+4,NZ+4)&
!     ,USRT(NX+4,NY+4,NZ+4),VSRT(NX+4,NY+4,NZ+4)&
!     ,WSRT(NX+4,NY+4,NZ+4),WST(NX+4,NY+4,NZ+4)&
!     ,OMEX(NX+4,NY+4,NZ+4),OMEY(NX+4,NY+4,NZ+4)&   !GAL??øΩ?øΩ??øΩ?øΩ??øΩ?øΩ??øΩ?øΩOMEX,Y,Z??øΩ?øΩ??øΩ?øΩOMEXG,YG,ZG??øΩ?øΩ…ïœùX
!     ,OMEZ(NX+4,NY+4,NZ+4),HKW(NX+4,NY+4,NZ+4)&
!     ,HKU(NX+4,NY+4,NZ+4),HKV(NX+4,NY+4,NZ+4)&
!     ,DD1(NX+4,NY+4,NZ+4),DD2(NX+4,NY+4,NZ+4)&   !GAL??øΩ?øΩ??øΩ?øΩ??øΩ?øΩ??øΩ?øΩDD1,2,3??øΩ?øΩ??øΩ?øΩDD1G,2G,3G??øΩ?øΩ…ïœùX
!     ,DD3(NX+4,NY+4,NZ+4)&
!     ,AU(NX+4,NY+4,NZ+4),W0(NX+4,NY+4,NZ+4)&
!     ,AP1(NX+4,NY+4,NZ+4),AP2(NX+4,NY+4,NZ+4)&
!     ,BTE(NX+4,NY+4,NZ+4),AP3(NX+4,NY+4,NZ+4)
!
!//////////allocate??øΩ?øΩz??øΩ?øΩ??øΩ?øΩ…ïœùX//////////!
!
      REAL(8), allocatable :: &
      fx(:,:,:,:,:,:),XF(:,:,:,:,:,:) &
     ,fy(:,:,:,:,:,:),YF(:,:,:,:,:,:) &
     ,fz(:,:,:,:,:,:),ZF(:,:,:,:,:,:) &
     ,CT(:,:,:),TLT(:,:,:)&
     ,UST(:,:,:),VST(:,:,:)&
     ,USRT(:,:,:),VSRT(:,:,:)&
     ,WSRT(:,:,:),WST(:,:,:)&
     ,OMEXG(:,:,:),OMEYG(:,:,:)&   !OMEX??øΩ?øΩ??øΩ?øΩOMEXG,OMEY??øΩ?øΩ??øΩ?øΩOMEYG
     ,OMEZG(:,:,:),HKW(:,:,:)&     !OMEZ??øΩ?øΩ??øΩ?øΩOMEZG
     ,HKU(:,:,:),HKV(:,:,:)&
     ,DD1G(:,:,:),DD2G(:,:,:)&     !DD1??øΩ?øΩ??øΩ?øΩDD1G,DD2??øΩ?øΩ??øΩ?øΩDD2G
     ,DD3G(:,:,:)&                 !DD3??øΩ?øΩ??øΩ?øΩDD3G
     ,AU(:,:,:),W0(:,:,:)&
     ,AP1(:,:,:),AP2(:,:,:)&
     ,BTE(:,:,:),AP3(:,:,:)
        
!C
      REAL(8) ::   &    !XXM(NX+4,NY+4,NZ+4),YYM(NX+4,NY+4,NZ+4)&
!     ,DX1(NX+4,NY+4,NZ+4),DX2(NX+4,NY+4,NZ+4)&
!     ,DY1(NX+4,NY+4,NZ+4),DY2(NX+4,NY+4,NZ+4)&
!     ,DZ1(NX+4,NY+4,NZ+4),DZ2(NX+4,NY+4,NZ+4)&
!     ,USQ(NX+4,NY+4,NZ+4),VSQ(NX+4,NY+4,NZ+4)&
!     ,WSQ(NX+4,NY+4,NZ+4),ZZM(NX+4,NY+4,NZ+4)&
!     ,TP(NX+4,NY+4,NZ+4),VR(NX+4,NY+4,NZ+4)&
!     ,VSQ2(NX+4,NY+4,NZ+4),YYM2(NX+4,NY+4,NZ+4)&
!     ,TAUP(NX+4,NY+4,NZ+4),JI(NX+4,NZ+4)&
     TAUS(NX+4,NY+4,NZ+4),PS(NX+4,NY+4,NZ+4)&
!     ,CAV(NX+4,NY+4,NZ+4),SL(NX+4,NY+4,NZ+4)&
     ,PCX(NX+4,NY+4,NZ+4),PCY(NX+4,NY+4,NZ+4)&
!     ,PRO(NX+4,NY+4,NZ+4),DIS(NX+4,NY+4,NZ+4)&
!     ,TS(NX+4,NY+4,NZ+4),arfa0(NX+4,NY+4,NZ+4)&
     ,TAUC11(NX+4,NY+4,NZ+4),TAUC12(NX+4,NY+4,NZ+4)&
     ,TAUC21(NX+4,NY+4,NZ+4),TAUC22(NX+4,NY+4,NZ+4)&
     ,TAUC31(NX+4,NY+4,NZ+4),TAUC32(NX+4,NY+4,NZ+4)&
     ,TAUC13(NX+4,NY+4,NZ+4),TAUC23(NX+4,NY+4,NZ+4)&
     ,TAUC33(NX+4,NY+4,NZ+4),PCZ(NX+4,NY+4,NZ+4)
!     ,XXM2(NX+4,NY+4,NZ+4),USQ2(NX+4,NY+4,NZ+4)&
!     ,ZZM2(NX+4,NY+4,NZ+4),WSQ2(NX+4,NY+4,NZ+4)
!C
!
!//////////allocate??øΩ?øΩz??øΩ?øΩ??øΩ?øΩ…ïœùX//////////!
!
      REAL(8), allocatable :: &
      XXM(:,:,:),YYM(:,:,:)&
     ,DX1(:,:,:),DX2(:,:,:)&
     ,DY1(:,:,:),DY2(:,:,:)&
     ,DZ1(:,:,:),DZ2(:,:,:)&
     ,USQ(:,:,:),VSQ(:,:,:)&
     ,WSQ(:,:,:),ZZM(:,:,:)&
     ,TP(:,:,:),VR(:,:,:)&
     ,VSQ2(:,:,:),YYM2(:,:,:)&
     ,TAUP(:,:,:),JI(:,:)&
!     ,TAUS(:,:,:),PS(:,:,:)&
     ,CAV(:,:,:),SL(:,:,:)&
!     ,PCX(:,:,:),PCY(:,:,:)&
     ,PRO(:,:,:),DIS(:,:,:)&
     ,TS(:,:,:),arfa0(:,:,:)&
!     ,TAUC11(:,:,:),TAUC12(:,:,:)&
!     ,TAUC21(:,:,:),TAUC22(:,:,:)&
!     ,TAUC31(:,:,:),TAUC32(:,:,:)&
!     ,TAUC13(:,:,:),TAUC23(:,:,:)&
!     ,TAUC33(:,:,:),PCZ(:,:,:)&
     ,XXM2(:,:,:),USQ2(:,:,:)&
     ,ZZM2(:,:,:),WSQ2(:,:,:)
!
!CCCCCCC////// ENERGY ////////CCCCCC
!C
      REAL(8) :: UA(NX+5),C3
      REAL(8) :: UKG(NX+4,NY+4,NZ+4),UKE(NX+4,NY+4,NZ+4)    !SAL??øΩ?øΩ≈ÇÔøΩ??øΩ?øΩ??øΩ?øΩ??øΩ?øΩl??øΩ?øΩÃéÔøΩ
      REAL(8) :: VKG(NX+4,NY+4,NZ+4),VKE(NX+4,NY+4,NZ+4)    !SAL??øΩ?øΩ≈ÇÔøΩ??øΩ?øΩ??øΩ?øΩ??øΩ?øΩl??øΩ?øΩÃéÔøΩ
      REAL(8) :: MYTGENE(NX+4,NY+4,NZ+4),MYTEENE(NX+4,NY+4,NZ+4)    !SAL??øΩ?øΩ≈ÇÔøΩ??øΩ?øΩ??øΩ?øΩ??øΩ?øΩl??øΩ?øΩÃéÔøΩ
      REAL(8) :: AK0(NX+4,NY+4,NZ+4)
      REAL(8) :: UK(NX+4,NY+4,NZ+4),VK(NX+4,NY+4,NZ+4),CKC    !SAL??øΩ?øΩ≈ÇÔøΩ??øΩ?øΩ??øΩ?øΩ??øΩ?øΩl??øΩ?øΩÃéÔøΩ
!
!C----------Original Dimension----------C
!      REAL(8) ::  HKG(NX+4,NY+4,NZ+4),HKE(NX+4,NY+4,NZ+4)& !HKG??øΩ?øΩ??øΩ?øΩHKGENE??øΩ?øΩCHKE??øΩ?øΩ??øΩ?øΩHKEENE
!     ,HKGG(NX+4,NY+4,NZ+4),HKEE(NX+4,NY+4,NZ+4)&
!     ,MYTG(NX+4,NY+4,NZ+4),MYTE(NX+4,NY+4,NZ+4)&   !MYTG??øΩ?øΩ??øΩ?øΩMYTGENE??øΩ?øΩCMYTE??øΩ?øΩ??øΩ?øΩMYTEENE
!     ,UKG(NX+4,NY+4,NZ+4),UKE(NX+4,NY+4,NZ+4)&
!     ,VKG(NX+4,NY+4,NZ+4),VKE(NX+4,NY+4,NZ+4)&
!     ,HK0(NX+4,NY+4,NZ+4),HKN(NX+4,NY+4,NZ+4)&
!     ,DK(NX+4,NY+4,NZ+4),AK0(NX+4,NY+4,NZ+4)&
!     ,SK(NX+4,NY+4,NZ+4),CK(NX+4,NY+4,NZ+4)
!
!//////////allocate??øΩ?øΩz??øΩ?øΩ??øΩ?øΩ…ïœùX//////////!
!
      REAL(8), allocatable :: &
      HKGENE(:,:,:),HKEENE(:,:,:)&
     ,HKGG(:,:,:),HKEE(:,:,:)&
     ,HK0(:,:,:),HKN(:,:,:)&
     ,DK(:,:,:)&
     ,SK(:,:,:),FLUC_RATE(:,:,:),UF(:,:,:),VF(:,:,:),WF(:,:,:),CK(:,:,:)&
     ,WDF1(:,:,:),WDF2(:,:,:),WDF3(:,:,:),WDF4(:,:,:),WDF5(:,:,:),WDF6(:,:,:),FW(:,:,:)&
     ,FW1(:,:,:),FW2(:,:,:),FW3(:,:,:),FW4(:,:,:),FW5(:,:,:),FW6(:,:,:),DETN(:,:,:)

!      REAL(8) ::  HTEI(NX+4,NY+4,NZ+4),TEI1(NX+4,NY+4,NZ+4)&
!     ,CKG(NX+4,NY+4,NZ+4),CKE(NX+4,NY+4,NZ+4)&
!     ,UUSRK(NX+4,NY+4,NZ+4),VVSRK(NX+4,NY+4,NZ+4)&
!     ,WWSRK(NX+4,NY+4,NZ+4),WWSRK1(NX+4,NY+4,NZ+4)&
!     ,UCK(NX+4,NY+4,NZ+4),VCK(NX+4,NY+4,NZ+4)&
!     ,HAMF(NX+4,NY+4,NZ+4)&
!     ,UUSRK1(NX+4,NY+4,NZ+4),VVSRK1(NX+4,NY+4,NZ+4)
!
!//////////allocate??øΩ?øΩz??øΩ?øΩ??øΩ?øΩ…ïœùX//////////!
!
      REAL(8), allocatable :: &
      HTEI(:,:,:),TEI1(:,:,:)&
     ,CKG(:,:,:),CKE(:,:,:)&
     ,UUSRK(:,:,:),VVSRK(:,:,:)&
     ,WWSRK(:,:,:),WWSRK1(:,:,:)&
     ,UCK(:,:,:),VCK(:,:,:)&
     ,HAMF(:,:,:)&
     ,UUSRK1(:,:,:),VVSRK1(:,:,:)
!
!      REAL(8) ::  UKD(NX+4,NY+4,NZ+4),VKD(NX+4,NY+4,NZ+4)&
!     ,WK(NX+4,NY+4,NZ+4),WSK(NX+4,NY+4,NZ+4)&
!     ,WKG(NX+4,NY+4,NZ+4),WKE(NX+4,NY+4,NZ+4)&
!     ,WKD(NX+4,NY+4,NZ+4),CKD(NX+4,NY+4,NZ+4)&
!     ,HKD(NX+4,NY+4,NZ+4),HKDD(NX+4,NY+4,NZ+4)&            !HKD??øΩ?øΩ??øΩ?øΩHKDENE
!     ,MYTD(NX+4,NY+4,NZ+4),WCK(NX+4,NY+4,NZ+4)&            !MYTD??øΩ?øΩ??øΩ?øΩMYTDENE
!     ,HTEP(NX+4,NY+4,NZ+4),HKP(NX+4,NY+4,NZ+4)&
!     ,UK(NX+4,NY+4,NZ+4),VK(NX+4,NY+4,NZ+4)&
!     ,USK(NX+4,NY+4,NZ+4),VSK(NX+4,NY+4,NZ+4)&
!     ,EK(NX+4,NY+4,NZ+4)
!
!//////////allocate??øΩ?øΩz??øΩ?øΩ??øΩ?øΩ…ïœùX//////////!
!
      REAL(8), allocatable :: &
      UKD(:,:,:),VKD(:,:,:)&
     ,WK(:,:,:),WSK(:,:,:)&
     ,WKG(:,:,:),WKE(:,:,:)&
     ,WKD(:,:,:),CKD(:,:,:)&
     ,HKDENE(:,:,:),HKDD(:,:,:)&
     ,MYTDENE(:,:,:),WCK(:,:,:)&
     ,HTEP(:,:,:),HKP(:,:,:)&
     ,USK(:,:,:),VSK(:,:,:)&
     ,EK(:,:,:),CCC(:,:,:)
!
!C
!CCCCCCC////// NSE ////////CCCCCC
!C
      REAL(8) ::  CCD,CCD2
      REAL(8) ::  ZANSU
      !REAL(8) ::  WWX(NX+4,NY+4,NZ+4)
      !REAL(8) ::  WWY(NX+4,NY+4,NZ+4)
      !REAL(8) ::  WWZ(NX+4,NY+4,NZ+4)
      REAL(8) ::  AE(NX+4,NY+4,NZ+4)
      REAL(8) ::  BE(NX+4,NY+4,NZ+4)
      REAL(8) ::  CCE(NX+4,NY+4,NZ+4)
!
!C----------Original Dimension----------C
!
!      REAL(8) ::  E11(NX+4,NY+4,NZ+4),F11(NX+4,NY+4,NZ+4)&
!     ,E12(NX+4,NY+4,NZ+4),F12(NX+4,NY+4,NZ+4)&
!     ,E13(NX+4,NY+4,NZ+4),F13(NX+4,NY+4,NZ+4)&
!     ,DD1(NX+4,NY+4,NZ+4)
!
!//////////allocate??øΩ?øΩz??øΩ?øΩ??øΩ?øΩ…ïœùX//////////!
!
     REAL(8), allocatable :: &
     E11(:,:,:),F11(:,:,:)&
    ,E12(:,:,:),F12(:,:,:)&
    ,E13(:,:,:),F13(:,:,:)&
    ,DD1(:,:,:)&
    ,PP1(:,:,:),PP2(:,:,:)&
    ,PP3(:,:,:),PP4(:,:,:)&
    ,PP5(:,:,:),PP6(:,:,:)&
    ,ETA_1(:,:,:),ETA_2(:,:,:)&
    ,ETA_3(:,:,:),ETA_4(:,:,:)&
    ,ETA_5(:,:,:),ETA_6(:,:,:)
!
!      REAL(8) ::  UGA(NX+4,NY+4,NZ+4),UGB(NX+4,NY+4,NZ+4)&
!     ,UEA(NX+4,NY+4,NZ+4),UEB(NX+4,NY+4,NZ+4)&
!     ,UDA(NX+4,NY+4,NZ+4),UDB(NX+4,NY+4,NZ+4)&
!     ,VGA(NX+4,NY+4,NZ+4),VGB(NX+4,NY+4,NZ+4)&
!     ,VEA(NX+4,NY+4,NZ+4),VEB(NX+4,NY+4,NZ+4)&
!     ,VDA(NX+4,NY+4,NZ+4),VDB(NX+4,NY+4,NZ+4)&
!     ,WGA(NX+4,NY+4,NZ+4),WGB(NX+4,NY+4,NZ+4)&
!     ,WEA(NX+4,NY+4,NZ+4),WEB(NX+4,NY+4,NZ+4)&
!     ,WDA(NX+4,NY+4,NZ+4),WDB(NX+4,NY+4,NZ+4)
!
!//////////allocate??øΩ?øΩz??øΩ?øΩ??øΩ?øΩ…ïœùX//////////!
!
      REAL(8), allocatable :: &
      UGA(:,:,:),UGB(:,:,:)&
     ,UEA(:,:,:),UEB(:,:,:)&
     ,UDA(:,:,:),UDB(:,:,:)&
     ,VGA(:,:,:),VGB(:,:,:)&
     ,VEA(:,:,:),VEB(:,:,:)&
     ,VDA(:,:,:),VDB(:,:,:)&
     ,WGA(:,:,:),WGB(:,:,:)&
     ,WEA(:,:,:),WEB(:,:,:)&
     ,WDA(:,:,:),WDB(:,:,:)
!
!      REAL(8) ::  A0(NX+4,NY+4,NZ+4),AP(NX+4,NY+4,NZ+4)&
!     ,B0(NX+4,NY+4,NZ+4),BP(NX+4,NY+4,NZ+4)&
!     ,C0(NX+4,NY+4,NZ+4),CP(NX+4,NY+4,NZ+4)&
!     ,AN(NX+4,NY+4,NZ+4),AE(NX+4,NY+4,NZ+4)&
!     ,BN(NX+4,NY+4,NZ+4),BE(NX+4,NY+4,NZ+4)&
!     ,CN(NX+4,NY+4,NZ+4),CCE(NX+4,NY+4,NZ+4)&
!     ,SU(NX+4,NY+4,NZ+4),CC1(NX+4,NY+4,NZ+4)&
!     ,SV(NX+4,NY+4,NZ+4),CC2(NX+4,NY+4,NZ+4)&
!     ,SW(NX+4,NY+4,NZ+4),CC3(NX+4,NY+4,NZ+4)
!
!//////////allocate??øΩ?øΩz??øΩ?øΩ??øΩ?øΩ…ïœùX//////////!
!
      REAL(8), allocatable :: &
      A0(:,:,:),AP(:,:,:)&
     ,B0(:,:,:),BP(:,:,:)&
     ,C0(:,:,:),CP(:,:,:)&
     ,AN(:,:,:)&
     ,BN(:,:,:)&
     ,CN(:,:,:)&
     ,SU(:,:,:),CC1(:,:,:)&
     ,SV(:,:,:),CC2(:,:,:)&
     ,SW(:,:,:),CC3(:,:,:)
!
!      REAL(8) ::  UG(NX+4,NY+4,NZ+4),VG(NX+4,NY+4,NZ+4)&
!     ,UE(NX+4,NY+4,NZ+4),VE(NX+4,NY+4,NZ+4)&
!     ,UD(NX+4,NY+4,NZ+4),VD(NX+4,NY+4,NZ+4)&
!     ,UGG(NX+4,NY+4,NZ+4),VGG(NX+4,NY+4,NZ+4)&
!     ,UEE(NX+4,NY+4,NZ+4),VEE(NX+4,NY+4,NZ+4)&
!     ,UDD(NX+4,NY+4,NZ+4),VDD(NX+4,NY+4,NZ+4)&
!     ,WG(NX+4,NY+4,NZ+4),WGG(NX+4,NY+4,NZ+4)&
!     ,WE(NX+4,NY+4,NZ+4),WEE(NX+4,NY+4,NZ+4)&
!     ,WD(NX+4,NY+4,NZ+4),WDD(NX+4,NY+4,NZ+4)
!
!//////////allocate??øΩ?øΩz??øΩ?øΩ??øΩ?øΩ…ïœùX//////////!
!
      REAL(8), allocatable :: &
      UG(:,:,:),VG(:,:,:)&
     ,UE(:,:,:),VE(:,:,:)&
     ,UD(:,:,:),VD(:,:,:)&
     ,UGG(:,:,:),VGG(:,:,:)&
     ,UEE(:,:,:),VEE(:,:,:)&
     ,UDD(:,:,:),VDD(:,:,:)&
     ,WG(:,:,:),WGG(:,:,:)&
     ,WE(:,:,:),WEE(:,:,:)&
     ,WD(:,:,:),WDD(:,:,:)
!
!      REAL(8) ::  MYTG(NX+4,NY+4,NZ+4),HKG(NX+4,NY+4,NZ+4)&
!     ,MYTE(NX+4,NY+4,NZ+4),HKE(NX+4,NY+4,NZ+4)&
!     ,MYTD(NX+4,NY+4,NZ+4),HKD(NX+4,NY+4,NZ+4)&
!     ,BOU(NX+4,NY+4,NZ+4),OMEX(NX+4,NY+4,NZ+4)&
!     ,OMEY(NX+4,NY+4,NZ+4),OMEZ(NX+4,NY+4,NZ+4)
!
!//////////allocate??øΩ?øΩz??øΩ?øΩ??øΩ?øΩ…ïœùX//////////!
!
      REAL(8), allocatable :: &
      MYTG(:,:,:),HKG(:,:,:)&
     ,MYTE(:,:,:),HKE(:,:,:)&
     ,MYTD(:,:,:),HKD(:,:,:)&
     ,BOU(:,:,:),OMEX(:,:,:)&
     ,OMEY(:,:,:),OMEZ(:,:,:),KF(:,:,:)
!
!      REAL(8) ::  FDX(NX+4,NY+4,NZ+4),FDY(NX+4,NY+4,NZ+4)&
!     ,FLX0(NX+4,NY+4,NZ+4),FLY0(NX+4,NY+4,NZ+4)&
!     ,FMX(NX+4,NY+4,NZ+4),FMY(NX+4,NY+4,NZ+4)&
!     ,DUDT(NX+4,NY+4,NZ+4),DVDT(NX+4,NY+4,NZ+4)&
!     ,DUSDT(NX+4,NY+4,NZ+4),DVSDT(NX+4,NY+4,NZ+4)&
!     ,DWDT(NX+4,NY+4,NZ+4),DWSDT(NX+4,NY+4,NZ+4)&
!     ,FMZ(NX+4,NY+4,NZ+4),FLZ0(NX+4,NY+4,NZ+4)&
!     ,FDZ(NX+4,NY+4,NZ+4)
!!C     &         ,D11(NX+4,NY+4,NZ+4),D12(NX+4,NY+4,NZ+4)
!!C     &         ,D21(NX+4,NY+4,NZ+4),D22(NX+4,NY+4,NZ+4)
!!C     &         ,D13(NX+4,NY+4,NZ+4),D23(NX+4,NY+4,NZ+4)
!!C     &         ,DD3(NX+4,NY+4,NZ+4),DD2(NX+4,NY+4,NZ+4)
!!C     &         ,CG1(NX+4,NY+4,NZ+4),CE1(NX+4,NY+4,NZ+4)
!!C     &         ,CG2(NX+4,NY+4,NZ+4),CE2(NX+4,NY+4,NZ+4)
!!C     &         ,UC(NX+4,NY+4,NZ+4),VC(NX+4,NY+4,NZ+4)
!
!//////////allocate??øΩ?øΩz??øΩ?øΩ??øΩ?øΩ…ïœùX//////////!
!
      REAL(8), allocatable :: &
      FDX(:,:,:),FDY(:,:,:)&
     ,FLX0(:,:,:),FLY0(:,:,:)&
     ,FMX(:,:,:),FMY(:,:,:)&
     ,DUDT(:,:,:),DVDT(:,:,:)&
     ,DUSDT(:,:,:),DVSDT(:,:,:)&
     ,DWDT(:,:,:),DWSDT(:,:,:)&
     ,FMZ(:,:,:),FLZ0(:,:,:)&
     ,FDZ(:,:,:)
!
!C
!CCCCCCC////// SMAC ////////CCCCCC
!C
!C----------Original Dimension----------C
!      REAL(8) ::  SS1(NX+4,NY+4,NZ+4),PSI1(NX+4,NY+4,NZ+4)&
!     ,CX(NX+4,NY+4,NZ+4),CY(NX+4,NY+4,NZ+4)&
!     ,CXX(NX+4,NY+4,NZ+4),CYY(NX+4,NY+4,NZ+4)&
!     ,CZ(NX+4,NY+4,NZ+4),CZZ(NX+4,NY+4,NZ+4)&
!     ,UCP(NX+4,NY+4,NZ+4),RPE(NX+4,NY+4,NZ+4)
!
!//////////allocate??øΩ?øΩz??øΩ?øΩ??øΩ?øΩ…ïœùX//////////!
!
      REAL(8), allocatable :: &
      SS1(:,:,:),PSI1(:,:,:)&
     ,CX(:,:,:),CY(:,:,:)&
     ,CXX(:,:,:),CYY(:,:,:)&
     ,CZ(:,:,:),CZZ(:,:,:)&
     ,UCP(:,:,:),RPE(:,:,:)
!
!C
!CCCCCCC////// KAKI ////////CCCCCC
!C
      INTEGER*2 t / 2313 /
      REAL(8) :: wx,wy,wz
!
!C----------Original Dimension----------C
!
!NONE
!      
!C
!CCCCCCC////// SALINITY ////////CCCCCC
!C
      REAL(8) :: SGSA,TOS
!
!C----------Original Dimension----------C
!      REAL(8) ::  SALG(NX+4,NY+4,NZ+4),SALE(NX+4,NY+4,NZ+4)&
!     ,SALGG(NX+4,NY+4,NZ+4),SALEE(NX+4,NY+4,NZ+4)&
!     ,SA0(NX+4,NY+4,NZ+4),SAN(NX+4,NY+4,NZ+4)&
!     ,DKS(NX+4,NY+4,NZ+4),SKS(NX+4,NY+4,NZ+4)&
!     ,SALAG(NX+4,NY+4,NZ+4),SALAE(NX+4,NY+4,NZ+4)&
!     ,SALAGG(NX+4,NY+4,NZ+4),SALAEE(NX+4,NY+4,NZ+4)&
!     ,SALG1(NX+4,NY+4,NZ+4),SALE1(NX+4,NY+4,NZ+4)&
!     ,SALGG1(NX+4,NY+4,NZ+4),SALEE1(NX+4,NY+4,NZ+4)
!
!//////////allocate??øΩ?øΩz??øΩ?øΩ??øΩ?øΩ…ïœùX//////////!
!
      REAL(8), allocatable :: &
      SALG(:,:,:),SALE(:,:,:)&
     ,SALGG(:,:,:),SALEE(:,:,:)&
     ,SA0(:,:,:),SAN(:,:,:)&
     ,DKS(:,:,:),SKS(:,:,:)&
     ,SALAG(:,:,:),SALAE(:,:,:)&
     ,SALAGG(:,:,:),SALAEE(:,:,:)&
     ,SALG1(:,:,:),SALE1(:,:,:)&
     ,SALGG1(:,:,:),SALEE1(:,:,:)

     REAL(8), allocatable :: &
     KU(:,:,:),KUS(:,:,:) &
    ,KV(:,:,:),KVS(:,:,:) &
    ,KW(:,:,:),KWS(:,:,:) &
    ,KC(:,:,:),KMYT(:,:,:) &
    ,KHK(:,:,:),KP(:,:,:) &
    ,WWX(:,:,:),WWY(:,:,:) &
    ,WWZ(:,:,:),Qvalue(:,:,:)

    end module MF_SF   
!C 
!C      
!*********************************************************************
!**                                                                  *
!**    The THREE DIMENSIONAL COMPUTATION OF NAVIER-STOKES EQUATIONS  *
!**                                                                  *
!**                          SF-1u4.f                                *
!**                                                                  *
!**                 FOR   MULTIPHASE TURBULENT   FLOW                *
!**           (3-D LES model for plane solid-liquid plume)           *
!**                       8/23  1997   BY Y.NIHEI                    *
!*********************************************************************
!*
!*****************************************************************
!*                          MAINROUTINE
!*****************************************************************
!
!CCCCCCCCCC////////// Advance Report //////////CCCCCCCCCC!
!   2022/10/26, Wed., [.f to .f90] started by Takashi Inoue.
!   2022/10/28, Fri., [.f to .f90] completed by Takashi Inoue.
!   2022/10/28, Fri., [.f90 added QOpenMP] started by Takashi Inoue.
!   2022/10/29, Sat., [.f90 added QOpenMP] completed without Subroutine GAL by Takashi Inoue.

    USE MF_SF
    USE OMP_LIB
    IMPLICIT NONE
    INTEGER MCLOCK    !221107TI

    DEPMODE=1.0

    OPEN(99,FILE='output/CPUTIME.txt')
    !OPEN(98,FILE='seiche.txt')

!C                   *******************
                        IT1=MCLOCK()
                         CALL INIT
                        IT2=MCLOCK()
                        CPUTIME01=IT2-IT1

                         !CALL KAKI2
                         CALL KAKI2F
                         !CALL KAKI3
                         !CALL KAKI4
                         !STOP

!C                   *******************
!
!********** CODING FOR HOT START **********!
!
!!!!!                         CALL READ_HS
!
            DO 100 N=N1,NMAX
!!!!!            DO 100 N=NMIN,NMAX
!                WRITE(*,*) NMIN,N
!C
                       TIME=DT*N
!
!                        IF(TIME.LE.1.0) THEN
!                            naka=MOD(N,100)
!                        ELSE IF (TIME.GT.1.0) THEN
                            naka=MOD(N,100) !ITO CHANGE
!                        END IF
!
!                       naka=MOD(N,100)
                       IF (naka.EQ.0.0) THEN
                       WRITE(*,15) TIME
 15                  FORMAT (20X,'T=',F10.4)
                       END IF
!C
!C                   *******************
                        IT3=MCLOCK()
                         CALL EXTP
                        IT4=MCLOCK()
                        CPUTIME02=CPUTIME02+(IT4-IT3)
!C                   ******************
!
                       ITC=0
                       ITC1=0
!C
  200                  CONTINUE
!C                      *******************
                        IT5=MCLOCK()
                         CALL ENERGY
                        IT6=MCLOCK()
                        CPUTIME03=CPUTIME03+(IT6-IT5)
                         ITC=ITC+1
                      !IF(ITC.GT.1.99) GO TO 250
                      !IF(GOSAE.GT.EPS4) GO TO 200
  250                   CONTINUE
!C                      *******************
!
  201                  CONTINUE

                       CALL KBC

!C{89 line}             *******************
                        IT7=MCLOCK()
                         CALL NSE
                        IT8=MCLOCK()
                        CPUTIME04=CPUTIME04+(IT8-IT7)
                         ITC1=ITC1+1
                      !IF(ITC1.GT.1.99) GOTO 251
!C                      IF(GOSAQ.GT.EPS4) GO TO 201
                      !IF(GOSAQ.GT.EPS2) GOTO 201
  251                   CONTINUE
!C                      *******************
!
!C                       IF(GOSAC.GT.EPS2) GO TO 200
!C                       IF(GOSAQ.GT.EPS2) GO TO 200
!C
!C                          IF(TIME.GT.1.80)  CALL GAL
                        IT9=MCLOCK()
                         !CALL GAL
                        IT10=MCLOCK()
                        CPUTIME05=CPUTIME05+(IT10-IT9)
!
                       ITC=0
!CCCCCCCC                            CALL SALINITY
!C
                        IT11=MCLOCK()
                         CALL SMAC
                        IT12=MCLOCK()
                        CPUTIME06=CPUTIME06+(IT12-IT11)

                        !CALL KBC
!
!C  270                  CONTINUE
!C=================================================================
!C
!C
!C
!C                       naka=MOD(N,20)
                       IF (naka.EQ.0.0) THEN
                         WRITE(*,17)
 17   FORMAT (8X,'-------------------------------------------')
                       END IF
!C                      *******************
!C
!                           HDD=MOD(N,100)
!                           IF(HDD.EQ.0) THEN
                           IF(naka.EQ.0) THEN

                             !CALL KAKI2
                             CALL KAKI2F
                             !CALL KAKI3
                             !CALL KAKI4

                             WRITE(*,18)
                            END IF
!C
 18    FORMAT('   DATA OUTPUT')
!C
!C                        naka=MOD(N,4)
                       IF (naka.EQ.0.0) THEN
                           WRITE(*,16)
 16    FORMAT(8X,'*******************************************')
                       END IF
!C
         WRITE(99,321)CPUTIME01*0.01/60.0
  321    FORMAT('INIT_CPU(min)   =  ',F12.5)
         WRITE(99,323)CPUTIME02*0.01/60.0
  323    FORMAT('EXTP_CPU(min)   =  ',F12.5)
         WRITE(99,325)CPUTIME03*0.01/60.0
  325    FORMAT('ENERGY_CPU(min)      =  ',F12.5)
         WRITE(99,327)CPUTIME04*0.01/60.0
  327    FORMAT('NSE_CPU(min)       =  ',F12.5)
         WRITE(99,329)CPUTIME05*0.01/60.0
  329    FORMAT('GAL_CPU(min)  =  ',F12.5)
         WRITE(99,331)CPUTIME06*0.01/60.0
  331    FORMAT('SMAC_CPU(min)    =  ',F12.5)
!
  100   CONTINUE
!
      CLOSE(51)
      !CLOSE(98)
!
      STOP
      END
!C
!C
!C*****************************************************************
      SUBROUTINE GAL
!C*****************************************************************
    USE MF_SF
    USE omp_lib
    IMPLICIT NONE

    allocate ( &
    fx(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4,-1:3,-1:3,-1:3) &
     ,XF(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4,-1:3,-1:3,-1:3) &
     ,fy(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4,-1:3,-1:3,-1:3) &
     ,YF(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4,-1:3,-1:3,-1:3) &
     ,fz(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4,-1:3,-1:3,-1:3) &
     ,ZF(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4,-1:3,-1:3,-1:3) &
     ,CT(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),TLT(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,UST(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),VST(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,USRT(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),VSRT(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,WSRT(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),WST(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,OMEXG(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),OMEYG(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &   !OMEX??øΩ?øΩ??øΩ?øΩOMEXG,OMEY??øΩ?øΩ??øΩ?øΩOMEYG
     ,OMEZG(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),HKW(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &     !OMEZ??øΩ?øΩ??øΩ?øΩOMEZG
     ,HKU(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),HKV(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,DD1G(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),DD2G(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &     !DD1??øΩ?øΩ??øΩ?øΩDD1G,DD2??øΩ?øΩ??øΩ?øΩDD2G
     ,DD3G(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &                 !DD3??øΩ?øΩ??øΩ?øΩDD3G
     ,AU(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),W0(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,AP1(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),AP2(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,BTE(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),AP3(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
!
     ,XXM(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),YYM(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,DX1(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),DX2(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,DY1(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),DY2(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,DZ1(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),DZ2(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,USQ(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),VSQ(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,WSQ(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),ZZM(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,TP(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),VR(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,VSQ2(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),YYM2(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,TAUP(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),JI(-1:IMAX+4,-1:KMAX+4) &
!     ,TAUS(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),PS(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,CAV(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),SL(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
!     ,PCX(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),PCY(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,PRO(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),DIS(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,TS(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),arfa0(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
!     ,TAUC11(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),TAUC12(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
!     ,TAUC21(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),TAUC22(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
!     ,TAUC31(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),TAUC32(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
!     ,TAUC13(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),TAUC23(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
!     ,TAUC33(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),PCZ(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,XXM2(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),USQ2(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,ZZM2(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),WSQ2(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,STAT = ALLOC_ERR)

!C//////////////  Gravity(AG) & Pressure Gradient(AP) ////////////

        sita0=ATAN(SLOPE)*180.0/PHI !0.0025
        AG1=(-GG*(RS-RC)/(RS+CM0*RC))*SIN(sita0)
        AG2=(-GG*(RS-RC)/(RS+CM0*RC))*COS(sita0)
        AG3=0.0
        DT1=DT*1.0
        B00=RC*(1.0+CM0)/(RS+RC*CM0)    !B0??øΩ?øΩ??øΩ?øΩB00, SUMO23/05/04

!$OMP PARALLEL
!$OMP DO
        DO 500 K=3,KMAX+2
        DO 500 I=3,IMAX+2
        DO 500 J=3,JMAX+1

!********** FOR ONE-WAY ANALYSIS ***********!
!
!            IF (J.LE.8) THEN
!                U(I,J,K)=0.0
!            ELSE
!                U(I,J,K)=5.84*(1.0/0.40*log(1.0*(J-8.5))+8.50)
!            END IF
!
!            V(I,J,K)=0.0
!            HK(I,J,K)=1.0
!            DET(I,J,K)=1.0

!********************************************!
!
!                AP1(I,J,K)=((U(I,J,K)-U1(I,J,K))/DT &
!                            +U(I,J,K)*(U(I+1,J,K)-U(I-1,J,K))*0.50*GX(I) &
!                            +V(I,J,K)*(U(I,J+1,K)-U(I,J-1,K))*0.50*EY(J) &
!                            +W(I,J,K)*(U(I,J,K+1)-U(I,J,K-1))*0.50*DZ(K) &
!                            -ABS(U(I,J,K))*0.50*GX(I) &
!                               *(U(I+1,J,K)-2.0*U(I,J,K)+U(I-1,J,K)) &
!                            -ABS(V(I,J,K))*0.50*EY(J) &
!                               *(U(I,J+1,K)-2.0*U(I,J,K)+U(I,J-1,K)) &
!                            -ABS(W(I,J,K))*0.50*DZ(K) &
!                               *(U(I,J,K+1)-2.0*U(I,J,K)+U(I,J,K-1))) &
!                            *RC*(1.0+CM0)/(RS+RC*CM0)
!
!********** FOR OSCILLATING FLOW ***********!
!
!                AP1(I,J,K)=0.010*RC*(1.0+CM0)/(RS+RC*CM0)/DT
!                AP1(I,J,K)=AA*COS(2*PHI*N*DT/T0)*RC*(1.0+CM0)/(RS+RC*CM0)
!
!*******************************************!

                !AP1(I,J,K)=API(I,J,K)*RC*(1.0+CM0)/(RS+RC*CM0)
                !AP1(I,J,K)=-(DETADX(I,K)/DT)*RC*(1.0+CM0)/(RS+RC*CM0)
                !AP1(I,J,K)=USTAB(I,K)**2/DEP(I,K)*RC*(1.0+CM0)/(RS+RC*CM0)
                AP1(I,J,K)=GG*SLOPE*RC*(1.0+CM0)/(RS+RC*CM0)

                AP2(I,J,K)=((V(I,J,K)-V1(I,J,K))/DT &
                            +U(I,J,K)*(V(I+1,J,K)-V(I-1,J,K))*0.50*GX(I) &
                            +V(I,J,K)*(V(I,J+1,K)-V(I,J-1,K))*0.50*EY(J) &
                            +W(I,J,K)*(V(I,J,K+1)-V(I,J,K-1))*0.50*DZ(K) &
                            -ABS(U(I,J,K))*0.50*GX(I) &
                               *(V(I+1,J,K)-2.0*V(I,J,K)+V(I-1,J,K)) &
                            -ABS(V(I,J,K))*0.50*EY(J) &
                               *(V(I,J+1,K)-2.0*V(I,J,K)+V(I,J-1,K)) &
                            -ABS(W(I,J,K))*0.50*DZ(K) &
                               *(V(I,J,K+1)-2.0*V(I,J,K)+V(I,J,K-1))) &
                            *RC*(1.0+CM0)/(RS+RC*CM0)*0.0
!
                AP3(I,J,K)=((W(I,J,K)-W1(I,J,K))/DT &
                            +U(I,J,K)*(W(I+1,J,K)-W(I-1,J,K))*0.50*GX(I) &
                            +V(I,J,K)*(W(I,J+1,K)-W(I,J-1,K))*0.50*EY(J) &
                            +W(I,J,K)*(W(I,J,K+1)-W(I,J,K-1))*0.50*DZ(K) &
                            -ABS(U(I,J,K))*0.50*GX(I) &
                               *(W(I+1,J,K)-2.0*W(I,J,K)+W(I-1,J,K)) &
                            -ABS(V(I,J,K))*0.50*EY(J) &
                               *(W(I,J+1,K)-2.0*W(I,J,K)+W(I,J-1,K)) &
                            -ABS(W(I,J,K))*0.50*DZ(K) &
                               *(W(I,J,K+1)-2.0*W(I,J,K)+W(I,J,K-1))) &
                            *RC*(1.0+CM0)/(RS+RC*CM0)*0.0
!
 500  CONTINUE
!
            !$OMP END DO
            !$OMP END PARALLEL
!
!            IF(MOD(N,100).EQ.0)THEN
!            WRITE(num,'(I6.6)')N
!            OPEN(88,file='check/AP1/AP1'//num//'.txt')
!!            DO K=3,KMAX+2
!                DO I=3,IMAX+2
!                    DO J=3,JMAX+1
!                        WRITE(88,'(2i5,9f15.5)') I,K,USTAB(I,K)**2/DEP(I,K)*RC*(1.0+CM0)/(RS+RC*CM0), &
!                            -(DETADX(I,K)/DT)*RC*(1.0+CM0)/(RS+RC*CM0)
!                    END DO
!                END DO
!            CLOSE(88)
!            END IF
!
            !$OMP PARALLEL
            !$OMP DO
!
            DO 550 K=3,KMAX+2
            DO 550 I=3,IMAX+2
            DO 550 J=3,JMAX+1
!
                HKU(I,J,K)=-2.0*MYT(I,J,K)*(U(I+1,J,K)-U(I-1,J,K))*GX(I)*0.50 &
                           +2.0/3.0*HK(I,J,K)
                HKV(I,J,K)=-2.0*MYT(I,J,K)*(V(I,J+1,K)-V(I,J-1,K))*EY(J)*0.50 &
                           +2.0/3.0*HK(I,J,K)
                HKW(I,J,K)=-2.0*MYT(I,J,K)*(W(I,J,K+1)-W(I,J,K-1))*DZ(K)*0.50 &
                           +2.0/3.0*HK(I,J,K)
!
 550  CONTINUE
!
            !$OMP END DO
            !$OMP END PARALLEL
!
!C/////////////   Drag Force   //////////////////////
!
            !$OMP PARALLEL
            !$OMP DO
!
            DO 600 K=3,KMAX+2
            DO 600 I=3,IMAX+2
            DO 600 J=3,JMAX+1
!
                RE1G(I,J,K)=SQRT(ABS(US(I,J,K)-U(I,J,K))**2 &   !RE1??øΩ?øΩ??øΩ?øΩRE1G, SUMO23/05/04
                                +ABS(VS(I,J,K)-V(I,J,K))**2 &
                                +ABS(WS(I,J,K)-W(I,J,K))**2) &
                                *DR/RED
!C
 600  CONTINUE
!
            !$OMP END DO
            !$OMP END PARALLEL
!
            !$OMP PARALLEL
            !$OMP DO
!
            DO 650 K=3,KMAX+2
            DO 650 I=3,IMAX+2
            DO 650 J=3,JMAX+1
!
!********** FOR ONE-WAY ANALYSIS ***********!
!
!                BTE(I,J,K)=0.75*RC/DR*0.40 &
!                          *((U(I,J,3)-US(I,J,3))**2 &
!                           +(V(I,J,3)-VS(I,J,3))**2)**0.50
!
!********************************************!
!
                BTE(I,J,K)=18.0*RED/DR**2*RC/(RS+CM0*RC) &
!                          *(0.650-CM(I,J,K))**(-2.70)
!                          *(1.0-CM(I,J,K))**(-2.70)
!                          *(1.0+0.150*RE1G(I,J,K)**0.6870)   !RE1??øΩ?øΩ??øΩ?øΩRE1G, SUMO23/05/04
                          *(1.0+0.4320*RE1G(I,J,K)**0.6870)  !SUMO23/10/23
!                          *(1.0+0.150*RE1G(I,J,K))**0.6870   !RE1??øΩ?øΩ??øΩ?øΩRE1G, SUMO23/05/04
!
 650  CONTINUE
!
            !$OMP END DO
            !$OMP END PARALLEL
!
!C/////////////   Lift Force   //////////////////////
!
            !$OMP PARALLEL
            !$OMP DO
!
            DO 700 K=3,KMAX+2
            DO 700 I=3,IMAX+2
            DO 700 J=3,JMAX+1
!
                OMEXG(I,J,K)=RC/(RS+RC*CM0)*CL0* &  !OMEX??øΩ?øΩ??øΩ?øΩOMEXG, SUMO23/05/04
                            ((W(I,J+1,K)-W(I,J-1,K))*0.50*EY(J) &
                            -(V(I,J,K+1)-V(I,J,K-1))*0.50*DZ(K))
                OMEYG(I,J,K)=RC/(RS+RC*CM0)*CL0* &  !OMEY??øΩ?øΩ??øΩ?øΩOMEYG, SUMO23/05/04
                            ((U(I,J,K+1)-U(I,J,K-1))*0.50*DZ(K) &
                            -(W(I+1,J,K)-W(I-1,J,K))*0.50*GX(I))
                OMEZG(I,J,K)=RC/(RS+RC*CM0)*CL0* &  !OMEZ??øΩ?øΩ??øΩ?øΩOMEZG, SUMO23/05/04
                            ((V(I+1,J,K)-V(I-1,J,K))*0.50*GX(I) &
                            -(U(I,J+1,K)-U(I,J-1,K))*0.50*EY(J))
!C
 700  CONTINUE
!
            !$OMP END DO
            !$OMP END PARALLEL
!
!C/////  Integral Time Scale of fluid turbulence  /////
!
            !$OMP PARALLEL
            !$OMP DO
!
            DO 750 K=3,KMAX+2
            DO 750 I=3,IMAX+2
            DO 750 J=3,JMAX+1
!
                IF(HK(I,J,K).GT.1.0E-7) THEN
!                    TLT(I,J,K)=1.50*ABS(MYTS(I,J,K)/HKS(I,J,K))
                    TLT(I,J,K)=5.0/12.0*DET(I,J,K)/SQRT(ABS(HK(I,J,K)))/CE
                ELSE
                    TLT(I,J,K)=0.0
                END IF
!C
!C       IF(SQRT((US(I,J,K)-U(I,J,K))**2+(VS(I,J,1)-V(I,J,1))**2)
!C     &    .GT.1.0E-5) THEN
!C             TLW(I,J,1)=DR/ST
!C     &         /SQRT((US(I,J,1)-U(I,J,1))**2+(VS(I,J,1)-V(I,J,1))**2)
!C       ELSE
!C             TLW(I,J,1)=0.0
!C       END IF
!C
                VR(I,J,K)=SQRT((U(I,J,K)-US(I,J,K))**2 &
                              +(V(I,J,K)-VS(I,J,K))**2 &
                              +(W(I,J,K)-WS(I,J,K))**2)
!C
                TAUP(I,J,K)=DR**2/(18.0*RED)*RS/RC &
                            /(1.0-CM(I,J,K))**(-2.70) &
!C     &            /(1.0+0.150*RE1G(I,J,K))**0.6870   !RE1??øΩ?øΩ??øΩ?øΩRE1G, SUMO23/05/04
!                            *(1.0+0.150*RE1G(I,J,K)**0.6870)   !RE1??øΩ?øΩ??øΩ?øΩRE1G, SUMO23/05/04
                            *(1.0+0.4320*RE1G(I,J,K)**0.6870)   !SUMO23/10/23
 750  CONTINUE
!
            !$OMP END DO
            !$OMP END PARALLEL
!
            !$OMP PARALLEL
            !$OMP DO
!
            DO 800 K=3,KMAX+2
            DO 800 I=3,IMAX+2
            DO 800 J=3,JMAX+1
!
                AU(I,J,K)=18.0*RED/DR**2*RC/(RS+CM0*RC) &
!                        *(1-CM(I,J,K))**(-2.70)*(1.0+0.150*RE1G(I,J,K)**0.687)   !RE1??øΩ?øΩ??øΩ?øΩRE1G, SUMO23/05/04
                        *(1-CM(I,J,K))**(-2.70)*(1.0+0.4320*RE1G(I,J,K)**0.687)   !SUMO23/10/23
!                        *(1-CM(I,J,K))**(-2.70)*(1.0+0.150*RE1G(I,J,K))**0.687   !RE1??øΩ?øΩ??øΩ?øΩRE1G, SUMO23/05/04
!
                W0(I,J,K)=2.0*PHI/DET(I,J,K) &
                        *SQRT(U(I,J,K)**2+V(I,J,K)**2+W(I,J,K)**2)
!
!                TP(I,J,K)=(TLT(I,J,K)+TAUP)
!                         /SQRT(1.0+(VR(I,J,K)*TLT(I,J,K)/DET(I,J,K))**2)
                TP(I,J,K)=(TLT(I,J,K)+TAUP(I,J,K)) &
                        /SQRT(1.0+(VR(I,J,K)*TLT(I,J,K)/DET(I,J,K))**2)
!C
 800  CONTINUE
!
            !$OMP END DO
            !$OMP END PARALLEL
!
            !$OMP PARALLEL
            !$OMP DO
!
            DO 815 K=3,KMAX+2
            DO 815 I=3,IMAX+2
            DO 815 J=3,JMAX+1
!
            IF (CM(I,J,K).GT.1.0E-7) THEN
                UUSR(I,J,K)=HKU(I,J,K)*(AU(I,J,K)**2+B00*W0(I,J,K)**2) &    !B0??øΩ?øΩ??øΩ?øΩB00, SUMO23/05/04
                                      /(AU(I,J,K)**2+W0(I,J,K)**2)
                VVSR(I,J,K)=HKV(I,J,K)*(AU(I,J,K)**2+B00*W0(I,J,K)**2) &    !B0??øΩ?øΩ??øΩ?øΩB00, SUMO23/05/04
                                      /(AU(I,J,K)**2+W0(I,J,K)**2)
                WWSR(I,J,K)=HKW(I,J,K)*(AU(I,J,K)**2+B00*W0(I,J,K)**2) &    !B0??øΩ?øΩ??øΩ?øΩB00, SUMO23/05/04
                                      /(AU(I,J,K)**2+W0(I,J,K)**2)
            ELSE
                UUSR(I,J,K)=0.0
                VVSR(I,J,K)=0.0
                WWSR(I,J,K)=0.0
            END IF
!C
 815  CONTINUE
!
            !$OMP END DO
            !$OMP END PARALLEL
!
!C****************************************************************
!C**************  Contact Force between particles ****************
!C****************************************************************

        myufs=0.500
        myufd=0.500
        CS0=0.100   !CS0=0.0700
        CSmin=0.400
        CSmax=0.600
        ee=0.80

!        sita0=ATAN(SLOPE)*180.0/PHI !0.0025

        DO 820 K=1,KMAX+4   !K=1,KMAX+4   !org
        DO 820 I=1,IMAX+4   !I=1,IMAX+4   !org

            JI(I,K)=3

            DO 825 J=3,JMAX+1
                IF (CM(I,J,K).GT.CS0) THEN
                    IF (CM(I,J+1,K).LE.CS0) THEN
                        JI(I,K)=J
                        GO TO 821
                    END IF
                END IF

 825    CONTINUE
 821    CONTINUE

        CAV(I,J,K)=0.0

 820    CONTINUE

        DO 830 K=1,KMAX+4   !K=1,KMAX+4   !org
        DO 830 I=1,IMAX+4   !I=1,IMAX+4   !org
        DO 830 J=JI(I,K)+1,3,-1

            CAV(I,J,K)=CAV(I,J+1,K)+CM(I,J,K)/EY(J)

 830    CONTINUE

        DO 835 K=1,KMAX+4   !K=1,KMAX+4   !org
        DO 835 I=1,IMAX+4   !I=1,IMAX+4   !org
        DO 835 J=2,JMAX+2   !J=2,JMAX+2   !org

            IF (J.LE.JI(I,K)) THEN
                CINT=CAV(I,J,K)/(Y(I,JI(I,K)+1,K)-Y(I,J,K))
                IF (CINT.LT.CSmin) THEN
                    arfa0(I,J,K)=0.0
                ELSE
                    IF (CINT.LT.CSmax) THEN
                        arfa0(I,J,K)=((CINT-CSmin)/(CSmax-CSmin))**0.50
                    ELSE
                        arfa0(I,J,K)=1.0
                    END IF
                END IF
            ELSE
                arfa0(I,J,K)=0.0
            END IF

!                SL(I,J)=1.0/(1.0-(CM(I,J+1,3)/CSmax)**0.333)
                SL(I,J,K)=1.0/(1.0-(CM(I,J,K)/(CSmax+0.50))**0.333)

 835    CONTINUE

    !$OMP PARALLEL
    !$OMP DO

            DO 840 K=2,KMAX+3   !K=2,KMAX+3   !org
            DO 840 I=2,IMAX+3   !I=2,IMAX+3   !org
            DO 840 J=2,JMAX+1   !J=2,JMAX+1   !org
!C
!C      PS(I,J,K)=1.0/3.0*(arfa0(I-1,J,K)*CAV(I-1,J,K)
!C     &                  +arfa0(I,J,K)*CAV(I,J,K)
!C     &                  +arfa0(I+1,J,K)*CAV(I+1,J,K))*(RS-RC)*GG
!C      TAUS(I,J,K)=1.0/3.0*(arfa0(I-1,J,K)*CAV(I-1,J,K)
!C     &                    +arfa0(I,J,K)*CAV(I,J,K)
!C     &                    +arfa0(I+1,J,K)*CAV(I+1,J,K))*(RS-RC)*GG
!C     &        *COS(sita0)*myufd
!C
                PS(I,J,K)=1.0/9.0*(arfa0(I-1,J,K)*CAV(I-1,J,K) &
                                  +arfa0(I,J,K)*CAV(I,J,K) &
                                  +arfa0(I+1,J,K)*CAV(I+1,J,K) &
                                  +arfa0(I-1,J,K+1)*CAV(I-1,J,K+1) &
                                  +arfa0(I,J,K+1)*CAV(I,J,K+1) &
                                  +arfa0(I+1,J,K+1)*CAV(I+1,J,K+1) &
                                  +arfa0(I-1,J,K-1)*CAV(I-1,J,K-1) &
                                  +arfa0(I,J,K-1)*CAV(I,J,K-1) &
                                  +arfa0(I+1,J,K-1)*CAV(I+1,J,K-1) &
                                  )*(RS-RC)*GG*COS(sita0)
!
                TAUS(I,J,K)=PS(I,J,K)*myufs
!
!                TAUS(I,J,K)=1.0/9.0*(arfa0(I-1,J,K)*CAV(I-1,J,K) &
!                                    +arfa0(I,J,K)*CAV(I,J,K) &
!                                    +arfa0(I+1,J,K)*CAV(I+1,J,K) &
!                                    +arfa0(I-1,J,K+1)*CAV(I-1,J,K+1) &
!                                    +arfa0(I,J,K+1)*CAV(I,J,K+1) &
!                                    +arfa0(I+1,J,K+1)*CAV(I+1,J,K+1) &
!                                    +arfa0(I-1,J,K-1)*CAV(I-1,J,K-1) &
!                                    +arfa0(I,J,K-1)*CAV(I,J,K-1) &
!                                    +arfa0(I+1,J,K-1)*CAV(I+1,J,K-1) &
!                                    )*(RS-RC)*GG*COS(sita0)*myufd
!
                TAUC11(I,J,K)=32.0/15.0*RS*DR*(0.50*(CM(I,J,K)+CM(I+1,J,K)))**2 &
                                              *0.50*(SL(I,J,K)+SL(I+1,J,K))*(1+ee) &
                                 /(PHI**0.50)*(0.50*(TEMP(I,J,K)+TEMP(I+1,J,K)))**0.50 &
                                         *(1.0-0.50*(CM(I,J,K)+CM(I+1,J,K))/CSmax)**3.0 &
                                             *(US(I+1,J,K)-US(I,J,K))*GX(I)
!
                TAUC12(I,J,K)=32.0/15.0*RS*DR*(0.50*(CM(I,J,K)+CM(I+1,J,K)))**2 &
                                              *0.50*(SL(I,J,K)+SL(I+1,J,K))*(1+ee) &
                                 /(PHI**0.50)*(0.50*(TEMP(I,J,K)+TEMP(I+1,J,K)))**0.50 &
                                         *(1.0-0.50*(CM(I,J,K)+CM(I+1,J,K))/CSmax)**3.0 &
                                              *0.50*((US(I,J+1,K)-US(I,J-1,K) &
                                                     +US(I+1,J+1,K)-US(I+1,J-1,K))*0.25*EY(J) &
                                            +(VS(I+1,J,K)-VS(I,J,K))*GX(I))
!
                TAUC13(I,J,K)=32.0/15.0*RS*DR*(0.50*(CM(I,J,K)+CM(I+1,J,K)))**2 &
                                              *0.50*(SL(I,J,K)+SL(I+1,J,K))*(1+ee) &
                                 /(PHI**0.50)*(0.50*(TEMP(I,J,K)+TEMP(I+1,J,K)))**0.50 &
                                         *(1.0-0.50*(CM(I,J,K)+CM(I+1,J,K))/CSmax)**3.0 &
                                              *0.50*((US(I,J,K+1)-US(I,J,K-1) &
                                                     +US(I+1,J,K+1)-US(I+1,J,K-1))*0.25*DZ(K) &
                                            +(WS(I+1,J,K)-WS(I,J,K))*GX(I))
!
                TAUC21(I,J,K)=32.0/15.0*RS*DR*(0.50*(CM(I,J,K)+CM(I,J+1,K)))**2 &
                                              *0.50*(SL(I,J,K)+SL(I,J+1,K))*(1+ee) &
                                 /(PHI**0.50)*(0.50*(TEMP(I,J,K)+TEMP(I,J+1,K)))**0.50 &
                                         *(1.0-0.50*(CM(I,J,K)+CM(I,J+1,K))/CSmax)**3.0 &
                                              *0.50*((VS(I+1,J+1,K)-VS(I-1,J+1,K) &
                                                     +VS(I+1,J,K)-VS(I-1,J,K))*0.25*GX(I) &
                                            +(US(I,J+1,K)-US(I,J,K))*EY(J))
!
                TAUC22(I,J,K)=32.0/15.0*RS*DR*(0.50*(CM(I,J,K)+CM(I,J+1,K)))**2 &
                                              *0.50*(SL(I,J,K)+SL(I,J+1,K))*(1+ee) &
                                 /(PHI**0.50)*(0.50*(TEMP(I,J,K)+TEMP(I,J+1,K)))**0.50 &
                                         *(1.0-0.50*(CM(I,J,K)+CM(I,J+1,K))/CSmax)**3.0 &
                                            *(VS(I,J+1,K)-VS(I,J,K))*EY(J)
!
                TAUC23(I,J,K)=32.0/15.0*RS*DR*(0.50*(CM(I,J,K)+CM(I,J+1,K)))**2 &
                                              *0.50*(SL(I,J,K)+SL(I,J+1,K))*(1+ee) &
                                 /(PHI**0.50)*(0.50*(TEMP(I,J,K)+TEMP(I,J+1,K)))**0.50 &
                                         *(1.0-0.50*(CM(I,J,K)+CM(I,J+1,K))/CSmax)**3.0 &
                                              *0.50*((VS(I,J+1,K+1)-VS(I,J+1,K-1) &
                                                     +VS(I,J,K+1)-VS(I,J,K-1))*0.25*DZ(K) &
                                            +(WS(I,J+1,K)-WS(I,J,K))*EY(J))
!
                TAUC31(I,J,K)=32.0/15.0*RS*DR*(0.50*(CM(I,J,K)+CM(I,J,K+1)))**2 &
                                              *0.50*(SL(I,J,K)+SL(I,J,K+1))*(1+ee) &
                                 /(PHI**0.50)*(0.50*(TEMP(I,J,K)+TEMP(I,J,K+1)))**0.50 &
                                         *(1.0-0.50*(CM(I,J,K)+CM(I,J,K+1))/CSmax)**3.0 &
                                              *0.50*((WS(I+1,J,K+1)-WS(I-1,J,K+1) &
                                                     +WS(I+1,J,K)-WS(I-1,J,K))*0.25*GX(I) &
                                            +(US(I,J,K+1)-US(I,J,K))*DZ(K))
!
                TAUC32(I,J,K)=32.0/15.0*RS*DR*(0.50*(CM(I,J,K)+CM(I,J,K+1)))**2 &
                                              *0.50*(SL(I,J,K)+SL(I,J,K+1))*(1+ee) &
                                 /(PHI**0.50)*(0.50*(TEMP(I,J,K)+TEMP(I,J,K+1)))**0.50 &
                                         *(1.0-0.50*(CM(I,J,K)+CM(I,J,K+1))/CSmax)**3.0 &
                                              *0.50*((WS(I,J+1,K+1)-WS(I,J-1,K+1) &
                                                     +WS(I,J+1,K)-WS(I,J-1,K))*0.25*EY(J) &
                                            +(VS(I,J,K+1)-VS(I,J,K))*DZ(K))
!
                TAUC33(I,J,K)=32.0/15.0*RS*DR*(0.50*(CM(I,J,K)+CM(I,J,K+1)))**2 &
                                              *0.50*(SL(I,J,K)+SL(I,J,K+1))*(1+ee) &
                                 /(PHI**0.50)*(0.50*(TEMP(I,J,K)+TEMP(I,J,K+1)))**0.50 &
                                         *(1.0-0.50*(CM(I,J,K)+CM(I,J,K+1))/CSmax)**3.0 &
                                            *(WS(I,J,K+1)-WS(I,J,K))*DZ(K)
!
                PCX(I,J,K)=-2.0*RS*(0.50*(CM(I,J,K)+CM(I+1,J,K)))**2 &
                            *(1+ee)*0.50*(SL(I,J,K)+SL(I+1,J,K)) &
                            *0.50*(TEMP(I,J,K)+TEMP(I+1,J,K)) &
                            *(1.0-0.50*(CM(I,J,K)+CM(I+1,J,K))/CSmax)**3.0
                PCY(I,J,K)=-2.0*RS*(0.50*(CM(I,J,K)+CM(I,J+1,K)))**2 &
                            *(1+ee)*0.50*(SL(I,J,K)+SL(I,J+1,K)) &
                            *0.50*(TEMP(I,J,K)+TEMP(I,J+1,K)) &
                            *(1.0-0.50*(CM(I,J,K)+CM(I,J+1,K))/CSmax)**3.0
                PCZ(I,J,K)=-2.0*RS*(0.50*(CM(I,J,K)+CM(I,J,K+1)))**2 &
                            *(1+ee)*0.50*(SL(I,J,K)+SL(I,J,K+1)) &
                            *0.50*(TEMP(I,J,K)+TEMP(I,J,K+1)) &
                            *(1.0-0.50*(CM(I,J,K)+CM(I,J,K+1))/CSmax)**3.0
!
 840  CONTINUE
!
            !$OMP END DO
            !$OMP END PARALLEL

!*************** Pier Boundary Condition ***************!

        DO 9241 J=2,JMAX+1
        DO 9241 K=Kpier_R,Kpier_L
        DO 9241 I=Ipier_U,Ipier_D

            IF(pier.EQ.1.0) THEN
                PS(Ipier_D,J,K)=PS(Ipier_D+1,J,K)  !SUMO 23/09/06
                PS(I,J,Kpier_R)=PS(I,J,Kpier_R-1)  !SUMO 23/09/06
                PS(I,J,Kpier_L)=PS(I,J,Kpier_L+1)  !SUMO 23/09/06
                PS(Ipier_U,J,K)=PS(Ipier_U-1,J,K)  !SUMO 23/09/06

            END IF

 9241  CONTINUE

        DO 9242 J=2,JMAX+1
        DO 9242 K=Kpier_R1,Kpier_L1
        DO 9242 I=Ipier_U1,Ipier_D1

            IF(pier.EQ.1.0) THEN

                PS(Ipier_D1,J,K)=PS(Ipier_D1+1,J,K)  !simple
                !PS(I,J,Kpier_R1)=PS(I,J,Kpier_R1-1)  !SUMO 23/09/06
                !PS(I,J,Kpier_L1)=PS(I,J,Kpier_L1+1)  !SUMO 23/09/06
                PS(I,J,K)=PS(I,J,Kpier_L1+1)
                PS(Ipier_U1,J,K)=PS(Ipier_U1-1,J,K)  !SUMO 23/09/06

            END IF

 9242  CONTINUE

        DO 9243 J=2,JMAX+1
        DO 9243 K=Kpier_R2,Kpier_L2
        DO 9243 I=Ipier_U2,Ipier_D2

            IF(pier.EQ.1.0) THEN

                PS(Ipier_D2,J,K)=PS(Ipier_D2+1,J,K)  !simple
                !PS(I,J,Kpier_R2)=PS(I,J,Kpier_R2-1)  !SUMO 23/09/06
                !PS(I,J,Kpier_L2)=PS(I,J,Kpier_L2+1)  !SUMO 23/09/06
                PS(I,J,K)=PS(I,J,Kpier_R2-1)
                PS(Ipier_U2,J,K)=PS(Ipier_U2-1,J,K)  !SUMO 23/09/06

            END IF

 9243  CONTINUE

        DO 9244 J=2,JMAX+1
        DO 9244 K=Kpier_R3,Kpier_L3
        DO 9244 I=Ipier_U3,Ipier_D3

            IF(pier.EQ.1.0) THEN

                PS(Ipier_D3,J,K)=PS(Ipier_D3+1,J,K)  !simple
                !PS(I,J,Kpier_R3)=PS(I,J,Kpier_R3-1)  !SUMO 23/09/06
                !PS(I,J,Kpier_L3)=PS(I,J,Kpier_L3+1)  !SUMO 23/09/06
                PS(I,J,K)=PS(I,J,Kpier_L3+1)
                PS(Ipier_U3,J,K)=PS(Ipier_U3-1,J,K)  !SUMO 23/09/06

            END IF

 9244  CONTINUE

        DO 9245 J=2,JMAX+1
        DO 9245 K=Kpier_R4,Kpier_L4
        DO 9245 I=Ipier_U4,Ipier_D4

            IF(pier.EQ.1.0) THEN

                PS(Ipier_D4,J,K)=PS(Ipier_D4+1,J,K)  !simple
                !PS(I,J,Kpier_R4)=PS(I,J,Kpier_R4-1)  !SUMO 23/09/06
                !PS(I,J,Kpier_L4)=PS(I,J,Kpier_L4+1)  !SUMO 23/09/06
                PS(I,J,K)=PS(I,J,Kpier_R4-1)  !SUMO 23/09/06
                PS(Ipier_U4,J,K)=PS(Ipier_U4-1,J,K)  !SUMO 23/09/06

            END IF

 9245  CONTINUE

            !$OMP PARALLEL
            !$OMP DO

        DO 845 K=3,KMAX+2
        DO 845 I=3,IMAX+2
        DO 845 J=2,JMAX+2

            PRO(I,J,K)=(TAUC11(I,J,K)*(US(I+1,J,K)-US(I,J,K))*GX(I) &
                       +TAUC11(I-1,J,K)*(US(I,J,K)-US(I-1,J,K))*GX(I) &
                       +TAUC12(I,J,K)*(VS(I+1,J,K)-VS(I,J,K))*GX(I) &
                       +TAUC12(I-1,J,K)*(VS(I,J,K)-VS(I-1,J,K))*GX(I) &
                       +TAUC13(I,J,K)*(WS(I+1,J,K)-WS(I,J,K))*GX(I) &
                       +TAUC13(I-1,J,K)*(WS(I,J,K)-WS(I-1,J,K))*GX(I) &
                       +TAUC21(I,J,K)*(US(I,J+1,K)-US(I,J,K))*EY(J) &
                       +TAUC21(I,J-1,K)*(US(I,J,K)-US(I,J-1,K))*EY(J) &
                       +TAUC22(I,J,K)*(VS(I,J+1,K)-VS(I,J,K))*EY(J) &
                       +TAUC22(I,J-1,K)*(VS(I,J,K)-VS(I,J-1,K))*EY(J) &
                       +TAUC23(I,J,K)*(WS(I,J+1,K)-WS(I,J,K))*EY(J) &
                       +TAUC23(I,J-1,K)*(WS(I,J,K)-WS(I,J-1,K))*EY(J) &
                       +TAUC31(I,J,K)*(US(I,J,K)-US(I,J,K-1))*DZ(K) &
                       +TAUC31(I,J,K-1)*(US(I,J,K)-US(I,J,K-1))*DZ(K) &
                       +TAUC32(I,J,K)*(VS(I,J,K)-VS(I,J,K-1))*DZ(K) &
                       +TAUC32(I,J,K-1)*(VS(I,J,K)-VS(I,J,K-1))*DZ(K) &
                       +TAUC33(I,J,K)*(WS(I,J,K)-WS(I,J,K-1))*DZ(K) &
                       +TAUC33(I,J,K-1)*(WS(I,J,K)-WS(I,J,K-1))*DZ(K))*0.50
!
                DIS(I,J,K)=12.0/PHI**0.50*(1.0-ee**2)*RS*CM(I,J,K)**2/DR &
                            *SL(I,J,K)*TEMP(I,J,K)**1.50
!
                TS(I,J,K)=DR*(((CSmax+0.50)/(CM(I,J,K)+0.001))**(1.0/3.0)-1.0) &
                            /(TEMP(I,J,K)+0.0001)**0.50
!
                IF (TS(I,J,K).GT.TP(I,J,K)) TS(I,J,K)=TP(I,J,K)
!
 845  CONTINUE
!
            !$OMP END DO
            !$OMP END PARALLEL
!
!C****************************************************************
!C********   Equations of dispersed-particle motion  *************
!C****************************************************************

!C///////   Equations for Average-Velocity of Dispersed Particle  //////////

        DO 1000 K=3,KMAX+2
        DO 1000 I=3,IMAX+2
        DO 1000 J=3,JMAX+1

            FX1=(U(I,J,K)-US(I,J,K))*BTE(I,J,K)+AP1(I,J,K) &
               +((TAUC21(I,J,K)-TAUC21(I,J-1,K))*EY(J)+TAUS(I,J+1,K)*EY(J)*0.0 &
                -(PS(I+1,J,K)-PS(I-1,J,K))*0.50*GX(I)+(PCX(I,J,K)-PCX(I-1,J,K))*GX(I)) &
               /(CM(I,J,K)/CSmax+0.0001)/RS

!            FX2=PS(I,J,K)*myufs*EY(J)/(CM(I,J,K)/CSmax+0.0001)/RS  !org
            FX2=TAUS(I,J,K)*EY(J)/(CM(I,J,K)/CSmax+0.0001)/RS   !SUMO23/06/14

            IF(ABS(FX2).GE.ABS(FX1)) THEN
                US(I,J,K)=0.0
                VS(I,J,K)=0.0
                WS(I,J,K)=0.0
                TEMP(I,J,K)=0.10
                XG1(I,J,K)=X(I,J,K)
                YG(I,J,K)=Y(I,J,K)
                ZG(I,J,K)=Z(I,J,K)
            ELSE
                TEMP(I,J,K)=((PRO(I,J,K)-DIS(I,J,K))/(CM(I,J,K)/CSmax+0.00010)/RS*DT &
                            +(AU(I,J,K)*(AU(I,J,K)**2+B00*W0(I,J,K)**2) &    !B0->B00, SUMO23/05/04
                            /(AU(I,J,K)**2+W0(I,J,K)**2)*2.0*HK(I,J,K) &
                            +B00*(UUSR(I,J,K)-UUSR1(I,J,K)+VVSR(I,J,K)-VVSR1(I,J,K))/DT &
                            -B00**2*(HK(I,J,K)-HK1(I,J,K))/DT)*DT &    !B0->B00, SUMO23/05/04
                            +1.50*TEMP1(I,J,K))/(1.50+3.0*AU(I,J,K)*DT)

                IF (TEMP(I,J,K).LE.0.01) TEMP(I,J,K)=0.01

                    US(I,J,K)=(DT1*(AP1(I,J,K)+AG1+U(I,J,K)*BTE(I,J,K))+US1(I,J,K) &
                              +DT1*((TAUS(I,J+1,K)-TAUS(I,J,K))*EY(J) &
                                    -(0.50*(PS(I+1,J,K)+PS(I+1,J+1,K))-0.50*(PS(I-1,J,K)+PS(I-1,J+1,K)))*0.50*GX(I) &
                                    +(TAUC11(I,J,K)-TAUC11(I-1,J,K))*GX(I) &
                                    +(TAUC21(I,J,K)-TAUC21(I,J-1,K))*EY(J) &
                                    +(TAUC31(I,J,K)-TAUC31(I,J,K-1))*DZ(K) &
                                    +(PCX(I,J,K)-PCX(I-1,J,K))*GX(I)) &
                                /(CM(I,J,K)/CSmax+0.0001)/(RS+RC*CM0))/(1.0+DT1*BTE(I,J,K))

                    VS(I,J,K)=(DT1*(AP2(I,J,K)+AG2+V(I,J,K)*BTE(I,J,K))+VS1(I,J,K) &
                              +DT1*(-(PS(I,J+1,K)-PS(I,J,K))*EY(J) &
                                    +(TAUC12(I,J,K)-TAUC12(I-1,J,K))*GX(I) &
                                    +(TAUC22(I,J,K)-TAUC22(I,J-1,K))*EY(J) &
                                    +(TAUC32(I,J,K)-TAUC32(I,J,K-1))*DZ(K) &
                                    +(PCY(I,J,K)-PCY(I,J-1,K))*EY(J)) &
                                /(CM(I,J,K)/CSmax+0.0001)/(RS+RC*CM0))/(1.0+DT1*BTE(I,J,K))

                    WS(I,J,K)=(DT1*(AP3(I,J,K)+AG3+W(I,J,K)*BTE(I,J,K))+WS1(I,J,K) &
                              +DT1*((TAUS(I,J+1,K)-TAUS(I,J,K))*EY(J) & !added by sumo 23/09/05
                                    -(0.50*(PS(I,J,K+1)+PS(I,J+1,K+1))-0.50*(PS(I,J,K-1)+PS(I,J+1,K-1)))*0.50*DZ(K) &
                                    +(TAUC13(I,J,K)-TAUC11(I-1,J,K))*GX(I) &
                                    +(TAUC23(I,J,K)-TAUC21(I,J-1,K))*EY(J) &
                                    +(TAUC33(I,J,K)-TAUC31(I,J,K-1))*DZ(K) &
                                    +(PCZ(I,J,K)-PCZ(I,J,K-1))*DZ(K)) &
                                /(CM(I,J,K)/CSmax+0.0001)/(RS+RC*CM0))/(1.0+DT1*BTE(I,J,K))

            END IF

!C         DD1G(I,J,K)=(DT1*((TAUS(I,J)-TAUS(I,J-1))*EY(J)  !221027TI   !DD1->DD1G
!C     &              +U(I,J,K)*BTE(I,J,K))+US1(I,J,K))/
!C     &             (1.0+DT1*BTE(I,J,K))-US(I,J,K)
!C        DD1G(I,J,K)=(DT1*(AP1(I,J,K)+AG1                  !221027TI   !DD1->DD1G
!C     &              +OMEZG(I,J,K)*(V(I,J,K)-VS(I,J,K))    !OMEZ->OMEZG, SUMO23/05/04
!C     &              -OMEYG(I,J,K)*(W(I,J,K)-WS(I,J,K))    !OMEY->OMEYG, SUMO23/05/04
!C     &              +U(I,J,K)*BTE(I,J,K))+US1(I,J,K))/
!C     &             (1.0+DT1*BTE(I,J,K))-US(I,J,K)
!C
!C        DD2G(I,J,K)=(DT1*(AP2(I,J,K)+AG2                  !221027TI   !DD2->DD2G
!C     &              +OMEXG(I,J,K)*(W(I,J,K)-WS(I,J,K))    !OMEX->OMEXG, SUMO23/05/04
!C     &              -OMEZG(I,J,K)*(U(I,J,K)-US(I,J,K))    !OMEZ->OMEZG, SUMO23/05/04
!C     &              +V(I,J,K)*BTE(I,J,K))+VS1(I,J,K))/
!C     &             (1.0+DT1*BTE(I,J,K))-VS(I,J,K)
!C
!C        DD3G(I,J,K)=(DT1*(AP3(I,J,K)+AG3                  !221027TI   !DD3->DD3G
!C     &              +OMEYG(I,J,K)*(U(I,J,K)-US(I,J,K))    !OMEY->OMEYG, SUMO23/05/04
!C     &              -OMEXG(I,J,K)*(V(I,J,K)-VS(I,J,K))    !OMEX->OMEXG, SUMO23/05/04
!C     &              +W(I,J,K)*BTE(I,J,K))+WS1(I,J,K))/
!C     &             (1.0+DT1*BTE(I,J,K))-WS(I,J,K)
!C
 1000   CONTINUE

    !$OMP PARALLEL
    !$OMP DO
!
            DO 1050 K=3,KMAX+2
            DO 1050 I=3,IMAX+2
            DO 1050 J=3,JMAX+1
!
                IF (CM(I,J,K).LT.1.0E-7) THEN
!CC        US(I,J,K)=US(I,J,K)+DD1G(I,J,K)
!CC        VS(I,J,K)=VS(I,J,K)+DD2G(I,J,K)
!CC        WS(I,J,K)=WS(I,J,K)+DD3G(I,J,K)
!CC          ELSE
                    US(I,J,K)=0.0
                    VS(I,J,K)=0.0
                    WS(I,J,K)=0.0
                END IF
!C
 1050  CONTINUE
!
    !$OMP END DO
    !$OMP END PARALLEL
!
!C///////////  Dispersion Components  ///////////////////
!
    !$OMP PARALLEL
    !$OMP DO
!
            DO 1100 K=3,KMAX+2
            DO 1100 I=3,IMAX+2
            DO 1100 J=3,JMAX+1
!
            IF (CM(I,J,K).GT.1.0E-7) THEN
                USR(I,J,K)=TEMP(I,J,K)
                VSR(I,J,K)=TEMP(I,J,K)
                WSR(I,J,K)=TEMP(I,J,K)
            ELSE
                USR(I,J,K)=0.0
                UUSR(I,J,K)=0.0
                VSR(I,J,K)=0.0
                VVSR(I,J,K)=0.0
                WSR(I,J,K)=0.0
                WWSR(I,J,K)=0.0
            END IF
!
 1100   CONTINUE
!
    !$OMP END DO
    !$OMP END PARALLEL
!
!C***********************************************************
!C***********************************************************
!CCCCCC------------- BOUNDARY CONDITION ----------CCCCCCCCCCC
!C***********************************************************
!C***********************************************************
!C
!C********** Lower Boundary **********C
!C
              DO 50 J=1,3
              DO 50 K=1,KMAX+4
              DO 50 I=1,IMAX+4
!C
                  C(I,J,K)=0.600
                  TEMP(I,J,K)=0.0
                  US(I,J,K)=0.0
                  VS(I,J,K)=0.0
                  WS(I,J,K)=0.0
                  TS(I,J,K)=0.0
                  USR(I,J,K)=TEMP(I,J,K)
                  VSR(I,J,K)=TEMP(I,J,K)
                  WSR(I,J,K)=TEMP(I,J,K)
                  CM(I,J,K)=C(I,J,K)
!
                  XG1(I,J,K)=X(I,J,K)
                  YG(I,J,K)=Y(I,J,K)
                  ZG(I,J,K)=Z(I,J,K)
!C
 50   CONTINUE
!C
!C********** Upper Boundary **********C
!C
              DO 60 J=JMAX+2,JMAX+3
              DO 60 K=1,KMAX+4
              DO 60 I=1,IMAX+4
!C
                  C(I,J,K)=0.0    !C(I,JMAX+1,K)
                  US(I,J,K)=US(I,JMAX+1,K)
                  VS(I,J,K)=0.0   !VS(I,JMAX+1,K)
                  WS(I,J,K)=WS(I,JMAX+1,K)
                  TEMP(I,J,K)=TEMP(I,JMAX+1,K)
                  TS(I,J,K)=TS(I,JMAX+1,K)  !added by SUMO23/06/11
                  USR(I,J,K)=USR(I,JMAX+1,K)
                  VSR(I,J,K)=VSR(I,JMAX+1,K)
                  WSR(I,J,K)=WSR(I,JMAX+1,K)
                  CM(I,J,K)=C(I,J,K)
!
                  XG1(I,J,K)=X(I,J,K)
                  YG(I,J,K)=Y(I,J,K)
                  ZG(I,J,K)=Z(I,J,K)
!C
 60   CONTINUE

!C********** Left-Side Boundary **********C

            DO 80 I=1,2
            DO 80 K=1,KMAX+4
            DO 80 J=1,JMAX+3

!CCCCC////// Periodic Boundary Condition ////////

                C(I,J,K)=C(IMAX+I,J,K)
                CM(I,J,K)=CM(IMAX+I,J,K)
                US(I,J,K)=US(IMAX+I,J,K)
                VS(I,J,K)=VS(IMAX+I,J,K)
                WS(I,J,K)=WS(IMAX+I,J,K)
                TEMP(I,J,K)=TEMP(IMAX+I,J,K)
                USR(I,J,K)=TEMP(IMAX+I,J,K)
                VSR(I,J,K)=TEMP(IMAX+I,J,K)
!C                USR(I,J,K)=USR(IMAX+I,J,K)
!C                VSR(I,J,K)=VSR(IMAX+I,J,K)
                WSR(I,J,K)=TEMP(IMAX+I,J,K)
                TS(I,J,K)=TS(IMAX+I,J,K)
                XG1(I,J,K)=X(I,J,K)
                YG(I,J,K)=Y(I,J,K)
                ZG(I,J,K)=Z(I,J,K)

!CCCCCC////// LOG-LAW Boundary Condition ////////

!                  IF(J.LE.IGL(I,K)) THEN
!                      C(I,J,K)=0.600
!                      US(I,J,K)=0.0
!                  ELSE
!                      C(I,J,K)=0.0
!                      US(I,J,K)=U(I,J,K)!AA*(1.0/0.40*log(1.0*(J-42.5))+8.50)
!                  END IF

!                  CM(I,J,K)=C(I,J,K)
!                  TEMP(I,J,K)=0.0
!                  TS(I,J,K)=0.0
!                  VS(I,J,K)=0.0
!                  WS(I,J,K)=0.0
!                  USR(I,J,K)=TEMP(I,J,K)
!                  VSR(I,J,K)=TEMP(I,J,K)
!                  WSR(I,J,K)=TEMP(I,J,K)

!                  XG1(I,J,K)=X(I,J,K)
!                  YG(I,J,K)=Y(I,J,K)
!                  ZG(I,J,K)=Z(I,J,K)

 80     CONTINUE

!C------------- Right-Side Boundary ----------

            DO 90 I=IMAX+3,IMAX+4
            DO 90 K=1,KMAX+4
            DO 90 J=1,JMAX+3

!CCCCCC////// Periodic Boundary Condition ////////

                C(I,J,K)=C(I-IMAX,J,K)
                CM(I,J,K)=CM(I-IMAX,J,K)
                US(I,J,K)=US(I-IMAX,J,K)
                VS(I,J,K)=VS(I-IMAX,J,K)
                WS(I,J,K)=WS(I-IMAX,J,K)
                TEMP(I,J,K)=TEMP(I-IMAX,J,K)
                USR(I,J,K)=TEMP(I-IMAX,J,K)
                VSR(I,J,K)=TEMP(I-IMAX,J,K)
                WSR(I,J,K)=TEMP(I-IMAX,J,K)
!C                USR(I,J,K)=USR(I-IMAX,J,K)
!C                VSR(I,J,K)=VSR(I-IMAX,J,K)
                TS(I,J,K)=TS(I-IMAX,J,K)
                XG1(I,J,K)=X(I,J,K)
                YG(I,J,K)=Y(I,J,K)
                ZG(I,J,K)=Z(I,J,K)

!CCCCCC////// ZERO Gradient Boundary Condition ////////

                !C(I,J,K)=C(IMAX+2,J,K)
                !CM(I,J,K)=CM(IMAX+2,J,K)
                !US(I,J,K)=US(IMAX+2,J,K)
                !VS(I,J,K)=VS(IMAX+2,J,K)
                !WS(I,J,K)=WS(IMAX+2,J,K)
                !USR(I,J,K)=USR(IMAX+2,J,K)
                !VSR(I,J,K)=VSR(IMAX+2,J,K)
                !WSR(I,J,K)=WSR(IMAX+2,J,K)
                !TEMP(I,J,K)=TEMP(IMAX+2,J,K)
                !TS(I,J,K)=TS(IMAX+2,J,K)

                !XG1(I,J,K)=X(I,J,K)
                !YG(I,J,K)=Y(I,J,K)
                !ZG(I,J,K)=Z(I,J,K)

 90     CONTINUE

!!C********** Spanwise Direction **********C
!C********** TEMAE Boundary **********C

            DO 55 K=1,2
            DO 55 I=1,IMAX+4
            DO 55 J=1,JMAX+3

!CCCCCC////// Wall Condition ///////

                !C(I,J,K)=C(I,J,3)
                !CM(I,J,K)=CM(I,J,3)

!C********** SLIP **********C

                !US(I,J,K)=US(I,J,3)
                !VS(I,J,K)=VS(I,J,3)
                !WS(I,J,K)=0.0

!C********** NO-SLIP (WALL POSITION K=2.5) **********C

!C                  US(I,J,2)=-US(I,J,3)
!C                  US(I,J,1)=-US(I,J,4)
!C                  VS(I,J,K)=VS(I,J,3)
!C                  WS(I,J,2)=-WS(I,J,3)
!C                  WS(I,J,1)=-WS(I,J,4)

                !USR(I,J,K)=USR(I,J,3)
                !VSR(I,J,K)=VSR(I,J,3)
                !WSR(I,J,K)=WSR(I,J,3)
                !TEMP(I,J,K)=TEMP(I,J,3)
                !TS(I,J,K)=TS(I,J,3)

                !XG1(I,J,K)=X(I,J,K)
                !YG(I,J,K)=Y(I,J,K)
                !ZG(I,J,K)=Z(I,J,K)

!CCCCCC////// Periodic Boundary Condition ////////

                C(I,J,K)=C(I,J,KMAX+K)
                CM(I,J,K)=CM(I,J,KMAX+K)
                US(I,J,K)=US(I,J,KMAX+K)
                VS(I,J,K)=VS(I,J,KMAX+K)
                WS(I,J,K)=WS(I,J,KMAX+K)
                TEMP(I,J,K)=TEMP(I,J,KMAX+K)
                USR(I,J,K)=USR(I,J,KMAX+K)
                VSR(I,J,K)=VSR(I,J,KMAX+K)
                WSR(I,J,K)=WSR(I,J,KMAX+K)
                TS(I,J,K)=TS(I,J,KMAX+K)
                XG1(I,J,K)=X(I,J,K)
                YG(I,J,K)=Y(I,J,K)
                ZG(I,J,K)=Z(I,J,K)

 55     CONTINUE

!C********** OKU Boundary **********C

            DO 65 K=KMAX+3,KMAX+4
            DO 65 I=1,IMAX+4
            DO 65 J=1,JMAX+3

!CCCCCC////// Wall Condition ///////

                !C(I,J,K)=C(I,J,KMAX+2)
                !CM(I,J,K)=CM(I,J,KMAX+2)

!C********** SLIP **********C

                !US(I,J,K)=US(I,J,KMAX+2)
                !VS(I,J,K)=VS(I,J,KMAX+2)
                !WS(I,J,K)=0.0

!C********** NO-SLIP (WALL POSITION K=KMAX+2.5) **********C

!C                  US(I,J,KMAX+2)=-US(I,J,KMAX+3)
!C                  US(I,J,KMAX+1)=-US(I,J,KMAX+4)
!C                  VS(I,J,K)=VS(I,J,KMAX+3)
!C                  WS(I,J,KMAX+2)=-WS(I,J,KMAX+3)
!C                  WS(I,J,KMAX+1)=-WS(I,J,KMAX+4)

                !USR(I,J,K)=USR(I,J,KMAX+2)
                !VSR(I,J,K)=VSR(I,J,KMAX+2)
                !WSR(I,J,K)=WSR(I,J,KMAX+2)
                !TEMP(I,J,K)=TEMP(I,J,KMAX+2)
                !TS(I,J,K)=TS(I,J,KMAX+2)

                !XG1(I,J,K)=X(I,J,K)
                !YG(I,J,K)=Y(I,J,K)
                !ZG(I,J,K)=Z(I,J,K)

!CCCCCC////// Periodic Boundary Condition ////////

                C(I,J,K)=C(I,J,K-KMAX)
                CM(I,J,K)=CM(I,J,K-KMAX)
                US(I,J,K)=US(I,J,K-KMAX)
                VS(I,J,K)=VS(I,J,K-KMAX)
                WS(I,J,K)=WS(I,J,K-KMAX)
                TEMP(I,J,K)=TEMP(I,J,K-KMAX)
                USR(I,J,K)=USR(I,J,K-KMAX)
                VSR(I,J,K)=VSR(I,J,K-KMAX)
                WSR(I,J,K)=WSR(I,J,K-KMAX)
                TS(I,J,K)=TS(I,J,K-KMAX)

                XG1(I,J,K)=X(I,J,K)
                YG(I,J,K)=Y(I,J,K)
                ZG(I,J,K)=Z(I,J,K)

 65   CONTINUE

!CCCCCC////// Pier Boundary Condition ////////CCCCCC

        IF(pier.EQ.1.0) THEN

            DO 924 J=1,JMAX+3
            DO 924 K=Kpier_R, Kpier_L
            DO 924 I=Ipier_U, Ipier_D

                C(I,J,K)=CSMAX
                IF(J.GE.IGL(I,K)+1) C(I,J,K)=0.0
                TEMP(I,J,K)=0.0
                TS(I,J,K)=0.0
                US(I,J,K)=0.0
                VS(I,J,K)=0.0
                WS(I,J,K)=0.0

                C(Ipier_D,J,K)=C(Ipier_D+1,J,K)
                C(I,J,Kpier_R)=C(I,J,Kpier_R-1)
                C(I,J,Kpier_L)=C(I,J,Kpier_L+1)               
                C(Ipier_U,J,K)=C(Ipier_U-1,J,K)

                US(Ipier_U,J,K)=-US(Ipier_U-1,J,K)
                US(Ipier_U+1,J,K)=-US(Ipier_U-2,J,K)
                US(Ipier_D,J,K)=-US(Ipier_D+1,J,K)
                US(Ipier_D-1,J,K)=-US(Ipier_D+2,J,K)

                WS(I,J,Kpier_R)=-WS(I,J,Kpier_R-1)
                WS(I,J,Kpier_R+1)=-WS(I,J,Kpier_R-2)
                WS(I,J,Kpier_L)=-WS(I,J,Kpier_L+1)
                WS(I,J,Kpier_L-1)=-WS(I,J,Kpier_L+2)

                USR(I,J,K)=TEMP(I,J,K)
                VSR(I,J,K)=TEMP(I,J,K)
                WSR(I,J,K)=TEMP(I,J,K)
                CM(I,J,K)=C(I,J,K)
                XG1(I,J,K)=X(I,J,K)
                YG(I,J,K)=Y(I,J,K)
                ZG(I,J,K)=Z(I,J,K)

 924    CONTINUE

        !END IF
        !IF(pier.EQ.1.0) THEN

            DO 925 J=1,JMAX+3
            DO 925 K=Kpier_R1,Kpier_L1
            DO 925 I=Ipier_U1,Ipier_D1

                C(I,J,K)=CSMAX
                IF(J.GE.IGL(I,K)+1) C(I,J,K)=0.0
                TEMP(I,J,K)=0.0
                TS(I,J,K)=0.0
                US(I,J,K)=0.0
                VS(I,J,K)=0.0
                WS(I,J,K)=0.0

                C(Ipier_D1,J,K)=C(Ipier_D1+1,J,K)
                C(I,J,Kpier_R1)=C(I,J,Kpier_R1-1)
                C(I,J,Kpier_L1)=C(I,J,Kpier_L1+1)               
                C(Ipier_U1,J,K)=C(Ipier_U1-1,J,K)

                US(Ipier_U1,J,K)=-US(Ipier_U1-1,J,K)
                US(Ipier_U1+1,J,K)=-US(Ipier_U1-2,J,K)
                US(Ipier_D1,J,K)=-US(Ipier_D1+1,J,K)
                US(Ipier_D1-1,J,K)=-US(Ipier_D1+2,J,K)

                WS(I,J,Kpier_R1)=-WS(I,J,Kpier_R1-1)
                WS(I,J,Kpier_R1+1)=-WS(I,J,Kpier_R1-2)
                WS(I,J,Kpier_L1)=-WS(I,J,Kpier_L1+1)
                WS(I,J,Kpier_L1-1)=-WS(I,J,Kpier_L1+2)

                USR(I,J,K)=TEMP(I,J,K)
                VSR(I,J,K)=TEMP(I,J,K)
                WSR(I,J,K)=TEMP(I,J,K)
                CM(I,J,K)=C(I,J,K)
                XG1(I,J,K)=X(I,J,K)
                YG(I,J,K)=Y(I,J,K)
                ZG(I,J,K)=Z(I,J,K)

 925  CONTINUE

        !END IF
        !IF(pier.EQ.1.0) THEN

            DO 926 J=1,JMAX+3
            DO 926 K=Kpier_R2,Kpier_L2
            DO 926 I=Ipier_U2,Ipier_D2

                C(I,J,K)=CSMAX
                IF(J.GE.IGL(I,K)+1) C(I,J,K)=0.0
                TEMP(I,J,K)=0.0
                TS(I,J,K)=0.0
                US(I,J,K)=0.0
                VS(I,J,K)=0.0
                WS(I,J,K)=0.0

                C(Ipier_D2,J,K)=C(Ipier_D2+1,J,K)
                C(I,J,Kpier_R2)=C(I,J,Kpier_R2-1)
                C(I,J,Kpier_L2)=C(I,J,Kpier_L2+1)               
                C(Ipier_U2,J,K)=C(Ipier_U2-1,J,K)

                US(Ipier_U2,J,K)=-US(Ipier_U2-1,J,K)
                US(Ipier_U2+1,J,K)=-US(Ipier_U2-2,J,K)
                US(Ipier_D2,J,K)=-US(Ipier_D2+1,J,K)
                US(Ipier_D2-1,J,K)=-US(Ipier_D2+2,J,K)

                WS(I,J,Kpier_R2)=-WS(I,J,Kpier_R2-1)
                WS(I,J,Kpier_R2+1)=-WS(I,J,Kpier_R2-2)
                WS(I,J,Kpier_L2)=-WS(I,J,Kpier_L2+1)
                WS(I,J,Kpier_L2-1)=-WS(I,J,Kpier_L2+2)

                USR(I,J,K)=TEMP(I,J,K)
                VSR(I,J,K)=TEMP(I,J,K)
                WSR(I,J,K)=TEMP(I,J,K)
                CM(I,J,K)=C(I,J,K)
                XG1(I,J,K)=X(I,J,K)
                YG(I,J,K)=Y(I,J,K)
                ZG(I,J,K)=Z(I,J,K)

 926    CONTINUE

        !END IF
        !IF(pier.EQ.1.0) THEN

            DO 927 J=1,JMAX+3
            DO 927 K=Kpier_R3,Kpier_L3
            DO 927 I=Ipier_U3,Ipier_D3

                C(I,J,K)=CSMAX
                IF(J.GE.IGL(I,K)+1) C(I,J,K)=0.0
                TEMP(I,J,K)=0.0
                TS(I,J,K)=0.0
                US(I,J,K)=0.0
                VS(I,J,K)=0.0
                WS(I,J,K)=0.0

                C(Ipier_D3,J,K)=C(Ipier_D3+1,J,K)
                C(I,J,Kpier_R3)=C(I,J,Kpier_R3-1)
                C(I,J,Kpier_L3)=C(I,J,Kpier_L3+1)               
                C(Ipier_U3,J,K)=C(Ipier_U3-1,J,K)

                US(Ipier_U3,J,K)=-US(Ipier_U3-1,J,K)
                US(Ipier_U3+1,J,K)=-US(Ipier_U3-2,J,K)
                US(Ipier_D3,J,K)=-US(Ipier_D3+1,J,K)
                US(Ipier_D3-1,J,K)=-US(Ipier_D3+2,J,K)

                WS(I,J,Kpier_R3)=-WS(I,J,Kpier_R3-1)
                WS(I,J,Kpier_R3+1)=-WS(I,J,Kpier_R3-2)
                WS(I,J,Kpier_L3)=-WS(I,J,Kpier_L3+1)
                WS(I,J,Kpier_L3-1)=-WS(I,J,Kpier_L3+2)

                USR(I,J,K)=TEMP(I,J,K)
                VSR(I,J,K)=TEMP(I,J,K)
                WSR(I,J,K)=TEMP(I,J,K)
                CM(I,J,K)=C(I,J,K)
                XG1(I,J,K)=X(I,J,K)
                YG(I,J,K)=Y(I,J,K)
                ZG(I,J,K)=Z(I,J,K)

 927    CONTINUE

        !END IF
        !IF(pier.EQ.1.0) THEN

            DO 928 J=1,JMAX+3
            DO 928 K=Kpier_R4,Kpier_L4
            DO 928 I=Ipier_U4,Ipier_D4

                C(I,J,K)=CSMAX
                IF(J.GE.IGL(I,K)+1) C(I,J,K)=0.0
                TEMP(I,J,K)=0.0
                TS(I,J,K)=0.0
                US(I,J,K)=0.0
                VS(I,J,K)=0.0
                WS(I,J,K)=0.0

                C(Ipier_D4,J,K)=C(Ipier_D4+1,J,K)
                C(I,J,Kpier_R4)=C(I,J,Kpier_R4-1)
                C(I,J,Kpier_L4)=C(I,J,Kpier_L4+1)               
                C(Ipier_U4,J,K)=C(Ipier_U4-1,J,K)

                US(Ipier_U4,J,K)=-US(Ipier_U4-1,J,K)
                US(Ipier_U4+1,J,K)=-US(Ipier_U4-2,J,K)
                US(Ipier_D4,J,K)=-US(Ipier_D4+1,J,K)
                US(Ipier_D4-1,J,K)=-US(Ipier_D4+2,J,K)

                WS(I,J,Kpier_R4)=-WS(I,J,Kpier_R4-1)
                WS(I,J,Kpier_R4+1)=-WS(I,J,Kpier_R4-2)
                WS(I,J,Kpier_L4)=-WS(I,J,Kpier_L4+1)
                WS(I,J,Kpier_L4-1)=-WS(I,J,Kpier_L4+2)

                USR(I,J,K)=TEMP(I,J,K)
                VSR(I,J,K)=TEMP(I,J,K)
                WSR(I,J,K)=TEMP(I,J,K)
                CM(I,J,K)=C(I,J,K)
                XG1(I,J,K)=X(I,J,K)
                YG(I,J,K)=Y(I,J,K)
                ZG(I,J,K)=Z(I,J,K)

 928    CONTINUE

        END IF

!CCCCCC////// Surface Boundary Condition ////////CCCCCC

!$OMP PARALLEL
!$OMP DO
        DO 589 I=3,IMAX+2
        DO 589 K=3,KMAX+2
        DO 589 J=IETA(I,K),JMAX+3

            IF(DEPMODE.EQ.1.0) THEN
                C(I,J,K)=0.0    !C(I,JMAX+1,K)
                US(I,J,K)=US(I,IETA(I,K)-1,K)
                VS(I,J,K)=0.0   !VS(I,JMAX+1,K)
                WS(I,J,K)=WS(I,IETA(I,K)-1,K)
                TEMP(I,J,K)=TEMP(I,IETA(I,K)-1,K)
                TS(I,J,K)=TS(I,IETA(I,K)-1,K)  !added by SUMO23/06/11
                USR(I,J,K)=USR(I,IETA(I,K)-1,K)
                VSR(I,J,K)=VSR(I,IETA(I,K)-1,K)
                WSR(I,J,K)=WSR(I,IETA(I,K)-1,K)
                CM(I,J,K)=C(I,J,K)

                XG1(I,J,K)=X(I,J,K)
                YG(I,J,K)=Y(I,J,K)
                ZG(I,J,K)=Z(I,J,K)
            END IF

 589  CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCC           REALLOCATION  ROUTINE            CCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!C////  Average-Position & Dispersion of Particle ///////

!$OMP PARALLEL
!$OMP DO
        DO 200 K=1,KMAX+4
        DO 200 I=1,IMAX+4
        DO 200 J=1,JMAX+3

            XM(I,J,K)=X(I,J,K)+US(I,J,K)*DT1
            YM(I,J,K)=Y(I,J,K)+VS(I,J,K)*DT1
            ZM(I,J,K)=Z(I,J,K)+WS(I,J,K)*DT1

!CCCCC///// IF USE PERIODIC BC FOR X or Z DIRECTION /////CCCCC

            XXM(I,J,K)=2.0*USR(I,J,K)*TS(I,J,K)*DT1
            ZZM(I,J,K)=2.0*WSR(I,J,K)*TS(I,J,K)*DT1

!CCCCC///////////////////////////////////////////////////CCCCC

            XG1(I,J,K)=X(I,J,K)
            YG(I,J,K)=Y(I,J,K)
            ZG(I,J,K)=Z(I,J,K)

 200    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!!$OMP PARALLEL
!!$OMP DO
        DO 205 K=1,KMAX+4
        DO 205 I=1,IMAX+4
        DO 205 J=2,JMAX+2

            YYM(I,J,K)=2.00*0.25*(VSR(I,J,K)+VSR(I,J+1,K)) &
                                *(TS(I,J,K)+TS(I,J+1,K))*DT1
            YYM2(I,J,K)=2.0*0.25*(VSR(I,J,K)+VSR(I,J-1,K)) &
                                *(TS(I,J,K)+TS(I,J-1,K))*DT1

            YYM(I,1,K)=YYM(I,2,K)
            YYM(I,JMAX+3,K)=YYM(I,JMAX+2,K)
            YYM2(I,1,K)=YYM2(I,2,K)
            YYM2(I,JMAX+3,K)=YYM2(I,JMAX+2,K)

 205    CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL

!!!$OMP PARALLEL
!!!$OMP DO
        !DO 206 K=1,KMAX+4
        !DO 206 I=2,IMAX+3
        !DO 206 J=1,JMAX+3

            !XXM(I,J,K)=2.00*0.25*(USR(I,J,K)+USR(I+1,J,K)) &
            !                    *(TS(I,J,K)+TS(I+1,J,K))*DT1
            !XXM2(I,J,K)=2.0*0.25*(USR(I,J,K)+USR(I-1,J,K)) &
            !                    *(TS(I,J,K)+TS(I-1,J,K))*DT1

            !XXM(1,J,K)=XXM(2,J,K)
            !XXM(IMAX+4,J,K)=XXM(IMAX+3,J,K)
            !XXM2(1,J,K)=XXM2(2,J,K)
            !XXM2(IMAX+4,J,K)=XXM2(IMAX+3,J,K)

 206    CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL

!!$OMP PARALLEL
!!$OMP DO
        !DO 207 K=2,KMAX+3
        !DO 207 I=1,IMAX+4
        !DO 207 J=1,JMAX+3

            !ZZM(I,J,K)=2.00*0.25*(WSR(I,J,K)+WSR(I,J,K+1)) &
            !                    *(TS(I,J,K)+TS(I,J,K+1))*DT1
            !ZZM2(I,J,K)=2.00*0.25*(WSR(I,J,K)+WSR(I,J,K-1)) &
            !                    *(TS(I,J,K)+TS(I,J,K-1))*DT1

            !ZZM(I,J,1)=ZZM(I,J,2)
            !ZZM(I,J,KMAX+4)=ZZM(I,J,KMAX+3)
            !ZZM2(I,J,1)=ZZM2(I,J,2)
            !ZZM2(I,J,KMAX+4)=ZZM2(I,J,KMAX+3)

 207    CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL

!C/////  Probability of particle position in the x-direction  //////
!C/////    fx(I,J,K,II,JJ,KK) & XF(I,J,K,II,JJ,KK)            //////

!$OMP PARALLEL
!$OMP DO
        DO 210 K=1,KMAX+4
        DO 210 I=1,IMAX+4
        DO 210 J=1,JMAX+3

            DX1(I,J,K)=X(I,J,K)+0.50*GHX(I)-XG1(I,J,K)
            DX2(I,J,K)=XG1(I,J,K)-(X(I,J,K)-0.50*GHX(I))

 210    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 220 K=1,KMAX+4
        DO 220 I=1,IMAX+4
        DO 220 J=1,JMAX+3

            IF (DX1(I,J,K).GT.DX2(I,J,K)) THEN
                DX1(I,J,K)=DX2(I,J,K)
            ELSE
                DX2(I,J,K)=DX1(I,J,K)
            END IF

 220    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 230 K=1,KMAX+4
        DO 230 I=1,IMAX+4
        DO 230 J=1,JMAX+3

            !USQ(I,J,K)=SQRT(ABS(3.0*XXM(I,J,K)/DX1(I,J,K)**2+1.0))*DX1(I,J,K)
            !USQ2(I,J,K)=SQRT(ABS(3.0*XXM2(I,J,K)/DX1(I,J,K)**2+1.0))*DX1(I,J,K)

            USQ(I,J,K)=SQRT(ABS(3.0*XXM(I,J,K)/DX1(I,J,K)**2+1.0))*DX1(I,J,K)

 230    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 240 KK=1,3
        DO 240 JJ=1,3
        DO 240 II=1,3
        DO 240 K=1,KMAX+4
        DO 240 I=1,IMAX+4
        DO 240 J=1,JMAX+3

            fx(I,J,K,II,JJ,KK)=0.0
            XF(I,J,K,II,JJ,KK)=0.0

 240    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 250 KK=-1,1
        DO 250 JJ=-1,1
        DO 250 K=2,KMAX+3
        DO 250 I=2,IMAX+3
        DO 250 J=2,JMAX+1

            !fx(I,J,K,1,JJ+2,KK+2)=(XM(I-1,J+JJ,K+KK)+USQ(I-1,J+JJ,K+KK)-(X(I,J,K)-0.50*GHX(I))) &
            !                     /(USQ(I-1,J+JJ,K+KK)+USQ2(I-1,J+JJ,K+KK))
            !XF(I,J,K,1,JJ+2,KK+2)=0.50*(XM(I-1,J+JJ,K+KK)+USQ(I-1,J+JJ,K+KK)+(X(I,J,K)-0.50*GHX(I)))

            !fx(I,J,K,3,JJ+2,KK+2)=(X(I,J,K)+0.50*GHX(I)-(XM(I+1,J+JJ,K+KK)-USQ2(I+1,J+JJ,K+KK))) &  !23/06/21SUMO   !USQ --> USQ2
            !                     /(USQ(I+1,J+JJ,K+KK)+USQ2(I+1,J+JJ,K+KK))
            !XF(I,J,K,3,JJ+2,KK+2)=0.50*(X(I,J,K)+0.50*GHX(I)+XM(I+1,J+JJ,K+KK)-USQ2(I+1,J+JJ,K+KK))

            fx(I,J,K,1,JJ+2,KK+2)=(XM(I-1,J+JJ,K+KK)+USQ(I-1,J+JJ,K+KK)-(X(I,J,K)-0.50*GHX(I))) &
                                 /(2.0*USQ(I-1,J+JJ,K+KK))
            XF(I,J,K,1,JJ+2,KK+2)=0.50*(XM(I-1,J+JJ,K+KK)+USQ(I-1,J+JJ,K+KK)+(X(I,J,K)-0.50*GHX(I)))

            fx(I,J,K,3,JJ+2,KK+2)=(X(I,J,K)+0.50*GHX(I)-(XM(I+1,J+JJ,K+KK)-USQ(I+1,J+JJ,K+KK))) &
                                 /(2.0*USQ(I+1,J+JJ,K+KK))
            XF(I,J,K,3,JJ+2,KK+2)=0.50*(X(I,J,K)+0.50*GHX(I)+XM(I+1,J+JJ,K+KK)-USQ(I+1,J+JJ,K+KK))

 250    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 260 KK=-1,1
        DO 260 JJ=-1,1
        DO 260 K=2,KMAX+3
        DO 260 I=2,IMAX+3
        DO 260 J=2,JMAX+1

            IF(fx(I,J,K,1,JJ+2,KK+2).LT.0.0) THEN
            !IF(fx(I,J,K,1,JJ+2,KK+2).LE.0.0) THEN
                fx(I,J,K,1,JJ+2,KK+2)=0.0
                XF(I,J,K,1,JJ+2,KK+2)=0.0
                !XF(I,J,K,1,JJ+2,KK+2)=X(I-1,J+JJ,K+KK)
            END IF

            IF(fx(I,J,K,1,JJ+2,KK+2).GT.1.0) THEN
                fx(I,J,K,1,JJ+2,KK+2)=1.0
                XF(I,J,K,1,JJ+2,KK+2)=XM(I-1,J+JJ,K+KK)
            END IF

            IF(fx(I,J,K,3,JJ+2,KK+2).LT.0.0) THEN
            !IF(fx(I,J,K,3,JJ+2,KK+2).LE.0.0) THEN
                fx(I,J,K,3,JJ+2,KK+2)=0.0
                XF(I,J,K,3,JJ+2,KK+2)=0.0
                !XF(I,J,K,3,JJ+2,KK+2)=X(I+1,J+JJ,K+KK)
            END IF

            IF(fx(I,J,K,3,JJ+2,KK+2).GT.1.0) THEN 
                fx(I,J,K,3,JJ+2,KK+2)=1.0
                XF(I,J,K,3,JJ+2,KK+2)=XM(I+1,J+JJ,K+KK)
            END IF

 260    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 270 KK=-1,1
        DO 270 JJ=-1,1
        DO 270 K=2,KMAX+3
        DO 270 I=2,IMAX+3   !I=3,IMAX+2
        DO 270 J=2,JMAX+1   !J=3,JMAX+1

            !IF((XM(I,J+JJ,K+KK)-USQ2(I,J+JJ,K+KK)).LT.(X(I,J,K)-0.50*GHX(I))) THEN
            !    IF((XM(I,J+JJ,K+KK)+USQ(I,J+JJ,K+KK)).LT.(X(I,J,K)-0.50*GHX(I))) THEN
            !        fx(I,J,K,2,JJ+2,KK+2)=0.0
            !        XF(I,J,K,2,JJ+2,KK+2)=0.0
            !    ELSE
            !        IF((XM(I,J+JJ,K+KK)+USQ(I,J+JJ,K+KK)).LT.(X(I,J,K)+0.50*GHX(I))) THEN
            !            fx(I,J,K,2,JJ+2,KK+2)=(XM(I,J+JJ,K+KK)+USQ(I,J+JJ,K+KK)-(X(I,J,K)-0.50*GHX(I))) &
            !                                 /(USQ(I,J+JJ,K+KK)+USQ2(I,J+JJ,K+KK))
            !            XF(I,J,K,2,JJ+2,KK+2)=0.50*(XM(I,J+JJ,K+KK)+USQ(I,J+JJ,K+KK)+(X(I,J,K)-0.50*GHX(I)))
            !        ELSE
            !            fx(I,J,K,2,JJ+2,KK+2)=GHX(I)/(USQ(I,J+JJ,K+KK)+USQ2(I,J+JJ,K+KK))   !SUMO23/06/21
            !            !fx(I,J,K,2,JJ+2,KK+2)=0.50*(X(I+1,J,K)-X(I-1,J,K))/(USQ(I,J+JJ,K+KK)+USQ2(I,J+JJ,K+KK))    !SUMO23/06/12
            !            XF(I,J,K,2,JJ+2,KK+2)=X(I,J,K)  !SUMO23/06/21
            !            !XF(I,J,K,2,JJ+2,KK+2)=0.50*(X(I+1,J,K)+X(I-1,J,K))
            !            END IF
            !        END IF
            !    ELSE
            !    IF((XM(I,J+JJ,K+KK)-USQ2(I,J+JJ,K+KK)).LT.(X(I,J,K)+0.50*GHX(I))) THEN  !IF((XM(I,J+JJ,K+KK)-USQ(I,J+JJ,K+KK)).LT.
            !        IF((XM(I,J+JJ,K+KK)+USQ(I,J+JJ,K+KK)).LT.(X(I,J,K)+0.50*GHX(I))) THEN
            !                fx(I,J,K,2,JJ+2,KK+2)=1.0
            !                XF(I,J,K,2,JJ+2,KK+2)=XM(I,J+JJ,K+KK)
            !        ELSE
            !                fx(I,J,K,2,JJ+2,KK+2)=((X(I,J,K)+0.50*GHX(I))-(XM(I,J+JJ,K+KK)-USQ2(I,J+JJ,K+KK))) &    !-(XM(I,J+JJ,K+KK)-USQ(I,J+JJ,K+KK)))
            !                                        /(USQ(I,J+JJ,K+KK)+USQ2(I,J+JJ,K+KK))   !/(2.0*USQ(I,J+JJ,K+KK))
            !                XF(I,J,K,2,JJ+2,KK+2)=0.50*((X(I,J,K)+0.50*GHX(I))+XM(I,J+JJ,K+KK)-USQ2(I,J+JJ,K+KK))   !+XM(I,J+JJ,K+KK)-USQ(I,J+JJ,K+KK))
            !        END IF 
            !    ELSE
            !            fx(I,J,K,2,JJ+2,KK+2)=0.0
            !            XF(I,J,K,2,JJ+2,KK+2)=0.0
            !    END IF
            !END IF

            IF((XM(I,J+JJ,K+KK)-USQ(I,J+JJ,K+KK)).LT.(X(I,J,K)-0.50*GHX(I))) THEN
                IF((XM(I,J+JJ,K+KK)+USQ(I,J+JJ,K+KK)).LT.(X(I,J,K)-0.50*GHX(I))) THEN
                    fx(I,J,K,2,JJ+2,KK+2)=0.0
                    XF(I,J,K,2,JJ+2,KK+2)=0.0
                ELSE
                    IF((XM(I,J+JJ,K+KK)+USQ(I,J+JJ,K+KK)).LT.(X(I,J,K)+0.50*GHX(I))) THEN
                        fx(I,J,K,2,JJ+2,KK+2)=(XM(I,J+JJ,K+KK)+USQ(I,J+JJ,K+KK)-(X(I,J,K)-0.50*GHX(I))) &
                                             /(2.0*USQ(I,J+JJ,K+KK))
                        XF(I,J,K,2,JJ+2,KK+2)=0.50*(XM(I,J+JJ,K+KK)+USQ(I,J+JJ,K+KK)+(X(I,J,K)-0.50*GHX(I)))
                    ELSE
                        fx(I,J,K,2,JJ+2,KK+2)=GHX(I)/(2.0*USQ(I,J+JJ,K+KK))
                        XF(I,J,K,2,JJ+2,KK+2)=X(I,J,K)
                    END IF
                END IF
            ELSE
                IF((XM(I,J+JJ,K+KK)-USQ(I,J+JJ,K+KK)).LT.(X(I,J,K)+0.50*GHX(I))) THEN
                    IF((XM(I,J+JJ,K+KK)+USQ(I,J+JJ,K+KK)).LT.(X(I,J,K)+0.50*GHX(I))) THEN
                        fx(I,J,K,2,JJ+2,KK+2)=1.0
                        XF(I,J,K,2,JJ+2,KK+2)=XM(I,J+JJ,K+KK)
                    ELSE
                        fx(I,J,K,2,JJ+2,KK+2)=((X(I,J,K)+0.50*GHX(I))-(XM(I,J+JJ,K+KK)-USQ(I,J+JJ,K+KK))) &
                                             /(2.0*USQ(I,J+JJ,K+KK))
                        XF(I,J,K,2,JJ+2,KK+2)=0.50*((X(I,J,K)+0.50*GHX(I))+XM(I,J+JJ,K+KK)-USQ(I,J+JJ,K+KK))
                    END IF 
                ELSE
                    fx(I,J,K,2,JJ+2,KK+2)=0.0
                    XF(I,J,K,2,JJ+2,KK+2)=0.0
                END IF
            END IF

 270    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!C/////  Probability of particle position in the y-direction  //////
!C/////     fy(I,J,K,II,JJ,KK) & YF(I,J,K,II,JJ,KK)           //////

    !$OMP PARALLEL
    !$OMP DO
!
            DO 310 K=1,KMAX+4
            DO 310 I=1,IMAX+4
            DO 310 J=2,JMAX+2
!
                DY1(I,J,K)=Y(I,J,K)+0.50/EY(J)-YG(I,J,K)
                DY2(I,J,K)=YG(I,J,K)-(Y(I,J,K)-0.50/EY(J))
!
 310  CONTINUE
!
    !$OMP END DO
    !$OMP END PARALLEL
!
    !$OMP PARALLEL
    !$OMP DO
!
            DO 320 K=1,KMAX+4
            DO 320 I=1,IMAX+4
            DO 320 J=2,JMAX+2
!
                IF (DY1(I,J,K).GT.DY2(I,J,K)) THEN
                    DY1(I,J,K)=DY2(I,J,K)
                ELSE
                    DY2(I,J,K)=DY1(I,J,K)
                END IF
!
 320  CONTINUE
!
    !$OMP END DO
    !$OMP END PARALLEL
!
    !$OMP PARALLEL
    !$OMP DO
!
            DO 330 K=1,KMAX+4
            DO 330 I=1,IMAX+4
            DO 330 J=2,JMAX+2
!
                VSQ(I,J,K)=SQRT(ABS(3.0*YYM(I,J,K)/DY1(I,J,K)**2+1.0))*DY1(I,J,K)
                VSQ2(I,J,K)=SQRT(ABS(3.0*YYM2(I,J,K)/DY1(I,J,K)**2+1.0))*DY1(I,J,K)
!
 330  CONTINUE
!
    !$OMP END DO
    !$OMP END PARALLEL
!
    !$OMP PARALLEL
    !$OMP DO
!
            DO 340 KK=1,3
            DO 340 II=1,3
            DO 340 JJ=1,3
            DO 340 K=1,KMAX+4
            DO 340 I=1,IMAX+4
            DO 340 J=1,JMAX+3
!
                fy(I,J,K,II,JJ,KK)=0.0
                YF(I,J,K,II,JJ,KK)=0.0
!
 340  CONTINUE
!
    !$OMP END DO
    !$OMP END PARALLEL
!
    !$OMP PARALLEL
    !$OMP DO
!
            DO 350 KK=-1,1
            DO 350 II=-1,1
            DO 350 K=2,KMAX+3
            DO 350 I=2,IMAX+3
            DO 350 J=2,JMAX+1
!
                fy(I,J,K,II+2,1,KK+2)=(YM(I+II,J-1,K+KK)+VSQ(I+II,J-1,K+KK)-0.50*(Y(I,J,K)+Y(I,J-1,K))) &
                                        /(VSQ(I+II,J-1,K+KK)+VSQ2(I+II,J-1,K+KK))
                YF(I,J,K,II+2,1,KK+2)=0.50*(YM(I+II,J-1,K+KK)+VSQ(I+II,J-1,K+KK)+0.50*(Y(I,J,K)+Y(I,J-1,K)))
!C
                fy(I,J,K,II+2,3,KK+2)=(0.50*(Y(I,J,K)+Y(I,J+1,K))-(YM(I+II,J+1,K+KK)-VSQ2(I+II,J+1,K+KK))) &
                                        /(VSQ(I+II,J+1,K+KK)+VSQ2(I+II,J+1,K+KK))
                YF(I,J,K,II+2,3,KK+2)=0.50*(0.50*(Y(I,J,K)+Y(I,J+1,K))+YM(I+II,J+1,K+KK)-VSQ2(I+II,J+1,K+KK))
!C
!C          fy(I,J,K,II+2,1,KK+2)=(YM(I+II,J-1,K+KK)+VSQ(I+II,J-1,K+KK)
!C     &                      -0.50*(Y(I,J,K)+Y(I,J-1,K)))
!C     &                     /(2.0*VSQ(I+II,J-1,K+KK))
!C          YF(I,J,K,II+2,1,KK+2)=
!C     &             0.50*(YM(I+II,J-1,K+KK)+VSQ(I+II,J-1,K+KK)
!C     &                   +0.50*(Y(I,J,K)+Y(I,J-1,K)) )
!C
!C          fy(I,J,K,II+2,3,KK+2)=(0.50*(Y(I,J,K)+Y(I,J+1,K))
!C     &                     -(YM(I+II,J+1,K+KK)-VSQ(I+II,J+1,K+KK)))
!C     &                   /(2.0*VSQ(I+II,J+1,K+KK))
!C          YF(I,J,K,II+2,3,KK+2)=0.50*(0.50*(Y(I,J,K)+Y(I,J+1,K))
!C     &                        +YM(I+II,J+1,K+KK)-VSQ(I+II,J+1,K+KK))
 350  CONTINUE
!
    !$OMP END DO
    !$OMP END PARALLEL
!
    !$OMP PARALLEL
    !$OMP DO
!
            DO 360 KK=-1,1
            DO 360 II=-1,1
            DO 360 K=2,KMAX+3
            DO 360 I=2,IMAX+3
            DO 360 J=2,JMAX+1
!
                IF (fy(I,J,K,II+2,1,KK+2).LT.0.0) THEN
!                IF (fy(I,J,K,II+2,1,KK+2).LE.0.0) THEN
                    fy(I,J,K,II+2,1,KK+2)=0.0
                    YF(I,J,K,II+2,1,KK+2)=0.0
!                    YF(I,J,K,II+2,1,KK+2)=Y(I+II,J-1,K+KK)
                END IF
!C
                IF (fy(I,J,K,II+2,1,KK+2).GT.1.0) THEN
                    fy(I,J,K,II+2,1,KK+2)=1.0
                    YF(I,J,K,II+2,1,KK+2)=YM(I+II,J-1,K+KK)
                END IF
!C
                IF (fy(I,J,K,II+2,3,KK+2).LT.0.0) THEN
!                IF (fy(I,J,K,II+2,3,KK+2).LE.0.0) THEN
                    fy(I,J,K,II+2,3,KK+2)=0.0
                    YF(I,J,K,II+2,3,KK+2)=0.0
!                    YF(I,J,K,II+2,3,KK+2)=Y(I+II,J+1,K+KK)
                END IF
!C
                IF (fy(I,J,K,II+2,3,KK+2).GT.1.0) THEN
                    fy(I,J,K,II+2,3,KK+2)=1.0
                    YF(I,J,K,II+2,3,KK+2)=YM(I+II,J+1,K+KK)
                END IF
!C
 360  CONTINUE
!
    !$OMP END DO
    !$OMP END PARALLEL
!
    !$OMP PARALLEL
    !$OMP DO
!
            DO 370 KK=-1,1
            DO 370 II=-1,1
            DO 370 K=2,KMAX+3
            DO 370 I=2,IMAX+3
            DO 370 J=2,JMAX+1
!C
                IF((YM(I+II,J,K+KK)-VSQ2(I+II,J,K+KK)).LT.0.50*(Y(I,J,K)+Y(I,J-1,K))) THEN
                    IF((YM(I+II,J,K+KK)+VSQ(I+II,J,K+KK)).LT.0.50*(Y(I,J,K)+Y(I,J-1,K))) THEN
                        fy(I,J,K,II+2,2,KK+2)=0.0
                        YF(I,J,K,II+2,2,KK+2)=0.0
                    ELSE
                        IF((YM(I+II,J,K+KK)+VSQ(I+II,J,K+KK)).LT.0.50*(Y(I,J,K)+Y(I,J+1,K))) THEN
                            fy(I,J,K,II+2,2,KK+2)=(YM(I+II,J,K+KK)+VSQ(I+II,J,K+KK)-0.50*(Y(I,J,K)+Y(I,J-1,K))) &
                                                    /(VSQ(I+II,J,K+KK)+VSQ2(I+II,J,K+KK))
!
                            YF(I,J,K,II+2,2,KK+2)=0.50*(YM(I+II,J,K+KK)+VSQ(I+II,J,K+KK)+0.50*(Y(I,J,K)+Y(I,J-1,K)))
                        ELSE
!                            fy(I,J,K,II+2,2,KK+2)=0.50*(Y(I,J+1,K)-Y(I,J-1,K))/(VSQ(I+II,J,K+KK)+VSQ2(I+II,J,K+KK))
                            fy(I,J,K,II+2,2,KK+2)=1.0/EY(J)/(VSQ(I+II,J,K+KK)+VSQ2(I+II,J,K+KK))   !SUMO23/06/21
!                            YF(I,J,K,II+2,2,KK+2)=0.50*(Y(I,J+1,K)+Y(I,J-1,K))
                            YF(I,J,K,II+2,2,KK+2)=Y(I,J,K)  !SUMO23/06/21
                        END IF
                    END IF
                ELSE
                    IF((YM(I+II,J,K+KK)-VSQ2(I+II,J,K+KK)).LT.0.50*(Y(I,J,K)+Y(I,J+1,K))) THEN
                        IF((YM(I+II,J,K+KK)+VSQ(I+II,J,K+KK)).LT.0.50*(Y(I,J,K)+Y(I,J+1,K))) THEN
                            fy(I,J,K,II+2,2,KK+2)=1.0
                            YF(I,J,K,II+2,2,KK+2)=YM(I+II,J,K+KK)
                        ELSE
                            fy(I,J,K,II+2,2,KK+2)=(0.50*(Y(I,J,K)+Y(I,J+1,K))-(YM(I+II,J,K+KK)-VSQ2(I+II,J,K+KK))) &
                                                    /(VSQ(I+II,J,K+KK)+VSQ2(I+II,J,K+KK))
                            YF(I,J,K,II+2,2,KK+2)=0.50*(0.50*(Y(I,J,K)+Y(I,J+1,K))+YM(I+II,J,K+KK)-VSQ2(I+II,J,K+KK))
                        END IF 
                    ELSE
                        fy(I,J,K,II+2,2,KK+2)=0.0
                        YF(I,J,K,II+2,2,KK+2)=0.0
                    END IF
                END IF
!
!                IF(J.EQ.1.AND.2) THEN
!                    fy(I,J,K,II+2,2,KK+2)=0.50*(Y(I,J+1,K)-Y(I,J-1,K))/(VSQ(I+II,J,K+KK)+VSQ2(I+II,J,K+KK)) !/(2.0*USQ(I,J+JJ,K+KK)) !SUMO23/06/12
!                    YF(I,J,K,II+2,2,KK+2)=0.50*(Y(I,J+1,K)+Y(I,J-1,K))
!                END IF
!                IF(J.EQ.JMAX+2.AND.JMAX+3) THEN
!                    fy(I,J,K,II+2,2,KK+2)=fy(I,JMAX+1,K,II+2,2,KK+2)
!                    YF(I,J,K,II+2,2,KK+2)=YF(I,JMAX+1,K,II+2,2,KK+2)
!                END IF
!
!C        IF ((YM(I+II,J,K+KK)-VSQ(I+II,J,K+KK)).LT.
!C     &       0.50*(Y(I,J,K)+Y(I,J-1,K))) THEN
!C          IF ((YM(I+II,J,K+KK)+VSQ(I+II,J,K+KK)).LT.
!C     &         0.50*(Y(I,J,K)+Y(I,J-1,K))) THEN
!C            fy(I,J,K,II+2,2,KK+2)=0.0
!C            YF(I,J,K,II+2,2,KK+2)=0.0
!C          ELSE
!C            IF ((YM(I+II,J,K+KK)+VSQ(I+II,J,K+KK)).LT.
!C     &           0.50*(Y(I,J,K)+Y(I,J+1,K))) THEN
!C              fy(I,J,K,II+2,2,KK+2)=(YM(I+II,J,K+KK)+VSQ(I+II,J,K+KK)
!C     &                     -0.50*(Y(I,J,K)+Y(I,J-1,K)))
!C     &                    /(2.0*VSQ(I+II,J,K+KK))
!C              YF(I,J,K,II+2,2,KK+2)=
!C     &                     0.50*(YM(I+II,J,K+KK)+VSQ(I+II,J,K+KK)
!C     &                          +0.50*(Y(I,J,K)+Y(I,J-1,K)))
!C            ELSE
!C              fy(I,J,K,II+2,2,KK+2)=0.50*(Y(I,J+1,K)-Y(I,J-1,K))
!C     &                   /(2.0*VSQ(I+II,J,K+KK))
!C              YF(I,J,K,II+2,2,KK+2)=0.50*(Y(I,J+1,K)+Y(I,J-1,K))
!C            END IF
!C          END IF
!C      ELSE
!C          IF ((YM(I+II,J,K+KK)-VSQ(I+II,J,K+KK)).LT.
!C     &         0.50*(Y(I,J,K)+Y(I,J+1,K))) THEN
!C           IF ((YM(I+II,J,K+KK)+VSQ(I+II,J,K+KK)).LT.
!C     &          0.50*(Y(I,J,K)+Y(I,J+1,K))) THEN
!C                 fy(I,J,K,II+2,2,KK+2)=1.0
!C                 YF(I,J,K,II+2,2,KK+2)=YM(I+II,J,K+KK)
!C           ELSE
!C            fy(I,J,K,II+2,2,KK+2)=(0.50*(Y(I,J,K)+Y(I,J+1,K))
!C     &                    -(YM(I+II,J,K+KK)-VSQ(I+II,J,K+KK)))
!C     &               /(2.0*VSQ(I+II,J,K+KK))
!C            YF(I,J,K,II+2,2,KK+2)=0.50*(0.50*(Y(I,J,K)+Y(I,J+1,K))
!C     &                           +YM(I+II,J,K+KK)-VSQ(I+II,J,K+KK))
!C           END IF 
!C          ELSE
!C            fy(I,J,K,II+2,2,KK+2)=0.0
!C            YF(I,J,K,II+2,2,KK+2)=0.0
!C          END IF
!C      END IF
!
 370  CONTINUE
!
    !$OMP END DO
    !$OMP END PARALLEL
!
!C/////  Probability of particle position in the z-direction  //////
!C/////     fz(I,J,K,II,JJ,KK) & ZF(I,J,K,II,JJ,KK)           ///////
!
    !$OMP PARALLEL
    !$OMP DO
!
            DO 610 K=2,KMAX+3
            DO 610 I=1,IMAX+4
            DO 610 J=1,JMAX+3
!
!                DZ1(I,J,K)=0.50*(Z(I,J,K+1)+Z(I,J,K))-ZG(I,J,K)
!                DZ2(I,J,K)=ZG(I,J,K)-0.50*(Z(I,J,K)+Z(I,J,K-1))
!
                DZ1(I,J,K)=Z(I,J,K)+0.50/DZ(K)-ZG(I,J,K)
                DZ2(I,J,K)=ZG(I,J,K)-(Z(I,J,K)-0.50/DZ(K))
!
 610  CONTINUE
!
    !$OMP END DO
    !$OMP END PARALLEL
!
    !$OMP PARALLEL
    !$OMP DO
!
            DO 620 K=2,KMAX+3
            DO 620 I=1,IMAX+4
            DO 620 J=1,JMAX+3
!
                IF (DZ1(I,J,K).GT.DZ2(I,J,K)) THEN
                    DZ1(I,J,K)=DZ2(I,J,K)
                ELSE
                    DZ2(I,J,K)=DZ1(I,J,K)
                END IF
!
 620  CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 630 K=2,KMAX+3
        DO 630 I=1,IMAX+4
        DO 630 J=1,JMAX+3

            !WSQ(I,J,K)=SQRT(ABS(3.0*ZZM(I,J,K)/DZ1(I,J,K)**2+1.0))*DZ1(I,J,K)
            !WSQ2(I,J,K)=SQRT(ABS(3.0*ZZM2(I,J,K)/DZ1(I,J,K)**2+1.0))*DZ1(I,J,K)

            WSQ(I,J,K)=SQRT(ABS(3.0*ZZM(I,J,K)/DZ1(I,J,K)**2+1.0))*DZ1(I,J,K)

 630    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 640 KK=1,3
        DO 640 II=1,3
        DO 640 JJ=1,3
        DO 640 K=1,KMAX+4
        DO 640 I=1,IMAX+4
        DO 640 J=1,JMAX+3

            fz(I,J,K,II,JJ,KK)=0.0
            ZF(I,J,K,II,JJ,KK)=0.0

 640    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 655 JJ=-1,1
        DO 655 II=-1,1
        DO 655 K=2,KMAX+3
        DO 655 I=2,IMAX+3
        DO 655 J=2,JMAX+1

            !fz(I,J,K,II+2,JJ+2,1)=(ZM(I+II,J+JJ,K-1)+WSQ(I+II,J+JJ,K-1)-0.50*(Z(I,J,K)+Z(I,J,K-1))) &
            !                     /(WSQ(I+II,J+JJ,K-1)+WSQ2(I+II,J+JJ,K-1))
            !ZF(I,J,K,II+2,JJ+2,1)=0.50*(ZM(I+II,J+JJ,K-1)+WSQ(I+II,J+JJ,K-1)+0.50*(Z(I,J,K)+Z(I,J,K-1)))

            !fz(I,J,K,II+2,JJ+2,3)=(0.50*(Z(I,J,K)+Z(I,J,K+1))-(ZM(I+II,J+JJ,K+1)-WSQ2(I+II,J+JJ,K+1))) &
            !                     /(WSQ(I+II,J+JJ,K+1)+WSQ2(I+II,J+JJ,K+1))
            !ZF(I,J,K,II+2,JJ+2,3)=0.50*(0.50*(Z(I,J,K)+Z(I,J,K+1))+ZM(I+II,J+JJ,K+1)-WSQ2(I+II,J+JJ,K+1))

            fz(I,J,K,II+2,JJ+2,1)=(ZM(I+II,J+JJ,K-1)+WSQ(I+II,J+JJ,K-1)-0.50*(Z(I,J,K)+Z(I,J,K-1))) &
                                 /(2.0*WSQ(I+II,J+JJ,K-1))
            ZF(I,J,K,II+2,JJ+2,1)=0.50*(ZM(I+II,J+JJ,K-1)+WSQ(I+II,J+JJ,K-1)+0.50*(Z(I,J,K)+Z(I,J,K-1)) )

            fz(I,J,K,II+2,JJ+2,3)=(0.50*(Z(I,J,K)+Z(I,J,K+1))-(ZM(I+II,J+JJ,K+1)-WSQ(I+II,J+JJ,K+1))) &
                                 /(2.0*WSQ(I+II,J+JJ,K+1))
            ZF(I,J,K,II+2,JJ+2,3)=0.50*(0.50*(Z(I,J,K)+Z(I,J,K+1))+ZM(I+II,J+JJ,K+1)-WSQ(I+II,J+JJ,K+1))

 655    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 660 JJ=-1,1
        DO 660 II=-1,1
        DO 660 K=2,KMAX+3
        DO 660 I=2,IMAX+3
        DO 660 J=2,JMAX+1

            IF(fz(I,J,K,II+2,JJ+2,1).LT.0.0) THEN
            !IF(fz(I,J,K,II+2,JJ+2,1).LE.0.0) THEN
                fz(I,J,K,II+2,JJ+2,1)=0.0
                ZF(I,J,K,II+2,JJ+2,1)=0.0
                !ZF(I,J,K,II+2,JJ+2,1)=Z(I+II,J+JJ,K-1)
            END IF

            IF(fz(I,J,K,II+2,JJ+2,1).GT.1.0) THEN
                fz(I,J,K,II+2,JJ+2,1)=1.0
                ZF(I,J,K,II+2,JJ+2,1)=ZM(I+II,J+JJ,K-1)
            END IF

            IF(fz(I,J,K,II+2,JJ+2,3).LT.0.0) THEN
            !IF(fz(I,J,K,II+2,JJ+2,3).LE.0.0) THEN
                fz(I,J,K,II+2,JJ+2,3)=0.0
                ZF(I,J,K,II+2,JJ+2,3)=0.0
                !ZF(I,J,K,II+2,JJ+2,3)=Z(I+II,J+JJ,K+1)
            END IF

            IF(fz(I,J,K,II+2,JJ+2,3).GT.1.0) THEN 
                fz(I,J,K,II+2,JJ+2,3)=1.0
                ZF(I,J,K,II+2,JJ+2,3)=ZM(I+II,J+JJ,K+1)
            END IF

 660    CONTINUE
!$OMP END DO
!$OMP END PARALLEL
!
!C                 DO 675 JJ=-1,1
!C                 DO 675 II=-1,1
!C                 DO 675 K=3,KMAX+2
!C                 DO 675 I=3,IMAX+2
!C                 DO 675 J=3,JMAX+1
!C         fz(I,J,K,II+2,JJ+2,2)=1.0-fz(I,J,K+1,II+2,JJ+2,1)
!C     &                         -fz(I,J,K-1,II+2,JJ+2,3)
!C       IF (fz(I,J,K,II+2,JJ+2,2).LE.0.0) THEN
!C         fz(I,J,K,II+2,JJ+2,2)=0.0
!C       END IF
!C 675		CONTINUE

!$OMP PARALLEL
!$OMP DO
        DO 670 JJ=-1,1
        DO 670 II=-1,1
        DO 670 K=2,KMAX+3
        DO 670 I=2,IMAX+3
        DO 670 J=2,JMAX+1

            !IF((ZM(I+II,J+JJ,K)-WSQ2(I+II,J+JJ,K)).LT.0.50*(Z(I,J,K)+Z(I,J,K-1))) THEN
            !    IF((ZM(I+II,J+JJ,K)+WSQ(I+II,J+JJ,K)).LT.0.50*(Z(I,J,K)+Z(I,J,K-1))) THEN
            !        fz(I,J,K,II+2,JJ+2,2)=0.0
            !        ZF(I,J,K,II+2,JJ+2,2)=0.0
            !    ELSE
            !        IF((ZM(I+II,J+JJ,K)+WSQ(I+II,J+JJ,K)).LT.0.50*(Z(I,J,K)+Z(I,J,K+1))) THEN
            !            fz(I,J,K,II+2,JJ+2,2)=(ZM(I+II,J+JJ,K)+WSQ(I+II,J+JJ,K)-0.50*(Z(I,J,K)+Z(I,J,K-1))) &
            !                                 /(WSQ(I+II,J+JJ,K)+WSQ2(I+II,J+JJ,K))
            !            ZF(I,J,K,II+2,JJ+2,2)=0.50*(ZM(I+II,J+JJ,K)+WSQ(I+II,J+JJ,K)+0.50*(Z(I,J,K)+Z(I,J,K-1)))
            !        ELSE
            !            !fz(I,J,K,II+2,JJ+2,2)=0.50*(Z(I,J,K+1)-Z(I,J,K-1))/(WSQ(I+II,J+JJ,K)+WSQ2(I+II,J+JJ,K))
            !            fz(I,J,K,II+2,JJ+2,2)=1.0/DZ(K)/(WSQ(I+II,J+JJ,K)+WSQ2(I+II,J+JJ,K))   !SUMO23/06/21
            !            !ZF(I,J,K,II+2,JJ+2,2)=0.50*(Z(I,J,K+1)+Z(I,J,K-1))
            !            ZF(I,J,K,II+2,JJ+2,2)=Z(I,J,K)  !SUMO23/06/21
            !        END IF
            !    END IF
            !ELSE
            !    IF((ZM(I+II,J+JJ,K)-WSQ2(I+II,J+JJ,K)).LT.0.50*(Z(I,J,K)+Z(I,J,K+1))) THEN
            !        IF((ZM(I+II,J+JJ,K)+WSQ(I+II,J+JJ,K)).LT.0.50*(Z(I,J,K)+Z(I,J,K+1))) THEN
            !            fz(I,J,K,II+2,JJ+2,2)=1.0
            !            ZF(I,J,K,II+2,JJ+2,2)=ZM(I+II,J+JJ,K)
            !        ELSE
            !            fz(I,J,K,II+2,JJ+2,2)=(0.50*(Z(I,J,K)+Z(I,J,K+1))-(ZM(I+II,J+JJ,K)-WSQ2(I+II,J+JJ,K))) &
            !                                 /(WSQ(I+II,J+JJ,K)+WSQ2(I+II,J+JJ,K))
            !            ZF(I,J,K,II+2,JJ+2,2)=0.50*(0.50*(Z(I,J,K)+Z(I,J,K+1))+ZM(I+II,J+JJ,K)-WSQ2(I+II,J+JJ,K))
            !        END IF 
            !    ELSE
            !        fz(I,J,K,II+2,JJ+2,2)=0.0
            !        ZF(I,J,K,II+2,JJ+2,2)=0.0
            !    END IF
            !END IF

            IF((ZM(I+II,J+JJ,K)-WSQ(I+II,J+JJ,K)).LT.0.50*(Z(I,J,K)+Z(I,J,K-1))) THEN
                IF((ZM(I+II,J+JJ,K)+WSQ(I+II,J+JJ,K)).LT.0.50*(Z(I,J,K)+Z(I,J,K-1))) THEN
                    fz(I,J,K,II+2,JJ+2,2)=0.0
                    ZF(I,J,K,II+2,JJ+2,2)=0.0
                ELSE
                    IF((ZM(I+II,J+JJ,K)+WSQ(I+II,J+JJ,K)).LT.0.50*(Z(I,J,K)+Z(I,J,K+1))) THEN
                        fz(I,J,K,II+2,JJ+2,2)=(ZM(I+II,J+JJ,K)+WSQ(I+II,J+JJ,K)-0.50*(Z(I,J,K)+Z(I,J,K-1))) &
                                             /(2.0*WSQ(I+II,J+JJ,K))
                        ZF(I,J,K,II+2,JJ+2,2)=0.50*(ZM(I+II,J+JJ,K)+WSQ(I+II,J+JJ,K)+0.50*(Z(I,J,K)+Z(I,J,K-1)))
                    ELSE
                        fz(I,J,K,II+2,JJ+2,2)=0.50*(Z(I,J,K+1)-Z(I,J,K-1))/(2.0*WSQ(I+II,J+JJ,K))
                        ZF(I,J,K,II+2,JJ+2,2)=0.50*(Z(I,J,K+1)+Z(I,J,K-1))
                    END IF
                END IF
            ELSE
                IF((ZM(I+II,J+JJ,K)-WSQ(I+II,J+JJ,K)).LT.0.50*(Z(I,J,K)+Z(I,J,K+1))) THEN
                    IF((ZM(I+II,J+JJ,K)+WSQ(I+II,J+JJ,K)).LT.0.50*(Z(I,J,K)+Z(I,J,K+1))) THEN
                        fz(I,J,K,II+2,JJ+2,2)=1.0
                        ZF(I,J,K,II+2,JJ+2,2)=ZM(I+II,J+JJ,K)
                    ELSE
                        fz(I,J,K,II+2,JJ+2,2)=(0.50*(Z(I,J,K)+Z(I,J,K+1))-(ZM(I+II,J+JJ,K)-WSQ(I+II,J+JJ,K))) &
                                             /(2.0*WSQ(I+II,J+JJ,K))
                        ZF(I,J,K,II+2,JJ+2,2)=0.50*(0.50*(Z(I,J,K)+Z(I,J,K+1))+ZM(I+II,J+JJ,K)-WSQ(I+II,J+JJ,K))
                    END IF 
                ELSE
                    fz(I,J,K,II+2,JJ+2,2)=0.0
                    ZF(I,J,K,II+2,JJ+2,2)=0.0
                END IF
            END IF

 670  CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!C///////////////////////////////////////////////////////////
!C//////////////   FINAL  REALLOCATION  /////////////////////
!C///////////////////////////////////////////////////////////
!
    !$OMP PARALLEL
    !$OMP DO
!
            DO 400 K=3,KMAX+2    !K=2,KMAX+3
            DO 400 I=3,IMAX+2    !I=3,IMAX+2
            DO 400 J=3,JMAX+1    !J=3,JMAX+1
!
                CT(I,J,K)=0.0
                UST(I,J,K)=0.0
                VST(I,J,K)=0.0
                WST(I,J,K)=0.0
                USRT(I,J,K)=0.0
                VSRT(I,J,K)=0.0
                WSRT(I,J,K)=0.0
                XG1(I,J,K)=0.0
                YG(I,J,K)=0.0
                ZG(I,J,K)=0.0
!
 400    CONTINUE
!
    !$OMP END DO
    !$OMP END PARALLEL
!
            DO 450 KK=-1,1
            DO 450 JJ=-1,1
            DO 450 II=-1,1
            DO 450 K=3,KMAX+2
            DO 450 I=3,IMAX+2
            DO 450 J=3,JMAX+1
!
                USRT(I,J,K)=fx(I,J,K,II+2,JJ+2,KK+2)*fy(I,J,K,II+2,JJ+2,KK+2) &
                           *fz(I,J,K,II+2,JJ+2,KK+2)*USR(I+II,J+JJ,K+KK) &
                           *CM(I+II,J+JJ,K+KK)*GHX(I+II)/GHX(I)+USRT(I,J,K)
                VSRT(I,J,K)=fx(I,J,K,II+2,JJ+2,KK+2)*fy(I,J,K,II+2,JJ+2,KK+2) &
                           *fz(I,J,K,II+2,JJ+2,KK+2)*VSR(I+II,J+JJ,K+KK) &
                           *CM(I+II,J+JJ,K+KK)*GHX(I+II)/GHX(I)+VSRT(I,J,K)
                WSRT(I,J,K)=fx(I,J,K,II+2,JJ+2,KK+2)*fy(I,J,K,II+2,JJ+2,KK+2) &
                           *fz(I,J,K,II+2,JJ+2,KK+2)*WSR(I+II,J+JJ,K+KK) &
                           *CM(I+II,J+JJ,K+KK)*GHX(I+II)/GHX(I)+WSRT(I,J,K)
!C
                XG1(I,J,K)=fx(I,J,K,II+2,JJ+2,KK+2)*fy(I,J,K,II+2,JJ+2,KK+2) &
                           *fz(I,J,K,II+2,JJ+2,KK+2)*XF(I,J,K,II+2,JJ+2,KK+2) &
                           *CM(I+II,J+JJ,K+KK)*GHX(I+II)/GHX(I)+XG1(I,J,K)
                YG(I,J,K)=fx(I,J,K,II+2,JJ+2,KK+2)*fy(I,J,K,II+2,JJ+2,KK+2) &
                           *fz(I,J,K,II+2,JJ+2,KK+2)*YF(I,J,K,II+2,JJ+2,KK+2) &
                           *CM(I+II,J+JJ,K+KK)*GHX(I+II)/GHX(I)+YG(I,J,K)
                ZG(I,J,K)=fx(I,J,K,II+2,JJ+2,KK+2)*fy(I,J,K,II+2,JJ+2,KK+2) &
                           *fz(I,J,K,II+2,JJ+2,KK+2)*ZF(I,J,K,II+2,JJ+2,KK+2) &
                           *CM(I+II,J+JJ,K+KK)*GHX(I+II)/GHX(I)+ZG(I,J,K)
!
!********** DON'T MOVE SOLID PHASE **********!
!
!                IF(TIME.LE.1.0) THEN
!                    CT(I,J,K)=0.600
!                    IF (J.GE.IGL(I,K)+1) CT(I,J,K)=0.0
!                    UST(I,J,K)=0.0
!                    VST(I,J,K)=0.0
!                    WST(I,J,K)=0.0
!                ELSE
!
                CT(I,J,K)=fx(I,J,K,II+2,JJ+2,KK+2)*fy(I,J,K,II+2,JJ+2,KK+2) &
                         *fz(I,J,K,II+2,JJ+2,KK+2)*CM(I+II,J+JJ,K+KK) &
                         *GHX(I+II)/GHX(I)+CT(I,J,K)
!
                UST(I,J,K)=fx(I,J,K,II+2,JJ+2,KK+2)*fy(I,J,K,II+2,JJ+2,KK+2) &
                          *fz(I,J,K,II+2,JJ+2,KK+2)*US(I+II,J+JJ,K+KK) &
                          *CM(I+II,J+JJ,K+KK)*GHX(I+II)/GHX(I)+UST(I,J,K)
                VST(I,J,K)=fx(I,J,K,II+2,JJ+2,KK+2)*fy(I,J,K,II+2,JJ+2,KK+2) &
                          *fz(I,J,K,II+2,JJ+2,KK+2)*VS(I+II,J+JJ,K+KK) &
                          *CM(I+II,J+JJ,K+KK)*GHX(I+II)/GHX(I)+VST(I,J,K)
                WST(I,J,K)=fx(I,J,K,II+2,JJ+2,KK+2)*fy(I,J,K,II+2,JJ+2,KK+2) &
                          *fz(I,J,K,II+2,JJ+2,KK+2)*WS(I+II,J+JJ,K+KK) &
                          *CM(I+II,J+JJ,K+KK)*GHX(I+II)/GHX(I)+WST(I,J,K)
!
!                END IF
!
 450  CONTINUE
!
            DO 480 K=3,KMAX+2
            DO 480 I=3,IMAX+2
            DO 480 J=3,JMAX+1
!
                IF (CT(I,J,K).GT.1.0E-7) THEN
                    C(I,J,K)=CT(I,J,K)
                    CM(I,J,K)=CT(I,J,K)
                    US(I,J,K)=UST(I,J,K)/C(I,J,K)
                    VS(I,J,K)=VST(I,J,K)/C(I,J,K)
                    WS(I,J,K)=WST(I,J,K)/C(I,J,K)
                    USR(I,J,K)=USRT(I,J,K)/C(I,J,K)
                    VSR(I,J,K)=VSRT(I,J,K)/C(I,J,K)
                    WSR(I,J,K)=WSRT(I,J,K)/C(I,J,K)
                    XG1(I,J,K)=XG1(I,J,K)/C(I,J,K)
                    YG(I,J,K)=YG(I,J,K)/C(I,J,K)
                    ZG(I,J,K)=ZG(I,J,K)/C(I,J,K)
                ELSE
                    C(I,J,K)=0.0
                    CM(I,J,K)=0.0
                    US(I,J,K)=0.0
                    VS(I,J,K)=0.0
                    WS(I,J,K)=0.0
                    USR(I,J,K)=0.0
                    VSR(I,J,K)=0.0
                    WSR(I,J,K)=0.0
                    XG1(I,J,K)=X(I,J,K)
                    YG(I,J,K)=Y(I,J,K)
                    ZG(I,J,K)=Z(I,J,K)
                END IF
!C
 480    CONTINUE
!C
            DO 481 K=3,KMAX+2
            DO 481 I=3,IMAX+2   !I=3,IMAX+2
            DO 481 J=3,JMAX+1   !J=3,JMAX
!C
                IF (CM(I,J,K).GE.CSmax) THEN
                    IF (CM(I,J-1,K).LE.CSmax) THEN
                        C(I,J-1,K)=C(I,J-1,K)+(C(I,J,K)-CSmax)
                        C(I,J,K)=Csmax
                        IF (C(I,J-1,K).GT.CSmax) THEN
                            C(I+1,J,K)=C(I+1,J,K)+(C(I,J-1,K)-CSmax)
                            C(I,J-1,K)=CSmax
                            IF (C(I+1,J,K).GT.CSmax) THEN
                                C(I-1,J,K)=C(I-1,J,K)+(C(I+1,J,K)-CSmax)
                                C(I+1,J,K)=CSmax
                                IF (C(I-1,J,K).GT.CSmax) THEN
                                    C(I,J+1,K)=C(I,J+1,K)+(C(I-1,J,K)-CSmax)
                                    C(I-1,J,K)=CSmax
                                END IF
                            END IF
                        END IF
                    ELSE
                        IF (C(I,J-1,K).GT.CSmax) THEN
                            C(I+1,J,K)=C(I+1,J,K)+(C(I,J-1,K)-CSmax)
                            C(I,J-1,K)=CSmax
                            IF (C(I+1,J,K).GT.CSmax) THEN
                                C(I-1,J,K)=C(I-1,J,K)+(C(I+1,J,K)-CSmax)
                                C(I+1,J,K)=CSmax
                                IF (C(I-1,J,K).GT.CSmax) THEN
                                    C(I,J+1,K)=C(I,J+1,K)+(C(I-1,J,K)-CSmax)
                                    C(I-1,J,K)=CSmax
                                END IF
                            END IF
                        END IF
                    END IF
                END IF
!C
                CM(I,J+1,K)=C(I,J+1,K)
                CM(I,J,K)=C(I,J,K)
                CM(I+1,J,K)=C(I+1,J,K)
                CM(I-1,J,K)=C(I-1,J,K)
                CM(I,J-1,K)=C(I,J-1,K)
!                CM(I,J,K-1)=C(I,J,K-1)
!                CM(I,J,K+1)=C(I,J,K+1)
!C
                TEMP(I,J,K)=VSR(I,J,K)
!C
 481              CONTINUE
!C
!!*************  Boundary Condition again *****************
!!
!!********** Lower Boundary **********!
!!
              DO 482 J=1,3
              DO 482 K=1,KMAX+4
              DO 482 I=1,IMAX+4
!!
                  C(I,J,K)=0.600
                  TEMP(I,J,K)=0.0
                  US(I,J,K)=0.0
                  VS(I,J,K)=0.0
                  WS(I,J,K)=0.0
                  TS(I,J,K)=0.0
                  USR(I,J,K)=TEMP(I,J,K)
                  VSR(I,J,K)=TEMP(I,J,K)
                  WSR(I,J,K)=TEMP(I,J,K)
                  CM(I,J,K)=C(I,J,K)
                  XG1(I,J,K)=X(I,J,K)
                  YG(I,J,K)=Y(I,J,K)
                  ZG(I,J,K)=Z(I,J,K)
!
 482  CONTINUE
!
!********** Upper Boundary **********!
!
              DO 484 J=JMAX+2,JMAX+3
              DO 484 K=1,KMAX+4
              DO 484 I=1,IMAX+4
!
                  C(I,J,K)=0.0
                  US(I,J,K)=US(I,JMAX+1,K)
                  VS(I,J,K)=0.0
                  WS(I,J,K)=WS(I,JMAX+1,K)
                  TEMP(I,J,K)=TEMP(I,JMAX+1,K)
                  USR(I,J,K)=USR(I,JMAX+1,K)
                  VSR(I,J,K)=VSR(I,JMAX+1,K)
                  WSR(I,J,K)=WSR(I,JMAX+1,K)
                  CM(I,J,K)=C(I,J,K)
                  XG1(I,J,K)=X(I,J,K)
                  YG(I,J,K)=Y(I,J,K)
                  ZG(I,J,K)=Z(I,J,K)
!
 484  CONTINUE
!
!********** Left-Side Boundary **********!
!
              DO 486 I=1,2
              DO 486 K=1,KMAX+4
              DO 486 J=1,JMAX+3
!
!CCCCCC////// Periodic Boundary Condition ////////

                  C(I,J,K)=C(IMAX+I,J,K)
                  CM(I,J,K)=CM(IMAX+I,J,K)
                  US(I,J,K)=US(IMAX+I,J,K)
                  VS(I,J,K)=VS(IMAX+I,J,K)
                  WS(I,J,K)=WS(IMAX+I,J,K)
                  TEMP(I,J,K)=TEMP(IMAX+I,J,K)
                  USR(I,J,K)=TEMP(IMAX+I,J,K)
                  VSR(I,J,K)=TEMP(IMAX+I,J,K)
!C                 USR(I,J,K)=USR(IMAX+I,J,K)
!C                 VSR(I,J,K)=VSR(IMAX+I,J,K)
                  WSR(I,J,K)=TEMP(IMAX+I,J,K)
                  TS(I,J,K)=TS(IMAX+I,J,K)
                  XG1(I,J,K)=X(I,J,K)
                  YG(I,J,K)=Y(I,J,K)
                  ZG(I,J,K)=Z(I,J,K)

!CCCCCC////// LOG-LAW Boundary Condition ////////
!
                  !IF(J.LE.IGL(I,K)) THEN
                  !    C(I,J,K)=0.600
                  !    US(I,J,K)=0.0
                  !ELSE
                  !    C(I,J,K)=0.0
                  !    US(I,J,K)=U(I,J,K)!AA*(1.0/0.40*log(1.0*(J-42.5))+8.50)
                  !END IF
!
                  !CM(I,J,K)=C(I,J,K)
                  !TEMP(I,J,K)=0.0
                  !TS(I,J,K)=0.0
                  !VS(I,J,K)=0.0
                  !WS(I,J,K)=0.0
                  !USR(I,J,K)=TEMP(I,J,K)
                  !VSR(I,J,K)=TEMP(I,J,K)
                  !WSR(I,J,K)=TEMP(I,J,K)
                  !XG1(I,J,K)=X(I,J,K)
                  !YG(I,J,K)=Y(I,J,K)
                  !ZG(I,J,K)=Z(I,J,K)

 486  CONTINUE

!********** Right-Side Boundary **********!

              DO 488 I=IMAX+3,IMAX+4
              DO 488 K=1,KMAX+4
              DO 488 J=1,JMAX+3

!CCCCCC////// Periodic Boundary Condition ////////

                  C(I,J,K)=C(I-IMAX,J,K)
                  CM(I,J,K)=CM(I-IMAX,J,K)
                  US(I,J,K)=US(I-IMAX,J,K)
                  VS(I,J,K)=VS(I-IMAX,J,K)
                  WS(I,J,K)=WS(I-IMAX,J,K)
                  TEMP(I,J,K)=TEMP(I-IMAX,J,K)
                  USR(I,J,K)=TEMP(I-IMAX,J,K)
                  VSR(I,J,K)=TEMP(I-IMAX,J,K)
                  WSR(I,J,K)=TEMP(I-IMAX,J,K)
!C                 USR(I,J,K)=USR(I-IMAX,J,K)
!C                 VSR(I,J,K)=VSR(I-IMAX,J,K)
                  TS(I,J,K)=TS(I-IMAX,J,K)
                  XG1(I,J,K)=X(I,J,K)
                  YG(I,J,K)=Y(I,J,K)
                  ZG(I,J,K)=Z(I,J,K)

!CCCCCC////// ZERO Gradient Boundary Condition ////////

                  !C(I,J,K)=C(IMAX+2,J,K)
                  !CM(I,J,K)=CM(IMAX+2,J,K)
                  !US(I,J,K)=US(IMAX+2,J,K)
                  !VS(I,J,K)=VS(IMAX+2,J,K)
                  !WS(I,J,K)=WS(IMAX+2,J,K)
                  !USR(I,J,K)=USR(IMAX+2,J,K)
                  !VSR(I,J,K)=VSR(IMAX+2,J,K)
                  !WSR(I,J,K)=WSR(IMAX+2,J,K)
                  !TEMP(I,J,K)=TEMP(IMAX+2,J,K)
                  !TS(I,J,K)=TS(IMAX+2,J,K)
                  !XG1(I,J,K)=X(I,J,K)
                  !YG(I,J,K)=Y(I,J,K)
                  !ZG(I,J,K)=Z(I,J,K)

 488  CONTINUE

!********** Spanwise Direction **********!
!********** TEMAE Boundary **********!

              DO 489 K=1,2
              DO 489 I=1,IMAX+4
              DO 489 J=1,JMAX+3

!CCCCCC////// Periodic Boundary Condition ////////

                  C(I,J,K)=C(I,J,KMAX+K)
                  CM(I,J,K)=CM(I,J,KMAX+K)
                  US(I,J,K)=US(I,J,KMAX+K)
                  VS(I,J,K)=VS(I,J,KMAX+K)
                  WS(I,J,K)=WS(I,J,KMAX+K)
                  TEMP(I,J,K)=TEMP(I,J,KMAX+K)
                  USR(I,J,K)=USR(I,J,KMAX+K)
                  VSR(I,J,K)=VSR(I,J,KMAX+K)
                  WSR(I,J,K)=WSR(I,J,KMAX+K)
                  TS(I,J,K)=TS(I,J,KMAX+K)
                  XG1(I,J,K)=X(I,J,K)
                  YG(I,J,K)=Y(I,J,K)
                  ZG(I,J,K)=Z(I,J,K)

!CCCCCC////// Wall Condition ///////

                  !C(I,J,K)=C(I,J,3)
                  !CM(I,J,K)=CM(I,J,3)

!********** SLIP **********!

                  !US(I,J,K)=US(I,J,3)
                  !VS(I,J,K)=VS(I,J,3)
                  !WS(I,J,K)=0.0

!********** NO-SLIP (WALL POSITION K=2.5) **********!
!
!                  US(I,J,2)=-US(I,J,3)
!                  US(I,J,1)=-US(I,J,4)
!                  VS(I,J,K)=VS(I,J,3)
!                  WS(I,J,2)=-WS(I,J,3)
!                  WS(I,J,1)=-WS(I,J,4)
!
                  !USR(I,J,K)=USR(I,J,3)
                  !VSR(I,J,K)=VSR(I,J,3)
                  !WSR(I,J,K)=WSR(I,J,3)
                  !TEMP(I,J,K)=TEMP(I,J,3)
                  !TS(I,J,K)=TS(I,J,3)
                  !XG1(I,J,K)=X(I,J,K)
                  !YG(I,J,K)=Y(I,J,K)
                  !ZG(I,J,K)=Z(I,J,K)

 489  CONTINUE

!********** TEMAE Boundary **********!

              DO 491 K=KMAX+3,KMAX+4
              DO 491 I=1,IMAX+4
              DO 491 J=1,JMAX+3

!CCCCCC////// Periodic Boundary Condition ////////

                  C(I,J,K)=C(I,J,K-KMAX)
                  CM(I,J,K)=CM(I,J,K-KMAX)
                  US(I,J,K)=US(I,J,K-KMAX)
                  VS(I,J,K)=VS(I,J,K-KMAX)
                  WS(I,J,K)=WS(I,J,K-KMAX)
                  TEMP(I,J,K)=TEMP(I,J,K-KMAX)
                  USR(I,J,K)=USR(I,J,K-KMAX)
                  VSR(I,J,K)=VSR(I,J,K-KMAX)
                  WSR(I,J,K)=WSR(I,J,K-KMAX)
                  TS(I,J,K)=TS(I,J,K-KMAX)
                  XG1(I,J,K)=X(I,J,K)
                  YG(I,J,K)=Y(I,J,K)
                  ZG(I,J,K)=Z(I,J,K)

!CCCCCC////// Wall Condition ///////

                  !C(I,J,K)=C(I,J,KMAX+2)
                  !CM(I,J,K)=CM(I,J,KMAX+2)

!********** SLIP **********!

                  !US(I,J,K)=US(I,J,KMAX+2)
                  !VS(I,J,K)=VS(I,J,KMAX+2)
                  !WS(I,J,K)=0.0

!********** NO-SLIP (WALL POSITION K=KMAX+2.5) **********!

!                  US(I,J,KMAX+2)=-US(I,J,KMAX+3)
!                  US(I,J,KMAX+1)=-US(I,J,KMAX+4)
!                  VS(I,J,K)=VS(I,J,KMAX+3)
!                  WS(I,J,KMAX+2)=-WS(I,J,KMAX+3)
!                  WS(I,J,KMAX+1)=-WS(I,J,KMAX+4)

                  !USR(I,J,K)=USR(I,J,KMAX+2)
                  !VSR(I,J,K)=VSR(I,J,KMAX+2)
                  !WSR(I,J,K)=WSR(I,J,KMAX+2)
                  !TEMP(I,J,K)=TEMP(I,J,KMAX+2)
                  !TS(I,J,K)=TS(I,J,KMAX+2)
                  !XG1(I,J,K)=X(I,J,K)
                  !YG(I,J,K)=Y(I,J,K)
                  !ZG(I,J,K)=Z(I,J,K)

 491  CONTINUE

!C/////  ZANSA  //////
!C
          IF (naka.EQ.0.0) THEN
!C
          ZANC=0.0
          AC=0.0
!
                   DO 490 K=3,KMAX+2
                   DO 490 I=3,IMAX+2
                   DO 490 J=3,JMAX+1
          IF (I.EQ.3) AC=C(I,J,K)+AC
          ZANC=ABS(CM(I,J,K)-C(I,J,K))+ZANC
 490              CONTINUE
      WRITE(*,1096)AC
 1096    FORMAT(' Total(C)=',F10.5)
          END IF
!C
!
!
    deallocate ( &
      fx,XF,fy,YF,fz,ZF,CT,TLT,UST,VST,USRT,VSRT &
     ,WSRT,WST,OMEXG,OMEYG,OMEZG,HKW,HKU,HKV &
     ,DD1G,DD2G,DD3G,AU,W0,AP1,AP2,BTE,AP3 &
!
     ,XXM,YYM,DX1,DX2,DY1,DY2,DZ1,DZ2,USQ,VSQ,WSQ,ZZM &
     ,TP,VR,VSQ2,YYM2,TAUP,JI,CAV,SL &  !TAUS,PS
     ,PRO,DIS,TS,arfa0 &    !PCX,PCY,TAUC11,TAUC12,TAUC21,TAUC22 &
!     ,TAUC31,TAUC32,TAUC13,TAUC23,TAUC33,PCZ &
     ,XXM2,USQ2,ZZM2,WSQ2)
!
      RETURN
      END

!C*****************************************************************
      SUBROUTINE ENERGY
!C*****************************************************************
    USE MF_SF
    USE OMP_LIB
    IMPLICIT NONE

      allocate ( &
      HKGENE(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),HKEENE(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,HKGG(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),HKEE(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,HK0(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),HKN(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,DK(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,SK(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),CK(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,HTEI(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),TEI1(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,CKG(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),CKE(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,UUSRK(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),VVSRK(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,WWSRK(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),WWSRK1(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,UCK(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),VCK(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,HAMF(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,UUSRK1(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),VVSRK1(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,UKD(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),VKD(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,WK(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),WSK(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,WKG(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),WKE(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,WKD(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),CKD(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,HKDENE(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),HKDD(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,MYTDENE(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),WCK(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,HTEP(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),HKP(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,USK(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),VSK(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,EK(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4)  &
     ,WDF1(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),WDF2(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,WDF3(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),WDF4(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,WDF5(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),WDF6(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,FW(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,FW1(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),FW2(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,FW3(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),FW4(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,FW5(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),FW6(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,DETN(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,STAT = ALLOC_ERR )

!C******************************************************************

!$OMP PARALLEL
!$OMP DO
        DO 100 K=2,KMAX+3
        DO 100 I=2,IMAX+3
        DO 100 J=2,JMAX+2

            UK(I,J,K)=U(I,J,K)
            VK(I,J,K)=V(I,J,K)
            WK(I,J,K)=W(I,J,K)
            CK(I,J,K)=C(I,J,K)
            USK(I,J,K)=US(I,J,K)
            VSK(I,J,K)=VS(I,J,K)
            WSK(I,J,K)=WS(I,J,K)
            UUSRK(I,J,K)=UUSR(I,J,K)
            VVSRK(I,J,K)=VVSR(I,J,K)
            WWSRK(I,J,K)=WWSR(I,J,K)
            UUSRK1(I,J,K)=UUSR1(I,J,K)
            VVSRK1(I,J,K)=VVSR1(I,J,K)
            WWSRK1(I,J,K)=WWSR1(I,J,K)

 100    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 80 K=2,KMAX+3 !K=3,KMAX+2
        DO 80 I=2,IMAX+3
        DO 80 J=2,JMAX+2

            RE1ENE(I,J,K)=SQRT((ABS(USK(I,J,K)-UK(I,J,K)))**2 &
                              +(ABS(VSK(I,J,K)-VK(I,J,K)))**2 &
                              +(ABS(WSK(I,J,K)-WK(I,J,K)))**2)*DR/RED

 80     CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 85 K=2,KMAX+3    !K=3,KMAX+2
        DO 85 I=2,IMAX+3
        DO 85 J=2,JMAX+2
            !ë¨ìxÅEîZìxÇÃãÛä‘å˘îzÇíÜêSç∑ï™Ç≈éZèo
            UKG(I,J,K)=(UK(I+1,J,K)-UK(I-1,J,K))*0.50*GX(I)
            UKE(I,J,K)=(UK(I,J+1,K)-UK(I,J-1,K))*0.50*EY(J)
            UKD(I,J,K)=(UK(I,J,K+1)-UK(I,J,K-1))*0.50*DZ(K)

            VKG(I,J,K)=(VK(I+1,J,K)-VK(I-1,J,K))*0.50*GX(I)
            VKE(I,J,K)=(VK(I,J+1,K)-VK(I,J-1,K))*0.50*EY(J)
            VKD(I,J,K)=(VK(I,J,K+1)-VK(I,J,K-1))*0.50*DZ(K)

            WKG(I,J,K)=(WK(I+1,J,K)-WK(I-1,J,K))*0.50*GX(I)
            WKE(I,J,K)=(WK(I,J+1,K)-WK(I,J-1,K))*0.50*EY(J)
            WKD(I,J,K)=(WK(I,J,K+1)-WK(I,J,K-1))*0.50*DZ(K)

            CKG(I,J,K)=(CK(I+1,J,K)-CK(I-1,J,K))*0.50*GX(I)
            CKE(I,J,K)=(CK(I,J+1,K)-CK(I,J-1,K))*0.50*EY(J)
            CKD(I,J,K)=(CK(I,J,K+1)-CK(I,J,K-1))*0.50*DZ(K)

            !GALä÷åWÅH
            TEI1(I,J,K)=18.0*RED/DR**2 &
                        *ABS(CK(I,J,K)/(1.0-CK(I,J,K)) ) &
                       !*(1.0+0.150*RE1ENE(I,J,K)**0.6870)
                        *(1.0+0.4320*RE1ENE(I,J,K)**0.6870)  !SUMO23/10/23
!CCCCCCCCcc     &             *ABS(1.0-CK(I,J,K))**(-2.70)
 85     CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 1600 K=1,KMAX+4
        DO 1600 I=1,IMAX+4
        DO 1600 J=1,JMAX+3
            !DETÇÕÉtÉBÉãÉ^ïùÉ¢ÅH
            !DET(I,J,K)=1.0/ABS(GX(I)*EY(J)*DZ(K)*(1.0-CK(I,J,K)))**(1.0/3.0)
            DET(I,J,K)=ABS((1.0/GX(I))*(1.0/EY(J))*(1.0/DZ(K))*(1.0-CK(I,J,K)))**(1.0/3.0)
            !DET(I,J,K)=1.0/ABS(GX(I)*EY(J))**(1.0/2.0)*(1.0-CK(I,J,K))
            !DET(I,J,K)=1.0/ABS(GX(I)*EY(J))**(1.0/2.0)

 1600   CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!***************                            **************
!***************    STARTING OF ITERATION   **************
!***************                            **************

 !20âÒÉãÅ[ÉvÇâÒÇµÇƒÅCGOSAEÇï]âø
        DO 1000 LOOP=1,20
            GOSAE=0.0

!********** BOUNDARY CONDITION **********!

!********** CEILING & GROUND CONDITION **********!

            DO 900 K=3,KMAX+2
            DO 900 I=3,IMAX+2
            DO 900 J=1,IGL(I,K)

!********** CEILING NO-SLIP **********!

!CC                  USTA1=ABS(U(I,JMAX,K)*CAR/LOG(1.50/EY(3)/Z0))
!CC                  HK(I,JMAX+1,K)=ABS(USTAu1(I))**2/0.30
!                  HK(I,JMAX+1,K)=(ABS(USTAu1(I))**2+ABS(VSTAu1(I))**2)/0.30
!                  HK(I,JMAX+2,K)=HK(I,JMAX+1,K)
!                  HK(I,JMAX+3,K)=HK(I,JMAX+2,K)

!********** CEILING SLIP **********!

                !HK(I,JMAX+2,K)=HK(I,JMAX+1,K)
                !HK(I,JMAX+3,K)=HK(I,JMAX+1,K)

!CCCCCC////// ZERO Gradient Boundary Condition ////////

                !HK(I,3,K)=HK(I,4,K)
                !HK(I,2,K)=HK(I,3,K)
                !HK(I,1,K)=HK(I,3,K)

                HK(I,J,K)=HK(I,IGL(I,K)+1,K)

 900    CONTINUE

!********** KARYUUGAWA & JYOURYUUGAWA NO JYOUKEN **********!

            DO 800 K=1,KMAX+4
            DO 800 J=1,JMAX+3

!CCCCCC////// Periodic Boundary Condition ////////

                HK(IMAX+3,J,K)=HK(3,J,K)    !ITO CHANGE
                HK(IMAX+4,J,K)=HK(4,J,K)    !ITO CHANGE
                HK(1,J,K)=HK(IMAX+1,J,K)    !ITO CHANGE
                HK(2,J,K)=HK(IMAX+2,J,K)    !ITO CHANGE

!CCCCCC////// ZERO Gradient Boundary Condition ////////

                !HK(IMAX+3,J,K)=HK(IMAX+2,J,K)
                !HK(IMAX+4,J,K)=HK(IMAX+2,J,K)
                !HK(1,J,K)=HK(3,J,K)
                !HK(2,J,K)=HK(3,J,K)

 800  CONTINUE

!********** OKUYUKI & TEMAE NO JYOUKEN **********!

            DO 700 J=1,JMAX+3
            DO 700 I=1,IMAX+4

!CCCCCC////// Periodic Boundary Condition ////////

                HK(I,J,KMAX+3)=HK(I,J,3)    !ITO CHANGE
                HK(I,J,KMAX+4)=HK(I,J,4)    !ITO CHANGE
                HK(I,J,1)=HK(I,J,KMAX+1)    !ITO CHANGE
                HK(I,J,2)=HK(I,J,KMAX+2)    !ITO CHANGE

!CCCCCC////// ZERO Gradient Boundary Condition ////////

                !SLIP
                !HK(I,J,KMAX+3)=HK(I,J,KMAX+2)    !ITO CHANGE
                !HK(I,J,KMAX+4)=HK(I,J,KMAX+2)    !ITO CHANGE
                !HK(I,J,1)=HK(I,J,3)    !ITO CHANGE
                !HK(I,J,2)=HK(I,J,3)    !ITO CHANGE

                !NO-SLIP
                !HK(I,J,KMAX+3)=-HK(I,J,KMAX+2)  !sumo260220
                !HK(I,J,KMAX+4)=-HK(I,J,KMAX+1)  !sumo260220
                !HK(I,J,1)=-HK(I,J,4)  !sumo260220
                !HK(I,J,2)=-HK(I,J,3)  !sumo260220

                !MYT(I,J,KMAX+3)=-MYT(I,J,KMAX+2)  !sumo260220
                !MYT(I,J,KMAX+4)=-MYT(I,J,KMAX+1)  !sumo260220
                !MYT(I,J,1)=-MYT(I,J,4)  !sumo260220
                !MYT(I,J,2)=-MYT(I,J,3)  !sumo260220

 700  CONTINUE

!CCCCCC////// Surface Boundary Condition ////////CCCCCC

            DO 589 I=3,IMAX+2
            DO 589 K=3,KMAX+2
            DO 589 J=IETA(I,K),JMAX+3
                !êÖñ Ç…Ç®ÇØÇÈóêó¨ÉGÉlÉãÉMÅ[ÇÃåvéZ
                IF(DEPMODE.EQ.1.0) THEN
                    HK(I,J,K)=HK(I,IETA(I,K)-1,K)
                END IF

 589    CONTINUE

!CCCCCC////// Pier Boundary Condition ////////CCCCCC

        !IF(pier.EQ.1.0) THEN

            !DO 701 J=1,JMAX+3
            !DO 701 K=Kpier_R,Kpier_L
            !DO 701 I=Ipier_U,Ipier_D

                !HK(I,J,K)=HK(I,J,Kpier_L+1)
                !HK(Ipier_D,J,K)=HK(Ipier_D+1,J,K)
                !!HK(I,J,Kpier_R)=HK(I,J,Kpier_R-1)
                !!HK(I,J,Kpier_L)=HK(I,J,Kpier_L+1)
                !HK(Ipier_U,J,K)=HK(Ipier_U-1,J,K)
                !!HK(I,J,K)=HK(Ipier_U-1,J,K)

 701    CONTINUE

        !END IF
        !IF(pier.EQ.1.0) THEN

            !DO 7011 J=1,JMAX+3
            !DO 7011 K=Kpier_R1,Kpier_L1
            !DO 7011 I=Ipier_U1,Ipier_D1

                !HK(Ipier_D1,J,K)=HK(Ipier_D1+1,J,K)
                !!HK(I,J,Kpier_R1)=HK(I,J,Kpier_R1-1)
                !!HK(I,J,Kpier_L1)=HK(I,J,Kpier_L1+1)
                !HK(I,J,K)=HK(I,J,Kpier_L1+1)
                !HK(Ipier_U1,J,K)=HK(Ipier_U1-1,J,K)

 7011  CONTINUE

        !END IF
        !IF(pier.EQ.1.0) THEN

            !DO 7012 J=1,JMAX+3
            !DO 7012 K=Kpier_R2,Kpier_L2
            !DO 7012 I=Ipier_U2,Ipier_D2

                !HK(Ipier_D2,J,K)=HK(Ipier_D2+1,J,K)
                !!HK(I,J,Kpier_R2)=HK(I,J,Kpier_R2-1)
                !!HK(I,J,Kpier_L2)=HK(I,J,Kpier_L2+1)
                !HK(I,J,K)=HK(I,J,Kpier_R2-1)
                !HK(Ipier_U2,J,K)=HK(Ipier_U2-1,J,K)

 7012   CONTINUE

        !END IF
        !IF(pier.EQ.1.0) THEN

            !DO 7013 J=1,JMAX+3
            !DO 7013 K=Kpier_R3,Kpier_L3
            !DO 7013 I=Ipier_U3,Ipier_D3

                !HK(Ipier_D3,J,K)=HK(Ipier_D3+1,J,K)
                !!HK(I,J,Kpier_R1)=HK(I,J,Kpier_R1-1)
                !!HK(I,J,Kpier_L1)=HK(I,J,Kpier_L1+1)
                !HK(I,J,K)=HK(I,J,Kpier_L3+1)
                !HK(Ipier_U3,J,K)=HK(Ipier_U3-1,J,K)

 7013   CONTINUE

        !END IF
        !IF(pier.EQ.1.0) THEN

            !DO 7014 J=1,JMAX+3
            !DO 7014 K=Kpier_R4,Kpier_L4
            !DO 7014 I=Ipier_U4,Ipier_D4

                !HK(Ipier_D4,J,K)=HK(Ipier_D4+1,J,K)
                !!HK(I,J,Kpier_R2)=HK(I,J,Kpier_R2-1)
                !!HK(I,J,Kpier_L2)=HK(I,J,Kpier_L2+1)
                !HK(I,J,K)=HK(I,J,Kpier_R4-1)
                !HK(Ipier_U4,J,K)=HK(Ipier_U4-1,J,K)

 7014   CONTINUE

        !END IF

!******************** ?øΩ«åÔøΩ?øΩ?øΩ?øΩ?øΩ?øΩf?øΩ?øΩ?øΩ?øΩ?øΩ?øΩ ********************
!!$OMP PARALLEL
!!$OMP DO
!        DO 6751 I=3,IMAX+2
!        DO 6751 J=3,JMAX+1
!        DO 6751 K=3,KMAX+2
!
!            USTABRW(I,J)=ABS(U(I,J,3)/(1.0/0.40*LOG(USTABRW1(I,J)*0.50/EY(3)/RED)+5.50))+0.01
!            USTABLW(I,J)=ABS(U(I,J,KMAX+2)/(1.0/0.40*LOG(USTABLW1(I,J)*0.50/EY(3)/RED)+5.50))+0.01
!            USTABRW_P(I,J)=ABS(U(I,J,Kpier_R-1)/(1.0/0.40*LOG(USTABRW_P1(I,J)*0.50/EY(3)/RED)+5.50))+0.01
!            USTABLW_P(I,J)=ABS(U(I,J,Kpier_L+1)/(1.0/0.40*LOG(USTABLW_P1(I,J)*0.50/EY(3)/RED)+5.50))+0.01
!            WSTABFW_P(J,K)=ABS(W(Ipier_U-1,J,K)/(1.0/0.40*LOG(WSTABFW_P1(J,K)*0.50/EY(3)/RED)+5.50))+0.01
!            WSTABBW_P(J,K)=ABS(W(Ipier_D+1,J,K)/(1.0/0.40*LOG(WSTABBW_P1(J,K)*0.50/EY(3)/RED)+5.50))+0.01
!            
! 6751     CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL
!
!!$OMP PARALLEL
!!$OMP DO
!        DO 6750 K=3,KMAX+2
!        DO 6750 I=3,IMAX+2
!        DO 6750 J=3,JMAX+1
!
!            WDF1(I,J,K)=(ABS(Z(I,3,K)-(Z(I,3,3)-0.50/DZ(3))))*USTABRW(I,J)/RED
!            WDF2(I,J,K)=(ABS(Z(I,3,K)-(Z(I,3,KMAX+2)+0.50/DZ(3))))*USTABLW(I,J)/RED
!            FW1(I,J,K)=1.0-(EXP(-WDF1(I,J,K)/25.0))
!            FW2(I,J,K)=1.0-(EXP(-WDF2(I,J,K)/25.0))
!            IF(pier.EQ.1.0) THEN
!                WDF3(I,J,K)=(ABS(X(I,3,K)-(X(Ipier_U,3,K)-0.50/GX(3))))*WSTABFW_P(J,K)/RED
!                WDF4(I,J,K)=(ABS(X(I,3,K)-(X(Ipier_D,3,K)+0.50/GX(3))))*WSTABBW_P(J,K)/RED
!                WDF5(I,J,K)=(ABS(Z(I,3,K)-(Z(I,3,Kpier_R)-0.50/DZ(3))))*USTABRW_P(I,J)/RED
!                WDF6(I,J,K)=(ABS(Z(I,3,K)-(Z(I,3,Kpier_L)+0.50/DZ(3))))*USTABLW_P(I,J)/RED
!                FW3(I,J,K)=1.0-(EXP(-WDF3(I,J,K)/25.0))
!                FW4(I,J,K)=1.0-(EXP(-WDF4(I,J,K)/25.0))
!                FW5(I,J,K)=1.0-(EXP(-WDF5(I,J,K)/25.0))
!                FW6(I,J,K)=1.0-(EXP(-WDF6(I,J,K)/25.0))
!            END IF
!            
! 6750     CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL

!?øΩ«åÔøΩ?øΩ?øΩ?øΩ?øΩ?øΩl?øΩ?øΩ?øΩ?øΩ?øΩ?øΩ sumo260223
        CKC=0.08
!$OMP PARALLEL
!$OMP DO
        DO 1660 K=2,KMAX+3
        DO 1660 I=2,IMAX+3
        DO 1660 J=2,JMAX+2

            DETN(I,J,K)=DET(I,J,K)/(1.0+CKC*(DET(I,J,K)**2) &
                       *(0.500*((2.0*UKG(I,J,K))**2+(2.0*VKE(I,J,K))**2+(2.0*WKD(I,J,K))**2 &
                       +(UKE(I,J,K)+VKG(I,J,K))**2+(UKD(I,J,K)+WKG(I,J,K))**2+(VKD(I,J,K)+WKE(I,J,K))**2)) &
                       /HK(I,J,K))

 1660   CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!C**********  VORTEX DIFUSION COEF  **************

!$OMP PARALLEL
!$OMP DO
        DO 50 K=1,KMAX+4
        DO 50 I=1,IMAX+4
        DO 50 J=1,JMAX+3

            EK(I,J,K)=CE*ABS(HK(I,J,K))**1.50/DET(I,J,K)
            MYT(I,J,K)=CS*ABS(HK(I,J,K))**0.50*DETN(I,J,K)
            !MYT(I,J,K)=CS*ABS(HK(I,J,K)*FW1(I,J,K)*FW2(I,J,K))**0.50*DET(I,J,K)
            !IF(pier.EQ.1.0) THEN
            !    MYT(I,J,K)=CS*ABS(HK(I,J,K))**0.50*DET(I,J,K)*FW1(I,J,K)*FW2(I,J,K)*FW6(I,J,K)
            !END IF

 50     CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 150 K=3,KMAX+2
        DO 150 I=3,IMAX+2
        DO 150 J=3,JMAX+1

            HKGENE(I,J,K)=(HK(I+1,J,K)-HK(I-1,J,K))*0.50*GX(I)
            HKEENE(I,J,K)=(HK(I,J+1,K)-HK(I,J-1,K))*0.50*EY(J)
            HKDENE(I,J,K)=(HK(I,J,K+1)-HK(I,J,K-1))*0.50*DZ(K)

            HKGG(I,J,K)=(HK(I+1,J,K)+HK(I-1,J,K))*GX(I)**2
            HKEE(I,J,K)=(HK(I,J+1,K)+HK(I,J-1,K))*EY(J)**2
            HKDD(I,J,K)=(HK(I,J,K+1)+HK(I,J,K-1))*DZ(K)**2

            MYTGENE(I,J,K)=(MYT(I+1,J,K)-MYT(I-1,J,K))*0.50*GX(I)
            MYTEENE(I,J,K)=(MYT(I,J+1,K)-MYT(I,J-1,K))*0.50*EY(J)
            MYTDENE(I,J,K)=(MYT(I,J,K+1)-MYT(I,J,K-1))*0.50*DZ(K)

 150    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!********** OKUYUKI & TEMAE NO JYOUKEN **********!

            !DO 700 J=1,JMAX+3
            !DO 700 I=1,IMAX+4

            !    !NO-SLIP
            !    !WALL ?øΩÃà íu?øΩ…ãÔøΩ?øΩ?øΩ?øΩI?øΩ?øΩ0?øΩ?øΩ^?øΩ?øΩ?øΩ?øΩ
            !    HKDENE(I,J,3)=(HK(I,J,4)-0.0)*DZ(K)/1.500
            !    HKDD(I,J,3)=(HK(I,J,4)+0.0)*DZ(K)**2
            !    MYTDENE(I,J,3)=(MYT(I,J,4)-0.0)*DZ(K)/1.500
            !
            !    HKDENE(I,J,KMAX+2)=(0.0-HK(I,J,KMAX+1))*DZ(K)/1.500
            !    HKDD(I,J,KMAX+2)=(0.0+HK(I,J,KMAX+1))*DZ(K)**2
            !    MYTDENE(I,J,KMAX+2)=(0.0-MYT(I,J,KMAX+1))*DZ(K)/1.500

! 700  CONTINUE

            IF(pier.EQ.1.0) THEN
!$OMP PARALLEL
!$OMP DO
        DO 1780 I=3,IMAX+2
        DO 1780 J=3,JMAX+1
        DO 1780 K=Kpier_R,Kpier_L

            IF(I.EQ.Ipier_U-1) THEN
                !SLIP CONDITION
                !HK(I+1,J,K)=HK(I,J,K)
                !MYT(I+1,J,K)=CS*ABS(HK(I+1,J,K))**0.50*DET(I+1,J,K)

                !NO-SLIP CONDITION
                !HK(I+1,J,K)=-HK(I,J,K)  !sumo260220
                !MYT(I+1,J,K)=-MYT(I,J,K)  !sumo260220
                !HKGENE(I,J,K)=(HK(I+1,J,K)-HK(I-1,J,K))*0.50*GX(I)
                !HKGG(I,J,K)=(HK(I+1,J,K)+HK(I-1,J,K))*GX(I)**2
                !MYTGENE(I,J,K)=(MYT(I+1,J,K)-MYT(I-1,J,K))*0.50*GX(I)
                
                !WALL ?øΩÃà íu?øΩ…ãÔøΩ?øΩ?øΩ?øΩI?øΩ?øΩ0?øΩ?øΩ^?øΩ?øΩ?øΩ?øΩ
                HKGENE(I,J,K)=(0.0-HK(I-1,J,K))*GX(I)/1.500
                HKGG(I,J,K)=(0.0+HK(I-1,J,K))*GX(I)**2
                MYTGENE(I,J,K)=(0.0-MYT(I-1,J,K))*GX(I)/1.500
            END IF
            IF(I.EQ.Ipier_D+1) THEN
                !SLIP CONDITION
                !HK(I-1,J,K)=HK(I,J,K)
                !MYT(I-1,J,K)=CS*ABS(HK(I-1,J,K))**0.50*DET(I-1,J,K)
                
                !NO-SLIP CONDITION
                !HK(I-1,J,K)=-HK(I,J,K)  !sumo260220
                !MYT(I-1,J,K)=-MYT(I,J,K)
                !HKGENE(I,J,K)=(HK(I+1,J,K)-HK(I-1,J,K))*0.50*GX(I)
                !HKGG(I,J,K)=(HK(I+1,J,K)+HK(I-1,J,K))*GX(I)**2
                !MYTGENE(I,J,K)=(MYT(I+1,J,K)-MYT(I-1,J,K))*0.50*GX(I)

                !WALL ?øΩÃà íu?øΩ…ãÔøΩ?øΩ?øΩ?øΩI?øΩ?øΩ0?øΩ?øΩ^?øΩ?øΩ?øΩ?øΩ
                HKGENE(I,J,K)=(HK(I+1,J,K)-0.0)*GX(I)/1.500
                HKGG(I,J,K)=(HK(I+1,J,K)+0.0)*GX(I)**2
                MYTGENE(I,J,K)=(MYT(I+1,J,K)-0.0)*GX(I)/1.500
            END IF
 1780   CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 1790 K=3,KMAX+2
        DO 1790 J=3,JMAX+1
        DO 1790 I=Ipier_U,Ipier_D

            IF(K.EQ.Kpier_R-1) THEN
                !SLIP CONDITION
                !HK(I,J,K+1)=HK(I,J,K)
                !MYT(I,J,K+1)=CS*ABS(HK(I,J,K+1))**0.50*DET(I,J,K+1)
                  
                !NO-SLIP CONDITION
                !HK(I,J,K+1)=-HK(I,J,K)  !sumo260220
                !MYT(I,J,K+1)=-MYT(I,J,K)
                !HKDENE(I,J,K)=(HK(I,J,K+1)-HK(I,J,K-1))*0.50*DZ(K)
                !HKDD(I,J,K)=(HK(I,J,K+1)+HK(I,J,K-1))*DZ(K)**2
                !MYTDENE(I,J,K)=(MYT(I,J,K+1)-MYT(I,J,K-1))*0.50*DZ(K)
                
                !WALL ?øΩÃà íu?øΩ…ãÔøΩ?øΩ?øΩ?øΩI?øΩ?øΩ0?øΩ?øΩ^?øΩ?øΩ?øΩ?øΩ
                HKDENE(I,J,K)=(0.0-HK(I,J,K-1))*DZ(K)/1.500
                HKDD(I,J,K)=(0.0+HK(I,J,K-1))*DZ(K)**2
                MYTDENE(I,J,K)=(0.0-MYT(I,J,K-1))*DZ(K)/1.500
            END IF
            IF(K.EQ.Kpier_L+1) THEN
                !SLIP CONDITION
                !HK(I,J,K-1)=HK(I,J,K)
                !MYT(I,J,K-1)=CS*ABS(HK(I,J,K-1))**0.50*DET(I,J,K-1)

                !NO-SLIP CONDITION
                !HK(I,J,K-1)=-HK(I,J,K)  !sumo260220
                !MYT(I,J,K-1)=-MYT(I,J,K)
                !HKDENE(I,J,K)=(HK(I,J,K+1)-HK(I,J,K-1))*0.50*DZ(K)
                !HKDD(I,J,K)=(HK(I,J,K+1)+HK(I,J,K-1))*DZ(K)**2
                !MYTDENE(I,J,K)=(MYT(I,J,K+1)-MYT(I,J,K-1))*0.50*DZ(K)

                !WALL ?øΩÃà íu?øΩ…ãÔøΩ?øΩ?øΩ?øΩI?øΩ?øΩ0?øΩ?øΩ^?øΩ?øΩ?øΩ?øΩ
                HKDENE(I,J,K)=(HK(I,J,K+1)-0.0)*DZ(K)/1.500
                HKDD(I,J,K)=(HK(I,J,K+1)+0.0)*DZ(K)**2
                MYTDENE(I,J,K)=(MYT(I,J,K+1)-0.0)*DZ(K)/1.500
            END IF

 1790   CONTINUE
!$OMP END DO
!$OMP END PARALLEL
            END IF

!$OMP PARALLEL
!$OMP DO

        DO 170 K=3,KMAX+2
        DO 170 I=3,IMAX+2
        DO 170 J=3,JMAX+1

            UCK(I,J,K)=-MYT(I,J,K)*CKG(I,J,K)/1.0
            VCK(I,J,K)=-MYT(I,J,K)*CKE(I,J,K)/1.0
            WCK(I,J,K)=-MYT(I,J,K)*CKD(I,J,K)/1.0

 170    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!C------------   iryukou  ------------------

!$OMP PARALLEL
!$OMP DO
        DO 200 K=3,KMAX+2
        DO 200 I=3,IMAX+2
        DO 200 J=3,JMAX+1

!CCC        HK0(I,J,K)=UK(I,J,K)*HKGENE(I,J,K)+VK(I,J,K)*HKEENE(I,J,K)
!CCCCC        HK0(I,J,K)=UK(I,J,K)*HKGENE(I,J,K)+VK(I,J,K)*HKEENE(I,J,K)
!CCCCC     &            -ABS(UK(I,J,K))*HKGG(I,J,K)/(2.0*GX(I))
!CCCCC     &            -ABS(VK(I,J,K))*HKEE(I,J,K)/(2.0*EY(J))
!CCCCC        AK0(I,J,K)=ABS(UK(I,J,K))*GX(I)+ABS(VK(I,J,K))*EY(J)

            HK0(I,J,K)=UK(I,J,K)*HKGENE(I,J,K)+VK(I,J,K)*HKEENE(I,J,K) &
                      +WK(I,J,K)*HKDENE(I,J,K) &
                      -ABS(UK(I,J,K))*HKGG(I,J,K)/(2.0*GX(I)) &
                      -ABS(VK(I,J,K))*HKEE(I,J,K)/(2.0*EY(J)) &
                      -ABS(WK(I,J,K))*HKDD(I,J,K)/(2.0*DZ(K))

            AK0(I,J,K)=ABS(UK(I,J,K))*GX(I)+ABS(VK(I,J,K))*EY(J) &
                      +ABS(WK(I,J,K))*DZ(K)

 200    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!C------------   KAKUSAN ---------------------

!$OMP PARALLEL
!$OMP DO
            DO 250 K=3,KMAX+2
            DO 250 I=3,IMAX+2
            DO 250 J=3,JMAX+1

                HKN(I,J,K)=MYT(I,J,K)*(HKGG(I,J,K)+HKEE(I,J,K)+HKDD(I,J,K) &
                          +HKGENE(I,J,K)*GXX(I)/GX(I)+HKEENE(I,J,K)*EYY(J)/EY(J) &
                          +HKDENE(I,J,K)*DZZ(K)/DZ(K)) &
                          +1.0*(HKGENE(I,J,K)*MYTGENE(I,J,K) &
                               +HKEENE(I,J,K)*MYTEENE(I,J,K) &
                               +HKDENE(I,J,K)*MYTDENE(I,J,K))

 250    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!C------------    production  ------------------

!$OMP PARALLEL
!$OMP DO
            DO 300 K=3,KMAX+2
            DO 300 I=3,IMAX+2
            DO 300 J=3,JMAX+1

!C        HKP(I,J,K)=MYT(I,J,K)
!C     &            *(2.0*(UKG(I,J,K)**2+VKE(I,J,K)**2)
!C     &             +(UKE(I,J,K)+VKG(I,J,K))**2)
!C     &            -2.0/3.0*HK(I,J,K)*(UKG(I,J,K)+VKE(I,J,K))

                HKP(I,J,K)=MYT(I,J,K) &
                        *(2.0*(UKG(I,J,K)**2+VKE(I,J,K)**2+WKD(I,J,K)**2) &
                             +(UKE(I,J,K)+VKG(I,J,K))**2 &
                             +(UKD(I,J,K)+WKG(I,J,K))**2 &
                             +(WKE(I,J,K)+VKD(I,J,K))**2) &
                        -2.0/3.0*HK(I,J,K)*(UKG(I,J,K)+VKE(I,J,K)+WKD(I,J,K))

 300    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!C------------    Drag Force  ------------------

!$OMP PARALLEL
!$OMP DO
            DO 320 K=3,KMAX+2
            DO 320 J=3,JMAX+1
            DO 320 I=3,IMAX+2

                HTEI(I,J,K)=TEI1(I,J,K)*(UUSRK(I,J,K)+VVSRK(I,J,K)+WWSRK(I,J,K))
                HTEP(I,J,K)=arfa*TEI1(I,J,K) &
                            *((ABS(USK(I,J,K)-UK(I,J,K)))**2 &
                             +(ABS(VSK(I,J,K)-VK(I,J,K)))**2 &
                             +(ABS(WSK(I,J,K)-WK(I,J,K)))**2)
                HAMF(I,J,K)=ABS(CK(I,J,K)/(1.0-CK(I,J,K))) &
                            *( CM0*(0.50*(UUSRK(I,J,K)-UUSRK1(I,J,K))/DT &
                                   +0.50*(VVSRK(I,J,K)-VVSRK1(I,J,K))/DT &
                                  +0.50*(WWSRK(I,J,K)-WWSRK1(I,J,K))/DT) &
                            -(1.0+CM0)*(HK(I,J,K)-HK1(I,J,K))/DT)

 320    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!C--------------  KEISU NO KEISAN  -------------------

!$OMP PARALLEL
!$OMP DO
        DO 350 K=3,KMAX+2
        DO 350 I=3,IMAX+2
        DO 350 J=3,JMAX+1

!C         DK(I,J,K)=DT*(2.0*MYT(I,J,K)*(GX(I)**2+EY(J)**2+DZ(K)**2)
!C     &                +AK0(I,J,K))

            DK(I,J,K)=DT*(2.0*MYT(I,J,K)*(GX(I)**2+EY(J)**2+DZ(K)**2) &
                        +AK0(I,J,K)+2.0*TEI1(I,J,K))
            IF(K.EQ.3.OR.K.EQ.KMAX+2) THEN
                DK(I,J,K)=DT*(1.500*MYT(I,J,K)*DZ(K)**2+ &
                              2.000*MYT(I,J,K)*(EY(J)**2+GX(I)**2) &
                              +AK0(I,J,K)+2.0*TEI1(I,J,K))
            END IF

 350    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

IF(pier.EQ.1.0) THEN
!$OMP PARALLEL
!$OMP DO
        DO 1781 I=3,IMAX+2
        DO 1781 J=3,JMAX+1
        DO 1781 K=Kpier_R,Kpier_L

            IF(I.EQ.Ipier_U-1) THEN               
                !WALL ?øΩÃà íu?øΩ…ãÔøΩ?øΩ?øΩ?øΩI?øΩ?øΩ0?øΩ?øΩ^?øΩ?øΩ?øΩ?øΩ
                DK(I,J,K)=DT*(1.500*MYT(I,J,K)*GX(I)**2+ &
                              2.000*MYT(I,J,K)*(EY(J)**2+DZ(K)**2) &
                              +AK0(I,J,K)+2.0*TEI1(I,J,K))
            END IF
            IF(I.EQ.Ipier_D+1) THEN
                !WALL ?øΩÃà íu?øΩ…ãÔøΩ?øΩ?øΩ?øΩI?øΩ?øΩ0?øΩ?øΩ^?øΩ?øΩ?øΩ?øΩ
                DK(I,J,K)=DT*(1.500*MYT(I,J,K)*GX(I)**2+ &
                              2.000*MYT(I,J,K)*(EY(J)**2+DZ(K)**2) &
                              +AK0(I,J,K)+2.0*TEI1(I,J,K))
            END IF
 1781   CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 1791 K=3,KMAX+2
        DO 1791 J=3,JMAX+1
        DO 1791 I=Ipier_U,Ipier_D

            IF(K.EQ.Kpier_R-1) THEN
                !WALL ?øΩÃà íu?øΩ…ãÔøΩ?øΩ?øΩ?øΩI?øΩ?øΩ0?øΩ?øΩ^?øΩ?øΩ?øΩ?øΩ
                DK(I,J,K)=DT*(1.500*MYT(I,J,K)*DZ(K)**2+ &
                              2.000*MYT(I,J,K)*(EY(J)**2+GX(I)**2) &
                              +AK0(I,J,K)+2.0*TEI1(I,J,K))
            END IF
            IF(K.EQ.Kpier_L+1) THEN
                !WALL ?øΩÃà íu?øΩ…ãÔøΩ?øΩ?øΩ?øΩI?øΩ?øΩ0?øΩ?øΩ^?øΩ?øΩ?øΩ?øΩ
                DK(I,J,K)=DT*(1.500*MYT(I,J,K)*DZ(K)**2+ &
                              2.000*MYT(I,J,K)*(EY(J)**2+GX(I)**2) &
                              +AK0(I,J,K)+2.0*TEI1(I,J,K))
            END IF

 1791   CONTINUE
!$OMP END DO
!$OMP END PARALLEL
            END IF

!C-------------------RESIDURE ----------------------------------

!$OMP PARALLEL
!$OMP DO
        DO 400 K=3,KMAX+2
        DO 400 I=3,IMAX+2
        DO 400 J=3,JMAX+1

!C         SK(I,J,K)=(HK1(I,J,K)+DT*(-HK0(I,J,K)+HKN(I,J,K)+HKP(I,J,K)
!C     &             +HTEI(I,J,K)+HAMF(I,J,K)-EK(I,J,K)))
!C     &             /(1.0+DK(I,J,K))-HK(I,J,K)

            SK(I,J,K)=(HK1(I,J,K)+DT*(-HK0(I,J,K)+HKN(I,J,K)+HKP(I,J,K) &
                     +HTEP(I,J,K)+HTEI(I,J,K)+HAMF(I,J,K)*0.0-EK(I,J,K))) &
                     /(1.0+DK(I,J,K))-HK(I,J,K)

 400    CONTINUE

!$OMP END DO
!$OMP END PARALLEL

!!$OMP PARALLEL
!!$OMP DO
!        DO 178 I=3,IMAX+2
!        DO 178 J=3,JMAX+1
!        DO 178 K=Kpier_R,Kpier_L
!!
!            IF(pier.EQ.1.0) THEN
!                IF(I.EQ.Ipier_U-1) THEN
!                    HK(Ipier_U,J,K)=HK(Ipier_U-1,J,K)
!                    SK(I,J,K)=(HK1(I,J,K)+DT*(-HK0(I,J,K)+HKN(I,J,K)+HKP(I,J,K) &
!                             +HTEP(I,J,K)+HTEI(I,J,K)+HAMF(I,J,K)*0.0-EK(I,J,K))) &
!                             /(1.0+DK(I,J,K))-HK(I,J,K)
!                END IF
!                IF(I.EQ.Ipier_D+1) THEN
!                    HK(Ipier_D,J,K)=HK(Ipier_D+1,J,K)
!                    SK(I,J,K)=(HK1(I,J,K)+DT*(-HK0(I,J,K)+HKN(I,J,K)+HKP(I,J,K) &
!                             +HTEP(I,J,K)+HTEI(I,J,K)+HAMF(I,J,K)*0.0-EK(I,J,K))) &
!                             /(1.0+DK(I,J,K))-HK(I,J,K)
!                END IF
!            END IF
! 178    CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL
!!
!!$OMP PARALLEL
!!$OMP DO
!        DO 179 K=3,KMAX+2
!        DO 179 J=3,JMAX+1
!        DO 179 I=Ipier_U,Ipier_D
!!
!            IF(pier.EQ.1.0) THEN
!                IF(K.EQ.Kpier_R-1) THEN
!                    HK(I,J,Kpier_R)=HK(I,J,Kpier_R-1)
!                    SK(I,J,K)=(HK1(I,J,K)+DT*(-HK0(I,J,K)+HKN(I,J,K)+HKP(I,J,K) &
!                             +HTEP(I,J,K)+HTEI(I,J,K)+HAMF(I,J,K)*0.0-EK(I,J,K))) &
!                             /(1.0+DK(I,J,K))-HK(I,J,K)
!                END IF
!                IF(K.EQ.Kpier_L+1) THEN
!                    HK(I,J,Kpier_L)=HK(I,J,Kpier_L+1)
!                    SK(I,J,K)=(HK1(I,J,K)+DT*(-HK0(I,J,K)+HKN(I,J,K)+HKP(I,J,K) &
!                             +HTEP(I,J,K)+HTEI(I,J,K)+HAMF(I,J,K)*0.0-EK(I,J,K))) &
!                             /(1.0+DK(I,J,K))-HK(I,J,K)
!                END IF
!            END IF
!!
! 179    CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL

!CCCCCC////// Surface Boundary Condition ////////CCCCCC

!$OMP PARALLEL
!$OMP DO
        DO 532 I=3,IMAX+2
        DO 532 K=3,KMAX+2
        DO 532 J=IETA(I,K),JMAX+1

            IF(DEPMODE.EQ.1.0) THEN
                SK(I,J,K)=0.0
            END IF

 532    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!!$OMP PARALLEL
!!$OMP DO
        !DO 534 I=3,IMAX+2
        !O 534 K=3,KMAX+2
        !DO 534 J=3,IGL(I,K)

            !SK(I,J,K)=0.0

 534    CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL

!CCCCCC////// Pier Boundary Condition ////////CCCCCC

        IF(pier.EQ.1.0) THEN
!$OMP PARALLEL
!$OMP DO
            DO 702 J=1,JMAX+3
            DO 702 K=Kpier_R,Kpier_L
            DO 702 I=Ipier_U,Ipier_D

                SK(I,J,K)=0.0

 702    CONTINUE
!$OMP END DO
!$OMP END PARALLEL
        !END IF
        !IF(pier.EQ.1.0) THEN
!!$OMP PARALLEL
!!$OMP DO
            !DO 7022 J=1,JMAX+3
            !DO 7022 K=Kpier_R1,Kpier_L1
            !DO 7022 I=Ipier_U1,Ipier_D1

                !SK(I,J,K)=0.0

 7022   CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL
        !END IF
        !IF(pier.EQ.1.0) THEN
!!$OMP PARALLEL
!!$OMP DO
            !DO 7023 J=1,JMAX+3
            !DO 7023 K=Kpier_R2,Kpier_L2
            !DO 7023 I=Ipier_U2,Ipier_D2

                !SK(I,J,K)=0.0

 7023   CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL
        !END IF
        !IF(pier.EQ.1.0) THEN
!!$OMP PARALLEL
!!$OMP DO
            !DO 7024 J=1,JMAX+3
            !DO 7024 K=Kpier_R3,Kpier_L3
            !DO 7024 I=Ipier_U3,Ipier_D3

                !SK(I,J,K)=0.0

 7024   CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL
        !END IF
        !IF(pier.EQ.1.0) THEN
!!$OMP PARALLEL
!!$OMP DO
            !DO 7025 J=1,JMAX+3
            !DO 7025 K=Kpier_R4,Kpier_L4
            !DO 7025 I=Ipier_U4,Ipier_D4

                !SK(I,J,K)=0.0

 7025   CONTINUE
!!$OMP END DO
!!OMP END PARALLEL
        END IF

        DO 450 K=3,KMAX+2
        DO 450 I=3,IMAX+2
        DO 450 J=3,JMAX+1

            HK(I,J,K)=HK(I,J,K)+CONST2*SK(I,J,K)

!C     Smagorinsky Model !

        SK(I,J,K)=0.0
        HK(I,J,K)=0.0
        MYT(I,J,K)=(0.1*DET(I,J,K))**2.0 &
             *(2.0*( (2.0*UKG(I,J,K))**2+(2.0*VKE(I,J,K))**2 &
                    +(2.0*WKD(I,J,K))**2+(UKE(I,J,K)+VKG(I,J,K))**2 &
                    +(UKD(I,J,K)+WKG(I,J,K))**2 &
                    +(VKD(I,J,K)+WKE(I,J,K))**2 ))**0.50

 450    CONTINUE

        DO 500 K=3,KMAX+2
        DO 500 I=3,IMAX+2
        DO 500 J=IGL(I,K)+1,JMAX+1

            GOSAE=GOSAE+ABS(SK(I,J,K))

 500    CONTINUE

        IF(GOSAE.LT.EPS4) GO TO 5000

 1000   CONTINUE
 5000   CONTINUE

!$OMP PARALLEL
!$OMP DO
        DO 1500 K=1,KMAX+4
        DO 1500 I=1,IMAX+4
        DO 1500 J=1,JMAX+3

            IF(HK(I,J,K).LT.0.0) THEN
                HK(I,J,K)=0.0
            END IF

 1500   CONTINUE
!$OMP END DO
!$OMP END PARALLEL

       IF (naka.EQ.0.0) THEN
       LOOP=LOOP+ITC*40
       WRITE(*,90) LOOP,GOSAE
  90   FORMAT(' LOOP(E)=',I3,2X,'GOSAE=',F13.10)
       END IF

    deallocate (HKGENE,HKEENE,HKGG,HKEE,HK0,HKN &
     ,DK,SK,CK &
     ,HTEI,TEI1,CKG,CKE,UUSRK,VVSRK,WWSRK,WWSRK1 &
     ,UCK,VCK,HAMF,UUSRK1,VVSRK1 &
     ,UKD,VKD,WK,WSK,WKG,WKE,WKD,CKD,HKDENE,HKDD &
     ,MYTDENE,WCK,HTEP,HKP,USK,VSK,EK,WDF1,WDF2,WDF3,WDF4,WDF5,WDF6,FW &
     ,FW1,FW2,FW3,FW4,FW5,FW6,DETN)

    RETURN
    END

!C*****************************************************************
      SUBROUTINE SALINITY
!C*****************************************************************
      USE MF_SF
      USE OMP_LIB
      IMPLICIT NONE
      
!
      allocate ( &
      SALG(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),SALE(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,SALGG(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),SALEE(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,SA0(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),SAN(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,DKS(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),SKS(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,SALAG(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),SALAE(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,SALAGG(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),SALAEE(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,SALG1(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),SALE1(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,SALGG1(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),SALEE1(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,STAT = ALLOC_ERR )
!
!C
!C      PARAMETER(NX=200,NY=30,NZ=100,PHI=3.14159)
!C           IMPLICIT REAL  (A-H,M,O-Z)
!C
!C      COMMON /COOR1/  GX(NX+4),GXX(NX+4)
!C      COMMON /COOR2/  EY(NY+4),EYY(NY+4)
!C      COMMON /COOR3/  DZ(NZ+4),DZZ(NZ+4)
!C      COMMON /COOR4/  X(NX+4,NY+4,NZ+4),Y(NX+4,NY+4,NZ+4)
!C      COMMON /COOR5/  Z(NX+4,NY+4,NZ+4)
!C      COMMON /UVW1/   U(NX+4,NY+4,NZ+4),U1(NX+4,NY+4,NZ+4)
!C      COMMON /UVW2/   V(NX+4,NY+4,NZ+4),V1(NX+4,NY+4,NZ+4)
!C      COMMON /UVW3/   W(NX+4,NY+4,NZ+4),W1(NX+4,NY+4,NZ+4)
!C      COMMON /UVW4/   US(NX+4,NY+4,NZ+4),US1(NX+4,NY+4,NZ+4)
!C      COMMON /UVW5/   VS(NX+4,NY+4,NZ+4),VS1(NX+4,NY+4,NZ+4)
!C      COMMON /UVW6/   WS(NX+4,NY+4,NZ+4),WS1(NX+4,NY+4,NZ+4)
!C      COMMON /UVW7/   U2(NX+4,NY+4,NZ+4),US2(NX+4,NY+4,NZ+4)
!C      COMMON /UVW8/   V2(NX+4,NY+4,NZ+4),VS2(NX+4,NY+4,NZ+4)
!C      COMMON /UVW9/   W2(NX+4,NY+4,NZ+4),WS2(NX+4,NY+4,NZ+4)
!C      COMMON /ENE1/   HK(NX+4,NY+4,NZ+4),HK1(NX+4,NY+4,NZ+4)
!C      COMMON /ENE2/   MYT(NX+4,NY+4,NZ+4),RE1(NX+4,NY+4,NZ+4)
!C      COMMON /ENE3/   DET(NX+4,NY+4,NZ+4),P(NX+4,NY+4,NZ+4)
!C      COMMON /DIS1/   C(NX+4,NY+4,NZ+4),C1(NX+4,NY+4,NZ+4)
!C      COMMON /DIS2/   USR(NX+4,NY+4,NZ+4),USR1(NX+4,NY+4,NZ+4)
!C      COMMON /DIS3/   VSR(NX+4,NY+4,NZ+4),VSR1(NX+4,NY+4,NZ+4)
!C      COMMON /DIS4/   WSR(NX+4,NY+4,NZ+4),WSR1(NX+4,NY+4,NZ+4)
!C      COMMON /DIS5/   UUSR(NX+4,NY+4,NZ+4),UUSR1(NX+4,NY+4,NZ+4)
!C      COMMON /DIG6/   VVSR(NX+4,NY+4,NZ+4),VVSR1(NX+4,NY+4,NZ+4)
!C      COMMON /DIG7/   WWSR(NX+4,NY+4,NZ+4),WWSR1(NX+4,NY+4,NZ+4)
!C      COMMON /REA1/   XM(NX+4,NY+4,NZ+4),YM(NX+4,NY+4,NZ+4)
!C      COMMON /REA2/   XG1(NX+4,NY+4,NZ+4),YG(NX+4,NY+4,NZ+4)
!C      COMMON /REA3/   ZG(NX+4,NY+4,NZ+4),ZM(NX+4,NY+4,NZ+4)
!C      COMMON /REA4/   CM(NX+4,NY+4,NZ+4)
!C      COMMON /DENS1/  ROU(NX+4,NY+4,NZ+4),ROUA(NX+4,NY+4,NZ+4)
!C      COMMON /DENS2/  SAL(NX+4,NY+4,NZ+4),SAL1(NX+4,NY+4,NZ+4)
!C      COMMON /DENS3/  ROU1(NX+4,NY+4,NZ+4),SALA(NX+4,NY+4,NZ+4)
!C      COMMON /MESH/  IMAX,JMAX,KMAX
!C      COMMON /DTIME/ DT,TIME,NMAX,NWRT,N1,N
!C      COMMON /GOSA/  GOSAC,GOSAP,GOSAQ,GOSAR,GOSAE
!C      COMMON /CONST/ RED,AL,CONST1,CONST2,CONST3,EPS1,EPS2,EPS3,EPS4
!C      COMMON /LOP1 / ITC,ITC1
!C      COMMON /NAMI2/ naka
!C      COMMON /CONT2/  DR,RC,RS,GG,CL0,htp,CM0
!C      COMMON /FACT/  FACT1,FACT2,FACT3,FACT4,FACT5,SS,G,D50,CMAX
!C      COMMON /NAMI/  h,h0,A,HHK,SG,WC
!C      COMMON /LOP2 / CS,CE,arfa,CAR,Z0,AS,AC0
!C      COMMON /tori/  UUU,MYU0
!C      DIMENSION SALG(NX+4,NY+4,NZ+4),SALE(NX+4,NY+4,NZ+4)
!C     &         ,SALGG(NX+4,NY+4,NZ+4),SALEE(NX+4,NY+4,NZ+4)
!C     &         ,MYTG(NX+4,NY+4,NZ+4),MYTE(NX+4,NY+4,NZ+4)
!C     &         ,UKG(NX+4,NY+4,NZ+4),UKE(NX+4,NY+4,NZ+4)
!C     &         ,VKG(NX+4,NY+4,NZ+4),VKE(NX+4,NY+4,NZ+4)
!C     &         ,SA0(NX+4,NY+4,NZ+4),SAN(NX+4,NY+4,NZ+4)
!C     &         ,DKS(NX+4,NY+4,NZ+4),AK0(NX+4,NY+4,NZ+4)
!C     &         ,SKS(NX+4,NY+4,NZ+4)
!C     &         ,SALAG(NX+4,NY+4,NZ+4),SALAE(NX+4,NY+4,NZ+4)
!C     &         ,SALAGG(NX+4,NY+4,NZ+4),SALAEE(NX+4,NY+4,NZ+4)
!C     &         ,SALG1(NX+4,NY+4,NZ+4),SALE1(NX+4,NY+4,NZ+4)
!C     &         ,SALGG1(NX+4,NY+4,NZ+4),SALEE1(NX+4,NY+4,NZ+4)
!C     &         ,UK(NX+4,NY+4,NZ+4),VK(NX+4,NY+4,NZ+4)
!C
!C******************************************************************
!C******************************************************************
!C
                   K=1
                   SGSA=1.0
!C
                   DO 100 I=2,IMAX+3
                   DO 100 J=2,JMAX+2
                   DO 100 K=3,KMAX+2
!C
         UK(I,J,K)=U(I,J,K)
         VK(I,J,K)=V(I,J,K)
         MYT(I,J,K)=MYU0
 100               CONTINUE
!C
                   DO 85 I=2,IMAX+3
                   DO 85 J=2,JMAX+2
                   DO 85 K=3,KMAX+2
         UKG(I,J,K)=(UK(I+1,J,K)-UK(I-1,J,K))*0.50*GX(I)
         UKE(I,J,K)=(UK(I,J+1,K)-UK(I,J-1,K))*0.50*EY(J)
!C
         VKG(I,J,K)=(VK(I+1,J,K)-VK(I-1,J,K))*0.50*GX(I)
         VKE(I,J,K)=(VK(I,J+1,K)-VK(I,J-1,K))*0.50*EY(J)
!C
 85               CONTINUE
!C
!C
!C***************                            **************
!C***************    STARTING OF ITERATION   **************
!C***************                            **************
!C
                   DO 1000 LOOP=1,100
                   GOSAS=0.0
!C
!C
!C**************   BOUNDARY  CONDITION   *******************
!C
!C ---------------- CEILING CONDITION ----------------------
!C
                   DO 900 I=1,IMAX+4
      SAL(I,JMAX+2,K)=SAL(I,JMAX+1,K)
      SAL(I,JMAX+3,K)=SAL(I,JMAX+2,K)
      SAL1(I,JMAX+2,K)=SAL1(I,JMAX+1,K)
      SAL1(I,JMAX+3,K)=SAL1(I,JMAX+2,K)
!C      SAL(I,JMAX+3,K)=0.0
!C      SAL(I,JMAX+2,K)=0.0
 900              CONTINUE
!C
!C                   DO 870 I=21,24
!C      SAL(I,JMAX+3,K)=0.0010
!C      SAL(I,JMAX+2,K)=0.0010
!C 870              CONTINUE
!C
!C ---------- GROUND CONDITION --------------
!C
                   DO 850 I=1,IMAX+4
!C      SAL(I,2,K)=0.0
!C      SAL(I,1,K)=0.0
      SAL(I,2,K)=SAL(I,3,K)
      SAL(I,1,K)=SAL(I,3,K)
!C      SAL(I,2,K)=0.10
!C      SAL(I,1,K)=0.10
!C      SAL1(I,2,K)=0.10
!C      SAL1(I,1,K)=0.10
  850              CONTINUE
!C
!C ---------- KARYUUGAWA NO JYOUKEN ------------
!C
                   DO 800 J=1,JMAX+3
      SAL(IMAX+3,J,K)=SAL(IMAX+2,J,K)
      SAL(IMAX+4,J,K)=SAL(IMAX+2,J,K)
!C      SAL(IMAX+3,J,K)=0.0
!C      SAL(IMAX+4,J,K)=0.0
  800              CONTINUE
!C----------- JYOURYUUGAWA NO JYOUKEN -------------
!C
                   DO 750 J=1,JMAX+3
      SAL(1,J,K)=SAL(3,J,K)
      SAL(2,J,K)=SAL(3,J,K)
!C      SAL(2,J,K)=0.10
!C      SAL(1,J,K)=0.10
!C      SAL1(2,J,K)=0.10
!C      SAL1(1,J,K)=0.10
  750               CONTINUE
!C
!C****************   NAITEN NO KEISAN  ******************
!C
!C
                   DO 150 I=3,IMAX+2
                   DO 150 J=3,JMAX+1
!C                   DO 150 K=3,KMAX+2
!C
         SALG(I,J,K)=(SAL(I+1,J,K)-SAL(I-1,J,K))*0.50*GX(I)
         SALE(I,J,K)=(SAL(I,J+1,K)-SAL(I,J-1,K))*0.50*EY(J)
         SALGG(I,J,K)=(SAL(I+1,J,K)+SAL(I-1,J,K))*GX(I)**2
         SALEE(I,J,K)=(SAL(I,J+1,K)+SAL(I,J-1,K))*EY(J)**2
!C
         SALG1(I,J,K)=(SAL1(I+1,J,K)-SAL1(I-1,J,K))*0.50*GX(I)
         SALE1(I,J,K)=(SAL1(I,J+1,K)-SAL1(I,J-1,K))*0.50*EY(J)
!
         SALGG1(I,J,K)=(SAL1(I+1,J,K)-2.0*SAL1(I,J,K) &
     +SAL1(I-1,J,K))*GX(I)**2
!
         SALEE1(I,J,K)=(SAL1(I,J+1,K)-2.0*SAL1(I,J,K) &
     +SAL1(I,J-1,K))*EY(J)**2
!C
         SALAG(I,J,K)=(SALA(I+1,J,K)-SALA(I-1,J,K))*0.50*GX(I)
         SALAE(I,J,K)=(SALA(I,J+1,K)-SALA(I,J-1,K))*0.50*EY(J)
!
         SALAGG(I,J,K)=(SALA(I+1,J,K)-2.0*SALA(I,J,K) &
     +SALA(I-1,J,K))*GX(I)**2
!
         SALAEE(I,J,K)=(SALA(I,J+1,K)-2.0*SALA(I,J,K) &
     +SALA(I,J-1,K))*EY(J)**2
!C
         MYTGENE(I,J,K)=(MYT(I+1,J,K)-MYT(I-1,J,K))*0.50*GX(I)
         MYTEENE(I,J,K)=(MYT(I,J+1,K)-MYT(I,J-1,K))*0.50*EY(J)
!C
 150               CONTINUE
!C
!C------------   iryukou  ------------------
!C
                   DO 200 I=3,IMAX+2
                   DO 200 J=3,JMAX+1
!C                   DO 200 K=3,KMAX+2
!C
!C        SA0(I,J,K)=UK(I,J,K)*(SALG(I,J,K)+SALAG(I,J,K))
!C     &            +VK(I,J,K)*(SALE(I,J,K)+SALAE(I,J,K))
!C     &            -ABS(UK(I,J,K))
!C     &             *(SALGG(I,J,K)+SALAGG(I,J,K))/(2.0*GX(I))
!C     &            -ABS(VK(I,J,K))
!C     &             *(SALEE(I,J,K)+SALAEE(I,J,K))/(2.0*EY(J))
!C
!C////    First Order Upwind Difference (Explicit Version)/////
!C
!C        SA0(I,J,K)=VK(I,J,K)*SALE1(I,J,K)
!C     &            -ABS(VK(I,J,K))*SALEE1(I,J,K)/(2.0*EY(J))
!
        SA0(I,J,K)=UK(I,J,K)*SALG1(I,J,K)+VK(I,J,K)*SALE1(I,J,K) &
     -ABS(UK(I,J,K))*SALGG1(I,J,K)/(2.0*GX(I))  &
     -ABS(VK(I,J,K))*SALEE1(I,J,K)/(2.0*EY(J))
!
        AK0(I,J,K)=0.0
!C
!C////    First Order Upwind Difference (Implicit Version)/////
!C
!C        SA0(I,J,K)=UK(I,J,K)*SALG(I,J,K)+VK(I,J,K)*SALE(I,J,K)
!C     &            -ABS(UK(I,J,K))*SALGG(I,J,K)/(2.0*GX(I))
!C     &            -ABS(VK(I,J,K))*SALEE(I,J,K)/(2.0*EY(J))
!C        AK0(I,J,K)=ABS(UK(I,J,K))*GX(I)+ABS(VK(I,J,K))*EY(J)
!C
!C////    First Order Central Difference ///////
!C        SA0(I,J,K)=UK(I,J,K)*SALG(I,J,K)+VK(I,J,K)*SALE(I,J,K)
!C        AK0(I,J,K)=0.0
!C
!C////    Third Order Upwind Difference  ///////
!C
!C      SA0(I,J,K)=1.0/12.0*(-SAL(I,J+2,K)+8.0*(SAL(I,J+1,K)
!C     &              -SAL(I,J-1,K))+SAL(I,J-2,K) )*VK(I,J,K)*EY(J)
!C     &           +0.25*(SAL(I,J+2,K)-4.0*SAL(I,J+1,K)
!C     &         -4.0*SAL(I,J-1,K)+SAL(I,J-2,K))*ABS(VK(I,J,K))*EY(J)
!C      SA0(I,J,K)=0.5*(SAL1(I,J-2,K)-4.0*SAL1(I,J-1,K))
!C     &              *VK(I,J,K)*EY(J)
!C      AK0(I,J,K)=1.5*ABS(VK(I,J,K))*EY(J)
!C
!C
 200               CONTINUE
!C
                   DO 250 I=3,IMAX+2
                   DO 250 J=3,JMAX+1
!C                   DO 250 K=3,KMAX+2
!C
        SAN(I,J,K)=MYT(I,J,K)/SGSA*(SALGG(I,J,K)+SALEE(I,J,K) &
     +SALG(I,J,K)*GXX(I)+SALE(I,J,K)*EYY(J)) &
     +1.0/SGSA*(SALG(I,J,K)*MYTGENE(I,J,K) &
     +SALE(I,J,K)*MYTEENE(I,J,K))
!C
 250               CONTINUE
!C
!C--------------  KEISU NO KEISAN  -------------------
!C
                   DO 350 I=3,IMAX+2
                   DO 350 J=3,JMAX+1
!C                   DO 350 K=3,KMAX+2
!C
         DKS(I,J,K)=DT*(2.0*MYT(I,J,K)/SGSA*(GX(I)**2+EY(J)**2) &
     +AK0(I,J,K))
!C         DKS(I,J,K)=DT*(AK0(I,J,K))
 350               CONTINUE
!C
!C-------------------RESIDURE ----------------------------------
!C
                   DO 400 I=3,IMAX+2
                   DO 400 J=3,JMAX+1
!C                   DO 400 K=3,KMAX+2
!C
         SKS(I,J,K)=(SAL1(I,J,K)+DT*(-SA0(I,J,K)+SAN(I,J,K))) &
     /(1.0+DKS(I,J,K))-SAL(I,J,K)
!C         SKS(I,J,K)=(SAL1(I,J,K)+DT*(-SA0(I,J,K)))
!C     &             /(1.0+DKS(I,J,K))-SAL(I,J,K)
 400               CONTINUE
!C
                   DO 450 J=3,JMAX+1
                   DO 450 I=3,IMAX+2
!C                   DO 450 K=3,KMAX+2
!C
         SAL(I,J,K)=SAL(I,J,K)+CONST2*SKS(I,J,K)
 450               CONTINUE
!C
                   DO 500 I=3,IMAX+2
                   DO 500 J=3,JMAX+1
!C                   DO 500 J=3,JMAX+1
        GOSAS=GOSAS+ABS(SKS(I,J,K))
 500               CONTINUE
!C
!CC        write(*,*) GOSAS
       IF(GOSAS.LT.EPS4) GO TO 5000
!C
 1000            CONTINUE
 5000            CONTINUE
!C
!C
            TOS=0
                   DO 600 I=2,IMAX+2
!CC                   DO 600 I=1,IMAX+4
!C                   DO 600 K=1,KMAX+1
                   DO 610 J=2,JMAX+1
!C           ROU(I,J,K)=RC+SAL(I,J,K)*htp-ROUA(I,J,K)
           ROU(I,J,K)=SAL(I,J,K)*htp
            IF (J.EQ.3) THEN
!C            IF (I.EQ.3) THEN
             TOS=SAL(I,J,K)+TOS
            END IF
           IF (J.EQ.JMAX) THEN
            IF (I.GT.45) THEN
             IF (I.LT.65) THEN
!C            write(*,*)I,SAL(I,J,K),C(I,J,K)
           END IF
            END IF
             END IF
 610              CONTINUE
!C
                  DO 620 J=1,2
            ROU(I,J,K)=ROU(I,3,K)
 620              CONTINUE
!C
                  DO 630 J=JMAX+2,JMAX+3
            ROU(I,J,K)=ROU(I,JMAX+1,K)
 630              CONTINUE
 600              CONTINUE
!C
       IF (naka.EQ.0.0) THEN
       LOOP=LOOP+ITC*10
       WRITE(6,90) LOOP,GOSAS
  90   FORMAT(' LOOP(S)=',I3,2X,'GOSAS=',F13.10)
       WRITE(*,91)TOS
  91   FORMAT(' Total(S)=',F10.5)
       END IF
!C
!    deallocate ( &
!      SALG,SALE &
!     ,SALGG,SALEE &
!     ,SA0,SAN &
!     ,DKS,SKS &
!     ,SALAG,SALAE &
!     ,SALAGG,SALAEE &
!     ,SALG1,SALE1 &
!     ,SALGG1,SALEE1) &
!     ,STAT = ALLOC_ERR )
!C
      RETURN
      END
!
!C*****************************************************************
    SUBROUTINE SMAC
!C*****************************************************************
    USE MF_SF
    USE OMP_LIB
    IMPLICIT NONE

      allocate (SS1(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),PSI1(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,CX(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),CY(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,CXX(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),CYY(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,CZ(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),CZZ(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,UCP(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),RPE(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,PP1(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),PP2(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,PP3(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),PP4(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,PP5(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),PP6(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,ETA_1(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),ETA_2(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,ETA_3(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),ETA_4(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,ETA_5(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),ETA_6(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,STAT = ALLOC_ERR )

!C***********  RIGHT HAND SIDE OF POIS EQUATION  ************

        DDT=1.0/DT
        sigmf=1.0

!$OMP PARALLEL
!$OMP DO
        DO 1400 K=1,KMAX+4
        DO 1400 I=1,IMAX+4
        DO 1400 J=1,JMAX+3

            PSI1(I,J,K)=PSI(I,J,K)

 1400   CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 100 K=1,KMAX+4
        DO 100 I=1,IMAX+4
        DO 100 J=1,JMAX+3

            PSI(I,J,K)=0.0

 100    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 150 K=3,KMAX+2
        DO 150 I=3,IMAX+2
        DO 150 J=3,JMAX+1

            CX(I,J,K)=0.50*(C(I+1,J,K)-C(I-1,J,K))
            CY(I,J,K)=0.50*(C(I,J+1,K)-C(I,J-1,K))
            CZ(I,J,K)=0.50*(C(I,J,K+1)-C(I,J,K-1))

            CXX(I,J,K)=C(I+1,J,K)-2.0*C(I,J,K)+C(I-1,J,K)
            CYY(I,J,K)=C(I,J+1,K)-2.0*C(I,J,K)+C(I,J-1,K)
            CZZ(I,J,K)=C(I,J,K+1)-2.0*C(I,J,K)+C(I,J,K-1)

 150    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

        !DO 1375 K=1,KMAX+4  !sss
        !DO 1375 J=1,JMAX+3

            !UB(2,J,K)=UB(3,J,K)
            !UB(1,J,K)=UB(3,J,K)
            !UB(IMAX+3,J,K)=UB(IMAX+2,J,K)
            !UB(IMAX+4,J,K)=UB(IMAX+2,J,K)

 1375   CONTINUE

        !DO 1376 I=1,IMAX+4  !sss
        !DO 1376 J=1,JMAX+3

            !WB(I,J,2)=0.0
            !WB(I,J,1)=-WB(I,J,3)
            !WB(I,J,KMAX+2)=0.0
            !WB(I,J,KMAX+3)=-WB(I,J,KMAX+1)
            !WB(I,J,KMAX+4)=-WB(I,J,KMAX)

 1376   CONTINUE
 
!!Revised by SUMO241204
!        DO 1598 K=Kpier_R,Kpier_L
!        DO 1598 I=Ipier_U,Ipier_D
!        DO 1598 J=1,JMAX+3

!            UB(Ipier_U-1,J,K)=0.0
!            UB(Ipier_D,J,K)=0.0
!            WB(I,J,Kpier_R-1)=0.0
!            WB(I,J,Kpier_L)=0.0

 1598   CONTINUE

        DO 200 K=3,KMAX+2
        DO 200 I=3,IMAX+2
        DO 200 J=3,JMAX+1

!C      RPE(I,J,K)=((UPP(I+1,J,K)-UPP(I-1,J,K))*0.50*GX(I)
!C     &           +(VPP(I,J+1,K)-VPP(I,J-1,K))*0.50*EY(J))
!C       RPE(I,J,K)=-(0.50*GX(I)*(U(I+1,J,K)-U(I-1,J,K))
!C     &             +0.50*EY(J)*(V(I,J+1,K)-V(I,J-1,K))
!C     &             +0.50*DZ(K)*(W(I,J,K+1)-W(I,J,K-1)) )

            !RPE(I,J,K)=-(0.50*GX(I)*(U(I+1,J,K)*(1.0-C(I+1,J,K)) &
            !                        -U(I-1,J,K)*(1.0-C(I-1,J,K))) &
            !            +0.50*EY(J)*(V(I,J+1,K)*(1.0-C(I,J+1,K)) &
            !                        -V(I,J-1,K)*(1.0-C(I,J-1,K))) &
            !            +0.50*DZ(K)*(W(I,J,K+1)*(1.0-C(I,J,K+1)) &
            !                        -W(I,J,K-1)*(1.0-C(I,J,K-1))) &
            !            +(C(I,J,K)-C1(I,J,K))*DDT)

!Revised by SUMO241017
            !RPE(I,J,K)=-(0.50*GX(I)*(UB(I+1,J,K)*(1.0-C(I+1,J,K)) &
            !                        -UB(I-1,J,K)*(1.0-C(I-1,J,K))) &
            !            +0.50*EY(J)*(VB(I,J+1,K)*(1.0-C(I,J+1,K)) &
            !                        -VB(I,J-1,K)*(1.0-C(I,J-1,K))) &
            !            +0.50*DZ(K)*(WB(I,J,K+1)*(1.0-C(I,J,K+1)) &
            !                        -WB(I,J,K-1)*(1.0-C(I,J,K-1))) &
            !            +(C(I,J,K)-C1(I,J,K))*DDT)

            RPE(I,J,K)=-((UB(I,J,K)*0.500*((1.0-C(I,J,K))+(1.0-C(I+1,J,K))) &
                         -UB(I-1,J,K)*0.500*((1.0-C(I,J,K))+(1.0-C(I-1,J,K))))*GX(I) &
                        +(VB(I,J,K)*0.500*((1.0-C(I,J,K))+(1.0-C(I,J+1,K))) &
                         -VB(I,J-1,K)*0.500*((1.0-C(I,J,K))+(1.0-C(I,J-1,K))))*EY(J) &
                        +(WB(I,J,K)*0.500*((1.0-C(I,J,K))+(1.0-C(I,J,K+1))) &
                         -WB(I,J,K-1)*0.500*((1.0-C(I,J,K))+(1.0-C(I,J,K-1))))*DZ(K) &
                        +(C(I,J,K)-C1(I,J,K))*DDT)

            UCP(I,J,K)=MYT(I,J,K)/sigmf &
                        *(GXX(I)*CX(I,J,K)+GX(I)**2*CXX(I,J,K) &
                         +EYY(J)*CY(I,J,K)+EY(J)**2*CYY(I,J,K) &
                         +DZZ(K)*CZ(I,J,K)+DZ(K)**2*CZZ(I,J,K)) &
                        +0.50/sigmf* &
                         (GX(I)**2*(MYT(I+1,J,K)-MYT(I-1,J,K))*CX(I,J,K) &
                         +EY(J)**2*(MYT(I,J+1,K)-MYT(I,J-1,K))*CY(I,J,K) &
                         +DZ(K)**2*(MYT(I,J,K+1)-MYT(I,J,K-1))*CZ(I,J,K))

 200    CONTINUE

!$OMP PARALLEL
!$OMP DO
        DO 500 I=3,IMAX+2
        DO 500 K=3,KMAX+2

            !PSI(I,2,K)=PSI(I,3,K)
            !PSI(I,1,K)=PSI(I,3,K)
            !PSI(I,JMAX+2,K)=PSI(I,JMAX+1,K)   !221021TI
            !PSI(I,JMAX+3,K)=PSI(I,JMAX+1,K)   !221021TI
                
            PSI(I,JMAX+2,K)=0.0 !SUMO23/05/04
            PSI(I,JMAX+3,K)=0.0 !SUMO23/05/04

 500    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 300 K=3,KMAX+2
        DO 300 I=3,IMAX+2
        DO 300 J=3,JMAX+1

            ETA_1(I,J,K)=X(I+1,J,K)-X(I,J,K)
            ETA_2(I,J,K)=X(I,J,K)-X(I-1,J,K)
            ETA_3(I,J,K)=Y(I,J+1,K)-Y(I,J,K)
            ETA_4(I,J,K)=Y(I,J,K)-Y(I,J-1,K)
            ETA_5(I,J,K)=Z(I,J,K+1)-Z(I,J,K)
            ETA_6(I,J,K)=Z(I,J,K)-Z(I,J,K-1)

            !IF(J.EQ.IETA(I,K)) THEN
            !    IF(ETA(I,K).LE.Y(I,J,K)) THEN
            !        ETA_3(I,IETA(I,K)-1,K)=ETA(I,K)-Y(I,J-1,K)
            !        PSI(I,IETA(I,K),K)=0.0
            !        PSI(I,IETA(I,K)+1,K)=-PSI(I,IETA(I,K)-1,K)
            !    ELSE IF(ETA(I,K).GT.Y(I,J,K)) THEN
            !        ETA_3(I,IETA(I,K),K)=ETA(I,K)-Y(I,J,K)
            !        PSI(I,IETA(I,K)+1,K)=0.0
            !        PSI(I,IETA(I,K)+2,K)=-PSI(I,IETA(I,K),K)
            !    END IF
            !END IF

 300    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!C***************   POIS EQUATION  ****************************

        DO 1000 LOOP=1,200
        GOSAP=0.0

!C*************     BOUNDARY  CONDITION          *****************

        DO 550 I=1,IMAX+4
        DO 550 K=1,KMAX+4
        DO 550 J=1,IGL(I,K)

            !!PSI(I,JMAX+2,K)=PSI(I,JMAX+1,K)
            !!PSI(I,JMAX+3,K)=PSI(I,JMAX+1,K)
            !PSI(I,2,K)=PSI(I,3,K) !SUMO23/05/04
            !PSI(I,1,K)=PSI(I,2,K) !SUMO23/05/04

            PSI(I,J,K)=PSI(I,IGL(I,K)+1,K)

 550  CONTINUE

        DO 600 I=1,2
        DO 600 J=1,JMAX+3
        DO 600 K=1,KMAX+4

            !PSI(I,J,K)=PSI(3,J,K)
            !PSI(IMAX+2+I,J,K)=PSI(IMAX+2,J,K)

!C********** PERIODIC BOUNDARY CONDITION **********C

            PSI(I,J,K)=PSI(IMAX+I,J,K)
            PSI(IMAX+2+I,J,K)=PSI(2+I,J,K)

 600    CONTINUE


        DO 650 K=1,2
        DO 650 J=1,JMAX+3
        DO 650 I=1,IMAX+4

            !PSI(I,J,K)=PSI(I,J,3)    !ITO CHANGE
            !PSI(I,J,KMAX+2+K)=PSI(I,J,KMAX+2)    !ITO CHANGE

!C********** WALL CONDITION ( POSITION K=2.5 AND KMAX+2.5 ) **********C

            !PSI(I,J,1)=PSI(I,J,4)
            !PSI(I,J,2)=PSI(I,J,3)
            !PSI(I,J,KMAX+3)=PSI(I,J,KMAX+2)
            !PSI(I,J,KMAX+4)=PSI(I,J,KMAX+1)

!C********** Periodic Boundary Condition **********C

            PSI(I,J,1)=PSI(I,J,KMAX+1)
            PSI(I,J,2)=PSI(I,J,KMAX+2)
            PSI(I,J,KMAX+3)=PSI(I,J,3)
            PSI(I,J,KMAX+4)=PSI(I,J,4)

 650    CONTINUE

!CCCCCC////// Surface Boundary Condition ////////CCCCCC

!!$OMP PARALLEL
!!$OMP DO
        !DO 589 I=3,IMAX+2
        !DO 589 K=3,KMAX+2
        !DO 589 J=IETA(I,K),JMAX+1

            !IF(DEPMODE.EQ.1.0) THEN
            !    PSI(I,J,K)=0.0
            !    PSI(I,IETA(I,K)+1,K)=-PSI(I,IETA(I,K)-1,K)
            !END IF

 589  CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL

!CCCCCC////// Pier Boundary Condition ////////CCCCCC

        !IF(pier.EQ.1.0) THEN

            !DO 701 J=1,JMAX+3
            !DO 701 K=Kpier_R,Kpier_L
            !DO 701 I=Ipier_U,Ipier_D

                !PSI(I,J,K)=PSI(I,J,Kpier_L+1)
                !PSI(Ipier_D,J,K)=PSI(Ipier_D+1,J,K)
                !!PSI(I,J,Kpier_R)=PSI(I,J,Kpier_R-1)
                !!PSI(I,J,Kpier_L)=PSI(I,J,Kpier_L+1)
                !PSI(Ipier_U,J,K)=PSI(Ipier_U-1,J,K)
                !!PSI(I,J,K)=PSI(Ipier_U-1,J,K)

 701  CONTINUE

        !END IF
        !IF(pier.EQ.1.0) THEN

            !DO 7011 J=1,JMAX+3
            !DO 7011 K=Kpier_R1,Kpier_L1
            !DO 7011 I=Ipier_U1,Ipier_D1

                !PSI(Ipier_D1,J,K)=PSI(Ipier_D1+1,J,K)
                !!PSI(I,J,Kpier_R1)=PSI(I,J,Kpier_R1-1)
                !!PSI(I,J,Kpier_L1)=PSI(I,J,Kpier_L1+1)
                !PSI(I,J,K)=PSI(I,J,Kpier_L1+1)
                !PSI(Ipier_U1,J,K)=PSI(Ipier_U1-1,J,K)

 7011   CONTINUE

        !END IF
        !IF(pier.EQ.1.0) THEN

            !DO 7012 J=1,JMAX+3
            !DO 7012 K=Kpier_R2,Kpier_L2
            !DO 7012 I=Ipier_U2,Ipier_D2

                !PSI(Ipier_D2,J,K)=PSI(Ipier_D2+1,J,K)
                !!PSI(I,J,Kpier_R2)=PSI(I,J,Kpier_R2-1)
                !!PSI(I,J,Kpier_L2)=PSI(I,J,Kpier_L2+1)
                !PSI(I,J,K)=PSI(I,J,Kpier_R2-1)
                !PSI(Ipier_U2,J,K)=PSI(Ipier_U2-1,J,K)

 7012  CONTINUE

        !END IF
        !IF(pier.EQ.1.0) THEN

            !DO 7013 J=1,JMAX+3
            !DO 7013 K=Kpier_R3,Kpier_L3
            !DO 7013 I=Ipier_U3,Ipier_D3

                !PSI(Ipier_D3,J,K)=PSI(Ipier_D3+1,J,K)
                !!PSI(I,J,Kpier_R1)=PSI(I,J,Kpier_R1-1)
                !!PSI(I,J,Kpier_L1)=PSI(I,J,Kpier_L1+1)
                !PSI(I,J,K)=PSI(I,J,Kpier_L3+1)
                !PSI(Ipier_U3,J,K)=PSI(Ipier_U3-1,J,K)

 7013   CONTINUE

        !END IF
        !IF(pier.EQ.1.0) THEN

            !DO 7014 J=1,JMAX+3
            !DO 7014 K=Kpier_R4,Kpier_L4
            !DO 7014 I=Ipier_U4,Ipier_D4

                !PSI(Ipier_D4,J,K)=PSI(Ipier_D4+1,J,K)
                !!PSI(I,J,Kpier_R2)=PSI(I,J,Kpier_R2-1)
                !!PSI(I,J,Kpier_L2)=PSI(I,J,Kpier_L2+1)
                !PSI(I,J,K)=PSI(I,J,Kpier_R4-1)
                !PSI(Ipier_U4,J,K)=PSI(Ipier_U4-1,J,K)

 7014  CONTINUE

        !END IF

        !aaa

        DO 303 K=3,KMAX+2
        DO 303 I=3,IMAX+2
        DO 303 J=3,JMAX+1

            IF(ETA(I,K).LE.Y(I,IETA(I,K),K)) THEN
                !ETA_3(I,IETA(I,K)-1,K)=DETA(I,K)+0.500/EY(3)  !from middle  sumo250912
                !PSI(I,IETA(I,K)-1,K)=RC*GG*(DETA(I,K)+0.500/EY(J)-(0.500/EY(J)-DETA(I,K)))*DT
                PSI(I,IETA(I,K)-1,K)=RC*GG*(DETA(I,K)+1.00/EY(3))*DT    !HY
                PSI(I,IETA(I,K),K)=RC*GG*(DETA(I,K)-(1.00/EY(J)-DETA(I,K)))*DT
                !PSI(I,IETA(I,K),K)=0.0
                PSI(I,IETA(I,K)+1,K)=0.0
            ELSE IF(ETA(I,K).GT.Y(I,IETA(I,K),K)) THEN
                !ETA_3(I,IETA(I,K),K)=DETA(I,K)-0.500/EY(3)  !from middle  sumo250912
                !ETA_3(I,IETA(I,K)-1,K)=DETA(I,K)+0.500/EY(3)  !from middle  sumo250912
                !PSI(I,IETA(I,K)-1,K)=RC*GG*(DETA(I,K)+0.500/EY(3)-(1.00/EY(3)-DETA(I,K))-0.500/EY(3))*DT
                !PSI(I,IETA(I,K)-1,K)=RC*GG*(DETA(I,K)+0.500/EY(3))*DT    !HY
                !PSI(I,IETA(I,K),K)=RC*GG*(DETA(I,K)-0.500/EY(3)-(1.00/EY(3)-DETA(I,K))-0.500/EY(3))*DT
                PSI(I,IETA(I,K)-1,K)=RC*GG*(DETA(I,K)+1.00/EY(3))*DT    !HY
                PSI(I,IETA(I,K),K)=RC*GG*(DETA(I,K)-(1.00/EY(J)-DETA(I,K)))*DT
                PSI(I,IETA(I,K)+1,K)=0.0
            END IF

 303    CONTINUE

!$OMP PARALLEL
!$OMP DO
        DO 660 K=3,KMAX+2
        DO 660 I=3,IMAX+2
        DO 660 J=3,JMAX+1

            PP1(I,J,K)=PSI(I+1,J,K)
            PP2(I,J,K)=PSI(I-1,J,K)
            PP3(I,J,K)=PSI(I,J+1,K)
            PP4(I,J,K)=PSI(I,J-1,K)
            PP5(I,J,K)=PSI(I,J,K+1)
            PP6(I,J,K)=PSI(I,J,K-1)

            !IF(ETA(I,K).LE.Y(I,IETA(I,K),K)) THEN
            !    !NS-9 Sumo250806
            !    PP3(I,IETA(I,K)-1,K)=RC*GG*(DETA(I,K)-(DETA(I,K)-1.00/EY(3)))*DT   !HY
            !    PP3(I,IETA(I,K)-2,K)=RC*GG*(DETA(I,K)+1.00/EY(3))*DT
            !    !PP4(I,IETA(I,K),K)=RC*GG*(DETA(I,K)+1.00/EY(3))*DT
            !    !PP3(I,IETA(I,K)-1,K)=RC*GG*DETA(I,K)*DT
            !ELSE IF(ETA(I,K).GT.Y(I,IETA(I,K),K)) THEN
            !    PP3(I,IETA(I,K)-1,K)=RC*GG*(DETA(I,K)-(DETA(I,K)-1.00/EY(3)))*DT   !HY
            !    PP3(I,IETA(I,K)-2,K)=RC*GG*(DETA(I,K)+1.00/EY(3))*DT
            !    !PP4(I,IETA(I,K)+1,K)=RC*GG*DETA(I,K)*DT
            !    !PP3(I,IETA(I,K)-1,K)=RC*GG*DETA(I,K)*DT
            !END IF

 660    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 700 K=3,KMAX+2
        DO 700 I=3,IMAX+2
        DO 700 J=3,JMAX+1

!C      SS1(I,J,K)=((1.0-CPP(I,J,K))*
!C     &           (GX(I)**2*(PSI(I+1,J,K)+PSI(I-1,J,K))
!C     &           +EY(J)**2*(PSI(I,J+1,K)+PSI(I,J-1,K))
!C     &           +0.50*GXX(I)*(PSI(I+1,J,K)-PSI(I-1,J,K))
!C     &           +0.50*EYY(J)*(PSI(I,J+1,K)-PSI(I,J-1,K)))
!C     &           -GX(I)**2*0.50*(PSI(I+1,J,K)-PSI(I-1,J,K))*CPX(I,J,K)
!C     &           -EY(J)**2*0.50*(PSI(I,J+1,K)-PSI(I,J-1,K))*CPY(I,J,K)
!C     &           +RPE(I,J,K)+UCP(I,J,K))
!C     &        /(2.0*(GX(I)**2+EY(J)**2)*(1.0-CPP(I,J,K)))-PSI(I,J,K)

            !SS1(I,J,K)=((1.0-C(I,J,K))* &
            !            (GX(I)**2*(PSI(I+1,J,K)+PSI(I-1,J,K)) &
            !            +EY(J)**2*(PSI(I,J+1,K)+PSI(I,J-1,K)) &
            !            +DZ(K)**2*(PSI(I,J,K+1)+PSI(I,J,K-1)) &
            !            +0.50*GXX(I)*(PSI(I+1,J,K)-PSI(I-1,J,K)) &
            !            +0.50*EYY(J)*(PSI(I,J+1,K)-PSI(I,J-1,K)) &
            !            +0.50*DZZ(K)*(PSI(I,J,K+1)-PSI(I,J,K-1))) &
            !            -GX(I)**2*0.50*(PSI(I+1,J,K)-PSI(I-1,J,K))*CX(I,J,K) &
            !            -EY(J)**2*0.50*(PSI(I,J+1,K)-PSI(I,J-1,K))*CY(I,J,K) &
            !            -DZ(K)**2*0.50*(PSI(I,J,K+1)-PSI(I,J,K-1))*CZ(I,J,K) &
            !            -RPE(I,J,K)-UCP(I,J,K)) &
            !            /(2.0*(GX(I)**2+EY(J)**2+DZ(K)**2)*(1.0-C(I,J,K))) &
            !            -PSI(I,J,K)

            SS1(I,J,K)=(ETA_1(I,J,K)*ETA_2(I,J,K)*ETA_3(I,J,K)*ETA_4(I,J,K)*ETA_5(I,J,K)*ETA_6(I,J,K)/( &
                     (ETA_1(I,J,K)*ETA_2(I,J,K)*ETA_3(I,J,K)*ETA_4(I,J,K)+ETA_3(I,J,K)*ETA_4(I,J,K)*ETA_5(I,J,K)*ETA_6(I,J,K)+ETA_1(I,J,K)*ETA_2(I,J,K)*ETA_5(I,J,K)*ETA_6(I,J,K))*(1.0-C(I,J,K)))) &
                     *((PP1(I,J,K)*(1.0-C(I+1,J,K))*ETA_2(I,J,K)+PP2(I,J,K)*(1.0-C(I-1,J,K))*ETA_1(I,J,K))/((ETA_1(I,J,K)+ETA_2(I,J,K))*ETA_1(I,J,K)*ETA_2(I,J,K)) &
                      +(PP3(I,J,K)*(1.0-C(I,J+1,K))*ETA_4(I,J,K)+PP4(I,J,K)*(1.0-C(I,J-1,K))*ETA_3(I,J,K))/((ETA_3(I,J,K)+ETA_4(I,J,K))*ETA_3(I,J,K)*ETA_4(I,J,K)) &
                      +(PP5(I,J,K)*(1.0-C(I,J,K+1))*ETA_6(I,J,K)+PP6(I,J,K)*(1.0-C(I,J,K-1))*ETA_5(I,J,K))/((ETA_5(I,J,K)+ETA_6(I,J,K))*ETA_5(I,J,K)*ETA_6(I,J,K)) &
                      -0.50*RPE(I,J,K)-0.50*UCP(I,J,K)) &
                      -PSI(I,J,K)

!C      SS1(I,J,K)=(GX(I)**2*(PSI(I+1,J,K)+PSI(I-1,J,K))
!C     &           +EY(J)**2*(PSI(I,J+1,K)+PSI(I,J-1,K))
!C     &           +DZ(K)**2*(PSI(I,J,K+1)+PSI(I,J,K-1))
!C     &           +0.50*GXX(I)*(PSI(I+1,J,K)-PSI(I-1,J,K))
!C     &           +0.50*EYY(J)*(PSI(I,J+1,K)-PSI(I,J-1,K))
!C     &           +0.50*DZZ(K)*(PSI(I,J,K+1)-PSI(I,J,K-1))*0.0
!C     &           -RPE(I,J,K))
!C     &           /(2.0*(GX(I)**2+EY(J)**2+DZ(K)**2))-PSI(I,J,K)
!C      SS1(I,J,K)=(GX(I)**2*(PSI(I+1,J,K)+PSI(I-1,J,K))
!C     &           +EY(J)**2*(PSI(I,J+1,K)+PSI(I,J-1,K))
!C     &           +RPE(I,J,K))
!C     &          /(2.0*(GX(I)**2+EY(J)**2))-PSI(I,J,K)

 700    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 178 I=3,IMAX+2
        DO 178 J=3,JMAX+1
        DO 178 K=Kpier_R,Kpier_L
!
            IF(pier.EQ.1.0) THEN
                IF(I.EQ.Ipier_U-1) THEN
                    PSI(Ipier_U,J,K)=PSI(Ipier_U-1,J,K)
                    PP1(I,J,K)=PSI(I+1,J,K)
                    PP2(I,J,K)=PSI(I-1,J,K)
                    PP3(I,J,K)=PSI(I,J+1,K)
                    PP4(I,J,K)=PSI(I,J-1,K)
                    PP5(I,J,K)=PSI(I,J,K+1)
                    PP6(I,J,K)=PSI(I,J,K-1)
                    SS1(I,J,K)=(ETA_1(I,J,K)*ETA_2(I,J,K)*ETA_3(I,J,K)*ETA_4(I,J,K)*ETA_5(I,J,K)*ETA_6(I,J,K)/( &
                               (ETA_1(I,J,K)*ETA_2(I,J,K)*ETA_3(I,J,K)*ETA_4(I,J,K)+ETA_3(I,J,K)*ETA_4(I,J,K)*ETA_5(I,J,K)*ETA_6(I,J,K)+ETA_1(I,J,K)*ETA_2(I,J,K)*ETA_5(I,J,K)*ETA_6(I,J,K))*(1.0-C(I,J,K)))) &
                             *((PP1(I,J,K)*(1.0-C(I+1,J,K))*ETA_2(I,J,K)+PP2(I,J,K)*(1.0-C(I-1,J,K))*ETA_1(I,J,K))/((ETA_1(I,J,K)+ETA_2(I,J,K))*ETA_1(I,J,K)*ETA_2(I,J,K)) &
                              +(PP3(I,J,K)*(1.0-C(I,J+1,K))*ETA_4(I,J,K)+PP4(I,J,K)*(1.0-C(I,J-1,K))*ETA_3(I,J,K))/((ETA_3(I,J,K)+ETA_4(I,J,K))*ETA_3(I,J,K)*ETA_4(I,J,K)) &
                              +(PP5(I,J,K)*(1.0-C(I,J,K+1))*ETA_6(I,J,K)+PP6(I,J,K)*(1.0-C(I,J,K-1))*ETA_5(I,J,K))/((ETA_5(I,J,K)+ETA_6(I,J,K))*ETA_5(I,J,K)*ETA_6(I,J,K)) &
                              -0.50*RPE(I,J,K)-0.50*UCP(I,J,K)) &
                              -PSI(I,J,K)
                    !SS1(I,J,K)=((1.0-C(I,J,K))* &
                    !    (GX(I)**2*(PSI(I+1,J,K)+PSI(I-1,J,K)) &
                    !    +EY(J)**2*(PSI(I,J+1,K)+PSI(I,J-1,K)) &
                    !    +DZ(K)**2*(PSI(I,J,K+1)+PSI(I,J,K-1)) &
                    !    +0.50*GXX(I)*(PSI(I+1,J,K)-PSI(I-1,J,K)) &
                    !    +0.50*EYY(J)*(PSI(I,J+1,K)-PSI(I,J-1,K)) &
                    !    +0.50*DZZ(K)*(PSI(I,J,K+1)-PSI(I,J,K-1))) &
                    !    -GX(I)**2*0.50*(PSI(I+1,J,K)-PSI(I-1,J,K))*CX(I,J,K) &
                    !    -EY(J)**2*0.50*(PSI(I,J+1,K)-PSI(I,J-1,K))*CY(I,J,K) &
                    !    -DZ(K)**2*0.50*(PSI(I,J,K+1)-PSI(I,J,K-1))*CZ(I,J,K) &
                    !    -RPE(I,J,K)-UCP(I,J,K)) &
                    !    /(2.0*(GX(I)**2+EY(J)**2+DZ(K)**2)*(1.0-C(I,J,K))) &
                    !    -PSI(I,J,K)
                END IF
                IF(I.EQ.Ipier_D+1) THEN
                    PSI(Ipier_D,J,K)=PSI(Ipier_D+1,J,K)
                    PP1(I,J,K)=PSI(I+1,J,K)
                    PP2(I,J,K)=PSI(I-1,J,K)
                    PP3(I,J,K)=PSI(I,J+1,K)
                    PP4(I,J,K)=PSI(I,J-1,K)
                    PP5(I,J,K)=PSI(I,J,K+1)
                    PP6(I,J,K)=PSI(I,J,K-1)
                    SS1(I,J,K)=(ETA_1(I,J,K)*ETA_2(I,J,K)*ETA_3(I,J,K)*ETA_4(I,J,K)*ETA_5(I,J,K)*ETA_6(I,J,K)/( &
                               (ETA_1(I,J,K)*ETA_2(I,J,K)*ETA_3(I,J,K)*ETA_4(I,J,K)+ETA_3(I,J,K)*ETA_4(I,J,K)*ETA_5(I,J,K)*ETA_6(I,J,K)+ETA_1(I,J,K)*ETA_2(I,J,K)*ETA_5(I,J,K)*ETA_6(I,J,K))*(1.0-C(I,J,K)))) &
                             *((PP1(I,J,K)*(1.0-C(I+1,J,K))*ETA_2(I,J,K)+PP2(I,J,K)*(1.0-C(I-1,J,K))*ETA_1(I,J,K))/((ETA_1(I,J,K)+ETA_2(I,J,K))*ETA_1(I,J,K)*ETA_2(I,J,K)) &
                              +(PP3(I,J,K)*(1.0-C(I,J+1,K))*ETA_4(I,J,K)+PP4(I,J,K)*(1.0-C(I,J-1,K))*ETA_3(I,J,K))/((ETA_3(I,J,K)+ETA_4(I,J,K))*ETA_3(I,J,K)*ETA_4(I,J,K)) &
                              +(PP5(I,J,K)*(1.0-C(I,J,K+1))*ETA_6(I,J,K)+PP6(I,J,K)*(1.0-C(I,J,K-1))*ETA_5(I,J,K))/((ETA_5(I,J,K)+ETA_6(I,J,K))*ETA_5(I,J,K)*ETA_6(I,J,K)) &
                              -0.50*RPE(I,J,K)-0.50*UCP(I,J,K)) &
                              -PSI(I,J,K)
                    !SS1(I,J,K)=((1.0-C(I,J,K))* &
                    !    (GX(I)**2*(PSI(I+1,J,K)+PSI(I-1,J,K)) &
                    !    +EY(J)**2*(PSI(I,J+1,K)+PSI(I,J-1,K)) &
                    !    +DZ(K)**2*(PSI(I,J,K+1)+PSI(I,J,K-1)) &
                    !    +0.50*GXX(I)*(PSI(I+1,J,K)-PSI(I-1,J,K)) &
                    !    +0.50*EYY(J)*(PSI(I,J+1,K)-PSI(I,J-1,K)) &
                    !    +0.50*DZZ(K)*(PSI(I,J,K+1)-PSI(I,J,K-1))) &
                    !    -GX(I)**2*0.50*(PSI(I+1,J,K)-PSI(I-1,J,K))*CX(I,J,K) &
                    !    -EY(J)**2*0.50*(PSI(I,J+1,K)-PSI(I,J-1,K))*CY(I,J,K) &
                    !    -DZ(K)**2*0.50*(PSI(I,J,K+1)-PSI(I,J,K-1))*CZ(I,J,K) &
                    !    -RPE(I,J,K)-UCP(I,J,K)) &
                    !    /(2.0*(GX(I)**2+EY(J)**2+DZ(K)**2)*(1.0-C(I,J,K))) &
                    !    -PSI(I,J,K)
                END IF
            END IF
 178    CONTINUE
!$OMP END DO
!$OMP END PARALLEL
!
!$OMP PARALLEL
!$OMP DO
        DO 179 K=3,KMAX+2
        DO 179 J=3,JMAX+1
        DO 179 I=Ipier_U,Ipier_D
!
            IF(pier.EQ.1.0) THEN
                IF(K.EQ.Kpier_R-1) THEN
                    PSI(I,J,Kpier_R)=PSI(I,J,Kpier_R-1)
                    PP1(I,J,K)=PSI(I+1,J,K)
                    PP2(I,J,K)=PSI(I-1,J,K)
                    PP3(I,J,K)=PSI(I,J+1,K)
                    PP4(I,J,K)=PSI(I,J-1,K)
                    PP5(I,J,K)=PSI(I,J,K+1)
                    PP6(I,J,K)=PSI(I,J,K-1)
                    SS1(I,J,K)=(ETA_1(I,J,K)*ETA_2(I,J,K)*ETA_3(I,J,K)*ETA_4(I,J,K)*ETA_5(I,J,K)*ETA_6(I,J,K)/( &
                               (ETA_1(I,J,K)*ETA_2(I,J,K)*ETA_3(I,J,K)*ETA_4(I,J,K)+ETA_3(I,J,K)*ETA_4(I,J,K)*ETA_5(I,J,K)*ETA_6(I,J,K)+ETA_1(I,J,K)*ETA_2(I,J,K)*ETA_5(I,J,K)*ETA_6(I,J,K))*(1.0-C(I,J,K)))) &
                             *((PP1(I,J,K)*(1.0-C(I+1,J,K))*ETA_2(I,J,K)+PP2(I,J,K)*(1.0-C(I-1,J,K))*ETA_1(I,J,K))/((ETA_1(I,J,K)+ETA_2(I,J,K))*ETA_1(I,J,K)*ETA_2(I,J,K)) &
                              +(PP3(I,J,K)*(1.0-C(I,J+1,K))*ETA_4(I,J,K)+PP4(I,J,K)*(1.0-C(I,J-1,K))*ETA_3(I,J,K))/((ETA_3(I,J,K)+ETA_4(I,J,K))*ETA_3(I,J,K)*ETA_4(I,J,K)) &
                              +(PP5(I,J,K)*(1.0-C(I,J,K+1))*ETA_6(I,J,K)+PP6(I,J,K)*(1.0-C(I,J,K-1))*ETA_5(I,J,K))/((ETA_5(I,J,K)+ETA_6(I,J,K))*ETA_5(I,J,K)*ETA_6(I,J,K)) &
                              -0.50*RPE(I,J,K)-0.50*UCP(I,J,K)) &
                              -PSI(I,J,K)
                    !SS1(I,J,K)=((1.0-C(I,J,K))* &
                    !    (GX(I)**2*(PSI(I+1,J,K)+PSI(I-1,J,K)) &
                    !    +EY(J)**2*(PSI(I,J+1,K)+PSI(I,J-1,K)) &
                    !    +DZ(K)**2*(PSI(I,J,K+1)+PSI(I,J,K-1)) &
                    !    +0.50*GXX(I)*(PSI(I+1,J,K)-PSI(I-1,J,K)) &
                    !    +0.50*EYY(J)*(PSI(I,J+1,K)-PSI(I,J-1,K)) &
                    !    +0.50*DZZ(K)*(PSI(I,J,K+1)-PSI(I,J,K-1))) &
                    !    -GX(I)**2*0.50*(PSI(I+1,J,K)-PSI(I-1,J,K))*CX(I,J,K) &
                    !    -EY(J)**2*0.50*(PSI(I,J+1,K)-PSI(I,J-1,K))*CY(I,J,K) &
                    !    -DZ(K)**2*0.50*(PSI(I,J,K+1)-PSI(I,J,K-1))*CZ(I,J,K) &
                    !    -RPE(I,J,K)-UCP(I,J,K)) &
                    !    /(2.0*(GX(I)**2+EY(J)**2+DZ(K)**2)*(1.0-C(I,J,K))) &
                    !    -PSI(I,J,K)
                END IF
                IF(K.EQ.Kpier_L+1) THEN
                    PSI(I,J,Kpier_L)=PSI(I,J,Kpier_L+1)
                    PP1(I,J,K)=PSI(I+1,J,K)
                    PP2(I,J,K)=PSI(I-1,J,K)
                    PP3(I,J,K)=PSI(I,J+1,K)
                    PP4(I,J,K)=PSI(I,J-1,K)
                    PP5(I,J,K)=PSI(I,J,K+1)
                    PP6(I,J,K)=PSI(I,J,K-1)
                    SS1(I,J,K)=(ETA_1(I,J,K)*ETA_2(I,J,K)*ETA_3(I,J,K)*ETA_4(I,J,K)*ETA_5(I,J,K)*ETA_6(I,J,K)/( &
                               (ETA_1(I,J,K)*ETA_2(I,J,K)*ETA_3(I,J,K)*ETA_4(I,J,K)+ETA_3(I,J,K)*ETA_4(I,J,K)*ETA_5(I,J,K)*ETA_6(I,J,K)+ETA_1(I,J,K)*ETA_2(I,J,K)*ETA_5(I,J,K)*ETA_6(I,J,K))*(1.0-C(I,J,K)))) &
                             *((PP1(I,J,K)*(1.0-C(I+1,J,K))*ETA_2(I,J,K)+PP2(I,J,K)*(1.0-C(I-1,J,K))*ETA_1(I,J,K))/((ETA_1(I,J,K)+ETA_2(I,J,K))*ETA_1(I,J,K)*ETA_2(I,J,K)) &
                              +(PP3(I,J,K)*(1.0-C(I,J+1,K))*ETA_4(I,J,K)+PP4(I,J,K)*(1.0-C(I,J-1,K))*ETA_3(I,J,K))/((ETA_3(I,J,K)+ETA_4(I,J,K))*ETA_3(I,J,K)*ETA_4(I,J,K)) &
                              +(PP5(I,J,K)*(1.0-C(I,J,K+1))*ETA_6(I,J,K)+PP6(I,J,K)*(1.0-C(I,J,K-1))*ETA_5(I,J,K))/((ETA_5(I,J,K)+ETA_6(I,J,K))*ETA_5(I,J,K)*ETA_6(I,J,K)) &
                              -0.50*RPE(I,J,K)-0.50*UCP(I,J,K)) &
                              -PSI(I,J,K)
                    !SS1(I,J,K)=((1.0-C(I,J,K))* &
                    !    (GX(I)**2*(PSI(I+1,J,K)+PSI(I-1,J,K)) &
                    !    +EY(J)**2*(PSI(I,J+1,K)+PSI(I,J-1,K)) &
                    !    +DZ(K)**2*(PSI(I,J,K+1)+PSI(I,J,K-1)) &
                    !    +0.50*GXX(I)*(PSI(I+1,J,K)-PSI(I-1,J,K)) &
                    !    +0.50*EYY(J)*(PSI(I,J+1,K)-PSI(I,J-1,K)) &
                    !    +0.50*DZZ(K)*(PSI(I,J,K+1)-PSI(I,J,K-1))) &
                    !    -GX(I)**2*0.50*(PSI(I+1,J,K)-PSI(I-1,J,K))*CX(I,J,K) &
                    !    -EY(J)**2*0.50*(PSI(I,J+1,K)-PSI(I,J-1,K))*CY(I,J,K) &
                    !    -DZ(K)**2*0.50*(PSI(I,J,K+1)-PSI(I,J,K-1))*CZ(I,J,K) &
                    !    -RPE(I,J,K)-UCP(I,J,K)) &
                    !    /(2.0*(GX(I)**2+EY(J)**2+DZ(K)**2)*(1.0-C(I,J,K))) &
                    !    -PSI(I,J,K)
                END IF
            END IF
!
 179    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!CCCCCC////// Surface Boundary Condition ////////CCCCCC

!$OMP PARALLEL
!$OMP DO
        DO 590 I=3,IMAX+2
        DO 590 K=3,KMAX+2
        DO 590 J=IETA(I,K),JMAX+1 !SUMO23/03/15

            IF(J.EQ.IETA(I,K)) THEN
                IF(ETA(I,K).LE.Y(I,J,K)) THEN
                    SS1(I,IETA(I,K)-1,K)=0.0
                    SS1(I,IETA(I,K),K)=0.0
                    SS1(I,IETA(I,K)+1,K)=0.0
                ELSE IF(ETA(I,K).GT.Y(I,J,K)) THEN
                    SS1(I,IETA(I,K)-1,K)=0.0
                    SS1(I,IETA(I,K),K)=0.0
                    SS1(I,IETA(I,K)+1,K)=0.0
                END IF
            END IF
            IF(J.GE.IETA(I,K)+2) SS1(I,J,K)=0.0

 590    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!!$OMP PARALLEL
!!$OMP DO
        !DO 592 I=3,IMAX+2
        !O 592 K=3,KMAX+2
        !DO 592 J=3,IGL(I,K) !SUMO23/03/15

            !SS1(I,J,K)=0.0

 592    CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL

!CCCCCC////// Pier Boundary Condition ////////CCCCCC

        IF(pier.EQ.1.0) THEN

!$OMP PARALLEL
!$OMP DO
        DO 702 J=1,JMAX+3
        DO 702 K=Kpier_R,Kpier_L
        DO 702 I=Ipier_U,Ipier_D

            SS1(I,J,K)=0.0

 702    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

        !END IF
        !IF(pier.EQ.1.0) THEN

!!$OMP PARALLEL
!!$OMP DO
        !DO 7021 J=1,JMAX+3
        !DO 7021 K=Kpier_R1,Kpier_L1
        !DO 7021 I=Ipier_U1,Ipier_D1

            !SS1(I,J,K)=0.0

 7021   CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL

        !END IF
        !IF(pier.EQ.1.0) THEN

!!$OMP PARALLEL
!!$OMP DO
        !DO 7022 J=1,JMAX+3
        !DO 7022 K=Kpier_R2,Kpier_L2
        !DO 7022 I=Ipier_U2,Ipier_D2

            !SS1(I,J,K)=0.0

 7022   CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL

        !END IF
        !IF(pier.EQ.1.0) THEN

!!$OMP PARALLEL
!!$OMP DO
        !DO 7023 J=1,JMAX+3
        !DO 7023 K=Kpier_R3,Kpier_L3
        !DO 7023 I=Ipier_U3,Ipier_D3

            !SS1(I,J,K)=0.0

 7023   CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL

        !END IF 
        !IF(pier.EQ.1.0) THEN

!!$OMP PARALLEL
!!$OMP DO
        !DO 7024 J=1,JMAX+3
        !DO 7024 K=Kpier_R4,Kpier_L4
        !DO 7024 I=Ipier_U4,Ipier_D4

            !SS1(I,J,K)=0.0

 7024   CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL

        END IF

        DO 750 K=3,KMAX+2
        DO 750 I=3,IMAX+2
        DO 750 J=3,JMAX+1

            GOSAP=GOSAP+ABS(SS1(I,J,K))

 750    CONTINUE

        DO 800 K=3,KMAX+2
        DO 800 I=3,IMAX+2
        DO 800 J=3,JMAX+1

            PSI(I,J,K)=PSI(I,J,K)+SS1(I,J,K)

 800    CONTINUE

        IF(GOSAP.LT.EPS1) GO TO 900

 1000   CONTINUE
 900    CONTINUE

        DO 1050 K=3,KMAX+2
        DO 1050 I=3,IMAX+2

!CCCCCC////// ZERO Gradient Boundary Condition ////////

            !PSI(I,2,K)=PSI(I,3,K)
            !PSI(I,1,K)=PSI(I,3,K)
            PSI(I,J,K)=PSI(I,IGL(I,K)+1,K)
            PSI(I,JMAX+2,K)=0.0!PSI(I,JMAX+1,K)
            PSI(I,JMAX+3,K)=0.0!PSI(I,JMAX+1,K)

 1050   CONTINUE

        DO 1100 I=1,2
        DO 1100 K=1,KMAX+4
        DO 1100 J=1,JMAX+3

!CCCCCC///// Periodic Boundary Condition /////

            PSI(I,J,K)=PSI(IMAX+I,J,K)
            PSI(IMAX+2+I,J,K)=PSI(2+I,J,K)

!CCCCCC////// ZERO Gradient Boundary Condition ////////

            !PSI(I,J,K)=PSI(3,J,K)
            !PSI(IMAX+2+I,J,K)=PSI(IMAX+2,J,K)

 1100   CONTINUE

        DO 1150 K=1,2
        DO 1150 J=1,JMAX+3
        DO 1150 I=1,IMAX+4

!CCCCCC///// Periodic Boundary Condition /////

            PSI(I,J,K)=PSI(I,J,KMAX+K)    !ITO CHANGE
            PSI(I,J,KMAX+2+K)=PSI(I,J,2+K)    !ITO CHANGE

!CCCCCC////// ZERO Gradient Boundary Condition ////////

            !PSI(I,J,K)=PSI(I,J,3)    !ITO CHANGE
            !PSI(I,J,KMAX+2+K)=PSI(I,J,KMAX+2)    !ITO CHANGE

            !PSI(I,J,1)=PSI(I,J,4)
            !PSI(I,J,2)=PSI(I,J,3)
            !PSI(I,J,KMAX+3)=PSI(I,J,KMAX+2)
            !PSI(I,J,KMAX+4)=PSI(I,J,KMAX+1)

 1150 CONTINUE

!$OMP PARALLEL
!$OMP DO
        DO 1200 K=1,KMAX+4
        DO 1200 I=1,IMAX+4
        DO 1200 J=1,JMAX+3

            !P(I,J,K)=P1(I,J,K)-RC*PSI(I,J,K)*DDT
            P(I,J,K)=-RC*PSI(I,J,K)*DDT

 1200   CONTINUE
!$OMP END DO
!$OMP END PARALLEL

        !DO 1300 K=3,KMAX+2
        !DO 1300 I=3,IMAX+2
        !DO 1300 J=3,JMAX+1

            !!U(I,J,K)=U(I,J,K)+(PSI(I+1,J,K)-PSI(I-1,J,K))*0.50*GX(I)
            !!V(I,J,K)=V(I,J,K)+(PSI(I,J+1,K)-PSI(I,J-1,K))*0.50*EY(J)
            !!W(I,J,K)=W(I,J,K)+(PSI(I,J,K+1)-PSI(I,J,K-1))*0.50*DZ(K)
    
            !U(I,J,K)=U(I,J,K)-(P(I+1,J,K)-P(I-1,J,K))*0.500*GX(I)*DT
            !V(I,J,K)=V(I,J,K)-(P(I,J+1,K)-P(I,J-1,K))*0.500*EY(J)*DT
            !W(I,J,K)=W(I,J,K)-(P(I,J,K+1)-P(I,J,K-1))*0.500*DZ(K)*DT

            !!U(I,IETA(I,K),K)=U(I,IETA(I,K)-1,K)
            !!W(I,IETA(I,K),K)=W(I,IETA(I,K)-1,K)

 1300   CONTINUE

        DO 1302 K=3,KMAX+2
        DO 1302 I=3,IMAX+2
        DO 1302 J=3,JMAX+1

            !sss

            !U(I,J,K)=U(I,J,K)+(PSI(I+1,J,K)-PSI(I-1,J,K))*0.50*GX(I)
            !V(I,J,K)=V(I,J,K)+(PSI(I,J+1,K)-PSI(I,J-1,K))*0.50*EY(J)
            !W(I,J,K)=W(I,J,K)+(PSI(I,J,K+1)-PSI(I,J,K-1))*0.50*DZ(K)

            U(I,J,K)=U(I,J,K)-(P(I+1,J,K)-P(I-1,J,K))*0.500*GX(I)*DT
            V(I,J,K)=V(I,J,K)-(P(I,J+1,K)-P(I,J-1,K))*0.500*EY(J)*DT
            W(I,J,K)=W(I,J,K)-(P(I,J,K+1)-P(I,J,K-1))*0.500*DZ(K)*DT
    
            !UB(I,J,K)=UB(I,J,K)-(P(I+1,J,K)-P(I,J,K))*GX(I)*DT
            !VB(I,J,K)=VB(I,J,K)-(P(I,J+1,K)-P(I,J,K))*EY(J)*DT
            !WB(I,J,K)=WB(I,J,K)-(P(I,J,K+1)-P(I,J,K))*DZ(K)*DT

            !U(I,IETA(I,K),K)=U(I,IETA(I,K)-1,K)
            !W(I,IETA(I,K),K)=W(I,IETA(I,K)-1,K)

 1302   CONTINUE

!!$OMP PARALLEL
!!$OMP DO
!        DO 1708 I=3,IMAX+2
!        DO 1708 J=3,JMAX+1
!        DO 1708 K=Kpier_R,Kpier_L
!!
!            IF(pier.EQ.1.0) THEN
!                IF(I.EQ.Ipier_U-1) THEN
!                    P(Ipier_U,J,K)=P(Ipier_U-1,J,K)
!                    U(I,J,K)=U(I,J,K)-(P(I+1,J,K)-P(I-1,J,K))*0.500*GX(I)*DT
!                    V(I,J,K)=V(I,J,K)-(P(I,J+1,K)-P(I,J-1,K))*0.500*EY(J)*DT
!                    W(I,J,K)=W(I,J,K)-(P(I,J,K+1)-P(I,J,K-1))*0.500*DZ(K)*DT
!                END IF
!                IF(I.EQ.Ipier_D+1) THEN
!                    P(Ipier_D,J,K)=P(Ipier_D+1,J,K)
!                    U(I,J,K)=U(I,J,K)-(P(I+1,J,K)-P(I-1,J,K))*0.500*GX(I)*DT
!                    V(I,J,K)=V(I,J,K)-(P(I,J+1,K)-P(I,J-1,K))*0.500*EY(J)*DT
!                    W(I,J,K)=W(I,J,K)-(P(I,J,K+1)-P(I,J,K-1))*0.500*DZ(K)*DT
!                END IF
!            END IF
! 1708   CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL
!!
!!$OMP PARALLEL
!!$OMP DO
!        DO 1709 K=3,KMAX+2
!        DO 1709 J=3,JMAX+1
!        DO 1709 I=Ipier_U,Ipier_D
!!
!            IF(pier.EQ.1.0) THEN
!                IF(K.EQ.Kpier_R-1) THEN
!                    P(I,J,Kpier_R)=P(I,J,Kpier_R-1)
!                    U(I,J,K)=U(I,J,K)-(P(I+1,J,K)-P(I-1,J,K))*0.500*GX(I)*DT
!                    V(I,J,K)=V(I,J,K)-(P(I,J+1,K)-P(I,J-1,K))*0.500*EY(J)*DT
!                    W(I,J,K)=W(I,J,K)-(P(I,J,K+1)-P(I,J,K-1))*0.500*DZ(K)*DT
!                END IF
!                IF(K.EQ.Kpier_L+1) THEN
!                    P(I,J,Kpier_L)=P(I,J,Kpier_L+1)
!                    U(I,J,K)=U(I,J,K)-(P(I+1,J,K)-P(I-1,J,K))*0.500*GX(I)*DT
!                    V(I,J,K)=V(I,J,K)-(P(I,J+1,K)-P(I,J-1,K))*0.500*EY(J)*DT
!                    W(I,J,K)=W(I,J,K)-(P(I,J,K+1)-P(I,J,K-1))*0.500*DZ(K)*DT
!                END IF
!            END IF
!!
! 1709   CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL

        !DO 1303 K=3,KMAX+2
        !DO 1303 I=3,IMAX+2
        !DO 1303 J=3,IETA(I,K)

            !sss
    
            !U(I,J,K)=(UB(I,J,K)+UB(I-1,J,K))*0.500
            !V(I,J,K)=(VB(I,J,K)+VB(I,J-1,K))*0.500
            !W(I,J,K)=(WB(I,J,K)+WB(I,J,K-1))*0.500

 1303   CONTINUE

        !DO 1301 K=3,KMAX+2
        !DO 1301 I=3,IMAX+2

            !DUUDX(I,K)=U(I,IETA(I,K),K)*(ELES(I+1,K)-ELES(I-1,K))*0.50*GX(I) &
            !        -ABS(U(I,IETA(I,K),K))*(ELES(I+1,K)-2.0*ELES(I,K)+ELES(I-1,K))*0.50*GX(I)
            !DWWDZ(I,K)=W(I,IETA(I,K),K)*(ELES(I,K+1)-ELES(I,K-1))*0.50*DZ(K) &
            !        -ABS(W(I,IETA(I,K),K))*(ELES(I,K+1)-2.0*ELES(I,K)+ELES(I,K-1))*0.50*DZ(K)

            !V(I,IETA(I,K),K)=(ELES(I,K)-ELES1(I,K))/DT+DUUDX(I,K)+DWWDZ(I,K)

 1301   CONTINUE

        IF(naka.EQ.0.0) THEN
        WRITE(*,50) LOOP,GOSAP
 50     FORMAT (1X,'LOOP(P)=',I6,2X,'GOSAP=',F15.5)
        END IF

    deallocate (SS1,PSI1,CX,CY,CXX,CYY,CZ,CZZ,UCP,RPE &
               ,PP1,PP2,PP3,PP4,PP5,PP6,ETA_1,ETA_2,ETA_3,ETA_4,ETA_5,ETA_6)

    RETURN
    END

!C***************************************************************
    SUBROUTINE NSE
!C*********************** LIQUID PHASE **************************
!C***************************************************************
    USE MF_SF
    USE OMP_LIB
    IMPLICIT NONE

      allocate ( &
      E11(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),F11(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,E12(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),F12(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,E13(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),F13(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,DD1(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,UGA(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),UGB(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,UEA(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),UEB(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,UDA(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),UDB(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,VGA(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),VGB(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,VEA(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),VEB(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,VDA(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),VDB(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,WGA(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),WGB(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,WEA(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),WEB(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,WDA(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),WDB(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,A0(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),AP(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,B0(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),BP(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,C0(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),CP(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,AN(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,BN(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,CN(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,SU(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),CC1(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,SV(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),CC2(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,SW(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),CC3(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,UG(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),VG(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,UE(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),VE(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,UD(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),VD(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,UGG(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),VGG(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,UEE(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),VEE(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,UDD(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),VDD(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,WG(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),WGG(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,WE(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),WEE(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,WD(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),WDD(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,MYTG(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),HKG(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,MYTE(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),HKE(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,MYTD(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),HKD(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,BOU(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),OMEX(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,OMEY(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),OMEZ(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,FDX(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),FDY(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,FLX0(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),FLY0(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,FMX(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),FMY(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,DUDT(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),DVDT(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,DUSDT(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),DVSDT(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,DWDT(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),DWSDT(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,FMZ(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),FLZ0(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,FDZ(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,FLUC_RATE(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,UF(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),VF(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,WF(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),CCC(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
     ,STAT = ALLOC_ERR )

!C------------------NAITEN NO KEISAN ----------------------

            CCD=1./12.

!$OMP PARALLEL
!$OMP DO
        DO 107 K=3,KMAX+2
        DO 107 I=3,IMAX+2
        DO 107 J=3,JMAX+1

            E11(I,J,K)=(U1(I,J,K))*GX(I)
            E12(I,J,K)=(V1(I,J,K))*EY(J)
            E13(I,J,K)=(W1(I,J,K))*DZ(K)

            F11(I,J,K)=ABS(U1(I,J,K))*GX(I)
            F12(I,J,K)=ABS(V1(I,J,K))*EY(J)
            F13(I,J,K)=ABS(W1(I,J,K))*DZ(K)

!C*******************************************************
!CC/////// Interaction between both phases //////////////
!C*******************************************************
!C       CG1(I,J,K)=0.5*(C(I+1,J,K)-C(I-1,J,K))
!C       CE1(I,J,K)=0.5*(C(I,J+1,K)-C(I,J-1,K))
!C       CG2(I,J,K)=0.5*(C(I+1,J,K)-C(I-1,J,K))
!C       CE2(I,J,K)=0.5*(C(I,J+1,K)-C(I,J-1,K))

!C      UC(I,J,K)=-MYT(I,J,K)/1.0*CG1(I,J,K)*GX(I)
!C      VC(I,J,K)=-MYT(I,J,K)/1.0*CE2(I,J,K)*EY(J)

       MYTG(I,J,K)=0.5*(MYT(I+1,J,K)-MYT(I-1,J,K))
       MYTE(I,J,K)=0.5*(MYT(I,J+1,K)-MYT(I,J-1,K))
       MYTD(I,J,K)=0.5*(MYT(I,J,K+1)-MYT(I,J,K-1))

       HKG(I,J,K)=0.5*(HK(I+1,J,K)-HK(I-1,J,K))
       HKE(I,J,K)=0.5*(HK(I,J+1,K)-HK(I,J-1,K))
       HKD(I,J,K)=0.5*(HK(I,J,K+1)-HK(I,J,K-1))

       RE1(I,J,K)=SQRT(ABS(US(I,J,K)-U1(I,J,1))**2 &
                      +ABS(VS(I,J,K)-V1(I,J,1))**2 &
                      +ABS(WS(I,J,K)-W1(I,J,1))**2) &
                  *DR/RED
 107       CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!C-------  Drag (Linear Version) ------

!$OMP PARALLEL
!$OMP DO
            DO 9190 K=3,KMAX+2
            DO 9190 I=3,IMAX+2
            DO 9190 J=3,JMAX+1
!C
!                FDX(I,J,K)=18.0*RED/DR**2*(1.0+0.150*RE1(I,J,K)**0.6870) &
                FDX(I,J,K)=18.0*RED/DR**2*(1.0+0.4320*RE1(I,J,K)**0.6870) &  !SUMO23/10/23
!C      FDX(I,J,K)=18.0*RED/DR**2*(1.0+0.150*RE1(I,J,K))**0.6870
!CCCCccc     &           *ABS(1.0-C(I,J,K))**(-2.70)
                            *(US(I,J,K)-U1(I,J,K))*1.0
!
!                FDY(I,J,K)=18.0*RED/DR**2*(1.0+0.150*RE1(I,J,K)**0.6870) &
                FDY(I,J,K)=18.0*RED/DR**2*(1.0+0.4320*RE1(I,J,K)**0.6870) &  !SUMO23/10/23
!C      FDY(I,J,K)=18.0*RED/DR**2*(1.0+0.150*RE1(I,J,K))**0.6870
!CCCCCC     &           *ABS(1.0-C(I,J,K))**(-2.70)
                            *(VS(I,J,K)-V1(I,J,K))*1.0
!
!C      FDZ(I,J,K)=18.0*RED/DR**2*(1.0+0.150*RE1(I,J,K)**0.6870)
!                FDZ(I,J,K)=18.0*RED/DR**2*(1.0+0.150*RE1(I,J,K))**0.6870 &
                FDZ(I,J,K)=18.0*RED/DR**2*(1.0+0.4320*RE1(I,J,K))**0.6870 &  !SUMO23/10/23
!CCCCCCc     &           *ABS(1.0-C(I,J,K))**(-2.70)
                            *(WS(I,J,K)-W1(I,J,K))
!C
                OMEX(I,J,K)=(W1(I,J+1,K)-W1(I,J-1,K))*0.50*EY(J) &
                           -(V1(I,J,K+1)-V1(I,J,K-1))*0.50*DZ(K)
!
                OMEY(I,J,K)=(U1(I,J,K+1)-U1(I,J,K-1))*0.50*DZ(K) &
                           -(W1(I+1,J,K)-W1(I-1,J,K))*0.50*GX(I)
!
                OMEZ(I,J,K)=(V1(I+1,J,K)-V1(I-1,J,K))*0.50*GX(I) &
                           -(U1(I,J+1,K)-U1(I,J-1,K))*0.50*EY(J)
!C
      DUDT(I,J,K)=(U1(I,J,K)-U2(I,J,K))/DT &
                  +U1(I,J,K)*(U1(I+1,J,K)-U1(I-1,J,K))*0.50*GX(I) &
                  +V1(I,J,K)*(U1(I,J+1,K)-U1(I,J-1,K))*0.50*EY(J) &
                  +W1(I,J,K)*(U1(I,J,K+1)-U1(I,J,K-1))*0.50*DZ(K) &
              -ABS(U1(I,J,K))*0.50*GX(I) &
                 *(U1(I+1,J,K)-2.0*U1(I,J,K)+U1(I-1,J,K)) &
              -ABS(V1(I,J,K))*0.50*EY(J) &
                 *(U1(I,J+1,K)-2.0*U1(I,J,K)+U1(I,J-1,K)) &
              -ABS(W1(I,J,K))*0.50*DZ(K) &
                 *(U1(I,J,K+1)-2.0*U1(I,J,K)+U1(I,J,K-1))
!
      DVDT(I,J,K)=(V1(I,J,K)-V2(I,J,K))/DT &
                  +U1(I,J,K)*(V1(I+1,J,K)-V1(I-1,J,K))*0.50*GX(I) &
                  +V1(I,J,K)*(V1(I,J+1,K)-V1(I,J-1,K))*0.50*EY(J) &
                  +W1(I,J,K)*(V1(I,J,K+1)-V1(I,J,K-1))*0.50*DZ(K) &
              -ABS(U1(I,J,K))*0.50*GX(I) &
                 *(V1(I+1,J,K)-2.0*V1(I,J,K)+V1(I-1,J,K)) &
              -ABS(V1(I,J,K))*0.50*EY(J) &
                 *(V1(I,J+1,K)-2.0*V1(I,J,K)+V1(I,J-1,K)) &
              -ABS(W1(I,J,K))*0.50*DZ(K) &
                 *(V1(I,J,K+1)-2.0*V1(I,J,K)+V1(I,J,K-1))
!
      DWDT(I,J,K)=(W1(I,J,K)-W2(I,J,K))/DT &
                  +U1(I,J,K)*(W1(I+1,J,K)-W1(I-1,J,K))*0.50*GX(I) &
                  +V1(I,J,K)*(W1(I,J+1,K)-W1(I,J-1,K))*0.50*EY(J) &
                  +W1(I,J,K)*(W1(I,J,K+1)-W1(I,J,K-1))*0.50*DZ(K) &
              -ABS(U1(I,J,K))*0.50*GX(I) &
                 *(W1(I+1,J,K)-2.0*W1(I,J,K)+W1(I-1,J,K)) &
              -ABS(V1(I,J,K))*0.50*EY(J) &
                 *(W1(I,J+1,K)-2.0*W1(I,J,K)+W1(I,J-1,K)) &
              -ABS(W1(I,J,K))*0.50*DZ(K) &
                 *(W1(I,J,K+1)-2.0*W1(I,J,K)+W1(I,J,K-1))
!C
      DUSDT(I,J,K)=(US1(I,J,K)-US2(I,J,K))/DT
      DVSDT(I,J,K)=(VS1(I,J,K)-VS2(I,J,K))/DT
      DWSDT(I,J,K)=(WS1(I,J,K)-WS2(I,J,K))/DT
!C
 9190      CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 9180 K=3,KMAX+2
        DO 9180 I=3,IMAX+2
        DO 9180 J=3,JMAX+1

!C------- Lift  ----------

!C      FLX0(I,J,K)=CL0*OMEG(I,J,K)*(VS(I,J,K)-V1(I,J,K))
!C      FLY0(I,J,K)=CL0*OMEG(I,J,K)*(US(I,J,K)-U1(I,J,K))
!C      FLZ0(I,J,K)=0.0

            FLX0(I,J,K)=CL0*(OMEZ(I,J,K)*(VS(I,J,K)-V1(I,J,K)) &
                            -OMEY(I,J,K)*(WS(I,J,K)-W1(I,J,K)) )
            FLY0(I,J,K)=CL0*(OMEX(I,J,K)*(WS(I,J,K)-W1(I,J,K)) &
                            -OMEZ(I,J,K)*(US(I,J,K)-U1(I,J,K)) )
            FLZ0(I,J,K)=CL0*(OMEY(I,J,K)*(US(I,J,K)-U1(I,J,K)) &
                            -OMEX(I,J,K)*(VS(I,J,K)-V1(I,J,K)) )

!C------- Virtual Mass & Pressure Gradient  ----------

            FMX(I,J,K)=CM0*DUSDT(I,J,K)-(1.0+CM0)*DUDT(I,J,K)
            FMY(I,J,K)=CM0*DVSDT(I,J,K)-(1.0+CM0)*DVDT(I,J,K)
            FMZ(I,J,K)=CM0*DWSDT(I,J,K)-(1.0+CM0)*DWDT(I,J,K)

 9180   CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!C////////// Bouyancy Effect ////////////////

!$OMP PARALLEL
!$OMP DO
        DO 9160 K=3,KMAX+2
        DO 9160 I=3,IMAX+2
        DO 9160 J=3,JMAX+1

            BOU(I,J,K)=-GG*ROU(I,J,K)/ROUA(I,J,K)
            !BOU(I,J,K)=-GG*ROUA(I,J,K)/ROUA(I,J,K)

 9160   CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!C************************************************************
!C*****************  Iteration Strat  ************************
!C************************************************************

        DO 1000 LOOP=1,20

            GOSAQ=0.0
            CCD=1.0/12.0
            CCD2=1.0/4.0

!********** HORIZONTAL BOUNDARY CONDITION **********!

            DO 320 I=1,2
            DO 320 K=1,KMAX+4
            DO 320 J=1,JMAX+3

!********** Periodic Boundary Condition **********!

                U(I,J,K)=U(IMAX+I,J,K)
                V(I,J,K)=V(IMAX+I,J,K)
                W(I,J,K)=W(IMAX+I,J,K)

                U(IMAX+2+I,J,K)=U(2+I,J,K)
                V(IMAX+2+I,J,K)=V(2+I,J,K)
                W(IMAX+2+I,J,K)=W(2+I,J,K) 

!********** BEFORE Log-law Boundary Condition **********!

!                  IF (J.LE.IGL(I,K)) THEN
!                      U(I,J,K)=0.0
!                  ELSE
!                      U(I,J,K)=AA*(1.0/0.40*log(1.0*(J-FLOAT(IGL(I,K))+
!     &                            0.50))+8.50)
!                  END IF

!                  V(I,J,K)=0.0
!                  W(I,J,K)=0.0

!                  U(IMAX+2+I,J,K)=U(IMAX+2,J,K)
!                  V(IMAX+2+I,J,K)=V(IMAX+2,J,K)
!                  W(IMAX+2+I,J,K)=W(IMAX+2,J,K)

 320    CONTINUE

!********** DEPTH USING Log-law Boundary Condition **********!

!*************** BC_Q2D ***************!

!        UuniSUM=0.0
!        I=3
!        DO 321 K=3,KMAX+2
!
!            Uuni(K)=DEP(I,K)**(2.0/3.0)*SLOPE**0.500/RNM(I,K)
!            UuniSUM=UuniSUM+Uuni(K)*DEP(I,K)/DZ(K)
!
! 321    CONTINUE
!
!        DO 322 I=1,2
!        DO 322 K=1,KMAX+4
!
!            UT(I,K)=Uuni(K)*QQQ(I)/UuniSUM
!            UT(IMAX+2+I,K)=UT(IMAX+2,K)
!            WT(IMAX+2+I,K)=WT(IMAX+2,K)
!
! 322    CONTINUE
!
!        DO 323 K=1,2
!        DO 323 I=1,IMAX+4
!
!            UT(I,K)=UT(I,3)
!            WT(I,K)=WT(I,3)
!            UT(I,KMAX+2+K)=UT(I,KMAX+2)
!            WT(I,KMAX+2+K)=WT(I,KMAX+2)
!
!            !UT(I,K)=UT(IMAX+I,K)
!            !WT(I,K)=WT(IMAX+I,K)
!            !UT(IMAX+2+I,K)=UT(2+I,K)
!            !WT(IMAX+2+I,K)=WT(2+I,K)
!
! 323    CONTINUE
!
!!*************** BC_Q ***************!
!
!        DO 324 I=1,2
!        DO 324 K=1,KMAX+4
!
!            !CFF(I,K)=GG*RNM(I,K)**2/(DEP(3,K)**(1.0/3.0))
!            !USTAB(I,K)=CFF(I,K)**0.500*UT(I,K)
!
!            DO 326 J=1,IETA(I,K)-1
!
!                IF(J.LE.IGL(I,K)) THEN
!                    U(I,J,K)=0.0
!                ELSE
!                    !U(I,J,K)=USTAB(I,K)*(1.0/0.40* &
!                    !         LOG((J-(FLOAT(IGL(I,K))+0.500))/EY(3)/ &
!                    !             ((RNM(I,K)*(GG**0.5)/0.13055)**6))+8.50)
!                    !!U(I,IGL(I,K)+1,K)=USTAB(I,K)*(1.0/0.40* &
!                    !!         LOG(1.0/EY(3)/ &
!                    !!            ((RNM(I,K)*(GG**0.5)/0.13055)**6))+8.50)
!                    U(I,J,K)=USTAB(I,K)*(1.0/0.40* &
!                             LOG(USTAB(I,K)*(J-(FLOAT(IGL(I,K))+0.500))/EY(3)/RED)+5.50)
!                    U(I,IGL(I,K)+1,K)=USTAB(I,K)*(1.0/0.40*LOG(USTAB(I,K)*0.50/EY(3)/RED)+5.50)
!                    !U(I,IGL(I,K)+1,K)=2.00*U(I,IGL(I,K)+2,K)-U(I,IGL(I,K)+3,K)
!                    V(I,J,K)=0.0
!                    W(I,J,K)=0.0
!                    HK(I,J,K)=1.0E-5
!                END IF
!
!                !U(I,J,K)=UT(I,K)
!                !IF(J.LE.IGL(I,K)) U(I,J,K)=0.0
!                V(I,J,K)=0.0
!                W(I,J,K)=0.0
!                HK(I,J,K)=1.0E-5
!
!                U(I,IETA(I,K),K)=U(I,IETA(I,K)-1,K)
!                V(I,IETA(I,K),K)=0.0
!                W(I,IETA(I,K),K)=0.0
!
! 326    CONTINUE
!
!            DO 3207 J=IETA(I,K)+1,JMAX+3
!
!                U(I,J,K)=0.0
!                V(I,J,K)=0.0
!                W(I,J,K)=0.0
!
! 3207   CONTINUE
! 324    CONTINUE
!
!        !FLUX_IN_I=0.0 !VOLUME IN FOR I DORECTION
!        !FLUX_OUT_I=0.0
!        !DO K=3,KMAX+2
!        !    FLUX_IN_I=FLUX_IN_I+UT(2,K)*DEP(3,K)/DZ(K)
!        !    FLUX_OUT_I=FLUX_OUT_I+UT(IMAX+2,K)*DEP(IMAX+2,K)/DZ(K)
!        !END DO
!
!        !DO I=IMAX+3,IMAX+4
!        !DO K=3,KMAX+2
!        !DO J=3,JMAX+1
!        !    CCC(I,J,K)=-(U1(I-1,J,K)-U3(I-1,J,K))/GX(I) &
!        !               /((U1(I-1,J,K)+U3(I-1,J,K)-2.00*U2(I-2,J,K))*DT)
!        !END DO
!        !END DO
!        !END DO
!
!        DO 327 I=1,2
!        DO 327 K=1,KMAX+4
!        DO 327 J=1,JMAX+3
!
!            U(IMAX+2+I,J,K)=U(IMAX+2,J,K)
!            V(IMAX+2+I,J,K)=V(IMAX+2,J,K)
!            W(IMAX+2+I,J,K)=W(IMAX+2,J,K)
!
!            IF(U(IMAX+2,J,K).LE.0.0) U(IMAX+2+I,J,K)=0.0
!            IF(W(IMAX+2,J,K).LE.0.0) W(IMAX+2+I,J,K)=0.0
!
!            !IF(U(IMAX+2,J,K).GE.0.0) THEN
!            !    U(IMAX+3,J,K)=(U1(IMAX+3,J,K)-UT00*(U1(IMAX+4,J,K)-U1(IMAX+2,J,K))*0.50*GX(IMAX+2)*DT &
!            !             +ABS(UT00)*0.50*GX(IMAX+2)*(U1(IMAX+4,J,K)-2.0*U1(IMAX+3,J,K)+U1(IMAX+2,J,K))*DT)!*(FLUX_IN_I/FLUX_OUT_I)
!            !    W(IMAX+3,J,K)=W1(IMAX+3,J,K)-UT00*(W1(IMAX+4,J,K)-W1(IMAX+2,J,K))*0.50*GX(IMAX+2)*DT &
!            !             +ABS(UT00)*0.50*GX(IMAX+2)*(W1(IMAX+4,J,K)-2.0*W1(IMAX+3,J,K)+W1(IMAX+2,J,K))*DT
!            !    !U(IMAX+3,J,K)=(U1(IMAX+3,J,K)-UT00*(U(IMAX+4,J,K)-U(IMAX+2,J,K))*0.50*GX(IMAX+2)*DT)!*(FLUX_IN_I/FLUX_OUT_I)
!            !    !W(IMAX+3,J,K)=W1(IMAX+3,J,K)-UT00*(W(IMAX+4,J,K)-W(IMAX+2,J,K))*0.50*GX(IMAX+2)*DT
!            !    !U(IMAX+3,J,K)=(U2(IMAX+3,J,K)*(1.0-(DT*GX(IMAX+3))*CCC(IMAX+3,J,K))/(1.0+(DT*GX(IMAX+3))*CCC(IMAX+3,J,K)) &
!            !    !              +U1(IMAX+2,J,K)*(2.0*(DT*GX(IMAX+3))*CCC(IMAX+3,J,K))/(1.0+(DT*GX(IMAX+3))*CCC(IMAX+3,J,K)))!*(FLUX_IN_I/FLUX_OUT_I)
!            !    !W(IMAX+3,J,K)=W2(IMAX+3,J,K)*(1.0-(DT*GX(IMAX+3))*CCC(IMAX+3,J,K))/(1.0+(DT*GX(IMAX+3))*CCC(IMAX+3,J,K)) &
!            !    !             +W1(IMAX+2,J,K)*(2.0*(DT*GX(IMAX+3))*CCC(IMAX+3,J,K))/(1.0+(DT*GX(IMAX+3))*CCC(IMAX+3,J,K))
!            !    !U(IMAX+4,J,K)=(U2(IMAX+4,J,K)*(1.0-(DT*GX(IMAX+4))*CCC(IMAX+4,J,K))/(1.0+(DT*GX(IMAX+4))*CCC(IMAX+4,J,K)) &
!            !    !              +U1(IMAX+3,J,K)*(2.0*(DT*GX(IMAX+4))*CCC(IMAX+4,J,K))/(1.0+(DT*GX(IMAX+4))*CCC(IMAX+4,J,K)))!*(FLUX_IN_I/FLUX_OUT_I)
!            !    !W(IMAX+4,J,K)=W2(IMAX+4,J,K)*(1.0-(DT*GX(IMAX+4))*CCC(IMAX+4,J,K))/(1.0+(DT*GX(IMAX+4))*CCC(IMAX+4,J,K)) &
!            !    !             +W1(IMAX+3,J,K)*(2.0*(DT*GX(IMAX+4))*CCC(IMAX+4,J,K))/(1.0+(DT*GX(IMAX+4))*CCC(IMAX+4,J,K))
!            !    U(IMAX+4,J,K)=U(IMAX+3,J,K)
!            !    W(IMAX+4,J,K)=W(IMAX+3,J,K)
!            !ELSE
!            !    U(IMAX+2+I,J,K)=U(IMAX+2,J,K)
!            !    W(IMAX+2+I,J,K)=W(IMAX+2,J,K)
!            !END IF
!
!            !U(IMAX+3,J,K)=(U1(IMAX+3,J,K)-UT00*(U1(IMAX+4,J,K)-U1(IMAX+2,J,K))*0.50*GX(IMAX+2)*DT &
!            !             +ABS(UT00)*0.50*GX(IMAX+2)*(U1(IMAX+4,J,K)-2.0*U1(IMAX+3,J,K)+U1(IMAX+2,J,K))*DT)!*(FLUX_IN_I/FLUX_OUT_I)
!            !W(IMAX+3,J,K)=W1(IMAX+3,J,K)-UT00*(W1(IMAX+4,J,K)-W1(IMAX+2,J,K))*0.50*GX(IMAX+2)*DT &
!            !             +ABS(UT00)*0.50*GX(IMAX+2)*(W1(IMAX+4,J,K)-2.0*W1(IMAX+3,J,K)+W1(IMAX+2,J,K))*DT
!            !U(IMAX+4,J,K)=U(IMAX+3,J,K)
!            !W(IMAX+4,J,K)=W(IMAX+3,J,K)
!
!            V(I,J,K)=0.0
!            W(I,J,K)=0.0
!
! 327    CONTINUE
!
!C*********** Fluctuating inflow velocity  ********C !add by Chico_231020

        !!?øΩ?øΩ?øΩœíl?øΩ∆ïW?øΩ?øΩ?øΩŒùÔøΩ
        !MEAN=0.0d0
        !STD_DEV=0.20d0

        !!?øΩ?øΩ?øΩ?øΩ?øΩÃùÔøΩ?øΩ?øΩ?øΩ?øΩ
        !CALL random_seed()
        !I=1
        !DO 3114 K=1,KMAX+4
        !    !?øΩ?øΩ?øΩK?øΩ?øΩ?øΩz?øΩ…ù]?øΩ?øΩ?øΩ?øΩ?øΩ?øΩ?øΩ?∂ùÔøΩ
        !    DO 3116 J=IGL(I,K)+1,IETA(I,K)
        !        CALL random_number(r1)
        !        CALL random_number(r2)
        !        FLUC_RATE(I,J,K)=MEAN+STD_DEV* &
        !                        (SQRT(-2.0d0*log(r1))*COS(2.0d0*PHI*r2))

                !PDF(I,J,K)=1.0d0/(STD_DEV*SQRT(2.0d0*PHI))* &
                !           EXP(-((FLUC_RATE(I,J,K)-MEAN)**2.00)/(2.0d0*STD_DEV**2.00))
! ?øΩƒìx?øΩ?øΩ?øΩv?øΩ?øΩ?øΩv?øΩZ
!c      r_sum2(I,K) = r_sum2(I,K)+Fluc_rate(I,J,K)

! ?øΩ?øΩ?øΩ?øΩ?øΩÃùÔøΩ?øΩv?øΩ?øΩ?øΩv?øΩZ
!C      r_sum1(I,K) = r_sum1(I,K)+FLuc_rate(I,J,K)

 3116  CONTINUE
 3114  CONTINUE

!C      I=1
!C      DO 3117 I=1,2
!c       DO 3117 K=1,KMAX+4
!c       r_sum2(I,K)=0.0d0
!c       DO 3118 J= IETA(I,K), IGL(I,K)+1, -1

!c       ! ?øΩ?øΩ?øΩv?øΩ?øΩ0?øΩ…ãﬂÇÔøΩ?øΩ»ÇÔøΩÊÇ§?øΩ…íÔøΩ?øΩ?øΩ
!c       FLuc_rate(I,J,K) = FLuc_rate(I,J,K)
!c      &            -r_sum1(I,K) / (IETA(I,K)-(IGL(I,K)+1)+1)

!c       pdf(I,J,K) = 1.0d0 / (std_dev * sqrt(2.0d0 *
!c      &            3.14159265358979323846d0)) *
!c      &            exp(-((FLuc_rate(I,J,K) - mean)**2) /
!c      &             (2.0d0 * std_dev**2))
!c       ! ?øΩƒìx?øΩ?øΩ?øΩv?øΩ?øΩ?øΩv?øΩZ
!c       r_sum2(I,K) = r_sum2(I,K)+Fluc_rate(I,J,K)

!c       write(*, *) FLuc_rate(I,J,K), pdf(I,J,K)

!c 3118  CONTINUE
!c 3117  CONTINUE

!c       WRITE(*,*) J,Fluc_rate(1,J,5),r_sum2(1,5),
!c      &                   IGL(1,5)+1,IETA(1,5)

        !I=1
        !DO 3224 K=1,KMAX+4
        !DO 3224 J=IGL(I,K)+1,IETA(I,K) !?øΩœìÔøΩ?øΩ?øΩ?øΩ?øΩ?øΩ?øΩŒùÔøΩ?øΩ?øΩ?øΩz?øΩ…ït?øΩ?øΩ

            !UF(I,J,K)=U(I,IETA(I,K),K)*FLUC_RATE(I,J,K)
            !VF(I,J,K)=U(I,IETA(I,K),K)*FLUC_RATE(I,J,K)
            !WF(I,J,K)=U(I,IETA(I,K),K)*FLUC_RATE(I,J,K)
            !U(I,J,K)=U(I,J,K)+UF(I,J,K)
            !V(I,J,K)=V(I,J,K)+VF(I,J,K)
            !W(I,J,K)=W(I,J,K)+WF(I,J,K)
           
            !U(2,J,K)=U(1,J,K)
            !V(2,J,K)=V(1,J,K)
            !W(2,J,K)=W(1,J,K)

 3224   CONTINUE

!********** Spanwise Direction ***********!

        DO 330 K=1,2
        DO 330 I=1,IMAX+4
        DO 330 J=1,JMAX+3

!********** Periodic Boundary **********!

            U(I,J,K)=U(I,J,KMAX+K)    !ITO CHANGE
            V(I,J,K)=V(I,J,KMAX+K)    !ITO CHANGE
            W(I,J,K)=W(I,J,KMAX+K)    !ITO CHANGE

            U(I,J,KMAX+2+K)=U(I,J,2+K)    !ITO CHANGE
            V(I,J,KMAX+2+K)=V(I,J,2+K)    !ITO CHANGE
            W(I,J,KMAX+2+K)=W(I,J,2+K)    !ITO CHANGE

!********** SLIP BOUNDARY **********

            !!U(I,J,K)=U(I,J,3)
            !V(I,J,K)=V(I,J,3)
            !W(I,J,1)=-W(I,J,4)
            !W(I,J,2)=-W(I,J,3)
            !
            !!U(I,J,KMAX+2+K)=U(I,J,KMAX+2)
            !V(I,J,KMAX+2+K)=V(I,J,KMAX+2)
            !W(I,J,KMAX+4)=-W(I,J,KMAX+1)
            !W(I,J,KMAX+3)=-W(I,J,KMAX+2)
            !
!********** No-slip Boundary **********!

            !U(I,J,1)=-U(I,J,4)
            !U(I,J,2)=-U(I,J,3)
            !U(I,J,KMAX+4)=-U(I,J,KMAX+1)
            !U(I,J,KMAX+3)=-U(I,J,KMAX+2)

 330    CONTINUE

!********** GROUND AND CEILING CONDITION **********!

        DO 411 K=3,KMAX+2
        DO 411 I=3,IMAX+2

            !SLIP for U
            !U(I,2,K)=U(I,IGL(I,K)+1,K)   !2.0*U(I,3,K)-U(I,4,K)
            !U(I,1,K)=U(I,IGL(I,K)+1,K)   !2.0*U(I,2,K)-U(I,3,K)

            U(I,1,K)=0.0
            U(I,2,K)=0.0

            V(I,1,K)=0.0
            V(I,2,K)=0.0

            W(I,1,K)=0.0
            W(I,2,K)=0.0
            
            !NO-SLIP for U where wall is J=2
            !U(I,1,K)=-U(I,3,K)  !sumo250826
            !U(I,2,K)=0.0  !sumo250826

            !NO-SLIP for U where wall is J=2.5
            !U(I,1,K)=-U(I,4,K)
            !U(I,2,K)=-U(I,3,K)

            !NO-SLIP for U where wall is J=2
            !W(I,1,K)=-W(I,3,K)
            !W(I,2,K)=0.0

            !NO-SLIP for W where wall is J=2.5
            !W(I,1,K)=-W(I,4,K)
            !W(I,2,K)=-W(I,3,K)

            !NO-SLIP for V where wall is J=2
            !V(I,1,K)=-V(I,3,K)
            !V(I,2,K)=0.0

            !NO-SLIP for V where wall is J=2.5
            !V(I,1,K)=-V(I,4,K)
            !V(I,2,K)=-V(I,3,K)

            !SLIP for U and W
            U(I,JMAX+2,K)=U(I,JMAX+1,K)
            U(I,JMAX+3,K)=U(I,JMAX+1,K)
            W(I,JMAX+2,K)=W(I,JMAX+1,K)
            W(I,JMAX+3,K)=W(I,JMAX+1,K)

            !NO-SLIP for V where wall is J=JMAX+2
            V(I,JMAX+2,K)=0.0
            V(I,JMAX+3,K)=-V(I,JMAX+1,K)

            !NO-SLIP for V where wall is J=JMAX+2.5
            !V(I,JMAX+2,K)=-V(I,JMAX,K)
            !V(I,JMAX+3,K)=-V(I,JMAX-1,K)
            
!       U(I,JMAX+2,K)=0.0
!       U(I,JMAX+3,K)=0.0
!       V(I,JMAX+2,K)=0.0
!       V(I,JMAX+3,K)=-V(I,JMAX+1,K)
!       W(I,JMAX+2,K)=0.0
!       W(I,JMAX+3,K)=0.0

!       U(I,JMAX+1,K)=U(I,JMAX,K)
!     &            *(1.0/CAR*LOG(USTAu1(I)*1.0/EY(3)/RED)+AS)
!     &            /(1.0/CAR*LOG(USTAu1(I)*2.0/EY(3)/RED)+AS)
!       W(I,JMAX+1,K)=W(I,JMAX,K)
!     &            *(1.0/CAR*LOG(VSTAu1(I)*1.0/EY(3)/RED)+AS)
!     &            /(1.0/CAR*LOG(VSTAu1(I)*2.0/EY(3)/RED)+AS)
!CCCCC       V(I,JMAX+1,K)=(U(I+1,JMAX+1,K)-U(I-1,JMAX+1,K))*0.50*GX(I)/EY(4)
!       V(I,JMAX+1,K)=((U(I+1,JMAX+1,K)-U(I-1,JMAX+1,K))*0.50*GX(I)
!     &               +(W(I,JMAX+1,K+1)-W(I,JMAX+1,K-1))*0.50*DZ(K))
!     &               /EY(4)
!       USTAu(I)=ABS(U(I,JMAX+1,K)
!     &        /(1.0/CAR*LOG(USTAu1(I)*1.0/EY(3)/RED)+AS))+0.01
!       VSTAu(I)=ABS(W(I,JMAX+1,K)
!     &        /(1.0/CAR*LOG(VSTAu1(I)*1.0/EY(3)/RED)+AS))+0.01
!CC
!CC        U(I,3,K)=U(I,4,K)*(1.0/CAR*LOG(USTA1(I)*1.0/EY(3)/RED)+AS)
!CC     &                    /(1.0/CAR*LOG(USTA1(I)*2.0/EY(3)/RED)+AS)
!CC        V(I,3,K)=-(U(I+1,3,K)-U(I-1,3,K))*0.50*GX(I)/EY(4)
!CC        USTA(I)=ABS(U(I,3,K)
!CC     &        /(1.0/CAR*LOG(USTA1(I)*1.0/EY(3)/RED)+AS))

            !U(I,2,K)=0.0
            !V(I,2,K)=0.0
            !W(I,2,K)=0.0

 411    CONTINUE

!/////////////// Surface Boundary Condition ///////////////!

!$OMP PARALLEL
!$OMP DO
        DO 5226 K=2,KMAX+3
        DO 5226 I=2,IMAX+3

            U(I,IETA(I,K),K)=U(I,IETA(I,K)-1,K)
            W(I,IETA(I,K),K)=W(I,IETA(I,K)-1,K)
            U(I,IETA(I,K)+1,K)=U(I,IETA(I,K)-1,K)
            W(I,IETA(I,K)+1,K)=W(I,IETA(I,K)-1,K)
            U(I,IETA(I,K)+2,K)=U(I,IETA(I,K)-1,K)
            W(I,IETA(I,K)+2,K)=W(I,IETA(I,K)-1,K)

            DUUDX(I,K)=U(I,IETA(I,K),K)*(ELES(I+1,K)-ELES(I-1,K))*0.50*GX(I) &
                    -ABS(U(I,IETA(I,K),K))*(ELES(I+1,K)-2.0*ELES(I,K)+ELES(I-1,K))*0.50*GX(I)
            DWWDZ(I,K)=W(I,IETA(I,K),K)*(ELES(I,K+1)-ELES(I,K-1))*0.50*DZ(K) &
                    -ABS(W(I,IETA(I,K),K))*(ELES(I,K+1)-2.0*ELES(I,K)+ELES(I,K-1))*0.50*DZ(K)

            !DUUDX(I,K)=U(I,IETA(I,K),K)*(ELES(I+1,K)-ELES(I-1,K))*0.50*GX(I)
            !DWWDZ(I,K)=W(I,IETA(I,K),K)*(ELES(I,K+1)-ELES(I,K-1))*0.50*DZ(K)

            !DUUDX(I,K)=U(I,IETA(I,K),K)* &
            !            (0.500*(ELES(I+1,K)+ELES(I,K))-0.500*(ELES(I,K)+ELES(I-1,K)))*GX(I)
            !DWWDZ(I,K)=W(I,IETA(I,K),K)* &
            !            (0.500*(ELES(I,K+1)+ELES(I,K))-0.500*(ELES(I,K)+ELES(I,K-1)))*DZ(K)

            V(I,IETA(I,K),K)=(ELES(I,K)-ELES1(I,K))/DT+DUUDX(I,K)+DWWDZ(I,K) !sumo250829
            !V(I,IETA(I,K),K)=(ELES(I,K)-ELES1(I,K))/DT*0.0+DUUDX(I,K)*0.0+DWWDZ(I,K) !sumo250829

            V(I,IETA(I,K)+1,K)=V(I,IETA(I,K),K)
            V(I,IETA(I,K)+2,K)=V(I,IETA(I,K),K)

            !V(I,IETA(I,K)+1,K)=-V(I,IETA(I,K),K)
            !V(I,IETA(I,K)+2,K)=-V(I,IETA(I,K)-1,K)

            !IF(TIME.LT.10.0) THEN
            !    V(I,IETA(I,K),K)=0.0
            !    V(I,IETA(I,K)+1,K)=-V(I,IETA(I,K)-1,K)
            !    V(I,IETA(I,K)+2,K)=-V(I,IETA(I,K)-2,K)    !Revised by SUMO241018
            !ELSE
                !V(I,IETA(I,K),K)=V(I,IETA(I,K)-1,K)   !Revised by SUMO241018
                !V(I,IETA(I,K)+1,K)=0.0   !Revised by SUMO241018
                !V(I,IETA(I,K)+2,K)=-V(I,IETA(I,K)-1,K)    !Revised by SUMO241018
            !END IF

5226   CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!!$OMP PARALLEL
!!$OMP DO
        !DO 5227 K=3,KMAX+2
        !DO 5227 I=3,IMAX+2
        !DO 5227 J=3,IGL(I,K)

            !U(I,J,K)=0.0
            !V(I,J,K)=0.0
            !W(I,J,K)=0.0

5227   CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL

!CCCCCC////// Pier Boundary Condition ////////CCCCCC

        IF(pier.EQ.1.0) THEN

            DO 701 J=1,JMAX+3
            DO 701 K=Kpier_R,Kpier_L
            DO 701 I=Ipier_U,Ipier_D

                U(I,J,K)=0.0
                V(I,J,K)=0.0
                W(I,J,K)=0.0

                !U(Ipier_U,J,K)=-U(Ipier_U-1,J,K)
                !U(Ipier_U+1,J,K)=-U(Ipier_U-2,J,K)
                !U(Ipier_D,J,K)=-U(Ipier_D+1,J,K)
                !U(Ipier_D-1,J,K)=-U(Ipier_D+2,J,K)

                !U(I,J,Kpier_L)=-U(I,J,Kpier_L+1)
                !U(I,J,Kpier_L-1)=-U(I,J,Kpier_L+2)

                !W(I,J,Kpier_R)=-W(I,J,Kpier_R-1)
                !W(I,J,Kpier_R+1)=-W(I,J,Kpier_R-2)
                !W(I,J,Kpier_L)=-W(I,J,Kpier_L+1)
                !W(I,J,Kpier_L-1)=-W(I,J,Kpier_L+2)

 701    CONTINUE

        !END IF
        !IF(pier.EQ.1.0) THEN

            !DO 702 J=1,JMAX+3
            !DO 702 K=Kpier_R1,Kpier_L1
            !DO 702 I=Ipier_U1,Ipier_D1

                !U(I,J,K)=0.0
                !V(I,J,K)=0.0
                !W(I,J,K)=0.0

                !U(Ipier_U1,J,K)=-U(Ipier_U1-1,J,K)
                !U(Ipier_U1+1,J,K)=-U(Ipier_U1-2,J,K)
                !U(Ipier_D1,J,K)=-U(Ipier_D1+1,J,K)
                !U(Ipier_D1-1,J,K)=-U(Ipier_D1+2,J,K)

                !!W(I,J,Kpier_R1)=-W(I,J,Kpier_R1-1)
                !!W(I,J,Kpier_R1+1)=-W(I,J,Kpier_R1-2)
                !W(I,J,Kpier_L1)=-W(I,J,Kpier_L1+1)
                !W(I,J,Kpier_L1-1)=-W(I,J,Kpier_L1+2)

 702    CONTINUE

        !END IF
        !IF(pier.EQ.1.0) THEN

            !DO 703 J=1,JMAX+3
            !DO 703 K=Kpier_R2,Kpier_L2
            !DO 703 I=Ipier_U2,Ipier_D2

                !U(I,J,K)=0.0
                !V(I,J,K)=0.0
                !W(I,J,K)=0.0

                !U(Ipier_U2,J,K)=-U(Ipier_U2-1,J,K)
                !U(Ipier_U2+1,J,K)=-U(Ipier_U2-2,J,K)
                !U(Ipier_D2,J,K)=-U(Ipier_D2+1,J,K)
                !U(Ipier_D2-1,J,K)=-U(Ipier_D2+2,J,K)

                !W(I,J,Kpier_R2)=-W(I,J,Kpier_R2-1)
                !W(I,J,Kpier_R2+1)=-W(I,J,Kpier_R2-2)
                !!W(I,J,Kpier_L2)=-W(I,J,Kpier_L2+1)
                !!W(I,J,Kpier_L2-1)=-W(I,J,Kpier_L2+2)

 703    CONTINUE

        !END IF
        !IF(pier.EQ.1.0) THEN

            !DO 704 J=1,JMAX+3
            !DO 704 K=Kpier_R3,Kpier_L3
            !DO 704 I=Ipier_U3,Ipier_D3

                !U(I,J,K)=0.0
                !V(I,J,K)=0.0
                !W(I,J,K)=0.0

                !U(Ipier_U3,J,K)=-U(Ipier_U3-1,J,K)
                !U(Ipier_U3+1,J,K)=-U(Ipier_U3-2,J,K)
                !U(Ipier_D3,J,K)=-U(Ipier_D3+1,J,K)
                !U(Ipier_D3-1,J,K)=-U(Ipier_D3+2,J,K)

                !!W(I,J,Kpier_R1)=-W(I,J,Kpier_R1-1)
                !!W(I,J,Kpier_R1+1)=-W(I,J,Kpier_R1-2)
                !W(I,J,Kpier_L3)=-W(I,J,Kpier_L3+1)
                !W(I,J,Kpier_L3-1)=-W(I,J,Kpier_L3+2)

 704    CONTINUE

        !END IF
        !IF(pier.EQ.1.0) THEN

            !DO 705 J=1,JMAX+3
            !DO 705 K=Kpier_R4,Kpier_L4
            !DO 705 I=Ipier_U4,Ipier_D4

                !U(I,J,K)=0.0
                !V(I,J,K)=0.0
                !W(I,J,K)=0.0

                !U(Ipier_U4,J,K)=-U(Ipier_U4-1,J,K)
                !U(Ipier_U4+1,J,K)=-U(Ipier_U4-2,J,K)
                !U(Ipier_D4,J,K)=-U(Ipier_D4+1,J,K)
                !U(Ipier_D4-1,J,K)=-U(Ipier_D4+2,J,K)

                !W(I,J,Kpier_R4)=-W(I,J,Kpier_R4-1)
                !W(I,J,Kpier_R4+1)=-W(I,J,Kpier_R4-2)
                !!W(I,J,Kpier_L2)=-W(I,J,Kpier_L2+1)
                !!W(I,J,Kpier_L2-1)=-W(I,J,Kpier_L2+2)

 705    CONTINUE

        END IF

!$OMP PARALLEL
!$OMP DO
        DO 105 K=3,KMAX+2
        DO 105 I=3,IMAX+2
        DO 105 J=3,JMAX+1

            UGA(I,J,K)=CCD*(-U(I+2,J,K)+8.0*(U(I+1,J,K) &
                            -U(I-1,J,K))+U(I-2,J,K))
            UEA(I,J,K)=CCD*(-U(I,J+2,K)+8.0*(U(I,J+1,K) &
                            -U(I,J-1,K))+U(I,J-2,K))
            UDA(I,J,K)=CCD*(-U(I,J,K+2)+8.0*(U(I,J,K+1) &
                            -U(I,J,K-1))+U(I,J,K-2))

            VGA(I,J,K)=CCD*(-V(I+2,J,K)+8.0*(V(I+1,J,K) &
                            -V(I-1,J,K))+V(I-2,J,K))
            VEA(I,J,K)=CCD*(-V(I,J+2,K)+8.0*(V(I,J+1,K) &
                            -V(I,J-1,K))+V(I,J-2,K))
            VDA(I,J,K)=CCD*(-V(I,J,K+2)+8.0*(V(I,J,K+1) &
                            -V(I,J,K-1))+V(I,J,K-2))

            WGA(I,J,K)=CCD*(-W(I+2,J,K)+8.0*(W(I+1,J,K) &
                            -W(I-1,J,K))+W(I-2,J,K))
            WEA(I,J,K)=CCD*(-W(I,J+2,K)+8.0*(W(I,J+1,K) &
                            -W(I,J-1,K))+W(I,J-2,K))
            WDA(I,J,K)=CCD*(-W(I,J,K+2)+8.0*(W(I,J,K+1) &
                            -W(I,J,K-1))+W(I,J,K-2))

            UGB(I,J,K)=CCD2*(U(I+2,J,K)-4.0*U(I+1,J,K) &
                        -4.0*U(I-1,J,K)+U(I-2,J,K))
            UEB(I,J,K)=CCD2*(U(I,J+2,K)-4.0*U(I,J+1,K) &
                        -4.0*U(I,J-1,K)+U(I,J-2,K))
            UDB(I,J,K)=CCD2*(U(I,J,K+2)-4.0*U(I,J,K+1) &
                        -4.0*U(I,J,K-1)+U(I,J,K-2))

            VGB(I,J,K)=CCD2*(V(I+2,J,K)-4.0*V(I+1,J,K) &
                        -4.0*V(I-1,J,K)+V(I-2,J,K))
            VEB(I,J,K)=CCD2*(V(I,J+2,K)-4.0*V(I,J+1,K) &
                        -4.0*V(I,J-1,K)+V(I,J-2,K))
            VDB(I,J,K)=CCD2*(V(I,J,K+2)-4.0*V(I,J,K+1) &
                        -4.0*V(I,J,K-1)+V(I,J,K-2))

            WGB(I,J,K)=CCD2*(W(I+2,J,K)-4.0*W(I+1,J,K) &
                        -4.0*W(I-1,J,K)+W(I-2,J,K))
            WEB(I,J,K)=CCD2*(W(I,J+2,K)-4.0*W(I,J+1,K) &
                        -4.0*W(I,J-1,K)+W(I,J-2,K))
            WDB(I,J,K)=CCD2*(W(I,J,K+2)-4.0*W(I,J,K+1) &
                        -4.0*W(I,J,K-1)+W(I,J,K-2))

 105    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 135 I=3,IMAX+2
        DO 135 J=3,JMAX+1
        DO 139 K=Kpier_R,Kpier_L

            IF(pier.EQ.1.0) THEN
                IF(I.EQ.Ipier_U-1) THEN
                    U(Ipier_U,J,K)=-U(Ipier_U-1,J,K)
                    U(Ipier_U+1,J,K)=-U(Ipier_U-2,J,K)
                    W(Ipier_U,J,K)=-W(Ipier_U-1,J,K)
                    W(Ipier_U+1,J,K)=-W(Ipier_U-2,J,K)
                    V(Ipier_U,J,K)=-V(Ipier_U-1,J,K)
                    V(Ipier_U+1,J,K)=-V(Ipier_U-2,J,K)
                    UGA(I,J,K)=CCD*(-U(I+2,J,K)+8.0*(U(I+1,J,K) &
                                    -U(I-1,J,K))+U(I-2,J,K))
                    UEA(I,J,K)=CCD*(-U(I,J+2,K)+8.0*(U(I,J+1,K) &
                                    -U(I,J-1,K))+U(I,J-2,K))
                    UDA(I,J,K)=CCD*(-U(I,J,K+2)+8.0*(U(I,J,K+1) &
                                    -U(I,J,K-1))+U(I,J,K-2))

                    VGA(I,J,K)=CCD*(-V(I+2,J,K)+8.0*(V(I+1,J,K) &
                                    -V(I-1,J,K))+V(I-2,J,K))
                    VEA(I,J,K)=CCD*(-V(I,J+2,K)+8.0*(V(I,J+1,K) &
                                    -V(I,J-1,K))+V(I,J-2,K))
                    VDA(I,J,K)=CCD*(-V(I,J,K+2)+8.0*(V(I,J,K+1) &
                                    -V(I,J,K-1))+V(I,J,K-2))

                    WGA(I,J,K)=CCD*(-W(I+2,J,K)+8.0*(W(I+1,J,K) &
                                    -W(I-1,J,K))+W(I-2,J,K))
                    WEA(I,J,K)=CCD*(-W(I,J+2,K)+8.0*(W(I,J+1,K) &
                                    -W(I,J-1,K))+W(I,J-2,K))
                    WDA(I,J,K)=CCD*(-W(I,J,K+2)+8.0*(W(I,J,K+1) &
                                    -W(I,J,K-1))+W(I,J,K-2))

                    UGB(I,J,K)=CCD2*(U(I+2,J,K)-4.0*U(I+1,J,K) &
                                -4.0*U(I-1,J,K)+U(I-2,J,K))
                    UEB(I,J,K)=CCD2*(U(I,J+2,K)-4.0*U(I,J+1,K) &
                                -4.0*U(I,J-1,K)+U(I,J-2,K))
                    UDB(I,J,K)=CCD2*(U(I,J,K+2)-4.0*U(I,J,K+1) &
                                -4.0*U(I,J,K-1)+U(I,J,K-2))

                    VGB(I,J,K)=CCD2*(V(I+2,J,K)-4.0*V(I+1,J,K) &
                                -4.0*V(I-1,J,K)+V(I-2,J,K))
                    VEB(I,J,K)=CCD2*(V(I,J+2,K)-4.0*V(I,J+1,K) &
                                -4.0*V(I,J-1,K)+V(I,J-2,K))
                    VDB(I,J,K)=CCD2*(V(I,J,K+2)-4.0*V(I,J,K+1) &
                                -4.0*V(I,J,K-1)+V(I,J,K-2))

                    WGB(I,J,K)=CCD2*(W(I+2,J,K)-4.0*W(I+1,J,K) &
                                -4.0*W(I-1,J,K)+W(I-2,J,K))
                    WEB(I,J,K)=CCD2*(W(I,J+2,K)-4.0*W(I,J+1,K) &
                                -4.0*W(I,J-1,K)+W(I,J-2,K))
                    WDB(I,J,K)=CCD2*(W(I,J,K+2)-4.0*W(I,J,K+1) &
                                -4.0*W(I,J,K-1)+W(I,J,K-2))
                END IF
                IF(I.EQ.Ipier_U-2) THEN
                    U(Ipier_U,J,K)=-U(Ipier_U-1,J,K)
                    W(Ipier_U,J,K)=-W(Ipier_U-1,J,K)
                    V(Ipier_U,J,K)=-V(Ipier_U-1,J,K)
                    UGA(I,J,K)=CCD*(-U(I+2,J,K)+8.0*(U(I+1,J,K) &
                                    -U(I-1,J,K))+U(I-2,J,K))
                    UEA(I,J,K)=CCD*(-U(I,J+2,K)+8.0*(U(I,J+1,K) &
                                    -U(I,J-1,K))+U(I,J-2,K))
                    UDA(I,J,K)=CCD*(-U(I,J,K+2)+8.0*(U(I,J,K+1) &
                                    -U(I,J,K-1))+U(I,J,K-2))

                    VGA(I,J,K)=CCD*(-V(I+2,J,K)+8.0*(V(I+1,J,K) &
                                    -V(I-1,J,K))+V(I-2,J,K))
                    VEA(I,J,K)=CCD*(-V(I,J+2,K)+8.0*(V(I,J+1,K) &
                                    -V(I,J-1,K))+V(I,J-2,K))
                    VDA(I,J,K)=CCD*(-V(I,J,K+2)+8.0*(V(I,J,K+1) &
                                    -V(I,J,K-1))+V(I,J,K-2))

                    WGA(I,J,K)=CCD*(-W(I+2,J,K)+8.0*(W(I+1,J,K) &
                                    -W(I-1,J,K))+W(I-2,J,K))
                    WEA(I,J,K)=CCD*(-W(I,J+2,K)+8.0*(W(I,J+1,K) &
                                    -W(I,J-1,K))+W(I,J-2,K))
                    WDA(I,J,K)=CCD*(-W(I,J,K+2)+8.0*(W(I,J,K+1) &
                                    -W(I,J,K-1))+W(I,J,K-2))

                    UGB(I,J,K)=CCD2*(U(I+2,J,K)-4.0*U(I+1,J,K) &
                                -4.0*U(I-1,J,K)+U(I-2,J,K))
                    UEB(I,J,K)=CCD2*(U(I,J+2,K)-4.0*U(I,J+1,K) &
                                -4.0*U(I,J-1,K)+U(I,J-2,K))
                    UDB(I,J,K)=CCD2*(U(I,J,K+2)-4.0*U(I,J,K+1) &
                                -4.0*U(I,J,K-1)+U(I,J,K-2))

                    VGB(I,J,K)=CCD2*(V(I+2,J,K)-4.0*V(I+1,J,K) &
                                -4.0*V(I-1,J,K)+V(I-2,J,K))
                    VEB(I,J,K)=CCD2*(V(I,J+2,K)-4.0*V(I,J+1,K) &
                                -4.0*V(I,J-1,K)+V(I,J-2,K))
                    VDB(I,J,K)=CCD2*(V(I,J,K+2)-4.0*V(I,J,K+1) &
                                -4.0*V(I,J,K-1)+V(I,J,K-2))

                    WGB(I,J,K)=CCD2*(W(I+2,J,K)-4.0*W(I+1,J,K) &
                                -4.0*W(I-1,J,K)+W(I-2,J,K))
                    WEB(I,J,K)=CCD2*(W(I,J+2,K)-4.0*W(I,J+1,K) &
                                -4.0*W(I,J-1,K)+W(I,J-2,K))
                    WDB(I,J,K)=CCD2*(W(I,J,K+2)-4.0*W(I,J,K+1) &
                                -4.0*W(I,J,K-1)+W(I,J,K-2))
                END IF
                IF(I.EQ.Ipier_D+1) THEN
                    U(Ipier_D,J,K)=-U(Ipier_D+1,J,K)
                    U(Ipier_D-1,J,K)=-U(Ipier_D+2,J,K)
                    W(Ipier_D,J,K)=-W(Ipier_D+1,J,K)
                    W(Ipier_D-1,J,K)=-W(Ipier_D+2,J,K)
                    V(Ipier_D,J,K)=-V(Ipier_D+1,J,K)
                    V(Ipier_D-1,J,K)=-V(Ipier_D+2,J,K)
                    UGA(I,J,K)=CCD*(-U(I+2,J,K)+8.0*(U(I+1,J,K) &
                                    -U(I-1,J,K))+U(I-2,J,K))
                    UEA(I,J,K)=CCD*(-U(I,J+2,K)+8.0*(U(I,J+1,K) &
                                    -U(I,J-1,K))+U(I,J-2,K))
                    UDA(I,J,K)=CCD*(-U(I,J,K+2)+8.0*(U(I,J,K+1) &
                                    -U(I,J,K-1))+U(I,J,K-2))

                    VGA(I,J,K)=CCD*(-V(I+2,J,K)+8.0*(V(I+1,J,K) &
                                    -V(I-1,J,K))+V(I-2,J,K))
                    VEA(I,J,K)=CCD*(-V(I,J+2,K)+8.0*(V(I,J+1,K) &
                                    -V(I,J-1,K))+V(I,J-2,K))
                    VDA(I,J,K)=CCD*(-V(I,J,K+2)+8.0*(V(I,J,K+1) &
                                    -V(I,J,K-1))+V(I,J,K-2))

                    WGA(I,J,K)=CCD*(-W(I+2,J,K)+8.0*(W(I+1,J,K) &
                                    -W(I-1,J,K))+W(I-2,J,K))
                    WEA(I,J,K)=CCD*(-W(I,J+2,K)+8.0*(W(I,J+1,K) &
                                    -W(I,J-1,K))+W(I,J-2,K))
                    WDA(I,J,K)=CCD*(-W(I,J,K+2)+8.0*(W(I,J,K+1) &
                                    -W(I,J,K-1))+W(I,J,K-2))

                    UGB(I,J,K)=CCD2*(U(I+2,J,K)-4.0*U(I+1,J,K) &
                                -4.0*U(I-1,J,K)+U(I-2,J,K))
                    UEB(I,J,K)=CCD2*(U(I,J+2,K)-4.0*U(I,J+1,K) &
                                -4.0*U(I,J-1,K)+U(I,J-2,K))
                    UDB(I,J,K)=CCD2*(U(I,J,K+2)-4.0*U(I,J,K+1) &
                                -4.0*U(I,J,K-1)+U(I,J,K-2))

                    VGB(I,J,K)=CCD2*(V(I+2,J,K)-4.0*V(I+1,J,K) &
                                -4.0*V(I-1,J,K)+V(I-2,J,K))
                    VEB(I,J,K)=CCD2*(V(I,J+2,K)-4.0*V(I,J+1,K) &
                                -4.0*V(I,J-1,K)+V(I,J-2,K))
                    VDB(I,J,K)=CCD2*(V(I,J,K+2)-4.0*V(I,J,K+1) &
                                -4.0*V(I,J,K-1)+V(I,J,K-2))

                    WGB(I,J,K)=CCD2*(W(I+2,J,K)-4.0*W(I+1,J,K) &
                                -4.0*W(I-1,J,K)+W(I-2,J,K))
                    WEB(I,J,K)=CCD2*(W(I,J+2,K)-4.0*W(I,J+1,K) &
                                -4.0*W(I,J-1,K)+W(I,J-2,K))
                    WDB(I,J,K)=CCD2*(W(I,J,K+2)-4.0*W(I,J,K+1) &
                                -4.0*W(I,J,K-1)+W(I,J,K-2))
                END IF
                IF(I.EQ.Ipier_D+2) THEN
                    U(Ipier_D,J,K)=-U(Ipier_D+1,J,K)
                    W(Ipier_D,J,K)=-W(Ipier_D+1,J,K)
                    V(Ipier_D,J,K)=-V(Ipier_D+1,J,K)
                    UGA(I,J,K)=CCD*(-U(I+2,J,K)+8.0*(U(I+1,J,K) &
                                    -U(I-1,J,K))+U(I-2,J,K))
                    UEA(I,J,K)=CCD*(-U(I,J+2,K)+8.0*(U(I,J+1,K) &
                                    -U(I,J-1,K))+U(I,J-2,K))
                    UDA(I,J,K)=CCD*(-U(I,J,K+2)+8.0*(U(I,J,K+1) &
                                    -U(I,J,K-1))+U(I,J,K-2))

                    VGA(I,J,K)=CCD*(-V(I+2,J,K)+8.0*(V(I+1,J,K) &
                                    -V(I-1,J,K))+V(I-2,J,K))
                    VEA(I,J,K)=CCD*(-V(I,J+2,K)+8.0*(V(I,J+1,K) &
                                    -V(I,J-1,K))+V(I,J-2,K))
                    VDA(I,J,K)=CCD*(-V(I,J,K+2)+8.0*(V(I,J,K+1) &
                                    -V(I,J,K-1))+V(I,J,K-2))

                    WGA(I,J,K)=CCD*(-W(I+2,J,K)+8.0*(W(I+1,J,K) &
                                    -W(I-1,J,K))+W(I-2,J,K))
                    WEA(I,J,K)=CCD*(-W(I,J+2,K)+8.0*(W(I,J+1,K) &
                                    -W(I,J-1,K))+W(I,J-2,K))
                    WDA(I,J,K)=CCD*(-W(I,J,K+2)+8.0*(W(I,J,K+1) &
                                    -W(I,J,K-1))+W(I,J,K-2))

                    UGB(I,J,K)=CCD2*(U(I+2,J,K)-4.0*U(I+1,J,K) &
                                -4.0*U(I-1,J,K)+U(I-2,J,K))
                    UEB(I,J,K)=CCD2*(U(I,J+2,K)-4.0*U(I,J+1,K) &
                                -4.0*U(I,J-1,K)+U(I,J-2,K))
                    UDB(I,J,K)=CCD2*(U(I,J,K+2)-4.0*U(I,J,K+1) &
                                -4.0*U(I,J,K-1)+U(I,J,K-2))

                    VGB(I,J,K)=CCD2*(V(I+2,J,K)-4.0*V(I+1,J,K) &
                                -4.0*V(I-1,J,K)+V(I-2,J,K))
                    VEB(I,J,K)=CCD2*(V(I,J+2,K)-4.0*V(I,J+1,K) &
                                -4.0*V(I,J-1,K)+V(I,J-2,K))
                    VDB(I,J,K)=CCD2*(V(I,J,K+2)-4.0*V(I,J,K+1) &
                                -4.0*V(I,J,K-1)+V(I,J,K-2))

                    WGB(I,J,K)=CCD2*(W(I+2,J,K)-4.0*W(I+1,J,K) &
                                -4.0*W(I-1,J,K)+W(I-2,J,K))
                    WEB(I,J,K)=CCD2*(W(I,J+2,K)-4.0*W(I,J+1,K) &
                                -4.0*W(I,J-1,K)+W(I,J-2,K))
                    WDB(I,J,K)=CCD2*(W(I,J,K+2)-4.0*W(I,J,K+1) &
                                -4.0*W(I,J,K-1)+W(I,J,K-2))
                END IF
            END IF
 139    CONTINUE
 135    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 165 K=3,KMAX+2
        DO 165 J=3,JMAX+1
        DO 166 I=Ipier_U,Ipier_D

            IF(pier.EQ.1.0) THEN

                IF(K.EQ.Kpier_R-1) THEN
                    W(I,J,Kpier_R)=-W(I,J,Kpier_R-1)
                    W(I,J,Kpier_R+1)=-W(I,J,Kpier_R-2)
                    U(I,J,Kpier_R)=-U(I,J,Kpier_R-1)
                    U(I,J,Kpier_R+1)=-U(I,J,Kpier_R-2)
                    V(I,J,Kpier_R)=-V(I,J,Kpier_R-1)
                    V(I,J,Kpier_R+1)=-V(I,J,Kpier_R-2)
                    UGA(I,J,K)=CCD*(-U(I+2,J,K)+8.0*(U(I+1,J,K) &
                                    -U(I-1,J,K))+U(I-2,J,K))
                    UEA(I,J,K)=CCD*(-U(I,J+2,K)+8.0*(U(I,J+1,K) &
                                    -U(I,J-1,K))+U(I,J-2,K))
                    UDA(I,J,K)=CCD*(-U(I,J,K+2)+8.0*(U(I,J,K+1) &
                                    -U(I,J,K-1))+U(I,J,K-2))

                    VGA(I,J,K)=CCD*(-V(I+2,J,K)+8.0*(V(I+1,J,K) &
                                    -V(I-1,J,K))+V(I-2,J,K))
                    VEA(I,J,K)=CCD*(-V(I,J+2,K)+8.0*(V(I,J+1,K) &
                                    -V(I,J-1,K))+V(I,J-2,K))
                    VDA(I,J,K)=CCD*(-V(I,J,K+2)+8.0*(V(I,J,K+1) &
                                    -V(I,J,K-1))+V(I,J,K-2))

                    WGA(I,J,K)=CCD*(-W(I+2,J,K)+8.0*(W(I+1,J,K) &
                                    -W(I-1,J,K))+W(I-2,J,K))
                    WEA(I,J,K)=CCD*(-W(I,J+2,K)+8.0*(W(I,J+1,K) &
                                    -W(I,J-1,K))+W(I,J-2,K))
                    WDA(I,J,K)=CCD*(-W(I,J,K+2)+8.0*(W(I,J,K+1) &
                                    -W(I,J,K-1))+W(I,J,K-2))

                    UGB(I,J,K)=CCD2*(U(I+2,J,K)-4.0*U(I+1,J,K) &
                                -4.0*U(I-1,J,K)+U(I-2,J,K))
                    UEB(I,J,K)=CCD2*(U(I,J+2,K)-4.0*U(I,J+1,K) &
                                -4.0*U(I,J-1,K)+U(I,J-2,K))
                    UDB(I,J,K)=CCD2*(U(I,J,K+2)-4.0*U(I,J,K+1) &
                                -4.0*U(I,J,K-1)+U(I,J,K-2))

                    VGB(I,J,K)=CCD2*(V(I+2,J,K)-4.0*V(I+1,J,K) &
                                -4.0*V(I-1,J,K)+V(I-2,J,K))
                    VEB(I,J,K)=CCD2*(V(I,J+2,K)-4.0*V(I,J+1,K) &
                                -4.0*V(I,J-1,K)+V(I,J-2,K))
                    VDB(I,J,K)=CCD2*(V(I,J,K+2)-4.0*V(I,J,K+1) &
                                -4.0*V(I,J,K-1)+V(I,J,K-2))

                    WGB(I,J,K)=CCD2*(W(I+2,J,K)-4.0*W(I+1,J,K) &
                                -4.0*W(I-1,J,K)+W(I-2,J,K))
                    WEB(I,J,K)=CCD2*(W(I,J+2,K)-4.0*W(I,J+1,K) &
                                -4.0*W(I,J-1,K)+W(I,J-2,K))
                    WDB(I,J,K)=CCD2*(W(I,J,K+2)-4.0*W(I,J,K+1) &
                                -4.0*W(I,J,K-1)+W(I,J,K-2))
                END IF
                IF(K.EQ.Kpier_R-2) THEN
                    W(I,J,Kpier_R)=-W(I,J,Kpier_R-1)
                    U(I,J,Kpier_R)=-U(I,J,Kpier_R-1)
                    V(I,J,Kpier_R)=-V(I,J,Kpier_R-1)
                    UGA(I,J,K)=CCD*(-U(I+2,J,K)+8.0*(U(I+1,J,K) &
                                    -U(I-1,J,K))+U(I-2,J,K))
                    UEA(I,J,K)=CCD*(-U(I,J+2,K)+8.0*(U(I,J+1,K) &
                                    -U(I,J-1,K))+U(I,J-2,K))
                    UDA(I,J,K)=CCD*(-U(I,J,K+2)+8.0*(U(I,J,K+1) &
                                    -U(I,J,K-1))+U(I,J,K-2))

                    VGA(I,J,K)=CCD*(-V(I+2,J,K)+8.0*(V(I+1,J,K) &
                                    -V(I-1,J,K))+V(I-2,J,K))
                    VEA(I,J,K)=CCD*(-V(I,J+2,K)+8.0*(V(I,J+1,K) &
                                    -V(I,J-1,K))+V(I,J-2,K))
                    VDA(I,J,K)=CCD*(-V(I,J,K+2)+8.0*(V(I,J,K+1) &
                                    -V(I,J,K-1))+V(I,J,K-2))

                    WGA(I,J,K)=CCD*(-W(I+2,J,K)+8.0*(W(I+1,J,K) &
                                    -W(I-1,J,K))+W(I-2,J,K))
                    WEA(I,J,K)=CCD*(-W(I,J+2,K)+8.0*(W(I,J+1,K) &
                                    -W(I,J-1,K))+W(I,J-2,K))
                    WDA(I,J,K)=CCD*(-W(I,J,K+2)+8.0*(W(I,J,K+1) &
                                    -W(I,J,K-1))+W(I,J,K-2))

                    UGB(I,J,K)=CCD2*(U(I+2,J,K)-4.0*U(I+1,J,K) &
                                -4.0*U(I-1,J,K)+U(I-2,J,K))
                    UEB(I,J,K)=CCD2*(U(I,J+2,K)-4.0*U(I,J+1,K) &
                                -4.0*U(I,J-1,K)+U(I,J-2,K))
                    UDB(I,J,K)=CCD2*(U(I,J,K+2)-4.0*U(I,J,K+1) &
                                -4.0*U(I,J,K-1)+U(I,J,K-2))

                    VGB(I,J,K)=CCD2*(V(I+2,J,K)-4.0*V(I+1,J,K) &
                                -4.0*V(I-1,J,K)+V(I-2,J,K))
                    VEB(I,J,K)=CCD2*(V(I,J+2,K)-4.0*V(I,J+1,K) &
                                -4.0*V(I,J-1,K)+V(I,J-2,K))
                    VDB(I,J,K)=CCD2*(V(I,J,K+2)-4.0*V(I,J,K+1) &
                                -4.0*V(I,J,K-1)+V(I,J,K-2))

                    WGB(I,J,K)=CCD2*(W(I+2,J,K)-4.0*W(I+1,J,K) &
                                -4.0*W(I-1,J,K)+W(I-2,J,K))
                    WEB(I,J,K)=CCD2*(W(I,J+2,K)-4.0*W(I,J+1,K) &
                                -4.0*W(I,J-1,K)+W(I,J-2,K))
                    WDB(I,J,K)=CCD2*(W(I,J,K+2)-4.0*W(I,J,K+1) &
                                -4.0*W(I,J,K-1)+W(I,J,K-2))
                END IF
                IF(K.EQ.Kpier_L+1) THEN
                    W(I,J,Kpier_L)=-W(I,J,Kpier_L+1)
                    W(I,J,Kpier_L-1)=-W(I,J,Kpier_L+2)
                    U(I,J,Kpier_L)=-U(I,J,Kpier_L+1)
                    U(I,J,Kpier_L-1)=-U(I,J,Kpier_L+2)
                    V(I,J,Kpier_L)=-V(I,J,Kpier_L+1)
                    V(I,J,Kpier_L-1)=-V(I,J,Kpier_L+2)
                    UGA(I,J,K)=CCD*(-U(I+2,J,K)+8.0*(U(I+1,J,K) &
                                    -U(I-1,J,K))+U(I-2,J,K))
                    UEA(I,J,K)=CCD*(-U(I,J+2,K)+8.0*(U(I,J+1,K) &
                                    -U(I,J-1,K))+U(I,J-2,K))
                    UDA(I,J,K)=CCD*(-U(I,J,K+2)+8.0*(U(I,J,K+1) &
                                    -U(I,J,K-1))+U(I,J,K-2))

                    VGA(I,J,K)=CCD*(-V(I+2,J,K)+8.0*(V(I+1,J,K) &
                                    -V(I-1,J,K))+V(I-2,J,K))
                    VEA(I,J,K)=CCD*(-V(I,J+2,K)+8.0*(V(I,J+1,K) &
                                    -V(I,J-1,K))+V(I,J-2,K))
                    VDA(I,J,K)=CCD*(-V(I,J,K+2)+8.0*(V(I,J,K+1) &
                                    -V(I,J,K-1))+V(I,J,K-2))

                    WGA(I,J,K)=CCD*(-W(I+2,J,K)+8.0*(W(I+1,J,K) &
                                    -W(I-1,J,K))+W(I-2,J,K))
                    WEA(I,J,K)=CCD*(-W(I,J+2,K)+8.0*(W(I,J+1,K) &
                                    -W(I,J-1,K))+W(I,J-2,K))
                    WDA(I,J,K)=CCD*(-W(I,J,K+2)+8.0*(W(I,J,K+1) &
                                    -W(I,J,K-1))+W(I,J,K-2))

                    UGB(I,J,K)=CCD2*(U(I+2,J,K)-4.0*U(I+1,J,K) &
                                -4.0*U(I-1,J,K)+U(I-2,J,K))
                    UEB(I,J,K)=CCD2*(U(I,J+2,K)-4.0*U(I,J+1,K) &
                                -4.0*U(I,J-1,K)+U(I,J-2,K))
                    UDB(I,J,K)=CCD2*(U(I,J,K+2)-4.0*U(I,J,K+1) &
                                -4.0*U(I,J,K-1)+U(I,J,K-2))

                    VGB(I,J,K)=CCD2*(V(I+2,J,K)-4.0*V(I+1,J,K) &
                                -4.0*V(I-1,J,K)+V(I-2,J,K))
                    VEB(I,J,K)=CCD2*(V(I,J+2,K)-4.0*V(I,J+1,K) &
                                -4.0*V(I,J-1,K)+V(I,J-2,K))
                    VDB(I,J,K)=CCD2*(V(I,J,K+2)-4.0*V(I,J,K+1) &
                                -4.0*V(I,J,K-1)+V(I,J,K-2))

                    WGB(I,J,K)=CCD2*(W(I+2,J,K)-4.0*W(I+1,J,K) &
                                -4.0*W(I-1,J,K)+W(I-2,J,K))
                    WEB(I,J,K)=CCD2*(W(I,J+2,K)-4.0*W(I,J+1,K) &
                                -4.0*W(I,J-1,K)+W(I,J-2,K))
                    WDB(I,J,K)=CCD2*(W(I,J,K+2)-4.0*W(I,J,K+1) &
                                -4.0*W(I,J,K-1)+W(I,J,K-2))
                END IF
                IF(K.EQ.Kpier_L+2) THEN
                    W(I,J,Kpier_L)=-W(I,J,Kpier_L+1)
                    U(I,J,Kpier_L)=-U(I,J,Kpier_L+1)
                    V(I,J,Kpier_L)=-V(I,J,Kpier_L+1)
                    UGA(I,J,K)=CCD*(-U(I+2,J,K)+8.0*(U(I+1,J,K) &
                                    -U(I-1,J,K))+U(I-2,J,K))
                    UEA(I,J,K)=CCD*(-U(I,J+2,K)+8.0*(U(I,J+1,K) &
                                    -U(I,J-1,K))+U(I,J-2,K))
                    UDA(I,J,K)=CCD*(-U(I,J,K+2)+8.0*(U(I,J,K+1) &
                                    -U(I,J,K-1))+U(I,J,K-2))

                    VGA(I,J,K)=CCD*(-V(I+2,J,K)+8.0*(V(I+1,J,K) &
                                    -V(I-1,J,K))+V(I-2,J,K))
                    VEA(I,J,K)=CCD*(-V(I,J+2,K)+8.0*(V(I,J+1,K) &
                                    -V(I,J-1,K))+V(I,J-2,K))
                    VDA(I,J,K)=CCD*(-V(I,J,K+2)+8.0*(V(I,J,K+1) &
                                    -V(I,J,K-1))+V(I,J,K-2))

                    WGA(I,J,K)=CCD*(-W(I+2,J,K)+8.0*(W(I+1,J,K) &
                                    -W(I-1,J,K))+W(I-2,J,K))
                    WEA(I,J,K)=CCD*(-W(I,J+2,K)+8.0*(W(I,J+1,K) &
                                    -W(I,J-1,K))+W(I,J-2,K))
                    WDA(I,J,K)=CCD*(-W(I,J,K+2)+8.0*(W(I,J,K+1) &
                                    -W(I,J,K-1))+W(I,J,K-2))

                    UGB(I,J,K)=CCD2*(U(I+2,J,K)-4.0*U(I+1,J,K) &
                                -4.0*U(I-1,J,K)+U(I-2,J,K))
                    UEB(I,J,K)=CCD2*(U(I,J+2,K)-4.0*U(I,J+1,K) &
                                -4.0*U(I,J-1,K)+U(I,J-2,K))
                    UDB(I,J,K)=CCD2*(U(I,J,K+2)-4.0*U(I,J,K+1) &
                                -4.0*U(I,J,K-1)+U(I,J,K-2))

                    VGB(I,J,K)=CCD2*(V(I+2,J,K)-4.0*V(I+1,J,K) &
                                -4.0*V(I-1,J,K)+V(I-2,J,K))
                    VEB(I,J,K)=CCD2*(V(I,J+2,K)-4.0*V(I,J+1,K) &
                                -4.0*V(I,J-1,K)+V(I,J-2,K))
                    VDB(I,J,K)=CCD2*(V(I,J,K+2)-4.0*V(I,J,K+1) &
                                -4.0*V(I,J,K-1)+V(I,J,K-2))

                    WGB(I,J,K)=CCD2*(W(I+2,J,K)-4.0*W(I+1,J,K) &
                                -4.0*W(I-1,J,K)+W(I-2,J,K))
                    WEB(I,J,K)=CCD2*(W(I,J+2,K)-4.0*W(I,J+1,K) &
                                -4.0*W(I,J-1,K)+W(I,J-2,K))
                    WDB(I,J,K)=CCD2*(W(I,J,K+2)-4.0*W(I,J,K+1) &
                                -4.0*W(I,J,K-1)+W(I,J,K-2))
                END IF
            END IF

 166    CONTINUE
 165    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 106 K=3,KMAX+2
        DO 106 I=3,IMAX+2
        DO 106 J=3,JMAX+1
            UG(I,J,K)=0.500*(U(I+1,J,K)-U(I-1,J,K))
            UE(I,J,K)=0.500*(U(I,J+1,K)-U(I,J-1,K))
            UD(I,J,K)=0.500*(U(I,J,K+1)-U(I,J,K-1))
            UGG(I,J,K)=(U(I+1,J,K)+U(I-1,J,K))
            UEE(I,J,K)=(U(I,J+1,K)+U(I,J-1,K))
            UDD(I,J,K)=(U(I,J,K+1)+U(I,J,K-1))

            VG(I,J,K)=0.500*(V(I+1,J,K)-V(I-1,J,K))
            VE(I,J,K)=0.500*(V(I,J+1,K)-V(I,J-1,K))
            VD(I,J,K)=0.500*(V(I,J,K+1)-V(I,J,K-1))
            VGG(I,J,K)=(V(I+1,J,K)+V(I-1,J,K))
            VEE(I,J,K)=(V(I,J+1,K)+V(I,J-1,K))
            VDD(I,J,K)=(V(I,J,K+1)+V(I,J,K-1))

            WGG(I,J,K)=(W(I+1,J,K)+W(I-1,J,K))
            WEE(I,J,K)=(W(I,J+1,K)+W(I,J-1,K))
            WDD(I,J,K)=(W(I,J,K+1)+W(I,J,K-1))
            WG(I,J,K)=0.500*(W(I+1,J,K)-W(I-1,J,K))
            WE(I,J,K)=0.500*(W(I,J+1,K)-W(I,J-1,K))
            WD(I,J,K)=0.500*(W(I,J,K+1)-W(I,J,K-1))

 106       CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 178 I=3,IMAX+2
        DO 178 J=3,JMAX+1
        DO 178 K=Kpier_R,Kpier_L

            IF(pier.EQ.1.0) THEN
                IF(I.EQ.Ipier_U-1) THEN
                    U(Ipier_U,J,K)=-U(Ipier_U-1,J,K)
                    W(Ipier_U,J,K)=-W(Ipier_U-1,J,K)
                    V(Ipier_U,J,K)=-V(Ipier_U-1,J,K)

                    UG(I,J,K)=0.500*(U(I+1,J,K)-U(I-1,J,K))
                    UE(I,J,K)=0.500*(U(I,J+1,K)-U(I,J-1,K))
                    UD(I,J,K)=0.500*(U(I,J,K+1)-U(I,J,K-1))
                    UGG(I,J,K)=(U(I+1,J,K)+U(I-1,J,K))
                    UEE(I,J,K)=(U(I,J+1,K)+U(I,J-1,K))
                    UDD(I,J,K)=(U(I,J,K+1)+U(I,J,K-1))

                    VG(I,J,K)=0.500*(V(I+1,J,K)-V(I-1,J,K))
                    VE(I,J,K)=0.500*(V(I,J+1,K)-V(I,J-1,K))
                    VD(I,J,K)=0.500*(V(I,J,K+1)-V(I,J,K-1))
                    VGG(I,J,K)=(V(I+1,J,K)+V(I-1,J,K))
                    VEE(I,J,K)=(V(I,J+1,K)+V(I,J-1,K))
                    VDD(I,J,K)=(V(I,J,K+1)+V(I,J,K-1))

                    WGG(I,J,K)=(W(I+1,J,K)+W(I-1,J,K))
                    WEE(I,J,K)=(W(I,J+1,K)+W(I,J-1,K))
                    WDD(I,J,K)=(W(I,J,K+1)+W(I,J,K-1))
                    WG(I,J,K)=0.500*(W(I+1,J,K)-W(I-1,J,K))
                    WE(I,J,K)=0.500*(W(I,J+1,K)-W(I,J-1,K))
                    WD(I,J,K)=0.500*(W(I,J,K+1)-W(I,J,K-1))
                END IF
                IF(I.EQ.Ipier_D+1) THEN
                    U(Ipier_D,J,K)=-U(Ipier_D+1,J,K)
                    W(Ipier_D,J,K)=-W(Ipier_D+1,J,K)
                    V(Ipier_D,J,K)=-V(Ipier_D+1,J,K)

                    UG(I,J,K)=0.500*(U(I+1,J,K)-U(I-1,J,K))
                    UE(I,J,K)=0.500*(U(I,J+1,K)-U(I,J-1,K))
                    UD(I,J,K)=0.500*(U(I,J,K+1)-U(I,J,K-1))
                    UGG(I,J,K)=(U(I+1,J,K)+U(I-1,J,K))
                    UEE(I,J,K)=(U(I,J+1,K)+U(I,J-1,K))
                    UDD(I,J,K)=(U(I,J,K+1)+U(I,J,K-1))

                    VG(I,J,K)=0.500*(V(I+1,J,K)-V(I-1,J,K))
                    VE(I,J,K)=0.500*(V(I,J+1,K)-V(I,J-1,K))
                    VD(I,J,K)=0.500*(V(I,J,K+1)-V(I,J,K-1))
                    VGG(I,J,K)=(V(I+1,J,K)+V(I-1,J,K))
                    VEE(I,J,K)=(V(I,J+1,K)+V(I,J-1,K))
                    VDD(I,J,K)=(V(I,J,K+1)+V(I,J,K-1))

                    WGG(I,J,K)=(W(I+1,J,K)+W(I-1,J,K))
                    WEE(I,J,K)=(W(I,J+1,K)+W(I,J-1,K))
                    WDD(I,J,K)=(W(I,J,K+1)+W(I,J,K-1))
                    WG(I,J,K)=0.500*(W(I+1,J,K)-W(I-1,J,K))
                    WE(I,J,K)=0.500*(W(I,J+1,K)-W(I,J-1,K))
                    WD(I,J,K)=0.500*(W(I,J,K+1)-W(I,J,K-1))
                END IF
            END IF
 178    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 179 K=3,KMAX+2
        DO 179 J=3,JMAX+1
        DO 179 I=Ipier_U,Ipier_D

            IF(pier.EQ.1.0) THEN
                IF(K.EQ.Kpier_R-1) THEN
                    W(I,J,Kpier_R)=-W(I,J,Kpier_R-1)
                    U(I,J,Kpier_R)=-U(I,J,Kpier_R-1)
                    V(I,J,Kpier_R)=-V(I,J,Kpier_R-1)

                    UG(I,J,K)=0.500*(U(I+1,J,K)-U(I-1,J,K))
                    UE(I,J,K)=0.500*(U(I,J+1,K)-U(I,J-1,K))
                    UD(I,J,K)=0.500*(U(I,J,K+1)-U(I,J,K-1))
                    UGG(I,J,K)=(U(I+1,J,K)+U(I-1,J,K))
                    UEE(I,J,K)=(U(I,J+1,K)+U(I,J-1,K))
                    UDD(I,J,K)=(U(I,J,K+1)+U(I,J,K-1))

                    VG(I,J,K)=0.500*(V(I+1,J,K)-V(I-1,J,K))
                    VE(I,J,K)=0.500*(V(I,J+1,K)-V(I,J-1,K))
                    VD(I,J,K)=0.500*(V(I,J,K+1)-V(I,J,K-1))
                    VGG(I,J,K)=(V(I+1,J,K)+V(I-1,J,K))
                    VEE(I,J,K)=(V(I,J+1,K)+V(I,J-1,K))
                    VDD(I,J,K)=(V(I,J,K+1)+V(I,J,K-1))

                    WGG(I,J,K)=(W(I+1,J,K)+W(I-1,J,K))
                    WEE(I,J,K)=(W(I,J+1,K)+W(I,J-1,K))
                    WDD(I,J,K)=(W(I,J,K+1)+W(I,J,K-1))
                    WG(I,J,K)=0.500*(W(I+1,J,K)-W(I-1,J,K))
                    WE(I,J,K)=0.500*(W(I,J+1,K)-W(I,J-1,K))
                    WD(I,J,K)=0.500*(W(I,J,K+1)-W(I,J,K-1))
                END IF
                IF(K.EQ.Kpier_L+1) THEN
                    W(I,J,Kpier_L)=-W(I,J,Kpier_L+1)
                    U(I,J,Kpier_L)=-U(I,J,Kpier_L+1)
                    V(I,J,Kpier_L)=-V(I,J,Kpier_L+1)

                    UG(I,J,K)=0.500*(U(I+1,J,K)-U(I-1,J,K))
                    UE(I,J,K)=0.500*(U(I,J+1,K)-U(I,J-1,K))
                    UD(I,J,K)=0.500*(U(I,J,K+1)-U(I,J,K-1))
                    UGG(I,J,K)=(U(I+1,J,K)+U(I-1,J,K))
                    UEE(I,J,K)=(U(I,J+1,K)+U(I,J-1,K))
                    UDD(I,J,K)=(U(I,J,K+1)+U(I,J,K-1))

                    VG(I,J,K)=0.500*(V(I+1,J,K)-V(I-1,J,K))
                    VE(I,J,K)=0.500*(V(I,J+1,K)-V(I,J-1,K))
                    VD(I,J,K)=0.500*(V(I,J,K+1)-V(I,J,K-1))
                    VGG(I,J,K)=(V(I+1,J,K)+V(I-1,J,K))
                    VEE(I,J,K)=(V(I,J+1,K)+V(I,J-1,K))
                    VDD(I,J,K)=(V(I,J,K+1)+V(I,J,K-1))

                    WGG(I,J,K)=(W(I+1,J,K)+W(I-1,J,K))
                    WEE(I,J,K)=(W(I,J+1,K)+W(I,J-1,K))
                    WDD(I,J,K)=(W(I,J,K+1)+W(I,J,K-1))
                    WG(I,J,K)=0.500*(W(I+1,J,K)-W(I-1,J,K))
                    WE(I,J,K)=0.500*(W(I,J+1,K)-W(I,J-1,K))
                    WD(I,J,K)=0.500*(W(I,J,K+1)-W(I,J,K-1))
                END IF
            END IF

 179    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!C     -------   KANSEIKOU NO KEISAN ------------------

!$OMP PARALLEL
!$OMP DO
           DO 112 K=3,KMAX+2
           DO 112 I=3,IMAX+2
           DO 112 J=3,JMAX+1

        A0(I,J,K)=-(E11(I,J,K)*UGA(I,J,K)+F11(I,J,K)*UGB(I,J,K) &
                   +E12(I,J,K)*UEA(I,J,K)+F12(I,J,K)*UEB(I,J,K) &
                   +E13(I,J,K)*UDA(I,J,K)+F13(I,J,K)*UDB(I,J,K))*DT
        B0(I,J,K)=-(E11(I,J,K)*VGA(I,J,K)+F11(I,J,K)*VGB(I,J,K) &
                   +E12(I,J,K)*VEA(I,J,K)+F12(I,J,K)*VEB(I,J,K) &
                   +E13(I,J,K)*VDA(I,J,K)+F13(I,J,K)*VDB(I,J,K))*DT
        C0(I,J,K)=-(E11(I,J,K)*WGA(I,J,K)+F11(I,J,K)*WGB(I,J,K) &
                   +E12(I,J,K)*WEA(I,J,K)+F12(I,J,K)*WEB(I,J,K) &
                   +E13(I,J,K)*WDA(I,J,K)+F13(I,J,K)*WDB(I,J,K))*DT

 112    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!C-----------U,V,W no keisuu ----------------------------

!$OMP PARALLEL
!$OMP DO
        DO 108 K=3,KMAX+2
        DO 108 I=3,IMAX+2
        DO 108 J=3,JMAX+1

            DD1(I,J,K)=DT*(6.0*CCD2*(F11(I,J,K)+F12(I,J,K)+F13(I,J,K)) &
                           +2.*(MYT(I,J,K))*(GX(I)**2+EY(J)**2+DZ(K)**2))
 108    CONTINUE

!$OMP END DO
!$OMP END PARALLEL

!C-----------NENSEI NO KEISAN --------------

        DO 140 K=3,KMAX+2
        DO 140 I=3,IMAX+2
        DO 140 J=3,JMAX+1

            AN(I,J,K)=(GX(I)**2*UGG(I,J,K)+EY(J)**2*UEE(I,J,K) &
                      +DZ(K)**2*UDD(I,J,K) &
                     +GXX(I)*UG(I,J,K)+EYY(J)*UE(I,J,K)+DZZ(K)*UD(I,J,K)) &
                     *MYT(I,J,K)*DT
            BN(I,J,K)=(GX(I)**2*VGG(I,J,K)+EY(J)**2*VEE(I,J,K) &
                      +DZ(K)**2*VDD(I,J,K) &
                     +GXX(I)*VG(I,J,K)+EYY(J)*VE(I,J,K)+DZZ(K)*VD(I,J,K)) &
                     *MYT(I,J,K)*DT
            CN(I,J,K)=(GX(I)**2*WGG(I,J,K)+EY(J)**2*WEE(I,J,K) &
                      +DZ(K)**2*WDD(I,J,K) &
                     +GXX(I)*WG(I,J,K)+EYY(J)*WE(I,J,K)+DZZ(K)*WD(I,J,K)) &
                     *MYT(I,J,K)*DT

!C----------- Interaction between two phases ---------------------

            CC1(I,J,K)=C(I,J,K)/(1.0-C(I,J,K))*(FDX(I,J,K)+FLX0(I,J,K)+FMX(I,J,K)*0.0)*DT
            CC2(I,J,K)=C(I,J,K)/(1.0-C(I,J,K))*(FDY(I,J,K)+FLY0(I,J,K))*DT
            CC3(I,J,K)=C(I,J,K)/(1.0-C(I,J,K))*(FDZ(I,J,K)+FLZ0(I,J,K)+FMZ(I,J,K)*0.0)*DT
!C********** Pressure Gradient (Non-Approxamation Pressure) **********C

            AP(I,J,K)=(P(I+1,J,K)-P(I-1,J,K))*0.50*GX(I)/ROUA(I,J,K)*DT*0.0
            BP(I,J,K)=(P(I,J+1,K)-P(I,J-1,K))*0.50*EY(J)/ROUA(I,J,K)*DT*0.0
            CP(I,J,K)=(P(I,J,K+1)-P(I,J,K-1))*0.50*DZ(K)/ROUA(I,J,K)*DT*0.0

!C********** Surface Gradient **********C

            DETADX(I,K)=GX(I)*(ETA(I+1,K)-ETA(I-1,K))*0.50*GG*DT
            DETADZ(I,K)=DZ(K)*(ETA(I,K+1)-ETA(I,K-1))*0.50*GG*DT

!C------------  Turbulence Correlation Term -----------

            AE(I,J,K)=DT*(2.0*UG(I,J,K)*MYTG(I,J,K)*GX(I)**2 &
                         -2.0/3.0*HKG(I,J,K)*GX(I) &
                        +(UE(I,J,K)*EY(J)+VG(I,J,K)*GX(I))*MYTE(I,J,K)*EY(J) &
                        +(UD(I,J,K)*DZ(K)+WG(I,J,K)*GX(I))*MYTD(I,J,K)*DZ(K))
            BE(I,J,K)=DT*(2.0*VE(I,J,K)*MYTE(I,J,K)*EY(J)**2 &
                         -2.0/3.0*HKE(I,J,K)*EY(J) &
                        +(UE(I,J,K)*EY(J)+VG(I,J,K)*GX(I))*MYTG(I,J,K)*GX(I) &
                        +(WE(I,J,K)*EY(J)+VD(I,J,K)*DZ(K))*MYTD(I,J,K)*DZ(K))
            CCE(I,J,K)=DT*(2.0*WD(I,J,K)*MYTD(I,J,K)*DZ(K)**2 &
                         -2.0/3.0*HKD(I,J,K)*DZ(K) &
                        +(UD(I,J,K)*DZ(K)+WG(I,J,K)*GX(I))*MYTG(I,J,K)*GX(I)  &
                        +(VD(I,J,K)*DZ(K)+WE(I,J,K)*EY(J))*MYTE(I,J,K)*EY(J))

 140    CONTINUE

!C     -------------RESIDURE ------------------------------

        DO 150 K=3,KMAX+2
        DO 150 I=3,IMAX+2
        DO 150 J=3,JMAX+1

            SU(I,J,K)=(U1(I,J,K)-AP(I,J,K)+CC1(I,J,K)+AN(I,J,K)+A0(I,J,K) &
                     +AE(I,J,K)-DETADX(I,K))/(1.0+DD1(I,J,K))-U(I,J,K)

            SV(I,J,K)=(V1(I,J,K)-BP(I,J,K)+CC2(I,J,K)+BN(I,J,K)+B0(I,J,K) &
                     +BE(I,J,K)+BOU(I,J,K))/(1+DD1(I,J,K))-V(I,J,K)

            SW(I,J,K)=(W1(I,J,K)-CP(I,J,K)+CC3(I,J,K)+CN(I,J,K)+C0(I,J,K) &
                     +CCE(I,J,K)-DETADZ(I,K))/(1+DD1(I,J,K))-W(I,J,K)

 150    CONTINUE

!CCCCCC////// Pier Boundary Condition ////////CCCCCC

        IF(pier.EQ.1.0) THEN
!$OMP PARALLEL
!$OMP DO
            DO 7023 J=1,JMAX+3
            DO 7023 K=Kpier_R, Kpier_L
            DO 7023 I=Ipier_U, Ipier_D

                SU(I,J,K)=0.0
                SV(I,J,K)=0.0
                SW(I,J,K)=0.0

 7023   CONTINUE     
!$OMP END DO
!$OMP END PARALLEL

        !END IF
        !IF(pier.EQ.1.0) THEN
!!$OMP PARALLEL
!!$OMP DO
            !DO 7033 J=1,JMAX+3
            !DO 7033 K=Kpier_R1,Kpier_L1
            !DO 7033 I=Ipier_U1,Ipier_D1

                !SU(I,J,K)=0.0
                !SV(I,J,K)=0.0
                !SW(I,J,K)=0.0

 7033   CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL
        !END IF
        !IF(pier.EQ.1.0) THEN
!!$OMP PARALLEL
!!$OMP DO
            !DO 7034 J=1,JMAX+3
            !DO 7034 K=Kpier_R2,Kpier_L2
            !DO 7034 I=Ipier_U2,Ipier_D2

                !SU(I,J,K)=0.0
                !SV(I,J,K)=0.0
                !SW(I,J,K)=0.0

 7034   CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL
        !END IF
        !IF(pier.EQ.1.0) THEN
!!$OMP PARALLEL
!!$OMP DO
            !DO 7035 J=1,JMAX+3
            !DO 7035 K=Kpier_R3,Kpier_L3
            !DO 7035 I=Ipier_U3,Ipier_D3

                !SU(I,J,K)=0.0
                !SV(I,J,K)=0.0
                !SW(I,J,K)=0.0

 7035   CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL
        !END IF
        !IF(pier.EQ.1.0) THEN
!!$OMP PARALLEL
!!$OMP DO
            !DO 7036 J=1,JMAX+3
            !DO 7036 K=Kpier_R4,Kpier_L4
            !DO 7036 I=Ipier_U4,Ipier_D4

                !SU(I,J,K)=0.0
                !SV(I,J,K)=0.0
                !SW(I,J,K)=0.0

 7036  CONTINUE     
!!$OMP END DO
!!$OMP END PARALLEL
        END IF

!C/////////////// Surface Boundary Condition ///////////////C

!$OMP PARALLEL
!$OMP DO
        DO 590 K=3,KMAX+2
        DO 590 I=3,IMAX+2
        DO 590 J=IETA(I,K),JMAX+1

            SU(I,J,K)=0.0
            SW(I,J,K)=0.0
            SV(I,J,K)=0.0

 590    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!!$OMP PARALLEL
!!$OMP DO
        !DO 592 K=3,KMAX+2
        !DO 592 I=3,IMAX+2
        !DO 592 J=3,IGL(I,K)

            !SU(I,J,K)=0.0
            !SW(I,J,K)=0.0
            !SV(I,J,K)=0.0

 592    CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL

        DO 151 K=3,KMAX+2
        DO 151 I=3,IMAX+2
        DO 151 J=3,JMAX+1

            U(I,J,K)=U(I,J,K)+CONST2*SU(I,J,K)
            V(I,J,K)=V(I,J,K)+CONST2*SV(I,J,K)
            W(I,J,K)=W(I,J,K)+CONST2*SW(I,J,K)

 151    CONTINUE

        DO 160 K=3,KMAX+2
        DO 160 I=3,IMAX+2
        DO 160 J=3,JMAX+1

            GOSAQ=GOSAQ+ABS(SU(I,J,K))+ABS(SV(I,J,K))+ABS(SW(I,J,K))

 160    CONTINUE

        IF(GOSAQ.LT.EPS2) GO TO 5000

 1000   CONTINUE
 5000   CONTINUE

        !IF(MOD(N,100).EQ.0)THEN
        !WRITE(num,'(I6.6)')N
        !OPEN(88,file='check/GOSAQ'//num//'.txt')
        !    DO K=Kpier_R,Kpier_L+10
        !    DO I=Ipier_U-10,Ipier_D+10
        !    !DO J=3,JMAX+1
        !        WRITE(88,'(2i5,9f15.5)') I,K,SU(I,IETA(I,K)-1,K),SV(I,IETA(I,K)-1,K),SW(I,IETA(I,K)-1,K)
        !    !END DO
        !    END DO
        !    END DO
        !CLOSE(88)
        !END IF

        !DO 5228 K=3,KMAX+2
        !DO 5228 I=3,IMAX+2

            !DUUDX(I,K)=U(I,IETA(I,K),K)*(ELES(I+1,K)-ELES(I-1,K))*0.50*GX(I) &
            !        -ABS(U(I,IETA(I,K),K))*(ELES(I+1,K)-2.0*ELES(I,K)+ELES(I-1,K))*0.50*GX(I)
            !DWWDZ(I,K)=W(I,IETA(I,K),K)*(ELES(I,K+1)-ELES(I,K-1))*0.50*DZ(K) &
            !        -ABS(W(I,IETA(I,K),K))*(ELES(I,K+1)-2.0*ELES(I,K)+ELES(I,K-1))*0.50*DZ(K)

            !V(I,IETA(I,K),K)=(ELES(I,K)-ELES1(I,K))/DT+DUUDX(I,K)+DWWDZ(I,K)

 5228   CONTINUE

!$OMP PARALLEL
!$OMP DO
            DO 1512 K=2,KMAX+3  !sumo241204
            DO 1512 I=2,IMAX+3
            DO 1512 J=2,JMAX+2

                !pattern1
                UB(I,J,K)=(U(I,J,K)+U(I+1,J,K))*0.500
                VB(I,J,K)=(V(I,J,K)+V(I,J+1,K))*0.500
                WB(I,J,K)=(W(I,J,K)+W(I,J,K+1))*0.500

 1512   CONTINUE
!$OMP END DO
!$OMP END PARALLEL

        IF(pier.EQ.1.0) THEN
!$OMP PARALLEL
!$OMP DO
        DO 1515 J=1,JMAX+3
            DO 1516 K=Kpier_R,Kpier_L  !sumo241204
            DO 1516 I=Ipier_U-1,Ipier_D

                UB(I,J,K)=0.0
                !VB(I,J,K)=0.0
                !WB(I,J,K)=0.0

 1516   CONTINUE

            DO 1517 I=Ipier_U,Ipier_D  !sumo241204
            DO 1517 K=Kpier_R-1,Kpier_L

                !UB(I,J,K)=0.0
                !VB(I,J,K)=0.0
                WB(I,J,K)=0.0

 1517   CONTINUE

 1515   CONTINUE
!$OMP END DO
!$OMP END PARALLEL
        END IF

        ZANSU=0.0
!SMP$ DOSERIAL
        DO 180 K=3,KMAX+2
        DO 180 I=3,IMAX+2
        DO 180 J=3,JMAX+1

            ZANSU=ABS(U(I,J,K)-U1(I,J,K))+ABS(V(I,J,K)-V1(I,J,K)) &
                 +ABS(W(I,J,K)-W1(I,J,K))+ZANSU

 180    CONTINUE

        IF(naka.EQ.0.0) THEN
        WRITE (0,90) (LOOP)+ITC1*40,GOSAQ
 90     FORMAT (25X,'LOOP(UVW)=',I3,2X,'GOSAQ=',F20.10)
        WRITE (0,92) ZANSU
 92     FORMAT (27X,'ZANSU=',F13.10)
        END IF

    deallocate (E11,F11,E12,F12,E13,F13,DD1 &
     ,UGA,UGB,UEA,UEB,UDA,UDB,VGA,VGB,VEA,VEB &
     ,VDA,VDB,WGA,WGB,WEA,WEB,WDA,WDB &
     ,A0,AP,B0,BP,C0,CP,AN,BN,CN &
     ,SU,CC1,SV,CC2,SW,CC3 &
     ,UG,VG,UE,VE,UD,VD,UGG,VGG,UEE,VEE &
     ,UDD,VDD,WG,WGG,WE,WEE,WD,WDD &
     ,MYTG,HKG,MYTE,HKE,MYTD,HKD &
     ,BOU,OMEX,OMEY,OMEZ &
     ,FDX,FDY,FLX0,FLY0,FMX,FMY &
     ,DUDT,DVDT,DUSDT,DVSDT &
     ,DWDT,DWSDT,FMZ,FLZ0 &
     ,FDZ,FLUC_RATE,UF,VF,WF,CCC)

    RETURN
    END

!C**************************************************************
    SUBROUTINE INIT
!C**************************************************************
    USE MF_SF
    USE OMP_LIB
    IMPLICIT NONE

!C=================
        IMAX=50.0   !ITO CHANGE
        JMAX=44.0   !ITO CHANGE
        KMAX=30.0   !ITO CHANGE
!C=================

        IF(DEPMODE.EQ.1.0) SLOPE=0.000872211d0   !ITO CHANGE   !SUMO23/03/23
        !SLOPE=0.0

!C********** MESH SETTING **********C

        DO 100 I=1,IMAX+4
        DO 100 J=1,JMAX+3
        DO 100 K=1,KMAX+4

            X(I,J,K)=FLOAT(I)*(1.0/(FLOAT(IMAX)))*22.0   !ITO CHANGE
            Y(I,J,K)=FLOAT(J)*(1.0/(FLOAT(JMAX)))*4.40   !ITO CHANGE
            Z(I,J,K)=FLOAT(K)*(1.0/(FLOAT(KMAX)))*25.0   !ITO CHANGE

!C*************** FOR Variable Slope Channel ***************C

            IF(DEPMODE.EQ.1.0) THEN
                YY(I,J,K)=Y(I,J,K)-(X(I,J,K)-X(1,J,K))*SLOPE   !SUMO23/03/23
                Y(I,J,K)=YY(I,J,K)   !SUMO23/03/23
            END IF

 100    CONTINUE

        DO 101 I=2,IMAX+3

            XG(I)=(X(I+1,1,1)-X(I-1,1,1))*0.5
            GX(I)=1/XG(I)

 101    CONTINUE 

            XG(1)=XG(IMAX+1)
            XG(IMAX+4)=XG(4)
            GX(1)=1/XG(1)
            GX(IMAX+4)=1/XG(IMAX+4)

            XHG(1)=(X(2,1,1)-X(1,1,1))
            GHX(1)=1/XHG(1)

        DO 117 I=2,IMAX+4

            XHG(I)=2.0*(X(I,1,1)-X(I-1,1,1))-XHG(I-1)

 117    CONTINUE 

        DO 118 I=1,IMAX+4

            GHX(I)=0.50*(XHG(I)+XHG(IMAX+5-I))
            !WRITE(*,*) I,GHX(I)

 118    CONTINUE 

        DO 105 I=2,IMAX+3

            GXX(I)=-(XG(I+1)-XG(I-1))*0.5*GX(I)**3

 105    CONTINUE

            GXX(1)=GXX(2)
            GXX(IMAX+4)=GXX(IMAX+3)

        DO 102 J=2,JMAX+2

            YE(J)=(Y(1,J+1,1)-Y(1,J-1,1))*0.5
            EY(J)=1/YE(J)

 102    CONTINUE

            YE(1)=YE(2)
            YE(JMAX+3)=YE(JMAX+2)
            EY(1)=1/YE(1)
            EY(JMAX+3)=1/YE(JMAX+2)

        DO 111 K=2,KMAX+3

            ZD(K)=(Z(1,1,K+1)-Z(1,1,K-1))*0.5
            DZ(K)=1.0/ZD(K)

 111    CONTINUE

            ZD(1)=ZD(2)
            ZD(KMAX+4)=ZD(KMAX+2)
            DZ(1)=1.0/ZD(1)
            DZ(KMAX+4)=1.0/ZD(KMAX+3)

        DO 113 J=2,JMAX+2

            EYY(J)=-(YE(J+1)-YE(J-1))*0.50*EY(J)**3

 113    CONTINUE

            EYY(1)=EYY(2)
            EYY(JMAX+3)=EYY(JMAX+2)

        DO 114 K=2,KMAX+3

            DZZ(K)=-(ZD(K+1)-ZD(K-1))*0.50*DZ(K)**3

 114    CONTINUE

            DZZ(1)=DZZ(2)
            DZZ(KMAX+4)=DZZ(KMAX+3)

        DO 103 I=1,IMAX+4

            IF(ABS(GX(I)).LT.1.0D-16) GX(I)=0.0D0

 103    CONTINUE

        DO 104 J=1,JMAX+3

            IF(ABS(EY(J)).LT.1.0D-16) EY(J)=0.0D0

 104    CONTINUE

        DO 112 K=1,KMAX+4

            IF(ABS(DZ(K)).LT.1.0D-16) DZ(K)=0.0D0

 112    CONTINUE

!C********** COORDINATE CHECK **********C

        !OPEN(88,file='00_INI/COORDINATE/COORDINATE.txt')
        !DO I=1,IMAX+4
        !DO K=1,KMAX+4
        !DO J=1,JMAX+3
        !    WRITE(88,'(3i5,9f15.5)') I,J,K,X(I,J,K) &
        !                            ,Y(I,J,K),Z(I,J,K) &
        !                            ,XG(I),YE(J),ZD(K)
        !END DO
        !END DO
        !END DO
        !CLOSE(88)

        !OPEN(88,file='00_INI/CENTER/COORDINATE.txt')
        !K=12
        !DO I=1,IMAX+4
        !DO J=1,JMAX+3
        !    WRITE(88,'(3i5,9f15.5)') I,J,K,X(I,J,K) &
        !                            ,Y(I,J,K),Z(I,J,K) &
        !                            ,XG(I),YE(J),ZD(K)
        !END DO
        !END DO
        !CLOSE(88)

!C********** PARAMETER **********C

        NMAX=300000   !ITO CHANGE
        DT=0.0010   !ITO CHANGE
        RED=0.0100

!C********** Pressure Gradient **********C

        !T0=3.60
        !AA=20.0*2.0*PHI/T0
        !AA=70.0*2.0*PHI/T0
        !AA=2.70

!C**********  LES teisuu **********C

        arfa=0.10
        !arfa=1.0
        !CS=0.10
        CS=0.050
        !CS=0.0
        CE=0.50

!C**********  Boundary Layer teisuu **********C

        !Z0=0.01
        !CAR=0.40
        !AS=5.50
        !AS=8.50

!C**********  Averaged Velosity(Umm) **********C

        !Umm=150.0
        !Umm=200.0
        !RE=Umm*DMAX/RED

!C**********  MITUDO & other cofe **********C

        N1=1
        CL0=0.0
        CM0=0.0

        RC=1.00
        RS=2.65
        GG=980.00
        DR=0.13350
        Qin=0.0025D0   !ITO CHANGE  !m^3/s
        CSmax=0.0
        !CSmax=1.00  !0.600

        WRITE(0,10) RS,RC,DR
 10     FORMAT('Rs=',F6.3,'  Rc=',F6.3,'  DR=',F6.3)

!C**********  RANSU prameter **********C

        !aaa=5.0
        !bbb=5.0

!C**********  GOSA prameter **********C

        EPS1=8.0E-6
        EPS2=4.0E-2
        EPS3=4.0E-4
        EPS4=4.0E-2

        CONST1=1.0E0
        !CONST2=1.0E0
        CONST2=0.80E0
        CONST3=1.0E0

!C**********  GHOST LOOP ROUA to SAL **********C

        DO 640 I=1,IMAX+4
        DO 640 K=1,KMAX+3
        DO 640 J=1,JMAX+3

            !ROUA(I,J,K)=RC+SALA(I,J,K)*htp
            ROUA(I,J,K)=RC
            !ROUA(I,J,K)=0.0
            ROU(I,J,K)=0.0

 640    CONTINUE

!********** CONCENTRATION AND GL SETTING **********!

        DO 911 I=1,IMAX+4
        DO 911 K=1,KMAX+4

            IGL(I,K)=2

 911    CONTINUE

!********** EXP DATA INPUT 23/05/28SUMO **********!

        !OPEN(16,FILE='INPUT/GL_position-notosui.dat')
        !DO III=1,25000
        !    READ(16,*) I,K,IGL(I,K)
        !    WRITE(*,*) I,K,IGL(I,K)
        !END DO
        !CLOSE(16)

        DO 9011 I=1,IMAX+4
        DO 9011 K=1,KMAX+4
        DO 9011 J=1,JMAX+3

            C(I,J,K)=CSMAX
            IF(J.GE.IGL(I,K)+1) C(I,J,K)=0.0

 9011   CONTINUE

!********** DEPTH AND ETA SETTING **********!

        !UT00=20.0

        DO 917 I=1,IMAX+4
        DO 917 K=1,KMAX+4
        DO 917 J=1,JMAX+3

            GL(I,K)=Y(I,IGL(I,K),K)   !SUMO23/03/17
            SODO(I,K)=0.01110 !ITO CHANGE   !  !Unit:s*m^(-1/3)
            RNM(I,K)=SODO(I,K)*0.21544  !Unit:cm^(-1/3)/s ; 100**(-1/3)=0.21544
            QQQ(I)=Qin*100.0**3 !unit:cm^3/s
            DEP(I,K)=(((QQQ(I)/(FLOAT(KMAX)/DZ(3)))**2)*(RNM(I,K)**2)/SLOPE)**(3.0/10.0)  !SUMO241113
            !IF(I.LE.3) DEP(I,K)=4.2200 !sumo250829
            !IF(I.GE.IMAX+2) DEP(I,K)=3.4400
            !IF(IGL(I,K).NE.42) THEN
            !    DEP(I,K)=DEP(I,K)+((42-IGL(I,K))/EY(J))
            !END IF
            HH(I,K)=DEP(I,K)
            ELES(I,K)=0.0
            ETA(I,K)=DEP(I,K)+GL(I,K)    !SUMO23/03/27

 917    CONTINUE

        UT00=QQQ(3)/(FLOAT(KMAX)/DZ(3))/DEP(3,3)
        DEPC=(((QQQ(3)/(FLOAT(KMAX)/DZ(3)))**2)/GG)**(1.0/3.0)
        WRITE(*,*) 'UT0=', UT00,'DEPC=', DEPC,DEP(3,3)

!*************** seishe ***************!

        !DO 2011 K=3,KMAX+2

            !DEP(3,K)=130.0
            !DEP(IMAX+2,K)=135.0

            !DO 2011 I=4,IMAX+1
            
                !DEP(I,K)=DEP(3,K)+REAL(I-3)*(DEP(IMAX+2,K)-DEP(3,K))/REAL(IMAX+2-3+1)

 2011   CONTINUE

        !DO 2012 I=3,IMAX+2
        !DO 2012 K=3,KMAX+2

            !ETA(I,K)=DEP(I,K)+GL(I,K)
            !HH(I,K)=DEP(I,K)

 2012   CONTINUE

        DO 997 I=1,2
        DO 997 K=3,KMAX+2

            GL(I,K)=GL(3,K)
            DEP(I,K)=DEP(3,K)
            ETA(I,K)=ETA(3,K)
            HH(I,K)=HH(3,K)
            GL(IMAX+2+I,K)=GL(IMAX+2,K)
            DEP(IMAX+2+I,K)=DEP(IMAX+2,K)
            ETA(IMAX+2+I,K)=ETA(IMAX+2,K)
            HH(IMAX+2+I,K)=HH(IMAX+2,K)

 997    CONTINUE

        DO 916 K=1,2
        DO 916 I=1,IMAX+4

            GL(I,K)=GL(I,3)
            GL(I,KMAX+2+K)=GL(I,KMAX+2)
            DEP(I,K)=DEP(I,3)
            DEP(I,KMAX+2+K)=DEP(I,KMAX+2)
            ETA(I,K)=ETA(I,3)
            ETA(I,KMAX+2+K)=ETA(I,KMAX+2)
            HH(I,K)=HH(I,3)
            HH(I,KMAX+2+K)=HH(I,KMAX+2)

 916    CONTINUE

        DO 2005 K=1,KMAX+4
        DO 2005 I=1,IMAX+4
        DO 2005 J=3,JMAX+1

            IF(ETA(I,K).GT.Y(I,J-1,K)) THEN
                IF(ETA(I,K).LE.Y(I,J,K)) THEN
                    IF(ETA(I,K).GT.(Y(I,J,K)-0.50/EY(J))) THEN
                        IETA(I,K)=J
                    ELSE
                        IETA(I,K)=J-1
                    END IF
                END IF
            END IF

 2005   CONTINUE

!********** VELOCITY SETTING **********!

        DO 912 I=1,IMAX+4
        DO 912 K=1,KMAX+4

            UT(I,K)=QQQ(I)/DEP(I,K)/(FLOAT(KMAX)/DZ(3))
            CFF(I,K)=GG*RNM(I,K)**2/(DEP(I,K)**(1.0/3.0))
            !USTAB(I,K)=CFF(I,K)**0.500*UT(I,K)
            USTAB(I,K)=1.5290D0 !ITO CHANGE

            DO 9119 J=1,IETA(I,K)-1

                IF(J.LE.IGL(I,K)) THEN
                    U(I,J,K)=0.0
                ELSE
                    !U(I,J,K)=USTAB(I,K)*(1.0/0.40* &
                    !         LOG((J-(FLOAT(IGL(I,K))+0.500))/EY(3)/ &
                    !             ((RNM(I,K)*(GG**0.5)/0.13055)**6))+8.50)
                    !!U(I,IGL(I,K)+1,K)=USTAB(I,K)*(1.0/0.40* &
                    !!         LOG(1.0/EY(3)/ &
                    !!            ((RNM(I,K)*(GG**0.5)/0.13055)**6))+8.50)
                    U(I,J,K)=USTAB(I,K)*(1.0/0.40* &
                             LOG(USTAB(I,K)*(J-(FLOAT(IGL(I,K))+0.500))/EY(3)/RED)+5.50)
                    U(I,IGL(I,K)+1,K)=USTAB(I,K)*(1.0/0.40*LOG(USTAB(I,K)*0.50/EY(3)/RED)+5.50)
                    !U(I,IGL(I,K)+1,K)=2.00*U(I,IGL(I,K)+2,K)-U(I,IGL(I,K)+3,K)
                    V(I,J,K)=0.0
                    W(I,J,K)=0.0
                    HK(I,J,K)=1.0E-5
                    !WRITE(*,*) U(I,J,K), 'USA'
                END IF

                !U(I,J,K)=UT(I,K)
                !IF(J.LE.IGL(I,K)) U(I,J,K)=0.0  !sumo250826
                V(I,J,K)=0.0
                W(I,J,K)=0.0
                HK(I,J,K)=1.0E-5

 9119   CONTINUE

            U(I,IETA(I,K),K)=U(I,IETA(I,K)-1,K)
            V(I,IETA(I,K),K)=0.0
            W(I,IETA(I,K),K)=0.0
            HK(I,J,K)=1.0E-5

            DO 9120 J=IETA(I,K)+1,JMAX+3

                U(I,J,K)=0.0
                V(I,J,K)=0.0
                W(I,J,K)=0.0
                HK(I,J,K)=1.0E-5

 9120   CONTINUE
 912    CONTINUE

        !WRITE(*,*) U(3,3,3), 'USA'
        !STOP

        DO 915 I=1,IMAX+4                
        DO 915 J=1,JMAX+3
        DO 915 K=1,KMAX+4

            P(I,J,K)=0.0
            !U(I,J,K)=0.0
            !V(I,J,K)=0.0
            !W(I,J,K)=0.0
            !UT(I,K)=0.0
            !WT(I,K)=0.0
            MYT(I,J,K)=0.010
            CM(I,J,K)=C(I,J,K)
            US(I,J,K)=0.0
            VS(I,J,K)=0.0
            WS(I,J,K)=0.0
            USR(I,J,K)=0.0
            VSR(I,J,K)=0.0
            WSR(I,J,K)=0.0
            TEMP(I,J,K)=0.010
            UUSR(I,J,K)=USR(I,J,K)
            VVSR(I,J,K)=VSR(I,J,K)
            WWSR(I,J,K)=WSR(I,J,K)
            XG1(I,J,K)=X(I,J,K)
            YG(I,J,K)=Y(I,J,K)
            ZG(I,J,K)=Z(I,J,K)

            RE1(I,J,K)=935.0
            APP(I)=0.0
            USTA(I)=0.10
            VSTA(I)=0.10
            USTAu(I)=0.10
            VSTAu(I)=0.10

            USTABRW(I,J)=0.010
            USTABLW(I,J)=0.010
            USTABRW_P(I,J)=0.010
            USTABLW_P(I,J)=0.010
            WSTABFW_P(J,K)=0.010
            WSTABBW_P(J,K)=0.010

 915    CONTINUE  

!CCCCCCCCCC/////////PIER location//////////CCCCCCCCCC

        pier=0.0    !0.0:withoutpier, 1.0:withpier
        IF(pier.EQ.1.0) THEN
            Ipier_U=53!((IMAX+2)/2+1)-1    !Idirection pier_Upstream location
            Ipier_D=62!((IMAX+2)/2+11)+1    !Idirection pier_Downstream location
            Kpier_R=3!11!((KMAX+2)/2+1)-1    !Kdirection pier_Rightside location
            Kpier_L=34!42!((KMAX+2)/2+1)+1    !Kdirection pier_Leftside location

            WRITE(*,*) Ipier_U, Ipier_D, Kpier_R, Kpier_L
            WRITE(*,*) 'PierSizeCheck'
            WRITE(*,*) 'PierFlowDirectionSize', (Ipier_D-Ipier_U+1)/GX(3)
            WRITE(*,*) 'PierWidth', (Kpier_L-Kpier_R+1)/DZ(3)

            !Ipier_U1=3!((IMAX+2)/2+1)-1    !Idirection pier_Upstream location
            !Ipier_D1=18!((IMAX+2)/2+11)+1    !Idirection pier_Downstream location
            !Kpier_R1=3!((KMAX+2)/2+1)-1    !Kdirection pier_Rightside location
            !Kpier_L1=22!((KMAX+2)/2+1)+1    !Kdirection pier_Leftside location

            !WRITE(*,*) Ipier_U1, Ipier_D1, Kpier_R1, Kpier_L1
            !WRITE(*,*) 'PierSizeCheck'
            !WRITE(*,*) 'PierFlowDirectionSize', (Ipier_D1-Ipier_U1+1)/GX(3)
            !WRITE(*,*) 'PierWidth', (Kpier_L1-Kpier_R1+1)/DZ(3)

            !Ipier_U2=3!((IMAX+2)/2+1)-1    !Idirection pier_Upstream location
            !Ipier_D2=18!((IMAX+2)/2+11)+1    !Idirection pier_Downstream location
            !Kpier_R2=83!((KMAX+2)/2+1)-1    !Kdirection pier_Rightside location
            !Kpier_L2=102!((KMAX+2)/2+1)+1    !Kdirection pier_Leftside location

            !WRITE(*,*) Ipier_U2, Ipier_D2, Kpier_R2, Kpier_L2
            !WRITE(*,*) 'PierSizeCheck'
            !WRITE(*,*) 'PierFlowDirectionSize', (Ipier_D2-Ipier_U2+1)/GX(3)
            !WRITE(*,*) 'PierWidth', (Kpier_L2-Kpier_R2+1)/DZ(3)

            !Ipier_U3=83!((IMAX+2)/2+1)-1    !Idirection pier_Upstream location
            !Ipier_D3=98!((IMAX+2)/2+11)+1    !Idirection pier_Downstream location
            !Kpier_R3=3!((KMAX+2)/2+1)-1    !Kdirection pier_Rightside location
            !Kpier_L3=22!((KMAX+2)/2+1)+1    !Kdirection pier_Leftside location

            !WRITE(*,*) Ipier_U3, Ipier_D3, Kpier_R3, Kpier_L3
            !WRITE(*,*) 'PierSizeCheck'
            !WRITE(*,*) 'PierFlowDirectionSize', (Ipier_D3-Ipier_U3+1)/GX(3)
            !WRITE(*,*) 'PierWidth', (Kpier_L3-Kpier_R3+1)/DZ(3)

            !Ipier_U4=83!((IMAX+2)/2+1)-1    !Idirection pier_Upstream location
            !Ipier_D4=98!((IMAX+2)/2+11)+1    !Idirection pier_Downstream location
            !Kpier_R4=83!((KMAX+2)/2+1)-1    !Kdirection pier_Rightside location
            !Kpier_L4=102!((KMAX+2)/2+1)+1    !Kdirection pier_Leftside location

            !WRITE(*,*) Ipier_U4, Ipier_D4, Kpier_R4, Kpier_L4
            !WRITE(*,*) 'PierSizeCheck'
            !WRITE(*,*) 'PierFlowDirectionSize', (Ipier_D4-Ipier_U4+1)/GX(3)
            !WRITE(*,*) 'PierWidth', (Kpier_L4-Kpier_R4+1)/DZ(3)
        END IF

!CCCCCC////// Pier Boundary Condition ////////CCCCCC

        IF(pier.EQ.1.0) THEN

            DO 701 J=1,JMAX+3
            DO 701 K=Kpier_R,Kpier_L
            DO 701 I=Ipier_U,Ipier_D

                C(I,J,K)=CSMAX
                IF(J.GE.IGL(I,K)) C(I,J,K)=0.0
                CM(I,J,K)=C(I,J,K)
                HK(I,J,K)=0.0
                U(I,J,K)=0.0
                V(I,J,K)=0.0
                W(I,J,K)=0.0
                US(I,J,K)=0.0
                VS(I,J,K)=0.0
                WS(I,J,K)=0.0
                UT(I,K)=0.0
                WT(I,K)=0.0
                !DEP(I,K)=0.0
                ELES(I,K)=0.0
                !ETA(I,K)=GL(I,K)+DEP(I,K)
                !HH(I,K)=0.0

 701    CONTINUE

        !END IF
        !IF(pier.EQ.1.0) THEN

            !DO 7011 J=1,JMAX+3
            !DO 7011 K=Kpier_R1,Kpier_L1
            !DO 7011 I=Ipier_U1,Ipier_D1

                !C(I,J,K)=CSMAX
                !IF(J.GE.IGL(I,K)) C(I,J,K)=0.0
                !CM(I,J,K)=C(I,J,K)
                !HK(I,J,K)=0.0
                !U(I,J,K)=0.0
                !V(I,J,K)=0.0
                !W(I,J,K)=0.0
                !US(I,J,K)=0.0
                !VS(I,J,K)=0.0
                !WS(I,J,K)=0.0
                !UT(I,K)=0.0
                !WT(I,K)=0.0
                !!DEP(I,K)=0.0
                !ELES(I,K)=0.0
                !!ETA(I,K)=GL(I,K)+DEP(I,K)
                !HH(I,K)=0.0

 7011   CONTINUE

        !END IF
        !IF(pier.EQ.1.0) THEN

            !DO 7012 J=1,JMAX+3
            !DO 7012 K=Kpier_R2,Kpier_L2
            !DO 7012 I=Ipier_U2,Ipier_D2

                !C(I,J,K)=CSMAX
                !IF(J.GE.IGL(I,K)) C(I,J,K)=0.0
                !CM(I,J,K)=C(I,J,K)
                !HK(I,J,K)=0.0
                !U(I,J,K)=0.0
                !V(I,J,K)=0.0
                !W(I,J,K)=0.0
                !US(I,J,K)=0.0
                !VS(I,J,K)=0.0
                !WS(I,J,K)=0.0
                !UT(I,K)=0.0
                !WT(I,K)=0.0
                !!DEP(I,K)=0.0
                !ELES(I,K)=0.0
                !!ETA(I,K)=GL(I,K)+DEP(I,K)
                !!HH(I,K)=0.0

 7012   CONTINUE

        !END IF
        !IF(pier.EQ.1.0) THEN

            !DO 7013 J=1,JMAX+3
            !DO 7013 K=Kpier_R3,Kpier_L3
            !DO 7013 I=Ipier_U3,Ipier_D3

                !C(I,J,K)=CSMAX
                !IF(J.GE.IGL(I,K)) C(I,J,K)=0.0
                !CM(I,J,K)=C(I,J,K)
                !HK(I,J,K)=0.0
                !U(I,J,K)=0.0
                !V(I,J,K)=0.0
                !W(I,J,K)=0.0
                !US(I,J,K)=0.0
                !VS(I,J,K)=0.0
                !WS(I,J,K)=0.0
                !UT(I,K)=0.0
                !WT(I,K)=0.0
                !!DEP(I,K)=0.0
                !ELES(I,K)=0.0
                !!ETA(I,K)=GL(I,K)+DEP(I,K)
                !!HH(I,K)=0.0

 7013   CONTINUE

        !END IF
        !IF(pier.EQ.1.0) THEN

            !DO 7014 J=1,JMAX+3
            !DO 7014 K=Kpier_R4,Kpier_L4
            !DO 7014 I=Ipier_U4,Ipier_D4

                !C(I,J,K)=CSMAX
                !IF(J.GE.IGL(I,K)) C(I,J,K)=0.0
                !CM(I,J,K)=C(I,J,K)
                !HK(I,J,K)=0.0
                !U(I,J,K)=0.0
                !V(I,J,K)=0.0
                !W(I,J,K)=0.0
                !US(I,J,K)=0.0
                !VS(I,J,K)=0.0
                !WS(I,J,K)=0.0
                !UT(I,K)=0.0
                !WT(I,K)=0.0
                !!DEP(I,K)=0.0
                !ELES(I,K)=0.0
                !!ETA(I,K)=GL(I,K)+DEP(I,K)
                !!HH(I,K)=0.0

 7014   CONTINUE

        END IF

!********** 3D PARAMETA INITIAL CHECK **********!

        !OPEN(88,file='INI3D/INI3D.txt')
        !    DO I=1,IMAX+4
        !    DO K=1,KMAX+4
        !    DO J=1,JMAX+3
        !        WRITE(88,'(3i5,9f15.5)') I,J,K,U(I,J,K),V(I,J,K),W(I,J,K),HK(I,J,K)
        !    END DO
        !    END DO
        !    END DO
        !CLOSE(88)

!********** 2D PARAMETA INITIAL CHECK **********!

        !OPEN(88,file='INI2D/INI2D.txt')
        !    DO I=1,IMAX+4
        !    DO K=1,KMAX+4
        !        WRITE(88,'(5i5,9f15.5)') I,J,K,IGL(I,K),IETA(I,K) &
        !                                    ,GL(I,K),Y(I,IGL(I,K),K) &
        !                                    ,RNM(I,K),QQQ(I),DEP(I,K) &
        !                                    ,ETA(I,K),UT(I,K),CFF(I,K) &
        !                                    ,USTAB(I,K)
        !    END DO
        !    END DO
        !CLOSE(88)

        DO 910 I=1,IMAX+4
        DO 910 J=1,JMAX+3
        DO 910 K=1,KMAX+4

            U1(I,J,K)=U(I,J,K)
            V1(I,J,K)=V(I,J,K)
            W1(I,J,K)=W(I,J,K)

            USTA1(I)=USTA(I)
            VSTA1(I)=VSTA(I)
            USTAu1(I)=USTAu(I)
            VSTAu1(I)=VSTAu(I)

            GL1(I,K)=GL(I,K)      !SUMO23/03/27
            DEP1(I,K)=DEP(I,K)      !SUMO23/03/27
            ETA1(I,K)=ETA(I,K)      !SUMO23/03/27
            ELES1(I,K)=ELES(I,K)

            US1(I,J,K)=US(I,J,K)
            VS1(I,J,K)=VS(I,J,K)
            WS1(I,J,K)=WS(I,J,K)
            C1(I,J,K)=C(I,J,K)
            U2(I,J,K)=U1(I,J,K)
            V2(I,J,K)=V1(I,J,K)
            W2(I,J,K)=W1(I,J,K)
            US2(I,J,K)=US1(I,J,K)
            VS2(I,J,K)=VS1(I,J,K)
            WS2(I,J,K)=WS1(I,J,K)

            USR1(I,J,K)=USR(I,J,K)
            VSR1(I,J,K)=VSR(I,J,K)
            WSR1(I,J,K)=WSR(I,J,K)
            UUSR1(I,J,K)=UUSR(I,J,K)
            VVSR1(I,J,K)=VVSR(I,J,K)
            WWSR1(I,J,K)=WWSR(I,J,K)
            TEMP1(I,J,K)=TEMP(I,J,K)

            P1(I,J,K)=P(I,J,K)
            HK1(I,J,K)=HK(I,J,K)
            SAL1(I,J,K)=SAL(I,J,K)
            APP1(I)=APP(I)
            Q1(I)=Q(I)

            USTABRW1(I,J)=USTABRW(I,J)
            USTABLW1(I,J)=USTABLW(I,J)
            USTABRW_P1(I,J)=USTABRW_P(I,J)
            USTABLW_P1(I,J)=USTABLW_P(I,J)
            WSTABFW_P1(J,K)=WSTABFW_P(J,K)
            WSTABBW_P1(J,K)=WSTABBW_P(J,K)

 910    CONTINUE

    RETURN
    END

!C**************************************************************
      SUBROUTINE KAKI2
!C**************************************************************

    USE MF_SF
    USE OMP_LIB
    IMPLICIT NONE

    ch10_0=char(48+int(mod(N,10)/1))
    ch10_1=char(48+int(mod(N,100)/10))
    ch10_2=char(48+int(mod(N,1000)/100))
    ch10_3=char(48+int(mod(N,10000)/1000))
    ch10_4=char(48+int(mod(N,100000)/10000))
    ch10_5=char(48+int(mod(N,1000000)/100000))
    ch10_6=char(48+int(mod(N,10000000)/1000000))
    ch10_7=char(48+int(mod(N,100000000)/10000000))

        OPEN(601,file='Results/tec_jc/tec_jc_DT=' &
                     //ch10_7//ch10_6//ch10_5//ch10_4 &
                     //ch10_3//ch10_2//ch10_1//ch10_0//'.dat')
        WRITE(601,*)"tecplot"
        WRITE(601,699)

 699    FORMAT('VARIABLES ="TIME",   "I",   "K",   "J",   "X",   "Z",   "Y",   "C",   "U",   "W",   "V" &
      ,   "WWX",   "WWZ",   "WWY",   "P",   "Qvalue",   "MYT",   "HK"')
        WRITE(601,698)TIME,IMAX,KMAX,JMAX-1

 698    FORMAT(' zone t="tecplot",strandid=1,solutiontime='F10.5,'  i=',i10,"  j=",i10,"  k=",i10)

!CCCCCC////// Pier Boundary Condition ////////CCCCCC

        IF(pier.EQ.1.0) THEN

            DO 702 J=1,JMAX+3
            DO 702 K=Kpier_R, Kpier_L
            DO 702 I=Ipier_U, Ipier_D

                CORIC(I,J,K)=C(I,J,K)
                CORICM(I,J,K)=CM(I,J,K)
                C(I,J,K)=0.600
                CM(I,J,K)=C(I,J,K)

 702    CONTINUE

        END IF

        IF(pier.EQ.1.0) THEN

            DO 7021 J=1,JMAX+3
            DO 7021 K=Kpier_R1,Kpier_L1
            DO 7021 I=Ipier_U1,Ipier_D1

                CORIC(I,J,K)=C(I,J,K)
                CORICM(I,J,K)=CM(I,J,K)
                C(I,J,K)=0.600
                CM(I,J,K)=C(I,J,K)

 7021   CONTINUE

        END IF

        IF(pier.EQ.1.0) THEN

            DO 7022 J=1,JMAX+3
            DO 7022 K=Kpier_R2,Kpier_L2
            DO 7022 I=Ipier_U2,Ipier_D2

                CORIC(I,J,K)=C(I,J,K)
                CORICM(I,J,K)=CM(I,J,K)
                C(I,J,K)=0.600
                CM(I,J,K)=C(I,J,K)

 7022   CONTINUE

        END IF

        IF(pier.EQ.1.0) THEN

            DO 7023 J=1,JMAX+3
            DO 7023 K=Kpier_R3,Kpier_L3
            DO 7023 I=Ipier_U3,Ipier_D3

                CORIC(I,J,K)=C(I,J,K)
                CORICM(I,J,K)=CM(I,J,K)
                C(I,J,K)=0.600
                CM(I,J,K)=C(I,J,K)

 7023   CONTINUE

        END IF

        IF(pier.EQ.1.0) THEN

            DO 7024 J=1,JMAX+3
            DO 7024 K=Kpier_R4,Kpier_L4
            DO 7024 I=Ipier_U4,Ipier_D4

                CORIC(I,J,K)=C(I,J,K)
                CORICM(I,J,K)=CM(I,J,K)
                C(I,J,K)=0.600
                CM(I,J,K)=C(I,J,K)

 7024   CONTINUE

        END IF

        DO 400 J=JMAX+1,3,-1    !J=3,JMAX+1
        DO 400 K=3,KMAX+2
        DO 400 I=3,IMAX+2

            IF (ABS(C(I,J,K)).LT.1.0E-20) THEN
                C(I,J,K)=0.0
            END IF
            IF (ABS(U(I,J,K)).LT.1.0E-20) THEN
                U(I,J,K)=0.0
            END IF
            IF (ABS(V(I,J,K)).LT.1.0E-20) THEN
                V(I,J,K)=0.0
            END IF
            IF (ABS(W(I,J,K)).LT.1.0E-20) THEN
                W(I,J,K)=0.0
            END IF
            IF (ABS(US(I,J,K)).LT.1.0E-20) THEN
                US(I,J,K)=0.0
            END IF
            IF (ABS(VS(I,J,K)).LT.1.0E-20) THEN
                VS(I,J,K)=0.0
            END IF
            IF (ABS(WS(I,J,K)).LT.1.0E-20) THEN
                WS(I,J,K)=0.0
            END IF

                WRITE(601,'(f10.5,3i10,40f20.7,i12)')TIME,I,K,J, &
                    X(I,J,K),Z(I,J,K),Y(I,J,K),CM(I,J,K),U(I,J,K),W(I,J,K),V(I,J,K), &
                    WWX(I,J,K),WWZ(I,J,K),WWY(I,J,K),P(I,J,K),Qvalue(I,J,K),MYT(I,J,K),HK(I,J,K)

 400    CONTINUE

        CLOSE(601)

!CCCCCC////// Pier Boundary Condition ////////CCCCCC

        IF(pier.EQ.1.0) THEN

            DO 703 J=1,JMAX+3
            DO 703 K=Kpier_R, Kpier_L
            DO 703 I=Ipier_U, Ipier_D

                C(I,J,K)=CORIC(I,J,K)
                CM(I,J,K)=CORICM(I,J,K)

 703    CONTINUE

        END IF

        IF(pier.EQ.1.0) THEN

            DO 7031 J=1,JMAX+3
            DO 7031 K=Kpier_R1,Kpier_L1
            DO 7031 I=Ipier_U1,Ipier_D1

                C(I,J,K)=CORIC(I,J,K)
                CM(I,J,K)=CORICM(I,J,K)

 7031   CONTINUE

        END IF

        IF(pier.EQ.1.0) THEN

            DO 7032 J=1,JMAX+3
            DO 7032 K=Kpier_R2,Kpier_L2
            DO 7032 I=Ipier_U2,Ipier_D2

                C(I,J,K)=CORIC(I,J,K)
                CM(I,J,K)=CORICM(I,J,K)

 7032   CONTINUE

        END IF

        IF(pier.EQ.1.0) THEN

            DO 7033 J=1,JMAX+3
            DO 7033 K=Kpier_R3,Kpier_L3
            DO 7033 I=Ipier_U3,Ipier_D3

                C(I,J,K)=CORIC(I,J,K)
                CM(I,J,K)=CORICM(I,J,K)

 7033   CONTINUE

        END IF

        IF(pier.EQ.1.0) THEN

            DO 7034 J=1,JMAX+3
            DO 7034 K=Kpier_R4,Kpier_L4
            DO 7034 I=Ipier_U4,Ipier_D4

                C(I,J,K)=CORIC(I,J,K)
                CM(I,J,K)=CORICM(I,J,K)

 7034   CONTINUE

        END IF

    RETURN
    END

!C**************************************************************
      SUBROUTINE KAKI2F
!C**************************************************************

    USE MF_SF
    USE OMP_LIB
    IMPLICIT NONE

    allocate (KU(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),KUS(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
             ,KV(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),KVS(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
             ,KW(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),KWS(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
             ,KC(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),KMYT(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
             ,KHK(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),KP(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
             ,WWX(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),WWY(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
             ,WWZ(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),Qvalue(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
             ,KF(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
             ,STAT = ALLOC_ERR )

        DO 1301 K=3,KMAX+2
        DO 1301 I=3,IMAX+2
        DO 1301 J=3,JMAX+1

            WWX(I,J,K)=(W(I,J+1,K)-W(I,J-1,K))*0.50*EY(J) &
                      -(V(I,J,K+1)-V(I,J,K-1))*0.50*DZ(K)
            WWY(I,J,K)=(U(I,J,K+1)-U(I,J,K-1))*0.50*DZ(K) &
                      -(W(I+1,J,K)-W(I-1,J,K))*0.50*GX(I)
            WWZ(I,J,K)=(V(I+1,J,K)-V(I-1,J,K))*0.50*GX(I) &
                      -(U(I,J+1,K)-U(I,J-1,K))*0.50*EY(J)

            Qvalue(I,J,K)=(((W(I,J+1,K)-W(I,J-1,K))*0.50*EY(J))* &
                           ((V(I,J,K+1)-V(I,J,K-1))*0.50*DZ(K))+ &
                           ((U(I,J,K+1)-U(I,J,K-1))*0.50*DZ(K))* &
                           ((W(I+1,J,K)-W(I-1,J,K))*0.50*GX(I))+ &
                           ((V(I+1,J,K)-V(I-1,J,K))*0.50*GX(I))* &
                           ((U(I,J+1,K)-U(I,J-1,K))*0.50*EY(J)))*(-0.500)

 1301   CONTINUE

        DO 5228 K=3,KMAX+2
        DO 5228 I=3,IMAX+2
        DO 5228 J=3,JMAX+1

            IF(J.LT.IETA(I,K)) THEN
                KF(I,J,K)=1.0d0
            ELSE IF(J.GT.IETA(I,K)) THEN
                KF(I,J,K)=0.0D0
            ELSE
                KF(I,J,K)=DETA(I,K)*EY(J)
            END IF

 5228   CONTINUE

!$OMP PARALLEL
!$OMP DO
        DO K=3,KMAX+2
        DO I=3,IMAX+2
        DO J=3,JMAX+1

            IF (ABS(C(I,J,K)).LT.1.0E-20) THEN
                C(I,J,K)=0.0
            END IF
            IF (ABS(U(I,J,K)).LT.1.0E-20) THEN
                U(I,J,K)=0.0
            END IF
            IF (ABS(V(I,J,K)).LT.1.0E-20) THEN
                V(I,J,K)=0.0
            END IF
            IF (ABS(W(I,J,K)).LT.1.0E-20) THEN
                W(I,J,K)=0.0
            END IF
            IF (ABS(US(I,J,K)).LT.1.0E-20) THEN
                US(I,J,K)=0.0
            END IF
            IF (ABS(VS(I,J,K)).LT.1.0E-20) THEN
                VS(I,J,K)=0.0
            END IF
            IF (ABS(WS(I,J,K)).LT.1.0E-20) THEN
                WS(I,J,K)=0.0
            END IF

            KU(I,J,K)=U(I,J,K)
            KV(I,J,K)=V(I,J,K)
            KW(I,J,K)=W(I,J,K)
            KUS(I,J,K)=US(I,J,K)
            KVS(I,J,K)=VS(I,J,K)
            KWS(I,J,K)=WS(I,J,K)
            KC(I,J,K)=C(I,J,K)
            KMYT(I,J,K)=MYT(I,J,K)
            KHK(I,J,K)=HK(I,J,K)
            KP(I,J,K)=P(I,J,K)

        END DO
        END DO
        END DO
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 1333 K=1,KMAX+4
        DO 1333 I=1,IMAX+4
        DO 1333 J=1,JMAX+3

            IF(J.GE.IETA(I,K)+1) THEN
                KU(I,J,K)=0.0
                KV(I,J,K)=0.0
                KW(I,J,K)=0.0
                KUS(I,J,K)=0.0
                KVS(I,J,K)=0.0
                KWS(I,J,K)=0.0
                WWX(I,J,K)=0.0
                WWY(I,J,K)=0.0
                WWZ(I,J,K)=0.0
                Qvalue(I,J,K)=0.0
                KC(I,J,K)=0.0
                KMYT(I,J,K)=0.0
                KHK(I,J,K)=0.0
                KP(I,J,K)=0.0
            END IF
            IF(J.LE.IGL(I,K)) THEN
                KU(I,J,K)=0.0
                KV(I,J,K)=0.0
                KW(I,J,K)=0.0
                KUS(I,J,K)=0.0
                KVS(I,J,K)=0.0
                KWS(I,J,K)=0.0
                WWX(I,J,K)=0.0
                WWY(I,J,K)=0.0
                WWZ(I,J,K)=0.0
                Qvalue(I,J,K)=0.0
                KC(I,J,K)=1.00
                KMYT(I,J,K)=0.0
                KHK(I,J,K)=0.0
                KP(I,J,K)=0.0
            END IF

 1333   CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!CCCCCC////// Pier Boundary Condition ////////CCCCCC

        IF(pier.EQ.1.0) THEN
!$OMP PARALLEL
!$OMP DO
            DO 707 J=1,JMAX+3
            DO 707 K=Kpier_R,Kpier_L
            DO 707 I=Ipier_U,Ipier_D

                KU(I,J,K)=0.0
                KV(I,J,K)=0.0
                KW(I,J,K)=0.0
                KUS(I,J,K)=0.0
                KVS(I,J,K)=0.0
                KWS(I,J,K)=0.0
                WWX(I,J,K)=0.0
                WWY(I,J,K)=0.0
                WWZ(I,J,K)=0.0
                Qvalue(I,J,K)=0.0
                KC(I,J,K)=1.00
                KMYT(I,J,K)=0.0
                KHK(I,J,K)=0.0
                KP(I,J,K)=0.0

 707    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

        !END IF
        !IF(pier.EQ.1.0) THEN
!!$OMP PARALLEL
!!$OMP DO
            !DO 7071 J=1,JMAX+3
            !DO 7071 K=Kpier_R1,Kpier_L1
            !DO 7071 I=Ipier_U1,Ipier_D1

                !KU(I,J,K)=0.0
                !KV(I,J,K)=0.0
                !KW(I,J,K)=0.0
                !KUS(I,J,K)=0.0
                !KVS(I,J,K)=0.0
                !KWS(I,J,K)=0.0
                !WWX(I,J,K)=0.0
                !WWY(I,J,K)=0.0
                !WWZ(I,J,K)=0.0
                !Qvalue(I,J,K)=0.0
                !KC(I,J,K)=CSMAX
                !KMYT(I,J,K)=0.0
                !KHK(I,J,K)=0.0
                !KP(I,J,K)=0.0

 7071   CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL

        !END IF
        !IF(pier.EQ.1.0) THEN
!!$OMP PARALLEL
!!$OMP DO
            !DO 7072 J=1,JMAX+3
            !DO 7072 K=Kpier_R2,Kpier_L2
            !DO 7072 I=Ipier_U2,Ipier_D2

                !KU(I,J,K)=0.0
                !KV(I,J,K)=0.0
                !KW(I,J,K)=0.0
                !KUS(I,J,K)=0.0
                !KVS(I,J,K)=0.0
                !KWS(I,J,K)=0.0
                !WWX(I,J,K)=0.0
                !WWY(I,J,K)=0.0
                !WWZ(I,J,K)=0.0
                !Qvalue(I,J,K)=0.0
                !KC(I,J,K)=CSMAX
                !KMYT(I,J,K)=0.0
                !KHK(I,J,K)=0.0
                !KP(I,J,K)=0.0

 7072   CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL

        !END IF
        !IF(pier.EQ.1.0) THEN
!!$OMP PARALLEL
!!$OMP DO
            !DO 7073 J=1,JMAX+3
            !DO 7073 K=Kpier_R3,Kpier_L3
            !DO 7073 I=Ipier_U3,Ipier_D3

                !KU(I,J,K)=0.0
                !KV(I,J,K)=0.0
                !KW(I,J,K)=0.0
                !KUS(I,J,K)=0.0
                !KVS(I,J,K)=0.0
                !KWS(I,J,K)=0.0
                !WWX(I,J,K)=0.0
                !WWY(I,J,K)=0.0
                !WWZ(I,J,K)=0.0
                !Qvalue(I,J,K)=0.0
                !KC(I,J,K)=CSMAX
                !KMYT(I,J,K)=0.0
                !KHK(I,J,K)=0.0
                !KP(I,J,K)=0.0

 7073   CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL

        !END IF
        !IF(pier.EQ.1.0) THEN
!!$OMP PARALLEL
!!$OMP DO
            !DO 7074 J=1,JMAX+3
            !DO 7074 K=Kpier_R4,Kpier_L4
            !DO 7074 I=Ipier_U4,Ipier_D4

                !KU(I,J,K)=0.0
                !KV(I,J,K)=0.0
                !KW(I,J,K)=0.0
                !KUS(I,J,K)=0.0
                !KVS(I,J,K)=0.0
                !KWS(I,J,K)=0.0
                !WWX(I,J,K)=0.0
                !WWY(I,J,K)=0.0
                !WWZ(I,J,K)=0.0
                !Qvalue(I,J,K)=0.0
                !KC(I,J,K)=CSMAX
                !KMYT(I,J,K)=0.0
                !KHK(I,J,K)=0.0
                !KP(I,J,K)=0.0

 7074   CONTINUE
!!$OMP END DO
!!$OMP END PARALLEL

        END IF

        ch10_0=char(48+int(mod(N,10)/1))
        ch10_1=char(48+int(mod(N,100)/10))
        ch10_2=char(48+int(mod(N,1000)/100))
        ch10_3=char(48+int(mod(N,10000)/1000))
        ch10_4=char(48+int(mod(N,100000)/10000))
        ch10_5=char(48+int(mod(N,1000000)/100000))
        ch10_6=char(48+int(mod(N,10000000)/1000000))
        ch10_7=char(48+int(mod(N,100000000)/10000000))

        OPEN(601,file='Results/tec_jc/tec_jc_DT=' &
                     //ch10_7//ch10_6//ch10_5//ch10_4 &
                     //ch10_3//ch10_2//ch10_1//ch10_0//'.dat')
        WRITE(601,*)"tecplot"
        WRITE(601,699)

 699    FORMAT('VARIABLES ="TIME",   "I",   "K",   "J",   "X",   "Z",   "Y",   "U",   "W",   "V" &
                                 ,   "P",   "DEP",    "MYT",   "HK",   "F"')
        WRITE(601,698)TIME,IMAX,KMAX,JMAX-1

 698    FORMAT(' zone t="tecplot",strandid=1,solutiontime='F10.3,'  i=',i10,"  j=",i10,"  k=",i10)

        DO 400 J=JMAX+1,3,-1    !J=3,JMAX+1
        DO 400 K=3,KMAX+2
        DO 400 I=3,IMAX+2

            WRITE(601,'(f10.3,3i10,40f20.7,i12)')TIME,I,K,J,X(I,J,K),Z(I,J,K),Y(I,J,K) &
                                                ,KU(I,J,K),KW(I,J,K),KV(I,J,K) &
                                                ,KP(I,J,K),DEP(I,K),KMYT(I,J,K),KHK(I,J,K),KF(I,J,K)

 400    CONTINUE

        CLOSE(601)

!        OPEN(602,file='Results-surf/tec_jc/tec_jc_DT=' &
!                       //ch10_7//ch10_6//ch10_5//ch10_4 &
!                       //ch10_3//ch10_2//ch10_1//ch10_0//'.dat')
!        WRITE(602,*)"tecplot"
!        WRITE(602,697)
!        WRITE(602,696) TIME,IMAX,KMAX,1
!
! 697    FORMAT('VARIABLES ="TIME",   "I",   "K",   "J",   "X",   "Z",   "C",   "U",   "W" &
!                                 ,   "WWY",   "P",    "Qvalue",   "MYT",   "HK",   "DEP",   "ETA"')
!
! 696    FORMAT(' zone t="tecplot",strandid=1,solutiontime='F10.3,'  i=',i10,"  j=",i10,"  k=",i10)
!
!        DO 401 K=3,KMAX+2
!        DO 401 I=3,IMAX+2
!
!            J=IETA(I,K)-1
!
!            WRITE(602,'(f10.3,3i10,40f20.7,i12)')TIME,I,K,J,0.500*(X(I,J,K)+X(I,J+1,K)),0.500*(Z(I,J,K)+Z(I,J+1,K)) &
!                                                ,0.500*(KC(I,J,K)+KC(I,J+1,K)),0.500*(KU(I,J,K)+KU(I,J+1,K)) &
!                                                ,0.500*(KW(I,J,K)+KW(I,J+1,K)) &
!                                                ,0.500*(WWY(I,J,K)+WWY(I,J+1,K)),0.500*(KP(I,J,K)+KP(I,J+1,K)) &
!                                                ,0.500*(Qvalue(I,J,K)+Qvalue(I,J+1,K)) &
!                                                ,0.500*(KMYT(I,J,K)+KMYT(I,J+1,K)),0.500*(KHK(I,J,K)+KHK(I,J+1,K)) &
!                                                ,DEP(I,K),ETA(I,K)
!
! 401    CONTINUE
!
!        CLOSE(602)
!
!        OPEN(605,file='Results-bom/tec_jc/tec_jc_DT=' &
!                       //ch10_7//ch10_6//ch10_5//ch10_4 &
!                       //ch10_3//ch10_2//ch10_1//ch10_0//'.dat')
!        WRITE(605,*)"tecplot"
!        WRITE(605,497)
!        WRITE(605,496) TIME,IMAX,KMAX,1
!
! 497    FORMAT('VARIABLES ="TIME",   "I",   "K",   "X",   "Z",   "C",   "U",   "W" &
!                                 ,   "WWY",   "P",    "Qvalue",   "MYT",   "HK",   "DEP",   "ETA"')!
!
! 496    FORMAT(' zone t="tecplot",strandid=1,solutiontime='F10.3,'  i=',i10,"  j=",i10,"  k=",i10)!
!
!        DO 461 K=3,KMAX+2
!        DO 461 I=3,IMAX+2
!
!            !J=IGL(I,K)+1    !deltaY=0.38
!            !J=IGL(I,K)+2    !deltaY=0.19
!            J=6
!
!            WRITE(605,'(f10.3,2i10,40f20.7,i12)')TIME,I,K,0.500*(X(I,J,K)+X(I,J+1,K)),0.500*(Z(I,J,K)+Z(I,J+1,K)) &
!                                                ,0.500*(KC(I,J,K)+KC(I,J+1,K)),0.500*(KU(I,J,K)+KU(I,J+1,K)) &
!                                                ,0.500*(KW(I,J,K)+KW(I,J+1,K)) &
!                                                ,0.500*(WWY(I,J,K)+WWY(I,J+1,K)),0.500*(KP(I,J,K)+KP(I,J+1,K)) &
!                                                ,0.500*(Qvalue(I,J,K)+Qvalue(I,J+1,K)) &
!                                                ,0.500*(KMYT(I,J,K)+KMYT(I,J+1,K)),0.500*(KHK(I,J,K)+KHK(I,J+1,K)) &
!                                                ,DEP(I,K),ETA(I,K)
!
! 461    CONTINUE
!
!        CLOSE(605)
!
!        OPEN(603,file='Results-sui/tec_jc/tec_jc_DT=' &
!                       //ch10_7//ch10_6//ch10_5//ch10_4 &
!                       //ch10_3//ch10_2//ch10_1//ch10_0//'.dat')
!        WRITE(603,*)"tecplot"
!        WRITE(603,695)
!        WRITE(603,694) TIME,IMAX,KMAX
!
! 695    FORMAT('VARIABLES ="TIME",   "I",   "K",   "X",   "Z",   "C",   "GL",   "DEP",   "ETA"')
!
! 694    FORMAT(' zone t="tecplot",strandid=1,solutiontime='F10.3,'  i=',i10,"  j=",i10)
!
!        DO 402 K=3,KMAX+2
!        DO 402 I=3,IMAX+2
!
!            J=IGL(I,K)
!
!            WRITE(603,'(f10.3,2i10,40f20.7,i12)')TIME,I,K, &
!                X(I,J,K),Z(I,J,K),C(I,J,K),GL(I,K),DEP(I,K),ETA(I,K)
!
! 402    CONTINUE
!
!        CLOSE(603)

    deallocate (KU,KUS,KV,KVS,KW,KWS,KC,KMYT,KHK,KP,WWX,WWY,WWZ,Qvalue)

    RETURN
    END

!C**************************************************************
    SUBROUTINE KAKI3
!C**************************************************************

    USE MF_SF
    USE OMP_LIB
    IMPLICIT NONE

    allocate (KU(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),KUS(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
             ,KV(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),KVS(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
             ,KW(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),KWS(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
             ,KC(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),KMYT(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
             ,KHK(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),KP(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
             ,WWX(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),WWY(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
             ,WWZ(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4),Qvalue(-1:IMAX+4,-1:JMAX+4,-1:KMAX+4) &
             ,STAT = ALLOC_ERR )

        !DO 1301 K=3,KMAX+2
        !DO 1301 I=3,IMAX+2
        !DO 1301 J=3,JMAX+1

            !WWX(I,J,K)=(W(I,J+1,K)-W(I,J-1,K))*0.50*EY(J) &
            !          -(V(I,J,K+1)-V(I,J,K-1))*0.50*DZ(K)
            !WWY(I,J,K)=(U(I,J,K+1)-U(I,J,K-1))*0.50*DZ(K) &
            !          -(W(I+1,J,K)-W(I-1,J,K))*0.50*GX(I)
            !WWZ(I,J,K)=(V(I+1,J,K)-V(I-1,J,K))*0.50*GX(I) &
            !          -(U(I,J+1,K)-U(I,J-1,K))*0.50*EY(J)

            !Qvalue(I,J,K)=(((W(I,J+1,K)-W(I,J-1,K))*0.50*EY(J))* &
            !               ((V(I,J,K+1)-V(I,J,K-1))*0.50*DZ(K))+ &
            !               ((U(I,J,K+1)-U(I,J,K-1))*0.50*DZ(K))* &
            !               ((W(I+1,J,K)-W(I-1,J,K))*0.50*GX(I))+ &
            !               ((V(I+1,J,K)-V(I-1,J,K))*0.50*GX(I))* &
            !               ((U(I,J+1,K)-U(I,J-1,K))*0.50*EY(J)))*(-0.500)

 1301   CONTINUE

!$OMP PARALLEL
!$OMP DO
        DO K=3,KMAX+2
        DO I=3,IMAX+2
        DO J=3,JMAX+1

            IF (ABS(C(I,J,K)).LT.1.0E-20) THEN
                C(I,J,K)=0.0
            END IF
            IF (ABS(U(I,J,K)).LT.1.0E-20) THEN
                U(I,J,K)=0.0
            END IF
            IF (ABS(V(I,J,K)).LT.1.0E-20) THEN
                V(I,J,K)=0.0
            END IF
            IF (ABS(W(I,J,K)).LT.1.0E-20) THEN
                W(I,J,K)=0.0
            END IF
            IF (ABS(US(I,J,K)).LT.1.0E-20) THEN
                US(I,J,K)=0.0
            END IF
            IF (ABS(VS(I,J,K)).LT.1.0E-20) THEN
                VS(I,J,K)=0.0
            END IF
            IF (ABS(WS(I,J,K)).LT.1.0E-20) THEN
                WS(I,J,K)=0.0
            END IF

            KU(I,J,K)=U(I,J,K)
            KV(I,J,K)=V(I,J,K)
            KW(I,J,K)=W(I,J,K)
            KUS(I,J,K)=US(I,J,K)
            KVS(I,J,K)=VS(I,J,K)
            KWS(I,J,K)=WS(I,J,K)
            KC(I,J,K)=C(I,J,K)
            KMYT(I,J,K)=MYT(I,J,K)
            KHK(I,J,K)=HK(I,J,K)
            KP(I,J,K)=P(I,J,K)

        END DO
        END DO
        END DO
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 1333 K=1,KMAX+4
        DO 1333 I=1,IMAX+4
        DO 1333 J=1,JMAX+3

            IF(J.GE.IETA(I,K)+1) THEN
                KU(I,J,K)=0.0
                KV(I,J,K)=0.0
                KW(I,J,K)=0.0
                KUS(I,J,K)=0.0
                KVS(I,J,K)=0.0
                KWS(I,J,K)=0.0
                WWX(I,J,K)=0.0
                WWY(I,J,K)=0.0
                WWZ(I,J,K)=0.0
                Qvalue(I,J,K)=0.0
                KC(I,J,K)=0.0
                KMYT(I,J,K)=0.0
                KHK(I,J,K)=0.0
                KP(I,J,K)=0.0
            END IF

 1333   CONTINUE
!$OMP END DO
!$OMP END PARALLEL

        ch10_0=char(48+int(mod(N,10)/1))
        ch10_1=char(48+int(mod(N,100)/10))             
        ch10_2=char(48+int(mod(N,1000)/100))
        ch10_3=char(48+int(mod(N,10000)/1000))
        ch10_4=char(48+int(mod(N,100000)/10000))
        ch10_5=char(48+int(mod(N,1000000)/100000))
        ch10_6=char(48+int(mod(N,10000000)/1000000))
        ch10_7=char(48+int(mod(N,100000000)/10000000))

    open(601,file='Results/tec_jc/tec_jc_DT=' &
                   //ch10_7//ch10_6//ch10_5//ch10_4 &
                   //ch10_3//ch10_2//ch10_1//ch10_0//'.dat')
    write(601,*)"tecplot"
    write(601,699)
 699    format('VARIABLES ="TIME",   "I",   "K",   "J",   &
                           "X",   "Z",   "Y",   "U",   "W",   "V",   "GL",   "DEP",   "ETA"')
    write(601,698)TIME,IMAX,KMAX,JMAX-1
 698    format(' zone t="tecplot",strandid=1,solutiontime='F10.3,'  i=',i10,"  j=",i10,"  k=",i10)

        DO 400 J=JMAX+1,3,-1    !J=3,JMAX+1
        DO 400 K=3,KMAX+2
        DO 400 I=3,IMAX+2

                write(601,'(f10.5,3i10,9f20.7,i10)')TIME,I,K,J, &
                    X(I,J,K),Z(I,J,K),Y(I,J,K),KU(I,J,K),KW(I,J,K),KV(I,J,K),GL(I,K),DEP(I,K),ETA(I,K)
 400    CONTINUE

    close(601)

    return
    end
    
!C**************************************************************
    SUBROUTINE KAKI4
!C**************************************************************

    USE MF_SF
    USE OMP_LIB
    IMPLICIT NONE

        write(98,'(f10.5,3f20.7,i10)') TIME,DEP(3,7),DEP(53,7),DEP(IMAX+2,7)

    return
    end

!C**************************************************************
      SUBROUTINE HS !Output for Hot Start
!C**************************************************************
!
      USE MF_SF
      USE OMP_LIB
      IMPLICIT NONE
!
!            IF(NHS.EQ.1)THEN
            WRITE(num,'(I6.6)')N
            OPEN(33,FILE='outputHS/HS_'//num//'.txt')
            DO J=1,JMAX+3
                DO I=1,IMAX+4
                    DO K=1,KMAX+4
                        WRITE(33,'(40f20.7)') X(I,J,K),Y(I,J,K),Z(I,J,K),U(I,J,K),V(I,J,K),W(I,J,K) &
                          ,XG1(I,J,K),YG(I,J,K),ZG(I,J,K),US(I,J,K),VS(I,J,K),WS(I,J,K) &
                          ,C(I,J,K),CM(I,J,K),USR(I,J,K),VSR(I,J,K),WSR(I,J,K),P(I,J,K),PSI(I,J,K) &
                          ,UT(I,K),WT(I,K),DEP(I,K),GL(I,K),ELES(I,K),ETA(I,K),HK(I,J,K),HH(I,K)
                    END DO
                END DO
            END DO
            CLOSE(33)
!
      RETURN
      END
!C
!C**************************************************************
      SUBROUTINE READ_HS !Output for Hot Start
!C**************************************************************
!
      USE MF_SF
      USE OMP_LIB
      IMPLICIT NONE
!
!
            OPEN(10,FILE= 'INPUT/10_HS.txt')
                READ(10,*) HSTIME   !HS??øΩ?øΩJ??øΩ?øΩn??øΩ?øΩ??øΩ?øΩ??øΩ?øΩ‘ÇÔøΩ??øΩ?øΩ??øΩ?øΩ??øΩ?øΩ(s)
            CLOSE(10)
!            NMIN=NINT(HSTIME)
!            NMIN=INT(HSTIME*1000)
            NMIN=NINT(HSTIME/DT)
            WRITE(*,*) NMIN !Step Number           
                WRITE(num,'(I6.6)')NMIN
                OPEN(35,FILE='outputHS/HS_'//num//'.txt')
                READ(35,*) X(I,J,K),Y(I,J,K),Z(I,J,K),U(I,J,K),V(I,J,K),W(I,J,K) &
                          ,XG1(I,J,K),YG(I,J,K),ZG(I,J,K),US(I,J,K),VS(I,J,K),WS(I,J,K) &
                          ,C(I,J,K),CM(I,J,K),USR(I,J,K),VSR(I,J,K),WSR(I,J,K),P(I,J,K),PSI(I,J,K) &
                          ,UT(I,K),WT(I,K),DEP(I,K),GL(I,K),ELES(I,K),ETA(I,K),HK(I,J,K),HH(I,K)
!                DEP1(I,K)=DEP(I,K)
!                ELES1(I,K)=ELES(I,K)
                CLOSE(35)
!
!            CALL EXTP
!
!            NMIN=NINT(NMIN/DTE)+1
!            TIME=FLOAT(NMIN/1000)
            N=NMIN
            CALL KAKI2
            NMIN=NMIN+1
            WRITE(*,*) 'NMIN=' ,NMIN
!            N=NMIN+1
!            TIME=(NMIN-1)*DT
!      CALL FILEOUT
!      CALL FILEOUT2D
!            CALL KAKI2
!
      RETURN
      END
!
!C*******************************************************************
       SUBROUTINE EXTP
!C*******************************************************************
       USE MF_SF
       USE OMP_LIB
      IMPLICIT NONE
      
!C
!C      PARAMETER(NX=200,NY=30,NZ=100)
!C              IMPLICIT NONE
!C      COMMON /COOR1/  GX(NX+4),GXX(NX+4)
!C      COMMON /COOR2/  EY(NY+4),EYY(NY+4)
!C      COMMON /COOR3/  DZ(NZ+4),DZZ(NZ+4)
!C      COMMON /COOR4/  X(NX+4,NY+4,NZ+4),Y(NX+4,NY+4,NZ+4)
!C      COMMON /COOR5/  Z(NX+4,NY+4,NZ+4)
!C      COMMON /UVW1/   U(NX+4,NY+4,NZ+4),U1(NX+4,NY+4,NZ+4)
!C      COMMON /UVW2/   V(NX+4,NY+4,NZ+4),V1(NX+4,NY+4,NZ+4)
!C      COMMON /UVW3/   W(NX+4,NY+4,NZ+4),W1(NX+4,NY+4,NZ+4)
!C      COMMON /UVW4/   US(NX+4,NY+4,NZ+4),US1(NX+4,NY+4,NZ+4)
!C      COMMON /UVW5/   VS(NX+4,NY+4,NZ+4),VS1(NX+4,NY+4,NZ+4)
!C      COMMON /UVW6/   WS(NX+4,NY+4,NZ+4),WS1(NX+4,NY+4,NZ+4)
!C      COMMON /UVW7/   U2(NX+4,NY+4,NZ+4),US2(NX+4,NY+4,NZ+4)
!C      COMMON /UVW8/   V2(NX+4,NY+4,NZ+4),VS2(NX+4,NY+4,NZ+4)
!C      COMMON /UVW9/   W2(NX+4,NY+4,NZ+4),WS2(NX+4,NY+4,NZ+4)
!C      COMMON /ENE1/   HK(NX+4,NY+4,NZ+4),HK1(NX+4,NY+4,NZ+4)
!C      COMMON /ENE2/   MYT(NX+4,NY+4,NZ+4),RE1(NX+4,NY+4,NZ+4)
!C      COMMON /ENE3/   DET(NX+4,NY+4,NZ+4),P(NX+4,NY+4,NZ+4)
!C      COMMON /DIS1/   C(NX+4,NY+4,NZ+4),C1(NX+4,NY+4,NZ+4)
!C      COMMON /DIS2/   USR(NX+4,NY+4,NZ+4),USR1(NX+4,NY+4,NZ+4)
!C      COMMON /DIS3/   VSR(NX+4,NY+4,NZ+4),VSR1(NX+4,NY+4,NZ+4)
!C      COMMON /DIS4/   WSR(NX+4,NY+4,NZ+4),WSR1(NX+4,NY+4,NZ+4)
!C      COMMON /DIS5/   UUSR(NX+4,NY+4,NZ+4),UUSR1(NX+4,NY+4,NZ+4)
!C      COMMON /DIG6/   VVSR(NX+4,NY+4,NZ+4),VVSR1(NX+4,NY+4,NZ+4)
!C      COMMON /DIG7/   WWSR(NX+4,NY+4,NZ+4),WWSR1(NX+4,NY+4,NZ+4)
!C      COMMON /REA1/   XM(NX+4,NY+4,NZ+4),YM(NX+4,NY+4,NZ+4)
!C      COMMON /REA2/   XG1(NX+4,NY+4,NZ+4),YG(NX+4,NY+4,NZ+4)
!C      COMMON /REA3/   ZG(NX+4,NY+4,NZ+4),ZM(NX+4,NY+4,NZ+4)
!C      COMMON /REA4/   CM(NX+4,NY+4,NZ+4)
!C      COMMON /DENS1/  ROU(NX+4,NY+4,NZ+4),ROUA(NX+4,NY+4,NZ+4)
!C      COMMON /DENS2/  SAL(NX+4,NY+4,NZ+4),SAL1(NX+4,NY+4,NZ+4)
!C      COMMON /DENS3/  ROU1(NX+4,NY+4,NZ+4),SALA(NX+4,NY+4,NZ+4)
!C      COMMON /UTSA/   USTA(NX+4),USTA1(NX+4)
!C      COMMON /VTSA/   VSTA(NX+4),VSTA1(NX+4)
!C      COMMON /USTA2/  USTAu(NX+4),USTAu1(NX+4)
!C      COMMON /VSTA2/  VSTAu(NX+4),VSTAu1(NX+4)
!C      COMMON /UTSA3/  APP(NX+4),APP1(NX+4),Q(NX+4),Q1(NX+4)
!C      COMMON /Pres/   P1(NX+4,NY+4,NZ+4),PSI(NX+4,NY+4,NZ+4)
!C      COMMON /SF1/    TEMP(NX+4,NY+4,NZ+4),TEMP1(NX+4,NY+4,NZ+4)
!C      COMMON /MESH  / IMAX,JMAX,KMAX
!C      COMMON /DTIME/ DT,TIME,NMAX,NWRT,N1,N
!C      COMMON /GOSA/  GOSAC,GOSAP,GOSAQ,GOSAR,GOSAE
!C      COMMON /CONST/ RED,AL,CONST1,CONST2,CONST3,EPS1,EPS2,EPS3,EPS4
!C      COMMON /LOP1 / ITC,ITC1
!C      COMMON /NAMI2/ naka
!C      COMMON /CONT2/  DR,RC,RS,GG,CL0,htp,CM0
!C
!--------------------------------------------------------------
!
                   DO 600 I=1,IMAX+4
                   DO 600 J=1,JMAX+3
                   DO 600 K=1,KMAX+4
!                   DO 600 K=1,KMAX+1
!
      U(I,J,K)=int(U(I,J,K)*1.0E6)/1.0E6
      V(I,J,K)=int(V(I,J,K)*1.0E6)/1.0E6
!      W(I,J,K)=int(W1(I,J,K)*1.0E6)/1.0E6
      W(I,J,K)=int(W(I,J,K)*1.0E6)/1.0E6      !23/05/20SUMO modified
!      W1(I,J,K)=W(I,J,K)
!
!CC      USTA(I)=int(USTA(I)*1.0E6)/1.0E6
!
      US(I,J,K)=int(US(I,J,K)*1.0E6)/1.0E6
      VS(I,J,K)=int(VS(I,J,K)*1.0E6)/1.0E6
      WS(I,J,K)=int(WS(I,J,K)*1.0E6)/1.0E6      !23/05/20SUMO added
      C(I,J,K)=int(C(I,J,K)*1.0E6)/1.0E6
!CC      MT(I,J,K)=int(MT(I,J,K)*1.0E6)/1.0E6
!      
!      HK(I,J,K)=int(HK(I,J,K)*1.0E6)/1.0E6
!      EK(I,J,K)=int(EK(I,J,K)*1.0E6)/1.0E6
!      MYT(I,J,K)=int(MYT(I,J,K)*1.0E6)/1.0E6
!
!      USR(I,J,K)=int(USR(I,J,K)*1.0E6)/1.0E6
!      VSR(I,J,K)=int(VSR(I,J,K)*1.0E6)/1.0E6
!      UUSR(I,J,K)=int(UUSR(I,J,K)*1.0E6)/1.0E6
!      VVSR(I,J,K)=int(VVSR(I,J,K)*1.0E6)/1.0E6
!
      APP1(I)=APP(I)
  600              CONTINUE
!
!
                   DO 500 I=1,IMAX+4
                   DO 500 J=1,JMAX+3
                   DO 500 K=1,KMAX+4
!
        U3(I,J,K)=U2(I,J,K)
      U2(I,J,K)=U1(I,J,K)
      V2(I,J,K)=V1(I,J,K)
      W2(I,J,K)=W1(I,J,K)      !23/05/20SUMO added
      US2(I,J,K)=US1(I,J,K)
      VS2(I,J,K)=VS1(I,J,K)
      WS2(I,J,K)=WS1(I,J,K)
!      UUSR2(I,J,K)=UUSR1(I,J,K)
!      VVSR2(I,J,K)=VVSR1(I,J,K)
!
      U1(I,J,K)=U(I,J,K)
      V1(I,J,K)=V(I,J,K)
      W1(I,J,K)=W(I,J,K)
!
      US1(I,J,K)=US(I,J,K)
      VS1(I,J,K)=VS(I,J,K)
      WS1(I,J,K)=WS(I,J,K)
      USR1(I,J,K)=USR(I,J,K)
      VSR1(I,J,K)=VSR(I,J,K)
      WSR1(I,J,K)=WSR(I,J,K)
      UUSR1(I,J,K)=UUSR(I,J,K)
      VVSR1(I,J,K)=VVSR(I,J,K)
      WWSR1(I,J,K)=WWSR(I,J,K)
      C1(I,J,K)=C(I,J,K)
      TEMP1(I,J,K)=TEMP(I,J,K)
!
      P1(I,J,K)=P(I,J,K)
!      PP1(I,J,K)=PP(I,J,K)
!      PPP1(I,J,K)=PPP(I,J,K)
      HK1(I,J,K)=HK(I,J,K)
!
      SAL1(I,J,K)=SAL(I,J,K)
      ROU1(I,J,K)=ROU(I,J,K)
!
      USTA1(I)=USTA(I)
      USTAu1(I)=USTAu(I)
      VSTA1(I)=VSTA(I)
      VSTAu1(I)=VSTAu(I)
      USTABRW1(I,J)=USTABRW(I,J)
        USTABLW1(I,J)=USTABLW(I,J)
        USTABRW_P1(I,J)=USTABRW_P(I,J)
            USTABLW_P1(I,J)=USTABLW_P(I,J)
            WSTABFW_P1(J,K)=WSTABFW_P(J,K)
            WSTABBW_P1(J,K)=WSTABBW_P(J,K)
  500              CONTINUE
!
                   DO 501 I=1,IMAX+4
                   DO 501 K=1,KMAX+4
!
                  GL1(I,K)=GL(I,K)      !SUMO23/03/27
                  DEP1(I,K)=DEP(I,K)      !SUMO23/03/27
                  ETA1(I,K)=ETA(I,K)      !SUMO23/03/27
                  ELES1(I,K)=ELES(I,K)
!
 501   CONTINUE
!
      RETURN
      END

      
!C**************************************************************
    SUBROUTINE KBC
!C**************************************************************

    USE MF_SF
    USE OMP_LIB
    IMPLICIT NONE

!//////////////////// CONTINUITY ////////////////////!

!********** DEPTH AND ETA INCORPORATED INTO GAL BY SUMO23/03/17 **********!

    DO 1048 I=2,IMAX+3
    DO 1048 K=2,KMAX+3   

        UT(I,K)=0.0
        WT(I,K)=0.0
        !Depth=0.0

        DO 1047 J=IGL(I,K)+1,IETA(I,K)   !SUMO23/03/24!

            !Depth=Depth+1.0/EY(J)

        !DO 1047 J=IGL(I,K),IETA(I,K)   !SUMO23/03/24

            UT(I,K)=UT(I,K)+U(I,J,K)/EY(J)
            WT(I,K)=WT(I,K)+W(I,J,K)/EY(J)

            !UT(I,K)=UT(I,K)+((1.0-C(I,J,K)/CSMAX)*U(I,J,K)+(C(I,J,K)/CSMAX)*US(I,J,K))/EY(J)
            !               !/DEP(I,K)
            !               !/FLOAT(IETA(I,K)-(IGL(I,K)+1))
            !WT(I,K)=WT(I,K)+((1.0-C(I,J,K)/CSMAX)*W(I,J,K)+(C(I,J,K)/CSMAX)*WS(I,J,K))/EY(J)
            !               !/DEP(I,K)
            !               !/FLOAT(IETA(I,K)-(IGL(I,K)+1))

 1047   CONTINUE

            !UT(I,K)=UT(I,K)/Depth
            !WT(I,K)=WT(I,K)/Depth
            UT(I,K)=UT(I,K)/DEP(I,K)
            WT(I,K)=WT(I,K)/DEP(I,K)

 1048   CONTINUE

        !DO 1050 K=1,KMAX+4

            !UT(1,K)=-UT(3,K)
            !UT(2,K)=0.0
            !UT(IMAX+2,K)=0.0
            !UT(IMAX+3,K)=-UT(IMAX+1,K)
            !UT(IMAX+4,K)=-UT(IMAX,K)

            !UT(1,K)=-UT(4,K)
            !UT(2,K)=-UT(3,K)
            !UT(IMAX+3,K)=-UT(IMAX+2,K)
            !UT(IMAX+4,K)=-UT(IMAX+1,K)

 1050   CONTINUE

        !DO 1051 I=1,IMAX+4

            !WT(I,1)=-WT(I,3)
            !WT(I,2)=0.0
            !WT(I,KMAX+2)=0.0
            !WT(I,KMAX+3)=-WT(I,KMAX+1)
            !WT(I,KMAX+4)=-WT(I,KMAX)
            
            !WT(I,1)=-WT(I,4)
            !WT(I,2)=-WT(I,3)
            !WT(I,KMAX+3)=-WT(I,KMAX+2)
            !WT(I,KMAX+4)=-WT(I,KMAX+1)

 1051   CONTINUE

!CCCCCC////// Pier Boundary Condition ////////CCCCCC

        IF(pier.EQ.1.0) THEN

            DO 9344 K=Kpier_R,Kpier_L
            DO 9344 I=Ipier_U,Ipier_D

                UT(I,K)=0.0
                WT(I,K)=0.0

                !UT(Ipier_U,K)=-UT(Ipier_U-1,K)
                !UT(Ipier_U+1,K)=-UT(Ipier_U-2,K)
                !UT(Ipier_D,K)=-UT(Ipier_D+1,K)
                !UT(Ipier_D-1,K)=-UT(Ipier_D+2,K)

                !WT(I,Kpier_R)=-WT(I,Kpier_R-1)
                !WT(I,Kpier_R+1)=-WT(I,Kpier_R-2)
                !WT(I,Kpier_L)=-WT(I,Kpier_L+1)
                !WT(I,Kpier_L-1)=-WT(I,Kpier_L+2)

 9344   CONTINUE

        END IF

!***************   POIS EQUATION  ****************************

        DO 4000 LOOP=1,10

            GOSAD=0.0

!*************** DEPTH AND ETA BOUNDARY CONDITION ***************!

        DO 799 I=1,2
        DO 799 K=1,KMAX+4

            !ELES(3,K)=0.0 !sumo250829
            !ELES(I,K)=0.0 !sumo250829

            !ELES(I,K)=ELES(3,K)

            !ELES(IMAX+2+I,K)=ELES(IMAX+2,K)
            !ELES(IMAX+2,KMAX+4)=0.0
            !ELES(IMAX+2+I,KMAX+4)=0.0
            !ELES(IMAX+2,KMAX+3)=0.0
            !ELES(IMAX+2+I,KMAX+3)=0.0
            !ELES(IMAX+2,KMAX+2)=0.0
            !ELES(IMAX+2+I,KMAX+2)=0.0

            !ELES(IMAX+2+I,K)=0.0

            ELES(IMAX+3,K)=ELES(3,K)
            ELES(IMAX+4,K)=ELES(4,K)
            ELES(1,K)=ELES(IMAX+1,K)
            ELES(2,K)=ELES(IMAX+2,K)

 799    CONTINUE

        DO 520 K=1,2
        DO 520 I=1,IMAX+4

            !ELES(I,K)=ELES(I,3)
            !ELES(I,KMAX+2+K)=ELES(I,KMAX+2)

            ELES(I,KMAX+3)=ELES(I,3)    !ITO CHANGE
            ELES(I,KMAX+4)=ELES(I,4)    !ITO CHANGE
            ELES(I,1)=ELES(I,KMAX+1)    !ITO CHANGE
            ELES(I,2)=ELES(I,KMAX+2)    !ITO CHANGE

 520    CONTINUE

!CCCCCC////// Pier Boundary Condition ////////CCCCCC

        !IF(pier.EQ.1.0) THEN

            !DO 7221 K=Kpier_R, Kpier_L
            !DO 7221 I=Ipier_U, Ipier_D

                !ELES(I,K)=ELES(I,Kpier_L+1)   
                !ELES(Ipier_D,K)=ELES(Ipier_D+1,K)
                !!ELES(I,Kpier_R)=ELES(I,Kpier_R-1)
                !!ELES(I,Kpier_L)=ELES(I,Kpier_L+1)
                !ELES(Ipier_U,K)=ELES(Ipier_U-1,K)

 7221   CONTINUE

        !END IF

!****************************************************************!

        DO 351 K=3,KMAX+2
        DO 351 I=3,IMAX+2

!********** COLLOCATE MESH BY SUMO 23/05/25 **********!

            DUDX(I,K)=((UT(I+1,K)+UT(I,K))*(ELES(I+1,K)+HH(I+1,K)+ELES(I,K)+HH(I,K))*0.25 &
                      -(UT(I,K)+UT(I-1,K))*(ELES(I,K)+HH(I,K)+ELES(I-1,K)+HH(I-1,K))*0.25)*GX(I)
            DWDZ(I,K)=((WT(I,K+1)+WT(I,K))*(ELES(I,K+1)+HH(I,K+1)+ELES(I,K)+HH(I,K))*0.25 &
                      -(WT(I,K)+WT(I,K-1))*(ELES(I,K)+HH(I,K)+ELES(I,K-1)+HH(I,K-1))*0.25)*DZ(K)

 351    CONTINUE

!$OMP PARALLEL
!$OMP DO
        DO 178 I=3,IMAX+2
        DO 178 K=Kpier_R,Kpier_L

            IF(pier.EQ.1.0) THEN
                IF(I.EQ.Ipier_U-1) THEN
                    UT(Ipier_U,K)=-UT(Ipier_U-1,K)
                    WT(Ipier_U,K)=-WT(Ipier_U-1,K)
                    ELES(Ipier_U,K)=ELES(Ipier_U-1,K)
                    DUDX(I,K)=((UT(I+1,K)+UT(I,K))*(ELES(I+1,K)+HH(I+1,K)+ELES(I,K)+HH(I,K))*0.25 &
                              -(UT(I,K)+UT(I-1,K))*(ELES(I,K)+HH(I,K)+ELES(I-1,K)+HH(I-1,K))*0.25)*GX(I)
                    DWDZ(I,K)=((WT(I,K+1)+WT(I,K))*(ELES(I,K+1)+HH(I,K+1)+ELES(I,K)+HH(I,K))*0.25 &
                              -(WT(I,K)+WT(I,K-1))*(ELES(I,K)+HH(I,K)+ELES(I,K-1)+HH(I,K-1))*0.25)*DZ(K)
                END IF
                IF(I.EQ.Ipier_D+1) THEN
                    UT(Ipier_D,K)=-UT(Ipier_D+1,K)
                    WT(Ipier_D,K)=-WT(Ipier_D+1,K)
                    ELES(Ipier_D,K)=ELES(Ipier_D+1,K)
                    DUDX(I,K)=((UT(I+1,K)+UT(I,K))*(ELES(I+1,K)+HH(I+1,K)+ELES(I,K)+HH(I,K))*0.25 &
                              -(UT(I,K)+UT(I-1,K))*(ELES(I,K)+HH(I,K)+ELES(I-1,K)+HH(I-1,K))*0.25)*GX(I)
                    DWDZ(I,K)=((WT(I,K+1)+WT(I,K))*(ELES(I,K+1)+HH(I,K+1)+ELES(I,K)+HH(I,K))*0.25 &
                              -(WT(I,K)+WT(I,K-1))*(ELES(I,K)+HH(I,K)+ELES(I,K-1)+HH(I,K-1))*0.25)*DZ(K)
                END IF
            END IF
 178    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL
!$OMP DO
        DO 179 K=3,KMAX+2
        DO 179 I=Ipier_U,Ipier_D

            IF(pier.EQ.1.0) THEN
                IF(K.EQ.Kpier_R-1) THEN
                    WT(I,Kpier_R)=-WT(I,Kpier_R-1)
                    UT(I,Kpier_R)=-UT(I,Kpier_R-1)
                    ELES(I,Kpier_R)=ELES(I,Kpier_R-1)
                    DUDX(I,K)=((UT(I+1,K)+UT(I,K))*(ELES(I+1,K)+HH(I+1,K)+ELES(I,K)+HH(I,K))*0.25 &
                              -(UT(I,K)+UT(I-1,K))*(ELES(I,K)+HH(I,K)+ELES(I-1,K)+HH(I-1,K))*0.25)*GX(I)
                    DWDZ(I,K)=((WT(I,K+1)+WT(I,K))*(ELES(I,K+1)+HH(I,K+1)+ELES(I,K)+HH(I,K))*0.25 &
                              -(WT(I,K)+WT(I,K-1))*(ELES(I,K)+HH(I,K)+ELES(I,K-1)+HH(I,K-1))*0.25)*DZ(K)
                END IF
                IF(K.EQ.Kpier_L+1) THEN
                    WT(I,Kpier_L)=-WT(I,Kpier_L+1)
                    UT(I,Kpier_L)=-UT(I,Kpier_L+1)
                    ELES(I,Kpier_L)=ELES(I,Kpier_L+1)
                    DUDX(I,K)=((UT(I+1,K)+UT(I,K))*(ELES(I+1,K)+HH(I+1,K)+ELES(I,K)+HH(I,K))*0.25 &
                              -(UT(I,K)+UT(I-1,K))*(ELES(I,K)+HH(I,K)+ELES(I-1,K)+HH(I-1,K))*0.25)*GX(I)
                    DWDZ(I,K)=((WT(I,K+1)+WT(I,K))*(ELES(I,K+1)+HH(I,K+1)+ELES(I,K)+HH(I,K))*0.25 &
                              -(WT(I,K)+WT(I,K-1))*(ELES(I,K)+HH(I,K)+ELES(I,K-1)+HH(I,K-1))*0.25)*DZ(K)
                END IF
            END IF

 179    CONTINUE
!$OMP END DO
!$OMP END PARALLEL

        DO 352 K=3,KMAX+2
        DO 352 I=3,IMAX+2

            !IF(TIME.LT.10.0) THEN
            !    SSD(I,K)=0.0
            !ELSE
                SSD(I,K)=ELES1(I,K)-DT*(DUDX(I,K)+DWDZ(I,K))-ELES(I,K)
                !SSD(3,K)=0.0 !sumo250829
                !SSD(IMAX+2,KMAX+2)=0.0
                !SSD(IMAX+2,K)=0.0
                !SSD(I,K)=0.0
            !END IF

            !SSD(I,K)=ELES1(I,K)-DT*(DUUDX(I,K)+DWWDZ(I,K)-V(I,IETA(I,K)-1,K))-ELES(I,K)

 352    CONTINUE

!CCCCCC////// Pier Boundary Condition ////////CCCCCC

        IF(pier.EQ.1.0) THEN

            DO 7043 K=Kpier_R,Kpier_L
            DO 7043 I=Ipier_U,Ipier_D

                SSD(I,K)=0.0

 7043   CONTINUE

        END IF

        DO 353 K=3,KMAX+2
        DO 353 I=3,IMAX+2

            GOSAD=GOSAD+ABS(SSD(I,K))

 353    CONTINUE

        DO 354 K=3,KMAX+2
        DO 354 I=3,IMAX+2

            ELES(I,K)=ELES(I,K)+SSD(I,K)

 354    CONTINUE

        IF(GOSAD.LT.0.01) GO TO 901

 4000   CONTINUE

 901    CONTINUE

        IF (naka.EQ.0.0) THEN
        WRITE(0,52) LOOP,GOSAD
 52     FORMAT (1X,'LOOP(D)=',I6,2X,'GOSAD=',F15.5)
        END IF

        DO 355 K=1,KMAX+4
        DO 355 I=1,IMAX+4

            DEP(I,K)=HH(I,K)+ELES(I,K)
            !IF(DEP(I,K).LT.0.0) DEP(I,K)=DEPC
            ETA(I,K)=DEP(I,K)+GL(I,K)

 355    CONTINUE

!CCCCCC////// Pier Boundary Condition ////////CCCCCC

        IF(pier.EQ.1.0) THEN

            DO 7225 K=Kpier_R,Kpier_L
            DO 7225 I=Ipier_U,Ipier_D
            DO 7225 J=2,JMAX+2

                ELES(Ipier_D,K)=ELES(Ipier_D+1,K)
                ELES(I,Kpier_R)=ELES(I,Kpier_R-1)
                ELES(I,Kpier_L)=ELES(I,Kpier_L+1)
                ELES(Ipier_U,K)=ELES(Ipier_U-1,K)

                ETA(Ipier_D,K)=ETA(Ipier_D+1,K)
                ETA(I,Kpier_R)=ETA(I,Kpier_R-1)
                ETA(I,Kpier_L)=ETA(I,Kpier_L+1)
                ETA(Ipier_U,K)=ETA(Ipier_U-1,K)

 7225   CONTINUE

        END IF

        DO 2055 K=1,KMAX+4
        DO 2055 I=1,IMAX+4
        DO 2055 J=3,JMAX+1

            IF(ETA(I,K).GT.Y(I,J-1,K)) THEN
                IF(ETA(I,K).LE.Y(I,J,K)) THEN
                    IF(ETA(I,K).GT.(Y(I,J,K)-0.50/EY(J))) THEN
                        IETA(I,K)=J
                        DETA(I,K)=ETA(I,K)-(Y(I,IETA(I,K),K)-0.50/EY(3)) !plus
                    ELSE
                        IETA(I,K)=J-1
                        DETA(I,K)=ETA(I,K)-(Y(I,IETA(I,K),K)-0.50/EY(3)) !minus
                    END IF
                END IF
            END IF

 2055   CONTINUE

!****************************************************************!

        !DO 5228 K=3,KMAX+2
        !DO 5228 I=3,IMAX+2

            !DUUDX(I,K)=U(I,IETA(I,K),K)*(ELES(I+1,K)-ELES(I-1,K))*0.50*GX(I) &
            !        -ABS(U(I,IETA(I,K),K))*(ELES(I+1,K)-2.0*ELES(I,K)+ELES(I-1,K))*0.50*GX(I)
            !DWWDZ(I,K)=W(I,IETA(I,K),K)*(ELES(I,K+1)-ELES(I,K-1))*0.50*DZ(K) &
            !        -ABS(W(I,IETA(I,K),K))*(ELES(I,K+1)-2.0*ELES(I,K)+ELES(I,K-1))*0.50*DZ(K)

            !V(I,IETA(I,K),K)=(ELES(I,K)-ELES1(I,K))/DT+DUUDX(I,K)+DWWDZ(I,K)

 5228   CONTINUE

    RETURN
    END
