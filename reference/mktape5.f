      program mktape5

      logical modtran,dis,sun1

      OPEN (7,FILE='tape5.src',
     $     STATUS='OLD') 

c     card 1
      read(7,1) modtran, MODEL,ITYPE,IEMSCT,IMULT,M1,M2,M3,
     $      M4,M5,M6,MDEF,IM,NOPRT,TBOUND,SALB  
 1    format(L1,I4,12I5,F8.3,F7.2)

c     NORTH supplimentary card
      read(7,21) dis,nstr,sun1,isun,
     $     co2mx,ch4mx,h2orat
 21   format(2(l1,i4),7f10.3)

c     card 2
      read(7,2) IHAZE,ISEASN,IVULCN,ICSTL,
     $     ICLD,IVSA,VIS,WSS,WHH,RAINRT,
     $     GNDALT 
 2    format(6I5,5F10.3)   

c     card 3
      READ(7,3)H1,H2,ANGLE,RANGE,BETA,RO,LEN 
 3    FORMAT(6F10.3,I5)

c     card 4
      READ(7,4)IV1,IV2,IDV,IFWHM
 4    format(4I10)

c     card 5
      read(7,5) irpt
 5    format(i5)

c     get altered information from stdin
      
      read(5,*) co2mx,ch4mx,tozonemx,sozonerat,
     $     toffset,h2otscaled,
     $     h2orat,model,icld,h1,angle

      if(h1 .EQ. 0) h1=h1+.001
c      if(imoon .EQ. 1) iemsct = 3

c     card 1
      write(6,1) modtran, MODEL,ITYPE,IEMSCT,IMULT,M1,M2,M3,
     $      M4,M5,M6,MDEF,IM,NOPRT,TBOUND,SALB  

c     card 2 North
      write(6,21) dis,nstr,sun1,isun,
     $     co2mx,ch4mx,tozonemx,sozonerat,toffset,h2otscaled,h2orat

c     card 2 proper
      write(6,2) IHAZE,ISEASN,IVULCN,ICSTL,
     $     ICLD,IVSA,VIS,WSS,WHH,RAINRT,
     $     GNDALT

c     card 2a cirrus input
      if( icld .GE. 18 ) then
         write (6,22) 0,0,0,0
      endif
 22   format(4I10) 

c     card 3
c      if (imoon .EQ. 0) then
         write(6,3)H1,H2,ANGLE,RANGE,BETA,RO,LEN 
c      else
c      if( imoon .EQ. 1) then
c         write(6,33)H1,H2,ANGLE,90,0,0,0 
c 33      FORMAT(3F10.3,I5,5X,F10.3,I5,F10.3)
c          if( model .EQ. 1) then
c            lat = 0
c         elseif( model .EQ. 2 .OR. model .EQ. 3) then
c            lat = 30
c         else
c            lat = 60
c         endif
cc        card 3a1
c         iparm = 0
c         iday = 90
c         isourc = 1
c         write(6,31) iparm, 2, iday, isourc
c 31      format(4i5)
c        card 3a2
c         parm1 = 0.
c         parm2 = 0.
c         parm3 = 0.
c         parm4 = 0.
c         time = 0.
c         psipo = 0.
c         anglem = 180.
c         g=0
c         write(6,32) parm1, parm2, parm3, parm4, time,
c     $        psipo, anglem, g
c 32      format(8f10.3)
c      endif

c     card 4
      write(6,4)IV1,IV2,IDV,IFWHM

c     card 5
      write(6,5)irpt

      end
