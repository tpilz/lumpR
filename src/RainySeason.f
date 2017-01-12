C lumpR/RainySeason.f
C Copyright (C) 1999 Friedrich-Wilhelm Gerstengarbe, Peter C. Werner
C Copyright (C) 2000-2004 Andreas Güntner
C Copyright (C) 2015 Tobias Pilz
C 
C This program is free software: you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published by
C the Free Software Foundation, either version 3 of the License, or
C (at your option) any later version.
C 
C This program is distributed in the hope that it will be useful,
C but WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C GNU General Public License for more details.
C 
C You should have received a copy of the GNU General Public License
C along with this program.  If not, see <http://www.gnu.org/licenses/>.



C  Last change Feb. 2015 by Tobias Pilz: wrapping souroutine around main program for use in R
C  Code needs updating !!! Currently old Fortran 77, implicit declarations, no comments, lots of GOTO etc.
C
C  Model to calculate the start and end day date of the rainy season
C  based on a statistical approach as described in Gerstengarbe & Werner (1999):
C  'Estimation of the beginning and end of recurrent events within a climate regime',
C   Climate Research, 11(2), 97-107
C  (Potsdam-Institute of Climate Impact Reserach, Potsdam, Germany)
C
C  The core of the program was written by Gerstengarbe et al. 
C
C  The programme was principally applied for daily precipitation time series
C  of semi-arid North-eastern Brazil
C  by Andreas Güntner  (Güntner, 2002; Güntner & Bronstert, 2004)
C
C  Required input data:
C  file with daily rainfall time series (see example precip_Pirangi.dat)
C  and some specifications in the first lines of the data file.
C  it is advisable to start the data file at January 01 (other options not tested!)
C
C  Note 1: for statistics of rainy seasons in Ceara, it resulted to be reasonable to set
C  the parameter lw (line 5 if input file) to about 243
C  (this is the starting point of the stastical procedure to look for the next 
C   rainy season in the data set. lw should be set approximately (+- 1 month)
C   to the centre of the dry season. For example,
C   lw=243 corresponds to the 243th day of the year, which is beginning of September)
C
C  Note 2: for statistical reasons resulting from Note 1, the first rainy season that can be
C  identified by the model is the one that starts after the first dry season in the given time series
C  (this means for Ceara that if the data file starts on 01 01 1978 and lw is set to 243,
C   the first detectable rainy season is the one starting around Dec78-Feb79;
C  (to get also the rainy season for year 1978 for this example, 365 daily values can be artifically 
C   inserted at the beginning of the file, representing the hypothetical year 1977
C   (just copy the data of 1978, and change the total number of days in the file accordingly (line 4)))

      subroutine rainyseason(h, day, month, year, n, lw, mima, xe,
     +outyear, outbegin, outend)
     
      integer day, month, year, n, lw, mima, outi, ida(3)
      integer outyear(300), outbegin(300), outend(300)
      double precision xe, h(80000)
      
      dimension hp(700,2),hpsa(300),hmpa(300)
      dimension c(80000),g(300),bm(700),hpse(300),hmpe(300),hpl(300)
      dimension b(80000)

      ida(1)=day
      ida(2)=month
      ida(3)=year
      outi=1

c      print *,'day, month, year, ida: ', day, month, year, ida
c      print *,'n, lw, mima, xe: ', n, lw, mima, xe
c      print *,'outyear ', outyear
c      print *,'outbegin ', outbegin
c      print *,'outend ', outend


      nv=365
      ug=-5000.1
      og=5000.0
      nfd=0
      

C  ----------------------------------------------------
C  Start core of program written by Gerstengarbe et al.  
      n=n-lw
      do 15 i=1,n
      h(i)=h(i+lw)
   15 continue
      ija=ida(3)
c   AUSBLENDUNG DES SCHALTTAGES
      ns=1
      if(ida(1).eq.0) goto 50
      iz=0
      do 1 i=1,n
      if(ida(1).eq.29.and.ida(2).eq.2) goto 51
      iz=iz+1
      h(iz)=h(i)
   51 continue
      call dafo(ida,ns)
    1 continue
      n=iz
   50 continue
c   MITTELWERTBERECHNUNG
      m=n/nv
      do 2 i=1,nv
      bm(i)=0.0
      iz=0
      do 3 i1=1,m
      if(h(i+(i1-1)*nv).lt.ug.or.h(i+(i1-1)*nv).gt.og) goto 3
      iz=iz+1
      bm(i)=bm(i)+h(i+(i1-1)*nv)
    3 continue
      bm(i)=bm(i)/iz
    2 continue
      if(xe.le.-999.) goto 59
      do 4 i=1,nv
      do 4 i1=1,m
      if(h(i+(i1-1)*nv).lt.ug.or.h(i+(i1-1)*nv).gt.og) h(i+(i1-1)*nv)
     +=bm(i)
    4 continue
      goto 60
   59 continue
      do 14 i=1,nv
      do 14 i1=1,m
      if(h(i+(i1-1)*nv).lt.ug.or.h(i+(i1-1)*nv).gt.og) h(i+(i1-1)*nv)=xe
   14 continue
   60 continue
c   GLAETTUNG MIT DREIECKSFILTER
      if(nfd.eq.0) goto 7788
      do 90 i=1,nv
      do 91 i1=1,m
      c(i1)=h(i+(i1-1)*nv)
   91 continue
      nnf=m+nfd-1
      call drf(m,nfd,nnf,g,c,b)
      do 92 i1=1,m
      h(i+(i1-1)*nv)=c(i1)
   92 continue
   90 continue
 7788 continue
c   BESTIMMUNG DES STARTPUNKTES (MIN/MAX)
      nf=nv*0.2
      af=nf+0.5
      nf2=af
      df=af-nf2
      if(df.eq.0) nf=nf+1
      nnf=nv+nf
c      write(15,204) (bm(i),i=1,nv)
      call drf(nv,nf,nnf,g,bm,c)
c      write(16,2044) (bm(i),i=1,nv)
 2044 format(f7.2)     
      do 5 i=1,nv
      c(i)=bm(i)
    5 continue
      call sort(nv,c)
      xm=c(1)
      if(mima.gt.0) xm=c(nv)
      do 6 i=1,nv
      if(bm(i).eq.xm) goto 52
    6 continue
   52 continue
      nsp=i
      lsa=nsp
      lse=nv-nsp+1
C      print 111,nsp
C  111 format('ANFANGSPUNKT (MIN/MAX):',i8)
c   BERECHNUNG VON PERIODENANFANG UND -ENDE FUER M INTERVALLE
      na=1
      ne=lsa
      nw=10
      nlta=lse*0.5
      nlte=lsa*0.5
      ntw=nsp-(nsp*0.01)
      do 7 i=1,m
      hmpa(i)=0.0
      na1=na
   54 continue
      iz=0
      do 8 i1=na1,ne
      iz=iz+1
      bm(iz)=h(i1)
    8 continue
      call proan(bm,iz,hp,hs,hma,dhp)
      if(hs.eq.0.0) goto 63
      s=hma*100.0
      ihs=s
      hma=ihs*0.01
      if(mima.lt.0.and.dhp.lt.0.0) goto 62
      if(mima.gt.0.and.dhp.gt.0.0) goto 62
      apsa=hs+na1-na
      if(apsa.le.0.0) goto 62
      if(hma.gt.hmpa(i)) hpsa(i)=hs+na1-na
      if(hma.gt.hmpa(i)) hmpa(i)=hma
   62 continue
      if(i.eq.1) goto 53
      na1=na1-nw
      nd=na-na1
      if(nd.le.nlta) goto 54
   53 continue
      if(hpsa(i).lt.ntw) goto 63
      hmpa(i)=0.0
      na1=na
   64 continue
      iz=0
      do 18 i1=na1,ne
      iz=iz+1
      bm(iz)=h(i1)
   18 continue
      call proan(bm,iz,hp,hs,hma,dhp)
      if(hs.eq.0.0) goto 63
      if(mima.lt.0.and.dhp.lt.0.0) goto 72
      if(mima.gt.0.and.dhp.gt.0.0) goto 72
      apsa=hs+na1-na
      if(apsa.le.0.0) goto 72
      hpsa(i)=hs+na1-na
      hmpa(i)=hma
      if(hpsa(i).lt.ntw) goto 63
   72 continue
      na1=na1+nw
      if(na1.ge.ne) goto 63
      goto 64
   63 continue
      na=na+nv
      ne=ne+nv
    7 continue
      i=i-1
c      print 112,i
c  112 format(1x,'ANFANGSPUNKT FUER',i4,2x,'ZEITSCHRITTE BERECHNET',/)
      na=nsp
      ne=nv
      ntw=nv+nlte-(nsp*0.01)
      do 9 i=1,m
      hmpe(i)=0.0
      ne1=ne
   55 continue
      iz=0
      do 10 i1=na,ne1
      iz=iz+1
      bm(iz)=h(i1)
   10 continue
      call proan(bm,iz,hp,hs,hma,dhp)
      if(hs.eq.0.0) goto 73
      s=hma*100.0
      ihs=s
      hma=ihs*0.01
      if(mima.lt.0.and.dhp.gt.0.0) goto 61
      if(mima.gt.0.and.dhp.lt.0.0) goto 61
      if(hma.gt.hmpe(i)) hpse(i)=hs+nsp-1
      if(hma.gt.hmpe(i)) hmpe(i)=hma
   61 continue
      if(i.eq.m) goto 56
      ne1=ne1+nw
      nd=ne1-ne
      if(nd.le.nlte) goto 55
   56 continue
      if(hpse(i).lt.ntw) goto 73
      hmpe(i)=0.0
      ne1=ne
   74 continue
      iz=0
      do 28 i1=na,ne1
      iz=iz+1
      bm(iz)=h(i1)
   28 continue
      call proan(bm,iz,hp,hs,hma,dhp)
      if(hs.eq.0.0) goto 73
      if(mima.lt.0.and.dhp.gt.0.0) goto 81
      if(mima.gt.0.and.dhp.lt.0.0) goto 81
      hpse(i)=hs+nsp-1
      hmpe(i)=hma
      if(hpse(i).lt.ntw) goto 73
   81 continue
      ne1=ne1-nw
      if(ne1.le.na) goto 73
      goto 74
   73 continue
      na=na+nv
      ne=ne+nv
    9 continue
      i=i-1
c      print 113,i
c  113 format(1x,'ENDPUNKT FUER',i4,2x,'ZEITSCHRITTE BERECHNET',/)
      na=1
      ne=m
      if(hpsa(1).eq.0.0) na=2
      if(hpse(m).eq.0.0) ne=m-1
      hpsam=0.0
      hpsem=0.0
      hplm=0.0
      ka=0
      ke=0
      do 11 i=na,ne
      if(hmpa(i).eq.0.0) goto 1122
      hpsam=hpsam+hpsa(i)
      ka=ka+1
 1122 continue
      if(hmpe(i).eq.0.0) goto 11
      hpsem=hpsem+hpse(i)
      ke=ke+1
   11 continue
      k=ne-na+1
      hpsam=hpsam/ka
      hpsem=hpsem/ke
      do 1111 i=na,ne
      if(hmpa(i).eq.0.0) hpsa(i)=hpsam
      if(hmpe(i).eq.0.0) hpse(i)=hpsem
      hpl(i)=hpse(i)-hpsa(i)
 1111 continue
      hpsam=0.0
      hpsem=0.0
      hplm=0.0
      do 2222 i=na,ne
      hpsam=hpsam+hpsa(i)
      hpsem=hpsem+hpse(i)
      hplm=hplm+hpl(i)
 2222 continue
      hpsam=hpsam/k
      hpsem=hpsem/k
      hplm=hplm/k
c      write(11,200) ite
c      write(11,2000) lw
c      if(ija.ne.0) write(11,201)
c      if(ija.eq.0) write(11,202)
      if(ija.eq.0) goto 57
      do 12 i=na,ne
      lja=ija+i-1
      ihpsa=hpsa(i)
      ihpse=hpse(i)
      ihpl=hpl(i)
      ihpsa=ihpsa+lw-365
      ihpse=ihpse+lw-365
      if (ihpsa .lt. 0) then 
       ihpsa=365+ihpsa
      endif 
      if (ihpse .lt. 0) then 
       ihpse=365+ihpse
      endif 
c tp write into output vector instead of writing into file
c      write(11,211) ija+i,ihpsa,ihpse
      outyear(outi)=ija+i
      outbegin(outi)=ihpsa
      outend(outi)=ihpse
      outi=outi+1
   12 continue
      goto 58
   57 continue
      do 13 i=na,ne
      ihpsa=hpsa(i)
      ihpse=hpse(i)
      ihpl=hpl(i)
      ihpsa=ihpsa+lw-365
      ihpse=ihpse+lw-365
      if (ihpsa .lt. 0) then 
       ihpsa=365+ihpsa
      endif 
      if (ihpse .lt. 0) then 
       ihpse=365+ihpse
      endif 
c tp write into output vector instead of writing into file
c      write(11,211) ihpsa,ihpse
      outbegin(outi)=ihpsa
      outend(outi)=ihpse
      outi=outi+1
   13 continue
   58 continue
      hpsam=hpsam+lw-365.
      hpsem=hpsem+lw-365.
      if (hpsam .lt. 0.) then 
       hpsam=365.+hpsam
      endif 
      if (hpsem .lt. 0.) then 
       hpsem=365.+hpsem
      endif 
c      write(12,'(3f6.0)') hpsam,hpsem,hplm
c      write(11,206)
c      if(xe.ne.-999.9) write(11,207) xe
c      if(xe.eq.-999.9) write(11,208)
c      write(11,209)
c      write(12,204) (hpsa(i),i=na,ne)
c      write(13,204) (hpse(i),i=na,ne)
c      write(14,204) (hpl(i),i=na,ne)
      nfd2=2*nfd
c      if(nfd.ne.0) write(11,210) nfd2
  100 format(a60)
  101 format(10f6.1)
  102 format(a32)
  200 format(1x,'ZEITLICHE DARSTELLUNG VON PERIODEN FUER:'//,1x,a60,/)
 2000 format(1x,'VERSCHIEBUNG DES STARTPUNKTES:'/,i5,/)
  201 format(1x,'JAHR',4x,'ANFANG',2x,'TESTWERT',5x,'  ENDE',2x,
     +'TESTWERT',5x,'LAENGE',/)
  202 format(1x,' NR.',4x,'ANFANG',2x,'TESTWERT',5x,'  ENDE',2x,
     +'TESTWERT',5x,'LAENGE',/)
  203 format(1x,i4,4x,i6,2x,f8.1,5x,i6,2x,f8.1,5x,i6)
  204 format(f6.1)
  205 format(//,1x,'MITTEL',2x,f6.1,2(15x,f6.1))
  206 format(///,1x,'ANMERKUNG:',/)
  207 format(/,1x,'1. AUSFALLWERTE WURDEN DURCH ',f6.1,2x,'ERSETZT.')
  208 format(/,1x,'1. AUSFALLWERTE WURDEN DURCH DEN MITTELWERT ERSETZT.'
     +)
  209 format(/,1x,'2. WENN TESTWERT = .0 ,DANN WURDE FUER DEN ANFANG BZW
     +. DAS ENDE',/,4x,'DER MITTELWERT AUS DEN WERTEN MIT TESTWERT > .0
     +EINGESETZT.')
  210 format(/,1x,'3. DIE AUSGANGSREIHE WURDE UEBER',i4,2x,'JAHRE MIT EI
     +NEM',/,4x,'DREIECKSFILTER GEGLAETTET.')
  211 format(3i5)
c      close(11)
c      close(12)
c      close(13)
c      close(14)

c      close(11)
c      close(12)
      end
c -------------------------------------------------------------------
c Ende Hauptprogramm
      end


      subroutine proan (h,n,hp,hs,hma,dhp)
      dimension h(n),hp(n,2),h1(500),hr(500)
      call makepa(h,h1,hr,hp,n,alpha,kode2,uta)
      if(hp(1,1).gt.hp(1,2)) goto 51
      do 1 i=1,n
      if(hp(i,1).ge.hp(i,2)) goto 52
    1 continue
   52 continue
      hs=i
      goto 53
   51 continue
      do 2 i=1,n
      if(hp(i,1).le.hp(i,2)) goto 54
    2 continue
   54 continue
      hs=i
   53 continue
      if(i.ne.n) goto 55
      hs=0.0
   55 continue
      hma=0.0
      do 5 i=1,n
      p1=abs(hp(i,1))
      p2=abs(hp(i,2))
      if(p1.gt.hma) hma=p1
      if(p2.gt.hma) hma=p2
    5 continue
      is=hs
      if(is.le.1) is=2
      dhp=hp(is-1,1)-hp(is-1,2)
      return
      end
      SUBROUTINE DAFO(IDA,NS)                                           SZE01710
      DIMENSION IDA(3),ML(12)                                           SZE01720
      DATA ML/31,28,31,30,31,30,31,31,30,31,30,31/                      SZE01730
      IF(NS.EQ.0) RETURN                                                SZE01740
      IT=IDA(1)+NS                                                      SZE01750
      SDA=FLOAT(IDA(3))                                                 SZE01760
      A=SDA*0.25                                                        SZE01770
      IA=INT(A)                                                         SZE01780
      D=A-IA                                                            SZE01790
      IF(D.LT.0.25) ML(2)=29                                            SZE01800
      IF(IDA(3).EQ.1900) ML(2)=28                                       SZE01810
      IF(IDA(3).EQ.1800) ML(2)=28                                       SZE01820
      IJ=IDA(3)                                                         SZE01830
      IH=IDA(2)                                                         SZE01840
   10 CONTINUE                                                          SZE01850
      IM=ML(IH)                                                         SZE01860
      IF(IT.LE.IM) GOTO 50                                              SZE01870
      ML(2)=28                                                          SZE01880
      IT=IT-IM                                                          SZE01890
      IH=IH+1                                                           SZE01900
      IF(IH.LE.12) GOTO 11                                              SZE01910
      IH=1                                                              SZE01920
      IJ=IJ+1                                                           SZE01930
      SJ=FLOAT(IJ)                                                      SZE01940
      A=SJ*0.25                                                         SZE01950
      IA=INT(A)                                                         SZE01960
      D=A-IA                                                            SZE01970
      IF(D.LT.0.25) ML(2)=29                                            SZE01980
      IF(IDA(3).EQ.1900) ML(2)=28                                       SZE01990
      IF(IDA(3).EQ.1800) ML(2)=28                                       SZE02000
   11 CONTINUE                                                          SZE02010
      IF(IT.GT.ML(IH)) GOTO 10                                          SZE02020
      IDA(2)=IH                                                         SZE02030
      IDA(3)=IJ                                                         SZE02040
   50 CONTINUE                                                          SZE02050
      IDA(1)=IT                                                         SZE02060
      RETURN                                                            SZE02070
      END                                                               SZE02080
      subroutine drf(n,nf,nnf,g,a,b)
      dimension a(n),b(nnf),g(nf)
      x=4.0/((nf+1)**2)
      nf1=(nf-1)/2
      nf2=(nf+1)/2
      j=nf1+1
      do 1 i=1,nf1
      xi=i*x
      g(i)=xi
      g(nf-i+1)=xi
    1 continue
      g(j)=j*x
      do 2 i=1,nf1
      b(i)=a(nf1-i+1)
    2 continue
      iz=0
      do 3 i=nf1+n+1,n+2*nf1
      iz=iz+1
      b(i)=a(nf1-iz+1)
    3 continue
      iz=0
      do 4 i=j,nf1+n
      iz=iz+1
      b(i)=a(iz)
    4 continue
      iz=0
      do 5 i=j,nf1+n
      iz=iz+1
      a(iz)=0.0
      jz=0
      do 5 i1=i-nf1,i+nf1
      jz=jz+1
      a(iz)=a(iz)+b(i1)*g(jz)
    5 continue
      return
      end
      FUNCTION FNKSF(MODE,X,N1,N2,KODE)                                 FNK00010
C     BERECHNUNG DER WAHRSCHEINLICHKEITEN P(X<X1)=ALPHA                 FNK00020
C     NACH R. CEHESSAT (1976)                                           FNK00030
C     MODE=1: STANDART-NORMALVERTEILUNG                                 FNK00040
C     MODE=2: CHI**2-VERTEILUNG VOM FREIHEITSGRAD N1                    FNK00050
C     MODE=3: STUDENT-VERTEILUNG VOM FREIHEITSGRAD N1                   FNK00060
C     MODE=4: F-VERTEILUNG MIT DEN FREIHEITSGRADEN N1,N2                FNK00070
C     KODE=0: PROGRAMMABLAUF NORMAL                                     FNK00080
C     KODE=1: FALSCHER WERT FUER MODE                                   FNK00090
C     KODE=2: X ODER N1 FUER CHI**2 NICHT DEFINIERT                     FNK00100
C     KODE=3: N1 FUER DIE STUDENT-VERTEILUNG NICHT DEFINIERT            FNK00110
C     KODE=4: X,N1,N2 FUER F NICHT DEFINIERT: FNKSF=9999.9              FNK00120
      DIMENSION G(5)                                                    FNK00130
      KODE=0                                                            FNK00140
  250 CONTINUE                                                          FNK00150
      G(1)=0.31938153                                                   FNK00160
      G(2)=-0.356563782                                                 FNK00170
      G(3)=1.781477937                                                  FNK00180
      G(4)=-1.821255978                                                 FNK00190
      G(5)=1.330274429                                                  FNK00200
      FN1=N1                                                            FNK00210
      FN2=N2                                                            FNK00220
      FNKSF=9999.9                                                      FNK00230
      M1=MODE-1                                                         FNK00240
      M2=MODE-2                                                         FNK00250
      M3=MODE-3                                                         FNK00260
      M4=MODE-4                                                         FNK00270
      M=M1*M2*M3*M4                                                     FNK00280
      IF(M) 103,200,103                                                 FNK00290
  200 CONTINUE                                                          FNK00300
      XO=0.0001                                                         FNK00310
      ABSX=ABS(X)                                                       FNK00320
      IF(ABSX-XO) 60,201,201                                            FNK00330
  201 CONTINUE                                                          FNK00340
      GOTO (1,2,3,4),MODE                                               FNK00350
    1 Z=X                                                               FNK00360
      GOTO 40                                                           FNK00370
    2 IF(X) 102,202,202                                                 FNK00380
  202 CONTINUE                                                          FNK00390
      IF(N1) 102,102,203                                                FNK00400
  203 CONTINUE                                                          FNK00410
      FISH=X/FN1                                                        FNK00420
      FN2=1.E10                                                         FNK00430
      GOTO 10                                                           FNK00440
    3 IF(N1) 101,101,204                                                FNK00450
  204 CONTINUE                                                          FNK00460
      FISH=X**2                                                         FNK00470
      FN2=FN1                                                           FNK00480
      FN1=1.0                                                           FNK00490
      GOTO 10                                                           FNK00500
    4 IF(X) 100,205,205                                                 FNK00510
  205 CONTINUE                                                          FNK00520
      IF(N1) 100,100,206                                                FNK00530
  206 CONTINUE                                                          FNK00540
      IF(N2) 100,100,207                                                FNK00550
  207 CONTINUE                                                          FNK00560
      FISH=X                                                            FNK00570
   10 F=FISH                                                            FNK00580
      IF(FISH-1.0) 208,20,20                                            FNK00590
  208 CONTINUE                                                          FNK00600
      F=1.0/FISH                                                        FNK00610
      U=FN1                                                             FNK00620
      FN1=FN2                                                           FNK00630
      FN2=U                                                             FNK00640
   20 A1=2.0/(9.0*FN1)                                                  FNK00650
      A2=2.0/(9.0*FN2)                                                  FNK00660
      EX=1./3.                                                          FNK00670
      Z=(1.0-A2)*(F**EX)-(1.0-A1)                                       FNK00680
      EX=2./3.                                                          FNK00690
      Z=Z/(A1+A2*F**EX)**0.5                                            FNK00700
      IF(FN2-3.0) 210,210,209                                           FNK00710
  210 Z=Z*(1.0+0.08*(Z**4)/FN2**3)                                      FNK00720
  209 CONTINUE                                                          FNK00730
      ABSZ=ABS(Z)                                                       FNK00740
      IF(ABSZ-7.0) 40,40,211                                            FNK00750
  211 CONTINUE                                                          FNK00760
      IF(Z-7.0) 212,212,30                                              FNK00770
  212 CONTINUE                                                          FNK00780
      FNKSF=0.0                                                         FNK00790
      RETURN                                                            FNK00800
   30 FNKSF=1.0                                                         FNK00810
      RETURN                                                            FNK00820
   40 CONTINUE                                                          FNK00830
      V=Z*Z*(-0.5)                                                      FNK00840
      V=EXP(V)*0.398942                                                 FNK00850
      Q=1.0/(1.0+0.2316419*ABS(Z))                                      FNK00860
      SOM=0.                                                            FNK00870
      DO 50 I=1,5                                                       FNK00880
      SOM=SOM+G(I)*Q**I                                                 FNK00890
   50 CONTINUE                                                          FNK00900
      FNKSF=0.5+SIGN(1.,Z)*(0.5-V*SOM)                                  FNK00910
      IF(MODE-1) 213,214,213                                            FNK00920
  214 RETURN                                                            FNK00930
  213 CONTINUE                                                          FNK00940
      IF(FISH-1.0) 215,216,216                                          FNK00950
  215 FNKSF=1.0-FNKSF                                                   FNK00960
  216 CONTINUE                                                          FNK00970
      IF(MODE-3) 217,218,217                                            FNK00980
  217 RETURN                                                            FNK00990
  218 CONTINUE                                                          FNK01000
      FNKSF=FNKSF+3.5*(1.0-FNKSF)                                       FNK01010
      IF(X) 219,219,220                                                 FNK01020
  219 FNKSF=1.0-FNKSF                                                   FNK01030
  220 CONTINUE                                                          FNK01040
      RETURN                                                            FNK01050
   60 GOTO (70,80,70,80),MODE                                           FNK01060
   70 FNKSF=0.5                                                         FNK01070
      RETURN                                                            FNK01080
   80 FNKSF=0.0                                                         FNK01090
      RETURN                                                            FNK01100
  100 KODE=KODE+1                                                       FNK01110
  101 KODE=KODE+1                                                       FNK01120
  102 KODE=KODE+1                                                       FNK01130
  103 KODE=KODE+1                                                       FNK01140
      RETURN                                                            FNK01150
      END                                                               FNK01160
      SUBROUTINE SORT(L,H)                                              SOR00010
      DIMENSION H(L),H1(3000)                                           SOR00020
      A=1.0E+06                                                         SOR00030
      DO 1 I=1,L                                                        SOR00040
      B=A                                                               SOR00050
      DO 2 I1=1,L                                                       SOR00060
      IF(H(I1).GT.B) GOTO 2                                             SOR00070
      B=H(I1)                                                           SOR00080
      IX=I1                                                             SOR00090
    2 CONTINUE                                                          SOR00100
      H(IX)=A                                                           SOR00110
      H1(I)=B                                                           SOR00120
    1 CONTINUE                                                          SOR00130
      DO 3 I=1,L                                                        SOR00140
      H(I)=H1(I)                                                        SOR00150
    3 CONTINUE                                                          SOR00160
      RETURN                                                            SOR00170
      END                                                               SOR00180
      SUBROUTINE MAKEPA(HF,H1,HR,HS,N,ALPHA,KODE2,UTA)                  MAK00010
C     TENDENZTEXT NACH MANN-KENDALL UND                                 MAK00020
C     PROGRESSIVE ANALYSE                                               MAK00030
      DIMENSION HF(N),H1(N),HR(N),HS(N,2)                               MAK00040
      CALL RAKRO(N,HF,H1,HR)                                            MAK00050
      M=1                                                               MAK00060
      MO=1                                                              MAK00070
      HS(1,1)=0.                                                        MAK00080
      J1=1                                                              MAK00090
      GOTO 20                                                           MAK00100
   10 CONTINUE                                                          MAK00110
      HS(N,2)=0.                                                        MAK00120
      J2=N                                                              MAK00130
   20 CONTINUE                                                          MAK00140
      T=0.                                                              MAK00150
      N1=N-1                                                            MAK00160
      DO 1 I=1,N1                                                       MAK00170
      GOTO(30,31),M                                                     MAK00180
   30 CONTINUE                                                          MAK00190
      L=I+1                                                             MAK00200
      J2=L-1                                                            MAK00210
      GOTO 40                                                           MAK00220
   31 CONTINUE                                                          MAK00230
      L=N-I                                                             MAK00240
      J1=L+1                                                            MAK00250
   40 CONTINUE                                                          MAK00260
      AN=0.                                                             MAK00270
      DO 2 J=J1,J2                                                      MAK00280
      P=HR(J)-HR(L)                                                     MAK00290
      IF(P) 50,51,2                                                     MAK00300
   50 CONTINUE                                                          MAK00310
      AN=AN+1.                                                          MAK00320
      GOTO 2                                                            MAK00330
   51 CONTINUE                                                          MAK00340
      AN=AN+0.5                                                         MAK00350
    2 CONTINUE                                                          MAK00360
      S=I+1                                                             MAK00370
      T=T+AN                                                            MAK00380
      E=S*(S-1.)*0.25                                                   MAK00390
      D2=E*(S+S+5.)/18.                                                 MAK00400
      D=SQRT(D2)                                                        MAK00410
      S=(T-E)/D                                                         MAK00420
      GOTO(60,61),M                                                     MAK00430
   60 CONTINUE                                                          MAK00440
      HS(L,1)=S                                                         MAK00450
      GOTO 1                                                            MAK00460
   61 CONTINUE                                                          MAK00470
      HS(L,2)=-S                                                        MAK00480
    1 CONTINUE                                                          MAK00490
      GOTO(70,71),M                                                     MAK00500
   70 CONTINUE                                                          MAK00510
      UT=HS(N,1)                                                        MAK00520
      UTA=ABS(UT)                                                       MAK00530
      ALPHA=FNKSF(MO,UTA,N,N,KODE2)                                     MAK00540
      ALPHA=(1.-ALPHA)*2.                                               MAK00550
      M=2                                                               MAK00560
      GOTO 10                                                           MAK00570
   71 CONTINUE                                                          MAK00580
      RETURN                                                            MAK00590
      END                                                               MAK00600
      SUBROUTINE RAKRO(N,RX,Y,R)                                        RAK00010
C     ERZEUGUNG DER ZUR ZEITREIHE GEHOERIGEN RANGREIHE                  RAK00020
      DIMENSION RX(N),Y(N),R(N)                                         RAK00030
      DO 10 I=1,N                                                       RAK00040
   10 Y(I)=RX(I)                                                        RAK00050
      CALL SORT(N,Y)                                                    RAK00060
      DO 300 I=1,N                                                      RAK00070
      K=0                                                               RAK00080
      SK=0.                                                             RAK00090
      DO 200 J=1,N                                                      RAK00100
      IF(RX(I)-Y(J)) 200,150,200                                        RAK00110
  150 K=K+1                                                             RAK00120
      A=J                                                               RAK00130
      SK=SK+A                                                           RAK00140
  200 CONTINUE                                                          RAK00150
      R(I)=SK/K                                                         RAK00160
  300 CONTINUE                                                          RAK00170
      RETURN                                                            RAK00180
      END                                                               RAK00190
