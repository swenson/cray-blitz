      subroutine setup
crev  last revision 03/04/87
c
c     ******************************************************************
c     *                                                                *
c     *      setup is called upon entry to blitz.  it initializes the  *
c     *  table of random numbers used to hash the chess board for      *
c     *  the transposition table.  this routine may be modified to     *
c     *  use the maximum word length of the machine blitz is running   *
c     *  on.                                                           *
c     *      setup also presets the transposition table so that random *
c     *  garbage will not appear to be a valid table entry causing     *
c     *  an unnecessary collision.                                     *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
c
c
      include 'common.f'
c
c
      include 'global.f'
c
c
      dimension ran(1300)
      equivalence (ran(1),random(1,1))
      real*8 tranf
      integer tran
      equivalence (tran, tranf)
c
c------------------------------< initialize attack array
c
      bitbd=0
      do 100 i=22,29
          bit0(i)=ishft(1,85-i)
          bit1(i)=ishft(7,83-i)
 100  continue
      do 200 i=32,39
          bit0(i)=ishft(1,87-i)
          bit1(i)=ishft(7,85-i)
          bit9(i)=ishft(o'40201',73-i)
          bit10(i)=ishft(o'200401',71-i)
          bit11(i)=ishft(o'1001001',69-i)
 200  continue
      do 300 i=42,49
          bit0(i)=ishft(1,89-i)
          bit1(i)=ishft(7,87-i)
          bit9(i)=ishft(o'40201',75-i)
          bit10(i)=ishft(o'200401',73-i)
          bit11(i)=ishft(o'1001001',71-i)
 300  continue
      do 400 i=52,59
          bit0(i)=ishft(1,91-i)
          bit1(i)=ishft(7,89-i)
          bit9(i)=ishft(o'40201',77-i)
          bit10(i)=ishft(o'200401',75-i)
          bit11(i)=ishft(o'1001001',73-i)
 400  continue
      do 500 i=62,69
          bit0(i)=ishft(1,93-i)
          bit1(i)=ishft(7,91-i)
          bit9(i)=ishft(o'40201',79-i)
          bit10(i)=ishft(o'200401',77-i)
          bit11(i)=ishft(o'1001001',75-i)
 500  continue
      do 600 i=72,79
          bit0(i)=ishft(1,95-i)
          bit1(i)=ishft(7,93-i)
          bit9(i)=ishft(o'40201',81-i)
          bit10(i)=ishft(o'200401',79-i)
          bit11(i)=ishft(o'1001001',77-i)
 600  continue
      do 700 i=82,89
          bit0(i)=ishft(1,97-i)
          bit1(i)=ishft(7,95-i)
          bit9(i)=ishft(o'40201',83-i)
          bit10(i)=ishft(o'200401',81-i)
          bit11(i)=ishft(o'1001001',79-i)
 700  continue
      do 800 i=92,99
          bit0(i)=ishft(1,99-i)
          bit1(i)=ishft(7,97-i)
 800  continue
      do 900 i=1,100
          cbit0(i)=xor(bit0(i),-1)
 900  continue
      do 1100 j=1,100
          do 1000 i=1,100
              at(i,j,4)=-1
              if(i.lt.22.or.j.lt.22) go to 1000
              if(cfiles(i).eq.10.or.cfiles(j).eq.10) go to 1000
              if(cfiles(i).eq.1.or.cfiles(j).eq.1) go to 1000
              if(j-i.eq. 9) at(i,j,4)=0
              if(i-j.eq. 9) at(i,j,4)=0
              if(j-i.eq.11) at(i,j,4)=0
              if(i-j.eq.11) at(i,j,4)=0
              if(j-i.eq.18) at(i,j,4)=bit0(i+ 9)
              if(i-j.eq.18) at(i,j,4)=bit0(j+ 9)
              if(j-i.eq.22) at(i,j,4)=bit0(i+11)
              if(i-j.eq.22) at(i,j,4)=bit0(j+11)
              if(cfiles(j).lt.cfiles(i).and.
     *           (j-i.eq.27)) at(i,j,4)=or(bit0(i+ 9),bit0(i+18))
              if(cfiles(i).lt.cfiles(j).and.
     *           (i-j.eq.27)) at(i,j,4)=or(bit0(j+ 9),bit0(j+18))
              if(cfiles(i).lt.cfiles(j).and.
     *           (j-i.eq.33)) at(i,j,4)=or(bit0(i+11),bit0(i+22))
              if(cfiles(j).lt.cfiles(i).and.
     *           (i-j.eq.33)) at(i,j,4)=or(bit0(j+11),bit0(j+22))
              if(cfiles(j).lt.cfiles(i).and.
     *           (j-i.eq.36)) at(i,j,4)=bit9(i+ 9)
              if(cfiles(i).lt.cfiles(j).and.
     *           (i-j.eq.36)) at(i,j,4)=bit9(j+ 9)
              if(cfiles(i).lt.cfiles(j).and.
     *           (j-i.eq.44)) at(i,j,4)=bit11(i+11)
              if(cfiles(j).lt.cfiles(i).and.
     *           (i-j.eq.44)) at(i,j,4)=bit11(j+11)
              if(cfiles(j).lt.cfiles(i).and.
     *           (j-i.eq.45)) at(i,j,4)=or(bit0(i+ 9),bit9(i+18))
              if(cfiles(i).lt.cfiles(j).and.
     *           (i-j.eq.45)) at(i,j,4)=or(bit0(j+ 9),bit9(j+18))
              if(cfiles(i).lt.cfiles(j).and.
     *           (j-i.eq.55)) at(i,j,4)=or(bit0(i+11),bit11(i+22))
              if(cfiles(j).lt.cfiles(i).and.
     *           (i-j.eq.55)) at(i,j,4)=or(bit0(j+11),bit11(j+22))
              if(cfiles(j).lt.cfiles(i).and.
     *           (j-i.eq.54)) at(i,j,4)=or(bit9(i+ 9),bit9(i+27))
              if(cfiles(i).lt.cfiles(j).and.
     *           (i-j.eq.54)) at(i,j,4)=or(bit9(j+ 9),bit9(j+27))
              if(cfiles(i).lt.cfiles(j).and.
     *           (j-i.eq.66)) at(i,j,4)=or(bit11(i+11),bit11(i+33))
              if(cfiles(j).lt.cfiles(i).and.
     *           (i-j.eq.66)) at(i,j,4)=or(bit11(j+11),bit11(j+33))
              if(cfiles(j).lt.cfiles(i).and.
     *           (j-i.eq.63)) at(i,j,4)=or(bit9(i+ 9),bit9(i+36))
              if(cfiles(i).lt.cfiles(j).and.
     *           (i-j.eq.63)) at(i,j,4)=or(bit9(j+ 9),bit9(j+36))
              if(cfiles(i).lt.cfiles(j).and.
     *           (j-i.eq.77)) at(i,j,4)=or(bit11(i+11),bit11(i+44))
              if(cfiles(j).lt.cfiles(i).and.
     *           (i-j.eq.77)) at(i,j,4)=or(bit11(j+11),bit11(j+44))
1000      continue
1100  continue
      do 1400 j=1,100
          do 1300 i=1,100
              at(i,j,3)=-1
              if(i.lt.22.or.j.lt.22) go to 1300
              if(cfiles(i).eq.10.or.cfiles(j).eq.10) go to 1300
              if(cfiles(i).eq.1.or.cfiles(j).eq.1) go to 1300
              if(j-i.eq. 1) at(i,j,3)=0
              if(i-j.eq. 1) at(i,j,3)=0
              if(j-i.eq.10) at(i,j,3)=0
              if(i-j.eq.10) at(i,j,3)=0
              if(j-i.eq. 2) at(i,j,3)=bit0(i+ 1)
              if(i-j.eq. 2) at(i,j,3)=bit0(j+ 1)
              if(j-i.eq.20) at(i,j,3)=bit0(i+10)
              if(i-j.eq.20) at(i,j,3)=bit0(j+10)
              if(cfiles(i).lt.cfiles(j).and.
     *           (j-i.eq. 3)) at(i,j,3)=or(bit0(i+ 1),bit0(i+ 2))
              if(cfiles(j).lt.cfiles(i).and.
     *           (i-j.eq. 3)) at(i,j,3)=or(bit0(j+ 1),bit0(j+ 2))
              if(j-i.eq.30) at(i,j,3)=or(bit0(i+10),bit0(i+20))
              if(i-j.eq.30) at(i,j,3)=or(bit0(j+10),bit0(j+20))
              if(cfiles(i).lt.cfiles(j).and.
     *           (j-i.eq. 4)) at(i,j,3)=bit1(i+ 1)
              if(cfiles(j).lt.cfiles(i).and.
     *           (i-j.eq. 4)) at(i,j,3)=bit1(j+ 1)
              if(j-i.eq.40) at(i,j,3)=bit10(i+10)
              if(i-j.eq.40) at(i,j,3)=bit10(j+10)
              if(cfiles(i).lt.cfiles(j).and.
     *           (j-i.eq. 5)) at(i,j,3)=or(bit0(i+ 1),bit1(i+ 2))
              if(cfiles(j).lt.cfiles(i).and.
     *           (i-j.eq. 5)) at(i,j,3)=or(bit0(j+ 1),bit1(j+ 2))
              if(j-i.eq.50) at(i,j,3)=or(bit0(i+10),bit10(i+20))
              if(i-j.eq.50) at(i,j,3)=or(bit0(j+10),bit10(j+20))
              if(cfiles(i).lt.cfiles(j).and.
     *           (j-i.eq. 6)) at(i,j,3)=or(bit1(i+ 1),bit1(i+ 3))
              if(cfiles(j).lt.cfiles(i).and.
     *           (i-j.eq. 6)) at(i,j,3)=or(bit1(j+ 1),bit1(j+ 3))
              if(j-i.eq.60) at(i,j,3)=or(bit10(i+10),bit10(i+30))
              if(i-j.eq.60) at(i,j,3)=or(bit10(j+10),bit10(j+30))
              if(cfiles(i).lt.cfiles(j).and.
     *           (j-i.eq. 7)) at(i,j,3)=or(bit1(i+ 1),bit1(i+ 4))
              if(cfiles(j).lt.cfiles(i).and.
     *           (i-j.eq. 7)) at(i,j,3)=or(bit1(j+ 1),bit1(j+ 4))
              if(j-i.eq.70) at(i,j,3)=or(bit10(i+10),bit10(i+40))
              if(i-j.eq.70) at(i,j,3)=or(bit10(j+10),bit10(j+40))
1300      continue
1400  continue
      do 1600 j=1,100
          do 1500 i=1,100
              at(i,j,5)=-1
              if(i.lt.22.or.j.lt.22) go to 1500
              if(cfiles(i).eq.10.or.cfiles(j).eq.10) go to 1500
              if(cfiles(i).eq.1.or.cfiles(j).eq.1) go to 1500
              if(j-i.eq. 8) at(i,j,5)=0
              if(i-j.eq. 8) at(i,j,5)=0
              if(j-i.eq.12) at(i,j,5)=0
              if(i-j.eq.12) at(i,j,5)=0
              if(j-i.eq.19) at(i,j,5)=0
              if(i-j.eq.19) at(i,j,5)=0
              if(j-i.eq.21) at(i,j,5)=0
              if(i-j.eq.21) at(i,j,5)=0
1500      continue
1600  continue
      do 1800 j=1,100
          do 1700 i=1,100
              at(i,j,1)=-1
              if(i.lt.22.or.j.lt.22) go to 1700
              if(cfiles(i).eq.10.or.cfiles(j).eq.10) go to 1700
              if(cfiles(i).eq.1.or.cfiles(j).eq.1) go to 1700
              if(j-i.eq. 1) at(i,j,1)=0
              if(i-j.eq. 1) at(i,j,1)=0
              if(j-i.eq. 9) at(i,j,1)=0
              if(i-j.eq. 9) at(i,j,1)=0
              if(j-i.eq.10) at(i,j,1)=0
              if(i-j.eq.10) at(i,j,1)=0
              if(j-i.eq.11) at(i,j,1)=0
              if(i-j.eq.11) at(i,j,1)=0
1700      continue
1800  continue
      do 2000 j=1,100
          do 1900 i=1,100
              at(i,j,6)=-1
              at(i,j,8)=-1
              if(i.lt.22.or.j.lt.22) go to 1900
              if(cfiles(i).eq.10.or.cfiles(j).eq.10) go to 1900
              if(cfiles(i).eq.1.or.cfiles(j).eq.1) go to 1900
              if(j-i.eq. 9) at(i,j,8)=0
              if(i-j.eq. 9) at(i,j,6)=0
              if(j-i.eq.11) at(i,j,8)=0
              if(i-j.eq.11) at(i,j,6)=0
1900      continue
2000  continue
      do 2200 j=1,100
          do 2100 i=1,100
              at(i,j,2)=and(at(i,j,3),at(i,j,4))
              at(i,j,12)=at(i,j,2)
              at(i,j,7)=-1
              at(i,j,9)=at(i,j,5)
              at(i,j,10)=at(i,j,4)
              at(i,j,11)=at(i,j,3)
              at(i,j,13)=at(i,j,1)
2100      continue
2200  continue
c
c------------------------------< initialize table of random numbers.
c
      do 2500 i=1,1300
          ran(i)=0
          if((i-1)/100+1 .eq. 7) go to 2500
          do 2400 j=0,3
              tranf=ranf()
              tran=and(ishft(tran,-32), 65535)
              ran(i)=or(ran(i),ishft(tran,16*j))
2400      continue
2500  continue
c
c------------------------------< now for a kludge. if the program is
c------------------------------< black, reverse the random number table
c------------------------------< so that the hash keys will agree with
c------------------------------< the book data base where the program
c------------------------------< was assumed to be white.
c
      if(color .eq. 1) go to 3000
          do 2700 i=1,100
              do 2600 j=1,6
                  temp=random(i,j)
                  random(i,j)=random(i,14-j)
                  random(i,14-j)=temp
2600          continue
2700      continue
          do 2900 i=21,60
              do 2800 j=1,13
                  temp=random(i,j)
                  random(i,j)=random(121-i,j)
                  random(121-i,j)=temp
2800          continue
2900      continue
3000  continue
      return
      end
