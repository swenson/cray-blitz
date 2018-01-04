      subroutine hcmnd(gover)
crev  last revision 12/09/89
c
c     ******************************************************************
c     *                                                                *
c     *      hcmnd is used to output a list of the game moves that     *
c     *  is kept by the program.  it also keeps up with the time used  *
c     *  by each side to make each move and prints these times in      *
c     *  with each move.  a '+' will indicate which color blitz is     *
c     *  playing.                                                      *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      integer buf1(30), buf2(30), varfmt(16), digits(40)
c
c
      include 'global.f'
c
c
      include 'common.f'
c
c
      equivalence (buf1(1),moves(100)),(buf2(1),moves(200))
      equivalence (period,alpha(68)),(blank,alpha(70))
      data varfmt/'(1x,' , 'i3,' , '1x,' , ' ' , 'a1,' ,
     *  '1x,' , 'a' , ' ' , ',' , 't25,' ,
     *  ' ' , 'a1,' , '1x,', 'a' , ' ' , ')' /
      data digits/'1','2','3','4','5','6','7','8','9','10',
     *'11','12','13','14','15','16','17','18','19','20','21','22',
     *'23','24','25','26','27','28','29','30','31','32','33','34',
     *'35','36','37','38','39','40'/
c
c
c------------------------------< initialize.
c
      tnomovs=nomovs
      if(pndrng .ne. 0) tnomovs=tnomovs-1
      nmoves=npmovs+tnomovs
      if((color.eq.1 .and. mod(nmoves,2).eq.0) .or.
     *   (color.eq.2 .and. mod(nmoves,2).ne.0)) nmoves=nmoves+1
      m=0
      i=0
c
c------------------------------< output heading
c
      if(gover.eq.0 .and. color.eq.2) print 100
      if(gover.eq.0 .and. color.eq.1) print 200
      if(gover.ne.0 .and. color.eq.2) write(3,100)
      if(gover.ne.0 .and. color.eq.1) write(3,200)
100   format(4x,'white',15x,'black+')
200   format(5x,'white+',13x,'black')
300   continue
          do 400 j=1,4
              buf2(j)=period
400       continue
          do 500 j=5,30
              buf2(j)=blank
500       continue
c
c------------------------------< read moves from history file
c
          elap2=0
          i=i+1
          if(i .gt. nmoves) go to 9999
          read(unit=1,fmt=501,rec=i+1) buf1, elap1, d1, d2, d3
501       format(30a1,4i6)
          i=i+1
          if(i .gt. nmoves) go to 600
          read(unit=1,fmt=501,rec=i+1) buf2, elap2, d1, d2, d3
600       continue
          m=m+1
c
c------------------------------< set up variable format to output
c------------------------------< '(movetime)'
c
          do 700 j=1,15
              if(buf1(16-j) .ne. blank) go to 800
700       continue
          j=15
800       continue
          end1=16-j
          varfmt(4)=digits(end1)
          varfmt(8)=digits(5)
          do 900 j=1,15
              if(buf2(16-j).ne.blank) go to 1000
900       continue
          j=15
1000      continue
          end2=16-j
          varfmt(11)=digits(end2)
          varfmt(15)=digits(5)
          if(buf1(1) .eq. period) elap1=0
          if(buf2(1) .eq. period) elap2=0
          if(gover .eq. 0)
     *    print varfmt, m, (buf1(j),j=1,end1), hhmmss(elap1,5),
     *                     (buf2(j),j=1,end2), hhmmss(elap2,5)
          if(gover .ne. 0)
     *    write(3,varfmt) m, (buf1(j),j=1,end1), hhmmss(elap1,5),
     *                       (buf2(j),j=1,end2), hhmmss(elap2,5)
      go to 300
9999  continue
          return
      end
      character*8 function hhmmss(tsecs,length)
crev  last revision 08/20/90
c
c     ******************************************************************
c     *                                                                *
c     *      hhmmss is used to output the time in the format           *
c     *  hh:mm:ss.  the length argument determines whether or not the  *
c     *  hh: part of the result is returned.                           *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      character*2 conv(60)
      data conv / '00','01','02','03','04','05','06','07','08','09',
     *            '10','11','12','13','14','15','16','17','18','19',
     *            '20','21','22','23','24','25','26','27','28','29',
     *            '30','31','32','33','34','35','36','37','38','39',
     *            '40','41','42','43','44','45','46','47','48','49',
     *            '50','51','52','53','54','55','56','57','58','59' / 
      secs=tsecs 
      hours=secs/3600
      secs=secs-hours*3600
      mins=secs/60
      secs=secs-mins*60
      hhmmss=conv(hours+1)//':'//conv(mins+1)//':'//conv(secs+1)
      hhmmss=hhmmss(9-length:8)
      if(hhmmss(1:1) .eq. '0') hhmmss(1:1)=' '
      return
      end
