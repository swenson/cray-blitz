      subroutine options(busy)
crev  last revision 12/09/90
c
c     ******************************************************************
c     *                                                                *
c     *      options is the driver for all of the separate             *
c     *  command modules.  it decodes the first character of the       *
c     *  input and calls the correct module.  the only direct          *
c     *  input to option is 'help' which requests a listing of all     *
c     *  legal commands.                                               *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      external asyncio
      external control
      real rftime, oh1, oh2, oh3, oh4, oh5, rplotlen, relap, ridle
      real xscore
      character*30 fname
      character*1 optitoc
      dimension plot(50), idx(3), tskarray(100)
      integer*4 hmemsize
c
c
      include 'global.f'
c
c
      include 'common.f'
c
c
      equivalence     (ala,alpha(27)),(alb,alpha(28)),(alc,alpha(29)),
     *(ald,alpha(30)),(ale,alpha(31)),(alf,alpha(32)),(alg,alpha(33)),
     *(alh,alpha(34)),(ali,alpha(35)),(alj,alpha(36)),(alk,alpha(37)),
     *(all,alpha(38)),(alm,alpha(39)),(aln,alpha(40)),(alo,alpha(41)),
     *(alp,alpha(42)),(alq,alpha(43)),(alr,alpha(44)),(als,alpha(45)),
     *(alt,alpha(46)),(alu,alpha(47)),(alv,alpha(48)),(alw,alpha(49)),
     *(alx,alpha(50)),(aly,alpha(51)),(alz,alpha(52)),(blank,alpha(70)),
     *(equal,alpha(67)),(quest,alpha(69)),(comma,alpha(72)),
     *(aster,alpha(66))
c
c
c------------------------------< don't treat algebraic moves as
c------------------------------< commands. simply return.
c
      return=0
      do 10 ii=1,3
          if(ii.lt.3 .and. atext(ii).eq.blank) go to 20
          if(atext(ii).eq.alpha(67) .or. 
     *       atext(ii).eq.alpha(65))           go to 20
          if(atext(ii).gt.alpha(53) .and. atext(ii).le.alpha(62)) return
10    continue
20    continue
      write(3,30) atext
30    format(1x,80a1)
c
c------------------------------< now, off to the correct routine.
c
      if(atext(1).eq.ala .and. atext(2).eq.alc)     go to  200
      if(atext(1).eq.ala .and. atext(2).eq.alg)     go to  200
      if(atext(1).eq.alb .and. atext(2).eq.alp)     go to  300
      if(atext(1).eq.alb .and. atext(2).eq.alm)     go to  400
      if(atext(1).eq.alc .and. atext(2).eq.all)     go to  500
      if(atext(1).eq.ald .and. atext(2).eq.blank)   go to  600
      if(atext(1).eq.ald .and. atext(2).eq.alb)     go to  700
      if(atext(1).eq.ald .and. atext(2).eq.alm)     go to  800
      if(atext(1).eq.ald .and. atext(2).eq.alo)     go to  600
      if(atext(1).eq.ald .and. atext(2).eq.alp)     go to  900
      if(atext(1).eq.ald .and. atext(2).eq.alr)     go to 1000
      if(atext(1).eq.ale .and. atext(2).eq.aln)     go to 3700
      if(atext(1).eq.ale .and. atext(2).eq.alc)     go to 1100
      if(atext(1).eq.ale .and. atext(2).eq.alx .and.
     *   atext(3).eq.ali .and. atext(4).eq.alt)     go to 1150
      if(atext(1).eq.alf .and. atext(2).eq.blank)   go to 1200
      if(atext(1).eq.alh .and. atext(2).eq.ale)     go to   50
      if(atext(1).eq.alh .and. atext(2).eq.blank)   go to 1300
      if(atext(1).eq.alh .and. atext(2).eq.als)     go to 1400
      if(atext(1).eq.ali .and. atext(2).eq.aln)     go to 1500
      if(atext(1).eq.all .and. atext(2).eq.alg)     go to 1600
      if(atext(1).eq.alm .and. atext(2).eq.alp)     go to 1700
      if(atext(1).eq.alm .and. atext(2).eq.als)     go to 1800
      if(atext(1).eq.aln .and. atext(2).eq.alm)     go to 1850
      if(atext(1).eq.alp .and. atext(2).eq.alg)     go to 1900
      if(atext(1).eq.alp .and. atext(2).eq.all)     go to 2000
      if(atext(1).eq.alp .and. atext(2).eq.alo)     go to 2100
      if(atext(1).eq.alp .and. atext(2).eq.alr)     go to 2200
      if(atext(1).eq.alr .and. atext(2).eq.blank)   go to 2300
      if(atext(1).eq.alr .and. atext(2).eq.alk)     go to 2300
      if(atext(1).eq.alr .and. atext(2).eq.als)     go to 2350
      if(atext(1).eq.als .and. atext(2).eq.alb)     go to 2400
      if(atext(1).eq.als .and. atext(2).eq.alc)     go to 2450
      if(atext(1).eq.als .and. atext(2).eq.ald)     go to 2500
      if(atext(1).eq.als .and. atext(2).eq.all)     go to 2600
      if(atext(1).eq.als .and. atext(2).eq.alm)     go to 2700
      if(atext(1).eq.als .and. atext(2).eq.aln)     go to 2800
      if(atext(1).eq.als .and. atext(2).eq.alo)     go to 2900
      if(atext(1).eq.als .and. atext(2).eq.alp)     go to 2950
      if(atext(1).eq.als .and. atext(2).eq.alr)     go to 3000
      if(atext(1).eq.als .and. atext(2).eq.alt)     go to 3100
      if(atext(1).eq.als .and. atext(2).eq.alv)     go to 3200
      if(atext(1).eq.alt .and. atext(2).eq.ali)     go to 1100
      if(atext(1).eq.alt .and. atext(2).eq.alm)     go to 3300
      if(atext(1).eq.alt .and. atext(2).eq.aln)     go to 3350
      if(atext(1).eq.alt .and. atext(2).eq.alr)     go to 3400
      if(atext(1).eq.alt .and. atext(2).eq.alv)     go to 3450
      if(atext(1).eq.alv .and. atext(2).eq.alt)     go to 3500
      if(atext(1).eq.alw .and. atext(2).eq.als)     go to 3600
      if(atext(1).eq.quest) go to 100
      backspace 3
      return=0
      return
c
c------------------------------< help : display commands and explanation
c
50    continue
      print 51
51    format(//
     *1x,'?            : repeat the program''s last move'/
     *1x,'ac           : analyze current game (using history file)'/
     *1x,'ag           : analyze a game by keying in all moves'/
     *1x,'ai           : enable asynchronous i/o mode'/
     *1x,'bm(s)        : set blitz mode (5 min)'/
     *1x,'bp           : toggle beep announcing move on/off'/
     *1x,'cl?          : control clock'/
     *1x,'d            : display board'/
     *1x,'dm           : display multiprocessing statistics'/
     *1x,'dp=n         : control parallel processing depth'/)
      print 52
52    format(
     *1x,'draw         : offer program a draw'/
     *1x,'eb           : activate electronic board'/
     *1x,'end          : terminate execution'/
     *1x,'f            : force blitz to make a specific move'/
     *1x,'h            : list game move history'/
     *1x,'hs=n         : change size of hash table [hash,pawn,king]'/
     *1x,'in x [title] : read input from file x until <eof>'/
     *1x,'               [search file for title before processing]'/
     *1x,'mp=n         : enable multiprocessing'/
     *1x,'msg          : enter message into history file'/
     *1x,'nm           : enable/disable null move algorithm'/)
      print 55
      call command(eof)
      if(eof .ne. 0) go to 9999
      if(atext(1) .ne. blank) go to 9999
      print 53
53    format(
     *1x,'pg           : print game history (compressed)'/
     *1x,'pl           : play black/white (switch sides)'/
     *1x,'po           : ponder a specific move'/
     *1x,'pr           : program a sequence of automatic commands'/
     *1x,'r n          : reset board to a prior position'/
     *1x,'rk n         : set board by inputting moves'/
     *1x,'sb n         : set up a specific board position'/
     *1x,'sc           : score current board position'/
     *1x,'scm          : scm 1 50 sets parameter 1 = 50'/
     *1x,'scd          : displays current scoring parameters'/
     *1x,'sd=n         : set a specific iteration depth'/
     *1x,'sl=n         : set status display level (0-3)'/
     *1x,'sm(s)        : set simultaneous mode'/
     *1x,'sn=n         : set minimum nodes before stats displayed'/)
      print 54
54    format(
     *1x,'so=n         : set opening book score margin'/
     *1x,'sp x         : save current board position in file x'/
     *1x,'sr           : set/display uscf ratings'/
     *1x,'st=n         : set a specific time per move'/
     *1x,'sv=n         : set upper bound to terminate search'/
     *1x,'tm(s)        : toggle tournament mode on/off'/
     *1x,'tr=n         : trace search'/
     *1x,'tv           : toggle true-value search on/off'/
     *1x,'vt=n         : set/reset video display'/
     *1x,'   (s) in some commands turns on automatic search'/
     *1x,'       statistics printout (do not type parentheses)'/
     *)
      print 55
55    format(1x,'...press return for more')
      call command(eof)
      if(eof .ne. 0) go to 9999
      if(atext(1) .ne. blank) go to 9999
      print 57
57    format(
     *1x,'multiple commands may be entered on one line separated '/
     *1x,'by '';'' characters.  each command will be executed in'/
     *1x,'sequence exactly as if they were entered on separate lines.'/
     *1x,'all input is subject to this so caution should be used as'/
     *1x,'it is possible to enter several moves at once.'//
     *1x,'moves are entered using standard algebraic notation.'/
     *1x,'the program will also accept minimum algebraic notation'/
     *1x,'if desired.  for example, the following are correct ways'/
     *1x,'to input moves:  Nb1-c3, b1-c3, Nc3, Nbc3, Pe2-e4, e2-e4'/)
      print 58
58    format(
     *1x,'e4, Nc3xe4, Nxe4, Ncxe4, Pe4xd5, exd5, exd.  notice that'/
     *1x,'pieces are indicated by capital letters and files are'/
     *1x,'indicated by small letters.  also, the ''x'' is used to'/
     *1x,'indicate a capture move.'/
     *1x,'note also that the ''pr'' command can be used to make the'/
     *1x,'program play itself by using ''pl'' (change sides) and'/
     *1x,'''go'' (make move) as the last two programmed commands!'/)
      print 55
      call command(eof)
      if(eof .ne. 0) go to 9999
      go to 9999
c
c------------------------------< ? : report last move
c
100   continue
          key=npmovs+npmovs
          if(color .eq. 2) key=key+1
          read(unit=1,fmt=101,rec=key) (text(i),i=1,30), time
101       format(30a1,i6)
          print 110,(text(i),i=1,30)
110       format(1x,'my move was ',30a1)
          print 120, hhmmss(time,5)
120       format(7x,'clock time was ',a5)
          go to 9999
c
c------------------------------< ac/ag : analyze a game which has moves
c------------------------------<         stored in the history file
c------------------------------<         (FORTRAN unit 1)
c
200   continue
          if(busy .ne. 0) go to 9998
          if(atext(4) .ne. blank) then
              col=4
              alimit=scan(col)*2
              amoves=0
              if(atext(2) .eq. alg) then
                  text(2)=alk
                  call rcmnd
              endif
              close (unit=11)
              open (unit=11,status='unknown',file='agame',
     *              access='sequential', form='formatted',err=1520)
              if(color .eq. 1) then
                  write(11,201)
201               format('go')
              endif
              do 210 i=1,alimit
                  read(unit=1,fmt=202,rec=i+1) 
     *                                  (atext(l),l=1,30)
202               format(30a1)
                  if((mod(i,2).eq.1 .and. color.eq.1) .or.
     *               (mod(i,2).eq.0 .and. color.eq.2)) then
                      write(11,203) (atext(l),l=1,30)
203                   format('f;',30a1)
                  else
                      write(11,202) (atext(l),l=1,30)
                  endif
210           continue
              close (unit=11)
              npmovs=0
              nomovs=0
              call setgb(0)
              print 215
215           format(/1x,'Set program options, then type "in agame"'/
     *                1x,'pondering is now turned off.  DO NOT do'/
     *                1x,'anything that might turn it back on.'/)
          else
              print 220
220           format(1x,'number of moves missing. (ag ''n'')')
          endif
          go to 9999
c
c------------------------------< bp : toggle beep on/off
c
300   continue
          if(bell .ne. '') then
              bell=''
              print 320
320           format(1x,'beep on.')
          else
              print 330
330           format(1x,'beep off.')
              bell=0
          endif
          go to 9999
c
c------------------------------< bm : set blitz mode (5 min)
c
400   continue
          if(busy .ne. 0) go to 9998
          tmode=xor(tmode,1)
          smode=0
          if(atext(3) .eq. als) autos=2
          timlim(1)=5
          timlim(2)=999999999
          timlim(3)=5
          timlim(4)=999999999
          timlim(5)=5
          cquery=0
          cputim=0
          if(tmode .ne. 0) then
              print 410
              write(3,410)
410           format(1x,'blitz mode.')
              open (unit=11,status='old',file='default.bm',
     *              access='sequential', form='formatted',err=440)
              print 420
420           format(1x,'use default.bm file?')
              call command(eof)
              if(atext(1) .ne. aly) then
                  close (unit=11)
                  go to 9999
              endif
              print 430
430           format(1x,'using default.tm')
              unit=11
440           continue
          endif
          go to 9999
c
c------------------------------< cl : control clock
c
500   continue
          call ccmnd
          go to 9999
c
c------------------------------< d : display game board
c
600   continue
          if(busy .ne. 0) go to 9998
          call dcmnd
          go to 9999
c
c------------------------------< db : set debug flags
c
700   continue
          if(atext(3) .ne. blank) then
              col=4
              debug=scan(col)
          endif
          if(and(debug,1) .ne. 0) print 710
          if(and(debug,2) .ne. 0) print 720
          if(and(debug,4) .ne. 0) print 730
          if(and(debug,8) .ne. 0) print 740
          if(and(debug,16) .ne. 0) print 750
          if(and(debug,32) .ne. 0) print 760
          if(and(debug,64) .ne. 0) print 770
          if(and(debug,128) .ne. 0) print 780
          if(and(debug,256) .ne. 0) print 790
          if(and(debug,512) .ne. 0) print 795
          if(and(debug,1024) .ne. 0) print 796
          if(and(debug,2048) .ne. 0) print 798
          go to 9999
710       format(1x,'   (1)  display ply-1 move list and scores')
720       format(1x,'   (2)  display bad hash board and move')
730       format(1x,'   (4)  disable transposition table scoring')
740       format(1x,'   (8)  display parallel splits')
750       format(1x,'  (16)  display parallel unsplits')
760       format(1x,'  (32)  display parallel merges')
770       format(1x,'  (64)  display parallel shares')
780       format(1x,' (128)  display parallel control messages')
790       format(1x,' (256)  display parallel backups')
795       format(1x,' (512)  display parallel search messages')
796       format(1x,'(1024)  display information about node types')
798       format(1x,'(2048)  display information about thrashing',
     *              ' control')
c
c-----------------------------< dm : display multiprocessing
c-----------------------------<      statistics
c
800   continue
          maxnodes=max0(totnodes(1),1)
          do 825 i=2,ncpus
              maxnodes=max0(maxnodes,totnodes(i))
825       continue
          if(maxnodes .ne. 0) then
              if(atext(3) .ne. aster) print 840
              write(3,840)
840           format(1x,'cpu','    idle  ',4x,'nodes',1x,
     *        '          20%       40%       60%       80%       100%')
              do 880 i=1,ncpus
                  ridle=float(idlet(i))/100.0
                  do 850 j=1,50
                      plot(j)=' '
850               continue
                  rplotlen=float(totnodes(i))/float(maxnodes)*50.0
                  iplotlen=rplotlen
                  iplotlen=max(iplotlen,1)
                  do 860 j=1,iplotlen
                      plot(j)='-'
860               continue
                  if(atext(3) .ne. aster) print 870, i, ridle,
     *                                               totnodes(i), plot
                  write(3,870) i, ridle, totnodes(i), plot
870               format(1x,i2,3x,f7.2,i10,3x,50a1)
880           continue
              oh1=float(cotime)/100.0
              oh2=float(metime)/100.0
              oh3=float(sptime)/100.0
              oh4=float(untime)/100.0
              oh5=float(overhead)/100.0
              tsplits=0
              ttprocs=0
              tnstops=0
              tfalses=0
              do 885 i=1,50
                  tsplits=tsplits+splits(i)
                  ttprocs=ttprocs+tprocs(i)
                  tnstops=tnstops+nstops(i)
                  tfalses=tfalses+falses(i)
885           continue
              if(atext(3) .ne. aster) print 890, oh1, oh2, oh3, oh4,
     *                   oh5, nsplits+tsplits, tsplits, ttprocs,
     *                   tnstops, tfalses
              write(3,890) oh1, oh2, oh3, oh4, oh5, nsplits+tsplits, 
     *                   tsplits, ttprocs, tnstops, tfalses
890           format(1x,'control=',f7.2, 's',
     *               1x,'merge=',f7.2, 's',
     *               1x,'split=',f7.2, 's',
     *               1x,'unsplit=',f7.2, 's',
     *               1x,'total=',f7.2, 's'/
     *               1x,'splits attempted=',i7,
     *               1x,'splits done=',i7/
     *               1x,'processors assigned=',i7,
     *               1x,'stopped early=',i7,
     *               1x,'false starts=',i7)
              if(atext(3) .eq. alp) then
                  i1=0
                  i2=0
                  i3=0
                  i4=0
                  i5=0
                  do 892 i=1,50
                      if(splits(i) .ne. 0) i1=i
                      if(tprocs(i) .ne. 0) i2=i
                      if(nstops(i) .ne. 0) i3=i
                      if(exnodes(i).ne. 0) i4=i
                      if(falses(i) .ne. 0) i5=i
892               continue
                  if(atext(3) .ne. aster) print 894, (splits(i),i=1,i1)
                  write(3,894) (splits(i),i=1,i1)
894               format(1x,'parallel splits by ply     :',7i7,
     *                   (/29x,7i7))
                  if(atext(3) .ne. aster) print 895, (tprocs(i),i=1,i2)
                  write(3,895) (tprocs(i),i=1,i2)
895               format(1x,'processors assigned  by ply:',7i7,
     *                   (/29x,7i7))
                  if(atext(3) .ne. aster) print 896, (nstops(i),i=1,i3)
                  write(3,896) (nstops(i),i=1,i3)
896               format(1x,'processors stopped by ply  :',7i7,
     *                   (/29x,7i7))
                  if(atext(3) .ne. aster) print 897, (exnodes(i),i=1,i4)       
                  write(3,897) (exnodes(i),i=1,i4)
897               format(1x,'extra nodes examined by ply:',7i7,
     *                   (/29x,7i7))
                  if(atext(3) .ne. aster) print 898, (falses(i),i=1,i5)
                  write(3,898) (falses(i),i=1,i5)
898               format(1x,'false starts done by ply   :',7i7,
     *                   (/29x,7i7))
              endif
          endif
          go to 9999
c
c------------------------------< dp : control multiprocessing
c------------------------------<      division depth minimum
c
900   continue
          if(atext(3) .ne. blank) then
              col=4
              minply=scan(col)
          endif
          minply=max(minply,2)
          print 930, minply
930       format(1x,'parallel division not below ply ',i2)
          go to 9999
c
c------------------------------< draw : offer program a draw
c
1000  continue
          if(peval .ge. drawsc+100) then
              print 1010, name
1010          format(/1x,'draw refused, ',a20/)
          else
              print 1030, name
1030          format(/1x,'draw accepted, ',a20)
          endif
          go to 9999
c
c------------------------------< echo/title : display text for operator.
c
1100  continue
          go to 9999
c
c------------------------------< exit : return to normal input device.
c
1150  continue
          unit=5
          go to 9999
c
c------------------------------< f : force blitz to make specific move.
c
1200  continue
          if(busy .ne. 0) go to 9998
          call fcmnd
          go to 9999
c
c------------------------------< h : list game history
c
1300  continue
          call hcmnd(0)
          go to 9999
c
c------------------------------< hs : change hash size
c
1400  continue
          if(busy .ne. 0) go to 9998
          if(atext(3) .ne. blank) then
              col=4
              sizeh=scan(col)
              col=col+1
              sizep=scan(col)
              if(sizep .eq. 0) sizep=sizeh-3
              col=col+1
              sizek=scan(col)
              if(sizek .eq. 0) sizek=sizeh-3
              reduced=0
              oldmem=(hsize+7168)*3+3*(psize/2)+ksize*2
1410          continue
                  print 1415, sizeh, sizep, sizek
1415              format('hs=',i2,',',i2,',',i2)
                  hsize=2**sizeh
                  hsizm1=hsize-1
                  ksize=2**sizek
                  ksizm1=ksize-1
                  psize=2**sizep
                  psizm1=psize-1
                  htbl1=7168+hsize
                  htbl2=htbl1*2
                  ktbl1=htbl2+hsize+7168+1
                  ktbl2=ktbl1+ksize
                  ptbl1=ktbl2+ksize+1
                  ptbl2=ptbl1+psize
                  newmem=3*(hsize+7168)+3*(psize/2)+2*ksize
                  hmemsize=8*(newmem-oldmem)
c                 sizeok=sbrkf(hmemsize)
                  if(sizeok .le. 0) then
                      reduced=reduced+1
                      if(reduced .le. 7) then
                          if(mod(reduced,2) .eq. 0) sizek=sizek-1
                          if(mod(reduced,2) .eq. 1) sizep=sizep-1
                          go to 1410
                      endif
                      sizeh=sizeh-1
                      sizep=sizeh-3
                      sizek=sizeh-3
                      reduced=0
                      go to 1410
                  endif
              endif
              newmem=3*(hsize+7168)+3*(psize/2)+2*ksize
              print 1450, optlog2(hsize),hsize, optlog2(psize), psize,
     *                    optlog2(ksize), ksize, newmem
              write(3,1450) optlog2(hsize),hsize, optlog2(psize), psize,
     *                      optlog2(ksize), ksize, newmem
1450          format(//1x,'tree search hash table size (3x)     2**',i2,
     *                                              ' (',i8,') entries'/
     *                 1x,'pawn hash table size (1.5x)          2**',i2,
     *                                              ' (',i8,') entries'/
     *                 1x,'king safety hash table size (2x)     2**',i2,
     *                                              ' (',i8,') entries'/
     *                 1x,'total memory requirement is               ',
     *                                              ' (',i8,') words'//)
              do 1460 i=1,newmem
                  htable(i)=0
1460          continue
              go to 9999
c
c------------------------------< in : read input from file x
c
1500  continue
          if(busy .ne. 0) go to 9998
          fname=optitoc(atext(4))
          do 1510 ichar=2,30
              if(atext(ichar+3) .eq. blank) go to 1520
              fname(ichar:ichar)=optitoc(atext(ichar+3))
1510      continue
1520      continue
          close (unit=11)
          open (unit=11,status='old',file=fname,
     *          access='sequential', form='formatted',err=1580)
          unit=11
          ichar=ichar+3
          do 1530 ifirst=ichar,80
              if(atext(ifirst) .ne. blank) go to 1532
1530      continue
          go to 9999
1532      continue
          do 1534 ilast=80,1,-1
              if(atext(ilast) .ne. blank) go to 1540
1534      continue
1540      continue
              read(unit=11,fmt=1550,end=1580) text
1550          format(80a1)
              if(text(1).ne.alt .or. text(2).ne.ali .or.
     *           text(3).ne.alt .or. text(4).ne.all .or.
     *           text(5).ne.ale) go to 1540
                  do 1552 jfirst=6,80
                      if(text(jfirst) .ne. blank) go to 1554
1552              continue
                  go to 1540
1554              continue
                      do 1560 i=ifirst,ilast
                          if(atext(i) .ne. text(jfirst)) go to 1540
                          jfirst=jfirst+1
1560                  continue
                      print 1565,(text(i),i=1,72)
1565                  format(1x,72a1)
          go to 9999
1570      continue
              print 1575
1575          format(1x,'file does not exist, check name')
              go to 9999
1580      continue
              print 1585
1585          format(1x,'couldn''t find a record with that title')
              go to 9999
c
c------------------------------< lg : list game
c
1600  continue
          if(busy .ne. 0) go to 9998
          rewind 3
1610  continue
          read(3,fmt=1620,end=1660) atext
1620      format(80a1)
          do 1630 lim=80,1,-1
              if(atext(lim) .ne. blank) go to 1640
1630      continue
1640      continue
          print 1650,(atext(i1),i1=1,lim)
1650      format(1x,80a1)
      go to 1610
1660  continue
      backspace 3
      go to 9999
c
c------------------------------< mp : enable multiprocessing
c
1700  continue
          if(busy .ne. 0) go to 9998
          oldncpus=ncpus
          if(atext(3) .ne. blank) then
              col=4
              ncpus=scan(col)
              write(3,1720) ncpus
              if(ncpus .gt. 1) cputim=0
              snodes=snodes/oldncpus*ncpus
          endif
          print 1720, ncpus
1720      format(1x,'multiprocessing using',i3,' processors.')
c         call tsktune('MAXCPU',aio+ncpus,'DBRELEAS',aio+ncpus,
c    *                 'DBACTIVE',0,'HOLDTIME',1000,'SAMPLE',1000)
          if(ncpus .gt. oldncpus) then
              print 1725,ncpus-oldncpus
1725          format(1x,'creating ',i2,' new tasks.')
              do 1730 i=1,ncpus-oldncpus
                  tskarray((i-1)*3+1)=3
                  tskarray((i-1)*3+2)=0
                  tskarray((i-1)*3+3)=i
c                 call tskstart(tskarray((i-1)*3+1),control,0)
1730          continue
          else if(ncpus .lt. oldncpus) then
              print 1735, oldncpus-ncpus
1735          format(1x,'terminating ',i2,' old tasks.')
              do 1740, i=oldncpus,ncpus+1,-1
                  stop(i)=-9999
1740          continue
          endif
          go to 9999
c
c------------------------------< msg : insert message into history
c
1800  continue
          print 1810
1810      format(1x,'enter message, blank line or <eof> to terminate'/)
1820      continue
              call command(eof)
              if(eof .ne. 0) go to 9999
              if(atext(1) .eq. blank) go to 9999
              write(3,1830) atext
1830          format(80a1)
          go to 1820
c
c------------------------------< nm : control null move search
c
1850  continue
          if(atext(3) .ne. blank) then
              col=4
              xnullm=scan(col)
              if(xnullm.lt.0 .or. xnullm.gt.2) then
                  print 1860
1860              format(1x,'legal range is 0-2.')
                  xnullm=0
              endif
          endif
          if(xnullm .gt. 0) then
              print 1870, xnullm
1870          format(1x,'null move search is to depth - ',i1,'.')
          else
              print 1880
1880          format(1x,'null move search is disabled.')
          endif
          go to 9999
c
c------------------------------< pg : print compressed history
c
1900  continue
          call pgcmnd
          go to 9999
c
c------------------------------< pl : play black/white (switch)
c
2000  continue
          if(busy .ne. 0) go to 9998
          call playbw(color)
          go to 9999
c
c------------------------------< po : ponder a specific move
c
2100  continue
          if(busy .ne. 0) go to 9998
          call pocmnd
          go to 9999
c
c------------------------------< pr : program automatic commands
c
2200  continue
          do 2220 ii=1,10
              print 2210,ii,(prcmnds(j,ii),j=1,5)
2210          format(1x,i2,2x,5a1)
2220      continue
              print 2230
2230          format(1x,'enter command number; command',
     *                  '[return when done]')
              call command(eof)
              if(eof .ne. 0) go to 9999
              col=1
              comnum=scan(col)
              if(comnum .eq. 0) go to 9999
              call command(eof)
              if(eof .ne. 0) go to 9999
              do 2240 ii=1,5
                  prcmnds(ii,comnum)=atext(ii)
2240          continue
          go to 2200 
c
c------------------------------< r : reset board to a prior position
c
2300  continue
          if(busy .ne. 0) go to 9998
          call rcmnd
          go to 9999
c
c------------------------------< rs : read scoring data from file x
c
2350  continue
          if(busy .ne. 0) go to 9998
          fname=optitoc(atext(4))
          do 2360 ii=2,30
              fname(ii:ii)=optitoc(atext(ii+3))
2360      continue
          close (unit=11)
          open (unit=11,status='old',file=fname,
     *          access='sequential', form='formatted',err=2370)
          read(11,2365) scoring
2365      format(t45,i4)
          go to 9999
2370      continue
              print 2380
2380          format(1x,'file does not exist, check name')
              go to 9999
c
c------------------------------< sb : set up a specific board position.
c
2400  continue
          if(busy .ne. 0) go to 9998
          call sbcmnd(1)
          go to 9999
c
c------------------------------< sc : score current board
c
2450  continue
          if(busy .ne. 0) go to 9998
          ply=0
          call locate
          if(atext(3) .eq. ald) go to 2460
          if(atext(3) .eq. alm) go to 2490
          pscore=0
          call scorep
          xscore=float(pscore)/1000.0
          print 2451, xscore
2451      format(1x,'pawn score.....................',f7.3)
          pscore=0
          call scorekm
          xscore=float(pscore)/1000.0
          print 2452, xscore
2452      format(1x,'opponent king safety score.....',f7.3)
          pscore=0
          call scorekp
          xscore=float(pscore)/1000.0
          print 2453, xscore
2453      format(1x,'program king safety score......',f7.3)
          pscore=0
          call scoredv
          xscore=float(pscore)/1000.0
          print 2454, xscore
2454      format(1x,'development score..............',f7.3)
          call score
          xscore=float(mscore)/1000.0
          print 2455, xscore
2455      format(1x,'material score.................',f7.3)
          xscore=float(pscore+mscore)/1000.0
          print 2456, xscore
2456      format(1x,'total score...................',f8.3)
          print 9393, pscore, mscore
9393      format(1x,"pscore=",i8, 2x, "mscore=",i8)
          go to 9999
c
c------------------------------< scd : display scoring parameters
c
2460      continue
              call prescore
              call scorep
              if(atext(4) .eq. blank) then
                  print 3620, (i,scoring(i),i=1,16)
                  print 2465
                  call command(eof) 
                  print 3621, (i,scoring(i),i=17,32)
                  print 2465
                  call command(eof) 
                  print 3622, (i,scoring(i),i=33,48)
                  print 2465
                  call command(eof) 
                  print 3623, (i,scoring(i),i=49,64)
                  print 2465
                  call command(eof) 
                  print 3624, (i,scoring(i),i=65,80)
                  print 2465
                  call command(eof) 
                  print 3625, (i,scoring(i),i=81,96)
                  print 2465
                  call command(eof) 
                  print 3626, (i,scoring(i),i=97,112)
                  print 2465
                  call command(eof) 
                  print 3627, (i,scoring(i),i=113,128)
                  print 2465
                  call command(eof) 
                  print 3628, (i,scoring(i),i=129,144)
                  print 2465
                  call command(eof) 
                  go to 9999
              else if (atext(4) .eq. alp) then
                  print 2470, (i-1,(ppawnb(i*10+j),j=9,2,-1),
     *                         i-1,(opawnb(i*10+j),j=9,2,-1),i=9,2,-1),
     *                         (pweakp(i),i=9,2,-1),
     *                         (oweakp(i),i=9,2,-1)
2470              format(9x,'program pawn board',21x,
     *                      'opponent pawn board'//
     *                          8(1x,i1,1x,8i4,5x,i1,1x,8i4/)/
     *                       3x,'   a   b   c   d   e   f   g   h'
     *                       7x,'   a   b   c   d   e   f   g   h'//
     *                   16x,'weak pawns',29x,'weak pawns'/
     *                       2x,8(1x,8i4,7x,8i4/))
              else if (atext(4) .eq. aln) then
                  print 2471, (i-1,(pknightb(i*10+j),j=9,2,-1),
     *                         i-1,(oknightb(i*10+j),j=9,2,-1),i=9,2,-1)
2471              format(8x,'program knight board',19x,
     *                      'opponent knight board'//
     *                          8(1x,i1,1x,8i4,5x,i1,1x,8i4/)/
     *                       3x,'   a   b   c   d   e   f   g   h'
     *                       7x,'   a   b   c   d   e   f   g   h'/)
              else if (atext(4) .eq. alb) then
                  print 2472, (i-1,(pbishopb(i*10+j),j=9,2,-1),
     *                         i-1,(obishopb(i*10+j),j=9,2,-1),i=9,2,-1)
2472              format(8x,'program bishop board',19x,
     *                      'opponent bishop board'//
     *                          8(1x,i1,1x,8i4,5x,i1,1x,8i4/)/
     *                       3x,'   a   b   c   d   e   f   g   h'
     *                       7x,'   a   b   c   d   e   f   g   h'/)
              else if (atext(4) .eq. alr) then
                  print 2473, (i-1,(prookb(i*10+j),j=9,2,-1),
     *                         i-1,(orookb(i*10+j),j=9,2,-1),i=9,2,-1)
2473              format(9x,'program rook board',21x,
     *                      'opponent rook board'//
     *                          8(1x,i1,1x,8i4,5x,i1,1x,8i4/)/
     *                       3x,'   a   b   c   d   e   f   g   h'
     *                       7x,'   a   b   c   d   e   f   g   h'/)
              else if (atext(4) .eq. alq) then
                  print 2474, (i-1,(pqueenb(i*10+j),j=9,2,-1),
     *                         i-1,(oqueenb(i*10+j),j=9,2,-1),i=9,2,-1)
2474              format(8x,'program queen board',20x,
     *                      'opponent queen board'//
     *                          8(1x,i1,1x,8i4,5x,i1,1x,8i4/)/
     *                       3x,'   a   b   c   d   e   f   g   h'
     *                       7x,'   a   b   c   d   e   f   g   h'/)
              else if (atext(4) .eq. alk) then
                  print 2475, (i-1,(pkingb(i*10+j),j=9,2,-1),
     *                         i-1,(okingb(i*10+j),j=9,2,-1),i=9,2,-1)
2475              format(9x,'program king board',21x,
     *                      'opponent king board'//
     *                          8(1x,i1,1x,8i4,5x,i1,1x,8i4/)/
     *                       3x,'   a   b   c   d   e   f   g   h'
     *                       7x,'   a   b   c   d   e   f   g   h'/)
              endif
2465          format(1x,'press return for more....')
c
c------------------------------< scm : modify scoring parameter
c
2490      continue
              if(atext(5) .ne. blank) then
                  col=5
                  parmno=scan(col)
                  col=col+1
                  parmv=scan(col)
                  if(parmv .gt. 200) then
                      print 2491
2491                  format(1x,'no such scoring parameter.')
                      go to 9999
                  endif
                  print 2495, scoring(parmno)
2495              format(1x,'was ',i4)
                  scoring(parmno)=parmv
              endif
              go to 9999
c
c------------------------------< sd : set a specific iteration depth
c
2500  continue
          if(atext(3) .ne. blank) then
              col=4
              if(atext(4) .eq. '+') then
                  col=5
                  fdepth=scan(col)*100
              else
                  fdepth=scan(col)
              endif
              if(fdepth .lt. 60) then
                  print 2510, fdepth
                  write(3,2510) fdepth
2510              format(1x,'search depth set to ',i2)
              else
                  print 2515, hhmmss(fdepth/100,5)
                  write(3,2515) hhmmss(fdepth/100,5)
2515              format(1x,'search depth set to ',a5)
              endif
              if(fdepth .lt. 0) then
                  print 2520
2520              format(1x,'legal range is 0-45.')
                  fdepth=0
              endif
          endif
          go to 9999
c
c------------------------------< sl : set status level
c
2600  continue
          if(atext(3) .ne. blank) then
              col=4
              autos=scan(col)
          endif
          print 2620, autos
2620      format(1x,'status level is',i2,'.')
          go to 9999
c
c-----------------------------< sm : toggle simultaneous mode
c
2700  continue
          if(busy .ne. 0) go to 9998
          smode=xor(smode,1)
          tmode=0
          if(atext(3) .eq. als) autos=3
          if(smode .ne. 0) then
              fdepth=45
              print 2710
              write(3,2710)
2710          format(1x,'simultaneous mode.')
          else
              fdepth=0
          endif
          go to 9999
c
c------------------------------< sn : set minimum nodes for stats
c
2800  continue
          if(atext(3) .ne. blank) then
              col=4
              snodes=scan(col)
          endif
          print 2820, snodes
2820      format(1x,'minimum nodes before stats display is '
     *                    ,i5,'.')
          go to 9999
c
c------------------------------< so : set opening book margin
c
2900  continue
          if(atext(3) .ne. blank) then
              col=4
              margin=scan(col)
          endif
          print 2910, margin
2910      format(1x,'book score margin is',i5,'.')
          if(margin .lt. 0) print 2920
2920      format(1x,'book is now disabled.')
          go to 9999
c
c------------------------------< sp : save current position
c
2950  continue
          if(busy .ne. 0) go to 9998
          call forsythe(text)
          fname=optitoc(atext(4))
          do 2960 ii=2,30
              fname(ii:ii)=optitoc(atext(ii+3))
2960      continue
          open (unit=12,status='new',file=fname,
     *          access='sequential', form='formatted',err=2980)
          write(12,2970) text
2970      format('sb'/80a1)
          close (unit=12)
          go to 9999
2980      continue
              print 2990
2990          format(1x,' -- error --  file already exists')
              go to 9999
c
c------------------------------< sr : set/display ratings
c
3000  continue
          call srcmnd
          go to 9999
c
c------------------------------< st : set a specific time per move
c
3100  continue
          if(atext(3) .ne. blank) then
              col=4
              ftime=scan(col)
              if(ftime .le. 0) then
                  print 3110
3110              format(1x,'time limit may not be zero/negative.')
                  ftime=0
              endif
          endif
          print 3130, hhmmss(ftime,5)
          write(3,3130) hhmmss(ftime,5)
3130      format(1x,'absolute time per move is ',a5,'.')
          go to 9999
c
c------------------------------< sv : set upper search bound
c
3200  continue
          if(atext(3) .ne. blank) then
              col=4
              matem=0
              if(atext(col) .eq. alm) matem=1
              if(atext(col) .eq. alm) col=5
              vterm=scan(col)
              if(matem .ne. 0) then
                  maxply=vterm*2+1
                  vterm=1000000-vterm*2
              endif
          endif
          print 3220, float(vterm)/1000.0
3220      format(1x,'upper search bound is',f9.3)
          go to 9999
c
c------------------------------< tm : toggle tournament mode
c
3300  continue
          if(busy .ne. 0) go to 9998
          tmode=xor(tmode,1)
          smode=0
          if(atext(3) .eq. als) autos=3
          timlim(1)=40
          timlim(2)=120*60
          timlim(3)=20
          timlim(4)=60*60
          timlim(5)=0
          cquery=1
          cputim=0
          surpls=10*60
          margin=0
          cbegin=4*60
          if(tmode .ne. 0) then
              print 3310
              write(3,3310)
3310          format(1x,'tournament mode.')
              open (unit=11,status='old',file='default.tm',
     *              access='sequential', form='formatted',err=3340)
              print 3320
3320          format(1x,'use default.tm file?')
              call command(eof)
              if(atext(1) .ne. aly) then
                  close (unit=11)
                  go to 9999
              endif
              print 3330
3330          format(1x,'using default.tm')
              unit=11
3340          continue
          endif
          go to 9999
c
c------------------------------< tn : set minimum nodes between 
c------------------------------<      splits
c
3350  continue
          if(atext(3) .ne. blank) then
              col=4
              tnodes=scan(col)
          endif
          print 3360, tnodes
3360      format(1x,'minimum nodes between splits is '
     *                    ,i5,'.')
          go to 9999
c
c------------------------------< tr : trace move look ahead
c
3400  continue
          temp=4
          tflag=scan(temp)
          go to 9999
c
c------------------------------< tv : do a "true value" search
c
3450  continue
          trueval=xor(trueval,1)
          if(trueval .eq. 0) print 3451
          if(trueval .ne. 0) print 3452
3451      format(1x,'perform normal alpha/beta search')
3452      format(1x,'perform true valued search')
          go to 9999
c
c------------------------------< vt : set video display type
c
3500  continue
          col=4
          vttype=scan(col)
          vttype=max(vttype,1)
          vttype=min(vttype,3)
          go to 9999
c
c------------------------------< ws : write scoring data to file x
c
3600  continue
          if(busy .ne. 0) go to 9998
          fname=optitoc(atext(4))
          do 3610 ii=2,30
              fname(ii:ii)=optitoc(atext(ii+3))
3610      continue
          close (unit=11)
          open (unit=11,status='unknown',file=fname,
     *          access='sequential', form='formatted')
          write(11,3620) (i,scoring(i),i=1,16)
3620      format(
     * i3,1x,'bonus for two bishops...................',i4/
     * i3,1x,'bishop center tropism...................',i4/
     * i3,1x,'bishop king tropism.....................',i4/
     * i3,1x,'bishops of opposite color (divisor).....',i4/
     * i3,1x,'bishop + wrong rook pawn penalty........',i4/
     * i3,1x,'0 isolated pawn penalty.................',i4/
     * i3,1x,'1 isolated pawn penalty.................',i4/
     * i3,1x,'2 isolated pawn penalty.................',i4/
     * i3,1x,'3 isolated pawn penalty.................',i4/
     * i3,1x,'4 isolated pawn penalty.................',i4/
     * i3,1x,'5 isolated pawn penalty.................',i4/
     * i3,1x,'6 isolated pawn penalty.................',i4/
     * i3,1x,'7 isolated pawn penalty.................',i4/
     * i3,1x,'8 isolated pawn penalty.................',i4/
     * i3,1x,'0 isolated/backward penalty (open file).',i4/
     * i3,1x,'1 isolated/backward penalty (open file).',i4)
          write(11,3621) (i,scoring(i),i=17,32)
3621      format(
     * i3,1x,'2 isolated/backward penalty (open file).',i4/
     * i3,1x,'3 isolated/backward penalty (open file).',i4/
     * i3,1x,'4 isolated/backward penalty (open file).',i4/
     * i3,1x,'5 isolated/backward penalty (open file).',i4/
     * i3,1x,'6 isolated/backward penalty (open file).',i4/
     * i3,1x,'7 isolated/backward penalty (open file).',i4/
     * i3,1x,'8 isolated/backward penalty (open file).',i4/
     * i3,1x,'K and Q pawns not moved penalty.........',i4/
     * i3,1x,'K or Q pawn not moved penalty. .........',i4/
     * i3,1x,'knight center tropism...................',i4/
     * i3,1x,'knight king tropism.....................',i4/
     * i3,1x,'knight on outpost.......................',i4/
     * i3,1x,'opponent pawn on 9th rank...............',i4/
     * i3,1x,'opponent pawn on 8th rank...............',i4/
     * i3,1x,'opponent pawn on 7th rank...............',i4/
     * i3,1x,'opponent pawn on 6th rank...............',i4)
          write(11,3622) (i,scoring(i),i=33,48)
3622      format(
     * i3,1x,'opponent pawn on 5th rank...............',i4/
     * i3,1x,'opponent pawn on 4th rank...............',i4/
     * i3,1x,'opponent pawn on 3rd rank...............',i4/
     * i3,1x,'opponent pawn on 2nd rank...............',i4/
     * i3,1x,'opponent pawn on 1st rank...............',i4/
     * i3,1x,'........................................',i4/
     * i3,1x,'doubled pawn penalty....................',i4/
     * i3,1x,'penalty for having eight pawns..........',i4/
     * i3,1x,'advancing pawn on XX file...............',i4/
     * i3,1x,'advancing pawn on QR file...............',i4/
     * i3,1x,'advancing pawn on QN file...............',i4/
     * i3,1x,'advancing pawn on QB file...............',i4/
     * i3,1x,'advancing pawn on Q  file...............',i4/
     * i3,1x,'advancing pawn on K  file...............',i4/
     * i3,1x,'advancing pawn on KB file...............',i4/
     * i3,1x,'advancing pawn on KN file...............',i4)
          write(11,3623) (i,scoring(i),i=49,64)
3623      format(
     * i3,1x,'advancing pawn on KR file...............',i4/
     * i3,1x,'advancing pawn on XX file...............',i4/
     * i3,1x,'king close to passed pawn bonus.........',i4/
     * i3,1x,'undeveloped piece penalty...............',i4/
     * i3,1x,'opposition with passed pawn bonus.......',i4/
     * i3,1x,'outside passed pawn bonus...............',i4/
     * i3,1x,'passed pawn bonus.......................',i4/
     * i3,1x,'protected passed pawn bonus.............',i4/
     * i3,1x,'passed pawn advancement bonus...........',i4/
     * i3,1x,'queening bonus (scoring a win)..........',i4/
     * i3,1x,'pawn ram penalty........................',i4/
     * i3,1x,'program pawn on 0th rank................',i4/
     * i3,1x,'program pawn on 1st rank................',i4/
     * i3,1x,'program pawn on 2nd rank................',i4/
     * i3,1x,'program pawn on 3rd rank................',i4/
     * i3,1x,'program pawn on 4th rank................',i4)
          write(11,3624) (i,scoring(i),i=65,80)
3624      format(
     * i3,1x,'program pawn on 5th rank................',i4/
     * i3,1x,'program pawn on 6th rank................',i4/
     * i3,1x,'program pawn on 7th rank................',i4/
     * i3,1x,'program pawn on 8th rank................',i4/
     * i3,1x,'tripled pawn penalty....................',i4/
     * i3,1x,'won K+P ending score....................',i4/
     * i3,1x,'queen center tropism....................',i4/
     * i3,1x,'moving queen too early penalty..........',i4/
     * i3,1x,'queen king tropism......................',i4/
     * i3,1x,'rook behind passed pawn bonus...........',i4/
     * i3,1x,'rook on half-open file bonus............',i4/
     * i3,1x,'rook king tropism.......................',i4/
     * i3,1x,'rook on 7th rank bonus..................',i4/
     * i3,1x,'rook on open file bonus.................',i4/
     * i3,1x,'rook pawn may draw penalty..............',i4/
     * i3,1x,'rook on a bad square....................',i4)
          write(11,3625) (i,scoring(i),i=81,96)
3625      format(
     * i3,1x,'rook trapped (no horiz. mobility).......',i4/
     * i3,1x,'king to center of pawns (endgame).......',i4/
     * i3,1x,'king holes around king location.........',i4/
     * i3,1x,'king tropism to opponents king..........',i4/
     * i3,1x,'king luft bonus.........................',i4/
     * i3,1x,'king lost castling ability penalty......',i4/
     * i3,1x,'king lost castling king-side penalty....',i4/
     * i3,1x,'king too many pawns moved around king...',i4/
     * i3,1x,'king attack on XX file..................',i4/
     * i3,1x,'king attack on KR file..................',i4/
     * i3,1x,'king attack on KN file..................',i4/
     * i3,1x,'king attack on KB file..................',i4/
     * i3,1x,'king attack on K  file..................',i4/
     * i3,1x,'king attack on Q  file..................',i4/
     * i3,1x,'king attack on QB file..................',i4/
     * i3,1x,'king attack on QN file..................',i4/)
          write(11,3626) (i,scoring(i),i=97,112)
3626      format(
     * i3,1x,'king attack on QR file..................',i4/
     * i3,1x,'king attack on XX file..................',i4/
     * i3,1x,'king safety on XX file..................',i4/
     * i3,1x,'king safety on KR file..................',i4/
     * i3,1x,'king safety on KN file..................',i4/
     * i3,1x,'king safety on KB file..................',i4/
     * i3,1x,'king safety on K  file..................',i4/
     * i3,1x,'king safety on Q  file..................',i4/
     * i3,1x,'king safety on QB file..................',i4/
     * i3,1x,'king safety on QN file..................',i4/
     * i3,1x,'king safety on QR file..................',i4/
     * i3,1x,'king safety on XX file..................',i4/
     * i3,1x,'blocked center pawn penalty.............',i4/
     * i3,1x,'trade bonus/penalty.....................',i4/
     * i3,1x,'trade pawn window size..................',i4/
     * i3,1x,'bking blockade isolated passer..........',i4)
          write(11,3627) (i,scoring(i),i=113,128)
3627      format(
     * i3,1x,'bqueen blockade isolated passer.........',i4/
     * i3,1x,'brook blockade isolated passer..........',i4/
     * i3,1x,'bbishop blockade isolated passer........',i4/
     * i3,1x,'bknight blockade isolated passer........',i4/
     * i3,1x,'unused..................................',i4/
     * i3,1x,'unused..................................',i4/
     * i3,1x,'unused..................................',i4/
     * i3,1x,'wknight blockade isolated passer........',i4/
     * i3,1x,'wbishop blockade isolated passer........',i4/
     * i3,1x,'wrook   blockade isolated passer........',i4/
     * i3,1x,'wqueen  blockade isolated passer........',i4/
     * i3,1x,'wking   blockade isolated passer........',i4/
     * i3,1x,'bking   blockade passer.................',i4/
     * i3,1x,'bqueen  blockade passer.................',i4/
     * i3,1x,'brook   blockade passer.................',i4/
     * i3,1x,'bbishop blockade passer.................',i4)
          write(11,3628) (i,scoring(i),i=129,145)
3628      format(
     * i3,1x,'bknight blockade passer.................',i4/
     * i3,1x,'unused..................................',i4/
     * i3,1x,'unused..................................',i4/
     * i3,1x,'unused..................................',i4/
     * i3,1x,'wknight blockade passer.................',i4/
     * i3,1x,'wbishop blockade passer.................',i4/
     * i3,1x,'wrook   blockade passer.................',i4/
     * i3,1x,'wqueen  blockade passer.................',i4/
     * i3,1x,'wking   blockade passer.................',i4/
     * i3,1x,'attack weak pawn........................',i4/
     * 7(i3,t45,i4/))
          close (unit=11)
          go to 9999
c
c------------------------------< end : terminate play
c
3700  continue
          if(busy .ne. 0) go to 9998
          call hcmnd(1)
          do 3650 i=2,ncpus
              stop(i)=-9999
3650      continue
          call exit
          stop
c
c------------------------------< return, command can not be
c------------------------------< executed while search busy
c
9998  continue
          return=2
          return
c
c------------------------------< return, command executed
c
9999  continue
          return=1
          return
      end
      integer function optlog2(number)
      implicit integer (a-z)
      do 100 log2=1,30
          if(2**log2 .ge. number) go to 200
100   continue
200   continue
      optlog2=log2
      return
      end
      character*1 function optitoc(char)
      implicit integer (a-z)
      character*1 char
      optitoc=char
      return
      end
