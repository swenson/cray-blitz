      subroutine stats
crev  last revision 09/29/91
c
c     ******************************************************************
c     *                                                                *
c     *      stats is used to inform the operator of the current       *
c     *  search status in response to the 'break' or 'attn' key being  *
c     *  hit on the terminal.  the format of the display is:           *
c     *                                                                *
c     *               depth:  3/ 4 time:    :13                        *
c     *               best:    -.026  Ng8-f6                           *
c     *               1. Nxe4  2. Bxe4  3. Qxe4  4. Rxf7  5. Kxf7      *
c     *                        2. Nc6   3. d4    4. exd4  5. Nxd4      *
c     *                                          4. Rxd4  5. Nxd4      *
c     *                        2. f5    3. exf5  4. Nxf5               *
c     *                        2. f6    3. d4    4. o-o   5. o-o       *
c     *               1. o-o   2. o-o   3. d4    4. exd4  5. Nxd4      *
c     *                                                                *
c     *      the first two lines are self-explanatory.  the other      *
c     *  lines come from each processor actively searching some branch *
c     *  of the tree.                                                  *
c     *                                                                *
c     *      if 'ponder' is true, it indicates that the program is     *
c     *  'thinking' on the opponent's time.  a break here indicates    *
c     *  that a move is ready.  if the operator types 'ok', then the   *
c     *  predicted move was made and the search will continue.  if     *
c     *  anything else is typed, the current search is aborted and     *
c     *  control returns to main.                                      *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
c
c
      include 'perm.f'
c
c
      include 'global.f'
c
c
      include 'common.f'
c
c
      integer cpmvs(50,16), used(16), 
     * outmvs(50,16), scurmvs(50,16)
      equivalence (blank,alpha(70)), (alphao,alpha(41)),
     *            (alphak,alpha(37)),(quest,alpha(69)),
     *            (equal,alpha(67)),(lbrack,alpha(73)),
     *            (rbrack,alpha(74)),(plus,alpha(63)),
     *            (minus,alpha(64)),(smallx,alpha(50)),
     *            (dot,alpha(68))
c
c
c
c
c------------------------------< if thinking on opponent's time, break
c------------------------------< means read in a move or command.
c
      if(broke .eq. 0) return
c     call lock22
      if(broke .eq. 0) go to 9999
      broke=0
      if(tmode .eq. 0) go to 4500
      if(.not. (pndrng.ne.0 .and. matchd.eq.0)) go to 4500
          if(playslf .eq. 0) then
              call command(eof)
          else
c             call piperd(playfp(2),atext)
          endif
          if(atext(1).eq.blank .or. atext(1).eq.dot) go to 1200
          call cptime(psec2,msec2)
          if(cputim .ne. 0) psec2=msec2
          tavgtim=avgtim
          if(easy .ne. 0) tavgtim=tavgtim/3
          rt=max(tavgtim-(psec2-fsec1),0)/100
          call options(1)
          if(return .ne. 0) then
              if(return .eq. 2) abort=1
              go to 9998
          endif
          tp=player
          player=2
          ts=side
          side=-1
          call inputx(predmv)
          player=tp
          side=ts
          if(return .eq. 1) then
              print 300, bell
300           format(1x,'that''s ambiguous',a1)
              go to 9998
          endif
          if(return .eq. 2) then
              print 400, bell
400           format(1x,'illegal move',a1)
              go to 9998
          endif
          if(return .eq. 3) then
              print 500, bell
500           format(1x,'unrecognizable move',a1)
              go to 9998
          endif
          if(mtype .eq. prevmv(6)) then
              if((mtype.eq.castkg .or. mtype.eq.castqn) .or.
     *           (mfrom.eq.prevmv(4) .and. mto.eq.prevmv(5))) then
                  do 600 i=1,15
                      atext(i)=ptext(i)
600               continue
                  call setclk
                  matchd=1
                  call cptime(psec1,msec1)
                  osec2=psec1
                  if(cputim .ne. 0) psec1=msec1
                  print 700, hhmmss(rt,5)
                  write(3,700) hhmmss(rt,5)
700               format(1x,'ok.  (done in ',a5,')')
                  go to 9998
              endif
          endif
          print 800
          write(3,800)
800       format(1x,'ok.  (not predicted)')
          abort=1
          go to 9998
c
c------------------------------< prepare to display the best move and
c------------------------------< score along with the search time.
c
1200  continue
          call cptime(psec2,msec2)
          if(cputim .ne. 0) psec2=msec2
          e=(psec2-psec1)/100
          print 1300, depth, rdepth, hhmmss(e,5)
1300      format(/1x,'depth:',i3,'/',i2,1x,'time:',a5)
c
c------------------------------< decode and output the current
c------------------------------< best move found so far.
c
          if(which(1) .lt. first(1)) go to 9999
          if(which(1) .le. last(1)) then
              mfrom=extrct(trace(1,1))
              call output(0,2)
              v1=value(1)
              if(v1.ge.100 .and. v1.lt.900000) v1=v1-100
              print 1400,float(v1)/1000.0,(text(l),l=1,30)
1400          format(1x,'best:',f9.3,2x,30a1)
          endif
c
c------------------------------< first, find the processor with the
c------------------------------< deepest variation and copy its moves
c------------------------------< to the first variation.
c
          do 1450 i=1,16
              do 1425 j=1,50
                  if(working(i) .ne. 0) then
                      scurmvs(j,i)=curmvs(j,i)
                  else
                      scurmvs(j,i)=0
                  endif
1425          continue
1450      continue
          do 1600 i=1,ncpus
              used(i)=0
              do 1500 j=1,50
                  outmvs(j,i)=0
                  cpmvs(j,i)=0
1500          continue
1600      continue
          maxd=0
          maxp=0
          line=0
          do 1800 iproc=1,ncpus
              do 1700 idepth=1,50
                  if(scurmvs(idepth,iproc) .eq. 0) then
                      if(maxd .lt. idepth-1) then
                          maxp=iproc
                          maxd=idepth-1
                      endif
                      go to 1800
                  endif
1700          continue
1800      continue
c
c------------------------------< now, find the variation that matches
c------------------------------< the last variation copied for the most
c------------------------------< consecutive moves.  this is the one
c------------------------------< to copy next.  repeat this process
c------------------------------< until all variations have been copied.
c
1900      continue
              used(maxp)=1
              line=line+1
              izero=0
              do 2000 i=1,50
                  if(scurmvs(i,maxp) .eq. 0) izero=1
                  if(izero .eq. 0) cpmvs(i,line)=scurmvs(i,maxp)
2000          continue
              maxp=0
              maxd=0
              do 2200 iproc=1,ncpus
                  if(used(iproc) .eq. 0) then
                      do 2100 i=1,50
                          if(cpmvs(i,line) .ne. scurmvs(i,iproc)) then
                              if(i-1 .ge. maxd) then
                                  maxd=i-1
                                  maxp=iproc
                              endif
                              go to 2200
                          endif
2100                  continue
                  endif
2200          continue
              if(maxp .ne. 0) go to 1900
c
c------------------------------< now convert the moves to text for
c------------------------------< output.  convert the entire first
c------------------------------< line, then only convert the trailing
c------------------------------< part of each variation that does not
c------------------------------< match the preceeding line.
c
              do 2400 i=1,50
                  if(cpmvs(i,1) .ne. 0) then
                      mfrom=extrct(cpmvs(i,1))
                      call output(0,2)
                      do 2300 k=1,4
cseq                      outmvs(i,1)=outmvs(i,1)+
cseq *                                ishft(and(text(k),255),(k-1)*8)
                          outmvs(i,1)=outmvs(i,1)+
     *                                ishft(ishft(text(k),-56),(8-k)*8)
2300                  continue
                  endif
2400          continue
              do 3000 j=2,ncpus
                  do 2600 ilim=1,50
                      if(cpmvs(ilim,j-1) .ne. cpmvs(ilim,j)) go to 2700
2600              continue
2700              continue
                  do 2900 i=ilim,50
                      if(cpmvs(i,j) .ne. 0) then
                          mfrom=extrct(cpmvs(i,j))
                          call output(0,2)
                          do 2800 k=1,4
cseq                          outmvs(i,j)=outmvs(i,j)+
cseq *                                ishft(and(text(k),255),(k-1)*8)
                              outmvs(i,j)=outmvs(i,j)+
     *                                ishft(ishft(text(k),-56),(8-k)*8)
2800                      continue
                      endif
2900              continue
3000          continue
c
c------------------------------< now, output each processor's 
c------------------------------< current variation, starting with
c------------------------------< the move where two processors
c------------------------------< diverge in the search tree.
c
              if(atext(1) .eq. dot) then
                  do 3700 i=1,ncpus
                      line=0
                      do 3100 strt=1,50
                          if(outmvs(strt,i) .ne. 0) go to 3150
3100                  continue
3150                  continue
                      if(strt .le. 9) then
                          offset=strt-1
                      else
                          offset=mod(strt-9,8)
                          if(offset .eq. 0) offset=8
                      endif
3200                  continue
                      lend=min0(strt+8-offset-line,50)
                      do 3300 lend=lend,strt,-1
                          if(outmvs(lend,i) .ne. 0) go to 3400
3300                  continue
                      go to 3700
3400                  continue
                      if(line .eq. 0) then
                          if(offset .eq. 0)
     *                        print 3501, (j,outmvs(j,i),j=strt,lend)
                          if(offset .eq. 1)
     *                        print 3502, (j,outmvs(j,i),j=strt,lend)
                          if(offset .eq. 2)
     *                        print 3503, (j,outmvs(j,i),j=strt,lend)
                          if(offset .eq. 3)
     *                        print 3504, (j,outmvs(j,i),j=strt,lend)
                          if(offset .eq. 4)
     *                        print 3505, (j,outmvs(j,i),j=strt,lend)
                          if(offset .eq. 5)
     *                        print 3506, (j,outmvs(j,i),j=strt,lend)
                          if(offset .eq. 6)
     *                        print 3507, (j,outmvs(j,i),j=strt,lend)
                          if(offset .eq. 7)
     *                        print 3508, (j,outmvs(j,i),j=strt,lend)
                          if(offset .eq. 8)
     *                        print 3509, (j,outmvs(j,i),j=strt,lend)
3501                      format(1x,9(i2,1x,a4,1x))
3502                      format(9x,8(i2,1x,a4,1x))
3503                      format(17x,7(i2,1x,a4,1x))
3504                      format(25x,6(i2,1x,a4,1x))
3505                      format(33x,5(i2,1x,a4,1x))
3506                      format(41x,4(i2,1x,a4,1x))
3507                      format(49x,3(i2,1x,a4,1x))
3508                      format(57x,2(i2,1x,a4,1x))
3509                      format(65x,1(i2,1x,a4,1x))
                          line=1
                          strt=lend+1
                      else
                          print 3600, (j, outmvs(j,i), j=strt,lend)
3600                      format(9x,8(i2,1x,a4,1x))
                          strt=strt+8
                      endif
                      if(lend .lt. 50) go to 3200
3700              continue
              endif
              done=0
              do 3800 i=1,last(1)
                  if(tried(i) .ne. 0) done=done+1
3800          continue
              print 3900, 1, last(1)-done, last(1)
3900          format(1x,'remaining - ',i2,' :(',i2,' of ',i2,')  ',$)
              do 4400 lv=2,ply
                  sp='*'
                  do 4000 bl=1,16
                      if(atply(bl) .eq. lv) go to 4100
4000              continue
                  sp=' '
4100              continue
                      remain=0
                      do 4200 i=first(lv),last(lv)
                          if(moves(i) .ne. 0) remain=remain+1
4200                  continue
                      print 4300, lv, sp, remain, last(lv)-first(lv)
4300                  format(1x,i2,a1,':(',i2,' of ',i2,')  ',$)
                      if(mod(lv,4) .eq. 0) then
                          print 4450
                          print 4451
                      endif
4400          continue
              print 4450
4450          format(1x)
4451          format(1x,'           ',$)
              go to 9999
c
c------------------------------< program is in simultaneous mode,
c------------------------------< ask the operator if it is time to
c------------------------------< make a move.
c
4500  continue
          call command(eof)
          if(atext(1).eq.blank .or. atext(1).eq.dot) go to 1200
          if(atext(1) .ne. quest) then
              call options(1)
              if(return .eq. 1) go to 9998
              print 4600
4600          format(1x,'command not legal now.')
              go to 9998
          endif
          if(depth .eq. 1) then
              if(which(1) .eq. 1) go to 9999
          endif
          abort=1
9998  continue
          broke=0
9999  continue
c         call unlock22
          return
      end
