      subroutine rcmnd
crev  last revision 12/09/90
c
c     ******************************************************************
c     *                                                                *
c     *      rcmnd is used to process the 'r' command which can reset  *
c     *  the board to the position after any prior move.  the board    *
c     *  is always reset to the position for the move indicated, so    *
c     *  that it is the opponent's turn to move.  for example, if the  *
c     *  computer is white, and you wish to back up to move 15, the    *
c     *  move for black listed under move 15 would not be made.        *
c     *      'rk' is an alternative way to set the board to some       *
c     *  position.  in this mode, you can key moves in and have them   *
c     *  made as you go.  you can terminate the moves with an end of   *
c     *  file, or by typing in the number of moves you indicated       *
c     *  would be entered.                                             *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
c
c
      include 'global.f'
c
c
      include 'common.f'
c
c
      equivalence (alphak,alpha(37)),(alphay,alpha(51)),
     *            (blank,alpha(70)),(alphad,alpha(30)),
     *            (period,alpha(68))
c
c
c
c
c------------------------------< initialize
c
      autors=0
      if(atext(1) .eq. blank) autors=1
      keyin=0
      if(atext(2) .eq. alphak) keyin=1
      temp=3
      if(atext(3) .eq. blank) temp=4
      moveno=scan(temp)
      if(moveno .eq. 0) return
      foundm=0
      over=0
      trace(51,1)=0
      player=1
      lmovep=0
      if(color .eq. 1) player=2
      side=1
      if(player .eq. 2) side=-1
      point=0
      pelap=0
      oelap=0
c
c------------------------------< determine exactly how many moves
c------------------------------< to process.  leave the opponent's
c------------------------------< last move out so it will be his turn.
c
      npmovs=moveno-1
      nomovs=moveno-1
      if(color .eq. 1) npmovs=npmovs+1
      moveno=(moveno-1)*2
      if(color .eq. 1) moveno=moveno+1
      inbook=1
      call setgb(0)
      count=0
      read(unit=1,rec=1,fmt=50) atext
50    format(80a1)
      call sbcmnd(0)
c
c------------------------------< now, either read the move file or
c------------------------------< from the user the list of moves
c------------------------------< to make.
c
100   continue
          player=3-player
          ply=player
          side=-side
          count=count+1
          time=0
          if(count .gt. moveno) go to 600
200       continue
          if(keyin .eq. 0) then
              read(unit=1,fmt=201,rec=count+1)(atext(ll),ll=1,30),time
201           format(30a1,i6)
          else
              if(and(count,1) .ne. 0) then
                  n=count/2+1
                  print 300, n
300               format(1x,'move',i3,'  (d to display)')
              endif
              call command(eof)
              if(eof .ne. 0) go to 600
          endif
c
c------------------------------< decode the move and make it on
c------------------------------< the board.
c
          if(side .ge. 0) pelap=pelap+time
          if(side .lt. 0) oelap=oelap+time
          if(atext(1).eq.alphad .and. atext(2).eq.blank) then
              call dcmnd
              go to 200
          endif
          if(atext(1) .eq. period) go to 100
          call input(1)
          if(return .ne. 0) go to 200
          prevmv(1+(player-1)*3)=mfrom
          prevmv(2+(player-1)*3)=mto
          prevmv(3+(player-1)*3)=mtype
          call pmover
          if(keyin .ne. 0) then
              write(unit=1,fmt=501,rec=count+1) (atext(ll),ll=1,30),
     *                                    0,0,0,0
501           format(30a1,4i6)
          endif
          ply=0
          point=point+1
          rtemp=repeat(dummy)
          lmoveo=lmovep
          lmovep=mfrom+ishft(mto,7)+ishft(mtype,14)
     *          +ishft(movgpc,20)+ishft(mcappc,17)
      go to 100
600   continue
          movedr(1)=moveds(1)
          movedr(2)=moveds(2)
          pmove=0
          if(autors .eq. 0) then
              print 700
700           format(1x,'first move out of book was ...')
              call command(eof)
              if(eof .ne. 0) go to 600
              col=1
              numout=scan(col)
          endif
          newmem=3*(hsize+7168)+3*(psize/2)+2*ksize
          do 750 i=1,newmem
              htable(i)=0
750       continue
          return
      end
