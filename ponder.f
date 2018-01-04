      subroutine ponder
crev  last revision 10/05/90
c
c     ******************************************************************
c     *                                                                *
c     *      ponder is the driver for the think on the opponent's time *
c     *  algorithm.  briefly, ponder takes the predicted move from the *
c     *  previous search and makes it.  then driver is called to find  *
c     *  a response to that move.  this response is saved; after the   *
c     *  opponent's move is entered, match determines if the move is   *
c     *  the same as the expected move.  if so, the response is ready  *
c     *  and no search is required.  when the program is pondering in  *
c     *  this mode, it is necessary to hit the break key before a move *
c     *  can be entered since the program ponders continuously and     *
c     *  does not hang a read to the terminal.  when the break key is  *
c     *  hit, two cases arise:                                         *
c     *                                                                *
c     *      1)  if a command or move is entered, the entire search    *
c     *          must be aborted to get back to main to interpret the  *
c     *          command (some commands don't have this restriction);  *
c     *                                                                *
c     *      2)  the characters 'ok' can be entered which indicated    *
c     *          that the opponent has moved and the move matches the  *
c     *          expected or predicted move.  in this, case the        *
c     *          opponent's clock is stopped, the program's clock is   *
c     *          started and the search continues without interruption *
c     *          saving the time used already.  the move can be        *
c     *          entered exactly as printed in the 'pondering a        *
c     *          reply to...' message with the same results.           *
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
      equivalence (alphao,alpha(41)),(colon,alpha(71)),
     *            (dash,alpha(64)),(equal,alpha(67))
c
c
c------------------------------< if still in the book or not in
c------------------------------< tournament mode, return. if there is
c------------------------------< no predicted move or ponder has already
c------------------------------< found a move, return also.
c
      spm4=prevmv(4)
      spm5=prevmv(5)
      spm6=prevmv(6)
      if(foundm.ne.0 .or. pmove.eq.0 .or.tmode.eq.0) go to 9999
c
c------------------------------< remember predicted move
c
      mfrom=extrct(pmove)
      prevmv(4)=mfrom
      prevmv(5)=mto
      prevmv(6)=mtype
      pmtype=mtype
      pmto=mto
      pmfrom=mfrom
c
c------------------------------< if pondering, print the move we
c------------------------------< are expecting (if not playing a
c------------------------------< fast game).
c
      ply=2
      side=-1
      player=2
      otype=0
      ctemp=checkg(2)
      if(ctemp .ne. 0) otype=1
      call output(otype,1)
      first(2)=1
      last(2)=0
      rdepth=2
      from(1)=prevmv(1)
      to(1)=prevmv(2)
      call movgen
c
c------------------------------< eliminate all illegal moves
c
      do 200 i=1,last(2)
          predmv(i)=moves(i) 
          mfrom=extrctf(predmv(i))
          ctemp=checki(player)
          if(ctemp .ne. 0) predmv(i)=0
          predmv(i+200)=checkg(player)
200   continue
      mfrom=extrct(pmove)
      predmv(200)=last(2)
      if(autos .gt. 1) then
          if(color .eq. 1) print 300,
     *              (npmovs+nomovs)/2+1,(text(l),l=1,10)
          if(color .eq. 2) print 400,
     *              (npmovs+nomovs)/2+1,(text(i),i=1,10)
      endif
      if(color .eq. 1) write(3,300)
     *              (npmovs+nomovs)/2+1,(text(i),i=1,10)
      if(color .eq. 2) write(3,400)
     *              (npmovs+nomovs)/2+1,(text(i),i=1,10)
300   format(1x,'pondering',i3,'.',' ...  ',10a1)
400   format(1x,'pondering',i3,'.',' ',10a1)
      do 450 i=1,30
          ptext(i)=text(i)
450   continue
      call savegb
      call pmover
c
c------------------------------< actual pondering starts here.
c
      nomovs=nomovs+1
      ply=0
      point=point+1
      rtemp=repeat(count)
      pndrng=1
      call driver
      point=point-1
      nomovs=nomovs-1
      if((abort.eq.0 .and. timeup.eq.0) .or. matchd.ne.0) then
          call cptime(psec2,msec2)
          if(cputim .ne. 0) psec2=msec2
          foundm=1
          mfrom=extrct(pmove)
          ply=2
          player=2
          side=-1
          cappc(ply)=mcappc
          call pumver
          pndrng=0
          if(matchd .ne. 0) go to 9998
          go to 9999
      endif
c
c------------------------------< operator hit break and did not
c------------------------------< type in a fully qualified move
c------------------------------< (or he typed in a command) so
c------------------------------< abort the look-ahead and see
c------------------------------< what he has on his mind.
c
      foundm=0
      pndrng=0
      mfrom=extrct(pmove)
      ply=2
      player=2
      side=-1
      cappc(ply)=mcappc
      call pumver
      prevmv(4)=spm4
      prevmv(5)=spm5
      prevmv(6)=spm6
      return=1
      return
c
c------------------------------< the operator typed 'ok' which indicates
c------------------------------< that the program correctly predicted
c------------------------------< the opponent's reply. set the input
c------------------------------< buffer so that the operator won't have
c------------------------------< to enter the move.
c
9998  continue
          mfrom=extrct(pmove)
          do 800 i=1,30
              atext(i)=ptext(i)
800       continue
          return=2
          return
c
c------------------------------< no pondering is possible since
c------------------------------< there is no predicted best move.
c
9999  continue
          prevmv(4)=spm4
          prevmv(5)=spm5
          prevmv(6)=spm6
          return=0
          return
      end
