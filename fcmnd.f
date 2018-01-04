      subroutine fcmnd
crev  last revision 12/09/93
c
c     ******************************************************************
c     *                                                                *
c     *      fcmnd is used to force blitz to make a specific move      *
c     *  that the normal search or book search would not choose.  it   *
c     *  prompts the operator for the move to make and if it is legal  *
c     *  will actually make it on the game board as though blitz had   *
c     *  chosen it all along.  the move actually last made by blitz    *
c     *  will naturally be retracted since a new move is being made.   *
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
      dimension digits(10)
      equivalence (digits(1),alpha(53))
      equivalence (blank,alpha(70))
c
c
c
c
c------------------------------< input new move to make
c
100   continue
          print 200
          write(3,200)
200       format(1x,'enter my move.')
          call command(eof)
          if(eof .ne. 0) return
          write(3,300) atext
300       format(1x,80a1)
          if(atext(1) .eq. blank) return
          inbook=1
          over=0
          foundm=0
          trace(51,1)=0
c
c------------------------------< if program has already made a move,
c------------------------------< it must be 'unmade'
c
          ply=1
          player=1
          side=1
          mfrom=lmovep
          if(mfrom .ne. 0) then
              mfrom=extrct(mfrom)
              cappc(1)=-mcappc
              call pumver
              lmovep=0
          endif
          pmove=0
c
c------------------------------< convert and make the new move
c
          call input(1)
      if(return .ne. 0) go to 100
      otype=0
      ctemp=checkg(1)
      if(ctemp .ne. 0) otype=1
      call output(otype,1)
      lmovep=mfrom+ishft(mto,7)+ishft(mtype,14)
     *       +ishft(movgpc,20)+ishft(mcappc,17)
      prevmv(1)=mfrom
      prevmv(2)=mto
      prevmv(3)=mtype
      call pmover
      ply=0
      if(point.eq.0 .or. point.eq.1) point=pointsv
      if(attack(-1,kloc(1)) .ne. 0) go to 700
      nmoves=npmovs+nomovs
      if(color .eq. 1)
     *     write(3,500) (npmovs-1+nomovs)/2+1, (text(l),l=1,10)
      if(color .eq. 2)
     *     write(3,600) (npmovs-1+nomovs)/2+1, (text(l),l=1,10)
500   format(1x,'my move  ',i3,'. ',10a1)
600   format(1x,'my move  ',i3,'. ...  ',10a1)
      write(unit=1,fmt=601,rec=nmoves+1) (text(l),l=1,30), 0,0,0,0
601   format(30a1,4i6)
      atext(1)=blank
      atext(2)=blank
      hd=(nomovs+1)/100
      td=((nomovs+1)-hd*100)/10
      ud=mod(nomovs+1,10)
      atext(3)=digits(hd+1)
      atext(4)=digits(td+1)
      atext(5)=digits(ud+1)
      atext(6)=blank
      spmove=pmove
      call rcmnd
      pmove=spmove
      return
c
c------------------------------< can't make a move that leaves
c------------------------------< the king in check
c
700   continue
          print 800
800       format(1x,'my king is in check.')
          go to 100
      end
