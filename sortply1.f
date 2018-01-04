      subroutine sortply1
crev  last revision 02/23/91
c
c     ******************************************************************
c     *                                                                *
c     *      sortply1 is used to set up the ply one move list.  it     *
c     *  does a "true value search" for a couple of plies to get a     *
c     *  score for each ply-1 move.  it then uses these scores to sort *
c     *  the move list.                                                *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      real rvalue, svalue, tvalue
c
c
      include 'global.f'
c
c
      include 'common.f'
c
c
c------------------------------< set up and call 'movgen' to generate
c------------------------------< the ply one move list.
c
      first(1)=1
      ply=1
      player=1
      side=1
      rdepth=ply+1
      inchk(1)=attack(-1,kloc(1))
      givchk(1)=0
      moveds(1)=movedr(1)
      call movgen
      if(return .ne. 0) return
      do 100 where=1,last(1)
          mfrom=extrct(moves(where))
          if(checki(1) .ne. 0) moves(where)=0
100   continue
      lastmv=0
      do 200 where=1,last(1)
          if(moves(where) .ne. 0) then
              lastmv=lastmv+1
              moves(lastmv)=moves(where)
          endif
200   continue
      last(1)=lastmv
      avg=0
c
c------------------------------< copy the ply-1 move list into
c------------------------------< ponmvs so that backup can add
c------------------------------< the score and expected move.
c
      do 400 i=1,200
          if(i .le. last(1)) then
              ponmvs(1,i)=moves(i)
          else
              ponmvs(1,i)=0
          endif
          do 300 j=2,7
              ponmvs(j,i)=0
300       continue
400   continue
c
c------------------------------< determine how much time to spend on
c------------------------------< ordering the ply one move list.
c------------------------------< basically:  if the target time for
c------------------------------< the search is 20 seconds or more,
c------------------------------< use 2 seconds for the true valued
c------------------------------< search;  if the target time is less
c------------------------------< than 20 seconds, use 1/100th of the
c------------------------------< target time.
c
      if(timlim(5) .eq. 0) then
          targtim=(timlim(2)-pelap-surpls)/max(timlim(1)-npmovs,1)
      else
          if(pelap .lt. timlim(2)-3*timlim(1)) then
              targtim=timlim(1)
          elseif(pelap .lt. timlim(2)+timlim(4)-5*timlim(3)) then
              targtim=timlim(3)
          else
              targtim=timlim(5)
          endif
      endif
      if(ftime .ne. 0) targtim=ftime
      if(targtim .ge. 20) then
          avgtim=200
      else
          avgtim=targtim
      endif
c
c------------------------------< now call iterate to do a "true
c------------------------------< value search" for a second or
c------------------------------< so to order the moves.
c
      temp=snodes
      temp1=trueval
      temp2=fdepth
      if(snodes .ge. 0) snodes=100000000
      trueval=1
      fdepth=0
      depth=0
      sortp1=1
      call iterate
      sortp1=0
      snodes=temp
      trueval=temp1
      fdepth=temp2
      depth=depth-1
c
c------------------------------< now, make each move and call
c------------------------------< score to compute a static score
c------------------------------< for the move.  this score will
c------------------------------< be used as a "tie breaker" when
c------------------------------< the true-value search returns
c------------------------------< the same score for different
c------------------------------< ply one moves.
c
      do 500 i=1,last(1)
          mfrom=extrct(ponmvs(1,i))
          call make
          call score
          ponmvs(6,i)=tscore-ripoff(mto)
          ponmvs(7,i)=pieces(mcappc)-ripoff(mto)
          call unmake
500   continue
c
c------------------------------< now sort the moves based on the
c------------------------------< preliminary scoring from above.
c
600   continue
          done=1
          do 800 where=1,last(1)-1
              if(ponmvs(5,where).lt.ponmvs(5,where+1) .or.
     *           (ponmvs(5,where).eq.ponmvs(5,where+1).and.
     *            ponmvs(6,where).lt.ponmvs(6,where+1))) then
                  do 700 i=1,7
                      temp=ponmvs(i,where)
                      ponmvs(i,where)=ponmvs(i,where+1)
                      ponmvs(i,where+1)=temp
700               continue
                  done=0
              endif
800       continue
      if(done .eq. 0) go to 600
c
c------------------------------< now copy the saved moves into
c------------------------------< the ply-1 move list in true
c------------------------------< value search order.
c
      do 900 i=1,last(1)
          moves(i)=ponmvs(1,i)
900   continue
c
c------------------------------< display the ply one move list
c------------------------------< if requested.
c
      if(iand(debug,1) .ne. 0) then
          print 1000
1000      format(/'                  --------score--------'
     *           /1x,'order move     dynamic static ripoff ',
     *               ' depth   vlen')
          do 1200 i=1,last(1)
              mfrom=extrct(ponmvs(1,i))
              side=1
              player=1 
              call output(0,1)
              v1=ponmvs(5,i)
              if(v1.ge.100 .and. v1.le.900000) v1=v1-100
              rvalue=float(v1)/1000.0
              svalue=float(ponmvs(6,i))/1000.0
              tvalue=float(ponmvs(7,i))/1000.0
              vlen=and(ponmvs(3,i),65535)
              print 1100, i, (text(j),j=1,7),rvalue,svalue,
     *                    tvalue,ponmvs(4,i),vlen
1100          format(2x,i3,2x,7a1,2x,3f7.3,2i7)
1200      continue
      endif
c
c------------------------------< check for an easy reply.
c------------------------------< if one move has a score clearly
c------------------------------< better than the rest, and the
c------------------------------< material score indicates that
c------------------------------< the move is a re-capture (not
c------------------------------< winning material), then we can
c------------------------------< make the move a little faster.
c
      easy=0
      if(ponmvs(5,1) .gt. ponmvs(5,2)+750) then
          mfrom=extrct(moves(1))
          if((ponmvs(5,1).lt.peval+500 .and. 
     *        ponmvs(5,1).gt.peval-500) .and. mcappc.ne.0) easy=1
      endif
      return
      end
