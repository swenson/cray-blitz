      subroutine backup
crev  last revision 06/21/90
c
c     ******************************************************************
c     *                                                                *
c     *      backup is called to backup the best score/variation       *
c     *  that has been found below this node.  when backing up to      *
c     *  ply one, the current move at ply one is forced to the         *
c     *  top of the move list and others are moved down one position   *
c     *  so that the next search iteration will have a more accurate   *
c     *  move list order to help increase the number of alpha/beta     *
c     *  cutoffs that occur.                                           *
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
c------------------------------< thanks to the parallel search, make
c------------------------------< sure that we always back up a better
c------------------------------< value than that found so far.  unsplit
c------------------------------< can slip in a new value just as we get
c------------------------------< ready to back up, if we don't check
c------------------------------< for it, it will get overwritten and
c------------------------------< will be lost.
c
      if(and(ply,1) .eq. 1) then
          if(value(ply) .gt. value(ply-1)) return
      else
          if(value(ply) .lt. value(ply-1)) return
      endif
c
c------------------------------< if backing up to ply one, a special
c------------------------------< case is handled because of the
c------------------------------< necessity of move reordering.
c
      if(ply .ne. 2) then
c
c------------------------------< don't back up if a null move was made
c------------------------------< at the previous ply because a null move
c------------------------------< cannot be best legally.
c
          if(trace(ply-1,ply) .eq. 16383) then
              w1=value(ply-3+player)
              w2=value(ply-player)
              if(value(ply).gt.w1 .and. value(ply).lt.w2) return
          endif
c
c------------------------------< first, backup the best score found by
c------------------------------< examining the tree below this node.
c
          value(ply-1)=value(ply)
c
c------------------------------< now, backup the principle variation
c------------------------------< that leads to the backed up score.
c
          if(value(ply) .ne. window(player)) then
              lim=and(trace(51,ply),65535)
              do 100 level=1,lim
                  trace(level,ply-1)=trace(level,ply)
100           continue
              trace(51,ply-1)=trace(51,ply)
              trace(52,ply-1)=trace(52,ply)
          endif
c
c------------------------------< first, backup the best score found by
c------------------------------< examining the tree below this node.
c
      else
          value(1)=min(value(2),window(2))
          if(value(2).lt.window(2) .and. trueval.eq.0) then
              window(2)=value(2)+1
              value(0)=value(2)+1
              raised=0
          endif
c
c------------------------------< now, backup the principle variation
c------------------------------< that leads to the backed up score.
c
          if(value(1) .ne. window(2)) then
              lim=and(trace(51,ply),65535)
              do 200 level=1,lim
                  trace(level,1)=trace(level,2)
200           continue
              trace(51,1)=trace(51,2)
              trace(52,1)=trace(52,2)
          else
              if(curmvs(1,taskid) .ne. trace(1,1)) then
                  trace(1,1)=curmvs(1,taskid)
                  trace(51,1)=1
                  trace(52,1)=1
              endif
          endif
c
c------------------------------< force the current move to
c------------------------------< the top of the ply one move list
c------------------------------< to be considered first in the
c------------------------------< next iteration.
c
c         call lock22
          if(trueval .eq. 0) then
              do 300 where=1,last(1)
                  if(curmvs(1,taskid) .eq. moves(where)) go to 400
300           continue
              go to 700
400           continue
                  if(where .eq. 1) go to 700
                      easy=0
                      temp=moves(where)
                      temp1=order(where)
                      temp2=tried(where)
500                   continue
                          if(where .le. first(1)) go to 600
                              moves(where)=moves(where-1)
                              order(where)=order(where-1)
                              tried(where)=tried(where-1)
                              where=where-1
                      go to 500
600                   continue
                          moves(where)=temp
                          order(where)=temp1
                          tried(where)=temp2
700           continue
          endif
c
c------------------------------< if the variation was cut off by a hash
c------------------------------< table lookup, follow it and see if we can
c------------------------------< recover any more of it.  if more info is
c------------------------------< in the table, add it on to the end of what
c------------------------------< we have.
c
c------------------------------< determine depth of variation
c
          matply=999
          maxd=and(trace(51,1),65535)
          if(iabs(value(1)).ge.900000 .and.
     *       iabs(value(1)).lt.1000000) then
              matply=999999-iabs(value(1))
              maxd=min0(maxd,matply)
          endif
c
c------------------------------< now follow the variation to it's
c------------------------------< deepest move.
c
          ply=1
          side=1
          player=1
          if(maxd .ge. 2) then
              do 800 ply=2,maxd
                  side=-side
                  player=3-player
                  mfrom=extrct(trace(ply,1))
                  from(ply)=mfrom
                  to(ply)=mto
                  type(ply)=mtype
                  call make
                  if(repeat(dummy) .ne. 0) then
                      call unmake
                      go to 1000
                  endif
800           continue
          endif
c
c------------------------------< now continue and see if the current
c------------------------------< position is in the transposition table.
c------------------------------< if so, and a suggested move is stored,
c------------------------------< add it to the principal variation.
c
          ply=maxd
900       continue
              ply=ply+1
              side=-side
              player=3-player
              if(ply .gt. 40) go to 1000
              call lookup
              if(return .eq. 0) go to 1000
              if(hmove(ply) .eq. 0) go to 1000
              if(illegal(hmove(ply)) .ne. 0) go to 1000
              to(ply)=mto
              from(ply)=mfrom
              type(ply)=mtype
              call make
              if(repeat(dummy) .ne. 0) then
                  call unmake
                  go to 1000
              endif
              trace(ply,1)=hmove(ply)
              trace(51,1)=trace(51,1)+1
          go to 900
c
c------------------------------< now, unmake all of this nonsense to
c------------------------------< get back to the root position where
c------------------------------< we started.
c
1000      continue
              if(ply .gt. 2) then
1001              continue
                      ply=ply-1
                      player=3-player
                      side=-side
                      mfrom=from(ply)
                      mto=to(ply)
                      mtype=type(ply)
                      call unmake
                  if(ply .gt. 2) go to 1001
              endif
c
c------------------------------< save the variation being backed up
c------------------------------< so that we can use it to ponder
c------------------------------< later if we ever complete a search
c------------------------------< without a predicted move (happens 
c------------------------------< on fail-highs regularly).  we can
c------------------------------< fall-back to this predicted sequence
c------------------------------< if nothing better is available.
c
          if(value(2) .lt. window(2)) then
              do 1100 cmove=1,last(1)
                  if(ponmvs(1,cmove) .eq. trace(1,1)) go to 1200
1100          continue
1200          continue
              if(trace(52,ply).ge.ponmvs(4,cmove) .and.
     *           trace(51,ply).ge.2) then
                  do 1300 i=1,2
                      ponmvs(i,cmove)=trace(i,1)
1300              continue
                  ponmvs(3,cmove)=trace(51,1)
                  ponmvs(4,cmove)=trace(52,1)
              endif
              ponmvs(5,cmove)=value(1)
          endif
c         call unlock22
c
c------------------------------< display the move/variation if
c------------------------------< required.
c
          if(value(1) .ne. window(2)) call inform('  ')
          if(matem.ne.0 .and. value(1).gt.900000) 
     *                  value(1)=value(1)-2
      endif
c
c------------------------------< now, if backing up to a split point,
c------------------------------< other processors at this split point
c------------------------------< should share this new best score to
c------------------------------< improve their alpha/beta performance.
c
      if(dlevel(ply-1,taskid) .ne. 0) call sharev
      return
      end
