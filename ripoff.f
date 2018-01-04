      integer function ripoff(rsquare)
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      ripoff is used to evaluate the safety of a piece located  *
c     *  on 'rsquare'.  it determines if the piece located on rsquare  *
c     *  can be profitably captured by the opposing side.  it eval-    *
c     *  uates the pieces bearing on 'rsquare' and simulates a series  *
c     *  of captures to see if capturing the piece will win or lose    *
c     *  material.  this check is a cursory examination, pieces that   *
c     *  are pinned or overloaded are still considered to bear on the  *
c     *  square even though their participation in the sequence of     *
c     *  exchanges might result in a greater loss elsewhere on the     *
c     *  board.                                                        *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      integer pmoves(10), omoves(10), list(20)
c
c
      include 'global.f'
c
c
      include 'common.f'
c
c
c------------------------------< initialize.
c
      ripoff=0
      if(board(rsquare) .eq. 0) return
      pmovs=0
      omovs=0
c
c------------------------------< pawn captures
c
      if(board(rsquare-9) .eq. 1) then
          pmovs=pmovs+1
          pmoves(pmovs)=pieces(1)
      endif
      if(board(rsquare-11) .eq. 1) then
          pmovs=pmovs+1
          pmoves(pmovs)=pieces(1)
      endif
      if(board(rsquare+9) .eq. -1) then
          omovs=omovs+1
          omoves(omovs)=pieces(1)
      endif
      if(board(rsquare+11) .eq. -1) then
          omovs=omovs+1
          omoves(omovs)=pieces(1)
      endif
c
c------------------------------< knight captures
c
      do 600 i=9,16
          loc=rsquare+movdir(i)
          if(board(loc) .eq. 2) then
              pmovs=pmovs+1
              pmoves(pmovs)=pieces(2)
          else if(board(loc) .eq. -2) then
              omovs=omovs+1
              omoves(omovs)=pieces(2)
          endif
600   continue
c
c------------------------------< bishop captures
c
      pqn=0
      oqn=0
      do 1400 i=5,8
          dir=movdir(i)
          loc=rsquare
700       continue
              loc=loc+dir
              temp=board(loc)
              if(temp .le. 6) then
                  go to (1400,1200,1400,1100,1400,1300,700,
     *                   1000,1400,800,1400,900,1400),     temp+7
800               continue
                      pmovs=pmovs+1
                      pmoves(pmovs)=pieces(temp)
                      go to 700
900               continue
                      pqn=1
                      go to 700
1000              continue
                      if(loc .ne. rsquare+dir) go to 1400
                      if(i .lt. 7) go to 1400
                          go to 700
1100              continue
                      omovs=omovs+1
                      omoves(omovs)=pieces(temp)
                      go to 700
1200              continue
                      oqn=1
                      go to 700
1300              continue
                      if(loc .ne. rsquare+dir) go to 1400
                      if(i .gt. 6) go to 1400
                          go to 700
              endif
1400  continue
c
c------------------------------< rook captures
c
      do 2000 i=1,4
          dir=movdir(i)
          loc=rsquare
1500      continue
              loc=loc+dir
              temp=board(loc)
              if(temp .le. 6) then
                  go to (2000,1900,1800,2000,2000,2000,1500,
     *                   2000,2000,2000,1600,1700,2000),    temp+7
1600              continue
                      pmovs=pmovs+1
                      pmoves(pmovs)=pieces(temp)
                      go to 1500
1700              continue
                      pqn=1
                      go to 1500
1800              continue
                      omovs=omovs+1
                      omoves(omovs)=pieces(temp)
                      go to 1500
1900              continue
                      oqn=1
                      go to 1500
              endif
2000      continue
c
c------------------------------< queen captures
c
      if(pqn .ne. 0) then
          pmovs=pmovs+1
          pmoves(pmovs)=pieces(5)
      endif
      if(oqn .ne. 0) then
          omovs=omovs+1
          omoves(omovs)=pieces(5)
      endif
c
c------------------------------< king captures
c
      do 2400 i=17,24
          loc=rsquare+movdir(i)
          if(board(loc) .eq. 6) then
              pmovs=pmovs+1
              pmoves(pmovs)=pieces(6)
          else if(board(loc) .eq. -6) then
              omovs=omovs+1
              omoves(omovs)=pieces(6)
          endif
2400  continue
c
c------------------------------< after storing the moves, evaluate the
c------------------------------< exchanges that are possible to
c------------------------------< determine the loss/gain.
c
      ipmovs=0
      iomovs=0
      list(1)=0
      rsign=1
      mdepth=1
      piece=pieces(board(rsquare))
      playr=0
      if(board(rsquare) .lt. 0) playr=1
c
c------------------------------< change sides.
c
2500  continue
          playr=xor(playr,1)
c
c------------------------------< it is the machines turn to capture,
c------------------------------< if it doesn't have any more pieces
c------------------------------< bearing on the square, time to quit.
c
          if(playr .eq. 0) then
              ipmovs=ipmovs+1
              if(ipmovs.gt.pmovs) go to 2800
              tpiece=pmoves(ipmovs)
              if(tpiece .gt. pieces(1)) go to 2700
                  if(rsquare .lt. 90) go to 2700
                  tpiece=pieces(5)
                  piece=piece+pieces(5)-pieces(1)
c
c------------------------------< it is the humans turn to capture,
c------------------------------< if he doesn't have any more pieces
c------------------------------< bearing on the rsquare, time to quit.
c
          else
              iomovs=iomovs+1
              if(iomovs.gt.omovs) go to 2800
              tpiece=omoves(iomovs)
              if(tpiece .gt. pieces(1)) go to 2700
                  if(rsquare .gt. 30) go to 2700
                  tpiece=pieces(5)
                  piece=piece+pieces(5)-pieces(1)
          endif
c
c------------------------------< perform all indicated captures by
c------------------------------< minimaxing the scores.
c
2700          continue
                  mdepth=mdepth+1
                  list(mdepth)=list(mdepth-1)+rsign*piece
                  piece=tpiece
                  rsign=-rsign
      go to 2500
c
c------------------------------< now that scoring is complete, scan
c------------------------------< the exchange list to determine when
c------------------------------< the sequence of exchanges would
c------------------------------< stop.
c
2800  continue
      if(mdepth .le. 1) return
      rsign=1
      if(and(mdepth,1) .eq. 0) rsign=-1
2900  continue
          if(mdepth .eq. 1) go to 9999
          if(rsign*list(mdepth) .le. rsign*list(mdepth-1))
     *        list(mdepth-1)=list(mdepth)
          mdepth=mdepth-1
          rsign=-rsign
      go to 2900
c
c------------------------------< the exchange sequences have been
c------------------------------< evaluated, now return the expected
c------------------------------< gain.
c
9999  continue
          ripoff=list(1)
          return
      end
