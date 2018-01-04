      subroutine mater
crev  last revision 03/30/91
c
c     ******************************************************************
c     *                                                                *
c     *      mater is called to determine how deeply checks should be  *
c     *  evaluated.  it gives blitz it's ''bloodthirsty'' attitude     *
c     *  by attempting to find mates at extremely deep nodes.  it      *
c     *  operates as follows:                                          *
c     *                                                                *
c     *        1)  count the number of checking moves that occurred    *
c     *            for the side on move in the basic search (plies     *
c     *            not greater than rdepth).                           *
c     *                                                                *
c     *        2)  make sure that all moves since the basic search     *
c     *            ended are also checks, since the opposing side      *
c     *            can simply 'stand pat' to avoid a mate if they      *
c     *            are not;  if they are, the opponent must try all    *
c     *            legal moves to avoid mate;                          *
c     *                                                                *
c     *        3)  count the number of checks that have been included  *
c     *            in the line under analysis (capturing checks count  *
c     *            as one half of a non-capturing check);  if this     *
c     *            number is less than the number of basic search      *
c     *            checks found in (1) above, then we will include     *
c     *            all checking moves for this level in the tree       *
c     *            to be examined.  note that only two (2) additional  *
c     *            checks can be included by this algorithm.           *
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
c
c
c------------------------------< count the number of checks that
c------------------------------< occurred in the basic search.
c
      lastply=rdepth+1
      start=2+and(ply+1,1)
      if(start .gt. lastply) return
          bcheck=0
          do 100  level=start,lastply,2
              if(inchk(level) .ne. 0) bcheck=bcheck+1
100       continue
          if(bcheck .eq. 0) return
          bcheck=min0(bcheck,2)
c
c------------------------------< make sure that the opponent was in
c------------------------------< check at all nodes that are in the
c------------------------------< quiescence search.  if not, mates
c------------------------------< cannot be found since the opponent
c------------------------------< can stand pat at any node rather
c------------------------------< than taking the search evaluation.
c
          start=rdepth+1
          if(2-and(start,1) .eq. player) start=start+1
          lastply=ply-1
          if(start .le. lastply) then
              do 200  level=start,lastply,2
                  if(inchk(level) .eq. 0) return
200           continue
          endif
c
c------------------------------< count the number of moves that are
c------------------------------< checks that have already occurred
c------------------------------< in the quiescence search.
c
          start=rdepth+1
          if(and(start,1) .ne. and(player,1)) start=start+1
          lastply=ply-2
          qcheck=0
          ccheck=0
          onecheck=0
          if(start .le. lastply) then
              do 500 level=start,lastply,2
                  if(onerep(level+1) .ne. 1) then
                      if(cappc(level).eq.0 .and.
     *                   type(level).lt.promot) then       
                          qcheck=qcheck+1
                      else
                          ccheck=ccheck+1
                      endif
                  else
                      onecheck=onecheck+1
                  endif
500           continue
              qcheck=qcheck+ishft(ccheck,-1)+ishft(onecheck,-2)
          endif
c
c------------------------------< if there were fewer non-capturing
c------------------------------< checks in the quiescence search than
c------------------------------< there were in the basic search, then
c------------------------------< examine all checks at this level.
c
          if(qcheck .lt. bcheck) givchk(ply)=1
          return
      end
