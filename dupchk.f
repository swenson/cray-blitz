      function dupchk(dummy)
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      dupchk is used to detect a draw by repetition.  it        *
c     *  saves the current position in the position table each time    *
c     *  it is called.  the table contains all positions encountered   *
c     *  since the last irreversable move (capture or pawn push).      *
c     *      dupchk then scans the table to determine how many times   *
c     *  this position has occurred before.  three times will be       *
c     *  treated as a draw by 'main'.                                  *
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
c------------------------------< compress the board into the next
c------------------------------< slot in the position table.
c
      bdsave(point)=hash
c
c------------------------------< scan prior positions list to
c------------------------------< determine if this position has
c------------------------------< occurred before.
c
      dupchk=1
      start=2-and(point,1)
      lim=point-1
      if(start .gt. lim) return
      do 100 i=start,lim,2
          if(hash .eq. bdsave(i)) then
              rmoves(dupchk)=i
              dupchk=dupchk+1
          endif
100   continue
      return
      end
