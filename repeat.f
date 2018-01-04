      integer function repeat(dummy)
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      repeat is used to detect a draw by repetition.  it        *
c     *  saves the current position in the position table each time    *
c     *  it is called.  the table contains all positions encountered   *
c     *  since the last irreversable move (capture or pawn push).      *
c     *      repeat then scans the table to determine if               *
c     *  this position has occurred before.  if so, the position       *
c     *  will be treated as a draw by 'search'.                        *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
c
c
      include 'common.f'
c
c
c------------------------------< initialize
c
      repeat=1
c
c------------------------------< compress the board into the next
c------------------------------< slot in the position table.
c
      temp=point+ply
      bdsave(temp)=hash
c
c------------------------------< scan prior positions list to
c------------------------------< determine if this position has
c------------------------------< occurred before.
c
      do 100 i=2-and(temp,1),point+ply-1,2
          if(hash .eq. bdsave(i)) return
100   continue
      repeat=0
      return
      end
