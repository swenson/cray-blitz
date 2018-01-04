      function attack(aside,asquare)
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *       attack is called to determine if square 'asquare' is     *
c     *  under attack by side 'aside'.  it returns a true/false.  this *
c     *  version uses the bit board for a fast check.                  *
c     *                                                                *
c     ******************************************************************
c
c
c    mtype:        0 = normal move
c                  1 = castle king-side
c                  2 = castle queen-side
c                  3 = en passant pawn capture
c                  4 = pawn promotion to knight
c                  5 = pawn promotion to bishop
c                  6 = pawn promotion to rook
c                  7 = pawn promotion to queen
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
c------------------------------< determine if 'asquare' is under
c------------------------------< attack.
c
      attack=-1
      playr=1
      if(aside .lt. 0) playr=2
      do 100 i=pstart(playr),pend(playr)
          if(plist(i) .eq. 0) go to 100
              rtemp=and(at(plist(i),asquare,vlist(i)+7),bitbd)
              if(rtemp .eq. 0) return
100   continue
      attack=0
      return
      end
