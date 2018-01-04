      subroutine setgb(init)
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      setgb is used to initialize the playing version of the    *
c     *  game board as well as castling status.                        *
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
      dimension piece(8)
      data piece / 4,2,3,5,6,3,2,4 /
c
c
c------------------------------< clear the board
c
      do 50 i=1,120
          board(i)=99
50    continue
      do 200 i=4,7
          do 100 j=2,9
              board(i*10+j)=0
100       continue
200   continue
c
c------------------------------< set pieces and pawns
c
      do 300 i=2,9
          board(20+i)=piece(i-1)
          board(30+i)=1
          board(80+i)=-1
          board(90+i)=-piece(i-1)
300   continue
c
c------------------------------< if program is black, the king and
c------------------------------< queen must be reversed.
c
      if(color .eq. 1) then
          board(25)=6
          board(26)=5
          board(95)=-6
          board(96)=-5
      endif
c
c------------------------------< initialize castling status
c
      do 500 i=1,2
          movedr(i)=3
500   continue
c
c------------------------------< initialize development and
c------------------------------< scoring status
c
      e4=0
      d4=0
      devdone=0
c
c------------------------------< set the pawn advance scores to
c------------------------------< encourage king-pawn advances more
c------------------------------< than queen-pawn advances.
c
      if(init .ne. 0) then
          if(color .eq. 1) then
              do 600 i=2,5
                  temp=pfiles(i)
                  pfiles(i)=pfiles(11-i)
                  pfiles(11-i)=temp
600           continue
          endif
      endif
      call locate
      return
      end
