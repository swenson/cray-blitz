      subroutine saver
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      saver with it's two entry points 'savegb' and 'restgb'    *
c     *  is used to save/restore the game board and related castling   *
c     *  status.  this is used by routines which must modify the       *
c     *  game board or castling status and need to restore it after    *
c     *  finishing whatever processing was required.                   *
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
c------------------------------< save the game board and castling
c------------------------------< status.
c
      entry savegb
      do 100 i=1,2
          psave(i)=movedr(i)
100   continue
      return
c
c------------------------------< restore the game board and
c------------------------------< castling status.
c
      entry restgb
      do 200 i=1,2
          movedr(i)=psave(i)
200   continue
      return
      end
