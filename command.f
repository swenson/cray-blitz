      subroutine command(eof)
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *     command is used to allow multiple commands to the chess    *
c     *  program to be entered on one line separated by semicolons.    *
c     *  the first call to command will result in a read to the        *
c     *  terminal.  the text up to the first ';' will be moved into    *
c     *  atext(i) array.  each successive call to command will return  *
c     *  the next group of characters up to the next ';'.  when no     *
c     *  more characters are left, the buffer is replinished by a read *
c     *  to the terminal.                                              *
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
      equivalence (blank,alpha(70)),(semic,alpha(77))
c
c
c------------------------------< refill the buffer if necessary
c
      eof=0
      do 200 i=1,80
          atext(i)=blank
200   continue
      read(unit,300,end=9999) atext
300   format(80a1)
      return
9999  continue
      return (1)
      end
