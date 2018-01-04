      subroutine srcmnd
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      this subroutine is used to set the performance rating for *
c     *  the opponent.  this is used in determining when to accept or  *
c     *  reject a draw or stalemate.  a 'contempt' factor is computed  *
c     *  based on how much better or worse the program is than the     *
c     *  opponent.  this factor is used in the search whenever a draw  *
c     *  or stalemate is found to determine if it should be accepted   *
c     *  or avoided.                                                   *
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
      equivalence (d,alpha(30))
c
c
c------------------------------< get the opponent's rating
c
      if(atext(3) .ne. d) then
          temp=4
          rtemp=scan(temp)
          if(rtemp .ne. 0) then
              orate=rtemp
              drawsc=(orate-prate)*5
          endif
      endif
c
c------------------------------< output both ratings
c
      print 100,prate,orate
      write(3,100) prate,orate
100   format(1x,'blitz:',i5,3x,'opponent:',i5)
      print 200, float(drawsc)/1000.0
      write(3,200) float(drawsc)/1000.0
200   format(1x,'draw is',f6.1,' pawns.')
      return
      end
