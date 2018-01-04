      subroutine pocmnd
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *     pocmnd is called to force the program to ponder using a    *
c     *  particular move for the opponent.  this is useful to try      *
c     *  several different moves in a given position and allow the     *
c     *  program to analyze and print it's analysis.                   *
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
      equivalence (blank,alpha(70))
c
c
c
c
c------------------------------< isolate move from command.
c
      do 200 i=1,80
          if(atext(1) .eq. blank) go to 300
          do 100 j=1,79
              atext(j)=atext(j+1)
100       continue
200   continue
300   continue
      do 500 i=1,80
          if(atext(1) .ne. blank) go to 600
          do 400 j=1,79
              atext(j)=atext(j+1)
400       continue
500   continue
600   continue
      if(atext(1) .eq. blank) return
c
c------------------------------< convert and make the new move
c
      ply=2
      player=2
      side=-1
      call input(1)
      if(return .ne. 0) go to 1100
      trace(51,1)=0
      lmoveo=mfrom+ishft(mto,7)+ishft(mtype,14)
     *       +ishft(movgpc,20)+ishft(mcappc,17)
      matchd=0
      foundm=0
      pmove=lmoveo
      if(attack(-1,kloc(1)) .eq. 0) return
c
c------------------------------< can't make a move that leaves
c------------------------------< the king in check
c
          print 1000
1000      format(1x,'your king is in check.')
          return
c
c------------------------------< illegal move
c
1100  continue
          print 1200
1200      format(1x,'that is an illegal move to ponder.')
          return
      end
