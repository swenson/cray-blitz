      subroutine test(packer)
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      test is used to determine if a move is ambiguous when     *
c     *  attempting to simplify moves for output.  it removes all      *
c     *  blanks and uses 'input' to analyze the move.                  *
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
c------------------------------< remove all blanks
c
      do 300 count=1,3
          do 200 i=1,29
              if(atext(i) .eq. blank) then
                  do 100 j=i,29
                      atext(j)=atext(j+1)
100               continue
              endif
200       continue
300   continue
c
c------------------------------< now set the input character strings
c------------------------------< so that input can decode what is
c------------------------------< going on
      do 400 ch=1,30
          if(atext(ch) .eq. blank) go to 500
400   continue
500   continue
c
c------------------------------< now try to decode the move to see if
c------------------------------< it is ambiguous.
c
      return=0
      if(packer .eq. 1) call input(0)
      return
      end
