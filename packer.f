      subroutine packer(id)
      implicit integer (a-z)
      include 'global.f'
      include 'common.f'
      common /ptable/ ptable(131072,8)
      if(id.lt.1 .or. id.gt.131072) print 50, id
50    format(///1x,'--------------bad id to packer = ',i9)
      do 200 i=2,9,2
          pack=0
          do 100 j=2,9
              pack=ishft(pack,4)+board(i*10+j)+6
100       continue
          do 150 j=2,9
              pack=ishft(pack,4)+board((i+1)*10+j)+6
150       continue
          ptable(id,i/2)=pack
200   continue
      ptable(id,5)=random(or(moveds(ply-2),
     *                            ishft(moveds(ply-1),2))+1,2)
      ptable(id,6)=0
      if(board(to(ply-1)).eq.-side .and.
     *   iabs(to(ply-1)-from(ply-1)).eq.20 .and.
     *   (board(to(ply-1)-1).eq.side .or.
     *    board(to(ply-1)+1).eq.side))
     *    ptable(id,6)=random(cfiles(to(ply-1)),1)
      ptable(id,7)=hash
      ptable(id,8)=hashbd
      xhashbd=0
      do 900 i=2,9
          do 900 j=2,9
              temp=random(i*10+j,board(i*10+j)+7)
              xhashbd=xor(xhashbd,temp)
900   continue
      if(xhashbd .ne. hash) print 950, hash, xhashbd
950   format(////'can''t match hash (good, bad - 1) =',2x,z16,2x,z16)
      xhashbd=xor(xhashbd,ptable(id,5))
      xhashbd=xor(xhashbd,ptable(id,6))
      if(xhashbd .ne. hashbd) print 1000, hashbd, xhashbd
1000  format(////'can''t match hashbd (good, bad - 2) =',2x,z16,2x,z16)
      return
      end
