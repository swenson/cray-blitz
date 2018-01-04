      subroutine output(otype,packer)
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      output is used to convert the internal move format to     *
c     *  a text string so that it can be printed.  packer is a flag    *
c     *  used to indicate whether (true) or not (false) to reduce the  *
c     *  text string to it's simplest form.  this reduction cannot     *
c     *  be done while the search is active since is causes move       *
c     *  generations and alters the move list.  in these cases, packer *
c     *  will always be false to avoid trouble.                        *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      integer piecnm(6)
      integer rank(8)
c
c
      include 'global.f'
c
c
      include 'common.f'
c
c
      equivalence (rank(1),alpha(54)),
     * (alphax,alpha(50)),(alphao,alpha(41)),(alphay,alpha(51)),
     * (plus,alpha(63)),(minus,alpha(64)),(slash,alpha(65)),
     * (equal,alpha(67)),(blank,alpha(70)),(alphaa,alpha(27)),
     * (alphan,alpha(40)), (alphau, alpha(47)), (alphal, alpha(38))
      data piecnm /'P','N','B','R','Q','K'/
c
c
c------------------------------< initialize.
c
      do 100 i=1,80
          text(i)=blank
100   continue
c
c------------------------------< process origin file/square
c------------------------------< and destination file/square
c
      go to (200,900,800,200,200,200,200,200,1000),mtype+1
200   continue
          fr=9-mfrom/10+1
          ff=mod(mfrom,10)-1
          tr=9-mto/10+1
          tf=mod(mto,10)-1
          if(color .eq. 1) then
              fr=9-fr
              ff=9-ff
              tr=9-tr
              tf=9-tf
          endif
          text(1)=piecnm(movgpc)
          text(2)=alpha(ff+26)
          text(3)=rank(fr)
          text(4)=minus
          if(mcappc .ne. 0) text(4)=alphax
          text(5)=alpha(tf+26)
          text(6)=rank(tr)
          ptr=7
c
c------------------------------< now build text string to output
c------------------------------< based on information from above.
c
          if(mtype .ge. promot) then
              text(ptr)=equal
              text(ptr+1)=piecnm(mpropc)
              ptr=ptr+2
          endif
c
c------------------------------< add ' +' if the move is a check
c
          if(otype .eq. 1) text(ptr)=plus
c
c------------------------------< now reduce the move text to the
c------------------------------< minimum required for non-ambiguity.
c
          if(packer .ne. 0) call reduce(packer)
          do 500 ptr=1,30
              if(text(ptr) .eq. blank) go to 600
500       continue
600       continue
c
c------------------------------< add '++' if the move gives
c------------------------------< checkmate.
c
          if(otype .eq. 2) then
              text(ptr)=plus
              text(ptr+1)=plus
          endif
          return
c
c------------------------------< process castling moves
c
800   continue
          text(4)=minus
          text(5)=alphao
900   continue
          text(1)=alphao
          text(2)=minus
          text(3)=alphao
          return
c
c------------------------------< process null moves
c
1000  continue
          text(1)=alphan
          text(2)=alphau
          text(3)=alphal
          text(4)=alphal
          return
      end
