      subroutine inform(id)
crev  last revision 03/24/90
c
c     ******************************************************************
c     *                                                                *
c     *      inform is used to inform the operator about how the       *
c     *   search is progressing.  it is called from 'backup' each      *
c     *   time a move is found to be best if the 'autos' flag is set.  *
c     *   it prints the current depth, the current evaluation in       *
c     *   pawns or fractions of pawns and the current principle        *
c     *   variation.                                                   *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
      integer id
      real rvalue
      integer buf(72), prsave(6)
c
c
      include 'global.f'
c
c
      include 'common.f'
c
c
      equivalence (aster,alpha(66)),(blank,alpha(70)),
     *            (period,alpha(68)), (alphad,alpha(30)),
     *            (alphar,alpha(44)), (alphaa,alpha(27)),
     *            (alphaw,alpha(49))
c
c
c------------------------------< output the principle variation.
c
      if(sortp1 .eq. 0) then
          if(rnodes+nodes.lt.snodes .and. depth.gt.0 .and.
     *       value(1).lt.900000) return
      else
          if(rnodes+nodes .lt. snodes) return
      endif
      if(trace(1,1) .eq. 0) return
      call cptime(psec2,msec2)
      if(cputim .ne. 0) psec2=msec2
      itime=(psec2-fsec1)/100
      do 100 i=1,6
          prsave(i)=prevmv(i)
100   continue
      nchars=0
      do 200 i=1,72
          buf(i)=blank
200   continue
      tfirst=1
      tply=ply
      tmside=side
      tmplayr=player
      ply=1
      side=1
      player=1
      mfrom=extrct(trace(1,1))
      call unmake
      v1=value(1)
      if(v1.ge.100 .and. v1.le.900000) v1=v1-100
      rvalue=float(v1)/1000.0
c
c------------------------------< determine depth of variation
c
      matply=999
      maxd=and(trace(51,1),65535)
      if(iabs(value(1)).ge.900000 .and.
     *   iabs(value(1)).lt.1000000) then
          matply=999999-iabs(value(1))
          maxd=min0(maxd,matply)
      endif
c
c------------------------------< convert moves to text for output.
c
      side=-1
      do 1100 ply=1,maxd
          mfrom=extrct(trace(ply,1))
          side=-side
          player=2-and(ply,1)
          prevmv(3*(player-1)+1)=mfrom
          prevmv(3*(player-1)+2)=mto
          prevmv(3*(player-1)+3)=mtype
          emtype=0
          ctemp=checkg(player)
          if(ctemp .ne. 0) emtype=1
          if(ply .eq. matply) emtype=2
          call output(emtype,1)
c
c------------------------------< compress moves into the buffer.
c
          nchars=nchars+1
          j=0
          do 400 i=nchars,72
              j=j+1
              buf(i)=text(j)
              if(text(j).eq.blank .and. text(j+1).eq.blank) go to 500
400       continue
c
c------------------------------< output a line when the buffer is full
c------------------------------< or when the variation is complete.
c
500       continue
          nchars=i
          if(nchars .ge. 36) then
              if(tfirst .ne. 0) then
                  if(autos .gt. 0) print 700, depth, id,
     *                             hhmmss(itime,5),
     *                             rvalue, (buf(l),l=1,nchars)
              else
                  if(autos .gt. 0) print 600,(buf(l),l=1,nchars)
              endif
              tfirst=0
600           format(40x,72a1)
700           format(17x,i3,a2,2x,a5,t30,f9.3,2x,72a1)
800           format(17x,'book ',2x,a5,t30,f9.3,2x,72a1)
              nchars=0
              do 900 i=1,72
                  buf(i)=blank
900           continue
          endif
          from(ply)=mfrom
          to(ply)=mto
          type(ply)=mtype
          call make
1100  continue
      if(maxd .gt. 1)  then
          side=-side
          do 1200 level=1,maxd-1
              ply=maxd-level+1
              player=2-and(ply,1)
              side=-side
              mfrom=extrct(trace(ply,1))
              call unmake
1200      continue
      endif
c
c------------------------------< output the buffer if anything is
c------------------------------< left in it.
c
      if(trace(51,1).ge.65536 .and. maxd.ne.matply) then
          l1=nchars+1
          l2=nchars+3
          do 1400 i=l1,l2
              buf(i)=period
1400      continue
          nchars=nchars+3
      endif
      if(value(1).ge.drawsc .and. value(1).lt.drawsc+100
     *   .and. depth.ne.0) then
          if(buf(nchars) .ne. blank) nchars=nchars+1
          buf(nchars+1)=alphad
          buf(nchars+2)=alphar
          buf(nchars+3)=alphaa
          buf(nchars+4)=alphaw
          nchars=nchars+4
      endif
      if(nchars .gt. 0) then
          if(tfirst .ne. 0) then
              if(depth .gt. 0) then
                  if(autos .gt. 0) print 700, depth,id,
     *                             hhmmss(itime,5),
     *                             rvalue,(buf(l),l=1,nchars)
              else
                  if(autos .gt. 0)
     *                print 800, hhmmss(itime,5), rvalue, 
     *                           (buf(l),l=1,nchars)
              endif
          else
              if(autos .gt. 0) print 600,(buf(l),l=1,nchars)
          endif
      endif
      do 1900 i=1,6
          prevmv(i)=prsave(i)
1900  continue
      ply=tply
      player=tmplayr
      side=tmside
      return
      end
