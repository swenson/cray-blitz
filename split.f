      subroutine split
crev  last revision 11/07/91
c
c     ******************************************************************
c     *                                                                *
c     *     split is called whenever idle processors are found that    *
c     *  can be used somewhere on the current branch.  it allocates    *
c     *  a block of the shared common, and then points the idle        *
c     *  processors to it by setting block(idle proc) to point to      *
c     *  the allocated block.                                          *
c     *                                                                *
c     *     If it cannot find a split point, it forwards the help      *
c     *  request to the next busy processor.                           *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
c
c
      include 'global.f'
c
c
      include 'perm.f'
c
c
      include 'common.f'
c
c
      dimension tn(50), wk(50), nosplit(50)
c
c
c------------------------------< return if we recently tried to do a
c------------------------------< split.  this will keep us from
c------------------------------< thrashing around near the end of an
c------------------------------< iteration and wasting time.
c
      if(nodes .lt. cnodes) then
          if(and(debug,2048).ne.0 .and. help(taskid).eq.20) then
              print 25, taskid
25            format(1x,'split    -  pid ',i2,' split request '
     *                  'ignored -- thrashing control')
              help(taskid)=19
              return
          endif
      endif
      cnodes=nodes+tnodes
c
c------------------------------< return if we are not deep enough to
c------------------------------< choose a good split point.  make
c------------------------------< sure that we don't defer too long
c------------------------------< by limiting deferals to 20.
c
      if(ply.lt.rdepth .and. help(taskid).gt.1) then
          help(taskid)=help(taskid)-1
          return
      endif
c
c------------------------------< first, find an unused common block
c------------------------------< and allocate it.
c
c     call lock22
      call cptime(ohsec1,msec)
      if(cputim .ne. 0) ohsec1=msec1
      do 50 i=1,50
          nosplit(i)=0
50    continue
75    continue
      if(idle .gt. 0) then
          do 100 blockid=1,16
              if(alloc(blockid) .eq. 0) go to 300
100       continue
          print 200, taskid
200       format(1x,'split block allocate failed, taskid=',i2)
          help(taskid)=0
c         call unlock22
          return
c
c------------------------------< determine if any idle processors
c------------------------------< are waiting on work.  we might be
c------------------------------< trying to split after another
c------------------------------< processor has already responded.
c------------------------------< this avoids repeated split attempts
c------------------------------< when none are needed.
c
300       continue
              do 320 i=1,ncpus
                  if(working(i) .eq. 0) go to 325
320           continue
              help(taskid)=0
              return
325       continue
c
c------------------------------< determine if this processor has any
c------------------------------< node that can be classified as needing
c------------------------------< a full-width search (Knuth's type one
c------------------------------< and type three nodes).  use 'typenode'
c------------------------------< to classify each ply level after first
c------------------------------< making sure that a node has work to be
c------------------------------< done.
c
              do 350 i=1,min0(ply+1,rdepth)
                  tn(i)=typenode(i)
350           continue
              do 355 i=rdepth+1,ply+1
                  tn(i)=tn(i-2)
355           continue
              do 380 splitply=1,ply+1
                  wk(splitply)=0
                  if(dlevel(splitply,taskid).eq.0 .and.
     *               nosplit(splitply).eq.0) then
                      if(which(splitply) .ne. 0) then
                          do 375 i=first(splitply),last(splitply)
                              if(phase(splitply) .le. 1) then
                                  wk(splitply)=last(1)-first(1)+1
                              else
                                  if(splitply .eq. 1) then
                                      if(tried(i) .eq. 0)
     *                                     wk(splitply)=wk(splitply)+1
                                  else
                                      if(moves(i) .ne. 0)
     *                                     wk(splitply)=wk(splitply)+1
                                  endif
                              endif
375                       continue
                      else
                          if(splitply .le. rdepth) then
                              wk(splitply)=last(1)-first(1)+1
                          else
                              wk(splitply)=4
                          endif
                      endif
                  endif
                  if(splitply .eq. ply) wk(splitply)=wk(splitply)-1
                  if(splitply.gt.1 .and. wk(splitply).lt.
     *               last(splitply)-first(splitply)) tn(splitply)=3
380           continue
              do 400 i=2,ply+1
                  if(tn(i).eq.1 .and. tn(i-1).ne.1) tn(i)=5-tn(i-1)
                  if(tn(i).eq.tn(i+1) .and. 
     *               tn(i).ne.1) tn(i+1) = 5-tn(i)
400           continue
c
c------------------------------< first, if all nodes up to this point
c------------------------------< are type=1, and the node at the next
c------------------------------< ply is also type=1, then don't split
c------------------------------< until ply=rdepth (max search depth
c------------------------------< for this iteration) since that node
c------------------------------< will be a principle variation node.
c
              splitply=rdepth
              if(ply.eq.rdepth .and. tn(ply).eq.1 .and.
     *           wk(ply).gt.0 .and. ply.ge.minply) go to 700
c
c------------------------------< find the type=3 node at the lowest
c------------------------------< depth since these nodes have the
c------------------------------< most work remaining.
c
              do 500 splitply=minply,min0(ply,rdepth)
                  if(tn(splitply).eq.3 .and.
     *               wk(splitply).gt.0)       go to 700
500           continue
c
c------------------------------< find the last type=1 node and go
c------------------------------< ahead and start on it rather than
c------------------------------< waiting idly by.
c
              do 501 splitply=minply,min0(ply,rdepth)
                  if(tn(splitply).eq.1 .and.
     *               tn(splitply+1).ne.1 .and.
     *                wk(splitply).gt.0)       go to 700
501           continue
c
c------------------------------< no reasonable split point found,
c------------------------------< forward the help request to the
c------------------------------< next higher-numbered processor.
c------------------------------< remember that it will forward the
c------------------------------< request on if it is not busy.
c
              helpid=taskid
502           continue
                  helpid=helpid+1
                  if(helpid .gt. ncpus) helpid=1
                  if(working(helpid) .ne. 0) go to 505
              if(helpid .ne. taskid) go to 502
              helpid=taskid+1
              if(helpid .gt. ncpus) helpid=1
              if(helpid .eq. taskid) helpid=helpid+1
505           continue
                  help(taskid)=0
                  help(helpid)=20
                  if(and(debug,8) .ne. 0) then
                      print 302, taskid, (tn(i),wk(i),i=1,ply)
                      print 601, taskid, helpid
601                   format(1x,'split    -  pid ',i2,' forwarding '
     *                       'help request to pid ',i2)
                  endif
                  nsplits=nsplits+1
                  call cptime(ohsec2,msec)
                  if(cputim .ne. 0) ohsec2=msec1
                  overhead=overhead+ohsec2-ohsec1
                  sptime=sptime+ohsec2-ohsec1
c                 call unlock22
                  return
c
c------------------------------< we found a reasonable split point.
c------------------------------< copy the current task common to the
c------------------------------< newly allocated shared common.
c
700       continue
              if(and(debug,8) .ne. 0) then
                  print 301, taskid, blockid, splitply
301               format(1x,'split    -  pid ',i2,' block ',i2,
     *                   ' ply ',i2)
                  if(and(debug,1024) .ne. 0) then
                      print 302, taskid, (tn(i),wk(i),i=1,ply)
302                   format(1x,'split    -  pid',i3,
     *                       ' node types = ',6(i2,'(',i3,')'),
     *                       5(/10x,6(i2,'(',i3,')')))
                  endif
              endif
              splits(splitply)=splits(splitply)+1
              if(wk(splitply) .lt. 1) then
                  print 305, taskid, splitply
305               format(1x,'warning! split failed, pid',i3,'  ply',i2)
                  print 302, taskid, (tn(i),wk(i),i=1,ply)
                  go to 75
              endif
c
c------------------------------< allocate the split block, set this
c------------------------------< processor to point to it and then
c------------------------------< point any other processors splitting
c------------------------------< with this processor at some other
c------------------------------< split point to this point also.
c
              alloc(blockid)=1
              stopping(blockid)=0
              atply(blockid)=splitply
              if(splitply .gt. splitl(taskid)) then
                  splitl(taskid)=splitply
                  block(taskid)=blockid
              endif
              dlevel(splitply,taskid)=blockid
              nbusy(blockid)=1
              do 750 i=1,ncpus
                  if(i.ne.taskid .and. splitl(i).gt.splitply) then
                      do 740 j=splitply+1,splitl(i)
                          if(dlevel(j,i) .ne. 0) then
                              if(dlevel(j,i) .eq.
     *                           dlevel(j,taskid)) then
                                  dlevel(splitply,i)=blockid
                                  nbusy(blockid)=nbusy(blockid)+1
                              endif
                              go to 750
                          endif
740                   continue
                  endif
750           continue
              call copyl(blockid)
              qdata(672,blockid)=2
c
c------------------------------< now sic the idle processors on the
c------------------------------< selected node to begin parallel
c------------------------------< processing at this point.
c
              do 900 i=1,ncpus
                  if(working(i) .eq. 0) then
                      wk(splitply)=wk(splitply)-1
                      if(wk(splitply) .lt. 0) then
                          nosplit(splitply)=1
                          go to 75
                      endif
                      do 800 j=1,splitply
                          dlevel(j,i)=dlevel(j,taskid)
800                   continue
                      do 805 j=1,splitply-1
                          curmvs(j,i)=curmvs(j,taskid)
805                   continue
                      do 810 j=splitply,50
                          curmvs(j,i)=0
810                   continue
                      working(i)=2
                      tprocs(splitply)=tprocs(splitply)+1
                      stop(i)=0
                      idle=idle-1
                      do 850 j=1,splitply
                          if(dlevel(j,i).ne.0) then
                              splitl(i)=j
                              nbusy(dlevel(j,i))=nbusy(dlevel(j,i))+1
                          endif
850                   continue
                      if(and(debug,8) .ne. 0)
     *                                     print 870, taskid, i, blockid
870                   format(1x,'split    -  pid ',i2,' attaching ',
     *                       'processor ',i2,' block ',i2)
                      block(i)=blockid
                  endif
900           continue
      endif
      call cptime(ohsec2,msec)
      if(cputim .ne. 0) ohsec2=msec1
      overhead=overhead+ohsec2-ohsec1
      sptime=sptime+ohsec2-ohsec1
      help(taskid)=0
c     call unlock22
      return
      end
