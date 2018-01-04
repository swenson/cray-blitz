      subroutine merge
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *     merge is called after each thread of a multiprocessing     *
c     *  search has completed its analysis.  the respective values     *
c     *  are merged together into the primary data structures.         *
c     *                                                                *
c     *    the major variables (result and stop) used to control the   *
c     *  parallel search are defined as follows:                       *
c     *                                                                *
c     *    result=0 --> normal result when a process completes the     *
c     *                 search it starts without being stopped.        *
c     *          =1 --> this process found a branch that refutes the   *
c     *                 move chosen at the previous ply.  all of the   *
c     *                 processes should be stopped so that the        *
c     *                 cutoff can be taken. this is not done if the   *
c     *                 mated flag is still set indicating a mate or   *
c     *                 draw.  just because one processor is mated     *
c     *                 doesn't mean they all are.                     *
c     *                                                                *
c     *      stop=0 --> normal case.  all processes continue.          *
c     *          =n --> all running processes should stop and back     *
c     *                 up to level 'n' immediately.  something has    *
c     *                 happened to make their search meaningless.     *
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
      include 'perm.f'
c
c
c------------------------------< determine if this parallel tasks'
c------------------------------< values are the correct values to
c------------------------------< back up to the parent task.
c
c     call lock22
      call cptime(ohsec1,msec)
      if(cputim .ne. 0) ohsec1=msec1
      return=0
      blockid=block(taskid)
      if(and(debug,32) .ne. 0) then
          print 100, taskid, blockid, result, nbusy(blockid),
     *               mated(ply), qdata(1449+ply,blockid), ply
100        format(1x,'merge    -  pid',i3,' block',i3,' result=',
     *            i1,' busy=',i2,' mf=',2i1,'  ply=',i2)
      endif
c
c------------------------------< if more than one processor is still
c------------------------------< busy at this split point, a merge
c------------------------------< operation is required.
c
c     call lock23
      if(result .eq. 1) then
          qdata(1099+ply,blockid)=curmvs(ply,taskid)
          cutmvs(ply)=curmvs(ply,taskid)
      endif
      if(nbusy(blockid) .gt. 1) then
          ptemp=0
          if(ply .gt. 1) ptemp=qdata(1449+ply-1,blockid)*mated(ply-1)
          ltemp=qdata(1449+ply,blockid)*mated(ply)
          if(qdata(672,blockid) .eq. 2 .or.
     *       mated(ply).eq.0 .and.
     *       qdata(1449+ply,blockid).ne.0) then
              call copyl(blockid)
              if(and(debug,32) .ne. 0) print 150, taskid, value(ply)
150           format(1x,'merge    -  pid',i3,' new value = ',i9)
          else if(mated(ply).eq.0 .or.
     *            qdata(1449+ply,blockid).ne.0) then
              if(qdata(672,blockid).ne.1 .and. result .eq. 1) then
                      call copyl(blockid)
                      if(and(debug,32) .ne. 0)
     *                         print 150, taskid, value(ply)
              else if(side*value(ply) .gt.
     *                side*qdata(4953+ply,blockid)) then
                  call copyl(blockid)
                  if(and(debug,32) .ne. 0)
     *                     print 150, taskid, value(ply)
              endif
          endif
c
c------------------------------< account for mated flags from all of
c------------------------------< the processors.
c
          if(ply .gt. 1) qdata(1449+ply-1,blockid)=ptemp
          qdata(1449+ply,blockid)=ltemp
c
c------------------------------< determine if other processors should
c------------------------------< be stopped due to what this processor
c------------------------------< found.  note that we do not stop other
c------------------------------< processors if the mated flat is set.
c------------------------------< they need to completely search their
c------------------------------< branch to determine if the mate/draw
c------------------------------< is forced.  if not, test to see if only
c------------------------------< one processor is still busy at this
c------------------------------< split point. if so, an unsplit must
c------------------------------< be done to conserve memory.
c
          if(result.eq.1 .and. mated(ply).eq.0) then
              stopping(blockid)=1
              do 300 i=1,ncpus
                  if(blockid.eq.dlevel(ply,i) .and.
     *               i.ne.taskid)                     then
                      if(stop(i).lt.ply .and.
     *                   stop(i).ne.0) go to 300
                          if(and(debug,32) .ne. 0) then
                              print 200, taskid, i, ply, blockid
200                           format(1x,'merge    -  ',
     *                            'pid ',i2,' sending stop',
     *                            ' to pid ',i2,
     *                            ' ply=',i2,' block ',i2)
                          endif
                          stop(i)=ply
                          go to 300
                  endif
300           continue
          endif
c
c------------------------------< if only one processor is busy, it
c------------------------------< is the one doing this merge call.
c------------------------------< substitute an unsplit operation
c------------------------------< instead.
c
      else
          call cptime(ohsec2,msec)
          if(cputim .ne. 0) ohsec2=msec1
          overhead=overhead+ohsec2-ohsec1
          metime=metime+ohsec2-ohsec1
          call unsplit
          return=1
c         call unlock23
c         call unlock22
          return
      endif
c     call unlock23
c
c------------------------------< this processor is now done at this
c------------------------------< (and possibly other) split level(s).
c------------------------------< if return=0 then this is not the
c------------------------------< last processor at this split point.
c------------------------------< this processor will therefore return
c------------------------------< thru search and go back to control
c------------------------------< to split again.  clean up all of the
c------------------------------< parallel data structures accordingly.
c
c
      if(return .eq. 0) then
          do 600 i=1,ply
              if(dlevel(i,taskid) .ne. 0) then
                  blk=dlevel(i,taskid)
                  nbusy(blk)=nbusy(blk)-1
                  if(nbusy(blk) .eq. 0) alloc(blk)=0
                  dlevel(i,taskid)=0
                  if(nbusy(blk) .eq. 1) then
                      do 500 j=1,16
                          if(dlevel(i,j) .eq. blk) then
                              if(and(debug,32) .ne. 0) then
                                  print 400, taskid, j, blk
400                               format(1x,'merge    -  pid ',
     *                                   i2,' sending unsplit',
     *                                   ' to pid ',i2,' block',
     *                                   i3)
                              endif
                              usplit(j)=1
                          endif
500                   continue
                  endif
              endif
600       continue
      else
c
c------------------------------< otherwise, this split point is now
c------------------------------< unused.  update the parallel data
c------------------------------< structures to reflect the next (going
c------------------------------< backward) split point so that it will
c------------------------------< be recognized properly by search.
c
          nbusy(blockid)=nbusy(blockid)-1
          splitl(taskid)=0
          block(taskid)=0
          dlevel(ply,taskid)=0
          do 800 i=1,ply
              if(dlevel(i,taskid) .ne. 0) then
                  block(taskid)=dlevel(i,taskid)
                  splitl(taskid)=i
              endif
800       continue
      endif
c
c------------------------------< this processor is now done at this
c------------------------------< split level.
c
      call cptime(ohsec2,msec)
      if(cputim .ne. 0) ohsec2=msec1
      overhead=overhead+ohsec2-ohsec1
      metime=metime+ohsec2-ohsec1
c     call unlock22
      return
      end
