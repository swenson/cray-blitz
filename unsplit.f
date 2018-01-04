      subroutine unsplit
crev  last revision 10/23/91
c
c     ******************************************************************
c     *                                                                *
c     *     unsplit is called after the next to last process working   *
c     *  at a node has completed its analysis.  the data returned from *
c     *  the best of the parallel processes working with the current   *
c     *  process is compared with the data of the current process.  if *
c     *  it is useful, it is merged with the current processes data    *
c     *  so that this shared block can be de-allocated and reused.     *
c     *                                                                *
c     *    the major variable (result) used to control the parallel    *
c     *  search is defined as follows:                                 *
c     *                                                                *
c     *    result=0 --> normal result when a process completes the     *
c     *                 search it starts without being stopped.        *
c     *          =1 --> some process found a branch that refutes the   *
c     *                 move chosen at the previous ply.  all of the   *
c     *                 processes have been stopped.                   *
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
c------------------------------< cycle through each ply to determine
c------------------------------< if it is a split point.  if so, then
c------------------------------< if only one processor is still busy
c------------------------------< at this point, an unsplit is necessary.
c
      call cptime(ohsec1,msec)
      if(cputim .ne. 0) ohsec1=msec1
      usplit(taskid)=0
      do 500 tply=ply,1,-1
          blockid=dlevel(tply,taskid)
          if(blockid.ne.0 .and. nbusy(blockid).eq.1) then
              if(and(tply,1) .eq. 1) then
                  tmside=1
              else
                  tmside=-1
              endif
              if(and(debug,16) .ne. 0)
     *            print 100, taskid, blockid, qdata(672,blockid),
     *                      mated(tply), qdata(1449+tply,blockid), tply
100               format(1x,'unsplit  -  pid ',i2,' is unsplitting ',
     *                'block ',i2,'  result=',i1,' mf=',2i1,'  ply=',i2)
c
c------------------------------< determine if the shared data should
c------------------------------< be copied over the current process
c------------------------------< data or if it should be discarded.
c------------------------------< 1) results that say not-mated are
c------------------------------<    better than results that indicate
c------------------------------<    mate since probably one processor
c------------------------------<    had no moves to examine while
c------------------------------<    others did.
c------------------------------< 2) better results overwrite poorer
c------------------------------<    results if above rule is not
c------------------------------<    violated.
c
              ptemp=0
              if(tply .gt. 1)
     *           ptemp=qdata(1449+tply-1,blockid)*mated(tply-1)
              ltemp=qdata(1449+tply,blockid)*mated(tply)
              if(qdata(672,blockid) .ne. 1) then
                  if(mated(tply).ne.0 .or.
     *               qdata(1449+tply,blockid).eq.0) then
                      if(mated(tply).ne.0 .and.
     *                   qdata(1449+tply,blockid).eq.0) then
                          mated(tply)=qdata(1449+tply,blockid)
                          value(tply)=qdata(4953+tply,blockid)
                          cutmvs(tply)=qdata(1099+tply,blockid)
                          do 200 i=1,52
                              trace(i,tply)=
     *                              qdata(2301+i+52*(tply-1),blockid)
200                       continue
                      else if(tmside*value(tply) .lt.
     *                        tmside*qdata(4953+tply,blockid)) then
                          mated(tply)=qdata(1449+tply,blockid)
                          value(tply)=qdata(4953+tply,blockid)
                          cutmvs(tply)=qdata(1099+tply,blockid)
                          do 300 i=1,52
                              trace(i,tply)=
     *                              qdata(2301+i+52*(tply-1),blockid)
300                       continue
                      endif
                  endif
              else if(qdata(1449+tply,blockid) .eq. 0) then
                  call copyg(blockid)
                  curmvs(tply,taskid)=cutmvs(tply)
              endif
              if(tply .gt. 1) mated(tply-1)=ptemp
              mated(tply)=ltemp
c
c------------------------------< clear any remaining moves since they
c------------------------------< have all been searched.  some may be
c------------------------------< left over due to parallel processing
c------------------------------< peculiarities caused by timing.
c
              if(tply .gt. 1) then
                  do 400 j=first(tply),last(tply)
                      moves(j)=0
400               continue
              endif
c
c------------------------------< now clean up this split point so
c------------------------------< that the memory used by it will be
c------------------------------< available for use later.
c
              dlevel(tply,taskid)=0
              nbusy(blockid)=0
              stopping(blockid)=0
              alloc(blockid)=0
              atply(blockid)=0
          endif
500   continue
c
c------------------------------< next, reset the current split point
c------------------------------< data so that it points to the next
c------------------------------< active split point for this processor.
c
      block(taskid)=0
      splitl(taskid)=0
      do 600 i=1,ply
           if(dlevel(i,taskid) .ne. 0) then
               splitl(taskid)=i
               block(taskid)=dlevel(i,taskid)
           endif
600   continue
      if(stop(taskid) .ge. ply) stop(taskid)=0
      call cptime(ohsec2,msec)
      if(cputim .ne. 0) ohsec2=msec1
      overhead=overhead+ohsec2-ohsec1
      untime=untime+ohsec2-ohsec1
      return
      end
