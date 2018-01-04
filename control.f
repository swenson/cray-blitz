      subroutine control(parent)
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *      control is used to manage the separate tree searches that *
c     *  are done in multiprocessing mode.                             *
c     *                                                                *
c     *      when parallel processes are created, they initially come  *
c     *  to control to wait for work.  control initially assigns each  *
c     *  task a unique id starting at 2 (since the initial process is  *
c     *  going to be known as 1.  these processes then loop waiting on *
c     *  block(taskid) to become non-zero.  this is their signal to    *
c     *  join in and help out on some branch.  when processes complete *
c     *  they return to control to wait for a request to help out on   *
c     *  some new branch.  when no work is available (such as between  *
c     *  searches when we are communicating with the operator) the     *
c     *  processors hang here.                                         *
c     *                                                                *
c     *      a special note concerns the original task (taskid=1)      *
c     *  since no special consideration is given to it during the      *
c     *  search.  when task 1 has no work to do, it also hangs in      *
c     *  control until one of the other tasks requests its help.  a    *
c     *  so-called symmetric approach.  however, when all task are     *
c     *  waiting on work, task 1 is allowed to return to driver so     *
c     *  the next search or iteration can be started.                  *
c     *                                                                *
c     *      note that a stop(taskid) of -9999 indicates that that     *
c     *  particular processor should stop.  for some reason the        *
c     *  operator has reduced the number of processors.                *
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
c------------------------------< first, if these are spawned processes,
c------------------------------< they each need a unique taskid (except
c------------------------------< for taskid=1).
c
c     tdata(8000)=setb60(loc(tdata(1)))
      if(ncpus .gt.1) help(1)=20
      if(parent .eq. 0) then
c         call lock22
              realid=realid+1
              taskid=realid
c         call unlock22
      else
          do 10 i=1,16
              block(i)=0
10        continue
      endif
      working(taskid)=1
      if(parent .eq. 1) go to 200
c
c------------------------------< this is the processor idle loop.  if a
c------------------------------< processor has no work to do, it will
c------------------------------< loop here until another process
c------------------------------< executes 'split' to obtain assistance.
c
100   continue
c     call lock22
          idle=idle+1
          helpid=taskid
102       continue
              helpid=helpid+1
              if(helpid .gt. ncpus) helpid=1
              if(working(helpid) .ne. 0) go to 104
          if(helpid .ne. taskid) go to 102
          helpid=taskid
104       continue
          help(helpid)=20
          if(and(debug,128) .ne. 0) print 101,taskid, helpid
101       format(1x,'control  -  pid ',i2,' sending help to pid',i3)
          working(taskid)=0
c     call unlock22
c
c------------------------------< idle loop starts here
c
      call cptime(ohsec1,msec)
      if(cputim .ne. 0) ohsec1=msec1
103   continue
          if(block(taskid) .eq. -1) then
              call cptime(ohsec1,msec)
              if(cputim .ne. 0) ohsec1=msec1
          endif
          if(stop(taskid) .eq. -9999) stop
          if(help(taskid) .ne. 0) then
c             call lock22
                  helpid=taskid
602               continue
                      helpid=helpid+1
                      if(helpid .gt. ncpus) helpid=1
                      if(working(helpid) .ne. 0) go to 603
                  if(helpid .ne. taskid) go to 602
                  helpid=taskid+1
                  if(helpid .gt. ncpus) helpid=1
603               continue
                  help(taskid)=0
                  help(helpid)=20
c             call unlock22
              if(and(debug,128) .ne. 0) then
                  print 601, taskid, helpid
601               format(1x,'control  -  pid ',i2,' forwarding '
     *                   'help request to pid ',i2)
              endif
          endif
      if(block(taskid).le.0 .and.
     *          .not.(taskid.eq.1.and.idle.eq.ncpus)) go to 103
      call cptime(ohsec2,msec)
      if(cputim .ne. 0) ohsec2=msec1
      idlet(taskid)=idlet(taskid)+ohsec2-ohsec1
c
c------------------------------< this processor now has work to do.
c
      working(taskid)=1
      if(splitl(taskid) .ne. 0)
     *       tprocs(splitl(taskid))=tprocs(splitl(taskid))+1
      if(parent.eq.1 .and. idle.eq.ncpus) then
c         call lock22
              idle=idle-1
c         call unlock22
          do 145 blockid=1,16
              if(alloc(blockid) .eq. -1) then
                  if(and(debug,128) .ne. 0) print 144, blockid
144               format(1x,'control  -  copying block ',i2,
     *                   ' to parent task for return')
                  call copyg(blockid)
              endif
145       continue
          if(and(debug,128) .ne. 0) then
              print 109
109           format(1x,'control  -  parent returning. ',i2,'.')
          endif
          do 155 i=1,16
              block(i)=-1
155       continue
          return
      endif
      if(and(debug,128) .ne. 0) then
          print 108, taskid, block(taskid)
108       format(1x,'control  -  pid ',i2,' working on block ',i2,'.')
      endif
c
c------------------------------< this processor now has work to do.
c------------------------------< set things up and call search to
c------------------------------< do the parallel search.
c
          blockid=block(taskid)
c         call lock23
          call copyg(blockid)
c         call unlock23
c
c------------------------------< backup search to the split point
c------------------------------< since it is currently too far down
c------------------------------< in the tree (because we are splitting
c------------------------------< before the current ply.)
c
150       continue
          if(ply .le. splitl(taskid)) go to 200
              rdepth=odepth(ply)
              ply=ply-1
              side=-side
              player=3-player
              mfrom=from(ply)
              mto=to(ply)
              mtype=type(ply)
              movgpc=board(mto)
              mcappc=cappc(ply)
              call unmake
          go to 150
200   continue
c
c------------------------------< now call search to process the work
c------------------------------< in parallel.
c
          call search
c
c------------------------------< now accumulate time and node
c------------------------------< information for statistical display
c------------------------------< later.
c
          call cptime(ohsec1,msec)
          if(cputim .ne. 0) ohsec1=msec1
c         call lock22
              rnodes=rnodes+nodes
              rhashes=rhashes+hashes
              rphashs=rphashs+phashs
              rkhashs=rkhashs+khashs
              rpcalls=rpcalls+pcalls
              totnodes(taskid)=totnodes(taskid)+nodes
              if(nodes.eq.0 .and. stop(taskid).eq.0)
     *           falses(splitl(taskid))=falses(splitl(taskid))+1
              splitl(taskid)=0
              block(taskid)=0
              stop(taskid)=0
c         call unlock22
          if(ncpus .eq. 1) go to 100
c
c------------------------------< now for a kludge.  since we don't
c------------------------------< differentiate between processes, we
c------------------------------< have no idea which process actually
c------------------------------< completed the search and returned
c------------------------------< the correct value.  therefore, each
c------------------------------< process that completes a search that
c------------------------------< backs up to ply=1 must save it's
c------------------------------< search data so that process 1 can
c------------------------------< copy it and return to the main
c------------------------------< program.  if several return with ply=1,
c------------------------------< we remember the best value.  the data
c------------------------------< is copied to a global section so that
c------------------------------< process 1 can then access it and
c------------------------------< return the correct information.
c
250       continue
              if(ply .le. 1) then
c                 call lock22
                  do 260 blockid=1,16
                      if(alloc(blockid) .lt. 0) go to 275
260               continue
                  do 270 blockid=1,16
                      if(alloc(blockid) .eq. 0) go to 280
270               continue
c                 call unlock22
                  print 276, taskid
276               format(1x,'pid ',i2,'waiting for block in control')
                  go to 250
275           continue
                  if(value(1) .le. qdata(4954,blockid)) then
c                     call unlock22
                      go to 285
                  endif
280               continue
                      alloc(blockid)=-1
                      if(and(debug,128) .ne. 0) print 281, taskid,
     *                                                    blockid
281                   format(1x,'control  -  pid ',i2,' copying ',
     *                       'to block ',i2,' for parent task')
                      ply=1
c                     call lock23
                      call copyl(blockid)
c                     call unlock23
c                     call unlock22
              endif
285       continue
c
c------------------------------< this process is now idle.
c
          block(taskid)=0
          if(abort .ne. 0) go to 100
c
c------------------------------< since this process now has no work to
c------------------------------< do, first check all of the active split
c------------------------------< points to see if we can join in and help
c------------------------------< out with little or no overhead.
c
          lowply=99
          lowplyp=0
          do 700 i=1,16
              if(alloc(i).gt.0 .and. nbusy(i).gt.1 .and.
     *           stopping(i).eq.0 .and. qdata(672,i).eq.2 .and.
     *           lowply .gt. atply(i)) then
                  left=0
                  do 600 j=qdata(1149+atply(i),i),
     *                            qdata(1399+atply(i),i)
                      if(atply(i) .gt. 1) then
                           if(qdata(5599+j,i) .ne. 0) left=left+1
                      else
                           if(tried(j) .eq. 0) left=left+1
                      endif
600               continue
                  if(left .gt. 0) then
                      lowply=atply(i)
                      lowplyp=i
                  endif
              endif
700       continue
          if(lowplyp .ne. 0) then
c             call lock22
              if(alloc(lowplyp).ne.0 .and. nbusy(lowplyp).gt.1 .and.
     *           qdata(672,lowplyp).eq.2) then
                  procid=0
                  do 750 i=1,16
                      if(dlevel(atply(lowplyp),i) .eq. lowplyp) procid=i
750               continue
                  if(procid .eq. 0) then
                      print 755,taskid, lowplyp
755                   format(1x,'****error, pid',i2,
     *                       ' cant find cpu using block ',i2)
                      go to 100
                  endif
                  do 800 i=1,atply(lowplyp)
                      dlevel(i,taskid)=dlevel(i,procid)
                      if(dlevel(i,taskid) .ne. 0)
     *                 nbusy(dlevel(i,taskid))=nbusy(dlevel(i,taskid))+1
800               continue
                  do 805 i=1,atply(lowplyp)-1
                      curmvs(i,taskid)=curmvs(i,procid)
805               continue
                  do 810 i=atply(lowplyp),50
                      curmvs(i,taskid)=0
810               continue
                  block(taskid)=lowplyp
                  splitl(taskid)=atply(lowplyp)
                  call cptime(ohsec2,msec)
                  if(cputim .ne. 0) ohsec2=msec1
                  overhead=overhead+ohsec2-ohsec1
                  cotime=cotime+ohsec2-ohsec1
c                 call unlock22
                  if(and(debug,128) .ne. 0) then
                      print 850, taskid, lowplyp
850                   format(1x,'control  -  pid ',i2,
     *                       ' joining split in progress, block ',i2)
                  endif
                  go to 103
              else
                  call cptime(ohsec2,msec)
                  if(cputim .ne. 0) ohsec2=msec1
                  overhead=overhead+ohsec2-ohsec1
                  cotime=cotime+ohsec2-ohsec1
c                 call unlock22
              endif
          endif
          go to 100
      end
