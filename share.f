      subroutine share
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *     a special case handled by share occurs when a new score    *
c     *  is backed up to any split point.  this score needs to be      *
c     *  sent to all processors working at this node since it can help *
c     *  reduce the tree size through alpha/beta cutoffs.  whenever    *
c     *  backup finds a new best scoe, it calls sharev to copy it to   *
c     *  the shared data area for this split ply.  share is then       *
c     *  called for each processor splitting at this ply so that it    *
c     *  can share the new backed up value and generate more cutoffs.  *
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
c------------------------------< a new backed-up value has been found.
c------------------------------< make sure it is better than what we
c------------------------------< have found and if so copy it to our
c------------------------------< local data area.
c
      shareit(taskid)=0
      do 400 tply=1,ply
          if(dlevel(tply,taskid) .ne. 0) then
              blockid=dlevel(tply,taskid)
              if(and(tply,1) .ne. 0) then
                  mside=1
              else
                  mside=-1
              endif
c             call lock23
              if(qdata(1449+tply,blockid).eq.0 .and.
     *            mside*value(tply) .lt. 
     *                     mside*qdata(4953+tply,blockid)) then
                  tval=value(tply)
                  do 100 cply=tply,ply
                      if(value(cply) .eq. tval) 
     *                           value(tply)=qdata(4953+tply,blockid)
100               continue
                  do 200 i=1,52
                      trace(i,tply)=qdata(2301+(tply-1)*52+i,blockid)
200               continue
                  if(and(debug,64) .ne. 0) print 300, taskid, 
     *                                                 blockid, tply
300               format(1x,'share    -  pid ',i2,' block ',i2,
     *                   '  ply ',i2)
              endif
c             call unlock23
          endif
400   continue
      return
      end
      subroutine sharev
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *     sharev is called by backup whenever it backs up a new      *
c     *  score to a level that is a split point.  sharev copies the    *
c     *  score and variation to the shared area (if it is better than  *
c     *  anything found there yet) so that share can the copy it to    *
c     *  each individual process that is splitting at that ply.  after *
c     *  the copy operation is completed, the 'shareit' flag is set    *
c     *  for each processor splitting at this point so that they will  *
c     *  then call share to copy this shared information to their own  *
c     *  local common.                                                 *
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
c------------------------------< a new backed-up value has been found.
c------------------------------< make sure it is better than what we
c------------------------------< have found and if so copy it to the
c------------------------------< shared data area.
c
      tply=ply-1
      blockid=dlevel(tply,taskid)
      if(and(debug,64) .ne. 0) print 100, taskid, blockid, tply
100   format(1x,'sharev   -  pid ',i2,' block ',i2,'  ply ',i2)
      if(and(tply,1) .ne. 0) then
          mside=1
      else
          mside=-1
      endif
c     call lock23
      if(mside*value(tply) .gt. mside*qdata(4953+tply,blockid)) then
          qdata(4953+tply,blockid)=value(tply)
          do 200 i=1,52
              qdata(2301+(tply-1)*52+i,blockid)=trace(i,tply)
200       continue
          do 300 i=1,16
              if(dlevel(tply,i) .eq. dlevel(tply,taskid) .and.
     *           i.ne.taskid)                            shareit(i)=1
300       continue
      endif
c     call unlock23
      return
      end
