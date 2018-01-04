      subroutine match
crev  last revision 08/29/90
c
c     ******************************************************************
c     *                                                                *
c     *      match is used to determine if the opponent's move matched *
c     *  the programs predicted response.  if it did, there are two    *
c     *  actions to be taken:                                          *
c     *                                                                *
c     *      1)  determine if think-on-the-opponent's-time has already *
c     *          found a move while waiting on the opponent.  if so,   *
c     *          no search is required, simply make the move that      *
c     *          has already been found, thereby saving time, and;     *
c     *                                                                *
c     *      2)  set the starting iteration depth to zero if the move  *
c     *          is not what was expected and clobber the principle    *
c     *          variation so that it will not be followed.            *
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
c------------------------------< did the human's move match the
c------------------------------< predicted move?
c
      if(foundm .ne. 0) then
          return=1
          if(pmtype .eq. mtype) then
              if(mtype.eq.castkg .or. mtype.eq.castqn) return
              if(pmto.eq.mto .and. pmfrom.eq.mfrom) return
          endif
      endif
c
c------------------------------< move didn't match predicted move,
c------------------------------< set the starting iteration depth
c------------------------------< to zero and clobber the variation
c------------------------------< so that the predicted move won't
c------------------------------< show up first and waste time.
c
      do 300 i=1,52
          trace(i,1)=0
300   continue
      return=0
      return
      end
