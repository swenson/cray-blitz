      function typenode(testply)
crev  last revision 04/26/89
c
c     ******************************************************************
c     *                                                                *
c     *     typenode is used to determine whether the current node is  *
c     *  a candidate for parallel process splitting.  this algorithm   *
c     *  is based on "An analysis of alpha-beta pruning(1975)" by      *
c     *  Knuth.  type 1 and type 3 nodes are candidates for splitting  *
c     *  since every move at that type of node will be examined        *
c     *  (assuming the tree is ordered correctly).                     *
c     *     if the upper/lower bounds are unknown (+/- infinity or     *
c     *  the equivalent (because of the restricted search bounds set   *
c     *  to speed up the search), then the node is a type 1 node that  *
c     *  must be examined completely if the tree is ordered correctly. *
c     *     if only the lower bound is unknown, then nodes at which    *
c     *  min is on move are type 2 nodes.  if only the upper bound is  *
c     *  unknown, then nodes at which max is on move are type 2 nodes. *
c     *  these nodes will usually only have one branch examined.       *
c     *     all other nodes are type three and will normally have all  *
c     *  branches examined and are therefore good candidates for       *
c     *  splitting across multiple processors.                         *
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
c------------------------------< determine lower bound (alpha)
c
      onmove=2-and(testply,1)
      alphav=value(or(min0(testply,ply)-1,1))
c
c------------------------------< determine upper bound (beta)
c
      if(min0(testply,ply) .lt. 2) then
          betav=window(2)
      else
          betav=value(and(min0(testply,ply),254))
      endif
c
c------------------------------< now determine whether the node
c------------------------------< is type 1, 2, or 3 based on the
c------------------------------< current testply and bounds.
c
      if(alphav.eq.window(1) .and. betav.eq.window(2)) go to 100
      if(onmove .eq. 1) then
          if(alphav .eq. window(1)) go to 200
      else
          if(betav .eq. window(2)) go to 200
      endif
      go to 300
c
c------------------------------< node is type 1
c
100   continue
          typenode=1
          return
c
c------------------------------< node is type 2.
c
200   continue
          typenode=2
          return
c
c------------------------------< node is type 3
c
300   continue
          typenode=3
          return
      end
