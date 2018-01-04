         include   'common'       
         ident     store1         
         entry     STORE1         
         ext       STORE5         
         
*   ******************************************************************
*   *                                                                *
*   *      store 1&2 are used to enter positions into the hash table *
*   *  for later use by 'lookup'. if a collision occurs, the current *
*   *  position will overlay the table  position if the table        *
*   *  position is from an earlier iteration or if the current       *
*   *  position is at a lower (numeric) rdepth than the table        *
*   *  position. store2 is used when the position value is unknown.  *
*   *      store uses the double hash algorihm to resolve all        *
*   *  collisions.  it tries to resolve collisions in two passes;    *
*   *  pass one attempts to find a slot that is empty or that was    *
*   *  stored from an earlier move.  pass two tries to find a        *
*   *  position stored during the current move that was stored       *
*   *  from a greater rdepth than the current search rdepth.  the    *
*   *  entire table  is not searched, just n random entries (n=4).   *
*   *                                                                *
*   *      table format for cray-1 is as follows (64 bit word):      *
*   *                                                                *
*   *         word 1   1 bit  : 1=old position, 0=current position   *
*   *                  1 bit  : onerep(ply) flag                     *
*   *                 22 bits : position's value + 2000000           *
*   *                 40 bits : hashed board position                *
*   *                                                                *
*   *         word 2   7 bits : level of search from this position   *
*   *                           plus a bias of 64                    *
*   *                  2 bits : table entry type (good, bound, etc.) *
*   *                 23 bits : suggested move for this position     *
*   *                                                                *
*   *    96-bits are used.  two word 2's are packed together.        *
*   *    storage is laid out as follows:                             *
*   *      hsize+6144 words  of  player1's word 1's                  *
*   *      hsize+6144 words with player1's word 2's in right half    *
*   *                        and player2's word 2's in left half     *
*   *      hsize+6144 words  of  player2's word 1's                  *
*   *                                                                *
*   *     the 6144 comes from rindex value, currently 1024,          *
*   *     times the number of colsns, currently 7.                   *
*   *     two king hash tables of length hsize/4 are placed          *
*   *     after the regular hash table.                              *
*   *     in addition a pawn hash table is tacked on at the end      *
*   *     of the king hash tables. its length is hsize/8.            *
*   *     a passed pawn hash table of size hsize/32 has been         *
*   *     added at the end of the pawn hash table. the pawn table.   *
*   *    total hash space=3*(hsize+7168)+hsize/8+hsize/2+5*hsize/32
*   *    the current hsize default is 32768, htable has 141338 words.*
*   *                                                                *
*   ******************************************************************
*
         align     
possep   =         *              htbl1
         a7        b60            
         a6        cfiles-1,a5    
         a5        a5+a7          
         a7        board-1,a5     
         a1        board-0,a5     
         a5        board-2,a5     
         a0        a7+a4          
         s2        random-1,a6    
         a6        b46            restore player
         a6        a6-1           restore player-1
         jan       l100           
         a0        a1-a4          
         s6        <10            
         s6        s6<24          
         jan       ckosp          
         s1        s1\s2          
         s6        s6&s1          
         s6        s6>24          
         s7        1              
         s6        s7!s6          
         a2        s6             
         j         l100           
ckosp    =         *              
         a0        a5-a4          
         jan       l100           
         s1        s1\s2          
         s6        s6&s1          
         s6        s6>24          
         s7        1              
         s6        s7!s6          
         a2        s6             
         j         l100           
STORE1   =         *              
         s5        debug,
         s7        4
         a7        b60            task common base address
         a5        ply,a7         
         s1        hash,a7        
         a4        side,a7        
         s6        <10            1023
         s3        hsizm1,        
         a3        htbl1,         
         s4        a5             save ply
         a5        a5+a7          
      b30       a5             save ply+base
         s2        moveds-2,a5    moveds(ply-1)
         a2        moveds-1,a5    moveds(ply)
         a6        player,a7      
         a7        from-2,a5      from(ply-1)
         s2        s2<2           
         s0        s5&s7
         a1        s2             
         a2        a1+a2          ndx=moveds(ply)+moved(ply-1).ls.2
         s5        random+100,a2  random(ndx+1,2)
         jsn       l9999
         a1        20             
         a5        to-2,a5        to(ply-1)
         s6        s6<24          mask for rindex
         a1        a1*a4          20*side
         b44       a4             save side
         b43       a3             save htbl1
         s1        s1\s5          hashbd
         s6        s6&s1          rindex.ls.24
         s7        1              for or of rindex
         s6        s6>24          rindex?
         a7        a7-a1          from-20
         s6        s7!s6          rindex
         b46       a6             save player
         a2        s6             rindex
         a0        a7-a5          from-to-20
         a6        a6-1           player-1
*     s5        npmovs,        move=npmovs+nomovs
         jaz       possep         
*   s1=hashbd,a2=rindex,s3=hsize-1,a3=htbl2,a6=player-1
l100     =         *              
      a1        b30            ply+base
         t41       s1             hashbd
         a3        a3+a3          htbl2
         s3        s3&s1          and(hash,hsizm1)
         a4        32             mask size
      s5        onerep-1,a1    onerep(ply)
         a5        a6*a4          hshft
*     s2        nomovs,        drop if nmoves right
         a3        a3*a6          (player-1)*htbl2
         s6        <32            
         a4        a4-a5          32 if player 1, 0 otherwise
         s6        s6<a4          hmask
         b45       a5             save hshft
         t46       s6             save hmask
*     s6        pndrng,        now equal 1 if true
         a1        s3             hkey
         a7        htable         base adr of table
         a1        a1+a7          loc(htable(hkey))
         a7        b60            
         s7        rdepth,a7      
         a7        8              number of colsns (+1)
         a0        a1+a3          loc(htable(rkey1))
         vl        a7             
         v0        ,a0,a2         htable(rkey1)
*     s5        s5+s2          drop if nmoves okay
*     s2        >2             mmask
         b42       a0             hold for store5
         a7        24             64-hbsiz
         v1        v0<a7          (htable.and.bmask).ls.(64-hbsiz)
*     s5        s5-s6          movenm; for civic use s5-s6
         a3        b43            
         a7        b60            
         a0        a1+a3          loc(htable(rkey2))
*     movenm,a7 s5             save movenm for store5
*     v7        s2&v0          extract age flag
      a7        63             shift right to extract age
      v7        v0>a7          extract age flag
         v4        ,a0,a2         htable(rkey2)
         s1        s1>24          now use left 40 bits
*     s5        s5<62          movenm.ls.62
      s5        s5<62          onerep(ply).ls.62
         s1        s1<24          hashbd.ls.(64-hbsiz)
         a7        25             
         v2        s1-v1          any hits?
         s6        s7-s4          rdepth-ply
         s7        64             64
         vm        v2,z           any hits?
         v3        v4>a5          htable(rkey2).rs.hshft
         s2        <32            
         a5        s4             ply
         v2        s2&v3          extract htable(rkey2)
         s2        vm             save hash hits
         s6        s6+s7          rdepth-ply+64
*     v6        s5-v7          same move?
         v5        v2>a7          mlevel+64
*     vm        v6,n           any not same move?
      vm        v7,n           any old positions?
         s7        <2             sentinel mask bits
         s0        s2             any hash hits?
         a3        zs2            hold for test
         jsn       l400m          hash hit
         a7        a7-a7          0 with delay
         s2        v5,a7          best (here 64 acts like 0)
         s4        v5,a0          better?
         s0        vm             
         s3        vm             move hits
         a1        zs3            rkey1 if not move
         jsn       l1000          
l102     =         *              
         s0        s2-s4          
         a7        2              next try
         s3        v5,a7          better
         a1        0              bestp
         jsm       skip1          if still best
         s2        s4             new best
         a1        1              new bestp
skip1    =         *
         s0        s2-s3          
         a7        3              
         s4        v5,a7          
         jsm       skip2          
         s2        s3             
         a1        2              
skip2    =         *
         s0        s2-s4          
         a7        4              delete
         s3        v5,a7          this
         jsm       skip3          set
         s2        s4             of
         a1        3              ten
skip3    =         *
         s0        s2-s3          commands
         a7        5              to go
         s4        v5,a7          back
         jsm       skip4          to three
         s2        s3             retries
         a1        4              delete line to go back to five retries
skip4    =         *
         s0        s2-s4          delete line to go back to five retries
         a7        6              delete
         s3        v5,a7          this
         jsm       skip5          set
         s2        s4             of
         a1        5              ten
skip5    =         *
         s0        s2-s3          commands
         a7        7              to go
         s4        v5,a7          back
         jsm       skip6          to five
         s2        s3             retries
         a1        2+2+2          delete +2 to go back by two retries
skip6    =         *
         s0        s2-s4          
         jsm       skip7          
         s2        s4             
         a1        3+2+2          delete +2 to go back by two retries
skip7    =         *
         s0        s6-s2          
         s3        v5,a1          mlevel+64
         s4        v3,a1          htable(rkey2).rs.hshft
         jsp       l400           
         j         b0             no store
*    a1=rkey1,s6=rdepth-ply+64,s5=0,a2=rindex,s1=hashbd<24,a3=rkey(hash)
l400m    =         *              
         s2        vm             move hits?
         a1        zs2            rkey1 if not move leftmost
         a0        a1-a3          
         s3        v5,a3          mlevel+64
         s4        v3,a3          htable(rkey2).rs.hshft
         jam       l1000          
         a1        a3             
l400     =         *              
         s0        s6-s3          replace?
         s2        1              
         s7        s7<23          
         s2        s2<23          
         jsp       l1000          
         s7        s7&s4          type
         s0        s2\s7          
         jsn       l1000          
         j         b0             no store
*   a1=rkey1,a2=rindex,a5=ply,s6=rdepth-ply+64,s5=0,s1=hash<24
l1000    =         *              
         a6        b60            
         a5        a5+a6          ply+base
         a2        a2*a1          rindex of hit
         s7        value-1,a5     value(ply)=w0
         s2        value-3,a5     value(ply-2)=w2
         s4        value-2,a5     value(ply-1)=w1
         a0        b44            side
         a3        b42            loc htable(rkey1)
         s1        s1>24          realign hashbd
         a2        a2+a3          loc(htable(rkey1))+rindex of hit
         s5        s1+s5          movenm<62+hashbd
         s3        2000000        
         jap       l1050p         
l1050m   =         *              
         s2        s7-s2          w0-w2
         s0        s4-s7          plus if w0.le.w1
         s2        s2>63          capture sign bit
         a0        s2             zero if w0.ge.w2
         jsp       l1100m         
         jaz       l1100m         
*   a2=rkey1,b46=player,a4=side,a5=ply,s6=rdepth-ply+64,s5=hash
l560m    =         *              
         a7        htable         
         a2        a2-a7          reset rkey for store5
         s6        t46            
         a7        b0             save b0
         a2        a2+1           rkey1
         b64       a7             put b0 away
         a3        b43            htbl1
         a7        b60            
         rkey1,a7  a2             save for store5
         a4        a2-a3          rkey2
         s1        t41            
         rkey2,a7  a4             save for store5
         hashbd,a7 s1             save for store5
         hmask,a7  s6             save for store5
         a5        b45            hshft
         hshft,a7  a5             save for store5
*    this critical section needs protection
         vwd       10/o'34,6/o'20 call lockon(store)
         r         STORE5         currently fortran
         vwd       10/o'36,6/o'20 call lockoff(store)
*    end of critical section needs protection
         j         b64            return
*    s7=value(ply),a2=rkey for store,s3=2000000
l1100m   =         *              
         a3        b43            htbl1
         a4        a2-a3          rkey2
         s4        s3+s7          2000000+value(ply)
*    this critical section needs protection
         vwd       10/o'34,6/o'20 call lockon(store)
         s1        0,a4           get current htable(rkey2)
         s2        1              
         s6        s6<25          d-p+64
         s2        s2<24          2.ls.23
         s4        s4<40          position 2000000+value(ply)
         a5        b45            hshft
         s2        s6!s2          (d-p+64)<25+2<23
         s5        s4!s5          htb(1)
         0,a2      s5             htable(rkey1)=
         s6        t46            hmask
         s2        s2<a5          position level+sentinel
         s1        s1&s6          clear half of htable(rkey2)
         s4        s2!s1          merge other half
         0,a4      s4             htable(rkey2)=
         vwd       10/o'36,6/o'20 call lockoff(store)
*    end of critical section needs protection
         j         b0             
l1050p   =         *              side=+1
*    s7=value(ply),a2=rkey for store,s3=2000000
         s4        s7-s4          w0-w1
         s0        s2-s7          plus if w0.le.w2
         s4        s4>63          capture sign bit
         a0        s4             zero if w0.ge.w1
         jsp       l1100p         
         jaz       l1100p         
*   a2=rkey1,b46=player,a4=side,a5=ply,s6=rdepth-ply+64,s5=hash
l560p    =         *              
         a7        htable         
         a2        a2-a7          reset rkey for store5
         s6        t46            
         a7        b0             save b0
         a2        a2+1           rkey1
         b64       a7             put b0 away
         a3        b43            htbl1
         a7        b60            
         rkey1,a7  a2             save for store5
         a4        a2+a3          rkey2
         s1        t41            
         rkey2,a7  a4             save for store5
         hashbd,a7 s1             save for store5
         hmask,a7  s6             save for store5
         a5        b45            hshft
         hshft,a7  a5             save for store5
*    this critical section needs protection
         vwd       10/o'34,6/o'20 call lockon(store)
         r         STORE5         currently fortran
         vwd       10/o'36,6/o'20 call lockoff(store)
*    end of critical section needs protection
         j         b64            return
l1100p   =         *              
         a3        b43            htbl1
         a4        a2+a3          rkey2
         s4        s3+s7          2000000+value(ply)
*    this critical section needs protection
         vwd       10/o'34,6/o'20 call lockon(store)
         s1        0,a4           get current htable(rkey2)
         s2        1              
         s6        s6<25          d-p+64
         s2        s2<24          2.ls.23
         s4        s4<40          position 2000000+value(ply)
         a5        b45            hshft
         s2        s6!s2          (d-p+64)<25+2<23
         s5        s4!s5          htb(1)
         0,a2      s5             htable(rkey1)=
         s6        t46            hmask
         s2        s2<a5          position level+sentinel
         s1        s1&s6          clear half of htable(rkey2)
         s4        s2!s1          merge other half
         0,a4      s4             htable(rkey2)=
         vwd       10/o'36,6/o'20 call lockoff(store)
*    end of critical section needs protection
l9999    =         *
         j         b0             
         include   'global'       
         end       
         ident     store2         
         entry     STORE2         
         align     
possep   =         *              htbl1
         a7        b60            
         a6        cfiles-1,a5    
         a5        a5+a7          
         a7        board-1,a5     
         a1        board-0,a5     
         a5        board-2,a5     
         a0        a7+a4          
         s2        random-1,a6    
         a6        b46            restore player
         a6        a6-1           restore player-1
         jan       l100           
         a0        a1-a4          
         s6        <10            
         s6        s6<24          
         jan       ckosp          
         s1        s1\s2          
         s6        s6&s1          
         s6        s6>24          
         s7        1              
         s6        s7!s6          
         a2        s6             
         j         l100           
ckosp    =         *              
         a0        a5-a4          
         jan       l100           
         s1        s1\s2          
         s6        s6&s1          
         s6        s6>24          
         s7        1              
         s6        s7!s6          
         a2        s6             
         j         l100           
STORE2   =         *              
         s5        debug,
         s7        4
         a7        b60            task common base address
         a5        ply,a7         
         s1        hash,a7        
         a4        side,a7        
         s6        <10            1023
         s3        hsizm1,        
         a3        htbl1,         
         s4        a5             save ply
         a5        a5+a7          
      b30       a5             save ply+base
         s2        moveds-2,a5    moveds(ply-1)
         a2        moveds-1,a5    moveds(ply)
         a6        player,a7      
         a7        from-2,a5      from(ply-1)
         s2        s2<2           
         s0        s5&s7
         a1        s2             
         a2        a1+a2          ndx=moveds(ply)+moved(ply-1).ls.2
         s5        random+100,a2  random(ndx+1,2)
         jsn       l9999
         a1        20             
         a5        to-2,a5        to(ply-1)
         s6        s6<24          mask for rindex
         a1        a1*a4          20*side
         b44       a4             save side
         b43       a3             save htbl1
         s1        s1\s5          hashbd
         s6        s6&s1          rindex.ls.24
         s7        1              for or of rindex
         s6        s6>24          rindex?
         a7        a7-a1          from-20
         s6        s7!s6          rindex
         b46       a6             save player
         a2        s6             rindex
         a0        a7-a5          from-to-20
         a6        a6-1           player-1
*     s5        npmovs,        move=npmovs+nomovs
         jaz       possep         
*   s1=hashbd,a2=rindex,s3=hsize-1,a3=htbl2,a6=player-1
l100     =         *              
      a1        b30            ply+base
         t41       s1             hashbd
         a3        a3+a3          htbl2
         s3        s3&s1          and(hash,hsizm1)
         a4        32             mask size
      s5        onerep-1,a1    onerep(ply)
         a5        a6*a4          hshft
         a3        a3*a6          (player-1)*htbl2
*     s2        nomovs,        drop if nmoves right
         s6        <32            
         a4        a4-a5          32 if player 1, 0 otherwise
         s6        s6<a4          hmask
         b45       a5             save hshft
         t46       s6             save hmask
*     s6        pndrng,        
         a1        s3             hkey
         a7        htable         base adr of table
         a1        a1+a7          loc(htable(hkey))
         a7        b60            
         s7        rdepth,a7      
         a7        8              number of colsns (+1)
         a0        a1+a3          loc(htable(rkey1))
         vl        a7             
         v0        ,a0,a2         htable(rkey1)
*     s5        s5+s2          drop if nmoves okay
*     s2        >2             mmask
         b42       a0             hold for store5
         a7        24             64-hbsiz
         v1        v0<a7          (htable.and.bmask).ls.(64-hbsiz)
*     s5        s5-s6          movenm; for civic use s5-s6
         a3        b43            
         a7        b60            
         a0        a1+a3          loc(htable(rkey2))
*     movenm,a7 s5             save movenm for store5
*     v7        s2&v0          extract mmove
      a7        63             shift right to extract age
      v7        v0>a7          extract mmove
         v4        ,a0,a2         htable(rkey2)
         s1        s1>24          now use left 40 bits
*     s5        s5<62          movenm.ls.62
      s5        s5<62          onerep(ply).ls.62
         s1        s1<24          hashbd.ls.(64-hbsiz)
         a7        25             for mlevel shift
         v2        s1-v1          any hits?
         s6        s7-s4          rdepth-ply
         s7        64             64
         vm        v2,z           any hits?
         v3        v4>a5          htable(rkey2).rs.hshft
         s2        <32            
         a5        s4             ply
         v2        s2&v3          extract htable(rkey2)
         s2        vm             save hash hits
         s6        s6+s7          rdepth-ply+64
*     v6        s5-v7          same move?
         v5        v2>a7          mlevel+64
*     vm        v6,n           any not same move?
      vm        v7,n           any old positions?
         s7        <2             sentinel mask bits
         s0        s2             any hash hits?
         a3        zs2            hold for test
         jsn       l400m          hash hit
         a7        0              
         s2        v5,a7          best (here 64 acts like 0)
         s4        v5,a0          better?
         s0        vm             
         s3        vm             move hits
         a1        zs3            rkey1 if not move
         jsn       l1000          
l102     =         *              
         s0        s2-s4          
         a7        2              next try
         s3        v5,a7          better
         a1        0              bestp
         jsm       skip1          if still best
         s2        s4             new best
         a1        1              new bestp
skip1    =         *
         s0        s2-s3          
         a7        3              
         s4        v5,a7          
         jsm       skip2          
         s2        s3             
         a1        2              
skip2    =         *
         s0        s2-s4          
         a7        4              delete
         s3        v5,a7          this
         jsm       skip3          set
         s2        s4             of
         a1        3              ten
skip3    =         *
         s0        s2-s3          commands
         a7        5              to go
         s4        v5,a7          back
         jsm       skip4          to three
         s2        s3             retries
         a1        4              delete line to go back to five retries
skip4    =         *
         s0        s2-s4          delete line to go back to five retries
         a7        6              delete
         s3        v5,a7          this
         jsm       skip5          set
         s2        s4             of
         a1        5              ten
skip5    =         *
         s0        s2-s3          commands
         a7        7              to go
         s4        v5,a7          back
         jsm       skip6          to five
         s2        s3             retries
         a1        2+2+2          delete +2 to go back by two retries
skip6    =         *
         s0        s2-s4          
         jsm       skip7          
         s2        s4             
         a1        3+2+2          delete +2 to go back by two retries
skip7    =         *
         s0        s6-s2          
         s3        v5,a1          mlevel+64
         s4        v3,a1          htable(rkey2).rs.hshft
         jsp       l400           
         j         b0             no store
*    a1=rkey1,s6=rdepth-ply+64,s5=0,a2=rindex,s1=hashbd<24,a3=rkey(hash)
l400m    =         *              
         s2        vm             move hits?
         a1        zs2            rkey1 if not move leftmost
         a0        a1-a3          
         s3        v5,a3          mlevel+64
         s4        v3,a3          htable(rkey2).rs.hshft
         jam       l1000          
         a1        a3             
l400     =         *              
         s0        s6-s3          replace?
         s2        1              
         s7        s7<23          
         s2        s2<23          
         jsp       l1000          
         s7        s7&s4          type
         s0        s2\s7          
         jsn       l1000          
         j         b0             no store
*   a1=rkey1,a5=ply,s6=rdepth-ply+64,s5=0,s1=hash<24
l1000    =         *              
         a6        b60            
         a4        taskid,a6      
         a7        a5+a6          ply+base
         a2        a2*a1          rindex of hit
         s4        value-2,a7     value(ply-1)
         a0        b44            side
         a3        b42            loc htable(rkey1)
         s1        s1>24          realign hashbd
         s5        s1+s5          movenm<62+hashbd
         a2        a2+a3          htable(rkey1)+rindex of hit
         a3        50             
         a4        a3*a4          loc(0,taskid)
         s3        2000000        
         jap       l720p          if side=+1
*    s4=value(ply-1),a2=rkey for store
l720m    =         *              
         a3        a5+a4          (ply,taskid)
         s1        curmvs-51,a3   curmvs(ply)
         a3        b43            htbl1
         a4        a2-a3          rkey2
         s4        s3+s4          2000000+value(ply-1)
*    this critical section needs protection
         vwd       10/o'34,6/o'20 call lockon(store)
         s3        0,a4           get current htable(rkey2)
         s6        s6<25          d-p+64
         s4        s4<40          position 2000000+value(ply-1)
         a5        b45            hshft
         s2        <2             
         s2        s2<23          3.ls.23
         s2        s6!s2          (d-p+64)<25+2<23
         s5        s4!s5          htb(1)
         s2        s2!s1          add in curmvs
         0,a2      s5             htable(rkey1)=
         s6        t46            hmask
         s2        s2<a5          position level+sentinel+(curmvs)
         s3        s3&s6          clear half of htable(rkey2)
         s4        s2!s3          merge other half
         0,a4      s4             htable(rkey2)=
         vwd       10/o'36,6/o'20 call lockoff(store)
*    end of critical section needs protection
         j         b0             
*    s4=value(ply-1),a2=rkey for store
l720p    =         *              
         a3        a5+a4          (ply,taskid)
         s1        curmvs-51,a3   curmvs(ply)
         a3        b43            htbl1
         a4        a2+a3          rkey2
         s4        s3+s4          2000000+value(ply-1)
*    this critical section needs protection
         vwd       10/o'34,6/o'20 call lockon(store)
         s3        0,a4           get current htable(rkey2)
         s6        s6<25          d-p+64
         s4        s4<40          position 2000000+value(ply-1)
         a5        b45            hshft
         s2        <2             
         s2        s2<23          3.ls.23
         s2        s6!s2          (d-p+64)<25+2<23
         s5        s4!s5          htb(1)
         s2        s2!s1          add in curmvs
         0,a2      s5             htable(rkey1)=
         s6        t46            hmask
         s2        s2<a5          position level+sentinel+(curmvs)
         s3        s3&s6          clear half of htable(rkey2)
         s4        s2!s3          merge other half
         0,a4      s4             htable(rkey2)=
         vwd       10/o'36,6/o'20 call lockoff(store)
*    end of critical section needs protection
l9999    =         *
         j         b0             
         include   'global'       
         end       
