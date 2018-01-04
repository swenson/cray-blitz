         include   'common'       
         ident     phase2        
         entry     PHASE2        
         ext       RIPOFF%        
         ext       MOVER         
         ext       UMOVER        
         ext       CHECKIU        
         
*           register utilization:    a7-a7 and s0-s7 are used
*                                    all t4's in use during ripoff call
*
*           b26      first+base
*           b27      pushpp
*
*           b30      count
*           b31      legals
*           b32      ply+base
*           b33      where  (+base)
*           b34      last(ply)  (+base)
*           b35      (rdepth+4)-ply  (used to exclude some captures)
*           b36      player
*           b37      return address
*              
*           t56      moves(where+1) (pre-fetched during loop)
*           t57      side
*
*           t60      mfrom
*           t61      mto
*           t62      mtype
*           t63      movgpc
*           t64      mpropc
*           t65      mcappc
*           t66      score
*           t67      inchk(ply)
*
         align     
PHASE2   =         *              
         a7        b60            task common base address
         s3        side,a7        side
         a3        player,a7
         a1        ply,a7         
         a6        rdepth,a7      
         a0        b0             
         b37       a0             save return
         a0        0              
         b30       a0             count=0
         b31       a0             legals=0
         a4        4              
         a6        a6+a4          
         a6        a6-a1          (rdepth+4) - ply
         b35       a6             (rdepth+4) - ply
         b36       a3             save player
         a1        a1+a7          ply+base
         s2        inchk-1,a1     inchk(ply)
         a2        first-1,a1     where=first(ply)
         a3        last-1,a1      last(ply)
         B32       a1             save ply+base
         a2        a2+a7          where+base
         a3        a3+a7          last+base
         s1        moves-1,a2     moves(where)
         t57       s3             side
         b34       a3             last(ply)
         b33       a2             where
         b26       a2             first(ply)+base
         t67       s2             save inchk
*        a2=where, a3=last(ply), a7=base, s1=moves(where)
l100     =         *
         a5        moves,a2       pre-fetch moves(where+1)
         a4        0
         b27       a4             pushpp=0
         s0        s1             test if move(where)=0
*        inline  extrct%
*        after extrct, t60=mfrom, t61=mto, t62=mtype
*                      t63=movgpc, t64=mpropc, t65=mcappc
         a1        B32            ply+base
         s2        <3             7
         s7        s1             temp
         jsn       hashmove       zero => hash table move
         a4        b31            legals
         a4        a4+1           legals+1
         b31       a4             put away
         s1        +a5            get for l200
         t56       s1             and tuck it away
         j         l200           next move
hashmove =         *
         s0        s1>20          extract movgpc
         s7        s7>14          position mtype
         movgpc,a7 s0
         t63       s0             save movgpc
         s7        s7&s2          extract mtype
         s6        >63            -2
         s6        s7+s6          mpropc?
         s4        <2             3
         s0        s4-s7          is mtype .gt. 3
         s3        s1             original move
         s3        s3>17          position mcappc
         mtype,a7  s7
         t62       s7             save mtype
         s3        s3&s2          extract mcappc
         s5        s1             original move
         s5        s5>7           position mto
         jsm       skip1          is mpropc ok
         s6        0              no mpropc
skip1    =         *
         s2        <7             127
         s5        s5&s2          extract mto
         s1        s1&s2          extract mfrom
         s0        t67            test inchk(ply)
         mpropc,a7 s6
         t64       s6             save mpropc
         mcappc,a7 s3
         t65       s3             save mcappc
         mto,a7    s5
         t61       s5             save mto
         mfrom,a7  s1
         t60       s1             save mfrom
         s1        +a5            get move(where+1)
         t56       s1             and save for l200
         jsp       notcheck       if inchk(ply) >= 0
         a4        b36            get player
         s1        +a4            player for checki
         r         checki         depends on t6* for extrct data
         s0        s1             return value
         a7        b60            base
         a2        b33            where
         a1        b31            legals
         a1        a1+1           legals=legals+1
         jsz       legal          move is legal
         s1        0              zero
         moves-1,a2 s1            moves(where)=0
         j         l200           and continue
legal    =         *
         b31       a1             save legals
notcheck =         *
*        a7=base
*        t60=mfrom, t61=mto, t62=mtype
*        t63=movgpc, t64=mpropc, t65=mcappc
         s2        t64            mpropc
         s0        t65            mcappc
         a0        s2             test mpropc
         s2        t61            mto
         s5        1
         s1        t62            mtype
         s3        t63            movgpc
         s4        60             test pawn push rank
         jsn       include        move is a capture, include it
         s0        s3-s5          movgpc = 1?
         jan       include        move is a promotion, include it
         jsn       l200           moving piece is not a pawn
         s0        t57            test side
         jsm       human          -1
         s0        s2-s4          mto > 60
         j         cont           do test
human    s0        s4-s2          mto < 60
cont     jsm       l200           not pushing a pawn to 5th-7th, exclude
         a1        500
         b27       a1             pushpp=500
include  =         *
         r         MOVER          make the move
         s1        t61            mto
         r         RIPOFF%        ripoff(mto)
         t66       s1             save score past call UMOVER
         r         UMOVER         unmake the move
         a7        b60            base
         s1        t66            restore score
         a1        b27            pushpp
         s2        +a1            to s2
         s7        s2-s1          score=pushpp-ripoff(mto) 
         s1        t65            mcappc
         s2        t64            mpropc
         a1        s1             mcappc
         a2        s2             mpropc
         s3        pieces-1,a1    pieces(mcappc)
         a0        a1             test mcappc
         s4        pieces-1,a2    pieces(mpropc)
         s0        +a2            test mpropc
         s5        1000           
         jaz       cappcis0       if mcappc = 0
         s7        s7+s3          score+pieces(mcappc)
cappcis0 =         *
         s4        s4-s5          pieces(mpropc)-1000
         jsz       propcis0       if mpropc = 0
         s7        s7+s4          score+pieces(mpropc)
propcis0 =         *
         s0        s7             test score
         a0        b35            ply-(rdepth+4)
         a2        b33            where
         s2        moves-1,a2     old moves(where)
         jsm       l200           if score < 0
         jsn       accept         if score > 0
         jam       l200           if ply > rdepth+4
accept   =         *
         a5        b30            count
         a6        b26            first(ply)
         a4        a5+a6          first(ply)+count
         a5        a5+1           count=count+1
         s7        moves-1,a4     temp
         b30       a5             save count
         a5        a5+a7          count+base
         val-1,a5  s0             val(count)=score
         moves-1,a4 s2            moves(first(ply)+count)=moves(where)
         moves-1,a2 s7            moves(where)=moves(first(ply)+count)
*        a2=where
l200     =         *              bottom of loop
         s1        t56            moves(where+1) [moves(where) next loop]
         a3        b34            restore last(ply)
         a0        a2-a3          where-last(ply)
         a2        a2+1           where=where+1
         b33       a2             where
         jam       l100           if where .le. last(ply)
         s0        t67            inchk(ply)
         a3        b31            legals
         a4        1
         a0        a3-a4          legals-1
         a2        0              onerep=0
         jsp       nonerep        if(inchk(ply) >= 0)
         jan       nonerep        if legals .ne. 1
         a2        1              onerep=1
nonerep  =         *
         a3        b32            ply+base
         onerep-1,a3 a2           onerep(ply)=0 or 1
*no more moves.  sort what we have.
*        a7=base
         a5        b30            count
         a0        a5-1           test count
         a1        a5+a7          count+base
         a7        b26            first(ply)
         a7        a7-1           first(ply)-1
         jam       l9998          if count=0
         jaz       l900           if count=1
*        sort moves
*        a5=count, a7=first(ply)-1
l600     =         *            outer loop
         a2        b60            where+base [=0]
         s0        -1             done=true
         a6        a7             move=first(ply)-1
l700     =         *            inner loop
         a3        val,a2         val(where)
         a4        val+1,a2       val(where+1)
         a2        a2+1           where=where+1
         a0        a2-a1          where-count
         jap       l800           where > count
         a0        a3-a4          val(+1)-val
         a6        a6+1           move=move+1
         jap       l700           val(where) > val(where+1)
         s1        moves-1,a6     moves(move)
         s2        moves,a6       moves(move+1)
         val,a2    a3             val(where)=val(where+1)
         val-1,a2  a4             val(where+1)=val(where)
         s0        0              done=false
         moves,a6  s1             moves(where+1)=moves(where)
         moves-1,a6 s2            moves(where)=moves(where+1)
         j         l700           and continue looping
l800     =         *              
         jsz       l600           if not done sorting, continue
l900     =         *              
         a2        b60            base
         a1        b32            ply+base
         a7        a7-a2          =first(ply)
         a5        a5+1           count+1
         which-1,a1 a7            which(ply)=first(ply)-1
         status-1,a1 a5           status(ply)=count+1
l9998    =         * 
         j         b37            return to caller
         align     
*        s1=player
*        t60=mfrom, t61=mto, t62=mtype
*        t63=movgpc, t64=mpropc, t65=mcappc
checki   =         *              
         a7        b60            task common base adr
         s0        s1>1           test 3-player
         s6        t60            mfrom
         s7        t61            mto
         a1        s6             mfrom
         a2        s7             mto
         tplayr,a7 s1             tplayr=playr
         jsn       playr2         
playr1   =         *              playr=1
         a6        pend+1,a7      pend(2)=vl
         s0        t62            mtype
         a4        16             
         a5        vlist+16       offset vlist(pst(2))
         a1        a1+a7          mfrom+base
         s7        kloc,a7        ktemp=kloc(1)
         tside,a7  s1             tside=1
         a3        a2+a7          mto+base
         s6        board-1,a1     board(mfrom)
         a3        pboard-1,a3    pboard(mto)
         a0        a5+a7          loc vlist(pst(2))
         a6        a6-a4          vl=pend(2)-16
         a5        plist+16       offset plist(pst(2))
         vl        a6             
         v5        ,a0,1          vlist(2)
         a0        a5+a7          loc plist(pst(2))
         jsn       l1000          if not normal move
         s4        0              
         v4        ,a0,1          plist(2)
         a3        a3-a4          correct offset of plist
         a3        a3-1           vreg offset of pboard(mto)
         s5        6              
         j         finish          
playr2   =         *              playr=2,pstart(playr)=16
         a6        pend,a7        pend(1)=vl
         s0        t62            mtype
         s5        -1             
         a4        plist          offset plist(pst(1))
         a5        vlist          offset vlist(pst(1))
         a1        a1+a7          mfrom+base
         s7        kloc+1,a7      ktemp=kloc(2)
         a3        a2+a7          mto+base
         s6        board-1,a1     board(mfrom)
         a3        pboard-1,a3    pboard(mto)
         tside,a7  s5             tside=-1
         a0        a5+a7          loc vlist(pst(1))
         vl        a6             
         v5        ,a0,1          vlist(1)
         a0        a4+a7          loc plist(pst(1))
         jsn       l1000          if not normal move
         v4        ,a0,1          plist(1)
         s4        0              
         a3        a3-1           vreg offset of pboard(mto)
         s5        -6             
finish   =         *              
         a4        100            
         a5        24             
         s0        s5\s6          is king moving?
         s5        <3             7
         v1        s5+v5          vlist+7
         s5        10000          
         s5        s5<24          position
         v2        v1<a5          position for *
         a5        s7             ktemp
         jsn       skip2          jump if not king
         a5        a2             reset ktemp
skip2    =         *
         v3        s5*fv2         (,,vlist+7)
         a5        a5*a4          (,kloc,)
         s3        bitbd,a7       
         a1        a1-a7          mfrom
         s5        bit0-1,a2      bit0(mto)
         s6        cbit0-1,a1     cbit0(mfrom)
         v4,a3     s4             plist(pboard(mto))=0
         v0        v4+v3          (plist,,vlist+7)
         a2        at-10101       loc at(0,0,0)
         a0        a5+a2          at(,kloc,)
         v6        ,a0,v0         equivalent in xmp/4 assy lang
         s3        s3!s5          or(bbd,bit0(mt))
         s3        s3&s6          and(bbd,cbit0(mf))
         v7        s3&v6          and(at,bbd)
         vm        v7,z           set bit if attack
         s1        vm             will be 0 if no attack, else non-zero
         j         b0             return
         s1        s1             spacer
         s1        s1             spacer
l1000    =         *              
         a0        b0             
         b64       a0             
         r         CHECKIU        
         j         b64            
         include   'global'       
         end       

         ident     phase3      
         entry     PHASE3        
         ext       CHECKG%        
         ext       CHECKI%        
         
*     this routine calls several other cal routines, however
*     only t47 and b77 are not available for use.
*
*     register usage
*
*       a a a a a   s s s s s   b b b b b b b b    t t t t t t t t
*       1 2 5 6 7   1 2 3 4 7   7 7 7 7 7 7 7 7    7 7 7 7 7 7 7 7
*       p l   l     m       b   0 1 2 3 4 5 6 7    0 1 2 3 4 5 6 7
*       l v   o     o       g   r p w l l v p f    c k i s g m 1 a
*       y l   c     v       m   t l h o v l l t    t i n i v t s t
*                   e       1   n y r c l   r n    m l c d c o t t
         
         align     
         
l2999    =         *              
         s1        1              
         return,a7 s1             
         j         b40            
PHASE3   =         *              
         a7        b60            task common base address
         a1        ply,a7         
         a3        player,a7      
         s3        side,a7        
         s7        <16            65535
         a6        b0             
         b40       a6             sav return adr
         a2        moves-1        .loc.moves(0)
         a1        a1+a7          
         s1        status-1,a1    status(ply)
         a6        first-1,a1     first(ply)
         a4        last-1,a1      last(ply)
         s6        givchk-1,a1    givchk(ply)
         b41       a1             ply
         b46       a3             player
         s7        s1&s7          begin-1
         s4        a6             sav first(ply)
         s0        s7>1           non-zero iff begin>2
         t46       s4             first(ply)
         a5        a4-a6          last(ply)-first(ply)
         s1        s1>16          level
         a5        a5+1           number of moves(ply)
         a6        a2+a6          .loc.moves(first(ply))
         b45       a5             vl=number of moves(ply)
         a6        a6+a7          
         t43       s3             side
         b43       a6             .loc.moves(first(ply))
         a2        s1             level
         a2        a2+a7          
         t44       s6             givchk(ply)
         b44       a2             save level
         jsn       l2400          if begin>2
l2100    =         *              a2=level,s7=begin-1
         a0        s7             begin-1
         a5        b45            vl
         a7        zs0            64
         jan       pass2          if begin=2
*pass1    =         *              *begin=kill=1,a2=level
         s1        killmv-1,a2    move=killmv(level,kill)
         a0        b43            .loc.moves(first(ply))
         vl        a5             
*l30
         v0        ,a0,1          moves(where)
         s4        1              
         t41       s4             kill
         a0        a7-a5          are there more than 64 moves in list
         s0        s1             is the killer zero?
         t45       s1             keep move for debug
         v1        s1-v0          is this killer in list?
         jsz       pass2          killer was zero so skip out
         vm        v1,z           bit on if killer found
         jam       mt641          if.gt.64 moves
         s0        vm             was killer in list
         s4        vm             
         a6        zs4            where=index(killer)
         jsn       l2500          if killer located
pass2    =         *              begin=kill=2,a2=level
         s1        killmv+49,a2   move=killmv(level,kill)
         a0        b43            .loc.moves(first(ply))
         vl        a5             
*l30
         v0        ,a0,1          moves(where)
         s4        2              
         t41       s4             kill
         a0        a7-a5          are there more than 64 moves in list
         s0        s1             is the killer zero?
         t45       s1             keep move for debug
         v1        s1-v0          is this killer in list?
         jsz       l2400          killer was zero so skip out
         vm        v1,z           bit on if killer found
         jam       mt642          if.gt.64 moves
         s0        vm             was killer in list
         s4        vm             
         a6        zs4            find location of killer
         jsn       l2500          if killer located
l2400    =         *              
         a5        b46            player
         a2        b44            level
         a1        b41            ply
         a7        b60            
         a0        a1-a2          is current level=ply?
         a3        rdepth,a7      
         a4        2              
         a2        a2-a7          initialize
         jan       skip3          if not at ply=level
         a2        a5-a4          level=player-2
skip3    =         *
         a2        a2+a4          level=level+2
         a1        a1-a7          
         s7        0              begin-1
         a0        a2-a1          level-ply
         jan       skip4          
         a2        a2+a4          level+2
skip4    =         *
         a0        a3-a2          rdepth-level
         a2        a2+a7          reset
         s1        0              for return
         b44       a2             save level
         jam       l2999          if dun
         j         l2100          if more levels to search
         bss       1              startbuf
l2500    =         *              a6=where,s1=move
*extrct%  =         *
         a7        b60            
         s2        <3             7
         s7        s1             temp
         s0        s1>20          movgpc
         s7        s7>14          position mtype
         movgpc,a7 s0             
         s7        s7&s2          mtype
         s6        >63            -2
         s6        s7+s6          mpropc?
         s4        <2             3
         s0        s4-s7          is mtype .gt. 3
         s3        s1             
         s3        s3>17          position mcappc
         mtype,a7  s7             
         s3        s3&s2          
         s5        s1             temp
         s5        s5>7           position mto
         jsm       skip5          is mpropc ok
         s6        0              no mpropc
skip5    =         *
         s2        <7             127
         s5        s5&s2          mto
         s1        s1&s2          mfrom
         mpropc,a7 s6             
         mcappc,a7 s3             
         mto,a7    s5             
         s0        t44            givchk
         mfrom,a7  s1             
         b42       a6             hold where
         jsn       l2700          
         t45       s5             save mto
l2600    =         *              a7=base
         s2        t46            recover first(ply)
         a3        s2             first(ply)
         a2        b44            level
         a2        a2-a7          
         s2        t41            kill
         s1        a2             level
         a6        b42            where
         a6        a6+a3          where for select
         s1        s1<16          
         a1        b41            ply
         s3        s1+s2          shiftl(level,16)+kill
         which-1,a1 a6            which(ply)=where
         s1        0              
         status-1,a1 s3           status(ply)=
         return,a7 s1             
         j         b40            
l2700    =         *              a7=base
         a2        b46            player
         s1        a2             
         r         CHECKG%        
l2711    =         *              
         s0        s1             ctemp
         a7        b60            
         a2        b42            where
         a3        b43            loc.moves(first(ply))
         jsn       l2600          select if true
         s3        t41            kill
         s0        s3>1           is kill=1?
         a4        a3+a2          loc moves(where)
         s4        0              
         0,a4      s4             moves(where)=0
         a5        b45            restore vl
         a7        zs0            restore 64
         a2        b44            restore level
         jsz       pass2          need to check kill=2
         j         l2400          
*
*     come here only if number of moves .gt. 64
*
mt641    =         *              
         s0        vm             was killer in list
         s4        vm             
         a6        zs4            where=index(killer)
         jsn       l2500          if killer located
         a3        a5-a7          number of moves done (normally)
         a4        b43            loc.moves(first(ply))
         a0        a4+a3          .loc.moves(where) (or+64)
         vl        a7             64
         v2        ,a0,1          moves(where)
         v3        s1-v2          
         vm        v3,z           
         s0        vm             
         s4        vm             
         a6        zs4            
         a6        a6+a3          where=index(killer)
         jsn       l2500          
         j         pass2          
mt642    =         *              
         s0        vm             was killer in list
         s4        vm             
         a6        zs4            where=index(killer)
         jsn       l2500          if killer located
         a3        a5-a7          number of moves done
         a4        b43            loc.moves(first(ply))
         a0        a4+a3          .loc.moves(where)
         vl        a7             64
         v2        ,a0,1          moves(where)
         v3        s1-v2          
         vm        v3,z           
         s0        vm             
         s4        vm             
         a6        zs4            
         a6        a6+a3          where
         jsn       l2500          
         j         l2400          
         include   'global'       
         end       
         ident     phase4        
         entry     PHASE4        
         ext       CHECKG%        
         ext       CHECKI%        
*
*       this routine selects the next move
*
         align     
*           register usage
*
*      a a a a a a a a    s s s s s s s s    b b b b b b b b    t t t t
*      0 1 2 3 4 5 6 7    0 1 2 3 4 5 6 7    7 7 7 7 7 7 7 7    7 7 7 7
*                                            0 1 2 3 4 5 6 7    0 1 2 7
*        p   w              m                r p p m m m   f    c   s a
*        l   h              o                e l l t f t   t    t   i t
*        y   i              v                t y a o r y   r    e   d t
*            c              e                n   y   o p   n    m   e c
*            h              s                    r   m e        p     k
*
PHASE4   =         *              
         a7        b60            task common base address
         a1        ply,a7         
         a2        player,a7      
         a0        b0             
         a1        a1+a7          
         b40       a0             
         b41       a1             ply
         b42       a2             player
l3200    =         *              a7=base,a1=ply
         a3        which-1,a1     
         a0        status-1,a1    
         a4        last-1,a1      
         a5        first-1,a1     
         s2        <1             
         a3        a3+a7          
         s1        moves,a3       moves(which(ply)+1)
         jaz       trip1          
         a4        a4+a7          
l3201    =         *              
         a0        a3-a4          
         a3        a3+1           which(ply)=which(ply)+1
         jap       l9998          
         s0        s1             
         s1        moves,a3       next moves(w(p))
         jsz       l3201          
         a3        a3-a7          
         which-1,a1 a3            
*  extrct       s0=packed_move
         t40       s0             sav old moves(w(p))
         s2        <3             7
         s7        t40            temp
         s0        s0>20          movgpc
         s7        s7>14          position mtype
         movgpc,a7 s0             
         s7        s7&s2          mtype
         s6        >63            -2
         s6        s7+s6          mpropc
         s4        <2             3
         s0        s4-s7          is mtype .gt. 3
         s3        t40            
         s3        s3>17          position mcappc
         mtype,a7  s7             
         s3        s3&s2          
         s5        t40            temp
         s2        <7             127
         s1        s5&s2          mfrom
         s5        s5>7           position mto
         jsm       skip6          is mpropc ok
         s6        0              no mpropc
skip6    =         *
         s0        givchk-1,a1    givchk(ply)
         a4        s7             mtype
         s5        s5&s2          mto
         mpropc,a7 s6             
         a2        s5             mto
         mcappc,a7 s3             
         mto,a7    s5             
         a3        s1             mfrom
         mfrom,a7  s1             
         jsz       l3400          
         a2        b42            player
         s1        a2             
         r         CHECKG%        uses b64
         s0        s1             ctemp
         a7        b60            
         a1        b41            ply
         jsz       l3200          if not ctemp
l3400    =         *              
         s1        0              
         return,a7 s1             set return to 0
         j         b40            
l9998    =         *              no more moves
         a3        a3-a7          
         s1        <1             
         which-1,a1 a3            
         return,a7 s1             
         j         b40            return
trip1    =         *              
         a3        a5-1           
         status-1,a1 s2           
         a4        a4+a7          
         a3        a3+a7          
         s1        moves,a3       moves(first(ply))
         j         l3201          
         include   'global'       
         end       
