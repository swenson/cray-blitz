         include   'common'       
         ident     pawnep         
         entry     PAWNEP         
         
*     this is the 1983 move generator, using 23-bit packed move
*     it includes pawnep, piece (vectorized), and pawn.
*
*      moves are stored as 23-bit packed array
*      format is  mmm ccc ttt ddddddd sssssss
*              movgpc,cappc,type,tosq,fromsq
*
*        register usage      (also uses most t, v and b regs)
*
*          a a a a a a a a    s s s s s s s s
*          0 1 2 3 4 5 6 7    0 1 2 3 4 5 6 7
*
*          t t t w t l s b    t t t t t t + m
*          m m m h m s q a    m m m m m m k o
*          p p p r p t   s    p p p p p p g v
*
         align     
PAWNEP   =         *              
         a7        b60            task common base addr
         a1        ply,a7         
         a3        player,a7      
         s1        gencap,a7      
         s4        side,a7        
         s2        o'4540000      
         t74       s2             
enpas    =         *              
         a0        a1-1           special case if ply.eq.1
         a1        a1+a7          
         a2        to-2,a1        to(ply-1)
         a4        from-2,a1      from(ply-1)
         a3        a3+a7          
         b45       a1             save ply+base
         b42       a3             save player+base
*    now just set t70=gencap
         t70       s1             gencap
         s0        s4             
         jan       plygt1         
         a2        prevmv+4,      to(0)
         a4        prevmv+3,      from(0)
plygt1   =         *              
         a5        last-1,a1      set pointer
         jsp       sidepp         if moving side is +
sidepm   =         *              moving side is -
         a3        20             
         a1        a2-a4          to-from
         a2        a2+a7          
         a6        board-1,a2     board(tolast)
         a0        a1-a3          was last move two squares forward?
         a1        board-2,a2     is pawn avail for capturing?
         a5        a5+a7          keep a5 as last+base
         jan       piece          no enpass
         a0        a6-1           is pawn?
         a4        board-0,a2     for check other file
         jan       piece          not pawn
         a0        a6+a1          is opposite pawn?
         s2        t74            mask for enpass
         a2        a2-a7          
         jan       cka2m          
doepm    =         *              
         a2        a2-1           tosq-1
         s1        a2             capturing pawn's square
         a3        9              
         s2        s2!s1          put into mask
         a3        a2-a3          
         s3        a3             
         a2        a2+1           tosq
         s3        s3<7           square on which capture effectively made
         a5        a5+1           up last(ply)
         s2        s3!s2          finish move
         moves-1,a5 s2            put move away
cka2m    =         *              
         a0        a6+a4          check other file
         s2        t74            mask enpass
         a2        a2+1           tosq+1
         jan       piece          no take avail
         s1        a2             capturing pawn's square
         a3        11             
         s2        s2!s1          put into mask
         a3        a2-a3          
         s3        a3             
         s3        s3<7           square on which capture effectively made
         a5        a5+1           up last(ply)
         s2        s3!s2          finish move
         moves-1,a5 s2            put move away
         j         piece          
sidepp   =         *              moving side is +
         a3        20             
         a1        a4-a2          from-to
         a2        a2+a7          
         a6        board-1,a2     board(tolast)
         a0        a1-a3          
         a1        board-2,a2     is pawn?
         a5        a5+a7          keep a5 as last+base
         jan       piece          no enpass
         a0        a6+1           is pawn?
         a4        board-0,a2     for check other file
         jan       piece          not pawn
         a0        a6+a1          is opposite pawn?
         s2        t74            enpass mask
         a2        a2-a7          
         jan       cka2p          
doepp    =         *              
         a2        a2-1           tosq-1
         s1        a2             capturing pawn's square
         a3        11             
         s2        s2!s1          put into mask
         a3        a2+a3          
         s3        a3             
         a2        a2+1           tosq
         s3        s3<7           square on which capture effectively made
         a5        a5+1           up last(ply)
         s2        s3!s2          finish move
         moves-1,a5 s2            put move away
cka2p    =         *              
         a0        a6+a4          check other file
         s2        t74            enpass mask
         a2        a2+1           tosq+1
         jan       piece          no take avail
         s1        a2             capturing pawn's square
         a3        9              
         s2        s2!s1          put into mask
         a3        a2+a3          
         s3        a3             
         s3        s3<7           square on which capture effectively made
         a5        a5+1           up last(ply)
         s2        s3!s2          finish move
         moves-1,a5 s2            put move away
*    s0=side, b42=player, b45=ply, a5=last(ply), t70=gencap
piece    =         *              
         a1        b42            player
         a3        pstart-1,a1    where
         a4        pcend-1,a1     pcend(player)
         a0        iota           loc.iota
         a2        8              vector length
         vl        a2             
         jsm       mside          is side minus
*      side = +
         a3        a3+a7          
         s4        vlist-1,a3     mpiece
         s0        t70            gencap?
         a4        a4+a7          
         v7        ,a0,1          iota
         b44       a4             
         a2        p.nmlocp       base adr of normal routines
         a1        p.gclocp       base adr of gencap routines
         b41       a2             
         jsz       skip1          0 implies false
         b41       a1             
skip1    =         *
*       used thruout:   a3=where, a5=last(ply), s7=move mask, a7=king test
sidep    =         *              
         s7        >63            -2
         a6        plist-1,a3     square
         s0        s4+s7          mpiece-2
         a2        s4             piece number for jump calc
         a1        parceln-parcel1 length of jump (ymp=2, c90=3)
         a2        a2*a1          (a1) parcels between jumps
         s6        6              king tester
         t72       s7             queen = -
         jsm       l2100p         pawn or gone
         a1        b41            proper jump table base adr
         s4        s4<20          mpiece<20
         s5        a6             square
         a1        a2+a1          jump table adr for this piece
         s7        s4!s5          mpiece<20+square
         s5        s5<7           sq<7
         b40       a1             
         s7        s7!s5          mpc<20+sq<7+sq
         a6        a6+a7          
         j         b40            off to proper routine
nmlocp   =         *              
         j         nmlocp         filler
         j         nmlocp         filler
parcel1  =         *
         j         nightp         
parceln  =         *
         j         bishop         
         j         rookp          
         j         queenp         
         j         kingp          
gclocp   =         *              
         j         gclocp         filler
         j         gclocp         filler
         j         nightpg        
         j         bishopg        
         j         rookpg         
         j         queenpg        
         j         kingpg         
nap1pz   =         *              
         moves,a5  s3             
         s2        -s2            
         a5        a5+1           
         jam       nap9p          
         jan       n10p           
nap9pz   =         *              
         moves,a5  s4             
         a5        a5+1           
         j         n10p           
nap10pz  =         *              
         moves,a5  s3             
         s2        -s2            
         a5        a5+1           
         jam       nap11p         
         jan       nm1p           
nap11pz  =         *              
         moves,a5  s4             
         a5        a5+1           
         j         nm1p           
*     s6=6, s7=mpc<20+sq<7+sq, a6=square
nightp   =         *              
         s0        board+7,a6     bd(sq+8)
         a0        board+11,a6    bd(sq+12)
         s1        board+7,a6     
         s2        board+11,a6    
         s3        1024           8<7
         s3        s7+s3          shiftl(sq+8,7) added in
         s4        1536           12<7
         s4        s7+s4          tosq9
         jsz       nap1pz         
         s1        -s1            -side*capc1
         jsm       nap1p          
         jaz       nap9pz         
         s2        -s2            -side*capc9
         jam       nap9p          
n10p     =         *              
         s0        board+18,a6    bd(sq+19)
         a0        board+20,a6    bd(sq+21)
         s1        board+18,a6    
         s2        board+20,a6    
         s3        2432           19<7
         s3        s7+s3          shiftl(sq+19,7) added in
         s4        2688           21<7
         s4        s7+s4          tosq11
         jsz       nap10pz        
         s1        -s1            -side*capc10
         jsm       nap10p         
         jaz       nap11pz        
         s2        -s2            -side*capc11
         jam       nap11p         
nm1p     =         *              
         s0        board-9,a6     bd(sq-8)
         a0        board-13,a6    bd(sq-12)
         s1        board-9,a6     
         s2        board-13,a6    
         s3        1024           8<7
         s3        s7-s3          shiftl(sq-8,7) added in
         s4        1536           12<7
         s4        s7-s4          tosqm12
         jsz       napm1pz        
         s1        -s1            -side*capcm1
         jsm       napm1p         
         jaz       napm9pz        
         s2        -s2            -side*capcm9
         jam       napm9p         
nm10p    =         *              
         s0        board-20,a6    bd(sq-19)
         a0        board-22,a6    bd(sq-21)
         s1        board-20,a6    
         s2        board-22,a6    
         s3        2432           19<7
         s3        s7-s3          shiftl(sq-19,7) added in
         s4        2688           21<7
         s4        s7-s4          tosqm21
         jsz       napm10pz       
         s1        -s1            -side*capcm10
         jsm       napm10p        
         jaz       napm11pz       
         s2        -s2            -side*capcm11
         jam       napm11p        
         j         l2100p         
*     s1=capc1,s2=capc9,s6=+6,s7=partial move,s3=tosq1,s4=tosq9
napm1pz  =         *              
         moves,a5  s3             
         s2        -s2            
         a5        a5+1           
         jam       napm9p         
         jan       nm10p          
napm9pz  =         *              
         moves,a5  s4             
         a5        a5+1           
         j         nm10p          
napm10pz =         *              
         moves,a5  s3             
         s2        -s2            
         a5        a5+1           
         jam       napm11p        
         jan       l2100p         
napm11pz =         *              
         moves,a5  s4             
         a5        a5+1           
         j         l2100p         
*     s1=capc1,s2=capc9,s6=+6,s7=partial move,s3=tosq1,s4=tosq9
nap1p    =         *              
         s0        s1\s6          king?
         s1        s1<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s1        s1!s3          add in cappc
         jsz       nxkppn         
         moves-1,a5 s1            put away move
         s2        -s2            -side*capc9
         jaz       nap9pz         if direction 9 ok
         jap       n10p           if 9 bad
nap9p    =         *              
         s0        s2\s6          king?
         s2        s2<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s2        s2!s4          add in cappc
         jsz       nxkppn         
         moves-1,a5 s2            put away move
         j         n10p           to direction 10
nap10p   =         *              
         s0        s1\s6          king?
         s1        s1<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s1        s1!s3          add in cappc
         jsz       nxkppn         
         moves-1,a5 s1            put away move
         s2        -s2            -side*capc11
         jaz       nap11pz        if direction 11 ok
         jap       nm1p           if 11 bad
nap11p   =         *              
         s0        s2\s6          king?
         s2        s2<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s2        s2!s4          add in cappc
         jsz       nxkppn         
         moves-1,a5 s2            put away move
         j         nm1p           to direction m1
nxkppn   =         *              
         s1        1              
         return,a7 s1             error exit
         j         b0             
napm1p   =         *              
         s0        s1\s6          king?
         s1        s1<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s1        s1!s3          add in cappc
         jsz       nxkppn         
         moves-1,a5 s1            put away move
         s2        -s2            -side*capcm9
         jaz       napm9pz        if direction m9 ok
         jap       nm10p          if m9 bad
napm9p   =         *              
         s0        s2\s6          king?
         s2        s2<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s2        s2!s4          add in cappc
         jsz       nxkppn         
         moves-1,a5 s2            put away move
         j         nm10p          to direction m10
napm10p  =         *              
         s0        s1\s6          king?
         s1        s1<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s1        s1!s3          add in cappc
         jsz       nxkppn         
         moves-1,a5 s1            put away move
         s2        -s2            -side*capcm11
         jaz       napm11pz       if direction m11 ok
         jap       l2100p         if m11 bad
napm11p  =         *              
         s0        s2\s6          king?
         s2        s2<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s2        s2!s4          add in cappc
         jsz       nxkppn         
         moves-1,a5 s2            put away move
         j         l2100p         
bxkpn    =         *              
         s1        1              
         return,a7 s1             error exit
         j         b0             
         ca,a0     a0             spacer
bishop   =         *              
         a1        board-1        loc.board(0)
         a1        a1+a6          loc.board(sq)
ifqp     =         *              
         a2        11             first ray incr
         a0        a1+a2          loc.board(sq+10)
         s1        +a2            incr to s, sign extended
         v0        ,a0,a2         load right ray
         s1        s1<24          position incr for *
         v1        s1*fv7         incr*iota<7
         a2        9              2nd ray direction
         vm        v0,n           set 1's where ray occupied
         v2        s7+v1          move
         b43       a3             save where
         a3        moves          adr of moves(0)
*   next 25 lines comprise the basic inner loop
         s5        vm             find 1st occupied square
         a6        zs5            first non-empty square
         s1        +a2            
         a0        a1+a2          
         s2        v0,a6          cappc
         s1        s1<24          
         v3        ,a0,a2         
         v4        s1*fv7         
         s0        v0,a6          check sign of cappc
         s6        v2,a6          pull out potential capture move
         a4        s2             cappc to a-reg
         a2        6              
         a0        a4+a2          was king captured
         s3        -s2            -cappc
         a2        -11            3rd ray direction
         vm        v3,n           
         v5        s7+v4          2nd direction move
         jaz       bxkpn          king capt xit for bishop + normal
         s3        s3<17          cappc<17
         a0        a5+a3          current store adr
         s3        s6!s3          add cappc to move
         v2,a6     s3             replace capt move
         ,a0,1     v2             store out 7 moves
         a5        a5+a6          up pointer to last non-capt move
         jsp       skip2          
         a5        a5+1           up pointer one more if capt move ok
skip2    =         *
*   end of basic inner loop and direction 1
         s5        vm             
         a6        zs5            
         a0        a1+a2          
         s2        v3,a6          
         v0        ,a0,a2         
         s0        v3,a6          
         s6        v5,a6          
         a4        s2             
         a2        6              
         a0        a4+a2          
         s3        -s2            
         a2        -9             4th ray direction
         vm        v0,n           
         v2        s7-v1          since direction = - 1st
         jaz       bxkpn          
         s3        s3<17          
         a0        a5+a3          
         s3        s6!s3          
         v5,a6     s3             
         ,a0,1     v5             
         a5        a5+a6          
         jsp       skip3          
         a5        a5+1           
skip3    =         *
*   end of 2nd direction
         s5        vm             find 1st occupied square
         a6        zs5            first non-empty square
         a0        a1+a2          
         s2        v0,a6          cappc
         v3        ,a0,a2         
         s0        v0,a6          check sign of cappc
         s6        v2,a6          pull out potential capture move
         a4        s2             cappc to a-reg
         a2        6              
         a0        a4+a2          was king captured
         s3        -s2            -cappc
         vm        v3,n           
         v5        s7-v4          since direction = -2nd
         jaz       bxkpn          king capt xit for bishop + normal
         s3        s3<17          cappc<17
         a0        a5+a3          current store adr
         s3        s6!s3          add cappc to move
         v2,a6     s3             replace capt move
         ,a0,1     v2             store out 7 moves
         a5        a5+a6          up pointer to last non-capt move
         jsp       skip4          
         a5        a5+1           up pointer one more if capt move ok
skip4    =         *
*   end of 3rd direction
         s5        vm             
         a6        zs5            
         s2        v3,a6          
         s0        v3,a6          
         s6        v5,a6          
         a4        s2             
         a0        a4+a2          
         s3        -s2            
         jaz       bxkpn          
         s3        s3<17          
         a0        a5+a3          
         s3        s6!s3          
         v5,a6     s3             
         ,a0,1     v5             
         a3        b43            restore where
         a5        a5+a6          
         jsp       skip5          
         a5        a5+1           
skip5    =         *
*   end of 4th direction
         j         l2100p         
rxkpn    =         *              
         s1        1              
         return,a7 s1             error exit
         j         b0             
*   v7=iota<31,a6=square,s7=mpc<20+sq<7+sq,a5=last(ply),a3=where,vl=8
*    used here: a7=6,a1=board(square),a2&a4&a6&s1&s3&s5&s6=temp,a3=loc.moves
queenp   =         *              
         t72       s6             queen = +
rookp    =         *              
         a1        board-1        loc.board(0)
         a1        a1+a6          loc.board(sq)
         a2        10             first ray incr
         a0        a1+a2          loc.board(sq+10)
         s1        +a2            incr to s, sign extended
         v0        ,a0,a2         load right ray
         s1        s1<24          position incr for *
         v1        s1*fv7         incr*iota<7
         a2        -a2            2nd ray direction
         vm        v0,n           set 1's where ray occupied
         v2        s7+v1          move
         b43       a3             save where
         a3        moves          adr of moves(0)
*   next 25 lines comprise the basic inner loop
         s5        vm             find 1st occupied square
         a6        zs5            first non-empty square
         a0        a1+a2          
         s2        v0,a6          cappc
         v3        ,a0,a2         
         s0        v0,a6          check sign of cappc
         s6        v2,a6          pull out potential capture move
         a4        s2             cappc to a-reg
         a2        6              
         a0        a4+a2          was king captured
         s3        -s2            -cappc
         a2        1              3rd ray direction
         vm        v3,n           
         v5        s7-v1          - first direction
         jaz       rxkpn          king capt xit for rook
         s3        s3<17          cappc<17
         a0        a5+a3          current store adr
         s3        s6!s3          add cappc to move
         v2,a6     s3             replace capt move
         ,a0,1     v2             store out 7 moves
         a5        a5+a6          up pointer to last non-capt move
         jsp       skip6          
         a5        a5+1           up pointer one more if capt move ok
skip6    =         *
*   end of basic inner loop and direction 1
         s5        vm             
         a6        zs5            
         s1        +a2            
         a0        a1+a2          
         s2        v3,a6          
         s1        s1<24          
         v0        ,a0,a2         
         v1        s1*fv7         
         s0        v3,a6          
         s6        v5,a6          
         a4        s2             
         a2        6              
         a0        a4+a2          
         s3        -s2            
         a2        -1             4th ray direction
         vm        v0,n           
         v2        s7+v1          since direction = 1
         jaz       rxkpn          
         s3        s3<17          
         a0        a5+a3          
         s3        s6!s3          
         v5,a6     s3             
         ,a0,1     v5             
         a5        a5+a6          
         jsp       skip7          
         a5        a5+1           
skip7    =         *
*   end of 2nd direction
         s5        vm             find 1st occupied square
         a6        zs5            first non-empty square
         a0        a1+a2          
         s2        v0,a6          cappc
         v3        ,a0,a2         
         s0        v0,a6          check sign of cappc
         s6        v2,a6          pull out potential capture move
         a4        s2             cappc to a-reg
         a2        6              
         a0        a4+a2          was king captured
         s3        -s2            -cappc
         vm        v3,n           
         v5        s7-v1          since direction = -1
         jaz       rxkpn          king capt xit for rook
         s3        s3<17          cappc<17
         a0        a5+a3          current store adr
         s3        s6!s3          add cappc to move
         v2,a6     s3             replace capt move
         ,a0,1     v2             store out 7 moves
         a5        a5+a6          up pointer to last non-capt move
         jsp       skip8          
         a5        a5+1           up pointer one more if capt move ok
skip8    =         *
*   end of 3rd direction
         s5        vm             
         a6        zs5            
         s2        v3,a6          
         s0        v3,a6          
         s6        v5,a6          
         a4        s2             
         a0        a4+a2          
         s3        -s2            
         jaz       rxkpn          
         s3        s3<17          
         a0        a5+a3          
         s3        s6!s3          
         v5,a6     s3             
         ,a0,1     v5             
         a3        b43            restore where
         a5        a5+a6          
         jsp       skip9          
         a5        a5+1           
skip9    =         *
*   end of 4th direction
         s0        t72            queen?
         jsp       ifqp           if queen go to bishop
         j         l2100p         
*     s1=capc1,s2=capc9,s6=+6,s7=partial move,s3=tosq1,s4=tosq9
kap1pz   =         *              
         moves,a5  s3             
         s2        -s2            
         a5        a5+1           
         jam       kap9p          
         jan       k10p           
kap9pz   =         *              
         moves,a5  s4             
         a5        a5+1           
         j         k10p           
kap10pz  =         *              
         moves,a5  s3             
         s2        -s2            
         a5        a5+1           
         jam       kap11p         
         jan       km1p           
kap11pz  =         *              
         moves,a5  s4             
         a5        a5+1           
         j         km1p           
*     s6=6, s7=mpc<20+sq<7+sq, a6=square
kingp    =         *              
         s0        board,a6       bd(sq+1)
         a0        board+8,a6     bd(sq+9)
         s1        board,a6       
         s2        board+8,a6     
         s3        128            1<7
         s3        s7+s3          shiftl(sq+1,7) added in
         s4        1152           9<7
         s4        s7+s4          tosq9
         jsz       kap1pz         
         s1        -s1            -side*capc1
         jsm       kap1p          
         jaz       kap9pz         
         s2        -s2            -side*capc9
         jam       kap9p          
k10p     =         *              
         s0        board+9,a6     bd(sq+10)
         a0        board+10,a6    bd(sq+11)
         s1        board+9,a6     
         s2        board+10,a6    
         s3        1280           10<7
         s3        s7+s3          shiftl(sq+10,7) added in
         s4        1408           11<7
         s4        s7+s4          tosq11
         jsz       kap10pz        
         s1        -s1            -side*capc10
         jsm       kap10p         
         jaz       kap11pz        
         s2        -s2            -side*capc11
         jam       kap11p         
km1p     =         *              
         s0        board-2,a6     bd(sq-1)
         a0        board-10,a6    bd(sq-9)
         s1        board-2,a6     
         s2        board-10,a6    
         s3        128            1<7
         s3        s7-s3          shiftl(sq-1,7) added in
         s4        1152           9<7
         s4        s7-s4          tosqm9
         jsz       kapm1pz        
         s1        -s1            -side*capcm1
         jsm       kapm1p         
         jaz       kapm9pz        
         s2        -s2            -side*capcm9
         jam       kapm9p         
km10p    =         *              
         s0        board-11,a6    bd(sq-10)
         a0        board-12,a6    bd(sq-11)
         s1        board-11,a6    
         s2        board-12,a6    
         s3        1280           10<7
         s3        s7-s3          shiftl(sq-10,7) added in
         s4        1408           11<7
         s4        s7-s4          tosqm11
         jsz       kapm10pz       
         s1        -s1            -side*capcm10
         jsm       kapm10p        
         jaz       kapm11pz       
         s2        -s2            -side*capcm11
         jam       kapm11p        
         j         l2100p         
*     s1=capc1,s2=capc9,s6=+6,s7=partial move,s3=tosq1,s4=tosq9
kapm1pz  =         *              
         moves,a5  s3             
         s2        -s2            
         a5        a5+1           
         jam       kapm9p         
         jan       km10p          
kapm9pz  =         *              
         moves,a5  s4             
         a5        a5+1           
         j         km10p          
kapm10pz =         *              
         moves,a5  s3             
         s2        -s2            
         a5        a5+1           
         jam       kapm11p        
         jan       l2100p         
kapm11pz =         *              
         moves,a5  s4             
         a5        a5+1           
         j         l2100p         
*     s1=capc1,s2=capc9,s6=+6,s7=partial move,s3=tosq1,s4=tosq9
kap1p    =         *              
         s0        s1\s6          king?
         s1        s1<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s1        s1!s3          add in cappc
         jsz       kxkppn         
         moves-1,a5 s1            put away move
         s2        -s2            -side*capc9
         jaz       kap9pz         if direction 9 ok
         jap       k10p           if 9 bad
kap9p    =         *              
         s0        s2\s6          king?
         s2        s2<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s2        s2!s4          add in cappc
         jsz       kxkppn         
         moves-1,a5 s2            put away move
         j         k10p           to direction 10
kap10p   =         *              
         s0        s1\s6          king?
         s1        s1<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s1        s1!s3          add in cappc
         jsz       kxkppn         
         moves-1,a5 s1            put away move
         s2        -s2            -side*capc11
         jaz       kap11pz        if direction 11 ok
         jap       km1p           if 11 bad
kap11p   =         *              
         s0        s2\s6          king?
         s2        s2<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s2        s2!s4          add in cappc
         jsz       kxkppn         
         moves-1,a5 s2            put away move
         j         km1p           to direction m1
kxkppn   =         *              
         s1        1              
         return,a7 s1             error exit
         j         b0             
kapm1p   =         *              
         s0        s1\s6          king?
         s1        s1<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s1        s1!s3          add in cappc
         jsz       kxkppn         
         moves-1,a5 s1            put away move
         s2        -s2            -side*capcm9
         jaz       kapm9pz        if direction m9 ok
         jap       km10p          if m9 bad
kapm9p   =         *              
         s0        s2\s6          king?
         s2        s2<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s2        s2!s4          add in cappc
         jsz       kxkppn         
         moves-1,a5 s2            put away move
         j         km10p          to direction m10
kapm10p  =         *              
         s0        s1\s6          king?
         s1        s1<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s1        s1!s3          add in cappc
         jsz       kxkppn         
         moves-1,a5 s1            put away move
         s2        -s2            -side*capcm11
         jaz       kapm11pz       if direction m11 ok
         jap       l2100p         if m11 bad
kapm11p  =         *              
         s0        s2\s6          king?
         s2        s2<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s2        s2!s4          add in cappc
         jsz       kxkppn         
         moves-1,a5 s2            put away move
         j         l2100p         
*     s6=6, s7=mpc<20+sq<7+sq, a6=square
nightpg  =         *              
         s0        board+7,a6     bd(sq+1)
         a0        board+11,a6    bd(sq+9)
         s1        board+7,a6     
         s2        board+11,a6    
         s3        1024           1<7
         s3        s7+s3          shiftl(sq+1,7) added in
         s4        1536           9<7
         s4        s7+s4          tosq9
         jsz       skip10         
         s1        -s1            -side*capc1
         jsm       ngp1p          
skip10   =         *
         jaz       skip11         
         s2        -s2            -side*capc9
         jam       ngp9p          
skip11   =         *
n10pg    =         *              
         s0        board+18,a6    bd(sq+10)
         a0        board+20,a6    bd(sq+11)
         s1        board+18,a6    
         s2        board+20,a6    
         s3        2432           10<7
         s3        s7+s3          shiftl(sq+10,7) added in
         s4        2688           11<7
         s4        s7+s4          tosq11
         jsz       skip12         
         s1        -s1            -side*capc10
         jsm       ngp10p         
skip12   =         *
         jaz       skip13         
         s2        -s2            -side*capc11
         jam       ngp11p         
skip13   =         *
nm1pg    =         *              
         s0        board-9,a6     bd(sq-1)
         a0        board-13,a6    bd(sq-9)
         s1        board-9,a6     
         s2        board-13,a6    
         s3        1024           1<7
         s3        s7-s3          shiftl(sq-1,7) added in
         s4        1536           9<7
         s4        s7-s4          tosqm9
         jsz       skip14         
         s1        -s1            -side*capcm1
         jsm       ngpm1p         
skip14   =         *
         jaz       skip15         
         s2        -s2            -side*capcm9
         jam       ngpm9p         
skip15   =         *
nm10pg   =         *              
         s0        board-20,a6    bd(sq-10)
         a0        board-22,a6    bd(sq-20)
         s1        board-20,a6    
         s2        board-22,a6    
         s3        2432           10<7
         s3        s7-s3          shiftl(sq-10,7) added in
         s4        2688           11<7
         s4        s7-s4          tosqm11
         jsz       skip16         
         s1        -s1            -side*capcm10
         jsm       ngpm10p        
skip16   =         *
         jaz       skip17         
         s2        -s2            -side*capcm11
         jam       ngpm11p        
skip17   =         *
         j         l2100p         
*     s1=capc1,s2=capc9,s6=+6,s3 or s4=partial move
ngp1p    =         *              
         s0        s1\s6          king?
         s1        s1<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s1        s1!s3          add in cappc
         jsz       nxkppg         
         moves-1,a5 s1            put away move
         s2        -s2            -side*capc9
         jap       n10pg          if 9 bad
ngp9p    =         *              
         s0        s2\s6          king?
         s2        s2<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s2        s2!s4          add in cappc
         jsz       nxkppg         
         moves-1,a5 s2            put away move
         j         n10pg          to direction 10
ngp10p   =         *              
         s0        s1\s6          king?
         s1        s1<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s1        s1!s3          add in cappc
         jsz       nxkppg         
         moves-1,a5 s1            put away move
         s2        -s2            -side*capc11
         jap       nm1pg          if 11 bad
ngp11p   =         *              
         s0        s2\s6          king?
         s2        s2<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s2        s2!s4          add in cappc
         jsz       nxkppg         
         moves-1,a5 s2            put away move
         j         nm1pg          to direction m1
nxkppg   =         *              
         s1        1              
         return,a7 s1             error exit
         j         b0             
ngpm1p   =         *              
         s0        s1\s6          king?
         s1        s1<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s1        s1!s3          add in cappc
         jsz       nxkppg         
         moves-1,a5 s1            put away move
         s2        -s2            -side*capcm9
         jap       nm10pg         if m9 bad
ngpm9p   =         *              
         s0        s2\s6          king?
         s2        s2<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s2        s2!s4          add in cappc
         jsz       nxkppg         
         moves-1,a5 s2            put away move
         j         nm10pg         to direction m10
ngpm10p  =         *              
         s0        s1\s6          king?
         s1        s1<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s1        s1!s3          add in cappc
         jsz       nxkppg         
         moves-1,a5 s1            put away move
         s2        -s2            -side*capcm11
         jap       l2100p         if m11 bad
ngpm11p  =         *              
         s0        s2\s6          king?
         s2        s2<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s2        s2!s4          add in cappc
         jsz       nxkppg         
         moves-1,a5 s2            put away move
         j         l2100p         
bxkpg    =         *              
         s1        1              
         return,a7 s1             error exit
         j         b0             
bishopg  =         *              
         a1        board-1        loc.board(0)
         a1        a1+a6          loc.board(sq)
ifqpg    =         *              
         a2        11             first ray incr
         a0        a1+a2          loc.board(sq+10)
         s1        +a2            incr to s, sign extended
         v0        ,a0,a2         load right ray
         s1        s1<24          position incr for *
         v1        s1*fv7         incr*iota<7
         a2        9              2nd ray direction
         vm        v0,n           set 1's where ray occupied
         v2        s7+v1          move
         b43       a3             save where
         a3        moves          adr of moves(0)
*   next 25 lines comprise the basic inner loop
         s5        vm             find 1st occupied square
         a6        zs5            first non-empty square
         s1        +a2            
         a0        a1+a2          
         s2        v0,a6          cappc
         s1        s1<24          
         v3        ,a0,a2         
         v4        s1*fv7         
         s0        v0,a6          check sign of cappc
         s6        v2,a6          pull out potential capture move
         a4        s2             cappc to a-reg
         a2        6              
         a0        a4+a2          was king captured
         s3        -s2            -cappc
         a2        -11            3rd ray direction
         vm        v3,n           
         v5        s7+v4          2nd direction
         jaz       bxkpg          king capt xit for bishop
         s3        s3<17          cappc<17
         a0        a5+a3          current store adr
         s3        s6!s3          add cappc to move
         jsp       lab01          
         moves,a5  s3             store capture move
         a5        a5+1           up pointer one more if capt move ok
*   end of basic inner loop and direction 1
lab01    =         *              
         s5        vm             
         a6        zs5            
         a0        a1+a2          
         s2        v3,a6          
         v0        ,a0,a2         
         s0        v3,a6          
         s6        v5,a6          
         a4        s2             
         a2        6              
         a0        a4+a2          
         s3        -s2            
         a2        -9             4th ray direction
         vm        v0,n           
         v2        s7-v1          
         jaz       bxkpg          
         s3        s3<17          
         a0        a5+a3          
         s3        s6!s3          
         jsp       lab02          
         moves,a5  s3             store capture move
         a5        a5+1           
*   end of 2nd direction
lab02    =         *              
         s5        vm             find 1st occupied square
         a6        zs5            first non-empty square
         a0        a1+a2          
         s2        v0,a6          cappc
         v3        ,a0,a2         
         s0        v0,a6          check sign of cappc
         s6        v2,a6          pull out potential capture move
         a4        s2             cappc to a-reg
         a2        6              
         a0        a4+a2          was king captured
         s3        -s2            -cappc
         vm        v3,n           
         v5        s7-v4          
         jaz       bxkpg          king capt xit for bishop
         s3        s3<17          cappc<17
         a0        a5+a3          current store adr
         s3        s6!s3          add cappc to move
         jsp       lab03          
         moves,a5  s3             store capture move
         a5        a5+1           up pointer one more if capt move ok
*   end of 3rd direction
lab03    =         *              
         s5        vm             
         a6        zs5            
         s2        v3,a6          
         s0        v3,a6          
         s6        v5,a6          
         a4        s2             
         a0        a4+a2          
         s3        -s2            
         jaz       bxkpg          
         s3        s3<17          
         a0        a5+a3          
         s3        s6!s3          
         a3        b43            restore where
         jsp       lab04          
         moves,a5  s3             store capture move
         a5        a5+1           
*   end of 4th direction
lab04    =         *              
         j         l2100p         
*   v7=iota<31,a6=square,s7=mpc<20+sq<7+sq,a5=last(ply),a3=where,vl=8
*    used here: a7=6,a1=board(square),a2&a4&a6&s1&s3&s5&s6=temp,a3=loc.moves
rxkpg    =         *              
         s1        1              
         return,a7 s1             error exit
         j         b0             
queenpg  =         *              
         t72       s6             queen = +
rookpg   =         *              
         a1        board-1        loc.board(0)
         a1        a1+a6          loc.board(sq)
         a2        10             first ray incr
         a0        a1+a2          loc.board(sq+10)
         s1        +a2            incr to s, sign extended
         v0        ,a0,a2         load right ray
         s1        s1<24          position incr for *
         v1        s1*fv7         incr*iota<7
         a2        -a2            2nd ray direction
         vm        v0,n           set 1's where ray occupied
         v2        s7+v1          move
         b43       a3             save where
         a3        moves          adr of moves(0)
*   next 25 lines comprise the basic inner loop
         s5        vm             find 1st occupied square
         a6        zs5            first non-empty square
         a0        a1+a2          
         s2        v0,a6          cappc
         v3        ,a0,a2         
         s0        v0,a6          check sign of cappc
         s6        v2,a6          pull out potential capture move
         a4        s2             cappc to a-reg
         a2        6              
         a0        a4+a2          was king captured
         s3        -s2            -cappc
         a2        1              3rd ray direction
         vm        v3,n           
         v5        s7-v1          - first direction
         jaz       rxkpg          king capt xit for rook
         s3        s3<17          cappc<17
         a0        a5+a3          current store adr
         s3        s6!s3          add cappc to move
         jsp       lab05          
         moves,a5  s3             store capture move
         a5        a5+1           up pointer one more if capt move ok
*   end of basic inner loop and direction 1
lab05    =         *              
         s5        vm             
         a6        zs5            
         s1        +a2            
         a0        a1+a2          
         s2        v3,a6          
         s1        s1<24          
         v0        ,a0,a2         
         v1        s1*fv7         
         s0        v3,a6          
         s6        v5,a6          
         a4        s2             
         a2        6              
         a0        a4+a2          
         s3        -s2            
         a2        -1             4th ray direction
         vm        v0,n           
         v2        s7+v1          since direction = 1
         jaz       rxkpg          
         s3        s3<17          
         a0        a5+a3          
         s3        s6!s3          
         jsp       lab06          
         moves,a5  s3             store capture move
         a5        a5+1           
*   end of 2nd direction
lab06    =         *              
         s5        vm             find 1st occupied square
         a6        zs5            first non-empty square
         a0        a1+a2          
         s2        v0,a6          cappc
         v3        ,a0,a2         
         s0        v0,a6          check sign of cappc
         s6        v2,a6          pull out potential capture move
         a4        s2             cappc to a-reg
         a2        6              
         a0        a4+a2          was king captured
         s3        -s2            -cappc
         vm        v3,n           
         v5        s7-v1          since direction = -1
         jaz       rxkpg          king capt xit for rook
         s3        s3<17          cappc<17
         a0        a5+a3          current store adr
         s3        s6!s3          add cappc to move
         jsp       lab07          
         moves,a5  s3             store capture move
         a5        a5+1           up pointer one more if capt move ok
*   end of 3rd direction
lab07    =         *              
         s5        vm             
         a6        zs5            
         s2        v3,a6          
         s0        v3,a6          
         s6        v5,a6          
         a4        s2             
         a0        a4+a2          
         s3        -s2            
         jaz       rxkpg          
         s3        s3<17          
         s3        s6!s3          
         a3        b43            restore where
         jsp       lab08          
         moves,a5  s3             store capture move
         a5        a5+1           
*   end of 4th direction
lab08    =         *              
         s0        t72            queen?
         jsp       ifqpg          if queen go to bishop
         j         l2100p         
         bss       3              startbuf
         ca,a0     a0             
         ca,a0     a0             
*     s6=6, s7=mpc<20+sq<7+sq, a6=square
kingpg   =         *              
         s0        board,a6       bd(sq+1)
         a0        board+8,a6     bd(sq+9)
         s1        board,a6       
         s2        board+8,a6     
         s3        128            1<7
         s3        s7+s3          shiftl(sq+1,7) added in
         s4        1152           9<7
         s4        s7+s4          tosq9
         jsz       skip18         
         s1        -s1            -side*capc1
         jsm       kgp1p          
skip18   =         *
         jaz       skip19         
         s2        -s2            -side*capc9
         jam       kgp9p          
skip19   =         *
k10pg    =         *              
         s0        board+9,a6     bd(sq+10)
         a0        board+10,a6    bd(sq+11)
         s1        board+9,a6     
         s2        board+10,a6    
         s3        1280           10<7
         s3        s7+s3          shiftl(sq+10,7) added in
         s4        1408           11<7
         s4        s7+s4          tosq11
         jsz       skip20         
         s1        -s1            -side*capc10
         jsm       kgp10p         
skip20   =         *
         jaz       skip21         
         s2        -s2            -side*capc11
         jam       kgp11p         
skip21   =         *
km1pg    =         *              
         s0        board-2,a6     bd(sq-1)
         a0        board-10,a6    bd(sq-9)
         s1        board-2,a6     
         s2        board-10,a6    
         s3        128            1<7
         s3        s7-s3          shiftl(sq-1,7) added in
         s4        1152           9<7
         s4        s7-s4          tosqm9
         jsz       skip22         
         s1        -s1            -side*capcm1
         jsm       kgpm1p         
skip22   =         *
         jaz       skip23         
         s2        -s2            -side*capcm9
         jam       kgpm9p         
skip23   =         *
km10pg   =         *              
         s0        board-11,a6    bd(sq-10)
         a0        board-12,a6    bd(sq-11)
         s1        board-11,a6    
         s2        board-12,a6    
         s3        1280           10<7
         s3        s7-s3          shiftl(sq-10,7) added in
         s4        1408           11<7
         s4        s7-s4          tosqm11
         jsz       skip24         
         s1        -s1            -side*capcm10
         jsm       kgpm10p        
skip24   =         *
         jaz       skip25         
         s2        -s2            -side*capcm11
         jam       kgpm11p        
skip25   =         *
         j         l2100p         
*     s1=capc1,s2=capc9,s6=+6,s3 or s4=partial move
kgp1p    =         *              
         s0        s1\s6          king?
         s1        s1<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s1        s1!s3          add in cappc
         jsz       kxkppg         
         moves-1,a5 s1            put away move
         s2        -s2            -side*capc9
         jap       k10pg          if 9 bad
kgp9p    =         *              
         s0        s2\s6          king?
         s2        s2<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s2        s2!s4          add in cappc
         jsz       kxkppg         
         moves-1,a5 s2            put away move
         j         k10pg          to direction 10
kgp10p   =         *              
         s0        s1\s6          king?
         s1        s1<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s1        s1!s3          add in cappc
         jsz       kxkppg         
         moves-1,a5 s1            put away move
         s2        -s2            -side*capc11
         jap       km1pg          if 11 bad
kgp11p   =         *              
         s0        s2\s6          king?
         s2        s2<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s2        s2!s4          add in cappc
         jsz       kxkppg         
         moves-1,a5 s2            put away move
         j         km1pg          to direction m1
kxkppg   =         *              
         s1        1              
         return,a7 s1             error exit
         j         b0             
kgpm1p   =         *              
         s0        s1\s6          king?
         s1        s1<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s1        s1!s3          add in cappc
         jsz       kxkppg         
         moves-1,a5 s1            put away move
         s2        -s2            -side*capcm9
         jap       km10pg         if m9 bad
kgpm9p   =         *              
         s0        s2\s6          king?
         s2        s2<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s2        s2!s4          add in cappc
         jsz       kxkppg         
         moves-1,a5 s2            put away move
         j         km10pg         to direction m10
kgpm10p  =         *              
         s0        s1\s6          king?
         s1        s1<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s1        s1!s3          add in cappc
         jsz       kxkppg         
         moves-1,a5 s1            put away move
         s2        -s2            -side*capcm11
         jap       l2100p         if m11 bad
kgpm11p  =         *              
         s0        s2\s6          king?
         s2        s2<17          cappc.ls.17
         a5        a5+1           up last(ply)
         s2        s2!s4          add in cappc
         jsz       kxkppg         
         moves-1,a5 s2            put away move
         j         l2100p         
         bss       1              startbuf
l2100p   =         *              
         a4        b44            pcend
         a0        a3-a4          
         s4        vlist,a3       next piece
         a3        a3+1           next where
         jam       sidep          go for next piece
         s0        nppwns,a7      any + pawns?
         a3        pwnstrt,a7     pwnstrt(1)
         a4        pend,a7        pend(1)
         a1        b45            
         s1        0              return
         a6        a5-a7          last(ply)
         return,a7 s1             
         last-1,a1 a6             return last(ply)=loc.moves(last(ply))
         jsz       skip26         if no pawns just return
         j         pawnp          
skip26   =         *
         j         b0             return to movgen
*
*      side = -
mside    =         *              
         a3        a3+a7          
         s4        vlist-1,a3     mpiece
         s0        t70            gencap?
         a4        a4+a7          
         v7        ,a0,1          iota
         b44       a4             
         a2        p.nmlocm       base adr of normal routines
         a1        p.gclocm       base adr of gencap routines
         b41       a2             
         jsz       skip27         0 implies false
         b41       a1             
skip27   =         *
*       used thruout:   a3=where, a5=last(ply), s7=move mask, a7=king test
sidem    =         *              
         a6        plist-1,a3     square
         s7        >63            -2
         s1        -s4            change mpiece to +
         s0        s7-s4          mpiece-2
         a2        s1             piece number for jump calc
         a1        parceln-parcel1 length of jump (ymp=2, c90=3)
         a2        a2*a1          (a1) parcels between jumps
         s6        6              king
         jsm       l2100m         pawn or gone
         t72       s7             queen = -
         s5        a6             square
         s1        s1<20          mpiece<20
         a1        b41            proper jump table base adr
         a1        a2+a1          jump table adr for this piece
         s7        s1!s5          mpiece<20+square
         s5        s5<7           sq<7
         b40       a1             
         s7        s7!s5          mpc<20+sq<7+sq
         a6        a6+a7          
         j         b40            off to proper routine
nmlocm   =         *              
         j         nmlocm         filler
         j         nmlocm         filler
         j         nightm         
         j         bishom         
         j         rookm          
         j         queenm         
         j         kingm          
gclocm   =         *              
         j         gclocm         filler
         j         gclocm         filler
         j         nightmg        
         j         bishomg        
         j         rookmg         
         j         queenmg        
         j         kingmg         
*      a5=last(ply), s3 or s4=move
nap1mz   =         *              
         a5        a5+1           
         moves-1,a5 s3            
         jam       n10m           
         jan       nap9m          
nap9mz   =         *              
         a5        a5+1           
         moves-1,a5 s4            
         j         n10m           
nap10mz  =         *              
         a5        a5+1           
         moves-1,a5 s3            
         jam       nm1m           
         jan       nap11m         
nap11mz  =         *              
         a5        a5+1           
         moves-1,a5 s4            
         j         nm1m           
*     s6=6, s7=ppc<20+sq<7+sq, a6=square
nightm   =         *              
         s0        board+7,a6     bd(sq+8)
         a0        board+11,a6    bd(sq+12)
         s1        board+7,a6     
         s2        board+11,a6    
         s3        1024           8<7
         s3        s7+s3          shiftl(sq+8,7) added in
         s4        1536           12<7
         s4        s7+s4          tosq9
         jsz       nap1mz         
         jsp       nap1m          
         jaz       nap9mz         
         jap       nap9m          
n10m     =         *              
         s0        board+18,a6    bd(sq+19)
         a0        board+20,a6    bd(sq+21)
         s1        board+18,a6    
         s2        board+20,a6    
         s3        2432           19<7
         s3        s7+s3          shiftl(sq+19,7) added in
         s4        2688           21<7
         s4        s7+s4          tosq11
         jsz       nap10mz        
         jsp       nap10m         
         jaz       nap11mz        
         jap       nap11m         
nm1m     =         *              
         s0        board-9,a6     bd(sq-8)
         a0        board-13,a6    bd(sq-12)
         s1        board-9,a6     
         s2        board-13,a6    
         s3        1024           8<7
         s3        s7-s3          shiftl(sq-8,7) added in
         s4        1536           12<7
         s4        s7-s4          tosqm9
         jsz       napm1mz        
         jsp       napm1m         
         jaz       napm9mz        
         jap       napm9m         
nm10m    =         *              
         s0        board-20,a6    bd(sq-19)
         a0        board-22,a6    bd(sq-21)
         s1        board-20,a6    
         s2        board-22,a6    
         s3        2432           19<7
         s3        s7-s3          shiftl(sq-19,7) added in
         s4        2688           21<7
         s4        s7-s4          tosqm11
         jsz       napm10mz       
         jsp       napm10m        
         jaz       napm11mz       
         jap       napm11m        
         j         l2100m         
*      a5=last(ply), s3 or s4=move
napm1mz  =         *              
         a5        a5+1           
         moves-1,a5 s3            
         jam       nm10m          
         jan       napm9m         
napm9mz  =         *              
         a5        a5+1           
         moves-1,a5 s4            
         j         nm10m          
napm10mz =         *              
         a5        a5+1           
         moves-1,a5 s3            
         jam       l2100m         
         jan       napm11m        
napm11mz =         *              
         a5        a5+1           
         moves-1,a5 s4            
         j         l2100m         
*     s1=capc1,s2=capc9,s6=+6,s3 or s4=partial move
nap1m    =         *              
         s0        s1-s6          king?
         s1        s1<17          cappc.ls.17
         s1        s1!s3          add in cappc
         jsz       nxkmpn         
         jsp       lab09          
         a5        a5+1           up last(ply)
         moves-1,a5 s1            put away move
lab09    =         *              
         jam       n10m           if 9 bad
         jaz       nap9mz         if direction 9 ok
nap9m    =         *              
         s0        s2-s6          king?
         s2        s2<17          cappc.ls.17
         s2        s2!s4          add in cappc
         jsz       nxkmpn         
         jsp       n10m           
         a5        a5+1           up last(ply)
         moves-1,a5 s2            put away move
         j         n10m           to direction 10
nap10m   =         *              
         s0        s1-s6          king?
         s1        s1<17          cappc.ls.17
         s1        s1!s3          add in cappc
         jsz       nxkmpn         
         jsp       lab10          
         a5        a5+1           up last(ply)
         moves-1,a5 s1            put away move
lab10    =         *              
         jam       nm1m           if 11 bad
         jaz       nap11mz        if direction 11 ok
nap11m   =         *              
         s0        s2-s6          king?
         s2        s2<17          cappc.ls.17
         s2        s2!s4          add in cappc
         jsz       nxkmpn         
         jsp       nm1m           
         a5        a5+1           up last(ply)
         moves-1,a5 s2            put away move
         j         nm1m           to direction m1
nxkmpn   =         *              
         a1        b45            
         s1        1              
         return,a7 s1             error exit
         a5        a5-1           reduce last(ply) since move is in error
         a5        a5-a7          
         last-1,a1 a5             sav actual last(ply); may not be needed
         j         b0             
*     s1=capc1,s2=capc9,s6=+6,s3 or s4=partial move
napm1m   =         *              
         s0        s1-s6          king?
         s1        s1<17          cappc.ls.17
         s1        s1!s3          add in cappc
         jsz       nxkmpn         
         jsp       lab11          
         a5        a5+1           up last(ply)
         moves-1,a5 s1            put away move
lab11    =         *              
         jam       nm10m          if m9 bad
         jaz       napm9mz        if direction m9 ok
napm9m   =         *              
         s0        s2-s6          king?
         s2        s2<17          cappc.ls.17
         s2        s2!s4          add in cappc
         jsz       nxkmpn         
         jsp       nm10m          
         a5        a5+1           up last(ply)
         moves-1,a5 s2            put away move
         j         nm10m          to direction m10
napm10m  =         *              
         s0        s1-s6          king?
         s1        s1<17          cappc.ls.17
         s1        s1!s3          add in cappc
         jsz       nxkmpn         
         jsp       lab12          
         a5        a5+1           up last(ply)
         moves-1,a5 s1            put away move
lab12    =         *              
         jam       l2100m         if m11 bad
         jaz       napm11mz       if direction m11 ok
napm11m  =         *              
         s0        s2-s6          king?
         s2        s2<17          cappc.ls.17
         s2        s2!s4          add in cappc
         jsz       nxkmpn         
         jsp       l2100m         
         a5        a5+1           up last(ply)
         moves-1,a5 s2            put away move
         j         l2100m         
bxkmn    =         *              
         a1        b45            
         s1        1              
         return,a7 s1             error exit
         a5        a5-1           reduce last(ply) since move is in error
         a5        a5-a7          
         last-1,a1 a5             sav actual last(ply); may not be needed
         j         b0             
bishom   =         *              
         a1        board-1        loc.board(0)
         a1        a1+a6          loc.board(sq)
ifqm     =         *              
         a2        11             first ray incr
         a0        a1+a2          loc.board(sq+10)
         s1        +a2            incr to s, sign extended
         v0        ,a0,a2         load right ray
         s1        s1<24          position incr for *
         v1        s1*fv7         incr*iota<7
         a2        9              2nd ray direction
         vm        v0,n           set 1's where ray occupied
         v2        s7+v1          move
         b43       a3             save where
         a3        moves          adr of moves(0)
*   next 25 lines comprise the basic inner loop
         s5        vm             find 1st occupied square
         a6        zs5            first non-empty square
         s1        +a2            
         a0        a1+a2          
         s2        v0,a6          cappc
         s1        s1<24          
         v3        ,a0,a2         
         v4        s1*fv7         
         s6        v2,a6          pull out potential capture move
         a4        s2             cappc to a-reg
         a2        6              
         a0        a4-a2          was king captured
         s3        s0+s2          +cappc
         a2        -11            3rd ray direction
         s0        s2<57          check sign of cappc or edge
         vm        v3,n           
         v5        s7+v4          
         jaz       bxkmn          king capt xit for bishop
         s3        s3<17          cappc<17
         a0        a5+a3          current store adr
         s3        s6!s3          add cappc to move
         v2,a6     s3             replace capt move
         ,a0,1     v2             store out 7 moves
         a5        a5+a6          up pointer to last non-capt move
         jsm       skip28         
         a5        a5+1           up pointer one more if capt move ok
skip28   =         *
*   end of basic inner loop
         s5        vm             
         a6        zs5            
         a0        a1+a2          
         s2        v3,a6          
         v0        ,a0,a2         
         s6        v5,a6          
         a4        s2             
         a2        6              
         a0        a4-a2          
         s3        s0+s2          
         a2        -9             4th ray direction
         s0        s2<57          check sign of cappc or edge
         vm        v0,n           
         v2        s7-v1          
         jaz       bxkmn          
         s3        s3<17          
         a0        a5+a3          
         s3        s6!s3          
         v5,a6     s3             
         ,a0,1     v5             
         a5        a5+a6          
         jsm       skip29         
         a5        a5+1           
skip29   =         *
*   end of 2nd direction
         s5        vm             find 1st occupied square
         a6        zs5            first non-empty square
         a0        a1+a2          
         s2        v0,a6          cappc
         v3        ,a0,a2         
         s6        v2,a6          pull out potential capture move
         a4        s2             cappc to a-reg
         a2        6              
         a0        a4-a2          was king captured
         s0        s2<57          check sign of cappc or edge
         s3        s0+s2          +cappc
         vm        v3,n           
         v5        s7-v4          
         jaz       bxkmn          king capt xit for bishop
         s3        s3<17          cappc<17
         a0        a5+a3          current store adr
         s3        s6!s3          add cappc to move
         v2,a6     s3             replace capt move
         ,a0,1     v2             store out 7 moves
         a5        a5+a6          up pointer to last non-capt move
         jsm       skip30         
         a5        a5+1           up pointer one more if capt move ok
skip30   =         *
*   end of 3rd direction
         s5        vm             
         a6        zs5            
         s2        v3,a6          
         s6        v5,a6          
         a4        s2             
         a0        a4-a2          
         s3        s0+s2          
         jaz       bxkmn          
         s3        s3<17          
         s0        s2<57          check sign of cappc or edge
         a0        a5+a3          
         s3        s6!s3          
         v5,a6     s3             
         ,a0,1     v5             
         a3        b43            restore where
         a5        a5+a6          
         jsm       skip31         
         a5        a5+1           
skip31   =         *
*   end of 4th direction
         j         l2100m         
*   v7=iota<31,a6=square,s7=ppc<20+sq<7+sq,a5=last(ply),a3=where,vl=8
*    used here:   a7=6,a1=board(square),a2&a4&a6&s1&s3&s5&s6=temp
rxkmn    =         *              
         a1        b45            
         s1        1              
         return,a7 s1             error exit
         a5        a5-1           reduce last(ply) since move is in error
         a5        a5-a7          
         last-1,a1 a5             sav actual last(ply); may not be needed
         j         b0             
queenm   =         *              
         t72       s6             queen = +
rookm    =         *              
         a1        board-1        loc.board(0)
         a1        a1+a6          loc.board(sq)
         a2        10             first ray incr
         a0        a1+a2          loc.board(sq+10)
         s1        +a2            incr to s, sign extended
         v0        ,a0,a2         load right ray
         s1        s1<24          position incr for *
         v1        s1*fv7         incr*iota<7
         a2        -a2            2nd ray direction
         vm        v0,n           set 1's where ray occupied
         v2        s7+v1          move
         b43       a3             save where
         a3        moves          adr of moves(0)
*   next 25 lines comprise the basic inner loop
         s5        vm             find 1st occupied square
         a6        zs5            first non-empty square
         a0        a1+a2          
         s2        v0,a6          cappc
         v3        ,a0,a2         
         s6        v2,a6          pull out potential capture move
         a4        s2             cappc to a-reg
         a2        6              
         a0        a4-a2          was king captured
         s3        s0+s2          +cappc
         a2        1              3rd ray direction
         s0        s2<57          check sign of cappc or edge
         vm        v3,n           
         v5        s7-v1          
         jaz       rxkmn          king capt xit for rook
         s3        s3<17          cappc<17
         a0        a5+a3          current store adr
         s3        s6!s3          add cappc to move
         v2,a6     s3             replace capt move
         ,a0,1     v2             store out 7 moves
         a5        a5+a6          up pointer to last non-capt move
         jsm       skip32         
         a5        a5+1           up pointer one more if capt move ok
skip32   =         *
*   end of basic inner loop
         s5        vm             
         a6        zs5            
         s1        +a2            
         a0        a1+a2          
         s2        v3,a6          
         s1        s1<24          
         v0        ,a0,a2         
         v1        s1*fv7         
         s6        v5,a6          
         a4        s2             
         a2        6              
         a0        a4-a2          
         s3        s0+s2          
         a2        -1             4th ray direction
         s0        s2<57          check sign of cappc or edge
         vm        v0,n           
         v2        s7+v1          
         jaz       rxkmn          
         s3        s3<17          
         a0        a5+a3          
         s3        s6!s3          
         v5,a6     s3             
         ,a0,1     v5             
         a5        a5+a6          
         jsm       skip33         
         a5        a5+1           
skip33   =         *
*   end of 2nd direction
         s5        vm             find 1st occupied square
         a6        zs5            first non-empty square
         a0        a1+a2          
         s2        v0,a6          cappc
         v3        ,a0,a2         
         s6        v2,a6          pull out potential capture move
         a4        s2             cappc to a-reg
         a2        6              
         a0        a4-a2          was king captured
         s0        s2<57          check sign of cappc or edge
         s3        s0+s2          +cappc
         vm        v3,n           
         v5        s7-v1          
         jaz       rxkmn          king capt xit for rook
         s3        s3<17          cappc<17
         a0        a5+a3          current store adr
         s3        s6!s3          add cappc to move
         v2,a6     s3             replace capt move
         ,a0,1     v2             store out 7 moves
         a5        a5+a6          up pointer to last non-capt move
         jsm       skip34         
         a5        a5+1           up pointer one more if capt move ok
skip34   =         *
*   end of 3rd direction
         s5        vm             
         a6        zs5            
         s2        v3,a6          
         s6        v5,a6          
         a4        s2             
         a0        a4-a2          
         s3        s0+s2          
         jaz       rxkmn          
         s3        s3<17          
         s0        s2<57          check sign of cappc or edge
         a0        a5+a3          
         s3        s6!s3          
         v5,a6     s3             
         ,a0,1     v5             
         a3        b43            restore where
         a5        a5+a6          
         jsm       skip35         
         a5        a5+1           
skip35   =         *
*   end of 4th direction
         s0        t72            queen?
         jsp       ifqm           if queen go to bishop
         j         l2100m         
*      a5=last(ply), s3 or s4=move
kap1mz   =         *              
         a5        a5+1           
         moves-1,a5 s3            
         jam       k10m           
         jan       kap9m          
kap9mz   =         *              
         a5        a5+1           
         moves-1,a5 s4            
         j         k10m           
kap10mz  =         *              
         a5        a5+1           
         moves-1,a5 s3            
         jam       km1m           
         jan       kap11m         
kap11mz  =         *              
         a5        a5+1           
         moves-1,a5 s4            
         j         km1m           
*     s6=6, s7=ppc<20+sq<7+sq, a6=square
kingm    =         *              
         s0        board,a6       bd(sq+1)
         a0        board+8,a6     bd(sq+9)
         s1        board,a6       
         s2        board+8,a6     
         s3        128            1<7
         s3        s7+s3          shiftl(sq+1,7) added in
         s4        1152           9<7
         s4        s7+s4          tosq9
         jsz       kap1mz         
         jsp       kap1m          
         jaz       kap9mz         
         jap       kap9m          
k10m     =         *              
         s0        board+9,a6     bd(sq+10)
         a0        board+10,a6    bd(sq+11)
         s1        board+9,a6     
         s2        board+10,a6    
         s3        1280           10<7
         s3        s7+s3          shiftl(sq+10,7) added in
         s4        1408           11<7
         s4        s7+s4          tosq11
         jsz       kap10mz        
         jsp       kap10m         
         jaz       kap11mz        
         jap       kap11m         
km1m     =         *              
         s0        board-2,a6     bd(sq-1)
         a0        board-10,a6    bd(sq-9)
         s1        board-2,a6     
         s2        board-10,a6    
         s3        128            1<7
         s3        s7-s3          shiftl(sq-1,7) added in
         s4        1152           9<7
         s4        s7-s4          tosqm9
         jsz       kapm1mz        
         jsp       kapm1m         
         jaz       kapm9mz        
         jap       kapm9m         
km10m    =         *              
         s0        board-11,a6    bd(sq-10)
         a0        board-12,a6    bd(sq-11)
         s1        board-11,a6    
         s2        board-12,a6    
         s3        1280           10<7
         s3        s7-s3          shiftl(sq-10,7) added in
         s4        1408           11<7
         s4        s7-s4          tosqm11
         jsz       kapm10mz       
         jsp       kapm10m        
         jaz       kapm11mz       
         jap       kapm11m        
         j         l2100m         
*      a5=last(ply), s3 or s4=move
kapm1mz  =         *              
         a5        a5+1           
         moves-1,a5 s3            
         jam       km10m          
         jan       kapm9m         
kapm9mz  =         *              
         a5        a5+1           
         moves-1,a5 s4            
         j         km10m          
kapm10mz =         *              
         a5        a5+1           
         moves-1,a5 s3            
         jam       l2100m         
         jan       kapm11m        
kapm11mz =         *              
         a5        a5+1           
         moves-1,a5 s4            
         j         l2100m         
*     s1=capc1,s2=capc9,s6=+6,s3 or s4=partial move
kap1m    =         *              
         s0        s1-s6          king?
         s1        s1<17          cappc.ls.17
         s1        s1!s3          add in cappc
         jsz       kxkmpn         
         jsp       lab13          
         a5        a5+1           up last(ply)
         moves-1,a5 s1            put away move
lab13    =         *              
         jam       k10m           if 9 bad
         jaz       kap9mz         if direction 9 ok
kap9m    =         *              
         s0        s2-s6          king?
         s2        s2<17          cappc.ls.17
         s2        s2!s4          add in cappc
         jsz       kxkmpn         
         jsp       k10m           
         a5        a5+1           up last(ply)
         moves-1,a5 s2            put away move
         j         k10m           to direction 10
kap10m   =         *              
         s0        s1-s6          king?
         s1        s1<17          cappc.ls.17
         s1        s1!s3          add in cappc
         jsz       kxkmpn         
         jsp       lab14          
         a5        a5+1           up last(ply)
         moves-1,a5 s1            put away move
lab14    =         *              
         jam       km1m           if 11 bad
         jaz       kap11mz        if direction 11 ok
kap11m   =         *              
         s0        s2-s6          king?
         s2        s2<17          cappc.ls.17
         s2        s2!s4          add in cappc
         jsz       kxkmpn         
         jsp       km1m           
         a5        a5+1           up last(ply)
         moves-1,a5 s2            put away move
         j         km1m           to direction m1
kxkmpn   =         *              
         a1        b45            
         s1        1              
         return,a7 s1             error exit
         a5        a5-1           reduce last(ply) since move is in error
         a5        a5-a7          
         last-1,a1 a5             sav actual last(ply); may not be needed
         j         b0             
*     s1=capc1,s2=capc9,s6=+6,s3 or s4=partial move
kapm1m   =         *              
         s0        s1-s6          king?
         s1        s1<17          cappc.ls.17
         s1        s1!s3          add in cappc
         jsz       kxkmpn         
         jsp       lab15          
         a5        a5+1           up last(ply)
         moves-1,a5 s1            put away move
lab15    =         *              
         jam       km10m          if m9 bad
         jaz       kapm9mz        if direction m9 ok
kapm9m   =         *              
         s0        s2-s6          king?
         s2        s2<17          cappc.ls.17
         s2        s2!s4          add in cappc
         jsz       kxkmpn         
         jsp       km10m          
         a5        a5+1           up last(ply)
         moves-1,a5 s2            put away move
         j         km10m          to direction m10
kapm10m  =         *              
         s0        s1-s6          king?
         s1        s1<17          cappc.ls.17
         s1        s1!s3          add in cappc
         jsz       kxkmpn         
         jsp       lab16          
         a5        a5+1           up last(ply)
         moves-1,a5 s1            put away move
lab16    =         *              
         jam       l2100m         if m11 bad
         jaz       kapm11mz       if direction m11 ok
kapm11m  =         *              
         s0        s2-s6          king?
         s2        s2<17          cappc.ls.17
         s2        s2!s4          add in cappc
         jsz       kxkmpn         
         jsp       l2100m         
         a5        a5+1           up last(ply)
         moves-1,a5 s2            put away move
         j         l2100m         
*     s6=6, s7=ppc<20+sq<7+sq, a6=square
nightmg  =         *              
         s0        board+7,a6     bd(sq+8)
         a0        board+11,a6    bd(sq+12)
         s1        board+7,a6     
         s2        board+11,a6    
         s3        1024           8<7
         s3        s7+s3          shiftl(sq+8,7) added in
         s4        1536           12<7
         s4        s7+s4          tosq9
         jsz       skip36         
         jsp       ngp1m          
skip36   =         *
         jaz       skip37         
         jap       ngp9m          
skip37   =         *
n10mg    =         *              
         s0        board+18,a6    bd(sq+19)
         a0        board+20,a6    bd(sq+21)
         s1        board+18,a6    
         s2        board+20,a6    
         s3        2432           19<7
         s3        s7+s3          shiftl(sq+19,7) added in
         s4        2688           21<7
         s4        s7+s4          tosq11
         jsz       skip38         
         jsp       ngp10m         
skip38   =         *
         jaz       skip39         
         jap       ngp11m         
skip39   =         *
nm1mg    =         *              
         s0        board-9,a6     bd(sq-8)
         a0        board-13,a6    bd(sq-12)
         s1        board-9,a6     
         s2        board-13,a6    
         s3        1024           8<7
         s3        s7-s3          shiftl(sq-8,7) added in
         s4        1536           12<7
         s4        s7-s4          tosqm9
         jsz       skip40         
         jsp       ngpm1m         
skip40   =         *
         jaz       skip41         
         jap       ngpm9m         
skip41   =         *
nm10mg   =         *              
         s0        board-20,a6    bd(sq-19)
         a0        board-22,a6    bd(sq-21)
         s1        board-20,a6    
         s2        board-22,a6    
         s3        2432           19<7
         s3        s7-s3          shiftl(sq-19,7) added in
         s4        2688           21<7
         s4        s7-s4          tosqm11
         jsz       skip42         
         jsp       ngpm10m        
skip42   =         *
         jaz       skip43         
         jap       ngpm11m        
skip43   =         *
         j         l2100m         
*     s1=capc1,s2=capc9,s6=+6,s3 or s4=partial move
ngp1m    =         *              
         s0        s1-s6          king?
         s1        s1<17          cappc.ls.17
         s1        s1!s3          add in cappc
         jsz       nxkmpg         
         jsp       lab17          
         a5        a5+1           up last(ply)
         moves-1,a5 s1            put away move
lab17    =         *              
         jam       n10mg          if 9 bad
         jaz       n10mg          if direction 9 ok
ngp9m    =         *              
         s0        s2-s6          king?
         s2        s2<17          cappc.ls.17
         s2        s2!s4          add in cappc
         jsz       nxkmpg         
         jsp       n10mg          
         a5        a5+1           up last(ply)
         moves-1,a5 s2            put away move
         j         n10mg          to direction 10
ngp10m   =         *              
         s0        s1-s6          king?
         s1        s1<17          cappc.ls.17
         s1        s1!s3          add in cappc
         jsz       nxkmpg         
         jsp       lab18          
         a5        a5+1           up last(ply)
         moves-1,a5 s1            put away move
lab18    =         *              
         jam       nm1mg          if 11 bad
         jaz       nm1mg          if direction 11 ok
ngp11m   =         *              
         s0        s2-s6          king?
         s2        s2<17          cappc.ls.17
         s2        s2!s4          add in cappc
         jsz       nxkmpg         
         jsp       nm1mg          
         a5        a5+1           up last(ply)
         moves-1,a5 s2            put away move
         j         nm1mg          to direction m1
nxkmpg   =         *              
         a1        b45            
         s1        1              
         return,a7 s1             error exit
         a5        a5-1           reduce last(ply) since move is in error
         a5        a5-a7          
         last-1,a1 a5             sav actual last(ply); may not be needed
         j         b0             
*     s1=capc1,s2=capc9,s6=+6,s3 or s4=partial move
ngpm1m   =         *              
         s0        s1-s6          king?
         s1        s1<17          cappc.ls.17
         s1        s1!s3          add in cappc
         jsz       nxkmpg         
         jsp       lab19          
         a5        a5+1           up last(ply)
         moves-1,a5 s1            put away move
lab19    =         *              
         jam       nm10mg         if m9 bad
         jaz       nm10mg         if direction m9 ok
ngpm9m   =         *              
         s0        s2-s6          king?
         s2        s2<17          cappc.ls.17
         s2        s2!s4          add in cappc
         jsz       nxkmpg         
         jsp       nm10mg         
         a5        a5+1           up last(ply)
         moves-1,a5 s2            put away move
         j         nm10mg         to direction m10
ngpm10m  =         *              
         s0        s1-s6          king?
         s1        s1<17          cappc.ls.17
         s1        s1!s3          add in cappc
         jsz       nxkmpg         
         jsp       lab20          
         a5        a5+1           up last(ply)
         moves-1,a5 s1            put away move
lab20    =         *              
         jam       l2100m         if m11 bad
         jaz       l2100m         if direction m11 ok
ngpm11m  =         *              
         s0        s2-s6          king?
         s2        s2<17          cappc.ls.17
         s2        s2!s4          add in cappc
         jsz       nxkmpg         
         jsp       l2100m         
         a5        a5+1           up last(ply)
         moves-1,a5 s2            put away move
         j         l2100m         
bxkmg    =         *              
         a1        b45            
         s1        1              
         return,a7 s1             error exit
         a5        a5-1           reduce last(ply) since move is in error
         a5        a5-a7          
         last-1,a1 a5             sav actual last(ply); may not be needed
         j         b0             
bishomg  =         *              
         a1        board-1        loc.board(0)
         a1        a1+a6          loc.board(sq)
ifqmg    =         *              
         a2        11             first ray incr
         a0        a1+a2          loc.board(sq+10)
         s1        +a2            incr to s, sign extended
         v0        ,a0,a2         load right ray
         s1        s1<24          position incr for *
         v1        s1*fv7         incr*iota<7
         a2        9              2nd ray direction
         vm        v0,n           set 1's where ray occupied
         v2        s7+v1          move
         b43       a3             save where
         a3        moves          adr of moves(0)
*   next 25 lines comprise the basic inner loop
         s5        vm             find 1st occupied square
         a6        zs5            first non-empty square
         s1        +a2            
         a0        a1+a2          
         s2        v0,a6          cappc
         s1        s1<24          
         v3        ,a0,a2         
         v4        s1*fv7         
         s6        v2,a6          pull out potential capture move
         a4        s2             cappc to a-reg
         a2        6              
         a0        a4-a2          was king captured
         s3        s0+s2          +cappc
         a2        -11            3rd ray direction
         s0        s2<57          check sign of cappc or edge
         vm        v3,n           
         v5        s7+v4          
         jaz       bxkmg          king capt xit for bishop
         s3        s3<17          cappc<17
         a0        a5+a3          current store adr
         s3        s6!s3          add cappc to move
         jsm       lab21          
         moves,a5  s3             store capture move
         a5        a5+1           up pointer one more if capt move ok
*   end of basic inner loop
lab21    =         *              
         s5        vm             
         a6        zs5            
         a0        a1+a2          
         s2        v3,a6          
         v0        ,a0,a2         
         s6        v5,a6          
         a4        s2             
         a2        6              
         a0        a4-a2          
         s3        s0+s2          
         a2        -9             4th ray direction
         s0        s2<57          check sign of cappc or edge
         vm        v0,n           
         v2        s7-v1          
         jaz       bxkmg          
         s3        s3<17          
         a0        a5+a3          
         s3        s6!s3          
         jsm       lab22          
         moves,a5  s3             store capture move
         a5        a5+1           
*   end of 2nd direction
lab22    =         *              
         s5        vm             find 1st occupied square
         a6        zs5            first non-empty square
         a0        a1+a2          
         s2        v0,a6          cappc
         v3        ,a0,a2         
         s6        v2,a6          pull out potential capture move
         a4        s2             cappc to a-reg
         a2        6              
         a0        a4-a2          was king captured
         s0        s2<57          check sign of cappc or edge
         s3        s0+s2          +cappc
         vm        v3,n           
         v5        s7-v4          
         jaz       bxkmg          king capt xit for bishop
         s3        s3<17          cappc<17
         a0        a5+a3          current store adr
         s3        s6!s3          add cappc to move
         jsm       lab23          
         moves,a5  s3             store capture move
         a5        a5+1           up pointer one more if capt move ok
*   end of 3rd direction
lab23    =         *              
         s5        vm             
         a6        zs5            
         s2        v3,a6          
         s6        v5,a6          
         a4        s2             
         a0        a4-a2          
         s3        s0+s2          
         jaz       bxkmg          
         s3        s3<17          
         s0        s2<57          check sign of cappc or edge
         a0        a5+a3          
         s3        s6!s3          
         a3        b43            restore where
         jsm       lab24          
         moves,a5  s3             store capture move
         a5        a5+1           
*   end of 4th direction
lab24    =         *              
         j         l2100m         
rxkmg    =         *              
         a1        b45            
         s1        1              
         return,a7 s1             error exit
         a5        a5-1           reduce last(ply) since move is in error
         a5        a5-a7          
         last-1,a1 a5             sav actual last(ply); may not be needed
         j         b0             
queenmg  =         *              
         t72       s6             queen = +
rookmg   =         *              
         a1        board-1        loc.board(0)
         a1        a1+a6          loc.board(sq)
         a2        10             first ray incr
         a0        a1+a2          loc.board(sq+10)
         s1        +a2            incr to s, sign extended
         v0        ,a0,a2         load right ray
         s1        s1<24          position incr for *
         v1        s1*fv7         incr*iota<7
         a2        -a2            2nd ray direction
         vm        v0,n           set 1's where ray occupied
         v2        s7+v1          move
         b43       a3             save where
         a3        moves          adr of moves(0)
*   next 25 lines comprise the basic inner loop
         s5        vm             find 1st occupied square
         a6        zs5            first non-empty square
         a0        a1+a2          
         s2        v0,a6          cappc
         v3        ,a0,a2         
         s6        v2,a6          pull out potential capture move
         a4        s2             cappc to a-reg
         a2        6              
         a0        a4-a2          was king captured
         s3        s0+s2          +cappc
         a2        1              3rd ray direction
         s0        s2<57          check sign of cappc or edge
         vm        v3,n           
         v5        s7-v1          
         jaz       rxkmg          king capt xit for rook
         s3        s3<17          cappc<17
         a0        a5+a3          current store adr
         s3        s6!s3          add cappc to move
         jsm       lab25          
         moves,a5  s3             store capture move
         a5        a5+1           up pointer one more if capt move ok
*   end of basic inner loop
lab25    =         *              
         s5        vm             
         a6        zs5            
         s1        +a2            
         a0        a1+a2          
         s2        v3,a6          
         s1        s1<24          
         v0        ,a0,a2         
         v1        s1*fv7         
         s6        v5,a6          
         a4        s2             
         a2        6              
         a0        a4-a2          
         s3        s0+s2          
         a2        -1             4th ray direction
         s0        s2<57          check sign of cappc or edge
         vm        v0,n           
         v2        s7+v1          
         jaz       rxkmg          
         s3        s3<17          
         a0        a5+a3          
         s3        s6!s3          
         jsm       lab26          
         moves,a5  s3             store capture move
         a5        a5+1           
*   end of 2nd direction
lab26    =         *              
         s5        vm             find 1st occupied square
         a6        zs5            first non-empty square
         a0        a1+a2          
         s2        v0,a6          cappc
         v3        ,a0,a2         
         s6        v2,a6          pull out potential capture move
         a4        s2             cappc to a-reg
         a2        6              
         a0        a4-a2          was king captured
         s0        s2<57          check sign of cappc or edge
         s3        s0+s2          +cappc
         vm        v3,n           
         v5        s7-v1          
         jaz       rxkmg          king capt xit for rook
         s3        s3<17          cappc<17
         a0        a5+a3          current store adr
         s3        s6!s3          add cappc to move
         jsm       lab27          
         moves,a5  s3             store capture move
         a5        a5+1           up pointer one more if capt move ok
*   end of 3rd direction
lab27    =         *              
         s5        vm             
         a6        zs5            
         s2        v3,a6          
         s6        v5,a6          
         a4        s2             
         a0        a4-a2          
         s3        s0+s2          
         jaz       rxkmg          
         s3        s3<17          
         s0        s2<57          check sign of cappc or edge
         s3        s6!s3          
         a3        b43            restore where
         jsm       lab28          
         moves,a5  s3             store capture move
         a5        a5+1           
*   end of 4th direction
lab28    =         *              
         s0        t72            queen?
         jsp       ifqmg          if queen go to bishop
         j         l2100m         
*     s6=6, s7=ppc<20+sq<7+sq, a6=square
kingmg   =         *              
         s0        board,a6       bd(sq+1)
         a0        board+8,a6     bd(sq+9)
         s1        board,a6       
         s2        board+8,a6     
         s3        128            1<7
         s3        s7+s3          shiftl(sq+1,7) added in
         s4        1152           9<7
         s4        s7+s4          tosq9
         jsz       skip44         
         jsp       kgp1m          
skip44   =         *
         jaz       skip45         
         jap       kgp9m          
skip45   =         *
k10mg    =         *              
         s0        board+9,a6     bd(sq+10)
         a0        board+10,a6    bd(sq+11)
         s1        board+9,a6     
         s2        board+10,a6    
         s3        1280           10<7
         s3        s7+s3          shiftl(sq+10,7) added in
         s4        1408           11<7
         s4        s7+s4          tosq11
         jsz       skip46         
         jsp       kgp10m         
skip46   =         *
         jaz       skip47         
         jap       kgp11m         
skip47   =         *
km1mg    =         *              
         s0        board-2,a6     bd(sq-1)
         a0        board-10,a6    bd(sq-9)
         s1        board-2,a6     
         s2        board-10,a6    
         s3        128            1<7
         s3        s7-s3          shiftl(sq-1,7) added in
         s4        1152           9<7
         s4        s7-s4          tosqm9
         jsz       skip48         
         jsp       kgpm1m         
skip48   =         *
         jaz       skip49         
         jap       kgpm9m         
skip49   =         *
km10mg   =         *              
         s0        board-11,a6    bd(sq-10)
         a0        board-12,a6    bd(sq-11)
         s1        board-11,a6    
         s2        board-12,a6    
         s3        1280           10<7
         s3        s7-s3          shiftl(sq-10,7) added in
         s4        1408           11<7
         s4        s7-s4          tosqm11
         jsz       skip50         
         jsp       kgpm10m        
skip50   =         *
         jaz       skip51         
         jap       kgpm11m        
skip51   =         *
         j         l2100m         
*     s1=capc1,s2=capc9,s6=+6,s3 or s4=partial move
kgp1m    =         *              
         s0        s1-s6          king?
         s1        s1<17          cappc.ls.17
         s1        s1!s3          add in cappc
         jsz       kxkmpg         
         jsp       lab29          
         a5        a5+1           up last(ply)
         moves-1,a5 s1            put away move
lab29    =         *              
         jam       k10mg          if 9 bad
         jaz       k10mg          if direction 9 ok
kgp9m    =         *              
         s0        s2-s6          king?
         s2        s2<17          cappc.ls.17
         s2        s2!s4          add in cappc
         jsz       kxkmpg         
         jsp       k10mg          
         a5        a5+1           up last(ply)
         moves-1,a5 s2            put away move
         j         k10mg          to direction 10
kgp10m   =         *              
         s0        s1-s6          king?
         s1        s1<17          cappc.ls.17
         s1        s1!s3          add in cappc
         jsz       kxkmpg         
         jsp       lab30          
         a5        a5+1           up last(ply)
         moves-1,a5 s1            put away move
lab30    =         *              
         jam       km1mg          if 11 bad
         jaz       km1mg          if direction 11 ok
kgp11m   =         *              
         s0        s2-s6          king?
         s2        s2<17          cappc.ls.17
         s2        s2!s4          add in cappc
         jsz       kxkmpg         
         jsp       km1mg          
         a5        a5+1           up last(ply)
         moves-1,a5 s2            put away move
         j         km1mg          to direction m1
kxkmpg   =         *              
         a1        b45            
         s1        1              
         return,a7 s1             error exit
         a5        a5-1           reduce last(ply) since move is in error
         a5        a5-a7          
         last-1,a1 a5             sav actual last(ply); may not be needed
         j         b0             
*     s1=capc1,s2=capc9,s6=+6,s3 or s4=partial move
kgpm1m   =         *              
         s0        s1-s6          king?
         s1        s1<17          cappc.ls.17
         s1        s1!s3          add in cappc
         jsz       kxkmpg         
         jsp       lab31          
         a5        a5+1           up last(ply)
         moves-1,a5 s1            put away move
lab31    =         *              
         jam       km10mg         if m9 bad
         jaz       km10mg         if direction m9 ok
kgpm9m   =         *              
         s0        s2-s6          king?
         s2        s2<17          cappc.ls.17
         s2        s2!s4          add in cappc
         jsz       kxkmpg         
         jsp       km10mg         
         a5        a5+1           up last(ply)
         moves-1,a5 s2            put away move
         j         km10mg         to direction m10
kgpm10m  =         *              
         s0        s1-s6          king?
         s1        s1<17          cappc.ls.17
         s1        s1!s3          add in cappc
         jsz       kxkmpg         
         jsp       lab32          
         a5        a5+1           up last(ply)
         moves-1,a5 s1            put away move
lab32    =         *              
         jam       l2100m         if m11 bad
         jaz       l2100m         if direction m11 ok
kgpm11m  =         *              
         s0        s2-s6          king?
         s2        s2<17          cappc.ls.17
         s2        s2!s4          add in cappc
         jsz       kxkmpg         
         jsp       l2100m         
         a5        a5+1           up last(ply)
         moves-1,a5 s2            put away move
*         j         l2100m
l2100m   =         *              
         a4        b44            pcend
         a0        a3-a4          
         s4        vlist,a3       next piece
         a3        a3+1           next where
         jam       sidem          go for next piece
         s0        nopwns,a7      any - pawns?
         a3        pwnstrt+1,a7   where=pwnstrt(2)
         a4        pend+1,a7      pend(2)
         a1        b45            
         s1        0              return
         a6        a5-a7          
         return,a7 s1             
         last-1,a1 a6             return last(ply)=loc.moves(last(ply))
         jsz       skip52         
         j         pawnm          
skip52   =         *
         j         b0             return to movgen
*
* enter with: b0=return jump adr, t70=gencap (- = true), b45=ply
*             a5=last(ply), a3=where, a4=pend(1)
*
pawnp    =         *              
         a3        a3+a7          
         a4        a4+a7          
         a1        vlist-1,a3     vlist(where)
         a6        plist-1,a3     plist(where)
         s6        o'4002400      1<20 + 10<7
         s4        1280           10<7
         a2        1              track captures of king
         s3        128            1<7
         t73       s3             
         s5        40             push2 test
         t75       s5             
         b44       a4             save pend
pnl      =         *              
         b43       a3             save vlist(where)+base
         a3        a6+a7          
         a0        a1-1           is pawn on square?
         s1        board+8,a3     sq+9
         s2        board+10,a3    sq+11
         jan       l2101p         piece or gone
         a4        60             
         a0        a6-a4          check for beyond square 60
         s5        a6             square
         s7        s5             
         jap       pr7pn          if pawn beyond square 60 do more
         s7        s7<7           
         a0        board+9,a3     sq+10
         s0        s1!s2          any captures possible?
         s7        s7!s5          sq<7+sq
         s7        s7+s6          mpc<20+(sq+10)<7+sq=move for push 1
         s3        board+19,a3    sq+20
         s1        -s1            cappc9
         s2        -s2            cappc11
         jsm       capn9          if a capture possible
         s0        t70            gencap?
         s2        t75            40
         jan       l2101p         no pushes for this pawn
         jsn       l2101p         no push allowed (by gencap)
morpn    =         *              
         s0        s2-s5          40-sq
         moves,a5  s7             store push1
         a0        s3             ck bd+20
         a5        a5+1           up last(ply)
         s1        s7+s4          for push2
         jsm       l2101p         push2 not legal
         jan       l2101p         cant push 2
         moves,a5  s1             store push2
         a5        a5+1           up last(ply)
l2101p   =         *              
         a3        b43            
         a1        vlist,a3       next pawn?
         a4        b44            
         a0        a3-a4          where-pend
         a6        plist,a3       next square?
         a3        a3+1           up where
         jam       pnl            go do next pawn
         s0        a2             was opponents king captured?
         a2        b45            ply
         a5        a5-a7          
         last-1,a2 a5             put away last(ply)
         jsz       pxkp           abort due to capture of king
         j         b0             normal exit, return already set to 0
pxkp     =         *              
         s1        1              
         return,a7 s1             
         j         b0             
*    s1=capp9, s2=capp11,a7(one of which is -)
capn9    =         *              
         s0        -s1            is bd+9 = -
         a4        s1             for king check
         s1        s1<17          cpc<17
         a1        6              king
         s3        t73            1<7
         jsp       capn11         no cap9
         s1        s7!s1          cpc in position
         a4        a4-a1          captured opponents king?
         s1        s1-s3          sq+10-1
         a5        a5+1           
         a2        a4*a2          product with previous king captures
         moves-1,a5 s1            
capn11   =         *              
         s0        -s2            
         a4        s2             
         s2        s2<17          
         a4        a4-a1          
         s2        s2!s7          
         jsp       gckpn          no cap11
         s2        s2+s3          
         a5        a5+1           
         a2        a4*a2          
         moves-1,a5 s2            
gckpn    =         *              
         s3        board+19,a3    sq+20
         s0        t70            gencap?
         s2        t75            40
         jan       l2101p         no pushes for this pawn
         jsn       l2101p         no push allowed (by gencap)
         j         morpn          
c7pn9    =         *              
         s0        -s1            is bd+9 = -
         a4        s1             for king check
         s1        s1<17          cpc<17
         a1        6              king
         s3        t73            1<7
         jsp       c7pn11         no cap9
         s1        s7!s1          cpc in position
         a4        a4-a1          captured opponents king?
         s1        s1-s3          sq+10-1
         a5        a5+1           
         a2        a4*a2          product with previous king captures
         moves-1,a5 s1            
c7pn11   =         *              
         s0        -s2            
         a4        s2             
         s2        s2<17          
         a4        a4-a1          
         s2        s2!s7          
         jsp       mor7pn         no cap11
         s2        s2+s3          sq+10+1
         a5        a5+1           
         a2        a4*a2          
         moves-1,a5 s2            
         j         mor7pn         
pr7pn    =         *              
         a1        80             
         a0        a1-a6          
         s7        s7<7           
         jam       pr8pn          promotion possible
         a0        board+9,a3     sq+10
         s7        s7!s5          sq<7+sq
         s7        s7+s6          mpc<20+(sq+10)<7+sq=move for push 1
         s0        s1!s2          any captures possible?
         s1        -s1            cappc9
         s2        -s2            cappc11
         jsm       c7pn9          if a capture possible
mor7pn   =         *              
         jan       l2101p         no moves for this pawn
*       if we have found a legal pawn push to the 6th rank in quiescence
*       dont allow it if not passed
         s0        t70            gencap
         a1        board+18,a3    check for opponent pawn on 7th, adjacent file
         s2        board+20,a3    check for opponent pawn on 7th, adjacent file
         a4        board+19,a3    check for opponent pawn on 7th, same file
         jsz       pnl5           if not gencap do regular store of move
         s1        -1             
         a0        a1+1           check for opponent pawn on this square
         s0        s2\s1          check for opponent pawn on this square
         jaz       pnl6           if there is opponent pawn dont allow push
         a0        a4+1           check for opponent pawn on this square
         jsz       pnl6           if there is opponent pawn dont allow push
         jaz       pnl6           if there is opponent pawn dont allow push
*             having passed all tests, allow pawn push to 6th
pnl5     =         *              
         moves,a5  s7             store push1
         a5        a5+1           up last(ply)
pnl6     =         *              check if dun
         a4        b44            
         a3        b43            
         a0        a3-a4          where-pend
         a1        vlist,a3       a3 next pawn?
         a6        plist,a3       a3 next square?
         a3        a3+1           up where
         jam       pnl            go do next pawn
         s0        a2             was opponents king captured?
         a2        b45            ply
         a5        a5-a7          
         last-1,a2 a5             put away last(ply)
         jsz       pxkp           abort due to capture of king
         j         b0             normal exit, return already set to 0
c8pn9    =         *              
         s0        -s1            is bd+9 = - or edge
         a4        s1             for king check
         s1        s1<17          cpc<17
         a1        6              king
         jsp       c8pn11         no cap9
         s3        t73            1<7
         s1        s7!s1          cpc in position
         a4        a4-a1          captured opponents king?
         s1        s1-s3          sq+10-1
         a2        a4*a2          product with previous king captures
         s3        o'340000       type=queen
         s3        s1!s3          add promotion type in
         moves,a5  s3             put away queen promotion
         s3        o'200000       type=night
         s3        s3!s1          
         moves+1,a5 s3            put away night promotion
         s0        t70            gencap?
         a4        2              
         a5        a5+a4          up last ply by 2
         jsn       c8pn11         if gencap=true only list promotions to q & n
         s3        o'300000       type=rook
         s3        s1!s3          
         moves,a5  s3             put away rook promotion
         s3        o'240000       type=bishop
         s1        s1!s3          
         moves+1,a5 s1            put away bishop promotion
         a5        a5+a4          up last(ply) by 2
c8pn11   =         *              
         s0        -s2            
         a4        s2             
         s2        s2<17          
         a4        a4-a1          
         jsp       mor8pn         no cap11
         s3        t73            1<7
         s2        s2!s7          
         s2        s2+s3          sq+10+1
         a2        a4*a2          
         s3        o'340000       type=queen
         s3        s2!s3          add promotion type in
         moves,a5  s3             put away queen promotion
         s3        o'200000       type=night
         s3        s3!s2          
         moves+1,a5 s3            put away night promotion
         s0        t70            gencap?
         a4        2              
         a5        a5+a4          up last ply by 2
         jsn       mor8pn         if gencap=true only list promotions to q & n
         s3        o'300000       type=rook
         s3        s2!s3          
         moves,a5  s3             put away rook promotion
         s3        o'240000       type=bishop
         s2        s2!s3          
         moves+1,a5 s2            put away bishop promotion
         a5        a5+a4          up last(ply) by 2
         j         mor8pn         
pr8pn    =         *              
         a0        board+9,a3     sq+10
         s0        s1!s2          any captures possible?
         s7        s7!s5          sq<7+sq
         s7        s7+s6          mpc<20+(sq-10)<7+sq=move for push 1
         s1        -s1            cappc9
         s2        -s2            cappc11
         jsm       c8pn9          if a capture possible
mor8pn   =         *              
         jan       l2101p         no moves for this pawn
         s3        o'340000       type=queen
         s3        s7!s3          add promotion type in
         moves,a5  s3             put away queen promotion
         s3        o'200000       type=night
         s3        s3!s7          
         moves+1,a5 s3            put away night promotion
         s0        t70            gencap?
         a4        2              
         a5        a5+a4          up last ply by 2
         jsn       l2101p         if gencap=true only list promotions to q & n
         s3        o'300000       type=rook
         s3        s7!s3          
         moves,a5  s3             put away rook promotion
         s3        o'240000       type=bishop
         s7        s7!s3          
         moves+1,a5 s7            put away bishop promotion
         a5        a5+a4          up last(ply) by 2
*  check if dun
         a3        b43            
         a4        b44            restore pend
         a0        a3-a4          where-pend
         a1        vlist,a3       a3 next pawn?
         a6        plist,a3       a3 next square?
         a3        a3+1           up where
         jam       pnl            go do next pawn
         s0        a2             was opponents king captured?
         a2        b45            ply
         a5        a5-a7          
         last-1,a2 a5             put away last(ply)
         jsz       pxkp           abort due to capture of king
         j         b0             normal exit, return already set to 0
pawnm    =         *              
         a3        a3+a7          
         a4        a4+a7          
         a1        vlist-1,a3     vlist(where)
         a6        plist-1,a3     plist(where)
         s6        o'3775400      1<20 - 10<7
         s4        1280           10<7
         a2        1              track captures of king
         s3        128            1<7
         t73       s3             
         s5        80             push2 test
         t75       s5             
         b44       a4             
mnl      =         *              
         b43       a3             
         a3        a6+a7          
         a0        a1+1           is pawn on square?
         s1        board-10,a3    sq-9
         s2        board-12,a3    sq-11
         jan       l2101m         piece or gone
         a4        60             for gencap7 test
         a0        a6-a4          check for beyond square 50
         s5        a6             square
         s7        s5             
         jam       pr7mn          if pawn beyond square 50 do more
         a0        board-11,a3    sq-10
         s7        s7<7           
         s0        s1!s2          any captures possible?
         s7        s7!s5          sq<7+sq
         s7        s7+s6          mpc<20+(sq-10)<7+sq=move for push 1
         s3        board-21,a3    sq-20
         jsn       camn9          if a capture possible
         s0        t70            gencap?
         s2        t75            80
         jan       l2101m         no pushes for this pawn
         jsn       l2101m         no push allowed (by gencap)
mormn    =         *              
         s0        s2-s5          80-sq
         moves,a5  s7             store push1
         a0        s3             ck bd-20
         a5        a5+1           up last(ply)
         s1        s7-s4          for push2
         jsp       l2101m         push2 not legal
         jan       l2101m         cant push 2
         moves,a5  s1             store push2
         a5        a5+1           up last(ply)
l2101m   =         *              
         a3        b43            
         a1        vlist,a3       next pawn?
         a4        b44            
         a0        a3-a4          where-pend
         a6        plist,a3       next square?
         a3        a3+1           up where
         jam       mnl            go do next pawn
         s0        a2             was opponents king captured?
         a2        b45            ply
         a5        a5-a7          
         last-1,a2 a5             put away last(ply)
         jsz       pxkm           abort due to capture of king
         j         b0             normal exit, return already set to 0
pxkm     =         *              
         s1        1              
         return,a7 s1             
         j         b0             
*    s1=camm9, s2=camm11,  (one of which is -)
camn9    =         *              
         s0        s1<57          is bd+9 = - or edge
         a4        s1             for king check
         s1        s1<17          cpc<17
         a1        6              king
         s3        t73            1<7
         jsz       camn11         no cap9
         jsm       camn11         no cap9
         s1        s7!s1          cpc in position
         a4        a4-a1          captured opponents king?
         s1        s1+s3          sq-10+1
         a5        a5+1           
         a2        a4*a2          product with previous king captures
         moves-1,a5 s1            
camn11   =         *              
         s0        s2<57          
         a4        s2             
         s2        s2<17          
         a4        a4-a1          
         jsz       gckmn          no cap11
         jsm       gckmn          no cap11
         s2        s2!s7          
         s2        s2-s3          sq-10-1
         a5        a5+1           
         a2        a4*a2          
         moves-1,a5 s2            
gckmn    =         *              
         s3        board-21,a3    sq-20
         s0        t70            gencap?
         s2        t75            80
         jan       l2101m         no pushes for this pawn
         jsn       l2101m         no push allowed (by gencap)
         j         mormn          
c7mn9    =         *              
         s0        s1<57          is bd+9 = - or edge
         a4        s1             for king check
         s1        s1<17          cpc<17
         a1        6              king
         s3        t73            1<7
         jsz       c7mn11         no cap9
         jsm       c7mn11         no cap9
         s1        s7!s1          cpc in position
         a4        a4-a1          captured opponents king?
         s1        s1+s3          sq-10+1
         a5        a5+1           
         a2        a4*a2          product with previous king captures
         moves-1,a5 s1            
c7mn11   =         *              
         s0        s2<57          
         a4        s2             
         s2        s2<17          
         a4        a4-a1          
         jsz       mor7mn         no cap11
         jsm       mor7mn         no cap11
         s2        s2!s7          
         s2        s2-s3          sq-10-1
         a5        a5+1           
         a2        a4*a2          
         moves-1,a5 s2            
         j         mor7mn         
pr7mn    =         *              
         a1        40             
         a0        a1-a6          
         s7        s7<7           
         jap       pr8mn          promotion possible
         a0        board-11,a3    sq-10
         s0        s1!s2          any captures possible?
         s7        s7!s5          sq<7+sq
         s7        s7+s6          mpc<20+(sq-10)<7+sq=move for push 1
         jsn       c7mn9          if a capture possible
mor7mn   =         *              
         jan       l2101m         no moves for this pawn
*       if we have found a legal pawn push to the 6th rank in quiescence
*       dont allow it if not passed
         s0        t70            gencap
         a1        board-22,a3    check for opponent pawn on 7th, adjacent file
         s2        board-20,a3    check for opponent pawn on 7th, adjacent file
         a4        board-21,a3    check for opponent pawn on 7th, same file
         jsz       mnl5           if not gencap do regular store of move
         s1        1              
         a0        a1-1           check for opponent pawn on this square
         s0        s2\s1          check for opponent pawn on this square
         jaz       mnl6           if there is opponent pawn dont allow push
         a0        a4-1           check for opponent pawn on this square
         jsz       mnl6           if there is opponent pawn dont allow push
         jaz       mnl6           if there is opponent pawn dont allow push
*             having passed all tests, allow pawn push to 6th
mnl5     =         *              
         moves,a5  s7             store push1
         a5        a5+1           up last(ply)
mnl6     =         *              check if done   *
         a3        b43            
         a4        b44            restore pend
         a0        a3-a4          where-pend
         a1        vlist,a3       a3 next pawn?
         a6        plist,a3       a3 next square?
         a3        a3+1           up where
         jam       mnl            go do next pawn
         s0        a2             was opponents king captured?
         a2        b45            ply
         a5        a5-a7          
         last-1,a2 a5             put away last(ply)
         jsz       pxkm           abort due to capture of king
         j         b0             normal exit, return already set to 0
c8mn9    =         *              
         s0        s1<57          is bd+9 = - or edge
         a4        s1             for king check
         s1        s1<17          cpc<17
         a1        6              king
         jsz       c8mn11         no cap9
         jsm       c8mn11         no cap9
         s3        t73            1<7
         s1        s7!s1          cpc in position
         a4        a4-a1          captured opponents king?
         s1        s1+s3          sq-10+1
         a2        a4*a2          product with previous king captures
         s3        o'340000       type=queen
         s3        s1!s3          add promotion type in
         moves,a5  s3             put away queen promotion
         s3        o'200000       type=night
         s3        s3!s1          
         moves+1,a5 s3            put away night promotion
         s0        t70            gencap?
         a4        2              
         a5        a5+a4          up last ply by 2
         jsn       c8mn11         if gencap=true only list promotions to q & n
         s3        o'300000       type=rook
         s3        s1!s3          
         moves,a5  s3             put away rook promotion
         s3        o'240000       type=bishop
         s1        s1!s3          
         moves+1,a5 s1            put away bishop promotion
         a5        a5+a4          up last(ply) by 2
c8mn11   =         *              
         s0        s2<57          
         a4        s2             
         s2        s2<17          
         a4        a4-a1          
         jsz       mor8mn         no cap11
         jsm       mor8mn         no cap11
         s3        t73            1<7
         s2        s2!s7          
         s2        s2-s3          sq-10-1
         a2        a4*a2          
         s3        o'340000       type=queen
         s3        s2!s3          add promotion type in
         moves,a5  s3             put away queen promotion
         s3        o'200000       type=night
         s3        s3!s2          
         moves+1,a5 s3            put away night promotion
         s0        t70            gencap?
         a4        2              
         a5        a5+a4          up last ply by 2
         jsn       mor8mn         if gencap=true only list promotions to q & n
         s3        o'300000       type=rook
         s3        s2!s3          
         moves,a5  s3             put away rook promotion
         s3        o'240000       type=bishop
         s2        s2!s3          
         moves+1,a5 s2            put away bishop promotion
         a5        a5+a4          up last(ply) by 2
         j         mor8mn         
pr8mn    =         *              
         a0        board-11,a3    sq-10
         s0        s1!s2          any captures possible?
         s7        s7!s5          sq<7+sq
         s7        s7+s6          mpc<20+(sq-10)<7+sq=move for push 1
         jsn       c8mn9          if a capture possible
mor8mn   =         *              
         jan       l2101m         no moves for this pawn
         s3        o'340000       type=queen
         s3        s7!s3          add promotion type in
         moves,a5  s3             put away queen promotion
         s3        o'200000       type=night
         s3        s3!s7          
         moves+1,a5 s3            put away night promotion
         s0        t70            gencap?
         a4        2              
         a5        a5+a4          up last ply by 2
         jsn       l2101m         if gencap=true only list promotions to q & n
         s3        o'300000       type=rook
         s3        s7!s3          
         moves,a5  s3             put away rook promotion
         s3        o'240000       type=bishop
         s7        s7!s3          
         moves+1,a5 s7            put away bishop promotion
         a5        a5+a4          up last(ply) by 2
*  check if dun
         a3        b43            
         a4        b44            restore pend
         a0        a3-a4          where-pend
         a1        vlist,a3       a3 next pawn?
         a6        plist,a3       a3 next square?
         a3        a3+1           up where
         jam       mnl            go do next pawn
         s0        a2             was opponents king captured?
         a2        b45            ply
         a5        a5-a7          
         last-1,a2 a5             put away last(ply)
         jsz       pxkm           abort due to capture of king
         j         b0             normal exit, return already set to 0
iota     con       1s31           1<31
         con       2s31           
         con       3s31           
         con       4s31           
         con       5s31           
         con       6s31           
         con       7s31           
         con       8s31           
         include   'global'       
         end       
