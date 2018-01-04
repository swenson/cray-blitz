         include   'common'       
         ident     ripoff         
         entry     RIPOFF%        
*
*       this routine lists all pieces attacking "square".
*       it is highly vectorized, tho vl is only 7.
*
*              register usage
*
*  a a a a a a a a   s s s s s s s s   v v v v v v v v   v  b   t t t t t t t t
*  0 1 2 3 4 5 6 7   0 1 2 3 4 5 6 7   0 1 2 3 4 5 6 7   m  7   7 7 7 7 7 7 7 7
*                                                           7   0 1 2 3 4 5 6 7
*  t t t i 6 n 0 b   t - k t 4 3 6 v   r t t t t r r s   a
*  m m m n 3     d   m   p m   2   m   a m m m m a a u   l  b   k k p p t 2 9 k
*  p p p c       +   p     p           y p p p p y y m   l  1   + - - + g r r l
*
*
*       v7 carries the number of pieces found, as follows:
*   0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 2 2 2 2 2 2 . 4 . 7 7 7 7 7 7
*   0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 0 1 2 3 4 5 . 3 . 2 3 4 5 6 7
*                                               .   .
*   . . n . . . . . . . . q r b 0 0 0 0 0 b r q   .   . . . . n .
*       +                 - - -           + + +               -
*
         align     
empty    =         *              
         j         b0             
         ca,a0     a0             
RIPOFF%  =         *              
         a1        b60            task common base adr
         a5        s1             
         a5        a1+a5          base + square
         a6        0              
         a2        o'76           n-
         v7,a2     s0             clear n-
         a7        board-1        offset of board
         a4        7              vector length
         s6        6              king
         s7        board-1,a5     square occupied?
         a7        a5+a7          target square adr
         s1        0              for return
         t70       s1             kflag+
         t71       s1             kflag-
         a2        26             vl for clear
         vl        a2             64
         v7        v5-v5          clear v7
         t72       s1             initialize pawn- to 0
         vl        a4             7
         s0        s7             is square occupied
         s5        o'6314632      .1 multiplier
         jsz       empty          no work, return 0
         ca,a0     a0             added for timing delay
         a3        s7             find out piece to be taken
         s3        pval,a3        value of piece on board(square)
*        s3        pieces-1,a3      value of piece on board(square)
         s7        a5             square
         a5        -8,a7          n 0+-
         s5        s5<24          position .1
         a0        a7+1           adr qb 0+-
         s5        s7*fs5         gets rank number
*qr0
         v0        ,a0,1          
         s7        >63            2nd rank
         t73       s1             initialize pawn+ to 0
         t74       s3             save value
         a4        63             shift count
         a3        -1             
         s7        s7+s5          check for pawn take on 2nd rank
         s3        v7,a5          
         v1        v0>a4          
         s4        4              used in qr
         s1        -1             
         s3        s3-s1          
         t75       s7             used in pawn
         s2        v0,a6          
         v7,a5     s3             
         s7        -9             9th rank
         a5        8,a7           
         v2        v1+v0          
         s7        s7+s5          check for pawn take on 9th rank
         s5        32             insert constant
         t77       s1             initialize kill to -1
         v3        s6&v2          
         a0        a7+a3          
         s0        s2\s6          
*qr1
         t76       s7             used in pawn
         v6        ,a0,a3         
         jsn       skip1          
         t70       s1             
         t77       s0             
skip1    =         *
         s0        s2+s6          
         v4        s4-v3          
         s3        v7,a5          
         v1        v6>a4          
         a3        9              
         vm        v4,z           
         s3        s3-s1          
         jsn       skip2          
         t71       s1             
         t77       s0             
skip2    =         *
         v7,a5     s3             
         a5        21,a7          
         s2        v6,a6          
         v2        v1+v6          
         s3        t77            
         s7        vm             
         s0        s3&s7          
         v3        s6&v2          
         jsn       qr0            
aftqr0   =         *              
         s0        s2\s6          
         a0        a7+a3          
*qbp0
         t77       s1             
         v5        ,a0,a3         
         jsn       skip3          
         t70       s1             
         t77       s0             
skip3    =         *
         v4        s4-v3          
         s0        s2+s6          
         s3        v7,a5          
         v1        s5!v5          
         v3        qv1            
         s3        s3-s1          
         v2        v5<a4          
         s0        s2+s6          
         vm        v4,z           
         v1        s0-v3          
         a3        -a3            
         v7,a5     s3             
         a5        -21,a7         
         s2        v5,a6          
         jsn       skip4          
         t71       s1             
         t77       s0             
skip4    =         *
         a0        a7+a3          
         s0        s2\s6          
         v4        v1&v2          
         s7        vm             
*qbp1
         v0        ,a0,a3         
         s3        t77            
         jsn       skip5          
         t70       s1             
         t77       s0             
skip5    =         *
         s0        s3&s7          
         s3        v7,a5          
         v1        s5!v0          
         t77       s1             
         jsn       qr1            
aftqr1   =         *              
         v3        qv1            
         s0        s2\s1          
         s3        s3-s1          
         v2        v0<a4          
         jsz       pawn0          
aftp0    =         *              
         s0        s2+s6          
         vm        v4,m           
         v1        s0-v3          
         a3        11             
         v7,a5     s3             
         a5        19,a7          
         s2        v0,a6          
         jsn       skip6          
         t71       s1             
         t77       s0             
skip6    =         *
         a0        a7+a3          
         s0        s2\s6          
         v4        v1&v2          
         s7        vm             
*qbp2
         v6        ,a0,a3         
         s3        t77            
         jsn       skip7          
         t70       s1             
         t77       s0             
skip7    =         *
         s0        s3&s7          
         s3        v7,a5          
         v1        s5!v6          
         t77       s1             
         jsn       qb0            
aftqb0   =         *              
         v3        qv1            
         s0        s2+s1          
         s3        s3-s1          
         v2        v6<a4          
         jsz       pawn1          
aftp1    =         *              
         v1        s0-v3          
         vm        v4,m           
         s0        s2+s6          
         a3        -a3            
         v7,a5     s3             
         a5        -19,a7         
         s2        v6,a6          
         jsn       skip8          
         t71       s1             
         t77       s0             
skip8    =         *
         a0        a7+a3          
         v4        v1&v2          
         s7        vm             
         s0        s2\s6          
*qbp3
         v5        ,a0,a3         
         s3        t77            
         jsn       skip9          
         t70       s1             
         t77       s0             
skip9    =         *
         s0        s3&s7          
         s3        v7,a5          
         v1        s5!v5          
         t77       s1             
         jsn       qb1            
aftqb1   =         *              
         v3        qv1            
         s0        s2\s1          
         s3        s3-s1          
         v2        v5<a4          
         jsz       pawn2          
aftp2    =         *              
         s0        s2+s6          
         vm        v4,m           
         v1        s0-v3          
         v7,a5     s3             
         a5        12,a7          
         a3        10             
         s2        v5,a6          
         jsn       skip10         
         t71       s1             
         t77       s0             
skip10   =         *
         a0        a7+a3          
         v4        v1&v2          
         s7        vm             
         s0        s2\s6          
*qr2
         v0        ,a0,a3         
         s3        t77            
         jsn       skip11         
         t70       s1             
         t77       s0             
skip11   =         *
         s0        s3&s7          
         s3        v7,a5          
         v1        v0>a4          
         jsn       qb2            
aftqb2   =         *              
         s0        s2+s1          
         t77       s1             
         vm        v4,m           
         a3        -a3            
         s3        s3-s1          
         jsz       pawn3          
aftp3    =         *              
         s0        s2+s6          
         s2        v0,a6          
         v7,a5     s3             
         a5        -12,a7         
         jsn       skip12         
         t71       s1             
         t77       s0             
skip12   =         *
         s3        t77            
         v2        v1+v0          
         s7        vm             
         s0        s3&s7          
         t77       s1             
         v3        s6&v2          
         jsn       qb3            
aftqb3   =         *              
         a0        a7+a3          
         s0        s2\s6          
*qr3
         v6        ,a0,a3         
         s3        v7,a5          
         jsn       skip13         
         t70       s1             
         t77       s0             
skip13   =         *
         s0        s2+s6          
         v4        s4-v3          
         v1        v6>a4          
         s3        s3-s1          
         vm        v4,z           
         jsn       skip14         
         t71       s1             
         t77       s0             
skip14   =         *
         v7,a5     s3             
         s3        t77            
         s2        v6,a6          
         v1,a4     s0             for timing delay
         v2        v1+v6          
         s7        vm             
         s0        s3&s7          
         v3        s6&v2          
         jsn       qr2            
aftqr2   =         *              
         v4        s4-v3          
         s0        s2\s6          
         t77       s1             
         vm        v4,z           
         jsn       skip15         
         t70       s1             
         t77       s0             
skip15   =         *
         s0        s2+s6          
         a1        0              
         s2        1000           value of p+
         jsn       skip16         
         t71       s1             
         t77       s0             
skip16   =         *
         s3        t77            
         s7        vm             
         s0        s3&s7          
         vm        v6,n           locate empty squares on qr 3+- (for qr3)
         jsn       qr3            
aftqr3   =         *              
plstmak  =         *              
         s0        t73            number of pawns
         s4        t73            
         a2        2              knight pointer
         jsz       nopp           
         s0        s4+s1          remaining pawn+
         v1,a1     s2             pawn to plist
         a1        a1+1           i+1
         jsz       nopp           
         v1,a1     s2             pawn to plist
         a1        a1+1           
nopp     =         *              
         s0        v7,a2          count of n+
         s7        v7,a2          
         s3        3250           value of n+
         a5        o'23           bishop+ pointer
         s2        -s2            value of pawn-
         a3        s7             count n to areg
         jsz       nonp           
nlpp     =         *              
         a0        a3-1           
         v1,a1     s3             n+ to plist
         a1        a1+1           i+1
         a3        a3-1           count down one n+
         jan       nlpp           
nonp     =         *              
         s0        v7,a5          
         s7        v7,a5          
         a5        o'24           rook pointer
         s4        5000           value of rook+
         a3        s7             
         jsz       nobp           
blpp     =         *              
         a0        a3-1           
         v1,a1     s3             
         a1        a1+1           
         a3        a3-1           
         jan       blpp           
nobp     =         *              
         s0        v7,a5          
         s7        v7,a5          
         s3        9000           value of queen+
         a5        o'25           queen pointer
         a3        s7             
         jsz       norp           
rlpp     =         *              
         a0        a3-1           
         v1,a1     s4             
         a1        a1+1           
         a3        a3-1           
         jan       rlpp           
norp     =         *              
         s0        v7,a5          
         s7        v7,a5          
         s4        o'7346545      value of k+
         a2        o'76           - knight pointer
         a4        0              
         a3        s7             
         jsz       noqp           
qlpp     =         *              
         a0        a3-1           
         v1,a1     s3             
         a1        a1+1           
         a3        a3-1           
         jan       qlpp           
noqp     =         *              
         s0        t70            
         s4        s4<9           now = 10**9
         jsz       nokp           
         v1,a1     s4             
         a1        a1+1           
nokp     =         *              
*     now a1 =pmovs
olstmak  =         *              
         s0        t72            number of pawns
         a1        a1-1           correct a1 for score usage
         s7        t72            
         jsz       nopm           
         s0        s7+s1          remaining pawn-
         v0,a4     s2             pawn to olist
         a4        a4+1           i-1
         jsz       nopm           
         v0,a4     s2             pawn to olist
         a4        a4+1           
nopm     =         *              
         s0        v7,a2          count of n-
         s7        v7,a2          
         s2        t74            piece(bd(sq))
         s3        -3250          value of n-
         a5        o'15           bishop- pointer
         a3        s7             count n to areg
         jsz       nonm           
nlpm     =         *              
         a0        a3-1           
         v0,a4     s3             n- to olist
         a4        a4+1           i-1
         a3        a3-1           count down one n-
         jan       nlpm           
nonm     =         *              
         s0        v7,a5          
         s7        v7,a5          
         a5        o'14           rook pointer
         a3        s7             
         jsz       nobm           
blpm     =         *              
         a0        a3-1           
         v0,a4     s3             
         a4        a4+1           
         a3        a3-1           
         jan       blpm           
nobm     =         *              
         s0        v7,a5          
         s7        v7,a5          
         a5        o'13           queen pointer
         s3        -5000          value of rook-
         v2,a0     s0             list(1) = 0
         a3        s7             
         jsz       norm           
rlpm     =         *              
         a0        a3-1           
         v0,a4     s3             
         a4        a4+1           
         a3        a3-1           
         jan       rlpm           
norm     =         *              
         s0        v7,a5          
         s7        v7,a5          
         a2        -1             initialize iomoves
         s3        -9000          value of queen-
         a5        1              mdepth = 1
         a3        s7             
         jsz       noqm           
qlpm     =         *              
         a0        a3-1           
         v0,a4     s3             
         a4        a4+1           
         a3        a3-1           
         jan       qlpm           
noqm     =         *              
         s0        t71            
         s3        -o'7346545     value of k-
         s3        s3<9           now = -10**9
         jsz       nokm           
         v0,a4     s3             
         a4        a4+1           
nokm     =         *              
*     now a4 =omovs
score    =         *              
*
*       a a a a a a a a    s s s s s s s s   v0=omoves
*       0 1 2 3 4 5 6 7    0 1 2 3 4 5 6 7   v1=pmoves
*                                            v2=list
*         p i i o m 2      p r p l t t s l   t74=piece(board(sq))
*         m o p m d        l t c m p p m m
*                                        1
*
*   $20    =    *
         s0        t74            player=sign(piece)
         a4        a4-1           correct a4 for score usage
         a3        -1             initialize ipmoves
         jsp       l250m          .not.player=+
l150p    =         *              
         a0        a3-a1          ipmoves-pmovs
         a3        a3+1           ipmoves=ipmoves+1
         s6        v2,a5          list(mdepth-1)
         s4        v1,a3          tpiece=pmoves(ipmovs)
         jap       l400           no more pieces so quit
l300p    =         *              
         a5        a5+1           mdepth=mdepth+1
         s6        s6-s2          add sign*piece
         s2        s4             piece=tpiece
         v2,a5     s6             list(mdepth)=
l250p    =         *              
         a0        a2-a4          
         a2        a2+1           
         s6        v2,a5          
         s5        v0,a2          tpiece=omoves(iomovs)
         jap       l400           
l301p    =         *              
         a5        a5+1           
         s6        s6-s2          
         s2        s5             
         v2,a5     s6             
         j         l150p          
l250m    =         *              
         a0        a2-a4          
         a2        a2+1           
         s6        v2,a5          
         s5        v0,a2          tpiece=omoves(iomovs)
         jap       l400           
l301m    =         *              
         a5        a5+1           
         s6        s6+s2          
         s2        s5             
         v2,a5     s6             
l150m    =         *              
         a0        a3-a1          ipmoves-pmovs
         a3        a3+1           ipmoves=ipmoves+1
         s6        v2,a5          list(mdepth-1)
         s4        v1,a3          tpiece=pmoves(ipmovs)
         jap       l400           no more pieces so quit
l300m    =         *              
         a5        a5+1           mdepth=mdepth+1
         s6        s6+s2          add sign*piece
         s2        s4             piece=tpiece
         v2,a5     s6             list(mdepth)=
         j         l250m          
         bss       7              startbuf
l400     =         *              a5=mdepth
         s1        v2,a5          list(md)
         s4        a5             md
         a5        a5-1           md=md-1
         s3        <1             1
         a0        a5-1           check for dun
         s7        v2,a5          list(md-1)
         s0        s4&s3          even or odd?
         a5        a5-1           md=md-1
         jam       ret0           if dun
         jsz       l500e          if even
l500o    =         *              
         s0        s1-s7          bigger?
         s6        v2,a5          next list entry
         a5        a5-1           md=md-1
         jaz       l700           return s1
         jsm       l600o          s1 ok
         s1        s7             s7 better
l600o    =         *              
         a0        a5+1           dun?
l501o    =         *              
         s0        s1-s6          smaller?
         s7        v2,a5          next list item
         a5        a5-1           md=md-1
         jaz       l700           
         jsp       l601o          
         s1        s6             better
l601o    =         *              
         a0        a5+1           
         j         l500o          
l500e    =         *              
         s0        s7-s1          bigger?
         s6        v2,a5          next list entry
         a5        a5-1           md=md-1
         jaz       l700           return s1
         jsm       l600e          s1 ok
         s1        s7             s7 better
l600e    =         *              
         a0        a5+1           dun?
l501e    =         *              
         s0        s6-s1          smaller?
         s7        v2,a5          next list item
         a5        a5-1           md=md-1
         jaz       l700           
         jsp       l601e          
         s1        s6             better
l601e    =         *              
         a0        a5+1           
         j         l500e          
ret0     =         *              
         s1        0              
l700     =         *              
         j         b0             
*l7
         bss       2              startbuf
qb0      =         *              
         vm        v5,n           locate empty squares on qb 0+-
*   loop to here
         a4        zs7            locate 1st qb 0+-
         s0        s7             any? qb 0+-
         s5        sb             erase bit
         s5        s5>a4          position erase bit
         s6        v5,a4          get piece 1+-
         jsz       ndqb0          dun 0+-
         s7        #s5&s7         empty a square
         a2        o'20           piece base adr
         a6        s6             piece to areg
         a6        a2+a6          piece adr
         s3        vm             empty 0+-
         a2        zs3            how many empty 1+-
         s6        v7,a6          get current sum
         s3        #s5&s3         clear a qb0+-
         a0        a2-a4          same?
         s0        s7             
         vm        s3             reset vm for next qb+-
         s6        s6-s1          add 1 to sum
         jan       ndqb0          if not same, then exit 1+-
         v7,a6     s6             return new sum
         jsn       qb0+1          repeat for next qb0+-
ndqb0    =         *              
         a6        0              
         a4        63             
         s6        6              
         s5        32             
         s3        v7,a5          replace possible knight
         j         aftqb0         
pawn0    =         *              
         s7        t72            pawn sum
         s0        t75            zero iff promoting +
         v5,a6     s0             erase pawn 0+
         s7        s7-s1          add a pawn+
         jsz       qp0            if storing a queen
         t72       s7             store a pawn
         j         aftp0          
qp0      =         *              very rare case
         a2        o'13           queen sum adr+
         s7        v7,a2          get queen sum
         s7        s7-s1          add a queen
         v7,a2     s7             store a queen
         j         aftp0          
         bss       3              startbuf
qb1      =         *              
         vm        v0,n           locate empty squares on qb 1+-
*   loop to here
         a4        zs7            locate 1st qb 1+-
         s0        s7             any? qb 1+-
         s5        sb             erase bit
         s5        s5>a4          position erase bit
         s6        v0,a4          get piece 1+-
         jsz       ndqb1          dun 1+-
         s7        #s5&s7         empty a square
         a2        o'20           piece base adr
         a6        s6             piece to areg
         a6        a2+a6          piece adr
         s3        vm             empty 1+-
         a2        zs3            how many empty 1+-
         s6        v7,a6          get current sum
         s3        #s5&s3         clear a qb1+-
         a0        a2-a4          same?
         s0        s7             
         vm        s3             reset vm for next qb+-
         s6        s6-s1          add 1 to sum
         jan       ndqb1          if not same, then exit 1+-
         v7,a6     s6             return new sum
         jsn       qb1+1          repeat for next qb1+-
ndqb1    =         *              
         a6        0              
         a4        63             
         s6        6              
         s5        32             
         s3        v7,a5          replace possible knight
         j         aftqb1         
pawn1    =         *              
         s7        t73            pawn sum
         s0        t76            zero iff promoting -
         v0,a6     s0             erase pawn 1-
         s7        s7-s1          add a pawn-
         jsz       qp1            if storing a queen
         t73       s7             store a pawn
         j         aftp1          
qp1      =         *              very rare case
         a2        o'25           queen sum adr+
         s7        v7,a2          get queen sum
         s7        s7-s1          add a queen
         v7,a2     s7             store a queen
         j         aftp1          
         bss       3              startbuf
qb2      =         *              
         vm        v6,n           locate empty squares on qb 2+-
*   loop to here
         a4        zs7            locate 1st qb 2+-
         s0        s7             any? qb 2+-
         s5        sb             erase bit
         s5        s5>a4          position erase bit
         s6        v6,a4          get piece 1+-
         jsz       ndqb2          dun 2+-
         s7        #s5&s7         empty a square
         a2        o'20           piece base adr
         a6        s6             piece to areg
         a6        a2+a6          piece adr
         s3        vm             empty 2+-
         a2        zs3            how many empty 1+-
         s6        v7,a6          get current sum
         s3        #s5&s3         clear a qb2+-
         a0        a2-a4          same?
         s0        s7             
         vm        s3             reset vm for next qb+-
         s6        s6-s1          add 1 to sum
         jan       ndqb2          if not same, then exit 1+-
         v7,a6     s6             return new sum
         jsn       qb2+1          repeat for next qb2+-
ndqb2    =         *              
         a6        0              
         a4        63             
         s6        6              
         s5        32             
         s3        v7,a5          replace possible knight
         j         aftqb2         
pawn2    =         *              
         s7        t72            pawn sum
         s0        t75            zero iff promoting +
         v6,a6     s0             erase pawn 2+
         s7        s7-s1          add a pawn+
         jsz       qp2            if storing a queen
         t72       s7             store a pawn
         j         aftp2          
qp2      =         *              very rare case
         a2        o'13           queen sum adr+
         s7        v7,a2          get queen sum
         s7        s7-s1          add a queen
         v7,a2     s7             store a queen
         j         aftp2          
         bss       3              startbuf
qb3      =         *              
         vm        v5,n           locate empty squares on qb 3+-
*   loop to here
         a4        zs7            locate 1st qb 3+-
         s0        s7             any? qb 3+-
         s5        sb             erase bit
         s5        s5>a4          position erase bit
         s6        v5,a4          get piece 3+-
         jsz       ndqb3          dun 3+-
         s7        #s5&s7         empty a square
         a2        o'20           piece base adr
         a6        s6             piece to areg
         a6        a2+a6          piece adr
         s3        vm             empty 3+-
         a2        zs3            how many empty 3+-
         s6        v7,a6          get current sum
         s3        #s5&s3         clear a qb3+-
         a0        a2-a4          same?
         s0        s7             
         vm        s3             reset vm for next qb+-
         s6        s6-s1          add 1 to sum
         jan       ndqb3          if not same, then exit 3+-
         v7,a6     s6             return new sum
         jsn       qb3+1          repeat for next qb3+-
ndqb3    =         *              
         a6        0              
         a4        63             
         s6        6              
         s5        32             
         j         aftqb3         
pawn3    =         *              
         s7        t73            pawn sum
         s0        t76            zero iff promoting -
         v5,a6     s0             erase pawn 3-
         s7        s7-s1          add a pawn-
         jsz       qp3            if storing a queen
         t73       s7             store a pawn
         j         aftp3          
qp3      =         *              very rare case
         a2        o'25           queen sum adr+
         s7        v7,a2          get queen sum
         s7        s7-s1          add a queen
         v7,a2     s7             store a queen
         j         aftp3          
         bss       4              startbuf
qr0      =         *              
         vm        v0,n           locate empty squares on qr 0+-
*   loop to here
         a4        zs7            locate 1st qr 0+-
         s0        s7             any? qr 0+-
         s5        sb             erase bit
         s5        s5>a4          position erase bit
         s6        v0,a4          get piece 1+-
         jsz       ndqr0          dun 0+-
         s7        #s5&s7         empty a square
         a2        o'20           
         a6        s6             piece to areg
         a6        a2+a6          
         s3        vm             empty 0+-
         a2        zs3            how many empty 1+-
         s6        v7,a6          get current sum
         s3        #s5&s3         clear a qr0+-
         a0        a2-a4          same?
         s0        s7             
         vm        s3             reset vm for next qr+-
         s6        s6-s1          add 1 to sum
         jan       ndqr0          if not same, then exit 1+-
         v7,a6     s6             return new sum
         jsn       qr0+1          repeat for next qr0+-
ndqr0    =         *              
         a6        0              
         a4        63             
         s6        6              
         s5        32             
         j         aftqr0         
qr3      =         *              
*   loop to here
         a4        zs7            locate 1st qr 3+-
         s0        s7             any? qr 3+-
         s5        sb             erase bit
         s5        s5>a4          position erase bit
         s6        v6,a4          get piece 1+-
         jsz       aftqr3         dun 3+-
         s7        #s5&s7         empty a square
         a2        o'20           
         a6        s6             piece to areg
         a6        a2+a6          
         s3        vm             empty 3+-
         a2        zs3            how many empty 1+-
         s6        v7,a6          get current sum
         s3        #s5&s3         clear a qr3+-
         a0        a2-a4          same?
         s0        s7             
         vm        s3             reset vm for next qr+-
         s6        s6-s1          add 1 to sum
         jan       aftqr3         if not same, then exit 1+-
         v7,a6     s6             return new sum
         jsn       qr3            repeat for next qr3+-
         j         aftqr3         
         bss       1              startbuf
qr2      =         *              
         vm        v0,n           locate empty squares on qr 2+-
*   loop to here
         a4        zs7            locate 1st qr 2+-
         s0        s7             any? qr 2+-
         s5        sb             erase bit
         s5        s5>a4          position erase bit
         s6        v0,a4          get piece 1+-
         jsz       ndqr2          dun 2+-
         s7        #s5&s7         empty a square
         a2        o'20           
         a6        s6             piece to areg
         a6        a2+a6          
         s3        vm             empty 2+-
         a2        zs3            how many empty 1+-
         s6        v7,a6          get current sum
         s3        #s5&s3         clear a qr2+-
         a0        a2-a4          same?
         s0        s7             
         vm        s3             reset vm for next qr+-
         s6        s6-s1          add 1 to sum
         jan       ndqr2          if not same, then exit 1+-
         v7,a6     s6             return new sum
         jsn       qr2+1          repeat for next qr2+-
ndqr2    =         *              
         s6        6              
         j         aftqr2         
qr1      =         *              
         vm        v6,n           locate empty squares on qr 1+-
*   loop to here
         a4        zs7            locate 1st qr 1+-
         s0        s7             any? qr 1+-
         s5        sb             erase bit
         s5        s5>a4          position erase bit
         s6        v6,a4          get piece 1+-
         jsz       ndqr1          dun 1+-
         s7        #s5&s7         empty a square
         a2        o'20           
         a6        s6             piece to areg
         a6        a2+a6          
         s3        vm             empty 1+-
         a2        zs3            how many empty 1+-
         s6        v7,a6          get current sum
         s3        #s5&s3         clear a qr1+-
         a0        a2-a4          same?
         s0        s7             
         vm        s3             reset vm for next qr+-
         s6        s6-s1          add 1 to sum
         jan       ndqr1          if not same, then exit 1+-
         v7,a6     s6             return new sum
         jsn       qr1+1          repeat for next qr1+-
ndqr1    =         *              
         a6        0              
         a4        63             
         s3        v7,a5          restore s3 for return
         s6        6              
         s5        32             
         j         aftqr1         
         con       -4000000       
         con       -9000          
         con       -5000          
         con       -3250          
         con       -3250          
         con       -1000          
pval     con       0000           
         con       1000           
         con       3250           
         con       3250           
         con       5000           
         con       9000           
         con       4000000        
         bss       3              startbuf
         include   'global'       
         end       
