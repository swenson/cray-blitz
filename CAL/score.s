         include   'common' 
         ident     score
         entry     SCORE
         ext       SCOREP         
         ext       SCORETRD         
         ext       SCOREKP        
         ext       SCOREKM        
         ext       SCORE1         
         ext       SCOREM         
         ext       SCOREBRP       
         ext       SCOREBOC       
         ext       SCOREDV        
*
*      b62=return since other routines clobber b63/b64
*
         align     
         
l1000    =         *              check for call to scorem
         a5        5              
         a0        a3-a5          npp-5
         s2        drawsc,        
         s4        ply,a1         
         s3        900000         
         s0        s1-s3          eval-900000
         jap       l1100          
         a0        a4-a5          nop-5
         jsp       l1100          
         s2        s2+s4          add ply to draw score
         jap       l1100          
         tscore,a1 s2             
         j         b62            
l1100    =         *              
         r         SCOREM         
         a7        b60            
         s6        tscore,a7      
         s2        drawsc,        
         s1        100            
         j         l996           
SCORE    =         *              
         a1        b60            task common base address
         s0        npawns,a1      
         a3        nppcs,a1       
         s1        eval,          
         a4        nopcs,a1       
         s7        0              
         a7        b0             
         pscore,a1 s7             
         b62       a7             
         jsz       l1000          
         r         SCOREP         
         r         SCORETRD       
         a0        devdone,       
         jan       skipdv         
         r         SCOREDV        
skipdv   =         *              
         a1        b60            task common base addr
         a0        rpflag,a1      
         a5        rpflag,a1      need for l1200
         jan       l1200          
l750     =         *              
         r         SCOREKP        
         r         SCOREKM        
         r         SCORE1         
         a7        b60            
         s1        nppcs,a7       
         s2        nopcs,a7       
         a5        pscore,a7      
         a1        minmax,a7      mm(1)
         a2        mscore,a7      
         a3        minmax+1,a7    mm(2)
         s3        <2             
         s0        s1\s3          
         s3        s2\s3          
         a0        s3             
         s5        nppwns,a7      
         a4        nopwns,a7      
         jsn       l800           
         jan       l800           
         r         SCOREBOC       
         a7        b60            
         a5        pscore,a7      
         a1        minmax,a7      mm(1)
         a2        mscore,a7      
         a3        minmax+1,a7    mm(2)
         s5        nppwns,a7      
         a4        nopwns,a7      
*  a2=msc,a4=nop,a5=psc,a1=mm1,s5=npp,a3=mm1
l800     =         *              
         s6        +a5            extend pscore
         a0        a5-a1          psc-mm1
         a1        nppcs,a7       
         a6        a5+a2          tsc
         jap       label1         
         minmax,a7 s6             
label1   =         *              
         a0        a3-a5          mm1-psc
         a2        nopcs,a7       for 8100
         a3        5              
         jap       label2         
         minmax+1,a7 s6           
label2   =         *              
         s0        s5             is nppwns.ne.0
         a5        a1+a3          np+5
         s2        drawsc,        
         a0        a4             nopwns for 900
         s1        100            
         jsn       l900           if nppwns.ne.0
         a0        a1-a3          nppcs-5
         s0        +a6            check sign of tsc
         s5        -10000         bonus
         a3        s5             bonus
         a2        a2+a4          nopcs+nopwns
         jap       rtnt2          
         jsm       rtnt2          
*    a6=tsc,a5=psc
rtnp     =         *              
         a0        a2-a5          no-np-5
         s7        s6+s5          add bonus
         jam       skip1          if no bonus added
         s6        s7             tsc+bonus to s6
skip1    =         *
         tscore,a7 s6             tsc=psc
         j         l996           
rtnt2    =         *              
         a0        a2-a5          no-np-5
         a6        a6+a3          add bonus
         jam       skip2          if no bonus added
         s0        +a6            tsc + bonus to s0
skip2    =         *
         tscore,a7 s0             
         j         l990           
l900     =         *              
         s0        +a6            check sign of tsc
         a4        a2+a3          nopcs+5
         jan       rtnt1          if nopwns.ne.0
         a0        a2-a3          nopcs-5
         a5        s5             nppwns
         s5        10000          bonus
         a3        s5             bonus
         a1        a1+a5          nppcs+nppwns
         jap       rtnt           
         jsm       rtnp2          
*    a6=tsc,a7=b60
rtnt     =         *              
         a0        a1-a4          np-no-5
         a6        a6+a3          add bonus
         jam       skip3          if no bonus added
         s0        +a6            tsc + bonus to s0
skip3    =         *
rtnt1    =         *              
         tscore,a7 s0             
         j         l990           
rtnp2    =         *              
         a0        a1-a4          np-no-5
         s7        s6+s5          add bonus
         jam       skip4          if no bonus added
         s6        s7             tsc+bonus to s6
skip4    =         *
         tscore,a7 s6             tsc=psc
l996     =         *              tsc=s6,s2=drawsc,s1=100
         s0        s6-s2          
         s7        s6+s1          
         jsp       skip5          
         j         b62            
skip5    =         *
         tscore,a7 s7             
         j         b62            
l990     =         *              tsc=s0,s2=drawsc,s1=100
         t41       s0             
         s6        t41            
         s0        s6-s2          
         s7        s6+s1          
         jsp       skip6          
         j         b62            
skip6    =         *
         tscore,a7 s7             
         j         b62            
*         a1=b60,a5=rpflag
l1200    =         *              
         a3        3              
         a0        a5-a3          
         a6        nppcs,a1       
         a7        nopcs,a1       
         jaz       l750           
         a2        0              return
         return,a1 a2             
         a0        a6+a7          check for nopcs+nppcs=0
         a4        a6+a7          nopcs+nppcs
         jaz       l1201          
         a0        a3-a4          
         jan       l750           
l1201    =         *              
         r         SCOREBRP       
         a1        b60            
         a0        return,a1      
         jaz       l750           
         j         b62            
         include   'global' 
         end       
         ident     score1
         entry     SCORE1
*
*        herein we use b41, b42, b44 & b45
*
         align     
SCORE1   =         *              
*   do - pieces first
         s6        0
         a7        b60            task common base adr
         a1        pstart+1,a7    pstart(2)
         a2        pcend+1,a7     pcend(2)
         t40       s6             pbish&obish=0
         a3        madr           jump base adr
         b44       a3             
         a1        a1+a7          where +base
         a5        vlist-1,a1     vlist(where)
         b41       a1             
         a2        a2+a7          pend + base
         a6        plist-1,a1     plist(where)
         b42       a2             
*     a5=piece,a6=square
l1000a   =         *              
         a2        parceln-parcel1  length of branch (ymp=2, c90=3)
         a5        a5*a2          factor in branch instruction length
         a7        b44            jump base adr
         a5        a7-a5          piece was negative
         b45       a5             
         j         b45            
parcel1  =         *
madr     j         l1000b         piece=0
parceln  =         *
         j         l1000b         piece=-1
         j         l1250          
         j         l1350          
         j         l1450          
         j         l1550          
*        j         l1000b
l1000b   =         *              
         a1        b41            
         a5        vlist,a1       next piece
         a2        b42            
         a0        a1-a2          done with - pieces?
         a3        a1+1           next where
         a6        plist,a1       next square
         b41       a3             
         jam       l1000a         
l1000    =         *              now do + pieces
         a7        b60            
         a2        pcend,a7       pcend(1)
         a5        vlist,a7       first +piece
         a1        padr           jump base adr
         b44       a1             
         a1        a7+1           pstart(1)
         a6        plist,a7       first +square
         a2        a2+a7          
         b41       a1             
         b42       a2             
*     a5=piece,a6=square
l1005a   =         *              
         a2        parceln-parcel1  length of jump (ymp=2, c90=3)
         a7        b44            jump base adr
         a5        a5*a2          factor in length of jump
         a5        a7+a5          piece was positive
         b45       a5             
         j         b45            
         j         l1005b         spacer
padr     j         l1005b         piece=0
         j         l1005b         piece=1
         j         l1200          
         j         l1300          
         j         l1400          
         j         l1500          
*        j         l1005b
l1005b   =         *              
         a1        b41            
         a5        vlist,a1       next piece
         a2        b42            
         a0        a1-a2          done?
         a3        a1+1           next where
         a6        plist,a1       next square
         b41       a3             
         jam       l1005a         
l1005    =         *              now consider development
         j         b0             
*
*   + piece scoring
*
l1500    =         *              ** program queen **
         s3        pqueenb-1,a6   pqueenb(square)
         a7        b60            
         s2        osafety,a7     osafety
         s1        pscore,a7      pscore
         a1        b41            
         a2        b42            
         a5        vlist,a1       
         a6        plist,a1       
         a0        a1-a2          
         a1        a1+1           
         s1        s1-s2          pscore=pscore-osafety
         s1        s1+s3          pscore=pscore+pqueenb(square)
         b41       a1             
         pscore,a7 s1             
         jam       l1005a         
         j         l1005          
         bss       1              startbuf
l1200    =         *              ** program knight **
         a7        pknightb-1,a6    pknightb(square)
         a4        cfiles-1,a6    file
         s1        cranks-1,a6    rank
         a2        b60            
         a0        popost-1,a6    
         a4        a4+a2          
         s2        ofirst-2,a4    ofirst(file-1)
         a3        pscore,a2      
         s3        ofirst,a4      ofirst(file+1)
         s0        s1-s2          rank-of(f-1)
         a3        a7+a3          pscore=pscore+pknightb(square)
         jaz       l230           
         jsm       l230           
         s0        s1-s3          rank-of(f+1)
         a5        50             noutpost
         jsm       l230           
         a3        a3+a5          
l230     =         *              
l1206    =         *              
         s1        +a3            extend sign
         a1        b41            
         pscore,a2 s1             
         a2        b42            
         a0        a1-a2          
         a5        vlist,a1       
         a6        plist,a1       
         a1        a1+1           
         b41       a1             
         jam       l1005a         
         j         l1005          
l1300    =         *              ** program bishop **
         a7        pbishopb-1,a6  pbishopb(square)
         s1        t40            pbish&obish
         s2        1
         a1        b60            
         s0        s1&s2          extract pbish  
         s1        s1!s2          pbish=1
         a4        pscore,a1      
         jsz       l1306          pbish=0
         a3        250            bbonus
         a4        a4+a3          psc+100
l1306    =         *              
         t40       s1             pbish=1
         a2        b42            
         a3        b41            
         a0        a3-a2          
         a5        vlist,a3       
         a6        plist,a3       
         a4        a4+a7          pscore=pscore+pbishopb(square)
         a3        a3+1           
         s1        +a4            extend sign
         b41       a3             
         pscore,a1 s1             
         jam       l1005a         
         j         l1005          
l1400    =         *              ** program rook **
         s5        prookb-1,a6     prookb(square)
         a5        a6             
         a6        b60            
         a7        a5+a6          
         s0        board-2,a7     board(square-1)
         a4        pscore,a6      
         a0        board,a7       board(square+1)
         a1        cfiles-1,a5    file
         a2        cranks-1,a5    rank
         a5        s5             prookb(square)
         a4        a4+a5          pscore=pscore+prookb(square)
         a5        100            rtrapped
         jsz       l1401          board(square-1)=0
         jsm       l1401          board(square-1)<0
         jaz       l1401          board(square+1)=0
         jam       l1401          board(square+1)<0
         a4        a4-a5          pscore=pscore-rtrapped
l1401    =         *              
         a1        a1+a6          
         s3        nopwns,a6      
         s0        pcount-1,a1    
         s1        ocount-1,a1    
         s5        oweakp-1,a1
         s2        ppass-1,a1     
         s7        opass-1,a1     
         a7        olast-1,a1     
         a3        plast-1,a1     
         a5        pfirst-1,a1    
         s6        ofirst-1,a1    
         jsz       l411           pcount=0?
l420     =         *              
         s0        s2             ck ppass
l430     =         *              
         a0        s7             ck opass
         jsn       l431           ppass=true?
l440     =         *              
         a2        a2-a7          rank-olast
         s7        +a4            extend sign
         s0        +a2            extend sign
         pscore,a6 s7             put away pscore
         jaz       l1005b         opass=false?
         a1        s6             
         a0        a1-a7          
         jsm       l1005b         
         a5        9              
         a3        a5-a7          
         a1        10             rbehindp
         a3        a1*a3          
         jan       l1005b         
         a4        a4+a3          
         s7        +a4            
         pscore,a6 s7             
         j         l1005b         
l411     =         *              
         a1        30             rhalfopn
         s0        s1             
         a4        a1+a4          
         a1        70             ropen (-rhalfopn)
         jsn       l411b          
         a4        a4+a1          
         j         l420           
l411b    s0        s5             oweakp(file) set?
         jsz       l420           
         a1        50             atkweakp
         a4        a4+a1
         j         l420
l431     =         *              
         a1        a3-a2          plast-rank
         a0        a5-a3          
         s0        +a1            preserve a0
         a5        2              
         a3        a3-a5          plast-2
         a1        10             rbehindp
         s7        +a4            
         a3        a1*a3          
         pscore,a6 s7             
         jsm       l1005b         
         jan       l1005b         
         a4        a4+a3          
         s7        +a4            
         pscore,a6 s7             
         j         l1005b         
         bss       6              startbuf
*
*   - piece scoring
*
l1550    =         *              ** opponent queen **
         s3        oqueenb-1,a6   oqueenb(square)
         a7        b60            
         s2        psafety,a7     
         s1        pscore,a7      
         a1        b41            
         a2        b42            
         a5        vlist,a1       
         a6        plist,a1       
         a0        a1-a2          
         a1        a1+1           
         s1        s1+s2          pscore=pscore+psafety
         s1        s1-s3          pscore=pscore-oqueenb(square)
         b41       a1             
         a7        b60            
         pscore,a7 s1             
         jam       l1000a         
         j         l1000          
l1250    =         *              ** opponent knight **
         a7        oknightb-1,a6    oknightb(square)
         a4        cfiles-1,a6    file
         s1        cranks-1,a6    rank
         a2        b60            
         a0        oopost-1,a6    
         a4        a4+a2          
         s2        pfirst-2,a4    pfirst(file-1)
         a3        pscore,a2      
         s3        pfirst,a4      pfirst(file+1)
         s0        s2-s1          pf(f-1)-rank
         a3        a3-a7          pscore=pscore-oknightb(square)
         jaz       l260           
         jsm       l260           
         s0        s3-s1          pf(f+1)-rank
         a5        50             noutpost
         jsm       l260           
         a3        a3-a5          
l260     =         *              
         s1        +a3            extend sign
         a1        b41            
         pscore,a2 s1             
         a2        b42            
         a0        a1-a2          
         a5        vlist,a1       next piece
         a6        plist,a1       
         a1        a1+1           
         b41       a1             
         jam       l1000a         
         j         l1000          
l1350    =         *              ** opponent bishop **
         a7        obishopb-1,a6  obishopb(square)
         s1        t40            pbish&obish
         s2        2
         a1        b60            
         s0        s1&s2          extract obish
         s1        s1!s2          obish=1
         a4        pscore,a1      
         jsz       l1356          obish = 0
         a3        250            bbonus
         a4        a4-a3          psc-100
l1356    =         *              
         t40       s1             obish=1
         a2        b42            
         a3        b41            
         a0        a3-a2          
         a5        vlist,a3       
         a6        plist,a3       
         a4        a4-a7          pscore-pscore-obishopb(square)
         a3        a3+1           
         s1        +a4            extend sign
         b41       a3             
         pscore,a1 s1             
         jam       l1000a         
         j         l1000          
l1450    =         *              ** opponent rook **
         s5        orookb-1,a6     orookb(square)
         a7        a6             save a6
         a6        b60            
         a5        a7+a6          
         s1        board-2,a5     board(square-1)
         a5        board,a5       board(square+1)
         a1        cfiles-1,a7    file
         a2        cranks-1,a7    rank
         a4        pscore,a6      
         s3        98             
         s0        s1             
         a0        a5             
         a7        s5             orookb(square)
         a4        a4-a7          pscore=pscore-orookb(square)
         jsm       l1451          board(square-1)<0
         s0        s1-s3          
         jsm       l1453          board(square) <> 99, done
l1451    =         *              
         jam       l1452          board(square+1)<0
         a7        98             
         a0        a5-a7          
         jam       l1453          
l1452    =         *              
         a5        100            rtrapped
         a4        a4+a5          pscore=pscore+rtrapped
l1453    =         *              
         a1        a1+a6          
         s3        nppwns,a6      
         s0        ocount-1,a1    
         s1        pcount-1,a1    
         s5        pweakp-1,a1
         s2        ppass-1,a1     
         s7        opass-1,a1     
         a7        olast-1,a1     
         a3        plast-1,a1     
         a5        pfirst-1,a1    pfirst(file)
         s6        ofirst-1,a1    
         jsz       l461           ocount=0?
l470     =         *              
         s0        s2             ck ppass
l480     =         *              
         a0        s7             ck opass
         jsn       l481           ppass=true?
l490     =         *              
         a2        a2-a7          rank-olast
         s7        +a4            extend sign
         s0        +a2            extend sign
         pscore,a6 s7             put away pscore
         jaz       l1000b         opass=false?
         a1        s6             ofirst
         a0        a1-a7          ofirst-olast
         jsm       l1000b         
         a5        9              
         a3        a5-a7          
         a1        10             rbehindp
         a3        a1*a3          
         jan       l1000b         
         a4        a4-a3          
         s7        +a4            
         pscore,a6 s7             
         j         l1000b         
         bss       2              startbuf
l461     =         *              
         a1        30             rhalfopn
         s0        s1             
         a4        a4-a1          
         a1        70             ropen (-rhalfopn)
         jsn       l461b          
         a4        a4-a1          
         j         l470           
l461b    s0        s5             pweakp(file) set?
         jsz       l470           nope
         a1        50             atkweakp
         a4        a4-a1
         j         l470           
l481     =         *              a4=pscore
         a1        a3-a2          plast-rank
         a0        a5-a3          pfirst-plast
         s0        +a1            preserve a0
         a5        2              
         a3        a3-a5          plast-2
         a1        10             rbehindp
         s7        +a4            pscore
         a3        a1*a3          
         pscore,a6 s7             put away pscore
         jsm       l1000b         
         jan       l1000b         
         a4        a4-a3          
         s7        +a4            
         pscore,a6 s7             
         j         l1000b         
         include   'global' 
         end       
         include   'common'       
         ident     scoredv
         entry     SCOREDV
*
*           register utilization:    a7-a7 and s0-s7 are used
*
*           b30      board(34) prefetch save
*           b31      board(37) prefetch save
*           b37      return address
*
*           t32      shiftr(rdepth+1,1)
*           t34      board(35)
*           t35      board(36)
*           t36      board(24)-3 (=0 means moved already)
*           t37      board(27)-3 (=0 means moved already)
*
SCOREDV  =         *
         a7        b60            task common base address
         a1        board+22,a7    board(23)
         a2        board+27,a7    board(28)
         a3        board+24,a7    board(25)
         s1        board+23,a7    board(24)
         s2        board+26,a7    board(27)
         s3        board+25,a7    board(26)
         a4        board+33,a7    board(34) (pre-fetch)
         a6        board+36,a7    board(37) (pre-fetch)
         s7        0              tpscor=0
         a5        b00            return address
         b37       a5             save
         a5        2              (knight)
         s5        3              (bishop)
         
*
*        penalize each unmoved bishop or knight (program)
*
         a0        a1-a5          board(23)-knight
         s0        s1-s5          board(24)-bishop
         s6        10             develop
         jan       nokt1          if board(23) .ne. 2
         s7        s7-s6          tpscor=tpscor-develop
nokt1    =         *
         a0        a2-a5          board(28)-knight
         jsn       nob1           if board(24) .ne. 3
         s7        s7-s6          tpscor=tpscor-develop
nob1     =         *
         t36       s0             save board(24)-3
         s0        s2-s5          board(27)-bishop
         jan       nokt2          if board(28) .ne. 2
         s7        s7-s6          tpscor=tpscor-develop
nokt2    =         *
         jsn       nob2           if board(27) .ne. 3
         s7        s7-s6          tpscor=tpscor-develop
nob2     =         *
         t37       s0             save board(27)-3
         s0        s7             test tpscor
         s6        100            qearly
         s5        5              (queen)
         a5        5              (queen)
         jsz       queen1         if minor pieces moved, ignore queen    
*        a5,s5=5 (queen) s7=tpscor, s6=100 (qearly)
         s0        s3-s5          board(26)-5
         a0        a3-a5          board(25)-5
         jsz       queen1         if board(26) .eq. 5
         jaz       queen1         if(board(25) .eq. 5
         s7        s7-s6          tpscor=tpscor-qearly
queen1   =         *

*        penalize blocked or unmoved center pawns. (program)
*        s1=board(24), s2=board(27), s6=pblocked, s7=tpscor
        
         s3        board+34,a7    board(35)
         s4        board+35,a7    board(36)
         a1        board+44,a7    board(45)
         a2        board+45,a7    board(46)
         b30       a4             save board(34)
         b31       a6             save board(37)
         a6        d4,            d4 opening flag
         s5        1              (pawn)
         a5        2              (>pawn)
         s0        s3-s5          board(35)-pawn
         t34       s3             save board(35)
         t35       s4             save board(36)
         a0        a1-a5          board(45)-2
         a1        color,         program white = 1, black = 2
         s5        3              (bishop)
         jsn       notp2          if board(35) .ne. 1
         s0        s1-s5          board(24)-bishop
         jam       notp1          if board(45) .lt. 2
         s7        s7-s6          tpscor=tpscor-pblocked
notp1    =         *
         jsn       notp2          if(board(24).ne.3 .or. board(35).ne.1
         s6        100            develop*10
         s7        s7-s6          tpscor=tpscor-develop
notp2    =         *
         s3        board+43,a7    board(44) (pre-fetch)
         s5        1              (pawn)
         s0        s4-s5          board(36)-pawn
         s5        3              (bishop)
         a0        a2-a5          board(46)-2
         jsn       notp4          if board(36) .ne. 1
         s0        s2-s5          board(27)-bishop
         jam       notp3          if board(46) .lt. 2
         s6        100            pblocked
         s7        s7-s6          tpscor=tpscor-pblocked
notp3    =         *
         jsn       notp4          if(board(27).ne.3 .or. board(36).ne.1
         s6        100            develop*10
         s7        s7-s6          tpscor=tpscor-develop
notp4    =         *
         s4        board+46,a7    board(47) (pre-fetch)

*        a1=color, a6=d4 flag, s3=board(44), s4=board(47)
*        b30=board(34), b31=board(37)
*        penalize unmoved c-pawn or blocked c-pawn on d4 openings
         a3        board+32,a7    board(33) (pre-fetch)
         a4        board+37,a7    board(38) (pre-fetch)
         a0        a6             test d4=0
         a6        taskid,a7      taskid (pre-fetch)
         s1        +a1            get color
         s2        1
         s0        s1-s2          test color
         a1        b30            a1=board(34) s3=board(44)
         jaz       notd41         not d4 opening, skip
         a2        1              (pawn)
         s2        2              (>pawn)
         jsn       pblack1        if(color .ne. 1)
         a1        b31            a1=board(37)
         s3        s4             s3=board(47)
pblack1  =         *
         a0        a1-a2          board(x)-1 [x=34 or 37]
         s0        s3-s2          board(x)-2 [x=44 or 47]
         s5        100            develop*10
         s6        200            pblocked*2
         jan       notd41         c-pawn moved already
         s7        s7-s5          tpscor=tpscor=develop*10
         jsm       notd41         c-pawn not blocked
         s7        s7-s6          tpscor=tpscor-pblocked*2
notd41   =         *
         s6        pscore,a7      get pscore
         a6        a6-1           taskid-1
         a5        50             dimension curmvs(50,16)
         a6        a5*a6          loc(curmvs(0,taskid))
         s1        curmvs,a6      curmvs(1,taskid)
         tpscor,a7 s7             save tpscor
         s6        s7+s6          pscore=pscore+tpscor
         s2        <7             127
         s7        s1             move
         s7        s7>7           align mto
         s7        s7&s2          mto
         s2        s2&s1          mfrom
         s1        s1>20          movgpc
         s3        2              knight
         s0        s1-s3          movgpc-2
         jsn       notan          if(movgpc .ne. 2)
         s3        23
         a3        28
         a2        s2             mfrom
         s0        s2-s3          mfrom-23
         a0        a2-a3          mfrom-28
         jsz       checksq        if(mfrom.eq.23 .and. movgpc.eq.2)
         jaz       checksq        if(mfrom.eq.28 .and. movgpc.eq.2)
notan    =         *
         s3        3              bishop
         s0        s1-s3          movgpc-3
         jsn       retrn          if(movgpc .ne. 3)
         s3        24
         a3        27
         a2        s2             mfrom
         s0        s2-s3          mfrom-24
         a0        a2-a3          mfrom-27
         jsz       develop        if(mfrom.eq.24 .and. movgpc.eq.3)
         jaz       develop        if(mfrom.eq.27 .and. movgpc.eq.3)
retrn    =         *
         pscore,a7 s6             save pscore
         j         b37            and return
checksq  =         *              s7=mto, s6=pscore
         s4        42             h3/a6
         a4        49             a3/h6
         a3        s7             mto
         s0        s7-s4          mto-42
         a0        a3-a4          mto-49
         jsz       retrn          if developing to edge of board
         jaz       retrn          if developing to edge of board
develop  =         *
         s4        50             develop*5
         s6        s6+s4          pscore=pscore+50
         pscore,a7 s6             save pscore
         j         b37            and return
         include   'global'       
         end
         include   'common'       
         ident     scorep     
         entry     SCOREP     
         ext       SCOREP1        
         ext       SCOREP2        
         ext       SCOREP3        
*
*       rewrite for oct84 ppass hash scheme
*       bug fixes of 18may89
*
         align     
SCOREP   =         *              
         a7        b60            task common base adr
         s5        0
         s1        phash,a7       part of scorep
         s2        psizm1,        part of scorep
         a1        ptbl1,         
         tradepsc,a7 s5           tradepsc=0
         a2        ptbl2,         
         s6        pcalls,a7      
         a0        b0             
         b63       a0             
         a6        ply,a7         
         a5        8              
         vl        a5             
         s2        s1&s2          and(phash,psizm1)
         a3        s2             
         a1        a1+a3          pkey1
         s2        s2>1           shift key
         s7        htable-1,a1    htable(pkey1)
         a3        s2             
         a2        a2+a3          pkey2
         s2        1              1
         s5        htable-1,a2    htable(pkey2)
         a6        a6+a7          ply+base
         s4        s1&s2          and(phash,1)
         s4        s4<5           pshft
         s3        <40            bmask
         s1        s1>24          now use left 40 bits
         a4        s4             pshft
         s3        s7&s3          and(ht(pk1),bm)
         s0        s3\s1          .eq.phash?
         s3        <32            2**32-1 (ffffffff)
         s5        s5>a4          ht(pk2).rs.pshft
         pkey1,a7  a1             
         s5        s5&s3          extract ptemp
         jsn       l300           not a match
         s5        s5<32          position ptemp for vm
l10      =         *              
         s3        <16
         s2        pscore,a7      
         a4        nppcs,a7       
         a5        nopcs,a7       
         s4        phashs,a7      
         s7        s7>45          
         s1        s7&s3          extract score+bias from table
         s7        s7>17          position rpflag
         s3        <15            32767
         s1        s1-s3          remove bias
         s3        1              
         s6        s6+s3          pcalls+1 (also done in l300)
         s2        s2+s1          pscore+table score
         s4        s4+s3          phashes+1
         rpflag,a7 s7             
         pcalls,a7 s6             
         pscore,a7 s2             
         phashs,a7 s4             count phashes even if calling scorep2
*a7=base address
*s3=1
*s5 = oweakp(8bits) pweakp(8bits) opass(8bits) ppass(8bits)
*vl=8
         t40       s5             save oweakp, pweakp
         v0        v7-v7          clear to 0
* first do oweakp and pweakp
         vm        s5             left 8 bits are oweakp
         a2        -1             store incr
         v1        s3!v0&vm       has word of 1 if oweakp=1
         a1        oweakp+8       offset(oweakp(9))
         s5        s5<8           position pweakp
         a0        a1+a7          loc(oweakp(9))
         vm        s5             left 8 bits are pweakp
         ,a0,a2    v1             store oweakp
         v2        s3!v0&vm       has word of 1 if pweakp=1
         a1        pweakp+8       offset(pweakp(9))
         a0        a1+a7          loc(pweakp(9))
         ,a0,a2    v2             store pweakp
* now do opass and ppass
         s5        t40
         s5        s5<16          scrub oweakp, pweakp off
         vm        s5             opass
         s6        s5             copy s5
         v1        s3!v0&vm       has word of 1 if any flag = 1
         a1        opass+8        offset(opass(9))
         s6        s6<8           position ppass
         a0        a1+a7          loc(opass(9))
         vm        s6             left 8 bits are ppassed
         s5        s5>56          move opass for store and test
         s6        s6>56          move ppass for store and test
         ,a0,a2    v1             store opass
         v2        s3!v0&vm       has word of 1 if ppass=1
         a1        ppass+8        offset(ppass(9))
         a0        a1+a7          loc(ppass(9))
         ,a0,a2    v2             store ppass
*a4=nppcs
*a5=nopcs
*s5=opassed
*s6=ppassed
         t55       s5             save opassed
         t56       s6             save ppassed
         b35       a4             save nppcs
         b36       a5             save nopcs
         a0        a4             nppcs
         s0        s5             opassed
         jan       skip7          
         jsn       l250           nppcs.eq.0 .and. opassed.ne.0
skip7    =         *
         a0        a5             nopcs
         s0        s6             ppassed
         jan       skip8          
         jsn       l250           nopcs.eq.0 .and. ppassed.eq.0
skip8    =         *
         s0        s5+s6          ppassed+opassed
         j         callsp3a       
l250     =         *              
         s1        s5+s6          
         t40       s1             
         r         SCOREP2        
         s0        t40            
callsp3a =         *              
         jsz       done1          
         r         SCOREP3        
done1    =         *              
         j         b63            
l300     =         *              a7=base,s6=pcalls
         s1        -1             
         s6        s6-s1          pcalls+1
         pflag,a7  s1             set pflag to -1
         pcalls,a7 s6             
         r         SCOREP1        
         a7        b60            base
         s6        ppassed,a7     get ppassed for add in l400
         s5        opassed,a7     get opassed for add in l400
         a0        nppcs,a7       nppcs
         a6        nopcs,a7       nopcs
         s0        s6             ppassed
         jan       skip9          
         jsn       l400           
skip9    =         *
         a0        a6             
         s0        s6             
         jan       skip10         
         jsn       l400           
skip10   =         *
         s0        s5+s6          ppassed+opassed
         j         callsp3b       
l400     =         *              a6=ply+base,s6=ppassed,s5=opassed
         s1        s6+s5          ppassed+opassed
         t40       s1             
         r         SCOREP2        
         s0        t40            
callsp3b =         *              
         jsz       done2          
         r         SCOREP3        
done2    =         *              
         j         b63            return
         include   'global' 
         end       
         ident     scorekm        
         entry     SCOREKM        
         ext       SCOREKM1       
         ext       SCOREKM2       
         align     
SCOREKM  =         *              
         a7        b60            task common base adr
         s1        ply,a7         
         a2        okingl,a7      
         a3        nppcs,a7       
         s6        >63            -2
         a4        15             
         s5        phash,a7       
         s2        ksizm1,        
         s6        s6&s1          
         a1        s6             even(ply)
         a1        a1+a7          
         a1        moveds-1,a1    moveds(even(ply))
         s3        random-1,a2    random(okingl,1)
         s4        random+200,    random(1,3)
         a0        a4-a3          16-nppcs
         a5        ktbl2,         
         s7        random+100,a1  random(moveds(even(ply))+1,2)
         s5        s5\s3          xor(ph,r(pk,1))
         s1        khashs,a7      
         a2        pscore,a7      
         s7        s5\s7          khash
         jam       skip11         if .gt.
         s7        s7\s4          khash=xor(kh,r(1,3))
skip11   =         *
         s2        s7&s2          and(kh,ks)
         a6        s2             
         a5        a6+a5          kkey1
         s2        htable-1,a5    htable(kkey1)
         s4        <15            32767
         s5        1              
         s1        s1+s5          khashs+1
         s5        <40            bmask
         khash,a7  s7             sav for scorek2
         s7        s7>24          now use left 40 bits
         kkey1,a7  a5             sav kkey1 for scorek2
         s6        s2&s5          and(ht(kk),bm)
         s0        s6\s7          eq.khash?
         s2        s2>48          
         a6        b0             sav b0
         b64       a6
         jsn       l100           
         s3        s2-s4          osafety
         s2        a3
         khashs,a7 s1             
         a4        s3             os
         s2        s2>1           shiftr(nppcs,1)
         jap       l50            if .le.
         a3        s2
         a4        a3*a4          ts*nppcs
l50      osafety,a7 s3            
         a2        a2-a4          pscore
         s2        +a2            extend pscore
         pscore,a7 s2             
         j         l200           
l100     =         *              
         r         SCOREKM1       
l200     =         *
         a7        b60
         s0        movedr+1,a7    movedr(2)
         jsz       l300
         r         SCOREKM2
l300     j         b64            return
         include   'global' 
         end       
         ident     scorekp        
         entry     SCOREKP        
         ext       SCOREKP1       
         ext       SCOREKP2       
         align     
SCOREKP  =         *              
         a7        b60            task common base adr
         s1        ply,a7         
         a2        pkingl,a7      
         a3        nopcs,a7       
         s6        1              
         s5        phash,a7       
         s1        s1-s6          ply-1
         s2        ksizm1,        
         s6        s6!s1          or(ply-1,1)
         a1        s6             odd(ply)
         a1        a1+a7          add in base adr
         a1        moveds-1,a1    moveds(odd(ply))
         a4        15             
         s3        random-1,a2    random(pkingl,1)
         s4        random+200,    random(1,3)
         a0        a4-a3          16-nopcs
         a5        ktbl1,         
         s7        random+100,a1  random(moveds(odd(ply))+1,2)
         s5        s5\s3          xor(ph,r(pk,1))
         s1        khashs,a7      
         a2        pscore,a7      
         s7        s5\s7          khash
         jam       skip12         if .gt.
         s7        s7\s4          khash=xor(kh,r(1,3))
skip12   =         *
         s2        s7&s2          and(kh,ks)
         a6        s2             
         a5        a6+a5          kkey1
         s2        htable-1,a5    htable(kkey1)
         s4        <15            32767
         s5        1              
         s1        s1+s5          khashs+1
         s5        <40            bmask
         khash,a7  s7             sav for scorek2
         s7        s7>24          now use left 40 bits
         kkey1,a7  a5             sav kkey1 for scorek2
         s6        s2&s5          and(ht(kk),bm)
         s0        s6\s7          .eq.khash?
         s2        s2>48          hashed score
         a6        b0             sav b0
         b64       a6             
         jsn       l100           
         s3        s2-s4          psafety
         s2        a3
         khashs,a7 s1             
         a4        s3             ps
         s2        s2>1           shiftr(nopcs,1)
         jap       l50            if .le.
         a3        s2
         a4        a3*a4          ts*nopcs
l50      psafety,a7 s3            
         a2        a2+a4          pscore
         s2        +a2            extend pscore
         pscore,a7 s2             
         j         l200           
l100     =         *              
         r         SCOREKP1       
l200     =         *
         a7        b60
         s0        movedr,a7      movedr(1)
         jsz       l300
         r         SCOREKP2
l300     j         b64            return
         include   'global' 
         end       
         include   'common'       
         ident     scoretrd         
         entry     SCORETRD
*
*           register utilization:    a0-a7 and s0-s7 are used
*
*
SCORETRD =         *
         a7        b60            task common base address
         a5        ply,a7
         s7        pscore,a7      pscore
         s2        tradepsc,a7    tradepsc
         a3        mscore,a7      mscore
         s5        mask,          binary 0101.....0101
         a6        cappc
         a0        a6+a7          loc(cappc(1))
         a5        a5-1           ply-1
         vl        a5             set vector length
         v7        ,a0,1          fetch cappc(1..ply-1)
*        s2=tradepsc, s7=pscore, a3=mscore, a7=base, v7=cappc(1..ply-1)
         s4        3000
         a4        2999
         s2        s2+s7          tradepsc=tradepsc+pscore
         s3        +a3            mscore
         s0        s3-s4          mscore-3000
         a0        a3+a4          mscore+2999
         a2        s2             tradepsc
         s6        50             tradeb=tradown
         jsp       trdone         if(mscore .ge. 3000)
         s4        101            tradpwin+1
         s0        s2-s4          tradepsc-tradpwin
         s6        -50            tradeb=-tradown
         jam       trdone         if(mscore .le. -3000)
         a4        100            tradpwin
         a0        a2+a4          tradepsc+tradpwin
         s6        50             tradeb=tradown
         jsp       trdone         if(tradepsc .gt. tradpwin)
         s6        -50            tradeb=-tradown
         jam       trdone         if(tradepsc .lt. -tradpwin)
         s6        0              tradeb=0
*        a7=base, s5=0101...0101, s6=tradeb, s7=pscore
*        v7=cappc(1..ply-1), vl=ply-1
trdone   =         *
         a0        a5-1
         v6        -v7            just in case
         s0        s6             tradeb
         jam       retrn          if(ply-2 .lt. 0)
         jsz       retrn          if(tradeb .eq. 0) return
         jsm       minus          if(tradeb .lt. 0)
         s5        s5<1           correct mask
         s1        1
         v1        s1+v7          cappc(ply)+1
         vm        v1,z           vm=1's for cappc(ply) .eq. -1
         s1        vm
         vm        v1,m           vm=1's for cappc(ply) .lt. -1
         s1        s1&s5          strip off even plies
         a1        ps1            count number of cappc(ply)=-1
         s2        vm
         s2        s2&s5          strip off even plies
         a2        ps2            count number of cappc(ply)<-1
         a3        a2-a1          + for pieces, - for pawns
         a4        s6             tradeb
         a4        a4*a3          total bonus/penalty
         s6        +a4            to s6
         s7        s7+s6          pscore=pscore+...
         pscore,a7 s7             save pscore
         j         b00            and return
minus    =         *
         s1        1
         v1        s1+v6          cappc(ply)+1
         vm        v1,z           vm=1's for cappc(ply) .eq. -1
         s1        vm
         vm        v1,m           vm=1's for cappc(ply) .lt. -1
         s1        s1&s5          strip off even plies
         a1        ps1            count number of cappc(ply)=-1
         s2        vm
         s2        s2&s5          strip off even plies
         a2        ps2            count number of cappc(ply)<-1
         a3        a2-a1          + for pieces, - for pawns
         a4        s6             tradeb
         a4        a4*a3          total bonus/penalty
         s6        +a4            to s6
         s7        s7+s6          pscore=pscore+...
         pscore,a7 s7             save pscore
retrn    =         *
         j         b00            and return
mask     con       o'525252525252525252525 
         include   'global'       
         end
