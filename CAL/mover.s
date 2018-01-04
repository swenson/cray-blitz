         include   'common'       
         ident     mover          
         entry     MOVER          
         ext       UMOVER         
         ext       MOVER1         
         ext       UMOVER1        
*
*     this routine makes normal moves or calls mover1 for unusual moves
*
         align     
MOVER    =         *              
         a1        b60            task common base adr
         a3        mfrom,a1       
         a7        b0             get return adr
         a0        mtype,a1       test for mtype=0
         a2        mto,a1         
         s6        6              king tester
         a4        ply,a1         
         a3        a3+a1          add in base adr
         s2        board-1,a3     board(mfrom)
         s7        0              
         a6        a2+a1          add in base adr for mto
         b40       a7             save return adr
         s1        board-1,a6     board(mto)
         jan       callu          
         board-1,a3 s7            board(mfrom)
         s3        s2+s6          check for - king
         board-1,a6 s2            board(mto)
         a4        a4+a1          add in base adr
         s0        s2-s6          is + king?
         a0        s3             is - king?
         cappc-1,a4 s1            
         jaz       kingm          
         jsz       kingp          
l120     j         b0             return
kingp    =         *              
         kloc,a1   a2             kloc of player 1
         j         b0             return
kingm    =         *              
         kloc+1,a1 a2             kloc of player 2
         j         b0             return
callu    =         *              
         r         MOVER1         process unusual moves
         j         b40            return
         include   'global'
         end
         ident     mover1         
         entry     MOVER1         
         
         align     
MOVER1   =         *              
         a7        b60            task common base adr
         a1        mtype,a7       
         a5        3              
         a2        player,a7      
         a3        ply,a7         
         a6        side,a7        
         a4        mto,a7         
         a0        a5-a1          3-mtype
         a5        mfrom,a7       
         s1        color,         
         jam       l5000          mtype.gt.3
         a3        a3+a7          
         a7        cbias-1,a2     bias
         jaz       l3000          mtype=3
         a6        a6+a6          2*side
         s2        0              
         a4        a6+a6          4*side
         a0        a1-1           
         s4        +a4            
         a6        a4+a6          6*side
         cappc-1,a3 s2            
         s0        s1<63          is color=1?
         s6        +a6            
         jan       l2000          
l1000    =         *              a2=player,s4=4*side,s6=6*side,a7=bias
         a4        b60            
         jsp       l3             color.ne.1
         a1        23             
         a1        a7+a1          bias+23
         a7        a7+a4          
         a2        a2+a4          
         board+21,a7 s2           
         board+22,a7 s6           
         board+23,a7 s4           
         board+24,a7 s2           
         kloc-1,a2 a1             
         j         b0             
l3       =         *              
         a1        28             
         a1        a7+a1          
         a7        a7+a4          
         a2        a2+a4          
         board+28,a7 s2           
         board+27,a7 s6           
         board+26,a7 s4           
         board+25,a7 s2           
         kloc-1,a2 a1             
         j         b0             
         bss       0              spacer
l2000    =         *              a2=player,s4=4*side,s6=6*side,a7=bias
         a3        b60            
         jsp       l5             color.ne.1
         a1        27             
         a1        a7+a1          
         a7        a7+a3          
         a2        a2+a3          
         board+28,a7 s2           
         board+26,a7 s6           
         board+25,a7 s4           
         board+24,a7 s2           
         kloc-1,a2 a1             
         j         b0             
l5       =         *              
         a1        24             
         a1        a7+a1          
         a7        a7+a3          
         a2        a2+a3          
         board+21,a7 s2           
         board+23,a7 s6           
         board+24,a7 s4           
         board+25,a7 s2           
         kloc-1,a2 a1             
         j         b0             
l3000    =         *              a3=ply,a4=mto,a5=mfrom,a6=side
         a2        b60            
         a1        10             
         a5        a5+a2          
         a1        a6*a1          10*side
         s1        board-1,a5     board(mfrom)
         s2        0              
         board-1,a5 s2            
         a6        -a6            -side
         a7        a4-a1          mt-10*side
         a4        a4+a2          
         a7        a7+a2          
         s3        +a6            extend sign
         board-1,a7 s2            
         cappc-1,a3 s3            
         board-1,a4 s1            
         j         b0             
         bss       0              startbuf
l5000    =         *              a1=mtype,mpropc=mtype-2
         a4        a4+a7          
         a3        a3+a7          
         a5        a5+a7          
         s1        board-1,a4     
         a2        a1*a6          mtype*side
         a7        a6+a6          2*side
         s2        0              
         board-1,a5 s2            
         a1        a2-a7          side*mpropc
         s3        +a1            extend sign
         cappc-1,a3 s1            
         board-1,a4 s3            
         j         b0             
         include   'global'       
         end       
