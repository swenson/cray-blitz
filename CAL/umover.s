         include   'common'       
         ident     umover
         entry     UMOVER
         ext       UMOVER1
*
*   this routine unmakes normal moves or calls umover1 for unusual moves
*
UMOVER   =         *              
         a1        b60            
         a2        mto,a1         
         a7        b0             get return adr
         a0        mtype,a1       test for mtype=0
         a4        ply,a1         
         a3        mfrom,a1       
         s6        -6             king tester
         a2        a1+a2          
         s1        board-1,a2     board(mto)
         a4        a4+a1          
         b40       a7             save return adr
         s2        cappc-1,a4     cappc(ply)
         jan       calluu         
         a6        a3+a1          
         board-1,a6 s1            board(mfrom)
         s3        s1\s6          check for - king
         s0        s1+s6          is + king?
         a0        s3             is - king?
         board-1,a2 s2            board(mto)
         jaz       ukingm         
         jsz       ukingp         
         j         b0             retuurn
ukingp   =         *              
         kloc,a1   a3             kloc of player 1
         j         b0             retuurn
ukingm   =         *              
         kloc+1,a1 a3             kloc of player 2
         j         b0             retuurn
calluu   =         *              
         r         UMOVER1        process unusual moves
         j         b40            return
         include   'global'       
         end       
         ident     umover1        
         entry     UMOVER1        
         align     
UMOVER1  =         *              
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
         a7        cbias-1,a2     bias
         a3        b60            
         jaz       l3000          mtype=3
         a6        a6+a6          2*side
         s2        0              
         a4        a6+a6          4*side
         a0        a1-1           
         s4        +a4            
         a6        a4+a6          6*side
         s0        s1<63          is color=1?
         s6        +a6            
         jan       l2000          
l1000    =         *              a2=player,s4=4*side,s6=6*side,a7=bias
         jsp       l3             color.ne.1
         a1        25             
         a1        a7+a1          bias+25
         a7        a7+a3          
         a2        a2+a3          
         board+21,a7 s4           
         board+22,a7 s2           
         board+23,a7 s2           
         board+24,a7 s6           
         kloc-1,a2 a1             
         j         b0             
l3       =         *              
         a1        26             
         a1        a7+a1          
         a7        a7+a3          
         a2        a2+a3          
         board+28,a7 s4           
         board+27,a7 s2           
         board+26,a7 s2           
         board+25,a7 s6           
         kloc-1,a2 a1             
         j         b0             
l2000    =         *              a2=player,s4=4*side,s6=6*side,a7=bias
         jsp       l5             color.ne.1
         a1        25             
         a1        a7+a1          
         a7        a7+a3          
         a2        a2+a3          
         board+28,a7 s4           
         board+26,a7 s2           
         board+25,a7 s2           
         board+24,a7 s6           
         kloc-1,a2 a1             
         j         b0             
l5       =         *              
         a1        26             
         a1        a7+a1          
         a7        a7+a3          
         a2        a2+a3          
         board+21,a7 s4           
         board+23,a7 s2           
         board+24,a7 s2           
         board+25,a7 s6           
         kloc-1,a2 a1             
         j         b0             
l3000    =         *              a3=ply,a4=mto,a5=mfrom,a6=side
         a1        10             
         a7        a4+a3          
         a1        a6*a1          10*side
         s1        board-1,a7     board(mto)
         s2        0              
         board-1,a7 s2            
         a6        -a6            -side
         a7        a4-a1          mt-10*side
         s3        +a6            extend sign
         a7        a7+a3          
         a5        a5+a3          
         board-1,a7 s3            
         board-1,a5 s1            
         j         b0             
l5000    =         *              a1=mtype,mpropc=mtype-2
         a3        a3+a7          
         a5        a5+a7          
         s3        cappc-1,a3     
         a4        a4+a7          
         s2        +a6            
         board-1,a5 s2            
         board-1,a4 s3            
         j         b0             
         include   'global'       
         end       
