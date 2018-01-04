         include   'common'       
         ident     killer1        
         entry     KILLER1        
         align     
KILLER1  =         *              
         a7        b60            
         a1        ply,a7         
         a2        player,a7      
         s0        mcappc,a7      
         a3        rdepth,a7      
         a5        52             
         a6        a5*a1          (,ply)
         a4        a1+a7          
         a2        a2+a7          
         s5        givchk-1,a4    
         s4        inchk-1,a4     
         jsn       rtn            
         a6        a6+a4          
         a0        a3-a1          
         s0        s5             
         s1        trace-53,a6    trace(ply,ply)
         jap       l90            
         a0        s4             
         jsn       l90            
         jan       l90            
rtn      =         *              
         j         b0             
*        s1=kmove,a4=ply+base
l90      =         *              
         s2        killmv-1,a4    killmv(,1)
         s3        killmv+99,a4   killmv(,3)
         s4        killmv+49,a4   killmv(,2)
         s5        killmv+149,a4  killmv(,4)
         s7        1              
         s0        s2\s1          
         s6        s3+s7          
         jsn       l100           
         killmv+99,a4 s6          
         s0        s6-s5          
         s3        s6             
         j         l200           
l100     =         *              
         s0        s4\s1          
         s6        s5+s7          
         jsn       l300           
         killmv+149,a4 s6         
         s0        s3-s6          
         s5        s6             
         j         l200           
l300     =         *              
         s5        1              
         s0        s3-s5          
         s4        s1             
         killmv+49,a4 s1          
         killmv+149,a4 s7         
*      s2=killmv(,1) s3=(killmv,3) s4=(killmv,2) s5=(killmv,4)
l200     =         *              
         jsp       l400           
         killmv-1,a4 s4           
         killmv+49,a4 s2          
         killmv+99,a4 s5          
         killmv+149,a4 s3         
l400     =         *              
         j         b0             
         include   'global'       
         end       
         
         ident     killer2        
         entry     KILLER2        
         align     
KILLER2  =         *              
         a7        b60            
         a6        taskid,a7      
         a1        ply,a7         
         a2        player,a7      
         s0        mcappc,a7      
         a3        rdepth,a7      
         a5        50             
         a6        a5*a6          
         a4        a1+a7          
         a2        a2+a7          
         s5        givchk-1,a4    
         s4        inchk-1,a4     
         a6        a6+a1          (ply,taskid)
         jsn       rtn            
         s1        curmvs-51,a6   
         a0        a3-a1          
         s0        s5             
         jap       l90            
         a0        s4             
         jsn       l90            
         jan       l90            
rtn      =         *              
         j         b0             
*        s1=kmove,a4=ply+base
l90      =         *              
         s2        killmv-1,a4    killmv(,1)
         s3        killmv+99,a4   killmv(,3)
         s4        killmv+49,a4   killmv(,2)
         s5        killmv+149,a4  killmv(,4)
         s7        1              
         s0        s2\s1          
         s6        s3+s7          
         jsn       l100           
         killmv+99,a4 s6          
         s0        s6-s5          
         s3        s6             
         j         l200           
l100     =         *              
         s0        s4\s1          
         s6        s5+s7          
         jsn       l300           
         killmv+149,a4 s6         
         s0        s3-s6          
         s5        s6             
         j         l200           
l300     =         *              
         s5        1              
         s0        s3-s5          
         s4        s1             
         killmv+49,a4 s1          
         killmv+149,a4 s7         
*      s2=killmv(,1) s3=(killmv,3) s4=(killmv,2) s5=(killmv,4)
l200     =         *              
         jsp       l400           
         killmv-1,a4 s4           
         killmv+49,a4 s2          
         killmv+99,a4 s5          
         killmv+149,a4 s3         
l400     =         *              
         j         b0             
         include   'global'       
         end       
