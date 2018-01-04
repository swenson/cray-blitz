         ident     setb60         
         entry     SETB60%        
*
*   this vfunction takes the address of tdata common and puts it in b60
*
SETB60%  =         *              
         a0        s1             
         b60       a0             
         j         b0             
         end       
         
