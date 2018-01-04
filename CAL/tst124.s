         ident     tst124         
         entry     TST124         
*  this routine's return depends on which cray hardware executes it
*           returns                machine type
*              1                   cray-1
*              2                   cray-2
*              3                   cray-3
*              4                   xmp without ymp extended addressing
*              5                   xmp with ext addr running in xmp mode
*              6                   xmp with ext addr running in ymp mode
*              7                   ymp running in xmp mode
*              8                   ymp running in ymp mode
*
         align     
TST124   =         *              
         a1        parceln-parcel1
         a2        3              c90 length of branch
         a0        a1-a2          check = 3
parcel1  =         *
         jan       notc90
parceln  =         *
         s1        90             90=c90
         j         b00            and return
notc90   =         *
         a0        0              
         vwd       8/o'21,24/not4 
         jan       xmp            
not4     =         *              
         a0        0              
         vwd       7/o'23,9/1     
         jan       ymp            
cray1    =         *              
         s1        1              
         j         b0             
ymp      =         *              
         s2        rt             
         vwd       48/o'2004000002600000 
         s3        rt             
         s3        s3-s2          
         s4        1              
         s4        s4<3           
         s1        s1-s3          
         s5        s1-s4          
         s5        s5+s5          
         s1        s5+s4          
         j         b0             
xmp      =         *              
         s1        1              
         s3        rt             
         a1        a1*a1          
         s1        s1<1           
         a1        0              
         s2        rt             
         s3        s3+s1          
         s1        s2-s3          
         s2        1              
         s2        s2<2           
         s0        s1-s2          
         s3        1              
         s2        s2+s3          
         a0        0              
         s2        s2<1           
         s3        rt             
         a0        1              
         jan       *+2            
         s4        rt             
         jsz       oldx           
         s5        s4-s3          
         s6        s5-s2          
         s6        s6+s6          
         s1        s1+s6          
oldx     =         *              
         j         b0             
         end       
         ident     setreg         
         entry     SETREG         
         align     
SETREG   =         *              
         a1        4660
*        b01       a1
*        b02       a1
*        b03       a1
*        b04       a1
*        b05       a1
*        b06       a1
*        b07       a1
*        b10       a1
*        b11       a1
*        b12       a1
*        b13       a1
*        b14       a1
*        b15       a1
*        b16       a1
*        b17       a1
*        b20       a1
*        b21       a1
*        b22       a1
*        b23       a1
*        b24       a1
*        b25       a1
*        b26       a1
*        b27       a1
*        b30       a1
*        b31       a1
*        b32       a1
*        b33       a1
*        b34       a1
*        b35       a1
*        b36       a1
*        b37       a1
         b40       a1
         b41       a1
         b42       a1
         b43       a1
         b44       a1
         b45       a1
         b46       a1
         b47       a1
         b50       a1
         b51       a1
         b52       a1
         b53       a1
         b54       a1
         b55       a1
         b56       a1
         b57       a1
         b60       a1
         b61       a1
         b62       a1
         b63       a1
         b64       a1
*        b65       a1
*        b66       a1
*        b67       a1
         b70       a1
         b71       a1
         b72       a1
         b73       a1
         b74       a1
         b75       a1
         b76       a1
         b77       a1
         j         b00
         end
         ident     dumpreg         
         entry     DUMPREG         
         align     
DUMPREG   =         *              
          a1        0
          b0       a1
          j        b0
          end
