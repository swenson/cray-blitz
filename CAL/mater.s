         include   'common'       
         ident     mater          
         entry     MATER          
         
*      this is a vectorized version of mater.
         
         align     
MATER    =         *              
         a7        b60            task common base adr
         s7        side,a7
         s6        rdepth,a7
         s5        ply,a7
         a6        32
         vl        a6
         v4        0              set v4=0
         s4        4              promote to knight or better
         s0        s7             side
         v4        s4+v4          v4=4
         a1        2              start1=2
         jsp       startok        if(side .gt. 0)
         a1        3              start1=3
startok  =         *
         a5        inchk-1        loc(inchk(0))
         s2        3
         s1        s6+s2          rdepth+1 (3 to correct vl)
         a5        a5+a7          add in base
         s2        +a1            start1
         s1        s1-s2          -start1
         s0        s1             must be > 0
         s1        s1>1           (rdepth+1)/2
         a0        a5+a1          +start1
         s0        s1             must be > 0
         a2        s1             vl
         vl        a2             set vector length
         a4        2              set stride
         jsz       retrn          if vl = 0, exit
         v7        ,a0,a4         fetch inchk(i),i=start1,rdepth+1,2
*        s5=ply, s6=rdepth, s7=side, a5=loc(inchk(0)+base)
         s0        s7             side
         s2        2
         jsm       s1minus        if(side .lt. 0)
         s1        s6+s2          rdepth+2
         s3        1
         s2        >63            111111..11110
         s1        s1&s2          and(rdepth+2,62)
         s2        s3+s6          rdepth+1
         s2        s2!s3          or(rdepth+1,1)
         j         s1plus         and finish up
s1minus  =         *
         s2        1
         s1        s2+s6          rdepth+1
         s1        s1!s2          or(rdepth+1,1)
         s2        2
         s2        s6+s2          rdepth+2
         s3        >63            111111..11110
         s2        s2&s3          and(rdepth+2,62)
s1plus   =         *
*        s1=start2, s2=start3, s5=ply, a5=loc(inchk(0)+base)
         s4        1
         s3        s5+s4          ply-1 (+2 to fudge for correct vl)
         s3        s3-s1          vl*2
         s3        s3>1           vl
         a1        s3 
         a0        a1-1           to test for vl <= 0
         vm        v7,n           set vm where inchk(ply).ne.0
         jam       skip           no tests necessary, vl <= 0
         vl        a1
         a2        s1             start2
         a0        a2+a5          loc(inchk(start2))
         a1        2              stride
         v6        ,a0,a1         fetch inchk(i),i=start2,ply-1,2
         s7        vm             get vector mask
         a1        ps7            bcheck
         a2        3
         a0        a1-a2          test bcheck <= 2
*        s2=start3, s5=ply, a1=bcheck, v6=inchk(i),i=start2,ply-1
         vm        v6,z           set vm where inchk(ply).eq.0
         s7        vm
         jam       bcheckok       if(bcheck .le. 2)
         a1        2              bcheck=min(bcheck,2)
bcheckok =         *
         a0        a1             test bcheck
         jaz       retrn          if(bcheck .eq. 0)
         a0        ps7            if any non-zero, exit
         s7        0              givchk(ply)=0
         jan       retrn          all moves not checks, exit
         s1        s5             ply-2 (really ply which adjusts VL correctly)
         s1        s1-s2          (ply-2)-start3
         s1        s1>1           compute vl
         s0        s1             test vl
         s7        1
         a2        s1             save it
         b10       a2             save # of quiescence checks
         vl        a2             set vl
         jsz       checkim        if(vl .eq. 0) no moves to test
         a3        s2             start3
         a4        cappc-1        loc(cappc(0))
         a4        a4+a7          +base
         a5        type-1         loc(type(0))
         a5        a5+a7          +base
         a0        a4+a3          +start3
         a2        2              stride
         a6        onerep         loc(onerep(1))
         a6        a6+a7          +base
         a5        a5+a3          +start3
         a6        a6+a3          +start3
         v1        ,a0,a2         fetch cappc(i),i=start3,ply-2
         a0        a5             address
         v2        ,a0,a2         fetch type(i),i=start3,ply-2
         a0        a6             address
         v5        v2-v4          type(i)-4
         v3        ,a0,a2         fetch onerep(i+1),i=start3,ply-2
         vm        v1,n           set vm for any capture
         s1        vm             1's for any capture
         vm        v5,p           set vm for any promotions
         s2        vm             1's for any promotion
         vm        v3,n           set vm for any one-legal-reply checks
         s3        vm             1's for any one-legal-reply checks
*        s1=capture bits, s2=promotion bits, s3=one-legal-reply bits
         a2        ps3            onecheck
         s4        -1             1111..1111
         s3        s3\s4          .not. onecheck bits
         s1        s1&s3          don't count one-legal-reply captures
         s2        s2&s3          ditto for promotions
         s4        s1!s2          determine capture or promotion bits
         a3        ps4            ccheck
         a4        b10            total checks
         a4        a4-a2          minue one-legal-reply checks
         a4        a4-a3          minus cchecks = qchecks
*        a1=bcheck, a2=onecheck, a3=ccheck, a4=qcheck
         s1        +a1
         s2        +a2
         s3        +a3
         s4        +a4
         s3        s3>1           shiftr(ccheck,1)
         s2        s2>2           shiftr(onecheck,2)
         s2        s2+s3          shiftr(ccheck,1)+shiftr(onecheck,2)
         s2        s2+s4          +qcheck
         s0        s2-s1          qcheck-bcheck
         s7        1              givchk(ply)=1?
         jsm       checkim        if(qcheck .lt. bcheck)
         j         b0             return, no checks to be included
checkim  =         *
         a1        s5             ply
         a1        a1+a7          ply+base
         givchk-1,a1 s7           save givchk(ply)
retrn    =         *
         j         b0             
skip     =         *
         s7        vm             get vector mask
         a0        ps7            bcheck
         jaz       retrn          if(bcheck .eq. 0) return
         s7        1              givchk(ply)=1
         j         checkim        and store it
         include   'global'       
         end       
