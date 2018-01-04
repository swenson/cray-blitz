         ident     lock           
         entry     LOCK20         
         entry     LOCK21         
         entry     LOCK22         
         entry     LOCK23         
         entry     LOCK24         
         entry     LOCK25         
         entry     LOCK26         
         entry     LOCK27         
         entry     UNLOCK20       
         entry     UNLOCK21       
         entry     UNLOCK22       
         entry     UNLOCK23       
         entry     UNLOCK24       
         entry     UNLOCK25       
         entry     UNLOCK26       
         entry     UNLOCK27       
         align     
LOCK20   =         *              
         vwd       10/o'34,6/o'20 call lockon(20)
         j         b0             
LOCK21   =         *              
         vwd       10/o'34,6/o'21 call lockon(21)
         j         b0             
LOCK22   =         *              
         vwd       10/o'34,6/o'22 call lockon(22)
         j         b0             
LOCK23   =         *              
         vwd       10/o'34,6/o'23 call lockon(23)
         j         b0             
LOCK24   =         *              
         vwd       10/o'34,6/o'24 call lockon(24)
         j         b0             
LOCK25   =         *              
         vwd       10/o'34,6/o'25 call lockon(25)
         j         b0             
LOCK26   =         *              
         vwd       10/o'34,6/o'26 call lockon(26)
         j         b0             
LOCK27   =         *              
         vwd       10/o'34,6/o'27 call lockon(27)
         j         b0             
UNLOCK20 =         *              
         vwd       10/o'36,6/o'20 call lockoff(20)
         j         b0             
UNLOCK21 =         *              
         vwd       10/o'36,6/o'21 call lockoff(21)
         j         b0             
UNLOCK22 =         *              
         vwd       10/o'36,6/o'22 call lockoff(22)
         j         b0             
UNLOCK23 =         *              
         vwd       10/o'36,6/o'23 call lockoff(23)
         j         b0             
UNLOCK24 =         *              
         vwd       10/o'36,6/o'24 call lockoff(24)
         j         b0             
UNLOCK25 =         *              
         vwd       10/o'36,6/o'25 call lockoff(25)
         j         b0             
UNLOCK26 =         *              
         vwd       10/o'36,6/o'26 call lockoff(26)
         j         b0             
UNLOCK27 =         *              
         vwd       10/o'36,6/o'27 call lockoff(27)
         j         b0             
         end       
