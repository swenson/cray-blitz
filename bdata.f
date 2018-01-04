      block data
crev  last revision 03/02/91
c
c     ******************************************************************
c     *                                                                *
c     *      block data is used to initialize all common.              *
c     *                                                                *
c     ******************************************************************
c
      implicit integer (a-z)
c
c
      include 'global.f'
c
c
      include 'rcommon.f'
c
c
      data abort / 0 /
      data aio / 0 /
      data alloc / 16*0 /
      data alpha /'A','B','C','D','E','F','G','H','I','J','K','L',
     *'M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
     *            'a','b','c','d','e','f','g','h','i','j','k','l',
     *'m','n','o','p','q','r','s','t','u','v','w','x','y','z',
     *'0','1','2','3','4','5','6','7','8','9','+','-','/','*','=',
     *'.','?',' ',':',',','[',']','@','!',';',' ',' ',' '/
c     dpt qmk blk col com lbr rbr  at exp smc blk blk blk
      data autos / 9 /
      data begin / 25,  9,  5,  1,  1, 17 /
      data bell / '' /
      data block / 16*-1 /
      data board / 55*99, 0, 176*99 /
      data broke / 0 /
      data bdsave / 150*0 /
      data castkg / 1 /
      data castqn / 2 /
      data cbegin / 0 /
      data cbias / 0, 70 /
      data center /
     * 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     * 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     * 0,-3,-3,-3,-3,-3,-3,-3,-3, 0,
     * 0,-3, 2, 2, 2, 2, 2, 2,-3, 0,
     * 0,-3, 2, 3, 3, 3, 3, 2,-3, 0,
     * 0,-3, 2, 3, 4, 4, 3, 2,-3, 0,
     * 0,-3, 2, 3, 4, 4, 3, 2,-3, 0,
     * 0,-3, 2, 3, 3, 3, 3, 2,-3, 0,
     * 0,-3, 2, 2, 2, 2, 2, 2,-3, 0,
     * 0,-3,-3,-3,-3,-3,-3,-3,-3, 0 /
      data centern /
     * 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     * 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     * 0,-10, -6,-3,-3,-3,-3, -6,-10, 0,
     * 0, -6, -3, 2, 3, 3, 2, -3, -6, 0,
     * 0, -6,  2, 4, 4, 4, 4,  2, -6, 0,
     * 0, -3,  2, 4, 5, 5, 4,  2, -3, 0,
     * 0, -3,  2, 4, 5, 5, 4,  2, -3, 0,
     * 0, -6,  2, 4, 4, 4, 4,  2, -6, 0,
     * 0, -6, -3, 2, 3, 3, 2, -3, -6, 0,
     * 0,-10, -6,-3,-3,-3,-3, -6,-10, 0 /
      data cfiles / 0,
     * 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
     * 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
     * 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
     * 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
     * 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 /
      data color / 2 /
      data cputim / 1 /
      data cquery / 0 /
      data cranks / 0,
     * 10*0, 10*1, 10*2, 10*3, 10*4, 10*5, 10*6, 10*7, 10*8, 10*9 /
      data cvside / 1, -1 /
      data devdone / 0 /
      data dlevel / 800*0 /
      data dnodes, xnodes / 1500, 1500 /
      data drawsc / 0 /
      data d4 / 0 /
      data end   / 28, 16,  8,  4,  8, 24 /
      data enpass / 3 /
      data eval/ 0 /
      data evbnbd /
     * 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     * 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     * 0,620,560,500,440,380,320,260,240,0,
     * 0,560,520,460,400,340,280,230,260,0,
     * 0,500,460,320,280,260,220,280,320,0,
     * 0,440,400,280,200,200,260,340,380,0,
     * 0,380,340,260,200,200,280,400,440,0,
     * 0,320,280,220,260,280,320,460,500,0,
     * 0,260,230,280,340,400,460,520,560,0,
     * 0,240,260,320,380,440,500,560,620,0 /
      data even /
     * 0, 2, 2, 4, 4, 6, 6, 8, 8,10,
     *10,12,12,14,14,16,16,18,18,20,
     *20,22,22,24,24,26,26,28,28,30,
     *30,32,32,34,34,36,36,38,38,40,
     *40,42,42,44,44,46,46,48,48,50 /
      data e4 / 0 /
      data fdepth/ 0 /
      data from / 50*0 /
      data foundm / 0 /
      data ftime/ 0 /
      data givchk / 50*0 /
      data help / 16*0 /
      data idle / 0 /
      data inbook / 1 /
      data inchk / 50*0 /
      data khashs / 0 /
      data killmv / 200*0 /
      data lmoveo, lmovep / 0, 0 /
      data margin / 100 /
      data matchd / 0 /
      data matebd /
     * 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     * 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     * 0,500,480,460,440,440,460,480,500,0,
     * 0,480,400,380,360,360,380,400,480,0,
     * 0,460,380,300,280,280,300,380,460,0,
     * 0,440,360,280,200,200,280,360,440,0,
     * 0,440,360,280,200,200,280,360,440,0,
     * 0,460,380,300,280,280,300,380,460,0,
     * 0,480,400,380,360,360,380,400,480,0,
     * 0,500,480,460,440,440,460,480,500,0 /
      data matem/ 0 /
      data maxply / 49 /
      data minply / 2 /
      data movdir /  +10, -10, +1,  -1, +11,  +9, -11,  -9,
     *               +8, +12, +19, +21,  -8, -12, -19, -21,
     *               +1,  +9, +10, +11,  -1,  -9, -10, -11,
     *               +9, +11, +10, +20 /
      data mscore / 0 /
      data names /
     * 'a','b','c','d','e','f','g','h','i','j',
     * 'k','l','m','n','o','p','q','r','s','t',
     * 'u','v','w','x','y','z','27','28','29',
     * '30','31','32','33','34','35','36','37','38','39',
     * '40','41','42','43','44','45','46','47','48','49',
     * '50','51','52','53','54','55','56','57','58','59',
     * '60','61','62','63','64','65','66','67','68','69',
     * '70','71','72','73','74','75','76','77','78','79',
     * '80','81','82','83','84','85','86','87','88','89',
     * '90','91','92','93','94','95','96','97','98','99',
     * '100' /
      data nbusy / 16*0 /
      data nchar / 101 /
      data ncpus / 1 /
      data nodes / 0 /
      data nomovs, npmovs / 0, 0 /
      data normal / 0 /
      data numout / 0 /
      data ocount / 10*0  /
      data odbnbd /
     * 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     * 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     * 0,240,260,320,380,440,500,560,620,0,
     * 0,260,230,280,340,400,460,520,560,0,
     * 0,320,280,220,260,280,320,460,500,0,
     * 0,380,340,260,200,200,280,400,440,0,
     * 0,440,400,280,200,200,260,340,380,0,
     * 0,500,460,320,280,260,220,280,320,0,
     * 0,560,520,460,400,340,280,230,260,0,
     * 0,620,560,500,440,380,320,260,240,0 /
      data odd /
     * 1, 1, 3, 3, 5, 5, 7, 7, 9, 9,
     *11,11,13,13,15,15,17,17,19,19,
     *21,21,23,23,25,25,27,27,29,29,
     *31,31,33,33,35,35,37,37,39,39,
     *41,41,43,43,45,45,47,47,49,49 /
      data oelap / 0 /
      data ofirst / 10*0 /
      data olast / 10*100 /
      data oopost(1),  oopost(2),  oopost(3),  oopost(4)  / 0, 0, 0, 0 /
      data oopost(5),  oopost(6),  oopost(7),  oopost(8)  / 0, 0, 0, 0 /
      data oopost(9),  oopost(10), oopost(11), oopost(12) / 0, 0, 0, 0 /
      data oopost(13), oopost(14), oopost(15), oopost(16) / 0, 0, 0, 0 /
      data oopost(17), oopost(18), oopost(19), oopost(20) / 0, 0, 0, 0 /
      data oopost(21), oopost(22), oopost(23), oopost(24) / 0, 0, 0, 0 /
      data oopost(25), oopost(26), oopost(27), oopost(28) / 0, 0, 0, 0 /
      data oopost(29), oopost(30), oopost(31), oopost(32) / 0, 0, 0, 0 /
      data oopost(33), oopost(34), oopost(35), oopost(36) / 0, 0, 0, 0 /
      data oopost(37), oopost(38), oopost(39), oopost(40) / 0, 0, 0, 0 /
      data oopost(41), oopost(42), oopost(43), oopost(44) / 0, 0, 0, 1 /
      data oopost(45), oopost(46), oopost(47), oopost(48) / 1, 1, 1, 0 /
      data oopost(49), oopost(50), oopost(51), oopost(52) / 0, 0, 0, 0 /
      data oopost(53), oopost(54), oopost(55), oopost(56) / 0, 1, 1, 1 /
      data oopost(57), oopost(58), oopost(59), oopost(60) / 1, 0, 0, 0 /
      data oopost(61), oopost(62), oopost(63), oopost(64) / 0, 0, 0, 0 /
      data oopost(65), oopost(66), oopost(67), oopost(68) / 0, 0, 0, 0 /
      data oopost(69), oopost(70), oopost(71), oopost(72) / 0, 0, 0, 0 /
      data oopost(73), oopost(74), oopost(75), oopost(76) / 0, 0, 0, 0 /
      data oopost(77), oopost(78), oopost(79), oopost(80) / 0, 0, 0, 0 /
      data oopost(81), oopost(82), oopost(83), oopost(84) / 0, 0, 0, 0 /
      data oopost(85), oopost(86), oopost(87), oopost(88) / 0, 0, 0, 0 /
      data oopost(89), oopost(90), oopost(91), oopost(92) / 0, 0, 0, 0 /
      data oopost(93), oopost(94), oopost(95), oopost(96) / 0, 0, 0, 0 /
      data oopost(97), oopost(98), oopost(99), oopost(100)/ 0, 0, 0, 0 /
      data orate / 2000 /
      data over / 0 /
      data pcalls / 0 /
      data pcount / 10*0 /
      data pelap / 0 /
      data peval / 0 /
      data pfirst / 10*100 /
      data phashs / 0 /
      data piecem / 4*4, 4*3, 8*2, 8*6, 4*1 /
      data pieces / 4000000, 9000, 5000, 3250, 3250, 1000, 0,
     *                 1000, 3250, 3250, 5000, 9000, 4000000 /
      data plast / 10*0 /
      data ply / 1 /
      data pmove / 0 /
      data pndrng / 0 /
      data point / 0 /
      data popost(1),  popost(2),  popost(3),  popost(4)  / 0, 0, 0, 0 /
      data popost(5),  popost(6),  popost(7),  popost(8)  / 0, 0, 0, 0 /
      data popost(9),  popost(10), popost(11), popost(12) / 0, 0, 0, 0 /
      data popost(13), popost(14), popost(15), popost(16) / 0, 0, 0, 0 /
      data popost(17), popost(18), popost(19), popost(20) / 0, 0, 0, 0 /
      data popost(21), popost(22), popost(23), popost(24) / 0, 0, 0, 0 /
      data popost(25), popost(26), popost(27), popost(28) / 0, 0, 0, 0 /
      data popost(29), popost(30), popost(31), popost(32) / 0, 0, 0, 0 /
      data popost(33), popost(34), popost(35), popost(36) / 0, 0, 0, 0 /
      data popost(37), popost(38), popost(39), popost(40) / 0, 0, 0, 0 /
      data popost(41), popost(42), popost(43), popost(44) / 0, 0, 0, 0 /
      data popost(45), popost(46), popost(47), popost(48) / 0, 0, 0, 0 /
      data popost(49), popost(50), popost(51), popost(52) / 0, 0, 0, 0 /
      data popost(53), popost(54), popost(55), popost(56) / 0, 0, 0, 0 /
      data popost(57), popost(58), popost(59), popost(60) / 0, 0, 0, 0 /
      data popost(61), popost(62), popost(63), popost(64) / 0, 0, 0, 1 /
      data popost(65), popost(66), popost(67), popost(68) / 1, 1, 1, 0 /
      data popost(69), popost(70), popost(71), popost(72) / 0, 0, 0, 0 /
      data popost(73), popost(74), popost(75), popost(76) / 0, 1, 1, 1 /
      data popost(77), popost(78), popost(79), popost(80) / 1, 0, 0, 0 /
      data popost(81), popost(82), popost(83), popost(84) / 0, 0, 0, 0 /
      data popost(85), popost(86), popost(87), popost(88) / 0, 0, 0, 0 /
      data popost(89), popost(90), popost(91), popost(92) / 0, 0, 0, 0 /
      data popost(93), popost(94), popost(95), popost(96) / 0, 0, 0, 0 /
      data popost(97), popost(98), popost(99), popost(100)/ 0, 0, 0, 0 /
      data prate/ 2500 /
      data prcmnds / 50*' ' /
      data prevmv / 6*0 /
      data promot / 4 /
      data pscore / 0 /
      data realid / 1 /
      data rmoves, frmove / 0, 0, 0 /
      data rttime / 0 /
      data scolor /
     * 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
     * 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
     * 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
     * 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
     * 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1 /
      data shareit / 16*0 /
      data smode / 0 /
      data snodes / 0 /
      data splitl / 16*0 /
      data stop / 16*0 /
      data taskid / 1 /
      data tflag / 0 /
      data timlim / 30, 1800, 15, 1800, 5 /
      data tmode / 0 /
      data to / 50*0 /
      data tscore  / 0 /
      data unit / 5 /
      data usplit / 16*0 /
      data value(1) / 0 /
      data version / '49h ' /
      data vterm / 4000000 /
      data working / 16*0 /
      data xnullm / 1 /
      data xscore / 0 /
c     data ztime / 0 /
c
c------------------------------< scoring terms follow
c
      data atkfil / 0, 4, 4, 4, 4, 4, 4, 4, 4, 0 /
      data atkweakp / 50 /
      data bbonus / 250 /
      data bcenter / 6 /
      data bkingtrp / 3 /
      data blocker / 60, 30, 40, 50, 60, 0, 0,
     *                0, 60, 50, 40, 30, 60 /
      data bocdiv / 2 /
      data brpscore / 1100 /
      data develop / 10 /
      data iblocker / 60, 40, 30, 60, 30, 0, 0,
     *                 0, 30, 60, 30, 40, 60 /
      data isolat / 0, 25, 100, 300, 500, 700, 800, 900, 900 /
      data isolatof / 0, 75, 200, 300, 400, 500, 600, 700, 800 /
      data kaqmov / 50 /
      data kcenter / 8 /
      data khole / 3 /
      data kluft / 1 /
      data knocast / 7 /
      data knocastk / 5 /
      data koqmov / 30 /
      data kpawns / 3 /
      data ksafety / 0, 4, 4, 1, 1, 1, 1, 4, 4, 0 /
      data ncenter / 6 /
      data nkingtrp / 3 /
      data noutpost / 50 /
      data pblocked / 100 /
      data pdouble / 15 /
      data pen8p / 50 /
      data pfiles / 0, 2, 4, 6, 8, 8, 6, 4, 2, 0 /
      data pkclose / 1000 /
      data popose / 1000 /
      data poutside / 150 /
      data ppasspn / 20 /
      data ppawncon / 5 /
      data ppawnrnk / 5 /
      data pqueen / 7000 /
      data prams / 30 /
      data pranks / 0, 0, 0, 0, 0, 0, 0, 0, 0 /
      data ptriple  / 300 /
      data pwonkp  / 6000 /
      data qcenter / 6 /
      data qearly / 100 /
      data qkingtrp / 4 /
      data rbad / 50 /
      data rbehindp / 10 /
      data rhalfopn / 30 /
      data rkingtrp / 4 /
      data ron7th / 100 /
      data ropen / 100 /
      data rpmaydrw / 100 /
      data rtrapped / 100 /
      data tradown / 50 /
      data tradpwin / 100 /
      end
