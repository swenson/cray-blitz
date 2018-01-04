#    configuration information:  CAL should point to the directory with the
#    CAL source (cray only).  FORTRAN should point to the FORTRAN source
#    directory.  C should point to the C source directory.

FORTRAN = .
C       = .

objects =    $(FORTRAN)/asyncio.o  $(FORTRAN)/attack.o   $(FORTRAN)/backup.o   \
             $(FORTRAN)/bdata.o    $(FORTRAN)/book.o     $(FORTRAN)/illegal.o  \
             $(FORTRAN)/castle.o   $(FORTRAN)/ccmnd.o                          \
             $(FORTRAN)/check.o    $(FORTRAN)/command.o  $(FORTRAN)/control.o  \
             $(FORTRAN)/copy.o     $(FORTRAN)/cptime.o   $(FORTRAN)/fail.o     \
             $(FORTRAN)/dcmnd.o    $(FORTRAN)/draw.o     $(FORTRAN)/driver.o   \
             $(FORTRAN)/dupchk.o   $(FORTRAN)/extrct.o   $(FORTRAN)/fcmnd.o    \
             $(FORTRAN)/forsythe.o $(FORTRAN)/hcmnd.o    $(FORTRAN)/boolean.o  \
             $(FORTRAN)/inform.o   $(FORTRAN)/input.o    $(FORTRAN)/iterate.o  \
             $(FORTRAN)/killer.o   $(FORTRAN)/locate.o   $(FORTRAN)/lookup.o   \
             $(FORTRAN)/main.o     $(FORTRAN)/make.o     $(FORTRAN)/match.o    \
             $(FORTRAN)/mater.o    $(FORTRAN)/merge.o    $(FORTRAN)/mover.o    \
             $(FORTRAN)/movgen.o   $(FORTRAN)/options.o  $(FORTRAN)/output.o   \
             $(FORTRAN)/pawn.o     $(FORTRAN)/pgcmnd.o   $(FORTRAN)/phase.o    \
             $(FORTRAN)/piece.o    $(FORTRAN)/playbw.o   $(FORTRAN)/pmvmkr.o   \
             $(FORTRAN)/pocmnd.o   $(FORTRAN)/ponder.o   $(FORTRAN)/prescore.o \
             $(FORTRAN)/query.o    $(FORTRAN)/rcmnd.o    $(FORTRAN)/reduce.o   \
             $(FORTRAN)/repeat.o   $(FORTRAN)/ripoff.o   $(FORTRAN)/saver.o    \
             $(FORTRAN)/sbcmnd.o   $(FORTRAN)/scan.o     $(FORTRAN)/setaio.o   \
             $(FORTRAN)/score.o    $(FORTRAN)/search.o   $(FORTRAN)/select.o   \
             $(FORTRAN)/setclk.o   $(FORTRAN)/setgb.o    $(FORTRAN)/setio.o    \
             $(FORTRAN)/setup.o    $(FORTRAN)/share.o    $(FORTRAN)/sortply1.o \
             $(FORTRAN)/split.o    $(FORTRAN)/srcmnd.o   $(FORTRAN)/stats.o    \
             $(FORTRAN)/store.o    $(FORTRAN)/test.o     $(FORTRAN)/typenode.o \
             $(FORTRAN)/umover.o   $(FORTRAN)/unmake.o   $(FORTRAN)/unsplit.o  \
             $(FORTRAN)/mplib.o

bpobjects =  $(FORTRAN)/attack.o   $(FORTRAN)/bdata.o    $(FORTRAN)/bookp.o    \
             $(FORTRAN)/castle.o   $(FORTRAN)/check.o    $(FORTRAN)/command.o  \
             $(FORTRAN)/copy.o     $(FORTRAN)/dcmnd.o    $(FORTRAN)/extrct.o   \
             $(FORTRAN)/input.o    $(FORTRAN)/locate.o   $(FORTRAN)/mplib.o    \
             $(FORTRAN)/make.o     $(FORTRAN)/movgen.o   $(FORTRAN)/pawn.o     \
             $(FORTRAN)/piece.o    $(FORTRAN)/prescore.o $(FORTRAN)/ripoff.o   \
             $(FORTRAN)/score.o    $(FORTRAN)/setgb.o    $(FORTRAN)/setup.o    \
             $(FORTRAN)/unmake.o   $(FORTRAN)/boolean.o

bsobjects =  $(FORTRAN)/attack.o   $(FORTRAN)/bdata.o    $(FORTRAN)/books.o    \
             $(FORTRAN)/castle.o   $(FORTRAN)/copy.o     $(FORTRAN)/extrct.o   \
             $(FORTRAN)/locate.o   $(FORTRAN)/mplib.o    $(FORTRAN)/make.o     \
             $(FORTRAN)/movgen.o   $(FORTRAN)/pawn.o     $(FORTRAN)/piece.o    \
             $(FORTRAN)/prescore.o $(FORTRAN)/ripoff.o   $(FORTRAN)/score.o    \
             $(FORTRAN)/setgb.o    $(FORTRAN)/setup.o    $(FORTRAN)/unmake.o   \
             $(FORTRAN)/boolean.o

blitz:	$(objects) Makefile
	ifort -i8 -o blitz $(objects)

bookp:	$(bpobjects)
	ifort -o bookp -i8 $(bpobjects)

books:	$(bsobjects)
	ifort -o books -i8 $(bsobjects)

$(objects): $(FORTRAN)/global.f $(FORTRAN)/common.f

.f.o:
	ifort -O -i8 -w -pipe -c $*.f
