CC=gcc
FC=gfortran

FLAGS=
#-g -fbounds-check

PROGS=gau2gau-in gau2gau-out
BINDIR=/path/to/bin

all: gau2gau-in gau2gau-out

gau2gau-in:gau2gau-in.f90
	$(FC) $(FLAGS) $^ -o $@
gau2gau-out:gau2gau-out.f90
	$(FC) $(FLAGS) $^ -o $@

install: $(PROGS) $(BINDIR)
	cp $(PROGS) $(BINDIR) -v

$(BINDIR):
	@echo ""
	@echo "To install the programs, please, specify the BINDIR as:"
	@echo ""
	@echo " make install BINDIR=/path/to/bin"
	@echo ""
	@echo "E.g."
	@echo " make install BINDIR=~/bin/"
	@echo ""
	@false

clean:
	rm $(PROGS) 
