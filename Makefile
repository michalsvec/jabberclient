#
# Project: jabberclient
# Author:  XXXX
#
# Usage:
#   - compile project:       make
#   - clean:                 make clean
#   - pack (zip):            make pack
#

# Compiler
HC = ghc
#HCFLAGS = -Wall -O2 -threaded --make
HCFLAGS = -Wall --make

# Output paths
OUTDIR = .
OUTPROJFILE = $(OUTDIR)/jabclient

# Source paths
SRCDIR = ./src
SRCFILES = $(SRCDIR)/gui/Main.hs
#	$(SRCDIR)/xmpp/*.hs

# Project compilation
all: $(SRCFILES)
	$(HC) $(HCFLAGS) $(SRCFILES) -o $(OUTPROJFILE)

# Cleaning
clean:
	rm -rf $(SRCDIR)/*.o $(SRCDIR)/*.hi
	rm -f $(OUTPROJFILE) $(OUTTESTFILE)

# Packing
pack:
	make clean
	zip -r xxx.zip $(SRCDIR) Makefile README

# End of file
