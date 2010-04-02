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
#HCFLAGS = -Wall -O2 -threaded --make -fglasgow-exts
HCFLAGS = -Wall -O2 -threaded -package qt --make -i./lib/XMPP-0.0.1 -odir $(BUILDDIR) -hidir $(BUILDDIR)

# Output paths
OUTDIR = .
BUILDDIR = $(OUTDIR)/build
OUTPROJFILE = $(OUTDIR)/jabclient


# Source paths
SRCDIR = ./src
#SRCFILES = ./test.hs
SRCFILES = $(SRCDIR)/gui/Main.hs \
	$(SRCDIR)/gui/Global.hs \
	$(SRCDIR)/xmpp/XMPPLight.hs \
	$(SRCDIR)/xmpp/XMPPXML.hs

# Project compilation
all: $(SRCFILES)
	$(HC) $(HCFLAGS) $(SRCFILES) -o $(OUTPROJFILE)

# Cleaning
clean:
	rm -rf $(BUILDDIR)/*.o $(BUILDDIR)/*.hi
	rm -f $(OUTPROJFILE)

# Packing
pack:
	make clean
	zip -r fpr-fun-xsrbpa00.zip $(SRCDIR) Makefile rozdeleni ./doc/* ./lib/*

# End of file
