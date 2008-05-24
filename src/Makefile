
OBJDIR= tmp_obj_dir
HSC= ghc
HSFLAGS= --make -odir $(OBJDIR) -hidir $(OBJDIR) -stubdir $(OBJDIR)

all:
	$(HSC) $(HSFLAGS) -o lipl Main

clean:
	rm -rf $(OBJDIR)/*
