OFLAGS	= 
INCLUDES = 

BINDIR = bin
MKDIRS = $(BINDIR)

VPATH = src

TARGET = $(BINDIR)/yukic
OBJS = yukic.cmo lexer.cmo parser.cmo

$(shell `mkdir -p $(MKDIRS)`)

all : $(TARGET)

%.cmo : %.ml
	ocamlc $(OFLAGS) $(INCLUDES) -c -o $@ $<

%.cmi : %.ml
	ocamlc $(OFLAGS) $(INCLUDES) -c -o $@ $<

%.cmi : %.mli
	ocamlc $(OFLAGS) $(INCLUDES) -c -o $@ $<

%.ml : %.mll
	ocamllex $(LEXFLAGS) $<

%.ml %.mli : %.mly
	ocamlyacc $(OYACCFLAGS) $<

$(TARGET) : $(OBJS)
	ocamlc $(OFLAGS) $(INCLUDES) -o $@ $^

yukic.cmo : parser.cmi lexer.cmi

lexer.cmo : parser.cmi

src/lexer.ml : lexer.mll

parser.cmo : ast.cmi parser.cmi

parser.cmi : src/parser.mli ast.cmi
	ocamlc $(OFLAGS) $(INCLUDES) -c -o $@ $<

src/parser.ml : parser.mly

.PHONY : clean all

clean :
	rm -r $(BINDIR) *.cmi *.cmo
