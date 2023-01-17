SHELL=		/bin/sh
LIBS=		-L/nwprod/lib -lbufr_4 -lw3_4 -lsp_4
FFLAGS=		-Q -O3 -qlist -qsource -qsuffix=f=f -qxlf77=leadzero \
   -bnoquiet

LDFLAGS=	$(FFLAGS)

SRCS=	datasort.f

OBJS=	datasort.o

FC=	xlf

CMD=	wave_datasort

all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

