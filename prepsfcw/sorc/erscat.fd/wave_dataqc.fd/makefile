SHELL=		/bin/sh
LIBS=		-L/nwprod/lib -lw3_4 -lsp_4 -lbacio_4
FFLAGS=		-Q -O3 -qlist -qsource -qsuffix=f=f -qxlf77=leadzero \
   -bnoquiet

LDFLAGS=	$(FFLAGS)

SRCS=	dataqc.f

OBJS=	dataqc.o

FC=	xlf

CMD=	wave_dataqc

all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

