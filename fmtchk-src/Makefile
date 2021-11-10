all:	fmtchk

libnndc.a: nndclib.f
	gfortran -c nndclib.f
	ar cr libnndc.a nndclib.o

fmtchk: libnndc.a fmtchk.f
	#gfortran -static fmtchk.f -L. -lnndc -o fmtchk
	gfortran fmtchk.f -L. -lnndc -o fmtchk

clean:
	rm -fv *.o
	rm -fv *.a
	rm -fv fmtchk
	rm -fv fmtchk.exe

