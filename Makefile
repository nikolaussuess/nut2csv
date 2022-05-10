# Choose any compiler you like ...
FF=gfortran
FFLAGS=-Wall -Wextra -g -static
OUTPUT_FILE=nut2csv
TEMPFILES=*.mod $(OUTPUT_FILE) f90getopt.o

nut2csv: f90getopt.o main.f90
	$(FF) $(FFLAGS) main.f90 f90getopt.o -o $(OUTPUT_FILE)

f90getopt.o: lib/f90getopt/f90getopt.F90
	$(FF) $(FFLAGS) -c lib/f90getopt/f90getopt.F90 -o f90getopt.o

clean:
	rm -f $(TEMPFILES)