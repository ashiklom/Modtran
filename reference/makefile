# Makefile for mod3.e

FILES = dpdisort.f driver.server.f stdmdl.f d1mach.f dplinpak.f modtran3.1.f modtran3.2.f bmtran.f

OBJECTS = dpdisort.o driver.cgi.o stdmdl.o d1mach.o dplinpak.o modtran3.1.o  modtran3.2.o bmtran.o

#FFLAGS = -O2 -byteswapio -Msave
FFLAGS = -g -byteswapio -Msave
#FFLAGS = -n32 -Ofast -mips4 -r10000 -OPT:Olimit=2000 -static -bytereclen
#FFLAGS = -C -g -static -bytereclen
#
mod3.e:  $(FILES)
	pgf77 $(FFLAGS) $(FILES) -o mod3.e

.f.o:		makefile
		pgf77 $(FFLAGS) -c $(*).f



#
#
#For the HP use:  FFLAGS = -C -O -K  
#For the SGI OS 5 use:  FFLAGS = -C -O -static -bytereclen
#For the SGI OS 4 use:  FFLAGS = -C -O -static -old_rl
#For the SUN use:  FFLAGS = -C -O -Nl20
#
#--------------- Compile Notes ---------------------------------------
#
# Local variables need to be saved when compiling by using the machine 
# dependent save option. For the HP the correct switch is -K for the SGI it
# is -static. The SUN saves local variables by default. 
#
# Some of the Sun compilers require the switch -Nl20 to increase the number of 
# continuation lines from the default of 19 to 20. (Note: the switch is -N'lower case 
# L'20 not  -N'one'20). 
#
# The SGI flags '-bytereclen' and 'old_rl' affect the record length size for reading 
# and writing the direct access binary band model file DIRAC such that a record length of 
# 13000 can be for the SGI as well as for the HP and SUN. Compiling with the '-bytereclen' 
# or 'old_rl' flag avoids the necessity of editing bcddir.f and driver.f to change 
# the parameter 'RECL' from 13000 to 3250. The '-bytereclen' and 'old_rl' flags 
# accomplish the same compiling function. '-bytereclen' is a new flag probably introduced 
# with OS 5.0. Currently 'old_rl' will work through OS 5.2, but the manual pages for f77 
# say:
# 
#	     -old_rl   Same as -bytereclen.  Please use -bytereclen as -old_rl may be retired.
#
# The file bcddir.f which reads the ascii band model file BMTP.ASC and writes the binary 
# file DIRAC must also be compiled with the '-bytereclen' or 'old_rl' flag. The makefile 
# 'makefile-band-model' is provided for convenience with the appropriate flags listed.  
#
# Some machines do not have sufficient temporary disk space for compiling the 
# code. The Sun has a compile option that allows the temporary files to be put 
# into your directory.
#     		 -temp=dir  Set directory for temporary files to be dir.
#
# Another difficulty may be in sufficient allocation of swap space to compile the file 
# 'modtran3.f' as a whole. The compile error on SGI machines has been:
#		Binary write failed
#		write(263527468, 0x1006fb80, 16) err = 28
# which looks like it has nothing to do with space limitations. However dividing 
# 'modtran3.f' into smaller pieces solves the problem. The size of the pieces will 
# depend on the limitation of the specific computer system. The compile error on 
# the SUN machine is clearer:
#		error: out of memory
#		Compilation failed
#		*** Error code 71
#		make: Fatal error: Command failed for target `modtran3.o'
#
#
