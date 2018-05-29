# __  __       _         __ _ _      
#|  \/  | __ _| | _____ / _(_) | ___ 
#| |\/| |/ _` | |/ / _ \ |_| | |/ _ \
#| |  | | (_| |   <  __/  _| | |  __/
#|_|  |_|\__,_|_|\_\___|_| |_|_|\___|
#                                    

########################################################

CC       = gcc -g -O2 -Wall -Wextra -Wpedantic

README   = README
MKFILE   = Makefile

MAINFILE = main.c
SUPPFILE = 
SOURCES  = ${MAINFILE} ${SUPPFILE}

HEADERS  = ${SUPPFILE:.c=.h}
OBJECTS  = ${SOURCES:.c=.o}
EXECBIN  = runx

########################################################

# Build the humble HummusPlus Assembler
${EXECBIN}: ${HEADERS} ${OBJECTS} 
	${CC} -o ${EXECBIN} ${OBJECTS}

%.o : %.c
	${CC} -c $<

clean:
	- rm ${OBJECTS}
	- rm vgcore.*

spotless: clean
	- rm ${EXECBIN}                                               
