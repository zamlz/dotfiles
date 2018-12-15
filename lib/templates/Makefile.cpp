# __  __       _         __ _ _
#|  \/  | __ _| | _____ / _(_) | ___
#| |\/| |/ _` | |/ / _ \ |_| | |/ _ \
#| |  | | (_| |   <  __/  _| | |  __/
#|_|  |_|\__,_|_|\_\___|_| |_|_|\___|
#

########################################################

CC       = g++ -g -O2 -Wall -Wextra -Wpedantic

README   = README
MKFILE   = Makefile

MAINFILE = main.cpp
OBJECTS  = ${MAINFILE:.cpp=.o}

SOURCES  = ${MAINFILE} ${README} ${MKFILE}

EXECBIN  = prog

########################################################

${EXECBIN}: ${HEADERS} ${OBJECTS}
	${CC} -o ${EXECBIN} ${OBJECTS}

%.o : %.c
	${CC} -c $<

clean:
	- rm ${OBJECTS}
	- rm vgcore.*

spotless: clean
	- rm ${EXECBIN}
