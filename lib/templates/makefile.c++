# __  __       _         __ _ _      
#|  \/  | __ _| | _____ / _(_) | ___ 
#| |\/| |/ _` | |/ / _ \ |_| | |/ _ \
#| |  | | (_| |   <  __/  _| | |  __/
#|_|  |_|\__,_|_|\_\___|_| |_|_|\___|
#                                    

# - Amlesh Sivanantham
# - 2902698653
# - sivanant@usc.edu

# - Homework Assignment 1 for CSCI-561 - Foundations of A.I.
# - Makefile - Makefile for homework11.cpp

########################################################

CC       = g++ -g -O2 -Wall -Wextra -Wpedantic

README   = README
MKFILE   = Makefile

MAINFILE = homework11.cpp
OBJECTS  = ${MAINFILE:.cpp=.o}

SOURCES  = ${MAINFILE} ${README} ${MKFILE}

EXECBIN  = hw

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
