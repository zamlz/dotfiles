
# __  __       _         __ _ _      
#|  \/  | __ _| | _____ / _(_) | ___ 
#| |\/| |/ _` | |/ / _ \ |_| | |/ _ \
#| |  | | (_| |   <  __/  _| | |  __/
#|_|  |_|\__,_|_|\_\___|_| |_|_|\___|
#                                    

###################################################

PDFEXE    = xelatex --shell-escape
BIBEXE    = bibtex
PDFTEST   = mupdf

TEXSRC    = cv.tex
BIBSRC    = bibliography.bib

OUTFILE   = ${TEXSRC:.tex=.pdf}
ENDFILE   = resume.pdf

MISCFILE  = ${TEXSRC:.tex=.aux} \
	    ${TEXSRC:.tex=.log} \
	    ${TEXSRC:.tex=.dvi} \
	    ${TEXSRC:.tex=.out} \
	    ${TEXSRC:.tex=.bbl} \
	    ${TEXSRC:.tex=.bcf} \
	    ${TEXSRC:.tex=.blg} \
	    ${TEXSRC:.tex=.toc} \
	    cv.run.xml \
	    texput.log

MAKEARGS  = --no-print-directory -C

###################################################

${OUTFILE}: ${TEXSRC} ${BIBSRC}
	-${PDFEXE} ${TEXSRC}
	-${BIBEXE} ${TEXSRC:.tex=.aux}
	-${PDFEXE} ${TEXSRC}
	-${PDFEXE} ${TEXSRC}
	mv ${OUTFILE} ${ENDFILE}

pdf: ${OUTFILE}
	-${PDFTEST} ${ENDFILE}


refresh: spotless pdf

clean:
	-rm -fv ${MISCFILE}
	-rm -rfv _minted*/

spotless: clean 
	-rm ${OUTFILE}
	-rm ${OUTFILE:.pdf=.dvi}
	-rm ${ENDFILE}
	-rm ${ENDFILE:.pdf=.dvi}

ci: spotless
	git add .
	git commit -e
