LATEX_FILES=cgore-utilities.latex string.latex

all: cgore-utilities.dvi cgore-utilities.ps cgore-utilities.pdf

cgore-utilities.dvi: ${LATEX_FILES}
	latex cgore-utilities.latex

cgore-utilities.ps: cgore-utilities.dvi
	dvips -o cgore-utilities.ps cgore-utilities.dvi

cgore-utilities.pdf: ${LATEX_FILES}
	pdflatex cgore-utilities.latex

clean:
	rm cgore-utilities.pdf cgore-utilities.ps cgore-utilities.dvi