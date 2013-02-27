all: cgore-utilities.dvi cgore-utilities.ps cgore-utilities.pdf

cgore-utilities.dvi: cgore-utilities.latex
	latex cgore-utilities.latex

cgore-utilities.ps: cgore-utilities.dvi
	dvips -o cgore-utilities.ps cgore-utilities.dvi

cgore-utilities.pdf: cgore-utilities.latex
	pdflatex cgore-utilities.latex

clean:
	rm cgore-utilities.pdf cgore-utilities.ps cgore-utilities.dvi