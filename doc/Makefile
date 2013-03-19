LATEX_FILES=sigma.latex string.latex

all: sigma.dvi sigma.ps sigma.pdf

sigma.dvi: ${LATEX_FILES}
	latex sigma.latex

sigma.ps: sigma.dvi
	dvips -o sigma.ps sigma.dvi

sigma.pdf: ${LATEX_FILES}
	pdflatex sigma.latex

clean:
	rm sigma.pdf sigma.ps sigma.dvi
