LATEX_FILES=sigma.latex copyright.latex introduction.latex behave-package.latex	\
	 bibliography.latex control-package.latex hash-package.latex		\
	 numeric-package.latex os-package.latex probability-package.latex	\
	 random-package.latex sequence-package.latex string-package.latex	\
	 time-series-package.latex truth-package.latex sigma-package.latex

all: sigma.dvi sigma.ps sigma.pdf

sigma.dvi: ${LATEX_FILES}
	latex sigma.latex

sigma.ps: sigma.dvi
	dvips -o sigma.ps sigma.dvi

sigma.pdf: ${LATEX_FILES}
	pdflatex sigma.latex

clean:
	rm sigma.pdf sigma.ps sigma.dvi
