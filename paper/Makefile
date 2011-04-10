all: om.dvi

om.dvi: om.tex  paraiso.bib
	latex om.tex
	bibtex om
	latex om.tex
	latex om.tex


clean:
	rm -f *.aux *.bbl *.blg *.dvi *.log *~
