report:
	rubber -d report.tex


clean:
	@rm -f *.blg *.log report.pdf *.toc *.aux *~ *.out *.bbl *.lof
