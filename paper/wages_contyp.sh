pdflatex -interaction batchmode wages_contyp.tex
echo "PDF1 =" $?

bibtex wages_contyp
echo "BIB1 =" $?

pdflatex -interaction batchmode wages_contyp.tex
echo "PDF2 =" $?

perl texcount.pl -1 -incbib -sum -total wages_contyp.tex > word_count.sum 

pdflatex -interaction batchmode wages_contyp.tex
echo "PDF3 =" $?

perl texcount.pl -inc -incbib -sum -total wages_contyp.tex

rm *.aux
rm *.bbl
rm *.blg
rm *.log
rm *.out



