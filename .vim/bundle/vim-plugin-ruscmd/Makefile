
all: dist

dist: 	clean
	find */ -type f -not -name tags > files.lst
	vi -c "%MkVimball! $$(basename $$(pwd)) ." -c 'q!' files.lst
	7z a $$(basename $$(pwd)).zip @files.lst
	rm -f files.lst

clean:
	rm -f $$(basename $$(pwd)).{vba,vmb,zip}
