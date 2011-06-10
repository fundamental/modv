all:
	bison -d -v -t modv.y
	flex modv.lex
	g++ lex.yy.c modv.tab.c -o modv-comp -g

clean: 
	rm -f *~ modv-comp modv.tab* lex.yy.c modv.output *.o
