all:  ice9

ice9:  lex.yy.c ice_9.tab.c ice_9.tab.h
	g++ lex.yy.c ice_9.tab.c -lfl -o ice9

ice_9.tab.c ice_9.tab.h: ice_9.y 
	bison -d ice_9.y
    
lex.yy.c: ice_9.l ice_9.tab.h
	flex ice_9.l

