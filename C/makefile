server: dispatcher.o filelist.o stringlist.o workers.o
	gcc -Wall dispatcher.c filelist.c stringlist.c workers.c -o server -lpthread -lrt
dispatcher.o: dispatcher.c workers.h
filelist.o: filelist.c filelist.h
stringlist.o: stringlist.c stringlist.h
workers.o: workers.c workers.h filelist.h stringlist.h

clean:
	rm -f *.o server
