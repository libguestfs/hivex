CROSS=i686-w64-mingw32-
CFLAGS=--std=c99
all: mkzero.exe
clean:
	rm -f *.exe *.o
mkzero.exe: mkzero.o
	$(CROSS)gcc -o $@ $< -lntdll
%.o: %.c
	$(CROSS)gcc $(CFLAGS) -Wpedantic -Wall -o $@ -c $<
