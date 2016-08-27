all:	dodgeball

dodgeball: 
	dasm dodgeball.asm -f3 -v0 -sdodgeball.sym -ldodgeball.lst -ododgeball.bin
	stella dodgeball.bin

clean:
	rm dodgeball.bin dodgeball.sym dodgeball.lst 
