all:	dodgeball

dodgeball: 
	dasm dodgeball.asm -f3 -v0 -sdodgeball.sym -ldodgeball.lst -ododgeball.bin
	stella dodgeball.bin

burn:
	/cygdrive/c/Program\ Files\ \(x86\)/HarmonyCart/HarmonyCart.exe dodgeball.bin

scratchpad:
	dasm scratchpad.asm -f3 -v0 -sscratchpad.sym -lscratchpad.lst -oscratchpad.bin
	stella scratchpad.bin

clean:
	rm dodgeball.bin dodgeball.sym dodgeball.lst
	rm scratchpad.bin scratchpad.sym scratchpad.lst
