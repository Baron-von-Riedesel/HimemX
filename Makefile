#
# HIMEMX.EXE is build with JWasm.
#

!ifndef DEBUG
DEBUG=0
!endif

NAME=HimemX
!if $(DEBUG)
OUTD=Debug
OPTD=-D_DEBUG
!else
OUTD=Release
OPTD=
!endif

ALL: $(OUTD)\$(NAME).exe $(OUTD)\$(NAME)2.exe

$(OUTD)\$(NAME).exe: $(NAME).asm Makefile
	@jwasm.exe -mz -nologo $(OPTD) -Sg -Fl$*.lst -Fo$*.exe $(NAME).asm

$(OUTD)\$(NAME)2.exe: $(NAME).asm Makefile
	@jwasm.exe -mz -nologo $(OPTD) -D?ALTSTRAT=1 -Sg -Fl$*.lst -Fo$*.exe $(NAME).asm

clean:
	erase $(OUTD)\*.exe
