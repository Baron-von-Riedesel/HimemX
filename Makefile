#
# HIMEMX.EXE can be build with JWasm.
#
NAME=HIMEMX

!if $(DEBUG)
OUTD=Debug
OPTD=-D_DEBUG
!else
OUTD=Release
OPTD=
!endif

AOPT=

$(OUTD)\$(NAME).EXE : $(OUTD)\$(NAME).asm
	@jwasm -c -mz -nologo $(OPTD) -Sg -Fl$(OUTD)\$(NAME).lst -Fo$(OUTD)\$(NAME).exe $(NAME).asm

clean:
	erase $(OUTD)\*.exe

