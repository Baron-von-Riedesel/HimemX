;******************************************************************************
;       H i m e m X                                
;******************************************************************************
; derived from FreeDOS Himem.
;
; original work by Till Gerken.
; major rework by tom ehlert.
; modified for >64M support, Michael Devore
;
; Comments and bug reports are always appreciated.
;
; Parts copyright (c) 1995, Till Gerken
;******************************************************************************
; -- NEW IMPLEMENTATION NOTES --
;
; modified for >64M support, Michael Devore, Nov 2003 - Apr 2004
;  added support for API functions 88h, 89h, 8eh, and 8fh
;  fixed various bugs
;  added /NOABOVE16 support to match Microsoft's HIMEM.SYS
;  added /X support to match Microsoft's HIMEM.SYS
;  Michael Devore's changes are not copyrighted and are released to the public
;   domain. This does not affect copyright on the rest of the code.
;
; - major rework done by tom ehlert, fixed
;
; - reported XMS version  reflects current implementation - 2.0
; - reported XMS internal version reflects driver version - 0.5
;
; - xms_free_handle now actually works
; - added support for protected mode operation
; - many more checks for valid requests (like valid handles,valid offsets)
; - major code cleaning
; - has chances to work in multitasking environments (not tested)
;
; - although the code is based on Till gerkens, much has been changed.
;   so the bugs are no longer due to Till Gerkens, but due to
;   tom.ehlert (tom.ehlert@ginko.de)
;
; - Changes 2004/24/9 (Aitor SANTAMARIA MERINO)
;   Minor changes to commandline (HMAMIN, TESTMEM, /?, suffixed values),
;   Rebuild the package with the HIMEM64, the HELP file, history, etc
;   Test less verbose for emulating MS-HIMEM
;   New test_a20 proc based on Undocumented PC (saves 12 bytes)
;
; still missing
;    support for 80286 (mostly due to long arith with 32 bit registers)
;      (no such support is planned)
;
;******************************************************************************
; japheth 2007 (all changes Public Domain): 
;       - bugs fixed (see History.txt for details)
;       - source changed from IDEAL to MASM format
;       - optimizations to reduce size of resident part
;       - use "unreal" mode for EMB moves
;       - most functions reeentrant now (A20 functions still are not)
;       - Interrupt window implemented for real-mode EMB move
;       - C part abandoned to simplify the make process and reduce binary size
;       - test A20 changed (no more memory writes)
; japheth 2020:
;       - v3.34: added multiple int 15h, ax=e820h memory block support
;       - v3.35: added alternative (re)alloc strategy (HimemX2).
;                see Readme.txt & History.txt for details.
;       - v3.36: signed comparison in block move made moves >= 2 GB buggy;
;                minor bug in HimemX, since it's very unlikely that such
;                large moves occured, but needed for HimemSX.

;--- assembly time parameters

VERSIONSTR		equ <'3.37'>
DRIVER_VER		equ 300h+37
INTERFACE_VER	equ 300h

ifndef NUMHANDLES
NUMHANDLES      equ 48      ;std 48, default number of handles
endif
MAXHANDLES      equ 128     ;std 128, max number of handles
ALLOWDISABLEA20 equ 1       ;std 1, 1=allow to disable A20
BLOCKSIZE       equ 2000h   ;std 2000h, block size moved to/from ext. memory
USEUNREAL       equ 1       ;std 1, 1=use "unreal" mode for EMB copy
PREF66LGDT      equ 0       ;std 0, 1=use 66h prefix for LGDT
?LOG            equ 0       ;std 0, 1=enable /LOG option
?TESTMEM        equ 0       ;std 0, 1=enable /TESTMEM:ON|OFF option
ifndef ?ALTSTRAT
?ALTSTRAT       equ 0       ;std 0, 1=use alternate strategie for (re)alloc emb
endif
?MERGE0HDL      equ 1       ;std 0, 1=try to merge even if handle to free has size 0
?ALLOCDX0       equ 1       ;std 1, 1=return DX=0 if alloc fails

;MAXFREEKB      equ 0FBC0h
MAXFREEKB       equ 0FFFFh  ;std FFFFh, xms v2.0 max ext. memory

;--- constants

XMS_START       equ 1024+64 ; XMS starts at 1088k after HMA

CMD_INIT        equ 0       ; init command (used when installed)

;--- DOS device driver status codes used

STATUS_OK       equ 0100h   ; driver is initialized and ok
STATUS_BAD      equ 8103h   ; driver couldn't be installed

VDISK_IDSTR     equ "VDISK"
VDISK_IDLEN     equ 5
VDISK_IDOFS     equ 13h

;--- A20 switch methods (must match order in "methods" table)

A20_KBC     equ 0
A20_PS2     equ 1
A20_BIOS    equ 2
A20_ALWAYSON equ 3
A20_FAST    equ 4
A20_PORT92  equ 5

;--- XMS error codes

XMS_NOT_IMPLEMENTED             equ 80h
XMS_VDISK_DETECTED              equ 81h
XMS_A20_FAILURE                 equ 82h
XMS_DRIVER_FAILURE              equ 8eh
XMS_DRIVER_FATAL                equ 8fh
XMS_HMA_NOT_THERE               equ 90h
XMS_HMA_IN_USE                  equ 91h
XMS_HMAREQ_TOO_SMALL            equ 92h
XMS_HMA_NOT_USED                equ 93h
XMS_A20_STILL_ENABLED           equ 94h
XMS_ALL_MEM_ALLOCATED           equ 0a0h
XMS_NO_HANDLE_LEFT              equ 0a1h
XMS_INVALID_HANDLE              equ 0a2h
XMS_INVALID_SOURCE_HANDLE       equ 0a3h
XMS_INVALID_SOURCE_OFFSET       equ 0a4h
XMS_INVALID_DESTINATION_HANDLE  equ 0a5h
XMS_INVALID_DESTINATION_OFFSET  equ 0a6h
XMS_INVALID_LENGTH              equ 0a7h
XMS_INVALID_OVERLAP             equ 0a8h
XMS_PARITY_ERROR                equ 0a9h
XMS_BLOCK_NOT_LOCKED            equ 0aah
XMS_BLOCK_LOCKED                equ 0abh
XMS_LOCK_COUNT_OVERFLOW         equ 0ach
XMS_LOCK_FAILED                 equ 0adh
XMS_ONLY_SMALLER_UMB            equ 0b0h
XMS_NO_UMB_AVAILABLE            equ 0b1h
XMS_UMB_SEGMENT_NR_INVALID      equ 0b2h

	option proc:private
	option casemap:none

@byte	equ <byte ptr>
@word	equ <word ptr>
@dword	equ <dword ptr>

@DbgOutS macro string
ifdef _DEBUG
	call printstring
	db string
	db 0
endif
    endm

@display macro value
	echo value
	endm

;--- structures

;--- cpu GDT descriptor

desc struct
  limit     dw  ?       ; segment limit
  base00_15 dw  ?       ; low word of base address
  base16_23 db  ?       ; high byte of base address
            db  ?       ; 93h = std ring 0 read/write segment
  attr      db  ?       ; attributes, limit 16-19
  base24_31 db  ?
desc ends

;--- DOS device driver request header

request_hdr struct
  req_size  db  ?       ; number of bytes stored
  unit_id   db  ?       ; unit ID code
  cmd       db  ?       ; command code
  status    dw  ?       ; status word
  rsvd      db  8 dup (?)   ; reserved
request_hdr ends

;--- DOS device driver request for INIT

init_strc struct
  init_hdr  db  size request_hdr dup (?)
  units     db  ?       ; number of supported units
  end_addr  dd  ?       ; end address of resident part
  cmd_line  dd  ?       ; address of command line
init_strc ends

;--- XMS_HANDLE is a documented structure, size 10 bytes

XMS_HANDLE struct
xh_flags     db  ?       ; see below
xh_locks     db  ?       ; lock count
xh_baseK     dd  ?       ; base address in kbytes
xh_sizeK     dd  ?       ; size in kbytes
XMS_HANDLE ends

;--- defined values for XMS_HANDLE.flags

XMSF_FREE	equ 1	;describes a free block
XMSF_USED	equ 2	;describes a used block
XMSF_INPOOL	equ 4	;describes a free handle

;--- XMS handle table (documented structure, size 8 bytes)

XMS_HANDLETABLE struct
xht_sig			DB ?	; identifier byte?, 1 for MS-DOS HIMEM, 0FDH for us
xht_sizeof		DB ?	; size of handle descriptor (xms_handle)
xht_numhandles	DW ?	; number of handles
xht_pArray		DD ?	; pointer to XMS handles array
XMS_HANDLETABLE ends

;--- structure for XMS AH=0Bh (documented structure)

xms_move struct
  len           dd  ?       ; block length in bytes
  src_handle    dw  ?       ; source handle
  src_offset    dd  ?       ; offset into source
  dest_handle   dw  ?       ; destination handle
  dest_offset   dd  ?       ; offset into destination
xms_move ends

;--- int 15h, ax=E820h (documented)

SMAP equ 534d4150h

E820MAP struct
baselow dd  ?
basehigh dd  ?
lenlow  dd  ?
lenhigh dd  ?
type_   dd  ?
E820MAP ends

IRETS struct
wIP		dw ?
wCS		dw ?
bFlags  db ?
bFlags2	db ?
IRETS ends

;--- segment definitions

	.SEQ	;place segments in the order defined here

_RTEXT segment dword public 'CODE'	; resident code+data
_RTEXT ends

_TEXT segment byte public 'CODE'	; nonresident code
_TEXT ends

_DATA segment word public 'DATA'	; nonresident data
_DATA ends

_STACK  segment STACK 'STACK'		;1 kB stack 
		db 1024 dup(?)
_stacktop label byte
_STACK  ends

;--- use the TINY model

DGROUP  group   _RTEXT,_TEXT,_DATA,_STACK

	.386P            ; 386 instructions + privileged opcodes

;--- nonresident data

_DATA segment

;--- variables

request_ptr				DD 0       ; pointer to request header
xms_mem_free			DD 0       ; size of XMS in kbytes
_xms_max				DD 4095*1024 ; value /MAX= parameter
_xms_num_handles		DW NUMHANDLES ;value /NUMHANDLES= parameter
_method					DB -1      ; value /METHOD: parameter
_no_above_16			DB 00H     ; value /NOABOVE16 parameter
_x_option				DB 00H     ; value /X parameter
_startup_verbose		DB 00H     ; value /VERBOSE parameter
if ?LOG
_xms_logging_enabled	DB 00H     ; value /LOG parameter
endif
hma_exists				db 0

;--- constants

szStartup   	DB 'HimemX ', VERSIONSTR, ' [', @CatStr(!"%@Date!"), '] (c) 1995 Till Gerken, 2001-2006 tom ehlert',  0aH,  00H
if ?TESTMEM
szTESTMEMOFF	DB '/TESTMEM:OFF',  00H
endif
szVERBOSE		DB '/VERBOSE',  00H
if ?LOG
szLOG			DB '/LOG',  00H
endif
szINTERFACE		DB 'Interface : XMS 3.00 80386 4G ',  0aH,  00H
szNUMHANDLES	DB '/NUMHANDLES=',  00H
szSelNumHandles	DB 'selected num_handles=%d',  0aH,  00H
szNumHandlesLim1	DB 'HimemX: NUMHANDLES must be >= 8, corrected',  0aH,  00H
szNumHandlesLim2	DB 'HimemX: NUMHANDLES must be <= ',@CatStr(!",%MAXHANDLES,!"),', corrected',  0aH,  00H
szNOABOVE16		DB '/NOABOVE16',  00H
szX2MAX32		DB '/X2MAX32',  00H
szX				DB '/X',  00H
szMETHOD		DB '/METHOD:',  00H
szMAX			DB '/MAX=',  00H
szMaximum		DB 'Maximum XMS: %luK',  0aH,  00H
szHMAMIN		DB '/HMAMIN=',  00H
szMinimum		DB 'Minimum HMA that has to be requested: %uK',  0aH,  00H
szHMAMAX		DB 'HimemX: HMAMIN must be <= 63, corrected',  0aH,  00H
szignored		DB 'ignored commandline <%s>',  0aH,  00H
;cant_disable_message db 'Can',27h,'t disable A20 - ignored',0dh,0ah,'$'

dHimem			db 'HimemX: $'

;-- method feedback text

szKBC			db "KBC",'$'
szPS2			db "PS/2",'$'
szFast	  		db "Fast",'$'
szBIOS  		db "BIOS",'$'
szPort92 		db "Port 92",'$'
szA20			db " A20 method used",13,10,'$'
szAlwaysOn		db 'Always on','$'
MsgUnknownA20	db 'No Supported A20 method detected',0dh,0ah,'$'

old_dos 		db 'XMS needs at least DOS version 3.00.$'
xms_twice		db 'XMS is already installed.$'
vdisk_detected	db 'VDISK has been detected.$'
no_386			db 'At least a 80386 is required.$'
a20_error		db 'Unable to switch A20 address line.$'
;xms_sizeerr 	db 'Unable to determine size of extended memory.$'
xms_toosmall	db 'Extended memory is too small or not available.$'
error_msg		db 'Driver won''t be installed.',7,13,10,'$'

methods label byte
	db 3,"kbc"       ;0 (A20_KBC)
	db 3,"ps2"       ;1 (A20_PS2)
	db 4,"bios"      ;2
	db 8,"alwayson"  ;3
	db 4,"fast"      ;4
	db 6,"port92"    ;5
	db 0

if ?TESTMEM
?TMSTR equ <" [/TESTMEM:ON|OFF]">
else
?TMSTR equ <" ">
endif
if ?LOG
?LGSTR equ <" [/LOG]">
else
?LGSTR equ <" ">
endif

szHello label byte
	db "Extended memory host for DOS (coordinates the usage of XMS and HMA)",10
	db "HimemX is a device driver that is loaded in CONFIG.SYS.",10
	db "Please place DEVICE=HIMEMX.EXE [options] before any driver using XMS.",10,10
	db "options: [/MAX=####] [/METHOD:xxx] [/HMAMIN=n] [/NUMHANDLES=m]",10
	db ?TMSTR," [/VERBOSE] [/NOABOVE16] [/X] [/X2MAX32]",?LGSTR,10,10
	db "  /MAX=#####      limit memory controlled by XMM to #####K.",10
	db "                  The HMA is not affected by this value, it's always included",10
	db "  /METHOD:xxx     Specifies the method to be used for A20 handling.",10
	db "                  Possible values for xxx:",10
	db "                  ALWAYSON    Assume that A20 line is permanently ON",10
	db "                  BIOS        Use BIOS to toggle the A20 line",10
	db "                  FAST        Use port 92h, bypass INT 15h test",10
	db "                  PS2         Use port 92h, bypass PS/2 test",10
	db "                  KBC         Use the keyboard controller",10
	db "                  PORT92      Use port 92h always",10
	db "  /HMAMIN=n       Specifies minimum number of Kbs of HMA that a program",10
	db "                  must request to gain access to the HMA (default: 0Kb)",10
	db "  /NUMHANDLES=m   Number of XMS handles (default: 48, min: 8, max: 128)",10
if ?TESTMEM
	db "  /TESTMEM:ON|OFF Performs or skips an extended memory test (def: OFF)",10
endif
	db "  /VERBOSE        Gives extra information",10 
	db "  /NOABOVE16      Do not use INT 15h function E801h to detect >64M",10
	db "  /X              Do not use INT 15h function E820h to detect >64M",10
	db "  /X2MAX32        Limit XMS 2.0 free/avail. memory report to 32M-1K",10
if ?LOG
	db "  /LOG            Logs the driver activity to the screen",10
endif
	db 0

_DATA ends

;******************************************************************************
; resident code and data
;******************************************************************************

_RTEXT segment

	assume  cs:DGROUP, ds:DGROUP

;******************************************************************************
; device driver header

        dd  -1                       ; +0 last driver in list
        dw  8000h                    ; +4 driver flags
        dw  offset strategy          ; +6 pointer to strategy routine
        dw  offset init_interrupt    ; +8 pointer to interrupt handler
        db  'XMSXXXX0'      ; device driver name

;******************************************************************************
; global data

;--- put some variables before gdt_start (so it is QWORD aligned)

a20_locks dw 0          ; internal A20 lock count
_x2max32  dw MAXFREEKB  ; maximum XMS 2.0 free/available 32M
_hma_min  dw 0          ; minimal space in HMA 


gdt_start	label near

gdt32       dw  gdt_size-1,gdt_start,0
hma_used  	db 0          ; set if HMA is used
dummyretf:
			retf

data32dsc   desc <-1,0,0,92h,0cfh,0>  ; 08 32-bit read/write data, 4G
data16dsc   desc <-1,0,0,92h,  0h,0>  ; 10 16-bit read/write data, 64k
;code16dsc   desc  <-1,0,0,9ah,0,0>    ; 18 16-bit execute/read code, 64K
gdt_size equ $ - offset gdt_start

data32sel   equ 08h
data16sel   equ 10h
;code16sel   equ 18h

xms_highest_addr dd 10FFFFh
xms_handle_table XMS_HANDLETABLE <0fdh, size XMS_HANDLE, 0, 0>

	align 4

;******************************************************************************
;--- A20 get status
; Out:  ZF=0 - A20 enabled
;   ZF=1 - A20 disabled

test_a20 proc
if 0
	push ax
	push ds
	push es
	xor ax,ax
	mov ds,ax
	dec ax
	mov es,ax		; es->FFFF seg
	assume es:DGROUP; es is NOT DGROUP, but this makes MASM happy
	pushf			; save original flags (IF is important)
	cli				; ensure interrupts are off while testing
	mov ax,es:[10h]	; read word at FFFF:10h, the 1M limit
	not ax          ; ~1M word
	push @word ds:[0]	; save word we're changing (INT 0 offset)
	mov ds:[0],ax	; save ~1M word to 0:0 (and FFFF:10h if A20 disabled)
;	mov ax,ds:[0]	; read back, may be unnecessary (forced memory access?)
	cmp ax,es:[10h]	; compare 0:0 ~1M word to 1M word, only equal if A20 is disabled
	pop @word ds:[0]	; restore INT 0 offset
	lahf
	popf
	sahf
	pop es
	pop ds
	pop ax
else

;--- this might be better: no need to disable interrupts and no memory
;--- is modified. 4 dwords are compared (0000:0010 and FFFF:0020)

	push ds
	push es
	push cx
	push si
	push di
	mov cx,-1
	mov es,cx
	mov si,10h
	inc cx
	mov ds,cx
	mov di,20h
	mov cl,4
	repz cmpsd
	pop di
	pop si
	pop cx
	pop es
	pop ds
endif
	ret
test_a20 endp

;******************************************************************************
; enables the A20 address line
; currently dummy/always on, code replaced as A20 tests indicate
; all registers AND flags preserved!

enable_a20 proc
	push ax
	mov ah,2
	jmp disable_enable_a20
enable_a20 endp

disable_a20 proc
	push ax
	mov ah,0
disable_a20 endp	;fall through

disable_enable_a20 proc	;patch area

;--- the default is the ALWAYSON "method", which is just a dummy

	pop ax
	ret

; since this is replaceable, we need to bulk up the space allocated for it
;  for larger replacement routines

	DB 42-2 DUP (?)

size_disable_enable_a20 equ $ - disable_enable_a20

disable_enable_a20 endp

;******************************************************************************
; Interrupt handlers
;******************************************************************************
;******************************************************************************
; new INT15h handler
;
; this externally preserves A20 state on function 87h
;

int15_handler proc
	cmp ah,87h
	je do_move
	cmp ah,88h				; is it a ext. mem size req.?
	je ext_mem_size
	db 0EAh
old_int15 dd 0 				; old INT 15h vector

ext_mem_size:
	xor ax,ax				; no memory available
exit_set_reset_carry:
	push bp
	mov bp,sp
	rcr [bp+2].IRETS.bFlags,1
	rol [bp+2].IRETS.bFlags,1
	pop bp
	iret

do_move:
	pushf
	call test_a20
	jz isdisabled
	call cs:[old_int15]
	call enable_a20 	 ; preserves flags
	jmp exit_set_reset_carry
isdisabled:
	call cs:[old_int15]
	call disable_a20	 ; preserves flags
	jmp exit_set_reset_carry

int15_handler endp

;******************************************************************************
; new INT2Fh handler. Catches Func. 4300h+4310h

int2f_handler proc
	pushf
	cmp ah,43h
	je @@maybe_my2f
@@jmp_old2f:
	popf
	db 0EAh
old_int2f dd 0            ; old INT 2fh vector


@@maybe_my2f:
	cmp al,00h			; is it "Installation Check"?
	je @@get_driver_installed
	cmp al,10h			; is it "Get Driver Address"?
	je @@get_xms_address	 
	cmp al,09h			; is it "get handle table"?
	je @@get_xms_handle_table
	cmp al,08h			; is it "get A20 handler number"?
	jne @@jmp_old2f
	mov al,ah		;al==43h if function supported
machine_type label byte    
	mov bx,0002		;bh=switch time; 0=medium, 1=fast, 2=slow
	popf			;bl=machine type; 01=std AT (KBC), 02=PS/2 (port 92)
	iret
@@get_driver_installed:
	mov al,80h				; yes, we are installed ;)
	popf
	iret
@@get_xms_address:
	mov bx,offset xms_dispatcher
@@shared2f:
	push cs
	pop es
	popf
	iret
@@get_xms_handle_table:
	mov al,ah 		;al==43h if function supported
	mov bx,offset xms_handle_table
	jmp @@shared2f

int2f_handler endp

;******************************************************************************
; XMS functions
;******************************************************************************
; returns XMS version number
; In:   AH=0
; Out:  AX=XMS version number
;   BX=internal revision number
;   DX=1 if HMA exists, 0 if not

xms_get_version proc
	mov ax,INTERFACE_VER
	mov bx,DRIVER_VER
	mov dx,1				; HMA is always available
	ret
xms_get_version endp

;******************************************************************************
; requests HMA
; In:   AH=1
;   DX=space needed in HMA (0ffffh if application tries to request HMA)
; Out:  AX=1 if successful
;   AX=0 if not successful
;     BL=80h -> function not implemented (implemented here ;) )
;     BL=81h -> VDISK is detected
;     BL=90h -> HMA does not exist
;     BL=91h -> HMA already in use
;     BL=92h -> DX less than HMA_MIN

xms_request_hma proc
	xor ax,ax
	cmp [hma_used],al			; is HMA already used?
	mov bl,XMS_HMA_IN_USE
	jnz @@exit
	cmp dx,[_hma_min]			; is request big enough?
	mov bl,XMS_HMAREQ_TOO_SMALL
	jb @@exit
	inc ax
	mov [hma_used],al			; assign HMA to caller
	mov bl,0
@@exit:
	ret
xms_request_hma endp

;******************************************************************************
; releases HMA
; In:   AH=2
; Out:  AX=1 if successful
;   AX=0 if not successful
;     BL=80h -> function not implemented
;     BL=81h -> VDISK is detected
;     BL=90h -> HMA doesn't exist
;     BL=93h -> HMA wasn't allocated

xms_release_hma proc
	xor ax,ax
	cmp [hma_used],al			; is HMA used?
	mov bl,XMS_HMA_NOT_USED
	jz @@exit 
	mov [hma_used],al			; now release it
	inc ax
	mov bl,0
@@exit:
	ret
xms_release_hma endp

;******************************************************************************
; global A20 address line enable
; In:   AH=3
; Out:  AX=1 if successful
;   AX=0 if not successful
;     BL=80h -> function is not implemented
;     BL=81h -> VDISK is detected
;     BL=82h -> A20 failure

xms_global_enable_a20 proc
	call enable_a20			; enable A20
	call test_a20			; is it really enabled?
	jz error_A20_failure
xms_global_enable_a20 endp	; fall throu

xms_success:
	mov ax,1
	mov bl,0
	ret

;******************************************************************************
; global A20 address line disable
; In:   AH=4
; Out:  AX=1 if successful (A20 is disabled)
;   AX=0 if not successful
;     BL=80h -> function is not implemented
;     BL=81h -> VDISK is detected
;     BL=82h -> A20 failure
;     BL=94h -> A20 still enabled

xms_global_disable_a20 proc

IF ALLOWDISABLEA20
  if 1
	cmp [a20_locks],0
	jnz error_A20_still_enabled
  endif  
	call disable_a20			; disable A20
	call test_a20				; is it really disabled?
	jz xms_success
endif

error_A20_still_enabled::
	xor ax,ax
	mov bl,XMS_A20_STILL_ENABLED
	ret

xms_global_disable_a20 endp

error_A20_failure:
	xor ax,ax
	mov bl,XMS_A20_FAILURE
	ret

;******************************************************************************
; enables A20 locally
; In:   AH=5
; Out:  AX=1 if A20 is enabled, 0 otherwise
;   BL=80h -> function not implemented
;   BL=81h -> VDISK is detected
;   BL=82h -> A20 failure

xms_local_enable_a20 proc

	cmp [a20_locks],1
	inc [a20_locks] 		 ; increase lock counter
	jc xms_global_enable_a20
	jmp xms_success

xms_local_enable_a20 endp

;******************************************************************************
; disables A20 locally
; In:   AH=6
; Out:  AX=1 if A20 is disabled, 0 otherwise
;   BL=80h -> function not implemented
;   BL=81h -> VDISK is detected
;   BL=82h -> A20 failure
;   BL=94h -> A20 still enabled

xms_local_disable_a20 proc

	cmp [a20_locks],0
	jz error_A20_failure
	dec [a20_locks] 			; decrease lock counter
	jnz error_A20_still_enabled
	jmp xms_global_disable_a20

xms_local_disable_a20 endp

;******************************************************************************
; returns the state of A20
; In:   AH=7
; Out:  AX=1 if A20 is physically enabled, AX=0 if not
;   BL=00h -> function was successful
;   BL=80h -> function is not implemented
;   BL=81h -> VDISK is detected

xms_query_a20 proc
	xor ax,ax			; suppose A20 is disabled
	call test_a20
	setnz al
	mov bl,0
	ret
xms_query_a20 endp


;******************************************************************************
; alloc a XMS memory handle
; In:   DS=CS
; Out:  CY=1 - no free handle
;   CY=0 - free handle found
;     BX - offset of free handle

xms_alloc_handle proc
	mov cx,[xms_handle_table.xht_numhandles]	; check all handles
	mov bx, @word [xms_handle_table.xht_pArray]
@@nexthandle:
	cmp [bx].XMS_HANDLE.xh_flags,XMSF_INPOOL
	jz @@found_handle			; found a blank handle
	add bx,sizeof XMS_HANDLE	; skip to next handle
	loop @@nexthandle
	stc 						; no free block found, error
@@found_handle:
	ret
xms_alloc_handle endp

;******************************************************************************
; xms_check_handle
; In:   DS=CS
;   SI - handle to check
;
; Out:  CY=1     - no valid handle
;         BL=0a2h  - XMS_INVALID_HANDLE
;         AX=0     - usual error return
;
;       CY=0     - no error
;
; registers destroyed - AX
;
;-- called by 
;--  xms_free_emb
;--  xms_move_emb
;--  xms_lock_emb
;--  xms_unlock_emb
;--  xms_ext_get_handle_info
;--  xms_ext_realloc_emb

xms_check_handle_ex:
	mov si,dx

xms_check_handle proc
	push dx

	mov ax,si
	sub ax, @word [xms_handle_table.xht_pArray]
	jb @@no_valid_handle
	xor dx,dx

	push bx
	mov bx,sizeof XMS_HANDLE
	div bx
	pop bx

	or dx,dx
	jnz @@no_valid_handle

	cmp ax,[xms_handle_table.xht_numhandles]	; less then last handle ??
	jae @@no_valid_handle

	cmp [si].XMS_HANDLE.xh_flags,XMSF_USED	 ; is it in use ??
	jne @@no_valid_handle
	pop dx
	xor ax,ax
	ret
@@no_valid_handle:
	pop dx
	pop ax	   ;skip return address
	xor ax,ax
	mov bl,XMS_INVALID_HANDLE
	stc
	ret

xms_check_handle endp

;******************************************************************************
; query free extended memory
; In:   AH=88h
; Out:  EAX=size of largest free XMS block in kbytes
;   ECX=highest ending address of any memory block
;   EDX=total amount of free XMS in kbytes
;   BL=0 if ok
;   BL=080h -> function not implemented
;   BL=081h -> VDISK is detected
;   BL=0a0h -> all XMS is allocated

xms_ext_query_free_mem proc

	xor eax,eax 	; contains largest free block
	xor edx,edx 	; contains total free XMS

	push bx
	pushf
	cli
	mov cx,[xms_handle_table.xht_numhandles]
	mov bx, @word [xms_handle_table.xht_pArray]
nextitem:
	test [bx].XMS_HANDLE.xh_flags,XMSF_FREE	; check if flagged free or in use
	je @F
	mov esi, [bx].XMS_HANDLE.xh_sizeK
	add edx, esi
	cmp esi, eax			  ; check if larger than largest
	jbe @F
	mov eax,esi 			  ; larger, update
@@:
	add bx,sizeof XMS_HANDLE
	loop nextitem
	popf			; restore IF
	pop bx
	mov bl,0
	and edx,edx
	jnz @@freeblockexists
	mov bl,XMS_ALL_MEM_ALLOCATED
@@freeblockexists:
	mov ecx,[xms_highest_addr] 	; highest address to ecx return value
	ret 			; success

xms_ext_query_free_mem endp

;******************************************************************************
; returns free XMS
; In:   AH=8
; Out:  AX=size of largest free XMS block in kbytes
;       DX=total amount of free XMS in kbytes
;   BL=0 if ok
;   BL=0a0h -> all XMS is allocated

xms_query_free_mem proc

				; protect high parts 
	push eax
	pop ax
	push ecx
	push edx
	pop dx

	call xms_ext_query_free_mem	
    
	; returns:
	;   EAX=size of largest free XMS block in kbytes
	;   ECX=highest ending address of any memory block (not used)
	;   EDX=total amount of free XMS in kbytes

	movzx ecx, [_x2max32]
	cmp edx,ecx             ; dx = min(edx,0ffff | 7fff)
	jb @@edx_not_larger
	mov dx,cx
@@edx_not_larger:
	cmp eax,ecx             ; ax = min(eax,0ffff | 7fff)
	jb @@eax_not_larger
	mov ax,cx
@@eax_not_larger:

				; restore high parts 
	push dx
	pop edx

	pop ecx

	push ax
	pop  eax

	ret
xms_query_free_mem endp

;******************************************************************************
; allocates an XMS block
; In:   AH=9
;   DX=amount of XMS being requested in kbytes
; Out:  AX=1 if successful
;     DX=handle
;   AX=0 if not successful
;     DX=0 (according to XMS docs)
;     BL=080h -> function not implemented
;     BL=081h -> VDISK is detected
;     BL=0a0h -> all XMS is allocated
;     BL=0a1h -> no free handles left

xms_alloc_emb proc
	@DbgOutS <"xms_alloc_emb",13,10>
	push edx
	movzx edx,dx	; extend alloc request to 32-bits
	jmp @@xms_alloc2

; 32-bit entry for function 89h, just uses full edx value

xms_ext_alloc_emb::
	@DbgOutS <"xms_ext_alloc_emb",13,10>
	push edx

@@xms_alloc2:
	push cx
	pushf
	cli
if 0; v3.35 check for size 0 after scan!
	and edx,edx 			 ; a request for 0 kB might still work
	jz @@nullhandle
endif
	mov cx,[xms_handle_table.xht_numhandles]	; check all handles
	mov di, @word [xms_handle_table.xht_pArray] 
@@nexthandle:
	cmp [di].XMS_HANDLE.xh_flags,XMSF_FREE
	jnz @@skipitem
	cmp edx,[di].XMS_HANDLE.xh_sizeK   ; check if it's large enough
	jbe @@found_block
@@skipitem:
	add di,sizeof XMS_HANDLE	 ; skip to next handle
	loop @@nexthandle
if 1; v3.35 check for size 0
	and edx,edx 			 ; a request for 0 kB might still work
	jz @@nullhandle
endif
	mov bl,XMS_ALL_MEM_ALLOCATED
@@alloc_failed:
	popf
	pop cx
	pop edx
if ?ALLOCDX0
	xor dx,dx  ;return DX=0 if alloc fails
endif
	xor ax,ax
	ret
@@nullhandle:
	push bx
	call xms_alloc_handle	 ; get a free handle in BX
	mov di,bx
	pop bx
	mov bl,XMS_NO_HANDLE_LEFT
	jc @@alloc_failed
	xor ax,ax				 ; set ZF to skip code below

@@found_block:
	mov @word [di].XMS_HANDLE.xh_flags,XMSF_USED ;clear locks field, too
	jz @@perfect_fit2				; if it fits perfectly, go on
	push bx
	call xms_alloc_handle			; get a free handle in BX
	jc @@perfect_fit				; no more handles, use all mem left
ife ?ALTSTRAT
	mov esi,[di].XMS_HANDLE.xh_sizeK
	mov [di].XMS_HANDLE.xh_sizeK,edx
	sub esi,edx 					; calculate resting memory
	add edx,[di].XMS_HANDLE.xh_baseK; calc new base address of free block 
	mov @word [bx].XMS_HANDLE.xh_flags,XMSF_FREE
	mov [bx].XMS_HANDLE.xh_baseK,edx; set new base of free block
	mov [bx].XMS_HANDLE.xh_sizeK,esi; set remaining size of free block
else
;--- alternate strategie: return the new allocated handle,
;--- the found handle stays free
	mov [di].XMS_HANDLE.xh_flags,XMSF_FREE
	mov esi,[di].XMS_HANDLE.xh_baseK
	add [di].XMS_HANDLE.xh_baseK,edx
	sub [di].XMS_HANDLE.xh_sizeK,edx
	mov [bx].XMS_HANDLE.xh_baseK,esi
	mov [bx].XMS_HANDLE.xh_sizeK,edx
	mov @word [bx].XMS_HANDLE.xh_flags,XMSF_USED ;clear locks field, too
	mov di,bx
endif
@@perfect_fit:
	pop bx
@@perfect_fit2:
	popf
	pop cx
	pop edx
	mov dx,di						; return handle in DX
	mov bl,0
	mov ax,1
	ret
xms_alloc_emb endp

;******************************************************************************
; frees an XMS block
; In:   AH=0ah
;   DX=handle to allocated block that should be freed
; Out:  AX=1 if successful
;   AX=0 if not successful
;     BL=080h -> function not implemented
;     BL=081h -> VDISK is detected
;     BL=0a2h -> handle is invalid
;     BL=0abh -> handle is locked

xms_free_emb proc

	call xms_check_handle_ex	; check if dx holds a "used" handle
	mov bl,XMS_BLOCK_LOCKED
	cmp [si].XMS_HANDLE.xh_locks,0	; is the block locked?
	jnz @@exit
	push eax
	push bx
	push cx
	push edx
	pushf
	cli
;--- see if there are blocks to merge
	mov eax,[si].XMS_HANDLE.xh_baseK   ; get base address
	mov edx,[si].XMS_HANDLE.xh_sizeK
	mov edi, eax                    ; base in edi
	add eax, edx					; end-address in eax
ife ?MERGE0HDL
	mov cl, XMSF_FREE
	and edx, edx
	jnz @F
	mov cl, XMSF_INPOOL
@@:
	mov [si].XMS_HANDLE.xh_flags,cl
	jz @@done
endif

;--- now scan the handle array for successor/predecessor

	mov cx,[xms_handle_table.xht_numhandles]
	mov bx,@word [xms_handle_table.xht_pArray] 
@@nextitem:
	cmp [bx].XMS_HANDLE.xh_flags,XMSF_FREE
	jnz @@skipitem
	mov edx,[bx].XMS_HANDLE.xh_baseK
	cmp eax, edx				; is successor free?
	je @F
	add edx,[bx].XMS_HANDLE.xh_sizeK
	cmp edi, edx				; is predecessor free?
	jne @@skipitem
@@:
;--- predecessor/successor in BX
	cmp bx,si
	jbe @F
	xchg bx,si					;merge into the "lower" handle and free the "higher" handle
@@:
	xor edx, edx
	xchg edx, [si].XMS_HANDLE.xh_sizeK
	add [bx].XMS_HANDLE.xh_sizeK, edx	;new size is sum of both handle sizes
	xor edx, edx
	xchg edx, [si].XMS_HANDLE.xh_baseK
	cmp edx, [bx].XMS_HANDLE.xh_baseK
	ja @F
	mov [bx].XMS_HANDLE.xh_baseK, edx	;new base is min(hdl1.base,hdl2.base)
@@:
	mov [si].XMS_HANDLE.xh_flags,XMSF_INPOOL
	mov si,bx
@@skipitem:
	add bx,sizeof XMS_HANDLE
	loop @@nextitem
if ?MERGE0HDL
	mov cl, XMSF_FREE
	cmp [si].XMS_HANDLE.xh_sizeK,0
	jnz @F
	mov cl, XMSF_INPOOL
@@:
	mov [si].XMS_HANDLE.xh_flags,cl
endif
@@done:
	popf
	pop edx
	pop cx
	pop bx
	pop eax
	inc ax
	mov bl,0
@@exit:
	ret

xms_free_emb endp

;******************************************************************************
; calculates the move address
; In: SI - handle (0 if EDX should be interpreted as seg:ofs value)
;   EDX - offset
;   ECX - length
; Out:  EAX - linear move address
; Modifies: EDX, SI

xms_get_move_addr proc
	or si,si			; translate address in EDX?
	jnz @@is_emb

						; its segment:offset in EDX

						; eax = 16*(edx high) + dx
	movzx eax,dx		; save offset
	mov dh,0
	shr edx,12			; convert segment to absolute address
	add eax,edx 		; add offset

	mov edx,eax 		; check that eax(address) + ecx (length) is <= 10fff0
	add edx,ecx
	jc @@wrong_size 	; negative length might wrap
	cmp edx,10fff0h
	ja @@wrong_size
	clc
	ret

@@is_emb:               ; it's a handle:offset pair
	call xms_check_handle	;check if si holds a "used" handle

	mov eax,ecx 		; contains length
	add eax,edx 		; assert length + offset < size    
	jc @@wrong_size		; probably negative length
	add eax,1024-1		;
	jc @@wrong_size		; probably negative length

	shr eax,10			; convert to kB units
	cmp eax,[si].XMS_HANDLE.xh_sizeK	; compare with max offset
	ja @@wrong_size

	mov eax,[si].XMS_HANDLE.xh_baseK   ; get block base address
	shl eax,10			; convert from kb to linear
	add eax,edx 		; add offset into block
	ret

@@wrong_size:
	mov bl,XMS_INVALID_LENGTH
	xor ax,ax
	stc
	ret
xms_get_move_addr endp

;******************************************************************************
; moves an EMB
; In:   AH=0bh
;   ES:SI=pointer to XMS move structure (DS is DGROUP)
; Out:  AX=1 if successful
;   AX=0 if not successful
;     BL=080h -> function not implemented
;     BL=081h -> VDISK is detected
;     BL=082h -> A20 failure
;     BL=0a3h -> source handle is invalid
;     BL=0a4h -> source offset is invalid
;     BL=0a5h -> destination handle is invalid
;     BL=0a6h -> destination offset is invalid
;     BL=0a7h -> length is invalid
;     BL=0a8h -> move has invalid overlap
;     BL=0a9h -> parity error


xms_move_emb proc

	@DbgOutS <"xms_move_emb enter",13,10>
IF ALLOWDISABLEA20
	call test_a20				; get A20 state
	jnz @F
	call enable_a20 			; now enable it! - if it was disabled
	push offset disable_a20		; and make sure it is disabled on exit
@@:
endif
	xor ax,ax					; default to error
	push ecx
	push edx
	push eax
	push bx

	mov ecx,es:[si].xms_move.len	; get length
	test cl,1						; is it even?
	jnz @@move_invalid_length

	push si
	mov edx,es:[si].xms_move.dest_offset
	mov si,es:[si].xms_move.dest_handle
	call xms_get_move_addr			; get move address
	pop si
	jc @@copy_dest_is_wrong
	mov edi,eax 					; store in destination index

	mov edx,es:[si].xms_move.src_offset
	mov si,es:[si].xms_move.src_handle
	call xms_get_move_addr			; get move address
	jc @@copy_source_is_wrong
	mov esi,eax 					; store in source index

;**************************************************
; setup finished with
;   ESI = source
;   EDI = destination
;   ECX = number of words to move
;
; now we must check for potential overlap
;**************************************************

	or ecx,ecx 				; nothing to do ??
	jz @@xms_exit_copy

	cmp esi,edi
;	jz @@xms_exit_copy	 ;11/2020: don't exit if src=dst

;
; if source >= destination, it's ok
; ( at least if the BIOS, too, does it with CLD)

;	ja @@move_ok_to_start
	jae @@move_ok_to_start

;
; no, it's less
; if (source + length > destination)
;    return ERROR_OVERLAP

	lea edx, [esi+ecx]
	cmp edx, edi
	ja @@move_invalid_overlap

;
; we might be able to handle that, but are not yet
; so better don't copy
;   jmp use_int15               ; always BIOS


@@move_ok_to_start:
	SMSW AX 					; don't use priviledged "mov eax,cr0"!
	test al,1					; are we already in PM?
	mov bx,offset rmcopy		; no, switch to pm yourself
	jz @F
	push ss						; set ES for int 15h, ah=87h call
	pop es
	mov bx,offset pmcopy		; yes, use INT 15h, ah=87h
@@:
	mov edx,ecx

@@copy_loop:
	mov ecx,edx
	cmp ecx,BLOCKSIZE
;	jle @F						;27.10.2020: no signed compare here!
	jbe @F
	mov ecx,BLOCKSIZE
@@:
	sub edx,ecx
	push dx
	call bx
	pop dx
	jc @@move_a20_failure
	and edx, edx
	jnz @@copy_loop
@@xms_exit_copy:
	pop bx
	pop eax
	pop edx
	pop ecx
	inc ax			; success
;	mov bl,0		; BL is not set to 00 by MS Himem
	ret

@@move_invalid_overlap:
	mov bl,XMS_INVALID_OVERLAP
	jmp @@xms_exit_copy_failure

@@move_invalid_length:
	mov bl,XMS_INVALID_LENGTH
	jmp @@xms_exit_copy_failure

@@copy_source_is_wrong:
	cmp bl,XMS_INVALID_LENGTH
	je @@xms_exit_copy_failure
	mov bl,XMS_INVALID_SOURCE_HANDLE
	jmp @@xms_exit_copy_failure

@@copy_dest_is_wrong:
	cmp bl,XMS_INVALID_LENGTH
	je @@xms_exit_copy_failure
	mov bl,XMS_INVALID_DESTINATION_HANDLE
	jmp @@xms_exit_copy_failure

@@move_a20_failure:
	mov bl,XMS_A20_FAILURE

							; common error exit routine
@@xms_exit_copy_failure:
	pop ax
	mov bh,ah				; restore BH only, preserve BL
	pop eax
	pop edx
	pop ecx
	ret

;------------------------------------------------------------
; "real-mode" copy proc
;  ESI = src linear adress
;  EDI = dst linear adress
;  ECX = length (hiword cleared)

;  2 strategies are implemented. The first does the move in protected-mode,
;  the latter activates "unreal" mode and does the move there.
;  After the last transfer "unreal" mode is exited. Interrupts occuring
;  during the "interrupt window" will run in "unreal" mode as well, but
;  this shouldn't hurt.

rmcopy:

ife USEUNREAL

	pushf
	cli 					; no interrupts when doing protected mode
if PREF66LGDT
	db 66h					; load full 32bit base
endif
	lgdt fword ptr cs:[gdt32]; load GDTR (use CS prefix here)
	mov eax,cr0
	inc ax					; set PE bit
	mov cr0,eax
	jmp @F					; setting CS to a protected-mode selector is not required
@@:
	mov dx,data32sel
	mov ds,dx
	mov es,dx

	shr ecx,2				; get number of DWORDS to move
	rep movs @dword [edi],[esi]
	adc cx,cx
	rep movs @word [edi],[esi]	 ; move a trailing WORD

	db 67h					; don't remove - some 80386s were buggy
	nop 					; don't remove - some 80386s were buggy

	mov dx,data16sel		; restore selector attributes to 64 kB
	mov ds,dx
	mov es,dx

	dec ax					; clear PE bit
	mov cr0,eax
	popf
	clc
	ret

else

  if 0
	pushf
	cli 					; no interrupts during the block move
	pushf
	call set_ureal			; every time since the mode might have been
	xor dx,dx				; exited by an interrupt routine.
	mov ds,dx
	mov es,dx
	shr ecx,2				; get number of DWORDS to move
	rep movs @dword [edi],[esi]
	adc cx,cx
	rep movs @word [edi],[esi]	 ; move a trailing WORD
	db 67h					; don't remove - some 80386s were buggy
	nop 					; don't remove - some 80386s were buggy
	popf
	jnz @F
	mov dx,data16sel
	call reset_ureal
@@:
	popf
	ret

set_ureal:    
	mov dx,data32sel
reset_ureal:
if PREF66LGDT
	db 66h					; load full 32bit base
endif
	lgdt fword ptr cs:[gdt32]; load GDTR (use CS prefix here)
	mov eax,cr0
	inc ax					; set PE bit
	mov cr0,eax
;--- the 80386 needs a "flush" after switching to PM
;--- before a segment register can be set!
	jmp @F
@@:
	dec ax					; clear PE bit
	mov ds,dx
	mov es,dx
	mov cr0,eax
	ret
  else
;--- set int 0dh, then just start to copy.
;--- if int 0dh is called, an exception occured, since IRQs are disabled.
;--- then set unreal mode inside int 0dh code.
	xor dx,dx
	mov ax,cs
	mov ds,dx
	mov es,dx
	pushf
	shl eax,16
	mov ax,offset myint0d
	shr ecx,2				; get number of DWORDS to move
	cli
	xchg eax,ds:[13*4]
	rep movs @dword [edi],[esi]
	adc cx,cx
	rep movs @word [edi],[esi]	 ; move a trailing WORD
	db 67h					; don't remove - some 80386s were buggy
	nop 					; don't remove - some 80386s were buggy
	mov ds:[13*4],eax		; restore int 0dh
	popf
	ret
myint0d:
	push ds
	push es
	push eax
	lgdt fword ptr cs:[gdt32]; load GDTR (use CS prefix here)
	mov eax,cr0
	inc ax					; set PE bit
	mov cr0,eax
	jmp @F
@@:
	mov dx,data32sel
	dec ax					; clear PE bit
	mov ds,dx
	mov es,dx
	mov cr0,eax
	pop eax
	pop es
	pop ds
	iret
  endif

endif


;------------------------------------------------------------------------
; cpu is in v86-mode, use int15, ah=87 to copy things around

;  ESI = src linear adress
;  EDI = dst linear adress
;  ECX = length (hiword cleared)

; int 15h, ah=87h expects:
;  CX = number of WORDS to copy
;  ES:SI -> GDT
;  GDT: entry 0 = 0
;       entry 1 = 0
;       entry 2 = src
;       entry 3 = dst
;       entry 4 = used internally
;       entry 5 = used internally

pmcopy:
	sub sp,2*8		;room for entries 4+5

	shld eax,edi,16	;push dst descriptor
	mov dl,al
	mov dh,093h
	mov al,0
	push ax
	push dx
	push di
	push cx
	add edi,ecx 	; buff += copied length

	shld eax,esi,16	;push src descriptor
	mov dl,al
	mov al,0
	push ax
	push dx
	push si
	push cx
	add esi,ecx

	xor eax,eax		;push entries 0+1
	push eax
	push eax
	push eax
	push eax

	push si
	lea si,[esp+2]
	shr cx,1		; convert to words

	clc
;--- using IVT vector is slightly faster, but incompatible with Windows
if 0
	push ds
	mov ds,ax
	mov ah,87h
	pushf
	call dword ptr ds:[15h*4]
	pop ds
else
	mov ah,87h
	int 15h
endif
	pop si
	lea sp,[esp+6*8];don't modify flags!
	ret

xms_move_emb endp 

;******************************************************************************
; locks an EMB
; In:   AH=0ch
;   DX=XMS handle to be locked
; Out:  AX=1 if block is locked
;     DX:BX=32-bit linear address of block
;   AX=0 if not successful
;     BL=080h -> function not implemented
;     BL=081h -> VDISK is detected
;     BL=0a2h -> handle is invalid
;     BL=0ach -> lock count overflow
;     BL=0adh -> lock fails

xms_lock_emb proc

	@DbgOutS <"xms_lock_emb enter",13,10>
	call xms_check_handle_ex	; check if dx holds "used" handle
	inc [si].XMS_HANDLE.xh_locks   ; increase lock counter
	jz @@lock_error
	mov esi,[si].XMS_HANDLE.xh_baseK
	shl esi,10					; calculate linear address
	push esi
	pop bx
	pop dx
	inc ax
	ret
@@lock_error:
	dec [si].XMS_HANDLE.xh_locks
	mov bl,XMS_LOCK_COUNT_OVERFLOW
	ret
xms_lock_emb endp

;******************************************************************************
; unlocks an EMB
; In:   AH=0dh
;   DX=XMS handle to unlock
; Out:  AX=1 if block is unlocked
;   AX=0 if not successful
;     BL=080h -> function not implemented
;     BL=081h -> VDISK is detected
;     BL=0a2h -> handle is invalid
;     BL=0aah -> block is not locked

xms_unlock_emb proc

	@DbgOutS <"xms_unlock_emb enter",13,10>
	call xms_check_handle_ex	   ; check if dx holds "used" handle
	cmp [si].XMS_HANDLE.xh_locks,al; check if block is locked
	jz @F
	dec [si].XMS_HANDLE.xh_locks   ; decrease lock counter
	inc ax
	mov bl,0
	ret
@@:
	mov bl,XMS_BLOCK_NOT_LOCKED
	ret
xms_unlock_emb endp

;******************************************************************************
; get XMS handle information
; In:   AH=8eh
;   DX=XMS handle
; Out:  AX=1 if successful
;     BH=block's lock count
;     CX=number of free XMS handles
;     EDX=block's length in kbytes

xms_ext_get_handle_info proc

	@DbgOutS <"xms_ext_get_handle_info enter",13,10>
	call xms_check_handle_ex; check handle validity (dx== "used" handle?)
	xor cx,cx				; reset free handle counter
	mov dx,[xms_handle_table.xht_numhandles]
	mov di,@word [xms_handle_table.xht_pArray] ;use DI here, not BX
@@nextitem:
	cmp [di].XMS_HANDLE.xh_flags,XMSF_INPOOL
	setz al
	add cx,ax
	add di,sizeof XMS_HANDLE
	dec dx
	jnz @@nextitem
	mov bh,[si].XMS_HANDLE.xh_locks 	; store lock count
	mov edx,[si].XMS_HANDLE.xh_sizeK	; store block size
;	mov bl,0   ;set BL on exit?
	mov al,1
	ret
xms_ext_get_handle_info endp

;********************************************************************
; returns XMS handle information
; In:   AH=0eh
;   DX=XMS handle
; Out:  AX=1 if successful
;     BH=block's lock count
;     BL=number of free XMS handles
;     DX=block's length in kbytes
;   AX=0 if not successful
;     BL=080h -> function not implemented
;     BL=081h -> VDISK is detected
;     BL=0a2h -> handle is invalid

xms_get_handle_info proc

	push cx
	push edx  ;save Hiword(edx)
	pop dx
	@DbgOutS <"xms_get_handle_info enter",13,10>

	call xms_ext_get_handle_info
	or ax,ax
	jz @@get_handle_info_err

;--- free handles is returned in BL
	cmp ch,0					; bl = min(cx,0xff)
	jz @@handle_count_ok
	mov cl,0ffh
@@handle_count_ok:
	mov bl,cl

;--- the size is returned in 16-bit register DX
;--- if it's larger than 65535, 65535 is returned.
;--- MS Himem behaves differently, it returns AX=0
;--- and BL=A2 (invalid handle).
	cmp edx,010000h				; dx = min(edx,0xffff);
	jb @@handle_size_ok
	mov dx,0ffffh
@@handle_size_ok:

@@get_handle_info_err:

	push dx
	pop edx   ;restore Hiword(edx)
	pop cx
	ret

xms_get_handle_info endp

;******************************************************************************
;  reallocates an EMB. shrinking and growing supported
; In:   AH=8fh
;   EBX=new size for the EMB in kbytes
;   DX=unlocked XMS handle
; Out:  AX=1 if successful
;   AX=0 if not successful
;     BL=080h -> function not implemented
;     BL=081h -> VDISK is detected
;     BL=0a0h -> all XMS is allocated
;     BL=0a1h -> all handles are in use
;     BL=0a2h -> invalid handle
;     BL=0abh -> block is locked

xms_ext_realloc_emb proc

	@DbgOutS <"xms_ext_realloc_emb enter",13,10>
	call xms_check_handle_ex   ; dx == "used" handle?
	push edx

; fail if block is locked
	cmp [si].XMS_HANDLE.xh_locks,0
	jne @@ext_xms_locked

	mov edx, ebx
if 1;v3.35
	mov cx,[xms_handle_table.xht_numhandles]
	mov di,@word [xms_handle_table.xht_pArray]
	mov eax,[si].XMS_HANDLE.xh_sizeK
	add eax,[si].XMS_HANDLE.xh_baseK
nextitem:
	test [di].XMS_HANDLE.xh_flags,XMSF_FREE	;scan "free embs" only
	jz skipitem
	cmp eax,[di].XMS_HANDLE.xh_baseK	;successor?
	jnz skipitem
	mov eax,[si].XMS_HANDLE.xh_sizeK
	add eax,[di].XMS_HANDLE.xh_sizeK	;get the total size
	cmp edx,eax                         ;new size > total size?
	ja @@ext_growing                    ;then the handle can't grow, have to copy...
	sub edx,[si].XMS_HANDLE.xh_sizeK	;get the size which is additionally needed (might be < 0!)
	mov [si].XMS_HANDLE.xh_sizeK, ebx
	add [di].XMS_HANDLE.xh_baseK, edx
	sub [di].XMS_HANDLE.xh_sizeK, edx
	jnz @@ext_grow_success              ;remaining size > 0?
	mov [di].XMS_HANDLE.xh_flags, XMSF_INPOOL	;no, so free the handle
	mov [di].XMS_HANDLE.xh_baseK, 0
	jmp @@ext_grow_success
skipitem:
	add di,sizeof XMS_HANDLE
	loop nextitem
endif
	cmp ebx,[si].XMS_HANDLE.xh_sizeK
	jbe @@ext_shrink_it

@@ext_growing:
; growing, try to allocate a new block

	call xms_ext_alloc_emb	;get a new handle in DX, size EDX
	and ax,ax
	jz @@ext_failed

; got new block, copy info from old block to new block

	pop si				; get old handle
	push si

; transfer old handle data to new location

	xor edi,edi
	push edi			; dst.offset
	push dx				; dst.handle
	push edi			; src.offset
	push si				; src.handle
	mov edi,[si].XMS_HANDLE.xh_sizeK
	shl edi,0ah			; K to byte
	push edi			; length
	mov si,sp
	push ss
	pop es				; es:si -> xms_move
	call xms_move_emb
	add sp, sizeof xms_move
	push cs				; xms_move_emb eats critical ds value
	pop ds

	pop si
	push si

; swap handle data so handle pointers remain valid
; handle data is 10 bytes long

	mov di,dx
	pushf
	cli 			 ;exchange must be atomic
	mov edx,[si+0]
	xchg edx,[di+0]
	mov [si+0],edx
	mov edx,[si+4]
	mov ax,[si+8]
	xchg edx,[di+4]
	xchg ax,[di+8]
	mov [si+4],edx
	mov [si+8],ax
	popf
	mov dx,di

; free newly allocated handle in DX with old handle data in it

	call xms_free_emb
	jmp @@ext_grow_success

@@ext_no_xms_handles_left:
	pop bx
	popf
	mov bl,XMS_NO_HANDLE_LEFT
	jmp @@ext_failed

@@ext_xms_locked:
	mov bl,XMS_BLOCK_LOCKED

@@ext_failed:
	pop edx
	xor ax,ax
	ret

@@ext_shrink_it:
	mov edi,[si].XMS_HANDLE.xh_sizeK ; get old size
	sub edi,edx 					 ; calculate what's left over
	jz @@ext_dont_need_handle		 ; jump if we don't need another handle
if 0; v3.36: don't modify si handle data until the new handle has been allocated
	mov [si].XMS_HANDLE.xh_sizeK, edx
	add edx,[si].XMS_HANDLE.xh_baseK ; calculate new base address
endif
	pushf
	cli
	push bx
	call xms_alloc_handle			 ; alloc a handle in BX, size EDI
	jc @@ext_no_xms_handles_left	 ; return if there's an error
if 1; v3.36: don't modify si handle data until the new handle has been allocated
	mov [si].XMS_HANDLE.xh_sizeK, edx
	add edx,[si].XMS_HANDLE.xh_baseK ; calculate new base address
endif
	mov [bx].XMS_HANDLE.xh_baseK, edx
	mov [bx].XMS_HANDLE.xh_sizeK, edi
if 1;v3.35
;--- if this branch is active, there's surely NO free successor
;--- so we don't need to merge.
	mov [bx].XMS_HANDLE.xh_flags,XMSF_FREE
	pop bx
	popf
else
	mov @word [bx].XMS_HANDLE.xh_flags,XMSF_USED
	mov dx,bx						 ; and FREE it again -
	pop bx
	popf
	call xms_free_emb				 ; to merge it with free block list
endif

@@ext_dont_need_handle:
@@ext_grow_success:
	pop edx
	mov ax,1
	mov bl,0
	ret
xms_ext_realloc_emb endp

;******************************************************************************
;  reallocates an EMB. shrinking and growing supported
; In:   AH=0fh
;   BX=new size for the EMB in kbytes
;   DX=unlocked XMS handle
;

xms_realloc_emb proc

	@DbgOutS <"xms_realloc_emb enter",13,10>
	push ebx    					; protect high part of ebx
	pop bx
	movzx ebx,bx					; clear top 16 bit
	call xms_ext_realloc_emb
	push bx 						; recover top 16 bit of ebx
	pop ebx
	ret

xms_realloc_emb endp

;******************************************************************************
; requests an UMB block
; In:   AH=10h
;   DX=size of requested memory block in paragraphs
; Out:  AX=1 if successful
;     BX=segment number of UMB
;     DX=actual size of the allocated block in paragraphs
;   AX=0 if not successful
;     DX=size of largest available UMB in paragraphs
;     BL=080h -> function not implemented
;     BL=0b0h -> only a smaller UMB are available
;     BL=0b1h -> no UMBs are available


;******************************************************************************
; releases an UMB block
; In:   AH=11h
;   DX=segment of UMB
; Out:  AX=1 if successful
;   AX=0 if not successful
;     BL=080h -> function not implemented
;     BL=0b2h -> UMB segment number is invalid


;******************************************************************************
; reallocates an UMB
; In:   AH=12h
;   BX=new size for UMB in paragraphs
;   DX=segment of UMB to reallocate
; Out:  AX=1 if successful
;   AX=0 if not successful
;     BL=080h -> function not implemented
;     BL=0b0h -> no UMB large enough to satisfy request
;       DX=size of largest UMB in paragraphs
;     BL=0b2h -> UMB segment is invalid

;xms_realloc_umb:
;xms_request_umb:
;xms_release_umb:
;    xor ax,ax
;    mov bl,XMS_NOT_IMPLEMENTED
;    ret;return_failure

;******************************************************************************
; XMS dispatcher
;******************************************************************************
; XMS dispatcher
; In:   AH - function number
; Out:  AX=0 -> function not supported
;   else see appr. routine

	align 2

xms_table label word
	dw xms_get_version			;0
	dw xms_request_hma			;1
	dw xms_release_hma			;2
	dw xms_global_enable_a20	;3
	dw xms_global_disable_a20	;4
	dw xms_local_enable_a20		;5
	dw xms_local_disable_a20	;6
	dw xms_query_a20			;7
	dw xms_query_free_mem		;8
	dw xms_alloc_emb			;9
	dw xms_free_emb				;10
	dw xms_move_emb				;11
	dw xms_lock_emb				;12
	dw xms_unlock_emb			;13
	dw xms_get_handle_info		;14
	dw xms_realloc_emb			;15

	dw xms_ext_query_free_mem	; 88            
	dw xms_ext_alloc_emb		; 89
	dw xms_ext_get_handle_info	; 8e
	dw xms_ext_realloc_emb		; 8f

xms_dispatcher proc
	jmp short @F
	nop
	nop 				; 3 nops, guarantee hookability
	nop
@@:

if ?LOG
dispatcher_log_entry label byte
	call log_entry      ; this might get patched
endif

	pushf
	cmp ah,0fh			; 00-0F?
	jbe @@ok1
	mov al,ah
	shr al,1
	cmp al,88h/2		; 88-89?
	jz @@ok2
	cmp al,8Eh/2		; 8E-8F?
	jz @@ok3
	xor ax,ax			; everything else fails
	mov bl,XMS_NOT_IMPLEMENTED
	jmp @@dispatcher_end
@@ok3:
	sub ah,4			; 8E-8F -> 8A-8B
@@ok2:
	sub ah, 88h-10h 	; 88-8B -> 10-13
;
;real dispatcher
;
; save ds,es,esi,edi
; set es = USERds
; set ds = DGROUP

@@ok1:
	cld
	push ds 		; protect registers
	push es
	push esi		; might get used 32 bit wide
	push edi		;

	push ds 		; set up segment registers for internal use
	pop es
	push cs
	pop ds

	movzx di,ah 	; is nowhere used as input
	shl di,1
	call [xms_table+di] ; call the handler here
	pop edi 		; restore saved registers
	pop esi

	pop es
	pop ds
;	 @DbgOutS <"xms exit",13,10>

@@dispatcher_end:
	popf
if ?LOG
dispatcher_log_exit label byte
	call log_exit		; this might get patched
endif
	retf

xms_dispatcher endp

?PRINTSTR = 0
ifdef _DEBUG 
?PRINTSTR = 1
endif
?PRINTSTR = ?PRINTSTR + ?LOG

if ?PRINTSTR
;*******************************************
; printing routines
;
; usage:
;   call printstring
;   db 'hello world'
;
; printdh,printdx - what the name implies
;
;*******************************************          
printstring proc

	pusha
	mov bp,sp
ifdef _DEBUG
	pushf
endif
	mov si,[bp+16]
	cld
@@nextitem:
	lods  @byte cs:[si]
	cmp al, 0 				   ; end of string?
	je @@done
	mov ah,0Eh
	mov bx,0007h
	int 10h
	jmp @@nextitem
@@done:
	mov [bp+16],si
ifdef _DEBUG
	popf
endif
	popa
	ret

printstring endp

endif

if 0

printdx proc
	call printdh
	ror dx,8

printdh:
	call printnibble
printnibble:

	ror dh,4
	mov al,dh
	and al,0Fh
	cmp al,10
	sbb al,69H
	das
	int 29h
	ret
printdx endp

endif

if ?LOG
;*** returns NZ, if we shall log NOW
;*** this will LOG stuff to screen only, if 
;*** SCROLL_LOCK is locked

lognow proc
	push ds
	push 40h
	pop ds
	test @byte ds:[17h],10h
	pop ds
	ret
lognow endp

log_entry proc
	pushf
	call lognow
	jz @F
	call printstring
	db 'XMS enter:',0
@@:
	popf
	ret
log_entry endp

log_exit proc
	pushf
	call lognow
	jz @F
	call printstring
	db ' XMS leave',13,10,0
@@:
	popf
	ret
log_exit endp

endif

;******************************************************************************
; mark for the trace log mode driver end. above has to be the resident part, 

	align 4

trace_driver_end:

_RTEXT ends

_TEXT segment

startoftext label byte

;******************************************************************************
; checks if VDISK is already installed
; note: HMA check is skipped because of speed and some other (weird) reasons.
; In:   nothing
; Out:  ZF=0 -> VDISK is installed
;   ZF=1 -> VDISK not installed
;
; tom:it's absolute unclear, if [13] or [12] should be checked.
;     HIMEM verifies [13], so we do that as well.
;     goto HELL, dear VDISK
;     verify only 4 bytes, should do as well
;


_install_check_vdisk proc
	push bx
	push ds

	xor bx,bx			; get interrupt vector 19h
	mov ds,bx
	lds bx,ds:[19h*4]

	cmp @dword [bx],053494456h; 'VDIS'

	pop ds
	pop bx
	ret
_install_check_vdisk endp

; checks if CPU is a 386
; In:   nothing
; Out:  CY=0 - processor is a 386 or higher
;   CY=1 - processor lower than 386

check_cpu proc
	pushf
	mov ah,70h
	push ax
	popf
	pushf
	pop ax
	popf
	and ah,0F0h
	cmp ah,70h	;Z=386 ok
	ret
check_cpu endp

if 0
; checks if A20 can be enabled and disabled
; Out:  CF=0 - A20 switching works
;   CF=1 - A20 failure

check_a20 proc
	call enable_a20
	call test_a20				; TEST_A20 should return ZF=0
	jz a20failed
IF ALLOWDISABLEA20	  
	call disable_a20
	call test_a20				; TEST_A20 should return ZF=1
	jz a20_ok
                                ; we can't disable A20.
                                ; so what ?
                                ; these guys are crazy anyway,
                                ; and we (nearly) ignore that 
                                    
	mov dx,offset cant_disable_message
	call dispmsg
a20_ok:
ENDIF
	clc
	ret
a20failed:
	stc
	ret
check_a20 endp

endif

;--- there are 3 A20 switch procs:
;--- 1. KBC (port 64h/60h)
;--- 2. fast, ps2, port92 (port 92h)
;--- 3. BIOS (int 15h, ax=240xh)

; try turning A20 on or off from current to see if it works
; KBC HIMEM method
; entry: ah == 0 A20 turn off, ah == 2 turn on, ax on stack
;
disable_enable_a20_KBC proc
	push cx
	pushf
	cli				; shut off interrupts while we twiddle

	call Sync8042	; check keyboard controller ready
	mov al,0D1h		; Send D1h
	out 64h,al
	call Sync8042
	mov al,0ddh		; or df=dd+2
	or al,ah		; disable/enable A20 command (DDh/DFh)
	out 60h,al
	call Sync8042

; wait up to 20 microseconds for A20 line to settle
	mov al,0FFh		; pulse output port NULL
	out 64h,al
	call Sync8042
	popf
	pop cx
	pop ax
	ret

Sync8042:
	xor cx,cx
@@InSync:
	in al,64h
	and al,2
	loopnz @@InSync
	ret

size_disable_enable_a20_KBC equ $ - disable_enable_a20_KBC

disable_enable_a20_KBC endp

; the so-called 'fast' A20 method replacement code
; entry: ah == 0 A20 turn off, ah == 2 turn on, ax on stack
;
disable_enable_a20_fast proc
	pushf
	in al,92h
	or ah,ah
	jne @@deaf_on	; turning on A20
	test al,2
	je @@deaf_done	; already flagged off, don't do it again, might upset something
	and al,NOT 2	; set A20 bit off
	jmp @@deaf_out

; ah == 2
@@deaf_on:
	test al,ah
	jne @@deaf_done	; already flagged on
	or al,ah		; set A20 bit on

@@deaf_out:
	out 92h,al

; wait until it gets on or off, possibly superfluous, code opinion differs
	push cx
	xor cx,cx
@@deaf_wait:
	in al,92h
	and al,2
	cmp al,ah
	loopne @@deaf_wait
	pop cx

@@deaf_done:
	popf
	pop ax
	ret

size_disable_enable_a20_fast equ $ - disable_enable_a20_fast

disable_enable_a20_fast endp

; BIOS A20 method
; entry: ah == 0 A20 turn off, ah == 2 turn on, ax on stack
; don't check for errors, assume BIOS works more than once on same call,
;  if it doesn't, not much we can do about it anyway
;
disable_enable_a20_BIOS:
	pushf
	sub sp,10	; give buggy BIOS some stack to chew on without causing problems
				; one word might suffice, but let's be really safe
	cli
	shr ah,1	; ah to 0 or 1
	mov al,24h
	xchg ah,al	; ax == 2400h to turn off, 2401h to turn on
	int 15h

	add sp,10	; restore potentially gnawed-on stack
	popf
	pop ax
	ret
size_disable_enable_a20_BIOS equ $ - disable_enable_a20_BIOS

;--- copy the enable/disable code to disable_enable_a20

flag_kbc:
	mov @byte [machine_type+1],1	;set machine to 1 (AT), which is KBC
	mov dx,offset szKBC
	mov si,offset disable_enable_a20_KBC
	mov cx,size_disable_enable_a20_KBC
	jmp xxx_success
flag_bios:
	mov dx,offset szBIOS
	mov si,offset disable_enable_a20_BIOS
	mov cx,size_disable_enable_a20_BIOS
	jmp xxx_success
flag_port92:
	mov dx,offset szPort92
	jmp fast_success
flag_ps2:
	mov dx,offset szPS2
	jmp fast_success
flag_fast:
	mov dx,offset szFast
fast_success:
	mov si, offset disable_enable_a20_fast
	mov cx, size_disable_enable_a20_fast
xxx_success:
	push si
	push cx

	mov ah,9
	int 21h
	mov dx,offset szA20
	mov ah,9
	int 21h
	
	pop cx
	pop si

	push di
	push es
	push cs
	pop  es
	mov di, offset disable_enable_a20
	rep movsb
	pop es
	pop di
	clc			; flag success
	ret

; check if BIOS flags port 92h fast method supported

detect_fast proc
	stc
	mov ax,2403h
	int 15h
	jc @@fail_test
	or ah,ah
	jne @@fail_test
	test bl,2		;PS/2 supported?
	je @@fail_test

	mov si,offset disable_enable_a20_fast
	call detect_and_handle_test
	ret
@@fail_test:
	stc			; flag failure
	ret
detect_fast endp

; check if BIOS flags PS/2 present, to try port 92h fast method used by PS/2's
;  shares enable/disable code with fast

detect_PS2 proc

	mov ah,0c0h		; get system description vector
	stc
	int 15h
	jc @@fail_test	; not a PS/2

; test feature information byte 1, micro channel implemented bit
	test @byte es:[bx+5],2
	jz @@fail_test	; not micro channel

	mov si,offset disable_enable_a20_fast
	call detect_and_handle_test
	ret

@@fail_test:
	stc			; flag failure
	ret

detect_PS2 endp

; check if port 92h fast method supported without BIOS or PS/2 test
;  shares enable/disable code with fast and PS/2

detect_port92 proc

	mov si,offset disable_enable_a20_fast
	call detect_and_handle_test
	ret

detect_port92 endp


detect_BIOS proc
	stc				; preset carry flag
	mov ax,2402h	; get gate status
	int 15h
	jc @@fail_test
	or ah,ah
	jne @@fail_test
	mov cl,al	; save status

	mov si,offset disable_enable_a20_BIOS
	call detect_and_handle_test
	ret
@@fail_test:
	stc			; flag failure
	ret

detect_BIOS endp


detect_KBC proc

	mov si,offset disable_enable_a20_KBC
	call detect_and_handle_test
	ret

detect_KBC endp

; upon entry si->disable/enable routine for a20 method being tested
; return carry set if failed, reset if success
;
detect_and_handle_test proc
	call test_a20
	setnz cl
	jz @@dah_2		; A20 disabled on entry

	push offset @@dah_2
	push ax
	mov ah,0
	jmp si			; try to disable A20

@@dah_2:
	call test_a20
	jnz @@dah_fail		; A20 not disabled

; try to enable A20 (always disabled at this point)
	push offset @@dah_3
	push ax
	mov ah,2
	jmp si

@@dah_3:
	call test_a20
	jz @@dah_fail		; A20 not enabled
	or cl,cl
	jne @@dah_success	; A20 was enabled on entry, done
	push offset @@dah_4	; disable to entry state
	push ax
	mov ah,0
	jmp si

@@dah_4:
	call test_a20
	jnz @@dah_fail		; A20 not disabled
@@dah_success:
	clc
	ret
@@dah_fail:
	stc
	ret
    
detect_and_handle_test endp 

;--- end of A20 code

; reserve size of routine checks

if size_disable_enable_a20_fast gt size_disable_enable_a20
	.err <disable_enable_a20_fast too long, increase buffer! >
endif
if size_disable_enable_a20_BIOS gt size_disable_enable_a20
	.err <disable_enable_a20_BIOS too long, increase buffer! >
endif
if size_disable_enable_a20_KBC gt size_disable_enable_a20
	.err <disable_enable_a20_KBC too long, increase buffer! >
endif

;--- set the a20 enable/disable code in the resident part
;--- out: NC if ok, C on errors

seta20method proc

; process forced methods

	mov al,[_method]
	cmp al,A20_ALWAYSON
	je @@is_alwayson
	cmp al,A20_BIOS
	je @@is_bios
	cmp al,A20_FAST
	je @@is_fast
	cmp al,A20_PS2
	je @@is_ps2
	cmp al,A20_KBC
	je @@is_kbc
	cmp al,A20_PORT92
	je @@is_port92

; check if the A20 line is on, if so assume it's always on
	call test_a20
	jz @@check_A20_method		; not on, try other methods

; use A20 always on code (dummy enable/disable A20 routine)
@@is_alwayson:
	mov dx,offset szAlwaysOn
	mov ah,9
	int 21h
	mov dx,offset szA20
	mov ah,9
	int 21h
	mov _method,A20_ALWAYSON
	jmp @@got_type

@@check_A20_method:

	call detect_fast; see if port 92h (2403h BIOS call) handler supported	
	jnc @@is_fast

	call detect_PS2	; see if port 92h (PS/2 signature) handler supported
	jnc @@is_ps2

	call detect_KBC	; see if KBC handler supported
	jnc @@is_kbc

; try BIOS here, demoted from first in line because unreliable BIOS
;  versions of A20 control exist

	call detect_BIOS	; see if BIOS A20 handler supported
	jnc @@is_bios

; see if fast port 92h handler supported without BIOS or PS/2 signature
;  leave this test until last because messing with port 92h is
;  reported to crash some machines which don't support that method
	call detect_port92
	jnc @@is_port92

; out of options to try, return error

	mov dx,offset MsgUnknownA20
	call dispmsg
	stc
	ret

@@is_bios:
	call flag_bios
	jmp @@got_type
@@is_fast:
	call flag_fast
	jmp @@got_type
@@is_ps2:
	call flag_ps2
	jmp @@got_type
@@is_kbc:
	call flag_kbc
	jmp @@got_type
@@is_port92:
	call flag_port92
@@got_type:
	clc
	ret

seta20method endp

;******************************************************************************
; strategy routine. is called by DOS to initialize the driver once.
; only thing to be done here is to store the address of the device driver
; request block.
; In:   ES:BX - address of request header
; Out:  nothing

strategy proc far
	mov @word cs:[request_ptr+0],bx  ; store offset addr
	mov @word cs:[request_ptr+2],es  ; store segment addr
	ret 				; far return here!
strategy endp

print_char proc
	pop cx
	pop dx
	push cx
	cmp dl,10
	jnz @@isnotlf
	mov dl,13
	mov ah,2
	int 21h
	mov dl,10
@@isnotlf:
	mov ah,2
	int 21h
	ret
print_char endp

;--- get the A20 method ("/METHOD:xxx")
;--- int _stdcall GetA20Method(char * pszMethod)

_GetA20Method proc

	pop cx
	pop ax	;get the pszMethod parameter
	push cx
	push si
	push di
	mov si,ax
	mov di,offset methods
	xor bx,bx
	push ds
	pop es
	cld
@@nextitem:
	mov cl,[di]
	mov ch,0
	jcxz @@notfound
	inc di
	pusha
@@nextchar:
	lodsb
	or al,20h
	scasb
	loopz @@nextchar
	popa
	jz @@found
	add di,cx
	inc bx
	jmp @@nextitem
@@notfound:
	or bx,-1
	jmp @@done
@@found:
	mov di,si
	add si,cx
@@nextchar2:
	lodsb
	stosb
	and al,al
	jnz @@nextchar2
@@done:
	mov ax,bx
	pop di
	pop si
	ret

_GetA20Method endp

;--- convert long to string
;--- assume SS!=DS
;--- assume psz onto stack!

ltob proc stdcall uses edi num:dword, psz:ptr, base:word

	mov ch,0
	movzx edi,base
	mov eax,num
	cmp di,-10
	jne @@ispositive
	mov di,10
	and eax,eax
	jns @@ispositive
	neg eax
	mov ch,'-'
@@ispositive:
	mov bx,psz
	add bx,10
	mov @byte ss:[bx],0
	dec bx
@@nextdigit:
	xor edx, edx
	div edi
	add dl,'0'
	cmp dl,'9'
	jbe @@isdigit
	add dl,7+20h
@@isdigit:
	mov ss:[bx],dl
	dec bx
	and eax, eax
	jne @@nextdigit
	cmp ch,0
	je @@nosign
	mov ss:[bx],ch
	dec bx
@@nosign:
	inc bx
	mov ax,bx
	ret

ltob endp

printf proc c uses si di fmt:ptr, args:vararg

local flag:byte
local longarg:byte
local size_:word
local fill:word
local szTmp[12]:byte

	lea di,args
nextfcharX:
	mov si,fmt
nextfchar:
	lodsb
	or al,al
	je done
	cmp al,'%'
	je isfspec
	push ax
	call print_char
	jmp nextfchar
done:
	xor ax,ax
	ret

isfspec:
	push nextfcharX
	xor dx,dx
	mov longarg,dl
	mov bl,1
	mov cl,' '
	cmp @byte [si],'-'
	jne @F
	dec bx
	inc si
@@:
	mov flag,bl
	cmp @byte [si],'0'
	jne @F
	mov cl,'0'
	inc si
@@:
	mov fill,cx
	mov bx,dx

nextdigit:
	cmp @byte [si],'0'
	jb digitsdone
	cmp @byte [si],'9'
	ja digitsdone
	lodsb
	sub al,'0'
	cbw
	imul bx,bx,10
	add bx,ax
	jmp nextdigit

digitsdone:
	mov size_,bx
	cmp @byte [si],'l'
	jne @F
	mov longarg,1
	inc si
@@:
	lodsb
	mov fmt,si
	cmp al,'x'
	je print_x
	cmp al,'X'
	je print_x
	cmp al,'c'
	je print_c
	cmp al,'d'
	je print_d
	cmp al,'i'
	je print_i
	cmp al,'u'
	je print_u
	cmp al,'s'
	je print_s
	and al,al
	jnz @F
	pop ax
	jmp done
print_c:
	mov ax,ss:[di]
	add di,2
@@:
	push ax
	call print_char
	retn
print_x:
	mov bx,16
	jmp print_number
print_d:
print_i:
	mov bx,-10
	jmp print_number
print_u:
	mov bx,10
print_number:
	cmp longarg,0
	je @F
	mov eax,ss:[di]
	add di,4
	jmp print_long
@@:
	movzx eax,@word ss:[di]
	add di,2
	cmp bx,0
	jge @F
	movsx eax,ax
@@:
print_long:
	lea cx,szTmp
	invoke ltob, eax, cx, bx
	mov si,ax
	push ds
	push ss
	pop ds
	call print_string
	pop ds
	retn

print_s:
	mov si,ss:[di]
	add di,2

print_string:
	mov bx,size_
	mov ax,si
	.while byte ptr [si]
		inc si
	.endw
	sub si,ax
	xchg ax,si
	sub bx,ax

	.if flag == 1
		.while sword ptr bx > 0
			push fill
			call print_char
			dec bx
		.endw
	.endif

	.while byte ptr [si]
		lodsb
		push ax
		call print_char
	.endw

	.while sword ptr bx > 0
		push fill
		call print_char
		dec bx
	.endw
	retn

printf endp


;--- skip "white space" characters

_skipWhite proc
nextitem:
	cmp @byte [si],' '
	je @F
	cmp @byte [si],9
	jne done
@@:
	inc si
	jmp nextitem
done:
	ret
_skipWhite endp

;--- must preserve BX!

_memicmp proc c uses si di psz1:word, psz2:word, len:word

	mov cx,len
	mov si,psz2
	mov di,psz1
	cld
@@nextitem:
	lodsb
	mov ah,[di]
	inc di
	or al,20h
	or ah,20h
	sub al,ah
	loopz @@nextitem
	cbw
	ret
_memicmp endp

;--- _stdcall toupper(char) returns uppercase character

_toupper proc
	pop cx
	pop ax
	push cx
	cmp al,'a'
	jb @@I290
	cmp al,'z'
	ja @@I290
	sub al,20h
@@I290:
	ret

_toupper endp

;--- convert a string into a DWORD, returned in EAX
;--- also accept suffix G, M, K and adjust value then

GetValue proc stdcall uses esi di commandline:word, base:word, usesuffix:word

	xor esi, esi			;result
	mov bx,commandline
nextchar:
	mov al,@byte [bx]
	cmp al,'0'
	jb @F
	cmp al,'9'
	ja @F
	sub al,'0'
	jmp @@I318
@@:
	call _toupper
	cmp al,'A'
	jl @@FB316
	sub al,55	;'A' -> 10
@@I318:
	movzx ecx, @word [base]
	cmp cl,al
	jle @@FB316
	xchg eax,esi
	mul ecx
	xchg eax,esi
	movzx eax,al
	add esi,eax
	inc bx
	jmp nextchar

@@FB316:
	cmp @byte [usesuffix],0
	je @@I322
	mov al,[bx]
	call _toupper
	cmp al,'M'
	je is_mega
	cmp al,'G'	;'G'
	je is_giga
	cmp al,'K'
	je is_kilo	;'K'
	jmp @@I322
is_giga:
	shl esi,10
is_mega:
	shl esi,10
is_kilo:
	mov @byte [bx],' '
@@I322:
	push esi
	mov si,bx
	mov di,commandline
	push ds
	pop es
@@nextchar:
	lodsb
	stosb
	and al,al
	jnz @@nextchar
	pop eax
	ret

GetValue endp

;--- char * _stdcall FindCommand(char * pszSearchString)
;--- parses the command line for a specific command.
;--- If found, the command is removed and
;--- the address behind that command is returned. Else, 0 is returned
;--- si=pszCmdLine

_FindCommand proc
	pop cx
	pop ax	;get pszSearchString
	push cx
	push di
	push si
    
	mov di,ax
	.while byte ptr [di]
		inc di
	.endw
	sub di,ax
	xchg ax,di
	mov bx,ax		;searchlen
;	mov si,[commandline]
@@F299:
	cmp @byte [si],0
	je @@FB301
	invoke _memicmp, si, di, bx
	or ax,ax
	je @@L384
	inc si
	jmp @@F299
@@L384:
	push si
	mov di,si
	add si,bx
	push ds
	pop es
@@nextitem:
	lodsb
	stosb
	and al,al
	jnz @@nextitem
	pop ax
	jmp @@EX297
@@FB301:
	xor ax,ax
@@EX297:
	pop si
	pop di
	ret

_FindCommand endp

;--- ParseCmdLine()
;--- in DS:SI -> cmdline

ParseCmdLine proc

	@DbgOutS <"ParseCmdLine enter",13,10>

	invoke printf, offset szStartup

if ?TESTMEM
	push offset szTESTMEMOFF
	call _FindCommand
	or ax,ax
	jne @F
;--- do something usefull here
@@:
endif

	push offset szVERBOSE
	call _FindCommand
	or ax,ax
	je @F
	mov _startup_verbose,1
@@:
if ?LOG
	push offset szLOG
	call _FindCommand
	or ax,ax
	je @F
	mov _xms_logging_enabled,1
@@:
endif
	cmp _startup_verbose,0
	je @F
	invoke printf, offset szINTERFACE
@@:

;--- option /NUMHANDLES
	push offset szNUMHANDLES
	call _FindCommand
	or ax,ax
	je no_numhandles
	invoke GetValue, ax, 10, 0
	mov _xms_num_handles,ax
	cmp _startup_verbose,0
	je @F
	invoke printf, offset szSelNumHandles, ax
@@:
	cmp _xms_num_handles,8
	jae @F
	invoke printf, offset szNumHandlesLim1
	mov _xms_num_handles,8
@@:
	cmp _xms_num_handles,MAXHANDLES
	jbe @F
	invoke printf, offset szNumHandlesLim2
	mov _xms_num_handles,MAXHANDLES
@@:
no_numhandles:

;--- option /NOABOVE16
	push offset szNOABOVE16
	call _FindCommand
	or ax,ax
	je @F
	mov _no_above_16,1
@@:

;--- option /X2MAX32
	push offset szX2MAX32
	call _FindCommand
	or ax,ax
	je @F
	mov _x2max32,32767	;7fffH
@@:

;--- option /X
	push offset szX
	call _FindCommand
	or ax,ax
	je @F
	mov _x_option,1
@@:

;--- option /METHOD
	push offset szMETHOD
	call _FindCommand
	or ax,ax
	je @F
	push ax
	call _GetA20Method
	mov _method,al
@@:

;--- option /MAX=
	push offset szMAX
	call _FindCommand
	or ax,ax
	je @F
	invoke GetValue, ax, 10, 1
	mov [_xms_max],eax
	cmp _startup_verbose,0
	je @F
	invoke printf, offset szMaximum, eax
@@:

;--- option /HMAMIN=
	push offset szHMAMIN
	call _FindCommand
	or ax,ax
	je no_hmamin
	invoke GetValue, ax, 10, 1
	mov _hma_min,ax
	cmp _startup_verbose,0
	je @F
	invoke printf, offset szMinimum, ax
@@:
	cmp _hma_min,63
	jbe @F
	invoke printf,offset szHMAMAX
	mov _hma_min,63
@@:
	shl _hma_min,10
no_hmamin:

	call _skipWhite
	cmp @byte [si],0
	je @F
	invoke printf, offset szignored, si
@@:

	@DbgOutS <"ParseCmdLine exit",13,10>
	xor ax,ax
	ret

ParseCmdLine endp

;******************************************************************************
; initializes the driver. called only once!
; may modify DI
; In:   DS:DI - pointer to init structure
; Out:  DS = DGROUP

DoCommandline proc

	mov ax,ss
	mov dx,sp

	push cs
	pop ss
	mov sp,offset _stacktop
	pusha
	push es
	lds si,[di].init_strc.cmd_line
@@:
	lodsb
	cmp al,20h
	ja @B
	dec si

	sub sp,128
	mov di,sp
	push ss
	pop es
	mov cx,128-1
@@nextitem2:
	lodsb
	cmp al,13
	jz @@done
	cmp al,10
	jz @@done
	stosb
	and al,al
	loopnz @@nextitem2
@@done:
	mov al,0
	stosb
	push ss
	pop ds	;DS=DGROUP

	mov si,sp
	call ParseCmdLine
	add sp,128
	pop es
	popa
;--- restore original stack, DOS requires it
	mov ss,ax
	mov sp,dx
	ret

DoCommandline endp

dispmsg proc
	push cs
	pop ds
	push dx
	mov dx,offset dHimem
	mov ah,9
	int 21h
	pop dx
	mov ah,9
	int 21h
	ret
dispmsg endp

;--- ds:si -> handle array
;--- edx = block addr in kB
;--- ecx = block size in kB

seti15handle proc c public

	cmp edx, 1024		;does the block start at 0x100000?
	jnz @F
	add edx, 64			;then exclude the first 64 kB for HMA
	sub ecx, 64
	jc exit
	or hma_exists,1
@@:
	sub _xms_max, ecx	;MAXEXT option set?
	jnc @F
	add ecx, _xms_max	;limit to maximum
	mov _xms_max,0
	jecxz exit
@@:
	add xms_mem_free, ecx
	lea eax,[edx+ecx]
	shl eax,10
	dec eax
	cmp eax, xms_highest_addr
	jb @F
	mov xms_highest_addr, eax
@@:
	mov [si].XMS_HANDLE.xh_flags, XMSF_FREE
	mov [si].XMS_HANDLE.xh_locks, 0
	mov [si].XMS_HANDLE.xh_baseK, edx
	mov [si].XMS_HANDLE.xh_sizeK, ecx
	add si, sizeof XMS_HANDLE
exit:
	ret
seti15handle endp

;-- look for extended memory, int 15h, ax/ah 0e820h -> 0e801h -> 88h
;-- in: ds:si->handle array
;-- updates variable xms_mem_free
;-- modifies eax, ebx, ecx, edx, si, di

geti15mem proc

local mmap:E820MAP

	cmp [_x_option],0
	jne @@e801_check   ; cannot use 0e820h, per user /X command

	@DbgOutS <"get15mem: get extended memory with int 15, E820",13,10>

; try 0e820h first

	xor ebx,ebx
	push ss
	pop es

e820_nextitem:   ; ebx offset is updated with each successive int 15h

	mov edx,SMAP
	mov ecx, sizeof E820MAP
	lea di, mmap
	xor eax,eax
	mov mmap.baselow,eax   ; insurance against buggy BIOS
	mov mmap.type_,eax
	mov mmap.lenlow,eax
	mov ax,0e820h
	clc
	int 15h
	setc dl 		; keep carry flag status
	cmp eax,SMAP
	jne e820_bad	; failure
	cmp dl,1
	je e820_done ; CF doesn't have to signal fail, can just mean done

	cmp ecx,sizeof E820MAP	; didn't return all the info needed, assume done
	jb e820_done

	cmp mmap.type_,1	; memory available to OS
	jne e820_itemdone
	cmp mmap.basehigh, 0;ignore memory beyond 4 GB for now
	jnz e820_itemdone
	mov edx, mmap.baselow
	cmp edx, 100000h ; has to live in extended memory
	jb e820_itemdone
	
	mov ecx, mmap.lenlow
	shr ecx, 10
	shr edx, 10
	call seti15handle	;set xms block, sizeK in ECX, baseK in EDX
e820_itemdone:
	cmp ebx,0		;was this the last entry?
	jnz e820_nextitem
e820_bad:
e820_done:
	cmp xms_mem_free,0
	jnz @@exit

; try 0e801h, but set up the registers to fail status because not
;  all BIOS's properly return the carry flag on failure
@@e801_check:
	cmp [_no_above_16],0
	jne @@try_88h	; cannot use 0e801h, per user /NOABOVE16 command

	@DbgOutS <"geti15mem: get extended memory with int 15, E801",13,10>

	xor ax,ax
	mov bx,ax
	mov cx,ax
	mov dx,ax
	mov ax,0e801h
	int 15h
	jc @@try_88h
	mov ax,cx
	or ax,dx
	je @@try_88h

; if dx is > 0, then cx should be 3c00h since that's full 1-16M range
;  if cx != 3c00h use cx and not dx
	cmp cx,3c00h
	je @@e801_compute
	cmp dx,0
	je @@e801_compute
	xor dx,dx

@@e801_compute:
	movzx edx,dx
	shl edx,6			; convert 64K blocks to 1K
	movzx eax,cx
	add eax,edx
	cmp eax,64		; only use if useful amount
	ja @@done

; e801h didn't do the trick, fall back to old 88h with 64M max
@@try_88h:

	@DbgOutS <"geti15mem: get extended memory with int 15, 88",13,10>

	clc
	mov ah,88h
	int 15h
	movzx eax,ax
@@done:
	mov edx, 1024
	mov ecx, eax
	call seti15handle
@@exit:
	ret
geti15mem endp


;--- driver init. this proc should be last
;--- since it initializes the handle table
;--- ds:di = request_ptr

initialize proc

	pushf
	pushad
	cld
	@DbgOutS <"initialize enter",13,10>

	mov ax,3000h		; get DOS version number
	int 21h
	cmp al,3h			; we need at least 3.00
	jnc @@dosok
	mov dx,offset old_dos
@@error_exit:
	call dispmsg
	mov dx,offset error_msg
	mov ah,9
	int 21h
	popad
	popf
	ret
@@dosok:
	mov ax,4300h			; check if XMS is already
	int 2fh 				; installed
	cmp al,80h
	mov dx,offset xms_twice
	je @@error_exit

	call DoCommandline		; parse commandline

;--- now DS=DGROUP

	@DbgOutS <"initialize: processing selected A20 method",13,10>

	call seta20method			; modifies SI!
	mov dx,offset a20_error
	jc @@error_exit

	call _install_check_vdisk	; is VDISK installed?
	mov dx,offset vdisk_detected
	jz @@error_exit


	mov ax,cs				; setup descriptors
;	 mov [code_seg],ax		; eliminate relocation entry
	movzx eax,ax
	shl eax,4
;	 or @dword [code16dsc+2],eax
	add @dword [gdt32+2],eax

	mov ax,352Fh			; getvect --> es:bx
	int 21h
	mov @word [old_int2f+0],bx
	mov @word [old_int2f+2],es
	mov ax,3515h			; getvect --> es:bx
	int 21h
	mov @word [old_int15+0],bx
	mov @word [old_int15+2],es

	; *****************  handle LOG mode

	mov si, offset trace_driver_end
ifdef _DEBUG
	jmp @F
else
if ?LOG
	cmp [_xms_logging_enabled],0
	jne @F
endif
endif

if ?LOG
	mov di, offset dispatcher_log_entry
	lea si, [di+3]
	mov cx, offset dispatcher_log_exit - (offset dispatcher_log_entry + 2)
	mov [dispatcher_log_exit],0cbh	; patch call to RETF
	push ds
	pop es
	rep movsb
	mov si, di
	add si, 3
	and si, not 3
@@:
endif
	mov @word xms_handle_table.xht_pArray+0, si
	mov @word xms_handle_table.xht_pArray+2, ds

	@DbgOutS <"initialize: init handle array",13,10>

	call geti15mem				; look for extended memory via int 15h
	cmp hma_exists,1
	mov dx,offset xms_toosmall
	jnz @@error_exit

; we clear the handle table, as this may overwrite part of the code above
; but must not erase itself

IF ($ - startoftext) le MAXHANDLES * sizeof XMS_HANDLE

	.err <this is an error! reserve some space after driver end ~!!>

ENDIF                

	mov ax, _xms_num_handles
	mov xms_handle_table.xht_numhandles,ax
	mov cx,sizeof XMS_HANDLE
	mul cx
	add ax,@word xms_handle_table.xht_pArray+0
	mov bx,ax
	xor eax,eax
@@:
	mov [si].XMS_HANDLE.xh_flags,XMSF_INPOOL  ; handle not used
	mov [si].XMS_HANDLE.xh_locks,al 	 ; clear locks
	mov [si].XMS_HANDLE.xh_baseK,eax
	mov [si].XMS_HANDLE.xh_sizeK,eax
	add si,sizeof XMS_HANDLE
	cmp si,bx
	jb @B

	@DbgOutS <"initialize: set int vectors 15h and 2Fh",13,10>

	mov ax,252Fh			; install own INT2Fh
	mov dx,offset int2f_handler
	int 21h
	mov ax,2515h			; install own INT15h
	mov dx,offset int15_handler
	int 21h

; driver init done

	les di,[request_ptr]
	mov @word es:[di+0].init_strc.end_addr,si
	mov @word es:[di+2].init_strc.end_addr,cs	; set end address
	mov es:[di].request_hdr.status,STATUS_OK	; we're alright

@@exit:
	@DbgOutS <"initialize exit",13,10>
	popad
	popf
	ret
initialize endp

;******************************************************************************
; init_interrupt routine. called by DOS right after the strategy routine to
; process the incoming job. also used to initialize the driver.

init_interrupt proc far

	push di
	push ds
	@DbgOutS <"init interrupt enter",13,10>

	lds di,cs:[request_ptr]		; load address of request header

	cmp [di].request_hdr.cmd,CMD_INIT; do we have to initialize?
	jne @@done
	mov @word [di].init_strc.end_addr+0,0   ; init to error
	mov @word [di].init_strc.end_addr+2,cs
	mov [di].request_hdr.status, STATUS_BAD
	call check_cpu				; do we have at least a 386?
	jz @F
	mov dx,offset no_386
	call dispmsg
	jmp @@done
@@:
	push es
	call initialize
	pop es
	mov @word cs:[6], offset dummyretf; new strategy offset
	mov @word cs:[8], offset dummyretf; new interrupt offset
@@done:
;	lds si,[request_ptr]		; return this to DOS (why?)

	@DbgOutS <"init interrupt exit",13,10>
	pop ds
	pop di
	ret
init_interrupt endp

;--- startpoint when executing as EXE

startexe proc
	push cs
	pop ds
	invoke printf, offset szStartup
	invoke printf, offset szHello
	mov ah,04ch
	int 21h
startexe endp

_TEXT ends

	end startexe
