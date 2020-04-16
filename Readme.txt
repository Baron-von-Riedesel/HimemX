
  1. About

  HimemX is a XMS memory manager. It's derived from FreeDOS Himem 
  (short: FDHimem) with bugfixes, optimizations and extensions.
  To see its usage just run HIMEMX.EXE from the command line.


  2. HimemX and HimemX2
 
  Currently there are 2 versions of HimemX supplied, HimemX and HimemX2.
  HimemX2 uses a different strategy when it comes to extended memory block
  allocations. This difference is only relevant if extended memory is 
  scattered in multiple blocks, as it is the case on newer machines.
  Then, generally, HimemX2 tends to allocate memory blocks from low addresses
  to high addresses, while the addresses of memory blocks allocated by
  HimemX are a "wild" mix.

   Since the allocation strategy of HimemX is the one also used by MS Himem,
  it is probably to be prefered. OTOH, MS Himem is from a time when extended
  memory wasn't scattered, so this argument hasn't much weight - there are
  no known incompatibilities of HimemX2.

   In any case, HimemX2 most likely is a cure if Jemm386 reports 
  "Warning: address of allocated EMB (=xxxxxxxx) is beyond 16 MB".
  This warning indicates that Jemm386 couldn't locate its DMA buffer in an
  address range where ISA DMA is working. ISA DMA is used by the floppy
  controller and may be used by the parallel port or ISA expansion cards.
  These are now virtually obsolete devices, but to be sure, use HimemX2 if
  you get the warning mentioned above - the warning is displayed by 
  Jemm386 v5.80+ only.


  3. UMBM

  To find out the purpose of UMBM.EXE, just run it from the command line.


  4. License
  
  All changes done for HimemX are Public Domain. However, since HimemX is
  derived from FD Himem, the FD Himem copyrights do apply. FD Himem is
  copyright Till Gerken and Tom Ehlert, with GPL and/or Artistic license.

  UMBM.EXE is Public Domain. The source code UMBM.ASM is supplied with
  Jemm.

  Japheth
