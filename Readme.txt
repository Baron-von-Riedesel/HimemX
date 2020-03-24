
  1. About

  HimemX is a XMS memory manager. It's derived from FreeDOS Himem 
  (short: FDHimem) with bugfixes, optimizations and extensions.
  To see its usage just run HIMEMX.EXE


  2. HimemX and HimemX2
 
  Currently there are 2 versions of HimemX supplied, HimemX and HimemX2.
  The differences are:

   a) HimemX2: when allocating an EMB, a new handle is allocated for the EMB 
      that will be returned to the caller.
      HimemX: a new handle is allocated and will get the rest of the memory 
      block that has "supplied" the memory. The caller will receive the "old"
      handle of this block. As a consequence, the block "moves" to the end of
      the handle array. This makes a difference for subsequent allocations, 
      if extended memory is scattered.
   b) HimemX2: when an EMB is to be increased, it is checked whether the
      successor is a "free" block and if its size is large enough to satisfy
      the request. If so, the successor's size is reduced and the EMB is
      enlarged. 
      HimemX: uses a very simple (and dull) strategy: a temporary handle is 
      allocated, with the requested size; then the content is copied. Since
      the handle provided by the caller must not change, the contents of both
      handles are exchanged. Finally the temporarily allocated handle is 
      released again.
   c) HimemX2: a request for a block with size 0 may return a handle with
      size > 0 if no more unused handles are available.
      HimemX: a request for a block with size 0 will fail if no unused handles
      are available.

  As for MS Himem, for b) and c) it follows HimemX2, while for a) it behaves
  like HimemX.
  
   HimemX2 is a test only. In the end, it is planned to activate at least b)
  and c) in the standard (in HimemX).
   At least b) is a true bug in HimemX. Example: if there's a pool of 64 MB of
  extended memory and 32 MB have been allocated, a try to increase the 
  allocated block to 34 MB will fail, because HimemX's strategy implies that
  there's enough space for both the old and new size at the same time.
   As for a), there's no decision yet. The new strategy has the advantage that 
  it is virtually ensured that memory is allocated from "bottom" to "top" if
  memory is scattered. This is good for Jemm386, because it needs its DMA
  buffer to reside in the first 16 MB of memory.


  3. License
  
  All changes done for HimemX are Public Domain. However, since HimemX is
  derived from FD Himem, the FD Himem copyrights do apply. FD Himem is
  copyright Till Gerken and Tom Ehlert, with GPL and/or Artistic license.

  UMBM.EXE is Public Domain. The source code UMBM.ASM is supplied with
  Jemm.

  Japheth
