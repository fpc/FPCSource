(*
 * Summary: interface for the memory allocator
 * Description: provides interfaces for the memory allocator,
 *              including debugging capabilities.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

(**
 * DEBUG_MEMORY:
 *
 * DEBUG_MEMORY replaces the allocator with a collect and debug
 * shell to the libc allocator.
 * DEBUG_MEMORY should only be activated when debugging 
 * libxml i.e. if libxml has been configured with --with-debug-mem too.
 *)
{.$DEFINE DEBUG_MEMORY_FREED}
{.$DEFINE DEBUG_MEMORY_LOCATION}

{$IFDEF DEBUG}
{$IFNDEF DEBUG_MEMORY}
{$DEFINE DEBUG_MEMORY}
{$ENDIF}
{$ENDIF}

(**
 * DEBUG_MEMORY_LOCATION:
 *
 * DEBUG_MEMORY_LOCATION should be activated only when debugging 
 * libxml i.e. if libxml has been configured with --with-debug-mem too.
 *)
{$IFDEF DEBUG_MEMORY_LOCATION}
{$ENDIF}

{$IFDEF TYPE}
(*
 * The XML memory wrapper support 4 basic overloadable functions.
 *)
(**
 * xmlFreeFunc:
 * @mem: an already allocated block of memory
 *
 * Signature for a free() implementation.
 *)
  xmlFreeFunc = procedure(mem: pointer); XMLCALL;

(**
 * xmlMallocFunc:
 * @size:  the size requested in bytes
 *
 * Signature for a malloc() implementation.
 *
 * Returns a pointer to the newly allocated block or NULL in case of error.
 *)
  xmlMallocFunc = function(size: size_t): pointer; XMLCALL;

(**
 * xmlReallocFunc:
 * @mem: an already allocated block of memory
 * @size:  the new size requested in bytes
 *
 * Signature for a realloc() implementation.
 *
 * Returns a pointer to the newly reallocated block or NULL in case of error.
 *)
  xmlReallocFunc = function(mem: pointer; size: size_t): pointer; XMLCALL;

(**
 * xmlStrdupFunc:
 * @str: a zero terminated string
 *
 * Signature for an strdup() implementation.
 *
 * Returns the copy of the string or NULL in case of error.
 *)
  xmlStrdupFunc = function(str: pchar): pchar; XMLCALL;

(*
 * The 4 interfaces used for all memory handling within libxml.
LIBXML_DLL_IMPORT extern xmlFreeFunc xmlFree;
LIBXML_DLL_IMPORT extern xmlMallocFunc xmlMalloc;
LIBXML_DLL_IMPORT extern xmlMallocFunc xmlMallocAtomic;
LIBXML_DLL_IMPORT extern xmlReallocFunc xmlRealloc;
LIBXML_DLL_IMPORT extern xmlStrdupFunc xmlMemStrdup;
 *)
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * The way to overload the existing functions.
 * The xmlGc function have an extra entry for atomic block
 * allocations useful for garbage collected memory allocators
 *)
function xmlMemSetup(freeFunc: xmlFreeFunc; mallocFunc: xmlMallocFunc; reallocFunc: xmlReallocFunc; strdupFunc: xmlStrdupFunc): cint; XMLCALL; XMLPUBFUN;
function xmlMemGet(var freeFunc: xmlFreeFunc; var mallocFunc: xmlMallocFunc; var reallocFunc: xmlReallocFunc; var strdupFunc: xmlStrdupFunc): cint; XMLCALL; XMLPUBFUN;
function xmlGcMemSetup(freeFunc: xmlFreeFunc; mallocFunc: xmlMallocFunc; mallocAtomicFunc: xmlMallocFunc; reallocFunc: xmlReallocFunc; strdupFunc: xmlStrdupFunc): cint; XMLCALL; XMLPUBFUN;
function xmlGcMemGet(var freeFunc: xmlFreeFunc; var mallocFunc: xmlMallocFunc; var mallocAtomicFunc: xmlMallocFunc; var reallocFunc: xmlReallocFunc; var strdupFunc: xmlStrdupFunc): cint; XMLCALL; XMLPUBFUN;

(*
 * Initialization of the memory layer.
 *)
function xmlInitMemory(): cint; XMLCALL; XMLPUBFUN;

(* 
 * Cleanup of the memory layer.
 *)
procedure xmlCleanupMemory(); XMLCALL; XMLPUBFUN;

(*
 * These are specific to the XML debug memory wrapper.
 *)
function xmlMemUsed(): cint; XMLCALL; XMLPUBFUN;
function xmlMemBlocks(): cint; XMLCALL; XMLPUBFUN;
{XMLPUBFUN void XMLCALL	xmlMemDisplay	(FILE *fp);
XMLPUBFUN void XMLCALL	xmlMemShow	(FILE *fp, int nr);}
procedure xmlMemoryDump(); XMLCALL; XMLPUBFUN;
function xmlMemMalloc(size: size_t): pointer; XMLCALL; XMLPUBFUN;
function xmlMemRealloc(ptr: pointer; size: size_t): pointer; XMLCALL; XMLPUBFUN;
procedure xmlMemFree(ptr: pointer); XMLCALL; XMLPUBFUN;
function xmlMemoryStrdup(str: pchar): pchar; XMLCALL; XMLPUBFUN;

function xmlMallocLoc(size: size_t; _file: pchar; line: cint): pointer; XMLCALL; XMLPUBFUN;
function xmlReallocLoc(ptr: pointer; size: size_t; _file: pchar; line: cint): pointer; XMLCALL; XMLPUBFUN;
function xmlMallocAtomicLoc(size: size_t; _file: pchar; line: cint): pointer; XMLCALL; XMLPUBFUN;
function xmlMemStrdupLoc(str: pchar; _file: pchar; line: cint): pchar; XMLCALL; XMLPUBFUN;

{$IFDEF DEBUG_MEMORY_LOCATION}
(**
 * xmlMalloc:
 * @size:  number of bytes to allocate
 *
 * Wrapper for the malloc() function used in the XML library.
 *
 * Returns the pointer to the allocated area or NULL in case of error.
 *)
//#define xmlMalloc(size) xmlMallocLoc((size), __FILE__, __LINE__)

(**
 * xmlMallocAtomic:
 * @size:  number of bytes to allocate
 *
 * Wrapper for the malloc() function used in the XML library for allocation
 * of block not containing pointers to other areas.
 *
 * Returns the pointer to the allocated area or NULL in case of error.
 *)
//#define xmlMallocAtomic(size) xmlMallocAtomicLoc((size), __FILE__, __LINE__)

(**
 * xmlRealloc:
 * @ptr:  pointer to the existing allocated area
 * @size:  number of bytes to allocate
 *
 * Wrapper for the realloc() function used in the XML library.
 *
 * Returns the pointer to the allocated area or NULL in case of error.
 *)
//#define xmlRealloc(ptr, size) xmlReallocLoc((ptr), (size), __FILE__, __LINE__)

(**
 * xmlMemStrdup:
 * @str:  pointer to the existing string
 *
 * Wrapper for the strdup() function, xmlStrdup() is usually preferred.
 *
 * Returns the pointer to the allocated area or NULL in case of error.
 *)
//#define xmlMemStrdup(str) xmlMemStrdupLoc((str), __FILE__, __LINE__)

{$ENDIF} (* DEBUG_MEMORY_LOCATION *)
{$ENDIF}