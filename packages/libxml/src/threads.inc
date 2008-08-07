(**
 * Summary: interfaces for thread handling
 * Description: set of generic threading related routines
 *              should work with pthreads, Windows native or TLS threads
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF TYPE}
(*
 * xmlMutex are a simple mutual exception locks.
 *)
  xmlMutex = record end;

(*
 * xmlRMutex are reentrant mutual exception locks.
 *)
  xmlRMutex = record end;
{$ENDIF}

{$IFDEF FUNCTION}
function xmlNewMutex: xmlMutexPtr; XMLCALL; XMLPUBFUN;
procedure xmlMutexLock(tok: xmlMutexPtr); XMLCALL; XMLPUBFUN;
procedure xmlMutexUnlock(tok: xmlMutexPtr); XMLCALL; XMLPUBFUN;
procedure xmlFreeMutex(tok: xmlMutexPtr); XMLCALL; XMLPUBFUN;

function xmlNewRMutex: xmlRMutexPtr; XMLCALL; XMLPUBFUN;
procedure xmlRMutexLock(tok: xmlRMutexPtr); XMLCALL; XMLPUBFUN;
procedure xmlRMutexUnlock(tok: xmlRMutexPtr); XMLCALL; XMLPUBFUN;
procedure xmlFreeRMutex(tok: xmlRMutexPtr); XMLCALL; XMLPUBFUN;

(*
 * Library wide APIs.
 *)
procedure xmlInitThreads; XMLCALL; XMLPUBFUN;
procedure xmlLockLibrary; XMLCALL; XMLPUBFUN;
procedure xmlUnlockLibrary; XMLCALL; XMLPUBFUN;

function xmlGetThreadId: cint; XMLCALL; XMLPUBFUN;
function xmlIsMainThread: cint; XMLCALL; XMLPUBFUN;
procedure xmlCleanupThreads; XMLCALL; XMLPUBFUN;
function xmlGetGlobalState: xmlGlobalStatePtr; XMLCALL; XMLPUBFUN;

{$IF defined(HAVE_WIN32_THREADS) and not defined(HAVE_COMPILER_TLS) and defined(LIBXML_STATIC_FOR_DLL)}
//int XMLCALL xmlDllMain(void *hinstDLL, unsigned long fdwReason, void *lpvReserved);
{$ENDIF}

{$ENDIF}