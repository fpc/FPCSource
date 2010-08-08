(**
 * Summary: interfaces for thread handling
 * Description: set of generic threading related routines
 *              should work with pthreads, Windows native or TLS threads
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF POINTER}
  xmlMutexPtr = ^xmlMutex;
  xmlRMutexPtr = ^xmlRMutex;
{$ENDIF}

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
function xmlNewMutex: xmlMutexPtr; EXTDECL; external xml2lib;
procedure xmlMutexLock(tok: xmlMutexPtr); EXTDECL; external xml2lib;
procedure xmlMutexUnlock(tok: xmlMutexPtr); EXTDECL; external xml2lib;
procedure xmlFreeMutex(tok: xmlMutexPtr); EXTDECL; external xml2lib;

function xmlNewRMutex: xmlRMutexPtr; EXTDECL; external xml2lib;
procedure xmlRMutexLock(tok: xmlRMutexPtr); EXTDECL; external xml2lib;
procedure xmlRMutexUnlock(tok: xmlRMutexPtr); EXTDECL; external xml2lib;
procedure xmlFreeRMutex(tok: xmlRMutexPtr); EXTDECL; external xml2lib;

(*
 * Library wide APIs.
 *)
procedure xmlInitThreads; EXTDECL; external xml2lib;
procedure xmlLockLibrary; EXTDECL; external xml2lib;
procedure xmlUnlockLibrary; EXTDECL; external xml2lib;

function xmlGetThreadId: cint; EXTDECL; external xml2lib;
function xmlIsMainThread: cint; EXTDECL; external xml2lib;
procedure xmlCleanupThreads; EXTDECL; external xml2lib;
function xmlGetGlobalState: xmlGlobalStatePtr; EXTDECL; external xml2lib;

{$IF defined(HAVE_WIN32_THREADS) and not defined(HAVE_COMPILER_TLS) and defined(LIBXML_STATIC_FOR_DLL)}
//int EXTDECL xmlDllMain(void *hinstDLL, unsigned long fdwReason, void *lpvReserved);
{$ENDIF}

{$ENDIF}