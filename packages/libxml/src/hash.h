(*
 * Summary: Chained hash tables
 * Description: This module implements the hash table support used in 
 * 		various places in the library.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Bjorn Reese <bjorn.reese@systematic.dk>
 *)

{$IFDEF TYPE}
(*
 * The hash table.
 *)
  xmlHashTable = record
  end;


(*
 * Recent version of gcc produce a warning when a function pointer is assigned
 * to an object pointer, or vice versa.  The following macro is a dirty hack
 * to allow suppression of the warning.  If your architecture has function
 * pointers which are a different size than a void pointer, there may be some
 * serious trouble within the library.
 *)

(*
 * function types:
 *)
(**
 * xmlHashDeallocator:
 * @payload:  the data in the hash
 * @name:  the name associated
 *
 * Callback to free data from a hash.
 *)
  xmlHashDeallocator = procedure(payload: pointer; name: xmlCharPtr); cdecl;

(**
 * xmlHashCopier:
 * @payload:  the data in the hash
 * @name:  the name associated
 *
 * Callback to copy data from a hash.
 *
 * Returns a copy of the data or NULL in case of error.
 *)
  xmlHashCopier = function(payload: pointer; name: xmlCharPtr): pointer; cdecl;

(**
 * xmlHashScanner:
 * @payload:  the data in the hash
 * @data:  extra scannner data
 * @name:  the name associated
 *
 * Callback when scanning data in a hash with the simple scanner.
 *)
  xmlHashScanner = procedure(payload: pointer; data: pointer; name: xmlCharPtr); cdecl;

(**
 * xmlHashScannerFull:
 * @payload:  the data in the hash
 * @data:  extra scannner data
 * @name:  the name associated
 * @name2:  the second name associated
 * @name3:  the third name associated
 *
 * Callback when scanning data in a hash with the full scanner.
 *)
  xmlHashScannerFull = procedure(payload: pointer; data: pointer; name, name2, name3: xmlCharPtr); cdecl;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * Constructor and destructor.
 *)
function xmlHashCreate(size: cint): xmlHashTablePtr; cdecl; external;
function xmlHashCreateDict(size: cint; dict: xmlDictPtr): xmlHashTablePtr; cdecl; external;
procedure xmlHashFree(table: xmlHashTablePtr; f: xmlHashDeallocator); cdecl; external;

(*
 * Add a new entry to the hash table.
 *)
function xmlHashAddEntry(table: xmlHashTablePtr; name: xmlCharPtr; userdata: pointer): cint; cdecl; external;
function xmlHashUpdateEntry(table: xmlHashTablePtr; name: xmlCharPtr; userdata: pointer; f: xmlHashDeallocator): cint; cdecl; external;
function xmlHashAddEntry2(table: xmlHashTablePtr; name, name2: xmlCharPtr; userdata: pointer): cint; cdecl; external;
function xmlHashUpdateEntry2(table: xmlHashTablePtr; name, name2: xmlCharPtr; userdata: pointer; f: xmlHashDeallocator): cint; cdecl; external;
function xmlHashAddEntry3(table: xmlHashTablePtr; name, name2, name3: xmlCharPtr; userdata: pointer): cint; cdecl; external;
function xmlHashUpdateEntry2(table: xmlHashTablePtr; name, name2, name3: xmlCharPtr; userdata: pointer; f: xmlHashDeallocator): cint; cdecl; external;

(*
 * Remove an entry from the hash table.
 *)
function xmlHashRemoveEntry(table: xmlHashTablePtr; name: xmlCharPtr; f: xmlHashDeallocator): cint; cdecl; external;
function xmlHashRemoveEntry2(table: xmlHashTablePtr; name, name2: xmlCharPtr; f: xmlHashDeallocator): cint; cdecl; external;
function xmlHashRemoveEntry3(table: xmlHashTablePtr; name, name2, name3: xmlCharPtr; f: xmlHashDeallocator): cint; cdecl; external;

(*
 * Retrieve the userdata.
 *)
function xmlHashRemoveEntry(table: xmlHashTablePtr; name: xmlCharPtr): pointer; cdecl; external;
function xmlHashRemoveEntry2(table: xmlHashTablePtr; name, name2: xmlCharPtr): pointer; cdecl; external;
function xmlHashRemoveEntry3(table: xmlHashTablePtr; name, name2, name3: xmlCharPtr): pointer; cdecl; external;
function xmlHashQLookup(table: xmlHashTablePtr; name, prefix: xmlCharPtr): pointer; cdecl; external;
function xmlHashQLookup2(table: xmlHashTablePtr; name, prefix, name2, prefix2: xmlCharPtr): pointer; cdecl; external;
function xmlHashQLookup3(table: xmlHashTablePtr; name, prefix, name2, prefix2, name3, prefix3: xmlCharPtr): pointer; cdecl; external;

(*
 * Helpers.
 *)
function xmlHashCopy(table: xmlHashTablePtr; f: xmlHashCopier): xmlHashTablePtr; cdecl; external;
function xmlHashSize(table: xmlHashTablePtr): cint; cdecl; external;
procedure xmlHashScan(table: xmlHashTablePtr; f: xmlHashScanner; data: pointer); cdecl; external;
procedure xmlHashScan(table: xmlHashTablePtr; name, name2, name3: xmlCharPtr; f: xmlHashScanner; data: pointer); cdecl; external;
procedure xmlHashScanFull(table: xmlHashTablePtr; f: xmlHashScannerFull; data: pointer); cdecl; external;
procedure xmlHashScanFull(table: xmlHashTablePtr; name, name2, name3: xmlCharPtr; f: xmlHashScannerFull; data: pointer); cdecl; external;
{$ENDIF}