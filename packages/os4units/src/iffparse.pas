{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by Free Pascal development team

    iffparse.library functions for Amiga OS 4.x

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$PACKRECORDS 2}

unit iffparse;

interface

uses
  exec, clipboard, utility;


// Struct associated with an active IFF stream. "iff_Stream" is a value used by the client's read/write/seek functions -
// it will not be accessed by the library itself and can have any value (could even be a pointer or a BPTR).
// This structure can only be allocated by iffparse.library
type
  PIFFHandle = ^TIFFHandle;
  TIFFHandle = record
    iff_Stream: LongWord;
    iff_Flags: LongWord;
    iff_Depth: LongInt;   //  Depth of context stack.
  end;

// Bit masks for "iff_Flags" field.
const
  IFFF_READ     = 0;                      // read mode - default
  IFFF_WRITE    = 1;                      // write mode
  IFFF_RWBITS   = IFFF_READ + IFFF_WRITE; // read/write bits
  IFFF_FSEEK    = 1 shl 1;                // forward seek only
  IFFF_RSEEK    = 1 shl 2;                // random seek
  IFFF_RESERVED = $FFFF0000;              // Don't touch these bits.

// When the library calls your stream handler, you'll be passed a pointer to this structure as the "message packet".
type
  PIFFStreamCmd = ^TIFFStreamCmd;
  TIFFStreamCmd = record
    sc_Command: LongInt;  //  Operation to be performed (IFFCMD_)
    sc_Buf: APTR;         //  Pointer to data buffer
    sc_NBytes: LongInt;   //  Number of bytes to be affected
  end;

// A node associated with a context on the iff_Stack.  Each node represents a chunk, the stack representing the current nesting of chunks in the
// open IFF file.  Each context node has associated local context items in the (private) LocalItems list.
// The ID, type, size and scan values describe the chunk associated with this node.
// This structure can only be allocated by iffparse.library
  PContextNode = ^TContextNode;
  TContextNode = record
    cn_Node: TMinNode;
    cn_ID: LongInt;
    cn_Type: LongInt;
    cn_Size: LongInt; //  Size of this chunk
    cn_Scan: LongInt; //  # of bytes read/written so far
  end;

// Local context items live in the ContextNode's.  Each class is identified by its lci_Ident code and has a (private) purge vector for when the
// parent context node is popped. This structure can only be allocated by iffparse.library
  PLocalContextItem = ^TLocalContextItem;
  TLocalContextItem = record
    lci_Node: TMinNode;
    lci_ID: LongWord;
    lci_Type: LongWord;
    lci_Ident: LongWord;
  end;
// StoredProperty: a local context item containing the data stored from a previously encountered property chunk.
  PStoredProperty = ^TStoredProperty;
  TStoredProperty = record
    sp_Size: LongInt;
    sp_Data: APTR;
  end;

// Collection Item: the actual node in the collection list at which client will look.
// The next pointers cross context boundaries so that the complete list is accessable.
  PCollectionItem = ^TCollectionItem;
  TCollectionItem = record
    ci_Next: PCollectionItem;
    ci_Size: LongInt;
    ci_Data: APTR;
  end;
// Structure returned by OpenClipboard().  You may do CMD_POSTs and such using this structure.
// However, once you call OpenIFF(), you may not do any more of your own I/O to the clipboard until you call CloseIFF().
  PClipboardHandle = ^TClipBoardHandle;
  TClipboardHandle = record
    cbh_Req: TIOClipReq;
    cbh_CBport: TMsgPort;
    cbh_SatisfyPort: TMsgPort;
  end;

const
// IFF return codes.  Most functions return either zero for success or one of these codes.  The exceptions are the read/write functions which
// return positive values for number of bytes or records read or written, or a negative error code.
// Some of these codes are not errors per sae, but valid conditions such as EOF or EOC (End of Chunk).
  IFFERR_EOF        = -1;  // Reached logical END of file
  IFFERR_EOC        = -2;  // About to leave context
  IFFERR_NOSCOPE    = -3;  // No valid scope for property
  IFFERR_NOMEM      = -4;  // Internal memory alloc failed
  IFFERR_READ       = -5;  // Stream read error
  IFFERR_WRITE      = -6;  // Stream write error
  IFFERR_SEEK       = -7;  // Stream seek error
  IFFERR_MANGLED    = -8;  // Data in file is corrupt
  IFFERR_SYNTAX     = -9;  // IFF syntax error
  IFFERR_NOTIFF     = -10; //  Not an IFF file
  IFFERR_NOHOOK     = -11; //  No call-back hook provided
  IFF_RETURN2CLIENT = -12; //  Client handler normal return

// Universal IFF identifiers.
  ID_FORM = 1179603533; // 'FORM'
  ID_LIST = 1279873876; // 'LIST'
  ID_CAT  = 1128354848; // 'CAT '
  ID_PROP = 1347571536; // 'PROP'
  ID_NULL = 538976288;  // '    '
  // Ident codes for universally recognized local context items.
  IFFLCI_PROP         = 1886547824; // 'prop'
  IFFLCI_COLLECTION   = 1668246636; // 'coll'
  IFFLCI_ENTRYHANDLER = 1701734500; // 'enhd'
  IFFLCI_EXITHANDLER  = 1702389860; // 'exhd'

// Control modes for ParseIFF() function.
  IFFPARSE_SCAN    = 0;
  IFFPARSE_STEP    = 1;
  IFFPARSE_RAWSTEP = 2;

// Control modes for StoreLocalItem() function
  IFFSLI_ROOT = 1; //  Store in default context
  IFFSLI_TOP  = 2; //  Store in current context
  IFFSLI_PROP = 3; //  Store in topmost FORM OR LIST

// Magic value for writing functions. If you pass this value in as a size to PushChunk() when writing a file, the parser will figure out the
// size of the chunk for you. If you know the size, is it better to provide as it makes things faster.
  IFFSIZE_UNKNOWN =  -1;


// Possible call-back command values.
  IFFCMD_INIT     = 0; // Prepare the stream for a session
  IFFCMD_CLEANUP  = 1; // Terminate stream session
  IFFCMD_READ     = 2; // Read bytes from stream
  IFFCMD_WRITE    = 3; // Write bytes to stream
  IFFCMD_SEEK     = 4; // Seek on stream
  IFFCMD_ENTRY    = 5; // You just entered a new context
  IFFCMD_EXIT     = 6; // You're about to leave a context
  IFFCMD_PURGELCI = 7; // Purge a LocalContextItem

const
  IFFPARSENAME: PChar = 'iffparse.library';

var
  IFFParseBase: PLibrary = nil;
  IIFFParse: PInterface = nil;

function IFFParseObtain(): LongWord; syscall IIFFParse 60;
function IFFParseRelease(): LongWord; syscall IIFFParse 64;
procedure IFFParseExpunge(); syscall IIFFParse 68;
function IFFParseClone(): PInterface; syscall IIFFParse 72;
function AllocIFF: PIFFHandle; syscall IIFFParse 76;
function OpenIFF(Iff: PIFFHandle; RWMode: LongInt): LongInt; syscall IIFFParse 80;
function ParseIFF(Iff: PIFFHandle; Control: LongInt): LongInt; syscall IIFFParse 84;
procedure CloseIFF(Iff: PIFFHandle); syscall IIFFParse 88;
procedure FreeIFF(Iff: PIFFHandle); syscall IIFFParse 92;
function ReadChunkBytes(Iff: PIFFHandle; Buf: APTR; NumBytes: LongInt): LongInt; syscall IIFFParse 96;
function WriteChunkBytes(Iff: PIFFHandle; const Buf: APTR; NumBytes: LongInt): LongInt; syscall IIFFParse 100;
function ReadChunkRecords(Iff: PIFFHandle; Buf: APTR; BytesPerRecord: LongInt; NumRecords: LongInt): LongInt; syscall IIFFParse 104;
function WriteChunkRecords(Iff: PIFFHandle; const Buf: APTR; BytesPerRecord: LongInt; NumRecords: LongInt): LongInt; syscall IIFFParse 108;
function PushChunk(Iff: PIFFHandle; Type_, ID, Size: LongInt): LongInt; syscall IIFFParse 112;
function PopChunk(Iff: PIFFHandle): LongInt; syscall IIFFParse 116;
function EntryHandler(Iff: PIFFHandle; Type_, ID, Position: LongInt; Handler: PHook; Obj: APTR): LongInt; syscall IIFFParse 120;
function ExitHandler(Iff: PIFFHandle; Type_, ID, Position: LongInt; Handler: PHook; Obj: APTR): LongInt; syscall IIFFParse 124;
function PropChunk(Iff: PIFFHandle; Type_, ID: LongInt): LongInt; syscall IIFFParse 128;
function PropChunks(Iff: PIFFHandle; const PropArray: PLongInt; NumPairs: LongInt): LongInt; syscall IIFFParse 132;
function StopChunk(Iff: PIFFHandle; Type_, ID: LongInt): LongInt; syscall IIFFParse 136;
function StopChunks(Iff: PIFFHandle; const PropArray: PLongInt; NumPairs: LongInt): LongInt; syscall IIFFParse 140;
function CollectionChunk(Iff: PIFFHandle; Type_, ID: LongInt): LongInt; syscall IIFFParse 144;
function CollectionChunks(Iff: PIFFHandle; const PropArray: PLongInt; NumPairs: LongInt): LongInt; syscall IIFFParse 148;
function StopOnExit(Iff: PIFFHandle; Type_, ID: LongInt): LongInt; syscall IIFFParse 152;
function FindProp(const Iff: PIFFHandle; Type_, ID: LongInt): PStoredProperty; syscall IIFFParse 156;
function FindCollection(const Iff: PIFFHandle; Type_, ID: LongInt): PCollectionItem; syscall IIFFParse 160;
function FindPropContext(const Iff: PIFFHandle): PContextNode; syscall IIFFParse 164;
function CurrentChunk(const Iff: PIFFHandle): PContextNode; syscall IIFFParse 168;
function ParentChunk(const ContextNode: PContextNode): PContextNode; syscall IIFFParse 172;
function AllocLocalItem(Type_, ID, Ident, DataSize: LongInt): PLocalContextItem; syscall IIFFParse 176;
function LocalItemData(const LocalItem: PLocalContextItem): POINTER; syscall IIFFParse 180;
procedure SetLocalItemPurge(LocalItem: PLocalContextItem; const PurgeHook: PHook); syscall IIFFParse 184;
procedure FreeLocalItem(LocalItem: PLocalContextItem); syscall IIFFParse 188;
function FindLocalItem(const Iff: PIFFHandle; Type_, ID, Ident: LongInt): PLocalContextItem; syscall IIFFParse 192;
function StoreLocalItem(Iff: PIFFHandle; LocalItem: PLocalContextItem; Position: LongInt): LongInt; syscall IIFFParse 196;
procedure StoreItemInContext(Iff: PIFFHandle; LocalItem: PLocalContextItem; ContextNode: PContextNode); syscall IIFFParse 200;
procedure InitIFF(Iff: PIFFHandle; Flags: LongInt; const StreamHook: PHook); syscall IIFFParse 204;
procedure InitIFFasDOS(Iff: PIFFHandle); syscall IIFFParse 208;
procedure InitIFFasClip(Iff: PIFFHandle); syscall IIFFParse 212;
function OpenClipboard(UnitNumber: LongInt): PClipboardHandle; syscall IIFFParse 216;
procedure CloseClipboard(clipHandle: PClipboardHandle); syscall IIFFParse 220;
function GoodID(ID: LongInt): LongInt; syscall IIFFParse 224;
function GoodType(Type_: LongInt): LongInt; syscall IIFFParse 228;
function IDtoStr(ID: LongInt; Buf: STRPTR): STRPTR; syscall IIFFParse 232;

function Make_ID(Str: string): LongWord;

implementation

function Make_ID(Str: string): LongWord;
begin
  Make_ID := 0;
  if Length(Str) >= 4 then
    Make_ID := (LongWord(Ord(Str[1])) shl 24) or
               (LongWord(Ord(Str[2])) shl 16) or
               (LongWord(Ord(Str[3])) shl  8) or
               (LongWord(Ord(Str[4])));
end;

const
    { Change LIBVERSION to proper values }
    LIBVERSION : longword = 0;

initialization
  IFFParseBase := OpenLibrary(IFFPARSENAME,LIBVERSION);
  if Assigned(IFFParseBase) then
    IIFFParse := GetInterface(PLibrary(IFFParseBase), 'main', 1, nil);
finalization
  if Assigned(IIFFParse) then
    DropInterface(IIFFParse);
  if Assigned(IFFParseBase) then
    CloseLibrary(IFFParseBase);
end.





