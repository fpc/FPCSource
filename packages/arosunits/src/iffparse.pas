{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by Free Pascal development team

    iffparse.library functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit iffparse;

interface

{$mode objfpc}

uses exec, clipboard, utility;


const
    IFFPARSENAME  : PChar = 'iffparse.library';

{
 * Struct associated with an active IFF stream.
 * "iff_Stream" is a value used by the client's read/write/seek functions -
 * it will not be accessed by the library itself and can have any value
 * (could even be a pointer or a BPTR).
 }
Type
       PIFFHandle = ^TIFFHandle;
       TIFFHandle = record
        iff_Stream,
        iff_Flags   : LongWord;
        iff_Depth   : LongInt;      {  Depth of context stack.  }
        {  There are private fields hiding here.  }
       end;

{
 * Bit masks for "iff_Flags" field.
 }
const
 IFFF_READ     =  0;                      { read mode - default }
 IFFF_WRITE    =  1;                      { write mode }
 IFFF_RWBITS   =  (IFFF_READ + IFFF_WRITE);        { read/write bits }
 IFFF_FSEEK    =  2;                 { forward seek only }
 IFFF_RSEEK    =  4;                 { random seek }
 IFFF_RESERVED =  $FFFF0000;             { Don't touch these bits. }

{
 * When the library calls your stream handler, you'll be passed a Pointer
 * to this structure as the "message packet".
 }
type
       PIFFStreamCmd = ^TIFFStreamCmd;
       TIFFStreamCmd = record
        sc_Command    : LongInt;     {  Operation to be performed (IFFCMD_) }
        sc_Buf        : Pointer;     {  Pointer to data buffer              }
        sc_NBytes     : LongInt;     {  Number of bytes to be affected      }
       end;
{
 * A node associated with a context on the iff_Stack.  Each node
 * represents a chunk, the stack representing the current nesting
 * of chunks in the open IFF file.  Each context node has associated
 * local context items in the (private) LocalItems list.  The ID, type,
 * size and scan values describe the chunk associated with this node.
 }
       PContextNode = ^TContextNode;
       TContextNode = record
        cn_Node         : TMinNode;
        cn_ID,
        cn_Type,
        cn_Size,        {  Size of this chunk             }
        cn_Scan  : LongInt;        {  # of bytes read/written so far }
        {  There are private fields hiding here.  }
       end;

{
 * Local context items live in the ContextNode's.  Each class is identified
 * by its lci_Ident code and has a (private) purge vector for when the
 * parent context node is popped.
 }
       PLocalContextItem = ^TLocalContextItem;
       TLocalContextItem = record
        lci_Node        : TMinNode;
        lci_ID,
        lci_Type,
        lci_Ident       : LongWord;
        {  There are private fields hiding here.  }
       end;

{
 * StoredProperty: a local context item containing the data stored
 * from a previously encountered property chunk.
 }
       PStoredProperty = ^TStoredProperty;
       TStoredProperty = record
        sp_Size  : LongInt;
        sp_Data  : Pointer;
       end;

{
 * Collection Item: the actual node in the collection list at which
 * client will look.  The next pointers cross context boundaries so
 * that the complete list is accessable.
 }
       PCollectionItem = ^TCollectionItem;
       TCollectionItem = record
        ci_Next                 : PCollectionItem;
        ci_Size                 : LongInt;
        ci_Data                 : Pointer;
       end;

{
 * Structure returned by OpenClipboard().  You may do CMD_POSTs and such
 * using this structure.  However, once you call OpenIFF(), you may not
 * do any more of your own I/O to the clipboard until you call CloseIFF().
 }
       PClipboardHandle = ^TClipBoardHandle;
       TClipboardHandle = record
        cbh_Req                 : TIOClipReq;
        cbh_CBport,
        cbh_SatisfyPort         : TMsgPort;
       end;

{
 * IFF return codes.  Most functions return either zero for success or
 * one of these codes.  The exceptions are the read/write functions which
 * return positive values for number of bytes or records read or written,
 * or a negative error code.  Some of these codes are not errors per sae,
 * but valid conditions such as EOF or EOC (end of Chunk).
 }
const
 IFFERR_EOF            =  -1 ;    {  Reached logical end of file }
 IFFERR_EOC            =  -2 ;    {  About to leave context      }
 IFFERR_NOSCOPE        =  -3 ;    {  No valid scope for property }
 IFFERR_NOMEM          =  -4 ;    {  Internal memory alloc failed}
 IFFERR_READ           =  -5 ;    {  Stream read error           }
 IFFERR_WRITE          =  -6 ;    {  Stream write error          }
 IFFERR_SEEK           =  -7 ;    {  Stream seek error           }
 IFFERR_MANGLED        =  -8 ;    {  Data in file is corrupt     }
 IFFERR_SYNTAX         =  -9 ;    {  IFF syntax error            }
 IFFERR_NOTIFF         =  -10;    {  Not an IFF file             }
 IFFERR_NOHOOK         =  -11;    {  No call-back hook provided  }
 IFF_RETURN2CLIENT     =  -12;    {  Client handler normal return}

{
 MAKE_ID(a,b,c,d)        \
        ((LongWord) (a)<<24 | (LongWord) (b)<<16 | (LongWord) (c)<<8 | (LongWord) (d))
     }
{
 * Universal IFF identifiers.
 }
 ID_FORM = 1179603533;
 ID_LIST = 1279873876;
 ID_CAT  = 1128354848;
 ID_PROP = 1347571536;
 ID_NULL = 538976288;

{
 * Ident codes for universally recognized local context items.
 }
 IFFLCI_PROP         = 1886547824;
 IFFLCI_COLLECTION   = 1668246636;
 IFFLCI_ENTRYHANDLER = 1701734500;
 IFFLCI_EXITHANDLER  = 1702389860;


{
 * Control modes for ParseIFF() function.
 }
 IFFPARSE_SCAN         =  0;
 IFFPARSE_STEP         =  1;
 IFFPARSE_RAWSTEP      =  2;

{
 * Control modes for StoreLocalItem().
 }
 IFFSLI_ROOT           =  1;      {  Store in default context       }
 IFFSLI_TOP            =  2;      {  Store in current context       }
 IFFSLI_PROP           =  3;      {  Store in topmost FORM OR LIST  }

{
 * "Flag" for writing functions.  If you pass this value in as a size
 * to PushChunk() when writing a file, the parser will figure out the
 * size of the chunk for you.  (Chunk sizes >= 2**31 are forbidden by the
 * IFF specification, so this works.)
 }
 IFFSIZE_UNKNOWN       =  -1;

{
 * Possible call-back command values.  (Using 0 as the value for IFFCMD_INIT
 * was, in retrospect, probably a bad idea.)
 }
 IFFCMD_INIT    = 0;       {  Prepare the stream for a session    }
 IFFCMD_CLEANUP = 1;       {  Terminate stream session            }
 IFFCMD_READ    = 2;       {  Read bytes from stream              }
 IFFCMD_WRITE   = 3;       {  Write bytes to stream               }
 IFFCMD_SEEK    = 4;       {  Seek on stream                      }
 IFFCMD_ENTRY   = 5;       {  You just entered a new context      }
 IFFCMD_EXIT    = 6;       {  You're about to leave a context     }
 IFFCMD_PURGELCI= 7;       {  Purge a LocalContextItem            }

{  Backward compatibility.  Don't use these in new code.  }
 IFFSCC_INIT    = IFFCMD_INIT;
 IFFSCC_CLEANUP = IFFCMD_CLEANUP;
 IFFSCC_READ    = IFFCMD_READ;
 IFFSCC_WRITE   = IFFCMD_WRITE;
 IFFSCC_SEEK    = IFFCMD_SEEK;

var IFFParseBase: PLibrary;

function AllocIFF: PIFFHandle; syscall IFFParseBase 5;
function AllocLocalItem(Typ: LongInt; Id: LongInt; Ident: LongInt; DataSize: LongInt): PLocalContextItem; syscall IFFParseBase 31;
procedure CloseClipboard(ClipHandle: PClipboardHandle); syscall IFFParseBase 42;
procedure CloseIFF(Iff: PIFFHandle);  syscall IFFParseBase 8;
function CollectionChunk(Iff: PIFFHandle; Typ: LongInt; Id: LongInt): LongInt; syscall IFFParseBase 23;
function CollectionChunks(Iff : PIFFHandle;const PropArray: PLongInt; NumPairs: LongInt): LongInt; syscall IFFParseBase 24;
function CurrentChunk(const Iff: PIFFHandle): PContextNode; syscall IFFParseBase 29;
function EntryHandler(Iff: PIFFHandle; Typ: LongInt; Id: LongInt; Position: LongInt; Handler: PHook; Obj: Pointer): LongInt; syscall IFFParseBase 17;
function ExitHandler(Iff: PIFFHandle; Typ: LongInt; Id: LongInt; Position: LongInt; Handler: PHook; Obj: Pointer): LongInt; syscall IFFParseBase 18;
function FindCollection(const Iff: PIFFHandle; Typ: LongInt; Id: LongInt): PCollectionItem; syscall IFFParseBase 27;
function FindLocalItem(const Iff: PIFFHandle; Typ: LongInt; Id: LongInt; Ident: LongInt): PLocalContextItem; syscall IFFParseBase 35;
function FindProp(const Iff: PIFFHandle; Typ: LongInt; Id: LongInt): PStoredProperty; syscall IFFParseBase 26;
function FindPropContext(const Iff: PIFFHandle): PContextNode; syscall IFFParseBase 28;
procedure FreeIFF(Iff: PIFFHandle);  syscall IFFParseBase 9;
procedure FreeLocalItem(LocalItem: PLocalContextItem); syscall IFFParseBase 34;
function GoodID(Id: LongInt): LongInt; syscall IFFParseBase 43;
function GoodType(Typ: LongInt): LongInt; syscall IFFParseBase 44;
function IDtoStr(Id: LongInt; Buf: PChar): PChar; syscall IFFParseBase 45;
procedure InitIFF(Iff: PIFFHandle; Flags: LongInt;const StreamHook: PHook); syscall IFFParseBase 38;
procedure InitIFFasClip(Iff: PIFFHandle); syscall IFFParseBase 40;
procedure InitIFFasDOS(Iff: PIFFHandle); syscall IFFParseBase 39;
function LocalItemData(const LocalItem: PLocalContextItem): Pointer; syscall IFFParseBase 32;
function OpenClipboard(unitNumber: LongInt): PClipboardHandle; syscall IFFParseBase 41;
function OpenIFF(Iff: PIFFHandle; rwMode: LongInt): LongInt;  syscall IFFParseBase 6;
function ParentChunk(const contextNode: PContextNode): PContextNode; syscall IFFParseBase 30;
function ParseIFF(Iff: PIFFHandle; control: LongInt): LongInt;  syscall IFFParseBase 7;
function PopChunk(Iff: PIFFHandle): LongInt; syscall IFFParseBase 15;
function PropChunk(Iff: PIFFHandle; Typ: LongInt; Id: LongInt): LongInt; syscall IFFParseBase 19;
function PropChunks(Iff: PIFFHandle;const PropArray: PLongInt; NumPairs: LongInt): LongInt; syscall IFFParseBase 20;
function PushChunk(Iff: PIFFHandle; Typ: LongInt; Id: LongInt; size: LongInt): LongInt; syscall IFFParseBase 14;
function ReadChunkBytes(Iff: PIFFHandle; Buf: Pointer; numBytes: LongInt): LongInt;  syscall IFFParseBase 10;
function ReadChunkRecords(Iff: PIFFHandle; Buf: Pointer; bytesPerRecord: LongInt; numRecords: LongInt): LongInt; syscall IFFParseBase 12;
procedure SetLocalItemPurge(LocalItem: PLocalContextItem;const purgeHook: PHook); syscall IFFParseBase 33;
function StopChunk(Iff: PIFFHandle; Typ: LongInt; Id: LongInt): LongInt; syscall IFFParseBase 21;
function StopChunks(Iff: PIFFHandle;const PropArray: PLongInt; NumPairs: LongInt): LongInt; syscall IFFParseBase 22;
function StopOnExit(Iff: PIFFHandle; Typ: LongInt; Id: LongInt): LongInt; syscall IFFParseBase 25;
procedure StoreItemInContext(Iff: PIFFHandle; LocalItem: PLocalContextItem; contextNode: PContextNode); syscall IFFParseBase 37;
function StoreLocalItem(Iff: PIFFHandle; LocalItem: PLocalContextItem; Position: LongInt): LongInt; syscall IFFParseBase 36;
function WriteChunkBytes(Iff: PIFFHandle;const Buf: Pointer; NumBytes: LongInt): LongInt; syscall IFFParseBase 11;
function WriteChunkRecords(Iff: PIFFHandle;const Buf: Pointer; BytesPerRecord: LongInt; NumRecords: LongInt): LongInt; syscall IFFParseBase 13;

function Make_ID(str: String): LongInt;

implementation

function Make_ID(str: String): LongInt;
begin
        Make_ID:= (LongInt(Ord(Str[1])) shl 24) or
                  (LongInt(Ord(Str[2])) shl 16 ) or
                  (LongInt(Ord(Str[3])) shl 8 ) or (LongInt(Ord(Str[4])));
end;

initialization
  IFFParseBase := OpenLibrary(IFFPARSENAME, 0);
finalization
  CloseLibrary(IFFParseBase);
end. (* UNIT IFFPARSE *)





