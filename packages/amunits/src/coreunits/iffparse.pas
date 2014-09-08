{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998-2003 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
    History:

    Added the defines use_amiga_smartlink and
    use_auto_openlib. Implemented autoopening of
    the library.
    14 Jan 2003.

    Added function Make_ID.
    14 Jan 2003.

    Update for AmigaOS 3.9.
    Changed start code for unit.
    01 Feb 2003.

    Changed cardinal > longword.
    09 Feb 2003.

    nils.sjoholm@mailbox.swipnet.se Nils Sjoholm
}
{$PACKRECORDS 2}

unit iffparse;

INTERFACE

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
       pIFFHandle = ^tIFFHandle;
       tIFFHandle = record
        iff_Stream,
        iff_Flags   : ULONG;
        iff_Depth   : LONGINT;      {  Depth of context stack.  }
        {  There are private fields hiding here.  }
       END;

{
 * Bit masks for "iff_Flags" field.
 }
CONST
 IFFF_READ     =  0;                      { read mode - default }
 IFFF_WRITE    =  1;                      { write mode }
 IFFF_RWBITS   =  (IFFF_READ + IFFF_WRITE);        { read/write bits }
 IFFF_FSEEK    =  2;                 { forward seek only }
 IFFF_RSEEK    =  4;                 { random seek }
 IFFF_RESERVED =  $FFFF0000;             { Don't touch these bits. }

{
 * When the library calls your stream handler, you'll be passed a pointer
 * to this structure as the "message packet".
 }
Type
       pIFFStreamCmd = ^tIFFStreamCmd;
       tIFFStreamCmd = record
        sc_Command    : Longint;     {  Operation to be performed (IFFCMD_) }
        sc_Buf        : Pointer;     {  Pointer to data buffer              }
        sc_NBytes     : Longint;     {  Number of bytes to be affected      }
       END;
{
 * A node associated with a context on the iff_Stack.  Each node
 * represents a chunk, the stack representing the current nesting
 * of chunks in the open IFF file.  Each context node has associated
 * local context items in the (private) LocalItems list.  The ID, type,
 * size and scan values describe the chunk associated with this node.
 }
       pContextNode = ^tContextNode;
       tContextNode = record
        cn_Node         : tMinNode;
        cn_ID,
        cn_Type,
        cn_Size,        {  Size of this chunk             }
        cn_Scan  : Longint;        {  # of bytes read/written so far }
        {  There are private fields hiding here.  }
       END;

{
 * Local context items live in the ContextNode's.  Each class is identified
 * by its lci_Ident code and has a (private) purge vector for when the
 * parent context node is popped.
 }
       pLocalContextItem = ^tLocalContextItem;
       tLocalContextItem = record
        lci_Node        : tMinNode;
        lci_ID,
        lci_Type,
        lci_Ident       : ULONG;
        {  There are private fields hiding here.  }
       END;

{
 * StoredProperty: a local context item containing the data stored
 * from a previously encountered property chunk.
 }
       pStoredProperty = ^tStoredProperty;
       tStoredProperty = Record
        sp_Size  : Longint;
        sp_Data  : Pointer;
       END;

{
 * Collection Item: the actual node in the collection list at which
 * client will look.  The next pointers cross context boundaries so
 * that the complete list is accessable.
 }
       pCollectionItem = ^tCollectionItem;
       tCollectionItem = record
        ci_Next                 : pCollectionItem;
        ci_Size                 : Longint;
        ci_Data                 : Pointer;
       END;

{
 * Structure returned by OpenClipboard().  You may do CMD_POSTs and such
 * using this structure.  However, once you call OpenIFF(), you may not
 * do any more of your own I/O to the clipboard until you call CloseIFF().
 }
       pClipboardHandle = ^tClipBoardHandle;
       tClipboardHandle = record
        cbh_Req                 : tIOClipReq;
        cbh_CBport,
        cbh_SatisfyPort         : tMsgPort;
       END;

{
 * IFF return codes.  Most functions return either zero for success or
 * one of these codes.  The exceptions are the read/write functions which
 * return positive values for number of bytes or records read or written,
 * or a negative error code.  Some of these codes are not errors per sae,
 * but valid conditions such as EOF or EOC (End of Chunk).
 }
CONST
 IFFERR_EOF            =  -1 ;    {  Reached logical END of file }
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
        ((ULONG) (a)<<24 | (ULONG) (b)<<16 | (ULONG) (c)<<8 | (ULONG) (d))
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

VAR IFFParseBase : pLibrary;

FUNCTION AllocIFF : pIFFHandle; syscall IFFParseBase 030;
FUNCTION AllocLocalItem(typ : LONGINT location 'd0'; id : LONGINT location 'd1'; ident : LONGINT location 'd2'; dataSize : LONGINT location 'd3') : pLocalContextItem; syscall IFFParseBase 186;
PROCEDURE CloseClipboard(clipHandle : pClipboardHandle location 'd0'); syscall IFFParseBase 252;
PROCEDURE CloseIFF(iff : pIFFHandle location 'a0'); syscall IFFParseBase 048;
FUNCTION CollectionChunk(iff : pIFFHandle location 'a0'; typ : LONGINT location 'd0'; id : LONGINT location 'd1') : LONGINT; syscall IFFParseBase 138;
FUNCTION CollectionChunks(iff : pIFFHandle location 'a0'; const propArray : pLONGINT location 'a1'; numPairs : LONGINT location 'd0') : LONGINT; syscall IFFParseBase 144;
FUNCTION CurrentChunk(const iff : pIFFHandle location 'a0') : pContextNode; syscall IFFParseBase 174;
FUNCTION EntryHandler(iff : pIFFHandle location 'a0'; typ : LONGINT location 'd0'; id : LONGINT location 'd1'; position : LONGINT location 'd2'; handler : pHook location 'a1'; obj : POINTER location 'a2') : LONGINT; syscall IFFParseBase 102;
FUNCTION ExitHandler(iff : pIFFHandle location 'a0'; typ : LONGINT location 'd0'; id : LONGINT location 'd1'; position : LONGINT location 'd2'; handler : pHook location 'a1'; obj : POINTER location 'a2') : LONGINT; syscall IFFParseBase 108;
FUNCTION FindCollection(const iff : pIFFHandle location 'a0'; typ : LONGINT location 'd0'; id : LONGINT location 'd1') : pCollectionItem; syscall IFFParseBase 162;
FUNCTION FindLocalItem(const iff : pIFFHandle location 'a0'; typ : LONGINT location 'd0'; id : LONGINT location 'd1'; ident : LONGINT location 'd2') : pLocalContextItem; syscall IFFParseBase 210;
FUNCTION FindProp(const iff : pIFFHandle location 'a0'; typ : LONGINT location 'd0'; id : LONGINT location 'd1') : pStoredProperty; syscall IFFParseBase 156;
FUNCTION FindPropContext(const iff : pIFFHandle location 'a0') : pContextNode; syscall IFFParseBase 168;
PROCEDURE FreeIFF(iff : pIFFHandle location 'a0'); syscall IFFParseBase 054;
PROCEDURE FreeLocalItem(localItem : pLocalContextItem location 'a0'); syscall IFFParseBase 204;
FUNCTION GoodID(id : LONGINT location 'd0') : LONGINT; syscall IFFParseBase 258;
FUNCTION GoodType(typ : LONGINT location 'd0') : LONGINT; syscall IFFParseBase 264;
FUNCTION IDtoStr(id : LONGINT location 'd0'; buf : pCHAR location 'a0') : pCHAR; syscall IFFParseBase 270;
PROCEDURE InitIFF(iff : pIFFHandle location 'a0'; flags : LONGINT location 'd0'; const streamHook : pHook location 'a1'); syscall IFFParseBase 228;
PROCEDURE InitIFFasClip(iff : pIFFHandle location 'a0'); syscall IFFParseBase 240;
PROCEDURE InitIFFasDOS(iff : pIFFHandle location 'a0'); syscall IFFParseBase 234;
FUNCTION LocalItemData(const localItem : pLocalContextItem location 'a0') : POINTER; syscall IFFParseBase 192;
FUNCTION OpenClipboard(unitNumber : LONGINT location 'd0') : pClipboardHandle; syscall IFFParseBase 246;
FUNCTION OpenIFF(iff : pIFFHandle location 'a0'; rwMode : LONGINT location 'd0') : LONGINT; syscall IFFParseBase 036;
FUNCTION ParentChunk(const contextNode : pContextNode location 'a0') : pContextNode; syscall IFFParseBase 180;
FUNCTION ParseIFF(iff : pIFFHandle location 'a0'; control : LONGINT location 'd0') : LONGINT; syscall IFFParseBase 042;
FUNCTION PopChunk(iff : pIFFHandle location 'a0') : LONGINT; syscall IFFParseBase 090;
FUNCTION PropChunk(iff : pIFFHandle location 'a0'; typ : LONGINT location 'd0'; id : LONGINT location 'd1') : LONGINT; syscall IFFParseBase 114;
FUNCTION PropChunks(iff : pIFFHandle location 'a0'; const propArray : pLONGINT location 'a1'; numPairs : LONGINT location 'd0') : LONGINT; syscall IFFParseBase 120;
FUNCTION PushChunk(iff : pIFFHandle location 'a0'; typ : LONGINT location 'd0'; id : LONGINT location 'd1'; size : LONGINT location 'd2') : LONGINT; syscall IFFParseBase 084;
FUNCTION ReadChunkBytes(iff : pIFFHandle location 'a0'; buf : POINTER location 'a1'; numBytes : LONGINT location 'd0') : LONGINT; syscall IFFParseBase 060;
FUNCTION ReadChunkRecords(iff : pIFFHandle location 'a0'; buf : POINTER location 'a1'; bytesPerRecord : LONGINT location 'd0'; numRecords : LONGINT location 'd1') : LONGINT; syscall IFFParseBase 072;
PROCEDURE SetLocalItemPurge(localItem : pLocalContextItem location 'a0'; const purgeHook : pHook location 'a1'); syscall IFFParseBase 198;
FUNCTION StopChunk(iff : pIFFHandle location 'a0'; typ : LONGINT location 'd0'; id : LONGINT location 'd1') : LONGINT; syscall IFFParseBase 126;
FUNCTION StopChunks(iff : pIFFHandle location 'a0'; const propArray : pLONGINT location 'a1'; numPairs : LONGINT location 'd0') : LONGINT; syscall IFFParseBase 132;
FUNCTION StopOnExit(iff : pIFFHandle location 'a0'; typ : LONGINT location 'd0'; id : LONGINT location 'd1') : LONGINT; syscall IFFParseBase 150;
PROCEDURE StoreItemInContext(iff : pIFFHandle location 'a0'; localItem : pLocalContextItem location 'a1'; contextNode : pContextNode location 'a2'); syscall IFFParseBase 222;
FUNCTION StoreLocalItem(iff : pIFFHandle location 'a0'; localItem : pLocalContextItem location 'a1'; position : LONGINT location 'd0') : LONGINT; syscall IFFParseBase 216;
FUNCTION WriteChunkBytes(iff : pIFFHandle location 'a0'; const buf : POINTER location 'a1'; numBytes : LONGINT location 'd0') : LONGINT; syscall IFFParseBase 066;
FUNCTION WriteChunkRecords(iff : pIFFHandle location 'a0'; const buf : POINTER location 'a1'; bytesPerRecord : LONGINT location 'd0'; numRecords : LONGINT location 'd1') : LONGINT; syscall IFFParseBase 078;

Function Make_ID(str : String) : LONGINT;

{Here we read how to compile this unit}
{You can remove this include and use a define instead}
{$I useautoopenlib.inc}
{$ifdef use_init_openlib}
procedure InitIFFPARSELibrary;
{$endif use_init_openlib}

{This is a variable that knows how the unit is compiled}
var
    IFFPARSEIsCompiledHow : longint;

IMPLEMENTATION

uses
{$ifndef dont_use_openlib}
amsgbox;
{$endif dont_use_openlib}


Function Make_ID(str : String) : LONGINT;
begin
        Make_ID := (LONGINT(Ord(Str[1])) shl 24) or
                  (LONGINT(Ord(Str[2])) shl 16 ) or
                  (LONGINT(Ord(Str[3])) shl 8 ) or (LONGINT(Ord(Str[4])));
end;

const
    { Change VERSION and LIBVERSION to proper values }

    VERSION : string[2] = '0';
    LIBVERSION : longword = 0;

{$ifdef use_init_openlib}
  {$Info Compiling initopening of iffparse.library}
  {$Info don't forget to use InitIFFPARSELibrary in the beginning of your program}

var
    iffparse_exit : Pointer;

procedure CloseiffparseLibrary;
begin
    ExitProc := iffparse_exit;
    if IFFParseBase <> nil then begin
        CloseLibrary(IFFParseBase);
        IFFParseBase := nil;
    end;
end;

procedure InitIFFPARSELibrary;
begin
    IFFParseBase := nil;
    IFFParseBase := OpenLibrary(IFFPARSENAME,LIBVERSION);
    if IFFParseBase <> nil then begin
        iffparse_exit := ExitProc;
        ExitProc := @CloseiffparseLibrary;
    end else begin
        MessageBox('FPC Pascal Error',
        'Can''t open iffparse.library version ' + VERSION + #10 +
        'Deallocating resources and closing down',
        'Oops');
        halt(20);
    end;
end;

begin
    IFFPARSEIsCompiledHow := 2;
{$endif use_init_openlib}

{$ifdef use_auto_openlib}
  {$Info Compiling autoopening of iffparse.library}

var
    iffparse_exit : Pointer;

procedure CloseiffparseLibrary;
begin
    ExitProc := iffparse_exit;
    if IFFParseBase <> nil then begin
        CloseLibrary(IFFParseBase);
        IFFParseBase := nil;
    end;
end;

begin
    IFFParseBase := nil;
    IFFParseBase := OpenLibrary(IFFPARSENAME,LIBVERSION);
    if IFFParseBase <> nil then begin
        iffparse_exit := ExitProc;
        ExitProc := @CloseiffparseLibrary;
        IFFPARSEIsCompiledHow := 1;
    end else begin
        MessageBox('FPC Pascal Error',
        'Can''t open iffparse.library version ' + VERSION + #10 +
        'Deallocating resources and closing down',
        'Oops');
        halt(20);
    end;

{$endif use_auto_openlib}

{$ifdef dont_use_openlib}
begin
    IFFPARSEIsCompiledHow := 3;
   {$Warning No autoopening of iffparse.library compiled}
   {$Warning Make sure you open iffparse.library yourself}
{$endif dont_use_openlib}


END. (* UNIT IFFPARSE *)





