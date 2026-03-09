{
    This file is part of the Free Pascal run time library.

    Copyright (c) 2006 by Thomas Schatzl, member of the FreePascal
    Development team
    Parts (c) 2000 Peter Vreman (adapted from original dwarfs line
    reader)

    Dwarf LineInfo Retriever

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  This unit should not be compiled in objfpc mode, since this would make it
  dependent on objpas unit.
}

{$modeswitch advancedrecords} {$modeswitch class} {$modeswitch result} {$modeswitch out} {$typedaddress on}

{$if not defined(FPUNONE) and not defined(FPUSOFT)}
  {$define can_use_ln_in_constants}
{$endif}

{-$define debug_lineinfocache}

unit lnfodwrf;

interface

{$S-}

{$IF FPC_VERSION<3}
type
  CodePointer = Pointer;
{$ENDIF}

function GetLineInfo(addr:codeptruint;var func,source:shortstring;var line:longint) : boolean;
function DwarfBackTraceStr(addr: CodePointer): shortstring;
procedure CloseDwarf;

var
  // Allows more efficient operation by reusing previously loaded debug data
  // when the target module filename is the same. However, if an invalid memory
  // address is supplied then further calls may result in an undefined behaviour.
  // In summary: enable for speed, disable for resilience.
  AllowReuseOfLineInfoData: Boolean = True;

// While the cache can scale down arbitrarily, e.g. down to 8 records, its mere code size is nontrivial (3.3 Kb on x64), so just disable for small bitnesses.
{$if not defined(cpuint8) and not defined(cpuint16)}
{$define has_LineInfoCache}
type
  LineInfoCache = record
    function TryGet(addr: CodePointer; out func, source: shortstring; out line: longint): boolean;
    procedure Put(addr: CodePointer; const func, source: shortstring; line: longint);

  private type
    StringDataHeaderHashType = uint16;

    pStringDataHeader = ^StringDataHeader;
    StringDataHeader = packed record
      len: uint8;
      hash: StringDataHeaderHashType; // Hopefully automatically unaligned().
    end;

  const
    // String data format, stored in strings.data and referenced by strings.dataOfs:
    // 3 bytes: StringDataHeader.
    // N bytes: string data.
    StringHeaderSize = sizeof(StringDataHeader);
    File0Function1 = 2;

  {$if false and defined(debug_lineinfocache)}
    StringLengthEstimation = 10;
    MaxStrings = 8;
    StringsHashSize = 4;
    MaxAddresses = 8;
    AddressesHashSize = 4;
  {$else select cache size}
    StringLengthEstimation = 24; // Expected average string length. (Value for FPC allocation traces when compiling itself at the time of writing this comment: 15.2.)
    HashLoadFactor = 4;

    // 64-bit CPUs tend to have more memory.
    MaxStrings = {$if sizeof(pointer) > 4} 1600 {$else} 1000 {$endif};
    StringsHashSizePrecomputed = {$if sizeof(pointer) > 4} 512 {$else} 256 {$endif};
    StringsHashSize = {$ifdef can_use_ln_in_constants} 1 shl round(ln(MaxStrings / HashLoadFactor) / ln(2)) {$else} StringsHashSizePrecomputed {$endif};
  {$if StringsHashSize <> StringsHashSizePrecomputed} {$error fix StringsHashSizePrecomputed} {$endif}
  {$if StringsHashSize and (StringsHashSize - 1) <> 0} {$error must be 2^N} {$endif}

    MaxAddresses = {$if sizeof(pointer) > 4} 8000 {$else} 3600 {$endif};
    AddressesHashSizePrecomputed = {$if sizeof(pointer) > 4} 2048 {$else} 1024 {$endif};
    AddressesHashSize = {$ifdef can_use_ln_in_constants} 1 shl round(ln(MaxAddresses / HashLoadFactor) / ln(2)) {$else} AddressesHashSizePrecomputed {$endif};
  {$if AddressesHashSize <> AddressesHashSizePrecomputed} {$error fix AddressesHashSizePrecomputed} {$endif}
  {$if AddressesHashSize and (AddressesHashSize - 1) <> 0} {$error must be 2^N} {$endif}
  {$endif select cache size}
    MaxStringsData = MaxStrings * (StringHeaderSize + StringLengthEstimation);

  type
    StringIndexType = {$if MaxStrings <= High(uint8)} uint8 {$elseif MaxStrings <= High(uint16)} uint16 {$else} {$error MaxStrings looks too large.} {$endif};
    StringDataOffsetType = {$if MaxStringsData <= High(uint8)} uint8 {$elseif MaxStringsData <= High(uint16)} uint16 {$else} {$error MaxStringsData looks too large.} {$endif};
    AddressIndexType = {$if MaxAddresses <= High(uint8)} uint8 {$elseif MaxAddresses <= High(uint16)} uint16 {$else} {$error MaxAddresses looks too large.} {$endif};

  var
    // File and function names are merged only for simplicity, not sure if it hurts the eviction behavior (think of MaxStrings functions in one file evicting other files)...
    strings: record
      nUpto, nFree, firstLru1, lastLru1, firstFree1: StringIndexType;
      hash1: array[0 .. StringsHashSize - 1] of StringIndexType;
      s: array[0 .. MaxStrings - 1] of record
      case uint32 of
        0: (prevLru1, nextLru1: StringIndexType);
        1: (nextFree1: StringIndexType);
      end;
      nextCollision1: array[0 .. MaxStrings - 1] of StringIndexType;
      firstUser1: array[0 .. MaxStrings - 1] of AddressIndexType;
      dataOfs: array[0 .. MaxStrings - 1] of StringDataOffsetType;
      dataSize: StringDataOffsetType;
      data: array[0 .. MaxStringsData - 1] of byte;
    end;

    addresses: record
      nUpto, nFree, firstLru1, lastLru1, firstFree1: AddressIndexType;
      hash1: array[0 .. AddressesHashSize - 1] of AddressIndexType;
      p: array[0 .. MaxAddresses - 1] of CodePointer;
      a: array[0 .. MaxAddresses - 1] of record
      case uint32 of
        0: (prevLru1, nextLru1: AddressIndexType);
        1: (nextFree1: AddressIndexType);
      end;
      nextCollision1: array[0 .. MaxAddresses - 1] of AddressIndexType;
      fileFunc: array[0 .. MaxAddresses - 1, 0 .. File0Function1 - 1] of StringIndexType;
      u: array[0 .. MaxAddresses - 1] of record
        // Addresses don’t distinguish between file and function names,
        // so address A might use string S as file name: fileFunc[A, 0] = S,
        // and address B might use the same string S as function name: fileFunc[B, 1] = S.
        //
        // If such a B is the user of S that immediately follows A,
        // u[A].nextUser[0] = 1 + B, and u[B].prevUser1[1] = 1 + A.
        //
        // These File0Function1 indices for a particular address A that uses a particular string S
        // can be determined with ord(S <> fileFunc[A, 0]).
        //
        // Special case: fileFunc[0] = fileFunc[1]. Only prev/nextUser1[0] are used then, and prev/nextUser1[1] are garbage.
        // (So ord(stringUsedByAddress = fileFunc[1]) won’t work.)
        prevUser1, nextUser1: array[0 .. File0Function1 - 1] of AddressIndexType;
      end;
      lineLo16: array[0 .. MaxAddresses - 1] of uint16;
      lineHi8: array[0 .. MaxAddresses - 1] of uint8; // Support line numbers up to 16,777,215.
    end;

    class function HashMemory(p: pointer; n: SizeUint): SizeUint; static;
    class function HashCodePointer(p: CodePointer): SizeUint; static; inline;
    procedure AddStringToLRU(id: SizeUint);
    procedure RemoveStringFromLRU(id: SizeUint);
    procedure BumpStringLRU(id: SizeUint); inline;
    function FindString(const s: shortstring; hash: SizeUint): SizeInt;
    function AddString(const s: shortstring; hash: SizeUint): SizeUint;
    procedure RemoveString(id: SizeUint);
    procedure StringToShortstring(id: SizeUint; out ss: shortstring);
    procedure PackStrings;
    procedure AddStringUser(stringId, addressId, iFileFunc: SizeUint);
    procedure RemoveStringUser(stringId, addressId, iFileFunc: SizeUint);

    procedure AddAddressToLRU(id: SizeUint);
    procedure RemoveAddressFromLRU(id: SizeUint);
    function FindAddress(addr: CodePointer; hash: SizeUint): SizeInt;
    function AddAddress(addr: CodePointer; hash, fileId, funcId: SizeUint; lineNo: uint32): SizeUint;
    procedure RemoveAddress(id: SizeUint);
  {$ifdef debug_lineinfocache}
    procedure DumpStringValue(var f: text; id: SizeUint);
    procedure DumpAddressValue(var f: text; id: SizeUint);
    procedure Dump(var f: text);
  {$endif}
  end;
{$endif enable LineInfoCache}


implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.ExeInfo;
{$ELSE FPC_DOTTEDUNITS}
uses
  exeinfo;
{$ENDIF FPC_DOTTEDUNITS}

{ Current issues:

  - ignores DW_LNS_SET_FILE
}

{$MACRO ON}

{ $DEFINE DEBUG_DWARF_PARSER}
{$ifdef DEBUG_DWARF_PARSER}
  {$define DEBUG_WRITELN := WriteLn}
  {$define DEBUG_COMMENT :=  }
{$else}
  {$define DEBUG_WRITELN := //}
  {$define DEBUG_COMMENT := //}
{$endif}

{ some type definitions }
type
  Bool8 = ByteBool;
{$ifdef CPUI8086}
  TOffset = Word;
{$else CPUI8086}
  TOffset = PtrUInt;
{$endif CPUI8086}
  TSegment = Word;

const
  DwarfLock = 0;
{$ifdef has_LineInfoCache}
  LiCacheLock = 1;
{$endif has_LineInfoCache}

var
{$ifdef fpc_has_feature_threading}
  locksAlive: boolean;
  locks: array[0 .. {$ifdef has_LineInfoCache} LiCacheLock {$else} DwarfLock {$endif}] of TRTLCriticalSection;
  prevInitProc, prevExitProc: CodePointer;
{$endif fpc_has_feature_threading}
  { the input file to read DWARF debug info from, i.e. paramstr(0) }
  e : TExeFile;
  { the offset and size of the DWARF debug_line section in the file }
  Dwarf_Debug_Line_Section_Offset,
  Dwarf_Debug_Line_Section_Size,
  { the offset and size of the DWARF debug_info section in the file }
  Dwarf_Debug_Info_Section_Offset,
  Dwarf_Debug_Info_Section_Size,
  { the offset and size of the DWARF debug_aranges section in the file }
  Dwarf_Debug_Aranges_Section_Offset,
  Dwarf_Debug_Aranges_Section_Size,
  { the offset and size of the DWARF debug_abbrev section in the file }
  Dwarf_Debug_Abbrev_Section_Offset,
  Dwarf_Debug_Abbrev_Section_Size : longint;
{$ifdef has_LineInfoCache}
  liCache: LineInfoCache;
{$endif has_LineInfoCache}

  procedure Lock(index: SizeUint); inline;
  begin
  {$ifdef fpc_has_feature_threading}
    if locksAlive then EnterCriticalSection(locks[index]);
  {$endif}
  end;

  procedure Unlock(index: SizeUint); inline;
  begin
  {$ifdef fpc_has_feature_threading}
    if locksAlive then LeaveCriticalSection(locks[index]);
  {$endif}
  end;

{ DWARF 2 default opcodes}
const
  { Extended opcodes }
  DW_LNE_END_SEQUENCE = 1;
  DW_LNE_SET_ADDRESS = 2;
  DW_LNE_DEFINE_FILE = 3;
{$ifdef CPUI8086}
  { non-standard Open Watcom extension; might conflict with future versions of
    the DWARF standard }
  DW_LNE_SET_SEGMENT = 4;
{$endif CPUI8086}
  { Standard opcodes }
  DW_LNS_COPY = 1;
  DW_LNS_ADVANCE_PC = 2;
  DW_LNS_ADVANCE_LINE = 3;
  DW_LNS_SET_FILE = 4;
  DW_LNS_SET_COLUMN = 5;
  DW_LNS_NEGATE_STMT = 6;
  DW_LNS_SET_BASIC_BLOCK = 7;
  DW_LNS_CONST_ADD_PC = 8;
  DW_LNS_FIXED_ADVANCE_PC = 9;
  DW_LNS_SET_PROLOGUE_END = 10;
  DW_LNS_SET_EPILOGUE_BEGIN = 11;
  DW_LNS_SET_ISA = 12;

  DW_FORM_addr = $1;
  DW_FORM_block2 = $3;
  DW_FORM_block4 = $4;
  DW_FORM_data2 = $5;
  DW_FORM_data4 = $6;
  DW_FORM_data8 = $7;
  DW_FORM_string = $8;
  DW_FORM_block = $9;
  DW_FORM_block1 = $a;
  DW_FORM_data1 = $b;
  DW_FORM_flag = $c;
  DW_FORM_sdata = $d;
  DW_FORM_strp = $e;
  DW_FORM_udata = $f;
  DW_FORM_ref_addr = $10;
  DW_FORM_ref1 = $11;
  DW_FORM_ref2 = $12;
  DW_FORM_ref4 = $13;
  DW_FORM_ref8 = $14;
  DW_FORM_ref_udata = $15;
  DW_FORM_indirect = $16;
  DW_FORM_sec_offset = $17;
  DW_FORM_exprloc = $18;
  DW_FORM_flag_present = $19;

type
  { state record for the line info state machine }
  TMachineState = record
    address : QWord;
    segment : TSegment;
    file_id : DWord;
    line : QWord;
    column : DWord;
    is_stmt : Boolean;
    basic_block : Boolean;
    end_sequence : Boolean;
    prolouge_end : Boolean;
    epilouge_begin : Boolean;
    isa : DWord;
    append_row : Boolean;
  end;

{ DWARF line number program header preceding the line number program, 64 bit version }
  TLineNumberProgramHeader64 = packed record
    magic : DWord;
    unit_length : QWord;
    version : Word;
    length : QWord;
    minimum_instruction_length : Byte;
    default_is_stmt : Bool8;
    line_base : ShortInt;
    line_range : Byte;
    opcode_base : Byte;
  end;

{ DWARF line number program header preceding the line number program, 32 bit version }
  TLineNumberProgramHeader32 = packed record
    unit_length : DWord;
    version : Word;
    length : DWord;
    minimum_instruction_length : Byte;
    default_is_stmt : Bool8;
    line_base : ShortInt;
    line_range : Byte;
    opcode_base : Byte;
  end;

  TDebugInfoProgramHeader64 = packed record
    magic : DWord;
    unit_length : QWord;
    version : Word;
    debug_abbrev_offset : QWord;
    address_size : Byte;
  end;

  TDebugInfoProgramHeader32= packed record
    unit_length : DWord;
    version : Word;
    debug_abbrev_offset : DWord;
    address_size : Byte;
  end;

  TDebugArangesHeader64 = packed record
    magic : DWord;
    unit_length : QWord;
    version : Word;
    debug_info_offset : QWord;
    address_size : Byte;
    segment_size : Byte;
{$ifndef CPUI8086}
    padding : DWord;
{$endif CPUI8086}
  end;

  TDebugArangesHeader32= packed record
    unit_length : DWord;
    version : Word;
    debug_info_offset : DWord;
    address_size : Byte;
    segment_size : Byte;
{$ifndef CPUI8086}
    padding : DWord;
{$endif CPUI8086}
  end;

{---------------------------------------------------------------------------
 I/O utility functions
---------------------------------------------------------------------------}

type
{$ifdef cpui8086}
  TFilePos = LongInt;
{$else cpui8086}
  TFilePos = SizeInt;
{$endif cpui8086}

var
  lastfilename: string;   { store last processed file }
  lastbaseaddr: {$ifdef cpui8086}farpointer{$else}pointer{$endif};  { store last base address }
  lastopendwarf: Boolean; { store last result of processing a file }

{$ifdef cpui8086}
function tofar(fp: FarPointer): FarPointer; inline;
begin
  tofar:=fp;
end;

function tofar(cp: NearCsPointer): FarPointer; inline;
begin
  tofar:=Ptr(CSeg,Word(cp));
end;

function tofar(cp: NearPointer): FarPointer; inline;
begin
  tofar:=Ptr(DSeg,Word(cp));
end;
{$else cpui8086}
type
  tofar=Pointer;
{$endif cpui8086}

procedure CloseDwarf(needLock: boolean);
begin
  if needLock then
    Lock(DwarfLock);
  if e.isopen then
    CloseExeFile(e);

  // Reset last processed filename
  lastfilename := '';
  if needLock then
    Unlock(DwarfLock);
end;

function OpenDwarf(addr : codepointer) : boolean;
var
  oldprocessaddress: TExeProcessAddress;
  baseaddr : {$ifdef cpui8086}farpointer{$else}pointer{$endif};
  filename,dbgfn : ansistring;
begin
  // False by default
  OpenDwarf:=false;

  // Empty so can test if GetModuleByAddr has worked
  filename := '';

  // Get filename by address using GetModuleByAddr
  GetModuleByAddr(tofar(addr),baseaddr,filename);
{$ifdef DEBUG_LINEINFO}
  writeln(stderr,filename,' Baseaddr: ',hexstr(baseaddr));
{$endif DEBUG_LINEINFO}

  // Check if GetModuleByAddr has worked
  if filename = '' then
    exit;

  // If target filename same as previous, then re-use previous result
  if AllowReuseOfLineInfoData and (filename = lastfilename) and (baseaddr = lastbaseaddr) then
  begin
    {$ifdef DEBUG_LINEINFO}
    writeln(stderr,'Reusing debug data');
    {$endif DEBUG_LINEINFO}
    OpenDwarf:=lastopendwarf;
    exit;
  end;

  // Close previously opened Dwarf
  CloseDwarf(false);

  // Reset last open dwarf result
  lastopendwarf := false;

  // Save newly processed filename
  lastfilename := filename;

  // Save newly processed file's base address
  lastbaseaddr := baseaddr;

  // Open exe file or debug link
  if not OpenExeFile(e,filename) then
    exit;
  if ReadDebugLink(e,dbgfn) then
    begin
      oldprocessaddress:=e.processaddress;
      CloseExeFile(e);
      if not OpenExeFile(e,dbgfn) then
        exit;
      e.processaddress:=oldprocessaddress;
    end;

  // Find debug data section
  e.processaddress:=ptruint(baseaddr)-e.processaddress;
  if FindExeSection(e,'.debug_line',Dwarf_Debug_Line_Section_offset,dwarf_Debug_Line_Section_size) and
    FindExeSection(e,'.debug_info',Dwarf_Debug_Info_Section_offset,dwarf_Debug_Info_Section_size) and
    FindExeSection(e,'.debug_abbrev',Dwarf_Debug_Abbrev_Section_offset,dwarf_Debug_Abbrev_Section_size) and
    FindExeSection(e,'.debug_aranges',Dwarf_Debug_Aranges_Section_offset,dwarf_Debug_Aranges_Section_size) then
  begin
    lastopendwarf:=true;
    OpenDwarf:=true;
    DEBUG_WRITELN('.debug_line starts at offset $',hexstr(Dwarf_Debug_Line_Section_offset,8),' with a size of ',Dwarf_Debug_Line_Section_Size,' Bytes');
    DEBUG_WRITELN('.debug_info starts at offset $',hexstr(Dwarf_Debug_Info_Section_offset,8),' with a size of ',Dwarf_Debug_Info_Section_Size,' Bytes');
    DEBUG_WRITELN('.debug_abbrev starts at offset $',hexstr(Dwarf_Debug_Abbrev_Section_offset,8),' with a size of ',Dwarf_Debug_Abbrev_Section_Size,' Bytes');
    DEBUG_WRITELN('.debug_aranges starts at offset $',hexstr(Dwarf_Debug_Aranges_Section_offset,8),' with a size of ',Dwarf_Debug_Aranges_Section_Size,' Bytes');
  end
  else
    CloseExeFile(e);
end;


procedure CloseDwarf;
begin
  CloseDwarf(true);
end;


type
  TEReader = object
    bufstart, base, limit: Int64; { All of these are absolute. }
    bufp, bufe: pByte; { bufp = pointer into buf, bufe - bufp = available bytes. }
    buf: array[0 .. 1023] of byte;
    procedure Init;
    procedure SetRange(aBase, aSize : Int64);
    function Pos : TFilePos; { Relative to base. }
    procedure Seek(newIndex : Int64); { Relative to base. }
    function ReadNext : longint; inline;
    function ReadNext(var dest; size : SizeInt) : Boolean;
    function ReadULEB128 : QWord;
    function ReadLEB128 : Int64;
    procedure SkipLEB128s(count : longint);
    function ReadAddress(addr_size: smallint) : PtrUInt;
  {$ifdef CPUI8086}
    function ReadSegment() : Word;
  {$endif CPUI8086}
    function ReadString(sp: PShortstring) : SizeInt;
    function ReadUHalf : Word;
  private
    function ReadNextBuffer : longint;
  end;


procedure TEReader.Init;
begin
  bufstart := 0;
  base := 0;
  limit := 0;
  bufp := pByte(buf);
  bufe := bufp;
end;

procedure TEReader.SetRange(aBase, aSize : Int64);
begin
  base := aBase;
  limit := base + aSize;
  Seek(0);
  if limit < bufstart + (bufe - pByte(buf)) then
    begin
      bufe := pByte(buf) + (limit - bufstart); { Respect the limit if the buffer was reused. Probably unreachable, but just in case... }
      System.Seek(e.f, limit);
    end;
end;


function TEReader.Pos() : TFilePos;
begin
  Pos := (bufstart - base) + (bufp - pByte(buf));
end;


procedure TEReader.Seek(newIndex : Int64);
var
  gpos : Int64;
begin
  gpos := base + newIndex;
  if (gpos >= bufstart) and (gpos <= bufstart + (bufe - pByte(buf))) then
    bufp := pByte(buf) + (gpos - bufstart) { Reuse the buffer. }
  else
    begin
      bufstart := gpos;
      System.Seek(e.f, gpos);
      bufp := pByte(buf);
      bufe := bufp;
    end;
end;


{ Returns the next Byte from the input stream, or -1 if there has been
  an error }
function TEReader.ReadNext() : Longint;
begin
  if (bufp = bufe) and (ReadNextBuffer <= 0) then
    exit(-1);
  ReadNext := bufp^;
  inc(bufp);
end;

{ Reads the next size bytes into dest. Returns true if successful,
  false otherwise. Note that dest may be partially overwritten after
  returning false. dest = nil^ — skip size bytes. }
function TEReader.ReadNext(var dest; size : SizeInt) : Boolean;
var
  bytesread : SizeInt;
  d: PByte;
begin
  d := @dest;
  while (size > 0) and ((bufp < bufe) or (ReadNextBuffer > 0)) do
    begin
      bytesread := bufe - bufp;
      if bytesread > size then bytesread := size;
      if Assigned(d) then
        begin
          Move(bufp^, d^, bytesread);
          inc(d, bytesread);
        end;
      inc(bufp, bytesread);
      dec(size, bytesread);
    end;
  ReadNext := size = 0;
end;


{ Reads an unsigned LEB encoded number from the input stream }
function TEReader.ReadULEB128() : QWord;
var
  shift : cardinal;
begin
  shift := 0;
  ReadULEB128 := 0;
  repeat
    if (bufp = bufe) and (ReadNextBuffer <= 0) then break;
    ReadULEB128 := ReadULEB128 or (QWord(bufp^ and $7f) shl shift);
    inc(bufp);
    inc(shift, 7);
  until ((bufp[-1] and $80) = 0);
end;

{ Reads a signed LEB encoded number from the input stream }
function TEReader.ReadLEB128() : Int64;
var
  shift : cardinal;
begin
  shift := 0;
  ReadLEB128 := 0;
  repeat
    if (bufp = bufe) and (ReadNextBuffer <= 0) then break;
    ReadLEB128 := ReadLEB128 or (Int64(bufp^ and $7f) shl shift);
    inc(bufp);
    inc(shift, 7);
  until ((bufp[-1] and $80) = 0);
  { extend sign. }
  ReadLEB128 := ReadLEB128 or -(ReadLEB128 and (Int64(1) shl (shift-1)));
end;


procedure TEReader.SkipLEB128s(count : longint);
{$ifdef DEBUG_DWARF_PARSER}
var temp : QWord;
{$endif DEBUG_DWARF_PARSER}
begin
  while count > 0 do
  begin
{$ifdef DEBUG_DWARF_PARSER}
    temp := er.ReadLEB128();
    DEBUG_WRITELN('Skipping LEB128 : ', temp);
{$else DEBUG_DWARF_PARSER}
    repeat
      if (bufp = bufe) and (ReadNextBuffer <= 0) then exit;
      inc(bufp);
    until bufp[-1] and $80 = 0;
{$endif DEBUG_DWARF_PARSER}
    dec(count);
  end;
end;


{ Reads an address from the current input stream }
function TEReader.ReadAddress(addr_size: smallint) : PtrUInt;
begin
{$ifdef CPUI8086}
  ReadAddress := 0;
  if (addr_size = 4) or (addr_size = 2) then
{$endif}
    ReadNext(ReadAddress, sizeof(ReadAddress));
end;

{$ifdef CPUI8086}
{ Reads a segment from the current input stream }
function TEReader.ReadSegment() : Word;
begin
  ReadNext(ReadSegment, sizeof(ReadSegment));
end;
{$endif CPUI8086}


{ Reads a zero-terminated string from the current input stream. If the
  string is larger than 255 chars (maximum allowed number of elements in
  a ShortString, excess characters will be chopped off.
  sp can be nil if not required. Returns the length of the string
  (even when sp is nil; also can return >255 if it was past the shortstring limit). }
function TEReader.ReadString(sp: PShortstring) : SizeInt;
var
  nbufpart,zp,nmove : SizeInt;
begin
  if Assigned(sp) then
    sp^[0] := #0;
  ReadString := 0;
  repeat
    if (bufp = bufe) and (ReadNextBuffer <= 0) then
    begin
      if Assigned(sp) then
        sp^ := '';
      exit(0); { unexpected end of file occurred }
    end;
    nbufpart := bufe - bufp;
    zp := IndexByte(bufp^, nbufpart, 0); { Search #0 in the available buffer. }
    if zp >= 0 then nbufpart := zp; { #0 found, copy up to it (otherwise copy the entire available buffer, and don’t end). }
    if Assigned(sp) then
    begin
      nmove := 255 - length(sp^);
      if nmove > nbufpart then nmove := nbufpart;
      Move(bufp^, sp^[1 + length(sp^)], nmove);
      inc(byte(sp^[0]), nmove);
    end;
    inc(ReadString, nbufpart);
    inc(bufp, nbufpart);
  until zp >= 0;
  inc(bufp); { Null terminator. }
end;


{ Reads an unsigned Half from the current input stream }
function TEReader.ReadUHalf() : Word;
begin
  ReadNext(ReadUHalf, sizeof(ReadUHalf));
end;

function TEReader.ReadNextBuffer : longint;
begin
  inc(bufstart, bufp - pByte(buf));
  ReadNextBuffer := limit - bufstart;
  if ReadNextBuffer > 0 then begin
    if ReadNextBuffer > length(buf) then ReadNextBuffer := length(buf);
    BlockRead(e.f, buf, ReadNextBuffer, ReadNextBuffer);
  end;
  bufp := pByte(buf);
  bufe := pByte(buf) + ReadNextBuffer;
end;


{---------------------------------------------------------------------------

 Generic Dwarf lineinfo reader

 The line info reader is based on the information contained in

   DWARF Debugging Information Format Version 3
   Chapter 6.2 "Line Number Information"

 from the

   DWARF Debugging Information Format Workgroup.

 For more information on this document see also

   http://dwarf.freestandards.org/

---------------------------------------------------------------------------}

{ initializes the line info state to the default values }
procedure InitStateRegisters(var state : TMachineState; const aIs_Stmt : Bool8);
begin
  with state do begin
    address := 0;
    segment := 0;
    file_id := 1;
    line := 1;
    column := 0;
    is_stmt := aIs_Stmt;
    basic_block := false;
    end_sequence := false;
    prolouge_end := false;
    epilouge_begin := false;
    isa := 0;
    append_row := false;
  end;
end;


{ Skips all line info directory entries }
procedure SkipDirectories(var er: TEReader);
{$ifdef DEBUG_DWARF_PARSER}
var s : ShortString;
{$endif}
begin
  while er.ReadString({$ifdef DEBUG_DWARF_PARSER} @s {$else} nil {$endif}) <> 0 do begin
    DEBUG_WRITELN('Skipping directory : ', s);
  end;
end;

{ Skips the filename section from the current file stream }
procedure SkipFilenames(var er: TEReader);
{$ifdef DEBUG_DWARF_PARSER}
var s : ShortString;
{$endif}
begin
  while er.ReadString({$ifdef DEBUG_DWARF_PARSER} @s {$else} nil {$endif}) <> 0 do begin
    DEBUG_WRITELN('Skipping filename : ', s);
    er.SkipLEB128s(3); { skip the (1) directory index for the file, (2) last modification time for file, (3) length of file }
  end;
end;

function CalculateAddressIncrement(opcode : Byte; const header : TLineNumberProgramHeader64) : Int64;
begin
  CalculateAddressIncrement := (Int64(opcode) - header.opcode_base) div header.line_range * header.minimum_instruction_length;
end;

function GetFullFilename(var er: TEReader; const filenameStart, directoryStart : Int64; const file_id : DWord) : ShortString;
var
  i : DWord;
  filename, directory : ShortString;
  dirindex : Int64;
begin
  filename := '';
  directory := '';
  i := 1;
  er.Seek(filenameStart);
  while (i <= file_id) and (er.ReadString(@filename) <> 0) do begin
    DEBUG_WRITELN('Found "', filename, '"');
    dirindex := er.ReadLEB128(); { read the directory index for the file }
    er.SkipLEB128s(2); { skip (1) last modification time for file, (2) length of file }
    inc(i);
  end;
  { if we could not find the file index, exit }
  if (filename = '') then begin
    GetFullFilename := '(Unknown file)';
    exit;
  end;

  er.Seek(directoryStart);
  i := 1;
  while (i <= dirindex) and (er.ReadString(@directory) <> 0) do begin
    inc(i);
  end;
  if (directory<>'') and (directory[length(directory)]<>'/') then
    directory:=directory+'/';
  GetFullFilename := directory + filename;
end;


function ParseCompilationUnit(var er : TEReader; const addr : TOffset; const segment : TSegment; const file_offset : QWord;
  var source : ShortString; var line : longint; var found : Boolean) : QWord;
var
  state : TMachineState;
  { we need both headers on the stack, although we only use the 64 bit one internally }
  header64 : TLineNumberProgramHeader64;
  header32 : TLineNumberProgramHeader32;

  adjusted_opcode : Int64;

  opcode : PtrInt;
  extended_opcode : PtrInt;
  extended_opcode_length : PtrInt;
  i, addrIncrement, lineIncrement : PtrInt;

  {$ifdef DEBUG_DWARF_PARSER}
  s : ShortString;
  {$endif}

  numoptable : array[1..255] of Byte;
  { the offset into the file where the include directories are stored for this compilation unit }
  include_directories : QWord;
  { the offset into the file where the file names are stored for this compilation unit }
  file_names : Int64;

  temp_length : DWord;
  unit_length : QWord;
  header_length : SizeInt;

  first_row : Boolean;

  prev_line : QWord;
  prev_file : DWord;

begin
  prev_line := 0;
  prev_file := 0;
  first_row := true;

  found := false;

  er.ReadNext(temp_length, sizeof(temp_length));
  if (temp_length <> $ffffffff) then begin
    unit_length := temp_length + sizeof(temp_length)
  end else begin
    er.ReadNext(unit_length, sizeof(unit_length));
    inc(unit_length, 12);
  end;

  ParseCompilationUnit := file_offset + unit_length;

  er.SetRange(file_offset, unit_length);

  DEBUG_WRITELN('Unit length: ', unit_length);
  if (temp_length <> $ffffffff) then begin
    DEBUG_WRITELN('32 bit DWARF detected');
    er.ReadNext(header32, sizeof(header32));
    header64.magic := $ffffffff;
    header64.unit_length := header32.unit_length;
    header64.version := header32.version;
    header64.length := header32.length;
    header64.minimum_instruction_length := header32.minimum_instruction_length;
    header64.default_is_stmt := header32.default_is_stmt;
    header64.line_base := header32.line_base;
    header64.line_range := header32.line_range;
    header64.opcode_base := header32.opcode_base;
    header_length :=
      sizeof(header32.length) + sizeof(header32.version) +
      sizeof(header32.unit_length);
  end else begin
    DEBUG_WRITELN('64 bit DWARF detected');
    er.ReadNext(header64, sizeof(header64));
    header_length :=
      sizeof(header64.magic) + sizeof(header64.version) +
      sizeof(header64.length) + sizeof(header64.unit_length);
  end;

  inc(header_length, header64.length);

  fillchar(numoptable, sizeof(numoptable), #0);
  er.ReadNext(numoptable, header64.opcode_base-1);
  DEBUG_WRITELN('Opcode parameter count table');
  for i := 1 to header64.opcode_base-1 do begin
    DEBUG_WRITELN('Opcode[', i, '] - ', numoptable[i], ' parameters');
  end;

  DEBUG_WRITELN('Reading directories...');
  include_directories := er.Pos();
  SkipDirectories(er);
  DEBUG_WRITELN('Reading filenames...');
  file_names := er.Pos();
  SkipFilenames(er);

  er.Seek(header_length);

  with header64 do begin
    InitStateRegisters(state, default_is_stmt);
  end;
  opcode := er.ReadNext();
  while (opcode <> -1) and (not found) do begin
    DEBUG_WRITELN('Next opcode: ');
    case (opcode) of
      { extended opcode }
      0 : begin
        extended_opcode_length := er.ReadULEB128();
        extended_opcode := er.ReadNext();
        case (extended_opcode) of
          -1: begin
            exit;
          end;
          DW_LNE_END_SEQUENCE : begin
            state.end_sequence := true;
            state.append_row := true;
            DEBUG_WRITELN('DW_LNE_END_SEQUENCE');
          end;
          DW_LNE_SET_ADDRESS : begin
            state.address := er.ReadAddress(extended_opcode_length-1);
            DEBUG_WRITELN('DW_LNE_SET_ADDRESS (', hexstr(state.address, sizeof(state.address)*2), ')');
          end;
{$ifdef CPUI8086}
          DW_LNE_SET_SEGMENT : begin
            state.segment := er.ReadSegment();
            DEBUG_WRITELN('DW_LNE_SET_SEGMENT (', hexstr(state.segment, sizeof(state.segment)*2), ')');
          end;
{$endif CPUI8086}
          DW_LNE_DEFINE_FILE : begin
            er.ReadString({$ifdef DEBUG_DWARF_PARSER}@s{$else}nil{$endif});
            er.SkipLEB128s(3);
            DEBUG_WRITELN('DW_LNE_DEFINE_FILE (', s, ')');
          end;
          else begin
            DEBUG_WRITELN('Unknown extended opcode (opcode ', extended_opcode, ' length ', extended_opcode_length, ')');
            if (extended_opcode_length>1) and not er.ReadNext(nil^, extended_opcode_length-1) then
              exit;
          end;
        end;
      end;
      DW_LNS_COPY : begin
        state.basic_block := false;
        state.prolouge_end := false;
        state.epilouge_begin := false;
        state.append_row := true;
        DEBUG_WRITELN('DW_LNS_COPY');
      end;
      DW_LNS_ADVANCE_PC : begin
        inc(state.address, er.ReadULEB128() * header64.minimum_instruction_length);
        DEBUG_WRITELN('DW_LNS_ADVANCE_PC (', hexstr(state.address, sizeof(state.address)*2), ')');
      end;
      DW_LNS_ADVANCE_LINE : begin
        // inc(state.line, ReadLEB128()); negative values are allowed
        // but those may generate a range check error
        state.line := state.line + er.ReadLEB128();
        DEBUG_WRITELN('DW_LNS_ADVANCE_LINE (', state.line, ')');
      end;
      DW_LNS_SET_FILE : begin
        state.file_id := er.ReadULEB128();
        DEBUG_WRITELN('DW_LNS_SET_FILE (', state.file_id, ')');
      end;
      DW_LNS_SET_COLUMN : begin
        state.column := er.ReadULEB128();
        DEBUG_WRITELN('DW_LNS_SET_COLUMN (', state.column, ')');
      end;
      DW_LNS_NEGATE_STMT : begin
        state.is_stmt := not state.is_stmt;
        DEBUG_WRITELN('DW_LNS_NEGATE_STMT (', state.is_stmt, ')');
      end;
      DW_LNS_SET_BASIC_BLOCK : begin
        state.basic_block := true;
        DEBUG_WRITELN('DW_LNS_SET_BASIC_BLOCK');
      end;
      DW_LNS_CONST_ADD_PC : begin
        inc(state.address, CalculateAddressIncrement(255, header64));
        DEBUG_WRITELN('DW_LNS_CONST_ADD_PC (', hexstr(state.address, sizeof(state.address)*2), ')');
      end;
      DW_LNS_FIXED_ADVANCE_PC : begin
        inc(state.address, er.ReadUHalf());
        DEBUG_WRITELN('DW_LNS_FIXED_ADVANCE_PC (', hexstr(state.address, sizeof(state.address)*2), ')');
      end;
      DW_LNS_SET_PROLOGUE_END : begin
        state.prolouge_end := true;
        DEBUG_WRITELN('DW_LNS_SET_PROLOGUE_END');
      end;
      DW_LNS_SET_EPILOGUE_BEGIN : begin
        state.epilouge_begin := true;
        DEBUG_WRITELN('DW_LNS_SET_EPILOGUE_BEGIN');
      end;
      DW_LNS_SET_ISA : begin
        state.isa := er.ReadULEB128();
        DEBUG_WRITELN('DW_LNS_SET_ISA (', state.isa, ')');
      end;
      else begin { special opcode }
        if (opcode < header64.opcode_base) then begin
          DEBUG_WRITELN('Unknown standard opcode $', hexstr(opcode, 2), '; skipping');
          er.SkipLEB128s(numoptable[opcode]);
        end else begin
          adjusted_opcode := opcode - header64.opcode_base;
          addrIncrement := CalculateAddressIncrement(opcode, header64);
          inc(state.address, addrIncrement);
          lineIncrement := header64.line_base + (adjusted_opcode mod header64.line_range);
          inc(state.line, lineIncrement);
          DEBUG_WRITELN('Special opcode $', hexstr(opcode, 2), ' address increment: ', addrIncrement, ' new line: ', lineIncrement);
          state.basic_block := false;
          state.prolouge_end := false;
          state.epilouge_begin := false;
          state.append_row := true;
        end;
      end;
    end;

    if (state.append_row) then begin
      DEBUG_WRITELN('Current state : address = ', hexstr(state.address, sizeof(state.address) * 2),
{$ifdef CPUI8086}
      DEBUG_COMMENT ' segment = ', hexstr(state.segment, sizeof(state.segment) * 2),
{$endif CPUI8086}
      DEBUG_COMMENT ' file_id = ', state.file_id, ' line = ', state.line, ' column = ', state.column,
      DEBUG_COMMENT  ' is_stmt = ', state.is_stmt, ' basic_block = ', state.basic_block,
      DEBUG_COMMENT  ' end_sequence = ', state.end_sequence, ' prolouge_end = ', state.prolouge_end,
      DEBUG_COMMENT  ' epilouge_begin = ', state.epilouge_begin, ' isa = ', state.isa);

      if (first_row) then begin
        if (state.segment > segment) or
           ((state.segment = segment) and
            (state.address > addr)) then
          break;
        first_row := false;
      end;

      { when we have found the address we need to return the previous
        line because that contains the call instruction
        Note that there may not be any call instruction, because this may
        be the actual instruction that crashed, and it may be on the first
        line of the function }
      if (state.segment > segment) or
         ((state.segment = segment) and
          (state.address >= addr)) then
        found:=true
      else
        begin
          { save line information }
          prev_file := state.file_id;
          prev_line := state.line;
        end;

      state.append_row := false;
      if (state.end_sequence) then begin
        InitStateRegisters(state, header64.default_is_stmt);
        first_row := true;
      end;
    end;

    opcode := er.ReadNext();
  end;

  if (found) then
    begin
      { can happen if the crash happens on the first instruction with line info }
      if prev_line = 0 then
        begin
          prev_line := state.line;
          prev_file := state.file_id;
        end;
      line := prev_line;
      source := GetFullFilename(er, file_names, include_directories, prev_file);
    end;
end;


type
  TAbbrevRec = record
    firstAttr: uint32; { Offset in Attrs. }
    nAttrs: uint16;
    tag, children: byte;
  end;
  TAttrRec = record
    attr, form: byte;
  end;
  TAbbrevs = record
    Abbrevs: array of TAbbrevRec;
    { Attributes of Abbrevs[i] are Attrs[Abbrevs[i].firstAttr .. Abbrevs[i].firstAttr + Abbrevs[i].nAttrs - 1]. }
    Attrs: array of TAttrRec;
    nAbbrevs, nAttrs: SizeInt;
  end;

procedure ReadAbbrevTable(var er: TEReader; var abbrevs: TAbbrevs);
  var
   tag,
   nr,
   attr,
   form : Int64;
   curAbbrev : ^TAbbrevRec;
   curAttr : ^TAttrRec;
  begin
    { Clear the old data and reuse allocations. Assumes nAbbrevs is initialized to 0 before the first use (see GetLineInfo);
      on subsequent uses nAbbrevs value left from the previous use is the exact amount of Abbrevs cells to clear. }
    FillChar(pointer(abbrevs.Abbrevs)^,abbrevs.nAbbrevs*sizeof(TAbbrevRec),0);
    abbrevs.nAbbrevs:=0;
    abbrevs.nAttrs:=0; { Clearing Attrs is not required because they are always filled sequentially while Abbrevs can be filled at random. }
    DEBUG_WRITELN('Starting to read abbrev. section at $',hexstr(Dwarf_Debug_Abbrev_Section_Offset+er.Pos,16));
    repeat
      nr:=er.ReadULEB128;
      if nr=0 then
        break;

      if nr>=abbrevs.nAbbrevs then
        begin
          abbrevs.nAbbrevs:=nr+1;
          if nr>high(abbrevs.Abbrevs) then
          begin
            if nr>8*1024*1024 then { Safeguard. }
            begin
              DEBUG_WRITELN('Implausible abbreviation ID (', nr, ') — GIVING UP.');
              exit;
            end;
            SetLength(abbrevs.Abbrevs,SizeUint(nr)+128+SizeUint(nr) div 4);
          end;
        end;

      { read tag }
      tag:=er.ReadULEB128;
      DEBUG_WRITELN('Abbrev ',nr,' at offset ',er.Pos,' has tag $',hexstr(tag,4));
      if tag>High(TAbbrevRec.tag) then
        tag:=High(TAbbrevRec.tag); { Values outside the byte range aren’t used anyway (in fact, only DW_TAG_subprogram is tested); $ff is meaningless enough. }
      curAbbrev:=@abbrevs.Abbrevs[nr];
      curAbbrev^.tag:=tag;
      { read flag for children }
      curAbbrev^.children:=er.ReadNext;
      { ensure that the entry has no attributes if overwritten (not sure if this will ever happen) }
      curAbbrev^.firstAttr:=abbrevs.nAttrs;
      curAbbrev^.nAttrs:=0;
      repeat
        attr:=er.ReadULEB128;
        if attr>High(TAttrRec.attr) then
          attr:=High(TAttrRec.attr);
        form:=er.ReadULEB128;
        if form>High(TAttrRec.form) then
          form:=High(TAttrRec.form);
        if attr=0 then
          break;
        if SizeInt(abbrevs.nAttrs)>High(abbrevs.Attrs) then
          SetLength(abbrevs.Attrs, SizeInt(abbrevs.nAttrs)+128+SizeInt(SizeUint(abbrevs.nAttrs) div 4));
        curAttr:=@abbrevs.Attrs[abbrevs.nAttrs];
        curAttr^.attr:=attr;
        curAttr^.form:=form;
        inc(abbrevs.nAttrs);
        inc(curAbbrev^.nAttrs);
      until false;
      DEBUG_WRITELN('Abbrev ',nr,' has ',curAbbrev^.nAttrs,' attributes');
    until false;
  end;


function ParseCompilationUnitForDebugInfoOffset(var er: TEReader; const addr : TOffset; const segment : TSegment; const file_offset : QWord;
  var debug_info_offset : QWord; var found : Boolean) : QWord;
{$ifndef CPUI8086}
const
  arange_segment = 0;
{$endif CPUI8086}
var
  { we need both headers on the stack, although we only use the 64 bit one internally }
  header64 : TDebugArangesHeader64;
  header32 : TDebugArangesHeader32;
  temp_length : DWord;
  unit_length : QWord;
{$ifdef CPUI8086}
  arange_start, arange_size: DWord;
  arange_segment: Word;
{$else CPUI8086}
  arange_start, arange_size: PtrUInt;
{$endif CPUI8086}
begin
  found := false;

  er.ReadNext(temp_length, sizeof(temp_length));
  if (temp_length <> $ffffffff) then begin
    unit_length := temp_length + sizeof(temp_length)
  end else begin
    er.ReadNext(unit_length, sizeof(unit_length));
    inc(unit_length, 12);
  end;

  ParseCompilationUnitForDebugInfoOffset := file_offset + unit_length;

  er.SetRange(file_offset, unit_length);

  DEBUG_WRITELN('Unit length: ', unit_length);
  if (temp_length <> $ffffffff) then
    begin
      DEBUG_WRITELN('32 bit DWARF detected');
      er.ReadNext(header32, sizeof(header32));
      header64.magic := $ffffffff;
      header64.unit_length := header32.unit_length;
      header64.version := header32.version;
      header64.debug_info_offset := header32.debug_info_offset;
      header64.address_size := header32.address_size;
      header64.segment_size := header32.segment_size;
    end
  else
    begin
      DEBUG_WRITELN('64 bit DWARF detected');
      er.ReadNext(header64, sizeof(header64));
    end;

  DEBUG_WRITELN('debug_info_offset: ',header64.debug_info_offset);
  DEBUG_WRITELN('address_size: ', header64.address_size);
  DEBUG_WRITELN('segment_size: ', header64.segment_size);
  arange_start:=er.ReadAddress(header64.address_size);
{$ifdef CPUI8086}
  arange_segment:=er.ReadSegment();
{$endif CPUI8086}
  arange_size:=er.ReadAddress(header64.address_size);

  while not((arange_start=0) and (arange_segment=0) and (arange_size=0)) and (not found) do
    begin
      if (segment=arange_segment) and (addr>=arange_start) and (addr<=arange_start+arange_size) then
        begin
          found:=true;
          debug_info_offset:=header64.debug_info_offset;
          DEBUG_WRITELN('Matching aranges entry $',hexStr(arange_start,header64.address_size*2),', $',hexStr(arange_size,header64.address_size*2));
        end;

      arange_start:=er.ReadAddress(header64.address_size);
{$ifdef CPUI8086}
      arange_segment:=er.ReadSegment();
{$endif CPUI8086}
      arange_size:=er.ReadAddress(header64.address_size);
    end;
end;

function ParseCompilationUnitForFunctionName(var er: TEReader; const addr : TOffset; const segment : TSegment; const file_offset : QWord;
  var abbrevs: TAbbrevs; { Just to reuse array allocations made by previous calls; otherwise could be local. }
  var func : ShortString; var found : Boolean) : QWord;
var
  { we need both headers on the stack, although we only use the 64 bit one internally }
  header64 : TDebugInfoProgramHeader64;
  header32 : TDebugInfoProgramHeader32;
  isdwarf64 : boolean;
  abbrev,
  high_pc,
  low_pc : QWord;
  temp_length : DWord;
  unit_length : QWord;
  name : ShortString;
  level : Integer;

procedure SkipAttr(var er: TEReader; form : QWord);
  var
    dl,nskip : dword;
  begin
    nskip := 0;
    case form of
      DW_FORM_addr:
        nskip := header64.address_size;
      DW_FORM_block2:
        nskip := er.ReadUHalf;
      DW_FORM_block4:
        begin
          er.ReadNext(dl,SizeOf(dl));
          nskip := dl;
        end;
      DW_FORM_data2, DW_FORM_data4, DW_FORM_data8:
        nskip := 2 shl (form - DW_FORM_data2);
      DW_FORM_string:
        er.ReadString(nil);
      DW_FORM_block,
      DW_FORM_exprloc:
        nskip := er.ReadULEB128;
      DW_FORM_block1:
        nskip := er.ReadNext;
      DW_FORM_data1,
      DW_FORM_flag:
        er.ReadNext;
      DW_FORM_sdata:
        er.ReadLEB128;
      DW_FORM_ref_addr:
        { the size of DW_FORM_ref_addr changed between DWAWRF2 and later versions:
          in DWARF2 it depends on the architecture address size, in later versions on the DWARF type (32 bit/64 bit)
        }
        if header64.version>2 then
          begin
            if isdwarf64 then
              nskip := 8
            else
              nskip := 4;
          end
        else
          begin
            { address size for DW_FORM_ref_addr must be at least 32 bits }
            { this is compatible with Open Watcom on i8086 }
            if header64.address_size<4 then
              nskip := 4
            else
              nskip := header64.address_size;
          end;
      DW_FORM_strp,
      DW_FORM_sec_offset:
        if isdwarf64 then
          nskip := 8
        else
          nskip := 4;
      DW_FORM_udata,
      DW_FORM_ref_udata:
        er.SkipLEB128s(1);
      DW_FORM_ref1, DW_FORM_ref2, DW_FORM_ref4, DW_FORM_ref8:
        nskip := 1 shl (form - DW_FORM_ref1);
      DW_FORM_indirect:
        SkipAttr(er, er.ReadULEB128);
      DW_FORM_flag_present: {none};
      else
        begin
          writeln(stderr,'Internal error: unknown dwarf form: $',hexstr(form,2));
          nskip := 1;
        end;
    end;
    er.ReadNext(nil^, nskip);
  end;

var
  i : PtrInt;
  prev_base,prev_size,prev_pos : TFilePos;
  curAbbrev : ^TAbbrevRec;

begin
  found := false;

  er.ReadNext(temp_length, sizeof(temp_length));
  if (temp_length <> $ffffffff) then begin
    unit_length := temp_length + sizeof(temp_length)
  end else begin
    er.ReadNext(unit_length, sizeof(unit_length));
    inc(unit_length, 12);
  end;

  ParseCompilationUnitForFunctionName := file_offset + unit_length;

  er.SetRange(file_offset, unit_length);

  DEBUG_WRITELN('Unit length: ', unit_length);
  if (temp_length <> $ffffffff) then begin
    DEBUG_WRITELN('32 bit DWARF detected');
    er.ReadNext(header32, sizeof(header32));
    header64.magic := $ffffffff;
    header64.unit_length := header32.unit_length;
    header64.version := header32.version;
    header64.debug_abbrev_offset := header32.debug_abbrev_offset;
    header64.address_size := header32.address_size;
    isdwarf64:=false;
  end else begin
    DEBUG_WRITELN('64 bit DWARF detected');
    er.ReadNext(header64, sizeof(header64));
    isdwarf64:=true;
  end;

  DEBUG_WRITELN('debug_abbrev_offset: ',header64.debug_abbrev_offset);
  DEBUG_WRITELN('address_size: ',header64.address_size);

  { not nice, but we have to read the abbrev section after the start of the debug_info section has been read }
  prev_size:=er.limit-er.base;
  prev_base:=er.base;
  prev_pos:=er.Pos;
  er.SetRange(Dwarf_Debug_Abbrev_Section_Offset+header64.debug_abbrev_offset,Dwarf_Debug_Abbrev_Section_Size);
  ReadAbbrevTable(er, abbrevs);

  { restore previous reading state and position }
  er.SetRange(prev_base,prev_size);
  er.Seek(prev_pos);

  abbrev:=er.ReadULEB128;
  level:=0;
  while (abbrev <> 0) and (not found) do
    begin
      if abbrev>=SizeUint(abbrevs.nAbbrevs) then { Safeguard. }
      begin
        DEBUG_WRITELN('Bad abbreviation ID (', abbrev, ')!');
        exit;
      end;
      DEBUG_WRITELN('Next abbrev: ',abbrev);
      curAbbrev:=@abbrevs.Abbrevs[abbrev];
      if curAbbrev^.children<>0 then
        inc(level);
      { DW_TAG_subprogram? }
      if curAbbrev^.tag=$2e then
        begin
          low_pc:=1;
          high_pc:=0;
          name:='';
          for i:=PtrInt(curAbbrev^.firstAttr) to PtrInt(curAbbrev^.firstAttr)+PtrInt(curAbbrev^.nAttrs)-1 do
            case abbrevs.Attrs[i].attr or uint32(abbrevs.Attrs[i].form) shl 8 of { Dispatch on attr and form at once. }
              $11 or DW_FORM_addr shl 8: { DW_AT_low_pc }
                begin
                  low_pc:=0;
                  er.ReadNext(low_pc,header64.address_size);
                end;
              $12 or DW_FORM_addr shl 8: { DW_AT_high_pc }
                begin
                  high_pc:=0;
                  er.ReadNext(high_pc,header64.address_size);
                end;
              $3 or DW_FORM_string shl 8: { DW_AT_name; avoid that we accidentally read an DW_FORM_strp entry }
                begin
                  er.ReadString(@name);
                end;
              else
                SkipAttr(er, abbrevs.Attrs[i].form);
            end;
          DEBUG_WRITELN('Got DW_TAG_subprogram with low pc = $',hexStr(low_pc,header64.address_size*2),', high pc = $',hexStr(high_pc,header64.address_size*2),', name = ',name);
          if (addr>=low_pc) and (addr<high_pc) then
            begin
              found:=true;
              func:=name;
            end;
        end
      else
        begin
          for i:=PtrInt(curAbbrev^.firstAttr) to PtrInt(curAbbrev^.firstAttr)+PtrInt(curAbbrev^.nAttrs)-1 do
            SkipAttr(er, abbrevs.Attrs[i].form);
        end;
      abbrev:=er.ReadULEB128;
      { skip entries signaling that no more child entries are following }
      while (level>0) and (abbrev=0) do
        begin
          dec(level);
          abbrev:=er.ReadULEB128;
        end;
    end;
end;


function GetLineInfo(addr : codeptruint; var func, source : shortstring; var line : longint) : boolean;
var
  current_offset,
  end_offset, debug_info_offset_from_aranges : QWord;
  segment : Word = 0;

  found, found_aranges : Boolean;
  er: TEReader;
  abbrevs: TAbbrevs; { To reuse allocations between ParseCompilationUnitForFunctionName calls. }

begin
{$ifdef has_LineInfoCache}
  Lock(LiCacheLock);
  result := liCache.TryGet(CodePointer(addr), func, source, line);
  Unlock(LiCacheLock);
  if result then exit((func <> '') or (source <> '') or (line <> 0));
{$endif has_LineInfoCache}

  func := '';
  source := '';
  line := 0;
  GetLineInfo:=false;

  Lock(DwarfLock);
  if not OpenDwarf(codepointer(addr)) then
  begin
    Unlock(DwarfLock);
  {$ifdef has_LineInfoCache}
    Lock(LiCacheLock);
    liCache.Put(CodePointer(addr), '', '', 0); { Cache the failure, too; in particular, on Win64 some frames above main() point into kernel32 or something. }
    Unlock(LiCacheLock);
  {$endif has_LineInfoCache}
    exit;
  end;

{$ifdef CPUI8086}
  {$if defined(FPC_MM_MEDIUM) or defined(FPC_MM_LARGE) or defined(FPC_MM_HUGE)}
    segment := (addr shr 16) - e.processsegment;
    addr := Word(addr);
  {$else}
    segment := CSeg - e.processsegment;
  {$endif}
{$endif CPUI8086}

  addr := addr - e.processaddress;

  current_offset := Dwarf_Debug_Line_Section_Offset;
  end_offset := Dwarf_Debug_Line_Section_Offset + Dwarf_Debug_Line_Section_Size;

  er.Init;
  found := false;
  while (current_offset < end_offset) and (not found) do begin
    er.SetRange(current_offset, end_offset - current_offset);
    current_offset := ParseCompilationUnit(er, addr, segment, current_offset,
      source, line, found);
  end;

  current_offset := Dwarf_Debug_Aranges_Section_Offset;
  end_offset := Dwarf_Debug_Aranges_Section_Offset + Dwarf_Debug_Aranges_Section_Size;

  found_aranges := false;
  while (current_offset < end_offset) and (not found_aranges) do begin
    er.SetRange(current_offset, end_offset - current_offset);
    current_offset := ParseCompilationUnitForDebugInfoOffset(er, addr, segment, current_offset, debug_info_offset_from_aranges, found_aranges);
  end;

  { no function name found yet }
  found := false;
  abbrevs.nAbbrevs := 0; { For reuse between ParseCompilationUnitForFunctionName calls. Other things don’t require initialization. }

  if found_aranges then
    begin
      DEBUG_WRITELN('Found .debug_info offset $',hexstr(debug_info_offset_from_aranges,8),' from .debug_aranges');
      current_offset := Dwarf_Debug_Info_Section_Offset + debug_info_offset_from_aranges;
      end_offset := Dwarf_Debug_Info_Section_Offset + debug_info_offset_from_aranges + Dwarf_Debug_Info_Section_Size;

      DEBUG_WRITELN('Reading .debug_info at section offset $',hexStr(current_offset-Dwarf_Debug_Info_Section_Offset,16));

      er.SetRange(current_offset, end_offset - current_offset);
      current_offset := ParseCompilationUnitForFunctionName(er, addr, segment, current_offset, abbrevs, func, found);
      if found then
        DEBUG_WRITELN('Found .debug_info entry by using .debug_aranges information');
    end
  else
    DEBUG_WRITELN('No .debug_info offset found from .debug_aranges');

  current_offset := Dwarf_Debug_Info_Section_Offset;
  end_offset := Dwarf_Debug_Info_Section_Offset + Dwarf_Debug_Info_Section_Size;

  while (current_offset < end_offset) and (not found) do begin
    DEBUG_WRITELN('Reading .debug_info at section offset $',hexStr(current_offset-Dwarf_Debug_Info_Section_Offset,16));

    er.SetRange(current_offset, end_offset - current_offset);
    current_offset := ParseCompilationUnitForFunctionName(er, addr, segment, current_offset, abbrevs, func, found);
  end;

  if not AllowReuseOfLineInfoData then
    CloseDwarf(false);
  Unlock(DwarfLock);

{$ifdef has_LineInfoCache}
  Lock(LiCacheLock);
  liCache.Put(CodePointer(addr), func, source, line);
  Unlock(LiCacheLock);
{$endif has_LineInfoCache}
  GetLineInfo:=true;
end;


function DwarfBackTraceStr(addr: CodePointer): shortstring;
var
  func,
  source,
  hs : shortstring;
  line   : longint;
  Store  : TBackTraceStrFunc;
  Success : boolean;
begin
  {$ifdef DEBUG_LINEINFO}
  writeln(stderr,'DwarfBackTraceStr called');
  {$endif DEBUG_LINEINFO}
  { reset to prevent infinite recursion if problems inside the code }
  Success:=false;
  Store := BackTraceStrFunc;
  BackTraceStrFunc := @SysBackTraceStr;
  Success:=GetLineInfo(codeptruint(addr), func, source, line);
  { create string }
  DwarfBackTraceStr :='  $' + HexStr(addr);
  if Success then
  begin
    if func<>'' then
      DwarfBackTraceStr := DwarfBackTraceStr + '  ' + func;
    if source<>'' then
    begin
      if func<>'' then
        DwarfBackTraceStr := DwarfBackTraceStr + ', ';
      if line<>0 then
      begin
        str(line, hs);
        DwarfBackTraceStr := DwarfBackTraceStr + ' line ' + hs;
      end;
      DwarfBackTraceStr := DwarfBackTraceStr + ' of ' + source;
    end;
  end;
  BackTraceStrFunc := Store;
end;


{$ifdef has_LineInfoCache}
{$push} {$q-,r-}
  class function LineInfoCache.HashMemory(p: pointer; n: SizeUint): SizeUint;
  const
    C1 = uint32($cc9e2d51);
    C2 = uint32($1b873593);
  var
    tail: uint32;
    rem: SizeUint;
  begin
    result := 0;

    rem := n div 4;
    while rem > 0 do
    begin
      result := RolDWord(result xor (RolDWord(unaligned(pUint32(p)^) * C1, 15) * C2), 13) * 5 + $e6546b64;
      inc(p, 4); dec(rem);
    end;

    rem := n mod 4;
    if rem > 0 then
    begin
      tail := pByte(p)^;
      if rem > 1 then inc(tail, unaligned(uint32(pUint16(p + rem - 2)^) shl 8));
      result := result xor (RolDWord(tail * C1, 15) * C2);
    end;

    result := result xor n;
    result := (result xor (result shr 16)) * $85ebca6b;
    result := (result xor (result shr 13)) * $c2b2ae35;
    result := result xor (result shr 16);
  end;

  class function LineInfoCache.HashCodePointer(p: CodePointer): SizeUint;
  begin
    result := (CodePtrUint(p) xor CodePtrUint(p) shr 16) * $21f0aaad;
    result := (result xor result shr 15) * $735a2d97;
    result := result xor result shr 15;
  end;
{$pop}

  procedure LineInfoCache.AddStringToLRU(id: SizeUint);
  var
    firstLru1: SizeUint;
  begin
    firstLru1 := strings.firstLru1;
    strings.s[id].prevLru1 := 0;
    strings.s[id].nextLru1 := firstLru1;
    inc(id);
    strings.firstLru1 := id;
    if firstLru1 = 0 then
      strings.lastLru1 := id
    else
      strings.s[firstLru1 - 1].prevLru1 := id;
  end;

  procedure LineInfoCache.RemoveStringFromLRU(id: SizeUint);
  var
    prevLru1, nextLru1: SizeUint;
  begin
    prevLru1 := strings.s[id].prevLru1;
    nextLru1 := strings.s[id].nextLru1;
    if prevLru1 = 0 then
      strings.firstLru1 := nextLru1
    else
      strings.s[prevLru1 - 1].nextLru1 := nextLru1;
    if nextLru1 = 0 then
      strings.lastLru1 := prevLru1
    else
      strings.s[nextLru1 - 1].prevLru1 := prevLru1;
  end;

  procedure LineInfoCache.BumpStringLRU(id: SizeUint);
  begin
    RemoveStringFromLRU(id);
    AddStringToLRU(id);
  end;

  function LineInfoCache.FindString(const s: shortstring; hash: SizeUint): SizeInt;
  var
    hp: pStringDataHeader;
  begin
    result := strings.hash1[hash and (StringsHashSize - 1)];
    while result <> 0 do
    begin
      pointer(hp) := @strings.data[strings.dataOfs[result - 1]];
      if (hp^.hash = StringDataHeaderHashType(hash)) and (hp^.len = length(s)) and (CompareByte(s[1], hp[1], hp^.len) = 0) then
        break;
      result := strings.nextCollision1[result - 1];
    end;
    dec(result);
  end;

  function LineInfoCache.AddString(const s: shortstring; hash: SizeUint): SizeUint;
  var
    dataOfs: SizeUint;
    hash1Cell: ^StringIndexType;
    hp: pStringDataHeader;
  begin
  {$ifdef debug_lineinfocache}
    Assert(SizeUint(StringHeaderSize + length(s)) <= SizeUint(MaxStringsData - strings.dataSize), 'not enough space');
    Assert(FindString(s, hash) < 0, 'already exists');
  {$endif}
    if strings.nFree <> 0 then
    begin
      result := strings.firstFree1 - 1;
      strings.firstFree1 := strings.s[result].nextFree1;
      dec(strings.nFree);
    end else
    begin
    {$ifdef debug_lineinfocache} Assert(strings.nUpto < MaxStrings, 'strings.nUpto past the limit'); {$endif}
      result := strings.nUpto;
      inc(strings.nUpto);
    end;

    // Add to strings.hash1.
    hash1Cell := @strings.hash1[hash and (StringsHashSize - 1)];
    strings.nextCollision1[result] := hash1Cell^;
    hash1Cell^ := 1 + result;

    // Add to strings.data.
    dataOfs := strings.dataSize;
    strings.dataOfs[result] := dataOfs;
    pointer(hp) := pByte(strings.data) + dataOfs;
    hp^.hash := StringDataHeaderHashType(hash);
    hp^.len := length(s);
    Move(s[1], hp[1], length(s));
    strings.dataSize := dataOfs + SizeUint(length(s) + StringHeaderSize);

    strings.firstUser1[result] := 0;
    AddStringToLRU(result);
  end;

  procedure LineInfoCache.RemoveString(id: SizeUint);
  var
    hLink1: ^StringIndexType;
  begin
    while strings.firstUser1[id] <> 0 do
      RemoveAddress(strings.firstUser1[id] - 1);

    // Remove from strings.hash1.
    hLink1 := @strings.hash1[pStringDataHeader(pByte(strings.data) + strings.dataOfs[id])^.hash and (StringsHashSize - 1)];
    while hLink1^ <> 1 + id do
    begin
    {$ifdef debug_lineinfocache} Assert(hLink1^ <> 0, 'not found in strings.hash1'); {$endif}
      hLink1 := @strings.nextCollision1[hLink1^ - 1];
    end;
    hLink1^ := strings.nextCollision1[id];

    // Careful: nextFree1 reuses the prevLru1 space, so removal from LRU must be performed before adding to nextFree1.
    RemoveStringFromLRU(id);

    // Add to strings.firstFree1.
    inc(strings.nFree);
    strings.s[id].nextFree1 := strings.firstFree1;
    strings.firstFree1 := 1 + id;
  end;

  procedure LineInfoCache.StringToShortstring(id: SizeUint; out ss: shortstring);
  var
    hp: pStringDataHeader;
  begin
    pointer(hp) := pByte(strings.data) + strings.dataOfs[id];
    SetLength(ss, hp^.len);
    Move(hp[1], ss[1], length(ss));
  end;

  procedure LineInfoCache.PackStrings;
  var
    dp, removed, moveN, id1, stringDataSize: SizeUint;
    hp: pStringDataHeader;
  begin
    dp := 0; removed := 0; moveN := 0;
    while dp < strings.dataSize do
    begin
      pointer(hp) := pByte(strings.data) + dp;
      stringDataSize := hp^.len + StringHeaderSize;
      id1 := strings.hash1[hp^.hash and (StringsHashSize - 1)];
      while (id1 <> 0) and (strings.dataOfs[id1 - 1] <> dp) do
        id1 := strings.nextCollision1[id1 - 1];
      if id1 <> 0 then // string alive
      begin
        strings.dataOfs[id1 - 1] := dp - removed;
        inc(moveN, stringDataSize);
      end else
      begin // string removed
        if (moveN > 0) and (removed > 0) then
          Move(strings.data[dp - moveN], strings.data[dp - moveN - removed], moveN);
        moveN := 0;
        inc(removed, stringDataSize);
      end;
      inc(dp, stringDataSize);
    end;
    if (moveN > 0) and (removed > 0) then
      Move(strings.data[dp - moveN], strings.data[dp - moveN - removed], moveN);
    dec(strings.dataSize, removed);
  end;

  procedure LineInfoCache.AddStringUser(stringId, addressId, iFileFunc: SizeUint);
  var
    nextUser1: SizeUint;
  begin
  {$ifdef debug_lineinfocache} Assert(stringId = addresses.fileFunc[addressId, iFileFunc]); {$endif}
    nextUser1 := strings.firstUser1[stringId];
    addresses.u[addressId].prevUser1[iFileFunc] := 0;
    addresses.u[addressId].nextUser1[iFileFunc] := nextUser1;
    if nextUser1 <> 0 then
      addresses.u[nextUser1 - 1].prevUser1[ord(stringId <> addresses.fileFunc[nextUser1 - 1, 0])] := 1 + addressId;
    strings.firstUser1[stringId] := 1 + addressId;
  end;

  procedure LineInfoCache.RemoveStringUser(stringId, addressId, iFileFunc: SizeUint);
  var
    prevUser1, nextUser1: SizeUint;
  begin
  {$ifdef debug_lineinfocache} Assert(stringId = addresses.fileFunc[addressId, iFileFunc]); {$endif}
    prevUser1 := addresses.u[addressId].prevUser1[iFileFunc];
    nextUser1 := addresses.u[addressId].nextUser1[iFileFunc];
    if prevUser1 = 0 then
      strings.firstUser1[stringId] := nextUser1
    else
      addresses.u[prevUser1 - 1].nextUser1[ord(stringId <> addresses.fileFunc[prevUser1 - 1, 0])] := nextUser1;
    if nextUser1 <> 0 then
      addresses.u[nextUser1 - 1].prevUser1[ord(stringId <> addresses.fileFUnc[nextUser1 - 1, 0])] := prevUser1;
  end;

  procedure LineInfoCache.AddAddressToLRU(id: SizeUint);
  var
    firstLru1: SizeUint;
  begin
    firstLru1 := addresses.firstLru1;
    addresses.a[id].prevLru1 := 0;
    addresses.a[id].nextLru1 := firstLru1;
    inc(id);
    addresses.firstLru1 := id;
    if firstLru1 = 0 then
      addresses.lastLru1 := id
    else
      addresses.a[firstLru1 - 1].prevLru1 := id;
  end;

  procedure LineInfoCache.RemoveAddressFromLRU(id: SizeUint);
  var
    prevLru1, nextLru1: SizeUint;
  begin
    prevLru1 := addresses.a[id].prevLru1;
    nextLru1 := addresses.a[id].nextLru1;
    if prevLru1 = 0 then
      addresses.firstLru1 := nextLru1
    else
      addresses.a[prevLru1 - 1].nextLru1 := nextLru1;
    if nextLru1 = 0 then
      addresses.lastLru1 := prevLru1
    else
      addresses.a[nextLru1 - 1].prevLru1 := prevLru1;
  end;

  function LineInfoCache.FindAddress(addr: CodePointer; hash: SizeUint): SizeInt;
  begin
    result := addresses.hash1[hash and (AddressesHashSize - 1)];
    while (result <> 0) and (addresses.p[result - 1] <> addr) do
      result := addresses.nextCollision1[result - 1];
    dec(result);
  end;

  function LineInfoCache.AddAddress(addr: CodePointer; hash, fileId, funcId: SizeUint; lineNo: uint32): SizeUint;
  var
    hash1Cell: ^AddressIndexType;
  begin
  {$ifdef debug_lineinfocache} Assert(FindAddress(addr, HashCodePointer(addr)) < 0, 'already exists'); {$endif}
    if addresses.nFree > 0 then
    begin
      result := addresses.firstFree1 - 1;
      addresses.firstFree1 := addresses.a[result].nextFree1;
      dec(addresses.nFree);
    end else
    begin
    {$ifdef debug_lineinfocache} Assert(addresses.nUpto < MaxAddresses, 'addresses.nUpto past the limit'); {$endif}
      result := addresses.nUpto;
      inc(addresses.nUpto);
    end;

    addresses.p[result] := addr;
    addresses.lineLo16[result] := uint16(lineNo);
    addresses.lineHi8[result] := lineNo shr 16;

    // Add to addresses.hash.
    hash1Cell := @addresses.hash1[hash and (AddressesHashSize - 1)];
    addresses.nextCollision1[result] := hash1Cell^;
    hash1Cell^ := 1 + result;

    addresses.fileFunc[result, 0] := fileId;
    addresses.fileFunc[result, 1] := funcId;
    AddStringUser(fileId, result, 0);
    if funcId <> fileId then AddStringUser(funcId, result, 1);

    AddAddressToLRU(result);
  end;

  procedure LineInfoCache.RemoveAddress(id: SizeUint);
  var
    hLink1: ^AddressIndexType;
  begin
    RemoveStringUser(addresses.fileFunc[id, 0], id, 0);
    if addresses.fileFunc[id, 0] <> addresses.fileFunc[id, 1] then
      RemoveStringUser(addresses.fileFunc[id, 1], id, 1);

    // Remove from addresses.hash.
    hLink1 := @addresses.hash1[HashCodePointer(addresses.p[id]) and (AddressesHashSize - 1)];
    while hLink1^ <> 1 + id do
    begin
    {$ifdef debug_lineinfocache} Assert(hLink1^ <> 0, 'not found in addresses.hash1'); {$endif}
      hLink1 := @addresses.nextCollision1[hLink1^ - 1];
    end;
    hLink1^ := addresses.nextCollision1[id];

    // Same as in RemoveString: nextFree1 reuses the prevLru1 space, careful.
    RemoveAddressFromLRU(id);

    // Add to addresses.firstFree1.
    inc(addresses.nFree);
    addresses.a[id].nextFree1 := addresses.firstFree1;
    addresses.firstFree1 := 1 + id;
  end;

{$ifdef debug_lineinfocache}
  procedure LineInfoCache.DumpStringValue(var f: text; id: SizeUint);
  var
    ss: shortstring;
  begin
    StringToShortstring(id, ss);
    write(f, ss);
  end;

  procedure LineInfoCache.DumpAddressValue(var f: text; id: SizeUint);
  begin
    write(f, HexStr(CodePtrUint(addresses.p[id]), 1 + {$if sizeof(CodePtrUint) >= 8} BsrQWord {$else} BsrDWord {$endif} (CodePtrUint(addresses.p[id]) or 1) div 4));
  end;

  procedure LineInfoCache.Dump(var f: text);
  var
    ih, id1, user1, nChain, maxChain, nChains: SizeUint;
  begin
    if strings.nUpto = strings.nFree then
      writeln(f, 'No strings (free ', strings.nFree, ')')
    else
    begin
      writeln(f, 'Strings hash:');
      nChains := 0;
      maxChain := 0;
      for ih := 0 to High(strings.hash1) do
      begin
        id1 := strings.hash1[ih];
        if id1 <> 0 then
        begin
          inc(nChains);
          nChain := 0;
          repeat
            inc(nChain);
            id1 := strings.nextCollision1[id1 - 1];
          until id1 = 0;
          if nChain > maxChain then maxChain := nChain;
          id1 := strings.hash1[ih];
          write(f, 'h[', HexStr(ih, 1 + BsrDWord(StringsHashSize - 1) div 4), ']');
          if nChain > 0 then write(f, ' (', nChain, ')');
          write(f, ' = ');
          DumpStringValue(f, id1 - 1);
          repeat
            id1 := strings.nextCollision1[id1 - 1];
            if id1 = 0 then break;
            write(f, ', ');
            DumpStringValue(f, id1 - 1);
          until false;
          writeln(f);
        end;
      end;
      writeln(f, 'Avg. chain length ', (strings.nUpto - strings.nFree) / nChains:0:2, ', max ', maxChain);
      write(f, LineEnding, 'Strings (', strings.nUpto - strings.nFree);
      if strings.nFree > 0 then write(f, ' + free ', strings.nFree);
      writeln(f, '), dataSize = ', strings.dataSize, ', LRU to MRU order:');
    end;
    id1 := strings.firstLru1;
    while id1 <> 0 do
    begin
      DumpStringValue(f, id1 - 1);
      write(f, ', id: ', id1 - 1, ', dataOfs: ', strings.dataOfs[id1 - 1], ', users: ');
      user1 := strings.firstUser1[id1 - 1];
      if user1 = 0 then
        writeln(f, '-')
      else
      begin
        write(f, '(');
        while user1 <> 0 do
        begin
          if user1 <> strings.firstUser1[id1 - 1] then write(f, ', ');
          DumpAddressValue(f, user1 - 1);
          user1 := addresses.u[user1 - 1].nextUser1[ord(id1 <> SizeUint(1) + addresses.fileFunc[user1 - 1, 0])];
        end;
        writeln(f, ')');
      end;
      id1 := strings.s[id1 - 1].nextLru1;
    end;
    if (strings.nUpto <> strings.nFree) or (addresses.nUpto <> addresses.nFree) then writeln(f);

    if addresses.nUpto = addresses.nFree then
      writeln(f, 'No addresses (free ', addresses.nFree, ')')
    else
    begin
      writeln(f, 'Addresses hash:');
      nChains := 0;
      maxChain := 0;
      for ih := 0 to High(addresses.hash1) do
      begin
        id1 := addresses.hash1[ih];
        if id1 <> 0 then
        begin
          inc(nChains);
          nChain := 0;
          repeat
            inc(nChain);
            id1 := addresses.nextCollision1[id1 - 1];
          until id1 = 0;
          if nChain > maxChain then maxChain := nChain;
          id1 := addresses.hash1[ih];
          write(f, 'h[', HexStr(ih, 1 + BsrDWord(AddressesHashSize - 1) div 4), ']');
          if nChain > 0 then write(f, ' (', nChain, ')');
          write(f, ' = ');
          DumpAddressValue(f, id1 - 1);
          repeat
            id1 := addresses.nextCollision1[id1 - 1];
            if id1 = 0 then break;
            write(f, ', ');
            DumpAddressValue(f, id1 - 1);
          until false;
          writeln(f);
        end;
      end;
      writeln(f, 'Avg. chain length ', (addresses.nUpto - addresses.nFree) / nChains:0:2, ', max ', maxChain);
      write(f, LineEnding, 'Addresses (', addresses.nUpto - addresses.nFree);
      if addresses.nFree > 0 then write(f, ' + free ', addresses.nFree);
      writeln(f, '), LRU to MRU order:');
    end;
    id1 := addresses.firstLru1;
    while id1 <> 0 do
    begin
      DumpAddressValue(f, id1 - 1);
      write(f, ', ');
      DumpStringValue(f, addresses.fileFunc[id1 - 1, 1]);
      write(f, ' @ ');
      DumpStringValue(f, addresses.fileFunc[id1 - 1, 0]);
      write(f, ':', uint32(addresses.lineLo16[id1 - 1]) or uint32(addresses.lineHi8[id1 - 1]) shl 16, ', id: ', id1 - 1);
      id1 := addresses.a[id1 - 1].nextLru1;
      writeln(f);
    end;
  end;
{$endif debug_lineinfocache}

  function LineInfoCache.TryGet(addr: CodePointer; out func, source: shortstring; out line: longint): boolean;
  var
    hash: SizeUint;
    id: SizeInt;
  begin
    hash := HashCodePointer(addr);
    id := FindAddress(addr, hash);
    result := id <> -1;
    if not result then exit;
    RemoveAddressFromLRU(id);
    AddAddressToLRU(id);
    BumpStringLRU(addresses.fileFunc[id, 0]);
    StringToShortstring(addresses.fileFunc[id, 0], source);
    BumpStringLRU(addresses.fileFunc[id, 1]);
    StringToShortstring(addresses.fileFunc[id, 1], func);
    line := uint32(addresses.lineLo16[id]) or uint32(addresses.lineHi8[id]) shl 16;
  end;

  procedure LineInfoCache.Put(addr: CodePointer; const func, source: shortstring; line: longint);
  const
    EvictionAlwaysMakesSpace = (MaxStrings >= 2) and (MaxStringsData >= 2 * (255 + StringHeaderSize));
  var
    addressHash, funcHash, sourceHash, reqStrings, reqData: SizeUint;
    funcId, sourceId, addressId, evictPtr, prev, stringsToEvict, dataToEvict, alt {$if not EvictionAlwaysMakesSpace}, origDataToEvict {$endif}: SizeInt;
  begin
    if longword(line) > 1 shl 24 - 1 then exit; // Won’t fit into lineLo16 + lineHi8...

    reqStrings := 0;
    reqData := 0;
    funcHash := HashMemory(@func[1], length(func));
    funcId := FindString(func, funcHash);
    if funcId = -1 then
    begin
      inc(reqStrings);
      inc(reqData, SizeUint(length(func) + StringHeaderSize));
    end;
    sourceHash := HashMemory(@source[1], length(source));
    sourceId := FindString(source, sourceHash);
    if sourceId = -1 then
    begin
      inc(reqStrings);
      inc(reqData, SizeUint(length(source) + StringHeaderSize));
    end;

    if (reqStrings > 0) or (reqData > 0) then
    begin
      // Has space, or need to evict something?
      stringsToEvict := SizeInt(SizeUint(strings.nUpto)) - SizeInt(SizeUint(strings.nFree)) + SizeInt(reqStrings) - MaxStrings;
      dataToEvict := SizeInt(SizeUint(strings.dataSize)) + SizeInt(reqData) - MaxStringsData;
      if (stringsToEvict > 0) or (dataToEvict > 0) then
      begin
        alt := strings.dataSize div 8 + strings.dataSize div 32; // Evict at least 15.6% of strings at once, because PackStrings is slow.
        if alt > dataToEvict then dataToEvict := alt;
      {$if not EvictionAlwaysMakesSpace}
        origDataToEvict := dataToEvict;
      {$endif not EvictionAlwaysMakesSpace}
        evictPtr := SizeInt(SizeUint(strings.lastLru1)) - 1;
        while {$if not EvictionAlwaysMakesSpace} (evictPtr <> -1) and {$endif} ((stringsToEvict > 0) or (dataToEvict > 0)) do
        begin
          prev := SizeInt(SizeUint(strings.s[evictPtr].prevLru1)) - 1;
          if (evictPtr <> funcId) and (evictPtr <> sourceId) then
          begin
            dec(stringsToEvict);
            dec(dataToEvict, SizeInt(pStringDataHeader(pByte(strings.data) + strings.dataOfs[evictPtr])^.len + StringHeaderSize));
            RemoveString(evictPtr);
          end;
          evictPtr := prev;
        end;
      {$if not EvictionAlwaysMakesSpace}
        if dataToEvict <> origDataToEvict then
      {$endif not EvictionAlwaysMakesSpace}
          PackStrings; // This is crucial.
      {$if not EvictionAlwaysMakesSpace}
        // Eviction hasn’t evicted enough. Impossible situation for large enough caches.
        // For small caches, a potential “improvement” is doing eviction in 2 passes, the first is just a simulation, so if the Put() request won’t fit anyway, don’t evict anything.
        if (stringsToEvict > 0) or (dataToEvict > 0) then exit;
      {$endif not EvictionAlwaysMakesSpace}
      end;
    end;

    addressHash := HashCodePointer(addr);
    addressId := FindAddress(addr, addressHash);
    if addressId <> -1 then
      RemoveAddress(addressId) // Just remove and re-add if already exists, this is cheap unlike adding and removing strings.
    else if addresses.nUpto - addresses.nFree = MaxAddresses then
      RemoveAddress(addresses.lastLru1 - 1);

    if funcId = -1 then
      funcId := AddString(func, funcHash)
    else
      BumpStringLRU(funcId);
    if sourceId = -1 then
      sourceId := AddString(source, sourceHash)
    else
      BumpStringLRU(sourceId);
    addressId := AddAddress(addr, addressHash, sourceId, funcId, line);
  end;
{$endif has_LineInfoCache}

{$ifdef fpc_has_feature_threading}
  procedure InitializeAfterUnits;
  var
    i: SizeInt;
  begin
    if Assigned(prevInitProc) then TProcedure(prevInitProc)();
    for i := 0 to High(locks) do InitCriticalSection(locks[i]);
    locksAlive := true;
  end;

  procedure FinalizeBeforeUnits;
  var
    i: SizeInt;
  begin
    ExitProc := prevExitProc;
    if not locksAlive then exit;
    locksAlive := false;
    for i := 0 to High(locks) do DoneCriticalSection(locks[i]);
  end;
{$endif fpc_has_feature_threading}

initialization
{$ifdef fpc_has_feature_threading}
  prevInitProc := InitProc;
  InitProc := @InitializeAfterUnits;
  prevExitProc := ExitProc;
  ExitProc := @FinalizeBeforeUnits;
{$endif fpc_has_feature_threading}
  lastfilename := '';
  lastopendwarf := false;
  BackTraceStrFunc := @DwarfBacktraceStr;

finalization
  CloseDwarf(false);

end.
