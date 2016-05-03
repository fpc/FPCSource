unit uhpackimp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uhpacktables;

const
  HPACK_MAX_HEADER_SIZE = 8192;
  HPACK_MAX_HEADER_TABLE_SIZE = 4096;

(*
 * Tries to "inline" some specific and short time critical functions.
 *)
{$DEFINE USEINLINE}

(*
 * This setting applies a bit manual optimizations here and there, but most
 * of them could stop correctly working if static information is changed in a
 * future, like THPackStaticTable fields (count and order). This optimizations
 * only gain around 0.50% in speed time.
 *)
{$DEFINE MANUALOPTIMIZATIONS}

type

  THPackHeaderAddEvent = procedure (aName,aValue: RawByteString; aSensitive: Boolean) of object;
  THPACKException=class (Exception);

  THPackHeaderTextItem=record
    HeaderName: RawByteString;
    HeaderValue: RawByteString;
    IsSensitive: Boolean;
  end;
  PHPackHeaderTextItem=^THPackHeaderTextItem;

  { THPackHeaderTextList }

  THPackHeaderTextList=class(TObject)
  private
    function Get(Index: integer): PHPackHeaderTextItem; overload;
    function GetAsText: string;
    function GetCount: integer;
  protected
    FList: TFPList;
  public
    constructor Create;
    destructor  Destroy; override;
    function Add(const aName, aValue: RawByteString; const aSensitive: Boolean=false
      ): integer;
    procedure Clear;
    function  GetHeaderValue(const aName: String; out aValue: String): Boolean;
    property  Count: integer read GetCount;
    property  Item[Index: integer]: PHPackHeaderTextItem read Get; default;
    property  Text: string read GetAsText;
  end;

  { THPackHeaderField }

  THPackHeaderField=class
  private
  protected
    FName: RawByteString;
    FValue: RawByteString;
  public
    const
      HEADER_ENTRY_OVERHEAD = 32;
  public
    // Section 4.1. Calculating Table Size
    // The additional 32 octets account for an estimated
    // overhead associated with the structure.
    class function SizeOf(const aName, aValue: RawByteString): Integer;

    constructor Create(const aName,aValue: RawByteString);
    function Size: Integer;

  end;

  { THPackHuffmanEncoder }

  THPackHuffmanEncoder=class
  private
  protected
    FCodes: PDWORD;
    FLengths: PByte;
  public
    constructor Create;
    constructor Create(const aCodes: PDWORD; const aLengths: PByte);
    function  GetEncodedLength(aData: RawByteString): integer;
    procedure Encode(aOutputStream: TStream; aData: RawByteString);
    procedure Encode(aOutputStream: TStream; aData: RawByteString; aOff,aLen: integer);
  end;

  { THPackHuffmanNode }

  THPackHuffmanNode=class
  private
  protected
    FSymbol: integer;      // terminal nodes have a symbol
    FBits: integer;        // number of bits matched by the node
    FChildren: array of THPackHuffmanNode; // internal nodes have children
    class procedure Insert(aRoot: THPackHuffmanNode; aSymbol: integer; aCode: integer; aLength: BYTE);
    class function BuildTree(const aCodes: PDWORD; const aLengths: PByte): THPackHuffmanNode;
  public
    constructor Create;
    constructor Create(aSymbol: Integer; aBits: Integer);
    destructor Destroy; override;
    function  isTerminal: Boolean;{$IFDEF USEINLINE}inline;{$ENDIF}
  end;

  { THPackHuffmanDecoder }

  THPackHuffmanDecoder=class
  private
  protected
    FCodes: PDWORD;
    FLengths: PByte;
    FRoot: THPackHuffmanNode;
  public
    constructor Create;
    constructor Create(const aCodes: PDWORD; const aLengths: PByte);
    destructor Destroy; override;
    function Decode(aBuf: RawByteString): RawByteString;
  end;

  THPackHuffman=class
  private
  protected
    class var
    FRefCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    class var Encoder: THPackHuffmanEncoder;
    class var Decoder: THPackHuffmanDecoder;
  end;

  { THPackDynamicTable }

  THPackDynamicTable=class
  private
    FSize: integer;
    FCapacity: integer;
  protected
    // a circular queue of header fields
    FHeaderFields: array of THPackHeaderField;
    FHead: integer;
    FTail: integer;
    procedure SetCapacity(aCapacity: integer);
    procedure Clear;
    function  Remove: THPackHeaderField;
    function  EntriesCount: integer; {$IFDEF USEINLINE}inline;{$ENDIF}
  public
    constructor Create(aInitialCapacity: integer);
    destructor Destroy; override;
    procedure Add(aHeaderField: THPackHeaderField);
    function  GetEntry(aIndex: integer): THPackHeaderField; {$IFDEF USEINLINE}inline;{$ENDIF}
    function  GetNameIndex(const aName: RawByteString): integer;
    function  GetEntry(const aName, aValue: RawByteString): THPackHeaderField;
    function  GetEntryIndex(const aName, aValue: RawByteString): integer;
    function  GetEntryIndex(const aHeader: THPackHeaderField): integer;
    procedure EnsureCapacityToHold(const aHeaderSize: integer);
    property  Size: integer read FSize;
    property  Capacity: integer read FCapacity;
  end;

  { THPackStaticTable }

  THPackStaticTable=class
  private
    HPackStaticTable: array [1..61] of THPackHeaderField; static;
  protected
  public
    class procedure InitializeStaticTable;
    class procedure DestroyStaticTable;
    class function  GetEntry(aIndex: integer): THPackHeaderField;
    (**
     * Returns the lowest index value for the given header field name in the static table.
     * Returns -1 if the header field name is not in the static table.
     *)
    class function  GetIndex(const aName: RawByteString): Integer;
    (**
     * Returns the index value for the given header field in the static table.
     * Returns -1 if the header field is not in the static table.
     *)
    class function GetIndex(const aName,aValue: RawByteString): integer;
    class function TableLength: integer; {$IFDEF USEINLINE}inline;{$ENDIF}
  end;

  { THPackDecoder }

  THPackDecoder=class
  private
    DynamicTable: THPackDynamicTable;
    MaxHeaderSize: integer;
    MaxDynamicTableSize: integer;
    EncoderMaxDynamicTableSize: integer;
    MaxDynamicTableSizeChangeRequired: Boolean;
    HeaderSize: integer;
    State: THPackState;
    IndexType: THPackIndexType;
    FIndex: integer;
    HuffmanEncoded: Boolean;
    SkipLength: integer;
    NameLength: integer;
    ValueLength: integer;
    FName: RawByteString;
    FMustReset: Boolean;
  protected
    FHeaderListenerAddHeader: THPackHeaderAddEvent;
    FDecodedHeaders: THPackHeaderTextList;
    Huffman: THPackHuffman;

    procedure Reset;
    function  GetHeaderField(aIndex: integer): THPackHeaderField;
    procedure SetDynamicTableSize(aDynamicTableSize: integer);
    procedure ReadName (aIndex: integer);
    function  ReadStringLiteral(aStream: TStream; aLength: integer): RawByteString;
    procedure AddHeader(aName,aValue: RawByteString; aSensitive: Boolean);
    procedure InsertHeader(aName,aValue: RawByteString; aIndexType: THPackIndexType);
    function  DecodeULE128(aStream: TStream): integer;
    procedure IndexHeader(aIndex: integer);
    function  ExceedsMaxHeaderSize(aSize: integer): Boolean;

    procedure DoAddHeader(aName,aValue: RawByteString; aSensitive: Boolean); virtual;

  public
    constructor Create;
    constructor Create(aMaxHeaderSize, aMaxHeaderTableSize: integer);
    destructor Destroy; override;
    procedure Decode(aStream: TStream);
    procedure Decode(aString: RawByteString);
    function  GetMaxHeaderTableSize: Integer;
    procedure SetMaxHeaderTableSize(aMaxHeaderTableSize: integer);
    function  EndHeaderBlockTruncated: Boolean;
    property  OnAddHeader: THPackHeaderAddEvent read FHeaderListenerAddHeader write FHeaderListenerAddHeader;
    property  DecodedHeaders: THPackHeaderTextList read FDecodedHeaders;
  end;

  { THPackEncoder }

  THPackEncoder=class
  private
  private
    // Used for debugging purposes, modifies class behaviour using or not
    // indexing, or Huffman compression.
    UseIndexing: Boolean;
    ForceHuffmanOn: Boolean;
    ForceHuffmanOff: Boolean;

    procedure EncodeLiteral(aOutStream: TStream; const aName: RawByteString; const aValue: RawByteString; const aIndexType: THPackIndexType; const aNameIndex: Integer);
    procedure EncodeInteger(aOutStream: TStream; const aMask: integer; const n: integer; const i: integer);
    procedure EncodeStringLiteral(aOutStream: TStream; const aString: RawByteString);
    function  GetNameIndex(const aName: RawByteString): integer;
    procedure Add(const aName, aValue: RawByteString);
    procedure Clear;

  protected
    DynamicTable: THPackDynamicTable;
    Huffman: THPackHuffman;
  public
    constructor Create;
    constructor Create(const aMaxHeaderTableSize: Integer);
    constructor Create(const aMaxHeaderTableSize: Integer;
                       const aUseIndexing: Boolean;
                       const aForceHuffmanOn: Boolean;
                       const aForceHuffmanOff: Boolean);
    destructor Destroy; override;
    procedure EncodeHeader(aOutStream: TStream; const aName: RawByteString; const aValue: RawByteString; const aSensitive: Boolean);
    procedure SetMaxHeaderTableSize(aOutStream: TStream; const aNewMaxHeaderTableSize: Integer);

  end;

implementation

const
    NOT_FOUND=-1;

{ THPackHeaderTextList }

function THPackHeaderTextList.Get(Index: integer): PHPackHeaderTextItem;
begin
  Result:=PHPackHeaderTextItem(FList[Index]);
end;

function THPackHeaderTextList.GetAsText: string;
const
  HEADER_SEPARATOR_MARK: char=':';
var
  j: integer;
  P: PHPackHeaderTextItem;
  O: string;
  w: integer;
  wl, lle: integer;
  Allocated: integer;
  LE : string;
  procedure EnsureSpace(const aNeeded: integer);{$IFDEF USEINLINE}inline;{$ENDIF}
  begin
    if (w+aNeeded)>Allocated then begin
      Allocated:=Allocated+aNeeded;
      SetLength(O,Allocated);
    end;
  end;
  
begin
  LE:=LineEnding;
  LLE:=Length(LE);
  Allocated:=HPACK_MAX_HEADER_SIZE*2;
  SetLength(O,Allocated);
  w:=1;
  for j := 0 to Pred(FList.Count) do begin
    P:=PHPackHeaderTextItem(FList[j]);
    wl:=Length(P^.HeaderName);
    if wl=0 then begin
      Raise THPACKException.Create('Header name is empty');
    end;
    EnsureSpace(wl);
    move(P^.HeaderName[1],O[w],wl);
    inc(w,wl);
    wl:=1;
    EnsureSpace(wl);
    move(HEADER_SEPARATOR_MARK,O[w],wl);
    inc(w,wl);
    wl:=Length(P^.HeaderValue);
    if wl>0 then begin
      EnsureSpace(wl);
      move(P^.HeaderValue[1],O[w],wl);
      inc(w,wl);
    end;
    EnsureSpace(lle);
    move(LE[1],O[w],lle);
    inc(w,lle);
  end;
  SetLength(O,w-1);
  Result:=O;
end;

function THPackHeaderTextList.GetCount: integer;
begin
  Result:=FList.Count;
end;

constructor THPackHeaderTextList.Create;
begin
  FList:=TFPList.Create;
end;

destructor THPackHeaderTextList.Destroy;
begin
  Clear;
  FList.Free;
end;

function THPackHeaderTextList.Add(const aName, aValue: RawByteString;
  const aSensitive: Boolean=false): integer;
var
  P: PHPackHeaderTextItem;
begin
  New(P);
  P^.HeaderName:=aName;
  P^.HeaderValue:=aValue;
  P^.IsSensitive:=aSensitive;
  Result:=FList.Add(P);
end;

procedure THPackHeaderTextList.Clear;
var
  j: integer;
begin
  for j := 0 to Pred(FList.Count) do begin
    Dispose(PHPackHeaderTextItem(FList[j]));
  end;
  FList.Clear;
end;

function THPackHeaderTextList.GetHeaderValue(const aName: String; out
  aValue: String): Boolean;
var
  j: integer;
  p: PHPackHeaderTextItem;
begin
  for j := 0 to Pred(FList.Count) do begin
    P:=PHPackHeaderTextItem(FList[j]);
    if p^.HeaderName=aName then begin
      aValue:=p^.HeaderValue;
      Result:=true;
      exit;
    end;
  end;
  aValue:='';
  Result:=false;
end;

{ THPackEncoder }

constructor THPackEncoder.Create(const aMaxHeaderTableSize: Integer);
begin
  Create(aMaxHeaderTableSize,true,false,false);
end;

constructor THPackEncoder.Create(const aMaxHeaderTableSize: Integer;
  const aUseIndexing: Boolean; const aForceHuffmanOn: Boolean;
  const aForceHuffmanOff: Boolean);
begin
  if aMaxHeaderTableSize < 0 then begin
    Raise THPACKException.CreateFmt('Illegal capacity: %d',[aMaxHeaderTableSize]);
  end;
  DynamicTable:=THPackDynamicTable.Create(aMaxHeaderTableSize);
  UseIndexing := aUseIndexing;
  ForceHuffmanOn := aForceHuffmanOn;
  ForceHuffmanOff := aForceHuffmanOff;
  Huffman:=THPackHuffman.Create;
end;

destructor THPackEncoder.Destroy;
begin
  Clear;
  FreeAndNil(Huffman);
  FreeAndNil(DynamicTable);
  inherited Destroy;
end;

procedure THPackEncoder.EncodeHeader(aOutStream: TStream;
  const aName: RawByteString; const aValue: RawByteString;
  const aSensitive: Boolean);
var
  NameIndex: integer;
  StaticTableIndex: integer;
  HeaderSize: integer;
  ThisHeaderField: THPackHeaderField;
  Index: integer;
  IndexType: THPackIndexType;
begin
  // If the header value is sensitive then it must never be indexed
  if aSensitive then begin
    NameIndex := GetNameIndex(aName);
    EncodeLiteral(aOutStream, aName, aValue, THPackIndexType.eHPackNEVER, NameIndex);
    exit;
  end;

  // If the peer will only use the static table
  if DynamicTable.Capacity = 0 then begin
    StaticTableIndex := THPackStaticTable.GetIndex(aName, aValue);
    if StaticTableIndex = NOT_FOUND then begin
      NameIndex := THPackStaticTable.GetIndex(aName);
      EncodeLiteral(aOutStream, aName, aValue, THPackIndexType.eHPackNONE, NameIndex);
    end else begin
      EncodeInteger(aOutStream, $80, 7, StaticTableIndex);
    end;
    exit;
  end;

  HeaderSize := THPackHeaderField.sizeOf(aName, aValue);

  // If the headerSize is greater than the max table size then it must be encoded literally
  if HeaderSize > DynamicTable.Capacity then begin
    NameIndex := GetNameIndex(aName);
    EncodeLiteral(aOutStream, aName, aValue, THPackIndexType.eHPackNONE, NameIndex);
    Exit;
  end;

  ThisHeaderField := DynamicTable.GetEntry(aName, aValue);
  if Assigned(ThisHeaderField) then begin
    Index := DynamicTable.GetEntryIndex(ThisHeaderField) + THPackStaticTable.TableLength;
    // Section 6.1. Indexed Header Field Representation
    EncodeInteger(aOutStream, $80, 7, Index);
  end else begin
    StaticTableIndex := THPackStaticTable.GetIndex(aName, aValue);
    if StaticTableIndex <> NOT_FOUND then begin
      // Section 6.1. Indexed Header Field Representation
      EncodeInteger(aOutStream, $80, 7, StaticTableIndex);
    end else begin
      NameIndex := GetNameIndex(aName);
      if UseIndexing then begin
        DynamicTable.EnsureCapacityToHold(HeaderSize);
      end;
      if UseIndexing then begin
        IndexType:=THPackIndexType.eHPackINCREMENTAL;
      end else begin
        IndexType:=THPackIndexType.eHPackNONE;
      end;
      EncodeLiteral(aOutStream, aName, aValue, IndexType, NameIndex);
      if UseIndexing then begin
        Add(aName, aValue);
      end;
    end;
  end;
end;

procedure THPackEncoder.SetMaxHeaderTableSize(aOutStream: TStream;
  const aNewMaxHeaderTableSize: Integer);
begin
  if aNewMaxHeaderTableSize < 0 then begin
    Raise THPACKException.CreateFmt('Illegal Capacity %d',[aNewMaxHeaderTableSize]);
  end;
  if DynamicTable.Capacity = aNewMaxHeaderTableSize then begin
    //No change needed
    exit;
  end;
  DynamicTable.SetCapacity(aNewMaxHeaderTableSize);
  //DynamicTable.EnsureCapacityToHold(0);
  EncodeInteger(aOutStream, $20, 5, aNewMaxHeaderTableSize);
end;

procedure THPackEncoder.EncodeLiteral(aOutStream: TStream;
  const aName: RawByteString; const aValue: RawByteString;
  const aIndexType: THPackIndexType; const aNameIndex: Integer);
(**
 * Encode literal header field according to Section 6.2.
 *)
var
  Mask: Integer;
  PrefixBits: Integer;
  v: integer;
begin
  case aIndexType of
    eHPackINCREMENTAL:
      begin
        Mask := $40;
        PrefixBits := 6;
      end;
    eHPackNONE:
      begin
        Mask := $00;
        PrefixBits := 4;
      end;
    eHPackNEVER:
      begin
        Mask := $10;
        PrefixBits := 4;
      end;
    else
      Raise THPACKException.Create('Should not reach here');
  end;
  if aNameIndex=NOT_FOUND then begin
    v:=0;
  end else begin
    v:=aNameIndex;
  end;
  EncodeInteger(aOutStream, Mask, PrefixBits, v);
  if aNameIndex = NOT_FOUND then begin
    EncodeStringLiteral(aOutStream, aName);
  end;
  EncodeStringLiteral(aOutStream, aValue);
end;

procedure THPackEncoder.EncodeInteger(aOutStream: TStream; const aMask: integer;
  const n: integer; const i: integer);
var
  nBits: Integer;
  Len: DWORD;
begin
  if (n < 0) or (n > 8) then begin
    Raise THPACKException.CreateFmt('Encode Integer Illegal Argument Exception ("N<0|N>8": %d)',[n]);
  end;
  nBits := $FF shr (8 - n);
  if i < nBits then begin
    aOutStream.WriteByte(BYTE(aMask or i));
  end else begin
    aOutStream.WriteByte(BYTE(aMask or nBits));
    Len := i - nBits;
    while (true) do begin
      if (Len and  (not $7F)) = 0 then begin
        aOutStream.WriteByte(BYTE(Len));
        exit;
      end else begin
        aOutStream.WriteByte(Byte((Len and $7F) or $80));
        Len:=Len shr 7;
      end;
    end;
  end;
end;

procedure THPackEncoder.EncodeStringLiteral(aOutStream: TStream;
  const aString: RawByteString);
var
  HuffmanLength: integer;
begin
  if Length(aString)=0 then begin
    EncodeInteger(aOutStream, $00, 7, 0);
    exit;
  end;
  HuffmanLength := THpackHuffman.Encoder.GetEncodedLength(aString);
  if ((HuffmanLength < Length(aString)) and not forceHuffmanOff) or forceHuffmanOn then begin
    EncodeInteger(aOutStream, $80, 7, HuffmanLength);
    if Length(aString)>0 then begin
      THPackHuffman.Encoder.Encode(aOutStream, aString);
    end;
  end else begin
    EncodeInteger(aOutStream, $00, 7, Length(aString));
    if Length(aString)>0 then begin
      aOutStream.Write(aString[1],Length(aString));
    end;
  end;
end;

function THPackEncoder.GetNameIndex(const aName: RawByteString): integer;
var
  Index: integer;
begin
  Index := THPackStaticTable.GetIndex(aName);
  if Index = NOT_FOUND then begin
    Index := DynamicTable.GetNameIndex(aName);
    if Index >= 0 then begin
      inc(Index,THPackStaticTable.TableLength);
    end;
  end;
  Result:=Index;
end;

procedure THPackEncoder.Add(const aName, aValue: RawByteString);
(**
 * Add the header field to the dynamic table.
 * Entries are evicted from the dynamic table until the size of the table
 * and the new header field is less than the table's capacity.
 * If the size of the new entry is larger than the table's capacity,
 * the dynamic table will be cleared.
 *)
var
//  HeaderSize: Integer;
  Header: THPackHeaderField;
begin
  (*
  HeaderSize := THPackHeaderField.SizeOf(aName, aValue);

  // Clear the table if the header field size is larger than the capacity.
  if HeaderSize > Capacity then begin
    Clear;
    // Do not add this entry to the DynamicTable
    exit;
  end;

  // Evict oldest entries until we have enough capacity.
  while (Size + HeaderSize) > Capacity do begin
    Remove().Free;
  end;
  *)
  // Copy name and value that modifications of original do not affect the dynamic table.

  Header:=THPackHeaderField.Create(aName,aValue);
  DynamicTable.Add(Header);

  //Inc(Size,HeaderSize);
end;

procedure THPackEncoder.Clear;
(**
 * Remove all entries from the dynamic table.
 *)
begin
  DynamicTable.Clear;
end;

constructor THPackEncoder.Create;
begin
  Create(HPACK_MAX_HEADER_TABLE_SIZE,true,false,false);
end;

{ THPackHuffman }

constructor THPackHuffman.Create;
begin
  if FRefCount=0 then begin
    Encoder:=THPackHuffmanEncoder.Create;
    Decoder:=THPackHuffmanDecoder.Create;
  end;
  inc(FRefCount);
end;

destructor THPackHuffman.Destroy;
begin
  dec(FRefCount);
  if FRefCount=0 then begin
    FreeAndNil(Encoder);
    FreeAndNil(Decoder);
  end;
  inherited Destroy;
end;

{ THPackStaticTable }

class function THPackStaticTable.TableLength: integer;
begin
  Result:=Length(HPackStaticTable);
end;

class procedure THPackStaticTable.InitializeStaticTable;
const
  EMPTY='';
begin
  // Appendix A: Static Table
  // http://tools.ietf.org/html/rfc7541#appendix-A
  HPackStaticTable[01]:=THPackHeaderField.Create(':authority',EMPTY);
  HPackStaticTable[02]:=THPackHeaderField.Create(':method', 'GET');
  HPackStaticTable[03]:=THPackHeaderField.Create(':method', 'POST');
  HPackStaticTable[04]:=THPackHeaderField.Create(':path', '/');
  HPackStaticTable[05]:=THPackHeaderField.Create(':path', '/index.html');
  HPackStaticTable[06]:=THPackHeaderField.Create(':scheme', 'http');
  HPackStaticTable[07]:=THPackHeaderField.Create(':scheme', 'https');
  HPackStaticTable[08]:=THPackHeaderField.Create(':status', '200');
  HPackStaticTable[09]:=THPackHeaderField.Create(':status', '204');
  HPackStaticTable[10]:=THPackHeaderField.Create(':status', '206');
  HPackStaticTable[11]:=THPackHeaderField.Create(':status', '304');
  HPackStaticTable[12]:=THPackHeaderField.Create(':status', '400');
  HPackStaticTable[13]:=THPackHeaderField.Create(':status', '404');
  HPackStaticTable[14]:=THPackHeaderField.Create(':status', '500');
  HPackStaticTable[15]:=THPackHeaderField.Create('accept-charset', EMPTY);
  HPackStaticTable[16]:=THPackHeaderField.Create('accept-encoding', 'gzip, deflate');
  HPackStaticTable[17]:=THPackHeaderField.Create('accept-language', EMPTY);
  HPackStaticTable[18]:=THPackHeaderField.Create('accept-ranges', EMPTY);
  HPackStaticTable[19]:=THPackHeaderField.Create('accept', EMPTY);
  HPackStaticTable[20]:=THPackHeaderField.Create('access-control-allow-origin', EMPTY);
  HPackStaticTable[21]:=THPackHeaderField.Create('age', EMPTY);
  HPackStaticTable[22]:=THPackHeaderField.Create('allow', EMPTY);
  HPackStaticTable[23]:=THPackHeaderField.Create('authorization', EMPTY);
  HPackStaticTable[24]:=THPackHeaderField.Create('cache-control', EMPTY);
  HPackStaticTable[25]:=THPackHeaderField.Create('content-disposition', EMPTY);
  HPackStaticTable[26]:=THPackHeaderField.Create('content-encoding', EMPTY);
  HPackStaticTable[27]:=THPackHeaderField.Create('content-language', EMPTY);
  HPackStaticTable[28]:=THPackHeaderField.Create('content-length', EMPTY);
  HPackStaticTable[29]:=THPackHeaderField.Create('content-location', EMPTY);
  HPackStaticTable[30]:=THPackHeaderField.Create('content-range', EMPTY);
  HPackStaticTable[31]:=THPackHeaderField.Create('content-type', EMPTY);
  HPackStaticTable[32]:=THPackHeaderField.Create('cookie', EMPTY);
  HPackStaticTable[33]:=THPackHeaderField.Create('date', EMPTY);
  HPackStaticTable[34]:=THPackHeaderField.Create('etag', EMPTY);
  HPackStaticTable[35]:=THPackHeaderField.Create('expect', EMPTY);
  HPackStaticTable[36]:=THPackHeaderField.Create('expires', EMPTY);
  HPackStaticTable[37]:=THPackHeaderField.Create('from', EMPTY);
  HPackStaticTable[38]:=THPackHeaderField.Create('host', EMPTY);
  HPackStaticTable[39]:=THPackHeaderField.Create('if-match', EMPTY);
  HPackStaticTable[40]:=THPackHeaderField.Create('if-modified-since', EMPTY);
  HPackStaticTable[41]:=THPackHeaderField.Create('if-none-match', EMPTY);
  HPackStaticTable[42]:=THPackHeaderField.Create('if-range', EMPTY);
  HPackStaticTable[43]:=THPackHeaderField.Create('if-unmodified-since', EMPTY);
  HPackStaticTable[44]:=THPackHeaderField.Create('last-modified', EMPTY);
  HPackStaticTable[45]:=THPackHeaderField.Create('link', EMPTY);
  HPackStaticTable[46]:=THPackHeaderField.Create('location', EMPTY);
  HPackStaticTable[47]:=THPackHeaderField.Create('max-forwards', EMPTY);
  HPackStaticTable[48]:=THPackHeaderField.Create('proxy-authenticate', EMPTY);
  HPackStaticTable[49]:=THPackHeaderField.Create('proxy-authorization', EMPTY);
  HPackStaticTable[50]:=THPackHeaderField.Create('range', EMPTY);
  HPackStaticTable[51]:=THPackHeaderField.Create('referer', EMPTY);
  HPackStaticTable[52]:=THPackHeaderField.Create('refresh', EMPTY);
  HPackStaticTable[53]:=THPackHeaderField.Create('retry-after', EMPTY);
  HPackStaticTable[54]:=THPackHeaderField.Create('server', EMPTY);
  HPackStaticTable[55]:=THPackHeaderField.Create('set-cookie', EMPTY);
  HPackStaticTable[56]:=THPackHeaderField.Create('strict-transport-security', EMPTY);
  HPackStaticTable[57]:=THPackHeaderField.Create('transfer-encoding', EMPTY);
  HPackStaticTable[58]:=THPackHeaderField.Create('user-agent', EMPTY);
  HPackStaticTable[59]:=THPackHeaderField.Create('vary', EMPTY);
  HPackStaticTable[60]:=THPackHeaderField.Create('via', EMPTY);
  HPackStaticTable[61]:=THPackHeaderField.Create('www-authenticate', EMPTY);
end;

class procedure THPackStaticTable.DestroyStaticTable;
var
  j: integer;
begin
  for j := Low(HPackStaticTable) to High(HPackStaticTable) do begin
    HPackStaticTable[j].Free;
    HPackStaticTable[j]:=nil;
  end;
end;

class function THPackStaticTable.GetEntry(aIndex: integer): THPackHeaderField;
begin
  Result:=HPackStaticTable[aIndex];
end;

class function THPackStaticTable.GetIndex(const aName: RawByteString
  ): Integer;
var
  lLeft,lRight: integer;
  Half: integer;
  c: integer;
begin
  lLeft:=Low(HPackStaticTable);
  lRight:=High(HPackStaticTable);
  {$IFDEF MANUALOPTIMIZATIONS}
  // Manual optimization
  if aName[1]>'c' then begin
    lLeft:=33;
  end;
  {$ENDIF}
  while lLeft<=lRight do begin
    Half:=(lLeft+lRight) div 2; // No overflow problem, low amount of elements
    c:=CompareStr(aName,HPackStaticTable[Half].FName);
    if c=0 then begin
      dec(Half);
      while Half>=lLeft do begin
        if HPackStaticTable[Half].FName<>aName then begin
          break;
        end;
        dec(Half);
      end;
      Result:=Half+1;
      exit;
    end else if c<0 then begin
      lRight:=Half-1;
    end else begin
      // c > 0
      lLeft:=Half+1;
    end;
  end;
  Result:=NOT_FOUND;
end;

class function THPackStaticTable.GetIndex(const aName, aValue: RawByteString): Integer;
var
  lLeft,lRight: integer;
  Half: integer;
  c: integer;
begin
  lLeft:=Low(HPackStaticTable);
  lRight:=High(HPackStaticTable);
  {$IFDEF MANUALOPTIMIZATIONS}
  // Manual optimization
  if aName[1]>'c' then begin
    lLeft:=33;
  end;
  {$ENDIF}
  while lLeft<=lRight do begin
    Half:=(lLeft+lRight) div 2; // No overflow problem, low amount of elements
    c:=CompareStr(aName,HPackStaticTable[Half].FName);
    if c=0 then begin
      c:=CompareStr(aValue,HPackStaticTable[Half].FValue);
      if c=0 then begin
        Result:=Half;
        exit;
      end else if c<0 then begin
        lRight:=Half-1;
      end else begin
        // c > 0
        lLeft:=Half+1;
      end;
    end else if c<0 then begin
      lRight:=Half-1;
    end else begin
      // c > 0
      lLeft:=Half+1;
    end;
  end;
  Result:=NOT_FOUND;
end;

{ THPackDecoder }

procedure THPackDecoder.Reset;
begin
  HeaderSize := 0;
  State := THPackState.READ_HEADER_REPRESENTATION;
  IndexType := THPackIndexType.eHPackNONE;
  FDecodedHeaders.Clear;
  FMustReset:=false;
end;

function THPackDecoder.EndHeaderBlockTruncated: Boolean;
begin
  Result:= HeaderSize > MaxHeaderSize;
  FMustReset:=true;
end;

procedure THPackDecoder.SetMaxHeaderTableSize(aMaxHeaderTableSize: integer);
begin
  if FMustReset then Reset;
  MaxDynamicTableSize := aMaxHeaderTableSize;
  if (MaxDynamicTableSize < EncoderMaxDynamicTableSize) then begin
    // decoder requires less space than encoder
    // encoder MUST signal this change
    MaxDynamicTableSizeChangeRequired := true;
    DynamicTable.SetCapacity(MaxDynamicTableSize);
  end;
end;

function THPackDecoder.GetHeaderField(aIndex: integer): THPackHeaderField;
begin
  Result:=DynamicTable.GetEntry(aIndex + 1);
end;

procedure THPackDecoder.SetDynamicTableSize(aDynamicTableSize: integer);
begin
  if aDynamicTableSize > MaxDynamicTableSize then begin
    Raise THPACKException.Create('Invalid MAX_DYNAMIC_TABLE_SIZE');
  end;
  EncoderMaxDynamicTableSize := aDynamicTableSize;
  MaxDynamicTableSizeChangeRequired := false;
  DynamicTable.SetCapacity(aDynamicTableSize);
end;

procedure THPackDecoder.ReadName(aIndex: integer);
var
  HeaderField: THPackHeaderField;
begin
  if aIndex <= THPackStaticTable.TableLength then begin
    HeaderField:= THPackStaticTable.GetEntry(aIndex);
    FName := HeaderField.FName;
  end else if (aIndex - THPackStaticTable.TableLength <= DynamicTable.EntriesCount) then begin
    HeaderField := DynamicTable.GetEntry(aIndex - THPackStaticTable.TableLength);
    FName := HeaderField.FName;
  end else begin
    Raise THPACKException.Create('Illegal index value');
  end;
end;

function THPackDecoder.ReadStringLiteral(aStream: TStream; aLength: integer
  ): RawByteString;
var
  buf: RawByteString;
begin
  SetLength(buf,aLength);
  if (aStream.Read(buf[1],aLength) <> aLength) then begin
    Raise THPACKException.Create('Decompression exception in ReadStringLiteral');
  end;

  if (HuffmanEncoded) then begin
    Result:=Huffman.Decoder.Decode(buf);
  end else begin
    Result:=buf;
  end;
end;

procedure THPackDecoder.AddHeader(aName, aValue: RawByteString; aSensitive: Boolean);
var
  NewSize: Integer;
begin
  if aName='' then begin
    Raise THPACKException.Create('Header name is empty');
  end;
  NewSize := HeaderSize + Length(aName) + Length(aValue);
  if NewSize <= MaxHeaderSize then begin
    DoAddHeader(aName, aValue, aSensitive);
    HeaderSize := newSize;
  end else begin
    // truncation will be reported during EndHeaderBlockTruncated
    HeaderSize := MaxHeaderSize + 1;
  end;
end;

procedure THPackDecoder.InsertHeader(aName, aValue: RawByteString;
  aIndexType: THPackIndexType);
begin
  AddHeader(aName, aValue, aIndexType = THPackIndexType.eHPackNEVER);

  case (aIndexType) of
    eHPackNONE,
    eHPackNEVER: exit;
    eHPackINCREMENTAL: begin
        DynamicTable.Add(THPackHeaderField.Create(aName, aValue));
      end;
    else
      Raise THPACKException.Create('Should not reach here');
  end;
end;

function THPackDecoder.DecodeULE128(aStream: TStream): integer;
var
  EntryMark: int64;
  Shift: integer;
  b: BYTE;
  function InAvailable(): int64; inline;
  begin
    Result:=aStream.Size-aStream.Position;
  end;
begin
  Shift:=0;
  Result:=0;
  EntryMark:=aStream.Position;
  while (Shift < 32) do begin
    if (InAvailable() = 0) then begin
      // Buffer does not contain entire integer,
      // reset reader index and return -1.
      aStream.Position:=EntryMark;
      exit(-1);
    end;
    b := aStream.ReadByte;
    if ((Shift = 28) and ((b and $F8) <> 0)) then begin
      break;
    end;

    Result:=Result or ((b and $7F) shl Shift);

    if ((b and $80) = 0) then begin
      exit;
    end;
    Inc(shift,7);
  end;

  // Value exceeds Integer.MAX_VALUE
  aStream.Position:=EntryMark;
  Raise THPACKException.Create('Decompression error DecodeULE128');
end;

procedure THPackDecoder.IndexHeader(aIndex: integer);
var
  HeaderField: THPackHeaderField;
begin
  if (aIndex <= THPackStaticTable.TableLength) then begin
    HeaderField := THPackStaticTable.GetEntry(aIndex);
    //addHeader(headerListener, headerField.name, headerField.value, false);
    AddHeader(HeaderField.FName, HeaderField.FValue, False);
  end else if (aIndex - THPackStaticTable.TableLength <= DynamicTable.EntriesCount) then begin
    HeaderField := DynamicTable.GetEntry(aIndex - THPackStaticTable.TableLength);
    //addHeader(headerListener, headerField.name, headerField.value, false);
    AddHeader(HeaderField.FName, HeaderField.FValue, False);
  end else begin
    Raise THPACKException.Create('Illegal index value');
  end;
end;

function THPackDecoder.ExceedsMaxHeaderSize(aSize: integer): Boolean;
begin
  // Check new header size against max header size
  if aSize + HeaderSize <= MaxHeaderSize then begin
    exit(False);
  end;
  // truncation will be reported during EndHeaderBlockTruncated
  HeaderSize := MaxHeaderSize + 1;
  Result:=true;
end;

procedure THPackDecoder.DoAddHeader(aName, aValue: RawByteString; aSensitive: Boolean);
begin
  if Assigned(FHeaderListenerAddHeader) then begin
    FHeaderListenerAddHeader(aName,aValue,aSensitive);
  end;
  FDecodedHeaders.Add(aName,aValue,aSensitive);
end;

constructor THPackDecoder.Create;
begin
  Create(HPACK_MAX_HEADER_SIZE,HPACK_MAX_HEADER_TABLE_SIZE);
end;

constructor THPackDecoder.Create(aMaxHeaderSize, aMaxHeaderTableSize: integer);
begin
  Huffman:=THPackHuffman.Create;
  DynamicTable := THPackDynamicTable.Create(aMaxHeaderTableSize);
  MaxHeaderSize := aMaxHeaderSize;
  MaxDynamicTableSize := aMaxHeaderTableSize;
  EncoderMaxDynamicTableSize := aMaxHeaderTableSize;
  MaxDynamicTableSizeChangeRequired := false;
  FDecodedHeaders:=THPackHeaderTextList.Create;
  Reset();
end;

destructor THPackDecoder.Destroy;
begin
  FreeAndNil(Huffman);
  FreeAndNil(DynamicTable);
  FreeAndNil(FDecodedHeaders);
  inherited Destroy;
end;

procedure THPackDecoder.Decode(aStream: TStream);
  function InAvailable(): int64; inline;
  begin
    Result:=aStream.Size-aStream.Position;
  end;
var
  b: BYTE;
  MaxSize: integer;
  HeaderIndex: integer;
  NameIndex: integer;
  NewHeaderSize: integer;
  tmpbuffer: RawByteString;
  Value: RawByteString;
begin
  if FMustReset then Reset;
  while InAvailable() > 0 do begin
    case State of
    READ_HEADER_REPRESENTATION: begin
        b := aStream.ReadByte;
        if MaxDynamicTableSizeChangeRequired and ((b and $E0) <> $20) then begin
          // Encoder MUST signal maximum dynamic table size change
          Raise THPACKException.Create('Max dynamic table size change not notified');
        end;
        if (b > 127) then begin
          // Indexed Header Field
          FIndex := b and $7F;
          if (Findex = 0) then begin
            Raise THPACKException.Create('Illegal index value in Decode');
          end else if (Findex = $7F) then begin
            State := THPackState.READ_INDEXED_HEADER;
          end else begin
            IndexHeader(Findex);
          end;
        end else if ((b and $40) = $40) then begin
          // Literal Header Field with Incremental Indexing
          IndexType := THPackIndexType.eHPackINCREMENTAL;
          FIndex := b and $3F;
          if (Findex = 0) then begin
            State := THPackState.READ_LITERAL_HEADER_NAME_LENGTH_PREFIX;
          end else if (findex = $3F) then begin
            State := THPackState.READ_INDEXED_HEADER_NAME;
          end else begin
            // Index was stored as the prefix
            ReadName(Findex);
            State := THPackState.READ_LITERAL_HEADER_VALUE_LENGTH_PREFIX;
          end;
        end else if ((b and $20) = $20) then begin
          // Dynamic Table Size Update
          Findex := b and $1F;
          if (Findex = $1F) then begin
            State := THPackState.READ_MAX_DYNAMIC_TABLE_SIZE;
          end else begin
            SetDynamicTableSize(Findex);
            State := THPackState.READ_HEADER_REPRESENTATION;
          end;
        end else begin
          // Literal Header Field without Indexing / never Indexed
          if (b and $10) = $10 then begin
            IndexType:=THPackIndexType.eHPackNEVER;
          end else begin
            IndexType:=THPackIndexType.eHPackNONE;
          end;
          Findex := b and $0F;
          if (Findex = 0) then begin
            State := THPackState.READ_LITERAL_HEADER_NAME_LENGTH_PREFIX;
          end else if (Findex = $0F) then begin
            State := THpackState.READ_INDEXED_HEADER_NAME;
          end else begin
            // Index was stored as the prefix
            ReadName(FIndex);
            State := THpackState.READ_LITERAL_HEADER_VALUE_LENGTH_PREFIX;
          end;
        end;
      end;
    READ_MAX_DYNAMIC_TABLE_SIZE: begin
        MaxSize := decodeULE128(aStream);
        if (MaxSize = -1) then begin
          exit;
        end;
        // Check for numerical overflow
        if (MaxSize > High(Integer) - Findex) then begin
          Raise THPACKException.Create('Decompression exception in Decode-READ_MAX_DYNAMIC_TABLE_SIZE');
        end;
        SetDynamicTableSize(Findex + MaxSize);
        State := THPackState.READ_HEADER_REPRESENTATION;
      end;
    READ_INDEXED_HEADER: begin
        HeaderIndex := decodeULE128(aStream);
        if (HeaderIndex = -1) then begin
          exit;
        end;
        // Check for numerical overflow
        if (HeaderIndex > High(Integer) - Findex) then begin
          Raise THPACKException.Create('Decompression exception in Decode-READ_INDEXED_HEADER');
        end;

        IndexHeader(Findex + HeaderIndex);
        State := THPackState.READ_HEADER_REPRESENTATION;
      end;
    READ_INDEXED_HEADER_NAME: begin
        // Header Name matches an entry in the Header Table
        NameIndex := decodeULE128(aStream);
        if (NameIndex = -1) then begin
          Exit;
        end;

        // Check for numerical overflow
        if (NameIndex > High(Integer) - Findex) then begin
          Raise THPACKException.Create('Decompression exception in Decode-READ_INDEXED_HEADER_NAME');
        end;

        ReadName(Findex + NameIndex);
        State := THPackState.READ_LITERAL_HEADER_VALUE_LENGTH_PREFIX;
      end;
    READ_LITERAL_HEADER_NAME_LENGTH_PREFIX: begin
        b := aStream.ReadByte;
        HuffmanEncoded := (b and $80) = $80;
        Findex := b and $7F;
        if (Findex = $7f) then begin
          State := THPackState.READ_LITERAL_HEADER_NAME_LENGTH;
        end else begin
          NameLength := Findex;

          // Disallow empty names -- they cannot be represented in HTTP/1.x
          if (NameLength = 0) then begin
            Raise THPACKException.Create('Empty name');
          end;

          // Check name length against max header size
          if ExceedsMaxHeaderSize(NameLength) then begin
            if (IndexType = THPackIndexType.eHPackNONE) then begin
              // Name is unused so skip bytes
              FName := '';
              SkipLength := NameLength;
              State := THPackState.SKIP_LITERAL_HEADER_NAME;
              break;
            end;

            // Check name length against max dynamic table size
            if (NameLength + HPACK_HEADER_ENTRY_OVERHEAD > DynamicTable.Capacity) then begin
              DynamicTable.Clear();
              Fname := '';
              SkipLength := NameLength;
              State := THPackState.SKIP_LITERAL_HEADER_NAME;
              break;
            end;
          end;
          State := THPackState.READ_LITERAL_HEADER_NAME;
        end;
      end;
    READ_LITERAL_HEADER_NAME_LENGTH: begin
        // Header Name is a Literal String
        NameLength := decodeULE128(aStream);
        if (NameLength = -1) then begin
          exit;
        end;

        // Check for numerical overflow
        if (NameLength > High(Integer) - Findex) then begin
          Raise THPACKException.Create('Decompression exception in Decode-READ_LITERAL_HEADER_NAME_LENGTH');
        end;
        inc(NameLength,Findex);

        // Check name length against max header size
        if ExceedsMaxHeaderSize(NameLength) then begin
          if (IndexType = THPackIndexType.eHPackNONE) then begin
            // Name is unused so skip bytes
            Fname := '';
            SkipLength := NameLength;
            State := THPackState.SKIP_LITERAL_HEADER_NAME;
            break;
          end;

          // Check name length against max dynamic table size
          if (NameLength + HPACK_HEADER_ENTRY_OVERHEAD > DynamicTable.Capacity) then begin
            DynamicTable.Clear();
            Fname := '';
            SkipLength := NameLength;
            State := THPackState.SKIP_LITERAL_HEADER_NAME;
            break;
          end;
          State := THPackState.READ_LITERAL_HEADER_NAME;
        end;
      end;
    READ_LITERAL_HEADER_NAME: begin
        // Wait until entire name is readable
        if (InAvailable() < NameLength) then begin
          exit;
        end;

        FName := ReadStringLiteral(aStream, NameLength);

        State := THPackState.READ_LITERAL_HEADER_VALUE_LENGTH_PREFIX;
      end;
    SKIP_LITERAL_HEADER_NAME: begin
        SetLength(tmpbuffer,SkipLength);
        dec(SkipLength, aStream.Read(tmpbuffer[1],SkipLength));

        if (SkipLength = 0) then begin
          State := THPackState.READ_LITERAL_HEADER_VALUE_LENGTH_PREFIX;
        end;
      end;
    READ_LITERAL_HEADER_VALUE_LENGTH_PREFIX: begin
        b := aStream.ReadByte;
        HuffmanEncoded := (b and $80) = $80;
        Findex := b and $7F;
        if (Findex = $7f) then begin
          State := THPackState.READ_LITERAL_HEADER_VALUE_LENGTH;
        end else begin
          ValueLength := Findex;

          // Check new header size against max header size
          NewHeaderSize := NameLength + ValueLength;
          if ExceedsMaxHeaderSize(NewHeaderSize) then begin
            // truncation will be reported during EndHeaderBlockTruncated
            HeaderSize := MaxHeaderSize + 1;

            if (IndexType = THPackIndexType.eHPackNONE) then begin
              // Value is unused so skip bytes
              State := THPackState.SKIP_LITERAL_HEADER_VALUE;
              break;
            end;

            // Check new header size against max dynamic table size
            if NewHeaderSize + HPACK_HEADER_ENTRY_OVERHEAD > DynamicTable.Capacity then begin
              DynamicTable.Clear();
              State := THPackState.SKIP_LITERAL_HEADER_VALUE;
              break;
            end;
          end;

          if (ValueLength = 0) then begin
            InsertHeader(FName, '', IndexType);
            State := THPackState.READ_HEADER_REPRESENTATION;
          end else begin
            State := THPackState.READ_LITERAL_HEADER_VALUE;
          end;
        end;
      end;
    READ_LITERAL_HEADER_VALUE_LENGTH: begin
        // Header Value is a Literal String
        ValueLength := decodeULE128(aStream);
        if (ValueLength = -1) then begin
          Exit;
        end;

        // Check for numerical overflow
        if (ValueLength > High(Integer) - Findex) then begin
          Raise THPACKException.Create('Decompression exception in Decode-READ_LITERAL_HEADER_VALUE_LENGTH');
        end;

        inc(ValueLength,Findex);

        // Check new header size against max header size
        NewHeaderSize := NameLength + ValueLength;
        if (NewHeaderSize + HeaderSize > MaxHeaderSize) then begin
          // truncation will be reported during EndHeaderBlockTruncated
          HeaderSize := MaxHeaderSize + 1;

          if (IndexType = THPackIndexType.eHPackNONE) then begin
            // Value is unused so skip bytes
            State := THPackState.SKIP_LITERAL_HEADER_VALUE;
            break;
          end;

          // Check new header size against max dynamic table size
          if (NewHeaderSize + HPACK_HEADER_ENTRY_OVERHEAD > DynamicTable.Capacity) then begin
            DynamicTable.Clear();
            State := THPackState.SKIP_LITERAL_HEADER_VALUE;
            break;
          end;
        end;
        State := THPackState.READ_LITERAL_HEADER_VALUE;
      end;
    READ_LITERAL_HEADER_VALUE: begin
        // Wait until entire value is readable
        if (InAvailable() < ValueLength) then begin
          Exit;
        end;

        Value := ReadStringLiteral(aStream, ValueLength);
        InsertHeader(FName, value, IndexType);
        State := THPackState.READ_HEADER_REPRESENTATION;
      end;
    SKIP_LITERAL_HEADER_VALUE: begin
        SetLength(tmpbuffer,ValueLength);
        dec(ValueLength, aStream.Read(tmpbuffer[1],ValueLength));

        if (ValueLength = 0) then begin
          State := THPackState.READ_HEADER_REPRESENTATION;
        end;
      end;
    end;
  end;
end;

procedure THPackDecoder.Decode(aString: RawByteString);
var
  Stream: TStringStream;
begin
  Stream:=TStringStream.Create(aString);
  try
    Decode(Stream);
  finally
    Stream.Free;
  end;
end;

function THPackDecoder.GetMaxHeaderTableSize: Integer;
begin
  Result:=DynamicTable.Capacity;
end;

{ THPackDynamicTable }

procedure THPackDynamicTable.SetCapacity(aCapacity: integer);
(*
 * Set the maximum Size of the dynamic table.
 * Entries are evicted from the dynamic table until the Size of the table
 * is less than or equal to the maximum Size.
 *)
var
  tmp: array of THPackHeaderField;
  MaxEntries: integer;
  Len,Cursor, i: integer;
  Entry: THPackHeaderField;
begin
  if aCapacity < 0 then begin
    Raise THPACKException.Create('Illegal Capacity: '+ inttostr(acapacity));
  end;

  // initially FCapacity will be -1 so init won't return here
  if FCapacity = aCapacity then begin
    exit;
  end;
  FCapacity := aCapacity;

  if FCapacity = 0 then begin
    Clear;
  end else begin
    // initially FSize will be 0 so remove won't be called
    while (FSize > FCapacity) do begin
      Remove().Free;
    end;
  end;
  MaxEntries := aCapacity div HPACK_HEADER_ENTRY_OVERHEAD;
  if (Acapacity mod HPACK_HEADER_ENTRY_OVERHEAD <> 0) then begin
    inc(MaxEntries);
  end;

  // check if FCapacity change requires us to reallocate the array
  if (Length(FHeaderFields)<>0) and (Length(FHeaderFields) = MaxEntries) then begin
    exit;
  end;

  SetLength(tmp,MaxEntries);

  // initially length will be 0 so there will be no copy
  Len := EntriesCount();
  Cursor := Ftail;
  for i := 0 to Pred(Len) do begin
    Entry:=FHeaderFields[Cursor];
    inc(Cursor);
    tmp[i]:=Entry;
    if Cursor=Length(FHeaderFields) then begin
      Cursor:=0;
    end;
  end;

  Ftail := 0;
  Fhead := Ftail + Len;
  FheaderFields := tmp;

end;

procedure THPackDynamicTable.Clear;
begin
  while (FTail <> FHead) do begin
    FHeaderFields[Ftail].Free;
    FHeaderFields[Ftail]:=nil;
    inc(FTail);
    if FTail = Length(FheaderFields) then begin
      FTail := 0;
    end;
  end;
  FHead := 0;
  FTail := 0;
  FSize := 0;
end;

function THPackDynamicTable.Remove: THPackHeaderField;
var
  Removed: THPackHeaderField;
begin
  Removed := FHeaderFields[Ftail];
  if (Removed = nil) then begin
    exit(nil);
  end;
  dec(FSize,Removed.Size());
  FHeaderFields[Ftail] := nil;
  inc(FTail);
  if FTail = Length(FheaderFields) then begin
    FTail := 0;
  end;
  Result:= Removed;
end;

function THPackDynamicTable.EntriesCount: integer;{$IFDEF USEINLINE}inline;{$ENDIF}
begin
  if FHead < FTail then begin
    Result:= Length(FHeaderFields) - FTail + FHead;
  end else begin
    Result:= FHead - FTail;
  end;
end;

constructor THPackDynamicTable.Create(aInitialCapacity: integer);
begin
  SetCapacity(aInitialCapacity);
end;

destructor THPackDynamicTable.Destroy;
var
  j: integer;
begin
  for j := Low(FHeaderFields) to High(FHeaderFields) do begin
    FHeaderFields[j].Free;
    FHeaderFields[j]:=nil;
  end;
  inherited Destroy;
end;

procedure THPackDynamicTable.Add(aHeaderField: THPackHeaderField);
var
  HeaderSize: integer;
begin
  HeaderSize := aHeaderField.size;
  if HeaderSize > Fcapacity then begin
    Clear;
    exit;
  end;
  while (Fsize + HeaderSize > FCapacity) do begin
    Remove().Free;
  end;
  FHeaderFields[FHead] := aHeaderField;
  inc(FHead);
  inc(FSize,aHeaderField.Size);
  if FHead = Length(FHeaderFields) then begin
    FHead := 0;
  end;
end;

function THPackDynamicTable.GetEntry(aIndex: integer): THPackHeaderField; {$IFDEF USEINLINE}inline;{$ENDIF}
var
  i: integer;
begin
  if (aIndex <= 0) or (aIndex > EntriesCount()) then begin
    Raise THPACKException.Create('Index out of bounds in GetEntry');
  end;
  i := FHead - aIndex;
  if i < 0 then begin
    Result:= FHeaderFields[i + Length(FHeaderFields)];
  end else begin
    Result:= FHeaderFields[i];
  end;
end;

function THPackDynamicTable.GetNameIndex(const aName: RawByteString): integer;
var
  j: integer;
  H: THPackHeaderField;
begin
  for j := 1 to Pred(EntriesCount()) do begin
    H:=GetEntry(j);
    if H.FName=aName then begin
      Result:=j;
      Exit;
    end;
  end;
  Result:=NOT_FOUND;
end;

function THPackDynamicTable.GetEntry(const aName, aValue: RawByteString
  ): THPackHeaderField;
var
  j: integer;
  H: THPackHeaderField;
begin
  for j := 1 to Pred(EntriesCount()) do begin
    H:=GetEntry(j);
    if (H.FName=aName) and (H.FValue=aValue) then begin
      Result:=H;
      Exit;
    end;
  end;
  Result:=nil;
end;

function THPackDynamicTable.GetEntryIndex(const aName, aValue: RawByteString
  ): integer;
var
  j: integer;
  H: THPackHeaderField;
begin
  for j := 1 to Pred(EntriesCount()) do begin
    H:=GetEntry(j);
    if (H.FName=aName) and (H.FValue=aValue) then begin
      Result:=j;
      Exit;
    end;
  end;
  Result:=NOT_FOUND;
end;

function THPackDynamicTable.GetEntryIndex(const aHeader: THPackHeaderField
  ): integer;
var
  j: integer;
  H: THPackHeaderField;
begin
  for j := 1 to Pred(EntriesCount()) do begin
    H:=GetEntry(j);
    if H=aHeader then begin
      Result:=j;
      Exit;
    end;
  end;
  Result:=NOT_FOUND;
end;

procedure THPackDynamicTable.EnsureCapacityToHold(const aHeaderSize: integer);
var
  Index: integer;
begin
  while (Size + aHeaderSize > FCapacity) do begin
    Index := EntriesCount;
    if Index = 0 then begin
      break;
    end;
    Remove.Free;
  end;
end;

{ THPackHuffmanNode }

class procedure THPackHuffmanNode.Insert(aRoot: THPackHuffmanNode;
  aSymbol: integer; aCode: integer; aLength: BYTE);
var
  Current: THPackHuffmanNode;
  Terminal: THPackHuffmanNode;
  i: integer;
  Shift,Start,iEnd: integer;
begin
  // traverse tree using the most significant bytes of code
  Current:=aRoot;
  while (aLength > 8) do begin
    if (Current.isTerminal) then begin
      Raise THPACKException.Create('Invalid Huffman code: prefix not unique');
    end;
    dec(aLength,8);
    i := integer((DWORD(aCode) {Unsigned Shift} shr aLength) and DWORD($FF));
    if (Current.FChildren[i] = Nil) then begin
      Current.FChildren[i] := THPackHuffmanNode.Create;
    end;
    Current := Current.FChildren[i];
  end;

  Terminal := THPackHuffmanNode.Create(aSymbol, aLength);
  Shift := 8 - aLength;
  Start := (aCode shl Shift) and $FF;
  iEnd := 1 << Shift;
  for i := Start to Pred(Start + iEnd) do begin
    Current.FChildren[i]:=Terminal;
  end;
end;

class function THPackHuffmanNode.BuildTree(const aCodes: PDWORD;
  const aLengths: PByte): THPackHuffmanNode;
var
  Root: THPackHuffmanNode;
  i: integer;
begin
    Root := THPackHuffmanNode.Create;
    for i := 0 to Pred(HPACK_HUFFMAN_CODES_LENGTH) do begin
      Insert(Root,i,aCodes[i],aLengths[i]);
    end;
    Result:=Root;
end;

constructor THPackHuffmanNode.Create;
begin
  FSymbol := 0;
  FBits := 8;
  SetLength(FChildren,256);
end;

constructor THPackHuffmanNode.Create(aSymbol: integer; aBits: integer);
begin
  //assert(FBits > 0 && FBits <= 8);
  if (aBits<1) or (aBits > 8) then begin
    Raise THPACKException.Create('BUG');
  end;
  FSymbol := aSymbol;
  FBits := aBits;
  SetLength(FChildren,0);
end;

destructor THPackHuffmanNode.Destroy;
var
  j,i: Integer;
  Node: THPackHuffmanNode;
begin
  for j := Low(FChildren) to high(FChildren) do begin
    Node:=FChildren[j];
    if Assigned(Node) then begin
      for i := j to High(FChildren) do begin
        if Node=FChildren[i] then begin
          FChildren[i]:=nil;
        end;
      end;
    end;
    Node.Free;
  end;
  inherited Destroy;
end;

function THPackHuffmanNode.isTerminal: Boolean;
begin
  if Length(FChildren)=0 then begin
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

{ THPackHuffmanDecoder }

constructor THPackHuffmanDecoder.Create;
begin
  Create(HPackHuffmanCodes,HPackHuffmanCodeLength);
end;

constructor THPackHuffmanDecoder.Create(const aCodes: PDWORD;
  const aLengths: PByte);
begin
  FCodes:=aCodes;
  FLengths:=aLengths;
  FRoot:=THPackHuffmanNode.BuildTree(aCodes,aLengths);
end;

destructor THPackHuffmanDecoder.Destroy;
begin
  FreeAndNil(FRoot);
  inherited Destroy;
end;

function THPackHuffmanDecoder.Decode(aBuf: RawByteString): RawByteString;
var
  WritePoint: integer;
  RealSize: integer;
  Node: THPackHuffmanNode;
  Current: integer;
  Bits: Integer;
  i,b,c: Integer;
  Mask: integer;
  OutputBuffer: RawByteString;
  procedure WriteByte(const aByte: Byte); {$IFDEF USEINLINE}inline;{$ENDIF}
  begin
    if WritePoint>RealSize then begin
      SetLength(OutputBuffer,RealSize*2);
      RealSize:=RealSize*2;
    end;
    Byte(OutputBuffer[WritePoint]):=aByte;
    inc(WritePoint);
  end;
begin
  if aBuf='' then begin
    Result:='';
    exit;
  end;
  WritePoint:=1;
  RealSize:=Length(aBuf)*2; //Huffman usually reach a 50% compress at best.
  SetLength(OutputBuffer,RealSize);
  Node := FRoot;
  Current := 0;
  Bits := 0;
  for i := 0 to Pred(Length(aBuf)) do begin
    b := Byte(aBuf[i+1]);
    Current := (current shl 8) or b;
    inc(Bits,8);
    while (Bits >= 8) do begin
      c := integer((DWORD(Current) {unsigned shift} shr (Bits - 8)) and DWORD($FF));
      Node := Node.FChildren[c];
      dec(Bits,Node.FBits);
      if (Node.isTerminal) then begin
        if (Node.FSymbol = HPACK_HUFFMAN_EOS) then begin
          Raise THPACKException.Create('EOS_DECODED');
        end;
        WriteByte(Byte(Node.FSymbol));
        Node := Froot;
      end;
    end;
  end;
  while (Bits > 0) do begin
    c := (current shl (8 - Bits)) and $FF;
    Node := Node.FChildren[c];
    if (Node.isTerminal and (Node.FBits <= Bits)) then begin
      dec(Bits,Node.FBits);
      WriteByte(Byte(Node.FSymbol));
      Node := Froot;
    end else begin
      break;
    end;
  end;

  // Section 5.2. String Literal Representation
  // Padding not corresponding to the most significant Bits of the code
  // for the EOS symbol (0xFF) MUST be treated as a decoding error.
  Mask := (1 shl Bits) - 1;
  if (current and Mask) <> Mask then begin
    Raise THPACKException.Create('INVALID_PADDING');
  end;
  SetLength(OutputBuffer,WritePoint-1);
  Result:=OutputBuffer;
end;

{ THPackHuffmanEncoder }

constructor THPackHuffmanEncoder.Create;
begin
  Create(HPackHuffmanCodes,HPackHuffmanCodeLength);
end;

constructor THPackHuffmanEncoder.Create(const aCodes: PDWORD;
  const aLengths: PByte);
begin
  FCodes:=aCodes;
  FLengths:=aLengths;
end;

procedure THPackHuffmanEncoder.Encode(aOutputStream: TStream; aData: RawByteString);
begin
  Encode(aOutputStream, aData, 0, Length(aData));
end;

procedure THPackHuffmanEncoder.Encode(aOutputStream: TStream;
  aData: RawByteString; aOff, aLen: integer);
var
  Current: DWORD=0;
  n: integer=0;
  i: integer;
  Code: DWORD;
  b,nBits: integer;
  v: DWORD;
begin
  if not Assigned(aOutputStream) then begin
    Raise THPACKException.Create('Output stream is nil');
  end else if aData = '' then begin
    Raise THPACKException.Create('Data is empty');
  end else if ((aOff < 0) or (aLen < 0) or ((aOff + aLen) < 0) or (aOff > Length(aData)) or ((aOff + aLen) > Length(aData))) then begin
    Raise THPACKException.Create('Index out of bounds');
  end else if aLen = 0 then begin
    exit;
  end;

  for i := 0 to Pred(aLen) do begin
    b := BYTE(aData[aOff + i + 1]) and $FF;
    Code := FCodes[b];
    nBits := FLengths[b];

    Current := Current shl nBits;
    Current := Current or Code;
    inc(n,nBits);

    while (n >= 8) do begin
      dec(n,8);
      v:=Current shr n;
      aOutputStream.WriteByte(Byte(v));
    end;
  end;

  if (n > 0) then begin
    Current := Current shl (8-n);
    Current:=Current or (DWORD($FF) {unsigned shift} shr n); // this should be EOS symbol
    aOutputStream.WriteByte(Byte(Current));
  end;
end;

function THPackHuffmanEncoder.GetEncodedLength(aData: RawByteString
  ): integer;
var
  Len: integer;
  i: integer;
begin
  if aData = '' then begin
    Raise THPACKException.Create('Data is empty');
  end;

  Len := 0;
  for i := 1 to Length(aData) do begin
    inc(Len,FLengths[BYTE(aData[i])]);
  end;
  Result:=(Len + 7) shr 3;
end;

{ THPackHeaderField }

class function THPackHeaderField.SizeOf(const aName, aValue: RawByteString
  ): Integer;
begin
  Result:=Length(aName) + Length(aValue) + HEADER_ENTRY_OVERHEAD;
end;

constructor THPackHeaderField.Create(const aName, aValue: RawByteString);
begin
  FName:=aName;
  FValue:=aValue;
end;

function THPackHeaderField.Size: Integer;
begin
  Result:=Length(FName) + Length(FValue) + HEADER_ENTRY_OVERHEAD;
end;

initialization
  THPackStaticTable.InitializeStaticTable;
finalization;
  THPackStaticTable.DestroyStaticTable;

end.

