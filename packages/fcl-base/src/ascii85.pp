{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2008 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
// Original header

// I, Danny Milosavljevic, hereby release this code into the public domain.

unit ascii85;

{$M+}
{$MODE OBJFPC}

// Based on C# code from <http://www.codinghorror.com/blog/archives/000410.html> by Jeff Atwood,
//   which is based on C code from <http://www.stillhq.com/cgi-bin/cvsweb/ascii85/>.

interface
uses sysutils, classes;

type
  TASCII85State = (ascInitial = 0, ascOneEncodedChar = 1, ascTwoEncodedChars = 2, ascThreeEncodedChars = 3, ascFourEncodedChars = 4, ascNoEncodedChar = 5, ascPrefix = 6);
  TASCII85RingBuffer = class
  private
    fBuffer : array[0..1023] of Byte;
    fBufferReadPosition : Cardinal;
    fBufferWritePosition : Cardinal;
    fBufferFillCount : Cardinal;
  protected
    function GetBufferSize() : Cardinal; inline;
  published
    property FillCount : Cardinal read fBufferFillCount;
    property Size : Cardinal read GetBufferSize;
    procedure Write(const aBuffer; aSize : Cardinal); inline;
    function Read(var aBuffer; aSize : Cardinal) : Cardinal; inline;
  end;

  TASCII85DecoderStream = class(TOwnerStream)
  private
    fBExpectBoundary : Boolean;
    fTuple : Cardinal;
    fState : TASCII85State;
    fBEOF : Boolean;
    fBSourceEOF : Boolean;
    fBuffer : TASCII85RingBuffer;
    fPosition : Int64;

    // decoded data:
    fEncodedBuffer : array[0..((1024 * 5 + 3) div 4) - 1] of Byte; // 1280. // could also be put on the stack, doesn't need to persist between calls.
  private
    procedure BufferByte(aValue : Byte); inline;
    procedure BufferTuple(aValue : Cardinal; aDecodedCount : Cardinal); inline; // decoding shrinks from 5 byte to 4 byte.
  published
    constructor Create(aStream : TStream);
    procedure Decode(aInput : Byte); inline;
    procedure Close();
    function ClosedP() : Boolean; inline;
    property BExpectBoundary : Boolean read fBExpectBoundary write fBExpectBoundary;
  protected
    function GetPosition() : Int64; override;
  public
    destructor Destroy(); override;
    function Read(var aBuffer; aCount : longint) : longint; override;
    function Seek(aOffset : longint; aOrigin : word) : longint; override;
    function Seek(const aOffset: Int64; aOrigin: TSeekOrigin): Int64; override; overload;
  end;

  // TODO encoder...
  TASCII85EncoderStream = class(TOwnerStream)
  private
    FPos,
    FTuple : Cardinal;
    FCount,
    FWidth : Integer;
    FBoundary : Boolean;
  protected  
    Procedure WriteBoundary;
    Procedure Flush;
    procedure Encode;
  public
    Constructor Create(ADest: TStream; AWidth : Integer = 72; ABoundary : Boolean = False); 
    Destructor Destroy; Override;
    function Write(Const aBuffer; aCount : longint) : longint; override;
    Property Width : Integer Read FWidth;
    Property Boundary : Boolean Read FBoundary;
  end;                      
        

implementation

{ TASCII85EncoderStream }

Procedure TASCII85EncoderStream.WriteBoundary;

Const
  SBoundary = '<~';

begin
  Source.Write(SBoundary[1],2);
  FPos:=2;
end;

Procedure TASCII85EncoderStream.Encode;

Var
  S : String[7];
  I,J : Integer;
  Buf : Array[0..4] of Byte;
  
begin
  If (FTuple=0) then
    begin
    // Write 'z'
    S:='z';
    Inc(FPos);
    If (FPos>FWidth) then
      begin
      S:=S+sLineBreak;
      FPos:=0;
      end;
    end
  else
    begin  
    For I:=0 to 4 do
      begin
      Buf[i]:=FTuple mod 85;
      FTuple:=FTuple div 85;
      end;
    J:=0;  
    S:='';
    For I:=FCount+1 downto 0 do
      begin
      Inc(j);
      S[J]:=Char(Buf[i]+Ord('!'));
      SetLength(S,J);
      Inc(FPos);
      If (FPos>FWidth) then
        begin
        FPos:=0;
        S:=S+sLinebreak;
        J:=Length(S);
        end;
      end;
    end;
  Source.Write(S[1],Length(S));
  FTuple:=0;
  FCount:=-1;
end;


Procedure TASCII85EncoderStream.Flush;

Const 
  Boundary1 = '~>'+slinebreak;
  Boundary2 = slinebreak+Boundary1;
  
Var
  S : String;

begin
  If FCount>0 then
    Encode;
  If FBoundary then
    begin
    If FPos+2>FWidth then
      S:=Boundary2
    else
      S:=Boundary1;
    Source.Write(S[1],Length(S));
    FBoundary:=False;
    end;
end;

Constructor TASCII85EncoderStream.Create(ADest: TStream; AWidth : Integer = 72; ABoundary : Boolean = False);

begin
  Inherited Create(ADest);
  FWidth:=AWidth;
  FBoundary:=ABoundary;
  If FBoundary then
    WriteBoundary;
end;

Destructor TASCII85EncoderStream.Destroy; 

begin
  Flush;
  Inherited;
end;

function TASCII85EncoderStream.Write(Const aBuffer; aCount : longint) : longint;

Var
  P : PByte;
  C : Byte;
  
begin
  P:=@Abuffer;  
  Result:=ACount;
  While ACount>0 do
    begin
    C:=P^;
    Case FCount of
      0 : FTuple:=FTuple or (C shl 24);
      1 : FTuple:=FTuple or (C shl 16);
      2 : FTuple:=FTuple or (C shl 8);
      3 : begin
          FTuple:=FTuple or C;
          encode;
          end;
     end;     
     Inc(FCount);
     Inc(P);
     Dec(ACount);
     end;
end;
                     
{ TRingBuffer }

function TASCII85RingBuffer.GetBufferSize() : Cardinal; inline;
begin
  Result := Length(fBuffer);
end;

procedure TASCII85RingBuffer.Write(const aBuffer; aSize : Cardinal); inline;
var
  vBuffer : PByte;
begin
  vBuffer := @aBuffer;
  // TODO optimize.

  while aSize > 0 do begin
    assert(fBufferFillCount < Length(fBuffer));
    fBuffer[fBufferWritePosition] := vBuffer^;
    Inc(vBuffer);
    Inc(fBufferFillCount);
    Inc(fBufferWritePosition);
    if fBufferWritePosition >= Length(fBuffer) then
      fBufferWritePosition := 0;

    assert(fBufferWritePosition <> fBufferReadPosition);
    Dec(aSize);
  end;
end;

function TASCII85RingBuffer.Read(var aBuffer; aSize : Cardinal) : Cardinal; inline;
var
  vBuffer : PByte;
  vBufferCount : Cardinal;
  vBufferCount1 : Cardinal;
  vBufferCount2 : Cardinal;
begin
  vBuffer := @aBuffer;
  Result := 0;

  if fBufferFillCount > 0 then begin
      vBufferCount := aSize; // try to take as much as requested by the client...
      if vBufferCount > fBufferFillCount then // ... if possible.
        vBufferCount := fBufferFillCount;

      if fBufferReadPosition < fBufferWritePosition then begin {  ------RXXXXXXW-------- }
        vBufferCount1 := fBufferWritePosition - fBufferReadPosition; // max count for the first Move.
        assert(vBufferCount <= vBufferCount1);
        Move(fBuffer[fBufferReadPosition], vBuffer^, vBufferCount);
        Inc(vBuffer, vBufferCount);
      end else begin                                           {  XXXW-----RXXXXXXXXXXXX }
        vBufferCount1 := Length(fBuffer) - fBufferReadPosition; // count for the first Move.
        if vBufferCount < vBufferCount1 then
          vBufferCount1 := vBufferCount;

        if vBufferCount1 > 0 then begin
          Move(fBuffer[fBufferReadPosition], vBuffer^, vBufferCount1);
          Inc(vBuffer, vBufferCount1);
        end;

        vBufferCount2 := vBufferCount - vBufferCount1; // remaining count for the second Move.

        if vBufferCount2 > 0 then begin
          Move(fBuffer[0], vBuffer^, vBufferCount2);
          Inc(vBuffer, vBufferCount2);
        end;
      end;

      Inc(fBufferReadPosition, vBufferCount);
      while fBufferReadPosition >= Length(fBuffer) do
        Dec(fBufferReadPosition, Length(fBuffer));

      assert(fBufferFillCount >= vBufferCount);
      Dec(fBufferFillCount, vBufferCount);
      Inc(Result, vBufferCount);
  end;
end;

{ TDecoder }

const
  cPow85 : array[0..4] of Cardinal = (85*85*85*85, 85*85*85, 85*85, 85, 1); // uint

function DecodeNonTrivialByte(aInput : Byte) : Cardinal; inline;
begin
  if (aInput >= ord('!')) and (aInput <= ord('u')) then
    Result := aInput - ord('!')
  else
    raise EConvertError.Create(Format('could not decode value %d', [aInput]));
//  if chr(aInput) in ['!'..'u'] then
end;

constructor TASCII85DecoderStream.Create(aStream : TStream);
begin
  inherited Create(aStream);
  fBuffer := TASCII85RingBuffer.Create();
end;

procedure TASCII85DecoderStream.BufferByte(aValue : Byte); inline;
begin
  fBuffer.Write(aValue, 1);
end;

procedure TASCII85DecoderStream.BufferTuple(aValue : Cardinal; aDecodedCount { DECODED!!!} : Cardinal); inline;
begin
  if aDecodedCount >= 1 then begin
    BufferByte(aValue shr (24 - (0 * 8)));
    if aDecodedCount >= 2 then begin
      BufferByte((aValue shr (24 - (1 * 8))) and $ff);
      if aDecodedCount >= 3 then begin
        BufferByte((aValue shr (24 - (2 * 8))) and $ff);
        if aDecodedCount >= 4 then begin
          BufferByte((aValue shr (24 - (3 * 8))) and $ff);
          if aDecodedCount >= 5 then begin
            raise EConvertError.Create('not enough decoded data (internal error).');
          end;
        end;
      end;
    end;
  end;
end;

procedure TASCII85DecoderStream.Decode(aInput : Byte);
begin
  if (aInput in [ 10, 13, 9, {0, 8,} 12, 32]) and (fState <> ascPrefix { chicken}) then // skip whitespace.
    Exit;

  case fState of
  ascInitial, ascNoEncodedChar:
    if aInput = ord('z') then begin
      BufferTuple(0, 4);
    end else begin
      if (aInput = ord('<')) and (fState = ascInitial) {and (fBExpectBoundary)} then begin
        fState := ascPrefix;
      end else begin
        fTuple := fTuple + DecodeNonTrivialByte(aInput) * cPow85[0];
        fState := ascOneEncodedChar;
      end;
    end;
  ascOneEncodedChar:
    begin
      fTuple := fTuple + DecodeNonTrivialByte(aInput) * cPow85[1];
      fState := ascTwoEncodedChars;
    end;
  ascTwoEncodedChars:
    begin
      fTuple := fTuple + DecodeNonTrivialByte(aInput) * cPow85[2];
      fState := ascThreeEncodedChars;
    end;
  ascThreeEncodedChars:
    begin
      fTuple := fTuple + DecodeNonTrivialByte(aInput) * cPow85[3];
      fState := ascFourEncodedChars;
    end;
  ascFourEncodedChars:
    begin
      fTuple := fTuple + DecodeNonTrivialByte(aInput) * cPow85[4];

      BufferTuple(fTuple, 4);
      fTuple := 0;
      fState := ascNoEncodedChar;
    end;
  ascPrefix:
    begin
      if aInput = ord('~') then begin
        fBExpectBoundary := True;
        fState := ascNoEncodedChar
      end else begin
        // whoops, actually "~" is outside the allowed range, so we CAN find out whether there was supposed to be a boundary string or not on our own...
        // we reached this place since we saw a '<', thought it was part of '<~', but it wasn't. '<' is an allowed encoded character.

        // catch up on work we should have been doing...

        assert(fTuple = 0);

        fTuple := fTuple + DecodeNonTrivialByte(ord('<')) * cPow85[0];
        //fState := ascOneEncodedChar;
        fTuple := fTuple + DecodeNonTrivialByte(aInput) * cPow85[1];
        fState := ascTwoEncodedChars;

        //raise EConvertError.Create(Format('expected ''<~'', got %d', [aInput]));
      end;
    end
  else
    raise EConvertError.Create('internal error');
  end;
end;

destructor TASCII85DecoderStream.Destroy();
begin
  Self.Close();
  FreeAndNil(fBuffer);
  inherited Destroy;
end;

function TASCII85DecoderStream.ClosedP() : Boolean; inline;
begin
  Result := (fState in [ascInitial, ascNoEncodedChar, ascPrefix]);
end;

procedure TASCII85DecoderStream.Close();
var
  vCount : Cardinal;
begin
  if fState = ascPrefix then
    raise EConvertError.Create('unexpected end of file while trying to find ''<~'' prefix (after the ''<'' was seen).');

  if not (fState in [ascInitial, ascNoEncodedChar, ascPrefix]) then begin // we have some bytes left over.
    if fState = ascOneEncodedChar then
      raise EConvertError.Create('The last block of ASCII85 data cannot be a single byte.');

    vCount := Cardinal(fState) - 1; // one less!!

    fTuple := fTuple + cPow85[vCount];
    BufferTuple(fTuple, vCount);

    fState := ascInitial;
  end;
end;

function TASCII85DecoderStream.Read(var aBuffer; aCount : longint) : longint;
var
  vAvailableCount : Cardinal;
  vPermittedReadCount : Cardinal;
  vEncodedBufferCount : Cardinal;
  vEncodedBufferIndex : Cardinal;
  vItem : Byte;
  vBuffer : PByte;
  vBufferCount : Cardinal;
begin
  vBuffer := @aBuffer;

  Result := 0;
  if fBEOF then begin
    Exit;
  end;

  repeat
    // first use up the buffer contents as far as possible.

    if aCount <= 0 then
      Break;

    vBufferCount := fBuffer.Read(vBuffer^, aCount);
    assert(vBufferCount <= aCount);

    Inc(vBuffer, vBufferCount);
    Dec(aCount, vBufferCount);
    Inc(Result, vBufferCount);

    if fBSourceEOF and (vBufferCount = 0) then begin
      fBEOF := True;
      Break;
    end;

    if aCount <= 0 then
      Break;

    // here, aCount contains the REMAINING request and the buffer is either empty or there wasn't that much needed anyway (in the latter case the Exit above finished the function).
    // if then, there's still something needed, fill the buffer only as far as we need to.

    assert(fBuffer.FillCount = 0);

    vAvailableCount := fBuffer.Size - fBuffer.FillCount;
    vPermittedReadCount := vAvailableCount shr 2; // worst-case, decoded data will grow 4x ('z' -> '0000').
    {if aCount < vAvailableCount then begin
      vAvailableCount := aCount;}

    vEncodedBufferCount := 0;
    if not fBSourceEOF then
      vEncodedBufferCount := Source.Read(fEncodedBuffer[0], vPermittedReadCount);

    if (vEncodedBufferCount = 0) then begin // EOF
      fBSourceEOF := True;
      if not ClosedP() then
        Close() // make sure we catch the "virtual characters". This could fill the buffer a little bit.
      {else
        fBEOF := True};
      Continue;
    end else     // Buffer the output we couldn't pass on so far.
    for vEncodedBufferIndex := 0 to vEncodedBufferCount - 1 do begin
      vItem := fEncodedBuffer[vEncodedBufferIndex];
      if (vItem = ord('~')) and fBExpectBoundary then begin // holy #@! oops...
        fBSourceEOF := True;
        {if not fBExpectBoundary then -- flag is not yet valid.
          raise EConvertError.Create('unexpected ''~>'' (there was no starting ''<~'', so why would there be a final one?).');
        }

        // note that here, it could be that we ran over the boundary '~>' suffix in the underlying stream and didn't notice. In that case, the 'Decode' call below would break.

        if not ClosedP() then
          Close(); // make sure we catch the "virtual characters". This could fill the buffer a little bit.

        // seek the underlying stream and hope nobody noticed that we completely ignored the boundary :)
        try
          Source.Seek(vEncodedBufferIndex - vEncodedBufferCount + 1, 1); // from current position.
          if Source.ReadByte() <> ord('>') then
            raise EConvertError.Create('the final ''~>'' is malformed.');
        except
          on E : EConvertError do
            raise;
{$IFNDEF UNSEEKABLE_STREAMS_ARE_EVIL}
          else
            ; // too bad... well, we tried.
{$ENDIF}
        end;
        Break; // for.
      end;
      Self.Decode(vItem);
    end;
  until (aCount <= 0) or (fBEOF) or (vPermittedReadCount = 0);

  Inc(fPosition, Result);
end;

function TASCII85DecoderStream.Seek(const aOffset: Int64; aOrigin: TSeekOrigin): Int64;
begin
  if (aOrigin = soCurrent) and (aOffset = 0) then begin // get position.
    Result := fPosition;
    Exit;
  end;

  raise EReadError.Create('could not seek...');
  //assert(fState in [ascInitial, ascNoEncodedChar]);
  //Result := inherited Seek(aOffset, aOrigin); // bad idea.
end;

function TASCII85DecoderStream.Seek(aOffset : longint; aOrigin : word) : longint;
begin
  Result := Self.Seek(Int64(aOffset), TSeekOrigin(aOrigin));
end;

function TASCII85DecoderStream.GetPosition() : Int64;
begin
  Result := fPosition;
end;

initialization
  assert(Sizeof(Cardinal) >= 4);

end.
